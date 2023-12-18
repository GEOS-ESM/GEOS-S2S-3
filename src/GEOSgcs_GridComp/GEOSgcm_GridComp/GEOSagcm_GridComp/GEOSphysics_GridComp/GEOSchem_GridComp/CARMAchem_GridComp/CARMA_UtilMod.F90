#include "MAPL_Generic.h"
!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  CARMA_UtilMod --- CARMA Utilities
!
! Container module for dealing with various utility processes
! central to setting up the CARMA problem, but not integral to
! actually running the CARMA subcode.  Examples are doing
! emissions, dry deposition, and wet deposition.
!
! !INTERFACE:
!

   MODULE  CARMA_UtilMod

! !USES:

   USE ESMF
   USE MAPL_Mod
   USE Chem_Mod
   USE Chem_UtilMod
   USE Chem_ConstMod, only: undef
   USE m_inpak90	     ! Resource file management
   USE m_chars, only: uppercase, lowercase

!  Utility Modules
   use DustEmissionMod       ! Dust Emissions
   use SeasaltEmissionMod    ! Seasalt Emissions
   use DryDepositionMod      ! Aerosol Dry Deposition
   use WetRemovalMod         ! Aerosol Wet Removal
   use ConvectionMod         ! Offline convective mixing/scavenging


!  CARMA Specific Methods
   use CARMA_GridCompMod
   use carma_precision_mod 
   use carma_constants_mod 
   use carma_enums_mod 
   use carma_types_mod 
   use carmaelement_mod
   use carmagroup_mod
   use carmastate_mod
   use carma_mod

   IMPLICIT NONE
   INTEGER, PARAMETER :: DBL = KIND(0.00D+00)
   REAL,    PARAMETER :: radToDeg = 57.2957795

! !TYPES:

   PRIVATE

! !PUBLIC MEMBER FUNCTIONS:

   PUBLIC  CARMA_Emissions
   PUBLIC  CARMA_DryDeposition
   PUBLIC  CARMA_WetRemoval
   PUBLIC  CARMA_Convection
   PUBLIC  CARMA_ComputeDiags
   PUBLIC  CARMA_GetMieTables
   PUBLIC  CARMA_DestroyMieTables

!
! !DESCRIPTION:
!
!  This module implements utilities for CARMA
!
! !REVISION HISTORY:
!
!  30Mar2010 Colarco   First crack
!
!EOP
!-------------------------------------------------------------------------

!!  Dust 8-bin specific values
!   real, parameter :: dMash(8) = &      ! GOCART like PSD
!    (/ 0.0009, 0.0081, 0.0234, 0.0676, &
!       0.25,   0.25,   0.25,   0.25 /)
!   real, parameter :: dMbc(8) = &    ! GADS initial PSD for BC
!    (/     0.0077,     0.0533,      0.1848,      0.3213, &
!           0.2803,     0.1227,      0.0269,      0.0030 /)


!  22-bin specific values
   real, parameter :: dMash(22) = &   ! Neimeier et al. 2009 PSD for Pinatubo
    (/ 0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000, &
       0.0000,  0.0000,  0.0001,  0.0008,  0.0031,  0.0103,  0.0278,  0.0609, &
       0.1082,  0.1557,  0.1817,  0.1719,  0.1318,  0.0820 /)
   real, parameter :: dMbc(22) = &   ! Ndola AERONET-mean PSD from Matichuk et al. 2007
    (/ 0.0085,  0.0325,  0.0836,  0.1449,  0.1694,  0.1335,  0.0708,  0.0254, &
       0.0066,  0.0023,  0.0033,  0.0067,  0.0126,  0.0209,  0.0307,  0.0399, &
       0.0460,  0.0470,  0.0424,  0.0339,  0.0240,  0.0151 /)
!  Pinatubo pulse initial particle size distribution for direct sulfate
!  injection based on Guo et al. 2004 Table 5 (6/15/91 10:53-18:07 effective
!  radius = 0.2 - 0.21 microns, so this is dMass mapped to 22 size bin sulfate
!  assuming rmed = 0.12 um and sigma = 1.59
   real, parameter :: dMpin(22) = &   
    (/ 0.00000,  0.00000,  0.00000,  0.00000,  0.00000,  0.00000,  0.00000, 0.00000, &
       0.00000,  0.00000,  0.00000,  0.00008,  0.00258,  0.03307,  0.17177, 0.36158, &
       0.30846,  0.10664,  0.01494,  0.00085,  0.00002,  0.00000 /)




!  Export stuff
#define ptrCARMA_DUCM     DU_cm         ! Bin column loading


CONTAINS

!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CARMA_Emissions -- Handle doing emissions calls for CARMA
!
! !INTERFACE:
!

   SUBROUTINE CARMA_Emissions ( gcCARMA, qa, impChem, expChem, nymd, nhms, cdt, &
                                rc )

   IMPLICIT none

! !INPUT PARAMETERS:

   INTEGER, INTENT(IN) :: nymd, nhms		       ! Time from AGCM
   REAL,    INTENT(IN) :: cdt			       ! Chemistry time step (secs)

! !OUTPUT PARAMETERS:

   TYPE(CARMA_GridComp), INTENT(INOUT) :: gcCARMA    ! Grid Component
   TYPE(ESMF_State),   INTENT(INOUT)   :: impChem    ! Import State
   TYPE(ESMF_State),   INTENT(INOUT)   :: expChem    ! Export State
   TYPE(Chem_Array), pointer           :: qa(:)   ! tracer array will go here

   INTEGER, INTENT(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Parses the CARMA registry and handles doing emissions to main
!               tracer array.
!
! !REVISION HISTORY:
!
!  10Mar2010 Colarco   First crack.
!
!EOP
!-------------------------------------------------------------------------

   CHARACTER(LEN=*), PARAMETER :: myname = 'CARMA_Emissions'
   CHARACTER(LEN=255) :: groupname, elemname

   INTEGER :: ielem, ibin, igroup, igas
   CHARACTER(LEN=*), PARAMETER :: IAm = 'CARMA_UtilMod'
   INTEGER :: STATUS
   INTEGER :: i1, i2, im, j1, j2, jm, km, ijl, n, ii, ios
   INTEGER :: nymd1, nhms1
   INTEGER :: n1, n2
   REAL(kind=f) :: dtime
   REAL :: volclon, volclat, dk, dkt, volcems, dlon, dlat
   INTEGER :: klow, kup, i, j, k


   real(kind=f), allocatable :: radius(:), dr(:), rLow(:), rUp(:), rhop(:)
   real, pointer             :: w10m(:,:)
   real                      :: radius_m, radius_um, rLow_um, rUp_um
   real                      :: qmin, qmax
   integer                   :: ibinfirst, ibinlast
   real, allocatable         ::  fgridefficiency(:,:), fsstemis(:,:), tskin_c(:,:)

   REAL, POINTER, DIMENSION(:,:,:) :: p, ple, rhoa, tmpu, zc, zl, q, zle, rh
   REAL, POINTER, DIMENSION(:,:,:) :: pso4
   REAL, POINTER, DIMENSION(:,:)   :: gwettop, frlake, oro, u10m, v10m, &
                                      ustar, pblh, z0h, shflux, precc, precl, &
                                      frocean, frseaice, frland, tskin, area
   REAL, POINTER, DIMENSION(:,:)   :: emissions, memissions, nemissions, dqa
   REAL, POINTER, DIMENSION(:,:)   :: biofuel_src, ebcant1_src, ebcant2_src, &
                                      bc_ship_src, biomass_src
   real, pointer, dimension(:,:)   :: du_emis, ss_emis, bc_emis, ash_emis, sm_emis

   type(CARMA_Registry), pointer :: reg => null()
   type(carma_type), pointer     :: r => null()

!  We are using the CARMA constants here (CGS units) but need
!  MKS values to go back to GEOS-5
   REAL, PARAMETER    :: grav_mks = grav/100.

!  Seasalt emission method
   integer, parameter                :: method = 3

!  Indices for point emissions
   integer, pointer, dimension(:)  :: iPoint, jPoint
   real, dimension(gcCARMA%km)     :: point_column_emissions
   real, dimension(gcCARMA%km)     :: delp

   rc = 0
   i1 = gcCARMA%i1
   i2 = gcCARMA%i2
   im = gcCARMA%im
   
   j1 = gcCARMA%j1
   j2 = gcCARMA%j2
   jm = gcCARMA%jm
   
   km = gcCARMA%km
   
   ijl = (i2-i1+1)*(j2-j1+1)

   dtime = cdt

   r   => gcCARMA%carma
   reg => gcCARMA%CARMAreg

   n1 =  1
   n2 =  gcCARMA%CARMAreg%nq

   allocate(emissions(i1:i2,j1:j2), memissions(i1:i2,j1:j2), &
            nemissions(i1:i2,j1:j2), w10m(i1:i2,j1:j2), dqa(i1:i2,j1:j2), stat=STATUS)
   VERIFY_(STATUS)

!  Get Imports
!  -----------
   call MAPL_GetPointer ( impChem, area,     'AREA',    __RC__)
   call MAPL_GetPointer ( impChem, frocean,  'FROCEAN',  __RC__)
   call MAPL_GetPointer ( impChem, frseaice, 'FRACI',    __RC__)
   call MAPL_GetPointer ( impChem, rhoa, 'AIRDENS', __RC__)
   call MAPL_GetPointer ( impChem, ple, 'PLE', __RC__)
   call MAPL_GetPointer ( impChem, zle, 'ZLE', __RC__)
   call MAPL_GetPointer ( impChem, q, 'Q', __RC__)
   call MAPL_GetPointer ( impChem, rh, 'RH2', __RC__)
   call MAPL_GetPointer ( impChem, tmpu, 'T', __RC__)
   call MAPL_GetPointer ( impChem, ustar, 'USTAR', __RC__)
   call MAPL_GetPointer ( impChem, frlake, 'FRLAKE', __RC__)
   call MAPL_GetPointer ( impChem, frland, 'FRLAND', __RC__)
   call MAPL_GetPointer ( impChem, gwettop, 'WET1', __RC__)
   call MAPL_GetPointer ( impChem, u10m, 'U10M', __RC__)
   call MAPL_GetPointer ( impChem, v10m, 'V10M', __RC__)
   call MAPL_GetPointer ( impChem, pblh, 'ZPBL', __RC__)
   call MAPL_GetPointer ( impChem, z0h, 'Z0H', __RC__)
   call MAPL_GetPointer ( impChem, shflux, 'SH', __RC__)
   call MAPL_GetPointer ( impChem, precl, 'NCN_PRCP', __RC__)
   call MAPL_GetPointer ( impChem, precc, 'CN_PRCP', __RC__)
   call MAPL_GetPointer ( impChem, oro, 'LWI', __RC__)
!   call MAPL_GetPointer ( impChem, tskin, 'TS', __RC__)
   call MAPL_GetPointer ( impChem, pso4, 'PSO4TOT', __RC__)


!  Define 10-m wind speed
   w10m = sqrt(u10m*u10m + v10m*v10m)

!  Get Exports
!  -----------
   call MAPL_GetPointer(expChem, du_emis,   'CARMA_DUEM',   __RC__)
   call MAPL_GetPointer(expChem, ss_emis,   'CARMA_SSEM',   __RC__)
   call MAPL_GetPointer(expChem, bc_emis,   'CARMA_BCEM',   __RC__)
   call MAPL_GetPointer(expChem, sm_emis,   'CARMA_SMEM',   __RC__)
   call MAPL_GetPointer(expChem, ash_emis,   'CARMA_ASHEM',   __RC__)

!  Loop over CARMA elements and assign emissions
!  If groupname is "mixedp" or "MIXEDP" then we allow
!  primary emissions (hence "p") to elements by element
!  name, regardless of if whether number concentration
!  element.
!  At this point emission diagnostics would break if 
!  there is both "mixedp" and "dust" groups.
   do ielem = 1, reg%NELEM

    igroup = reg%igroup(ielem)
    groupname = uppercase(trim(reg%groupname(igroup)))
    elemname  = uppercase(trim(reg%elemname(ielem)))

!   Dust
!   ------------------------------------------------------------------------
    if(  groupname == 'DUST' .or. &
       ( groupname == 'MIXEDP' .AND. elemname  == 'DUST'      ) ) then

!    Compute the vertical dust emission flux (kg m-2 s-1) to be
!    apportioned across size and added as a dMass to each bin.
!      flux = S * s(r) * f(u,v,...)
!    where S is the surface source function (grid-box efficiency)
!    which should come from an input file provided in resource
!    and s(r) is some function of particle size and f(u,v,...) is
!    the functional form of the actual mobilization process.
!    In this implementation we compute f(u,v,...) in a separate routine
!    and do the rest here.

!    Read dust source function from file if not already allocated
     if( .not. associated(gcCARMA%dust_source)) then
      allocate(gcCARMA%dust_source(i1:i2,j1:j2),__STAT__)
      gcCARMA%dust_source = 0.
      call MAPL_GetPointer( impChem, gcCARMA%dust_source, 'CARMA_DU_SRC', __RC__)
     endif

!    Do the emission calculation
!    The DEAD emission calculation occurs outside the size bins, returning
!    total emissions [kg m-2 s-1] which need to be scaled by particle
!    size distribution factors, dust source function, land fraction, and
!    other tuning coefficients (i.e., resolution dependent tuning)
     emissions = 0.
     call DustEmissionDEAD( i1, i2, j1, j2, km, &
                            gwettop, oro, ustar, u10m, v10m, &
                            emissions, rc )
     if( associated(DU_emis)) DU_emis(:,:) = 0.

!    Update tracer mixing ratio and emissions diagnostic
     do ibin = 1, reg%NBIN
      dqa = 0.
      n = n1 + (ielem-1)*reg%NBIN + ibin - 1
      dqa = reg%dust_emissions_fudgefactor * frland * &
            reg%dmass_dust(ibin) * gcCARMA%dust_source * emissions *&
            dtime * grav_mks / (ple(:,:,km)-ple(:,:,km-1))
      qa(n)%data3d(:,:,km) = qa(n)%data3d(:,:,km) + dqa
      if( associated(DU_emis)) &
       DU_emis = DU_emis + dqa / (dtime * grav_mks / (ple(:,:,km)-ple(:,:,km-1)))
     end do

    endif  ! Dust
!   ------------------------------------------------------------------------


!   Sea Salt
!   -----------------------------------------------------------------------
    if(  groupname == 'SEASALT' .or. &
       ( groupname == 'MIXEDP' .AND. elemname  == 'SEASALT'      ) ) then

!    Do the emission calculation
     if( associated(SS_emis)) SS_emis(:,:) = 0.
     allocate(rLow(reg%NBIN), rUp(reg%NBIN), __STAT__ )

!    Grid box efficiency to emission (fraction of sea water)
     allocate(fgridefficiency(i1:i2,j1:j2), __STAT__ )
     fgridefficiency = min(max(0.,frocean-frseaice),1.)

!    Sea surface temperature correction
     allocate(fsstemis(i1:i2,j1:j2), __STAT__ )
     fsstemis = 0.0
     allocate( tskin_c(i1:i2,j1:j2), __STAT__ )
!     tskin_c  = tskin - 273.15
     tskin_c  = 285. - 273.15
   
     where(tskin_c < -0.1) tskin_c = -0.1    ! temperature range (0, 36) C 
     where(tskin_c > 36.0) tskin_c = 36.0    !

     fsstemis = (-1.107211 -0.010681*tskin_c -0.002276*tskin_c**2 + 60.288927*1.0/(40.0 - tskin_c))
     where(fsstemis < 0.0) fsstemis = 0.0
     where(fsstemis > 7.0) fsstemis = 7.0

     deallocate( tskin_c, __STAT__ )


     call CARMAGROUP_Get(gcCARMA%carma, igroup, rc, rlow=rlow, rup=rup)
!    Update tracer mixing ratio and emissions diagnostic
     do ibin = 1, reg%NBIN
      memissions = 0.
      nemissions = 0.
      dqa = 0.
      n = n1 + (ielem-1)*reg%NBIN + ibin - 1
!     Take radius from cm to um
      rLow_um = rLow(ibin) * 1.e4
      rUp_um  = rUp(ibin) * 1.e4
      call SeasaltEmission( rLow_um, rUp_um, method, w10m, &
                            ustar, memissions, nemissions, rc )
      dqa = reg%seasalt_emissions_Fudgefactor * fgridefficiency * fsstemis * &
            memissions * dtime * grav_mks / (ple(:,:,km)-ple(:,:,km-1))
      qa(n)%data3d(:,:,km) = qa(n)%data3d(:,:,km) + dqa
      if( associated(SS_emis)) &
       SS_emis = SS_emis + reg%seasalt_emissions_Fudgefactor * fgridefficiency * fsstemis * memissions
     end do
     deallocate(rLow,rUp, fgridefficiency, fsstemis, __STAT__ )
    endif  ! Seasalt
!   ------------------------------------------------------------------------

!   Black Carbon
!   ------------------------------------------------------------------------
    if(groupname == 'blackcarbon' .OR. groupname == 'BLACKCARBON') then

     if(gcCARMA%nymd_bc .ne. nymd) then

      gcCARMA%nymd_bc = nymd

     endif

    endif  ! Black Carbon
!   ------------------------------------------------------------------------

!   Smoke
!   ------------------------------------------------------------------------
!   For now just dump emission sources in the lower model layer
    if(  groupname == 'SMOKE' .or. &
       ( groupname == 'MIXEDP' .AND. elemname  == 'SMOKE'      ) ) then

!     Do the emission calculation
      if( associated(SM_emis)) SM_emis(:,:) = 0.

      call MAPL_GetPointer( impChem, biomass_src, 'CARMA_SM_BIOMASS', __RC__)
      call MAPL_GetPointer( impChem, biofuel_src, 'CARMA_SM_BIOFUEL', __RC__)
      call MAPL_GetPointer( impChem, ebcant1_src, 'CARMA_SM_ANTEOC1', __RC__)
      call MAPL_GetPointer( impChem, ebcant2_src, 'CARMA_SM_ANTEOC2', __RC__)
      call MAPL_GetPointer( impChem, bc_ship_src, 'CARMA_SM_SHIP', __RC__)

      if(associated(SM_emis)) SM_emis = biomass_src + biofuel_src + &
                                        bc_ship_src + ebcant1_src + ebcant2_src

      do ibin = 1, reg%NBIN
       n = n1 + (ielem-1)*reg%NBIN + ibin - 1
       dqa =     dMbc(ibin) * dtime *grav_mks / (ple(:,:,km)-ple(:,:,km-1)) &
             * (   biomass_src + biofuel_src + bc_ship_src &
                 + ebcant1_src + ebcant2_src)
       qa(n)%data3d(:,:,km) = qa(n)%data3d(:,:,km) + dqa
      enddo

    endif  ! Smoke
!   ------------------------------------------------------------------------

!   Volcanic Ash
!   ------------------------------------------------------------------------
    if(groupname == 'ash' .OR. groupname == 'ASH') then

    if(reg%doing_point_emissions_ash) then
     call Chem_UtilPointEmissions( nymd, reg%point_emissions_srcfilen_ash, &
                                   reg%nPts_ash, reg%vLat_ash, reg%vLon_ash, &
                                   reg%vBase_ash, reg%vTop_ash, reg%vEmis_ash, &
                                   reg%vStart_ash, reg%vEnd_ash )


!    Distribute
!    ----------
     if(reg%nPts_ash > 0) then
!     Get indices for point emissions
!     -------------------------------
      allocate(iPoint(reg%nPts_ash), jPoint(reg%nPts_ash), stat=ios)

      call MAPL_GetHorzIJIndex(reg%nPts_ash, iPoint, jPoint, &
                               grid = gcCARMA%grid,      &
                               lon  = reg%vLon_ash/radToDeg, &
                               lat  = reg%vLat_ash/radToDeg, &
                               rc   = rc)
      if ( rc /= 0 ) call die(myname,'cannot get indices for point emissions')

      do ii = 1, reg%nPts_ash
       i = iPoint(ii)
       j = jPoint(ii)
       if( i<1 .OR. j<1 )              cycle    ! point emission not in this sub-domain
!       if( reg%regionMask(i,j) == 0 ) cycle    ! masked by region mask
!      Check that the emissions happen in this time step
       if(nhms < reg%vStart_ash(ii) .or. nhms >= reg%vEnd_ash(ii)) cycle

       delp = ple(i,j,1:km)-ple(i,j,0:km-1)

       call distribute_point_emissions(delp, rhoa(i,j,:), &
                                       reg%vBase_ash(ii), reg%vTop_ash(ii), reg%vEmis_ash(ii), &
                                       point_column_emissions, km)
!      Update tracer mixing ratio and emissions diagnostic
       do ibin = 1, reg%NBIN
        dqa = 0.
        n = n1 + (ielem-1)*reg%NBIN + ibin - 1
        qa(n)%data3d(i,j,:) =  qa(n)%data3d(i,j,:) &
                             + dMash(ibin)*dtime*grav_mks/delp*point_column_emissions/area(i,j)
        if( associated(ASH_emis)) &
         ASH_emis(i,j) = ASH_emis(i,j) + dMash(ibin)*sum(point_column_emissions)/area(i,j)
       enddo
      enddo
      deallocate(iPoint, jPoint, stat=ios)
     endif
    endif
   endif   ! Ash

!  ------------------------------------------------------------------------

!  Point Emissions -- need some logic to put them appropriately, for now...
!  ------------------------------------------------------------------------
   if(  groupname == 'DUST' .or. &
       ( groupname == 'MIXEDP' .AND. elemname  == 'DUST'      ) ) then

    if(reg%doing_point_emissions_dust) then
     call Chem_UtilPointEmissions( nymd, reg%point_emissions_srcfilen_dust, &
                                   reg%nPts_dust, reg%vLat_dust, reg%vLon_dust, &
                                   reg%vBase_dust, reg%vTop_dust, reg%vEmis_dust, &
                                   reg%vStart_dust, reg%vEnd_dust )


!    Distribute
!    ----------
     if(reg%nPts_dust > 0) then
!     Get indices for point emissions
!     -------------------------------
      allocate(iPoint(reg%nPts_dust), jPoint(reg%nPts_dust), stat=ios)

      call MAPL_GetHorzIJIndex(reg%nPts_dust, iPoint, jPoint, &
                               grid = gcCARMA%grid,      &
                               lon  = reg%vLon_dust/radToDeg, &
                               lat  = reg%vLat_dust/radToDeg, &
                               rc   = rc)
      if ( rc /= 0 ) call die(myname,'cannot get indices for point emissions')

      do ii = 1, reg%nPts_dust
       i = iPoint(ii)
       j = jPoint(ii)
       if( i<1 .OR. j<1 )              cycle    ! point emission not in this sub-domain
!       if( reg%regionMask(i,j) == 0 ) cycle    ! masked by region mask
!      Check that the emissions happen in this time step
       if(nhms < reg%vStart_dust(ii) .or. nhms >= reg%vEnd_dust(ii)) cycle

       delp = ple(i,j,1:km)-ple(i,j,0:km-1)

       call distribute_point_emissions(delp, rhoa(i,j,:), &
                                       reg%vBase_dust(ii), reg%vTop_dust(ii), reg%vEmis_dust(ii), &
                                       point_column_emissions, km)
!      Update tracer mixing ratio and emissions diagnostic
       do ibin = 1, reg%NBIN
        dqa = 0.
        n = n1 + (ielem-1)*reg%NBIN + ibin - 1
!NB: using ash PSD here until a proper ash component is integrated
        qa(n)%data3d(i,j,:) =  qa(n)%data3d(i,j,:) &
                             + dMash(ibin)*dtime*grav_mks/delp*point_column_emissions/area(i,j)
       end do
      enddo
      deallocate(iPoint, jPoint, stat=ios)
     endif
    endif
   endif

   if(  groupname == 'SULFATE' .or. &
      ( groupname == 'MIXEDP' .AND. elemname  == 'SULFATE'      ) ) then

    if(reg%doing_point_emissions_sulfate) then
     call Chem_UtilPointEmissions( nymd, reg%point_emissions_srcfilen_sulfate, &
                                   reg%nPts_sulfate, reg%vLat_sulfate, reg%vLon_sulfate, &
                                   reg%vBase_sulfate, reg%vTop_sulfate, reg%vEmis_sulfate, &
                                   reg%vStart_sulfate, reg%vEnd_sulfate )


!    Distribute
!    ----------
     if(reg%nPts_sulfate > 0) then
!     Get indices for point emissions
!     -------------------------------
      allocate(iPoint(reg%nPts_sulfate), jPoint(reg%nPts_sulfate), stat=ios)

      call MAPL_GetHorzIJIndex(reg%nPts_sulfate, iPoint, jPoint, &
                               grid = gcCARMA%grid,      &
                               lon  = reg%vLon_sulfate/radToDeg, &
                               lat  = reg%vLat_sulfate/radToDeg, &
                               rc   = rc)
      if ( rc /= 0 ) call die(myname,'cannot get indices for point emissions')

      do ii = 1, reg%nPts_sulfate
       i = iPoint(ii)
       j = jPoint(ii)
       if( i<1 .OR. j<1 )              cycle    ! point emission not in this sub-domain
!       if( reg%regionMask(i,j) == 0 ) cycle    ! masked by region mask
!      Check that the emissions happen in this time step
       if(nhms < reg%vStart_sulfate(ii) .or. nhms >= reg%vEnd_sulfate(ii)) cycle

       delp = ple(i,j,1:km)-ple(i,j,0:km-1)

       call distribute_point_emissions(delp, rhoa(i,j,:), &
                                       reg%vBase_sulfate(ii), &
                                       reg%vTop_sulfate(ii), &
                                       reg%vEmis_sulfate(ii), &
                                       point_column_emissions, km)
!      Update tracer mixing ratio and emissions diagnostic
       do ibin = 1, reg%NBIN
        dqa = 0.
        n = n1 + (ielem-1)*reg%NBIN + ibin - 1
        qa(n)%data3d(i,j,:) =  qa(n)%data3d(i,j,:) &
                             + dMpin(ibin)*dtime*grav_mks/delp*point_column_emissions/area(i,j)
       end do
      enddo
      deallocate(iPoint, jPoint, stat=ios)
     endif
    endif
   endif


   enddo   ! NELEM

!  Do the gases (for now set up only for H2SO4 from GOCART)
!  --------------------------------------------------------
   if(reg%NGAS > 0) then
     do igas = 1, reg%NGAS
      n = n1 + reg%NELEM*reg%NBIN - 1 + igas
      if( trim(reg%gasname(igas)) == 'h2so4' .or. &
          trim(reg%gasname(igas)) == 'H2SO4') &
         qa(n)%data3d = qa(n)%data3d + pso4 * dtime
     end do
   endif

   deallocate(emissions, memissions, nemissions, dqa, w10m, stat=STATUS)
   VERIFY_(STATUS)


  RETURN

 end subroutine CARMA_Emissions

!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CARMA_DryDeposition -- Handle doing dry deposition calls for CARMA
!
! !INTERFACE:
!

   SUBROUTINE CARMA_DryDeposition ( gcCARMA, qa, impChem, expChem, nymd, nhms, cdt, &
                                    rc )

   IMPLICIT none

! !INPUT PARAMETERS:

   INTEGER, INTENT(IN) :: nymd, nhms		       ! Time from AGCM
   REAL,    INTENT(IN) :: cdt			       ! Chemistry time step (secs)

! !OUTPUT PARAMETERS:

   TYPE(CARMA_GridComp), INTENT(INOUT) :: gcCARMA    ! Grid Component
   TYPE(ESMF_State),   INTENT(INOUT)   :: impChem    ! Import State
   TYPE(ESMF_State),   INTENT(INOUT)   :: expChem    ! Export State
   TYPE(Chem_Array), pointer           :: qa(:)   ! tracer array will go here

   INTEGER, INTENT(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Parses the CARMA registry and handles doing emissions to main
!               tracer array.
!
! !REVISION HISTORY:
!
!  10Mar2010 Colarco   First crack.
!
!EOP
!-------------------------------------------------------------------------

   CHARACTER(LEN=*), PARAMETER :: myname = 'CARMA_DryDeposition'
   CHARACTER(LEN=255) :: groupname, elemname

   INTEGER :: ielem, ibin, igroup, ienconc
   CHARACTER(LEN=*), PARAMETER :: IAm = 'CARMA_UtilMod'
   INTEGER :: STATUS
   INTEGER :: i1, i2, im, j1, j2, jm, km, ijl, n
   INTEGER :: nymd1, nhms1
   INTEGER :: n1, n2
   REAL(kind=f) :: dtime
   real(kind=f), allocatable :: radius_cgs(:)
   real(kind=f), allocatable :: rhop_cgs(:)
   real                      :: radius, rhop

   REAL, POINTER, DIMENSION(:,:,:) :: p, ple, rhoa, tmpu, zc, zl, q, zle, rh
   REAL, POINTER, DIMENSION(:,:)   :: gwettop, fraclake, oro, u10m, v10m, &
                                      ustar, pblh, z0h, shflux, precc, precl
   REAL, POINTER, DIMENSION(:,:)   :: drydepositionfrequency, dqa
   real, pointer, dimension(:,:)   :: du_dep, su_dep, ss_dep, bc_dep, ash_dep, sm_dep

   type(CARMA_Registry), pointer :: reg => null()
   type(carma_type), pointer     :: r => null()

!  We are using the CARMA constants here (CGS units) but need
!  MKS values to go back to GEOS-5
   REAL, PARAMETER    :: grav_mks = grav/100.

   rc = 0
   i1 = gcCARMA%i1
   i2 = gcCARMA%i2
   im = gcCARMA%im
   
   j1 = gcCARMA%j1
   j2 = gcCARMA%j2
   jm = gcCARMA%jm
   
   km = gcCARMA%km
   
   ijl = (i2-i1+1)*(j2-j1+1)

   dtime = cdt

   r   => gcCARMA%carma
   reg => gcCARMA%CARMAreg

   n1 =  1
   n2 =  gcCARMA%CARMAreg%nq

   allocate(drydepositionfrequency(i1:i2,j1:j2), dqa(i1:i2,j1:j2), stat=STATUS)
   VERIFY_(STATUS)
   allocate(radius_cgs(reg%NBIN), rhop_cgs(reg%NBIN),stat=STATUS)
   VERIFY_(STATUS)

!  Get Imports
!  -----------
   call MAPL_GetPointer ( impChem, rhoa, 'AIRDENS', __RC__)
   call MAPL_GetPointer ( impChem, ple, 'PLE', __RC__)
   call MAPL_GetPointer ( impChem, zle, 'ZLE', __RC__)
   call MAPL_GetPointer ( impChem, q, 'Q', __RC__)
   call MAPL_GetPointer ( impChem, rh, 'RH2', __RC__)
   call MAPL_GetPointer ( impChem, tmpu, 'T', __RC__)
   call MAPL_GetPointer ( impChem, ustar, 'USTAR', __RC__)
   call MAPL_GetPointer ( impChem, fraclake, 'FRLAKE', __RC__)
   call MAPL_GetPointer ( impChem, gwettop, 'WET1', __RC__)
   call MAPL_GetPointer ( impChem, u10m, 'U10M', __RC__)
   call MAPL_GetPointer ( impChem, v10m, 'V10M', __RC__)
   call MAPL_GetPointer ( impChem, pblh, 'ZPBL', __RC__)
   call MAPL_GetPointer ( impChem, z0h, 'Z0H', __RC__)
   call MAPL_GetPointer ( impChem, shflux, 'SH', __RC__)
   call MAPL_GetPointer ( impChem, precl, 'NCN_PRCP', __RC__)
   call MAPL_GetPointer ( impChem, precc, 'CN_PRCP', __RC__)
   call MAPL_GetPointer ( impChem, oro, 'LWI', __RC__)

!  Get Exports
!  -----------
   call MAPL_GetPointer(expChem, du_dep,   'CARMA_DUDP',   __RC__)
   call MAPL_GetPointer(expChem, su_dep,   'CARMA_SUDP',   __RC__)
   call MAPL_GetPointer(expChem, ss_dep,   'CARMA_SSDP',   __RC__)
   call MAPL_GetPointer(expChem, bc_dep,   'CARMA_BCDP',   __RC__)
   call MAPL_GetPointer(expChem, sm_dep,   'CARMA_SMDP',   __RC__)
   call MAPL_GetPointer(expChem, ash_dep,   'CARMA_ASHDP',   __RC__)

!  Do dry (turbulent) deposition
!  Routine calls the GOCART dry deposition routine (sans
!  the dust resuspension term at present).  Note that this
!  scheme is entirely independent of species/size, so we
!  apply it equivalently at this point for all species/elements.
!  An exception is allowed for dust to use the resuspension term.

   if( associated(DU_dep)) DU_dep(:,:) = 0.
   if( associated(SU_dep)) SU_dep(:,:) = 0.
   if( associated(SS_dep)) SS_dep(:,:) = 0.
   if( associated(BC_dep)) BC_dep(:,:) = 0.
   if( associated(SM_dep)) SM_dep(:,:) = 0.
   if( associated(ASH_dep)) ASH_dep(:,:) = 0.
   do ielem = 1, reg%NELEM

!   Routine returns the dry deposition frequency [s-1].
    drydepositionfrequency = 0.
    call DryDepositionGOCART( i1, i2, j1, j2, km, &
                              tmpu, rhoa, zle, oro, ustar, &
                              pblh, shflux, z0h, drydepositionfrequency, rc )

    igroup = reg%igroup(ielem)
    groupname = uppercase(trim(reg%groupname(igroup)))
    elemname  = uppercase(trim(reg%elemname(ielem)))
    ienconc = r%f_group(igroup)%f_ienconc

    do ibin = 1, reg%NBIN

!    If doing dust, recompute the dry deposition frequency per bin
!    to allow dust resuspension term.
!    NB: This may not make a lot of sense for multi-component dust
     if(groupname == 'dust' .OR. groupname == 'DUST') then
      call CARMAGROUP_Get(gcCARMA%carma, igroup, rc, r=radius_cgs)
      call CARMAELEMENT_Get(gcCARMA%carma, ielem, rc, rho=rhop_cgs)
      radius = radius_cgs(ibin) * 1.e-2
      rhop   = rhop_cgs(ibin) * 1000.
      drydepositionfrequency = 0.
      call DryDepositionGOCART( i1, i2, j1, j2, km, &
                                tmpu, rhoa, zle, oro, ustar, &
                                pblh, shflux, z0h, drydepositionfrequency, &
				rc, radius, rhop, u10m, v10m, fraclake, &
				gwettop )
     endif

     dqa = 0.
     n = n1 + (ielem-1)*reg%NBIN + ibin - 1
     dqa = max(0.0, qa(n)%data3d(:,:,km)*(1.-exp(-drydepositionfrequency*dtime)))
     qa(n)%data3d(:,:,km) = qa(n)%data3d(:,:,km) - dqa

     if(  groupname == 'DUST' .or. &
        ( groupname == 'MIXEDP' .AND. elemname  == 'DUST'      ) ) then
      if( associated(DU_dep)) &
       DU_dep(:,:) = DU_dep(:,:) &
        + dqa * (ple(:,:,km)-ple(:,:,km-1)) / grav_mks / dtime
     endif

     if(  groupname == 'SULFATE' .or. &
        ( groupname == 'MIXEDP' .AND. elemname  == 'SULFATE'      ) ) then
      if( associated(SU_dep)) &
       SU_dep(:,:) = SU_dep(:,:) &
        + dqa * (ple(:,:,km)-ple(:,:,km-1)) / grav_mks / dtime
     endif

     if(  groupname == 'SEASALT' .or. &
        ( groupname == 'MIXEDP' .AND. elemname  == 'SEASALT'      ) ) then
      if( associated(SS_dep)) &
       SS_dep(:,:) = SS_dep(:,:) &
        + dqa * (ple(:,:,km)-ple(:,:,km-1)) / grav_mks / dtime
     endif

     if(  groupname == 'BLACKCARBON' .or. &
        ( groupname == 'MIXEDP' .AND. elemname  == 'BLACKCARBON'      ) ) then
      if( associated(BC_dep)) &
       BC_dep(:,:) = BC_dep(:,:) &
        + dqa * (ple(:,:,km)-ple(:,:,km-1)) / grav_mks / dtime
     endif

     if(  groupname == 'SMOKE' .or. &
        ( groupname == 'MIXEDP' .AND. elemname  == 'SMOKE'      ) ) then
      if( associated(SM_dep)) &
       SM_dep(:,:) = SM_dep(:,:) &
        + dqa * (ple(:,:,km)-ple(:,:,km-1)) / grav_mks / dtime
     endif

     if(  groupname == 'ASH' .or. &
        ( groupname == 'MIXEDP' .AND. elemname  == 'ASH'      ) ) then
      if( associated(ASH_dep)) &
       ASH_dep(:,:) = ASH_dep(:,:) &
        + dqa * (ple(:,:,km)-ple(:,:,km-1)) / grav_mks / dtime
     endif

    enddo  ! NBIN
   enddo   ! NELEM

   deallocate(radius_cgs, rhop_cgs, drydepositionfrequency, dqa, stat=STATUS)
   VERIFY_(STATUS)


  RETURN

 end subroutine CARMA_DryDeposition




!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CARMA_WetRemoval -- Handle doing wet removal calls for CARMA
!
! !INTERFACE:
!

   SUBROUTINE CARMA_WetRemoval ( gcCARMA, qa, impChem, expChem, nymd, nhms, cdt, &
                                 rc )

   IMPLICIT none

! !INPUT PARAMETERS:

   INTEGER, INTENT(IN) :: nymd, nhms		       ! Time from AGCM
   REAL,    INTENT(IN) :: cdt			       ! Chemistry time step (secs)

! !OUTPUT PARAMETERS:

   TYPE(CARMA_GridComp), INTENT(INOUT) :: gcCARMA    ! Grid Component
   TYPE(ESMF_State),   INTENT(INOUT)   :: impChem    ! Import State
   TYPE(ESMF_State),   INTENT(INOUT)   :: expChem    ! Export State
   TYPE(Chem_Array), pointer           :: qa(:)   ! tracer array will go here

   INTEGER, INTENT(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Parses the CARMA registry and handles doing emissions to main
!               tracer array.
!
! !REVISION HISTORY:
!
!  10Mar2010 Colarco   First crack.
!
!EOP
!-------------------------------------------------------------------------

   CHARACTER(LEN=*), PARAMETER :: myname = 'CARMA_WetRemoval'
   CHARACTER(LEN=255) :: groupname

   INTEGER :: ielem, ibin, igroup, ienconc
   CHARACTER(LEN=*), PARAMETER :: IAm = 'CARMA_UtilMod'
   INTEGER :: STATUS
   INTEGER :: i1, i2, im, j1, j2, jm, km, ijl, n
   INTEGER :: nymd1, nhms1
   INTEGER :: n1, n2
   REAL(kind=f) :: dtime

   REAL, POINTER, DIMENSION(:,:,:) :: p, ple, rhoa, tmpu, zc, zl, q, zle, &
                                      rh, pfllsan, pfilsan
   REAL, POINTER, DIMENSION(:,:)   :: gwettop, fraclake, oro, u10m, v10m, &
                                      ustar, pblh, z0h, shflux, precc, precl
   type(Chem_Array), pointer       :: wetremovalflux
   real, pointer, dimension(:,:)   :: du_wet, su_wet, ss_wet, bc_wet, ash_wet, sm_wet

   type(CARMA_Registry), pointer   :: reg => null()
   type(carma_type), pointer       :: r => null()

!  We are using the CARMA constants here (CGS units) but need
!  MKS values to go back to GEOS-5
   REAL, PARAMETER                 :: grav_mks = grav/100.

   real                            :: qmin, qmax

!  This flag was added to wet removal call to indicate aerosol (true) or gas (false)
   logical                         :: KIN

   rc = 0
   i1 = gcCARMA%i1
   i2 = gcCARMA%i2
   im = gcCARMA%im
   
   j1 = gcCARMA%j1
   j2 = gcCARMA%j2
   jm = gcCARMA%jm
   
   km = gcCARMA%km
   
   ijl = (i2-i1+1)*(j2-j1+1)

   dtime = cdt

   r   => gcCARMA%carma
   reg => gcCARMA%CARMAreg

   n1 =  1
   n2 =  reg%nq

   allocate(wetremovalflux, stat=STATUS)
   VERIFY_(STATUS)
   allocate(wetremovalflux%data2d(i1:i2,j1:j2), stat=STATUS)
   VERIFY_(STATUS)
   wetremovalflux%data2d = 0.


!  Get Imports
!  -----------
   call MAPL_GetPointer ( impChem, rhoa, 'AIRDENS', __RC__)
   call MAPL_GetPointer ( impChem, ple, 'PLE', __RC__)
   call MAPL_GetPointer ( impChem, zle, 'ZLE', __RC__)
   call MAPL_GetPointer ( impChem, q, 'Q', __RC__)
   call MAPL_GetPointer ( impChem, rh, 'RH2', __RC__)
   call MAPL_GetPointer ( impChem, tmpu, 'T', __RC__)
   call MAPL_GetPointer ( impChem, ustar, 'USTAR', __RC__)
   call MAPL_GetPointer ( impChem, fraclake, 'FRLAKE', __RC__)
   call MAPL_GetPointer ( impChem, gwettop, 'WET1', __RC__)
   call MAPL_GetPointer ( impChem, u10m, 'U10M', __RC__)
   call MAPL_GetPointer ( impChem, v10m, 'V10M', __RC__)
   call MAPL_GetPointer ( impChem, pblh, 'ZPBL', __RC__)
   call MAPL_GetPointer ( impChem, z0h, 'Z0H', __RC__)
   call MAPL_GetPointer ( impChem, shflux, 'SH', __RC__)
   call MAPL_GetPointer ( impChem, precl, 'NCN_PRCP', __RC__)
   call MAPL_GetPointer ( impChem, precc, 'CN_PRCP', __RC__)
   call MAPL_GetPointer ( impChem, oro, 'LWI', __RC__)
   call MAPL_GetPointer ( impChem, pfllsan,'PFL_LSAN', __RC__ )
   call MAPL_GetPointer ( impChem, pfilsan,'PFI_LSAN', __RC__ )

!  Get Exports
!  ----------
   call MAPL_GetPointer(expChem, du_wet,   'CARMA_DUWT',   __RC__)
   call MAPL_GetPointer(expChem, su_wet,   'CARMA_SUWT',   __RC__)
   call MAPL_GetPointer(expChem, ss_wet,   'CARMA_SSWT',   __RC__)
   call MAPL_GetPointer(expChem, bc_wet,   'CARMA_BCWT',   __RC__)
   call MAPL_GetPointer(expChem, sm_wet,   'CARMA_SMWT',   __RC__)
   call MAPL_GetPointer(expChem, ash_wet,   'CARMA_ASHWT',   __RC__)

!  Routine calls the GOCART wet removal routine (large scale
!  precip).  Note that this scheme is entirely independent of size,
!  but could use species/size varying efficiency factor.
!  Returned are the updated tracer mixing ratios (qa) and a flux
!  diagnostic (e.g., DU_wet, [integrated kg m-2 s-1 loss from column all bins]).
   if( associated(DU_wet)) DU_wet(:,:) = 0.
   if( associated(SU_wet)) SU_wet(:,:) = 0.
   if( associated(SS_wet)) SS_wet(:,:) = 0.
   if( associated(BC_wet)) BC_wet(:,:) = 0.
   if( associated(SM_wet)) SM_wet(:,:) = 0.
   if( associated(ASH_wet)) ASH_wet(:,:) = 0.


!  This is a bit clumsy, but for now we define scavenging parameters here
   do ielem = 1, reg%NELEM
    igroup = reg%igroup(ielem)
    groupname = trim(reg%groupname(igroup))
    do ibin = 1, reg%NBIN
     n = n1 + (ielem-1)*reg%NBIN + ibin - 1
     if(groupname == 'ash'  .OR. groupname == 'ASH' .or. &
        groupname == 'blackcarbon'  .OR. groupname == 'BLACKCARBON' .or. &
        groupname == 'dust'  .OR. groupname == 'DUST' .or. &
        groupname =='smoke' .or. groupname == 'SMOKE' ) then
      qa(n)%fwet  = 0.3
     else
      qa(n)%fwet  = 1.
     endif
    end do
   end do


   do ielem = 1, reg%NELEM

    igroup = reg%igroup(ielem)
    groupname = trim(reg%groupname(igroup))
    ienconc = r%f_group(igroup)%f_ienconc

    n = n1 + (ielem-1)*reg%NBIN

!   For now we presume we are wet removing an aerosol
    KIN = .true.    ! aerosol
    call WetRemovalGOCART (i1, i2, j1, j2, km, n, n+reg%NBIN-1, cdt,      &
                           lowercase(trim(groupname)), KIN,               &
                           qa, ple, tmpu, rhoa, pfllsan, pfilsan, & 
                           precc, precl, wetremovalflux, rc )

    if(ienconc == ielem) then
     if(lowercase(trim(groupname)) == 'dust') then
      if( associated(DU_wet)) DU_wet(:,:) = wetremovalflux%data2d
     endif

     if(lowercase(trim(groupname)) == 'sulfate') then
      if( associated(SU_wet)) SU_wet(:,:) = wetremovalflux%data2d
     endif

     if(lowercase(trim(groupname)) == 'seasalt') then
      if( associated(SS_wet)) SS_wet(:,:) = wetremovalflux%data2d
     endif

     if(lowercase(trim(groupname)) == 'blackcarbon') then
      if( associated(BC_wet)) BC_wet(:,:) = wetremovalflux%data2d
     endif

     if(lowercase(trim(groupname)) == 'smoke') then
      if( associated(SM_wet)) SM_wet(:,:) = wetremovalflux%data2d
     endif

     if(lowercase(trim(groupname)) == 'ash') then
      if( associated(ASH_wet)) ASH_wet(:,:) = wetremovalflux%data2d
     endif

    endif

   enddo   ! NELEM

   deallocate(wetremovalflux%data2d, stat=STATUS)
   VERIFY_(STATUS)
   deallocate(wetremovalflux, stat=STATUS)
   VERIFY_(STATUS)


  RETURN

 end subroutine CARMA_WetRemoval



!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CARMA_Convection -- Apply offline convective mixing and
!                                 scavenging code
!
! !INTERFACE:
!

   SUBROUTINE CARMA_Convection ( gcCARMA, qa, impChem, expChem, nymd, nhms, cdt, &
                                 rc )

   IMPLICIT none

! !INPUT PARAMETERS:

   INTEGER, INTENT(IN) :: nymd, nhms		       ! Time from AGCM
   REAL,    INTENT(IN) :: cdt			       ! Chemistry time step (secs)

! !OUTPUT PARAMETERS:

   TYPE(CARMA_GridComp), INTENT(INOUT) :: gcCARMA    ! Grid Component
   TYPE(ESMF_State),   INTENT(INOUT)   :: impChem    ! Import State
   TYPE(ESMF_State),   INTENT(INOUT)   :: expChem    ! Export State
   TYPE(Chem_Array), pointer           :: qa(:)   ! tracer array will go here

   INTEGER, INTENT(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Parses the CARMA registry and handles doing emissions to main
!               tracer array.
!
! !REVISION HISTORY:
!
!  10Mar2010 Colarco   First crack.
!
!EOP
!-------------------------------------------------------------------------

   CHARACTER(LEN=*), PARAMETER :: myname = 'CARMA_Convection'
   CHARACTER(LEN=255) :: groupname

   INTEGER :: ielem, ibin, igroup, ienconc
   CHARACTER(LEN=*), PARAMETER :: IAm = 'CARMA_UtilMod'
   INTEGER :: STATUS
   INTEGER :: i1, i2, im, j1, j2, jm, km, ijl, n, k
   INTEGER :: nymd1, nhms1
   INTEGER :: n1, n2
   REAL(kind=f) :: dtime
   real :: qmin, qmax

   REAL, POINTER, DIMENSION(:,:,:) :: cmfmc, qccu, dtrain, ple, zle, rhoa, tmpu
   REAL, POINTER, DIMENSION(:,:)   :: frocean, frseaice, frlake, area
   real, pointer, dimension(:,:)   :: du_scav, su_scav, ss_scav, bc_scav, ash_scav, sm_scav

!  Locals
   real*8, allocatable, dimension(:,:,:) ::  cmfmc_, qccu_, dtrain_, &
                                             airmass_, airmol_, vud_, &
                                             delz_, delp_, ple_, tmpu_
   real*8, allocatable                   ::  tc_(:,:,:,:), bcnv_(:,:,:)
   real*8, allocatable                   ::  area_(:,:), frlake_(:,:), &
                                             frocean_(:,:), frseaice_(:,:)
   integer*4                             ::  icdt


   type(CARMA_Registry), pointer :: reg => null()
   type(carma_type), pointer     :: r => null()

!  We are using the CARMA constants here (CGS units) but need
!  MKS values to go back to GEOS-5
   REAL, PARAMETER    :: grav_mks = grav/100.

!  This flag was added to wet removal call to indicate aerosol (true) or gas (false)
   logical                         :: KIN

   rc = 0
   i1 = gcCARMA%i1
   i2 = gcCARMA%i2
   im = gcCARMA%im
   
   j1 = gcCARMA%j1
   j2 = gcCARMA%j2
   jm = gcCARMA%jm
   
   km = gcCARMA%km
   
   ijl = (i2-i1+1)*(j2-j1+1)

   dtime = cdt

   r   => gcCARMA%carma
   reg => gcCARMA%CARMAreg

   n1 =  1
   n2 =  reg%nq

!  Get Imports
!  -----------
   call MAPL_GetPointer ( impChem, rhoa, 'AIRDENS', __RC__)
   call MAPL_GetPointer ( impChem, ple, 'PLE', __RC__)
   call MAPL_GetPointer ( impChem, zle, 'ZLE', __RC__)
   call MAPL_GetPointer ( impChem, frlake,   'FRLAKE',  __RC__)
   call MAPL_GetPointer ( impChem, area,     'AREA',    __RC__)
   call MAPL_GetPointer ( impChem, frocean,  'FROCEAN', __RC__)
   call MAPL_GetPointer ( impChem, frseaice, 'FRACI',   __RC__)
   call MAPL_GetPointer ( impChem, qccu,     'CNV_QC',  __RC__)
   call MAPL_GetPointer ( impChem, cmfmc,    'CNV_MFC', __RC__)
   call MAPL_GetPointer ( impChem, dtrain,   'CNV_MFD', __RC__)
   call MAPL_GetPointer ( impChem, tmpu,     'T',       __RC__)

!  Get Exports
!  -----------
   call MAPL_GetPointer(expChem, du_scav,   'CARMA_DUSV',   __RC__)
   call MAPL_GetPointer(expChem, su_scav,   'CARMA_SUSV',   __RC__)
   call MAPL_GetPointer(expChem, ss_scav,   'CARMA_SSSV',   __RC__)
   call MAPL_GetPointer(expChem, bc_scav,   'CARMA_BCSV',   __RC__)
   call MAPL_GetPointer(expChem, sm_scav,   'CARMA_SMSV',   __RC__)
   call MAPL_GetPointer(expChem, ash_scav,   'CARMA_ASHSV',   __RC__)

#ifdef DEBUG
   call pmaxmin('CARMA::area    : ', area     , qmin, qmax, ijl, 1, 1. )
   call pmaxmin('CARMA::frlake  : ', frlake   , qmin, qmax, ijl, 1, 1. )
   call pmaxmin('CARMA::frocean : ', frocean  , qmin, qmax, ijl, 1, 1. )
   call pmaxmin('CARMA::frseaice: ', frseaice , qmin, qmax, ijl, 1, 1. )
   call pmaxmin('CARMA::rhoa    : ', rhoa     , qmin, qmax, ijl, km, 1. )
   call pmaxmin('CARMA::ple     : ', ple      , qmin, qmax, ijl, km+1, 1. )
   call pmaxmin('CARMA::zle     : ', zle      , qmin, qmax, ijl, km+1, 1. )
   call pmaxmin('CARMA::cmfmc   : ', cmfmc    , qmin, qmax, ijl, km+1, 1. )
   call pmaxmin('CARMA::qccu    : ', qccu     , qmin, qmax, ijl, km, 1. )
   call pmaxmin('CARMA::dtrain  : ', dtrain   , qmin, qmax, ijl, km, 1. )
#endif

!  Local allocation and creation
   icdt = cdt
   allocate(cmfmc_(i1:i2,j1:j2,km+1), qccu_(i1:i2,j1:j2,km), &
            dtrain_(i1:i2,j1:j2,km), airmass_(i1:i2,j1:j2,km), &
            delz_(i1:i2,j1:j2,km), vud_(i1:i2,j1:j2,km), &
            tc_(i1:i2,j1:j2,km,reg%NBIN), delp_(i1:i2,j1:j2,km), &
            airmol_(i1:i2,j1:j2,km), tmpu_(i1:i2,j1:j2,km), &
            bcnv_(i1:i2,j1:j2,reg%NBIN), ple_(i1:i2,j1:j2,km+1), &
            area_(i1:i2,j1:j2), frlake_(i1:i2,j1:j2), &
            frocean_(i1:i2,j1:j2), frseaice_(i1:i2,j1:j2), __STAT__ )


   area_            = area
   frlake_          = frlake
   frocean_         = frocean
   frseaice_        = frseaice
   do k = 1, km+1
    cmfmc_(:,:,k)   = cmfmc(:,:,km-k+1)
    ple_(:,:,k)     = ple(:,:,km-k+1)
   end do
   do k = 1, km
    dtrain_(:,:,k)  = dtrain(:,:,km-k+1)
    qccu_(:,:,k)    = qccu(:,:,km-k+1)
    delp_(:,:,k)    = ple(:,:,km-k+1)-ple(:,:,km-k)
    airmass_(:,:,k) = delp_(:,:,k)/grav_mks*area_
    airmol_(:,:,k)  = airmass_(:,:,k)*1000./28.966
    delz_(:,:,k)    = delp_(:,:,k)/grav_mks/rhoa(:,:,km-k+1)
    tmpu_(:,:,k)    = tmpu(:,:,km-k+1)
   enddo

!  Routine calls the GOCART wet removal routine (large scale
!  precip).  Note that this scheme is entirely independent of size,
!  but could use species/size varying efficiency factor.
!  Returned are the updated tracer mixing ratios (qa) and a flux
!  diagnostic (e.g., DU_wet, [integrated kg m-2 s-1 loss from column all bins]).
   if( associated(DU_scav)) DU_scav(:,:) = 0.
   if( associated(SU_scav)) SU_scav(:,:) = 0.
   if( associated(SS_scav)) SS_scav(:,:) = 0.
   if( associated(BC_scav)) BC_scav(:,:) = 0.
   if( associated(SM_scav)) SM_scav(:,:) = 0.
   if( associated(ASH_scav)) ASH_scav(:,:) = 0.

!  For now we do the calculation based on elements
   do ielem = 1, reg%NELEM
    igroup = reg%igroup(ielem)
    groupname = trim(reg%groupname(igroup))
    ienconc = r%f_group(igroup)%f_ienconc

    do ibin = 1, reg%NBIN
     n = n1 + (ielem-1)*reg%NBIN + ibin - 1
     do k = 1, km
      tc_(:,:,k,ibin)   = qa(n)%data3d(:,:,km-k+1)
     enddo
    enddo
     
    call set_vud(i1, i2, j1, j2, km, frlake_, frocean_, frseaice_, cmfmc_, qccu_, &
                 airmass_, delz_, area_, vud_)

!   For now we presume we are wet removing an aerosol
    KIN = .true.    ! aerosol
    call convection(i1, i2, j1, j2, km, 1, reg%NBIN, icdt, &
                    lowercase(trim(groupname)), KIN, &
                    tc_, cmfmc_, dtrain_, area_, delz_, delp_, vud_, &
                    airmass_, airmol_, tmpu_, ple_, &
                    bcnv_)

!   Return adjusted tracer to mixing ratio and accumulate diagnostic
!   Note GOCART returns bcnv_ as negative, recast for my diagnostic
!   PRC -- In GeoMIP style simulations finding non-conservative
!   return from convection at some North Pole points.  So I check 
!   for bcnv_ > 0 as an indication of this (addition of particles
!   to column) and exclude those columns.  This is a hack.

    do ibin = 1, reg%NBIN

     n = n1 + (ielem-1)*reg%NBIN + ibin - 1
     do k = 1, km
      where(bcnv_(:,:,ibin) < 0) qa(n)%data3d(:,:,km-k+1) = tc_(:,:,k,ibin)
     enddo
     where(bcnv_(:,:,ibin) > 0) bcnv_(:,:,ibin) = 0.

     if(ienconc == ielem) then
      if(groupname == 'dust' .OR. groupname == 'DUST') then
       if( associated(DU_scav)) &
          DU_scav(:,:) = DU_scav(:,:) - bcnv_(:,:,ibin)/area_/icdt
      endif

      if(groupname == 'sulfate' .OR. groupname == 'SULFATE') then
       if( associated(SU_scav)) &
          SU_scav(:,:) = SU_scav(:,:) - bcnv_(:,:,ibin)/area_/icdt
      endif

      if(groupname == 'seasalt' .OR. groupname == 'SEASALT') then
       if( associated(SS_scav)) &
          SS_scav(:,:) = SS_scav(:,:) - bcnv_(:,:,ibin)/area_/icdt
      endif

      if(groupname == 'blackcarbon' .OR. groupname == 'BLACKCARBON') then
       if( associated(BC_scav)) &
          BC_scav(:,:) = BC_scav(:,:) - bcnv_(:,:,ibin)/area_/icdt
      endif

      if(groupname == 'smoke' .OR. groupname == 'SMOKE') then
       if( associated(SM_scav)) &
          SM_scav(:,:) = SM_scav(:,:) - bcnv_(:,:,ibin)/area_/icdt
      endif

      if(groupname == 'ash' .OR. groupname == 'ASH') then
       if( associated(ASH_scav)) &
          ASH_scav(:,:) = ASH_scav(:,:) - bcnv_(:,:,ibin)/area_/icdt
      endif
     endif

    enddo  ! NBIN

   enddo   ! NELEM

   deallocate(cmfmc_, qccu_, dtrain_, tc_, airmass_, &
              delz_, vud_, delp_, airmol_, bcnv_, tmpu_, ple_, &
              area_, frlake_, frocean_, frseaice_, __STAT__ )


  RETURN

 end subroutine CARMA_Convection



!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CARMA_ComputeDiags -- Compute some diagnostics
!
! !INTERFACE:
!

   SUBROUTINE CARMA_ComputeDiags ( gcCARMA, qa, impChem, expChem, nymd, nhms, cdt, &
                                   rc )

   IMPLICIT none

! !INPUT PARAMETERS:

   INTEGER, INTENT(IN) :: nymd, nhms		       ! Time from AGCM
   REAL,    INTENT(IN) :: cdt			       ! Chemistry time step (secs)

! !OUTPUT PARAMETERS:

   TYPE(CARMA_GridComp), INTENT(INOUT) :: gcCARMA    ! Grid Component
   TYPE(ESMF_State),   INTENT(INOUT)   :: impChem    ! Import State
   TYPE(ESMF_State),   INTENT(INOUT)   :: expChem    ! Export State
   TYPE(Chem_Array), pointer           :: qa(:)   ! tracer array will go here

   INTEGER, INTENT(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Parses the CARMA registry and handles doing emissions to main
!               tracer array.
!
! !REVISION HISTORY:
!
!  10Mar2010 Colarco   First crack.
!
!EOP
!-------------------------------------------------------------------------

   CHARACTER(LEN=*), PARAMETER :: myname = 'CARMA_ComputeDiags'
   CHARACTER(LEN=255) :: groupname, elemname, qname

   INTEGER :: ielem, ibin, igroup, ienconc
   CHARACTER(LEN=*), PARAMETER :: IAm = 'CARMA_UtilMod'
   INTEGER :: STATUS
   INTEGER :: i1, i2, im, j1, j2, jm, km, ijl, n, i, j, k, idx
   INTEGER :: nymd1, nhms1
   INTEGER :: n1, n2
   logical :: do_angstrom
   REAL(kind=f) :: dtime
   REAL    :: delp

   REAL, POINTER, DIMENSION(:,:,:) :: p, ple, rhoa, tmpu, zc, zl, q, zle, &
                                      rh, u, v
   REAL, POINTER, DIMENSION(:,:)   :: gwettop, fraclake, oro, u10m, v10m, &
                                      ustar, pblh, z0h, shflux, precc, precl
   real, pointer, dimension(:,:,:) :: du_mass, su_mass, ss_mass, bc_mass, ash_mass, sm_mass
   real, pointer, dimension(:,:,:) :: du_conc, su_conc, ss_conc, bc_conc, ash_conc, sm_conc
   real, pointer, dimension(:,:)   :: du_fluxu, su_fluxu, ss_fluxu, bc_fluxu, ash_fluxu, sm_fluxu
   real, pointer, dimension(:,:)   :: du_fluxv, su_fluxv, ss_fluxv, bc_fluxv, ash_fluxv, sm_fluxv
   real, pointer, dimension(:,:)   :: du_smass, su_smass, ss_smass, bc_smass, ash_smass, sm_smass
   real, pointer, dimension(:,:)   :: du_cmass, su_cmass, ss_cmass, bc_cmass, ash_cmass, sm_cmass
!  Columnar optical quantities: Extinction AOT (??_exttau @ 550 nm), 
!                               Scattering AOT (??_scatau @ 550 nm),
!                               Angstrom parameter (??_angstr for 470 and 870 nm wavelength pair)
   real, pointer, dimension(:,:)   :: du_exttau, su_exttau, ss_exttau, bc_exttau, ash_exttau, sm_exttau
   real, pointer, dimension(:,:)   :: du_scatau, su_scatau, ss_scatau, bc_scatau, ash_scatau, sm_scatau
   real, pointer, dimension(:,:)   :: du_angstr, su_angstr, ss_angstr, bc_angstr, ash_angstr, sm_angstr
   real, pointer, dimension(:,:)   :: totexttau, totscatau, totangstr
!  Vertical optical quantities: Extinction coefficient (??_extcoef @ 550 nm in m-1),
!                               Scattering coefficient (??_scacoef @ 550 nm in m-1)
   real, pointer, dimension(:,:,:) :: du_extcoef, su_extcoef, ss_extcoef, bc_extcoef, ash_extcoef, sm_extcoef
   real, pointer, dimension(:,:,:) :: du_scacoef, su_scacoef, ss_scacoef, bc_scacoef, ash_scacoef, sm_scacoef
!   type(Chem_Array), pointer       :: du_cm(:)

   type(CARMA_Registry), pointer :: reg => null()
   type(carma_type), pointer     :: r => null()
   type(Chem_Mie), pointer       :: mie => null()

!  We are using the CARMA constants here (CGS units) but need
!  MKS values to go back to GEOS-5
   REAL, PARAMETER    :: grav_mks = grav/100.

!  Optical calculations
   real :: ilam550, ilam470, ilam870
   real :: tau, ssa
   real, allocatable, dimension(:,:) :: tau470, tau870, tottau470, tottau870

   rc = 0
   i1 = gcCARMA%i1
   i2 = gcCARMA%i2
   im = gcCARMA%im
   
   j1 = gcCARMA%j1
   j2 = gcCARMA%j2
   jm = gcCARMA%jm
   
   km = gcCARMA%km
   
   ijl = (i2-i1+1)*(j2-j1+1)

   dtime = cdt

   r   => gcCARMA%carma
   reg => gcCARMA%CARMAreg
   mie => gcCARMA%CARMAmie

   n1 =  1
   n2 =  gcCARMA%CARMAreg%nq

!  Get Imports
!  -----------
   call MAPL_GetPointer ( impChem, rhoa, 'AIRDENS', __RC__)
   call MAPL_GetPointer ( impChem, ple, 'PLE', __RC__)
   call MAPL_GetPointer ( impChem, zle, 'ZLE', __RC__)
   call MAPL_GetPointer ( impChem, q, 'Q', __RC__)
   call MAPL_GetPointer ( impChem, rh, 'RH2', __RC__)
   call MAPL_GetPointer ( impChem, tmpu, 'T', __RC__)
   call MAPL_GetPointer ( impChem, ustar, 'USTAR', __RC__)
   call MAPL_GetPointer ( impChem, fraclake, 'FRLAKE', __RC__)
   call MAPL_GetPointer ( impChem, gwettop, 'WET1', __RC__)
   call MAPL_GetPointer ( impChem, u10m, 'U10M', __RC__)
   call MAPL_GetPointer ( impChem, v10m, 'V10M', __RC__)
   call MAPL_GetPointer ( impChem, pblh, 'ZPBL', __RC__)
   call MAPL_GetPointer ( impChem, z0h, 'Z0H', __RC__)
   call MAPL_GetPointer ( impChem, shflux, 'SH', __RC__)
   call MAPL_GetPointer ( impChem, precl, 'NCN_PRCP', __RC__)
   call MAPL_GetPointer ( impChem, precc, 'CN_PRCP', __RC__)
   call MAPL_GetPointer ( impChem, oro, 'LWI', __RC__)
   call MAPL_GetPointer ( impChem, u, 'U', __RC__)
   call MAPL_GetPointer ( impChem, v, 'V', __RC__)

!  Get Exports
!  -----------
   call MAPL_GetPointer(expChem, du_mass,   'CARMA_DUMASS',   __RC__)
   call MAPL_GetPointer(expChem, su_mass,   'CARMA_SUMASS',   __RC__)
   call MAPL_GetPointer(expChem, ss_mass,   'CARMA_SSMASS',   __RC__)
   call MAPL_GetPointer(expChem, bc_mass,   'CARMA_BCMASS',   __RC__)
   call MAPL_GetPointer(expChem, sm_mass,   'CARMA_SMMASS',   __RC__)
   call MAPL_GetPointer(expChem, ash_mass,   'CARMA_ASHMASS',   __RC__)
   call MAPL_GetPointer(expChem, du_conc,   'CARMA_DUCONC',   __RC__)
   call MAPL_GetPointer(expChem, su_conc,   'CARMA_SUCONC',   __RC__)
   call MAPL_GetPointer(expChem, ss_conc,   'CARMA_SSCONC',   __RC__)
   call MAPL_GetPointer(expChem, bc_conc,   'CARMA_BCCONC',   __RC__)
   call MAPL_GetPointer(expChem, sm_conc,   'CARMA_SMCONC',   __RC__)
   call MAPL_GetPointer(expChem, ash_conc,   'CARMA_ASHCONC',   __RC__)
   call MAPL_GetPointer(expChem, du_fluxu,   'CARMA_DUFLUXU',   __RC__)
   call MAPL_GetPointer(expChem, su_fluxu,   'CARMA_SUFLUXU',   __RC__)
   call MAPL_GetPointer(expChem, ss_fluxu,   'CARMA_SSFLUXU',   __RC__)
   call MAPL_GetPointer(expChem, bc_fluxu,   'CARMA_BCFLUXU',   __RC__)
   call MAPL_GetPointer(expChem, sm_fluxu,   'CARMA_SMFLUXU',   __RC__)
   call MAPL_GetPointer(expChem, ash_fluxu,   'CARMA_ASHFLUXU',   __RC__)
   call MAPL_GetPointer(expChem, du_fluxv,   'CARMA_DUFLUXV',   __RC__)
   call MAPL_GetPointer(expChem, su_fluxv,   'CARMA_SUFLUXV',   __RC__)
   call MAPL_GetPointer(expChem, ss_fluxv,   'CARMA_SSFLUXV',   __RC__)
   call MAPL_GetPointer(expChem, bc_fluxv,   'CARMA_BCFLUXV',   __RC__)
   call MAPL_GetPointer(expChem, sm_fluxv,   'CARMA_SMFLUXV',   __RC__)
   call MAPL_GetPointer(expChem, ash_fluxv,   'CARMA_ASHFLUXV',   __RC__)
   call MAPL_GetPointer(expChem, du_smass,   'CARMA_DUSMASS',   __RC__)
   call MAPL_GetPointer(expChem, su_smass,   'CARMA_SUSMASS',   __RC__)
   call MAPL_GetPointer(expChem, ss_smass,   'CARMA_SSSMASS',   __RC__)
   call MAPL_GetPointer(expChem, bc_smass,   'CARMA_BCSMASS',   __RC__)
   call MAPL_GetPointer(expChem, sm_smass,   'CARMA_SMSMASS',   __RC__)
   call MAPL_GetPointer(expChem, ash_smass,   'CARMA_ASHSMASS',   __RC__)
   call MAPL_GetPointer(expChem, du_cmass,   'CARMA_DUCMASS',   __RC__)
   call MAPL_GetPointer(expChem, su_cmass,   'CARMA_SUCMASS',   __RC__)
   call MAPL_GetPointer(expChem, ss_cmass,   'CARMA_SSCMASS',   __RC__)
   call MAPL_GetPointer(expChem, bc_cmass,   'CARMA_BCCMASS',   __RC__)
   call MAPL_GetPointer(expChem, sm_cmass,   'CARMA_SMCMASS',   __RC__)
   call MAPL_GetPointer(expChem, ash_cmass,   'CARMA_ASHCMASS',   __RC__)
   call MAPL_GetPointer(expChem, du_exttau,   'CARMA_DUEXTTAU',   __RC__)
   call MAPL_GetPointer(expChem, su_exttau,   'CARMA_SUEXTTAU',   __RC__)
   call MAPL_GetPointer(expChem, ss_exttau,   'CARMA_SSEXTTAU',   __RC__)
   call MAPL_GetPointer(expChem, bc_exttau,   'CARMA_BCEXTTAU',   __RC__)
   call MAPL_GetPointer(expChem, sm_exttau,   'CARMA_SMEXTTAU',   __RC__)
   call MAPL_GetPointer(expChem, ash_exttau,   'CARMA_ASHEXTTAU',   __RC__)
   call MAPL_GetPointer(expChem, du_scatau,   'CARMA_DUSCATAU',   __RC__)
   call MAPL_GetPointer(expChem, su_scatau,   'CARMA_SUSCATAU',   __RC__)
   call MAPL_GetPointer(expChem, ss_scatau,   'CARMA_SSSCATAU',   __RC__)
   call MAPL_GetPointer(expChem, bc_scatau,   'CARMA_BCSCATAU',   __RC__)
   call MAPL_GetPointer(expChem, sm_scatau,   'CARMA_SMSCATAU',   __RC__)
   call MAPL_GetPointer(expChem, ash_scatau,   'CARMA_ASHSCATAU',   __RC__)
   call MAPL_GetPointer(expChem, du_angstr,   'CARMA_DUANGSTR',   __RC__)
   call MAPL_GetPointer(expChem, su_angstr,   'CARMA_SUANGSTR',   __RC__)
   call MAPL_GetPointer(expChem, ss_angstr,   'CARMA_SSANGSTR',   __RC__)
   call MAPL_GetPointer(expChem, bc_angstr,   'CARMA_BCANGSTR',   __RC__)
   call MAPL_GetPointer(expChem, sm_angstr,   'CARMA_SMANGSTR',   __RC__)
   call MAPL_GetPointer(expChem, ash_angstr,   'CARMA_ASHANGSTR',   __RC__)
   call MAPL_GetPointer(expChem, totexttau,   'CARMA_TOTEXTTAU',   __RC__)
   call MAPL_GetPointer(expChem, totscatau,   'CARMA_TOTSCATAU',   __RC__)
   call MAPL_GetPointer(expChem, totangstr,   'CARMA_TOTANGSTR',   __RC__)
   call MAPL_GetPointer(expChem, du_extcoef,  'CARMA_DUEXTCOEF',  __RC__)
   call MAPL_GetPointer(expChem, du_scacoef,  'CARMA_DUSCACOEF',  __RC__)
   call MAPL_GetPointer(expChem, su_extcoef,  'CARMA_SUEXTCOEF',  __RC__)
   call MAPL_GetPointer(expChem, su_scacoef,  'CARMA_SUSCACOEF',  __RC__)
   call MAPL_GetPointer(expChem, ss_extcoef,  'CARMA_SSEXTCOEF',  __RC__)
   call MAPL_GetPointer(expChem, ss_scacoef,  'CARMA_SSSCACOEF',  __RC__)
   call MAPL_GetPointer(expChem, bc_extcoef,  'CARMA_BCEXTCOEF',  __RC__)
   call MAPL_GetPointer(expChem, bc_scacoef,  'CARMA_BCSCACOEF',  __RC__)
   call MAPL_GetPointer(expChem, sm_extcoef,  'CARMA_SMEXTCOEF',  __RC__)
   call MAPL_GetPointer(expChem, sm_scacoef,  'CARMA_SMSCACOEF',  __RC__)
   call MAPL_GetPointer(expChem, ash_extcoef,  'CARMA_ASHEXTCOEF',  __RC__)
   call MAPL_GetPointer(expChem, ash_scacoef,  'CARMA_ASHSCACOEF',  __RC__)
!   call MAPL_GetPointer(expChem, du_cm,   'CARMA_DUCM',   __RC__)

!  Routine computes some basic diagnostics

!  Initialize Quantities
!  Mass mixing ratio and concentration (size integrated)
!  ---------------------------------------------------
   if( associated(DU_mass)) DU_mass(:,:,:) = 0.
   if( associated(SU_mass)) SU_mass(:,:,:) = 0.
   if( associated(SS_mass)) SS_mass(:,:,:) = 0.
   if( associated(BC_mass)) BC_mass(:,:,:) = 0.
   if( associated(SM_mass)) SM_mass(:,:,:) = 0.
   if( associated(ASH_mass)) ASH_mass(:,:,:) = 0.
   if( associated(DU_conc)) DU_conc(:,:,:) = 0.
   if( associated(SU_conc)) SU_conc(:,:,:) = 0.
   if( associated(SS_conc)) SS_conc(:,:,:) = 0.
   if( associated(BC_conc)) BC_conc(:,:,:) = 0.
   if( associated(SM_conc)) SM_conc(:,:,:) = 0.
   if( associated(ASH_conc)) ASH_conc(:,:,:) = 0.

!  Mass Fluxes (size integrated)
!  -----------------------------
   if( associated(DU_fluxu)) DU_fluxu(:,:) = 0.
   if( associated(SU_fluxu)) SU_fluxu(:,:) = 0.
   if( associated(SS_fluxu)) SS_fluxu(:,:) = 0.
   if( associated(BC_fluxu)) BC_fluxu(:,:) = 0.
   if( associated(SM_fluxu)) SM_fluxu(:,:) = 0.
   if( associated(ASH_fluxu)) ASH_fluxu(:,:) = 0.
   if( associated(DU_fluxv)) DU_fluxv(:,:) = 0.
   if( associated(SU_fluxv)) SU_fluxv(:,:) = 0.
   if( associated(SS_fluxv)) SS_fluxv(:,:) = 0.
   if( associated(BC_fluxv)) BC_fluxv(:,:) = 0.
   if( associated(SM_fluxv)) SM_fluxv(:,:) = 0.
   if( associated(ASH_fluxv)) ASH_fluxv(:,:) = 0.


!  Surface Concentration (size integrated)
!  ---------------------------------------
   if( associated(DU_smass)) DU_smass(:,:) = 0.
   if( associated(SU_smass)) SU_smass(:,:) = 0.
   if( associated(SS_smass)) SS_smass(:,:) = 0.
   if( associated(BC_smass)) BC_smass(:,:) = 0.
   if( associated(SM_smass)) SM_smass(:,:) = 0.
   if( associated(ASH_smass)) ASH_smass(:,:) = 0.

!  Column Loading (size integrated)
!  --------------------------------
   if( associated(DU_cmass)) DU_cmass(:,:) = 0.
   if( associated(SU_cmass)) SU_cmass(:,:) = 0.
   if( associated(SS_cmass)) SS_cmass(:,:) = 0.
   if( associated(BC_cmass)) BC_cmass(:,:) = 0.
   if( associated(SM_cmass)) SM_cmass(:,:) = 0.
   if( associated(ASH_cmass)) ASH_cmass(:,:) = 0.


   do ielem = 1, reg%NELEM
    igroup = reg%igroup(ielem)
    groupname = uppercase(trim(reg%groupname(igroup)))
    elemname  = uppercase(trim(reg%elemname(ielem)))

    do ibin = 1, reg%NBIN
     n = n1 + (ielem-1)*reg%NBIN + ibin - 1

!    DUST DIAGNOSTICS
!    ----------------
     if(  groupname == 'DUST' .or. &
        ( groupname == 'MIXEDP' .AND. elemname  == 'DUST'      )) then

        k = km
        if(associated(DU_smass))  DU_smass = DU_smass + qa(n)%data3d(:,:,k) * rhoa(:,:,k)
        if(associated(DU_mass) )  DU_mass = DU_mass + qa(n)%data3d
        if(associated(DU_conc) )  DU_conc = DU_conc + qa(n)%data3d*rhoa
        if(associated(DU_fluxu))  then
         do k = 1, km
          DU_fluxu = DU_fluxu + qa(n)%data3d(:,:,k) * (ple(:,:,k)-ple(:,:,k-1))/grav_mks*u(:,:,k)
         enddo
        endif
        if(associated(DU_fluxv))  then
         do k = 1, km
          DU_fluxv = DU_fluxv + qa(n)%data3d(:,:,k) * (ple(:,:,k)-ple(:,:,k-1))/grav_mks*v(:,:,k)
         enddo
        endif
        if(associated(DU_cmass))  then
         do k = 1, km
          DU_cmass = DU_cmass + qa(n)%data3d(:,:,k) * (ple(:,:,k)-ple(:,:,k-1))/grav_mks
         end do
        endif

     endif

!    SEASALT DIAGNOSTICS
!    -------------------
     if(  groupname == 'SEASALT' .or. &
        ( groupname == 'MIXEDP' .AND. elemname  == 'SEASALT'      )) then

        k = km
        if(associated(SS_smass))  SS_smass = SS_smass + qa(n)%data3d(:,:,k) * rhoa(:,:,k)
        if(associated(SS_mass) )  SS_mass = SS_mass + qa(n)%data3d
        if(associated(SS_conc) )  SS_conc = SS_conc + qa(n)%data3d*rhoa
        if(associated(SS_fluxu))  then
         do k = 1, km
          SS_fluxu = SS_fluxu + qa(n)%data3d(:,:,k) * (ple(:,:,k)-ple(:,:,k-1))/grav_mks*u(:,:,k)
         enddo
        endif
        if(associated(SS_fluxv))  then
         do k = 1, km
          SS_fluxv = SS_fluxv + qa(n)%data3d(:,:,k) * (ple(:,:,k)-ple(:,:,k-1))/grav_mks*v(:,:,k)
         enddo
        endif
        if(associated(SS_cmass))  then
         do k = 1, km
          SS_cmass = SS_cmass + qa(n)%data3d(:,:,k) * (ple(:,:,k)-ple(:,:,k-1))/grav_mks
         end do
        endif

     endif

!    SULFATE DIAGNOSTICS
!    -------------------
     if(  groupname == 'SULFATE' .or. &
        ( groupname == 'MIXEDP' .AND. elemname  == 'SULFATE'      )) then

        k = km
        if(associated(SU_smass))  SU_smass = SU_smass + qa(n)%data3d(:,:,k) * rhoa(:,:,k)
        if(associated(SU_mass) )  SU_mass = SU_mass + qa(n)%data3d
        if(associated(SU_conc) )  SU_conc = SU_conc + qa(n)%data3d*rhoa
        if(associated(SU_fluxu))  then
         do k = 1, km
          SU_fluxu = SU_fluxu + qa(n)%data3d(:,:,k) * (ple(:,:,k)-ple(:,:,k-1))/grav_mks*u(:,:,k)
         enddo
        endif
        if(associated(SU_fluxv))  then
         do k = 1, km
          SU_fluxv = SU_fluxv + qa(n)%data3d(:,:,k) * (ple(:,:,k)-ple(:,:,k-1))/grav_mks*v(:,:,k)
         enddo
        endif
        if(associated(SU_cmass))  then
         do k = 1, km
          SU_cmass = SU_cmass + qa(n)%data3d(:,:,k) * (ple(:,:,k)-ple(:,:,k-1))/grav_mks
         end do
        endif

     endif

!    BLACKCARBON DIAGNOSTICS
!    -----------------------
     if(  groupname == 'BLACKCARBON' .or. &
        ( groupname == 'MIXEDP' .AND. elemname  == 'BLACKCARBON'      )) then

        k = km
        if(associated(BC_smass))  BC_smass = BC_smass + qa(n)%data3d(:,:,k) * rhoa(:,:,k)
        if(associated(BC_mass) )  BC_mass = BC_mass + qa(n)%data3d
        if(associated(BC_conc) )  BC_conc = BC_conc + qa(n)%data3d*rhoa
        if(associated(BC_fluxu))  then
         do k = 1, km
          BC_fluxu = BC_fluxu + qa(n)%data3d(:,:,k) * (ple(:,:,k)-ple(:,:,k-1))/grav_mks*u(:,:,k)
         enddo
        endif
        if(associated(BC_fluxv))  then
         do k = 1, km
          BC_fluxv = BC_fluxv + qa(n)%data3d(:,:,k) * (ple(:,:,k)-ple(:,:,k-1))/grav_mks*v(:,:,k)
         enddo
        endif
        if(associated(BC_cmass))  then
         do k = 1, km
          BC_cmass = BC_cmass + qa(n)%data3d(:,:,k) * (ple(:,:,k)-ple(:,:,k-1))/grav_mks
         end do
        endif

     endif

!    SMOKE DIAGNOSTICS
!    -----------------
     if(  groupname == 'SMOKE' .or. &
        ( groupname == 'MIXEDP' .AND. elemname  == 'SMOKE'      )) then

        k = km
        if(associated(SM_smass))  SM_smass = SM_smass + qa(n)%data3d(:,:,k) * rhoa(:,:,k)
        if(associated(SM_mass) )  SM_mass = SM_mass + qa(n)%data3d
        if(associated(SM_conc) )  SM_conc = SM_conc + qa(n)%data3d*rhoa
        if(associated(SM_fluxu))  then
         do k = 1, km
          SM_fluxu = SM_fluxu + qa(n)%data3d(:,:,k) * (ple(:,:,k)-ple(:,:,k-1))/grav_mks*u(:,:,k)
         enddo
        endif
        if(associated(SM_fluxv))  then
         do k = 1, km
          SM_fluxv = SM_fluxv + qa(n)%data3d(:,:,k) * (ple(:,:,k)-ple(:,:,k-1))/grav_mks*v(:,:,k)
         enddo
        endif
        if(associated(SM_cmass))  then
         do k = 1, km
          SM_cmass = SM_cmass + qa(n)%data3d(:,:,k) * (ple(:,:,k)-ple(:,:,k-1))/grav_mks
         end do
        endif

     endif

!    ASH DIAGNOSTICS
!    -------------------
     if(  groupname == 'ASH' .or. &
        ( groupname == 'MIXEDP' .AND. elemname  == 'ASH'      )) then

        k = km
        if(associated(ASH_smass))  ASH_smass = ASH_smass + qa(n)%data3d(:,:,k) * rhoa(:,:,k)
        if(associated(ASH_mass) )  ASH_mass = ASH_mass + qa(n)%data3d
        if(associated(ASH_conc) )  ASH_conc = ASH_conc + qa(n)%data3d*rhoa
        if(associated(ASH_fluxu))  then
         do k = 1, km
          ASH_fluxu = ASH_fluxu + qa(n)%data3d(:,:,k) * (ple(:,:,k)-ple(:,:,k-1))/grav_mks*u(:,:,k)
         enddo
        endif
        if(associated(ASH_fluxv))  then
         do k = 1, km
          ASH_fluxv = ASH_fluxv + qa(n)%data3d(:,:,k) * (ple(:,:,k)-ple(:,:,k-1))/grav_mks*v(:,:,k)
         enddo
        endif
        if(associated(ASH_cmass))  then
         do k = 1, km
          ASH_cmass = ASH_cmass + qa(n)%data3d(:,:,k) * (ple(:,:,k)-ple(:,:,k-1))/grav_mks
         end do
        endif

     endif


    enddo
   enddo



!  Optical properties
!  ------------------

!  Get the wavelength indices
!  Must provide ilam550 for AOT calculation
   ilam550 = 1.
   ilam470 = 0.
   ilam870 = 0.
   if(mie%nch .gt. 1) then
    do i = 1, mie%nch
     if ( mie%channels(i) .ge. 5.49e-7 .and. &
          mie%channels(i) .le. 5.51e-7) ilam550 = i
     if ( mie%channels(i) .ge. 4.69e-7 .and. &
          mie%channels(i) .le. 4.71e-7) ilam470 = i
     if ( mie%channels(i) .ge. 8.69e-7 .and. &
          mie%channels(i) .le. 8.71e-7) ilam870 = i
    enddo
   endif

!  Do an Angstrom parameter calculation?
!  -------------------------------------
   do_angstrom = .false.
!  If both 470 and 870 channels provided (and not the same) then
!  possibly will do Angstrom parameter calculation
   if(( ilam470 .ne. 0. .and. &
        ilam870 .ne. 0. .and. &
        ilam470 .ne. ilam870) .and. &
      ( associated(DU_angstr) .or. &
        associated(BC_angstr) .or. &
        associated(BC_angstr) .or. &
        associated(SM_angstr) .or. &
        associated(ASH_angstr) .or. &
        associated(totangstr) &
      ) ) do_angstrom = .true.
   if(do_angstrom) then
    allocate(tau470(i1:i2,j1:j2), tottau470(i1:i2,j1:j2), &
             tau870(i1:i2,j1:j2), tottau870(i1:i2,j1:j2), stat=STATUS)
    VERIFY_(STATUS)
    tau470(i1:i2,j1:j2) = tiny(1.0)
    tau870(i1:i2,j1:j2) = tiny(1.0)
    tottau470(i1:i2,j1:j2) = tiny(1.0)
    tottau870(i1:i2,j1:j2) = tiny(1.0)
   endif


!  Extinction and Scattering AOD, Angstrom parameter
!  -------------------------------------------------
   if( associated(totexttau)) totexttau(:,:) = 0.
   if( associated(totscatau)) totscatau(:,:) = 0.
   if( associated(totangstr)) totangstr(:,:) = 0.

   if( associated(DU_exttau)) DU_exttau(:,:) = 0.
   if( associated(DU_scatau)) DU_scatau(:,:) = 0.
   if( associated(DU_angstr)) DU_angstr(:,:) = 0.

   if( associated(SU_exttau)) SU_exttau(:,:) = 0.
   if( associated(SU_scatau)) SU_scatau(:,:) = 0.
   if( associated(SU_angstr)) SU_angstr(:,:) = 0.

   if( associated(SS_exttau)) SS_exttau(:,:) = 0.
   if( associated(SS_scatau)) SS_scatau(:,:) = 0.
   if( associated(SS_angstr)) SS_angstr(:,:) = 0.

   if( associated(BC_exttau)) BC_exttau(:,:) = 0.
   if( associated(BC_scatau)) BC_scatau(:,:) = 0.
   if( associated(BC_angstr)) BC_angstr(:,:) = 0.

   if( associated(SM_exttau)) SM_exttau(:,:) = 0.
   if( associated(SM_scatau)) SM_scatau(:,:) = 0.
   if( associated(SM_angstr)) SM_angstr(:,:) = 0.

   if( associated(ASH_exttau)) ASH_exttau(:,:) = 0.
   if( associated(ASH_scatau)) ASH_scatau(:,:) = 0.
   if( associated(ASH_angstr)) ASH_angstr(:,:) = 0.

   if( associated(DU_extcoef)) DU_extcoef(:,:,:) = 0.
   if( associated(DU_scacoef)) DU_scacoef(:,:,:) = 0.
   if( associated(SS_extcoef)) SS_extcoef(:,:,:) = 0.
   if( associated(SS_scacoef)) SS_scacoef(:,:,:) = 0.
   if( associated(SU_extcoef)) SU_extcoef(:,:,:) = 0.
   if( associated(SU_scacoef)) SU_scacoef(:,:,:) = 0.
   if( associated(BC_extcoef)) BC_extcoef(:,:,:) = 0.
   if( associated(BC_scacoef)) BC_scacoef(:,:,:) = 0.
   if( associated(SM_extcoef)) SM_extcoef(:,:,:) = 0.
   if( associated(SM_scacoef)) SM_scacoef(:,:,:) = 0.
   if( associated(ASH_extcoef)) ASH_extcoef(:,:,:) = 0.
   if( associated(ASH_scacoef)) ASH_scacoef(:,:,:) = 0.

!  Dust
!  ----
   if( associated(DU_exttau) .or. associated(DU_scatau) .or. &
       associated(DU_extcoef) .or. associated(DU_scacoef) .or. &
       associated(DU_angstr)  ) then

     if(do_angstrom)tau470(i1:i2,j1:j2) = tiny(1.0)
     if(do_angstrom)tau870(i1:i2,j1:j2) = tiny(1.0)

     do ielem = 1, reg%NELEM

      igroup = reg%igroup(ielem)
      groupname = uppercase(trim(reg%groupname(igroup)))
      elemname  = uppercase(trim(reg%elemname(ielem)))
      if(  groupname == 'DUST' .or. &
         ( groupname == 'MIXEDP' .AND. elemname  == 'DUST'      )) then

      do ibin = 1, reg%NBIN
       n = n1 + (ielem-1)*reg%NBIN + ibin - 1

       qname = trim(reg%vname(n))
       idx = Chem_MieQueryIdx(mie,'CARMA::'//qname,rc)
       if(idx .eq. -1) cycle

         do k = 1, km
          do j = j1, j2
           do i = i1, i2

              delp = ple(i,j,k)-ple(i,j,k-1)

              call Chem_MieQuery(mie, idx, ilam550, &
                    qa(n)%data3d(i,j,k)*delp/grav_mks, &
                    rh(i,j,k), tau=tau, ssa=ssa)
              if (associated(DU_exttau)) DU_exttau(i,j) = DU_exttau(i,j) + tau
              if (associated(DU_scatau)) DU_scatau(i,j) = DU_scatau(i,j) + ssa*tau

              if( associated(DU_extcoef) ) then
                  DU_extcoef(i,j,k) = DU_extcoef(i,j,k) + &
                                      tau * (grav_mks * rhoa(i,j,k) / delp)
              endif
              if( associated(DU_scacoef) ) then
                  DU_scacoef(i,j,k) = DU_scacoef(i,j,k) + &
                                      ssa * tau * (grav_mks * rhoa(i,j,k) / delp)
              endif

              if (associated(DU_angstr) .and. do_angstrom) then
               call Chem_MieQuery(mie, idx, ilam470, &
                     qa(n)%data3d(i,j,k)*delp/grav_mks, &
                     rh(i,j,k), tau=tau)
               tau470(i,j) = tau470(i,j) + tau
               tottau470(i,j) = tottau470(i,j) + tau
               call Chem_MieQuery(mie, idx, ilam870, &
                     qa(n)%data3d(i,j,k)*delp/grav_mks, &
                     rh(i,j,k), tau=tau)
               tau870(i,j) = tau870(i,j) + tau
               tottau870(i,j) = tottau870(i,j) + tau
              endif

           enddo
          enddo
         enddo

      end do
      endif
     end do
   endif

   if (associated(DU_angstr) .and. do_angstrom) then
         DU_angstr(i1:i2,j1:j2) = &
           -log(tau470(i1:i2,j1:j2)/tau870(i1:i2,j1:j2)) / &
            log(470./870.)
   endif

!  Sulfate
!  -------
   if( associated(SU_exttau) .or. associated(SU_scatau) .or. &
       associated(SU_extcoef) .or. associated(SU_scacoef) .or. &
       associated(SU_angstr)  ) then

     if(do_angstrom)tau470(i1:i2,j1:j2) = tiny(1.0)
     if(do_angstrom)tau870(i1:i2,j1:j2) = tiny(1.0)

     do ielem = 1, reg%NELEM

      igroup = reg%igroup(ielem)
      groupname = uppercase(trim(reg%groupname(igroup)))
      elemname  = uppercase(trim(reg%elemname(ielem)))
      if(  groupname == 'SULFATE' .or. &
         ( groupname == 'MIXEDP' .AND. elemname  == 'SULFATE'      )) then

      do ibin = 1, reg%NBIN
       n = n1 + (ielem-1)*reg%NBIN + ibin - 1

       qname = trim(reg%vname(n))
       idx = Chem_MieQueryIdx(mie,'CARMA::'//qname,rc)
       if(idx .eq. -1) cycle

         do k = 1, km
          do j = j1, j2
           do i = i1, i2

              delp = ple(i,j,k)-ple(i,j,k-1)

              call Chem_MieQuery(mie, idx, ilam550, &
                    qa(n)%data3d(i,j,k)*delp/grav_mks, &
                    rh(i,j,k), tau=tau, ssa=ssa)
              if (associated(SU_exttau)) SU_exttau(i,j) = SU_exttau(i,j) + tau
              if (associated(SU_scatau)) SU_scatau(i,j) = SU_scatau(i,j) + ssa*tau

              if( associated(SU_extcoef) ) then
                  SU_extcoef(i,j,k) = SU_extcoef(i,j,k) + &
                                      tau * (grav_mks * rhoa(i,j,k) / delp)
              endif
              if( associated(SU_scacoef) ) then
                  SU_scacoef(i,j,k) = SU_scacoef(i,j,k) + &
                                      ssa * tau * (grav_mks * rhoa(i,j,k) / delp)
              endif

              if (associated(SU_angstr) .and. do_angstrom) then
               call Chem_MieQuery(mie, idx, ilam470, &
                     qa(n)%data3d(i,j,k)*delp/grav_mks, &
                     rh(i,j,k), tau=tau)
               tau470(i,j) = tau470(i,j) + tau
               tottau470(i,j) = tottau470(i,j) + tau
               call Chem_MieQuery(mie, idx, ilam870, &
                     qa(n)%data3d(i,j,k)*delp/grav_mks, &
                     rh(i,j,k), tau=tau)
               tau870(i,j) = tau870(i,j) + tau
               tottau870(i,j) = tottau870(i,j) + tau
              endif

           enddo
          enddo
         enddo

      end do
      endif
     end do
   endif

   if (associated(SU_angstr) .and. do_angstrom) then
         SU_angstr(i1:i2,j1:j2) = &
           -log(tau470(i1:i2,j1:j2)/tau870(i1:i2,j1:j2)) / &
            log(470./870.)
   endif

!  Seasalt
!  -------
   if( associated(SS_exttau) .or. associated(SS_scatau) .or. &
       associated(SS_extcoef) .or. associated(SS_scacoef) .or. &
       associated(SS_angstr)  ) then

     if(do_angstrom)tau470(i1:i2,j1:j2) = tiny(1.0)
     if(do_angstrom)tau870(i1:i2,j1:j2) = tiny(1.0)

     do ielem = 1, reg%NELEM

      igroup = reg%igroup(ielem)
      groupname = uppercase(trim(reg%groupname(igroup)))
      elemname  = uppercase(trim(reg%elemname(ielem)))
      if(  groupname == 'SEASALT' .or. &
         ( groupname == 'MIXEDP' .AND. elemname  == 'SEASALT'      )) then

      do ibin = 1, reg%NBIN
       n = n1 + (ielem-1)*reg%NBIN + ibin - 1

       qname = trim(reg%vname(n))
       idx = Chem_MieQueryIdx(mie,'CARMA::'//qname,rc)
       if(idx .eq. -1) cycle

         do k = 1, km
          do j = j1, j2
           do i = i1, i2

              delp = ple(i,j,k)-ple(i,j,k-1)

              call Chem_MieQuery(mie, idx, ilam550, &
                    qa(n)%data3d(i,j,k)*delp/grav_mks, &
                    rh(i,j,k), tau=tau, ssa=ssa)
              if (associated(SS_exttau)) SS_exttau(i,j) = SS_exttau(i,j) + tau
              if (associated(SS_scatau)) SS_scatau(i,j) = SS_scatau(i,j) + ssa*tau

              if( associated(SS_extcoef) ) then
                  SS_extcoef(i,j,k) = SS_extcoef(i,j,k) + &
                                      tau * (grav_mks * rhoa(i,j,k) / delp)
              endif
              if( associated(SS_scacoef) ) then
                  SS_scacoef(i,j,k) = SS_scacoef(i,j,k) + &
                                      ssa * tau * (grav_mks * rhoa(i,j,k) / delp)
              endif

              if (associated(SS_angstr) .and. do_angstrom) then
               call Chem_MieQuery(mie, idx, ilam470, &
                     qa(n)%data3d(i,j,k)*delp/grav_mks, &
                     rh(i,j,k), tau=tau)
               tau470(i,j) = tau470(i,j) + tau
               tottau470(i,j) = tottau470(i,j) + tau
               call Chem_MieQuery(mie, idx, ilam870, &
                     qa(n)%data3d(i,j,k)*delp/grav_mks, &
                     rh(i,j,k), tau=tau)
               tau870(i,j) = tau870(i,j) + tau
               tottau870(i,j) = tottau870(i,j) + tau
              endif

           enddo
          enddo
         enddo

      end do
      endif
     end do
   endif

   if (associated(SS_angstr) .and. do_angstrom) then
         SS_angstr(i1:i2,j1:j2) = &
           -log(tau470(i1:i2,j1:j2)/tau870(i1:i2,j1:j2)) / &
            log(470./870.)
   endif


!  Black Carbon
!  ------------
   if( associated(BC_exttau) .or. associated(BC_scatau) .or. &
       associated(BC_extcoef) .or. associated(BC_scacoef) .or. &
       associated(BC_angstr)  ) then

     if(do_angstrom)tau470(i1:i2,j1:j2) = tiny(1.0)
     if(do_angstrom)tau870(i1:i2,j1:j2) = tiny(1.0)

     do ielem = 1, reg%NELEM

      igroup = reg%igroup(ielem)
      groupname = uppercase(trim(reg%groupname(igroup)))
      elemname  = uppercase(trim(reg%elemname(ielem)))
      if(  groupname == 'BLACKCARBON' .or. &
         ( groupname == 'MIXEDP' .AND. elemname  == 'BLACKCARBON'      )) then

      do ibin = 1, reg%NBIN
       n = n1 + (ielem-1)*reg%NBIN + ibin - 1

       qname = trim(reg%vname(n))
       idx = Chem_MieQueryIdx(mie,'CARMA::'//qname,rc)
       if(idx .eq. -1) cycle

         do k = 1, km
          do j = j1, j2
           do i = i1, i2

              delp = ple(i,j,k)-ple(i,j,k-1)

              call Chem_MieQuery(mie, idx, ilam550, &
                    qa(n)%data3d(i,j,k)*delp/grav_mks, &
                    rh(i,j,k), tau=tau, ssa=ssa)
              if (associated(BC_exttau)) BC_exttau(i,j) = BC_exttau(i,j) + tau
              if (associated(BC_scatau)) BC_scatau(i,j) = BC_scatau(i,j) + ssa*tau

              if( associated(BC_extcoef) ) then
                  BC_extcoef(i,j,k) = BC_extcoef(i,j,k) + &
                                      tau * (grav_mks * rhoa(i,j,k) / delp)
              endif
              if( associated(BC_scacoef) ) then
                  BC_scacoef(i,j,k) = BC_scacoef(i,j,k) + &
                                      ssa * tau * (grav_mks * rhoa(i,j,k) / delp)
              endif

              if (associated(BC_angstr) .and. do_angstrom) then
               call Chem_MieQuery(mie, idx, ilam470, &
                     qa(n)%data3d(i,j,k)*delp/grav_mks, &
                     rh(i,j,k), tau=tau)
               tau470(i,j) = tau470(i,j) + tau
               tottau470(i,j) = tottau470(i,j) + tau
               call Chem_MieQuery(mie, idx, ilam870, &
                     qa(n)%data3d(i,j,k)*delp/grav_mks, &
                     rh(i,j,k), tau=tau)
               tau870(i,j) = tau870(i,j) + tau
               tottau870(i,j) = tottau870(i,j) + tau
              endif

           enddo
          enddo
         enddo

      end do
      endif
     end do
   endif

   if (associated(BC_angstr) .and. do_angstrom) then
         BC_angstr(i1:i2,j1:j2) = &
           -log(tau470(i1:i2,j1:j2)/tau870(i1:i2,j1:j2)) / &
            log(470./870.)
   endif


!  Smoke
!  -----
   if( associated(SM_exttau) .or. associated(SM_scatau) .or. &
       associated(SM_extcoef) .or. associated(SM_scacoef) .or. &
       associated(SM_angstr)  ) then

     if(do_angstrom)tau470(i1:i2,j1:j2) = tiny(1.0)
     if(do_angstrom)tau870(i1:i2,j1:j2) = tiny(1.0)

     do ielem = 1, reg%NELEM

      igroup = reg%igroup(ielem)
      groupname = uppercase(trim(reg%groupname(igroup)))
      elemname  = uppercase(trim(reg%elemname(ielem)))
      if(  groupname == 'SMOKE' .or. &
         ( groupname == 'MIXEDP' .AND. elemname  == 'SMOKE'      )) then

      do ibin = 1, reg%NBIN
       n = n1 + (ielem-1)*reg%NBIN + ibin - 1

       qname = trim(reg%vname(n))
       idx = Chem_MieQueryIdx(mie,'CARMA::'//qname,rc)
       if(idx .eq. -1) cycle

         do k = 1, km
          do j = j1, j2
           do i = i1, i2

              delp = ple(i,j,k)-ple(i,j,k-1)

              call Chem_MieQuery(mie, idx, ilam550, &
                    qa(n)%data3d(i,j,k)*delp/grav_mks, &
                    rh(i,j,k), tau=tau, ssa=ssa)
              if (associated(SM_exttau)) SM_exttau(i,j) = SM_exttau(i,j) + tau
              if (associated(SM_scatau)) SM_scatau(i,j) = SM_scatau(i,j) + ssa*tau

              if( associated(SM_extcoef) ) then
                  SM_extcoef(i,j,k) = SM_extcoef(i,j,k) + &
                                      tau * (grav_mks * rhoa(i,j,k) / delp)
              endif
              if( associated(SM_scacoef) ) then
                  SM_scacoef(i,j,k) = SM_scacoef(i,j,k) + &
                                      ssa * tau * (grav_mks * rhoa(i,j,k) / delp)
              endif

              if (associated(SM_angstr) .and. do_angstrom) then
               call Chem_MieQuery(mie, idx, ilam470, &
                     qa(n)%data3d(i,j,k)*delp/grav_mks, &
                     rh(i,j,k), tau=tau)
               tau470(i,j) = tau470(i,j) + tau
               tottau470(i,j) = tottau470(i,j) + tau
               call Chem_MieQuery(mie, idx, ilam870, &
                     qa(n)%data3d(i,j,k)*delp/grav_mks, &
                     rh(i,j,k), tau=tau)
               tau870(i,j) = tau870(i,j) + tau
               tottau870(i,j) = tottau870(i,j) + tau
              endif

           enddo
          enddo
         enddo

      end do
      endif
     end do
   endif

   if (associated(SM_angstr) .and. do_angstrom) then
         SM_angstr(i1:i2,j1:j2) = &
           -log(tau470(i1:i2,j1:j2)/tau870(i1:i2,j1:j2)) / &
            log(470./870.)
   endif

!    Totals
!    ------
       if  ( associated(totexttau)) then
        if ( associated(DU_exttau)) totexttau = totexttau + DU_exttau
        if ( associated(SU_exttau)) totexttau = totexttau + SU_exttau
        if ( associated(SS_exttau)) totexttau = totexttau + SS_exttau
        if ( associated(BC_exttau)) totexttau = totexttau + BC_exttau
        if ( associated(SM_exttau)) totexttau = totexttau + SM_exttau
       endif
       if  ( associated(totscatau)) then
        if ( associated(DU_scatau)) totscatau = totscatau + DU_scatau
        if ( associated(SU_scatau)) totscatau = totscatau + SU_scatau
        if ( associated(SS_scatau)) totscatau = totscatau + SS_scatau
        if ( associated(BC_scatau)) totscatau = totscatau + BC_scatau
        if ( associated(SM_scatau)) totscatau = totscatau + SM_scatau
       endif
       if (associated(totangstr) .and. do_angstrom) then
         totangstr(i1:i2,j1:j2) = &
           -log(tottau470(i1:i2,j1:j2)/tottau870(i1:i2,j1:j2)) / &
            log(470./870.)
       endif

   if(do_angstrom) then
    deallocate(tau470, tottau470, tau870, tottau870, stat=STATUS)
    VERIFY_(STATUS)
   endif


  RETURN

 end subroutine CARMA_ComputeDiags



!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CARMA_GetMieTables -- Get GEOS-5 GOCART-style Mie Look Up Tables
!
! !INTERFACE:
!

   SUBROUTINE CARMA_GetMieTables ( gcCARMA, rc )

   IMPLICIT none

! !INPUT/OUTPUT PARAMETERS:
   TYPE(CARMA_GridComp), INTENT(INOUT) :: gcCARMA    ! Grid Component
   INTEGER, INTENT(out) ::  rc                       ! Error return code:
                                                     !  0 - all is well
                                                     !  1 - 

! !DESCRIPTION: From CARMA registry create appropriate GEOS-5 style
!               Mie lookup tables
!
! !REVISION HISTORY:
!
!  10Jun2011 Colarco   First crack.
!
!EOP
!-------------------------------------------------------------------------

!  Locals
   type(CARMA_Registry), pointer :: reg => null()
   type(Chem_Mie), pointer       :: mie => null()
   integer :: iq, iq0, STATUS, ielem, igroup, ibin, n, n1
   CHARACTER(LEN=*), PARAMETER :: IAm = 'CARMA_GetMieTables'
   character(len=255)          :: groupname, elemname

   rc = 0

   reg => gcCARMA%CARMAreg
   allocate( gcCARMA%CARMAmie, stat=STATUS)
   VERIFY_(STATUS)
   mie => gcCARMA%CARMAmie
   
   mie%nq = reg%nq
   allocate(mie%vname( mie%nq) )
   allocate(mie%vindex(mie%nq) )
   allocate(mie%vtable(mie%nq) )
   do iq = 1, mie%nq
    mie%vindex(iq) = -1
    mie%vname(iq)  = trim(reg%vname(iq))
   enddo

   mie%rcfile = reg%rcfilen

   mie%nch      = reg%nchannels
   mie%nmom     = reg%nmoments
   allocate( mie%channels(mie%nch), stat=STATUS)
   VERIFY_(STATUS)
   mie%channels = reg%channels

   mie%du_optics_file = reg%du_optics_file
   mie%ss_optics_file = reg%ss_optics_file
   mie%bc_optics_file = reg%bc_optics_file
   mie%oc_optics_file = reg%sm_optics_file  ! Note I am using OC table hook for smoke
   mie%su_optics_file = reg%su_optics_file

!  Allocate and fill Mie tables
   allocate( mie%mie_DU, mie%mie_SS, mie%mie_BC, mie%mie_OC, mie%mie_SU, __STAT__)

   mie%mie_DU = Chem_MieTableCreate(mie%du_optics_file, rc)
   if ( rc /= 0 ) return
   mie%mie_SS = Chem_MieTableCreate(mie%ss_optics_file, rc)
   if ( rc /= 0 ) return
   mie%mie_BC = Chem_MieTableCreate(mie%bc_optics_file, rc)
   if ( rc /= 0 ) return
   mie%mie_OC = Chem_MieTableCreate(mie%oc_optics_file, rc)
   if ( rc /= 0 ) return
   mie%mie_SU = Chem_MieTableCreate(mie%su_optics_file, rc)
   if ( rc /= 0 ) return

   call Chem_MieTableRead(mie%mie_DU,mie%nch,mie%channels,rc,nmom=mie%nmom)
   if ( rc /= 0 ) return
   call Chem_MieTableRead(mie%mie_SS,mie%nch,mie%channels,rc,nmom=mie%nmom)
   if ( rc /= 0 ) return
   call Chem_MieTableRead(mie%mie_BC,mie%nch,mie%channels,rc,nmom=mie%nmom)
   if ( rc /= 0 ) return
   call Chem_MieTableRead(mie%mie_OC,mie%nch,mie%channels,rc,nmom=mie%nmom)
   if ( rc /= 0 ) return
   call Chem_MieTableRead(mie%mie_SU,mie%nch,mie%channels,rc,nmom=mie%nmom)
   if ( rc /= 0 ) return

!  Map the mie tables to the particular tracers
   do ielem = 1, reg%NELEM
      igroup = reg%igroup(ielem)
      groupname = uppercase(trim(reg%groupname(igroup)))
      elemname  = uppercase(trim(reg%elemname(ielem)))
      do ibin = 1, reg%NBIN
       iq = (ielem-1)*reg%NBIN + ibin
       if(  groupname == 'DUST' .OR. groupname == 'ASH' .OR. &
          ( groupname == 'MIXEDP' .AND. elemname  == 'DUST'     ) .OR. &
          ( groupname == 'MIXEDP' .AND. elemname  == 'ASH'      ) ) then
        mie%vindex(iq) = ibin
        mie%vtable(iq) = mie%mie_DU
       endif
       if(  groupname == 'SEASALT' .OR. &
          ( groupname == 'MIXEDP' .AND. elemname  == 'SEASALT'  ) ) then
        mie%vindex(iq) = ibin
        mie%vtable(iq) = mie%mie_SS
       endif
       if(  groupname == 'BLACKCARBON' .OR. &
          ( groupname == 'MIXEDP' .AND. elemname  == 'BLACKCARBON'  ) ) then
        mie%vindex(iq) = ibin
        mie%vtable(iq) = mie%mie_BC
       endif
       if(  groupname == 'SMOKE' .OR. &
          ( groupname == 'MIXEDP' .AND. elemname  == 'SMOKE'  ) ) then
        mie%vindex(iq) = ibin
        mie%vtable(iq) = mie%mie_OC
       endif
       if(  groupname == 'SULFATE' .OR. &
          ( groupname == 'MIXEDP' .AND. elemname  == 'SULFATE'  ) ) then
        mie%vindex(iq) = ibin
        mie%vtable(iq) = mie%mie_SU
       endif
      end do
   end do

  RETURN

 end subroutine CARMA_GetMieTables



!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CARMA_DestroyMieTables -- Destroy GEOS-5 GOCART-style Mie Look Up Tables
!
! !INTERFACE:
!

   SUBROUTINE CARMA_DestroyMieTables ( gcCARMA, rc )

   IMPLICIT none

! !INPUT/OUTPUT PARAMETERS:
   TYPE(CARMA_GridComp), INTENT(INOUT) :: gcCARMA    ! Grid Component
   INTEGER, INTENT(out) ::  rc                       ! Error return code:
                                                     !  0 - all is well
                                                     !  1 - 

! !DESCRIPTION: Clean up Mie tables
!
! !REVISION HISTORY:
!
!  10Jun2011 Colarco   First crack.
!
!EOP
!-------------------------------------------------------------------------

!  Locals
   type(Chem_Mie), pointer       :: mie => null()
   integer :: STATUS
   CHARACTER(LEN=*), PARAMETER :: IAm = 'CARMA_DestroyMieTables'

   rc = 0

   mie => gcCARMA%CARMAmie

   call Chem_MieTableDestroy(mie%mie_DU, __RC__)
   call Chem_MieTableDestroy(mie%mie_SS, __RC__)
   call Chem_MieTableDestroy(mie%mie_BC, __RC__)
   call Chem_MieTableDestroy(mie%mie_OC, __RC__)
   call Chem_MieTableDestroy(mie%mie_SU, __RC__)

   deallocate(mie%mie_DU, mie%mie_SS, mie%mie_BC, mie%mie_OC, mie%mie_SU, __STAT__)
   deallocate(mie%vname, mie%vindex, mie%vtable, mie%channels, __STAT__)
   deallocate(mie, __STAT__)

  RETURN

 end subroutine CARMA_DestroyMieTables

   subroutine distribute_point_emissions(delp, rhoa, z_bot, z_top, emissions_point, &
                                         emissions, km)

    implicit none

    integer, intent(in) :: km

    real, dimension(:), intent(in) :: delp
    real, dimension(:), intent(in) :: rhoa
    real,               intent(in) :: emissions_point
    real, intent(in)                   :: z_bot
    real, intent(in)                   :: z_top
    real, dimension(:), intent(out):: emissions
    
!   local
    integer :: k
    integer :: k_bot, k_top
    real    :: z_
    real, dimension(km) :: z, dz, w_
    real, parameter     :: grav_mks = grav/100.
    
!   find level height
    z = 0.0
    z_= 0.0 

    do k = km, 1, -1
       dz(k) = delp(k)/rhoa(k)/grav_mks
       z_    = z_ + dz(k)
       z(k)  = z_
    end do

!   find the bottom level
    do k = km, 1, -1
       if (z(k) >= z_bot) then
           k_bot = k
           exit
       end if
    end do
            
!   find the top level
    do k = k_bot, 1, -1
       if (z(k) >= z_top) then
           k_top = k
           exit
       end if
    end do

!   find the weights
    w_ = 0

!   if (k_top > k_bot) then
!       need to bail - something went wrong here
!   end if

    if (k_bot .eq. k_top) then
        w_(k_bot) = z_top - z_bot
    else
     do k = k_bot, k_top, -1
        if ((k < k_bot) .and. (k > k_top)) then
             w_(k) = dz(k)
        else
             if (k == k_bot) then
                 w_(k) = (z(k) - z_bot)
             end if

             if (k == k_top) then
                 w_(k) = z_top - (z(k)-dz(k))
             end if
        end if
     end do
    end if
           
!   distribute emissions in the vertical 
    emissions(:) = (w_ / sum(w_)) * emissions_point

    end subroutine distribute_point_emissions


 END MODULE CARMA_UtilMod

