#include "MAPL_Generic.h"
!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  CARMA_GridCompMod --- CARMA Grid Component Class
!
! Grid Component class for the Community Aerosol and Radiation
! Model for Atmospheres aerosol/cloud microphysics package.
!
! !INTERFACE:
!

   MODULE  CARMA_GridCompMod

! !USES:

   USE ESMF
   USE MAPL_Mod
   USE Chem_Mod
   USE Chem_UtilMod
   USE m_inpak90	     ! Resource file management
   USE m_chars, only: uppercase

!  Utility Modules
   use DryDepositionMod      ! Aerosol Dry Deposition
   use WetRemovalMod         ! Aerosol Wet Removal
   use DustEmissionMod, only: KokSizeDistribution


!  CARMA Specific Methods
   use carma_precision_mod 
   use carma_constants_mod 
   use carma_enums_mod 
   use carma_types_mod 
   use carmaelement_mod
   use carmagroup_mod
   use carmagas_mod
   use carmastate_mod
   use carma_mod

   IMPLICIT NONE
   INTEGER, PARAMETER :: DBL = KIND(0.00D+00)

! !TYPES:

   PRIVATE
   PUBLIC  CARMA_GridComp       ! The CARMA object 
   PUBLIC  CARMA_Registry

! !PUBLIC MEMBER FUNCTIONS:

   PUBLIC  CARMA_GridCompInitialize
   PUBLIC  CARMA_GridCompRun
   PUBLIC  CARMA_GridCompFinalize
   PRIVATE dumpGas
   PRIVATE dumpElement
   PRIVATE dumpGroup

!
! !DESCRIPTION:
!
!  This module implements the CARMA aerosol & cloud microphysics model
!
! !REVISION HISTORY:
!
!  16Sep2003 da Silva  First crack.
!  24Jan2O05 Nielsen   Implementation of Code 916 chemistry
!  19Dec2005 d Silva   Minor portability mods.
!  30Oct2007 Nielsen   GMI Combo set up
!  18May2009 Colarco   Follow GMI setup to implement CARMA
!
!EOP
!-------------------------------------------------------------------------

  TYPE CARMA_GridComp
   CHARACTER(LEN=255) :: name = "CARMA aerosol/cloud microphysics"
   type(CARMA_Registry), pointer :: CARMAreg => null()
   type(carma_type), pointer     :: carma
   type(Chem_Mie), pointer       :: CARMAmie     ! GOCART style Mie lookup tables
   integer :: i1 = 1, i2, im, j1 = 1, j2, jm, km
   type(ESMF_grid) :: grid
   real, pointer, dimension(:,:) :: LONS, LATS
   integer :: nymd_bc = 1

!  Pointers to species specific emissions

!  Dust
   real, pointer, dimension(:,:) :: dust_source => null()

!  Smoke

!  Sulfate
   real, pointer :: vLat(:)    => null(), &
                    vLon(:)    => null(), &
                    vSO2(:)    => null(), &
                    vElev(:)   => null(), &
                    vCloud(:)  => null()

! Component derived type declarations
! -----------------------------------
!   TYPE(t_Chemistry )		:: Chemistry
 
  END TYPE CARMA_GridComp

  TYPE CARMA_Registry
     logical                     :: doing_CARMA = .false.
     integer                     :: nq
     character(len=255), pointer :: vname(:)  ! variable name (groupname::elemname::XXX)
     CHARACTER(LEN=255)          :: rcfilen = 'CARMAchem_Registry.rc'
     integer                     :: NBIN, NGROUP, NELEM, NSOLUTE, NGAS, NWAVE
     REAL(kind=f), pointer       :: RMRAT(:)        =>null(), &
                                    RMIN(:)         =>null(), &
                                    RHOP(:)         =>null(), &
                                    ESHAPE(:)       =>null(), &
                                    FSCAV(:)        =>null()
     INTEGER, pointer            :: IGROUP(:)       =>null(), &
                                    IRHSWELL(:)     =>null(), &
                                    IRHSWCOMP(:)    =>null(), &
                                    ISHAPE(:)       =>null(), &
                                    ICOMPOSITION(:) =>null(), &
                                    ITYPE(:)        =>null()
     character(len=255), pointer :: GROUPNAME(:)    =>null(), &
                                    ELEMNAME(:)     =>null()

!    Gases
     character(len=255), pointer :: GASNAME(:)      => null()
     integer, pointer            :: IGCOMP(:)       => null(), &
                                    IGVAPREQ(:)     => null()

     logical :: do_cnst_rlh = .false.
     logical :: do_coag = .false.       !! do coagulation?
     logical :: do_detrain = .false.
     logical :: do_fixedinit = .false.
     logical :: do_grow = .false.       !! do nucleation, growth and evaporation?
     logical :: do_incloud = .false.
     logical :: do_explised = .false.
     logical :: do_print_init = .false.
     logical :: do_substep = .false.    !! do substepping
     logical :: do_thermo = .false.     !! do thermodynamics
     logical :: do_vdiff = .false.      !! do Brownin diffusion
     logical :: do_vtran  = .false.     !! do sedimentation
     real(kind=f)  :: vf_const = 0._f   !! if specified and non-zero, constant
                                        !! fall velocity for all particles [cm/s]
     integer :: minsubsteps = 1         !! minimum number of substeps, default = 1
     integer :: maxsubsteps = 32        !! maximum number of substeps, default = 32
     integer :: maxretries = 16         !! maximum number of substep retries, default = 16
     real(kind=f)  :: conmax  = 0.1_f   !! minimum relative concentration to 
                                        !! consider, default = 1e-1
!    Species specific information

!    Dust
     real               :: dust_emissions_fudgefactor
     real, pointer      :: dmass_dust(:) => null()  !! dust emission size distribution

!    Sea Salt
     real               :: seasalt_emissions_fudgefactor

!    Smoke

!    Black Carbon

!    Sulfate

!    GOCART-style Mie Lookup Tables
     integer            :: nchannels, nmoments
     real, pointer      :: channels(:)
     character(len=255) :: du_optics_file
     character(len=255) :: ss_optics_file
     character(len=255) :: bc_optics_file
     character(len=255) :: sm_optics_file
     character(len=255) :: su_optics_file

!    Workspace for any requested point emissions
!    Sulfate
     logical :: doing_point_emissions_sulfate=.FALSE.         ! Providing pointwise emissions
     character(len=255) :: point_emissions_srcfilen_sulfate   ! filename for pointwise emissions
     integer                         :: nPts_sulfate = -1
     integer, pointer, dimension(:)  :: vstart_sulfate => null(), &
                                        vend_sulfate   => null()
     real, pointer, dimension(:)     :: vLat_sulfate   => null(), &
                                        vLon_sulfate   => null(), &
                                        vBase_sulfate  => null(), &
                                        vTop_sulfate   => null(), &
                                        vEmis_sulfate  => null()
!    Ash
     logical :: doing_point_emissions_ash=.FALSE.         ! Providing pointwise emissions
     character(len=255) :: point_emissions_srcfilen_ash   ! filename for pointwise emissions
     integer                         :: nPts_ash = -1
     integer, pointer, dimension(:)  :: vstart_ash => null(), &
                                        vend_ash   => null()
     real, pointer, dimension(:)     :: vLat_ash   => null(), &
                                        vLon_ash   => null(), &
                                        vBase_ash  => null(), &
                                        vTop_ash   => null(), &
                                        vEmis_ash  => null()
!    Dust
     logical :: doing_point_emissions_dust=.FALSE.         ! Providing pointwise emissions
     character(len=255) :: point_emissions_srcfilen_dust   ! filename for pointwise emissions
     integer                         :: nPts_dust = -1
     integer, pointer, dimension(:)  :: vstart_dust => null(), &
                                        vend_dust   => null()
     real, pointer, dimension(:)     :: vLat_dust   => null(), &
                                        vLon_dust   => null(), &
                                        vBase_dust  => null(), &
                                        vTop_dust   => null(), &
                                        vEmis_dust  => null()


  END TYPE CARMA_Registry

CONTAINS

!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CARMA_GridCompInitialize --- Initialize CARMA_GridComp
!
! !INTERFACE:
!

   SUBROUTINE CARMA_GridCompInitialize( gcCARMA, impChem, expChem, nymd, nhms, cdt, &
                                        rc )

   IMPLICIT none
   INTEGER, PARAMETER :: DBL = KIND(0.00D+00)

! !INPUT PARAMETERS:

   INTEGER, INTENT(IN) :: nymd, nhms		       ! Time from AGCM
   REAL,    INTENT(IN) :: cdt			       ! Chemistry time step (secs)

! !OUTPUT PARAMETERS:

   TYPE(CARMA_GridComp), INTENT(INOUT) :: gcCARMA    ! Grid Component
   TYPE(ESMF_State),   INTENT(INOUT)   :: impChem    ! Import State
   TYPE(ESMF_State),   INTENT(INOUT)   :: expChem    ! Export State

   INTEGER, INTENT(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Initializes the CARMA Grid Component. It primarily sets
!               the import state.
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!  30Jun2007 Nielsen   GMI Combo set up
!  18May2009 Colarco   Adapt to use for CARMA
!
!EOP
!-------------------------------------------------------------------------

   CHARACTER(LEN=*), PARAMETER :: IAm = 'CARMA_GridCompInitialize'
   CHARACTER(LEN=255) :: rcfilen = 'CARMAchem_Registry.rc'
   CHARACTER(LEN=255) :: string

   INTEGER :: ios, n
   INTEGER, ALLOCATABLE :: ier(:)
   INTEGER :: i, i1, i2, ic, im, j, j1, j2, jm, km
   INTEGER :: nbins, n1, n2
   INTEGER :: STATUS

   INTEGER :: NBIN, NGROUP, NELEM, NSOLUTE, NWAVE, NGAS
   REAL(kind=f), allocatable    :: rmrat(:), rmin(:), rhop(:), &
                                   eshape(:), ishape(:),       &
                                   radius_(:), rlow_(:), rup_(:)
   real, allocatable            :: radius(:),  rlow(:),  rup(:)
   REAL(kind=f)                 :: gwtmol
   INTEGER, allocatable :: IGROUP(:)
   character(len=255), allocatable :: groupname(:), elemname(:)
   character(len=255)              :: grpname, gasname
   type(carmagroup_type)               :: cgroup
   type(carmaelement_type)             :: celement
   type(carmastate_type), allocatable  :: cstate(:)
   logical :: do_coag = .false.       !! do coagulation?
   logical :: do_grow = .false.       !! do nucleation, growth and evaporation?
   logical :: do_implised = .false.   !! do sedimentation with substepping
   logical :: do_substep = .false.    !! do substepping
   logical :: do_thermo = .false.     !! do thermodynamics
   logical :: do_vdiff = .false.      !! do Brownin diffusion
   logical :: do_vtran  = .false.     !! do sedimentation
   logical :: is_sulfate = .false.    !! special handling for sulfate aerosol case
   real(kind=f) :: vf_const = 0._f    !! if specified and non-zero, constant fall velocity for all particles [cm/s]
   integer :: minsubsteps = 1  !! minimum number of substeps, default = 1
   integer :: maxsubsteps = 1  !! maximum number of substeps, default = 1
   integer :: maxretries = 5   !! maximum number of substep retries, default = 5
   real(kind=f) :: conmax  = 0.1_f      !! minimum relative concentration to consider, default = 1e-1
   type(CARMA_Registry), pointer :: r => null()
   integer :: igrp_mixed    = -1        !! mixed group
   integer :: mixedcorecomp = -1        !! mixed core element (sulfate)
   integer :: igrp_sulfate  = -1        !! sulfate group
   integer :: ielm_sulfate  = -1        !! sulfate pc element
   integer :: ielm_mxpc     = -1        !! mixed group pc element
   integer :: ielm_mxsulfate= -1        !! mixed group sulfate core element
   integer :: igas_h2o      = -1        !! water vapor
   integer :: igas_h2so4    = -1        !! sulfuric acid gas
   integer :: ifallrtn      =  1        !! default fall velocity routine for particles


! This is for initializing condensed H2O to zero, for now
! -------------------------------------------------------
   REAL, ALLOCATABLE :: h2ocond(:,:,:)
   REAL, ALLOCATABLE :: cellArea(:,:)

   gcCARMA%name = 'CARMA aerosol/cloud microphysics'

!  Initialize local variables
!  --------------------------
   rc = 0

   CALL init_()
   IF ( rc /= 0 ) RETURN
   ier(:)=0

   r => gcCARMA%CARMAreg

!!  Check on the parameters and if they agree with the Chem_Registry
!!  ----------------------------------------------------------------
!!  n_CARMA = NELEM*NBIN + NGAS
!   if(nbins .ne. (nbin*nelem + ngas) ) then
!    call final_(25)
!    return
!   endif

!  Establish the CARMA structure
!  -----------------------------
   allocate(gcCARMA%carma, stat=ios)
   if(ios /= 0) then
    call final_(103)
    return
   endif
   if(MAPL_AM_I_ROOT()) then
    call CARMA_Create(gcCARMA%carma, r%NBIN, r%NELEM, r%NGROUP, &
                                     r%NSOLUTE, r%NGAS, r%NWAVE, rc, &
                                     LUNOPRT=6)
   else 
    call CARMA_Create(gcCARMA%carma, r%NBIN, r%NELEM, r%NGROUP, &
                                     r%NSOLUTE, r%NGAS, r%NWAVE, rc )
   endif

   if (rc /=0) then
    call final_(rc)
    return
   endif

!  Establish the groups
!  --------------------
!  NOTE: Hard coded optionals and parameters here
   do j = 1, r%NGROUP
    is_sulfate = .false.
    if(trim(r%groupname(j)) == 'sulfate' .or. &
       trim(r%groupname(j)) == 'SULFATE') is_sulfate = .true.
    if(r%ishape(j) .eq. 1) then
     ifallrtn = I_FALLRTN_STD
    else
     ifallrtn = I_FALLRTN_STD_SHAPE
    endif
    call CARMAGROUP_Create(gcCARMA%carma, j, r%groupname(j), r%rmin(j), r%rmrat(j), &
                           r%ishape(j), r%eshape(j), .FALSE., rc, ifallrtn=ifallrtn, &
                           irhswell=r%irhswell(j), irhswcomp=r%irhswcomp(j), is_sulfate=is_sulfate)
    if(rc /=0) then
     call final_(rc)
     return
    endif
   end do

!  Establish the elements
!  ----------------------
!  NOTE: Hard coded optionals and parameters here
   do i = 1, r%NELEM
    call CARMAELEMENT_Create(gcCARMA%carma, i, r%igroup(i), &
                             r%elemname(i), r%rhop(i), r%itype(i), r%icomposition(i), rc)
    if(rc /=0) then
     call final_(rc)
     return
    endif
   end do

!  Establish the gases
!  -------------------
   do i = 1, r%NGAS
    select case (r%igcomp(i))
     case (1) 
      gwtmol = WTMOL_H2O
     case (2)
      gwtmol = WTMOL_H2SO4
     case default
      print *, 'Unknown gas IGCOMP from CARMAchem_Registry.rc for gas ',i
      call final_(-100)
      return
    end select
    call CARMAGAS_Create(gcCARMA%carma, i, r%gasname(i), &
                         gwtmol, r%igvapreq(i), r%igcomp(i), rc, &
                         ds_threshold=-0.2_f)
    if(rc /=0) then
     call final_(rc)
     return
    endif
   end do

!  NEED:
!  Hooks to CARMA_Solute

!  Setup Growth/Nucleation
!  -----------------------
!  NOTE: For now, if <do_grow> then set up growth as if for
!  sulfuric acid and sulfate particles
   if(r%do_grow) then

!   Look for sulfate aerosol group and sulfuric acid gas
    do i = 1, r%NELEM
     j = r%igroup(i)
     grpname = uppercase(trim(r%groupname(j)))
     if(grpname == 'SULFATE') then
      igrp_sulfate = j
      if(r%elemname(i) == 'pc') ielm_sulfate = i
     endif
!    Mixed group may contain sulfate element
     if(grpname == 'MIXEDP') then
      if(r%elemname(i) == 'pc')      ielm_mxpc      = i
      if(r%elemname(i) == 'sulfate') ielm_mxsulfate = i
     endif
    end do

    do i = 1, r%NGAS
     gasname = trim(r%gasname(i))
     if(gasname == 'H2SO4' .OR. gasname == 'h2so4') igas_h2so4 = i
     if(gasname == 'H2O'   .OR. gasname == 'h2o')   igas_h2o   = i
    end do

    if(igrp_sulfate < 0 .or. ielm_sulfate < 0 .or. &
       igas_h2so4 < 0 .or. igas_h2o < 0 ) then
       r%do_grow = .false.
       print *, 'Note set up correctly for growth; do_grow set false'
    endif

   endif

   if(r%do_grow) then
    call CARMA_AddGrowth(gcCARMA%carma, ielm_sulfate, igas_h2so4, rc)
    if(rc /=0) then
     call final_(rc)
     return
    endif

!    if(ielm_mxsulfate > 0) then
!!    Add growth to the pc element of that group
!     call CARMA_AddGrowth(gcCARMA%carma, ielm_mxpc, igas_h2so4, rc)
!     if(rc /=0) then
!      call final_(rc)
!      return
!     endif
!!    Add growth to the sulfate element of that group
!     call CARMA_AddGrowth(gcCARMA%carma, ielm_mxsulfate, igas_h2so4, rc)
!     if(rc /=0) then
!      call final_(rc)
!      return
!     endif
!    endif

    call CARMA_AddNucleation(gcCARMA%carma, ielm_sulfate, ielm_sulfate, &
                             I_HOMNUC, 0._f, rc, igas=igas_h2so4)
    if(rc /=0) then
     call final_(rc)
     return
    endif
   endif


!  Setup Coagulation
!  --------------------
!  NOTE that for now self coagulation is setup only for the pure sulfate
!  group.  Additionally, there is special handling for the pure sulfate group
!  to coagulation to the mixed group provided there is a sulfate core element.
   if(r%do_coag) then

!   Set special handling for sulfate to mixed particle group
    do i = 1, r%NELEM
     j = r%igroup(i)
     grpname = uppercase(trim(r%groupname(j)))
     if(grpname == 'MIXEDP') then
      igrp_mixed = j
      if(uppercase(trim(r%elemname(i))) == 'SULFATE') mixedcorecomp = r%icomposition(i)
     endif
    end do

    do i = 1, r%NELEM
     j = r%igroup(i)
     grpname = uppercase(trim(r%groupname(j)))
!    This block adds the self coagulation of the pure sulfate group and the
!    coagulation of the pure sulfate to the mixed group.
     if( grpname == 'SULFATE' ) then
      call CARMA_AddCoagulation(gcCARMA%carma, j, j, j, I_COLLEC_FUCHS, rc )
      if(igrp_mixed > 0 .AND. r%icomposition(i) == mixedcorecomp) then
       call CARMA_AddCoagulation(gcCARMA%carma, j, igrp_mixed, igrp_mixed, I_COLLEC_FUCHS, rc )
      endif
     endif
     if( grpname == 'SMOKE' ) then
      call CARMA_AddCoagulation(gcCARMA%carma, j, j, j, I_COLLEC_FUCHS, rc )
     endif
     if(rc /=0) then
      call final_(rc)
      return
     endif
    end do
   endif



!  Initialize CARMA
!  ----------------
   call CARMA_Initialize(gcCARMA%carma, rc, &
                         do_cnst_rlh=r%do_cnst_rlh, do_coag=r%do_coag, &
                         do_detrain=r%do_detrain, do_fixedinit=r%do_fixedinit, &
                         do_grow=r%do_grow, do_incloud=r%do_incloud, &
                         do_explised=r%do_explised, do_print_init=r%do_print_init, &
                         do_substep=r%do_substep, &
                         do_thermo=r%do_thermo, do_vdiff=r%do_vdiff, &
                         do_vtran=r%do_vtran, vf_const=r%vf_const, conmax=r%conmax, &
                         minsubsteps=r%minsubsteps, maxsubsteps=r%maxsubsteps, &
                         maxretries=r%maxretries, dt_threshold=1._f )

!  Get the dust emissions size fraction
!  -----------------------
!   Look for dust aerosol group / element
    do i = 1, r%NELEM
     j = r%igroup(i)
     grpname  = uppercase(trim(r%groupname(j)))
     if(grpname == 'DUST' .OR. uppercase(trim(r%elemname(i))) == 'DUST') then
      allocate(radius_(r%NBIN), rlow_(r%NBIN), rup_(r%NBIN), __STAT__)
      allocate(radius(r%NBIN),  rlow(r%NBIN),  rup(r%NBIN), __STAT__)
      call CARMAGroup_Get(gcCARMA%carma, j, rc, r=radius_, rlow=rlow_, rup=rup_)
      radius = radius_ / 100.  ! go from CARMA cm -> m
      rlow   = rlow_   / 100.
      rup    = rup_    / 100.
      call KokSizeDistribution(radius, rlow, rup, r%dmass_dust)
      deallocate(radius, rlow, rup, radius_, rlow_, rup_, __STAT__)
     endif
    enddo



!  Print information
!  -----------------
   IF( MAPL_AM_I_ROOT() ) THEN
    call dumpGroup(gcCARMA%carma, rc)
    if(rc /=0) then
     call final_(104)
     return
    endif
    call dumpElement(gcCARMA%carma, rc)
    if(rc /=0) then
     call final_(105)
     return
    endif
   END IF

!! Housekeeping
!! ------------
!   deallocate ( r, stat=ios )
!   if ( ios /= 0) then
!    call final_(200)
!    return
!   endif
!   ier(:)=0

  RETURN

CONTAINS

   SUBROUTINE init_()
   INTEGER :: ios, n
   n=128
   ios=0
   ALLOCATE ( ier(n), stat=ios )
   IF ( ios /= 0 ) rc = 100
   END SUBROUTINE init_

   SUBROUTINE final_(ierr)
   INTEGER :: ios, ierr
   DEALLOCATE ( gcCARMA%carma, ier, stat=ios )
   CALL I90_release()
   rc = ierr
   END SUBROUTINE final_
   
!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  TFQuery: Find whether a token in T or F.
!
! !INTERFACE:
!
   LOGICAL FUNCTION TFQuery(string,fn)

! !USES:

    IMPLICIT NONE

! !INPUT PARAMETERS:

    CHARACTER(LEN=*), INTENT(IN) :: string, fn

! !OUTPUT PARAMETERS:

! !DESCRIPTION: Return the value (T or F) of a particular token (string)
!               in the file fn.

! !REVISION HISTORY:
!
!  15 Aug 2007  Nielsen    First version.
!EOP
!-------------------------------------------------------------------------
    CHARACTER(LEN=8) :: tOrF

    INTEGER :: rc
    rc = 0
    tOrF = ' '
    TFQuery = .FALSE.

    CALL I90_label ( TRIM(string), rc )
    IF(rc .NE. 0) THEN
     PRINT *,'Could not find ',TRIM(string),' in ',TRIM(fn)
     CALL final_(99)
    END IF

    CALL I90_Gtoken( tOrF, rc )
    IF(TRIM(tOrF) ==    'T' .OR. TRIM(tOrF) ==    't' .OR. &
       TRIM(tOrF) == 'TRUE' .OR. TRIM(tOrF) == 'true') TFQuery = .TRUE.

   END FUNCTION TFQuery

!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  setmcor: Find area of tiles.
!
! !INTERFACE:
!
  SUBROUTINE setmcor(i1,i2,j1,j2,im,jm,lats,cellArea)

! !USES:

  IMPLICIT NONE

! !INPUT PARAMETERS:

  INTEGER, INTENT(IN) :: i1,i2,j1,j2,im,jm
  REAL, INTENT(IN) :: lats(i1:i2,j1:j2)   !radians

! !OUTPUT PARAMETERS:

  REAL, INTENT(OUT) :: cellArea(i1:i2,j1:j2)   !m^2

! !DESCRIPTION: Find the horizontal surface area (m^2) of each cell.  
!               In testing with 8-byte words, the total surface area 
!               was 4.0000508 PI R^2.

! !REVISION HISTORY:

!  15 Aug 2007  Nielsen    First version.
!EOP
!-------------------------------------------------------------------------
  REAL, PARAMETER :: ae=6.371E+06

  INTEGER :: i,j
  REAL :: scale,dlat,f,arg,err,pi

  err=1.00E-05
  pi=4.00*ATAN(1.00)
  dlat=pi/FLOAT(jm-1)
  scale = 2.00*pi*ae*ae/FLOAT(im)

  DO j=j1,j2
   DO i=i1,i2

! South pole

    IF( lats(i,j) < -0.50*pi+err ) THEN
     f=0.25
     arg=0.50*(lats(i,j)+lats(i,j+1))

! North pole

    ELSE IF( lats(i,j) > 0.50*pi-err ) THEN
     f=0.25
     arg=0.50*(lats(i,j-1)+lats(i,j))

! Interior

    ELSE
     f=1.00
     arg=lats(i,j)
    END IF

    cellArea(i,j)=scale*dlat*f*cos(arg)

   END DO
  END DO

  RETURN 
  END SUBROUTINE setmcor

   END SUBROUTINE CARMA_GridCompInitialize

!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  CARMA_GridCompRun --- The CARMA Driver 
!
! !INTERFACE:
!

   SUBROUTINE CARMA_GridCompRun ( gcCARMA, qa, impChem, expChem, nymd, nhms, &
                                  cdt, rc )

   IMPLICIT none

! !INPUT/OUTPUT PARAMETERS:

   TYPE(CARMA_GridComp), INTENT(INOUT) :: gcCARMA ! Grid Component
   TYPE(Chem_Array), pointer           :: qa(:)   ! tracer array will go here

! !INPUT PARAMETERS:

   TYPE(ESMF_State), INTENT(INOUT) :: impChem ! Import State
   INTEGER, INTENT(IN) :: nymd, nhms	      ! time
   REAL,    INTENT(IN) :: cdt		      ! chemical timestep (secs)

! !OUTPUT PARAMETERS:

   TYPE(ESMF_State), INTENT(INOUT) :: expChem   ! Export State
   INTEGER, INTENT(OUT) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 -

! !DESCRIPTION: This routine implements the CARMA driver
!
! !IMPLEMENTATION NOTES:
!
!  No pointer is reservered in the export state for deposition of water.
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!  24Jan2005 Nielsen   Implementation of Code 916 chemistry
!  30Oct2007 Nielsen   Implementation of GMI cmbined 
!                       stratosphere/troposphere chemistry
!  12Aug2009 Colarco   First crack at CARMA run method
!
!EOP
!-------------------------------------------------------------------------

   CHARACTER(LEN=*), PARAMETER :: IAm = 'CARMA_GridCompRun'
   INTEGER                     :: STATUS

!  Input fields from GEOS-5
!  ------------------------
   REAL, POINTER, DIMENSION(:,:,:) :: p, ple, rhoa, tmpu, zc, zl, q, zle, &
                                      rh, u, v, su_nuc, su_sarea, su_numd, &
                                      su_sareav, su_reff
   REAL, POINTER, DIMENSION(:,:)   :: gwettop, fraclake, oro, u10m, v10m, &
                                      ustar, pblh, z0h, shflux, precc, precl
   real, pointer, dimension(:,:)   :: du_sed, su_sed, ss_sed, bc_sed, ash_sed, sm_sed

!  Local
!  -----
   INTEGER :: i, i1, i2, ic, ier(512), im, ijl, ios
   INTEGER :: j, j1, j2, jm
   INTEGER :: ielem, ibin, igrp, igas
   INTEGER :: k, km, kReverse
   INTEGER :: n, n2, nbegin, nend, nCARMABegin, nCARMAEnd
   INTEGER :: nymd1, nhms1
   logical :: rootproc
   real(kind=f) :: dtime

   INTEGER, PARAMETER :: ToCARMA = 1
   INTEGER, PARAMETER :: FromCARMA = -1
!  We are using the CARMA constants here (CGS units) but need
!  MKS values to go back to GEOS-5
   REAL, PARAMETER    :: grav_mks = grav/100.
   integer :: igroup
   CHARACTER(LEN=255) :: groupname

   REAL :: qmax,qmin
   real(kind=f) :: lon, lat
   real(kind=f), allocatable :: xc(:), dx(:), yc(:), dy(:)
   real(kind=f), allocatable :: p_(:), ple_(:), tmpu_(:), zc_(:), zl_(:), &
                                q_(:), rh_(:), nuc_(:), sarea_(:), numd_(:), &
                                r_wet_(:), reff_num(:), reff_den(:)
   real(kind=f), allocatable :: satice_(:,:), satliq_(:,:), told_(:), gasold_(:,:)
   real(kind=f) :: dq_

   type(carmastate_type) :: cstate
   type(carma_type), pointer :: r => null()
   type(CARMA_Registry), pointer :: reg => null()
   character(len=255) :: string

!  For a reference atmosphere we'll choose some values
   real, parameter, dimension(73) :: pleRef = & 
         (/ 1, 2, 3, 4, 6, 8, 11, 15, 21, 27, 36, 47, 61, 79, 101, 130,       &
            165, 208, 262, 327, 407, 504, 621, 761, 929, 1127, 1364, 1645,  &
            1979, 2373, 2836, 3381, 4017, 4764, 5638, 6660, 7851, 9236,     &
            10866, 12783, 15039, 17693, 20792, 24398, 28606, 33388, 37003,  &
            40612, 44214, 47816, 51405, 54997, 58584, 62170, 65769, 68147,  &
            70540, 72931, 75313, 77711, 79623, 81046, 82485, 83906, 85344,  &
            86765, 88201, 89636, 91071, 92516, 93921, 95376, 100000 /) 
   real, parameter, dimension(72) :: tmpuRef = & 
         (/ 219, 221, 223, 228, 230, 230, 232, 238, 245, 253, 259, 263, &
            264, 262, 258, 253, 247, 239, 233, 229, 227, 227, 226, 223, &
            222, 221, 220, 219, 218, 217, 216, 215, 214, 213, 212, 212, &
            214, 214, 216, 219, 219, 210, 210, 218, 227, 234, 240, 245, &
            250, 254, 257, 260, 262, 263, 265, 266, 267, 268, 269, 270, &
            270, 270, 270, 270, 271, 271, 271, 270, 267, 265, 266, 266 /)
   real, parameter, dimension(72) :: rhRef = 1e-6 * &
         (/ 1, 2, 2, 2, 3, 4, 4, 3, 4, 4, 4, 4, 4, 4, 4, 6, 18, 51,          &
            129, 267, 394, 502, 682, 1135, 1603, 2076, 2820, 3792, 5120,     &
            6806, 8912, 11597, 15397, 20386, 28168, 29755, 28748, 33875,     &
            34058, 28657, 43458, 401856, 947266, 932618, 902344, 657227,     &
            371583, 203370, 235108, 317872, 413086, 511719, 691407, 686524,  &
            601563, 456055, 475098, 626954, 590821, 483399, 380860, 297852,  &
            230958, 183594, 144288, 111084, 96558, 136963, 369629, 770508,   &
            793946, 799805 /) 

   ier(:) = 0

!  Short-hand to object
   r   => gcCARMA%carma
   reg => gcCARMA%CARMAreg

!  Grid specs from Chem_Bundle%grid
!  --------------------------------
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

!  Location of species from Chem_Bundle%registry.
!  ----------------------------------------------
   nCARMABegin =  1
   nCARMAEnd   =  gcCARMA%CARMAreg%nq

   rootProc=.FALSE.
   IF( MAPL_AM_I_ROOT() ) THEN
    rootProc=.TRUE.
   END IF

!  Allocate
!  --------
   allocate(p(i1:i2,j1:j2,km), __STAT__ )
   allocate(xc(km), dx(km), yc(km), dy(km), &
            p_(km), ple_(km+1), tmpu_(km), zc_(km), zl_(km+1), &
            q_(km), rh_(km), nuc_(km), sarea_(km), numd_(km), &
            r_wet_(km), reff_num(km), reff_den(km), &
            __STAT__ )
   allocate(told_(km), gasold_(km,reg%NGAS), satice_(km,reg%NGAS), satliq_(km,reg%NGAS), __STAT__ )

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
   call MAPL_GetPointer(expChem, du_sed,    'CARMA_DUSD',   __RC__)
   call MAPL_GetPointer(expChem, su_sed,    'CARMA_SUSD',   __RC__)
   call MAPL_GetPointer(expChem, bc_sed,    'CARMA_BCSD',   __RC__)
   call MAPL_GetPointer(expChem, ss_sed,    'CARMA_SSSD',   __RC__)
   call MAPL_GetPointer(expChem, ash_sed,   'CARMA_ASHSD',  __RC__)
   call MAPL_GetPointer(expChem, sm_sed,    'CARMA_SMSD',   __RC__)
   call MAPL_GetPointer(expChem, su_nuc,    'CARMA_SUNUC',  __RC__)
   call MAPL_GetPointer(expChem, su_sarea,  'CARMA_SUSAREA',  __RC__)
   call MAPL_GetPointer(expChem, su_sareav, 'CARMA_SUSAREAv',  __RC__)
   call MAPL_GetPointer(expChem, su_numd,   'CARMA_SUNUMD',   __RC__)
   call MAPL_GetPointer(expChem, su_reff,   'CARMA_SUREFF',   __RC__)


!  Get the mid-point pressure
!  --------------------------
   DO k=1,km
    p(i1:i2,j1:j2,k)=exp((log(ple(i1:i2,j1:j2,k-1))+log(ple(i1:i2,j1:j2,k)) )*0.50)
   END DO

!   call pmaxmin('CARMA::U:       ', u(i1:i2,j1:j2,1:km), qmin, qmax, ijl, km, 1. )
!   call pmaxmin('CARMA::V:       ', v(i1:i2,j1:j2,1:km), qmin, qmax, ijl, km, 1. )
!   call pmaxmin('CARMA::AIRDENS: ', rhoa(i1:i2,j1:j2,1:km), qmin, qmax, ijl, km, 1. )
!   call pmaxmin('CARMA::Q:       ', q(i1:i2,j1:j2,1:km), qmin, qmax, ijl, km, 1. )
!   call pmaxmin('CARMA::RH:      ', rh(i1:i2,j1:j2,1:km), qmin, qmax, ijl, km, 1. )
!   call pmaxmin('CARMA::P:       ', p(i1:i2,j1:j2,1:km), qmin, qmax, ijl, km, 1. )
!   call pmaxmin('CARMA::ZLE:     ', zle(i1:i2,j1:j2,1:km), qmin, qmax, ijl, km, 1. )
!   call pmaxmin('CARMA::T:       ', tmpu(i1:i2,j1:j2,1:km), qmin, qmax, ijl, km, 1. )
!   call pmaxmin('CARMA::ORO:     ', oro(i1:i2,j1:j2), qmin, qmax, ijl, 1, 1. )
!   call pmaxmin('CARMA::USTAR:   ', ustar(i1:i2,j1:j2), qmin, qmax, ijl, 1, 1. )
!   call pmaxmin('CARMA::FRACLAKE:', fraclake(i1:i2,j1:j2), qmin, qmax, ijl, 1, 1. )
!   call pmaxmin('CARMA::U10M:    ', u10m(i1:i2,j1:j2), qmin, qmax, ijl, 1, 1. )
!   call pmaxmin('CARMA::V10M:    ', v10m(i1:i2,j1:j2), qmin, qmax, ijl, 1, 1. )
!   call pmaxmin('CARMA::PBLH:    ', pblh(i1:i2,j1:j2), qmin, qmax, ijl, 1, 1. )
!   call pmaxmin('CARMA::Z0H:     ', z0h(i1:i2,j1:j2), qmin, qmax, ijl, 1, 1. )
!   call pmaxmin('CARMA::SHFLUX:  ', shflux(i1:i2,j1:j2), qmin, qmax, ijl, 1, 1. )
!   call pmaxmin('CARMA::PRECL:   ', precl(i1:i2,j1:j2), qmin, qmax, ijl, 1, 1. )
!   call pmaxmin('CARMA::PRECC:   ', precc(i1:i2,j1:j2), qmin, qmax, ijl, 1, 1. )
!   call pmaxmin('CARMA::GWETTOP: ', gwettop(i1:i2,j1:j2), qmin, qmax, ijl, 1, 1. )

   IF( ANY(ier(:) /= 0) ) THEN
    PRINT *,Iam,': Failed on MAPL_GetPointer for imports in CARMA_GridCompRun.'
    rc = 11 
    RETURN
   END IF
   ier(:)=0

! For substepping you want to remember the old temperature.
! This is set in the internal_spec but possibly is bootstrapped.
! If bootstrapped set to current temperature.
  n = nCARMAbegin + reg%NBIN*reg%NELEM + reg%NGAS
  if(qa(n)%data3d(i1,j2,km) < 0.) qa(n)%data3d = tmpu
! And same for gases -- first, initialize water vapor to current
  do igas = 1, reg%NGAS
   n  = nCARMAbegin + reg%NBIN*reg%NELEM - 1 + igas
   if(trim(reg%gasname(igas)) == 'h2o' .or. trim(reg%gasname(igas)) == 'H2O') qa(n)%data3d = q
   n2 = nCARMAbegin + reg%NBIN*reg%NELEM + reg%NGAS + igas
   if(qa(n2)%data3d(i1,j2,km) < 0.) qa(n2)%data3d = qa(n)%data3d
  enddo

#ifdef DEBUG
if(reg%NGAS > 0) then
   n = reg%NBIN*reg%NELEM + 1
   call pmaxmin('CARMA::h2o_0:           ', qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, ijl, km, 1. )
   n = reg%NBIN*reg%NELEM + reg%NGAS
   call pmaxmin('CARMA::h2so4_0:         ', qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, ijl, km, 1. )
   call pmaxmin('CARMA::su001_0:         ', qa(1)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, ijl, km, 1. )
   n = reg%NBIN*reg%NELEM + reg%NGAS + 1
   call pmaxmin('CARMA::told_0:          ', qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, ijl, km, 1. )
   n = reg%NBIN*reg%NELEM + reg%NGAS + reg%NGAS
   call pmaxmin('CARMA::h2o_old_0:       ', qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, ijl, km, 1. )
   n = reg%NBIN*reg%NELEM + reg%NGAS + reg%NGAS + 1
   call pmaxmin('CARMA::h2so4_old_0:     ', qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, ijl, km, 1. )
   n = reg%NBIN*reg%NELEM + reg%NGAS + reg%NGAS + reg%NGAS + 1
   call pmaxmin('CARMA::satliq2_old_0:   ', qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, ijl, km, 1. )
   n = reg%NBIN*reg%NELEM + reg%NGAS + reg%NGAS + reg%NGAS + reg%NGAS + 1
   call pmaxmin('CARMA::satice2_old_0:   ', qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, ijl, km, 1. )
endif
#endif

!  ====================  CARMA Step ================================
!  Establish the CARMA state, do a step, and retain diagnostic
   if( associated(DU_sed)) DU_sed(:,:) = 0.
   if( associated(SU_sed)) SU_sed(:,:) = 0.
   if( associated(SS_sed)) SS_sed(:,:) = 0.
   if( associated(BC_sed)) BC_sed(:,:) = 0.
   if( associated(SM_sed)) SM_sed(:,:) = 0.
   if( associated(ASH_sed)) ASH_sed(:,:) = 0.
   if( associated(SU_nuc)) SU_nuc(:,:,:) = 0.
   if( associated(SU_sarea))  SU_sarea(:,:,:) = 0.
   if( associated(SU_sareav)) SU_sareav(:,:,:) = 0.
   if( associated(SU_numd))   SU_numd(:,:,:) = 0.
   if( associated(SU_reff))   SU_reff(:,:,:) = 0.

!  Possibly create a CARMA reference state column for 1,1 column in tile
   if(reg%do_fixedinit) then
!    dx and dy are hack as if for a "b" resolution grid
     dx(:) = 2.5
     dy(:) = 2.
     xc(:) = 0.
     yc(:) = 0.
     p_(1:km) = exp((log(pleRef(1:km))+log(pleRef(2:km+1)) )*0.50)
     ple_(1:km+1) = pleRef
     zc_(:) = p_(:)/pleRef(km+1)
     zl_(1:km+1) = pleRef(1:km+1)/pleRef(km+1)
     tmpu_(:) = tmpuRef(:)
     rh_(:) = rhRef(:)
     call CARMASTATE_CreateFromReference(cstate, r, 1._f, dtime, km, &
                              I_HYBRID, I_CART, lat, lon,  &
                              xc, dx, yc, dy, &
                              zc_, zl_, p_, ple_, tmpu_, rc, &
                              relhum=rh_)
   endif

   do j = j1, j2
    do i = i1, i2
     
     lon = gcCARMA%lons(i,j)
     lat = gcCARMA%lats(i,j)
!    dx and dy are hack as if for a "b" resolution grid
     dx(:) = 2.5
     dy(:) = 2.
     xc(:) = lon
     yc(:) = lat
     p_(:) = p(i,j,:)
     ple_(1:km+1) = ple(i,j,0:km)
     zc_(:) = p(i,j,:)/ple(i,j,km)
     zl_(1:km+1) = ple(i,j,0:km)/ple(i,j,km)
     tmpu_(:) = tmpu(i,j,:)
     rh_(:) = rh(i,j,:)
!    prior time step values for sub-stepping
     n = reg%NBIN*reg%NELEM + reg%NGAS + 1
     told_(:) = qa(n)%data3d(i,j,:)
     do igas = 1, reg%NGAS
      gasold_(:,igas) = qa(n+igas)%data3d(i,j,:)
      satliq_(:,igas) = qa(n+reg%NGAS+igas)%data3d(i,j,:)
      satice_(:,igas) = qa(n+reg%NGAS+reg%NGAS+igas)%data3d(i,j,:)
     enddo

     call CARMASTATE_Create(cstate, r, 1._f, dtime, km, &
                            I_HYBRID, I_CART, lat, lon,  &
                            xc, dx, yc, dy, &
                            zc_, zl_, p_, ple_, tmpu_, rc, &
                            relhum=rh_, told=told_)

   ! Map the model MMR to CARMA
     do ielem = 1, reg%NELEM
      do ibin = 1, reg%NBIN
       n = nCARMAbegin + (ielem-1)*reg%NBIN + ibin - 1
       q_(:) = qa(n)%data3d(i,j,:)
       where(q_ < 1.e-32) q_ = 1.e-32
       call CARMASTATE_SetBin(cstate, ielem, ibin, q_, rc)
      end do
     end do

   ! Map the model gases to CARMA
     if(reg%NGAS > 0) then
     do igas = 1, reg%NGAS
      n = nCARMAbegin + reg%NELEM*reg%NBIN - 1 + igas
      q_(:) = qa(n)%data3d(i,j,:)
      where(q_ < 1.e-32) q_ = 1.e-32
!!     HACK: Want to put in 10 Tg S of H2SO4 per year total into band 30N - 30S
!!     all longitude between 20 - 25 km altitude.  So this is 2.e10 kg H2SO4
!!     per year over an area of 2.626e14 m2 over 32.65 hPa depth for levels 30 - 34.
!      if( (trim(reg%gasname(igas)) == 'h2so4' .or. trim(reg%gasname(igas)) == 'H2SO4') .and. &
!         lat >= -30. .and. lat <= 30.) then
!       do k = 30,34
!        q_(k) = q_(k) + 2.e10 / (365.*86400.)*cdt  / 2.626e14 / 3265. * 9.8
!       enddo
!      endif

      call CARMASTATE_SetGas(cstate, igas, q_(:), rc, &
                             mmr_old = gasold_(:,igas), satice_old=satice_(:,igas), &
                             satliq_old=satliq_(:,igas) )
     end do
     endif
			
   ! Execute the step
     call CARMASTATE_Step(cstate, ios)

   ! Map CARMA back to model MMR
     do ielem = 1, reg%NELEM
      do ibin = 1, reg%NBIN
       n = nCARMAbegin + (ielem-1)*reg%NBIN + ibin - 1
       call CARMASTATE_GetBin(cstate, ielem, ibin, &
                              q_, rc)
       where(q_ < 1.e-32) q_ = 1.e-32
       qa(n)%data3d(i,j,:) = q_(:)
      end do
     end do

   ! Map CARMA back to model gas
     if(reg%NGAS > 0) then
     do igas = 1, reg%NGAS
      n = nCARMAbegin + reg%NELEM*reg%NBIN - 1 + igas
      call CARMASTATE_GetGas(cstate, igas, q_, rc, &
                             satice=satice_(:,igas), satliq=satliq_(:,igas))
      where(q_ < 1.e-32) q_ = 1.e-32
      qa(n)%data3d(i,j,:) = q_(:)
!     Save current gas mixing ratio and saturations for "old" values of next step
      n = reg%NBIN*reg%NELEM + reg%NGAS + 1
      qa(n+igas)%data3d(i,j,:) = q_(:)
      qa(n+reg%NGAS+igas)%data3d(i,j,:) = satliq_(:,igas)
      qa(n+reg%NGAS+reg%NGAS+igas)%data3d(i,j,:) = satice_(:,igas)
     end do

!    Hack - for now we assume gas does not change temperature, save told
     n = nCARMAbegin + reg%NELEM*reg%NBIN + reg%NGAS
     qa(n)%data3d(i,j,:) = tmpu_
     endif

   ! If the sedimentation flux diagnostic is asked for, get it
     do ielem = 1, reg%NELEM
      igroup = gcCARMA%CARMAreg%igroup(ielem)
      groupname = trim(gcCARMA%CARMAreg%groupname(igroup))
      if(ielem /= r%f_group(igroup)%f_ienconc ) cycle
      do ibin = 1, reg%NBIN
       n = nCARMAbegin + (ielem-1)*reg%NBIN + ibin - 1
       call CARMASTATE_GetBin(cstate, ielem, ibin, &
                              q_, rc, sedimentationflux=dq_)
       if( (groupname == 'dust' .OR. groupname == 'DUST') .AND. &
           (associated(DU_sed)) ) then
        DU_sed(i,j) = DU_sed(i,j) + dq_
       endif
       if( (groupname == 'sulfate' .OR. groupname == 'SULFATE') .AND. &
           (associated(SU_sed)) ) then
        SU_sed(i,j) = SU_sed(i,j) + dq_
       endif
       if( (groupname == 'seasalt' .OR. groupname == 'SEASALT') .AND. &
           (associated(SS_sed)) ) then
        SS_sed(i,j) = SS_sed(i,j) + dq_
       endif
       if( (groupname == 'blackcarbon' .OR. groupname == 'BLACKCARBON') .AND. &
           (associated(BC_sed)) ) then
        BC_sed(i,j) = BC_sed(i,j) + dq_
       endif
       if( (groupname == 'smoke' .OR. groupname == 'SMOKE') .AND. &
           (associated(SM_sed)) ) then
        SM_sed(i,j) = SM_sed(i,j) + dq_
       endif
       if( (groupname == 'ash' .OR. groupname == 'ASH') .AND. &
           (associated(ASH_sed)) ) then
        ASH_sed(i,j) = ASH_sed(i,j) + dq_
       endif
      end do
     end do

!    Get the nucleation rate if it is asked for (note conversion cm-3 s-1 to m-3 s-1)
     do ielem = 1, reg%NELEM
      igroup = gcCARMA%CARMAreg%igroup(ielem)
      groupname = trim(gcCARMA%CARMAreg%groupname(igroup))
      if(groupname /= 'sulfate' .AND. groupname /= 'SULFATE') cycle
      if(ielem /= r%f_group(igroup)%f_ienconc ) cycle
      if(.not.associated(SU_nuc)) cycle
      do ibin = 1, reg%NBIN
       call CARMASTATE_GetBin(cstate, ielem, ibin, &
                              q_, rc, nucleationrate=nuc_)
       SU_nuc(i,j,:) = SU_nuc(i,j,:) + nuc_ * 1.e6
      enddo
     enddo

!    Get the group effective radius (note conversion cm to m)
     do ielem = 1, reg%NELEM
      igroup = gcCARMA%CARMAreg%igroup(ielem)
      groupname = trim(gcCARMA%CARMAreg%groupname(igroup))
      if(groupname /= 'sulfate' .AND. groupname /= 'SULFATE') cycle
      if(ielem /= r%f_group(igroup)%f_ienconc ) cycle
      if(.not.associated(SU_reff)) cycle
      reff_num = 0.
      reff_den = 0.
      do ibin = 1, reg%NBIN
       call CARMASTATE_GetBin(cstate, ielem, ibin, q_, rc, &
        r_wet=r_wet_, numberdensity=numd_)
       reff_num = reff_num + r_wet_**3.*numd_
       reff_den = reff_den + r_wet_**2.*numd_
      enddo
      where(reff_den > 0) SU_reff(i,j,:) = reff_num / reff_den * 1.e-2 ! m
     enddo

!    Get the surface area density and/or number density
!    if asked for (note conversion from CGS to MKS)
     do ielem = 1, reg%NELEM
      igroup = gcCARMA%CARMAreg%igroup(ielem)
      groupname = trim(gcCARMA%CARMAreg%groupname(igroup))
      if(groupname /= 'sulfate' .AND. groupname /= 'SULFATE') cycle
      if(ielem /= r%f_group(igroup)%f_ienconc ) cycle
      if(.not.associated(SU_sarea) .and. .not.associated(SU_numd)) cycle
      do ibin = 1, reg%NBIN
       call CARMASTATE_GetBin(cstate, ielem, ibin, &
                              q_, rc, areadensity=sarea_, numberdensity=numd_)
       if(associated(SU_sarea)) SU_sarea(i,j,:) = SU_sarea(i,j,:) + sarea_ * 100.
       if(associated(SU_numd))  SU_numd(i,j,:)  = SU_numd(i,j,:) + numd_ * 1.e6
      enddo
     enddo

!  Hack -- for now don't change temperature
!   ! Get the updated temperature.
!     call CARMASTATE_GetState(cstate, rc, t=tmpu_)
!     tmpu(i,j,:) = tmpu_(:)

    end do
   end do

 ! Cleanup the carma state objects
   call CARMASTATE_Destroy(cstate, rc)


#ifdef DEBUG
if(reg%NGAS > 0) then
   n = reg%NBIN*reg%NELEM + reg%NGAS
   call pmaxmin('CARMA::h2o_1:           ', qa(n-1)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, ijl, km, 1. )
   call pmaxmin('CARMA::h2so4_1:         ', qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, ijl, km, 1. )
   call pmaxmin('CARMA::su001_1:         ', qa(1)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, ijl, km, 1. )
   n = reg%NBIN*reg%NELEM + reg%NGAS + 1
   call pmaxmin('CARMA::told_1:          ', qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, ijl, km, 1. )
   n = reg%NBIN*reg%NELEM + reg%NGAS + reg%NGAS
   call pmaxmin('CARMA::h2o_old_1:       ', qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, ijl, km, 1. )
   n = reg%NBIN*reg%NELEM + reg%NGAS + reg%NGAS + 1
   call pmaxmin('CARMA::h2so4_old_1:     ', qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, ijl, km, 1. )
   n = reg%NBIN*reg%NELEM + reg%NGAS + reg%NGAS + reg%NGAS + 1
   call pmaxmin('CARMA::satliq2_old_1:   ', qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, ijl, km, 1. )
   n = reg%NBIN*reg%NELEM + reg%NGAS + reg%NGAS + reg%NGAS + reg%NGAS + 1
   call pmaxmin('CARMA::satice2_old_1:   ', qa(n)%data3d(i1:i2,j1:j2,1:km), qmin, qmax, ijl, km, 1. )
endif
#endif
!  =================  END CARMA Step ================================

!  Deallocate
!  --------
   deallocate(p, xc, dx, yc, dy, gasold_, told_, satice_, satliq_, &
              p_, ple_, tmpu_, zc_, zl_, q_, rh_, nuc_, sarea_, numd_, &
              r_wet_, reff_num, reff_den, stat=ios)


! ------------------------------------------------------------------------
! Export states
! ------------------------------------------------------------------------



   RETURN

 END SUBROUTINE CARMA_GridCompRun

!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CARMA_GridCompFinalize
!
! !INTERFACE:
!

   SUBROUTINE CARMA_GridCompFinalize ( gcCARMA, impChem, expChem, &
                                       nymd, nhms, cdt, rc )

  IMPLICIT none

! !INPUT/OUTPUT PARAMETERS:

   TYPE(CARMA_GridComp), INTENT(inout) :: gcCARMA ! Grid Component

! !INPUT PARAMETERS:

   INTEGER, INTENT(in) :: nymd, nhms	      ! time
   REAL,    INTENT(in) :: cdt  	              ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   TYPE(ESMF_State), INTENT(inout) :: impChem	! Import State
   TYPE(ESMF_State), INTENT(inout) :: expChem	! Import State
   INTEGER, INTENT(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 -
 
! !DESCRIPTION: This routine finalizes this Grid Component.
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!
!EOP
!-------------------------------------------------------------------------

   CHARACTER(LEN=*), PARAMETER   :: IAm = 'CARMA_GridCompFinalize'
   integer :: STATUS

   rc=0

   deallocate ( gcCARMA%carma )

   RETURN

 END SUBROUTINE CARMA_GridCompFinalize
 

 
  subroutine dumpElement(carma, rc)

  type(carma_type), intent(in)     :: carma              !! the carma object
  integer, intent(inout)           :: rc                 !! return code, negative indicates failure
  
  ! Local Variables
  integer                          :: i
	
  write(*,*)  ""
  write(*,*)  "Element Information"
  
  do i = 1, carma%f_NELEM
    call CARMAELEMENT_Print(carma, i, rc)
    if (rc /=0) write(carma%f_LUNOPRT, *) "    *** FAILED ***, rc=", rc
    write(carma%f_LUNOPRT,*) ""  
  end do
 
  write(carma%f_LUNOPRT,*) ""
  return
  end subroutine dumpElement




  subroutine dumpGas(carma, rc)

  type(carma_type), pointer, intent(inout)  :: carma              !! the carma object
  integer, intent(inout)                    :: rc                 !! return code, negative indicates failure
  
  ! Local Variables
  integer                       :: i
  type(carmagas_type), pointer  :: cgas
  character(len=255)            :: gasname
  real(kind=f)                  :: gwtmol
	
  write(*,*)  ""
  write(*,*)  "Gas Information"
  
  do i = 1, carma%f_NGAS
!   call CARMA_GetGas(carma, i, cgas, rc)
   if (rc /=0) write(*, *) "    *** FAILED ***, rc=", rc
   
!   call CARMAGAS_Print(cgas, carma, rc)
   if (rc /=0) write(*, *) "    *** FAILED ***, rc=", rc
   
   write(*,*) ""  
  end do
 
  write(*,*) ""  
  end subroutine dumpGas




  subroutine dumpGroup(carma, rc)

  type(carma_type), intent(in)     :: carma              !! the carma object
  integer, intent(inout)           :: rc                 !! return code, negative indicates failure
  
  ! Local Variables
  integer                          :: i
	
  write(*,*)  ""
  write(*,*)  "Group Information"
  
  do i = 1, carma%f_NGROUP
    call CARMAGROUP_Print(carma, i, rc)
    if (rc /=0) write(carma%f_LUNOPRT, *) "    *** FAILED ***, rc=", rc
    
    write(carma%f_LUNOPRT,*) ""  
  end do
 
  write(carma%f_LUNOPRT,*) ""
  return
  end subroutine dumpGroup

 END MODULE CARMA_GridCompMod

