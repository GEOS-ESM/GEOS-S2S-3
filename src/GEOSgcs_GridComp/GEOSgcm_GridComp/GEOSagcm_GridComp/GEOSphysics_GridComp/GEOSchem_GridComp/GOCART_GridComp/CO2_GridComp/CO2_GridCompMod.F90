#include "MAPL_Generic.h"
!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 610.1, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  CO2_GridCompMod --- CO2 Grid Component Class
!
! !INTERFACE:
!

   module  CO2_GridCompMod

! !USES:

   USE ESMF
   USE MAPL_Mod

   use Chem_Mod              ! Chemistry Base Class
   use Chem_StateMod         ! Chemistry State
   use Chem_UtilMod
   use m_inpak90             ! Resource file management

   implicit none

! !PUBLIC TYPES:
!
   PRIVATE
   PUBLIC  CO2_GridComp       ! The CO2 object 

!
! !PUBLIIC MEMBER FUNCTIONS:
!

   PUBLIC  CO2_GridCompSetServices
   PUBLIC  CO2_GridCompInitialize
   PUBLIC  CO2_GridCompRun
   PUBLIC  CO2_GridCompFinalize

!
! !DESCRIPTION:
!
!  This module implements the (pre-ESMF) CO2 Grid Component. 
!
! !REVISION HISTORY:
!
!  16Sep2003 da Silva  First crack.
!  24Oct2005     Bian  tag CO2 to 4 regions 
!                      (total, north america, south america, africa)
!  19Dec2005 da Silva  Activated 3D diags for output
!  26Nov2010  Nielsen  Simplified PBL partitioning for biomass burning emissions   
!                      
!EOP
!-------------------------------------------------------------------------

  type CO2_GridComp
        character(len=255) :: name

        CHARACTER(LEN=255) :: eFilen_biomass     ! Biomass emissions (for use with climatology)
        CHARACTER(LEN=255) :: eFilen_ff          ! Fossil fuel emissions
        CHARACTER(LEN=255) :: eFilen_ocn         ! Ocean flux inputs
        CHARACTER(LEN=255) :: eFilen_nep         ! Biosphere fluxes

        CHARACTER(LEN=255) :: maskFileName

        INTEGER            :: nymd_ff  = 0
        INTEGER            :: nymd_ocn = 0
        INTEGER            :: nymd_nep = 0


        INTEGER            :: BCnymd             ! Date of last emissions/production read
        INTEGER            :: dt_read            ! Time since last read
        REAL               :: BBconFac           ! conversion factor of BB emissions to CO2

        REAL               :: FFconFac           ! conversion factor of FF emissions to C

        REAL               :: BioDrawDownFactor  ! Biosphere drawdown factor

        REAL, POINTER      ::    eCO2_FF(:,:)    ! kgC/m2/s, Earth surface
        REAL, POINTER      ::   eCO2_NEP(:,:)    ! kgC/m2/s, Earth surface
        REAL, POINTER      ::   eCO2_OCN(:,:)    ! kgC/m2/s, Earth surface
        REAL, POINTER      ::   eCO2_BB_(:,:)    ! kgC/m2/s, PBL (before diurnal)
        REAL, POINTER      ::    eCO2_BB(:,:)    ! kgC/m2/s, PBL
        REAL, POINTER      :: regionMask(:,:)    ! regional mask
        REAL, POINTER      ::        SST(:,:)    ! SST, C, for CMS ocean flux calc
        REAL, POINTER      ::        SSS(:,:)    ! Salinty, PSU, for CMS ocean flux calc
        REAL, POINTER      ::       pco2(:,:)    ! Ocean pCO2, for CMS ocean flux calc
        REAL, POINTER      ::       pice(:,:)    ! % grid covered by ice, for CMS ocean flux calc
        INTEGER, POINTER   ::  regionIndex(:)    ! desired regions from mask

        LOGICAL            :: DBG                ! Run-time debug switch
        LOGICAL            :: CMS_EMIS           ! Run-time switch to use CMS emissions
        LOGICAL            :: OCN_FLUX_CALC      ! Method of ocean flux calculation from input file name
  end type CO2_GridComp

  real, parameter :: radToDeg = 57.2957795

CONTAINS

   subroutine CO2_GridCompSetServices(  gc, chemReg, rc)
   type(ESMF_GridComp), intent(INOUT) :: GC
   type(Chem_Registry), intent(INOUT) :: chemReg
   integer,             intent(OUT  ) :: rc

   CHARACTER(LEN=255) :: rcbasen = 'CO2_GridComp'
   CHARACTER(LEN=255) :: name

   integer            :: status
   character(len=ESMF_MAXSTR) :: Iam

   integer :: ier,doingCMS,ocnFlux

   type(ESMF_Config) :: cfg

   Iam = "CO2_GridCompSetServices"

!  Load resource file
!  ------------------
   cfg = ESMF_ConfigCreate(rc=status)
   VERIFY_(STATUS)
   call ESMF_ConfigLoadFile(cfg,TRIM(rcbasen)//'.rc',rc=status)
   VERIFY_(STATUS)

   call ESMF_ConfigGetAttribute(cfg, value=doingCMS, label='CMS_EMIS:', rc=status)
   VERIFY_(STATUS)
   call ESMF_ConfigGetAttribute(cfg, value=ocnFlux, label='OCN_FLUX_CALC:', default= 0, rc=status)
   VERIFY_(STATUS)

   call MAPL_AddImportSpec(GC,           &
        SHORT_NAME = 'CO2_regionMask',   &
        LONG_NAME  = 'source species'  , &
        UNITS      = '1',                &
        DIMS       = MAPL_DimsHorzOnly,  &
        VLOCATION  = MAPL_VLocationNone, &
        RESTART    = MAPL_RestartSkip,   &
        RC         = STATUS)
   VERIFY_(STATUS)
   if (doingCMS == 0) then 
      call MAPL_AddImportSpec(GC, &
           SHORT_NAME = 'CO2_BIOMASS', &
           LONG_NAME  = 'source species'  , &
           UNITS      = '1', &
           DIMS       = MAPL_DimsHorzOnly, &
           VLOCATION  = MAPL_VLocationNone, &
           RESTART    = MAPL_RestartSkip,   &
           RC         = STATUS)
      VERIFY_(STATUS)
      call MAPL_AddImportSpec(GC, &
           SHORT_NAME = 'CO2_FF', &
           LONG_NAME  = 'source species'  , &
           UNITS      = '1', &
           DIMS       = MAPL_DimsHorzOnly, &
           VLOCATION  = MAPL_VLocationNone, &
           RESTART    = MAPL_RestartSkip,   &
           RC         = STATUS)
      VERIFY_(STATUS)
      call MAPL_AddImportSpec(GC, &
           SHORT_NAME = 'CO2_NEP', &
           LONG_NAME  = 'source species'  , &
           UNITS      = '1', &
           DIMS       = MAPL_DimsHorzOnly, &
           VLOCATION  = MAPL_VLocationNone, &
           RESTART    = MAPL_RestartSkip,   &
           RC         = STATUS)
      VERIFY_(STATUS)
      call MAPL_AddImportSpec(GC, &
           SHORT_NAME = 'CO2_OCN', &
           LONG_NAME  = 'source species'  , &
           UNITS      = '1', &
           DIMS       = MAPL_DimsHorzOnly, &
           VLOCATION  = MAPL_VLocationNone, &
           RESTART    = MAPL_RestartSkip,   &
           RC         = STATUS)
      VERIFY_(STATUS)
   else
      call MAPL_AddImportSpec(GC, &
           SHORT_NAME = 'CO2_CMS_BIOMASS', &
           LONG_NAME  = 'source species'  , &
           UNITS      = '1', &
           DIMS       = MAPL_DimsHorzOnly, &
           VLOCATION  = MAPL_VLocationNone, &
           RESTART    = MAPL_RestartSkip,   &
           RC         = STATUS)
      VERIFY_(STATUS)
      call MAPL_AddImportSpec(GC, &
           SHORT_NAME = 'CO2_CMS_FF', &
           LONG_NAME  = 'source species'  , &
           UNITS      = '1', &
           DIMS       = MAPL_DimsHorzOnly, &
           VLOCATION  = MAPL_VLocationNone, &
           RESTART    = MAPL_RestartSkip,   &
           RC         = STATUS)
      VERIFY_(STATUS)
      call MAPL_AddImportSpec(GC, &
           SHORT_NAME = 'CO2_CMS_NEP', &
           LONG_NAME  = 'source species'  , &
           UNITS      = '1', &
           DIMS       = MAPL_DimsHorzOnly, &
           VLOCATION  = MAPL_VLocationNone, &
           RESTART    = MAPL_RestartSkip,   &
           RC         = STATUS)
      VERIFY_(STATUS)
      if (ocnFlux /=0 ) then
         call MAPL_AddImportSpec(GC, &
              SHORT_NAME = 'CO2_CMS_T', &
              LONG_NAME  = 'source species'  , &
              UNITS      = '1', &
              DIMS       = MAPL_DimsHorzOnly, &
              VLOCATION  = MAPL_VLocationNone, &
              RESTART    = MAPL_RestartSkip,   &
              RC         = STATUS)
              VERIFY_(STATUS)
         call MAPL_AddImportSpec(GC, &
              SHORT_NAME = 'CO2_CMS_S', &
              LONG_NAME  = 'source species'  , &
              UNITS      = '1', &
              DIMS       = MAPL_DimsHorzOnly, &
              VLOCATION  = MAPL_VLocationNone, &
              RESTART    = MAPL_RestartSkip,   &
              RC         = STATUS)
         VERIFY_(STATUS)
         call MAPL_AddImportSpec(GC, &
              SHORT_NAME = 'CO2_CMS_PICE', &
              LONG_NAME  = 'source species'  , &
              UNITS      = '1', &
              DIMS       = MAPL_DimsHorzOnly, &
              VLOCATION  = MAPL_VLocationNone, &
              RESTART    = MAPL_RestartSkip,   &
              RC         = STATUS)
         VERIFY_(STATUS)
         call MAPL_AddImportSpec(GC, &
              SHORT_NAME = 'CO2_CMS_PCO2', &
              LONG_NAME  = 'source species'  , &
              UNITS      = '1', &
              DIMS       = MAPL_DimsHorzOnly, &
              VLOCATION  = MAPL_VLocationNone, &
              RESTART    = MAPL_RestartSkip,   &
              RC         = STATUS)
         VERIFY_(STATUS)
 
      else
         call MAPL_AddImportSpec(GC, &
              SHORT_NAME = 'CO2_CMS_OCN', &
              LONG_NAME  = 'source species'  , &
              UNITS      = '1', &
              DIMS       = MAPL_DimsHorzOnly, &
              VLOCATION  = MAPL_VLocationNone, &
              RESTART    = MAPL_RestartSkip,   &
              RC         = STATUS)
         VERIFY_(STATUS)
      end if
   end if

   RETURN_(ESMF_SUCCESS)

   end subroutine CO2_GridCompSetServices

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CO2_GridCompInitialize --- Initialize CO2_GridComp
!
! !INTERFACE:
!

   subroutine CO2_GridCompInitialize ( gcCO2, w_c, impChem, expChem, &
                                      nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(Chem_Bundle), intent(in) :: w_c        ! Chemical tracer fields      
   integer, intent(in) :: nymd, nhms           ! time
   real,    intent(in) :: cdt                  ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   type(CO2_GridComp), intent(inout) :: gcCO2   ! Grid Component
   type(ESMF_State), intent(inout)  :: impChem  ! Import State
   type(ESMF_State), intent(inout)  :: expChem  ! Export State
   integer, intent(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: Initializes the CO2 Grid Component. It primarily sets
!               the import state for each active constituent package.
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!  24OCT2005     Bian  Mods for 5 tagged CO2  
!                      (total, fossil fuel, ecosystem, oceanic, and biomass)
!  25OCT2005     Bian  Mods for 5 regions
!
!EOP
!-------------------------------------------------------------------------

   character(len=*), parameter :: myname = 'CO2_GridCompInitialize'


   character(len=255) :: rcfilen = 'CO2_GridComp.rc'
   integer :: ios, n
   integer, allocatable :: ier(:)
   integer :: i1, i2, im, j1, j2, jm, km, ijl
   integer :: nbins, nbeg, nend, nbins_rc, nymd1, nhms1
   integer :: nTimes, begTime, incSecs
   real    :: qmin, qmax

   gcCO2%name = 'CO2 Constituent Package'
   gcCO2%BCnymd = -1
   gcCO2%dt_read = 0

!  Initialize local variables
!  --------------------------
   rc = 0
   i1 = w_c%grid%i1; i2 = w_c%grid%i2; im = w_c%grid%im
   j1 = w_c%grid%j1; j2 = w_c%grid%j2; jm = w_c%grid%jm
   km = w_c%grid%km
   nbins = w_c%reg%n_CO2;  nbeg  = w_c%reg%i_CO2; nend  = w_c%reg%j_CO2

   ijl = ( i2 - i1 + 1 ) * ( j2 - j1 + 1 )

   call init_()
   if ( rc /= 0 ) return

   ier(:) =0

!                       -------------------
!                       Parse resource file
!                       -------------------

!  Load resource file
!  ------------------
   call i90_loadf ( TRIM(rcfilen), ier(1) )
   if ( ier(1) .ne. 0 ) then
      call final_(10)
      return
   end if
   ier(:)=0

   call i90_label ( 'number_CO2_bins:', ier(1) )
   nbins_rc = i90_gint ( ier(2) )
   if ( nbins_rc /= nbins ) then
      call final_(11)
      return
   end if

!  Run-time switch to use CMS emissions
!  ------------------------------------
   CALL I90_label ( 'CMS_EMIS:', ier(3) )
   n = I90_gint ( ier(4) )
   IF(n /= 0) THEN
    gcCO2%CMS_EMIS = .TRUE.
    if ( MAPL_AM_I_ROOT() ) then
       print *, 'CO2_GridComp: Reading  C M S  Emissions'
    end if
   ELSE
    gcCO2%CMS_EMIS = .FALSE.
    if ( MAPL_AM_I_ROOT() ) then
       print *, 'CO2_GridComp: Reading CLIMATOLOGICAL Emissions'
    end if
   END IF

   IF (gcCO2%CMS_EMIS) THEN
    CALL I90_label ( 'CMS_biomass_emission_filename:', ier(5) )
    CALL I90_gtoken( gcCO2%eFilen_biomass, ier(6) )
    
    CALL I90_label ( 'CMS_biomass_emission_factor:', ier(7) )
    gcCO2%BBconFac = i90_gfloat ( ier(8) )

    CALL I90_label ( 'CMS_fossilfuel_emissions_filename:', ier(9) )
    CALL I90_gtoken( gcCO2%eFilen_ff, ier(10) )

    CALL I90_label ( 'CMS_fossilfuel_emissions_factor:', ier(11) )
    gcCO2%FFconFac = i90_gfloat ( ier(12) )

    CALL I90_label ( 'CMS_biosphere_emissions_filename:', ier(13) )
    CALL I90_gtoken( gcCO2%eFilen_nep, ier(14) )

    CALL I90_label ( 'CMS_ocean_emissions_filename:', ier(15) )
    CALL I90_gtoken( gcCO2%eFilen_ocn, ier(16) )
   ELSE
    CALL I90_label ( 'CO2_biomass_emission_filename:', ier(5) )
    CALL I90_gtoken( gcCO2%eFilen_biomass, ier(6) )

    CALL I90_label ( 'CO2_biomass_emission_factor:', ier(7) )
    gcCO2%BBconFac = i90_gfloat ( ier(8) )

    CALL I90_label ( 'CO2_fossilfuel_emissions_filename:', ier(9) )
    CALL I90_gtoken( gcCO2%eFilen_ff, ier(10) )

     CALL I90_label ( 'CO2_fossilfuel_emissions_factor:', ier(11) )
    gcCO2%FFconFac = i90_gfloat ( ier(12) )

    CALL I90_label ( 'CO2_biosphere_emissions_filename:', ier(13) )
    CALL I90_gtoken( gcCO2%eFilen_nep, ier(14) )

    CALL I90_label ( 'CO2_ocean_emissions_filename:', ier(15) )
    CALL I90_gtoken( gcCO2%eFilen_ocn, ier(16) )
   END IF

! Biosphere drawdown factor, used for adjusting biosphere drawdown.
! Valid range: >0.  If < 1, reduces sink.  If = 1, neutral.  If > 1, enhances sink.
! ---------------------------------------------------------------------------------
   gcCO2%BioDrawDownFactor = -1.00
   CALL I90_label ( 'Biosphere_drawdown_factor:', ier(17) )
   gcCO2%BioDrawDownFactor = I90_gfloat ( ier(18) )
   IF(gcCO2%BioDrawDownFactor < 0.00) THEN
    IF(MAPL_AM_I_ROOT()) THEN
     PRINT *," "
     PRINT *,TRIM(myname)//": Invalid biosphere drawdown factor."
    END IF
    CALL final_(12)
    RETURN
   END IF

   IF(MAPL_AM_I_ROOT()) THEN
    PRINT *," "
    PRINT *,TRIM(myname)//": "
    PRINT *," Biomass emission factor:     ", gcCO2%BBconFac
    PRINT *," Fossil fuel emission factor: ", gcCO2%FFconFac
    PRINT *," Biosphere drawdown factor:   ", gcCO2%BioDrawDownFactor
   END IF

!  Run-time debug switch
!  ---------------------
   CALL I90_label ( 'DEBUG:', ier(19) )
   n = I90_gint ( ier(20) )
   IF(n /= 0) THEN
    gcCO2%DBG = .TRUE.
   ELSE
    gcCO2%DBG = .FALSE.
   END IF

!  Get the desired regions to run on
!  ---------------------------------
   CALL I90_label ( 'CO2_regions:', ier(21) )
   CALL I90_gtoken( gcCO2%maskFileName, ier(22) )
   call i90_label ( 'CO2_regions_indices:', ier(23) )
   do n = 1, nbins
      gcCO2%regionIndex(n) = i90_gint ( ier(24+n) )
   end do

   IF( ANY( ier(:) /= 0 ) ) THEN
    CALL final_(13)
    RETURN
   END IF
   ier(:)=0

   call I90_label ( 'OCN_FLUX_CALC:' , ier(1))
   if (ier(1) == 0) then
      n = I90_gint( ier(2) )
   else
      n =0
   end if
   if(n /= 0) then
     gcCO2%OCN_FLUX_CALC=.true.
   else
     gcCO2%OCN_FLUX_CALC=.false.
   end if

   ier(:)=0

!  Check initial date of inventory emission/oxidant files
!  ------------------------------------------------------
!  The intent here is that these files are valid for a particular
!  YYYY or YYYYMMDD (if 1x year in file).  We need to request
!  the correct date
   if( index(gcCO2%eFilen_ff,'%') .le. 0) then
    call Chem_UtilGetTimeInfo( gcCO2%eFilen_ff, gcCO2%nymd_ff, &
                               begTime, nTimes, incSecs )
   endif
   if( index(gcCO2%eFilen_ocn,'%') .le. 0) then
    call Chem_UtilGetTimeInfo( gcCO2%eFilen_ocn, gcCO2%nymd_ocn, &
                               begTime, nTimes, incSecs )
   endif
   if( index(gcCO2%eFilen_nep,'%') .le. 0) then
    call Chem_UtilGetTimeInfo( gcCO2%eFilen_nep, gcCO2%nymd_nep, &
                               begTime, nTimes, incSecs )
   endif

   ier(1) = gcCO2%nymd_ff
   ier(2) = gcCO2%nymd_ocn
   ier(3) = gcCO2%nymd_nep

   if( any(ier(1:3) < 0 ) ) then
     call final_(14)
     return
   endif

   DEALLOCATE(ier)

   return

CONTAINS

   subroutine init_()
   integer ios, nerr
   nerr = max ( 100, nbins+27 )
   allocate (   gcCO2%eCO2_FF(i1:i2,j1:j2), & 
               gcCO2%eCO2_NEP(i1:i2,j1:j2), & 
               gcCO2%eCO2_OCN(i1:i2,j1:j2), & 
                gcCO2%eCO2_BB(i1:i2,j1:j2), & 
               gcCO2%eCO2_BB_(i1:i2,j1:j2), & 
             gcCO2%regionMask(i1:i2,j1:j2), &
                  gcCO2%regionIndex(nbins), &
                    gcCO2%SST(i1:i2,j1:j2), & 
                    gcCO2%SSS(i1:i2,j1:j2), & 
                   gcCO2%pCO2(i1:i2,j1:j2), & 
                   gcCO2%pice(i1:i2,j1:j2), & 
                      ier(nerr), stat=ios )

   if ( ios /= 0 ) rc = 100
   end subroutine init_

   subroutine final_(ierr)
   integer :: ierr
   integer ios
   deallocate ( gcCO2%eCO2_FF, gcCO2%eCO2_NEP, gcCO2%eCO2_OCN, &
                gcCO2%eCO2_BB, gcCO2%regionMask, gcCO2%regionIndex, &
                gcCO2%eCO2_BB_, gcCO2%SST, gcCO2%SSS, gcCO2%pCO2, &
                gcCO2%pice, ier, stat=ios )
   call i90_release()
   rc = ierr
   end subroutine final_

   end subroutine CO2_GridCompInitialize

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CO2_GridCompRun --- The Chem Driver 
!
! !INTERFACE:
!

   subroutine CO2_GridCompRun ( gcCO2, w_c, impChem, expChem, &
                               nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT/OUTPUT PARAMETERS:

   type(CO2_GridComp), intent(inout) :: gcCO2   ! Grid Component
   type(Chem_Bundle), intent(inout) :: w_c      ! Chemical tracer fields   

! !INPUT PARAMETERS:

   type(ESMF_State), intent(inout) :: impChem    ! Import State
   integer, intent(in) :: nymd, nhms          ! time
   real,    intent(in) :: cdt                 ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   type(ESMF_State), intent(inout) :: expChem     ! Export State
   integer, intent(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 -
 
! !DESCRIPTION: This routine implements the so-called CO2 Driver. That 
!               is, adds chemical tendencies to each of the constituents,
!  Note: water wapor, the first constituent is not considered a chemical
!  constituents.
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!  24OCT2005     Bian  Mods for 5 tagged CO2  
!                      (total, fossil fuel, ecosystem, oceanic, and biomass)
!  25OCT2005     Bian  Mods for 5 regions

!  Mask  Region
!  ----  -------------
!    1   North America
!    2   Mexico
!    3   Europe
!    4   Asia
!    5   Africa
!
!EOP
!-------------------------------------------------------------------------

   character(len=*), parameter :: myname = 'CO2_GridCompRun'
   character(len=*), parameter :: Iam = myname

!  Input fields from fvGCM
!  -----------------------
   REAL, POINTER, DIMENSION(:,:)   ::  pblh  => null()
   REAL, POINTER, DIMENSION(:,:)   ::  ps    => null()
   REAL, POINTER, DIMENSION(:,:)   ::  v10m  => null()
   REAL, POINTER, DIMENSION(:,:)   ::  u10m  => null()
   REAL, POINTER, DIMENSION(:,:,:) ::  T     => null()
   REAL, POINTER, DIMENSION(:,:,:) ::  rhoa  => null()
   REAL, POINTER, DIMENSION(:,:,:) ::  zle   => null()

   integer :: i1, i2, im, j1, j2, jm, km, idiag, ios, ijl
   integer :: i, j, k, n, nbins, nbeg, nend
   INTEGER :: nymd1, nhms1, ier(7)

   REAL    :: qmin, qmax, BBconFac, c2co2

   REAL, PARAMETER :: mwtCO2 = 44.00
   REAL, PARAMETER :: mwtC   = 12.00
   INTEGER, PARAMETER :: emis_freq  = 10800

   LOGICAL:: doingBB
   
   REAL, POINTER, DIMENSION(:,:)   :: ptr2d => null()
   REAL, POINTER, DIMENSION(:,:,:) :: ptr3d => null()

#define EXPORT     expChem

#define ptrCO2EM      CO2_emis
#define ptrCO2CL      CO2_column
#define ptrCO2SC      CO2_surface

   integer :: STATUS

#include "CO2_GetPointer___.h"

!  Initialize local variables
!  --------------------------
   rc = 0
   i1 = w_c%grid%i1; i2 = w_c%grid%i2; im = w_c%grid%im
   j1 = w_c%grid%j1; j2 = w_c%grid%j2; jm = w_c%grid%jm
   km = w_c%grid%km
   ijl = ( i2 - i1 + 1 ) * ( j2 - j1 + 1 )

!  Retreive mask
!  -------------
   call MAPL_GetPointer(impChem,ptr2d,'CO2_regionMask',rc=status)
   VERIFY_(STATUS)
   gcCO2%regionMask=ptr2d

   nbins = w_c%reg%n_CO2;  nbeg  = w_c%reg%i_CO2; nend  = w_c%reg%j_CO2

   c2co2 = mwtCO2 / mwtC

   if ( any((/NBIN_CO2CL,NBIN_CO2SC/)/=NBIN_CO2EM)) then
      call die(myname,'all emissions in registry must have same number of bins')
   endif
   if ( nbins > NBIN_CO2EM ) then
      call die(myname,'nbins in chem registry must be <= those in component registry')
   end if

!  Are we including biomass burning?
!  ---------------------------------
   !IF(INDEX(gcCO2%eFilen_biomass,'null') .GT. 0 ) THEN
    !doingBB = .FALSE.
   !ELSE
    !doingBB = .TRUE.
   !END IF
   doingBB = .true.

!   Read climatological emissions, if specified
!   -------------------------------------------
    IF ( .NOT. gcCO2%CMS_EMIS ) THEN

!   Biomass burning climatology, is in kg C m^-2 s^-1
!   ------------------------------------------------- 
    call MAPL_GetPointer(impChem, ptr2d, 'CO2_BIOMASS',rc=status)
    VERIFY_(STATUS)
    gcCO2%eCO2_BB = ptr2d
    BBconFac = gcCO2%BBconFac


! Fossil fuel emissions
    call MAPL_GetPointer(impChem, ptr2d, 'CO2_FF',rc=status)
    VERIFY_(STATUS)
    gcCO2%eCO2_FF = ptr2d

! Biosphere flux
! --------------
    call MAPL_GetPointer(impChem, ptr2d, 'CO2_NEP',rc=status)
    VERIFY_(STATUS)
    gcCO2%eCO2_NEP = ptr2d
! Ocean flux
! ----------
    call MAPL_GetPointer(impChem, ptr2d, 'CO2_OCN',rc=status)
    VERIFY_(STATUS)
    gcCO2%eCO2_OCN = ptr2d

    IF(gcCO2%DBG) THEN
     CALL pmaxmin('CO2: e_ff',  gcCO2%eCO2_FF,  qmin, qmax, ijl, 1, 1. )
     CALL pmaxmin('CO2: e_nep', gcCO2%eCO2_NEP, qmin, qmax, ijl, 1, 1. )
     CALL pmaxmin('CO2: e_ocn', gcCO2%eCO2_OCN, qmin, qmax, ijl, 1, 1. )
     CALL pmaxmin('CO2: e_bb',  gcCO2%eCO2_BB,  qmin, qmax, ijl, 1, 1. )
    END IF


!     Bian says that we need to adjust the uptake flux of CO2 in the
!     ecosystem database to reflect the emissions from biomass burning.
!     In principle this adds a factor that needs to be balanced on an
!     interannual basis.  For year 2000 TRMM (GFED v 1.2) emissions this
!     factor is 1.2448
!     ------------------------------------------------------------------
      WHERE(gcCO2%eCO2_NEP(i1:i2,j1:j2) .gt. 0.0) &
          gcCO2%eCO2_NEP(i1:i2,j1:j2) = gcCO2%eCO2_NEP(i1:i2,j1:j2)*c2co2
      WHERE(gcCO2%eCO2_NEP(i1:i2,j1:j2) .le. 0.0) &
          gcCO2%eCO2_NEP(i1:i2,j1:j2) = gcCO2%eCO2_NEP(i1:i2,j1:j2)*c2co2*gcCO2%BioDrawDownFactor
      gcCO2%eCO2_OCN(i1:i2,j1:j2) = gcCO2%eCO2_OCN(i1:i2,j1:j2)*c2co2

      gcCO2%eCO2_BB(i1:i2,j1:j2) = gcCO2%eCO2_BB(i1:i2,j1:j2)*BBconFac
      gcCO2%eCO2_FF(i1:i2,j1:j2) = gcCO2%eCO2_FF(i1:i2,j1:j2)*gcCO2%FFconFac
      

    ELSE ! TYPE OF EMISS IS CMS			     

      call MAPL_GetPointer(impChem, ptr2d, 'CO2_CMS_BIOMASS',rc=status)
      VERIFY_(STATUS)
      gcCO2%eCO2_BB = ptr2d
      
      call MAPL_GetPointer(impChem, ptr2d, 'CO2_CMS_FF',rc=status)
      VERIFY_(STATUS)
      gcCO2%eCO2_FF = ptr2d

      call MAPL_GetPointer(impChem, ptr2d, 'CO2_CMS_NEP',rc=status)
      VERIFY_(STATUS)
      gcCO2%eCO2_NEP = ptr2d

      IF( gcCO2%OCN_FLUX_CALC )  THEN
         call MAPL_GetPointer(impChem, ptr2d, 'CO2_CMS_T',rc=status)
         VERIFY_(STATUS)
         gcCO2%sst = ptr2d
         call MAPL_GetPointer(impChem, ptr2d, 'CO2_CMS_S',rc=status)
         VERIFY_(STATUS)
         gcCO2%sss = ptr2d
         call MAPL_GetPointer(impChem, ptr2d, 'CO2_CMS_PICE',rc=status)
         VERIFY_(STATUS)
         gcCO2%pice = ptr2d
         call MAPL_GetPointer(impChem, ptr2d, 'CO2_CMS_PCO2',rc=status)
         VERIFY_(STATUS)
         gcCO2%pco2 = ptr2d
      else
         call MAPL_GetPointer(impChem, ptr2d, 'CO2_CMS_OCN',rc=status)
         VERIFY_(STATUS)
         gcCO2%eCO2_OCN = ptr2d
      end if


      BBconFac = gcCO2%BBconFac
      gcCO2%eCO2_NEP(i1:i2,j1:j2) = gcCO2%eCO2_NEP(i1:i2,j1:j2)*c2co2
      gcCO2%eCO2_BB(i1:i2,j1:j2) = gcCO2%eCO2_BB(i1:i2,j1:j2)*BBconFac*c2co2
      gcCO2%eCO2_FF(i1:i2,j1:j2) = gcCO2%eCO2_FF(i1:i2,j1:j2)*gcCO2%FFconFac

      IF( .NOT. gcCO2%OCN_FLUX_CALC ) gcCO2%eCO2_OCN(i1:i2,j1:j2) = gcCO2%eCO2_OCN(i1:i2,j1:j2)*c2co2

   END IF ! type of emiss  

!   Save this in case we need to apply diurnal cycle
!   ------------------------------------------------
   if ( w_c%diurnal_bb ) then
        gcCO2%eCO2_BB_(:,:) = gcCO2%eCO2_BB(:,:)
   end if

!  Units for surface flux must be kgCO2 m^-2 s^-1
!  ----------------------------------------------
    gcCO2%eCO2_FF(i1:i2,j1:j2) = gcCO2%eCO2_FF(i1:i2,j1:j2)*c2co2

!  Apply diurnal cycle if so desired
!  ---------------------------------
   if ( w_c%diurnal_bb ) then
      call Chem_BiomassDiurnal ( gcCO2%eCO2_BB, gcCO2%eCO2_BB_,   &
                                 w_c%grid%lon(:,:)*radToDeg, &
                                 w_c%grid%lat(:,:)*radToDeg, nhms, cdt )      
   end if

!  Get imports
!  -----------
   call MAPL_GetPointer ( impChem, pblh, 'ZPBL',    rc=ier(1) ) 
   call MAPL_GetPointer ( impChem, T,    'T',       rc=ier(2) ) 
   call MAPL_GetPointer ( impChem, rhoa, 'AIRDENS', rc=ier(3) ) 
   call MAPL_GetPointer ( impChem, zle,  'ZLE',     rc=ier(4) ) 
   call MAPL_GetPointer ( impChem, u10m, 'U10M',    rc=ier(5) )
   call MAPL_GetPointer ( impChem, v10m, 'V10M',    rc=ier(6) )
   call MAPL_GetPointer ( impChem, ps,   'PS',      rc=ier(7) )

   IF(gcCO2%DBG) THEN
    CALL pmaxmin('CO2: pblh', pblh, qmin, qmax, ijl, 1,    1. )
    CALL pmaxmin('CO2: ps',   ps,   qmin, qmax, ijl, 1,    1. )
    CALL pmaxmin('CO2: u10m', u10m, qmin, qmax, ijl, 1,    1. )
    CALL pmaxmin('CO2: v10m', v10m, qmin, qmax, ijl, 1,    1. )
    CALL pmaxmin('CO2: T',    T,    qmin, qmax, ijl, km,   1. )
    CALL pmaxmin('CO2: rhoa', rhoa, qmin, qmax, ijl, km,   1. )
    CALL pmaxmin('CO2: zle',  zle,  qmin, qmax, ijl, km+1, 1. )
   END IF

!  CO2 Emissions
!  -------------
   CALL CO2_Emission(rc)


!  Fill the export states
!  ----------------------
!  Surface concentration in ppmv
   do n = 1, nbins
    if(associated(CO2_surface(n)%data2d)) &
      CO2_surface(n)%data2d(i1:i2,j1:j2) = w_c%qa(nbeg+n-1)%data3d(i1:i2,j1:j2,km)*1.e6
   enddo

!  Column burden in kg m-2
!  -----------------------
   do n = 1, nbins
     if(associated(CO2_column(n)%data2d)) then
      CO2_column(n)%data2d(i1:i2,j1:j2) = 0.
      do k = 1, km
       CO2_column(n)%data2d(i1:i2,j1:j2) &
        =   CO2_column(n)%data2d(i1:i2,j1:j2) &
          +   w_c%qa(nbeg+n-1)%data3d(i1:i2,j1:j2,k)*w_c%delp(i1:i2,j1:j2,k)
      enddo
      CO2_column(n)%data2d(i1:i2,j1:j2)=CO2_column(n)%data2d(i1:i2,j1:j2)/(ps(i1:i2,j1:j2)-w_c%grid%ptop)
    endif
   enddo

!  Fill the export state with current mixing ratios
!  ------------------------------------------------
   if(associated(CO2%data3d)) &
      CO2%data3d(i1:i2,j1:j2,1:km) = w_c%qa(nbeg)%data3d(i1:i2,j1:j2,1:km)
   if(associated(CO2NAMER%data3d)) &
      CO2NAMER%data3d(i1:i2,j1:j2,1:km) = w_c%qa(nbeg+1)%data3d(i1:i2,j1:j2,1:km)
   if(associated(CO2SAMER%data3d)) &
      CO2SAMER%data3d(i1:i2,j1:j2,1:km) = w_c%qa(nbeg+2)%data3d(i1:i2,j1:j2,1:km)
   if(associated(CO2AFRIC%data3d)) &
      CO2AFRIC%data3d(i1:i2,j1:j2,1:km) = w_c%qa(nbeg+3)%data3d(i1:i2,j1:j2,1:km)

   return

CONTAINS

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  CO2_Emission - Adds emissions for CO2 for one timestep
!             We have emissions from 4 sources, which are distributed
!             differently in the vertical
!             1) fossil fuel - emitted at surface
!             2) ecosystem   - fluxes at surface
!             3) oceanic     - fluxes at surface
!             4) biomass burning - uniformly mixed in PBL
!
! !DESCRIPTION: Updates the CO2 concentration with emissions every timestep
!
! !REVISION HISTORY:
!
!  24Oct2005, Bian
!  26Nov2010, Nielsen Simplified PBL partitioning for biomass burning emissions 
!  
! !INTERFACE:
!
!EOP
!-------------------------------------------------------------------------
   SUBROUTINE CO2_Emission ( rc )
!-------------------------------------------------------------------------

! !USES:

  IMPLICIT NONE

! !INPUT PARAMETERS:

! !OUTPUT PARAMETERS:

   INTEGER, INTENT(OUT) :: rc ! Error return code

! !LOCAL VARIABLES

   CHARACTER(LEN=*), PARAMETER :: myname = 'CO2_Emission'

   INTEGER ::  i, j, k, kt, minkPBL
   INTEGER, ALLOCATABLE :: index(:)

   REAL, PARAMETER :: mwtCO2=44.00
   REAL, ALLOCATABLE :: pblLayer(:,:),sfcFlux(:,:),myMask(:,:)
   REAL, ALLOCATABLE :: fPBL(:,:,:)

   real :: srctot,nobm_flux
   real :: scco2, scco2arg,wssq,rkwco2,tk,tk100,tk1002,ff
   real :: ffuatm,xco2,deltco2,wspd,flxmolm2

   rc = 0

! Grab some memory for manipulating emissions, ...
! ------------------------------------------------
   ALLOCATE(sfcFlux(i1:i2,j1:j2),STAT=ios)

! ... for the partitioning of BB, ...
! -----------------------------------
   ALLOCATE(fPBL(i1:i2,j1:j2,1:km),STAT=ios)
   fPBL(i1:i2,j1:j2,1:km)=0.00

! ... and for local copy of the region mask.
! ------------------------------------------
   ALLOCATE(myMask(i1:i2,j1:j2),STAT=ios)

! Get fractions
! -------------
   Fractions: IF(doingBB) THEN

! Find the layer that contains the PBL.
! Layer thicknesses are ZLE(:,:,0:km).
! -------------------------------------
    ALLOCATE(index(0:km),STAT=ios)
    ALLOCATE(pblLayer(i1:i2,j1:j2),STAT=ios)
    DO j=j1,j2
     DO i=i1,i2
      index(0:km)=0
      WHERE(zle(i,j,0:km)-zle(i,j,km) > pblh(i,j)) index(0:km)=1
      pblLayer(i,j)=SUM(index)

! Calculate NOBM fluxes using archived ocean fields.
! --------------------------------------------------
   IF (gcCO2%OCN_FLUX_CALC ) then
      if ( gcCO2%sst(i,j) .lt. 1000.) THEN
       scco2 = 2073.1 - 125.62*gcCO2%sst(i,j) + 3.6276*gcCO2%sst(i,j)**2. - &
               0.043219*gcCO2%sst(i,j)**3.
       scco2arg = (scco2/660.0)**(-0.5)
       wspd = (u10m(i,j)**2 + v10m(i,j)**2)**0.5
       wssq = wspd*wspd
       rkwco2 = wssq*scco2arg*0.337/(3.6E5) 
       tk = gcCO2%sst(i,j) + 273.15
       tk100 = tk*0.01
       tk1002 = tk100*tk100
       ff = exp(-162.8301 + 218.2968/tk100  +                       &
               90.9241*log(tk100) - 1.47696*tk1002 +                &
               gcCO2%sss(i,j) * (.025695 - .025225*tk100 +          &
               0.0049867*tk1002))   
       ffuatm = ff*1.0E-6                    
       xco2 = w_c%qa(nbeg)%data3d(i,j,km)*ps(i,j)*1.0E4/1013.25
       deltco2 = (xco2-gcCO2%pco2(i,j))*ffuatm*1024.5   !mol/m3
       flxmolm2 = rkwco2*deltco2             !units of mol/m2/s
       gcCO2%eCO2_OCN(i,j) = -flxmolm2*44.0E-3*(100.-gcCO2%pice(i,j))/100.    
      ELSE  ! SST must be undef
       gcCO2%eCO2_OCN(i,j) = 0.   
      END IF 
   END IF ! DOING CMS EMISSIONS

     END DO
    END DO
    minkPBL=MINVAL(pblLayer)

! Determine partitioning fraction based on layer thicknesses
! ----------------------------------------------------------
    DO j=j1,j2
     DO i=i1,i2
      kt=pblLayer(i,j)
      DO k=kt,km
       fPBL(i,j,k)=(zle(i,j,k-1)-zle(i,j,k))/(zle(i,j,kt-1)-zle(i,j,km))
      END DO
     END DO
    END DO

! Release memory
! --------------
    DEALLOCATE(index,STAT=ios)
    DEALLOCATE(pblLayer,STAT=ios)

   END IF Fractions

! For each CO2 bin ...
! --------------------
   CO2_Bin: DO n=1,nbins

! Finalize the mask. Update CO2 globally if the region index is -1.
! Otherwise update only where the mask's value is the region index.
! -----------------------------------------------------------------
    IF(gcCO2%regionIndex(n) == -1) THEN
     myMask(i1:i2,j1:j2)=gcCO2%regionIndex(n)
    ELSE
     myMask(i1:i2,j1:j2)=gcCO2%regionMask(i1:i2,j1:j2)
    END IF

! Establish range of layers on which to work
! ------------------------------------------
    IF(doingBB) THEN
     kt=minkPBL
    ELSE
     kt=km
    END IF

! For each layer ...
! --------------------
    Layer: DO k=kt,km

! Emissions: Weighted biomass burning if active
! ---------------------------------------------
     IF(doingBB) THEN
      sfcFlux(i1:i2,j1:j2)=gcCO2%eCO2_BB(i1:i2,j1:j2)*fPBL(i1:i2,j1:j2,k)
     ELSE
      sfcFlux(i1:i2,j1:j2)=0.00
     END IF

! Add Fossil fuel, net ecosystem production, and ocean source when in surface layer
! ---------------------------------------------------------------------------------
     IF(k == km) sfcFlux(i1:i2,j1:j2)= sfcFlux(i1:i2,j1:j2)+ &
                                       gcCO2%eCO2_FF(i1:i2,j1:j2)+             &
                                       gcCO2%eCO2_NEP(i1:i2,j1:j2)+            &
                                       gcCO2%eCO2_OCN(i1:i2,j1:j2)

! Update CO2 for this bin
! -----------------------
     WHERE(myMask(i1:i2,j1:j2) == gcCO2%regionIndex(n)) &
      w_c%qa(nbeg+n-1)%data3d(i1:i2,j1:j2,k) = w_c%qa(nbeg+n-1)%data3d(i1:i2,j1:j2,k)+ &
                                               cdt*sfcFlux(i1:i2,j1:j2)* &
                                               (MAPL_AIRMW/mwtCO2)/(w_c%delp(i1:i2,j1:j2,k)/MAPL_GRAV)
! Next layer
! ----------
    END DO Layer

! Update Surface flux diagnostic for this bin
! -------------------------------------------
    IF(ASSOCIATED(CO2_emis(n)%data2d)) THEN
     CO2_emis(n)%data2d(i1:i2,j1:j2)=0.00

     sfcFlux(i1:i2,j1:j2)=gcCO2%eCO2_FF(i1:i2,j1:j2)+gcCO2%eCO2_NEP(i1:i2,j1:j2)+ &
                          gcCO2%eCO2_OCN(i1:i2,j1:j2)
     IF(doingBB) sfcFlux(i1:i2,j1:j2)=sfcFlux(i1:i2,j1:j2)+gcCO2%eCO2_BB(i1:i2,j1:j2)

     WHERE(myMask(i1:i2,j1:j2) == gcCO2%regionIndex(n)) &
       CO2_emis(n)%data2d(i1:i2,j1:j2)=sfcFlux(i1:i2,j1:j2)
    END IF

! Next bin
! --------
   END DO CO2_Bin

! Release memory
! --------------
    DEALLOCATE(fPBL,STAT=ios)
    DEALLOCATE(myMask,STAT=ios)
    DEALLOCATE(sfcFlux,STAT=ios)

   RETURN
   END SUBROUTINE CO2_Emission

 END SUBROUTINE CO2_GridCompRun

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CO2_GridCompFinalize --- The Chem Driver 
!
! !INTERFACE:
!

   subroutine CO2_GridCompFinalize ( gcCO2, w_c, impChem, expChem, &
                                    nymd, nhms, cdt, rc )

! !USES:

  implicit NONE

! !INPUT/OUTPUT PARAMETERS:

   type(CO2_GridComp), intent(inout) :: gcCO2   ! Grid Component

! !INPUT PARAMETERS:

   type(Chem_Bundle), intent(in)  :: w_c      ! Chemical tracer fields   
   integer, intent(in) :: nymd, nhms          ! time
   real,    intent(in) :: cdt                 ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   type(ESMF_State), intent(inout) :: impChem   ! Import State
   type(ESMF_State), intent(inout) :: expChem   ! Import State
   integer, intent(out) ::  rc                  ! Error return code:
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

   character(len=*), parameter :: myname = 'CO2_GridCompFinalize'
   INTEGER :: ios

   DEALLOCATE ( gcCO2%eCO2_FF, gcCO2%eCO2_NEP, gcCO2%eCO2_OCN, &
                gcCO2%eCO2_BB, gcCO2%regionMask, gcCO2%SST, &
                gcCO2%SSS, gcCO2%pCO2, gcCO2%pice, STAT=ios )
   rc = 0
   IF ( ios /= 0 ) rc = 1

   return

 end subroutine CO2_GridCompFinalize

 end module CO2_GridCompMod

