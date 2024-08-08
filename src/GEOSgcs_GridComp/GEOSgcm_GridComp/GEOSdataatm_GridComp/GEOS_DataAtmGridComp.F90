!$Id: GEOS_DataAtmGridComp.F90,v 1.15.6.11.4.18.2.1.2.1.2.5.2.4.4.5.8.4.2.1 2021/05/21 20:59:48 atrayano Exp $

#include "MAPL_Generic.h"

!=============================================================================

module GEOS_DataAtmGridCompMod

!BOP

! !MODULE: GEOS_DataAtm -- A ``fake'' atmospheric component.

! !USES: 

  use ESMF
  use MAPL_Mod
!  use AdvCore_GridCompMod,        only : FVSetServices        => SetServices
  use FVdycoreCubed_GridComp,     only : FVSetServices        => SetServices
  use GEOS_SurfaceGridCompMod,    only : SurfSetServices      => SetServices
  use GEOS_UtilsMod

  use ice_init,           only: dealloc_column_physics

  implicit none
  private

! !PUBLIC MEMBER FUNCTIONS:

  public SetServices

!=============================================================================

! !DESCRIPTION:
! 
!   {\tt GEOS\_DataAtm  } is a gridded component that ...??

!EOP

  integer            :: SURF
  integer            :: FV

  integer            :: DO_OBIO         ! default (=0) is to run without ocean bio and chem
  integer, parameter :: DO_CO2SC  = 0
  logical, parameter :: DO_GOSWIM = .false.

  integer, parameter :: NUM_DUDP = 5
  integer, parameter :: NUM_DUWT = 5
  integer, parameter :: NUM_DUSD = 5
  integer, parameter :: NUM_BCDP = 2                           ! number of Black Carbon 
  integer, parameter :: NUM_BCWT = 2
  integer, parameter :: NUM_OCDP = 2                           ! number of Organic Carbon 
  integer, parameter :: NUM_OCWT = 2

  integer, parameter :: NB_CHOU_UV  = 5                        ! number of UV bands
  integer, parameter :: NB_CHOU_NIR = 3                        ! number of near-IR bands
  integer, parameter :: NB_CHOU     = NB_CHOU_UV + NB_CHOU_NIR ! total number of bands
  integer, parameter :: NB_OBIO     = 33

  integer, parameter :: ICE   = 1
  integer, parameter :: WATER = 2
  integer, parameter :: OBIO  = 3

  real,    parameter :: KUVR = 0.09
  real,    parameter :: EPS6 = 1.e-7

#include "definebio.h"
#include "comlte.h"

  type T_ORADBIO_STATE
    private
    integer :: lam(nlt)
    real    :: aw(nlt),bw(nlt),excdom(nlt),exdet(nlt),WtoQ(nlt),wfac(nlt)
    real    :: ac(nchl,nlt),bc(nchl,nlt)
    real    :: bpic(nlt)
    real    :: rad,pi2
    real    :: Fobar(nlt),thray(nlt),oza(nlt),awv(nlt),ao(nlt),aco2(nlt)
    real    :: am,Vi
    real    :: asl(ncld),bsl(ncld),csl(ncld),dsl(ncld),esl(ncld),fsl(ncld)
    integer :: ica(nlt)
    type(ESMF_Alarm) :: DayAlarm
  end type T_ORADBIO_STATE

  type ORADBIO_WRAP
     type (T_ORADBIO_STATE), pointer :: PTR => null()
  end type ORADBIO_WRAP

  type bandptr
   real, pointer, dimension(:,:)  :: b 
  end type bandptr

  type dustptr
   real, pointer, dimension(:,:)  :: d
  end type dustptr  

  character(len = 3) :: suffix
  character(len = 3) :: label
  integer k, nl 

   contains

!BOP

! !IROUTINE: SetServices -- Sets ESMF services for this component

! !INTERFACE:

  subroutine SetServices ( GC, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
    integer, optional                  :: RC  ! return code

!  !DESCRIPTION: This version uses the MAPL_GenericSetServices. This function sets
!                the Initialize and Finalize services, as well as allocating
!   our instance of a generic state and putting it in the 
!   gridded component (GC). Here we only need to set the run method and
!   add the state variable specifications (also generic) to our instance
!   of the generic state. This is the way our true state variables get into
!   the ESMF_State INTERNAL, which is in the MAPL_MetaComp.
!
!EOP

!=============================================================================
!
! ErrLog Variables


    character(len=ESMF_MAXSTR)              :: IAm
    integer                                 :: STATUS
    character(len=ESMF_MAXSTR)              :: COMP_NAME

! Local derived type aliases

    type (ESMF_Config)                      :: CF
    integer                                 :: DO_CICE_THERMO  ! default (=1) is to run with CICE
    type (MAPL_MetaComp),  pointer          :: MAPL

!=============================================================================

! Begin...

! Get my name and set-up traceback handle
! ---------------------------------------

    Iam = "SetServices"
    call ESMF_GridCompGet( GC, NAME=COMP_NAME, CONFIG=CF, RC=STATUS); VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // Iam


! Get my MAPL_Generic state
!--------------------------

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_GetResource ( MAPL, DO_CICE_THERMO, Label="USE_CICE_Thermo:" , DEFAULT=1, RC=STATUS)
    VERIFY_(STATUS)

! Get constants from CF
! ---------------------

    if (DO_CICE_THERMO == 0) then
       if(MAPL_AM_I_ROOT()) then
          print *, 'Current DATA ATM Gridded Component'
          print *, 'Needs CICE Thermodynamics! You turned it off, so this run will now terminate!'
          ASSERT_(DO_CICE_THERMO == 0)
       endif
    endif

! Ocean biology and chemistry: using OBIO or not?
! ------------------------------------------------

    call MAPL_GetResource ( MAPL, DO_OBIO, Label="USE_OCEANOBIOGEOCHEM:", DEFAULT=0, RC=STATUS)
    VERIFY_(STATUS)

! Set the state variable specs.
! -----------------------------

!BOS

!  !IMPORT STATE:

    call MAPL_AddImportSpec(GC,                   &
      SHORT_NAME = 'PS',                          &
      LONG_NAME  = 'surface_pressure',            &
      UNITS      = 'Pa',                          &
      DIMS       = MAPL_DimsHorzOnly,             &
      VLOCATION  = MAPL_VLocationNone,      RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                   &
      SHORT_NAME = 'TA',                          &
      LONG_NAME  = 'surface_air_temperature',     &
      UNITS      = 'K',                           &
      DIMS       = MAPL_DimsHorzOnly,             &
      VLOCATION  = MAPL_VLocationNone,      RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,              &
      SHORT_NAME = 'QA',                          &
      LONG_NAME  = 'surface_specific_humidity',   &
      UNITS      = '1',                           &  ! convert to kg/kg?? or g/kg??
      DIMS       = MAPL_DimsHorzOnly,             &
      VLOCATION  = MAPL_VLocationNone,      RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,              &
      SHORT_NAME = 'UA',                          &
      LONG_NAME  = '10-meter_eastward_wind',      &
      UNITS      = 'm s-1',                       &
      DIMS       = MAPL_DimsHorzOnly,             &
      VLOCATION  = MAPL_VLocationNone,      RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,              &
      SHORT_NAME = 'VA',                          &
      LONG_NAME  = '10-meter_northward_wind',     &
      UNITS      = 'm s-1',                       &
      DIMS       = MAPL_DimsHorzOnly,             &
      VLOCATION  = MAPL_VLocationNone,      RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,              &
      SHORT_NAME = 'RUNOFF',                      &
      LONG_NAME  = 'overland_runoff_including_throughflow', &
      UNITS      = 'kg m-2 s-1',                  &
      DIMS       = MAPL_DimsHorzOnly,             &
      VLOCATION  = MAPL_VLocationNone,      RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                   &
      SHORT_NAME = 'PCU',                         &
      LONG_NAME  = 'convective_rainfall',         &
      UNITS      = 'kg m-2 s-1',                  &
      DIMS       = MAPL_DimsHorzOnly,             &
      VLOCATION  = MAPL_VLocationNone,      RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,              &
      SHORT_NAME = 'PLS',                         &
      LONG_NAME  = 'large_scale_rainfall',        &
      UNITS      = 'kg m-2 s-1',                  &
      DIMS       = MAPL_DimsHorzOnly,             &
      VLOCATION  = MAPL_VLocationNone,      RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,              &
      SHORT_NAME = 'SNO',                         &
      LONG_NAME  = 'snowfall',                    &
      UNITS      = 'kg m-2 s-1',                  &
      DIMS       = MAPL_DimsHorzOnly,             &
      VLOCATION  = MAPL_VLocationNone,      RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,               &
      SHORT_NAME = 'LWDN',                        &
      LONG_NAME  = 'open_water_downward_longwave_flux',&
      UNITS      = 'W m-2',                       &
      DIMS       = MAPL_DimsHorzOnly,             &
      VLOCATION  = MAPL_VLocationNone,      RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                   &
      SHORT_NAME = 'SWGDWN',                      &
      LONG_NAME  = 'surface_incoming_shortwave_flux',&
      UNITS      = 'W m-2',                       &
      DIMS       = MAPL_DimsHorzOnly,             &
      VLOCATION  = MAPL_VLocationNone,      RC=STATUS)
    VERIFY_(STATUS)

! Internal
    call MAPL_AddInternalSpec(GC,            &
      SHORT_NAME = 'TS',                          &
      LONG_NAME  = 'surface_temperature',         &
      UNITS      = 'K',                           &
      DIMS       = MAPL_DimsHorzOnly,             &
      VLOCATION  = MAPL_VLocationNone,      RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,            &
      SHORT_NAME = 'EMIS',                        &
      LONG_NAME  = 'surface_emissivity',          &
      UNITS      = '1',                           &
      DEFAULT    = 1.0,                           &
      DIMS       = MAPL_DimsHorzOnly,             &
      VLOCATION  = MAPL_VLocationNone,      RC=STATUS)
    VERIFY_(STATUS)

    if (DO_OBIO/=0) then
      call OBIO_SetServices(RC)
    end if

! Set the Initialize and Run entry points
! ---------------------------------------

    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_INITIALIZE, Initialize, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN,        Run, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_FINALIZE,  Finalize, RC=STATUS)
    VERIFY_(STATUS)

! Create children`s gridded components and invoke their SetServices
! -----------------------------------------------------------------

    FV = MAPL_AddChild(GC, NAME='AdvCore', SS=FVSetServices, RC=STATUS)
    VERIFY_(STATUS)
    SURF = MAPL_AddChild(GC, NAME='SURFACE', SS=SurfSetServices, RC=STATUS)
    VERIFY_(STATUS)

! Set the state variable specs.
! -----------------------------

!BOS

!  !IMPORT STATE:
! none for now

!  !INTERNAL STATE:
! none for now

!  !EXPORT STATE:

!EOS

! Set generic init and final methods
! ----------------------------------

    call MAPL_TimerAdd(GC,    name="INITIALIZE", RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="RUN"       , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="FINALIZE"  , RC=STATUS)
    VERIFY_(STATUS)

    ! This call is needed only when we use ReadForcing.
    ! If we switch to use ExtData, next line has be commented out
    call MAPL_TerminateImport    ( GC, ALL=.true., RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_GenericSetServices    ( GC, RC=STATUS)
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)
  
    contains 

    subroutine OBIO_SetServices(RC)

      integer, optional,      intent(  OUT) ::  RC
      integer                               :: STATUS

! obgc imports

      call MAPL_AddImportSpec(GC,                  &
        SHORT_NAME     = 'CO2SC',                        &
        LONG_NAME      = 'Atmospheric CO2',              &
        UNITS          = '1e-6',                         &
        DIMS           = MAPL_DimsHorzOnly,              &
        VLOCATION      = MAPL_VLocationNone,       RC=STATUS)
      VERIFY_(STATUS)         

      do k=1,NUM_DUDP
        write(unit = suffix, fmt = '(i3.3)') k
        call MAPL_AddImportSpec(GC,                  &
          SHORT_NAME     = 'DUDP'//suffix,                &
          LONG_NAME      = 'Dry Dust Deposition',          &
          UNITS          = 'kg m-2 s-1',                   &
          DIMS           = MAPL_DimsHorzOnly,              &
          VLOCATION      = MAPL_VLocationNone,       RC=STATUS)
        VERIFY_(STATUS)
    
        call MAPL_AddImportSpec(GC,                  &
          LONG_NAME      = 'Wet Dust Deposition',          &
          UNITS          = 'kg m-2 s-1',                   &
          SHORT_NAME     = 'DUWT'//suffix,                &
          DIMS           = MAPL_DimsHorzOnly,              &
          VLOCATION      = MAPL_VLocationNone,       RC=STATUS)
        VERIFY_(STATUS)
    
        call MAPL_AddImportSpec(GC,                  &
          LONG_NAME      = 'Dust Sedimentation',           &
          UNITS          = 'kg m-2 s-1',                   &
          SHORT_NAME     = 'DUSD'//suffix,                &
          DIMS           = MAPL_DimsHorzOnly,              &
          VLOCATION      = MAPL_VLocationNone,       RC=STATUS)
        VERIFY_(STATUS)
      enddo

#ifdef CARBON_to_OBIO
      call MAPL_AddImportSpec(GC,                  &
        LONG_NAME      = 'Black Carbon Dry Deposition',  &
        UNITS          = 'kg m-2 s-1',                   &
        SHORT_NAME     = 'BCDP',                         &
        DIMS           = MAPL_DimsHorzOnly,              &
        UNGRIDDED_DIMS = (/NUM_BCDP/),                   &
        VLOCATION      = MAPL_VLocationNone,       RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_AddImportSpec(GC,                  &
        LONG_NAME      = 'Black Carbon Wet Deposition',  &
        UNITS          = 'kg m-2 s-1',                   &
        SHORT_NAME     = 'BCWT',                         &
        DIMS           = MAPL_DimsHorzOnly,              &
        UNGRIDDED_DIMS = (/NUM_BCWT/),                   &
        VLOCATION      = MAPL_VLocationNone,       RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_AddImportSpec(GC,                  &
        LONG_NAME      = 'Organic Carbon Dry Deposition',&
        UNITS          = 'kg m-2 s-1',                   &
        SHORT_NAME     = 'OCDP',                         &
        DIMS           = MAPL_DimsHorzOnly,              &
        UNGRIDDED_DIMS = (/NUM_OCDP/),                   &
        VLOCATION      = MAPL_VLocationNone,       RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_AddImportSpec(GC,                  &
        LONG_NAME      = 'Organic Carbon Wet Deposition',&
        UNITS          = 'kg m-2 s-1',                   &
        SHORT_NAME     = 'OCWT',                         &
        DIMS           = MAPL_DimsHorzOnly,              &
        UNGRIDDED_DIMS = (/NUM_OCWT/),                   &
        VLOCATION      = MAPL_VLocationNone,       RC=STATUS)
      VERIFY_(STATUS)  
#endif

!oradbio imports
if (1==2) then
        call MAPL_AddImportSpec(GC,                   &
          SHORT_NAME = 'TAUA',                             &
          LONG_NAME  = 'aerosol optical thickness',         &
          UNITS      = '',                                  &
          DIMS       = MAPL_DimsHorzOnly,                   &
          UNGRIDDED_DIMS = (/nlt/),                         &
          VLOCATION  = MAPL_VLocationNone,            RC=STATUS)
        VERIFY_(STATUS)

        call MAPL_AddImportSpec(GC,                   &
          SHORT_NAME = 'SSALB',                             &
          LONG_NAME  = 'single scattering albedo',          &
          UNITS      = '',                                  &
          DIMS       = MAPL_DimsHorzOnly,                   &
          UNGRIDDED_DIMS = (/nlt/),                         &
          VLOCATION  = MAPL_VLocationNone,            RC=STATUS)
        VERIFY_(STATUS)

        call MAPL_AddImportSpec(GC,                   &
          SHORT_NAME = 'ASYMP',                             &
          LONG_NAME  = 'asymmetry parameter',               &
          UNITS      = '',                                  &
          DIMS       = MAPL_DimsHorzOnly,                   &
          UNGRIDDED_DIMS = (/nlt/),                         &
          VLOCATION  = MAPL_VLocationNone,            RC=STATUS)
        VERIFY_(STATUS)
endif

      do nl = 1,nlt
        write(unit = suffix, fmt = '(i2.2)') nl                  
        call MAPL_AddImportSpec(GC,                   &
          SHORT_NAME = 'TAUA_'//suffix,                     &
          LONG_NAME  = 'aerosol optical thickness',         &
          UNITS      = '',                                  &
          DIMS       = MAPL_DimsHorzOnly,                   &
          VLOCATION  = MAPL_VLocationNone,            RC=STATUS)
        VERIFY_(STATUS)
    
        call MAPL_AddImportSpec(GC,                   &
          SHORT_NAME = 'SSALB_'//suffix,                    &
          LONG_NAME  = 'single scattering albedo',          &
          UNITS      = '',                                  &
          DIMS       = MAPL_DimsHorzOnly,                   &
          VLOCATION  = MAPL_VLocationNone,            RC=STATUS)
        VERIFY_(STATUS)

        call MAPL_AddImportSpec(GC,                   &
          SHORT_NAME = 'ASYMP_'//suffix,                    &
          LONG_NAME  = 'asymmetry parameter',               &
          UNITS      = '',                                  &
          DIMS       = MAPL_DimsHorzOnly,                   &
          VLOCATION  = MAPL_VLocationNone,            RC=STATUS)
      VERIFY_(STATUS)
      enddo

      call MAPL_AddImportSpec(GC,                  &
        SHORT_NAME = 'OZ',                               &  
        LONG_NAME  = 'ozone thickness',                  &
        UNITS      = 'Dobson units',                     &
        DIMS       = MAPL_DimsHorzOnly,                  &
        VLOCATION  = MAPL_VLocationNone,           RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_AddImportSpec(GC,                  &
        SHORT_NAME = 'WV',                               &
        LONG_NAME  = 'water vapor',                      &
        UNITS      = 'cm',                               &
        DIMS       = MAPL_DimsHorzOnly,                  &
        VLOCATION  = MAPL_VLocationNone,           RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_AddImportSpec(GC,                  &
        SHORT_NAME = 'RH',                               &
        LONG_NAME  = 'relative humidity',                &
        UNITS      = 'percent',                          &
        DIMS       = MAPL_DimsHorzOnly,                  &
        VLOCATION  = MAPL_VLocationNone,           RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_AddImportSpec(GC,                  &
        SHORT_NAME = 'CCOVM',                            &
        LONG_NAME  = 'cloud cover',                      &
        UNITS      = 'percent',                          &
        DIMS       = MAPL_DimsHorzOnly,                  &
        VLOCATION  = MAPL_VLocationNone,           RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_AddImportSpec(GC,                  &
        SHORT_NAME = 'CLDTCM',                           &
        LONG_NAME  = 'cloud optical thickness',          &
        UNITS      = 'dimensionless',                    &
        DIMS       = MAPL_DimsHorzOnly,                  &
        VLOCATION  = MAPL_VLocationNone,           RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_AddImportSpec(GC,                  &
        SHORT_NAME = 'RLWPM',                            &
        LONG_NAME  = 'cloud liquid water path',          &
        UNITS      = 'dimensionless',                    &
        DIMS       = MAPL_DimsHorzOnly,                  &
        VLOCATION  = MAPL_VLocationNone,           RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_AddImportSpec(GC,                  &
        SHORT_NAME = 'CDREM',                            & 
        LONG_NAME  = 'cloud droplet effective radius',   &
        UNITS      = 'dimensionless',                    &
        DIMS       = MAPL_DimsHorzOnly,                  &
        VLOCATION  = MAPL_VLocationNone,           RC=STATUS)
      VERIFY_(STATUS)

    end subroutine OBIO_SetServices

  end subroutine SetServices

!BOP

! !IROUTINE: INITIALIZE -- Initialize stage for the DataAtm component

! !INTERFACE:

subroutine INITIALIZE ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
  type(ESMF_State),    intent(inout) :: IMPORT ! Import state
  type(ESMF_State),    intent(inout) :: EXPORT ! Export state
  type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
  integer, optional,   intent(  out) :: RC     ! Error code:

! !DESCRIPTION: 

!EOP

! ErrLog Variables

  character(len=ESMF_MAXSTR)          :: IAm
  integer                             :: STATUS
  character(len=ESMF_MAXSTR)          :: COMP_NAME

! Locals

  type (MAPL_MetaComp), pointer   :: MAPL => null()
  type (ESMF_GridComp), pointer   :: GCS(:) => null()
  type (MAPL_LocStream)           :: LOCSTREAM
  type (MAPL_LocStream)           :: EXCH
  real, pointer :: tmp(:,:) ! needed only to force allocation
  type (ESMF_State),         pointer  :: GIM(:), GEX(:)
  type (ESMF_Alarm) :: alarm, solarAlarm

  type (ESMF_State),    pointer :: SurfImport => null()
  type (ESMF_FieldBundle)       :: Bundle_DP
  type (ESMF_Field)             :: Field  

! OradBio local variables
  type (T_ORADBIO_STATE), pointer     :: State => null()
  type (ORADBIO_wrap)                 :: WRAP

!  Begin...
!----------

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    Iam = "Initialize"
    call ESMF_GridCompGet( GC, name=COMP_NAME, RC=STATUS)
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // Iam

! Get my internal MAPL_Generic state
!----------------------------------

    call MAPL_GetObjectFromGC(GC, MAPL, STATUS)
    VERIFY_(STATUS)

! Start Total timer
!------------------

    call MAPL_TimerOn (MAPL,"TOTAL")
    call MAPL_TimerOn (MAPL,"INITIALIZE"  )

!!! ALT this section below is not needed
!+=======================================+
! Change the location stream to just the ocean part
!--------------------------------------------------

    call MAPL_Get (MAPL, GCS=GCS, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_Get(MAPL, EXCHANGEGRID=EXCH, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_LocStreamCreate(LOCSTREAM, EXCH, NAME='OCEAN', &
                                       MASK=(/MAPL_OCEAN/), RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_ExchangeGridSet(GCS(SURF), LOCSTREAM, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_TimerOff(MAPL,"INITIALIZE"  )
    call MAPL_TimerOff(MAPL,"TOTAL")

    call MAPL_GenericInitialize ( GC, IMPORT, EXPORT, CLOCK,  RC=STATUS)
    VERIFY_(STATUS)

!ALT: At this point all the children (i.e. Surface) and grand-children 
! have been initialized
!
! we are now mimicking connections in Physics to force allocation
! for some Surface exports

    call MAPL_Get (MAPL, GIM=GIM, GEX=GEX, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(GEX(SURF), tmp, 'LWI'  ,  alloc=.true., RC=STATUS)
    VERIFY_(STATUS)

! also we need to fake the "Solar alarm"
    call MAPL_Get (MAPL, runAlarm=alarm, RC=STATUS)
    VERIFY_(STATUS)
    solarAlarm = ESMF_AlarmCreate(alarm, RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_AlarmSet(solarAlarm, name='SOLAR_Alarm', RC=STATUS)
    VERIFY_(STATUS)


    if (DO_OBIO/=0) then 

!Put OBIO dust imports into AERO_DP bundle
 !  !IMPORT STATE:
      SurfImport => GIM(SURF)   
      
      call ESMF_StateGet(SurfImport, 'AERO_DP', Bundle_DP,    RC=STATUS)
      VERIFY_(STATUS)
      do k=1, NUM_DUDP
        write(unit = suffix, fmt = '(i3.3)') k

        call ESMF_StateGet(IMPORT, 'DUDP'//suffix, Field,   RC=STATUS)
        VERIFY_(STATUS)
        call MAPL_FieldBundleAdd(Bundle_DP, Field, RC=STATUS)
        VERIFY_(STATUS)

        call ESMF_StateGet(IMPORT, 'DUWT'//suffix, Field,   RC=STATUS)
        VERIFY_(STATUS)
        call MAPL_FieldBundleAdd(Bundle_DP, Field, RC=STATUS)
        VERIFY_(STATUS)

        call ESMF_StateGet(IMPORT, 'DUSD'//suffix, Field,   RC=STATUS)
        VERIFY_(STATUS)
        call MAPL_FieldBundleAdd(Bundle_DP, Field, RC=STATUS)
        VERIFY_(STATUS)
      enddo


!If running OBIO prepare private state for OradBio
! Allocate this instance of the private state and put it in wrapper.
! -------------------------------------------------------------------

      allocate( State, __STAT__ )
      WRAP%PTR => State

! Save pointer to the wrapped private state in the GC
! ----------------------------------------------------

      call ESMF_UserCompSetInternalState ( GC, 'ORADBIO_state', WRAP, STATUS ); VERIFY_(STATUS)

! Initialize OASIM parameters in private state
!-----------------------------------------------

      call setsfclte(State%rad,State%pi2,State%lam,State%Fobar,         &
                     State%thray,State%oza,State%awv,State%ao,          &
                     State%aco2, State%am, State%Vi,                    &
                     State%asl,State%bsl,State%csl,State%dsl,State%esl, &
                     State%fsl,State%ica)
    endif

    RETURN_(ESMF_SUCCESS)

  end subroutine INITIALIZE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOP

! ! IROUTINE: RUN -- Run stage for the DataAtm component

! !INTERFACE:

subroutine RUN ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
  type(ESMF_State),    intent(inout) :: IMPORT ! Import state
  type(ESMF_State),    intent(inout) :: EXPORT ! Export state
  type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
  integer, optional,   intent(  out) :: RC     ! Error code:

! ! DESCRIPTION: Periodically refreshes ...??

!EOP

! ErrLog Variables

  character(len=ESMF_MAXSTR)          :: IAm
  integer                             :: STATUS
  character(len=ESMF_MAXSTR)          :: COMP_NAME

! Locals

  type (MAPL_MetaComp),       pointer :: MAPL       => null()
  type (ESMF_GridComp),       pointer :: GCS(:)     => null()
  type (ESMF_State),          pointer :: GIM(:)     => null()
  type (ESMF_State),          pointer :: GEX(:)     => null()
  character(len=ESMF_MAXSTR), pointer :: GCNAMES(:) => null()
  type (ESMF_State),          pointer :: SurfImport => null()
  type (ESMF_State),          pointer :: SurfExport => null()
  type (ESMF_Time)                    :: currentTime

  integer                                   :: IM, JM
  real, dimension(:,:), allocatable         :: Uskin, Vskin, Qskin
  real, dimension(:,:), allocatable, target :: swrad
  real, dimension(:,:), pointer             :: PS, PSsurf, Tair, Qair, Uair, Vair, DZ
  real, dimension(:,:), pointer             :: ALW, BLW, SPEED, DISCHARGE, rPCU, rPLS, sSNO
  real, dimension(:,:), pointer             :: CT, CQ, CM, SH, EVAP, TAUX, TAUY, Tskin, lwdnsrf
  real, dimension(:,:), pointer             :: DRPARN, DFPARN, DRNIRN, DFNIRN, DRUVRN, DFUVRN
  real, dimension(:,:), pointer             :: EMISSRF

  real, allocatable, dimension(:,:) :: ZTH
  real, allocatable, dimension(:,:) :: SLR
  real,    pointer, dimension(:,:)    :: LATS     => NULL()
  real,    pointer, dimension(:,:)    :: LONS     => NULL()
  real                                :: SC, MG, SB
  logical                             :: USE_NRLSSI2
  character(len=ESMF_MAXPATHLEN) :: SolCycFileName
  type (MAPL_SunOrbit)                :: ORBIT
  type (ESMF_TimeInterval)            :: DELT

! Andrea: do we need these????
  real, parameter :: FRUVR          = 0.07
  real, parameter :: FRPAR          = 0.40
  real, parameter :: FRNIR          = 0.53

  ! partitioning of shortwave radiation for ice 
  real, parameter :: FRVISDIR       = 0.29
  real, parameter :: FRVISDIF       = 0.31
  real, parameter :: FRNIRDIR       = 0.24
  real, parameter :: FRNIRDIF       = 0.16

  real, parameter ::  KUVR          = 0.09

  real, parameter :: alb            = 0.066


! pointers to import - none
! internal pointers to tile variables ???

  real, pointer, dimension(:,:) :: TA       ! => null()
  real, pointer, dimension(:,:) :: QA       ! => null()
  real, pointer, dimension(:,:) :: UA       ! => null()
  real, pointer, dimension(:,:) :: VA       ! => null()
  real, pointer, dimension(:,:) :: RUNOFF   ! => null()
  real, pointer, dimension(:,:) :: PCU      ! => null()
  real, pointer, dimension(:,:) :: PLS      ! => null()
  real, pointer, dimension(:,:) :: SNO      ! => null()
  real, pointer, dimension(:,:) :: LWDN ! => null()
  real, pointer, dimension(:,:) :: SWGDWN   ! => null()

! pointers to export - none???

! pointers to internal
  real, pointer, dimension(:,:) :: TS
  real, pointer, dimension(:,:) :: EMIS
  type(ESMF_STATE) :: internal

! Andrea: OBSERVE????
!  logical, dimension(1) :: OBSERVE
!  real LATSO, LONSO

   real, parameter :: HW_hack = 2.
   logical :: firsttime = .true.

   real :: TAU_TS
   real :: DT

   integer :: year, month, day, hr, mn, se

!  Begin...
!----------

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    Iam = "Run"
    call ESMF_GridCompGet( GC, name=COMP_NAME, RC=STATUS)
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // Iam

! Get my MAPL_Generic (GG) state
!-------------------------------

    call MAPL_GetObjectFromGC(GC, MAPL, status)
    VERIFY_(status)

! Start timers
!-------------

    call MAPL_TimerOn(MAPL,"TOTAL")
    call MAPL_TimerOn(MAPL,"RUN" )

! Get the time step
! -----------------

! Get current time from clock
!----------------------------
    call ESMF_ClockGet(CLOCK, currTime=CurrentTime, RC=STATUS)
    VERIFY_(STATUS)

    call esmf_timeget(CurrentTime, yy=year, mm=month, dd=day, h=hr, m=mn, s=se, rc = status);

    call MAPL_Get (MAPL, GCS=GCS, GIM=GIM, GEX=GEX, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_Get (MAPL, INTERNAL_ESMF_STATE = INTERNAL, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_Get(MAPL, HEARTBEAT = DT, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, DT, Label="DT:", DEFAULT=DT, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, TAU_TS, Label="TAU_TS:", DEFAULT=7200.0, RC=STATUS)
    VERIFY_(STATUS)

! Pointers to Imports
!--------------------

! Pointers to Internals
!----------------------
    call MAPL_GetPointer(Internal, Tskin, 'TS', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(Internal, EMIS, 'EMIS', RC=STATUS)
    VERIFY_(STATUS)

!  Pointers to Exports ????
!---------------------

!new stuff: this is what Surface needs
!======================================

! Get children and their im/ex states from my generic state.
!----------------------------------------------------------
    call MAPL_Get ( MAPL, GCS=GCS, GIM=GIM, GEX=GEX, GCNAMES=GCNAMES, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_Get(MAPL, IM=IM, JM=JM, LATS=LATS, LONS=LONS, ORBIT=ORBIT,RC=STATUS)
    VERIFY_(STATUS)

    allocate( ZTH  (IM,JM), STAT=STATUS); VERIFY_(STATUS)
    allocate( SLR  (IM,JM), STAT=STATUS); VERIFY_(STATUS)

!  !IMPORT STATE:
    SurfImport => GIM(SURF)
    SurfExport => GEX(SURF)

! Read Sea Level Pressure (Pa)
!---------------------------------------------------
    !ALT is this default too low?
!    call ReadForcingData(impName='PS', frcName='SLP', default=90000., RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(import, PS, 'PS', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(SurfImport, PSsurf, 'PS', RC=STATUS)
    VERIFY_(STATUS)
    where(ps==0.0) ps=100000.
    PSsurf = PS 

! Read 10m temperature (K)
!---------------------------------------------------
!   call ReadForcingData(impName='TA', frcName='T10', default=290., RC=STATUS)
!    VERIFY_(STATUS) 
! how about default T10 = 270+30*COS(LAT)
    call MAPL_GetPointer(SurfImport, Tair, 'TA', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(import, TA, 'TA', RC=STATUS)
    VERIFY_(STATUS)
    where(ta==0.0) ta=MAPL_Tice
    Tair = TA

! Read 10m specific humidity (kg kg-1)
!---------------------------------------------------
!   call ReadForcingData(impName='QA', frcName='Q10', default=2.0e-6, RC=STATUS)
!   VERIFY_(STATUS)
    call MAPL_GetPointer(SurfImport, Qair, 'QA', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(import, QA, 'QA', RC=STATUS)
    VERIFY_(STATUS)
!@@    Qair = QA * 0.001 ! SA: convert g/Kg to Kg/Kg
    Qair = QA

! Read 10m zonal wind speed (m s-1)
!---------------------------------------------------
!   call ReadForcingData(impName='UA', frcName='U10', default=0., RC=STATUS)
!   VERIFY_(STATUS)
    call MAPL_GetPointer(SurfImport, Uair, 'UA', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(import, UA, 'UA', RC=STATUS)
    VERIFY_(STATUS)
    Uair = UA
    Uair = merge(tsource = uair, fsource = 0.0, mask = (abs(uair) < 1000.0)); 

! Read 10m meridional wind speed (m s-1)
!---------------------------------------------------
!   call ReadForcingData(impName='VA', frcName='V10', default=0., RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(SurfImport, Vair, 'VA', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(import, VA, 'VA', RC=STATUS)
    VERIFY_(STATUS)
    Vair = VA
    Vair = merge(tsource = vair, fsource = 0.0, mask = (abs(vair) < 1000.0)); 

    call MAPL_GetPointer(SurfImport, SPEED, 'SPEED', RC=STATUS)
    VERIFY_(STATUS)
    SPEED = SQRT(Uair**2 + Vair**2)

    IM = size(Uair, 1)
    JM = size(Uair, 1)
    allocate(Uskin(IM,JM), Vskin(IM,JM), Qskin(IM,JM), swrad(IM,JM), STAT=STATUS); VERIFY_(STATUS)

    call MAPL_GetPointer(SurfImport, DZ, 'DZ', RC=STATUS)
    VERIFY_(STATUS)
    DZ = 50.0 ! meters

! River runoff    
!   call ReadForcingData(impName='DISCHARGE', frcName='RR', default=0., RC=STATUS)
!   VERIFY_(STATUS)
    call MAPL_GetPointer(SurfImport, DISCHARGE, 'DISCHARGE', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(import, RUNOFF, 'RUNOFF', RC=STATUS)
    VERIFY_(STATUS)
    DISCHARGE=RUNOFF

    !ALT: we should read topo, but for now over ocean this is fine
    call SetVarToZero('PHIS', RC=STATUS)
    VERIFY_(STATUS)

! these are not used, 0 should be OK
    call SetVarToZero('DEWL', RC=STATUS)
    VERIFY_(STATUS)
    call SetVarToZero('FRSL', RC=STATUS)
    VERIFY_(STATUS)

! these should be set to 0 (for now)
    call SetVarToZero('DSH', RC=STATUS)
    VERIFY_(STATUS)
    call SetVarToZero('DFU', RC=STATUS)
    VERIFY_(STATUS)
    call SetVarToZero('DFV', RC=STATUS)
    VERIFY_(STATUS)
    call SetVarToZero('DEVAP', RC=STATUS)
    VERIFY_(STATUS)
    call SetVarToZero('DDEWL', RC=STATUS)
    VERIFY_(STATUS)
    call SetVarToZero('DFRSL', RC=STATUS)
    VERIFY_(STATUS)

! Precipitation
! question for Andrea: the old code reads RAIN. How to partition into
! PCU and PLS? Right now all of RAIN goes into PCU (arbitralily chosen)
! SA: above can be squashed, we can read PCU and PLS and add to get RAIN.
!   call ReadForcingData(impName='PCU', frcName='RAIN', default=0., RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(SurfImport, rPCU, 'PCU', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(import, PCU,    'PCU',    RC=STATUS)
    VERIFY_(STATUS)
    rPCU=PCU

    call MAPL_GetPointer(SurfImport, rPLS, 'PLS', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(import, PLS,    'PLS',    RC=STATUS)
    VERIFY_(STATUS)
    rPLS=PLS

!   RAIN = PCU + PLS
!   call SetVarToZero('PLS', RC=STATUS)
!   VERIFY_(STATUS)

!   call ReadForcingData(impName='SNO', frcName='SNO', default=0., RC=STATUS)
!   VERIFY_(STATUS)
    call MAPL_GetPointer(SurfImport, sSNO, 'SNO', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(import, SNO,    'SNO',    RC=STATUS)
    VERIFY_(STATUS)
    sSNO=SNO

! Radiation
!   call ReadForcingData(impName='LWDNSRF', frcName='LWRAD', default=100.0, RC=STATUS)
!    VERIFY_(STATUS)
    call MAPL_GetPointer(import, LWDN, 'LWDN', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(SurfImport, lwdnsrf, 'LWDNSRF', RC=STATUS)
    VERIFY_(STATUS)

!   call ReadForcingData(swrad, frcName='SWRAD', default=0.200, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(import, SWGDWN, 'SWGDWN', RC=STATUS)
    VERIFY_(STATUS)
    swrad = SWGDWN

    ! get sw at the TOA (next few lines are copy-and-paste from Surface)

! Get the insolation and zenith angle on grid and tiles
!------------------------------------------------------

    call ESMF_ClockGet(CLOCK,     TIMESTEP=DELT, RC=STATUS)
    VERIFY_(STATUS)
    DELT = DELT * NINT((86400./DT)) ! emulate daily Solar

    call MAPL_SunGetInsolation(LONS, LATS,      &
              ORBIT, ZTH, SLR, &
              INTV  = DELT,    &
              CLOCK = CLOCK,   &
              RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_GetResource( MAPL, SC, 'SOLAR_CONSTANT:', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource( MAPL, SolCycFileName, "SOLAR_CYCLE_FILE_NAME:", DEFAULT='/dev/null', RC=STATUS)
    VERIFY_(STATUS)

    if(SolCycFileName /= '/dev/null') THEN

       call MAPL_GetResource( MAPL, USE_NRLSSI2, "USE_NRLSSI2:", DEFAULT=.TRUE., RC=STATUS)
       VERIFY_(STATUS)

       if (USE_NRLSSI2) then
          call MAPL_SunGetSolarConstant(CLOCK,trim(SolCycFileName),SC,MG,SB,RC=STATUS)
          VERIFY_(STATUS)
       else
          call MAPL_SunGetSolarConstant(CLOCK,trim(SolCycFileName),SC,RC=STATUS)
          VERIFY_(STATUS)
       endif
    else if(SC<0.0) then
       call MAPL_SunGetSolarConstant(CURRENTTIME,SC,RC=STATUS)
       VERIFY_(STATUS)
    end if

!    where (zth <= 0.0 .or. slr == 0.0)
    where (zth <= 1.0e-6)
       swrad = 0.0
    elsewhere
       swrad = swrad / (sc*slr) 
    end where

    call MAPL_GetPointer(SurfImport, DRPARN, 'DRPARN', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(SurfImport, DFPARN, 'DFPARN', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(SurfImport, DRNIRN, 'DRNIRN', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(SurfImport, DFNIRN, 'DFNIRN', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(SurfImport, DRUVRN, 'DRUVRN', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(SurfImport, DFUVRN, 'DFUVRN', RC=STATUS)
    VERIFY_(STATUS)

! Andrea: Is partitioning OK here? Or something else?
! Direct vs duffuse, zenith angle, time of the day???
    DRPARN = swrad*FRPAR*0.6
    DFPARN = swrad*FRPAR*0.4
    DRUVRN = swrad*FRUVR*0.6
    DFUVRN = swrad*FRUVR*0.4
    DRNIRN = swrad*FRNIR*0.6
    DFNIRN = swrad*FRNIR*0.4

!@@   print *,'DEBUG:min/max DRPARN',minval(DRPARN),maxval(DRPARN)

! Andrea: Tskin = TS? From previous time step?
! Andrea: do we need radiation vars computed before we call phase 1
!@    call MAPL_GetPointer(SurfExport, Tskin, 'TS_FOUND', RC=STATUS)
!    VERIFY_(STATUS)
!@    call MAPL_GetPointer(SurfExport, Tskin, 'TS', RC=STATUS)
!     VERIFY_(STATUS)

    call MAPL_GetPointer(SurfImport, ALW, 'ALW', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(SurfImport, BLW, 'BLW', RC=STATUS)
    VERIFY_(STATUS)

    if (firsttime) then
       firsttime = .false.
       Tskin = TA
    end if

   BLW = EMIS*4*MAPL_STFBOL * Tskin**3
   ALW = EMIS*MAPL_STFBOL * Tskin**4 - BLW*Tskin

   call MAPL_GetPointer(SurfExport, EMISSRF, 'EMIS', alloc=.true., RC=STATUS)
   VERIFY_(STATUS)

   lwdnsrf = lwdn
!   lwdnsrf = 0.0
 !  where(LWGNTWTR /=  MAPL_Undef)
 !     lwdnsrf = -(LWGNTWTR - MAPL_STFBOL * Tskin**4)
 !  end where

!    call SetVarToZero('BLW', RC=STATUS)
    VERIFY_(STATUS)
    
    call SetVarToZero('DTSDT', RC=STATUS)
    VERIFY_(STATUS)

!!!    if (mapl_am_i_root()) PRINT*, __FILE__, __LINE__
! call Run (or, phase) 1 of Surface
    call ESMF_GridCompRun (GCS(SURF), importState=GIM(SURF), &
         exportState=GEX(SURF), clock=CLOCK, PHASE=1, userRC=status )
    VERIFY_(status)

! we deal with these after Run1 of surf
    call MAPL_GetPointer(SurfImport, Tair, 'TA', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(SurfImport, Qair, 'QA', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(SurfImport, Uair, 'UA', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(SurfImport, Vair, 'VA', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(SurfImport, PS,   'PS', RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_GetPointer(SurfExport, CT, 'CT', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(SurfExport, CQ, 'CQ', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(SurfExport, CM, 'CM', RC=STATUS)
    VERIFY_(STATUS)

! Andrea:    -- Tskin can be the SST for this purpose – T foundation   - i think the export version is called HFLX

! Andrea: are you sure about this??? Or is GEOS_QsatLQU(Tskin, PS)

    Qskin = GEOS_Qsat(Tskin, PS, RAMP=0.0, PASCALS=.TRUE.)

    ! here we assume that the wind at the skin is zero
    Uskin = 0.0
    Vskin = 0.0

    call MAPL_GetPointer(SurfImport, SH, 'SH', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(SurfImport, EVAP, 'EVAP', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(SurfImport, TAUX, 'TAUX', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(SurfImport, TAUY, 'TAUY', RC=STATUS)
    VERIFY_(STATUS)

    SH = CT * MAPL_CP * (Tskin - Tair)
    EVAP = CQ * (Qskin - Qair) 
    TAUX = CM * (Uskin - Uair)
    TAUY = CM * (Vskin - Vair)

101 format (A, e20.12, 3I3.2)

!  do anything that relates to OBIO via call to obio_extra
!  ONLY if the user wants to run with OBIO, default is not to run it; see ABOVE SetServices

   if (DO_OBIO/=0) call obio_extra(RC)
   VERIFY_(STATUS)
! ------------------------------------------------

!!!    if (mapl_am_i_root()) PRINT*, __FILE__, __LINE__
! call Run (or, phase) 2 of Surface
    call ESMF_GridCompRun (GCS(SURF), importState=GIM(SURF), &
         exportState=GEX(SURF), clock=CLOCK, PHASE=2, userRC=status )
    VERIFY_(status)

! by now Saltwater should be able to provide everything the ocean needs
    call MAPL_GetPointer(SurfExport, TS, 'TS', RC=STATUS)
    VERIFY_(STATUS)

   WHERE (TS /= MAPL_Undef)   Tskin = Tskin + (TS - Tskin) * DT / (DT + TAU_TS)
   WHERE (EMISSRF /= MAPL_Undef) EMIS = EMISSRF

!-- still left to do
! modify GCM to always get the skin from Saltwater
! modify Surface so we can pass discharge, salinity, etc

! === CHECK_EMAIL 12/22/20

! Andrea: sea surface saloinity???,  river runoff???
! Read sea surface salinity (psu),
!---------------------------------------------------

    deallocate(Uskin, Vskin, Qskin, swrad, STAT=STATUS); VERIFY_(STATUS)

!  All done
!-----------

    call MAPL_TimerOff(MAPL,"RUN"  )
    call MAPL_TimerOff(MAPL,"TOTAL")

    RETURN_(ESMF_SUCCESS)

contains
  subroutine ReadForcingData(dataptr, impName, frcName, default, rc)
    ! note that the variables MAPL, SurfImport and currentTime are "borrowed"
    ! from the parent
    real, pointer,    optional, intent(IN) :: dataptr(:,:)
    character(len=*), optional, intent(IN) :: impName
    character(len=*), optional, intent(IN) :: frcName
    real, intent(IN) :: default
    integer, optional :: rc

    ! local vars
    integer :: status
    real, pointer :: ptr(:,:) => null()
    character(len=ESMF_MAXSTR) :: datafile, frcName_, label_

    if (present(dataptr)) then
       if (mapl_am_i_root()) PRINT*, trim(impName)
       ASSERT_(.not.present(impName))
       ptr => dataPtr
    else
       call MAPL_GetPointer(SurfImport, ptr, impName, RC=STATUS)
       VERIFY_(STATUS)
    end if
    if (present(frcName)) then
       frcName_ = frcName
    else
       ASSERT_(present(impName))
       frcName_ = impName
    end if
    label_ = trim(frcName_)//'_FILE:'

    call MAPL_GetResource(MAPL, datafile, label=label_, default='none', RC=STATUS)
    VERIFY_(STATUS)
    if(trim(datafile) == 'none') then
       ptr = default
    else
       call MAPL_ReadForcing(MAPL, frcName_, renamefile(datafile, time = currenttime), currenttime, ptr, RC=STATUS)
       VERIFY_(STATUS)
    endif 

    RETURN_(ESMF_SUCCESS)
  end subroutine ReadForcingData

  subroutine SetVarToZero(impName, rc)
    ! note that the variable SurfImport is "borrowed" from the parent
    character(len=*), intent(IN) :: impName
    integer, optional :: rc

    ! local vars
    integer :: status
    real, pointer :: ptr(:,:) => null()

    call MAPL_GetPointer(SurfImport, ptr, impName, RC=STATUS)
    VERIFY_(STATUS)
    ptr = 0.0

    RETURN_(ESMF_SUCCESS)
  end subroutine SetVarToZero

  subroutine obio_extra(rc)

! Locals
    integer, intent(out), optional :: rc
    integer                        :: STATUS
    type (T_ORADBIO_STATE), pointer :: State => null()
    type (ORADBIO_wrap)             :: WRAP

    character(len=ESMF_MAXSTR)   :: skinname
    character(len=ESMF_MAXSTR)   :: DATAFILE
    integer :: i, j, DOY

    real    :: hr,rday,daycor
    real    :: slp,wspd,ozone,wvapor,relhum
    real    :: cov,cldtau,clwp,cldre
    real, dimension(nlt) :: ta,wa,asym
    real, pointer, dimension(:,:,:) :: Ed,Es
  
!Pointers to Imports

    type (ESMF_FieldBundle)       :: Bundle_DP
    type (ESMF_Field)             :: Field
    real, pointer, dimension(:,:) :: DUDP 
    real, pointer, dimension(:,:) :: DUWT 
    real, pointer, dimension(:,:) :: DUSD 
    real, pointer, dimension(:,:,:) :: ADUDP
    real, pointer, dimension(:,:,:) :: ADUWT
    real, pointer, dimension(:,:,:) :: ADUSD
!    type(dustptr), dimension(NUM_DUDP)   :: ADUDP
!    type(dustptr), dimension(NUM_DUWT)   :: ADUWT
!    type(dustptr), dimension(NUM_DUSD)   :: ADUSD
    real, pointer, dimension(:,:)   :: CO2SC

    real, pointer, dimension(:,:,:) :: DRY_DUST 
    real, pointer, dimension(:,:,:) :: WET_DUST 
    real, pointer, dimension(:,:,:) :: SED_DUST 
    real, pointer, dimension(:,:)   :: ATMCO2   

    real, pointer, dimension(:,:)   :: TAUA  
    real, pointer, dimension(:,:)   :: ASYMP 
    real, pointer, dimension(:,:)   :: SSALB 
    type(bandptr), dimension(nlt)   :: ATAUA
    type(bandptr), dimension(nlt)   :: AASYMP
    type(bandptr), dimension(nlt)   :: ASSALB
    real, pointer, dimension(:,:)   :: OZ   
    real, pointer, dimension(:,:)   :: WV   
    real, pointer, dimension(:,:)   :: RH   
    real, pointer, dimension(:,:)   :: CCOV 
    real, pointer, dimension(:,:)   :: CLDTC
    real, pointer, dimension(:,:)   :: RLWP 
    real, pointer, dimension(:,:)   :: CDRE  
    real, pointer, dimension(:,:,:) :: DROBIO 
    real, pointer, dimension(:,:,:) :: DFOBIO
    real, pointer, dimension(:,:,:) :: DRRAD => null()
    real, pointer, dimension(:,:,:) :: DFRAD => null()
    real, pointer, dimension(:,:,:) :: TAUAex => null()

!    print*, 'in obio_extra'

    allocate( ADUDP(IM,JM,NUM_DUDP), STAT=STATUS); VERIFY_(STATUS)
    allocate( ADUWT(IM,JM,NUM_DUWT), STAT=STATUS); VERIFY_(STATUS)
    allocate( ADUSD(IM,JM,NUM_DUSD), STAT=STATUS); VERIFY_(STATUS)

    call MAPL_GetPointer(IMPORT, CO2SC, 'CO2SC',   RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(SurfImport, ATMCO2, 'CO2SC',   RC=STATUS)
    VERIFY_(STATUS)
    ATMCO2 = CO2SC

!    call MAPL_GetPointer(IMPORT, TAUA,  'TAUA',   RC=STATUS)
!    VERIFY_(STATUS)
!    call MAPL_GetPointer(IMPORT, ASYMP, 'ASYMP',  RC=STATUS)
!    VERIFY_(STATUS)
!    call MAPL_GetPointer(IMPORT, SSALB, 'SSALB',  RC=STATUS)
!    VERIFY_(STATUS)

    do k=1, nlt
       write(unit = suffix, fmt = '(i2.2)') k
       call MAPL_GetPointer(IMPORT, TAUA,  'TAUA_'//suffix,   RC=STATUS)
       VERIFY_(STATUS)
       ATAUA(k)%b => TAUA
       call MAPL_GetPointer(IMPORT, ASYMP, 'ASYMP_'//suffix,  RC=STATUS)
       VERIFY_(STATUS)
       AASYMP(k)%b => ASYMP
       call MAPL_GetPointer(IMPORT, SSALB, 'SSALB_'//suffix,  RC=STATUS)
       VERIFY_(STATUS)
       ASSALB(k)%b => SSALB
    enddo

    call MAPL_GetPointer(IMPORT, OZ,    'OZ',     RC=STATUS)
    VERIFY_(STATUS) 
    call MAPL_GetPointer(IMPORT, WV,    'WV',     RC=STATUS)
    VERIFY_(STATUS) 
    call MAPL_GetPointer(IMPORT, RH,    'RH',     RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, CCOV,  'CCOVM',  RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, CLDTC, 'CLDTCM', RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, RLWP,  'RLWPM',  RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT, CDRE,  'CDREM',  RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(SurfImport, DROBIO, 'DROBIO',   RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetPointer(SurfImport, DFOBIO, 'DFOBIO',   RC=STATUS)
    VERIFY_(STATUS)
!    call MAPL_GetPointer(EXPORT, TAUAex,  'TAUA',   RC=STATUS)
!    VERIFY_(STATUS)

!    if(associated(TAUAex)) TAUAex = TAUA

! Get my internal private state
!------------------------------

    call ESMF_UserCompGetInternalState(GC, 'ORADBIO_state', WRAP, STATUS)
    VERIFY_(STATUS)
    State => WRAP%PTR

! Obtain Earth-Sun distance 
    hr=1.0
    rday = float(DOY) + hr/24.0
    daycor = (1.0+1.67E-2*cos(State%pi2*(rday-3.0)/365.0))**2

    allocate( Ed  (IM,JM,nlt), STAT=STATUS); VERIFY_(STATUS)
    allocate( Es  (IM,JM,nlt), STAT=STATUS); VERIFY_(STATUS)

    do j = 1, JM
      do i = 1, IM
        if (ZTH(i,j) > 0.0)then
          slp = PS(i,j)*0.01  ! convert from Pa to mbar
          wspd = SPEED(i,j)
          ozone = OZ(i,j)
          wvapor = WV(i,j)
          relhum = RH(i,j)
!          ta(:) = TAUA(i,j,:)
!          wa(:) = SSALB(i,j,:)
!          asym(:) = ASYMP(i,j,:)
          do nl = 1,nlt
            ta(nl) = ATAUA(nl)%b(i,j)
            wa(nl) = ASSALB(nl)%b(i,j)
            asym(nl) = AASYMP(nl)%b(i,j)
          enddo
          cov = CCOV(i,j)
          cldtau = CLDTC(i,j)
          clwp = RLWP(i,j)
          cldre = CDRE(i,j)       

!  There are mismatches between the ocean, land and atmosphere in GEOS-5

!  to have ocean underneath with land overhead.  Set irradiance to 0
!  to represent this condition.
!       if (slp < 0.0 .or. slp >1.0E10)then ! original line, EMS
          if ((slp < 0.0 .or. slp >1.0E10) .or. (wspd == MAPL_UNDEF) .or. (ozone == MAPL_UNDEF) &
              .or. (wvapor == MAPL_UNDEF) .or. (relhum == MAPL_UNDEF)) then ! extended to include additional variables, EMS
            Ed(i,j,:) = 0.0
            Es(i,j,:) = 0.0
          else
!   Spectral irradiance just above surface
          call sfcirr(State%lam,State%Fobar,State%thray,State%oza,       &
                      State%awv,State%ao,State%aco2,                     &
                      State%asl,State%bsl,State%csl,State%dsl,           &
                      State%esl,State%fsl,State%ica,                     &
                      daycor,ZTH(i,j),                                  &
                      slp,wspd,ozone,wvapor,relhum,                      &
                      ta,wa,asym,State%am,State%Vi,                      &
                      cov,cldtau,clwp,cldre,Ed(i,j,:),Es(i,j,:))
          endif 
        endif
      enddo
    enddo

!    if(associated(DRRAD)) DRRAD= Ed
!    if(associated(DFRAD)) DFRAD= Es

    DROBIO = Ed
    DFOBIO = Es

     
!     print*, 'DROBIO min max ', minval(DROBIO), maxval(DROBIO)
!     print*, 'DFOBIO min max ', minval(DFOBIO), maxval(DFOBIO)

#ifdef LEFTOVER_FROM_OLD_CODE

!   Read Clay-Sized Dry Atmospheric Dust Depositions
!-------------------------------------------------
    do K = 1, NUM_DUDP
       write(label,'(I3.3)') K
       call MAPL_GetResource( MAPL, DATAfile, LABEL='DUDP'//label//'_FILE:', default = 'none', RC=STATUS)
       VERIFY_(STATUS)
       if(trim(datafile) == 'none') then; dry_clay = 0.0
       else
          call MAPl_ReadForcing( MAPL, 'DUDP'//label, DATAFILE, CURRENTTIME, dry_clay, RC=STATUS)
          VERIFY_(STATUS)
       endif
       if (associated(dry_clayx)) dry_clayx(:,K) = dry_clay
    end do

!   Read Clay-Sized Wet Atmospheric Dust Depositions
!-------------------------------------------------
    do K = 1, NUM_DUWT
       write(label,'(I3.3)') K
       call MAPL_GetResource( MAPL, DATAfile, LABEL='DUWT'//label//'_FILE:', default = 'none', RC=STATUS)
       VERIFY_(STATUS)
       if(trim(datafile) == 'none') then; wet_clay = 0.0
       else
          call MAPl_ReadForcing( MAPL, 'DUWT'//label, DATAFILE, CURRENTTIME, wet_clay, RC=STATUS)
          VERIFY_(STATUS)
       endif
       if (associated(wet_clayx)) wet_clayx(:,K) = wet_clay
    end do

!   Read Clay-Sized Sedimentary Atmospheric Dust Depositions
!---------------------------------------------------------
    do K = 1, NUM_DUSD
       write(label,'(I3.3)') K
       call MAPL_GetResource( MAPL, DATAfile, LABEL='DUSD'//label//'_FILE:', default = 'none', RC=STATUS)
       VERIFY_(STATUS)
       if(trim(datafile) == 'none') then; sed_clay = 0.0
       else
          call MAPl_ReadForcing( MAPL, 'DUSD'//label, DATAFILE, CURRENTTIME, sed_clay, RC=STATUS)
          VERIFY_(STATUS)
       endif
       if (associated(sed_clayx)) sed_clayx(:,K) = sed_clay
    end do

!   Read Atmospheric Clouds (Atmospheric Optics)
!---------------------------------------------
    call MAPL_GetResource( MAPL, DATAfile, LABEL='CCOVM_FILE:', default = 'none', RC=STATUS)
    VERIFY_(STATUS)
    if(trim(datafile) == 'none') then; ccovm = 0.0; 
    else; call MAPl_ReadForcing( MAPL, 'CCOVM', DATAFILE, CURRENTTIME, ccovm, RC=STATUS)
     VERIFY_(STATUS)
    endif; 

    call MAPL_GetResource( MAPL, DATAfile, LABEL='CLDTCM_FILE:', default = 'none', RC=STATUS)
    VERIFY_(STATUS)
    if(trim(datafile) == 'none') then; cldtcm = 0.0; 
    else; call MAPl_ReadForcing( MAPL, 'CLDTCM', DATAFILE, CURRENTTIME, cldtcm, RC=STATUS)
     VERIFY_(STATUS)
    endif; 

    call MAPL_GetResource( MAPL, DATAfile, LABEL='RLWPM_FILE:', default = 'none', RC=STATUS)
    VERIFY_(STATUS)
    if(trim(datafile) == 'none') then; rlwpm = 0.0; 
    else; call MAPl_ReadForcing( MAPL, 'RLWPM', DATAFILE, CURRENTTIME, rlwpm, RC=STATUS)
     VERIFY_(STATUS)
    endif; 

    call MAPL_GetResource( MAPL, DATAfile, LABEL='CDREM_FILE:', default = 'none', RC=STATUS)
    VERIFY_(STATUS)
    if(trim(datafile) == 'none') then; cdrem = 0.0; 
    else; call MAPl_ReadForcing( MAPL, 'CDREM', DATAFILE, CURRENTTIME, cdrem, RC=STATUS)
     VERIFY_(STATUS)
    endif; 

!   Read Atmospheric Properties (Atmospheric Optics)
!-------------------------------------------------
    call MAPL_GetResource( MAPL, DATAfile, LABEL='RH_FILE:', default = 'none', RC=STATUS)
    VERIFY_(STATUS)
    if(trim(datafile) == 'none') then; rh = 0.0; 
    else; call MAPl_ReadForcing( MAPL, 'RH', DATAFILE, CURRENTTIME, rh, RC=STATUS)
     VERIFY_(STATUS)
    endif; 

    call MAPL_GetResource( MAPL, DATAfile, LABEL='OZ_FILE:', default = 'none', RC=STATUS)
    VERIFY_(STATUS)
    if(trim(datafile) == 'none') then; oz = 0.0; 
    else; call MAPl_ReadForcing( MAPL, 'OZ', DATAFILE, CURRENTTIME, oz, RC=STATUS)
     VERIFY_(STATUS)
    endif; 

    call MAPL_GetResource( MAPL, DATAfile, LABEL='WV_FILE:', default = 'none', RC=STATUS)
    VERIFY_(STATUS)
    if(trim(datafile) == 'none') then; wv = 0.0; 
    else; call MAPl_ReadForcing( MAPL, 'WV', DATAFILE, CURRENTTIME, wv, RC=STATUS)
     VERIFY_(STATUS)
    endif; 

!   Read Atmospheric Carbon Dioxide from Carbon Tracker (_2011_OI)
!-----------------------------------------------------
    call MAPL_GetResource( MAPL, DATAfile, LABEL='CO2SC_FILE:', default = 'none', RC=STATUS)
    VERIFY_(STATUS)
    if(trim(datafile) == 'none') then; co2sc = 0.0; 
    else; call MAPl_ReadForcing( MAPL, 'CO2SC', DATAFILE, CURRENTTIME, co2sc,  RC=STATUS)
     VERIFY_(STATUS)
    endif;
    if ( associated(co2scx) ) co2scx = co2sc


!   Read MODIS Aerosols (Atmospheric Optics)
!-----------------------------------------

    do k=1, 33
     write(unit = suffix, fmt = '(i2.2)') k
     call MAPL_GetResource( MAPL, DATAfile, LABEL='TAUA_FILE:', default = 'none', RC=STATUS)
     VERIFY_(STATUS)
     if(trim(datafile) == 'none') then; taua = 0.0
     else; call MAPL_ReadForcing( MAPL, 'TAUA_' // suffix, trim(DATAFILE) // suffix, CURRENTTIME, taua, RC=STATUS)
      VERIFY_(STATUS)
     endif;
     ataua(k)%b => taua

     call MAPL_GetResource( MAPL, DATAfile, LABEL='ASYMP_FILE:', default = 'none', RC=STATUS)
    VERIFY_(STATUS)
     if(trim(datafile) == 'none') then; asymp = 0.0
     else; call MAPL_ReadForcing( MAPL, 'ASYMP_' // suffix, trim(DATAFILE) // suffix, CURRENTTIME, asymp, RC=STATUS)
      VERIFY_(STATUS)
     endif;  
     aasymp(k)%b => asymp

     call MAPL_GetResource( MAPL, DATAfile, LABEL='SSALB_FILE:', default = 'none', RC=STATUS)
     VERIFY_(STATUS)
     if(trim(datafile) == 'none') then; ssalb = 0.0
     else; call MAPL_ReadForcing( MAPL, 'SSALB_' // suffix, trim(DATAFILE) // suffix, CURRENTTIME, ssalb, RC=STATUS)
      VERIFY_(STATUS)
     endif;
     assalb(k)%b => ssalb
    enddo
#endif

    RETURN_(ESMF_SUCCESS)
  end subroutine obio_extra

end subroutine RUN
!----------------------------------------------------------------------------------------------------------------------------------

    function renamefile(name, time) result(name0)

      character(len = *), intent(in) :: name;
      type(esmf_time), intent(inout) :: time;
      character(len = len(name)) :: name0; 

      integer :: year, month, day, status, i;
          
        name0 = trim(name);
        i = index(string = name, substring = "yyyymmdd");
        if(i == 0) return;

        call esmf_timeget(time, yy = year, mm = month, dd = day, rc = status); 
        write(unit = name0(i:i + 7), fmt = "(i4,i2.2,i2.2)") year, month, day;

    end function;  

!----------------------------------------------------------------------------------------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! !IROUTINE: Finalize        -- Finalize method for CICEThermo wrapper

! !INTERFACE:

  subroutine Finalize ( gc, import, export, clock, rc ) 

! !ARGUMENTS:

  type(ESMF_GridComp), intent(INOUT) :: gc     ! Gridded component 
  type(ESMF_State),    intent(INOUT) :: import ! Import state
  type(ESMF_State),    intent(INOUT) :: export ! Export state
  type(ESMF_Clock),    intent(INOUT) :: clock  ! The supervisor clock
  integer, optional,   intent(  OUT) :: rc     ! Error code:

!EOP

    type (MAPL_MetaComp), pointer:: MAPL 

! ErrLog Variables

    character(len=ESMF_MAXSTR)       :: IAm
    integer                          :: STATUS
    character(len=ESMF_MAXSTR)       :: COMP_NAME

    integer                          :: DO_CICE_THERMO  ! default (=0) is to run without CICE

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    Iam = "Finalize"
    call ESMF_GridCompGet( gc, NAME=comp_name, RC=STATUS)
    VERIFY_(STATUS)

! Get my internal MAPL_Generic state
!-----------------------------------

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_GetResource ( MAPL, DO_CICE_THERMO, Label="USE_CICE_Thermo:" , DEFAULT=0, RC=STATUS)
    VERIFY_(STATUS)

! Profilers
!----------

    call MAPL_TimerOn(MAPL,"TOTAL"   )
    call MAPL_TimerOn(MAPL,"FINALIZE")

    if (DO_CICE_THERMO /= 0) call dealloc_column_physics( MAPL_AM_I_Root(), Iam )

    call MAPL_TimerOff(MAPL,"FINALIZE")
    call MAPL_TimerOff(MAPL,"TOTAL"   )

! Generic Finalize
! ------------------
    
    call MAPL_GenericFinalize( GC, IMPORT, EXPORT, CLOCK, RC=STATUS)
    VERIFY_(STATUS)


! All Done
!---------

    RETURN_(ESMF_SUCCESS)
  end subroutine Finalize

end module GEOS_DataAtmGridCompMod
