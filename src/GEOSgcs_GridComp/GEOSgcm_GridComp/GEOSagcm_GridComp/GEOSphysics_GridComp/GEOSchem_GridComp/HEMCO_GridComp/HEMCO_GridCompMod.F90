#include "MAPL_Generic.h"
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1          !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: HEMCO_GridCompMod -- A gridded component to compute emissions 
!  using the Harvard-NASA emissions component HEMCO. 
!
!  !DESCRIPTION:
!
!  HEMCO is a software component for computing (atmospheric) emissions
!  from different sources, regions, and species on a user-defined
!  grid. It can combine, overlay, and update a set of data inventories
!  ('base emissions') and scale factors, as specified by the user
!  through the HEMCO configuration file. Emissions that depend on
!  environmental variables and non-linear parameterizations are
!  calculated in separate HEMCO extensions. See Keller et al. (2014) for
!  more details.
!\\
!\\
!  This component computes emissions as specified by the user via the HEMCO
!  configuration file (HEMCO\_Config.rc, unless specified otherwise). Multiple 
!  HEMCO instances can be employed in a single run (with instances referring 
!  to different emission configurations). The computed emissions are made 
!  avaiable to other components (e.g. GOCART) via user-specified HEMCO 
!  diagnostics. All HEMCO diagnostics are automatically added to the list of 
!  HEMCO exports, thus making them available to any other component. The 
!  HEMCO diagnostics can be defined in the HEMCO diagnostics configuration 
!  (HEMCO\_DiagnFile.rc, unless specified otherwise).
!
! !REFERENCES: 
!
! C. A. Keller, M. S. Long, R. M. Yantosca, A. M. Da Silva, S. Pawson, D. J. 
! Jacob: HEMCO v1.0: a versatile, ESMF-compliant component for calculation 
! emissions in atmospheric models. Geosci. Model Dev., 7, 1409-1417, 2014. 
!\\
!\\
!
! !INTERFACE: 
!
MODULE HEMCO_GridCompMod
!
! !USES:
!
  USE ESMF
  USE MAPL_Mod

  ! HEMCO routines/variables
  USE HCO_ERROR_MOD
  USE HCO_Diagn_Mod
  USE HCO_CharTools_Mod
  USE HCO_TYPES_MOD,        ONLY : ConfigObj
  USE HCO_STATE_MOD,        ONLY : HCO_State
  USE HCOX_STATE_MOD,       ONLY : EXT_State

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC SetServices
!
! !PRIVATE MEMBER FUNCTIONS:
!
  PRIVATE                              :: Initialize_
  PRIVATE                              :: Run1
  PRIVATE                              :: Run2
  PRIVATE                              :: Finalize_
  PRIVATE                              :: HEMCOinit_
  PRIVATE                              :: HEMCOrun_
  PRIVATE                              :: HEMCOfinal_
  PRIVATE                              :: NewInst_
  PRIVATE                              :: SetExtFields
!
! !PRIVATE TYPES:
!

  ! HEMCO state objects for various HEMCO instances 
  TYPE :: Instance
     TYPE(ConfigObj), POINTER             :: HcoConfig 
     TYPE(HCO_State), POINTER             :: HcoState
     TYPE(Ext_State), POINTER             :: ExtState
     TYPE(Instance),  POINTER             :: NextInst
  END TYPE Instance

  ! Linked list holding all active HEMCO instances
  TYPE(Instance), POINTER                 :: Instances => NULL()
!
! !MODULE INTERFACES
!
!EOP
CONTAINS

!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1          !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  SetServices 
!
! !DESCRIPTION: SetServices routine 
!
! !INTERFACE:
!
    subroutine SetServices ( GC, RC )
!
! !USES:
!
    USE HCOI_ESMF_MOD, ONLY : HCO_SetServices
!
! !INPUT/OUTPUT PARAMETERS:
!
    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
!
! !OUTPUT PARAMETERS:
!
    integer, optional  , intent(  OUT) :: RC  ! return code
!
! !REVISION HISTORY:
!  22 Feb 2016 - C. Keller   - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
! 
! !LOCAL VARIABLES:
!
    integer                            :: N, nnInst
    type (Instance), pointer           :: ThisInst => NULL()
    type (ESMF_Config)                 :: MaplCF
    type (ESMF_Config)                 :: HemcoCF
    character(len=ESMF_MAXSTR)         :: COMP_NAME
    character(len=ESMF_MAXSTR)         :: Label 
    character(len=ESMF_MAXSTR)         :: ConfigFile 
    logical                            :: am_I_Root

    character(len=ESMF_MAXSTR)         :: Iam
    integer                            :: STATUS

    !=======================================================================
    ! Set services begins here 
    !=======================================================================

    ! Set up traceback info
    Iam = 'SetServices'
    call ESMF_GridCompGet( GC, NAME=COMP_NAME, __RC__ )
    Iam = trim(COMP_NAME) // '::' // Iam

    ! Set the Initialize, Run and Finalize entry points
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_INITIALIZE, Initialize_, __RC__ ) 
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN,        Run1,        __RC__ )
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN,        Run2,        __RC__ )
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_FINALIZE,   Finalize_,   __RC__ )

    ! Root CPU?
    am_I_Root = MAPL_Am_I_Root()

    ! Define ESMF config for HEMCO
    HemcoCF = ESMF_ConfigCreate(__RC__)
    CALL ESMF_ConfigLoadFile( HemcoCF, TRIM(COMP_NAME)//'_GridComp.rc', __RC__ )

    ! Get number of instances
    call ESMF_ConfigGetAttribute(HemcoCF, nnInst, Label="HEMCO_Instances:" , DEFAULT=1, __RC__)
  
    ! Verbose
    IF ( am_I_Root ) WRITE(*,*) TRIM(Iam), ' - number of HEMCO instances: ', nnInst
 
    ! Set HEMCO services for all instances
    DO N = 1, nnInst 

       ! Get HEMCO configuration file names
       WRITE(Label,'(a14,i3.3,a1)') 'HEMCO_CONFIG--',N,':'
       call ESMF_ConfigGetAttribute( HemcoCF, ConfigFile, Label=TRIM(Label), &
                                     DEFAULT="HEMCOsa_Config.rc", __RC__)

       ! Verbose
       IF ( am_I_Root ) WRITE(*,'(a19,i3.3,a2,a)') '--> HEMCO instance ', N, ': ', TRIM(ConfigFile)
 
       ! Create a new instance object that holds the HEMCO states for this 
       ! instance. Will be added to linked list Instances.
       CALL NewInst_( ThisInst, __RC__ ) 
 
       ! Set HEMCO services (reads the HEMCO configuration file and registers
       ! all required emission fields). This also creates exports for every
       ! diagnostics field defined in the HEMCO diagnostics file. 
       CALL HCO_SetServices( am_I_Root, GC, ThisInst%HcoConfig, &
                             TRIM(ConfigFile), __RC__ )

       ! Cleanup pointer
       ThisInst => NULL()
    ENDDO 

    !=======================================================================
    !                    %%% MAPL Data Services %%%
    !=======================================================================
!EOC
!BOP
!
! !IMPORT STATE:
!
    ! Imports
    CALL MAPL_AddImportSpec(GC,                                    &
         SHORT_NAME         = 'AIRDENS',                           &
         LONG_NAME          = 'air_density',                       &
         UNITS              = 'kgm-3',                             &
         DIMS               = MAPL_DimsHorzVert,                   &
         VLOCATION          = MAPL_VLocationCenter,                &
                                                         __RC__  )

    CALL MAPL_AddImportSpec(GC,                                    &
         SHORT_NAME         = 'ZLE',                               &
         LONG_NAME          = 'geopotential_height',               &
         UNITS              = 'm',                                 &
         DIMS               = MAPL_DimsHorzVert,                   &
         VLOCATION          = MAPL_VLocationEdge,                  &
                                                         __RC__  )

    CALL MAPL_AddImportSpec(GC,                                    &
         SHORT_NAME         = 'PS',                                &
         LONG_NAME          = 'surface_pressure',                  &
         UNITS              = 'Pa',                                &
         DIMS               = MAPL_DimsHorzOnly,                   &
         VLOCATION          = MAPL_VLocationNone,                  &
                                                         __RC__  )

    CALL MAPL_AddImportSpec(GC,                                    &
         SHORT_NAME         = 'AREA',                              &
         LONG_NAME          = 'grid_cell_area',                    &
         UNITS              = 'm2',                                &
         DIMS               = MAPL_DimsHorzOnly,                   &
         VLOCATION          = MAPL_VLocationNone,                  &
                                                         __RC__  )

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'U10M',  &
        LONG_NAME          = '10-meter_eastward_wind',  &
        UNITS              = 'm/s', &
        DIMS               = MAPL_DimsHorzOnly,    &
        VLOCATION          = MAPL_VLocationNone,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'V10M',  &
        LONG_NAME          = '10-meter_northward_wind',  &
        UNITS              = 'm/s', &
        DIMS               = MAPL_DimsHorzOnly,    &
        VLOCATION          = MAPL_VLocationNone,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'FRLAND',  &
        LONG_NAME          = 'fraction_of_land',  &
        UNITS              = '1', &
        DIMS               = MAPL_DimsHorzOnly,    &
        VLOCATION          = MAPL_VLocationNone,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'FRLANDICE',  &
        LONG_NAME          = 'fraction_of_land_ice',  &
        UNITS              = '1', &
        DIMS               = MAPL_DimsHorzOnly,    &
        VLOCATION          = MAPL_VLocationNone,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'FRLAKE',  &
        LONG_NAME          = 'fraction_of_lake',  &
        UNITS              = '1', &
        DIMS               = MAPL_DimsHorzOnly,    &
        VLOCATION          = MAPL_VLocationNone,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'FROCEAN',  &
        LONG_NAME          = 'fraction_of_ocean',  &
        UNITS              = '1', &
        DIMS               = MAPL_DimsHorzOnly,    &
        VLOCATION          = MAPL_VLocationNone,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'TS',  &
        LONG_NAME          = 'surface_skin_temperature',  &
        UNITS              = 'K', &
        DIMS               = MAPL_DimsHorzOnly,    &
        VLOCATION          = MAPL_VLocationNone,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'T2M', &
        LONG_NAME          = '2-meter_air_temperature', &
        UNITS              = 'K', &
        DIMS               = MAPL_DimsHorzOnly, &
        VLOCATION          = MAPL_VLocationNone, &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'ALBVF',  &
        LONG_NAME          = 'surface_albedo_for_visible_diffuse',  &
        UNITS              = '1', &
        DIMS               = MAPL_DimsHorzOnly,    &
        VLOCATION          = MAPL_VLocationNone,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'PLE',  &
        LONG_NAME          = 'air_pressure_layer_interfaces',  &
        UNITS              = 'Pa', &
        DIMS               = MAPL_DimsHorzVert,    &
        VLOCATION          = MAPL_VLocationEdge,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'TROPP',  &
        LONG_NAME          = 'tropopause_pressure_based_on_blended_estimate',  &
        UNITS              = 'Pa', &
        DIMS               = MAPL_DimsHorzOnly,    &
        VLOCATION          = MAPL_VLocationNone,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'T',  &
        LONG_NAME          = 'air_temperature',  &
        UNITS              = 'K', &
        DIMS               = MAPL_DimsHorzVert,    &
        VLOCATION          = MAPL_VLocationCenter,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'Z0H',  &
        LONG_NAME          = 'surface_roughness',  &
        UNITS              = 'm', &
        DIMS               = MAPL_DimsHorzOnly,    &
        VLOCATION          = MAPL_VLocationNone,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'USTAR',  &
        LONG_NAME          = 'surface_velocity_scale',  &
        UNITS              = 'm/s', &
        DIMS               = MAPL_DimsHorzOnly,    &
        VLOCATION          = MAPL_VLocationNone,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'WET1',  &
        LONG_NAME          = 'surface_soil_wetness',  &
        UNITS              = '1', &
        DIMS               = MAPL_DimsHorzOnly,    &
        VLOCATION          = MAPL_VLocationNone,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'WET2',  &
        LONG_NAME          = 'root_soil_wetness',  &
        UNITS              = '1', &
        DIMS               = MAPL_DimsHorzOnly,    &
        VLOCATION          = MAPL_VLocationNone,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'Q',  &
        LONG_NAME          = 'specific_humidity',  &
        UNITS              = 'kg/kg', &
        DIMS               = MAPL_DimsHorzVert,    &
        VLOCATION          = MAPL_VLocationCenter,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'SNOMAS',  &
        LONG_NAME          = 'snow_mass', &
        UNITS              = 'kg m-2', &
        DIMS               = MAPL_DimsHorzOnly,    &
        VLOCATION          = MAPL_VLocationNone,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'SNOWDP',  &
        LONG_NAME          = 'snow_depth', &
        UNITS              = 'm', &
        DIMS               = MAPL_DimsHorzOnly,    &
        VLOCATION          = MAPL_VLocationNone,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'LWI',  &
        LONG_NAME          = 'land-ocean-ice_mask',  &
        UNITS              = '1', &
        DIMS               = MAPL_DimsHorzOnly,    &
        VLOCATION          = MAPL_VLocationNone,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'LAI',  &
        LONG_NAME          = 'leaf area index',  &
        UNITS              = '1', &
        DIMS               = MAPL_DimsHorzOnly,    &
        VLOCATION          = MAPL_VLocationNone,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'CNV_MFC',  &
        LONG_NAME          = 'cumulative_mass_flux',  &
        UNITS              = 'kg/m2/s', &
        DIMS               = MAPL_DimsHorzVert,    &
        VLOCATION          = MAPL_VLocationEdge,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'DRPAR',  &
        LONG_NAME          = 'surface_downwelling_par_beam_flux',  &
        UNITS              = 'W/m2', &
        DIMS               = MAPL_DimsHorzOnly,    &
        VLOCATION          = MAPL_VLocationNone,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'DFPAR',  &
        LONG_NAME          = 'surface_downwelling_par_diffuse_flux',  &
        UNITS              = 'W/m2', &
        DIMS               = MAPL_DimsHorzOnly,    &
        VLOCATION          = MAPL_VLocationNone,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'SWNDSRF',  &
        LONG_NAME          = 'surface_net_downward_shortwave_flux',  &
        UNITS              = 'W/m2', &
        DIMS               = MAPL_DimsHorzOnly,    &
        VLOCATION          = MAPL_VLocationNone,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'CLDTT',  &
        LONG_NAME          = 'total_cloud_area_fraction', &
        UNITS              = '1', &
        DIMS               = MAPL_DimsHorzOnly,    &
        VLOCATION          = MAPL_VLocationNone,    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)
!
! !INTERNAL STATE:
!
#   include "HEMCO_InternalSpec___.h"
!
! !EXTERNAL STATE:
!
!!!#   include "HEMCO_ExportSpec___.h"

!EOP
!BOC

    CALL MAPL_TimerAdd(GC, NAME="INITIALIZE", __RC__ )
    CALL MAPL_TimerAdd(GC, NAME="RUN",        __RC__ )
    CALL MAPL_TimerAdd(GC, NAME="FINALIZE",   __RC__ )

! Set services now
! ----------------
   call MAPL_GenericSetServices  ( GC, RC=STATUS )
   VERIFY_(STATUS)

   RETURN_(ESMF_SUCCESS)
  
  end subroutine SetServices

!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1          !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize_ 
!
! !DESCRIPTION: Initialize\_ is the initialize method of the HEMCO 
!  gridded component. This is a simple ESMF/MAPL wrapper which calls down
!  to the Initialize method of the HEMCO code. 
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Initialize_( GC, Import, Export, Clock, RC )
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT)         :: GC      ! Ref. to this GridComp
    TYPE(ESMF_State),    INTENT(INOUT), TARGET :: Import  ! Import state
    TYPE(ESMF_State),    INTENT(INOUT)         :: Export  ! Export state
    TYPE(ESMF_Clock),    INTENT(INOUT)         :: Clock   ! ESMF clock object
!                                                      
! !OUTPUT PARAMETERS:                                  
!                                                      
    INTEGER,             INTENT(OUT)   :: RC          ! Error return code
!
! !REVISION HISTORY:
!  22 Feb 2016 - C. Keller   - Initial version 
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!
    TYPE(Instance), POINTER  :: ThisInst => NULL()

    ! Set up traceback 
    __Iam__('Initialize_')

    !=======================================================================
    ! Initialize_ begins here!
    !=======================================================================

    ! Initialize MAPL Generic
    CALL MAPL_GenericInitialize( GC, Import, Export, Clock, __RC__ )

    ! Initialize HEMCO for each instance
    ThisInst => Instances
    DO WHILE ( ASSOCIATED(ThisInst) )

       ! Initialize HEMCO for this instance
       CALL HEMCOinit_( GC, Import, Export, Clock, ThisInst, __RC__ )

       ! Go to next instance in list
       ThisInst => ThisInst%NextInst
    ENDDO

    ! Cleanup
    ThisInst => NULL()

    ! Successful return
    RETURN_(ESMF_SUCCESS)

  END SUBROUTINE Initialize_
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1          !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Run1
!
! !DESCRIPTION: Run1 is the HEMCO phase 1 run interface. It calls the main
! HEMCO run routine. 
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Run1 ( GC, Import, Export, Clock, RC )
!
! !USES:
!
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT) :: GC          ! Ref to this GridComp
    TYPE(ESMF_State),    INTENT(INOUT) :: Import      ! Import State
    TYPE(ESMF_State),    INTENT(INOUT) :: Export      ! Export State
    TYPE(ESMF_Clock),    INTENT(INOUT) :: Clock       ! ESMF Clock object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,             INTENT(OUT)   :: RC          ! Error return code
!
! !REVISION HISTORY:
!  22 Feb 2016 - C. Keller   - Initial version 
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!  
    TYPE(Instance), POINTER  :: ThisInst => NULL()

    __Iam__('Run1')

    !=======================================================================
    ! Run1 begins here!
    !=======================================================================

    ! Call Run method for each instance
    ThisInst => Instances
    DO WHILE ( ASSOCIATED( ThisInst ) )
       CALL HEMCOrun_( GC, Import, Export, Clock, ThisInst, __RC__ )
       ThisInst => ThisInst%NextInst
    ENDDO
 
    ! Cleanup
    ThisInst => NULL()

    ! Successful return
    RETURN_(ESMF_SUCCESS)

  end subroutine Run1
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1          !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Run2
!
! !DESCRIPTION: Run2 is the HEMCO phase 2 run interface. This is an 'empty'
! routine. All HEMCO related calculations are done in run phase 1.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Run2 ( GC, Import, Export, Clock, RC )
!
! !USES:
!
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT) :: GC          ! Ref to this GridComp
    TYPE(ESMF_State),    INTENT(INOUT) :: Import      ! Import State
    TYPE(ESMF_State),    INTENT(INOUT) :: Export      ! Export State
    TYPE(ESMF_Clock),    INTENT(INOUT) :: Clock       ! ESMF Clock object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,             INTENT(OUT)   :: RC          ! Error return code
!
! !REVISION HISTORY:
!  22 Feb 2016 - C. Keller   - Initial version 
!EOP
!------------------------------------------------------------------------------
!BOC
!
! LOCAL VARIABLES:
!  
    __Iam__('Run2')

    !=======================================================================
    ! Run2 begins here!
    !=======================================================================

    !!! Nothing to do in run phase 2 !!!

    ! Successful return
    RETURN_(ESMF_SUCCESS)

  end subroutine Run2
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1          !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Finalize_
!
! !DESCRIPTION: Finalize\_ is the finalize method of the HEMCO gridded
! component.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE Finalize_( GC, Import, Export, Clock, RC ) 
!
! !USES:
!
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT) :: GC       ! Ref. to this GridComp
    TYPE(ESMF_State),    INTENT(INOUT) :: Import   ! Import State
    TYPE(ESMF_State),    INTENT(INOUT) :: Export   ! Export State
    TYPE(ESMF_Clock),    INTENT(INOUT) :: Clock    ! ESMF Clock object
!
! !OUTPUT PARAMETERS:
!
    INTEGER,             INTENT(OUT)   :: RC       ! Success or failure?
!
! !REVISION HISTORY:
!  22 Feb 2016 - C. Keller   - Initial version 
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE(Instance), POINTER     :: ThisInst => NULL()
    TYPE(Instance), POINTER     :: NextInst => NULL()
    CHARACTER(LEN=ESMF_MAXSTR)  :: compName
    LOGICAL                     :: am_I_Root 
    INTEGER                     :: ERROR

    __Iam__('Finalize')

    !=======================================================================
    ! FINALIZE_ begins here!
    !=======================================================================

    ! Call Run method for each instance
    ThisInst => Instances
    DO WHILE ( ASSOCIATED( ThisInst ) )
       CALL HEMCOfinal_( GC, Import, Export, Clock, ThisInst, __RC__ )
       ThisInst => ThisInst%NextInst
    ENDDO

    ! Cleanup instances list
    ThisInst => Instances
    DO WHILE ( ASSOCIATED( ThisInst ) )
       NextInst           => ThisInst%NextInst 
       ThisInst%HcoConfig => NULL() 
       ThisInst%HcoState  => NULL() 
       ThisInst%ExtState  => NULL() 
       ThisInst           => NextInst
    ENDDO

    ! Cleanup
    ThisInst => NULL()
    NextInst => NULL()

    ! Finalize MAPL Generic
    CALL MAPL_GenericFinalize( GC, Import, Export, Clock, __RC__ )

    ! Return w/ success
    RETURN_(ESMF_SUCCESS)

  end subroutine Finalize_
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1          !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HEMCOinit_ 
!
! !DESCRIPTION: Subroutine HEMCOinit\_ is a wrapper routine to initialize the
! HEMCO gridded component. 
!\\
!\\
! !INTERFACE:
!
    SUBROUTINE HEMCOinit_( GC, Import, Export, Clock, Inst, RC )
!
! !USES:
!
    USE HCOI_STANDALONE_MOD,     ONLY : Get_nnMatch
    USE HCOI_STANDALONE_MOD,     ONLY : Register_Species 
    USE HCOI_STANDALONE_MOD,     ONLY : HCOI_SA_InitCleanup 
    USE HCOI_STANDALONE_MOD,     ONLY : Define_Diagnostics 
    USE HCO_STATE_MOD,           ONLY : HcoState_Init 
    USE HCO_DRIVER_MOD,          ONLY : HCO_Init
    USE HCOX_DRIVER_MOD,         ONLY : HCOX_Init
    USE HCO_ARR_MOD,             ONLY : HCO_ArrAssert
!
! !INPUT PARAMETERS:
!
    TYPE(ESMF_Clock),    INTENT(IN)            :: Clock       ! ESMF clock obj 
!                                                             
! !INPUT/OUTPUT PARAMETERS:                                   
!
    TYPE(ESMF_GridComp), INTENT(INOUT), TARGET :: GC          ! GC grid comp
    TYPE(ESMF_State),    INTENT(INOUT), TARGET :: Import      ! Import state
    TYPE(ESMF_State),    INTENT(INOUT), TARGET :: Export      ! Export state
    TYPE(Instance),      POINTER               :: Inst        ! Instance object
!                                                             
! !OUTPUT PARAMETERS:                                   
!
    INTEGER,             INTENT(OUT), OPTIONAL :: RC          ! 0 = all is well
!
! !REVISION HISTORY:
!  22 Feb 2016 - C. Keller   - Initial version 
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE(MAPL_MetaComp), POINTER    :: STATE
    TYPE(ESMF_Config)               :: MaplCF
    TYPE(ESMF_Grid)                 :: Grid
    TYPE(ESMF_Alarm)                :: ALARM
    TYPE(ESMF_TimeInterval)         :: RingInterval 
    
    REAL(ESMF_KIND_R4),  POINTER    :: lonCtr(:,:)        ! Lon ctrs [rad]
    REAL(ESMF_KIND_R4),  POINTER    :: latCtr(:,:)        ! Lat ctrs [rad]
    INTEGER                         :: locDims(3)         ! grid dimensions

    INTEGER                         :: nnMatch
    REAL                            :: tsChem, tsDyn
    REAL(ESMF_KIND_R8)              :: s_r8

    LOGICAL                         :: am_I_Root 
    INTEGER                         :: HCRC

    ! For MAPL/ESMF error handling (defined Iam and STATUS)
    __Iam__('HEMCOinit_ (GEOS_EmisGridComp.F90)') 

    ! ================================================================
    ! HEMCOinit_ begins here
    ! ================================================================

    ! Root CPU?
    am_I_Root = MAPL_am_I_Root()

    ! ------------------------------------------------------------------
    ! HEMCO initialization 
    ! ------------------------------------------------------------------

    IF ( am_I_Root ) THEN
       CALL HCO_LogFile_Open( Inst%HcoConfig%Err, RC = HCRC )
       ASSERT_(HCRC==HCO_SUCCESS)
    ENDIF

    !-----------------------------------------------------------------
    ! Extract species to use in HEMCO 
    CALL Get_nnMatch( am_I_Root, Inst%HcoConfig, nnMatch, HCRC )
    ASSERT_(HCRC==HCO_SUCCESS)

    !-----------------------------------------------------------------
    ! Initialize HCO state. Use only species that are used
    ! in GEOS-Chem and are also found in the HEMCO config. file.
    CALL HcoState_Init ( am_I_Root, Inst%HcoState, Inst%HcoConfig, nnMatch, HCRC )
    ASSERT_(HCRC==HCO_SUCCESS)

    ! ------------------------------------------------------------------
    ! Register species. Grid MUST be defined before doing this! 
    CALL Register_Species ( am_I_Root, Inst%HcoState, HCRC )
    ASSERT_(HCRC==HCO_SUCCESS)

    ! ------------------------------------------------------------------
    ! Get grid definition 
    ! ==> Grid box area is set in run call

    ! Grid dimensions
    CALL ESMF_GridCompGet ( GC, grid=grid, __RC__ )
    CALL MAPL_GridGet     ( Grid, localCellCountPerDim = locDims, __RC__ )
    Inst%HcoState%NX = locDims(1) 
    Inst%HcoState%NY = locDims(2)
    Inst%HcoState%NZ = locDims(3)

    ! Grid box centers
    CALL MAPL_GetObjectFromGC ( GC, STATE, __RC__ ) 
    CALL MAPL_Get             ( STATE, lons=lonCtr, lats=latCtr, __RC__ )

    CALL HCO_ArrAssert ( Inst%HcoState%Grid%XMID, Inst%HcoState%NX, &
                         Inst%HcoState%NY, RC )
    ASSERT_(RC==HCO_SUCCESS)
    CALL HCO_ArrAssert ( Inst%HcoState%Grid%YMID, Inst%HcoState%NX, &
                         Inst%HcoState%NY, RC )
    ASSERT_(RC==HCO_SUCCESS)

    Inst%HcoState%Grid%XMID%Val = lonCtr / Inst%HcoState%Phys%PI_180 
    Inst%HcoState%Grid%YMID%Val = latCtr / Inst%HcoState%Phys%PI_180 

    ! ------------------------------------------------------------------
    ! Set time steps 
    CALL ESMF_GridCompGet( GC, Config=MaplCF, __RC__ )
    CALL ESMF_ConfigGetAttribute( MaplCF, tsDyn, Label="RUN_DT:", __RC__ )
    Inst%HcoState%TS_DYN  = tsDyn

    CALL MAPL_Get            ( STATE, RUNALARM=ALARM, __RC__ )
    CALL ESMF_AlarmGet       ( ALARM, RingInterval=RingInterval, __RC__ )
    CALL ESMF_TimeIntervalGet( RingInterval, s_r8=s_r8, __RC__ )
    Inst%HcoState%TS_CHEM = s_r8
    Inst%HcoState%TS_EMIS = s_r8

!    IF ( am_I_Root ) THEN
!       WRITE(*,*) 'HEMCO time steps:'
!       WRITE(*,*) 'Dynamic  : ', Inst%HcoState%TS_DYN 
!       WRITE(*,*) 'Emissions: ', Inst%HcoState%TS_EMIS
!       WRITE(*,*) 'Chemistry: ', Inst%HcoState%TS_CHEM
!    ENDIF

    ! ------------------------------------------------------------------
    ! Manually set some settings

    ! Pass ESMF/MAPL states to HEMCO state object
    Inst%HcoState%GRIDCOMP => GC
    Inst%HcoState%IMPORT   => Import
    Inst%HcoState%EXPORT   => Export

    ! Don't let HEMCO schedule the diagnostics output (will be scheduled manually)
    Inst%HcoState%Options%HcoWritesDiagn = .FALSE.

    ! Don't add import fields to HEMCO diagnostics. This is redundant in ESMF.
    Inst%HcoState%Options%Field2Diagn    = .FALSE.  

    ! Set ESMF flag to TRUE
    Inst%HcoState%isESMF = .TRUE.

    ! ------------------------------------------------------------------
    ! Initialize HEMCO internal lists and variables. All data
    ! information is written into internal lists (ReadList) and 
    ! the HEMCO configuration file is removed from buffer in this
    ! step. Also initializes the HEMCO clock
    CALL HCO_Init( am_I_Root, Inst%HcoState, HCRC )
    ASSERT_(HCRC==HCO_SUCCESS)

    ! ------------------------------------------------------------------
    ! Initialize extensions.
    ! This initializes all (enabled) extensions and selects all met.
    ! fields needed by them. 
    CALL HCOX_Init( am_I_Root, Inst%HcoState, Inst%ExtState, HCRC )
    ASSERT_(HCRC==HCO_SUCCESS)

    ! ------------------------------------------------------------------
    ! Define diagnostics. This creates a HEMCO diagnostics entry for
    ! every element of the HEMCO diagnostics file. In addition, if set
    ! in the HEMCO configuration file, a default diagnostics is created 
    ! for every HEMCO species. 
    CALL Define_Diagnostics( am_I_Root, Inst%HcoState, HCRC )
    ASSERT_(HCRC==HCO_SUCCESS)

    ! ------------------------------------------------------------------
    ! Cleanup 
    CALL HCOI_SA_InitCleanup ( am_I_Root, HCRC )
    ASSERT_(HCRC==HCO_SUCCESS)

    ! Nullify pointers
    Inst%HcoState%GRIDCOMP => NULL()
    Inst%HcoState%IMPORT   => NULL() 
    Inst%HcoState%EXPORT   => NULL() 

    ! Return w/ success
    RETURN_(ESMF_SUCCESS)

    END SUBROUTINE HEMCOinit_ 
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1          !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: HEMCOrun_ 
!
! !DESCRIPTION: Subroutine HEMCOrun\_ is a wrapper routine to initialize the
! HEMCO gridded component. 
!\\
!\\
! !INTERFACE:
!
    SUBROUTINE HEMCOrun_( GC, Import, Export, Clock, Inst, RC )
!
! !USES:
!
    USE HCO_ARR_MOD,                 ONLY : HCO_ArrAssert
    USE HCO_FLUXARR_MOD,             ONLY : HCO_FluxArrReset 
    USE HCO_CLOCK_MOD,               ONLY : HcoClock_Set
    USE HCO_DRIVER_MOD,              ONLY : HCO_RUN
    USE HCOX_DRIVER_MOD,             ONLY : HCOX_RUN
    USE HCOIO_DIAGN_MOD,             ONLY : HcoDiagn_Write
!
! !INPUT PARAMETERS:
!
    TYPE(ESMF_Clock),    INTENT(IN)            :: Clock       ! ESMF clock obj 
!                                                             
! !INPUT/OUTPUT PARAMETERS:                                   
!
    TYPE(ESMF_GridComp), INTENT(INOUT), TARGET :: GC          ! GC grid comp
    TYPE(ESMF_State),    INTENT(INOUT), TARGET :: Import      ! Import state
    TYPE(ESMF_State),    INTENT(INOUT), TARGET :: Export      ! Export state
    TYPE(Instance),      POINTER               :: Inst        ! HEMCO instance 
!                                                             
! !OUTPUT PARAMETERS:                                   
!
    INTEGER,             INTENT(OUT), OPTIONAL :: RC          ! 0 = all is well
!
! !REVISION HISTORY:
!  22 Feb 2016 - C. Keller   - Initial version 
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE(ESMF_Time)             :: currTime
    INTEGER                     :: yyyy, mm, dd, h, m, s, doy 
    INTEGER                     :: STAT
    LOGICAL                     :: am_I_Root

    ! For MAPL/ESMF error handling (defined Iam and STATUS)
    __Iam__('HEMCOrun_ (HEMCO_GridCompMod.F90)') 

    ! ================================================================
    ! HEMCOrun_ begins here
    ! ================================================================

    ! ------------------------------------------------------------------
    ! Pre-run assignments 
    ! ------------------------------------------------------------------
   
    ! Root CPU?
    am_I_Root = MAPL_am_I_Root()

    ! Pass ESMF/MAPL states to HEMCO state object
    Inst%HcoState%GRIDCOMP => GC
    Inst%HcoState%IMPORT   => Import
    Inst%HcoState%EXPORT   => Export

    ! ------------------------------------------------------------------
    ! Set time 
    ! ------------------------------------------------------------------
    CALL ESMF_ClockGet( Clock, currTime = currTime, __RC__ ) 
    CALL ESMF_TimeGet ( currTime, yy=yyyy, mm=mm, dd=dd, &
                        dayOfYear=doy, h=h, m=m, s=s, __RC__ )

    CALL HcoClock_Set ( am_I_Root, Inst%HcoState, yyyy, mm, dd, h, &
                        m, s, cDOY=doy, IsEmisTime=.TRUE., RC=STAT )
    ASSERT_(STAT==HCO_SUCCESS)

    ! ------------------------------------------------------------------
    ! Set HEMCO fields 
    ! ------------------------------------------------------------------
    ! Make sure all required extension imports (met-fields and grid 
    ! quantities) are filled.
    CALL SetExtFields( am_I_Root, Clock, Inst, __RC__ ) 

    ! ------------------------------------------------------------------
    ! Run HEMCO core
    ! ------------------------------------------------------------------
    ! Reset all emissions to zero
    CALL HCO_FluxArrReset( Inst%HcoState, STAT )
    ASSERT_(STAT==HCO_SUCCESS)

    ! Make sure options are correct
    Inst%HcoState%Options%SpcMin     =  1
    Inst%HcoState%Options%SpcMax     =  Inst%HcoState%nSpc + 1
    Inst%HcoState%Options%CatMin     =  1
    Inst%HcoState%Options%CatMax     = -1
    Inst%HcoState%Options%ExtNr      =  0
    Inst%HcoState%Options%FillBuffer = .FALSE.

    ! Now run driver routine. This calculates all 'core' emissions, 
    ! i.e. all emissions that are not extensions.
    CALL HCO_Run( am_I_Root, Inst%HcoState, -1, STAT )
    ASSERT_(STAT==HCO_SUCCESS)

    ! ------------------------------------------------------------------
    ! Run HEMCO extensions 
    ! ------------------------------------------------------------------
    ! Calculate parameterized emissions
    CALL HCOX_Run( am_I_Root, Inst%HcoState, Inst%ExtState, STAT )
    ASSERT_(STAT==HCO_SUCCESS)

    ! ------------------------------------------------------------------
    ! Diagnostics 
    ! ------------------------------------------------------------------

    ! Update HEMCO diagnostics 
    CALL HcoDiagn_AutoUpdate ( am_I_Root, Inst%HcoState, STAT )
    ASSERT_(STAT==HCO_SUCCESS)
 
    ! Fill exports (from HEMCO diagnostics)
    !IF ( HcoState%Options%HcoWritesDiagn ) THEN 
    CALL HcoDiagn_Write( am_I_Root, Inst%HcoState, .FALSE., STAT ) 
    ASSERT_(STAT==HCO_SUCCESS)
    !ENDIF

    ! ------------------------------------------------------------------
    ! Cleanup 
    ! ------------------------------------------------------------------

    ! Nullify pointers
    Inst%HcoState%GRIDCOMP => NULL()
    Inst%HcoState%IMPORT   => NULL() 
    Inst%HcoState%EXPORT   => NULL() 

    ! Return w/ success
    RETURN_(ESMF_SUCCESS)

    END SUBROUTINE HEMCOrun_
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1          !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HEMCOfinal_
!
! !DESCRIPTION: HEMCOfinal\_ is the finalize method of the HEMCO gridded
! component.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HEMCOfinal_( GC, Import, Export, Clock, Inst, RC ) 
!
! !USES:
!
  USE HCO_DRIVER_MOD,    ONLY : HCO_FINAL
  USE HCOX_DRIVER_MOD,   ONLY : HCOX_FINAL
  USE HCO_STATE_MOD,     ONLY : HcoState_Final
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(ESMF_GridComp), INTENT(INOUT) :: GC       ! Ref. to this GridComp
    TYPE(ESMF_State),    INTENT(INOUT) :: Import   ! Import State
    TYPE(ESMF_State),    INTENT(INOUT) :: Export   ! Export State
    TYPE(ESMF_Clock),    INTENT(INOUT) :: Clock    ! ESMF Clock object
    TYPE(Instance),      POINTER       :: Inst     ! HEMCO instance
!
! !OUTPUT PARAMETERS:
!
    INTEGER,             INTENT(OUT)   :: RC       ! Success or failure?
!
! !REVISION HISTORY:
!  22 Feb 2016 - C. Keller   - Initial version 
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    LOGICAL    :: am_I_Root
    INTEGER    :: ERROR

    __Iam__('HEMCOfinal')

    !=======================================================================
    ! HEMCOfinal_ begins here!
    !=======================================================================

    ! write out diagnostics (?)
    ! TODO
    ! CALL Diagn_Cleanup()

    ! Root CPU?
    am_I_Root = MAPL_am_I_Root()

    ! Cleanup extensions and ExtOpt object 
    CALL HCOX_Final( am_I_Root, Inst%HcoState, Inst%ExtState, ERROR )
    ASSERT_(ERROR==HCO_SUCCESS)

    ! Cleanup HCO core
    CALL HCO_Final( am_I_Root, Inst%HcoState, .FALSE., ERROR ) 
    ASSERT_(ERROR==HCO_SUCCESS)

    ! Cleanup diagnostics
    CALL DiagnBundle_Cleanup ( Inst%HcoState%Diagn )

    ! Cleanup HcoState object
    CALL HcoState_Final ( Inst%HcoState ) 

    ! Return w/ success
    RETURN_(ESMF_SUCCESS)

  end subroutine HEMCOfinal_
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1          !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: SetExtFields 
!
! !DESCRIPTION: Subroutine SetExtFields makes sure that all required ExtState
! fields are filled. 
!\\
!\\
! !INTERFACE:
!
    SUBROUTINE SetExtFields ( am_I_Root, Clock, Inst, RC )
!
! !USES:
!
    USE HCOI_ESMF_MOD,      ONLY : HCO_Imp2Ext
    USE HCO_ARR_MOD,        ONLY : HCO_ArrAssert
    USE HCO_GeoTools_Mod,   ONLY : HCO_GetSUNCOS
!
! !INPUT PARAMETERS:
!
    LOGICAL,             INTENT(IN   )         :: am_I_Root   ! Root CPU? 
    TYPE(ESMF_Clock),    INTENT(IN)            :: Clock       ! ESMF clock obj 
!                                                             
! !INPUT/OUTPUT PARAMETERS:                                   
!
    TYPE(Instance),      POINTER               :: Inst        ! HEMCO instance 
!                                                             
! !OUTPUT PARAMETERS:                                   
!
    INTEGER,             INTENT(OUT), OPTIONAL :: RC          ! 0 = all is well
!
! !REVISION HISTORY:
!  22 Feb 2016 - C. Keller   - Initial version 
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE(HCO_State),     POINTER :: HcoState    => NULL()
    TYPE(Ext_State),     POINTER :: ExtState    => NULL()
    TYPE(MAPL_MetaComp), POINTER :: STATE
    INTEGER                      :: L, LR, NZ, OFF, STAT
!    REAL, ALLOCATABLE            :: AIRDENS(:,:,:)
!    REAL, POINTER                :: TH      (:,:,:) => NULL()
    REAL, POINTER                :: PLE     (:,:,:) => NULL()
    REAL, POINTER                :: AIRDENS (:,:,:) => NULL()
    REAL, POINTER                :: ZLE     (:,:,:) => NULL()
    REAL, POINTER                :: Q       (:,:,:) => NULL()
    REAL, POINTER                :: PS      (:,:  ) => NULL()
    REAL, POINTER                :: AREA    (:,:  ) => NULL()

    TYPE(MAPL_SunOrbit)          :: sunOrbit
    REAL(ESMF_KIND_R4),  POINTER :: lonCtr(:,:)
    REAL(ESMF_KIND_R4),  POINTER :: latCtr(:,:)
    REAL, ALLOCATABLE            :: ZTH(:,:)
    REAL, ALLOCATABLE            :: SLR(:,:)

    ! For MAPL/ESMF error handling (defined Iam and STATUS)
    __Iam__('SetExtFields (HEMCO_GridCompMod.F90)') 

    ! ================================================================
    ! SetExtFields begins here
    ! ================================================================

    ! Pointers to HEMCO state object and extension state object
    HcoState => Inst%HcoState
    ExtState => Inst%ExtState

    ! Get imports
!    CALL MAPL_GetPointer ( HcoState%IMPORT,      TH,      'TH', __RC__ ) 
    CALL MAPL_GetPointer ( HcoState%IMPORT,     PLE,     'PLE', __RC__ ) 
    CALL MAPL_GetPointer ( HcoState%IMPORT,     ZLE,     'ZLE', __RC__ ) 
    CALL MAPL_GetPointer ( HcoState%IMPORT,       Q,       'Q', __RC__ ) 
    CALL MAPL_GetPointer ( HcoState%IMPORT,      PS,      'PS', __RC__ ) 
    CALL MAPL_GetPointer ( HcoState%IMPORT,    AREA,    'AREA', __RC__ )
    CALL MAPL_GetPointer ( HcoState%IMPORT, AIRDENS, 'AIRDENS', __RC__ )

    ! ---------------------------------------------------------------- 
    ! Define grid quantities 
    ! ---------------------------------------------------------------- 

    ! 3rd dimension indexing
    NZ  = SIZE(ZLE,3) - 1
    ASSERT_(NZ == HcoState%NZ)
    OFF = LBOUND(ZLE,3)  ! Left bound index (0 or 1)

    ! Set AREA
    IF ( .NOT. ASSOCIATED(HcoState%Grid%AREA_M2%Val) ) THEN
       CALL HCO_ArrAssert( HcoState%Grid%AREA_M2, HcoState%NX, HcoState%NY, STAT )
       ASSERT_(STAT==HCO_SUCCESS)
       HcoState%Grid%AREA_M2%Val = AREA
    ENDIF

    ! Geopotential height (m)
    CALL HCO_ArrAssert( HcoState%Grid%ZSFC, HcoState%NX, HcoState%NY, STAT )
    ASSERT_(RC==HCO_SUCCESS)
    HcoState%Grid%ZSFC%Val = ZLE(:,:,NZ+OFF)

    ! Surface pressure
    CALL HCO_ArrAssert( HcoState%Grid%PSFC, HcoState%NX, HcoState%NY, STAT )
    ASSERT_(RC==HCO_SUCCESS)
    HcoState%Grid%PSFC%Val = PS

    ! Make sure HEMCO arrays are allocated and of correct size
    CALL HCO_ArrAssert( HcoState%Grid%BXHEIGHT_M, HcoState%NX, HcoState%NY, HcoState%NZ, STAT )
    ASSERT_(RC==HCO_SUCCESS)
    CALL HCO_ArrAssert( HcoState%Grid%PEDGE, HcoState%NX, HcoState%NY, HcoState%NZ+1, STAT )
    ASSERT_(RC==HCO_SUCCESS)
    IF ( ExtState%AIRVOL%DoUse ) THEN
       CALL HCO_ArrAssert( ExtState%AIRVOL%Arr, HcoState%NX, HcoState%NY, HcoState%NZ, STAT )
       ASSERT_(RC==HCO_SUCCESS)
    ENDIF 
    IF ( ExtState%AIR%DoUse ) THEN
       CALL HCO_ArrAssert( ExtState%AIR%Arr, HcoState%NX, HcoState%NY, HcoState%NZ, STAT )
       ASSERT_(RC==HCO_SUCCESS)
    ENDIF 

    ! Pass 3D fields to HEMCO state
    DO L=1,NZ+1

       ! Reversed index
       LR = NZ + 1 - L + OFF

       ! Pressure edges (Pa)
       HcoState%Grid%PEDGE%Val(:,:,L) = PLE(:,:,LR)

       ! Grid box height (m)
       IF ( L <= NZ ) THEN
          HcoState%Grid%BXHEIGHT_M%Val(:,:,L) = ZLE(:,:,LR-1) - ZLE(:,:,LR)

          ! Air volume (m3)
          IF ( ExtState%AIRVOL%DoUse ) THEN
             ExtState%AIRVOL%Arr%Val(:,:,L) = &
                HcoState%Grid%AREA_M2%Val(:,:) * HcoState%Grid%BXHEIGHT_M%Val(:,:,L)
          ENDIF

          ! Air mass (kg)
          IF ( ExtState%AIR%DoUse ) THEN
             ExtState%AIR%Arr%Val(:,:,L) = AIRDENS(:,:,NZ-L+1) * &
                HcoState%Grid%AREA_M2%Val(:,:) * HcoState%Grid%BXHEIGHT_M%Val(:,:,L)
          ENDIF
       ENDIF
    ENDDO

    ! ---------------------------------------------------------------- 
    ! Define extension variables 
    ! ---------------------------------------------------------------- 
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%U10M    , 'U10M'     , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%V10M    , 'V10M'     , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%ALBD    , 'ALBVF'    , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%WLI     ,  'LWI'     , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%T2M     , 'T2M'      , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%TSKIN   , 'TS'       , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%GWETTOP , 'WET1'     , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%GWETROOT, 'WET2'     , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%SNOWHGT , 'SNOMAS'   , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%SNODP   , 'SNOWDP'   , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%USTAR   , 'USTAR'    , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%Z0      , 'Z0H'      , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%TROPP   , 'TROPP'    , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%SZAFACT , 'SZAFACT'  , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%PARDR   , 'DRPAR'    , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%PARDF   , 'DFPAR'    , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%RADSWG  , 'SWNDSRF'  , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%FRCLND  , 'FRLAND'   , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%FRLAND  , 'FRLAND'   , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%FROCEAN , 'FROCEAN'  , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%FRLAKE  , 'FRLAKE'   , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%FRLANDIC, 'FRLANDICE', __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%CLDFRC  , 'CLDTT'    , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%JNO2    , 'JNO2'     , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%JO1D    , 'JO1D'     , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%LAI     , 'LAI'      , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%CHLR    , 'CHLR'     , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%CNV_MFC , 'CNV_MFC'  , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%SPHU    , 'Q'        , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%TK      , 'T'        , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%O3      , 'HCO_O3'   , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%NO      , 'HCO_NO'   , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%NO2     , 'HCO_NO2'  , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%HNO3    , 'HCO_HNO3' , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%DRY_TOTN, 'DRY_TOTN' , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%WET_TOTN, 'WET_TOTN' , __RC__ )
    CALL HCO_Imp2Ext ( am_I_Root, HcoState, ExtState%BYNCY   , 'BYNCY'    , __RC__ )

    ! SUNCOS
    IF ( ExtState%SUNCOS%DoUse ) THEN

       ALLOCATE( ZTH(HcoState%NX,HcoState%NY),            &
                 SLR(HcoState%NX,HcoState%NY), STAT=STATUS )
       ASSERT_(STATUS==0)

       ! Get the Orbit object (of type MAPL_SunOrbit),
       ! which is used in the call to MAPL_SunGetInsolation
       CALL MAPL_GetObjectFromGC ( HcoState%GRIDCOMP, STATE, __RC__ ) 
       CALL MAPL_Get( STATE,                       &
                      LONS      = lonCtr,             &
                      LATS      = latCtr,             &
                      ORBIT     = sunOrbit,           &
                      __RC__                         )

       ! Get the solar zenith angle 
       CALL MAPL_SunGetInsolation( LONS  = lonCtr,    &
                                   LATS  = latCtr,    &
                                   ORBIT = sunOrbit,  &
                                   ZTH   = ZTH,       &
                                   SLR   = SLR,       &
                                   CLOCK = Clock,     &
                                   __RC__            )

       ! Pass to HEMCO
       CALL HCO_ArrAssert( ExtState%SUNCOS%Arr, HcoState%NX, HcoState%NY, STAT )
       ASSERT_(RC==HCO_SUCCESS)
       ExtState%SUNCOS%Arr%Val(:,:) = ZTH(:,:)

       ! Cleanup
       DEALLOCATE(ZTH,SLR)

!       CALL HCO_GetSUNCOS( am_I_Root, HcoState, ExtState%SUNCOS%Arr%Val, 0, STAT )
!       ASSERT_(STAT==HCO_SUCCESS)
    ENDIF

    ! FRAC_OF_PBL
    IF ( ExtState%FRAC_OF_PBL%DoUse ) THEN
       IF(am_I_Root) WRITE(*,*) 'HEMCO field FRAC_OF_PBL not defined!'
       ASSERT_(.FALSE.)
    ENDIF

    ! PBL_MAX: currently undefined
    ExtState%PBL_MAX => NULL()

    ! Check for vector DRYCOEFF
    IF ( ExtState%WET_TOTN%DoUse .OR. ExtState%DRY_TOTN%DoUse ) THEN
       IF ( .NOT. ASSOCIATED(ExtState%DRYCOEFF) ) THEN
          IF(am_I_Root) WRITE(*,*) 'HEMCO vector DRYCOEFF not defined!'
          ASSERT_(.FALSE.)
       ENDIF
    ENDIF

    ! ---------------------------------------------------------------- 
    ! Cleanup 
!    TH       => NULL()
    PLE      => NULL()
    ZLE      => NULL()
    Q        => NULL()
    PS       => NULL()
    AREA     => NULL()
    HcoState => NULL()
    ExtState => NULL()

    ! Return w/ success
    RETURN_(ESMF_SUCCESS)

    END SUBROUTINE SetExtFields 
!EOC
!------------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1          !
!------------------------------------------------------------------------------
!BOP
!
! !ROUTINE: NewInst_ 
!
! !DESCRIPTION: Subroutine NewInst\_ creates a new HEMCO instance. 
!\\
!\\
! !INTERFACE:
!
    SUBROUTINE NewInst_ ( Inst, RC ) 
!
! !USES:
!
!
! !INPUT PARAMETERS:
!
!                                                             
! !INPUT/OUTPUT PARAMETERS:                                   
!
!                                                             
! !OUTPUT PARAMETERS:                                   
!
    TYPE(Instance),      POINTER               :: Inst           ! pointer to new instance 
    INTEGER,             INTENT(  OUT)         :: RC             ! Return code
!
! !REVISION HISTORY:
!  22 Feb 2016 - C. Keller - Initial version 
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    TYPE(Instance), POINTER   :: NewInst => NULL()

    __Iam__('NewInst_ (GEOS_EmisGridComp.F90)')

    ! Initialize new instance 
    ALLOCATE(NewInst)
    NewInst%HcoConfig => NULL()
    NewInst%HcoState  => NULL()
    NewInst%ExtState  => NULL()

    ! Add to linked list (place at beginning)
    NewInst%NextInst  => Instances
    Instances         => NewInst

    ! Connect return pointer
    Inst => NewInst

    ! Return w/ success
    RETURN_(ESMF_SUCCESS)

    END SUBROUTINE NewInst_ 
!EOC
END MODULE HEMCO_GridCompMod
