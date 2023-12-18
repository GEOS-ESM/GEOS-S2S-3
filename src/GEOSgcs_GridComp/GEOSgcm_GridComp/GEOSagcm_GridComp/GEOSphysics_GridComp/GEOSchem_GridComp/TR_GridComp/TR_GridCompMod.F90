#include "MAPL_Generic.h"

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1     !
!                  and                                                   !
!                Atmospheric Chemistry and Dynamics Lab,  Code 614       !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: TR_GridCompMod - Implements Passive Tracers
!
! !INTERFACE:
!
   MODULE TR_GridCompMod
!
! !USES:
!
   USE ESMF
   USE MAPL_Mod

   USE MAPL_SimpleBundleMod

   USE Chem_RegistryMod
   USE Chem_UtilMod       ! I/O

   USE m_inpak90             ! Resource file management
   USE m_die,   ONLY: die
   USE m_chars, ONLY: lowercase,uppercase

   USE MAPL_NewArthParserMod,  ONLY: MAPL_StateEval      ! for evaluating constraints

   USE   DryDepositionGmiMod,  ONLY: DryDepositionGMI         ! in this dir
   USE      WetRemovalGmiMod,  ONLY:    WetRemovalGMI         ! in this dir
   USE         TR_GravSetMod,  ONLY: TR_GravitationalSettling ! in this dir

   USE             VegLaiMod,  ONLY: Decode_Land_Types, Decode_XLAI  ! in Chem_Shared

   IMPLICIT NONE
   PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:

   PUBLIC SetServices
!
! !DESCRIPTION: 
!
!  {\tt TR\_GridComp} is an ESMF gridded component implementing
!  passive tracers.
!
!  Developed for GEOS-5 release Ganymed 4 beta 11 and later.
!
! !REVISION HISTORY:
!
!  06Dec2009  da Silva     Created the MATRIX skeleton.
!  20May2014  Manyin       Initial version of TR in Ganymed, similar to MAMchem_GridCompMod.F90
!  19aug2014  Manyin       Added WetDep, DryDep, Emissions constraints; traded I90 for ESMF calls.
!   7jul2015  Manyin       Added run_order to replace spaghetti.
!  27may2016  Manyin       ExtData is now used for 2D and 3D source files, veg/lai files, and masks.
!
!EOP
!-------------------------------------------------------------------------

! For use in the run_order CASE statement:
#define     NO_PHASE 0
#define SOURCE_PHASE 1
#define   SINK_PHASE 2


! A set of values common to all passive tracers
! ---------------------------------------------
  TYPE TR_TracerKit
    INTEGER                            :: tr_count           ! number of passive tracers
    INTEGER                            :: im,jm,km           ! global max values
    INTEGER                            :: i1,i2,j1,j2        ! local index bounds
    REAL*8, POINTER, DIMENSION(:,:)    :: lats  => NULL()    ! Latitudes in degrees
    REAL*8, POINTER, DIMENSION(:,:)    :: lons  => NULL()    ! Longitudes in degrees, 0 to 360
    REAL                               :: ptop               ! top pressure in Pa
    ! Only if needed for Dry Deposition:
    INTEGER, POINTER, DIMENSION(:,:)   :: ireg  => NULL()    ! Land type counts
    INTEGER, POINTER, DIMENSION(:,:,:) :: iland => NULL()    ! Land types
    INTEGER, POINTER, DIMENSION(:,:,:) :: iuse  => NULL()    ! Land type percentages
    REAL*8,  POINTER, DIMENSION(:,:,:) :: xlai  => NULL()    ! Land type LeafAreaIndex vals
    LOGICAL                            :: veg_fraction_done  ! Veg fraction only to be read once
    TYPE(ESMF_Grid)                    :: grid_esmf          ! Grid
  END TYPE TR_TracerKit

! Parameters for a 2D (surface) constraint
! ----------------------------------------
  TYPE TR_SurfaceConstraintSpec
    CHARACTER(LEN=20)  :: mode            ! set or scale

    LOGICAL            :: use_regions
    CHARACTER(LEN=255) :: regions_str     ! one or more indices

    LOGICAL            :: use_bool
    CHARACTER(LEN=255) :: bool_FIELD      ! field name
    CHARACTER(LEN=10 ) :: bool_OP         ! (LT, GT, EQ, NE, LE, GE)
    REAL*4             :: bool_SCALAR

    CHARACTER(LEN=255) :: expr            ! expression involving scalars and fields

    LOGICAL, POINTER, DIMENSION(:,:) ::  bool_array  => NULL() ! set or scale where TRUE
                                                               ! region list and bool expr determine this

    LOGICAL, POINTER, DIMENSION(:,:) ::  bool_aux    => NULL() ! scratch space

    REAL*4,  POINTER, DIMENSION(:,:) :: value_array  => NULL() ! set or scale w/ these values
                                                               ! value or expr determine this
  END TYPE TR_SurfaceConstraintSpec


! Parameters to specify a single tracer
! -------------------------------------
  TYPE TR_TracerSpec

        CHARACTER(LEN=255) :: name            ! name of the tracer 
        CHARACTER(LEN=255) :: units           ! units for the tracer 
        CHARACTER(LEN=255) :: iname           ! instance name
        CHARACTER(LEN=255) :: rcfilen         ! resource file name
        CHARACTER(LEN=255) :: regions_ExtData_entry         ! Dataset that divides the globe into regions
        REAL, POINTER      :: regions_array(:,:) => NULL()  ! Fill with GetPointer
        CHARACTER(LEN=255) :: regionsString   ! Comma-delimited string of regions
        CHARACTER(LEN=255) :: emisFileName    ! TRACER emission file name

        INTEGER :: instance                   ! instance number

        REAL :: decayConstant                 ! Decay constant, inverse seconds.
        REAL :: emission                      ! kg m^{-2} s^{-1}
        CHARACTER(LEN=255) :: decays_to       ! Name of the Passive Tracer produced by the decay
        CHARACTER(LEN=255) :: loss_species    ! Chemical loss rate of this species will be the sink

        REAL, POINTER :: regionMask(:,:)   => NULL() ! regional mask  - TO USE, alloc same as TRsfcFlux
        REAL, POINTER ::  TRsfcFlux(:,:)   => NULL() ! TR surface flux kg m^-2 s^-1
        REAL, POINTER ::  TRvolFlux(:,:,:) => NULL() ! TR 3D volume flux mixrat/sec, or kg m^-2 s^-1
        REAL, POINTER :: tropp_prev(:,:)   => NULL() ! Tropopause pressure from previous timestep

        INTEGER :: run_order(2)               ! order to do SOURCE_PHASE and SINK_PHASE

        LOGICAL            :: src_add         ! T- add values; F- replace values
        CHARACTER(LEN=255) :: src_mode        ! Source mode of tracer
        REAL :: src_value                     ! Source constant, parts per part

        CHARACTER(LEN=255) :: src_horiz       ! Horizontal spec of tracer source
        LOGICAL, POINTER :: src_mask_horiz(:,:) => NULL()  ! regional mask

        CHARACTER(LEN=255) :: src_vert        ! Vertical spec of tracer source
        INTEGER :: src_lev1, src_lev2         ! Level bounds for tracer source
        REAL    :: src_press1, src_press2     ! Pressure bounds for tracer source

        CHARACTER(LEN=255) :: src_field_name  ! Field to copy from, as source

        CHARACTER(LEN=255) :: snk_mode        ! Sink mode of tracer
        REAL :: snk_value                     ! Sink constant, parts per part

        CHARACTER(LEN=255) :: snk_horiz       ! Horizontal spec of tracer sink
        LOGICAL, POINTER :: snk_mask_horiz(:,:) => NULL()    ! regional mask

        CHARACTER(LEN=255) :: snk_vert        ! Vertical spec of tracer sink
        INTEGER :: snk_lev1, snk_lev2         ! Level bounds for tracer sink
        REAL    :: snk_press1, snk_press2     ! Pressure bounds for tracer sink

        REAL               :: mw              ! molecular weight

        ! Dry Dep
        LOGICAL            :: dry_deposition  ! when true, do Gravitational Settling and Dry Deposition
        REAL               :: aero_density    ! see examples in gmi_aerosol.h
        REAL               :: aero_eff_radius ! see examples in gmi_aerosol.h
        REAL               :: aero_c(1:4)     ! see examples in gmi_aerosol.h

        ! Wet Dep
        LOGICAL            :: wet_removal     ! when true, do rainout, washout & wet deposition
        REAL               :: rel_scav_eff    ! scavenging efficiency relative to sulfate (unitless)
        REAL               :: retention_eff   ! retention efficiency
        LOGICAL            :: h2o2_flag       ! if the species should be treated as H2O2
        LOGICAL            :: hno3_flag       ! if the species should be treated as HNO3

        ! Dry & Wet Dep
        REAL               :: hstar           ! a parameter for computing Henry's law constant
        REAL               :: delH_298_over_R ! a parameter for computing Henry's law constant
        LOGICAL            :: aero_flag       ! treat as an aerosol for the purposes of washout and reevaporation
        REAL               :: f0              ! reactivity factor for oxidation of biological substances


        INTEGER            :: surface_constraint_count = 0   ! constraints on SRC
        TYPE(TR_SurfaceConstraintSpec), POINTER, DIMENSION(:) :: constraints => NULL()

!       REAL, POINTER :: src_3d_array(:,:,:) => NULL() ! For reading 3D file, or if VERT depends on P

  END TYPE TR_TracerSpec


! Legacy state
! ------------
  TYPE TR_State

     PRIVATE
     type(ESMF_Config)           :: CF                ! Private Config

     type(ESMF_Grid)             :: grid              ! Grid

     type(Chem_Registry), pointer:: chemReg => NULL()

     type(MAPL_SimpleBundle)     :: qa                ! Passive tracers

     real                        :: dt                ! Model time step

     type(TR_TracerKit),  pointer:: kit     => NULL() ! Set of values common to all tracers

     type(TR_TracerSpec), pointer:: spec(:) => NULL() ! Tracer specific values

!    integer                     :: model             ! MAM7 or MAM3
!    type(MAM_MetaSpec)          :: meta              ! MAM meta data

!    real                        :: femisSS           ! Seasalt emission tuning parameter
!    real                        :: femisDU           ! Dust emission tuning parameter

!    logical                     :: dry_removal       ! turn on/off dry removal processes
!    logical                     :: wet_removal       ! turn on/off wet removal processes
!    logical                     :: nucleation        ! turn on/off nucleation process
!    logical                     :: condensation      ! turn on/off condensation process
!    logical                     :: coagulation       ! turn on/off coagulation process

     logical                     :: verbose           ! turn on/off more verbose messages

     integer                     :: pet               ! ID of the persistent execution thread

  END TYPE TR_State

! Hook for the ESMF
! -----------------
  TYPE TR_Wrap
     TYPE (TR_State), pointer :: PTR => null()
  END TYPE TR_Wrap

  INTEGER, PARAMETER :: DBL = KIND(0.00D+00)


! Constant string
#define NO_DECAY '<no_specific_species>'
#define NULL_REGION_MASK '<no_mask>'


CONTAINS


!-------------------------------------------------------------------------
!     NASA/GSFC, Atmospheric Chemistry and Dynamics Lab,  Code 614       !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: SetServices --- Sets IRF services for the TR Grid Component
!
! !INTERFACE:

   SUBROUTINE SetServices ( GC, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
    integer, optional                  :: RC  ! return code

! !DESCRIPTION: Sets Initialize, Run and Finalize services. 
!
! !REVISION HISTORY:
!
! 20May2014  Manyin    First crack.
!
!EOP
!-------------------------------------------------------------------------

                            __Iam__('SetServices')

!   Local derived type aliases
!   --------------------------
    type (TR_State), pointer       :: myState   ! internal state
    type (TR_Wrap)                 :: wrap
    type (Chem_Registry), pointer  :: r
    TYPE (ESMF_VM)                 :: vm

    character(len=ESMF_MAXSTR) :: comp_name

!   Local variables
!   --------------------------
!   logical                        :: flagMAM7, flagMAM3
    integer                        :: n
    integer                        :: petID
    character(len=ESMF_MAXSTR)     :: rcfilen
    character(len=ESMF_MAXSTR)     :: src_mode
    character(len=ESMF_MAXSTR)     :: regions_ExtData_entry
    logical                        :: dry_dep
    logical                        :: lai_needed

    character(len=ESMF_MAXSTR), allocatable   :: str_array(:)  ! to hold unique ExtData entries
    integer                        :: s_count                  ! number of entries in str_array


!                              ------------

!   Get my name and set-up traceback handle
!   ---------------------------------------
    call ESMF_GridCompGet( GC, name=comp_name, __RC__ )
    Iam = TRIM(comp_name) // '::' // trim(Iam)

!   if (MAPL_AM_I_ROOT()) then
!      print *, trim(Iam)//': setting up...'
!      print *, ''
!   end if

!   Wrap internal state for storing in GC; rename legacyState
!   -------------------------------------
    allocate ( myState, __STAT__ )
    wrap%ptr => myState


!   Identify the PET (persistent execution thread) ID
!   -------------------------------------------------
    call ESMF_VMGetCurrent(vm, __RC__ )
    call ESMF_VMGet(vm, localPet=petID, __RC__ )
    myState%pet = petID


    allocate ( myState%kit, __STAT__ )

!   Start by loading the Chem Registry.
!   This duplicates the approach in GMI and GOCART.
!   It might be better to break off the Passive Tracers into a
!    separate file, but for now use the Chem Registry. (MeM 5.27.14)
!   ----------------------------------
    allocate ( myState%chemReg, __STAT__ )
    myState%chemReg = Chem_RegistryCreate ( STATUS )
    VERIFY_(STATUS)
    r => myState%chemReg   ! short-hand

!   Load private Config Attributes
!   ------------------------------
    myState%CF = ESMF_ConfigCreate(__RC__)

! CAN WE USE esmf ROUTINES HERE?  OR i90 ROUTINES???

!   call ESMF_ConfigLoadFile ( myState%CF, 'MAMchem_GridComp.rc', __RC__ )

!   call ESMF_ConfigGetAttribute ( myState%CF, myState%verbose,      Label='verbose:',       default=.false.,  __RC__ )

!   call ESMF_ConfigGetAttribute ( myState%CF, flagMAM7,          Label='doing_MAM7:',    default=.true. ,  __RC__ )
!   call ESMF_ConfigGetAttribute ( myState%CF, flagMAM3,          Label='doing_MAM3:',    default=.false.,  __RC__ )

!   call ESMF_ConfigGetAttribute ( myState%CF, myState%dry_removal,  Label='dry_removal:',   default=.true.,   __RC__ )
!   call ESMF_ConfigGetAttribute ( myState%CF, myState%wet_removal,  Label='wet_removal:',   default=.true.,   __RC__ )
!   call ESMF_ConfigGetAttribute ( myState%CF, myState%nucleation,   Label='nucleation:',    default=.true.,   __RC__ )
!   call ESMF_ConfigGetAttribute ( myState%CF, myState%condensation, Label='condensation:',  default=.true.,   __RC__ )
!   call ESMF_ConfigGetAttribute ( myState%CF, myState%coagulation,  Label='coagulation:',   default=.true.,   __RC__ )

!   call ESMF_ConfigGetAttribute ( myState%CF, myState%femisSS,      Label='seasalt_femis:', default=1.0,      __RC__ )
!   call ESMF_ConfigGetAttribute ( myState%CF, myState%femisDU,      Label='dust_femis:',    default=1.0,      __RC__ )


!   ASSERT_(flagMAM3 /= flagMAM7)

!   if (flagMAM7) then
!       myState%model = MAM7_MODEL
!   else
!       myState%model = MAM3_MODEL
!   end if


!                       ------------------------
!                       ESMF Functional Services
!                       ------------------------

!   Set the Initialize, Run, Finalize entry points
!   ----------------------------------------------
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_INITIALIZE,  Initialize_, __RC__ )
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN,         Run_,        __RC__ )
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_FINALIZE,    Finalize_,   __RC__ )
        
!   Store internal state in GC
!   --------------------------
    call ESMF_UserCompSetInternalState ( GC, 'TR_STATE', wrap, STATUS )
    VERIFY_(STATUS)
  
!                         ------------------
!                         MAPL Data Services
!                         ------------------

!BOS
!
! These .h files are generated from a Registry, by acg.pl
!
! !IMPORT STATE:

!   #include "MAMchem_ImportSpec___.h"
!!  The TR version could include calls like these:
!!  (these are copied from GOCART_GridCompMod.F90)

!   TROPP - Connectivity from SDYN to PHYS is TROPP_BLENDED to TROPP
!   ----------------------------------------------------------------
    call MAPL_AddImportSpec(GC,                                    &
        SHORT_NAME         = 'TROPP',                              &
        LONG_NAME          = 'tropopause_pressure_based_on_blended_estimate', &
        UNITS              = 'Pa',                                 &
        DEFAULT            = MAPL_UNDEF,                           &
        DIMS               = MAPL_DimsHorzOnly,                    &
        VLOCATION          = MAPL_VLocationNone,                   &
                                                       __RC__ )

!   PLE - Pressure at the edges
!   ----------------------------------------------------------------
    call MAPL_AddImportSpec(GC,                                    &
         SHORT_NAME = 'PLE',                                       &
         LONG_NAME  = 'air_pressure',                              &
         UNITS      = 'Pa',                                        &
         DEFAULT    = MAPL_UNDEF,                                  &
         DIMS       =  MAPL_DimsHorzVert,                          &
         VLOCATION  =  MAPL_VLocationEdge,                         &
                                                       __RC__ )

!   ZLE - Height at the edges
!   ----------------------------------------------------------------
    call MAPL_AddImportSpec(GC,                                    &
         SHORT_NAME = 'ZLE',                                       &
         LONG_NAME  = 'geopotential_height',                       &
         UNITS      = 'm',                                         &
         DEFAULT            = MAPL_UNDEF,                          &
         DIMS       =  MAPL_DimsHorzVert,                          &
         VLOCATION  =  MAPL_VLocationEdge,                         &
                                                       __RC__ )

!   T - Air Temperature
!   ----------------------------------------------------------------
    call MAPL_AddImportSpec(GC,                                    &
         SHORT_NAME = 'T',                                         &
         LONG_NAME  = 'air_temperature',                           &
         UNITS      = 'K',                                         &
         DEFAULT    = MAPL_UNDEF,                                  &
         DIMS       = MAPL_DimsHorzVert,                           &
         VLOCATION  = MAPL_VLocationCenter,                        &
                                                       __RC__ )

!   AIRDENS - Air density
!   ----------------------------------------------------------------
    call MAPL_AddImportSpec(GC,                                    &
        SHORT_NAME         = 'AIRDENS',                            &
        LONG_NAME          = 'air_density',                        &
        UNITS              = 'kg m-3',                             &
!ALT        DEFAULT            = MAPL_UNDEF,                       &
        DIMS               = MAPL_DimsHorzVert,                    &
        VLOCATION          = MAPL_VLocationCenter,     __RC__ )

!   AIRDENS_DRY - Dry air density
!   ----------------------------------------------------------------
    call MAPL_AddImportSpec(GC,                                    &
        SHORT_NAME         = 'AIRDENS_DRY',                        &
        LONG_NAME          = 'dry_air_density',                    &
        UNITS              = 'kg m-3',                             &
!ALT        DEFAULT            = MAPL_UNDEF,                       &
        DIMS               = MAPL_DimsHorzVert,                    &
        VLOCATION          = MAPL_VLocationCenter,     __RC__ )

!   AREA - Cell area
!   ----------------------------------------------------------------
    call MAPL_AddImportSpec(GC,                                    &
        SHORT_NAME         = 'AREA',                               &
        LONG_NAME          = 'agrid_cell_area',                    &
        UNITS              = 'm+2',                                &
        DIMS               = MAPL_DimsHorzOnly,                    &
        VLOCATION          = MAPL_VLocationNone,                   &
                                                       __RC__ )

!   Q - Specific Humidity
!   ----------------------------------------------------------------
     call MAPL_AddImportSpec(GC,                                   &
        SHORT_NAME         = 'Q',                                  &
        LONG_NAME          = 'specific_humidity',                  &
        UNITS              = 'kg kg-1',                            &
        DIMS               = MAPL_DimsHorzVert,                    &
        VLOCATION          = MAPL_VLocationCenter,                 &
                                                       __RC__  )

     call MAPL_AddImportSpec(GC,                                   &
        SHORT_NAME         = 'LWI',                                &
        LONG_NAME          = 'land-ocean-ice_mask',                &
        UNITS              = '1',                                  &
        DIMS               = MAPL_DimsHorzOnly,                    &
        VLOCATION          = MAPL_VLocationNone,                   &
                                                       __RC__  )

     call MAPL_AddImportSpec(GC,                                   &
        SHORT_NAME         = 'T2M',                                &
        LONG_NAME          = '2-meter_air_temperature',            &
        UNITS              = 'K',                                  &
        DIMS               = MAPL_DimsHorzOnly,                    &
        VLOCATION          = MAPL_VLocationNone,                   &
                                                       __RC__  )
     call MAPL_AddImportSpec(GC,                                   &
        SHORT_NAME         = 'CLDTT',                              &
        LONG_NAME          = 'total_cloud_area_fraction',          &
        UNITS              = '1',                                  &
        DIMS               = MAPL_DimsHorzOnly,                    &
        VLOCATION          = MAPL_VLocationNone,                   &
                                                       __RC__  )
     call MAPL_AddImportSpec(GC,                                   &
        SHORT_NAME         = 'Z0H',                                &
        LONG_NAME          = 'surface_roughness_for_heat',         &
        UNITS              = 'm',                                  &
        DIMS               = MAPL_DimsHorzOnly,                    &
        VLOCATION          = MAPL_VLocationNone,                   &
                                                       __RC__  )
     call MAPL_AddImportSpec(GC,                                   &
        SHORT_NAME         = 'SWNDSRF',                            &
        LONG_NAME          = 'surface_net_downward_shortwave_flux',&
        UNITS              = 'W m-2',                              &
        DIMS               = MAPL_DimsHorzOnly,                    &
        VLOCATION          = MAPL_VLocationNone,                   &
                                                       __RC__  )
     call MAPL_AddImportSpec(GC,                                   &
        SHORT_NAME         = 'USTAR',                              &
        LONG_NAME          = 'surface_velocity_scale',             &
        UNITS              = 'm s-1',                              &
        DIMS               = MAPL_DimsHorzOnly,                    &
        VLOCATION          = MAPL_VLocationNone,                   &
                                                       __RC__  )
!    FRACI
!    -----
     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'FRACI',                             &
        LONG_NAME          = 'ice_covered_fraction_of_tile',      &
        UNITS              = '1',                                 &
!ALT        DEFAULT            = MAPL_UNDEF,                          &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       __RC__  )
!    TSOIL1, from SURFACE
!    --------------------
     call MAPL_AddImportSpec(GC,                             &
        SHORT_NAME         = 'TSOIL1',                            &
        LONG_NAME          = 'soil_temperatures_layer_1',         &
        UNITS              = 'K',                                 &
!ALT        DEFAULT   = MAPL_UNDEF,                                   &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       __RC__  )

     call MAPL_AddImportSpec(GC,                                         &
        SHORT_NAME         = 'DQDT',                                     &
        LONG_NAME          = 'specific_humidity_tendency_due_to_moist',  &
        UNITS              = 's-1',                                      &
        DIMS               = MAPL_DimsHorzVert,                          &
        VLOCATION          = MAPL_VLocationCenter,                       &
                                                       __RC__  )
     VERIFY_(STATUS)


     call MAPL_AddImportSpec(GC,                                            &
        SHORT_NAME         = 'PFL_CN',                                      &
        LONG_NAME          = '3D_flux_of_liquid_convective_precipitation',  &
        UNITS              = 'kg m-2 s-1',                                  &
        DIMS               = MAPL_DimsHorzVert,                             &
        VLOCATION          = MAPL_VLocationEdge,                            &
                                                       __RC__  )
     VERIFY_(STATUS)


     call MAPL_AddImportSpec(GC,                                               &
        SHORT_NAME         = 'PFL_LSAN',                                       &
        LONG_NAME          = '3D_flux_of_liquid_nonconvective_precipitation',  &
        UNITS              = 'kg m-2 s-1',                                     &
        DIMS               = MAPL_DimsHorzVert,                                &
        VLOCATION          = MAPL_VLocationEdge,                               &
                                                       __RC__  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                   &
        SHORT_NAME         = 'ASNOW',                              &
        LONG_NAME          = 'fractional_area_of_land_snowcover',  &
        UNITS              = '1',                                  &
        DIMS               = MAPL_DimsHorzOnly,                    &
        VLOCATION          = MAPL_VLocationNone,                   &
                                                       __RC__  )

    call MAPL_AddImportSpec(GC,                                    &
       SHORT_NAME         = 'FRLANDICE',                           &
       LONG_NAME          = 'fraction_of_land_ice',                &
       UNITS              = '1',                                   &
       DIMS               = MAPL_DimsHorzOnly,                     &
       VLOCATION          = MAPL_VLocationNone,                    &
                                                       __RC__  )
!! --------------------------------------
!! Some Chemical Species will need to be IMPORTED here...
!! To really do this correctly we will parse the .rc files, and
!! keep a list of unique names of imports; long name and units will get filler values?

! Got this from GMICHEM_InternalSpec___.h

  IF ( r%doing_GMI ) THEN

     call MAPL_AddImportSpec(GC, &
        SHORT_NAME         = 'OX',  &
        LONG_NAME          = 'Ozone',  &
        UNITS              = 'mol mol-1', &
        DIMS               = MAPL_DimsHorzVert,    &
        VLOCATION          = MAPL_VLocationCenter,    &
                                                       __RC__  )

! Adapted these from Reactions_ExportSpec___.h

     call MAPL_AddImportSpec(GC,  &
        SHORT_NAME         = 'QQK007',  &
        LONG_NAME          = 'reaction_rate: H2O + O1D = 2 OH',  &
        UNITS              = 'mole m-3 s-1', &
        DIMS               = MAPL_DimsHorzVert,    &
        VLOCATION          = MAPL_VLocationCenter,    &
                                                       __RC__  )

     call MAPL_AddImportSpec(GC,  &
        SHORT_NAME         = 'QQK027',  &
        LONG_NAME          = 'reaction_rate: O3 + OH = HO2 + O2',  &
        UNITS              = 'mole m-3 s-1', &
        DIMS               = MAPL_DimsHorzVert,    &
        VLOCATION          = MAPL_VLocationCenter,    &
                                                       __RC__  )

     call MAPL_AddImportSpec(GC,  &
        SHORT_NAME         = 'QQK028',  &
        LONG_NAME          = 'reaction_rate: HO2 + O3 = 2 O2 + OH',  &
        UNITS              = 'mole m-3 s-1', &
        DIMS               = MAPL_DimsHorzVert,    &
        VLOCATION          = MAPL_VLocationCenter,    &
                                                       __RC__  )
     call MAPL_AddImportSpec(GC,  &
        SHORT_NAME         = 'DD_OX',  &
        LONG_NAME          = 'dry_deposition_of_OX',  &
        UNITS              = 'kg m-2 s-1', &
        DIMS               = MAPL_DimsHorzOnly,    &
        VLOCATION          = MAPL_VLocationNone,    &
                                                       __RC__  )

     call MAPL_AddImportSpec(GC,  &
        SHORT_NAME         = 'QQK005',  &
        LONG_NAME          = 'reaction_rate: O1D + O3 = 2 O2',  &
        UNITS              = 'mole m-3 s-1', &
        DIMS               = MAPL_DimsHorzVert,    &
        VLOCATION          = MAPL_VLocationCenter,    &
                                                       __RC__  )

     call MAPL_AddImportSpec(GC,  &
        SHORT_NAME         = 'QQK204',  &
        LONG_NAME          = 'reaction_rate: O3 + PRPE = 0.50 ALD2 + 0.54 CH2O + 0.42 CO + 0.06 H2 +',  &
        UNITS              = 'mole m-3 s-1', &
        DIMS               = MAPL_DimsHorzVert,    &
        VLOCATION          = MAPL_VLocationCenter,    &
                                                       __RC__  )

     call MAPL_AddImportSpec(GC,  &
        SHORT_NAME         = 'QQK222',  &
        LONG_NAME          = 'reaction_rate: ISOP + O3 = 0.90 CH2O + 0.05 CO + 0.06 HO2 + 0.39 MACR +',  &
        UNITS              = 'mole m-3 s-1', &
        DIMS               = MAPL_DimsHorzVert,    &
        VLOCATION          = MAPL_VLocationCenter,    &
                                                       __RC__  )

     call MAPL_AddImportSpec(GC,  &
        SHORT_NAME         = 'QQK223',  &
        LONG_NAME          = 'reaction_rate: MVK + O3 = 0.04 ALD2 + 0.80 CH2O + 0.05 CO + 0.06 HO2 +',  &
        UNITS              = 'mole m-3 s-1', &
        DIMS               = MAPL_DimsHorzVert,    &
        VLOCATION          = MAPL_VLocationCenter,    &
                                                       __RC__  )

     call MAPL_AddImportSpec(GC,  &
        SHORT_NAME         = 'QQK224',  &
        LONG_NAME          = 'reaction_rate: MACR + O3 = 0.70 CH2O + 0.20 CO + 0.28 HO2 + 0.80 MGLY +',  &
        UNITS              = 'mole m-3 s-1', &
        DIMS               = MAPL_DimsHorzVert,    &
        VLOCATION          = MAPL_VLocationCenter,    &
                                                       __RC__  )

     call MAPL_AddImportSpec(GC,  &
        SHORT_NAME         = 'QQK253',  &
        LONG_NAME          = 'reaction_rate: IALD + O3 = 0.12 CH2O + 0.28 GLYC + 0.20 GLYX + 0.20 HAC',  &
        UNITS              = 'mole m-3 s-1', &
        DIMS               = MAPL_DimsHorzVert,    &
        VLOCATION          = MAPL_VLocationCenter,    &
                                                       __RC__  )
  END IF

!! --------------------------------------

!EOS

!!
!!  Add IMPORT specs for tracers that rely on 2D or 3D files
!!  Also add IMPORT specs for unique ExtData region masks
!!  Conditionally add IMPORT specs for VEG and LAI fractions
!!
    lai_needed = .FALSE.

    allocate ( str_array( r%j_TR - r%i_TR + 1), __STAT__ )
    s_count = 0

    do n = r%i_TR, r%j_TR

     rcfilen = 'TR_GridComp---' // TRIM(r%vname(n)) // '.rc'

     call ESMF_ConfigLoadFile ( myState%CF, TRIM(rcfilen), __RC__ )

     call ESMF_ConfigGetAttribute ( myState%CF, label='src_mode:', value=src_mode, default='NO_SRC',  __RC__ )

     IF( TRIM(src_mode) == "file2d" ) THEN
       call MAPL_AddImportSpec(GC,                                    &
            SHORT_NAME = 'SRC_2D_'//TRIM(r%vname(n)),                 &
            LONG_NAME  = 'source_values_for_'//TRIM(r%vname(n)),      &
!            UNITS      = 'units',                                     &
            DIMS        = MAPL_DimsHorzOnly,                          &
            RESTART     = MAPL_RestartSkip,                           &
            VLOCATION   = MAPL_VLocationNone,                         &
                                                          __RC__ )
     END IF

     IF( TRIM(src_mode) == "file3d" ) THEN
       call MAPL_AddImportSpec(GC,                                    &
            SHORT_NAME = 'SRC_3D_'//TRIM(r%vname(n)),                 &
            LONG_NAME  = 'source_values_for_'//TRIM(r%vname(n)),      &
!            UNITS      = 'units',                                     &
            DIMS       = MAPL_DimsHorzVert,                           &
            RESTART    = MAPL_RestartSkip,                            &
            VLOCATION  = MAPL_VLocationCenter,                        &
                                                          __RC__ )
     END IF

     call ESMF_ConfigGetAttribute ( myState%CF, label='dry_deposition:', value=dry_dep, default=.FALSE., __RC__ )
     IF ( dry_dep )  lai_needed = .TRUE.

!    Identify UNIQUE Region Mask entries in ExtData
     call ESMF_ConfigGetAttribute ( myState%CF, label='regions_ExtData_entry:', value=regions_ExtData_entry, default=NULL_REGION_MASK, __RC__ )
     IF ( regions_ExtData_entry /= NULL_REGION_MASK ) THEN
       IF ( s_count == 0 ) THEN
         s_count = s_count+1
         str_array(s_count) = regions_ExtData_entry
       ELSE
         IF ( .NOT. ANY(str_array(1:s_count) == regions_ExtData_entry) ) THEN
           s_count = s_count+1
           str_array(s_count) = regions_ExtData_entry
         END IF
       END IF
     END IF

    end do

    IF ( lai_needed ) THEN

      call MAPL_AddImportSpec(GC,                                    &
           SHORT_NAME = 'TR_VEG_FRAC',                               &
           LONG_NAME  = 'vegetation_fraction',                       &
!          UNITS      = '1',                                         &
           RESTART    = MAPL_RestartSkip,                            &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationCenter,                        &
                                                         __RC__ )

      call MAPL_AddImportSpec(GC,                                    &
           SHORT_NAME = 'TR_LAI_FRAC',                               &
           LONG_NAME  = 'leaf_area_index',                           &
!          UNITS      = '1',                                         &
           RESTART    = MAPL_RestartSkip,                            &
           DIMS       = MAPL_DimsHorzVert,                           &
           VLOCATION  = MAPL_VLocationCenter,                        &
                                                         __RC__ )

    END IF

!   Import the Region Masks
    DO n = 1, s_count

       call MAPL_AddImportSpec(GC,                                    &
            SHORT_NAME = str_array(n),                                &
            LONG_NAME  = 'TR Region Mask',                            &
!            UNITS      = 'units',                                     &
            DIMS        = MAPL_DimsHorzOnly,                          &
            VLOCATION   = MAPL_VLocationNone,                         &
            RESTART     = MAPL_RestartSkip,                           &
                                                          __RC__ )

    END DO





! !INTERNAL STATE:

!   #include "MAMchem_InternalSpec___.h"
!!  example:

!!
!!  Add the 3D Tracer fields listed in the Registry, to the INTERNAL state
!!
    do n = r%i_TR, r%j_TR

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = TRIM(r%vname(n)),               &
        LONG_NAME          = TRIM(r%vtitle(n)),              &
        UNITS              = TRIM(r%vunits(n)),              &
        ADD2EXPORT         = .TRUE.,                         &
        DIMS               = MAPL_DimsHorzVert,              &
        VLOCATION          = MAPL_VLocationCenter,           &
        FRIENDLYTO         = 'DYNAMICS:TURBULENCE:MOIST',    &
                         __RC__ )

    end do


! !EXTERNAL STATE:

!   #include "MAMchem_ExportSpec___.h"
!!  example:
!    call MAPL_AddExportSpec(GC,  &
!       SHORT_NAME         = 'SSMASS',  &
!       LONG_NAME          = 'Sea Salt Mass Mixing Ratio',  &
!       UNITS              = 'kg kg-1', &
!       DIMS               = MAPL_DimsHorzVert,    &
!       VLOCATION          = MAPL_VLocationCenter,    &
!                                                      __RC__ )



!   This bundle is not filled in by MAM, just a place holder for now
!   ----------------------------------------------------------------
!   call MAPL_AddExportSpec(GC,                                   &
!        SHORT_NAME         = 'AERO',                             &
!        LONG_NAME          = 'aerosol_mass_mixing_ratios',       &
!        UNITS              = 'kg kg-1',                          &
!        DIMS               = MAPL_DimsHorzVert,                  &
!        VLOCATION          = MAPL_VLocationCenter,               &
!        DATATYPE           = MAPL_BundleItem,                    &
!                                                      RC=STATUS  )
!   VERIFY_(STATUS)



!   Generic Set Services
!   --------------------
    call MAPL_GenericSetServices ( GC, __RC__ )


!   Finish up
!   ---------
    deallocate ( str_array )


!   All done
!   --------

    RETURN_(ESMF_SUCCESS)

  END SUBROUTINE SetServices


!-------------------------------------------------------------------------
!     NASA/GSFC, Atmospheric Chemistry and Dynamics Lab,  Code 614       !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Initialize_ --- Initialize TR
!
! !INTERFACE:
!

   SUBROUTINE Initialize_ ( GC, IMPORT, EXPORT, CLOCK, RC )

! !USES:

   implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout) :: GC     ! Grid Component
   type(ESMF_State),    intent(inout) :: IMPORT ! Import State
   type(ESMF_State),    intent(inout) :: EXPORT ! Export State
   integer,             intent(  out) :: RC     ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: This is a simple ESMF wrapper.
!
! !REVISION HISTORY:
!
! 20May2014  Manyin    First crack.
!
!EOP
!-------------------------------------------------------------------------

                              __Iam__('Initialize_')

    type(TR_State), pointer       :: myState     ! Legacy state
    type(ESMF_Grid)               :: GRID        ! Grid
    type(ESMF_Config)             :: CF          ! Universal Config 

    integer                       :: im, jm, km  ! 3D Dimensions

    integer                       :: nymd, nhms  ! date, time
    real                          :: cdt         ! time step in secs

    character(len=1024)           :: var_names   ! comma separated names

    character(len=ESMF_MAXSTR)    :: comp_name

    type (Chem_Registry), pointer :: r
    type (TR_TracerKit),  pointer :: k

    integer                       :: n, ichem

    REAL, POINTER, DIMENSION(:,:) :: lats
    REAL, POINTER, DIMENSION(:,:) :: lons

    integer, dimension(3)         :: dims


! For NTYPE
# include "gmi_emiss_constants.h"
! For GMI_PI, DEGPRAD
# include "gmi_phys_constants.h"


!  Declare pointers to IMPORT/EXPORT/INTERNAL states 
!  -------------------------------------------------
! This file has lots of auto-generated lines, and then some boilerplate
! #  include "MAMchem_DeclarePointer___.h"
! ==== The boilerplate:
        type(MAPL_MetaComp), pointer :: genState    ! MAPL generic state (was MetaComp)
        type(ESMF_State)             :: INTERNAL
! ====

! ==== Custom:
!       real, pointer, dimension(:,:,:) :: TR_sample

  
!  Get my name and set-up traceback handle
!  ---------------------------------------
   call ESMF_GridCompGet( GC, name=comp_name, __RC__ )
   Iam = trim(comp_name) // '::' // trim(Iam)

!                               --------
!  if (MAPL_AM_I_ROOT()) then
!     print *, trim(Iam)//': Starting...'
!  end if


!  Initialize MAPL Generic
!  -----------------------
   call MAPL_GenericInitialize ( GC, IMPORT, EXPORT, clock,  __RC__ )

!  Get pointers to IMPORT/EXPORT/INTERNAL states 
!  ---------------------------------------------
! This file has lots of auto-generated lines, and some boilerplate
! #  include "MAMchem_GetPointer___.h"
! ==== The boilerplate:
!       Get my MAPL Meta Component
!       --------------------------
        call MAPL_GetObjectFromGC ( GC, genState, __RC__ )

!       Associate the Internal State fields with our legacy state
!       ---------------------------------------------------------
        call MAPL_Get ( genState, INTERNAL_ESMF_STATE=INTERNAL, __RC__ )
! ====

! ==== Custom:
!       Get pointers to data in state
!       -----------------------------
!       call MAPL_GetPointer ( INTERNAL, TR_sample,  'TR_sample', __RC__ )



!  Extract relevant runtime information
!  ------------------------------------
   call extract_ ( GC, CLOCK, myState, GRID, CF, nymd, nhms, cdt, __RC__ )


!  Shorthand to reference the Chem Registry:
   r => myState%chemReg


!  Init the Tracer Kit of values
!  -----------------------------
   k => myState%kit   ! short-hand

     k%tr_count = r%n_TR

!    Local dimensions
!    ----------------
     call ESMF_GridGet(GRID, localDE=0, &
                             staggerloc=ESMF_STAGGERLOC_CENTER, &
                             computationalCount=dims, __RC__)
     k%i1 = 1       ! As in GOCART_GridCompMod
     k%j1 = 1       ! As in GOCART_GridCompMod
     k%i2 = dims(1)
     k%j2 = dims(2)

!    Global dimensions
!    ----------------
     call MAPL_GridGet ( GRID, globalCellCountPerDim=dims, __RC__ )
     k%im = dims(1)
     k%jm = dims(2)
     k%km = dims(3)

     k%grid_esmf = GRID

!    Get parameters from generic state
!    ----------------------------------
     call MAPL_Get(genState, LONS=lons, LATS=lats, __RC__)

!    Note: With the Cubed Sphere, all longitude values are positive (0 to 2*pi).
!          When running on the lat/lon grid, longitude values range from -pi to pi.
!    print*,'LATITUDE  minmax: ', MINVAL(lats*DEGPRAD), MAXVAL(lats*DEGPRAD)
!    print*,'LONGITUDE minmax: ', MINVAL(lons*DEGPRAD), MAXVAL(lons*DEGPRAD)

!    Here is how to tell if Cubed Sphere  (string == FV or FV3):
!    call ESMF_ConfigGetAttribute(CF, dynCoreStr, Label="DYCORE:", __RC__ )

     allocate ( k%lats(k%i1 : k%i2, k%j1 : k%j2), __STAT__ )
     allocate ( k%lons(k%i1 : k%i2, k%j1 : k%j2), __STAT__ )

     k%lats = lats * DEGPRAD
     k%lons = lons * DEGPRAD
     WHERE ( k%lons < 0.0 ) k%lons = k%lons + 360.0d0

!    Hardcode Ptop, just as for w_c 
     k%ptop = 1.0  ! Pa

!  Allocate storage for all of the Tracer Specs
!  --------------------------------------------
   allocate( myState%spec(r%n_TR), __STAT__ )

!  Initialize the Tracer Specs  (parse the RC files)
!  -------------------------------------------------
   do n = 1, r%n_TR

     ichem = r%i_TR + n - 1   ! ichem = index of tracer in the chem registry

     ! If we TRIM the first 2 args, we do not adequately clear the contents of the receiving strings
     call TR_init_tracer_spec_ ( r%vname(ichem), r%vunits(ichem), myState%kit, myState%CF, myState%spec(n), &
                                  IMPORT, EXPORT,  nymd, nhms, cdt, __RC__ )

   end do

!  Set the grid
!  -------------------------------------------------
   myState%grid = GRID

!  Set the time step
!  -------------------------------------------------
   myState%dt = cdt

!  Bundle the passive tracers
!  -------------------------------------------------
   do n = r%i_TR, r%j_TR

     if ( n .EQ. r%i_TR ) then
       var_names =                           TRIM(r%vname(n))
     else
       var_names = TRIM(var_names) // ',' // TRIM(r%vname(n))
     endif

   end do

   myState%qa = MAPL_SimpleBundleCreate(INTERNAL, name='TR_PASSIVE_TRACERS', &
                                               only_vars=var_names, __RC__)
   call MAPL_SimpleBundlePrint(myState%qa)


!  Optional elements of the Tracer Kit
!  -------------------------------------------------
   IF ( ANY(myState%spec(1:r%n_TR)%dry_deposition) ) THEN

     allocate ( k%ireg (k%i1 : k%i2, k%j1 : k%j2       ), __STAT__ )
     allocate ( k%iland(k%i1 : k%i2, k%j1 : k%j2, NTYPE), __STAT__ )
     allocate ( k%iuse (k%i1 : k%i2, k%j1 : k%j2, NTYPE), __STAT__ )
     allocate ( k%xlai (k%i1 : k%i2, k%j1 : k%j2, NTYPE), __STAT__ )

     k%veg_fraction_done = .FALSE.

   END IF


!  All done
!  --------
   RETURN_(ESMF_SUCCESS)

   END SUBROUTINE Initialize_


!-------------------------------------------------------------------------
!     NASA/GSFC, Atmospheric Chemistry and Dynamics Lab,  Code 614       !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Run_ --- Runs TR
!
! !INTERFACE:
!

   SUBROUTINE Run_ ( GC, IMPORT, EXPORT, CLOCK, RC )

! !USES:

   implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout) :: GC     ! Grid Component
   type(ESMF_State),    intent(inout) :: IMPORT ! Import State
   type(ESMF_State),    intent(inout) :: EXPORT ! Export State
   integer,             intent(  out) :: RC     ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: This is a simple ESMF wrapper.
!
! !REVISION HISTORY:
!
! 20May2014  Manyin    First crack.
!
!EOP
!-------------------------------------------------------------------------

!   This macro has the unintended side effect of making Iam a SAVEd variable
!                             __Iam__('Run_')
!   Instead:
    integer                       :: STATUS
    character(len=ESMF_MAXSTR)    :: Iam
    character(len=64)             :: subrtn_name='Run_'

   
    type(TR_State), pointer       :: myState     ! Legacy state
    type(ESMF_Grid)               :: GRID        ! Grid
    type(ESMF_Config)             :: CF          ! Universal Config 

    type(ESMF_Alarm)              :: run_alarm
    logical                       :: run_alarm_ringing

    type (TR_TracerKit),  pointer :: k

    integer                       :: nymd, nhms  ! date, time
    real                          :: cdt         ! time step in secs

    character(len=ESMF_MAXSTR)    :: comp_name

    integer                       :: ii          ! loop var
    integer                       :: n

    real, pointer, dimension(:,:,:) :: PTR3D

    logical                       :: verbose=.FALSE.


! For NTYPE
# include "gmi_emiss_constants.h"

!  Declare pointers to IMPORT/EXPORT/INTERNAL states 
!  -------------------------------------------------
! This file has lots of auto-generated lines, and then some boilerplate
! #  include "MAMchem_DeclarePointer___.h"
! ==== The boilerplate:
        type(MAPL_MetaComp), pointer :: genState    ! MAPL generic state (was MetaComp)
        type(ESMF_State)             :: INTERNAL
! ====

! ==== Custom:
!       real, pointer, dimension(:,:,:) :: TR_sample

!  All declarations are done
!  Now we can assign to Iam without making that variable persistent
   Iam = trim(subrtn_name)

!  Get my name and set-up traceback handle
!  ---------------------------------------
   call ESMF_GridCompGet( GC, name=comp_name, __RC__ )
   Iam = trim(comp_name) // '::' // trim(Iam)

!  Informational message
!  -----------------------------
   if (verbose .AND. MAPL_AM_I_ROOT()) print *, 'Passive Tracer -- Beginning of RUN routine'

!  Get pointers to IMPORT/EXPORT/INTERNAL states 
!  ---------------------------------------------
! This file has lots of auto-generated lines, and some boilerplate
! #  include "MAMchem_GetPointer___.h"
! ==== The boilerplate:
!       Get my MAPL Meta Component
!       --------------------------
        call MAPL_GetObjectFromGC ( GC, genState, __RC__ )

!       Associate the Internal State fields with our legacy state
!       ---------------------------------------------------------
        call MAPL_Get ( genState, INTERNAL_ESMF_STATE=INTERNAL, __RC__ )
! ====

! ==== Custom:
!       Get pointers to data in state
!       -----------------------------
!       call MAPL_GetPointer ( INTERNAL, TR_sample,  'TR_sample', __RC__ )

!  After we have extracted the Legacy State, we will have the names
!  of the fields in the Internal State, and we can Get Pointers.


!  Get my internal MAPL_Generic state
!  -----------------------------------
   call MAPL_GetObjectFromGC(GC, genState, __RC__)


!  Get parameters from generic state
!  ----------------------------------
   call MAPL_Get(genState, RunAlarm=run_alarm, __RC__)


!  If it is time, update TR state
!  -------------------------------
   run_alarm_ringing = ESMF_AlarmIsRinging(run_alarm, __RC__)

   if (run_alarm_ringing) then
       call ESMF_AlarmRingerOff(run_alarm, __RC__)
   else
       RETURN_(ESMF_SUCCESS)
   endif


!  Extract relevant runtime information
!  ------------------------------------
   call extract_ ( GC, CLOCK, myState, GRID, CF, nymd, nhms, cdt, __RC__ )

   if (verbose .AND. MAPL_AM_I_ROOT()) print *, 'RUNNING Passive Tracer -- cdt = ', cdt


   k => myState%kit  ! shorthand


!  Update values that apply to all passive tracers
!  -----------------------------------------------

   IF ( ANY(myState%spec(:)%dry_deposition) ) THEN

     IF ( .NOT. k%veg_fraction_done ) THEN

       CALL MAPL_GetPointer(IMPORT, PTR3D, 'TR_VEG_FRAC', __RC__ )

       CALL Decode_Land_Types(PTR3D, NTYPE,                       &
                              k%i1, k%i2, k%j1, k%j2, NVEGTYPE-2, &
                              k%ireg,                             &
                              k%iuse,                             &
                              k%iland,                            &
                              __RC__ )

       NULLIFY(PTR3D)

       k%veg_fraction_done = .TRUE.

     END IF

     CALL MAPL_GetPointer(IMPORT, PTR3D, 'TR_LAI_FRAC', __RC__ )

     CALL Decode_XLAI(PTR3D, NTYPE,                       &
                      k%i1, k%i2, k%j1, k%j2, NVEGTYPE-2, &
                      k%ireg,                             &
                      k%iuse,                             &
                      k%iland,                            &
                      k%xlai,                             &
                      __RC__ )

     NULLIFY(PTR3D)

   END IF


!  Run each tracer (source and sink)
!  ---------------------------------
   do n = 1, k%tr_count

     call TR_run_tracer_ ( myState%pet, myState%kit, myState%spec(n), myState%qa, &
                           GRID, IMPORT, EXPORT,  nymd, nhms, cdt, __RC__ )

   end do



#if (0)
!  Force non-negative mixing ratios
!  --------------------------------
   do ii = 1, myState%qa%n3d
       where(myState%qa%r3(ii)%q < 0) myState%qa%r3(ii)%q = 1.0e-20
   end do
#endif


!  Informational message
!  -----------------------------
   if (verbose .AND. MAPL_AM_I_ROOT()) print *, 'Passive Tracer -- End of RUN routine'

!  All done
!  --------
   RETURN_(ESMF_SUCCESS)

   END SUBROUTINE Run_

!-------------------------------------------------------------------------
!     NASA/GSFC, Atmospheric Chemistry and Dynamics Lab,  Code 614       !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Finalize_ --- Finalize TR
!
! !INTERFACE:
!

   SUBROUTINE Finalize_ ( GC, IMPORT, EXPORT, CLOCK, RC )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout) :: gc     ! Grid Component
   type(ESMF_State),    intent(inout) :: IMPORT ! Import State
   type(ESMF_State),    intent(inout) :: EXPORT ! Export State
   integer,             intent(  out) :: RC     ! Error return code:
                                                !  0 - all is well
                                                !  1 - 

! !DESCRIPTION: This is a simple ESMF wrapper.
!
! !REVISION HISTORY:
!
! 20May2014  Manyin    First crack.
!
!EOP
!-------------------------------------------------------------------------

!   This macro has the unintended side effect of making Iam a SAVEd variable
!                             __Iam__('Finalize_')
!   Instead:
    integer                       :: STATUS
    character(len=ESMF_MAXSTR)    :: Iam
    character(len=64)             :: subrtn_name='Finalize_'
   
    type(TR_State), pointer       :: myState     ! Legacy state
    type(ESMF_Grid)               :: GRID        ! Grid
    type(ESMF_Config)             :: CF          ! Universal Config 

    integer                       :: i,j         ! for do-loops

    integer                       :: nymd, nhms  ! date, time
    real                          :: cdt         ! time step in secs

    character(len=ESMF_MAXSTR)    :: comp_name

!  All declarations are done
!  Now we can assign to Iam without making that variable persistent
   Iam = trim(subrtn_name)

!  Get my name and set-up traceback handle
!  ---------------------------------------
   call ESMF_GridCompGet( GC, name=comp_name, __RC__ )
   Iam = trim(comp_name) // '::' // trim(Iam)

!  if (MAPL_AM_I_ROOT()) then
!     print *, trim(Iam)//': wrap up...'
!     print *, ''
!  end if

!  Finalize MAPL Generic
!  ---------------------
    call MAPL_GenericFinalize ( GC, IMPORT, EXPORT, CLOCK,  __RC__ )

!  Extract relevant runtime information
!  ------------------------------------
   call extract_ ( GC, CLOCK, myState, GRID, CF, nymd, nhms, cdt, __RC__ )

!  Free up the dynamically allocated memory
!  ----------------------------------------
#define test_and_dealloc(A) if(associated(A)) deallocate(A)
   deallocate( myState%chemReg )

   do i = 1, myState%kit%tr_count
     do j = 1, myState%spec(i)%surface_constraint_count
       test_and_dealloc( myState%spec(i)%constraints(j)%bool_array  )
       test_and_dealloc( myState%spec(i)%constraints(j)%bool_aux    )
       test_and_dealloc( myState%spec(i)%constraints(j)%value_array )
     end do
     test_and_dealloc( myState%spec(i)%constraints   )
     test_and_dealloc( myState%spec(i)%TRvolFlux     )
     deallocate(       myState%spec(i)%TRsfcFlux      )
     deallocate(       myState%spec(i)%tropp_prev     )
     deallocate(       myState%spec(i)%src_mask_horiz )
     deallocate(       myState%spec(i)%snk_mask_horiz )
   end do
   deallocate(       myState%spec      )

   deallocate(       myState%kit%lats  )
   deallocate(       myState%kit%lons  )
   test_and_dealloc( myState%kit%ireg  )
   test_and_dealloc( myState%kit%iland )
   test_and_dealloc( myState%kit%iuse  )
   test_and_dealloc( myState%kit%xlai  )
   deallocate(       myState%kit       )
#undef test_and_dealloc

!  Delete the tracers bundles
!  ------------------------------------
   call MAPL_SimpleBundleDestroy(myState%qa)


!  Informational message
!  -----------------------------
!  if (MAPL_AM_I_ROOT()) then
!     print *, 'Passive Tracer FINALIZING stuff...'
!  end if

!  All done
!  --------
   RETURN_(ESMF_SUCCESS)

 END SUBROUTINE Finalize_

!.......................................................................

 SUBROUTINE extract_ (GC, CLOCK,             &
                          myState, GRID, CF, &
                          nymd, nhms,        &
                          cdt, rc)

    type(ESMF_GridComp), intent(INout)  :: GC           ! Grid Comp object
    type(ESMF_Clock), intent(in)        :: CLOCK        ! Clock

    type(TR_State), pointer             :: myState      ! Legacy state
    type(ESMF_Grid),     intent(out)    :: GRID         ! Grid
    type(ESMF_Config),   intent(out)    :: CF           ! Universal Config 

    integer, intent(out)                :: nymd, nhms   ! date, time
    real, intent(out)                   :: cdt          ! time step in secs
    integer, intent(out), optional      :: rc

!                            ---

!   This macro has the unintended side effect of making Iam a SAVEd variable
!                             __Iam__('extract_')
!   Instead:
    integer                             :: STATUS
    character(len=ESMF_MAXSTR)          :: Iam
    character(len=64)                   :: subrtn_name='extract_'

    character(len=ESMF_MAXSTR)          :: comp_name

    type(MAPL_MetaComp), pointer        :: genState      ! MAPL generic state
    type(TR_Wrap)                       :: wrap

    integer, dimension(3)               :: dims

    type(ESMF_Alarm)                    :: run_alarm
    type(ESMF_TimeInterval)             :: ring_interval
    real(ESMF_KIND_R8)                  :: time_step

    type(ESMF_Time)                     :: time
    integer                             :: iyr, imm, idd, ihr, imn, isc


!   All declarations are done
!   Now we can assign to Iam without making that variable persistent
    Iam = trim(subrtn_name)

!   Get my name and set-up traceback handle
!   ---------------------------------------
    call ESMF_GridCompGet(GC, name=comp_name, __RC__)
    Iam = trim(comp_name) // '::' // trim(Iam)

    rc = 0

!   Get my internal MAPL_Generic state
!   -----------------------------------
    call MAPL_GetObjectFromGC(GC, genState, __RC__)

!   Get my internal state
!   ---------------------
    call ESMF_UserCompGetInternalState(GC, 'TR_STATE', wrap, STATUS)
    VERIFY_(STATUS)
    myState => wrap%ptr

!   Get the configuration
!   ---------------------
    call ESMF_GridCompGet(GC, config=CF, __RC__)

!   Get time step
!   -------------
    call MAPL_Get(genState, RunAlarm=run_alarm, __RC__)
    call ESMF_AlarmGet(run_alarm, ringInterval=ring_interval, __RC__)

    call ESMF_TimeIntervalGet(ring_interval, s_r8=time_step, __RC__)
    cdt = real(time_step)

!   Extract time as simple integers from clock
!   ------------------------------------------
    call ESMF_ClockGet(CLOCK, currTime=time, __RC__)
    call ESMF_TimeGet(TIME, yy=iyr, mm=imm, dd=idd, h=ihr,   m=imn,  s=isc, __RC__)

    call MAPL_PackTime(nymd, iyr, imm, idd)
    call MAPL_PackTime(nhms, ihr, imn, isc)

!   Extract the ESMF Grid
!   ---------------------
    call ESMF_GridCompGet(GC, grid=GRID, __RC__)


    RETURN_(ESMF_SUCCESS)

 END SUBROUTINE extract_


!-------------------------------------------------------------------------
!
!              Methods for handling a single passive tracer
!
!-------------------------------------------------------------------------
!     NASA/GSFC  Atmospheric Chemistry and Dynamics Lab   Code 614       !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  TR_init_tracer_spec_ --- Set up a tracer Specification
!
! !INTERFACE:
!

   SUBROUTINE TR_init_tracer_spec_ ( tname, tunits, kit, CF, spec, impChem, expChem, nymd, nhms, cdt, rc )

! !USES:

  IMPLICIT NONE

! !INPUT PARAMETERS:

   CHARACTER(LEN=128),  INTENT(IN)          :: tname      ! Name of the tracer
   CHARACTER(LEN=128),  INTENT(IN)          :: tunits     ! Units for the tracer
   TYPE(TR_TracerKit),  INTENT(IN), pointer :: kit        ! A set of values common to all passive tracers
   INTEGER,             INTENT(IN)          :: nymd, nhms ! time
   REAL,                INTENT(IN)          :: cdt        ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   TYPE(TR_TracerSpec), INTENT(INOUT) :: spec    ! Specification for a Passive Tracer
   TYPE(ESMF_State),    INTENT(INOUT) :: impChem ! Import State
   TYPE(ESMF_State),    INTENT(INOUT) :: expChem ! Export State
   TYPE(ESMF_Config),   INTENT(INOUT) :: CF      ! For reading rc files
   INTEGER, OPTIONAL,   INTENT(  OUT) :: RC      ! Error return code:
                                                 !  0 - all is well
                                                 !  1 - 

! !DESCRIPTION: Initializes a Tracer Specification.
!
! !REVISION HISTORY:
!
!  28May2014   Manyin  First crack.
!
!EOP
!-------------------------------------------------------------------------

   CHARACTER(LEN=*), PARAMETER :: myname = 'TR_init_tracer_spec_'
   CHARACTER(LEN=ESMF_MAXSTR)  :: Iam
   INTEGER                     :: STATUS

   CHARACTER(LEN=255) :: rcfilen 

   INTEGER :: n
   INTEGER :: i1, i2, j1, j2, im, jm, km
   LOGICAL :: NoRegionalConstraint

   REAL :: conFac, limitN, limitS, limitW, limitE, log10Emission

   LOGICAL            :: spec_ok    ! track whether a specified option is legal

   CHARACTER(LEN=255) :: first_phase_str
   REAL               :: src_value
   CHARACTER(LEN=255) :: src_units
   REAL               :: src_press1, src_press2
   REAL               :: snk_value
   CHARACTER(LEN=255) :: snk_units
   REAL               :: snk_period
   CHARACTER(LEN=255) :: snk_time_units
   REAL               :: snk_press1, snk_press2

   CHARACTER          :: c
   CHARACTER(LEN=255) :: token

   LOGICAL            :: verbose=.FALSE.  ! turn on/off more verbose messages

   Iam = TRIM(myname)
   
   spec%name  = TRIM(tname)
   spec%units = TRIM(tunits)

   IF(verbose.AND.MAPL_AM_I_ROOT()) PRINT *,'--------- passive tracer: ----------'
   IF(verbose.AND.MAPL_AM_I_ROOT()) PRINT *,myname,": TRACER ",TRIM(tname),"(",TRIM(tunits),")"

   rcfilen = 'TR_GridComp---' // TRIM(tname) // '.rc'

!  Initialize local variables
!  --------------------------
   rc = ESMF_SUCCESS   ! zero

   i1 = kit%i1
   i2 = kit%i2

   j1 = kit%j1
   j2 = kit%j2

   im = kit%im
   jm = kit%jm
   km = kit%km

   IF(verbose.AND.MAPL_AM_I_ROOT()) PRINT *,myname,": i1,i2,j1,j2 ",i1,i2,j1,j2
   IF(verbose.AND.MAPL_AM_I_ROOT()) PRINT *,myname,": im,jm,km ",im,jm,km


!  Allocate memory, etc
!  --------------------
   CALL init_()
   IF (rc /= 0) RETURN

!  Simplistic TROPP default
!  (100 hPa at EQ, 300 hPa at poles)
!  ---------------------------------
   spec%tropp_prev = ( ABS(kit%lats) * 200./90. + 100. ) * 100  ! Pa

!  Load the file using the ESMF utility
!  ------------------------------------
   IF(verbose.AND.MAPL_AM_I_ROOT()) PRINT *,myname,": about to load config file ",TRIM(rcfilen)
   call ESMF_ConfigLoadFile ( CF, TRIM(rcfilen), __RC__ )


!------------------------------------------ >>>
!    Regions file
!---------------------

     call ESMF_ConfigGetAttribute ( CF, label='regions_ExtData_entry:', value=spec%regions_ExtData_entry, default=NULL_REGION_MASK, __RC__ )
     IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,': regions_ExtData_entry = '//TRIM(spec%regions_ExtData_entry)

!------------------------------------------ <<<


!------------------------------------------ >>>
!  Source Mode
!---------------------
   call ESMF_ConfigGetAttribute ( CF, label='src_mode:', value=spec%src_mode,  __RC__ )

   IF(verbose.AND.MAPL_AM_I_ROOT()) PRINT *,myname,": src_mode ",TRIM(spec%src_mode)


   !  If the SRC is not explicit, skip reading some flags
   !-----------------------------------------------------
   IF( (TRIM(spec%src_mode) == "none" ) .OR. (TRIM(spec%src_mode) == "only_from_decay" ) ) THEN
     spec%run_order(1) = SINK_PHASE
     spec%run_order(2) =   NO_PHASE
     spec%src_add   = .FALSE.
   ELSE

     ! order of source/sink phases
     !----------------------------
     call ESMF_ConfigGetAttribute ( CF, label='first_phase:', value=first_phase_str, __RC__ )
     IF ( lowercase(TRIM(first_phase_str)) == "source" ) THEN
       spec%run_order(1) = SOURCE_PHASE
       spec%run_order(2) =   SINK_PHASE
     ELSE IF ( lowercase(TRIM(first_phase_str)) == "sink" ) THEN
       spec%run_order(1) =   SINK_PHASE
       spec%run_order(2) = SOURCE_PHASE
     ELSE
      IF(MAPL_AM_I_ROOT()) PRINT *,myname,": Invalid entry for first_phase - "//first_phase_str
      CALL final_(15)
      VERIFY_(rc)
     END IF

     IF(verbose.AND.MAPL_AM_I_ROOT()) PRINT *,myname,": first_phase ",lowercase(TRIM(first_phase_str))


     ! src_add
     !---------
     call ESMF_ConfigGetAttribute ( CF, label='src_add:', value=spec%src_add,  __RC__ )

     IF(verbose.AND.MAPL_AM_I_ROOT()) PRINT *,myname,": src_add ",spec%src_add

   END IF


!  Now see if the SRC specification is legal
   spec_ok = .FALSE.

!
!  NONE
!
   IF(TRIM(spec%src_mode) == "none"       ) spec_ok = .TRUE.

!
!  FROM_DECAY  (actually pushed by the contributing tracer, not pulled by this tracer)
!
   IF(TRIM(spec%src_mode) == "only_from_decay" ) spec_ok = .TRUE.

!
!  CONSTANT
!
   IF(TRIM(spec%src_mode) == "constant" ) THEN
     spec_ok = .TRUE.

     call ESMF_ConfigFindLabel    ( CF, 'src_value:',  __RC__ )
     call ESMF_ConfigGetAttribute ( CF,  src_value,    __RC__ )
     call ESMF_ConfigGetAttribute ( CF,  src_units,    __RC__ )

     IF(     TRIM(src_units) == TRIM(spec%units)) THEN
                                                        spec%src_value = src_value
     ELSE IF(TRIM(src_units) ==   "pppv"    ) THEN
                                                        spec%src_value = src_value
     ELSE IF(TRIM(src_units) ==   "ppmv"    ) THEN
                                                        spec%src_value = src_value * 1.0E-06
     ELSE IF(TRIM(src_units) ==   "ppbv"    ) THEN
                                                        spec%src_value = src_value * 1.0E-09
     ELSE IF(TRIM(src_units) ==   "pptv"    ) THEN
                                                        spec%src_value = src_value * 1.0E-12
     ELSE IF(TRIM(src_units) ==   "timestep") THEN

!      Compute the fraction in terms of the tracer units
       IF(     TRIM(spec%units) == "hours" ) THEN
                                                        spec%src_value = src_value * cdt / (60.*60.)
       ELSE IF(TRIM(spec%units) == "days"  ) THEN
                                                        spec%src_value = src_value * cdt / (60.*60.*24.)
       ELSE IF(TRIM(spec%units) == "years" ) THEN
                                                        spec%src_value = src_value * cdt / (60.*60.*24.*365.25)
       ELSE
         IF(MAPL_AM_I_ROOT()) PRINT *,myname,": TRACER src units 'timestep' requires tracer units hours, days or years."
         CALL final_(16)
         VERIFY_(rc)
       END IF

     ELSE
      IF(MAPL_AM_I_ROOT()) PRINT *,myname,": Invalid unit specified for TRACER src value."
      CALL final_(17)
      VERIFY_(rc)
     END IF

     IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": src value ",spec%src_value

   END IF

!
!  FILE2D or FILE3D
!
   IF( (TRIM(spec%src_mode) == "file2d").OR.(TRIM(spec%src_mode) == "file3d")) THEN
     spec_ok = .TRUE.

     call ESMF_ConfigGetAttribute ( CF, label='mw:',               value=spec%mw,                            __RC__ )

     IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": mw ",                 spec%mw

     IF(TRIM(spec%src_mode) == "file3d") THEN
       ALLOCATE (  spec%TRvolFlux(i1:i2,j1:j2,1:km), STAT=rc )
       VERIFY_(rc)
     END IF
   END IF

!
!  MAINTAIN MIXING RATIO (e.g. e90)
!
   IF(TRIM(spec%src_mode) == "maintain_mixing_ratio" ) THEN
     spec_ok = .TRUE.

     call ESMF_ConfigFindLabel    ( CF, 'src_value:',  __RC__ )
     call ESMF_ConfigGetAttribute ( CF,  src_value,    __RC__ )
     call ESMF_ConfigGetAttribute ( CF,  src_units,    __RC__ )

     IF(     TRIM(src_units) == TRIM(spec%units)) THEN
                                                        spec%src_value = src_value
     ELSE IF(TRIM(src_units) ==   "pppv")         THEN
                                                        spec%src_value = src_value
     ELSE IF(TRIM(src_units) ==   "ppmv")         THEN
                                                        spec%src_value = src_value * 1.0E-06
     ELSE IF(TRIM(src_units) ==   "ppbv")         THEN
                                                        spec%src_value = src_value * 1.0E-09
     ELSE IF(TRIM(src_units) ==   "pptv")         THEN
                                                        spec%src_value = src_value * 1.0E-12
     ELSE
      IF(MAPL_AM_I_ROOT()) PRINT *,myname,": Invalid unit specified for TRACER src value."
      CALL final_(19)
      VERIFY_(rc)
     END IF

     IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": src_value ",spec%src_value

     IF ( .NOT. spec%src_add ) THEN
       spec%src_add = .TRUE.
       IF(MAPL_AM_I_ROOT()) PRINT *,myname,": setting  src_add = TRUE "
     END IF

   END IF

!
!  FIELD in the model
!
   IF(TRIM(spec%src_mode) == "model_field" ) THEN
     spec_ok = .TRUE.

     call ESMF_ConfigGetAttribute ( CF, label='src_field_name:', value=spec%src_field_name, __RC__ )

     IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": src_field_name ",TRIM(spec%src_field_name)

   END IF

   IF ( .NOT. spec_ok ) THEN
     IF(MAPL_AM_I_ROOT()) PRINT *,myname,": Invalid src_mode for TRACER"
     CALL final_(21)
     VERIFY_(rc)
   END IF

!------------------------------------------ <<<


   Complete_the_SOURCE_spec: &
   IF( (TRIM(spec%src_mode) == "constant"              ) .OR. &
       (TRIM(spec%src_mode) == "file2d"                ) .OR. &
       (TRIM(spec%src_mode) == "file3d"                ) .OR. &
       (TRIM(spec%src_mode) == "maintain_mixing_ratio" ) .OR. &
       (TRIM(spec%src_mode) == "model_field"           )      &
     ) THEN

!------------------------------------------ >>>
!    Source Horizontal
!---------------------
     call ESMF_ConfigGetAttribute ( CF, label='src_horiz:', value=spec%src_horiz,  __RC__ )

     IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": src_horiz ",TRIM(spec%src_horiz)

!    Now see if the specification is legal
     spec_ok = .FALSE.

!
!    ALL
!
     IF(TRIM(spec%src_horiz) == "all" ) THEN
       spec_ok = .TRUE.
       spec%src_mask_horiz(:,:) = .TRUE.
     END IF

!
!    LAT_ZONE
!
     IF(TRIM(spec%src_horiz) == "lat_zone" ) THEN
       spec_ok = .TRUE.

       call ESMF_ConfigFindLabel    ( CF, 'src_lats:',  __RC__ )
       call ESMF_ConfigGetAttribute ( CF,  limitS,      __RC__ )
       call ESMF_ConfigGetAttribute ( CF,  limitN,      __RC__ )

       IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": src_lats ",limitS, limitN

!      Set up the horizontal mask
       spec%src_mask_horiz(:,:) = ( kit%lats >= limitS .AND. &
                                    kit%lats <= limitN       )

     END IF

!
!    LATLON_BOX
!
     IF(TRIM(spec%src_horiz) == "latlon_box" ) THEN
       spec_ok = .TRUE.

       call ESMF_ConfigFindLabel    ( CF, 'src_lats:',  __RC__ )
       call ESMF_ConfigGetAttribute ( CF,  limitS,      __RC__ )
       call ESMF_ConfigGetAttribute ( CF,  limitN,      __RC__ )

       call ESMF_ConfigFindLabel    ( CF, 'src_lons:',  __RC__ )
       call ESMF_ConfigGetAttribute ( CF,  limitW,      __RC__ )
       call ESMF_ConfigGetAttribute ( CF,  limitE,      __RC__ )

       IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": src_lats ",limitS, limitN
       IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": src_lons ",limitW, limitE

!      Set up the horizontal mask

       CALL mask_on_longitude_( kit%lons, limitW, limitE, spec%src_mask_horiz )

       WHERE ( kit%lats < limitS .OR. &
               kit%lats > limitN      )   spec%src_mask_horiz = .FALSE.

     END IF
!
!    MASKS
!
     IF(TRIM(spec%src_horiz) == "masks" ) THEN
       spec_ok = .TRUE.

       IF(MAPL_AM_I_ROOT()) PRINT *,myname,": TRACER src MASKS not implemented yet"
       CALL final_(25)
       VERIFY_(rc)

     END IF

     IF ( .NOT. spec_ok ) THEN
       IF(MAPL_AM_I_ROOT()) PRINT *,myname,": Invalid src_horiz for TRACER"
       CALL final_(26)
       VERIFY_(rc)
     END IF

!------------------------------------------ <<<


!------------------------------------------ >>>
!    Source Vertical
!---------------------
     call ESMF_ConfigGetAttribute ( CF, label='src_vert:', value=spec%src_vert,  __RC__ )

     IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": src_vert ",TRIM(spec%src_vert)

!    Now see if the specification is legal
     spec_ok = .FALSE.

!
!    ALL
!
     IF(TRIM(spec%src_vert) == "all" ) THEN
       spec_ok = .TRUE.
     END IF

!
!    SURFACE
!
     IF(TRIM(spec%src_vert) == "surface" ) THEN
       spec_ok = .TRUE.
     END IF

!
!    LEVELS
!
     IF(TRIM(spec%src_vert) == "levels" ) THEN
       spec_ok = .TRUE.

       call ESMF_ConfigFindLabel    ( CF, 'src_levels:',  __RC__ )
       call ESMF_ConfigGetAttribute ( CF,  spec%src_lev1, __RC__ )
       call ESMF_ConfigGetAttribute ( CF,  spec%src_lev2, __RC__ )

       IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": src levels ",spec%src_lev1,spec%src_lev2

     END IF

!
!    PRESSURES
!
     IF(TRIM(spec%src_vert) == "pressures" ) THEN
       spec_ok = .TRUE.

       call ESMF_ConfigFindLabel    ( CF, 'src_pressures:', __RC__ )
       call ESMF_ConfigGetAttribute ( CF,  src_press1,      __RC__ )
       call ESMF_ConfigGetAttribute ( CF,  src_press2,      __RC__ )

       spec%src_press1 = MIN(src_press1,src_press2) * 100.0   ! hPa -> Pa
       spec%src_press2 = MAX(src_press1,src_press2) * 100.0   ! hPa -> Pa

       IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": src pressures ",spec%src_press1,spec%src_press2

     END IF

!
!    TROPOSPHERE
!
     IF(TRIM(spec%src_vert) == "troposphere" ) THEN
       spec_ok = .TRUE.
     END IF

!
!    STRATOSPHERE
!
     IF(TRIM(spec%src_vert) == "stratosphere" ) THEN
       spec_ok = .TRUE.
     END IF

     IF ( .NOT. spec_ok ) THEN
       IF(MAPL_AM_I_ROOT()) PRINT *,myname,": Invalid src_vert for TRACER"
       CALL final_(30)
       VERIFY_(rc)
     END IF

!------------------------------------------ <<<


!------------------------------------------ >>>
!    Source Constraints
!---------------------

     call ESMF_ConfigGetAttribute ( CF, label='surface_constraint_count:', value=spec%surface_constraint_count, default=0, __RC__ )

     IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,': surface constraint count = ',spec%surface_constraint_count

     IF( spec%surface_constraint_count > 9 ) THEN
       IF(MAPL_AM_I_ROOT()) PRINT *,myname,': cannot have more than 9 surface constraints'
       CALL final_(131)
       VERIFY_(rc)
     END IF

     Init_Surface_Constraints: &
     IF ( spec%surface_constraint_count > 0 ) THEN

       ALLOCATE( spec%constraints( spec%surface_constraint_count ), __STAT__ )

       DO n = 1, spec%surface_constraint_count

         write (c ,'(i1.1)') n

         ! MODE
         !
         call ESMF_ConfigGetAttribute ( CF, label='surf_con'//c//'_mode:', value=spec%constraints(n)%mode,  __RC__ )

         IF( (TRIM(spec%constraints(n)%mode) .NE. 'set'  ) .AND. &
             (TRIM(spec%constraints(n)%mode) .NE. 'scale') ) THEN
           CALL final_(133)
           VERIFY_(rc)
         END IF

         IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,': constraint mode ',TRIM(spec%constraints(n)%mode)

         ! REGIONS
         !
         call ESMF_ConfigGetAttribute ( CF, label='surf_con'//c//'_regions:', value=spec%constraints(n)%regions_str,  &
                                        default='NO_REGIONS', __RC__ )

         IF ( spec%constraints(n)%regions_str == 'NO_REGIONS' ) THEN
           spec%constraints(n)%use_regions  = .FALSE.
           IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,': constraint regions not specified'
         ELSE
           IF( spec%regions_ExtData_entry == NULL_REGION_MASK ) THEN
             IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,': Must include regions_ExtData_entry to use REGIONS'
             CALL final_(134)
             VERIFY_(rc)
           END IF
           spec%constraints(n)%use_regions  = .TRUE.
           IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,': constraint regions = ',TRIM(spec%constraints(n)%regions_str)
         END IF

         ! BOOLEAN EXPRESSION
         !
         call ESMF_ConfigFindLabel    ( CF, 'surf_con'//c//'_bool:',  rc=rc )
         IF ( rc == ESMF_SUCCESS ) THEN
           spec%constraints(n)%use_bool  = .TRUE.

           call ESMF_ConfigGetAttribute ( CF,  spec%constraints(n)%bool_FIELD,  __RC__ )
           call ESMF_ConfigGetAttribute ( CF,  spec%constraints(n)%bool_OP,     __RC__ )
           call ESMF_ConfigGetAttribute ( CF,  spec%constraints(n)%bool_SCALAR, __RC__ )

           IF ( (TRIM(spec%constraints(n)%bool_OP) .NE. '.EQ.') .AND.   &
                (TRIM(spec%constraints(n)%bool_OP) .NE. '.NE.') .AND.   &
                (TRIM(spec%constraints(n)%bool_OP) .NE. '.LT.') .AND.   &
                (TRIM(spec%constraints(n)%bool_OP) .NE. '.GT.') .AND.   &
                (TRIM(spec%constraints(n)%bool_OP) .NE. '.LE.') .AND.   &
                (TRIM(spec%constraints(n)%bool_OP) .NE. '.GE.') .AND.   &
                (TRIM(spec%constraints(n)%bool_OP) .NE.  '==' ) .AND.   &
                (TRIM(spec%constraints(n)%bool_OP) .NE.  '/=' ) .AND.   &
                (TRIM(spec%constraints(n)%bool_OP) .NE.  '<'  ) .AND.   &
                (TRIM(spec%constraints(n)%bool_OP) .NE.  '>'  ) .AND.   &
                (TRIM(spec%constraints(n)%bool_OP) .NE.  '<=' ) .AND.   &
                (TRIM(spec%constraints(n)%bool_OP) .NE.  '>=' ) )  THEN
             IF(MAPL_AM_I_ROOT()) PRINT *,myname,': bad boolean operator'
             CALL final_(136)
             VERIFY_(rc)
           END IF
           IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,': constraint boolean  FIELD: '//TRIM(spec%constraints(n)%bool_FIELD )
           IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,': constraint boolean     OP: '//TRIM(spec%constraints(n)%bool_OP    )
           IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,': constraint boolean SCALAR: ',      spec%constraints(n)%bool_SCALAR
         ELSE
           spec%constraints(n)%use_bool  = .FALSE.
           IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,': constraint boolean not specified'
         END IF

         ! EXPR - determines the values to use when setting or scaling
         !
         call ESMF_ConfigGetAttribute ( CF, label='surf_con'//c//'_expr:', value=spec%constraints(n)%expr, __RC__ )

         IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,': constraint Expr='//TRIM(spec%constraints(n)%expr )

         ALLOCATE( spec%constraints(n)%bool_array( i1:i2,j1:j2),  &
                   spec%constraints(n)%bool_aux(   i1:i2,j1:j2),  &
                   spec%constraints(n)%value_array(i1:i2,j1:j2), __STAT__ )

       END DO

     END IF  Init_Surface_Constraints

   END IF  Complete_the_SOURCE_spec


!------------------------------------------ >>>
!  Sink Mode
!---------------------
   call ESMF_ConfigGetAttribute ( CF, label='snk_mode:', value=spec%snk_mode,  __RC__ )

   IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": snk_mode ",TRIM(spec%snk_mode)

!  Now see if the specification is legal
   spec_ok = .FALSE.

!
!  NO SINK
!
   IF(TRIM(spec%snk_mode) == "none" ) THEN
     spec_ok = .TRUE.
   END IF

!
!  CONSTANT
!
   IF(TRIM(spec%snk_mode) == "constant" ) THEN
     spec_ok = .TRUE.

     call ESMF_ConfigFindLabel    ( CF, 'snk_value:',  __RC__ )
     call ESMF_ConfigGetAttribute ( CF,  snk_value,    __RC__ )
     call ESMF_ConfigGetAttribute ( CF,  snk_units,    __RC__ )

     IF(     TRIM(snk_units) == TRIM(spec%units)) THEN
                                                        spec%snk_value = snk_value
     ELSE IF(TRIM(snk_units) ==   "pppv")         THEN
                                                        spec%snk_value = snk_value
     ELSE IF(TRIM(snk_units) ==   "ppmv")         THEN
                                                        spec%snk_value = snk_value * 1.0E-06
     ELSE IF(TRIM(snk_units) ==   "ppbv")         THEN
                                                        spec%snk_value = snk_value * 1.0E-09
     ELSE IF(TRIM(snk_units) ==   "pptv")         THEN
                                                        spec%snk_value = snk_value * 1.0E-12
     ELSE
      IF(MAPL_AM_I_ROOT()) PRINT *,myname,": Invalid unit specified for TRACER snk value."
      CALL final_(33)
      VERIFY_(rc)
     END IF

     IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": snk_value ",spec%snk_value

   END IF

!
!  E-FOLDING  or  HALF-LIFE
!
   IF( (TRIM(spec%snk_mode) == "efolding") .OR. &
       (TRIM(spec%snk_mode) == "halflife") ) THEN
     spec_ok = .TRUE.

     call ESMF_ConfigFindLabel    ( CF, 'snk_period:',   __RC__ )
     call ESMF_ConfigGetAttribute ( CF,  snk_period,     __RC__ )
     call ESMF_ConfigGetAttribute ( CF,  snk_time_units, __RC__ )

!    Find the constant needed to convert half-life to seconds.
!    ----------------------------------------------------

     IF(     TRIM(snk_time_units) == "years"  ) THEN
                                                       conFac = 86400.00*365.25
     ELSE IF(TRIM(snk_time_units) == "days"   ) THEN
                                                       conFac = 86400.00
     ELSE IF(TRIM(snk_time_units) == "seconds") THEN
                                                       conFac = 1.00
     ELSE
      IF(MAPL_AM_I_ROOT()) PRINT *,myname,": Invalid TRACER time units",snk_time_units
      CALL final_(35)
      VERIFY_(rc)
     END IF

     IF(snk_period <= 0.00) THEN
      IF(MAPL_AM_I_ROOT()) PRINT *,myname,": TRACER decay period must be greater than zero."
      CALL final_(36)
      VERIFY_(rc)
     END IF

     IF ( TRIM(spec%snk_mode) == "efolding" ) THEN

!      Compute the decay constant (inverse seconds) from the e-folding time:
!        ln(N/No) = ln(1/e) = (-1) =  -decayConstant * e-folding time
!      ----------------------------------------------------------------

       spec%decayConstant = 1.0/(snk_period*conFac)
     END IF

     IF ( TRIM(spec%snk_mode) == "halflife" ) THEN

!      Compute the decay constant (inverse seconds) from the half-life:
!        ln(N/No) = ln(1/2) = -decayConstant * halfLife
!      ----------------------------------------------------------------

       spec%decayConstant = 0.693147/(snk_period*conFac)
     END IF

     IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": decay constant ",spec%decayConstant

!    Optional tracer that results from decay
!    ----------------------------------------------------
     call ESMF_ConfigGetAttribute ( CF, label='snk_decays_to:', value=spec%decays_to, default=NO_DECAY, __RC__ )

     IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": decays to ",TRIM(spec%decays_to)

   END IF
!
!  CHEMICAL_LOSS
!
   IF(TRIM(spec%snk_mode) == "chemical_loss" ) THEN
     spec_ok = .TRUE.

     call ESMF_ConfigGetAttribute ( CF, label='loss_species:', value=spec%loss_species, __RC__ )

     IF(     TRIM(spec%loss_species) ==   "O3") THEN
!                                                       OK
     ELSE
      IF(MAPL_AM_I_ROOT()) PRINT *,myname,": Invalid loss_species specified for TRACER snk."
      CALL final_(37)
      VERIFY_(rc)
     END IF

     IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": loss_species ",spec%loss_species

   END IF

   IF ( .NOT. spec_ok ) THEN
     IF(MAPL_AM_I_ROOT()) PRINT *,myname,": Invalid snk_mode for TRACER"
     CALL final_(38)
     VERIFY_(rc)
   END IF
!------------------------------------------ <<<

   Complete_the_SINK_spec: &
   IF(TRIM(spec%snk_mode) /= "none" ) THEN

!------------------------------------------ >>>
!    Sink Horizontal
!---------------------
     call ESMF_ConfigGetAttribute ( CF, label='snk_horiz:', value=spec%snk_horiz,  __RC__ )

     IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": snk_horiz ",TRIM(spec%snk_horiz)

!    Now see if the specification is legal
     spec_ok = .FALSE.

!
!    ALL
!
     IF(TRIM(spec%snk_horiz) == "all" ) THEN
       spec_ok = .TRUE.
       spec%snk_mask_horiz(:,:) = .TRUE.
     END IF

!
!    LAT_ZONE
!
     IF(TRIM(spec%snk_horiz) == "lat_zone" ) THEN
       spec_ok = .TRUE.

       call ESMF_ConfigFindLabel    ( CF, 'snk_lats:', __RC__ )
       call ESMF_ConfigGetAttribute ( CF,  limitS,     __RC__ )
       call ESMF_ConfigGetAttribute ( CF,  limitN,     __RC__ )

       IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": snk lats ",limitS,limitN

!      Set up the horizontal mask
       spec%snk_mask_horiz(:,:) = ( kit%lats >= limitS .AND. &
                                    kit%lats <= limitN       )

     END IF

!
!    LATLON_BOX
!
     IF(TRIM(spec%snk_horiz) == "latlon_box" ) THEN
       spec_ok = .TRUE.

       call ESMF_ConfigFindLabel    ( CF, 'snk_lats:', __RC__ )
       call ESMF_ConfigGetAttribute ( CF,  limitS,     __RC__ )
       call ESMF_ConfigGetAttribute ( CF,  limitN,     __RC__ )

       call ESMF_ConfigFindLabel    ( CF, 'snk_lons:', __RC__ )
       call ESMF_ConfigGetAttribute ( CF,  limitW,     __RC__ )
       call ESMF_ConfigGetAttribute ( CF,  limitE,     __RC__ )

       IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": snk lats ",limitS,limitN
       IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": snk lons ",limitW,limitE

!      Set up the horizontal mask

       CALL mask_on_longitude_( kit%lons, limitW, limitE, spec%snk_mask_horiz )

       WHERE ( kit%lats < limitS .OR. &
               kit%lats > limitN      )   spec%snk_mask_horiz = .FALSE.

     END IF
!
!    MASKS
!
     IF(TRIM(spec%snk_horiz) == "masks" ) THEN
       spec_ok = .TRUE.

       IF(MAPL_AM_I_ROOT()) PRINT *,myname,": TRACER snk MASKS not implemented yet"
       CALL final_(41)
       VERIFY_(rc)

     END IF

     IF ( .NOT. spec_ok ) THEN
       IF(MAPL_AM_I_ROOT()) PRINT *,myname,": Invalid snk_horiz for TRACER"
       CALL final_(42)
       VERIFY_(rc)
     END IF

!------------------------------------------ <<<


!------------------------------------------ >>>
!    Sink Vertical
!---------------------
     call ESMF_ConfigGetAttribute ( CF, label='snk_vert:', value=spec%snk_vert,  __RC__ )

     IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": snk_vert ",TRIM(spec%snk_vert)

!    Now see if the specification is legal
     spec_ok = .FALSE.

!
!    ALL
!
     IF(TRIM(spec%snk_vert) == "all" ) THEN
       spec_ok = .TRUE.
     END IF

!
!    SURFACE
!
     IF(TRIM(spec%snk_vert) == "surface" ) THEN
       spec_ok = .TRUE.
     END IF

!
!    LEVELS
!
     IF(TRIM(spec%snk_vert) == "levels" ) THEN
       spec_ok = .TRUE.

       call ESMF_ConfigFindLabel    ( CF, 'snk_levels:',  __RC__ )
       call ESMF_ConfigGetAttribute ( CF,  spec%snk_lev1, __RC__ )
       call ESMF_ConfigGetAttribute ( CF,  spec%snk_lev2, __RC__ )

       IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": snk levels ",spec%snk_lev1,spec%snk_lev2

     END IF

!
!    PRESSURES
!
     IF(TRIM(spec%snk_vert) == "pressures" ) THEN
       spec_ok = .TRUE.

       call ESMF_ConfigFindLabel    ( CF, 'snk_pressures:', __RC__ )
       call ESMF_ConfigGetAttribute ( CF,  snk_press1,      __RC__ )
       call ESMF_ConfigGetAttribute ( CF,  snk_press2,      __RC__ )

       spec%snk_press1 = MIN(snk_press1,snk_press2) * 100.0   ! hPa -> Pa
       spec%snk_press2 = MAX(snk_press1,snk_press2) * 100.0   ! hPa -> Pa

       IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": snk pressures ",spec%snk_press1,spec%snk_press2

     END IF

!
!    TROPOSPHERE
!
     IF(TRIM(spec%snk_vert) == "troposphere" ) THEN
       spec_ok = .TRUE.
     END IF

!
!    STRATOSPHERE
!
     IF(TRIM(spec%snk_vert) == "stratosphere" ) THEN
       spec_ok = .TRUE.
     END IF

     IF ( .NOT. spec_ok ) THEN
       IF(MAPL_AM_I_ROOT()) PRINT *,myname,": Invalid snk_vert for TRACER"
       CALL final_(46)
       VERIFY_(rc)
     END IF

!------------------------------------------ <<<

   END IF  Complete_the_SINK_spec


!
!  DRY DEPOSITION
!
  call ESMF_ConfigGetAttribute (   CF, label='dry_deposition:',   value=spec%dry_deposition, default=.FALSE., __RC__ )
  IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": dry_deposition ",spec%dry_deposition

  IF ( spec%dry_deposition ) THEN

     call ESMF_ConfigGetAttribute ( CF, label='aerosol_density:',  value=spec%aero_density,     __RC__ )
     call ESMF_ConfigGetAttribute ( CF, label='effective_radius:', value=spec%aero_eff_radius,  __RC__ )
     call ESMF_ConfigGetAttribute ( CF, label='c1:',               value=spec%aero_c(1),        __RC__ )
     call ESMF_ConfigGetAttribute ( CF, label='c2:',               value=spec%aero_c(2),        __RC__ )
     call ESMF_ConfigGetAttribute ( CF, label='c3:',               value=spec%aero_c(3),        __RC__ )
     call ESMF_ConfigGetAttribute ( CF, label='c4:',               value=spec%aero_c(4),        __RC__ )
     call ESMF_ConfigGetAttribute ( CF, label='mw:',               value=spec%mw,               __RC__ )
     call ESMF_ConfigGetAttribute ( CF, label='hstar:',            value=spec%hstar,            __RC__ )
     call ESMF_ConfigGetAttribute ( CF, label='delH_298_over_R:',  value=spec%delH_298_over_R,  __RC__ )
     call ESMF_ConfigGetAttribute ( CF, label='treat_as_aerosol:', value=spec%aero_flag,        __RC__ )
     call ESMF_ConfigGetAttribute ( CF, label='f0:',               value=spec%f0,               __RC__ )

     IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": dry deposition params ", &
         spec%aero_density, spec%aero_eff_radius, &
         spec%aero_c(1), spec%aero_c(2), spec%aero_c(3), spec%aero_c(4), &
         spec%mw, spec%hstar, spec%delH_298_over_R, spec%aero_flag, spec%f0

  END IF

!
!  WET REMOVAL
!
  call ESMF_ConfigGetAttribute (   CF, label='wet_removal:',      value=spec%wet_removal, default=.FALSE., __RC__ )
  IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": wet_removal ",spec%wet_removal

  IF ( spec%wet_removal ) THEN

     call ESMF_ConfigGetAttribute ( CF, label='rel_scav_eff:',     value=spec%rel_scav_eff,     __RC__ )
     call ESMF_ConfigGetAttribute ( CF, label='hstar:',            value=spec%hstar,            __RC__ )
     call ESMF_ConfigGetAttribute ( CF, label='delH_298_over_R:',  value=spec%delH_298_over_R,  __RC__ )
     call ESMF_ConfigGetAttribute ( CF, label='retention_eff:',    value=spec%retention_eff,    __RC__ )
     call ESMF_ConfigGetAttribute ( CF, label='treat_as_aerosol:', value=spec%aero_flag,        __RC__ )
     call ESMF_ConfigGetAttribute ( CF, label='treat_as_h2o2:',    value=spec%h2o2_flag,        __RC__ )
     call ESMF_ConfigGetAttribute ( CF, label='treat_as_hno3:',    value=spec%hno3_flag,        __RC__ )
     call ESMF_ConfigGetAttribute ( CF, label='mw:',               value=spec%mw,               __RC__ )

     IF(MAPL_AM_I_ROOT().AND.verbose) PRINT *,myname,": wet removal parameters ", &
         spec%rel_scav_eff, spec%hstar, spec%delH_298_over_R, spec%retention_eff, &
         spec%aero_flag, spec%h2o2_flag, spec%hno3_flag, spec%mw

  END IF


  RETURN_(ESMF_SUCCESS)

! End of MAIN BODY of  TR_init_tracer_spec_


CONTAINS

   SUBROUTINE init_( )

   INTEGER ios
   ALLOCATE (            spec%TRsfcFlux(i1:i2,j1:j2), &
                        spec%tropp_prev(i1:i2,j1:j2), &
                    spec%src_mask_horiz(i1:i2,j1:j2), &
                    spec%snk_mask_horiz(i1:i2,j1:j2), &
                        STAT=ios )

   IF ( ios /= 0 ) THEN
     rc=100
   ELSE
     rc=ESMF_SUCCESS
   END IF
   END SUBROUTINE init_

   SUBROUTINE final_(ierr)
   INTEGER :: ierr
   INTEGER ios
   DEALLOCATE ( spec%TRsfcFlux, spec%tropp_prev, spec%src_mask_horiz, spec%snk_mask_horiz, STAT=ios )
   rc = ierr
   END SUBROUTINE final_

!-------------------------------------------------------------------------
!     NASA/GSFC  Atmospheric Chemistry and Dynamics Lab   Code 614       !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  mask_on_longitude_ --- Determine which gridboxes fall into
!                                    a longitude range
!
! !INTERFACE:
!

  SUBROUTINE mask_on_longitude_ ( lons, west, east, mask )

! !USES:

  IMPLICIT NONE

! !INPUT PARAMETERS:

    REAL*8,  POINTER, DIMENSION(:,:), INTENT(IN)    :: lons           ! Longitudes in degrees, 0 to 360
    REAL,                             INTENT(IN)    :: west, east     ! Boundaries in the range -180 to 180

! !OUTPUT PARAMETERS:
    LOGICAL, POINTER, DIMENSION(:,:), INTENT(OUT)   :: mask

! !DESCRIPTION: Given east and west bounds and an array of longitudes,
!               set a mask
!
! !REVISION HISTORY:
!
!  30May2014   Manyin  First crack.
!
!EOP
!-------------------------------------------------------------------------

    REAL  :: e,w   ! copies of the input that we can alter

    ASSERT_( east >= -180. .AND. east <= 180. )
    ASSERT_( west >= -180. .AND. west <= 180. )

    e = east
    w = west

    mask = .FALSE.

    ! If they can be drawn as a single box between -180 and 180
    IF ( west < east ) THEN

      ! Both positive
      IF ( east >= 0. .AND. west >= 0. ) THEN
        WHERE ( w <= lons .AND. lons <= e ) mask = .TRUE.
      END IF

      ! Both negative
      IF ( east < 0. .AND. west < 0. ) THEN
        e = east + 360.
        w = west + 360.
        WHERE ( w <= lons .AND. lons <= e ) mask = .TRUE.
      END IF

      ! They straddle the Prime Meridian
      IF ( east >= 0. .AND. west < 0. ) THEN
        w = west + 360.
        WHERE ( w <= lons .OR.  lons <= e ) mask = .TRUE.
      END IF

    END IF

    ! If they straddle 180
    IF ( west > east ) THEN

      ! step 1   west to 180
      ! ------
      w = west
      e = 180.

      ! Both positive
      IF ( west >= 0. ) THEN
        WHERE ( w <= lons .AND. lons <= e ) mask = .TRUE.
      END IF

      ! They straddle the Prime Meridian
      IF ( west < 0. ) THEN
        w = w + 360.
        WHERE ( w <= lons .OR.  lons <= e ) mask = .TRUE.
      END IF

      ! step 2   -180 to east
      ! ------
      w = -180.
      e = east

      ! Both negative
      IF ( east < 0. ) THEN
        e = e + 360.
        w = w + 360.
        WHERE ( w <= lons .AND. lons <= e ) mask = .TRUE.
      END IF

      ! They straddle the Prime Meridian
      IF ( east >= 0. ) THEN
        w = w + 360.
        WHERE ( w <= lons .OR.  lons <= e ) mask = .TRUE.
      END IF

    END IF

  END SUBROUTINE mask_on_longitude_

 END SUBROUTINE TR_init_tracer_spec_



!-------------------------------------------------------------------------
!     NASA/GSFC  Atmospheric Chemistry and Dynamics Lab   Code 614       !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  TR_run_tracer_
!
! !INTERFACE:
!

   SUBROUTINE TR_run_tracer_ ( pet, kit, spec, qa, grid_esmf, impChem, expChem, nymd, nhms, cdt, rc )


! !USES:

  IMPLICIT NONE

! !INPUT/OUTPUT PARAMETERS:

   INTEGER,                 INTENT(IN)          :: pet        ! thread ID
   TYPE(TR_TracerKit),      INTENT(IN), pointer :: kit        ! A set of values common to all passive tracers
   INTEGER,                 INTENT(IN)          :: nymd, nhms ! time
   REAL,                    INTENT(IN)          :: cdt        ! chemical timestep (secs)
   TYPE(ESMF_Grid)                              :: grid_esmf  ! Grid



! !OUTPUT PARAMETERS:

   TYPE(TR_TracerSpec),     INTENT(INOUT)       :: spec       ! Specification for a Passive Tracer
   TYPE(MAPL_SimpleBundle), INTENT(INOUT)       :: qa         ! passive tracer fields
   TYPE(ESMF_State),        INTENT(INOUT)       :: impChem    ! Import State
   TYPE(ESMF_State),        INTENT(INOUT)       :: expChem    ! Export State
   INTEGER, OPTIONAL,       INTENT(  OUT)       :: RC         ! Error return code:
                                                              !  0 - all is well
                                                              !  1 - 

 
! !DESCRIPTION: This routine implements source/sink algorithm for a Passive Tracer
!
! !REVISION HISTORY:
!
!   3Jun2014  Manyin   First crack.
!
!EOP
!-------------------------------------------------------------------------

   CHARACTER(LEN=*), PARAMETER :: myname = 'TR_run_tracer_'
   CHARACTER(LEN=*), PARAMETER :: Iam = myname

!  Input fields from fvGCM
!  -----------------------
   REAL, POINTER, DIMENSION(:,:,:) :: T       => null()
   REAL, POINTER, DIMENSION(:,:,:) :: ple     => null()
   REAL, POINTER, DIMENSION(:,:,:) :: zle     => null()
   REAL, POINTER, DIMENSION(:,:)   :: soilT   => null()
   REAL, POINTER, DIMENSION(:,:)   :: fracIce => null()
   REAL, POINTER, DIMENSION(:,:)   :: tropp   => null()

   REAL, POINTER, DIMENSION(:,:,:) :: Q       => null()
   REAL, POINTER, DIMENSION(:,:,:) :: lwi     => null()

   INTEGER :: i, j, i1, i2, j1, j2, im, jm, km, ios, idiag
   INTEGER :: k, n
   INTEGER :: phase_index
   INTEGER :: ier(8)
   INTEGER :: k1, k2
   INTEGER :: index_in_bundle
   LOGICAL :: reclaim        ! this tracer decays into another tracer
   INTEGER :: receiver_index ! index of the other tracer

   REAL, PARAMETER :: Na=6.022E+23
   REAL, PARAMETER :: rstar=8.3143E+03   ! J/mol/K

   REAL    :: decadence
   REAL, ALLOCATABLE :: nd(:,:,:),p(:,:,:),dZ(:,:,:),tropPa(:,:)
   LOGICAL, ALLOCATABLE :: src_mask3d(:,:,:), snk_mask3d(:,:,:)
   INTEGER, ALLOCATABLE :: mask(:,:)

#define TREMnh5     TR_emis
#define TRCLnh5     TR_column
#define TRSCnh5     TR_surface
#define TRLSnh5     TR_loss

   integer :: STATUS
   LOGICAL :: need_to_free_groupA                  ! Deallocate these when finished: p, nd, dZ, tropPa

   REAL(KIND=DBL),  ALLOCATABLE ::   data_double(:,:,:)   ! For computing chemical loss

   real, pointer, dimension(:,:,:) ::  airdens
   real, pointer, dimension(:,:,:) ::  airdens_dry
   real, pointer, dimension(:,:)   ::  area
   real, pointer, dimension(:,:,:) ::  src_field
   real, pointer, dimension(:,:)   ::  src_ext_2d  ! For getting ExtData pointer
   real, pointer, dimension(:,:,:) ::  src_ext_3d  ! For getting ExtData pointer
   real, pointer, dimension(:,:,:) ::  imp_3d_A    ! For getting ExtData pointer
   real, pointer, dimension(:,:,:) ::  imp_3d_B    ! For getting ExtData pointer
   real, pointer, dimension(:,:,:) ::  imp_3d_C    ! For getting ExtData pointer
   real, pointer, dimension(:,:,:) ::  imp_3d_D    ! For getting ExtData pointer
   real, pointer, dimension(:,:,:) ::  imp_3d_E    ! For getting ExtData pointer
   real, pointer, dimension(:,:,:) ::  imp_3d_F    ! For getting ExtData pointer
   real, pointer, dimension(:,:,:) ::  imp_3d_G    ! For getting ExtData pointer
   real, pointer, dimension(:,:,:) ::  imp_3d_H    ! For getting ExtData pointer
   real, pointer, dimension(:,:,:) ::  imp_3d_I    ! For getting ExtData pointer
   real, pointer, dimension(:,:)   ::  imp_2d_A    ! For getting ExtData pointer

   REAL, ALLOCATABLE ::  local_realmask(:,:)
   REAL(KIND=DBL)    ::  global_kmoles
   REAL(KIND=DBL)    ::  global_area               ! Global area considering the mask values (m^2)
   REAL(KIND=DBL)    ::  local_tally
   REAL              ::  total_kmoles              ! Same as global_kmoles, but in real*4
   REAL              ::  total_area                ! Global area considering the mask values (m^2) in real*4

!  For gravitational settling / dry deposition:
   REAL,           ALLOCATABLE ::             diffaer(:,:)
   REAL,           ALLOCATABLE ::            s_radius(:,:)
   REAL,           ALLOCATABLE ::          s_velocity(:,:)
   REAL,           ALLOCATABLE ::                mass(:,:,:)
   REAL,           ALLOCATABLE ::               geoht(:,:,:)
   REAL(KIND=DBL), ALLOCATABLE :: cosSolarZenithAngle(:,:)

!  For chemical loss
   REAL*8,         ALLOCATABLE ::           loss_term(:,:,:)
   REAL*8,         ALLOCATABLE ::            sum_term(:,:,:)
   REAL*8,         ALLOCATABLE ::            dep_term(:,:)

   INTEGER, ALLOCATABLE ::      lwi_flags(:,:)

   LOGICAL :: debug_verbose = .FALSE.
   TYPE(ESMF_VM) :: vm


! #include "TR_GetPointer___.h"

!  Initialize local variables
!  --------------------------
   rc = ESMF_SUCCESS   ! zero

   i1 = kit%i1
   i2 = kit%i2

   j1 = kit%j1
   j2 = kit%j2

   im = kit%im
   jm = kit%jm
   km = kit%km

   need_to_free_groupA = .FALSE.

   IF(debug_verbose) PRINT *,myname, pet, "DEBUG ", TRIM(spec%name), " begin "


   index_in_bundle = MAPL_SimpleBundleGetIndex(qa, spec%name, 3, __RC__)
! shorthand
#define DATA3d  qa%r3(index_in_bundle)%q

   ASSERT_( (i1 == LBOUND(DATA3d,1)) .AND. (i2 == UBOUND(DATA3d,1)) .AND. (j1 == LBOUND(DATA3d,2)) .AND. (j2 == UBOUND(DATA3d,2)) )

   IF(debug_verbose .AND. MAPL_AM_I_ROOT()) THEN
     PRINT *,"Tracer NAME ",TRIM(spec%name)
     IF((TRIM(spec%src_mode) == "only_from_decay") .OR. (TRIM(spec%src_mode) == "none")) THEN
       PRINT *,"   SOURCE ",TRIM(spec%src_mode)
     ELSE
       PRINT *,"   SOURCE ",TRIM(spec%src_mode), " ",TRIM(spec%src_horiz), " ",TRIM(spec%src_vert)
     ENDIF
     IF(TRIM(spec%snk_mode) == "none" ) THEN
       PRINT *,"   SINK ",  TRIM(spec%snk_mode)
     ELSE
       PRINT *,"   SINK ",  TRIM(spec%snk_mode)," ", TRIM(spec%snk_horiz)," ", TRIM(spec%snk_vert)
     ENDIF
   ENDIF

   IF(debug_verbose) PRINT *,myname, pet, "DEBUG done setup"

!  Get regions mask if needed
   IF ( spec%regions_ExtData_entry == NULL_REGION_MASK ) THEN
     spec%regions_array    => NULL()
   ELSE
     call MAPL_GetPointer( impChem, spec%regions_array, TRIM(spec%regions_ExtData_entry), __RC__ ) 
   END IF


   WHERE(DATA3d < 0.0) DATA3d = 0.0    !  Force non-negative mixing ratios before the work

!    We will always use a 3d mask for src and snk
     ALLOCATE( src_mask3d(i1:i2,j1:j2,km), &
               snk_mask3d(i1:i2,j1:j2,km), __STAT__)

     IF(debug_verbose) PRINT *,myname, pet, "DEBUG done with mask alloc"

!                   SRC                                          SNK
     IF ( TRIM(spec%src_vert) == "pressures"    .OR.   TRIM(spec%snk_vert) == "pressures"    .OR. &
          TRIM(spec%src_vert) == "troposphere"  .OR.   TRIM(spec%snk_vert) == "troposphere"  .OR. &
          TRIM(spec%src_vert) == "stratosphere" .OR.   TRIM(spec%snk_vert) == "stratosphere" .OR. &
          TRIM(spec%src_mode) == "file2d"       .OR. &
          TRIM(spec%src_mode) == "maintain_mixing_ratio" ) THEN

       IF(debug_verbose) PRINT *,myname, pet, "DEBUG start groupA"

!      Allocate temporary workspace
!      ----------------------------
       ALLOCATE(  p( i1:i2, j1:j2, 1:km), &
                 nd( i1:i2, j1:j2, 1:km), &
                 dZ( i1:i2, j1:j2, 1:km), &
             tropPa( i1:i2, j1:j2),    __STAT__)

       need_to_free_groupA = .TRUE.

!      Get imports
!      -----------
       call MAPL_GetPointer( impChem,       T,      'T', __RC__ ) 
       call MAPL_GetPointer( impChem,     ple,    'PLE', __RC__ ) 
       call MAPL_GetPointer( impChem,     zle,    'ZLE', __RC__ ) 
!      call MAPL_GetPointer( impChem,   soilT, 'TSOIL1', __RC__ ) 
       call MAPL_GetPointer( impChem, fracIce,  'FRACI', __RC__ ) 
       call MAPL_GetPointer( impChem,   tropp,  'TROPP', __RC__ ) 


!      Fix bad tropopause pressure values if they exist.
!      (As in GMIchem GC, search: most recent valid tropopause)
!      --------------------------------------------------------

       WHERE( tropp /= MAPL_UNDEF )
         tropPa = tropp                ! use the good values
         spec%tropp_prev = tropp       ! update the fallback values
         p(:,:,1) = 0.0                ! tally
       ELSEWHERE
         tropPa = spec%tropp_prev      ! use the fallback values
         p(:,:,1) = 1.0                ! tally
       ENDWHERE

!      IF ( SUM(p(:,:,1)) > 0.0 ) print*,"THERE ARE BAD VALUES IN TROPP ",SUM(p(:,:,1))

!      Previous approach -- averages values of neighbors (but only those on the local CPU)
!      CALL Chem_UtilTroppFixer(i2, j2, tropp, VERBOSE=.TRUE., NEWTROPP=tropPa, __RC__ )



!      Layer thicknesses     ZLE(:,:,0:km)  [m]
!      -----------------------------------
       dZ(i1:i2,j1:j2,1:km) =  zle(i1:i2,j1:j2, 0:km-1 ) - zle(i1:i2,j1:j2,  1:km  )


!      Layer mean pressures  ple(:,:,1:km)  [Pa]
!      -----------------------------------
        p(i1:i2,j1:j2,1:km) = (PLE(i1:i2,j1:j2,  1:km  ) + PLE(i1:i2,j1:j2, 0:km-1 ))*0.50


!      Number density  [molec/m3]
!      --------------
       nd(i1:i2,j1:j2,1:km)=    Na*p(i1:i2,j1:j2,1:km)/ &
                            (rstar*T(i1:i2,j1:j2,1:km))
 

       IF(debug_verbose) PRINT *,myname, pet, "DEBUG done groupA"

     END IF

!!!
!!!  Set up the SOURCE 3d mask
!!!

!    Init
     src_mask3d(:,:,:) = .FALSE.

!    First set the Vertical extent
     IF ( TRIM(spec%src_vert) == "all" )           src_mask3d         = .TRUE.

     IF ( TRIM(spec%src_vert) == "surface" )       src_mask3d(:,:,km) = .TRUE.

     IF ( TRIM(spec%src_vert) == "levels" )        src_mask3d(:,:, spec%src_lev1:spec%src_lev2 ) = .TRUE.

     IF ( TRIM(spec%src_vert) == "pressures" ) THEN
       WHERE ( (p .GE. spec%src_press1) .AND.  &
               (p .LE. spec%src_press2) )          src_mask3d         = .TRUE.
     END IF

     IF ( TRIM(spec%src_vert) == "troposphere" ) THEN
       DO k=1,km
         WHERE ( p(:,:,k) .GE. tropPa(:,:) )       src_mask3d(:,:,k)  = .TRUE.
       END DO
     END IF

     IF ( TRIM(spec%src_vert) == "stratosphere" ) THEN
       DO k=1,km
         WHERE ( p(:,:,k) .LE. tropPa(:,:) )       src_mask3d(:,:,k)  = .TRUE.
       END DO
     END IF

!    Then add in the horizontal extent
     DO k=1,km
       src_mask3d(:,:,k) = src_mask3d(:,:,k) .AND. spec%src_mask_horiz(:,:) 
     END DO


!!!
!!!  Set up the SINK 3d mask
!!!

!    Init
     snk_mask3d(:,:,:) = .FALSE.

!    First set the Vertical extent
     IF ( TRIM(spec%snk_vert) == "all" )           snk_mask3d         = .TRUE.

     IF ( TRIM(spec%snk_vert) == "surface" )       snk_mask3d(:,:,km) = .TRUE.

     IF ( TRIM(spec%snk_vert) == "levels" )        snk_mask3d(:,:, spec%snk_lev1:spec%snk_lev2 ) = .TRUE.

     IF ( TRIM(spec%snk_vert) == "pressures" ) THEN
       WHERE ( (p .GE. spec%snk_press1) .AND.  &
               (p .LE. spec%snk_press2) )          snk_mask3d         = .TRUE.
     END IF

     IF ( TRIM(spec%snk_vert) == "troposphere" ) THEN
       DO k=1,km
         WHERE ( p(:,:,k) .GE. tropPa(:,:) )       snk_mask3d(:,:,k)  = .TRUE.
       END DO
     END IF

     IF ( TRIM(spec%snk_vert) == "stratosphere" ) THEN
       DO k=1,km
         WHERE ( p(:,:,k) .LE. tropPa(:,:) )       snk_mask3d(:,:,k)  = .TRUE.
       END DO
     END IF

!    Then add in the horizontal extent
     DO k=1,km
       snk_mask3d(:,:,k) = snk_mask3d(:,:,k) .AND. spec%snk_mask_horiz(:,:) 
     END DO


     DO phase_index=1,2
     SELECT CASE (spec%run_order(phase_index))
       CASE (SOURCE_PHASE)
!!!!!
!!!!!    SOURCE section
!!!!!

         IF(debug_verbose) PRINT *,myname, pet, "SOURCE section"

         SRC_constant: &
         IF ( TRIM(spec%src_mode) == "constant" ) THEN

           IF(debug_verbose) PRINT *,myname, pet, "DEBUG src const start"

! still to do:  HORIZ = masks

           IF ( TRIM(spec%src_vert) == "surface" .AND. spec%surface_constraint_count > 0 ) THEN

             spec%TRsfcFlux = spec%src_value

!            Apply spec%constraints on the values in spec%TRsfcFlux
             CALL impose_surface_constraints( kit, spec, impChem, grid_esmf )

             IF ( spec%src_add ) THEN
               WHERE(spec%src_mask_horiz) DATA3d(:,:,km) =  DATA3d(:,:,km) +  spec%TRsfcFlux
             ELSE
               WHERE(spec%src_mask_horiz) DATA3d(:,:,km) =                    spec%TRsfcFlux
             ENDIF

           ELSE

             IF ( spec%src_add ) THEN
               WHERE ( src_mask3d ) DATA3d = &
                                    DATA3d + spec%src_value
             ELSE
               WHERE ( src_mask3d ) DATA3d = spec%src_value
             END IF

           END IF

           IF(debug_verbose) PRINT *,myname, pet, "DEBUG src const finish"

         END IF SRC_constant

         SRC_file2d: &
         IF ( TRIM(spec%src_mode) == "file2d" ) THEN
           IF(debug_verbose) PRINT *,myname, pet, "DEBUG src file2d start"

           call MAPL_GetPointer( impChem, src_ext_2d, 'SRC_2D_'//TRIM(spec%name), __RC__ ) 

           spec%TRsfcFlux = src_ext_2d

           IF ( TRIM(spec%src_vert ) .NE. "surface"  ) THEN
             IF(MAPL_AM_I_ROOT()) PRINT *,"file2d requires vert=surface"
             rc = 13
             VERIFY_(rc)
           ENDIF

!          Apply spec%constraints on the values in spec%TRsfcFlux
           CALL impose_surface_constraints( kit, spec, impChem, grid_esmf )

!          Units conversion
!          NOTES from 4/7/14 and 8/28/14    emiss * cdt / dZ / molwt * Na*1000  /nd
           spec%TRsfcFlux = ((cdt * Na) / (spec%mw)) * spec%TRsfcFlux / ( dZ(:,:,km) * nd(:,:,km) )

           IF ( spec%src_add ) THEN
             WHERE(spec%src_mask_horiz) DATA3d(:,:,km) =  DATA3d(:,:,km) +  spec%TRsfcFlux
           ELSE
             WHERE(spec%src_mask_horiz) DATA3d(:,:,km) =                    spec%TRsfcFlux
           ENDIF


           IF(debug_verbose) PRINT *,myname, pet, "DEBUG src file2d finish"

         ENDIF SRC_file2d


         SRC_file3d: &
         IF ( TRIM(spec%src_mode) == "file3d" ) THEN
           IF(debug_verbose) PRINT *,myname, pet, "DEBUG src file3d start"

           call MAPL_GetPointer( impChem, src_ext_3d, 'SRC_3D_'//TRIM(spec%name), __RC__ ) 

           spec%TRvolFlux = src_ext_3d


           ! To do: Impose constraints on the SOURCE  --- instead of SURFACE constraints, use VOLUME constraints?

           ! To do: Resolve inconsistency of 3D units (mol/mol/s) vs 2D (kg/m2/s)


           IF ( spec%src_add ) THEN
             WHERE( src_mask3d ) DATA3d =  DATA3d +  cdt * spec%TRvolFlux
           ELSE
             WHERE( src_mask3d ) DATA3d =            cdt * spec%TRvolFlux
           ENDIF

           IF(debug_verbose) PRINT *,myname, pet, "DEBUG src file3d finish"

         ENDIF SRC_file3d

         SRC_maintain: &
         IF ( TRIM(spec%src_mode) == "maintain_mixing_ratio" ) THEN

           IF(debug_verbose) PRINT *,myname, pet, "DEBUG src mmr start"

           IF ( TRIM(spec%src_vert ) .NE. "surface"  ) THEN
             IF(MAPL_AM_I_ROOT()) PRINT *,"maintain_mixing_ratio requires vert=surface"
             rc = 15
             VERIFY_(rc)
           ENDIF

           IF ( TRIM(spec%src_horiz) == "all"          .OR.   &
                TRIM(spec%src_horiz) == "lat_zone"     .OR.   &
                TRIM(spec%src_horiz) == "latlon_box" ) THEN

             call ESMF_VmGetCurrent(vm, __RC__)

!            Compute the surface flux needed to restore the total burden

             ALLOCATE( local_realmask(i1:i2,j1:j2),  __STAT__)

             call MAPL_GetPointer ( impChem, airdens, 'AIRDENS', __RC__ )
             call MAPL_GetPointer ( impChem, area,    'AREA',    __RC__ )

!            Compute kmol of Tracer  needed to achieve the desired value
             local_tally = 0.0_8
             DO k=1,km
               DO j=j1,j2
                 DO i=i1,i2
                   local_tally = local_tally + (spec%src_value - DATA3d(i,j,k)) *  &
                                               (nd(i,j,k)/Na) * area(i,j) * dZ(i,j,k)
                 END DO
               END DO
             END DO

             call MAPL_CommsAllReduceSum(vm, sendbuf=local_tally, recvbuf=global_kmoles, cnt=1, __RC__)

             ! To preserve layout regression with very small values, cast the global sum at r8 back
             ! to r4 and use that.
             total_kmoles = real(global_kmoles, kind=MAPL_R4)

!            To distribute it uniformly on the surface, compute the total area
             local_realmask(:,:) = 0.0
             WHERE( spec%src_mask_horiz ) local_realmask = 1.0

             local_tally = 0.0_8
             do j=j1,j2
                do i=i1,i2
                   local_tally = local_tally + area(i,j)*local_realmask(i,j)
                enddo
             enddo
             call MAPL_CommsAllReduceSum(vm, sendbuf=local_tally, recvbuf=global_area, cnt=1, __RC__)

             ! To preserve layout regression with very small values, cast the global sum at r8 back
             ! to r4 and use that.
             total_area = real(global_area, kind=MAPL_R4)

             ! Use the r4 versions of the global sums
             spec%TRsfcFlux = (total_kmoles/total_area) * local_realmask

             DEALLOCATE( local_realmask, __STAT__)

!            TRsfcFlux / dZ    kmol / m^3
!            * Na              molec/ m^3
!            / nd              mix rat

!            src_add MUST be true
             DATA3d(:,:,km) =  &
             DATA3d(:,:,km) +  spec%TRsfcFlux * Na / ( dZ(:,:,km) * nd(:,:,km) )

           ELSE
             IF(MAPL_AM_I_ROOT()) PRINT *,"maintain_mixing_ratio requires horiz=all | lat_zone | latlon_box"
             rc = 19
             VERIFY_(rc)
           ENDIF

           IF(debug_verbose) PRINT *,myname, pet, "DEBUG src mmr finish"

         ENDIF SRC_maintain

         SRC_model_field: &
         IF ( TRIM(spec%src_mode) == "model_field" ) THEN

           IF(debug_verbose) PRINT *,myname, pet, "DEBUG src field start"

!          We assume that the field has been exported from the source GC, connected to TR,
!          and imported by TR.  Otherwise this call will fail:
           call MAPL_GetPointer( impChem,  src_field, spec%src_field_name, rc=ier(1) ) 

           IF( ier(1) /= 0 ) THEN
            print*,'TR problem getting PTR for field '//spec%src_field_name
            rc = 20
            VERIFY_(rc)
           END IF

           IF ( spec%src_add ) THEN
             WHERE( src_mask3d ) DATA3d =  DATA3d +  src_field
           ELSE
             WHERE( src_mask3d ) DATA3d =            src_field
           ENDIF

           IF(debug_verbose) PRINT *,myname, pet, "DEBUG src field finish"

         END IF SRC_model_field
       CASE (SINK_PHASE)
!!!!!
!!!!!    SINK section
!!!!!

         IF(debug_verbose) PRINT *,myname, pet, "SINK section"

         SNK_constant: &
         IF ( TRIM(spec%snk_mode) == "constant" ) THEN

           IF(debug_verbose) PRINT *,myname, pet, "DEBUG snk const start"

!     still to do:  HORIZ = masks

           WHERE ( snk_mask3d ) DATA3d = spec%snk_value

           IF(debug_verbose) PRINT *,myname, pet, "DEBUG snk const finish"

         END IF SNK_constant

         SNK_efolding: &
         IF ( (TRIM(spec%snk_mode) == "efolding") .OR.  &
              (TRIM(spec%snk_mode) == "halflife")   )  THEN

           IF(debug_verbose) PRINT *,myname, pet, "DEBUG snk Efolding start"

           decadence = EXP(-spec%decayConstant*cdt)
!          IF(MAPL_AM_I_ROOT()) PRINT *,"Tracer SINK ",TRIM(spec%snk_mode), decadence

! still to do:  HORIZ = masks

           IF ( TRIM(spec%decays_to) == NO_DECAY ) THEN
             reclaim = .FALSE.
           ELSE
             reclaim = .TRUE.
             receiver_index = MAPL_SimpleBundleGetIndex(qa, TRIM(spec%decays_to), 3, __RC__)
#define RECEIVER  qa%r3(receiver_index)%q
           END IF

           IF ( reclaim ) THEN
             WHERE( snk_mask3d ) RECEIVER = &
                                 RECEIVER + DATA3d * (1-decadence)
           END IF

           WHERE( snk_mask3d ) DATA3d = &
                               DATA3d * decadence

#undef RECEIVER
           IF(debug_verbose) PRINT *,myname, pet, "DEBUG snk Efolding finish"

         END IF SNK_efolding

         SNK_chemical_loss:  &
         IF ( TRIM(spec%snk_mode) == "chemical_loss" ) THEN

           IF(debug_verbose) PRINT *,myname, pet, "DEBUG snk chemloss start"

           ALLOCATE( data_double(i1:i2,j1:j2,1:km),  &
                       loss_term(i1:i2,j1:j2,1:km),  &
                        dep_term(i1:i2,j1:j2),       &
                        sum_term(i1:i2,j1:j2,1:km),  __STAT__)

           IF ( TRIM(spec%loss_species) == 'O3' ) THEN

!            Use volume mixing ratio
             data_double = DBLE(DATA3d)

!            We trust that the fields have been exported from the source GC, connected to TR,
!            and imported by TR.  Otherwise these calls will fail:

!            These should all be in terms of mole/m3/s
             call MAPL_GetPointer( impChem,  imp_3d_A, 'QQK007', __RC__ )
             call MAPL_GetPointer( impChem,  imp_3d_B, 'QQK027', __RC__ )
             call MAPL_GetPointer( impChem,  imp_3d_C, 'QQK028', __RC__ )
             call MAPL_GetPointer( impChem,  imp_3d_D, 'QQK005', __RC__ )
             call MAPL_GetPointer( impChem,  imp_3d_E, 'QQK204', __RC__ )
             call MAPL_GetPointer( impChem,  imp_3d_F, 'QQK222', __RC__ )
             call MAPL_GetPointer( impChem,  imp_3d_G, 'QQK223', __RC__ )
             call MAPL_GetPointer( impChem,  imp_3d_H, 'QQK224', __RC__ )
             call MAPL_GetPointer( impChem,  imp_3d_I, 'QQK253', __RC__ )
!            This is in terms of kg/m2/s
             call MAPL_GetPointer( impChem,  imp_2d_A, 'DD_OX' , __RC__ )
!            This is in terms of kg/m3
             call MAPL_GetPointer ( impChem, airdens_dry, 'AIRDENS_DRY', __RC__ )

!            For debugging
!             call MAPL_GetPointer( expChem,  exp_3d_A, 'TR_DIAG_A', __RC__ )
!             call MAPL_GetPointer( expChem,  exp_3d_B, 'TR_DIAG_B', __RC__ )
  
!            Convert kg m-2 s-1 to mole/mole/s
             dep_term = ((imp_2d_A/MAPL_O3MW)/ dZ(:,:,km))/(airdens_dry(:,:,km)/MAPL_AIRMW)     

!            force computation to be done in double-precision
!            and guard against division by zero and convert from mole/m3/s to mole/mole/s
             sum_term = ( DBLE(imp_3d_A) + DBLE(imp_3d_B) + DBLE(imp_3d_C) + DBLE(imp_3d_D) + DBLE(imp_3d_E) &
                          + DBLE(imp_3d_F) + DBLE(imp_3d_G) + DBLE(imp_3d_H) + DBLE(imp_3d_I) ) / (airdens_dry*(1.0D3/MAPL_AIRMW))
             sum_term(:,:,km) = sum_term (:,:,km) + dep_term

             loss_term = 0.0
             WHERE( sum_term > 1.E-30 ) loss_term = data_double / sum_term

             WHERE( snk_mask3d .AND. (loss_term > 1.E-30) ) data_double = data_double * exp(-cdt/loss_term)

!            Leave as volume mixing ratio
             DATA3d = data_double 

           ENDIF

           DEALLOCATE( data_double, loss_term, dep_term, sum_term, __STAT__)

          
           IF(debug_verbose) PRINT *,myname, pet, "DEBUG snk chemloss finish"

         END IF SNK_chemical_loss

         IF ( spec%dry_deposition ) CALL TR_settle_and_depos(DATA3d, impChem, nymd, nhms, kit, cdt, spec)

         IF ( spec%wet_removal    ) CALL      TR_wet_removal(DATA3d, impChem, kit, cdt, spec)

       CASE DEFAULT
         IF(debug_verbose) PRINT *,myname, pet, "NO-OP section"
     END SELECT
     END DO  ! phase_index


     IF ( need_to_free_groupA ) THEN

       DEALLOCATE( p, nd, dZ, tropPa, __STAT__)

     END IF

     DEALLOCATE(src_mask3d, snk_mask3d, __STAT__)

   WHERE(DATA3d < 0.0) DATA3d = 0.0    !  Force non-negative mixing ratios after the work

   IF(debug_verbose) PRINT *,myname, pet, "DEBUG ", TRIM(spec%name), " end"

#undef DATA3d

   RETURN

CONTAINS


!
! Apply any surface constraints to the surface flux field
!
  SUBROUTINE impose_surface_constraints( kit, spec, impChem, grid_esmf )

    IMPLICIT NONE

!  Parameters
    TYPE(TR_TracerKit), POINTER,  INTENT(IN)    :: kit        ! A set of values common to all passive tracers
    TYPE(TR_TracerSpec),          INTENT(INOUT) :: spec       ! Specification for a Passive Tracer
    TYPE(ESMF_State),             INTENT(INOUT) :: impChem    ! Import State
    TYPE(ESMF_Grid)                             :: grid_esmf  ! Grid

!  Local vars
    TYPE(ESMF_Field),   POINTER                 :: fld            ! for evaluating Constraints
    REAL(ESMF_KIND_R4), POINTER                 :: fld_ptr(:,:)   ! for evaluating Constraints

    CHARACTER(LEN=*), PARAMETER                 :: Iam = 'impose_surface_constraints'

    INTEGER :: RC, STATUS
    INTEGER :: n
    INTEGER :: i1, i2, j1, j2

!  Initialize local variables
!  --------------------------
    RC = ESMF_SUCCESS   ! zero

    i1 = kit%i1
    i2 = kit%i2

    j1 = kit%j1
    j2 = kit%j2


    IF ( spec%surface_constraint_count > 0 ) THEN
      DO n = 1, spec%surface_constraint_count

        ! Determine WHERE to constrain -> set the bool_array
        ! A constraint may combine a regional extent and a boolean expression;
        ! if both are present, then (for a given gridbox) BOTH must be satisfied.

        spec%constraints(n)%bool_array(:,:) = .TRUE.

        IF ( spec%constraints(n)%use_regions ) THEN
          CALL set_regional_mask( kit,                             &
                                  spec%constraints(n)%regions_str, &
                                  spec%regions_array,              &
                                  spec%constraints(n)%bool_aux   )
          spec%constraints(n)%bool_array = spec%constraints(n)%bool_array .AND. spec%constraints(n)%bool_aux
        END IF

        IF ( spec%constraints(n)%use_bool ) THEN
          CALL set_bool_mask( kit,                             &
                              impChem,                         &
                              spec%constraints(n)%bool_FIELD,  &
                              spec%constraints(n)%bool_OP,     &
                              spec%constraints(n)%bool_SCALAR, &
                              spec%constraints(n)%bool_aux   )
          spec%constraints(n)%bool_array = spec%constraints(n)%bool_array .AND. spec%constraints(n)%bool_aux
        END IF

        ! Determine WHAT VALUES to use -> set the value_array
        ! (a constant or the result of a field-expression)

        !-----
        ! (Calling sequence for MAPL_StateEval taken from Chem_SimpleBundleCreate:)
        ALLOCATE(fld,fld_ptr(i1:i2,j1:j2), __STAT__)
        fld_ptr = MAPL_UNDEF
        fld = ESMF_FieldCreate(name='temp_f', grid=grid_esmf, &
          fArrayPtr=fld_ptr, dataCopyFlag=ESMF_DATACOPY_REFERENCE, __RC__ )

        CALL MAPL_StateEval( impChem, spec%constraints(n)%expr, fld, __RC__ )

        WHERE ( fld_ptr /= MAPL_UNDEF )
          spec%constraints(n)%value_array(:,:) = fld_ptr(:,:)
        ELSEWHERE
          spec%constraints(n)%value_array(:,:) = 0.0    ! Allow user to set this fallback value?
        ENDWHERE

        DEALLOCATE( fld, fld_ptr, __STAT__)
        !-----

        ! Constrain based on the mode
        IF ( TRIM(spec%constraints(n)%mode) == 'set' ) THEN
          WHERE( spec%constraints(n)%bool_array ) spec%TRsfcFlux =                  spec%constraints(n)%value_array
! IF(MAPL_AM_I_ROOT()) PRINT *,"sampling of Constraint SET Values:", spec%constraints(n)%value_array(1:3,1:3)
        END IF

        IF ( TRIM(spec%constraints(n)%mode) == 'scale' ) THEN
          WHERE( spec%constraints(n)%bool_array ) spec%TRsfcFlux = spec%TRsfcFlux * spec%constraints(n)%value_array
! IF(MAPL_AM_I_ROOT()) PRINT *,"sampling of Constraint SCALE Values:", spec%constraints(n)%value_array(1:3,1:3)
        END IF

      END DO
    END IF

  END SUBROUTINE impose_surface_constraints


  SUBROUTINE TR_settle_and_depos(data3d, impChem, nymd, nhms, kit, cdt, spec)
    IMPLICIT NONE

    REAL*4,  POINTER, DIMENSION(:,:,:), INTENT(INOUT)    :: data3d
    TYPE(ESMF_State),                   INTENT(INOUT)    :: impChem    ! Import State
    INTEGER,                            INTENT(IN)       :: nymd, nhms
    TYPE(TR_TracerKit),     POINTER,    INTENT(IN)       :: kit        ! A set of values common to all passive tracers
    REAL,                               INTENT(IN)       :: cdt        ! chemical timestep (secs)
    TYPE(TR_TracerSpec),                INTENT(IN)       :: spec       ! Specification for a Passive Tracer

    integer                      :: STATUS
    CHARACTER(LEN=*), PARAMETER  :: Iam = 'TR_settle_and_depos'

    !
    ! POINTERS
    !

    ! needed for Gravitational Settling
    REAL, POINTER, DIMENSION(:,:,:) :: Q       => null()
    REAL, POINTER, DIMENSION(:,:,:) :: airdens => null()
    REAL, POINTER, DIMENSION(:,:)   :: area    => null()
    REAL, POINTER, DIMENSION(:,:,:) :: zle     => null()
    REAL, POINTER, DIMENSION(:,:,:) :: T       => null()
    REAL, POINTER, DIMENSION(:,:,:) :: ple     => null()

    ! needed for Dry Deposition
    REAL, POINTER, DIMENSION(:,:)   :: lwi     => null()
    REAL, POINTER, DIMENSION(:,:)   :: T2m     => null()
    REAL, POINTER, DIMENSION(:,:)   :: cldtt   => null()
    REAL, POINTER, DIMENSION(:,:)   :: z0h     => null()
    REAL, POINTER, DIMENSION(:,:)   :: swndsrf => null()
    REAL, POINTER, DIMENSION(:,:)   :: ustar   => null()

    !
    ! AUTOMATIC ARRAYS
    !

    ! we fill these
    REAL                              mass( kit%i1:kit%i2, kit%j1:kit%j2, 1:kit%km )
    REAL                             geoht( kit%i1:kit%i2, kit%j1:kit%j2, 1:kit%km )
    INTEGER                      lwi_flags( kit%i1:kit%i2, kit%j1:kit%j2 )

    ! these will be filled by TR_GravitationalSettling
    REAL*8                         diffaer( kit%i1:kit%i2, kit%j1:kit%j2 )
    REAL*8                        s_radius( kit%i1:kit%i2, kit%j1:kit%j2 )
    REAL*8                      s_velocity( kit%i1:kit%i2, kit%j1:kit%j2 )

    ! this will be filled by set_cos_sza
    REAL(KIND=DBL)  :: cosSolarZenithAngle( kit%i1:kit%i2, kit%j1:kit%j2 )

    ! these are just DBL copies of REAL fields
    REAL(KIND=DBL)  ::   TwoMeter_air_temp( kit%i1:kit%i2, kit%j1:kit%j2 )
    REAL(KIND=DBL)  ::      fracCloudCover( kit%i1:kit%i2, kit%j1:kit%j2 )
    REAL(KIND=DBL)  ::          surf_rough( kit%i1:kit%i2, kit%j1:kit%j2 )
    REAL(KIND=DBL)  ::              radswg( kit%i1:kit%i2, kit%j1:kit%j2 )
    REAL(KIND=DBL)  ::    frictionVelocity( kit%i1:kit%i2, kit%j1:kit%j2 )

    ! odds and ends
    INTEGER  :: lev
    LOGICAL  :: pr_diag
    INTEGER  :: loc_proc
    INTEGER  :: chem_opt
    INTEGER  :: num_species


    !!!
    !!!  Gravitational Settling
    !!!

    CALL MAPL_GetPointer( impChem,       Q,       'Q', __RC__ ) 
    CALL MAPL_GetPointer( impChem, airdens, 'AIRDENS', __RC__ )
    CALL MAPL_GetPointer( impChem,    area,    'AREA', __RC__ )
    CALL MAPL_GetPointer( impChem,     zle,     'ZLE', __RC__ ) 
    CALL MAPL_GetPointer( impChem,       T,       'T', __RC__ ) 
    CALL MAPL_GetPointer( impChem,     ple,     'PLE', __RC__ ) 


    geoht(:,:,1:kit%km) =  zle(:,:, 0:kit%km-1 ) - zle(:,:,  1:kit%km  )

    mass = geoht * airdens
    DO lev=1,kit%km
      mass(:,:,lev) = mass(:,:,lev) * area
    END DO

    ! routine in a neighboring module
    CALL TR_GravitationalSettling ( data3d,                             &
       spec%aero_density, spec%aero_eff_radius,                         &
       spec%aero_c(1), spec%aero_c(2), spec%aero_c(3), spec%aero_c(4),  &
       cdt, geoht, Q, mass, ple, T,                                     &
       diffaer, s_radius, s_velocity,                                   &
       kit%i1, kit%i2, kit%j1, kit%j2, kit%km )

!     ------------------------------------------------------------------------
!     Deposition
!
!     NOTE: In GEOS-5, the "instantaneous" deposition is held in dry_depos and
!           wet_depos.  The accumulated deposition is obtained by instantiating
!           time-averaged exports via the HISTORY.rc in the run script.
!     ------------------------------------------------------------------------

 !  IF(self%pr_dry_depos) THEN
 !   ALLOCATE(var3dDBL(kit%i1:kit%i2,kit%j1:kit%j2,1:NSP),STAT=STATUS)


    ! var names as found in GmiDepos_GridCompClassMod.F90

    pr_diag = .FALSE.
    loc_proc = 999
    chem_opt = 999
    num_species = 1

   CALL MAPL_GetPointer(impChem,       lwi,       'LWI', __RC__ ) 
   lwi_flags = lwi + 1  ! 1=water 2=land 3=ice; as in GmiDepos_GridCompClassMod.F90

   CALL MAPL_GetPointer(impChem,       T2m,       'T2M', __RC__ )
   CALL MAPL_GetPointer(impChem,     cldtt,     'CLDTT', __RC__ )
   CALL MAPL_GetPointer(impChem,       z0h,       'Z0H', __RC__ )
   CALL MAPL_GetPointer(impChem,   swndsrf,   'SWNDSRF', __RC__ )
   CALL MAPL_GetPointer(impChem,     ustar,     'USTAR', __RC__ )

   TwoMeter_air_temp(:,:) =     T2m(:,:)       ! K
      fracCloudCover(:,:) =   cldtt(:,:)       ! fraction
          surf_rough(:,:) =     z0h(:,:)       ! m
              radswg(:,:) = swndsrf(:,:)       ! w m^{-2}
    frictionVelocity(:,:) =   ustar(:,:)       ! m s^{-1}


   CALL set_cos_sza(nymd, nhms, kit, cosSolarZenithAngle)

   ! routine in Chem_Shared; optional output-> 'dry_depos'
   CALL DryDepositionGMI ( lwi_flags, area, cosSolarZenithAngle,                         &
             fracCloudCover, radswg, TwoMeter_air_temp, surf_rough, frictionVelocity,    &
             mass(:,:,kit%km), diffaer, s_radius, s_velocity, geoht(:,:,kit%km),         &
             kit%ireg, kit%iland, kit%iuse, kit%xlai,                                    &
             spec%aero_flag, spec%hstar, spec%f0, spec%delH_298_over_R,   &
             pr_diag, loc_proc, chem_opt, cdt,                                           &
             kit%i1, kit%i2, kit%j1, kit%j2, 1, kit%km,                                  &
             spec%mw, one_species = data3d(:,:,kit%km) )

  END SUBROUTINE TR_settle_and_depos


  SUBROUTINE set_cos_sza(nymd, nhms, kit, cosSolarZenithAngle)

    USE GmiTimeControl_mod,            ONLY : GetSecondsFromJanuary1
    USE GmiSolar_mod,                  ONLY : CalcCosSolarZenithAngle

    IMPLICIT NONE

    INTEGER,                       INTENT(IN)  :: nymd, nhms
    TYPE(TR_TracerKit),  POINTER,  INTENT(IN)  :: kit        ! A set of values common to all passive tracers
    REAL(KIND=DBL),                INTENT(OUT) :: cosSolarZenithAngle( kit%i1:kit%i2, kit%j1:kit%j2 )

    INTEGER              :: ic
    REAL(KIND=DBL)       :: dayOfYear
    REAL, PARAMETER      :: secPerDay = 86400.0d0

    CALL GetSecondsFromJanuary1(ic, nymd, nhms)
    dayOfYear = (1.00*ic)/secPerDay
    CALL CalcCosSolarZenithAngle(dayOfYear, kit%lats, kit%lons, cosSolarZenithAngle, &
                                 kit%i1, kit%i2, kit%j1, kit%j2)
  END SUBROUTINE set_cos_sza

!
! Given a list of regional indices, and a 2D array, return a boolean array
! reflecting where any of the list indices exist in the 2D array.
!
  SUBROUTINE set_regional_mask( kit, regions_str, regions_array, bool_array )

    IMPLICIT NONE

    TYPE(TR_TracerKit), POINTER,  INTENT(IN)    :: kit         ! A set of values common to all passive tracers
    CHARACTER(LEN=255),           INTENT(IN)    :: regions_str                                    ! one or more indices
    REAL,                         INTENT(IN)    :: regions_array( kit%i1:kit%i2, kit%j1:kit%j2 )  ! as read from file
    LOGICAL,                      INTENT(INOUT) ::    bool_array( kit%i1:kit%i2, kit%j1:kit%j2 )  ! set these

    CHARACTER(LEN=*), PARAMETER     :: Iam = 'set_regional_mask'

    INTEGER, PARAMETER              :: kmax=32

    INTEGER :: regionNumbers(kmax)
    INTEGER ::          flag(kmax)
    INTEGER :: k  ! the number of meaningful entries in the regionNumbers array
    INTEGER :: fillValue, i, STATUS


    ! Assume:  0 = water;  non-zero = land

    IF ( UPPERCASE(TRIM(regions_str)) == "LAND" ) THEN
!     DO k=1,kmax
!       regionNumbers(k)=k
!     END DO
!     k=kmax
      regionNumbers(1)= -1  ! shortcut
    ELSE IF ( UPPERCASE(TRIM(regions_str)) == "WATER" ) THEN
      regionNumbers(1)=0
      k=1
    ELSE
      fillValue = 99

      ! Obtain region numbers from comma-delimited list of integers
      ! -----------------------------------------------------------
      CALL Chem_UtilExtractIntegers(regions_str,kmax,regionNumbers,fillValue=fillValue, __RC__ )

      ! How many integers were found?
      ! -----------------------------
      flag(:) = 1
      WHERE(regionNumbers(:) == fillValue) flag(:) = 0
      k = SUM(flag)
    END IF

    ! Set boolean entries to TRUE where gridMask matches each integer (within precision!).
    ! ----------------------------------------------------------------------------
    bool_array(:,:) = .FALSE.
    IF(regionNumbers(1) == -1) THEN
     WHERE(regions_array > 0.01) bool_array = .TRUE.
    ELSE
     DO i=1,k
      WHERE( regionNumbers(i)-0.01 <= regions_array .AND. &
                                      regions_array <= regionNumbers(i)+0.01)  bool_array = .TRUE.
     END DO
    END IF

   RETURN
  END SUBROUTINE set_regional_mask

!
! Given a Boolean expression ( field OP scalar ), return a boolean array.
!
  SUBROUTINE set_bool_mask( kit, impChem, bool_FIELD, bool_OP, bool_SCALAR, bool_array )

    IMPLICIT NONE

    TYPE(TR_TracerKit), POINTER,  INTENT(IN)    :: kit         ! A set of values common to all passive tracers
    TYPE(ESMF_State),             INTENT(INOUT) :: impChem     ! Import State
    CHARACTER(LEN=255),           INTENT(IN)    :: bool_FIELD
    CHARACTER(LEN=10 ),           INTENT(IN)    :: bool_OP
    REAL*4,                       INTENT(IN)    :: bool_SCALAR
    LOGICAL,                      INTENT(INOUT) :: bool_array( kit%i1:kit%i2, kit%j1:kit%j2 )  ! set these

    CHARACTER(LEN=*), PARAMETER                 :: Iam = 'set_bool_mask'
    INTEGER                                     :: STATUS

    REAL, POINTER, DIMENSION(:,:)               :: f2
    REAL, POINTER, DIMENSION(:,:,:)             :: f3

    TYPE(ESMF_Field)                            :: field
    INTEGER                                     :: dim_count

    bool_array=.FALSE.

    !
    ! Discover the dimension count (usu. == rank) of the field
    ! If 3D field, assume vertical centered (not edges) and use the bottom-most layer.
    !

    call ESMF_StateGet(impChem, TRIM(bool_FIELD), field, __RC__ )

    call ESMF_FieldGet(field, dimCount=dim_count, __RC__ )


    IF ( dim_count .EQ. 2 ) THEN

      call MAPL_GetPointer( impChem, f2, TRIM(bool_FIELD), __RC__ ) 

      IF ( (TRIM(bool_OP) == '.EQ.' ) .OR. (TRIM(bool_OP) == '==') )  WHERE( f2 .EQ. bool_SCALAR ) bool_array=.TRUE.
      IF ( (TRIM(bool_OP) == '.NE.' ) .OR. (TRIM(bool_OP) == '/=') )  WHERE( f2 .NE. bool_SCALAR ) bool_array=.TRUE.
      IF ( (TRIM(bool_OP) == '.LT.' ) .OR. (TRIM(bool_OP) == '<' ) )  WHERE( f2 .LT. bool_SCALAR ) bool_array=.TRUE.
      IF ( (TRIM(bool_OP) == '.GT.' ) .OR. (TRIM(bool_OP) == '>' ) )  WHERE( f2 .GT. bool_SCALAR ) bool_array=.TRUE.
      IF ( (TRIM(bool_OP) == '.LE.' ) .OR. (TRIM(bool_OP) == '<=') )  WHERE( f2 .LE. bool_SCALAR ) bool_array=.TRUE.
      IF ( (TRIM(bool_OP) == '.GE.' ) .OR. (TRIM(bool_OP) == '>=') )  WHERE( f2 .GE. bool_SCALAR ) bool_array=.TRUE.

    ELSE IF ( dim_count .EQ. 3 ) THEN

      call MAPL_GetPointer( impChem, f3, TRIM(bool_FIELD), __RC__ )

      IF ( (TRIM(bool_OP) == '.EQ.' ) .OR. (TRIM(bool_OP) == '==') )  WHERE( f3(:,:,kit%km) .EQ. bool_SCALAR ) bool_array=.TRUE.
      IF ( (TRIM(bool_OP) == '.NE.' ) .OR. (TRIM(bool_OP) == '/=') )  WHERE( f3(:,:,kit%km) .NE. bool_SCALAR ) bool_array=.TRUE.
      IF ( (TRIM(bool_OP) == '.LT.' ) .OR. (TRIM(bool_OP) == '<' ) )  WHERE( f3(:,:,kit%km) .LT. bool_SCALAR ) bool_array=.TRUE.
      IF ( (TRIM(bool_OP) == '.GT.' ) .OR. (TRIM(bool_OP) == '>' ) )  WHERE( f3(:,:,kit%km) .GT. bool_SCALAR ) bool_array=.TRUE.
      IF ( (TRIM(bool_OP) == '.LE.' ) .OR. (TRIM(bool_OP) == '<=') )  WHERE( f3(:,:,kit%km) .LE. bool_SCALAR ) bool_array=.TRUE.
      IF ( (TRIM(bool_OP) == '.GE.' ) .OR. (TRIM(bool_OP) == '>=') )  WHERE( f3(:,:,kit%km) .GE. bool_SCALAR ) bool_array=.TRUE.

    ELSE

      IF(MAPL_AM_I_ROOT()) THEN
        PRINT *,"BAD DimCount of Boolean expr field "//TRIM(bool_FIELD)
        STATUS = 234
        VERIFY_(STATUS)
      END IF

    END IF


   RETURN
  END SUBROUTINE set_bool_mask


  SUBROUTINE TR_wet_removal(data3d, impChem, kit, cdt, spec)
    IMPLICIT NONE

    REAL*4,  POINTER, DIMENSION(:,:,:), INTENT(INOUT)    :: data3d
    TYPE(ESMF_State),                   INTENT(INOUT)    :: impChem    ! Import State
    TYPE(TR_TracerKit),     POINTER,    INTENT(IN)       :: kit        ! A set of values common to all passive tracers
    REAL,                               INTENT(IN)       :: cdt        ! chemical timestep (secs)
    TYPE(TR_TracerSpec),                INTENT(IN)       :: spec       ! Specification for a Passive Tracer

    integer                      :: STATUS
    CHARACTER(LEN=*), PARAMETER  :: Iam = 'TR_wet_removal'

    !
    ! PARAMETERS
    !
    REAL, PARAMETER :: Pa2hPa = 0.01
    REAL, PARAMETER :: ToGrPerKg = 1000.00
    REAL, PARAMETER :: secPerDay = 86400.00

    !
    ! POINTERS
    !
    REAL, POINTER, DIMENSION(:,:,:) :: airdens  => null()
    REAL, POINTER, DIMENSION(:,:)   :: area     => null()
    REAL, POINTER, DIMENSION(:,:,:) :: zle      => null()
    REAL, POINTER, DIMENSION(:,:,:) :: T        => null()
    REAL, POINTER, DIMENSION(:,:,:) :: ple      => null()
    REAL, POINTER, DIMENSION(:,:,:) :: dqdt     => null()
    REAL, POINTER, DIMENSION(:,:,:) :: pfl_lsan => null()
    REAL, POINTER, DIMENSION(:,:,:) :: pfl_cn   => null()

    !
    ! AUTOMATIC ARRAYS
    !

    ! we fill these
    ! centers
    REAL              mass( kit%i1:kit%i2, kit%j1:kit%j2, 1:kit%km )
    REAL             geoht( kit%i1:kit%i2, kit%j1:kit%j2, 1:kit%km )
    REAL           press3c( kit%i1:kit%i2, kit%j1:kit%j2, 1:kit%km )  ! pressure at centers
    REAL            moistq( kit%i1:kit%i2, kit%j1:kit%j2, 1:kit%km )
    REAL          rain3Dls( kit%i1:kit%i2, kit%j1:kit%j2, 1:kit%km )
    REAL          rain3Dcn( kit%i1:kit%i2, kit%j1:kit%j2, 1:kit%km )
    ! edges
    REAL           press3e( kit%i1:kit%i2, kit%j1:kit%j2, 0:kit%km )  ! pressure at edges

    ! odds and ends
    INTEGER  :: lev

    ! some var names as found in GmiDepos_GridCompClassMod.F90

    CALL MAPL_GetPointer( impChem,   airdens,   'AIRDENS', __RC__ )
    CALL MAPL_GetPointer( impChem,      area,      'AREA', __RC__ )
    CALL MAPL_GetPointer( impChem,       zle,       'ZLE', __RC__ )
    CALL MAPL_GetPointer( impChem,         T,         'T', __RC__ )
    CALL MAPL_GetPointer( impChem,       ple,       'PLE', __RC__ )
    CALL MAPL_GetPointer( impChem,      dqdt,      'DQDT', __RC__ )
    CALL MAPL_GetPointer( impChem,  pfl_lsan,  'PFL_LSAN', __RC__ )
    CALL MAPL_GetPointer( impChem,    pfl_cn,    'PFL_CN', __RC__ )

      geoht(:,:,1:kit%km) =  zle(:,:, 0:kit%km-1 ) - zle(:,:,  1:kit%km  )
    press3c(:,:,1:kit%km) = (ple(:,:, 0:kit%km-1 ) + ple(:,:,  1:kit%km  ))/2.0   ! convert units below

    mass = geoht * airdens
    DO lev=1,kit%km
      mass(:,:,lev) = mass(:,:,lev) * area
    END DO

!   Layer means                                                   GEOS-5 Units       GMI Units
!   -----------                                                   ------------       -------------
    press3c  = press3c*Pa2hPa                                     ! Pa               hPa
    rain3Dls(:,:,1:kit%km) = pfl_lsan(:,:,0:kit%km-1) * secPerDay ! kg m^{-2}s^{-1}  mm d^{-1}
    rain3Dcn(:,:,1:kit%km) =   pfl_cn(:,:,0:kit%km-1) * secPerDay ! kg m^{-2}s^{-1}  mm d^{-1}
    moistq   = dqdt*ToGrPerKg*secPerDay                           ! kg kg^{-1}s^{-1} g kg^{-1}d^{-1}

!   Layer edges                                                   GEOS-5 Units       GMI Units
!   -----------                                                   ------------       -------------
    press3e = ple*Pa2hPa                                          ! Pa               hPa


!     ------------------------------------------------------------------------
!     NOTE: In GEOS-5, the "instantaneous" deposition is held in dry_depos and
!           wet_depos.  The accumulated deposition is obtained by instantiating
!           time-averaged exports via the HISTORY.rc in the run script.
!     ------------------------------------------------------------------------

!  IF(self%pr_wet_depos) THEN
!   ALLOCATE(var3dDBL(kit%i1:kit%i2,kit%j1:kit%j2,1:NSP),STAT=STATUS)


    ! routine in Chem_Shared; optional outputs-> 'wet_depos' and 'scav3d'
    CALL WetRemovalGMI ( kit%i1, kit%i2, kit%j1, kit%j2, 1, kit%km, cdt, spec%mw,                 &
                         spec%rel_scav_eff, spec%hstar, spec%delH_298_over_R, spec%retention_eff, &
                         spec%aero_flag, spec%h2o2_flag, spec%hno3_flag,                          &
                         area, geoht, mass, moistq, rain3Dcn, rain3Dls, T, press3c, press3e,      &
                         data3d )

  END SUBROUTINE TR_wet_removal


 END SUBROUTINE TR_run_tracer_

 END MODULE TR_GridCompMod
