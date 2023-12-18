#include "MAPL_Generic.h"

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1     !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: GMIchem_GridCompMod - The GMI COMBO Model Grid Component
!
! !INTERFACE:
!
   MODULE GMIchem_GridCompMod
!
! !USES:
!
   USE ESMF
   USE MAPL_Mod
   USE Chem_Mod 	                        ! Chemistry Base Class
   USE GMI_GridCompMod                          ! ESMF parent component
   USE Chem_UtilMod, ONLY : Chem_UtilNegFiller  ! Eliminates negative vmr
   USE m_chars, ONLY : uppercase

   IMPLICIT NONE
   PRIVATE

   TYPE(Chem_Mie), DIMENSION(2), SAVE :: gocartMieTable
!
! !PUBLIC MEMBER FUNCTIONS:

   PUBLIC SetServices
!
! !DESCRIPTION: 
!
!  {\tt GMIchem\_GridComp} is a ESMF gridded component for the Global Modeling
!  Initiative combined troposphere/stratospheric chemistry package.

! !REVISION HISTORY:

!  31Jul2006  da Silva  Created the GMI stub.
!  11Dec2007  Nielsen   Real code for Eros-beta7p17.
!  25Nov2011  Nielsen   Trying cubed sphere.
!  10Sep2013  Nielsen   Added run alarm, but allow for updating age-of-air and for 
!                       returning zero tendencies, etc., when alarm is not ringing.
!
!EOP
!-------------------------------------------------------------------------

  TYPE GMIchem_State
     PRIVATE
     TYPE(Chem_Registry), POINTER :: chemReg  => null()
     TYPE(GMI_GridComp),  POINTER :: gcGMI    => null()
     TYPE(Chem_Bundle),   POINTER :: w_c      => null()
  END TYPE GMIchem_State

  TYPE GMIchem_WRAP
     TYPE (GMIchem_State), pointer :: PTR => null()
  END TYPE GMIchem_WRAP

CONTAINS


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: SetServices --- Sets IRF services for GMIchem Grid Component
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
!  31Jul2006  da Silva  First crack.
!
!EOP
!-------------------------------------------------------------------------

!   ErrLog Variables
!   ----------------
    character(len=ESMF_MAXSTR)      :: IAm
    integer                         :: STATUS
    character(len=ESMF_MAXSTR)      :: COMP_NAME

!   Local derived type aliases
!   --------------------------
    type (ESMF_Config)              :: CF
    type (GMIchem_State), pointer   :: state   ! internal, that is
    type (GMIchem_wrap)             :: wrap

    integer                         :: m, n, i_XX, j_XX
    CHARACTER(LEN=ESMF_MAXSTR)      :: FRIENDLIES
    CHARACTER(LEN=ESMF_MAXSTR)      :: providerName
    CHARACTER(LEN=ESMF_MAXSTR)      :: aeroProviderName

    LOGICAL :: searchForImports
    INTEGER, PARAMETER :: numAeros = 5
    CHARACTER(LEN=2) :: aeroID(numAeros) = (/ "BC", "DU", "OC", "SS", "SU" /)
    CHARACTER(LEN=2) :: leadChars
    CHARACTER(LEN=ESMF_MAXSTR) :: name

!   Get my name and set-up traceback handle
!   ---------------------------------------
    call ESMF_GridCompGet( GC, NAME=COMP_NAME, CONFIG=CF, RC=STATUS )
    VERIFY_(STATUS)
    Iam = TRIM(COMP_NAME)//"::SetServices"

!   Wrap internal state for storing in GC; rename legacyState
!   -------------------------------------
    allocate ( state, stat=STATUS )
    VERIFY_(STATUS)
    wrap%ptr => state

!   Start by loading the Chem Registry
!   ----------------------------------
    allocate ( state%chemReg )
    state%chemReg = Chem_RegistryCreate ( STATUS )
    VERIFY_(STATUS)

!                       ------------------------
!                       ESMF Functional Services
!                       ------------------------


    IF(MAPL_AM_I_ROOT()) THEN
     PRINT *, TRIM(Iam)//': ACTIVE'
     CALL Chem_RegistryPrint ( state%chemReg )
    END IF


!   Set the Initialize, Run, Finalize entry points
!   ----------------------------------------------
    CALL MAPL_GridCompSetEntryPoint(GC, ESMF_METHOD_INITIALIZE, Initialize_, RC=STATUS)
    VERIFY_(STATUS)
    CALL MAPL_GridCompSetEntryPoint(GC,        ESMF_METHOD_RUN,        Run_, RC=STATUS)
    VERIFY_(STATUS)
    CALL MAPL_GridCompSetEntryPoint(GC,   ESMF_METHOD_FINALIZE,   Finalize_, RC=STATUS)
    VERIFY_(STATUS)

!   Store internal state in GC
!   --------------------------
    CALL ESMF_UserCompSetInternalState(GC, 'GMIchem_state', wrap, STATUS)
    VERIFY_(STATUS)

! ========================= IMPORT STATE =========================

    CALL ESMF_ConfigGetAttribute(CF, aeroProviderName, DEFAULT='none', LABEL="AERO_PROVIDER:", __RC__ )

    SELECT CASE (TRIM(aeroProviderName))

     CASE ("GOCART")

!   GOCART aerosols and dust.  With the exception of
!   SO4, select upon dust/aerosol two-letter identifier 
!   ---------------------------------------------------
      DO m=1,numAeros-1

       searchForImports = .FALSE.

       SELECT CASE (aeroID(m))
        CASE("BC")
         searchForImports = state%chemReg%doing_BC
        CASE("DU")
         searchForImports = state%chemReg%doing_DU
        CASE("OC")
         searchForImports = state%chemReg%doing_OC
        CASE("SS")
         searchForImports = state%chemReg%doing_SS
        CASE DEFAULT
         searchForImports = .FALSE.
       END SELECT

       Doing_Search: IF(searchForImports) THEN

        IF(MAPL_AM_I_ROOT() .AND. m == 1) PRINT *,"Adding the following from GOCART to GMICHEM import state:"

        DO n = state%chemReg%i_GOCART, state%chemReg%j_GOCART

         name = TRIM(state%chemReg%vname(n))
         leadChars = UPPERCASE(name(1:2))

         Match: IF(leadChars == aeroID(m)) THEN

          CALL MAPL_AddImportSpec(GC,                                  &
               SHORT_NAME  = "GOCART::"//TRIM(state%chemReg%vname(n)), &
               LONG_NAME   = state%chemReg%vtitle(n),		     &
               UNITS       = state%chemReg%vunits(n),		     &
               DIMS        = MAPL_DimsHorzVert,		             &
               VLOCATION   = MAPL_VLocationCenter,	  RC=STATUS  )
          VERIFY_(STATUS)

          IF(MAPL_AM_I_ROOT()) PRINT *,"  ",TRIM(state%chemReg%vname(n))

         END IF Match

        END DO 

       END IF Doing_Search

      END DO

!   This is the special case for SO4, which 
!   does not have "SU" as its leading two characters
!   ------------------------------------------------
      IF(state%chemReg%doing_SU) THEN

       Doing_SO4: DO n = state%chemReg%i_SU, state%chemReg%j_SU

        IF( (TRIM(state%chemReg%vname(n)) == "SO4" ) .OR.  &
            (TRIM(state%chemReg%vname(n)) == "SO4v")       )  THEN
         CALL MAPL_AddImportSpec(GC,                                  &
     	      SHORT_NAME  = "GOCART::"//TRIM(state%chemReg%vname(n)), &
     	      LONG_NAME   = state%chemReg%vtitle(n),                  &
     	      UNITS       = state%chemReg%vunits(n),                  &
     	      DIMS        = MAPL_DimsHorzVert,                        &
     	      VLOCATION   = MAPL_VLocationCenter,           RC=STATUS )
         VERIFY_(STATUS)
         IF(MAPL_AM_I_ROOT()) PRINT *,"  ",TRIM(state%chemReg%vname(n))
        END IF

       END DO Doing_SO4

      END IF

     CASE("GOCART.data")

      CALL MAPL_AddImportSpec(GC,				    &
	  SHORT_NAME	     = 'AERO',  			    &
	  LONG_NAME	     = 'aerosol_mass_mixing_ratios',	    &
	  UNITS 	     = 'kg kg-1',			    &
	  DIMS  	     = MAPL_DimsHorzVert,		    &
	  VLOCATION	     = MAPL_VLocationCenter,		    &
	  DATATYPE	     = MAPL_StateItem,  		    &
          RESTART            = MAPL_RestartSkip,                    &
							RC=STATUS  )
      VERIFY_(STATUS)


     CASE("GMICHEM")

      STATUS = 0

     CASE DEFAULT

      PRINT *, TRIM(Iam)//": Invalid AERO_PROVIDER when running GMIChem."
      STATUS = 1
      VERIFY_(STATUS)
    
    END SELECT

#include "GMICHEM_ImportSpec___.h"

! ======================== INTERNAL STATE =========================

! Is GMICHEM providing ozone (mole fraction) to the ANALYSIS bundle?
! ------------------------------------------------------------------
     CALL ESMF_ConfigGetAttribute(CF, providerName, Default="PCHEM", &
                                  Label="ANALYSIS_OX_PROVIDER:", RC=STATUS )
     VERIFY_(STATUS)


!   Species to be transported:
!   --------------------------
    DO n = state%chemReg%i_GMI, state%chemReg%j_GMI

	  IF(TRIM(state%chemReg%vname(n)) == "OX" .AND. TRIM(providerName) == "GMICHEM") THEN
           FRIENDLIES="ANALYSIS:DYNAMICS:TURBULENCE:MOIST"
	  ELSE
           FRIENDLIES="DYNAMICS:TURBULENCE:MOIST"
	  END IF
	 
          CALL MAPL_AddInternalSpec(GC,                                  &
               SHORT_NAME         = TRIM(state%chemReg%vname(n)),        &
               LONG_NAME          = TRIM(state%chemReg%vtitle(n)),       &
               UNITS              = TRIM(state%chemReg%vunits(n)),       &     
               FRIENDLYTO         = TRIM(FRIENDLIES),                    &
               DIMS               = MAPL_DimsHorzVert,                   &
               VLOCATION          = MAPL_VLocationCenter,     RC=STATUS  )
          VERIFY_(STATUS)

    END DO

!   Non-transported species
!   -----------------------
    DO n = state%chemReg%i_XX, state%chemReg%j_XX 

          CALL MAPL_AddInternalSpec(GC,                               &
               SHORT_NAME      = TRIM(state%chemReg%vname(n)),        &
               LONG_NAME       = TRIM(state%chemReg%vtitle(n)),       &
               UNITS           = TRIM(state%chemReg%vunits(n)),       &     
               ADD2EXPORT      = .TRUE.,                              &
               DIMS            = MAPL_DimsHorzVert,                   &
               VLOCATION       = MAPL_VLocationCenter,     RC=STATUS  )
          VERIFY_(STATUS)

    END DO

! ========================== EXPORT STATE =========================

    IF(TRIM(aeroProviderName) == "GMICHEM") THEN

!   This state is needed by radiation, and contains aerosols and aerosol optics
!   ---------------------------------------------------------------------------
     CALL MAPL_AddExportSpec(GC,                                   &
         SHORT_NAME         = 'AERO',                              &
         LONG_NAME          = 'aerosol_mass_mixing_ratios',        &
         UNITS              = 'kg kg-1',                           &
         DIMS               = MAPL_DimsHorzVert,                   &
         VLOCATION          = MAPL_VLocationCenter,                &
         DATATYPE           = MAPL_StateItem,                      &
                                                        RC=STATUS  )
     VERIFY_(STATUS)

! This bundle is reserved for SURFACE to update snow albedo due to
! aerosol settling and deposition. But since GMI's aerosols are prescribed,
! they are considered "data-driven," and the bundle is not filled by GMICHEM.
! ---------------------------------------------------------------------------
     CALL MAPL_AddExportSpec(GC,                                  &
         SHORT_NAME         = 'AERO_DP',                          &
         LONG_NAME          = 'aerosol_deposition',               &
         UNITS              = 'kg m-2 s-1',                       &
         DIMS               = MAPL_DimsHorzOnly,                  &
         VLOCATION          = MAPL_VLocationNone,                 &
         DATATYPE           = MAPL_BundleItem,                    &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

    END IF

! SAD related variables
! ---------------------

    CALL MAPL_AddExportSpec(GC, 				 &
       SHORT_NAME	  = 'HNO3CONDsad'   ,			 &
       LONG_NAME	  = 'condensed_phase_hno3',		 &
       UNITS		  = 'mixing_ratio',			 &
       DIMS		  = MAPL_DimsHorzVert,  		 &
       VLOCATION	  = MAPL_VLocationCenter,		 &
    						      RC=STATUS  )
    VERIFY_(STATUS)

    CALL MAPL_AddExportSpec(GC, 				 &
       SHORT_NAME	  = 'HNO3GASsad'   ,			 &
       LONG_NAME	  = 'gas_phase-hno3',			 &
       UNITS		  = 'mixing_ratio',			 &
       DIMS		  = MAPL_DimsHorzVert,  		 &
       VLOCATION	  = MAPL_VLocationCenter,		 &
    						      RC=STATUS  )
    VERIFY_(STATUS)

! Ship Emissions
! --------------

    CALL MAPL_AddExportSpec(GC, 				 &
       SHORT_NAME	  = 'jNO2val',  			 &
       LONG_NAME	  = 'photolysis_rate_constants_for_NO',  &
       UNITS		  = 's^-1',				 &
       DIMS		  = MAPL_DimsHorzOnly,  		 &
       VLOCATION	  = MAPL_VLocationNone, 		 &
    						      RC=STATUS  )
    VERIFY_(STATUS)

    CALL MAPL_AddExportSpec(GC,                                  &
        SHORT_NAME         = 'surfEmissForChem',                 &
        LONG_NAME          = 'surface_emission_for_chemistry',   &
        UNITS              = 'kg m-2 s-1',                       &
        DIMS               = MAPL_DimsHorzOnly,                  &
        VLOCATION          = MAPL_VLocationNone,                 &
        DATATYPE           = MAPL_BundleItem,                    &
                                                      RC=STATUS  )
    VERIFY_(STATUS)

! Aerosol Surface Area Densities (SAD) bundle.
! --------------------------------------------

    CALL MAPL_AddExportSpec(GC,                                  &
        SHORT_NAME         = 'gmiSAD',                           &
        LONG_NAME          = 'surface_area_densities',           &
        UNITS              = 'cm^2/cm^3',                        &
        DIMS               = MAPL_DimsHorzVert,                  &
        VLOCATION          = MAPL_VLocationCenter,               &
        DATATYPE           = MAPL_BundleItem,                    &
                                                      RC=STATUS  )
    VERIFY_(STATUS)

! Photolysis Rate Constants bundle.
! --------------------------------

    CALL MAPL_AddExportSpec(GC,                                  &
        SHORT_NAME         = 'gmiQJ',                            &
        LONG_NAME          = 'photolysis_rate_constants',        &
        UNITS              = 'cm3 s-1',                          &
        DIMS               = MAPL_DimsHorzVert,                  &
        VLOCATION          = MAPL_VLocationCenter,               &
        DATATYPE           = MAPL_BundleItem,                    &
                                                      RC=STATUS  )
    VERIFY_(STATUS)

    CALL MAPL_AddExportSpec(GC,                                  &
        SHORT_NAME         = 'gmiQQJ',                           &
        LONG_NAME          = 'photolysis_reaction_rates',        &
        UNITS              = 'cm-3 s-1',                         &
        DIMS               = MAPL_DimsHorzVert,                  &
        VLOCATION          = MAPL_VLocationCenter,               &
        DATATYPE           = MAPL_BundleItem,                    &
                                                      RC=STATUS  )
    VERIFY_(STATUS)

! Thermal Rate Constants bundle.
! --------------------------------

    CALL MAPL_AddExportSpec(GC,                                  &
        SHORT_NAME         = 'gmiQK',                            &
        LONG_NAME          = 'thermal_rate_constants',           &
        UNITS              = '2-3body_varies',                   &
        DIMS               = MAPL_DimsHorzVert,                  &
        VLOCATION          = MAPL_VLocationCenter,               &
        DATATYPE           = MAPL_BundleItem,                    &
                                                      RC=STATUS  )
    VERIFY_(STATUS)

    CALL MAPL_AddExportSpec(GC,                                  &
        SHORT_NAME         = 'gmiQQK',                           &
        LONG_NAME          = 'thermal_reaction_rates',           &
        UNITS              = 'cm-3 s-1',                         &
        DIMS               = MAPL_DimsHorzVert,                  &
        VLOCATION          = MAPL_VLocationCenter,               &
        DATATYPE           = MAPL_BundleItem,                    &
                                                      RC=STATUS  )
    VERIFY_(STATUS)

! Aerosol or Dust Radii bundle.
! -----------------------------

    CALL MAPL_AddExportSpec(GC,                                  &
        SHORT_NAME         = 'gmiERADIUS',                       &
        LONG_NAME          = 'Aerosol_Dust_Radii',               &
        UNITS              = 'cm',                               &
        DIMS               = MAPL_DimsHorzVert,                  &
        VLOCATION          = MAPL_VLocationCenter,               &
        DATATYPE           = MAPL_BundleItem,                    &
                                                      RC=STATUS  )

! Surface Area of Aerosol or Dust bundle.
! ---------------------------------------

    CALL MAPL_AddExportSpec(GC,                                  &
        SHORT_NAME         = 'gmiTAREA',                         &
        LONG_NAME          = 'surface_area_aerosol_dust',        &
        UNITS              = 'cm^2/cm^3',                        &
        DIMS               = MAPL_DimsHorzVert,                  &
        VLOCATION          = MAPL_VLocationCenter,               &
        DATATYPE           = MAPL_BundleItem,                    &
                                                      RC=STATUS  )
    VERIFY_(STATUS)

#include "GMICHEM_ExportSpec___.h"
#include "Deposition_ExportSpec___.h"
#include "Reactions_ExportSpec___.h"
#include "Tendency_ExportSpec___.h"

! =================================================================

!   Set the Profiling timers
!   ------------------------
    CALL MAPL_TimerAdd(GC, NAME="INITIALIZE", RC=STATUS)
    VERIFY_(STATUS)
    CALL MAPL_TimerAdd(GC, NAME="RUN", RC=STATUS)
    VERIFY_(STATUS)
    CALL MAPL_TimerAdd(GC, NAME="FINALIZE", RC=STATUS)
    VERIFY_(STATUS)

!   Generic Set Services
!   --------------------
    call MAPL_GenericSetServices ( GC, RC=STATUS )
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)
  
  END SUBROUTINE SetServices


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Initialize_ --- Initialize GMIchem
!
! !INTERFACE:
!

   SUBROUTINE Initialize_ ( gc, impChem, expChem, clock, rc )

! !USES:

   implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),  intent(inout) :: clock      ! The clock

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout)  :: gc      ! Grid Component
   type(ESMF_State), intent(inout) :: impChem     ! Import State
   type(ESMF_State), intent(inout) :: expChem     ! Export State
   integer, intent(out) ::  rc                    ! Error return code:
                                                  !  0 - all is well
                                                  !  1 - 

! !DESCRIPTION: This is a simple ESMF wrapper.
!
! !REVISION HISTORY:
!
!  27Feb2005 da Silva  First crack.
!
!EOP
!-------------------------------------------------------------------------


!  ErrLog Variables
!  ----------------
   character(len=ESMF_MAXSTR)      :: IAm
   integer                         :: STATUS
   character(len=ESMF_MAXSTR)      :: COMP_NAME

   type(Chem_Registry), pointer    :: chemReg
   type(GMI_GridComp), pointer     :: gcGMI       ! Grid Component
   type(Chem_Bundle), pointer      :: w_c         ! Chemical tracer fields
   integer                         :: nymd, nhms  ! time of day
   real                            :: gmiDt       ! chemistry timestep (secs)
   real                            :: runDt       ! heartbeat (secs)

   type(ESMF_Config)               :: CF
   type(ESMF_Grid)                 :: grid
 
   integer                         :: i1=1, i2, ig=0, im  ! dist grid indices
   integer                         :: j1=1, j2, jg=0, jm  ! dist grid indices
   integer                         :: km                  ! dist grid indices
   integer                         :: dims(3), k, l, n

   type(Chem_Array), pointer       :: q(:)	   ! array of pointers
   type(MAPL_MetaComp), pointer    :: ggState	   ! GEOS Generic State
   type(ESMF_State)                :: internal
   type(MAPL_VarSpec), pointer     :: InternalSpec(:)

   REAL, POINTER, DIMENSION(:,:)   :: LATS
   REAL, POINTER, DIMENSION(:,:)   :: LONS

   CHARACTER(LEN=ESMF_MAXSTR)	   :: short_name
   CHARACTER(LEN=ESMF_MAXSTR)	   :: diurnal_bb
   CHARACTER(LEN=ESMF_MAXSTR)      :: providerName
   CHARACTER(LEN=ESMF_MAXSTR), POINTER, DIMENSION(:) :: fieldNames

   INTEGER, PARAMETER :: numAeroes = 13
   CHARACTER(LEN=ESMF_MAXSTR) :: aeroName(numAeroes) = (/"BCphobic","BCphilic", &
             "du001   ","du002   ","du003   ","du004   ","OCphobic","OCphilic", &
             "ss001   ","ss003   ","ss004   ","ss005   ","SO4     "/)

   rc = 0

!  Get my name and set-up traceback handle
!  ---------------------------------------
   call ESMF_GridCompGet( GC, NAME=COMP_NAME, CONFIG=CF, RC=STATUS )
   VERIFY_(STATUS)
   Iam = TRIM(COMP_NAME)//"::Initialize_"

!  Get my internal MAPL_Generic state
!  -----------------------------------
   call MAPL_GetObjectFromGC ( GC, ggState, RC=STATUS)
   VERIFY_(STATUS)

!  Start timers
!  ------------
   CALL MAPL_TimerOn(ggSTATE, "TOTAL")
   CALL MAPL_TimerOn(ggSTATE, "INITIALIZE")
   
!  Initialize GEOS Generic
!  ------------------------
   call MAPL_GenericInitialize ( gc, impChem, expChem, clock,  RC=STATUS )
   VERIFY_(STATUS)

!  Get parameters from gc and clock
!  --------------------------------
   call extract_ ( gc, clock, chemReg, gcGMI, w_c, nymd, nhms, gmiDt, runDt, STATUS )
   VERIFY_(STATUS)
   IF(MAPL_AM_I_ROOT()) THEN
    PRINT *," "
    PRINT *, TRIM(Iam)//": GMICHEM time step length: ",gmiDt," seconds"
   END IF

!  Aerosol for radiation
!  ---------------------
   CALL ESMF_ConfigGetAttribute(CF, providerName, Default="PCHEM", &
                                Label="AERO_PROVIDER:", __RC__ )
   gcGMI%gcPhot%aeroProviderName = TRIM(providerName)
   IF(TRIM(providerName) == "GMICHEM") THEN
    gcGMI%gcPhot%AM_I_AERO_PROVIDER = .TRUE.
   ELSE
    gcGMI%gcPhot%AM_I_AERO_PROVIDER = .FALSE.
   END IF

!  Create Chem Bundle
!  ------------------
   call ESMF_GridCompGet ( GC, GRID=grid, rc=STATUS)
   VERIFY_(STATUS)

   call MAPL_GridGet ( grid, globalCellCountPerDim=DIMS, RC=STATUS)
   VERIFY_(STATUS)

   im = dims(1)
   jm = dims(2)

   call ESMF_GridGet(GRID, localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
   		     computationalCount=DIMS, RC=STATUS)
   VERIFY_(STATUS)

!  Associate the Internal State fields with our legacy state 
!  ---------------------------------------------------------
   call MAPL_Get ( ggSTATE, INTERNALSPEC=InternalSpec, &
                   INTERNAL_ESMF_STATE=internal, &
                   LONS=LONS, LATS=LATS, RC=STATUS  )
   VERIFY_(STATUS)

!  Local sizes of three dimensions
!  --------------------------------
   i2 = dims(1)
   j2 = dims(2)
   km = dims(3)

!  Broadcast necessary information to individual GCs
!  -------------------------------------------------
   CALL sendToGCs(STATUS)
   VERIFY_(STATUS)

!  Initalize the legacy state but do not allocate memory for arrays
!  ----------------------------------------------------------------
   call Chem_BundleCreate_ ( chemReg, i1, i2, ig, im, j1, j2, jg, jm, km,  &
                             w_c, lon=lons(:,1), lat=lats(1,:), &
                             skipAlloc=.true., rc=STATUS )
   VERIFY_(STATUS)

   w_c%grid_esmf = grid
   ALLOCATE(w_c%delp(i1:i2,j1:j2,km),w_c%rh(i1:i2,j1:j2,km),STAT=STATUS)
   VERIFY_(STATUS)

!  Activate or de-activate diurnal cycle for biomass burning. Default is OFF.
!  --------------------------------------------------------------------------
   CALL ESMF_ConfigGetAttribute(CF, diurnal_bb, LABEL="DIURNAL_BIOMASS_BURNING:", &
                                DEFAULT="NO", RC=STATUS )
   VERIFY_(STATUS)
   IF(diurnal_bb(1:3) == "yes" .OR. diurnal_bb(1:3) == "YES" .OR. diurnal_bb(1:3) == "Yes") THEN	
    short_name = "will be"
    w_c%diurnal_bb = .TRUE.
   ELSE
    short_name = "will not be"
    w_c%diurnal_bb = .FALSE.
   END IF
   IF(MAPL_AM_I_ROOT()) PRINT *, TRIM(Iam)//': Diurnal cycle '//TRIM(short_name)//" applied to biomass burning."

!  Consistency Checks
!  ------------------
   ASSERT_ ( chemReg%i_XX == (chemReg%j_GMI+1) )
   ASSERT_ ( size(InternalSpec) == (chemReg%n_GMI+chemReg%n_XX) )

   do L = 1, size(InternalSpec)

      call MAPL_VarSpecGet ( InternalSpec(L),          &
                             SHORT_NAME = short_name,  &
                             RC=STATUS )
      VERIFY_(STATUS)

      N = chemReg%i_GMI + L - 1 ! Assumption: XX species immediately follow GMI species
      CALL MAPL_GetPointer ( internal, NAME=short_name, ptr=w_c%qa(N)%data3d, &
                             rc = STATUS )
      VERIFY_(STATUS)

   end do

!  Call initialize
!  ---------------
   call GMI_GridCompInitialize(gcGMI, w_c, impChem, expChem, nymd, nhms, gmiDt, STATUS)
   VERIFY_(STATUS)

!  Initialize the AERO state if GMIChem is the AERO_PROVIDER
!  ---------------------------------------------------------
   Building_AERO: IF(gcGMI%gcPhot%AM_I_AERO_PROVIDER) THEN

    CALL Aero_StateInitialize(STATUS)
    VERIFY_(STATUS)

   END IF Building_AERO

!  Stop timers
!  -----------
   CALL MAPL_TimerOff(ggSTATE, "INITIALIZE")
   CALL MAPL_TimerOff(ggSTATE, "TOTAL")

   RETURN_(ESMF_SUCCESS)

  CONTAINS

   SUBROUTINE sendToGCs(rc)
   IMPLICIT NONE
   INTEGER, INTENT(OUT) :: rc
   rc = 0
   
   gcGMI%gcChem%i1 = i1
   gcGMI%gcChem%i2 = i2
   gcGMI%gcChem%im = im
   gcGMI%gcChem%j1 = j1
   gcGMI%gcChem%j2 = j2
   gcGMI%gcChem%jm = jm
   gcGMI%gcChem%km = km
   
   gcGMI%gcDepos%i1 = i1
   gcGMI%gcDepos%i2 = i2
   gcGMI%gcDepos%im = im
   gcGMI%gcDepos%j1 = j1
   gcGMI%gcDepos%j2 = j2
   gcGMI%gcDepos%jm = jm
   gcGMI%gcDepos%km = km
   
   gcGMI%gcEmiss%i1 = i1
   gcGMI%gcEmiss%i2 = i2
   gcGMI%gcEmiss%im = im
   gcGMI%gcEmiss%j1 = j1
   gcGMI%gcEmiss%j2 = j2
   gcGMI%gcEmiss%jm = jm
   gcGMI%gcEmiss%km = km
   gcGMI%gcEmiss%heartBeat = runDt
   
   gcGMI%gcFBC%i1 = i1
   gcGMI%gcFBC%i2 = i2
   gcGMI%gcFBC%im = im
   gcGMI%gcFBC%j1 = j1
   gcGMI%gcFBC%j2 = j2
   gcGMI%gcFBC%jm = jm
   gcGMI%gcFBC%km = km
   
   gcGMI%gcPhot%i1 = i1
   gcGMI%gcPhot%i2 = i2
   gcGMI%gcPhot%im = im
   gcGMI%gcPhot%j1 = j1
   gcGMI%gcPhot%j2 = j2
   gcGMI%gcPhot%jm = jm
   gcGMI%gcPhot%km = km
   
   gcGMI%gcSAD%i1 = i1
   gcGMI%gcSAD%i2 = i2
   gcGMI%gcSAD%im = im
   gcGMI%gcSAD%j1 = j1
   gcGMI%gcSAD%j2 = j2
   gcGMI%gcSAD%jm = jm
   gcGMI%gcSAD%km = km
   
   gcGMI%gcThermalRC%i1 = i1
   gcGMI%gcThermalRC%i2 = i2
   gcGMI%gcThermalRC%im = im
   gcGMI%gcThermalRC%j1 = j1
   gcGMI%gcThermalRC%j2 = j2
   gcGMI%gcThermalRC%jm = jm
   gcGMI%gcThermalRC%km = km

! Latitudes and longitudes (radians) where needed. Note: 
! Deallocations are done by each respective GridCompFinalize.
! -----------------------------------------------------------
   ALLOCATE(gcGMI%gcDepos%lonRad(1:i2,1:j2),STAT=STATUS)
   VERIFY_(STATUS)
   gcGMI%gcDepos%lonRad = LONS

   ALLOCATE(gcGMI%gcDepos%latRad(1:i2,1:j2),STAT=STATUS)
   VERIFY_(STATUS)
   gcGMI%gcDepos%latRad = LATS

    ALLOCATE(gcGMI%gcEmiss%lonRad(1:i2,1:j2),STAT=STATUS)
   VERIFY_(STATUS)
   gcGMI%gcEmiss%lonRad = LONS

   ALLOCATE(gcGMI%gcEmiss%latRad(1:i2,1:j2),STAT=STATUS)
   VERIFY_(STATUS)
   gcGMI%gcEmiss%latRad = LATS

  ALLOCATE(gcGMI%gcFBC%latRad(1:i2,1:j2),STAT=STATUS)
   VERIFY_(STATUS)
   gcGMI%gcFBC%latRad = LATS

   ALLOCATE(gcGMI%gcPhot%lonRad(1:i2,1:j2),STAT=STATUS)
   VERIFY_(STATUS)
   gcGMI%gcPhot%lonRad = LONS

   ALLOCATE(gcGMI%gcPhot%latRad(1:i2,1:j2),STAT=STATUS)
   VERIFY_(STATUS)
   gcGMI%gcPhot%latRad = LATS

   ALLOCATE(gcGMI%gcSAD%lonRad(1:i2,1:j2),STAT=STATUS)
   VERIFY_(STATUS)
   gcGMI%gcSAD%lonRad = LONS

   ALLOCATE(gcGMI%gcSAD%latRad(1:i2,1:j2),STAT=STATUS)
   VERIFY_(STATUS)
   gcGMI%gcSAD%latRad = LATS

   RETURN_(ESMF_SUCCESS)
   END SUBROUTINE sendToGCs

   SUBROUTINE Aero_StateInitialize(rc)
!  ---------------------------------------------------------------------------------
!   Substantially plagarized from GOCART to imitate, as closely as possible, 
!   building the AERO state, which is a container for aerosols and an optics method. 
!  ---------------------------------------------------------------------------------
   IMPLICIT NONE
   INTEGER, INTENT(OUT) :: rc
   CHARACTER(LEN=ESMF_MAXSTR) :: fieldName
   REAL, POINTER, DIMENSION(:,:,:) :: PTR3D

   TYPE(ESMF_State) :: aero
   TYPE(ESMF_Field) :: field
   TYPE(ESMF_Field) :: renamedField
   TYPE(ESMF_FieldBundle) :: aeroBundle
   
   INTEGER, PARAMETER :: numAttributes = 5
   CHARACTER(LEN=ESMF_MAXSTR) :: attName(numAttributes) = &
         (/'air_pressure_for_aerosol_optics            ', &
           'relative_humidity_for_aerosol_optics       ', &
           'extinction_in_air_due_to_ambient_aerosol   ', &
           'single_scattering_albedo_of_ambient_aerosol', &
           'asymmetry_parameter_of_ambient_aerosol     '/)
   CHARACTER(LEN=ESMF_MAXSTR) :: attValu(numAttributes) = (/'PLE','RH ','EXT','SSA','ASY'/)
   INTEGER :: loc(numAttributes) = (/ MAPL_VLocationEdge, MAPL_VLocationCenter, MAPL_VLocationCenter, &
                                                          MAPL_VLocationCenter, MAPL_VLocationCenter /)

!  GMI aerosols are prescribed, so consider our instantiation "data-driven"
!  ------------------------------------------------------------------------
    INTEGER, PARAMETER :: instance = 2

    rc = 0
    NULLIFY(PTR3D)

    CALL ESMF_StateGet(expChem, 'AERO', aero, __RC__)
    CALL ESMF_AttributeSet(aero, NAME='implements_aerosol_optics_method', VALUE=.TRUE., __RC__)
    aeroBundle = ESMF_FieldBundleCreate(NAME='AEROSOLS', __RC__)
    CALL MAPL_StateAdd(aero, aeroBundle, __RC__)

    DO n = 1,numAeroes
     CALL MAPL_GetPointer(expChem, PTR3D, "GMICHEM::"//TRIM(aeroName(n)), ALLOC=.TRUE., RC=STATUS)
     CALL ESMF_StateGet(expChem, "GMICHEM::"//TRIM(aeroName(n)), field, __RC__)
     renamedField = MAPL_FieldCreate(field, NAME=TRIM(aeroName(n)), __RC__)
     CALL MAPL_FieldBundleAdd(aeroBundle, renamedField, __RC__)
     NULLIFY(PTR3D)
    END DO

    gocartMieTable(instance) = Chem_MieCreate(CF, __RC__)

    CALL ESMF_AttributeSet(aero, NAME='mie_table_instance', VALUE=instance, __RC__)
    CALL ESMF_AttributeSet(aero, NAME='cloud_area_fraction_for_aerosol_optics', VALUE='', __RC__)
    CALL ESMF_AttributeSet(aero, NAME='band_for_aerosol_optics', VALUE=0, __RC__)

    DO n = 1,numAttributes
     CALL ESMF_AttributeSet(aero, NAME=TRIM(attName(n)), VALUE=TRIM(attValu(n)), __RC__)
    END DO

    DO n = 1,numAttributes
     CALL ESMF_AttributeGet(aero, NAME=TRIM(attName(n)), VALUE=fieldName, __RC__)
     IF(fieldName /= '') THEN
      field = MAPL_FieldCreateEmpty(TRIM(fieldName), w_c%grid_esmf, __RC__)
      CALL MAPL_FieldAllocCommit(field, DIMS=MAPL_DimsHorzVert, LOCATION=loc(n), TYPEKIND=MAPL_R4, HW=0, __RC__)
      CALL MAPL_StateAdd(aero, field, __RC__)
     END IF
    END DO

    CALL ESMF_MethodAdd(aero, LABEL='aerosol_optics', USERROUTINE=aerosol_optics, __RC__)

    IF(MAPL_AM_I_ROOT()) THEN
     PRINT *," "
     PRINT *, TRIM(Iam)//": AEROSOLS Bundle Members:" 
     CALL ESMF_FieldBundleGet(aeroBundle, FieldCount=n, RC=STATUS)
     VERIFY_(STATUS)
     ALLOCATE(fieldNames(n), STAT=STATUS)
     VERIFY_(STATUS)
     CALL ESMF_FieldBundleGet(aeroBundle, FieldNameList=fieldNames, RC=STATUS)
     VERIFY_(STATUS)
     WRITE(*,FMT="('  Number  Field name')")
     WRITE(*,FMT="('  ------  ------------------')")
     DO k = 1,n
      WRITE(*,FMT="(I7,3X,A)") k,TRIM(fieldNames(k))
     END DO
     PRINT *," "
     DEALLOCATE(fieldNames, STAT=STATUS)
     VERIFY_(STATUS)
    END IF

   RETURN_(ESMF_SUCCESS)
   END SUBROUTINE Aero_StateInitialize

  END SUBROUTINE Initialize_


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Run_ --- Runs GMIchem
!
! !INTERFACE:
!

   SUBROUTINE Run_ ( gc, impChem, expChem, clock, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),  intent(inout) :: clock      ! The clock

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout)  :: gc      ! Grid Component
   type(ESMF_State), intent(inout) :: impChem     ! Import State
   type(ESMF_State), intent(inout) :: expChem     ! Export State
   integer, intent(out) ::  rc                    ! Error return code:
                                                  !  0 - all is well
                                                  !  1 - 

! !DESCRIPTION: This is a simple ESMF wrapper.
!
! !REVISION HISTORY:
!
!  27Feb2005 da Silva  First crack.
!  10Sep2013 Nielsen   Added run alarm, but allow for updating age-of-air and for 
!                      returning zero tendencies, etc., when alarm is not ringing.
!
!EOP
!-------------------------------------------------------------------------

!  ErrLog Variables
!  ----------------
   character(len=ESMF_MAXSTR)      :: IAm
   integer                         :: STATUS
   character(len=ESMF_MAXSTR)      :: COMP_NAME

   type(Chem_Registry), pointer    :: chemReg
   type(GMI_GridComp), pointer     :: gcGMI       ! Grid Component
   type(Chem_Bundle), pointer      :: w_c         ! Chemical tracer fields     
   integer                         :: nymd, nhms  ! time
   real                            :: gmiDt       ! chemistry timestep (secs)
   real                            :: runDt       ! heartbeat (secs)
   integer                         :: i, i2, iOX, iT2M, j2, k, km, m, n
   LOGICAL                         :: RunGMINow

   type(ESMF_Config)               :: CF
   type(ESMF_Grid)                 :: grid        
   type(ESMF_Time)                 :: TIME
   TYPE(ESMF_Alarm)                :: ALARM

   type(MAPL_MetaComp), pointer    :: ggState      ! GEOS Generic State

   real, pointer, dimension(:,:,:) :: rh2
   real, pointer, dimension(:,:)   :: LATS
   real, pointer, dimension(:,:)   :: LONS
   type (MAPL_SunOrbit)            :: ORBIT
   real, allocatable, target       :: ZTH(:,:)
   real, allocatable               :: SLR(:,:)

   REAL :: dtInverse
   REAL, POINTER, DIMENSION(:,:,:) :: WATER
   REAL, POINTER, DIMENSION(:,:,:) :: SPECHUM
   REAL, POINTER, DIMENSION(:,:,:) :: Q_TEND
   REAL, POINTER, DIMENSION(:,:,:) :: OX_TEND
   REAL, POINTER, DIMENSION(:,:,:) :: PLE
   REAL, POINTER, DIMENSION(:,:,:) :: DELP
   REAL, POINTER, DIMENSION(:,:)   :: TROPP
   REAL, POINTER, DIMENSION(:,:)   :: AGCMTROPP
   REAL, POINTER, DIMENSION(:,:)   :: GMITROPP
   REAL, POINTER, DIMENSION(:,:)   :: TO3
   REAL, POINTER, DIMENSION(:,:)   :: TTO3
   REAL, ALLOCATABLE               :: wrk(:,:)
   REAL, ALLOCATABLE               :: wgt(:,:)

! Tendency Bundle
! ---------------
   TYPE(ESMF_Field)                  :: FIELD
   REAL, POINTER, DIMENSION(:,:,:,:) :: sInitial
   REAL, POINTER, DIMENSION(:,:,:)   :: sIncrement
   LOGICAL                           :: doingTendencies
   INTEGER                           :: nAlloc
   LOGICAL, ALLOCATABLE              :: doMyTendency(:)
   CHARACTER(LEN=ESMF_MAXSTR)        :: fieldName, incFieldName

! Replay mode detection
! ---------------------
   CHARACTER(LEN=ESMF_MAXSTR)        :: ReplayMode
   TYPE(ESMF_Alarm)                  :: PredictorIsActive
   LOGICAL                           :: doingPredictorNow

!  Get my name and set-up traceback handle
!  ---------------------------------------
   CALL ESMF_GridCompGet(GC, NAME=COMP_NAME, CONFIG=CF, GRID=grid, RC=STATUS)
   VERIFY_(STATUS)
   Iam = TRIM(COMP_NAME)//"::Run_"

!  Get my internal MAPL_Generic state
!  -----------------------------------
   CALL MAPL_GetObjectFromGC(GC, ggState, RC=STATUS)
   VERIFY_(STATUS)

!  Start a comprehensive timer
!  ---------------------------
   CALL MAPL_TimerOn(ggState, "TOTAL")

!  Get parameters from generic state
!  ---------------------------------
   CALL MAPL_Get(ggState, IM=i2, JM=j2, LM=km, LONS=LONS, LATS=LATS,  &
                 ORBIT=ORBIT, RUNALARM=ALARM, RC=STATUS)
   VERIFY_(STATUS)

!  Query the alarm
!  ---------------
   RunGMINow = ESMF_AlarmIsRinging(ALARM, RC=STATUS)
   VERIFY_(STATUS)

!  Set aside some space for the solar parameters
!  ---------------------------------------------
   ALLOCATE(ZTH(1:i2,1:j2), STAT=STATUS)
   VERIFY_(STATUS)
   ALLOCATE(SLR(1:i2,1:j2), STAT=STATUS)
   VERIFY_(STATUS)

!  Update solar zenith angle
!  -------------------------
   CALL MAPL_SunGetInsolation(LONS, LATS, ORBIT, ZTH, SLR, CLOCK=CLOCK, RC=STATUS)
   VERIFY_(STATUS)

!  Get ESMF parameters from gc and clock
!  -------------------------------------
   CALL extract_(GC, clock, chemReg, gcGMI, w_c, nymd, nhms, gmiDt, runDt, STATUS)
   VERIFY_(STATUS)

!  Set pointers for cosine zenith angle
!  ------------------------------------
   w_c%cosz => zth

!  Layer interface pressures
!  -------------------------
   CALL MAPL_GetPointer(impChem, PLE, 'PLE', RC=STATUS)
   VERIFY_(STATUS)

!  Layer pressure thickness
!  ------------------------
   CALL MAPL_GetPointer(impChem, DELP, 'DELP', RC=STATUS)
   VERIFY_(STATUS)
   w_c%delp = DELP

!  Fill in RH.  Note: Not converted to %
!  -------------------------------------
   CALL MAPL_GetPointer(impChem, rh2, 'RH2', RC=STATUS)
   w_c%rh = rh2
   VERIFY_(STATUS)

!  Assure non-negative volumetric mixing ratios [mole fractions]
!  -------------------------------------------------------------
   DO n = ChemReg%i_GMI, ChemReg%j_GMI
    CALL Chem_UtilNegFiller(w_c%qa(n)%data3d, w_c%delp, i2, j2, QMIN=TINY(1.0))
   END DO

!  Occasionally, MAPL_UNDEFs appear in the imported tropopause pressures,
!  TROPP. To avoid encountering them, save the most recent valid tropopause 
!  pressures in an unused "layer" of T2M15D, i.e. w_c%qa(iT2M)%data3d(:,:,km). 
!  By passing the information through the internal state via w_c%qa (at least
!  for now), reproducibility across various layouts is assured, which is a 
!  requirement that Chem_UtilTroppFixer, used before Ganymed, cannot satisfy.
!  --------------------------------------------------------------------------
   CALL MAPL_GetPointer(impChem, TROPP, 'TROPP', RC=STATUS)
   VERIFY_(STATUS)

   m = ChemReg%i_XX
   n = ChemReg%j_XX
   iT2M = -1
    
   DO i = m,n
    IF(TRIM(chemReg%vname(i)) == "T2M15d") iT2M = i
    IF(iT2M > 0) EXIT
   END DO

   IF(iT2M < 1) THEN
    PRINT *,TRIM(Iam)//": Invalid index for T2M15d (",iT2M,")"
    STATUS = 1
    VERIFY_(STATUS)
   END IF

   WHERE(tropp /= MAPL_UNDEF) w_c%qa(iT2M)%data3d(:,:,km) = TROPP

   IF( ANY(w_c%qa(iT2M)%data3d(:,:,km) == MAPL_UNDEF) ) THEN
    PRINT *,TRIM(Iam)//": At least one invalid tropopause pressure."
    STATUS = 1
    VERIFY_(STATUS)
   END IF

!  For comparison purposes, export both the "no MAPL_UNDEFs" TROPP and the
!  imported TROPP. Note that AGCM updates TROPP before HISTORY is written.
!  -----------------------------------------------------------------------
   CALL MAPL_GetPointer(expChem, GMITROPP, 'GMITROPP', RC=STATUS)
   VERIFY_(STATUS)
   IF(ASSOCIATED(GMITROPP)) GMITROPP = w_c%qa(iT2M)%data3d(:,:,km)
   CALL MAPL_GetPointer(expChem, AGCMTROPP, 'AGCMTROPP', RC=STATUS)
   VERIFY_(STATUS)
   IF(ASSOCIATED(AGCMTROPP)) AGCMTROPP = TROPP

! Are species tendencies requested?
! ---------------------------------
    m = ChemReg%i_GMI
    n = ChemReg%j_GMI
    ALLOCATE(doMyTendency(m:n), STAT=STATUS)
    VERIFY_(STATUS)
    doMyTendency(:) = .FALSE.

    nAlloc = 0
    DO i = m,n

     fieldName = TRIM(chemReg%vname(i))
     incFieldName = TRIM(fieldName)//"_GMITEND"

     CALL MAPL_GetPointer(expChem, sIncrement, TRIM(incFieldName), RC=STATUS)
     VERIFY_(STATUS)

     IF(ASSOCIATED(sIncrement)) THEN
      NULLIFY(sIncrement)
      nAlloc = nAlloc+1
      doMyTendency(i) = .TRUE.
     END IF

    END DO
    
    IF(nAlloc > 0) doingTendencies = .TRUE.

!  Save current species configurations so chemical tendencies can be calculated.
!  NOTE: Restricted to transported species.
!  ----------------------------------------------------------------------------
   StoreIC: IF(doingTendencies) THEN

    ALLOCATE(sInitial(1:i2,1:j2,km,nAlloc), STAT=STATUS)
    VERIFY_(STATUS)

    k = 1
    m = ChemReg%i_GMI
    n = ChemReg%j_GMI
  
    DO i = m,n
     IF(doMyTendency(i)) THEN
      sInitial(:,:,:,k) = w_c%qa(i)%data3d
      k = k+1
     END IF
    END DO
   
   END IF StoreIC

! ... and for specific humidity
! -----------------------------
   CALL MAPL_GetPointer(impChem, SPECHUM,       'Q', RC=STATUS)
   VERIFY_(STATUS)
   CALL MAPL_GetPointer(expChem, Q_TEND, 'H2O_TEND', RC=STATUS)
   VERIFY_(STATUS)
   IF(ASSOCIATED(Q_TEND)) Q_TEND = SPECHUM

! Is replay running?
! ------------------
   doingPredictorNow = .FALSE.
   CALL ESMF_ConfigGetAttribute(CF, ReplayMode, Label='REPLAY_MODE:', DEFAULT="NoReplay", RC=STATUS)

   IF(ReplayMode == "Regular") THEN
    CALL ESMF_ClockGetAlarm(CLOCK, "PredictorActive", PredictorIsActive, RC=STATUS)

    IF(STATUS == 0) THEN
     doingPredictorNow = ESMF_AlarmIsRinging(PredictorIsActive, RC=STATUS)
     VERIFY_(STATUS)
     IF(MAPL_AM_I_ROOT()) PRINT *,TRIM(Iam)//": Replay predictor step detection: ",doingPredictorNow
    END IF

   END IF

! Pass to the underlying GCs, where needed
! ----------------------------------------
   gcGMI%gcEmiss%doingPredictorNow = doingPredictorNow

!  Run when the ALARM is ringing
!  -----------------------------
   RunningGMI: IF(RunGMINow) THEN

    IF(MAPL_AM_I_ROOT()) THEN
     PRINT *, " "
     PRINT *, TRIM(Iam)//": Running GMI chemistry ..."
    END IF

    CALL MAPL_TimerOn(ggState, "RUN")

    CALL GMI_GridCompRun(gcGMI, w_c, impChem, expChem, nymd, nhms, gmiDt, STATUS)
    VERIFY_(STATUS)

    CALL MAPL_TimerOff(ggState, "RUN")
    CALL ESMF_AlarmRingerOff(ALARM, RC=STATUS)
    VERIFY_(STATUS)

    IF(MAPL_AM_I_ROOT()) PRINT *, " "

   END IF RunningGMI

!  Done with these ...
!  -------------------
   DEALLOCATE(SLR)
   DEALLOCATE(ZTH)

!  Update age-of-air. This transported "specie" is at w_c%qa(ChemReg%i_GMI)%data3d.
!  --------------------------------------------------------------------------------
   n = ChemReg%i_GMI
   w_c%qa(n)%data3d(:,:,:) = w_c%qa(n)%data3d(:,:,:)+runDt/86400.00
   w_c%qa(n)%data3d(:,:,km) = 0.00

!  Gas-phase water in mole fraction.  Purpose: Allow plotting of mole 
!  fraction when using quickplot.  This avoids potential conflicts 
!  with existing directives for plotting specific humidity from MOIST. 
!  -------------------------------------------------------------------
   CALL MAPL_GetPointer(expChem,   WATER, 'GMIH2O', RC=STATUS)
   VERIFY_(STATUS)
   IF(ASSOCIATED(WATER)) WATER(:,:,:) = SPECHUM(:,:,:)*MAPL_AIRMW/MAPL_H2OMW

!  Total ozone: In each layer
!   molecules m^{-2} =  O3(vmr) * Avogadros number * dp / ( mwt air * g )
!  The most recent valid tropopause pressures are stored in T2M15D(:,:,km)
!  -----------------------------------------------------------------------
   CALL MAPL_GetPointer(expChem,  TO3,  'GMITO3', RC=STATUS)
   VERIFY_(STATUS)
   IF(ASSOCIATED( TO3))  TO3 = 0.00
   CALL MAPL_GetPointer(expChem, TTO3, 'GMITTO3', RC=STATUS)
   VERIFY_(STATUS)
   IF(ASSOCIATED(TTO3)) TTO3 = 0.00

   DoingTotalOzone: IF(ASSOCIATED(TTO3) .OR. ASSOCIATED(TO3)) THEN

    m = ChemReg%i_GMI
    n = ChemReg%j_GMI
    iOX = -1
    
    DO i = m,n
     IF(TRIM(chemReg%vname(i)) == "OX") iOX = i
     IF(iOx > 0) EXIT
    END DO

    IF(iOx < 1) THEN
     PRINT *,TRIM(Iam)//": Invalid index for Ox (",iOx,")"
     STATUS = 1
     VERIFY_(STATUS)
    END IF

    ALLOCATE(wrk(SIZE(LATS,1), SIZE(LATS,2)), STAT=STATUS)
    VERIFY_(STATUS)
    ALLOCATE(wgt(SIZE(LATS,1), SIZE(LATS,2)), STAT=STATUS)
    VERIFY_(STATUS)

    DO k = 1,km
     wrk = w_c%qa(iOX)%data3d(:,:,k)*(PLE(:,:,k)-PLE(:,:,k-1))*(MAPL_AVOGAD/2.69E+20)/(MAPL_AIRMW*MAPL_GRAV)
     IF(ASSOCIATED( TO3)) TO3 = TO3+wrk

     IF(ASSOCIATED(TTO3)) THEN
      wgt  = MAX(0.0,MIN(1.0,(PLE(:,:,k)-w_c%qa(iT2M)%data3d(:,:,km))/(PLE(:,:,k)-PLE(:,:,k-1))))
      TTO3 = TTO3+wrk*wgt
     END IF

    END DO

    DEALLOCATE(wrk)
    DEALLOCATE(wgt)

   END IF DoingTotalOzone

!  Obtain chemical tendencies and fill export states.  NOTE: Restricted to transported species.
!  --------------------------------------------------------------------------------------------
   StoreTendencies: IF(doingTendencies) THEN

    k = 1
    m = ChemReg%i_GMI
    n = ChemReg%j_GMI
    dtInverse = 1.00/runDt

    DO i = m,n

     IF(doMyTendency(i)) THEN

      fieldName = TRIM(chemReg%vname(i))
      incFieldName = TRIM(fieldName)//"_GMITEND"

      CALL MAPL_GetPointer(expChem, sIncrement, TRIM(incFieldName), RC=STATUS)
      VERIFY_(STATUS)
      IF(ASSOCIATED(sIncrement)) sIncrement = (w_c%qa(i)%data3d - sInitial(:,:,:,k))*dtInverse

      NULLIFY(sIncrement)
      k = k+1

     END IF

    END DO

!  Clean up
!  --------
    DEALLOCATE(sInitial, STAT=STATUS)
    VERIFY_(STATUS)
   
   END IF StoreTendencies

   DEALLOCATE(doMyTendency, STAT=STATUS)
   VERIFY_(STATUS)

! ... and for specific humidity [kg kg^{-1} s^{-1}]
! -------------------------------------------------
   IF(ASSOCIATED(Q_TEND)) Q_TEND = (SPECHUM-Q_TEND)*dtInverse

!  Stop timer
!  ----------
   CALL MAPL_TimerOff(ggState, "TOTAL")

   RETURN_(ESMF_SUCCESS)

   END SUBROUTINE Run_

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Finalize_ --- Finalize GMIchem_GridComp (ESMF)
!
! !INTERFACE:
!

   SUBROUTINE Finalize_ ( gc, impChem, expChem, clock, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),  intent(inout) :: clock      ! The clock

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout)  :: gc      ! Grid Component
   type(ESMF_State), intent(inout) :: impChem     ! Import State
   type(ESMF_State), intent(inout) :: expChem     ! Export State
   integer, intent(out) ::  rc                    ! Error return code:
                                                  !  0 - all is well
                                                  !  1 - 

! !DESCRIPTION: This is a simple ESMF wrapper.
!
! !REVISION HISTORY:
!
!  27Feb2005 da Silva  First crack.
!
!EOP
!-------------------------------------------------------------------------


!  ErrLog Variables
!  ----------------
   character(len=ESMF_MAXSTR)      :: IAm
   integer                         :: STATUS
   character(len=ESMF_MAXSTR)      :: COMP_NAME

   type(Chem_Registry), pointer    :: chemReg
   type(GMI_GridComp), pointer     :: gcGMI       ! Grid Component
   type(Chem_Bundle), pointer      :: w_c         ! Chemical tracer fields     
   type(MAPL_MetaComp), pointer    :: ggState     ! GEOS Generic State
   integer                         :: nymd, nhms  ! time
   real                            :: gmiDt       ! chemistry timestep (secs)
   real                            :: runDt       ! heartbeat (secs)

    type(GMIchem_state), pointer   :: state

!  Get my name and set-up traceback handle
!  ---------------------------------------
   call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
   VERIFY_(STATUS)
   Iam = TRIM(COMP_NAME)//"Finalize_"

!  Get my internal MAPL_Generic state
!  -----------------------------------
   CALL MAPL_GetObjectFromGC(GC, ggState, RC=STATUS)

!  Start timers
!  ------------
   CALL MAPL_TimerOn(ggState, "TOTAL")
   CALL MAPL_TimerOn(ggState, "FINALIZE")

!  Get ESMF parameters from gc and clock
!  -------------------------------------
   call extract_(gc, clock, chemReg, gcGMI, w_c, nymd, nhms, gmiDt, runDt, STATUS, state = state)
   VERIFY_(STATUS)

!  Call ESMF version
!  -----------------
   call GMI_GridCompFinalize(gcGMI, w_c, impChem, expChem, nymd, nhms, gmiDt, STATUS)
   VERIFY_(STATUS)

!  Destroy Chem_Bundle
!  -------------------
   call Chem_BundleDestroy ( w_c, STATUS )
   VERIFY_(STATUS)

!  Destroy Chem_Registry
!  ---------------------
   call Chem_RegistryDestroy ( chemReg, STATUS ) 
   VERIFY_(STATUS)

!  Destroy Legacy state
!  --------------------
   deallocate ( state%chemReg, state%gcGMI, state%w_c, stat = STATUS )
   VERIFY_(STATUS)

!  Stop timers
!  -----------
   CALL MAPL_TimerOff(ggState, "FINALIZE")
   CALL MAPL_TimerOff(ggState, "TOTAL")

!  Finalize MAPL Generic
!  ---------------------
   call MAPL_GenericFinalize ( gc, impChem, expChem, clock,  RC=STATUS )
   VERIFY_(STATUS)

   RETURN_(ESMF_SUCCESS)

   END SUBROUTINE Finalize_

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
    SUBROUTINE extract_(gc, clock, chemReg, gcGMI, w_c, nymd, nhms, gmiDt, runDt, rc, state)

    type(ESMF_GridComp), intent(inout) :: gc
    type(ESMF_Clock), intent(in)       :: clock
    type(Chem_Registry), pointer       :: chemReg
    type(GMI_GridComp), pointer        :: gcGMI
    type(Chem_Bundle), pointer         :: w_c
    integer, intent(out)               :: nymd, nhms
    real, intent(out)                  :: gmiDt
    real, intent(out)                  :: runDt
    integer, intent(out)               :: rc
    type(GMIchem_state), pointer, optional   :: state


    type(GMIchem_state), pointer    :: myState

!   ErrLog Variables
!   ----------------
    character(len=ESMF_MAXSTR)      :: IAm
    integer                         :: STATUS
    character(len=ESMF_MAXSTR)      :: COMP_NAME

    type(ESMF_Time)      :: TIME
    type(ESMF_Config)    :: CF
    type(GMIchem_Wrap)   :: wrap
    integer              :: IYR, IMM, IDD, IHR, IMN, ISC


!   Get my name and set-up traceback handle
!   ---------------------------------------
    call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // 'extract_'

    rc = 0

!   Get my internal state
!   ---------------------
    call ESMF_UserCompGetInternalState(gc, 'GMIchem_state', WRAP, STATUS)
    VERIFY_(STATUS)
    myState => wrap%ptr
    if ( present(state) ) then
         state => wrap%ptr
    end if

!   This is likely to be allocated during initialize only
!   -----------------------------------------------------
    if ( .not. associated(myState%chemReg) ) then
         allocate ( myState%chemReg, stat=STATUS )
         VERIFY_(STATUS)
    end if
    if ( .not. associated(myState%gcGMI) ) then
         allocate ( myState%gcGMI, stat=STATUS )
         VERIFY_(STATUS)
    end if
    if ( .not. associated(myState%w_c) ) then
         allocate ( myState%w_c, stat=STATUS )
         VERIFY_(STATUS)
    end if

    chemReg => myState%chemReg
    gcGMI  => myState%gcGMI
    w_c     => myState%w_c

!   Get the configuration
!   ---------------------
    call ESMF_GridCompGet ( GC, CONFIG = CF, RC=STATUS )
    VERIFY_(STATUS)

!   Get GEOS-5 time step
!   --------------------
    call ESMF_ConfigGetAttribute ( CF, runDt, LABEL="RUN_DT:", RC=STATUS )
    VERIFY_(STATUS)

!   Chemistry time step can be longer if GMICHEM_DT is set in AGCM.rc
!   -----------------------------------------------------------------
    CALL ESMF_ConfigGetAttribute ( CF, gmiDt, LABEL="GMICHEM_DT:", DEFAULT=runDt, RC=STATUS )
    VERIFY_(STATUS)
    
    IF(gmiDt < runDt) THEN
     IF(MAPL_AM_I_ROOT()) PRINT *,"GMICHEM_DT cannot be less than RUN_DT"
     STATUS = 1
     VERIFY_(STATUS)
    END IF

!   Extract nymd, nhms, day of year from clock
!   ------------------------------------------
    call ESMF_ClockGet(CLOCK,currTIME=TIME,rc=STATUS)
    VERIFY_(STATUS)

    call ESMF_TimeGet(TIME ,YY=IYR, MM=IMM, DD=IDD, H=IHR, M=IMN, S=ISC, rc=STATUS)
    VERIFY_(STATUS)

    call MAPL_PackTime(NYMD,IYR,IMM,IDD)
    call MAPL_PackTime(NHMS,IHR,IMN,ISC)

    RETURN_(ESMF_SUCCESS)

   END SUBROUTINE extract_

subroutine aerosol_optics(state, rc)

  implicit none

! Arguments
! ---------
  type(ESMF_State)     :: state
  integer, intent(out) :: rc


! Local
! ---------
  integer                                 :: n_aerosols
  character(len=ESMF_MAXSTR), allocatable :: aerosol_names(:)
  type(ESMF_FieldBundle)                  :: aerosols

  real, dimension(:,:,:), pointer         :: ple
  real, dimension(:,:,:), pointer         :: rh
  real, dimension(:,:,:), pointer         :: var
  real, dimension(:,:,:), pointer         :: q
  real, dimension(:,:,:,:), pointer       :: q_4d

  real, dimension(:,:,:), allocatable     :: dp, f_p

  character(len=ESMF_MAXSTR)              :: fld_name
  type(ESMF_Field)                        :: fld

  real, dimension(:,:,:,:), allocatable   :: ext, ssa, asy  ! (lon:,lat:,lev:,band:)

  integer                                 :: n
  integer                                 :: i1, j1, i2, j2, km

  integer                                 :: band, offset

  integer                                 :: instance

  integer                                 :: STATUS
  character(len=ESMF_MAXSTR)              :: Iam

  integer, parameter                      :: n_bands = 1

  real    :: x
  integer :: i, j, k

  Iam = 'GOCART::aerosol_optics()'


! Mie Table instance/index
! ------------------------
  call ESMF_AttributeGet(state, name='mie_table_instance', value=instance, __RC__)

! Radiation band
! --------------
  band = 0
  call ESMF_AttributeGet(state, name='band_for_aerosol_optics', value=band, __RC__)
  offset = band - n_bands

! Pressure at layer edges 
! ------------------------
  call ESMF_AttributeGet(state, name='air_pressure_for_aerosol_optics', value=fld_name, __RC__)
  call MAPL_GetPointer(state, ple, trim(fld_name), __RC__)

  i1 = lbound(ple, 1); i2 = ubound(ple, 1)
  j1 = lbound(ple, 2); j2 = ubound(ple, 2)
                       km = ubound(ple, 3)

! Relative humidity
! -----------------
  call ESMF_AttributeGet(state, name='relative_humidity_for_aerosol_optics', value=fld_name, __RC__)
  call MAPL_GetPointer(state, rh, trim(fld_name), __RC__)

  i1 = lbound(rh, 1); i2 = ubound(rh, 1)
  j1 = lbound(rh, 2); j2 = ubound(rh, 2)
                      km = ubound(rh, 3)
  
  call ESMF_StateGet(state, 'AEROSOLS', aerosols, __RC__)
  call ESMF_FieldBundleGet(aerosols, fieldCount=n_aerosols, __RC__)

  allocate(aerosol_names(n_aerosols), __STAT__)
 
  call ESMF_FieldBundleGet(aerosols, FieldNameList=aerosol_names, __RC__)
 
  allocate(ext(i1:i2,j1:j2,km,n_bands), &
           ssa(i1:i2,j1:j2,km,n_bands), &
           asy(i1:i2,j1:j2,km,n_bands), __STAT__)

  allocate(q_4d(i1:i2,j1:j2,km,n_aerosols), __STAT__)

#if (0)
  allocate(dp(i1:i2,j1:j2,km), f_p(i1:i2,j1:j2,km), __STAT__)

  dp  = ple(:,:,1:km) - ple(:,:,0:km-1)
  f_p = dp / MAPL_GRAV

  do n = 1, n_aerosols
      call ESMF_FieldBundleGet(aerosols, trim(aerosol_names(n)), field=fld, __RC__)
      call ESMF_FieldGet(fld, farrayPtr=q, __RC__)

      q_4d(:,:,:,n) = f_p * q
  end do

  call ESMF_AttributeGet(state, name='mie_table_instance', value=instance, __RC__)
  call mie_(gocartMieTable(instance, aerosol_names, n_bands, offset, q_4d, rh, ext, ssa, asy, __RC__)

  deallocate(dp, f_p, __STAT__)
#else
  do n = 1, n_aerosols
      call ESMF_FieldBundleGet(aerosols, trim(aerosol_names(n)), field=fld, __RC__)
      call ESMF_FieldGet(fld, farrayPtr=q, __RC__)

      do k = 1, km
          do j = j1, j2
              do i = i1, i2
                  x = ((PLE(i,j,k) - PLE(i,j,k-1))*0.01)*(100./MAPL_GRAV)
                  q_4d(i,j,k,n) = x * q(i,j,k)
              end do
          end do
      end do
  end do

  call mie_(gocartMieTable(instance), aerosol_names, n_bands, offset, q_4d, rh, ext, ssa, asy, __RC__)
#endif
  
  call ESMF_AttributeGet(state, name='extinction_in_air_due_to_ambient_aerosol', value=fld_name, __RC__)
  if (fld_name /= '') then 
      call MAPL_GetPointer(state, var, trim(fld_name), __RC__)
      var = ext(:,:,:,1)
  end if

  call ESMF_AttributeGet(state, name='single_scattering_albedo_of_ambient_aerosol', value=fld_name, __RC__)
  if (fld_name /= '') then 
      call MAPL_GetPointer(state, var, trim(fld_name), __RC__)
      var = ssa(:,:,:,1)
  end if

  call ESMF_AttributeGet(state, name='asymmetry_parameter_of_ambient_aerosol', value=fld_name, __RC__)
  if (fld_name /= '') then 
      call MAPL_GetPointer(state, var, trim(fld_name), __RC__)
      var = asy(:,:,:,1)
  end if

  deallocate(aerosol_names, ext, ssa, asy, q_4d, __STAT__)

  RETURN_(ESMF_SUCCESS)

contains 

    subroutine mie_(mie_table, aerosol, nb, offset, q, rh, ext, ssa, asy, rc)
     
     implicit none

     type(Chem_Mie),    intent(inout):: mie_table    ! mie table
     character(len=*),  intent(in )  :: aerosol(:)   ! list of aerosols
     integer,           intent(in )  :: nb           ! number of bands
     integer,           intent(in )  :: offset       ! bands offset 
     real,              intent(in )  :: q(:,:,:,:)   ! aerosol mass mixing ratio, kg kg-1
     real,              intent(in )  :: rh(:,:,:)    ! relative humidity

     real,              intent(out)  :: ext(:,:,:,:) ! extinction
     real,              intent(out)  :: ssa(:,:,:,:) ! SSA
     real,              intent(out)  :: asy(:,:,:,:) ! asymmetry parameter

     integer,           intent(out)  :: rc

     ! local
     integer :: STATUS
     character(len=ESMF_MAXSTR) :: Iam='aerosol_optics::mie_' 

     integer :: l, idx, na

     real(kind=8) :: ext_(size(ext,1),size(ext,2),size(ext,3),size(ext,4))
     real(kind=8) :: ssa_(size(ext,1),size(ext,2),size(ext,3),size(ext,4))
     real(kind=8) :: asy_(size(ext,1),size(ext,2),size(ext,3),size(ext,4))

     na = size(aerosol)

     ASSERT_ (na == size(q,4))

     ext_ = 0.0d0
     ssa_ = 0.0d0
     asy_ = 0.0d0

     do l = 1, na
        idx = Chem_MieQueryIdx(mie_table, trim(aerosol(l)), __RC__)

        call Chem_MieQueryAllBand4D(mie_table, idx, nb, offset, q(:,:,:,l), rh, ext, ssa, asy, __RC__)

        ext_ = ext_ +          ext     ! total extinction
        ssa_ = ssa_ +     (ssa*ext)    ! total scattering
        asy_ = asy_ + asy*(ssa*ext)    ! sum of (asy * sca)
     end do

     ext = ext_
     ssa = ssa_
     asy = asy_

     RETURN_(ESMF_SUCCESS)

    end subroutine mie_

end subroutine aerosol_optics

 END MODULE GMIchem_GridCompMod
