!------------------------------------------------------------------------------
!                  Harvard-NASA Emissions Component (HEMCO)                   !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcox_driver_mod.F90 
!
! !DESCRIPTION: Module hcox\_driver\_mod.F90 contains the driver routines 
! (INIT, RUN, FINAL) for the HEMCO extensions. It determines the extensions 
! to be used (based on the settings specified in the configuration file) 
! and invokes the respective extension module calls.
!\\
!\\
! Call this module at the HEMCO - model interface level to execute the
! HEMCO extensions.
!\\
!\\
! !INTERFACE:
!
MODULE HCOX_Driver_Mod
!
! !USES:
!
  USE HCO_Error_Mod
  USE HCO_State_Mod,  ONLY : HCO_State
  USE HCOX_State_Mod, ONLY : Ext_State 

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: HCOX_Init
  PUBLIC :: HCOX_Run
  PUBLIC :: HCOX_Final

  PRIVATE :: HCOX_DiagnDefine
  PRIVATE :: HCOX_DiagnFill
!
! !REMARKS: 
! (1) The extension option objects (e.g. meteorological variables) are 
!     defined in the HEMCO - model interface module and passed to this 
!     module. 
! (2) To add/remove HEMCO extensions from a model application, just 
!     add/remove the corresponding initialize, run, and finalize calls 
!     in the respective driver routines!
!
! !REVISION HISTORY:
!  15 Dec 2013 - C. Keller   - Initial version 
!  01 Jul 2014 - R. Yantosca - Cosmetic changes in ProTeX headers
!  01 Jul 2014 - R. Yantosca - Now use F90 free-format indentation
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !PRIVATE VARIABLES
!

  ! Variables for diagnostics: diagnostics toggle and output frequency.
  LOGICAL, PARAMETER            :: DoDiagn   = .FALSE.

  ! Arrays needed for diagnostics. Diagnostics are defined / filled via
  ! subroutines HCOX_DiagnDefine and HCOX_DiagnFill, respectively.
  REAL(sp), ALLOCATABLE, TARGET :: DGN_SUNCOS   (:,:  )
  REAL(sp), ALLOCATABLE, TARGET :: DGN_DRYTOTN  (:,:  )
  REAL(sp), ALLOCATABLE, TARGET :: DGN_WETTOTN  (:,:  )
  REAL(sp), ALLOCATABLE, TARGET :: DGN_LAI      (:,:  )
!  REAL(sp), ALLOCATABLE, TARGET :: DGN_T2M      (:,:  )
!  REAL(sp), ALLOCATABLE, TARGET :: DGN_GWET     (:,:  )
!  REAL(sp), ALLOCATABLE, TARGET :: DGN_U10M     (:,:  )
!  REAL(sp), ALLOCATABLE, TARGET :: DGN_V10M     (:,:  )
!  REAL(sp), ALLOCATABLE, TARGET :: DGN_PARDR    (:,:  )
!  REAL(sp), ALLOCATABLE, TARGET :: DGN_PARDF    (:,:  )
!  REAL(sp), ALLOCATABLE, TARGET :: DGN_SZAFACT  (:,:  )
!  REAL(sp), ALLOCATABLE, TARGET :: DGN_CLDFRC   (:,:  )
!  REAL(sp), ALLOCATABLE, TARGET :: DGN_ALBD     (:,:  )
!  REAL(sp), ALLOCATABLE, TARGET :: DGN_WLI      (:,:  )
!  REAL(sp), ALLOCATABLE, TARGET :: DGN_TROPP    (:,:  )

CONTAINS
!EOC
!------------------------------------------------------------------------------
!                  Harvard-NASA Emissions Component (HEMCO)                   !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOX_Init
!
! !DESCRIPTION: Subroutine HCOX\_Init is the driver routine to initialize 
!  all enabled HEMCO extensions. 
!\\
! !INTERFACE:
!
  SUBROUTINE HCOX_Init( amIRoot, HcoState, ExtState, RC )
!
! !USES:
!
    USE HCOX_State_MOD,         ONLY : ExtStateInit
    USE HCOX_Custom_Mod,        ONLY : HCOX_Custom_Init
    USE HCOX_SeaFlux_Mod,       ONLY : HCOX_SeaFlux_Init
    USE HCOX_ParaNOx_Mod,       ONLY : HCOX_ParaNOx_Init
    USE HCOX_LightNox_Mod,      ONLY : HCOX_LightNox_Init
    USE HCOX_SoilNox_Mod,       ONLY : HCOX_SoilNox_Init
    USE HCOX_DustDead_Mod,      ONLY : HCOX_DustDead_Init
    USE HCOX_DustGinoux_Mod,    ONLY : HCOX_DustGinoux_Init
    USE HCOX_SeaSalt_Mod,       ONLY : HCOX_SeaSalt_Init
    USE HCOX_GFED_Mod,          ONLY : HCOX_GFED_Init
    USE HCOX_MEGAN_Mod,         ONLY : HCOX_MEGAN_Init
    USE HCOX_Finn_Mod,          ONLY : HCOX_FINN_Init
    USE HCOX_GC_RnPbBe_Mod,     ONLY : HCOX_GC_RnPbBe_Init
    USE HCOX_GC_POPs_Mod,       ONLY : HCOX_GC_POPs_Init
    USE HCOX_CH4WetLand_MOD,    ONLY : HCOX_CH4WETLAND_Init
    USE HCOX_AeroCom_Mod,       ONLY : HCOX_AeroCom_Init
#if defined( TOMAS )
    USE HCOX_TOMAS_SeaSalt_Mod, ONLY : HCOX_TOMAS_SeaSalt_Init
    USE HCOX_TOMAS_DustDead_Mod, ONLY : HCOX_TOMAS_DustDead_Init  
#endif
!
! !INPUT PARAMETERS:
!
    LOGICAL,          INTENT(IN   )  :: amIRoot        ! Are we on root CPU?
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER        :: HcoState       ! HEMCO state object 
    TYPE(Ext_State),  POINTER        :: ExtState       ! HEMCO extension object 
    INTEGER,          INTENT(INOUT)  :: RC             ! Failure or success
!
! !REMARKS:
!  By default we will call routine ExtStateInit, which initializes the
!  ExtState object.  In some instances, we may want to call ExtStateInit
!  from a higher-level routine.  For example, this allows us to pass
!  via ExtState additional scalar quantities from the driving model to
!  HEMCO.  To do this, you will need to (1) call ExtStateInit separately,
!  and (2) set the optional argument NoExtStateInit=.FALSE. flag in the 
!  call to HCOX_INIT.
!
! !REVISION HISTORY: 
!  12 Sep 2013 - C. Keller   - Initial version 
!  07 Jul 2014 - R. Yantosca - Now init GEOS-Chem Rn-Pb-Be emissions module
!  20 Aug 2014 - M. Sulprizio- Now init GEOS-Chem POPs emissions module
!  01 Oct 2014 - R. Yantosca - Now init TOMAS sea salt emissions module
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    CHARACTER(LEN=255) :: MSG

    !=======================================================================
    ! HCOX_INIT begins here!
    !=======================================================================

    ! Error handling 
    CALL HCO_ENTER(HcoState%Config%Err,'HCOX_INIT (hcox_driver_mod.F90)', RC )
    IF ( RC /= HCO_SUCCESS ) RETURN

    !=======================================================================
    ! Initialize extensions 
    !=======================================================================
    CALL ExtStateInit( ExtState, RC )
    IF ( RC /= HCO_SUCCESS ) RETURN

    !-----------------------------------------------------------------------
    ! Custom 
    !-----------------------------------------------------------------------
    CALL HCOX_Custom_Init( amIRoot, HcoState, 'Custom', ExtState, RC )
    IF ( RC /= HCO_SUCCESS ) RETURN

    !-----------------------------------------------------------------------
    ! SeaFlux
    !-----------------------------------------------------------------------
    CALL HCOX_SeaFlux_Init( amIRoot, HcoState, 'SeaFlux', ExtState, RC )
    IF ( RC /= HCO_SUCCESS) RETURN 

    !-----------------------------------------------------------------------
    ! ParaNox
    !-----------------------------------------------------------------------
    CALL HCOX_PARANOX_INIT( amIRoot, HcoState, 'ParaNOx', ExtState, RC )
    IF ( RC /= HCO_SUCCESS ) RETURN 

    !-----------------------------------------------------------------------
    ! LightNox 
    !-----------------------------------------------------------------------
    CALL HCOX_LightNox_Init( amIRoot,  HcoState, 'LightNOx', ExtState, RC )
    IF ( RC /= HCO_SUCCESS ) RETURN 

    !-----------------------------------------------------------------------
    ! SoilNox 
    !-----------------------------------------------------------------------
    CALL HCOX_SoilNox_Init( amIRoot, HcoState, 'SoilNOx', ExtState, RC )
    IF ( RC /= HCO_SUCCESS ) RETURN 

    !-----------------------------------------------------------------------
    ! Dust emissions (DEAD model) 
    !-----------------------------------------------------------------------
    CALL HCOX_DustDead_Init( amIRoot, HcoState, 'DustDead', ExtState,  RC )
    IF ( RC /= HCO_SUCCESS ) RETURN 
#if defined( TOMAS )
    CALL HCOX_TOMAS_DustDead_Init( amIRoot, HcoState, 'TOMAS_DustDead', &
    	 		     	      ExtState,  RC )
    IF ( RC /= HCO_SUCCESS ) RETURN 
#endif 
    !-----------------------------------------------------------------------
    ! Dust Ginoux emissions 
    !-----------------------------------------------------------------------
    CALL HCOX_DustGinoux_Init( amIRoot,  HcoState, 'DustGinoux',  &
                                         ExtState,  RC           )
    IF ( RC /= HCO_SUCCESS ) RETURN 

    !-----------------------------------------------------------------------
    ! SeaSalt aerosol extension
    !-----------------------------------------------------------------------
    CALL HCOX_SeaSalt_Init( amIRoot, HcoState, 'SeaSalt', ExtState, RC )
    IF ( RC /= HCO_SUCCESS ) RETURN 

    !-----------------------------------------------------------------------
    ! MEGAN extension
    !-----------------------------------------------------------------------
    CALL HCOX_Megan_Init( amIRoot, HcoState, 'MEGAN', ExtState, RC ) 
    IF ( RC /= HCO_SUCCESS ) RETURN 

    !-----------------------------------------------------------------------
    ! GFED extension
    !-----------------------------------------------------------------------
    CALL HCOX_GFED_Init( amIRoot, HcoState, 'GFED', ExtState, RC ) 
    IF ( RC /= HCO_SUCCESS ) RETURN 

    !-----------------------------------------------------------------------
    ! FINN biomass burning emissions
    !-----------------------------------------------------------------------
    CALL HcoX_FINN_Init( amIRoot, HcoState, 'FINN', ExtState, RC ) 
    IF( RC /= HCO_SUCCESS ) RETURN 

    !-----------------------------------------------------------------------
    ! Extension for GEOS-Chem Rn-Pb-Be specialty simulation
    !-----------------------------------------------------------------------
    CALL HCOX_GC_RnPbBe_Init( amIRoot, HcoState, 'GC_Rn-Pb-Be', &
                                       ExtState,  RC ) 
    IF ( RC /= HCO_SUCCESS ) RETURN 

    !-----------------------------------------------------------------------
    ! Extension for GEOS-Chem POPs specialty simulation
    !-----------------------------------------------------------------------
    CALL HCOX_GC_POPs_Init( amIRoot, HcoState, 'GC_POPs', &
                                     ExtState,  RC ) 
    IF ( RC /= HCO_SUCCESS ) RETURN 

    !-----------------------------------------------------------------------
    ! CH4 wetland emissions 
    !-----------------------------------------------------------------------
    CALL HCOX_CH4Wetland_Init( amIRoot,  HcoState, 'CH4_WETLANDS', &
                                         ExtState,  RC ) 
    IF ( RC /= HCO_SUCCESS ) RETURN 

    !-----------------------------------------------------------------------
    ! AeroCom volcano emissions 
    !-----------------------------------------------------------------------
    CALL HCOX_AeroCom_Init( amIRoot,  HcoState, 'AeroCom_Volcano', &
                            ExtState,  RC ) 
    IF ( RC /= HCO_SUCCESS ) RETURN 

#if defined( TOMAS )
    !-----------------------------------------------------------------------
    ! TOMAS sectional sea salt aerosol emissions
    !-----------------------------------------------------------------------
    CALL HCOX_TOMAS_SeaSalt_Init( amIRoot, HcoState, 'TOMAS_SeaSalt',  &
                                           ExtState,  RC              ) 
    IF ( RC /= HCO_SUCCESS ) RETURN 
#endif

    !-----------------------------------------------------------------------
    ! Add extensions here ...
    !-----------------------------------------------------------------------

    !=======================================================================
    ! Sanity checks 
    !=======================================================================

    ! Cannot have both DustDead and DustGinoux turned on!
    IF ( ExtState%DustDead > 0 .AND. ExtState%DustGinoux ) THEN
       MSG = 'Ginoux and DEAD dust emissions switched on!'
       CALL HCO_ERROR(HcoState%Config%Err,MSG, RC )
       RETURN
    ENDIF

    !=======================================================================
    ! Define diagnostics 
    !=======================================================================
    CALL HCOX_DiagnDefine( amIRoot, HcoState, ExtState, RC )
    IF ( RC /= HCO_SUCCESS ) RETURN 

    !=======================================================================
    ! Leave w/ success
    !=======================================================================
    CALL HCO_LEAVE( HcoState%Config%Err,RC )

  END SUBROUTINE HCOX_Init
!EOC
!------------------------------------------------------------------------------
!                  Harvard-NASA Emissions Component (HEMCO)                   !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOX_Run
!
! !DESCRIPTION: Subroutine HCOX\_Run is the driver routine to run the HEMCO
! extensions. All enabled emission extensions are executed, and the
! emissions calculated therein become added to the respective flux arrays 
! in HcoState.\\
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOX_Run( amIRoot, HcoState, ExtState, RC )
!
! !USES:
!
    USE HCO_Clock_Mod,          ONLY : HcoClock_Get
    USE HCOX_Custom_Mod,        ONLY : HCOX_Custom_Run
    USE HCOX_SeaFlux_Mod,       ONLY : HCOX_SeaFlux_Run 
    USE HCOX_ParaNox_Mod,       ONLY : HCOX_ParaNox_Run 
    USE HCOX_LightNox_Mod,      ONLY : HCOX_LightNox_Run 
    USE HCOX_SoilNox_Mod,       ONLY : HCOX_SoilNox_Run 
    USE HCOX_DustDead_Mod,      ONLY : HCOX_DustDead_Run 
    USE HCOX_DustGinoux_Mod,    ONLY : HCOX_DustGinoux_Run 
    USE HCOX_SeaSalt_Mod,       ONLY : HCOX_SeaSalt_Run 
    USE HCOX_Megan_Mod,         ONLY : HCOX_Megan_Run 
    USE HCOX_GFED_Mod,          ONLY : HCOX_GFED_Run 
    USE HcoX_FINN_Mod,          ONLY : HcoX_FINN_Run 
    USE HCOX_GC_RnPbBe_Mod,     ONLY : HCOX_GC_RnPbBe_Run
    USE HCOX_GC_POPs_Mod,       ONLY : HCOX_GC_POPs_Run
    USE HCOX_CH4WetLand_mod,    ONLY : HCOX_CH4Wetland_Run
    USE HCOX_AeroCom_Mod,       ONLY : HCOX_AeroCom_Run
#if defined( TOMAS )
    USE HCOX_TOMAS_SeaSalt_Mod, ONLY : HCOX_TOMAS_SeaSalt_Run
    USE HCOX_TOMAS_DustDead_Mod, ONLY : HCOX_TOMAS_DustDead_Run
#endif
!
! !INPUT PARAMETERS:
!
    LOGICAL,          INTENT(IN   )  :: amIRoot    ! root CPU?
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER        :: HcoState   ! HEMCO state object 
    TYPE(Ext_State),  POINTER        :: ExtState   ! Extension options object 
    INTEGER,          INTENT(INOUT)  :: RC         ! Failure or success
!
! !NOTE:
!
! The ExtState object contains all extension option objects. In particular, 
! it contains the pointers to all met fields used by the extensions. These
! pointers have to be set in the HEMCO-model interface module beforehand!
!
! !REVISION HISTORY: 
!  15 Dec 2013 - C. Keller   - Initial version 
!  07 Jul 2014 - R. Yantosca - Now Run GEOS-Chem Rn-Pb-Be emissions module
!  20 Aug 2014 - M. Sulprizio- Now run GEOS-Chem POPs emissions module
!  01 Oct 2014 - R. Yantosca - Now run TOMAS sea salt emissions module
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    CHARACTER(LEN=255) :: MSG
    LOGICAL            :: IsEmisTime

    !=======================================================================
    ! HCOX_RUN begins here!
    !=======================================================================

    ! For error handling
    CALL HCO_ENTER(HcoState%Config%Err,'HCOX_RUN (hcox_driver_mod.F90)', RC )
    IF ( RC /= HCO_SUCCESS ) RETURN

    ! Is it time for emissions?
    CALL HcoClock_Get ( amIRoot, HcoState%Clock, IsEmisTime=IsEmisTime, RC=RC )
    IF ( RC /= HCO_SUCCESS ) RETURN

    ! Can leave here if it's not time for emissions
    IF ( .NOT. IsEmisTime ) THEN
       CALL HCO_LEAVE( HcoState%Config%Err,RC )
       RETURN
    ENDIF

    !-----------------------------------------------------------------------
    ! Customized emissions 
    !-----------------------------------------------------------------------
    IF ( ExtState%Custom ) THEN
       CALL HCOX_Custom_Run( amIRoot, ExtState, HcoState, RC)
       IF ( RC /= HCO_SUCCESS ) RETURN 
    ENDIF

    !-----------------------------------------------------------------------
    ! SeaFlx (Air-sea exchange)
    !-----------------------------------------------------------------------
    IF ( ExtState%SeaFlux) THEN
       CALL HCOX_SeaFlux_Run( amIRoot, ExtState, HcoState, RC)
       IF ( RC /= HCO_SUCCESS ) RETURN 
    ENDIF

    !-----------------------------------------------------------------------
    ! ParaNox (Ship NO emissions) 
    !-----------------------------------------------------------------------
    IF (ExtState%ParaNOx ) THEN
       CALL HCOX_ParaNox_Run( amIRoot, ExtState, HcoState, RC )
       IF ( RC /= HCO_SUCCESS ) RETURN 
    ENDIF

    !-----------------------------------------------------------------------
    ! Lightning NOx  
    !-----------------------------------------------------------------------
    IF ( ExtState%LightNOx > 0 ) THEN
       CALL HCOX_LightNox_Run( amIRoot, ExtState, HcoState, RC )
       IF ( RC /= HCO_SUCCESS ) RETURN 
    ENDIF

    !-----------------------------------------------------------------------
    ! SoilNOx  
    !-----------------------------------------------------------------------
    IF ( ExtState%SoilNOx > 0 ) THEN
       CALL HCOX_SoilNox_Run( amIRoot, ExtState, HcoState, RC )
       IF ( RC /= HCO_SUCCESS ) RETURN 
    ENDIF

    !-----------------------------------------------------------------------
    ! Dust emissions (DEAD model) 
    !-----------------------------------------------------------------------
    IF ( ExtState%DustDead > 0 ) THEN
       CALL HCOX_DustDead_Run( amIRoot, ExtState, HcoState, RC )
       IF ( RC /= HCO_SUCCESS ) RETURN 
    ENDIF

#if defined( TOMAS )
    IF ( ExtState%TOMAS_DustDead ) THEN
       !print*, 'JACK TOMAS_DustDead is on'
       CALL HCOX_TOMAS_DustDead_Run( amIRoot, ExtState, HcoState, RC )
       IF ( RC /= HCO_SUCCESS ) RETURN 
    ENDIF
#endif 

    !-----------------------------------------------------------------------
    ! Dust emissions (Ginoux)
    !-----------------------------------------------------------------------
    IF ( ExtState%DustGinoux ) THEN
       CALL HCOX_DustGinoux_Run( amIRoot, ExtState, HcoState, RC )
       IF ( RC /= HCO_SUCCESS ) RETURN 
    ENDIF

    !-----------------------------------------------------------------------
    ! Sea salt aerosols
    !-----------------------------------------------------------------------
    IF ( ExtState%SeaSalt ) THEN
       CALL HCOX_SeaSalt_Run( amIRoot, ExtState, HcoState, RC )
       IF ( RC /= HCO_SUCCESS ) RETURN 
    ENDIF

    !-----------------------------------------------------------------------
    ! MEGAN biogenic emissions 
    !-----------------------------------------------------------------------
    IF ( ExtState%Megan > 0 ) THEN
       CALL HCOX_Megan_Run( amIRoot, ExtState, HcoState, RC )
       IF ( RC /= HCO_SUCCESS ) RETURN 
    ENDIF

    !-----------------------------------------------------------------------
    ! GFED biomass burning emissions 
    !-----------------------------------------------------------------------
    IF ( ExtState%GFED ) THEN
       CALL HCOX_GFED_Run( amIRoot, ExtState, HcoState, RC )
       IF ( RC /= HCO_SUCCESS ) RETURN 
    ENDIF

    !-----------------------------------------------------------------------
    ! FINN biomass burning emissions 
    ! ----------------------------------------------------------------------
    IF ( ExtState%FINN ) THEN
       CALL HcoX_FINN_Run( amIRoot, ExtState, HcoState, RC )
       IF ( RC /= HCO_SUCCESS ) RETURN 
    ENDIF

    !-----------------------------------------------------------------------
    ! Emissions for GEOS-Chem Rn-Pb-Be specialty simulation
    !-----------------------------------------------------------------------
    IF ( ExtState%GC_RnPbBe ) THEN
       CALL HCOX_GC_RnPbBe_Run( amIRoot, ExtState, HcoState, RC )
       IF ( RC /= HCO_SUCCESS ) RETURN 
    ENDIF

    !-----------------------------------------------------------------------
    ! Emissions for GEOS-Chem POPs specialty simulation
    !-----------------------------------------------------------------------
    IF ( ExtState%GC_POPs ) THEN
       CALL HCOX_GC_POPs_Run( amIRoot, ExtState, HcoState, RC )
       IF ( RC /= HCO_SUCCESS ) RETURN 
    ENDIF

    !-----------------------------------------------------------------------
    ! CH4 wetland emissions 
    !-----------------------------------------------------------------------
    IF ( ExtState%Wetland_CH4 > 0 ) THEN
       CALL HCOX_CH4Wetland_Run( amIRoot, ExtState, HcoState, RC )
       IF ( RC /= HCO_SUCCESS ) RETURN 
    ENDIF

    !-----------------------------------------------------------------------
    ! TOMAS sectional sea salt emissions
    !-----------------------------------------------------------------------
#if defined( TOMAS )
    IF ( ExtState%TOMAS_SeaSalt ) THEN
       CALL HCOX_TOMAS_SeaSalt_Run( amIRoot, ExtState, HcoState, RC )
       IF ( RC /= HCO_SUCCESS ) RETURN 
    ENDIF
#endif

    !-----------------------------------------------------------------------
    ! AeroCom volcano emissions 
    !-----------------------------------------------------------------------
    IF ( ExtState%AeroCom > 0 ) THEN
       CALL HCOX_AeroCom_Run( amIRoot, ExtState, HcoState, RC )
       IF ( RC /= HCO_SUCCESS ) RETURN 
    ENDIF

    !-----------------------------------------------------------------
    ! Add extensions here ...
    !-----------------------------------------------------------------

    !=======================================================================
    ! Fill diagnostics
    ! This updates the diagnostics defined in HCOX_DiagnDefine. Subroutine 
    ! HCOIO_DIAGN_WRITEOUT can be used to write out diagnostics to disk.
    ! This subroutine is called in higher-level routines. 
    !=======================================================================
    CALL HCOX_DiagnFill( amIRoot, HcoState, ExtState, RC )
    IF ( RC /= HCO_SUCCESS ) RETURN 

    !=======================================================================
    ! Return w/ success 
    !=======================================================================
    CALL HCO_LEAVE( HcoState%Config%Err,RC )

  END SUBROUTINE HCOX_Run
!EOC
!------------------------------------------------------------------------------
!                  Harvard-NASA Emissions Component (HEMCO)                   !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOX_Final
!
! !DESCRIPTION: Subroutine HCOX\_Final finalizes all HEMCO extensions. 
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOX_Final( am_I_Root, HcoState, ExtState, RC )
!
! !USES:
!
    USE HCOX_State_Mod,         ONLY : ExtStateFinal
    USE HCOX_Custom_Mod,        ONLY : HCOX_Custom_Final
    USE HCOX_SeaFlux_Mod,       ONLY : HCOX_SeaFlux_Final
    USE HCOX_ParaNOx_Mod,       ONLY : HCOX_PARANOX_Final
    USE HCOX_LightNox_Mod,      ONLY : HCOX_LightNox_Final
    USE HCOX_SoilNox_Mod,       ONLY : HCOX_SoilNox_Final
    USE HCOX_DustDead_Mod,      ONLY : HCOX_DustDead_Final
    USE HCOX_DustGinoux_Mod,    ONLY : HCOX_DustGinoux_Final
    USE HCOX_SeaSalt_Mod,       ONLY : HCOX_SeaSalt_Final
    USE HCOX_MEGAN_Mod,         ONLY : HCOX_MEGAN_Final
    USE HCOX_GFED_Mod,          ONLY : HCOX_GFED_Final
    USE HcoX_FINN_Mod,          ONLY : HcoX_FINN_Final
    USE HCOX_GC_RnPbBe_Mod,     ONLY : HCOX_GC_RnPbBe_Final
    USE HCOX_GC_POPs_Mod,       ONLY : HCOX_GC_POPs_Final
    USE HCOX_CH4WetLand_Mod,    ONLY : HCOX_CH4Wetland_Final
    USE HCOX_AeroCom_Mod,       ONLY : HCOX_AeroCom_Final
#if defined( TOMAS )
    USE HCOX_TOMAS_SeaSalt_Mod, ONLY : HCOX_TOMAS_SeaSalt_Final
    USE HCOX_TOMAS_DustDead_Mod, ONLY : HCOX_TOMAS_DustDead_Final
#endif
!
! !INPUT PARAMETERS:
!
    LOGICAL,          INTENT(IN   )  :: am_I_Root  ! root CPU?
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER        :: HcoState   ! HEMCO state object 
    TYPE(Ext_State),  POINTER        :: ExtState   ! Extension options object 
    INTEGER,          INTENT(INOUT)  :: RC         ! Failure or success
!
! !REVISION HISTORY: 
!  12 Sep 2013 - C. Keller   - Initial version 
!  07 Jul 2014 - R. Yantosca - Now finalize GEOS-Chem Rn-Pb-Be emissions pkg
!  20 Aug 2014 - M. Sulprizio- Now finalize GEOS-Chen POPs emissions module
!  01 Oct 2014 - R. Yantosca - Now finalize TOMAS sea salt emissions module
!  09 Mar 2015 - C. Keller   - Now pass HcoState since it is needed by some
!                              finalization calls.
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!

    !=======================================================================
    ! HCOX_FINAL begins here!
    !=======================================================================

    IF ( ASSOCIATED( ExtState ) ) THEN 

       ! Nullify all ExtState object pointers
       CALL ExtStateFinal( ExtState ) 

       ! Call individual cleanup routines
       IF ( ExtState%Custom        ) CALL HCOX_Custom_Final()
       IF ( ExtState%SeaFlux       ) CALL HCOX_SeaFlux_Final()
       IF ( ExtState%ParaNOx       ) CALL HCOX_PARANOX_Final(am_I_Root,HcoState,RC)
       IF ( ExtState%LightNOx > 0  ) CALL HCOX_LIGHTNOX_Final( ExtState )
       IF ( ExtState%DustDead > 0  ) CALL HCOX_DustDead_Final( ExtState )
#if defined( TOMAS)
       IF ( ExtState%TOMAS_DustDead )  CALL HCOX_TOMAS_DustDead_Final()
#endif
       IF ( ExtState%DustGinoux    ) CALL HCOX_DustGinoux_Final()
       IF ( ExtState%SeaSalt       ) CALL HCOX_SeaSalt_Final()
       IF ( ExtState%Megan /= 0    ) CALL HCOX_Megan_Final(am_I_Root,HcoState,ExtState,RC)
       IF ( ExtState%GFED          ) CALL HCOX_GFED_Final()
       IF ( ExtState%SoilNOx > 0   ) CALL HCOX_SoilNox_Final(am_I_Root,HcoState,ExtState,RC)
       IF ( ExtState%FINN          ) CALL HcoX_FINN_Final
       IF ( ExtState%GC_RnPbBe     ) CALL HCOX_GC_RnPbBe_Final()
       IF ( ExtState%GC_POPs       ) CALL HCOX_GC_POPs_Final()
       IF ( ExtState%Wetland_CH4>0 ) CALL HCOX_CH4Wetland_Final( ExtState )
       IF ( ExtState%AeroCom > 0   ) CALL HCOX_AeroCom_Final( ExtState )
#if defined( TOMAS )
       IF ( ExtState%TOMAS_SeaSalt ) CALL HCOX_TOMAS_SeaSalt_Final()
#endif       

       ! Deallocate ExtState object
       DEALLOCATE( ExtState )
       ExtState => NULL()
    ENDIF

    ! Eventually deallocate diagnostics array
    IF ( ALLOCATED( DGN_LAI      ) ) DEALLOCATE( DGN_LAI      )
!    IF ( ALLOCATED( DGN_T2M      ) ) DEALLOCATE( DGN_T2M      )
!    IF ( ALLOCATED( DGN_GWET     ) ) DEALLOCATE( DGN_GWET     )
!    IF ( ALLOCATED( DGN_U10M     ) ) DEALLOCATE( DGN_U10M     )
!    IF ( ALLOCATED( DGN_V10M     ) ) DEALLOCATE( DGN_V10M     )
!    IF ( ALLOCATED( DGN_PARDR    ) ) DEALLOCATE( DGN_PARDR    )
!    IF ( ALLOCATED( DGN_PARDF    ) ) DEALLOCATE( DGN_PARDF    )
!    IF ( ALLOCATED( DGN_SZAFACT  ) ) DEALLOCATE( DGN_SZAFACT  )
!    IF ( ALLOCATED( DGN_CLDFRC   ) ) DEALLOCATE( DGN_CLDFRC   )
!    IF ( ALLOCATED( DGN_ALBD     ) ) DEALLOCATE( DGN_ALBD     )
!    IF ( ALLOCATED( DGN_WLI      ) ) DEALLOCATE( DGN_WLI      )
!    IF ( ALLOCATED( DGN_TROPP    ) ) DEALLOCATE( DGN_TROPP    )
    IF ( ALLOCATED( DGN_SUNCOS   ) ) DEALLOCATE( DGN_SUNCOS   )
    IF ( ALLOCATED( DGN_DRYTOTN  ) ) DEALLOCATE( DGN_DRYTOTN  )
    IF ( ALLOCATED( DGN_WETTOTN  ) ) DEALLOCATE( DGN_WETTOTN  )

    ! Return w/ success
    RC = HCO_SUCCESS
 
  END SUBROUTINE HCOX_Final
!EOC
!------------------------------------------------------------------------------
!                  Harvard-NASA Emissions Component (HEMCO)                   !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOX_DiagnDefine
!
! !DESCRIPTION: Subroutine HCOX\_DiagnDefine creates custom-defined diagnostics. 
!  This is very preliminary and for testing only.
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOX_DiagnDefine( am_I_Root, HcoState, ExtState, RC )
!
! !USES:
!
!
! !INPUT PARAMETERS:
!
    LOGICAL,          INTENT(IN   )  :: am_I_Root    ! root CPU?
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER        :: HcoState   ! HEMCO state object 
    TYPE(Ext_State),  POINTER        :: ExtState   ! Extension options object 
    INTEGER,          INTENT(INOUT)  :: RC         ! Failure or success
!
! !REVISION HISTORY: 
!  19 Feb 2015 - C. Keller   - Initial version 
!EOP
!------------------------------------------------------------------------------
!
! !LOCAL VARIABLES:
!
    INTEGER            :: I,  J, AS
    INTEGER            :: ExtNr, II
    CHARACTER(LEN=255) :: LOC = 'HCOX_DiagnDefine (hcox_driver_mod.F90)'

    !=======================================================================
    ! HCOX_DiagnDefine begins here!
    !=======================================================================

    I = HcoState%NX
    J = HcoState%NY
    ExtNr = -1

    IF ( DoDiagn ) THEN

       ALLOCATE( DGN_LAI(I,J), STAT=AS )
       IF ( AS /= 0 ) THEN
          CALL HCO_ERROR( HcoState%Config%Err, 'Diagnostics allocation error 1', RC, THISLOC=LOC )
          RETURN
       ENDIF
!       ALLOCATE( DGN_GWET(I,J), STAT=AS )
!       IF ( AS /= 0 ) THEN
!          CALL HCO_ERROR( HcoState%Config%Err, 'Diagnostics allocation error 1', RC, THISLOC=LOC )
!          RETURN
!       ENDIF
!       ALLOCATE( DGN_T2M(I,J), DGN_V10M(I,J), DGN_U10M(I,J), STAT=AS )
!       IF ( AS /= 0 ) THEN
!          CALL HCO_ERROR( HcoState%Config%Err, 'Diagnostics allocation error 2', RC, THISLOC=LOC )
!          RETURN
!       ENDIF
!       ALLOCATE( DGN_PARDR(I,J), DGN_PARDF(I,J), DGN_SZAFACT(I,J), STAT=AS )
!       IF ( AS /= 0 ) THEN
!          CALL HCO_ERROR( HcoState%Config%Err, 'Diagnostics allocation error 3', RC, THISLOC=LOC )
!          RETURN
!       ENDIF
!       ALLOCATE( DGN_CLDFRC(I,J), DGN_ALBD(I,J), DGN_WLI(I,J), STAT=AS )
!       IF ( AS /= 0 ) THEN
!          CALL HCO_ERROR( HcoState%Config%Err, 'Diagnostics allocation error 4', RC, THISLOC=LOC )
!          RETURN
!       ENDIF
!       ALLOCATE( DGN_TROPP(I,J), STAT=AS )
!       IF ( AS /= 0 ) THEN
!          CALL HCO_ERROR( HcoState%Config%Err, 'Diagnostics allocation error 5', RC, THISLOC=LOC )
!          RETURN
!       ENDIF

       ALLOCATE( DGN_SUNCOS(I,J), DGN_DRYTOTN(I,J), DGN_WETTOTN(I,J), STAT=AS )
       IF ( AS /= 0 ) THEN
          CALL HCO_ERROR( HcoState%Config%Err, 'Diagnostics allocation error 6', RC, THISLOC=LOC )
          RETURN
       ENDIF

       DGN_LAI     = 0.0_sp
!       DGN_T2M     = 0.0_sp
!       DGN_GWET    = 0.0_sp
!       DGN_V10M    = 0.0_sp
!       DGN_U10M    = 0.0_sp
!       DGN_PARDR   = 0.0_sp
!       DGN_PARDF   = 0.0_sp
!       DGN_SZAFACT = 0.0_sp
!       DGN_CLDFRC  = 0.0_sp
!       DGN_ALBD    = 0.0_sp
!       DGN_WLI     = 0.0_sp
!       DGN_TROPP   = 0.0_sp
       DGN_SUNCOS  = 0.0_sp
       DGN_DRYTOTN = 0.0_sp
       DGN_WETTOTN = 0.0_sp

!       CALL DgnDefine ( am_I_Root, HcoState, 'HCO_T2M', DGN_T2M, RC ) 
!       IF ( RC /= HCO_SUCCESS ) RETURN
!
!       CALL DgnDefine ( am_I_Root, HcoState, 'HCO_GWET', DGN_GWET, RC ) 
!       IF ( RC /= HCO_SUCCESS ) RETURN
!
       CALL DgnDefine ( am_I_Root, HcoState, 'HCO_LAI', DGN_LAI, RC ) 
       IF ( RC /= HCO_SUCCESS ) RETURN
   
!       CALL DgnDefine ( am_I_Root, HcoState, 'HCO_U10M', DGN_U10M, RC ) 
!       IF ( RC /= HCO_SUCCESS ) RETURN
!   
!       CALL DgnDefine ( am_I_Root, HcoState, 'HCO_V10M', DGN_V10M, RC ) 
!       IF ( RC /= HCO_SUCCESS ) RETURN
!   
!       CALL DgnDefine ( am_I_Root, HcoState, 'HCO_PARDR', DGN_PARDR, RC ) 
!       IF ( RC /= HCO_SUCCESS ) RETURN
!   
!       CALL DgnDefine ( am_I_Root, HcoState, 'HCO_PARDF', DGN_PARDF, RC ) 
!       IF ( RC /= HCO_SUCCESS ) RETURN
!   
!       CALL DgnDefine ( am_I_Root, HcoState, 'HCO_SZAFACT', DGN_SZAFACT, RC ) 
!       IF ( RC /= HCO_SUCCESS ) RETURN
!   
!       CALL DgnDefine ( am_I_Root, HcoState, 'HCO_CLDFRC', DGN_CLDFRC, RC ) 
!       IF ( RC /= HCO_SUCCESS ) RETURN
!   
!       CALL DgnDefine ( am_I_Root, HcoState, 'HCO_ALBD', DGN_ALBD, RC ) 
!       IF ( RC /= HCO_SUCCESS ) RETURN
!   
!       CALL DgnDefine ( am_I_Root, HcoState, 'HCO_WLI', DGN_WLI, RC ) 
!       IF ( RC /= HCO_SUCCESS ) RETURN
!   
!       CALL DgnDefine ( am_I_Root, HcoState, 'HCO_TROPP', DGN_TROPP, RC ) 
!       IF ( RC /= HCO_SUCCESS ) RETURN

       CALL DgnDefine ( am_I_Root, HcoState, 'HCO_SUNCOS', DGN_SUNCOS, RC ) 
       IF ( RC /= HCO_SUCCESS ) RETURN
   
       CALL DgnDefine ( am_I_Root, HcoState, 'DRY_TOTN', DGN_DRYTOTN, RC ) 
       IF ( RC /= HCO_SUCCESS ) RETURN
   
       CALL DgnDefine ( am_I_Root, HcoState, 'WET_TOTN', DGN_WETTOTN, RC ) 
       IF ( RC /= HCO_SUCCESS ) RETURN
   
    ENDIF

    ! Return w/ success
    RC = HCO_SUCCESS

  END SUBROUTINE HCOX_DiagnDefine
!EOC
!------------------------------------------------------------------------------
!                  Harvard-NASA Emissions Component (HEMCO)                   !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: DgnDefine
!
! !DESCRIPTION: Helper routine to define a target diagnostics. 
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE DgnDefine ( am_I_Root, HcoState, DgnName, Trgt2D, RC ) 
!
! !USES:
!
    USE HCO_DIAGN_MOD, ONLY : Diagn_Create 
!
! !INPUT PARAMETERS:
!
    LOGICAL,          INTENT(IN   )  :: am_I_Root    ! root CPU?
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER        :: HcoState   ! HEMCO state object 
    CHARACTER(LEN=*), INTENT(IN   )  :: DgnName      ! diagnostics name
    REAL(sp),         INTENT(IN   )  :: Trgt2D(HcoState%NX,HcoState%NY)
    INTEGER,          INTENT(INOUT)  :: RC         ! Failure or success
!
! !REVISION HISTORY: 
!  19 Feb 2015 - C. Keller   - Initial version 
!EOP
!------------------------------------------------------------------------------

    CALL Diagn_Create ( am_I_Root, HcoState%Diagn,      &
                        HcoState   = HcoState,          & 
                        cName      = TRIM(DgnName),     &
                        ExtNr      = -1,                &
                        Cat        = -1,                &
                        Hier       = -1,                &
                        HcoID      = -1,                &
                        SpaceDim   = 2,                 &
                        OutUnit    = '1',               &
                        AutoFill   = 0,                 &
                        Trgt2D     = Trgt2D,            &
                        COL = HcoState%Diagn%HcoDiagnIDDefault, &
                        RC         = RC                  )
    IF ( RC /= HCO_SUCCESS ) RETURN

    ! Return w/ success
    RC = HCO_SUCCESS

  END SUBROUTINE DgnDefine 
!EOC
!------------------------------------------------------------------------------
!                  Harvard-NASA Emissions Component (HEMCO)                   !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOX_DiagnFill
!
! !DESCRIPTION: Subroutine HCOX\_DiagnFill fills custom-defined diagnostics. 
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOX_DiagnFill( am_I_Root, HcoState, ExtState, RC )
!
! !USES:
!
!
! !INPUT PARAMETERS:
!
    LOGICAL,          INTENT(IN   )  :: am_I_Root    ! root CPU?
!
! !INPUT/OUTPUT PARAMETERS:
!
    TYPE(HCO_State),  POINTER        :: HcoState   ! HEMCO state object 
    TYPE(Ext_State),  POINTER        :: ExtState   ! Extension options object 
    INTEGER,          INTENT(INOUT)  :: RC         ! Failure or success
!
! !REVISION HISTORY: 
!  19 Feb 2015 - C. Keller   - Initial version 
!EOP
!------------------------------------------------------------------------------

    IF ( DoDiagn ) THEN
       DGN_LAI     = ExtState%LAI%Arr%Val
!       DGN_T2M     = ExtState%T2M%Arr%Val
!       DGN_GWET    = ExtState%GWETTOP%Arr%Val
!       DGN_U10M    = ExtState%U10M%Arr%Val
!       DGN_V10M    = ExtState%V10M%Arr%Val
!       DGN_PARDR   = ExtState%PARDR%Arr%Val
!       DGN_PARDF   = ExtState%PARDF%Arr%Val
!       DGN_SZAFACT = ExtState%SZAFACT%Arr%Val
!       DGN_CLDFRC  = ExtState%CLDFRC%Arr%Val
!       DGN_ALBD    = ExtState%ALBD%Arr%Val
!       DGN_WLI     = ExtState%WLI%Arr%Val
!       DGN_TROPP   = ExtState%TROPP%Arr%Val
       DGN_SUNCOS  = ExtState%SUNCOS%Arr%Val
       DGN_DRYTOTN = ExtState%DRY_TOTN%Arr%Val
       DGN_WETTOTN = ExtState%WET_TOTN%Arr%Val
    ENDIF
   
    ! Return w/ success
    RC = HCO_SUCCESS

  END SUBROUTINE HCOX_DiagnFill
!EOC
END MODULE HCOX_Driver_Mod
