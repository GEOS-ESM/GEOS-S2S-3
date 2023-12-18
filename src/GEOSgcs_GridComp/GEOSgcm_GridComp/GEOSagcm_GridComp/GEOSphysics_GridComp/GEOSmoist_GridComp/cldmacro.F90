! $Id: cldmacro.F90,v 1.3.6.4.2.2.2.1.2.6.4.3.2.4.2.3.6.4.22.4.2.5.6.2.2.2.2.4.22.1.72.1.6.1.4.1.4.1.30.1.6.3 2019/06/05 20:49:52 dbarahon Exp $
! $Name: GEOS-S2S-3_20231512 $

module cldmacro


   !This module handles large scale condesation and cloud fraction, convective precipitation, and makes 
   ! preliminary calculations for the two-moment cloud microphysics.  
   !=======================================================================

   use CLDPARAMS
   use GEOS_UtilsMod,     only:QSAT=>GEOS_Qsat, DQSAT=>GEOS_DQsat, &
         QSATLQ=>GEOS_QsatLQU, QSATIC=>GEOS_QsatICE

   use MAPL_ConstantsMod, only: MAPL_TICE , MAPL_CP   , &
         MAPL_GRAV , MAPL_ALHS , &
         MAPL_ALHL , MAPL_ALHF , &
         MAPL_RGAS , MAPL_H2OMW, &
         MAPL_AIRMW, MAPL_RVAP , &
         MAPL_PI   , MAPL_R8   , &
         MAPL_R4

   use MAPL_BaseMod,      only: MAPL_UNDEF

   implicit none



   private

   PUBLIC MACRO_CLOUD
   PUBLIC UPDATE_CLD
   public meltfrz_inst
   public fix_up_clouds_2M
   public Bergeron_iter
   
   
   !! Some parameters set by CLDPARAMS 

   real    :: CNV_BETA
   real    :: ANV_BETA
   real    :: LS_BETA
   real    :: RH00
   real    :: C_00
   real    :: LWCRIT
   real    :: C_ACC
   real    :: C_EV_R
   real    :: C_EV_S
   real    :: CLDVOL2FRC
   real    :: RHSUP_ICE
   real    :: SHR_EVAP_FAC
   real    :: MIN_CLD_WATER
   real    :: CLD_EVP_EFF
   integer :: NSMAX
   real    :: LS_SDQV2
   real    :: LS_SDQV3
   real    :: LS_SDQVT1
   real    :: ANV_SDQV2
   real    :: ANV_SDQV3
   real    :: ANV_SDQVT1
   real    :: ANV_TO_LS
   real    :: N_WARM
   real    :: N_ICE
   real    :: N_ANVIL
   real    :: N_PBL
   integer :: DISABLE_RAD
   real    :: ANV_ICEFALL_C
   real    :: LS_ICEFALL_C
   real    :: REVAP_OFF_P
   real    :: CNVENVFC
   real    :: WRHODEP
   real    :: T_ICE_ALL
   real    :: CNVICEPARAM
   integer :: ICEFRPWR
   real    :: CNVDDRFC
   real    :: ANVDDRFC
   real    :: LSDDRFC
   integer :: tanhrhcrit
   real    :: minrhcrit
   real    :: maxrhcrit
   real    :: turnrhcrit
   real    :: turnrhcrit_upper
   real    :: MIN_RI, MAX_RI, MIN_RL, MAX_RL, RI_ANV
   integer :: FR_LS_WAT, FR_LS_ICE, FR_AN_WAT, FR_AN_ICE
   real    :: maxrhcritland
   integer :: pdfflag
   real    :: sloperhcrit
   real :: min_lts
   real :: disp_factor_liq
   real :: disp_factor_ice
   real :: sclm_shw

   real, parameter :: T_ICE_MAX    = MAPL_TICE  ! -7.0+MAPL_TICE
   real, parameter :: RHO_W        = 1.0e3      ! Density of liquid water in kg/m^3
   real, parameter :: MIN_CLD_FRAC = 1.0e-8

   ! There are two PI's in this routine: PI_0 and MAPL_PI
   real, parameter :: PI_0 = 4.*atan(1.)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains 


   subroutine macro_cloud( &
!!! first vars are (in) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IRUN, LM         , &
         DT               , &
         PP_dev           , &
         PPE_dev          , &
         EXNP_dev         , &
         FRLAND_dev       , &
         KH_dev           , &
         RMFDTR_dev       , &
         QLWDTR_dev       , &              
         QRN_CU_dev       , &
         CNV_UPDFRC_dev   , &
         SC_RMFDTR_dev    , &
         SC_QLWDTR_dev    , &
         SC_QIWDTR_dev    , &
         QRN_SC_dev       , &
         QSN_SC_dev       , &
         SC_UPDFRC_dev    , &
         U_dev            , &
         V_dev            , & 
         TH_dev           , &
         Q_dev            , &
         QLW_LS_dev       , &
         QLW_AN_dev       , &
         QIW_LS_dev       , &
         QIW_AN_dev       , &
         ANVFRC_dev       , &
         CLDFRC_dev       , &        
         PRECU_dev        , &
         CUARF_dev        , &
         SNRCU_dev        , &
         CLDPARAMS        , &
         SCLMFDFR         , &
         QST3_dev         , &
         DZET_dev         , &
         QDDF3_dev        , &
         RHX_dev          , &
         REV_CN_dev       , &
         RSU_CN_dev       , &
         ACLL_CN_dev,ACIL_CN_dev, &
         PFL_CN_dev,PFI_CN_dev, &
         PDFL_dev,PDFI_dev,FIXL_dev,FIXI_dev, &         
         DCNVL_dev, DCNVi_dev,      &
         ALPHT_dev,   &
         CFPDF_dev,   &
         DQRL_dev,  &
         VFALLSN_CN_dev,  &
         VFALLRN_CN_dev,  &
                                !'=======two_moment=====
         CNV_FICE_dev,   &
         CNV_NDROP_dev, &
         CNV_NICE_dev, &
         SC_NDROP_dev, &
         SC_NICE_dev, &
         SCICE_dev,  & 
         NCPL_dev,  &
         NCPI_dev,  &
         PFRZ_dev,  &
         INC_NUC_dev,  &
         DNDCNV_dev,  &
         DNCCNV_dev,  &
         RAS_DT_dev,  &
         QRAIN_CN, & 
         QSNOW_CN, &
         KCBL, LTS_  )       

      type (CLDPARAM_TYPE), intent(in)          :: CLDPARAMS

      integer, intent(in   )                    :: IRUN ! IM*JM
      integer, intent(in   )                    :: LM   ! LM
      real, intent(in   )                       :: DT   ! DT_MOIST
      real, intent(in   ), dimension(IRUN,  LM) :: PP_dev      ! PLO
      real, intent(in   ), dimension(IRUN,0:LM) :: PPE_dev     ! CNV_PLE
      real, intent(in   ), dimension(IRUN,  LM) :: EXNP_dev    ! PK
      real, intent(in   ), dimension(IRUN     ) :: FRLAND_dev  ! FRLAND
      real, intent(in   ), dimension(IRUN,0:LM) :: KH_dev      ! KH
      real, intent(in   ), dimension(IRUN,  LM) :: RMFDTR_dev  ! CNV_MFD
      real, intent(in   ), dimension(IRUN,  LM) :: QLWDTR_dev  ! CNV_DQLDT
      real, intent(inout), dimension(IRUN,  LM) :: QRN_CU_dev  ! CNV_PRC3 IS THIS INTENT IN?
      real, intent(inout), dimension(IRUN,  LM) :: CNV_UPDFRC_dev ! CNV_UPDF
      real, intent(in   ), dimension(IRUN,  LM) :: SC_RMFDTR_dev
      real, intent(in   ), dimension(IRUN,  LM) :: SC_QLWDTR_dev
      real, intent(in   ), dimension(IRUN,  LM) :: SC_QIWDTR_dev
      real, intent(inout), dimension(IRUN,  LM) :: QRN_SC_dev
      real, intent(inout), dimension(IRUN,  LM) :: QSN_SC_dev
      real, intent(inout), dimension(IRUN,  LM) :: SC_UPDFRC_dev
      real, intent(in   ), dimension(IRUN,  LM) :: U_dev  ! U1
      real, intent(in   ), dimension(IRUN,  LM) :: V_dev  ! V1
      real, intent(inout), dimension(IRUN,  LM) :: TH_dev ! TH1
      real, intent(inout), dimension(IRUN,  LM) :: Q_dev  ! Q1
      real, intent(inout), dimension(IRUN,  LM) :: QLW_LS_dev ! QLLS
      real, intent(inout), dimension(IRUN,  LM) :: QLW_AN_dev ! QLCN
      real, intent(inout), dimension(IRUN,  LM) :: QIW_LS_dev ! QILS
      real, intent(inout), dimension(IRUN,  LM) :: QIW_AN_dev ! QICN
      real, intent(inout), dimension(IRUN,  LM) :: ANVFRC_dev ! CLCN
      real, intent(inout), dimension(IRUN,  LM) :: CLDFRC_dev ! CLLS   
      real, intent(  out), dimension(IRUN     ) :: PRECU_dev ! CN_PRC2    
      real, intent(  out), dimension(IRUN     ) :: CUARF_dev ! CN_ARFX
      real, intent(  out), dimension(IRUN     ) :: SNRCU_dev ! CN_SNR
      real, intent(in   )                       :: SCLMFDFR   ! CLOUD_CTL%SCLMFDFR
      real, intent(in   ), dimension(IRUN,  LM) :: QST3_dev   ! QST3
      real, intent(in   ), dimension(IRUN,  LM) :: DZET_dev   ! DZET
      real, intent(in   ), dimension(IRUN,  LM) :: QDDF3_dev  ! QDDF3
      real, intent(  out), dimension(IRUN,  LM) :: RHX_dev    ! RHX    
      real, intent(  out), dimension(IRUN,  LM) :: REV_CN_dev ! REV_CN
      real, intent(  out), dimension(IRUN,  LM) :: RSU_CN_dev ! RSU_CN
      real, intent(  out), dimension(IRUN,  LM) :: ACLL_CN_dev ! ACLL_CN
      real, intent(  out), dimension(IRUN,  LM) :: ACIL_CN_dev ! ACIL_CN     
      real, intent(  out), dimension(IRUN,0:LM) :: PFL_CN_dev ! PFL_CN
      real, intent(  out), dimension(IRUN,0:LM) :: PFI_CN_dev ! PFI_CN 
      real, intent(  out), dimension(IRUN,  LM) :: PDFL_dev ! DlPDF
      real, intent(  out), dimension(IRUN,  LM) :: PDFI_dev ! DiPDF
      real, intent(  out), dimension(IRUN,  LM) :: FIXL_dev ! DlFIX
      real, intent(  out), dimension(IRUN,  LM) :: FIXI_dev ! DiFIX                      
      real, intent(  out), dimension(IRUN,  LM) :: DCNVL_dev ! DCNVL
      real, intent(  out), dimension(IRUN,  LM) :: DCNVi_dev ! DCNVi
      real, intent(  out), dimension(IRUN,  LM) :: ALPHT_dev ! ALPHT
      real, intent(  out), dimension(IRUN,  LM) :: CFPDF_dev ! CFPDF    
      real, intent(  out), dimension(IRUN,  LM) :: DQRL_dev ! DQRL    
      real, intent(  out), dimension(IRUN,  LM) :: VFALLSN_CN_dev ! VFALLSN_CN
      real, intent(  out), dimension(IRUN,  LM) :: VFALLRN_CN_dev ! VFALLRN_CN
      !=====two_moment
      real, intent(inout), dimension(IRUN,  LM) ::  CNV_FICE_dev
      real, intent(inout), dimension(IRUN,  LM) :: CNV_NDROP_dev
      real, intent(inout), dimension(IRUN,  LM) :: CNV_NICE_dev
      real, intent(inout), dimension(IRUN,  LM) :: SC_NDROP_dev
      real, intent(inout), dimension(IRUN,  LM) :: SC_NICE_dev
      real, intent(in), dimension(IRUN,  LM) :: INC_NUC_dev
      real, intent(inout), dimension(IRUN,  LM) :: SCICE_dev 
      real, intent(inout), dimension(IRUN,  LM) :: NCPL_dev
      real, intent(inout), dimension(IRUN,  LM) :: NCPI_dev
      real, intent(out), dimension(IRUN,  LM) :: PFRZ_dev
      real, intent(out), dimension(IRUN,  LM) :: DNDCNV_dev
      real, intent(out), dimension(IRUN,  LM) :: DNCCNV_dev
      real, intent(out), dimension(IRUN,  LM) :: RAS_DT_dev
      real, intent(out), dimension(IRUN,  LM) :: QRAIN_CN
      real, intent(out), dimension(IRUN,  LM) :: QSNOW_CN

      
      real, intent(in   ), dimension(IRUN     )  :: LTS_   
      real, dimension(IRUN,  LM) :: FRZ_PP_dev ! FRZ_PP
      real :: TOT_UPDFRC
      integer, intent(in   ), dimension(IRUN     ) :: KCBL  ! RAS CLOUD BASE


      ! GPU The GPUs need to know how big local arrays are during compile-time
      !     as the GPUs cannot allocate memory themselves. This command resets
      !     this a priori size to LM for the CPU.


      integer :: I , J , K , L

      integer :: FRACTION_REMOVAL

      real :: MASS, iMASS
      real :: TOTFRC
      real :: QRN_CU_1D
      real :: QSN_CU
      real :: QRN_SUM
      real :: QSN_SUM
      real :: QRN_ALL, QSN_ALL
      real :: QTMP1, QTMP2, QTMP3, QTOT
      real :: TEMP
      real :: RHCRIT
      real :: AA3, BB3, ALPHA
      real :: VFALL, VFALLRN, VFALLSN
      real :: TOT_PREC_UPD   
      real :: AREA_UPD_PRC
      real :: AREA_UPD_PRC_tolayer
      real :: U_above,U_below
      real :: V_above,V_below
      real :: DZET_above,DZET_below
      real :: PRN_CU_above, PSN_CU_above
      real :: EVAP_DD_CU_above, SUBL_DD_CU_above

      real :: NIX, TOTAL_WATER, fQi, QRN_XS, QSN_XS, AUX

      logical :: use_autoconv_timescale

      CNV_BETA      = CLDPARAMS%CNV_BETA  ! Area factor for convective rain showers (non-dim)
      ANV_BETA      = CLDPARAMS%ANV_BETA  ! Area factor for anvil rain showers (non-dim)
      LS_BETA       = CLDPARAMS%LS_BETA   ! Area factor for Large Scale rain showers (non-dim)
      RH00          = CLDPARAMS%RH_CRIT   ! Critical relative humidity
      C_00          = CLDPARAMS%AUTOC_LS
      LWCRIT        = CLDPARAMS%QC_CRIT_LS
      C_ACC         = CLDPARAMS%ACCRETION
      C_EV_R        = CLDPARAMS%RAIN_REVAP_FAC
      C_EV_S        = CLDPARAMS%SNOW_REVAP_FAC
      CLDVOL2FRC    = CLDPARAMS%VOL_TO_FRAC
      RHSUP_ICE     = CLDPARAMS%SUPERSAT
      SHR_EVAP_FAC  = CLDPARAMS%SHEAR_EVAP_FAC
      MIN_CLD_WATER = CLDPARAMS%MIN_ALLOW_CCW
      CLD_EVP_EFF   = CLDPARAMS%CCW_EVAP_EFF
      NSMAX         = INT( CLDPARAMS%NSUB_AUTOCONV  )
      LS_SDQV2      = CLDPARAMS%LS_SUND_INTER
      LS_SDQV3      = CLDPARAMS%LS_SUND_COLD
      LS_SDQVT1     = CLDPARAMS%LS_SUND_TEMP1
      ANV_SDQV2     = CLDPARAMS%ANV_SUND_INTER
      ANV_SDQV3     = CLDPARAMS%ANV_SUND_COLD
      ANV_SDQVT1    = CLDPARAMS%ANV_SUND_TEMP1
      ANV_TO_LS     = CLDPARAMS%ANV_TO_LS_TIME
      N_WARM        = CLDPARAMS%NCCN_WARM
      N_ICE         = CLDPARAMS%NCCN_ICE
      N_ANVIL       = CLDPARAMS%NCCN_ANVIL
      N_PBL         = CLDPARAMS%NCCN_PBL
      DISABLE_RAD   = INT( CLDPARAMS%DISABLE_RAD )
      ANV_ICEFALL_C = CLDPARAMS%ANV_ICEFALL
      LS_ICEFALL_C  = CLDPARAMS%LS_ICEFALL
      REVAP_OFF_P   = CLDPARAMS%REVAP_OFF_P
      CNVENVFC      = CLDPARAMS%CNV_ENVF
      WRHODEP       = CLDPARAMS%WRHODEP
      T_ICE_ALL     = 240.0
      CNVICEPARAM   = CLDPARAMS%CNV_ICEPARAM
      ICEFRPWR      = INT( CLDPARAMS%CNV_ICEFRPWR + .001 )
      CNVDDRFC      = CLDPARAMS%CNV_DDRF
      ANVDDRFC      = CLDPARAMS%ANV_DDRF
      LSDDRFC       = CLDPARAMS%LS_DDRF
      TANHRHCRIT    = INT( CLDPARAMS%TANHRHCRIT )
      MINRHCRIT     = CLDPARAMS%MINRHCRIT
      MAXRHCRIT     = CLDPARAMS%MAXRHCRIT
      TURNRHCRIT    = CLDPARAMS%TURNRHCRIT
      MAXRHCRITLAND = CLDPARAMS%MAXRHCRITLAND
      FR_LS_WAT     = INT( CLDPARAMS%FR_LS_WAT )
      FR_LS_ICE     = INT( CLDPARAMS%FR_LS_ICE )
      FR_AN_WAT     = INT( CLDPARAMS%FR_AN_WAT )
      FR_AN_ICE     = INT( CLDPARAMS%FR_AN_ICE )
      MIN_RL        = CLDPARAMS%MIN_RL
      MIN_RI        = CLDPARAMS%MIN_RI
      MAX_RL        = CLDPARAMS%MAX_RL
      MAX_RI        = CLDPARAMS%MAX_RI
      PDFFLAG       = INT(CLDPARAMS%PDFSHAPE)
      DISP_FACTOR_LIQ   = CLDPARAMS%DISP_FACTOR_LIQ
      DISP_FACTOR_ICE   = CLDPARAMS%DISP_FACTOR_ICE
      sclm_shw =  CLDPARAMS%SCLM_SHW
      
      turnrhcrit_upper = CLDPARAMS%TURNRHCRIT_UP
        sloperhcrit= CLDPARAMS%SLOPERHCRIT
       min_lts = CLDPARAMS%MIN_LTS
      use_autoconv_timescale = .false.
      QRN_XS = 0.0
      QSN_XS = 0.0

      RUN_LOOP: DO I = 1, IRUN

         K_LOOP: DO K = 1, LM         

            if (K == 1) then
               TOT_PREC_UPD = 0.
               AREA_UPD_PRC = 0.
            end if

            if (K == LM ) then
               !! ZERO DIAGNOSTIC OUTPUTS BEFORE SHOWERS !!
               PRECU_dev(I) = 0.
               SNRCU_dev(I) = 0. 
               CUARF_dev(I) = 0.
            end if

            !Zero out/initialize precips, except QRN_CU which comes from RAS        
            QRN_CU_1D = 0.
            QSN_CU    = 0.
            VFALL     = 0.

            PFL_CN_dev(I,K) = 0.
            PFI_CN_dev(I,K) = 0.

            IF (K == 1) THEN
               PFL_CN_dev(I,0) = 0.
               PFI_CN_dev(I,0) = 0.
            END IF

            ! Initialize other diagnostics 

            RHX_dev(I,K) = MAPL_UNDEF
            REV_CN_dev(I,K) = MAPL_UNDEF
            RSU_CN_dev(I,K) = MAPL_UNDEF
            ACLL_CN_dev(I,K) = MAPL_UNDEF
            ACIL_CN_dev(I,K) = MAPL_UNDEF
            PDFL_dev(I,K) = MAPL_UNDEF
            PDFI_dev(I,K) = MAPL_UNDEF
            FIXL_dev(I,K) = MAPL_UNDEF
            FIXI_dev(I,K) = MAPL_UNDEF
            DCNVL_dev(I,K) = MAPL_UNDEF
            DCNVi_dev(I,K) = MAPL_UNDEF
            ALPHT_dev(I,K) = MAPL_UNDEF
            CFPDF_dev(I,K) = MAPL_UNDEF
            DQRL_dev(I,K) = MAPL_UNDEF
            VFALLSN_CN_dev(I,K) = MAPL_UNDEF
            VFALLRN_CN_dev(I,K) = MAPL_UNDEF
            !====two-moment

            DNDCNV_dev(I, K) = MAPL_UNDEF
            DNCCNV_dev(I, K) =  MAPL_UNDEF
            RAS_DT_dev(I, K) =  MAPL_UNDEF
            QRAIN_CN(I,K) = 0.0
            QSNOW_CN(I,K) = 0.0  
            NIX= 0.0

            ! Copy QRN_CU into a temp scalar
            QRN_CU_1D = QRN_CU_dev(I,K)

            MASS =  ( PPE_dev(I,K) - PPE_dev(I,K-1) )*100./MAPL_GRAV  ! layer-mass (kg/m**2)
            iMASS = 1.0 / MASS
            TEMP =  EXNP_dev(I,K) * TH_dev(I,K) 
            FRZ_PP_dev(I,K) = 0.00
            !PFRZ_dev(I, K) = 0.0

!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ! Condensate Source
!!!!!!!!!!!!!!!!!!!!!!!!!!!

  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


            TOTAL_WATER =  (QIW_AN_dev(I,K)+QLW_AN_dev(I,K) + QIW_LS_dev(I,K)+ QIW_LS_dev(I,K))*MASS +QLWDTR_dev(I,K)*DT+SC_QLWDTR_dev(I,K)*DT+SC_QIWDTR_dev(I,K)*DT

            DCNVi_dev(I,K) = QIW_AN_dev(I,K)
            DCNVL_dev(I,K) = QLW_AN_dev(I,K)
            DNDCNV_dev(I, K) = NCPL_dev(I, K)
            DNCCNV_dev(I, K) = NCPI_dev(I, K)


            if (FRLAND_dev(I) < 0.1) then 

              AUX  = SCLMFDFR
              else
              AUX=1.0
              end if
              

            CALL cnvsrc (DT             , &
                  CNVICEPARAM    , &
                  AUX       , &
                  MASS           , & 
                  iMASS          , &
                  PP_dev(I,K)    , &
                  TEMP           , &
                  Q_dev(I,K)     , &
                  QLWDTR_dev(I,K), &
                  RMFDTR_dev(I,K), &
                  SC_QLWDTR_dev(I,K), &
                  SC_QIWDTR_dev(I,K), &
                  SC_RMFDTR_dev(I,K), &
                  QLW_AN_dev(I,K), &
                  QIW_AN_dev(I,K), &
                  CLDFRC_dev(I,K), & 
                  ANVFRC_dev(I,K), &
                  QST3_dev(I,K) ,   & 
                                !====2Moment
                  NCPL_dev(I, K), &
                  NCPI_dev(I, K), &
                  CNV_FICE_dev(I, K), &
                  CNV_NDROP_dev(I, K), &
                  CNV_NICE_dev(I, K), &
                  SC_NDROP_dev(I,K), &  
                  SC_NICE_dev(I,K) )


            DCNVi_dev(I,K) = ( QIW_AN_dev(I,K) - DCNVi_dev(I,K) ) / DT
            DCNVL_dev(I,K) = ( QLW_AN_dev(I,K) - DCNVL_dev(I,K) ) / DT
            DNDCNV_dev(I, K) = (NCPL_dev(I, K)-DNDCNV_dev(I, K))/DT
            DNCCNV_dev(I, K) = (NCPI_dev(I, K)-DNCCNV_dev(I, K))/DT




!!!!!!!!!!!!!!!!!!!!check consistency!!!!!!!!!!!!!!!!!!!!!!!!!!!

            FIXL_dev(I,K) = QLW_AN_dev(I,K) + QLW_LS_dev(I,K)
            FIXI_dev(I,K) = QIW_AN_dev(I,K) + QIW_LS_dev(I,K)


               CALL fix_up_clouds(    &
                  Q_dev(I,K)     , &
                  TEMP           , &
                  QLW_LS_dev(I,K), & 
                  QIW_LS_dev(I,K), & 
                  CLDFRC_dev(I,K), &   
                  QLW_AN_dev(I,K), & 
                  QIW_AN_dev(I,K), & 
                  ANVFRC_dev(I,K))
                  

            FIXL_dev(I,K) = -( QLW_AN_dev(I,K) + QLW_LS_dev(I,K) - FIXL_dev(I,K) ) / DT 
            FIXI_dev(I,K) = -( QIW_AN_dev(I,K) + QIW_LS_dev(I,K) - FIXI_dev(I,K) ) / DT 


!!!!!!!!!!!Spread!!!!!!!!!!!!!!

            if (k == 1 .or. k == lm) then
               U_above = 0.0
               U_below = 0.0
               V_above = 0.0
               V_below = 0.0
               DZET_above = 0.0
               DZET_below = 0.0
            else
               U_above = U_dev(i,k-1)
               U_below = U_dev(i,k+1)
               V_above = V_dev(i,k-1)
               V_below = V_dev(i,k+1)
               DZET_above = DZET_dev(i,k-1)
               DZET_below = DZET_dev(i,k+1)
            end if

            ! assume deep and shallow updraft fractions non-overlapping
            TOT_UPDFRC = CNV_UPDFRC_dev(I,K) + SC_UPDFRC_dev(I,K)
            TOT_UPDFRC = MAX( MIN( TOT_UPDFRC, 1.), 0.)

            call pdf_spread (&
                  K,LM,&
                  U_dev(I,K),U_above,U_below,&
                  V_dev(I,K),V_above,V_below,&
                  KH_dev(I,K-1),DZET_above,DZET_below,&
                  TOT_UPDFRC,PP_dev(I,K),ALPHA,&
                  ALPHT_dev(I,K), & 
                  FRLAND_dev(I), LTS_(I))  

            ! impose a minimum amount of variability
            ALPHA    = MAX(  ALPHA , 1.0 - RH00 )

            RHCRIT = 1.0 - ALPHA


            !=================================new condensate ====================================
!!!!!!!!!Calculate probability of freezing to scale nucleated ice crystals !!
            !================================   


            call Pfreezing (  &       
                  ALPHA    , &
                  PP_dev(I,K)              , &
                  TEMP                  , &
                  Q_dev(I,K)                , &
                  QLW_LS_dev(I,K), &
                  QLW_AN_dev(I,K), &
                  QIW_LS_dev(I,K), &
                  QIW_AN_dev(I,K), &    
                  SCICE_dev(I, K)      , &
                  CLDFRC_dev(I,K), & 
                  ANVFRC_dev(I,K), &
                  PFRZ_dev(I, K) )   




            !=================================new condensate ====================================
!!!!!!!!!Create large scale condensate and cloud fraction
            !================================   
            
         call hystpdf( &
            DT          , &
            ALPHA       , &
            pdfflag , &
            PP_dev(I,K)          , &
             Q_dev(I,K)                , &
             QLW_LS_dev(I,K), &
             QLW_AN_dev(I,K), &
             QIW_LS_dev(I,K), &
             QIW_AN_dev(I,K), &    
             TEMP          , &
             CLDFRC_dev(I,K), & 
             ANVFRC_dev(I,K), &
             SCICE_dev(I, K)   ,  &
             NCPL_dev(I, K), &
             NCPI_dev(I, K) )
            


            !=============Collect convective precip==============


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !  Add in convective rain 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            ! CU-FREEZE 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ! Also "freeze" out any conv. precip that needs
            ! to be since this isn't done in RAS. This is
            ! precip w/ large particles, so freezing is 
            ! strict. Check up on this!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            QTMP1 = 0.
            QTMP2 = 0.
            QTMP3 = 0.
            QRN_ALL = 0.
            QSN_ALL = 0.


            if ( TEMP < MAPL_TICE ) then	    
               QTMP2     = QRN_CU_1D
               QSN_CU    = QRN_CU_1D
               QRN_CU_1D = 0.
               TEMP      = TEMP + QSN_CU*(MAPL_ALHS-MAPL_ALHL) / MAPL_CP
            end if

            QRN_CU_1D = QRN_CU_1D + QRN_SC_dev(I,K)*DT !+ QRN_XS!! add any excess precip to convective 
            QSN_CU    = QSN_CU    + QSN_SC_dev(I,K)*DT !+ QSN_XS

            !        
!	    fQi=  ICE_FRACTION(TEMP)	    
!            QSN_CU     = QRN_CU_1D*fQi
!	    QRN_CU_1D = max(QRN_CU_1D -QSN_CU, 0.0)
!            TEMP      = TEMP + QSN_CU*(MAPL_ALHS-MAPL_ALHL) / MAPL_CP




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            !----------------------------------------------------------------------------------------------
            ! Column will now be swept from top-down for precip accumulation/accretion/re-evaporation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


            AREA_UPD_PRC_tolayer = 0.0


            TOT_PREC_UPD  = TOT_PREC_UPD + ( ( QRN_CU_1D + QSN_CU ) * MASS )
            AREA_UPD_PRC  = AREA_UPD_PRC + ( TOT_UPDFRC* ( QRN_CU_1D + QSN_CU )* MASS )

            if ( TOT_PREC_UPD > 0.0 ) AREA_UPD_PRC_tolayer = MAX( AREA_UPD_PRC/TOT_PREC_UPD, 1.E-6 )

            AREA_UPD_PRC_tolayer = CNV_BETA * AREA_UPD_PRC_tolayer

            IF (K == LM) THEN ! We've accumulated over the whole column

               if ( TOT_PREC_UPD > 0.0 ) AREA_UPD_PRC = MAX( AREA_UPD_PRC/TOT_PREC_UPD, 1.E-6 )

               AREA_UPD_PRC = CNV_BETA * AREA_UPD_PRC

               !! "couple" to diagnostic areal fraction output 
               !! Intensity factor in PRECIP3 is floored at
               !! 1.0. So this is fair.

               CUARF_dev(I) = MIN( AREA_UPD_PRC, 1.0 )

            END IF


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ! GET SOME MICROPHYSICAL QUANTITIES 

            CALL MICRO_AA_BB_3( TEMP,PP_dev(I,K),QST3_dev(I,K),AA3,BB3 )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            QTMP1 = QLW_LS_dev(I,K) + QLW_AN_dev(I,K)
            QTMP2 = QIW_LS_dev(I,K) + QIW_AN_dev(I,K)
            QTOT=QTMP1+QTMP2

            ! QTMP1 = 0.0
            ! QTMP2 = 0.0


            ! Convective
            ! ----------
            !RHCRIT=1.0

            call  PRECIP3(          &
                  K,LM            , &
                  DT              , &
                  FRLAND_dev(I)   , &
                  RHCRIT          , &
                  QRN_CU_1D       , &
                  QSN_CU          , &
                  QTMP1           , &
                  QTMP2           , &
                  TEMP            , &
                  Q_dev(I,K)      , &
                  mass            , &
                  imass           , &
                  PP_dev(I,K)     , &
                  DZET_dev(I,K)   , &
                  QDDF3_dev(I,K)  , &
                  AA3             , &
                  BB3             , &
                  AREA_UPD_PRC_tolayer    , &
                  PRECU_dev(I)    , & 
                  SNRCU_dev(I)    , & 
                  PRN_CU_above    , &
                  PSN_CU_above    , &
                  EVAP_DD_CU_above, &
                  SUBL_DD_CU_above, &
                  REV_CN_dev(I,K) , &
                  RSU_CN_dev(I,K) , &
                  ACLL_CN_dev(I,K), &
                  ACIL_CN_dev(I,K), &
                  PFL_CN_dev(I,K) , &
                  PFI_CN_dev(I,K) , &
                  VFALLRN         , &
                  VFALLSN         , &
                  FRZ_PP_dev(I,K) , &
                  CNVENVFC, CNVDDRFC, &
                  ANVFRC_dev(I,k), CLDFRC_dev(I,k),  &
                  PP_dev(I,KCBL(I)))

            VFALLSN_CN_dev(I,K) = VFALLSN
            VFALLRN_CN_dev(I,K) = VFALLRN

            if (.not.use_autoconv_timescale) then
               if (VFALLSN.NE.0.) then
                  QSN_ALL = QSN_ALL + PFI_CN_dev(I,K)/VFALLSN
               end if
               if (VFALLRN.NE.0.) then
                  QRN_ALL = QRN_ALL + PFL_CN_dev(I,K)/VFALLRN
               end if
            end if

            if (.true.) then 

               IF ( (QLW_LS_dev(I,K)+QLW_AN_dev(I,K)) > 1.e-20 ) THEN
                  QTMP3 = 1./(QLW_LS_dev(I,K)+QLW_AN_dev(I,K))
               ELSE
                  QTMP3 = 0.0
               END IF
               QLW_LS_dev(I,K) = QLW_LS_dev(I,K) * QTMP1 * QTMP3
               QLW_AN_dev(I,K) = QLW_AN_dev(I,K) * QTMP1 * QTMP3
               NCPL_dev(I, K) = NCPL_dev(I, K)* QTMP1 * QTMP3

               IF ( (QIW_LS_dev(I,K)+QIW_AN_dev(I,K)) > 1.0e-20 ) THEN
                  QTMP3 = 1./(QIW_LS_dev(I,K)+QIW_AN_dev(I,K))
               ELSE
                  QTMP3 = 0.0
               END IF
               QIW_LS_dev(I,K) = QIW_LS_dev(I,K) * QTMP2 * QTMP3
               QIW_AN_dev(I,K) = QIW_AN_dev(I,K) * QTMP2 * QTMP3
               NCPI_dev(I, K) = NCPI_dev(I, K)* QTMP2 * QTMP3

               ! reduce cloud farction as well
               QTMP3 = QIW_LS_dev(I,K)+QIW_AN_dev(I,K) + QLW_LS_dev(I,K)+QLW_AN_dev(I,K)

               If (QTOT .gt. 0.0) then 
                  CLDFRC_dev(I,k) = CLDFRC_dev(I,k)*QTMP3/QTOT          
                  ANVFRC_dev(I,k) = ANVFRC_dev(I,k)*QTMP3/QTOT     
               end if

            end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            QRAIN_CN(I,K) = QRN_ALL / (100.*PP_dev(I,K) / (MAPL_RGAS*TEMP ))
            QSNOW_CN(I,K) = QSN_ALL / (100.*PP_dev(I,K) / (MAPL_RGAS*TEMP ))
            QRN_CU_dev(I,K) = QRN_CU_1D


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            TOTFRC = CLDFRC_dev(I,K) + ANVFRC_dev(I,K)

            IF ( TOTFRC > 1.00 ) THEN
               CLDFRC_dev(I,k) = CLDFRC_dev(I,k)*(1.00 / TOTFRC )
               ANVFRC_dev(I,k) = ANVFRC_dev(I,k)*(1.00 / TOTFRC )
            END IF

            TOTFRC = CLDFRC_dev(I,K) + ANVFRC_dev(I,K)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



            TH_dev(I,K)  =  TEMP / EXNP_dev(I,K) 

         end do K_LOOP


      end do RUN_LOOP

   END SUBROUTINE MACRO_CLOUD


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!              P R O C E S S   S U B R O U T I N E S                 !!
   !!                         * * * * *                                  !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!              P R O C E S S   S U B R O U T I N E S                 !!
   !!                         * * * * *                                  !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
   !!                                                                    !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!              P R O C E S S   S U B R O U T I N E S                 !!
   !!                         * * * * *                                  !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine pdf_spread (K,LM,&
         U,U_above,U_below,&
         V,V_above,V_below,&
         KH,&
         DZ_above,DZ_below,&
         UPDF,PP,ALPHA,&
         ALPHT_DIAG, &
         FRLAND, LTSX )  

      integer, intent(in)  :: k,lm
      real,    intent(in)  :: U,U_above,U_below
      real,    intent(in)  :: V,V_above,V_below
      real,    intent(in)  :: DZ_above,DZ_below
      real,    intent(in)  :: UPDF,PP
      real,    intent(in)  :: KH
      real,    intent(out) :: ALPHA
      real,    intent(out) ::  ALPHT_DIAG
      real,    intent(in)  :: FRLAND, LTSX

      real    :: A1,A2,A3
      real    :: tempmaxrh, slope, slope_up, turnrhcrit_up, aux1, aux2, maxalpha

      !slope = 20.0
      
      slope_up = 20.0
     
    
      !turnrhcrit_up = 300.0

      turnrhcrit_up = turnrhcrit_upper
      slope =  sloperhcrit

      !turnrhcrit = 884.0 !turnrhcrit is slightly modified to match the old tan-1 function DONIF
      maxalpha=1.0-minrhcrit


      ! alpha is the 1/2*width so RH_crit=1.0-alpha

      !  Use Slingo-Ritter (1985) formulation for critical relative humidity
      !  array a1 holds the critical rh, ranges from 0.8 to 1
      !Reformulated by Donifan Barahona

      aux1 = min(max((pp- turnrhcrit)/slope, -20.0), 20.0) 
      aux2 = min(max((turnrhcrit_up - pp)/slope_up, -20.0), 20.0)


      !if (frland > 0.05)  then           
       !  aux1=1.0
         !maxalpha=max(maxalpha-0.05, 0.001)
      !else
         aux1 = 1.0/(1.0+exp(aux1)) !this function reproduces the old Sligo function. 
         !aux2=min(max(2.0*(ltsx-min_lts), -20.0), 20.0)
         !aux2=0.5/(1.0+exp(aux2))
         !aux1=max(aux2, aux1)
          
      !end if

      !aux2= 1.0/(1.0+exp(aux2)) !this function reverses the profile at low P    
      aux2=1.0

      alpha  = maxalpha*aux1*aux2



      ALPHA = MIN( ALPHA , 0.4 )  ! restrict RHcrit to > 60% 
      ALPHT_DIAG = ALPHA

   end subroutine pdf_spread

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine fix_up_clouds_2M( &
         QV, &
         TE, &
         QLC,&
         QIC,&
         CF, &
         QLA,&
         QIA,&
         AF, &
         NL, &
         NI, & 
         QRN_SX, &
         QSN_SX  )

      real, intent(inout) :: TE,QV,QLC,CF,QLA,AF,QIC,QIA, NL, NI
      real, intent(out) :: QSN_SX, QRN_SX
      real, parameter  :: qmin  = 1.0e-11
      real, parameter :: cfmin  = 1.0e-4
      real, parameter :: nmin  = 100.0
      real, parameter :: RL_cub  = 1.0e-15
      real, parameter :: RI_cub  = 1.0e-12
      real, parameter :: nlmax  = 500.0e6
      real, parameter :: nimax  = 1.0e5


      QRN_SX =  0.0
      QSN_SX = 0.0
      ! Fix if Anvil cloud fraction too small
      if (AF < cfmin) then
         QV  = QV + QLA + QIA
         TE  = TE - (MAPL_ALHL/MAPL_CP)*QLA - (MAPL_ALHS/MAPL_CP)*QIA
         AF  = 0.
         QLA = 0.
         QIA = 0.
      end if

      ! Fix if LS cloud fraction too small
      if ( CF < cfmin) then
         QV = QV + QLC + QIC
         TE = TE - (MAPL_ALHL/MAPL_CP)*QLC - (MAPL_ALHS/MAPL_CP)*QIC
         CF  = 0.
         QLC = 0.
         QIC = 0.
      end if

      ! LS LIQUID too small
      if ( QLC  < qmin ) then
         QV = QV + QLC 
         TE = TE - (MAPL_ALHL/MAPL_CP)*QLC
         QLC = 0.
      end if
      ! LS ICE too small
      if ( QIC  < qmin) then
         QV = QV + QIC 
         TE = TE - (MAPL_ALHS/MAPL_CP)*QIC
         QIC = 0.
      end if

      ! Anvil LIQUID too small
      if ( QLA  < qmin ) then
         QV = QV + QLA 
         TE = TE - (MAPL_ALHL/MAPL_CP)*QLA
         QLA = 0.
      end if
      ! Anvil ICE too small
      if ( QIA  < qmin) then
         QV = QV + QIA 
         TE = TE - (MAPL_ALHS/MAPL_CP)*QIA
         QIA = 0.
      end if

      ! Fix ALL cloud quants if Anvil cloud LIQUID+ICE too small
      if ( ( QLA + QIA ) < qmin) then
         QV = QV + QLA + QIA
         TE = TE - (MAPL_ALHL/MAPL_CP)*QLA - (MAPL_ALHS/MAPL_CP)*QIA
         AF  = 0.
         QLA = 0.
         QIA = 0.
      end if
      ! Ditto if LS cloud LIQUID+ICE too small
      if ( ( QLC + QIC ) < qmin ) then
         QV = QV + QLC + QIC
         TE = TE - (MAPL_ALHL/MAPL_CP)*QLC - (MAPL_ALHS/MAPL_CP)*QIC
         CF  = 0.
         QLC = 0.
         QIC = 0.
      end if

      if ((QLA+QLC) .le. qmin) then 
         NL = 0.0
      end if

      if ((QIA+QIC) .le. qmin) then 
         NI = 0.0
      end if

    !  ! make sure N > 0 if Q >0!

!      if (((QLA+QLC) .gt. qmin) .and. (NL .le. nmin)) then    
!         NL =  max((QLA+QLC)/( 1.333 * MAPL_PI *RL_cub*997.0), nmin)             
!         NL=min(NL, nlmax)
!      end if
!
 !     if (((QIA+QIC) .gt. qmin) .and. (NI .le. nmin)) then    
 !        NI =  max((QIA+QIC)/( 1.333 * MAPL_PI *RI_cub*500.0), nmin)             
 !        NI=min(NI, nimax)
 !     end if


    ! if Q > 0 and N ~0 this is probably falling (also helps eliminating ice if using initial conditons from single moment).
   
            if (((QLA+QLC) .gt. qmin) .and. (NL .le. nmin)) then    
                 NL = 0.0
                 QRN_SX =  QLA+QLC
                 QLA = 0.0
                 QLC = 0.0
              
              end if    

              if (((QIA+QIC) .gt. qmin) .and. (NI .le. nmin)) then    
                 NI = 0.0
                 QSN_SX = QIA+QIC
                 QIA = 0.0
                 QIC = 0.0                 
              end if
   

   end subroutine fix_up_clouds_2M

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 subroutine fix_up_clouds( &
         QV, &
         TE, &
         QLC,&
         QIC,&
         CF, &
         QLA,&
         QIA,&
         AF  )

      real, intent(inout) :: TE,QV,QLC,CF,QLA,AF,QIC,QIA

      ! Fix if Anvil cloud fraction too small
      if (AF < 1.E-5) then
         QV  = QV + QLA + QIA
         TE  = TE - (MAPL_ALHL/MAPL_CP)*QLA - (MAPL_ALHS/MAPL_CP)*QIA
         AF  = 0.
         QLA = 0.
         QIA = 0.
      end if

      ! Fix if LS cloud fraction too small
      if ( CF < 1.E-5 ) then
         QV = QV + QLC + QIC
         TE = TE - (MAPL_ALHL/MAPL_CP)*QLC - (MAPL_ALHS/MAPL_CP)*QIC
         CF  = 0.
         QLC = 0.
         QIC = 0.
      end if
      
      ! LS LIQUID too small
      if ( QLC  < 1.E-8 ) then
         QV = QV + QLC 
         TE = TE - (MAPL_ALHL/MAPL_CP)*QLC
         QLC = 0.
      end if
      ! LS ICE too small
      if ( QIC  < 1.E-8 ) then
         QV = QV + QIC 
         TE = TE - (MAPL_ALHS/MAPL_CP)*QIC
         QIC = 0.
      end if

      ! Anvil LIQUID too small
      if ( QLA  < 1.E-8 ) then
         QV = QV + QLA 
         TE = TE - (MAPL_ALHL/MAPL_CP)*QLA
         QLA = 0.
      end if
      ! Anvil ICE too small
      if ( QIA  < 1.E-8 ) then
         QV = QV + QIA 
         TE = TE - (MAPL_ALHS/MAPL_CP)*QIA
         QIA = 0.
      end if

      ! Fix ALL cloud quants if Anvil cloud LIQUID+ICE too small
      if ( ( QLA + QIA ) < 1.E-8 ) then
         QV = QV + QLA + QIA
         TE = TE - (MAPL_ALHL/MAPL_CP)*QLA - (MAPL_ALHS/MAPL_CP)*QIA
         AF  = 0.
         QLA = 0.
         QIA = 0.
      end if
      ! Ditto if LS cloud LIQUID+ICE too small
      if ( ( QLC + QIC ) < 1.E-8 ) then
         QV = QV + QLC + QIC
         TE = TE - (MAPL_ALHL/MAPL_CP)*QLC - (MAPL_ALHS/MAPL_CP)*QIC
         CF  = 0.
         QLC = 0.
         QIC = 0.
      end if

   end subroutine fix_up_clouds

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine update_cld( &
         DT          , &
         ALPHA       , &
         PDFSHAPE    , &
         PL          , &
         QV          , &
         QCl         , &
         QAl         , &
         QCi         , &
         QAi         , &
         TE          , &
         CF          , &
         AF          ,  &
         SCICE   ,  &
         NI            , &
         NL           , &
         NCnuc   , &
         RHcmicro)

      real, intent(in)    :: DT,ALPHA,PL,  NCnuc
      integer, intent(in) :: pdfshape
      real, intent(inout) :: TE,QV,QCl,QCi,CF,QAl,QAi,AF, NI, RHCmicro, NL,  SCICE

      ! internal arrays
      real :: QCO, QVO, CFO, QAO, TAU, SCICE_x
      real :: QT, QMX, QMN, DQ, QVtop, sigmaqt1, sigmaqt2, qsnx

      real :: TEO,QSx,DQsx,QS,DQs

      real :: TEp, QSp, CFp, QVp, QCp
      real :: TEn, QSn, CFn, QVn, QCn

      real :: QCx, QVx, CFx, QAx, QC, QA, fQi, fQi_A
      real :: dQAi, dQAl, dQCi, dQCl 

      real :: QX, QSLIQ, QSICE, CFALL, DQx, FQA, DELQ

      real :: tmpARR
      real :: ALHX, DQCALL, SHOM, maxalpha
      ! internal scalars
      integer :: N


      pdfflag = PDFSHAPE
      maxalpha=1.0-minrhcrit 

      QC = QCl + QCi
      QA = QAl + QAi
      QT  =  QC  + QA + QV  !Total water after microphysics
      CFALL  = AF+CF
      FQA = 0.0
      if (QA+QC .gt. tiny(1.0))  FQA=QA/(QA+QC)

      SHOM=2.349-(TE/259.0) !hom threeshold Si according to Ren & McKenzie, 2005

      !================================================
      ! First find the cloud fraction that would correspond to the current condensate 
      QSLIQ  = QSATLQ(         &
            TE   , &
            PL*100.0 , DQ=DQx )


      QSICE  = QSATIC(         &
            TE   , &
            PL*100.0 , DQ=DQx )

      if ((QC+QA) .gt. 1.0e-13) then 
         QSx=((QCl+QAl)*QSLIQ + QSICE*(QCi+QAi))/(QC+QA)               
      else
         DQSx  = DQSAT(         &
               TE   , &
               PL ,  35.0, QSAT=QSx     ) !use ramp to -40 
      end if


      
      if (TE .gt. T_ICE_ALL)   SCICE = 1.0
      QCx=QC+QA   
      QX=QT-QSx*SCICE
      CFo=0.
      
      !====== recalculate QX if too low and SCICE<SHOM
        if ((QX .gt. QCx) .and. (QCx .gt. 0.0)) then     
           QX=QT-QSx*SHOM
       end if 
      
      !=======================

     DELQ=max(min(2.0*maxalpha*QSx, 0.5*QT), 1.0e-12)   
      
      if  ((QX .le. QCx)  .and. (QCx .gt. tiny(1.0)))  then          
         CFo =  (1.0+SQRT(1.0-(QX/QCx)))
         if (CFo .gt. 1.e-6) then
            CFo = min(1.0/CFo, 1.0)
            DELQ=  2.0*QCx/(CFo*CFo)
         else
            CFo = 0.0
         end if
      elseif (QCx .gt. tiny(1.0)) then  
         !   CFo = 1.0  !Outside of distribution but still with condensate         
        DELQ=max(min(2.0*maxalpha*QSx, 0.5*QT), 1.0e-12)          
        CFo = SQRT(2.0*QCx/DELQ)        
      else
        CFo = 0.0         
      end if

      if  (QSx .gt. tiny(1.0)) then 
         RHCmicro = SCICE - 0.5*DELQ/Qsx
      else
         RHCmicro = 0.0
      end if

      CFALL   = max(CFo, 0.0) 
      CFALL   = min(CFo, 1.0) 

      CF=CFALL*(1.0-FQA)
      AF=CFALL*FQA

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      return 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    

 !     if ((TE .le. T_ICE_ALL))  return !don't do anything else for cirrus


      !================================================
      !================================================
      ! Now find the equilibirum cloud fraction for mix-phase and liquid clouds. Create new condensate if neccesary

       call hystpdf( &
             DT          , &
            ALPHA       , &
            PDFSHAPE    , &
            PL          , &
            QV          , &
            QCl         , &
            QAl         , &
            QCi         , &
            QAi         , &
            TE          , &
            CF          , &
            AF          ,  &
            SCICE   ,  &
            NI            , &
           NL           )


    CALL fix_up_clouds(    &
                  QV     , &
                  TE           , &
                  QCl, & 
                  QCi, & 
                  CF, &   
                  QAl, & 
                  QAi, & 
                  AF)



   end subroutine update_cld

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine hystpdf( &
         DT          , &
         ALPHA       , &
         PDFSHAPE    , &
         PL          , &
         QV          , &
         QCl         , &
         QAl         , &
         QCi         , &
         QAi         , &
         TE          , &
         CF          , &
         AF          ,  &
         SCICE   ,  &
         NI            , &
         NL           )

      real, intent(in)    :: DT,ALPHA,PL
      integer, intent(in) :: pdfshape
      real, intent(inout) :: TE,QV,QCl,QCi,CF,QAl,QAi,AF, NI, NL,  SCICE

      ! internal arrays
      real :: QCO, QVO, CFO, QAO, TAU, SCICE_x
      real :: QT, QMX, QMN, DQ, QVtop, sigmaqt1, sigmaqt2, qsnx

      real :: TEO,QSx,DQsx,QS,DQs

      real :: TEp, QSp, CFp, QVp, QCp
      real :: TEn, QSn, CFn, QVn, QCn

      real :: QCx, QVx, CFx, QAx, QC, QA, fQi, fQi_A
      real :: dQAi, dQAl, dQCi, dQCl 

      real :: QX, QSLIQ, QSICE, CFALL, DQx, FQA, DQSI, DQSL, LHcorr, fQip

      real :: tmpARR
      real :: ALHX, DQCALL
      ! internal scalars
      integer :: N, nmax


      pdfflag = PDFSHAPE

      QC = QCl + QCi
      QA = QAl + QAi
      QT  =  QC  + QA + QV  !Total water after microphysics
      CFALL  = AF+CF
      FQA = 0.0
      fQi = 0.0
      tmpARR = 0.0
      nmax =  20
      QAx = 0.0

      if (QA+QC .gt. 0.0)  FQA=QA/(QA+QC)
      if ( QA > 0.0 )  fQi_A = QAi / QA 
      if (QT .gt. 0.0) fQi = (QCI+QAI)/QT
      if (TE .lt. T_ICE_ALL)  fQi = 1.0
      if ( AF < 1.0 )  tmpARR = 1./(1.-AF)

      TEo = TE

      fQi = ice_fraction( TE )
      DQSx  = DQSAT( TE, PL, QSAT=QSx )
      CFx = CF*tmpARR
      QCx = QC*tmpARR
      QVx = ( QV - QSx*AF )*tmpARR

      if ( AF >= 1.0 )    QVx = QSx*1.e-4 
      if ( AF > 0. )  QAx = QA/AF

      QT  = QCx + QVx

      TEp = TEo
      QSn = QSx
      TEn = TEo
      CFn = CFx
      QVn = QVx
      QCn = QCx
      DQS = DQSx


      do n=1,nmax

         QVp = QVn
         QCp = QCn
         CFp = CFn
         TEp = TEn
         fQip= fQi


         if(pdfflag.lt.2) then

            sigmaqt1  = ALPHA*QSn
            sigmaqt2  = ALPHA*QSn

         elseif(pdfflag.eq.2) then
            ! for triangular, symmetric: sigmaqt1 = sigmaqt2 = alpha*qsn (alpha is half width)
            ! for triangular, skewed r : sigmaqt1 < sigmaqt2
            ! try: skewed right below 500 mb
!!!       if(pl.lt.500.) then
            sigmaqt1  = ALPHA*QSn
            sigmaqt2  = ALPHA*QSn
!!!       else
!!!       sigmaqt1  = 2*ALPHA*QSn*0.4
!!!       sigmaqt2  = 2*ALPHA*QSn*0.6
!!!       endif
         elseif(pdfflag .eq. 4) then !lognormal (sigma is dimmensionless)
            sigmaqt1 =  max(ALPHA/sqrt(3.0), 0.001)
         endif


         qsnx= qsn*SCICE !
         if ((QCI .ge. 0.0) .and. (qsn .gt. qt))  qsnx=qsn !this way we do not evaporate preexisting ice but maintain supersat

         call pdffrac(PDFSHAPE,qt,sigmaqt1,sigmaqt2,qsnx,CFn)
         call pdfcondensate(PDFSHAPE,qt,sigmaqt1,sigmaqt2,qsnx,QCn, CFn) 

         DQCALL = QCn - QCp
         CF  = CFn * ( 1.-AF)

         call Bergeron_iter    (  &         !Microphysically-based partitions the new condensate
               DT                 , &
               PL                 , &
               TEp               , &                  
               QT                , &
               QCi               , &
               QAi               , &
               QCl               , &
               QAl               , &     
               CF                , &
               AF                , &
               NL                , &
               NI                 , & 
               DQCALL        , &
               fQi                 )



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         ! These lines represent adjustments
         ! to anvil condensate due to the 
         ! assumption of a stationary TOTAL 
         ! water PDF subject to a varying 
         ! QSAT value during the iteration
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
         if ( AF > 0. ) then
            QAo = QAx  ! + QSx - QS 
         else
            QAo = 0.
         end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         ALHX = (1.0-fQi)*MAPL_ALHL + fQi*MAPL_ALHS

         if(pdfflag.eq.1) then 
            QCn = QCp + ( QCn - QCp ) / ( 1. - (CFn * (ALPHA-1.) - (QCn/QSn))*DQS*ALHX/MAPL_CP)             
         elseif(pdfflag.eq.2) then
            ! This next line needs correcting - need proper d(del qc)/dT derivative for triangular
            ! for now, just use relaxation of 1/2.
            if (n.ne.nmax) QCn = QCp + ( QCn - QCp ) *0.5
         endif

         QVn = QVp - (QCn - QCp)
         TEn = TEp + (1.0-fQi)*(MAPL_ALHL/MAPL_CP)*( (QCn - QCp)*(1.-AF) + (QAo-QAx)*AF ) &
               +      fQi* (MAPL_ALHS/MAPL_CP)*( (QCn - QCp)*(1.-AF) + (QAo-QAx)*AF )

         if (abs(Ten - Tep) .lt. 0.00001) exit 

         DQS  = DQSAT( TEn, PL, QSAT=QSn )

      enddo ! qsat iteration

      CFo = CFn
      CF = CFn
      QCo = QCn
      QVo = QVn
      TEo = TEn

      ! Update prognostic variables.  Deal with special case of AF=1
      ! Temporary variables QCo, QAo become updated grid means.
      if ( AF < 1.0 ) then
         CF  = CFo * ( 1.-AF)
         QCo = QCo * ( 1.-AF)
         QAo = QAo *   AF  
      else

         ! Special case AF=1, i.e., box filled with anvil. 
         !   - Note: no guarantee QV_box > QS_box
         CF  = 0.          ! Remove any other cloud
         QAo = QA  + QC    ! Add any LS condensate to anvil type
         QCo = 0.          ! Remove same from LS   
         QT  = QAo + QV    ! Total water
         ! Now set anvil condensate to any excess of total water 
         ! over QSx (saturation value at top)
         QAo = MAX( QT - QSx, 0. )
      end if


      ! Now take {\em New} condensate and partition into ice and liquid
      ! taking care to keep both >=0 separately. New condensate can be
      ! less than old, so $\Delta$ can be < 0.


      dQCl = 0.0
      dQCi = 0.0
      dQAl = 0.0
      dQAi = 0.0

      !large scale   

      QCx   = QCo - QC
      if  (QCx .lt. 0.0) then  !net evaporation. Water evaporates first
         dQCl = max(QCx, -QCl)   
         dQCi = max(QCx - dQCl, -QCi)
      else
         dQCl  = (1.0-fQi)*QCx
         dQCi  =    fQi  * QCx
      end if

      !Anvil   
      QAx   = QAo - QA

      if  (QAx .lt. 0.0) then  !net evaporation. Water evaporates first
         dQAl = max(QAx, -QAl)   
         dQAi = max(QAx - dQAl, -QAi)
      else            
         dQAl  =  (1.0-fQi)*QAx
         dQAi  = QAx*fQi
      end if

      ! Clean-up cloud if fractions are too small
      if ( AF < 1.e-5 ) then
         dQAi = -QAi
         dQAl = -QAl
      end if
      if ( CF < 1.e-5 ) then
         dQCi = -QCi
         dQCl = -QCl
      end if

      QAi    = QAi + dQAi
      QAl    = QAl + dQAl
      QCi    = QCi + dQCi
      QCl    = QCl + dQCl
      QV     = QV  - ( dQAi+dQCi+dQAl+dQCl) 


      TE  = TE + (MAPL_ALHL*( dQAi+dQCi+dQAl+dQCl)+MAPL_ALHF*(dQAi+dQCi))/ MAPL_CP


      ! We need to take care of situations where QS moves past QA
      ! during QSAT iteration. This should be only when QA/AF is small
      ! to begin with. Effect is to make QAo negative. So, we 
      ! "evaporate" offending QA's
      !
      ! We get rid of anvil fraction also, although strictly
      ! speaking, PDF-wise, we should not do this.
      if ( QAo <= 0. ) then
         QV  = QV + QAi + QAl
         TE  = TE - (MAPL_ALHS/MAPL_CP)*QAi - (MAPL_ALHL/MAPL_CP)*QAl
         QAi = 0.
         QAl = 0.
         AF  = 0.  
      end if

   end subroutine hystpdf

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine pdffrac (flag,qtmean,sigmaqt1,sigmaqt2,qstar,clfrac)
      implicit none

      integer flag            ! flag to indicate shape of pdf
      ! 1 for tophat, 2 for triangular, 3 for Gaussian
      real qtmean             ! Grid box value of q total
      real sigmaqt1           ! width of distribution (sigma)
      real sigmaqt2           ! width of distribution (sigma)
      real qstar              ! saturation q at grid box avg T
      real clfrac             ! cloud fraction (area under pdf from qs)

      real :: qtmode, qtmin, qtmax


      real :: qtmedian,  aux

      if(flag.eq.1) then
         if((qtmean+sigmaqt1).lt.qstar) then
            clfrac = 0.
         else
            if(sigmaqt1.gt.0.) then
               clfrac = min((qtmean + sigmaqt1 - qstar),2.*sigmaqt1)/(2.*sigmaqt1)
            else
               clfrac = 1.
            endif
         endif
      elseif(flag.eq.2) then
         qtmode =  qtmean + (sigmaqt1-sigmaqt2)/3.
         qtmin = min(qtmode-sigmaqt1,0.)
         qtmax = qtmode + sigmaqt2
         if(qtmax.lt.qstar) then
            clfrac = 0.
         elseif ( (qtmode.le.qstar).and.(qstar.lt.qtmax) ) then
            clfrac = (qtmax-qstar)*(qtmax-qstar) / ( (qtmax-qtmin)*(qtmax-qtmode) )
         elseif ( (qtmin.le.qstar).and.(qstar.lt.qtmode) ) then
            clfrac = 1. - ( (qstar-qtmin)*(qstar-qtmin) / ( (qtmax-qtmin)*(qtmode-qtmin) ) )
         elseif ( qstar.le.qtmin ) then
            clfrac = 1.
         endif


      elseif(flag.eq.4) then !lognormal Donif
         if (qtmean .gt. 1.0e-20) then 
            qtmedian = qtmean*exp(-0.5*sigmaqt1*sigmaqt1)    
            aux  = log(qtmedian/qstar)/sqrt(2.0)/sigmaqt1
            aux=min(max(aux, -20.0), 20.0)   
            clfrac=0.5*(1.0+erf_app(aux))               
         else
            clfrac = 0.0
         end if
      endif

      return
   end subroutine pdffrac

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


   subroutine pdfcondensate (flag,qtmean4,sigmaqt14,sigmaqt24,qstar4,condensate4, clfrac4)
      implicit none

      integer flag            ! flag to indicate shape of pdf
      ! 1 for tophat, 2 for triangular, 4 for lognormal
      real qtmean4            ! Grid box value of q total
      real sigmaqt14          ! width of distribution (to left)
      real sigmaqt24          ! width of distribution (to right)
      real qstar4             ! saturation q at grid box avg T
      real condensate4        ! condensate (area under (q*-qt)*pdf from qs)
      real clfrac4

      real *8 :: qtmode, qtmin, qtmax, constA, constB, cloudf
      real *8 :: term1, term2, term3
      real *8 :: qtmean, sigmaqt1, sigmaqt2, qstar, condensate
      real *8 :: qtmedian, aux, clfrac

      qtmean = dble(qtmean4)
      sigmaqt1 = dble(sigmaqt14)
      sigmaqt2 = dble(sigmaqt24)
      qstar = dble(qstar4)
      clfrac=dble(clfrac4)

      if(flag.eq.1) then
         if(qtmean+sigmaqt1.lt.qstar) then
            condensate = 0.d0
         elseif(qstar.gt.qtmean-sigmaqt1)then
            if(sigmaqt1.gt.0.d0) then
               condensate = (min(qtmean + sigmaqt1 - qstar,2.d0*sigmaqt1)**2)/ (4.d0*sigmaqt1)
            else
               condensate = qtmean-qstar
            endif
         else
            condensate = qtmean-qstar
         endif
      elseif(flag.eq.2) then
         qtmode =  qtmean + (sigmaqt1-sigmaqt2)/3.d0
         qtmin = min(qtmode-sigmaqt1,0.d0)
         qtmax = qtmode + sigmaqt2
         if ( qtmax.lt.qstar ) then
            condensate = 0.d0
         elseif ( (qtmode.le.qstar).and.(qstar.lt.qtmax) ) then
            constB = 2.d0 / ( (qtmax - qtmin)*(qtmax-qtmode) )
            cloudf = (qtmax-qstar)*(qtmax-qstar) / ( (qtmax-qtmin)*(qtmax-qtmode) )
            term1 = (qstar*qstar*qstar)/3.d0
            term2 = (qtmax*qstar*qstar)/2.d0
            term3 = (qtmax*qtmax*qtmax)/6.d0
            condensate = constB * (term1-term2+term3) - qstar*cloudf
         elseif ( (qtmin.le.qstar).and.(qstar.lt.qtmode) ) then
            constA = 2.d0 / ( (qtmax - qtmin)*(qtmode-qtmin) )
            cloudf = 1.d0 - ( (qstar-qtmin)*(qstar-qtmin) / ( (qtmax-qtmin)*(qtmode-qtmin) ) )
            term1 = (qstar*qstar*qstar)/3.d0
            term2 = (qtmin*qstar*qstar)/2.d0
            term3 = (qtmin*qtmin*qtmin)/6.d0
            condensate = qtmean - ( constA * (term1-term2+term3) ) - qstar*cloudf
         elseif ( qstar.le.qtmin ) then
            condensate = qtmean-qstar
         endif

      elseif(flag.eq.4) then !lognormal DONIF

         if (qtmean .gt. 1.0e-20) then 
            aux = 0.5*sigmaqt1*sigmaqt1
            !sigma_lg = 2.0*sigmaqt1/sqrt(3.0)
            qtmedian = qtmean*exp(-aux) 
            !aux = log(qtmedian/qstar)/sqrt(2.0)/sigmaqt1
            !aux=min(max(aux, -20.0), 20.0)
            !clfrac=0.5*(1.0+dble(erf_app(sngl(aux))))  

            aux =  (aux  + log(qtmedian/qstar))/sqrt(2.0)/sigmaqt1
            aux=min(max(aux, -20.0), 20.0)

            aux =  1.0 + dble(erf_app(sngl(aux)))
            condensate = ((0.5*qtmean*aux/qstar) -clfrac)*qstar  
            condensate=  min(condensate, qtmean)
         else
            condensate = 0.0
         end if

      endif
      condensate4 = real(condensate)

      return
   end subroutine pdfcondensate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine cnvsrc( DT      , &
         ICEPARAM, &
         SCLMFDFR, &
         MASS    , &
         iMASS   , &
         PL      , &
         TE      , &
         QV      , &
         DCF     , &
         DMF     , &
         DCLFshlw, &
         DCIFshlw, &
         DMFshlw , &
         QLA     , & 
         QIA     , & 
         CF      , &
         AF      , &
         QS     , &
         NL, NI  , &
         CNVFICE , &
         CNVNDROP, &
         CNVNICE, &
         SCNDROP, &
         SCNICE)




      !INPUTS:
      !
      !       ICEPARAM: 0-1  controls how strongly new conv condensate is partitioned in ice-liquid
      !                 1 means partitioning follows ice_fraction(TE). 0 means all new condensate is
      !                 liquid (not used anymore)
      !
      !       SCLMFDFR: Scales detraining mass flux to a cloud fraction source - kludge. Thinly justified
      !                 by fuzziness of cloud boundaries and existence of PDF of condensates (for choices
      !                 0.-1.0) or by subgrid layering (for choices >1.0) 

      real, intent(in)    :: DT,ICEPARAM,SCLMFDFR
      real, intent(in)    :: MASS,iMASS,QS
      real, intent(in)    :: DMF,PL
      real, intent(in)    :: DCF,CF,DCIFshlw,DCLFshlw,DMFshlw
      real, intent(inout) :: TE
      real, intent(inout) :: AF,QV
      real, intent(inout) :: QLA, QIA


      real , intent(inout)       :: CNVFICE, CNVNDROP, CNVNICE !DONIF
      real, intent(inout) :: SCNDROP, SCNICE
      real, intent(inout) :: NI, NL

      real :: TEND,QVx,QCA,fQi, CFALL, fact, RL, RI

      integer  :: STRATEGY
      real     :: minrhx 
      real, parameter :: RL_cub  = 1.0e-15
      real, parameter :: RI_cub  =1.0e-12
      

      STRATEGY = 1

      !Minimum allowed env RH
      minrhx    = 0.001  
      fQi =0.0
      CFALL = AF+CF


      if (TE .lt. T_ICE_ALL)  then 
         fQi = 1.0
      elseif (TE .gt. T_ICE_MAX)  then 
         fQi = 0.0
      else
         fQi=CNVFICE
      end if

        RL =   9.0  + (12.0*(283.0- Te)/40.0)             
        RL =   min(max(RL, 9.0), 17.0)*1.e-6     
   
        RI =   60.0 + (80.0*(Te- 253.0)/40.0)
        RI =   min(max(RI, 20.0), 100.0)*1.e-6


      !Addition of condensate from RAS
      TEND = DCF*iMASS

      QLA  = QLA + (1.0-fQi)* TEND*DT + DCLFshlw*iMASS*DT
      QIA  = QIA +    fQi   * TEND*DT + DCIFshlw*iMASS*DT






      ! Check that N and Q are consistent 
      if  ( ( (1.0-fQi)*DCF .gt. 0.0) .and. (CNVNDROP .le. 0.0)) then 
         CNVNDROP =   (1.0-fQi)*DCF/( 1.333 * MAPL_PI *RL_cub*997.0)  
      end if

      if  ((fQi*DCF .gt. 0.0) .and. (CNVNICE .le. 0.0)) then 
         CNVNICE =   fQi*DCF/( 1.333 * MAPL_PI *RI_cub*500.0)         
      end if

      fact  = 1.333 * MAPL_PI *RL*RL*RL*997.0*disp_factor_liq
      NL= max(NL + CNVNDROP*iMASS*DT +  (DCLFshlw*iMASS*DT/fact) , 0.0)  !number source DONIF
      
      fact  =   1.333 * MAPL_PI *RI*RI*RI*800.0*disp_factor_ice
      NI = max(NI   +   CNVNICE*iMASS*DT + (fQi*TEND+ DCIFshlw*iMASS*DT/fact), 0.0)




      ! dont forget that conv cond has never frozen !!!!
      TE   = TE +   (MAPL_ALHS-MAPL_ALHL) * fQi * TEND*DT/MAPL_CP

      QCA  = QLA + QIA

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !! Tiedtke-style anvil fraction !!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      TEND=(DMF*SCLMFDFR+DMFshlw*SCLM_SHW)*iMASS    
      AF = AF + TEND*DT
      AF = MIN( AF , 0.99 ) 

      ! where ( (AF+CF) > 1.00 )
      !    AF=1.00-CF
      ! endwhere
      !!!!!!!!!!!!!!!

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Check for funny (tiny, negative)
      ! external QV s, resulting from assumed
      ! QV=QSAT within anvil.
      !
      ! Two strategies to fix 
      !   1) Simply constrain AF assume condensate
      !      just gets "packed" in
      !   2) Evaporate QCA to bring up QVx leave AF alone
      !      Should include QSAT iteration, but ...        
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      !   QS  = QSAT(         &
      !               TE    , &
      !               PL      )

      if ( AF < 1.0 ) then
         QVx  = ( QV - QS * AF )/(1.-AF)
      else
         QVx  = QS
      end if

      if (STRATEGY==1) then 
         if ( (( QVx - minrhx*QS ) < 0.0 ) .and. (AF > 0.) ) then
            AF  = (QV  - minrhx*QS )/( QS*(1.0-minrhx) )
         end if
         if ( AF < 0. ) then  ! If still cant make suitable env RH then destroy anvil
            AF  = 0.
            QV  = QV + QLA + QIA
            TE  = TE - (MAPL_ALHL*QLA + MAPL_ALHS*QIA)/MAPL_CP        
            QLA = 0.
            QIA = 0.
         end if
      else if (STRATEGY==2) then
         if ( (( QVx - minrhx*QS ) < 0.0 ) .and. (AF > 0.) ) then
            QV  = QV  + (1.-AF)*( minrhx*QS - QVx )
            QCA = QCA - (1.-AF)*( minrhx*QS - QVx )
            TE  = TE  - (1.-AF)*( minrhx*QS - QVx )*MAPL_ALHL/MAPL_CP
         end if
      end if

   end subroutine cnvsrc



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine PRECIP3(  &
         K,LM         , &
         DT           , & 
         FRLAND       , & 
         RHCR3        , &
         QPl          , &
         QPi          , &
         QCl          , &
         QCi          , &
         TE           , &
         QV           , &
         mass         , &
         imass        , &
         PL           , &
         dZE          , &
         QDDF3        , &
         AA           , &
         BB           , &
         AREA         , &
         RAIN         , & 
         SNOW         , &
         PFl_above    , &
         PFi_above    , &
         EVAP_DD_above, &
         SUBL_DD_above, &
         REVAP_DIAG   , &
         RSUBL_DIAG   , &
         ACRLL_DIAG        , &
         ACRIL_DIAG        , &
         PFL_DIAG     , &
         PFI_DIAG     , &
         VFALLRN      , &
         VFALLSN      , &
         FRZ_DIAG     , &
         ENVFC,DDRFC, AF, CF, &
         PCBL  )


      integer, intent(in) :: K,LM

      real, intent(in   ) :: DT

      real, intent(inout) :: QV,QPl,QPi,QCl,QCi,TE

      real, intent(in   ) :: mass,imass
      real, intent(in   ) :: PL
      real, intent(in   ) :: AA,BB
      real, intent(in   ) :: RHCR3
      real, intent(in   ) :: dZE
      real, intent(in   ) :: QDDF3
      real, intent(  out) :: RAIN,SNOW
      real, intent(in   ) :: AREA
      real, intent(in   ) :: FRLAND

      real, intent(inout) :: PFl_above, PFi_above
      real, intent(inout) :: EVAP_DD_above, SUBL_DD_above

      real, intent(  out) :: REVAP_DIAG
      real, intent(  out) :: RSUBL_DIAG
      real, intent(  out) :: ACRLL_DIAG,ACRIL_DIAG
      real, intent(  out) :: PFL_DIAG, PFI_DIAG
      real, intent(inout) :: FRZ_DIAG
      real, intent(  out) :: VFALLSN, VFALLRN

      real, intent(in   ) :: ENVFC,DDRFC

      real, intent(in   ) :: AF,CF, PCBL


      real :: PFi,PFl,QS,dQS,ENVFRAC
      real :: TKo,QKo,QSTKo,DQSTKo,RH_BOX,T_ED,QPlKo,QPiKo
      real :: Ifactor,RAINRAT0,SNOWRAT0
      real :: FALLRN,FALLSN,VEsn,VErn,NRAIN,NSNOW,Efactor

      real :: TinLAYERrn,DIAMrn,DROPRAD
      real :: TinLAYERsn,DIAMsn,FLAKRAD

      real :: EVAP,SUBL,ACCR,MLTFRZ,EVAPx,SUBLx
      real :: EVAP_DD,SUBL_DD,DDFRACT
      real :: LANDSEAF, TC, MAXMLT, iDT

      real :: tmpARR, CFR, aux, RH_EVAP

    
      real :: TAU_MLT, QSICE, DQSI, Told, QKCLR

      integer :: NS, NSMX, itr,L

      logical, parameter :: taneff = .true.
      
      
      
       real, parameter :: TRMV_L  = 1.0         ! m/s
      real, parameter :: TAU_FRZ = 5000.0      ! sec       
      real, parameter :: FRZ_TAU = 1.0/TAU_FRZ ! sec^-1
      real, parameter :: MELT_T  = 5.0         ! degrees C
      real, parameter :: LFBYCP  = MAPL_ALHF/MAPL_CP
      real, parameter :: CPBYLF  = 1.0/LFBYCP
      real, parameter :: B_SUB   = 1.00

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! fraction of precip falling through "environment" vs
      ! through cloud
     

      if(taneff) then
         !reproduces the atan profile but is less messy

        ! aux = min(max((pl- PCBL)/10.0, -20.0), 20.0) 
        ! aux = 1.0/(1.0+exp(-aux))
        ! envfrac = ENVFC + (1.0-ENVFC)*aux   !ENVFC is the minimum exposed area. Below cloud base envfrac becomes 1.    


         if (pl .le. 600.) then
            envfrac = 0.25
         else
            envfrac = 0.25 + (1.-0.25)/(19.) *                    &
                  ((atan( (2.*(pl-600.)/(900.-600.)-1.) *       &
                  tan(20.*MAPL_PI/21.-0.5*MAPL_PI) ) + 0.5*MAPL_PI) * 21./MAPL_PI - 1.)
          end if


         envfrac = min(envfrac,1.)

      else
         ENVFRAC = ENVFC
      endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      CFR= AF+CF
      if ( CFR < 0.99) then 
         tmpARR = 1./(1.-CFR)
      else
         tmpARR = 0.0
      end if
!!!!!!!!!!!!!!!!!!!

      IF ( AREA > 0. ) THEN
         Ifactor = 1./ ( AREA )
      ELSE
         Ifactor = 1.00
      END if

      Ifactor = MAX( Ifactor, 1.) !

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !
      !   Start at top of precip column:
      !  
      !               a) Accrete                   
      !               b) Evaporate/Sublimate  
      !               c) Rain/Snow-out to next level down 
      !               d) return to (a)
      !
      !   ....................................................................
      !           
      !  Accretion formulated according to Smith (1990, Q.J.R.M.S., 116, 435
      !  Eq. 2.29)
      !  
      !  Evaporation (ibid. Eq. 2.32)
      !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


iDT = 1.0/DT

!!! INITIALIZE DIAGNOSTIC ARRAYS !!!!!!!!!!!!!!!!!!!!!
      PFL_DIAG =  0.
      PFI_DIAG =  0.
      ACRIL_DIAG    =  0.
      ACRLL_DIAG    =  0.
      REVAP_DIAG    =  0.
      RSUBL_DIAG    =  0.
      RH_EVAP= RHCR3
      
      !RH_EVAP= 1.0

      DDFRACT = DDRFC 

      IF (K == 1) THEN
         PFl=QPl*MASS
         PFi=QPi*MASS

         EVAP_DD = 0.
         SUBL_DD = 0.

         VFALLRN = 0.0
         VFALLSN = 0.0
      ELSE 
         QPl   = QPl + PFl_above * iMASS
         PFl = 0.00

         QPi   = QPi + PFi_above * iMASS
         PFi = 0.00


          IF(QCL > 0.0) THEN
            IF(QPi > 0.0) THEN
               ACCR = min(C_ACC*(QPl*MASS)*QCl, QCl)
               QPl  = QPl + ACCR
               QCl  = QCl - ACCR

               ACRLL_DIAG = ACCR * iDT
            END IF

            IF(QPi > 0.0) THEN
               ACCR = min(C_ACC*(QPi*MASS)*QCl, QCl)
               QPi  = QPi + ACCR
               QCl  = QCl - ACCR
               TE   = TE + LFBYCP*ACCR

               ACRIL_DIAG = ACCR * iDT
            END IF
         END IF
        

         RAINRAT0 = Ifactor*QPl*MASS/DT
         SNOWRAT0 = Ifactor*QPi*MASS/DT

         call MARSHPALMQ2(RAINRAT0,PL,DIAMrn,NRAIN,FALLrn,VErn)
         call MARSHPALMQ2(SNOWRAT0,PL,DIAMsn,NSNOW,FALLsn,VEsn)

         IF ( FRLAND < 0.1 ) THEN
            !!      DIAMsn = MAX(  DIAMsn, 1.0e-3 )   ! Over Ocean
         END IF

         VFALLRN = FALLrn
         VFALLSN = FALLsn

         TinLAYERrn = dZE / ( FALLrn+0.01 )
         TinLAYERsn = dZE / ( FALLsn+0.01 )

          !*****************************************
         !  Melting of Frozen precipitation      
         !*****************************************

         TC = TE - MAPL_TICE

         IF( QPi > 0.0 .AND. TC > 0.0) THEN

            MAXMLT = min(QPi, TC*CPBYLF)

            IF ( K < LM-3 .and. TC <= MELT_T) THEN
               MLTFRZ = min(TinLAYERsn*QPi*TC*FRZ_TAU, MAXMLT)
            else
               MLTFRZ = MAXMLT
            END IF

            TE       = TE  - LFBYCP*MLTFRZ
            QPl      = QPl + MLTFRZ
            QPi      = QPi - MLTFRZ
            FRZ_DIAG = FRZ_DIAG - MLTFRZ * iDT

         END IF

         !*****************************************
         !  Freezing of rain
         !*****************************************

         IF ( QPl > 0.0 .AND.  TC <= 0.0) THEN

            MLTFRZ = min(QPl,-TC*CPBYLF)
            TE     = TE  + LFBYCP*MLTFRZ
            QPi    = QPi + MLTFRZ
            QPl    = QPl - MLTFRZ
            FRZ_DIAG = FRZ_DIAG + MLTFRZ * iDT

         END IF
         

         ! ******************************************
         !   In the exp below, evaporation time 
         !   scale is determined "microphysically"
         !   from temp, press, and drop size. In this
         !   context C_EV becomes a dimensionless 
         !   fudge-fraction.
         !   Also remember that these microphysics 
         !   are still only for liquid.
         ! ******************************************

         QKo   = QV
         TKo   = TE
         QPlKo = QPl
         QPiKo = QPi

         EVAP = 0.0
         SUBL = 0.0

         !  if (TKo .gt. 240.0) then 
         do itr = 1,20 ! 

            DQSTKo = DQSAT  ( TKo , PL, QSAT=QSTko   ) !use for rain


            !  DQSTKo = dQS
            ! QSTKo  = QS  + DQSTKo * ( TKo - TE )
            QSTKo  = MAX( QSTKo , 1.0e-7 )        

            !RH_BOX = QKo/QSTKo


!!!!! RAin falling !!!!!!!!!!!!!!!!!!!!!!!
            if (tmpARR .gt. 0.0) then 
               QKCLR=(QKo -QSTKo*CFR)*tmpARR
               RH_BOX =QKCLR/QSTKo 
            else
               RH_BOX = QKo/QSTKo
            end if

            IF ( RH_BOX < RH_EVAP ) THEN
               Efactor =  RHO_W * ( AA + BB )    / (RH_EVAP - RH_BOX )
            else
               Efactor = 9.99e9
            end if


            LANDSEAF = 1.00


            if ( ( RH_BOX < RH_EVAP ) .AND. ( DIAMrn > 0.00 ) .AND. &
                  ( PL > 100. ) .AND. ( PL < REVAP_OFF_P ) ) then
               DROPRAD=0.5*DIAMrn
               T_ED =  Efactor * DROPRAD**2 
               T_ED =  T_ED * ( 1.0 + DQSTKo*MAPL_ALHL/MAPL_CP )
               !EVAP =  QPl*(1.0 - EXP( -C_EV_R * VErn * LANDSEAF * ENVFRAC * TinLAYERrn / T_ED ) )
               EVAP =  QPl*(1.0 - EXP( -C_EV_R * VErn * LANDSEAF *ENVFRAC* TinLAYERrn / T_ED ) )
            ELSE
               EVAP = 0.0
            END if

!!!!! Snow falling !!!!!!!!!!!!!!!!!!!!!!! 

            !QSICE  = QSATIC(  min(TKo, T_ICE_MAX), PL*100.0 , DQ=DQSI ) ! use for snow

            DQSI = DQSAT( TKo , PL, 5.0, QSAT = QSICE ) !use for snow, small ramp to assure continuitiy at higher T 
            !DQSI = DQSAT( TKo , PL, QSAT = QSICE ) 
            QSICE  = MAX( QSICE , 1.0e-7 )
            if (tmpARR .gt. 0.0) then 
               QKCLR =(QKo -QSICE*CFR)*tmpARR !Snow only sublimates when QV<QICE  
               RH_BOX =QKCLR/QSICE 
            else
               RH_BOX =QKo/QSICE                                        
            end if



            IF ( RH_BOX < RH_EVAP ) THEN
               Efactor =  0.5*RHO_W * ( AA + BB )    / (RH_EVAP - RH_BOX )
            else
               Efactor = 9.99e9
            end if


            if ( ( RH_BOX < RH_EVAP ) .AND. ( DIAMsn > 0.00 ) .AND. &
                  ( PL > 100. ) .AND. ( PL < REVAP_OFF_P ) ) then
               FLAKRAD=0.5*DIAMsn
               T_ED =  Efactor * FLAKRAD**2   
               T_ED =  T_ED * ( 1.0 + DQSI*MAPL_ALHS/MAPL_CP )
               SUBL =  QPi*(1.0 - EXP( -C_EV_S * VEsn * LANDSEAF * ENVFRAC * TinLAYERsn / T_ED ) )
               !SUBL =  QPi*(1.0 - EXP( -C_EV_S * VEsn * LANDSEAF * ENVFRAC * TinLAYERsn / T_ED ) )
            ELSE
               SUBL = 0.0
            END IF

            if (itr == 1) then 
               EVAPx  = EVAP
               SUBLx  = SUBL
            else
               EVAP   = (EVAP+EVAPx) /2.0
               SUBL   = (SUBL+SUBLx) /2.0
            endif

            EVAP= EVAP*(1.-CFR)! Can only evaporate in the clear parto of the cell DONIF
            SUBL = SUBL*(1.-CFR)

            Told = TKo
        	    !QKo=QKo + EVAP + SUBL
            TKo=TKo - EVAP * MAPL_ALHL / MAPL_CP - SUBL * MAPL_ALHS / MAPL_CP


            if (abs(Told-Tko) .le. 0.01) exit
         enddo
         ! end if       

         QPi  = QPi - SUBL
         QPl  = QPl - EVAP

         !! Put some re-evap/re-subl precip in to a \quote{downdraft} to be applied later
         EVAP_DD = EVAP_DD_above + DDFRACT*EVAP*MASS 
         EVAP    = EVAP          - DDFRACT*EVAP
         SUBL_DD = SUBL_DD_above + DDFRACT*SUBL*MASS 
         SUBL    = SUBL          - DDFRACT*SUBL
         ! -----

         QV   = QV  + EVAP + SUBL
         TE   = TE  - EVAP * MAPL_ALHL / MAPL_CP - SUBL * MAPL_ALHS / MAPL_CP

         REVAP_DIAG = EVAP / DT
         RSUBL_DIAG = SUBL / DT

         PFl  = QPl*MASS
         PFi  = QPi*MASS

         PFL_DIAG =  PFl/DT
         PFI_DIAG =  PFi/DT
      end if

      ! QDDF3 (<= QDDF3_dev) is calculated on the CPU in order to avoid
      ! the reverse loop on GPUs and thus save local memory use.
      EVAP = QDDF3*EVAP_DD/MASS
      SUBL = QDDF3*SUBL_DD/MASS
      QV   = QV  + EVAP + SUBL
      TE   = TE  - EVAP * MAPL_ALHL / MAPL_CP - SUBL * MAPL_ALHS / MAPL_CP
      REVAP_DIAG = REVAP_DIAG + EVAP / DT
      RSUBL_DIAG = RSUBL_DIAG + SUBL / DT

      IF (K == LM) THEN
         RAIN  = PFl/DT
         SNOW  = PFi/DT
      END IF

      QPi = 0.
      QPl = 0.

      PFl_above = PFl
      PFi_above = Pfi

      EVAP_DD_above = EVAP_DD
      SUBL_DD_above = SUBL_DD

   end subroutine precip3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine MARSHPALMQ2(RAIN,PR,DIAM3,NTOTAL,W,VE)

      real, intent(in ) :: RAIN,PR     ! in kg m**-2 s**-1, mbar
      real, intent(out) :: DIAM3,NTOTAL,W,VE

      real :: RAIN_DAY,LAMBDA,A,B,SLOPR,DIAM1

      real, parameter  :: N0   = 0.08  ! # cm**-3

      INTEGER :: IQD

      real :: RX(8) , D3X(8)

      ! Marshall-Palmer sizes at different rain-rates: avg(D^3)

      RX = (/ 0.   , 5.   , 20.  , 80.  , 320. , 1280., 4*1280., 16*1280. /)  ! rain per in mm/day
      D3X= (/ 0.019, 0.032, 0.043, 0.057, 0.076, 0.102, 0.137  ,  0.183   /)

      RAIN_DAY = RAIN * 3600. *24.

      IF ( RAIN_DAY <= 0.00 ) THEN
         DIAM1 = 0.00
         DIAM3 = 0.00
         NTOTAL= 0.00
         W     = 0.00
      END IF

      DO IQD = 1,7
         IF ( (RAIN_DAY <= RX(IQD+1)) .AND. (RAIN_DAY > RX(IQD) ) ) THEN
            SLOPR =( D3X(IQD+1)-D3X(IQD) ) / ( RX(IQD+1)-RX(IQD) )
            DIAM3 = D3X(IQD) + (RAIN_DAY-RX(IQD))*SLOPR
         END IF
      END DO

      IF ( RAIN_DAY >= RX(8) ) THEN
         DIAM3=D3X(8)
      END IF

      NTOTAL = 0.019*DIAM3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      DIAM3 = 0.664 * DIAM3  !!  DRYING/EVAP SHOULD PROBABLY GO AS          !!
      !!  D_1.5 == <<D^(3/2)>>^(2/3) NOT AS          !!
      !!  D_3   == <<D^3>>^(1/3)                     !!
      !!  RATIO D_1.5/D_3 =~ 0.66  (JTB 10/17/2002)  !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      W      = (2483.8 * DIAM3 + 80.)*SQRT(1000./PR)
      !VE     = 1.0  + 28.0*DIAM3 
      VE     = MAX( 0.99*W/100. , 1.000 )

      DIAM1  = 3.0*DIAM3
      !  Change back to MKS units

      DIAM1  = DIAM1/100.
      DIAM3  = DIAM3/100.
      W      = W/100.
      NTOTAL = NTOTAL*1.0e6

   end subroutine MARSHPALMQ2
   !==========================================================

   subroutine MICRO_AA_BB_3(TEMP,PR,Q_SAT,AA,BB)

      real, intent(in ) :: TEMP,Q_SAT
      real, intent(in ) :: PR
      real, intent(out) :: AA,BB

      real :: E_SAT

      real, parameter  :: EPSILON =  MAPL_H2OMW/MAPL_AIRMW
      real, parameter  :: K_COND  =  2.4e-2        ! J m**-1 s**-1 K**-1
      real, parameter  :: DIFFU   =  2.2e-5        ! m**2 s**-1

      E_SAT = 100.* PR * Q_SAT /( (EPSILON) + (1.0-(EPSILON))*Q_SAT )  ! (100 converts from mbar to Pa)

      AA  = ( GET_ALHX3(TEMP)**2 ) / ( K_COND*MAPL_RVAP*(TEMP**2) )
      ! AA  = ( MAPL_ALHL**2 ) / ( K_COND*MAPL_RVAP*(TEMP**2) )

      BB  = MAPL_RVAP*TEMP / ( DIFFU*(1000./PR)*E_SAT )

   end subroutine MICRO_AA_BB_3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   function LDRADIUS3(PL,TE,QCL,NN) RESULT(RADIUS)

      real, intent(in) :: TE,PL,NN,QCL
      real :: RADIUS

      real :: MUU,RHO


      RHO = 100.*PL / (MAPL_RGAS*TE )
      MUU = QCL * RHO                       
      RADIUS = MUU/(NN*RHO_W*(4./3.)*MAPL_PI)
      RADIUS = RADIUS**(1./3.)    ! Equiv. Spherical Cloud Particle Radius in m


   end function LDRADIUS3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


   function ICE_FRACTION (TEMP) RESULT(ICEFRCT)
      real, intent(in) :: TEMP
      real             :: ICEFRCT

      ICEFRCT  = 0.00
      if ( TEMP <= T_ICE_ALL ) then
         ICEFRCT = 1.000
      else if ( (TEMP > T_ICE_ALL) .AND. (TEMP <= T_ICE_MAX) ) then
         ICEFRCT = 1.00 -  ( TEMP - T_ICE_ALL ) / ( T_ICE_MAX - T_ICE_ALL ) 
      end if
      ICEFRCT = MIN(ICEFRCT,1.00)
      ICEFRCT = MAX(ICEFRCT,0.00)

      !!ICEFRCT = ICEFRCT**4
      !ICEFRCT = ICEFRCT**ICEFRPWR

   end function ICE_FRACTION

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   function GET_ALHX3(T) RESULT(ALHX3)

      real, intent(in) :: T
      real :: ALHX3

      real :: T_X

      T_X = T_ICE_MAX

      if ( T < T_ICE_ALL ) then
         ALHX3=MAPL_ALHS
      end if

      if ( T > T_X ) then
         ALHX3=MAPL_ALHL
      end if

      if ( (T <= T_X) .and. (T >= T_ICE_ALL) ) then
         ALHX3 = MAPL_ALHS + (MAPL_ALHL-MAPL_ALHS)*( T - T_ICE_ALL ) /( T_X - T_ICE_ALL )
      end if

   end function GET_ALHX3


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


   real function ICEFRAC(T,T_TRANS,T_FREEZ)

      real, intent(in) :: T
      real, intent(in),optional :: T_TRANS
      real, intent(in),optional :: T_FREEZ

      real :: T_X,T_F

      if (present( T_TRANS )) then 
         T_X = T_TRANS
      else
         T_X = T_ICE_MAX
      endif
      if (present( T_FREEZ )) then 
         T_F = T_FREEZ
      else
         T_F = T_ICE_ALL
      endif


      if ( T < T_F ) ICEFRAC=1.000

      if ( T > T_X ) ICEFRAC=0.000

      if ( T <= T_X .and. T >= T_F ) then 
         ICEFRAC = 1.00 - ( T - T_F ) /( T_X - T_F )
      endif

   end function ICEFRAC



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !Parititions DQ into ice and liquid. Follows Morrison and Gettelman, 2008
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine Bergeron_iter    (           &
         DTIME            , &
         PL               , &
         TE               , &
         QV               , &
         QILS             , &
         QICN             , &
         QLLS             , &
         QLCN             , &     
         CF               , &
         AF               , &
         NL               , &
         NI               , & 
         DQALL            , &
         FQI                 )

      real ,  intent(in   )    :: DTIME, PL, TE       !, RHCR
      real ,  intent(inout   )    ::  DQALL 
      real ,  intent(in)    :: QV, QLLS, QLCN, QICN, QILS
      real ,  intent(in)    :: CF, AF, NL, NI
      real, intent (out) :: FQI
      real  :: DC, TEFF,QCm,DEP, &
            QC, QS, RHCR, DQSL, DQSI, QI, TC, &
            DIFF, DENAIR, DENICE, AUX, &
            DCF, QTOT, LHCORR,  QL, DQI, DQL, &
            QVINC, QSLIQ, CFALL,  new_QI, new_QL, &
            QSICE, fQI_0, QS_0, DQS_0, FQA, NIX


      DIFF = 0.0     
      DEP=0.0 
      QI = QILS + QICN !neccesary because NI is for convective and large scale 
      QL = QLLS +QLCN
      QTOT=QI+QL
      FQA = 0.0
      if (QTOT .gt. 0.0) FQA = (QICN+QILS)/QTOT
      NIX= (1.0-FQA)*NI

      DQALL=DQALL/DTIME                                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
      CFALL= min(CF+AF, 1.0)
      TC=TE-273.0
      fQI_0 = fQI


      !Completelely glaciated cloud:
      if (TE .ge. T_ICE_MAX) then   !liquid cloud
         FQI   = 0.0

      elseif(TE .le. T_ICE_ALL) then !ice cloud

         FQI   = 1.0

      else !mixed phase cloud


         FQI   = 0.0
         if (QILS .lt. 1.e-12) then 
           fQi=  ICE_FRACTION(TE)	
         end if
          

         QVINC=  QV 
         QSLIQ  = QSATLQ(         &
               TE   , &
               PL*100.0 , DQ=DQSL )


         QSICE  = QSATIC(         &
               TE   , &
               PL*100.0 , DQ=DQSI )


         QVINC =MIN(QVINC, QSLIQ) !limit to below water saturation 



         ! Calculate deposition onto preexisting ice 

         DIFF=(0.211*1013.25/(PL+0.1))*(((TE+0.1)/273.0)**1.94)*1e-4  !From Seinfeld and Pandis 2006
         DENAIR=PL*100.0/MAPL_RGAS/TE
         DENICE= 1000.0*(0.9167 - 1.75e-4*TC -5.0e-7*TC*TC) !From PK 97
         LHcorr = ( 1.0 + DQSI*MAPL_ALHS/MAPL_CP) !must be ice deposition

         if  ((NIX .gt. 1.0) .and. (QILS .gt. 1.0e-10)) then 
            DC=max((QILS/(NIX*DENICE*MAPL_PI))**(0.333), 20.0e-6) !Assumme monodisperse size dsitribution 
         else
            DC = 20.0e-6
         end if


         TEFF= NIX*DENAIR*2.0*MAPL_PI*DIFF*DC/LHcorr

         DEP=0.0
         if ((TEFF .gt. 0.0) .and. (QILS .gt. 1.0e-14)) then 
            AUX =max(min(DTIME*TEFF, 20.0), 0.0)
            DEP=(QVINC-QSICE)*(1.0-EXP(-AUX))/DTIME
         end if

         DEP=MAX(DEP, -QILS/DTIME) !only existing ice can be sublimated

         !DEP=max(DEP, 0.0)

         DQI = 0.0
         DQL = 0.0
         FQI=0.0
         !QS_MIX=QSLIQ
         !DQS_MIX = DQSL
         !Partition DQALL accounting for Bergeron-Findensen process

         if  (DQALL .ge. 0.0) then !net condensation. Note do not allow bergeron with QLCN

            if (DEP .gt. 0.0) then 
               DQI = min(DEP, DQALL + QLLS/DTIME)
               DQL = DQALL - DQI
            else
               DQL=DQALL ! could happen because the PDF allows condensation in subsaturated conditions
               DQI = 0.0 
            end if
         end if


         if  (DQALL .lt. 0.0) then  !net evaporation. Water evaporates first regaardless of DEP   
            DQL = max(DQALL, -QLLS/DTIME)   
            DQI = max(DQALL - DQL, -QILS/DTIME)        
         end if

         if (DQALL .ne. 0.0)  FQI=max(min(DQI/DQALL, 1.0), 0.0)


      end if !=====  


   end subroutine Bergeron_iter



   !=============================================================================
   ! Subroutine Pfreezing: calculates the probability of finding a supersaturated parcel in the grid cell
   !SC_ICE is the effective freezing point for ice (Barahona & Nenes. 2009)
   ! Modified 02/19/15. in situ nucleation only occurs in the non_convective part of the grid cell


   subroutine Pfreezing (     &             
         ALPHA    , &
         PL       , &
         TE       , &
         QV       , &              
         QCl      , &
         QAl      , & 
         QCi      , &
         QAi      , &    
         SC_ICE   , &
         CF       , &
         AF       , &          
         PF       )



      real ,   intent(in)         :: PL,ALPHA, QV, SC_ICE, AF, TE, &
            QCl, QCi, QAl, QAi, CF  
      real ,   intent(out)       ::    PF


      real   :: qt, QCx, QSn, tmpARR, CFALL, QVx, CFio, QA, QAx, QC, QI, QL, DQSx
      real   :: sigmaqt1, sigmaqt2, qsnx, aux


      QA = QAl + QAi  
      QC=QCl+QCi 
      !CFALL = AF+CF 
      
      CFALL = AF

      if ( CFALL >= 1.0 ) then 
         PF = 0.0 
         return 
      end if


      QSn = QSATIC(         &
            TE   , &
            PL*100.0 , DQ=DQSx ) !only with respect to ice

      QSn  = MAX( QSn , 1.0e-9 )  



      tmpARR = 0.0    
      if  ( CFALL < 0.99 ) then 
         tmpARR = 1./(1.0-CFALL)
      end if

      QCx = QC*tmpARR
      QVx = ( QV - QSn*CFALL )*tmpARR
      !      QVx = QV*tmpARR

      qt =  QCx  +  QVx
      !qt=QVx

      CFio=0.0

  !QSn =  QSn*SC_ICE

      if(pdfflag.lt.2) then
           sigmaqt1  = max(ALPHA, 0.01)*QSn
           sigmaqt2  = max(ALPHA, 0.01)*QSn 
      elseif(pdfflag.eq.2) then
         ! for triangular, symmetric: sigmaqt1 = sigmaqt2 = alpha*qsn (alpha is half width)
         ! for triangular, skewed r : sigmaqt1 < sigmaqt2
         ! try: skewed right below 500 mb
!!!       if(pl.lt.500.) then
         sigmaqt1  = ALPHA*QSn
         sigmaqt2  = ALPHA*QSn
      elseif(pdfflag .eq. 4) then !lognormal (sigma is dimmensionless)
         sigmaqt1 =  max(ALPHA/sqrt(3.0), 0.001)     
      endif

      qsnx= Qsn*SC_ICE

     call pdffrac(pdfflag,qt,sigmaqt1,sigmaqt2,qsnx,CFio)

      PF  = CFio*(1.0-CFALL)        

      PF=min(max(PF, 0.0), 0.999)


   end subroutine Pfreezing




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Instantaneous freezing of condensate!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


   subroutine meltfrz_inst  (     &
         IM,JM,LM , &
         TE       , &
         QCL       , &
         QAL       , &
         QCI        , &
         QAI        , &               
         NL       , &
         NI            )

      integer, intent(in)                             :: IM,JM,LM
      real ,   intent(inout), dimension(:,:,:)   :: TE,QCL,QCI, QAL, QAI, NI, NL

      real ,   dimension(im,jm,lm)              :: fQi,dQil, DQmax, QLTOT, QITOT, FQAL, FQAI, dNil, FQA

      real, parameter :: iceliq_fact = 1.0 !accounts for the fact that ice crystals are larger than liquid drops

      QITOT= QCI+QAI
      QLTOT=QCL + QAL
      FQA = 0.0


      where (QITOT+QLTOT .gt. 0.0)
         FQA= (QAI+QAL)/(QITOT+QLTOT)
      end where


      dQil = 0.0
      dNil =0.0
      DQmax  = 0.0

      ! freeze liquid instantaneosly below -40 C
      where( TE <= T_ICE_ALL )
         DQmax = (T_ICE_ALL - TE)*MAPL_CP/(MAPL_ALHS-MAPL_ALHL)   
         dQil = min(QLTOT , DQmax)  
      end where

      where ((dQil .le. DQmax) .and. (dQil .gt. 0.0))
         dNil = NL*iceliq_fact 
      end where

      where ((dQil .gt. DQmax) .and. (dQil .gt. 0.0)) 
         dNil  =  NL*iceliq_fact*DQmax/dQil 
      end where

      dQil = max(  0., dQil )
      QITOT = max(QITOT + dQil, 0.0)
      QLTOT= max(QLTOT -  dQil, 0.0)                  
      NL  = NL - dNil
      NI   = NI  + dNil
      TE   = TE + (MAPL_ALHS-MAPL_ALHL)*dQil/MAPL_CP

      dQil = 0.0
      dNil =0.0
      DQmax  = 0.0

      ! melt ice instantly above 0^C
      where( TE > T_ICE_MAX )
         DQmax =  (TE-T_ICE_MAX) *MAPL_CP/(MAPL_ALHS-MAPL_ALHL)   
         dQil = min(QITOT, DQmax)  
      endwhere

      where ((dQil .le. DQmax) .and. (dQil .gt. 0.0))
         dNil = NI
      end where
      where ((dQil .gt. DQmax) .and. (dQil .gt. 0.0)) 
         dNil  =  NI*DQmax/dQil 
      end where
      dQil = max(  0., dQil )
      QLTOT =  max(QLTOT+ dQil, 0.)
      QITOT = max(QITOT - dQil, 0.) 
      NL  = NL + dNil
      NI   = NI  - dNil

      TE   = TE - (MAPL_ALHS-MAPL_ALHL)*dQil/MAPL_CP

      QCI   = QITOT*(1.0-FQA)
      QAI = QITOT*FQA      
      QCL   = QLTOT*(1.0-FQA)
      QAL = QLTOT*FQA

   end subroutine meltfrz_inst



   !======================================
   subroutine cloud_ptr_stubs (             &
         
         SMAXL, SMAXI, WSUB, CCN01, CCN04, CCN1, &
         NHET_NUC, NLIM_NUC, SO4, ORG, BCARBON, &
         DUST, SEASALT, NCPL_VOL, NCPI_VOL, NRAIN, NSNOW, &
         CDNC_NUC, INC_NUC, SAT_RAT, QSTOT, QRTOT, CLDREFFS, CLDREFFR, &
         DQVDT_micro,DQIDT_micro, DQLDT_micro, DTDT_micro, &
         RL_MASK, RI_MASK, KAPPA, SC_ICE, CFICE, CFLIQ, RHICE, RHLIQ, &
         RAD_CF, RAD_QL, RAD_QI, RAD_QS, RAD_QR, RAD_QV, &
         CLDREFFI, CLDREFFL, NHET_IMM, NHET_DEP,  &
         DUST_IMM, DUST_DEP, SCF, SCF_ALL, &           
         SIGW_GW, SIGW_CNV, SIGW_TURB, SIGW_RC, RHCmicro, DNHET_IMM,  & 
         BERG, BERGSO, MELT, DNHET_CT, DTDT_macro, QCRES, DT_RASP, FRZPP_LS, SNOWMELT_LS, &
         QIRES, AUTICE, PFRZ, DNCNUC, DNCHMSPLIT, DNCSUBL, &
         DNCAUTICE, DNCACRIS, DNDCCN, DNDACRLS, DNDEVAPC, DNDACRLR, DNDAUTLIQ, DNDCNV, DNCCNV)



      real  ,  pointer , dimension(:,:,:)  :: SMAXL,SMAXI, WSUB, CCN01, CCN04, CCN1, &
            NHET_NUC, NLIM_NUC, SO4, ORG, BCARBON, &
            DUST, SEASALT, NCPL_VOL, NCPI_VOL, NRAIN, NSNOW, &
            CDNC_NUC, INC_NUC, SAT_RAT, QSTOT, QRTOT, CLDREFFS, &
            CLDREFFR, DQVDT_micro,DQIDT_micro, DQLDT_micro, &
            DTDT_micro, RL_MASK, RI_MASK, KAPPA, SC_ICE, &
            CFICE, CFLIQ, RHICE, RHLIQ, RAD_CF, RAD_QL, &
            RAD_QI, RAD_QS, RAD_QR, RAD_QV,  &
            CLDREFFI, CLDREFFL, NHET_IMM, NHET_DEP, &
            DUST_IMM, DUST_DEP,  SCF, SCF_ALL, &
            SIGW_GW, SIGW_CNV, SIGW_TURB, SIGW_RC, RHCmicro, DNHET_IMM, &
            BERG, BERGSO, MELT, DNHET_CT,  &
            DTDT_macro, QCRES, DT_RASP, FRZPP_LS, SNOWMELT_LS, QIRES, AUTICE, &
            PFRZ, DNCNUC, DNCHMSPLIT, DNCSUBL, &
            DNCAUTICE, DNCACRIS, DNDCCN, DNDACRLS, &
            DNDEVAPC, DNDACRLR, DNDAUTLIQ,  DNDCNV, DNCCNV


      !DONIF   

      IF( ASSOCIATED(SMAXL) )        SMAXL    = 0. 
      IF( ASSOCIATED(SMAXI) )           SMAXI    = 0. 
      IF( ASSOCIATED(WSUB) )         WSUB     = 0. 
      IF( ASSOCIATED(CCN01) )         CCN01    = 0. 
      IF( ASSOCIATED(CCN04) )         CCN04    = 0. 
      IF( ASSOCIATED(CCN1) )          CCN1     = 0. 
      IF( ASSOCIATED(NHET_NUC) )  NHET_NUC    = 0. 
      IF( ASSOCIATED(NLIM_NUC) )   NLIM_NUC    = 0. 
      IF( ASSOCIATED(SO4) )             SO4       = 0. 
      IF( ASSOCIATED(ORG) )            ORG       = 0. 
      IF( ASSOCIATED(BCARBON) )   BCARBON   = 0. 
      IF( ASSOCIATED(DUST) )          DUST      = 0. 
      IF( ASSOCIATED(SEASALT) )    SEASALT   = 0. 
      IF( ASSOCIATED(NCPL_VOL) )  NCPL_VOL  = 0. 
      IF( ASSOCIATED(NCPI_VOL) )   NCPI_VOL  = 0. 

      IF( ASSOCIATED(NRAIN) )         NRAIN  = 0. 
      IF( ASSOCIATED(NSNOW) )       NSNOW  = 0.
      IF( ASSOCIATED(CDNC_NUC) )  CDNC_NUC  = 0.
      IF( ASSOCIATED(INC_NUC) )   INC_NUC  = 0.
      IF( ASSOCIATED(SAT_RAT) )   SAT_RAT  = 0.
      IF( ASSOCIATED(QSTOT) )     QSTOT  = 0.
      IF( ASSOCIATED(QRTOT) )     QRTOT  = 0.   

      IF( ASSOCIATED(DQVDT_micro) )   DQVDT_micro  = 0.   
      IF( ASSOCIATED(DQIDT_micro) )   DQIDT_micro  = 0.
      IF( ASSOCIATED(DQLDT_micro) )   DQLDT_micro  = 0.
      IF( ASSOCIATED(DTDT_micro) )    DTDT_micro   = 0.
      IF( ASSOCIATED(DTDT_macro) )    DTDT_macro   = 0.

      IF( ASSOCIATED(RL_MASK) )       RL_MASK      = 0.
      IF( ASSOCIATED(RI_MASK) )       RI_MASK      = 0.
      IF( ASSOCIATED(KAPPA) )         KAPPA        = 0.            
      IF( ASSOCIATED(SC_ICE))         SC_ICE        = 0.
      IF( ASSOCIATED(RHICE) )         RHICE        = 0.
      IF( ASSOCIATED(RHLIQ) )         RHLIQ        = 0.
      IF( ASSOCIATED(CFICE) )         CFICE        = 0.
      IF( ASSOCIATED(CFLIQ) )         CFLIQ        = 0.


      IF( ASSOCIATED(RAD_CF) )          RAD_CF        = 0.
      IF( ASSOCIATED(RAD_QL) )          RAD_QL        = 0.
      IF( ASSOCIATED(RAD_QI) )          RAD_QI        = 0.
      IF( ASSOCIATED(RAD_QS) )          RAD_QS        = 0.
      IF( ASSOCIATED(RAD_QR) )          RAD_QR        = 0.
      IF( ASSOCIATED(RAD_QV) )          RAD_QV        = 0.
      IF( ASSOCIATED(CLDREFFI) )        CLDREFFI      = 0.
      IF( ASSOCIATED(CLDREFFL) )       CLDREFFL      = 0.
      IF( ASSOCIATED(CLDREFFS) )      CLDREFFS  = 0.   
      IF( ASSOCIATED(CLDREFFR) )      CLDREFFR  = 0.

      IF( ASSOCIATED(NHET_IMM) )        NHET_IMM      = 0.
      IF( ASSOCIATED(NHET_DEP) )        NHET_DEP      = 0.
      IF( ASSOCIATED(DUST_IMM) )        DUST_IMM      = 0.
      IF( ASSOCIATED(DUST_DEP) )        DUST_DEP      = 0.


      IF( ASSOCIATED(SCF) )             SCF           = 0.
      IF( ASSOCIATED(SCF_ALL) )         SCF_ALL       = 0.
      IF( ASSOCIATED(SIGW_GW) )     SIGW_GW   = 0.
      IF( ASSOCIATED(SIGW_CNV) )     SIGW_CNV   = 0.
      IF( ASSOCIATED(SIGW_TURB) )   SIGW_TURB   = 0.
      IF( ASSOCIATED(SIGW_RC) )     SIGW_RC  = 0.
      IF( ASSOCIATED(RHCmicro) )     RHCmicro = 0.
      IF( ASSOCIATED(DNHET_IMM) )    DNHET_IMM = 0. 
      IF( ASSOCIATED(BERG) )    BERG = 0.
      IF( ASSOCIATED(BERGSO))   BERGSO = 0.
      IF( ASSOCIATED(MELT) )    MELT = 0.
      IF( ASSOCIATED(DNHET_CT) )    DNHET_CT = 0.
      IF( ASSOCIATED(DT_RASP) )    DT_RASP = 0.

      IF( ASSOCIATED(QCRES) )    QCRES = 0.
      IF( ASSOCIATED(QIRES) )    QIRES = 0.
      IF( ASSOCIATED(AUTICE) )    AUTICE = 0.
      IF( ASSOCIATED(FRZPP_LS) )    FRZPP_LS = 0.
      IF( ASSOCIATED(SNOWMELT_LS) )    SNOWMELT_LS = 0.
      IF( ASSOCIATED(PFRZ) )    PFRZ = 0.

      IF( ASSOCIATED(DNCNUC) )    DNCNUC = 0.
      IF( ASSOCIATED(DNCSUBL) )    DNCSUBL = 0.
      IF( ASSOCIATED(DNCHMSPLIT) )    DNCHMSPLIT = 0.
      IF( ASSOCIATED(DNCAUTICE) )    DNCAUTICE = 0.
      IF( ASSOCIATED(DNCACRIS) )    DNCACRIS = 0.
      IF( ASSOCIATED(DNDCCN) )      DNDCCN = 0.
      IF( ASSOCIATED(DNDACRLS) )    DNDACRLS = 0.
      IF( ASSOCIATED(DNDACRLR) )    DNDACRLR = 0.
      IF( ASSOCIATED(DNDEVAPC) )    DNDEVAPC = 0.
      IF( ASSOCIATED(DNDAUTLIQ) )   DNDAUTLIQ = 0.
      IF( ASSOCIATED(DNDCNV) )   DNDCNV = 0.
      IF( ASSOCIATED(DNCCNV) )   DNCCNV = 0.

   end subroutine cloud_ptr_stubs

   !C=======================================================================
   !C
   !C *** REAL FUNCTION erf (overwrites previous versions)
   !C *** THIS SUBROUTINE CALCULATES THE ERROR FUNCTION USING A
   !C *** POLYNOMIAL APPROXIMATION
   !C
   !C=======================================================================
   !C
   REAL FUNCTION erf_app(x)
      REAL :: x
      REAL*8:: AA(4), axx, y
      DATA AA /0.278393d0,0.230389d0,0.000972d0,0.078108d0/

      y = dabs(dble(x))
      axx = 1.d0 + y*(AA(1)+y*(AA(2)+y*(AA(3)+y*AA(4))))
      axx = axx*axx
      axx = axx*axx
      axx = 1.d0 - (1.d0/axx)
      if(x.le.0.) then
         erf_app = sngl(-axx)
      else
         erf_app = sngl(axx)
      endif
      RETURN
   END FUNCTION erf_app


end module cldmacro
