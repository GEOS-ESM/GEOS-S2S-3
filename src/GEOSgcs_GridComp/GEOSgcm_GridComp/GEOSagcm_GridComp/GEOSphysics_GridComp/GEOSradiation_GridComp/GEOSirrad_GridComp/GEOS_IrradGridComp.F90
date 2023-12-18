
!  $Id: GEOS_IrradGridComp.F90,v 1.51.10.2.12.35.10.4.18.12.4.9.8.2 2018/02/05 15:46:53 ltakacs Exp $

#include "MAPL_Generic.h"

module GEOS_IrradGridCompMod

!BOP
! !MODULE: GEOS_Irrad -- A Module to compute longwaves radiative transfer through a cloudy atmosphere

! !DESCRIPTION:
! 
!   {\tt Irrad} is a light-weight gridded component to compute longwave 
! radiative fluxes. It operates on the ESMF grid that appears in the
! gridded component passed to its {\tt Initialize} method. Unlike
! heavier gridded components, it does not enforce its own grid.
! The only restrictions are that it be a 3-dimensional grid
! in which one dimension is aligned with the vertical coordinate and
! only the horizontal dimensions are decomposed.
!
!   The radiative transfer calculation is based on M-D Chou's IRRAD routine.
! A full documentation of the code may be found in
! "A Thermal Infrared Radiation Parameterization for Atmospheric Studies"
! M.-D. Chou et al., NASA/TM-2001-104606, Vol. 19, 55 pp, 2003.
! Based on the 1996-version of the Air Force Geophysical Laboratory HITRAN data
! base (Rothman et al., 1998), the parameterization includes the absorption due
! to major gaseous absorption (water vapor, CO2 , O3 ) and most of the minor 
! trace gases (N2O, CH4 , CFC's), as well as clouds and aerosols. The thermal
! infrared spectrum is divided into nine bands and a subband. To achieve a high
! degree of accuracy and speed, various approaches of computing the transmission
! function are applied to different spectral bands and gases. The gaseous 
! transmission function is computed either using the k-distribution method or 
! the table look-up method. To include the effect of scattering due to clouds 
! and aerosols, the optical thickness is scaled by the single-scattering albedo
! and asymmetry factor. The optical thickness, the single-scattering albedo, 
! and the asymmetry factor of clouds are parameterized as functions of the ice
! and water content and the particle size.

!   All outputs are optional and are filled only if they have been
! initialized by a coupler. 
!
!   The net (+ve downward) fluxes are returned at the layer
! interfaces, which are indexed from the top of the atmosphere (L=0)
! to the surface. It also computes the sensitivity of net downward flux to 
! surface temperature and emission by the surface.
! The full transfer calculation, including the linearization w.r.t. the surface temperature,
! is done intermitently, on the component's main time step and its results are 
! kept in the internal state. Exports are refreshed each heartbeat based on the
! latest surface temperature.
!
!   Radiation should be called either before or after thos components
!    (usually SURFACE and DYNAMICS) that use its fluxes and modify
!    its inputs. If it is called before, the intemittent refresh should
!    occur during the first step of the radiation cycle, while if it
!    is called after, it should occur during the last step. The behavior
!    of the component needs to be somewhat different in these two cases
!    and so a means is provided, through the logical attribute \texttt{CALL\_LAST} in
!    configuration, of telling the component how it is being used. The 
!    default is \texttt{CALL\_LAST = "TRUE"}. 
!
!
! !USES:

  use ESMF
  use MAPL_Mod
  use GEOS_UtilsMod
  use MAPL_ShmemMod, only: MAPL_CoresPerNodeGet

  use rrtmg_lw_rad, only: rrtmg_lw  !  RRTMG Code
  use rrtmg_lw_init, only: rrtmg_lw_ini
  use parrrtm, only: ngptlw

#ifdef _CUDA
  use cudafor
  ! NOTE: USE renames are used below to prevent name clashes with
  !       CUDA copies to the GPU.
  use rad_constants, only: &
        AIB_IR_CONST=>AIB_IR, AWB_IR_CONST=>AWB_IR, &
        AIW_IR_CONST=>AIW_IR, AWW_IR_CONST=>AWW_IR, &
        AIG_IR_CONST=>AIG_IR, AWG_IR_CONST=>AWG_IR
  use irrad_constants, only: &
        XKW_CONST=>XKW, XKE_CONST=>XKE,  MW_CONST=>MW,  &
         AW_CONST=>AW,   BW_CONST=>BW,                  &
         PM_CONST=>PM,  FKW_CONST=>FKW, GKW_CONST=>GKW, &
         CB_CONST=>CB,  DCB_CONST=>DCB,                 &
        W11_CONST=>W11, W12_CONST=>W12, W13_CONST=>W13, &
        P11_CONST=>P11, P12_CONST=>P12, P13_CONST=>P13, &
        DWE_CONST=>DWE, DPE_CONST=>DPE,                 &
         C1_CONST=>C1,   C2_CONST=>C2,   C3_CONST=>C3,  &
        OO1_CONST=>OO1, OO2_CONST=>OO2, OO3_CONST=>OO3, &
        H11_CONST=>H11, H12_CONST=>H12, H13_CONST=>H13, &
        H21_CONST=>H21, H22_CONST=>H22, H23_CONST=>H23, &
        H81_CONST=>H81, H82_CONST=>H82, H83_CONST=>H83
  use irradmod, only: &
        ! Subroutines
        IRRAD, &
        ! Parameters
        NX, NO, NC, NH, &
        ! Inputs
        PLE_DEV, TA_DEV, WA_DEV, OA_DEV, TB_DEV, &
        N2O_DEV, CH4_DEV, CFC11_DEV, CFC12_DEV, CFC22_DEV, &
        FS_DEV, TG_DEV, EG_DEV, TV_DEV, EV_DEV, &
        RV_DEV, CWC_DEV, FCLD_DEV, REFF_DEV, &
        ! Aerosol inputs
        TAUA_DEV, SSAA_DEV, ASYA_DEV, &
        ! Constant arrays in global memory
         C1,  C2,  C3, &
        OO1, OO2, OO3, &
        H11, H12, H13, &
        H21, H22, H23, &
        H81, H82, H83, &
        ! Outputs
        FLXU_DEV, FLXAU_DEV, FLCU_DEV, FLAU_DEV, &
        FLXD_DEV, FLXAD_DEV, FLCD_DEV, FLAD_DEV, &
        DFDTS_DEV, SFCEM_DEV, TAUDIAG_DEV, &
        ! Constants
        XKW, XKE, MW, AW, BW, PM, FKW, &
        GKW, AIB_IR, AWB_IR, AIW_IR, AWW_IR, AIG_IR, AWG_IR, &
        CB, DCB, W11, W12, W13, P11, P12, &
        P13, DWE, DPE
#else
  use irradmod, only: IRRAD
#endif
  
  implicit none
  private

! !PUBLIC MEMBER FUNCTIONS:

  public SetServices

!EOP

   contains

!BOP
! !IROUTINE: SetServices -- Sets ESMF services for this component

! !INTERFACE:
  subroutine SetServices ( GC, RC )

! !ARGUMENTS:
    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
    integer, optional                  :: RC  ! return code

! !DESCRIPTION: This version uses the MAPL\_GenericSetServices. This function sets
!                the Initialize and Finalize services, as well as allocating
!   our instance of a generic state and putting it in the 
!   gridded component (GC). Here we only need to set the run method and
!   add the state variable specifications (also generic) to our instance
!   of the generic state. This is the way our true state variables get into
!   the ESMF\_State INTERNAL, which is in the MAPL\_MetaComp.

!EOP

!=============================================================================
!
! ErrLog Variables


    character(len=ESMF_MAXSTR)              :: IAm
    integer                                 :: STATUS
    character(len=ESMF_MAXSTR)              :: COMP_NAME

! Local derived type aliases

    type (ESMF_Config          )            :: CF

    integer      :: MY_STEP
    integer      :: ACCUMINT
    real         :: DT

!=============================================================================

! Begin...

! Get my name and set-up traceback handle
! ---------------------------------------

    Iam = 'SetServices'
    call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // Iam

! Set the Run entry point
! -----------------------

    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN, Run, RC=STATUS)
    VERIFY_(STATUS)

! Get the configuration
! ---------------------

    call ESMF_GridCompGet( GC, CONFIG = CF, RC=STATUS )
    VERIFY_(STATUS)

! Get the intervals; "heartbeat" must exist
! -----------------------------------------

    call ESMF_ConfigGetAttribute( CF, DT, Label="RUN_DT:"                          , RC=STATUS)
    VERIFY_(STATUS)

! Refresh interval defaults to heartbeat. This will also be read by
!  MAPL_Generic and set as the component's main time step.
! -----------------------------------------------------------------

    call ESMF_ConfigGetAttribute( CF, DT, Label=trim(COMP_NAME)//"_DT:", default=DT, RC=STATUS)
    VERIFY_(STATUS)

    MY_STEP = nint(DT)

! Averaging interval defaults to the refresh interval.
!-----------------------------------------------------

    call ESMF_ConfigGetAttribute(CF, DT, Label=trim(COMP_NAME)//'Avrg:', default=DT, RC=STATUS)
    VERIFY_(STATUS)

    ACCUMINT = nint(DT)

! Set the state variable specs.
! -----------------------------

!BOS

!  !IMPORT STATE:

     call MAPL_AddImportSpec(GC,                                  &
        SHORT_NAME         = 'PLE',                               &
        LONG_NAME          = 'air_pressure',                      &
        UNITS              = 'Pa',                                &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationEdge,                  &
        AVERAGING_INTERVAL = ACCUMINT,                            &
        REFRESH_INTERVAL   = MY_STEP,                             &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                  &
        SHORT_NAME         = 'T',                                 &
        LONG_NAME          = 'air_temperature',                   &
        UNITS              = 'K',                                 &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
        AVERAGING_INTERVAL = ACCUMINT,                            &
        REFRESH_INTERVAL   = MY_STEP,                             &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                  &
        SHORT_NAME         = 'QV',                                &
        LONG_NAME          = 'specific_humidity',                 &
        UNITS              = 'kg kg-1',                           &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
        AVERAGING_INTERVAL = ACCUMINT,                            &
        REFRESH_INTERVAL   = MY_STEP,                             &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                  &
        SHORT_NAME         = 'QL',                                &
        LONG_NAME          = 'mass_fraction_of_cloud_liquid_water_in_air', &
        UNITS              = 'kg kg-1',                           &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
        AVERAGING_INTERVAL = ACCUMINT,                            &
        REFRESH_INTERVAL   = MY_STEP,                             &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                  &
        SHORT_NAME         = 'QI',                                &
        LONG_NAME          = 'mass_fraction_of_cloud_ice_in_air', &
        UNITS              = 'kg kg-1',                           &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
        AVERAGING_INTERVAL = ACCUMINT,                            &
        REFRESH_INTERVAL   = MY_STEP,                             &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                  &
        SHORT_NAME         = 'QR',                                &
        LONG_NAME          = 'mass_fraction_of_rain_water_in_air',&
        UNITS              = 'kg kg-1',                           &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
        AVERAGING_INTERVAL = ACCUMINT,                            &
        REFRESH_INTERVAL   = MY_STEP,                             &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                  &
        SHORT_NAME         = 'QS',                                &
        LONG_NAME          = 'mass_fraction_of_snow_in_air',      &
        UNITS              = 'kg kg-1',                           &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
        AVERAGING_INTERVAL = ACCUMINT,                            &
        REFRESH_INTERVAL   = MY_STEP,                             &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                  &
        SHORT_NAME         = 'RL',                                &
        LONG_NAME          = 'effective_radius_of_cloud_liquid_water_particles',      &
        UNITS              = 'm',                                 &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
        AVERAGING_INTERVAL = ACCUMINT,                            &
        REFRESH_INTERVAL   = MY_STEP,                             &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                  &
        SHORT_NAME         = 'RI',                                &
        LONG_NAME          = 'effective_radius_of_cloud_ice_particles',   &
        UNITS              = 'm',                                 &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
        AVERAGING_INTERVAL = ACCUMINT,                            &
        REFRESH_INTERVAL   = MY_STEP,                             &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                  &
        SHORT_NAME         = 'RR',                                &
        LONG_NAME          = 'effective_radius_of_rain_particles',&
        UNITS              = 'm',                                 &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
        AVERAGING_INTERVAL = ACCUMINT,                            &
        REFRESH_INTERVAL   = MY_STEP,                             &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                  &
        SHORT_NAME         = 'RS',                                &
        LONG_NAME          = 'effective_radius_of_snow_particles',&
        UNITS              = 'm',                                 &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
        AVERAGING_INTERVAL = ACCUMINT,                            &
        REFRESH_INTERVAL   = MY_STEP,                             &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                  &
        SHORT_NAME         = 'O3',                                &
        LONG_NAME          = 'ozone_mass_mixing_ratio',           &
        UNITS              = 'kg kg-1',                           &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
        AVERAGING_INTERVAL = ACCUMINT,                            &
        REFRESH_INTERVAL   = MY_STEP,                             &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                  &
        SHORT_NAME         = 'CH4',                               &
        LONG_NAME          = 'methane_concentration',             &
        UNITS              = 'pppv',                              &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
        AVERAGING_INTERVAL = ACCUMINT,                            &
        REFRESH_INTERVAL   = MY_STEP,                             &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                  &
        SHORT_NAME         = 'N2O',                               &
        LONG_NAME          = 'nitrous_oxide_concentration',       &
        UNITS              = 'pppv',                              &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
        AVERAGING_INTERVAL = ACCUMINT,                            &
        REFRESH_INTERVAL   = MY_STEP,                             &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                  &
        SHORT_NAME         = 'CFC11',                             &
        LONG_NAME          = 'CFC11_concentration',               &
        UNITS              = 'pppv',                              &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
        AVERAGING_INTERVAL = ACCUMINT,                            &
        REFRESH_INTERVAL   = MY_STEP,                             &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                  &
        SHORT_NAME         = 'CFC12',                             &
        LONG_NAME          = 'CFC12_concentration',               &
        UNITS              = 'pppv',                              &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
        AVERAGING_INTERVAL = ACCUMINT,                            &
        REFRESH_INTERVAL   = MY_STEP,                             &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                  &
        SHORT_NAME         = 'HCFC22',                            &
        LONG_NAME          = 'HCFC22_concentration',              &
        UNITS              = 'pppv',                              &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
        AVERAGING_INTERVAL = ACCUMINT,                            &
        REFRESH_INTERVAL   = MY_STEP,                             &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                  &
        SHORT_NAME         = 'FCLD',                              &
        LONG_NAME          = 'cloud_area_fraction_in_atmosphere_layer', &
        UNITS              = '1',                                 &
        DIMS               = MAPL_DimsHorzVert,                   &
        VLOCATION          = MAPL_VLocationCenter,                &
        AVERAGING_INTERVAL = ACCUMINT,                            &
        REFRESH_INTERVAL   = MY_STEP,                             &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                  &
        SHORT_NAME         = 'TS',                                &
        LONG_NAME          = 'surface_skin_temperature',          &
        UNITS              = 'K',                                 &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        AVERAGING_INTERVAL = ACCUMINT,                            &
        REFRESH_INTERVAL   = MY_STEP,                             &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                  &
        SHORT_NAME         = 'EMIS',                              &
        LONG_NAME          = 'surface_emissivity',                &
        UNITS              = '1',                                 &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        AVERAGING_INTERVAL = ACCUMINT,                            &
        REFRESH_INTERVAL   = MY_STEP,                             &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddImportSpec(GC,                                  &
        SHORT_NAME         = 'PREF',                              &
        LONG_NAME          = 'reference_air_pressure',            &
        UNITS              = 'Pa',                                &
        DIMS               = MAPL_DimsVertOnly,                   &
        VLOCATION          = MAPL_VLocationEdge,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

! Instantaneous TS is used only for updating the IR fluxes due to TS change

     call MAPL_AddImportSpec(GC,                                  &
        SHORT_NAME         = 'TSINST',                            &
        LONG_NAME          = 'surface_skin_temperature',          &
        UNITS              = 'K',                                 &
        DIMS               = MAPL_DimsHorzOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)


    call MAPL_AddImportSpec(GC,                                   &
       LONG_NAME  = 'aerosols',                                   &
       UNITS      = 'kg kg-1',                                    &
       SHORT_NAME = 'AERO',                                       &
       DIMS       = MAPL_DimsHorzVert,                            &
       VLOCATION  = MAPL_VLocationCenter,                         &
       DATATYPE   = MAPL_StateItem,                               &
       RESTART    = MAPL_RestartSkip,                             &
                                                       RC=STATUS  )
    VERIFY_(STATUS)

!  !EXPORT STATE:

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'FLX',                                       &
        LONG_NAME  = 'net_downward_longwave_flux_in_air',         &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzVert,                           &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'FLXA',                                      &
        LONG_NAME  = 'net_downward_longwave_flux_in_air_and_no_aerosol', &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzVert,                           &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'FLXD',                                      &
        LONG_NAME  = 'downward_longwave_flux_in_air',             &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzVert,                           &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'FLXAD',                                     &
        LONG_NAME  = 'downward_longwave_flux_in_air_and_no_aerosol', &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzVert,                           &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'FLXU',                                      &
        LONG_NAME  = 'upward_longwave_flux_in_air',               &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzVert,                           &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'FLXAU',                                     &
        LONG_NAME  = 'upward_longwave_flux_in_air_and_no_aerosol',&
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzVert,                           &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'FLC',                                       &
        LONG_NAME  = 'net_downward_longwave_flux_in_air_assuming_clear_sky', &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzVert,                           &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'FLCD',                                      &
        LONG_NAME  = 'downward_longwave_flux_in_air_assuming_clear_sky', &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzVert,                           &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)
    
    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'FLCU',                                      &
        LONG_NAME  = 'upward_longwave_flux_in_air_assuming_clear_sky', &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzVert,                           &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'FLA',                                       &
        LONG_NAME  = 'net_downward_longwave_flux_in_air_assuming_clear_sky_and_no_aerosol',&
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzVert,                           &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'FLAD',                                      &
        LONG_NAME  = 'downward_longwave_flux_in_air_assuming_clear_sky_and_no_aerosol', &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzVert,                           &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'FLAU',                                      &
        LONG_NAME  = 'upward_longwave_flux_in_air_assuming_clear_sky_and_no_aerosol', &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzVert,                           &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'SFCEM',                                     &
        LONG_NAME  = 'longwave_flux_emitted_from_surface',        &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'SFCEM0',                                    &
        LONG_NAME  = 'longwave_flux_emitted_from_surface_at_reference_time',&
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'LWS0',                                      &
        LONG_NAME  = 'surface_absorbed_longwave_radiation_at_reference_time',&
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'DSFDTS',                                    &
        LONG_NAME  = 'sensitivity_of_longwave_flux_emitted_from_surface_to_surface_temperature', &
        UNITS      = 'W m-2 K-1',                                 &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'DSFDTS0',                                   &
        LONG_NAME  = 'sensitivity_of_longwave_flux_emitted_from_surface_to_surface_temperature_at_reference_time', &
        UNITS      = 'W m-2 K-1',                                 &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'TSREFF',                                    &
        LONG_NAME  = 'surface_temperature',                       &
        UNITS      = 'K',                                         &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'OLR',                                       &
        LONG_NAME  = 'upwelling_longwave_flux_at_toa',            &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'OLRA',                                      &
        LONG_NAME  = 'upwelling_longwave_flux_at_toa_and_no_aerosol', &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'OLC',                                       &
        LONG_NAME  = 'upwelling_longwave_flux_at_toa_assuming_clear_sky',&
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'OLCC5',                                     &
        LONG_NAME  = 'upwelling_longwave_flux_at_toa_assuming_clear_sky_masked_using_cldtt_LE_5',&
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'OLA',                                       &
        LONG_NAME  = 'upwelling_longwave_flux_at_toa_assuming_clear_sky_and_no_aerosol',&
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'FLNS',                                      &
        LONG_NAME  = 'surface_net_downward_longwave_flux',        &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'FLNSNA',                                    &
        LONG_NAME  = 'surface_net_downward_longwave_flux_and_no_aerosol', &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'FLNSC',                                     &
        LONG_NAME  = 'surface_net_downward_longwave_flux_assuming_clear_sky',&
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'FLNSA',                                     &
        LONG_NAME  = 'surface_net_downward_longwave_flux_assuming_clear_sky_and_no_aerosol',&
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'LWS',                                       &
        LONG_NAME  = 'surface_absorbed_longwave_radiation',       &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'LWSA',                                      &
        LONG_NAME  = 'surface_absorbed_longwave_radiation_and_no_aerosol', &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'LCS',                                       &
        LONG_NAME  = 'surface_absorbed_longwave_radiation_assuming_clear_sky',&
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'LCSC5',                                     &
        LONG_NAME  = 'surface_absorbed_longwave_radiation_assuming_clear_sky_masked_using_cldtt_LE_5',&
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'LAS',                                       &
        LONG_NAME  = 'surface_absorbed_longwave_radiation_assuming_clear_sky_and_no_aerosol',&
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'CLDTMP',                                    &
        LONG_NAME  = 'cloud_top_temperature',                     &
        UNITS      = 'K',                                         &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'CLDPRS',                                    &
        LONG_NAME  = 'cloud_top_pressure',                        &
        UNITS      = 'Pa',                                        &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'TAUIR',                                     &
        LONG_NAME  = 'longwave_cloud_optical_thickness_at_800_cm-1',&
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzVert,                           &
        VLOCATION  = MAPL_VLocationCenter,             RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'CLDTT'  ,                                   &
        LONG_NAME  = 'total_2D_cloud_area_fraction',              &
        UNITS      = '1',                                         &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'CLDTTLW',                                   &
        LONG_NAME  = 'total_cloud_area_fraction_rrtmg_lw',        &
        UNITS      = '1',                                         &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'CLDHILW',                                   &
        LONG_NAME  = 'total_hi-level_cloud_area_fraction_rrtmg_lw',     &
        UNITS      = '1',                                         &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'CLDMDLW',                                   &
        LONG_NAME  = 'total_mid-level_cloud_area_fraction_rrtmg_lw',    &
        UNITS      = '1',                                         &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                   &
        SHORT_NAME = 'CLDLOLW',                                   &
        LONG_NAME  = 'total_low_level_cloud_area_fraction_rrtmg_lw',    &
        UNITS      = '1',                                         &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

!  Irrad does not have a "real" internal state. To update the net_longwave_flux
!  due to the change of surface temperature every time step, we keep 
!  several variables in the internal state.

!  !INTERNAL STATE:

    call MAPL_AddInternalSpec(GC,                                 &
        SHORT_NAME = 'FLX',                                       &
        LONG_NAME  = 'net_downward_longwave_flux_in_air',         &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzVert,                           &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                 &
        SHORT_NAME = 'FLC',                                       &
        LONG_NAME  = 'net_downward_longwave_flux_in_air_for_clear_sky(INTERNAL)',  &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzVert,                           &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                 &
        SHORT_NAME = 'FLA',                                       &
        LONG_NAME  = 'net_downward_longwave_flux_in_air_for_clear_sky_and_no_aerosol',  &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzVert,                           &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                 &
        SHORT_NAME = 'FLXD',                                      &
        LONG_NAME  = 'downward_longwave_flux_in_air',             &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzVert,                           &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                 &
        SHORT_NAME = 'FLXU',                                      &
        LONG_NAME  = 'upward_longwave_flux_in_air',               &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzVert,                           &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                    &
        SHORT_NAME = 'FLCD',                                         &
        LONG_NAME  = 'downward_longwave_flux_in_air_for_clear_sky',  &
        UNITS      = 'W m-2',                                        &
        DIMS       = MAPL_DimsHorzVert,                              &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                    &
        SHORT_NAME = 'FLCU',                                         &
        LONG_NAME  = 'upward_longwave_flux_in_air_for_clear_sky',    &
        UNITS      = 'W m-2',                                        &
        DIMS       = MAPL_DimsHorzVert,                              &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                                   &
        SHORT_NAME = 'FLAD',                                                        &
        LONG_NAME  = 'downward_longwave_flux_in_air_for_clear_sky_and_no_aerosol',  &
        UNITS      = 'W m-2',                                                       &
        DIMS       = MAPL_DimsHorzVert,                                             &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                                   &
        SHORT_NAME = 'FLAU',                                                        &
        LONG_NAME  = 'upward_longwave_flux_in_air_for_clear_sky_and_no_aerosol',    &
        UNITS      = 'W m-2',                                                       &
        DIMS       = MAPL_DimsHorzVert,                                             &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                 &
        SHORT_NAME = 'DFDTS',                                     &
        LONG_NAME  = 'sensitivity_of_net_downward_longwave_flux_in_air_to_surface_temperature', &
        UNITS      = 'W m-2 K-1',                                 &
        DIMS       = MAPL_DimsHorzVert,                           &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                 &
        SHORT_NAME = 'DFDTSC',                                    &
        LONG_NAME  = 'sensitivity_of_net_downward_longwave_flux_in_air_to_surface_temperature_for_clear_sky',&
        UNITS      = 'W m-2 K-1',                                 &
        DIMS       = MAPL_DimsHorzVert,                           &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                 &
        SHORT_NAME = 'SFCEM',                                     &
        LONG_NAME  = 'longwave_flux_emitted_from_surface',        &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                 &
        SHORT_NAME = 'TS',                                        &
        LONG_NAME  = 'surface_temperature',                       &
        UNITS      = 'K',                                         &
        DIMS       = MAPL_DimsHorzOnly,                           &
        VLOCATION  = MAPL_VLocationNone,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                 &
        SHORT_NAME = 'FLXA',                                      &
        LONG_NAME  = 'net_downward_longwave_flux_in_air_and_no_aerosol', &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzVert,                           &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                 &
        SHORT_NAME = 'FLXAD',                                     &
        LONG_NAME  = 'downward_longwave_flux_in_air_and_no_aerosol', &
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzVert,                           &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                 &
        SHORT_NAME = 'FLXAU',                                     &
        LONG_NAME  = 'upward_longwave_flux_in_air_and_no_aerosol',&
        UNITS      = 'W m-2',                                     &
        DIMS       = MAPL_DimsHorzVert,                           &
        VLOCATION  = MAPL_VLocationEdge,               RC=STATUS  )
    VERIFY_(STATUS)


!EOS

! Set the Profiling timers
! ------------------------

    call MAPL_TimerAdd(GC,    name="-LW_DRIVER"   ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="--IRRAD"      ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="---IRRAD_RUN" ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="---IRRAD_DATA" ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="----IRRAD_DATA_DEVICE" ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="----IRRAD_DATA_CONST" ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="---IRRAD_ALLOC" ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="---IRRAD_DEALLOC" ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="--RRTMG"       ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="---RRTMG_RUN"       ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="---RRTMG_INIT"       ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="---RRTMG_FLIP"       ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="--MISC"       ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="---AEROSOLS"       ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="-UPDATE_FLX"  ,RC=STATUS)
    VERIFY_(STATUS)

! Set generic init and final methods
! ----------------------------------

    call MAPL_GenericSetServices    ( GC, RC=STATUS)
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)
  
  end subroutine SetServices

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOP
! !IROUTINE: RUN -- Run method for the LW component

! !INTERFACE:
subroutine RUN ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:
  type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
  type(ESMF_State),    intent(inout) :: IMPORT ! Import state
  type(ESMF_State),    intent(inout) :: EXPORT ! Export state
  type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
  integer, optional,   intent(  out) :: RC     ! Error code:

! !DESCRIPTION: Periodically refreshes the fluxes and their derivatives
!                w.r.t surface skin temperature. On every step it produces
!                a linear estimate of the fluxes based on the instantaneous
!                surface temperature.

!EOP


! ErrLog Variables

  character(len=ESMF_MAXSTR)         :: IAm
  integer                            :: STATUS
  character(len=ESMF_MAXSTR)         :: COMP_NAME

! Local derived type aliases

  type (MAPL_MetaComp),     pointer  :: MAPL
  type (ESMF_State)                  :: INTERNAL
  type (ESMF_Alarm)                  :: ALARM

  integer                            :: IM, JM, LM
  integer                            :: CalledLast

! Pointers to internal

   real, pointer, dimension(:,:  )   :: SFCEM_INT
   real, pointer, dimension(:,:  )   :: TS_INT
   real, pointer, dimension(:,:,:)   :: FLX_INT
   real, pointer, dimension(:,:,:)   :: FLXA_INT
   real, pointer, dimension(:,:,:)   :: FLC_INT
   real, pointer, dimension(:,:,:)   :: FLA_INT
   real, pointer, dimension(:,:,:)   :: FLXU_INT
   real, pointer, dimension(:,:,:)   :: FLXAU_INT
   real, pointer, dimension(:,:,:)   :: FLCU_INT
   real, pointer, dimension(:,:,:)   :: FLAU_INT
   real, pointer, dimension(:,:,:)   :: FLXD_INT
   real, pointer, dimension(:,:,:)   :: FLXAD_INT
   real, pointer, dimension(:,:,:)   :: FLCD_INT
   real, pointer, dimension(:,:,:)   :: FLAD_INT

   real, pointer, dimension(:,:,:)   :: DFDTS
   real, pointer, dimension(:,:,:)   :: DFDTSC

   real, external :: getco2

! Additional pointers for RRTMG

   real, pointer, dimension(:,:  )   :: LONS
   real, pointer, dimension(:,:  )   :: LATS

   type (ESMF_VM)                    :: VM
   integer                           :: CoresPerNode
   integer                           :: COMM

!=============================================================================

! Begin... 

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

   Iam = "Run"
   call ESMF_GridCompGet( GC, name=COMP_NAME, RC=STATUS )
   VERIFY_(STATUS)
   Iam = trim(COMP_NAME) // Iam

! Get my internal MAPL_Generic state
!-----------------------------------

   call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
   VERIFY_(STATUS)

   call MAPL_TimerOn(MAPL,"TOTAL")

! Get parameters from generic state. The RUNALARM is used to control
!  the calling of the full transfer calculation
!-------------------------------------------------------------------

   call MAPL_Get(MAPL,              &
         IM=IM, JM=JM, LM=LM,                    &
         LONS=LONS, LATS=LATS,                   &
         RUNALARM            = ALARM,            &
         INTERNAL_ESMF_STATE = INTERNAL,         &
                                       RC=STATUS )
   VERIFY_(STATUS)

! Get number of cores per node for RRTMG GPU
! ------------------------------------------

   call ESMF_VMGetCurrent(VM, RC=STATUS)
   VERIFY_(STATUS)

   call ESMF_VmGet(VM, mpiCommunicator=COMM, RC=STATUS)
   VERIFY_(STATUS)

   CoresPerNode = MAPL_CoresPerNodeGet(COMM,RC=STATUS)
   VERIFY_(STATUS)


! Pointers to Internals; these are needed by both Update and Refresh
!-------------------------------------------------------------------

   call MAPL_GetPointer(INTERNAL, SFCEM_INT, 'SFCEM', RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL, FLX_INT,   'FLX',   RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL, FLXA_INT,  'FLXA',  RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL, FLC_INT,   'FLC',   RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL, FLA_INT,   'FLA',   RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL, FLXU_INT,  'FLXU',  RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL, FLXAU_INT, 'FLXAU', RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL, FLCU_INT,  'FLCU',  RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL, FLAU_INT,  'FLAU',  RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL, FLXD_INT,  'FLXD',  RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL, FLXAD_INT, 'FLXAD', RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL, FLCD_INT,  'FLCD',  RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL, FLAD_INT,  'FLAD',  RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL, TS_INT,    'TS',    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL, DFDTS,     'DFDTS', RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL, DFDTSC,    'DFDTSC',RC=STATUS); VERIFY_(STATUS)

! Determine calling sequence
!---------------------------

   call MAPL_GetResource(MAPL,CalledLast,'CALLED_LAST:', default=1, RC=STATUS)
   VERIFY_(STATUS)

! Fill exported fluxed based on latest Ts
!----------------------------------------

   if(CalledLast/=0) then
      call MAPL_TimerOn(MAPL,"-UPDATE_FLX")
       call Update_Flx( IM,JM,LM, RC=STATUS )
       VERIFY_(STATUS)
      call MAPL_TimerOff(MAPL,"-UPDATE_FLX")
   endif

! If it is time, refresh internal state.
!---------------------------------------
   
   if ( ESMF_AlarmIsRinging   (ALARM, RC=STATUS) ) then
      call ESMF_AlarmRingerOff(ALARM, RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOn(MAPL,"-LW_DRIVER")
       call LW_Driver( IM,JM,LM,LATS,LONS,CoresPerNode, RC=STATUS )
       VERIFY_(STATUS)
      call MAPL_TimerOff(MAPL,"-LW_DRIVER")
 
   endif

! Fill exported fluxes based on latest Ts
!----------------------------------------

   if(CalledLast==0) then
      call MAPL_TimerOn(MAPL,"-UPDATE_FLX")
       call Update_Flx( IM,JM,LM, RC=STATUS )
       VERIFY_(STATUS)
      call MAPL_TimerOff(MAPL,"-UPDATE_FLX")
   endif

   call MAPL_TimerOff(MAPL,"TOTAL")

!  All done
!-----------

   RETURN_(ESMF_SUCCESS)

contains



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine Lw_Driver(IM,JM,LM,LATS,LONS,CoresPerNode,RC)
   integer,                   intent(IN )    :: IM, JM, LM, CoresPerNode
   real,    dimension(IM,JM), intent(IN )    :: LATS, LONS
   integer, optional,         intent(OUT)    :: RC
     
!  Locals

   character(len=ESMF_MAXSTR)        :: IAm
   integer                           :: STATUS

! local variables

   logical, parameter :: TRACE    = .true.

   integer, parameter :: NS       = 1       ! number of sub-grid surface types 

   integer, parameter :: KICE     = 1
   integer, parameter :: KLIQUID  = 2
   integer, parameter :: KRAIN    = 3
   integer, parameter :: KSNOW    = 4

   real    :: CO2

   real    :: TAUCRIT                       ! pressure separating low and middle clouds
   real    :: PRS_LOW_MID                   ! pressure separating low and middle clouds
   real    :: PRS_MID_HIGH                  ! pressure separating low and high clouds
   integer :: LCLDMH                        ! model level separating high and middle clouds
   integer :: LCLDLM                        ! model level separating low  and middle clouds

   logical :: USE_CHOU, USE_RRTMG           ! Logical concerning what radiation to use
   real    :: RRTMG_IRRAD                   ! Holds value from AGCM.rc

   logical :: USE_CHOU_SORAD, &
              USE_RRTMG_SORAD               ! Logical concerning what solar rad is being used
   real    :: RRTMG_SORAD                   ! Holds value from AGCM.rc

   integer :: NUM_BANDS                     ! Holds value from AGCM.rc
   integer :: TOTAL_RAD_BANDS

   character(len=ESMF_MAXSTR), pointer :: AEROSOLS(:)

   integer :: i, j, K, L, YY, DOY

   real, dimension (IM,JM)         :: T2M   !  fractional cover of sub-grid regions
   real, dimension (IM,JM,NS)      :: FS    !  fractional cover of sub-grid regions
   real, dimension (IM,JM,NS)      :: TG    !  land or ocean surface temperature
   real, dimension (IM,JM,NS,10)   :: EG    !  land or ocean surface emissivity
   real, dimension (IM,JM,NS)      :: TV    !  vegetation temperature
   real, dimension (IM,JM,NS,10)   :: EV    !  vegetation emissivity
   real, dimension (IM,JM,NS,10)   :: RV    !  vegetation reflectivity
   real, dimension (IM,JM,LM,4)    :: CWC   !  cloud water mixing ratio
   real, dimension (IM,JM,LM,4)    :: REFF  !  effective radius of cloud particles
   real, dimension (IM,JM,LM,10)   :: TAUDIAG
   real, dimension (IM,JM,LM)      :: RH, PL

! Local Aerosol Variables
! -----------------------

   REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: TAUA
   REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: SSAA
   REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: ASYA

   REAL :: X
   INTEGER :: IB, NA

   integer, parameter :: NB_CHOU  = 10       ! Number of bands in IRRAD calcs for Chou
   integer, parameter :: NB_RRTMG = 16       ! Number of bands in IRRAD calcs for RRTMG

   integer, parameter :: NB_CHOU_SORAD  = 8  ! Number of bands in IRRAD calcs for Chou
   integer, parameter :: NB_RRTMG_SORAD = 14 ! Number of bands in IRRAD calcs for RRTMG

   integer            :: NB_IRRAD            ! Number of bands in IRRAD calcs

   INTEGER :: OFFSET

! AERO state variables
! --------------------
   type (ESMF_State)                    :: AERO
   type (ESMF_Field)                    :: AS_FIELD
   character(len=ESMF_MAXSTR)           :: AS_FIELD_NAME   
   type (ESMF_Field)                    :: AS_FIELD_Q
   integer                              :: AS_STATUS
   real, pointer,     dimension(:,:,:)  :: AS_PTR_3D
   real, pointer,     dimension(:,:,:)  :: AS_PTR_PLE
   real, pointer,     dimension(:,:,:)  :: AS_PTR_T
   real, pointer,     dimension(:,:,:)  :: AS_PTR_Q
   real, allocatable, dimension(:,:,:)  :: AS_ARR_RH
   real, allocatable, dimension(:,:,:)  :: AS_ARR_PL

   real, allocatable, dimension(:,:,:,:):: AEROSOL_EXT
   real, allocatable, dimension(:,:,:,:):: AEROSOL_SSA
   real, allocatable, dimension(:,:,:,:):: AEROSOL_ASY

   real, pointer,     dimension(:,:,:)  :: VAR_PTR_3D

   logical                              :: implements_aerosol_optics 

   integer                              :: band

! Variables for RRTMG Code
! ------------------------

   integer :: icld      ! Cloud overlap method
                        !    0: Clear only
                        !    1: Random
                        !    2: Maximum/random
                        !    3: Maximum
   integer :: inflglw         ! Flag for cloud optical properties
   integer :: iceflglw        ! Flag for ice particle specification
   integer :: liqflglw        ! Flag for liquid droplet specification
   integer :: idrv            ! Flux for derivative calculation: 0=no derivative, 1=yes derivative
   integer :: NN, IJ, LV

   real,    allocatable, dimension(:,:)   :: FCLD_R
   real,    allocatable, dimension(:,:)   :: TLEV_R       ! Edge Level temperature
   real,    allocatable, dimension(:,:)   :: PLE_R        ! Reverse of level pressure
   real,    allocatable, dimension(:,:)   :: ZL_R         ! Reverse of level height
   real,    allocatable, dimension(:,:)   :: EMISS        ! Surface emissivity at 16 RRTMG bands
   real,    allocatable, dimension(:,:)   :: CLIQWP       ! Cloud liquid water path 
   real,    allocatable, dimension(:,:)   :: CICEWP       ! Cloud ice water path 
   real,    allocatable, dimension(:,:)   :: RELIQ        ! Cloud liquid effective radius 
   real,    allocatable, dimension(:,:)   :: REICE        ! Cloud ice effective radius 
   real,    allocatable, dimension(:,:,:) :: TAUCLD
   real,    allocatable, dimension(:,:,:) :: TAUAER
   real,    allocatable, dimension(:,:)   :: PL_R, T_R,  Q_R, O2_R,  O3_R
   real,    allocatable, dimension(:,:)   :: CO2_R, CH4_R, N2O_R, CFC11_R, CFC12_R, CFC22_R, CCL4_R
   real,    allocatable, dimension(:)     :: TSFC
   real,    allocatable, dimension(:,:)   :: UFLX, DFLX, UFLXC, DFLXC, DUFLX_DT, DUFLXC_DT
   real,    allocatable, dimension(:,:)   :: HR, HRC
   integer, allocatable, dimension(:,:)   :: CLOUDFLAG
   real,    allocatable, dimension(:)     :: ALAT

   real, parameter :: O2   = 0.2090029E+00
   real, parameter :: CCL4 = 0.1105000E-09

! For aerosol
   integer                    :: in
   real                       :: xx
   type (ESMF_Time)           :: CURRENTTIME
   real, dimension (LM+1)     :: TLEV ! Level temperature and its reverse
   real, dimension (LM)       :: DP

! pointers to import
!-------------------

   real, pointer, dimension(:    )   :: PREF
   real, pointer, dimension(:,:  )   :: TS
   real, pointer, dimension(:,:  )   :: EMIS
   real, pointer, dimension(:,:,:)   :: PLE, T,  Q,  O3
   real, pointer, dimension(:,:,:)   :: CH4, N2O, CFC11, CFC12, HCFC22
   real, pointer, dimension(:,:,:)   :: QL,  QI, QR, QS
   real, pointer, dimension(:,:,:)   :: RI,  RL, RR, RS, FCLD
   real, pointer, dimension(:,:,:,:) :: RAERO
   real, pointer, dimension(:,:,:)   :: QAERO

! pointers to exports
!--------------------

   real, pointer, dimension(:,:  )   :: CLDPRS
   real, pointer, dimension(:,:  )   :: CLDTMP
   real, pointer, dimension(:,:,:)   :: TAUIR
   real, pointer, dimension(:,:  )   :: CLDTTLW
   real, pointer, dimension(:,:  )   :: CLDHILW
   real, pointer, dimension(:,:  )   :: CLDMDLW
   real, pointer, dimension(:,:  )   :: CLDLOLW
   real, pointer, dimension(:,:  )   :: TSREFF
   real, pointer, dimension(:,:  )   :: SFCEM
   real, pointer, dimension(:,:  )   :: LWS0 
   real, pointer, dimension(:,:  )   :: DSFDTS

#ifdef _CUDA
! MATMAT CUDA Variables
   type(dim3) :: Grid, Block
   integer :: blocksize
#endif

!  Begin...
!----------

   IAm = "Lw_Driver"
   call MAPL_TimerOn(MAPL,"--MISC")

! Pointer to Imports used only for full transfer calculation
!-----------------------------------------------------------
   
   call MAPL_GetPointer(IMPORT, PLE,    'PLE',    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT, T,      'T',      RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT, Q,      'QV',     RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT, QL,     'QL',     RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT, QI,     'QI',     RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT, QR,     'QR',     RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT, QS,     'QS',     RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT, RL,     'RL',     RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT, RI,     'RI',     RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT, RR,     'RR',     RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT, RS,     'RS',     RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT, O3,     'O3',     RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT, CH4,    'CH4',    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT, N2O,    'N2O',    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT, CFC11,  'CFC11',  RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT, CFC12,  'CFC12',  RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT, HCFC22, 'HCFC22', RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT, FCLD,   'FCLD',   RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT, EMIS,   'EMIS',   RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT, PREF,   'PREF',   RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(IMPORT, TS,     'TS',     RC=STATUS); VERIFY_(STATUS)

   PL = 0.5*(PLE(:,:,:UBOUND(PLE,3)-1)+PLE(:,:,LBOUND(PLE,3)+1:))
   RH = Q/GEOS_QSAT(T,PL,PASCALS=.true.)

! Get trace gases concentrations by volume (pppv) from configuration
!-------------------------------------------------------------------

   call MAPL_GetResource( MAPL, CO2, 'CO2:', RC=STATUS)
   VERIFY_(STATUS)

   if(CO2<0.0) then
      call ESMF_ClockGet(CLOCK, currTIME=CURRENTTIME, RC=STATUS)
      VERIFY_(STATUS)
      call ESMF_TimeGet (CURRENTTIME, YY=YY, DayOfYear=DOY, RC=STATUS)
      VERIFY_(STATUS)
      CO2 = GETCO2(YY,DOY)
   endif

   call MAPL_GetResource( MAPL, PRS_LOW_MID,  'PRS_LOW_MID_CLOUDS:',  DEFAULT=70000., &
        RC=STATUS)
   VERIFY_(STATUS)

   call MAPL_GetResource( MAPL, PRS_MID_HIGH, 'PRS_MID_HIGH_CLOUDS:', DEFAULT=40000., &
        RC=STATUS)
   VERIFY_(STATUS)

! Test if we are using RRTMG
!---------------------------

   call MAPL_GetResource( MAPL, RRTMG_IRRAD ,'USE_RRTMG_IRRAD:', DEFAULT=0.0, RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetResource( MAPL, RRTMG_SORAD ,'USE_RRTMG_SORAD:', DEFAULT=0.0, RC=STATUS)
   VERIFY_(STATUS)

   USE_RRTMG = RRTMG_IRRAD /= 0.0
   USE_CHOU  = .not.USE_RRTMG

   USE_RRTMG_SORAD = RRTMG_SORAD /= 0.0
   USE_CHOU_SORAD  = .not.USE_RRTMG_SORAD

   ! Set the offset for the IRRAD aerosol bands
   if (USE_RRTMG_SORAD) then
      OFFSET = NB_RRTMG_SORAD 
   else
      OFFSET = NB_CHOU_SORAD
   end if

   ! Set NB_IRRAD so we can use the same AEROSOL opt
   ! code
   if (USE_RRTMG) then
      NB_IRRAD = NB_RRTMG
   else
      NB_IRRAD = NB_CHOU
   end if

! Test to see if AGCM.rc is set up correctly for the Radiation selected
!----------------------------------------------------------------------

   call MAPL_GetResource( MAPL, NUM_BANDS ,'NUM_BANDS:', RC=STATUS)
   VERIFY_(STATUS)

   if (USE_RRTMG .and. USE_RRTMG_SORAD) TOTAL_RAD_BANDS = NB_RRTMG + NB_RRTMG_SORAD
   if (USE_RRTMG .and. USE_CHOU_SORAD ) TOTAL_RAD_BANDS = NB_RRTMG + NB_CHOU_SORAD
   if (USE_CHOU  .and. USE_RRTMG_SORAD) TOTAL_RAD_BANDS = NB_CHOU  + NB_RRTMG_SORAD
   if (USE_CHOU  .and. USE_CHOU_SORAD ) TOTAL_RAD_BANDS = NB_CHOU  + NB_CHOU_SORAD

   if (NUM_BANDS /= TOTAL_RAD_BANDS) then
      if (MAPL_am_I_Root()) then
         write (*,*) "NUM_BANDS is not set up correctly for the radiation combination selected:"
         write (*,*) "    IRRAD RRTMG: ", USE_RRTMG
         write (*,*) "    SORAD RRTMG: ", USE_RRTMG_SORAD
         write (*,*) "Please check that your optics tables and NUM_BANDS are correct."
      end if
      ASSERT_(.FALSE.)
   end if

! Compute surface air temperature ("2 m") adiabatically
!------------------------------------------------------ 

   T2M = T(:,:,LM)*(0.5*(1.0 + PLE(:,:,LM-1)/PLE(:,:,LM)))**(-MAPL_KAPPA)

! For now, use the same emissivity for all bands
!-----------------------------------------------

   do K = 1, 10
      EG(:,:,1,K)   = EMIS(:,:)
   end do

! For now, hardwire vegetation and aerosol parameters
!----------------------------------------------------

   FS                  = 1.0
   TG(:,:,1)           = TS
   TV(:,:,1)           = TS
   EV                  = 0.0
   RV                  = 0.0

! Copy cloud constituent properties into contiguous buffers 
!----------------------------------------------------------

   CWC (:,:,:,KICE   ) = QI
   CWC (:,:,:,KLIQUID) = QL
   CWC (:,:,:,KRAIN  ) = QR
   CWC (:,:,:,KSNOW  ) = QS

   REFF(:,:,:,KICE   ) = RI * 1.0e6
   REFF(:,:,:,KLIQUID) = RL * 1.0e6
   REFF(:,:,:,KRAIN  ) = RR * 1.0e6
   REFF(:,:,:,KSNOW  ) = RS * 1.0e6

! Determine the model level seperating high and middle clouds
!------------------------------------------------------------

   LCLDMH = 1
   do K = 1, LM
      if( PREF(K) >= PRS_MID_HIGH ) then
         LCLDMH = K
         exit
      end if
   end do

! Determine the model level seperating low and middle clouds
!-----------------------------------------------------------

   LCLDLM = LM
   do K = 1, LM
      if( PREF(K) >= PRS_LOW_MID  ) then
         LCLDLM = K
         exit
      end if
   end do

   call MAPL_GetPointer(EXPORT, CLDTTLW, 'CLDTTLW', RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT, CLDHILW, 'CLDHILW', RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT, CLDMDLW, 'CLDMDLW', RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT, CLDLOLW, 'CLDLOLW', RC=STATUS); VERIFY_(STATUS)

   ! ------------------
   ! Begin aerosol code
   ! ------------------

   ! Allocate per-band aerosol arrays
   ! --------------------------------

   ALLOCATE(TAUA(IM,JM,LM,NB_IRRAD), STAT = STATUS)
   VERIFY_(STATUS)
   ALLOCATE(SSAA(IM,JM,LM,NB_IRRAD), STAT = STATUS)
   VERIFY_(STATUS)
   ALLOCATE(ASYA(IM,JM,LM,NB_IRRAD), STAT = STATUS)
   VERIFY_(STATUS)

   ! Zero out aerosol arrays. If NA == 0, these zeroes are then used inside IRRAD.
   ! -----------------------------------------------------------------------------
   NA    = 0

   TAUA  = 0.0
   SSAA  = 0.0
   ASYA  = 0.0

   ! If we have aerosols, accumulate the arrays
   ! ------------------------------------------

   call MAPL_TimerOn(MAPL,"---AEROSOLS")

   call ESMF_StateGet(IMPORT, 'AERO', AERO, RC=STATUS)
   VERIFY_(STATUS)

   call ESMF_AttributeGet(aero, name='implements_aerosol_optics_method', &
                                value=implements_aerosol_optics, RC=STATUS)
   VERIFY_(STATUS)

   RADIATEVELY_ACTIVE_AEROSOLS: if (implements_aerosol_optics) then

      ! set RH for aerosol optics
      call ESMF_AttributeGet(AERO, name='relative_humidity_for_aerosol_optics', value=AS_FIELD_NAME, RC=STATUS)
      VERIFY_(STATUS)

      if (AS_FIELD_NAME /= '') then
         call MAPL_GetPointer(AERO, AS_PTR_3D, trim(AS_FIELD_NAME), RC=STATUS)
         VERIFY_(STATUS)

         AS_PTR_3D = RH
      end if

      ! set PLE for aerosol optics
      call ESMF_AttributeGet(AERO, name='air_pressure_for_aerosol_optics', value=AS_FIELD_NAME, RC=STATUS)
      VERIFY_(STATUS)

      if (AS_FIELD_NAME /= '') then
         call MAPL_GetPointer(AERO, AS_PTR_3D, trim(AS_FIELD_NAME), RC=STATUS)
         VERIFY_(STATUS)
           
         AS_PTR_3D = PLE
      end if

      ! allocate memory for total aerosol ext, ssa and asy at all solar bands
      allocate(AEROSOL_EXT(IM,JM,LM,NB_IRRAD),  &
               AEROSOL_SSA(IM,JM,LM,NB_IRRAD),  &
               AEROSOL_ASY(IM,JM,LM,NB_IRRAD),  stat=STATUS)
      VERIFY_(STATUS)

      AEROSOL_EXT = 0.0
      AEROSOL_SSA = 0.0
      AEROSOL_ASY = 0.0

      ! compute aerosol optics at all solar bands
      IR_BANDS: do band = 1, NB_IRRAD
         call ESMF_AttributeSet(AERO, name='band_for_aerosol_optics', value=(OFFSET+band), RC=STATUS)
         VERIFY_(STATUS)

         ! execute the aero provider's optics method
         call ESMF_MethodExecute(AERO, label="aerosol_optics", RC=STATUS)
         VERIFY_(STATUS)

         ! EXT from AERO_PROVIDER
         call ESMF_AttributeGet(AERO, name='extinction_in_air_due_to_ambient_aerosol', value=AS_FIELD_NAME, RC=STATUS)
         VERIFY_(STATUS)

         if (AS_FIELD_NAME /= '') then
            call MAPL_GetPointer(AERO, AS_PTR_3D, trim(AS_FIELD_NAME),  RC=STATUS); VERIFY_(STATUS)

            if (associated(AS_PTR_3D)) then 
               AEROSOL_EXT(:,:,:,band) = AS_PTR_3D
            end if
         end if

         ! SSA from AERO_PROVIDER
         call ESMF_AttributeGet(AERO, name='single_scattering_albedo_of_ambient_aerosol', value=AS_FIELD_NAME, RC=STATUS)
         VERIFY_(STATUS)

         if (AS_FIELD_NAME /= '') then
            call MAPL_GetPointer(AERO, AS_PTR_3D, trim(AS_FIELD_NAME),  RC=STATUS); VERIFY_(STATUS)

            if (associated(AS_PTR_3D)) then 
               AEROSOL_SSA(:,:,:,band) = AS_PTR_3D
            end if
         end if

         ! ASY from AERO_PROVIDER
         call ESMF_AttributeGet(AERO, name='asymmetry_parameter_of_ambient_aerosol', value=AS_FIELD_NAME, RC=STATUS)
         VERIFY_(STATUS)

         if (AS_FIELD_NAME /= '') then
            call MAPL_GetPointer(AERO, AS_PTR_3D, trim(AS_FIELD_NAME),  RC=STATUS)
            VERIFY_(STATUS)

            if (associated(AS_PTR_3D)) then 
               AEROSOL_ASY(:,:,:,band) = AS_PTR_3D
            end if
         end if
      end do IR_BANDS

      NA = 3

      TAUA = AEROSOL_EXT
      SSAA = AEROSOL_SSA
      ASYA = AEROSOL_ASY

      deallocate(AEROSOL_EXT, __STAT__)
      deallocate(AEROSOL_SSA, __STAT__)
      deallocate(AEROSOL_ASY, __STAT__)

   end if RADIATEVELY_ACTIVE_AEROSOLS

   call MAPL_TimerOff(MAPL,"---AEROSOLS")

   call MAPL_TimerOff(MAPL,"--MISC")

   SCHEME: if (USE_CHOU) then

      call MAPL_TimerOn (MAPL,"--IRRAD",RC=STATUS)
      VERIFY_(STATUS)

#ifdef _CUDA

      call MAPL_GetResource(MAPL,BLOCKSIZE,'BLOCKSIZE:',DEFAULT=128,RC=STATUS)
      VERIFY_(STATUS)

      Block = dim3(blocksize,1,1)
      Grid = dim3(ceiling(real(IM*JM)/real(blocksize)),1,1)

      ASSERT_(LM <= GPU_MAXLEVS) ! If this is tripped, ESMA_arch.mk must be edited.

      ASSERT_(NS == MAXNS) ! If this is tripped, the local GNUmakefile
                           ! must be edited.

      call MAPL_TimerOn(MAPL,"---IRRAD_ALLOC",RC=STATUS)
      VERIFY_(STATUS)

      ! ----------------------
      ! Allocate device arrays
      ! ----------------------

      ! Inputs
      ! ------

      ALLOCATE(PLE_DEV(IM*JM,LM+1), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(TA_DEV(IM*JM,LM), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(WA_DEV(IM*JM,LM), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(OA_DEV(IM*JM,LM), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(TB_DEV(IM*JM), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(N2O_DEV(IM*JM,LM), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(CH4_DEV(IM*JM,LM), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(CFC11_DEV(IM*JM,LM), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(CFC12_DEV(IM*JM,LM), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(CFC22_DEV(IM*JM,LM), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(FS_DEV(IM*JM,NS), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(TG_DEV(IM*JM,NS), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(EG_DEV(IM*JM,NS,10), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(TV_DEV(IM*JM,NS), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(EV_DEV(IM*JM,NS,10), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(RV_DEV(IM*JM,NS,10), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(CWC_DEV(IM*JM,LM,4), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(FCLD_DEV(IM*JM,LM), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(REFF_DEV(IM*JM,LM,4), STAT=STATUS)
      VERIFY_(STATUS)

      ALLOCATE(TAUA_DEV(IM*JM,LM,NB_CHOU), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(SSAA_DEV(IM*JM,LM,NB_CHOU), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(ASYA_DEV(IM*JM,LM,NB_CHOU), STAT = STATUS)
      VERIFY_(STATUS)

      ! Constant arrays in global memory
      ! --------------------------------

      ALLOCATE(C1(NX,NC), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(C2(NX,NC), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(C3(NX,NC), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(OO1(NX,NO), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(OO2(NX,NO), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(OO3(NX,NO), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(H11(NX,NH), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(H12(NX,NH), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(H13(NX,NH), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(H21(NX,NH), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(H22(NX,NH), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(H23(NX,NH), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(H81(NX,NH), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(H82(NX,NH), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(H83(NX,NH), STAT=STATUS)
      VERIFY_(STATUS)

      ! Outputs
      ! -------

      ALLOCATE(FLXU_DEV(IM*JM,LM+1), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(FLXAU_DEV(IM*JM,LM+1), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(FLCU_DEV(IM*JM,LM+1), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(FLAU_DEV(IM*JM,LM+1), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(FLXD_DEV(IM*JM,LM+1), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(FLXAD_DEV(IM*JM,LM+1), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(FLCD_DEV(IM*JM,LM+1), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(FLAD_DEV(IM*JM,LM+1), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(DFDTS_DEV(IM*JM,LM+1), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(SFCEM_DEV(IM*JM), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(TAUDIAG_DEV(IM*JM,LM,10), STAT=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOff(MAPL,"---IRRAD_ALLOC",RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOn(MAPL,"---IRRAD_DATA",RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOn(MAPL,"----IRRAD_DATA_DEVICE",RC=STATUS)
      VERIFY_(STATUS)

      ! --------------------------
      ! Copy host arrays to device
      ! --------------------------

      ! Inputs
      ! ------

      STATUS = cudaMemcpy(PLE_DEV,PLE,IM*JM*(LM+1))
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(TA_DEV,T,IM*JM*LM)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(WA_DEV,Q,IM*JM*LM)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(OA_DEV,O3,IM*JM*LM)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(TB_DEV,T2M,IM*JM)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(N2O_DEV,N2O,IM*JM*LM) 
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(CH4_DEV,CH4,IM*JM*LM) 
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(CFC11_DEV,CFC11,IM*JM*LM) 
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(CFC12_DEV,CFC12,IM*JM*LM) 
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(CFC22_DEV,HCFC22,IM*JM*LM) 
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(FS_DEV,FS,IM*JM*NS) 
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(TG_DEV,TG,IM*JM*NS) 
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(EG_DEV,EG,IM*JM*NS*10) 
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(TV_DEV,TV,IM*JM*NS) 
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(EV_DEV,EV,IM*JM*NS*10) 
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(RV_DEV,RV,IM*JM*NS*10) 
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(CWC_DEV,CWC,IM*JM*LM*4) 
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(FCLD_DEV,FCLD,IM*JM*LM) 
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(REFF_DEV,REFF,IM*JM*LM*4) 
      VERIFY_(STATUS)

      STATUS = cudaMemcpy(TAUA_DEV,TAUA,IM*JM*LM*NB_CHOU)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(SSAA_DEV,SSAA,IM*JM*LM*NB_CHOU)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(ASYA_DEV,ASYA,IM*JM*LM*NB_CHOU)
      VERIFY_(STATUS)

      ! ---------------------------------------
      ! Copy Constant Arrays into Global Memory
      ! ---------------------------------------

      C1 = C1_CONST
      C2 = C2_CONST
      C3 = C3_CONST
      OO1 = OO1_CONST
      OO2 = OO2_CONST
      OO3 = OO3_CONST
      H11 = H11_CONST
      H12 = H12_CONST
      H13 = H13_CONST
      H21 = H21_CONST
      H22 = H22_CONST
      H23 = H23_CONST
      H81 = H81_CONST
      H82 = H82_CONST
      H83 = H83_CONST

      call MAPL_TimerOff(MAPL,"----IRRAD_DATA_DEVICE",RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOn(MAPL,"----IRRAD_DATA_CONST",RC=STATUS)
      VERIFY_(STATUS)

      ! --------------
      ! Copy Constants
      ! --------------

      XKW = XKW_CONST
      XKE = XKE_CONST
      MW = MW_CONST
      AW = AW_CONST
      BW = BW_CONST
      PM = PM_CONST
      FKW = FKW_CONST
      GKW = GKW_CONST
      AIB_IR = AIB_IR_CONST
      AWB_IR = AWB_IR_CONST
      AIW_IR = AIW_IR_CONST
      AWW_IR = AWW_IR_CONST
      AIG_IR = AIG_IR_CONST
      AWG_IR = AWG_IR_CONST
      CB = CB_CONST
      DCB = DCB_CONST
      W11 = W11_CONST
      W12 = W12_CONST
      W13 = W13_CONST
      P11 = P11_CONST
      P12 = P12_CONST
      P13 = P13_CONST
      DWE = DWE_CONST
      DPE = DPE_CONST

      call MAPL_TimerOff(MAPL,"----IRRAD_DATA_CONST",RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOff(MAPL,"---IRRAD_DATA",RC=STATUS)
      VERIFY_(STATUS)

      ! Do longwave calculations on a list of soundings
      !  This fills the internal state
      !------------------------------------------------
      ! Note: IRRAD wants all species in mole fraction
      ! except O3, which must be in mass mixing ratio.
      !------------------------------------------------

      call MAPL_TimerOn(MAPL,"---IRRAD_RUN",RC=STATUS)
      VERIFY_(STATUS)

      call irrad<<<Grid, Block>>>(IM*JM,LM,CO2,TRACE,NS,NA,NB_CHOU,LCLDMH,LCLDLM)

      STATUS = cudaGetLastError()
      if (STATUS /= 0) then
         write (*,*) "Error code from IRRAD kernel call: ", STATUS
         write (*,*) "Kernel call failed: ", cudaGetErrorString(STATUS)
         ASSERT_(.FALSE.)
      end if

      call MAPL_TimerOff(MAPL,"---IRRAD_RUN",RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOn(MAPL,"---IRRAD_DATA",RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOn(MAPL,"----IRRAD_DATA_DEVICE",RC=STATUS)
      VERIFY_(STATUS)

      ! Copy outputs from device
      ! ------------------------

      STATUS = cudaMemcpy(FLXU_INT,FLXU_DEV,IM*JM*(LM+1))
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(FLXAU_INT,FLXAU_DEV,IM*JM*(LM+1))
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(FLCU_INT,FLCU_DEV,IM*JM*(LM+1))
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(FLAU_INT,FLAU_DEV,IM*JM*(LM+1))
      VERIFY_(STATUS)

      STATUS = cudaMemcpy(FLXD_INT,FLXD_DEV,IM*JM*(LM+1))
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(FLXAD_INT,FLXAD_DEV,IM*JM*(LM+1))
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(FLCD_INT,FLCD_DEV,IM*JM*(LM+1))
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(FLAD_INT,FLAD_DEV,IM*JM*(LM+1))
      VERIFY_(STATUS)

      STATUS = cudaMemcpy(DFDTS,DFDTS_DEV,IM*JM*(LM+1))
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(SFCEM_INT,SFCEM_DEV,IM*JM)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(TAUDIAG,TAUDIAG_DEV,IM*JM*LM*10)
      VERIFY_(STATUS)

      call MAPL_TimerOff(MAPL,"----IRRAD_DATA_DEVICE",RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOff(MAPL,"---IRRAD_DATA",RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOn(MAPL,"---IRRAD_DEALLOC",RC=STATUS)
      VERIFY_(STATUS)

      ! ------------------------
      ! Deallocate device arrays
      ! ------------------------

      ! Inputs
      ! ------

      DEALLOCATE(PLE_DEV)
      DEALLOCATE(TA_DEV)
      DEALLOCATE(WA_DEV)
      DEALLOCATE(OA_DEV)
      DEALLOCATE(TB_DEV)
      DEALLOCATE(N2O_DEV)
      DEALLOCATE(CH4_DEV)
      DEALLOCATE(CFC11_DEV)
      DEALLOCATE(CFC12_DEV)
      DEALLOCATE(CFC22_DEV)
      DEALLOCATE(FS_DEV)
      DEALLOCATE(TG_DEV)
      DEALLOCATE(EG_DEV)
      DEALLOCATE(TV_DEV)
      DEALLOCATE(EV_DEV)
      DEALLOCATE(RV_DEV)
      DEALLOCATE(CWC_DEV)
      DEALLOCATE(FCLD_DEV)
      DEALLOCATE(REFF_DEV)

      DEALLOCATE(TAUA_DEV)
      DEALLOCATE(SSAA_DEV)
      DEALLOCATE(ASYA_DEV)

      ! Constant arrays in global memory
      ! --------------------------------

      DEALLOCATE(C1)
      DEALLOCATE(C2)
      DEALLOCATE(C3)
      DEALLOCATE(OO1)
      DEALLOCATE(OO2)
      DEALLOCATE(OO3)
      DEALLOCATE(H11)
      DEALLOCATE(H12)
      DEALLOCATE(H13)
      DEALLOCATE(H21)
      DEALLOCATE(H22)
      DEALLOCATE(H23)
      DEALLOCATE(H81)
      DEALLOCATE(H82)
      DEALLOCATE(H83)

      ! Outputs
      ! -------

      DEALLOCATE(FLXU_DEV)
      DEALLOCATE(FLXAU_DEV)
      DEALLOCATE(FLCU_DEV)
      DEALLOCATE(FLAU_DEV)
      DEALLOCATE(FLXD_DEV)
      DEALLOCATE(FLXAD_DEV)
      DEALLOCATE(FLCD_DEV)
      DEALLOCATE(FLAD_DEV)
      DEALLOCATE(DFDTS_DEV)
      DEALLOCATE(SFCEM_DEV)
      DEALLOCATE(TAUDIAG_DEV)

      call MAPL_TimerOff(MAPL,"---IRRAD_DEALLOC",RC=STATUS)
      VERIFY_(STATUS)

#else
! Do longwave calculations on a list of soundings
!  This fills the internal state
!------------------------------------------------
! Note: IRRAD wants all species in mole fraction
! except O3, which must be in mass mixing ratio.
!------------------------------------------------

      call MAPL_TimerOn(MAPL,"---IRRAD_RUN",RC=STATUS)
      VERIFY_(STATUS)

      call IRRAD( IM*JM, LM,       PLE,                           &
       T,        Q,      O3,    T2M,    CO2,                      &
       TRACE,    N2O,   CH4,    CFC11,     CFC12, HCFC22,         &
       CWC,    FCLD,  LCLDMH, LCLDLM,    REFF,                    &
       NS,       FS,     TG,    EG,     TV,        EV,    RV,     &
       NA, NB_CHOU, TAUA, SSAA, ASYA,                             &
       FLXU_INT,  FLCU_INT, FLAU_INT, FLXAU_INT,                  &
       FLXD_INT,  FLCD_INT, FLAD_INT, FLXAD_INT,                  &
       DFDTS, SFCEM_INT, TAUDIAG                                  )

      call MAPL_TimerOff(MAPL,"---IRRAD_RUN",RC=STATUS)
      VERIFY_(STATUS)

#endif

      call MAPL_TimerOff(MAPL,"--IRRAD",RC=STATUS)
      VERIFY_(STATUS)

   else if (USE_RRTMG) then

      call MAPL_TimerOn(MAPL,"--RRTMG",RC=STATUS)
      VERIFY_(STATUS)
 
      allocate(FCLD_R(IM*JM,LM),__STAT__)
      allocate(TLEV_R(IM*JM,0:LM),__STAT__)
      allocate(PLE_R(IM*JM,0:LM),__STAT__)
      allocate(ZL_R(IM*JM,LM),__STAT__)
      allocate(EMISS(IM*JM,NB_RRTMG),__STAT__)
      allocate(CLIQWP(IM*JM,LM),__STAT__)
      allocate(CICEWP(IM*JM,LM),__STAT__)
      allocate(RELIQ(IM*JM,LM),__STAT__)
      allocate(REICE(IM*JM,LM),__STAT__)
      allocate(TAUCLD(IM*JM,NB_RRTMG,LM),__STAT__)
      allocate(TAUAER(IM*JM,LM,NB_RRTMG),__STAT__)
      allocate(PL_R(IM*JM,LM),__STAT__)
      allocate(T_R(IM*JM,LM),__STAT__)
      allocate(Q_R(IM*JM,LM),__STAT__)
      allocate(O2_R(IM*JM,LM),__STAT__)
      allocate(O3_R(IM*JM,LM),__STAT__)
      allocate(CO2_R(IM*JM,LM),__STAT__)
      allocate(CH4_R(IM*JM,LM),__STAT__)
      allocate(N2O_R(IM*JM,LM),__STAT__)
      allocate(CFC11_R(IM*JM,LM),__STAT__)
      allocate(CFC12_R(IM*JM,LM),__STAT__)
      allocate(CFC22_R(IM*JM,LM),__STAT__)
      allocate(CCL4_R(IM*JM,LM),__STAT__)
      allocate(TSFC(IM*JM),__STAT__)
      allocate(UFLX(IM*JM,LM+1),__STAT__)
      allocate(DFLX(IM*JM,LM+1),__STAT__)
      allocate(UFLXC(IM*JM,LM+1),__STAT__)
      allocate(DFLXC(IM*JM,LM+1),__STAT__)
      allocate(DUFLX_DT(IM*JM,LM+1),__STAT__)
      allocate(DUFLXC_DT(IM*JM,LM+1),__STAT__)
      allocate(HR(IM*JM,LM+1),__STAT__)
      allocate(HRC(IM*JM,LM+1),__STAT__)
      allocate(CLOUDFLAG(IM*JM,4),__STAT__)
      allocate(ALAT(IM*JM),__STAT__)

      ICLD = 4
      INFLGLW = 2
      ICEFLGLW = 3
      LIQFLGLW = 1

      !  Set flag for flux derivative calculation
      IDRV = 1

      LCLDMH = LM - LCLDMH + 1
      LCLDLM = LM - LCLDLM + 1

      call MAPL_TimerOn(MAPL,"---RRTMG_FLIP",RC=STATUS)
      VERIFY_(STATUS)
 
      IJ = 0
      do j=1,jm
      do i=1,im
         IJ = IJ + 1
         TSFC(IJ)    = TS(I,J)
         EMISS(IJ,:) = EMIS(i,j)

         ALAT(IJ)    = LATS(I,J)

         DP(1) = (PLE(I,J,1)-PLE(I,J,0))
         do k = 2, LM
            DP(K) = (PLE(I,J,K)-PLE(I,J,K-1) )
            TLEV(k)=  (T(I,J,k-1)* DP(k) + T(I,J,k) * DP(k-1)) &
                     /            (DP(k-1) + DP(k))
         enddo

         TLEV(LM+1) = T2M(I,J)
         TLEV(   1) = TLEV(2)

         !  Flip vertical and convert to Real*8
         !  RRTMG is indexed bottom to top.
         !  Cloud WP and effective particle size
         !  have already been reversed.

         do k=1, LM
            LV = LM-k+1

            xx = 1.02*100*DP(LV)
            CLIQWP(IJ,k) = xx*CWC(I,J,LV,KLIQUID)
            CICEWP(IJ,k) = xx*CWC(I,J,LV,KICE)
            RELIQ (IJ,k) =   REFF(I,J,LV,KLIQUID)
            REICE (IJ,k) =   REFF(I,J,LV,KICE   )
               
            if    (LIQFLGLW.eq.0) then
               RELIQ(IJ,K) = min(max(RELIQ(IJ,K),5.0),10.0)
            elseif (LIQFLGLW.eq.1) then
               RELIQ(IJ,K) = min(max(RELIQ(IJ,K),2.5),60.0)
            endif

            if    (ICEFLGLW.eq.0) then
               REICE(IJ,K) = min(max(REICE(IJ,K),10.0),30.0)
            elseif (ICEFLGLW.eq.1) then
               REICE(IJ,K) = min(max(REICE(IJ,K),13.0),130.0)
            elseif (ICEFLGLW.eq.2) then
               REICE(IJ,K) = min(max(REICE(IJ,K), 5.0),131.0)
            elseif (ICEFLGLW.eq.3) then
               REICE(IJ,K) = min(max(REICE(IJ,K), 5.0),140.0)
            elseif (ICEFLGLW.eq.4) then
               REICE(IJ,K) = min(max(REICE(IJ,K)*2., 1.0),200.0)
            endif

            PLE_R  (IJ,k-1) = PLE(I,J,LV)/100.
            TLEV_R (IJ,k-1) = TLEV(LV+1)
            PL_R   (IJ,k) = PL(I,J,LV)/100.
            T_R    (IJ,k) = T(I,J,LV)
            Q_R    (IJ,k) = Q(I,J,LV) / (1.-Q(I,J,LV)) * (MAPL_AIRMW/MAPL_H2OMW)  ! Specific humidity to Volume Mixing Ratio
            O3_R   (IJ,k) = O3(I,J,LV) * (MAPL_AIRMW/MAPL_O3MW)  ! Mass to Volume Mixing Ratio
            CH4_R  (IJ,k) = CH4(I,J,LV)
            N2O_R  (IJ,k) = N2O(I,J,LV)
            CO2_R  (IJ,k) = CO2
            O2_R   (IJ,k) = O2
            CCL4_R (IJ,k) = CCL4
            CFC11_R(IJ,k) = CFC11(I,J,LV)
            CFC12_R(IJ,k) = CFC12(I,J,LV)
            CFC22_R(IJ,k) = HCFC22(I,J,LV)
            FCLD_R (IJ,k) = FCLD(I,J,LV)

            TAUAER(IJ,K,:) = TAUA(I,J,LV,:) - SSAA(I,J,LV,:)

            TAUCLD(IJ,:,k) = 0.0
         enddo

         PLE_R (IJ,LM) = PLE(I,J,0)/100.
         TLEV_R(IJ,LM) = TLEV(1)

         ZL_R(IJ,1) = 0. ! Assume lowest level ZL_R = 0.
         do k=2,LM
            ! dz = RT/g x dp/p
            ZL_R(IJ,k) = ZL_R(IJ,k-1)+MAPL_RGAS*TLEV_R(IJ,k)/MAPL_GRAV*(PL_R(IJ,k-1)-PL_R(IJ,k))/PLE_R(IJ,k)
         enddo
      enddo ! IM
      enddo ! JM

      call MAPL_TimerOff(MAPL,"---RRTMG_FLIP",RC=STATUS)
      VERIFY_(STATUS)
 
      call MAPL_TimerOn(MAPL,"---RRTMG_INIT",RC=STATUS)
      VERIFY_(STATUS)
 
      call RRTMG_LW_INI(1.004e3)

      call MAPL_TimerOff(MAPL,"---RRTMG_INIT",RC=STATUS)
      VERIFY_(STATUS)
 
      call MAPL_TimerOn(MAPL,"---RRTMG_RUN",RC=STATUS)
      VERIFY_(STATUS)

      call RRTMG_LW (IM*JM     ,LM    ,ICLD    ,    IDRV, &
              PL_R    ,PLE_R    ,T_R    ,TLEV_R    ,TSFC    , &
              Q_R  ,O3_R   ,CO2_R  ,CH4_R  ,N2O_R  ,O2_R, &
              CFC11_R, CFC12_R, CFC22_R, CCL4_R , EMISS, &
              INFLGLW ,ICEFLGLW, LIQFLGLW, FCLD_R  , &
              TAUCLD ,CICEWP ,CLIQWP ,REICE ,RELIQ , &
              TAUAER  , ZL_R, LCLDLM, LCLDMH, &
              UFLX, DFLX, HR, UFLXC, DFLXC, HRC, DUFLX_DT, DUFLXC_DT,&
              CLOUDFLAG, DOY, ALAT, CoresPerNode)

      call MAPL_TimerOff(MAPL,"---RRTMG_RUN",RC=STATUS)
      VERIFY_(STATUS)
 
      call MAPL_TimerOn(MAPL,"---RRTMG_FLIP",RC=STATUS)
      VERIFY_(STATUS)
 
      IJ = 0
      do j = 1, JM
      do i = 1, IM
         IJ = IJ + 1
         if(associated(CLDTTLW)) then
            CLDTTLW(I,J) = 1.0 - CLOUDFLAG(IJ,1)/float(NGPTLW)
         endif

         if(associated(CLDHILW)) then
            CLDHILW(I,J) = 1.0 - CLOUDFLAG(IJ,2)/float(NGPTLW)
         endif

         if(associated(CLDMDLW)) then
            CLDMDLW(I,J) = 1.0 - CLOUDFLAG(IJ,3)/float(NGPTLW)
         endif

         if(associated(CLDLOLW)) then
            CLDLOLW(I,J) = 1.0 - CLOUDFLAG(IJ,4)/float(NGPTLW)
         endif

         ! Flip vertical and convert to Real*4

         do k=0,LM
            LV = LM-k+1
            FLXU_INT(i,j,k) =-UFLX     (IJ,LV)
            FLXD_INT(i,j,k) = DFLX     (IJ,LV)
            FLCU_INT(i,j,k) =-UFLXC    (IJ,LV)
            FLCD_INT(i,j,k) = DFLXC    (IJ,LV)
            DFDTS   (i,j,k) =-DUFLX_DT (IJ,LV)
            DFDTSC  (i,j,k) =-DUFLXC_DT(IJ,LV)
         enddo
!mjs:  Corrected emitted at the surface to remove reflected
!      from upward. Note that emiss is the same for all bands,
!      so we use band 1 for the total flux.

         SFCEM_INT(i,j) = -UFLX(IJ,1) + DFLX(IJ,1)*(1.0-EMISS(IJ,1))
      enddo ! IM
      enddo ! JM

      call MAPL_TimerOff(MAPL,"---RRTMG_FLIP",RC=STATUS)
      VERIFY_(STATUS)

      deallocate(FCLD_R,__STAT__)
      deallocate(TLEV_R,__STAT__)
      deallocate(PLE_R,__STAT__)
      deallocate(ZL_R,__STAT__)
      deallocate(EMISS,__STAT__)
      deallocate(CLIQWP,__STAT__)
      deallocate(CICEWP,__STAT__)
      deallocate(RELIQ,__STAT__)
      deallocate(REICE,__STAT__)
      deallocate(TAUCLD,__STAT__)
      deallocate(TAUAER,__STAT__)
      deallocate(PL_R,__STAT__)
      deallocate(T_R,__STAT__)
      deallocate(Q_R,__STAT__)
      deallocate(O2_R,__STAT__)
      deallocate(O3_R,__STAT__)
      deallocate(CO2_R,__STAT__)
      deallocate(CH4_R,__STAT__)
      deallocate(N2O_R,__STAT__)
      deallocate(CFC11_R,__STAT__)
      deallocate(CFC12_R,__STAT__)
      deallocate(CFC22_R,__STAT__)
      deallocate(CCL4_R,__STAT__)
      deallocate(TSFC,__STAT__)
      deallocate(UFLX,__STAT__)
      deallocate(DFLX,__STAT__)
      deallocate(UFLXC,__STAT__)
      deallocate(DFLXC,__STAT__)
      deallocate(DUFLX_DT,__STAT__)
      deallocate(DUFLXC_DT,__STAT__)
      deallocate(HR,__STAT__)
      deallocate(HRC,__STAT__)
      deallocate(CLOUDFLAG,__STAT__)
      deallocate(ALAT,__STAT__)

      call MAPL_TimerOff(MAPL,"--RRTMG",RC=STATUS)
      VERIFY_(STATUS)
 
   else

      ! Something is wrong. We've selected neither Chou or RRTMG
      ASSERT_(.false.)

   end if SCHEME

! Sum up the U and D fluxes

   FLX_INT  = FLXD_INT  + FLXU_INT
   FLXA_INT = FLXAD_INT + FLXAU_INT
   FLC_INT  = FLCD_INT  + FLCU_INT
   FLA_INT  = FLAD_INT  + FLAU_INT
   
! Ming-Dah defines the surface emitted as positive downwards
!-----------------------------------------------------------

   SFCEM_INT = -SFCEM_INT

! Clear sky linearization w.r.t Ts not implemented
!-------------------------------------------------

   DFDTSC    = 0.0

! Save surface temperature in internal state
!-------------------------------------------

   TS_INT    = TS

! Export some cloud properties in the infrared
!---------------------------------------------

   call MAPL_TimerOn (MAPL,"--MISC")

   call MAPL_GetResource( MAPL, TAUCRIT, 'TAUCRIT:', DEFAULT=0.30, RC=STATUS)
   VERIFY_(STATUS)
   TAUCRIT   = TAUCRIT/2.13

   call MAPL_GetPointer(EXPORT,   CLDPRS,  'CLDPRS'  ,RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   CLDTMP,  'CLDTMP'  ,RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,    TAUIR,   'TAUIR'  ,RC=STATUS); VERIFY_(STATUS)

   if(associated(TAUIR)) TAUIR = 0.5*(TAUDIAG(:,:,:,3)+TAUDIAG(:,:,:,4))

   if(associated(CLDTMP).or.associated(CLDPRS)) then
      if(associated(CLDTMP)) CLDTMP = MAPL_UNDEF
      if(associated(CLDPRS)) CLDPRS = MAPL_UNDEF
      do j=1,jm
         do i=1,im
            do l=1,lm
               if(0.5*(TAUDIAG(I,J,L,3)+TAUDIAG(I,J,L,4))>TAUCRIT) then
                  if(associated(CLDTMP)) CLDTMP(I,J) = T  (I,J,L)
                  if(associated(CLDPRS)) CLDPRS(I,J) = PLE(I,J,L-1)
                  exit
               end if
            end do
         end do
      end do
   end if

   ! Correcting the timing of the alw and blw (mjs)

   call MAPL_GetPointer(EXPORT,   TSREFF,    'TSREFF' ,RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   SFCEM,     'SFCEM0' ,RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   DSFDTS,    'DSFDTS0',RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   LWS0,      'LWS0'   ,RC=STATUS); VERIFY_(STATUS)

   if(associated(TSREFF)) TSREFF = TS
   if(associated(DSFDTS)) DSFDTS =-DFDTS(:,:,LM)
   if(associated(SFCEM )) SFCEM  = SFCEM_INT
   if(associated(LWS0  )) LWS0   = FLX_INT(:,:,LM) + SFCEM_INT

   ! Deallocate per-band aerosol arrays
   ! ----------------------------------

   DEALLOCATE(TAUA)
   DEALLOCATE(SSAA)
   DEALLOCATE(ASYA)

   call MAPL_TimerOff(MAPL,"--MISC")

!  All done
!-----------

   RETURN_(ESMF_SUCCESS)

 end subroutine LW_Driver

!------------------------------------------------
!------------------------------------------------

 subroutine Update_Flx(IM,JM,LM,RC)
   integer,           intent(IN ) :: IM, JM, LM
   integer, optional, intent(OUT) :: RC

!  Locals

   character(len=ESMF_MAXSTR)        :: Iam
   integer                           :: STATUS

   real,          dimension(IM,JM)   :: DELT
   integer                           :: K
   integer                           :: LEV_LOW_MID
   integer                           :: LEV_MID_HIGH
   real                              :: PRS_LOW_MID                   ! pressure separating low and middle clouds
   real                              :: PRS_MID_HIGH                  ! pressure separating low and high   clouds

! pointer to import

   real, pointer, dimension(:,:  )   :: TSINST

! pointers to export

   real, pointer, dimension(:,:,:)   :: FLX
   real, pointer, dimension(:,:,:)   :: FLXA
   real, pointer, dimension(:,:,:)   :: FLC
   real, pointer, dimension(:,:,:)   :: FLA
   real, pointer, dimension(:,:,:)   :: FLXU
   real, pointer, dimension(:,:,:)   :: FLXAU
   real, pointer, dimension(:,:,:)   :: FLCU
   real, pointer, dimension(:,:,:)   :: FLAU
   real, pointer, dimension(:,:,:)   :: FLXD
   real, pointer, dimension(:,:,:)   :: FLXAD
   real, pointer, dimension(:,:,:)   :: FLCD
   real, pointer, dimension(:,:,:)   :: FLAD
   real, pointer, dimension(:,:  )   :: TSREFF
   real, pointer, dimension(:,:  )   :: SFCEM
   real, pointer, dimension(:,:  )   :: DSFDTS
   real, pointer, dimension(:,:  )   :: SFCEM0
   real, pointer, dimension(:,:  )   :: DSFDTS0
   real, pointer, dimension(:,:  )   :: OLR
   real, pointer, dimension(:,:  )   :: OLRA
   real, pointer, dimension(:,:  )   :: OLC
   real, pointer, dimension(:,:  )   :: OLCC5
   real, pointer, dimension(:,:  )   :: OLA
   real, pointer, dimension(:,:  )   :: FLNS
   real, pointer, dimension(:,:  )   :: FLNSNA
   real, pointer, dimension(:,:  )   :: FLNSC
   real, pointer, dimension(:,:  )   :: FLNSA
   real, pointer, dimension(:,:  )   :: LWS
   real, pointer, dimension(:,:  )   :: LWSA
   real, pointer, dimension(:,:  )   :: LCS
   real, pointer, dimension(:,:  )   :: LCSC5
   real, pointer, dimension(:,:  )   :: LAS
   real, pointer, dimension(:,:  )   :: CLDTT

   real, pointer, dimension(:,:,:)   :: FCLD
   real, pointer, dimension(:    )   :: PREF

   real, allocatable, dimension(:,:) :: DUMTT

!  Begin...
!----------

   IAm = "Update_Flx"

! Pointers to Exports
!--------------------

   call MAPL_GetPointer(EXPORT,   FLX   ,    'FLX',   RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   FLXA  ,    'FLXA',  RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   FLC   ,    'FLC',   RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   FLA   ,    'FLA',   RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   FLXU  ,    'FLXU',  RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   FLXAU ,    'FLXAU', RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   FLCU  ,    'FLCU',  RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   FLAU  ,    'FLAU',  RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   FLXD  ,    'FLXD',  RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   FLXAD ,    'FLXAD', RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   FLCD  ,    'FLCD',  RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   FLAD  ,    'FLAD',  RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   TSREFF,    'TSREFF',RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   SFCEM ,    'SFCEM', RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   DSFDTS,    'DSFDTS',RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   SFCEM0,    'SFCEM0',RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   DSFDTS0,  'DSFDTS0',RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   OLR   ,    'OLR'   ,RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   OLRA   ,    'OLRA' ,RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   OLC   ,    'OLC'   ,RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   OLCC5 ,    'OLCC5' ,RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   OLA   ,    'OLA'   ,RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   LWS   ,    'LWS'   ,RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   LWSA   ,    'LWSA' ,RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   LCS   ,    'LCS'   ,RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   LCSC5 ,    'LCSC5' ,RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   LAS   ,    'LAS'   ,RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   FLNS  ,   'FLNS'   ,RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   FLNSNA,   'FLNSNA' ,RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   FLNSC ,  'FLNSC'   ,RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(EXPORT,   FLNSA ,  'FLNSA'   ,RC=STATUS); VERIFY_(STATUS)

   call MAPL_GetPointer(EXPORT,   CLDTT ,  'CLDTT'   ,ALLOC=.TRUE.,RC=STATUS); VERIFY_(STATUS)

! Determine the 2-D Total Cloud Fraction
!---------------------------------------

   call MAPL_GetResource( MAPL, PRS_LOW_MID,    'PRS_LOW_MID_CLOUDS:' ,   DEFAULT=70000.,      RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetResource( MAPL, PRS_MID_HIGH,   'PRS_MID_HIGH_CLOUDS:',   DEFAULT=40000.,      RC=STATUS)
   VERIFY_(STATUS)

   call MAPL_GetPointer( IMPORT, FCLD, 'FCLD', RC=STATUS)
   VERIFY_(STATUS)
   call MAPL_GetPointer( IMPORT, PREF, 'PREF', RC=STATUS)
   VERIFY_(STATUS)
   
   ALLOCATE( DUMTT(IM,JM), STAT=STATUS)
   VERIFY_(STATUS)

! Determine the model level seperating mid and high clouds
!---------------------------------------------------------
   LEV_MID_HIGH = 1
   do K = 1, LM
      if( PREF(K) >= PRS_MID_HIGH ) then
         LEV_MID_HIGH = K
         exit
      end if
   end do

! Determine the model level seperating low and middle clouds
!-----------------------------------------------------------
   LEV_LOW_MID = LM
   do K = 1, LM
      if( PREF(K) >= PRS_LOW_MID  ) then
         LEV_LOW_MID = K
         exit
      end if
   end do

         DUMTT = 0.
         do K=1,LEV_MID_HIGH-1
            DUMTT = max(DUMTT,FCLD(:,:,K))
         end do
         CLDTT = (1-DUMTT)
         DUMTT = 0.
         do K= LEV_MID_HIGH,LEV_LOW_MID-1
            DUMTT = max(DUMTT,FCLD(:,:,K))
         end do
         CLDTT = CLDTT*(1-DUMTT)
         DUMTT = 0.
         do K=LEV_LOW_MID,LM
            DUMTT = max(DUMTT,FCLD(:,:,K))
         end do
         CLDTT = 1.0 - CLDTT*(1-DUMTT)

! Pointers to Imports
!--------------------

   call MAPL_GetPointer(IMPORT,   TSINST, 'TSINST',   RC=STATUS); VERIFY_(STATUS)

! Update fluxes
!--------------

   DELT = TSINST - TS_INT

   do K = 0, LM
      if(associated(FLX))    FLX (:,:,K) = FLX_INT  (:,:,K) + DFDTS (:,:,K) * DELT
      if(associated(FLXA))  FLXA (:,:,K) = FLXA_INT (:,:,K) + DFDTS (:,:,K) * DELT
      if(associated(FLC))    FLC (:,:,K) = FLC_INT  (:,:,K) + DFDTSC(:,:,K) * DELT
      if(associated(FLA))    FLA (:,:,K) = FLA_INT  (:,:,K) + DFDTSC(:,:,K) * DELT
      if(associated(FLXU))   FLXU(:,:,K) = FLXU_INT (:,:,K) + DFDTS (:,:,K) * DELT
      if(associated(FLXAU)) FLXAU(:,:,K) = FLXAU_INT(:,:,K) + DFDTS (:,:,K) * DELT
      if(associated(FLCU))   FLCU(:,:,K) = FLCU_INT (:,:,K) + DFDTSC(:,:,K) * DELT
      if(associated(FLAU))   FLAU(:,:,K) = FLAU_INT (:,:,K) + DFDTSC(:,:,K) * DELT
      if(associated(FLXD))   FLXD(:,:,K) = FLXD_INT (:,:,K)
      if(associated(FLXD))  FLXAD(:,:,K) = FLXAD_INT(:,:,K)
      if(associated(FLCD))   FLCD(:,:,K) = FLCD_INT (:,:,K)
      if(associated(FLAD))   FLAD(:,:,K) = FLAD_INT (:,:,K)
   end do

   if(associated(DSFDTS)) DSFDTS =                                - DFDTS (:,:,LM)
   if(associated(DSFDTS0))DSFDTS0=                                - DFDTS (:,:,LM)
   if(associated(SFCEM )) SFCEM  =                      SFCEM_INT - DFDTS (:,:,LM) * DELT
   if(associated(SFCEM0)) SFCEM0 =                      SFCEM_INT - DFDTS (:,:,LM) * DELT
   if(associated(OLR   )) OLR    = -(FLX_INT (:,:, 0)             + DFDTS (:,:, 0) * DELT)
   if(associated(OLRA  )) OLRA   = -(FLXA_INT(:,:, 0)             + DFDTS (:,:, 0) * DELT)
   if(associated(OLC   )) OLC    = -(FLC_INT (:,:, 0)             + DFDTSC(:,:, 0) * DELT)
   if(associated(OLCC5 )) then
           where(CLDTT <= 0.05 )
                 OLCC5  = -(FLC_INT (:,:, 0) + DFDTSC(:,:, 0) * DELT)
           elsewhere
                 OLCC5  = MAPL_UNDEF
           endwhere
   endif
   if(associated(OLA   )) OLA    = -(FLA_INT (:,:, 0)             + DFDTSC(:,:, 0) * DELT)
   if(associated(LWS   )) LWS    =   FLX_INT (:,:,LM) + SFCEM_INT  
   if(associated(LWSA  )) LWSA   =   FLXA_INT(:,:,LM) + SFCEM_INT  
   if(associated(LCS   )) LCS    =   FLC_INT (:,:,LM) + SFCEM_INT
   if(associated(LCSC5 )) then
           where(CLDTT <= 0.05 )
                 LCSC5  = FLC_INT(:,:,LM) + SFCEM_INT
           elsewhere
                 LCSC5  = MAPL_UNDEF
           endwhere
   endif
   if(associated(LAS   )) LAS    =   FLA_INT (:,:,LM) + SFCEM_INT
   if(associated(FLNS  )) FLNS   =   FLX_INT (:,:,LM)             + DFDTS (:,:,LM) * DELT
   if(associated(FLNSNA)) FLNSNA =   FLXA_INT(:,:,LM)             + DFDTS (:,:,LM) * DELT
   if(associated(FLNSC )) FLNSC  =   FLC_INT (:,:,LM)             + DFDTSC(:,:,LM) * DELT
   if(associated(FLNSA )) FLNSA  =   FLA_INT (:,:,LM)             + DFDTSC(:,:,LM) * DELT

! Reference surface temperature for export consistent with updated fluxes
!------------------------------------------------------------------------

   if(associated(TSREFF)) TSREFF = TSINST

!  All done
!-----------
   deallocate( DUMTT )

   RETURN_(ESMF_SUCCESS)

 end subroutine Update_Flx

end subroutine RUN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module GEOS_IrradGridCompMod

