!  $Id: GEOS_SolarGridComp.F90,v 1.131.2.16.2.1.6.2.2.3.2.8.8.2.14.3.4.16.4.11.4.3.14.8.22.3.2.1.2.3.2.1.84.1 2023/01/12 03:41:08 mmehari Exp $

#include "MAPL_Generic.h"

module GEOS_SolarGridCompMod

!=============================================================================
!BOP

! !MODULE: GEOS_SolarGridCompMod -- Computes solar radiation fluxes in a cloudy atmosphere

! /*
! !DESCRIPTION:
! 
! {\tt GEOS\_SolarGridCompMod} is an ESMF/MAPL gridded component that performs
!  a broadband calculation of shortwave radiative fluxes for use as a
!  solar radiation parameterization in atmospheric models on a sphere. \newline
! 
! {\em Scientific Basis:} The radiative transfer calculation is based on M-D Chou shortwave
! parameterization. The basic reference for the scheme is:
! Chou and Suarez 1999: A Solar Radiation Parameterization for Atmospheric Studies,
! NASA-TM-1999-104606, Vol 15. An updated version of this report can be
! found in SolarDoc.pdf in this directory. \newline
!
! The parameterization treats direct and diffuse fluxes of solar 
! radiation in eight spectral bands:  
! \begin{verbatim}
!        in the uv region :
!           index  1 for the 0.225-0.285 micron band
!           index  2 for the 0.175-0.225;0.285-0.300 micron band
!           index  3 for the 0.300-0.325 micron band
!           index  4 for the 0.325-0.4 micron band
!        in the par region :
!           index  5 for the 0.4-0.690 micron band
!        in the infrared region :
!           index  6 for the 0.690-1.220 micron band
!           index  7 for the 1.220-2.270 micron band
!           index  8 for the 2.270-3.850 micron band
! \end{verbatim}
! It includes gaseous absorption due to water vapor, ozone, CO$_2$, and
! molecular oxygen and the effects of molecular scattering,
! as well as multiple scattering due to clouds and aerosols. 
!
! It allows clouds to occur in any layer and 
! horizontal cloud cover fractions must be specified 
! for all layers; clear layers
! simply have a fraction of zero. Vertically, the layers are 
! assumed to be filled by cloud. To simplify the treatment of
! cloud effects, the model layers,
! are grouped into three super layers. Effective cloud properties are then
! parameterized by assuming that clouds are maximally overlapped within the super layers
! and randomly overlapped between the super layers.  The  
! optical properties of cloud particles depend on the liquid, ice, and rain mixing ratios,
! as well as on spatially dependent effective radii for the three species.
! These are all inputs to the component. \newline
!
!  The parameterization can include the effects of an arbitrary
!  number of aerosol species.
!  Aerosol optical thickness, single-scattering albedo, and asymmetry
!  factor must be determined as functions of height and spectral band
!  for each species. \newline
!
!
! {\em Code Implementation:} \newline
! 
!  {\tt GEOS\_SolarGridCompMod} is an encapsulation of Chou's plug-compatible
!  SORAD Fortran routine in a MAPL/ESMF gridded component (GC). 
!  It follows the standard rules for an ESMF/MAPL GCs.
!  It operates on the ESMF grid that appears in the
!  gridded component. This grid must
!  be present in the GC and properly initialized before Initialize
!  is called. The only restrictions on the grid are that it be 3-dimensional
!  with two horizontal and one vertical dimension and
!  with only the horizontal dimensions decomposed. The vertical dimension
!  is also assumed to the the thrid dimension of the Fortran arrays and
!  is indexed from the top down. No particular vertical coordinate is assumed,
!  rather the 3-dimensional field of air pressure at the layer interfaces is 
!  a required Import. \newline
!
!  This module contains only SetServices and Run methods. 
!  The Initialize and Finalize methods
!  being defaulted to the MAPL\_Generic versions. 
!  The SetServices method is the only public
!  entity. There are no public types or data. \newline
!
!  The contents of the Import, Export, and Internal States are explicitly
!  described in SetServices and in tables in this documentation.
!  All quantities in these states are in either ESMF Fields or Bundles,
!  and all share a common grid---the ESMF grid in the gridded component
!  at the time Initialize (MAPL\_GenericInitialize, in this case) was called.
!  All outputs appearing in the Export state are optional and are 
!  filled only if they have been allocated. All filled Exports are valid
!  for the time interval on the GC's clock when the run method is invoked.
!  Imports can be from either an instantaneous or a time-averaged state of the
!  atmosphere. All Imports are read-only; none are Friendly.
!  Most imports are simple ESMF Fields containing 2- or 
!  3-dimensional quantities, such as temperature and humidity, needed in
!  the flux calculation. Non-cloud aerosol amounts are the exception; they 
!  appear in an ESMF Bundle.  \newline 
!
!  The net (+ve downward) fluxes on the Export state are defined at the layer
!  interfaces, which are indexed from the top of the atmosphere (L=0)
!  to the surface. Incident fluxes
!  at the surface also appear in the Export state; these are separated
!  into direct (beam) and diffuse fluxes for three spectral bands
!  (uv, par, nir), as defined in the table above.  \newline
!
!  The full transfer calculation is done infrequently and 
!  its results kept in the Internal state. 
!  The frequency of full calculations is controlled
!  by an alarm whose interval can be set
!  from a value in the configuration and whose origin is taken as the 
!  beginning of the run.
!  For the full calculations, solar fluxes are computed based on
!  mean zenith angles averaged over sun positions for a 
!  given period (the long interval, which can be specified in 
!  the configuration) beyond the
!  current time on the input clock. On every call to the Run method,
!  whatever the state of the alarm that controls the full calculation,
!  the sun's position
!  is updated to the mean position for the clock's current interval
!  and fluxes are updated based on normalized fluxes computed during
!  the previous full transfer calculation, but using
!  the TOA insolation for the current time on the clock. Because of this
!  intermittent scheme, checkpoint-restart sequences are seamless
!  only when interrupted at the time of the full calculation.\newline
!
!  The calculation relies in MAPL's Astronomy layer, which in turn 
!  assumes that the ESMF grid can be queried for latitude and longitude
!  coordinates. \newline
!
! {\em Configuration:} \newline
!  
!  Like all MAPL GCs, {\tt GEOS\_SolarGridCompMod} assumes that the configuration
!  in the ESMF GC is open and treats it as an environment from which it can
!  {\em at any time} read control information. It uses MAPL rules for scanning this
!  configuration.
!\begin{verbatim}
!
!VARIABLE             DESCRIPTION           UNITS      DEFAULT   NOTES
!
!RUN_DT:              Short time interval   (seconds)  none   
!DT:                  Long time interval    (seconds)  RUN_DT
!AVGR:                Averaging interval    (seconds)  DT
!PRS_LOW_MID_CLOUDS:  Interface pressure    (Pa)       70000.
!                     between the low and
!                     middle cloud layers
!PRS_MID_HIGH_CLOUDS: Interface pressure    (Pa)       40000.
!                     between the high and
!                     middle cloud layers
!SOLAR_CONSTANT:                            (W m-2)    none      Use -1 for time-dependent values
!CO2:                 CO2 concentration     (ppmv)     none      Use -1 for time-dependent values
!
!\end{verbatim}
!
!
! !BUGS:
!
!\end{verbatim} 
!\begin{itemize} 
!    \item Aerosol properties for each aerosol in the Bundle are obtained by
!    calling a global method (Get\_AeroOptProp) that must recognize
!    the aerosol by its Field name in the Bundle. This is a placeholder
!    for a scheme in which each Field carries with it a method for computing
!    its aerosol's optical properties.
!
!    \item The grid must have two horizontal dimensions and they must be the inner dimensions
!    of Fortran arrays.
!
!    \item The load-balancing relies on the grid describing a sphere. Everything
!    works for non-spherical grids but the load-balancing should be disabled
!    and this can be done only by going into the code.
!\end{itemize} 
! \begin{verbatim}
!
!   */  

! !USES:

  use ESMF
  use MAPL_Mod
  use MAPL_ShmemMod, only: MAPL_CoresPerNodeGet

#ifdef _CUDA
  use cudafor
  ! NOTE: USE renames are used below to prevent name clashes with
  !       CUDA copies to the GPU.
  use sorad_constants, only: &
        ZK_UV_CONST=>ZK_UV, WK_UV_CONST=>WK_UV, RY_UV_CONST=>RY_UV, &
        XK_IR_CONST=>XK_IR, RY_IR_CONST=>RY_IR,                     &
          COA_CONST=>COA,     CAH_CONST=>CAH
  use rad_constants, only: &
         AIB_UV_CONST=>AIB_UV,   AWB_UV_CONST=>AWB_UV,   ARB_UV_CONST=>ARB_UV,  &
         AIG_UV_CONST=>AIG_UV,   AWG_UV_CONST=>AWG_UV,   ARG_UV_CONST=>ARG_UV,  &
        AIB_NIR_CONST=>AIB_NIR, AWB_NIR_CONST=>AWB_NIR, ARB_NIR_CONST=>ARB_NIR, &
        AIA_NIR_CONST=>AIA_NIR, AWA_NIR_CONST=>AWA_NIR, ARA_NIR_CONST=>ARA_NIR, &
        AIG_NIR_CONST=>AIG_NIR, AWG_NIR_CONST=>AWG_NIR, ARG_NIR_CONST=>ARG_NIR, &
           CAIB_CONST=>CAIB,       CAIF_CONST=>CAIF
  use soradmod, only: &
        ! Subroutines
        SORAD, &
        ! Device Inputs
        COSZ_DEV, PL_DEV, TA_DEV, WA_DEV, OA_DEV, CWC_DEV, FCLD_DEV, REFF_DEV, &
        RSUVBM_DEV, RSUVDF_DEV, RSIRBM_DEV, RSIRDF_DEV, &
        ! Aerosol inputs
        TAUA_DEV, SSAA_DEV, ASYA_DEV, &
        ! Constants in Global Memory
        COA, CAH, CAIB, CAIF, &
        ! Device Outputs
        FLX_DEV, FLC_DEV, FLXU_DEV, FLCU_DEV, &
        FDIRUV_DEV, FDIFUV_DEV, FDIRPAR_DEV, FDIFPAR_DEV, FDIRIR_DEV, FDIFIR_DEV, &
        FLX_SFC_BAND_DEV, &
        ! Constants
         ZK_UV,  WK_UV,  RY_UV, AIB_UV, AWB_UV, ARB_UV, &
        AIG_UV, AWG_UV, ARG_UV,  XK_IR,  RY_IR, AIB_NIR, &
        AWB_NIR, ARB_NIR, AIA_NIR, AWA_NIR, ARA_NIR, AIG_NIR, &
        AWG_NIR, ARG_NIR, HK_UV, HK_IR

#else
  use soradmod, only: SORAD
#endif
  use sorad_constants, only : HK_IR_OLD, HK_UV_OLD
  use gettau, only: getvistau

  use rrtmg_sw_rad
  use rrtmg_sw_init, only: rrtmg_sw_ini

  implicit none
  private

! !PUBLIC MEMBER FUNCTIONS:

  public SetServices

! !GLOBAL PARAMETERS
  INTEGER, PARAMETER :: NB_CHOU_UV  = 5 ! Number of UV bands
  INTEGER, PARAMETER :: NB_CHOU_NIR = 3 ! Number of near-IR bands
  INTEGER, PARAMETER :: NB_CHOU     = NB_CHOU_UV + NB_CHOU_NIR ! Total number of bands
  INTEGER, PARAMETER :: NB_RRTMG    = 14
  INTEGER, PARAMETER :: NB_OBIO     = 33
  integer            :: DO_OBIO

#define PACKIT   1
#define UNPACKIT 2

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
!   our instance of a MAPL\_MetaComp and putting it in the 
!   gridded component (GC). Here we only need to register the Run method with ESMF and
!   register the state variable specifications with MAPL. \newline
!   

!EOP

!=============================================================================

! ErrLog Variables

    character(len=ESMF_MAXSTR)              :: IAm
    integer                                 :: STATUS
    character(len=ESMF_MAXSTR)              :: COMP_NAME

! Local derived type aliases

    type (ESMF_Config          )            :: CF
    type (MAPL_MetaComp),     pointer       :: MAPL

! Locals

    integer      :: RUN_DT
    integer      :: MY_STEP
    integer      :: ACCUMINT
    real         :: DT

    logical      :: USE_CHOU, USE_RRTMG           ! Logical concerning what radiation to use
    real         :: RRTMG_SORAD                   ! Holds value from AGCM.rc
    integer      :: NUM_BANDS_SOLAR
    logical      :: SOLAR_TO_OBIO

!=============================================================================

! Begin...

! Get my name and set-up traceback handle
! ---------------------------------------

    call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // 'SetServices'

! Get the configuration
! ---------------------

    call ESMF_GridCompGet( GC, CONFIG = CF, RC=STATUS )
    VERIFY_(STATUS)

! Get the intervals; "heartbeat" must exist
! -----------------------------------------

    call ESMF_ConfigGetAttribute(CF, DT, Label="RUN_DT:"                          , RC=STATUS)
    VERIFY_(STATUS)

    RUN_DT = nint(DT)

! Refresh interval defaults to heartbeat.
! ---------------------------------------

    call ESMF_ConfigGetAttribute(CF, DT, Label=trim(COMP_NAME)//"_DT:", default=DT, RC=STATUS)
    VERIFY_(STATUS)

    MY_STEP = nint(DT)

! Averaging interval defaults to refresh interval.
!-------------------------------------------------

    call ESMF_ConfigGetAttribute(CF, DT, Label=trim(COMP_NAME)//'Avrg:',default=DT,RC=STATUS)
    VERIFY_(STATUS)

    ACCUMINT = nint(DT)

! Test if we are using RRTMG.
!----------------------------

    ! MAT This is needed in SetServices because we Export a per-band flux 
    !     and the number of bands are different between Chou and RRTMG

    call ESMF_ConfigGetAttribute(CF, RRTMG_SORAD , Label='USE_RRTMG_SORAD:', DEFAULT=0.0, RC=STATUS)
    VERIFY_(STATUS)

    USE_RRTMG = RRTMG_SORAD /= 0.0
    USE_CHOU  = .not.USE_RRTMG

    if (USE_CHOU) then
       NUM_BANDS_SOLAR = NB_CHOU
    else
       NUM_BANDS_SOLAR = NB_RRTMG
    end if

    ! Decide if should make OBIO exports

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_GetResource ( MAPL, DO_OBIO, Label="USE_OCEANOBIOGEOCHEM:",DEFAULT=0, RC=STATUS)
    VERIFY_(STATUS)

    if (DO_OBIO/=0) then
       SOLAR_TO_OBIO = .TRUE.
    else 
       SOLAR_TO_OBIO = .FALSE.
    endif

! Set the state variable specs.
! -----------------------------

!BOS

! !IMPORT STATE:

    call MAPL_AddImportSpec(GC,                                              &
       LONG_NAME  = 'air_pressure',                                          &
       UNITS      = 'Pa',                                                    &
       SHORT_NAME = 'PLE',                                                   &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationEdge,                                      &
       AVERAGING_INTERVAL = ACCUMINT,                                        &
       REFRESH_INTERVAL   = MY_STEP,                                         &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                              &
       LONG_NAME          = 'surface_skin_temperature',                      &
       UNITS              = 'K',                                             &
       SHORT_NAME         = 'TS',                                            &
       DIMS               = MAPL_DimsHorzOnly,                               &
       VLOCATION          = MAPL_VLocationNone,                              &
       AVERAGING_INTERVAL = ACCUMINT,                                        &
       REFRESH_INTERVAL   = MY_STEP,                                         &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                              &
       LONG_NAME          = 'methane_concentration',                         &
       UNITS              = 'pppv',                                          &
       SHORT_NAME         = 'CH4',                                           &
       DIMS               = MAPL_DimsHorzVert,                               &
       VLOCATION          = MAPL_VLocationCenter,                            &
       AVERAGING_INTERVAL = ACCUMINT,                                        &
       REFRESH_INTERVAL   = MY_STEP,                                         &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                              &
       LONG_NAME          = 'nitrous_oxide_concentration',                   &
       UNITS              = 'pppv',                                          &
       SHORT_NAME         = 'N2O',                                           &
       DIMS               = MAPL_DimsHorzVert,                               &
       VLOCATION          = MAPL_VLocationCenter,                            &
       AVERAGING_INTERVAL = ACCUMINT,                                        &
       REFRESH_INTERVAL   = MY_STEP,                                         &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                              &
       LONG_NAME  = 'air_temperature',                                       &
       UNITS      = 'K',                                                     &
       SHORT_NAME = 'T',                                                     &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationCenter,                                    &
       AVERAGING_INTERVAL = ACCUMINT,                                        &
       REFRESH_INTERVAL   = MY_STEP,                                         &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                              &
       LONG_NAME  = 'specific_humidity',                                     &
       UNITS      = 'kg kg-1',                                               &
       SHORT_NAME = 'QV',                                                    &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationCenter,                                    &
       AVERAGING_INTERVAL = ACCUMINT,                                        &
       REFRESH_INTERVAL   = MY_STEP,                                         &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                              &
       LONG_NAME  = 'mass_fraction_of_cloud_liquid_water_in_air',            &
       UNITS      = 'kg kg-1',                                               &
       SHORT_NAME = 'QL',                                                    &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationCenter,                                    &
       AVERAGING_INTERVAL = ACCUMINT,                                        &
       REFRESH_INTERVAL   = MY_STEP,                                         &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                              &
       LONG_NAME  = 'mass_fraction_of_cloud_ice_in_air',                     &
       UNITS      = 'kg kg-1',                                               &
       SHORT_NAME = 'QI',                                                    &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationCenter,                                    &
       AVERAGING_INTERVAL = ACCUMINT,                                        &
       REFRESH_INTERVAL   = MY_STEP,                                         &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                              &
       LONG_NAME  = 'mass_fraction_of_rain_water_in_air',                    &
       UNITS      = 'kg kg-1',                                               &
       SHORT_NAME = 'QR',                                                    &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationCenter,                                    &
       AVERAGING_INTERVAL = ACCUMINT,                                        &
       REFRESH_INTERVAL   = MY_STEP,                                         &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                              &
       LONG_NAME  = 'mass_fraction_of_snow_in_air',                          &
       UNITS      = 'kg kg-1',                                               &
       SHORT_NAME = 'QS',                                                    &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationCenter,                                    &
       AVERAGING_INTERVAL = ACCUMINT,                                        &
       REFRESH_INTERVAL   = MY_STEP,                                         &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                              &
       LONG_NAME  = 'effective_radius_of_cloud_liquid_water_particles',      &
       UNITS      = 'm',                                                     &
       SHORT_NAME = 'RL',                                                    &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationCenter,                                    &
       AVERAGING_INTERVAL = ACCUMINT,                                        &
       REFRESH_INTERVAL   = MY_STEP,                                         &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                              &
       LONG_NAME  = 'effective_radius_of_cloud_ice_particles',               &
       UNITS      = 'm',                                                     &
       SHORT_NAME = 'RI',                                                    &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationCenter,                                    &
       AVERAGING_INTERVAL = ACCUMINT,                                        &
       REFRESH_INTERVAL   = MY_STEP,                                         &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                              &
       LONG_NAME  = 'effective_radius_of_rain_particles',                    &
       UNITS      = 'm',                                                     &
       SHORT_NAME = 'RR',                                                    &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationCenter,                                    &
       AVERAGING_INTERVAL = ACCUMINT,                                        &
       REFRESH_INTERVAL   = MY_STEP,                                         &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                              &
       LONG_NAME  = 'effective_radius_of_snow_particles',                    &
       UNITS      = 'm',                                                     &
       SHORT_NAME = 'RS',                                                    &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationCenter,                                    &
       AVERAGING_INTERVAL = ACCUMINT,                                        &
       REFRESH_INTERVAL   = MY_STEP,                                         &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                              &
       LONG_NAME  = 'odd-oxygen_volume_mixing_ratio',                        &
       UNITS      = 'mol mol-1',                                             &
       SHORT_NAME = 'OX',                                                    &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationCenter,                                    &
       AVERAGING_INTERVAL = ACCUMINT,                                        &
       REFRESH_INTERVAL   = MY_STEP,                                         &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                              &
       LONG_NAME  = 'cloud_area_fraction',                                   &
       UNITS      = '1',                                                     &
       SHORT_NAME = 'FCLD',                                                  &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationCenter,                                    &
       AVERAGING_INTERVAL = ACCUMINT,                                        &
       REFRESH_INTERVAL   = MY_STEP,                                         &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                              &
       LONG_NAME  = 'aerosols',                                              &
       UNITS      = 'kg kg-1',                                               &
       SHORT_NAME = 'AERO',                                                  &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationCenter,                                    &
       DATATYPE   = MAPL_StateItem,                                          &
       RESTART    = MAPL_RestartSkip,                                        &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                              &
       LONG_NAME  = 'surface_albedo_for_visible_beam',                       &
       UNITS      = '1',                                                     &
       SHORT_NAME = 'ALBVR',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                              &
       LONG_NAME  = 'surface_albedo_for_visible_diffuse',                    &
       UNITS      = '1',                                                     &
       SHORT_NAME = 'ALBVF',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                              &
       LONG_NAME  = 'surface_albedo_for_near_infrared_beam',                 &
       UNITS      = '1',                                                     &
       SHORT_NAME = 'ALBNR',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                              &
       LONG_NAME  = 'surface_albedo_for_near_infrared_diffuse',              &
       UNITS      = '1',                                                     &
       SHORT_NAME = 'ALBNF',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                              &
       SHORT_NAME = 'PREF',                                                  &
       LONG_NAME  = 'reference_air_pressure',                                &
       UNITS      = 'Pa',                                                    &
       DIMS       = MAPL_DimsVertOnly,                                       &
       VLOCATION  = MAPL_VLocationEdge,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)


!  Solar does not have a "real" state. We keep an internal variable
!  for each variable produced by solar during the compute steps. 
!  Versions of these, weighted by the appropriate TOA insolation,
!  are returned at each time step.

!  !INTERNAL STATE:

    call MAPL_AddInternalSpec(GC,                                            &
       LONG_NAME  ='normalized_net_downward_shortwave_flux_in_air',          &
       UNITS      ='1',                                                      &
       SHORT_NAME ='FSWN',                                                   &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationEdge,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                            &
       LONG_NAME  ='normalized_net_downward_shortwave_flux_in_air_assuming_clear_sky',&
       UNITS      ='1',                                                      &
       SHORT_NAME ='FSCN',                                                   &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationEdge,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                            &
       LONG_NAME  ='normalized_upward_shortwave_flux_in_air',                &
       UNITS      ='1',                                                      &
       SHORT_NAME ='FSWUN',                                                  &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationEdge,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                            &
       LONG_NAME  ='normalized_upward_shortwave_flux_in_air_assuming_clear_sky',&
       UNITS      ='1',                                                      &
       SHORT_NAME ='FSCUN',                                                  &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationEdge,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                            &
       LONG_NAME      = 'normalized_net_surface_downward_shortwave_flux_per_band_in_air', &
       UNITS          = '1',                                                 &
       SHORT_NAME     = 'FSWBANDN',                                          &
       DIMS           = MAPL_DimsHorzOnly,                                   &
       UNGRIDDED_DIMS = (/ NUM_BANDS_SOLAR /),                                  &
       VLOCATION      = MAPL_VLocationNone,                                  &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                            &
       LONG_NAME  = 'normalized_surface_downwelling_ultraviolet_beam_flux',  &
       UNITS      = '1',                                                     &
       SHORT_NAME = 'DRUVRN',                                                &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                            &
       LONG_NAME  = 'normalized_surface_downwelling_ultraviolet_diffuse_flux', &
       UNITS      = '1',                                                     &
       SHORT_NAME = 'DFUVRN',                                                &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                            &
       LONG_NAME  = 'normalized_surface_downwelling_par_beam_flux',          &
       UNITS      = '1',                                                     &
       SHORT_NAME = 'DRPARN',                                                &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                            &
       LONG_NAME  = 'normalized_surface_downwelling_par_diffuse_flux',       &
       UNITS      = '1',                                                     &
       SHORT_NAME = 'DFPARN',                                                &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                            &
       LONG_NAME  = 'normalized_surface_downwelling_nearinfrared_beam_flux', &
       UNITS      = '1',                                                     &
       SHORT_NAME = 'DRNIRN',                                                &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                            &
       LONG_NAME  = 'normalized_surface_downwelling_nearinfrared_diffuse_flux', &
       UNITS      = '1',                                                     &
       SHORT_NAME = 'DFNIRN',                                                &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    if (SOLAR_TO_OBIO) then

       call MAPL_AddInternalSpec(GC,                                         &
          LONG_NAME      = 'normalized_surface_downwelling_shortwave_beam_flux_per_band',&
          UNITS          = '1',                                              &
          SHORT_NAME     = 'DRBANDN',                                        &
          DIMS           = MAPL_DimsHorzOnly,                                &
          UNGRIDDED_DIMS = (/ NUM_BANDS_SOLAR /),                               &
          VLOCATION      = MAPL_VLocationNone,                         __RC__)

       call MAPL_AddInternalSpec(GC,                                         &
          LONG_NAME      = 'normalized_surface_downwelling_shortwave_diffuse_flux_per_band',&
          UNITS          = '1',                                              &
          SHORT_NAME     = 'DFBANDN',                                        &
          DIMS           = MAPL_DimsHorzOnly,                                &
          UNGRIDDED_DIMS = (/ NUM_BANDS_SOLAR /),                               &
          VLOCATION      = MAPL_VLocationNone,                         __RC__)

    end if

    call MAPL_AddInternalSpec(GC,                                            &
       LONG_NAME  ='normalized_net_downward_shortwave_flux_in_air_assuming_no_aerosol', &
       UNITS      ='1',                                                      &
       SHORT_NAME ='FSWNAN',                                                 &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationEdge,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                            &
       LONG_NAME  ='normalized_net_downward_shortwave_flux_in_air_assuming_clear_sky_and_no_aerosol',&
       UNITS      ='1',                                                      &
       SHORT_NAME ='FSCNAN',                                                 &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationEdge,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                            &
       LONG_NAME  ='normalized_upward_shortwave_flux_in_air_assuming_no_aerosol', &
       UNITS      ='1',                                                      &
       SHORT_NAME ='FSWUNAN',                                                &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationEdge,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                            &
       LONG_NAME  ='normalized_upward_shortwave_flux_in_air_assuming_clear_sky_and_no_aerosol',&
       UNITS      ='1',                                                      &
       SHORT_NAME ='FSCUNAN',                                                &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationEdge,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddInternalSpec(GC,                                            &
       LONG_NAME      = 'normalized_net_surface_downward_shortwave_flux_per_band_in_air_assuming_no_aerosol', &
       UNITS          = '1',                                                 &
       SHORT_NAME     = 'FSWBANDNAN',                                        &
       DIMS           = MAPL_DimsHorzOnly,                                   &
       UNGRIDDED_DIMS = (/ NUM_BANDS_SOLAR /),                                  &
       VLOCATION      = MAPL_VLocationNone,                                  &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)


!  !EXPORT STATE:
  

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  ='net_downward_shortwave_flux_in_air',                     &
       UNITS      ='W m-2',                                                  &
       SHORT_NAME ='FSW',                                                    &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationEdge,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  ='net_downward_shortwave_flux_in_air_assuming_clear_sky',  &
       UNITS      ='W m-2',                                                  &
       SHORT_NAME ='FSC',                                                    &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationEdge,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  ='net_downward_shortwave_flux_in_air_assuming_no_aerosol', &
       UNITS      ='W m-2',                                                  &
       SHORT_NAME ='FSWNA',                                                  &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationEdge,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  ='net_downward_shortwave_flux_in_air_assuming_clear_sky_and_no_aerosol',&
       UNITS      ='W m-2',                                                  &
       SHORT_NAME ='FSCNA',                                                  &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationEdge,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  ='downward_shortwave_flux_in_air',                         &
       UNITS      ='W m-2',                                                  &
       SHORT_NAME ='FSWD',                                                   &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationEdge,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  ='downward_shortwave_flux_in_air_assuming_clear_sky',      &
       UNITS      ='W m-2',                                                  &
       SHORT_NAME ='FSCD',                                                   &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationEdge,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  ='downward_shortwave_flux_in_air_assuming_no_aerosol',     &
       UNITS      ='W m-2',                                                  &
       SHORT_NAME ='FSWDNA',                                                 &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationEdge,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  ='downward_shortwave_flux_in_air_assuming_clear_sky_and_no_aerosol',&
       UNITS      ='W m-2',                                                  &
       SHORT_NAME ='FSCDNA',                                                 &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationEdge,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  ='upward_shortwave_flux_in_air',                           &
       UNITS      ='W m-2',                                                  &
       SHORT_NAME ='FSWU',                                                   &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationEdge,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  ='upward_shortwave_flux_in_air_assuming_clear_sky',        &
       UNITS      ='W m-2',                                                  &
       SHORT_NAME ='FSCU',                                                   &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationEdge,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  ='upward_shortwave_flux_in_air_assuming_no_aerosol',       &
       UNITS      ='W m-2',                                                  &
       SHORT_NAME ='FSWUNA',                                                 &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationEdge,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  ='upward_shortwave_flux_in_air_assuming_clear_sky_and_no_aerosol',&
       UNITS      ='W m-2',                                                  &
       SHORT_NAME ='FSCUNA',                                                 &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationEdge,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME      = 'net_surface_downward_shortwave_flux_per_band_in_air', &
       UNITS          = 'W m-2',                                             &
       SHORT_NAME     = 'FSWBAND',                                           &
       DIMS           = MAPL_DimsHorzOnly,                                   &
       UNGRIDDED_DIMS = (/ NUM_BANDS_SOLAR /),                                  &
       VLOCATION      = MAPL_VLocationNone,                                  &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME      = 'net_surface_downward_shortwave_flux_per_band_in_air_assuming_no_aerosol', &
       UNITS          = 'W m-2',                                             &
       SHORT_NAME     = 'FSWBANDNA',                                         &
       DIMS           = MAPL_DimsHorzOnly,                                   &
       UNGRIDDED_DIMS = (/ NUM_BANDS_SOLAR /),                                  &
       VLOCATION      = MAPL_VLocationNone,                                  &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'normalized_surface_downwelling_ultraviolet_beam_flux',  &
       UNITS      = '1',                                                     &
       SHORT_NAME = 'DRUVRN',                                                &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'normalized_surface_downwelling_ultraviolet_diffuse_flux', &
       UNITS      = '1',                                                     &
       SHORT_NAME = 'DFUVRN',                                                &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'normalized_surface_downwelling_par_beam_flux',          &
       UNITS      = '1',                                                     &
       SHORT_NAME = 'DRPARN',                                                &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'normalized_surface_downwelling_par_diffuse_flux',       &
       UNITS      = '1',                                                     &
       SHORT_NAME = 'DFPARN',                                                &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'normalized_surface_downwelling_nearinfrared_beam_flux', &
       UNITS      = '1',                                                     &
       SHORT_NAME = 'DRNIRN',                                                &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'normalized_surface_downwelling_nearinfrared_diffuse_flux', &
       UNITS      = '1',                                                     &
       SHORT_NAME = 'DFNIRN',                                                &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_downwelling_ultraviolet_beam_normal_flux',      &
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'DRNUVR',                                                &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_downwelling_par_beam_normal_flux',              &
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'DRNPAR',                                                &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_downwelling_nearinfrared_beam_normal_flux',     &
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'DRNNIR',                                                &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_downwelling_ultraviolet_beam_flux',             &
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'DRUVR',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_downwelling_ultraviolet_diffuse_flux',          &
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'DFUVR',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_downwelling_par_beam_flux',                     &
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'DRPAR',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_downwelling_par_diffuse_flux',                  &
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'DFPAR',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_downwelling_nearinfrared_beam_flux',            &
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'DRNIR',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_downwelling_nearinfrared_diffuse_flux',         &
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'DFNIR',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    if (SOLAR_TO_OBIO) then

       call MAPL_AddExportSpec(GC,                                           &
          LONG_NAME      = 'surface_downwelling_shortwave_beam_flux_per_OBIO_band',&
          UNITS          = 'W m-2',                                          &
          SHORT_NAME     = 'DROBIO',                                         &
          DIMS           = MAPL_DimsHorzOnly,                                &
          UNGRIDDED_DIMS = (/ NB_OBIO /),                                    &
          VLOCATION      = MAPL_VLocationNone,                         __RC__)

       call MAPL_AddExportSpec(GC,                                           &
          LONG_NAME      = 'surface_downwelling_shortwave_diffuse_flux_per_OBIO_band',&
          UNITS          = 'W m-2',                                          &
          SHORT_NAME     = 'DFOBIO',                                         &
          DIMS           = MAPL_DimsHorzOnly,                                &
          UNGRIDDED_DIMS = (/ NB_OBIO /),                                    &
          VLOCATION      = MAPL_VLocationNone,                         __RC__)

    end if

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'cloud_area_fraction',                                   &
       UNITS      = '1' ,                                                    &
       SHORT_NAME = 'FCLD',                                                  &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationCenter,                                    &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'cloud_area_fraction_for_low_clouds',                    &
       UNITS      = '1' ,                                                    &
       SHORT_NAME = 'CLDLO',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'cloud_area_fraction_for_middle_clouds',                 &
       UNITS      = '1' ,                                                    &
       SHORT_NAME = 'CLDMD',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'cloud_area_fraction_for_high_clouds',                   &
       UNITS      = '1' ,                                                    &
       SHORT_NAME = 'CLDHI',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'total_cloud_area_fraction',                             &
       UNITS      = '1' ,                                                    &
       SHORT_NAME = 'CLDTT',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'in_cloud_optical_thickness_of_low_clouds',                       &
       UNITS      = '1' ,                                                    &
       SHORT_NAME = 'TAULO',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'in_cloud_optical_thickness_of_middle_clouds',                    &
       UNITS      = '1' ,                                                    &
       SHORT_NAME = 'TAUMD',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'in_cloud_optical_thickness_of_high_clouds(EXPORT)',              &
       UNITS      = '1' ,                                                    &
       SHORT_NAME = 'TAUHI',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'in_cloud_optical_thickness_of_all_clouds',                       &
       UNITS      = '1' ,                                                    &
       SHORT_NAME = 'TAUTT',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'in_cloud_optical_thickness_for_ice_clouds',                      &
       UNITS      = '1' ,                                                    &
       SHORT_NAME = 'TAUCLI',                                                &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationCenter,                                    &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'in_cloud_optical_thickness_for_liquid_clouds',                   &
       UNITS      = '1' ,                                                    &
       SHORT_NAME = 'TAUCLW',                                                &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationCenter,                                    &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'in_cloud_optical_thickness_for_falling_rain',                    &
       UNITS      = '1' ,                                                    &
       SHORT_NAME = 'TAUCLR',                                                &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationCenter,                                    &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'in_cloud_optical_thickness_for_falling_snow',                    &
       UNITS      = '1' ,                                                    &
       SHORT_NAME = 'TAUCLS',                                                &
       DIMS       = MAPL_DimsHorzVert,                                       &
       VLOCATION  = MAPL_VLocationCenter,                                    &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_net_downward_shortwave_flux_assuming_clear_sky',&
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'RSCS',                                                  &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_net_downward_shortwave_flux',                   &
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'RSRS',                                                  &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_net_downward_shortwave_flux_assuming_clear_sky_and_no_aerosol',&
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'RSCSNA',                                                &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_net_downward_shortwave_flux_assuming_no_aerosol',&
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'RSRSNA',                                                &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_incoming_shortwave_flux',                       &
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'SLRSF',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_incoming_shortwave_flux_assuming_clear_sky',    &
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'SLRSFC',                                                &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_incoming_shortwave_flux_assuming_clean_sky',    &
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'SLRSFNA',                                               &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_incoming_shortwave_flux_assuming_clear_clean_sky',&
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'SLRSFCNA',                                              &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_outgoing_shortwave_flux',                       &
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'SLRSUF',                                                &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_outgoing_shortwave_flux_assuming_clear_sky',    &
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'SLRSUFC',                                               &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_outgoing_shortwave_flux_assuming_clean_sky',    &
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'SLRSUFNA',                                              &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_outgoing_shortwave_flux_assuming_clear_clean_sky',&
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'SLRSUFCNA',                                             &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)
!END CAR

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'toa_outgoing_shortwave_flux',                           &
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'OSR',                                                   &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'toa_outgoing_shortwave_flux_assuming_clear_sky',        &
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'OSRCLR',                                                &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'toa_outgoing_shortwave_flux_no_aerosol',                &
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'OSRNA',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'toa_outgoing_shortwave_flux_no_aerosol__clear_sky',     &
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'OSRCNA',                                                &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)
!END CAR

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'toa_net_downward_shortwave_flux',                       &
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'RSR',                                                   &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'toa_net_downward_shortwave_flux_assuming_clear_sky',    &
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'RSC',                                                   &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'toa_net_downward_shortwave_flux_assuming_no_aerosol',   &
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'RSRNA',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'toa_net_downward_shortwave_flux_assuming_clear_sky_and_no_aerosol',&
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'RSCNA',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'toa_incoming_shortwave_flux',                           &
       UNITS      = 'W m-2',                                                 &
       SHORT_NAME = 'SLRTP',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_albedo',                                        &
       UNITS      = '1',                                                     &
       SHORT_NAME = 'ALBEDO',                                                &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_albedo_for_visible_beam',                       &
       UNITS      = '1',                                                     &
       SHORT_NAME = 'ALBVR',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_albedo_for_visible_diffuse',                    &
       UNITS      = '1',                                                     &
       SHORT_NAME = 'ALBVF',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_albedo_for_near_infrared_beam',                 &
       UNITS      = '1',                                                     &
       SHORT_NAME = 'ALBNR',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'surface_albedo_for_near_infrared_diffuse',              &
       UNITS      = '1',                                                     &
       SHORT_NAME = 'ALBNF',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'cosine_of_the_solar_zenith_angle',                      &
       UNITS      = '1',                                                     &
       SHORT_NAME = 'COSZ',                                                  &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
       LONG_NAME  = 'mean_cosine_of_the_solar_zenith_angle',                 &
       UNITS      = '1',                                                     &
       SHORT_NAME = 'MCOSZ',                                                 &
       DIMS       = MAPL_DimsHorzOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
        SHORT_NAME = 'CLDTMP',                                               &
        LONG_NAME  = 'cloud_top_temperature',                                &
        UNITS      = 'K',                                                    &
        DIMS       = MAPL_DimsHorzOnly,                                      &
        VLOCATION  = MAPL_VLocationNone,                                     &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                              &
        SHORT_NAME = 'CLDPRS',                                               &
        LONG_NAME  = 'cloud_top_pressure',                                   &
        UNITS      = 'Pa',                                                   &
        DIMS       = MAPL_DimsHorzOnly,                                      &
        VLOCATION  = MAPL_VLocationNone,                                     &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

!EOS

! Set the Profiling timers
! ------------------------

    call MAPL_TimerAdd(GC, name="PRELIMS"     ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC, name="REFRESH"     ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC, name="-AEROSOLS"   ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC, name="-SORAD"      ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC, name="--SORAD_RUN" ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC, name="--SORAD_DATA",RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC, name="---SORAD_DATA_DEVICE"    ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC, name="---SORAD_DATA_CONST"    ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC, name="--SORAD_ALLOC"    ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC, name="--SORAD_DEALLOC"    ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC, name="-RRTMG" ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC, name="--RRTMG_RUN" ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC, name="--RRTMG_INIT" ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC, name="--RRTMG_FLIP" ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC, name="-BALANCE"    ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC, name="--CREATE"    ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC, name="--DISTRIBUTE"    ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC, name="--RETRIEVE"    ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC, name="--DESTROY"    ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC, name="-MISC"       ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC, name="UPDATE"      ,RC=STATUS)
    VERIFY_(STATUS)
    
! Set Run method and use generic init and final methods
! -----------------------------------------------------

    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN,  Run, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_GenericSetServices    ( GC, RC=STATUS)
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)  
  end subroutine SetServices

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOP

! !IROUTINE: RUN -- Run method for the SOLAR component

! !INTERFACE:

  subroutine RUN ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
    type(ESMF_State),    intent(inout) :: IMPORT ! Import state
    type(ESMF_State),    intent(inout) :: EXPORT ! Export state
    type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
    integer, optional,   intent(  out) :: RC     ! Error code:
! /*
! !DESCRIPTION: Each time the Run method is called it fills all Exports for
!   which an allocated pointer is available. Exports are filled from the normalized
!   fluxes kept in the Internal state and the position of the Sun for the
!   current interval in the Clock. If MAPL's RunAlarm is ringing,
!   it also refreshes the normalized fluxes kept in the internal state by doing
!   a full transfer calculation valid for solar positions over a ``future interval''
!   extebnding to the next anticipated
!   ringing of the RunAlarm. Whether this is done before or after the
!   Exports are updated and the excat definition of the ``future interval'' is
!   controlled by a flag in the configuration. \newline
!
!   A simple load balancing scheme is used that evens work between antipodal
!   processors. \newline 
! \newline
! */

! !BUGS:
!
!\end{verbatim} 
!
! \begin{itemize}
!  \item Deciding on the correct behavior for intermitent calls can be tricky.
!
!  \item Load-balancing communication needs to be upgraded to most up-to-date
!  ESMF machine model. 
! \end{itemize}
!
! \begin{verbatim}
!

!EOP


! ErrLog Variables

    character(len=ESMF_MAXSTR)          :: IAm
    integer                             :: STATUS
    character(len=ESMF_MAXSTR)          :: COMP_NAME

! Local derived type aliases

    type (MAPL_MetaComp),     pointer   :: MAPL
    type (ESMF_Grid        )            :: ESMFGRID
    type (ESMF_Config      )            :: CF

! Local variables

    type (ESMF_Alarm)                   :: ALARM
    type (ESMF_State)                   :: INTERNAL
    type (ESMF_Time)                    :: currentTime
    type (ESMF_TimeInterval)            :: intDT
    integer                             :: IM, JM, LM
    type (MAPL_SunOrbit)                :: ORBIT
    type (MAPL_VarSpec), pointer        :: IMPORTspec(:)   => null()
    type (MAPL_VarSpec), pointer        :: EXPORTspec(:)   => null()
    type (MAPL_VarSpec), pointer        :: INTERNALspec(:) => null()
    real, pointer, dimension(:,:)       :: LONS
    real, pointer, dimension(:,:)       :: LATS

    real, pointer, dimension(:,:,:)     :: FSWNA
    real, pointer, dimension(:,:,:)     :: FSCNA       
    real, pointer, dimension(:,:,:)     :: FSWUNA
    real, pointer, dimension(:,:,:)     :: FSCUNA       
    real, pointer, dimension(:,:,:)     :: FSWBANDNA

    real, pointer, dimension(:,:  )     :: RSRSNA      
    real, pointer, dimension(:,:  )     :: RSCSNA      
    real, pointer, dimension(:,:  )     :: RSRNA      
    real, pointer, dimension(:,:  )     :: RSCNA      

    real, pointer, dimension(:,:  ) :: DFUVRN, DRUVRN
    real, pointer, dimension(:,:  ) :: DFPARN, DRPARN
    real, pointer, dimension(:,:  ) :: DFNIRN, DRNIRN
    real, pointer, dimension(:,:  ) ::  DFUVR,  DRUVR
    real, pointer, dimension(:,:  ) ::  DFPAR,  DRPAR
    real, pointer, dimension(:,:  ) ::  DFNIR,  DRNIR

    type (ESMF_State)                    :: AERO
    character(len=ESMF_MAXSTR)           :: AS_FIELD_NAME   
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

    integer                              :: band
    logical                              :: implements_aerosol_optics
    logical                              :: USE_CHOU, USE_RRTMG    ! Logical concerning what radiation to use
    real                                 :: RRTMG_SORAD            ! Holds value from AGCM.rc
    integer                              :: NUM_BANDS_SOLAR        ! Number of bands in solar
    logical                              :: SOLAR_TO_OBIO
    integer, parameter                   :: BANDS_SOLAR_OFFSET = 0 ! Offset for bands in solar
    integer, parameter                   :: NB_CHOU  = 8           ! Number of bands in solar for Chou  scheme
    integer, parameter                   :: NB_RRTMG = 14          ! Number of bands in solar for RRTMG scheme


    integer :: CalledLast
    integer :: LCLDMH
    integer :: LCLDLM
    integer :: K
    integer :: YY, DOY
    real    :: CO2
    real    :: PRS_LOW_MID
    real    :: PRS_MID_HIGH
    real    :: SC, HK(8), HK_IR_TEMP(3,10), HK_UV_TEMP(5), MG, SB
    integer :: SUNFLAG
    real, pointer, dimension(:  )   :: PREF

    logical :: REFRESH_FLUXES
    logical :: UPDATE_FIRST

    real, external                  :: getco2
    character(len=ESMF_MAXSTR)      :: MSGSTRING

    real, save :: CO2_0, SC_0, MG_0, SB_0
    data          CO2_0 /0.0/, SC_0/0.0/, MG_0/0.0/, SB_0/0.0/

    logical                    :: LoadBalance
    character(len=ESMF_MAXSTR) :: DYCORE
    integer                    :: SOLAR_LOAD_BALANCE
    integer                    :: SolarBalanceHandle

    character(len=ESMF_MAXPATHLEN) :: SolCycFileName
    logical                        :: USE_NRLSSI2

!=============================================================================

! Begin... 

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    call ESMF_GridCompGet( GC, name=COMP_NAME, GRID=ESMFGRID, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // "Run"

! Get my internal MAPL_Generic state
!-----------------------------------

    call MAPL_GetObjectFromGC    (GC, MAPL,    RC=STATUS); VERIFY_(STATUS)

    call MAPL_TimerOn (MAPL,"TOTAL"  ,RC=STATUS); VERIFY_(STATUS)
    call MAPL_TimerOn (MAPL,"PRELIMS",RC=STATUS); VERIFY_(STATUS)

! Get parameters from generic state.
!-----------------------------------

    call MAPL_Get(MAPL,                                                &
         IM                  = IM,                                     &
         JM                  = JM,                                     &
         LM                  = LM,                                     &
         CF                  = CF,                                     &
         LONS                = LONS,                                   &
         LATS                = LATS,                                   &
         RUNALARM            = ALARM,                                  &
         ORBIT               = ORBIT,                                  &
         INTERNALspec        = INTERNALspec,                           &
         IMPORTspec          = IMPORTspec,                             &
         EXPORTspec          = EXPORTspec,                             &
         INTERNAL_ESMF_STATE = INTERNAL,                               &
                                                             RC=STATUS )
    VERIFY_(STATUS)

! Get parameters from configuration
!----------------------------------
    
    call MAPL_GetResource( MAPL, PRS_LOW_MID,    'PRS_LOW_MID_CLOUDS:' ,   DEFAULT=70000.,      RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource( MAPL, PRS_MID_HIGH,   'PRS_MID_HIGH_CLOUDS:',   DEFAULT=40000.,      RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource( MAPL, CO2,            'CO2:',                                        RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource( MAPL, SC,             'SOLAR_CONSTANT:',                             RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource( MAPL, SUNFLAG,        'SUN_FLAG:',              DEFAULT=0,           RC=STATUS)
    VERIFY_(STATUS)

! Should we load balance solar radiation?
! For the single-column model, we always use the
! DATMO DYCORE. If this is our DYCORE, turn off
! load balancing.
!---------------------------------------------

    call MAPL_GetResource( MAPL, DYCORE,            'DYCORE:'                       , RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_GetResource( MAPL, SOLAR_LOAD_BALANCE,'SOLAR_LOAD_BALANCE:', DEFAULT=1, RC=STATUS)
    VERIFY_(STATUS)

    if (adjustl(DYCORE)=="DATMO" .OR. SOLAR_LOAD_BALANCE==0) then
       LoadBalance = .FALSE. 
    else
       LoadBalance = .TRUE.
    end if

! Use time-varying co2
!---------------------

    call ESMF_ClockGet(CLOCK, currTIME=CURRENTTIME,       RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_TimeGet (CURRENTTIME, YY=YY, DayOfYear=DOY, RC=STATUS)
    VERIFY_(STATUS)

    if(CO2<0.0) then
       CO2 = GETCO2(YY,DOY)
       write(MSGSTRING,'(A,I4,A,I3,A,e12.5)') &
            "Updated CO2 in solar for year/day ", YY, "/", DOY, " is ", CO2
       if( MAPL_AM_I_ROOT() ) then
          if( CO2_0.ne.CO2 ) then
             CO2_0  = CO2
             print *
             print *, trim(msgstring)
             print *
          endif
       endif
       call ESMF_LogWrite(MSGSTRING, ESMF_LOGMSG_INFO, rc=STATUS)
       VERIFY_(STATUS)
    end if

    call MAPL_GetResource( MAPL, SolCycFileName, "SOLAR_CYCLE_FILE_NAME:", DEFAULT='/dev/null', RC=STATUS)
    VERIFY_(STATUS) 

    ! Test if we are using RRTMG
    call MAPL_GetResource( MAPL, RRTMG_SORAD ,'USE_RRTMG_SORAD:', DEFAULT=0.0, RC=STATUS)
    VERIFY_(STATUS)

    USE_RRTMG = RRTMG_SORAD /= 0.0
    USE_CHOU  = .not. USE_RRTMG

    ! Decide if should make OBIO exports
    call MAPL_GetResource ( MAPL, DO_OBIO, Label="USE_OCEANOBIOGEOCHEM:",DEFAULT=0, RC=STATUS)

    if (DO_OBIO/=0) then
       SOLAR_TO_OBIO = .TRUE.
    else
       SOLAR_TO_OBIO = .FALSE.
    endif

    if(SolCycFileName /= '/dev/null') THEN

       call MAPL_GetResource( MAPL, USE_NRLSSI2, "USE_NRLSSI2:", DEFAULT=.TRUE., RC=STATUS)
       VERIFY_(STATUS)

       if (USE_NRLSSI2) then

         ! For now only use with RRTMG SW
         ASSERT_(USE_RRTMG)

         call MAPL_SunGetSolarConstant(CLOCK,trim(SolCycFileName),SC,MG,SB,rc=STATUS)
         VERIFY_(STATUS)

         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         ! write(MSGSTRING,'(A,I4,A,I3,A,F8.3,A,F8.6,A,F9.4)') &                                          !
         !       "Solar Constants for year/day ", YY, "/", DOY, " are SC: ", SC, " MG: ", MG, " SB: ", SB !
         ! if( MAPL_AM_I_ROOT() ) then                                                                    !
         !    if( SC_0.ne.SC ) then                                                                       !
         !       SC_0  = SC                                                                               !
         !       MG_0  = MG                                                                               !
         !       SB_0  = SB                                                                               !
         !       print *                                                                                  !
         !       print *, trim(msgstring)                                                                 !
         !       print *                                                                                  !
         !    endif                                                                                       !
         ! endif                                                                                          !
         ! call ESMF_LogWrite(MSGSTRING, ESMF_LOGMSG_INFO, rc=STATUS)                                     !
         ! VERIFY_(STATUS)                                                                                !
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       else
         call MAPL_SunGetSolarConstant(CLOCK,trim(SolCycFileName),SC,HK=HK,rc=STATUS)
         VERIFY_(STATUS)

         HK_UV_TEMP = HK(:5)
   
         do K=1,3
            HK_IR_TEMP(K,:)=HK_IR_OLD(K,:)*(HK(5+K)/sum(HK_IR_OLD(K,:)))
         end do
       end if
    else if(SC<0.0) then
       call MAPL_SunGetSolarConstant(CURRENTTIME,SC,HK, rc=STATUS)
       VERIFY_(STATUS) 

       HK_UV_TEMP = HK(:5)
 
       do K=1,3
          HK_IR_TEMP(K,:)=HK_IR_OLD(K,:)*(HK(5+K)/sum(HK_IR_OLD(K,:)))
       end do

       write(MSGSTRING,'(A,I4,A,I3,A,e12.5)') &
            "Updated Solar Constant for year/day ", YY, "/", DOY, " is ", SC
       if( MAPL_AM_I_ROOT() ) then
          if( SC_0.ne.SC ) then
             SC_0  = SC
             print *
             print *, trim(msgstring)
             print *
          endif
       endif
       call ESMF_LogWrite(MSGSTRING, ESMF_LOGMSG_INFO, rc=STATUS)
       VERIFY_(STATUS)
    else
       HK_UV_TEMP = HK_UV_OLD
       HK_IR_TEMP = HK_IR_OLD
    end if


! We use the reference pressures to separate high, middle, and low clouds.
!-------------------------------------------------------------------------

    call MAPL_GetPointer(IMPORT, PREF, 'PREF', RC=STATUS)
    VERIFY_(STATUS)

! Determine the model level seperating high-middle and low-middle clouds
!-----------------------------------------------------------------------

    ASSERT_(PRS_LOW_MID >  PRS_MID_HIGH)
    ASSERT_(PRS_LOW_MID <  PREF(LM)    )
    
    K = 1
    do while ( PREF(K) < PRS_MID_HIGH )
      k=k+1
    end do
    LCLDMH = K
    do while ( PREF(K) < PRS_LOW_MID  )
      k=k+1
    end do
    LCLDLM = K

    call MAPL_TimerOff(MAPL,"PRELIMS",RC=STATUS); VERIFY_(STATUS)

! Determine calling sequence
! This getresource is a kludge for now and needs to be fixed 
! in the spec, because GG needs thins info to 
! know when to set the alarm, last or first step of interval
! right now it is always the last, which is only correct for called_last=1.
!---------------------------

    call MAPL_TimerOn (MAPL,"UPDATE",  RC=STATUS); VERIFY_(STATUS)

    call MAPL_GetResource(MAPL,CalledLast,'CALLED_LAST:', default=1, RC=STATUS)
    VERIFY_(STATUS)

    UPDATE_FIRST = CalledLast /= 0

! Update the Sun position and weigh the export variables
! --------------------------------------------------------
   
    if(UPDATE_FIRST) then
       call UPDATE_EXPORT(IM,JM,LM,     RC=STATUS); VERIFY_(STATUS)
    end if

    call MAPL_TimerOff(MAPL,"UPDATE",  RC=STATUS); VERIFY_(STATUS)

! If its time, refresh the internal state
! ---------------------------------------

    REFRESH_FLUXES = ESMF_AlarmIsRinging( ALARM, rc=status)
    VERIFY_(STATUS)
       
    REFRESH: if ( REFRESH_FLUXES ) then
       call MAPL_TimerOn (MAPL,"REFRESH", RC=STATUS); VERIFY_(STATUS)

       call ESMF_AlarmRingerOff(ALARM,                 RC=STATUS); VERIFY_(STATUS)
       call ESMF_ClockGet(CLOCK, currTIME=CURRENTTIME, RC=STATUS); VERIFY_(STATUS)

       if(UPDATE_FIRST) then
          call ESMF_ClockGet(CLOCK, timeSTEP=INTDT,    RC=STATUS); VERIFY_(STATUS)
       else
          call ESMF_TimeIntervalSet(INTDT, s=0,        RC=STATUS); VERIFY_(STATUS)
       end if

       ! Test if we are using RRTMG
       call MAPL_GetResource( MAPL, RRTMG_SORAD ,'USE_RRTMG_SORAD:', DEFAULT=0.0, RC=STATUS)
       VERIFY_(STATUS)

       USE_RRTMG = RRTMG_SORAD /= 0.0
       USE_CHOU  = .not. USE_RRTMG

       if (USE_RRTMG) then
           NUM_BANDS_SOLAR = NB_RRTMG
       else
           NUM_BANDS_SOLAR = NB_CHOU
       end if

       call MAPL_TimerOn (MAPL,"-AEROSOLS", RC=STATUS); VERIFY_(STATUS)

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
               call MAPL_GetPointer(IMPORT, AS_PTR_PLE, 'PLE',  RC=STATUS)
               VERIFY_(STATUS)

               call MAPL_GetPointer(IMPORT, AS_PTR_Q,   'QV',   RC=STATUS)
               VERIFY_(STATUS)

               call MAPL_GetPointer(IMPORT, AS_PTR_T,   'T',    RC=STATUS)
               VERIFY_(STATUS)

               allocate(AS_ARR_RH(IM,JM,LM), AS_ARR_PL(IM,JM,LM), stat=STATUS)
               VERIFY_(STATUS)

               AS_ARR_PL = 0.5*(AS_PTR_PLE(:,:,1:LM) + AS_PTR_PLE(:,:,0:LM-1))

               AS_ARR_RH = AS_PTR_Q / MAPL_EQSAT(AS_PTR_T, PL=AS_ARR_PL)
     
               call MAPL_GetPointer(AERO, AS_PTR_3D, trim(AS_FIELD_NAME), RC=STATUS)
               VERIFY_(STATUS)

               AS_PTR_3D = AS_ARR_RH
      
               deallocate(AS_ARR_RH, AS_ARR_PL, stat=STATUS)
               VERIFY_(STATUS)
           end if

           ! set PLE for aerosol optics
           call ESMF_AttributeGet(AERO, name='air_pressure_for_aerosol_optics', value=AS_FIELD_NAME, RC=STATUS)
           VERIFY_(STATUS)

           if (AS_FIELD_NAME /= '') then
               call MAPL_GetPointer(IMPORT, AS_PTR_PLE, 'PLE',  RC=STATUS)
               VERIFY_(STATUS)

               call MAPL_GetPointer(AERO, AS_PTR_3D, trim(AS_FIELD_NAME), RC=STATUS)
               VERIFY_(STATUS)
           
               AS_PTR_3D = AS_PTR_PLE
           end if

           ! allocate memory for total aerosol ext, ssa and asy at all solar bands
           allocate(AEROSOL_EXT(IM,JM,LM,NUM_BANDS_SOLAR),  &
                    AEROSOL_SSA(IM,JM,LM,NUM_BANDS_SOLAR),  &
                    AEROSOL_ASY(IM,JM,LM,NUM_BANDS_SOLAR),  stat=STATUS)
           VERIFY_(STATUS)

           AEROSOL_EXT = 0.0
           AEROSOL_SSA = 0.0
           AEROSOL_ASY = 0.0

           ! compute aerosol optics at all solar bands
           SOLAR_BANDS: do band = 1, NUM_BANDS_SOLAR
               call ESMF_AttributeSet(AERO, name='band_for_aerosol_optics', value=(BANDS_SOLAR_OFFSET+band), RC=STATUS)
               VERIFY_(STATUS)

               ! execute the aero provider's optics method 
               call ESMF_MethodExecute(AERO, label="aerosol_optics", userRC=AS_STATUS, RC=STATUS)
               VERIFY_(AS_STATUS)
               VERIFY_(STATUS)

               ! EXT from AERO_PROVIDER
               call ESMF_AttributeGet(AERO, name='extinction_in_air_due_to_ambient_aerosol', value=AS_FIELD_NAME, RC=STATUS)
               VERIFY_(STATUS)

               if (AS_FIELD_NAME /= '') then
                   call MAPL_GetPointer(AERO, AS_PTR_3D, trim(AS_FIELD_NAME),  RC=STATUS)
                   VERIFY_(STATUS)

                   if (associated(AS_PTR_3D)) then
                       AEROSOL_EXT(:,:,:,band) = AS_PTR_3D
                   end if
               end if

               ! SSA from AERO_PROVIDER
               call ESMF_AttributeGet(AERO, name='single_scattering_albedo_of_ambient_aerosol', value=AS_FIELD_NAME, RC=STATUS)
               VERIFY_(STATUS)

               if (AS_FIELD_NAME /= '') then
                   call MAPL_GetPointer(AERO, AS_PTR_3D, trim(AS_FIELD_NAME),  RC=STATUS)
                   VERIFY_(STATUS)

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
           end do SOLAR_BANDS

       end if RADIATEVELY_ACTIVE_AEROSOLS

       call MAPL_TimerOff(MAPL,"-AEROSOLS", RC=STATUS); VERIFY_(STATUS)

! No aerosol diagnostics
!-----------------------

       call MAPL_GetPointer(EXPORT, FSWNA,     'FSWNA',  RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(EXPORT, FSCNA,     'FSCNA',  RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(EXPORT, FSWUNA,    'FSWUNA', RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(EXPORT, FSCUNA,    'FSCUNA', RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(EXPORT, FSWBANDNA, 'FSWBANDNA',  RC=STATUS)
       VERIFY_(STATUS)

       call MAPL_GetPointer(EXPORT, RSRSNA, 'RSRSNA', RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(EXPORT, RSCSNA, 'RSCSNA', RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(EXPORT, RSRNA,  'RSRNA',  RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(EXPORT, RSCNA,  'RSCNA',  RC=STATUS)
       VERIFY_(STATUS)
       
       if( associated(FSWNA)     .or. associated(FSCNA)  .or.   &
           associated(FSWUNA)    .or. associated(FSCUNA) .or.   &
           associated(FSWBANDNA) .or.                           &

           associated(RSRNA)     .or. associated(RSCNA)  .or.   &
           associated(RSRSNA)    .or. associated(RSCSNA) ) then
           call SORADCORE(IM,JM,LM,            &
                NO_AERO  = .true.,             &
                CURRTIME = CURRENTTIME+INTDT,  &
                LoadBalance = LoadBalance,     &
                RC=STATUS )
           VERIFY_(STATUS)
       else
          call MAPL_GetPointer(INTERNAL, FSWNA,     'FSWNAN',  RC=STATUS)
          VERIFY_(STATUS)
          call MAPL_GetPointer(INTERNAL, FSWUNA,    'FSWUNAN', RC=STATUS)
          VERIFY_(STATUS)
          call MAPL_GetPointer(INTERNAL, FSCNA,     'FSCNAN',  RC=STATUS)
          VERIFY_(STATUS)
          call MAPL_GetPointer(INTERNAL, FSCUNA,    'FSCUNAN', RC=STATUS)
          VERIFY_(STATUS)
          call MAPL_GetPointer(INTERNAL, FSWBANDNA, 'FSWBANDNAN',  RC=STATUS)
          VERIFY_(STATUS)
          FSWNA = 0.0
          FSCNA = 0.0
          FSWUNA = 0.0
          FSCUNA = 0.0
          FSWBANDNA = 0.0
       end if

       call SORADCORE(IM,JM,LM,                    &
                      NO_AERO  = .false.,          &
                      CURRTIME = CURRENTTIME+INTDT,&
                      LoadBalance = LoadBalance,   &
                      RC=STATUS )
       VERIFY_(STATUS)

       if (implements_aerosol_optics) then
           deallocate(AEROSOL_EXT, __STAT__)
           deallocate(AEROSOL_SSA, __STAT__)
           deallocate(AEROSOL_ASY, __STAT__)
       end if

       call MAPL_GetPointer(INTERNAL, DRUVRN, 'DRUVRN', RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(INTERNAL, DFUVRN, 'DFUVRN', RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(INTERNAL, DRPARN, 'DRPARN', RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(INTERNAL, DFPARN, 'DFPARN', RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(INTERNAL, DRNIRN, 'DRNIRN', RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(INTERNAL, DFNIRN, 'DFNIRN', RC=STATUS)
       VERIFY_(STATUS)

       call MAPL_GetPointer(EXPORT  , DRUVR,  'DRUVRN', RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(EXPORT  , DFUVR,  'DFUVRN', RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(EXPORT  , DRPAR,  'DRPARN', RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(EXPORT  , DFPAR,  'DFPARN', RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(EXPORT  , DRNIR,  'DRNIRN', RC=STATUS)
       VERIFY_(STATUS)
       call MAPL_GetPointer(EXPORT  , DFNIR,  'DFNIRN', RC=STATUS)
       VERIFY_(STATUS)

       if(associated( DRUVR))  DRUVR = DRUVRN
       if(associated( DFUVR))  DFUVR = DFUVRN
       if(associated( DRPAR))  DRPAR = DRPARN
       if(associated( DFPAR))  DFPAR = DFPARN
       if(associated( DRNIR))  DRNIR = DRNIRN
       if(associated( DFNIR))  DFNIR = DFNIRN

       call MAPL_TimerOff(MAPL,"REFRESH", RC=STATUS); VERIFY_(STATUS)
    endif REFRESH


! Update the Sun position and weigh the export variables
! --------------------------------------------------------
    
    if(.not.UPDATE_FIRST) then
       call MAPL_TimerOn (MAPL,"UPDATE",  RC=STATUS); VERIFY_(STATUS)
          call UPDATE_EXPORT(IM,JM,LM,    RC=STATUS); VERIFY_(STATUS)
       call MAPL_TimerOff(MAPL,"UPDATE",  RC=STATUS); VERIFY_(STATUS)
    end if

! All done
!---------

    call MAPL_TimerOff(MAPL,"TOTAL",RC=STATUS); VERIFY_(STATUS)
    RETURN_(ESMF_SUCCESS)

  contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine SORADCORE(IM,JM,LM,NO_AERO,CURRTIME,LoadBalance,RC)
      integer,           intent(IN ) :: IM, JM, LM
      logical,           intent(IN ) :: NO_AERO
      type (ESMF_Time),  intent(IN ) :: CURRTIME
      logical,           intent(IN ) :: LoadBalance
      integer, optional, intent(OUT) :: RC

!  Locals

      character(len=ESMF_MAXSTR)      :: IAm
      integer                         :: STATUS

      type (ESMF_TimeInterval)        :: TINT
      type (ESMF_DELayout)            :: LAYOUT
      type (ESMF_Array)               :: ARRAY
      type (ESMF_FieldBundle)         :: BUNDLE
      type (ESMF_Field)               :: FIELD

      type (ESMF_VM)                  :: VM
      integer                         :: COMM

      real,    dimension(IM,JM)       :: ZTH, SLR
      logical, dimension(IM,JM)       :: DAYTIME

!  DAYTIME ONLY COPY OF VARIABLES

      real, pointer, dimension(:  )   :: ALBNR,ALBNF,ALBVR,ALBVF,ZT,    &
                                         UVRR,UVRF,PARR,PARF,NIRR,NIRF
      real, pointer, dimension(:,:,:) :: FCLD,TAUI,TAUW,CLIN,RRL,RRI,RQI,RQL,RQR
      real, pointer, dimension(:,:,:) :: DP, PLL
      real, pointer, dimension(:,:,:) :: RAERO
      real, pointer, dimension(:,:)   :: CLDH,CLDM,CLDL,CLDT,           &
                                         TAUH,TAUM,TAUL,TAUT
      real, pointer, dimension(:,:)   :: T, Q, OX, PLE, CL, QL, QI, QR, QS, &
                                         RL, RI, RR, RS, FSW, FSC, FSWA, FSCA
      real, pointer, dimension(:,:)   :: ALBIMP, ALBINT
                                         
      real, pointer, dimension(:,:)   :: FSWU ! Flux shortwave up all-sky
      real, pointer, dimension(:,:)   :: FSCU ! Flux shortwave up clear-sky
      real, pointer, dimension(:,:)   :: FSWUA ! Flux shortwave up all-sky no aerosol
      real, pointer, dimension(:,:)   :: FSCUA ! Flux shortwave up clear-sky no aerosol
      real, pointer, dimension(:,:)   :: FSWBAND ! Flux shortwave surface per band
      real, pointer, dimension(:,:)   :: FSWBANDA ! Flux shortwave surface per band no aerosol

      real, pointer, dimension(:,:)   :: DRBAND, DFBAND
                                         
      integer :: INFLGSW         ! Flag for cloud optical properties
      integer :: ICEFLGSW        ! Flag for ice particle specification
      integer :: LIQFLGSW        ! Flag for liquid droplet specification
      integer :: ICLD            ! Flag for cloud overlap

      real, pointer, dimension(:  )       :: ALAT
      real, pointer, dimension(:  )       :: TS
      real, pointer, dimension(:,:)       :: CH4, N2O
      real, allocatable, dimension(:,:)   :: SWUFLXR, SWDFLXR, SWUFLXCR, SWDFLXCR

      real, allocatable, dimension(:,:)   :: TLEV, TLEV_R, PLE_R
      real, allocatable, dimension(:,:)   :: FCLD_R, CLIQWP, CICEWP, RELIQ, REICE
      real, allocatable, dimension(:,:,:) :: TAUCLD, SSACLD, ASMCLD, FSFCLD

      real, allocatable, dimension(:,:,:) :: TAUAER, SSAAER, ASMAER, ECAER
      real, allocatable, dimension(:,:)   :: DPR, PL_R, T_R,  Q_R, O2_R, O3_R, ZL_R
      real, allocatable, dimension(:,:)   :: CO2_R, CH4_R, N2O_R
      real, allocatable, dimension(:,:)   :: SWUFLX, SWDFLX, SWHR, SWUFLXC, SWDFLXC, SWHRC 
      real, allocatable, dimension(:)     :: NIRR_R, NIRF_R, PARR_R, PARF_R, UVRR_R, UVRF_R

      real, parameter :: O2 = 0.2090029E+00
      real    :: ADJES, DIST
      integer :: DYOFYR
      integer :: NCOL
      integer :: RPART, IAER, NORMFLX


      logical :: USE_CHOU, USE_RRTMG           ! Logical concerning what radiation to use
      real    :: RRTMG_SORAD                   ! Holds value from AGCM.rc

      logical :: USE_CHOU_IRRAD, &
                 USE_RRTMG_IRRAD               ! Logical concerning what irrad rad is being used
      real    :: RRTMG_IRRAD                   ! Holds value from AGCM.rc

      integer :: NUM_BANDS                     ! Holds value from AGCM.rc
      integer :: TOTAL_RAD_BANDS

      integer, parameter :: NB_CHOU_IRRAD  = 10 ! Number of bands in IRRAD calcs for Chou
      integer, parameter :: NB_RRTMG_IRRAD = 16 ! Number of bands in IRRAD calcs for RRTMG

      integer, parameter :: NB_CHOU  = 8  ! Number of bands in SORAD calcs for Chou
      integer, parameter :: NB_RRTMG = 14 ! Number of bands in SORAD calcs for RRTMG

      integer :: OFFSET
      integer :: NUM_BANDS_SOLAR

      real :: X

      integer                   :: ISOLVAR
      real, dimension(2)        :: INDSOLVAR
      real, dimension(nb_rrtmg) :: BNDSOLVAR
      real                      :: SOLCYCFRAC

! For Aerosol
      integer :: in, idx, ibb, i
      real, pointer, dimension(:,:,:) :: qaero, taua, ssaa, asya

      real, pointer, dimension(:,:,:)     :: BUFIMP_AEROSOL_EXT => null()
      real, pointer, dimension(:,:,:)     :: BUFIMP_AEROSOL_SSA => null()
      real, pointer, dimension(:,:,:)     :: BUFIMP_AEROSOL_ASY => null()
      real, allocatable, dimension(:,:,:) :: BUF_AEROSOL

! Decorrelation length
      real    :: AM1, AM2, AM3, AM4, AMR

      ! CPU Topology information for RRTMGPU
      ! ------------------------------------
      integer        :: CoresPerNode

      character(len=ESMF_MAXSTR)      :: NAME

      integer :: DIMS
      integer :: NumVars
      integer :: L, L1, LN, J, J1, JN, NA, K
      integer :: NUMLIT, NUM2DO

      real, pointer :: QQ3(:,:,:), RR3(:,:,:),  PTR3(:,:,:)
      real, pointer :: PTR2(:,:), RH(:,:), PL(:,:), O3(:,:), PLTMP(:,:)

      character(len=ESMF_MAXSTR), allocatable :: NAMESimp(:), NAMESint(:)
      type (ESMF_FieldBundle)         :: AEROBUNDLE
      integer, allocatable      :: SLICESimp(:),SLICESint(:)
      real, target, allocatable :: BUFIMP(:),BUFINT(:)
      integer :: NUMMAX, NUMimp, NUMint, HorzDims(2), BufLen

      type RealF90Ptr2d
         real, pointer :: PTR(:,:  ) => null()
      end type RealF90Ptr2d
      type RealF90Ptr3d
         real, pointer :: PTR(:,:,:) => null()
      end type RealF90Ptr3d

      type(RealF90Ptr2D), allocatable :: VARSINT(:)
      type(RealF90Ptr3D), allocatable :: VARSIMP(:)

!  Begin...
!----------

      IAm = trim(COMP_NAME)//"Soradcore"
      call MAPL_TimerOn(MAPL,"-MISC")

! Get the average insolation for the next interval
!-------------------------------------------------

      call ESMF_AlarmGet(ALARM, RINGINTERVAL=TINT, RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_SunGetInsolation(LONS, LATS,      &
              ORBIT, ZTH, SLR, &
              INTV  = TINT,    &
              currTime = currTime,   &
              TIME = SUNFLAG,        &
              DIST = DIST,           &
              RC=STATUS )
      VERIFY_(STATUS)

! Test if we are using RRTMG
!---------------------------

      call MAPL_GetResource( MAPL, RRTMG_IRRAD ,'USE_RRTMG_IRRAD:', DEFAULT=0.0, RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetResource( MAPL, RRTMG_SORAD ,'USE_RRTMG_SORAD:', DEFAULT=0.0, RC=STATUS)
      VERIFY_(STATUS)

      USE_RRTMG = RRTMG_SORAD /= 0.0
      USE_CHOU  = .not.USE_RRTMG

      USE_RRTMG_IRRAD = RRTMG_IRRAD /= 0.0
      USE_CHOU_IRRAD  = .not.USE_RRTMG_IRRAD

      ! Set the offset for the SOLAR aerosol bands
      OFFSET = 0 

      if (USE_RRTMG) then
         NUM_BANDS_SOLAR = NB_RRTMG
      else
         NUM_BANDS_SOLAR = NB_CHOU
      end if

! Test to see if AGCM.rc is set up correctly for the Radiation selected
!----------------------------------------------------------------------

      call MAPL_GetResource( MAPL, NUM_BANDS ,'NUM_BANDS:', RC=STATUS)
      VERIFY_(STATUS)

      if (USE_RRTMG .and. USE_RRTMG_IRRAD) TOTAL_RAD_BANDS = NB_RRTMG + NB_RRTMG_IRRAD
      if (USE_RRTMG .and. USE_CHOU_IRRAD ) TOTAL_RAD_BANDS = NB_RRTMG + NB_CHOU_IRRAD
      if (USE_CHOU  .and. USE_RRTMG_IRRAD) TOTAL_RAD_BANDS = NB_CHOU  + NB_RRTMG_IRRAD
      if (USE_CHOU  .and. USE_CHOU_IRRAD ) TOTAL_RAD_BANDS = NB_CHOU  + NB_CHOU_IRRAD

      if (NUM_BANDS /= TOTAL_RAD_BANDS) then
         if (MAPL_am_I_Root()) then
            write (*,*) "NUM_BANDS is not set up correctly for the radiation combination selected:"
            write (*,*) "    SORAD RRTMG: ", USE_RRTMG
            write (*,*) "    IRRAD RRTMG: ", USE_RRTMG_IRRAD
            write (*,*) "Please check that your optics tables and NUM_BANDS are correct."
         end if
         ASSERT_(.FALSE.)
      end if

      call MAPL_TimerOff(MAPL,"-MISC")

!  Load balancing by packing the lit points and sharing work with antipode
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      call MAPL_TimerOn(MAPL,"-BALANCE")

!  Load balancing by packing the lit points and sharing work with night regions
!------------------------------------------------------------------------------

!  Identify lit soundings with the DAYTIME mask
!----------------------------------------------

      if (LoadBalance) then
         DAYTIME = ZTH  >  0.
         NUMLIT  = count(DAYTIME)

!  The load balancer does not work if there are no lit points. This is only
!  important model-wise with the single-column model. Thus, for continuity,
!  we revert to the behaviour of the old load-balancer when running the
!  cubed-sphere. Note we must protect ZTH since in solar, we divide by
!  ZTH and, thus, we will get a divide-by-zero if not protected.
!--------------------------------------------------------------------------

      else 
         ZTH     = max(.0001,ZTH)
         DAYTIME = .true.
         NUMLIT  = size(ZTH)
      end if

!  Create a balancing strategy. This is a collective call on the
!  communicator of the current VM. The original, unbalanced local work consists
!  of (OrgLen) NUMLIT soundings, which may be zero. The local work after implementing
!  the strategy consists of (BalLen) NUM2DO soundings, which is generally non-zero.
!  The data movement to implement this strategy will occur when MAPL_BalanceWork
!  is called to "distribute" excess work to less busy processors and later to 
!  "retrieve" that work to its home processor. Because the data balancing will be
!  done "in place", the in-out buffer must be large enough to accomodate the data
!  held at each stage (pass) of the balancing. This can be larger than the max of
!  the initial and final sizes; so the required size is passed back in BufLen.
!------------------------------------------------------------------------------------

      call ESMF_VMGetCurrent(VM, RC=STATUS)
      VERIFY_(STATUS)

      call ESMF_VMGet(VM, mpiCommunicator=COMM, RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOn(MAPL,"--CREATE")

      call MAPL_BalanceCreate(OrgLen=NUMLIT, Comm=COMM, Handle=SolarBalanceHandle, BalLen=NUM2DO, BufLen=NUMMAX, rc=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOff(MAPL,"--CREATE")

! The number of input and output variables to the calculations.
!  The input number is two greater than the IMPORTS because the
!  component get the LATS and cos(zth) from MAPL. The outputs 
!  are the INTERNAL variables being refreshed.
!--------------------------------------------------------------

      NumImp = size(IMPORTspec) + 2
      NumInt = size(INTERNALspec)

      allocate(SLICESimp(NumImp), NAMESimp(NumImp), &
               SLICESint(NumInt), NAMESint(NumInt), stat=STATUS)
      VERIFY_(STATUS)

      HorzDims = (/IM,JM/)

!  Count the 2D slices in the input variables and calculate
!   the required length of the 1D input buffer (BUFIMP).
!----------------------------------------------------------

      BUFLEN = 0

      INPUT_VARS_1: do K=1,NumImp

         if(K<NumImp-1) then
            call MAPL_VarSpecGet(IMPORTspec(K), SHORT_NAME=NAMESimp(K), DIMS=DIMS, RC=STATUS)
            VERIFY_(STATUS)
         else if (K == NumImp-1) then
            NAMESimp(K) = "LATS"
            DIMS = MAPL_DIMSHORZONLY
         else
            NAMESimp(K) = "ZTH"
            DIMS = MAPL_DIMSHORZONLY
         end if

         if(DIMS == MAPL_DIMSVERTONLY) then ! Skip PREF

            SLICESimp(K) = 0

         else

! If Import is the aerosol bundle, we need to set NA and the list
!  of aerosol names. Note that we are assuming all aerosol species
!  are dimensions by LM levels. This will be asserted later.
!-----------------------------------------------------------------


            if (NAMESimp(K)=="AERO") then

               if(NO_AERO) then ! This is a no aerosol call done for "clean" diagnostics
                  NA = 0
               else
                  if (implements_aerosol_optics) then
                     NA = 3
                  else
                     NA = 0
                  end if
               end if

               SLICESimp(K) = LM*NA*NUM_BANDS_SOLAR
            else  ! Import is not the aerosol bundle

               select case(DIMS)               
               case(MAPL_DIMSHORZVERT) ! We currently assume this case is 3D
                  call ESMFL_StateGetPointerToData(IMPORT, PTR3, NAMESimp(K), RC=STATUS)
                  SLICESimp(K) = size(PTR3,3)

               case(MAPL_DIMSHORZONLY)
                  SLICESimp(K) = 1

               case default
                  ASSERT_(.false.)
               end select

            end if  ! Special treatment for AERO Import Bundle
         end if

         BUFLEN = BUFLEN + NUMMAX*SLICESimp(K)

      enddo INPUT_VARS_1

!  Allocate the buffer that will hold all balanced variables. The "inner"
!   dimension of its 2D representation must ne NUMMAX---the larger of the
!   balanced and unbalanced runs.
!------------------------------------------------------------------------

      allocate(BUFIMP(BUFLEN),stat=STATUS)
      VERIFY_(STATUS)

      BUFIMP = MAPL_UNDEF

!  Loop over imports, packing into the buffer that will be 
!   load balanced and used in the SORAD calculations.
!---------------------------------------------------------

      LN = 0

      INPUT_VARS_2: do K=1, NumImp
         L1 = LN + 1

         if(SLICESimp(K) > 0) then ! Skip PREF

            if (NAMESimp(K)=="AERO") then

               ASSERT_(size(AEROSOL_EXT,3)==LM)
               ASSERT_(size(AEROSOL_SSA,3)==LM)
               ASSERT_(size(AEROSOL_ASY,3)==LM)

               allocate(BUF_AEROSOL(size(AEROSOL_EXT,1), &
                                    size(AEROSOL_EXT,2), &
                                    size(AEROSOL_EXT,3)), stat=STATUS)
               VERIFY_(STATUS)

               BUF_AEROSOL = MAPL_UNDEF
               do J=1,NUM_BANDS_SOLAR
                   BUF_AEROSOL = AEROSOL_EXT(:,:,:,J)

                   call ReOrder(BUFIMP(L1 + (J-1)*LM*NUMMAX),BUF_AEROSOL,DAYTIME,NUMMAX,&
                        HorzDims,LM,PACKIT)
               end do

               LN = L1 + NUMMAX*LM*NUM_BANDS_SOLAR - 1
                
               PTR3(1:NUMMAX,1:LM,1:NUM_BANDS_SOLAR) => BUFIMP(L1:LN)
               BUFIMP_AEROSOL_EXT => PTR3(1:NUM2DO,:,:)

               L1 = LN + 1
               
               BUF_AEROSOL = MAPL_UNDEF
               do J=1,NUM_BANDS_SOLAR
                   BUF_AEROSOL = AEROSOL_SSA(:,:,:,J)

                   call ReOrder(BUFIMP(L1 + (J-1)*LM*NUMMAX),BUF_AEROSOL,DAYTIME,NUMMAX,&
                        HorzDims,LM,PACKIT)
               end do

               LN = L1 + NUMMAX*LM*NUM_BANDS_SOLAR - 1

               PTR3(1:NUMMAX,1:LM,1:NUM_BANDS_SOLAR) => BUFIMP(L1:LN)
               BUFIMP_AEROSOL_SSA => PTR3(1:NUM2DO,:,:)
               
               L1 = LN + 1
            
               BUF_AEROSOL = MAPL_UNDEF
               do J=1,NUM_BANDS_SOLAR

                   BUF_AEROSOL = AEROSOL_ASY(:,:,:,J)
                   call ReOrder(BUFIMP(L1 + (J-1)*LM*NUMMAX),BUF_AEROSOL,DAYTIME,NUMMAX,&
                        HorzDims,LM,PACKIT)
               end do

               LN = L1 + NUMMAX*LM*NUM_BANDS_SOLAR - 1

               PTR3(1:NUMMAX,1:LM,1:NUM_BANDS_SOLAR) => BUFIMP(L1:LN)
               BUFIMP_AEROSOL_ASY => PTR3(1:NUM2DO,:,:)

               deallocate(BUF_AEROSOL, stat=STATUS)
               VERIFY_(STATUS)
            else
               if (SLICESimp(K) /= 1) then
                  call ESMFL_StateGetPointerToData(IMPORT, PTR3, NAMESimp(K), RC=STATUS)
                  VERIFY_(STATUS)
                  call ReOrder(BUFIMP(L1),Ptr3,DAYTIME,NUMMAX,HorzDims,size(Ptr3,3),PACKIT)

                  LN = L1 + NUMMAX*size(PTR3,3) - 1

               else  ! case(MAPL_DIMSHORZONLY)
                  if (trim(NAMESimp(K)) /= 'ZTH' .AND. trim(NAMESimp(K)) /= 'LATS') then
                     call ESMFL_StateGetPointerToData(IMPORT, PTR2, NAMESimp(K), RC=STATUS)
                     VERIFY_(STATUS)
                     call ReOrder(BUFIMP(L1),Ptr2,DAYTIME,NUMMAX,HorzDims,1,PACKIT)
                  else if (trim(NAMESimp(K)) == 'LATS') then
                     call ReOrder(BUFIMP(L1),LATS,DAYTIME,NUMMAX,HorzDims,1,PACKIT)
                  else if (trim(NAMESimp(K)) == 'ZTH') then
                     call ReOrder(BUFIMP(L1), ZTH,DAYTIME,NUMMAX,HorzDims,1,PACKIT)
                  end if

                  LN = L1 + NUMMAX - 1

               end if

! Handles for the working input (Import) variables.
!  These use Fortran 2003 syntax for reshaping a 1D
!  vector into a higher rank array.
!--------------------------------------------------

               PTR2(1:NUMMAX,1:SLICESimp(K)) => BUFIMP(L1:LN)

               select case(NAMESimp(K))
               case('PLE')
                  PLE   => PTR2(1:NUM2DO,:)
               case('TS')
                  TS    => PTR2(1:NUM2DO,1)
               case('CH4')
                  CH4   => PTR2(1:NUM2DO,:)
               case('N2O')
                  N2O   => PTR2(1:NUM2DO,:)
               case('T')     
                  T     => PTR2(1:NUM2DO,:)
               case('QV')    
                  Q     => PTR2(1:NUM2DO,:)
               case('OX')    
                  OX    => PTR2(1:NUM2DO,:)
               case('FCLD')  
                  CL    => PTR2(1:NUM2DO,:)
               case('QL')    
                  QL    => PTR2(1:NUM2DO,:)
               case('QI')    
                  QI    => PTR2(1:NUM2DO,:)
               case('QR')    
                  QR    => PTR2(1:NUM2DO,:)
               case('QS')    
                  QS    => PTR2(1:NUM2DO,:)
               case('RL')    
                  RL    => PTR2(1:NUM2DO,:)
               case('RI')    
                  RI    => PTR2(1:NUM2DO,:)
               case('RR')    
                  RR    => PTR2(1:NUM2DO,:)
               case('RS')    
                  RS    => PTR2(1:NUM2DO,:)
               case('ALBVR') 
                  ALBVR => PTR2(1:NUM2DO,1)
               case('ALBVF') 
                  ALBVF => PTR2(1:NUM2DO,1)
               case('ALBNR') 
                  ALBNR => PTR2(1:NUM2DO,1)
               case('ALBNF')   
                  ALBNF => PTR2(1:NUM2DO,1)
               case('LATS')   
                  ALAT  => PTR2(1:NUM2DO,1)
               case('ZTH')   
                  ZT    => PTR2(1:NUM2DO,1)
               end select

            end if ! AERO vs non AERO input

         end if ! skip PREF

      enddo INPUT_VARS_2

! Load balance the IMPORTS
!-------------------------

      call MAPL_TimerOn(MAPL,"--DISTRIBUTE")

      call MAPL_BalanceWork(BUFIMP, NUMMAX, Direction=MAPL_Distribute, Handle=SolarBalanceHandle, RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOff(MAPL,"--DISTRIBUTE")

! Count the slices of internals, which will hold the intent(OUT) results of the
!   calculations. In Solar calculations there are no INOUT variables.
!------------------------------------------------------------------------------

      OUTPUT_VARS_1: do K=1,NumInt

         call MAPL_VarSpecGet(INTERNALspec(K),DIMS=DIMS,SHORT_NAME=NAMESint(K),RC=STATUS)
         VERIFY_(STATUS)

         if(        NO_AERO .and.                                           &
                 ( 'FSWN'      ==trim(NAMESint(K)) .or.    'FSCN'==trim(NAMESint(K)) .or. &       
                   'FSWUN'     ==trim(NAMESint(K)) .or.   'FSCUN'==trim(NAMESint(K)) .or. &
                   'FSWBANDN'  ==trim(NAMESint(K))                                )&
           .or.                                                             &
               .not.NO_AERO .and.                                           &
                 ( 'FSWNAN'    ==trim(NAMESint(K)) .or.  'FSCNAN'==trim(NAMESint(K)) .or. &       
                   'FSWUNAN'   ==trim(NAMESint(K)) .or. 'FSCUNAN'==trim(NAMESint(K)) .or. &
                   'FSWBANDNAN'==trim(NAMESint(K))                                )&
            )   then

            SLICESint(K) = 0
            cycle
         end if

         ! Don't calculate D[RF]BANDN for NO_AERO
         if (NO_AERO) then
            if (NAMESint(k) == 'DRBANDN' .or. NAMESint(k) == 'DFBANDN') then
               SLICESint(k) = 0
               cycle
            end if
         end if

         if(  DIMS==MAPL_DIMSHORZVERT .or. &
             NAMESint(k) == 'FSWBANDN'   .or. &
             NAMESint(k) == 'FSWBANDNAN' .or. &
             NAMESint(k) == 'DRBANDN'    .or. &
             NAMESint(k) == 'DFBANDN'         &
           ) then

            call ESMFL_StateGetPointerToData(INTERNAL, PTR3, NAMESint(K), RC=STATUS)
            VERIFY_(STATUS)

            SLICESint(K) = size(PTR3,3)

         elseif(DIMS==MAPL_DIMSHORZONLY) then
            SLICESint(K) = 1            

         else
            ASSERT_(.false.)

         end if

      enddo OUTPUT_VARS_1

! Allocate the output buffer with enough space to hold both the 
!  balanced and unbalanced data associated with the local PE.
!--------------------------------------------------------------

      allocate(BUFINT(NUMMAX*sum(SLICESint)),stat=STATUS)
      VERIFY_(STATUS)

! Handles for the working output (Internal) variables.
!  These have an inner dimension of the balanced work. 
!-----------------------------------------------------

      L1 = 1

      OUTPUT_VARS_2: do K=1,size(INTERNALspec)
         if(SLICESint(K)==0) cycle

         LN = L1 + SLICESint(K)*NUMMAX - 1
         PTR2(1:NUMMAX,1:SLICESint(K))=>BUFINT(L1:LN)
         L1 = LN + 1
         
         select case(NAMESint(K))
         case('FSWN')    
            FSW       => PTR2(1:NUM2DO,:)   
         case('FSCN')    
            FSC       => PTR2(1:NUM2DO,:)               
         case('FSWUN')   
            FSWU      => PTR2(1:NUM2DO,:)           
         case('FSCUN')   
            FSCU      => PTR2(1:NUM2DO,:)               
         case('FSWBANDN')
            FSWBAND   => PTR2(1:NUM2DO,:)               
         case('DRUVRN')  
            UVRR      => PTR2(1:NUM2DO,1)
         case('DFUVRN')  
            UVRF      => PTR2(1:NUM2DO,1)
         case('DRPARN')  
            PARR      => PTR2(1:NUM2DO,1)
         case('DFPARN')  
            PARF      => PTR2(1:NUM2DO,1)
         case('DRNIRN')  
            NIRR      => PTR2(1:NUM2DO,1)
         case('DFNIRN')  
            NIRF      => PTR2(1:NUM2DO,1)
         case('DRBANDN')
            DRBAND    => ptr2(1:Num2do,:)
         case('DFBANDN')
            DFBAND    => ptr2(1:Num2do,:)
         case('FSWNAN')  
            FSWA      => PTR2(1:NUM2DO,:)               
         case('FSCNAN')  
            FSCA      => PTR2(1:NUM2DO,:)               
         case('FSWUNAN') 
            FSWUA     => PTR2(1:NUM2DO,:)                              
         case('FSCUNAN') 
            FSCUA     => PTR2(1:NUM2DO,:)                              
         case('FSWBANDNAN')
            FSWBANDA  => PTR2(1:NUM2DO,:)                              
         end select

      enddo OUTPUT_VARS_2

      call MAPL_TimerOff(MAPL,"-BALANCE")

! Do shortwave calculations on a list of soundings
!-------------------------------------------------
!-------------------------------------------------

      call MAPL_TimerOn(MAPL,"-MISC")

      if(NO_AERO) then
         FSW  => FSWA
         FSC  => FSCA
         FSWU => FSWUA
         FSCU => FSCUA
      FSWBAND => FSWBANDA
      end if

      allocate(QQ3(size(Q,1),size(Q,2),4),STAT=STATUS)
      VERIFY_(STATUS)

      allocate(RR3(size(Q,1),size(Q,2),4),STAT=STATUS)
      VERIFY_(STATUS)

      allocate(RH (size(Q,1),size(Q,2)  ),STAT=STATUS)
      VERIFY_(STATUS)

      allocate(PL (size(Q,1),size(Q,2)  ),STAT=STATUS)
      VERIFY_(STATUS)

      allocate(O3 (size(Q,1),size(Q,2)  ),STAT=STATUS)
      VERIFY_(STATUS)

      PL = 0.5*(PLE(:,:UBOUND(PLE,2)-1)+PLE(:,LBOUND(PLE,2)+1:))
      RH = Q/MAPL_EQSAT(T,PL=PL)

! Water ammounts and effective radii are in arrays indexed by species
!--------------------------------------------------------------------

      QQ3(:,:,1) = QI
      QQ3(:,:,2) = QL
      QQ3(:,:,3) = QR
      QQ3(:,:,4) = QS
      RR3(:,:,1) = RI*1.e6
      RR3(:,:,2) = RL*1.e6
      RR3(:,:,3) = RR*1.e6
      RR3(:,:,4) = RS*1.e6

! Convert odd oxygen, which is the model prognostic, to ozone
!--------------------------------------------------------------

      O3 = OX
      WHERE(PL < 100.)
         O3 = O3*EXP(-1.5*(LOG10(PL)-2.)**2)
      ENDWHERE

! SORAD expects ozone fraction by volume
!---------------------------------------

      O3 = O3*(MAPL_O3MW / MAPL_AIRMW)

! Assure positive ozone before doing shortwave
! --------------------------------------------

      O3 = MAX(O3, 0.00)

      ! Prepare for aerosol calculations
      ! --------------------------------

      allocate(PLTMP (size(PLE,1),size(PLE,2)  ),STAT=STATUS)
      VERIFY_(STATUS)

      PLTMP = PLE * 0.01  !convert Pa to mb for Sorad and aerosol opt props

      ! ------------------
      ! Begin aerosol code
      ! ------------------

      ! Allocate per-band aerosol arrays for shortwave code
      ! ---------------------------------------------------

      allocate(TAUA  (size(Q,1),size(Q,2),NUM_BANDS_SOLAR),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(SSAA  (size(Q,1),size(Q,2),NUM_BANDS_SOLAR),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(ASYA  (size(Q,1),size(Q,2),NUM_BANDS_SOLAR),STAT=STATUS)
      VERIFY_(STATUS)

      ! Zero out aerosol arrays. If NA == 0, these zeroes are then used inside the code.
      ! --------------------------------------------------------------------------------

      TAUA  = 0.0
      SSAA  = 0.0
      ASYA  = 0.0

      ! If we have aerosols, accumulate the arrays
      ! ------------------------------------------

      if (NA > 0) then
         TAUA = BUFIMP_AEROSOL_EXT
         SSAA = BUFIMP_AEROSOL_SSA
         ASYA = BUFIMP_AEROSOL_ASY
      end if

      call MAPL_TimerOff(MAPL,"-MISC")

   SCHEME: if (USE_CHOU) then
      call shrtwave(                                          &
                     PLTMP, T, Q, O3, CO2, ZT,                &
                     QQ3, RR3, CL,                            &
                     LCLDMH,LCLDLM,                           &
                     ALBVR, ALBVF, ALBNR, ALBNF,              &
                     TAUA,SSAA,ASYA,                          &

                     FSW ,FSC ,                               &
                     NIRR,NIRF,PARR,PARF,UVRR,UVRF,           &
                     FSWU,FSCU,                               &
                     FSWBAND,                                 &
                     SOLAR_TO_OBIO .and. .not. NO_AERO,       &
                     DRBAND, DFBAND,                          &
                     RC=STATUS                                )
      VERIFY_(STATUS)

   else if (USE_RRTMG) then

      allocate(TLEV  (size(Q,1),size(Q,2)+1 ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(TLEV_R(size(Q,1),size(Q,2)+1 ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(PLE_R (size(Q,1),size(Q,2)+1 ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(FCLD_R(size(Q,1),size(Q,2)   ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(CLIQWP(size(Q,1),size(Q,2)   ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(CICEWP(size(Q,1),size(Q,2)   ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(RELIQ (size(Q,1),size(Q,2)   ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(REICE (size(Q,1),size(Q,2)   ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(TAUCLD(size(Q,1),size(Q,2),NB_RRTMG),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(SSACLD(size(Q,1),size(Q,2),NB_RRTMG),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(ASMCLD(size(Q,1),size(Q,2),NB_RRTMG),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(FSFCLD(size(Q,1),size(Q,2),NB_RRTMG),STAT=STATUS)
      VERIFY_(STATUS)

      allocate(TAUAER(size(Q,1),size(Q,2),NB_RRTMG),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(SSAAER(size(Q,1),size(Q,2),NB_RRTMG),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(ASMAER(size(Q,1),size(Q,2),NB_RRTMG),STAT=STATUS)
      VERIFY_(STATUS)
      allocate( ECAER(size(Q,1),size(Q,2),6),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(DPR   (size(Q,1),size(Q,2)   ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(PL_R  (size(Q,1),size(Q,2)   ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(ZL_R  (size(Q,1),size(Q,2)   ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(T_R   (size(Q,1),size(Q,2)   ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(Q_R   (size(Q,1),size(Q,2)   ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(O2_R  (size(Q,1),size(Q,2)   ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(O3_R  (size(Q,1),size(Q,2)   ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(CO2_R (size(Q,1),size(Q,2)   ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(CH4_R (size(Q,1),size(Q,2)   ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(N2O_R (size(Q,1),size(Q,2)   ),STAT=STATUS)
      VERIFY_(STATUS)

      allocate(SWUFLX (size(Q,1),size(Q,2)+1 ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(SWDFLX (size(Q,1),size(Q,2)+1 ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(SWHR   (size(Q,1),size(Q,2)   ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(SWUFLXC(size(Q,1),size(Q,2)+1 ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(SWDFLXC(size(Q,1),size(Q,2)+1 ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(SWHRC  (size(Q,1),size(Q,2)   ),STAT=STATUS)
      VERIFY_(STATUS)

      allocate(SWUFLXR (size(Q,1),size(Q,2)+1 ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(SWDFLXR (size(Q,1),size(Q,2)+1 ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(SWUFLXCR(size(Q,1),size(Q,2)+1 ),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(SWDFLXCR(size(Q,1),size(Q,2)+1 ),STAT=STATUS)
      VERIFY_(STATUS)

      allocate(NIRR_R  (size(Q,1)),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(NIRF_R  (size(Q,1)),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(PARR_R  (size(Q,1)),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(PARF_R  (size(Q,1)),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(UVRR_R  (size(Q,1)),STAT=STATUS)
      VERIFY_(STATUS)
      allocate(UVRF_R  (size(Q,1)),STAT=STATUS)
      VERIFY_(STATUS)

! Code to convert units, flip, and interpolate T, etc.
! Prepare Input for RRTMG

! Set flags related to cloud properties
      ICLD = 4
      INFLGSW = 2
      ICEFLGSW = 3
      LIQFLGSW = 1

      NCOL = size(Q,1)

      ! Get number of cores per node for RRTMG GPU
      ! ------------------------------------------

      ! Reuse the COMM from above

      CoresPerNode = MAPL_CoresPerNodeGet(COMM, RC=STATUS)

      call MAPL_TimerOn(MAPL,"-RRTMG")

      if (NA > 0) then

         where (TAUA > 0.0 .and. SSAA > 0.0 )
            ASYA = ASYA/SSAA
            SSAA = SSAA/TAUA
         elsewhere
            TAUA = 0.0
            SSAA = 0.0
            ASYA = 0.0
         end where

      end if

      call MAPL_TimerOn (MAPL,"--RRTMG_FLIP")

         DPR(:,1:LM) = (PLE(:,2:LM+1)-PLE(:,1:LM))

      CICEWP(:,1:LM) = (1.02*100*DPR(:,LM:1:-1))*QQ3(:,LM:1:-1,1)                  ! g/g -> g/m^2
      CLIQWP(:,1:LM) = (1.02*100*DPR(:,LM:1:-1))*QQ3(:,LM:1:-1,2)

      REICE (:,1:LM) = RR3(:,LM:1:-1,1)
      RELIQ (:,1:LM) = RR3(:,LM:1:-1,2)

      IF      (ICEFLGSW == 0) THEN
         WHERE (REICE < 10.)  REICE = 10.
         WHERE (REICE > 30.)  REICE = 30.
      ELSE IF (ICEFLGSW == 1) THEN
         WHERE (REICE < 13.)  REICE = 13.
         WHERE (REICE > 130.) REICE = 130.
      ELSE IF (ICEFLGSW == 2) THEN
         WHERE (REICE < 5.)   REICE = 5.
         WHERE (REICE > 131.) REICE = 131.
      ELSE IF (ICEFLGSW == 3) THEN
         WHERE (REICE < 5.)   REICE = 5.
         WHERE (REICE > 140.) REICE = 140.
      ELSE IF (ICEFLGSW == 4) THEN
         REICE(:,:) = REICE(:,:)*2.
         WHERE (REICE < 1.)   REICE = 1.
         WHERE (REICE > 200.) REICE = 200.
      END IF

      IF      (LIQFLGSW == 0) THEN
         WHERE (RELIQ < 10.)  RELIQ = 10.
         WHERE (RELIQ > 30.)  RELIQ = 30.
      ELSE IF (LIQFLGSW == 1) THEN
         WHERE (RELIQ < 2.5)  RELIQ = 2.5
         WHERE (RELIQ > 60.)  RELIQ = 60.
      END IF

      ECAER = 0.0
      TAUCLD = 0.0
      SSACLD = 0.0
      ASMCLD = 0.0
      FSFCLD = 0.0

      TLEV(:,2:LM)=(T(:,1:LM-1)* DPR(:,2:LM) + T(:,2:LM) * DPR(:,1:LM-1)) &
            /(DPR(:,1:LM-1) + DPR(:,2:LM))

      TLEV(:,LM+1) = TS(:)
      TLEV(:,   1) = TLEV(:,2)

!  RRTMG index convention is that indices increase from bottom to top
!  Reverse input. Cloud WP and effective particle size have already been reversed.

      PLE_R (:,1:LM+1) = PLE(:,LM+1:1:-1)/100.
      TLEV_R(:,1:LM+1) = TLEV(:,LM+1:1:-1)

      PL_R  (:,1:LM  ) = PL (:,LM:1:-1) / 100.
      T_R   (:,1:LM  ) = T  (:,LM:1:-1)
      Q_R   (:,1:LM  ) = Q  (:,LM:1:-1) / (1.-Q(:,LM:1:-1)) * (MAPL_AIRMW/MAPL_H2OMW)     ! Convert Specific humidity to Volume Mixing Ratio
      O3_R  (:,1:LM  ) = O3 (:,LM:1:-1) * (MAPL_AIRMW/MAPL_O3MW)     ! Convert Mass Mixing Ratio to Volume Mixing Ratio
      CH4_R (:,1:LM  ) = CH4(:,LM:1:-1)
      N2O_R (:,1:LM  ) = N2O(:,LM:1:-1)
      CO2_R (:,1:LM  ) = CO2
      O2_R  (:,1:LM  ) = O2
      FCLD_R(:,1:LM  ) = CL (:,LM:1:-1)

! Adjustment for Earth/Sun distance, from MAPL_SunGetInsolation
      ADJES  = DIST

      ZL_R(:,1) = 0. ! Assume surface ZL_R = 0.
      do k=2,LM
         ! dz = RT/g x dp/p
         ZL_R(:,k) = ZL_R(:,k-1) + MAPL_RGAS*TLEV_R(:,k)/MAPL_GRAV*(PL_R(:,k-1)-PL_R(:,k))/PLE_R(:,k)
      enddo

! Flip the aerosols from above
      TAUAER(:,1:LM,:) = TAUA(:,LM:1:-1,:)
      SSAAER(:,1:LM,:) = SSAA(:,LM:1:-1,:)
      ASMAER(:,1:LM,:) = ASYA(:,LM:1:-1,:)

      call MAPL_TimerOff(MAPL,"--RRTMG_FLIP")
      call MAPL_TimerOn (MAPL,"--RRTMG_INIT")

      call RRTMG_SW_INI(MAPL_CP)

      call MAPL_TimerOff(MAPL,"--RRTMG_INIT")
      call MAPL_TimerOn (MAPL,"--RRTMG_RUN")

      RPART = 0    ! Per recommendation of AER

      IAER = 10    ! Per AER: 
                   !  0: Turns off aerosols
                   ! 10: Enables aerosols

      NORMFLX = 1  ! 0: Do not normalize fluxes
                   ! 1: Normalize fluxes

      DYOFYR = DOY ! Above this is set to 0, but that might only
                   ! be valid if you can use ICLD = 4. That scheme
                   ! is not implemented at present, so we must pass
                   ! in the current day of year.

      call MAPL_GetResource( MAPL, ISOLVAR ,'ISOLVAR:', DEFAULT=0, RC=STATUS)
      VERIFY_(STATUS)

                   ! ISOLVAR:
                   ! Flag for solar variability method
                   !   -1 = (when scon .eq. 0.0): No solar variability
                   !        and no solar cycle (Kurucz solar irradiance
                   !        of 1368.22 Wm-2 only);
                   !        (when scon .ne. 0.0): Kurucz solar irradiance
                   !        scaled to scon and solar variability defined
                   !        (optional) by setting non-zero scale factors
                   !        for each band in bndsolvar
                   !    0 = (when SCON .eq. 0.0): No solar variability 
                   !        and no solar cycle (NRLSSI2 solar constant of 
                   !        1360.85 Wm-2 for the 100-50000 cm-1 spectral 
                   !        range only), with facular and sunspot effects 
                   !        fixed to the mean of Solar Cycles 13-24;
                   !        (when SCON .ne. 0.0): No solar variability 
                   !        and no solar cycle (NRLSSI2 solar constant of 
                   !        1360.85 Wm-2 for the 100-50000 cm-1 spectral 
                   !        range only), is scaled to SCON
                   !    1 = Solar variability (using NRLSSI2  solar
                   !        model) with solar cycle contribution
                   !        determined by fraction of solar cycle
                   !        with facular and sunspot variations
                   !        fixed to their mean variations over the
                   !        average of Solar Cycles 13-24;
                   !        two amplitude scale factors allow
                   !        facular and sunspot adjustments from
                   !        mean solar cycle as defined by indsolvar 
                   !    2 = Solar variability (using NRLSSI2 solar
                   !        model) over solar cycle determined by 
                   !        direct specification of Mg (facular)
                   !        and SB (sunspot) indices provided
                   !        in indsolvar (scon = 0.0 only)
                   !    3 = (when scon .eq. 0.0): No solar variability
                   !        and no solar cycle (NRLSSI2 solar irradiance
                   !        of 1360.85 Wm-2 only);
                   !        (when scon .ne. 0.0): NRLSSI2 solar irradiance
                   !        scaled to scon and solar variability defined
                   !        (optional) by setting non-zero scale factors
                   !        for each band in bndsolvar

      if (ISOLVAR == 1) then
         if (MAPL_AM_I_ROOT()) then
            write (*,*) "ERROR in RRTMG_SW"
            write (*,*) "ISOLVAR==1 is currently unsupported as we have no"
            write (*,*) "way of correctly setting solcycfrac."
         end if
         ASSERT_(.FALSE.)
      end if

      !INDSOLVAR =  Facular and sunspot amplitude 
      !             scale factors (isolvar=1), or
      !             Mg and SB indices (isolvar=2)
      !                Dimensions: (2)

      if (ISOLVAR == 2) then
         ! Here we have the indices from our file

         INDSOLVAR(1) = MG
         INDSOLVAR(2) = SB

      else

         call MAPL_GetResource( MAPL, INDSOLVAR(1) ,'INDSOLVAR_1:', DEFAULT=1.0, RC=STATUS)
         VERIFY_(STATUS)
         call MAPL_GetResource( MAPL, INDSOLVAR(2) ,'INDSOLVAR_2:', DEFAULT=1.0, RC=STATUS)
         VERIFY_(STATUS)

      end if


      BNDSOLVAR = 1.0 ! Solar variability scale factors 
                      ! for each shortwave band
                      !    Dimensions: (nbndsw=14)

      !SOLCYCFRAC: Fraction of averaged 11-year solar cycle (0-1)
      !               at current time (isolvar=1)
      !               0.0 represents the first day of year 1
      !               1.0 represents the last day of year 11

      ! MAT: Note while we don't currently use SOLCYCFRAC, we set it to something
      !      to avoid an optional variable on GPUs

      call MAPL_GetResource( MAPL, SOLCYCFRAC ,'SOLCYCFRAC:', DEFAULT=1.0, RC=STATUS)
      VERIFY_(STATUS)

      call RRTMG_SW (                                           &
            RPART   ,NCOL    ,LM      ,ICLD    ,IAER    ,       &
            PL_R    ,PLE_R   ,T_R     ,TLEV_R  ,TS      ,       &
            Q_R     ,O3_R    ,CO2_R   ,CH4_R   ,N2O_R   ,O2_R , &
            ALBVR   ,ALBVF   ,ALBNR   ,ALBNF   ,                &
            ZT      ,ADJES   ,DYOFYR  ,SC      ,ISOLVAR ,       &
            INFLGSW ,ICEFLGSW,LIQFLGSW,FCLD_R  ,                &
            TAUCLD  ,SSACLD  ,ASMCLD  ,FSFCLD  ,                &
            CICEWP  ,CLIQWP  ,REICE   ,RELIQ   ,                &
            TAUAER  ,SSAAER  ,ASMAER  ,ECAER   ,                &
            SWUFLX  ,SWDFLX  ,SWHR    ,SWUFLXC ,SWDFLXC ,SWHRC, &
            NIRR_R  ,NIRF_R  ,PARR_R  ,PARF_R  ,UVRR_R  ,UVRF_R,&
            NORMFLX ,ZL_R    ,ALAT    ,CoresPerNode,            &
            BNDSOLVAR,       INDSOLVAR,        SOLCYCFRAC       )

      call MAPL_TimerOff(MAPL,"--RRTMG_RUN")
      call MAPL_TimerOn (MAPL,"--RRTMG_FLIP")

      ! MAT The fluxes coming from RRTMG are, like the inputs,
      !     flipped. So, we must flip the outputs to match our
      !     level layout.

      SWUFLXR (:,1:LM+1) = SWUFLX (:,LM+1:1:-1)
      SWDFLXR (:,1:LM+1) = SWDFLX (:,LM+1:1:-1)
      SWUFLXCR(:,1:LM+1) = SWUFLXC(:,LM+1:1:-1)
      SWDFLXCR(:,1:LM+1) = SWDFLXC(:,LM+1:1:-1)

      call MAPL_TimerOff(MAPL,"--RRTMG_FLIP")
      call MAPL_TimerOff(MAPL,"-RRTMG")

      FSW  = SWDFLXR  - SWUFLXR 
      FSC  = SWDFLXCR - SWUFLXCR
      FSWU = SWUFLXR
      FSCU = SWUFLXCR

      ! MAT Just set the FSWBAND to MAPL_UNDEF. More work will
      !     be needed if this is to be extracted from RRTMG as
      !     it does not seem to be an output

      FSWBAND = MAPL_UNDEF

      NIRR(:) = NIRR_R(:)
      NIRF(:) = NIRF_R(:)
      PARR(:) = PARR_R(:)
      PARF(:) = PARF_R(:)
      UVRR(:) = UVRR_R(:)
      UVRF(:) = UVRF_R(:)

! Deallocate the working inputs
!------------------------------
      deallocate(TLEV  ,__STAT__)
      deallocate(TLEV_R,__STAT__)
      deallocate(PLE_R ,__STAT__)
      deallocate(FCLD_R,__STAT__)
      deallocate(CLIQWP,__STAT__)
      deallocate(CICEWP,__STAT__)
      deallocate(RELIQ ,__STAT__)
      deallocate(REICE ,__STAT__)
      deallocate(TAUCLD,__STAT__)
      deallocate(SSACLD,__STAT__)
      deallocate(ASMCLD,__STAT__)
      deallocate(FSFCLD,__STAT__)

      deallocate(TAUAER,__STAT__)
      deallocate(SSAAER,__STAT__)
      deallocate(ASMAER,__STAT__)
      deallocate( ECAER,__STAT__)
      deallocate(DPR   ,__STAT__)
      deallocate(PL_R  ,__STAT__)
      deallocate(ZL_R  ,__STAT__)
      deallocate(T_R   ,__STAT__)
      deallocate(Q_R   ,__STAT__)
      deallocate(O2_R  ,__STAT__)
      deallocate(O3_R  ,__STAT__)
      deallocate(CO2_R ,__STAT__)
      deallocate(CH4_R ,__STAT__)
      deallocate(N2O_R ,__STAT__)

      deallocate(SWUFLX ,__STAT__)
      deallocate(SWDFLX ,__STAT__)
      deallocate(SWHR   ,__STAT__)
      deallocate(SWUFLXC,__STAT__)
      deallocate(SWDFLXC,__STAT__)
      deallocate(SWHRC  ,__STAT__)

      deallocate(SWUFLXR ,__STAT__)
      deallocate(SWDFLXR ,__STAT__)
      deallocate(SWUFLXCR,__STAT__)
      deallocate(SWDFLXCR,__STAT__)

      deallocate(NIRR_R,__STAT__)
      deallocate(NIRF_R,__STAT__)
      deallocate(PARR_R,__STAT__)
      deallocate(PARF_R,__STAT__)
      deallocate(UVRR_R,__STAT__)
      deallocate(UVRF_R,__STAT__)

   else

      ! Something is wrong. We've selected neither Chou nor RRTMG
      ASSERT_(.FALSE.)

   end if SCHEME

! Deallocate the working inputs
!------------------------------

      deallocate (QQ3, RR3)
      deallocate ( RH,  PL)
      deallocate (O3)
      deallocate (PLTMP)

      deallocate (TAUA)
      deallocate (SSAA)
      deallocate (ASYA)

      call MAPL_TimerOn(MAPL,"-BALANCE")

! Complete load balancing by retrieving work done remotely
!---------------------------------------------------------

      call MAPL_TimerOn(MAPL,"--RETRIEVE")

      call MAPL_BalanceWork(BUFINT, NUMMAX, Direction=MAPL_Retrieve, Handle=SolarBalanceHandle, RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOff(MAPL,"--RETRIEVE")

! Unpack the results. ReOrder fills masked (night) locations with zero.
!----------------------------------------------------------------------

      NumInt = size(INTERNALspec)

      L1 = 1
      OUTPUT_VARS_3: do K=1,NumInt
         if(SLICESint(K)>0) then

            if(SLICESint(K)>1) then
               call ESMFL_StateGetPointerToData(INTERNAL, PTR3, NAMESint(K), RC=STATUS)
               VERIFY_(STATUS)
               call ReOrder(BUFINT(L1),PTR3,DAYTIME,NUMMAX,HorzDims,size(Ptr3,3),UNPACKIT)
            else
               call ESMFL_StateGetPointerToData(INTERNAL, PTR2, NAMESint(K), RC=STATUS)
               VERIFY_(STATUS)
               call ReOrder(BUFINT(L1),PTR2,DAYTIME,NUMMAX,HorzDims,1           ,UNPACKIT)
            end if

            L1 = L1 + NUMMAX*SLICESint(K)

         endif
      enddo OUTPUT_VARS_3  ! Over all internal variables

! These are the contiguous versions of the working imports and internals
!-----------------------------------------------------------------------

      deallocate(BUFIMP,BUFINT,SLICESimp,SLICESint,NAMESimp,NAMESint,STAT=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOn(MAPL,"--DESTROY")

      call MAPL_BalanceDestroy(Handle=SolarBalanceHandle, RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOff(MAPL,"--DESTROY")

      call MAPL_TimerOff(MAPL,"-BALANCE")

!  All done
!-----------

      RETURN_(ESMF_SUCCESS)
    end subroutine SORADCORE



    subroutine SHRTWAVE(PLTMP,TA,WA,OA,CO2,COSZ   , &
                          CWC,REFF,FCLD,ICT,ICB  , &
                          RGBUV,RGFUV,RGBIR,RGFIR, &
                          TAUA,SSAA,ASYA, &

                          FLX,    FLC     , &
                          FDIRIR, FDIFIR  , &
                          FDIRPAR,FDIFPAR , &
                          FDIRUV, FDIFUV  , &
                          FLXU,   FLCU    , &
                          FLXBAND,          &
                          do_drfband,       &
                          drband, dfband,   &

                          RC                )


!   Inlineable cover for the f77 version of SORAD.
!   This cover works on a 1D run of soundings.

      real, dimension(:,:    ), intent(IN ) :: PLTMP,TA, WA, OA, FCLD
      real, dimension(:,:,:  ), intent(IN ) :: CWC, REFF
      real,                     intent(IN ) :: CO2
      integer,                  intent(IN ) :: ICT, ICB
      real, dimension(:      ), intent(IN ) :: COSZ, RGBUV, RGFUV, RGBIR, RGFIR
      real, dimension(:,:,:  ), intent(IN ) :: TAUA, SSAA, ASYA
      logical,                  intent(in ) :: do_drfband  ! Compute drband, dfband?

      real, dimension(:,:    ), intent(OUT) :: FLX,    FLC
      real, dimension(:,:    ), intent(OUT) :: FLXU,   FLCU
      real, dimension(:      ), intent(OUT) :: FDIRIR, FDIFIR
      real, dimension(:      ), intent(OUT) :: FDIRPAR,FDIFPAR
      real, dimension(:      ), intent(OUT) :: FDIRUV, FDIFUV
      real, dimension(:,:    ), intent(OUT) :: FLXBAND

      ! Surface downwelling direct and diffuse (W/m2) in each solar band:
      ! Only filled if (do_drfband), otherwise not touched and can be null pointers;
      ! if (do_drfband), must point to an (#cols,#bands) space.
      real, intent(inout), dimension (:,:), pointer :: drband, dfband

      integer, optional,        intent(OUT) :: RC

! Locals
!-------
      
      integer :: IRUN, LN

      INTEGER :: IDX,I,J,K,IN,IB,L
      INTEGER :: STATUS

#ifdef _CUDA
      ! CUDA Fortran gridding
      type(dim3) :: Grid, Block
      integer :: blocksize
#endif

! Begin
!------
      
      call MAPL_TimerOn(MAPL,"-MISC")

      IRUN     = SIZE(TA,1) 
      LN       = SIZE(TA,2)

      call MAPL_TimerOff(MAPL,"-MISC")

      call MAPL_TimerOn(MAPL,"-SORAD")

#ifdef _CUDA
      call MAPL_GetResource(MAPL,BLOCKSIZE,'BLOCKSIZE:',DEFAULT=128,RC=STATUS)
      VERIFY_(STATUS)

      ! Set up the CUDA Model variables
      Block = dim3(blocksize,1,1)
      Grid = dim3(ceiling(real(IRUN)/real(blocksize)),1,1)

      ASSERT_(LN <= GPU_MAXLEVS) ! If this is tripped, ESMA_arch.mk must
                                 ! be edited.

      call MAPL_TimerOn(MAPL,"--SORAD_ALLOC",RC=STATUS)
      VERIFY_(STATUS)

      ! ----------------------
      ! Allocate device arrays
      ! ----------------------

      ! Inputs
      ! ------

      ALLOCATE(COSZ_DEV(IRUN), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(PL_DEV(IRUN,LN+1), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(TA_DEV(IRUN,LN), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(WA_DEV(IRUN,LN), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(OA_DEV(IRUN,LN), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(CWC_DEV(IRUN,LN,4), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(FCLD_DEV(IRUN,LN), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(REFF_DEV(IRUN,LN,4), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(RSUVBM_DEV(IRUN), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(RSUVDF_DEV(IRUN), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(RSIRBM_DEV(IRUN), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(RSIRDF_DEV(IRUN), STAT = STATUS)
      VERIFY_(STATUS)

      ! Constant Tables in Device Memory
      ! --------------------------------

      ! GPU These are in device memory because they are accessed randomly
      !     Constant memory is best for values that are broadcast to all
      !     threads. Plus, being in global means caching could occur.

      ALLOCATE(COA(62,101), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(CAH(43,37), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(CAIB(11,9,11), STAT=STATUS)
      VERIFY_(STATUS)
      ALLOCATE(CAIF(9,11), STAT=STATUS)
      VERIFY_(STATUS)

      ! Aerosol Inputs
      ! --------------

      ALLOCATE(TAUA_DEV(IRUN,LN,NB_CHOU), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(SSAA_DEV(IRUN,LN,NB_CHOU), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(ASYA_DEV(IRUN,LN,NB_CHOU), STAT = STATUS)
      VERIFY_(STATUS)

      ! Outputs
      ! -------

      ALLOCATE(FLX_DEV(IRUN,LN+1), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(FLC_DEV(IRUN,LN+1), STAT = STATUS)
      VERIFY_(STATUS)
      allocate(FLXU_DEV(IRUN,LN+1), stat = STATUS)
      VERIFY_(STATUS)
      allocate(FLCU_DEV(IRUN,LN+1), stat = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(FDIRUV_DEV(IRUN), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(FDIFUV_DEV(IRUN), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(FDIRPAR_DEV(IRUN), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(FDIFPAR_DEV(IRUN), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(FDIRIR_DEV(IRUN), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(FDIFIR_DEV(IRUN), STAT = STATUS)
      VERIFY_(STATUS)
      ALLOCATE(FLX_SFC_BAND_DEV(IRUN,NB_CHOU), STAT = STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOff(MAPL,"--SORAD_ALLOC",RC=STATUS)
      VERIFY_(STATUS)

      ! Copy host inputs to device
      ! --------------------------

      call MAPL_TimerOn(MAPL,"--SORAD_DATA",RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOn(MAPL,"---SORAD_DATA_DEVICE",RC=STATUS)
      VERIFY_(STATUS)

      ! Inputs
      ! ------

      STATUS = cudaMemcpy(COSZ_DEV,COSZ,IRUN)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(PL_DEV,PLTMP,IRUN*(LN+1))
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(TA_DEV,TA,IRUN*LN)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(WA_DEV,WA,IRUN*LN)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(OA_DEV,OA,IRUN*LN)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(CWC_DEV,CWC,IRUN*LN*4)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(FCLD_DEV,FCLD,IRUN*LN)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(REFF_DEV,REFF,IRUN*LN*4)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(RSUVBM_DEV,RGBUV,IRUN)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(RSUVDF_DEV,RGFUV,IRUN)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(RSIRBM_DEV,RGBIR,IRUN)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(RSIRDF_DEV,RGFIR,IRUN)
      VERIFY_(STATUS)

      ! Aerosol Inputs
      ! --------------

      STATUS = cudaMemcpy(TAUA_DEV,TAUA,IRUN*LN*NB_CHOU)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(SSAA_DEV,SSAA,IRUN*LN*NB_CHOU)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(ASYA_DEV,ASYA,IRUN*LN*NB_CHOU)
      VERIFY_(STATUS)

      ! Constant Tables in Device Memory
      ! --------------------------------

      COA = COA_CONST
      CAH = CAH_CONST
      CAIB = CAIB_CONST
      CAIF = CAIF_CONST

      call MAPL_TimerOff(MAPL,"---SORAD_DATA_DEVICE",RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOn(MAPL,"---SORAD_DATA_CONST",RC=STATUS)
      VERIFY_(STATUS)

      ! Load constants in constant memory
      ! ---------------------------------

      HK_UV=HK_UV_TEMP
      HK_IR=HK_IR_TEMP

      ZK_UV=ZK_UV_CONST
      WK_UV=WK_UV_CONST
      RY_UV=RY_UV_CONST
      AIB_UV=AIB_UV_CONST
      AWB_UV=AWB_UV_CONST
      ARB_UV=ARB_UV_CONST
      AIG_UV=AIG_UV_CONST
      AWG_UV=AWG_UV_CONST
      ARG_UV=ARG_UV_CONST

      XK_IR=XK_IR_CONST
      RY_IR=RY_IR_CONST
      AIB_NIR=AIB_NIR_CONST
      AWB_NIR=AWB_NIR_CONST
      ARB_NIR=ARB_NIR_CONST
      AIA_NIR=AIA_NIR_CONST
      AWA_NIR=AWA_NIR_CONST
      ARA_NIR=ARA_NIR_CONST
      AIG_NIR=AIG_NIR_CONST
      AWG_NIR=AWG_NIR_CONST
      ARG_NIR=ARG_NIR_CONST

      call MAPL_TimerOff(MAPL,"---SORAD_DATA_CONST",RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOff(MAPL,"--SORAD_DATA",RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOn(MAPL,"--SORAD_RUN",RC=STATUS)
      VERIFY_(STATUS)

      call sorad<<<Grid, Block>>>(IRUN,LN,NB_CHOU,CO2,ICT,ICB)
      STATUS = cudaGetLastError()
      if (STATUS /= 0) then
         write (*,*) "Error code from SORAD kernel call: ", STATUS
         write (*,*) "Kernel Call failed: ", cudaGetErrorString(STATUS)
         ASSERT_(.FALSE.)
      end if

      call MAPL_TimerOff(MAPL,"--SORAD_RUN",RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOn(MAPL,"--SORAD_DATA",RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOn(MAPL,"---SORAD_DATA_DEVICE",RC=STATUS)
      VERIFY_(STATUS)

      STATUS = cudaMemcpy(FLX,FLX_DEV,IRUN*(LN+1))
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(FLC,FLC_DEV,IRUN*(LN+1))
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(FLXU,FLXU_DEV,IRUN*(LN+1))
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(FLCU,FLCU_DEV,IRUN*(LN+1))
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(FDIRUV,FDIRUV_DEV,IRUN)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(FDIFUV,FDIFUV_DEV,IRUN)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(FDIRPAR,FDIRPAR_DEV,IRUN)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(FDIFPAR,FDIFPAR_DEV,IRUN)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(FDIRIR,FDIRIR_DEV,IRUN)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(FDIFIR,FDIFIR_DEV,IRUN)
      VERIFY_(STATUS)
      STATUS = cudaMemcpy(FLXBAND,FLX_SFC_BAND_DEV,IRUN*NB_CHOU)
      VERIFY_(STATUS)

      call MAPL_TimerOff(MAPL,"---SORAD_DATA_DEVICE",RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOff(MAPL,"--SORAD_DATA",RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_TimerOn(MAPL,"--SORAD_DEALLOC",RC=STATUS)
      VERIFY_(STATUS)

      ! ------------------------
      ! Deallocate device arrays
      ! ------------------------
      
      ! Inputs
      ! ------

      DEALLOCATE(COSZ_DEV)
      DEALLOCATE(PL_DEV)
      DEALLOCATE(TA_DEV)
      DEALLOCATE(WA_DEV)
      DEALLOCATE(OA_DEV)
      DEALLOCATE(CWC_DEV)
      DEALLOCATE(FCLD_DEV)
      DEALLOCATE(REFF_DEV)
      DEALLOCATE(RSUVBM_DEV)
      DEALLOCATE(RSUVDF_DEV)
      DEALLOCATE(RSIRBM_DEV)
      DEALLOCATE(RSIRDF_DEV)

      ! Aerosol inputs
      ! --------------

      DEALLOCATE(TAUA_DEV)
      DEALLOCATE(SSAA_DEV)
      DEALLOCATE(ASYA_DEV)
   
      ! Constant tables in device memory
      ! --------------------------------

      DEALLOCATE(COA)
      DEALLOCATE(CAH)
      DEALLOCATE(CAIB)
      DEALLOCATE(CAIF)

      ! Outputs
      ! -------

      DEALLOCATE(FLX_DEV)
      DEALLOCATE(FLC_DEV)
      DEALLOCATE(FLXU_DEV)
      DEALLOCATE(FLCU_DEV)
      DEALLOCATE(FDIRUV_DEV)
      DEALLOCATE(FDIFUV_DEV)
      DEALLOCATE(FDIRPAR_DEV)
      DEALLOCATE(FDIFPAR_DEV)
      DEALLOCATE(FDIRIR_DEV)
      DEALLOCATE(FDIFIR_DEV)
      DEALLOCATE(FLX_SFC_BAND_DEV)

      call MAPL_TimerOff(MAPL,"--SORAD_DEALLOC",RC=STATUS)
      VERIFY_(STATUS)

#else
      call MAPL_TimerOn(MAPL,"--SORAD_RUN",RC=STATUS)
      VERIFY_(STATUS)
      call SORAD (IRUN,LN,NB_CHOU,COSZ,PLTMP,TA,WA,OA,CO2,      &
           CWC,FCLD,ICT,ICB,REFF,HK_UV_TEMP,  HK_IR_TEMP,       &
           TAUA,SSAA,ASYA,                                      &
           RGBUV,RGFUV, RGBIR, RGFIR,                           &
           FLX,FLC,FDIRUV,FDIFUV,FDIRPAR,FDIFPAR,FDIRIR,FDIFIR, &
           FLXU,FLCU,                                           &
           FLXBAND,                                             &
           do_drfband, drband, dfband)
      call MAPL_TimerOff(MAPL,"--SORAD_RUN",RC=STATUS)
      VERIFY_(STATUS)

#endif

      call MAPL_TimerOff(MAPL,"-SORAD")

      RETURN_(ESMF_SUCCESS)

    end subroutine SHRTWAVE

!==========================================================

    subroutine UPDATE_EXPORT(IM,JM,LM, RC)
      use parrrsw, only: nbndsw, jpb1, jpb2
      use rrsw_wvn, only: wavenum1, wavenum2

      integer,           intent(IN ) :: IM, JM, LM
      integer, optional, intent(OUT) :: RC

!  Locals

      character(len=ESMF_MAXSTR)      :: IAm
      integer                         :: STATUS

      real,    dimension(IM,JM)       :: ZTH, SLR, ALB, CLD, SLN, ZTHN

      real, pointer, dimension(:,:  ) :: ALBEXP, ALBIMP

      real, pointer, dimension(:,:,:) ::     FSW,     FSC
      real, pointer, dimension(:,:,:) ::    FSWN,    FSCN
      real, pointer, dimension(:,:,:) ::   FSWNA,   FSCNA
      real, pointer, dimension(:,:,:) ::  FSWNAN,  FSCNAN

      real, pointer, dimension(:,:,:) ::    FSWU,    FSCU
      real, pointer, dimension(:,:,:) ::   FSWUN,   FSCUN
      real, pointer, dimension(:,:,:) ::  FSWUNA,  FSCUNA
      real, pointer, dimension(:,:,:) :: FSWUNAN, FSCUNAN

      real, pointer, dimension(:,:,:) ::    FSWD,    FSCD
      real, pointer, dimension(:,:,:) ::  FSWDNA,  FSCDNA

      real, pointer, dimension(:,:,:) ::     FSWBAND
      real, pointer, dimension(:,:,:) ::    FSWBANDN
      real, pointer, dimension(:,:,:) ::   FSWBANDNA
      real, pointer, dimension(:,:,:) ::  FSWBANDNAN

      real, pointer, dimension(:,:,:) ::  DRBANDN, DFBANDN
      real, pointer, dimension(:,:,:) ::  DROBIO , DFOBIO

      real, pointer, dimension(:,:  ) ::  DFUVR,  DRUVR
      real, pointer, dimension(:,:  ) ::  DFPAR,  DRPAR
      real, pointer, dimension(:,:  ) ::  DFNIR,  DRNIR
      real, pointer, dimension(:,:  ) :: DRNUVR, DRNPAR, DRNNIR 
      real, pointer, dimension(:,:  ) ::  SLRTP,    RSR,  RSC
      real, pointer, dimension(:,:  ) ::  SLRSF,   RSCS,  RSRS, SLRSFC
      real, pointer, dimension(:,:  ) ::  SLRSFNA, SLRSFCNA
      real, pointer, dimension(:,:  ) ::  SLRSUF,  SLRSUFNA
      real, pointer, dimension(:,:  ) ::  SLRSUFC, SLRSUFCNA
      real, pointer, dimension(:,:  ) ::  RSRNA,  RSCNA
      real, pointer, dimension(:,:  ) :: RSCSNA, RSRSNA
      real, pointer, dimension(:,:  ) ::    OSR, OSRCLR
      real, pointer, dimension(:,:  ) ::  OSRNA, OSRCNA
      real, pointer, dimension(:,:  ) :: DFUVRN, DRUVRN
      real, pointer, dimension(:,:  ) :: DFPARN, DRPARN
      real, pointer, dimension(:,:  ) :: DFNIRN, DRNIRN
      real, pointer, dimension(:,:  ) :: ALBEDO
      real, pointer, dimension(:,:  ) :: COSZ, MCOSZ

      real, pointer, dimension(:,:,:)   :: FCLD,CLIN,RH
      real, pointer, dimension(:,:,:)   :: DP, PL, PLL, AERO, T, Q, RAERO
      real, pointer, dimension(:,:,:)   :: RRL,RRI,RRR,RRS
      real, pointer, dimension(:,:,:)   :: RQL,RQI,RQR,RQS
      real, pointer, dimension(:,:,:,:) :: TAUCLD, HYDROMETS, REFF
      real, pointer, dimension(:,:,:)   :: TAUI,TAUW,TAUR,TAUS

      real, dimension(LM  ) :: DUM1D
      real, dimension(LM,4) :: DUM2D

      real, pointer, dimension(:,:)   :: TDUST,TSALT,TSO4,TBC,TOC
      real, pointer, dimension(:,:)   :: CLDH,CLDM,CLDL,CLDT,  &
                                         TAUH,TAUM,TAUL,TAUT,  &
                                         CLDTMP, CLDPRS

      type (ESMF_FieldBundle)         :: BUNDLE
      type (ESMF_Field)               :: FIELD
      type (ESMF_TimeInterval)        :: DELT
      character(len=ESMF_MAXSTR)      :: NAME

      integer :: L, I, J, N, NA, IB
      real    :: TAUCRIT
      real    :: FAC
      integer :: idx
      integer,external:: GetAeroIndex

      logical :: USE_CHOU, USE_RRTMG           ! Logical concerning what radiation to use
      real    :: RRTMG_SORAD                   ! Holds value from AGCM.rc

      integer, parameter :: NB_CHOU  = 8  ! Number of bands in SORAD calcs for Chou
      integer, parameter :: NB_RRTMG = 14 ! Number of bands in SORAD calcs for RRTMG
      integer :: NUM_BANDS_SOLAR

      ! OBIO bands (start,finish) in [nm]
      real, parameter :: OBIO_bands_nm (2,NB_OBIO) = reshape([ &
          200.0,  300.0, &  ! 01
          300.0,  350.0, &  ! 02
          350.0,  362.5, &  ! 03
          362.5,  387.5, &  ! 04
          387.5,  412.5, &  ! 05
          412.5,  437.5, &  ! 06
          437.5,  462.5, &  ! 07
          462.5,  487.5, &  ! 08
          487.5,  512.5, &  ! 09
          512.5,  537.5, &  ! 10
          537.5,  562.5, &  ! 11
          562.5,  587.5, &  ! 12
          587.5,  612.5, &  ! 13
          612.5,  637.5, &  ! 14
          637.5,  662.5, &  ! 15
          662.5,  687.5, &  ! 16
          687.5,  700.0, &  ! 17
          700.0,  750.0, &  ! 18
          750.0,  800.0, &  ! 19
          800.0,  900.0, &  ! 20
          900.0, 1000.0, &  ! 21
         1000.0, 1100.0, &  ! 22
         1100.0, 1200.0, &  ! 23
         1200.0, 1300.0, &  ! 24
         1300.0, 1400.0, &  ! 25
         1400.0, 1500.0, &  ! 26
         1500.0, 1600.0, &  ! 27
         1600.0, 1700.0, &  ! 28
         1700.0, 1800.0, &  ! 29
         1800.0, 2000.0, &  ! 30
         2000.0, 2400.0, &  ! 31
         2400.0, 3400.0, &  ! 32
         3400.0, 4000.0  &  ! 33
      ], shape(OBIO_bands_nm))

      ! We will do the conversion from SOLAR to OBIO bands in wavenumber [cm^-1],
      ! since photon energy \propto wavenumber, where wavenumber \def 1 / wavelength.
      real, parameter :: OBIO_bands_wavenum (2,NB_OBIO) = 1.e7 / OBIO_bands_nm(2:1:-1,:)
      
      ! CHOU bands (start,finish) in [nm]
      real, parameter :: CHOU_bands_nm (2,NB_CHOU) = reshape([ &
          225.0,  285.0, &  ! 01
          285.0,  300.0, &  ! sub-band 2b (see note below)
          300.0,  325.0, &  ! 03
          325.0,  400.0, &  ! 04
          400.0,  690.0, &  ! 05
          690.0, 1220.0, &  ! 06
         1220.0, 2270.0, &  ! 07
         2270.0, 3850.0  &  ! 08
      ], shape(CHOU_bands_nm))

      ! Note: Chou-Suarez "Band 2" also includes sub-band 2a=(175,225) [nm].
      ! But almost no radiation in this extreme UV sub-band reaches the surface,
      ! so we treat all the band "2" as actually all sub-band 2b=(285,300) [nm],
      ! as in the CHOU_bands_nm array above.

      ! loaded later

      real, allocatable, dimension(:,:)  :: SOLAR_bands_wavenum
      integer, allocatable, dimension(:) :: SOLAR_band_number_in_wvn_order

      real :: swvn1, swvn2, owvn1, owvn2, sfrac
      integer :: iseg, ibbeg, ibend, jb, kb, kb_start, kb_used_last
      logical :: sfirst, ofirst

      Iam  = trim(COMP_NAME)//"SolarUpdateExport"

      call ESMF_ClockGet(CLOCK, TIMESTEP=DELT, currTIME=CURRENTTIME, RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_SunGetInsolation(LONS, LATS,      &
              ORBIT, ZTH, SLR, &
              INTV  = DELT,    &
              CLOCK = CLOCK,   &
              TIME = SUNFLAG,  &
              ZTHN = ZTHN,     &
              RC=STATUS )
      VERIFY_(STATUS)

      ZTH = max(ZTH,0.0)
      SLR = SLR * SC

      where(ZTH>0.0) 
         SLN = (SLR/ZTH)
      else where
         SLN = 0.0
      end where

      call MAPL_GetPointer(INTERNAL, FSWN,       'FSWN',       RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(INTERNAL, FSCN,       'FSCN',       RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(INTERNAL, FSWNAN,     'FSWNAN',     RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(INTERNAL, FSCNAN,     'FSCNAN',     RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(INTERNAL, FSWUN,      'FSWUN',      RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(INTERNAL, FSCUN,      'FSCUN',      RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(INTERNAL, FSWUNAN,    'FSWUNAN',    RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(INTERNAL, FSCUNAN,    'FSCUNAN',    RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(INTERNAL, FSWBANDN,   'FSWBANDN',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(INTERNAL, FSWBANDNAN, 'FSWBANDNAN', RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_GetPointer(INTERNAL, DRUVRN,  'DRUVRN', RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(INTERNAL, DFUVRN,  'DFUVRN', RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(INTERNAL, DRPARN,  'DRPARN', RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(INTERNAL, DFPARN,  'DFPARN', RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(INTERNAL, DRNIRN,  'DRNIRN', RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(INTERNAL, DFNIRN,  'DFNIRN', RC=STATUS)
      VERIFY_(STATUS)


      call MAPL_GetPointer(EXPORT  , FSW,       'FSW',       RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , FSC,       'FSC',       RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , FSWNA,     'FSWNA',     RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , FSCNA,     'FSCNA',     RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , FSWD,      'FSWD',      RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , FSCD,      'FSCD',      RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , FSWDNA,    'FSWDNA',    RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , FSCDNA,    'FSCDNA',    RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , FSWU,      'FSWU',      RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , FSCU,      'FSCU',      RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , FSWUNA,    'FSWUNA',    RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , FSCUNA,    'FSCUNA',    RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , FSWBAND,   'FSWBAND',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , FSWBANDNA, 'FSWBANDNA', RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_GetPointer(EXPORT  , DRUVR,   'DRUVR',  RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , DFUVR,   'DFUVR',  RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , DRPAR,   'DRPAR',  RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , DFPAR,   'DFPAR',  RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , DRNIR,   'DRNIR',  RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , DFNIR,   'DFNIR',  RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,   RSR,     'RSR',  RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,   RSC,     'RSC',  RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , RSRNA,   'RSRNA',  RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , RSCNA,   'RSCNA',  RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , SLRTP,   'SLRTP',  RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,  RSCS,    'RSCS',  RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,  RSRS,    'RSRS',  RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,RSCSNA,  'RSCSNA',  RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,RSRSNA,  'RSRSNA',  RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , SLRSF,   'SLRSF',  RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,SLRSFC,  'SLRSFC',  RC=STATUS)
      VERIFY_(STATUS) 
      call MAPL_GetPointer(EXPORT  ,SLRSFNA,'SLRSFNA',  RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,SLRSFCNA,'SLRSFCNA',RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , SLRSUF,  'SLRSUF', RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,SLRSUFC, 'SLRSUFC', RC=STATUS)
      VERIFY_(STATUS) 
      call MAPL_GetPointer(EXPORT  ,SLRSUFNA,'SLRSUFNA',RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,SLRSUFCNA,'SLRSUFCNA',RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,   OSR,    'OSR',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,OSRCLR, 'OSRCLR',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , OSRNA,  'OSRNA',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,OSRCNA, 'OSRCNA',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,ALBEDO, 'ALBEDO',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,  COSZ,   'COSZ',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  , MCOSZ,  'MCOSZ',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,DRNUVR, 'DRNUVR',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,DRNPAR, 'DRNPAR',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,DRNNIR, 'DRNNIR',   RC=STATUS)
      VERIFY_(STATUS)


      call MAPL_GetPointer(IMPORT,CLIN,   'FCLD',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(IMPORT, PLL,    'PLE',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(IMPORT, RRI,     'RI',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(IMPORT, RRL,     'RL',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(IMPORT, RRR,     'RR',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(IMPORT, RRS,     'RS',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(IMPORT, RQI,     'QI',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(IMPORT, RQL,     'QL',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(IMPORT, RQR,     'QR',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(IMPORT, RQS,     'QS',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(IMPORT, T,       'T',    RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(IMPORT, Q,       'QV',   RC=STATUS)
      VERIFY_(STATUS)

      call MAPL_GetPointer(EXPORT  ,FCLD,   'FCLD',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,TAUI, 'TAUCLI',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,TAUW, 'TAUCLW',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,TAUR, 'TAUCLR',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,TAUS, 'TAUCLS',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,CLDL,  'CLDLO',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,CLDM,  'CLDMD',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,CLDH,  'CLDHI',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,CLDT,  'CLDTT',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,TAUL,  'TAULO',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,TAUM,  'TAUMD',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,TAUH,  'TAUHI',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,TAUT,  'TAUTT',   RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,CLDTMP,'CLDTMP',  RC=STATUS)
      VERIFY_(STATUS)
      call MAPL_GetPointer(EXPORT  ,CLDPRS,'CLDPRS',  RC=STATUS)
      VERIFY_(STATUS)

      if (SOLAR_TO_OBIO) then
         call MAPL_GetPointer(INTERNAL, DRBANDN, 'DRBANDN',    __RC__)
         call MAPL_GetPointer(INTERNAL, DFBANDN, 'DFBANDN',    __RC__)
         call MAPL_GetPointer(EXPORT  , DROBIO,  'DROBIO',     __RC__)
         call MAPL_GetPointer(EXPORT  , DFOBIO,  'DFOBIO',     __RC__)
      end if

      if(associated(FCLD)) FCLD = CLIN

      if(associated(CLDH)) then
         CLDH = 0.
         do l=1,LCLDMH-1
            CLDH = max(CLDH,CLIN(:,:,L))
         end do
      end if

      if(associated(CLDM)) then
         CLDM = 0.
         do l=LCLDMH,LCLDLM-1
            CLDM = max(CLDM,CLIN(:,:,L))
         end do
      end if

      if(associated(CLDL)) then
         CLDL = 0.
         do l=LCLDLM,LM
            CLDL = max(CLDL,CLIN(:,:,L))
         end do
      end if

      if(associated(CLDT)) then
         CLD = 0.
         do l=1,LCLDMH-1
            CLD = max(CLD,CLIN(:,:,L))
         end do
         CLDT = (1-CLD)
         CLD = 0.
         do l= LCLDMH,LCLDLM-1
            CLD = max(CLD,CLIN(:,:,L))
         end do
         CLDT = CLDT*(1-CLD)
         CLD = 0.
         do l=LCLDLM,LM
            CLD = max(CLD,CLIN(:,:,L))
         end do
         CLDT = 1.0 - CLDT*(1-CLD)
      end if

      if(associated(TAUI  ).or.associated(TAUW  ).or. &
         associated(TAUR  ).or.associated(TAUS  ).or. &
         associated(TAUL  ).or.associated(TAUM  ).or. &
         associated(TAUH  ).or.associated(TAUT  ).or. &
         associated(CLDTMP).or.associated(CLDPRS)    ) then

         allocate(   TAUCLD(IM,JM,LM,4), stat=STATUS)
         VERIFY_(STATUS)
         allocate(HYDROMETS(IM,JM,LM,4), stat=STATUS)
         VERIFY_(STATUS)
         allocate(     REFF(IM,JM,LM,4), stat=STATUS)
         VERIFY_(STATUS)
         allocate(       DP(IM,JM,LM  ), stat=STATUS)
         VERIFY_(STATUS)

         DP = PLL(:,:,1:LM)-PLL(:,:,0:LM-1)

         ! In REFF, HYDROMETS, and TAUCLD, the final index is as follows:
         !       1  Cloud Ice
         !       2  Cloud Liquid
         !       3  Falling Liquid (Rain)
         !       4  Falling Ice (Rain)

         REFF(:,:,:,1) = RRI * 1.0e6  ! REFF must be in microns
         REFF(:,:,:,2) = RRL * 1.0e6
         REFF(:,:,:,3) = RRR * 1.0e6
         REFF(:,:,:,4) = RRS * 1.0e6

         HYDROMETS(:,:,:,1) = RQI
         HYDROMETS(:,:,:,2) = RQL
         HYDROMETS(:,:,:,3) = RQR
         HYDROMETS(:,:,:,4) = RQS

         TAUCLD = 0.0

         ! Due to the generic use of this routine, it currently works on one column at a time,
         ! thus the need for the array sections below.

         ! NOTE: Dummy arrays are passed into outputs 1 and 3 because these are currently only 
         !       used in sorad.F90.

         DO I = 1, IM
            DO J = 1, JM
               CALL GETVISTAU(LM,ZTH(I,J),DP(I,J,:),CLIN(I,J,:),REFF(I,J,:,:),HYDROMETS(I,J,:,:),LCLDMH,LCLDLM,&
                              DUM2D(:,:),TAUCLD(I,J,:,:),DUM1D(:))
            END DO
         END DO

         if(associated(TAUI)) TAUI = TAUCLD(:,:,:,1)
         if(associated(TAUW)) TAUW = TAUCLD(:,:,:,2)
         if(associated(TAUR)) TAUR = TAUCLD(:,:,:,3)
         if(associated(TAUS)) TAUS = TAUCLD(:,:,:,4)

         TAUCLD(:,:,:,1) = TAUCLD(:,:,:,1) + TAUCLD(:,:,:,2) + TAUCLD(:,:,:,3) + TAUCLD(:,:,:,4)

         if(associated(TAUH)) then
            TAUH = 0.
            do l=1,LCLDMH-1
               TAUH = TAUH + TAUCLD(:,:,L,1)
            end do
         end if

         if(associated(TAUM)) then
            TAUM = 0.
            do l=LCLDMH,LCLDLM-1
               TAUM = TAUM + TAUCLD(:,:,L,1)
            end do
         end if

         if(associated(TAUL)) then
            TAUL = 0.
            do l=LCLDLM,LM
               TAUL = TAUL + TAUCLD(:,:,L,1)
            end do
         end if

         if(associated(TAUT)) then
            TAUT = 0.
            do l=1,LM
               TAUT = TAUT + TAUCLD(:,:,L,1)
            end do
         end if

         if(associated(CLDTMP).or.associated(CLDPRS)) then
            call MAPL_GetResource( MAPL, TAUCRIT , 'TAUCRIT:', DEFAULT=0.10, RC=STATUS)
            VERIFY_(STATUS)

            if(associated(CLDTMP)) CLDTMP = MAPL_UNDEF
            if(associated(CLDPRS)) CLDPRS = MAPL_UNDEF

            do l=LM,1,-1
               if(associated(CLDTMP)) then
                  where(TAUCLD(:,:,L,1)>TAUCRIT) CLDTMP = T(:,:,L)
               end if
               if(associated(CLDPRS)) then
                  where(TAUCLD(:,:,L,1)>TAUCRIT) CLDPRS = PLL(:,:,L-1)
               end if
            end do
         end if

         deallocate(TAUCLD   )
         deallocate(HYDROMETS)
         deallocate(REFF     )
         deallocate(DP       )

      end if

! Fill Albedos
!-------------

      FAC = 1.0

! Visible/UV diffuse

      call MAPL_GetPointer(EXPORT,   ALBEXP, 'ALBVF', RC=STATUS)
      VERIFY_(STATUS)

      if(associated(ALBEXP)) then
         call MAPL_GetPointer(IMPORT,   ALBIMP, 'ALBVF', RC=STATUS)
         VERIFY_(STATUS)
         where(SLR>0)
            ALBEXP = ALBIMP * FAC
         elsewhere
            ALBEXP = MAPL_UNDEF
         end where
      end if

! Visible/UV direct

      call MAPL_GetPointer(EXPORT,   ALBEXP, 'ALBVR', RC=STATUS)
      VERIFY_(STATUS)

      if(associated(ALBEXP)) then
         call MAPL_GetPointer(IMPORT,   ALBIMP, 'ALBVR', RC=STATUS)
         VERIFY_(STATUS)
         where(SLR>0)
            ALBEXP = ALBIMP * FAC
         elsewhere
            ALBEXP = MAPL_UNDEF
         end where
      end if

! NIR diffuse

      call MAPL_GetPointer(EXPORT,   ALBEXP, 'ALBNF', RC=STATUS)
      VERIFY_(STATUS)

      if(associated(ALBEXP)) then
         call MAPL_GetPointer(IMPORT,   ALBIMP, 'ALBNF', RC=STATUS)
         VERIFY_(STATUS)
         where(SLR>0)
            ALBEXP = ALBIMP * FAC
         elsewhere
            ALBEXP = MAPL_UNDEF
         end where
      end if

! NIR direct

      call MAPL_GetPointer(EXPORT,   ALBEXP, 'ALBNR', RC=STATUS)
      VERIFY_(STATUS)

      if(associated(ALBEXP)) then
         call MAPL_GetPointer(IMPORT,   ALBIMP, 'ALBNR', RC=STATUS)
         VERIFY_(STATUS)
         where(SLR>0)
            ALBEXP = ALBIMP * FAC
         elsewhere
            ALBEXP = MAPL_UNDEF
         end where
      end if

! Total surface albedo
!---------------------

      ALB = DRUVRN+DFUVRN+DRPARN+DFPARN+DRNIRN+DFNIRN
      where(SLR>0.0 .and. ALB>0.0)
         ALB = min( max(1.0 - FSWN(:,:,LM)/ALB,.01), 0.9 )
      elsewhere
         ALB = MAPL_UNDEF
      end where

      if(associated(ALBEDO)) ALBEDO = ALB

! Fill incident fluxes
!---------------------

      if(associated( SLRTP))  SLRTP =        SLR
      if(associated( DRUVR))  DRUVR = DRUVRN*SLR
      if(associated( DFUVR))  DFUVR = DFUVRN*SLR
      if(associated( DRPAR))  DRPAR = DRPARN*SLR
      if(associated( DFPAR))  DFPAR = DFPARN*SLR
      if(associated( DRNIR))  DRNIR = DRNIRN*SLR
      if(associated( DFNIR))  DFNIR = DFNIRN*SLR
      if(associated(DRNUVR)) DRNUVR = DRUVRN*SLN
      if(associated(DRNPAR)) DRNPAR = DRPARN*SLN
      if(associated(DRNNIR)) DRNNIR = DRNIRN*SLN

      if(associated( SLRSF))  SLRSF = (DRUVRN+DFUVRN+DRPARN+DFPARN+DRNIRN+DFNIRN)*SLR

      if(associated(SLRSFC)) then
         where(ALB /= MAPL_UNDEF)
            SLRSFC = (FSCN(:,:,LM)*SLR)/(1.-ALB)
         elsewhere
            SLRSFC = 0.0
         end where
      end if

      if(associated(SLRSFNA)) then
         where(ALB /= MAPL_UNDEF)
            SLRSFNA = (FSWNAN(:,:,LM)*SLR)/(1.-ALB)
         elsewhere
            SLRSFNA = 0.0
         end where
      end if

      if(associated(SLRSFCNA)) then
         where(ALB /= MAPL_UNDEF)
            SLRSFCNA = (FSCNAN(:,:,LM)*SLR)/(1.-ALB)
         elsewhere
            SLRSFCNA = 0.0
         end where
      end if

      if(associated(   SLRSUF)) SLRSUF  = ALB*(DRUVRN+DFUVRN+DRPARN+DFPARN+DRNIRN+DFNIRN)*SLR
      
      if(associated(  SLRSUFC)) then
        where(ALB /= MAPL_UNDEF)
           SLRSUFC = ALB*(FSCN(:,:,LM)/(1.-ALB))*SLR
        elsewhere
           SLRSUFC = 0.0
        end where
      end if

      if(associated( SLRSUFNA)) then
        where(ALB /= MAPL_UNDEF)
           SLRSUFNA = ALB*(FSWNAN(:,:,LM)/(1.-ALB))*SLR
        elsewhere
           SLRSUFNA = 0.0
        end where
      end if

      if(associated(SLRSUFCNA)) then
        where(ALB /= MAPL_UNDEF)
           SLRSUFCNA = ALB*(FSCNAN(:,:,LM)/(1.-ALB))*SLR
        elsewhere
           SLRSUFCNA = 0.0
        end where
      end if
  

! Fill 3D FLuxes
!---------------

      DO L=0,LM
         ! Fill Export Net Fluxes from Internal
         ! ------------------------------------
         if(associated(FSW  )) FSW  (:,:,L) = FSWN  (:,:,L)*SLR
         if(associated(FSC  )) FSC  (:,:,L) = FSCN  (:,:,L)*SLR
         if(associated(FSWNA)) FSWNA(:,:,L) = FSWNAN(:,:,L)*SLR
         if(associated(FSCNA)) FSCNA(:,:,L) = FSCNAN(:,:,L)*SLR

         ! Fill Export Up Fluxes from Internal
         ! -----------------------------------
         if(associated(FSWU  )) FSWU  (:,:,L) = FSWUN  (:,:,L)*SLR
         if(associated(FSCU  )) FSCU  (:,:,L) = FSCUN  (:,:,L)*SLR
         if(associated(FSWUNA)) FSWUNA(:,:,L) = FSWUNAN(:,:,L)*SLR
         if(associated(FSCUNA)) FSCUNA(:,:,L) = FSCUNAN(:,:,L)*SLR

         ! Fill Export Down Fluxes from (Net + Up) Internal
         ! ------------------------------------------------
         if(associated(FSWD  )) FSWD  (:,:,L) = (FSWN  (:,:,L) + FSWUN  (:,:,L))*SLR
         if(associated(FSCD  )) FSCD  (:,:,L) = (FSCN  (:,:,L) + FSCUN  (:,:,L))*SLR
         if(associated(FSWDNA)) FSWDNA(:,:,L) = (FSWNAN(:,:,L) + FSWUNAN(:,:,L))*SLR
         if(associated(FSCDNA)) FSCDNA(:,:,L) = (FSCNAN(:,:,L) + FSCUNAN(:,:,L))*SLR
      end do

! Test if we are using RRTMG.
!----------------------------

      ! MAT This is needed in SetServices because we Export a per-band flux 
      !     and the number of bands are different between Chou and RRTMG
  
      call ESMF_ConfigGetAttribute(CF, RRTMG_SORAD , Label='USE_RRTMG_SORAD:', DEFAULT=0.0, RC=STATUS)
      VERIFY_(STATUS)
  
      USE_RRTMG = RRTMG_SORAD /= 0.0
      USE_CHOU  = .not.USE_RRTMG
  
      if (USE_CHOU) then
         NUM_BANDS_SOLAR = NB_CHOU
      else
         NUM_BANDS_SOLAR = NB_RRTMG
      end if
 
! Fill 3D per-band Fluxes
! -----------------------

      do IB = 1, NUM_BANDS_SOLAR
         if(associated(FSWBAND  )) FSWBAND  (:,:,IB) = FSWBANDN  (:,:,IB)*SLR
         if(associated(FSWBANDNA)) FSWBANDNA(:,:,IB) = FSWBANDNAN(:,:,IB)*SLR
      end do

! Fill 2D Fluxes
!---------------

      if(associated(   RSR))    RSR =       FSWN(:,:, 0) *SLR
      if(associated(   RSC))    RSC =       FSCN(:,:, 0) *SLR
      if(associated( RSRNA))  RSRNA =     FSWNAN(:,:, 0) *SLR
      if(associated( RSCNA))  RSCNA =     FSCNAN(:,:, 0) *SLR
      if(associated(  RSRS))   RSRS =       FSWN(:,:,LM) *SLR
      if(associated(  RSCS))   RSCS =       FSCN(:,:,LM) *SLR
      if(associated(RSRSNA)) RSRSNA =     FSWNAN(:,:,LM) *SLR
      if(associated(RSCSNA)) RSCSNA =     FSCNAN(:,:,LM) *SLR
      if(associated(   OSR))    OSR = (1.-  FSWN(:,:, 0))*SLR
      if(associated(OSRCLR)) OSRCLR = (1.-  FSCN(:,:, 0))*SLR
      if(associated( OSRNA))  OSRNA = (1.-FSWNAN(:,:, 0))*SLR
      if(associated(OSRCNA)) OSRCNA = (1.-FSCNAN(:,:, 0))*SLR

! SOLAR TO OBIO conversion ...
! Done in wavenum [cm^-1] space for reasons detailed under OBIO_bands_wavenum declaration
! ---------------------------------------------------------------------------------------

      if (SOLAR_TO_OBIO) then
         if (associated(DROBIO) .or. associated(DFOBIO)) then

            allocate(SOLAR_bands_wavenum(2,NUM_BANDS_SOLAR), stat=STATUS)
            VERIFY_(STATUS)
            allocate(SOLAR_band_number_in_wvn_order(NUM_BANDS_SOLAR), stat=STATUS)
            VERIFY_(STATUS)

            ! first load SOLAR_bands_wavenum and specify ordering
            !    that makes it monotonically increasing ...

            if (USE_RRTMG) then

               ! note: RRTMG bands are [wavenum1(jpb1:jpb2),wavenum2(jpb1:jpb2)] in [cm^-1]
               ! The index jpb1:jpb2 (16:29) is over the 14 bands. Band 14 is OUT of order.

               ASSERT_(jpb2-jpb1+1     == 14) ! 'RRTMG band inconsistency!'
               ASSERT_(NUM_BANDS_SOLAR == 14) ! 'wrong number of RRTMG bands!'

               ! load RRTMG bands (2,NB_RRTMG) [cm^-1]
               SOLAR_bands_wavenum (1,:) = wavenum1(jpb1:jpb2)
               SOLAR_bands_wavenum (2,:) = wavenum2(jpb1:jpb2)

               ! RRTMG band 14 comes before band 1 in increasing wavenumber
               SOLAR_band_number_in_wvn_order(1) = 14
               SOLAR_band_number_in_wvn_order(2:14) = [(i, i=1,13)]

            else if (USE_CHOU) then

               ! Source CHOU_bands_nm (2,NB_CHOU) is ordered in increasing waveLENGTH.
               ! See CHOU_bands_nm declaration for note about band 2a/b.

               ! Load Chou-Suarez bands (2,NB_CHOU) [cm^-1].
               ! Flip waveLENGTH bounds to waveNUMBER bounds.
               SOLAR_bands_wavenum (1,:) = 1.e7 / CHOU_bands_nm(2,:)
               SOLAR_bands_wavenum (2,:) = 1.e7 / CHOU_bands_nm(1,:)

               ! Specify sorting of Chou-Suarez bands in increasing waveNUMBER
               SOLAR_band_number_in_wvn_order = [(i, i=NUM_BANDS_SOLAR,1,-1)]

            endif

            ! zero accumulators
            if (associated(DROBIO)) DROBIO = 0.
            if (associated(DFOBIO)) DFOBIO = 0.

            ! We attempt to do the conversion efficiently by taking into account the band
            ! ordering in increasing wavenumber and knowing there are no gaps or overlaps
            ! in either SOLAR or OBIO bands.
            sfirst = .true.
            ofirst = .true.
            kb_start = NB_OBIO
            do jb = 1,NUM_BANDS_SOLAR
               ib = SOLAR_band_number_in_wvn_order(jb)
               ! SOLAR band (swvn1,swvn2)
               ! Note: check SOLAR band continuity before updating swvn2
               swvn1 = SOLAR_bands_wavenum(1,ib)
               if (.not.sfirst) then
                  ASSERT_(swvn1 == swvn2) !'SOLAR bands not complete and unique!')
               end if
               swvn2 = SOLAR_bands_wavenum(2,ib)
               sfirst = .false.

               ! Now find which OBIO bands the SOLAR band contributes to. OBIO bands
               ! are in increasing wavelength, decreasing wavenumber, so this loop is
               ! in increasing wavenumber.
               do kb = kb_start,1,-1
                  ! OBIO band (owvn1,owvn2)
                  ! Note: check OBIO band continuity before updating owvn2
                  owvn1 = OBIO_bands_wavenum(1,kb)
                  if (.not.ofirst) then
                     if (kb .ne. kb_used_last) then
                        ASSERT_(owvn1 == owvn2) ! 'OBIO bands not complete and unique!')
                     end if
                  end if
                  owvn2 = OBIO_bands_wavenum(2,kb)
                  kb_used_last = kb
                  ofirst = .false.

                  ! if we exit the OBIO band loop for any reason, begin
                  ! processing the next SOLAR band into the same OBIO band
                  kb_start = kb

                  ! OBIO band has wavenumbers all higher than current SOLAR
                  ! band, so immediately move on to the next SOLAR band
                  if (owvn1 >= swvn2) exit

                  ! OBIO band has wavenumbers all lower than current SOLAR
                  ! band, so skip it and move on to the next OBIO band
                  if (owvn2 <= swvn1) cycle

                  ! now there is some overlap between the SOLAR and OBIO bands ...

                  ! accumulate assuming constant energy spread across each SOLAR band, sfrac in (0,1]
                  sfrac = (min(swvn2,owvn2)-max(swvn1,owvn1)) / (swvn2 - swvn1)
                  if (associated(DROBIO)) DROBIO (:,:,kb) = DROBIO (:,:,kb) + DRBANDN (:,:,ib) * sfrac
                  if (associated(DFOBIO)) DFOBIO (:,:,kb) = DFOBIO (:,:,kb) + DFBANDN (:,:,ib) * sfrac

                  ! OBIO band has some wavenumbers higher than current
                  ! SOLAR band, so move on to the next SOLAR band
                  if (owvn2 > swvn2) exit

               end do  ! kb (OBIO band)
            end do  ! jb (SOLAR band)

            ! unnormalise to W/m2
            do kb = 1,NB_OBIO
               if (associated(DROBIO)) DROBIO (:,:,kb) = DROBIO (:,:,kb) * SLR
               if (associated(DFOBIO)) DFOBIO (:,:,kb) = DFOBIO (:,:,kb) * SLR
            end do

            deallocate(SOLAR_bands_wavenum)
            deallocate(SOLAR_band_number_in_wvn_order)

         end if
      end if

! Solar zenith angles: mean for time step and at end of time step.
!  Note SLR should not be used after this point!!
!-----------------------------------------------------------------

      if(associated( MCOSZ))  MCOSZ = ZTH
      if(associated(  COSZ))   COSZ = ZTHN

      RETURN_(ESMF_SUCCESS)
    end subroutine UPDATE_EXPORT


  end subroutine RUN

subroutine PackLoc(A,B,MSK,LENA,LENB,MASKIT)

  implicit none

  real,    intent(IN   ) :: A(LENA)
  real,    intent(  OUT) :: B(LENB)
  integer, intent(IN   ) :: LENA, LENB
  logical, intent(IN   ) :: MSK(LENA), MASKIT

  integer :: I, M

  if (MASKIT) then
     M = 1
     do I = 1,LENA
        if (MSK(I)) then
           B(M) = A(I)
           M = M+1
           if (M>LENB) exit
        end if
     end do
  else
     if (LENA /= LENB) stop
     B = A
  end if

end subroutine PackLoc

subroutine UnPackLoc(A,B,MSK,F,LENA,LENB,MASKIT)

   implicit none

   real,    intent(IN   ) :: A(LENA), F
   real,    intent(  OUT) :: B(LENB)
   integer, intent(IN   ) :: LENA, LENB
   logical, intent(IN   ) :: MSK(LENB), MASKIT

   integer :: I, M

   if(MASKIT) then
      M = 1
      do I = 1,LENB
         if (MSK(I)) then
            if (M>LENA) then
               B(I) = F
            else
               B(I) = A(M)
            end if
            M = M+1
         else
            B(I) = F
         end if
      end do
   else
      if (LENA /= LENB) stop
      B = A
   end if

end subroutine UnPackLoc

end module GEOS_SolarGridCompMod


subroutine ReOrder(Packed, UnPacked, MSK, Pdim, Udim, LM, DIR)
  integer, intent(IN   ) :: Pdim, Udim(2), LM, DIR 
  real,    intent(INOUT) ::   Packed(Pdim,*)
  real,    intent(INOUT) :: UnPacked(Udim(1),Udim(2),*)
  logical, intent(IN   ) :: MSK(Udim(1),Udim(2))

  integer :: I, J, L, M

  do L = 1,LM
     M = 1
     do J = 1,Udim(2)
        do I = 1,Udim(1)
           if (MSK(I,J)) then
              if(DIR==PACKIT) then
                 Packed(M,L) = UnPacked(I,J,L)
              else
                 Unpacked(I,J,L) = Packed(M,L)
              end if
              M = M+1
           else
              if(DIR/=PACKIT) then
                 UnPacked(I,J,L) = 0
              end if
           end if
        end do
     end do
  end do

  return
end subroutine ReOrder
