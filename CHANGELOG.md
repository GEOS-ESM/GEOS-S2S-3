# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- The new DataAtm has been backported from the current GEOS version to the S2Sv3 model, it replaced the old one. It includes the atmospheric component of OASIM (Ocean and Atmosphere Spectral Irradiance Model) that is used to force OBIO (NOBM).
- Added .rc files needed by DataAtm. 

### Changed

- In the coupled run the GOCART AERO_DP bundle consists of 20 groups of aerosols. When the model is coupled to DataAtm and OBIO, a corresponding AERO_DP bundle is created in DataAtm GC that consists only the 5 groups of aerosols that OBIO needs. For this reason, the way Surf GC handles the AERO_DP bundle has been modified to accommodate the two possible sources of the bundle. Previously, Surf GC expected all the 20 groups of GOCART aerosols to be present in the AERO_DP bundle, but now it has been tailored to processes any number out of the 20 groups that exist in the AERO_DP bundle.
- Updated `gcm_setup` with a question for data atmosphere in coupled runs

### Removed

- Removed old GEOSdataatm_GridComp.

