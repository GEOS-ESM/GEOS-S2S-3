
!=============================================================================
!
! $Id: gmi_io_constants.h,v 1.1.1.1 2008/02/12 16:06:36 trayanov Exp $
!
! CODE DEVELOPER
!   John Tannahill, LLNL
!   jrt@llnl.gov
!
! FILE
!   gmi_io_constants.h
!
! DESCRIPTION
!   This include file contains some input/output related constants.
!
!=============================================================================


!     -----------------
!     Dummy I/O values.
!     -----------------

      character (len=3), parameter :: CHAR_DUM_VALUE = 'NIL'

      integer, parameter :: INT_DUM_VALUE = -999


!     -----------------------------
!     NetCDF header field constant.
!     -----------------------------

      integer, parameter :: NETCDF_HDF = 3

