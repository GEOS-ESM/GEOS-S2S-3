module infnan
!-------------------------------------------------------------------------
!
! Purpose:
!
! Set parameters for the floating point flags "inf" Infinity
! and "nan" not-a-number. As well as "bigint" the point
! at which integers start to overflow. These values are used
! to initialize arrays with as a way to detect if arrays
! are being used before being set.
!
! Author: CCM Core group
!
! $Id: infnan.F90,v 1.2 2015/11/05 14:23:23 adarmeno Exp $
!
!-------------------------------------------------------------------------
#ifndef GEOS5_PORT
  use shr_kind_mod, only: r8 => shr_kind_r8
#else
  use MAPL_ConstantsMod, only: r8 => MAPL_R8
#endif

#ifdef __PGI
! quiet nan for portland group compilers
  real(r8), parameter :: inf = O'0777600000000000000000'
  real(r8), parameter :: nan = O'0777700000000000000000'
  integer,  parameter :: bigint = O'17777777777'    
#else
! signaling nan otherwise
  real(r8), parameter :: inf = O'0777600000000000000000'
  real(r8), parameter :: nan = O'0777610000000000000000'
  integer,  parameter :: bigint = O'17777777777'           ! largest possible 32-bit integer
#endif
  real(r8), parameter :: uninit_r8 = inf                   ! uninitialized floating point number
end module infnan
