! $Id: GEOSgcm.F90,v 1.107 2014/12/08 19:25:08 ltakacs Exp $

! *********************************************************************
! *****                      Main Program                          ****
! *****         Finite-Volume Dynamical Core (Lin/Rood)            ****
! *****         Forced by GEOS5 Physics              ****
! *********************************************************************

#define I_AM_MAIN

#include "MAPL_Generic.h"

Program ExtData_Driver


   use MAPL_Mod
   use ExtDataRoot_GridCompMod, only:  ROOT_SetServices => SetServices

#ifdef __INTEL_COMPILER
   use ifport
#endif

   implicit none
   include "mpif.h"

!EOP

!EOC

   integer           :: STATUS
   character(len=18) :: Iam="ExtDataDriver_main"
  
   call MAPL_CAP(ROOT_SetServices, FinalFile='EGRESS', rc=STATUS)
   VERIFY_(STATUS)

   call exit(0)

 end Program ExtData_Driver

!EOC
