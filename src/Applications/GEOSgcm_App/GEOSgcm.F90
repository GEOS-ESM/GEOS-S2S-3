! $Id: GEOSgcm.F90,v 1.107 2014/12/08 19:25:08 ltakacs Exp $

! *********************************************************************
! *****                      Main Program                          ****
! *****         Finite-Volume Dynamical Core (Lin/Rood)            ****
! *****         Forced by GEOS5 Physics              ****
! *********************************************************************

#define I_AM_MAIN

#include "MAPL_Generic.h"

Program GEOS5_Main


   use MAPL_Mod
#ifdef USE_GCS
   use GEOS_GcsGridCompMod, only:  ROOT_SetServices => SetServices
#else
   use GEOS_GcmGridCompMod, only:  ROOT_SetServices => SetServices
#endif

#ifdef __INTEL_COMPILER
   use ifport
#endif
   use m_inpak90

   implicit none
   include "mpif.h"

!EOP

!EOC

   integer           :: STATUS
   character(len=18) :: Iam="GEOS5_Main"

   integer           :: ensemble_comm
   integer           :: myRank,nProc,mycolor
   logical           :: runens
   character(len=6)  :: dirname
   integer           :: nensembles, ensemble_size

#ifdef __PGI
   integer           :: chdir
#endif

   inquire(file="ensemble_setup.rc",exist=runens)

   if (runens) then

      call mpi_init(status)
      VERIFY_(STATUS)
      call mpi_comm_size(MPI_COMM_WORLD,nProc,status)
      VERIFY_(STATUS)
      call mpi_comm_rank(MPI_COMM_WORLD,myRank,status)
      VERIFY_(STATUS)

      if (myrank ==0) then
         call i90_loadf("ensemble_setup.rc",status)
         VERIFY_(STATUS)
         call i90_label("N_ENSEMBLES:",status)
         VERIFY_(STATUS)
         nensembles = i90_gint(status)
         VERIFY_(STATUS)
         call i90_label("ENSEMBLE_SIZE:",status)
         VERIFY_(STATUS)
         ensemble_size = i90_gint(status)
         call i90_release
      endif
      call MPI_BCAST(nensembles, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, status)
      VERIFY_(STATUS)
      call MPI_BCAST(ensemble_size, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, status)
      VERIFY_(STATUS)

      ASSERT_(nProc == (ensemble_size*nensembles) )
      mycolor = myRank/ensemble_size
      call mpi_comm_split(MPI_COMM_WORLD,mycolor,myRank,ensemble_comm,status)
      VERIFY_(STATUS)
      write(dirname,"(A3,I0.3)")"mem",mycolor+1
      status = chdir(dirname)
      VERIFY_(STATUS)
      call MAPL_CAP(ROOT_SetServices, FinalFile='EGRESS', CommIn=ensemble_comm, rc=STATUS)
      VERIFY_(STATUS)
      call mpi_finalize(status)
      VERIFY_(STATUS)

   else
  
      call MAPL_CAP(ROOT_SetServices, FinalFile='EGRESS', rc=STATUS)
      VERIFY_(STATUS)

   end if

   call exit(0)

 end Program GEOS5_Main

!EOC
