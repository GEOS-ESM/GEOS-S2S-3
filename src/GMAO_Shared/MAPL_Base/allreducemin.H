! $Id: allreducemin.H,v 1.1 2009/02/23 20:01:21 trayanov Exp $

#ifdef NAME_
#undef NAME_
#endif

#define NAME_ MAPL_CommsAllReduceMin_

#include "overload.macro"

  subroutine SUB_ (vm, sendbuf, recvbuf, cnt, RC)
  type(ESMF_VM),        intent(in   )            :: vm
    TYPE_(kind=EKIND_), intent(IN   )            :: sendbuf DIMENSIONS_
    TYPE_(kind=EKIND_), intent(INOUT)            :: recvbuf DIMENSIONS_
    integer,            intent(IN   )            :: cnt
    integer,            intent(  OUT),  optional :: RC

    character(len=ESMF_MAXSTR), parameter :: IAM='MAPL_CommsAllReduceMin'
    integer                               :: status
    integer                               :: comm

    call ESMF_VMGet(vm, mpiCommunicator=COMM, rc=status)
    VERIFY_(STATUS)

    call mpi_allreduce ( sendbuf, recvbuf, cnt, MPITYPE_, &
                         MPI_MIN, comm, status )
    VERIFY_(STATUS)


    RETURN_(ESMF_SUCCESS)
  END SUBROUTINE SUB_

#undef NAME_

#undef DIMENSIONS_
#undef RANK_
#undef VARTYPE_

