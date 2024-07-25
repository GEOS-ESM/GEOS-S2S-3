module MemUtilsMod

  implicit none
  private

  public MemReport

  include "mpif.h"

  contains

subroutine get_unit ( iunit )
  implicit none
!
  integer i
  integer ios
  integer iunit
  logical lopen

  iunit = 0

  do i = 1, 99

    if ( i /= 5 .and. i /= 6 ) then

      inquire ( unit = i, opened = lopen, iostat = ios )

      if ( ios == 0 ) then
        if ( .not. lopen ) then
          iunit = i
          return
        end if
      end if

    end if

  end do

  return
end subroutine get_unit

  subroutine MemReport(comm,decorator,rc)
     integer, intent(in) :: comm
     character(len=*), intent(in) :: decorator
     integer, intent(out), optional :: rc

      real :: mem_total,mem_used,percent_used
      real :: committed_total,committed,percent_committed
      integer :: rank,status
      character(len=:), allocatable :: extra_message

#ifdef sysDarwin
      if (present(rc)) then
         rc = 0
         return
      end if
#endif
      !call MPI_Barrier(comm,status)
      call MemUsed(mem_total,mem_used,percent_used)
      call MemCommited(committed_total,committed,percent_committed)
      call MPI_Comm_Rank(comm,rank,status)
      if (rank == 0) write(*,'("Mem report ",A35," ",f5.1,"% : ",f5.1,"% Mem Comm:Used")')trim(decorator),percent_committed,percent_used

  end subroutine

  subroutine MemUsed ( memtotal, used, percent_used, RC )
     real, intent(out) :: memtotal, used, percent_used
     integer, optional, intent(OUT  ) :: RC

     ! This routine returns the memory usage on Linux systems.
     ! It does this by querying a system file (file_name below).

     character(len=32) :: meminfo   = '/proc/meminfo'
     character(len=32) :: string
     integer :: mem_unit
     real    :: multiplier, available

     character(len=128), parameter :: IAm="MemUtils:MemUsed"
     integer :: status

     available = -1
     memtotal = -1


     call get_unit(mem_unit)
     open(UNIT=mem_unit,FILE=meminfo,FORM='formatted',IOSTAT=STATUS)
     !_VERIFY(STATUS)

     ! Note: On at least one CircleCI compute machine, this was returning IOSTAT=9, with an IOMSG of:
     !          permission to access file denied, unit 100, file /proc/meminfo
     !       So, instead, if we get this issue, just act like macOS and not return useful info rather
     !       than crashing the model
     if (STATUS /= 0) then
        memtotal = 0.0
        used = 0.0
        percent_used = 0.0
        if (present(rc)) then
           rc = 0
           return
        end if
     end if

     do
        read (mem_unit,'(a)', end=20) string
        if ( index ( string, 'MemTotal:' ) == 1 ) then  ! High Water Mark
           read (string(10:LEN_trim(string)-2),*) memtotal
           multiplier = 1.0
           if (trim(string(LEN_trim(string)-1:)) == "kB" ) &
                multiplier = 1.0/1024. ! Convert from kB to MB
           memtotal = memtotal * multiplier
        endif
        if ( index ( string, 'MemAvailable:' ) == 1 ) then  ! Resident Memory
           multiplier = 1.0
           read (string(14:LEN_trim(string)-2),*) available
           if (trim(string(LEN_trim(string)-1:)) == "kB" ) &
                multiplier = 1.0/1024. ! Convert from kB to MB
           available = available * multiplier
        endif
     enddo
20   close(mem_unit)

     if (memtotal >= 0 .and. available >= 0) then
        used = memtotal-available
        percent_used = 100.0*(used/memtotal)
     else
        ! fail, but don't crash
        used = -1
        percent_used = -1
     end if

     if (present(rc)) then
        rc = 0
        return
     end if
  end subroutine MemUsed

subroutine MemCommited ( memtotal, committed_as, percent_committed, RC )

real, intent(out) :: memtotal, committed_as, percent_committed
integer, optional, intent(OUT  ) :: RC

! This routine returns the memory usage on Linux systems.
! It does this by querying a system file (file_name below).

character(len=32) :: meminfo   = '/proc/meminfo'
character(len=32) :: string
integer :: mem_unit
real    :: multiplier

character(len=128), parameter :: IAm="MemUtils:MemCommited"
integer :: status

  memtotal = 0.0
  committed_as = 0.0
  percent_committed = 0.0

  multiplier = 1.0

  call get_unit(mem_unit)
  open(UNIT=mem_unit,FILE=meminfo,FORM='formatted',IOSTAT=STATUS)

  ! Note: On at least one CircleCI compute machine, this was returning IOSTAT=9, with an IOMSG of:
  !          permission to access file denied, unit 100, file /proc/meminfo
  !       So, instead, if we get this issue, just act like macOS and not return useful info rather
  !       than crashing the model
  if (STATUS /= 0) then
     memtotal = 0.0
     committed_as = 0.0
     percent_committed = 0.0
  end if

  do; read (mem_unit,'(a)', end=20) string
    if ( INDEX ( string, 'MemTotal:' ) == 1 ) then  ! High Water Mark
      read (string(10:LEN_TRIM(string)-2),*) memtotal
      if (TRIM(string(LEN_TRIM(string)-1:)) == "kB" ) &
        multiplier = 1.0/1024. ! Convert from kB to MB
      memtotal = memtotal * multiplier
    endif
    if ( INDEX ( string, 'Committed_AS:' ) == 1 ) then  ! Resident Memory
      read (string(14:LEN_TRIM(string)-2),*) committed_as
      if (TRIM(string(LEN_TRIM(string)-1:)) == "kB" ) &
        multiplier = 1.0/1024. ! Convert from kB to MB
      committed_as = committed_as * multiplier
    endif
  enddo
20 close(mem_unit)

   percent_committed = 100.0*(committed_as/memtotal)
     if (present(rc)) then
        rc = 0
        return
     end if

end subroutine MemCommited

end module MemUtilsMod
