!  $Id: regrid_forcing.F90,v 1.8.2.1 2019/06/27 19:19:37 yvikhlia Exp $

#include "MAPL_Generic.h"

#define DEALOC_(A) if(associated(A)) then; A=0; call MAPL_DeAllocNodeArray(A,rc=STATUS); if(STATUS==MAPL_NoShm) deallocate(A, stat=STATUS); VERIFY_(STATUS); NULLIFY(A); endif

module GenGridCompMod

! !USES:

   use ESMF
   use MAPL_Mod
   use MAPL_ShmemMod
   use MAPL_IOMod
   use, intrinsic :: iso_fortran_env

  implicit none

  private

! !PUBLIC MEMBER FUNCTIONS:

  public SetServices

!EOP

  type :: T_PrivateState
     type(MAPL_HorzTransForm) :: XFORM
     type(ESMF_Grid)           :: pgrid
     type(ESMF_Grid)           :: ogrid
     integer                   :: ntO
     integer                   :: ntP
     logical                   :: initialized=.false.
     logical                   :: transformNeeded
     integer                   :: PIM
     integer                   :: PJM
     integer                   :: GPIM
     integer                   :: GPJM
     type(ESMF_Time)           :: start_time
     type(ESMF_Time)           :: end_time
     logical                   :: select_time
     integer                   :: input_img
     integer                   :: input_jmg
  end type T_PrivateState

  type :: T_PrivateState_Wrap
     type(T_PrivateState), pointer :: ptr
  end type T_PrivateState_Wrap

  type(T_PrivateState), pointer :: privateState
contains

!BOP
! !IROUTINE: SetServices -- Sets ESMF services for this component

! !INTERFACE:
  subroutine SetServices ( GC, RC )

! !ARGUMENTS:
    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
    integer, optional                  :: RC  ! return code


!EOP

!=============================================================================
!
! ErrLog Variables

    character(len=ESMF_MAXSTR)              :: IAm
    integer                                 :: STATUS
    character(len=ESMF_MAXSTR)              :: COMP_NAME

!=============================================================================

! Begin...

! Get my name and set-up traceback handle
! ---------------------------------------

    Iam = 'SetServices'
    call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // Iam

! Set the Initialize, Run, Finalize entry points
! ----------------------------------------------

    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_INITIALIZE,   Initialize, RC=status)
    VERIFY_(STATUS)

    call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_RUN,  Run,  &
                                      RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_TimerAdd(GC, name="INITIALIZE"    ,RC=STATUS)
    VERIFY_(STATUS)

! Set generic init and final methods
! ----------------------------------

    call MAPL_GenericSetServices    ( gc, RC=STATUS)
    VERIFY_(STATUS)


    RETURN_(ESMF_SUCCESS)
  
  end subroutine SetServices

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine Initialize ( GC, IMPORT, EXPORT, CLOCK, RC )



! !ARGUMENTS:

    type(ESMF_GridComp),     intent(INOUT) :: GC     ! Gridded component 
    type(ESMF_State),        intent(INOUT) :: IMPORT ! Import state
    type(ESMF_State),        intent(INOUT) :: EXPORT ! Export state
    type(ESMF_Clock),        intent(INOUT) :: CLOCK  ! The clock
    integer, optional,       intent(  OUT) :: RC     ! Error code:

!EOP

! ErrLog Variables

    character(len=ESMF_MAXSTR)		   :: IAm
    integer				   :: STATUS
    character(len=ESMF_MAXSTR)             :: COMP_NAME

! Locals


! Locals with ESMF and MAPL types

    type (MAPL_MetaComp), pointer          :: MAPL 
    type(ESMF_Grid)                        :: Grid

! Locals

!   Variable to hold model state for each instance

    TYPE(T_PrivateState_Wrap) :: wrap


!   Local variables used for allocating exports pointers

    type(ESMF_Grid)                        :: pgrid
    type(ESMF_Grid)                        :: ogrid
    type(ESMF_DistGrid)                    :: distgrid
    type(ESMF_DELayout)                    :: layout
    integer                                :: COUNTS(3)
    integer                                :: DIMS(3)
    logical                                :: transformNeeded
    character(len=ESMF_MAXSTR)             :: TILINGFILE, OUTPUT_GRIDNAME, INPUT_GRIDNAME
    integer                                :: NX, NY, output_im, output_jm, input_im, input_jm,tdate
    integer                                :: yy,mm,dd,stat1,stat2
    type(ESMF_Config)                      :: cf
    type(ESMF_VM)                          :: vm
    integer :: i1w,inw,j1w,jnw

! Begin... 

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    Iam = "Initialize"
    call ESMF_GridCompGet( gc, NAME=comp_name, RC=status )
    VERIFY_(STATUS)
    Iam = trim(comp_name) // trim(Iam)

! Get my internal MAPL_Generic state
!-----------------------------------

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_GridCompGet(GC, CONFIG = CF, RC=STATUS)
    VERIFY_(STATUS)

! Allocate the private state...
!------------------------------
    
    allocate( PrivateSTATE , stat=STATUS )
    VERIFY_(STATUS)

    wrap%ptr => PrivateState

! And put it in the GC
!---------------------

    CALL ESMF_UserCompSetInternalState( GC, trim(comp_name)//'_internal_state',&
         WRAP, STATUS )
    VERIFY_(status)

    call ESMF_ConfigGetAttribute(cf,nx,label='NX:',RC=status)
    VERIFY_(STATUS)
    call ESMF_ConfigGetAttribute(cf,ny,label='NY:',RC=status)
    VERIFY_(STATUS)
    call ESMF_ConfigGetAttribute(cf,input_im,label='INPUT_IM:',RC=status)
    VERIFY_(STATUS)
    privateState%input_img=input_im
    call ESMF_ConfigGetAttribute(cf,input_jm,label='INPUT_JM:',RC=status)
    VERIFY_(STATUS)
    privateState%input_jmg=input_jm
    call ESMF_ConfigGetAttribute(cf,input_gridname,label='INPUT_GRIDNAME:',RC=status)
    VERIFY_(STATUS)
    call ESMF_ConfigGetAttribute(cf,tdate,label='START_DATE:',RC=stat1)
    if (stat1 == ESMF_SUCCESS) then
       call MAPL_UnpackTime(tdate,yy,mm,dd)
       call ESMF_TimeSet(privateState%start_time,yy=yy,mm=mm,dd=dd,rc=status)
       VERIFY_(STATUS)
    end if
    call ESMF_ConfigGetAttribute(cf,tdate,label='END_DATE:',RC=stat2)
    if (stat2 == ESMF_SUCCESS) then
       call MAPL_UnpackTime(tdate,yy,mm,dd)
       call ESMF_TimeSet(privateState%end_time,yy=yy,mm=mm,dd=dd,rc=status)
       VERIFY_(STATUS)
    end if
    privateState%select_time = ( (stat1 == ESMF_SUCCESS) .and. (stat2 == ESMF_SUCCESS) )
   

! Create grid for this component
!-------------------------------
    ogrid = MAPL_LatLonGridCreate(input_gridname,nx=nx,ny=ny, &
            IM_World=input_im,JM_World=input_jm,LM_World=1,rc=status)
    VERIFY_(STATUS)
    call ESMF_GridCompSet(GC, grid=ogrid, rc=status)
    VERIFY_(STATUS)

! Profilers
!----------

    call MAPL_TimerOn(MAPL,"TOTAL"     )
    call MAPL_TimerOn(MAPL,"INITIALIZE")

! Get the grid, configuration
!----------------------------

! Profilers
! ---------

    call MAPL_TimerOff(MAPL,"INITIALIZE")
    call MAPL_TimerOff(MAPL,"TOTAL"     )

! Generic initialize
! ------------------

    call MAPL_GenericInitialize( GC, IMPORT, EXPORT, CLOCK, RC=status )
    VERIFY_(STATUS)

! Get current ocean grid (ogrid)
!-------------------------------
    call ESMF_GridCompGet(GC, grid=ogrid, rc=status)
    VERIFY_(STATUS)
    call ESMF_GridGet(ogrid, DistGrid=distgrid, rc=status)
    VERIFY_(STATUS)
    call ESMF_DistGridGet(distGRID, deLayout=layout, RC=STATUS)
    VERIFY_(STATUS)

    call ESMF_ConfigGetAttribute(cf,output_im,label='OUTPUT_IM:',RC=status)
    VERIFY_(STATUS)
    call ESMF_ConfigGetAttribute(cf,output_jm,label='OUTPUT_JM:',RC=status)
    VERIFY_(STATUS)
    call ESMF_ConfigGetAttribute(cf,output_gridname,label='OUTPUT_GRIDNAME:',RC=status)
    VERIFY_(STATUS)
    pgrid = MAPL_LatLonGridCreate(output_gridname,nx=nx,ny=ny, &
            IM_World=output_im,JM_World=output_jm,LM_World=1,rc=status)
    VERIFY_(STATUS)

    PrivateState%ogrid=ogrid
    PrivateState%pgrid=pgrid

! Query Pgrid to save IM and JM
    call MAPL_GridGet(pgrid, localCellCountPerDim=COUNTS, &
         globalCellCountPerDim=dims, RC=STATUS)
    VERIFY_(STATUS)
    PrivateState%pim=counts(1)
    PrivateState%pjm=counts(2)
    PrivateState%gpim=dims(1)
    PrivateState%gpjm=dims(2)

! Check if the ocean grid is different then the grid for this plug
!-----------------------------------------------------------------
    transformNeeded = .false.
    if (pgrid /= ogrid) then
       transformNeeded = .true.
    end if
    PrivateState%transformNeeded = transformNeeded

    if (transformNeeded) then
! Create exchange grids from tile file
!-------------------------------------

       call MAPL_GetResource(MAPL, TILINGFILE, 'OUTPUT_TILING_FILE:', &
            default="mittile.til", RC=STATUS)
       VERIFY_(STATUS)

       call ESMF_VMGetCurrent(vm)
       call ESMF_GRID_INTERIOR(privateState%pgrid,I1w,INw,J1w,JNw)

       call MAPL_HorzTransformCreate (privatestate%xform, tilingfile, input_gridname, output_gridname, RootOnly=.false., &
                vm=vm, i1=i1w, in=inw, j1=j1w, jn=jnw, rc=rc)
       VERIFY_(STATUS)

    end if


! All Done
!---------
    call WRITE_PARALLEL("Done OUTPUT_PlugInit")

    RETURN_(ESMF_SUCCESS)
  end subroutine Initialize

!BOP

! !IROUTINE: RUN -- Run method for this component

! !INTERFACE:
  subroutine RUN ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:
  type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
  type(ESMF_State),    intent(inout) :: IMPORT ! Import state
  type(ESMF_State),    intent(inout) :: EXPORT ! Export state
  type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
  integer, optional,   intent(  out) :: RC     ! Error code:

! !DESCRIPTION: This version uses the MAPL\_GenericSetServices. This function sets
!                the Initialize and Finalize services, as well as allocating

!EOP


! ErrLog Variables

  character(len=ESMF_MAXSTR)          :: IAm
  integer                             :: STATUS
  character(len=ESMF_MAXSTR)          :: COMP_NAME

! Local vars

  REAL, pointer                       :: ODATA(:,:)
  REAL, pointer                       :: PDATA(:,:)

  integer :: IM, JM
  integer :: IM_WORLD, JM_WORLD
  type(ESMF_Grid) :: ogrid, pgrid
  type(ESMF_VM) :: vm
  type(ESMF_Time) :: currentTime, dateN
  type(ESMF_DistGrid) :: distgrid
  type(ESMF_DELayout)   :: layout
  integer :: stat, nvars, n
  integer :: UNIT_R, UNIT_W, npoints
  real    :: HDR(14)
  integer :: HEADER(14)
  character(len=ESMF_MAXSTR) :: filename, vname
  TYPE(T_PrivateState_Wrap) :: wrap
  type (MAPL_MetaComp), pointer          :: MAPL 
  logical :: amIRoot,dowrite
  type(ESMF_Time) :: start_interval, end_interval
  type(MAPL_NCIO)      :: maskNCIO
  real, allocatable :: mask(:,:)

!=============================================================================

! Begin... 

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

   Iam = "Run"
   call ESMF_GridCompGet( GC, name=COMP_NAME, VM=vm, RC=STATUS )
   VERIFY_(STATUS)
   Iam = trim(COMP_NAME) // Iam

! Get my internal MAPL_Generic state
!-----------------------------------

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
    VERIFY_(STATUS)

! Get Private State
!--------------------
    CALL ESMF_UserCompGetInternalState( GC, trim(comp_name)//'_internal_state',&
         WRAP, STATUS )
    VERIFY_(status)
    PrivateState => wrap%ptr 


! Get pointer(s) to Export vars
! ---------------------------------------------------

   call MAPL_AllocNodeArray(odata,(/privateState%input_img,privateState%input_jmg/),rc=status)
   if (status==MAPL_NoShm) allocate(odata(privateState%input_img,privateState%input_jmg),stat=status)
   VERIFY_(status)

! get filename (SEAWIS..., or fraci...)
   call MAPL_GetResource(MAPL, filename, 'INPUT_FILE:', default='', RC=STATUS)
   VERIFY_(STATUS)

   UNIT_R = getfile(filename)

! get filename (SEAWIS..., or fraci...)
   call MAPL_GetResource(MAPL, filename, 'OUTPUT_FILE:', default='', RC=STATUS)
   VERIFY_(STATUS)

   UNIT_W = getfile(filename)
   
   IM = PrivateState%pim
   JM = PrivateState%pjm
   IM_WORLD = PrivateState%gpim
   JM_WORLD = PrivateState%gpjm

   ogrid = PrivateState%ogrid
   pgrid = PrivateState%pgrid

   amIRoot = MAPL_Am_I_Root(vm)
   call ESMF_GridGet(pgrid, distGrid=distGrid, rc=STAT)
   VERIFY_(STAT)
   call ESMF_DistGridGet(distGrid, delayout=layout, rc=STAT)
   VERIFY_(STAT)

   do 

      if(amIRoot) then

         ! test for end-of-file by 
         ! making a blank read followed by backspace
         read(UNIT_R,IOSTAT=status)
      end if
      call MAPL_CommsBcast(layout, status, n=1, ROOT=MAPL_Root, rc=stat)
      VERIFY_(stat)

      if (status == IOSTAT_END) then
         RETURN_(ESMF_SUCCESS)
      end if
      VERIFY_(STATUS)
      call MAPL_Backspace(UNIT_R,layout,rc=status)
      VERIFY_(STATUS)
      
      if(amIRoot) then

         read(UNIT_R, iostat=status) HDR
         VERIFY_(STATUS)

      end if

      call MAPL_CommsBcast(layout,hdr,n=14,root=mapl_root,rc=stat)
      VERIFY_(Stat)
      HEADER = nint(HDR)
      call ESMF_TimeSet(start_interval,yy=header(1),mm=header(2),dd=header(3),h=header(4),m=header(5),s=header(6),rc=status)
      VERIFY_(STATUS)
      call ESMF_TimeSet(end_interval,yy=header(7),mm=header(8),dd=header(9),h=header(10),m=header(11),s=header(12),rc=status)
      VERIFY_(STATUS)
   
      if (privateState%select_time) then 
         dowrite = (end_interval > privatestate%start_time) .and. (start_interval < privatestate%end_time) 
      else
         dowrite = .true.
      end if

      if (amIRoot .and. dowrite) then

         HDR(13) = IM_WORLD
         HDR(14) = JM_WORLD
         write(UNIT_W) HDR

      end if

      call ESMF_VMBarrier(vm, rc=status)
      VERIFY_(STATUS)
      
      allocate(PDATA(IM,JM), stat=status)
      VERIFY_(STATUS)

      if (amIRoot) then
         read(unit_r,iostat=status)odata
         VERIFY_(STATUS)

! get land-sea mask of input data
         call MAPL_GetResource(MAPL, filename, 'MASK_FILE:', default='', RC=STATUS)
         VERIFY_(STATUS)

         if(filename .ne. '') then
            allocate(mask(size(odata,1), size(odata,2)))
            
            maskNCIO = MAPL_NCIOOpen(filename,rc=status)
            VERIFY_(STATUS)
         
            call MAPL_VarRead(maskNCIO,'lsmask',mask,rc=status)
            VERIFY_(STATUS)

            where(mask == 0.0)
               odata = MAPL_UNDEF
            end where
            deallocate(mask)
         end if         
      end if

      if (dowrite) then

         npoints = privateState%input_img*privateState%input_jmg
         call MAPL_BcastShared(VM, Data=odata, N=npoints, Root=0, RootOnly=.false., rc=status)
         VERIFY_(STATUS)
         call MAPL_SyncSharedMemory(rc=status)
         VERIFY_(STATUS)
         call MAPL_HorzTransformRun(privateState%xform,odata,pdata,mapl_undef,rc=status)
         VERIFY_(status)
         call MAPL_SyncSharedMemory(rc=status)
         VERIFY_(STATUS)
         call MAPL_VarWrite(unit_w, grid=pgrid, a=pdata, rc=status)
         VERIFY_(STATUS)
      end if

   enddo

   deallocate(pdata)
   DEALOC_(odata)

   RETURN_(ESMF_SUCCESS)

  end subroutine RUN

end module GenGridCompMod

! $Id: regrid_forcing.F90,v 1.8.2.1 2019/06/27 19:19:37 yvikhlia Exp $

!
! Main program for the TM test
!

Program regrid_forcing
  integer :: status

  interface 
     subroutine do_regrid_forcing(rc)
        implicit none
        integer, optional :: rc
     end subroutine do_regrid_forcing
  end interface

  call do_regrid_forcing(status)

end Program regrid_forcing

Subroutine do_regrid_forcing(rc)
  use ESMF
  use MAPL_Mod

  use GenGridCompMod,          only : Root_SetServices => SetServices

  implicit none
  integer, optional :: rc

  type(ESMF_Clock)    :: clock
  type(ESMF_VM)       :: vm

  integer             :: I
  type (ESMF_Time)    :: CURRENTTIME

  character(len=24)  :: timeStamp
  type(ESMF_Alarm)            :: alarm
  type(ESMF_Time)             :: ringTime
!@  type(ESMF_Time)             :: startTime
!@  type(ESMF_Time)             :: stopTime
!@  type(ESMF_Time)             :: targetTime
  type(ESMF_TimeInterval)     :: Frequency
  type(ESMF_TimeInterval)     :: timeStep

  logical                     :: R

  integer             :: status
  character(len=ESMF_MAXSTR), parameter   :: IAm='UnitSysTest'
  integer :: alfreq
  integer :: alst
  logical :: as

  integer                      :: ROOT
  integer                      :: HIST


! A MAPL object for the cap
!--------------------------

  type(MAPL_MetaComp)          :: MAPLOBJ

! The children's GCs and IM/Ex states
!------------------------------------

  type(ESMF_GridComp), pointer :: GCS(:)
  type(ESMF_State),    pointer :: IMPORTS(:)
  type(ESMF_State),    pointer :: EXPORTS(:)

  type(ESMF_Config)            :: cf_root
  type(ESMF_Config)            :: cf_hist
  character(len=ESMF_MAXSTR), parameter :: CF_FILE='REGRID_FORCING.rc'
  logical                      :: frwd
!                                -----

!  Initialize framework
!  --------------------
  call ESMF_Initialize(vm=vm, logKindFlag=ESMF_LOGKIND_NONE,rc=STATUS)
  VERIFY_(STATUS)

!  Setup config
!  ------------

  cf_root = ESMF_ConfigCreate(rc=STATUS )
  VERIFY_(STATUS)
  call ESMF_ConfigLoadFile(cf_root, CF_FILE, rc=STATUS )
  VERIFY_(STATUS)

   !  Create Root child
   !-------------------
  call MAPL_Set(MAPLOBJ, CF=CF_ROOT, RC=STATUS)
  VERIFY_(STATUS)

  ROOT = MAPL_AddChild ( MAPLOBJ,     &
       name       = "INPUT",        &
       SS         = ROOT_SetServices, &
       rc=STATUS )  
  VERIFY_(STATUS)


  call MAPL_ProfDisable( rc=STATUS )
  VERIFY_(STATUS)
  call MAPL_MemUtilsDisable( rc=STATUS )
  VERIFY_(STATUS)


   !  Query MAPL for the the children's for GCS, IMPORTS, EXPORTS
   !-------------------------------------------------------------

  call MAPL_Get ( MAPLOBJ, GCS=GCS, GIM=IMPORTS, GEX=EXPORTS, RC=STATUS )
  VERIFY_(STATUS)


! Set-up application clock

  call AppClockCreate(clock, cf_root, status)  
  VERIFY_(STATUS)

  call ESMF_GridCompInitialize ( GCS(ROOT), importState=IMPORTS(ROOT), &
       exportState=EXPORTS(ROOT), clock=CLOCK, userRC=STATUS )
  VERIFY_(STATUS)

  call ESMF_GridCompRun( GCS(ROOT), importState=IMPORTS(ROOT), &
       exportState=EXPORTS(ROOT), clock=CLOCK, userRC=STATUS )
  VERIFY_(STATUS)

!  Finalize
!  --------

  call ESMF_Finalize(rc=STATUS)
  VERIFY_(STATUS)

  RETURN_(STATUS)

contains
  subroutine AppClockCreate(clock, config, rc)
    type(ESMF_Clock), intent(OUT)    :: clock
    type(ESMF_Config), intent(INOUT) :: config
    integer, optional,   intent(OUT) :: rc


    ! local vars
    integer  :: status
    integer             :: ts
    integer             :: start_year, start_month, start_date, start_hour
    integer             :: stop_year, stop_month, stop_date, stop_hour
    type(ESMF_Time)     :: startTime
    type(ESMF_Time)     :: stopTime
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Calendar), save :: gregorianCalendar
    character(len=ESMF_MAXSTR), parameter   :: IAm='AppClockCreate'


    ! initialize calendar to be Gregorian type
    gregorianCalendar = ESMF_CalendarCreate( ESMF_CALKIND_GREGORIAN, name="ApplicationCalendar", rc=status )
    VERIFY_(STATUS)
    call ESMF_CalendarSetDefault(ESMF_CALKIND_GREGORIAN, RC=STATUS)
    VERIFY_(STATUS)
    
    call ESMF_ConfigGetAttribute(config, start_year, label ='start_year:', default=2009, rc = status )
    VERIFY_(STATUS)
    call ESMF_ConfigGetAttribute(config, start_month, label ='start_month:', default=08, rc = status )
    VERIFY_(STATUS)
    call ESMF_ConfigGetAttribute(config, start_date, label ='start_date:', default=21, rc = status )
    VERIFY_(STATUS)
    call ESMF_ConfigGetAttribute( config, start_hour, label ='start_hour:', default=21, rc = status )
    VERIFY_(STATUS)

    ! initialize start time
    call ESMF_TimeSet(startTime, &
         YY=start_year, MM=start_month, DD=start_date, &
         H=start_hour, M=0, S=0, calendar=gregorianCalendar, rc=status)
    VERIFY_(STATUS)
    
    call ESMF_ConfigGetAttribute( config, stop_year, label ='stop_year:', default=start_year, rc = status )
    VERIFY_(STATUS)
    call ESMF_ConfigGetAttribute( config, stop_month, label ='stop_month:', default=start_month, rc = status )
    VERIFY_(STATUS)
    call ESMF_ConfigGetAttribute( config, stop_date, label ='stop_date:', default=start_date+5, rc = status )
    VERIFY_(STATUS)
    call ESMF_ConfigGetAttribute( config, stop_hour, label ='stop_hour:', default=start_hour, rc = status )
    VERIFY_(STATUS)

    ! initialize stop time
    call ESMF_TimeSet(stopTime, YY=stop_year, MM=stop_month, DD=stop_date, &
         H=stop_hour, M=0, S=0, calendar=gregorianCalendar, rc=status)
    VERIFY_(STATUS)
    
    call ESMF_ConfigGetAttribute( config, ts, label ='timestep:', &
         default=1800, rc = status )
    VERIFY_(STATUS)
    ! initialize time interval
    call ESMF_TimeIntervalSet(timeStep, S=ts, rc=status)
    VERIFY_(STATUS)
    
    ! initialize the clock with the above values
    clock = ESMF_ClockCreate( name="ApplClock", timeStep=timeStep, &
         startTime=StartTime, stopTime=StopTime, rc=STATUS )
    VERIFY_(STATUS)


    RETURN_(STATUS)
  end subroutine AppClockCreate
  
  subroutine PrintClock(clock, rc)
    type (ESMF_Clock) :: clock
    integer, optional :: rc

    type(ESMF_Time)                   :: currentTime
    type(ESMF_TimeInterval)           :: TimeStep
    character(len=ESMF_MAXSTR)        :: TimeString
    integer                           :: secs
    character(len=79)                 :: OUT_LINE

    call ESMF_ClockGet(clock, currTime=currentTime, rc=rc)
    call ESMF_TimeGet(currentTime, timeString=TimeString, rc=rc)
    call ESMF_ClockGet(clock, timeStep=TimeStep, rc=rc)
    call ESMF_TimeIntervalGet(TimeStep, S=secs, rc=rc)

!    call WRITE_PARALLEL(" ")
    print *, " "
    write(OUT_LINE, FMT='("Current state of the clock: ", A)') &
         TimeString(1:19)
!    call WRITE_PARALLEL(OUT_LINE)
    print *, trim(OUT_LINE)
    write(OUT_LINE, FMT='("  Clock interval = ",F14.1," seconds")') &
         float(secs)
!    call WRITE_PARALLEL(OUT_LINE)
    print *, trim(OUT_LINE)
    call esmf_Clockprint(clock)

  end subroutine PrintClock

end Subroutine do_regrid_forcing


