#include "MAPL_Generic.h"

!=============================================================================
!BOP

! !MODULE: GEOS_GcsGridCompMod -- Parent DAS Component

! !INTERFACE:

module GEOS_GcsGridCompMod

! !USES:

  use ESMF
  use MAPL_Mod

  use GEOS_GcmGridCompMod,     only : GcmSetServices   => SetServices
  use GEOS_AnaGridCompMod,     only : AnaSetServices   => SetServices

  implicit none
  private

! !PUBLIC MEMBER FUNCTIONS:

  public SetServices

#ifndef USE_ODAS
!=============================================================================

! !DESCRIPTION: This gridded component (GC) combines 
 
  integer :: GCM  ! Global Atmopheric Model
  integer :: ANA  ! Global Analysis
!EOP

  type MAPL_RouteHandle
     type(ESMF_RouteHandle) :: RH
     logical                :: isNeeded
  end type MAPL_RouteHandle

  logical                             :: differentVMs
  type (ESMF_State)                   :: SIMP, SEXP
  type(MAPL_RouteHandle)              :: g2aRH(3)
  logical,                    pointer :: anaNeedsThis(:)
  character(len=ESMF_MAXSTR), pointer :: ANAIM(:), GCMEX(:)

contains

!BOP

! !IROUTINE: SetServices -- Sets ESMF services for this component

! !INTERFACE:

    subroutine SetServices ( GC, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
    integer, optional  , intent(  OUT) :: RC  ! return code

! !DESCRIPTION:  The SetServices for the Chemistry GC needs to register its
!   Initialize and Run.  It uses the MAPL\_Generic construct for defining 
!   state specs and couplings among its children.  In addition, it creates the   
!   children GCs and runs their respective SetServices.
!
! !REVISION HISTORY:
!
!   13Aug2007 Todling/daSilva  Add this component; still doesn't work as meant
!   08Jun2008 Todling          Merge with latest MAPL version as in DAS-215

!EOP

!=============================================================================
!
! ErrLog Variables

    character(len=ESMF_MAXSTR)              :: IAm
    integer                                 :: STATUS
    character(len=ESMF_MAXSTR)              :: COMP_NAME

! Locals

    integer                    :: I

    integer                    :: n, id
    integer                    :: nGCM, nANA
    integer                    :: myid, npes
    integer                    :: NX, NY
    integer, pointer           :: gcmPets(:), anaPets(:)
    type (ESMF_VM)             :: vm
    type(MAPL_MetaComp), pointer :: MAPL

    type (ESMF_Config)         :: CF
    integer                    :: DO_CICE_THERMO  ! default (=0) is to run without CICE
    integer                    :: CICE_N_ICE_CATEGORIES
    integer                    :: CICE_N_ICE_LAYERS

!=============================================================================

! Begin...

! Get my name and set-up traceback handle
! ---------------------------------------
    Iam = 'SetServices'
    call ESMF_GridCompGet( GC, NAME=COMP_NAME, CONFIG=CF, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // '::' // Iam

! nothing to register for this component
! --------------------------------------

! get vm
    call ESMF_VMGetCurrent(vm, rc=status)
    VERIFY_(STATUS)
    call ESMF_VmGet(VM, localPet=MYID, petCount=npes, rc=status)
    VERIFY_(STATUS)

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
    VERIFY_(STATUS)

! Get parameters for CICE
! ----------------------------

    call MAPL_GetResource (    MAPL,    DO_CICE_THERMO,        Label="USE_CICE_Thermo:" ,      DEFAULT=0, RC=STATUS)
    VERIFY_(STATUS)

    if (DO_CICE_THERMO /= 0) then
       call MAPL_GetResource ( MAPL,    CICE_N_ICE_CATEGORIES, LABEL="CICE_N_ICE_CATEGORIES:", DEFAULT=5, RC=status)
       VERIFY_(STATUS)
       call MAPL_GetResource ( MAPL,    CICE_N_ICE_LAYERS,     LABEL="CICE_N_ICE_LAYERS:",     DEFAULT=4, RC=status)
       VERIFY_(STATUS)

       call MAPL_ConfigSetAttribute(CF, CICE_N_ICE_CATEGORIES, Label="CICE_N_ICE_CATEGORIES:", RC=status)
       if (STATUS == ESMF_RC_NOT_FOUND) STATUS = ESMF_SUCCESS
       VERIFY_(STATUS)

       call MAPL_ConfigSetAttribute(CF, CICE_N_ICE_LAYERS,     Label="CICE_N_ICE_LAYERS:",     RC=status)
       if (STATUS == ESMF_RC_NOT_FOUND) STATUS = ESMF_SUCCESS
       VERIFY_(STATUS)
    endif

! get NX and NY from the resource file
    call MAPL_GetResource(MAPL, NX, LABEL="NX:", RC=status)
    VERIFY_(STATUS)
    call MAPL_GetResource(MAPL, NY, LABEL="NY:", RC=status)
    VERIFY_(STATUS)

    nGCM = NX *NY

    differentVMs = .false.
    if (nGCM < npes) then
       differentVMs = .true.
       nANA = npes - nGCM
       allocate(gcmPets(nGCM), anaPets(nANA), stat=status)
       VERIFY_(STATUS)
       
#ifdef GCM_STARTS_AT_PE0
       do I = 1, nGCM
          gcmPets(I) = I-1 ! 0-based
       end do

       do I = 1, nANA
          anaPets(I) = nGCM+I-1 ! 0-based
       end do
#else
       do I = 1, nANA
          anaPets(I) = I-1 ! 0-based
       end do

       do I = 1, nGCM
          gcmPets(I) = nANA+I-1 ! 0-based
       end do
#endif

       GCM = MAPL_AddChild(GC, NAME='GCM', SS=GcmSetServices, &
            petList=gcmPets, RC=STATUS)
       VERIFY_(STATUS)
       ANA = MAPL_AddChild(GC, NAME='ANA', SS=AnaSetServices, &
            petList=anaPets, RC=STATUS)
       VERIFY_(STATUS)

       deallocate(gcmPets, anaPets)

       ! Set the Initialize, Run and Finalize entry points
       !--------------------------------------------------

       call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_INITIALIZE, Initialize, RC=STATUS )
       VERIFY_(STATUS)
       call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN, Run, RC=STATUS )
       VERIFY_(STATUS)
       call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_FINALIZE, Finalize, RC=STATUS )
       VERIFY_(STATUS)
    else
       ASSERT_(nGCM == npes)
! Create childrens gridded components and invoke their SetServices
! ----------------------------------------------------------------
       GCM = MAPL_AddChild(GC, NAME='GCM', SS=GcmSetServices, RC=STATUS)
       VERIFY_(STATUS)
       ANA = MAPL_AddChild(GC, NAME='ANA', SS=AnaSetServices, RC=STATUS)
       VERIFY_(STATUS)
       
    end if

! Connect GCM with ANA
! --------------------

#ifndef DATAATM
    call MAPL0_AddConnectivity ( GC,                                         &
         SRC_NAME  = (/                                                      &
         'U            ','V            ','TV           ','Q            '   , &
         'O3PPMV       ',                'PHIS         ','PS           '   , &
         'TS           ','U10N         ','V10N         ','SNOMAS       '   , &
         'WET1         ','TSOIL1       ','Z0           ','QITOT        '   , &
         'QLTOT        ','FRLAND       ','FRLANDICE    ','FRLAKE       '   , &
         'FROCEAN      ','FRACI        '                                 /), &
         DST_NAME  = (/                                                      &
         'u            ','v            ','tv           ','sphu         '   , &
         'ozone        ',                'phis         ','ps           '   , &
         'ts           ','U10M         ','V10M         ','SNOWDP       '   , &
         'GWETTOP      ','TSOIL1       ','Z0M          ','qitot        '   , &
         'qltot        ','frland       ','frlandice    ','frlake       '   , & 
         'frocean      ','frseaice     '                                 /), &
         SRC_ID = GCM,                                        &
         DST_ID = ANA,                                        &
         RC=STATUS  )
    VERIFY_(STATUS)
#endif

    ! We manually fill all of ANA's imports
    call MAPL_TerminateImport    ( GC, CHILD = ANA, RC=STATUS  )
    VERIFY_(STATUS)

! Set services now
! ----------------
    call MAPL_GenericSetServices  ( GC, RC=STATUS )
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)
    
  end subroutine SetServices

! .................................
  subroutine MAPL0_AddConnectivity ( GC, SRC_NAME, SRC_ID, DST_NAME, DST_ID, RC )

    type(ESMF_GridComp),            intent(INOUT) :: GC ! Gridded component
    character (len=*),              intent(IN   ) :: SRC_NAME(:)
    character (len=*),              intent(IN   ) :: DST_NAME(:)
    integer,                        intent(IN   ) :: SRC_ID 
    integer,                        intent(IN   ) :: DST_ID 
    integer,              optional, intent(  OUT) :: RC     ! Error code:

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_AddConnectivityRename"
    integer                               :: STATUS
    integer                               :: N


    ASSERT_(size(SRC_NAME) ==  size(DST_NAME))

    if (.not. differentVMs) then
       call MAPL_AddConnectivity(GC, SRC_NAME, SRC_ID, DST_NAME, DST_ID, RC=status)
       VERIFY_(STATUS)

       RETURN_(ESMF_SUCCESS)
    end if

    N = size(SRC_NAME)
    allocate(ANAIM(N), GCMEX(N), stat=status)
    VERIFY_(STATUS)

    ASSERT_(SRC_ID == GCM)
    ASSERT_(DST_ID == ANA)

    GCMEX = SRC_NAME
    ANAIM = DST_NAME

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL0_AddConnectivity
! .................................


!=============================================================================
! The rest of this file contains routines that are executed ONLY when the code
! is running on different VMs
!=============================================================================

!BOP

! !IROUTINE: Initialize -- Initialize method for the GEOS ANA component

! !INTERFACE:

  subroutine Initialize ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
    type(ESMF_State),    intent(inout) :: IMPORT ! Import state
    type(ESMF_State),    intent(inout) :: EXPORT ! Export state
    type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
    integer, optional,   intent(  out) :: RC     ! Error code

! !DESCRIPTION: The Initialize method of the Composite Analysis Gridded Component.

!EOP

! ErrLog Variables

    character(len=ESMF_MAXSTR)              :: IAm 
    integer                                 :: STATUS
    character(len=ESMF_MAXSTR)              :: COMP_NAME
    
! Local derived type aliases

    type (MAPL_MetaComp), pointer       :: MAPL
    type (ESMF_GridComp),      pointer  :: GCS(:)
    type (ESMF_State),         pointer  :: GIM(:), GEX(:)

    type (ESMF_Field)                   :: FIELD
    type (ESMF_Field)                   :: FIELD0
    type (ESMF_VM)                      :: vm
    integer                             :: J
    integer                             :: dims
    character(len=ESMF_MAXSTR)          :: string
    integer                             :: freq
    type(ESMF_TimeInterval)             :: tstep
    type(ESMF_TimeInterval)             :: Frequency
    type(ESMF_Time)                     :: currTime
    type(ESMF_Time)                     :: ringTime
    type(ESMF_Alarm)                    :: ALARM

!=============================================================================

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    Iam = "Initialize"
    call ESMF_GridCompGet ( GC, name=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // Iam

! Get my internal MAPL_Generic state
!-----------------------------------

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
    VERIFY_(STATUS)

! get vm
    call ESMF_VMGetCurrent(vm, rc=status)
    VERIFY_(STATUS)

! create alarm for coupling/redist
!=================================
    call MAPL_GetResource(MAPL, freq, Label="COUPLE_FREQUENCY:", &
         default=1, rc=status)
    VERIFY_(STATUS)
    call ESMF_ClockGet(clock, currTime=currTime, timestep=tstep, rc=status)
    VERIFY_(STATUS)

    Frequency = freq*tstep

    RingTime=currTime
    Alarm = ESMF_AlarmCreate( name="COUPLE_"//trim(COMP_NAME), &
         clock=clock, RingTime=ringTime, RingInterval=Frequency, &
         sticky=.false., rc=status )
    VERIFY_(STATUS)
!   make sure this alrm is ringing on the first time step
    call ESMF_AlarmRingerOn(Alarm, rc=status)
    VERIFY_(STATUS)

! traditional Initialize

    call MAPL_Get ( MAPL, GCS=GCS, GIM = GIM, GEX = GEX, RC=STATUS )
    VERIFY_(STATUS)

! Create grid for this component ?
!---------------------------------
!    call MAPL_GridCreate(GC, rc=status)
!    VERIFY_(STATUS)

! Recursive setup of grids (should be disabled)
!    call ESMF_GridCompGet(GC,  grid=anagrid, rc=status)
!    VERIFY_(STATUS)
!    call ESMF_GridCompSet(GCS(ANA),  grid=anagrid, rc=status)
!    VERIFY_(STATUS)

! Call Initialize for every Child
!--------------------------------

     call MAPL_GenericInitialize ( GC, IMPORT, EXPORT, CLOCK,  RC=STATUS)
     VERIFY_(STATUS)

!     call ESMF_VMBarrier(vm, rc=status)
!     VERIFY_(STATUS)

     SIMP = ESMF_StateCreate(name = "GCS_super_imports", rc=status)
     VERIFY_(STATUS)
     SEXP = ESMF_StateCreate(name = "GCS_super_exports", rc=status)
     VERIFY_(STATUS)

     DO J = 1, size(ANAIM)
        call ESMF_StateGet ( GIM(ANA), ANAIM(J), FIELD, RC=STATUS )
        if (STATUS /= ESMF_SUCCESS) cycle
        call MAPL_StateAdd( SIMP, FIELD, RC=STATUS)
        VERIFY_(STATUS)
     END DO

     call write_parallel("DBG:reconcile IMPORTS")
     call ESMF_StateReconcile(SIMP, vm, ESMF_ATTRECONCILE_ON, rc=status)
     VERIFY_(STATUS)

     allocate(anaNeedsThis(size(ANAIM)), stat=status)
     VERIFY_(STATUS)

     DO J = 1, size(GCMEX)
        call ESMF_StateGet ( SIMP, ANAIM(J), FIELD, RC=STATUS )
        if (STATUS /= ESMF_SUCCESS) then
           anaNeedsThis(J) = .false. ! ANA does not care about this
           cycle
        else
           anaNeedsThis(J) = .true.
        end if
        
        call ESMF_StateGet ( GEX(GCM), GCMEX(J), FIELD, RC=STATUS )
        if (STATUS /= ESMF_SUCCESS) cycle ! this is not GCM pe

        ! force allocation of deferred exports
        call MAPL_AllocateCoupling(field, rc=status)
        VERIFY_(STATUS)
        call MAPL_StateAdd( SEXP, FIELD, RC=STATUS)
        VERIFY_(STATUS)
     END DO

     call write_parallel("DBG:reconcile EXPORTS")
     call ESMF_StateReconcile(SEXP, vm, ESMF_ATTRECONCILE_ON, rc=status)
     VERIFY_(STATUS)

     g2aRH(:)%isNeeded = .false.
     do J = 1, size(GCMEX)
        if (.not. anaNeedsThis(J)) cycle
        call ESMF_StateGet ( SIMP, ANAIM(J), FIELD, RC=STATUS )
        VERIFY_(STATUS)
        call ESMF_StateGet ( SEXP, GCMEX(J), FIELD0, RC=STATUS )
        VERIFY_(STATUS)
        call ESMF_AttributeGet(FIELD0, NAME='DIMS', VALUE=DIMS, RC=STATUS)
        VERIFY_(STATUS)

        if (.not. g2aRH(dims)%isNeeded) then
           g2aRH(dims)%isNeeded = .true.
           call WRITE_PARALLEL("DBG: redistStore called for "//trim(ANAIM(J)))
           call ESMF_FieldRedistStore(srcField=field0, dstField=field, &
                routehandle=g2aRH(dims)%rh, rc=status)
           VERIFY_(STATUS)
        end if
     end do

! ALT: at this point could also call ESMF_FieldRedist
!      but this is going to be needed only if ANA
!      needs valid (i.e. filled in) imports

     RETURN_(ESMF_SUCCESS)

   end subroutine Initialize

!=============================================================================

   subroutine Run ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

     type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
     type(ESMF_State),    intent(inout) :: IMPORT ! Import state
     type(ESMF_State),    intent(inout) :: EXPORT ! Export state
     type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
     integer, optional,   intent(  out) :: RC     ! Error code

! !DESCRIPTION: The Run method of the Composite Analysis Gridded Component.

!EOP

! ErrLog Variables

     character(len=ESMF_MAXSTR)          :: IAm 
     integer                             :: STATUS
     character(len=ESMF_MAXSTR)          :: COMP_NAME
     
     integer                             :: J
     integer                             :: dims
     type (MAPL_MetaComp), pointer       :: MAPL
     type (ESMF_GridComp),      pointer  :: GCS(:)
     type (ESMF_State),         pointer  :: GIM(:), GEX(:)
     type (ESMF_Field)                   :: FIELD, FIELD0
     type (ESMF_Alarm)                   :: ALARM
     type (ESMF_VM)                      :: vm
     logical                             :: timeToCouple

     Iam = "Run"
     call ESMF_GridCompGet ( GC, name=COMP_NAME, RC=STATUS )
     VERIFY_(STATUS)
     Iam = trim(COMP_NAME) // Iam
     
! Get my MAPL_Generic state
!--------------------------

     call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
     VERIFY_(STATUS)
     
! get vm
     call ESMF_VMGetCurrent(vm, rc=status)
     VERIFY_(STATUS)

! Get children and their im/ex states from my generic state.
!----------------------------------------------------------

     call MAPL_Get ( MAPL, GCS=GCS, GIM=GIM, GEX=GEX, RC=STATUS )
     VERIFY_(STATUS)

     call ESMF_ClockGetAlarm(clock, alarmname="COUPLE_"//trim(COMP_NAME), &
          alarm=ALARM, rc=status)
     VERIFY_(STATUS)

     timeToCouple = ESMF_AlarmIsRinging( ALARM, rc=status)
     VERIFY_(STATUS)

     if (timeToCouple) then
        call ESMF_AlarmRingerOff(ALARM, RC=STATUS)
        VERIFY_(STATUS)
     end if

! run GCMs
! --------

! this is only for debugging
!     call WRITE_PARALLEL("Running GCM")
     call ESMF_GridCompRun ( GCS(GCM), importState=GIM(GCM), exportState=GEX(GCM), clock=clock, userRc=status )
     VERIFY_(STATUS)


! run "couplers", i.e. redist
! ---------------------------
     if (timeToCouple) then
        do J = 1, size(GCMEX)
           if (.not. anaNeedsThis(J)) cycle
           call ESMF_StateGet ( SIMP, ANAIM(J), FIELD, RC=STATUS )
           VERIFY_(STATUS)
! this is only for debugging
!           call WRITE_PARALLEL("... doing redist for "//trim(ANAIM(J)))
           call ESMF_StateGet ( SEXP, GCMEX(J), FIELD0, RC=STATUS )
           VERIFY_(STATUS)
! ALT warning: I am not sure if everybody has DIMS properly set
           call ESMF_AttributeGet(FIELD0, NAME='DIMS', VALUE=DIMS, RC=STATUS)
           VERIFY_(STATUS)
           call ESMF_FieldRedist(srcField=FIELD0, dstField=FIELD, &
                routehandle=g2aRH(dims)%rh, rc=status)
           VERIFY_(STATUS)
        end do
        call ESMF_VMBarrier(vm, rc=status)
        VERIFY_(STATUS)
     end if


!  run ANA
! --------

     if (timeToCouple) then
! this is only for debugging
!        call WRITE_PARALLEL("Running analysis/couple")
        call ESMF_GridCompRun ( GCS(ANA), importState=GIM(ANA), exportState=GEX(ANA), clock=clock, userRc=status )
        VERIFY_(STATUS)

! run "couplers", i.e. redist (NONE yet)
! ---------------------------
     end if

     RETURN_(ESMF_SUCCESS)

   end subroutine Run

!=============================================================================

   subroutine Finalize ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

     type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
     type(ESMF_State),    intent(inout) :: IMPORT ! Import state
     type(ESMF_State),    intent(inout) :: EXPORT ! Export state
     type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
     integer, optional,   intent(  out) :: RC     ! Error code
     
! !DESCRIPTION: The Finalize method of the Composite Analysis Gridded Component.

!EOP

! ErrLog Variables

     character(len=ESMF_MAXSTR)          :: IAm 
     integer                             :: STATUS
     character(len=ESMF_MAXSTR)          :: COMP_NAME
     
     integer                             :: J
     
     Iam = "Finalize"
     call ESMF_GridCompGet ( GC, name=COMP_NAME, RC=STATUS )
     VERIFY_(STATUS)
     Iam = trim(COMP_NAME) // Iam

     call MAPL_GenericFinalize( GC, IMPORT, EXPORT, CLOCK, RC=STATUS )
     VERIFY_(STATUS)

! clean-up
     DO J=1,3 ! covers MAPL_DimsVertOnly, MAPL_DimsHorzOnly, MAPL_DimsHorzVert

        if (g2aRH(J)%isNeeded) then
           call ESMF_FieldRedistRelease(g2aRH(J)%rh, rc=status)
           VERIFY_(STATUS)
        end if
     END DO
     deallocate(anaNeedsThis)
     deallocate(ANAIM, GCMEX)
     
     call ESMF_StateDestroy(SIMP, rc=status)
     VERIFY_(STATUS)
     call ESMF_StateDestroy(SEXP, rc=status)
     VERIFY_(STATUS)

     RETURN_(ESMF_SUCCESS)
   end subroutine Finalize

!=============================================================================
#else
!=============================================================================

! !DESCRIPTION: This gridded component (GC) combines the GCM model and analyses 
 
!EOP

  type MAPL_RouteHandle
     type(ESMF_RouteHandle) :: RH
     logical                :: isNeeded
  end type MAPL_RouteHandle
  integer, pointer                :: GCM(:)  ! GCM
  integer                         :: ANA     ! Analysis

  integer                         :: NENS
  integer                         :: I0
  type(MAPL_RouteHandle), pointer    :: a2gRH(:,:),g2aRH(:,:)
  character(len=ESMF_MAXSTR)      :: ANAEX(13), GCMEX(19)

  type (ESMF_State),     pointer  :: EXS(:)
  type (ESMF_State),     pointer  :: IMP(:)
  character(len=ESMF_MAXSTR), pointer :: CHILD_NAME(:)
  integer                         :: N_CHILDREN

  type PetListType
     integer, pointer :: pet(:)
  end type PetListType


contains

!BOP

! !IROUTINE: SetServices -- Sets ESMF services for this component

! !INTERFACE:

    subroutine SetServices ( GC, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
    integer, optional                  :: RC  ! return code

! !DESCRIPTION:


! !REVISION HISTORY:
!
!   9Jul2008 Trayanov: first version

!EOP

!=============================================================================
!
! ErrLog Variables

    character(len=ESMF_MAXSTR)              :: IAm
    integer                                 :: STATUS
    character(len=ESMF_MAXSTR)              :: COMP_NAME

! Locals

    type (MAPL_MetaComp), pointer       :: MAPL
    integer, pointer                    :: JENS(:)
    type (PetListType), pointer         :: petList(:)
    type (ESMF_VM)                      :: vm
    integer                             :: I, J
    integer                             :: myid, npes
    character(len=ESMF_MAXSTR)          :: ENSNUM

!=============================================================================

! Get my name and set-up traceback handle
! ---------------------------------------
    Iam = 'SetServices'
    call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // '::' // Iam

! Set the Initialize, Run and Finalize entry points
!----------------------------------------

    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_INITIALIZE, Initialize, RC=STATUS )
    VERIFY_(STATUS)
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN, Run, RC=STATUS )
    VERIFY_(STATUS)
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_FINALIZE, Finalize, RC=STATUS )
    VERIFY_(STATUS)

! Get my internal MAPL_Generic state
!-----------------------------------

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
    VERIFY_(STATUS)

! get vm
    call ESMF_VMGetCurrent(vm, rc=status)
    VERIFY_(STATUS)
    call ESMF_VmGet(VM, localPet=MYID, petCount=npes, rc=status)
    VERIFY_(STATUS)

! get the number of ensembles from the resource file
    call MAPL_GetResource(MAPL, NENS, LABEL="ENSEMBLE_SIZE:", default=1, RC=status)
    VERIFY_(STATUS)

    N_CHILDREN = NENS + 1

    allocate(GCM(NENS), stat=status)
    VERIFY_(STATUS)

    allocate(CHILD_NAME(N_CHILDREN), stat=status)
    VERIFY_(STATUS)

! try to be clever and distribute evenly the PETs

    allocate(jens(NENS), stat=status)
    VERIFY_(STATUS)
    call MAPL_DecomposeDim ( NPES,jens,NENS )

    allocate(petlist(NENS), stat=status)
    VERIFY_(STATUS)

    allocate(g2aRH(NENS,3), a2gRH(NENS,3), stat=status)
    VERIFY_(STATUS)
    g2aRH(:,:)%isNeeded = .false.
    a2gRH(:,:)%isNeeded = .false.

    do i = 1, nens
       allocate(petlist(i)%pet(JENS(i)), stat=status)
       VERIFY_(STATUS)
    end do

    petlist(1)%pet(1) = 0
    do i = 1, nens-1
       petlist(i+1)%pet(1) = petlist(i)%pet(1) + jens(i)
    end do

    do i = 1, nens
       do j = 2, jens(i)
          petlist(i)%pet(j) = petlist(i)%pet(1) + j - 1
       end do
    end do

    deallocate(jens)

! do the "regular" MAPL-style setservices

!   Create the children's gridded components
!   ----------------------------------------
    I0 = -1
    DO I = 1, N_CHILDREN
       if (I == N_CHILDREN) then
          CHILD_NAME(I) = 'ANA'
          ANA = MAPL_AddChild(GC, NAME='ANA', SS=ANASetServices, RC=STATUS)
          VERIFY_(STATUS)
       else
          WRITE(ENSNUM,'(I4.4)') I   !ALT: this limits number of ensemble members to 9999

          if(nens > 1) then; child_name(i) = 'ens' // trim(ensnum) // ':GCM'
          else; child_name(i) = 'GCM'
          endif;  

          GCM(i) = MAPL_AddChild(GC, NAME=CHILD_NAME(I), &
               SS=GCMSetServices, petList=petList(i)%pet, RC=STATUS)
          VERIFY_(STATUS)

! figure out which ensemble member I am
          DO J = 1, size(petlist(i)%pet)
             if (myid == petlist(i)%pet(j)) then
                I0 = I 
                exit
             end if
          END DO
       end if
    END DO
    ASSERT_(I0>0)
 

    DO I = 1, NENS
       deallocate(petList(I)%pet)
    END DO
    deallocate(petList)

! Set internal connections between the childrens IMPORTS and EXPORTS
! ------------------------------------------------------------------

     GCMEX = ['T', 'S', 'U', 'V', 'SLV', 'SSH', 'PBO', 'skin_T', 'skin_S', 'TX', 'TY', 'MLD', 'PSI', 'MOM_3D_MASK', 'Z', 'RHO', 'AICE', 'HICE', 'CHLOROPHYLL']
     ANAEX = ['Tinc', 'Sinc', 'Uinc', 'Vinc', 'SSHinc', 'PBOinc', 'skin_Tinc', 'skin_Sinc', 'TXinc', 'TYinc', 'AICEinc', 'HICEinc', 'CHLOROPHYLLinc']

     call MAPL_TerminateImport    ( GC,   &
          CHILD      = ANA,           &
          RC=STATUS  )
     VERIFY_(STATUS)

     DO I = 1, NENS
        call MAPL_TerminateImport    ( GC,   &
             CHILD = GCM(I),           &
             RC=STATUS  )
        VERIFY_(STATUS)
     END DO

    call MAPL_GenericSetServices  ( GC, RC=STATUS)
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)
  
  end subroutine SetServices

!=============================================================================

!BOP

! !IROUTINE: Initialize -- Initialize method for the GEOS ANA component

! !INTERFACE:

  subroutine Initialize ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
    type(ESMF_State),    intent(inout) :: IMPORT ! Import state
    type(ESMF_State),    intent(inout) :: EXPORT ! Export state
    type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
    integer, optional,   intent(  out) :: RC     ! Error code

! !DESCRIPTION: The Initialize method of the Composite Analysis Gridded Component.

!EOP

! ErrLog Variables

    character(len=ESMF_MAXSTR)              :: IAm 
    integer                                 :: STATUS
    character(len=ESMF_MAXSTR)              :: COMP_NAME
    
! Local derived type aliases

    type (MAPL_MetaComp), pointer       :: MAPL
    type(ESMF_Grid)                     :: anagrid
    type(ESMF_Grid)                     :: bgrid
    type (ESMF_GridComp),      pointer  :: GCS(:)
    type (ESMF_State),         pointer  :: GIM(:), GEX(:)
    type (ESMF_State)                   :: SIMP, SEXP

    type (ESMF_FieldBundle)             :: BUNDLE
    type (ESMF_Field)                   :: FIELD, BFIELD
    type (ESMF_Field)                   :: FIELD0
    type (ESMF_Array)                   :: ARRAY, BARRAY
    type (ESMF_VM)                      :: vm
    integer                             :: I, J
    integer                             :: myid, npes
    integer                             :: rank
    integer                             :: dims
    real, pointer                       :: PTR2D(:,:)
    real, pointer                       :: PTR3D(:,:,:)
    character(len=ESMF_MAXSTR)          :: string
    integer                             :: freq
    type(ESMF_TimeInterval)             :: tstep
    type(ESMF_TimeInterval)             :: Frequency
    type(ESMF_Time)                     :: currTime
    type(ESMF_Time)                     :: ringTime
    type(ESMF_Alarm)                    :: ALARM

!=============================================================================

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    Iam = "Initialize"
    call ESMF_GridCompGet ( GC, name=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // Iam

! Get my internal MAPL_Generic state
!-----------------------------------

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
    VERIFY_(STATUS)

! get vm
    call ESMF_VMGetCurrent(vm, rc=status)
    VERIFY_(STATUS)

! create alarm for coupling/redist
!=================================
    call MAPL_GetResource(MAPL, freq, Label="COUPLE_FREQUENCY:", &
         default=1, rc=status)
    VERIFY_(STATUS)
    call ESMF_ClockGet(clock, currTime=currTime, timestep=tstep, rc=status)
    VERIFY_(STATUS)

    Frequency = freq*tstep

    RingTime=currTime
    Alarm = ESMF_AlarmCreate(name = "COUPLE_"//trim(COMP_NAME), &
         clock=clock, RingTime=ringTime, RingInterval=Frequency, &
         sticky=.false., rc=status )
    VERIFY_(STATUS)
!   make sure this alrm is ringing on the first time step
    call ESMF_AlarmRingerOn(Alarm, rc=status)
    VERIFY_(STATUS)

! traditional Initialize

    call MAPL_Get ( MAPL, GCS=GCS, GIM = GIM, GEX = GEX, RC=STATUS )
    VERIFY_(STATUS)

! Create Ocean grid for this component
!-------------------------------------
    call MAPL_GridCreate(GC, rc=status)
    VERIFY_(STATUS)

! Recursive setup of grids (should be disabled)
    call ESMF_GridCompGet(GC,  grid=anagrid, rc=status)
    VERIFY_(STATUS)
    call ESMF_GridCompSet(GCS(ANA),  grid=anagrid, rc=status)
    VERIFY_(STATUS)

! Call Initialize for every Child
!--------------------------------

     call MAPL_GenericInitialize ( GC, IMPORT, EXPORT, CLOCK,  RC=STATUS)
     VERIFY_(STATUS)

     call ESMF_VMBarrier(vm, rc=status)
     VERIFY_(STATUS)

! the next section is a workaround an "alledged" bug in ESMF_StateReconcile
     SIMP = ESMF_StateCreate(name = "GCS_super_imports", rc=status)
     VERIFY_(STATUS)
     SEXP = ESMF_StateCreate(name = "GCS_super_exports", rc=status)
     VERIFY_(STATUS)

     allocate(IMP(NENS), stat=status)
     VERIFY_(STATUS)
     allocate(EXS(NENS), stat=status)
     VERIFY_(STATUS)
     DO I = 1, NENS
        IMP(I) = ESMF_StateCreate(name = trim(CHILD_NAME(I)) // '_Imports', stateintent = ESMF_STATEINTENT_IMPORT, rc = STATUS)
        VERIFY_(STATUS)
        EXS(I) = ESMF_StateCreate(name = trim(CHILD_NAME(I)) // '_Exports', stateintent = ESMF_STATEINTENT_EXPORT, rc = STATUS )
        VERIFY_(STATUS)

        if (I == I0) then
           DO J = 1, size(ANAEX)
              call ESMF_StateGet ( GIM(I), ANAEX(J), FIELD, RC=STATUS )
              VERIFY_(STATUS)
              call ESMF_StateAdd( IMP(I), [FIELD], RC=STATUS)
              VERIFY_(STATUS)
           END DO

           DO J = 1, size(GCMEX)
              call ESMF_StateGet ( GEX(I), GCMEX(J), FIELD, RC=STATUS )
              VERIFY_(STATUS)
              call ESMF_StateAdd( EXS(I), [FIELD], RC=STATUS)
              VERIFY_(STATUS)
           END DO

           call ESMF_StateAdd(SIMP, [IMP(I)], RC=status)
           VERIFY_(STATUS)
           call ESMF_StateAdd(SEXP, [EXS(I)], RC=status)
           VERIFY_(STATUS)
        end if
     END DO


     if(NENS > 1) then
         call date_and_time(time = string)  
         if(mapl_am_i_root()) write(unit = *, fmt = '(a,i4)') 'time: ' // string(1:2) // ':' // string(3:4) // ':' // string(5:6) // ' -- reconciling imports'
         call ESMF_StateReconcile(SIMP, vm = vm, attreconflag = ESMF_ATTRECONCILE_ON, rc = status)
         VERIFY_(STATUS)

         call date_and_time(time = string)  
         if(mapl_am_i_root()) write(unit = *, fmt = '(a,i4)') 'time: ' // string(1:2) // ':' // string(3:4) // ':' // string(5:6) // ' -- reconciling exports' 
         call ESMF_StateReconcile(SEXP, vm = vm, attreconflag = ESMF_ATTRECONCILE_ON, rc = status)
         VERIFY_(STATUS)
     endif    
     call date_and_time(time = string)  
     if(mapl_am_i_root()) write(unit = *, fmt = '(a,i4)') 'time: ' // string(1:2) // ':' // string(3:4) // ':' // string(5:6) 


     DO I = 1, NENS
        call ESMF_StateGet(SIMP, trim(CHILD_NAME(I)) // '_Imports', IMP(I), rc=status)
        VERIFY_(STATUS)
        call ESMF_StateGet(SEXP, trim(CHILD_NAME(I)) // '_Exports', EXS(I), rc=status)
        VERIFY_(STATUS)
     END DO

! Do the connections manually
!-----------------------------

     do I = 1, size(ANAEX)
        call ESMF_StateGet ( GEX(ANA), ANAEX(I), BUNDLE, RC=STATUS )
        VERIFY_(STATUS)
        call ESMF_StateGet ( GIM(GCM(I0)), ANAEX(I), FIELD0, RC=STATUS )
        VERIFY_(STATUS)
        call ESMF_AttributeGet(FIELD0, NAME='DIMS', VALUE=DIMS, RC=STATUS)
        VERIFY_(STATUS)

        DO J = 1, NENS
           call ESMF_StateGet ( IMP(J), ANAEX(I), FIELD, RC=STATUS )
           VERIFY_(STATUS)
           call ESMF_FieldGet(field, array=array, rc=status)
           VERIFY_(STATUS)

           bgrid = anagrid

           BFIELD = MAPL_FieldCreate(FIELD0, bgrid, RC=status) 
           VERIFY_(STATUS)
           call ESMF_FieldGet(bfield, array=barray, rc=status)
           VERIFY_(STATUS)
           ! we might need to consider name mangling 
           ! otherwise the bundle contains many fields with the SAME name
           call ESMF_FieldBundleAdd(BUNDLE, [BFIELD], RC=STATUS)
           VERIFY_(STATUS)

           if (.not. a2gRH(J,dims)%isNeeded) then
              a2gRH(J,dims)%isNeeded = .true.
              call ESMF_VMBarrier(vm, rc=status)
              VERIFY_(STATUS)
              call ESMF_ArrayRedistStore(srcArray=barray, dstArray=array, &
                   routehandle=a2gRH(J,dims)%rh, rc=status)
              VERIFY_(STATUS)
              if(mapl_am_i_root()) write(unit = *, fmt = '(a,i4)') 'success connecting import ' // trim(anaex(i)) // ' for ensemble member ', j  
           endif

! this may not be needed
!@           call ESMF_ArrayRedist(srcArray=barray, dstArray=array, &
!@                routehandle=a2gRH(J,I), rc=status)
!@           VERIFY_(STATUS)
        END DO

     end do

     do I = 1, size(GCMEX)
        call ESMF_StateGet ( GIM(ANA), GCMEX(I), BUNDLE, RC=STATUS )
        VERIFY_(STATUS)
        call ESMF_StateGet ( GEX(GCM(I0)), GCMEX(I), FIELD0, RC=STATUS )
        VERIFY_(STATUS)
        call ESMF_AttributeGet(FIELD0, NAME='DIMS', VALUE=DIMS, RC=STATUS)
        VERIFY_(STATUS)

        DO J = 1, NENS
           if(mapl_am_i_root()) write(unit = *, fmt = '(a,i4)') 'connecting export ' // trim(gcmex(i)) // ' for ensemble member ', j  
           call ESMF_StateGet ( EXS(J), GCMEX(I), FIELD, RC=STATUS )
           VERIFY_(STATUS)
           call ESMF_FieldGet(field, array=array, rc=status)
           VERIFY_(STATUS)
           call ESMF_ArrayGet(array, rank=rank, rc=status)
           VERIFY_(STATUS)

           if (rank == 3) then
              if (J == I0) then
                 ! ALT force allocation of deferred exports
                 call MAPL_GetPointer( GEX(GCM(J)), PTR3D, GCMEX(I), alloc=.true., rc=status )
                 VERIFY_(STATUS)
              end if
              bgrid = anagrid
           else if (rank == 2) then
              if (J == I0) then
                 call MAPL_GetPointer( GEX(GCM(J)), PTR2D, GCMEX(I), alloc=.true., rc=status )
                 VERIFY_(STATUS)
              end if
              bgrid = anagrid
           else
              RETURN_(ESMF_FAILURE)
           end if

           BFIELD = MAPL_FieldCreate(FIELD0, bgrid, RC=status) 
           VERIFY_(STATUS)
           call ESMF_FieldGet(bfield, array=barray, rc=status)
           VERIFY_(STATUS)
        
           call ESMF_FieldBundleAdd(BUNDLE, [BFIELD], RC=STATUS)
           VERIFY_(STATUS)

           call ESMF_VMBarrier(vm, rc=status)
           VERIFY_(STATUS)
           call date_and_time(time = string)  
           if(mapl_am_i_root()) write(unit = *, fmt = '(a,i4)') 'time: ' // string(1:2) // ':' // string(3:4) // ':' // string(5:6) // ' -- redist_store for ensemble member ', j
           if (.not. g2aRH(J,dims)%isNeeded) then
              g2aRH(J,dims)%isNeeded = .true.
              call ESMF_ArrayRedistStore(srcArray=array, dstArray=barray, &
                   routehandle=g2aRH(J,dims)%rh, rc=status)
              VERIFY_(STATUS)
           end if
           call date_and_time(time = string)  
           if(mapl_am_i_root()) write(unit = *, fmt = '(a,i4)') 'time: ' // string(1:2) // ':' // string(3:4) // ':' // string(5:6) // ' -- DONE redist_store for ensemble member ', j  

           call ESMF_VMBarrier(vm, rc=status)
           VERIFY_(STATUS)

! this may not be needed if we run GCM first
           call ESMF_ArrayRedist(srcArray=array, dstArray=barray, &
                routehandle=g2aRH(J,dims)%rh, rc=status)
           VERIFY_(STATUS)
        END DO
        
        
     end do

     RETURN_(ESMF_SUCCESS)

   end subroutine Initialize

   subroutine Run ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

     type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
     type(ESMF_State),    intent(inout) :: IMPORT ! Import state
     type(ESMF_State),    intent(inout) :: EXPORT ! Export state
     type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
     integer, optional,   intent(  out) :: RC     ! Error code

! !DESCRIPTION: The Run method of the Composite Analysis Gridded Component.

!EOP

! ErrLog Variables

     character(len=ESMF_MAXSTR)          :: IAm 
     integer                             :: STATUS
     character(len=ESMF_MAXSTR)          :: COMP_NAME
     
     integer                             :: I, J
     integer                             :: dims
     type (MAPL_MetaComp), pointer       :: MAPL
     type (ESMF_GridComp),      pointer  :: GCS(:)
     type (ESMF_State),         pointer  :: GIM(:), GEX(:)
     type (ESMF_FieldBundle)             :: BUNDLE
     type (ESMF_Field)                   :: FIELD, BFIELD
     type (ESMF_Array)                   :: ARRAY, BARRAY
     type (ESMF_Alarm)                   :: ALARM
     logical                             :: timeToCouple

     Iam = "Run"
     call ESMF_GridCompGet ( GC, name=COMP_NAME, RC=STATUS )
     VERIFY_(STATUS)
     Iam = trim(COMP_NAME) // Iam
     
! Get my MAPL_Generic state
!--------------------------

     call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
     VERIFY_(STATUS)
     

! Get children and their im/ex states from my generic state.
!----------------------------------------------------------

     call MAPL_Get ( MAPL, GCS=GCS, GIM=GIM, GEX=GEX, RC=STATUS )
     VERIFY_(STATUS)

     call ESMF_ClockGetAlarm(clock, alarmname="COUPLE_"//trim(COMP_NAME), &
          alarm=ALARM, rc=status)
     VERIFY_(STATUS)

     timeToCouple = ESMF_AlarmIsRinging( ALARM, rc=status)
     VERIFY_(STATUS)

!  run ANA
! --------

     if (timeToCouple) then
        call ESMF_AlarmRingerOff(ALARM, RC=STATUS)
        VERIFY_(STATUS)
        call ESMF_GridCompRun ( GCS(ANA), importState=GIM(ANA), exportState=GEX(ANA), clock=clock, userRc=status )
        VERIFY_(STATUS)

! run "couplers", i.e. redist
! ---------------------------
        do I = 1, size(ANAEX)
           call ESMF_StateGet ( GEX(ANA), ANAEX(I), BUNDLE, RC=STATUS )
           VERIFY_(STATUS)
           do J = 1, NENS
              call ESMF_StateGet ( IMP(GCM(J)), ANAEX(I), FIELD, RC=STATUS )
              VERIFY_(STATUS)
              call ESMF_AttributeGet(FIELD, NAME='DIMS', VALUE=DIMS, RC=STATUS)
              VERIFY_(STATUS)
              call ESMF_FieldGet(field, array=array, rc=status)
              VERIFY_(STATUS)
              call ESMF_FieldBundleGet(BUNDLE, J, BFIELD,  RC=STATUS)
              VERIFY_(STATUS)
              call ESMF_FieldGet(bfield, array=barray, rc=status)
              VERIFY_(STATUS)

              call ESMF_ArrayRedist(srcArray=barray, dstArray=array, &
                   routehandle=a2gRH(J,dims)%rh, rc=status)
              VERIFY_(STATUS)
           end do
        end do
     end if

! run GCMs
! --------

     DO I = 1, NENS
        call ESMF_GridCompRun ( GCS(GCM(I)), importState=GIM(GCM(I)), exportState=GEX(GCM(I)), clock=clock, userRc=status )
        VERIFY_(STATUS)
     END DO

! run "couplers", i.e. redist
! ---------------------------
     if (timeToCouple) then
        do I = 1, size(GCMEX)
           call ESMF_StateGet ( GIM(ANA), GCMEX(I), BUNDLE, RC=STATUS )
           VERIFY_(STATUS)
           call WRITE_PARALLEL("... doing redist for "//trim(GCMEX(I)))
           do J = 1, NENS
              call ESMF_StateGet ( EXS(GCM(J)), GCMEX(I), FIELD, RC=STATUS )
              VERIFY_(STATUS)
              call ESMF_AttributeGet(FIELD, NAME='DIMS', VALUE=DIMS, RC=STATUS)
              VERIFY_(STATUS)
              call ESMF_FieldGet(field, array=array, rc=status)
              VERIFY_(STATUS)
        
              call ESMF_FieldBundleGet(BUNDLE, J, BFIELD,  RC=STATUS)
              VERIFY_(STATUS)
              call ESMF_FieldGet(bfield, array=barray, rc=status)
              VERIFY_(STATUS)
              call ESMF_ArrayRedist(srcArray=array, dstArray=barray, &
                   routehandle=g2aRH(J,dims)%rh, &
                   rc=status)
              VERIFY_(STATUS)
           end do
        end do
     end if

     RETURN_(ESMF_SUCCESS)

   end subroutine Run

   subroutine Finalize ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

     type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
     type(ESMF_State),    intent(inout) :: IMPORT ! Import state
     type(ESMF_State),    intent(inout) :: EXPORT ! Export state
     type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
     integer, optional,   intent(  out) :: RC     ! Error code
     
! !DESCRIPTION: The Finalize method of the Composite Analysis Gridded Component.

!EOP

! ErrLog Variables

     character(len=ESMF_MAXSTR)          :: IAm 
     integer                             :: STATUS
     character(len=ESMF_MAXSTR)          :: COMP_NAME
     
     integer                             :: I,J
     
     Iam = "Finalize"
     call ESMF_GridCompGet ( GC, name=COMP_NAME, RC=STATUS )
     VERIFY_(STATUS)
     Iam = trim(COMP_NAME) // Iam

     DO J = 1, NENS
        DO I=1,3 ! covers MAPL_DimsVertOnly, MAPL_DimsHorzOnly, MAPL_DimsHorzVert

           if (a2gRH(J,I)%isNeeded) then
              call ESMF_ArrayRedistRelease(a2gRH(J,I)%rh, rc=status)
              VERIFY_(STATUS)
           end if

           if (g2aRH(J,I)%isNeeded) then
              call ESMF_ArrayRedistRelease(g2aRH(J,I)%rh, rc=status)
              VERIFY_(STATUS)
           end if
        END DO
     END DO
     deallocate(a2gRH, g2aRH)
     deallocate(GCM)
     deallocate(EXS)
     
     call MAPL_GenericFinalize( GC, IMPORT, EXPORT, CLOCK, RC=STATUS )
     VERIFY_(STATUS)

     RETURN_(ESMF_SUCCESS)
   end subroutine Finalize

#endif
end module GEOS_GcsGridCompMod


