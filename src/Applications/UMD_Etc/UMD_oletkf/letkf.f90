PROGRAM letkf
!==============================================================================
!
! [PURPOSE:] Main program of LETKF
!
!===============================================================================
! MODULE: letkf_local
! 
! USES:
!  use common
!  use common_mpi
!  use common_mom4
!  use common_mpi_mom4
!  use common_letkf
!  use letkf_obs
!  use letkf_tools
!  use params_letkf
!  use params_model
!  use params_obs
!
! DESCRIPTION: 
!   This is the main program for the letkf data assimilation.
!
! REVISION HISTORY:
!   01/16/2009 Takemasa Miyoshi created for atmospheric analysis
!   04/26/2011 Steve Penny converted to OCEAN for use with mom4
!   03/18/2014 Steve Penny adapted to use on Gaea at NCEP/GFDL
!   07/08/2015 uncomment all the drifters subroutine
!
!-------------------------------------------------------------------------------
! $Authors: Steve Penny, Takemasa Miyoshi $
!===============================================================================

  USE common
  USE common_mpi
  USE common_mom4
  USE common_mpi_mom4
  USE common_letkf
  USE letkf_obs
  USE letkf_tools
  USE params_letkf
  USE params_model
  USE params_obs
  USE MemUtilsMod

! USE letkf_drifters_tools !LUYU: add the drifters tool

  IMPLICIT NONE
  REAL(r_size),ALLOCATABLE :: gues3d(:,:,:,:)
  REAL(r_size),ALLOCATABLE :: gues2d(:,:,:)
  REAL(r_size),ALLOCATABLE :: anal3d(:,:,:,:)
  REAL(r_size),ALLOCATABLE :: anal2d(:,:,:)
  REAL(r_size),ALLOCATABLE :: gues4d(:,:,:,:,:)
  REAL(r_size),ALLOCATABLE :: anal4d(:,:,:,:,:)
  REAL(r_size) :: rtimer00,rtimer
  INTEGER :: ierr
  CHARACTER(9) :: stdoutf='NOUT-0000'
  CHARACTER(4) :: guesf='bkg/'
  
  INTEGER :: ij, m !STEVE: for debugging
  LOGICAL :: ex
  INTEGER :: fid=21
  LOGICAL :: dortout=.true.    ! Force 'realtime' output (helps with parallel debugging)
  LOGICAL :: dodebug0=.false.  ! Debug flag for various routines

  logical :: ACTIVE_Tprof, ACTIVE_Sprof, ACTIVE_ADT, ACTIVE_SST, ACTIVE_SSS, ACTIVE_AICE, ACTIVE_HICE
  type(ObsStatus) :: obs_status


  NAMELIST /params_model_nml/ gridfile, SSHclm_file
  NAMELIST /params_obs_nml/   nslots,nbslot,sigma_obs,sigma_obs0,sigma_obsv,sigma_obst,gross_error
  NAMELIST /params_letkf_nml/ DO_DRIFTERS, DO_ALTIMETRY, DO_NO_VERT_LOC, localization_method, cov_infl_mul,sp_infl_add,DO_INFL_RESET
  namelist /set_active_obs_nml/ ACTIVE_Tprof, ACTIVE_Sprof, ACTIVE_ADT, ACTIVE_SST, ACTIVE_SSS, ACTIVE_AICE, ACTIVE_HICE

!------------------------------------------------------------------------------
! Initial settings
!------------------------------------------------------------------------------
  CALL CPU_TIME(rtimer00)
  CALL initialize_mpi

  WRITE(stdoutf(6:9), '(I4.4)') myrank
  WRITE(6,'(3A,I4.4)') 'STDOUT goes to ',stdoutf,' for MYRANK ', myrank
  !STEVE: if it halts here, it probably means the nlon, nlat and nlev in common_mom4
  !       have not been set properly for this model's grid
  OPEN(6,FILE=stdoutf)
  WRITE(6,'(A,I4.4,2A)') 'MYRANK=',myrank,', STDOUTF=',stdoutf

  !----------------------------------------------------------------------------
  ! Read in namelist parameters
  !----------------------------------------------------------------------------
  INQUIRE(FILE="input.nml", EXIST=ex)
  if (ex) then
    open(fid,file="input.nml", status='OLD') !, delim='APOSTROPHE')
    read(fid,nml=params_model_nml)
    read(fid,nml=params_obs_nml)
    read(fid,nml=params_letkf_nml)
    read(fid,nml=set_active_obs_nml)
  endif
  
  WRITE(6,*) "================================================================="
  WRITE(6,*) "Namelist inputs:"
  WRITE(6,*) "================================================================="
  write(6,params_model_nml)
  write(6,params_obs_nml)
  write(6,params_letkf_nml)
  write(6,set_active_obs_nml)
  WRITE(6,*) "================================================================="
  obs_status%Tprof = ACTIVE_Tprof
  obs_status%Sprof = ACTIVE_Sprof
  obs_status%ADT = ACTIVE_ADT
  obs_status%SST = ACTIVE_SST
  obs_status%SSS = ACTIVE_SSS
  obs_status%AICE =  ACTIVE_AICE
  obs_status%HICE = ACTIVE_HICE

  !----------------------------------------------------------------------------
  ! Print header
  !----------------------------------------------------------------------------
  WRITE(6,'(A)') '================================================='
  WRITE(6,'(A)') '  THE LOCAL ENSEMBLE TRANSFORM KALMAN FILTER     '
  WRITE(6,'(A)') '                                                 '
  WRITE(6,'(A)') '   LL      EEEEEE  TTTTTT  KK  KK  FFFFFF        '
  WRITE(6,'(A)') '   LL      EE        TT    KK KK   FF            '
  WRITE(6,'(A)') '   LL      EEEEE     TT    KKK     FFFFF         '
  WRITE(6,'(A)') '   LL      EE        TT    KK KK   FF            '
  WRITE(6,'(A)') '   LLLLLL  EEEEEE    TT    KK  KK  FF            '
  WRITE(6,'(A)') '                                                 '
  WRITE(6,'(A)') '  Developed for NCEP use by Steve Penny (2014)   '
  WRITE(6,'(A)') '  Developed for the OCEAN by Steve Penny (2011)  '
  WRITE(6,'(A)') '                                                 '
  WRITE(6,'(A)') '  Based on original code by T. Miyoshi           '
  WRITE(6,'(A)') '  for the SPEEDY Atmospheric GCM                 '
  WRITE(6,'(A)') '  and algorithms by Ott (2004) and Hunt (2007)   '
  WRITE(6,'(A)') '================================================='
  WRITE(6,'(A)') '              LETKF PARAMETERS                   '
  WRITE(6,'(A)') ' ----------------------------------------------- '
  WRITE(6,'(A,I15)')   '   nbv        :',nbv
  WRITE(6,'(A,I15)')   '   nslots     :',nslots
  WRITE(6,'(A,I15)')   '   nbslot     :',nbslot
  WRITE(6,'(A,F15.2)') '   sigma_obs  :',sigma_obs
  WRITE(6,'(A,F15.2)') '   sigma_obs0 :',sigma_obs0
  WRITE(6,'(A,F15.2)') '   sigma_obsv :',sigma_obsv
  WRITE(6,'(A,F15.2)') '   sigma_obst :',sigma_obst
  WRITE(6,'(A,L5)') '   Active T profile:',ACTIVE_Tprof
  WRITE(6,'(A,L5)') '   Active S profile:',ACTIVE_Sprof
  WRITE(6,'(A,L5)') '   Active ADT:', ACTIVE_ADT
  WRITE(6,'(A,L5)') '   Active SST:', ACTIVE_SST
  WRITE(6,'(A,L5)') '   Active SSS:', ACTIVE_SSS
  WRITE(6,'(A,L5)') '   Active AICE:', ACTIVE_AICE
  WRITE(6,'(A,L5)') '   Active HICE:', ACTIVE_HICE
  WRITE(6,'(A)') '================================================='

  !-----------------------------------------------------------------------------
  ! Initialize modules
  !-----------------------------------------------------------------------------
  CALL set_common_mom4
  CALL set_common_mpi_mom4
! if (DO_DRIFTERS) then
!   CALL set_common_drifters
! endif

  !-----------------------------------------------------------------------------
  ! Allocate dynamic arrays
  !-----------------------------------------------------------------------------
  ALLOCATE(gues3d(nij1,nlev,nbv,nv3d))
  ALLOCATE(gues2d(nij1,nbv,nv2d))
  ALLOCATE(anal3d(nij1,nlev,nbv,nv3d))
  ALLOCATE(anal2d(nij1,nbv,nv2d))

  !-----------------------------------------------------------------------------
  ! Check timer for initialization
  !-----------------------------------------------------------------------------
  CALL CPU_TIME(rtimer)
  WRITE(6,'(A,2F10.2)') '### TIMER(INITIALIZE):',rtimer,rtimer-rtimer00
  if (dortout) then !STEVE: force output to file
    CLOSE(6)
    OPEN(6,FILE=stdoutf,POSITION='APPEND',STATUS = 'OLD')
  endif
  rtimer00=rtimer

  !-----------------------------------------------------------------------------
  ! Observations
  !-----------------------------------------------------------------------------
  call MemReport(MPI_COMM_WORLD,"before set_letkf_obs")
  CALL set_letkf_obs(obs_status)
  call MemReport(MPI_COMM_WORLD,"After set_letkf_obs")
 
  !STEVE: calls read_grd, then read_ens_mpi calls read_grd4 below. This may be an inefficiency
  !STEVE: Perhaps call once here then output v3d and v2d for use as gues3d and gues2d below.

  !-----------------------------------------------------------------------------
  ! Check timer for initializing observations
  !-----------------------------------------------------------------------------
  CALL CPU_TIME(rtimer)
  WRITE(6,'(A,2F10.2)') '### TIMER(READ_OBS):',rtimer,rtimer-rtimer00
  if (dortout) then !STEVE: force output to file
    CLOSE(6)
    OPEN(6,FILE=stdoutf,POSITION='APPEND',STATUS = 'OLD')
  endif
  rtimer00=rtimer

  !-----------------------------------------------------------------------------
  ! Read forecast ensemble
  !-----------------------------------------------------------------------------
  call MemReport(MPI_COMM_WORLD,"before MPI_BARRIER")
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  call MemReport(MPI_COMM_WORLD,"after MPI_BARRIER")
  !WRITE(guesf(3:4),'(I2.2)') nbslot 
  !WRITE(guesf(11:12),'(I2.2)') nbslot  ! CDA:nbslot not used in the prefix. bkg/001
  WRITE(6,*) "From letkf.f90, calling read_ens_mpi..."
  print *,'guesf:',guesf
  CALL read_ens_mpi(guesf,nbv,gues3d,gues2d)
  WRITE(6,*) "From letkf.f90, finished calling read_ens_mpi..."


  !if (dodebug0) then ! Test processing of forecast ensemble, write, and quit
  !  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  !  WRITE(6,*) "From letkf.f90, calling write_ens_mpi..."
  !  CALL write_ens_mpi('anal',nbv,gues3d,gues2d)
  !  WRITE(6,*) "From letkf.f90, finished calling write_ens_mpi..."
  !  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  !  STOP 4
  !endif

  !-----------------------------------------------------------------------------
  ! Check timer for reading forecast ensemble
  !-----------------------------------------------------------------------------
  CALL CPU_TIME(rtimer)
  WRITE(6,'(A,2F10.2)') '### TIMER(READ_ENS_MPI):',rtimer,rtimer-rtimer00
  if (dortout) then !STEVE: force output to file
    CLOSE(6)
    OPEN(6,FILE=stdoutf,POSITION='APPEND',STATUS = 'OLD')
  endif
  rtimer00=rtimer

  !-----------------------------------------------------------------------------
  ! Write ensemble mean and spread
  !-----------------------------------------------------------------------------
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  CALL write_ensmspr_mpi('gues',nbv,gues3d,gues2d)
! if (DO_DRIFTERS) then
!  CALL write_ensmspr_drifters('gues',gues4d) !Original: CALL write_ensmspr_drifters('gues',nbv,gues4d)
! endif

  !STEVE: debug
  if (dodebug0) CALL write_ens_mpi_grd('test',1,gues3d,gues2d)
  
  !-----------------------------------------------------------------------------
  ! Check timer for writing forecast ensemble
  !-----------------------------------------------------------------------------
  CALL CPU_TIME(rtimer)
  WRITE(6,'(A,2F10.2)') '### TIMER(WRITE_GUES):',rtimer,rtimer-rtimer00
  if (dortout) then !STEVE: force output to file
    CLOSE(6)
    OPEN(6,FILE=stdoutf,POSITION='APPEND',STATUS = 'OLD')
  endif
  rtimer00=rtimer

  !------------------------------------------------------------------------------
  ! Data Assimilation (MAIN)
  !------------------------------------------------------------------------------
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  CALL das_letkf(gues3d,gues2d,anal3d,anal2d)

  !(DRIFTERS)
  ! Update drifter position by shifting entire modeled trajectories to the new observed
  ! position at the observed time.
  ! This can be done independently because each drifter id's xyz coordinates are considered
  ! part of the model state vector, appended to the model grid coordinates.
  ! Using LETKF's grid-based localization strategy, there is no difference.
! if (DO_DRIFTERS) then
!   CALL das_drifters(gues4d,anal4d)
! endif

  !-----------------------------------------------------------------------------
  ! Check timer for computing letkf analysis
  !-----------------------------------------------------------------------------
  CALL CPU_TIME(rtimer)
  WRITE(6,'(A,2F10.2)') '### TIMER(DAS_LETKF):',rtimer,rtimer-rtimer00
  if (dortout) then !STEVE: force output to file
    CLOSE(6)
    OPEN(6,FILE=stdoutf,POSITION='APPEND',STATUS = 'OLD')
  endif
  rtimer00=rtimer

  !--
  ! Could call 3DVar here for hybrid...
  !--

  !----------------------------------------------------------------------------
  ! Write analysis ensemble
  !----------------------------------------------------------------------------
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  CALL write_ens_mpi('ana/',nbv,anal3d,anal2d)
! if (DO_DRIFTERS) then
!  CALL write_ens_drifters('anal',anal4d)
! endif

  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  CALL write_ensmspr_mpi('anal',nbv,anal3d,anal2d)
! if (DO_DRIFTERS) then
!  CALL write_ensmspr_drifters('anal',anal4d)
! endif

  !-----------------------------------------------------------------------------
  ! Check timer for writing analysis ensemble, mean, and spread
  !-----------------------------------------------------------------------------
  CALL CPU_TIME(rtimer)
  WRITE(6,'(A,2F10.2)') '### TIMER(WRITE_ANAL):',rtimer,rtimer-rtimer00
  if (dortout) then !STEVE: force output to file
    CLOSE(6)
    OPEN(6,FILE=stdoutf,POSITION='APPEND',STATUS = 'OLD')
  endif
  rtimer00=rtimer

  !-----------------------------------------------------------------------------
  ! Check timer for total runtime
  !-----------------------------------------------------------------------------
  CALL CPU_TIME(rtimer)
  WRITE(6,'(A,2F10.2)') '### TIMER(FINAL):',rtimer,rtimer-rtimer00
  if (dortout) then !STEVE: force output to file
    CLOSE(6)
    OPEN(6,FILE=stdoutf,POSITION='APPEND',STATUS = 'OLD')
  endif
  rtimer00=rtimer


  !----------------------------------------------------------------------------
  ! Finalize the MPI
  !----------------------------------------------------------------------------
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  CALL finalize_mpi

END PROGRAM letkf
