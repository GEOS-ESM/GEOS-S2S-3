MODULE letkf_obs
!===============================================================================
! MODULE: letkf_obs
! 
! USES:
!   use common
!   use common_mpi
!   use common_mom4
!   use common_obs_mom4
!   use common_mpi_mom4
!   use common_letkf
!   use letkf_drifters !(DRIFTERS)
!
! !PUBLIC TYPES:
!                 implicit none
!                 [save]
!
!                 <type declaration>
!     
! !PUBLIC MEMBER FUNCTIONS:
!           <function>                     ! Description      
!
! !PUBLIC DATA MEMBERS:
!           <type> :: <variable>           ! Variable description
!
! DESCRIPTION: 
!   This module reads all observation data and stores in appropriate data structures
!
! !REVISION HISTORY:
!   04/26/2011 Steve PENNY converted to OCEAN for use with MOM4
!   01/23/2009 Takemasa MIYOSHI  created
! 
!-------------------------------------------------------------------------------
! $Author: yvikhlia $
!===============================================================================
  USE common
  USE common_mpi
  USE common_mom4
  USE common_obs_mom4
  USE common_mpi_mom4
  USE common_letkf
  USE params_obs
  USE vars_obs
  USE MemUtilsMod  ! CDA_DEBUG

  !(DRIFTERS)
! USE letkf_drifters

  IMPLICIT NONE
  PUBLIC

  !--
  !STEVE: for adaptive obs error:
  LOGICAL :: oerfile_exists

  !-----------------------------------------------------------------------------
  ! For debugging
  !-----------------------------------------------------------------------------
  LOGICAL :: debug_hdxf_0 = .true.   !This error occured because there was not a model representation of the observed value (i.e. SST obs with no SST model field)
                                     ! Solution was to populate a SST model field (v2d) with surface temp data from the model (v3d(:,:,1))
  INTEGER :: cnt_obs_u, cnt_obs_v, cnt_obs_t, cnt_obs_s, cnt_obs_x, cnt_obs_y, cnt_obs_z, cnt_obs_ssh, cnt_obs_eta, cnt_obs_sst, cnt_obs_sss
  integer :: cnt_obs_aice, cnt_obs_hice
  !CDA_START
  !orignal line !INTEGER, DIMENSION(nv3d+nv2d), SAVE :: cnt_obs = 0
  INTEGER, DIMENSION(10), SAVE :: cnt_obs = 0 !temp fix by CDA
  !CDA_END

  !STEVE: for debugging observation culling:
  INTEGER :: cnt_yout=0, cnt_xout=0, cnt_zout=0, cnt_triout=0
  INTEGER :: cnt_rigtnlon=0, cnt_nearland=0

CONTAINS

  SUBROUTINE set_letkf_obs(obs_status)
    !===============================================================================
    ! Initialize the module
    !===============================================================================
    IMPLICIT NONE
    REAL(r_size) :: dz,tg,qg
    REAL(r_size) :: ri,rj,rk
    REAL(r_size) :: dlon1,dlon2,dlon,dlat
    REAL(r_size),ALLOCATABLE :: wk2d(:,:)
    INTEGER,ALLOCATABLE      :: iwk2d(:,:)
    REAL(r_size),ALLOCATABLE :: tmpelm(:)
    REAL(r_size),ALLOCATABLE :: tmplon(:)
    REAL(r_size),ALLOCATABLE :: tmplat(:)
    REAL(r_size),ALLOCATABLE :: tmplev(:)
    REAL(r_size),ALLOCATABLE :: tmpdat(:)
    REAL(r_size),ALLOCATABLE :: tmperr(:)
    REAL(r_size),ALLOCATABLE :: tmpinstid(:)
    REAL(r_size),ALLOCATABLE :: tmpi(:)
    REAL(r_size),ALLOCATABLE :: tmpj(:)
    REAL(r_size),ALLOCATABLE :: tmpk(:)
    REAL(r_size),ALLOCATABLE :: tmpdep(:)
    REAL(r_size),ALLOCATABLE :: tmphdxf(:,:)
    REAL(r_size),ALLOCATABLE :: tmpid(:)   !(DRIFTERS)
    REAL(r_size),ALLOCATABLE :: tmptime(:) !(DRIFTERS)
    INTEGER,ALLOCATABLE :: tmpqc0(:,:)
    INTEGER,ALLOCATABLE :: tmpqc(:)
    REAL(r_size),ALLOCATABLE :: tmp2elm(:)
    REAL(r_size),ALLOCATABLE :: tmp2lon(:)
    REAL(r_size),ALLOCATABLE :: tmp2lat(:)
    REAL(r_size),ALLOCATABLE :: tmp2lev(:)
    REAL(r_size),ALLOCATABLE :: tmp2dat(:)
    REAL(r_size),ALLOCATABLE :: tmp2err(:)
    REAL(r_size),ALLOCATABLE :: tmp2dep(:)
    REAL(r_size),ALLOCATABLE :: tmp2instid(:)
    REAL(r_size),ALLOCATABLE :: tmp2hdxf(:,:)
    REAL(r_size),ALLOCATABLE :: tmp2id(:)   !(DRIFTERS)
    REAL(r_size),ALLOCATABLE :: tmp2time(:) !(DRIFTERS)
    INTEGER :: nobslots(nslots)
    INTEGER :: n,i,j,ierr,islot,nn,l,im, ii
    INTEGER :: nj(0:nlat-1)
    INTEGER :: njs(1:nlat-1)
    CHARACTER(12) :: obsfile='obsTTNNN.dat'

    !STEVE: for adaptive observation error:
    REAL(r_size) :: tmpoerr 
    !STEVE: to adjust writing to output file
    LOGICAL :: verbose = .false.
    LOGICAL :: dodebug = .true.
    !STEVE: for obs qc:
    REAL(r_size) :: hdx2,mstd
    INTEGER :: gross_cnt,gross_2x_cnt
    !STEVE: for DO_ALTIMETRY
    REAL(r_size) :: SSH_CLM_m 
    real(r_size) :: offset, cnt_adt   ! ALTIMETER OFFSET

    type(ObsStatus), intent(in) :: obs_status

    call MemReport(MPI_COMM_WORLD,"into main in set_letkf_obs") ! CDA_DEBUG

    WRITE(6,'(A)') 'Hello from set_letkf_obs'

    dist_zero = sigma_obs * SQRT(10.0d0/3.0d0) * 2.0d0
    dist_zerov = sigma_obsv * SQRT(10.0d0/3.0d0) * 2.0d0
    dlat_zero = dist_zero / pi / re * 180.0d0

    ALLOCATE(dlon_zero(nij1))
    do i=1,nij1
       dlon_zero(i) = dlat_zero / COS(pi*lat1(i)/180.0d0)
    enddo

    if (myrank == 0) then !Assuming all members have the identical obs records
       do islot=1,nslots
          im = myrank+1
          WRITE(obsfile(4:8),'(I2.2,I3.3)') islot,im
          WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is reading an obs2-formatted file ',obsfile
          CALL get_nobs(obsfile,obs2nrec,nobslots(islot))
       enddo
    endif
    call MemReport(MPI_COMM_WORLD,"line 148 in set_letkf_obs") ! CDA_DEBUG

    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    call MemReport(MPI_COMM_WORLD,"line 151 in set_letkf_obs") ! CDA_DEBUG
    CALL MPI_BCAST(nobslots,nslots,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
    call MemReport(MPI_COMM_WORLD,"line 153 in set_letkf_obs") ! CDA_DEBUG
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    call MemReport(MPI_COMM_WORLD,"line 155 in set_letkf_obs") ! CDA_DEBUG

    nobs = SUM(nobslots)
    WRITE(6,'(I10,A)') nobs,' TOTAL OBSERVATIONS INPUT'

    if (nobs == 0) then
       WRITE(6,'(A)') 'No observation assimilated'
       RETURN
    endif

    !-----------------------------------------------------------------------------
    ! INITIALIZE GLOBAL VARIABLES
    !-----------------------------------------------------------------------------
    ALLOCATE( tmpelm(nobs) )
    ALLOCATE( tmplon(nobs) )
    ALLOCATE( tmplat(nobs) )
    ALLOCATE( tmplev(nobs) )
    ALLOCATE( tmpdat(nobs) )
    ALLOCATE( tmperr(nobs) )
    ALLOCATE( tmpinstid(nobs) )
    ALLOCATE( tmpk(nobs) )
    ALLOCATE( tmpdep(nobs) )
    ALLOCATE( tmphdxf(nobs,nbv) )
    ALLOCATE( tmpqc0(nobs,nbv) )
    ALLOCATE( tmpqc(nobs) )
    ALLOCATE( tmpid(nobs) )   !(DRIFTERS)
    ALLOCATE( tmptime(nobs) ) !(DRIFTERS)
    tmpqc0 = 0
    tmphdxf = 0.0d0
    tmperr = 0.0d0
    call MemReport(MPI_COMM_WORLD,"line 185 in set_letkf_obs") ! CDA_DEBUG

    !-----------------------------------------------------------------------------
    ! LOOP of timeslots
    !-----------------------------------------------------------------------------
    nn=0
    timeslots0: do islot=1,nslots
       if (nobslots(islot) == 0) CYCLE
       l=0
       do
          im = myrank+1 + nprocs * l
          if (im > nbv) EXIT
          WRITE(obsfile(4:8),'(I2.2,I3.3)') islot,im
          WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is reading a file ',obsfile
          CALL read_obs2(obsfile,nobslots(islot),&
               & tmpelm(nn+1:nn+nobslots(islot)),tmplon(nn+1:nn+nobslots(islot)),&
               & tmplat(nn+1:nn+nobslots(islot)),tmplev(nn+1:nn+nobslots(islot)),&
               & tmpdat(nn+1:nn+nobslots(islot)),tmperr(nn+1:nn+nobslots(islot)),&
               & tmpinstid(nn+1:nn+nobslots(islot)),&
               & tmphdxf(nn+1:nn+nobslots(islot),im),tmpqc0(nn+1:nn+nobslots(islot),im),&
               & tmptime(nn+1:nn+nobslots(islot)) )
          l = l+1
       enddo
       nn = nn + nobslots(islot)
    enddo timeslots0
    call MemReport(MPI_COMM_WORLD,"line 210 in set_letkf_obs") ! CDA_DEBUG


    WRITE(6,*) "Commencing collecting obs on all procs..."
    !STEVE: broadcast the 1d arrays from root onto all procs
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    WRITE(6,*) "Calling MPI_BCAST's..."
    CALL MPI_BCAST( tmpelm, nobs, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD,ierr)
    !STEVE: just to be safe, calling MPI_BARRIER after each MPI_BCAST
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    CALL MPI_BCAST( tmplon, nobs, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD,ierr)
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    CALL MPI_BCAST( tmplat, nobs, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD,ierr)
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    CALL MPI_BCAST( tmplev, nobs, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD,ierr)
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    CALL MPI_BCAST( tmpdat, nobs, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD,ierr)
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    CALL MPI_BCAST( tmperr, nobs, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD,ierr)
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    CALL MPI_BCAST( tmpinstid, nobs, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD,ierr)
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    call MemReport(MPI_COMM_WORLD,"line 232 in set_letkf_obs") ! CDA_DEBUG

    !STEVE: compile the tmphdxf array on all procs
    ALLOCATE(wk2d(nobs,nbv))
    wk2d = tmphdxf
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    WRITE(6,*) "Calling MPI_ALLREDUCE..."
    CALL MPI_ALLREDUCE(wk2d,tmphdxf,nobs*nbv,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    DEALLOCATE(wk2d)

    !STEVE: compile the tmpqc0 array on all procs
    ALLOCATE(iwk2d(nobs,nbv))
    iwk2d = tmpqc0
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    WRITE(6,*) "Calling MPI_ALLREDUCE..."
    CALL MPI_ALLREDUCE(iwk2d,tmpqc0,nobs*nbv,MPI_INTEGER,MPI_MAX,MPI_COMM_WORLD,ierr)
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    DEALLOCATE(iwk2d)
    WRITE(6,*) "Finished collecting obs on all procs."
    call MemReport(MPI_COMM_WORLD,"line 252 in set_letkf_obs") ! CDA_DEBUG
!!$
!!$    WRITE(6,*) "STEVE: DEBUGGING..."
!!$    WRITE(6,'(I10,A,I3.3)') nobs,' OBSERVATIONS, MYRANK = ',myrank
!!$    WRITE(6,*) "tmphdxf(1,:) = ", tmphdxf(1,:)
!!$    WRITE(6,*) "tmphdxf(2,:) = ", tmphdxf(2,:)
!!$    WRITE(6,*) "tmphdxf(3,:) = ", tmphdxf(3,:)
!!$    WRITE(6,*) "..."
!!$    WRITE(6,*) "tmphdxf(nobs,:) = ", tmphdxf(nobs,:)
!!$    WRITE(6,*)
!!$    n=1
!!$    WRITE(6,*) "For n=1,"
!!$    WRITE(6,*) "MINVAL(tmpqc0(n,:)) = ",MINVAL(tmpqc0(n,:))
!!$    WRITE(6,*) "tmpelm(n) = ", tmpelm(n) 
!!$    WRITE(6,*) "tmplon(n) = ", tmplon(n) 
!!$    WRITE(6,*) "tmplat(n) = ", tmplat(n) 
!!$    WRITE(6,*) "tmplev(n) = ", tmplev(n) 
!!$    WRITE(6,*) "tmpdat(n) = ", tmpdat(n) 
!!$    WRITE(6,*) "tmperr(n) = ", tmperr(n) 
!!$    WRITE(6,*)
!!$    n=nobs
!!$    WRITE(6,*) "For n=nobs=",nobs
!!$    WRITE(6,*) "MINVAL(tmpqc0(n,:)) = ",MINVAL(tmpqc0(n,:))
!!$    WRITE(6,*) "tmpelm(n) = ", tmpelm(n) 
!!$    WRITE(6,*) "tmplon(n) = ", tmplon(n) 
!!$    WRITE(6,*) "tmplat(n) = ", tmplat(n) 
!!$    WRITE(6,*) "tmplev(n) = ", tmplev(n) 
!!$    WRITE(6,*) "tmpdat(n) = ", tmpdat(n) 
!!$    WRITE(6,*) "tmperr(n) = ", tmperr(n) 
!!$    WRITE(6,*) "STEVE: END DEBUGGING."
!!$    WRITE(6,*)

    !STEVE: After processing ensemble members, apply some actions based on
    !forecast mean, to all observations
    cnt_obs_u = 0
    cnt_obs_v = 0
    cnt_obs_t = 0
    cnt_obs_s = 0
    cnt_obs_x = 0
    cnt_obs_y = 0
    cnt_obs_z = 0
    cnt_obs_ssh = 0
    cnt_obs_eta = 0
    cnt_obs_sst = 0
    cnt_obs_sss = 0
    cnt_obs_aice = 0
    cnt_obs_hice = 0
    gross_cnt = 0
    gross_2x_cnt = 0

    WRITE(6,*) "Processing tmphdxf for n=1 to n=nobs=",nobs 
    WRITE(6,*) "and filtering bad observations..."

    offset = 0.0
    cnt_adt = 0.0
    do n=1,nobs
       tmpqc(n) = MINVAL(tmpqc0(n,:))
       if (tmpqc(n) /= 1) CYCLE
       tmpdep(n) = tmphdxf(n,1) !note: tmpdep is just used as a dummy variable to compute the mean over the next few lines
       do i=2,nbv
          tmpdep(n) = tmpdep(n) + tmphdxf(n,i)
       enddo
       tmpdep(n) = tmpdep(n) / REAL(nbv,r_size)
       do i=1,nbv
          tmphdxf(n,i) = tmphdxf(n,i) - tmpdep(n) ! Hdxf (perturbations from mean)
       enddo
       ! Now, tmpdep is defined appropriately as the obs departure from mean background
       tmpdep(n) = tmpdat(n) - tmpdep(n) ! y-Hx

       select case(nint(tmpelm(n)))
       case(id_u_obs)               
          if (abs(tmpdep(n))>50.0) tmpqc(n) = 0

       case(id_t_obs)                       ! QC T for omf>10 degC
          if (abs(tmpdep(n))>10.0) tmpqc(n) = 0
          if (not(obs_status%Tprof)) tmpqc(n) = 0

       case(id_sst_obs)                     ! QC SST for omf>10 degC
          if (abs(tmpdep(n))>50.0) tmpqc(n) = 0
          if (not(obs_status%SST)) tmpqc(n) = 0             

       case(id_s_obs)                       !QC S for omf>10 psu
          if (abs(tmpdep(n))>10.0) tmpqc(n) = 0
          if (not(obs_status%Sprof)) tmpqc(n) = 0             

       case(id_sss_obs)                     ! QC SSS for omf>10 psu
          if (abs(tmpdep(n))>10.0) tmpqc(n) = 0
          if (not(obs_status%SSS)) tmpqc(n) = 0             

       case(id_eta_obs)                     ! QC eta for omf>1 m
          if (abs(tmpdep(n))>1.0) then
             tmpqc(n) = 0
          else
             offset=offset+tmpdep(n)
             cnt_adt=cnt_adt+1.0
          end if
          if (not(obs_status%ADT)) tmpqc(n) = 0 

       case(id_ssh_obs)                    ! QC eta for omf>1 m
          if (abs(tmpdep(n))>1.0) tmpqc(n) = 0

       case(id_aice_obs) 
          if (abs(tmpdep(n))>1.0) tmpqc(n) = 0
          !tmperr(n) = 0.02
          if (obs_status%AICE==.false.) tmpqc(n) = 0
          
       case(id_hice_obs)
          tmpqc(n) = 0
          !if (mod(n,10).eq.0) tmpqc(n) = 1   !poor man's subsampling
          if (abs(tmpdep(n))>5.0) tmpqc(n) = 0
          if (not(obs_status%HICE)) tmpqc(n) = 0             
          
       end select

       !STEVE: as a check, count the number of each type of observation
       if (tmpelm(n) .eq. id_u_obs) cnt_obs_u = cnt_obs_u + 1
       if (tmpelm(n) .eq. id_v_obs) cnt_obs_v = cnt_obs_v + 1
       if (tmpelm(n) .eq. id_t_obs) cnt_obs_t = cnt_obs_t + 1
       if (tmpelm(n) .eq. id_s_obs) cnt_obs_s = cnt_obs_s + 1
       if (tmpelm(n) .eq. id_ssh_obs) cnt_obs_ssh = cnt_obs_ssh + 1
       if (tmpelm(n) .eq. id_eta_obs) cnt_obs_eta = cnt_obs_eta + 1
       if (tmpelm(n) .eq. id_sst_obs) cnt_obs_sst = cnt_obs_sst + 1
       if (tmpelm(n) .eq. id_sss_obs) cnt_obs_sss = cnt_obs_sss + 1
       !(DRIFTERS)
       if (tmpelm(n) .eq. id_x_obs) cnt_obs_x = cnt_obs_x + 1
       if (tmpelm(n) .eq. id_y_obs) cnt_obs_y = cnt_obs_y + 1
       if (tmpelm(n) .eq. id_z_obs) cnt_obs_z = cnt_obs_z + 1
       !SEA-ICE
       if (tmpelm(n) .eq. id_aice_obs) cnt_obs_aice = cnt_obs_aice + 1
       if (tmpelm(n) .eq. id_hice_obs) cnt_obs_hice = cnt_obs_hice + 1

    enddo
    DEALLOCATE(tmpqc0)
    call MemReport(MPI_COMM_WORLD,"line 385 in set_letkf_obs") ! CDA_DEBUG

    ! Remove offset when assimilating altimetry
    if (cnt_adt>0.0) then
       offset=offset/cnt_adt
    else
       offset=0.0d0
    end if
    !where (tmpelm == id_eta_obs)
    !   tmpdep=tmpdep-offset
    !end where

    WRITE(6,*) "obs_status = ", obs_status

    WRITE(6,'(I10,A)') SUM(tmpqc),' OBSERVATIONS TO BE ASSIMILATED'
    !STEVE:
    WRITE(6,*) "cnt_obs_u = ", cnt_obs_u
    WRITE(6,*) "cnt_obs_v = ", cnt_obs_v
    WRITE(6,*) "cnt_obs_t = ", cnt_obs_t
    WRITE(6,*) "cnt_obs_s = ", cnt_obs_s
    WRITE(6,*) "cnt_obs_x = ", cnt_obs_x
    WRITE(6,*) "cnt_obs_y = ", cnt_obs_y
    WRITE(6,*) "cnt_obs_z = ", cnt_obs_z
    WRITE(6,*) "cnt_obs_ssh = ", cnt_obs_ssh
    WRITE(6,*) "cnt_obs_eta = ", cnt_obs_eta," with offset =",offset
    WRITE(6,*) "cnt_obs_sst = ", cnt_obs_sst
    WRITE(6,*) "cnt_obs_sss = ", cnt_obs_sss
    WRITE(6,*) "cnt_obs_aice = ", cnt_obs_aice
    WRITE(6,*) "cnt_obs_hice = ", cnt_obs_hice

    WRITE(6,*) "gross_cnt = ", gross_cnt
    WRITE(6,*) "gross_2x_cnt = ", gross_2x_cnt

    cnt_obs(iv3d_u) = cnt_obs_u
    cnt_obs(iv3d_v) = cnt_obs_v
    cnt_obs(iv3d_t) = cnt_obs_t
    cnt_obs(iv3d_s) = cnt_obs_s
    cnt_obs(nv3d+iv2d_ssh) = cnt_obs_ssh
    cnt_obs(nv3d+iv2d_eta) = cnt_obs_eta
    cnt_obs(nv3d+iv2d_sst) = cnt_obs_sst
    cnt_obs(nv3d+iv2d_sss) = cnt_obs_sss
    cnt_obs(nv3d+iv2d_aice) = cnt_obs_aice
    cnt_obs(nv3d+iv2d_hice) = cnt_obs_hice

    call MemReport(MPI_COMM_WORLD,"line 429 in set_letkf_obs") ! CDA_DEBUG
    CALL monit_dep(nobs,tmpelm,tmpdep,tmpqc)
    call MemReport(MPI_COMM_WORLD,"line 431 in set_letkf_obs") ! CDA_DEBUG

    !STEVE: maybe use this if there are enough observations...
    !
    ! temporal observation localization
    !
    ! If nbslot == 5, multiplier is approximately
    ! At islot = 1, ~ 1.04
    !          = 2, ~ 1.02
    !          = 3, ~ 1.01
    !          = 4, ~ 1.0025
    !          = 5, ~ 1.0
    !
    ! STEVE: watch out if using this for adaptive observation error, this scaling
    ! may cause problems if applied here...
    ! STEVE: commenting out because the observations are not having enough of an
    ! impact. It may be due to the increased error on the observations.
    ! PLUS, the temporal correlation scales are much longer than 5 days, so we can just ignore this

    !  nn = 0
    !  do islot=1,nslots
    !    if ( islot .ne. nbslot ) then
    !      tmperr(nn+1:nn+nobslots(islot)) = tmperr(nn+1:nn+nobslots(islot)) &
    !                                      & * exp(0.25d0 * (REAL(islot-nbslot,r_size) / sigma_obst)**2)
    !    endif
    !    nn = nn + nobslots(islot)
    !  enddo

    !
    ! SELECT OBS IN THE NODE
    !
    nn = 0
    !STEVE: first, remove all of the Quality-Controlled data
    do n=1,nobs
       if (tmpqc(n) /= 1) CYCLE
       !    if (tmplat(n) < MINVAL(lat1) .OR. MAXVAL(lat1) < tmplat(n)) then
       !      dlat = MIN( ABS(MINVAL(lat1)-tmplat(n)),ABS(MAXVAL(lat1)-tmplat(n)) )
       !      if (dlat > dlat_zero) CYCLE
       !    endif
       !    if (tmplon(n) < MINVAL(lon1) .OR. MAXVAL(lon1) < tmplon(n)) then
       !      dlon1 = ABS(MINVAL(lon1) - tmplon(n))
       !      dlon1 = MIN(dlon1,360.0d0-dlon1)
       !      dlon2 = ABS(MAXVAL(lon1) - tmplon(n))
       !      dlon2 = MIN(dlon2,360.0d0-dlon2)
       !      dlon =  MIN(dlon1,dlon2) &
       !         & * pi*re*COS(tmplat(n)*pi/180.d0)/180.0d0
       !      if (dlon > dist_zero) CYCLE
       !    endif
       nn = nn+1
       tmpelm(nn) = tmpelm(n)
       tmplon(nn) = tmplon(n)
       tmplat(nn) = tmplat(n)
       tmplev(nn) = tmplev(n)
       tmpdat(nn) = tmpdat(n)
       tmperr(nn) = tmperr(n)
       tmpinstid(nn) = tmpinstid(n)
       tmpk(nn) = tmpk(n)
       tmpdep(nn) = tmpdep(n)
       tmphdxf(nn,:) = tmphdxf(n,:)
       tmpqc(nn) = tmpqc(n)
       tmpid(nn) = tmpid(n)     !(DRIFTERS)
       tmptime(nn) = tmptime(n) !(DRIFTERS)
    enddo
    nobs = nn
    WRITE(6,'(I10,A,I3.3)') nobs,' OBSERVATIONS TO BE ASSIMILATED IN MYRANK ',myrank
    call MemReport(MPI_COMM_WORLD,"line 496 in set_letkf_obs") ! CDA_DEBUG
    if (myrank==0) call system("cat /proc/meminfo") ! CDA_DEBUG


    ALLOCATE( obselm(nobs) )
    ALLOCATE( obslon(nobs) )
    ALLOCATE( obslat(nobs) )
    ALLOCATE( obslev(nobs) )
    ALLOCATE( obsdat(nobs) )
    ALLOCATE( obserr(nobs) )
    ALLOCATE( obsinstid(nobs) )
    ALLOCATE( obsdep(nobs) )
    ALLOCATE( obshdxf(nobs,nbv) )
    ALLOCATE( obsid(nobs) )    !(DRIFTERS)
    ALLOCATE( obstime(nobs) )  !(DRIFTERS)

    ALLOCATE(nobsgrd(nlon,nlat)) !STEVE: added 07/09/15, changed nobsgrd to ALLOCATABLE

    call MemReport(MPI_COMM_WORLD,"line 523 in set_letkf_obs") ! CDA_DEBUG
    if (myrank==0) call system("cat /proc/meminfo") !CDA_DEBUG

    !
    ! ONLY SORT OBS IN THE ROOT PROCESS
    !
    if (myrank==0) then

        ALLOCATE( tmp2elm(nobs) )
        ALLOCATE( tmp2lon(nobs) )
        ALLOCATE( tmp2lat(nobs) )
        ALLOCATE( tmp2lev(nobs) )
        ALLOCATE( tmp2dat(nobs) )
        ALLOCATE( tmp2err(nobs) )
        ALLOCATE( tmp2dep(nobs) )
        ALLOCATE( tmp2instid(nobs) )
        !ALLOCATE( tmp2hdxf(nobs,nbv) )
        ALLOCATE( tmp2hdxf(nbv,nobs) )
        ALLOCATE( tmp2id(nobs) )    !(DRIFTERS)
        ALLOCATE( tmp2time(nobs) )  !(DRIFTERS)

        call MemReport(MPI_COMM_WORLD,"line 526 in set_letkf_obs") ! CDA_DEBUG
        nobsgrd = 0
        nj = 0
        ! Count the number of observations within each latitude range
        do j=1,nlat-1
           do n=1,nobs
              if (tmplat(n) < lat(j) .OR. lat(j+1) <= tmplat(n)) CYCLE
              nj(j) = nj(j) + 1
           enddo
        enddo
        call MemReport(MPI_COMM_WORLD,"line 536 in set_letkf_obs")
        ! Record cumulative sum of observations up to this latitude
        ! Creates the basis for an indexing of observations from lat to lat
        do j=1,nlat-1
           njs(j) = SUM(nj(0:j-1))
        enddo

        call MemReport(MPI_COMM_WORLD,"line 565 in set_letkf_obs_before") ! CDA_DEBUG

        ! Rearrange observations by latitude
        do j=1,nlat-1
           nn = 0
           do n=1,nobs
              if (tmplat(n) < lat(j) .OR. lat(j+1) <= tmplat(n)) CYCLE
              !     if (tmplon(n) >= lon(nlon)-EPSILON(1.0d0)) CYCLE   !STEVE: I added this to align with the same condition in the code above
              !       Otherwise, sometimes nn /= nj(j)
              nn = nn + 1
              !WRITE(6,*) "nn, njs(j)+nn, n, nobs=", nn, njs(j)+nn, n, nobs ! CDA: tmp
              tmp2elm(njs(j)+nn) = tmpelm(n)
              tmp2lon(njs(j)+nn) = tmplon(n)
              tmp2lat(njs(j)+nn) = tmplat(n)
              tmp2lev(njs(j)+nn) = tmplev(n)
              tmp2dat(njs(j)+nn) = tmpdat(n)
              tmp2err(njs(j)+nn) = tmperr(n)
              tmp2instid(njs(j)+nn) = tmpinstid(n)
              !      tmp2k(njs(j)+nn) = tmpk(n)
              tmp2dep(njs(j)+nn) = tmpdep(n)
              !tmp2hdxf(njs(j)+nn,:) = tmphdxf(n,:)
              do ii = 1, nbv
                 tmp2hdxf(ii,njs(j)+nn) = tmphdxf(n,ii)
              enddo
              tmp2id(njs(j)+nn) = tmpid(n)     !(DRIFTERS)
              tmp2time(njs(j)+nn) = tmptime(n) !(DRIFTERS)
           enddo
        enddo
        call MemReport(MPI_COMM_WORLD,"line 565 in set_letkf_obs") ! CDA_DEBUG
        if (myrank==0) call system("cat /proc/meminfo") ! CDA_DEBUG

        ! For each latitude, identify the number of obs per longitude.
        ! Then, rearrange observations by longitude within each latitude step
        do j=1,nlat-1
           if (nj(j) == 0) then
              nobsgrd(:,j) = njs(j)
              CYCLE
           endif
           nn = 0
           WRITE(6,*) "DEBUG_lat: j, nlat=", j, nlat
           do i=1,nlon
              do n=njs(j)+1,njs(j)+nj(j)

                 ! Find the correct longitude bin for this observation...
                 if (i < nlon) then
                    if (tmp2lon(n) < lon(i) .OR. lon(i+1) <= tmp2lon(n)) CYCLE
                 else
                    ! STEVE: this is causing nn /= nj(j), the error thrown below.
                    !        We need these points that are skipped, otherwise there are
                    !        blank entries in the obselm etc. arrays, and this will
                    !        lead to problems during the main letkf algorithm.
                    !        Another solution may be to cut out all the empty entries
                    !        by changing the obsxxx indicies.
                    !
                    if (tmp2lon(n) < lon(nlon)) CYCLE

                    !STEVE: debugging
                    if (.false.) then
                       WRITE(6,*) "n, nn, njs(j), nj(j) = ", n, nn, njs(j), nj(j)
                       WRITE(6,*) "KEEPING, i == nlon == ", i, nlon
                       WRITE(6,*) "tmp2lon(n) = ", tmp2lon(n)
                       WRITE(6,*) "lon(nlon) = ", lon(nlon)
                       !WRITE(6,*) "either tmp2lon(n) >= lon(nlon) .OR. 360.0d0 > tmp2lon(n)"
                       WRITE(6,*) "tmp2lon(n) >= lon(nlon)"
                       WRITE(6,*) "========================================================"
                    ENDIF
                 endif
                 nn = nn + 1
                 obselm(njs(j)+nn) = tmp2elm(n)
                 obslon(njs(j)+nn) = tmp2lon(n)
                 obslat(njs(j)+nn) = tmp2lat(n)
                 obslev(njs(j)+nn) = tmp2lev(n)
                 obsdat(njs(j)+nn) = tmp2dat(n)
                 obserr(njs(j)+nn) = tmp2err(n)
                 obsdep(njs(j)+nn) = tmp2dep(n)
                 obsinstid(njs(j)+nn) = tmp2instid(n)
                 do ii = 1, nbv
                    obshdxf(njs(j)+nn,ii) = tmp2hdxf(ii,n)
                 enddo
                 obsid(njs(j)+nn) = tmp2id(n)     !(DRIFTERS)
                 obstime(njs(j)+nn) = tmp2time(n) !(DRIFTERS)
              enddo

              ! This now contains the accumulated count of obs up to this lat, up to this lon
              nobsgrd(i,j) = njs(j) + nn
           enddo

           if (nn /= nj(j)) then
              WRITE(6,'(A,2I)') 'OBS DATA SORT ERROR: ',nn,nj(j)
              WRITE(6,'(F6.2,A,F6.2)') lat(j),'<= LAT <',lat(j+1)
              WRITE(6,'(F6.2,A,F6.2)') MINVAL(tmp2lat(njs(j)+1:njs(j)+nj(j))),'<= OBSLAT <',MAXVAL(tmp2lat(njs(j)+1:njs(j)+nj(j)))
              WRITE(6,*) "j = ", j
              WRITE(6,*) "njs(j) = ", njs(j)
              WRITE(6,*) "nj(j) = ", nj(j)
              !STEVE: this is bad, something is wrong
              WRITE(6,*) "STEVE: this error will cause matrix eigenvalue < 0 error."
              STOP 3
           endif

        enddo
        call MemReport(MPI_COMM_WORLD,"line 633 in set_letkf_obs") ! CDA_DEBUG

        DEALLOCATE( tmp2elm )
        DEALLOCATE( tmp2lon )
        DEALLOCATE( tmp2lat )
        DEALLOCATE( tmp2lev )
        DEALLOCATE( tmp2dat )
        DEALLOCATE( tmp2err )
        DEALLOCATE( tmp2instid )
        DEALLOCATE( tmp2dep )
        DEALLOCATE( tmp2hdxf )
        DEALLOCATE( tmp2id )    !(DRIFTERS)
        DEALLOCATE( tmp2time )  !(DRIFTERS)

    end if ![myrank==0]


    !
    !  broadcast sorted obs array to all processes
    !
    WRITE(6,*) "boradcast sorted obs array to all procs..."
    !STEVE: broadcast the 1d arrays from root onto all procs
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    WRITE(6,*) "Calling MPI_BCAST's..."
    CALL MPI_BCAST( obselm, nobs, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD,ierr)
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    CALL MPI_BCAST( obslon, nobs, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD,ierr)
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    CALL MPI_BCAST( obslat, nobs, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD,ierr)
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    CALL MPI_BCAST( obslev, nobs, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD,ierr)
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    CALL MPI_BCAST( obsdat, nobs, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD,ierr)
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    CALL MPI_BCAST( obserr, nobs, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD,ierr)
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    CALL MPI_BCAST( obsinstid, nobs, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD,ierr)
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    CALL MPI_BCAST( obsdep, nobs, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD,ierr)
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    CALL MPI_BCAST( obsid, nobs, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD,ierr)
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    CALL MPI_BCAST( obstime, nobs, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD,ierr)
    do ii =1, nbv
        CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
        WRITE(6,*) "Calling BSCAT OBS MEM=",ii
        CALL MPI_BCAST(obshdxf(:,ii), nobs, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD,ierr)
    enddo

    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    CALL MPI_BCAST( nobsgrd, nlon*nlat, MPI_INTEGER, 0, MPI_COMM_WORLD,ierr)
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

    WRITE(6,*) "sum(nobsgrd)=",sum(nobsgrd)
    WRITE(6,*) "sum(obshdxf)=",sum(obshdxf)


    DEALLOCATE( tmpelm )
    DEALLOCATE( tmplon )
    DEALLOCATE( tmplat )
    DEALLOCATE( tmplev )
    DEALLOCATE( tmpdat )
    DEALLOCATE( tmperr )
    DEALLOCATE( tmpinstid )
    DEALLOCATE( tmpk )
    DEALLOCATE( tmpdep )
    DEALLOCATE( tmphdxf )
    DEALLOCATE( tmpqc )
    DEALLOCATE( tmpid )     !(DRIFTERS)
    DEALLOCATE( tmptime )   !(DRIFTERS)

    RETURN
  END SUBROUTINE set_letkf_obs

SUBROUTINE get_hdxa(anal3dg,anal2dg,hdxa) !,depa)
!===============================================================================
! Compute the obs operator applied to the analysis analagous to hdxf
!===============================================================================
REAL(r_size), INTENT(IN) :: anal3dg(nlon,nlat,nlev,nv3d)
REAL(r_size), INTENT(IN) :: anal2dg(nlon,nlat,nv2d)
REAL(r_size),INTENT(OUT) :: hdxa(nobs)
!REAL(r_size),INTENT(OUT) :: depa(nobs)
REAL(r_size),ALLOCATABLE :: wk2d(:,:)
REAL(r_size),ALLOCATABLE :: wk1d(:)
REAL(r_size) :: ri,rj,rk
INTEGER :: n,i,ierr,obsper,start,finish
INTEGER :: prntmod=HUGE(1)
LOGICAL :: dodebug = .true.

hdxa = 0.0d0

! l=0
! DO
!   im = myrank+1 + nprocs * l
!   if (im > nbv) EXIT

!   WRITE(analfile(5:7),'(I3.3)') im
!   WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is reading a file ',analfile
!   CALL read_grd(analfile,v3d,v2d)

!   WRITE(6,*) "letkf_tools.f90::adapt_obserr: calling get_hdxa..."
!   CALL get_hdxa(v3d,v2d,hdxa(:,im),obsdep_a)
!     
!   l = l+1
! enddo

WRITE(6,'(A,I3.3,2A)') 'MYRANK ',myrank,' is computing Trans_XtoY for get_hdxa'
WRITE(6,*) "myrank+1,nobs,nprocs = ", myrank+1,nobs,nprocs
obsper = CEILING(REAL(nobs)/REAL(nprocs))
if (myrank + 1 .lt. nprocs)  then
  start = myrank*obsper+1
  finish = (myrank+1)*obsper 
else
  start = myrank*obsper+1
  finish = nobs
endif
WRITE(6,*) "obsper = ", obsper
WRITE(6,*) "start,finish = ", start, finish

if (dodebug) prntmod = NINT(obsper/2.0)

! Process all obs allocated to this processor
do n=start,finish
  if (MOD(n,prntmod) .eq. 0) WRITE(6,*) "n = ", n 
  ! interpolation
  if (MOD(n,prntmod) .eq. 0) WRITE(6,*) "Calling phys2ijk..."  
  !if (obslat(n)<65.0) then
  if (obslat(n)<=90.0) then
     CALL phys2ijk(obselm(n),obslon(n),obslat(n),obslev(n),ri,rj,rk)
  else
     CALL phys2ijk_tri(obselm(n),obslon(n),obslat(n),obslev(n),ri,rj,rk)
  end if
  if (MOD(n,prntmod) .eq. 0) WRITE(6,*) "ri,rj,rk = ", ri,rj,rk
  ! observational operator
  if (MOD(n,prntmod) .eq. 0) WRITE(6,*) "Calling Trans_XtoY..."
  CALL Trans_XtoY(obselm(n),ri,rj,rk,anal3dg,anal2dg,hdxa(n))
  if (MOD(n,prntmod) .eq. 0) then
    WRITE(6,*) "anal3d(FLOOR(ri),FLOOR(rj),FLOOR(rk),:) = ", anal3dg(FLOOR(ri),FLOOR(rj),FLOOR(rk),:)
    WRITE(6,*) "anal3d(CEILING(ri),FLOOR(rj),FLOOR(rk),:) = ", anal3dg(CEILING(ri),FLOOR(rj),FLOOR(rk),:)
    WRITE(6,*) "anal3d(FLOOR(ri),CEILING(rj),FLOOR(rk),:) = ", anal3dg(FLOOR(ri),CEILING(rj),FLOOR(rk),:)
    WRITE(6,*) "anal3d(FLOOR(ri),FLOOR(rj),CEILING(rk),:) = ", anal3dg(FLOOR(ri),FLOOR(rj),CEILING(rk),:)
    WRITE(6,*) "anal3d(CEILING(ri),CEILING(rj),FLOOR(rk),:) = ", anal3dg(CEILING(ri),CEILING(rj),FLOOR(rk),:)
    WRITE(6,*) "anal3d(CEILING(ri),FLOOR(rj),CEILING(rk),:) = ", anal3dg(CEILING(ri),FLOOR(rj),CEILING(rk),:)
    WRITE(6,*) "anal3d(FLOOR(ri),CEILING(rj),CEILING(rk),:) = ", anal3dg(FLOOR(ri),CEILING(rj),CEILING(rk),:)
    WRITE(6,*) "anal3d(CEILING(ri),CEILING(rj),CEILING(rk),:) = ", anal3dg(CEILING(ri),CEILING(rj),CEILING(rk),:)
  endif
  if (MOD(n,prntmod) .eq. 0) WRITE(6,*) "hdxa(n) = ", hdxa(n)
  if (MOD(n,prntmod) .eq. 0) WRITE(6,*) "obsdat(n) = ", obsdat(n)
! if (MOD(n,prntmod) .eq. 0) WRITE(6,*) "pre-calc: depa(n) = ", depa(n)

! if (MOD(n,prntmod) .eq. 0) WRITE(6,*) "Calculating dep..."
! depa(n) = obsdat(n) - hdxa(n) ! y-Hx
! if (MOD(n,prntmod) .eq. 0) WRITE(6,*) "depa(n) = ", depa(n)
enddo

WRITE(6,*) "MPI_BARRIER"
CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
WRITE(6,*) "ALLOCATE(wk1d(nobs))..."
ALLOCATE(wk1d(nobs))
wk1d=hdxa
WRITE(6,*) "MPI_BARRIER"
CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
WRITE(6,*) "MPI_ALLREDUCE"
CALL MPI_ALLREDUCE(wk1d,hdxa,nobs,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
WRITE(6,*) "Calling MPI_BARRIER..."
CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
WRITE(6,*) "finished MPI_ALLREDUCE"
DEALLOCATE(wk1d)

!WRITE(6,*) "MPI_BARRIER"
!CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
!WRITE(6,*) "ALLOCATE(wk1d(nobs))..."
!ALLOCATE(wk1d(nobs))
!wk1d = depa
!WRITE(6,*) "MPI_BARRIER"
!CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
!WRITE(6,*) "MPI_ALLREDUCE"
!CALL MPI_ALLREDUCE(wk1d,depa,nobs,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
!WRITE(6,*) "MPI_BARRIER"
!CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
!DEALLOCATE(wk1d)

!STEVE: may need this if I want to do for full ensemble:
!do n=1,nobs
!  depa(n) = hdxa(n,1)
!  do i=2,nbv
!    depa(n) = depa(n) + hdxa(n,i)
!  enddo
!  depa(n) = depa(n) / REAL(nbv,r_size)
!  do i=1,nbv
!    hdxa(n,i) = hdxa(n,i) - depa(n) ! Hdx
!    !STEVE: make sure none are zero
!    if ( debug_hdxf_0 .AND. hdxa(n,i) == 0 ) then
!      WRITE(6,*) "get_hdxa:: WARNING: hdxa(n,i) == 0"
!      WRITE(6,*) "This is later used as a divisor for adaptive inflation."
!    endif
!  enddo
!  depa(n) = obsdat(n) - depa(n) ! y-Hx
!enddo

END SUBROUTINE get_hdxa

END MODULE letkf_obs
