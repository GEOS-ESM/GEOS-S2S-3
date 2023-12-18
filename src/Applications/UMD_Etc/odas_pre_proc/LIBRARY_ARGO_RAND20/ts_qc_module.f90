MODULE TS_QC_MODULE

  IMPLICIT NONE
     
  PUBLIC :: constant_value, t_spike, s_spike, t_jump, s_jump
  
  CONTAINS

  ! .................................................................
    SUBROUTINE constant_value(nobs,t,z,npts,qc_lev,svar,nflag)
  ! Implement MetOffice spike check (Ingleby & Huddleston)
  ! .................................................................
    IMPLICIT NONE
    character,     intent(in)     :: svar
    integer,       intent(in)     :: npts, nobs
    real,          intent(in)     :: t(0:npts-1)
    real,          intent(in)     :: z(0:npts-1)
    real,          intent(inout)  :: qc_lev(0:npts-1)
    integer,       intent(out)    :: nflag

    integer                       :: i, j, k, print_error, cnt, debug
    real                          :: dt(npts-1)
    real                          :: dmax, perc
    real                          :: dZ, dP
    real,          parameter      :: miss = 999.e9

    debug = 0
    dt = 0

    if (svar=='T')  then
      dZ = 100
      dP = 90
    elseif (svar=='S') then
      dZ = 50
      dP = 70
    endif

    cnt  = 0
    perc = 0
    DO j=1, npts-1
      if ( (qc_lev(j)>=0 .and. qc_lev(j)<=1) .and. (qc_lev(j-1)>=0 .and. qc_lev(j-1)<=1)) then
        dt(j) = t(j)-t(j-1)
        if (dt(j)==0) cnt=cnt+1
      endif ! good levels
    ENDDO

    if ( cnt>=1) then
      perc = 100*(cnt/npts)
    endif
    dmax = abs(z(0)-z(npts-1))

    if (perc >= dP .and. dmax >= dZ) then
      if (debug==1) print *, '    CONSTANT VALUE ',nobs, npts, cnt, perc, dP, dmax, dZ
      nflag=1
    endif
 

  return
  END SUBROUTINE constant_value


  ! .................................................................
    SUBROUTINE t_spike(nobs,t,s,z,npts,tqc_lev,sqc_lev,lat,nflag,metflag)
  ! Implement MetOffice spike check (Ingleby & Huddleston)
  ! If T is flagged, need to flag S also 
  ! .................................................................
    IMPLICIT NONE
    integer,       intent(inout)  :: npts
    integer,       intent(in)     :: nobs
    real,          intent(inout)  :: t(0:npts-1)
    real,          intent(inout)  :: s(0:npts-1)
    real,          intent(inout)  :: z(0:npts-1)
    real,          intent(in)     :: lat
    real,          intent(inout)  :: tqc_lev(0:npts-1)
    real,          intent(inout)  :: sqc_lev(0:npts-1)
    integer,       intent(out)    :: nflag, metflag

    integer                       :: i, j, k, print_error, debug
    real                          :: dt(npts-1),   dz(npts-1)
    real                          :: ttoli(npts-1)
    real                          :: ttol(0:npts-1)
    real                          :: ztol(0:npts-1)

    debug = 0
    nflag = 0
    !print *, 'Iterate: ', nflag

  ! Define Tolerances for Each Depth
    DO j=0, npts-1
      if (z(j) < 300)                    ttol(j) = 5
      if (z(j) >=300 .and. z(j) < 400)   ttol(j) = 2.5
      if (z(j) >=500 .and. z(j) < 600)   ttol(j) = 2.0
      if (z(j) >=600)                    ttol(j) = 1.5
      if (lat>=-20 .or. lat<=20) then
        if (z(j) >=300 .and. z(j) < 400) ttol(j) = 5.0
        if (z(j) >=400 .and. z(j) < 500) ttol(j) = 2.5
      endif
     ! Vertical differences are formed where level k is at most 50m deeper than level k-1
     !                                                        (100m where level k is 350m or deeper)
      if (z(j) < 350)  ztol(j) = 50
      if (z(j) >= 350) ztol(j) = 100
    ENDDO
 

  DO j=1, npts-1
    dt(j)    = t(j)-t(j-1)
    dz(j)    = z(j)-z(j-1)
    dz(j)    = max(10.0,z(j)-z(j-1)) ! From Simon, added Aug 3, 2009
    ttoli(j) = (ttol(j)+ttol(j-1))/2
    !print *, j, dt(j), dz(j), ttoli(j)
  ENDDO
  !print *, npts
  !print *, t(0:npts-1)
  !print *, ''
  !print *, z(0:npts-1)

  DO k=2,npts-1
    !print *, k, dt(k), dt(k-1), ttoli(k), ttoli(k-1), 0.5*ttoli(k), 0.5*ttoli(k-1)
     
    IF ( dz(k) <= ztol(k) ) THEN
      ! A. Large Spikes                                                    opposite sign
        if ( (abs(dt(k-1)) > ttoli(k-1) .or. abs(dt(k)) > ttoli(k)) .and. abs(dt(k-1)+dt(k)) < 0.5*ttoli(k) ) then
          if (debug==1) print *, '    T SPIKE A: ', nobs, k, npts
          metflag=1
          nflag = 1
          npts = npts-1
          t(0:k-2)            = t(0:k-2)
          t(k-1:npts-1)       = t(k:npts-1)
          s(0:k-2)            = s(0:k-2)
          s(k-1:npts-1)       = s(k:npts-1)
          z(0:k-2)            = z(0:k-2)
          z(k-1:npts-1)       = z(k:npts-1)
          tqc_lev(0:k-2)      = tqc_lev(0:k-2)
          tqc_lev(k-1:npts-1) = tqc_lev(k:npts-1)
          sqc_lev(0:k-2)      = sqc_lev(0:k-2)
          sqc_lev(k-1:npts-1) = sqc_lev(k:npts-1)

      ! New. Large Spike at First Point
        elseif ( (abs(dt(k-1)) > ttoli(k-1) .or. abs(dt(k)) > ttoli(k)) .and. k==2 .and.  &
                  abs(dt(k-1)) > abs(dt(k)) ) then
          if (debug==1) print *, '    T SPIKE A First Pt: ', nobs, k, npts
          nflag = 1
          metflag=2
          if ( abs(dt(k-1)) > abs(dt(k)) ) then
            npts = npts-1
            t(0:npts-1)      = t(k-1:npts)
            s(0:npts-1)      = s(k-1:npts)
            z(0:npts-1)      = z(k-1:npts)
            tqc_lev(0:npts-1) = tqc_lev(k-1:npts)
            sqc_lev(0:npts-1) = sqc_lev(k-1:npts)
          else ! This seems to be okay for now
            !print *, 'ERROR:CHECK THIS Spike A First pt', k, npts
            !print *, t(0:npts-1)
            !print *, z(0:npts-1)
            !stop
          endif

      ! New. Large Spike at Last Point (flag last 3 pts)
        elseif ( (abs(dt(k-1)) > ttoli(k-1) .or. abs(dt(k)) > ttoli(k)) .and. k==npts-1 ) then
          if (debug==1) print *, '    T SPIKE A Last Pt: ',  nobs, k, npts
          nflag = 1
          metflag=2
          npts = npts-3
            t(0:npts-1)      = t(0:npts-1)
            s(0:npts-1)      = s(0:npts-1)
            z(0:npts-1)      = z(0:npts-1)
            tqc_lev(0:npts-1) = tqc_lev(0:npts-1)
            sqc_lev(0:npts-1) = sqc_lev(0:npts-1)

      ! B. Smaller Spike
      ! i
        elseif ( abs(dt(k-1)) > 0.5*ttoli(k-1) .or. abs(dt(k)) > 0.5*ttoli(k) ) then
          !print *, 'B i', k
          ! ii
            if ( abs(dt(k-1)/dz(k-1)) > 0.05 .or. abs(dt(k)/dz(k)) > 0.05 ) then
              !print *, 'B ii', k
            ! iii
              if ( abs(dt(k-1) + dt(k)) < 0.25*abs(dt(k-1) - dt(k)) ) then
                if (debug==1) print *, '    T SPIKE B: ', nobs, k, npts
                nflag = 1
                npts = npts-1
                metflag=1
                t(0:k-2)           = t(0:k-2)
                t(k-1:npts-1)      = t(k:npts)
                s(0:k-2)           = s(0:k-2)
                s(k-1:npts-1)      = s(k:npts)
                z(0:k-2)           = z(0:k-2)
                z(k-1:npts-1)      = z(k:npts)
                tqc_lev(0:k-2)      = tqc_lev(0:k-2)
                tqc_lev(k-1:npts-1) = tqc_lev(k:npts)
                sqc_lev(0:k-2)      = sqc_lev(0:k-2)
                sqc_lev(k-1:npts-1) = sqc_lev(k:npts)
              endif
            endif
        endif ! 1st Checks

      ! C. Large Step
        if ( abs(dt(k-1)) > ttoli(k-1) ) then
          !print *, 'C'
          if (z(k) <= 250 .or. (dt(k-1) < 0 .and. dt(k-1) < 3*ttoli(k-1)) ) then
            !print *, 'ok 1', dt(k-1), dt(k), 3*ttoli(k-1)
          elseif ( t(k-1)<= 0.5*(ttoli(k-2)+ttoli(k))/2 ) then
            !print *, 'ok 2', dt(k-1), dt(k), 3*ttoli(k-1)
          else
            if (debug==1) print *, '    T STEP C: ', nobs, k, npts
            metflag=1
            nflag = 1
            npts = npts-1
            t(0:k-2)            = t(0:k-2)
            !print *, size(t)
            !print *, k, npts
            
            t(k-1:npts-1)       = t(k:npts)
            s(0:k-2)            = s(0:k-2)
            s(k-1:npts-1)       = s(k:npts)
            z(0:k-2)            = z(0:k-2)
            z(k-1:npts-1)       = z(k:npts)
            tqc_lev(0:k-2)      = tqc_lev(0:k-2)
            tqc_lev(k-1:npts-1) = tqc_lev(k:npts)
            sqc_lev(0:k-2)      = sqc_lev(0:k-2)
            sqc_lev(k-1:npts-1) = sqc_lev(k:npts)

            if (k /= npts) then ! not last point in profile
              !print *, 'SPECIAL CASE ', k, npts, t(k-1), t(k)
              npts = npts-1
              t(0:k-3)            = t(0:k-3)
              t(k-2:npts-1)       = t(k-1:npts)
              s(0:k-3)            = s(0:k-3)
              s(k-2:npts-1)       = s(k-1:npts)
              z(0:k-3)            = z(0:k-3)
              z(k-2:npts-1)       = z(k-1:npts)
              tqc_lev(0:k-3)      = tqc_lev(0:k-3)
              tqc_lev(k-2:npts-1) = tqc_lev(k-1:npts)
              sqc_lev(0:k-3)      = sqc_lev(0:k-3)
              sqc_lev(k-2:npts-1) = sqc_lev(k-1:npts)
            endif
          endif
        else  ! No flag set
          !print *, 'No Flag', k
        endif ! 2nd Checks

    ELSE ! Fails MetOffice Depth Check
         ! Still want to check for T jumps at 1st and last points
      ! New. Large Spike at First Point
       if ( (abs(dt(k-1)) > ttoli(k-1) .or. abs(dt(k)) > ttoli(k)) .and. k==2 .and.  &
                  abs(dt(k-1)) > abs(dt(k)) ) then
          if (debug==1) print *, '    T SPIKE A First Pt end: ', nobs, k, npts
          nflag = 1
          metflag=2
          npts = npts-1
          if ( abs(dt(k-1)) > abs(dt(k)) ) then
            t(0:npts-1)      = t(k-1:npts-1)
            s(0:npts-1)      = s(k-1:npts-1)
            z(0:npts-1)      = z(k-1:npts-1)
            tqc_lev(0:npts-1) = tqc_lev(k-1:npts-1)
            sqc_lev(0:npts-1) = sqc_lev(k-1:npts-1)
          else ! This seems to be okay for now
            !print *, 'ERROR:CHECK THIS Spike A First pt', k, npts
            !print *, t(0:npts-1)
            !print *, z(0:npts-1)
            !stop
          endif

      ! New. Large Spike at Last Point (flag last 3 pts)
        elseif ( (abs(dt(k-1)) > ttoli(k-1) .or. abs(dt(k)) > ttoli(k)) .and. k==npts-1 ) then
          if (debug==1) print *, '    T SPIKE A Last Pt end: ', nobs, k, npts
          nflag = 1
          npts = npts-3
          metflag=2
            t(0:npts-1)      = t(0:npts-1)
            s(0:npts-1)      = s(0:npts-1)
            z(0:npts-1)      = z(0:npts-1)
            tqc_lev(0:npts-1) = tqc_lev(0:npts-1)
            sqc_lev(0:npts-1) = sqc_lev(0:npts-1)
        endif
       
    ENDIF ! Depth Check

  ENDDO


  return
  END SUBROUTINE t_spike


  ! .................................................................
    SUBROUTINE s_spike(nobs,s,z,npts,sqc_lev,lat,nflag,metflag)
  ! Implement MetOffice spike check (Ingleby & Huddleston)
  ! .................................................................
    IMPLICIT NONE
    integer,       intent(inout)  :: npts
    integer,       intent(in)     :: nobs
    real,          intent(inout)  :: s(0:npts-1)
    real,          intent(inout)  :: z(0:npts-1)
    real                          :: ttol(0:npts-1), ztol(0:npts-1)
    real,          intent(in)     :: lat
    real,          intent(inout)  :: sqc_lev(0:npts-1)
    integer,       intent(out)    :: nflag, metflag

    integer                       :: i, j, k, print_error, debug
    real                          :: dt(npts-1),   dz(npts-1)
    real                          :: ttoli(npts-1)

    debug = 0
    nflag = 0
    !print *, 'Iterate: ', nflag

  ! Define Tolerances for Each Depth
    DO j=0, npts-1
      if (z(j) < 300)  ttol(j) = 1.0
      if (z(j) >=300)  ttol(j) = 0.2

      if (lat>=-20 .or. lat<=20) then
        if (z(j) >=300 .and. z(j) < 400) ttol(j) = 1.0
        if (z(j) >=400 .and. z(j) < 500) ttol(j) = 0.2
      endif
     ! Vertical differences are formed where level k is at most 50m deeper than level k-1
     !                                                        (100m where level k is 350m or deeper)
      if (z(j) < 350)  ztol(j) = 50
      if (z(j) >= 350) ztol(j) = 100
    ENDDO

    DO j=1, npts-1
      dt(j)    = s(j)-s(j-1)
      dz(j)    = z(j)-z(j-1)
      ttoli(j) = (ttol(j)+ttol(j-1))/2
    ENDDO


  DO k=2,npts-1
    !print *, k, dt(k), dt(k-1), ttoli(k), ttoli(k-1), 0.5*ttoli(k), 0.5*ttoli(k-1)
     
    IF ( dz(k) <= ztol(k) ) THEN
      ! A. Large Spikes                                                    opposite sign
        if ( (abs(dt(k-1)) > ttoli(k-1) .or. abs(dt(k)) > ttoli(k)) .and. abs(dt(k-1)+dt(k)) < 0.5*ttoli(k) ) then
          if (debug==1) print *, '    S SPIKE A: ', nobs, k, npts, dt(k-1), dt(k)
          metflag = 1
          nflag   = 1

          npts = npts-1
          s(0:k-2)            = s(0:k-2)
          s(k-1:npts-1)       = s(k:npts)
          z(0:k-2)            = z(0:k-2)
          z(k-1:npts-1)       = z(k:npts)
          sqc_lev(0:k-2)      = sqc_lev(0:k-2)
          sqc_lev(k-1:npts-1) = sqc_lev(k:npts)
          !print *, s(0:npts-1)
          !print *, z(0:npts-1)

      ! New. Large Spike at First Point
        elseif ( (abs(dt(k-1)) > ttoli(k-1) .or. abs(dt(k)) > ttoli(k)) .and. k==2 .and.  &
                  abs(dt(k-1)) > abs(dt(k)) ) then
          if (debug==1) print *, '    S SPIKE A First Pt: ', nobs, k, npts, dt(k-1), dt(k)
          nflag = 1
          metflag=2

          npts = npts-1
          if ( abs(dt(k-1)) > abs(dt(k)) ) then
            s(0:npts-1)       = s(k-1:npts)
            z(0:npts-1)       = z(k-1:npts)
            sqc_lev(0:npts-1) = sqc_lev(k-1:npts)
          else ! This seems to be okay for now
            !print *, 'ERROR:CHECK THIS Spike A First pt', k, npts
            !print *, s(0:npts-1)
            !print *, z(0:npts-1)
            !stop
          endif


      ! New. Large Spike at Last Point (flag last 3 pts)
        elseif ( (abs(dt(k-1)) > ttoli(k-1) .or. abs(dt(k)) > ttoli(k)) .and. k==npts-1 ) then
          if (debug==1) print *, '    S SPIKE A Last Pt: ',  nobs, k, npts, dt(k-1), dt(k)
          nflag = 1
          metflag=2
          npts = npts-3
          s(0:npts-1)       = s(0:npts-1)
          z(0:npts-1)       = z(0:npts-1)
          sqc_lev(0:npts-1) = sqc_lev(0:npts-1)
        endif ! 1st Checks

      ! C. Large Step
        if ( abs(dt(k-1)) > ttoli(k-1) ) then
          !print *, 'C'
           !print *,  abs(dt(k-1)), ttoli(k-1)
           !print *, dt(k-1), 0.5*(ttoli(k-2)+ttoli(k))/2
          if ( dt(k-1) <= 0.5*(ttoli(k-2)+ttoli(k))/2 ) then
            !print *, 'ok i'
          else
            if (debug==1) print *, '    S STEP C: ', nobs, k, npts, dt(k-1), dt(k), ttoli(k-1) 
            metflag=1
            nflag = 1
            npts = npts-1
            s(0:k-2)            = s(0:k-2)
            s(k-1:npts-1)       = s(k:npts)
            z(0:k-2)            = z(0:k-2)
            z(k-1:npts-1)       = z(k:npts)
            sqc_lev(0:k-2)      = sqc_lev(0:k-2)
            sqc_lev(k-1:npts-1) = sqc_lev(k:npts)
          endif
        endif ! 2nd Checks

    ELSE ! Fails MetOffice Depth Check
         ! Still want to check for T jumps at 1st and last points
      ! New. Large Spike at First Point
       if ( (abs(dt(k-1)) > ttoli(k-1) .or. abs(dt(k)) > ttoli(k)) .and. k==2 .and.  &
                  abs(dt(k-1)) > abs(dt(k)) ) then
          if (debug==1) print *, '    S SPIKE A First Pt end: ', nobs, k, npts, dt(k-1), dt(k)
          nflag = 1
          metflag=2
          npts = npts-1
          if ( abs(dt(k-1)) > abs(dt(k)) ) then
            s(0:npts-1)      = s(k-1:npts-1)
            z(0:npts-1)      = z(k-1:npts-1)
             sqc_lev(0:npts-1) = sqc_lev(k-1:npts-1)
          else ! This seems to be okay for now
            print *, 'ERROR:CHECK THIS Spike A First pt', k, npts
            !print *, t(0:npts-1)
            !print *, z(0:npts-1)
            !stop
          endif

      ! New. Large Spike at Last Point (flag last 3 pts)
        elseif ( (abs(dt(k-1)) > ttoli(k-1) .or. abs(dt(k)) > ttoli(k)) .and. k==npts-1 ) then
          if (debug==1) print *, '    S SPIKE A Last Pt end: ', nobs, k, npts, dt(k-1), dt(k)
          nflag = 1
          metflag=2
          npts = npts-3
          s(0:npts-1)      = s(0:npts-1)
          z(0:npts-1)      = z(0:npts-1)
          sqc_lev(0:npts-1) = sqc_lev(0:npts-1)
         endif
       
    ENDIF ! Depth Check

  ENDDO 

  return
  END SUBROUTINE s_spike


  ! .................................................................
    SUBROUTINE t_jump(nobs,t,s,z,npts,tqc_lev,sqc_lev,lon,lat,month,nflag,date_time)
  ! .................................................................
    USE levitus_clim_module
    IMPLICIT NONE

    integer,       intent(in)     :: month, date_time
    integer,       intent(inout)  :: npts
    integer,       intent(in)     :: nobs
    real,          intent(in)     :: lon, lat
    real,          intent(inout)  :: t(npts), s(npts)
    real,          intent(inout)  :: z(npts)
    real,          intent(inout)  :: tqc_lev(npts), sqc_lev(npts)
    integer,       intent(out)    :: nflag

    integer                       :: j, jj, debug
    integer                       :: firstpt, lastpt
    real                          :: dT1, dT2, dZ1, dZ2    
    real, parameter               :: dT = 3.0, dZ = 1000  

  ! Levitus Clim Module
    character*4, parameter        :: svar='TEMP'
    integer, parameter            :: Ldim = 33
    integer                       :: Ldim_good, intdim
    real                          :: Ldepth(Ldim), Ldata(Ldim)
    real                          :: Ldata_interp(npts)
    real                          :: max_ds, max_dz, Trms, Zlim

  debug = 0
  nflag = 0
     
  LOOP_J : DO j=1, npts
    dT1 = 0
    dT2 = 0
    dZ1 = 0
    dZ2 = 0
    lastpt  = 0
    firstpt = 0   
    ! Compute Deltas
    ! Middle of Profile
     if ( j /= 1 .and. j /= npts ) then
             dT1 = (t(j) - t(j-1))
             dT2 = (t(j) - t(j+1))
	     dZ1 = (z(j) - z(j-1))
             dZ2 = (z(j) - z(j+1))
             if ( j == npts-1 ) then
                  lastpt = 1
             endif
    ! First Point
     elseif ( j==1 ) then
	     dT1 = 0
	     dT2 = (t(j) - t(j+1))
	     dZ1 = 0
	     dZ2 = (z(j) - z(j+1))
	     firstpt = 1
    ! Last Point
     elseif ( j==npts ) then
	     dT1 = (t(j) - t(j-1))	           
	     dT2 = 0
	     dZ1 = (z(j) - z(j-1))	           
	     dZ2 = 0
	     lastpt = 1	     
     endif           	  
 
     ! Two Tjump>3 
       if ( (abs(dT1) > dT .and. abs(dT2) > dT) .and. &
	        ((dT1 >= 0 .and. dT2 >= 0) .or. (dT1 < 0 .and. dT2 < 0)) .and. &
	          tqc_lev(j)==1 .and. lastpt == 0 .and. firstpt == 0) then	       
                  tqc_lev(j) = -1
                  nflag=1
                  EXIT LOOP_J
       endif
  ENDDO LOOP_J
 
  if (nflag==1) then
    if (debug==1) print *, '    T-JUMP: Compare with Climatology ', j, lon, lat

    call get_lev_data (lon,lat,month,Ldim_good, &
                       Ldata,Ldepth,svar)

    if (Ldim_good > 0) then
      call interp_lev_data(Ldim_good,Ldata,Ldepth,npts,t,z,&
                           Ldata_interp,max_ds,max_dz,intdim,Trms,Zlim)

    ! Zlim must be greater than z(j) (point of T-Jump)

      if ( Trms > 0.5 .and. Zlim > z(j) ) then
        if (debug==1) print *, '     RMS > 0.5: ',nobs, date_time, Trms, Zlim, z(j)
        !print *, t(1:npts)
        !print *, z(1:npts)
        !print *, Ldata_interp(1:intdim)
        !print *, z(1:intdim) 
        tqc_lev(1:npts) = -1
        sqc_lev(1:npts) = -1
        npts = 0
      elseif( Trms > 0.5 .and. Zlim <= z(j) ) then ! TJump is past Levitus comparison
        if (debug==1) print *, '     RMS > 0.5 > LevDepth: ',nobs, date_time, Trms, max_ds, max_dz
        tqc_lev(1:npts) = -1
        sqc_lev(1:npts) = -1
        npts = 0
      elseif (Trms <= 0.5 .and. Zlim > z(j)) then
        if (debug==1) print *, '     RMS okay: ',nobs, date_time, Trms, Zlim, z(j)
        !print *, t(1:npts)
        !print *, z(1:npts)
        !print *, Ldata_interp(1:npts)
      elseif (Trms <= 0.5 .and. Zlim <= z(j)) then
        if (debug==1) print *, '     Not Enough Levitus Data: ',nobs, date_time, Trms, Zlim, z(j)
        npts = npts-1
        t(j:npts-1) = t(j+1:npts)
        s(j:npts-1) = s(j+1:npts)
        z(j:npts-1) = z(j+1:npts)    
        stop
      endif
    else
      if (debug==1) print *, '     No Levitus Data', nobs
      ! Take out points below where the T-Jump occured
      !print *, t(1:npts)
      !print *, z(1:npts)
      npts = npts-1
      t(j:npts-1) = t(j+1:npts)
      s(j:npts-1) = s(j+1:npts)
      z(j:npts-1) = z(j+1:npts)    
      !print *, t(1:npts)
      !print *, z(1:npts)

    endif
  endif

  return
  END SUBROUTINE t_jump

  ! .................................................................
    SUBROUTINE s_jump(nobs,s,z,npts,sqc_lev,lon,lat,month,nflag,date_time)
  ! .................................................................
    USE levitus_clim_module
    IMPLICIT NONE
    integer,       intent(in)     :: month, date_time
    integer,       intent(inout)  :: npts
    integer,       intent(in)     :: nobs
    real,          intent(in)     :: lon, lat
    real,          intent(inout)  :: s(npts)
    real,          intent(inout)  :: z(npts)
    real,          intent(inout)  :: sqc_lev(npts)
    integer,       intent(out)    :: nflag

    integer                       :: j, jj, debug
    integer                       :: firstpt, lastpt
    real                          :: DS1, DS2, dZ1, dZ2    
    real, parameter               :: DS = 0.3, dZ = 1000  

  ! Levitus Clim Module
    character*4, parameter        :: svar='SALT'
    integer, parameter            :: Ldim = 33
    integer                       :: Ldim_good, intdim
    real                          :: Ldepth(Ldim), Ldata(Ldim)
    real                          :: Ldata_interp(npts)
    real                          :: max_ds, max_dz, Srms, Zlim

  debug = 0
  nflag = 0
  
  !print *, 'in sjump ',npts
  LOOP_J : DO j=1, npts
    DS1 = 0
    DS2 = 0
    dZ1 = 0
    dZ2 = 0
    lastpt  = 0
    firstpt = 0   
    ! Compute Deltas
    ! Middle of Profile
     if ( j /= 1 .and. j /= npts ) then
             DS1 = (s(j) - s(j-1))
             DS2 = (s(j) - s(j+1))
	     dZ1 = (z(j) - z(j-1))
             dZ2 = (z(j) - z(j+1))
             if ( j == npts-1 ) then
                  lastpt = 1
             endif
    ! First Point
     elseif ( j==1 ) then
	     DS1 = 0
	     DS2 = (s(j) - s(j+1))
	     dZ1 = 0
	     dZ2 = (z(j) - z(j+1))
	     firstpt = 1
    ! Last Point
     elseif ( j==npts ) then
	     DS1 = (s(j) - s(j-1))	           
	     DS2 = 0
	     dZ1 = (z(j) - z(j-1))	           
	     dZ2 = 0
	     lastpt = 1	     
     endif           	  
 
     
       if ( (abs(DS1) > DS .and. abs(DS2) > DS) .and. &
	        ((DS1 >= 0 .and. DS2 >= 0) .or. (DS1 < 0 .and. DS2 < 0)) .and. &
	          sqc_lev(j)==1 .and. lastpt == 0 .and. firstpt == 0) then	       
                  sqc_lev(j) = -1
                  nflag=1
                  EXIT LOOP_J
       endif
  ENDDO LOOP_J
 
  if (nflag==1) then
      s(j:npts-1) = s(j+1:npts)
      z(j:npts-1) = z(j+1:npts)    
      !s(j-1:npts-3) = s(j+2:npts) ! Take out 3 pts
      !z(j-1:npts-3) = z(j+2:npts) ! Take out 3 pts   
      npts = npts-3

    if (debug==1) print *, '    S-JUMP: Compare with Climatology ', j, lon, lat

    call get_lev_data (lon,lat,month,Ldim_good, &
                       Ldata,Ldepth,svar)

    if (Ldim_good > 0) then
      !print *, 'call lev data ', npts, shape(s)
      call interp_lev_data(Ldim_good,Ldata,Ldepth,npts,s,z,&
                           Ldata_interp,max_ds,max_dz,intdim,Srms,Zlim)
    ! Zlim must be greater than z(j) (point of T-Jump)
      !open (100, file='test', status='unknown',form='unformatted')
      !write(100) s(1:npts)
      !write(100) z(1:npts)
      !write(100) Ldata_interp(1:intdim)
      !write(100) z(1:intdim) 
      !close(100)

      if ( Srms > 0.05 .and. Zlim > z(j) ) then
        if (debug==1) print *, '     RMS > 0.05: ',nobs, date_time, Srms, Zlim, z(j)
        !print *, s(1:npts)
        !print *, z(1:npts)
        !print *, Ldata_interp(1:intdim)
        !print *, z(1:intdim) 

        sqc_lev(1:npts) = -1
        sqc_lev(1:npts) = -1
        npts = 0
      elseif( Srms > 0.05 .and. Zlim <= z(j) ) then ! TJump is past Levitus comparison
        if (debug==1) print *, '     RMS > 0.05 > LevDepth: ',nobs, date_time, Srms, max_ds, max_dz
        sqc_lev(1:npts) = -1
        sqc_lev(1:npts) = -1
        npts = 0
      elseif (Srms <= 0.05 .and. Zlim > z(j)) then
        if (debug==1) print *, '     RMS okay: ',nobs, date_time, Srms, Zlim, z(j)
        !print *, t(1:npts)
        !print *, z(1:npts)
        !print *, Ldata_interp(1:npts)
      elseif (Srms <= 0.05 .and. Zlim <= z(j)) then
        if (debug==1) print *, '     Not Enough Levitus Data: ',nobs, date_time, Srms, Zlim, z(j)
        npts = npts-1
        s(j:npts-1) = s(j+1:npts)
        z(j:npts-1) = z(j+1:npts)    
        stop
      endif
    else
      if (debug==1) print *, '     No Levitus Data', nobs
      ! Take out points below where the T-Jump occured
      !print *, t(1:npts)
      !print *, z(1:npts)
      npts = npts-1
      s(j:npts-1) = s(j+1:npts)
      z(j:npts-1) = z(j+1:npts)    
      !print *, t(1:npts)
      !print *, z(1:npts)

    endif
  endif

  return
  END SUBROUTINE s_jump

END MODULE TS_QC_MODULE
