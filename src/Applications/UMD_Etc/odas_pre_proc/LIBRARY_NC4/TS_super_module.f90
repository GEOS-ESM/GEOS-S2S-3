MODULE TS_SUPER_MODULE

  IMPLICIT NONE
     
  PUBLIC :: superob_XYT, get_rms, interp_prf, sort_Z, interp2mom
  CONTAINS



   ! ...................................................................
    SUBROUTINE superob_XYT ( miss, npts1, npts2, npts3,     &
               temp1,  temp2,    salt1,   salt2, depth1, depth2, &
	       tqc1,   tqc2,     sqc1,    sqc2,  dqc1,   dqc2,   &
               depth3, temp_new, salt_new, &
	       dqc3,   tqc_new,  sqc_new,  &
               superob, superob_s,         &
	       Smiss1, Sbad1, Smiss2, Sbad2, Smiss3, Sbad3, Trms, Srms, mom_lev, max_npts)
  ! ...................................................................
  ! If positions of two profiles are to close and the rms is
  ! close enough, average them.
  ! If the rms is too large, accept profile with most points
  !
    IMPLICIT NONE
    real, parameter       :: Trms_lim = 0.5, Srms_lim = 0.1
    integer, intent(in)   :: npts1, npts2, max_npts
    integer, intent(in)   :: Smiss1, Sbad1, Smiss2, Sbad2
    integer, intent(out)  :: Smiss3, Sbad3
    integer, intent(inout):: superob, superob_s, npts3
    real,    intent(in)   :: miss
    real,    intent(in)   :: mom_lev(40)
    real,    intent(in)   :: temp1(npts1),  temp2(npts2)
    real,    intent(in)   :: depth1(npts1), depth2(npts2)
    real,    intent(in)   :: salt1(npts1),  salt2(npts2)
    real, intent(in)      :: tqc1(npts1),    tqc2(npts2)
    real, intent(in)      :: dqc1(npts1),    dqc2(npts2)
    real, intent(in)      :: sqc1(npts1),    sqc2(npts2)
    integer               :: i, j, i3
    real                  :: temp3a(npts3), salt3a(npts3)
    real                  :: tqc3a(npts3),  sqc3a(npts3)
    real                  :: temp3b(npts3), salt3b(npts3)
    real                  :: tqc3b(npts3),  sqc3b(npts3)

    real,  intent(inout)  :: dqc3(npts3), depth3(npts3)
    real,  intent(out)    :: temp_new(npts3) 
    real,  intent(out)    :: salt_new(npts3)
    real,  intent(out)    :: tqc_new(npts3),  sqc_new(npts3)
    real,  intent(out)    :: Trms, Srms

    Trms=0
    Srms=0
    !print *,'IN MODULE'
    ! Create and sort the new set of common depths
      !call sort_Z (depth1,dqc1,depth2,dqc2,depth3,dqc3,npts3,mom_lev)
       

      !print *, temp1(1:npts1)
      !print *, depth1(1:npts1)
      !print *, temp2(1:npts2)
      !print *, depth2(1:npts2)

      !print *, depth3(1:npts3)

      if ( npts3==0 ) then
           temp_new  = temp1
	   salt_new  = salt1
           npts3     = npts1
           superob   = 1
	   superob_s = 1
           return      
      endif
      
      !print *, 'Depth 1'
      !print *, depth1(1:npts1)
      !print *, 'Temp 1'
      !print *, temp1(1:npts1)

      !print *, 'Depth 2'
      !print *, depth2(1:npts2)
      !print *, 'Temp 2'
      !print *, temp2(1:npts2)

      !print *, 'Depth 3'
      !print *, depth3(1:npts3)

    ! Loop through new depths and interpolate for profile 1
 
      call interp_prf ( miss, npts1, depth1, temp1, salt1, tqc1, sqc1, &
                            npts3, depth3, temp3a, salt3a ,tqc3a, sqc3a)
          
           !print *, 'Temp 3a'
           !print *, temp3a(1:npts3)
           !print *, 'Tqc 3a'
           !print *, tqc3a(1:npts3)

           !print *, 'Salt 3a'
           !print *, salt3a(1:npts3)
           !print *, 'Sqc 3a'
           !print *, sqc3a(1:npts3)

    ! Loop through new depths and interpolate for profile 2 
      call interp_prf ( miss, npts2, depth2, temp2, salt2, tqc2, sqc2, &
                            npts3, depth3, temp3b, salt3b, tqc3b, sqc3b)
     
           !print *, 'Temp 3b'
           !print *, temp3b(1:npts3)
           !print *, 'Tqc 3b'
           !print *, tqc3b(1:npts3)

           !print *, 'Salt 3b'
           !print *, salt3b(1:npts3)
           !print *, 'Sqc 3b'
           !print *, sqc3b(1:npts3)

    ! Calculate RMS difference between the two profiles
    ! if Trms > 0.2  keep profile with the most points
    ! if Trms <= 0.2  superob the common depths
    ! if Srms <= 0.07 superob the common depths

      call get_rms ( miss, npts3, temp3a, temp3b, salt3a, salt3b, &
                          Trms, Srms )
      
      !print *, '   TRMS ', Trms, 'SRMS ', Srms, npts1, npts2, npts3

    ! If Trms <= 0.5, SUPEROB: average the two profiles at common depths
    if ( Trms <= Trms_lim ) then
      DO i=1,npts3
         temp_new(i) = (temp3a(i) + temp3b(i))/2
         tqc_new(i) = min(tqc3a(i),tqc3b(i))
      ENDDO
    
    ! If Srms <= 0.1
      if ( Srms <= Srms_lim ) then
         !print *, 'Ave S'
         DO i=1,npts3
          salt_new(i) = salt3a(i)
          sqc_new(i) = sqc3a(i)
          if  ( salt3a(i) /= miss .and. salt3b(i) /= miss ) then
                salt_new(i) = (salt3a(i) + salt3b(i))/2
                sqc_new(i) = min(sqc3a(i),sqc3b(i))
          elseif (salt3a(i) == miss .and. salt3b(i) /= miss ) then
                salt_new(i) = salt3b(i)
                sqc_new(i) = sqc3b(i)
          elseif (salt3a(i) /= miss .and. salt3b(i) == miss ) then
                salt_new(i) = salt3a(i)
                sqc_new(i) = sqc3a(i)
          endif
          ENDDO
       else
          !print *, 'No Ave S'
          !print *, Sbad1, Smiss1, Sbad2, Smiss2
          if ( Sbad1+Smiss1 < Sbad2+Smiss2 ) then
             !print *, '     Use Salt a'
             salt_new(1:npts3) = salt3a(1:npts3)
             sqc_new(1:npts3)  = sqc3a(1:npts3)
             superob_s = 1
          elseif ( Sbad1+Smiss1 > Sbad2+Smiss2 ) then
             !print *, '     Use Salt b'
             salt_new(1:npts3) = salt3b(1:npts3) 
             sqc_new(1:npts3)  = sqc3b(1:npts3)
             superob_s = 2          
          else
             if ( npts1 >= npts2 ) then
                !print *, '     Use Salt a'
                salt_new(1:npts3) = salt3a(1:npts3)
                sqc_new(1:npts3) = sqc3a(1:npts3)
                superob_s = 1
              else
                !print *, '     Use Salt b'
                salt_new(1:npts3) = salt3b(1:npts3)
                sqc_new(1:npts3) = sqc3b(1:npts3)
                superob_s = 2
              endif
          endif
       endif ! Srms

      !print *, 'Temp New'
      !print *, temp_new(1:npts3)
      !print *, 'TQC New'
      !print *, tqc_new(1:npts3)
      !print *, 'Salt New'
      !print *, salt_new(1:npts3)
      !print *, 'SQC New'
      !print *, sqc_new(1:npts3)
   
    else ! Too different to average
      !print *, 'T too diff ', npts1, npts2, npts3
      if ( npts1 >= npts2 ) then
           temp_new = temp1
           salt_new = salt1
           tqc_new  = tqc1
           sqc_new  = sqc1
           npts3    = npts1
           superob  = 1
           superob_s = 1
           !print *, 'Temp New 1'
           !print *, temp_new(1:npts1)
      elseif (npts1 < npts2 ) then
           temp_new = temp2
           salt_new = salt2
           tqc_new  = tqc2
           sqc_new  = sqc2
           npts3    = npts2
           superob  = 2
           superob_s = 2
          !print *, 'Temp New 2'
           !print *, temp_new(1:npts2)
      endif
    endif

    !
    Sbad3  = 0
    Smiss3 = 0
    DO i=1,npts3
         if ( sqc_new(i) == -1 ) then
            Sbad3 = Sbad3 + 1
         endif
         if ( sqc_new(i) == 9 ) then
            Smiss3 = Smiss3 + 1
         endif
    ENDDO

  return
  END SUBROUTINE superob_XYT


  ! ...................................................................
    SUBROUTINE get_rms ( miss, npts, temp1, temp2, salt1, salt2, &
                              Trms, Srms)
  ! ...................................................................
  ! Calculate point by point rms between two profiles
  !
    IMPLICIT NONE
    real, intent(in)   :: miss, temp1(150), temp2(150), salt1(150), salt2(150)
    real, intent(out)  :: Trms, Srms
    integer,intent(in) :: npts
    integer            :: i, notmiss
    real               :: mean_diff, diff(npts)
    real               :: rms_diff,  std(npts)
      
    !print *, 'GET_RMS'

    ! Temperature
    ! ...........
    mean_diff = 0
    DO i=1,npts
       diff(i) = temp1(i) - temp2(i)
       mean_diff = mean_diff + diff(i)
    ENDDO
    mean_diff = mean_diff/npts

    rms_diff = 0
    DO i=1,npts
       std(i) = (diff(i) - mean_diff)**2
       rms_diff = rms_diff + std(i)
    ENDDO
    rms_diff = rms_diff/npts
    Trms = sqrt(rms_diff)
    !print *, 'TRMS done'

    ! Salinity
    ! ........
    mean_diff = 0
    notmiss   = 0
    DO i=1,npts
       if ( salt1(i) < 999 .and. salt2(i) < 999 ) then
          diff(i) = salt1(i) - salt2(i)
          mean_diff = mean_diff + diff(i)
          notmiss = notmiss + 1
       endif
    ENDDO

    !print *, mean_diff, notmiss, npts

    if ( notmiss > 0 ) then
       mean_diff = mean_diff/notmiss
    endif

    if ( notmiss > 0 ) then
       rms_diff = 0
       DO i=1,notmiss
          std(i) = (diff(i) - mean_diff)**2
          rms_diff = rms_diff + std(i)
       ENDDO
       rms_diff = rms_diff/notmiss
       Srms = sqrt(rms_diff)
    else
       Srms = 9
    endif
    !print *, 'SRMS done'


  return
  END SUBROUTINE get_rms


  ! .................................................................................
    SUBROUTINE interp_prf ( miss, npts1,   depth1, temp1,  salt1, tqc1, sqc1, &
                                 npts3, depth3, temp3a, salt3a, tqc3a, sqc3a)
  ! .................................................................................
  ! Interpolate profile to new set of common depths
  !
    IMPLICIT NONE
    real,    intent(in)     :: miss
    integer, intent(in)     :: npts1,  npts3
    real,    intent(in)     :: temp1(npts1),  depth1(npts1), salt1(npts1)
    real,    intent(in)     :: tqc1(npts1),   sqc1(npts1)
    real,    intent(in)     :: depth3(npts3)
    real,    intent(out)    :: temp3a(npts3), salt3a(npts3)
    real,    intent(out)    :: tqc3a(npts3),  sqc3a(npts3)

    integer              :: i, j, done
    real                 :: lodepth, hidepth, lotemp, hitemp, losalt, hisalt
    integer              :: lotqc, hitqc, losqc, hisqc
    real                 :: dz, dt, ds
    integer              :: d_tqc, d_sqc
    real                 :: theta_t, theta_s, theta_tqc, theta_sqc
    real, parameter      :: miss_salt = 99

    !print *, 'INTERP_PRF'

    DO i = 1, npts3
        j       =  1
        done    =  0
        DO WHILE ( done==0 .and. j <= npts1 )
            if ( depth3(i) == depth1(j) ) then
                 temp3a(i) = temp1(j)
                 salt3a(i) = salt1(j)
                 tqc3a(i)  = tqc1(j)
                 sqc3a(i)  = sqc1(j)
                 done = 1
                 !print *, 'Equal      ', i, j, depth3(i), temp3a(i)

            elseif ( depth3(i) > depth1(j) ) then
                 lodepth = depth1(j)
                 lotemp  = temp1(j)
                 losalt  = salt1(j)
                 lotqc   = tqc1(j)
                 losqc   = sqc1(j)
                 !print *, 'Dont need', i, j, lodepth, lotemp, lotqc
            else
                 hidepth = depth1(j)
                 hitemp  = temp1(j)
                 hisalt  = salt1(j)
                 hitqc   = tqc1(j)
                 hisqc   = sqc1(j)
                 done    = 1

                 dz    = hidepth-lodepth
                 dt    = hitemp-lotemp
                 ds    = hisalt-losalt
                 d_tqc = hitqc-lotqc
                 d_sqc = hisqc-losqc
                 theta_t = atan(dt/dz)
                 theta_s = atan(ds/dz)

                 dz    = depth3(i) - lodepth
                 temp3a(i) = lotemp + tan(theta_t)*dz
                 tqc3a(i)  = min(lotqc,hitqc)

                 if ( losqc == 9 .or. hisqc == 9 ) then
                    salt3a(i) = miss
                    sqc3a(i)  = 9
                 else
                    salt3a(i) = losalt + tan(theta_s)*dz
                    sqc3a(i)  = min(losqc,hisqc)
                 endif

                 !print *, 'Interpolate', i, j, depth3(i), temp3a(i)                                 
            endif
            j = j + 1   
         ENDDO ! While not done
       ENDDO ! New Depths         

  return
  END SUBROUTINE interp_prf
            

  ! ..................................................................
    SUBROUTINE sort_Z (depth1,dqc1,depth2,dqc2,depth3,dqc3,npts3,mom_lev)
  ! ..................................................................
  ! Create a common set of descending depths
  !
    IMPLICIT NONE    
    real, intent(in)       :: depth1(npts3), depth2(npts3), mom_lev(40)
    real, intent(in)       :: dqc1(npts3),  dqc2(npts3)
    real, intent(inout)    :: depth3(npts3)
    real, intent(inout)    :: dqc3(npts3)
    integer, intent(inout) :: npts3
    real                   :: depth3_sort(npts3)
    real                   :: dqc3_sort(npts3)
    integer                :: npts1, npts2, npts3_sort
    integer                :: i, j, k
    real                   :: mindepth, maxdepth
       
    !print *, 'SORT_Z'

    npts1 = size(depth1)
    npts2 = size(depth2)
 
    if (depth1(1) > depth2(1)) then
        mindepth = depth1(1) 
    else
        mindepth =  depth2(1)
    endif

    if (depth1(npts1) < depth2(npts2)) then
        maxdepth = depth1(npts1)
    else
        maxdepth = depth2(npts2)
    endif

    i = 1
    k = 1
    DO WHILE ( i <= 40 )
       if ( mom_lev(i) >= mindepth .and. mom_lev(i) <= maxdepth ) then
          depth3(k) = mom_lev(i)
          dqc3(k)  = 1
          k = k + 1
       endif
       i = i + 1
    ENDDO
    npts3 = k - 1
    !print *, k
  
  return
  END SUBROUTINE sort_Z

  
 ! .................................................................................
    SUBROUTINE interp2mom ( miss, npts1,   depth1, temp1,  salt1, tqc1, sqc1, &
                                  npts3, depth3, temp3a, salt3a, tqc3a, sqc3a)
  ! .................................................................................
  ! Interpolate profile to new set of common depths on mom levels
  !
    IMPLICIT NONE
    real,    intent(in)     :: miss
    integer, intent(in)     :: npts1,  npts3
    real,    intent(in)     :: temp1(npts1),  depth1(npts1), salt1(npts1)
    real,    intent(in)     :: tqc1(npts1),   sqc1(npts1)
    real,    intent(in)     :: depth3(npts3)
    real,    intent(out)    :: temp3a(npts3), salt3a(npts3)
    real,    intent(out)    :: tqc3a(npts3),  sqc3a(npts3)

    integer              :: i, j, done
    real                 :: lodepth, hidepth, lotemp, hitemp, losalt, hisalt
    integer              :: lotqc, hitqc, losqc, hisqc
    real                 :: dz, dt, ds
    integer              :: d_tqc, d_sqc
    real                 :: theta_t, theta_s, theta_tqc, theta_sqc
    real, parameter      :: miss_salt = 99

    DO i = 1, npts3
        j       =  1
        done    =  0
        DO WHILE ( done==0 .and. j <= npts1 )
            !print *, i, j, depth3(i), depth1(j)
            if ( depth3(i) == depth1(j) ) then
                 temp3a(i) = temp1(j)
                 salt3a(i) = salt1(j)
                 tqc3a(i)  = tqc1(j)
                 sqc3a(i)  = sqc1(j)
                 done = 1
                 !print *, 'Equal      ', i, j, depth3(i),depth1(j)

            elseif ( depth3(i) > depth1(j) ) then
                 lodepth = depth1(j)
                 lotemp  = temp1(j)
                 losalt  = salt1(j)
                 lotqc   = tqc1(j)
                 losqc   = sqc1(j)
                 !print *, 'Dont need', i, j, lodepth
            else
                 hidepth = depth1(j)
                 hitemp  = temp1(j)
                 hisalt  = salt1(j)
                 hitqc   = tqc1(j)
                 hisqc   = sqc1(j)
                 done    = 1

                 dz    = hidepth-lodepth
                 dt    = hitemp-lotemp
                 ds    = hisalt-losalt
                 d_tqc = hitqc-lotqc
                 d_sqc = hisqc-losqc
                 theta_t = atan(dt/dz)
                 theta_s = atan(ds/dz)

                 dz    = depth3(i) - lodepth
                 temp3a(i) = lotemp + tan(theta_t)*dz
                 tqc3a(i)  = min(lotqc,hitqc)

                 if ( losqc == 9 .or. hisqc == 9 ) then
                    salt3a(i) = miss
                    sqc3a(i)  = 9
                 elseif ( losqc == -1 .or. hisqc == -1 ) then
                    salt3a(i) = miss
                    sqc3a(i)  = -1
                 else
                    salt3a(i) = losalt + tan(theta_s)*dz
                    sqc3a(i)  = min(losqc,hisqc)
                 endif
                 if (dz<=0) then
                   print *, 'INTERP ERROR:', i, j, lodepth, hidepth, dz   
                 endif                       
                 !print *, 'Interpolate', i, j, depth3(i), temp3a(i)                                 
            endif
            j = j + 1   
         ENDDO ! While not done
       ENDDO ! New Depths         

  return
  END SUBROUTINE interp2mom

END MODULE TS_SUPER_MODULE
		
