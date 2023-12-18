MODULE SUPER_MODULE

  IMPLICIT NONE
      
  PUBLIC :: superob_XYT, get_rms, interp_prf, sort_Z, interp2mom
  
  CONTAINS
 

  ! .........................................................
    SUBROUTINE superob_XYT ( miss, npts1, npts2, npts3,    &
                             temp1, temp2, depth1, depth2, &
                             depth3, temp_new, superob, rms)
  ! .........................................................
  ! If positions of two profiles are too close and the rms is
  ! close enough, average them.
  ! If the rms is too large, accept profile with most points
  !
    IMPLICIT NONE
    integer, intent(in)   :: npts1, npts2
    integer, intent(inout):: superob, npts3
    real,    intent(in)   :: miss
    real,    intent(in)   :: temp1(npts1),  temp2(npts2)
    real,    intent(in)   :: depth1(npts1), depth2(npts2)
  
    integer               :: i, i3
    real                  :: temp3a(npts3), temp3b(npts3)  
    real,     intent(out) :: depth3(npts3), temp_new(npts3)
    real,     intent(out) :: rms
    superob = 0

    ! Create and sort the new set of common depths           
      call sort_Z (depth1,depth2,depth3,npts3)
      
      !print *, npts1,' npts1', npts2, ' npts2', npts3, ' npts3'
      
      if ( npts3==0 ) then
           temp_new = temp1
           npts3    = npts1
           superob  = 1
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
 
      call interp_prf ( miss, npts1, depth1, temp1,  &
                            npts3, depth3, temp3a)         
          
           !print *, 'Temp 3a'
           !print *, temp3a(1:npts3)
   
    ! Loop through new depths and interpolate for profile 2 
      call interp_prf ( miss, npts2, depth2, temp2,  &
                            npts3, depth3, temp3b)  
     
           !print *, 'Temp 3b'
           !print *, temp3b(1:npts3)         

    ! Calculate RMS difference between the two profiles
    ! if rms > 0.4 use profile with the most points
    ! if rms <= 0.4 only superob the common depths
      call get_rms ( miss, npts3, temp3a, temp3b, rms )
      
      !print *, 'RMS', rms   

    ! If rms <= 0.4, average the two profiles at common depths
    if ( rms <= 0.4 ) then
      DO i=1,npts3
         if  ( temp3a(i) /= miss .and. temp3b(i) /= miss ) then
             temp_new(i) = (temp3a(i) + temp3b(i))/2
         elseif (temp3a(i) == miss .and. temp3b(i) /= miss ) then
             temp_new(i) = temp3b(i)
         elseif (temp3a(i) /= miss .and. temp3b(i) == miss ) then
             temp_new(i) = temp3a(i)
         else
             temp_new(i) = miss
         endif

       ENDDO
      !print *, 'Temp New'
      !print *, temp_new(1:npts3)
        
    else
      if ( npts1 >= npts2 ) then
           temp_new = temp1
           npts3    = npts1
           superob  = 1
           !print *, 'Temp New 1'
           !print *, temp_new(1:npts1)
      elseif (npts1 < npts2 ) then
           temp_new = temp2
           npts3    = npts2
           superob  = 2
           !print *, 'Temp New 2'
           !print *, temp_new(1:npts2)
      endif     
    endif

  return
  END SUBROUTINE superob_XYT


  ! ...................................................
    SUBROUTINE get_rms ( miss, npts, temp1, temp2, rms)                     
  ! ...................................................
  ! Calculate point by point rms between two profiles
  !
    IMPLICIT NONE
    real, intent(in)   :: miss, temp1(:), temp2(:)
    real, intent(out)  :: rms
    integer,intent(in) :: npts
    integer            :: i
    real               :: mean_diff, diff(npts)
    real               :: rms_diff,  std(npts)
      
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
    rms = sqrt(rms_diff)
 
  return
  END SUBROUTINE get_rms


  ! ....................................................
    SUBROUTINE interp_prf ( miss, npts1, depth1, temp1,  &
                                npts3, depth3, temp3a)                     
  ! ....................................................
  ! Interpolate profile to new set of common depths
  !
    IMPLICIT NONE
    real,    intent(in)  :: miss
    integer, intent(in)  :: npts1,  npts3
    real,    intent(in)  :: temp1(npts1),  depth1(npts1)
    real,    intent(in)  :: depth3(npts3)
    real,    intent(out) :: temp3a(npts3)

    integer              :: i, j, done
    real                 :: lodepth, hidepth, lotemp, hitemp
    real                 :: dz, dt
    real                 :: theta_t

    DO i = 1, npts3
        j       =  1
        done    =  0
        DO WHILE ( done==0 .and. j <= npts1 )
            if ( depth3(i) == depth1(j) ) then
                 temp3a(i) = temp1(j)
                 done = 1
                 !print *, 'Equal      ', i, j, depth3(i), temp3a(i)

            elseif ( depth3(i) > depth1(j) ) then
                 lodepth = depth1(j)
                 lotemp  = temp1(j)
                 !print *, 'Dont need', i, j, lodepth, lotemp, lotqc
            else
                 hidepth = depth1(j)
                 hitemp  = temp1(j)
                  done    = 1

                 dz    = hidepth-lodepth
                 dt    = hitemp-lotemp
                 theta_t = atan(dt/dz)
                                  
                 dz    = depth3(i) - lodepth
                 temp3a(i) = lotemp + tan(theta_t)*dz
                 !print *, 'Interpolate', i, j, depth3(i), temp3a(i)                                 
            endif
            j = j + 1   
         ENDDO ! While not done
       ENDDO ! New Depths         

  return
  END SUBROUTINE interp_prf
            

  ! ..............................................
    SUBROUTINE sort_Z (depth1,depth2,depth3,npts3)    
  ! ..............................................
  ! Create a common set of descending depths
  !
    IMPLICIT NONE    
    real, intent(in)       :: depth1(:), depth2(:)
    real, intent(out)      :: depth3(:)
    integer, intent(inout) :: npts3
    real                   :: depth3_sort(npts3)
    integer                :: npts1, npts2, npts3_sort
    integer                :: i, j, k
    real                   :: mindepth, maxdepth
       
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
    j = 1
    k = 1
   
    DO WHILE ( i <= npts1 .and. j <= npts2 )
       if ( depth1(i) == depth2(j) ) then
            depth3_sort(k) = depth1(i)
            i = i + 1
            j = j + 1
       elseif ( depth1(i) < depth2(j) ) then
            depth3_sort(k) = depth1(i)
            i = i + 1
       else
            depth3_sort(k) = depth2(j)
            j = j + 1
       endif
       k = k + 1
    ENDDO
    npts3_sort = k - 1
    
    i = 1
    k = 1
    DO WHILE ( i<= npts3_sort )
       if ( depth3_sort(i) >= mindepth .and. depth3_sort(i) <= maxdepth ) then
          depth3(k) = depth3_sort(i)
          k = k + 1
       endif
       i = i + 1
    ENDDO
    npts3 = k - 1
    
  return
  END SUBROUTINE sort_Z

 ! .................................................................................
    SUBROUTINE interp2mom ( miss, npts1,   depth1, temp1,  tqc1, &
                                  npts3, depth3, temp3a, tqc3a)
  ! .................................................................................
  ! Interpolate profile to new set of common depths
  !
    IMPLICIT NONE
    real,    intent(in)     :: miss
    integer, intent(in)     :: npts1,  npts3
    real,    intent(in)     :: temp1(npts1),  depth1(npts1)
    real,    intent(in)     :: tqc1(npts1)
    real,    intent(in)     :: depth3(npts3)
    real,    intent(out)    :: temp3a(npts3)
    real,    intent(out)    :: tqc3a(npts3)

    integer              :: i, j, done
    real                 :: lodepth, hidepth, lotemp, hitemp
    integer              :: lotqc, hitqc
    real                 :: dz, dt
    integer              :: d_tqc
    real                 :: theta_t,  theta_tqc

    !print *, 'INTERP_PRF'

    DO i = 1, npts3
        j       =  1
        done    =  0
        DO WHILE ( done==0 .and. j <= npts1 )
            if ( depth3(i) == depth1(j) ) then
                 temp3a(i) = temp1(j)
                 tqc3a(i)  = tqc1(j)
                 done = 1
                 !print *, 'Equal      ', i, j, depth3(i), temp3a(i)

            elseif ( depth3(i) > depth1(j) ) then
                 lodepth = depth1(j)
                 lotemp  = temp1(j)
                 lotqc   = tqc1(j)
                 !print *, 'Dont need', i, j, lodepth, lotemp, lotqc
            else
                 hidepth = depth1(j)
                 hitemp  = temp1(j)
                 hitqc   = tqc1(j)
                 done    = 1

                 dz    = hidepth-lodepth
                 dt    = hitemp-lotemp
                 d_tqc = hitqc-lotqc
                 theta_t = atan(dt/dz)

                 dz    = depth3(i) - lodepth
                 temp3a(i) = lotemp + tan(theta_t)*dz
                 tqc3a(i)  = min(lotqc,hitqc)

                 !print *, 'Interpolate', i, j, depth3(i), temp3a(i)                                 
            endif
            j = j + 1   
         ENDDO ! While not done
       ENDDO ! New Depths         

  return
  END SUBROUTINE interp2mom

END MODULE SUPER_MODULE
		
