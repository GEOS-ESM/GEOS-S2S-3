MODULE TS_MEAN_MODULE

  IMPLICIT NONE
     
  PUBLIC :: mean_XYT, interp_prf
  CONTAINS



   ! ...................................................................
    SUBROUTINE mean_XYT (ndup, miss, npts, temp, salt, depth, tqc, sqc, dqc, &
                temp_new, salt_new,  tqc_new,  sqc_new)
  ! ...................................................................
  ! If the rms is close enough, average them.
  ! If the rms is too large, accept profile with most points
  !
    IMPLICIT NONE
    integer, intent(in)   :: ndup
    integer, intent(in), dimension(ndup)   :: npts
    real,    intent(in)   :: miss
    real,    intent(in)   :: temp(:,:)
    real,    intent(in)   :: depth(:,:)
    real,    intent(in)   :: salt(:,:)
    real,    intent(in)   :: tqc(:,:)
    real,    intent(in)   :: dqc(:,:)
    real,    intent(in)   :: sqc(:,:)
    integer               :: i, j, i3

    real                  :: temp3(npts(1),ndup), salt3(npts(1),ndup)
    real                  :: tqc3(npts(1),ndup),  sqc3(npts(1),ndup)

    real,  intent(out)    :: temp_new(npts(1)), salt_new(npts(1))
    real,  intent(out)    :: tqc_new(npts(1)),  sqc_new(npts(1))
    
    DO i=1,ndup
      call interp_prf ( ndup, miss, npts(i), depth(1:npts(i),i), temp(1:npts(i),i), salt(1:npts(i),i), &
                        tqc(1:npts(i),i),  sqc(1:npts(i),i),  npts(1), &
                        depth(1:npts(1),1),  temp3(:,i), salt3(:,i) ,tqc3(:,i), sqc3(:,i) )
    ENDDO

    
    DO j=1,npts(1)
      if (maxval(temp3(j,:)) < miss) then
        temp_new(j) = sum(temp3(j,:))/ndup
      else
        print *, 'T miss'
      endif
      if (maxval(salt3(j,:)) < miss) then
        salt_new(j) = sum(salt3(j,:))/ndup
      else
        print *, 'S miss'
      endif
      if (maxval(sqc3(j,:)) == 1 .and. minval(sqc3(j,:)) == 1) then
        sqc_new(j) = sum(sqc3(j,:))/ndup
      else
        print *, 'SQC miss'
      endif
      if (maxval(tqc3(j,:)) == 1 .and. minval(tqc3(j,:)) == 1) then
        tqc_new(j) = sum(tqc3(j,:))/ndup
      else
        print *, 'TQC miss'
      endif

    ENDDO
    !print *, salt_new


  return
  END SUBROUTINE mean_XYT


  ! .................................................................................
    SUBROUTINE interp_prf ( ndup, miss, npts1, depth1, temp1, salt1, tqc1, sqc1, &
                            npts3, depth3, temp3a, salt3a, tqc3a, sqc3a)
  ! .................................................................................
  ! Interpolate profile to new set of common depths
  !
    IMPLICIT NONE
    real,    intent(in)     :: miss
    integer, intent(in)     :: ndup,   npts3
    integer, intent(in)     :: npts1
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

    !print *, 'interp_prf'

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
  

END MODULE TS_MEAN_MODULE
		
