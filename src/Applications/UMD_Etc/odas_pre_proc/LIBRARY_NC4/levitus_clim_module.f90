MODULE LEVITUS_CLIM_MODULE
  
  IMPLICIT NONE
     
  PUBLIC :: get_lev_data, interp_lev_data, get_rms
  
  CONTAINS


  ! ...............................................................
    SUBROUTINE get_lev_data (PrfLon, PrfLat, month, kdim_good, &
                             LevData, LevDepth, svar)             
       
  ! ...............................................................
  ! Open and read levitus grid file
  ! Get closest coordinates (im,jm)
  ! Read levitus data for im, jm and month
  !
    IMPLICIT NONE
    integer, parameter      :: idim = 360, jdim = 180, kdim = 33
    real,        intent(in) :: PrfLon, PrfLat
    integer,     intent(in) :: month
    character*4, intent(in) :: svar
    real,        intent(out):: LevDepth(kdim), LevData(kdim)
    integer,     intent(out):: kdim_good

    real                    :: LevLon(idim),   LevLat(jdim)
    integer, parameter      :: UNIT_LEV = 10
    character*256           :: fname_lev
    character*3             :: mmm
    integer                 :: im, jm
    real                    :: minlon0, minlon1, minlat0, minlat1, PrfLonRot    
    real                    :: LData(idim,jdim)     
    integer                 :: i, j, k
    real, parameter         :: levmiss = -99.9                
        
	if (month==1) then
	   mmm = 'jan'
	elseif (month==2) then
	   mmm = 'feb'
	elseif (month==3) then
	   mmm = 'mar'
	elseif (month==4) then
	   mmm = 'apr'
	elseif (month==5) then
	   mmm = 'may'
	elseif (month==6) then
	   mmm = 'jun'
	elseif (month==7) then
	   mmm = 'jul'
	elseif (month==8) then
	   mmm = 'aug'
	elseif (month==9) then
	   mmm = 'sep'
	elseif (month==10) then
	   mmm = 'oct'
	elseif (month==11) then
	   mmm = 'nov'
	elseif (month==12) then
	   mmm = 'dec'
	endif

    fname_lev = '/discover/nobackup/gmaofcst/ocean/obs/odas-2/LEVITUS/levitus.grid.ieee'  
    open  (UNIT_LEV, file=fname_lev(1:len_trim(fname_lev)), status='old', &
                     form='unformatted', convert='big_endian') 
    read  (UNIT_LEV) LevLon, LevLat, LevDepth  
    close (UNIT_LEV)


  ! Get closest coordinates (im,jm)
  ! ...............................
    if ( PrfLon < 0 ) then
       PrfLonRot = PrfLon+360
    else
       PrfLonRot = PrfLon    
    endif
    !print *, PrfLonRot
    
    im = 1
    do while ( PrfLonRot > LevLon(im) )
        if (PrfLonRot >= maxval(LevLon)) then
            im = idim 
            exit
        endif
        im = im + 1 
        minlon0 = abs(PrfLonRot-LevLon(im-1))
        minlon1 = abs(PrfLonRot-LevLon(im))
    enddo
    if (minlon0 < minlon1 ) im = im -1                            
    
    
    jm = 1
    do while ( PrfLat > LevLat(jm) ) 
       if (PrfLat >= maxval(LevLat)) then
           jm = jdim 
           exit
       endif
       jm = jm + 1   
       minlat0 = abs(PrfLat-LevLat(jm-1))
       minlat1 = abs(PrfLat-LevLat(jm))                               
    enddo
    if (minlat0 < minlat1 ) jm = jm -1 
    
    !print *, 'PrfLon', PrfLonRot,  'PrfLat', PrfLat
    !print *, 'LevLon', LevLon(im), 'LevLat', LevLat(jm)
    

  ! Read levitus data for im, jm and month
  ! .......................................    
    if (svar=='TEMP') then
      !fname_lev = '/discover/nobackup/gmaofcst/ocean/obs/odas-2/LEVITUS/2009/Data/temp/temp.' // mmm
      fname_lev = '/discover/nobackup/gmaofcst/ocean/obs/odas-2/LEVITUS/2009/Data/ptemp/ptemp.' // mmm
    elseif (svar=='SALT') then
      fname_lev = '/discover/nobackup/gmaofcst/ocean/obs/odas-2/LEVITUS/2009/Data/salt/salt.' // mmm
    else
      print *, 'ERROR: svar not specified'
      stop
    endif
    open(UNIT_LEV, file=fname_lev(1:len_trim(fname_lev)),form='unformatted', status='old')      
   
    do k=1,kdim
       read (UNIT_LEV) LData 
       LevData(k) = LData(im,jm)
       if ( LData(im,jm) <= levmiss+1 ) then
            kdim_good = k - 1
            exit
       endif
       kdim_good = k        
    enddo
    close(UNIT_LEV)
    !print *, kdim_good
    !print *, LevData(1:kdim_good)
    !print *, LevDepth(1:kdim_good)

  return
  END SUBROUTINE get_lev_data

  
   
  ! ...............................................................
  SUBROUTINE interp_lev_data(Ldim,Ldata,Ldepth,Adim,Adata,Adepth,&
                        Ldata_interp,max_ds,max_dz,intdim,Trms,Zlim)
  ! ...............................................................
  ! Find two closest Levitus points for each Point
  ! Interp to find matching data on Levitus Curve
  ! Report on new Data, maxdiff from Profile, and depth of maxdiff
    IMPLICIT NONE
    
    integer, intent(in)  :: Ldim
    integer, intent(in)  :: Adim
    real,    intent(in)  :: Ldata(Ldim),Ldepth(Ldim)
    real,    intent(in)  :: Adata(Adim),Adepth(Adim)
    real,    intent(out) :: Ldata_interp(Adim), max_ds, max_dz, Trms
    integer, intent(out) :: intdim
    integer              :: i, j, minjj(2), inc,  minj
    real                 :: dz(Ldim), angj, salj, diff(Adim)
    real                 :: maxA, maxL
    real, intent(out)    :: Zlim ! Maximum depth of Levitus/Profile Data
    
    diff = 0
    maxL = maxval(Ldepth)
    maxA = maxval(Adepth)
    !print *, maxL, maxA, Ldim, Adim
    
    i = 1
    !do i=1,Adim
    do while (Adepth(i) <= maxL .and. i<=Adim)
       do j=1,Ldim 
          dz(j) = abs(Adepth(i) - Ldepth(j))
       enddo
       minjj = minloc(dz(:),1)
       minj = minjj(1)
       if ( Adepth(i) - Ldepth(minj) < 0 ) then
          inc = -1
       else
          inc = 1
       endif
       angj = atan(abs(Ldepth(minj)-Ldepth(minj+inc))/ &
              abs(Ldata(minj)-Ldata(minj+inc)))
       salj = abs(Adepth(i)-Ldepth(minj+inc))/tan(angj)
       if ( Ldata(minj) - Ldata(minj+inc) < 0 ) then
          Ldata_interp(i) = Ldata(minj+inc) - salj
       else
          Ldata_interp(i) = Ldata(minj+inc) + salj       
       endif
       diff(i) = abs(Adata(i) - Ldata_interp(i))
       !print *, i, Adepth(i), maxL, diff(i)
       i = i + 1
    enddo
    intdim = i-1    
    max_ds = maxval(diff(:))
    minjj  = maxloc(diff(:),1)
    max_dz = Adepth(minjj(1))

!print *, intdim
!print *, Adata(1:intdim)
!print *, Ldata_interp(1:intdim)

    call get_rms(intdim,Adata,Ldata_interp,Trms)
    Zlim = Adepth(intdim)

    !print *, '   IN  MOD', max_ds, max_dz, intdim, Trms
    
  return
  END SUBROUTINE interp_lev_data 
    
  ! ...................................................................
    SUBROUTINE get_rms ( npts, temp1, temp2, Trms)
  ! ...................................................................
  ! Calculate point by point rms between two profiles
  !
    IMPLICIT NONE
    real, intent(in)   :: temp1(:), temp2(:)
    real, intent(out)  :: Trms
    integer,intent(in) :: npts
    integer            :: i, notmiss
    real               :: mean_diff, diff(npts)
    real               :: rms_diff,  std(npts)
    real               :: tmp
      
    ! Temperature
    ! ...........
    mean_diff = 0
    tmp = 0
    DO i=1,npts
       tmp = (temp1(i) - temp2(i))**2 + tmp
    ENDDO
    Trms = sqrt(tmp/npts)


  return
  END SUBROUTINE get_rms

END MODULE LEVITUS_CLIM_MODULE
		
