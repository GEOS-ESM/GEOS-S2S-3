MODULE SYN_SALT_MODULE
  
  IMPLICIT NONE
     
  PUBLIC :: iterate_pden, find_dinv, get_pden_profile, find_tinv, get_pressure
  PUBLIC :: get_levitus_pos, read_levitus, ts_pairs
  
  CONTAINS

  ! ..............................................
  SUBROUTINE iterate_pden(npts,temp,depth,pnpts,pden,out_three)
  ! ..............................................
   IMPLICIT NONE 
   integer, intent(inout)  :: npts
   real,    intent(inout)  :: temp(npts),depth(npts)
   real,    intent(inout)  :: pnpts(npts),pden(npts)  
   integer, intent(inout)  :: out_three
   
   real*4, parameter       :: max_T     = 50,   min_T = -40
   
   integer                 :: out(npts)
   real                    :: delta_rho
   real, parameter         :: delta_rho_max = 0.01   
   integer                 :: j, k, lastj, iterate, nDinv, newj
   integer                 :: badNPTS
   integer                 :: newNPTS
   real                    :: newTEMP(npts), newPDEN(npts)
   real                    :: newDEPTH(npts), newPNPTS(npts)
   
   k         = 1
   lastj     = 1
   iterate   = 1
   badNPTS   = 0
   newNPTS   = 0
   nDinv     = 0 
   
   !print *, pden(1:npts)
      
   DO WHILE ( iterate == 1 .or. out_three == 0 )
      
      ! ................................................................
      DO j=2,npts
      ! ................................................................   
         delta_rho = pden(j) - pden(j-1) 
         
         IF ( delta_rho < 0 .and. abs(delta_rho) > delta_rho_max ) then
              
              !print *, i,j,k
              nDinv = nDinv + 1

              if ( j-lastj == 2 ) then
                   out(k)   = j
                   out(k+1) = j+1
                 
                   if (j /= npts) then
                       out(k+2) = j+1
                       k = k + 2
                   else
                       k = k + 1
                   endif                                           
     
              elseif ( j-lastj == 1 ) then

                   if (j /= npts) then
                       out(k) = j+1
                       k =  k + 1
                   else
                       k = k
                   endif


              else
                   out(k)   = j-1
                   out(k+1) = j 
             
                   if (j /= npts) then
                       out(k+2) = j+1
                       k = k + 3
                   else
                       k = k + 2
                   endif                                           
              endif
                                          
              lastj = j
  
         ENDIF ! inversion         
      ENDDO ! npts

      
      ! ................................................................
      ! ................................................................
      
      ! If there are 3 inversions, throw out observation
      if (nDinv >= 3) then
          out_three = 1
          !print *, '          Three inversions'
          exit
      endif 
            
      if ( j == npts+1 .and. k == 1 ) then
           !print *, '          No more inversions'
           exit
      endif
      
      badNPTS = k-1
      newNPTS = npts - (k-1)
      
      !print *, '  newNPTS', newNPTS
      !print *, '  badNPTS', badNPTS
      
      ! ................................................................
      newj = 1      
      DO j=1,npts
         DO k=1,badNPTS
            if ( j/= out(k) ) then
                 newTEMP(newj)  = TEMP(j)
                 newPDEN(newj)  = PDEN(j)
                 newDEPTH(newj) = DEPTH(j)
                 newPNPTS(newj) = PNPTS(j)
                 
                 if (newTemp(newj) > max_T) then 
                    print *, 'ERROR: T = ',newTemp(newj) 
                    stop
                 endif
                if (newTemp(newj) < min_T) then 
                    print *, 'ERROR: T = ',newTemp(newj) 
                    stop
                 endif

            else
                 newj = newj - 1
                 exit
            endif         
         
         ENDDO ! badNPTS
         newj = newj + 1
      
      ENDDO ! npts

      k     = 1
      lastj = 1
      
      NPTS  = newNPTS
      PDEN  = newPDEN(1:NPTS)
      TEMP  = newTEMP(1:NPTS)
      DEPTH = newDEPTH(1:NPTS)
      PNPTS = newPNPTS(1:NPTS)
      
      !print *, 'Iterate'
      !print *, PDEN(1:NPTS)
      !print *, ' '
       
   ENDDO ! iterate
   

  return
  end subroutine iterate_pden

  ! ..............................................
  SUBROUTINE find_dinv(npts,pden,ndinv)
  ! ..............................................
    IMPLICIT NONE
    integer, intent(in)  :: npts
    real,    intent(in)  :: pden(npts)
    integer, intent(out) :: ndinv
    integer              :: j
    real                 :: delta_rho
    real, parameter      :: delta_rho_max = 0.01
    
    ! Just find first inversion (if any)
    ndinv = 0
    DO j=2,npts       
       delta_rho = pden(j) - pden(j-1)       
       if ( delta_rho < 0 .and. abs(delta_rho) > delta_rho_max ) then
            ndinv = 1
            exit
       endif    
    ENDDO

  return
  end subroutine find_dinv

  ! ..............................................
  SUBROUTINE get_pden_profile(lon,lat,npts,temp,salt,depth,month_name, &
                              Llon,Llat,Ldepth,idim,jdim,kdim,pden,kdim_good)
  ! ..............................................
    IMPLICIT NONE
    integer,     intent(in)  :: npts
    real,        intent(in)  :: lon, lat, temp(npts),depth(npts)
    integer,     intent(in)  :: idim, jdim, kdim
    real,        intent(in)  :: Llon(idim), Llat(jdim), Ldepth(kdim)    
    character*3, intent(in)  :: month_name
    
    real,        intent(out) :: pden(npts),salt(npts)
    integer                  :: kdim_good
    
    integer                  :: j, im, jm
    real                     :: Ltemp(kdim), Lpotemp(kdim), Lsalt(kdim)
    real                     :: press(npts)
    real                     :: ptemp(npts)
    real, parameter          :: REF_DENSITY = 30.0, g = 9.7976
    real, parameter          :: rho_0 = 1000 + REF_DENSITY
  
  
    ! Find Closest Lat/Lon Grid Points in Levitus
    ! Llon(im),Llat(jm)
    ! ................................................
      call get_levitus_pos (lon,lat,Llon,Llat,idim,jdim,im,jm)
                                                                    
           !print *, 'Profile Position ', lat,lon
           !print *, 'Levitus Position ', Llat(jm),Llon(im)
                  
             
    ! Read Levitus Temp, Potential Temp and Salinity Data
    ! ...................................................
      call read_levitus (month_name,idim,jdim,kdim, &
                         im,jm,Ltemp,Lpotemp,Lsalt,kdim_good)
           
           if ( kdim_good <= 2 ) then 
                pden = 0 
                !print *, 'Only ', kdim_good, 'Good Levitus Levels'          
           else                               
                !print *, kdim_good, 'Good Levitus Levels to', Ldepth(kdim_good), 'meters'                    
                !print *, npts,      'Good Profile Levels to', depth(npts),       'meters'                    
           endif
   
      if ( kdim_good > 2 ) then   
      ! Find T-S Pairs
      ! Use profile temp to find corresponding Levitus Temp
      ! Get corresponding Depth and Levitus Salt
      ! ................................................... 
        call ts_pairs (temp,depth,npts,lat, &
                       Ltemp(1:kdim_good),Lsalt(1:kdim_good), &
                       Ldepth(1:kdim_good),kdim_good,salt)                             
           
     
      endif  ! if Levitus Data
  return
  end subroutine  get_pden_profile 

 
  ! ..............................................
  SUBROUTINE find_tinv(npts,temp,ntinv)
  ! ..............................................
    IMPLICIT NONE
    integer, intent(in)  :: npts
    real,    intent(in)  :: temp(npts)
    integer, intent(out) :: ntinv
    integer              :: j
    real                 :: delta_t
    real, parameter      :: delta_t_max = 0.1
    
    ! Just find first inversion (if any)
    ntinv = 0
    DO j=2,npts       
       delta_t = temp(j-1) - temp(j)       
       if ( delta_t < 0 .and. abs(delta_t) > delta_t_max ) then
            ntinv = 1
            exit
       endif    
    ENDDO

  return
  end subroutine find_tinv
  
  
  ! ..............................................
  SUBROUTINE get_pressure(depth,lat,npts,pressure)
  ! ..............................................
    IMPLICIT NONE
    integer, intent(in)  :: npts
    real,    intent(in)  :: depth(npts), lat
    real,    intent(out) :: pressure(npts)     
    real                 :: pi, deg2rad, x, c1    
 
    ! From MATLAB Seawater Mfile, sw_pres
    pi       = 4*atan(1.)  
    deg2rad  = pi/180
    x        = sin(abs(lat)*deg2rad)
    c1       = 5.92E-3 + ((x**2)*5.25E-3)
    pressure = ((1-c1)-sqrt(((1-c1)**2) - (8.8E-6*depth)))/4.42E-6
     
  return
  end subroutine get_pressure
 
 
  ! ..............................................................
  SUBROUTINE get_levitus_pos (Plon,Plat,Llon,Llat,idim,jdim,im,jm)
  ! ..............................................................  
    IMPLICIT NONE
    real, intent(in)     :: Plon,Plat
    integer, intent(in)  :: idim, jdim
    real, intent(in)     :: Llon(idim), Llat(jdim)
    integer, intent(out) :: im, jm
    real                 :: minlon0,minlon1,minlat0,minlat1
    
    !print *, Plon, Plat
    !print *, Llon
    !print *, ' '
    !print *, Llat
    
    im = 1
    do while ( Plon > Llon(im) )
               if (Plon >= maxval(Llon)) then
                   im = idim 
                   exit
               endif
               im = im + 1 
               minlon0 = abs(Plon-Llon(im-1))
               minlon1 = abs(Plon-Llon(im))
    enddo
    if (minlon0 < minlon1 ) im = im -1                            
    
    jm = 1
    do while ( Plat > Llat(jm) ) 
               if (Plat >= maxval(Llat)) then
                   jm = jdim 
                   exit
               endif
               
               jm = jm + 1   
               minlat0 = abs(Plat-Llat(jm-1))
               minlat1 = abs(Plat-Llat(jm))                               
    enddo
    if (minlat0 < minlat1 ) jm = jm -1 
    
    !print*, Llon(im), Llat(jm)
    
    
  return
  end subroutine get_levitus_pos


  ! .................................................................
  SUBROUTINE read_levitus (month_name,idim,jdim,kdim,im,jm, &
                           Ltemp,Lpotemp,Lsalt,kdim_good)
  ! ................................................................. 
    IMPLICIT NONE
    character*3, intent(in)  :: month_name
    integer,     intent(in)  :: idim, jdim, kdim, im, jm
    real,        intent(out) :: Ltemp(kdim), Lpotemp(kdim), Lsalt(kdim)
    integer                  :: kdim_good
    
    character*256            :: filedir, filename, fname
    character*76             :: fname_trim
    integer                  :: len, lendir, k, UNIT=10
    real                     :: potemp(idim,jdim), temp(idim,jdim), salt(idim,jdim)
    real, parameter          :: levmiss = -99.9    
    
!    filedir  = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/LEVITUS/2009/Data/'
    filedir  = '/gpfsm/dnb05/projects/p23/lren1/LEVITUS/2018/Data/'
 
    lendir     = len_trim(filedir)
    filename   = filedir(1:lendir) // 'temp/temp.' // month_name
    len        = len_trim(filename)
    !print *, 'lendir ',lendir
    !print *, 'filename ',filename(1:len)
    !print *, 'len ',len

    !fname      = '/discover/nobackup/gmaofcst/ocean/obs/odas-2/LEVITUS/2009/Data/temp/temp.' // month_name
    !fname_trim = adjustr(fname)
    !print *, 'fname_trim ', fname_trim
    !write(6,*)'in read_levitus',filename,month_name
    open(UNIT, file=filename(1:len),form='unformatted', status='old')        
    !open(UNIT, file=fname_trim,form='unformatted', status='old')  
    !print *, 'ok'
    !stop

    do k=1,kdim
       read (UNIT) potemp 
       Lpotemp(k) = potemp(im,jm) 
       !print *, k,  potemp(im,jm)
       if ( potemp(im,jm) == levmiss ) then
            kdim_good = k - 1
            exit
       endif
       kdim_good = k 
    enddo
    close(UNIT)
    !print *, kdim_good

      
    filename = filedir(1:lendir) // 'temp/temp.' // month_name
    fname_trim = adjustr(filename)
    len      = len_trim(filename)   
    !open(UNIT, file=fname_trim,form='unformatted', status='old')          
    open(UNIT, file=filename(1:len),form='unformatted', status='old')        
    do k=1,kdim_good
       read (UNIT) temp 
       Ltemp(k) = temp(im,jm)
    enddo
    close(UNIT)

    filename = filedir(1:lendir) // 'salt/salt.' // month_name
    fname_trim = adjustr(filename)
    len      = len_trim(filename)  
    !open(UNIT, file=fname_trim,form='unformatted', status='old')          
    open(UNIT, file=filename(1:len),form='unformatted', status='old')        
    do k=1,kdim_good
       read (UNIT) salt 
       Lsalt(k) = salt(im,jm)
    enddo
    close(UNIT)
     
 
  return    
  end subroutine read_levitus   


  ! .................................................................
  SUBROUTINE ts_pairs (Ptemp,Pdepth,Pdim,Plat, &
                       Ltemp,Lsalt,Ldepth,Ldim,S)
  ! .................................................................                            
  ! Ptemp : Profile Temp
  ! Ltemp : Levitus Temp
   
    IMPLICIT NONE
    integer, intent(in)  :: Pdim, Ldim
    real,    intent(in)  :: Ptemp(Pdim), Pdepth(Pdim),Plat
    real,    intent(in)  :: Ltemp(Ldim), Lsalt(Ldim), Ldepth(Ldim)
    real,    intent(out) :: S(Pdim)
    integer              :: i, j, dt, inv
    integer              :: mint, maxt
    integer              :: minz, maxz,ij,nl(20),inv_pt,inv_lt
    real                 :: deltat(Ldim), deltaz(Ldim),Tdiff(Ldim)
    real                 :: theta, zmatch, smatch,theta1
    real                 :: z0(2), zmin, zmin1, zmin2,Pdmin,Pdmax
    real 		 :: delt, deltatmin
    real                 :: delt1, delt2,deltz,mindz,znk,znj
    integer 		 :: nj, nk,jlev,minkz,jdepth ,jd(2),j1,j2,j0(2)
    real 	         :: z1,z2,deltmin,delz1,delz2,delz,zp,dels,dT1,dT2
    real 		 :: dtmin=0.2
    real 		 :: dzmax=200.
    logical 	         :: mask(Ldim)
    !real                  :: lat_lim1=-30.
    !real 		  :: lat_lim2=30.
    real                  :: lat_lim1=-90.
    real 		  :: lat_lim2=90.
      
!print *, 'Pt ',Ptemp(1:Pdim)
!print *, 'Pz ',Pdepth(1:Pdim)
!print *, 'Lt ',Ltemp(1:Ldim)
!print *, 'Ls ',Lsalt(1:Ldim)
!print *, 'Lz ',Ldepth(1:Ldim)

if (Plat >lat_lim1 .and. Plat < lat_lim2) then  
   DO i=1,Pdim
	!print *,'i',i
	!
	! first get the deltaz with Levitus temp profile 
	! and if shortest deltaz and dt < dtmin, then chose the Lsalt 
	!
	Do j=1,Ldim
	 deltaz(j)=abs(Pdepth(i)-Ldepth(j))
	end do
	jd= minloc(deltaz(:),1)
	jdepth=jd(1)
	!print *,'jdepth',jdepth
	if (abs(Ptemp(i)-Ltemp(jdepth))< dtmin) then 
		S(i)=Lsalt(jdepth)
		!print *,'i, found an Smatch at minloc',i,S(i)
		cycle
	endif
	nj=0
	nk=0
	jlev=-1
	S(i)=-999.
	delz1=10000.
	delz2=10000.
	do j=2,Ldim
	 delt1=Ptemp(i)-Ltemp(j)
	 delt2=Ptemp(i)-Ltemp(j-1)
	 delz=abs(Pdepth(i)-Ldepth(j))
	!decreasing temp with depth	
	!
	if (delt1 >= 0 .and. delt2 <= 0 .and. delz < delz1) then   
		nj=j
		delz1=delz
	endif
	! increasing temp with depth
	!
	if ((delt1 < 0.) .and. (delt2 > 0.) .and. (delz < delz2)) then
		delz2=delz
		nk=j
	endif
	enddo   ! end of do Ldim
	!
	!
	! if nj and nk not zero ..there is inversion, choose  closest in depth
	! if nj not zero and nk=0, no inversion
	if (nk==0 .and. nj /=0) then ! no inversions
		jlev=nj
		delz=delz1
	  if (delz < dzmax) then 	 
          dels  = (Lsalt(jlev)-Lsalt(jlev-1))*abs(Ptemp(i)-Ltemp(jlev-1))/  &
			abs(Ltemp(jlev)-Ltemp(jlev-1))
          S(i) = Lsalt(jlev-1)+dels 
	  endif 	
	  cycle
	endif
	! if nj=0 and nk/=0 : abnormal temp
	!
	if (nj==0 .and. nk/=0) then 
		jlev=nk
		delz=delz2
		! check whether there is inversion within 200m
		inv=0
	  do j=2,Ldim-1
	    if (Ldepth(j)< Pdepth(i)+100 .and.Ldepth(j)>Pdepth(i)-100.) then 
	      if (Ltemp(j)<Ltemp(j-1).and.Ltemp(j)< Ltemp(j+1)) then 
		 inv=1
		 !print *, 'There is an inversion within 200m'
		endif
	   endif
	 enddo  
	!
	! We need to make sure we aren't interpolating across an inversion
	!
	if (delz < dzmax.and. inv/=1) then 
          dels  = (Lsalt(jlev)-Lsalt(jlev-1))*abs(Ptemp(i)-Ltemp(jlev-1))/  &
			abs(Ltemp(jlev)-Ltemp(jlev-1))
          S(i) = Lsalt(jlev-1)+dels 
	   else 
	   S(i)=-999.
	  endif 	
	  cycle
	endif 	
	if (nj/=0 .and. nk/=0) then 
	    	jlev=nj  
	    	delz=delz1
		if (delz2.lt.delz1) then 
			jlev=nk
			delz=delz2
		endif
	  if (delz < dzmax/2) then 
          dels  = (Lsalt(jlev)-Lsalt(jlev-1))*abs(Ptemp(i)-Ltemp(jlev-1))/  &
			abs(Ltemp(jlev)-Ltemp(jlev-1))
          S(i) = Lsalt(jlev-1)+dels 
	  endif 	
	  cycle
	endif
	!if nj=0 and nk=0 profile out of range 
	!
	if (nj.eq.0 .and. nk.eq.0) then 
		jlev=-1
		S(i)=-999.
	endif
	!print *,'Entering test 2 for no match'
	 !! if jlev <= 0, then we have not find a match. Is it because the
	 !! the profile is slightly out of range??
	 !!
		if (jlev <= 0) then 
		 Tdiff(:)=abs(Ptemp(i)-Ltemp(:))
		 mask(:)=Tdiff(:)<dtmin
		 !print *,mask
		 jd=minloc(Tdiff(:),1,mask)
		!	 print *,'found minloc',jd
		 jlev= jd(1)  ! if minloc exists, then jlev < Ldim
			      ! if no minloc exist, gives largest number 
		   if (jlev > 0 .and.jlev <= Ldim) then 
		!	print *,'jlev', jlev
			if (abs(Pdepth(i)-Ldepth(jlev)) < dzmax) then 
			
		 	S(i)=Lsalt(jlev)
		!	print *,'S',S(i) 
			endif
		   endif
		    cycle
		 endif
		 
	!print *,'nj=nk=0, profile out of range',jlev
	if (delz > dzmax) cycle
	!print *,Ptemp(i),Ltemp(jlev), Ltemp(jlev-1),nj,nk,jlev,delz
	!
	! We need to make sure we aren't interpolating across an inversion
	!
	if (jlev <=0) cycle      ! no value within range, S(i) should not have 
				 ! been filled
	
   ENDDO ! i (Pdim)
else 
   S(1:Pdim)=-999.
endif  ! end if Plat from Lat_lim1 to Lat_lim2 

  !print *, 'Ps ',S(1:Pdim)

  return                            
  end subroutine ts_pairs                         

 
END MODULE SYN_SALT_MODULE
		
