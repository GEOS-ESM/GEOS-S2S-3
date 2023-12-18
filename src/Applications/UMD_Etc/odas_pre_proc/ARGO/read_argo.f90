PROGRAM read_argo
 
! COMPILE: make read_argo
! RUN:     read_argo.exe sdate sbasin
!
!          read_argo.exe 20121201 PAC 2013
!
! Read Daily NetCDF USGODAE ARGO DAC Profiles
!   Add some QC to clean up the bad profiles
!   Add my QC 1:good data, 0:no QC, 9:miss data
!
! Original: July 30 2009 
! Updated:  Dec 17 2009
! Updated:  Mar 19 2010: New format
!   Since each basin is in a separate file we need to output all data into one file
!   then sort it in the next program, read_argo_2.f90
 
! ****************************************************************************
  USE MISC_MODULE   
  USE DATA_ID_MODULE
  USE Buoyancy_module,    only: buoyancy, potemp, dbdtemp
  USE ARGO_NETCDF_MODULE
  USE TS_QC_MODULE
  USE NETCDF_MODULE_ODAS2
  USE SUPER_MODULE
  USE SORT_MODULE

  IMPLICIT NONE
  INCLUDE 'netcdf.inc'

  logical, EXTERNAL ::  nanchk
  logical           ::  inanchk, exist

 ! Input Parameters 
   character*8				         :: sdate
   character*3				         :: sbasin
   character*4				         :: sdoyear
   integer				         :: IARGC, nstatus, ncid
   integer, parameter				 :: kx = 508

  ! Argo GreyList and CheckList
    integer, parameter                    :: UNIT_GREY = 10, UNIT_CHCK = 20
    character*8                           :: sdate1_grey,sdate2_grey
    character*7                           :: swmo_grey, swmo_chck
    character*1                           :: svar_grey
    integer                               :: cnt_grey, cnt_chck, bs, do_tspikes

 ! Date Variables
   character*4			                 :: syear, syear_next, syear_prev
   character*3				         :: mmm
   character*2				         :: smon
   integer				         :: date, year, month, year_next, year_prev
   character*1                                   :: sh
   character*2                                   :: shh
   character*10                                  :: sdate_time
   integer,       allocatable, dimension(:)      :: date_time

   character*256			         :: fname_in
   integer                                       :: nobs, nlevs

 ! Argo File Variables
   integer,          allocatable, dimension(:)   :: npts, idate, data_id
   character*7,      allocatable, dimension(:)   :: stn_name7
   character*8,      allocatable, dimension(:)   :: stn_name
   character*64,     allocatable, dimension(:)   :: inst_ref
   character*4,      allocatable, dimension(:)   :: inst_type, data_state
   real*4,           allocatable, dimension(:)   :: lat, lon
   double precision, allocatable, dimension(:)   :: JulianDay
   real,             allocatable, dimension(:,:) :: depth
   real*4,           allocatable, dimension(:,:) :: z,       t,       s, d
   real*4,             allocatable, dimension(:,:) :: zqc_lev, tqc_lev, sqc_lev, dqc_lev
   real*4,             allocatable, dimension(:)   :: zqc_prf, tqc_prf, sqc_prf
   real*4,             allocatable, dimension(:)   :: pos_qc_prf
   character*1,        allocatable, dimension(:)   :: direction, data_mode_t, data_mode_s

 ! Misc Variables
   integer,          allocatable, dimension(:)   :: ihh, iday
   real                  	   		 :: miss, missing = 999.e9, inc
   
 ! Counters   do_it_all_argo
   integer                                       :: i, j, k, jj, ih, ieof
   integer                                       :: good_obs, bad_obs
   integer,      allocatable, dimension(:)       :: good_lev, bad_lev
  
 ! Checks
   integer                                       :: print_checks, cnt, match
   integer,         allocatable, dimension(:)    :: good_levT, good_levS, factor
   integer,         allocatable, dimension(:)    :: bad_levT,  bad_levS
   integer                                       :: match_flag, cnt_badlev
   integer                                       :: cnt_nonmon, cnt_dinv
   integer                                       :: iterate, notdone
    

  ! Density Inversion
    real, parameter                              :: REF_DENSITY = 30.0
    real, parameter                              :: rho_0 = 1000 + REF_DENSITY
    real, parameter                              :: g = 9.7976 
    real, parameter                              :: delta_rho_max = 0.05
    real                                         :: delta_rho
    integer,    allocatable, dimension(:) 	 :: dinv 

 ! Output Parameters
    integer,      parameter                     :: max_npts    = 5500
    integer,      parameter                     :: max_cdfnpts = 5000
    integer,      parameter                     :: max_nobs    = 10000    
    character*256			        :: fname_out
    integer,      parameter                     :: var_id_T = 101
    integer,      parameter                     :: var_id_S = 102

    character*64, parameter 	                :: title = 'USGODAE ARGO PROFILES'
    character*64, parameter 	                :: source = 'ftp usgodae1.fnmoc.navy.mil'
    character*4,  parameter                     :: svarT='TEMP', svarS='SALT'
    real*4,         parameter                     :: inst_errorT = 0.005
    real*4,         parameter                     :: inst_errorS = 0.02
    integer,      allocatable, dimension(:) 	:: tqc_flag, sqc_flag
    integer,      allocatable, dimension(:) 	:: inst_id
    real*4,         allocatable, dimension(:,:) 	:: obs_errT, obs_errS
    real*4,         allocatable, dimension(:,:) 	:: t2,s2,z2,zs2,tqc2_lev,sqc2_lev

  ! QC Stuff
    integer                                     :: debug, nflag, npts0, metflag, n2, do_qc
    integer                                     :: write_proc_qc, write_stats, print_test
    integer,      allocatable, dimension(:)     :: tqc_fail, sqc_fail
    integer                                     :: tqc_flag_cnt(2), sqc_flag_cnt(2), zqc_flag_cnt(2)
    integer                                     :: tqc_fail_cnt(2), sqc_fail_cnt(2), zqc_fail_cnt(2)
    integer                                     :: UNIT_STATS = 40
    character*9  			        :: STAT_FILE
    real,         allocatable, dimension(:,:)   :: zs
    integer,      allocatable, dimension(:)     :: snpts
    integer                                     :: snobs

    real, parameter           :: Tmin = -10, Tmax = 50
    real, parameter           :: Smin =  10, Smax = 40

  integer                                       :: npts3, interp_mom
  real*4                                        :: mindepth, maxdepth
  real*4                                        :: tqc_tmp(max_npts),  sqc_tmp(max_npts)
  real*4		                        :: depth_tmp(max_npts),temp_tmp(max_npts), salt_tmp(max_npts)

! ********************************************************************************************

print_checks    = 0
print_test      = 0
write_proc_qc   = 1 
write_stats     = 0 
debug           = 0  
           
! RETRIEVE FILE DTG ARGUMENT (sdate, basin)
  call GETARG (1, sdate)
  call GETARG (2, sbasin)
  call GETARG (3, sdoyear) 
 

  ! Get date and year in integer format
    read (sdate, *) date
      syear = sdate(1:4) 
      smon  = sdate(5:6)

  ! Get date and year in character format
    read (syear, *) year
    read (smon, *)  month     
      
! Get Input File
!  fname_in = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/ARGO/RAW/' &
  fname_in = '/discover/nobackup/lren1/pre_proc/NRT/ARGO/RAW/' &
  & // sbasin // '/' // syear // '/' // smon // '/' // sdate // '_prof.nc'

  if (print_checks==1) print *, 'Input File ', trim(fname_in)
 
 
! Get Initial Conditions
  CALL READ_ARGO_INIT(trim(fname_in), nobs, nlevs)
  
  
  if (print_test==1) print *, nobs,' Initial Profiles', nlevs,' Initial Max Levs'   

  if (nobs==0 .or. nlevs == 0) stop
 
  CALL READ_ARGO_INIT_QC(fname_in, nobs, nlevs)  
  if (print_checks==1) print *, nobs,' Initial Profiles', nlevs,' Initial Max Levs'
  if (nobs==0 .or. nlevs==0) stop
            
  if (nlevs > max_npts) then
    print *, 'ERROR: Increase max_npts to ', nlevs, max_npts
    !print *, trim(fname_in)
    stop
  endif
  if (nlevs > max_cdfnpts) then
    print *, 'ERROR: Increase max_cdf_npts to ', nlevs, max_cdfnpts
    !print *, trim(fname_in)
    stop
  endif

! Allocate NetCDF Variables
   allocate ( idate(nobs),             date_time(nobs),         stn_name7(nobs), data_id(nobs) )
   allocate ( data_state(nobs),        data_mode_t(nobs),       data_mode_s(nobs)       )
   allocate ( stn_name(nobs),          inst_ref(nobs),          inst_type(nobs)         )
   allocate ( lat(nobs),               lon(nobs),               npts(nobs)              )
   allocate ( pos_qc_prf(nobs),        JulianDay(nobs),         direction(nobs)         )

   allocate ( d(nlevs,nobs),      z(nlevs,nobs),        t(nlevs,nobs),       s(nlevs,nobs)   )
   allocate ( zqc_prf(nobs),       tqc_prf(nobs),        sqc_prf(nobs)      )
   allocate ( zqc_lev(nlevs,nobs), dqc_lev(nlevs, nobs), tqc_lev(nlevs,nobs), sqc_lev(nlevs,nobs))
   allocate ( zs(nlevs,nobs))

   allocate ( inst_id(nobs), tqc_flag(nobs),          sqc_flag(nobs)        )
   
! Allocate Misc Variables
  allocate ( ihh(nobs), iday(nobs) )   
  allocate ( bad_lev(nobs), good_lev(nobs), factor(nobs)  )

! QC Variables
  allocate ( tqc_fail(nobs), sqc_fail(nobs) )   
  allocate ( snpts(nobs), dinv(nobs))
 
! NetCDF Output
  allocate ( tqc2_lev(max_cdfnpts,nobs), obs_errT(max_cdfnpts,nobs) )
  allocate ( sqc2_lev(max_cdfnpts,nobs), obs_errS(max_cdfnpts,nobs) )
  allocate ( z2(max_cdfnpts,nobs),      t2(max_cdfnpts,nobs) )
  allocate ( zs2(max_cdfnpts,nobs),     s2(max_cdfnpts,nobs) )
                       
               
! Pre-define variable values
  data_id(:)       = 50899999
  inst_id(:)       = 508
  tqc_flag(:)      = 0
  sqc_flag(:)      = 0
  obs_errT(:,:)    = 0.5
  obs_errS(:,:)    = 0.1
  tqc_flag(:)      = 0
  sqc_flag(:)      = 0
 
  pos_qc_prf(:) = 9
  zqc_prf(:)    = 9
  tqc_prf(:)    = 9
  sqc_prf(:)    = 9
 
  zqc_lev(:,:)  = 9
  tqc_lev(:,:)  = 9 
  sqc_lev(:,:)  = 9
  dqc_lev(:,:)  = 9
     
  s(:,:)        = missing    
  z(:,:)        = missing 
  t(:,:)        = missing
  d(:,:)        = missing    
  
  t2(:,:)        = missing  
  z2(:,:)        = missing 
  zs2(:,:)       = missing 
  tqc2_lev(:,:)  = 9 
  sqc2_lev(:,:)  = 9    
  
  zqc_flag_cnt = 0            
  zqc_fail_cnt = 0
  tqc_flag_cnt = 0       
  tqc_fail_cnt = 0  
  sqc_flag_cnt = 0       
  sqc_fail_cnt = 0
  cnt_nonmon   = 0
  cnt_badlev   = 0
   
!_________________________________________________________________________________________________________
   
! Get NetCDF Variables   
! READ_ARGO_TS is as subroutine in 
!   /gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/SLES12/OBS/V3/LIBRARY/argo_netcdf_module.90
!   read_argo_ts starts on line 494 and ends on line 1517       
  !print *, 'Call READ_ARGO_TS ', nlevs, nobs
  CALL READ_ARGO_TS(fname_in,miss,nobs,nlevs,stn_name,inst_ref,inst_type,lat,lon, &
                    z,t,s,zqc_lev,tqc_lev,sqc_lev, &
                    zqc_prf,tqc_prf,sqc_prf,pos_qc_prf, &
                    direction,data_state,data_mode_t,data_mode_s,npts,JulianDay)    
  !print *, 'End READ_ARGO_TS'    
  ! There error happens while trying to get back from READ_ARGO_TS
        
!_________________________________________________________________________________________________________
 
       
! Get Hour Field 
  DO i=1,nobs  
    idate(i) = date
    !print *, i, JulianDay(i), iday(i), ihh(i) 
    call jd2date(JulianDay(i),iday(i),ihh(i))
      idate(i)  = date
      if ( ihh(i) <= 9 ) then
       write (sh,'(I1)') ihh(i)
       read (sh,*) ih
       shh = '0' // sh
      else
       write (shh,'(I2)') ihh(i)
      endif
      sdate_time = sdate // shh
      read (sdate_time,*) date_time(i)
      stn_name7(i) = trim(adjustl(stn_name(i)))

      !print *,i, date_time(i), npts(i)
      !print *, s(1:npts(i),i)
  ENDDO


! OPEN and READ GREYLIST
! Need to put both wmo values to  7-units
! Put profile QC to -1 if on list
  cnt_grey = 0
  !print *, 'Check GREYLIST'
  open (UNIT_GREY, file='REJECTS/ARGO_GREYLIST.txt', status='old', form='formatted')  
  ieof = 0
  DO WHILE (ieof == 0)
    read (UNIT_GREY, '(7a,1a,8a,8a)',iostat = ieof) swmo_grey,svar_grey, sdate1_grey, sdate2_grey  
    if (ieof /= 0) exit
    swmo_grey = trim(adjustl(swmo_grey))
    ! Now loop through obs
      do i=1,nobs
        if (stn_name7(i) .eq. swmo_grey ) then
            if (sdate >= sdate1_grey .and. sdate <= sdate2_grey) then
              !print *, 'GREYLIST  ',i,sdate1_grey,' ',sdate2_grey,' ',swmo_grey,' ',&
              !                    svar_grey,' ',npts(i),tqc_prf(i),sqc_prf(i),zqc_prf(i)
              if (svar_grey=='1') tqc_prf(i) = -1
              if (svar_grey=='2') sqc_prf(i) = -1
              if (svar_grey=='3') zqc_prf(i) = -1
              cnt_grey = cnt_grey+1
            endif ! if date macth
        endif ! if matching stn_name
      enddo ! obs         
  ENDDO
  close (UNIT_GREY)


! OPEN and READ GREYLIST
! Need to put both wmo values to  7-units
! Put profile QC to -1 if on list
  cnt_chck = 0
  open (UNIT_CHCK, file='REJECTS/ARGO_CHECKLIST_DIFF.txt', status='old', form='formatted')  
  ieof = 0
  DO WHILE (ieof == 0)
    read (UNIT_CHCK, '(7a)',iostat = ieof) swmo_chck
    if (ieof /= 0) exit
    swmo_chck = trim(adjustl(swmo_chck))
    ! Now loop through obs
      do i=1,nobs
        if (stn_name7(i) .eq. swmo_chck ) then
          !print *, 'CHECKLIST ',i,sdate,' ',swmo_chck,' ',stn_name7(i), npts(i), lon(i), lat(i), &
          !          tqc_prf(i), sqc_prf(i),zqc_prf(i)
          tqc_prf(i) = -1
          sqc_prf(i) = -1
          zqc_prf(i) = -1
          cnt_chck = cnt_chck+1
        endif
      enddo ! obs        
  ENDDO
  close (UNIT_CHCK)
  if (print_checks==1) print *, 'GREY: ',cnt_grey, 'CHCK: ', cnt_chck



! ..................................................................
!  If all values are bad or miss, set entire profile to bad
   DO i=1,nobs  
       !print *, i
       !print *, sqc_lev(1:npts(i),i)
       !print *, s(1:npts(i),i)
       !print *,  maxval(sqc_lev(1:npts(i),i))
       if ( maxval(tqc_lev(1:npts(i),i)) == -1 ) then
         tqc_prf(i) = -1
         sqc_prf(i) = -1
       endif
       if ( maxval(sqc_lev(1:npts(i),i)) == -1 ) then
         sqc_prf(i) = -1
       endif
       if ( maxval(zqc_lev(1:npts(i),i)) == -1 ) then
         zqc_prf(i) = -1
         tqc_prf(i) = -1
         sqc_prf(i) = -1
       endif
       !print *, tqc_prf(i), sqc_prf(i), zqc_prf(i)
   ENDDO ! i


! ................................................................
! CHECK 1
! Accept profiles with temp, pres, position profile flag==1
! Accept valid positions, npts >= 4
! Accept float with valid WMO number
! First point must be below 100 m depth
! Ascending profile
! ..................................................................
   good_obs = 0
   maxdepth = 0
   DO i=1,nobs
      !print *, i, tqc_prf(i), zqc_prf(i), pos_qc_prf(i), npts(i)
      
      if ( tqc_prf(i) == 1 .and. zqc_prf(i) == 1 .and. &
           pos_qc_prf(i) == 1 .and. npts(i) >= 4 .and. &                                            
           (lat(i) < miss .and. lon(i) < miss) .and. &
           len_trim(stn_name(i))>1 .and. z(1,i)<100 .and. &
           direction(i) == 'A') then     

           !write(UNIT_WMO, '(a)') stn_name(i)

           !if (date_time(i)==2011070303) then
           !  print *, i, lon(i), lat(i)
           !  print *,t(1:5,i)
           !  print *,z(1:5,i)
           !  print *,tqc_lev(1:5,i)
           !  print *,zqc_lev(1:5,i)
           !endif
  	   good_obs = good_obs + 1

	   npts(good_obs)       = npts(i)
           idate(good_obs)      = idate(i)
           date_time(good_obs)  = date_time(i)
	   stn_name(good_obs)   = stn_name(i)
	   lat(good_obs)        = lat(i)
	   lon(good_obs)        = lon(i)

	   z(:,good_obs)        = z(:,i)
	   t(:,good_obs)        = t(:,i)
	   s(:,good_obs)        = s(:,i)

	   zqc_lev(:,good_obs)  = zqc_lev(:,i)
	   tqc_lev(:,good_obs)  = tqc_lev(:,i)
	   sqc_lev(:,good_obs)  = sqc_lev(:,i)
	    
	   zqc_prf(good_obs)    = zqc_prf(i)
	   tqc_prf(good_obs)    = tqc_prf(i)
	   sqc_prf(good_obs)    = sqc_prf(i)
	   
	   !print *, good_obs, minval(tqc_lev(:,good_obs)),maxval(tqc_lev(:,good_obs))
	   
	   !maxdepth = z(npts(good_obs),good_obs)
	   !print *, maxdepth
	   !if ((maxdepth >= 3500) .and. (maxdepth < 5000)) then
	   !     print *, good_obs, maxdepth, npts(good_obs), stn_name(good_obs), date_time(good_obs)
	   !     !print *, z(1:npts(good_obs),good_obs)
	   !     stop
	   !endif
 
           !print *,z(1,good_obs), zqc_lev(1,good_obs), t(1,good_obs)
      else
           good_obs = good_obs
           !print *, i, lon(i), lat(i)
      endif 
   ENDDO
   nobs = good_obs

   if (print_checks==1)   print *, '   Check 1       :', nobs, maxval(npts(1:nobs))
   !print *, '   Check 1       :', nobs, maxval(npts(1:nobs))
   if (nobs == 0) stop 


! ..................................................................
! Accept profile levels if temp_qc = 1 or 2 and depth_qc = 1 or 2
! Added accpeting '2' on Sep 2020 to get more profiles
! If 4 or more levels are bad, flag the profile
! ..................................................................
  good_lev = 0
  bad_lev  = 0
 
 DO i=1,nobs
 !DO i=161,161
      !print *, tqc_lev(1:npts(i),i)
      !print *, t(1:npts(i),i)
      !print *, zqc_lev(1:npts(i),i)
      !!print *, z(1:npts(i),i)
      
      !print *, i, z(npts(i), i), stn_name(i)

     jj = 0                      
     DO j=1,npts(i)          
       if ( ((tqc_lev(j,i) >= 1) .and. (tqc_lev(j,i)<=2)) .and. &
            ((zqc_lev(j,i) >= 1) .and. (zqc_lev(j,i)<=2)) .and. &
            t(j,i) < miss .and. z(j,i) < miss .and. z(j,i) > 0) then	
            
            
       !if ( tqc_lev(j,i) == 1 .and. zqc_lev(j,i) == 1 .and. &
       !     t(j,i) < miss .and. z(j,i) < miss .and. z(j,i) > 0) then	     
                                
	     good_lev(i) = good_lev(i) + 1
	     jj = jj + 1
             
             z(jj,i)        = z(j,i)
	     t(jj,i)        = t(j,i)
	     s(jj,i)        = s(j,i)
	     zqc_lev(jj,i)  = 1
	     tqc_lev(jj,i)  = 1
	     sqc_lev(jj,i)  = 1
	else
	   !print *, i, j, z(j, i), zqc_lev(j,i), t(j,i), tqc_lev(j,i)
	   good_lev(i) = good_lev(i)
           bad_lev(i) = bad_lev(i)+1
	   jj = jj
	endif
	
     ENDDO 
     inc = 100*(float(bad_lev(i))/float(npts(i)))
     if (bad_lev(i) >= 4) then
     !if (bad_lev(i) >= 1 .and. inc > 15) then
       !print *, i, npts(i)
       cnt_badlev = cnt_badlev+1
       sqc_prf(i) = -1
       tqc_prf(i) = -1
     endif
     npts(i) = good_lev(i)
     !print *, i, tqc_prf(i), npts(i), stn_name(i),lon(i),lat(i),bad_lev(i)
  ENDDO
  

! ..................................................................
! NPTS >= 4 and tqc_prf == 1
! ..................................................................
   good_obs = 0
   DO i=1,nobs
      if ( npts(i) >= 4 .and. tqc_prf(i) == 1) then     
           !print *, i, date_time(i), tqc_prf(i), npts(i)
  	   good_obs = good_obs + 1

	   npts(good_obs)       = npts(i)
           idate(good_obs)      = idate(i)
           date_time(good_obs)  = date_time(i)
	   stn_name(good_obs)   = stn_name(i)
	   lat(good_obs)        = lat(i)
	   lon(good_obs)        = lon(i)

	   z(:,good_obs)        = z(:,i)
	   t(:,good_obs)        = t(:,i)
	   s(:,good_obs)        = s(:,i)

	   zqc_lev(:,good_obs)  = zqc_lev(:,i)
	   tqc_lev(:,good_obs)  = tqc_lev(:,i)
	   sqc_lev(:,good_obs)  = sqc_lev(:,i)
	    
	   zqc_prf(good_obs)    = zqc_prf(i)
	   tqc_prf(good_obs)    = tqc_prf(i)
	   sqc_prf(good_obs)    = sqc_prf(i)
	   
	   !maxdepth = z(npts(good_obs),good_obs)
	   !print *, maxdepth
	   !if ((maxdepth >= 3500) .and. (maxdepth < 5000)) then
	   !     print *, good_obs, maxdepth, npts(good_obs), stn_name(good_obs), date_time(good_obs)
	        !print *, z(1:npts(good_obs),good_obs)
	        !stop
	   !endif
      else
           good_obs = good_obs
      endif 
   ENDDO
   nobs = good_obs
   if (print_checks==1)   print *, '   Check 2       :', nobs, maxval(npts(1:nobs))
   !print *, '   Check 2       :', nobs, maxval(npts(1:nobs))
   if (nobs == 0) stop


! Throw out profile if pressure is NOT monotonic
! .........................................................................
  good_obs = 0
  bad_obs  = 0
   
  DO i=1,nobs
    jj = 0
    bad_obs = 0
    DO j=1, npts(i)-1
        if ( (z(j+1,i) > z(j,i))  ) then
	   jj = 0
        else
           jj = jj + 1
	   bad_obs = 1
           !print *, 'ERROR ', i, j, date_time(i), lon(i), lat(i), stn_name(i)
           !print *, t(1:npts(i),i)
           !print *, z(1:npts(i),i)
           !print *, zqc_lev(1:npts(i),i)
           !print *, tqc_lev(1:npts(i),i)
        endif
        !print *, j,jj, bad_obs, z(j+1,i), z(j,i)
    ENDDO ! j
    if ( bad_obs == 0 ) then
       good_obs = good_obs + 1

	   npts(good_obs)       = npts(i)
           idate(good_obs)      = idate(i)
           date_time(good_obs)  = date_time(i)
	   stn_name(good_obs)   = stn_name(i)
	   lat(good_obs)        = lat(i)
	   lon(good_obs)        = lon(i)
           bad_lev(good_obs)    = bad_lev(i)

	   z(:,good_obs)        = z(:,i)
	   t(:,good_obs)        = t(:,i)
	   s(:,good_obs)        = s(:,i)

	   zqc_lev(:,good_obs)  = zqc_lev(:,i)
	   tqc_lev(:,good_obs)  = tqc_lev(:,i)
	   sqc_lev(:,good_obs)  = sqc_lev(:,i)
	    
	   zqc_prf(good_obs)    = zqc_prf(i)
	   tqc_prf(good_obs)    = tqc_prf(i)
	   sqc_prf(good_obs)    = sqc_prf(i)
	   
	  ! maxdepth = z(npts(good_obs),good_obs)
	  ! print *, maxdepth
	  ! if ((maxdepth >= 3500) .and. (maxdepth < 5000)) then
	  !      print *, i, maxdepth, npts(good_obs), stn_name(good_obs), date_time(good_obs)
	        !print *, z(1:npts(good_obs),good_obs)
	  !     stop
	  !  endif
    else
       good_obs = good_obs
       zqc_fail_cnt(1) = zqc_fail_cnt(1) + 1
       cnt_nonmon = cnt_nonmon + 1
    endif
  ENDDO ! i
  nobs = good_obs
  if (print_checks==1)   print *, '   Check 3 DEPTH :', nobs, maxval(npts(1:nobs))
  if (nobs == 0) stop


! Check for Duplicate Profiles, Time and Space
! Choose duplicate based on profile rating
! .........................................................................
  DO i=1,nobs
    factor(i) = npts(i) + z(npts(i)-1,i)/100
  ENDDO

  iterate = 0  
  notdone = 0
  DO WHILE (iterate .eq. 0)
    i = 1
    do while (i .le. nobs-1)
      j = i + 1
      do while (j .le. nobs)
        if ( lon(i)==lon(j) .and. lat(i)==lat(j) .and. &
             idate(i)==idate(j) .and. &
             (tqc_prf(i)==1 .and. tqc_prf(j)==1))  then
             !print *, i, 'Duplicate: ', i,j,factor(i),factor(j)

             notdone = 1
             if (factor(i) > factor(j)) then
                tqc_prf(j) = -1
                tqc_prf(i) = 1
             elseif (factor(j) > factor(i)) then
                tqc_prf(i) = -1
                tqc_prf(j) = 1
             else ! Same Factor
                tqc_prf(i) = -1
                tqc_prf(j) = 1
             endif
        endif
        j = j + 1
      enddo ! j
      i = i + 1
    enddo ! i
    if (notdone==0) then
  	iterate = 1
    else  ! Continuing iteration
  	notdone = 0
        iterate = 0
    endif
  ENDDO ! while

! ..................................................................
! tqc_prf == 1
! ..................................................................
   good_obs = 0
   DO i=1,nobs
      if ( tqc_prf(i) == 1) then     
           !print *, i, tqc_prf(i), npts(i), stn_name(i),lon(i),lat(i)
  	   good_obs = good_obs + 1

	   npts(good_obs)       = npts(i)
           idate(good_obs)      = idate(i)
           date_time(good_obs)  = date_time(i)
	   stn_name(good_obs)   = stn_name(i)
	   lat(good_obs)        = lat(i)
	   lon(good_obs)        = lon(i)

	   z(:,good_obs)        = z(:,i)
	   t(:,good_obs)        = t(:,i)
	   s(:,good_obs)        = s(:,i)

	   zqc_lev(:,good_obs)  = zqc_lev(:,i)
	   tqc_lev(:,good_obs)  = tqc_lev(:,i)
	   sqc_lev(:,good_obs)  = sqc_lev(:,i)
	    
	   zqc_prf(good_obs)    = zqc_prf(i)
	   tqc_prf(good_obs)    = tqc_prf(i)
	   sqc_prf(good_obs)    = sqc_prf(i)
	   maxdepth = z(npts(good_obs),good_obs)
	   !if ((maxdepth >= 3500) .and. (maxdepth < 5000)) then
	   !     print *, good_obs, maxdepth, npts(good_obs), stn_name(good_obs), date_time(good_obs)
	   !endif
      else
           !print *, i, tqc_prf(i), npts(i), stn_name(i),lon(i),lat(i)
           good_obs = good_obs
      endif 
   ENDDO
   nobs = good_obs
   if (print_checks==1)   print *, '   Check 4       :', nobs, maxval(npts(1:nobs))
   if (nobs == 0) stop
 

! ..................................................................
! Calculate Potential Temp, Density, and Depth
! ..................................................................
!print *, 'Potemp'
  DO i=1, nobs
    DO j=1,npts(i) 
      !print *, z(j,i), t(j,i)
      if (zqc_lev(j,i) == 1) then
        call pres2deph(z(j,i),lat(i),z(j,i))
      endif
      if (sqc_lev(j,i) == 1 .and. zqc_lev(j,i) == 1) then
        t(j,i) = potemp(t(j,i),s(j,i),z(j,i))
        d(j,i) = buoyancy(t(j,i),s(j,i)-35,z(j,i))
        d(j,i) = rho_0/((d(j,i)/g)+1.0)
        dqc_lev(j,i)  = 1
      ! if salinity is missing use s=35
      elseif (sqc_lev(j,i) == -1 .and. zqc_lev(j,i) == 1) then
        s(j,i) = 35
        t(j,i) = potemp(t(j,i),s(j,i),z(j,i))
        d(j,i) = buoyancy(t(j,i),s(j,i)-35,z(j,i))
        d(j,i) = rho_0/((d(j,i)/g)+1.0)
        dqc_lev(j,i)  = 1
        s(j,i) = miss
      endif
      !print *, z(j,i),t(j,i), d(j,i)
    ENDDO
  ENDDO
  

! ..................................................................
! Add up Density Inversions and flag salinity levels
!print *, 'Density'
  cnt_dinv = 0
  DO i=1, nobs
     dinv(i) = 0
     if ( sqc_prf(i) == 1) then
        DO j=2,npts(i)
           if ( d(j,i) /= miss .and. d(j-1,i) /= miss ) then
              delta_rho = d(j,i) - d(j-1,i)
              if ( delta_rho < 0 .and. abs(delta_rho) > delta_rho_max ) then
                 dinv(i) = dinv(i) + 1 
                 dqc_lev(j,i)   = -1
                 dqc_lev(j-1,i) = -1  
                 sqc_lev(j,i)   = -1
                 sqc_lev(j-1,i) = -1  
              endif
           endif
        ENDDO
     endif
     if (dinv(i) >= 3) then
        sqc_prf(i) = -1
        !tqc_prf(i) = -1
        cnt_dinv = cnt_dinv+1
       !print *, 'DINV ', i, date_time(i), dinv(i), lon(i), lat(i)
       !print *, t(1:npts(i),i)
       !print *, s(1:npts(i),i)
       !print *, d(1:npts(i),i)
       !print *, z(1:npts(i),i)
       !print *, dqc_lev(1:npts(i),i)
     endif
     !print *,i, npts(i), lon(i), lat(i)
  ENDDO
if (nobs > max_nobs) then
  print *, 'ERROR: Increase max_nobs to ', nobs
  stop
endif

! = 160
!print *, npts(i), z(npts(i),i)
!stop

! TEMPERATURE QC Checks
! .......................................................................................... 
  !debug = 1     
       
  !print *, 'Temp QC'
  ! MetOffice: T Contant Value   
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  do i=1,nobs        
    !print *, 'INFO: ', i, npts(i), date_time(i),  lon(i), lat(i)
    nflag       = 0     
    metflag     = 0 
    tqc_fail(i) = 1
    call CONSTANT_VALUE(i,t(1:npts(i),i),z(1:npts(i),i),npts(i),tqc_lev(1:npts(i),i),svarT,nflag)
      if (nflag==1) then 
        tqc_flag(i)     = 1  ! MetOffice QC
        tqc_fail(i)     = -1 ! Flag Profile
        tqc_prf(i)      = -1 ! Flag Profile
        tqc_fail_cnt(1) = tqc_fail_cnt(1) + 1
        sqc_flag(i)     = 1  ! MetOffice QC
        sqc_fail(i)     = -1 ! Flag Profile
        sqc_prf(i)      = -1 ! Flag Profile
        if (debug==1) print *, '  MET: T CONSTANT VALUE: ', i, date_time(i), tqc_fail(i), bad_lev(i), npts(i)
      endif
  enddo

 
  ! MetOffice: T Spikes    
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
do_tspikes=0
if (do_tspikes==1) then
  do i=1,nobs
    if (tqc_prf(i)==1 .and. npts(i) >= 4) then
      !print *, 'INFO: ', i, npts(i), date_time(i),  lon(i), lat(i)
      !if (i==86) then
      !  print *, '     ',t(1:npts(i),i)  
      !  print *, '     ',z(1:npts(i),i)  
      !endif
      nflag=1
      npts0 = npts(i) 
      ! *, npts(i)
      do while (nflag==1 .and. tqc_fail(i)>=0) 
      call T_SPIKE(i,t(1:npts(i),i),s(1:npts(i),i),z(1:npts(i),i),npts(i),&
                   tqc_lev(1:npts(i),i),sqc_lev(1:npts(i),i),lat(i),nflag,metflag)
         
         if ( nflag==1 .and. ((npts(i)==0 .or. npts(i)<4) .or. (npts0-npts(i)>=4)) ) then
           tqc_flag(i) = metflag
           tqc_fail(i) = -1 ! Flag Profile
           tqc_prf(i)  = -1 ! Flag Profile
           sqc_flag(i) = metflag
           sqc_fail(i) = -1 ! Flag Profile
           sqc_prf(i)  = -1 ! Flag Profile
           if (debug==1) print *, '  Fail: MET: T_SPIKE: ', i, date_time(i), tqc_fail(i), bad_lev(i), npts0, npts(i)
           !print *, '     ',t(1:npts(i),i)  
           !print *, '     ',z(1:npts(i),i)  
         endif  
         if (nflag==1 .and. tqc_fail(i) >= 0) then 
           tqc_flag(i) = metflag ! MetOffice QC
           tqc_fail(i)   = 0     ! Flag levels
           sqc_flag(i) = metflag ! MetOffice QC
           sqc_fail(i)   = 0     ! Flag levels
           if (debug==1) print *, '  Flag: MET: T_SPIKE: ', i, date_time(i), tqc_fail(i), bad_lev(i),  npts0, npts(i) 
           !print *, '     ',t(1:npts(i),i)  
           !print *, '     ',z(1:npts(i),i)   
         endif
       enddo  
       if ( tqc_fail(i) == -1 .and.  tqc_flag(i) == 1 )  tqc_fail_cnt(1) = tqc_fail_cnt(1) + 1    
       if ( tqc_fail(i) ==  0 .and.  tqc_flag(i) == 1 )  tqc_flag_cnt(1) = tqc_flag_cnt(1) + 1    
       if ( tqc_fail(i) == -1 .and.  tqc_flag(i) == 2 )  tqc_fail_cnt(2) = tqc_fail_cnt(2) + 1    
       if ( tqc_fail(i) == 0  .and.  tqc_flag(i) == 2 )  tqc_flag_cnt(2) = tqc_flag_cnt(2) + 1    
    endif ! Good Profile
  enddo ! obs
endif ! do_tspikes
  

  !print *, 'qc T jumps'
  ! GMAO: QC T Jumps           
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
do_qc=0
if (do_qc==1) then
  do i=1,nobs       
   if (tqc_prf(i)==1 .and. npts(i) >= 4) then
     !print *, 'INFO: ', i, npts(i), date_time(i),  lon(i), lat(i)
     !print *, '     ',t(1:npts(i),i)  
     !print *, '     ',z(1:npts(i),i)  
     nflag=1         
     npts0 = npts(i)     
      do while (nflag==1 .and. tqc_fail(i)>=0)
        call T_JUMP(i,t(1:npts(i),i),s(1:npts(i),i),z(1:npts(i),i),npts(i),&
                    tqc_lev(1:npts(i),i),sqc_lev(1:npts(i),i),lon(i),lat(i),month,nflag,date_time(i)) 
        if ( nflag==1 .and. (npts(i)==0 .or. npts(i)<4) )  then 
          tqc_flag(i) =  2 ! GMAO QC
          tqc_fail(i) = -1 ! Flag Profile
          tqc_prf(i)  = -1 ! Flag Profile
          sqc_flag(i) =  2 ! GMAO QC
          sqc_fail(i) = -1 ! Flag Profile
          sqc_prf(i)  = -1 ! Flag Profile
          if (debug==1) print *, '  GMAO: T_JUMP: ', i, date_time(i), tqc_fail(i), bad_lev(i), npts(i)  
         elseif ( nflag==1 .and. tqc_fail(i) >= 0 .and. npts(i) /= npts0) then
          tqc_flag(i) = 2 ! GMAO QC
          tqc_fail(i) = 0 ! Flag Levels
          sqc_flag(i) = 2 ! GMAO QC
          sqc_fail(i) = 0 ! Flag Levels
          if (debug==1) print *, '  GMAO: T_JUMP: ', i, date_time(i), tqc_fail(i), bad_lev(i), npts(i) 
        endif  
      enddo   
      if ( tqc_fail(i) == -1 .and. tqc_flag(i) == 2 )  tqc_fail_cnt(2) = tqc_fail_cnt(2) + 1    
      if ( tqc_fail(i) == 0 .and.  tqc_flag(i) == 2 )  tqc_flag_cnt(2) = tqc_flag_cnt(2) + 1    
    endif ! Good Profile
  enddo ! obs  
endif ! do_qc
! END TQC Checks                
! ........................................................................................... 



! ..................................................................
! NPTS >= 4 
! ..................................................................
   good_obs = 0
   DO i=1,nobs
      if ( npts(i) >= 4 .and. (z(1,i)<100 .and. tqc_lev(1,i)==1) .and. tqc_prf(i)==1 ) then          
       
 	   good_obs = good_obs + 1

	   npts(good_obs)        = npts(i)
           idate(good_obs)       = idate(i)
           date_time(good_obs)   = date_time(i)
	   stn_name(good_obs)    = stn_name(i)
	   lat(good_obs)         = lat(i)
	   lon(good_obs)         = lon(i)

           bad_lev(good_obs)     = bad_lev(i)
           dinv(good_obs)        = dinv(i)

           tqc_flag(good_obs)    = tqc_flag(i)
           sqc_flag(good_obs)    = sqc_flag(i)
	    
	   z(:,good_obs)        = z(:,i)
	   t(:,good_obs)        = t(:,i)
	   s(:,good_obs)        = s(:,i)

	   zqc_lev(:,good_obs)  = zqc_lev(:,i)
	   tqc_lev(:,good_obs)  = tqc_lev(:,i)
	   sqc_lev(:,good_obs)  = sqc_lev(:,i)
	    
	   zqc_prf(good_obs)    = zqc_prf(i)
	   tqc_prf(good_obs)    = tqc_prf(i)
	   sqc_prf(good_obs)    = sqc_prf(i)
	   
	   !maxdepth = z(npts(good_obs),good_obs)
	   !print *,good_obs, maxdepth
	   !if ((maxdepth >= 3500) .and. (maxdepth < 5000)) then
	   !     print *, good_obs, maxdepth, npts(good_obs), stn_name(good_obs), date_time(good_obs)
	        !print *, z(1:npts(good_obs),good_obs)
	   !    stop
	   ! endif
	 
      else
           good_obs = good_obs
      endif 
   ENDDO 
   nobs = good_obs
   if (print_checks==1)   print *, '   Check 5       :', nobs, maxval(npts(1:nobs))
   if (nobs == 0) then
       stop
   endif  

! STATS File
!if (write_stats==1) then
!  STAT_FILE  = 'tqc_stats'
!  open (UNIT_STATS, file=STAT_FILE, status='unknown', &
!         position = 'append',form='formatted', action='write')
!  write(UNIT_STATS, '(i4,1x,i2,1x,i5,1x,i5,1x,i5,1x,i5,1x,i5,1x,i5,1x,i5,1x)') &
!                     year,month,nobs,&
!                     tqc_flag_cnt(1),tqc_flag_cnt(2), &
!                     tqc_fail_cnt(1),tqc_fail_cnt(2), cnt_badlev, cnt_nonmon
!endif


! SALINITY: Take out bad levels, snpts
! ..................................................................
! Accept profile levels if sqc_lev = 1
! If 4 or more levels are bad, flag the profile (take this out for now)
! ..................................................................
  zs = z
  good_lev = 0
  bad_lev  = 0
  DO i=1,nobs
     jj = 0                      
     DO j=1,npts(i)          
       if ( sqc_lev(j,i) == 1 .and. s(j,i) < miss .and. tqc_lev(j,i) == 1 .and. zqc_lev(j,i) == 1 ) then	                    
	     good_lev(i) = good_lev(i) + 1
	     jj = jj + 1
             
             zs(jj,i)      = z(j,i)
	     s(jj,i)       = s(j,i)
	     sqc_lev(jj,i) = sqc_lev(j,i)
	else
	   good_lev(i) = good_lev(i)
           bad_lev(i) = bad_lev(i)+1
	   jj = jj
	endif
     ENDDO 
     if (bad_lev(i) >= 4) then
       cnt_badlev = cnt_badlev+1
       sqc_prf(i) = -1
     endif
     snpts(i) = good_lev(i)
     !print *, i, npts(i), snpts(i)
     !print *, i,stn_name(i),lon(i),lat(i)

  ENDDO


! Salinity QC Checks
! ..........................................................................................   
  
  ! MetOffice: S Contant Value   
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  do i=1,nobs        
   if (sqc_prf(i)==1 .and. snpts(i) >= 4) then
    !print *, 'INFO: ', i, npts(i), date_time(i),  lon(i), lat(i)
    !print *, '     ',t(1:npts(i),i)  
    !print *, '     ',z(1:npts(i),i)  

    nflag=0     
    metflag=0 
    sqc_fail(i) = 1
    call CONSTANT_VALUE(i,s(1:snpts(i),i),zs(1:snpts(i),i),snpts(i),sqc_lev(1:snpts(i),i),svarS,nflag)
      if (nflag==1) then 
        sqc_flag(i)     = 1  ! MetOffice QC
        sqc_fail(i)     = -1 ! Flag Profile
        sqc_prf(i)      = -1 ! Flag Profile
        sqc_fail_cnt(1) = sqc_fail_cnt(1) + 1
        if (debug==1) print *, '  MET: S CONSTANT VALUE: ', i, date_time(i), sqc_fail(i), bad_lev(i), snpts(i)
      endif
    endif ! Good Profiles
  enddo ! nobs


 ! MetOffice: S Spikes    
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
do_tspikes=0
if (do_tspikes==1) then
  do i=1,nobs
           !if (date_time(i)==2004112914 .and. i==8) then
           !  print *, i, lon(i), lat(i), sqc_prf(i),npts(i), snpts(i)
           !!  print *, s(1:npts(i),i)
           !  print *, sqc_lev(1:npts(i),i)
           !endif

    if (sqc_prf(i)==1 .and. snpts(i) >= 4) then
      !print *, 'INFO: ', i, npts(i),snpts(i), date_time(i), lon(i), lat(i), bad_levT(i),bad_lev(i)
      !print *, '     ',s(1:snpts(i),i)  
      !print *, '     ',zs(1:snpts(i),i)  

     !open (100, file='test', status='unknown',form='unformatted')
     !write(100) zs(1:snpts(i),i)
     !write(100) s(1:snpts(i),i)

      nflag=1 
      npts0 = snpts(i) 
      do while (nflag==1 .and. sqc_fail(i)>=0) 
      
      call S_SPIKE(i,s(1:snpts(i),i),zs(1:snpts(i),i),snpts(i),&
                   sqc_lev(1:snpts(i),i),lat(i),nflag,metflag)
    
         !print *, nflag, snpts(i), npts(i)
         if ( nflag==1 .and. ((snpts(i)==0 .or. snpts(i)<4) .or. (npts0-snpts(i)>=4)) ) then
           sqc_flag(i) = metflag
           sqc_fail(i) = -1 ! Flag Profile
           sqc_prf(i)  = -1 ! Flag Profile
           if (debug==1) print *, '  Fail: MET: S_SPIKE: ',i,date_time(i),bad_lev(i),dinv(i),npts0,snpts(i)
           !print *, '     ',s(1:snpts(i),i)  
           !print *, '     ',zs(1:snpts(i),i)  
         endif  

         if (nflag==1 .and. sqc_fail(i) >= 0) then 
           sqc_flag(i) = metflag ! MetOffice QC
           sqc_fail(i)   = 0     ! Flag levels
           if (debug==1) print *, '  Flag: MET: S_SPIKE: ',i,date_time(i),bad_lev(i),dinv(i),npts0,snpts(i)
           !print *, '     ',s(1:snpts(i),i)  
           !print *, '     ',zs(1:snpts(i),i)   
         endif
       enddo  
       if ( sqc_fail(i) == -1 .and.  sqc_flag(i) == 1 )  sqc_fail_cnt(1) = sqc_fail_cnt(1) + 1    
       if ( sqc_fail(i) ==  0 .and.  sqc_flag(i) == 1 )  sqc_flag_cnt(1) = sqc_flag_cnt(1) + 1    
       if ( sqc_fail(i) == -1 .and.  sqc_flag(i) == 2 )  sqc_fail_cnt(2) = sqc_fail_cnt(2) + 1    
       if ( sqc_fail(i) == 0  .and.  sqc_flag(i) == 2 )  sqc_flag_cnt(2) = sqc_flag_cnt(2) + 1   
 
     !write(100) zs(1:snpts(i),i)
     !write(100) s(1:snpts(i),i)
     !close(100)
  
    endif ! Good Profile
  enddo ! obs
endif ! do_tspikes

  ! GMAO: QC S Jumps           
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
do_qc=0
if (do_qc==1) then
  snobs = 0
  debug=0  
  do i=1,nobs   
           !if (date_time(i)==2007100401) then
           !  print *, i, lon(i), lat(i), sqc_prf(i)
           !  print *, s(1:npts(i),i)
           !  print *, sqc_lev(1:npts(i),i)
           !endif

   if (sqc_prf(i)==1 .and. snpts(i) >= 4) then

     !print *, 'INFO: ', i, npts(i), date_time(i),  lon(i), lat(i)
     !print *, '     ',s(1:snpts(i),i)  
     !print *, '     ',zs(1:snpts(i),i)  
     !open (100, file='test', status='unknown',form='unformatted')
     !write(100) zs(1:snpts(i),i)
     !write(100) s(1:snpts(i),i)
     nflag=1         
     npts0 = snpts(i)   
     !print *, 'before sjump', npts0  
      do while (nflag==1 .and. sqc_fail(i)>=0)
        call S_JUMP(i,s(1:snpts(i),i),zs(1:snpts(i),i),snpts(i),&
                    sqc_lev(1:npts(i),i),lon(i),lat(i),month,nflag,date_time(i)) 
        if ( nflag==1 .and. (snpts(i)==0 .or. snpts(i)<4) )  then 
          sqc_flag(i) =  2 ! GMAO QC
          sqc_fail(i) = -1 ! Flag Profile
          sqc_prf(i)  = -1 ! Flag Profile
          if (debug==1) print *, '  Fail: GMAO: S_JUMP: ',i,date_time(i),bad_lev(i),dinv(i),npts0,snpts(i)
         elseif ( nflag==1 .and. sqc_fail(i) >= 0 .and. snpts(i) /= npts0) then
          sqc_flag(i) = 2 ! GMAO QC
          sqc_fail(i) = 0 ! Flag Levels
          if (debug==1) print *, '  Flag: GMAO: S_JUMP: ',i,date_time(i),bad_lev(i),dinv(i),npts0,snpts(i)
        endif  
      enddo   
      if ( sqc_fail(i) == -1 .and. sqc_flag(i) == 2 )  sqc_fail_cnt(2) = sqc_fail_cnt(2) + 1    
      if ( sqc_fail(i) == 0 .and.  sqc_flag(i) == 2 )  sqc_flag_cnt(2) = sqc_flag_cnt(2) + 1    
    endif ! Good Profile

    if (snpts(i)<4) sqc_prf(i) = -1
    if (sqc_prf(i)==1) then
      snobs = snobs + 1
    else
      !print *, 'Bad S ', nobs, snobs
    endif
  enddo ! obs  
endif ! do_qc

! END SQC Checks                
! ... ..  . ...... ... ............................................................................ 
 
     
! GET DATA_ID   
  DO i=1,nobs      
    !print *, i, '*',stn_name(i),'*'
    CALL WMO_TO_DATA_ID (kx,stn_name(i),date_time(i),data_id(i),'arg')
    !print *, data_id(i)
  ENDDO     


!STAT_FILE  = 'sqc_stats'
!open (UNIT_STATS, file=STAT_FILE, status='unknown', &
!         position = 'append',form='formatted', action='write')
   good_obs = 0
   DO i=1,nobs
      !print *, i, npts(i)
      if ( npts(i) >= 4 .and. npts(i) <= max_cdfnpts .and. (z(1,i)<100 .and. tqc_lev(1,i)==1) .and. tqc_prf(i)==1 ) then    
 	   good_obs = good_obs + 1

	   npts(good_obs)        = npts(i)
	   snpts(good_obs)        = snpts(i)

           idate(good_obs)       = idate(i)
           date_time(good_obs)   = date_time(i)
	   stn_name(good_obs)    = stn_name(i)
	   data_id(good_obs)     = data_id(i)
	   lat(good_obs)         = lat(i)
	   lon(good_obs)         = lon(i)

           tqc_flag(good_obs)    = tqc_flag(i)
           sqc_flag(good_obs)    = sqc_flag(i)
	    
	   z(:,good_obs)       = z(:,i)
	   zs(:,good_obs)       = zs(:,i)
	   t(:,good_obs)       = t(:,i)
	   s(:,good_obs)       = s(:,i)

	   tqc_lev(:,good_obs)  = tqc_lev(:,i)	    
	   tqc_prf(good_obs)     = tqc_prf(i)
	   sqc_lev(:,good_obs)  = sqc_lev(:,i)	    
	   sqc_prf(good_obs)     = sqc_prf(i)
           !print *, i, date_time(i),lon(i), lat(i), s(1:4,i)
      else
           if (nobs-i /= 0) then
             print *, '*****',i, ' ',date_time(i), ' ', sbasin, ' ', npts(i)
             stop
             !write(UNIT_STATS, '(a8,1x,a3,1x,i3,1x)') sdate,sbasin,nobs-i
             bs = i
           endif
           good_obs = good_obs
      endif 
   ENDDO 
   nobs = good_obs
   snobs = good_obs
   if (nobs == 0) then
       stop
   endif  

!print *,'After'
  !DO i=1,snobs
    !print *, i,date_time(i),lon(i),lat(i), s(1:4,i)
    !if (i>=bs) then
    !print *, i,date_time(i),lon(i),lat(i), s(1:4,i)
         !if (date_time(i)==1998081316) then
         !   print *, lon(i), lat(i), s(1:10,i)
         !endif
    !endif
 !ENDDO


! NetCDF File
! ..................................................... 

if (write_PROC_qc==1) then
! Allocate size for netCDF File
  
 DO i=1,nobs
    j=npts(i)+1
      !print *, i, npts(i), j, max_cdfnpts
      t2(1:npts(i),i)           = t(1:npts(i),i)
      z2(1:npts(i),i)           = z(1:npts(i),i)
      tqc2_lev(1:npts(i),i)     = tqc_lev(1:npts(i),i)
      obs_errT(1:npts(i),i)     = 0.5
      t2(j:max_cdfnpts,i)       = missing
      z2(j:max_cdfnpts,i)       = missing 
      tqc2_lev(j:max_cdfnpts,i) = 9
      obs_errT(j:max_cdfnpts,i) = missing
      
    if (minval(t2(:,i)) < Tmin .or. maxval(t2(:,i),mask=t2(:,i)/=missing) > Tmax ) then
       print *, 'ERROR: T Miss ', i,date_time(i),lon(i),lat(i),minval(t2(:,i)), maxval(t2(:,i),mask=t2(:,i)/=missing)
    endif   
  ENDDO
   
  
  !call SORT_ODAS2_ARGO(date_time,lon,lat,npts,data_id,tqc_flag,z2,t2,tqc2_lev,max_cdfnpts,nobs)
   
!  fname_out = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/assim/ARGO/V3/STEP1/T_ARGO_' // sdoyear // '.nc'
   fname_out = '/discover/nobackup/lren1/pre_proc/NRT/ARGO/STEP1/T_ARGO_' // sdoyear // '.nc'


  inquire (file=trim(fname_out), exist=exist)
  !print *, trim(fname_out)
  
  IF (exist) then
     call append_netcdf(fname_out,max_nobs,nobs,max_cdfnpts,inst_errorT,var_id_T,miss, &
	     date_time,lon,lat,npts,tqc_flag, &
             data_id,inst_id,tqc_prf,z2,t2,tqc2_lev,obs_errT,svarT)
  ELSE
     call write_netcdf(fname_out,max_nobs,nobs,max_cdfnpts,inst_errorT,var_id_T,miss, &
	     date_time,lon,lat,npts,tqc_flag, &
             data_id,inst_id,tqc_prf,z2,t2,tqc2_lev,obs_errT,title,source,svarT) 
    
  ENDIF !exist


    
! ****************************************************************************
! WRITE TO ARGO S NETCDF FILE
! ****************************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Allocate size for netCDF File
  !s2 = s
  !zs2 = zs
  !sqc2_lev = sqc_lev
  DO i=1,snobs 
    j=snpts(i)+1
      s2(j:max_cdfnpts,i) = missing
      zs2(j:max_cdfnpts,i) = missing
      sqc2_lev(j:max_cdfnpts,i) = 9
      
      s2(1:npts(i),i)           = s(1:npts(i),i)
      zs2(1:npts(i),i)          = zs(1:npts(i),i)
      sqc2_lev(1:npts(i),i)     = sqc_lev(1:npts(i),i)
      obs_errS(1:npts(i),i)     = 0.1
      s2(j:max_cdfnpts,i)       = missing
      zs2(j:max_cdfnpts,i)       = missing 
      sqc2_lev(j:max_cdfnpts,i) = 9
      obs_errS(j:max_cdfnpts,i) = missing
      
      if (minval(s2(:,i)) < Smin .or. maxval(s2(:,i),mask=s2(:,i)/=missing) > Smax ) then
        print *, 'ERROR: S Miss ', i,date_time(i),lon(i),lat(i), minval(s2(:,i)), maxval(s2(:,i),mask=s2(:,i)/=missing)
      endif
  ENDDO

   good_obs = 0
   DO i=1,snobs
      !print *, i, sqc_prf(i), snpts(i), zs2(1,i)<100, sqc2_lev(1,i)
      if ( snpts(i) >= 4 .and. snpts(i) <= max_cdfnpts .and. (zs2(1,i)<100 .and. sqc2_lev(1,i)==1) .and. sqc_prf(i)==1 ) then    
 
 	   good_obs = good_obs + 1

	   snpts(good_obs)       = snpts(i)
           idate(good_obs)       = idate(i)
           date_time(good_obs)   = date_time(i)
	   stn_name(good_obs)    = stn_name(i)
	   data_id(good_obs)     = data_id(i)
	   lat(good_obs)         = lat(i)
	   lon(good_obs)         = lon(i)

           sqc_flag(good_obs)    = sqc_flag(i)
	    
	   zs2(:,good_obs)       = zs2(:,i)
	   s2(:,good_obs)        = s2(:,i)

	   sqc2_lev(:,good_obs)  = sqc2_lev(:,i)	    
	   sqc_prf(good_obs)     = sqc_prf(i)

      else
           !print *, '*****',i, ' ',date_time(i), ' ', sbasin, ' ', npts(i)
           good_obs = good_obs
      endif 
   ENDDO 
   snobs = good_obs
   if (nobs == 0) then
       stop
   endif  

  !call SORT_ODAS2_ARGO(date_time,lon,lat,npts,data_id,sqc_flag,zs2,s2,sqc2_lev,max_cdfnpts,snobs)

!  fname_out = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/assim/ARGO/V3/STEP1/S_ARGO_' // sdoyear // '.nc'
  fname_out = '/discover/nobackup/lren1/pre_proc/NRT/ARGO/STEP1/S_ARGO_' // sdoyear // '.nc'

  inquire (file=trim(fname_out), exist=exist)
  IF (exist) then
     call append_netcdf(fname_out,max_nobs,snobs,max_cdfnpts,inst_errorS,var_id_S,miss, &
	     date_time,lon,lat,snpts,sqc_flag, &
             data_id,inst_id,sqc_prf,zs2,s2,sqc2_lev,obs_errS,svarS)
  ELSE
     call write_netcdf(fname_out,max_nobs,snobs,max_cdfnpts,inst_errorS,var_id_S,miss, &
	     date_time,lon,lat,snpts,sqc_flag, &
             data_id,inst_id,sqc_prf,zs2,s2,sqc2_lev,obs_errS,title,source,svarS)
  ENDIF !exist

endif


END PROGRAM read_argo
