   PROGRAM syn_salt_cdf
      
 ! COMPILE WITH: make syn_salt_cdf

 ! RUN WITH    : syn_salt_cdf.exe YYYY TYPE
 ! 
 !               where YYYY is the year 
 !                     TYPE is tao, pir, mxbt, marg, nxbt
 ! Sep 2004   Jossy Jacob
 ! Mar 2006   Robin Kovach
 ! Jun 2006   Robin Kovach, DATE_TIME, OBS_ERROR, NETCDF_MODULE, SYN_SALT_MODULE
 ! Sep 2009   New GEOS-5 format, move to kalman
 ! Mar 201 0   New GEOS-5 format, data_id, var_id, inst_id, data_id
 !
 ! OUTPUT FILES: 
 ! /ocean/OCN_IN/DATA/OBS/?/S_TYPE_YYYY.nc
 !
 ! Read Profile data 
 ! Read Levitus Data to find matching profile (in time and space)
 ! Calculate matching salinity and density profile (for ts pairs)
 ! Write out S profiles 
 ! ****************************************************************************
   USE NETCDF_MODULE_MOOR 
   USE SYN_SALT_MODULE 
   IMPLICIT NONE       

   integer            :: nobs, nlevs, i, j, k, good_obs, maxnobs, nptsmax
!   character*256      :: fname_obs, fname_in, fname_out, fmain
   character(len=:), allocatable  :: fname_obs, fname_in, fname_out, fmain
   integer, parameter :: UNIT_IN = 10, UNIT_OUT = 20
   character*4        :: yyyy, type	    
 
   character*64                             :: title,source,history,conventions, title_s
   real                                     :: inst_error, missing, salt_error, salt_inst_error
   integer                                  :: var_id, nlen, mlen
   integer,     allocatable, dimension(:)   :: date_time, inst_id, data_id
   integer,     allocatable, dimension(:)   :: npts, qc_flag
   integer,     allocatable, dimension(:)   :: npts_good
   real,        allocatable, dimension(:)   :: lon, lat, qc_prf
   real,        allocatable, dimension(:,:) :: depth, depth2, field, qc_lev, obs_error

   character*10                             :: sdate_time
   character*2                              :: smm
   character*3, allocatable, dimension(:)   :: smmm
   character*3                              :: months(12), moor
   character*4                              :: svar='SALT'
   integer                                  :: imm

 ! LEVITUS GRID VARIABLES  
   integer, parameter :: idim = 360, jdim = 180, kdim = 102
   integer            :: im, jm
   real               :: Llon(idim), Llat(jdim), Ldepth(kdim)

 ! INVERSION VARIABLES TO CALCULATE DENSITY CURVE   
   integer                           :: nTinv, levitus_good
   real, allocatable, dimension(:)   :: pden
   real, allocatable, dimension(:,:) :: salt, salt2
 
   months = (/ 'jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec' /)

! **************************************************************************
! EXECUTABLE
! **************************************************************************

   call GETARG (1, yyyy)
   call GETARG (2, type)

!  fmain = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/assim/'
!   fmain = '/gpfsm/dnb78s2/projects/p26/ehackert/TAO_PIRATA_RAMA_processing/MOOR/'
   fmain = '/discover/nobackup/lren1/pre_proc/NRT'

   !print *, trim(fmain) // '/METOFFICE/FINAL/T_XBT_' // yyyy // '.nc'  
   !print *, trim(fmain) // '/NCEP/FINAL/T_XBT_' // yyyy // '.nc'  
   !print *, trim(fmain) // '/ARGO/FINAL/T_ARGO_' // yyyy // '.nc'   

   if (type == 'mxbt') then 
      fname_obs = trim(fmain) // '/XBT_CTD/XBT/V3/FINAL/T_XBT_' // yyyy // '.nc'   
      fname_out = trim(fmain) // '/XBT_CTD/XBT/V3/FINAL/SYN_XBT_' // yyyy // '.nc'
!      fname_obs = '/gpfsm/dnb31/lren1/pre_proc/NRT/XBT_CTD/XBT/V3/FINAL/T_XBT_' // yyyy // '.nc'   
!      fname_out = '/gpfsm/dnb31/lren1/pre_proc/NRT/XBT_CTD/XBT/V3/FINAL/SYN_XBT_' // yyyy // '.nc'
      salt_error = 0.2
      salt_inst_error = 0.1
      maxnobs = 12000
      title_s = 'METOFFICE EN3 v2a XBT Synthetic Salinity Profiles'
      nptsmax = 3
      moor = 'MBT'
   elseif (type == 'nxbt') then
     ! fname_obs = trim(fmain) // '/NCEP/FINAL/T_XBT_' // yyyy // '.nc'   
     ! fname_out = trim(fmain) // '/NCEP/FINAL/SYN_XBT_' // yyyy // '.nc'
      salt_error = 0.2
      salt_inst_error = 0.1
      maxnobs = 25000
      title_s = 'NCEP OFMT XBT Synthetic Salinity Profiles'
      nptsmax = 3
      moor = 'NBT'
    elseif (type == 'argo') then
      !fname_obs = trim(fmain) // '/ARGO/FINAL/T_ARGO_' // yyyy // '.nc'   
      !fname_out = trim(fmain) // '/ARGO/FINAL/SYN_ARGO_' // yyyy // '.nc'
      salt_error = 0.2
      salt_inst_error = 0.1
      maxnobs = 2866
      title_s = 'ARGO Synthetic Salinity Profiles'
      nptsmax = 3
      moor = 'ARG'
    elseif (trim(type) == 'tao') then
      fname_obs = trim(fmain) // '/MOOR/TAO/V3/FINAL/T_TAO_' // yyyy // '.nc'   
      fname_out = trim(fmain) // '/MOOR/TAO/V3/FINAL/SYN_TAO_' // yyyy // '.nc'
      salt_error = 0.2
      salt_inst_error = 0.1
      maxnobs = 30000
      title_s = 'NDBC TAO Synthetic Salinity Profiles'
      nptsmax = 1
      moor = 'TAO'
      write(6,*)fname_obs,"%%%%%%%%"
    elseif (trim(type) == 'pir') then
      fname_obs = trim(fmain) // '/MOOR/PIRATA/V3/FINAL/T_PIR_' // yyyy // '.nc'   
      fname_out = trim(fmain) // '/MOOR/PIRATA/V3/FINAL/SYN_PIR_' // yyyy // '.nc'
      salt_error = 0.2
      salt_inst_error = 0.1
      maxnobs = 5000
      title_s = 'PMEL PIRATA Synthetic Salinity Profiles'
      nptsmax = 1
      moor = 'PIR' 
      write(6,*)fname_obs,"%%%%%%%%"
   elseif (trim(type) == 'rama') then
      fname_obs = trim(fmain) // '/MOOR/RAMA/V3/FINAL/T_RAMA_' // yyyy // '.nc'   
      fname_out = trim(fmain) // '/MOOR/RAMA/V3/FINAL/SYN_RAMA_' // yyyy // '.nc'
      salt_error = 0.2
      salt_inst_error = 0.1
      maxnobs = 5000
      title_s = 'PMEL RAMA Synthetic Salinity Profiles'
      nptsmax = 1
      moor = 'RAM' 
   else
      print *, 'Invalid TYPE'
      stop
   endif
  !print *, 'Output File: ', trim(fname_out)
 
 ! .....................................  ..................................................
 ! Read Levitus Grid File
   fname_in = '/discover/nobackup/lren1/pre_proc/NRT/LIBRARY/levitus.grid.ieee'
   print *, fname_in
   open(UNIT_IN, file=trim(fname_in), status='old',form='unformatted', convert='big_endian') 
   read(UNIT_IN) Llon, Llat, Ldepth   
   close(UNIT_IN)

! Read Netcdf File ...........................................................................................
  
  nlen = len(trim(fname_obs))
  write(6,*)'fname_obs',trim(fname_obs),nlen
  CALL READ_NETCDF_INIT(trim(fname_obs), nobs, nlevs, moor, nlen)
  print *, 'Input File: ', trim(fname_obs)
  print *, '   MAX_NOBS: ', nobs, 'MAX_NPTS: ', nlevs
 

  allocate ( date_time(nobs), inst_id(nobs) )
  allocate ( npts(nobs), qc_flag(nobs) )
  allocate ( lon(nobs), lat(nobs), qc_prf(nobs), data_id(nobs), smmm(nobs) )
  allocate ( depth(nlevs,nobs), depth2(nlevs,nobs), field(nlevs,nobs), qc_lev(nlevs,nobs), obs_error(nlevs,nobs) )
  allocate ( salt(nlevs,nobs), npts_good(nobs),salt2(nlevs,nobs))

  CALL read_netcdf (trim(fname_obs),nobs,nlevs,inst_error,var_id,missing,             &
                    date_time,lon,lat,npts,qc_flag,  &
                    data_id,inst_id,qc_prf,depth,field,qc_lev,obs_error, &
                    title,source,history,conventions,moor,nlen)

!  write(6,*)date_time
! stop 9999

  obs_error  = salt_error
  inst_error = salt_inst_error
  salt       = missing
  depth2     = missing
  salt2      = missing
  var_id = 102

  ! First lets subset the data
  good_obs = 0
  DO i=1,nobs
     !IF (lat(i)<=30 .and. lat(i) >= -30 .and. date_time(i) <= 2005120112) then
 !    IF (lat(i)<=30 .and. lat(i) >= -30) then
        good_obs = good_obs + 1

        !print *, i, date_time(i), npts(i), lon(i), lat(i), data_id(i), qc_prf(i)

        date_time(good_obs)   = date_time(i)
        lon(good_obs)         = lon(i)
        lat(good_obs)         = lat(i)
        npts(good_obs)        = npts(i)
        qc_flag(good_obs)     = qc_flag(i)
	data_id(good_obs)     = data_id(i)
	inst_id(good_obs)     = inst_id(i)
	qc_prf(good_obs)      = qc_prf(i)
	depth(:,good_obs)     = depth(:,i)
	field(:,good_obs)     = field(:,i)
	qc_lev(:,good_obs)    = qc_lev(:,i)
	obs_error(:,good_obs) = obs_error(:,i)
 !    ELSE
 !       good_obs = good_obs
 !    ENDIF
  ENDDO
  nobs=good_obs
  print *, nobs, good_obs 

  DO i=1,nobs

     levitus_good=3
     npts_good(i) = 0

     ! Extract month name for Levitus Data (smmm) 
       write ( sdate_time,'(I10)') date_time(i) 
!       print *, date_time(i)
       smm = sdate_time(5:6)    
       read (smm,*) imm     
       smmm(i) = months(imm)
      ! write(6,*)'in loop main ',smmm(i),sdate_time(5:6) ,imm

     ! Rotate LON to 0 to 360 range for Levitus
       if ( lon(i) < 0 ) then
          lon(i) = lon(i) + 360
       endif

     ! Find a temperature inversion   
       call find_tinv ( npts(i), field(:,i), nTinv )

     ! Calculate density profile (pden)
       allocate ( pden(npts(i)) )
      
       !print *, 'start pden'
       call get_pden_profile (lon(i),lat(i),npts(i),field(:,i), &
                              salt(:,i),depth(:,i),smmm(i), &
                              Llon,Llat,Ldepth,idim,jdim,kdim,pden,levitus_good )
       !print *, 'end pden'

       if (levitus_good <=2 ) then
         !print *, i, levitus_good, date_time(i),data_id(i) 
         !print *, field(1:npts(i),i)
         !print *, depth(1:npts(i),i)
         !print *, salt(1:kdim,i)
         !print *, depth(1:kdim,i)
         salt(:,i) = -999
       endif

       if ( lon(i) > 180 ) then 
            lon(i) = lon(i) - 360
       endif 
 

      !! We will eliminate all data with missing values 
          do k=1,npts(i)
	    if (salt(k,i) <=42 .and. salt(k,i) >= 28) then 
	      npts_good(i) = npts_good(i)+1
              depth2(npts_good(i),i)  = depth(k,i)
	      salt2(npts_good(i),i)   = salt(k,i)
              qc_lev(npts_good(i),i) = qc_lev(k,i)
              obs_error(npts_good(i),i) = salt_error
            else
               salt2(k,i) = MISSING
               qc_lev(k,i) = 9
               depth2(k,i)=MISSING
	    endif
	    npts(i) = npts_good(i)
         end do
        deallocate(pden)

  ENDDO ! nobs

  ! Only accept npts>=nptsmax
  good_obs = 0
  DO i=1,nobs
     !IF (npts(i) >=nptsmax ) then
     IF (npts(i) >=0) then
        good_obs = good_obs + 1

        !print *, i, date_time(i), good_obs, npts(i), data_id(i)
        date_time(good_obs)   = date_time(i)
        lon(good_obs)         = lon(i)
        lat(good_obs)         = lat(i)
        npts(good_obs)        = npts(i)
        qc_flag(good_obs) = qc_flag(i)
	data_id(good_obs)    = data_id(i)
	inst_id(good_obs)      = inst_id(i)
	qc_prf(good_obs)      = qc_prf(i)
	depth(:,good_obs)     = depth2(:,i)
	field(:,good_obs)     = field(:,i)
        salt(:,good_obs)      = salt2(:,i)
	qc_lev(:,good_obs)    = qc_lev(:,i)
	obs_error(:,good_obs) = obs_error(:,i)
     ELSE
        good_obs = good_obs
     ENDIF
  ENDDO
  nobs=good_obs
  !print *, 'GOOD NOBS: ',good_obs
  
  mlen=len(trim(fname_out))
  CALL write_netcdf(trim(fname_out),maxnobs,good_obs,nlevs,    &
               inst_error,var_id,missing,date_time,      &
               lon,lat,npts,qc_flag,data_id,inst_id, &
               qc_prf,depth,salt,qc_lev,obs_error,title_s,source,svar,mlen)
  !print *, trim(fname_out)
  !print *, '  ',date_time(nobs)


END PROGRAM syn_salt_cdf

 

