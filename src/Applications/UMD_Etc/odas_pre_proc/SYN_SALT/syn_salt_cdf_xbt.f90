   PROGRAM syn_salt_cdf_xbt
      
 ! COMPILE WITH: make syn_salt_cdf_xbt
 ! RUN WITH    : syn_salt_cdf.exe YYYY TYPE
 !               where YYYY is the year 
 !                      TYPE is tao, pir, mxbt, marg, nxbt
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
   USE NETCDF_MODULE_ODAS2
   USE SYN_SALT_MODULE
   IMPLICIT NONE      

   integer            :: nobs, nlevs, i, j, k, good_obs, maxnobs, nptsmax
   character*256      :: fname_obs, fname_in, fname_out
   integer, parameter :: UNIT_IN = 10, UNIT_OUT = 20
   character*4        :: yyyy, type	    
 
   character*64                             :: title,source,history,conventions, title_s
   real                                     :: inst_error, missing, salt_error, salt_inst_error
   integer                                  :: var_id
   integer,     allocatable, dimension(:)   :: date_time, inst_id, data_id
   integer,     allocatable, dimension(:)   :: npts, qc_flag
   integer,     allocatable, dimension(:)   :: npts_good
   real,        allocatable, dimension(:)   :: lon, lat, qc_prf
   real,        allocatable, dimension(:,:) :: depth,depth2, field, qc_lev, obs_error

   character*10                             :: sdate_time
   character*2                              :: smm
   character*3, allocatable, dimension(:)   :: smmm
   character*3                              :: months(12)
   character*4                              :: svar='SALT'
   integer                                  :: imm

 ! LEVITUS GRID VARIABLES 
   integer, parameter :: idim = 360, jdim = 180, kdim = 102
   integer            :: im, jm
   real               :: Llon(idim), Llat(jdim), Ldepth(kdim)

 ! INVERSION VARIABLES TO CALCULATE DENSITY CURVE   
   integer                           :: nTinv, levitus_good
   real, allocatable, dimension(:)   :: pden
   real, allocatable, dimension(:,:) :: salt,salt2
 
   months = (/ 'jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec' /)

! **************************************************************************
! EXECUTABLE
! **************************************************************************

   call GETARG (1, yyyy)
   call GETARG (2, type)
      
   if (type == 'mxbt') then
      fname_obs = '/gpfsm/dnb31/lren1/pre_proc/NRT/XBT_CTD/XBT/V3/FINAL/T_XBT_' // yyyy // '.nc'   
!      fname_obs = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/assim/SYN_7.0/XBT/T_XBT_' // yyyy // '.nc'   
      fname_out = '/gpfsm/dnb31/lren1/pre_proc/NRT/XBT_CTD/XBT/V3/FINAL/SYN_XBT_' // yyyy // '.nc'
      salt_error = 0.2
      salt_inst_error = 0.1
      maxnobs = 12000
      title_s = 'METOFFICE EN3 v2a XBT Synthetic Salinity Profiles'
      nptsmax = 3
   elseif (type == 'nxbt') then
      fname_obs = '/gpfsm/dnb42/gmaofcst/ocean/obs/odas-2/NCEP/FINAL/T_XBT_' // yyyy // '.nc'   
      fname_out = '/gpfsm/dnb42/gmaofcst/ocean/obs/odas-2/NCEP/FINAL/SYN_XBT_' // yyyy // '.nc'
      salt_error = 0.2
      salt_inst_error = 0.1
      maxnobs = 25000
      title_s = 'NCEP OFMT XBT Synthetic Salinity Profiles'
      nptsmax = 3
   else
      print *, 'Invalid TYPE'
      stop
   endif
 
 ! .....................................  ..................................................
 ! Read Levitus Grid File
!   fname_in = '/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/PRE/LIBRARY/levitus.grid.ieee'
   fname_in = '/discover/nobackup/lren1/pre_proc/NRT/LIBRARY/levitus.grid.ieee'
   print *, fname_in
   open(UNIT_IN, file=trim(fname_in), status='old',form='unformatted', convert='big_endian') 
   read(UNIT_IN) Llon,Llat,Ldepth
   close(UNIT_IN)

! Read Netcdf File ...........................................................................................
  write(6,*)'fname_obs',trim(fname_obs)
  CALL READ_NETCDF_INIT(trim(fname_obs), nobs, nlevs)
  print *, 'Input File: ', trim(fname_obs)
  print *, '   MAX_NOBS: ', nobs, 'MAX_NPTS: ', nlevs

  allocate ( date_time(nobs), inst_id(nobs) )
  allocate ( npts(nobs), qc_flag(nobs) )
  allocate ( lon(nobs), lat(nobs), qc_prf(nobs), data_id(nobs), smmm(nobs) )
  allocate ( depth(nlevs,nobs),depth2(nlevs,nobs), field(nlevs,nobs), qc_lev(nlevs,nobs), obs_error(nlevs,nobs) )
  allocate ( salt(nlevs,nobs), salt2(nlevs,nobs), npts_good(nobs) )

  CALL read_netcdf (trim(fname_obs),nobs,nlevs,inst_error,var_id,missing,             &
                    date_time,lon,lat,npts,qc_flag,  &
                    data_id,inst_id,qc_prf,depth,field,qc_lev,obs_error, &
                    title,source,history,conventions)

  obs_error  = salt_error
  inst_error = salt_inst_error
  salt       = missing
  salt2      = missing
  depth2     = missing
  var_id = 102

  print *, nobs
    
  DO i=1,nobs
 ! DO i=9,9

     levitus_good=3
     npts_good = 0

     ! Extract month name for Levitus Data (smmm)
       write ( sdate_time,'(I10)') date_time(i) 
       smm = sdate_time(5:6)      
       read (smm,*) imm     
       smmm(i) = months(imm)
     ! Rotate LON to 0 to 360 range for Levitus
       if ( lon(i) < 0 ) then
          lon(i) = lon(i) + 360
       endif

     ! Find a temperature inversion   
       call find_tinv ( npts(i), field(:,i), nTinv )

     ! Calculate density profile (pden)
       allocate ( pden(npts(i)) )
      
       call get_pden_profile (lon(i),lat(i),npts(i),field(:,i), &
                              salt(:,i),depth(:,i),smmm(i), &
                              Llon,Llat,Ldepth,idim,jdim,kdim,pden,levitus_good )

       !print *, i
       if (levitus_good <=2 ) then
         !print *, i, levitus_good, date_time(i),data_id(i) 
         !print *, field(1:npts(i),i)
         !print *, depth(1:npts(i),i)
         !print *, salt(1:kdim,i)
         !print *, depth(1:kdim,i)
         salt(:,i) = MISSING
       endif

       if ( lon(i) > 180 ) then 
            lon(i) = lon(i) - 360
       endif 
 

      !! We will eliminate all data with missing values 
          do k=1,npts(i)
	!    if (salt(k,i) > 0 .and. salt(i,k) >= 31) then 
            if (salt(k,i) <=42 .and. salt(k,i) >= 28) then
	      npts_good(i) = npts_good(i)+1
              depth2(npts_good(i),i)  = depth(k,i)
	      salt2(npts_good(i),i)   = salt(k,i)
              qc_lev(npts_good(i),i) = qc_lev(k,i)
              obs_error(npts_good(i),i) = salt_error
            else
               salt2(k,i) = MISSING
               depth2(k,i) = MISSING
               qc_lev(k,i) = 9
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
	depth(:,good_obs)     = depth(:,i)
	field(:,good_obs)     = field(:,i)
        salt(:,good_obs)      = salt(:,i)
	qc_lev(:,good_obs)    = qc_lev(:,i)
	obs_error(:,good_obs) = obs_error(:,i)
     ELSE
        good_obs = good_obs
     ENDIF
  ENDDO
  nobs=good_obs
  !print *, 'GOOD NOBS: ',good_obs
  print *, '  ',date_time(nobs)

  CALL write_netcdf(fname_out,maxnobs,good_obs,nlevs,inst_error,   &
               var_id,missing,date_time,      &
               lon,lat,npts,qc_flag,data_id,inst_id, &
               qc_prf,depth,salt,qc_lev,obs_error,title_s,source,svar)
  !print *, trim(fname_out)
END PROGRAM syn_salt_cdf_xbt

 

