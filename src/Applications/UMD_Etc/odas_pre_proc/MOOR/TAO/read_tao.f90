  PROGRAM read_tao
 
! COMPILE WITH: make read_tao
! RUN WITH    : read_tao.exe svar
!
! Reformats TAO data from ascii to netcdf and extracts data by year
! Working with the daily averaged data from the TAO web pageD
! Outputs yearly netcdf files for ODAS-2
!
! Updated July 22: Robin Kovach (added fl, had new moorings with longer name
   
   USE MISC_MODULE  
   USE ZEUS_CLOCKS
   USE PMEL_MODULE
   USE NETCDF_MODULE_ODAS2
   
   IMPLICIT NONE
  
!  integer :: yrbeg  = 2020
   integer :: yrbeg  = 2018
   integer :: yrend  = 2022
   !integer :: yrbeg  = 2016
   !integer :: yrend  = 2016
   integer :: monbeg = 12
   integer :: monend = 1
   
   real    :: ming = 100
   real    :: maxg = 0
   real    :: meang
 
   real,         parameter     :: miss         = 999.e9
   integer,      parameter     :: miss_flag    = 9
   real                        :: inst_error

   integer,      parameter     :: nlevs        = 30
   integer,      parameter     :: maxstns      = 101
   integer,      parameter     :: inst_id      = 501
   real,         parameter     :: tqc_prf      = 1 
   integer,      parameter     :: qc_flag      = 0
   character*64                :: title 
   character*64                :: source
   character*64                :: desc
!  character*65                :: dir 
   character*256               :: dir 
   character*256               :: fout
 
   integer                     :: nobs, var_id
   integer,      parameter     :: maxdays = 428, maxnobs = maxdays*maxstns
   integer                     :: ibas_gen(maxstns), ibas_spc(maxstns)
   character*8                 :: sdate
   character*10                :: sdate_time
   character*4                 :: svar
   character*4		       :: syyyy
   character*4		       :: eyyyy
   
type tao_struct
      integer            :: date, date_time
      integer            :: qc_flag, data_id
      real               :: tqc_prf
      integer            :: inst_id, npts
      character*8        :: stn_name
      real	         :: lon, lat
   end type tao_struct
   type (tao_struct), dimension(maxnobs) :: tao

   real               :: depth(nlevs,maxnobs)
   real               :: temp(nlevs,maxnobs)
   real               :: tqc_lev(nlevs,maxnobs) = 1.0
   real               :: obs_err(nlevs,maxnobs) = 0.5

   ! WMO Stuff
   character*7, dimension(maxstns) :: tao_wmo1, tao_wmo2, tao_stn
   character*8, dimension(maxstns) :: stn_name

   integer,     dimension(maxstns) :: tao_data_id,data_id
   integer                         :: marktao, itao
   integer, parameter              :: UNIT_TAO_WMO  = 100
   integer, parameter              :: UNIT_TAO_LIST = 110
   integer, parameter              :: UNIT_STAT     = 120
   character*13, parameter         :: fname_tao = 'tao_wmo.ascii'

   real, dimension(maxstns)        :: lon, lat

   real, allocatable, dimension(:,:,:) :: temps  ! big array of temperatures
   real, allocatable, dimension(:,:)   :: tdummy ! array for single mooring

   real, allocatable, dimension(:,:,:) :: depths ! big array of depths
   real, allocatable, dimension(:,:)   :: tdeps !  array for single mooring
   
   integer, allocatable :: nidate(:)

   integer     :: ndat, nd, ieof, month, mon, hh, fl, ndays
   integer     :: i, j, k, m, ndata, iyear, year0, year2
   real        :: flag, xlon, xlat

   character*256   :: fname_in
   character*256   :: fname_out, fname_stats, fname
   integer         :: markout
   logical         :: exist

   data fname_stats/'latest_data'/

   call GETARG (1, svar)
   call GETARG (2, syyyy)
   call GETARG (3, eyyyy)
	
   read (syyyy, *) yrbeg
   read (eyyyy, *) yrend

   if (svar=='TEMP') then
     title   = 'PMEL TAO Temperature Profiles'
     source  = 'ftp.pmel.noaa.gov'
     desc    = 'TAO Pirata Temperature Profiles'
!    dir     = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/assim/TAO/V3/FINAL/'
!     dir     = '/gpfsm/dnb78s2/projects/p26/ehackert/TAO_PIRATA_RAMA_processing/MOOR/TAO/V3/FINAL/'
     dir     = '/discover/nobackup/lren1/pre_proc/NRT/MOOR/TAO/V3/FINAL/'
     var_id  = 101
     tqc_lev(nlevs,maxnobs) = 1.0
     obs_err(nlevs,maxnobs) = 0.5
     inst_error             = 0.09 ! 0.02 after 2002
     data fname_out/'T_TAO_1980.nc'/

   elseif (svar=='SALT') then
     title   = 'PMEL TAO Salinity Profiles'
     source  = 'ftp.pmel.noaa.gov'
     desc    = 'PMEL TAO Salinity Profiles'
!    dir     = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/assim/TAO/V3/FINAL/'
!     dir     = '/gpfsm/dnb78s2/projects/p26/ehackert/TAO_PIRATA_RAMA_processing/MOOR/TAO/V3/FINAL/'
     dir     = '/discover/nobackup/lren1/pre_proc/NRT/MOOR/TAO/V3/FINAL/'
     var_id  = 102
     tqc_lev(nlevs,maxnobs) = 1.0
     obs_err(nlevs,maxnobs) = 0.2
     inst_error             = 0.1
     data fname_out/'S_TAO_1980.nc'/
   endif


! ******************************************************************

  markout      = scan(fname_out,'1980')

  temp = miss
  depth = miss

! Open TAO WMO numbers ascii file
! tao_wmo1 is taoname from ftp site, tao_wmo2 is real wmo number
! ..............................................................
  OPEN (UNIT_TAO_WMO, file=fname_tao, status='old', form='formatted')
  ieof = 0
  itao = 1
  DO WHILE (ieof == 0)
     read (UNIT_TAO_WMO, '(a6,1x,a6,1x,i8)',iostat = ieof) &
                          tao_wmo1(itao),tao_wmo2(itao),tao_data_id(itao)
     if (ieof /= 0) exit
     !print *, itao,' *',tao_wmo1(itao),'*',tao_wmo2(itao),'*', tao_data_id(itao)      
     itao = itao + 1             
  ENDDO
  CLOSE (UNIT_TAO_WMO)
  itao = itao - 1

     

! Work one year at a time, Work out how large the time arrays
! ...........................................................
  mon = 2

  DO iyear=yrbeg,yrend

     inst_error = 0.09
     if (iyear >= 2002) then
       inst_error = 0.02
     endif
     if (svar=='SALT') then
       inst_error = 0.1
     endif

     year0 = iyear-1
     year2 = iyear+1

     write(fname_out(markout:markout+3),'(i4)') iyear

     nd = 0
     do month=monbeg,12
        call zdays(month,year0,ndays)
        nd = nd + ndays
        !nd = nd +  Z_DAYSINM(month,year0)
     enddo
     call zdays(mon,iyear,ndays)
     nd = nd + 337 + ndays
     !nd = nd + 337 +  Z_DAYSINM(mon,iyear)
     do month=1,monend
        call zdays(month,year2,ndays)
        nd = nd + ndays
        !nd = nd +  Z_DAYSINM(month,year2)
     enddo

     !print *, nd, ' days in ', monbeg, '/', year0, ' to ', monend, '/', year2

     allocate ( nidate(nd) )
     allocate ( temps(maxstns,nlevs,nd) )
     allocate ( tdummy(nlevs,1:nd) )
     allocate ( depths(maxstns,nlevs,nd) )
     allocate ( tdeps(nlevs,1:nd) )
       
     temps = miss

     ! CALL make_dates
     ! ..........................
       call make_dates(year0, monbeg, year2, monend, nidate)
       
       if (svar=='TEMP') then
         open(UNIT_TAO_LIST,file='tao_temp_files.list',form='formatted',status='old')
       elseif (svar=='SALT') then
         open(UNIT_TAO_LIST,file='tao_salt_files.list',form='formatted',status='old')
       endif
 
 
       DO k=1,maxstns
          read(UNIT_TAO_LIST,fmt='(a)',end=999) fname_in
          print *, k, trim(fname_in) 
          marktao = scan(trim(fname_in),'.')
          fl = marktao + 5 
!          print *, marktao
!          if ( marktao == 71 ) then
!             tao_stn(k) = fname_in(marktao-10 : marktao-4) 
!             fl = 76
!          elseif ( marktao == 70 ) then
!             tao_stn(k) = fname_in(marktao-9 : marktao-4) 
!             fl = 75
!          elseif ( marktao == 69 ) then
!             tao_stn(k) = fname_in(marktao-8 : marktao-4) 
!             fl = 74
!          elseif ( marktao == 68 ) then
!             tao_stn(k) = fname_in(marktao-7 : marktao-4) 
!             fl = 73
!          elseif ( marktao == 78 ) then
!             tao_stn(k) = fname_in(marktao-11 : marktao-4) 
!             fl = 83    
!          elseif ( marktao == 77 ) then
!             tao_stn(k) = fname_in(marktao-10 : marktao-4) 
!             fl = 82
!          elseif ( marktao == 76 ) then
!             tao_stn(k) = fname_in(marktao-9 : marktao-4) 
!             fl = 81  
             
             
!          else
!            print *, trim(fname_in), marktao, len(trim(fname_in))
!            stop
!          endif  
 
          !print *, maxstns, trim(fname_in), ' ', marktao, '*',tao_stn(k),'*' 
          !stop

          do i=1,itao  
             if ( tao_stn(k) == tao_wmo1(i) ) then
                stn_name(k) = tao_wmo2(i)
                data_id(k)  = tao_data_id(i)
                print *, tao_stn(k),' ', stn_name(k), data_id(k)
             endif
          enddo  
          !print *, i, maxstns, fname_in   
 
          !print *, trim(fname_in)            
	  !print *, 'in',nlevs, nd

	  !print *, 'start read_raw_pmel', trim(fname_in)
          call read_raw_pmel(trim(fname_in), nlevs, tdeps, nidate, &
                            flag, xlat, xlon, nd, tdummy, miss,svar,fl)   
	  !print *, 'end read_raw_pmel' 
 
          lat(k) = xlat
          lon(k) = xlon
          if (xlon < 0.) lon(k) = xlon+360.
          temps(k,1:nlevs,1:nd)= tdummy(1:nlevs,1:nd)
          depths(k,1:nlevs,1:nd)= tdeps(1:nlevs,1:nd)
    
         
       ENDDO ! maxstns
      
   999  close(UNIT_TAO_LIST)


  ! For each year,
  ! Loop through days and stations,getting data with ndat>0
  ! Put data in tao structure by nobs=nd*maxstns
       nobs = 0
       DO i=1,nd
          ndata = 0
          do m=1,maxstns
             ndat = count(temps(m,1:nlevs,i) < miss )
             if (ndat > 0) then
                nobs = nobs + 1                
                ndata = ndata + ndat
                tao(nobs)%date            = nidate(i)
                ! Add hour interger (12 noon)
                   write (sdate,'(I8)') nidate(i)
                   sdate_time = sdate // '12'
                   read (sdate_time,*) tao(nobs)%date_time
 
                tao(nobs)%lon             = lon(m)

                if (tao(nobs)%lon > 180) then
                  tao(nobs)%lon =  tao(nobs)%lon- 360
                endif

                tao(nobs)%lat             = lat(m)
                tao(nobs)%npts            = ndat
                tao(nobs)%qc_flag         = qc_flag
                tao(nobs)%data_id         = data_id(m)
                tao(nobs)%inst_id         = inst_id
                tao(nobs)%tqc_prf         = tqc_prf
                depth(1:ndat,nobs)        = depths(m,1:ndat,i)
                temp(1:ndat,nobs)         = temps(m,1:ndat,i)
                tqc_lev(1:ndat,nobs)      = 1
		obs_err(1:ndat,nobs)      = 0.5
	     endif
          enddo  ! m, number of stations
          !print *,  nidate(i), ': ',ndata,'  observations'          
       ENDDO     ! nd, number of days in year                    
       !print *,  nobs, ' observations in ', iyear

       deallocate ( nidate)
       deallocate ( temps, tdummy, depths, tdeps )
 
 
     ! Write netCDF File
     ! ..................................................
       fname = trim(fname_out)
       if (svar=='SALT') then
         fname = 'S' // fname_out(2:14)
       endif
       fout = trim(dir) // trim(fname)
       inquire (file=trim(fout), exist=exist)
       print *, trim(fname), nd, nobs
  
       IF (exist) then    
          call append_netcdf(fout,maxnobs,nobs,nlevs,inst_error, &
               var_id,miss,tao%date_time, &
               tao%lon,tao%lat,tao%npts, &
               tao%qc_flag,tao%data_id,tao%inst_id, &
               tao%tqc_prf,depth,temp,tqc_lev,obs_err,svar)
       ELSE 
          call write_netcdf(fout,maxnobs,nobs,nlevs,inst_error, &
               var_id,miss,tao%date_time, &
               tao%lon,tao%lat,tao%npts, &
               tao%qc_flag,tao%data_id,tao%inst_id, &
               tao%tqc_prf,depth,temp,tqc_lev,obs_err,title,source,svar) 
       ENDIF

  ENDDO  !  iyear

  !print *, tao(nobs)%date
  open(UNIT_STAT, file=fname_stats, status='unknown', form='formatted')
  write(UNIT_STAT, '(i8)') tao(nobs)%date
  close(UNIT_STAT)


  END PROGRAM read_tao


