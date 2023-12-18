  PROGRAM read_rama
 
! COMPILE WITH: make read_rama
! RUN WITH    : read_rama.exe svar
!
! svar = 'TEMP', 'SALT'
!
! Reformats rama data from ascii to netcdf and extracts data by year
! Working with the daily averaged data from the rama web page
! Outputs yearly netcdf files
   
   USE MISC_MODULE      
   USE ZEUS_CLOCKS
   USE PMEL_MODULE
   USE NETCDF_MODULE_ODAS2
   
   IMPLICIT NONE
  
   integer :: yrbeg  = 2018
   integer :: yrend  = 2022
   integer :: monbeg = 12
   integer :: monend = 1
   
   real    :: ming = 100
   real    :: maxg = 0
   real    :: meang
 
   real,         parameter     :: miss         = 999.e9
   integer,      parameter     :: miss_flag    = 9
   real                        :: inst_error 

   integer,      parameter     :: nlevs        = 30
   integer,      parameter     :: maxstns      = 100
   integer,      parameter     :: inst_id      = 504
   real,         parameter     :: tqc_prf      = 1
   integer,      parameter     :: qc_flag      = 0
   character*64                :: title 
   character*64                :: source
   character*64                :: desc
!  character*65               :: dir 
   character*256              :: dir 
   character*256               :: fout
 
   integer                     :: nobs, var_id, fl
   integer,      parameter     :: maxdays = 428, maxnobs = maxdays*maxstns
   integer                     :: ibas_gen(maxstns), ibas_spc(maxstns)
   character*8                 :: sdate
   character*10                :: sdate_time
   character*4                 :: svar
   character*4		       :: syyyy
   character*4		       :: eyyyy

type rama_struct
      integer            :: date, date_time
      integer            :: qc_flag, data_id
      real               :: tqc_prf
      integer            :: inst_id, npts
      character*8        :: stn_name
      real	         :: lon, lat
   end type rama_struct
   type (rama_struct), dimension(maxnobs) :: rama

   real               :: depth(nlevs,maxnobs)
   real               :: temp(nlevs,maxnobs)
   real               :: tqc_lev(nlevs,maxnobs) = 1.0
   real               :: obs_err(nlevs,maxnobs) = 0.5

   ! WMO Stuff  
   character*6, dimension(maxstns) :: rama_wmo2
   character*9, dimension(maxstns) :: rama_wmo1
   character*9, dimension(maxstns) :: rama_stn
   character*8, dimension(maxstns) :: stn_name

   integer,     dimension(maxstns) :: rama_data_id,data_id
   integer                         :: markrama, irama
   integer, parameter              :: UNIT_RAMA_WMO  = 100
   integer, parameter              :: UNIT_RAMA_LIST = 110
   integer, parameter              :: UNIT_STAT     = 120
   character*14, parameter         :: fname_rama = 'rama_wmo.ascii'

   real, dimension(maxstns)        :: lon, lat

   real, allocatable, dimension(:,:,:) :: temps  ! big array of temperatures
   real, allocatable, dimension(:,:)   :: tdummy ! array for single mooring

   real, allocatable, dimension(:,:,:) :: depths ! big array of depths
   real, allocatable, dimension(:,:)   :: tdeps !  array for single mooring
   
   integer, allocatable :: nidate(:)

   integer     :: ndat, nd, ieof, month, mon, hh
   integer     :: i, j, k, m, ndata, iyear, year0, year2
   real        :: flag, xlon, xlat

   character*256    :: fname_in
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
     title   = 'PMEL RAMA Temperature Profiles'
     source  = 'ftp.pmel.noaa.gov'
     desc    = 'PMEL RAMA Temperature Profiles'
!    dir     = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/assim/RAMA/V3/FINAL/'
!     dir     = '/gpfsm/dnb78s2/projects/p26/ehackert/TAO_PIRATA_RAMA_processing/MOOR/RAMA/V3/FINAL/'
     dir     = '/discover/nobackup/lren1/pre_proc/NRT/MOOR/RAMA/V3/FINAL/'
     var_id  = 101
     tqc_lev(nlevs,maxnobs) = 1.0
     obs_err(nlevs,maxnobs) = 0.5
     inst_error             = 0.09 ! 0.02 after 2002
     data fname_out/'T_RAMA_1980.nc'/
   elseif (svar=='SALT') then
     title   = 'PMEL RAMA Salinity Profiles'
     source  = 'ftp.pmel.noaa.gov'
     desc    = 'PMEL RAMA Salinity Profiles'
!    dir     = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/assim/RAMA/V3/FINAL/'
!     dir     = '/gpfsm/dnb78s2/projects/p26/ehackert/TAO_PIRATA_RAMA_processing/MOOR/RAMA/V3/FINAL/'
     dir     = '/discover/nobackup/lren1/pre_proc/NRT/MOOR/RAMA/V3/FINAL/'
     var_id  = 102
     tqc_lev(nlevs,maxnobs) = 1.0
     obs_err(nlevs,maxnobs) = 0.2
     inst_error             = 0.1
     data fname_out/'S_RAMA_1980.nc'/
  endif

! ******************************************************************

  temp    = miss
  depth   = miss

  markout      = scan(fname_out,'1980')

! Open rama WMO numbers ascii file
! rama_wmo1 is ramaname from ftp site, rama_wmo2 is real wmo number
! ..............................................................
  OPEN (UNIT_RAMA_WMO, file=fname_rama, status='old', form='formatted')
  ieof = 0
  irama = 1
  DO WHILE (ieof == 0)
     read (UNIT_RAMA_WMO, '(a9,1x,a6,1x,i8)',iostat = ieof) &
          rama_wmo1(irama),rama_wmo2(irama), rama_data_id(irama)
     if (ieof /= 0) exit
     print *, '*',rama_wmo1(irama),'*',rama_wmo2(irama),'*',rama_data_id(irama)   
     irama = irama + 1             
  ENDDO
  CLOSE (UNIT_RAMA_WMO)
  irama = irama - 1



! Work one year at a time, Work out how large the time arrays
! ...........................................................
  mon = 2

  DO iyear=yrbeg,yrend

     year0 = iyear-1
     year2 = iyear+1

     write(fname_out(markout:markout+3),'(i4)') iyear

     nd = 0
     do month=monbeg,12
        nd = nd + Z_DAYSINM(month,year0)
     enddo
     nd = nd + 337 + Z_DAYSINM(mon,iyear)
     do month=1,monend
        nd = nd + Z_DAYSINM(month,year2)
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
         open(UNIT_RAMA_LIST,file='rama_temp_files.list',form='formatted',status='old')
       elseif (svar=='SALT') then
         open(UNIT_RAMA_LIST,file='rama_salt_files.list',form='formatted',status='old')
       endif

       DO k=1,maxstns
          read(UNIT_RAMA_LIST,fmt='(a)',end=999) fname_in
          markrama = scan(fname_in,'.', back=.true.)
          fl = markrama + 5
          print *, k, trim(fname_in)
!          if ( markrama == 71 ) then
!             rama_stn(k) = fname_in(markrama-9 : markrama-1)
!             fl = 79
!          elseif ( markrama == 70 ) then
!             rama_stn(k) = fname_in(markrama-8 : markrama-1)
!             fl = 78
!          elseif ( markrama == 69 ) then
!             rama_stn(k) = fname_in(markrama-7 : markrama-1)
!             fl = 77
!          elseif ( markrama == 68 ) then 
!             rama_stn(k) = fname_in(markrama-6 : markrama-1)
!             fl = 76
!          elseif ( markrama == 67 ) then
!             rama_stn(k) = fname_in(markrama-5 : markrama-1)
!             fl = 75
!          elseif ( markrama == 66 ) then
!             rama_stn(k) = fname_in(markrama-4 : markrama-1)
!             fl = 74
               
!          elseif ( markrama == 74 ) then
!             rama_stn(k) = fname_in(markrama-6 : markrama-1)
!             fl = 82  
!          elseif ( markrama == 75 ) then
!             rama_stn(k) = fname_in(markrama-7 : markrama-1)
!             fl = 83  
!          elseif ( markrama == 76 ) then
!             rama_stn(k) = fname_in(markrama-8 : markrama-1)
!             fl = 84     
!          elseif ( markrama == 77 ) then
!             rama_stn(k) = fname_in(markrama-9 : markrama-1)
!             fl = 85    
!          elseif ( markrama == 78 ) then
!             rama_stn(k) = fname_in(markrama-10 : markrama-1)
!             fl = 86    
!          else
!            print *, 'No match ',trim(fname_in), ' ', markrama
!            stop       
!          endif   
     
          !print *, trim(fname_in)
          !print *, markrama,' *',rama_stn(k),'* ',len(trim(fname_in))
         
          
          do i=1,irama
             if ( rama_stn(k) == rama_wmo1(i) ) then
                stn_name(k) = rama_wmo2(i)
                data_id(k)  = rama_data_id(i)
                !print *, rama_stn(k),' ', stn_name(k), data_id(k)
             endif
          enddo
   
          !print *, 'read start'
          !print *, len(trim(fname_in))
          call read_raw_pmel(trim(fname_in), nlevs, tdeps, nidate, &
                            flag, xlat, xlon, nd, tdummy, miss,svar,fl)
          !print *, 'read end'
          !print *, xlat, xlon, ' ',rama_stn(k)
          lat(k) = xlat
          lon(k) = xlon
          if (xlon < 0.) lon(k) = xlon+360.
          temps(k,1:nlevs,1:nd)= tdummy(1:nlevs,1:nd)
          depths(k,1:nlevs,1:nd)= tdeps(1:nlevs,1:nd)


       ENDDO ! maxstns
      
   999  close(UNIT_RAMA_LIST)


  ! For each year,
  ! Loop through days and stations,getting data with ndat>0
  ! Put data in rama structure by nobs=nd*maxstns
       nobs = 0
       DO i=1,nd
          ndata = 0
          do m=1,maxstns
             ndat = count(temps(m,1:nlevs,i) < miss )
             if (ndat > 0) then
                nobs = nobs + 1                
                ndata = ndata + ndat
                rama(nobs)%date            = nidate(i)
                ! Add hour interger (12 noon)
                   write (sdate,'(I8)') nidate(i)
                   sdate_time = sdate // '12'
                   read (sdate_time,*) rama(nobs)%date_time
 
                rama(nobs)%lon             = lon(m)

                if (rama(nobs)%lon > 180) then
                  rama(nobs)%lon =  rama(nobs)%lon- 360
                endif
               
                rama(nobs)%lat             = lat(m)
                rama(nobs)%npts            = ndat
                rama(nobs)%qc_flag         = qc_flag
                rama(nobs)%data_id         = data_id(m)
                rama(nobs)%inst_id         = inst_id
                rama(nobs)%tqc_prf         = tqc_prf
                depth(1:ndat,nobs)        = depths(m,1:ndat,i)
                temp(1:ndat,nobs)         = temps(m,1:ndat,i)
                tqc_lev(1:ndat,nobs)      = 1
		obs_err(1:ndat,nobs)      = 0.5
                if (minval(temp(1:ndat,nobs)) < 2) then
                  temp(1:ndat,nobs) = miss
                  tqc_lev(1:ndat,nobs)      = -1
                  !print *, 'error ', sdate, rama(nobs)%lat, rama(nobs)%lon, minval(temp(1:ndat,nobs))
                endif


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
       !print *, '   ',trim(fname), nd, nobs
       print *, fout
       IF (exist) then    
          call append_netcdf(fout,maxnobs,nobs,nlevs,inst_error, &
               var_id,miss,rama%date_time, &
               rama%lon,rama%lat,rama%npts,&
               rama%qc_flag,rama%data_id,rama%inst_id, &
               rama%tqc_prf,depth,temp,tqc_lev,obs_err,svar)
       ELSE 
          call write_netcdf(fout,maxnobs,nobs,nlevs,inst_error, &
               var_id,miss,rama%date_time, &
               rama%lon,rama%lat,rama%npts, &
               rama%qc_flag,rama%data_id,rama%inst_id, &
               rama%tqc_prf,depth,temp,tqc_lev,obs_err,title,source,svar) 
       ENDIF

  ENDDO  !  iyear
  !print *, fout
  !print *, rama(nobs)%date
  open(UNIT_STAT, file=fname_stats, status='unknown', form='formatted')
  write(UNIT_STAT, '(i8)') rama(nobs)%date
  close(UNIT_STAT)


  END PROGRAM read_rama


