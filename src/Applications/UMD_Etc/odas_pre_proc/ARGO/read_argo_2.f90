 PROGRAM read_argo_2
 
! COMPILE: make read_argo_2
! RUN:     read_argo_2.exe syear svar0
!
! Original: July 30 2009
! Updated:  Dec 17, 2009
! Updated:  Mar 24, 2010 new netcdf format

! ****************************************************************************
  USE MISC_MODULE 
  USE SORT_MODULE
  USE NETCDF_MODULE_ODAS2
  USE DATA_ID_MODULE

  IMPLICIT NONE
  logical, EXTERNAL ::  nanchk
  logical           ::  inanchk, exist

 ! Input Parameters 
   character*4				         :: syear
   character*1				         :: svar0
   integer				         :: IARGC, STAT, deallocatestatus
   integer                                       :: kx = 508

   integer				         :: year
   character*256			         :: fname_in
   integer                                       :: nobs, nlevs
   integer                                       :: i, j, k
   integer                                       :: cnt, cnt1, cnt2, cnt3, cnt4, cnt5, cnt6, cnt7, cnt8, cnt9, cnt10

 ! Variables read in
   integer,          allocatable, dimension(:)   :: imon
   integer,          allocatable, dimension(:)   :: npts
   integer,          allocatable, dimension(:)   :: date_time, data_id
   character*8,      allocatable, dimension(:)   :: stn_name
   real,             allocatable, dimension(:)   :: lat, lon
   real,             allocatable, dimension(:)   :: qc_prf
   integer,          allocatable, dimension(:) 	 :: qc_flag
   integer,          allocatable, dimension(:) 	 :: inst_id

   real,             allocatable, dimension(:,:) :: z, t
   real,             allocatable, dimension(:,:) :: qc_lev
   real,             allocatable, dimension(:,:) :: obs_err

 ! Variables to sort
   real,             allocatable, dimension(:,:) :: z1, t1, qc1
   integer,          allocatable, dimension(:)   :: npts1, date1
   real,             allocatable, dimension(:)   :: lon1, lat1
   integer,          allocatable, dimension(:)   :: data_id1

   real                 	   		 :: miss
   character*4				         :: svar
   integer                    	                 :: var_id
   character*64	                                 :: title, hist
   character*64	                                 :: source, conv
   real                                          :: inst_error
   integer                                       :: max_npts, max_nobs
   character*256			         :: fname_out

! ********************************************************************************************
  call GETARG (1, syear)
  call GETARG (2, svar0)

  read (syear, *) year

  if (svar0=='T')  svar = 'TEMP'
  if (svar0=='S')  svar = 'SALT'
    
! Get Input File
!  fname_in = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/assim/ARGO/V3/STEP1/' &
  fname_in = '/discover/nobackup/lren1/pre_proc/NRT/ARGO/STEP1/' &
  & // svar0 // '_ARGO_' // syear // '.nc'
  print *, 'Input File ', trim(fname_in)
 
! Get Initial Conditions
  CALL read_netcdf_init(trim(fname_in), nobs, nlevs)
  print *, nobs,' Initial Profiles', nlevs,' Initial Max Levs'

 
  if (nobs == 0 .or. nlevs == 0) stop
 
! Allocate NetCDF Variables
   allocate ( date_time(nobs),  qc_prf(nobs),    qc_flag(nobs),  data_id(nobs)              )
   allocate ( lat(nobs),        lon(nobs),       npts(nobs),          inst_id(nobs)         )
   allocate ( z(nlevs,nobs),    t(nlevs,nobs),   qc_lev(nlevs,nobs),  obs_err(nlevs,nobs)   )
    
  max_nobs = nobs
  max_npts = nlevs
  !print *, nobs, max_nobs
  
  
  !print *, 'before', len(trim(fname_in))
  call read_netcdf(trim(fname_in),max_nobs,max_npts,inst_error,var_id,miss, &
	     date_time,lon,lat,npts,qc_flag,data_id,inst_id,qc_prf, &
             z,t,qc_lev,obs_err,title,source,hist,conv)  
 
! .......................................................................................
 
if (year==2013) then
  cnt = 0
  DO i=1,nobs
    if (npts(i) > cnt) then
      cnt = npts(i)
    endif
  ENDDO
  !print *, cnt

! entire year is too much for the sort routine
! need to split array into three

! Sort 1

! Sort 2
  i=1
  cnt=0
  DO WHILE (i <= nobs)
    if (date_time(i)>=2014011400 .and. date_time(i)<=2014022524) then
    !if (date_time(i)>=2013011400 .and. date_time(i)<=2013022524) then
    !if (date_time(i)>=2012011400 .and. date_time(i)<=2012022524) then
    !if (date_time(i)>=2011011400 .and. date_time(i)<=2011022524) then
    !if (date_time(i)>=2010030100.and. date_time(i)<=2010051524) then
    !if (date_time(i)>=2009022000 .and. date_time(i)<=2009043124) then
      cnt=i
      i=nobs
    endif
    i = i+1;
  ENDDO
  cnt = cnt-1
  if (cnt==0) then
    print *, 'ERROR: splitting year 1'
    stop
  endif
  cnt1 = cnt
  !print *, 'cnt1=',cnt1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Sort 3
  i=1
  cnt=0
  DO WHILE (i <= nobs)
     if (date_time(i)>=2014022600 .and. date_time(i)<=2014040824) then
     !if (date_time(i)>=2013022600 .and. date_time(i)<=2013040824) then
     !if (date_time(i)>=2012022600 .and. date_time(i)<=2012040824) then
     !if (date_time(i)>=2011022600 .and. date_time(i)<=2011040824) then
     !if (date_time(i)>=2010051600 .and. date_time(i)<=2010081424) then
     !if (date_time(i)>=2009050100 .and. date_time(i)<=2009073124) then
      cnt=i
      i=nobs
    endif
    i = i+1;
  ENDDO
  !print cnt, cnt1
  cnt = cnt-1-cnt1
  if (cnt==0) then
    print *, 'ERROR: splitting year 2'
    stop
  endif
  cnt2 = cnt

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Sort 4
  i=1
  cnt=0
  DO WHILE (i <= nobs)
     if (date_time(i)>=2014040900 .and. date_time(i)<=2014052024) then
     !if (date_time(i)>=2013040900 .and. date_time(i)<=2013052024) then
     !if (date_time(i)>=2012040900 .and. date_time(i)<=2012052024) then
     !if (date_time(i)>=2011040900 .and. date_time(i)<=2011052024) then
     !if (date_time(i)>=2010081500 .and. date_time(i)<=2010101424) then
     !if (date_time(i)>=2009080100 .and. date_time(i)<=2009103124) then
      cnt=i
      i=nobs
    endif
    i = i+1;
  ENDDO
  cnt = cnt-1-cnt1-cnt2
  if (cnt==0) then
    print *, 'ERROR: splitting year 3'
    stop
  endif
  cnt3 = cnt
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Sort 5
  i=1
  cnt=0
  DO WHILE (i <= nobs)
    if (date_time(i)>=2014052100 .and. date_time(i)<=2014070524) then
    !if (date_time(i)>=2013052100 .and. date_time(i)<=2013070524) then 
    !if (date_time(i)>=2012052100 .and. date_time(i)<=2012070524) then
    !if (date_time(i)>=2011052100 .and. date_time(i)<=2011070524) then
    !if (date_time(i)>=2010111500 .and. date_time(i)<=2011123124) then
    !if (date_time(i)>=2009110100 .and. date_time(i)<=2009123124) then
      cnt=i
      i=nobs
    endif
    i = i+1;
  ENDDO
  cnt = cnt-1-cnt1-cnt2-cnt3 
  if (cnt==0) then
    print *, 'ERROR: splitting year 4'
    stop
  endif
  cnt4 = cnt
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Sort 6
  i=1
  cnt=0
  DO WHILE (i <= nobs)
     if (date_time(i)>=2014070600 .and. date_time(i)<=2014081824) then
    !if (date_time(i)>=2013070600 .and. date_time(i)<=2013081824) then
    !if (date_time(i)>=2012070600 .and. date_time(i)<=2012081824) then
    !if (date_time(i)>=2011070600 .and. date_time(i)<=2011081824) then
    !if (date_time(i)>=2010111500 .and. date_time(i)<=2011123124) then
    !if (date_time(i)>=2009110100 .and. date_time(i)<=2009123124) then
      cnt=i
      i=nobs
    endif
    i = i+1;
  ENDDO
  cnt = cnt-1-cnt1-cnt2-cnt3-cnt4
  if (cnt==0) then
    print *, 'ERROR: splitting year 5'
    stop
  endif
  cnt5 = cnt 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Sort 7
  i=1
  cnt=0
  DO WHILE (i <= nobs)
     if (date_time(i)>=2014081900 .and. date_time(i)<=2014092824) then
    !if (date_time(i)>=2013081900 .and. date_time(i)<=2013093124) then
    !if (date_time(i)>=2012081900 .and. date_time(i)<=2012093124) then
    !if (date_time(i)>=2011081900 .and. date_time(i)<=2011093124) then
    !if (date_time(i)>=2011010100 .and. date_time(i)<=2011013124) then
    !if (date_time(i)>=2010010100 .and. date_time(i)<=2010013124) then
      cnt=i
      i=nobs
    endif
    i = i+1;
  ENDDO
  cnt = cnt-1-cnt1-cnt2-cnt3-cnt4-cnt5
  if (cnt==0) then
    print *, 'ERROR: splitting year 6'
    stop
  endif
  cnt6 = cnt
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Sort8
  i=1
  cnt=0
  DO WHILE (i <= nobs)
     if (date_time(i)>=2014092900 .and. date_time(i)<=2014092924) then
    !if (date_time(i)>=2013100100 .and. date_time(i)<=2013100924) then
    !if (date_time(i)>=2012100100 .and. date_time(i)<=2012111024) then
    !if (date_time(i)>=2011100100 .and. date_time(i)<=2011111024) then
    !if (date_time(i)>=2011010100 .and. date_time(i)<=2011013124) then
    !if (date_time(i)>=2010010100 .and. date_time(i)<=2010013124) then
      cnt=i
      i=nobs
    endif
    i = i+1;
  ENDDO
  cnt = cnt-1-cnt1-cnt2-cnt3-cnt4-cnt5-cnt6
  if (cnt==0) then
    print *, 'ERROR: splitting year 7'
    stop
  endif
  cnt7 = cnt
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Sort9
  i=1
  cnt=0
  DO WHILE (i <= nobs)
     if (date_time(i)>=2014093000 .and. date_time(i)<=2014093024) then
    !if (date_time(i)>=2013101000 .and. date_time(i)<=2013101424) then
    !if (date_time(i)>=2012111600 .and. date_time(i)<=2012122524) then
    !if (date_time(i)>=2011111600 .and. date_time(i)<=2011122524) then
    !if (date_time(i)>=2011010100 .and. date_time(i)<=2011013124) then
    !if (date_time(i)>=2010010100 .and. date_time(i)<=2010013124) then
      cnt=i
      i=nobs
    endif
    i = i+1;
  ENDDO
  cnt = cnt-1-cnt1-cnt2-cnt3-cnt4-cnt5-cnt6-cnt7
  if (cnt==0) then
    print *, 'ERROR: splitting year 8'
    stop
  endif
  cnt8 = cnt
 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Sort10
  i=1
  cnt=0
  DO WHILE (i <= nobs)
     if (date_time(i)>=2014100100 .and. date_time(i)<=2014123124) then
    !if (date_time(i)>=2013101500 .and. date_time(i)<=2013123124) then
    !if (date_time(i)>=2012122600 .and. date_time(i)<=2013011024) then
    !if (date_time(i)>=2011122600 .and. date_time(i)<=2012013124) then
    !if (date_time(i)>=2011010100 .and. date_time(i)<=2011013124) then
    !if (date_time(i)>=2010010100 .and. date_time(i)<=2010013124) then
      cnt=i
      i=nobs
    endif
    i = i+1;
  ENDDO
  cnt = cnt-1-cnt1-cnt2-cnt3-cnt4-cnt5-cnt6-cnt7-cnt8
  if (cnt==0) then
    print *, 'ERROR: splitting year 9'
    stop
  endif
  cnt9 = cnt
  
 
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! END
  cnt10 = nobs-cnt9-cnt8-cnt7-cnt6-cnt5-cnt4-cnt3-cnt2-cnt1
  !print *, cnt1, cnt2, cnt3, cnt4, cnt5, cnt6, cnt7, cnt8, cnt9, cnt10
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Sort 1
   allocate ( date1(cnt1),  data_id1(cnt1), lat1(cnt1), lon1(cnt1), npts1(cnt1) )
   allocate ( z1(nlevs,cnt1), t1(nlevs,cnt1), qc1(nlevs,cnt1)   )
   date1     = date_time(1:cnt1)
   data_id1 = data_id(1:cnt1)
   lat1      = lat(1:cnt1)
   lon1      = lon(1:cnt1)
   npts1     = npts(1:cnt1)
   z1        = z(:,1:cnt1)
   t1        = t(:,1:cnt1)
   qc1      = qc_lev(:,1:cnt1)
   call SORT_ODAS2_ARGO(date1,lon1,lat1,npts1,data_id1,z1,t1,qc1,nlevs,cnt1)
   date_time(1:cnt1) = date1
   deallocate (date1)
   data_id(1:cnt1)   = data_id1
   deallocate (data_id1)
   lat(1:cnt1)       = lat1
   deallocate (lat1)
   lon(1:cnt1)       = lon1
   deallocate (lon1)
   npts(1:cnt1)      = npts1
   deallocate (npts1)
   z(:,1:cnt1)       = z1
   deallocate (z1)
   t(:,1:cnt1)       = t1
   deallocate (t1)
   qc_lev(:,1:cnt1)  = qc1
   deallocate (qc1)
   print *, 'Sort 1 complete: 1',' - ',cnt1

! Sort 2
   allocate ( date1(cnt2),  data_id1(cnt2), lat1(cnt2), lon1(cnt2), npts1(cnt2) )
   allocate ( z1(nlevs,cnt2), t1(nlevs,cnt2), qc1(nlevs,cnt2)   )
   date1     = date_time(cnt1+1:cnt1+cnt2)
   data_id1 = data_id(cnt1+1:cnt1+cnt2)
   lat1      = lat(cnt1+1:cnt1+cnt2)
   lon1      = lon(cnt1+1:cnt1+cnt2)
   npts1     = npts(cnt1+1:cnt1+cnt2)
   z1        = z(:,cnt1+1:cnt1+cnt2)
   t1        = t(:,cnt1+1:cnt1+cnt2)
   qc1       = qc_lev(:,cnt1+1:cnt1+cnt2)
   call SORT_ODAS2_ARGO(date1,lon1,lat1,npts1,data_id1,z1,t1,qc1,nlevs,cnt2)
   date_time(cnt1+1:cnt1+cnt2) = date1
   deallocate (date1)
   data_id(cnt1+1:cnt1+cnt2)  = data_id1
   deallocate (data_id1)
   lat(cnt1+1:cnt1+cnt2)       = lat1
   deallocate (lat1)
   lon(cnt1+1:cnt1+cnt2)       = lon1
   deallocate (lon1)
   npts(cnt1+1:cnt1+cnt2)      = npts1
   deallocate (npts1)
   z(:,cnt1+1:cnt1+cnt2)       = z1
   deallocate (z1)
   t(:,cnt1+1:cnt1+cnt2)       = t1
   deallocate (t1)
    qc_lev(:,cnt1+1:cnt1+cnt2)  = qc1
   deallocate (qc1)
   print *, 'Sort 2 complete: ',cnt1+1,' - ',cnt1+cnt2

! Sort 3
   allocate ( date1(cnt3),  data_id1(cnt3), lat1(cnt3), lon1(cnt3), npts1(cnt3) )
   allocate ( z1(nlevs,cnt3), t1(nlevs,cnt3), qc1(nlevs,cnt3)   )
   date1     = date_time(cnt1+cnt2+1:cnt1+cnt2+cnt3)
   data_id1 = data_id(cnt1+cnt2+1:cnt1+cnt2+cnt3)
   lat1      = lat(cnt1+cnt2+1:cnt1+cnt2+cnt3)
   lon1      = lon(cnt1+cnt2+1:cnt1+cnt2+cnt3)
   npts1     = npts(cnt1+cnt2+1:cnt1+cnt2+cnt3)
   z1        = z(:,cnt1+cnt2+1:cnt1+cnt2+cnt3)
   t1        = t(:,cnt1+cnt2+1:cnt1+cnt2+cnt3)
   qc1       = qc_lev(:,cnt1+cnt2+1:cnt1+cnt2+cnt3)
   call SORT_ODAS2_ARGO(date1,lon1,lat1,npts1,data_id1,z1,t1,qc1,nlevs,cnt3)
   date_time(cnt1+cnt2+1:cnt1+cnt2+cnt3) = date1
   deallocate (date1)
   data_id(cnt1+cnt2+1:cnt1+cnt2+cnt3)  = data_id1
   deallocate (data_id1)
   lat(cnt1+cnt2+1:cnt1+cnt2+cnt3)       = lat1
   deallocate (lat1)
   lon(cnt1+cnt2+1:cnt1+cnt2+cnt3)       = lon1
   deallocate (lon1)
   npts(cnt1+cnt2+1:cnt1+cnt2+cnt3)      = npts1
   deallocate (npts1)
   z(:,cnt1+cnt2+1:cnt1+cnt2+cnt3)       = z1
   deallocate (z1)
   t(:,cnt1+cnt2+1:cnt1+cnt2+cnt3)       = t1
   deallocate (t1)
   qc_lev(:,cnt1+cnt2+1:cnt1+cnt2+cnt3)  = qc1
   deallocate (qc1)
   !deallocate (date1, data_id1, lat1, lon1, npts1, z1, t1, qc1)   
   print *, 'Sort 3 complete: ',cnt1+cnt2+1,' - ',cnt1+cnt2+cnt3

! Sort 4
   allocate ( date1(cnt4),  data_id1(cnt4), lat1(cnt4), lon1(cnt4), npts1(cnt4) )
   allocate ( z1(nlevs,cnt4), t1(nlevs,cnt4), qc1(nlevs,cnt4)   )
   date1     = date_time(cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4)
   data_id1 = data_id(cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4)
   lat1      = lat(cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4)
   lon1      = lon(cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4)
   npts1     = npts(cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4)
   z1        = z(:,cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4)
   t1        = t(:,cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4)
   qc1       = qc_lev(:,cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4)
   call SORT_ODAS2_ARGO(date1,lon1,lat1,npts1,data_id1,z1,t1,qc1,nlevs,cnt4)
   date_time(cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4) = date1
   deallocate (date1)
   data_id(cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4)   = data_id1
   deallocate (data_id1)
   lat(cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4)       = lat1
   deallocate (lat1)
   lon(cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4)       = lon1
   deallocate (lon1)
   npts(cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4)      = npts1
   deallocate (npts1)
   z(:,cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4)       = z1
   deallocate (z1)
   t(:,cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4)       = t1
   deallocate (t1)
   qc_lev(:,cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4)  = qc1
   deallocate (qc1)
   print *, 'Sort 4 complete: ',cnt1+cnt2+cnt3+1,' - ',cnt1+cnt2+cnt3+cnt4

! Sort 5
   allocate (    date1(cnt5), data_id1(cnt5),      lat1(cnt5),  lon1(cnt5) )
   allocate ( z1(nlevs,cnt5), t1(nlevs,cnt5), qc1(nlevs,cnt5), npts1(cnt5)   )

   date1     = date_time(cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5)
   data_id1  =   data_id(cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5)
   lat1      =       lat(cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5)
   lon1      =       lon(cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5)
   npts1     =      npts(cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5)
   z1        =       z(:,cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5)
   t1        =       t(:,cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5)
   qc1       =  qc_lev(:,cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5)
   call SORT_ODAS2_ARGO(date1,lon1,lat1,npts1,data_id1,z1,t1,qc1,nlevs,cnt5)
   date_time(cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5) = date1
   deallocate (date1)
     data_id(cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5) = data_id1
   deallocate (data_id1)
         lat(cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5) = lat1
   deallocate (lat1)
         lon(cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5) = lon1
   deallocate (lon1)
        npts(cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5) = npts1
   deallocate (npts1)
         z(:,cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5) = z1
   deallocate (z1)
         t(:,cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5) = t1
   deallocate (t1)
    qc_lev(:,cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5) = qc1
   deallocate (qc1)
   print *, 'Sort 5 complete: ',cnt1+cnt2+cnt3+cnt4+1,' - ',cnt1+cnt2+cnt3+cnt4+cnt5

! Sort 6
   allocate (    date1(cnt6), data_id1(cnt6),      lat1(cnt6),  lon1(cnt6) )
   allocate ( z1(nlevs,cnt6), t1(nlevs,cnt6), qc1(nlevs,cnt6), npts1(cnt6)   )
   date1     = date_time(cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6)
   data_id1  =   data_id(cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6)
   lat1      =       lat(cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6)
   lon1      =       lon(cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6)
   npts1     =      npts(cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6)
   z1        =       z(:,cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6)
   t1        =       t(:,cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6)
   qc1       =  qc_lev(:,cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6)
   call SORT_ODAS2_ARGO(date1,lon1,lat1,npts1,data_id1,z1,t1,qc1,nlevs,cnt6)
   date_time(cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6) = date1
   deallocate (date1)
     data_id(cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6) = data_id1
   deallocate (data_id1)
         lat(cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6) = lat1
   deallocate (lat1)
         lon(cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6) = lon1
   deallocate (lon1)
        npts(cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6) = npts1
   deallocate (npts1)
         z(:,cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6) = z1
   deallocate (z1)
         t(:,cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6) = t1
   deallocate (t1)
    qc_lev(:,cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6) = qc1
   deallocate (qc1)
   print *, 'Sort 6 complete: ',cnt1+cnt2+cnt3+cnt4+cnt5+1,' - ',cnt1+cnt2+cnt3+cnt4+cnt5+cnt6
!
! Sort 7
   allocate (    date1(cnt7), data_id1(cnt7),      lat1(cnt7),  lon1(cnt7) )
   allocate ( z1(nlevs,cnt7), t1(nlevs,cnt7), qc1(nlevs,cnt7), npts1(cnt7)   )
   date1     = date_time(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7)
   data_id1  =   data_id(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7)
   lat1      =       lat(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7)
   lon1      =       lon(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7)
   npts1     =      npts(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7)
   z1        =       z(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7)
   t1        =       t(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7)
   qc1       =  qc_lev(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7)
   call SORT_ODAS2_ARGO(date1,lon1,lat1,npts1,data_id1,z1,t1,qc1,nlevs,cnt7)
   date_time(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7) = date1
   deallocate (date1)
     data_id(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7) = data_id1
   deallocate (data_id1)
         lat(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7) = lat1
   deallocate (lat1)
         lon(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7) = lon1
   deallocate (lon1)
        npts(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7) = npts1
   deallocate (npts1) 
         z(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7) = z1
   deallocate (z1)
         t(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7) = t1
   deallocate (t1)
    qc_lev(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7) = qc1
   deallocate (qc1)
   print *, 'Sort 7 complete: ',cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1,' - ',cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7

! Sort 8
   allocate (    date1(cnt8), data_id1(cnt8),      lat1(cnt8),  lon1(cnt8) )
   allocate ( z1(nlevs,cnt8), t1(nlevs,cnt8), qc1(nlevs,cnt8), npts1(cnt8)   )
   date1     = date_time(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8)
   data_id1  =   data_id(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8)
   lat1      =       lat(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8)
   lon1      =       lon(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8)
   npts1     =      npts(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8)
   z1        =       z(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8)
   t1        =       t(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8)
   qc1       =  qc_lev(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8)
   call SORT_ODAS2_ARGO(date1,lon1,lat1,npts1,data_id1,z1,t1,qc1,nlevs,cnt8)
   date_time(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8) = date1
   deallocate (date1)
     data_id(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8) = data_id1
   deallocate (data_id1)
         lat(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8) = lat1
   deallocate (lat1)
         lon(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8) = lon1
   deallocate (lon1)
        npts(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8) = npts1
   deallocate (npts1) 
         z(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8) = z1
   deallocate (z1)
         t(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8) = t1
   deallocate (t1)
    qc_lev(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8) = qc1
   deallocate (qc1)
   print *, 'Sort 8 complete: ',cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1,' - ',cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8

! Sort 9
   allocate (    date1(cnt9), data_id1(cnt9),      lat1(cnt9),  lon1(cnt9) )
   allocate ( z1(nlevs,cnt9), t1(nlevs,cnt9), qc1(nlevs,cnt9), npts1(cnt9)   )
   date1     = date_time(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9)
   data_id1  =   data_id(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9)
   lat1      =       lat(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9)
   lon1      =       lon(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9)
   npts1     =      npts(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9)
   z1        =       z(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9)
   t1        =       t(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9)
   qc1       =  qc_lev(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9)
   call SORT_ODAS2_ARGO(date1,lon1,lat1,npts1,data_id1,z1,t1,qc1,nlevs,cnt9)
   date_time(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9) = date1
   deallocate (date1)
     data_id(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9) = data_id1
   deallocate (data_id1)
         lat(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9) = lat1
   deallocate (lat1)
         lon(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9) = lon1
   deallocate (lon1)
        npts(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9) = npts1
   deallocate (npts1)
         z(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9) = z1
   deallocate (z1)
         t(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9) = t1
   deallocate (t1)
    qc_lev(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9) = qc1
   deallocate (qc1)
   print *, 'Sort 9 complete: ',cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1,' - ',cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9


! Sort END
   allocate (    date1(cnt10),  data_id1(cnt10),      lat1(cnt10),  lon1(cnt10)   )
   allocate ( z1(nlevs,cnt10),  t1(nlevs,cnt10), qc1(nlevs,cnt10), npts1(cnt10)   )
   date1     = date_time(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:nobs)
   data_id1  =   data_id(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:nobs)
   lat1      =       lat(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:nobs)
   lon1      =       lon(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:nobs)
   npts1     =      npts(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:nobs)
   z1        =       z(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:nobs)
   t1        =       t(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:nobs)
   qc1       =  qc_lev(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:nobs)
   call SORT_ODAS2_ARGO(date1,lon1,lat1,npts1,data_id1,z1,t1,qc1,nlevs,cnt10)
   date_time(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:nobs) = date1
   deallocate (date1)
     data_id(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:nobs) = data_id1
   deallocate (data_id1)
         lat(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:nobs) = lat1
   deallocate (lat1)
         lon(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:nobs) = lon1
   deallocate (lon1)
        npts(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:nobs) = npts1
   deallocate (npts1)
         z(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:nobs) = z1
   deallocate (z1)
         t(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:nobs) = t1
   deallocate (t1)
    qc_lev(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:nobs) = qc1
   deallocate (qc1)

   !deallocate (date1, data_id1, lat1, lon1, npts1, z1, t1, qc1)   
   print *, 'Sort END complete: ',cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1,' - ',nobs


else
   call SORT_ODAS2_ARGO(date_time,lon,lat,npts,data_id,z,t,qc_lev,nlevs,nobs)
endif

!fname_out = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/assim/ARGO/V3/STEP2/' // svar0 // '_ARGO_' // syear // '.nc'
fname_out = '/discover/nobackup/lren1/pre_proc/NRT/ARGO/STEP2/' // svar0 // '_ARGO_' // syear // '.nc'

inquire (file=fname_out(1:len_trim(fname_out)), exist=exist)
!print *, 'Output File ', trim(fname_out)

if (year==2013) then
! 1:cnt1
  max_nobs = cnt1
  nobs = cnt1
     call write_netcdf(fname_out,max_nobs,nobs,max_npts,inst_error,var_id,miss, &
	     date_time(1:cnt1),lon(1:cnt1),lat(1:cnt1),npts(1:cnt1),qc_flag(1:cnt1), &
             data_id(1:cnt1),inst_id(1:cnt1),qc_prf(1:cnt1),z(:,1:cnt1),t(:,1:cnt1), &
             qc_lev(:,1:cnt1),obs_err(:,1:cnt1),title,source,svar)

! cnt1+1 : cnt1+cnt2
  max_nobs = cnt2
  nobs = cnt2
  call append_netcdf(fname_out,max_nobs,nobs,max_npts,inst_error,var_id,miss, &
	date_time(cnt1+1:cnt1+cnt2),lon(cnt1+1:cnt1+cnt2),lat(cnt1+1:cnt1+cnt2), &
	npts(cnt1+1:cnt1+cnt2),qc_flag(cnt1+1:cnt1+cnt2),data_id(cnt1+1:cnt1+cnt2), &
        inst_id(cnt1+1:cnt1+cnt2),qc_prf(cnt1+1:cnt1+cnt2),z(:,cnt1+1:cnt1+cnt2), &
        t(:,cnt1+1:cnt1+cnt2),qc_lev(:,cnt1+1:cnt1+cnt2),obs_err(:,cnt1+1:cnt1+cnt2),svar)

! cnt1+cnt2+1 : cnt1+cnt2+cnt3
  max_nobs = cnt3
  nobs = cnt3
  call append_netcdf(fname_out,max_nobs,nobs,max_npts,inst_error,var_id,miss, &
	date_time(cnt1+cnt2+1:cnt1+cnt2+cnt3),lon(cnt1+cnt2+1:cnt1+cnt2+cnt3), &
        lat(cnt1+cnt2+1:cnt1+cnt2+cnt3),npts(cnt1+cnt2+1:cnt1+cnt2+cnt3), &
        qc_flag(cnt1+cnt2+1:cnt1+cnt2+cnt3),data_id(cnt1+cnt2+1:cnt1+cnt2+cnt3), &
        inst_id(cnt1+cnt2+1:cnt1+cnt2+cnt3),qc_prf(cnt1+cnt2+1:cnt1+cnt2+cnt3), &
        z(:,cnt1+cnt2+1:cnt1+cnt2+cnt3),t(:,cnt1+cnt2+1:cnt1+cnt2+cnt3), &
        qc_lev(:,cnt1+cnt2+1:cnt1+cnt2+cnt3),obs_err(:,cnt1+cnt2+1:cnt1+cnt2+cnt3),svar)

! cnt1+cnt2+cnt3+1 : cnt1+cnt2+cnt3+cnt4
  max_nobs = cnt4
  nobs = cnt4
  call append_netcdf(fname_out,max_nobs,nobs,max_npts,inst_error,var_id,miss, &
	date_time(cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4),lon(cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4), &
        lat(cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4),npts(cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4), &
        qc_flag(cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4),data_id(cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4), &
        inst_id(cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4),qc_prf(cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4), &
        z(:,cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4),t(:,cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4), &
        qc_lev(:,cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4),obs_err(:,cnt1+cnt2+cnt3+1:cnt1+cnt2+cnt3+cnt4),svar)

! cnt1+cnt2+cnt3+cnt4+1 : cnt1+cnt2+cnt3+cnt4+cnt5
  max_nobs = cnt5
  nobs = cnt5
  call append_netcdf(fname_out,max_nobs,nobs,max_npts,inst_error,var_id,miss, &
	date_time(cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5),lon(cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5), &
        lat(cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5),npts(cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5), &
        qc_flag(cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5),data_id(cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5), &
        inst_id(cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5),qc_prf(cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5), &
        z(:,cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5),t(:,cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5), &
        qc_lev(:,cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5),obs_err(:,cnt1+cnt2+cnt3+cnt4+1:cnt1+cnt2+cnt3+cnt4+cnt5),svar)

! cnt1+cnt2+cnt3+cnt4+cnt5+1 : cnt1+cnt2+cnt3+cnt4+cnt5+cnt6
  max_nobs = cnt6
  nobs = cnt6
  call append_netcdf(fname_out,max_nobs,nobs,max_npts,inst_error,var_id,miss, &
	date_time(cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6),&
        lon(cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6),      &
        lat(cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6),      &
        npts(cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6),     &
        qc_flag(cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6),  &
        data_id(cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6),  &
        inst_id(cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6),  &
        qc_prf(cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6),   &
        z(:,cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6),      &
        t(:,cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6),      &
        qc_lev(:,cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6), &
        obs_err(:,cnt1+cnt2+cnt3+cnt4+cnt5+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6),svar)

! cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1 : cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7
  max_nobs = cnt7
  nobs = cnt7
  call append_netcdf(fname_out,max_nobs,nobs,max_npts,inst_error,var_id,miss,  &
	date_time(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7), &
              lon(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7), &
              lat(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7), &
             npts(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7), &
          qc_flag(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7), &
          data_id(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7), &
          inst_id(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7), &
           qc_prf(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7), &
              z(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7), &
              t(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7), &
         qc_lev(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7), &
        obs_err(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7),svar)

! cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1 : cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8
  max_nobs = cnt8
  nobs = cnt8
  call append_netcdf(fname_out,max_nobs,nobs,max_npts,inst_error,var_id,miss,  &
	date_time(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8), &
              lon(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8), &
              lat(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8), &
             npts(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8), &
          qc_flag(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8), &
          data_id(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8), &
          inst_id(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8), &
           qc_prf(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8), &
              z(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8), &
              t(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8), &
         qc_lev(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8), &
        obs_err(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8),svar)

! cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1 : cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9
  max_nobs = cnt9
  nobs = cnt9
  call append_netcdf(fname_out,max_nobs,nobs,max_npts,inst_error,var_id,miss,  &
	date_time(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9), &
              lon(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9), &
              lat(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9), &
             npts(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9), &
          qc_flag(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9), &
          data_id(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9), &
          inst_id(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9), &
           qc_prf(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9), &
              z(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9), &
              t(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9), &
         qc_lev(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9), &
        obs_err(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9),svar)

! cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1 : cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+cnt10
  max_nobs = cnt10
  nobs = cnt10
  call append_netcdf(fname_out,max_nobs,nobs,max_npts,inst_error,var_id,miss,  &
	date_time(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+cnt10), &
              lon(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+cnt10), &
              lat(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+cnt10), &
             npts(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+cnt10), &
          qc_flag(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+cnt10), &
          data_id(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+cnt10), &
          inst_id(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+cnt10), &
           qc_prf(cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+cnt10), &
              z(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+cnt10), &
              t(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+cnt10), &
         qc_lev(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+cnt10), &
        obs_err(:,cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+1:cnt1+cnt2+cnt3+cnt4+cnt5+cnt6+cnt7+cnt8+cnt9+cnt10),svar)

else
     print *, 'Max Obs ',max_nobs, nobs, max_npts
     print *, date_time(1), maxval(date_time)
     call write_netcdf(fname_out,max_nobs,nobs,max_npts,inst_error,var_id,miss, &
	     date_time,lon,lat,npts,qc_flag, &
             data_id,inst_id,qc_prf,z,t,qc_lev,obs_err,title,source,svar)

endif

END PROGRAM read_argo_2
