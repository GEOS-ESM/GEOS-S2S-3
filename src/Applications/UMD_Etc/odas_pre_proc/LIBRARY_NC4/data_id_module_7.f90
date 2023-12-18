MODULE DATA_ID_MODULE_7
  
  IMPLICIT NONE
  PUBLIC :: get_wmo_tao, get_wmo_pir, get_wmo_rama
  PUBLIC :: get_inst_id, get_inst_id_pos
  PUBLIC :: get_wmo_arg, is_argo_master
  PUBLIC :: get_wmo_ctd, is_ctd_master
  PUBLIC :: get_wmo_xbt, is_xbt_master
  PUBLIC :: wmo_to_data_id, make_data_id
  PUBLIC :: s_eq, ch_cap
  CONTAINS
  
  ! ..........................................................................
    SUBROUTINE get_wmo_tao (max_cnt,cnt,wmo,lon_tao,lat_tao)    
  ! ..........................................................................
  ! Read WMO numbers from ascii file
  !
    IMPLICIT NONE
    integer                  :: max_cnt        
    integer,     intent(out) :: cnt
    character*8, intent(out) :: wmo(max_cnt) 
    character*6              :: tlon
    character*4              :: tlat
    real,        intent(out) :: lon_tao(max_cnt), lat_tao(max_cnt)               
    integer                  :: ieof,UNIT=10
    character*256            :: fname  
        
   fname = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/PRE/MOOR/TAO/tao_pos.ascii' 
   open (UNIT, file=fname(1:len_trim(fname)), status='old', form='formatted')  
   ieof = 0
   cnt = 1 
   DO WHILE (ieof == 0)
     read (UNIT, '(8a,6a,4a)',iostat = ieof) wmo(cnt), tlon, tlat     
     read (tlon,*) lon_tao(cnt)
     read (tlat,*) lat_tao(cnt) 
     !print *, cnt, wmo(cnt), tlon, tlat
     ! SWITCH TO 0/360 LONGITUDE RANGE
       if (lon_tao(cnt) < 0.) then
           lon_tao(cnt) = lon_tao(cnt) + 360.
       endif                             
     if (ieof /= 0) exit
     cnt = cnt + 1             
   ENDDO 
   close (UNIT)      
   cnt = cnt-1;  
  return
  END SUBROUTINE get_wmo_tao

 ! ..........................................................................
    SUBROUTINE get_wmo_pir (max_cnt,cnt,wmo,lon_pir,lat_pir)    
  ! ..........................................................................
  ! Read WMO numbers from ascii file
  !
    IMPLICIT NONE
    integer                  :: max_cnt        
    integer,     intent(out) :: cnt
    character*8, intent(out) :: wmo(max_cnt) 
    character*6              :: plon
    character*4              :: plat
    real,        intent(out) :: lon_pir(max_cnt), lat_pir(max_cnt)               
    integer                  :: ieof,UNIT=10
    character*256            :: fname  
        
   fname = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/PRE/MOOR/PIRATA/pir_pos.ascii' 
   open (UNIT, file=fname(1:len_trim(fname)), status='old', form='formatted')  
   ieof = 0
   cnt = 1 
   DO WHILE (ieof == 0)
     read (UNIT, '(8a,6a,4a)',iostat = ieof) wmo(cnt), plon, plat     
     read (plon,*) lon_pir(cnt)
     read (plat,*) lat_pir(cnt) 
     !print *, cnt, wmo(cnt), plon, plat
     ! SWITCH TO 0/360 LONGITUDE RANGE
       if (lon_pir(cnt) < 0.) then
           lon_pir(cnt) = lon_pir(cnt) + 360.
       endif                             
     if (ieof /= 0) exit
     cnt = cnt + 1             
   ENDDO 
   close (UNIT)      
   cnt = cnt-1;  
  return
  END SUBROUTINE get_wmo_pir


 ! ..........................................................................
    SUBROUTINE get_wmo_rama (max_cnt,cnt,wmo,lon_rama,lat_rama)    
  ! ..........................................................................
  ! Read WMO numbers from ascii file
  !
    IMPLICIT NONE
    integer                  :: max_cnt        
    integer,     intent(out) :: cnt
    character*8, intent(out) :: wmo(max_cnt) 
    character*6              :: plon
    character*4              :: plat
    real,        intent(out) :: lon_rama(max_cnt), lat_rama(max_cnt)               
    integer                  :: ieof,UNIT=10
    character*256            :: fname  
        
   fname = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/PRE/MOOR/RAMA/rama_pos.ascii' 
   open (UNIT, file=fname(1:len_trim(fname)), status='old', form='formatted')  
   ieof = 0
   cnt = 1 
   DO WHILE (ieof == 0)
     read (UNIT, '(8a,6a,4a)',iostat = ieof) wmo(cnt), plon, plat     
     if (ieof /= 0) exit
     !print *, cnt, wmo(cnt), plon, plat
     read (plon,*) lon_rama(cnt)
     read (plat,*) lat_rama(cnt) 
     ! SWITCH TO 0/360 LONGITUDE RANGE
       if (lon_rama(cnt) < 0.) then
           lon_rama(cnt) = lon_rama(cnt) + 360.
       endif                             
     cnt = cnt + 1             
   ENDDO 
   close (UNIT)    
   cnt = cnt-1;  
  return
  END SUBROUTINE get_wmo_rama


  ! ..............................................
    SUBROUTINE get_wmo_arg (max_cnt,cnt,wmo,list)    
  ! ..............................................
  ! Read ARGO WMO numbers from ascii file
  ! list = 0 for master list
  ! list = 1 for current list
    IMPLICIT NONE
    integer, intent(in)      :: max_cnt, list       
    integer,     intent(out) :: cnt
    character*8, intent(out) :: wmo(max_cnt)
    integer                  :: ieof,UNIT=10
    character*256            :: fname
        
    if (list==0) then
      fname = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/ARGO/PRF/ARGO_WMO_MASTER.txt' 
    elseif (list==1) then
      fname = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/ARGO/PRF/ARGO_WMO_CURRENT.txt' 
    endif
    open (UNIT, file=fname(1:len_trim(fname)), status='old', form='formatted')  
    ieof = 0
    cnt = 1 
    DO WHILE (ieof == 0)
     read (UNIT, '(8a)',iostat = ieof) wmo(cnt)
     if (ieof /= 0) exit
     cnt = cnt + 1             
    ENDDO
    cnt = cnt-1
    close (UNIT)
   return
  END SUBROUTINE get_wmo_arg

  ! ..............................................
    SUBROUTINE get_wmo_ctd (max_cnt,cnt,wmo,list)    
  ! ..............................................
  ! Read CTD WMO numbers from ascii file
  ! list = 0 for master list
  ! list = 1 for current list
    IMPLICIT NONE
    integer, intent(in)      :: max_cnt, list       
    integer,     intent(out) :: cnt
    character*8, intent(out) :: wmo(max_cnt)
    integer                  :: ieof,UNIT=10
    character*256            :: fname
        
    if (list==0) then
      fname = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/METOFFICE/CTD/MET_CTD_WMO_MASTER.txt' 
    elseif (list==1) then
      fname = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/METOFFICE/CTD/MET_CTD_WMO_CURRENT.txt' 
    endif
    open (UNIT, file=fname(1:len_trim(fname)), status='old', form='formatted')  
    ieof = 0
    cnt = 1 
    DO WHILE (ieof == 0)
     read (UNIT, '(8a)',iostat = ieof) wmo(cnt)
     if (ieof /= 0) exit
     cnt = cnt + 1             
    ENDDO
    cnt = cnt-1
    close (UNIT)
   return
  END SUBROUTINE get_wmo_ctd

  ! ..............................................
    SUBROUTINE get_wmo_xbt (max_cnt,cnt,wmo,list,src)    
  ! ..............................................
  ! Read XBT WMO numbers from ascii file
  ! list = 0 for master list
  ! list = 1 for current list
  ! src  = meto,ncep,both
    IMPLICIT NONE
    integer, intent(in)      :: max_cnt, list   
    character*4, intent(in)  :: src    
    integer,     intent(out) :: cnt
    character*8, intent(out) :: wmo(max_cnt)
    integer                  :: ieof,UNIT=10
    character*256            :: fname
        
    if (list==0) then
      if (src=='meto') then
        fname = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/METOFFICE//XBT/MET_XBT_WMO_MASTER.txt' 
      elseif (src=='ncep') then
        fname = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/PRE/NCEP/NCEP_XBT_WMO_MASTER.txt' 
      elseif (src=='both') then
        fname = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/PRE/XBT_WMO_MASTER.txt' 
      endif
    elseif (list==1) then
      if (src=='meto') then
        fname = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/METOFFICE/XBT/MET_XBT_WMO_CURRENT.txt'
      elseif (src=='ncep') then
        fname = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/PRE/NCEP/NCEP_XBT_WMO_CURRENT.txt' 
      endif
    endif
    !print *, trim(fname)
    open (UNIT, file=fname(1:len_trim(fname)), status='old', form='formatted')  
    ieof = 0
    cnt = 1 
    DO WHILE (ieof == 0)
     read (UNIT, '(8a)',iostat = ieof) wmo(cnt)
     if (ieof /= 0) exit
     cnt = cnt + 1             
    ENDDO
    cnt = cnt-1
    close (UNIT)
    !print *, max_cnt,cnt
   return
  END SUBROUTINE get_wmo_xbt


  ! ..........................................................................
    SUBROUTINE get_inst_id (nobs,idate,itao,ipir,iarg,irama,wmo_tao,wmo_pir, &
                           wmo_arg,wmo_rama,lon,lat,lon_tao,lat_tao,lon_pir, &
			   lat_pir,lon_rama,lat_rama,stn_name,inst_id)    
  ! ..........................................................................
  ! Get inst_id
  !
    IMPLICIT NONE
    integer,                      intent(in)    :: nobs 
    integer,     dimension(nobs), intent(in)    :: idate
    integer,                      intent(in)    :: itao, ipir, iarg, irama
    character*8, dimension(nobs), intent(in)    :: wmo_tao, wmo_pir, wmo_arg, wmo_rama 
    real,        dimension(nobs), intent(in)    :: lon, lat
    real,        dimension(itao), intent(in)    :: lon_tao, lat_tao
    real,        dimension(ipir), intent(in)    :: lon_pir, lat_pir    
    real,        dimension(ipir), intent(in)    :: lon_rama, lat_rama    
    character*8, dimension(nobs), intent(inout) :: stn_name
    integer,     dimension(nobs), intent(inout) :: inst_id
    integer,     dimension(nobs)                :: badpos   
    integer,     dimension(nobs)                :: istao, ispir, isarg, israma
    integer                                     :: i, tt, pp, aa, rr
    integer                                     :: scan_c, scan_q, scan_9
    real,        dimension(nobs)                :: lon2   
    
    DO i = 1, nobs 
    ! SWITCH TO 0/360 LONGITUDE RANGE
       lon2(i) = lon(i)
       if (lon(i) < 0) then
          lon2(i) = lon(i) + 360.
       endif    
                         
    ! ARGO Floats
    !  ................................................................
        !print *, i, stn_name(i)
        do aa=1,iarg
           scan_q = index(stn_name(i),"Q",.false.)
           scan_9 = index(stn_name(i),"9",.false.) 
           if ( idate(i) >= 19950101 .and. (scan_q==1 .and. scan_9==3) ) then
                 inst_id(i) = 508
           endif
           if ( idate(i) >= 19950101 .and. &
                (trim(adjustl(stn_name(i))) .eq. trim(adjustl(wmo_arg(aa)))) ) then
                 inst_id(i) = 508
           endif
        enddo
  

 
    !  TAO ARRAY
    !  ................................................................
       do tt=1,itao
          scan_c = index(stn_name(i),"c",.false.)
          if ( inst_id(i) == 0 ) then
            if ( (stn_name(i) .eq. wmo_tao(tt) .or. scan_c==1) .and. &
                 abs(lon2(i) - lon_tao(tt)) < 0.5 .and. &
                 abs(lat(i) - lat_tao(tt)) < 0.5) then
                inst_id(i)       = 501	
                istao(i)        = tt
     
	       ! NCEP OLD Profiles
	         if (index(stn_name(i),"c110w0")==1) then
                     stn_name(i) = '32323'
                 elseif (index(stn_name(i),"c140w0")==1) then
                     stn_name(i) = '51311'
                 elseif (index(stn_name(i),"c156e0")==1) then
                     stn_name(i) = '52085'
                 elseif (index(stn_name(i),"c165e0")==1) then
                     stn_name(i) = '52321'
                 elseif (index(stn_name(i),"c7n140")==1) then
                     stn_name(i) = '07140'
                 endif
            endif
          endif

          if ( index(stn_name(i),"B",.false.) .eq. 6 .and. &
               stn_name(i)(1:5) == wmo_tao(tt) .and. &
                 abs(lon2(i) - lon_tao(tt)) < 0.5 .and. &
                 abs(lat(i) - lat_tao(tt)) < 0.5) then
               inst_id(i)        = 501
               istao(i)         = tt
          endif 
  
	  if ( scan_c==1 .and. inst_id(i) /= 501 ) then
              !print *, 'c WMO not TAO'
              !print *, idate(i), stn_name(i), lon(i), lat(i)
          endif

       ! MetOffice doesn't have correct WMO for TAO, could be CTD servicing cruise
       ! need to check for tao locations
          if ( inst_id(i) == 0  ) then
            if ( abs(lon2(i) - lon_tao(tt)) < 0.5 .and. &
                 abs(lat(i) - lat_tao(tt)) < 0.5 ) then
               istao(i)   = tt
               inst_id(i) = 513
               !print *, 'TAO CTD', idate(i),lon(i),lon_tao(istao(i)), &
	       !  lat(i),lat_tao(istao(i)),stn_name(i)	
           endif          
         endif

       enddo ! tt tao
       
       ! If not TAO but at TAO location-must be CTD from maintenance cruise
       do tt=1,itao
          if (stn_name(i) == 'WTEU') then
             if ( abs(lon2(i) - lon_tao(tt)) <=1 .and. &
                  abs(lat(i) - lat_tao(tt)) <=1 ) then
                  inst_id(i) = 513		  
                  !print *, 'TAO CTD',idate(i),lon(i),lat(i),stn_name(i),wmo_tao(tt)
                  stn_name(i) = wmo_tao(tt)             
	     endif
          endif
       enddo   
 

    !  PIRATA ARRAY
    !  ................................................................
       do pp=1,ipir
          if ( inst_id(i) == 0 ) then
            if ( stn_name(i) .eq. wmo_pir(pp) .and. &
                 abs(lon2(i) - lon_pir(pp)) < 0.3 .and. &
                 abs(lat(i) - lat_pir(pp)) < 0.3) then
               inst_id(i) = 502
               ispir(i) = pp
               !print *, 'PIR 1 ', stn_name(i), wmo_pir(pp), lon2(i), lat(i)
            endif
          endif
          if ( index(stn_name(i),"B",.false.) .eq. 6 .and. &
               stn_name(i)(1:5) == wmo_pir(pp)) then
               inst_id(i) = 502
               ispir(i) = pp
               !print *, 'PIR 2 ', stn_name(i)(1:5), wmo_pir(pp), lon2(i), lat(i)
          endif 
      ! MetOffice doesn't have correct WMO for TAO, could be CTD servicing cruise
       ! need to check for tao locations
          if ( inst_id(i) == 0 ) then
            if ( abs(lon2(i) - lon_pir(pp)) < 0.3 .and. &
                 abs(lat(i) - lat_pir(pp)) < 0.3 ) then
               !print *, 'PIR: ',idate(i),lon2(i), lon_pir(pp), lat(i), lat_pir(pp),stn_name(i),wmo_tao(pp)
               ispir(i)   = pp
               inst_id(i) = 502
           endif          
         endif
       enddo
       	  

    ! Indian Ocean RAMA Array
    !  ................................................................
       do rr=1,irama
          if ( inst_id(i) == 0 ) then
            if ( stn_name(i) .eq. wmo_rama(rr) .and. & 
                 abs(lon2(i) - lon_rama(pp)) < 1.0 .and. &
                 abs(lat(i) - lat_rama(pp)) < 1.0) then
               inst_id(i) = 504
               israma(i) = rr
            endif
         endif
       ! MetOffice doesn't have correct WMO for TAO, could be CTD servicing cruise
       ! need to check for tao locations
          if ( inst_id(i) == 0 ) then
            if ( abs(lon2(i) - lon_rama(rr)) < 1.0 .and. &
                 abs(lat(i) - lat_rama(rr)) < 1.0 ) then
               !print *, 'RAMA: ',idate(i),lon2(i), lon_rama(rr), lat(i), lat_rama(rr),stn_name(i),wmo_rama(rr)
               inst_id(i) = 504
               israma(i) = rr
           endif          
         endif
       enddo


     ENDDO  
  
  END SUBROUTINE get_inst_id

 
  ! ..........................................................................
    SUBROUTINE get_inst_id_pos (nobs,idate,itao,ipir,irama,wmo_tao,wmo_pir, &
                           wmo_rama,lon,lat,lon_tao,lat_tao,lon_pir, &
			   lat_pir,lon_rama,lat_rama,stn_name,stn_idS,stn_idT)    
  ! ..........................................................................
  ! Get stn_id
  !
    IMPLICIT NONE
    integer,                      intent(in)    :: nobs 
    integer,     dimension(nobs), intent(in)    :: idate
    integer,                      intent(in)    :: itao, ipir, irama
    character*8, dimension(nobs), intent(in)    :: wmo_tao, wmo_pir, wmo_rama 
    real,        dimension(nobs), intent(in)    :: lon, lat
    real,        dimension(itao), intent(in)    :: lon_tao, lat_tao
    real,        dimension(ipir), intent(in)    :: lon_pir, lat_pir    
    real,        dimension(ipir), intent(in)    :: lon_rama, lat_rama    
    character*8, dimension(nobs), intent(inout) :: stn_name
    integer,     dimension(nobs), intent(inout) :: stn_idS, stn_idT
    integer,     dimension(nobs)                :: badpos   
    integer,     dimension(nobs)                :: istao, ispir, isarg, israma
    integer                                     :: i, tt, pp, aa, rr
    integer                                     :: scan_c, scan_q, scan_9
    real,        dimension(nobs)                :: lon2   
    
    DO i = 1, nobs 

    ! SWITCH TO 0/360 LONGITUDE RANGE
       lon2(i) = lon(i)
       if (lon(i) < 0) then
          lon2(i) = lon(i) + 360.
       endif    
                         

    !  TAO ARRAY
    !  ................................................................
       do tt=1,itao

       ! Is it a  CTD servicing cruise, check locations
            if ( abs(lon2(i) - lon_tao(tt)) == 0 .and. &
                 abs(lat(i) - lat_tao(tt)) == 0 ) then
               istao(i)   = tt
               stn_idS(i) = 501
               !print *, '1:TAO CTD', idate(i),lon(i),lon_tao(istao(i)), &
	         !lat(i),lat_tao(istao(i)),stn_name(i)	
           endif          

       enddo ! tt tao
       
       ! If not TAO but at TAO location-must be CTD from maintenance cruise
       do tt=1,itao
          if (stn_name(i) == 'WTEU') then
             if ( abs(lon2(i) - lon_tao(tt)) == 0 .and. &
                  abs(lat(i) - lat_tao(tt)) == 0 ) then
                  stn_idS(i) = 501		  
                  !print *, '2: TAO CTD',idate(i),lon(i),lat(i),stn_name(i),wmo_tao(tt)
                  stn_name(i) = wmo_tao(tt)           
	     endif
          endif
       enddo   
 

    !  PIRATA ARRAY
    !  ................................................................
       do pp=1,ipir
          !if ( stn_idT(i) == 0 ) then
            if ( abs(lon2(i) - lon_pir(pp)) == 0 .and. &
                 abs(lat(i) - lat_pir(pp))  == 0 ) then
               !print *, 'PIR: ',lon2(i), lon_pir(pp), lat(i), lat_pir(pp)
               ispir(i)   = pp
               stn_idS(i) = 502
           endif          
         !endif
       enddo
       	   

    ! Indian Ocean RAMA Array
    !  ................................................................
       do rr=1,irama
          !if ( stn_idT(i) == 0 ) then
            if ( abs(lon2(i) - lon_rama(rr)) == 0 .and. &
                 abs(lat(i) - lat_rama(rr)) == 0) then
               !print *, 'RAMA: ',lon2(i), lon_rama(pp), lat(i), lat_rama(pp)
               stn_idS(i) = 504
               israma(i) = rr
           endif          
         !endif
       enddo


     ENDDO  
  
  END SUBROUTINE get_inst_id_pos


  ! ..........................................................................
    SUBROUTINE is_argo_master(cntM,wmoM,cntC,wmoC)    
  ! ..........................................................................
  ! Find out if WMO is really an Argo
    IMPLICIT NONE
    integer,                   intent(in)  :: cntM, cntC
    character*8, dimension(:), intent(in)  :: wmoM(cntM), wmoC(cntC)
    character*8                            :: tmp 
    integer                                :: data_id
    integer                                :: i, j, cnt, ieof
    character*256                          :: fname_master, fname_data_id

   fname_master  = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/ARGO/PRF/ARGO_WMO_MASTER.txt'    
     open (10, file=trim(fname_master),status='old',position = 'append',form='formatted')     

  ! Get last record of data_id
    fname_data_id = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/ARGO/PRF/ARGO_WMO_DATA_ID.txt'    
    open (20, file=trim(fname_data_id),status='old',form='formatted')     
    ieof = 0
    DO WHILE (ieof == 0)
     read (20, '(A,I8)',iostat = ieof) tmp, data_id
     if (ieof /= 0) exit
    ENDDO
    close (20)
    !print *, 'Last Record: ',tmp,data_id
    fname_data_id = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/ARGO/PRF/ARGO_WMO_DATA_ID.txt'    
    open (20, file=trim(fname_data_id),status='old',position='append',form='formatted')     
 
  ! Get any new wmo's
    DO i = 1, cntC 
      cnt = 0
      DO j=1,cntM
        if ( trim(adjustl(wmoC(i))) .eq. trim(adjustl(wmoM(j))) ) then
           cnt = cnt+1
           !print *, 'MATCH: ',cnt,i,j,wmoC(i), ' ',wmoM(j)
        endif
       ENDDO ! j master

       ! if cnt==0, not in master list
       ! need to append it and create a data_id
       if (cnt==0) then
         !print *, 'New WMO: ',wmoC(i)
         data_id = data_id + 1
         !write (20, '(A8,1X,I8)') trim(adjustl(wmoC(i))),data_id
         !write (10, '(A8)')    wmoC(i)
         write (20, '(A,I8)') adjustl(wmoC(i)),data_id
         write (10, '(A)')    wmoC(i)
       else
         !print *, 'No New WMO'
       endif
     ENDDO ! i current
     close(10)
     close(20)
  
  END SUBROUTINE is_argo_master


  ! ..........................................................................
    SUBROUTINE is_ctd_master(cntM,wmoM,cntC,wmoC)    
  ! ..........................................................................
  ! Find out if WMO is really an Argo
    IMPLICIT NONE
    integer,                   intent(in)  :: cntM, cntC
    character*8, dimension(:), intent(in)  :: wmoM(cntM), wmoC(cntC)
    character*8                            :: tmp 
    integer                                :: data_id
    integer                                :: i, j, cnt, ieof
    character*256                          :: fname_master, fname_data_id

   fname_master  = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/METOFFICE/CTD/MET_CTD_WMO_MASTER.txt'    
     open (10, file=trim(fname_master),status='old',position = 'append',form='formatted')     

  ! Get last record of data_id
    fname_data_id = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS//METOFFICE/CTD/MET_CTD_WMO_DATA_ID.txt'    
    open (20, file=trim(fname_data_id),status='old',form='formatted')     
    ieof = 0
    DO WHILE (ieof == 0)
     read (20, '(A,I8)',iostat = ieof) tmp, data_id
     if (ieof /= 0) exit
    ENDDO
    close (20)
    !print *, 'Last Record: ',tmp,data_id
    fname_data_id = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/METOFFICE/CTD/MET_CTD_WMO_DATA_ID.txt'    
    open (20, file=trim(fname_data_id),status='old',position='append',form='formatted')     
 
  ! Get any new wmo's
    DO i = 1, cntC 
      cnt = 0
      DO j=1,cntM
        if ( trim(adjustl(wmoC(i))) .eq. trim(adjustl(wmoM(j))) ) then
           cnt = cnt+1
           !print *, 'MATCH: ',cnt,i,j,wmoC(i), ' ',wmoM(j)
        endif
       ENDDO ! j master
       ! if cnt==0, not in master list
       ! need to append it and create a data_id
       if (cnt==0) then
         !print *, 'New WMO: ',wmoC(i)
         data_id = data_id + 1
         !write (20, '(A8,1X,I8)') trim(adjustl(wmoC(i))),data_id
         !write (10, '(A8)')    wmoC(i)
         write (20, '(A,I8)') adjustl(wmoC(i)),data_id
         write (10, '(A)')    wmoC(i)
       else
         !print *, 'No New WMO'
       endif
     ENDDO ! i current
     close(10)
     close(20)
  
  END SUBROUTINE is_ctd_master



  ! ..........................................................................
    SUBROUTINE is_xbt_master(cntM,wmoM,cntC,wmoC,ilist)    
  ! ..........................................................................
    IMPLICIT NONE
    integer,                   intent(in)  :: cntM, cntC, ilist
    character*8, dimension(:), intent(in)  :: wmoM(cntM), wmoC(cntC)
    character*8                            :: tmp 
    integer                                :: data_id
    integer                                :: i, j, cnt, ieof
    character*256                          :: fname_master, fname_data_id

    ! list=1, check NCEP against MASTER
    if (ilist==1) then
      !fname_master  = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/PRE/XBT_WMO_MASTER.txt'    
      !fname_master = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/PRE/METOFFICE/EN3_v2a/XBT/MET_XBT_WMO_MASTER.txt' 
      fname_master = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/METOFFICE/XBT/MET_XBT_WMO_MASTER.txt' 
      open (10, file=trim(fname_master),status='old',position = 'append',form='formatted')     

     ! Get last record of data_id
       !fname_data_id = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/PRE/XBT_WMO_DATA_ID.txt'    
       !fname_data_id = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/PRE/METOFFICE/EN3_v2a/XBT/MET_XBT_WMO_DATA_ID.txt' 
       fname_data_id = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/METOFFICE/XBT/MET_XBT_WMO_DATA_ID.txt' 
       open (20, file=trim(fname_data_id),status='old',form='formatted')     
       ieof = 0
       DO WHILE (ieof == 0)
        read (20, '(A,1X,I8)',iostat = ieof) tmp, data_id
        if (ieof /= 0) exit
       ENDDO
       close (20)
       !print *, 'Last Record: ',tmp,data_id

       !fname_data_id = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/PRE/XBT_WMO_DATA_ID.txt'    
       !fname_data_id = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/PRE/METOFFICE/EN3_v2a/XBT/MET_XBT_WMO_DATA_ID.txt'   
        fname_data_id = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/METOFFICE/XBT/MET_XBT_WMO_DATA_ID.txt'   
       open (20, file=trim(fname_data_id),status='old',position='append',form='formatted')     
 
     ! Get any new wmo's
       DO i = 1, cntC 
         cnt = 0
         DO j=1,cntM
           !print *, '*',trim(adjustl(wmoC(i))),'*',trim(adjustl(wmoM(j))),'*'
           if ( trim(adjustl(wmoC(i))) .eq. trim(adjustl(wmoM(j))) ) then
             cnt = cnt+1
             !print *, 'MATCH: ', wmoC(i), ' ',wmoM(j)
           endif
         ENDDO ! j master

         ! if cnt==0, not in master list
         ! need to append it and create a data_id
         if (cnt==0) then
           print *, 'New WMO: ',wmoC(i)
           data_id = data_id + 1
           write (20, '(A,I8)') adjustl(wmoC(i)),data_id
           write (10, '(A)')    wmoC(i)
         else
         !print *, 'No New WMO'
         endif
       ENDDO ! i current
       close(10)
       close(20)
    endif ! list==1



  
  END SUBROUTINE is_xbt_master


  ! ..............................................
    SUBROUTINE wmo_to_data_id (kx,wmo,date_time,data_id,src)    
  ! ..............................................
  ! Read WMO numbers from ascii file
  !
    IMPLICIT NONE
    character*8, intent(in)  :: wmo   
    character*3, intent(in)  :: src   
    integer,     intent(in)  :: kx, date_time
    integer,     intent(out) :: data_id

    integer                  :: cnt, dataidM
    character*8              :: wmoM
    integer                  :: ieof, ind1, ind2, scan1, scan2
    integer                  :: len1, len2, v1, v2
    integer                  :: UNIT_IN1=10, UNIT_IN2=20
    character*256            :: fname_in1,fname_in2
    logical                  :: iseq1, iseq2
    
    if (kx==508) then
      fname_in1  = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/ARGO/PRF/ARGO_WMO_DATA_ID.txt' 
      fname_in2  = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/ARGO/PRF/ARGO_WMO_MASTER.txt' 
    elseif (kx==503 .and. src=='met') then
      fname_in1  = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/METOFFICE/XBT/MET_XBT_WMO_DATA_ID.txt' 
      fname_in2 = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/METOFFICE/XBT/MET_XBT_WMO_MASTER.txt' 
    elseif (kx==503 .and. src=='nce') then
      fname_in1  = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/PRE/XBT_WMO_DATA_ID.txt' 
      fname_in2 = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/PRE/XBT_WMO_MASTER.txt' 
    elseif (kx==513 .and. src=='ctd') then
      fname_in1  = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/METOFFICE/CTD/MET_CTD_WMO_DATA_ID.txt' 
      fname_in2 = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/METOFFICE/CTD/MET_CTD_WMO_MASTER.txt' 
    endif
    open (UNIT_IN1,  file=trim(fname_in1),  status='old', form='formatted')
    open (UNIT_IN2,  file=trim(fname_in2),  status='old', form='formatted')
  
    cnt  = 0
    ieof = 0
    DO WHILE (ieof == 0)
      read  (UNIT_IN1,  '(A8,1X,I8)',iostat = ieof) wmoM, dataidM 
      read  (UNIT_IN2,  '(A)',iostat = ieof) wmoM

      if (trim(adjustl(wmo)) .eq. trim(adjustl(wmoM))) then
        data_id = dataidM
        cnt = cnt+1
        !print *, 'Match:','*',wmo,'*',wmoM,'*',data_id
        cnt = 0
        close (UNIT_IN1)
        close (UNIT_IN2)
        return
      endif
      if (ieof /= 0) exit
    ENDDO

    if (cnt==0) then
        !print *, 'NO Match:','*',wmo,'*','*',wmoM,'*',dataidM, date_time
        close (UNIT_IN1)
        close (UNIT_IN2)
        return
    endif
    
   return
  END SUBROUTINE wmo_to_data_id

  ! ..............................................
    SUBROUTINE make_data_id (kx,src)    
  ! ..............................................
  ! Read WMO numbers from ascii file
  !
    IMPLICIT NONE
    integer,     intent(in)  :: kx
    character*3, intent(in)  :: src
    integer                  :: data_id, ieof

    integer                  :: cnt, maxcnt, data_id0, max_id
    character*8              :: wmoM
    integer                  :: UNIT_IN=10, UNIT_OUT=20
    character*256            :: fname_in,fname_out
    
    if (kx==508) then
      fname_in  = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/ARGO/PRF/ARGO_WMO_MASTER.txt' 
      fname_out  = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/ARGO/PRF/ARGO_WMO_DATA_ID.txt' 
    elseif (kx==503 .and. src=='met') then
      fname_in  = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/METOFFICE/XBT/MET_XBT_WMO_MASTER.txt' 
      fname_out = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/METOFFICE/XBT/MET_XBT_WMO_DATA_ID.txt' 
      maxcnt    = 990000
      data_id0  = (kx*100000)-1;
      max_id    = (kx+1)*100000;  
    elseif (kx==513) then
      fname_in  = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/METOFFICE/CTD/MET_CTD_WMO_MASTER.txt' 
      fname_out = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/OPS/METOFFICE/CTD/MET_CTD_WMO_DATA_ID.txt' 
      maxcnt    = 990000
      data_id0  = (kx*100000)-1;
      max_id    = (kx+1)*100000;  
    endif
    open (UNIT_IN,  file=trim(fname_in),  status='old', form='formatted')
    open (UNIT_OUT, file=trim(fname_out),  status='old', form='formatted')
  
    cnt  = 0
    ieof = 0
    DO WHILE (ieof == 0)
      read  (UNIT_IN,'(A8)',iostat = ieof) wmoM
      if (ieof /= 0) exit
        data_id = data_id0+1
        data_id0 = data_id
        cnt = cnt+1
        write(UNIT_OUT, '(a,1x,I8)')  adjustl(wmoM), data_id       
    ENDDO
    close (UNIT_IN)
    close (UNIT_OUT)

    
   return
  END SUBROUTINE make_data_id

  ! ..............................................
    SUBROUTINE s_eq (s1,s2,s_eqi)    
  ! ..............................................
  ! Compares two strings, ignoring case and blanks
  !
  ! http://people.sc.fsu.edu/~%20burkardt/f_src/chrpak/chrpak.f90
  !
  implicit none

  character              c1
  character              c2
  integer   ( kind = 4 ) i
  integer   ( kind = 4 ) lenc
  logical, intent(out) ::  s_eqi
  character ( len = *  ) s1
  integer   ( kind = 4 ) s1_length
  character ( len = *  ) s2
  integer   ( kind = 4 ) s2_length

  s1_length = len ( s1 )
  s2_length = len ( s2 )
  lenc = min ( s1_length, s2_length )
 
  s_eqi = .false.

  do i = 1, lenc

    c1 = s1(i:i)
    c2 = s2(i:i)
    call ch_cap ( c1 )
    call ch_cap ( c2 )

    if ( c1 /= c2 ) then
      return
    end if

  end do
 
  do i = lenc + 1, s1_length
    if ( s1(i:i) /= ' ' ) then
      return
    end if
  end do
 
  do i = lenc + 1, s2_length
    if ( s2(i:i) /= ' ' ) then
      return
    end if
  end do
 
  s_eqi = .true.
 
   return
  END SUBROUTINE s_eq
 
  ! ..............................................
    SUBROUTINE ch_cap (ch)    
  ! ..............................................
  !
  ! CH_CAP capitalizes a single character.
  !
  !  Discussion:
  !
  !    Instead of CHAR and ICHAR, we now use the ACHAR and IACHAR functions, 
  !    which guarantee the ASCII collating sequence.
  !
  !  http://people.sc.fsu.edu/~%20burkardt/f_src/chrpak/chrpak.f90
  !
  !  Parameters:
  !    Input/output, character CH, the character to capitalize.
  !
    implicit none

    character              ch
    integer   ( kind = 4 ) itemp

    itemp = iachar ( ch )
 
    if ( 97 <= itemp .and. itemp <= 122 ) then
      ch = achar ( itemp - 32 )
    end if
 
   return
  END SUBROUTINE ch_cap

      !ind1  = index(trim(adjustl(wmo)),trim(adjustl(wmoM)))
      !ind2  = index(trim(adjustl(wmo)),trim(adjustl(wmoM)),.true.)
      !scan1 = scan(wmo,wmoM)
      !scan2 = scan(wmo,wmoM,.true.)
      !len1  = len(wmo)
      !len2  = len(wmoM)
      !v1    = verify(wmo,wmoM)
      !v2    = verify(wmo,wmoM,.true.)
      !iseq1 = lle(wmoM,wmo)
      !iseq2 = lge(wmo,wmoM)
      !print *, '*',wmo,'*',wmoM,'*',ind1,ind2,scan1,scan2,v1,v2,len1,len2
      !call s_eq(wmoM,trim(wmo),iseq) 
      !if ( trim(adjustl(wmo)) .eq. trim(adjustl(wmoM)) ) then   

  
END MODULE DATA_ID_MODULE_7
		
