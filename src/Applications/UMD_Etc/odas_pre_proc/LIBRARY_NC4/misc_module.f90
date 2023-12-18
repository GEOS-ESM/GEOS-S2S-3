MODULE MISC_MODULE

  IMPLICIT NONE
     
  PUBLIC :: make_dates, make_year, sort_ji, sort_ji_ts, get_rms, pres2deph, zdays
  PUBLIC :: deph2pres, deph2pres2, jd2date, sort_ji_argo, sort_ji_argo_1var
  
  CONTAINS


  ! ..........................................................
    SUBROUTINE zdays(month,year,ndays)
  ! ..........................................................
    USE ZEUS_CLOCKS 
    IMPLICIT NONE 
   
    integer, intent(in)  :: year, month
    integer, intent(out) :: ndays
    
    !***********************************************************!

    ndays = Z_DAYSINM(month, year)

 END SUBROUTINE zdays


  ! ..........................................................
    SUBROUTINE make_dates(yrbeg, monbeg, yrend, monend, nidate)
  ! ..........................................................
  ! December previous year, Jan-Dec current year, Jan next year
    USE ZEUS_CLOCKS 
    IMPLICIT NONE 
   
    integer, intent(in)                :: yrbeg, yrend
    integer, intent(in)                :: monbeg, monend
    integer, intent(out), dimension(:) :: nidate
    integer                            :: mon, iyr, i, k, ndays
    
    !***********************************************************!

    i = 0
    do mon=monbeg,12
       ndays = Z_DAYSINM(mon,yrbeg)
       do k=1,ndays
          i = i+1
          nidate(i) = k+mon*100+yrbeg*10000
       enddo
    enddo

    do iyr=yrbeg+1,yrend-1
       do mon=1,12
          ndays = Z_DAYSINM(mon,iyr)
          do k=1,ndays
             i = i+1
             nidate(i) = k+mon*100+iyr*10000
          enddo
       enddo
    enddo

    do mon=1, monend
       ndays = Z_DAYSINM(mon,yrend)
        do k=1,ndays
           i = i+1
           nidate(i) = k+mon*100+yrend*10000
        enddo
    enddo
  return
  END SUBROUTINE make_dates

  ! ..........................................................
    SUBROUTINE make_year(yrbeg, nidate)
  ! ..........................................................
  ! December previous year, Jan-Dec current year, Jan next year
    USE ZEUS_CLOCKS 
    IMPLICIT NONE 
   
    integer, intent(in)                :: yrbeg
    integer                            :: monbeg, monend, yrend
    integer, intent(out), dimension(:) :: nidate
    integer                            :: mon, iyr, i, k, ndays
    
    !***********************************************************!
    monbeg = 1
    monend = 12
    yrend = yrbeg

    i = 0
    do iyr=yrbeg+1,yrend-1
       do mon=1,12
          ndays = Z_DAYSINM(mon,iyr)
          do k=1,ndays
             i = i+1
             nidate(i) = k+mon*100+iyr*10000
          enddo
       enddo
    enddo

  return
  END SUBROUTINE make_year

  ! ............................................................
    SUBROUTINE sort_ji(Item,date,lon,lat,npts,stn_name,stn_id,qc_prf,&
            interp_prf, qc_flag, depth,field,qc_lev,max_npts,nobs)
  ! ............................................................
    
    IMPLICIT NONE
    integer,     dimension(:),  intent(inout) :: Item, date, stn_id, npts
    real,        dimension(:),  intent(inout) :: lon, lat, qc_prf
    integer,     dimension(:),  intent(inout) :: interp_prf, qc_flag
    character*8, dimension(:),  intent(inout) :: stn_name
    integer,                    intent(in)    :: max_npts, nobs
    real,        dimension(max_npts,nobs)     :: depth, field, qc_lev
   
    integer  :: NumItems, SmallestItem, tmp, i, LocationSmallest
    integer, dimension(1) :: MINLOC_array

    integer                        :: Itmp
    real                           :: Rtmp
    real, dimension(max_npts)      :: RMtmp
    character*8                    :: Stmp

    NumItems = size(Item)

    DO i=1, NumItems

       SmallestItem = MINVAL(Item(i:NumItems))
       MINLOC_array = MINLOC(Item(i:NumItems))
       LocationSmallest = (i-1) + MINLOC_array(1)

       Item(LocationSmallest) = Item(i)
       Item(i)                = SmallestItem
  
       Itmp = date(LocationSmallest)
         date(LocationSmallest) = date(i)
         date(i)                = Itmp
       Itmp = stn_id(LocationSmallest)
         stn_id(LocationSmallest) = stn_id(i)
         stn_id(i)                = Itmp
       Itmp = npts(LocationSmallest)
         npts(LocationSmallest) = npts(i)
         npts(i)                = Itmp
       Rtmp = lon(LocationSmallest)
         lon(LocationSmallest) = lon(i)
         lon(i)                = Rtmp
       Rtmp = lat(LocationSmallest)
         lat(LocationSmallest) = lat(i)
         lat(i)                = Rtmp
       Rtmp = qc_prf(LocationSmallest)
         qc_prf(LocationSmallest) = qc_prf(i)
         qc_prf(i)                = Rtmp
       Rtmp = qc_flag(LocationSmallest)
         qc_flag(LocationSmallest) = qc_flag(i)
         qc_flag(i)                = Rtmp
       Rtmp = interp_prf(LocationSmallest)
         interp_prf(LocationSmallest) = interp_prf(i)
         interp_prf(i)                = Rtmp
       Stmp = stn_name(LocationSmallest)
         stn_name(LocationSmallest) = stn_name(i)
         stn_name(i)  = Stmp

       RMtmp = depth(:,LocationSmallest)
         depth(:,LocationSmallest) = depth(:,i)
         depth(:,i)  = RMtmp

       RMtmp = field(:,LocationSmallest)
         field(:,LocationSmallest) = field(:,i)
         field(:,i)  = RMtmp

       RMtmp = qc_lev(:,LocationSmallest)
         qc_lev(:,LocationSmallest) = qc_lev(:,i)
         qc_lev(:,i)  = RMtmp

    ENDDO
    
  return
  END SUBROUTINE sort_ji

  ! ............................................................
    SUBROUTINE sort_ji_ts(Item,date,lon,lat,npts,snpts,stn_name,stn_id,tqc_prf,sqc_prf,&
            tqc_flag,sqc_flag,depth,field,tqc_lev,field2,sqc_lev,depth2,max_npts,nobs)
  ! ............................................................
    
    IMPLICIT NONE
    integer,     dimension(:),  intent(inout) :: Item, date, stn_id, npts, snpts,tqc_flag, sqc_flag
    real,        dimension(:),  intent(inout) :: lon, lat, tqc_prf, sqc_prf
    character*8, dimension(:),  intent(inout) :: stn_name
    integer,                    intent(in)    :: max_npts, nobs
    real,        dimension(max_npts,nobs),  intent(inout)     :: field,  tqc_lev, depth
    real,        dimension(max_npts,nobs),  intent(inout)     :: field2, sqc_lev, depth2
   
    integer  :: NumItems, SmallestItem, tmp, i, LocationSmallest, MINLOCI
    integer, dimension(1) :: MINLOC_array

    integer                        :: Itmp
    real                           :: Rtmp
    real, dimension(max_npts)      :: RMtmp
    character*8                    :: Stmp

    NumItems = size(Item)

    DO i=1, NumItems

       SmallestItem = MINVAL(Item(i:NumItems))
       MINLOC_array = MINLOC(Item(i:NumItems))
       MINLOCI = MINLOC_array(1)
       LocationSmallest = (i-1) + MINLOCI
 
       Item(LocationSmallest) = Item(i)
       Item(i)                = SmallestItem
       !print *, i, Item(i), date(i), npts(i), lon(i), lat(i), depth(1,i), field(1,i)
 
       Itmp = date(LocationSmallest)
         date(LocationSmallest) = date(i)
         date(i)                = Itmp
       Itmp = stn_id(LocationSmallest)
         stn_id(LocationSmallest) = stn_id(i)
         stn_id(i)                = Itmp
       Itmp = npts(LocationSmallest)
         npts(LocationSmallest) = npts(i)
         npts(i)                = Itmp
       Itmp = snpts(LocationSmallest)
         snpts(LocationSmallest) = snpts(i)
         snpts(i)                = Itmp

       Itmp = tqc_flag(LocationSmallest)
         tqc_flag(LocationSmallest) = tqc_flag(i)
         tqc_flag(i)                = Itmp
       Itmp = sqc_flag(LocationSmallest)
         sqc_flag(LocationSmallest) = sqc_flag(i)
         sqc_flag(i)                = Itmp

       Rtmp = lon(LocationSmallest)
         lon(LocationSmallest) = lon(i)
         lon(i)                = Rtmp
       Rtmp = lat(LocationSmallest)
         lat(LocationSmallest) = lat(i)
         lat(i)                = Rtmp
       Rtmp = tqc_prf(LocationSmallest)
         tqc_prf(LocationSmallest) = tqc_prf(i)
         tqc_prf(i)                = Rtmp
       Rtmp = sqc_prf(LocationSmallest)
         sqc_prf(LocationSmallest) = sqc_prf(i)
         sqc_prf(i)                = Rtmp

       Stmp = stn_name(LocationSmallest)
         stn_name(LocationSmallest) = stn_name(i)
         stn_name(i)  = Stmp

       RMtmp(:)=0
       RMtmp = depth(:,LocationSmallest)
         depth(:,LocationSmallest) = depth(:,i)
         depth(:,i)  = RMtmp

       RMtmp(:)=0
       RMtmp = depth2(:,LocationSmallest)
         depth2(:,LocationSmallest) = depth2(:,i)
         depth2(:,i)  = RMtmp

       RMtmp(:)=0
       RMtmp = field(:,LocationSmallest) ! 22
         !print *, i
         !print *, field(1:10,22)
         !print *, field(1:10,1)
         field(:,LocationSmallest) = field(:,i) ! put i to location 22
         field(:,i)  = RMtmp ! put location 22 to i

       RMtmp(:)=0
       RMtmp = tqc_lev(:,LocationSmallest)
         tqc_lev(:,LocationSmallest) = tqc_lev(:,i)
         tqc_lev(:,i)  = RMtmp

       RMtmp(:)=0
       RMtmp = field2(:,LocationSmallest)
         field2(:,LocationSmallest) = field2(:,i)
         field2(:,i)  = RMtmp

       RMtmp(:)=0
       RMtmp = sqc_lev(:,LocationSmallest)
         sqc_lev(:,LocationSmallest) = sqc_lev(:,i)
         sqc_lev(:,i)  = RMtmp

       !print *, i, Item(i), npts(i), lon(i), lat(i), depth(1,i), field(1,i)
    ENDDO
    
  return
  END SUBROUTINE sort_ji_ts


  ! ...................................................
    SUBROUTINE get_rms ( miss, npts, temp1, temp2, rms)                     
  ! ...................................................
  ! Calculate point by point rms between two profiles
  !
    IMPLICIT NONE
    real, intent(in)   :: miss, temp1(:), temp2(:)
    real, intent(out)  :: rms
    integer,intent(in) :: npts
    integer            :: i
    real               :: mean_diff, diff(npts)
    real               :: rms_diff,  std(npts)
      
    mean_diff = 0
    DO i=1,npts
       diff(i) = temp1(i) - temp2(i)   
       mean_diff = mean_diff + diff(i)
    ENDDO
    mean_diff = mean_diff/npts

    rms_diff = 0
    DO i=1,npts
       std(i) = (diff(i) - mean_diff)**2   
       rms_diff = rms_diff + std(i)
    ENDDO
    rms_diff = rms_diff/npts
    rms = sqrt(rms_diff)
 
  return
  END SUBROUTINE get_rms

   ! .................................................................
    SUBROUTINE pres2deph(PRES,LAT,DEPH)
  ! .................................................................
    IMPLICIT NONE

    ! Comparable to Matlab Seawater Library
    real, intent(in)    :: PRES, LAT
    real, intent(out)   :: DEPH
    real                :: X, GRAV, TMP

    X      = sin(LAT/57.29578) * sin(LAT/57.29578)

    ! Gravity at lattitude
    GRAV   = 9.780318*(1.0+(5.2788e-3+2.36e-5*X)*X) + 1.092e-6*PRES
    TMP    = (((-1.82e-15*PRES+2.279e-10)*PRES-2.2512e-5)*PRES+9.72659*PRES)
    DEPH   = TMP/GRAV

  return
  END SUBROUTINE pres2deph


   ! .................................................................
    SUBROUTINE deph2pres(DEPH,LAT,NPTS,PRES)
  ! .................................................................
    IMPLICIT NONE

    ! Comparable to Matlab Seawater Library
    integer, intent(in) :: NPTS
    real, intent(in)    :: DEPH(NPTS), LAT
    real, intent(out)   :: PRES(NPTS)
    real                :: PLAT, D, C1, pi

      pi = 3.1415926535897
      PLAT=abs(LAT*pi/180.);
      D=sin(PLAT);
      C1=5.92E-3+(D*D)*5.25E-3;
      PRES=((1-C1)-sqrt(((1-C1)**2)-(8.84E-6*DEPH)))/4.42E-6;

  return
  END SUBROUTINE deph2pres

  ! ..............................................
    SUBROUTINE deph2pres2(depth,lat,npts,pressure)
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
  end subroutine deph2pres2


 
  ! .................................................................
    SUBROUTINE jd2date(jd1950,day,hh)
  ! .................................................................
    USE calendar
    IMPLICIT NONE

    double precision, intent(in)   :: JD1950
    integer, intent(out)  :: day, hh
    real                  :: fjd
    integer               :: ijd, n ,t
    integer               :: julian, values(1), ierr, year, month

   julian = int(2433283 + jd1950)
   call julian_to_date(julian,day,month,year,values,ierr)
   !subroutine julian_to_date(julian,day,month,year,values,ierr)
   !print *, year, month, day
   
      ijd = floor(JD1950) ! integer part
      fjd = JD1950 - ijd  !fraction part
      hh  = nint(fjd*24)
 
      !print *, jd1950, hh

  return
  END SUBROUTINE jd2date



  ! ............................................................
    SUBROUTINE sort_ji_argo(Item,lon,lat,npts,snpts,stn_name,tqc_prf,sqc_prf,&
            tqc_flag,sqc_flag,depth,field,tqc_lev,field2,sqc_lev,depth2,max_npts,nobs)
  ! ............................................................
    
    IMPLICIT NONE
    integer,     dimension(:),  intent(inout) :: Item, npts, snpts,tqc_flag, sqc_flag
    real,        dimension(:),  intent(inout) :: lon, lat, tqc_prf, sqc_prf
    character*8, dimension(:),  intent(inout) :: stn_name
    integer,                    intent(in)    :: max_npts, nobs
    real,        dimension(max_npts,nobs),  intent(inout)     :: field,  tqc_lev, depth
    real,        dimension(max_npts,nobs),  intent(inout)     :: field2, sqc_lev, depth2
   
    integer  :: NumItems, SmallestItem, tmp, i, LocationSmallest, MINLOCI
    integer, dimension(1) :: MINLOC_array

    integer                        :: Itmp
    real                           :: Rtmp
    real, dimension(max_npts)      :: RMtmp
    character*8                    :: Stmp

    NumItems = size(Item)

    DO i=1, NumItems

       SmallestItem = MINVAL(Item(i:NumItems))
       !print *, i, MINLOC(Item(i:NumItems))
       MINLOC_array = MINLOC(Item(i:NumItems))
       MINLOCI = MINLOC_array(1)
       LocationSmallest = (i-1) + MINLOCI
 
       Item(LocationSmallest) = Item(i)
       Item(i)                = SmallestItem
 
       Itmp = npts(LocationSmallest)
         npts(LocationSmallest) = npts(i)
         npts(i)                = Itmp
       Itmp = snpts(LocationSmallest)
         snpts(LocationSmallest) = snpts(i)
         snpts(i)                = Itmp

       Itmp = tqc_flag(LocationSmallest)
         tqc_flag(LocationSmallest) = tqc_flag(i)
         tqc_flag(i)                = Itmp
       Itmp = sqc_flag(LocationSmallest)
         sqc_flag(LocationSmallest) = sqc_flag(i)
         sqc_flag(i)                = Itmp

       Rtmp = lon(LocationSmallest)
         lon(LocationSmallest) = lon(i)
         lon(i)                = Rtmp
       Rtmp = lat(LocationSmallest)
         lat(LocationSmallest) = lat(i)
         lat(i)                = Rtmp
       Rtmp = tqc_prf(LocationSmallest)
         tqc_prf(LocationSmallest) = tqc_prf(i)
         tqc_prf(i)                = Rtmp
       Rtmp = sqc_prf(LocationSmallest)
         sqc_prf(LocationSmallest) = sqc_prf(i)
         sqc_prf(i)                = Rtmp

       Stmp = stn_name(LocationSmallest)
         stn_name(LocationSmallest) = stn_name(i)
         stn_name(i)  = Stmp

       RMtmp(:)=0
       RMtmp = depth(:,LocationSmallest)
         depth(:,LocationSmallest) = depth(:,i)
         depth(:,i)  = RMtmp

       RMtmp(:)=0
       RMtmp = depth2(:,LocationSmallest)
         depth2(:,LocationSmallest) = depth2(:,i)
         depth2(:,i)  = RMtmp

       RMtmp(:)=0
       RMtmp = field(:,LocationSmallest) ! 22
         field(:,LocationSmallest) = field(:,i) ! put i to location 22
         field(:,i)  = RMtmp ! put location 22 to i

       RMtmp(:)=0
       RMtmp = tqc_lev(:,LocationSmallest)
         tqc_lev(:,LocationSmallest) = tqc_lev(:,i)
         tqc_lev(:,i)  = RMtmp

       RMtmp(:)=0
       RMtmp = field2(:,LocationSmallest)
         field2(:,LocationSmallest) = field2(:,i)
         field2(:,i)  = RMtmp

       RMtmp(:)=0
       RMtmp = sqc_lev(:,LocationSmallest)
         sqc_lev(:,LocationSmallest) = sqc_lev(:,i)
         sqc_lev(:,i)  = RMtmp

    ENDDO
    
  return
  END SUBROUTINE sort_ji_argo
  

  ! ............................................................
    SUBROUTINE sort_ji_argo_1var(Item,lon,lat,npts,stn_name,&
                            tqc_flag,depth,field,tqc_lev,max_npts,nobs)
  ! ............................................................
    
    IMPLICIT NONE
    integer,     dimension(:),  intent(inout) :: Item, npts, tqc_flag
    real,        dimension(:),  intent(inout) :: lon, lat
    character*8, dimension(:),  intent(inout) :: stn_name
    integer,                    intent(in)    :: max_npts, nobs
    real,        dimension(max_npts,nobs),  intent(inout)     :: field,  tqc_lev, depth
   
    integer  :: NumItems, SmallestItem, tmp, i, LocationSmallest, MINLOCI
    integer, dimension(1) :: MINLOC_array

    integer                        :: Itmp
    real                           :: Rtmp
    real, dimension(max_npts)      :: RMtmp
    character*8                    :: Stmp

    NumItems = size(Item)

    DO i=1, NumItems

       SmallestItem = MINVAL(Item(i:NumItems))
       !print *, i, MINLOC(Item(i:NumItems))
       MINLOC_array = MINLOC(Item(i:NumItems))
       MINLOCI = MINLOC_array(1)
       LocationSmallest = (i-1) + MINLOCI
 
       Item(LocationSmallest) = Item(i)
       Item(i)                = SmallestItem
 
       Itmp = npts(LocationSmallest)
         npts(LocationSmallest) = npts(i)
         npts(i)                = Itmp

       Itmp = tqc_flag(LocationSmallest)
         tqc_flag(LocationSmallest) = tqc_flag(i)
         tqc_flag(i)                = Itmp

       Rtmp = lon(LocationSmallest)
         lon(LocationSmallest) = lon(i)
         lon(i)                = Rtmp
       Rtmp = lat(LocationSmallest)
         lat(LocationSmallest) = lat(i)
         lat(i)                = Rtmp

       Stmp = stn_name(LocationSmallest)
         stn_name(LocationSmallest) = stn_name(i)
         stn_name(i)  = Stmp

       RMtmp(:)=0
       RMtmp = depth(:,LocationSmallest)
         depth(:,LocationSmallest) = depth(:,i)
         depth(:,i)  = RMtmp

       RMtmp(:)=0
       RMtmp = field(:,LocationSmallest) ! 22
         field(:,LocationSmallest) = field(:,i) ! put i to location 22
         field(:,i)  = RMtmp ! put location 22 to i

       RMtmp(:)=0
       RMtmp = tqc_lev(:,LocationSmallest)
         tqc_lev(:,LocationSmallest) = tqc_lev(:,i)
         tqc_lev(:,i)  = RMtmp

    ENDDO
    
  return
  END SUBROUTINE sort_ji_argo_1var
  




END MODULE MISC_MODULE
		
