MODULE SORT_MODULE

  IMPLICIT NONE
     
  PUBLIC :: sort_odas2, sort_odas2_argo
  CONTAINS

  ! ............................................................
    SUBROUTINE sort_odas2(Item,date,lon,lat,npts,data_id,stn_id,qc_prf,&
                          qc_flag,depth,field,qc_lev,max_npts,nobs)
  ! ............................................................
    
    IMPLICIT NONE
    integer,     dimension(:),  intent(inout) :: Item, date, stn_id, npts
    real,        dimension(:),  intent(inout) :: lon, lat, qc_prf
    integer,     dimension(:),  intent(inout) :: qc_flag, data_id
    integer,                    intent(in)    :: max_npts, nobs
    real,        dimension(max_npts,nobs)     :: depth, field, qc_lev
   
    integer  :: NumItems, SmallestItem, tmp, i, LocationSmallest
    integer, dimension(1) :: MINLOC_array

    integer                        :: Itmp
    real                           :: Rtmp
    real, dimension(max_npts)      :: RMtmp

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
       Itmp = data_id(LocationSmallest)
         data_id(LocationSmallest) = data_id(i)
         data_id(i)  = Itmp

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
  END SUBROUTINE sort_odas2


  ! ............................................................
    SUBROUTINE sort_odas2_ts(Item,date,lon,lat,npts,snpts,data_id,stn_id,tqc_prf,sqc_prf,&
            tqc_flag,sqc_flag,depth,field,tqc_lev,field2,sqc_lev,depth2,max_npts,nobs)
  ! ............................................................
    
    IMPLICIT NONE
    integer,     dimension(:),  intent(inout) :: Item, date, stn_id, npts, snpts,tqc_flag, sqc_flag, data_id
    real,        dimension(:),  intent(inout) :: lon, lat, tqc_prf, sqc_prf
    integer,                    intent(in)    :: max_npts, nobs
    real,        dimension(max_npts,nobs),  intent(inout)     :: field,  tqc_lev, depth
    real,        dimension(max_npts,nobs),  intent(inout)     :: field2, sqc_lev, depth2
   
    integer  :: NumItems, SmallestItem, tmp, i, LocationSmallest, MINLOCI
    integer, dimension(1) :: MINLOC_array

    integer                        :: Itmp
    real                           :: Rtmp
    real, dimension(max_npts)      :: RMtmp

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
       Itmp = data_id(LocationSmallest)
         data_id(LocationSmallest) = data_id(i)
         data_id(i)                = Itmp

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
  END SUBROUTINE sort_odas2_ts


  ! ............................................................
    SUBROUTINE sort_odas2_argo(Item,lon,lat,npts,data_id,&
                           depth,field,tqc_lev,max_npts,nobs)
  ! ............................................................
    
    IMPLICIT NONE
    integer,     dimension(:),  intent(inout) :: Item, npts, data_id
    real,        dimension(:),  intent(inout) :: lon, lat
    integer,                    intent(in)    :: max_npts, nobs
    real,        dimension(max_npts,nobs),  intent(inout)     :: field,  tqc_lev, depth
   
    integer  :: NumItems, SmallestItem, tmp, i, LocationSmallest, MINLOCI
    integer, dimension(1) :: MINLOC_array

    integer                        :: Itmp
    real                           :: Rtmp
    real, dimension(max_npts)      :: RMtmp

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

       !Itmp = tqc_flag(LocationSmallest)
        ! tqc_flag(LocationSmallest) = tqc_flag(i)
       !  tqc_flag(i)                = Itmp

       Itmp = data_id(LocationSmallest)
         data_id(LocationSmallest) = data_id(i)
         data_id(i)  = Itmp

       Rtmp = lon(LocationSmallest)
         lon(LocationSmallest) = lon(i)
         lon(i)                = Rtmp
       Rtmp = lat(LocationSmallest)
         lat(LocationSmallest) = lat(i)
         lat(i)                = Rtmp

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
  END SUBROUTINE sort_odas2_argo
  




END MODULE SORT_MODULE
		
