PROGRAM mk_sla_obserror

  use netcdf

  implicit none

  integer                                       :: N_PROF, N_LEVS  
  character*256			                :: fname
  real,allocatable, dimension(:,:)              :: sla, obserror, lon, lat, distance, filt_sla
  real,allocatable, dimension(:,:)                :: ww
  integer                                       :: varid
  integer                                       :: ncid
  integer                                       :: index, step,N, ii
  real  :: mean, L0=100e3


  character*300                                  :: BUFFER 


  call getarg(1,BUFFER)
  read(BUFFER,*) fname

  !fname="sla.nc"

  call get_profile_dim(fname,N_PROF,N_LEVS)

  print *,N_PROF,N_LEVS

  allocate ( obserror(N_LEVS, N_PROF), sla(N_LEVS, N_PROF), filt_sla(N_LEVS, N_PROF))
  
  allocate ( lat(N_PROF, N_LEVS), lon(N_PROF, N_LEVS), distance(N_PROF, N_LEVS) )
  
  
  call check(nf90_open(fname,NF90_WRITE,ncid))

  call check(nf90_inq_varid(ncid,'OBS_ERROR',varid))
  call check(nf90_get_var(ncid,varid,obserror))

  call check(nf90_inq_varid(ncid,'ADT',varid))
  call check(nf90_get_var(ncid,varid,sla))

  call check(nf90_inq_varid(ncid,'LON',varid))
  call check(nf90_get_var(ncid,varid,lon))

  call check(nf90_inq_varid(ncid,'LAT',varid))
  call check(nf90_get_var(ncid,varid,lat))

  

  do index=1,N_PROF-1
     
     call orthodromic_dist(lon(index,1),lat(index,1),lon(index+1,1),lat(index+1,1),distance(index,1))
  !   if ( (distance(index,1)/1000)>100.0 ) then 
     write(100,*)distance(index,1)/1000
     !"d=",distance(index,1)/1000," km"        
        !pause
  !   end if
  end do


  step=10
  N=2*step+1
  allocate(ww(-step:step,1))
  do index=step+1,N_PROF-(step)
     
     !print *,index

     do ii=-step,step
        call orthodromic_dist(lon(index,1),lat(index,1),lon(ii+index,1),lat(ii+index,1),ww(ii,1))

        ww(ii,1) = exp( - (ww(ii,1)/L0)**2 )
     end do
     ww=ww/sum(ww)


     !print *,'sla: ',sla(1,index-step:index+step)
     !print *,'ww: ',ww(-step:step,1)

     !mean=sum(sla(1,index-step:index+step))/real(N)
     mean=sum(ww(-step:step,1)*sla(1,index-step:index+step))

     !print *,mean
     !print *,sla(1,index)
     !read *
     
     !print *,sla(1,index-step:index+step)
     obserror(1,index)=(sum( (sla(1,index-step:index+step)-mean)**2 )/real(N))**0.5

     filt_sla(1,index) = mean

     !print *,"std=",obserror(1,index)," mean=",mean
     !pause

  end do

  obserror(1,1:step)=obserror(1,step+1)
  obserror(1,N_PROF-step+1:N_PROF)=obserror(1,N_PROF-step)

  call check(nf90_inq_varid(ncid,'ADT',varid))
  call check(nf90_put_var(ncid,varid,filt_sla))

  call check(nf90_inq_varid(ncid,'OBS_ERROR',varid))
  call check(nf90_put_var(ncid,varid,obserror))

CONTAINS

  !-----------------------
  subroutine check(status)
    !-----------------------
    integer, intent ( in) :: status

    if(status /= nf90_noerr) then 
       print *, trim(nf90_strerror(status))
       stop "Stopped"
    end if

  end subroutine check

  !-----------------------
  subroutine get_profile_dim(FNAME,N_PROF,N_LEVS)
    !-----------------------

    use netcdf
    implicit none
    integer                                                     :: nDimensions
    integer                                                     :: ncid
    integer                                                     :: N_PROF
    integer                                                     :: N_LEVS
    integer                                                     :: STR8
    character (len = 200), intent(inout)                        :: FNAME

    FNAME=trim(FNAME)

    !GET DIMENSIONS
    call check(nf90_open(FNAME,NF90_NOWRITE,ncid))

    call check( nf90_inq_dimid(ncid, 'N_PROF', nDimensions) )
    call check( nf90_inquire_dimension(ncid, nDimensions, len=N_PROF) )

    call check( nf90_inq_dimid(ncid, 'N_LEVS', nDimensions) )
    call check( nf90_inquire_dimension(ncid, nDimensions, len=N_LEVS) )

    call check(nf90_close(ncid))

  end subroutine get_profile_dim

  !-----------------------
  subroutine orthodromic_dist(lons,lats,lonf,latf,d)
    !-----------------------

    use netcdf
    implicit none
    real :: lons,lats,lonf,latf,d
    real :: dlon, dlat, pi=3.141592653589793, d2r
    real :: R=6378137


    d2r=pi/180.0
    dlon=abs(lonf-lons)
    !dlon=min(dlon,abs(dlon-360))

    dlat=abs(latf-lats)

    d=R*2*asin( sqrt( (sin(dlat*d2r*0.5))**2 + cos(lons*d2r)*cos(lonf*d2r)*(sin(dlon*d2r*0.5))**2 ) )   
    

  end subroutine orthodromic_dist
    

END PROGRAM MK_SLA_OBSERROR
