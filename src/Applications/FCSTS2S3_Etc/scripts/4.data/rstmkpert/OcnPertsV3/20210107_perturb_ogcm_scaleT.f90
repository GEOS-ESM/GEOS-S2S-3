program perturb_aogcm

  use netcdf
  !use odas_resolution
  !use odas_types
  !use my_stats

  implicit none

  real, allocatable, dimension(:,:,:)                   :: CVAR3D,PVAR3D,VAR3D,RESFAC,TVAR
  real, allocatable, dimension(:,:)                     :: CVAR2D,PVAR2D,VAR2D
  real, allocatable, dimension(:)                       :: CVAR1D,PVAR1D,VAR1D

  integer						  :: lon ! just indices for iteration through RESFAC
  integer						  :: lat
  integer 						  :: lev
  real   						  :: T ! single grid point value
  ! ARRAY DIMENSION
  common/dims/NLON,NLAT,NLEV
  integer                                               :: NLON            ! Number of longitude points
  integer                                               :: NLAT            ! Number of latitude points
  integer                                               :: NLEV            ! Number of levels
  
  ! netCDF stuff
  integer                                               :: cncid,cvarid
  integer                                               :: nDimensions
  integer                                               :: lon_dimid, lat_dimid, lev_dimid
  integer, parameter                                    :: NDIMS = 3
  integer                                               :: state_dimids(NDIMS)

  
  real                                                  :: meanT
  real                                                  :: meanS

  !type(ocean)                                           :: X
  !type(ocean)                                           :: sigX
  !type(ocean)                                           :: dX

  integer                                               :: sign_of_bv

  character (len = 200)                                 :: RPFILE_NAME         ! Name of the data file to be written
  character (len = 200)                                 :: RNFILE_NAME         ! Name of the data file to be written
  character (len = 200)                                 :: CFILE_NAME          ! Name of the data file to be read
  character (len = 200)                                 :: NFILE_NAME          ! Name of the data file to be read
  character (len = 200)                                 :: PFILE_NAME          ! Name of the data file to be read
  character (len = 200)                                 :: VAR_NAME            ! Descriptive name of the variable
  character (len = 200), dimension(2)                   :: VAR_NAME_TEMP
  character (len = 200), dimension(2)                   :: VAR_NAME_CUR
  character (len = 200), dimension(6)                   :: VAR_NAME_SBC
  character (len = 200), dimension(7)                   :: VAR_NAME_ICE
  character (len = 200), dimension(6)                   :: VAR_NAME_SLT
  character (len = 200), dimension(3)                   :: VAR_NAME_FVC
  character (len = 200), dimension(1)                   :: VAR_NAME_MST

  character (len = 200)                                 :: EOF_BASEDIR           ! Name of the data file to be read
  character (len = 200)                                 :: EOF_FNAME           ! Name of the data file to be read

  !Random number
  !=============
  integer, parameter                                    :: Ne=10              ! # of EOF to read
  integer                                               :: iseed, i
  real, dimension(Ne)                                   :: beta

  !Dummies
  !=======
  integer                                               :: index

  !I/O stuff
  !=========
  integer  :: ioerr

  !COMMAND LINE ARGUMENT STUFF
  !===========================
  character*300                      :: BUFFER  
  real                               :: resl

  !VARIABLE NAMES 
  !===========================
  data VAR_NAME_TEMP /"temp"  ,"salt"   /
  data VAR_NAME_CUR  /"u"     ,"v"      /

  !Read Temp and Salt from MOM restart files
  !-----------------------------------------
  CFILE_NAME="./cdata/aocean_temp_salt.res.nc"
  NFILE_NAME="./ndata/uocean_temp_salt.res.nc"
  PFILE_NAME="./pdata/uocean_temp_salt.res.nc"
  RNFILE_NAME="./ndata/ocean_temp_salt.res.nc"
  RPFILE_NAME="./pdata/ocean_temp_salt.res.nc"

  !READ IN RESCALING MAGNITUDE
  open (99, file='coef.dat')
  read (99,*) resl
  close(99)
  print*,'Rescaling factor read:',resl

  ! COMPUTE BIN's TAPERING ! read temperature only
  ! Open netcdf file
  call check( nf90_open(CFILE_NAME, NF90_WRITE, cncid) )
  !Read dimensions
  call check( nf90_inq_dimid(cncid, 'xaxis_1', nDimensions) )
  call check( nf90_inquire_dimension(cncid, nDimensions, len=NLON) )
  print *,'NLON=',NLON
  call check( nf90_inq_dimid(cncid, 'yaxis_1', nDimensions) )
  call check( nf90_inquire_dimension(cncid, nDimensions, len=NLAT) )
  print *,'NLAT=',NLAT
  call check( nf90_inq_dimid(cncid, 'zaxis_1', nDimensions) )
  call check( nf90_inquire_dimension(cncid, nDimensions, len=NLEV) )
  print *,'NLEV=',NLEV

  allocate(TVAR(NLON,NLAT,NLEV),RESFAC(NLON,NLAT,NLEV))

  call check( nf90_inq_varid(cncid, 'temp', cvarid) )
  call check( nf90_get_var(cncid, cvarid, TVAR) )
  print *,'FINISHED READING temp for scaling'
    
  ! call Bin's mask to create 3-dimensional RESFAC
  RESFAC = 1.0

  do lat = 1, NLAT
    do lon = 1, NLON
      T = TVAR(lon,lat,1)  ! use only sst T(:,:,1) to compute the scaling factor - check that T indexing starts at the top of the ocean
      RESFAC(lon,lat,1) = ScalingFactor(T)  !!! optional for now: L,U
    end do
  end do
  do lev = 2, NLEV
    do lat = 1, NLAT
      do lon = 1, NLON
        RESFAC(lon,lat,lev) = RESFAC(lon,lat,1)
      end do
    end do
  end do
  print *,'FINISHED RESFAC'
  print *,'RESFAC 180 90; min/max=',RESFAC(180,90,1),minval(RESFAC),maxval(RESFAC)
  deallocate(TVAR)

  DO I=1,2
  !DO I=1,1
  VAR_NAME=VAR_NAME_TEMP(I)
  print *,'****** UPDATE ',VAR_NAME
  sign_of_bv=-1
  !RNBV=CTR-(UPBV-UNBV)/2
  call read_3D3(CFILE_NAME, NFILE_NAME, PFILE_NAME, VAR3D, VAR_NAME, resl,sign_of_bv,RESFAC)
  call write_3D(RNFILE_NAME, VAR3D, VAR_NAME)
  deallocate(VAR3D)
  sign_of_bv= 1
  !RPBV=CTR+(UPBV-UNBV)/2
  call read_3D3(CFILE_NAME, NFILE_NAME, PFILE_NAME, VAR3D, VAR_NAME, resl,sign_of_bv,RESFAC)
  call write_3D(RPFILE_NAME, VAR3D, VAR_NAME)
  deallocate(VAR3D)
  ENDDO

  print*,'OCN MOM RESTARTS GENERATED ocean_temp_salt - temp salt'
  
  
  RESFAC = 1.0  

  !Read Zonal and Meridional Currents from MOM restart files
  !-----------------------------------------
  CFILE_NAME="./cdata/aocean_velocity.res.nc"
  NFILE_NAME="./ndata/uocean_velocity.res.nc"
  PFILE_NAME="./pdata/uocean_velocity.res.nc"
  RNFILE_NAME="./ndata/ocean_velocity.res.nc"
  RPFILE_NAME="./pdata/ocean_velocity.res.nc"

  DO I=1,2
  VAR_NAME=VAR_NAME_CUR(I)
  print *,'****** UPDATE ',VAR_NAME
  sign_of_bv=-1
  !RNBV=CTR-(UPBV-UNBV)/2
  call read_3D3(CFILE_NAME, NFILE_NAME, PFILE_NAME, VAR3D, VAR_NAME, resl,sign_of_bv,RESFAC)
  call write_3D(RNFILE_NAME, VAR3D, VAR_NAME)
  deallocate(VAR3D)
  sign_of_bv= 1
  !RPBV=CTR+(UPBV-UNBV)/2
  call read_3D3(CFILE_NAME, NFILE_NAME, PFILE_NAME, VAR3D, VAR_NAME, resl,sign_of_bv,RESFAC)
  call write_3D(RPFILE_NAME, VAR3D, VAR_NAME)
  deallocate(VAR3D)
  ENDDO

  print*,'OCN MOM RESTARTS GENERATED ocean_velocity - u v'

!========================================================================
!========================================================================

contains
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
  real function ScalingFactor(T,L,U)
  !-----------------------
    real, intent ( in) :: T
    real, optional :: L,U  ! L and U are lower and upper limits to mask temperature T
    real :: y
    
    real :: lo = -1.0
    real :: up = 1.0
    if (present(L)) lo = L
    if (present(U)) up = U

    if(T < lo) then
       ScalingFactor = 0.0
    elseif(T > up) then
       ScalingFactor = 1.0
    else
       y = (T - lo) / (up - lo)
       ScalingFactor = y * y * y * (6.0 * y * y - 15.0*y + 10.0)
    endif

  end function ScalingFactor


  !------------------------------------------
  subroutine read_3D3(CFILE_NAME,NFILE_NAME,PFILE_NAME,VAR,VAR_NAME,resl,sign_of_bv, RESFAC)
  !------------------------------------------

    use netcdf
    implicit none

    character (len = 200), intent(in)                     :: CFILE_NAME,NFILE_NAME,PFILE_NAME ! Name of the data file to be read
    character (len = 200), intent(in)                     :: VAR_NAME            ! Descriptive name of the variable
    real                                                  :: RESFAC(NLON,NLAT,NLEV)
    
    ! ARRAY DIMENSION
    common/dims/NLON,NLAT,NLEV
    integer                                               :: NLON            ! Number of longitude points
    integer                                               :: NLAT            ! Number of latitude points
    integer                                               :: NLEV            ! Number of levels
    real                                                  :: resl            ! rescaling factor
    integer                                               :: sign_of_bv      ! poitive or negative choice: -1 or +1

    ! VARIABLES
    real, allocatable, dimension(:,:,:)                   :: CVAR,NVAR,PVAR,VAR   ! 3D array of stuff

    ! netCDF stuff
    integer                                               :: cncid,nncid,pncid
    integer                                               :: cvarid,nvarid,pvarid
    integer                                               :: nDimensions
    integer                                               :: lon_dimid, lat_dimid, lev_dimid
    integer, parameter                                    :: NDIMS = 3
    integer                                               :: state_dimids(NDIMS)

    ! Open netcdf file
    call check( nf90_open(CFILE_NAME, NF90_WRITE, cncid) )
    call check( nf90_open(NFILE_NAME, NF90_WRITE, nncid) )
    call check( nf90_open(PFILE_NAME, NF90_WRITE, pncid) )
    
    !Read dimensions.
    call check( nf90_inq_dimid(cncid, 'xaxis_1', nDimensions) )
    call check( nf90_inquire_dimension(cncid, nDimensions, len=NLON) )
    print *,'NLON=',NLON

    call check( nf90_inq_dimid(cncid, 'yaxis_1', nDimensions) )
    call check( nf90_inquire_dimension(cncid, nDimensions, len=NLAT) )
    print *,'NLAT=',NLAT

    call check( nf90_inq_dimid(cncid, 'zaxis_1', nDimensions) )
    call check( nf90_inquire_dimension(cncid, nDimensions, len=NLEV) )
    print *,'NLEV=',NLEV

    allocate(VAR(NLON,NLAT,NLEV),CVAR(NLON,NLAT,NLEV),NVAR(NLON,NLAT,NLEV),PVAR(NLON,NLAT,NLEV))

    call check( nf90_inq_varid(cncid, VAR_NAME, cvarid) )
    call check( nf90_inq_varid(nncid, VAR_NAME, nvarid) )
    call check( nf90_inq_varid(pncid, VAR_NAME, pvarid) )
    call check( nf90_get_var(cncid, cvarid, CVAR) )
    call check( nf90_get_var(nncid, nvarid, NVAR) )
    call check( nf90_get_var(pncid, pvarid, PVAR) )

    print *,'FINISHED READING ALL VARs'
    print *,'GETTING READY TO SCALE'
    
    do lev = 1, NLEV
      do lat = 1, NLAT
        do lon = 1, NLON
           VAR(lon,lat,lev) = CVAR(lon,lat,lev) + sign_of_bv*(PVAR(lon,lat,lev)-NVAR(lon,lat,lev))*RESFAC(lon,lat,lev)*resl/2.
        enddo
      enddo
    enddo
    
    print *, 'FINISHED SCALING'
    print *, VAR_NAME
    
    print *,'VAR 180 90;min/max=',VAR(180,90,1),minval(VAR),maxval(VAR)
    print *,'CVAR 180 90;min/max ',CVAR(180,90,1),minval(CVAR),maxval(CVAR)
    print *,'NVAR 180 90;min/max ',NVAR(180,90,1),minval(NVAR),maxval(NVAR)
    print *,'PVAR 180 90;min/max ',PVAR(180,90,1),minval(PVAR),maxval(PVAR)
    print *,"*** SUCCESS reading control file ", CFILE_NAME
    print *,"*** SUCCESS reading un-nbv  file ", NFILE_NAME
    print *,"*** SUCCESS reading un-pbv  file ", PFILE_NAME
    
    if (VAR_NAME .eq.'temp') then
     if ( minval(VAR).lt.-5.   ) print*,' Min PERT ErrVAR< -5:',minval(VAR)
     if ( minval(CVAR).lt.-5.  ) print*,' Min ASSI ErrVAR< -5:',minval(CVAR)
     if ( minval(NVAR).lt.-5   ) print*,' Min BV   ErrVAR< -5:',minval(NVAR)
     if ( minval(PVAR).lt.-5.  ) print*,' Min BV   ErrVAR< -5:',minval(PVAR)
     if ( maxval(VAR).gt.55.   ) print*,' Max PERT ErrVAR> 55:',maxval(VAR)
     if ( maxval(CVAR).gt.55.  ) print*,' Max ASSI ErrVAR> 55:',maxval(CVAR)
     if ( maxval(NVAR).gt.55   ) print*,' Max BV   ErrVAR> 55:',maxval(NVAR)
     if ( maxval(PVAR).gt.55.  ) print*,' Max BV   ErrVAR> 55:',maxval(PVAR)
    endif

    if (VAR_NAME .eq.'salt') then
     if ( minval(VAR).lt.-1.   ) print*,' Min PERT ErrVAR< -1:',minval(VAR)
     if ( minval(CVAR).lt.-1.  ) print*,' Min ASSI ErrVAR< -1:',minval(CVAR)
     if ( minval(NVAR).lt.-1   ) print*,' Min BV   ErrVAR< -1:',minval(NVAR)
     if ( minval(PVAR).lt.-1.  ) print*,' Min BV   ErrVAR< -1:',minval(PVAR)
     if ( maxval(VAR).gt.65.   ) print*,' Max PERT ErrVAR> 65:',maxval(VAR)
     if ( maxval(CVAR).gt.65.  ) print*,' Max ASSI ErrVAR> 65:',maxval(CVAR)
     if ( maxval(NVAR).gt.65   ) print*,' Max BV   ErrVAR> 65:',maxval(NVAR)
     if ( maxval(PVAR).gt.65.  ) print*,' Max BV   ErrVAR> 65:',maxval(PVAR)
    endif

    
    deallocate(CVAR,NVAR,PVAR)

  end subroutine read_3D3

  !------------------------------------------
  subroutine read_3D(CFILE_NAME,VAR,VAR_NAME)
  !------------------------------------------

    use netcdf
    implicit none


    character (len = 200), intent(in)                     :: CFILE_NAME ! Name of the data file to be read
    character (len = 200), intent(in)                     :: VAR_NAME            ! Descriptive name of the variable
    
    ! ARRAY DIMENSION
    integer                                               :: NLON            ! Number of longitude points
    integer                                               :: NLAT            ! Number of latitude points
    integer                                               :: NLEV            ! Number of levels

    ! VARIABLES
    real, allocatable, dimension(:,:,:)                   :: CVAR,VAR   ! 3D array of stuff

    ! netCDF stuff
    integer                                               :: cncid
    integer                                               :: cvarid
    integer                                               :: nDimensions
    integer                                               :: lon_dimid, lat_dimid, lev_dimid
    integer, parameter                                    :: NDIMS = 3
    integer                                               :: state_dimids(NDIMS)

    ! Open netcdf file
    call check( nf90_open(CFILE_NAME, NF90_WRITE, cncid) )
    
    !Read dimensions.
    call check( nf90_inq_dimid(cncid, 'xaxis_1', nDimensions) )
    call check( nf90_inquire_dimension(cncid, nDimensions, len=NLON) )
    print *,'NLON=',NLON

    call check( nf90_inq_dimid(cncid, 'yaxis_1', nDimensions) )
    call check( nf90_inquire_dimension(cncid, nDimensions, len=NLAT) )
    print *,'NLAT=',NLAT

    call check( nf90_inq_dimid(cncid, 'zaxis_1', nDimensions) )
    call check( nf90_inquire_dimension(cncid, nDimensions, len=NLEV) )
    print *,'NLEV=',NLEV

    allocate(VAR(NLON,NLAT,NLEV),CVAR(NLON,NLAT,NLEV))

    call check( nf90_inq_varid(cncid, VAR_NAME, cvarid) )
    call check( nf90_get_var(cncid, cvarid, CVAR) )

    ! saving
    VAR = CVAR
    print *,'VAR=',VAR(180,90,1)
    print *,'CVAR',CVAR(180,90,1)
    print *,"*** SUCCESS reading file ", CFILE_NAME
    
    deallocate(CVAR)

  end subroutine read_3D

  !------------------------------------------
  subroutine write_3D(FILE_NAME,VAR,VAR_NAME)
  !------------------------------------------

    use netcdf
    implicit none


    character (len = 200), intent(in)                     :: FILE_NAME           ! Name of the data file to be read
    character (len = 200), intent(in)                     :: VAR_NAME            ! Descriptive name of the variable
    
    ! ARRAY DIMENSION
    integer                                               :: NLON            ! Number of longitude points
    integer                                               :: NLAT            ! Number of latitude points
    integer                                               :: NLEV            ! Number of levels

    ! VARIABLES
    real, allocatable, dimension(:,:,:)                   :: VAR              ! 3D array of stuff

    ! netCDF stuff
    integer                                               :: ncid
    integer                                               :: varid
    integer                                               :: nDimensions
    integer                                               :: lon_dimid, lat_dimid, lev_dimid
    integer, parameter                                    :: NDIMS = 3
    integer                                               :: state_dimids(NDIMS)

    ! Create netcdf file
    call check( nf90_open(FILE_NAME, NF90_WRITE, ncid) )

    
    ! Define the dimensions.
    !NLON=size(VAR,1)
    !NLAT=size(VAR,2)
    !NLEV=size(VAR,3)
    !call check( nf90_def_dim(ncid, 'lat', NLAT, lat_dimid) )
    !call check( nf90_def_dim(ncid, 'lon', NLON, lon_dimid) )
    !call check( nf90_def_dim(ncid, 'lev', NLEV, lev_dimid) )
    !state_dimids = (/ lon_dimid, lat_dimid, lev_dimid/)
    !call check( nf90_def_var(ncid, VAR_NAME, NF90_REAL, state_dimids, varid) )
    !call check( nf90_enddef(ncid) )
    call check( nf90_inq_varid(ncid, VAR_NAME, varid) )
    call check( nf90_put_var(ncid, varid, VAR))
    call check( nf90_close(ncid) )

    print *,"*** SUCCESS writing file ", FILE_NAME

  end subroutine write_3D

end program perturb_aogcm

