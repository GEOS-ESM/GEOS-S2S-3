MODULE PMEL_MODULE
  
  IMPLICIT NONE
  PUBLIC :: read_raw_pmel

  CONTAINS
 
  ! ..........................................................
    SUBROUTINE read_raw_pmel(infile, nlevs, depths, nidate, &
                            flag, xlat, xlon, nd, tdummy, miss,svar,fl)
  ! ..........................................................

  ! This program reads PMEL anonymous FTP ascii-format temperature
  ! files, for example 8n110w.tmp. It creates an array called t, 
  ! which is evenly spaced in time, and an array called iqual
  ! which contains the data quality for each depth.
  !
  ! Programmed by Dai McClurg, NOAA/PMEL/OCRD, March 1994
  ! Adapted by Sonya Kulkarni, July 29, 1998 for NSIPP
  ! Re-adapated Michele Rienecker, Jan 6, 2000, or changing pir depths between blocks
  ! re-adapated Robin Kovach, April 2004, for new format
  ! re-adapated Robin Kovach, June 2011, for new user/password ftp site
  !
  ! ftp://taopmelftp@ftp.pmel.noaa.gov

   
    IMPLICIT NONE
    !character*62, intent(in) :: infile
    character*100, intent(in) :: infile
    character*4,  intent(in) :: svar
    integer,      intent(in) :: nlevs
    integer,      intent(in) :: nd, fl
    integer,      intent(in) :: nidate(nd)
    real,         intent(in) :: miss 
    
    real,         intent(out)                :: xlat, xlon, flag
    real, dimension(nlevs,1:nd), intent(out) :: tdummy
    real, dimension(nlevs,1:nd), intent(out) :: depths 
    
    integer, parameter  :: nz=42
    integer             :: UNIT_IN = 10
    integer             :: iq, j, k, kk, n, m, len
    real                :: ixlat, ixlon
    integer             :: nblock, nk, ndep, nn, nday, n1, n2
    integer             :: kdep(nz),  idep(nz), idepth(nz)
    real                :: depth(nz), t(nz), rdepth(nz)
      
    character*256       :: header, line, line2, depline, infile2
    integer             :: js, jint, ns
    character*9         :: xypos, xpos, ypos
    integer             :: jfile, markdata, markn, marks, marke, markw, markend, markt


    integer, allocatable, dimension(:,:) :: iqual
    integer, allocatable, dimension(:)   :: idate, ihms
 
    data jfile/50/
    save jfile
    
    character*40  :: FMT
    
    character*1         :: ahem, ohem

    
    !***********************************************************!

    jfile = jfile+1
    tdummy(1:nlevs,1:nd) = miss

    !print *, 'read_raw ',trim(infile(1:fl))
    open(1, file=trim(infile(1:fl)), status='old', form='formatted')

    if (svar=='TEMP') then
      markdata = index(trim(infile),'/t')
    elseif (svar=='SALT') then
      markdata = index(trim(infile),'/s')
    endif
    markend = index(trim(infile),'dy')
    xypos = infile(markdata+2:markend-2)
    !print *, 'xypos ',xypos

    markn = index(trim(xypos),'n')
    marks = index(trim(xypos),'s')
    marke = index(trim(xypos),'e')
    markw = index(trim(xypos),'w')
    markt = index(trim(xypos),'t')

    !print *, 'Marks ',markn, marks, marke, markw
    if (markn > 0) then
      ypos = xypos(1:markn-1)
      ypos = trim(ypos)
      read (ypos,*) ixlat
      ixlat = ixlat
      if (marke > 0) then
        xpos = xypos(markn+1:marke-1)
        xpos = trim(xpos)
        read (xpos,*) ixlon
      else
        xpos = xypos(markn+1:markw-1)
        xpos = trim(xpos)        
        read (xpos,*) ixlon
        ixlon = -ixlon
      endif
    else
      ypos = xypos(1:marks-1)
      ypos = trim(ypos)
      read (ypos,*) ixlat
      ixlat = -ixlat
      if (marke > 0) then
        xpos = xypos(marks+1:marke-1)
        xpos = trim(xpos)
        read (xpos,*) ixlon
      else
        xpos = xypos(marks+1:markw-1)
        xpos = trim(xpos)
        read (xpos,*) ixlon
        ixlon = -ixlon
      endif
    endif
    xlat = ixlat
    xlon = ixlon
    if (xlat>0 .and. xlat<0.1) then
      xlat=0.0
    endif
    !print *, xypos,' ',xlat,xlon

  ! Read total number of days, depths and blocks of data
  ! ....................................................
   read(1,10) nday, ndep, nblock
10 format(49x,i5,7x,i3,8x,i3)
   !print *, '10 nday, ndep, nblock ',nday, ndep, nblock

!    if (xypos == '0n170w') then
!	nday = 11729
!    endif

    !print *, 'allocate', nday, nz
    allocate (idate(nday), ihms(nday))
    allocate (iqual(nz,nday))
 

!!$c Read the missing data flag

    read(1,20) flag
20  format(40x,f7.3)
    !print *, '20', flag

    t     = flag
    iqual = 5
    !isrc  = 0

  ! Read the data
  ! ..............................
    js   = 1
    jint = 1
    n    = 0

    DO m = 1, nblock
       read(1,30) n1, n2, nn, nk
       !print *, '30 ',n1, n2, nn, nk

       !print *, 'm to nblock n1, n2, nn, nk: ', n1, n2, nn, nk
       if(nk > nz) then
          write(6,*) 'need to increase depth array, nk = ',nk
          stop
       endif
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       !40  format(15x,<nk>i7)       ! Old
       !read(1,40) (kdep(k), k=1,nk) ! Old
       write(FMT,'("(15x,",I0,"I7)")') nk
       read(1,FMT) (kdep(k), k=1,nk) ! index line
       !print *, FMT
      
       
       !print *, 'index line ',kdep(1:nk)
       read(1,'(a)') depline        ! Depth Line
       !print *, 'depth line ',depline

       iq    = index(depline,'Q')
       line2 = depline(11:iq-1)   ! Depth Line: just numbers
       
       read(line2,*) (rdepth(k),k=1,nk)
       depth(1:nk) = rdepth(1:nk)       
       
       read(1,'(a)') header
       !print *, 'header ',header
  
       DO ns = n1,n2
          n = n+1
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          !60  format(x,i8,x,i4,x,<nk>f7.3,x,<nk>i1,x,<nk>i2) ! Old
	  !read(1,60) idate(n), ihms(n), t(1:nk)              ! Old
	  write(FMT,'("(x,i8,x,i4,x,",I0,"f7.3,x,",I0,"i1,x,",I0,"i2)")') nk,nk,nk
	  !print *, FMT
          read(1,FMT) idate(n), ihms(n), t(1:nk)         
          
          !print *, '60 ',ns,idate(n), ihms(n), t(1:nk)

          where (t(1:nk) < flag+0.01) t(1:nk) = miss

          if (idate(n) < nidate(1) ) cycle
          if (idate(n) > nidate(nd)) exit 

          do j=js,nd
             if (idate(n) == nidate(j)) then
                jint = j
                kk = 0
                do k=1,nk
                   if(t(k) < miss) then
                      kk = kk + 1
                      tdummy(kk,j) = t(k)
                      depths(kk,j) = depth(k)
                   endif
                enddo  ! k
                exit
             endif
          end do   ! j
          js = jint
       ENDDO    ! ns
       if (idate(n) > nidate(nd)) exit      
    ENDDO     ! m

30  format(50x,i6,3x,i6,1x,i6,7x,i3)
    

 

    close(1)
    !print *, 'close', n

    deallocate( idate, iqual, ihms)
    !print *, 'deallocate'

  return
  END SUBROUTINE read_raw_pmel



END MODULE PMEL_MODULE
