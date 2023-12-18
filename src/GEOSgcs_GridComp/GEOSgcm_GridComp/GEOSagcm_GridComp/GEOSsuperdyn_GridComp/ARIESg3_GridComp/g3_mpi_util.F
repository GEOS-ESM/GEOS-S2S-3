C **********************************************************************
      subroutine init_dynamics_lattice ( lattice,comm,imglobal,jmglobal,lm )
C **********************************************************************
      use g3_dynamics_lattice_module
      implicit none
      include 'g3_mymalloc_interface'
      type ( dynamics_lattice_type ) lattice
      integer comm,imglobal,jmglobal,lm
      real(kind=8) dummy

      include 'mpif.h'
      integer  status(mpi_status_size)
      integer  myid,ierror,im,jm,i,j,m,n,npes,rm
      integer  isum,jsum,imx,nx, img, ppe(imglobal)

      call mpi_comm_rank ( comm,myid,ierror )

      call mymalloc ( lattice%ppeg,imglobal )

      lattice%comm = comm
      lattice%myid = myid
      lattice%nx   = size( lattice%im )
      lattice%ny   = size( lattice%jm )

      npes = lattice%nx*lattice%ny

      lattice%imglobal = imglobal
      lattice%jmglobal = jmglobal
      lattice%lm       = lm

      if( kind(dummy).eq.4 ) lattice%mpi_rkind = mpi_real
      if( kind(dummy).eq.8 ) lattice%mpi_rkind = mpi_double_precision

c Initialize lattice%im
c ---------------------
      im = imglobal/lattice%nx
      rm = imglobal-lattice%nx*im
      lattice%imax = im
      do n=0,lattice%nx-1
                      lattice%im(n) = im
      if( n.le.rm-1 ) lattice%im(n) = im+1
                      lattice%imax  = max( lattice%imax,lattice%im(n) )
      enddo

c Initialize lattice%jm
c ---------------------
      jm = jmglobal/lattice%ny
      rm = jmglobal-lattice%ny*jm
      lattice%jmax = jm
      do n=0,lattice%ny-1
                      lattice%jm(n) = jm
      if( n.le.rm-1 ) lattice%jm(n) = jm+1
                      lattice%jmax  = max( lattice%jmax,lattice%jm(n) )
      enddo

c Initialize relative PE address
c ------------------------------
      lattice%pei = mod(myid,lattice%nx)
      lattice%pej =     myid/lattice%nx

c Initialize global index for local locations
c -------------------------------------------
      call mymalloc ( lattice%iglobal,lattice%im(lattice%pei) )
      call mymalloc ( lattice%jglobal,lattice%jm(lattice%pej) )

      isum = 0
      do n=0,lattice%nx-1
         if ( n.eq.lattice%pei ) then
            do i=1,lattice%im(n)
            lattice%iglobal(i) = i+isum
            enddo
         endif
      isum = isum + lattice%im(n)
      enddo

      jsum = 0
      do m=0,lattice%ny-1
         if ( m.eq.lattice%pej ) then
            do j=1,lattice%jm(m)
            lattice%jglobal(j) = j+jsum
            enddo
         endif
      jsum = jsum + lattice%jm(m)
      enddo

c Initialize local index for global locations
c -------------------------------------------
      call mymalloc ( lattice%ilocal,lattice%imglobal )
      call mymalloc ( lattice%jlocal,lattice%jmglobal )

         n = 0
      isum = 0
      do i = 1,imglobal
      if(i.gt.isum+lattice%im(n) ) then
      isum =  isum+lattice%im(n)
         n =  n + 1
      endif
      lattice%ilocal(i) = i-isum
      enddo

         m = 0
      jsum = 0
      do j = 1,jmglobal
      if(j.gt.jsum+lattice%jm(m) ) then
      jsum =  jsum+lattice%jm(m)
         m =  m + 1
      endif
      lattice%jlocal(j) = j-jsum
      enddo

c Initialize relative PE address for global i-j locations
c -------------------------------------------------------
      call mymalloc ( lattice%peiglobal,imglobal )
      call mymalloc ( lattice%pejglobal,jmglobal )

      isum = 0
      do n=0,lattice%nx-1
         do i=1,lattice%im(n)
         lattice%peiglobal( i+isum ) = n
         enddo
      isum = isum + lattice%im(n)
      enddo

      jsum = 0
      do m=0,lattice%ny-1
         do j=1,lattice%jm(m)
         lattice%pejglobal( j+jsum ) = m
         enddo
      jsum = jsum + lattice%jm(m)
      enddo

c Initialize Pole PE ghosts for each iglobal
c ------------------------------------------
      isum = 0
      do nx=0,lattice%nx-1
        imx = lattice%im(nx)
        do i=1,imx
        ppe(i+isum) = nx
        enddo
        isum = isum + imx
      enddo

      do i=1,imglobal/2
      lattice%ppeg(i) = ppe(i+imglobal/2)
      lattice%ppeg(i+imglobal/2) = ppe(i)
      enddo

c Allocate Lattice%img
c --------------------
      if(.not.associated(lattice%img)) then
      allocate ( lattice%img(0:nx-1,imglobal) )
      do m=1,imglobal
      do n=0,nx-1
      lattice%img(n,m) = 0
      enddo
      enddo
      else
             m=size(lattice%img)
          if(m.ne.nx*imglobal) then
          print *, 'Allocated Lattice Size (',m,') does not match request (',nx*imglobal,') for lattice%img!'
          call my_finalize
          call my_exit (101)
          endif
      endif

c Allocate Lattice%im0
c --------------------
      if(.not.associated(lattice%im0)) then
      allocate ( lattice%im0(0:nx-1,imglobal) )
      do m=1,imglobal
      do n=0,nx-1
      lattice%im0(n,m) = 0
      enddo
      enddo
      else
             m=size(lattice%im0)
          if(m.ne.nx*imglobal) then
          print *, 'Allocated Lattice Size (',m,') does not match request (',nx*imglobal,') for lattice%im0!'
          call my_finalize
          call my_exit (101)
          endif
      endif

c Determine Pole PE ghosts for each Processor
c -------------------------------------------
      isum = 0
      do nx=0,lattice%nx-1
        imx  = lattice%im(nx)
        lattice%npeg(nx)   = 1
        lattice% im0(nx,1) = 1
        img  = 1
        lattice%img (nx,lattice%npeg(nx)) = img
        do i=2,imx
           if( lattice%ppeg(i+isum) .eq. lattice%ppeg(i-1+isum) ) then
           img  = img  + 1
           lattice%img (nx,lattice%npeg(nx)) = img
           else
           lattice%npeg(nx) = lattice%npeg(nx) + 1
           lattice%im0 (nx,lattice%npeg(nx)) = i
           img  = 1
           lattice%img (nx,lattice%npeg(nx)) = img
           endif
        enddo
      isum = isum + imx
      enddo

#if 0
c Print Lattice Characteristics
c -----------------------------
      if( myid.eq.0 ) then
      print *, 'Number of Processors in X: ',lattice%nx
      print *, 'Number of Processors in Y: ',lattice%ny
      print *
      endif

      do n=0,npes-1
         if( n.eq.myid ) then
         write(6,1000) myid,lattice%pei,lattice%pej,lattice%im(lattice%pei),lattice%jm(lattice%pej)
         endif
      call mpi_barrier (lattice%comm,ierror)
      enddo
      if( myid.eq.npes-1 ) print *

 1000 format(1x,'absolute PE id: ',i3,'  relative PE (i,j): (',i2,',',i2,')  (im,jm) = (',i3,',',i3,')')
#endif
      return
      end

C **********************************************************************
      subroutine scatter_1d ( qglobal,qlocal,lattice )
C **********************************************************************
      use g3_dynamics_lattice_module
      implicit none
      type   ( dynamics_lattice_type ) lattice
      real(kind=8) qglobal( lattice%imglobal )
      real(kind=8) qlocal ( lattice%im(lattice%myid) )

      include 'mpif.h'
      integer status(mpi_status_size)

      integer comm
      integer i,n,loc,im,imx,imglobal,myid,npes,ierror,mpi_rkind
      real(kind=8), allocatable ::   buf(:)

      comm      = lattice%comm
      myid      = lattice%myid
      npes      = lattice%nx
      im        = lattice%im(myid)
      imglobal  = lattice%imglobal
      mpi_rkind = lattice%mpi_rkind

      if( myid.eq.0 ) then
      do i=1,im
      qlocal(i) = qglobal(i)
      enddo

      loc = im
      do n=1,npes-1
      imx = lattice%im(n)
      allocate ( buf(imx) )
         do i=1,imx
         loc = loc + 1
         buf(i) = qglobal(loc)
         enddo
      call mpi_send ( buf,imx,mpi_rkind,n,n,comm,ierror )
      deallocate ( buf )
      enddo
      else
      call mpi_recv ( qlocal,im,mpi_rkind,0,myid,comm,status,ierror )

      endif

      return
      end

C **********************************************************************
      subroutine scatter_2d ( qglobal,qlocal,lattice )
C **********************************************************************
      use g3_dynamics_lattice_module
      implicit none
      type   ( dynamics_lattice_type ) lattice
      real(kind=8) qglobal( lattice%imglobal,lattice%jmglobal )
      real(kind=8) qlocal ( lattice%im(lattice%pei),lattice%jm(lattice%pej) )

      include 'mpif.h'
      integer status(mpi_status_size)

      integer comm
      integer myid,npe,ierror,mpi_rkind
      integer nx,i,iloc,im,imx,imglobal,isum
      integer ny,j,jloc,jm,jmy,jmglobal,jsum
      real(kind=8), allocatable ::   buf(:,:)

      comm      = lattice%comm
      myid      = lattice%myid
      iloc      = lattice%pei
      jloc      = lattice%pej
      im        = lattice%im(iloc)
      jm        = lattice%jm(jloc)
      imglobal  = lattice%imglobal
      jmglobal  = lattice%jmglobal
      mpi_rkind = lattice%mpi_rkind

      if( myid.eq.0 ) then
          jsum = 0
          do ny=0,lattice%ny-1
          jmy = lattice%jm(ny)
             isum = 0
             do nx=0,lattice%nx-1
             imx = lattice%im(nx)

             if( nx.eq.0 .and. ny.eq.0 ) then
                 do j=1,jmy
                 do i=1,imx
                 qlocal(i,j) = qglobal(i,j)
                 enddo
                 enddo
                 isum = isum + imx
             else

                 allocate ( buf(imx,jmy) )
                 do j=1,jmy
                 do i=1,imx
                 buf(i,j) = qglobal(i+isum,j+jsum)
                 enddo
                 enddo
                 isum = isum + imx
             
                 npe = nx + ny*lattice%nx
                 call mpi_send ( buf,imx*jmy,mpi_rkind,npe,npe,comm,ierror )
                 deallocate ( buf )

             endif
             enddo
          jsum = jsum + jmy
          enddo
      else

          call mpi_recv ( qlocal,im*jm,mpi_rkind,0,myid,comm,status,ierror )

      endif

      call mpi_barrier ( comm,ierror )

      return
      end

C **********************************************************************
      subroutine iscatter_2d ( qglobal,qlocal,lattice )
C **********************************************************************
      use g3_dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      integer   qglobal( lattice%imglobal,lattice%jmglobal )
      integer   qlocal ( lattice%im(lattice%pei),lattice%jm(lattice%pej) )

      include 'mpif.h'
      integer status(mpi_status_size)

      integer comm
      integer myid,npe,ierror
      integer nx,i,iloc,im,imx,imglobal,isum
      integer ny,j,jloc,jm,jmy,jmglobal,jsum
      integer, allocatable ::   buf(:,:)

      comm     = lattice%comm
      myid     = lattice%myid
      iloc     = lattice%pei
      jloc     = lattice%pej
      im       = lattice%im(iloc)
      jm       = lattice%jm(jloc)
      imglobal = lattice%imglobal
      jmglobal = lattice%jmglobal

      if( myid.eq.0 ) then
          jsum = 0
          do ny=0,lattice%ny-1
          jmy = lattice%jm(ny)
             isum = 0
             do nx=0,lattice%nx-1
             imx = lattice%im(nx)

             if( nx.eq.0 .and. ny.eq.0 ) then
                 do j=1,jmy
                 do i=1,imx
                 qlocal(i,j) = qglobal(i,j)
                 enddo
                 enddo
                 isum = isum + imx
             else

                 allocate ( buf(imx,jmy) )
                 do j=1,jmy
                 do i=1,imx
                 buf(i,j) = qglobal(i+isum,j+jsum)
                 enddo
                 enddo
                 isum = isum + imx
             
                 npe = nx + ny*lattice%nx
                 call mpi_send ( buf,imx*jmy,mpi_integer,npe,npe,comm,ierror )
                 deallocate ( buf )

             endif
             enddo
          jsum = jsum + jmy
          enddo
      else

          call mpi_recv ( qlocal,im*jm,mpi_integer,0,myid,comm,status,ierror )

      endif

      call mpi_barrier ( comm,ierror )

      return
      end

C **********************************************************************
      subroutine scatter_3d ( qglobal,qlocal,lattice )
C **********************************************************************
      use g3_dynamics_lattice_module
      implicit none
      type   ( dynamics_lattice_type ) lattice
      real(kind=8) qglobal( lattice%imglobal,lattice%jmglobal,lattice%lm )
      real(kind=8) qlocal ( lattice%im(lattice%pei),lattice%jm(lattice%pej),lattice%lm )

      include 'mpif.h'
      integer status(mpi_status_size)

      integer comm,lm,L
      integer myid,npe,ierror,mpi_rkind
      integer nx,i,iloc,im,imx,imglobal,isum
      integer ny,j,jloc,jm,jmy,jmglobal,jsum
      real(kind=8), allocatable ::   buf(:,:,:)

      comm      = lattice%comm
      myid      = lattice%myid
      iloc      = lattice%pei
      jloc      = lattice%pej
      im        = lattice%im(iloc)
      jm        = lattice%jm(jloc)
      lm        = lattice%lm
      imglobal  = lattice%imglobal
      jmglobal  = lattice%jmglobal
      mpi_rkind = lattice%mpi_rkind

      if( myid.eq.0 ) then
          jsum = 0
          do ny=0,lattice%ny-1
          jmy = lattice%jm(ny)
             isum = 0
             do nx=0,lattice%nx-1
             imx = lattice%im(nx)

             if( nx.eq.0 .and. ny.eq.0 ) then
                 do L=1,lm 
                 do j=1,jmy
                 do i=1,imx
                 qlocal(i,j,L) = qglobal(i,j,L)
                 enddo
                 enddo
                 enddo
                 isum = isum + imx
             else

                 allocate ( buf(imx,jmy,lm) )
                 do L=1,lm
                 do j=1,jmy
                 do i=1,imx
                 buf(i,j,L) = qglobal(i+isum,j+jsum,L)
                 enddo
                 enddo
                 enddo
                 isum = isum + imx
             
                 npe = nx + ny*lattice%nx
                 call mpi_send ( buf,imx*jmy*lm,mpi_rkind,npe,npe,comm,ierror )
                 deallocate ( buf )

             endif
             enddo
          jsum = jsum + jmy
          enddo
      else

          call mpi_recv ( qlocal,im*jm*lm,mpi_rkind,0,myid,comm,status,ierror )

      endif

      call mpi_barrier ( comm,ierror )

      return
      end

C **********************************************************************
      subroutine gather_1d ( qglobal,qlocal,lattice )
C **********************************************************************
      use g3_dynamics_lattice_module
      implicit none
      type   ( dynamics_lattice_type ) lattice
      real(kind=8) qglobal( lattice%imglobal )
      real(kind=8) qlocal ( lattice%im(lattice%myid) )

      include 'mpif.h'
      integer status(mpi_status_size)

      integer comm, mpi_rkind
      integer i,n,loc,im,imx,myid,npes,ierror

      mpi_rkind = lattice%mpi_rkind
      comm      = lattice%comm
      myid      = lattice%myid
      npes      = lattice%nx
      im        = lattice%im(myid)

      if( myid.eq.0 ) then
      do i=1,im
      qglobal(i) = qlocal(i)
      enddo

      loc = im
      do n=1,npes-1
      imx = lattice%im(n)
      call mpi_recv ( qglobal(1+loc),imx,mpi_rkind,n,n,comm,status,ierror )
      loc = loc + imx
      enddo
      else
      call mpi_send ( qlocal,im,mpi_rkind,0,myid,comm,ierror )

      endif

      call mpi_barrier ( comm,ierror )

      return
      end

C **********************************************************************
      subroutine gather_2d ( qglobal,qlocal,lattice )
C **********************************************************************
      use g3_dynamics_lattice_module
      implicit none
      type   ( dynamics_lattice_type ) lattice
      real(kind=8) qglobal( lattice%imglobal,lattice%jmglobal )
      real(kind=8) qlocal ( lattice%im(lattice%pei),lattice%jm(lattice%pej) )

      include 'mpif.h'
      integer status(mpi_status_size)

      integer comm, mpi_rkind
      integer myid,npe,ierror
      integer nx,i,iloc,im,imx,imglobal,isum
      integer ny,j,jloc,jm,jmy,jmglobal,jsum
      real(kind=8), allocatable ::   buf(:,:)

      mpi_rkind = lattice%mpi_rkind
      comm      = lattice%comm
      myid      = lattice%myid
      iloc      = lattice%pei
      jloc      = lattice%pej
      im        = lattice%im(iloc)
      jm        = lattice%jm(jloc)
      imglobal  = lattice%imglobal
      jmglobal  = lattice%jmglobal

      if( myid.eq.0 ) then
          jsum = 0
          do ny=0,lattice%ny-1
          jmy = lattice%jm(ny)
             isum = 0
             do nx=0,lattice%nx-1
             imx = lattice%im(nx)

             if( nx.eq.0 .and. ny.eq.0 ) then
                 do j=1,jmy
                 do i=1,imx
                 qglobal(i,j) = qlocal(i,j)
                 enddo
                 enddo
                 isum = isum + imx
             else

                 allocate ( buf(imx,jmy) )
                 npe = nx + ny*lattice%nx
                 call mpi_recv ( buf,imx*jmy,mpi_rkind,npe,npe,comm,status,ierror )
                 do j=1,jmy
                 do i=1,imx
                 qglobal(i+isum,j+jsum) = buf(i,j)
                 enddo
                 enddo
                 isum = isum + imx
                 deallocate ( buf )

             endif
             enddo
          jsum = jsum + jmy
          enddo
      else

          call mpi_send ( qlocal,im*jm,mpi_rkind,0,myid,comm,ierror )

      endif

      call mpi_barrier ( comm,ierror )

      return
      end

C **********************************************************************
      subroutine gather_3d ( qglobal,qlocal,lattice )
C **********************************************************************
      use g3_dynamics_lattice_module
      implicit none
      type   ( dynamics_lattice_type ) lattice
      real(kind=8) qglobal( lattice%imglobal,lattice%jmglobal,lattice%lm )
      real(kind=8) qlocal ( lattice%im(lattice%pei),lattice%jm(lattice%pej),lattice%lm )

      include 'mpif.h'
      integer status(mpi_status_size)

      integer comm,lm,L
      integer myid,npe,ierror, mpi_rkind
      integer nx,i,iloc,im,imx,imglobal,isum
      integer ny,j,jloc,jm,jmy,jmglobal,jsum
      real(kind=8), allocatable ::   buf(:,:,:)

      mpi_rkind = lattice%mpi_rkind
      comm      = lattice%comm
      myid      = lattice%myid
      iloc      = lattice%pei
      jloc      = lattice%pej
      im        = lattice%im(iloc)
      jm        = lattice%jm(jloc)
      lm        = lattice%lm
      imglobal  = lattice%imglobal
      jmglobal  = lattice%jmglobal

      if( myid.eq.0 ) then
          jsum = 0
          do ny=0,lattice%ny-1
          jmy = lattice%jm(ny)
             isum = 0
             do nx=0,lattice%nx-1
             imx = lattice%im(nx)

             if( nx.eq.0 .and. ny.eq.0 ) then
                 do L=1,lm
                 do j=1,jmy
                 do i=1,imx
                 qglobal(i,j,L) = qlocal(i,j,L)
                 enddo
                 enddo
                 enddo
                 isum = isum + imx
             else

                 allocate ( buf(imx,jmy,lm) )
                 npe = nx + ny*lattice%nx
                 call mpi_recv ( buf,imx*jmy*lm,mpi_rkind,npe,npe,comm,status,ierror )
                 do L=1,lm
                 do j=1,jmy
                 do i=1,imx
                 qglobal(i+isum,j+jsum,L) = buf(i,j,L)
                 enddo
                 enddo
                 enddo
                 isum = isum + imx
                 deallocate ( buf )

             endif
             enddo
          jsum = jsum + jmy
          enddo
      else

          call mpi_send ( qlocal,im*jm*lm,mpi_rkind,0,myid,comm,ierror )

      endif

      call mpi_barrier ( comm,ierror )

      return
      end

C **********************************************************************
      subroutine ghostx (a,b,im,jm,lm,n,lattice,flag)
C **********************************************************************
C ****                                                              ****
C ****  This program fills GHOST values in the Y-direction          ****
C ****                                                              ****
C ****  a ....... Input  Array  Non-Ghosted                         ****
C ****  b ....... Output Array      Ghosted                         ****
C ****  im ...... Local  Zonal      Dimension                       ****
C ****  jm ...... Local  Meridional Dimension                       ****
C ****  lm ...... Local  Vertical   Dimension                       ****
C ****  n ....... Number of GHOST values                            ****
C ****  lattice . Grid Lattice for MPI                              ****
C ****  flag .... Character*(*) to do 'east' or 'west' only         ****
C ****                                                              ****
C **********************************************************************
      use g3_dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice

      include 'mpif.h'
      integer  stat(mpi_status_size,4)

      integer  myid, nx, comm, ierror, mpi_rkind
      integer  send_reqeast, send_reqwest
      integer  recv_reqeast, recv_reqwest
      character*(*) flag

      integer  im,jm,lm,n,i,j,L
      integer  east,west
      real(kind=8) getcon,undef

      real(kind=8) a(1  :im  ,1:jm,lm)
      real(kind=8) b(1-n:im+n,1:jm,lm)
      real(kind=8) bufs(n,jm,lm,2)
      real(kind=8) bufr(n,jm,lm,2)

      mpi_rkind = lattice%mpi_rkind
      comm      = lattice%comm
      myid      = lattice%myid
      nx        = lattice%nx
      undef     = getcon('UNDEF')

      if( n.gt.im ) then
      print *
      print *, 'Processor ',myid,' does not contain enough grid-points in X (',im,') to perform ',n,' point ghosting!'
      call my_finalize
      call my_exit (101)
      endif

      east = mod( myid+nx+1,nx ) + (myid/nx)*nx
      west = mod( myid+nx-1,nx ) + (myid/nx)*nx

      do L=1,lm
      do j=1,jm
         do i=1,im
         b(i,j,L) = a(i,j,L)
         enddo
         do i=1,n
         bufs(i,j,L,1) = a(i,j,L)
         bufs(i,j,L,2) = a(im-n+i,j,L)
         bufr(i,j,L,1) = undef
         bufr(i,j,L,2) = undef
         b( 1-i,j,L)   = undef ! Initialize ghost regions
         b(im+i,j,L)   = undef ! Initialize ghost regions
         enddo
      enddo
      enddo

        if( nx.gt.1 ) then

          if( east.eq.west ) then
              call mpi_sendrecv( bufs,2*n*jm*lm,mpi_rkind,east,east,
     .                           bufr,2*n*jm*lm,mpi_rkind,west,myid,comm,stat,ierror )
          else

            if( flag.ne.'east' ) then
                                 call mpi_isend ( bufs(1,1,1,2),n*jm*lm,mpi_rkind,east,east,comm,send_reqeast,ierror )
                                 call mpi_irecv ( bufr(1,1,1,2),n*jm*lm,mpi_rkind,west,myid,comm,recv_reqwest,ierror )
            endif
            if( flag.ne.'west' ) then
                                 call mpi_isend ( bufs(1,1,1,1),n*jm*lm,mpi_rkind,west,west,comm,send_reqwest,ierror )
                                 call mpi_irecv ( bufr(1,1,1,1),n*jm*lm,mpi_rkind,east,myid,comm,recv_reqeast,ierror )
            endif
            if( flag.ne.'east' ) then
                                 call mpi_wait  ( send_reqeast,stat(1,1),ierror )
                                 call mpi_wait  ( recv_reqwest,stat(1,2),ierror )
            endif
            if( flag.ne.'west' ) then
                                 call mpi_wait  ( send_reqwest,stat(1,3),ierror )
                                 call mpi_wait  ( recv_reqeast,stat(1,4),ierror )
            endif

          endif

        else
          do L=1,lm
          do j=1,jm
          do i=1,n
          bufr(i,j,L,1) = bufs(i,j,L,1)
          bufr(i,j,L,2) = bufs(i,j,L,2)
          enddo
          enddo
          enddo
        endif

      do L=1,lm
      do j=1,jm
         do i=-n+1,0
         b(i,j,L) = bufr(i+n,j,L,2)
         enddo
         do i=im+1,im+n
         b(i,j,L) = bufr(i-im,j,L,1)
         enddo
      enddo
      enddo

      return
      end

C **********************************************************************
      subroutine ghosty (a,b,im,jm,lm,shift,msgn,n,lattice,flag)
C **********************************************************************
C ****                                                              ****
C ****  This program fills GHOST values in the Y-direction          ****
C ****                                                              ****
C ****  a ....... Input  Array  Non-Ghosted                         ****
C ****  b ....... Output Array      Ghosted                         ****
C ****  im ...... Local  Zonal      Dimension                       ****
C ****  jm ...... Local  Meridional Dimension                       ****
C ****  lm ...... Local  Vertical   Dimension                       ****
C ****  shift ... Integer Flag: 0 for A-Grid, 1 for C-Grid VWND     ****
C ****  msgn .... Integer Flag for Scaler (1) or Vector (-1)        ****
C ****  n ....... Number of GHOST values                            ****
C ****  lattice . Grid Lattice for MPI                              ****
C ****  flag .... Character*(*) to do 'north', 'south', or 'pole'   ****
C ****                                                              ****
C **********************************************************************
      use g3_dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice

      include 'mpif.h'
      integer  status(mpi_status_size)

      integer  myid, nx, comm, ierror, mpi_rkind
      character*(*) flag

      integer  im,jm,lm,shift,m,n,i,j,L,i0,req(im)
      integer  north,south,imx,ibeg,npes,request,msgn
      real(kind=8) getcon,undef

      real(kind=8) a(1:im,  1:jm  ,lm)
      real(kind=8) b(1:im,1-n:jm+n,lm)

      real(kind=8), allocatable :: apls(:,:,:)
      real(kind=8), allocatable :: amns(:,:,:)
      real(kind=8), allocatable ::  buf(:,:)

      mpi_rkind = lattice%mpi_rkind
      comm      = lattice%comm
      myid      = lattice%myid
      nx        = lattice%nx
      undef     = getcon('UNDEF')

      if( (lattice%pej.eq.0            .and. n.gt.jm-1) .or.  ! Pole values cannot be used for ghosting
     .    (lattice%pej.eq.lattice%ny-1 .and. n.gt.jm-1) .or.  ! Pole values cannot be used for ghosting
     .                                      (n.gt.jm) ) then
      print *
      print *, 'Processor ',myid,' does not contain enough grid-points in Y (',jm,') to perform ',n,' point ghosting!'
      call my_finalize
      call my_exit (101)
      endif

      do L=1,lm
      do j=1,jm
      do i=1,im
      b(i,j,L) = a(i,j,L)
      enddo
      enddo
      b(:, 1-n:0   ,L) = undef  ! Initialize ghost regions
      b(:,jm+1:jm+n,L) = undef  ! Initialize ghost regions
      enddo

c Ghost South Pole
c ----------------
      if( lattice%pej.eq.0 .and. flag.ne.'north' ) then
          do L=1,lm
          do j=1,n
          do i=1,im
          b(i,1-j,L) = a(i,1+j-shift,L)
          enddo
          enddo
          enddo
      endif

c Ghost North Pole
c ----------------
      if( lattice%pej.eq.lattice%ny-1 .and. flag.ne.'south' ) then
          do L=1,lm
          do j=1,n
          do i=1,im
          b(i,jm+j-shift,L) = a(i,jm-j,L)
          enddo
          enddo
          enddo
      endif

c Ghost Non-Pole Points North
c ---------------------------
      if( flag.ne.'south' .and. flag.ne.'pole' ) then
          if( lattice%pej.eq.lattice%ny-1 .and. lattice%pej.ne.0 ) then
              south = myid - nx
              allocate (  buf(im*n*lm,1) )
                 do L=1,lm
                 do j=1,n
                 do i=1,im
                  buf(i+(j-1)*im+(L-1)*im*n,1) = a(i,j,L)
                 enddo
                 enddo
                 enddo

              call mpi_isend( buf,n*im*lm,mpi_rkind,south,south,comm,request,ierror )
              call mpi_wait ( request,status,ierror )

              deallocate ( buf )
           endif
           if( lattice%pej.eq.0 .and. lattice%pej.ne.lattice%ny-1 ) then
              north = myid + nx
              allocate ( apls(im,n,lm) )

              call mpi_recv ( apls,n*im*lm,mpi_rkind,north,myid,comm,status,ierror )

                 do L=1,lm
                 do j=1,n
                 do i=1,im
                 b(i,jm+j,L) = apls(i,j,L)
                 enddo
                 enddo
                 enddo
              deallocate ( apls )
           endif
           if( lattice%pej.ne.0 .and. lattice%pej.ne.lattice%ny-1 ) then
              north = myid + nx
              south = myid - nx
              allocate ( apls(im,n,lm) )
              allocate (  buf(im*n*lm,1) )
                 do L=1,lm
                 do j=1,n
                 do i=1,im
                  buf(i+(j-1)*im+(L-1)*im*n,1) = a(i,j,L)
                 enddo
                 enddo
                 enddo

              call mpi_isend( buf,n*im*lm,mpi_rkind,south,south,comm,request,ierror )
              call mpi_recv ( apls,n*im*lm,mpi_rkind,north,myid,comm,status,ierror )
              call mpi_wait ( request,status,ierror )

                 do L=1,lm
                 do j=1,n
                 do i=1,im
                 b(i,jm+j,L) = apls(i,j,L)
                 enddo
                 enddo
                 enddo
              deallocate ( apls )
              deallocate ( buf )
           endif
      endif

c Ghost Non-Pole Points South
c ---------------------------
      if( flag.ne.'north' .and. flag.ne.'pole' ) then
          if( lattice%pej.eq.0 .and. lattice%pej.ne.lattice%ny-1 ) then
              north = myid + nx
              allocate (  buf(im*n*lm,1) )
                 do L=1,lm
                 do j=1,n
                 do i=1,im
                  buf(i+(j-1)*im+(L-1)*im*n,1) = a(i,jm-n+j,L)
                 enddo
                 enddo
                 enddo

              call mpi_isend( buf,n*im*lm,mpi_rkind,north,north,comm,request,ierror )
              call mpi_wait ( request,status,ierror )

              deallocate ( buf )
           endif
           if( lattice%pej.eq.lattice%ny-1 .and. lattice%pej.ne.0 ) then
              south = myid - nx
              allocate ( amns(im,n,lm) )

              call mpi_recv ( amns,n*im*lm,mpi_rkind,south,myid,comm,status,ierror )

                 do L=1,lm
                 do j=1,n
                 do i=1,im
                 b(i,j-n,L) = amns(i,j,L)
                 enddo
                 enddo
                 enddo
              deallocate ( amns )
           endif
           if( lattice%pej.ne.0 .and. lattice%pej.ne.lattice%ny-1 ) then
              north = myid + nx
              south = myid - nx
              allocate ( amns(im,n,lm) )
              allocate (  buf(im*n*lm,1) )
                 do L=1,lm
                 do j=1,n
                 do i=1,im
                  buf(i+(j-1)*im+(L-1)*im*n,1) = a(i,jm-n+j,L)
                 enddo
                 enddo
                 enddo

              call mpi_isend( buf,n*im*lm,mpi_rkind,north,north,comm,request,ierror )
              call mpi_recv ( amns,n*im*lm,mpi_rkind,south,myid,comm,status,ierror )
              call mpi_wait ( request,status,ierror )

                 do L=1,lm
                 do j=1,n
                 do i=1,im
                 b(i,j-n,L) = amns(i,j,L)
                 enddo
                 enddo
                 enddo
              deallocate ( amns )
              deallocate ( buf )
           endif
      endif

      return
      end

      subroutine par_dot ( q1,q2,qdot,im,jm,lattice )
C***********************************************************************
C  PURPOSE                                                              
C     Compute dot product for MPI grid
C
C   q1 .... Input First  Vector
C   q2 .... Input Second Vector
C   qdot .. Output Dot Product
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

      use g3_dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      integer         im,jm
      real(kind=8)     q1(im,jm),q2(im,jm)
      real(kind=8)   qdot(jm)
      real(kind=8)   q12(lattice%imglobal,jm)
      real(kind=8), allocatable :: buf(:,:)

      include 'mpif.h'
      integer status(mpi_status_size)

      real(kind=8) sum
      integer i,j,n,i0,peid,peid0,ierror, mpi_rkind

      mpi_rkind = lattice%mpi_rkind

c Compute Local Product
c ---------------------
      do j=1,jm
      do i=1,im
      q12(i,j) = q1(i,j)*q2(i,j)
      enddo
      enddo

c Get Data from other PEs to ensure reproducibility
c -------------------------------------------------

      peid0 = lattice%pej*lattice%nx
      if( lattice%pei.eq.0 ) then
          i0 = im
          do n=1,lattice%nx-1
          peid = n + lattice%pej*lattice%nx
          allocate ( buf( lattice%im(n),jm ) )
          call mpi_recv ( buf,lattice%im(n)*jm,mpi_rkind,peid,peid,lattice%comm,status,ierror )
          do j=1,jm
          do i=1,lattice%im(n)
          q12(i+i0,j) = buf(i,j)
          enddo
          enddo
          deallocate ( buf )
          i0 = i0 + lattice%im(n)
          enddo
      else
          call mpi_send ( q12,im*jm,mpi_rkind,peid0,lattice%myid,lattice%comm,ierror )
      endif

c Begin Dot Product Calculation
c -----------------------------
      if( lattice%pei.eq.0 ) then

      do j=1,jm
      sum = q12(1,j)
      do i=2,lattice%imglobal
      sum = sum + q12(i,j)
      enddo
      qdot(j) = sum
      enddo

c Send Dot Product to other PEs
c -----------------------------

          do n=1,lattice%nx-1
          peid = n + lattice%pej*lattice%nx
          call mpi_send ( qdot,jm,mpi_rkind,peid,peid0,lattice%comm,ierror )
          enddo
      else
          call mpi_recv ( qdot,jm,mpi_rkind,peid0,peid0,lattice%comm,status,ierror )

      endif  ! End PEI   Check

      return
      end

      subroutine par_sum ( q,qsum,im,jm,lattice )
C***********************************************************************
C  PURPOSE                                                              
C     Compute sum for MPI grid
C
C   q ..... Input  Vector
C   qsum .. Output Sum
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

      use g3_dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      integer         im,jm
      real(kind=8)      q(im,jm)
      real(kind=8)   qsum(jm)
      real(kind=8)   qg(lattice%imglobal,jm)
      real(kind=8), allocatable :: buf(:,:)

      include 'mpif.h'
      integer status(mpi_status_size)

      real(kind=8) sum
      integer i,j,n,i0,peid,peid0,ierror, mpi_rkind

      mpi_rkind = lattice%mpi_rkind

c Get Data from other PEs to ensure reproducibility
c -------------------------------------------------
      peid0 = lattice%pej*lattice%nx
      if( lattice%pei.eq.0 ) then
          do j=1,jm
          do i=1,im
          qg(i,j) = q(i,j)
          enddo
          enddo

          i0 = im
          do n=1,lattice%nx-1
          peid = n + lattice%pej*lattice%nx
          allocate ( buf( lattice%im(n),jm ) )
          call mpi_recv ( buf,lattice%im(n)*jm,mpi_rkind,peid,peid,lattice%comm,status,ierror )
          do j=1,jm
          do i=1,lattice%im(n)
          qg(i+i0,j) = buf(i,j)
          enddo
          enddo
          deallocate ( buf )
          i0 = i0 + lattice%im(n)
          enddo
      else
          call mpi_send ( q,im*jm,mpi_rkind,peid0,lattice%myid,lattice%comm,ierror )
      endif

c Begin Sum Calculation
c ---------------------
      if( lattice%pei.eq.0 ) then

      do j=1,jm
      sum = qg(1,j)
      do i=2,lattice%imglobal
      sum = sum + qg(i,j)
      enddo
      qsum(j) = sum
      enddo

c Send Dot Product to other PEs
c -----------------------------
          do n=1,lattice%nx-1
          peid = n + lattice%pej*lattice%nx
          call mpi_send ( qsum,jm,mpi_rkind,peid,peid0,lattice%comm,ierror )
          enddo
      else
          call mpi_recv ( qsum,jm,mpi_rkind,peid0,peid0,lattice%comm,status,ierror )

      endif  ! End PEI   Check

      return
      end


      subroutine zmean ( q,qz,dlam,im,jm,m,lcheck,lattice )
C***********************************************************************
C  PURPOSE                                                              
C     Compute zonal mean for generalized MPI grid
C
C  Note:  m=0  Mass   Point
C         m=1  U-Wind Point
C  lcheck      Flag   for UNDEF check
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

      use g3_dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      logical  lcheck
      integer   im,jm,m
      real(kind=8)    q(im,jm)
      real(kind=8)   qz(jm)
      real(kind=8) dlam(lattice%imglobal)
      real(kind=8)   qg(lattice%imglobal,jm)
      real(kind=8), allocatable :: buf(:,:)

      include 'mpif.h'
      integer status(mpi_status_size)

      real(kind=8) sum1,sum2,getcon,undef
      integer i,j,n,i0,peid,peid0,ierror, mpi_rkind

      mpi_rkind = lattice%mpi_rkind

c Get Data from other PEs to ensure reproducibility
c -------------------------------------------------
      peid0 = lattice%pej*lattice%nx
      if( lattice%pei.eq.0 ) then
          do j=1,jm
          do i=1,im
          qg(i,j) = q(i,j)
          enddo
          enddo

          i0 = im
          do n=1,lattice%nx-1
          peid = n + lattice%pej*lattice%nx
          allocate ( buf( lattice%im(n),jm ) )
          call mpi_recv ( buf,lattice%im(n)*jm,mpi_rkind,peid,peid,lattice%comm,status,ierror )
          do j=1,jm
          do i=1,lattice%im(n)
          qg(i+i0,j) = buf(i,j)
          enddo
          enddo
          deallocate ( buf )
          i0 = i0 + lattice%im(n)
          enddo
      else
          call mpi_send ( q,im*jm,mpi_rkind,peid0,lattice%myid,lattice%comm,ierror )
      endif

c Begin Zonal Mean Calculation
c ----------------------------
      if( lattice%pei.eq.0 ) then
      if( lcheck ) then
      undef = getcon('UNDEF')

c U-Wind Point w/Undef Check
c --------------------------
      if( m.eq.1 ) then
      do j=1,jm
         if( qg(1,j).ne.undef ) then
         sum1 = qg(1,j)*dlam(1)
         sum2 =         dlam(1)
         else
         sum1 = 0.0
         sum2 = 0.0
         endif
      do i=2,lattice%imglobal
         if( qg(i,j).ne.undef ) then
         sum1 = sum1 + qg(i,j)*dlam(i)
         sum2 = sum2 +         dlam(i)
         endif
      enddo
         if( sum2.ne.0.0 ) then
         qz(j) = sum1/sum2
         else
         qz(j) = undef
         endif
      enddo
      endif

c Mass Point w/Undef Check
c ------------------------
      if( m.eq.0 ) then
      do j=1,jm
         if( qg(1,j).ne.undef ) then
         sum1 = qg(1,j)*( dlam(1)+dlam(lattice%imglobal) )*0.5
         sum2 =         ( dlam(1)+dlam(lattice%imglobal) )*0.5
         else
         sum1 = 0.0
         sum2 = 0.0
         endif
      do i=2,lattice%imglobal
         if( qg(i,j).ne.undef ) then
         sum1 = sum1 + qg(i,j)*( dlam(i)+dlam(i-1) )*0.5
         sum2 = sum2 +         ( dlam(i)+dlam(i-1) )*0.5
         endif
      enddo
         if( sum2.ne.0.0 ) then
         qz(j) = sum1/sum2
         else
         qz(j) = undef
         endif
      enddo
      endif

      else

c U-Wind Point w/o Undef Check
c ----------------------------
      if( m.eq.1 ) then
      do j=1,jm
      sum1 = qg(1,j)*dlam(1)
      sum2 =         dlam(1)
      do i=2,lattice%imglobal
      sum1 = sum1 + qg(i,j)*dlam(i)
      sum2 = sum2 +         dlam(i)
      enddo
      qz(j) = sum1/sum2
      enddo
      endif

c Mass Point w/o Undef Check
c --------------------------
      if( m.eq.0 ) then
      do j=1,jm
      sum1 = qg(1,j)*( dlam(1)+dlam(lattice%imglobal) )*0.5
      sum2 =         ( dlam(1)+dlam(lattice%imglobal) )*0.5
      do i=2,lattice%imglobal
      sum1 = sum1 + qg(i,j)*( dlam(i)+dlam(i-1) )*0.5
      sum2 = sum2 +         ( dlam(i)+dlam(i-1) )*0.5
      enddo
      qz(j) = sum1/sum2
      enddo
      endif

      endif  ! End UNDEF Check

c Send Zonal Mean Data to other PEs
c ---------------------------------
          do n=1,lattice%nx-1
          peid = n + lattice%pej*lattice%nx
          call mpi_send ( qz,jm,mpi_rkind,peid,peid0,lattice%comm,ierror )
          enddo
      else
          call mpi_recv ( qz,jm,mpi_rkind,peid0,peid0,lattice%comm,status,ierror )

      endif  ! End PEI   Check

      return
      end

      subroutine my_barrier (comm)
      implicit none
      integer  comm,ierror
      call mpi_barrier (  comm,ierror )
      return
      end

      subroutine my_finalize
      implicit none
      integer  ierror
      call mpi_finalize (ierror )
      return
      end

      subroutine my_exit (irc)
      implicit none
      integer irc
      integer  ierror
      call system ('touch gcm_error')
      call mpi_finalize (ierror)
      call exit (irc)
      return
      end

      subroutine parwritit (qloc,im,jm,lm,ku,lattice)
      use g3_dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      real(kind=8)  qloc(im,jm,lm)
      real(kind=8)  qglo(lattice%imglobal,lattice%jmglobal)
      integer im,jm,lm,ku,L
      do L=lm,1,-1
      call gather_2d ( qglo,qloc(:,:,L),lattice)
      if( lattice%myid.eq.0 ) write(ku) real(qglo,kind=4)
      enddo
      call my_barrier (lattice%comm)
      return
      end

      subroutine printchar (string,lattice)
      use g3_dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      character*(*) string
      integer i
      do i=0,lattice%nx*lattice%ny-1
         if( i.eq.lattice%myid ) print *, 'myid: ',i,string
      call my_barrier (lattice%comm)
      enddo
      call my_barrier (lattice%comm)
      return
      end

      subroutine printint (string,n,lattice)
      use g3_dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      character*(*) string
      integer i,n
      do i=0,lattice%nx*lattice%ny-1
         if( i.eq.lattice%myid ) print *, 'myid: ',i,string,n
      call my_barrier (lattice%comm)
      enddo
      call my_barrier (lattice%comm)
      return
      end

      subroutine printreal (string,q,num,lattice)
      use g3_dynamics_lattice_module
      implicit none
      type ( dynamics_lattice_type ) lattice
      character*(*) string
      integer  i,num,n
      real(kind=8) q(num)
      do i=0,lattice%nx*lattice%ny-1
         if( i.eq.lattice%myid ) print *, 'myid: ',i,string,(q(n),n=1,num)
      call my_barrier (lattice%comm)
      enddo
      call my_barrier (lattice%comm)
      return
      end

      subroutine init_dynamics_grid (grid,imglobal,jmglobal,lm,nt,ak,bk)
      use g3_dynamics_state_module
      type ( dynamics_grid_type ) grid

      integer imglobal,jmglobal,lm,nt
      real(kind=8) ak(lm+1), bk(lm+1)

      integer im,jm,L
      real(kind=8) pi,dl,dp

      im = grid%lattice%im( grid%lattice%pei )
      jm = grid%lattice%jm( grid%lattice%pej )

      pi = 4.0*atan(1.0)
      dl = 2.0*pi/ imglobal
      dp =     pi/(jmglobal-1)

      grid%n  = 1
      grid%im = im
      grid%jm = jm
      grid%lm = lm
      grid%ptracer = nt
      grid%ntracer = nt

      grid%lam_np =  0.0
      grid%phi_np = 90.0
      grid%lam_0  =  0.0

      grid%dl      = dl
      grid%dp      = dp
      grid%dlam(:) = dl
      grid%dphi(:) = dp

      grid%ptop    = ak(1)
      grid%alf (:) = ak(:)
      grid%bet (:) = bk(:)
      grid%sige(:) = grid%bet(:)
      do   L=1,lm
      grid%dsig(L) =  grid%sige(L+1) - grid%sige(L)
      grid% sig(L) = (grid%sige(L+1) + grid%sige(L))*0.5
      enddo

      return
      end

      real(kind=8) FUNCTION GETCON (NAME)                                                    
C***********************************************************************        
C                                                                               
C  GENERIC FUNCTION GETCON IS A REPOSITORY OF GLOBAL VARIABLES, I.E.            
C  A MEMORY FOR SCALAR VALUES NEEDED THROUGHOUT A LARGE PROGRAM.                
C                                                                               
C  THIS SPECIFIC FUNCTION, GETCON, REMEMBERS FLOATING POINT VALUES.             
C  THE FUNCTION IS CALLED WITH A CHARACTER NAME TO INTERROGATE A VALUE.         
C                                                                               
C***********************************************************************        
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *        
C***********************************************************************        

      integer, PARAMETER :: MAXCON=40
      CHARACTER*(*) NAME                                                        
      CHARACTER*16 ANAME(MAXCON)                                                
      real(kind=8)     ACON (MAXCON)                                                
      integer i

C COMPUTATIONAL CONSTANTS                                                       
C -----------------------                                                       
      real(kind=8), PARAMETER ::  VECMAX = 65535.5
      real(kind=8), PARAMETER ::  CALTOJ = 4184.
      real(kind=8), PARAMETER ::  UNDEF  = -999

C ASTRONOMICAL CONSTANTS                                                        
C ----------------------                                                        
      real(kind=8), PARAMETER ::  OB     = 23.45
      real(kind=8), PARAMETER ::  PER    = 102.
      real(kind=8), PARAMETER ::  ECC    = .0167
c     real(kind=8), PARAMETER ::  AE     = 6371E3        ! GEOS-3
      real(kind=8), PARAMETER ::  AE     = 6376E3        ! GEOS-5
      real(kind=8), PARAMETER ::  EQNX   = 80.5
      real(kind=8), PARAMETER ::  SOLS   = 176.5
      real(kind=8), PARAMETER ::  S0     = 1365.0

C TERRESTRIAL CONSTANTS                                                         
C ---------------------                                                         
c     real(kind=8), PARAMETER ::  GRAV   = 9.81          ! GEOS-3
c     real(kind=8), PARAMETER ::  GRAV   = 9.80616       ! fv
      real(kind=8), PARAMETER ::  GRAV   = 9.80          ! GEOS-5

      real(kind=8), PARAMETER ::  SRFPRS = 984.7
      real(kind=8), PARAMETER ::  PIMEAN = 984.7 
      real(kind=8), PARAMETER ::  PSTD   = 1000.0
      real(kind=8), PARAMETER ::  TSTD   = 280.0
      real(kind=8), PARAMETER ::  SDAY   = 86400.0
      real(kind=8), PARAMETER ::  SSALB  = 0.99
      real(kind=8), PARAMETER ::  CO2    = 355.0
      real(kind=8), PARAMETER ::  CFC11  = 0.3
      real(kind=8), PARAMETER ::  CFC12  = 0.5
      real(kind=8), PARAMETER ::  CFC22  = 0.2

C THERMODYNAMIC CONSTANTS                                                       
C -----------------------                                                       
      real(kind=8), PARAMETER ::  STFBOL = 5.67E-8
      real(kind=8), PARAMETER ::  AIRMW  = 28.97
      real(kind=8), PARAMETER ::  H2OMW  = 18.01
      real(kind=8), PARAMETER ::  RUNIV  = 8314.3

c     real(kind=8), PARAMETER ::  RGAS   = 287.04        ! fv
      real(kind=8), PARAMETER ::  RGAS   = RUNIV/AIRMW   ! GEOS-3/5

c     real(kind=8), PARAMETER ::  RVAP   = 461.00        ! fv
      real(kind=8), PARAMETER ::  RVAP   = RUNIV/H2OMW   ! GEOS-3/5

c     real(kind=8), PARAMETER ::  RKAP   = RGAS/CPD      ! fv GEOS-3
      real(kind=8), PARAMETER ::  RKAP   = 2.0/7.0       ! GEOS-5

      real(kind=8), PARAMETER ::  HEATW  = 597.2
      real(kind=8), PARAMETER ::  HEATI  = 680.0
      real(kind=8), PARAMETER ::  TICE   = 273.16

c     real(kind=8), PARAMETER ::  CPD    = 1004.16       ! GEOS-3
c     real(kind=8), PARAMETER ::  CPD    = 1004.64       ! fv
      real(kind=8), PARAMETER ::  CPD    = RGAS/RKAP     ! GEOS-5

      real(kind=8), PARAMETER ::  CPV    = 1869.46
      real(kind=8), PARAMETER ::  ALHL   = 2.499E6
      real(kind=8), PARAMETER ::  ALHS   = 2.845E6

C TURBULENCE CONSTANTS                                                          
C --------------------                                                          
      real(kind=8), PARAMETER ::  VKRM   = 0.4

C MOISTURE CONSTANTS                                                            
C ------------------                                                            
      real(kind=8), PARAMETER ::  EPS    = 0.622
      real(kind=8), PARAMETER ::  VIRTCON= 0.609
      real(kind=8), PARAMETER ::  EPSFAC = EPS*HEATW/RGAS*CALTOJ

       DATA ANAME(1 ),ACON(1 ) / 'CP              ', CPD    /                   
       DATA ANAME(2 ),ACON(2 ) / 'RGAS            ', RGAS   /                   
       DATA ANAME(3 ),ACON(3 ) / 'KAPPA           ', RKAP   /                   
       DATA ANAME(4 ),ACON(4 ) / 'LATENT HEAT COND', ALHL   /                   
       DATA ANAME(5 ),ACON(5 ) / 'GRAVITY         ', GRAV   /                   
       DATA ANAME(6 ),ACON(6 ) / 'STEFAN-BOLTZMAN ', STFBOL /                   
       DATA ANAME(7 ),ACON(7 ) / 'VON KARMAN      ', VKRM   /                   
       DATA ANAME(8 ),ACON(8 ) / 'EARTH RADIUS    ', AE     /                   
       DATA ANAME(9 ),ACON(9 ) / 'OBLIQUITY       ', OB     /                   
       DATA ANAME(10),ACON(10) / 'ECCENTRICITY    ', ECC    /                   
       DATA ANAME(11),ACON(11) / 'PERIHELION      ', PER    /                   
       DATA ANAME(12),ACON(12) / 'VERNAL EQUINOX  ', EQNX   /                   
       DATA ANAME(13),ACON(13) / 'SUMMER SOLSTICE ', SOLS   /                   
       DATA ANAME(14),ACON(14) / 'MAX VECT LENGTH ', VECMAX /                   
       DATA ANAME(15),ACON(15) / 'MOL WT H2O      ', H2OMW  /                   
       DATA ANAME(16),ACON(16) / 'MOL WT AIR      ', AIRMW  /                   
       DATA ANAME(17),ACON(17) / 'CPV             ', CPV    /                   
       DATA ANAME(18),ACON(18) / 'CPD             ', CPD    /                   
       DATA ANAME(19),ACON(19) / 'UNIV GAS CONST  ', RUNIV  /                   
       DATA ANAME(20),ACON(20) / 'LATENT HEAT SBLM', ALHS   /                   
       DATA ANAME(21),ACON(21) / 'FREEZING-POINT  ', TICE   /                   
       DATA ANAME(23),ACON(23) / 'CALTOJ          ', CALTOJ /                   
       DATA ANAME(24),ACON(24) / 'EPS             ', EPS    /                   
       DATA ANAME(25),ACON(25) / 'HEATW           ', HEATW  /                   
       DATA ANAME(26),ACON(26) / 'EPSFAC          ', EPSFAC /                   
       DATA ANAME(27),ACON(27) / 'VIRTCON         ', VIRTCON/                   
       DATA ANAME(28),ACON(28) / 'PIMEAN          ', PIMEAN /                   
       DATA ANAME(29),ACON(29) / 'SDAY            ', SDAY   /                   
       DATA ANAME(30),ACON(30) / 'HEATI           ', HEATI  /                   
       DATA ANAME(31),ACON(31) / 'S0              ', S0     /                   
       DATA ANAME(32),ACON(32) / 'PSTD            ', PSTD   /                   
       DATA ANAME(33),ACON(33) / 'TSTD            ', TSTD   /                   
       DATA ANAME(34),ACON(34) / 'SSALB           ', SSALB  /                   
       DATA ANAME(35),ACON(35) / 'UNDEF           ', UNDEF  /                   
       DATA ANAME(36),ACON(36) / 'CO2             ', CO2    /                   
       DATA ANAME(37),ACON(37) / 'RVAP            ', RVAP   /                   
       DATA ANAME(38),ACON(38) / 'CFC11           ', CFC11  /                   
       DATA ANAME(39),ACON(39) / 'CFC12           ', CFC12  /                   
       DATA ANAME(40),ACON(40) / 'CFC22           ', CFC22  /                   

      DO 10 I=1,MAXCON                                                          
      IF(NAME.EQ.ANAME(I)) THEN                                                 
         GETCON = ACON(I)                                                       
         RETURN                                                                 
      ENDIF                                                                     
10    CONTINUE                                                                  

900   PRINT *,' CANNOT FIND FLOATING POINT CONSTANT - ',NAME                    
      PRINT *,' GETCON - CANNOT FIND CONSTANT REQUESTED'                        
      end FUNCTION GETCON
