!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 module ice_domain

!BOP
! !MODULE: ice_domain
!
! !DESCRIPTION:
!  This module contains the model domain and routines for initializing
!  the domain.  It also initializes the decompositions and
!  distributions across processors/threads by calling relevant
!  routines in the block, distribution modules.
!
! !REVISION HISTORY:
!  SVN:$Id: ice_domain.F90,v 1.2 2009/09/24 21:54:53 bzhao Exp $
!
! author: Phil Jones, LANL
! Oct. 2004: Adapted from POP by William H. Lipscomb, LANL
! Feb. 2007: E. Hunke removed NE and SW boundary options (they were buggy
!  and not used anyhow).
!
! !USES:
!
   use ice_kinds_mod
   use ice_constants
   use ice_communicate
   use ice_broadcast
   use ice_blocks
   use ice_distribution
   use ice_exit
   use ice_fileunits
   use ice_boundary
   use ice_domain_size

   implicit none
   private
   save

! !PUBLIC MEMBER FUNCTIONS

   public  :: init_domain_blocks ,&
              init_domain_distribution

! !PUBLIC DATA MEMBERS:

   integer (int_kind), public :: &
      nblocks            ! actual number of blocks on this processor

   integer (int_kind), dimension(:), pointer, public :: &
      blocks_ice         ! block ids for local blocks

   type (distrb), public :: &
      distrb_info        ! block distribution info

   type (ice_halo), public :: &
      halo_info          !  ghost cell update info

   logical (log_kind), public :: &
      ltripole_grid      ! flag to signal use of tripole grid

    character (char_len), public :: &
       ew_boundary_type,    &! type of domain bndy in each logical
       ns_boundary_type      !    direction (ew is i, ns is j)

!EOP
!BOC
!-----------------------------------------------------------------------
!
!   module private variables - for the most part these appear as
!   module variables to facilitate sharing info between init_domain1
!   and init_domain2.
!
!-----------------------------------------------------------------------

    character (char_len) :: &
       distribution_type,   &! method to use for distributing blocks
                             ! 'cartesian'
                             ! 'rake' 
       distribution_wght     ! method for weighting work per block 
                             ! 'block' = POP default configuration
                             ! 'latitude' = no. ocean points * |lat|

    integer (int_kind) :: &
       nprocs                ! num of processors

!EOC
!***********************************************************************

 contains

!***********************************************************************
!BOP
! !IROUTINE: init_domain_blocks
! !INTERFACE:
#ifdef GEOS
 subroutine init_domain_blocks(m_nprocs, m_processor_shape, m_distribution_type, &
                        m_distribution_wght, m_ew_boundary_type, m_ns_boundary_type)
#else
 subroutine init_domain_blocks
#endif


! !DESCRIPTION:
!  This routine reads in domain information and calls the routine
!  to set up the block decomposition.
!
! !REVISION HISTORY:
!  same as module

! !USES:
!
   use ice_global_reductions
!
#ifdef GEOS
   integer (int_kind),   intent(in) :: &
       m_nprocs
   character (char_len), intent(in) :: &
       m_ew_boundary_type,    &
       m_ns_boundary_type,    &      
       m_processor_shape
   character (char_len), intent(in) :: &
       m_distribution_type,   &
       m_distribution_wght  
#endif
!EOP
!BOC
!----------------------------------------------------------------------
!
!  local variables
!
!----------------------------------------------------------------------

   integer (int_kind) :: &
      nml_error          ! namelist read error flag

!----------------------------------------------------------------------
!
!  input namelists
!
!----------------------------------------------------------------------

   namelist /domain_nml/ nprocs, &
                         processor_shape,   &
                         distribution_type, &
                         distribution_wght, &
                         ew_boundary_type,  &
                         ns_boundary_type

!----------------------------------------------------------------------
!
!  read domain information from namelist input
!
!----------------------------------------------------------------------
#ifdef GEOS
   nprocs             = m_nprocs
   processor_shape    = m_processor_shape
   distribution_type  = m_distribution_type
   distribution_wght  = m_distribution_wght
   ew_boundary_type   = m_ew_boundary_type
   ns_boundary_type   = m_ns_boundary_type
#else

   nprocs = -1
   processor_shape   = 'slenderX2'
   distribution_type = 'cartesian'
   distribution_wght = 'latitude'
   ew_boundary_type  = 'cyclic'
   ns_boundary_type  = 'open'

   call get_fileunit(nu_nml)
   if (my_task == master_task) then
      open (nu_nml, file=nml_filename, status='old',iostat=nml_error)
      if (nml_error /= 0) then
         nml_error = -1
      else
         nml_error =  1
      endif
      do while (nml_error > 0)
         read(nu_nml, nml=domain_nml,iostat=nml_error)
	 if (nml_error > 0) read(nu_nml,*)  ! for Nagware compiler
      end do
      if (nml_error == 0) close(nu_nml)
   endif
   call release_fileunit(nu_nml)

   call broadcast_scalar(nml_error, master_task)
   if (nml_error /= 0) then
      call abort_ice('ice: error reading domain_nml')
   endif

   call broadcast_scalar(nprocs,            master_task)
   call broadcast_scalar(processor_shape,   master_task)
   call broadcast_scalar(distribution_type, master_task)
   call broadcast_scalar(distribution_wght, master_task)
   call broadcast_scalar(ew_boundary_type,  master_task)
   call broadcast_scalar(ns_boundary_type,  master_task)
#endif
!----------------------------------------------------------------------
!
!  perform some basic checks on domain
!
!----------------------------------------------------------------------

   if (trim(ns_boundary_type) == 'tripole') then
      ltripole_grid = .true.
   else
      ltripole_grid = .false.
   endif

   if (nx_global < 1 .or. ny_global < 1 .or. ncat < 1) then
      !***
      !*** domain size zero or negative
      !***
      call abort_ice('ice: Invalid domain: size < 1') ! no domain
   else if (nprocs /= get_num_procs()) then
      !***
      !*** input nprocs does not match system (eg MPI) request
      !***
#if (defined SEQ_MCT)
      nprocs = get_num_procs()
#else
      call abort_ice('ice: Input nprocs not same as system request')
#endif
   else if (nghost < 1) then
      !***
      !*** must have at least 1 layer of ghost cells
      !***
      call abort_ice('ice: Not enough ghost cells allocated')
   endif

!----------------------------------------------------------------------
!  notify global_reductions whether tripole grid is being used
!----------------------------------------------------------------------

   call init_global_reductions (ltripole_grid)

!----------------------------------------------------------------------
!
!  compute block decomposition and details
!
!----------------------------------------------------------------------

   call create_blocks(nx_global, ny_global, trim(ew_boundary_type), &
                                            trim(ns_boundary_type))

!----------------------------------------------------------------------
!
!  Now we need grid info before proceeding further
!  Print some domain information
!
!----------------------------------------------------------------------

   if (my_task == master_task) then
     write(nu_diag,'(/,a18,/)')'Domain Information'
     write(nu_diag,'(a26,i6)') '  Horizontal domain: nx = ',nx_global
     write(nu_diag,'(a26,i6)') '                     ny = ',ny_global
     write(nu_diag,'(a26,i6)') '  No. of categories: nc = ',ncat
     write(nu_diag,'(a26,i6)') '  No. of ice layers: ni = ',nilyr
     write(nu_diag,'(a26,i6)') '  No. of snow layers:ns = ',nslyr
     write(nu_diag,'(a26,i6)') '  Processors:  total    = ',nprocs
     write(nu_diag,'(a25,a10)') '  Processor shape:        ', &
                                  trim(processor_shape)
     write(nu_diag,'(a25,a10)') '  Distribution type:      ', &
                                  trim(distribution_type)
     write(nu_diag,'(a25,a10)') '  Distribution weight:    ', &
                                  trim(distribution_wght)
     write(nu_diag,'(a26,i6)') '  max_blocks =            ', max_blocks
     write(nu_diag,'(a26,i6,/)')'  Number of ghost cells:  ', nghost
   endif

!----------------------------------------------------------------------
!EOC

 end subroutine init_domain_blocks

!***********************************************************************
!BOP
! !IROUTINE: init_domain_distribution
! !INTERFACE:

 subroutine init_domain_distribution(KMTG,ULATG)

! !DESCRIPTION:
!  This routine calls appropriate setup routines to distribute blocks
!  across processors and defines arrays with block ids for any local
!  blocks. Information about ghost cell update routines is also
!  initialized here through calls to the appropriate boundary routines.
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETERS:

   real (dbl_kind), dimension(nx_global,ny_global), intent(in) :: &
      KMTG           ,&! global topography
      ULATG            ! global latitude field (radians)

!EOP
!BOC
!----------------------------------------------------------------------
!
!  local variables
!
!----------------------------------------------------------------------

   integer (int_kind), dimension (nx_global, ny_global) :: &
      flat                 ! latitude-dependent scaling factor

   character (char_len) :: outstring

   integer (int_kind), parameter :: &
      max_work_unit=10    ! quantize the work into values from 1,max

   integer (int_kind) :: &
      i,j,k,n            ,&! dummy loop indices
      ig,jg              ,&! global indices
      work_unit          ,&! size of quantized work unit
      nblocks_tmp        ,&! temporary value of nblocks
      nblocks_max          ! max blocks on proc

   integer (int_kind), dimension(:), allocatable :: &
      nocn               ,&! number of ocean points per block
      work_per_block       ! number of work units per block

   type (block) :: &
      this_block           ! block information for current block

!----------------------------------------------------------------------
!
!  check that there are at least nghost+1 rows or columns of land cells
!  for closed boundary conditions (otherwise grid lengths are zero in
!  cells neighboring ocean points).  
!
!----------------------------------------------------------------------

   if (trim(ns_boundary_type) == 'closed') then
      allocate(nocn(nblocks_tot))
      nocn = 0
      do n=1,nblocks_tot
         this_block = get_block(n,n)
         if (this_block%jblock == nblocks_y) then ! north edge
         do j = this_block%jhi-1, this_block%jhi
            if (this_block%j_glob(j) > 0) then
               do i = 1, nx_block
                  if (this_block%i_glob(i) > 0) then
                     ig = this_block%i_glob(i)
                     jg = this_block%j_glob(j)
                     if (KMTG(ig,jg) > puny) nocn(n) = nocn(n) + 1
                  endif
               enddo
            endif
         enddo
         endif
         if (this_block%jblock == 1) then ! south edge
         do j = this_block%jlo, this_block%jlo+1
            if (this_block%j_glob(j) > 0) then
               do i = 1, nx_block
                  if (this_block%i_glob(i) > 0) then
                     ig = this_block%i_glob(i)
                     jg = this_block%j_glob(j)
                     if (KMTG(ig,jg) > puny) nocn(n) = nocn(n) + 1
                  endif
               enddo
            endif
         enddo
         endif
         if (nocn(n) > 0) then
            print*, 'ice: Not enough land cells along ns edge'
            call abort_ice('ice: Not enough land cells along ns edge')
         endif
      enddo
      deallocate(nocn)
   endif
   if (trim(ew_boundary_type) == 'closed') then
      allocate(nocn(nblocks_tot))
      nocn = 0
      do n=1,nblocks_tot
         this_block = get_block(n,n)
         if (this_block%iblock == nblocks_x) then ! east edge
         do j = 1, ny_block
            if (this_block%j_glob(j) > 0) then
               do i = this_block%ihi-1, this_block%ihi
                  if (this_block%i_glob(i) > 0) then
                     ig = this_block%i_glob(i)
                     jg = this_block%j_glob(j)
                     if (KMTG(ig,jg) > puny) nocn(n) = nocn(n) + 1
                  endif
               enddo
            endif
         enddo
         endif
         if (this_block%iblock == 1) then ! west edge
         do j = 1, ny_block
            if (this_block%j_glob(j) > 0) then
               do i = this_block%ilo, this_block%ilo+1
                  if (this_block%i_glob(i) > 0) then
                     ig = this_block%i_glob(i)
                     jg = this_block%j_glob(j)
                     if (KMTG(ig,jg) > puny) nocn(n) = nocn(n) + 1
                  endif
               enddo
            endif
         enddo
         endif
         if (nocn(n) > 0) then
            print*, 'ice: Not enough land cells along ew edge'
            call abort_ice('ice: Not enough land cells along ew edge')
         endif
      enddo
      deallocate(nocn)
   endif

!----------------------------------------------------------------------
!
!  estimate the amount of work per processor using the topography
!  and latitude
!
!----------------------------------------------------------------------

   if (distribution_wght == 'latitude') then
       flat = NINT(abs(ULATG*rad_to_deg), int_kind) ! linear function
   else
       flat = 1
   endif

   allocate(nocn(nblocks_tot))

   nocn = 0
   do n=1,nblocks_tot
      this_block = get_block(n,n)
      do j=this_block%jlo,this_block%jhi
         if (this_block%j_glob(j) > 0) then
            do i=this_block%ilo,this_block%ihi
               if (this_block%i_glob(i) > 0) then
	          ig = this_block%i_glob(i)
                  jg = this_block%j_glob(j)
                  if (KMTG(ig,jg) > puny .and.                      &
                     (ULATG(ig,jg) < shlat/rad_to_deg .or.          &
                      ULATG(ig,jg) > nhlat/rad_to_deg) )            & 
 	              nocn(n) = nocn(n) + flat(ig,jg)
               endif
            end do
         endif
      end do

      !*** with array syntax, we actually do work on non-ocean
      !*** points, so where the block is not completely land,
      !*** reset nocn to be the full size of the block

      ! use processor_shape = 'square-pop' and distribution_wght = 'block' 
      ! to make CICE and POP decompositions/distributions identical.

      if (distribution_wght == 'block' .and. &   ! POP style
          nocn(n) > 0) nocn(n) = nx_block*ny_block

   end do

   work_unit = maxval(nocn)/max_work_unit + 1

   !*** find number of work units per block

   allocate(work_per_block(nblocks_tot))

   where (nocn > 0)
     work_per_block = nocn/work_unit + 1
   elsewhere
     work_per_block = 0
   end where
   deallocate(nocn)

!----------------------------------------------------------------------
!
!  determine the distribution of blocks across processors
!
!----------------------------------------------------------------------

   distrb_info = create_distribution(distribution_type, &
                                     nprocs, work_per_block)

   deallocate(work_per_block)

!----------------------------------------------------------------------
!
!  allocate and determine block id for any local blocks
!
!----------------------------------------------------------------------

   call create_local_block_ids(blocks_ice, distrb_info)

   if (associated(blocks_ice)) then
      nblocks = size(blocks_ice)
   else
      nblocks = 0
   endif
   nblocks_max = 0
   do n=0,distrb_info%nprocs - 1
     nblocks_tmp = nblocks
     call broadcast_scalar(nblocks_tmp, n)
     nblocks_max = max(nblocks_max,nblocks_tmp)
   end do

   if (nblocks_max > max_blocks) then
     write(outstring,*) &
         'ice: no. blocks exceed max: increase max to', nblocks_max
     call abort_ice(trim(outstring))
   else if (nblocks_max < max_blocks) then
     write(outstring,*) &
         'ice: no. blocks too large: decrease max to', nblocks_max
     if (my_task == master_task) then
        write(nu_diag,*) ' ********WARNING***********'
        write(nu_diag,*) trim(outstring)
        write(nu_diag,*) ' **************************'
        write(nu_diag,*) ' '
     endif
   endif

!----------------------------------------------------------------------
!
!  Set up ghost cell updates for each distribution.
!  Boundary types are cyclic, closed, or tripole. 
!
!----------------------------------------------------------------------

   ! update ghost cells on all four boundaries
   halo_info = ice_HaloCreate(distrb_info,     &
                        trim(ns_boundary_type),     &
                        trim(ew_boundary_type),     &
                        nx_global)

!----------------------------------------------------------------------
!EOC

 end subroutine init_domain_distribution

!***********************************************************************

 end module ice_domain

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
