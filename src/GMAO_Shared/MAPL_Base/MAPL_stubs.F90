subroutine cube2latlon(npx, npy, nlon, nlat, data_cs, data_ll)

  implicit none
  
  integer, intent(in) :: npx, npy, nlon, nlat
  real, dimension(npx , npy ), intent(in ) :: data_cs
  real, dimension(nlon, nlat), intent(out) :: data_ll
  
end subroutine cube2latlon

subroutine latlon2cube(npx, npy, nlon, nlat, data_ll, data_cs)

 implicit none

 integer, intent(in) :: npx, npy, nlon, nlat
 real, dimension(npx , npy ), intent(out) :: data_cs
 real, dimension(nlon, nlat), intent(in ) :: data_ll

end subroutine latlon2cube

subroutine GetWeightsC2C(npx, npy, npxout, npyout, index, weight, &
         ee1, ee2, ff1, ff2)
  implicit none
  integer,  intent(in   ) :: npx,  npy
  integer,  intent(in   ) :: npxout, npyout
  integer,  intent(  out) :: index(:,:,:,:)
  real(8),  intent(  out) :: weight(:,:,:,:)
  real(8),  intent(  out) :: ee1(:,:,:)
  real(8),  intent(  out) :: ee2(:,:,:)
  real(8),  intent(  out) :: ff1(:,:,:)
  real(8),  intent(  out) :: ff2(:,:,:)
end subroutine GetWeightsC2C

!!!!!!!!!!!!!!!%%%%%%%%%%%%%%%%%%%%%%%!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function AppGridCreateF(IM_WORLD, JM_WORLD, LM, NX, NY, rc) result(grid)
#include "MAPL_Generic.h"

  use ESMF
  use MAPL_BaseMod

  implicit none
  
! !ARGUMENTS:
  integer,           intent(IN)    :: IM_WORLD, JM_WORLD, LM
  integer,           intent(IN)    :: NX, NY
  integer, optional, intent(OUT)   :: rc
  type (ESMF_Grid)                 :: grid
  
! ErrLog variables
!-----------------

  integer                      :: STATUS
  character(len=ESMF_MAXSTR), parameter :: Iam="AppGridCreateF"

! Local variables
!-----------------

  RETURN_(STATUS)
end function AppGridCreateF

subroutine AppGridCreate (META, GRID, RC)

#include "MAPL_Generic.h"

  use ESMF
  use MAPL_Mod

  implicit none

! !ARGUMENTS:

  type(MAPL_MetaComp), intent(INOUT) :: META
  type (ESMF_Grid),    intent(  OUT) :: grid
  integer, optional,   intent(  OUT) :: rc

! ErrLog variables
!-----------------

  integer                      :: STATUS
  character(len=ESMF_MAXSTR), parameter :: Iam="AppGridCreate"

! Local variables
!-----------------
  RETURN_(ESMF_SUCCESS)

end subroutine AppGridCreate

#define R8  8
subroutine GetWeights_init (in_ntiles,in_ncnst,in_npx,in_npy,in_npz,&
                              in_nx,in_ny,in_hydro,in_mknh,comm)
  implicit none
  integer,intent(in) :: in_ntiles,in_ncnst
  integer,intent(in) :: in_npx,in_npy,in_npz
  integer,intent(in) :: in_nx,in_ny
  logical,intent(in) :: in_hydro,in_mknh
  integer            :: comm
end subroutine GetWeights_init
subroutine GetWeights(npx, npy, nlat, nlon, &
    index, weight, id1, id2, jdc, l2c,     &
    ee1, ee2, ff1, ff2, gg1, gg2,          &
     e1,  e2,  f1,  f2,  g1,  g2,          &
    sublons, sublats, AmNodeRoot, WriteNetcdf) 
  integer,  intent(in   ) :: npx,  npy
  integer,  intent(in   ) :: nlon, nlat
  integer,  intent(  out) :: index(3,nlon,nlat)
  real(R8), intent(  out) :: weight(4,nlon,nlat)
  integer,  intent(  out) :: id1(npx,npy)
  integer,  intent(  out) :: id2(npx,npy)
  integer,  intent(  out) :: jdc(npx,npy)
  real(R8), intent(  out) :: l2c(4,npx,npy)
  real(R8), intent(  out) :: ee1(npx,npy,3)
  real(R8), intent(  out) :: ee2(npx,npy,3)
  real(R8), intent(  out) :: ff1(npx,npy,3)
  real(R8), intent(  out) :: ff2(npx,npy,3)
  real(R8), intent(  out) :: gg1(npx,npy,3)
  real(R8), intent(  out) :: gg2(npx,npy,3)
  real(R8), pointer       ::  e1(:,:,:)
  real(R8), pointer       ::  e2(:,:,:)
  real(R8), pointer       ::  f1(:,:,:)
  real(R8), pointer       ::  f2(:,:,:)
  real(R8), pointer       ::  g1(:,:,:)
  real(R8), pointer       ::  g2(:,:,:)
  real(R8), optional      :: sublons(:)
  real(R8), optional      :: sublats(:)
  logical , optional      :: AmNodeRoot
  logical , optional      :: WriteNetcdf
end subroutine GetWeights

subroutine A2D2C(U, V, lm, getC)
   real,    intent(INOUT)           :: U(:,:,:)
   real,    intent(INOUT)           :: V(:,:,:)
   integer, intent(   IN)           :: lm
   logical, intent(   IN), optional :: getC
end subroutine A2D2C

subroutine AppCSEdgeCreateF(IM_WORLD, LonEdge, LatEdge, LonCenter, LatCenter, rc)
#include "MAPL_Generic.h"

  use ESMF
  use MAPL_BaseMod

  implicit none

! !ARGUMENTS:
    integer,           intent(IN)     :: IM_WORLD
    integer, optional, intent(OUT)    :: rc
    real(ESMF_KIND_R8), intent(inout) :: LonEdge(IM_World+1,IM_World+1,6)
    real(ESMF_KIND_R8), intent(inout) :: LatEdge(IM_World+1,IM_World+1,6)
    real(ESMF_KIND_R8), optional, intent(inout) :: LonCenter(IM_World,IM_World)
    real(ESMF_KIND_R8), optional, intent(inout) :: LatCenter(IM_World,IM_World)

! ErrLog variables
!-----------------

 integer                      :: STATUS
 character(len=ESMF_MAXSTR), parameter :: Iam="AppCSEdgeCreateF"

  RETURN_(STATUS)
end subroutine AppCSEdgeCreateF

subroutine CubeHaloInit(comm, im_world, npes, nx, ny, domainIdx)
  integer :: comm, im_world, npes, nx, ny
  integer :: domainIdx
end subroutine CubeHaloInit

subroutine CubeHalo(domainIdx, input)
  integer :: domainIdx
  real *4 :: input(:,:)
end subroutine CubeHalo
