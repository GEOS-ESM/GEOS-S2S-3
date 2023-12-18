!=======================================================================
!
! $Id: setkin_surf_bc.h,v 1.1.1.1.22.1.282.1.8.1 2016/12/21 18:59:05 mmanyin Exp $
!
! CODE DEVELOPER
!   Peter Connell, LLNL
!   connell2@llnl.gov
!
! FILE
!   setkin_surf_bc.h
!
! DESCRIPTION
!   This include file contains information about surface boundary
!   conditions.
!
!  Chemistry input file:    09/2016
!  Reaction dictionary:     GMI_Combo_rxns_124species_SO2_JPL15.db
!  Setkin files generated:  Wed Sep  7 15:23:58 2016
!
!=======================================================================

!     -----------------------
!     Parameter declarations.
!     -----------------------

!     -------------------------------------------------------
!     NUM_SBC : number of surface bounday conditions to reset
!     K_SBC   : max k to which species will be adjusted
!     -------------------------------------------------------

      integer, parameter :: NUM_SBC = 1

      integer, parameter :: K_SBC   = 2

!     ------------------
!     Integer variables.
!     ------------------

      integer, save :: sbc_map(NUM_SBC) = &
     &  (/ 0 /)

!                                  --^--

