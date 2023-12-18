#include "MAPL_Generic.h"
!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  GMI_GridCompMod --- GMI Grid Component Class
!
! Aggregated Grid Component class for the GMI combined stratopshere/troposphere 
! chemistry
!
! !INTERFACE:
!
      MODULE  GMI_GridCompMod

! !USES:

      USE ESMF
      USE MAPL_Mod
      USE Chem_Mod 	     ! Chemistry Base Class
      USE Chem_UtilMod

      USE GmiSAD_GCCMod
      USE GmiChem_GCCMod
      USE GmiDepos_GCCMod
      USE GmiEmiss_GCCMod
      USE GmiThermalRC_GCCMod
      USE GmiForcingBC_GCCMod
      USE GmiPhotolysis_GCCMod

      IMPLICIT NONE
!
! !DEFINED PARAMETERS:
      INTEGER, PARAMETER :: DBL = KIND(0.00D+00)

      PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:

      PUBLIC  GMI_GridComp       ! The GMI object 
      PUBLIC  GMI_GridCompInitialize
      PUBLIC  GMI_GridCompRun
      PUBLIC  GMI_GridCompFinalize
!
! !PUBLIC TYPES:
      TYPE GMI_GridComp
         CHARACTER(LEN=255) :: name = "GMI Stratospheric/Tropospheric Chemistry"

         ! Component derived type declarations
         ! -----------------------------------
         TYPE(GmiDepos_GridComp)      :: gcDepos 
         TYPE(GmiEmiss_GridComp)      :: gcEmiss
         TYPE(GmiSAD_GridComp)        :: gcSAD
         TYPE(GmiThermalRC_GridComp)  :: gcThermalRC
         TYPE(GmiChemistry_GridComp)  :: gcChem
         TYPE(GmiForcingBC_GridComp)  :: gcFBC
         TYPE(GmiPhotolysis_GridComp) :: gcPhot
      END TYPE GMI_GridComp
!
! !DESCRIPTION:
!  This module implements the GMI combined stratopshere/troposphere
!  chemistry. The methods (Initialize, Run and Finalize) of the
!  the following grid component classes are called here:
!  \begin{enumerate}
!  \item Deposition ({\em GmiDepos\_GridCompInitialize}, GmiDepos\_GridCompRun, GmiDepos\_GridCompFinalize)
!  \item Emission ({\em GmiEmiss\_GridCompInitialize}, GmiEmiss\_GridCompRun, GmiEmiss\_GridCompFinalize)
!  \item Surface Area Densities for Aerosols ({\em GmiSAD\_GridCompInitialize}, GmiSAD\_GridCompRun, GmiSAD\_GridCompFinalize)
!  \item Photolysis ({\em GmiPhotolysis\_GridCompInitialize}, GmiPhotolysis\_GridCompRun, GmiPhotolysis\_GridCompFinalize)
!  \item Forcing Boundary Conditions ({\em GmiForcingBC\_GridCompInitialize}, GmiForcingBC\_GridCompRun, GmiForcingBC\_GridCompFinalize)
!  \item Thermal Rate Constants ({\em GmiThermalRC\_GridCompInitialize}, GmiThermalRC\_GridCompRun, GmiThermalRC\_GridCompFinalize)
!  \item Chemistry Solver ({\em GmiChemistry\_GridCompInitialize}, GmiChemistry\_GridCompRun, GmiChemistry\_GridCompFinalize)
!  \end{enumerate}
!
! !REVISION HISTORY:
!
!  16Sep2003 da Silva  First crack.
!  24Jan2O05 Nielsen   Implementation of Code 916 chemistry
!  19Dec2005 d Silva   Minor portability mods.
!  30Oct2007 Nielsen   GMI Combo set up
!  09Sep2010 Kouatchou Added all the individuals compent class methods.
!
!EOP
!-------------------------------------------------------------------------
      CONTAINS
!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  GMI_GridCompInitialize --- Initialize GMI_GridComp
!
! !INTERFACE:
!
   SUBROUTINE GMI_GridCompInitialize(gcGMI, w_c, impChem, expChem, nymd, nhms, cdt, rc)
!
   IMPLICIT none

! !INPUT PARAMETERS:

   TYPE(Chem_Bundle), INTENT(in) :: w_c         ! Chemical tracer fields, delp, +
   INTEGER,	      INTENT(in) :: nymd, nhms  ! Time from AGCM
   REAL, 	      INTENT(in) :: cdt         ! Chemistry time step (secs)

! !INPUT/OUTPUT PARAMETERS:

   TYPE(GMI_GridComp), INTENT(inOut)  :: gcGMI	    ! Grid Component
   TYPE(ESMF_State),   INTENT(inOut)  :: impChem    ! Import State
   TYPE(ESMF_State),   INTENT(inOut)  :: expChem    ! Export State
!
! !OUTPUT PARAMETERS:
   INTEGER, INTENT(out) ::  rc        ! Error return code:
                                      !  0 - all is well
                                      !  1 - 

! !DESCRIPTION: Initializes the GMI Grid Component. It primarily sets
!               the import state.
!
! !DEFINED PARAMETERS:
   INTEGER, PARAMETER :: DBL = KIND(0.00D+00)
   CHARACTER(LEN=*), PARAMETER :: IAm    = 'GMI_GridCompInitialize'
!
! !LOCAL VARIABLES:
   INTEGER :: ios, m, n, STATUS, procID
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!  30Jun2007 Nielsen   GMI Combo set up
!   7Apr2016 Nielsen   Initialize rc
!
!EOP
!-------------------------------------------------------------------------
!BOC
      rc = 0

      CALL GmiEmiss_GridCompInitialize     (gcGMI%gcEmiss,     w_c, impChem, expChem, nymd, nhms, cdt, STATUS)
      VERIFY_(STATUS)

      CALL GmiDepos_GridCompInitialize     (gcGMI%gcDepos,     w_c, impChem, expChem, nymd, nhms, cdt, STATUS)
      VERIFY_(STATUS)

      CALL GmiSAD_GridCompInitialize       (gcGMI%gcSAD,       w_c, impChem, expChem, nymd, nhms, cdt, STATUS)
      VERIFY_(STATUS)

      CALL GmiPhotolysis_GridCompInitialize(gcGMI%gcPhot,      w_c, impChem, expChem, nymd, nhms, cdt, STATUS)
      VERIFY_(STATUS)

      CALL GmiForcingBC_GridCompInitialize (gcGMI%gcFBC,       w_c, impChem, expChem, nymd, nhms, cdt, STATUS)
      VERIFY_(STATUS)

      CALL GmiThermalRC_GridCompInitialize (gcGMI%gcThermalRC, w_c, impChem, expChem, nymd, nhms, cdt, STATUS)
      VERIFY_(STATUS)

      CALL GmiChemistry_GridCompInitialize (gcGMI%gcChem,      w_c, impChem, expChem, nymd, nhms, cdt, STATUS)
      VERIFY_(STATUS)

      RETURN

      END SUBROUTINE GMI_GridCompInitialize
!EOC
!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  GMI_GridCompRun --- The GMI Driver
!
! !INTERFACE:
!
   SUBROUTINE GMI_GridCompRun(gcGMI, w_c, impChem, expChem, nymd, nhms, cdt, rc)

! !USES:

   IMPLICIT none

! !INPUT/OUTPUT PARAMETERS:

   TYPE(GMI_GridComp), INTENT(inOut) :: gcGMI   ! Grid Component
   TYPE(Chem_Bundle),  INTENT(inOut) :: w_c     ! Chemical tracer fields   
   TYPE(ESMF_State),   INTENT(inOut) :: impChem ! Import State
   TYPE(ESMF_State),   INTENT(inOut) :: expChem ! Export State

! !INPUT PARAMETERS:

   INTEGER, INTENT(in) :: nymd, nhms	      ! time
   REAL,    INTENT(in) :: cdt		      ! chemical timestep (secs)

! !OUTPUT PARAMETERS:
   INTEGER, INTENT(out) ::  rc                ! Error return code:
                                              !  0 - all is well
                                              !  1 -

! !DESCRIPTION: This routine implements the GMI Strat/Trop Driver. That 
!               is, adds chemical tendencies to each of the constituents
!
! !IMPLEMENTATION NOTES:
!
!  No pointer is reservered in the export state for deposition of water.
!
! !DEFINED PARAMETERS:
      CHARACTER(LEN=*), PARAMETER :: IAm    = 'GMI_GridCompRun'
!
! !LOCAL VARIABLES:
      INTEGER :: STATUS
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!  30Jun2007 Nielsen   GMI Combo set up
!   7Apr2016 Nielsen   Initialize rc
!
!EOP
!-------------------------------------------------------------------------
!BOC
      rc = 0

      CALL GmiDepos_GridCompRun     (gcGMI%gcDepos,     w_c, impChem, expChem, nymd, nhms, cdt, STATUS)
      VERIFY_(STATUS)

      CALL GmiEmiss_GridCompRun     (gcGMI%gcEmiss,     w_c, impChem, expChem, nymd, nhms, cdt, STATUS)
      VERIFY_(STATUS)

      CALL GmiSAD_GridCompRun       (gcGMI%gcSAD,       w_c, impChem, expChem, nymd, nhms, cdt, STATUS)
      VERIFY_(STATUS)

      CALL GmiPhotolysis_GridCompRun(gcGMI%gcPhot,      w_c, impChem, expChem, nymd, nhms, cdt, STATUS)
      VERIFY_(STATUS)

      CALL GmiForcingBC_GridCompRun (gcGMI%gcFBC,       w_c, impChem, expChem, nymd, nhms, cdt, STATUS)
      VERIFY_(STATUS)

      CALL GmiThermalRC_GridCompRun (gcGMI%gcThermalRC, w_c, impChem, expChem, nymd, nhms, cdt, STATUS)
      VERIFY_(STATUS)

      CALL GmiChemistry_GridCompRun (gcGMI%gcChem,      w_c, impChem, expChem, nymd, nhms, cdt, STATUS)
      VERIFY_(STATUS)

      RETURN

      END SUBROUTINE GMI_GridCompRun
!EOC
!-------------------------------------------------------------------------
!NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GEOS/DAS!
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  GMI_GridCompFinalize
!
! !INTERFACE:
!

   SUBROUTINE GMI_GridCompFinalize(gcGMI, w_c, impChem, expChem, nymd, nhms, cdt, rc)

   IMPLICIT none

! !INPUT/OUTPUT PARAMETERS:

   TYPE(GMI_GridComp), INTENT(inOut) :: gcGMI	! Grid Component
   TYPE(ESMF_State),   INTENT(inOut) :: impChem ! Import State
   TYPE(ESMF_State),   INTENT(inOut) :: expChem ! Import State

! !INPUT PARAMETERS:

   TYPE(Chem_Bundle), INTENT(in)  :: w_c      ! Chemical tracer fields   
   INTEGER, INTENT(in) :: nymd, nhms	      ! time
   REAL,    INTENT(in) :: cdt  	              ! chemical timestep (secs)


! !OUTPUT PARAMETERS:

   INTEGER, INTENT(out) ::  rc                  ! Error return code:
                                                !  0 - all is well
                                                !  1 -
! 
! !DESCRIPTION: 
!  This routine finalizes this Grid Component.
!
! !DEFINED PARAMETERS:
      CHARACTER(LEN=*), PARAMETER :: IAm    = 'GMI_GridCompFinalize'
!
! !LOCAL VARIABLES:
      INTEGER :: STATUS
!
! !REVISION HISTORY:
!
!  18Sep2003 da Silva  First crack.
!  30Jun2007 Nielsen   GMI Combo set up
!   7Apr2016 Nielsen   Initialize rc
!
!EOP
!-------------------------------------------------------------------------
!BOC
      rc = 0

      CALL GmiDepos_GridCompFinalize     (gcGMI%gcDepos,     w_c, impChem, expChem, nymd, nhms, cdt, STATUS)
      VERIFY_(STATUS)

      CALL GmiEmiss_GridCompFinalize     (gcGMI%gcEmiss,     w_c, impChem, expChem, nymd, nhms, cdt, STATUS)
      VERIFY_(STATUS)

      CALL GmiSAD_GridCompFinalize       (gcGMI%gcSAD,       w_c, impChem, expChem, nymd, nhms, cdt, STATUS)
      VERIFY_(STATUS)

      CALL GmiPhotolysis_GridCompFinalize(gcGMI%gcPhot,      w_c, impChem, expChem, nymd, nhms, cdt, STATUS)
      VERIFY_(STATUS)

      CALL GmiForcingBC_GridCompFinalize (gcGMI%gcFBC,       w_c, impChem, expChem, nymd, nhms, cdt, STATUS)
      VERIFY_(STATUS)

      CALL GmiThermalRC_GridCompFinalize (gcGMI%gcThermalRC, w_c, impChem, expChem, nymd, nhms, cdt, STATUS)
      VERIFY_(STATUS)

      CALL GmiChemistry_GridCompFinalize (gcGMI%gcChem,      w_c, impChem, expChem, nymd, nhms, cdt, STATUS)
      VERIFY_(STATUS)

      RETURN

      END SUBROUTINE GMI_GridCompFinalize
!EOC
!------------------------------------------------------------------------------
  
      END MODULE GMI_GridCompMod
