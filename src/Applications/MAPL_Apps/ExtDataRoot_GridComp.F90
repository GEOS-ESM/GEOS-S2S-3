
!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!
#include "MAPL_Generic.h"

MODULE ExtDataRoot_GridCompMod
      use ESMF
      use MAPL_Mod
      use, intrinsic :: iso_fortran_env, only: REAL64

      IMPLICIT NONE
      PRIVATE

      PUBLIC SetServices


   contains

      subroutine SetServices ( GC, RC )

! !ARGUMENTS:

         type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
         integer,             intent(  OUT) :: RC  ! return code


         character(len=ESMF_MAXSTR) :: Iam='ss'
         integer :: status

         call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_INITIALIZE,  Initialize_, rc=status)
         VERIFY_(STATUS)
         call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN,   Run_, rc=status)
         VERIFY_(STATUS)

         call MAPL_AddImportSpec(GC,&
               short_name='var_2d', &
               long_name='na' , &
               units = 'na', &
               dims = MAPL_DimsHorzOnly, &
               vlocation = MAPL_VLocationNone, rc=status)
         VERIFY_(STATUS)

         call MAPL_GenericSetServices ( GC, rc=status)
         VERIFY_(STATUS)


      end subroutine SetServices

      SUBROUTINE Initialize_ ( GC, IMPORT, EXPORT, CLOCK, rc )

         implicit NONE

         type(ESMF_Clock),  intent(inout) :: CLOCK     ! The clock

         type(ESMF_GridComp), intent(inout) :: GC      ! Grid Component
         type(ESMF_State), intent(inout) :: IMPORT     ! Import State
         type(ESMF_State), intent(inout) :: EXPORT     ! Export State
         integer, intent(out)            :: rc         ! Error return code:

         character(len=ESMF_MAXSTR) :: Iam='init'
         integer :: status

         call MAPL_GridCreate(GC, rc=status)
         VERIFY_(STATUS)
        
    call MAPL_GenericInitialize ( GC, import, export, clock, rc=status )
    VERIFY_(STATUS) 
         rc = 0

      END SUBROUTINE Initialize_

      SUBROUTINE Run_ ( GC, IMPORT, EXPORT, CLOCK, rc )

         implicit NONE

         type(ESMF_Clock),  intent(inout) :: CLOCK     ! The clock

         type(ESMF_GridComp), intent(inout)  :: GC     ! Grid Component
         type(ESMF_State), intent(inout) :: IMPORT     ! Import State
         type(ESMF_State), intent(inout) :: EXPORT     ! Export State
         integer, intent(out) ::  rc                   ! Error return code:

         integer :: status
         character(len=ESMF_MAXSTR) :: Iam='run'

         rc = 0
      END SUBROUTINE Run_

end module ExtDataRoot_GridCompMod

