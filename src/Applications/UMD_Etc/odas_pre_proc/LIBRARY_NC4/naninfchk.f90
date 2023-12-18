!***********************************************************************
!
!                        naninfchk.f
!
!  	*****************************************************************
! 	* 								*
!	* 	Absoft !orporation 					* 
! 	*	2781 Bond Street					*
!	*	Rochester Hills, MI  48309				*
!	*								*
!	*	This file contains example code for demonstration	*
!	*	purposes only.  Absoft makes no warranty of the		* 
!	*	suitability of this code for any purpose.		*
!	*								*
!	*	In no event shall Absoft be liable for any incidental,	*
!	*	indirect, special, or consequential damages arising	*
!	*	out of the use of this code.				*
!	*								*
!	***************************************************************** 
!
! Routines to test real and double values against NaN and INF
!
!            NANCHK(X) - tests REAL*4 value X against NaN
!
! For little endian machines (Intel x86), compile with
!
!      f77 -c -DBYTE_SWAPPED=1 naninfchk.f
!	or
!      f90 -c -DBYTE_SWAPPED=1 naninfchk.f -YBOZTYPE=INT
!
! For big endian machines (PowerP!), compile with
!
!      f77 -c naninfchk.f
!	or
!      f90 -c naninfchk.f -YBOZTYPE=INT
!
!***********************************************************************

        LOGICAL FUNCTION NANCHK(X)
        IMPLICIT NONE
        REAL X,Y
        INTEGER I
        EQUIVALENCE(Y,I)
        Y = X
        NANCHK = ((I .and. z'7f80 0000') .eq. z'7f80 0000') .and. ((I .and. z'007f ffff') .ne. z'0000 0000')


        RETURN
        END


