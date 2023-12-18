!=======================================================================
!BOP
!
! !MODULE: ice_kinds_mod - defines variable precision
!
! !DESCRIPTION:
!
! Defines variable precision for all common data types \\
! Code originally based on kinds_mod.F in POP
!
! !REVISION HISTORY:
!  SVN:$Id: ice_kinds_mod_GEOS.F90,v 1.1 2009/07/17 18:55:33 trayanov Exp $
!
! author: Elizabeth C. Hunke and William H. Lipscomb, LANL
! 2006: ECH converted to free source form (F90)
!
! !INTERFACE:
!
      module ice_kinds_mod
!
! !USES:
!
!EOP
!=======================================================================

      implicit none
      save

      integer, parameter :: char_len  = 80, &
                            char_len_long  = 256, &
                            log_kind  = kind(.true.), &
                            int_kind  = selected_int_kind(6), &
                            real_kind = selected_real_kind(6), &
                            r16_kind  = selected_real_kind(26), &
#ifdef GEOS
                            dbl_kind  = selected_real_kind(6)
#else
                            dbl_kind  = selected_real_kind(13)
#endif

!=======================================================================

      end module ice_kinds_mod

!=======================================================================
