!=======================================================================
!
! $Id: setkin_smv2par.h,v 1.1.1.1.22.1.282.1.8.1 2016/12/21 18:59:05 mmanyin Exp $
!
! FILE
!   setkin_smv2par.h
!   12 JUN 02 - PSC
!
! DESCRIPTION
!   This include file sets symbolic constants for the photochemical
!   mechanism.
!
!  Chemistry input file:    09/2016
!  Reaction dictionary:     GMI_Combo_rxns_124species_SO2_JPL15.db
!  Setkin files generated:  Wed Sep  7 15:23:58 2016
!
!========1=========2=========3=========4=========5=========6=========7==

      integer &
     &  SK_IGAS &
     & ,SK_IPHOT &
     & ,SK_ITHERM &
     & ,SK_NACT

      parameter (SK_IGAS   = 121)
      parameter (SK_IPHOT  =  81)
      parameter (SK_ITHERM = 321)
      parameter (SK_NACT   = 117)

!                                  --^--

