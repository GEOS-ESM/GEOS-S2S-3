!=======================================================================
! Copyright 2008, LANS LLC. All rights reserved.
! Unless otherwise indicated, this information has been authored by an 
! employee or employees of the Los Alamos National Security, LLC (LANS), 
! operator of the Los Alamos National Laboratory under Contract No. 
! DE-AC52-06NA25396 with the U.S. Department of Energy. The U.S. Government 
! has rights to use, reproduce, and distribute this information. The public 
! may copy and use this information without charge, provided that this 
! Notice and any statement of authorship are reproduced on all copies. 
! Neither the Government nor LANS makes any warranty, express or implied, 
! or assumes any liability or responsibility for the use of this 
! information.
!
! CICE is developed and maintained by Elizabeth C. Hunke (eclare@lanl.gov)
! and William H. Lipscomb (lipscomb@lanl.gov) of Group T-3 (Fluid 
! Dynamics), Los Alamos National Laboratory, with support from the 
! Climate Change Prediction Program (CCPP) and the Scientific 
! Discovery through Advanced Computing (SciDAC) program of the U.S. 
! Department of Energy.  We thank John Dukowicz (T-3), Phil Jones (T-3), 
! and Robert Malone (CCS-2) for their support of the sea ice modeling 
! effort at LANL.
!   
! CICE has been developed in close collaboration with the NCAR CCSM
! climate modeling project and includes ideas and efforts from 
! members the CCSM Polar Climate Working Group (PCWG).  We especially 
! thank the following members of the PCWG code development team:
!
! Cecilia Bitz, UW
! Bruce Briegleb, NCAR
! Tony Craig, NCAR
! John Dennis, NCAR
! Marika Holland, NCAR
! Bonnie Light, UW
! Julie Schramm, NCAR
! David Bailey, NCAR
!
! Assistance from researchers from U.K. Met Office Hadley Centre is
! also much appreciated, especially our collaboration with Alison 
! McLaren and Ann Keen. Numerous others have contributed to this 
! effort--thanks to all! 
!=======================================================================
#ifndef popcice
!
!BOP
!
! !MODULE: icemodel - main ice model program
!
! !DESCRIPTION:
!
! Main driver routine for CICE.  Initializes and steps through the model.
! This program should be compiled if CICE is run as a separate executable,
!  but not if CICE subroutines are called from another program (e.g., CAM).
!
! !REVISION HISTORY:
!  SVN:$Id: CICE.F90,v 1.1.1.1 2009/06/30 20:22:44 f4mjs Exp $
!
! authors Elizabeth C. Hunke and William H. Lipscomb, LANL
!
! 2006: Converted to free form source (F90) by Elizabeth Hunke
! 2008 ECH: moved ESMF code to its own driver
!
! !INTERFACE:
!
      program icemodel
!
! !USES:
!
      use ice_kinds_mod
#ifdef USE_ESMF
      use esmf_mod
      use CICE_ComponentMod
#else
      use CICE_InitMod
      use CICE_RunMod
      use CICE_FinalMod
#endif

!
!EOP
!
      implicit none

      !-----------------------------------------------------------------
      ! local variables
      !-----------------------------------------------------------------

#ifdef USE_ESMF
      type (ESMF_GridComp) :: &
         CICE_Comp          ! CICE as an ESMF component

      type (ESMF_VM) :: &
         worldVM            ! ESMF VM describing processor world

      type (ESMF_Clock) :: &
         synchClock         ! clock used to synchronize model in coupled
                            ! mode - used as arguments to init,run,final

      type (ESMF_State) :: &
         ciceImportState,   ! CICE import state
         ciceExportState    ! CICE export state

      integer (int_kind) :: &
         errorCode          ! error code from method calls

#else  
! declare as integer dummy arguments

      integer (int_kind) :: &
           CICE_Comp        , & ! dummy argument
           worldVM          , & ! dummy argument
           synchClock       , & ! dummy argument
           ciceimportState  , & ! dummy argument
           ciceexportState  , & ! dummy argument
           errorCode            ! dummy argument

#endif

#ifdef USE_ESMF
      !--------------------------------------------------------------------
      !  initialize ESMF and retrieve the global VM
      !--------------------------------------------------------------------

      errorCode = ESMF_Success

      call ESMF_Initialize(rc=errorCode)
      if (errorCode /= ESMF_SUCCESS) &
          stop 'CICE: Error initializing ESMF'

      call ESMF_VMGetGlobal(worldVM, rc=errorCode)
      if (errorCode /= ESMF_SUCCESS) stop 'CICE: Error getting ESMF VM'

      !--------------------------------------------------------------------
      !  create the mostly empty CICE component and register methods
      !  clock and grid info will be added during initialize
      !--------------------------------------------------------------------

      CICE_Comp = ESMF_GridCompCreate(worldVM, &
                                      name = 'CICE Sea Ice Component', &
                                      gridcomptype = ESMF_SEAICE, &
                                      rc=errorCode)

      if (errorCode /= ESMF_SUCCESS) &
         stop 'CICE: Error creating CICE comp'

      call ESMF_GridCompSetServices(CICE_Comp, CICE_SetServices, &
                                    errorCode)

      if (errorCode /= ESMF_SUCCESS) &
          stop 'CICE: Error registering CICE methods'
#endif

      !-----------------------------------------------------------------
      ! Initialize CICE
      !-----------------------------------------------------------------

#ifdef USE_ESMF
      call ESMF_GridCompInitialize(CICE_Comp, ciceImportState, &
                                ciceExportState, synchClock, errorCode)

      if (errorCode /= ESMF_Success) stop 'CICE: Error in init method' &
#else
      call CICE_Initialize(CICE_Comp, ciceImportState, &
                           ciceExportState, synchClock, errorCode)
#endif

      !-----------------------------------------------------------------
      ! Run CICE
      !-----------------------------------------------------------------

#ifdef USE_ESMF
      call ESMF_GridCompRun(CICE_Comp, ciceImportState, ciceExportState, &
                            synchClock, errorCode)

      if (errorCode /= ESMF_Success) stop 'CICE: Error in run method' 
#else
      call CICE_Run(CICE_Comp,  ciceImportState, ciceExportState, &
                    synchClock, errorCode)
#endif

      !-----------------------------------------------------------------
      ! Finalize CICE and exit ESMF
      !-----------------------------------------------------------------

#ifdef USE_ESMF
      call ESMF_GridCompFinalize(CICE_Comp, ciceImportState, &
                                            ciceExportState, &
                                            synchClock, errorCode)
      
       if (errorCode /= ESMF_Success) &
         stop 'CICE: Error in finalize method'

#else
      call CICE_Finalize(CICE_Comp,  ciceImportState, ciceExportState, &
                         synchClock, errorCode)
#endif


#ifdef USE_ESMF
      call ESMF_Finalize(rc=errorCode)
#endif

      end program icemodel

#endif
!=======================================================================
!BOP
!
! !ROUTINE: debug_ice - wrapper for print_state
!
! !DESCRIPTION:
!
! Wrapper for the print_state debugging routine.
! Useful for debugging in the main driver (see ice.F_debug)
! ip, jp, mtask are set in ice_diagnostics.F
!
! !REVISION HISTORY:
!
! author Elizabeth C. Hunke, LANL
!
! !INTERFACE:
!
      subroutine debug_ice(plabeld)
!
! !USES:
!
      use ice_kinds_mod
      use ice_diagnostics
      use ice_domain, only: nblocks
      use ice_blocks, only: nx_block, ny_block
!
! !INPUT/OUTPUT PARAMETERS:
!
      character (char_len), intent(in) :: plabeld
!
!EOP
!
      integer (kind=int_kind) :: i, j, iblk

      do iblk = 1, nblocks
      do j = 1, ny_block
      do i = 1, nx_block
         if (iblk==iblkp .and. i==ip .and. j==jp .and. my_task==mtask) &
              call print_state(plabeld,i,j,iblk)
      enddo
      enddo
      enddo

      end subroutine debug_ice

!=======================================================================
