!EOC
!------------------------------------------------------------------------------
!                  Harvard-NASA Emissions Component (HEMCO)                   !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hcoio_diagn_mod.F90
!
! !DESCRIPTION: Module HCOIO\_Diagn\_Mod.F90 is the data interface module
! for the HEMCO diagnostics. It contains routines to write out diagnostics
! into a netCDF file.
!\\
!\\
! In an ESMF/MAPL environment, the HEMCO diagnostics are not directly 
! written to disk but passed to the gridded component export state, where 
! they can be picked up by the MAPL HISTORY component.
!\\
!\\
! !INTERFACE:
!
MODULE HCOIO_DIAGN_MOD
!
! !USES:
!
  USE HCO_ERROR_MOD

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC :: HcoDiagn_Write 
  PUBLIC :: HCOIO_Diagn_WriteOut 
!
! !REMARKS:
!  HEMCO diagnostics are still in testing mode. We will fully activate them
!  at a later time.  They will be turned on when debugging & unit testing.
!
! !REVISION HISTORY:
!  04 May 2014 - C. Keller   - Initial version. 
!  11 Jun 2014 - R. Yantosca - Cosmetic changes in ProTeX headers
!  11 Jun 2014 - R. Yantosca - Now use F90 freeform indentation
!  28 Jul 2014 - C. Keller   - Removed GC specific initialization calls and
!                              moved to HEMCO core.
!  05 Aug 2014 - C. Keller   - Added dummy interface for ESMF.
!  03 Apr 2015 - C. Keller   - Added HcoDiagn_Write
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !DEFINED PARAMETERS:
!
  ! Fill value used in HEMCO diagnostics netCDF files.
!  REAL(hp), PARAMETER :: FillValue = 1.e-31_hp
  REAL(sp), PARAMETER :: FillValue = HCO_MISSVAL 

CONTAINS
!EOC
!------------------------------------------------------------------------------
!                  Harvard-NASA Emissions Component (HEMCO)                   !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HcoDiagn_Write
!
! !DESCRIPTION: Subroutine HcoDiagn_Write is the wrapper routine to write out
! the content of the built-in HEMCO diagnostics. If input argument Restart is
! set to TRUE, only the restart collection will be written out. Otherwise,
! the default collection
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HcoDiagn_Write( am_I_Root, HcoState, Restart, RC )
!
! !USES:
!
    USE HCO_State_Mod,        ONLY : HCO_State
!
! !INPUT/OUTPUT PARAMETERS:
!
    LOGICAL,         INTENT(IN   )    :: am_I_Root    ! Root CPU?
    TYPE(HCO_State), POINTER          :: HcoState     ! HEMCO state object 
    LOGICAL,         INTENT(IN   )    :: Restart      ! write restart (enforced)?
!
! !INPUT/OUTPUT PARAMETERS:
!
    INTEGER,         INTENT(INOUT)    :: RC           ! Return code
!
! !REVISION HISTORY: 
!  03 Apr 2015 - C. Keller   - Initial version 
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    INTEGER            :: I, COL
    CHARACTER(LEN=255) :: MSG, LOC

    !=================================================================
    ! HcoDiagn_Write begins here!
    !=================================================================

    ! Init
    LOC = 'HcoDiagn_Write (hcoi_diagn_mod.F90)'

    ! To write restart (enforced)
    IF ( RESTART ) THEN
       CALL HCOIO_DIAGN_WRITEOUT ( am_I_Root,                              &
                                   HcoState,                               &
                                   ForceWrite  = .TRUE.,                   &
                                   UsePrevTime = .FALSE.,                  &
                                   COL = HcoState%Diagn%HcoDiagnIDRestart, &
                                   RC          = RC                         )
       IF( RC /= HCO_SUCCESS) RETURN 

    ! Write all HEMCO diagnostics
    ELSE

       ! Loop over all collections that shall be written out.
       ! HCOIO_DIAGN_WRITEOUT will determine whether it is time to
       ! write a collection or not.
       DO I = 1, 3 

          ! Define collection ID
          SELECT CASE ( I ) 
             CASE ( 1 ) 
                COL = HcoState%Diagn%HcoDiagnIDDefault
             CASE ( 2 ) 
                COL = HcoState%Diagn%HcoDiagnIDRestart
             CASE ( 3 ) 
                COL = HcoState%Diagn%HcoDiagnIDManual
          END SELECT
  
          ! If not ESMF environment, never write the manual diagnostics
          ! to disk. Instead, the content of the manual diagnostics needs
          ! to be fetched explicitly.
#if       !defined ( ESMF_ ) 
          IF ( I == 3 ) CYCLE
#endif
 
          ! Restart file 
          CALL HCOIO_DIAGN_WRITEOUT ( am_I_Root,                       &
                                      HcoState,                        &
                                      ForceWrite  = .FALSE.,           &
                                      UsePrevTime = .FALSE.,           &
                                      COL         = COL,               &
                                      RC          = RC                  )
          IF(RC /= HCO_SUCCESS) RETURN 
       ENDDO
    ENDIF

  END SUBROUTINE HcoDiagn_Write 
!EOC
!------------------------------------------------------------------------------
!                  Harvard-NASA Emissions Component (HEMCO)                   !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: HCOIO_Diagn_WriteOut
!
! !DESCRIPTION: Subroutine HCOIO\_Diagn\_WriteOut writes diagnostics to 
! output. Depending on the model environment, different subroutines will
! be invoked. 
!\\
!\\
! !INTERFACE:
!
  SUBROUTINE HCOIO_Diagn_WriteOut( am_I_Root,   HcoState, ForceWrite,  &
                                   RC,          PREFIX,   UsePrevTime, &
                                   OnlyIfFirst, COL                     )
!
! !USES:
!
    USE HCO_State_Mod,        ONLY : HCO_State
#if defined(ESMF_)
    USE HCOIO_WRITE_ESMF_MOD, ONLY : HCOIO_WRITE_ESMF
#else
    USE HCOIO_WRITE_STD_MOD,  ONLY : HCOIO_WRITE_STD
#endif
!
! !INPUT PARAMETERS:
!
    LOGICAL,                    INTENT(IN   ) :: am_I_Root   ! root CPU?
    TYPE(HCO_State),  POINTER                 :: HcoState    ! HEMCO state object 
    LOGICAL,                    INTENT(IN   ) :: ForceWrite  ! Write all diagnostics? 
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN   ) :: PREFIX      ! File prefix
    LOGICAL,          OPTIONAL, INTENT(IN   ) :: UsePrevTime ! Use previous time 
    LOGICAL,          OPTIONAL, INTENT(IN   ) :: OnlyIfFirst ! Only write if nnDiagn is 1
    INTEGER,          OPTIONAL, INTENT(IN   ) :: COL         ! Collection Nr. 
!
! !INPUT/OUTPUT PARAMETERS:
!

    INTEGER,          INTENT(INOUT) :: RC          ! Failure or success
!
! !REVISION HISTORY: 
!  12 Sep 2013 - C. Keller   - Initial version 
!  11 Jun 2014 - R. Yantosca - Cosmetic changes in ProTeX headers
!  11 Jun 2014 - R. Yantosca - Now use F90 freeform indentation
!  19 Feb 2015 - C. Keller   - Added optional argument OnlyIfFirst
!  23 Feb 2015 - R. Yantosca - Now make Arr1D REAL(sp) so that we can write
!                              out lon & lat as float instead of double
!  06 Nov 2015 - C. Keller   - Output time stamp is now determined from 
!                              variable OutTimeStamp.
!  20 Jan 2016 - C. Keller   - Added options DiagnRefTime and DiagnNoLevDim.
!  22 Feb 2016 - C. Keller   - Now calls down to environment specific routines.
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
    CHARACTER(LEN=255), PARAMETER :: LOC = 'HCOIO_DIAGN_WRITEOUT (hcoio_diagn_mod.F90)' 

    !=================================================================
    ! HCOIO_DIAGN_WRITEOUT begins here!
    !=================================================================
    
    !-----------------------------------------------------------------
    ! ESMF environment: call ESMF output routines
    !-----------------------------------------------------------------
#if defined(ESMF_)
    CALL HCOIO_WRITE_ESMF ( am_I_Root,               &
                            HcoState,                &
                            RC,                      &
                            OnlyIfFirst=OnlyIfFirst, &
                            COL=COL                   ) 
    IF ( RC /= HCO_SUCCESS ) RETURN

    !-----------------------------------------------------------------
    ! Standard environment: call default output routines
    !-----------------------------------------------------------------
#else
    CALL HCOIO_WRITE_STD( am_I_Root,                &
                          HcoState,                 &
                          ForceWrite,               &
                          RC,                       &
                          PREFIX      =PREFIX,      &
                          UsePrevTime =UsePrevTime, &
                          OnlyIfFirst =OnlyIfFirst, &
                          COL         = COL          )
    IF ( RC /= HCO_SUCCESS ) RETURN

#endif

    ! Return 
    RC = HCO_SUCCESS

  END SUBROUTINE HCOIO_DIAGN_WRITEOUT
!EOC
END MODULE HCOIO_Diagn_Mod

