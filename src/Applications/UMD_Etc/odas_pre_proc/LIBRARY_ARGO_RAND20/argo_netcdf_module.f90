MODULE ARGO_NETCDF_MODULE
  
  IMPLICIT NONE
  PUBLIC :: read_argo_init, read_argo_init_qc, read_argo_ts, read_argo_pid
      
  CONTAINS


  ! .................................................................
    SUBROUTINE read_argo_init(fname,N_PROF_len,N_LEVELS_len)
  ! .................................................................
    IMPLICIT NONE
    INCLUDE 'netcdf.inc'

    character(len=*)       :: fname
    integer,  intent(out)  :: N_PROF_len, N_LEVELS_len

    character*100          :: fname_trim
!   character*75           :: fname_trim

    integer                :: ncid, nstatus, ndims, nvars, nlen
    integer                :: N_PROF_id, N_LEVELS_id


  ! ******************************************************************
  ! Open Dataset
  ! ............................ 
    fname_trim = adjustr(trim(fname))
    !status = nf_open(trim(fname),0, ncid)
    nstatus = nf_open(fname_trim, 0, ncid)
    !print *, ncid, nstatus, len(fname_trim), len(fname)

    if (nstatus>0) then
      N_PROF_len = 0
      N_LEVELS_len = 0
      nstatus = nf_close(ncid)
      return
    endif
 
  ! Inquire about Dataset
  ! .................................
    nstatus = nf_inq_ndims(ncid,ndims)
    nstatus = nf_inq_nvars(ncid,nvars)

  ! Inquire about Dimensions
  ! .....................................................
    nstatus = nf_inq_dimid(ncid,'N_PROF',N_PROF_id)
    nstatus = nf_inq_dimlen(ncid,N_PROF_id,N_PROF_len)

    nstatus = nf_inq_dimid(ncid,'N_LEVELS',N_LEVELS_id)
    nstatus = nf_inq_dimlen(ncid,N_LEVELS_id,N_LEVELS_len)

    nstatus = nf_close(ncid)

  return
  END SUBROUTINE read_argo_init


  ! .................................................................
    SUBROUTINE read_argo_init_qc(fname,N_PROF_len,N_LEVELS_len)
  ! .................................................................
    IMPLICIT NONE
    INCLUDE 'netcdf.inc'

  ! Input Variables
    character*256             :: fname
!   character*84              :: fname_trim
    character*100              :: fname_trim

    character*3               :: basin
    character*8               :: sdate
    integer,       intent(in) :: N_PROF_len
    integer,       intent(inout) ::N_LEVELS_len
    
    real                      :: miss2
    real                      :: miss
    character                 :: DATA_MODE_T(N_PROF_len)
    integer                   :: NPTS_T(N_PROF_len)
    integer                   :: NPTS_TR(N_PROF_len), NPTS_TD(N_PROF_len)
    integer                   :: NPTS_PR(N_PROF_len), NPTS_PD(N_PROF_len)
    integer                   :: NPTS_TR_QC(N_PROF_len), NPTS_TD_QC(N_PROF_len)

    integer                   :: NPTS_PR_QC(N_PROF_len), NPTS_PD_QC(N_PROF_len)

  ! Define Dataset
    integer :: ncid, nstatus, i, j,cnt
    integer :: ndims, nvars, ngatts, unlimdimid
    integer :: start1, count1
    integer :: start2(2), count2(2)

  ! Define Dimensions
    integer :: N_PROF_id
    integer :: N_LEVELS_id
    integer :: STRING1_id,   STRING1_len
    integer :: STRING4_id,   STRING4_len
    integer :: STRING8_id,   STRING8_len
    integer :: STRING64_id,  STRING64_len

  ! Define Variables
    integer              :: DATA_MODE_id, DATA_MODE_dim
    character*1          :: DATA_MODE(N_PROF_len)   
    
    integer              :: PROFILE_PRES_QC_id, PROFILE_PRES_QC_dim
    character            :: PROFILE_PRES_QC(N_PROF_len)

    integer              :: PROFILE_TEMP_QC_id, PROFILE_TEMP_QC_dim
    character            :: PROFILE_TEMP_QC(N_PROF_len)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    integer              :: PRES_ADJUSTED_id, PRES_ADJUSTED_dim
    real                 :: PRES_ADJUSTED(N_LEVELS_len,N_PROF_len)
    
    integer              :: PRES_id, PRES_dim
    real                 :: PRES(N_LEVELS_len,N_PROF_len)

    integer              :: PRES_ADJUSTED_QC_id, PRES_ADJUSTED_QC_dim
    character            :: PRES_ADJUSTED_QC(N_LEVELS_len,N_PROF_len)    

    integer              :: PRES_QC_id, PRES_QC_dim
    character            :: PRES_QC(N_LEVELS_len,N_PROF_len)    
    
    integer              :: TEMP_ADJUSTED_id, TEMP_ADJUSTED_dim
    real                 :: TEMP_ADJUSTED(N_LEVELS_len,N_PROF_len)

    integer              :: TEMP_id, TEMP_dim
    real                 :: TEMP(N_LEVELS_len,N_PROF_len)
    
    integer              :: TEMP_ADJUSTED_QC_id, TEMP_ADJUSTED_QC_dim
    character            :: TEMP_ADJUSTED_QC(N_LEVELS_len,N_PROF_len)

    integer              :: TEMP_QC_id, TEMP_QC_dim
    character            :: TEMP_QC(N_LEVELS_len,N_PROF_len)
    
    ! Final data after checking DATA_MODE
    real                 :: T(N_LEVELS_len,N_PROF_len)
    real                 :: P(N_LEVELS_len,N_PROF_len)
    real                 :: TQC(N_LEVELS_len,N_PROF_len)
    real                 :: PQC(N_LEVELS_len,N_PROF_len)
    real                 :: PRF_TEMP_QC(N_PROF_len),PRF_PRES_QC(N_PROF_len)

    miss  = 99999.0
    miss2 = 999.e9
    cnt = 0

  ! ****************************************************************************
  ! Open Dataset
  ! ............................
    
    !print *, 'fname ',trim(fname)
    !fname_trim = adjustr(fname)
    fname_trim = trim(fname)
    !print *, 'fname_trim ',fname_trim
    nstatus = nf_open(fname_trim, 0, ncid)

    !print *, nstatus   
    !print *, len(trim(fname_trim))
    !print *, trim(fname_trim)

  ! Inquire about Dataset
  ! .................................
    nstatus = nf_inq_ndims(ncid,ndims)
    nstatus = nf_inq_nvars(ncid,nvars)

  ! Inquire about Dimensions
  ! .....................................................
    nstatus = nf_inq_dimid(ncid,'N_PROF',N_PROF_id)

    nstatus = nf_inq_dimid(ncid,'N_LEVELS',N_LEVELS_id)

    nstatus = nf_inq_dimid(ncid,'STRING1',STRING1_id)
    nstatus = nf_inq_dimlen(ncid,STRING1_id,STRING1_len)

    nstatus = nf_inq_dimid(ncid,'STRING4',STRING4_id)
    nstatus = nf_inq_dimlen(ncid,STRING4_id,STRING4_len)

    nstatus = nf_inq_dimid(ncid,'STRING8',STRING8_id)
    nstatus = nf_inq_dimlen(ncid,STRING8_id,STRING8_len)

    nstatus = nf_inq_dimid(ncid,'STRING64',STRING64_id)     
    nstatus = nf_inq_dimlen(ncid,STRING64_id,STRING64_len)

 
  ! Inquire about Variables
  ! .....................................................................
    nstatus = nf_inq_varid(ncid,'DATA_MODE',DATA_MODE_id)
    nstatus = nf_inq_varndims(ncid,DATA_MODE_id,DATA_MODE_dim)

    nstatus = nf_inq_varid(ncid,'PROFILE_PRES_QC',PROFILE_PRES_QC_id)
    nstatus = nf_inq_varndims(ncid,PROFILE_PRES_QC_id,PROFILE_PRES_QC_dim)

    nstatus = nf_inq_varid(ncid,'PROFILE_TEMP_QC',PROFILE_TEMP_QC_id)
    nstatus = nf_inq_varndims(ncid,PROFILE_TEMP_QC_id,PROFILE_TEMP_QC_dim)

!!!!! Adjusted Values   
    nstatus = nf_inq_varid(ncid,'PRES_ADJUSTED',PRES_ADJUSTED_id)
    nstatus = nf_inq_varndims(ncid,PRES_ADJUSTED_id,PRES_ADJUSTED_dim)

    nstatus = nf_inq_varid(ncid,'PRES_ADJUSTED_QC',PRES_ADJUSTED_QC_id)     
    nstatus = nf_inq_varndims(ncid,PRES_ADJUSTED_QC_id,PRES_ADJUSTED_QC_dim)

    nstatus = nf_inq_varid(ncid,'TEMP_ADJUSTED',TEMP_ADJUSTED_id)
    nstatus = nf_inq_varndims(ncid,TEMP_ADJUSTED_id,TEMP_ADJUSTED_dim)

    nstatus = nf_inq_varid(ncid,'TEMP_ADJUSTED_QC',TEMP_ADJUSTED_QC_id)     
    nstatus = nf_inq_varndims(ncid,TEMP_ADJUSTED_QC_id,TEMP_ADJUSTED_QC_dim)

!!!!! Real-Time Values   
    nstatus = nf_inq_varid(ncid,'PRES',PRES_id)
    nstatus = nf_inq_varndims(ncid,PRES_id,PRES_dim)

    nstatus = nf_inq_varid(ncid,'PRES_QC',PRES_QC_id)     
    nstatus = nf_inq_varndims(ncid,PRES_QC_id,PRES_QC_dim)

    nstatus = nf_inq_varid(ncid,'TEMP',TEMP_id)
    nstatus = nf_inq_varndims(ncid,TEMP_id,TEMP_dim)

    nstatus = nf_inq_varid(ncid,'TEMP_QC',TEMP_QC_id)     
    nstatus = nf_inq_varndims(ncid,TEMP_QC_id,TEMP_QC_dim)

  ! Get Variables
  ! ............................................................................ 
    start1 = 1
    count1 = N_PROF_len         
      nstatus = nf_get_vara_text(ncid,DATA_MODE_id,start1,count1,DATA_MODE)        

    start1 = 1
    count1 = N_PROF_len
      nstatus = nf_get_vara_text(ncid,PROFILE_PRES_QC_id,start1,count1,PROFILE_PRES_QC)
      nstatus = nf_get_vara_text(ncid,PROFILE_TEMP_QC_id,start1,count1,PROFILE_TEMP_QC)

    start2 = (/1,1/)
    count2 = (/N_LEVELS_len,N_PROF_len/)
      nstatus = nf_get_vara_real(ncid,PRES_ADJUSTED_id,start2,count2,PRES_ADJUSTED)
      nstatus = nf_get_vara_real(ncid,TEMP_ADJUSTED_id,start2,count2,TEMP_ADJUSTED)

      nstatus = nf_get_vara_text(ncid,PRES_ADJUSTED_QC_id,start2,count2,PRES_ADJUSTED_QC)
      nstatus = nf_get_vara_text(ncid,TEMP_ADJUSTED_QC_id,start2,count2,TEMP_ADJUSTED_QC)

      nstatus = nf_get_vara_real(ncid,PRES_id,start2,count2,PRES)
      nstatus = nf_get_vara_real(ncid,TEMP_id,start2,count2,TEMP)

      nstatus = nf_get_vara_text(ncid,PRES_QC_id,start2,count2,PRES_QC)
      nstatus = nf_get_vara_text(ncid,TEMP_QC_id,start2,count2,TEMP_QC)

 
! Backtrack from end of profile get number of valid levels: NPTS
! ...............................................................
! TEMP
! ...............................................................
    ! Zeros and Nans
    DO  i = 1,  N_PROF_len 
       DO j =  1, N_LEVELS_len
         if ( TEMP(j,i) == 0 ) then
              TEMP(j,i) = miss
         endif
         if ( (TEMP(j,i)/TEMP(j,i)) /= 1 ) then
              TEMP(j,i) = miss
         endif        
      ENDDO
    ENDDO
    
    DO i = 1,  N_PROF_len
       j = N_LEVELS_len         
       if ( TEMP(N_LEVELS_len,i) >= miss ) then 
          DO WHILE ( TEMP(j,i) >= miss .and. j > 0 )
             j=j-1     
             if (j == 0) exit
          ENDDO
          NPTS_TR(i) = j
       else
          NPTS_TR(i) = N_LEVELS_len
       endif
    ENDDO
    
    ! Zeros and Nans
    DO  i = 1,  N_PROF_len 
       DO j =  1, N_LEVELS_len
         if ( TEMP_ADJUSTED(j,i) == 0 ) then
              TEMP_ADJUSTED(j,i) = miss
         endif
         if ( (TEMP_ADJUSTED(j,i)/TEMP_ADJUSTED(j,i)) /= 1 ) then
              TEMP_ADJUSTED(j,i) = miss
         endif        
      ENDDO
    ENDDO 
       
    DO i = 1,  N_PROF_len
       j = N_LEVELS_len         
       if ( TEMP_ADJUSTED(N_LEVELS_len,i) >= miss ) then 
          DO WHILE ( TEMP_ADJUSTED(j,i) >= miss .and. j > 0 )
             j=j-1       
             if (j == 0) exit
          ENDDO
          NPTS_TD(i) = j
       else
          NPTS_TD(i) = N_LEVELS_len
       endif
    ENDDO
    
    

    DO i = 1,  N_PROF_len
       j = N_LEVELS_len
       if ( len_trim(TEMP_QC(N_LEVELS_len,i)) == 0 ) then 
          DO WHILE ( len_trim(TEMP_QC(j,i)) == 0 .and. j > 0 )
             j=j-1       
             if (j == 0) exit
          ENDDO
          NPTS_TR_QC(i) = j
       else
          NPTS_TR_QC(i) = N_LEVELS_len
       endif
       DO j = 1, NPTS_TR_QC(i)
         if ( len_trim(TEMP_QC(j,i)) == 0 ) then 
           TEMP_QC(j,i) = '9'
         endif
       ENDDO
    ENDDO

    DO i = 1,  N_PROF_len
       j = N_LEVELS_len         
       if ( len_trim(TEMP_ADJUSTED_QC(N_LEVELS_len,i)) == 0 ) then 
          DO WHILE ( len_trim(TEMP_ADJUSTED_QC(j,i)) == 0 .and. j > 0 )
             j=j-1       
             if (j == 0) exit
          ENDDO
          NPTS_TD_QC(i) = j
       else
          NPTS_TD_QC(i) = N_LEVELS_len
       endif
       DO j = 1, NPTS_TD_QC(i)
         if ( len_trim(TEMP_ADJUSTED_QC(j,i)) == 0 ) then 
           TEMP_ADJUSTED_QC(j,i) = '9'
         endif
       ENDDO
    ENDDO

! ...............................................................
! Pressure
! ...............................................................
    ! Zeros and Nans
    DO  i = 1,  N_PROF_len 
       DO j =  1, N_LEVELS_len
         if ( PRES(j,i) == 0 ) then
              PRES(j,i) = miss
         endif
         if ( (PRES(j,i)/PRES(j,i)) /= 1 ) then
              PRES(j,i) = miss
         endif        
      ENDDO
    ENDDO

    DO i = 1,  N_PROF_len
        j = N_LEVELS_len       
        if ( PRES(N_LEVELS_len,i) >= miss ) then 
          DO WHILE ( PRES(j,i) >= miss .and. j > 0 )
             j=j-1       
             if (j == 0) exit
          ENDDO
          NPTS_PR(i) = j
       else
          NPTS_PR(i) = N_LEVELS_len
       endif
    ENDDO
    
    ! Zeros and Nans
    DO  i = 1,  N_PROF_len 
       DO j =  1, N_LEVELS_len
         if ( PRES_ADJUSTED(j,i) == 0 ) then
              PRES_ADJUSTED(j,i) = miss
         endif
         if ( (PRES_ADJUSTED(j,i)/PRES_ADJUSTED(j,i)) /= 1 ) then
              PRES_ADJUSTED(j,i) = miss
         endif        
      ENDDO
    ENDDO 

    DO i = 1,  N_PROF_len
       j = N_LEVELS_len         
       if ( PRES_ADJUSTED(N_LEVELS_len,i) >= miss ) then 
          DO WHILE ( PRES_ADJUSTED(j,i) >= miss .and. j > 0 )
             j=j-1       
             if (j == 0) exit
          ENDDO
          NPTS_PD(i) = j
       else
          NPTS_PD(i) = N_LEVELS_len
       endif 
    ENDDO 

    DO i = 1,  N_PROF_len
       j = N_LEVELS_len         
       if ( len_trim(PRES_QC(N_LEVELS_len,i)) == 0 ) then 
          DO WHILE ( len_trim(PRES_QC(j,i)) == 0 .and. j > 0 )
             j=j-1       
             if (j == 0) exit
          ENDDO
          NPTS_PR_QC(i) = j
       else
          NPTS_PR_QC(i) = N_LEVELS_len
       endif
       DO j = 1, NPTS_PR_QC(i)
         if ( len_trim(PRES_QC(j,i)) == 0 ) then 
           PRES_QC(j,i) = '9'
         endif
       ENDDO
    ENDDO
    DO i = 1,  N_PROF_len
       j = N_LEVELS_len         
       if ( len_trim(PRES_ADJUSTED_QC(N_LEVELS_len,i)) == 0 ) then 
          DO WHILE ( len_trim(PRES_ADJUSTED_QC(j,i)) == 0 .and. j > 0 )
             j=j-1       
             if (j == 0) exit
          ENDDO
          NPTS_PD_QC(i) = j
       else
          NPTS_PD_QC(i) = N_LEVELS_len
       endif
       DO j = 1, NPTS_PD_QC(i)
         if ( len_trim(PRES_ADJUSTED_QC(j,i)) == 0 ) then 
           PRES_ADJUSTED_QC(j,i) = '9'
         endif
       ENDDO
    ENDDO

! Profile QC are letters A to F, deny D,E,F
! Level QC are numbers 0 to 9, deny 4 and 9
! ...............................................................
! Decide to use either R (realtime) or D (delayed mode) data
! ...............................................................
    DO i = 1,N_PROF_len 
       if (DATA_MODE(i) == 'R') then
        !.............................................................
	! TEMP!
        !.............................................................
          if (NPTS_TR(i) > 0) then
	     NPTS_T(i) = NPTS_TR(i)
             if (PROFILE_TEMP_QC(i) == '4' .or. PROFILE_TEMP_QC(i) == 'D' .or. &
                 PROFILE_TEMP_QC(i) == 'E' .or. PROFILE_TEMP_QC(i) == 'F' .or. &
                 PROFILE_TEMP_QC(i) == '9' .or. PROFILE_TEMP_QC(i) == 'C' ) then	
	        NPTS_T(i) = 0	
             endif    
	  else
	     NPTS_T(i) = 0  
	  endif

        !.............................................................
	! PRESSURE!
        !.............................................................
          if (NPTS_PR(i) > 0) then
             if (PROFILE_TEMP_QC(i) == '4' .or. PROFILE_TEMP_QC(i) == 'D' .or. &
                 PROFILE_TEMP_QC(i) == 'E' .or. PROFILE_TEMP_QC(i) == 'F' .or. &
                 PROFILE_TEMP_QC(i) == '9'  .or. PROFILE_TEMP_QC(i) == 'C') then	     
	        NPTS_T(i) = 0	     		     
             endif    
	  else  	     
	     NPTS_T(i) = 0	     		            	  	          
	  endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Sometimes it says 'D' but the adjuste values are missing, then we need to use 'R'
       elseif (DATA_MODE(i) == 'D') then
        !.............................................................
	! TEMP!
        !.............................................................
          if (NPTS_TD(i) > 0) then
	     NPTS_T(i) = NPTS_TD(i)
             if (PROFILE_TEMP_QC(i) == '4' .or. PROFILE_TEMP_QC(i) == 'D' .or. &
                 PROFILE_TEMP_QC(i) == 'E' .or. PROFILE_TEMP_QC(i) == 'F' .or. &
                 PROFILE_TEMP_QC(i) == '9' .or. PROFILE_TEMP_QC(i) == 'C' ) then	
	        NPTS_T(i) = 0	
             endif  
           elseif (NPTS_TR(i) > 0) then
	     NPTS_T(i) = NPTS_TR(i)
             if (PROFILE_TEMP_QC(i) == '4' .or. PROFILE_TEMP_QC(i) == 'D' .or. &
                 PROFILE_TEMP_QC(i) == 'E' .or. PROFILE_TEMP_QC(i) == 'F' .or. &
                 PROFILE_TEMP_QC(i) == '9' .or. PROFILE_TEMP_QC(i) == 'C' ) then	
	        NPTS_T(i) = 0	
             endif   
	  else
	     NPTS_T(i) = 0  
	  endif

        !.............................................................
	! PRESSURE !
        !.............................................................
          if (NPTS_PD(i) > 0) then
             if (PROFILE_TEMP_QC(i) == '4' .or. PROFILE_TEMP_QC(i) == 'D' .or. &
                 PROFILE_TEMP_QC(i) == 'E' .or. PROFILE_TEMP_QC(i) == 'F' .or. &
                 PROFILE_TEMP_QC(i) == '9' .or. PROFILE_TEMP_QC(i) == 'C') then	
	        NPTS_T(i) = 0	     	
	          
             endif   
          elseif (NPTS_PR(i) > 0) then
             if (PROFILE_TEMP_QC(i) == '4' .or. PROFILE_TEMP_QC(i) == 'D' .or. &
                 PROFILE_TEMP_QC(i) == 'E' .or. PROFILE_TEMP_QC(i) == 'F' .or. &
                 PROFILE_TEMP_QC(i) == '9' .or. PROFILE_TEMP_QC(i) == 'C' ) then	
	        NPTS_T(i) = 0	     	
             endif   
	  else  
	     NPTS_T(i) = 0	     	  	          
	  endif


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       elseif (DATA_MODE(i) == 'A') then
	  DATA_MODE_T(i) = 'A'
          if (NPTS_TD(i) > 0) then
	     NPTS_T(i) = NPTS_TD(i)
             if (PROFILE_TEMP_QC(i) == '4' .or. PROFILE_TEMP_QC(i) == 'D' .or. &
                 PROFILE_TEMP_QC(i) == 'E' .or. PROFILE_TEMP_QC(i) == 'F' .or. &
                 PROFILE_TEMP_QC(i) == '9' .or. PROFILE_TEMP_QC(i) == 'C' ) then	
	        NPTS_T(i) = 0		
             endif    
	  elseif (NPTS_TR(i) > 0) then
	     NPTS_T(i) = NPTS_TR(i)
             if (PROFILE_TEMP_QC(i) == '4' .or. PROFILE_TEMP_QC(i) == 'D' .or. &
                 PROFILE_TEMP_QC(i) == 'E' .or. PROFILE_TEMP_QC(i) == 'F' .or. &
                 PROFILE_TEMP_QC(i) == '9' .or. PROFILE_TEMP_QC(i) == 'C' ) then	
	        NPTS_T(i) = 0		
             endif    
	  else
	     NPTS_T(i) = 0
	  endif 
          
 
	  if (NPTS_PD(i) > 0) then	  

	  elseif (NPTS_PR(i) > 0) then

	  else
  	     NPTS_T(i) = 0
 	  endif  

       endif
       if (NPTS_T(i) > cnt) then
         cnt = NPTS_T(i)
       endif
       !print *, i, NPTS_T(i), PROFILE_TEMP_QC(i)

    ENDDO
    N_LEVELS_len = cnt
    !print *, cnt
    nstatus = nf_close(ncid)

  return
  END SUBROUTINE read_argo_init_qc



  ! ..................................................................................
    SUBROUTINE read_argo_ts (fname_in,miss2,N_PROF_len,N_LEVELS_len,PLATFORM_NUMBER,&
                          INST_REFERENCE,WMO_INST_TYPE,LAT,LON,P,T,S,&
			  PQC,TQC,SQC,PRF_PRES_QC,PRF_TEMP_QC,&
			  PRF_PSAL_QC,POS_PRF_QC,DIRECTION,DATA_STATE_INDICATOR,&
			  DATA_MODE_T,DATA_MODE_S,NPTS,JULD)
  ! ..................................................................................
    IMPLICIT NONE
    INCLUDE 'netcdf.inc'

  ! Input Variables
    character*256, intent(in) :: fname_in
    character*8               :: sdate
    integer,       intent(in) :: N_PROF_len, N_LEVELS_len

    real, parameter           :: Tmin = -10, Tmax = 50
    real, parameter           :: Smin =  10, Smax = 40
    
    real                      :: miss2
    real                      :: miss, negmiss
    character*1               :: DATA_MODE_T(N_PROF_len), DATA_MODE_S(N_PROF_len)
    integer                   :: NPTS_T(N_PROF_len), NPTS_S(N_PROF_len), NPTS(N_PROF_len)
    integer                   :: NPTS_TR(N_PROF_len), NPTS_TD(N_PROF_len)
    integer                   :: NPTS_SR(N_PROF_len), NPTS_SD(N_PROF_len)
    integer                   :: NPTS_PR(N_PROF_len), NPTS_PD(N_PROF_len)
    integer                   :: NPTS_TR_QC(N_PROF_len), NPTS_TD_QC(N_PROF_len)
    integer                   :: NPTS_SR_QC(N_PROF_len), NPTS_SD_QC(N_PROF_len)
    integer                   :: NPTS_PR_QC(N_PROF_len), NPTS_PD_QC(N_PROF_len)

  ! Define Dataset
    integer :: ncid, status, i, j, k, psal_status, psala_status, debug
    integer :: pres_status, presa_status
    integer :: ndims, nvars, ngatts, unlimdimid
    integer :: start1, count1
    integer :: start2(2), count2(2)

  ! Define Dimensions
    integer :: N_PROF_id
    integer :: N_LEVELS_id
    integer :: STRING1_id,   STRING1_len
    integer :: STRING4_id,   STRING4_len
    integer :: STRING8_id,   STRING8_len
    integer :: STRING64_id,  STRING64_len

  ! Define Variables
    integer              :: PLATFORM_NUMBER_id, PLATFORM_NUMBER_dim
    character*8          :: PLATFORM_NUMBER(N_PROF_len)

    integer              :: INST_REFERENCE_id, INST_REFERENCE_dim
    character*64         :: INST_REFERENCE(N_PROF_len)

    integer              :: WMO_INST_TYPE_id, WMO_INST_TYPE_dim
    character*4          :: WMO_INST_TYPE(N_PROF_len)
    
    integer              :: DATA_STATE_INDICATOR_id, DATA_STATE_INDICATOR_dim
    character*4          :: DATA_STATE_INDICATOR(N_PROF_len)

    integer              :: DIRECTION_id, DIRECTION_dim
    character*1          :: DIRECTION(N_PROF_len)
    
    integer              :: DATA_MODE_id, DATA_MODE_dim
    character*1          :: DATA_MODE(N_PROF_len)   
    
    integer              :: LATITUDE_id, LATITUDE_dim
    double precision     :: LATITUDE(N_PROF_len)
    real*4               :: LAT(N_PROF_len)

    integer              :: LONGITUDE_id, LONGITUDE_dim
    double precision     :: LONGITUDE(N_PROF_len)
    real*4               :: LON(N_PROF_len)

    integer              :: JULD_id, JULD_dim
    double precision     :: JULD(N_PROF_len)
    real*4               :: JULIANDAY(N_PROF_len)

    integer              :: POSITION_QC_id, POSITION_QC_dim
    character            :: POSITION_QC(N_PROF_len)

    integer              :: PROFILE_PRES_QC_id, PROFILE_PRES_QC_dim
    character            :: PROFILE_PRES_QC(N_PROF_len)

    integer              :: PROFILE_TEMP_QC_id, PROFILE_TEMP_QC_dim
    character            :: PROFILE_TEMP_QC(N_PROF_len)

    integer              :: PROFILE_PSAL_QC_id, PROFILE_PSAL_QC_dim
    character            :: PROFILE_PSAL_QC(N_PROF_len)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    integer              :: PRES_ADJUSTED_id, PRES_ADJUSTED_dim
    real*4               :: PRES_ADJUSTED(N_LEVELS_len,N_PROF_len)
    
    integer              :: PRES_id, PRES_dim
    real*4               :: PRES(N_LEVELS_len,N_PROF_len)

    integer              :: PRES_ADJUSTED_QC_id, PRES_ADJUSTED_QC_dim
    character            :: PRES_ADJUSTED_QC(N_LEVELS_len,N_PROF_len)    

    integer              :: PRES_QC_id, PRES_QC_dim 
    character            :: PRES_QC(N_LEVELS_len,N_PROF_len)    
    
    integer              :: PSAL_ADJUSTED_id, PSAL_ADJUSTED_dim
    real                 :: PSAL_ADJUSTED(N_LEVELS_len,N_PROF_len)
    
    integer              :: PSAL_id, PSAL_dim
    real                 :: PSAL(N_LEVELS_len,N_PROF_len)

    integer              :: PSAL_ADJUSTED_QC_id, PSAL_ADJUSTED_QC_dim
    character            :: PSAL_ADJUSTED_QC(N_LEVELS_len,N_PROF_len)

    integer              :: PSAL_QC_id, PSAL_QC_dim
    character            :: PSAL_QC(N_LEVELS_len,N_PROF_len)
    
    integer              :: TEMP_ADJUSTED_id, TEMP_ADJUSTED_dim
    real                 :: TEMP_ADJUSTED(N_LEVELS_len,N_PROF_len)

    integer              :: TEMP_id, TEMP_dim
    real                 :: TEMP(N_LEVELS_len,N_PROF_len)
    
    integer              :: TEMP_ADJUSTED_QC_id, TEMP_ADJUSTED_QC_dim
    character            :: TEMP_ADJUSTED_QC(N_LEVELS_len,N_PROF_len)

    integer              :: TEMP_QC_id, TEMP_QC_dim
    character            :: TEMP_QC(N_LEVELS_len,N_PROF_len)
    
    ! Final data after checking DATA_MODE
    real*4                 :: T(N_LEVELS_len,N_PROF_len), S(N_LEVELS_len,N_PROF_len)
    real*4                 :: P(N_LEVELS_len,N_PROF_len)
    real*4                 :: TQC(N_LEVELS_len,N_PROF_len), SQC(N_LEVELS_len,N_PROF_len)
    real*4                 :: PQC(N_LEVELS_len,N_PROF_len)
    real*4                 :: PRF_TEMP_QC(N_PROF_len),PRF_PSAL_QC(N_PROF_len),PRF_PRES_QC(N_PROF_len)
    real*4                 :: POS_PRF_QC(N_PROF_len) 
    
    double precision       :: big_number, infinity

    !print *, '   nlevs nobs ', N_LEVELS_len, N_PROF_len
    miss  = 99999.0
    miss2 = 999.e9
    big_number = 9e20
    infinity = HUGE(big_number)
    negmiss = -10

    T(:,:) = miss2
    S(:,:) = miss2
    P(:,:) = miss2

    TQC(:,:) = miss2
    SQC(:,:) = miss2
    PQC(:,:) = miss2

    PRF_TEMP_QC(:) = miss2
    PRF_PSAL_QC(:) = miss2
    PRF_PRES_QC(:) = miss2

    debug=0


  ! ****************************************************************************
  ! Open Dataset
  ! ............................
    status   = nf_open(trim(fname_in), 0, ncid)
    !print *, 'open status ', status


  ! Inquire about Dataset
  ! .................................
    status = nf_inq_ndims(ncid,ndims)
    status = nf_inq_nvars(ncid,nvars)


  ! Inquire about Dimensions
  ! .....................................................
    status = nf_inq_dimid(ncid,'N_PROF',N_PROF_id)

    status = nf_inq_dimid(ncid,'N_LEVELS',N_LEVELS_id)

    status = nf_inq_dimid(ncid,'STRING1',STRING1_id)
    status = nf_inq_dimlen(ncid,STRING1_id,STRING1_len)

    status = nf_inq_dimid(ncid,'STRING4',STRING4_id)
    status = nf_inq_dimlen(ncid,STRING4_id,STRING4_len)

    status = nf_inq_dimid(ncid,'STRING8',STRING8_id)
    status = nf_inq_dimlen(ncid,STRING8_id,STRING8_len)

    status = nf_inq_dimid(ncid,'STRING64',STRING64_id)     
    status = nf_inq_dimlen(ncid,STRING64_id,STRING64_len)

 
  ! Inquire about Variables
  ! .....................................................................
    status = nf_inq_varid(ncid,'PLATFORM_NUMBER',PLATFORM_NUMBER_id)
    status = nf_inq_varndims(ncid,PLATFORM_NUMBER_id,PLATFORM_NUMBER_dim)
 
    status = nf_inq_varid(ncid,'INST_REFERENCE',INST_REFERENCE_id)     
    status = nf_inq_varndims(ncid,INST_REFERENCE_id,INST_REFERENCE_dim)

    status = nf_inq_varid(ncid,'WMO_INST_TYPE',WMO_INST_TYPE_id)
    status = nf_inq_varndims(ncid,WMO_INST_TYPE_id,WMO_INST_TYPE_dim)

    status = nf_inq_varid(ncid,'DIRECTION',DIRECTION_id)
    status = nf_inq_varndims(ncid,DIRECTION_id,DIRECTION_dim)

    status = nf_inq_varid(ncid,'DATA_MODE',DATA_MODE_id)
    status = nf_inq_varndims(ncid,DATA_MODE_id,DATA_MODE_dim)
    
    status = nf_inq_varid(ncid,'DATA_STATE_INDICATOR',DATA_STATE_INDICATOR_id)
    status = nf_inq_varndims(ncid,DATA_STATE_INDICATOR_id,DATA_STATE_INDICATOR_dim)

    status = nf_inq_varid(ncid,'LATITUDE',LATITUDE_id)
    status = nf_inq_varndims(ncid,LATITUDE_id,LATITUDE_dim)

    status = nf_inq_varid(ncid,'LONGITUDE',LONGITUDE_id)
    status = nf_inq_varndims(ncid,LONGITUDE_id,LONGITUDE_dim)

    status = nf_inq_varid(ncid,'JULD',JULD_id)
    status = nf_inq_varndims(ncid,JULD_id,JULD_dim)

    status = nf_inq_varid(ncid,'POSITION_QC',POSITION_QC_id)
    status = nf_inq_varndims(ncid,POSITION_QC_id,POSITION_QC_dim)

    status = nf_inq_varid(ncid,'PROFILE_PRES_QC',PROFILE_PRES_QC_id)
    status = nf_inq_varndims(ncid,PROFILE_PRES_QC_id,PROFILE_PRES_QC_dim)

    status = nf_inq_varid(ncid,'PROFILE_TEMP_QC',PROFILE_TEMP_QC_id)
    status = nf_inq_varndims(ncid,PROFILE_TEMP_QC_id,PROFILE_TEMP_QC_dim)

    status = nf_inq_varid(ncid,'PROFILE_PSAL_QC',PROFILE_PSAL_QC_id)
    status = nf_inq_varndims(ncid,PROFILE_PSAL_QC_id,PROFILE_PSAL_QC_dim)

!!!!! Adjusted Values   
    status = nf_inq_varid(ncid,'PRES_ADJUSTED',PRES_ADJUSTED_id)
    status = nf_inq_varndims(ncid,PRES_ADJUSTED_id,PRES_ADJUSTED_dim)

    status = nf_inq_varid(ncid,'PRES_ADJUSTED_QC',PRES_ADJUSTED_QC_id)     
    status = nf_inq_varndims(ncid,PRES_ADJUSTED_QC_id,PRES_ADJUSTED_QC_dim)

    status = nf_inq_varid(ncid,'PSAL_ADJUSTED',PSAL_ADJUSTED_id)
    status = nf_inq_varndims(ncid,PSAL_ADJUSTED_id,PSAL_ADJUSTED_dim)

    status = nf_inq_varid(ncid,'PSAL_ADJUSTED_QC',PSAL_ADJUSTED_QC_id)     
    status = nf_inq_varndims(ncid,PSAL_ADJUSTED_QC_id,PSAL_ADJUSTED_QC_dim)

    status = nf_inq_varid(ncid,'TEMP_ADJUSTED',TEMP_ADJUSTED_id)
    status = nf_inq_varndims(ncid,TEMP_ADJUSTED_id,TEMP_ADJUSTED_dim)

    status = nf_inq_varid(ncid,'TEMP_ADJUSTED_QC',TEMP_ADJUSTED_QC_id)     
    status = nf_inq_varndims(ncid,TEMP_ADJUSTED_QC_id,TEMP_ADJUSTED_QC_dim)

!!!!! Real-Time Values   
    status = nf_inq_varid(ncid,'PRES',PRES_id)
    status = nf_inq_varndims(ncid,PRES_id,PRES_dim)

    status = nf_inq_varid(ncid,'PRES_QC',PRES_QC_id)     
    status = nf_inq_varndims(ncid,PRES_QC_id,PRES_QC_dim)

    status = nf_inq_varid(ncid,'PSAL',PSAL_id)
    status = nf_inq_varndims(ncid,PSAL_id,PSAL_dim)

    status = nf_inq_varid(ncid,'PSAL_QC',PSAL_QC_id)     
    status = nf_inq_varndims(ncid,PSAL_QC_id,PSAL_QC_dim)

    status = nf_inq_varid(ncid,'TEMP',TEMP_id)
    status = nf_inq_varndims(ncid,TEMP_id,TEMP_dim)

    status = nf_inq_varid(ncid,'TEMP_QC',TEMP_QC_id)     
    status = nf_inq_varndims(ncid,TEMP_QC_id,TEMP_QC_dim)

  ! Get Variables
  ! ............................................................................
    !print *, 'Get Variables'

    start2 = (/1,1/)
    count2 = (/STRING8_len,N_PROF_len/)
      status = nf_get_vara_text(ncid,PLATFORM_NUMBER_id,start2,count2,PLATFORM_NUMBER)
    count2 = (/STRING64_len,N_PROF_len/)  
      status = nf_get_vara_text(ncid,INST_REFERENCE_id,start2,count2,INST_REFERENCE)
    count2 = (/STRING4_len,N_PROF_len/)  
      status = nf_get_vara_text(ncid,WMO_INST_TYPE_id,start2,count2,WMO_INST_TYPE)
      status = nf_get_vara_text(ncid,DATA_STATE_INDICATOR_id,start2,count2,DATA_STATE_INDICATOR)
  
    start1 = 1
    count1 = N_PROF_len         
      status = nf_get_vara_text(ncid,DIRECTION_id,start1,count1,DIRECTION)
      status = nf_get_vara_text(ncid,DATA_MODE_id,start1,count1,DATA_MODE)        
      status = nf_get_vara_double(ncid,LATITUDE_id,start1,count1,LATITUDE)
      status = nf_get_vara_double(ncid,LONGITUDE_id,start1,count1,LONGITUDE)
      status = nf_get_vara_double(ncid,JULD_id,start1,count1,JULD)

    start1 = 1
    count1 = N_PROF_len
      status = nf_get_vara_text(ncid,POSITION_QC_id,start1,count1,POSITION_QC)
      status = nf_get_vara_text(ncid,PROFILE_PRES_QC_id,start1,count1,PROFILE_PRES_QC)
      status = nf_get_vara_text(ncid,PROFILE_TEMP_QC_id,start1,count1,PROFILE_TEMP_QC)
      status = nf_get_vara_text(ncid,PROFILE_PSAL_QC_id,start1,count1,PROFILE_PSAL_QC)

    start2 = (/1,1/)
    count2 = (/N_LEVELS_len,N_PROF_len/)
      presa_status = nf_get_vara_real(ncid,PRES_ADJUSTED_id,start2,count2,PRES_ADJUSTED)
      psala_status = nf_get_vara_real(ncid,PSAL_ADJUSTED_id,start2,count2,PSAL_ADJUSTED)
      status = nf_get_vara_real(ncid,TEMP_ADJUSTED_id,start2,count2,TEMP_ADJUSTED)

      status = nf_get_vara_text(ncid,PRES_ADJUSTED_QC_id,start2,count2,PRES_ADJUSTED_QC)
      status = nf_get_vara_text(ncid,PSAL_ADJUSTED_QC_id,start2,count2,PSAL_ADJUSTED_QC)
      status = nf_get_vara_text(ncid,TEMP_ADJUSTED_QC_id,start2,count2,TEMP_ADJUSTED_QC)

      pres_status = nf_get_vara_real(ncid,PRES_id,start2,count2,PRES)
      psal_status = nf_get_vara_real(ncid,PSAL_id,start2,count2,PSAL)
      status = nf_get_vara_real(ncid,TEMP_id,start2,count2,TEMP)

      status = nf_get_vara_text(ncid,PRES_QC_id,start2,count2,PRES_QC)
      status = nf_get_vara_text(ncid,PSAL_QC_id,start2,count2,PSAL_QC)
      status = nf_get_vara_text(ncid,TEMP_QC_id,start2,count2,TEMP_QC)

  ! Need to set double precision values to real values (memory problem)
    LAT = LATITUDE
    LON = LONGITUDE
    ! This is not acceptable for JULD since we need the precision for the hours
    JULIANDAY = JULD

    DO i = 1, N_PROF_len
      if (len_trim(POSITION_QC(i)) > 0) then
          read (POSITION_QC(i),*) POS_PRF_QC(i)	  
          if (POSITION_QC(i) == '4' .or. POSITION_QC(i) == '9') then
            !print *, 'Bad Pos: ',POSITION_QC(i), PROFILE_TEMP_QC(i), LON(i), LAT(i)
            LAT(i) = miss2
            LON(i) = miss2
          endif
 
          if (LAT(i) >= miss) then
              LAT(i) = miss2
          endif
          if (LON(i) >= miss) then
              LON(i) = miss2
          endif
      else
        POS_PRF_QC(i) = 0	
      endif
    ENDDO
!print *, 'end 1 loop'

!print *, 'PA_QC ', PRES_ADJUSTED_QC(:,1)
!print *, 'PQC   ', PQC(:,1)
!print *, 'PA    ', PRES_ADJUSTED(:,1)
!print *, 'P     ', P(:,1)
!stop

! Backtrack from end of profile get number of valid levels: NPTS
! ...............................................................
! Pressure
! ............................................................... 

   DO  i = 1,  N_PROF_len 
       DO j =  1, N_LEVELS_len
         if ( PRES(j,i) == 0 ) then
              PRES(j,i) = miss
         endif
         if ( (PRES(j,i)/PRES(j,i)) /= 1 ) then
              PRES(j,i) = miss
         endif        
      ENDDO
    ENDDO

    DO i = 1,  N_PROF_len
        j = N_LEVELS_len  
        if ( PRES(N_LEVELS_len,i) >= miss ) then 
           DO WHILE ( PRES(j,i) >= miss .and. j > 0 )
             j=j-1       
             if (j == 0) exit 
           ENDDO
           NPTS_PR(i) = j
        else
           NPTS_PR(i) = N_LEVELS_len
        endif       
    ENDDO
    
    DO  i = 1,  N_PROF_len 
       DO j =  1, N_LEVELS_len
         if ( PRES_ADJUSTED(j,i) == 0 ) then
              PRES_ADJUSTED(j,i) = miss
         endif
         if ( (PRES_ADJUSTED(j,i)/PRES_ADJUSTED(j,i)) /= 1 ) then
              PRES_ADJUSTED(j,i) = miss
         endif        
      ENDDO
    ENDDO


    DO i = 1,  N_PROF_len
       j = N_LEVELS_len         
       if ( PRES_ADJUSTED(N_LEVELS_len,i) >= miss ) then 
          DO WHILE ( PRES_ADJUSTED(j,i) >= miss .and. j > 0 )
             j=j-1       
             if (j == 0) exit 
          ENDDO
          NPTS_PD(i) = j
       else
          NPTS_PD(i) = N_LEVELS_len
       endif 
    ENDDO 

    DO i = 1,  N_PROF_len
       j = N_LEVELS_len         
       if ( len_trim(PRES_QC(N_LEVELS_len,i)) == 0 ) then 
          DO WHILE ( len_trim(PRES_QC(j,i)) == 0 .and. j > 0 )
             j=j-1       
             if (j == 0) exit 
          ENDDO
          NPTS_PR_QC(i) = j
       else
          NPTS_PR_QC(i) = N_LEVELS_len
       endif
       DO j = 1, NPTS_PR_QC(i)
         if ( PRES_QC(j,i) /= '1' .and.  PRES_QC(j,i) /= '2' .and. PRES_QC(j,i) /= '3' .and.  PRES_QC(j,i) /= '4' .and. &
              PRES_QC(j,i) /= '5' .and.  PRES_QC(j,i) /= '6' .and. PRES_QC(j,i) /= '7' .and.  PRES_QC(j,i) /= '8' .and. &
	      PRES_QC(j,i) /= '9') then 
           PRES_QC(j,i) = '9'
         endif

         if ( len_trim(PRES_QC(j,i)) == 0 ) then 
           PRES_QC(j,i) = '9'
         endif
       ENDDO
    ENDDO

    DO i = 1,  N_PROF_len
       j = N_LEVELS_len         
       if ( len_trim(PRES_ADJUSTED_QC(N_LEVELS_len,i)) == 0 ) then 
          DO WHILE ( len_trim(PRES_ADJUSTED_QC(j,i)) == 0 .and. j > 0 )
             j=j-1       
             if (j == 0) exit 
          ENDDO
          NPTS_PD_QC(i) = j
       else
          NPTS_PD_QC(i) = N_LEVELS_len
       endif
       DO j = 1, NPTS_PD_QC(i)
         if ( PRES_ADJUSTED_QC(j,i) /= '1' .and.  PRES_ADJUSTED_QC(j,i) /= '2' .and. PRES_ADJUSTED_QC(j,i) /= '3' .and.  PRES_ADJUSTED_QC(j,i) /= '4' .and. &
              PRES_ADJUSTED_QC(j,i) /= '5' .and.  PRES_ADJUSTED_QC(j,i) /= '6' .and. PRES_ADJUSTED_QC(j,i) /= '7' .and.  PRES_ADJUSTED_QC(j,i) /= '8' .and. &
	      PRES_ADJUSTED_QC(j,i) /= '9') then 
           PRES_ADJUSTED_QC(j,i) = '9'
         endif
         if ( len_trim(PRES_ADJUSTED_QC(j,i)) == 0 ) then 
           PRES_ADJUSTED_QC(j,i) = '9'
         endif
         !print *, j, PRES_ADJUSTED_QC(j,i)

       ENDDO
    ENDDO
!if (debug==1) print *,'end PRES loop'
!print *, PRES_ADJUSTED(:,1)

! ...............................................................
! TEMP
! ...............................................................

    DO  i = 1,  N_PROF_len 
       DO j =  1, N_LEVELS_len
         if ( TEMP(j,i) == 0 ) then
              TEMP(j,i) = miss
         endif
         if ( (TEMP(j,i)/TEMP(j,i)) /= 1 ) then
              TEMP(j,i) = miss
         endif        
      ENDDO
    ENDDO
    
    DO  i = 1,  N_PROF_len 
       DO j =  1, N_LEVELS_len
         if ( TEMP_ADJUSTED(j,i) == 0 ) then
              TEMP_ADJUSTED(j,i) = miss
         endif
         if ( (TEMP_ADJUSTED(j,i)/TEMP_ADJUSTED(j,i)) /= 1 ) then
              TEMP_ADJUSTED(j,i) = miss
         endif        
      ENDDO
    ENDDO
    
    !DO  i = 1,  N_PROF_len 
     !  DO j =  1, N_LEVELS_len
     !    if ( TEMP_QC(j,i) == 0 ) then
     !         TEMP_QC(j,i) = miss
     !    endif
     !    if ( (TEMP_QC(j,i)/TEMP_QC(j,i)) /= 1 ) then
     !         TEMP_QC(j,i) = miss
     !    endif        
     ! ENDDO
    !ENDDO
    
    DO i = 1,  N_PROF_len
       j = N_LEVELS_len         
       if ( TEMP(N_LEVELS_len,i) >= miss ) then 
          DO WHILE ( TEMP(j,i) >= miss .and. j > 0 )
             j=j-1     
             if (j == 0) exit 
          ENDDO
          NPTS_TR(i) = j
       else
          NPTS_TR(i) = N_LEVELS_len
       endif
    ENDDO
    
    DO i = 1,  N_PROF_len
       j = N_LEVELS_len               
       if ( TEMP_ADJUSTED(N_LEVELS_len,i) >= miss ) then 
          DO WHILE ( TEMP_ADJUSTED(j,i) >= miss .and. j > 0 )
             j=j-1      
             if (j == 0) exit 
          ENDDO
          NPTS_TD(i) = j
       else
          NPTS_TD(i) = N_LEVELS_len
       endif
    ENDDO
    

    DO i = 1,  N_PROF_len
       j = N_LEVELS_len         
       if ( len_trim(TEMP_QC(N_LEVELS_len,i)) == 0 ) then 
          DO WHILE ( len_trim(TEMP_QC(j,i)) == 0 .and. j > 0 )
             j=j-1       
             if (j == 0) exit 
          ENDDO
          NPTS_TR_QC(i) = j
       else
          NPTS_TR_QC(i) = N_LEVELS_len
       endif

       if ( NPTS_TR_QC(i)==0 ) then
	  TEMP_QC(:,i) = '9'
       endif

       DO j = 1, NPTS_TR_QC(i)
         if ( TEMP_QC(j,i) /= '1' .and.  TEMP_QC(j,i) /= '2' .and. TEMP_QC(j,i) /= '3' .and.  TEMP_QC(j,i) /= '4' .and. &
              TEMP_QC(j,i) /= '5' .and.  TEMP_QC(j,i) /= '6' .and. TEMP_QC(j,i) /= '7' .and.  TEMP_QC(j,i) /= '8' .and. &
	      TEMP_QC(j,i) /= '9') then 
              TEMP_QC(j,i) = '9'
         endif

         if ( len_trim(TEMP_QC(j,i)) == 0 ) then 
           TEMP_QC(j,i) = '9'
         endif
       ENDDO
    ENDDO

    DO i = 1,  N_PROF_len
       j = N_LEVELS_len         
       if ( len_trim(TEMP_ADJUSTED_QC(N_LEVELS_len,i)) == 0 ) then 
          DO WHILE ( len_trim(TEMP_ADJUSTED_QC(j,i)) == 0 .and. j > 0 )
             j=j-1       
             if (j == 0) exit 
          ENDDO
          NPTS_TD_QC(i) = j
       else
          NPTS_TD_QC(i) = N_LEVELS_len
       endif

       if ( NPTS_TD_QC(i)==0 ) then
	  TEMP_ADJUSTED_QC(:,i) = '9'
       endif

       DO j = 1, NPTS_TD_QC(i)
         if ( TEMP_ADJUSTED_QC(j,i) /= '1' .and.  TEMP_ADJUSTED_QC(j,i) /= '2' .and. TEMP_ADJUSTED_QC(j,i) /= '3' .and.  TEMP_ADJUSTED_QC(j,i) /= '4' .and. &
              TEMP_ADJUSTED_QC(j,i) /= '5' .and.  TEMP_ADJUSTED_QC(j,i) /= '6' .and. TEMP_ADJUSTED_QC(j,i) /= '7' .and.  TEMP_ADJUSTED_QC(j,i) /= '8' .and. &
	      TEMP_ADJUSTED_QC(j,i) /= '9') then 
              TEMP_ADJUSTED_QC(j,i) = '9'
         endif

         if ( len_trim(TEMP_ADJUSTED_QC(j,i)) == 0 ) then 
           TEMP_ADJUSTED_QC(j,i) = '9'
         endif

       ENDDO
    ENDDO
if (debug==1) print *, 'end TEMP loop' 
!print *, '1 ',PRES_ADJUSTED(1,1)

! ...............................................................
! PSAL
! ...............................................................
  ! Fix for NaNs
  if (psal_status >= 0) then
    DO  i = 1,  N_PROF_len 
       DO j =  1, N_LEVELS_len
         !print *, i, j, PSAL(j,i)
         if ( PSAL(j,i) == 0 ) then
              PSAL(j,i) = miss
         endif
         if ( (PSAL(j,i)/PSAL(j,i)) /= 1 ) then
              PSAL(j,i) = miss
         endif        
      ENDDO
    ENDDO
  
  
    DO  i = 1,  N_PROF_len 
       j = N_LEVELS_len     
       if ( PSAL(N_LEVELS_len,i) >= miss ) then              
          DO WHILE ( PSAL(j,i) >= miss .and. j > 0 )
             j=j-1       
             if (j == 0) exit 
          ENDDO
          NPTS_SR(i) = j
       else
          NPTS_SR(i) = N_LEVELS_len
       endif
    ENDDO
  else
    NPTS_SR(:) = 0
  endif
 
   ! Fix for NaNs
   ! i = 73 has -infinity
   if (psala_status >= 0) then
    DO  i = 1,  N_PROF_len 
    !DO  i = 73,73
       DO j =  1, N_LEVELS_len
         !print *, i, j, PSAL_ADJUSTED(j,i)       
         
         if ( PSAL_ADJUSTED(j,i) == 0 ) then
              PSAL_ADJUSTED(j,i) = miss
         endif
         
         ! if it is NOT NaN, check for infinity
         if (PSAL_ADJUSTED(j,i) == PSAL_ADJUSTED(j,i)) then
            if ( PSAL_ADJUSTED(j,i) < -infinity ) then
               PSAL_ADJUSTED(j,i) = miss
            endif
         endif
         if (PSAL_ADJUSTED(j,i) /= PSAL_ADJUSTED(j,i)) then          
               PSAL_ADJUSTED(j,i) = miss           
         endif
         
         ! If it is NOT infinity, check for NaN
         if ( PSAL_ADJUSTED(j,i) > -infinity ) then
            if (PSAL_ADJUSTED(j,i) /= PSAL_ADJUSTED(j,i)) then
               PSAL_ADJUSTED(j,i) = miss
            endif
         endif
         
         
         !if (PSAL_ADJUSTED(j,i) < -infinity) then
         !    PSAL_ADJUSTED(j,i) = miss
         !endif
      
      ENDDO
    ENDDO
   endif

  if (psala_status >= 0) then
    DO i = 1,  N_PROF_len
       j = N_LEVELS_len                  
       if ( PSAL_ADJUSTED(N_LEVELS_len,i) >= miss ) then 
           DO WHILE ( (PSAL_ADJUSTED(j,i) >= miss .or. PSAL_ADJUSTED(j,i)<=negmiss) .and. j > 0 )
             j=j-1       
             if (j == 0) exit 
          ENDDO
          NPTS_SD(i) = j
       else
          NPTS_SD(i) = 0
       endif
   ENDDO
  else
    NPTS_SD(:) = 0
  endif
!print *, '3 ',PRES_ADJUSTED(1,1)

  if (psal_status >= 0) then
    DO i = 1,  N_PROF_len
       j = N_LEVELS_len         
       if ( len_trim(PSAL_QC(N_LEVELS_len,i)) == 0 ) then 
          DO WHILE ( len_trim(PSAL_QC(j,i)) == 0 .and. j > 0 )
             j=j-1       
             if (j == 0) exit 
          ENDDO
          NPTS_SR_QC(i) = j
       else
          NPTS_SR_QC(i) = N_LEVELS_len
       endif
       DO j = 1, NPTS_SR_QC(i)
         if ( PSAL_QC(j,i) /= '1' .and.  PSAL_QC(j,i) /= '2' .and. PSAL_QC(j,i) /= '3' .and.  PSAL_QC(j,i) /= '4' .and. &
              PSAL_QC(j,i) /= '5' .and.  PSAL_QC(j,i) /= '6' .and. PSAL_QC(j,i) /= '7' .and.  PSAL_QC(j,i) /= '8' .and. &
              PSAL_QC(j,i) /= '9') then 
              PSAL_QC(j,i) = '9'
         endif

         if ( len_trim(PSAL_QC(j,i)) == 0 ) then 
           PSAL_QC(j,i) = '9'
         endif
       ENDDO
    ENDDO
  else
    NPTS_SR_QC(:) = 0
    PSAL_QC(:,:) = '9'
  endif
!print *, '4 ',PRES_ADJUSTED(1,1)

!print *, psala_status

  if (psala_status >= 0) then
    DO i = 1,  N_PROF_len
       j = N_LEVELS_len         
       if ( len_trim(PSAL_ADJUSTED_QC(N_LEVELS_len,i)) == 0 ) then 
          DO WHILE ( len_trim(PSAL_ADJUSTED_QC(j,i)) == 0 .and. j > 0 )
             j=j-1       
             if (j == 0) exit 
          ENDDO
          NPTS_SD_QC(i) = j
       else
          NPTS_SD_QC(i) = N_LEVELS_len
       endif
       DO j = 1, NPTS_SD_QC(i)
         if ( PSAL_ADJUSTED_QC(j,i) /= '1' .and.  PSAL_ADJUSTED_QC(j,i) /= '2' .and. PSAL_ADJUSTED_QC(j,i) /= '3' .and.  PSAL_ADJUSTED_QC(j,i) /= '4' .and. &
              PSAL_ADJUSTED_QC(j,i) /= '5' .and.  PSAL_ADJUSTED_QC(j,i) /= '6' .and. PSAL_ADJUSTED_QC(j,i) /= '7' .and.  PSAL_ADJUSTED_QC(j,i) /= '8' .and. &
	      PSAL_ADJUSTED_QC(j,i) /= '9') then 
              PSAL_ADJUSTED_QC(j,i) = '9'
         endif

         if ( len_trim(PSAL_ADJUSTED_QC(j,i)) == 0 ) then 
           PSAL_ADJUSTED_QC(j,i) = '9'
         endif
       ENDDO
    ENDDO
  else
    NPTS_SD_QC(:) = 0
    PSAL_ADJUSTED_QC(:,:) = '9'
  endif

!print *, '5 ',PRES_ADJUSTED(1,1)

!print *, 'end PSAL loop'
!print *, '6 ',PRES_ADJUSTED(1,1)

! Profile QC are letters A to F, deny D,E,F
! Level QC are numbers 0 to 9, deny 4 and 9
! ...............................................................
! Decide to use either R (realtime) or D (delayed mode) data
! ...............................................................
    DO i = 1,N_PROF_len 
       
       if (debug==1) print *, i, DATA_MODE(i), NPTS_TR(i), NPTS_TD(i),NPTS_SR(i), NPTS_SD(i)
 
       if (DATA_MODE(i) == 'R') then
        !.............................................................
	! TEMP! len_trim(TEMP_QC(N_LEVELS_len,i)) == 0
        !.............................................................
          if (NPTS_TR(i) > 0) then
             !print *, 'NPTS_TR(i) > 0'
             T(:,i)   = TEMP(:,i)
             read (TEMP_QC(1:NPTS_TR(i),i),*) TQC(1:NPTS_TR(i),i)	 
	     NPTS_T(i) = NPTS_TR(i)
	     DATA_MODE_T(i) = 'R'
	     PRF_TEMP_QC(i) = 1	
             if (PROFILE_TEMP_QC(i) == '4' .or. PROFILE_TEMP_QC(i) == 'D' .or. &
                 PROFILE_TEMP_QC(i) == 'E' .or. PROFILE_TEMP_QC(i) == 'F' .or. &
                 PROFILE_TEMP_QC(i) == '9' .or. PROFILE_TEMP_QC(i) == 'C' ) then	
	        PRF_TEMP_QC(i) = -1
	        T(:,i) = miss2
	        TQC(:,i) = -1		
	        DATA_MODE_T(i) = 'M'
	        NPTS_T(i) = 0	
             endif    
	  else
	     T(:,i) = miss2
	     TQC(:,i) = -1
	     DATA_MODE_T(i) = 'M'
	     NPTS_T(i) = 0  
	     PRF_TEMP_QC(i) = -1 
	  endif

        !.............................................................
	! SALT!
        !.............................................................
          if (NPTS_SR(i) > 0 .and. psal_status==0) then	  
             !print *, 'NPTS_SR(i) > 0'
             S(:,i)   = PSAL(:,i)
             read (PSAL_QC(1:NPTS_SR(i),i),*) SQC(1:NPTS_SR(i),i)	     
	     NPTS_S(i) = NPTS_SR(i)
	     DATA_MODE_S(i) = 'R'
	     PRF_PSAL_QC(i) = 1	
             if (PROFILE_PSAL_QC(i) == '4' .or. PROFILE_PSAL_QC(i) == 'D' .or. &
                 PROFILE_PSAL_QC(i) == 'E' .or. PROFILE_PSAL_QC(i) == 'F' .or. &
                 PROFILE_PSAL_QC(i) == '9' .or. PROFILE_PSAL_QC(i) == 'C' ) then	
	        PRF_PSAL_QC(i) = -1
	        S(:,i) = miss2
	        SQC(:,i) = -1		
	        DATA_MODE_S(i) = 'M'
	        NPTS_S(i) = 0	
             endif    
	  else
	     S(:,i) = miss2
	     SQC(:,i) = -1		
	     DATA_MODE_S(i) = 'M'
	     NPTS_S(i) = 0	
	     PRF_PSAL_QC(i) = -1	          	  
	  endif

        !.............................................................
	! PRESSURE!
        !.............................................................
          if (NPTS_PR(i) > 0) then
             !print *, 'NPTS_PR(i) > 0'
             P(:,i)   = PRES(:,i)
	     PRF_PRES_QC(i) = 1	
             read (PRES_QC(1:NPTS_PR(i),i),*) PQC(1:NPTS_PR(i),i)	     
             if (PROFILE_PRES_QC(i) == '4' .or. PROFILE_PRES_QC(i) == 'D' .or. &
                 PROFILE_PRES_QC(i) == 'E' .or. PROFILE_PRES_QC(i) == 'F' .or. &
                 PROFILE_PRES_QC(i) == '9' .or. PROFILE_PRES_QC(i) == 'C' ) then	
	        PRF_PRES_QC(i) = -1
	        P(:,i) = miss2
	        PQC(:,i) = -1		
	        DATA_MODE_S(i) = 'M'
	        DATA_MODE_T(i) = 'M'	     
	        NPTS_S(i) = 0
	        NPTS_T(i) = 0	     	
	        PRF_PSAL_QC(i) = -1	          	  
	        PRF_TEMP_QC(i) = -1		     
	        SQC(:,i) = -1   
             endif    
	  else  
	     P(:,i)   = miss2	
	     PQC(:,i) = -1
	     PRF_PRES_QC(i) = -1	
	     DATA_MODE_S(i) = 'M'
	     DATA_MODE_T(i) = 'M'	     
	     NPTS_S(i) = 0
	     NPTS_T(i) = 0	     	
	     PRF_PSAL_QC(i) = -1	          	  
	     PRF_TEMP_QC(i) = -1		     
	     SQC(:,i) = -1   
	     TQC(:,i) = -1 	      
     	  	          
	  endif
       if (debug==1) print *, '     end DATAMODE R ', DATA_MODE(i)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Sometimes it says 'D' but the adjuste values are missing, then we need to use 'R'
       elseif (DATA_MODE(i) == 'D') then
        !print *, 'TA ',TEMP_ADJUSTED_QC(1,i)
        !print *, 'SA ',PSAL_ADJUSTED_QC(1,i)
        !print *, 'PA ',PRES_ADJUSTED_QC(1,i)
        !.............................................................
	! TEMP!
        !.............................................................
          if (NPTS_TD(i) > 0) then
             !print *, 'NPTS_TD(i) > 0'
	     !print *, TEMP_ADJUSTED_QC(1:5,i)
             T(:,i)   = TEMP_ADJUSTED(:,i)
             read (TEMP_ADJUSTED_QC(1:NPTS_TD(i),i),*) TQC(1:NPTS_TD(i),i)	
	     NPTS_T(i) = NPTS_TD(i)
	     DATA_MODE_T(i) = 'D'
	     PRF_TEMP_QC(i) = 1	
             if (PROFILE_TEMP_QC(i) == '4' .or. PROFILE_TEMP_QC(i) == 'D' .or. &
                 PROFILE_TEMP_QC(i) == 'E' .or. PROFILE_TEMP_QC(i) == 'F' .or. &
                 PROFILE_TEMP_QC(i) == '9' .or. PROFILE_TEMP_QC(i) == 'C' ) then	
	        PRF_TEMP_QC(i) = -1
	        T(:,i) = miss2
	        TQC(:,i) = -1		
	        DATA_MODE_T(i) = 'M'
	        NPTS_T(i) = 0	
             endif  
         
           ! if (len_trim(TEMP_QC(N_LEVELS_len,i)) == 0)
           elseif (NPTS_TR(i) > 0 .and. len_trim(PROFILE_TEMP_QC(i)) >0) then
             !print *, 'NPTS_TR(i) > 0'
             T(:,i)   = TEMP(:,i)
             read (TEMP_QC(1:NPTS_TR(i),i),*) TQC(1:NPTS_TR(i),i)	     
	     NPTS_T(i) = NPTS_TR(i)
	     DATA_MODE_T(i) = 'R'
	     PRF_TEMP_QC(i) = 1	
             if (PROFILE_TEMP_QC(i) == '4' .or. PROFILE_TEMP_QC(i) == 'D' .or. &
                 PROFILE_TEMP_QC(i) == 'E' .or. PROFILE_TEMP_QC(i) == 'F' .or. &
                 PROFILE_TEMP_QC(i) == '9' .or. PROFILE_TEMP_QC(i) == 'C' ) then	
	        PRF_TEMP_QC(i) = -1
	        T(:,i) = miss2
	        TQC(:,i) = -1		
	        DATA_MODE_T(i) = 'M'
	        NPTS_T(i) = 0	
             endif   
	  else
	     T(:,i) = miss2
	     TQC(:,i) = -1
	     DATA_MODE_T(i) = 'M'
	     NPTS_T(i) = 0  
	     PRF_TEMP_QC(i) =-1 
	  endif
          if (debug==1) print *,'     T done'

                 
        !.............................................................
	! SALT !
        !.............................................................
         ! if (NPTS_SD(i) > 0 .and. len_trim(PROFILE_PSAL_QC(i)) > 0 .and. &
         !     ichar(PSAL_ADJUSTED_QC(NPTS_SD(i),i)) > 0 .and. psala_status==0) then
          if (NPTS_SD(i) > 0 .and. len_trim(PROFILE_PSAL_QC(i)) > 0 .and. psala_status==0) then    	  
             !print *, 'NPTS_SD(i) > 0: ', NPTS_SD(i)
             S(:,i)   = PSAL_ADJUSTED(:,i)
             read (PSAL_ADJUSTED_QC(1:NPTS_SD(i),i),*) SQC(1:NPTS_SD(i),i)
	     NPTS_S(i) = NPTS_SD(i)
	     DATA_MODE_S(i) = 'D'
	     PRF_PSAL_QC(i) = 1	
             if (PROFILE_PSAL_QC(i) == '4' .or. PROFILE_PSAL_QC(i) == 'D' .or. &
                 PROFILE_PSAL_QC(i) == 'E' .or. PROFILE_PSAL_QC(i) == 'F' .or. &
                 PROFILE_PSAL_QC(i) == '9' .or. PROFILE_PSAL_QC(i) == 'C' ) then	
	        PRF_PSAL_QC(i) = -1
	        S(:,i) = miss2
	        SQC(:,i) = -1		
	        DATA_MODE_S(i) = 'M'
	        NPTS_S(i) = 0	
             endif    
          elseif (NPTS_SR(i) > 0 .and. len_trim(PROFILE_PSAL_QC(i)) >0) then
             !print *, 'NPTS_SR(i) > 0'
             S(:,i)   = PSAL(:,i)
             read (PSAL_QC(1:NPTS_SR(i),i),*) SQC(1:NPTS_SR(i),i)	     
	     NPTS_S(i) = NPTS_SR(i)
	     DATA_MODE_S(i) = 'R'
	     PRF_PSAL_QC(i) = 1	
             if (PROFILE_PSAL_QC(i) == '4' .or. PROFILE_PSAL_QC(i) == 'D' .or. &
                 PROFILE_PSAL_QC(i) == 'E' .or. PROFILE_PSAL_QC(i) == 'F' .or. &
                 PROFILE_PSAL_QC(i) == '9' .or. PROFILE_PSAL_QC(i) == 'C') then	
	        PRF_PSAL_QC(i) = -1
	        S(:,i) = miss2
	        SQC(:,i) = -1		
	        DATA_MODE_S(i) = 'M'
	        NPTS_S(i) = 0	
             endif    
	  else
	     S(:,i) = miss2
	     SQC(:,i) = -1		
	     DATA_MODE_S(i) = 'M'
	     NPTS_S(i) = 0	
	     PRF_PSAL_QC(i) = -1	          	  
	  endif
          if (debug==1) print *, '     S done' 

        !.............................................................
	! PRESSURE !
        !.............................................................
          if (NPTS_PD(i) > 0 .and. len_trim(PRES_ADJUSTED_QC(1,i)) >0) then
             !print *, 'NPTS_PD(i) > 0: ',PROFILE_PRES_QC(i)
	     !print *, P(1,i)
	     !print *, PRES(1,i)
	     !print *, PRES_ADJUSTED(1,i)
             P(1:NPTS_PD(i),i)   = PRES_ADJUSTED(1:NPTS_PD(i),i)
             read (PRES_ADJUSTED_QC(1:NPTS_PD(i),i),*) PQC(1:NPTS_PD(i),i)
	     PRF_PRES_QC(i) = 1	

             if (PROFILE_PRES_QC(i) == '4' .or. PROFILE_PRES_QC(i) == 'D' .or. &
                 PROFILE_PRES_QC(i) == 'E' .or. PROFILE_PRES_QC(i) == 'F' .or. &
                 PROFILE_PRES_QC(i) == '9' .or. PROFILE_PRES_QC(i) == 'C' ) then	
	        PRF_PRES_QC(i) = -1
	        P(:,i) = miss2
	        PQC(:,i) = -1	
	        DATA_MODE_S(i) = 'M'
	        DATA_MODE_T(i) = 'M'	     
	        NPTS_S(i) = 0
	        NPTS_T(i) = 0	     	
	        PRF_PSAL_QC(i) = -1	          	  
	        PRF_TEMP_QC(i) = -1		     
	        SQC(:,i) = -1   
	        TQC(:,i) = -1 	            	  	          
             endif  

          elseif (NPTS_PR(i) > 0) then
             !print *, 'NPTS_PR(i) > 0'
             P(:,i)   = PRES(:,i)
             read (PRES_QC(1:NPTS_PR(i),i),*) PQC(1:NPTS_PR(i),i)	     
	     PRF_PRES_QC(i) = 1	
             if (PROFILE_PRES_QC(i) == '4' .or. PROFILE_PRES_QC(i) == 'D' .or. &
                 PROFILE_PRES_QC(i) == 'E' .or. PROFILE_PRES_QC(i) == 'F' .or. &
                 PROFILE_PRES_QC(i) == '9' .or. PROFILE_PRES_QC(i) == 'C' ) then	
	        PRF_PRES_QC(i) = -1
	        P(:,i) = miss2
	        PQC(:,i) = -1	
	        DATA_MODE_S(i) = 'M'
	        DATA_MODE_T(i) = 'M'	     
	        NPTS_S(i) = 0
	        NPTS_T(i) = 0	     	
	        PRF_PSAL_QC(i) = -1	          	  
	        PRF_TEMP_QC(i) = -1		     
	        SQC(:,i) = -1   
	        TQC(:,i) = -1 	            	  	          
             endif   
	  else  
             !print *, 'ELSE'
 	     P(:,i)   = miss2	
	     PQC(:,i) = -1
	     PRF_PRES_QC(i) = -1	
	     DATA_MODE_S(i) = 'M'
	     DATA_MODE_T(i) = 'M'	     
	     NPTS_S(i) = 0
	     NPTS_T(i) = 0	     	
	     PRF_PSAL_QC(i) = -1	          	  
	     PRF_TEMP_QC(i) = -1		     
	     SQC(:,i) = -1   
	     TQC(i:,i) = -1 	            	  	          
	  endif
          if (debug==1) print *, '     P done'

       !if (debug==1) print *, '     end DATAMODE D'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       elseif (DATA_MODE(i) == 'A') then
	  DATA_MODE_T(i) = 'A'
	  DATA_MODE_S(i) = 'A'
          if (NPTS_TD(i) > 0) then
 	     !print *, 'TD'
             T(:,i)   = TEMP_ADJUSTED(:,i)
             read (TEMP_ADJUSTED_QC(1:NPTS_TD(i),i),*) TQC(1:NPTS_TD(i),i)	     
	     NPTS_T(i) = NPTS_TD(i)
	     PRF_TEMP_QC(i) = 1
             if (PROFILE_TEMP_QC(i) == '4' .or. PROFILE_TEMP_QC(i) == 'D' .or. &
                 PROFILE_TEMP_QC(i) == 'E' .or. PROFILE_TEMP_QC(i) == 'F' .or. &
                 PROFILE_TEMP_QC(i) == '9' .or. PROFILE_TEMP_QC(i) == 'C' ) then	
	        PRF_TEMP_QC(i) = -1
	        T(:,i) = miss2
	        TQC(:,i) = -1		
	        DATA_MODE_T(i) = 'M'
	        NPTS_T(i) = 0		
             endif    
	  elseif (NPTS_TR(i) > 0) then
 	     !print *, 'TR'
             T(:,i)   = TEMP(:,i)
             read (TEMP_QC(1:NPTS_TR(i),i),*) TQC(1:NPTS_TR(i),i)
	     NPTS_T(i) = NPTS_TR(i)
	     PRF_TEMP_QC(i) = 1	
             if (PROFILE_TEMP_QC(i) == '4' .or. PROFILE_TEMP_QC(i) == 'D' .or. &
                 PROFILE_TEMP_QC(i) == 'E' .or. PROFILE_TEMP_QC(i) == 'F' .or. &
                 PROFILE_TEMP_QC(i) == '9' .or. PROFILE_TEMP_QC(i) == 'C' ) then	
	        PRF_TEMP_QC(i) = -1
	        T(:,i) = miss2
	        TQC(:,i) = -1		
	        DATA_MODE_T(i) = 'M'
	        NPTS_T(i) = 0		
             endif    
	  else
             T(:,i)   = miss2
             TQC(:,i) = -1		  
	     NPTS_T(i) = 0
	     DATA_MODE_T(i) = 'M'
	     PRF_TEMP_QC(i) = -1		       
	  endif 
          
	  if (NPTS_SD(i) > 0) then	      
 	     !print *, 'SD'
             S(:,i)   = PSAL_ADJUSTED(:,i)       
             read (PSAL_ADJUSTED_QC(1:NPTS_SD(i),i),*) SQC(1:NPTS_SD(i),i)	     
	     NPTS_S(i) = NPTS_SD(i)
	     PRF_PSAL_QC(i) = 1
	  elseif (NPTS_SR(i) > 0) then
 	     !print *, 'SR'
             S(:,i)   = PSAL(:,i)          
             read (PSAL_QC(1:NPTS_SR(i),i),*) SQC(1:NPTS_SR(i),i)	     
	     NPTS_S(i) = NPTS_SR(i)
             PRF_PSAL_QC(i) = 1
             if (PROFILE_PSAL_QC(i) == '4' .or. PROFILE_PSAL_QC(i) == 'D' .or. &
                 PROFILE_PSAL_QC(i) == 'E' .or. PROFILE_PSAL_QC(i) == 'F' .or. &
                 PROFILE_PSAL_QC(i) == '9' .or. PROFILE_PSAL_QC(i) == 'C' ) then	
	        PRF_PSAL_QC(i) = -1
	        S(:,i) = miss2
	        SQC(:,i) = -1		
	        DATA_MODE_S(i) = 'M'
	        NPTS_S(i) = 0	
             endif    
	  else
	     S(:,i)   = miss2
             SQC(:,i) = -1          
	     NPTS_S(i) = 0
	     DATA_MODE_S(i) = 'M'
	     PRF_PSAL_QC(i) = -1  
	  endif
 
	  if (NPTS_PD(i) > 0) then	  
	     !print *, 'PD'
 	     P(:,i)   = PRES_ADJUSTED(:,i)
 	     PRES_ADJUSTED_QC(1:NPTS_PD(i),i) = adjustl(PRES_ADJUSTED_QC(1:NPTS_PD(i),i))
             read (PRES_ADJUSTED_QC(1:NPTS_PD(i),i),*) PQC(1:NPTS_PD(i),i)
	     PRF_PRES_QC(i) = 1
	  elseif (NPTS_PR(i) > 0) then
	     !print *, 'PR'
	     P(:,i)   = PRES(:,i)	  
             read (PRES_QC(1:NPTS_PR(i),i),*) PQC(1:NPTS_PR(i),i)	
	     PRF_PRES_QC(i) = 1
	  else
	     P(:,i)   = miss2
             PQC(:,i) = -1
	     PRF_PRES_QC(i) = -1
	     T(:,i)   = miss2
	     TQC(:,i) = -1 
	     PRF_TEMP_QC(i) = -1
	     S(:,i)   = miss2
	     SQC(:,i) = -1 
	     PRF_PSAL_QC(i) = -1	
  	     NPTS_T(i) = 0
  	     NPTS_S(i) = 0
	  endif  
       if (debug==1) print *, '     end DATAMODE A'  
       endif

       NPTS(i) = NPTS_T(i)
       !print *, 'NPTS line 1470 ', NPTS(i)


    ! Check each level QC, if 4 or 9, change to -1
      DO j=1,NPTS(i)
        if (TQC(j,i) == 4 .or. TQC(j,i) == 9) then
          TQC(j,i) = -1
	  T(j,i)   = miss2
          SQC(j,i) = -1
	  S(j,i)   = miss2
        endif
        if (SQC(j,i) == 4 .or. SQC(j,i) == 9) then
          SQC(j,i) = -1
	  S(j,i)   = miss2
        endif
        if (PQC(j,i) == 4 .or. PQC(j,i) == 9) then
          PQC(j,i) = -1
	  P(j,i)   = miss2
        endif
        
        ! Check for NaNs
         if ( T(j,i) == 0 ) then
              T(j,i) = miss2
         endif
        if ( (T(j,i)/T(j,i)) /= 1 ) then
              T(j,i) = miss2
              TQC(j,i) = -1
        endif     
        if ( S(j,i) == 0 ) then
             S(j,i) = miss
         endif   
        if ( (S(j,i)/S(j,i)) /= 1 ) then
              S(j,i) = miss2
              SQC(j,i) = -1
        endif  
        
        ! Check min and max values
        if (T(j,i) < Tmin .or. T(j,i) > Tmax) then
          TQC(j,i) = -1
	  T(j,i)   = miss2
        endif
        if (S(j,i) < Smin .or. S(j,i) > Smax) then
          SQC(j,i) = -1
	  S(j,i)   = miss2
        endif
      ENDDO
      !print *, 'end loop 1 1498'

      DO j=NPTS(i)+1,N_LEVELS_len
        T(j,i) = miss2
        S(j,i) = miss2
        P(j,i) = miss2
        TQC(j,i) = 9
        SQC(j,i) = 9
        PQC(j,i) = 9
      ENDDO
      !print *, 'end loop 2 1508'

      !print *, i, NPTS_T(i), maxval(T(1:NPTS_T(i),i),mask=T(1:NPTS_T(i),i)/=miss2)
      !print *, i, DATA_MODE_T(i),NPTS_T(i),PRF_TEMP_QC(i)
      !print *, i,DATA_MODE_T(i),DATA_MODE_S(i)
    ENDDO
    !print *, 'done ', i
    status = nf_close(ncid)
    !print *, '   END OF MODULE ', shape(S)
    !print *, S
    
  return
  END SUBROUTINE read_argo_ts

  ! ..................................................................................
    SUBROUTINE read_argo_pid (fname_in,N_PROF_len,N_LEVELS_len,PLATFORM_NUMBER)
  ! ..................................................................................
    IMPLICIT NONE
    INCLUDE 'netcdf.inc'

  ! Input Variables
    character*256, intent(in) :: fname_in
    character*3               :: basin
    character*8               :: sdate
    integer,       intent(in) :: N_PROF_len, N_LEVELS_len

  ! Define Dataset
    integer :: ncid, status, i, j, psal_status, psala_status
    integer :: ndims, nvars, ngatts, unlimdimid
    integer :: start1, count1
    integer :: start2(2), count2(2)

  ! Define Dimensions
    integer :: N_PROF_id
    integer :: N_LEVELS_id
    integer :: STRING1_id,   STRING1_len
    integer :: STRING4_id,   STRING4_len
    integer :: STRING8_id,   STRING8_len
    integer :: STRING64_id,  STRING64_len

  ! Define Variables
    integer              :: PLATFORM_NUMBER_id, PLATFORM_NUMBER_dim
    character*8          :: PLATFORM_NUMBER(N_PROF_len)


  ! ****************************************************************************
  ! Open Dataset
  ! ............................
    status   = nf_open(trim(fname_in), 0, ncid)

  ! Inquire about Dataset
  ! .................................
    status = nf_inq_ndims(ncid,ndims)
    status = nf_inq_nvars(ncid,nvars)


  ! Inquire about Dimensions
  ! .....................................................
    status = nf_inq_dimid(ncid,'N_PROF',N_PROF_id)

    status = nf_inq_dimid(ncid,'N_LEVELS',N_LEVELS_id)

    status = nf_inq_dimid(ncid,'STRING1',STRING1_id)
    status = nf_inq_dimlen(ncid,STRING1_id,STRING1_len)

    status = nf_inq_dimid(ncid,'STRING4',STRING4_id)
    status = nf_inq_dimlen(ncid,STRING4_id,STRING4_len)

    status = nf_inq_dimid(ncid,'STRING8',STRING8_id)
    status = nf_inq_dimlen(ncid,STRING8_id,STRING8_len)

    status = nf_inq_dimid(ncid,'STRING64',STRING64_id)     
    status = nf_inq_dimlen(ncid,STRING64_id,STRING64_len)

 
  ! Inquire about Variables
  ! .....................................................................
    status = nf_inq_varid(ncid,'PLATFORM_NUMBER',PLATFORM_NUMBER_id)
    status = nf_inq_varndims(ncid,PLATFORM_NUMBER_id,PLATFORM_NUMBER_dim)
 
  ! Get Variables
  ! ............................................................................
    start2 = (/1,1/)
    count2 = (/STRING8_len,N_PROF_len/)
      status = nf_get_vara_text(ncid,PLATFORM_NUMBER_id,start2,count2,PLATFORM_NUMBER)

    basin = fname_in(57:59)
    sdate = fname_in(69:77) 

    !print *, trim(fname_in)
    !print *, basin
    !print *, sdate
 
    DO i = 1, N_PROF_len
      if (len_trim(PLATFORM_NUMBER(i)) > 0 .and. ichar(PLATFORM_NUMBER(i)(1:1)) > 0) then
          !print *, i, len_trim(PLATFORM_NUMBER(i)), ichar(PLATFORM_NUMBER(i)),PLATFORM_NUMBER(i)
      else
          !print *, 'no wmo',trim(fname_in),i,PLATFORM_NUMBER(i)
      endif
    ENDDO
    

    status = nf_close(ncid)
    
  return
  END SUBROUTINE read_argo_pid

END MODULE ARGO_NETCDF_MODULE
