#!/bin/csh -fv

#   oda_run.j output goes to eh019.o*

#Stuff to do: Test without -f ...

limit stacksize unlimited
setenv I_MPI_DAPL_UD enable

####################################################################### 
#                  Forecast and run Ocean Observer
#######################################################################                                                       
set NX = ${1}
set NY = ${2}

#Save date of analysis            
#---------------------                                                                                                     
set yyyy  = `cat cap_restart | cut -c1-4`
set mm    = `cat cap_restart | cut -c5-6`
set dd    = `cat cap_restart | cut -c7-8`
set hh    = `cat cap_restart | cut -c10-11`

cp -f $SCRDIR/cap_restart $SCRDIR/cap_restart_ana
echo "CAP RESTART 1"
more cap_restart
pwd
echo "CAP RESTART 1"

if (! -e ${EXPDIR}/ocean_das ) mkdir -p ${EXPDIR}/ocean_das 
setenv DA_seq  1   #Obsolete ... need to remove ...
setenv ANADIR  oana-${yyyy}${mm}${dd}_${hh}
mkdir ${EXPDIR}/ocean_das/$ANADIR

#Prepare MERRA-2 SST
#-------------------
source ${EXPDIR}/ocean_das_config
set NP = $JOB_NDAYS
@ NP = $NP + 2
rm -f $SCRDIR/sst_*_1200z.nc $SCRDIR/AICE_*_1200z.nc $SCRDIR/SSS_*_1200z.nc
rm -f $SCRDIR/rawM2sss_*.nc $SCRDIR/rawM2sst_*.nc $SCRDIR/rawM2sic_*.nc
ln -s ./INPUT/grid_spec.nc .
mpirun -np $NP $UMD_LETKFUTILS/ocean_sponge.py $yyyy $mm $dd > ocean_sponge.out
rm -f temp_sponge.nc 
rm -f salt_sponge.nc 

########################################################################
#   eric added 8/3/22 to save M2 forcing fields
mkdir ${EXPDIR}/ocean_das/$ANADIR/save_M2.forcing
cp -f rawM2*.nc ${EXPDIR}/ocean_das/$ANADIR/save_M2.forcing/
cp -f *1200z.nc ${EXPDIR}/ocean_das/$ANADIR/save_M2.forcing/
########################################################################

echo 'RESTORING VALUES ARE' ${ODAS_dt_restore_sst} ${ODAS_dt_restore_sss}
$UMD_LETKFUTILS/ocean_iau.x -DO_SPONGE ${ODAS_dt_restore_sst} ${ODAS_dt_restore_sss}  # Create sst relaxation files but no ocean increments
cp -f temp_sponge.nc $SCRDIR/INPUT/
cp -f salt_sponge.nc $SCRDIR/INPUT/
ln -s temp_sponge.nc $SCRDIR/INPUT/temp_sponge_coeff.nc
ln -s salt_sponge.nc $SCRDIR/INPUT/salt_sponge_coeff.nc

# Start scheduler for the observers
#----------------------------------
pkill python # Make sure obs scheduler is not running 
rm -f $SCRDIR/OCN_OBS_????????_??             
rm -f $SCRDIR/BADOBS
source ${EXPDIR}/ocean_das_config
echo 'BEFORE ocean_observer.py omf'
#${EXPDIR}/ocean_observer.py           omf ${ANADIR} True $DA_seq &
${UMD_LETKFSCRIPTS}/ocean_observer.py omf ${ANADIR} True $DA_seq &
set OBSSTATUS = $status
echo $OBSSTATUS 'AFTER  ocean_observer.py omf'
if( $OBSSTATUS != 0 ) then
   echo "BAD OMF OBSERVER IN oda_run.j"
   scancel ${SLURM_JOB_ID}
   exit(1)
endif


#if( -e hostfile ) then
#  /bin/rm -f hostfile
#endif
#scontrol show hostnames | tail -n +2 > hostfile
#
#set nodes = `cat hostfile`
#set nodelist1 = ''
#foreach node ( $nodes )
#   set nodelist1 = ${nodelist1}`echo -n ${node},`
#end
#   set nodelists = `echo ${nodelist1} | rev | cut -c 2- | rev`

# Start the Forecast
#-------------------
###############################################################################
#   ADDED 10/23/19 to test RECORD_FREQUENCY etc.  TURN OFF RECORD_FREQ STUFF

sed -i -e 's/^RECORD_FREQUENCY/#RECORD_FREQUENCY/' \
    -e 's/^RECORD_REF_DATE/#RECORD_REF_DATE/ ' \
    -e 's/^RECORD_REF_TIME/#RECORD_REF_TIME/' AGCM.rc

###############################################################################

@ NPES = $NX * $NY
$RUN_CMD $NPES ./GEOSgcm.x | tee geos.out
#mpirun $nodelists -np 24 ./GEOSgcm.x | tee geos.out


if( -e EGRESS ) then
   set rc = 0
else
   set rc = -1
   echo 'MODEL BOMBED'
   exit
endif
echo GEOSgcm Run Status: $rc

#   ERIC DEBUG 5/8/20
#exit

#######################################################################
#                  Rewind and run the Ocean DAS
#######################################################################                                                                

# Save date for end of da window
#------------------------------                                                                                                     
set yyyy_e  = `cat cap_restart | cut -c1-4`
set mm_e    = `cat cap_restart | cut -c5-6`
set dd_e    = `cat cap_restart | cut -c7-8`
set hh_e    = `cat cap_restart | cut -c10-11`

echo 'HERE IS CAP_RESTART2'
more cap_restart
pwd
echo 'HERE IS CAP_RESTART2'

pkill python                                         # Stop the scheduler ... a bit dangerous since it will kill everything python ...  
${UMD_LETKFUTILS}/wait4observer.py                   # Need to make sure the observers are done before starting the letkf               
echo "${yyyy}${mm}${dd} ${hh}0000" > cap_restart     # Rewind cap_restart                                                               
echo 'HERE IS CAP_RESTART3'
more cap_restart
pwd
echo 'HERE IS CAP_RESTART3'
echo 'ODAS_IAU ODAS_IAU_TIME' $ODAS_IAU $ODAS_IAU_TIME

#  Eric ERROR EXIT
#echo 'ERROC EXIT JUST BEFOER ODAS $$$$$$$$$$$$$$$$$$$$$$$$$$$'
#exit

${UMD_LETKFSCRIPTS}/ocean_das.csh ${yyyy} ${mm} ${dd} ${hh}  >& ocean_das_${yyyy}${mm}${dd}${hh}.out


cp -f ocean_das_${yyyy}${mm}${dd}${hh}.out ${EXPDIR}/ocean_das/${ANADIR}/
    
##################################################################################                                                                
#                  Retrospective Forecast and run Ocean Observer to collect oma's
##################################################################################                                                                

# Remove stuff relevant to properly running the observer
mkdir $SCRDIR/fcst_hist
#mv -f $SCRDIR/*.oletkf_*.nc4 $SCRDIR/fcst_hist/
mv -f $SCRDIR/*.ocn_inst_6hr_glo_T1440x1080_z50.*.nc4 $SCRDIR/fcst_hist/
mv -f $SCRDIR/*.ocn_inst_6hr_glo_L1440x721_z50.*.nc4  $SCRDIR/fcst_hist/
mv -f $SCRDIR/*.ocn_inst_6hr_glo_L1440x721_slv.*.nc4  $SCRDIR/fcst_hist/
mv -f $SCRDIR/*.ice_inst_6hr_glo_L1440x721_slv.*.nc4  $SCRDIR/fcst_hist/
pkill python         # Make sure obs scheduler is not running 
rm -f $SCRDIR/OCN_OBS_????????_??             
#  output OMA's
if( $ODAS_OUTPUT_OMA == True )then
    ${UMD_LETKFSCRIPTS}/ocean_observer.py oma ${ANADIR} True $DA_seq &
    set OBSSTATUS = $status
    if( $OBSSTATUS == 1 ) then
      echo "BAD OMA OBSERVER IN oda_run.j"
      scancel ${SLURM_JOB_ID}
      exit(1)
    endif
endif

# Add offset to sea-level
#${UMD_LETKFUTILS}/get_slv_offset.py $BASEDIR $SCRDIR $SCRDIR/INPUT/ocean_barotropic.res.nc > ${EXPDIR}/slv_offset_${yyyy}${mm}${dd}${hh}.out
${UMD_LETKFUTILS}/get_slv_offset.py $BASEDIR $SCRDIR $SCRDIR/INPUT/ocean_barotropic.res.nc > ${EXPDIR}/slv_offset.out
cp -f $SCRDIR/ocnobs_*.e ${EXPDIR}/ocean_das/$ANADIR/.
cp -f $SCRDIR/ocnobs_*.o ${EXPDIR}/ocean_das/$ANADIR/.
cp -f ${EXPDIR}/slv_offset.out ${EXPDIR}/ocean_das/${ANADIR}/.
    

# run the analysis sequence 
rm -f EGRESS
rm -f $SCRDIR/RESTART/* #Clean up to allow writting of ocean restarts

echo 'BEFORE ANALYSIS ^^^^^^^^^^^^^^^^^^^^^^^^^'
echo 'ODAS_IAU ODAS_IAU_TIME' $ODAS_IAU $ODAS_IAU_TIME
ls -ltr $SCRDIR *increment*

###############################################################################
#  NOW TURN ON RECORD_FREQUENCY ON 2ND PASS 

sed -i -e 's/^#RECORD_FREQUENCY/RECORD_FREQUENCY/' \
    -e 's/^#RECORD_REF_DATE/RECORD_REF_DATE/ ' \
    -e 's/^#RECORD_REF_TIME/RECORD_REF_TIME/' AGCM.rc

###############################################################################
#  CHECK TO SEE IF ODAS HAS COMPLETED PROPERLY
if ( -f ${EXPDIR}/ocean_das/${ANADIR}/temp_increment.nc ) then
    echo FOUND ${EXPDIR}/ocean_das/${ANADIR}/temp_increment.nc
    #  now check to make sure it's not zero
    $UMD_LETKFUTILS/bombifincriszero.py ${EXPDIR}/ocean_das/${ANADIR}/temp_increment.nc temp
    if ( -f ${SCRDIR}/BADINCR ) then
        exit(1)
    endif
else
    echo MISSING ODAS OUTPUT temp increment
    exit(1)
endif
if ( -f ${EXPDIR}/ocean_das/${ANADIR}/salt_increment.nc ) then
    echo FOUND ${EXPDIR}/ocean_das/${ANADIR}/salt_increment.nc
   #  now check to make sure it's not zero
    $UMD_LETKFUTILS/bombifincriszero.py ${EXPDIR}/ocean_das/${ANADIR}/salt_increment.nc salt
    if ( -f ${SCRDIR}/BADINCR ) then
        exit(1)
    endif
else
    echo MISSING ODAS OUTPUT salt increment
    exit(1)
endif

##################  eric change 6/6/23 ~3pm ##############
#  CHECK TO SEE IF ODAS WAS BUILT WITH PROPER NUMBER OF obs-*.nc FILES
ls -1 ${EXPDIR}/ocean_das/${ANADIR}/ocean_obs*/obs-* > numobsfiles
set numobslines=`cat -n numobsfiles | tail -n 1 | cut -f1`
echo "in oda_run.j JOB_NDAYS IS $JOB_NDAYS"
@ numobservers = $JOB_NDAYS * 4
echo "in oda_run.j numobserver $numobservers , numobsfiles is $numobslines"
if ( $numobslines != ( ${numobservers}  ) ) then
  echo  'NUMBER OF obs- FILES IS', $numobslines, 'SHOULD BE ', "${numobservers}"
  echo "ERROR EXIT NOT ENOUGH obs-*.nc FILES!"
  mv -f numobsfiles ${EXPDIR}/ocean_das/${ANADIR}
  exit(1)
endif

###############################################################################

$RUN_CMD $NPES ./GEOSgcm.x | tee geos_ana.out

echo 'ODAS_IAU ODAS_IAU_TIME' $ODAS_IAU $ODAS_IAU_TIME
echo ' ODAS_OUTPUT_OMA' $ODAS_OUTPUT_OMA
echo 'AFTER  ANALYSIS vvvvvvvvvvvvvvvvvvvvvvvvv'

#mpirun $nodelists -np 24 ./GEOSgcm.x | tee geos_ana.out

if( -e EGRESS ) then
    set rc = 0
else
    set rc = -1
    echo 'MODEL BOMBED'
    exit
endif
echo GEOSgcm Run Status: $rc
if( $ODAS_OUTPUT_OMA == True )then
    pkill python
    ${UMD_LETKFUTILS}/wait4observer.py 
endif

if( ! -e ${EXPDIR}/hindcast_restarts ) /bin/mkdir ${EXPDIR}/hindcast_restarts

# ----------------------------------------
#/bin/mv -f *_checkpoint* ${EXPDIR}/restarts
set   checkpoints  =    `/bin/ls -1 *_checkpoint.*`
if( $#checkpoints != 0 ) then
   /bin/mv -f *_checkpoint.* ${EXPDIR}/hindcast_restarts
   cd ${EXPDIR}/hindcast_restarts
#  set dd = `ls $checkpoints[1] | rev | cut -c1-19 | rev`
#  $GEOSBIN/stripname _checkpoint${dd} _rst   # ERIC KEEP THE DATE WITH ATM RESTARTS
   $GEOSBIN/stripname _checkpoint _rst
   cd $SCRDIR
endif
echo 'OUT OF STAGE 1'
#  shoud have ATM_rst_YYYYMMDD_2100z.nc4 in hindcast_restarts ########################

set   checkpoints  =    `/bin/ls -1 RESTART/ocean*.nc`
if( $#checkpoints != 0 ) then
   if( ! -e ${EXPDIR}/hindcast_restarts/RESTART ) /bin/mkdir ${EXPDIR}/hindcast_restarts/RESTART
#  /bin/mv -f RESTART/*z.ocean*.nc ${EXPDIR}/hindcast_restarts/RESTART
   /bin/mv -f RESTART/*_2100z.ocean*.nc ${EXPDIR}/hindcast_restarts/RESTART
#  cd ${EXPDIR}/hindcast_restarts/RESTART
#  set   checkpoints  =    `/bin/ls -1 *z.ocean*.nc`
#  set   checkpoints  =    `/bin/ls -1 *2100z.ocean*.nc`
#  set dd = `ls $checkpoints[1] | cut -c1-15`
#  $GEOSBIN/stripname ${dd}    #  ERIC LEAVE THE DATE 
   /bin/rm -f RESTART/*z.ocean*.nc  #  GET RID OF ALL THE 3Z INTERMEDIATES
endif
#  shoud have YYYYMMDD_2100z_OCN*.nc in hindcast_restarts/RESTARTS ###################
echo 'OUT OF STAGE 2'

#   NOW CREATE ONE TAR FILE FOR EACH RESTART DATE
echo 'IN DAILY DUMP ODA_RUN.J'
cd ${EXPDIR}/hindcast_restarts
#set tidates = `/bin/ls -1  turb_internal_checkpoint*`
set tidates = `/bin/ls -1  turb_internal_rst*`
if( $#tidates != 0 ) then
   foreach date ( $tidates )
      set ddd = `echo $date  | rev  | cut -c8-18  | rev`
      echo 'DDD IS' ${ddd}
      tar cvf restarts.e${ddd}z.tar *rst.$ddd* RESTART/$ddd*
      /bin/rm -f RESTART/$ddd*
      /bin/rm -f *rst.$ddd*
   end
endif
#  /bin/rm -rf RESTART
cd $SCRDIR
echo 'OUT OF STAGE 3'
 
#cd ${EXPDIR}/hindcast_restarts
##set checkpoints  =    `/bin/ls -1 *_rst`
#set checkpoints  =    `/bin/ls -1 *_rst*`
#if( $#checkpoints != 0 ) then
#   set ddd = `echo $dd | cut -c1-11`
#   tar cvf  restarts.e${ddd}z.tar *_rst* RESTART/
#   /bin/rm -rf RESTART
#   /bin/rm -rf *_rst
#endif
#cd $SCRDIR
#
###################################################################
# Remove increments and sponges
##################################################################
rm -f $SCRDIR/INPUT/temp_increment.nc
rm -f $SCRDIR/INPUT/salt_increment.nc
rm -f $SCRDIR/INPUT/eta_increment.nc
rm -f temp_salt_sponge.nc $SCRDIR/INPUT/

##################################################################
# Rename and Move Intermediate Checkpoints
##################################################################
#Get date from cap_restart            
#-------------------------                                                                                                 
set yyyy  = `cat cap_restart | cut -c1-4`
set mm    = `cat cap_restart | cut -c5-6`
set dd    = `cat cap_restart | cut -c7-8`
set hh    = `cat cap_restart | cut -c10-11`

#   NOW SAVE THE ODAS RESTART
set RSTDIR = ${EXPDIR}/ocean_das/restarts/
mkdir $RSTDIR
set edate = e`cat cap_restart | cut -c1-8`_`cat cap_restart | cut -c10-11`z

#OGCM
#----
/bin/cp -r ./RESTART/ $RSTDIR

#AGCM
#----
#/bin/cp -f *_checkpoint* $RSTDIR
/bin/cp -f *_rst* $RSTDIR
cd $RSTDIR
#$GEOSDIR/Linux/bin/stripname _checkpoint _rst
tar cvf  restarts.${edate}.tar *_rst RESTART
/bin/rm -rf *_rst RESTART
echo 'OUT OF STAGE 4'

##################################################################
# cleanup stuff 
##################################################################

rm  -f $SCRDIR/ocnobs_????????_??.e
rm  -f $SCRDIR/ocnobs_????????_??.o
rm  -f ${EXPDIR}/ocean_das/oana-*/ocean_observer_*/*.x
rm  -f ${EXPDIR}/ocean_das/oana-*/*.grd
rm -rf ${EXPDIR}/ocean_das/oana-*/recenter


