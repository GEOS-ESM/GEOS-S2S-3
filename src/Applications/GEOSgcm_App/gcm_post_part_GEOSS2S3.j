#!/bin/csh -f

#######################################################################
#                Batch Parameters for Post-Processing Job
#######################################################################

#@BATCH_TIME@POST_T
##@POSTPRC_PRES
##@POSTPRC_QRES
#@POSTPRC_P
#@POST_Q
#@BATCH_JOBNAMEP@FCSTDATE@ENSEMBLE_MEMBER.@COLLECTION.@YYYYMM
#@GEOSS2S3_OPT2
##@GEOSS2S3_OPT3
#@BATCH_GROUP
#@BATCH_OUTPUTNAME@POST_O
#@BATCH_JOINOUTERR

#######################################################################
#                  System Environment Variables
#######################################################################

umask 022

limit stacksize unlimited

@SETENVS

#######################################################################
#           Architecture Specific Environment Variables
#######################################################################

setenv ARCH `uname`

setenv SITE             @SITE
setenv GEOSBIN          @GEOSBIN
setenv GEOSUTIL         @GEOSSRC/GMAO_Shared/GEOS_Util
setenv BATCHNAME       "P@FCSTDATE@ENSEMBLE_MEMBER"

if( $?SLURM_NTASKS ) then
      setenv RUN_CMD "@RUN_CMD"
      set NCPUS = $SLURM_NTASKS
else if( $?PBS_NODEFILE ) then
      setenv RUN_CMD "@RUN_CMD"
      set NCPUS = `cat $PBS_NODEFILE | wc -l`
else
      set NCPUS = NULL
endif

source $GEOSBIN/g5_modules
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib

#######################################################################
#                      Perform Post Processing
#######################################################################

$GEOSUTIL/post/gcmpost_GEOSS2S3.script -source @EXPDIR -ncpus $NCPUS -nostrict -collections @COLLECTION -recover @YYYYMM -ignore_nan -history @EXPDIR/HISTORY_1.rc

exit
