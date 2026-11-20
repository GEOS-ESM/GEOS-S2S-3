#!/bin/csh -f

#######################################################################
#                Batch Parameters for ODAS Portal Publish Job
#######################################################################

#@BATCH_TIME@PUBLISH_T
#@PUBLISH_P
#@BATCH_JOBNAME@PUBLISH_N
#@PUBLISH_Q
#@BATCH_GROUP
#@BATCH_OUTPUTNAME@EXPDIR/portal/gcm_publish.o
#@PUBLISH_OPENMODE

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

source $GEOSBIN/g5_modules
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib

if ( ! $?CARTOPY_DATA_DIR ) then
    setenv CARTOPY_DATA_DIR /discover/nobackup/projects/gmao/SIteam/CartopyDownloads
endif

#######################################################################
#             Experiment Specific Environment Variables
#######################################################################

setenv EXPID   @EXPID
setenv EXPDIR  @EXPDIR
setenv HOMDIR  @HOMDIR

#######################################################################
#        Plot and publish ODAS diagnostics to the experiment portal
#######################################################################

cd $EXPDIR

$GEOSUTIL/portal/run_odas_workflow.py -c $EXPDIR/portal/config.yaml -tw 3m

exit $status
