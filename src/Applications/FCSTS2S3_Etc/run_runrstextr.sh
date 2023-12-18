#!/usr/bin/env bash 
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cd $SCRIPTPATH

./run_rstextr.sh -w --skip-lst 202308-202308 srcme_dis_cice >> $SCRIPTPATH/message/stderr_rstextr

./run_rstextr.sh -w 1999-2020 srcme_dis_fcst_giocean  >> $SCRIPTPATH/message/stderr_rstextr 2>&1

./run_rstextr.sh -w 1992-1998 srcme_dis_fcst          >> $SCRIPTPATH/message/stderr_rstextr 2>&1

./run_rstextr.sh -w 1985-1991 srcme_dis_fcst          >> $SCRIPTPATH/message/stderr_rstextr 2>&1

./run_rstextr.sh -w 1999-2020 srcme_dis_fcst_giocean  >> $SCRIPTPATH/message/stderr_rstextr 2>&1


#./run_rstextr.sh -w --skip-lst 202305-202305 srcme_dis_cice >> $SCRIPTPATH/message/stderr_rstextr

#./run_rstextr.sh -w 2022-2022 srcme_dis_htesch  >> $SCRIPTPATH/message/stderr_rstextr 2>&1
#./run_rstextr.sh -w 2022-2022 srcme_dis_hterpl  >> $SCRIPTPATH/message/stderr_rstextr 2>&1

#./run_rstextr.sh -w 2022-2022 srcme_dis_cice  >> $SCRIPTPATH/message/stderr_rstextr 2>&1
#./run_rstextr.sh -w 2016-2016 srcme_rim904 >> /discover/nobackup/projects/gmao/t2ssp/GEOSS2S3/GEOS_util/message/message_rstextr_rim904 2>&1
#./run_rstextr.sh -w 2016-2016 srcme_rim905 >> /discover/nobackup/projects/gmao/t2ssp/GEOSS2S3/GEOS_util/message/message_rstextr_rim905 2>&1

