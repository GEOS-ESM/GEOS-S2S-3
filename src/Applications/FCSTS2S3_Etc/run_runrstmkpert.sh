#!/usr/bin/env bash 
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cd $SCRIPTPATH


./run_rstmkpert.sh -w --skip-lst -r 202208-202208 srcme_dis_cice            >> $SCRIPTPATH/message/stderr_rstmkpert 2>&1
./run_rstmkpert.sh -w            -r 199812-202012 srcme_dis_fcst_giocean    >> $SCRIPTPATH/message/stderr_rstmkpert 2>&1
./run_rstmkpert.sh -w            -r 199201-199811 srcme_dis_fcst            >> $SCRIPTPATH/message/stderr_rstmkpert 2>&1
./run_rstmkpert.sh -w            -r 198512-199111 srcme_dis_fcst            >> $SCRIPTPATH/message/stderr_rstmkpert 2>&1


#./run_rstmkpert.sh -w --skip-lst -r 199201-199212 srcme_dis_fcst >> $SCRIPTPATH/message/stderr_rstmkpert 2>&1


#./run_rstmkpert.sh -w -r 202204-202204 srcme_dis_htesch >> $SCRIPTPATH/message/stderr_rstmkpert 2>&1
#./run_rstmkpert.sh -w -r 202204-202204 srcme_dis_hterpl >> $SCRIPTPATH/message/stderr_rstmkpert 2>&1
