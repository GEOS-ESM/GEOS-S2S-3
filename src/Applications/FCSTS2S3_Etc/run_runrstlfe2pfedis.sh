#!/usr/bin/env bash  
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cd $SCRIPTPATH

source $SCRIPTPATH/func_fcst.sh

hstname=$( hostname )

#todo:  when this script is executed by one of reserved computing node, this will run command in a loop
csec=$( date '+%s' )
if [[ ${hstname:0:3} == pfe ]];then
    hrs=24
    sec=$( echo "3600 * $hrs" | bc ) 
    sec_end=$(( csec + $sec ))
    runbycron=false

elif [[ -n $RUN_BY_CRON ]] && $RUN_BY_CRON ;then
    sec_end=$(( csec + 10 ))
    runbycron=true
else
    resqid=R14731034
    enddate=$( pbs_rstat -f $resqid | grep reserve_end | cut -d'=' -f2 )
    sec_end=$( date -d "$enddate - 1hours" +%s ) 

    runbycron=false
fi

while (( $csec <= $sec_end ));do 
 
    #./run_rstlfe2pfedis.sh -w -r 199601-202012 srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_rstlfe2pfedis 2>&1
    #./run_rstlfe2pfedis.sh -w -r 202205-202205 srcme_pfe_cice >> $SCRIPTPATH/message/stderr_rstlfe2pfedis 2>&1


    #./run_rstlfe2pfedis.sh -w -r 202205-202205 srcme_pfe_cice >> $SCRIPTPATH/message/stderr_rstlfe2pfedis 2>&1

    ##eric's exp
    #./run_rstlfe2pfedis.sh -w -f data_ose00Xa_dates srcme_pfe_ose001a >> $SCRIPTPATH/message/stderr_rstlfe2pfedis 2>&1
    #./run_rstlfe2pfedis.sh -w -f data_ose00Xa_dates srcme_pfe_ose002a >> $SCRIPTPATH/message/stderr_rstlfe2pfedis 2>&1
    
    ##Hunga Tonga experiment
    #./run_rstlfe2pfedis.sh -w -r 202204-202204 srcme_pfe_htesch >> $SCRIPTPATH/message/stderr_rstlfe2pfedis 2>&1
    #./run_rstlfe2pfedis.sh -w -r 202204-202204 srcme_pfe_hterpl >> $SCRIPTPATH/message/stderr_rstlfe2pfedis 2>&1
    
    #anna's exp
    #./run_rstlfe2pfedis.sh -w -s -r 202105-202105 srcme_pfe_nrt    >> $SCRIPTPATH/message/stderr_rstlfe2pfedis 2>&1
    
    #eric's exp
    #./run_rstlfe2pfedis.sh -w -s -r 202104-202104 srcme_pfe_rim904 >> $SCRIPTPATH/message/stderr_rstlfe2pfedis 2>&1
    #./run_rstlfe2pfedis.sh -w -s -r 202104-202104 srcme_pfe_rim905 >> $SCRIPTPATH/message/stderr_rstlfe2pfedis 2>&1
    #./run_rstlfe2pfedis.sh -w -s -r 201703-201703 srcme_pfe_rim904 >> $SCRIPTPATH/message/stderr_rstlfe2pfedis 2>&1
    #./run_rstlfe2pfedis.sh -w -s -r 201703-201703 srcme_pfe_rim905 >> $SCRIPTPATH/message/stderr_rstlfe2pfedis 2>&1
    #./run_rstlfe2pfedis.sh -w -s -r 201705-201705 srcme_pfe_rim904 >> $SCRIPTPATH/message/stderr_rstlfe2pfedis 2>&1
    #./run_rstlfe2pfedis.sh -w -s -r 201705-201705 srcme_pfe_rim905 >> $SCRIPTPATH/message/stderr_rstlfe2pfedis 2>&1

    sleep 5m
    csec=$( date '+%s' )
done
