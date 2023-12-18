#!/usr/bin/env bash 
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cd $SCRIPTPATH
cdir=$SCRIPTPATH
source $cdir/func_fcst.sh

csec=$( date '+%s' )
if [[ -n $RUN_BY_CRON ]] && $RUN_BY_CRON ;then
    #sec_end=$(( csec + 10 ))
    exit
else
    resqid=R14731034
    enddate=$( pbs_rstat -f $resqid | grep reserve_end | cut -d'=' -f2 )
    res_end=$( date -d "$enddate" +%s ) 
    sec_end=$( date -d "$enddate - 1hours" +%s ) 
fi

while (( $csec <= $sec_end ));do 
        
    #./run_pckwinners.sh -w -r 202204-202204 srcme_pfe_htesch >> $SCRIPTPATH/message/stderr_pckwinners 2>&1
    ./run_pckwinners.sh -w -r 202204-202204 srcme_pfe_hterpl >> $SCRIPTPATH/message/stderr_pckwinners 2>&1
    
    #./run_pckwinners.sh -w -r 202205-202205 srcme_pfe_cice >> $SCRIPTPATH/message/stderr_pckwinners 2>&1

    #arryyyymm=($( cat $SCRIPTPATH/data/pckwinners/data_pckwinners_fcst_mkdata* ))
    #for yyyymm in ${arryyyymm[@]};do 
    #    ./run_pckwinners.sh -w -r ${yyyymm}-${yyyymm} srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_pckwinners 2>&1
    #done

    sleep 30m
    csec=$( date '+%s' )
done

