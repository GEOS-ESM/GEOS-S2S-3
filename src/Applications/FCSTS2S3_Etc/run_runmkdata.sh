#!/usr/bin/env bash 
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cd $SCRIPTPATH

csec=$( date '+%s' )
if [[ -n $RUN_BY_CRON ]] && $RUN_BY_CRON ;then
    sec_end=$(( csec + 10 ))
    exit
else
    #note:  R13909086 reservation ends 2022-10-13 11:00 EST
    sec_end=$( date -d "2022-10-13 09:00" +%s )
fi

while (( $csec <= $sec_end ));do 
    ./run_mkdata.sh -w -y 1990 srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_mkdata
    ./run_mkdata.sh -w -y 2000 srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_mkdata

    sleep 1h
    csec=$( date '+%s' )
done
