#!/usr/bin/env bash 
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cd $SCRIPTPATH
csec=$( date '+%s' )
if [[ -n $RUN_BY_CRON ]] && $RUN_BY_CRON ;then
    sec_end=$(( csec + 10 ))
else
    #end date = Thu Aug 18 07:00:00 EDT 2022
    #sec_end=1660820400

    #note:  R13909086 reservation ends 2022-10-13 11:00 EST
    sec_end=$( date -d "2022-10-13 09:00" +%s )
    thisqid=R13909086
fi

while (( $csec <= $sec_end ));do 

    #./run_quickarch.sh -w -u -q $thisqid -f $SCRIPTPATH/data/quickarch/data_quickarch_fcst srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_quickarch 2>&1
    ./run_quickarch.sh -w -u -q $thisqid -f $SCRIPTPATH/data/quickarch/data_quickarch_fcst srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_quickarch 2>&1
    sleep 10m
    csec=$( date '+%s' )
done 
