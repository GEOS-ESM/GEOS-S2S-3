#!/usr/bin/env bash 
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cd $SCRIPTPATH
source $SCRIPTPATH/func_fcst.sh

#todo:  when this script is executed by one of reserved computing node, this will run command in a loop
runbycron=false
csec=$( date '+%s' )
if [[ -n $RUN_BY_CRON ]] && $RUN_BY_CRON ;then
    sec_end=$(( csec + 10 ))
    runbycron=true
else
    #end date = Thu Aug 18 07:00:00 EDT 2022
    sec_end=1660820400
    runbycron=false
fi

blrun3mo=true
blrunwin=false
initexec=false
while (( $csec <= $sec_end ));do 
    if $runbycron;then
        :
    else
        #todo:  skip this for the first run.
        if $initexec;then
            #todo: figure out next quarter hour
            interval=15
            csec=$( date "+%s" )
            nsec=$(( csec - ( csec % ( interval * 60))  + ( interval * 60 )  ))
            
            sc_wait=$(( nsec - csec ))
            mn_wait=($( echo "scale=1;$sc_wait / 60" | bc -l ))

            #wmessage "wait for $mn_wait min" 
            
            sleep ${sc_wait}s
        fi

        if $blrun3mo;then 
            blrun3mo=false
            blrunwin=true
            ./run_outrecover.sh -w -f data/outrecover/data_outrecover_1994_3mo srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_outrecover 2>&1
        elif $blrunwin;then
            blrun3mo=true
            blrunwin=false
            ./run_outrecover.sh -w -f data/outrecover/data_outrecover_1994_win srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_outrecover 2>&1
        fi

    fi
done

