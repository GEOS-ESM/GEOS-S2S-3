#!/usr/bin/env bash 
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"

cd $SCRIPTPATH
cdir=$SCRIPTPATH
strscr=$(basename "$0" | cut -d'.' -f1 | cut -d'_' -f2 )
source $cdir/func_fcst.sh
source $cdir/srcme_pfe_runrun

csec=$( date '+%s' )
if [[ -n $RUN_BY_CRON ]] && $RUN_BY_CRON ;then
    sec_end=$(( csec + 10 ))
else
    #todo:  figure out ending time if this script is running on 
    #       a reserved node
    hst=$( hostname ) 
    
    #note:  set date & time here if any particular date & time should be the end 
    #       of run of this script. 
    #enddate="Wed May 17 08:00:00 EDT 2023"
    hrsbf=1
    minsbf=
    [[ -n $hrsbf && -z $minsbf ]] && secbf=$( echo "$hrsbf  * 3600" | bc ) 
    [[ -z $hrsbf && -n $minsbf ]] && secbf=$( echo "$minsbf * 60"   | bc ) 
    [[ -z $secbf ]] && secbf=0
    
    if [[ -n $enddate ]];then 
        #ref:https://unix.stackexchange.com/a/497642
        #sec_end=$( date -d "$( date -Iseconds -d "$enddate" ) -${hrsbf}hours" +'%s' )
        sec_end=$( date -d "$enddate -${secbf}seconds" +'%s' )
    
    else
        hst=$( hostname ) 
        if [[ "$hst" =~ "pfe"* ]];then 
            resqid=
        else
            resqid=$( cmd_gjob_nas  | grep $hst 2>/dev/null | cut -d' ' -f3 )
            if [[ -z $resqid ]];then 
                resqid=$( /PBS/bin/pbs_rfe status | tail -1 | cut -d' ' -f2 )
            fi
        fi
       
        if [[ -n $resqid ]];then 
            enddate=$( pbs_rstat -f $resqid | grep reserve_end | cut -d'=' -f2 )
            sec_end=$( date -d "$enddate - 1hours" +%s ) 
        else
            sec_end=$( date -d "+30days" +%s ) 
        fi
    fi
fi

#wmessage \@$LINENO
#date -d@$sec_end
#exit

#mintowait=$( calc_wait $cmin_runsmessage ) 
#sleep ${mintowait}m

while (( $csec <= $sec_end ));do 

    #====================
    #      smessage
    #====================
    ./run_smessage.sh -w >> $cdir/message/stderr_smessage 2>&1

    source $cdir/srcme_pfe_runrun
    mintowait=$( calc_wait $cmin_runsmessage ) 
    
    sleep ${mintowait}m
    csec=$( date '+%s' )
done


