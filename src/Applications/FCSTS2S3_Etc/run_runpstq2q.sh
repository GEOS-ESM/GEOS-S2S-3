#!/usr/bin/env bash 
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cd $SCRIPTPATH

#todo:  when this script is executed by one of reserved computing node, this will run command in a loop
csec=$( date '+%s' )
if [[ -n $RUN_BY_CRON ]] && $RUN_BY_CRON ;then
    sec_end=$(( csec + 10 ))
else
    resqid=R15274220
    enddate=$( pbs_rstat -f $resqid | grep reserve_end | cut -d'=' -f2 )
    res_end=$( date -d "$enddate" +%s ) 
    sec_end=$( date -d "$enddate - 1hours" +%s ) 
fi

while (( $csec <= $sec_end ));do 

    if (( $csec <= $res_end ));then 
        #resqid=R14731034
        :
    else
        break
    fi

    ./run_pstq2q.sh -w -s P2022 $resqid ivy 1 srcme_pfe_ose004      >> $SCRIPTPATH/message/stderr_pstq2q 2>&1
    ./run_pstq2q.sh -w -s P2022 $resqid ivy 1 srcme_pfe_ose003      >> $SCRIPTPATH/message/stderr_pstq2q 2>&1
    #./run_pstq2q.sh -w -s P2022 $resqid ivy 1 srcme_pfe_htesch       >> $SCRIPTPATH/message/stderr_pstq2q 2>&1
    #./run_pstq2q.sh -w -s P2022 $resqid ivy 1 srcme_pfe_hterpl       >> $SCRIPTPATH/message/stderr_pstq2q 2>&1

    #./run_pstq2q.sh -w -s P2015 $resqid ivy 1 srcme_pfe_ose001a      >> $SCRIPTPATH/message/stderr_pstq2q 2>&1
    #./run_pstq2q.sh -w -s P2015 $resqid ivy 1 srcme_pfe_ose002a      >> $SCRIPTPATH/message/stderr_pstq2q 2>&1
    #./run_pstq2q.sh -w -s P2015 R13245282 ivy 1 srcme_pfe_ose001     >> $SCRIPTPATH/message/stderr_pstq2q 2>&1
    #./run_pstq2q.sh -w -s P2015 R13245282 ivy 1 srcme_pfe_ose002     >> $SCRIPTPATH/message/stderr_pstq2q 2>&1
    #./run_pstq2q.sh -w -s P2022 $resqid ivy 1 srcme_pfe_ose003       >> $SCRIPTPATH/message/stderr_pstq2q 2>&1
    #./run_pstq2q.sh -w -s P2022 $resqid ivy 1 srcme_pfe_ose004       >> $SCRIPTPATH/message/stderr_pstq2q 2>&1

    #./run_pstq2q.sh -w -s P19 $resqid ivy 1 srcme_pfe_fcst     >> $SCRIPTPATH/message/stderr_pstq2q 2>&1
    #./run_pstq2q.sh -w -s P20 $resqid ivy 1 srcme_pfe_fcst     >> $SCRIPTPATH/message/stderr_pstq2q 2>&1

    sleep 7m
    csec=$( date '+%s' )
done
    
