#!/usr/bin/env bash 
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cd $SCRIPTPATH

source $SCRIPTPATH/func_fcst.sh
hstshort=$( get_host )

if [[ "$hstshort" == "pfe" ]];then 
    #todo:  exit out if number of process reached to the max 
    numproc=$( ps aux | awk '{ print $1 }' | grep -w $USER | wc -l )
    numproc_max=$( ulimit -u )
    (( $numproc >= $numproc_max )) && wmessage numproc = $numproc numproc_max = $numproc_max && exit
fi

#todo:  when this script is executed by one of reserved computing node, this will run command in a loop
csec=$( date '+%s' )
if [[ -n $RUN_BY_CRON ]] && $RUN_BY_CRON ;then
    sec_end=$(( csec + 10 ))
    runbycron=true

elif [[ "$hstshort" == "pfe" ]];then 
    #resqid=R15216128
    resqid=
    if [[ -n $resqid ]];then 
        enddate=$( pbs_rstat -f $resqid | grep reserve_end | cut -d'=' -f2 )
        sec_end=$( date -d "$enddate - 1hours" +%s ) 
    else
        sec_end=$( date -d "+7days" +%s ) 
    fi
    runbycron=false

elif [[ "$hstshort" == "dis" ]];then 
    sec_end=$( date -d "+7days" +%s ) 
    runbycron=false
fi

#todo:  calculate increments and wait until next inc_min 
#note:  this code is created to start running the main process at a particular 
#       time in order to get "nice" increments (similar to what cron does). 
#       i.e.) inc_min=20min => runs sripts every 20 mins at 0, 20, 40min 
#       i.e.) inc_min=10min => runs sripts every 10 mins at 0, 10, 20min, ...
cmin=$( date +%M )

#note:  60 has to be divisible by inc_min
inc_min=20
inc_divis=$( echo "60 % $inc_min" | bc -l )
num_inc=$( echo "60 / $inc_min" | bc )
arrmins=($( printf '%s\n' $( seq $num_inc )  | xargs -i bash -c "echo "'"'"$inc_min * {}"'"'" | bc " ))
   
arrminnext=($( printf '%s\n' ${arrmins[@]} | xargs -i bash -c "(( $cmin < {} )) && echo {}"))
minnext=${arrminnext[0]#0}
cmin=$( date +%M )
mintowait=$( echo "$minnext - $cmin" | bc ) 

sleep ${mintowait}m


while (( $csec <= $sec_end ));do 
    
    if [[ "$hstshort" == "dis" ]];then 
        ./run_monitor.sh -w -z -r 199601-199601 srcme_${hstshort}_fcst >> $SCRIPTPATH/message/stderr_monitor 2>&1 
    fi
    
    if [[ "$hstshort" == "pfe" ]];then 
        :
        #./run_monitor.sh -w -z -r 202204-202204 srcme_pfe_htesch >> $SCRIPTPATH/message/stderr_monitor 2>&1
        #./run_monitor.sh -w -z -r 202204-202204 srcme_pfe_hterpl >> $SCRIPTPATH/message/stderr_monitor 2>&1

        #./run_monitor.sh -w -z -f data_ose001a_dexp srcme_pfe_ose001a >> $SCRIPTPATH/message/stderr_monitor 2>&1
        #./run_monitor.sh -w -z -f data_ose002a_dexp srcme_pfe_ose002a >> $SCRIPTPATH/message/stderr_monitor 2>&1
        #./run_monitor.sh -w -z -f data_ose003_dexp srcme_pfe_ose003 >> $SCRIPTPATH/message/stderr_monitor 2>&1 
        #./run_monitor.sh -w -z -f data_ose004_dexp srcme_pfe_ose004 >> $SCRIPTPATH/message/stderr_monitor 2>&1 

        #./run_monitor.sh -w -z -r 202205-202205 srcme_pfe_cice >> $SCRIPTPATH/message/message_monitor_cice 2>&1

        ##arryyyymm=($( misc_readfbyline $SCRIPTPATH/data/monitor/data_monitor_fcst_mkdata ))
        #arryyyymm=($( cat $SCRIPTPATH/data/monitor/data_monitor_fcst_mkdata* ))
        #for yyyymm in ${arryyyymm[@]}; do 
        #    ./run_monitor.sh -w -z -r ${yyyymm}-${yyyymm}   srcme_pfe_fcst    >> $SCRIPTPATH/message/message_monitor 2>&1
        #done 

        #./run_monitor.sh -w -z -f data_dexp_ose001 srcme_pfe_ose001 >> $SCRIPTPATH/message/stderr_monitor_ose001 2>&1 
        #./run_monitor.sh -w -z -f data_dexp_ose002 srcme_pfe_ose002 >> $SCRIPTPATH/message/stderr_monitor_ose002 2>&1 
        
        #./run_monitor.sh -w -z -r 202105-202105 srcme_pfe_nrt   >> $SCRIPTPATH/message/stderr_monitor_nrt 2>&1
        
        #./run_monitor.sh -z -r 202104-202104 srcme_pfe_rim904 >> $SCRIPTPATH/message/message_monitor_rim 2>&1
        #./run_monitor.sh -z -r 202104-202104 srcme_pfe_rim905 >> $SCRIPTPATH/message/message_monitor_rim 2>&1
    fi 


    sleep ${inc_min}m
    csec=$( date '+%s' )
done 
