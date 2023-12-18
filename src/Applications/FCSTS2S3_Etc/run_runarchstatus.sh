#!/usr/bin/env bash 
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cd $SCRIPTPATH

source $SCRIPTPATH/func_fcst.sh

csec=$( date '+%s' )
if [[ -n $RUN_BY_CRON ]] && $RUN_BY_CRON ;then
    sec_end=$(( csec + 10 ))
    runbycron=true
else
    runbycron=false
    blinitexecute=true

    #resqid=R15216128
    resqid=
    if [[ -n $resqid ]];then 
        enddate=$( pbs_rstat -f $resqid | grep reserve_end | cut -d'=' -f2 )
        res_end=$( date -d "$enddate" +%s ) 
        sec_end=$( date -d "$enddate - 1hours" +%s ) 
    else
        sec_end=$( date -d "+7days" +%s ) 
    fi

fi

#todo:  calculate increments and wait until next inc_min 
#note:  this code is created to start running the main process at a particular 
#       time in order to get "nice" increments (similar to what cron does). 
#       i.e.) inc_min=20min => runs sripts every 20 mins at 0, 20, 40min 
#       i.e.) inc_min=10min => runs sripts every 10 mins at 0, 10, 20min, ...
cmin=$( date +%M )

#note:  60 has to be divisible by inc_min
inc_min=1
inc_divis=$( echo "60 % $inc_min" | bc -l )
num_inc=$( echo "60 / $inc_min" | bc )
arrmins=($( printf '%s\n' $( seq $num_inc )  | xargs -i bash -c "echo "'"'"$inc_min * {}"'"'" | bc " ))
   
arrminnext=($( printf '%s\n' ${arrmins[@]} | xargs -i bash -c "(( $cmin < {} )) && echo {}"))
minnext=${arrminnext[0]#0}
cmin=$( date +%M )
mintowait=$( echo "$minnext - $cmin" | bc ) 


./run_archstatus.sh -w -r 198112-200109 srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_archstatus 2>&1 
exit

sleep ${mintowait}m

while (( $csec <= $sec_end ));do 

    if $runbycron;then
        arrfdata=($( find $SCRIPTPATH/data/archstatus/* -type f -name "data_archstatus_fcst_mkdata*" ))
        for fdata in ${arrfdata[@]}; do 
            ./run_archstatus.sh -w -r $fdata srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_archstatus 2>&1 
        done
    else
        ./run_archstatus.sh -w -r 200001-200112 srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_archstatus 2>&1 

        runhere=false
        if $runhere;then
            arrfdata=($( find $SCRIPTPATH/data/archstatus/* -type f -name "data_archstatus_fcst_mkdata*" ))
            for fdata in ${arrfdata[@]}; do 
                ./run_archstatus.sh -w -f $fdata srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_archstatus 2>&1 
                sleep 1m
            done
        fi
    fi
    csec=$( date '+%s' )
    sleep ${inc_min}m
done

    
#./run_archstatus.sh -w -f data_dexp_ose001 srcme_pfe_ose001 >> $SCRIPTPATH/message/stderr_archstatus 2>&1 
#./run_archstatus.sh -w -f data_dexp_ose002 srcme_pfe_ose002 >> $SCRIPTPATH/message/stderr_archstatus 2>&1 
#./run_archstatus.sh -w -f data_ose003_dexp srcme_pfe_ose003 >> $SCRIPTPATH/message/stderr_archstatus 2>&1 
#./run_archstatus.sh -w -f data_ose004_dexp srcme_pfe_ose004 >> $SCRIPTPATH/message/stderr_archstatus 2>&1 


