#!/usr/bin/env bash 
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cd $SCRIPTPATH

source $SCRIPTPATH/func_fcst.sh
hstshort=$( get_host )

#todo:  when this script is executed by one of reserved computing node, this will run command in a loop
runbycron=false
csec=$( date '+%s' )
if [[ -n $RUN_BY_CRON ]] && $RUN_BY_CRON ;then
    sec_end=$(( csec + 10 ))
    runbycron=true
elif [[ "$hstshort" == "pfe" ]];then 
    #note:  R13909086 reservation ends 2022-10-13 11:00 EST
    sec_end=$( date -d "2022-10-13 09:00" +%s )
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
    if $runbycron;then
        ./run_getstatus.sh -w -l -y 1990 srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_getstatus 2>&1
    else
        if [[ "$hstshort" == "pfe" ]];then 
            #./run_getstatus.sh -w -r 202205-202205 srcme_pfe_cice >> $SCRIPTPATH/message/stderr_getstatus 2>&1
            #sleep 1m

            #./run_getstatus.sh -w -l -y 2000 srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_getstatus 2>&1
            :
        elif [[ "$hstshort" == "dis" ]];then 
            ./run_getstatus.sh -w -l srcme_dis_fcst >> $SCRIPTPATH/message/stderr_getstatus 2>&1
        fi
    fi

    sleep ${inc_min}m
    csec=$( date '+%s' )
done

