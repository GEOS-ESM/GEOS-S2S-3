#!/usr/bin/env bash 
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cd $SCRIPTPATH

source $SCRIPTPATH/func_fcst.sh
hstshort=$( get_host )

csec=$( date '+%s' )
if [[ -n $RUN_BY_CRON ]] && $RUN_BY_CRON ;then
    #sec_end=$(( csec + 10 ))
    exit
elif [[ "$hstshort" == "pfe" ]];then 
    resqid=R15216128
    enddate=$( pbs_rstat -f $resqid | grep reserve_end | cut -d'=' -f2 )
    sec_end=$( date -d "$enddate - 1hours" +%s ) 

    #numfmt --to=iec 200000000000000
    #182T
    #numfmt --to=iec 400000000000000
    #364T
       subtract=200000000000000
       #subtract=400000000000000
    nbp18_limit=$( /usr/bin/lfs quota -u $USER /nobackupp18/$USER | tail -1 | tr -s '[:space:]' | sed 's#^ *##g' | cut -d' ' -f2 )000

elif [[ "$hstshort" == "dis" ]];then 
    sec_end=$( date -d "+7days" +%s ) 
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
        ./run_submit.sh -w -t 5 >> $SCRIPTPATH/message/stderr_submit 2>&1

    elif [[ "$hstshort" == "pfe" ]];then 
        ./run_submit.sh -w -t 5 -e ose004 >> $SCRIPTPATH/message/stderr_submit 2>&1

#        #note:  quota is show with kbytes. Thus, 000 added
#        nbp18_quota=$( /usr/bin/lfs quota -u $USER /nobackupp18/$USER | tail -1 | tr -s '[:space:]' | sed 's#^ *##g' | cut -d' ' -f1 )000
#        nbp18_thres=$(( $nbp18_limit - $subtract ))
##wmessage $nbp18_quota $nbp18_thres
##exit
#
#       if (( $nbp18_quota <= $nbp18_thres ));then 
#           ./run_submit.sh -w  >> $SCRIPTPATH/message/stderr_submit 2>&1
#           #./run_submit.sh -w -e cice >> $SCRIPTPATH/message/stderr_submit 2>&1
#
#       else
#           wmessage "$(date )"
#           wmessage "*** run_submit.sh is not executed at this time:"
#           wmessage "Subtracted quota value from Quota Limit : $( numfmt --to=iec $subtract ) $subtract"
#           wmessage "                   Threshold calculated : $( numfmt --to=iec $nbp18_thres ) $nbp18_thres"
#           wmessage "                            Quota Limit : $( numfmt --to=iec $nbp18_limit ) $nbp18_limit" 
#           wmessage "                            Quota Usage : $( numfmt --to=iec $nbp18_quota ) $nbp18_quota"
#       fi
    fi

    sleep ${inc_min}m
    csec=$( date '+%s' )
done
