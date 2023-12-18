#!/usr/bin/env bash 
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"

cd $SCRIPTPATH
cdir=$SCRIPTPATH
source $cdir/func_fcst.sh

csec=$( date '+%s' )
if [[ -n $RUN_BY_CRON ]] && $RUN_BY_CRON ;then
    sec_end=$(( csec + 10 ))
else
    resqid=R15216128
    enddate=$( pbs_rstat -f $resqid | grep reserve_end | cut -d'=' -f2 )
    res_end=$( date -d "$enddate" +%s ) 
    sec_end=$( date -d "$enddate - 1hours" +%s ) 
fi

#todo:  calculate increments and wait until next inc_min 
#note:  this code is created to start running the main process at a particular 
#       time in order to get "nice" increments (similar to what cron does). 
#       i.e.) inc_min=20min => runs sripts every 20 mins at 0, 20, 40min 
#       i.e.) inc_min=10min => runs sripts every 10 mins at 0, 10, 20min, ...
cmin=$( date +%-M )

#note:  60 has to be divisible by inc_min
inc_min=20
inc_divis=$( echo "60 % $inc_min" | bc -l )
num_inc=$( echo "60 / $inc_min" | bc )
arrmins=($( printf '%s\n' $( seq $num_inc )  | xargs -i bash -c "echo "'"'"$inc_min * {}"'"'" | bc " ))
   
arrminnext=($( printf '%s\n' ${arrmins[@]} | xargs -i bash -c "(( $cmin < {} )) && echo {}"))
minnext=${arrminnext[0]#0}
cmin=$( date +%-M )
mintowait=$( echo "$minnext - $cmin" | bc ) 

sleep ${mintowait}m

while (( $csec <= $sec_end ));do 

    #====================
    #      smessage
    #====================
    ./run_smessage.sh -w >> $SCRIPTPATH/message/stderr_smessage 2>&1

    #====================
    #      utility
    #====================
    #delete long-running shiftc
    ./run_utility.sh -w --sftc-days=true srcme_pfe_ose003 >> $SCRIPTPATH/message/stderr_utility 2>&1
    ./run_utility.sh -w --sftc-days=true srcme_pfe_ose004 >> $SCRIPTPATH/message/stderr_utility 2>&1
    
    #./run_utility.sh -w --sftc-days=true srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_utility 2>&1
    
    #compress_sst (optt)
    #./run_utility.sh -w -t 202204 srcme_pfe_htesch >> $SCRIPTPATH/message/stderr_utility 2>&1

    #cnt_sst (optq)
    #./run_utility.sh -w -q 202204 srcme_pfe_htesch >> $SCRIPTPATH/message/stderr_utility 2>&1

    runhere=false
    if $runhere;then 
        #count_archshiftc (opto) 
        #./run_utility.sh -w -o srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_utility 2>&1 
   
        #compress_sst (optt)
        #./run_utility.sh -w -t 202205 srcme_pfe_cice >> $SCRIPTPATH/message/stderr_utility 2>&1
        #cnt_sst (optq)
        #./run_utility.sh -w -q 202205 srcme_pfe_cice >> $SCRIPTPATH/message/stderr_utility 2>&1

               
        arryyyymm=($( cat $SCRIPTPATH/data/utility_compress_sst/data_utility_compress_sst_fcst_mkdata* ))

        for yyyymm in ${arryyyymm[@]};do
            #compress_sst (optt)
            ./run_utility.sh -w -t $yyyymm srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_utility 2>&1

            #cnt_sst (optq)
            ./run_utility.sh -w -q $yyyymm srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_utility 2>&1
        done 
    fi

    cmin=$( date +%-M )
    arrminnext=($( printf '%s\n' ${arrmins[@]} | xargs -i bash -c "(( $cmin < {} )) && echo {}"))
    minnext=${arrminnext[0]#0}
    mintowait=$( echo "$minnext - $cmin" | bc ) 
    
    sleep ${mintowait}m
    #sleep ${inc_min}m
    csec=$( date '+%s' )
done


#====================
#      getwtime
#====================
#screen -dmS getwtime bash -c "./run_getwtime.sh srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_getwtime 2>&1"
#screen -dmS getwtime bash -c "./run_getwtime.sh srcme_pfe_rim905 >> $SCRIPTPATH/message/stderr_getwtime 2>&1"
#screen -dmS getwtime bash -c "./run_getwtime.sh srcme_pfe_rim904 >> $SCRIPTPATH/message/stderr_getwtime 2>&1"

#====================
#     getstatus
#====================
#!!!!! NOTE: 03/02/2022 these are moved to run_runarch.sh
#screen -dmS getstatus_fcst bash -c "./run_getstatus.sh -w srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_getstatus 2>&1"
#screen -dmS getstatus_fcst bash -c "./run_getstatus.sh -l -w srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_getstatus 2>&1"

#screen -dmS getstatus bash -c "./run_getstatus.sh srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_getstatus 2>&1"
#screen -dmS getstatus bash -c "./run_getstatus.sh -r 198112-198201 srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_getstatus 2>&1"

#./run_getstatus.sh -r 202004-202004 srcme_pfe_rim904 >> $SCRIPTPATH/message/stderr_getstatus 2>&1
#./run_getstatus.sh -r 201904-201904 srcme_pfe_rim905 >> $SCRIPTPATH/message/stderr_getstatus 2>&1
#screen -dmS getstatus_cice bash -c "./run_getstatus.sh srcme_pfe_cice >> $SCRIPTPATH/message/stderr_getstatus 2>&1"

#====================
#      sherlock
#====================
#!!!!! NOTE: 03/02/2022 these are moved to run_runarch.sh
#screen -dmS sherlock bash -c "./run_sherlock.sh -w -q R12896507 -r 199112-199201 srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_sherlock 2>&1"

#if (( $chh_mod == 0 ));then
#    #screen -dmS sherlock bash -c "./run_sherlock.sh -w -q R12753106 -r 198406-198410 srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_sherlock 2>&1"
#    screen -dmS sherlock bash -c "./run_sherlock.sh -w -r 198406-198410 srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_sherlock 2>&1"
#else
#    #screen -dmS sherlock bash -c "./run_sherlock.sh -w -q R12753106 -f /nobackupp11/gmaofcst/GEOSS2S3/GEOS_util/data_sherlock  srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_sherlock 2>&1"
#    screen -dmS sherlock bash -c "./run_sherlock.sh -w -f /nobackupp11/gmaofcst/GEOSS2S3/GEOS_util/data_sherlock  srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_sherlock 2>&1"
#fi

#====================
#   deletearchjobs
#====================
#./run_deletearchjobs.sh  >> $SCRIPTPATH/message/stderr_deletearchjobs 2>&1

#====================
#    clean shiftc
#====================
#./run_utility.sh -i srcme_pfe_fcstrome





