#!/usr/bin/env bash 
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cd $SCRIPTPATH

source $SCRIPTPATH/func_fcst.sh

csec=$( date '+%s' )
if [[ -n $RUN_BY_CRON ]] && $RUN_BY_CRON ;then
    sec_end=$(( csec + 10 ))
    xmin=10
    xmin_extra=15
else
    resqid=R15216128
    resqid=
    if [[ -n $resqid ]];then 
        enddate=$( pbs_rstat -f $resqid | grep reserve_end | cut -d'=' -f2 )
        sec_end=$( date -d "$enddate - 1hours" +%s ) 
    else
        sec_end=$( date -d "+7days" +%s ) 
    fi

    #xmin_extra=10
    thisqid=
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

sleep ${mintowait}m

while (( $csec <= $sec_end ));do 
                        
    #./run_arch.sh -w -u -r 200001-200012 srcme_pfe_fcst   >> $SCRIPTPATH/message/stderr_arch 2>&1
    ./run_arch.sh -w -u -r 200101-200109 srcme_pfe_fcst   >> $SCRIPTPATH/message/stderr_arch 2>&1

    #for x in {1..2};do 

    #    if (( $x == 1 ));then 
    #        msg_newfile $SCRIPTPATH/data/arch/data_arch_fcst_runarch 
    #        numf=$( find $SCRIPTPATH/data/misc/* -type f -name "data_dexpsize_fcst_mkdata*" 2>/dev/null | wc -l )

    #        if (( $numf ));then
    #            thisfdata=$SCRIPTPATH/data/arch/data_arch_fcst_runarch
    #            [[ -f $thisfdata ]] && msg_newfile $thisfdata
    #            limexp=100
    #            cat  $SCRIPTPATH/data/misc/data_dexpsize_fcst_mkdata*  | sort -Vr | grep -E '199|200' | head -$limexp | sed 's#\t# #g' | cut -d' ' -f2 >> $thisfdata
    #            runhere=true
    #            nummax=20
    #        else
    #            runhere=false
    #        fi 
    #    elif (( $x == 2 ));then 
    #        runhere=true
    #        nummax=5
    #        thisfdata=$SCRIPTPATH/data/arch/data_arch_doover_fcst
    #    fi

    #    if $runhere;then
    #        cnt=0
    #        for x in {1..$nummax};do
    #            if [[ -n $thisqid ]];then  
    #                ./run_arch.sh -w -u -q $thisqid -f $thisfdata  srcme_pfe_fcst   >> $SCRIPTPATH/message/stderr_arch 2>&1
    #            else
    #                ./run_arch.sh -w -u -f $thisfdata              srcme_pfe_fcst   >> $SCRIPTPATH/message/stderr_arch 2>&1
    #            fi
    #                
    #            sleep ${xmin_extra}m
    #        done
    #        cnt=$(( cnt + 1 ))
    #    fi
    #done

    #runhere=true
    #if $runhere;then
    #    arrfdata=($( find $SCRIPTPATH/data/arch/* -type f -name "data_arch_fcst_mkdata*" ))
    #    arryyyymm=()
    #    for fdata in ${arrfdata[@]};do
    #        arryyyymm+=($( misc_readfbyline $fdata ))
    #    done 
    #    arryyyymm=($( printf '%s\n' ${arryyyymm[@]} | sort -Vr ))

    #    cnt=0
    #    for yyyymm in ${arryyyymm[@]};do 
    #        yyyy=$( echo $yyyymm | cut -c1-4 )

    #        if (( $yyyy > 2000 ));then
    #            for x in {1..2};do
    #                if [[ -n $thisqid ]];then  
    #                    ./run_arch.sh -w -u -q $thisqid -r ${yyyymm}-${yyyymm} srcme_pfe_fcst   >> $SCRIPTPATH/message/stderr_arch 2>&1
    #                else
    #                    ./run_arch.sh -w -u -r ${yyyymm}-${yyyymm}             srcme_pfe_fcst   >> $SCRIPTPATH/message/stderr_arch 2>&1
    #                fi
    #                sleep ${xmin}m
    #            done
    #        else
    #            if [[ -n $thisqid ]];then  
    #                ./run_arch.sh -w -u -q $thisqid -r ${yyyymm}-${yyyymm}     srcme_pfe_fcst   >> $SCRIPTPATH/message/stderr_arch 2>&1
    #            else
    #                ./run_arch.sh -w -u -r ${yyyymm}-${yyyymm}                 srcme_pfe_fcst   >> $SCRIPTPATH/message/stderr_arch 2>&1
    #            fi
    #            sleep ${xmin}m
    #        fi
    #        cnt=$(( cnt + 1 ))
    #    done
    #fi
    #

    sleep ${inc_min}m
    csec=$( date '+%s' )
    
done

#./run_arch.sh -w -u -f data_ose_dates srcme_pfe_ose003 >> $SCRIPTPATH/message/stderr_arch 2>&1
#./run_arch.sh -w -u -f data_ose_dates srcme_pfe_ose004 >> $SCRIPTPATH/message/stderr_arch 2>&1

#./run_arch.sh -w -u -r 202204-202204 srcme_pfe_htesch       >> $SCRIPTPATH/message/stderr_arch 2>&1
#./run_arch.sh -w -u -r 202204-202204 srcme_pfe_hterpl       >> $SCRIPTPATH/message/stderr_arch 2>&1

#./run_arch.sh -w -u -f data_ose00Xa_dates srcme_pfe_ose001a >> $SCRIPTPATH/message/stderr_arch 2>&1
#./run_arch.sh -w -u -f data_ose00Xa_dates srcme_pfe_ose002a >> $SCRIPTPATH/message/stderr_arch 2>&1

##for x in {1..5};do 
##    ./run_arch.sh -w -u -r 202205-202205 srcme_pfe_cice >> $SCRIPTPATH/message/stderr_arch 2>&1
##    sleep 5m
##done 
#
##./run_arch.sh -w -u -f data_dexp_ose001  srcme_pfe_ose001 >> $SCRIPTPATH/message/stderr_arch 2>&1
##./run_arch.sh -w -u -f data_dexp_ose002  srcme_pfe_ose002 >> $SCRIPTPATH/message/stderr_arch 2>&1
#


