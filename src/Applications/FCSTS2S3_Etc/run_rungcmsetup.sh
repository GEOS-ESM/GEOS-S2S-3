#!/usr/bin/env bash 
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cd $SCRIPTPATH

source $SCRIPTPATH/func_fcst.sh

#todo:  when this script is executed by one of reserved computing node, this will run command in a loop
csec=$( date '+%s' )
if [[ -n $RUN_BY_CRON ]] && $RUN_BY_CRON ;then
    sec_end=$( date -d "+7days" +%s )
else
    sec_end=$( date -d "+7days" +%s )
fi

#dnb05_t2ssp quota limit
#/discover/nobackup/projects/gmao/t2ssp/GEOSS2S3/GEOS_fcst
dnb05_t2ssp_lim=471859200000
subtract=200000000000000
subtract=0

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
    
    dnb05_t2ssp_quota=$( showquota.beta  | grep -A1 dnb05_t2ssp | tail -1 | tr -s '[:space:]' | cut -d' ' -f3 )
    dnb05_t2ssp_thres=$(( $dnb05_t2ssp_lim - $subtract ))

    if (( $dnb05_t2ssp_quota <= $dnb05_t2ssp_thres ));then 
        ./run_gcmsetup.sh -w srcme_dis_fcst 1 >> $SCRIPTPATH/message/stderr_gcmsetup 2>&1

    else
        wmessage "$( date )"
        wmessage "*** run_gcmsetup.sh is not executed at this time:"
        wmessage "Subtracted quota value from Quota Limit : $( numfmt --to=iec $subtract ) $subtract"
        wmessage "                   Threshold calculated : $( numfmt --to=iec $dnb05_t2ssp_thres ) $dnb05_t2ssp_thres"
        wmessage "                            Quota Limit : $( numfmt --to=iec $dnb05_t2ssp_lim   ) $dnb05_t2ssp_lim" 
        wmessage "                            Quota Usage : $( numfmt --to=iec $dnb05_t2ssp_quota ) $dnb05_t2ssp_quota"
        wmessage
    fi

    sleep ${mintowait}m
    csec=$( date '+%s' )

done



    #./run_gcmsetup.sh -q R12319844 -f 20210526/ens1 srcme_pfe_nrt 1 >> $SCRIPTPATH/message/stderr_gcmsetup 2>&1
    #./run_gcmsetup.sh -q R12319844 -f 20210531/ens1 srcme_pfe_nrt 1 >> $SCRIPTPATH/message/stderr_gcmsetup 2>&1
    
    #./run_gcmsetup.sh -w -d 19811202 srcme_pfe_fcst 0 >> $SCRIPTPATH/message/stderr_gcmsetup 2>&1
    #./run_gcmsetup.sh -w -q R12139672 -r 198201-198201 srcme_pfe_fcst 0 >> $SCRIPTPATH/message/stderr_gcmsetup 2>&1
    #./run_gcmsetup.sh -w -r 198212-198212 srcme_pfe_fcstrome 0 >> /nobackupp11/gmaofcst/GEOSS2S3/GEOS_util/message/stderr_gcmsetup 2>&1
    #./run_gcmsetup.sh -w -r 202105-202105 srcme_pfe_cice 0 >> /nobackupp11/gmaofcst/GEOSS2S3/GEOS_util/message/stderr_gcmsetup 2>&1
    
    #./run_gcmsetup.sh -w -r 201904-201904 srcme_pfe_rim904 0 >> /nobackupp11/gmaofcst/GEOSS2S3/GEOS_util/message/stderr_gcmsetup_rim904 2>&1
    #./run_gcmsetup.sh -w -r 201904-201904 srcme_pfe_rim905 0 >> /nobackupp11/gmaofcst/GEOSS2S3/GEOS_util/message/stderr_gcmsetup_rim905 2>&1
 
    #./run_gcmsetup.sh -w -r 202205-202205 srcme_pfe_cice 0 >> $SCRIPTPATH/message/stderr_gcmsetup 2>&1
