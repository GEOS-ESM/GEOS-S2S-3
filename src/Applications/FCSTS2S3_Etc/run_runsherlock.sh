#!/usr/bin/env bash 
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cd $SCRIPTPATH
    
chh=$( date +'%-H' )
chh_mod=$( echo "$chh % 2" | bc )
#chh_mod=$( echo "$chh % 3" | bc )

#./run_sherlock.sh -w -f data_sherlock srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_sherlock 2>&1
#./run_sherlock.sh -w -r 199112-199204 srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_sherlock 2>&1

#if (( $chh_mod == 0 ));then
#    #screen -dmS sherlock bash -c "./run_sherlock.sh -w -q R12753106 -r 198406-198410 srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_sherlock 2>&1"
#    screen -dmS sherlock bash -c "./run_sherlock.sh -w -r 198406-198410 srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_sherlock 2>&1"
#else
#    #screen -dmS sherlock bash -c "./run_sherlock.sh -w -q R12753106 -f /nobackupp11/gmaofcst/GEOSS2S3/GEOS_util/data_sherlock  srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_sherlock 2>&1"
#    screen -dmS sherlock bash -c "./run_sherlock.sh -w -f /nobackupp11/gmaofcst/GEOSS2S3/GEOS_util/data_sherlock  srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_sherlock 2>&1"
#fi
