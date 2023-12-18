#!/usr/bin/env bash 
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cd $SCRIPTPATH
#todo:  exit out if number of process reached to the max 
numproc=$( ps aux | awk '{ print $1 }' | grep -w $USER | wc -l )
numproc_max=$( ulimit -u )

(( $numproc >= $numproc_max )) && exit
arryyyymm=( 1981 1982 1993 1994 1995  )
arryyyymm=( 1993 1994 1995  )
for yyyy in ${arryyyymm[@]};do
    ./run_clean.sh -c -w -r ${yyyy}01-${yyyy}12 srcme_pfe_fcst >> $SCRIPTPATH/message/stderr_clean 2>&1
    #./run_clean.sh -w -b -r ${yyyy}01-${yyyy}12 srcme_pfe_fcst 
done
#./run_clean.sh -c -w -r 198402-198402 srcme_pfe_fcst >> $SCRIPTPATH/message/message_clean 2>&1
