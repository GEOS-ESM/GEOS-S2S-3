#!/usr/bin/env bash 
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cd $SCRIPTPATH

for x in {1..2};do 
    ./run_rstpreps.sh -w -r 199812-202012 srcme_dis_fcst_giocean >> $SCRIPTPATH/message/stderr_rstpreps 2>&1
done

for x in {1..2};do 
    ./run_rstpreps.sh -w -r 199203-199811 srcme_dis_fcst >> $SCRIPTPATH/message/stderr_rstpreps 2>&1
done

./run_rstpreps.sh -w -r 198805-199111 srcme_dis_fcst >> $SCRIPTPATH/message/stderr_rstpreps 2>&1

#./run_rstpreps.sh -w -r 198112-199112 srcme_dis_fcst >> $SCRIPTPATH/message/stderr_rstpreps 2>&1
#
#./run_rstpreps.sh -w -r 199112-199912 srcme_dis_fcst >> $SCRIPTPATH/message/stderr_rstpreps 2>&1

#for x in {1..2};do 
#    ./run_rstpreps.sh -w  --skip-lst -r 199203-199212 srcme_dis_fcst >> $SCRIPTPATH/message/stderr_rstpreps 2>&1
#done
#
#./run_rstpreps.sh -w  --skip-lst -r 198805-198812 srcme_dis_fcst >> $SCRIPTPATH/message/stderr_rstpreps 2>&1

#./run_rstpreps.sh -w srcme_dis_nrtv3 >> $SCRIPTPATH/message/stderr_rstpreps_nrt3  2>&1

#./run_rstpreps.sh -w -r 202205-202205 srcme_dis_cice >> $SCRIPTPATH/message/stderr_rstpreps_cice 2>&1

#./run_rstpreps.sh -w -f data_ose001a_dates srcme_dis_ose001a >> $SCRIPTPATH/message/message_rstpreps_ose 2>&1
#./run_rstpreps.sh -w -f data_ose002a_dates srcme_dis_ose002a >> $SCRIPTPATH/message/message_rstpreps_ose 2>&1

#./run_rstpreps.sh -w -r 202204-202204 srcme_dis_htesch >> $SCRIPTPATH/message/message_rstpreps_hte 2>&1
#./run_rstpreps.sh -w -r 202204-202204 srcme_dis_hterpl >> $SCRIPTPATH/message/message_rstpreps_hte 2>&1

#./run_rstpreps.sh -w -r 202105-202105 srcme_dis_nrt  >> $SCRIPTPATH/message/message_rstpreps_nrt 2>&1

#./run_rstpreps.sh -w -r 202104-202104 srcme_dis_rim904 >> $SCRIPTPATH/message/message_rstpreps_rim904 2>&1
#./run_rstpreps.sh -w -r 202104-202104 srcme_dis_rim905 >> $SCRIPTPATH/message/message_rstpreps_rim905 2>&1


