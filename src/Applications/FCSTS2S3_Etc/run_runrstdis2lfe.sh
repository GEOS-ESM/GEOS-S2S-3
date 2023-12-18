#!/usr/bin/env bash 
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cd $SCRIPTPATH

./run_rstdis2lfe.sh -w srcme_dis_cice   >> $SCRIPTPATH/message/message_rstdis2lfe 2>&1
./run_rstdis2lfe.sh -w srcme_dis_fcst   >> $SCRIPTPATH/message/message_rstdis2lfe 2>&1

#./run_rstdis2lfe.sh -w srcme_dis_ose001a >> $SCRIPTPATH/message/message_rstdis2lfe 2>&1
#./run_rstdis2lfe.sh -w srcme_dis_ose002a >> $SCRIPTPATH/message/message_rstdis2lfe 2>&1
#
#./run_rstdis2lfe.sh -w srcme_dis_htesch >> $SCRIPTPATH/message/message_rstdis2lfe 2>&1
#./run_rstdis2lfe.sh -w srcme_dis_hterpl >> $SCRIPTPATH/message/message_rstdis2lfe 2>&1


#./run_rstdis2lfe.sh -w srcme_dis_rim904 >> $SCRIPTPATH/message/message_rstdis2lfe 2>&1
#./run_rstdis2lfe.sh -w srcme_dis_rim905 >> $SCRIPTPATH/message/message_rstdis2lfe 2>&1
#./run_rstdis2lfe.sh -w srcme_dis_nrt    >> $SCRIPTPATH/message/message_rstdis2lfe 2>&1
exit
