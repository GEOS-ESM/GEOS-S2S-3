SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd )"
cd $SCRIPTPATH
source func_fcst.sh

./run_wftmsz.sh -wp  $SCRIPTPATH/data/wftmsz/data_wftmsz_dis_rst  >> $SCRIPTPATH/message/stderr_wftmsz 2>&1
