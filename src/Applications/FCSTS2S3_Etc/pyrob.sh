#!/bin/tcsh

#########################
#   Imported Variables  #
#########################
#dout_geos=outgeos/$strid
#cdir=`pwd`
#$ftype= ("t" for text, "r" for rtf)
#########################

#todo: source available g5_module 
setenv ESMADIR /nobackupp2/knakada/GEOS_Models/knakada_s2s3_unstable_FullModel_20211203_ROME/GEOSodas
source $ESMADIR/src/g5_modules >& /dev/null
cd $_dout_geos
module list 
which python 
exit
#note: HISTORY.rc is not necessary to run pyrob
if ( $ftype == "r" ) then
    echo "rich text format"
    #env PYTHONPATH=$ESMADIR/Linux/lib/Python/ $ESMADIR/Linux/bin/pyrob *.nc* -f rtf --output=spec |& tail -n 1
    env PYTHONPATH=$ESMADIR/Linux/lib/Python/ $cdir/pyrob_ocean.py *.nc* -f rtf --output=spec |& tail -n 1

else if ( $ftype == "t" ) then
    echo "text"
    env PYTHONPATH=$ESMADIR/Linux/lib/Python/ $cdir/pyrob_ocean.py *.nc* -f text --output=spec |& tail -n 1
endif

echo "env PYTHONPATH=$ESMADIR/Linux/lib/Python/ $cdir/pyrob_ocean.py *.nc* -f text --output=spec |& tail -n 1"
#env PYTHONPATH=$ESMADIR/Linux/lib/Python/ $ESMADIR/Linux/bin/pyrob *.nc* -f rtf --output=spec 
#env PYTHONPATH=$ESMADIR/Linux/lib/Python/ $ESMADIR/Linux/bin/pyrob -H HISTORY.rc *.nc4 -f rtf --output=spec

