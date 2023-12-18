#!/bin/csh
#		
# Launch script for each mooring


if($#argv != 1) then
#  echo "e.g., ./run_moor_2.csh 2000 2022 "
   echo "input year"
   exit
endif

set year = $1

#set main = /gpfsm/dhome/gmaofcst/ODAS/OBS/V3/
set main = /discover/nobackup/lren1/odas_pre_proc
cd $main/MOOR
source ./config.csh

cd $run/RAMA
pwd
./do_rama.csh $year
#

cd $run/TAO
pwd
./do_tao.csh $year
#

cd $run/PIRATA
./do_pir.csh  $year
#

cd $run

#dboss_check_moor_v2.py

 
#
#  the end
#



