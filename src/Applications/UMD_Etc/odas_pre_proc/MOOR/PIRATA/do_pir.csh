#!/bin/csh
#		
# Pirata
#
#set main = /gpfsm/dhome/gmaofcst/ODAS/OBS/V3/
set main = /discover/nobackup/lren1/odas_pre_proc
cd $main/MOOR/PIRATA
source config.csh

echo $pathfinal $pathraw

#cp vertical_thin.x $pathfinal/.

#set year=`date +"20%y" `
#set month=`date +%m`
#set day=`date +%-d`

if($#argv != 1) then
#  echo "e.g., ./run_moor_2.csh 2000 2022 "
   echo "input the year for processing"
   exit
endif

set year=$1

set lyear=`expr $year - 1`
set llyear=`expr $year - 2`

cd $pathhome

# Process T
##########################################################
  echo 'Process PIRATA T', $pathhome
  cd $pathhome
  rm -f $pathfinal/T_PIR_$year.nc
  rm -f $pathfinal/T_PIR_$lyear.nc
  rm -f $pathfinal/T_PIR_$llyear.nc
  ./read_pir.x TEMP $llyear $year
  
  cp $pathfinal/T_PIR_$year*nc $pathfinal/IN_SITU/
  cp $pathfinal/T_PIR_$lyear*nc $pathfinal/IN_SITU/
  cp $pathfinal/T_PIR_$llyear*nc $pathfinal/IN_SITU/
  

# Process S
##########################################################
  echo 'Process PIRATA S'
  cd $pathhome
  rm -f $pathfinal/S_PIR_$year.nc
  rm -f $pathfinal/S_PIR_$lyear.nc
  rm -f $pathfinal/S_PIR_$llyear.nc
  ./read_pir.x SALT $llyear $year
  cp $pathfinal/S_PIR_$year*nc $pathfinal/SALT/
  cp $pathfinal/S_PIR_$lyear*nc $pathfinal/SALT/
  cp $pathfinal/S_PIR_$llyear*nc $pathfinal/SALT/


# Process Syn S
##########################################################
  echo 'Process PIRATA SYN S'
  cd $syn

  rm -f $pathfinal/SYN*_PIR_$year.nc
  ./syn_salt_cdf.x $year pir
  cp $pathfinal/SYN_PIR_$year*nc $pathfinal/SYN/

  rm -f $pathfinal/SYN*_PIR_$lyear.nc
  ./syn_salt_cdf.x $lyear pir
  cp $pathfinal/SYN_PIR_$lyear*nc $pathfinal/SYN/

  rm -f $pathfinal/SYN*_PIR_$llyear.nc
  ./syn_salt_cdf.x $llyear pir
  cp $pathfinal/SYN_PIR_$llyear*nc $pathfinal/SYN/

# In-situ to potential
##########################################################
  echo 'Insitu_2_pot PIRATA'
  cd $pathhome
  module load matlab/R2014a   
#  matlab -nosplash -nojvm -nodisplay -nodesktop < insitu2pot.m > out.matlab

 set start_year='syear='$llyear
 set end_year='eyear='$year
matlab -nodisplay -nosplash -nodesktop -r "clear all;close all;$start_year;$end_year;insitu2pot;exit"

  cp $pathfinal/T_PIR_$year*nc $pathfinal/POT/ 
  cp $pathfinal/T_PIR_$lyear*nc $pathfinal/POT/ 
  cp $pathfinal/T_PIR_$llyear*nc $pathfinal/POT/ 


##########################################################
  set new_date = `head -1 latest_data | awk '{printf "%s",substr($1,1,8)}' `;
  echo '   PIRATA ' $new_date

cd $pathfinal
$run/vertical_thin.x T_PIR_$year.nc TEMP                > out.qc.pir.$year
$run/vertical_thin.x S_PIR_$year.nc SALT               >> out.qc.pir.$year

$run/vertical_thin.x T_PIR_$lyear.nc TEMP                > out.qc.pir.$lyear
$run/vertical_thin.x S_PIR_$lyear.nc SALT               >> out.qc.pir.$lyear

$run/vertical_thin.x T_PIR_$llyear.nc TEMP                > out.qc.pir.$llyear
$run/vertical_thin.x S_PIR_$llyear.nc SALT               >> out.qc.pir.$llyear

#
#  the end
#



