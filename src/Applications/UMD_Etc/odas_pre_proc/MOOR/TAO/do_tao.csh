#!/bin/csh
#		
# TAO
#
#set main = /gpfsm/dhome/gmaofcst/ODAS/OBS/V3/
#set main = /gpfsm/dnb78s2/projects/p26/ehackert/ODAS/OBS/V3/
set main = /discover/nobackup/lren1/odas_pre_proc

echo 'in do_tao.csh $main is',$main
#set pathhome = $main/MOOR/TAO
cd $main/MOOR/TAO
source config.csh

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
echo 'pathfinal is',$pathfinal

# Process T
##########################################################
  echo 'Process T'
  cd $pathhome
  rm -f $pathfinal/T_TAO_$year*nc
  rm -f $pathfinal/T_TAO_$lyear*nc
  rm -f $pathfinal/T_TAO_$llyear.nc
  ./read_tao.x TEMP $llyear $year
  
  cp $pathfinal/T_TAO_$year*nc $pathfinal/IN_SITU/
  cp $pathfinal/T_TAO_$lyear*nc $pathfinal/IN_SITU/
  cp $pathfinal/T_TAO_$llyear*nc $pathfinal/IN_SITU/

  #echo 'Process S'
  rm -f $pathfinal/S_TAO_$year*nc
  rm -f $pathfinal/S_TAO_$lyear*nc
  rm -f $pathfinal/S_TAO_$llyear*nc
  
  ./read_tao.x SALT $llyear $year
  
  cp $pathfinal/S_TAO_$year*nc $pathfinal/SALT/
  cp $pathfinal/S_TAO_$lyear*nc $pathfinal/SALT/
  cp $pathfinal/S_TAO_$llyear*nc $pathfinal/SALT/

# Process SYN S
##########################################################
  echo 'Process SYN'
  cd $syn
#  cp /gpfsm/dnb78s2/projects/p26/ehackert/ODAS/OBS/V3/SYN_SALT/*.x .
  
  rm -f $pathfinal/SYN*_TAO_$year.nc
  ./syn_salt_cdf.x $year tao
  cp $pathfinal/SYN_TAO_$year*nc $pathfinal/SYN/

  rm -f $pathfinal/SYN*_TAO_$lyear.nc
  ./syn_salt_cdf.x $lyear tao
  cp $pathfinal/SYN_TAO_$lyear*nc $pathfinal/SYN/

  rm -f $pathfinal/SYN*_TAO_$llyear.nc
  ./syn_salt_cdf.x $llyear tao
  cp $pathfinal/SYN_TAO_$llyear*nc $pathfinal/SYN/

# In-situ to potential
##########################################################
 echo 'Insitu_2_pot'
 cd $pathhome
 module load matlab/R2014a   
# matlab -nosplash -nojvm -nodisplay -nodesktop < insitu2pot.m > out.matlab
# set start_year='syear='$llyear
 set start_year='syear='$year
 set end_year='eyear='$year
matlab -nodisplay -nosplash -nodesktop -r "clear all;close all;$start_year;$end_year;insitu2pot;exit"

 cp $pathfinal/T_TAO_$year*nc $pathfinal/POT/
 cp $pathfinal/T_TAO_$lyear*nc $pathfinal/POT/
 cp $pathfinal/T_TAO_$llyear*nc $pathfinal/POT/

#  echo 'Process S'
  ##########################################################
#  rm -f $pathfinal/S_TAO_$year*nc
#  rm -f $pathfinal/S_TAO_$lyear*nc
#  rm -f $pathfinal/S_TAO_$llyear*nc
  
#  ./read_tao.x SALT
  
#  cp $pathfinal/S_TAO_$year*nc $pathfinal/SALT/
#  cp $pathfinal/S_TAO_$lyear*nc $pathfinal/SALT/
#  cp $pathfinal/S_TAO_$llyear*nc $pathfinal/SALT/


##########################################################
  set new_date = `head -1 latest_data | awk '{printf "%s",substr($1,1,8)}' `;
  echo '   TAO    ' $new_date
cd $pathfinal

$run/vertical_thin.x T_TAO_$year.nc TEMP                > out.qc.tao.$year
$run/vertical_thin.x S_TAO_$year.nc SALT               >> out.qc.tao.$year

$run/vertical_thin.x T_TAO_$lyear.nc TEMP                > out.qc.tao.$lyear
$run/vertical_thin.x S_TAO_$lyear.nc SALT               >> out.qc.tao.$lyear

$run/vertical_thin.x T_TAO_$llyear.nc TEMP                > out.qc.tao.$llyear
$run/vertical_thin.x S_TAO_$llyear.nc SALT               >> out.qc.tao.$llyear

#
#  the end
#



