#!/bin/csh
#		
# RAMA
#
#set main = /gpfsm/dhome/gmaofcst/ODAS/OBS/V3/
set main = /discover/nobackup/lren1/OBS/V3
cd $main/MOOR/scripts/RAMA
source config.csh

cp vertical_thin.x $pathfinal/.

#set year=`date +"20%y" `
#set month=`date +%m`
#set day=`date +%-d`

#echo "current date:" $year$month$day


if($#argv != 1) then
#  echo "e.g., ./run_moor_2.csh 2000 2022 "
   echo "input the year for processing"
   exit
endif

set year = $1
set lyear=`expr $year - 1`
set llyear=`expr $year - 2`

cd $pathhome

# Process T
 #########################################################
  echo 'Process RAMA T'
#  cd $pathhome
#  rm -f $pathfinal/T*_RAMA_$year.nc
#  rm -f $pathfinal/T*_RAMA_$lyear.nc
#  rm -f $pathfinal/T*_RAMA_$llyear.nc
#  ./read_rama.x TEMP $llyear $year
  
#  cp $pathfinal/T_RAMA_$year*nc $pathfinal/IN_SITU/
#  cp $pathfinal/T_RAMA_$lyear*nc $pathfinal/IN_SITU/
#  cp $pathfinal/T_RAMA_$llyear*nc $pathfinal/IN_SITU/

# Process S
##########################################################
  echo 'Process RAMA Syn S'
#  cd $syn
  
#  rm -f $pathfinal/SYN*_RAMA_$year.nc
#  ./syn_salt_cdf.x $year rama
#  cp $pathfinal/SYN_RAMA_$year*nc $pathfinal/SYN/

#  rm -f $pathfinal/SYN*_RAMA_$lyear.nc
#  ./syn_salt_cdf.x $lyear rama
#  cp $pathfinal/SYN_RAMA_$lyear*nc $pathfinal/SYN/
  
#  rm -f $pathfinal/SYN*_RAMA_$llyear.nc
#  ./syn_salt_cdf.x $llyear rama
#  cp $pathfinal/SYN_RAMA_$llyear*nc $pathfinal/SYN/ 


# In-situ to potential
##########################################################
 echo 'Insitu_2_pot RAMA'
 cd $pathhome
 module load matlab/R2014a   
 matlab -nosplash -nojvm -nodisplay -nodesktop < insitu2pot.m > out.matlab
 cp $pathfinal/T_RAMA_$year*nc $pathfinal/POT/
 cp $pathfinal/T_RAMA_$lyear*nc $pathfinal/POT/
 cp $pathfinal/T_RAMA_$llyear*nc $pathfinal/POT/


# Process S
##########################################################
 echo 'Process RAMA S'
 cd $pathhome
  rm -f $pathfinal/S_RAMA_$year.nc
  rm -f $pathfinal/S_RAMA_$lyear.nc
  rm -f $pathfinal/S_RAMA_$llyear.nc  
  ./read_rama.x SALT $llyear $year
  cp $pathfinal/S_RAMA_$year*nc $pathfinal/SALT
  cp $pathfinal/S_RAMA_$lyear*nc $pathfinal/SALT
  cp $pathfinal/S_RAMA_$llyear*nc $pathfinal/SALT

##########################################################
  set new_date = `head -1 latest_data | awk '{printf "%s",substr($1,1,8)}' `;
  echo '   RAMA   ' $new_date

cd $pathfinal
./vertical_thin.x T_RAMA_$year.nc TEMP > out.qc.rama.$year
./vertical_thin.x S_RAMA_$year.nc SALT >> out.qc.rama.$year

./vertical_thin.x T_RAMA_$lyear.nc TEMP >> out.qc.rama.$lyear
./vertical_thin.x S_RAMA_$lyear.nc SALT >> out.qc.rama.$lyear

./vertical_thin.x T_RAMA_$llyear.nc TEMP >> out.qc.rama.$llyear
./vertical_thin.x S_RAMA_$llyear.nc SALT >> out.qc.rama.$llyear



#
#  the end
#



