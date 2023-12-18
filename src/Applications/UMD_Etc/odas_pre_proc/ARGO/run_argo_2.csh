#!/bin/csh
#
# Process GODAE ARGO data

#cd /discover/nobackup/lren1/pre_proc/NRT/ARGO/scripts
source config.csh
limit stacksize unlimited

set idate=$1
echo 'Date: ' $idate
set year=`echo $idate | cut -c1-4`
set month=`echo $idate | cut -c5-6`
set day=`echo $idate | cut -c7-8`


#do_it_all_wmo

# Remove files first
rm -f $step1/*$year.nc
rm -f $step2/*$year.nc
rm -f $final/*$year.nc

# STEP1
cd $run
pwd
  set month = {01,02,03,04,05,06,07,08,09,10,11,12}
    foreach mm ($month)       
    set lof  = `ls $raw/*/$year/$mm/*.nc | /gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/SLES12/OBS/V3/ARGO/sort -t/ -b -n +13n`
      foreach f ( ` echo $lof ` )
        set basin = ` echo $f | awk '{printf "%s",substr($1,48,3)}' `;
        set base  = ` basename $f _prof.nc  `
        echo $base $basin $year
        ./read_argo.x $base $basin $year  #>> out.argo.$year               
       end
    end

# STEP2
cd $run
touch out.argo2.$year
./read_argo_2.x $year T >> out.argo2.$year
./read_argo_2.x $year S >> out.argo2.$year

# STEP3
cp $step2/*$year.nc $final/
cd $final
$run/vertical_thin.x T_ARGO_$year.nc TEMP > out.qc.argoT.$year
$run/vertical_thin.x S_ARGO_$year.nc SALT > out.qc.argoS.$year

mv *out* OUTFILES/

echo 'Last Date: ' $base
cd $run
./dboss_check_argo_v3.py

#
#  the end
#



