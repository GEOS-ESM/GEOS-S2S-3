#!/bin/csh
#
# run_argo_1.csh: D-BOSS Script to ftp data then call run_argo_2.csh

if($#argv != 1) then
   echo "e.g., ./run_argo_1.csh 20220516"
else


#set main = /gpfsm/dhome/gmaofcst/ODAS/OBS/V3/
#set main = /gpfsm/dnb78s2/projects/p26/ehackert/ARGO_processing
#set main = /gpfsm/dnb78s2/projects/p26/ehackert/ODAS/OBS/V3
set main = /discover/nobackup/lren1/odas_pre_proc
set run  = $main/ARGO/
cd $run
rm -f status
source config.csh
#/gpfsm/dnb78s2/projects/p26/ehackert/ARGO_processing/V3/

set idate=$1
set rawdir=/discover/nobackup/lren1/pre_proc/NRT/ARGO
#set rawdir=/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/ARGO

set raw   = ${rawdir}/RAW/
set final = ${rawdir}/FINAL/

echo 'Date: ' $idate
set year=`echo $idate | cut -c1-4`
set month=`echo $idate | cut -c5-6`
set day=`echo $idate | cut -c7-8`
set hour=00
set min=00

set lmonth=`expr $month - 1`
if ($lmonth < 10) then
  set lmonth=0$lmonth
endif
set lyear=$year

if ($lmonth == 00) then
  set lmonth=12
  set lyear=`expr $year - 1`
endif

# FTP Data
# Get previous month and current month
# We don't need to do this since since V2 D-BOSS is ftping the data

if (! -d ${raw}/IND/${lyear}/${lmonth}) then
     mkdir -p ${raw}/IND/${lyear}/${lmonth}
endif

cd $raw/IND/$lyear/$lmonth/
wget  --no-check-certificate --auth-no-challenge=on -r --accept "nc" --reject "index.html*" --no-directories -np -e robots=off -N -nv 'https://data-argo.ifremer.fr/geo/indian_ocean/'$lyear'/'$lmonth'/';
# OLD wget -N -nv 'ftp://www.usgodae.org/pub/outgoing/argo/geo/indian_ocean/'$lyear'/'$lmonth'/*_prof.nc';

if (! -d ${raw}/ATL/${lyear}/${lmonth}) then
     mkdir -p ${raw}/ATL/${lyear}/${lmonth}
endif

cd $raw/ATL/$lyear/$lmonth/
wget  --no-check-certificate --auth-no-challenge=on -r --accept "nc" --reject "index.html*" --no-directories -np -e robots=off -N -nv 'https://data-argo.ifremer.fr/geo/atlantic_ocean/'$lyear'/'$lmonth'/';
# OLD wget -N -nv 'ftp://www.usgodae.org/pub/outgoing/argo/geo/atlantic_ocean/'$lyear'/'$lmonth'/*_prof.nc';

if (! -d ${raw}/PAC/${lyear}/${lmonth}) then
     mkdir -p ${raw}/PAC/${lyear}/${lmonth}
endif

cd $raw/PAC/$lyear/$lmonth/
wget  --no-check-certificate --auth-no-challenge=on -r --accept "nc" --reject "index.html*" --no-directories -np -e robots=off -N -nv 'https://data-argo.ifremer.fr/geo/pacific_ocean/'$lyear'/'$lmonth'/';
# OLD wget -N -nv 'ftp://www.usgodae.org/pub/outgoing/argo/geo/pacific_ocean/'$lyear'/'$lmonth'/*_prof.nc';

if (! -d ${raw}/IND/${lyear}/${month}) then
     mkdir -p ${raw}/IND/${lyear}/${month}
endif

cd $raw/IND/$lyear/$month/
wget  --no-check-certificate --auth-no-challenge=on -r --accept "nc" --reject "index.html*" --no-directories -np -e robots=off -N -nv 'https://data-argo.ifremer.fr/geo/indian_ocean/'$lyear'/'$month'/';

if (! -d ${raw}/ATL/${lyear}/${month}) then
     mkdir -p ${raw}/ATL/${lyear}/${month}
endif

cd $raw/ATL/$lyear/$month/
wget  --no-check-certificate --auth-no-challenge=on -r --accept "nc" --reject "index.html*" --no-directories -np -e robots=off -N -nv 'https://data-argo.ifremer.fr/geo/atlantic_ocean/'$lyear'/'$month'/';

if (! -d ${raw}/PAC/${lyear}/${month}) then
     mkdir -p ${raw}/PAC/${lyear}/${month}
endif

cd $raw/PAC/$lyear/$month/
wget  --no-check-certificate --auth-no-challenge=on -r --accept "nc" --reject "index.html*" --no-directories -np -e robots=off -N -nv 'https://data-argo.ifremer.fr/geo/pacific_ocean/'$lyear'/'$month'/';

set latest = ` ls -l * | tail -1 | awk '{printf "%s",substr($9,1,8)}' `;

echo 'TODAY: ' $year$month$day $hour':'$min
echo ' '
echo 'DATA:  ' $latest 
echo ' '

# Process Data
cd $run
./run_argo_2.csh $idate

#
#  the end
#

