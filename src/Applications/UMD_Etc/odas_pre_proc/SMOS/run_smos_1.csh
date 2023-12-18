#!/bin/csh
#
# NRT ftp://sealion.jpl.nasa.gov/pub/outgoing/smap/nrt-delayed/2018/09/15/
#
# Just do ftp everyday

#
cd /discover/nobackup/lren1/odas_pre_proc/SMOS
source config.csh

set year=`date +"20%y"`
set month=`date +%m`
set day=`date +%d`

echo $year $month $day

# Run for yesterday
set day=`expr $day - 1`
if ($day < 10) then
  set day=0$day
endif
if ($day < 1) then
  set day=1
  set month=`expr $month - 1`
endif
if ($month < 1) then
  set month=12
  set year=`expr $year - 1`
endif

echo $year $month $day

set yday=`date +%j`
set yday=`expr $yday - 1`

  if ($yday < 10) then
    set yday = 00$yday
  endif

  if ($yday >= 10 & $yday < 100) then
    set yday = 0$yday
  endif

echo $year $month $day $yday 


set ins = 'SMOS'
set ver = 'L32Q'
set raw = '/discover/nobackup/lren1/pre_proc/NRT/SMOS/RAW/'$ver
echo $raw

# FTP Data
if (! -e $raw/$year )		mkdir -p $raw/$year
if (! -e $raw/$year/$yday ) 	mkdir -p $raw/$year/$yday

cd $raw/$year/$yday 
/usr/bin/wget "ftp://ext-catds-cpdc:catds2010@ftp.ifremer.fr/Ocean_products/GRIDDED/L3OS/OPER/NRT_CSF2QD/${year}/${yday}/SM_OPER_NRT_CSF2QD_*.tgz"
/usr/bin/wget "ftp://ext-catds-cpdc:catds2010@ftp.ifremer.fr/Ocean_products/GRIDDED/L3OS/OPER/NRT_CSF2QA/${year}/${yday}/SM_OPER_NRT_CSF2QA*.tgz"

  gunzip *gz
  cat *.tar | tar -xvf - -i
  rm -f *.tar



#
#  the end
#



