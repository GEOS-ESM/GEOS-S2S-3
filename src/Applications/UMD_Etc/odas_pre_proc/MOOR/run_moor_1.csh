#!/bin/csh
#		
# run_moor_1.csh: D-BOSS Script to ftp data

if($#argv != 1) then
#  echo "e.g., ./run_moor_1.csh 20220516 /gpfsm/dnb78s2/projects/p26/ehackert/ODAS/OBS/V3 "
   echo "need date (yyyymmdd)"
   exit
endif

#set main = /gpfsm/dhome/gmaofcst/ODAS/OBS/V3/
set main = /discover/nobackup/lren1/odas_pre_proc
cd $main/MOOR/
rm -f status
source config.csh

set idate=$1
set rawdir=$rawdir

set pathraw_all = $rawdir
set pathraw_tao = ${rawdir}/TAO
set pathraw_ram = ${rawdir}/RAMA
set pathraw_pir = ${rawdir}/PIRATA

echo 'Date: ' $idate
set year=`echo $idate | cut -c1-4`
set month=`echo $idate | cut -c5-6`
set day=`echo $idate | cut -c7-8`
set hour=00
set min=00


#echo 'TODAY: ' $year$month$day $hour':'$min
echo ' '

# We don't need to do the ftp since V2 is running this
set do_ftp=1

if ($do_ftp == 1) then

cd $pathraw_all/SALT
rm -f *
#wget --user taopmelftp --password G10b@LCh@Ng3 --secure-protocol=auto "ftp://taopmelftp@ftp.pmel.noaa.gov/ascii/sites/daily/s*_dy.ascii"
wget -r -np -k -nd -e robots=off -nv https://www.pmel.noaa.gov/tao/taoweb/taopmelftp/ascii/sites/daily/ -A "s*_dy.ascii"
echo 'downloading salinity'
rm -f ss* sw* sss*
rm -f $pathraw_tao/SALT/*
rm -f $pathraw_pir/SALT/*
rm -f $pathraw_ram/SALT/*
cp *.ascii $pathraw_tao/SALT/
cp *.ascii $pathraw_pir/SALT/
cp *.ascii $pathraw_ram/SALT/

cd $pathraw_all/TEMP
rm -f *
#wget --user taopmelftp --password G10b@LCh@Ng3 --secure-protocol=auto "ftp://taopmelftp@ftp.pmel.noaa.gov/ascii/sites/daily/t*_dy.ascii"
wget -r -np -k -nd -e robots=off -nv https://www.pmel.noaa.gov/tao/taoweb/taopmelftp/ascii/sites/daily/ -A "t*_dy.ascii"
rm -f tau*
rm -f $pathraw_tao/TEMP/*
rm -f $pathraw_pir/TEMP/*
rm -f $pathraw_ram/TEMP/*
cp *.ascii $pathraw_tao/TEMP
cp *.ascii $pathraw_pir/TEMP
cp *.ascii $pathraw_ram/TEMP



# TAO 
##################################################################################
  cd $pathraw_tao/SALT
  rm -f sss* ss* sw*
  rm -f s0n0e_dy.ascii *s55e_dy.ascii *n55e_dy.ascii
  rm -f *n10w_dy.ascii *s10w_dy.ascii
  rm -f *n3*w_dy.ascii *s3*w_dy.ascii
  rm -f *n2*w_dy.ascii *s2*w_dy.ascii
  rm -f *n6*e_dy.ascii *s6*e_dy.ascii
  rm -f *n7*e_dy.ascii *s7*e_dy.ascii
  rm -f *n8*e_dy.ascii *s8*e_dy.ascii
  rm -f *n9*e_dy.ascii *s9*e_dy.ascii
  rm -f s13n114e_dy.ascii s15n115e_dy.ascii s18n116e_dy.ascii s20n117e_dy.ascii
  rm -f s39s30e_dy.ascii
  rm -f s8s100e_dy.ascii
  rm -f s3.5n* s38n146.5*
  rm -f s0.7*110w_dy.ascii
  rm -f s0n110.5w_dy.ascii

  cd $pathraw_tao/TEMP
  rm -f tau*
  rm -f t0n0e_dy.ascii *s55e_dy.ascii *n55e_dy.ascii *s57e* *s100e*
  rm -f *n10w_dy.ascii *s10w_dy.ascii
  rm -f *n3*w_dy.ascii *s3*w_dy.ascii
  rm -f *n2*w_dy.ascii *s2*w_dy.ascii
  rm -f *n6*e_dy.ascii *s6*e_dy.ascii
  rm -f *n7*e_dy.ascii *s7*e_dy.ascii
  rm -f *n8*e_dy.ascii *s8*e_dy.ascii
  rm -f *n9*e_dy.ascii *s9*e_dy.ascii
  rm -f t13n114e_dy.ascii t15n115e_dy.ascii t18n116e_dy.ascii t20n117e_dy.ascii
  rm -f t39s30e_dy.ascii
  rm -f t8s100e_dy.ascii
  rm -f t3.5n* t38n146.5*
  rm -f t0.7*110w_dy.ascii
  rm -f t0n110.5w_dy.ascii

  cd $run/TAO
  ls -1 $pathraw_tao/TEMP/*ascii > tao_files.list 
  ls -1 $pathraw_tao/TEMP/*ascii > tao_temp_files.list 
  ls -1 $pathraw_tao/SALT/*ascii > tao_salt_files.list 

# PIRATA 
##################################################################################
  cd $pathraw_pir/SALT
  rm -f sss* ss* sw*
  rm -f *n1*e* *s1*e* *n9*e* *s9*e* *80.5e* *65e*
  rm -f *110* *125* *140* *155* *170* *180* *95* *132* *147* *157* *108w* *152w* *153w* *85w* *67e* *55e* *150w* *57e*

  cd $pathraw_pir/TEMP
  rm -f tau*
  rm -f *n1*e* *s1*e* *n9*e* *s9*e* *80.5e* *65e*
  rm -f *110* *125* *140* *155* *170* *180* *95*  *132* *147* *157* *108w* *152w* *153w* *85w* *67e* *55e* *150w* *57e*

  cd $run/PIRATA
  ls -1 $pathraw_pir/TEMP/*ascii > pir_temp_files.list 
  ls -1 $pathraw_pir/SALT/*ascii > pir_salt_files.list 

# RAMA
##################################################################################
  cd $pathraw_ram/SALT
  rm -f sss* ss* sw*
  rm -f *w*
  rm -f *n13* *s13*
  rm -f *n14* *s14*
  rm -f *n15* *s15*
  rm -f *n16* *s16*
  rm -f *n17* *s17*
  rm -f *n0e* *s0e*
  rm -f *n8e* *s8e*

  cd $pathraw_ram/TEMP
  rm -f tau*
  rm -f *w*
  rm -f *n13* *s13*
  rm -f *n14* *s14*
  rm -f *n15* *s15*
  rm -f *n16* *s16*
  rm -f *n17* *s17*
  rm -f *n0e* *s0e*
  rm -f *n8e* *s8e*

  cd $run/RAMA
  ls -1 $pathraw_ram/TEMP/*ascii > rama_temp_files.list 
  ls -1 $pathraw_ram/SALT/*ascii > rama_salt_files.list 
endif

# Now run observation processing for each type of mooring
cd $run
pwd
#./run_moor_2.csh

#
#  the end
#



