#!/bin/csh
#
# run_aviso_1.csh: D-BOSS Script to ftp data

#set main = /gpfsm/dhome/gmaofcst/ODAS/OBS/V2/
#set main = /discover/nobackup/lren1/SANDBOX/lr001/ocean_das/UMD_Etc/pre_proc/NRT/AVISO
set MYNAME = $0
set main=/discover/nobackup/lren1/odas_pre_proc
cd $main/AVISO
rm -f status
source config.csh

set idate=$1

#set rawdir=$2
set rawdir=$raw

echo 'Date: ' $idate
set year=`echo $idate | cut -c1-4`
set month=`echo $idate | cut -c5-6`
set day=`echo $idate | cut -c7-8`
set hour=00
set min=00

# ftp last month too
  set lmon = `expr $month - 1`
  set lyear = $year
  if ($lmon < 1) then
    set lyear = `expr $year - 1`
    set lmon = 12
  endif
  if ($lmon < 10) then
    set lmon = 0$lmon
  endif

# FTP Data
#############################################################################################################################

#------------------------------------------Satellite Altika-------------------------------------------------------------
set sat = 'al'

if (! -d ${rawdir}/${sat}/${year}) then
     mkdir -p ${rawdir}/${sat}/${year}
endif

cd ${rawdir}/${sat}/${year}
rm -f *l3_${year}${month}*.nc
/usr/bin/wget --user=rkovach1 --password=rnt072796 "ftp://nrt.cmems-du.eu/Core/SEALEVEL_GLO_PHY_L3_NRT_OBSERVATIONS_008_044/dataset-duacs-nrt-global-${sat}-phy-l3/${year}/${month}/*l3_${year}${month}*.nc"
set latest = `ls -ltr * | tail -1 | awk '{printf "%s",substr($9,31,8)}'`
echo 'TODAY: ' $idate
echo ' '
echo 'DATA:  ' $latest
echo ' '
if ($latest >= $idate) then
  echo "AVISO ${sat} File is Up to Date"
else
   echo "AVISO ${sat} File is NOT up to Date"
   set emessage = "AVISO ${sat} File is NOT up to Date"
   set ecode = 4
   set ERROR_ID = "gmaofcst"
   /home/dao_ops/GEOSadas-CURRENT/GEOSadas/Linux/bin/Err_Log.pl -N ${MYNAME}.job.${idate} -C $ecode -I $ERROR_ID -X $MYNAME -E 5 -D "${MYNAME}.job.${idate} ${emessage}"
   exit 1
endif

# Last month
cd ${rawdir}/${sat}/${lyear}
rm -f *l3_${lyear}${lmon}*.nc
/usr/bin/wget --user=rkovach1 --password=rnt072796 "ftp://nrt.cmems-du.eu/Core/SEALEVEL_GLO_PHY_L3_NRT_OBSERVATIONS_008_044/dataset-duacs-nrt-global-${sat}-phy-l3/${lyear}/${lmon}/*l3_${lyear}${lmon}*.nc"

echo 'done for Altika'

## Skip c2n with the if(0) construct.  Uncomment this IF to skip.
#if (0) then

#------------------------------------------Satellite CryoSate-2n-------------------------------------------------------------
set sat = 'c2n'
if (! -d ${rawdir}/${sat}/${year}) then
     mkdir -p ${rawdir}/${sat}/${year}
endif

cd ${rawdir}/${sat}/${year}
rm -f *l3_${year}${month}*.nc
/usr/bin/wget --user=rkovach1 --password=rnt072796 "ftp://ftp.cmems-du.eu/Core/SEALEVEL_GLO_PHY_L3_NRT_OBSERVATIONS_008_044/dataset-duacs-nrt-global-${sat}-phy-l3/${year}/${month}/*l3_${year}${month}*.nc"
set latest = `ls -ltr * | tail -1 | awk '{printf "%s",substr($9,32,8)}'`
echo 'TODAY: ' $idate
echo ' '
echo 'DATA:  ' $latest
echo ' '
if ($latest >= $idate) then
  echo "AVISO ${sat} File is Up to Date"
else
   echo "AVISO ${sat} File is NOT up to Date"
   echo "DEBUG: here"
   set emessage = "AVISO ${sat} File is NOT up to Date"
   set ecode = 4
   set ERROR_ID = "gmaofcst"
   /home/dao_ops/GEOSadas-CURRENT/GEOSadas/Linux/bin/Err_Log.pl -N ${MYNAME}.job.${idate} -C $ecode -I $ERROR_ID -X $MYNAME -E 5 -D "${MYNAME}.job.${idate} ${emessage}"
  exit 1
endif

# Last month
cd ${rawdir}/${sat}/${lyear}
rm -f *l3_${lyear}${lmon}*.nc
/usr/bin/wget --user=rkovach1 --password=rnt072796 "ftp://ftp.cmems-du.eu/Core/SEALEVEL_GLO_PHY_L3_NRT_OBSERVATIONS_008_044/dataset-duacs-nrt-global-${sat}-phy-l3/${lyear}/${lmon}/*l3_${lyear}${lmon}*.nc"
# ENDIF FOR SKIP C2N
#endif

echo 'done for c2n'
#------------------------------------------Satellite JASON-3-------------------------------------------------------------


set sat = 'j3n'
if (! -d ${rawdir}/${sat}/${year}) then
     mkdir -p ${rawdir}/${sat}/${year}
endif

cd ${rawdir}/${sat}/${year}
rm -f *l3_${year}${month}*.nc
/usr/bin/wget --user=rkovach1 --password=rnt072796 "ftp://ftp.cmems-du.eu/Core/SEALEVEL_GLO_PHY_L3_NRT_OBSERVATIONS_008_044/cmems_obs-sl_glo_phy-ssh_nrt_${sat}-l3-duacs_PT1S/${year}/${month}/*l3_${year}${month}*.nc"
set latest = `ls -ltr * | tail -1 | awk '{printf "%s",substr($9,32,8)}'`
echo 'TODAY: ' $idate
echo ' '
echo 'DATA:  ' $latest
echo ' '
if ($latest >= $idate) then
  echo "AVISO ${sat} File is Up to Date"
else
   echo "AVISO ${sat} File is NOT up to Date"
   set emessage = "AVISO ${sat} File is NOT up to Date"
   set ecode = 4
   set ERROR_ID = "gmaofcst"
   /home/dao_ops/GEOSadas-CURRENT/GEOSadas/Linux/bin/Err_Log.pl -N ${MYNAME}.job.${idate} -C $ecode -I $ERROR_ID -X $MYNAME -E 5 -D "${MYNAME}.job.${idate} ${emessage}"
   exit 1
endif

# Last month
cd ${rawdir}/${sat}/${lyear}
rm -f *l3_${lyear}${lmon}*.nc
/usr/bin/wget --user=rkovach1 --password=rnt072796 "ftp://ftp.cmems-du.eu/Core/SEALEVEL_GLO_PHY_L3_NRT_OBSERVATIONS_008_044/cmems_obs-sl_glo_phy-ssh_nrt_${sat}-l3-duacs_PT1S/${lyear}/${lmon}/*l3_${lyear}${lmon}*.nc"

echo 'done for J3n'
#------------------------------------------Satellite Sentinel-3a-------------------------------------------------------------

set sat = 's3a'
if (! -d ${rawdir}/${sat}/${year}) then
     mkdir -p ${rawdir}/${sat}/${year}
endif

cd ${rawdir}/${sat}/${year}
rm -f *l3_${year}${month}*.nc
/usr/bin/wget --user=rkovach1 --password=rnt072796 "ftp://ftp.cmems-du.eu/Core/SEALEVEL_GLO_PHY_L3_NRT_OBSERVATIONS_008_044/dataset-duacs-nrt-global-${sat}-phy-l3/${year}/${month}/*l3_${year}${month}*.nc"
set latest = `ls -ltr * | tail -1 | awk '{printf "%s",substr($9,32,8)}'`
echo 'TODAY: ' $idate
echo ' '
echo 'DATA:  ' $latest
echo ' '
if ($latest >= $idate) then
  echo "AVISO ${sat} File is Up to Date"
else
   echo "AVISO ${sat} File is NOT up to Date"
   set emessage = "AVISO ${sat} File is NOT up to Date"
   set ecode = 4
   set ERROR_ID = "gmaofcst"
   /home/dao_ops/GEOSadas-CURRENT/GEOSadas/Linux/bin/Err_Log.pl -N ${MYNAME}.job.${idate} -C $ecode -I $ERROR_ID -X $MYNAME -E 5 -D "${MYNAME}.job.${idate} ${emessage}"
   exit 1
endif

# Last month
cd ${rawdir}/${sat}/${lyear}
rm -f *l3_${lyear}${lmon}*.nc
/usr/bin/wget --user=rkovach1 --password=rnt072796 "ftp://ftp.cmems-du.eu/Core/SEALEVEL_GLO_PHY_L3_NRT_OBSERVATIONS_008_044/dataset-duacs-nrt-global-${sat}-phy-l3/${lyear}/${lmon}/*l3_${lyear}${lmon}*.nc"

echo 'done for s3a'

#------------------------------------------Satellite Sentinel-6a-------------------------------------------------------------

set sat = 's6a'
if (! -d ${rawdir}/${sat}/${year}) then
     mkdir -p ${rawdir}/${sat}/${year}
endif

cd ${rawdir}/${sat}/${year}
rm -f *l3_${year}${month}*.nc
/usr/bin/wget --user=rkovach1 --password=rnt072796 "ftp://ftp.cmems-du.eu/Core/SEALEVEL_GLO_PHY_L3_NRT_OBSERVATIONS_008_044/cmems_obs-sl_glo_phy-ssh_nrt_${sat}-hr-l3-duacs_PT1S/${year}/${month}/*l3_${year}${month}*.nc"
set latest = `ls -ltr * | tail -1 | awk '{printf "%s",substr($9,35,8)}'`
echo 'TODAY: ' $idate
echo ' '
echo 'DATA:  ' $latest
echo ' '
if ($latest >= $idate) then
  echo "AVISO ${sat} File is Up to Date"
else
   echo "AVISO ${sat} File is NOT up to Date"
   set emessage = "AVISO ${sat} File is NOT up to Date"
   set ecode = 4
   set ERROR_ID = "gmaofcst"
   /home/dao_ops/GEOSadas-CURRENT/GEOSadas/Linux/bin/Err_Log.pl -N ${MYNAME}.job.${idate} -C $ecode -I $ERROR_ID -X $MYNAME -E 5 -D "${MYNAME}.job.${idate} ${emessage}"
   exit 1
endif

# Last month
cd ${rawdir}/${sat}/${lyear}
rm -f *l3_${lyear}${lmon}*.nc
/usr/bin/wget --user=rkovach1 --password=rnt072796 "ftp://ftp.cmems-du.eu/Core/SEALEVEL_GLO_PHY_L3_NRT_OBSERVATIONS_008_044/cmems_obs-sl_glo_phy-ssh_nrt_${sat}-hr-l3-duacs_PT1S/${lyear}/${lmon}/*l3_${lyear}${lmon}*.nc"

echo 'done for s6a'

echo 'TODAY: ' $year$month$day $hour
echo ' '
echo 'DATA:  ' $latest 


# Make Assim File
#########################
cd $run

rm -f $assim/ADT_TRK_AL_$year.nc
rm -f $assim/ADT_TRK_C2N_$year.nc
rm -f $assim/ADT_TRK_J3N_$year.nc
rm -f $assim/ADT_TRK_S3A_$year.nc
rm -f $assim/ADT_TRK_S6A_$year.nc



./run_aviso_2.py $year AL
./run_aviso_2.py $year C2N
./run_aviso_2.py $year J3N
./run_aviso_2.py $year S3A
./run_aviso_2.py $year S6A


set v3dir = $assim

cd $v3dir
$run/adt_sigos_sles12.x ADT_TRK_AL_$year.nc
$run/adt_sigos_sles12.x ADT_TRK_C2N_$year.nc
$run/adt_sigos_sles12.x ADT_TRK_J3N_$year.nc
$run/adt_sigos_sles12.x ADT_TRK_S3A_$year.nc
$run/adt_sigos_sles12.x ADT_TRK_S6A_$year.nc



cd $run
./dboss_check_aviso_v3.py





