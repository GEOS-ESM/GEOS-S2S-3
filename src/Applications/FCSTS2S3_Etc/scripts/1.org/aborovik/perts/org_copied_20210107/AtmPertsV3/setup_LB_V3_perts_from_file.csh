#!/bin/csh -vx

########################################################################
# script to generate random ATM perturbations to the lagged V3 restarts
########################################################################
limit stacksize unlimited
limit vmemoryuse unlimited

# Start: User Defined Parameters      
set pertdata = $1
set ananame = $2
# End: User Defined Parameters

set ensnums = {201,202,203,204} 

# location of experimental perturbations 

# populate /data directory (when all V# restarts are in one place)
mkdir -p $XDIR/AtmPertsV3/data_${ananame}/${pertdata}/ens200/
ln -s $RESDIR/moist_internal_rst.${pertdata}_2100z.nc4 $XDIR/AtmPertsV3/data_${ananame}/${pertdata}/ens200/moist_internal_rst
ln -s $RESDIR/fvcore_internal_rst.${pertdata}_2100z.nc4 $XDIR/AtmPertsV3/data_${ananame}/${pertdata}/ens200/fvcore_internal_rst
#exit

# Set perturb  directory in the driver directory
setenv XBVDIR $XDIR/AtmPertsV3/perts
mkdir -p $XBVDIR
#cp perturb_aogcm_test.exe $XBVDIR
cp -p moist_pertV3single.sh $XBVDIR
cp -p fvcore_pertV3single.sh $XBVDIR

# this is where we get samples for perturbations (odas analysis)
#set RESTARTS = '/gpfsm/dnb02/projects/p58/aogcm/g5fcst/s2sv3/netcdf/V3/'${ananame}'_hindcast_restarts_extract/'
set RESTARTS = $RESDIR # these directories are usually the same

cd $XBVDIR
pwd

# get perturbations from random (precomputed) dates ######### LATER this will be changed to dates from the continuous V3 analysis as the restarts
   
############### this is where the restarts to be perturbed must be #################################
# link to ens200'
set NETCDF_DIR = $XDIR'/AtmPertsV3/data_'${ananame}'/'$pertdata'/ens200/'
if ( ! -e $NETCDF_DIR/fvcore_internal_rst) then
   echo "REQUIRED $NETCDF_DIR fvcore_internal_rst is MISSING"
   exit
endif
# These files will be perturbed, i.e. added and subtracted from
echo LINK IC DATE ANALYSIS RESTARTS in cdata
cp -p $NETCDF_DIR/fvcore_internal_rst fort.13
cp -p $NETCDF_DIR/moist_internal_rst fort.23

@ nt = 1
foreach enscnt ($ensnums) 
# These files must be premade: separation in days and scaling factors that match
   set datsepfac = `cat ../files_${ananame}/rand_5_pert${pertdata}.txt | head -n $nt | tail -n 1`
   echo $datsepfac
   
   set rdat = `echo $datsepfac | cut -c1-8`
   set difr = `echo $datsepfac | cut -d' ' -f2`
   set fact = `echo $datsepfac | cut -d' ' -f3`
   echo 'rdat fact difr' $rdat $fact $difr
   
   @ nens = $enscnt

   set PERT_DIR = $XDIR'/AtmPertsV3/data_'${ananame}'/'$pertdata'/ens'$nens'/'
   mkdir -p $PERT_DIR

   @ n = $nt
   set rd = $rdat
   set rday = `echo $rd | cut -c7-8`
   set rmon = `echo $rd | cut -c5-6`
   set ryrn = `echo $rd | cut -c1-4`
     
   set numdays = 31
   if ( $rmon == 11 | $rmon == 06 | $rmon == 04 | $rmon == 09 ) set numdays = 30
   if ( $rmon == 02 ) set numdays = 28

   @ tmpday = $rday + $difr
   if ( $tmpday > $numdays ) then
     @ ptmpday = $tmpday - $numdays
     @ ptmpmon = $rmon + 1
     if ($ptmpmon > 12 ) then
       @ ptmpmon = 1
       @ pertyrn = $ryrn + 1
     else  
       @ pertyrn = $ryrn
     endif
   else
     @ ptmpday = $tmpday
     @ ptmpmon = $rmon
     @ pertyrn = $ryrn
   endif
   set pertday = $ptmpday
   set pertmon = $ptmpmon
   if ( $ptmpday < 10 ) set pertday = 0$ptmpday
   if ( $ptmpmon < 10 ) set pertmon = 0$ptmpmon
   set pd = $pertyrn$pertmon$pertday

   set nfile1 = fvcore_internal_rst.${rd}_2100z.nc4
   set pfile1 = fvcore_internal_rst.${pd}_2100z.nc4
   set nfile2 = moist_internal_rst.${rd}_2100z.nc4
   set pfile2 = moist_internal_rst.${pd}_2100z.nc4
   echo $n DATA $difr $fact $pfile1 $nfile1

   cp -p $RESTARTS/${pfile2} fort.22
   cp -p $RESTARTS/${nfile2} fort.21
   cp -p $RESTARTS/${pfile1} fort.12
   cp -p $RESTARTS/${nfile1} fort.11

   echo PERTURB moist and fvcore
   moist_pertV3single.sh ${fact}
   fvcore_pertV3single.sh ${fact}

   /bin/mv fort.41 $PERT_DIR/moist_internal_rst
   /bin/mv fort.31 $PERT_DIR/fvcore_internal_rst

   /bin/rm -f fort.11
   /bin/rm -f fort.12
   /bin/rm -f fort.21
   /bin/rm -f fort.22
  
   @ nt = $nt + 1

end

cd $XDIR

