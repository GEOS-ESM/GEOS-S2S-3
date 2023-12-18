#!/bin/csh -vx

################################
# script to generate random OCN perturbations to the lagged V3 restarts
################################
limit stacksize unlimited
limit vmemoryuse unlimited

# Start: User Defined Parameters      
set pertdata = $1
set ananame = $2
# End: User Defined Parameters

module load nco/4.8.1

# Set perturb  directory in the driver directory
setenv XBVDIR $XDIR/OcnPertsV3/perts_scaleT_${ananame}_V3_${pertdata}_21z
mkdir -p $XBVDIR
rm -f $XBVDIR/perturb_ogcm_scaleT.exe
cp perturb_ogcm_scaleT.exe $XBVDIR

# Restarts that the perturbations will be centered at
setenv RSTDIR_C $XBVDIR/cdata
# Positively perturbed 
setenv RSTDIR_PBV $XBVDIR/pdata 
# Negatively perturbed 
setenv RSTDIR_NBV $XBVDIR/ndata 

# Create the PT directories
if ( -e $XBVDIR/PANICSTOP_PT ) /bin/rm -f $XBVDIR/PANICSTOP_PT
#if ( -e $XBVDIR/cdata ) /bin/rm -rf $XBVDIR/?data
mkdir -p $XBVDIR/cdata
mkdir -p $XBVDIR/ndata
mkdir -p $XBVDIR/pdata

# this is where we get samples for perturbations (odas analysis)
set RESTARTS = $RESDIR/RESTART/

# this is where the restarts to be perturbed must be
# FOR NOW, V3 ODAS restarts are all in the same place: the unperturbed and those to create perturbations from
set NETCDF_DIR = $RESTARTS

if ( ! -e ${NETCDF_DIR}${pertdata}_2100z.ocean_temp_salt.res.nc) then
  echo "REQUIRED ${NETCDF_DIR}${pertdata}_2100z.ocean_temp_salt.res.nc is MISSING"
  exit
endif

# These files will be perturbed, i.e. added and subtracted from
echo LINK IC DATE ANALYSIS RESTARTS in cdata
ln -f $NETCDF_DIR/${pertdata}_2100z.ocean_temp_salt.res.nc ${RSTDIR_C}/aocean_temp_salt.res.nc
ln -f $NETCDF_DIR/${pertdata}_2100z.ocean_velocity.res.nc ${RSTDIR_C}/aocean_velocity.res.nc
cd $XBVDIR
pwd

@ nt = 1
while ( $nt <= 10 ) 
  @ n = $nt 
  set datsepfac = `cat ../files_${ananame}/rand_10_pert${pertdata}.txt | head -n $nt | tail -n 1`
  echo $datsepfac
   
  set rdat = `echo $datsepfac | cut -c1-8`
  set difr = `echo $datsepfac | cut -d' ' -f2`
  set fact = `echo $datsepfac | cut -d' ' -f3`
  echo rdat fact difr $rdat $fact $difr
      
#########################################################################################################################
 # we want to double the size of the perturbations - I don't know how to do this in csh script - modified fortran code
#########################################################################################################################
  echo $n 'fact ' $fact 
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

  echo SAVE CURRENT ANALYSIS RESTARTS in ndata as templates
  /bin/cp -L $NETCDF_DIR/${pertdata}_2100z.ocean_temp_salt.res.nc ${RSTDIR_NBV}/ocean_temp_salt.res.nc
  /bin/cp -L $NETCDF_DIR/${pertdata}_2100z.ocean_velocity.res.nc ${RSTDIR_NBV}/ocean_velocity.res.nc

  echo SAVE CURRENT ANALYSIS RESTARTS in pdata as templates
  /bin/cp -L $NETCDF_DIR/${pertdata}_2100z.ocean_temp_salt.res.nc ${RSTDIR_PBV}/ocean_temp_salt.res.nc
  /bin/cp -L $NETCDF_DIR/${pertdata}_2100z.ocean_velocity.res.nc ${RSTDIR_PBV}/ocean_velocity.res.nc

# Get all restarts needed to ndata, pdata (ndata&pdata will be updated)
  echo LINK FIRST SAMPLE OF RESTARTS in ndata
  ln -sfv  $RESTARTS/${rd}_2100z.ocean_temp_salt.res.nc ${RSTDIR_NBV}/uocean_temp_salt.res.nc
  ln -sfv  $RESTARTS/${rd}_2100z.ocean_velocity.res.nc ${RSTDIR_NBV}/uocean_velocity.res.nc
  
  echo LINK SECOND SAMPLE OF RESTARTS in pdata
  ln -sfv  $RESTARTS/${pd}_2100z.ocean_temp_salt.res.nc ${RSTDIR_PBV}/uocean_temp_salt.res.nc
  ln -sfv  $RESTARTS/${pd}_2100z.ocean_velocity.res.nc ${RSTDIR_PBV}/uocean_velocity.res.nc

  echo $fact > coef.dat
  echo PERTURB RESTARTS IN ndata and pdata

  ./perturb_ogcm_scaleT.exe > $XBVDIR/perturb.out

  set pertstat = $status
  set PTR = `grep GENERATED $XBVDIR/perturb.out | wc -l`
  if ( $PTR != 2 ) set pertstat = 99

  if ( $pertstat != 0 ) then
     touch $XBVDIR/PANICSTOP_PT
     exit
  endif

  if ( ! -e $XBVDIR/perturb.out ) then
     touch $XBVDIR/PANICSTOP_PT
     exit
  endif

  set nnn = `grep ErrVAR perturb.out | wc -l`
  if ( $nnn > 0 ) then
     echo ERROR - ErrVAR RANGE ERROR
     touch $XBVDIR/PANICSTOP_PT
     exit
  endif

  if ( ! -e $XBVDIR/PANICSTOP_PT ) then
    mv ${RSTDIR_NBV}/ocean_temp_salt.res.nc ${RSTDIR_NBV}/${nt}_ocean_temp_salt.res.nc
    mv ${RSTDIR_NBV}/ocean_velocity.res.nc ${RSTDIR_NBV}/${nt}_ocean_velocity.res.nc

    mv ${RSTDIR_PBV}/ocean_temp_salt.res.nc ${RSTDIR_PBV}/${nt}_ocean_temp_salt.res.nc
    mv ${RSTDIR_PBV}/ocean_velocity.res.nc ${RSTDIR_PBV}/${nt}_ocean_velocity.res.nc

# this is needed for NAS runs; checksum craches the model, needs to be removed from the original analysis restarts
   ncatted -h -O -a checksum,"v",d,, ${RSTDIR_NBV}/${nt}_ocean_velocity.res.nc
   ncatted -h -O -a checksum,"v",d,, ${RSTDIR_PBV}/${nt}_ocean_velocity.res.nc
   ncatted -h -O -a checksum,"u",d,, ${RSTDIR_NBV}/${nt}_ocean_velocity.res.nc
   ncatted -h -O -a checksum,"u",d,, ${RSTDIR_PBV}/${nt}_ocean_velocity.res.nc
   ncatted -h -O -a checksum,"temp",d,, ${RSTDIR_NBV}/${nt}_ocean_temp_salt.res.nc
   ncatted -h -O -a checksum,"temp",d,, ${RSTDIR_PBV}/${nt}_ocean_temp_salt.res.nc
   ncatted -h -O -a checksum,"salt",d,, ${RSTDIR_NBV}/${nt}_ocean_temp_salt.res.nc
   ncatted -h -O -a checksum,"salt",d,, ${RSTDIR_PBV}/${nt}_ocean_temp_salt.res.nc

  @ nt = $nt + 1
end
cd $XDIR

