#!/bin/csh -f

# ocean_das - run Ocean Static LETKF 
#
#      May 2016   Vernieres Initial script
#------------------------------------------------------------------

limit stacksize unlimited

source ../ocean_das_config
#module purge
#set MODS = `$GEOSDIR/Linux/bin/g5_modules modules`
#module load $MODS
#module load python/GEOSpyD/Ana2019.10_py3.7
#module load python/GEOSpyD/Min4.8.3_py3.8
#set BASEDIR = `$GEOSDIR/Linux/bin/g5_modules basedir`

#module use -a /discover/swdev/gmao_SIteam/modulefiles-SLES12
#module load mpi/impi-prov/19.1.0.166
#module swap mpi/impi mpi/impi/2021.2.0

#######################################################################
#               Set Analysis Date Time
####################################################################### 

set yyyy       = ${1} 
set mm         = ${2} 
set dd         = ${3} 
set hh         = ${4} 

set ANADIR  = oana-${yyyy}${mm}${dd}_${hh}

#######################################################################
#               Get Ocean Restarts
####################################################################### 


set DAHH = ${ODAS_IAU_TIME}          # Analyzed at IC time + 18hrs = Time for IAU

set center_ctrl       = ${EXPDIR}/scratch/${EXPID}.${OCNCTRL}.${yyyy}${mm}${dd}_${DAHH}00z.nc4   # T & S on native grid
set center_fname3d    = ${EXPDIR}/scratch/${EXPID}.${OCN3D}.${yyyy}${mm}${dd}_${DAHH}00z.nc4
set center_fname2d    = ${EXPDIR}/scratch/${EXPID}.${OCN2D}.${yyyy}${mm}${dd}_${DAHH}00z.nc4
set center_fname_cice = ${EXPDIR}/scratch/${EXPID}.${SEAICE}.${yyyy}${mm}${dd}_${DAHH}00z.nc4

#Convert T in center_ctrl in deg C
#  Bin's recipe 4/1/19 /home/bzhao/V3_Recipes
#${BASEDIR}/Linux/bin/ncap  -O -s "temp=(temp-273.15)" $center_ctrl $center_ctrl   # !!! DANGER !!! the background file at the time 
#${BASEDIR}/Linux/bin/ncap2 -O -s "temp=(temp-273.15)" $center_ctrl $center_ctrl   # !!! DANGER !!! the background file at the time 
#/discover/swdev/mathomp4/Baselibs/ESMA-Baselibs-4.0.11-SLES12/x86_64-unknown-linux-gnu/ifort_18.0.5.274-intelmpi_19.1.0.166-nco_2020Apr09_snapshot/Linux/bin/ncap2 -O -s "temp=(temp-273.15)" $center_ctrl $center_ctrl   # !!! DANGER !!! the background file at the time
/discover/swdev/gmao_SIteam/Baselibs/latest-mpiuni-SLES12/Linux/bin/ncap2 -O -s "temp=(temp-273.15)" $center_ctrl $center_ctrl   # !!! DANGER !!! the background file at the time
										 #                of the analysis is now in C
                                                                                 #                while the others are in K  

#######################################################################
#                 Create Experiment Sub-Directories
#######################################################################    

if (! -e $UMD_LETKFSCRDIR            ) mkdir -p $UMD_LETKFSCRDIR

###############################################################################
#   Move to Scratch Clean and Copy rc/exec neccesary to run the ocean das
###############################################################################

cd $UMD_LETKFSCRDIR

#Clean stuff
#-----------
rm -f  obs010*.dat
rm -f  NOUT-0*
rm -f  recenter*.nc
rm -rf ana bkg states
rm -f  *.out
rm -f  gmao-obs-*.nc
rm -f  *.txt
rm -rf ana
rm -rf bkg
rm -rf omfs
rm -rf omas

# Get resource files and binaries
#--------------------------------
rm -f grid_spec.nc
ln -s ${ODAS_RC}/BKGERR/anom-${ODAS_Nx}x${ODAS_Ny}x${ODAS_Nz}-${ODAS_GRID_TYPE}/grid_spec_${ODAS_Nx}x${ODAS_Ny}x${ODAS_Nz}.nc grid_spec.nc     # Link Ocean grid
cp -f $UMD_LETKFRC/* .                      # Copy namelists
cp -f $UMD_LETKFSRC/oceanda.x .             # LETKF
cp -f $UMD_LETKFSRC/oceanobsop.x .          # Observing operator
cp -f $UMD_LETKFSRC/oceanobs_nc2bin.x .     # gmao obs format to binary readable by oceanobsop.x 
cp -f $UMD_LETKFUTILS/ocean_recenter.x .    # recentering when running with static members 

# Need to change permissions of ,v in repository ...
chmod +x ${UMD_LETKFUTILS}/mean_ods.py
chmod +x ${UMD_LETKFUTILS}/rst2hist.py

echo '============================================='
echo 'Analysis valid for '${yyyy}${mm}${dd}' '${DAHH}
echo '============================================='

if (${DO_BIAS_CORR} == 'True') then
#######eric 11/29/22
        echo 'ERIC'
        echo 'pwd in ocean_das.csh just before ocean_recenter.csh'
        pwd
        ls -ltr center*.nc
        echo 'date is' $yyyy $mm $dd
        echo 'BIAS CORRECTING MODEL STATE BEFORE ANALYSIS'
        ${UMD_LETKFUTILS}/do_bias_correction.py center3d.nc $mm $dd
#######eric 11/29/22
endif

#source ${EXPDIR}/ocean_recenter.csh ${UMD_LETKFSCRDIR} K #C
source ${UMD_LETKFSCRIPTS}/ocean_recenter.csh ${UMD_LETKFSCRDIR} K #C

cp -r ${UMD_LETKFSCRDIR}/bkg ${UMD_LETKFSCRDIR}/ana

# Concatenate obs*.dat files from multiple observers
#---------------------------------------------------
set n = 0
while ( $n < $ODAS_Ne )
    @ n++
    if ($n<10) then
        set nnn = '00'$n
    endif
    if ($n>9) then
        set nnn = '0'$n
    endif
    if ($n>99) then
        set nnn = $n
    endif
    echo obs01${nnn}.dat
    cat ${EXPDIR}/ocean_das/${ANADIR}/ocean_observer_*/obs*${nnn}.dat > ${UMD_LETKFSCRDIR}/obs01${nnn}.dat
end	 

echo '============================================='
echo 'Run NCEP Ocean DAS'
echo '============================================='

# NOTE: DA-1, DA-2 & DA-3 are independent and could/should be done in parallel.  
set MLD_FILTER = False
set S_infl = 1.0

# DA-1: Assimilation of T, S, SST & ADT   
${UMD_LETKFUTILS}/ocean_letkf_nml.py   # Create namelist for oceanda.x

#  Bin's recipe 4119 (/home/bzhao/V3_Recipes)
#mpirun -np $ODAS_NPE_ocean ./oceanda.x
$RUN_CMD $ODAS_NPE_ocean ./oceanda.x

# DA-2: Assimilation of ice fraction
if (${ODAS_do_aice} == 'True') then
    rm -rf ${EXPDIR}/ocean_das/ana_ts
    cp -r  ${EXPDIR}/ocean_das/ana ${EXPDIR}/ocean_das/ana_ts #Save intermediate analysis
    ${UMD_LETKFUTILS}/ocean_letkf_nml.py -c T  # Use sea-ice specific localization length scale
#  Bin's recipe 4119 (/home/bzhao/V3_Recipes)
#   mpirun -np $ODAS_NPE_cice ./oceanda.x
#   $RUN_CMD $ODAS_NPE_cice ./oceanda.x
    $RUN_CMD $ODAS_NPE_ocean ./oceanda.x
    
    # Recombine analyses
    set n = 0
    while ( $n < $ODAS_Ne )
	@ n++
	if ($n<10) then
	    set nnn = '00'$n
	endif
	if ($n>9) then
	    set nnn = '0'$n
	endif
	if ($n>99) then
	    set nnn = $n
	endif
	echo obs01${nnn}.dat
	cp -f ${EXPDIR}/ocean_das/ana_ts/${nnn}/ocean_temp_salt.res.nc  ${EXPDIR}/ocean_das/ana/${nnn}/ocean_temp_salt.res.nc
	cp -f ${EXPDIR}/ocean_das/ana_ts/${nnn}/ocean_barotropic.res.nc ${EXPDIR}/ocean_das/ana/${nnn}/ocean_barotropic.res.nc
    end	 
endif

# DA-3: Assimilation of ice thickness
#cp ${UMD_LETKFRC}/input_hice.nml input.nml
#   Bin's recipe 4119 (/home/bzhao/V3_Recipes)
#mpirun -np 264 ./oceanda.x
#$RUN_CMD 264 ./oceanda.x

#cp -r ana ana_hice    

#exit

#   Bin's recipe 4119 (/home/bzhao/V3_Recipes)
#mpirun -np 84 ./oceanda.x #| tee oda.out
#$RUN_CMD 84 ./oceanda.x #| tee oda.out

# oceanda.x profiling, Nobs = 507,207 Ne = 20
#    CPUS     TIME (s)
#    16       223.6
#    32       100.46
#    64       65.18
#   128       50.56
#   256       49.5


# oceanda.x profiling, Nobs = 75,075 Ne = 20
#    CPUS     TIME (s)
#    16       45.74
#    20       41.65
#    32       44
#    64       
#   128       
#   256       
#######eric 11/29/22
echo 'ERIC'
echo 'pwd in ocean_das.csh just after analysis'
pwd
ls -ltr $EXPDIR/ocean_das/mean_ana_restart/incr.nc
echo 'date is' $yyyy $mm $dd
#######eric 11/29/22

# Post process
#-------------
echo '============================================='
echo 'Postprocess'
echo '============================================='
#get mom's namelist
cp -f $EXPDIR/scratch/input.nml $UMD_LETKFSCRDIR                        # Get MOM's namelist (used by ocean_moments.x))
#   Bin's recipe 4119 (/home/bzhao/V3_Recipes)
#mpirun -np ${ODAS_Ne} ${UMD_LETKFUTILS}/ocean_moments.x           # Creates stats of analysis under /mean_ana_restart/*.nc
$RUN_CMD ${ODAS_Ne} ${UMD_LETKFUTILS}/ocean_moments.x              # Creates stats of analysis under /mean_ana_restart/*.nc

if (${DO_BIAS_CORR} == 'True') then
#######eric 11/29/22
        echo 'ERIC'
        echo 'pwd in ocean_das.csh just after ocean_moments.x'
        pwd
        ls -ltr $EXPDIR/ocean_das/mean_ana_restart/incr.nc
        echo 'date is' $yyyy $mm $dd
        echo 'BIAS CORRECTING INCREMENT AFTER ANALYSIS'
        ${UMD_LETKFUTILS}/do_bias_correction_on_incr.py $EXPDIR/ocean_das/mean_ana_restart/incr.nc $mm $dd
#######eric 11/29/22
endif

###############################################################################
#   Mapping from ocean das grid to model grid
###############################################################################

echo '============================================='
echo 'Regridding analysis'
echo '============================================='    

set bkg_temp_salt = ${center_ctrl}
#   BIN VERSION
#set bkg_saltwater = $EXPDIR/scratch/saltwater_internal_rst
set bkg_saltwater = $EXPDIR/scratch/seaicethermo_internal_rst
set bkg_water     = $EXPDIR/scratch/saltwater_import_rst

if (${ODAS_do_aice} == 'True') then
    echo "Saving mean ocean-sea-ice analysis"
    ${UMD_LETKFUTILS}/ana2model.py ${OCEAN_Nx} ${OCEAN_Ny} ${OCEAN_Nz} $EXPDIR/ocean_das/mean_ana_restart/incr.nc ${bkg_temp_salt} ANA-ocean_temp_salt.res.nc $EXPDIR/ocean_das/mean_ana_restart/sea-ice.nc
else
    echo "Saving mean ocean analysis assuming no sea-ice update"
    ${UMD_LETKFUTILS}/ana2model.py ${OCEAN_Nx} ${OCEAN_Ny} ${OCEAN_Nz} $EXPDIR/ocean_das/mean_ana_restart/incr.nc ${bkg_temp_salt} ANA-ocean_temp_salt.res.nc
endif
cp -f ${bkg_temp_salt}  ${EXPDIR}/ocean_das/${ANADIR}/BKG-ocean_temp_salt.res.nc  # Save background (native grid)
#cp ANA-ocean_temp_salt.res.nc ${bkg_temp_salt}                                # Over-write background with analysis
cp -f ANA-ocean_temp_salt.res.nc ${EXPDIR}/ocean_das/${ANADIR}/                   # Save analysis (native grid)
cp -f ANA-seaice.nc ${EXPDIR}/ocean_das/${ANADIR}/	

#   BINS VERSION
set do_aice = '.false.'
set do_hice = '.false.'
set do_hsno = '.false.'
set ana_file = $SCRDIR/AICE_${yyyy}${mm}${dd}_1200z.nc
set ana_fmt = 'regular'
set hice_mask = '.false.'
set hice_file = hice_${yyyy}${mm}${dd}.nc

if ( $?ODAS_do_reinit ) then
  if (${ODAS_do_reinit} == 'True') then
    set ana_file = ANA-seaice.nc
    set ana_fmt = 'oletkf'
  endif 
endif 
if (${ODAS_do_aice_nudging} == 'True') then
   set do_aice = '.true.'
endif
if (${ODAS_do_hice} == 'True') then
   set do_hice = '.true.'
   if( ! $?HICE_DATADIR ) then
       echo 'HICE_DATADIR not defined, assuming hice data is in analysis files' 
       set ana_file = ANA-seaice.nc
       set ana_fmt = 'oletkf'
   else
       echo 'using hice data provided from HICE_DATADIR if available' 
       if ( -e $HICE_DATADIR/hice_${yyyy}${mm}${dd}.nc ) then
           /bin/cp -f $HICE_DATADIR/hice_${yyyy}${mm}${dd}.nc .
           if (${HICE_HASMASK} == 'True') then
               set hice_mask = '.true.' 
           endif
       else
           echo 'hice data not available, turn off HICE assimilation' 
           set do_hice = '.false.'
       endif   
   endif
endif

###############################################################################
#   Dump Sea-Ice analysis in saltwater/temp_salt
###############################################################################
#   BINS VERSION
#if (${ODAS_do_aice} == 'True') then
    cp -f $UMD_LETKFUTILS/anaice2rst.x .
    rm -f grid_spec.nc                    # Remove analysis grid
    ln -s $SCRDIR/INPUT/grid_spec.nc . # Link native ocean grid
    cp -f $SCRDIR/tile.data .
    cp -f $bkg_saltwater .
#  BINS VERSION
#    if( ! $?ODAS_do_reinit ) then
#       echo 'ODAS_do_reinit not defined, assuming False' 
#       ./anaice2rst.x -rst ${bkg_saltwater} -ana $SCRDIR/AICE_${yyyy}${mm}${dd}_1200z.nc -ana_fmt regular -do_aice .true. -do_all_1st .true. -hi_new 0.5 -do_adhoc_tw_adjustment .true. -do_hice .false. -do_hsno .false. -orst ANA-ocean_temp_salt.res.nc
#    else 
#        if (${ODAS_do_reinit} == 'True') then
#          ./anaice2rst.x -rst ${bkg_saltwater} -ana ANA-seaice.nc -ana_fmt oletkf -do_aice .true. -do_all_1st .true. -hi_new 0.5 -do_adhoc_tw_adjustment .true. -do_hice .false. -do_hsno .false. -orst ANA-ocean_temp_salt.res.nc
#        else
#          ./anaice2rst.x -rst ${bkg_saltwater} -ana $SCRDIR/AICE_${yyyy}${mm}${dd}_1200z.nc -ana_fmt regular -do_aice .true. -do_all_1st .true. -hi_new 0.5 -do_adhoc_tw_adjustment .true. -do_hice .false. -do_hsno .false. -orst ANA-ocean_temp_salt.res.nc
#        endif 
#    endif
#    cp $bkg_saltwater ${EXPDIR}/ocean_das/$ANADIR/saltwater_internal_rst_ORIG.nc4
#   cp saltwater_internal_rst_OUT.nc4 ${EXPDIR}/ocean_das/$ANADIR/
#   cp saltwater_internal_rst_OUT.nc4 ${bkg_saltwater}

    cp -f $bkg_water .
    ./anaice2rst.x -rst ${bkg_saltwater} -wrst ${bkg_water} -ana $ana_file -ana_fmt $ana_fmt -do_aice $do_aice -do_all_1st .true. -hi_new 0.5 -do_adhoc_tw_adjustment .true. -do_hice $do_hice -hice_file $hice_file -hice_mask $hice_mask -do_hsno $do_hsno -orst ANA-ocean_temp_salt.res.nc
if (${ODAS_do_aice_nudging} == 'True' || ${ODAS_do_hice} == 'True') then 
    cp -f $bkg_saltwater ${EXPDIR}/ocean_das/$ANADIR/seaicethermo_internal_rst_ORIG.nc4
    cp -f seaicethermo_internal_rst_OUT.nc4 ${EXPDIR}/ocean_das/$ANADIR/
endif
    cp -f ocean_temp_salt.res.OUT.nc ${EXPDIR}/ocean_das/$ANADIR/ANA-ocean_temp_salt.res.nc
    cp -f seaicethermo_internal_rst_OUT.nc4 ${bkg_saltwater}
    cp -f saltwater_import_rst_OUT.nc4 ${bkg_water}

    #Add sea-ice mass increment to eta_t in barotropic restart
    #set mass_incr = `cat seaice_mass_incr.txt`
    #echo 'Mass incr = '$mass_incr
#   Bin's recipe 4119 (/home/bzhao/V3_Recipes)
    #${BASEDIR}/Linux/bin/ncap  -O -s "eta_t=(eta_t+${mass_incr})" $SCRDIR/INPUT/ocean_barotropic.res.nc $SCRDIR/INPUT/ocean_barotropic.res.nc > add_si_mass.out
    #${BASEDIR}/Linux/bin/ncap2 -O -s "eta_t=(eta_t+${mass_incr})" $SCRDIR/INPUT/ocean_barotropic.res.nc $SCRDIR/INPUT/ocean_barotropic.res.nc > add_si_mass.out
#endif

###############################################################################
#   Compute increment for MOM's IAU
###############################################################################
cp -f ${EXPDIR}/ocean_das/$ANADIR/ANA-ocean_temp_salt.res.nc .
cp -f ${bkg_temp_salt} ./ocean_temp_salt.res.nc
#cp ${EXPDIR}/ocean_das/seaice_mass_incr.txt 
rm -f grid_spec.nc                    # Remove analysis grid
ln -s $SCRDIR/INPUT/grid_spec.nc . # Link native ocean grid
cp -f $SCRDIR/input.nml .             # Get MOM's input.nml
if (${ODAS_do_aice} == 'True') then
    echo "ocean_iau is now being applied (with mass increment)"
    $UMD_LETKFUTILS/ocean_iau.x -DO_ANA_INCR -DO_MASS_INCR
else
    echo "ocean_iau is now being applied"
    $UMD_LETKFUTILS/ocean_iau.x -DO_ANA_INCR
endif

#   scale or eliminate increment in a box
if (${DO_INCR_BOX} == 'True') then
    echo 'WESTLON '${WESTLON}' EASTLON '${EASTLON}' NORTHLAT '${NORTHLAT}' SOUTHLAT '${SOUTHLAT} 'from setenv in ocean_das_config'
    $UMD_LETKFUTILS/remove_incr_in_box.py temp_increment.nc ${yyyy} ${mm} ${dd} # cold blob box -70 -20 45 65 $yyyy $mm $dd
    $UMD_LETKFUTILS/remove_incr_in_box.py salt_increment.nc ${yyyy} ${mm} ${dd}
    echo 'REMOVING COLD BLOB INCREMENT '${WESTLON}'-'${EASTLON}','${SOUTHLAT}'-'${NORTHLAT}
endif

#Revert T in center_ctrl back to K
#  Bin's recipe 4119 (/home/bzhao/V3_Recipes)
#${BASEDIR}/Linux/bin/ncap  -O -s "temp=(temp+273.15)" $center_ctrl $center_ctrl > ncap_back2K.out
#${BASEDIR}/Linux/bin/ncap2 -O -s "temp=(temp+273.15)" $center_ctrl $center_ctrl > ncap_back2K.out
#/discover/swdev/mathomp4/Baselibs/ESMA-Baselibs-4.0.11-SLES12/x86_64-unknown-linux-gnu/ifort_18.0.5.274-intelmpi_19.1.0.166-nco_2020Apr09_snapshot/Linux/bin/ncap2 -O -s "temp=(temp+273.15)" $center_ctrl $center_ctrl > ncap_back2K.out
/discover/swdev/gmao_SIteam/Baselibs/latest-mpiuni-SLES12/Linux/bin/ncap2 -O -s "temp=(temp+273.15)" $center_ctrl $center_ctrl > ncap_back2K.out
cp -f temp_increment.nc $SCRDIR/INPUT/
cp -f salt_increment.nc $SCRDIR/INPUT/
cp -f  eta_increment.nc $SCRDIR/INPUT/
mv -f temp_increment.nc ${EXPDIR}/ocean_das/$ANADIR/
mv -f salt_increment.nc ${EXPDIR}/ocean_das/$ANADIR/
mv -f  eta_increment.nc ${EXPDIR}/ocean_das/$ANADIR/

###############################################################################
#   Clean up
###############################################################################
mv -f *.png $ANADIR
mv -f ana $ANADIR
mv -f bkg $ANADIR
mv -f incr $ANADIR
mv -f mean_ana_restart $ANADIR  
mv -f recenter $ANADIR
mv -f *.out $ANADIR
mv -f NOUT* $ANADIR
mv -f gmao-obs*.nc $ANADIR
mv -f moments*.nc $ANADIR
mv -f ${yyyy}${mm}${dd}.dat $ANADIR 
mv -f *.dat $ANADIR
mv -f *.dat.nc $ANADIR 
mv -f *.grd $ANADIR 
cp -f *.nml $ANADIR
rm -rf states

