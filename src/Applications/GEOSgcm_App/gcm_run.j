#!/bin/csh -f

#######################################################################
#                     Batch Parameters for Run Job
#######################################################################

#@BATCH_TIME@RUN_T
#@RUN_P
#@RUN_P2
#@BATCH_JOBNAME@RUN_N
#@RUN_Q
#@BATCH_GROUP
#@BATCH_JOINOUTERR
#@BATCH_NAME -o gcm_run.o@RSTDATE

#######################################################################
#                         System Settings 
#######################################################################

umask 022

limit stacksize unlimited

#######################################################################
#           Architecture Specific Environment Variables
#######################################################################

setenv ARCH `uname`

setenv SITE             @SITE
setenv GEOSDIR          @GEOSDIR 
setenv GEOSBIN          @GEOSBIN 
setenv GEOSUTIL         @GEOSSRC/GMAO_Shared/GEOS_Util
setenv RUN_CMD         "@RUN_CMD"
setenv GCMVER           @GCMVER

source $GEOSBIN/g5_modules

if( $SITE == NAS ) then
    module swap mpi-hpe/mpt.2.23 mpi-hpe/mpt.2.25
endif

setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib

#######################################################################
#             Experiment Specific Environment Variables
#######################################################################


setenv  EXPID   @EXPID
setenv  EXPDIR  @EXPDIR
setenv  HOMDIR  @HOMDIR

setenv  RSTDATE @RSTDATE
setenv  GCMEMIP @GCMEMIP

#######################################################################
#                 Create Experiment Sub-Directories
#######################################################################

if (! -e $EXPDIR/restarts   ) mkdir -p $EXPDIR/restarts
if (! -e $EXPDIR/holding    ) mkdir -p $EXPDIR/holding
if (! -e $EXPDIR/archive    ) mkdir -p $EXPDIR/archive
if (! -e $EXPDIR/post       ) mkdir -p $EXPDIR/post
if (! -e $EXPDIR/plot       ) mkdir -p $EXPDIR/plot
>>>withODAS<<<if (! -e $EXPDIR/hindcast_restarts ) mkdir -p $EXPDIR/hindcast_restarts
>>>withODAS<<<if (! -e $EXPDIR/agcm_import       ) mkdir -p $EXPDIR/agcm_import

if( $GCMEMIP == TRUE ) then
    if (! -e $EXPDIR/restarts/$RSTDATE ) mkdir -p $EXPDIR/restarts/$RSTDATE
    setenv  SCRDIR  $EXPDIR/scratch.$RSTDATE
else
    setenv  SCRDIR  $EXPDIR/scratch
#    if( $SITE == NCCS ) then
#        if ( -l $SCRDIR ) unlink $SCRDIR
#        mkdir -p $TSE_TMPDIR/scratch
#        ln -s $TSE_TMPDIR/scratch $SCRDIR
#    endif 
endif

if (! -e $SCRDIR ) mkdir -p $SCRDIR

>>>withODAS<<<#######################################################################
>>>withODAS<<<#   ERIC ADD THIS SO SSS RESTORE FILE HAS A PLACE TO GO
>>>withODAS<<<if (! -e $SCRDIR/INPUT ) mkdir $SCRDIR/INPUT

#######################################################################
#                   Set Experiment Run Parameters
#######################################################################

set       NX  = `grep      "^ *NX:" $HOMDIR/AGCM.rc | cut -d':' -f2`
set       NY  = `grep      "^ *NY:" $HOMDIR/AGCM.rc | cut -d':' -f2`
set  AGCM_IM  = `grep      AGCM_IM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  AGCM_JM  = `grep      AGCM_JM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  AGCM_LM  = `grep      AGCM_LM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  OGCM_IM  = `grep      OGCM_IM: $HOMDIR/AGCM.rc | cut -d':' -f2`
set  OGCM_JM  = `grep      OGCM_JM: $HOMDIR/AGCM.rc | cut -d':' -f2`

>>>COUPLED<<<set  OGCM_LM  = `grep OGCM_LM: $HOMDIR/AGCM.rc | cut -d':' -f2`
>>>COUPLED<<<set       NX = `grep  OGCM_NX: $HOMDIR/AGCM.rc | cut -d':' -f2`
>>>COUPLED<<<set       NY = `grep  OGCM_NY: $HOMDIR/AGCM.rc | cut -d':' -f2`
>>>COUPLED<<<set  HIST_IM = @HIST_IM
>>>COUPLED<<<set  HIST_JM = @HIST_JM

# Check for Over-Specification of CPU Resources
# ---------------------------------------------
  if ($?PBS_NODEFILE) then
     set  NCPUS = `cat $PBS_NODEFILE | wc -l`
     @    NPES  = $NX * $NY
        if( $NPES > $NCPUS ) then
             echo "CPU Resources are Over-Specified"
             echo "--------------------------------"
             echo "Allotted NCPUs: $NCPUS"
             echo "Specified  NX : $NX"
             echo "Specified  NY : $NY"
             exit
        endif
     endif
  endif

#######################################################################
#                       GCMEMIP Setup
#######################################################################

if( $GCMEMIP == TRUE & ! -e $EXPDIR/restarts/$RSTDATE/cap_restart ) then

cd $EXPDIR/restarts/$RSTDATE

set      FILE = strip
/bin/rm $FILE
cat << EOF > $FILE
#!/bin/ksh
/bin/mv \$1 \$1.tmp
touch   \$1
while read line
do
echo \$line >> \$1
done < \$1.tmp
exit
EOF
chmod +x $FILE

@CPEXEC $HOMDIR/CAP.rc .
./strip         CAP.rc

set year  = `echo $RSTDATE | cut -d_ -f1 | cut -b1-4`
set month = `echo $RSTDATE | cut -d_ -f1 | cut -b5-6`

# Copy MERRA-2 Restarts
# ---------------------
@CPEXEC /discover/nobackup/projects/gmao/g6dev/ltakacs/MERRA2/restarts/AMIP/M${month}/restarts.${year}${month}.tar .
@TAREXEC xf  restarts.${year}${month}.tar
/bin/rm restarts.${year}${month}.tar
/bin/rm MERRA2*bin


# Regrid MERRA-2 Restarts
# -----------------------
set RSTID = `/bin/ls *catch*bin | cut -d. -f1`
$GEOSBIN/regrid.pl -np -ymd ${year}${month}10 -hr 21 -grout C${AGCM_IM} -levsout ${AGCM_LM} -outdir . -d . -expid $RSTID -tagin Ganymed-4_0 -oceanin e -i -nobkg -lbl -nolcv -tagout Icarus -rs 3 -oceanout @OCEANOUT
/bin/rm $RSTID.*.bin

     set IMC = $AGCM_IM
if(     $IMC < 10 ) then
     set IMC = 000$IMC
else if($IMC < 100) then
     set IMC = 00$IMC
else if($IMC < 1000) then
     set IMC = 0$IMC
endif

$GEOSBIN/stripname C${AGCM_IM}@OCEANOUT_${RSTID}.
$GEOSBIN/stripname .${year}${month}10_21z.bin.@BCSTAG.@ATMOStag_@OCEANtag
/bin/mv gocart_internal_rst gocart_internal_rst.merra2
$GEOSBIN/gogo.x -s $RSTID.Chem_Registry.rc.${year}${month}10_21z -t $EXPDIR/RC/Chem_Registry.rc -i gocart_internal_rst.merra2 -o gocart_internal_rst -r C${AGCM_IM} -l ${AGCM_LM}


# Create CAP.rc and cap_restart
# -----------------------------
set   nymd = ${year}${month}10
set   nhms = 210000
echo $nymd $nhms > cap_restart

set curmonth = $month
      @ count = 0
while( $count < 4 )
       set date  = `$GEOSBIN/tick $nymd $nhms 86400`
       set nymd  =  $date[1]
       set nhms  =  $date[2]
       set year  = `echo $nymd | cut -c1-4`
       set month = `echo $nymd | cut -c5-6`
       if( $curmonth != $month ) then
        set curmonth  = $month
             @ count  = $count + 1
       endif
end
set oldstring =  `cat CAP.rc | grep END_DATE:`
set newstring =  "END_DATE: ${year}${month}01 210000"
/bin/mv CAP.rc CAP.tmp
cat CAP.tmp | sed -e "s?$oldstring?$newstring?g" > CAP.rc
/bin/rm CAP.tmp

endif

#Link files needed for OBIO
#--------------------------
@COMMENT_OBIO/bin/ln -sf /discover/nobackup/mmehari/DATA/MISCEL/lidata/*  $EXPDIR/RC

#######################################################################
#   Move to Scratch Directory and Copy RC Files from Home Directory
#######################################################################

cd $SCRDIR
/bin/rm -rf *
                             /bin/ln -sf $EXPDIR/RC/* .
                             @CPEXEC     $EXPDIR/cap_restart .
                             @CPEXEC -f  $HOMDIR/*.rc .
                             @CPEXEC -f  $HOMDIR/*.rc.tmpl .
                             @CPEXEC -f  $HOMDIR/*.nml .

                             cat fvcore_layout.rc >> input.nml

if( $GCMEMIP == TRUE ) then
    @CPEXEC -f  $EXPDIR/restarts/$RSTDATE/cap_restart .
    @CPEXEC -f  $EXPDIR/restarts/$RSTDATE/CAP.rc .
endif

set END_DATE  = `grep     END_DATE:  CAP.rc | cut -d':' -f2`
set NUM_SGMT  = `grep     NUM_SGMT:  CAP.rc | cut -d':' -f2`
set FSEGMENT  = `grep FCST_SEGMENT:  CAP.rc | cut -d':' -f2`
set USE_SHMEM = `grep    USE_SHMEM:  CAP.rc | cut -d':' -f2`

#######################################################################
#         Create Strip Utility to Remove Multiple Blank Spaces
#######################################################################

set      FILE = strip
/bin/rm $FILE
cat << EOF > $FILE
#!/bin/ksh
/bin/mv \$1 \$1.tmp
touch   \$1
while read line
do
echo \$line >> \$1
done < \$1.tmp
exit
EOF
chmod +x $FILE

#######################################################################
#              Create HISTORY Collection Directories
#######################################################################

set collections = ''
foreach line ("`cat HISTORY.rc`")
   set firstword  = `echo $line | awk '{print $1}'`
   set firstchar  = `echo $firstword | cut -c1`
   set secondword = `echo $line | awk '{print $2}'`

   if ( $firstword == "::" ) goto done

   if ( $firstchar != "#" ) then
      set collection  = `echo $firstword | sed -e "s/'//g"`
      set collections = `echo $collections $collection`
      if ( $secondword == :: ) goto done
   endif

   if ( $firstword == COLLECTIONS: ) then
      set collections = `echo $secondword | sed -e "s/'//g"`
   endif
end

done:
   foreach collection ( $collections )
      if (! -e $EXPDIR/$collection )         mkdir $EXPDIR/$collection
      if (! -e $EXPDIR/holding/$collection ) mkdir $EXPDIR/holding/$collection
   end

#######################################################################
#                        Link Boundary Datasets
#######################################################################

setenv BCSDIR    @BCSDIR
>>>DATAOCEAN<<<setenv SSTDIR    @SSTDIR
setenv CHMDIR    @CHMDIR
>>>DATAOCEAN<<<setenv BCRSLV    @ATMOStag_@OCEANtag
>>>COUPLED<<<setenv BCRSLV    @ATMOStag_DE0360xPE0180
setenv DATELINE  DC
setenv EMISSIONS @EMISSIONS

>>>COUPLED<<<setenv GRIDDIR  @COUPLEDIR/a${AGCM_IM}x${AGCM_JM}_o${OGCM_IM}x${OGCM_JM}
>>>COUPLED<<<if ($SITE == NAS) then
>>>COUPLED<<<setenv GRIDDIR2  @COUPLEDIR/SST/MERRA2/${OGCM_IM}x${OGCM_JM}
>>>COUPLED<<<else if( $SITE == NCCS ) then
>>>COUPLED<<<setenv GRIDDIR2 @COUPLE2DIR/DE1440xPE0720_TM1440xTM1080
>>>COUPLED<<<setenv BCTAG `basename $GRIDDIR`
>>>DATAOCEAN<<<setenv BCTAG `basename $BCSDIR`

set             FILE = linkbcs
/bin/rm -f     $FILE
cat << _EOF_ > $FILE
#!/bin/csh -f

>>>COUPLED<<</bin/mkdir -p RESTART
/bin/mkdir -p            ExtData
/bin/ln    -sf $CHMDIR/* ExtData

>>>COUPLED<<</bin/ln -sf $GRIDDIR/SEAWIFS_KPAR_mon_clim.${OGCM_IM}x${OGCM_JM} SEAWIFS_KPAR_mon_clim.data
>>>COUPLED<<</bin/ln -sf $GRIDDIR/@ATMOStag_@OCEANtag-Pfafstetter.til   tile.data
>>>COUPLED<<</bin/ln -sf $GRIDDIR/@ATMOStag_@OCEANtag-Pfafstetter.TRN   runoff.bin
>>>COUPLED<<</bin/ln -sf $GRIDDIR/tripolar_${OGCM_IM}x${OGCM_JM}.ascii .
>>>COUPLED<<</bin/ln -sf $GRIDDIR/vgrid${OGCM_LM}.ascii ./vgrid.ascii
>>>COUPLED<<</bin/ln -s @COUPLEDIR/a@HIST_IMx@HIST_JM_o${OGCM_IM}x${OGCM_JM}/DC0@HIST_IMxPC0@HIST_JM_@OCEANtag-Pfafstetter.til tile_hist.data
>>>COUPLED<<</bin/ln -s @COUPLEDIR/a1440x721_o1440x1080/DC1440xPC0721_TM1440xTM1080-Pfafstetter.til tile_hist_1440x721.data

# Precip correction
#/bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput/merra_land/precip_CPCUexcludeAfrica-CMAP_corrected_MERRA/GEOSdas-2_1_4 ExtData/PCP
>>>COUPLED_DUAL<<<rm -f ExtData/PCP

>>>COUPLED_DUAL<<<# 1981-2014
>>>COUPLED_DUAL<<<#/bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput/merra_land/precip_CPCUexcludeAfrica-CMAP_corrected_MERRA/GEOSdas-2_1_4 ExtData/PCP

>>>COUPLED_DUAL<<<#2015
>>>COUPLED_DUAL<<<#/bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput/merra_land/precip_CPCUexcludeAfrica-CMAP_corrected_MERRA/GEOSdas-2_1_4 ExtData/PCP

>>>COUPLED_DUAL<<<# 2017
>>>COUPLED_DUAL<<</bin/ln -s /discover/nobackup/projects/gmao/share/gmao_ops/fvInput/merra_land/precip_CPCUexcludeAfrica-CMAP_corrected_MERRA/GEOSadas-5_12_4 ExtData/PCP

>>>COUPLED_DUAL<<<# Fortuna from Qing
>>>COUPLED_DUAL<<<#/discover/nobackup/qliu/merra_land/precip_corr_SSD_near-present/f516_fp/diag/Y2017/
>>>COUPLED_DUAL<<<# PRECIP_FILE: ExtData/PCP/f516_fp/diag/Y%y4/M%m2/f516_fp.tavg1_2d_lfo_Nx_corr.%y4%m2%d2_%h230z.nc
>>>COUPLED_DUAL<<<# /bin/ln -s  /discover/nobackup/qliu/merra_land/precip_corr_SSD_near-present ExtData/PCP

>>>COUPLED_DUAL<<<# Precip 2017 realtime
>>>COUPLED_DUAL<<<#/bin/ln -s  /discover/nobackup/dao_ops/PrecipCorr/CMAPcorr/ ExtData/PCP


>>>DATAOCEAN<<</bin/ln -sf $BCSDIR/$BCRSLV/${BCRSLV}-Pfafstetter.til  tile.data
>>>DATAOCEAN<<<if(     -e  $BCSDIR/$BCRSLV/${BCRSLV}-Pfafstetter.TIL) then
>>>DATAOCEAN<<</bin/ln -sf $BCSDIR/$BCRSLV/${BCRSLV}-Pfafstetter.TIL  tile.bin
>>>DATAOCEAN<<<endif

# CMIP-5 Ozone Data (228-Years)
# -----------------------------
/bin/ln -sf $BCSDIR/Shared/pchem.species.CMIP-5.1870-2097.z_91x72.nc4 species.data

# MERRA-2 Ozone Data (39-Years)
# -----------------------------
#/bin/ln -sf $BCSDIR/Shared/pchem.species.CMIP-5.MERRA2OX.197902-201706.z_91x72.nc4 species.data

/bin/ln -sf /discover/nobackup/projects/gmao/SIteam/CEDS-Tile-Files/bin/*bin .

/bin/ln -sf $BCSDIR/Shared/*bin .
/bin/ln -sf $BCSDIR/Shared/*c2l*.nc4 .

>>>DATAOCEAN<<</bin/ln -sf $BCSDIR/$BCRSLV/visdf_@RES_DATELINE.dat visdf.dat
>>>DATAOCEAN<<</bin/ln -sf $BCSDIR/$BCRSLV/nirdf_@RES_DATELINE.dat nirdf.dat
>>>DATAOCEAN<<</bin/ln -sf $BCSDIR/$BCRSLV/vegdyn_@RES_DATELINE.dat vegdyn.data
>>>DATAOCEAN<<</bin/ln -sf $BCSDIR/$BCRSLV/lai_clim_@RES_DATELINE.data lai.data
>>>DATAOCEAN<<</bin/ln -sf $BCSDIR/$BCRSLV/green_clim_@RES_DATELINE.data green.data
/bin/ln -sf $BCSDIR/$BCRSLV/ndvi_clim_@RES_DATELINE.data ndvi.data

>>>COUPLED<<<if( $OGCM_IM == 1440 ) then
>>>COUPLED<<</bin/ln -sf $GRIDDIR/ndvi.data ndvi.data
>>>COUPLED<<<endif 

>>>COUPLED<<</bin/ln -sf $GRIDDIR/visdf.dat visdf.dat
>>>COUPLED<<</bin/ln -sf $GRIDDIR/nirdf.dat nirdf.dat
>>>COUPLED<<</bin/ln -sf $GRIDDIR/vegdyn.data vegdyn.data
>>>COUPLED<<</bin/ln -sf $GRIDDIR/lai.dat lai.data
>>>COUPLED<<</bin/ln -sf $GRIDDIR/green.dat green.data
/bin/ln -sf $BCSDIR/$BCRSLV/topo_DYN_ave_@RES_DATELINE.data topo_dynave.data
/bin/ln -sf $BCSDIR/$BCRSLV/topo_GWD_var_@RES_DATELINE.data topo_gwdvar.data
/bin/ln -sf $BCSDIR/$BCRSLV/topo_TRB_var_@RES_DATELINE.data topo_trbvar.data

>>>FVCUBED<<<if(     -e  $BCSDIR/$BCRSLV/Gnomonic_$BCRSLV.dat ) then
>>>FVCUBED<<</bin/ln -sf $BCSDIR/$BCRSLV/Gnomonic_$BCRSLV.dat .
>>>FVCUBED<<<endif

>>>COUPLED<<<@CPEXEC $HOMDIR/*_table .
>>>COUPLED<<</bin/ln -sf $GRIDDIR/cice/kmt_cice.bin .
>>>COUPLED<<</bin/ln -sf $GRIDDIR/cice/grid_cice.bin .

_EOF_

>>>DATAOCEAN<<<echo "/bin/ln -sf $SSTDIR"'/@SSTFILE   sst.data' >> $FILE
>>>DATAOCEAN<<<echo "/bin/ln -sf $SSTDIR"'/@ICEFILE fraci.data' >> $FILE
>>>DATAOCEAN<<<echo "/bin/ln -sf $SSTDIR"'/@KPARFILE SEAWIFS_KPAR_mon_clim.data' >> $FILE

chmod +x linkbcs
@CPEXEC  linkbcs $EXPDIR

#######################################################################
#          Get C2L History weights/index file for Cubed-Sphere
#######################################################################

set C_NPX = `echo $AGCM_IM | awk '{printf "%5.5i", $1}'`
set C_NPY = `echo $AGCM_JM | awk '{printf "%5.5i", $1}'`
set H_NPX = `echo @HIST_IM | awk '{printf "%5.5i", $1}'`
set H_NPY = `echo @HIST_JM | awk '{printf "%5.5i", $1}'`

set c2l_file = "${C_NPX}x${C_NPY}_c2l_${H_NPX}x${H_NPY}.bin"

if (-e $BCSDIR/$BCRSLV/${c2l_file}) /bin/ln -s $BCSDIR/$BCRSLV/${c2l_file} .

#######################################################################
#                    Get Executable and RESTARTS 
#######################################################################

@CPEXEC $EXPDIR/GEOSgcm.x .

set rst_files      = `cat AGCM.rc | grep "RESTART_FILE"    | grep -v VEGDYN | grep -v "#" | cut -d ":" -f1 | cut -d "_" -f1-2`
set rst_file_names = `cat AGCM.rc | grep "RESTART_FILE"    | grep -v VEGDYN | grep -v "#" | cut -d ":" -f2`

set chk_files      = `cat AGCM.rc | grep "CHECKPOINT_FILE" | grep -v "#" | cut -d ":" -f1 | cut -d "_" -f1-2`
set chk_file_names = `cat AGCM.rc | grep "CHECKPOINT_FILE" | grep -v "#" | cut -d ":" -f2`

set monthly_chk_names = `cat $EXPDIR/HISTORY.rc | grep -v '^[\t ]*#' | sed -n 's/\([^\t ]\+\).monthly:[\t ]*1.*/\1/p' | sed 's/$/_rst/' `

# Remove possible bootstrap parameters (+/-)
# ------------------------------------------
set dummy = `echo $rst_file_names`
set rst_file_names = ''
foreach rst ( $dummy )
  set length  = `echo $rst | awk '{print length($0)}'`
  set    bit  = `echo $rst | cut -c1`
  if(  "$bit" == "+" | \
       "$bit" == "-" ) set rst = `echo $rst | cut -c2-$length`
  set rst_file_names = `echo $rst_file_names $rst`
end

# Copy Restarts to Scratch Directory
# ----------------------------------
if( $GCMEMIP == TRUE ) then
    foreach rst ( $rst_file_names )
      if(-e $EXPDIR/restarts/$RSTDATE/$rst ) @CPEXEC $EXPDIR/restarts/$RSTDATE/$rst . &
    end
else
    foreach rst ( $rst_file_names $monthly_chk_names )
      if(-e $EXPDIR/$rst ) @CPEXEC $EXPDIR/$rst . &
    end
>>>withODAS<<<      /bin/cp -f $EXPDIR/*_glo_*_rst . &
endif
wait

>>>COUPLED<<<@CPEXEC -R $GRIDDIR/INPUT .
>>>COUPLED<<<@CPEXEC $EXPDIR/RESTART/* INPUT
>>>withODAS<<<#####################################################################
>>>withODAS<<<# GET THE WOA18 DATA TO RELAX TO (DONT FORGET TO FIX INPUT.NML)
>>>withODAS<<<#cp /discover/nobackup/bzhao/ObservationData/CM2.5/global_0.25_degree_NYF/INPUT/salt_sfc_restore_new_woa18.nc $SCRDIR/INPUT/salt_sfc_restore.nc
>>>withODAS<<<# USE MY FLOODED WOA18 PRODUCT INSTEAD
>>>withODAS<<<#/bin/cp -f /gpfsm/dnb42/projects/p17/ehackert/geos5/exp/WOA18/eh015_ocean_sponge_output/test_salt_sfc_restore_new_woa18.nc INPUT/salt_sfc_restore.nc
>>>withODAS<<<#/bin/cp -f /gpfsm/dnb78s2/projects/p26/ehackert/GEOSodas-V3/RC/OCEAN_DAS_RC_BASE_3_ALL_MONTHS/test_salt_sfc_restore_new_woa18.nc INPUT/salt_sfc_restore.nc
>>>withODAS<<<####################################################################

# Copy and Tar Initial Restarts to Restarts Directory
# ---------------------------------------------------
set edate = e`cat cap_restart | cut -c1-8`_`cat cap_restart | cut -c10-11`z
set numrs = `/bin/ls -1 ${EXPDIR}/restarts/*${edate}* | wc -l`
if($numrs == 0) then
   foreach rst ( $rst_file_names )
      if( -e $rst & ! -e ${EXPDIR}/restarts/$EXPID.${rst}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV} ) then
            @CPEXEC $rst ${EXPDIR}/restarts/$EXPID.${rst}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV} &
      endif
   end
   wait
>>>COUPLED<<<   @CPEXEC -r $EXPDIR/RESTART ${EXPDIR}/restarts/RESTART.${edate}
   cd $EXPDIR/restarts
      >>>DATAOCEAN<<<@TAREXEC cf  restarts.${edate}.tar $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}
      >>>COUPLED<<<@TAREXEC cvf  restarts.${edate}.tar $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV} RESTART.${edate}
     /bin/rm -rf `/bin/ls -d -1     $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}`
     >>>COUPLED<<</bin/rm -rf RESTART.${edate}
   cd $SCRDIR
endif

# If any restart is binary, set NUM_READERS to 1 so that
# +-style bootstrapping of missing files can occur in 
# MAPL. pbinary cannot do this, but pnc4 can.
# ------------------------------------------------------
set found_binary = 0

foreach rst ( $rst_file_names )
   if (-e $rst) then
      set rst_type = `/usr/bin/file -Lb --mime-type $rst`
      if ( $rst_type =~ "application/octet-stream" ) then
         set found_binary = 1
      endif
   endif
end

if ($found_binary == 1) then
   /bin/mv AGCM.rc AGCM.tmp
   cat AGCM.tmp | sed -e "/^NUM_READERS/ s/\([0-9]\+\)/1/g" > AGCM.rc
   /bin/rm AGCM.tmp
endif


##################################################################
######
######         Perform multiple iterations of Model Run
######
##################################################################

@ counter    = 1
while ( $counter <= ${NUM_SGMT} )

/bin/rm -f  EGRESS

if( $GCMEMIP == TRUE ) then
    @CPEXEC -f  $EXPDIR/restarts/$RSTDATE/CAP.rc .
else
    @CPEXEC -f $HOMDIR/CAP.rc .

    ######################################################################
#   Use 6-day JOB_SGMT (02/25-03/02) for leap year
######################################################################
set nymdc2 = `cat cap_restart | cut -c1-11`
set nyears  = `echo $nymdc2 | cut -c1-4`
set nmonths = `echo $nymdc2 | cut -c5-6`
set ndays   = `echo $nymdc2 | cut -c7-8`

if( $nmonths == 02 & \
    $ndays   == 25 ) then
    @ yr = 1 * $nyears
    @ yr1 =($yr - 1) / 4
    @ yr2 = $yr / 4

if( $yr2 > $yr1 ) then

    echo "Year: " $yr
    /bin/cp -f  $HOMDIR/CAP.rc_6dy CAP.rc

endif
endif
######################################################################

endif

./strip CAP.rc

# Set Time Variables for Current_(c), Ending_(e), and Segment_(s) dates 
# ---------------------------------------------------------------------
set nymdc = `cat cap_restart | cut -c1-8`
set nhmsc = `cat cap_restart | cut -c10-15`
set nymde = `cat CAP.rc | grep END_DATE:     | cut -d: -f2 | cut -c2-9`
set nhmse = `cat CAP.rc | grep END_DATE:     | cut -d: -f2 | cut -c11-16`
set nymds = `cat CAP.rc | grep JOB_SGMT:     | cut -d: -f2 | cut -c2-9`
set nhmss = `cat CAP.rc | grep JOB_SGMT:     | cut -d: -f2 | cut -c11-16`

>>>COUPLED_DUAL<<<# Set currmonth for discharge adjust
>>>COUPLED_DUAL<<<# ---------------------------------------------------------------------
set currmonth = `cat cap_restart | cut -c5-6`
>>>COUPLED_DUAL<<<#ln -sf /discover/nobackup/projects/gmao/merra2-ocean/fordischarge/evallM$currmonth.nc4  evall.nc4
>>>COUPLED_DUAL<<< ln -sf /discover/nobackup/projects/gmao/merra2-ocean/fordischarge/evallANN.nc4          evall.nc4

>>>withODAS<<<# Compute Start Time for plotting 
>>>withODAS<<<# ---------------------------------------------------------------------
>>>withODAS<<<set nymdc2 = `cat cap_restart | cut -c1-11`
>>>withODAS<<<set nyears   = `echo $nymdc2 | cut -c1-4`
>>>withODAS<<<set nmonths  = `echo $nymdc2 | cut -c5-6`
>>>withODAS<<<set ndays    = `echo $nymdc2 | cut -c7-8`
>>>withODAS<<<set nhours   = `echo $nymdc2 | cut -c10-11`

# Compute Time Variables at the Finish_(f) of current segment
# -----------------------------------------------------------
set nyear   = `echo $nymds | cut -c1-4`
set nmonth  = `echo $nymds | cut -c5-6`
set nday    = `echo $nymds | cut -c7-8`
set nhour   = `echo $nhmss | cut -c1-2`
set nminute = `echo $nhmss | cut -c3-4`
set nsec    = `echo $nhmss | cut -c5-6`
       @ dt = $nsec + 60 * $nminute + 3600 * $nhour + 86400 * $nday

set nymdf = $nymdc
set nhmsf = $nhmsc
set date  = `$GEOSBIN/tick $nymdf $nhmsf $dt`
set nymdf =  $date[1]
set nhmsf =  $date[2]
set year  = `echo $nymdf | cut -c1-4`
set month = `echo $nymdf | cut -c5-6`
set day   = `echo $nymdf | cut -c7-8`
set hour  = `echo $nhmsf | cut -c1-2`

     @  month = $month + $nmonth
while( $month > 12 )
     @  month = $month - 12
     @  year  = $year  + 1
end
     @  year  = $year  + $nyear
     @ nymdf  = $year * 10000 + $month * 100 + $day

if( $nymdf >  $nymde )    set nymdf = $nymde
if( $nymdf == $nymde )    then
    if( $nhmsf > $nhmse ) set nhmsf = $nhmse
endif

set yearc = `echo $nymdc | cut -c1-4`
set yearf = `echo $nymdf | cut -c1-4`

# For Non-Reynolds SST, Modify local CAP.rc Ending date if Finish time exceeds Current year boundary
# --------------------------------------------------------------------------------------------------
if( @OCEANtag != DE0360xPE0180 ) then
    if( $yearf > $yearc ) then
       @ yearf = $yearc + 1
       @ nymdf = $yearf * 10000 + 0101
        set oldstring = `cat CAP.rc | grep END_DATE:`
        set newstring = "END_DATE: $nymdf $nhmsf"
        /bin/mv CAP.rc CAP.tmp
        cat CAP.tmp | sed -e "s?$oldstring?$newstring?g" > CAP.rc
    endif
endif

# Select proper MERRA-2 GOCART Emission RC Files
# (NOTE: MERRA2-DD has same transition date)
# ----------------------------------------------
if( ${EMISSIONS} == MERRA2 | \
    ${EMISSIONS} == MERRA2-DD ) then
    set MERRA2_Transition_Date = 20000401

    if( $nymdc < ${MERRA2_Transition_Date} ) then
         set MERRA2_EMISSIONS_DIRECTORY = $GEOSDIR/$ARCH/etc/$EMISSIONS/19600101-20000331
         if( $nymdf > ${MERRA2_Transition_Date} ) then
          set nymdf = ${MERRA2_Transition_Date}
          set oldstring = `cat CAP.rc | grep END_DATE:`
          set newstring = "END_DATE: $nymdf $nhmsf"
          /bin/mv CAP.rc CAP.tmp
                     cat CAP.tmp | sed -e "s?$oldstring?$newstring?g" > CAP.rc
         endif
    else
         set MERRA2_EMISSIONS_DIRECTORY = $GEOSDIR/$ARCH/etc/$EMISSIONS/20000401-present
    endif

    if( $AGCM_LM == 72 ) then
        @CPEXEC --remove-destination ${MERRA2_EMISSIONS_DIRECTORY}/*.rc .
    else
        set files =      `/bin/ls -1 ${MERRA2_EMISSIONS_DIRECTORY}/*.rc`
        foreach file ($files)
          /bin/rm -f   `basename $file`
          /bin/rm -f    dummy
          @CPEXEC $file dummy
              cat       dummy | sed -e "s|/L72/|/L${AGCM_LM}/|g" | sed -e "s|z72|z${AGCM_LM}|g" > `basename $file`
        end
    endif

endif

#-------------------------------------
#--> Begin setting GAAS file paths
#-------------------------------------
set startYear = `cat cap_restart | cut -c1-4`

if( $startYear >= 1998 && $startYear <= 2007 ) then
    set sYear  = 1998
#   set MERRA2type = d5294_geosit_jan98
    set MERRA2type = d5294_geositocn_may01
    set data_Transition_Date = 20080101
else if( $startYear >= 2008 && $startYear <= 2017 ) then
    set sYear  = 2008
    set MERRA2type = d5294_geosit_jan08
    set data_Transition_Date = 20180101
else if( $startYear >= 2018                       ) then
    set sYear  = 2018
    set MERRA2type = d5294_geosit_jan18
    set data_Transition_Date = 20280101
endif

# Edit the GAAS_GridComp.rc file
#-------------------------------
set tFILE = tmpfile
rm -f $tFILE
cat GAAS_GridComp.rc.tmpl > $tFILE
set sFILE = sedfile
rm -f $sFILE

cat << EOF > $sFILE 
s/@MERRA2type/$MERRA2type/g
EOF

sed -f $sFILE $tFILE > GAAS_GridComp.rc
chmod 755  GAAS_GridComp.rc
rm -f $tFILE
rm -f $sFILE

#-----------------------------
##--> End setting GAAS file paths
#-----------------------------

if(-e ExtData.rc )    /bin/rm -f   ExtData.rc
set  extdata_files = `/bin/ls -1 *_ExtData.rc`
cat $extdata_files > ExtData.rc 

# Link Boundary Conditions for Appropriate Date
# ---------------------------------------------
setenv YEAR $yearc
./linkbcs

if (! -e tile.bin) then
$RUN_CMD 1 $GEOSBIN/binarytile.x tile.data tile.bin
>>>COUPLED<<<$RUN_CMD 1 $GEOSBIN/binarytile.x tile_hist.data tile_hist.bin
endif

# If running in dual ocean mode, link sst and fraci data here
>>>COUPLED_DUAL<<<set yy  = `cat cap_restart | cut -c1-4`
>>>COUPLED_DUAL<<<echo $yy
>>>COUPLED_DUAL<<<ln -sf $GRIDDIR2/dataoceanfile_MERRA2_SST.${OGCM_IM}x${OGCM_JM}.${yy}.data sst.data
>>>COUPLED_DUAL<<<ln -sf $GRIDDIR2/dataoceanfile_MERRA2_ICE.${OGCM_IM}x${OGCM_JM}.${yy}.data fraci.data

# Test Saltwater Restart for Number of tiles correctness
# ------------------------------------------------------

if ( -x $GEOSBIN/rs_numtiles.x ) then

   set N_SALT_TILES_EXPECTED = `grep '^ *0' tile.data | wc -l`
   set N_SALT_TILES_FOUND = `$RUN_CMD 1 $GEOSBIN/rs_numtiles.x openwater_internal_rst | grep Total | awk '{print $3}'`
         
   if ( $N_SALT_TILES_EXPECTED != $N_SALT_TILES_FOUND ) then
      echo "Error! Found $N_SALT_TILES_FOUND tiles in saltwater. Expect to find $N_SALT_TILES_EXPECTED tiles."
      echo "Your restarts are probably for a different ocean."
      exit 7
   endif    

endif

# Environment variables for MPI, etc
# ----------------------------------

@SETENVS

@GPUSTART

# Test if at NAS and if BATCH
# ---------------------------
set NAS_BATCH = FALSE
if ($SITE == NAS) then
   if ($PBS_ENVIRONMENT == PBS_BATCH) then
      set NAS_BATCH = TRUE
   endif
endif

# Run GEOSgcm.x
# -------------
if( $USE_SHMEM == 1 ) $GEOSBIN/RmShmKeys_sshmpi.csh
       @  NPES = $NX * $NY

>>>noODAS<<<if( $NAS_BATCH == TRUE ) then
>>>noODAS<<<   setenv MPI_IB_RAILS 2
>>>noODAS<<<   $RUN_CMD $NPES ./GEOSgcm.x >& $HOMDIR/gcm_run.$PBS_JOBID.$nymdc.out
>>>noODAS<<<else
>>>noODAS<<<   $RUN_CMD $NPES ./GEOSgcm.x
>>>noODAS<<<endif

>>>withODAS<<<# RUN ODAS HERE
>>>withODAS<<<#$EXPDIR/ocean_das/UMD_Etc/scripts/oda_run.j $NX $NY > $EXPDIR/ocean_das/oda_run.out
>>>withODAS<<<$EXPDIR/ocean_das/UMD_Etc/scripts/oda_run.j $NX $NY
>>>withODAS<<<
>>>withODAS<<<set ODARUNSTATUS = $status
>>>withODAS<<<if ($ODARUNSTATUS == 1) then
>>>withODAS<<<   echo "BAD ODA_RUN.j "
>>>withODAS<<<   exit(1)
>>>withODAS<<<endif
>>>withODAS<<<
>>>withODAS<<<echo "OUTOF ODA_RUN.J CAP_RESTART "
>>>withODAS<<<more cap_restart
>>>withODAS<<<pwd
>>>withODAS<<<echo "OUTOF ODA_RUN.J CAP_RESTART "

if( $USE_SHMEM == 1 ) $GEOSBIN/RmShmKeys_sshmpi.csh

@GPUEND

if( -e EGRESS ) then
   set rc = 0
else
   set rc = -1
>>>withODAS<<<   echo 'MODEL BOMBED IN gcm_run.j'
>>>withODAS<<<   exit
endif
echo GEOSgcm Run Status: $rc
 
#######################################################################
#   Rename Final Checkpoints => Restarts for Next Segment and Archive
#        Note: cap_restart contains the current NYMD and NHMS
#######################################################################

set edate  = e`cat cap_restart | cut -c1-8`_`cat cap_restart | cut -c10-11`z

>>>COUPLED<<<@CPEXEC -r RESTART ${EXPDIR}/restarts/RESTART.${edate}
>>>COUPLED<<<@CPEXEC RESTART/* INPUT

# Move Intermediate Checkpoints to RESTARTS directory
# ---------------------------------------------------
>>>withODAS<<</bin/mv -f  ${EXPID}_agcm_import*  ${EXPDIR}/agcm_import/

set   checkpoints  =    `/bin/ls -1 *_checkpoint.*`
if( $#checkpoints != 0 ) /bin/mv -f *_checkpoint.* ${EXPDIR}/restarts


# Rename Final Checkpoints for Archive
# ------------------------------------
    set checkpoints = `/bin/ls -1 *_checkpoint`
foreach checkpoint ($checkpoints)
        set   chk_type = `/usr/bin/file -Lb --mime-type $checkpoint`
            if ( $chk_type =~ "application/octet-stream" ) then
                  set ext  = bin
            else
                  set ext  = nc4
            endif
       /bin/mv            $checkpoint      $EXPID.${checkpoint}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.$ext
       $GEOSBIN/stripname _checkpoint _rst $EXPID.${checkpoint}.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.$ext
end


# Remove Initial RESTARTS
# -----------------------
set restarts = `/bin/ls -1 *_rst`
/bin/rm  $restarts


# Copy Renamed Final Checkpoints to RESTARTS directory
# ----------------------------------------------------
    set  restarts = `/bin/ls -1 $EXPID.*_rst.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.*`
foreach  restart ($restarts)
@CPEXEC $restart ${EXPDIR}/restarts
end

# Remove EXPID from RESTART name
# ------------------------------
    set  restarts = `/bin/ls -1 $EXPID.*_rst.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.*`
foreach  restart ($restarts)
$GEOSBIN/stripname $EXPID. '' $restart
end

# Remove DATE and VERSION Stamps from RESTART name
# ------------------------------------------------
    set  restarts = `/bin/ls -1 *_rst.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.*`
foreach  restart ($restarts)
$GEOSBIN/stripname .${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.\* '' $restart
end

# Move monthly collection checkpoints to restarts
#------------------------------------------------
set monthlies = `ls *chk`
if ( $#monthlies > 0 ) then
    echo 'check 3'
    foreach ff (*chk)
            mv $ff `basename $ff chk`rst
    end
endif
wait

# Copy Renamed monthly to RESTARTS directory
# ----------------------------------------------------
    set  restarts = `/bin/ls -1 *_tavg_1mo_glo_*_rst`
foreach  restart ($restarts)
echo restart
/bin/cp $restart ${EXPDIR}/restarts
end

# TAR ARCHIVED RESTARTS
# ---------------------
cd $EXPDIR/restarts
    if( $FSEGMENT == 00000000 ) then
	>>>DATAOCEAN<<<@TAREXEC cf  restarts.${edate}.tar $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.*
        >>>COUPLED<<<@TAREXEC cvf  restarts.${edate}.tar $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.* *_tavg_1mo_glo_*_rst RESTART.${edate}
        /bin/rm -rf `/bin/ls -d -1     $EXPID.*.${edate}.${GCMVER}.${BCTAG}_${BCRSLV}.*`
        /bin/rm -rf *_tavg_1mo_glo_*_rst
	>>>COUPLED<<</bin/rm -rf RESTART.${edate}
    endif
cd $SCRDIR


#######################################################################
#               Move HISTORY Files to Holding Directory
#######################################################################

# Check for files waiting in /holding
# -----------------------------------
set     waiting_files = `/bin/ls -1 $EXPDIR/holding/*/*nc4`
set num_waiting_files = $#waiting_files

# Move current files to /holding
# ------------------------------
foreach collection ( $collections )
   /bin/mv `/bin/ls -1 *.${collection}.*` $EXPDIR/holding/$collection
end

>>>COUPLED<<<# MOM-Specific Output Files
>>>COUPLED<<<# -------------------------
>>>noODAS<<< set dsets="ocean_month"
>>>withODAS<<< set dsets="ocean_daily"
>>>COUPLED<<< foreach dset ( $dsets )
>>>COUPLED<<< set num = `/bin/ls -1 $dset.nc | wc -l`
>>>COUPLED<<< if($num != 0) then
>>>COUPLED<<<    if(! -e $EXPDIR/MOM_Output) mkdir -p $EXPDIR/MOM_Output
>>>COUPLED<<<    /bin/mv $SCRDIR/$dset.nc $EXPDIR/MOM_Output/$dset.${edate}.nc
>>>COUPLED<<< endif
>>>COUPLED<<< end
>>>COUPLED<<<
#######################################################################
#                 Run Post-Processing and Forecasts
#######################################################################

$GEOSUTIL/post/gcmpost.script -source $EXPDIR -movefiles

>>>withODAS<<<#  new plotting of OMF/OMA stats from ERIC 3/19/21
>>>withODAS<<<$GEOSUTIL/plots/odas_plots.csh $EXPDIR $nyears $nmonths $ndays $nhours $year $month $day $hour $GEOSUTIL/plots
 
if( $FSEGMENT != 00000000 ) then
     set REPLAY_BEG_DATE = `grep BEG_REPDATE: $HOMDIR/CAP.rc | cut -d':' -f2`
     set REPLAY_END_DATE = `grep END_REPDATE: $HOMDIR/CAP.rc | cut -d':' -f2`
     set nday  = `echo $FSEGMENT | cut -c7-8`
         @ dt  = 10800 - 86400 * $nday
     set date  = `$GEOSBIN/tick $nymdc $nhmsc $dt`
     set nymdz =  $date[1]
     set nhmsz =  $date[2]

     if( $nymdz >= ${REPLAY_BEG_DATE} & \
         $nymdz <= ${REPLAY_END_DATE} ) then
         $EXPDIR/forecasts/gcm_forecast.setup $nymdz $nymdz $FSEGMENT TRUE
     endif
endif

#######################################################################
#                         Update Iteration Counter
#######################################################################

set enddate = `echo  $END_DATE | cut -c1-8`
set capdate = `cat cap_restart | cut -c1-8`

if ( $capdate < $enddate ) then
@ counter = $counter    + 1
else
@ counter = ${NUM_SGMT} + 1
endif

end

#######################################################################
#                              Re-Submit Job
#######################################################################

if( $GCMEMIP == TRUE ) then
     foreach rst ( `/bin/ls -1 *_rst` )
        /bin/rm -f $EXPDIR/restarts/$RSTDATE/$rst
     end
        /bin/rm -f $EXPDIR/restarts/$RSTDATE/cap_restart
     foreach rst ( `/bin/ls -1 *_rst` )
       @CPEXEC $rst $EXPDIR/restarts/$RSTDATE/$rst &
     end
     wait
     @CPEXEC cap_restart $EXPDIR/restarts/$RSTDATE/cap_restart
else
     foreach rst ( `/bin/ls -1 *_rst` )
        /bin/rm -f $EXPDIR/$rst
     end
        /bin/rm -f $EXPDIR/cap_restart
     foreach rst ( `/bin/ls -1 *_rst` )
       @CPEXEC $rst $EXPDIR/$rst &
     end
     wait
     @CPEXEC cap_restart $EXPDIR/cap_restart
endif

>>>COUPLED<<<@CPEXEC -rf RESTART $EXPDIR

if ( $rc == 0 ) then
      cd  $HOMDIR
      if( $GCMEMIP == TRUE ) then
          if( $capdate < $enddate ) qsub $HOMDIR/gcm_run.j$RSTDATE
      else
          if( $capdate < $enddate ) qsub $HOMDIR/gcm_run.j
      endif
endif
