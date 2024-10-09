#!/bin/csh  -f
setenv MPLBACKEND Agg    

if ( $#argv != 10 ) then
   echo 'need 10 args'
   echo ' EXPDIR nyears nmonths ndays nhours year month day hour'
else
set EXPDIR  = ${1}
set nyears  = ${2}
set nmonths = ${3}
set ndays   = ${4}
set nhours  = ${5}
set year    = ${6}
set month   = ${7}
set day     = ${8}
set hour    = ${9}
set GEOSUTIL = ${10}

set PLOTDIR = $GEOSUTIL/odas_plots

echo 'here' $EXPDIR  $GEOSUTIL $PLOTDIR

#  new plotting of OMF/OMA stats from ERIC 2/12/21
echo 'dates for plotting',$nyears,$nmonths,$ndays,$nhours,$year,$month,$day,$hour
pwd
echo '$PLOTDIR is',$PLOTDIR
echo '$EXPDIR is',$EXPDIR

#   global plots
$PLOTDIR/plot_odas_obsfile_binned1x1.with_instid.p3.with_nobsa.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/ocean_obs\*/ $nyears $nmonths $ndays $nhours $year $month $day $hour SSS 0 5 omf-oma ALL > $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/plot1_output
$PLOTDIR/plot_odas_obsfile_binned1x1.with_instid.p3.with_nobsa.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/ocean_obs\*/ $nyears $nmonths $ndays $nhours $year $month $day $hour SSS 0 5 obs ALL > $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/plot17_output
$PLOTDIR/plot_odas_obsfile_binned1x1.with_instid.p3.with_nobsa.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/ocean_obs\*/ $nyears $nmonths $ndays $nhours $year $month $day $hour SSH 0 5 omf-oma ALL > $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/plot2_output
$PLOTDIR/plot_odas_obsfile_binned1x1.with_instid.p3.with_nobsa.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/ocean_obs\*/ $nyears $nmonths $ndays $nhours $year $month $day $hour SSH 0 5 obs ALL > $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/plot18_output
$PLOTDIR/plot_odas_obsfile_binned1x1.with_instid.p3.with_nobsa.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/ocean_obs\*/ $nyears $nmonths $ndays $nhours $year $month $day $hour TZ 0 30 omf-oma ALL > $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/plot3_output
$PLOTDIR/plot_odas_obsfile_binned1x1.with_instid.p3.with_nobsa.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/ocean_obs\*/ $nyears $nmonths $ndays $nhours $year $month $day $hour TZ 0 30 omf-oma ALL
$PLOTDIR/plot_odas_obsfile_binned1x1.with_instid.p3.with_nobsa.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/ocean_obs\*/ $nyears $nmonths $ndays $nhours $year $month $day $hour TZ 0 30 obs ALL > $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/plot4_output
$PLOTDIR/plot_odas_obsfile_binned1x1.with_instid.p3.with_nobsa.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/ocean_obs\*/ $nyears $nmonths $ndays $nhours $year $month $day $hour SZ 0 30 obs ALL > $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/plot16_output
$PLOTDIR/plot_odas_obsfile_binned1x1.with_instid.p3.with_nobsa.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/ocean_obs\*/ $nyears $nmonths $ndays $nhours $year $month $day $hour SZ 0 30 omf-oma ALL > $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/plot5_output
#
##   vertical plots
$PLOTDIR/plot_odas_obsfile_vs_depth.p3.with_nobsa.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/ocean_obs\*/ $nyears $nmonths $ndays $nhours $year $month $day $hour TZ -30 30 0 360 0 300 ALL > $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/plot6_output
$PLOTDIR/plot_odas_obsfile_vs_depth.p3.with_nobsa.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/ocean_obs\*/ $nyears $nmonths $ndays $nhours $year $month $day $hour SZ -30 30 0 360 0 300 ALL > $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/plot7_output
$PLOTDIR/plot_odas_obsfile_vs_depth.p3.with_nobsa.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/ocean_obs\*/ $nyears $nmonths $ndays $nhours $year $month $day $hour TZ -90 -30 0 360 0 300 ALL > $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/plot8_output
$PLOTDIR/plot_odas_obsfile_vs_depth.p3.with_nobsa.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/ocean_obs\*/ $nyears $nmonths $ndays $nhours $year $month $day $hour SZ -90 -30 0 360 0 300 ALL > $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/plot9_output
$PLOTDIR/plot_odas_obsfile_vs_depth.p3.with_nobsa.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/ocean_obs\*/ $nyears $nmonths $ndays $nhours $year $month $day $hour TZ 30 90 0 360 0 300 ALL > $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/plot10_output
$PLOTDIR/plot_odas_obsfile_vs_depth.p3.with_nobsa.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/ocean_obs\*/ $nyears $nmonths $ndays $nhours $year $month $day $hour SZ 30 90 0 360 0 300 ALL > $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/plot11_output
$PLOTDIR/plot_odas_obsfile_vs_depth.p3.with_nobsa.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/ocean_obs\*/ $nyears $nmonths $ndays $nhours $year $month $day $hour TZ -10 10 120 280 0 300 ALL > $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/plot12_output
$PLOTDIR/plot_odas_obsfile_vs_depth.p3.with_nobsa.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/ocean_obs\*/ $nyears $nmonths $ndays $nhours $year $month $day $hour SZ -10 10 120 280 0 300 ALL > $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/plot13_output
$PLOTDIR/plot_odas_obsfile_vs_depth.p3.with_nobsa.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/ocean_obs\*/ $nyears $nmonths $ndays $nhours $year $month $day $hour TZ -90 90 0 360 0 3000 ALL > $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/plot14_output
$PLOTDIR/plot_odas_obsfile_vs_depth.p3.with_nobsa.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/ocean_obs\*/ $nyears $nmonths $ndays $nhours $year $month $day $hour SZ -90 90 0 360 0 3000 ALL > $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/plot15_output
 
#  now do the increment plots

#  NOTE:  currently set up for 1 5-day cycle for later 
#  perhaps ncap all the incr.nc files together if multiple time steps

$PLOTDIR/plot_increment.p3.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/mean_ana_restart/incr.nc temp  1 'TEMP INCREMENT LEVEL 1'  -5.0 5.0 incr.$nyears$nmonths$ndays.TEMP.LEV1.png >  $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/plot16_output
$PLOTDIR/plot_increment.p3.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/mean_ana_restart/incr.nc temp 10 'TEMP INCREMENT LEVEL 10' -2.0 2.0 incr.$nyears$nmonths$ndays.TEMP.LEV10.png > $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/plot17_output
$PLOTDIR/plot_increment.p3.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/mean_ana_restart/incr.nc salt  1 'SALT INCREMENT LEVEL 1'  -2.0 2.0 incr.$nyears$nmonths$ndays.SALT.LEV1.png >  $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/plot18_output
$PLOTDIR/plot_increment.p3.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/mean_ana_restart/incr.nc salt 10 'SALT INCREMENT LEVEL 10' -0.5 0.5 incr.$nyears$nmonths$ndays.SALT.LEV10.png > $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/plot19_output 

#  now do the depth at equator plots
#$PLOTDIR/calculate_xz_plot $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/mean_ana_restart/incr.nc 'temp' -5. 5. 300. $nyears $nmonths $ndays $nhours > calculate_xz_output1
#$PLOTDIR/calculate_xz_plot $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/mean_ana_restart/incr.nc 'salt' -5. 5. 300. $nyears $nmonths $ndays $nhours > calculate_xz_output2

#$PLOTDIR/xarray.xzplot.p3.py XZ_TEMP_-5-5_0-300_$nyears$nmonths$ndays$nhours.nc 'temp' -1.0 1.0 XZ_TEMP_-5-5_0-300_$nyears$nmonths$ndays$nhours.png > outputxz1
$PLOTDIR/xz_plots.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/mean_ana_restart/incr.nc 'temp' -5.0 5.0 300. $nyears $nmonths $ndays $nhours > outputxz1
#$PLOTDIR/xarray.xzplot.p3.py XZ_SALT_-5-5_0-300_$nyears$nmonths$ndays$nhours.nc 'salt' -0.2 0.2 XZ_SALT_-5-5_0-300_$nyears$nmonths$ndays$nhours.png > outputxz2
$PLOTDIR/xz_plots.py $EXPDIR/ocean_das/oana-$nyears$nmonths$ndays'_'$nhours/mean_ana_restart/incr.nc 'salt' -5.0 5.0 300. $nyears $nmonths $ndays $nhours > outputxz2

echo 'list all the plots'
ls -ltr *.png

#   now move the plots and plot output
 if ( -d $EXPDIR/plots_odas ) then
     echo 'Exists', $EXPDIR/plots_odas
 else
    echo 'createing plots_odas directory', $EXPDIR/plots_odas
    mkdir $EXPDIR/plots_odas
 endif
mv *.png $EXPDIR/plots_odas/.
mv plot*_output $EXPDIR/plots_odas/.
mv outputxz* $EXPDIR/plots_odas/.
echo 'moving plots'
 echo $EXPDIR

endif
