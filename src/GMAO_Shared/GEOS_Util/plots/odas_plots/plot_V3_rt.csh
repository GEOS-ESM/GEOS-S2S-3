#!/bin/csh -fv
#

#setenv TMPDIR /tmp/

#module purge
#module load comp/gcc/8.3.0
#module load python/GEOSpyD/Min24.4.0-0_py3.11
module load other/ImageMagick/latest
module load ncview/2.1.7

cd $EXPDIR
# Get date of increment to plot
set incr_date = `head -1 incr_date | awk '{printf "%s",substr($1,1,8)}' `;
set incr_date = $incr_date'_00'
set yyyy = `echo ${incr_date} | cut -c1-4`
set model_path = $EXPDIR/ocean_das
echo 'Increment Date and Final Restart: '$incr_date
rm -f ODAS_Check.txt
$GEOSUTIL/plots/odas_plots/plot_increment.v3.py $model_path/oana-$incr_date/mean_ana_restart/incr.nc 'temp' '1' 'Tinc' 4. -4. Tinc.png
$GEOSUTIL/plots/odas_plots/plot_stats.latest.v3.py Tprof glb 0 300 $yyyy
$GEOSUTIL/plots/odas_plots/plot_stats.latest.v3.py Sprof glb 0 300 $yyyy
$GEOSUTIL/plots/odas_plots/plot_stats.latest.v3.py ADT glb 0 300 $yyyy
$GEOSUTIL/plots/odas_plots/plot_v3_odas_ObsNum_daily.py $yyyy

set p0 = Tinc.png
set p1 = stats_Tprof.png
set p2 = stats_Sprof.png
set p3 = stats_ADT.png
set p4 = stats_odas_obs_v3.png

convert -resize 600x400 -background white $p0 p0.png
convert -resize 600x400 -background white $p1 p1.png
convert -resize 600x400 -background white $p2 p2.png
convert -resize 600x400 -background white $p3 p3.png
convert -resize 600x400 -background white $p4 p4.png

convert p0.png p1.png p2.png p3.png p4.png  -background white -append timeseries.png
rm -f p*png
pwd
echo "GiOcean-NRT ODAS Stats" > mail.txt
/usr/bin/mailx -s "GiOcean-NRT ODAS stats" -a timeseries.png li.ren@nasa.gov,kazumi.nakada@nasa.gov,andrea.m.molod@nasa.gov< mail.txt




