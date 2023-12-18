#!/bin/csh -vx

#########################################################################
##### script to untar daily seasonal history files, 
##### extract a sigle variable, and remove the daily files
#########################################################################

module load other/cdo

set ICyear = {1982} #{2005}
#set ICdates = {20051101,20051106,20051111,20051116,20051121,20051126} 
set ICdates = {19820405,19820410,19820415,19820420,19820425,19820430}  #{20050301} #{20170326,20170301,20170306,20170311,20170316,20170321} #{nov27} # {nov01,nov02,nov03,nov04,nov05,nov06,nov07,nov08,nov09,nov10,nov11,nov12,nov13,nov14,nov15,nov16,nov17,nov18,nov19,nov20,nov21,nov22,nov23,nov24,nov25,nov26,nov27,nov28,nov29,nov30}
set fcstmon = {198206}  #{200503} #{200601} #{201801}  #{201512}  # month to use for stratification, the second full month of integration, i.e. Jun for Apr ICs
#set V3date = {19820601}  #{20050301} #{20171101} #

set collection = {geosgcm_ocn2d}  #{geosgcm_vis2d}
set xvar = {TS}

#set ensmems = {37,38,39,40,41} #,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76}
set ensmems = # {337,338,339,340,341,342,343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367,368,369,370,371,372,373,374,375,376}
#set ensmems = {380,400,401,402,403,404,405,406,407,408,409,410,411,412,413,414,415,416} # 377,378,379,381,382,383,384,385,386,387,388,389,390,391,392,393,394,395,396,397,398,399,
set ensmems = {200} #{200,201,202,203,204}
#set ensmems = {337,341,345,349,353,357,361,365,369,373}
#set ensmems = {301,302,303,304,305,306,307,308,309,310}

set utildir = '/gpfsm/dnb04/projects/p71/aogcm/g5fcst/forecast/production/geos-s2s/util/stratify/extract/'
cd $utildir

#set expdir = '/gpfsm/dnb02/projects/p58/aogcm/g5fcst/forecast/study4ver3/geos-s2s/runx/'
#set expdir = '/discover/nobackup/projects/gmao/t2ssp/geosgs2s/GEOS_exp/ic33p2-newskin-15102018gc/'
#set expdir = '/discover/nobackup/projects/gmao/t2ssp/geosgs2s/GEOS_exp/yuri-s2s3-donifan-062119/'
set expdir = '/discover/nobackup/projects/gmao/t2ssp/geosks2s/GEOS_exp/pfe/NOASM/'
#set archdir = '/archive/gmaofcst/GEOS_S2Sv3' # let's try reading tar files from archive

foreach ICdate ($ICdates)
   foreach ensmem ($ensmems)
      #cd $expdir/$ICdate/ens$ensmem/   #$collection/
      set filename = $ICdate.$collection.daily.$fcstmon.nc4.tar
      #set filename = $V3date.$collection.daily.$fcstmon.nc4.tar
      #set filename = $archdir/$ICdate/ens$ensmem/$collection/$ICdate.$collection.daily.$fcstmon.nc4.tar
      echo $filename
      cp $expdir/$ICdate/ens$ensmem/$filename .
      tar -xvf $filename
      set fmon = `echo $fcstmon | cut -c5-6`
      set numdays = 31
      if ( $fmon == 04 ) set numdays = 30
      @ day = 1
      while ( $day <= $numdays )
         if ( $day < 10 ) then
            set ddate = ${fcstmon}0$day
         else
            set ddate = $fcstmon$day
         endif
         echo $ICdate.$collection.${ddate}_0900z.nc4
         cdo -selvar,TS $ICdate.$collection.${ddate}_0900z.nc4 $ICdate.TS.$ddate.nc4
         rm -f $ICdate.$collection.${ddate}_0900z.nc4
         #echo $V3date.$collection.${ddate}_0900z.nc4
         #cdo -selvar,TS $V3date.$collection.${ddate}_0900z.nc4 $ICdate.TS.$ddate.nc4
         #rm -f $V3date.$collection.${ddate}_0900z.nc4
         @ day = $day + 1
      end 
   end
end
cd $utildir
