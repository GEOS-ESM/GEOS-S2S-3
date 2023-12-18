#!/bin/csh

# run_perturbations_V3.csh 2020 10 02 (YYYY MM DD)
# make sure MM and DD are two-digit

limit stacksize unlimited
limit vmemoryuse unlimited

# Start: User Defined Parameters      
set year = $1        
set mon  = $2       
set day  = $3     
# End: User Defined Parameters
set pertdata = $year$mon$day

# location of experimental perturbations 
setenv XDIR /gpfsm/dnb02/projects/p58/aogcm/g5fcst/s2sv3/
# temporary setting for ODAS run name
set ananame = 'eh020'
# temporary location of the daily restarts 
setenv RESDIR /gpfsm/dnb02/projects/p58/aogcm/g5fcst/s2sv3/netcdf/V3/${ananame}_hindcast_restarts_extract/

# This is a list of calendar dates on which forecasts are run each year.
# These dates are the dates of the restarts, i.e. mmdd_21z  The forecast directory is usually named with the next date at 00z, i.e. restart 1231_21z, forecast 0101_01z

#ATM pertubation dates
set atmDJF = {1201,1206,1211,1216,1221,1226,1231,0105,0110,0115,0120,0125,0130,0204,0209,0214,0219,0224}
set atmMAM = {0301,0306,0311,0316,0321,0326,0331,0405,0410,0415,0420,0425,0430,0505,0510,0515,0520,0525,0530}
set atmJJA = {0604,0609,0614,0619,0624,0629,0704,0709,0714,0719,0724,0729,0803,0808,0813,0818,0823,0828}
set atmSON = {0902,0927,0912,0917,0922,0927,1002,1007,1012,1017,1022,1027,1101,1106,1111,1116,1121,1126}

# OCN perturbation dates
set ocnDJF = {1226,0130,0224}
set ocnMAM = {0326,0425,0530}
set ocnJJA = {0629,0729,0828}
set ocnSON = {0927,1027,1126}

set iday = 01
set iyear = $year
set doATMperts = 0
foreach testdate ( $atmDJF )
   if ( $mon$day == $testdate ) then
      set doATMperts = 1 
      set season = 'DJF'
      set imon = 12
      if ( $mon == 12) @ iyear = $iyear + 1
   endif
end
foreach testdate ( $atmMAM )
   if ( $mon$day == $testdate ) then
      set doATMperts = 1 
      set season = 'MAM'
      set imon = 03
   endif
end
foreach testdate ( $atmJJA )
   if ( $mon$day == $testdate ) then 
      set doATMperts = 1 
      set season = 'JJA'
      set imon = 06
   endif
end
foreach testdate ( $atmSON )
   if ( $mon$day == $testdate ) then
      set doATMperts = 1 
      set season = 'SON'
      set imon = 09
   endif
end

set doOCNperts = 0
foreach testdate ( $ocnDJF )
   if ( $mon$day == $testdate ) set doOCNperts = 1 
end
foreach testdate ( $ocnMAM )
   if ( $mon$day == $testdate ) set doOCNperts = 1 
end
foreach testdate ( $ocnJJA )
   if ( $mon$day == $testdate ) set doOCNperts = 1 
end
foreach testdate ( $ocnSON )
   if ( $mon$day == $testdate ) set doOCNperts = 1 
end

echo 'doATMperts', $doATMperts
echo 'doOCNperts', $doOCNperts

set numdays = 31
if ( $mon == 11 | $mon == 06 | $mon == 04 | $mon == 09 ) set numdays = 30
if ( $mon == 02 ) set numdays = 28

# Do ATM perturbations
if ( $doATMperts ) then
   cd AtmPertsV3/
# The file with separations and corresponding weights (from the .../SANDBOX/ENSEMBLE/)
# This file must be pre-made (see README_V3_perts)
   set AtmSep = `cat -n alpha10_rho_${season}_9ddiffs_V3_${ananame}.txt | head -n 1`
   set SeasAtmFac = `cat -n alpha10_rho_${season}_9ddiffs_V3_${ananame}.txt | head -n 2 | tail -n 1`
   echo $AtmSep
   echo $SeasAtmFac

  @ n = 3
  @ m = 2   
  while ( $n < 12 )
      set sep = `echo $AtmSep[$n] | tr -d ','`
      set fac = $SeasAtmFac[$m]
      echo $sep $fac >> test_seps_facs.txt
      @ n = $n + 1
      @ m = $m + 1
   end
# Create random dates from the adjacent season and random separation.   
   set ind2beg = `shuf -i1-60 -n5`
   set ind4seps = `shuf -i1-9 -n5`
# Create proper date labels for the restart files to get these perturbations from.
   @ ii = 0
   foreach ind ( $ind2beg )
      @ ii = $ii + 1
      @ tmpday = $iday + $ind
      if ( $tmpday > $numdays ) then
         @ ptmpday = $tmpday - $numdays
         @ ptmpmon = $imon + 1
         @ pertyrn = $iyear
      else
         @ ptmpday = $tmpday
         @ ptmpmon = $imon
         @ pertyrn = $iyear
      endif

      set pertday = $ptmpday
      set pertmon = $ptmpmon
      if ( $ptmpday < 10 ) set pertday = 0$ptmpday
      if ( $ptmpmon < 10 ) set pertmon = 0$ptmpmon
      if ( $ptmpmon > 12 ) then
         @ pertmon = $ptmpmon - 12
         set pertmon = 0$pertmon
         @ pertyrn = $iyear + 1
      endif
  
      set pertbdates = $pertyrn$pertmon$pertday
      echo "$ii $pertbdates BEGIN FOR $pertdata"
  
      set sepfac = `cat test_seps_facs.txt | head -n $ind4seps[$ii] | tail -n 1`
      echo $sepfac
      echo $pertbdates $sepfac >> rand_5_pert${pertdata}.txt
   end

# Test the text file with the dates, separations and weights and move it to the /files_$ananame directory
# This "while" loop can be deleted, if everything looks ok.
   @ nt = 1
   while ( $nt <= 5 )
      set datsepfac = `cat rand_5_pert${pertdata}.txt | head -n $nt | tail -n 1`
      echo $datsepfac
   
      set rdat = `echo $datsepfac | cut -c1-8`
      set difr = `echo $datsepfac | cut -d' ' -f2`
      set fact = `echo $datsepfac | cut -d' ' -f3`
      echo rdat fact difr $rdat $fact $difr
      @ nt = $nt + 1
   end
   mkdir -p files_$ananame
   mv rand_5_pert${pertdata}.txt files_${ananame}

# Call to the main script that does ATM perturbations. The perturbed restarts will be located in /AtmPerts/data_$ananame/$pertdata/.  They are called ens200(unperturbed) and ens201-204 (perturbed).  This naming can be changed at the top of the setup_LB_V3_perts_from_file script.
   ./setup_LB_V3_perts_from_file.csh $pertdata $ananame
endif

cd $XDIR
# Now do OCN perturbations
if ( $doOCNperts ) then
   cd OcnPertsV3/
# The file with separations and corresponding weights (from the .../SANDBOX/ENSEMBLE/)
   set OcnSep = `cat -n ocn_alpha10_${season}_9ddiffs_5levs_V3_${ananame}.txt | head -n 1`
   set SeasOcnFac = `cat -n ocn_alpha10_${season}_9ddiffs_5levs_V3_${ananame}.txt | head -n 2 | tail -n 1`
   echo $OcnSep
   echo $SeasOcnFac

   @ n = 3
   @ m = 2   
   while ( $n < 12 )
      set sep = `echo $OcnSep[$n] | tr -d ','`
      set fac = $SeasOcnFac[$m]
      echo $sep $fac >> ocn_test_seps_facs.txt
     @ n = $n + 1
      @ m = $m + 1
   end

   echo 'CREATE A RANDOM SET'
   /bin/rm -f rand_10_pert${pertdata}.txt
   touch rand_10_pert${pertdata}.txt

   set ind2beg = `shuf -i1-60 -n10`
# shuf does not allow repetitions, so need something different for ocn perts
   set ind4seps = `repeat 10 perl -e 'print int(rand() * 9 + 1) . "\n";'`
   echo 'ind4seps' $ind4seps

   @ ii = 0
   foreach ind ( $ind2beg )
     @ ii = $ii + 1
     @ tmpday = $iday + $ind
     if ( $tmpday > $numdays ) then
       @ ptmpday = $tmpday - $numdays
       @ ptmpmon = $imon + 1
       @ pertyrn = $iyear
     else
       @ ptmpday = $tmpday
       @ ptmpmon = $imon
       @ pertyrn = $iyear
     endif

     set pertday = $ptmpday
     set pertmon = $ptmpmon
     if ( $ptmpday < 10 ) set pertday = 0$ptmpday
     if ( $ptmpmon < 10 ) set pertmon = 0$ptmpmon
     if ( $ptmpmon > 12 ) then
        @ pertmon = $ptmpmon - 12
        set pertmon = 0$pertmon
        @ pertyrn = $iyear + 1
     endif
     set pertbdates = $pertyrn$pertmon$pertday
     echo "$ii $pertbdates BEGIN FOR $pertdata"
  
     set sepfac = `cat ocn_test_seps_facs.txt | head -n $ind4seps[$ii] | tail -n 1`
     echo $sepfac
     echo $pertbdates $sepfac >> rand_10_pert${pertdata}.txt
   end
   mkdir -p files_$ananame
   mv rand_10_pert${pertdata}.txt files_${ananame}

   ./setup_ocn_taper_perts_V3_from_file.csh $pertdata $ananame
endif
