#!/bin/csh -vx
# very custom selection of experimental V3 forecasts

#18 May 2020: for SLES-12
module purge
module load comp/gcc/9.2.0
#module load python/GEOSpyD/Ana2019.10_py2.7
module load python/GEOSpyD/Ana2019.10_py3.7  #python 3

# SLES11
#module load other/comp/gcc-4.9.2-sp3
#module load other/mpi/mvapich2-2.1/gcc-4.9.2-sp3
#module load lib/mkl-15.0.2.164
#module load other/SIVO-PyD/spd_1.24.0_gcc-4.9.2-sp3_mkl-15.0.2.164_mvapich2-2.1

set regnames = {nino3.4,}  #nino2,nino3,nino4,nino1+2,idm,tasi,idm_west,idm_east,nino1} # nat, sat 
set datestamps4 = {20151102,20151107,20151112,20151117,20151122,20151127}
set ensmems4 = {ens400,ens401,ens402,ens403,ens404} 
set datestamps10 = {20151127}
set ensmems10 = {ens501,ens502,ens503,ens504,ens505,ens506,ens507,ens508,ens509,ens510}

foreach datestamp ($datestamps4)
   foreach ensmem ($ensmems4)
      foreach reg ($regnames)
         set label = $datestamp$ensmem
#         echo $label
       ./text_indices_S2S_fcst_full_v3_daily_select.py -r $reg  $label
     end
   end
end

foreach datestamp ($datestamps10)
   foreach ensmem ($ensmems10)
      foreach reg ($regnames)
         set label = $datestamp$ensmem
#         echo $label
       ./text_indices_S2S_fcst_full_v3_daily_select.py -r $reg  $label
     end
   end
end
