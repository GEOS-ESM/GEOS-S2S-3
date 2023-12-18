#!/bin/csh 
#module load other/cdo  # sles11
module load cdo/1.9.9rc2 #sles12
# input:
# fort.23 -> 0 c start date ; start date of forecast
# fort.22 -> 1 r1 pert date
# fort.21 -> 2 r2 pert date 

# output:
# fort.41 -> r pert

set factor = $1
echo $factor

# Q
cdo --silent -L replace fort.23 -add -selname,Q fort.23 -mulc,$factor -sub -selname,Q fort.22 -selname,Q fort.21 fort.41
#cdo --silent -L replace fort.23 -sub -selname,Q fort.23 -mulc,$factor -sub -selname,Q fort.22 -selname,Q fort.21 fort.41

