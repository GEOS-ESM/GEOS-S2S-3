#!/bin/csh 
#module load other/cdo  # sles11
module load cdo/1.9.9rc2 #sles12
# input:
# fort.13 -> 0 c start date ; start date of forecast
# fort.12 -> 1 r2 pert date
# fort.11 -> 2 r1 pert date 

# output:
# fort.31 -> perturbation 

set factor = $1 
echo $factor

# U
cdo --silent -L replace fort.13 -add -selname,U fort.13 -mulc,$factor -sub -selname,U fort.12 -selname,U fort.11 fort.1031
#cdo --silent -L replace fort.13 -sub -selname,U fort.13 -mulc,$factor -sub -selname,U fort.12 -selname,U fort.11 fort.1031

# V
cdo --silent -L replace fort.1031 -add -selname,V fort.13 -mulc,$factor -sub -selname,V fort.12 -selname,V fort.11 fort.1131
#cdo --silent -L replace fort.1031 -sub -selname,V fort.13 -mulc,$factor -mulc,1.5 -sub -selname,V fort.12 -selname,V fort.11 fort.1131

# PT
cdo --silent -L replace fort.1131 -add -selname,PT fort.13 -mulc,$factor -sub -selname,PT fort.12 -selname,PT fort.11 fort.31
#cdo --silent -L replace fort.1131 -sub -selname,PT fort.13 -mulc,$factor -sub -selname,PT fort.12 -selname,PT fort.11 fort.31

wait

/bin/rm -f fort.1031
/bin/rm -f fort.1131
