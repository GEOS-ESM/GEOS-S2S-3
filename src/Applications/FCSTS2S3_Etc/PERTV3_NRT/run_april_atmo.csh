#!/bin/csh

# Load the python module
module load python/GEOSpyD/Min23.5.2-0_py3.11

# List of dates
set dates = (20160326)

# Loop over each date
foreach date ($dates)

    # Run the python script
    #./run_perturbations_V3.py $date
    ./run_perturbations_V3-paralell.py $date
    # Define source and destination paths
    set pathA_src = "AtmPertsV3/output/"
    set pathA_dst = "AtmPertsV3/output_${date}/"
    set pathO_src = "OcnPertsV3/output"
    set pathO_dst = "OcnPertsV3/output_${date}"

    # Check if source directories exist before moving
    if (-d $pathA_src) then
        mv $pathA_src $pathA_dst
    endif

    if (-d $pathO_src) then
        mv $pathO_src $pathO_dst
    endif
end
