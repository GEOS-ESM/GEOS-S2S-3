#!/bin/csh
#

module purge
module load comp/gcc/8.3.0
module load python/GEOSpyD/Ana2019.10_py2.7

# for fortran from Matt
module load comp/intel/19.1.3.304 mpi/impi/19.1.3.304

#set main = /gpfsm/dhome/gmaofcst/ODAS/OBS/V3/
set main = /gpfsm/dnb78s2/projects/p26/ehackert/ODAS/OBS/V3/

set run = $main/ARGO
set lib = $main/LIBRARY

#set raw    = /gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/ARGO/RAW/
set raw     = /gpfsm/dnb78s2/projects/p26/ehackert/obs/raw/ARGO/RAW/

set step1  = /gpfsm/dnb78s2/projects/p26/ehackert/obs/assim/ARGO/V3/STEP1
set step2  = /gpfsm/dnb78s2/projects/p26/ehackert/obs/assim/ARGO/V3/STEP2
set final  = /gpfsm/dnb78s2/projects/p26/ehackert/obs/assim/ARGO/V3/FINAL


