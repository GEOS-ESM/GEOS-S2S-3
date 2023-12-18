#!/bin/csh
#
module purge
module load comp/gcc/8.3.0
module load python/GEOSpyD/Ana2019.10_py2.7

#set main = /gpfsm/dhome/gmaofcst/ODAS/OBS/V3/
set main = /discover/nobackup/lren1/odas_pre_proc

set run    = $main/MOOR
set lib    = $main/LIBRARY
set rawdir = /discover/nobackup/lren1/pre_proc/NRT/MOOR/RAW
#set rawdir = /gpfsm/dnb78s2/projects/p26/ehackert/TAO_PIRATA_RAMA_processing/obs/raw/MOOR


