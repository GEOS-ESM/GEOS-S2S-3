#!/bin/csh
#

module purge
module load comp/gcc/8.3.0
module load python/GEOSpyD/Ana2019.10_py2.7

# for fortran from Matt
module load comp/intel/19.1.3.304 mpi/impi/19.1.3.304

set main = /discover/nobackup/lren1/odas_pre_proc

set run = $main/ARGO
set lib = $main/LIBRARY

set raw    = /discover/nobackup/lren1/pre_proc/NRT/ARGO/RAW

set step1  = /discover/nobackup/lren1/pre_proc/NRT/ARGO/STEP1
set step2  = /discover/nobackup/lren1/pre_proc/NRT/ARGO/STEP2
set final  = /discover/nobackup/lren1/pre_proc/NRT/ARGO/FINAL


