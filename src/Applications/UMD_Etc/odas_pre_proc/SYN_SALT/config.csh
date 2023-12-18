#!/bin/csh
#

module purge
module load comp/gcc/8.3.0
module load python/GEOSpyD/Ana2019.10_py2.7

#set main = /gpfsm/dnome/gmaofcst/ODAS/OBS/V3
#set main = /gpfsm/dnb78s2/projects/p26/ehackert/ODAS/OBS/V3/
set main = /discover/nobackup/lren1/odas_pre_proc
set run = $main/XBT_CTD
set lib  = $main/LIBRARY
set syn = $main/SYN_SALT



