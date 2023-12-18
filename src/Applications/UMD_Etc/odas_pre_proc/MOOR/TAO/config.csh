#!/bin/csh
#
module purge
module load comp/gcc/8.3.0
module load python/GEOSpyD/Ana2019.10_py2.7

#set main = /gpfsm/dhome/gmaofcst/ODAS/OBS/V3/
#set main = /gpfsm/dnb78s2/projects/p26/ehackert/ODAS/OBS/V3/
set main = /discover/nobackup/lren1/odas_pre_proc

set run  = $main/MOOR/TAO
set lib  = $main/LIBRARY
set syn  = $main/SYN_SALT

set pathhome     = $main/MOOR/TAO
set pathraw      = /discover/nobackup/lren1/pre_proc/NRT/MOOR/RAW/TAO
#set pathfinal    = /gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/assim/TAO/V3/FINAL/
set pathfinal    = /discover/nobackup/lren1/pre_proc/NRT/MOOR/TAO/V3/FINAL
echo  'in config',$pathhome
