#!/bin/csh
#

module purge
module load comp/gcc/8.3.0
module load python/GEOSpyD/Ana2019.10_py2.7

set main = /discover/nobackup/lren1/odas_pre_proc

set raw   = /discover/nobackup/lren1/pre_proc/NRT/AVISO/RAW/
set assim = /discover/nobackup/lren1/pre_proc/NRT/AVISO/assim/ADT_TRK_7.0/YEARLY_FILES

set run   = $main/AVISO
set lib   = $main/LIBRARY





#
#  the end
#



