#!/bin/bash
SRC="diag_utils/MemUtilsMod.F90 common.f90 common_mpi.f90 common_mtx.f90 mod_sfmt.f90 params_letkf.f90 params_model.f90 params_obs.f90 vars_model.f90 vars_obs.f90 common_letkf.f90 common_mom4.f90 common_mpi_mom4.f90 common_obs_mom4.f90 letkf_obs.f90 letkf_local.f90 letkf_tools.f90 netlib2.f"
EXESRC="letkf.f90"
EXE="letkf.x"
MKL="-mkl=sequential"
#MKL=""
FPP="-fpp -DUSE_MKL_ODAS"  # use MKL instead of lean blas from netlib2.f
FOPTS="-O2"

rm -f *.mod *.x  *.o

mpiifort $FOPTS $FPP $MKL -traceback -convert big_endian -heap-array -I/discover/swdev/gmao_SIteam/Baselibs/ESMA-Baselibs-7.5.0/x86_64-pc-linux-gnu/ifort_2021.6.0-intelmpi_2021.6.0/Linux/include/netcdf -I/discover/swdev/gmao_SIteam/Baselibs/ESMA-Baselibs-7.5.0/x86_64-pc-linux-gnu/ifort_2021.6.0-intelmpi_2021.6.0/Linux/include/netcdf $SRC $EXESRC -o $EXE -L/discover/swdev/gmao_SIteam/Baselibs/ESMA-Baselibs-7.5.0/x86_64-pc-linux-gnu/ifort_2021.6.0-intelmpi_2021.6.0/Linux/lib -lnetcdff  -lnetcdf -lm -L/discover/swdev/gmao_SIteam/Baselibs/ESMA-Baselibs-7.5.0/x86_64-pc-linux-gnu/ifort_2021.6.0-intelmpi_2021.6.0/Linux/lib -L/discover/swdev/gmao_SIteam/Baselibs/ESMA-Baselibs-7.5.0/x86_64-pc-linux-gnu/ifort_2021.6.0-intelmpi_2021.6.0/Linux/lib -lnetcdf -ljpeg -lmfhdf -ldf -ljpeg -lhdf5_hl -lhdf5 -lm -L/discover/swdev/gmao_SIteam/Baselibs/ESMA-Baselibs-7.5.0/x86_64-pc-linux-gnu/ifort_2021.6.0-intelmpi_2021.6.0/Linux/lib -lsz -ljpeg -lgpfs -L/discover/swdev/gmao_SIteam/Baselibs/ESMA-Baselibs-7.5.0/x86_64-pc-linux-gnu/ifort_2021.6.0-intelmpi_2021.6.0/Linux/lib -lcurl -lssl -lcrypto -lssl -lcrypto -lz -lm -pthread -ldl -lm -L/discover/swdev/gmao_SIteam/Baselibs/ESMA-Baselibs-7.5.0/x86_64-pc-linux-gnu/ifort_2021.6.0-intelmpi_2021.6.0/Linux/lib -lcurl -lssl -lcrypto -lssl -lcrypto -lz -lm -pthread 
