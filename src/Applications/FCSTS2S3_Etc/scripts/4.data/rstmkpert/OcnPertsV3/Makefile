LIB_NETCDF      = -L/discover/nobackup/projects/gmao/share/dao_ops/Baselibs/v3.2.0_build3/x86_64-unknown-linux-gnu/ifort/Linux/lib -lnetcdf -L/discover/nobackup/projects/gmao/share/dao_ops/Baselibs/v3.2.0_build3/x86_64-unknown-linux-gnu/ifort/Linux/lib -lhdf5_hl -lhdf5 -lz -lm -L/discover/nobackup/projects/gmao/share/dao_ops/Baselibs/v3.2.0_build3/x86_64-unknown-linux-gnu/ifort/Linux/lib -lmfhdf -ldf -lsz -ljpeg -lm -L/discover/nobackup/projects/gmao/share/dao_ops/Baselibs/v3.2.0_build3/x86_64-unknown-linux-gnu/ifort/Linux/lib -lmfhdf -ldf -L/discover/nobackup/projects/gmao/share/dao_ops/Baselibs/v3.2.0_build3/x86_64-unknown-linux-gnu/ifort/Linux/lib -lcurl -lrt -lm -lz -lm
INC_NETCDF      = /discover/nobackup/projects/gmao/share/dao_ops/Baselibs/v3.2.0_build3/Linux/include/netcdf
F90_COMPILE	= ifort
F90FLAGS    =  -traceback -convert big_endian -O2
# -warn unused
# -g -real_size 32
.SUFFIXES: .o .f90 .txt 

perturb_ogcm_scaleT:
	$(F90_COMPILE) $(F90FLAGS) perturb_ogcm_scaleT.f90 modules.f90 $(LIB_NETCDF) -I $(INC_NETCDF) -o perturb_ogcm_scaleT.exe
