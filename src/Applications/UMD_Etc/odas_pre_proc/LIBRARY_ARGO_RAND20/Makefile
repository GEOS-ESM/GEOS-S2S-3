.PHONY:
	all clean veryclean

BASEDIR = /discover/swdev/gmao_SIteam/Baselibs/ESMA-Baselibs-6.0.13-SLES12/x86_64-unknown-linux-gnu/gfortran_8.3.0-mpiuni/Linux
#BASEDIR = /discover/swdev/gmao_SIteam/Baselibs/ESMA-Baselibs-6.0.27/x86_64-pc-linux-gnu/ifort_19.1.3.304-intelmpi_19.1.3.304/Linux
#BASEDIR = /discover/swdev/gmao_SIteam/Baselibs/ESMA-Baselibs-6.0.13-SLES12/x86_64-unknown-linux-gnu/gfortran_8.3.0-mpiuni/Linux

BASEBIN = ${BASEDIR}/bin

LIB_NETCDF := $(shell $(BASEBIN)/nf-config --flibs)
INC_NETCDF := $(shell $(BASEBIN)/nf-config --cflags)

FC      = gfortran
FCFLAGS = -g -fbacktrace -fconvert=big-endian -ffree-line-length-none

INCS    = $(INC_NETCDF) -I.
LIBS    = $(LIB_NETCDF)

all: libprofiles.a

%.o : %.f90
	$(FC) -c $(FCFLAGS) $(LIBS) $(INCS) -c $<
 
libprofiles.a: data_id_module.o argo_netcdf_module.o calendar.o levitus_clim_module.o ts_qc_module.o zeus_clocks.cpp.o misc_module.o netcdf_module_odas2.o pmel_module.o syn_salt_module.o netcdf_module_moor.o super_module.o sort_module.o buoyancy.cpp.o
	ar rcs $@ $^
	ranlib $@

clean:
	rm -f *.o *.a *.mod

veryclean: clean
