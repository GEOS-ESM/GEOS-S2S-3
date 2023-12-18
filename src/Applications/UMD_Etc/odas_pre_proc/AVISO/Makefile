.PHONY:
	all clean veryclean

all: adt_sigos_sles12.x

# New for sles-12
# Need to link Library
# module load comp/gcc/8.3.0

BASEDIR = /discover/swdev/gmao_SIteam/Baselibs/ESMA-Baselibs-6.0.13-SLES12/x86_64-unknown-linux-gnu/gfortran_8.3.0-mpiuni/Linux

BASEBIN = ${BASEDIR}/bin

LIB_NETCDF := $(shell $(BASEBIN)/nf-config --flibs)
INC_NETCDF := $(shell $(BASEBIN)/nf-config --cflags)
INC_HDF = $(BASEDIR)/include/hdf

FC      = gfortran
FCFLAGS = -g -fbacktrace

LIB_PROFILES = ../LIBRARY/libprofiles.a
LIBS = $(LIB_NETCDF) -lpthread
INCS = -I../LIBRARY $(INC_NETCDF)

%.o : %.f90
	$(FC) $(FCFLAGS) $(LIBS) $(INCS) -c $<
	

adt_sigos_sles12.x: adt_sigos_sles12.o $(LIB_PROFILES)
	$(FC) $< $(FCFLAGS) $(LIB_PROFILES) $(LIBS) $(INCS) -o $@
		
clean:
	rm -f *.o *.mod core.*

	
veryclean: clean
	rm -f make_adt_sigos_sles12
	
	
