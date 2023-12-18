.PHONY:
	all clean veryclean

#all: read_argo_wmo.x
all: vertical_thin.x

# New for sles-12
# Need to link Library
# module load comp/gcc/8.3.0

#BASEDIR = /discover/swdev/gmao_SIteam/Baselibs/ESMA-Baselibs-6.0.13-SLES12/x86_64-unknown-linux-gnu/gfortran_8.3.0-mpiuni/Linux
BASEDIR = /discover/swdev/gmao_SIteam/Baselibs/ESMA-Baselibs-6.0.27/x86_64-pc-linux-gnu/ifort_19.1.3.304-intelmpi_19.1.3.304/Linux
BASEBIN = ${BASEDIR}/bin

LIB_NETCDF := $(shell $(BASEBIN)/nf-config --flibs)
INC_NETCDF := $(shell $(BASEBIN)/nf-config --cflags)
INC_HDF = $(BASEDIR)/include/hdf

FC      = gfortran
FCFLAGS = -g -fbacktrace

#FC      = mpiifort
#FCFLAGS = -O0 -g -traceback -convert big_endian -extend-source -debug -nolib-inline -fno-inline-functions -assume protect_parens,minus0 -prec-div -prec-sqrt -check all,noarg_temp_created -fp-stack-check -warn unused -init=snan,arrays

LIB_PROFILES = ../LIBRARY/libprofiles.a
LIBS = $(LIB_NETCDF) -lpthread
INCS = -I../LIBRARY $(INC_NETCDF)

%.o : %.f90
	$(FC) $(FCFLAGS) $(LIBS) $(INCS) -c $<

#read_argo_wmo.x: read_argo_wmo.o $(LIB_PROFILES)
#	$(FC) $< $(FCFLAGS) $(LIB_PROFILES) $(LIBS) $(INCS) -o $@
	
vertical_thin.x: vertical_thin.o $(LIB_PROFILES)
	$(FC) $< $(FCFLAGS) $(LIB_PROFILES) $(LIBS) $(INCS) -o $@
	
clean:
	rm -f *.o *.mod core.*

#veryclean: clean
#	rm -f make_read_argo_wmo
	
veryclean: clean
	rm -f make_vertical_thin
	
