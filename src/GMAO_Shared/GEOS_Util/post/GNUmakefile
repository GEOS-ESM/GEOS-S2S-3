SHELL=/bin/sh

#.......................................................................
#
# Makefile for ESMA components
#
# REVISION HISTORY:
#
#  28Aug2006  stassi  Created with gmm script.
#  16Mar2007  Kokron  fix location of libsz   
#  15Oct2010  Todling install rs2hdf.x
#
#.......................................................................

########################################################################

BINS = makeiau.x hdf2rs.x gg2fv.x eta2prs.x eta2rst.x stats.x rs_hinterp.x \
       time_ave.x time_ave_nompi.x binarytile.x convert_eta.x rs2hdf.x \
       ec_prs2fv.x flat2hdf.x fvrst.x gg2eta.x ec_prs2eta.x \
       merra2scm.x rs_numtiles.x

BINSR8 = convert_aerosols.x

UT = checkdate.x swapendian_FV.x swapendian_RST.x

PKGS = 
MOD_DIRS = $(INC_GEOS_SHARED) $(INC_MAPL_BASE) $(INC_ESMF) $(INC_SDF) $(INC_MPI) $(INC_GMAO_SHARED)
PERLSCRIPTS = $(wildcard *.pl)
PERLPKGS = 
SHELLSCRIPTS = ec2grd.csh
RCFILES = time_ave.rc
NMLFILES = 
SUBDIRS = 

########################################################################

SCRIPTS  = $(PERLSCRIPTS) $(PERLPKGS) $(SHELLSCRIPTS)
ETCFILES = $(RCFILES) $(NMLFILES)

scripts  = $(SCRIPTS)
perl     = $(PERLSCRIPTS)
csh      = $(SHELLSCRIPTS)
etc      = $(ETCFILES)
rc       = $(RCFILES)
nml      = $(NMLFILES)

SEDCMD = s^\@DASPERL^\$(PERL)^;s^\@DASSED^\$(SED)^;s^\@DASHELL^\$(SHELL)^

########################################################################

THIS = $(shell basename `pwd`)
LIB  = lib$(THIS).a
LIBZ = lib$(THIS)_nompi.a

########################################################################

#---------------------------------------------------
# Get GNUmakefile name
# note: MAKEFILE_LIST defined for gmake versions >= 3.80
#---------------------------------------------------
ifdef MAKEFILE_LIST
   GNUMAKEFILE := $(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))
else
   GNUMAKEFILE := GNUmakefile
endif


#-------------------------------
# Compilation rules, flags, etc
#-------------------------------
include $(ESMADIR)/Config/ESMA_base.mk   # Generic stuff
include $(ESMADIR)/Config/ESMA_arch.mk   # System dependencies
include $(ESMADIR)/Config/GMAO_base.mk   # System dependencies


#-----------------------
# Standard ESMA Targets
#-----------------------
THIS = $(shell basename `pwd`)

esma_install install: $(LIB) $(BINS) $(BINSR8) # $(UT)
	$(MKDIR) $(ESMABIN) $(ESMALIB)
	-$(CP) -p $(BINS) $(ESMABIN)
	-$(CP) -p $(BINSR8) $(ESMABIN)
	$(CP) $(LIB) $(ESMALIB)

	@if [ "$(SCRIPTS)" != "  " ]; then (\
	   gmake -f \$(GNUMAKEFILE) scripts \
	) fi

	@if [ "$(ETCFILES)" != " " ]; then (\
	   gmake -f \$(GNUMAKEFILE) etc \
	) fi

	@if [ "$(SUBDIRS)" != "" ]; then (\
	   SUBTARGET=$@;export SUBTARGET;gmake -f \$(GNUMAKEFILE) subdirs\
	) fi

esma_clean clean:
	$(RM) *~ *.[aox] *.[Mm][Oo][Dd] *___.F

	@if [ "$(SUBDIRS)" != "" ]; then (\
	   SUBTARGET=$@;export SUBTARGET;gmake -f \$(GNUMAKEFILE) subdirs\
	) fi

esma_distclean distclean:
	$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd] *___.F

	@if [ "$(SUBDIRS)" != "" ]; then (\
	   SUBTARGET=$@;export SUBTARGET;gmake -f \$(GNUMAKEFILE) subdirs\
	) fi

esma_doc doc:
	@echo No documentation here

esma_help help :
	@echo "Standard ESMA targets:"
	@echo "% gmake esma_install    (builds and install under ESMADIR)"
	@echo "% gmake esma_clean      (removes deliverables: *.[aox], etc)"
	@echo "% gmake esma_distclean  (leaves in the same state as cvs co)"
	@echo "% gmake esma_doc        (generates PDF, installs under ESMADIR)"
	@echo "% gmake esma_help       (this message)"
	@echo "% gmake scripts         (installs scripts only)"
	@echo "% gmake perl            (installs perl scripts only)"
	@echo "% gmake csh             (installs shell scripts only)"
	@echo "% gmake etc             (installs rc and nml files only)"
	@echo "% gmake rc              (installs rc files only)"
	@echo "% gmake nml             (installs nml files only)"
	@echo "% gmake ut              (compiles utility programs)"
	@echo "Environment:"
	@echo "      ESMADIR = $(ESMADIR)"
	@echo "      BASEDIR = $(BASEDIR)"
	@echo "         ARCH = $(ARCH)"
	@echo "         SITE = $(SITE)"
	@echo "         LIBZ = $(LIBZ)"
	@echo "         SRCZ = $(SRCZ)"
	@echo "         OBJZ = $(OBJZ)"

########################################################################

scripts perl csh:
	@echo ""
	@for scr in $($@); do  \
	   ( $(SED) -e "$(SEDCMD)"< $$scr > $(ESMABIN)/$$scr; \
	     echo "script to \$$(ESMABIN): $$scr";  \
	     chmod 755 $(ESMABIN)/$$scr ) \
	done
	@echo ""
	@echo "\$$(ESMABIN) = $(ESMABIN)"
	@echo ""

etc rc nml:
	@echo ""
	@for file in $($@); do  \
	   ( $(CP) $$file $(ESMAETC); \
	     echo "copy to \$$(ESMAETC): $$file")  \
	done
	@echo ""
	@echo "\$$(ESMAETC) = $(ESMAETC)"
	@echo ""

subdirs:
	@ SUBTARGET=
	@ for subdir in $(SUBDIRS); do  \
	   ( cd $$subdir;  \
	     echo "$(MAKE) -e $(SUBTARGET) $$subdir";  \
	     $(MAKE) -e $(SUBTARGET) ) \
	done

ut: $(UT)

########################################################################

#----------------------
# User Defined Targets
#----------------------

vpath % $(MOD_DIRS)

SRCS := dynamics_lattice_module.F mpi_util.F mymalloc.F PWSSSP.F windfix.F \
        timer.F gmap.F alias.F fft.F set_eta.F90
OBJS := $(addsuffix .o, $(basename $(SRCS)))
DEPS := $(addsuffix .d, $(basename $(SRCS))) \
        $(addsuffix .d, $(basename $(BINS))) \
        $(addsuffix .d, $(basename $(BINSR8)))

SRCZ := dynamics_lattice_module.F mpi_util.F mymalloc.F timer.F
OBJZ := $(addsuffix _nompi.o, $(basename $(SRCZ)))

THIS_GFIO  = GMAO_gfio_r4
THIS_CFIO  = MAPL_cfio_r4

# Overwriting BIG_ENDIAN to Native
# --------------------------------
#BIG_ENDIAN =

USER_FFLAGS = $(BYTERECLEN) $(DGEOS5)
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
USER_FINCS  = $(foreach dir,$(MOD_DIRS),$(I)$(dir)) 

#---------------------
FREAL = $(FREAL4)

$(LIB) lib : $(OBJS)
	$(RM) $(LIB)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS)
	$(RANLIB) $(RANLIB_FLAGS) $(LIB)

$(LIBZ) : $(OBJZ)
	$(RM) $(LIBZ)
	$(AR) $(AR_FLAGS) $(LIBZ) $(OBJZ)
	$(RANLIB) $(RANLIB_FLAGS) $(LIBZ)

%.o : %.F
	$(FC) -c $(FFLAGS) $(D)mpi $*.F

dynamics_lattice_module_nompi.o: dynamics_lattice_module.o

mpi_util.o: dynamics_lattice_module_nompi.o
mpi_util_nompi.o: dynamics_lattice_module_nompi.o

eta2rst.o: dynamics_lattice_module_nompi.o
hdf2rs.o: dynamics_lattice_module_nompi.o
makeiau.o: dynamics_lattice_module_nompi.o
time_ave.o: dynamics_lattice_module_nompi.o
windfix.o: dynamics_lattice_module_nompi.o

stats.o : stats.F90
	$(FC) -c $(FFLAGS) $(BIG_ENDIAN) $*.F90

%_nompi.o : %.o
	$(FC) -c $(FFLAGS) -o $@ $*.F

# Generic definition of dependent libraries
# -----------------------------------------
LIBS = -Wl,--start-group $(LIB_MAPL_BASE) $(LIB_MAPL_BASE_STUBS) -Wl,--end-group \
		 $(LIB_GEOS_SHARED) \
       $(LIB_CFIO) $(LIB_GFIO) $(LIB_MPEU) $(LIB_MFHDF3) \
       $(LIB_ESMF) $(LIB_SDF) $(LIB_MPI) $(LIB_SYS)

# Generic rule for mpi builds
# ---------------------------
%.x : %.o $(LIB)
	$(LD) $(LDFLAGS) -o $@ $+ $(LIBS)

convert_eta.x: convert_eta.o $(LIB)
	$(LD) $(LDFLAGS) -o $@ $+ $(LIBS) $(LIB_HERMES)

eta2prs.x: eta2prs.o timer_nompi.o $(LIB)
	$(LD) $(LDFLAGS) -o $@ $+ $(LIBS)

stats.x: stats.o timer_nompi.o $(LIB)
	$(LD) $(LDFLAGS) -o $@ $+ $(LIBS)

flat2hdf.x: flat2hdf_nompi.o timer_nompi.o $(LIB)
	$(LD) $(LDFLAGS) -o $@ $+ $(LIBS)

gg2fv.x: gg2fv_nompi.o $(LIB)
	$(LD) $(LDFLAGS) -o $@ $+ $(LIBS)

rs2hdf.x: rs2hdf_nompi.o $(LIB)
	$(LD) $(LDFLAGS) -o $@ $+ $(LIBS)

time_ave.x: time_ave.o $(LIB)
	$(LD) $(LDFLAGS) -o $@ $+ $(LIBS)

time_ave_nompi.x: time_ave_nompi.o $(LIBZ)
	$(LD) $(LDFLAGS) -o $@ $+ $(LIBS)

PWSSSP.o: PWSSSP.F
	$(FC) -c $(FFLAGS) $(FREAL8) -I. PWSSSP.F

rsg3_vinterp.o: rsg3_vinterp.F
	$(FC) -c $(FFLAGS) $(FREAL8) -I. rsg3_vinterp.F

rsg3_vinterp.x: rsg3_vinterp.o $(LIB)
	$(LD) $(LDFLAGS) -o $@ $+ $(LIBS) $(LIB_HERMES)

rs_vinterp.o: rs_vinterp.F90
	$(FC) -c $(FFLAGS) $(FREAL8) -I. rs_vinterp.F90

rs_vinterp.x: rs_vinterp.o $(LIB)
	$(LD) $(LDFLAGS) -o $@ $+ $(LIBS) $(LIB_HERMES)

swapendian_%.x : swapendian_%.o 
	$(LD) $(LDFLAGS) -o $@ $(EXTENDED_SOURCE) $<

rs_vinterp_scm.o: rs_vinterp_scm.F90
	$(FC) -c $(FFLAGS) $(FREAL8) -I. rs_vinterp_scm.F90

rs_vinterp_scm.x: rs_vinterp_scm.o $(LIB)
	$(LD) $(LDFLAGS) -o $@ $+ $(LIBS) $(LIB_HERMES)

# convert_aerosols needs to be r8
# -------------------------------

convert_aerosols.x: THIS_GFIO  = GMAO_gfio_r8
convert_aerosols.x: THIS_CFIO  = MAPL_cfio_r8

set_eta_r8.o : set_eta.o
	$(FC) -c $(F90FLAGS) $(FREAL8) -o $@ set_eta.F90

convert_aerosols_r8.o : set_eta_r8.o
	$(FC) -c $(FFLAGS) $(FREAL8) -o $@ convert_aerosols.F

convert_aerosols.x: convert_aerosols_r8.o set_eta_r8.o
	$(LD) $(LDFLAGS) -o $@ $+ $(LIB_CFIO) $(LIB_GFIO) $(LIB_MPEU) \
		                       $(LIB_SDF) $(LIB_SYS)

#----------------------------------------------------
# Hack to prevent remaking dep files during cleaning
#----------------------------------------------------
ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
   -include $(DEPS)
endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
