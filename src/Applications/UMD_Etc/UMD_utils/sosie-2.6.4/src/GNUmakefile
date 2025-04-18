#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 3mar2004  Zaslavsky  Initial imlementation.
# 20Oct2004  da Silva  Standardization
#

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(PWD)/../../../../../..
endif

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
  include $(ESMADIR)/Config/GMAO_base.mk  # System dependencies

#                  ---------------------
#                  Standard ESMA Targets
#                  ---------------------


THIS := $(shell basename `pwd`)
LIB  = lib$(THIS).a

BIN_SRCS = sosie.f90 corr_vect.f90 interp_to_line.f90 mask_drown_field.f90
BIN      = sosie.x corr_vect.x interp_to_line.x mask_drown_field.x

esma_install install: $(LIB) $(BIN) 
	$(MKDIR) $(ESMALIB) $(ESMAETC) $(ESMAINC)/$(THIS) $(ESMABIN)
	$(CP) -p $(BIN)      $(ESMABIN)

esma_clean clean:
	$(RM) *~ *.[aox] *.[Mm][Oo][Dd]

esma_distclean distclean:
	$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd]

esma_doc doc:
	@echo "Target $@ not implemented yet in `pwd`"


esma_help help:
	@echo "Standard ESMA targets:"
	@echo "% make esma_install    (builds and install under ESMADIR)"
	@echo "% make esma_clean      (removes deliverables: *.[aox], etc)"
	@echo "% make esma_distclean  (leaves in the same state as cvs co)"
	@echo "% make esma_doc        (generates PDF, installs under ESMADIR)"
	@echo "% make esma_help       (this message)"
	@echo "Environment:"
	@echo "      ESMADIR = $(ESMADIR)"
	@echo "      BASEDIR = $(BASEDIR)"
	@echo "         ARCH = $(ARCH)"
	@echo "         SITE = $(SITE)"
	@echo "        FREAL = $(FREAL)"

#                  --------------------
#                  User Defined Targets
#                  --------------------

SRCS := io_ezcdf.f90 mod_akima_1d.f90  mod_bicub_2d.f90 mod_conf.f90 \
        mod_grids.f90  mod_interp.f90  mod_akima_2d.f90  mod_bilin_2d.f90 \
        mod_drown.f90  mod_init.f90   mod_manip.f90

OBJS := $(addsuffix .o, $(basename $(SRCS)))

ALL_SRCS = $(SRCS) $(BIN_SRCS)
DEPS := $(addsuffix .d, $(basename $(ALL_SRCS)))

FOPT = $(FOPT3) -traceback
FREAL = $(FREAL4)



MOD_DIRS = . $(INC_NETCDF) 
USER_FDEFS = $(D)_LAPACK_ $(D)gmao_intf $(D)HAVE_ESMF
USER_FFLAGS = $(BIG_ENDIAN) $(BYTERECLEN)
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
USER_FINC   = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 

vpath % $(MOD_DIRS)

$(LIB) lib : $(DEPS) $(OBJS)
	$(RM) $(LIB)
	$(AR) $(AR_FLAGS) $(LIB) $(OBJS)

sosie.x:  $(LIB) sosie.o
	$(LD) $(LDFLAGS) -o $@ sosie.o $(LIB) \
              $(LIB_NETCDF) \
              $(LIB_MPI) $(LIB_SYS) $(LIB_SCI)

corr_vect.x: $(LIB) corr_vect.o
	$(LD) $(LDFLAGS) -o $@ corr_vect.o $(LIB) \
              $(LIB_NETCDF) \
              $(LIB_MPI) $(LIB_SYS) $(LIB_SCI)

mask_drown_field.x: $(LIB) mask_drown_field.o
	$(LD) $(LDFLAGS) -o $@ mask_drown_field.o $(LIB) \
              $(LIB_NETCDF) \
              $(LIB_MPI) $(LIB_SYS) $(LIB_SCI)


interp_to_line.x: $(LIB) interp_to_line.o
	$(LD) $(LDFLAGS) -o $@ interp_to_line.o $(LIB) \
              $(LIB_NETCDF) \
              $(LIB_MPI) $(LIB_SYS) $(LIB_SCI)

# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
