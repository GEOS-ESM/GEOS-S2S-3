#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 17Oct2008  da Silva  Changed from distributed to centralized makefile.
#
#-------------------------------------------------------------------------

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(PWD)/../../../../../..
endif

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
  include $(ESMADIR)/Config/GMAO_base.mk  # GMAO stuff

#                  ---------------------
#                  Standard ESMA Targets
#                  ---------------------

THIS := $(shell basename `pwd`)
NAME := $(patsubst %_GridComp,%,$(THIS))
LIB_THIS := lib$(THIS).a

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

esma_install install: $(ACGS) $(LIB_THIS) 
	$(MKDIR) $(ESMALIB) $(ESMAETC) $(ESMAINC)/$(THIS)
	$(CP) -p $(LIB_THIS) $(ESMALIB)
	$(CP) -p *.mod       $(ESMAINC)/$(THIS)
	$(CP) -p *.rc        $(ESMAETC)

esma_clean clean:
	$(RM) $(OBJS) *~ *.[aox] *.[Mm][Oo][Dd] *___.rc *___.h

esma_distclean distclean:
	$(RM) $(ACGS) $(OBJS) *~ *.[aoxd] *.[Mm][Oo][Dd] *___.rc *___.h

esma_doc doc:
	@$(PROTEX) $(PROTEX_FLAGS) *GridComp*.[fF]* > $(ESMADOC)/$(THIS).tex


#                  --------------------
#                  User Defined Targets
#                  --------------------

SRC_DIRS := .                        \
            ./HEMCO/ext              \
            ./HEMCO/src/Core         \
            ./HEMCO/src/Extensions   \
            ./HEMCO/src/Interfaces

THIS_CFIO = MAPL_cfio_r4

INC_DIRS = . $(INC_ESMF) $(INC_GMAO_SHARED) $(INC_MPI)
MOD_DIRS = . $(INC_DIRS)

SRCS := $(foreach dir,$(SRC_DIRS), \
        $(wildcard $(dir)/*.[fFc]) $(wildcard $(dir)/*.[fF]90) )

###SRCS := $(filter-out ./HEMCO/ext/regrid_a2a_mod.F90 \
###                     , $(SRCS))

OBJS := $(notdir $(addsuffix .o, $(basename $(SRCS))))
ACGS := $(NAME)_ExportSpec___.h $(NAME)_GetPointer___.h \
        $(NAME)_DeclarePointer___.h $(NAME)_History___.rc
DEPS := $(notdir $(addsuffix .d, $(basename $(SRCS))))

FREAL = $(FREAL4)

# MAT These are Intel Fortran specific
#USER_FDEFS  = $(D)ESMF_ $(D)DEVEL $(D)LINUX_IFORT $(D)GEOS_FP
#USER_CDEFS  = $(I)ESMF_ $(I)DEVEL
#USER_FFLAGS = $(D)ESMF_ $(BIG_ENDIAN) -override-limits -CU -mkl -traceback

USER_FDEFS  = $(D)ESMF_ $(D)DEVEL $(D)GEOS_FP
USER_CDEFS  = $(I)ESMF_ $(I)DEVEL
USER_FFLAGS = $(D)ESMF_ $(BIG_ENDIAN)

USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir)) 
USER_FINCS  = $(foreach dir,$(INC_DIRS),$(I)$(dir)) 

ACG_FLAGS += -F

vpath % $(SRC_DIRS) $(INC_DIRS) $(MOD_DIRS)

$(LIB_THIS) lib : $(ACGS) $(DEPS) $(OBJS)
	$(RM) $(LIB_THIS)
	$(AR) $(AR_FLAGS) $(LIB_THIS) $(OBJS) 
	$(RANLIB) $(RANLIB_FLAGS) $(LIB_THIS) 


$(ACGS) : $(NAME)_Registry.rc $(ACG)
	@$(ACG) $(ACG_FLAGS) $(NAME)_Registry.rc

%.x : $(LIB) %.o
	@echo "$(LIB_MPI)"
	$(LD) $(LDFLAGS) -o $@ $*.o $(LIB_THIS) \
	$(LIB_PILGRIM)\
	$(LIB_MAPL_BASE) $(LIB_CFIO) $(LIB_MPEU) \
	$(LIB_ESMF) $(LIB_SDF) \
	$(LIB_SYS) $(LIB_MPI)

# Reduce the optimization for (only)
# HEMCO_GridCompMod.F90 if it compiles too slowly.
#
# MAT: To allow debugging, only repeat the rule to not
#      break parallel build
# -------------------------------------------------------
ifneq ("$(BOPT)","g")
HEMCO_GridCompMod.o : HEMCO_GridCompMod.F90 hcoi_standalone_mod.o
	$(FC) -c $(patsubst $(FOPT),$(FOPT2),$(F90FLAGS)) $<
else
HEMCO_GridCompMod.o : HEMCO_GridCompMod.F90 hcoi_standalone_mod.o
endif

# Hack to prevent remaking dep files during cleaning
# --------------------------------------------------
  ifneq ($(findstring clean,$(MAKECMDGOALS)),clean)
    -include $(DEPS)
  endif

# For parallel install
# --------------------
  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
