#
# recursive makefile for ESMA.
#

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(PWD)/../..
endif


# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies

#                  ---------------------
#                  Standard ESMA Targets
#                  ---------------------

esma_help :
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


#                  --------------------------------
#                   Recurse Make in Sub-directories
#                  --------------------------------

ALLDIRS = LANL_cice

SUBDIRS = $(wildcard $(ALLDIRS) )

TARGETS = esma_install esma_clean esma_distclean esma_doc \
          help install clean doc

export ESMADIR BASEDIR ARCH SITE

$(TARGETS): 
	@ t=$@; argv="$(SUBDIRS)" ;\
	  for d in $$argv; do			 \
	    ( cd $$d				;\
	      echo ""; echo Making $$t in `pwd`          ;\
	      $(MAKE) -e $$t ) \
	  done

local_esma_install local_install: $(LIB)
	@echo No local install in here

#                  --------------------
#                  User Defined Targets
#                  --------------------

distclean:
	-$(RM) *~ *.[aoxd] *.[Mm][Oo][Dd]
	$(MAKE) esma_distclean

#              ------------------------------------------
#              Package Dependencies for Parallel Install
#              ------------------------------------------

NCEP_irsse_install : NCEP_crtm_install

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
