#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 20oct2004  da Silva  First ESMA version.
#

# Make sure ESMADIR is defined
# ----------------------------
ifndef ESMADIR
       ESMADIR := $(PWD)/../..
endif

THIS := $(shell basename `pwd`)

# Compilation rules, flags, etc
# -----------------------------
  include $(ESMADIR)/Config/ESMA_base.mk  # Generic stuff
  include $(ESMADIR)/Config/ESMA_arch.mk  # System dependencies
  include $(ESMADIR)/Config/GMAO_base.mk
  include FMS_arch.mk


#                  ---------------------
#                  Standard ESMA Targets
#                  ---------------------

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
	@echo "         SITE = $(SITE) "
	@echo "         THIS = $(THIS) "


#                  --------------------------------
#                   Recurse Make in Sub-directories
#                  --------------------------------

SUBDIRS = r4 r8

TARGETS = esma_install esma_clean esma_doc \
          install clean doc

$(TARGETS):
	@ t=$@; argv="$(SUBDIRS)" ;\
	  for d in $$argv; do			 \
	    ( $(MKDIR) $$d            		;\
	      $(MAKE) -C $$d -f ../GNUmakefile_r4r8 -e $$t NTHIS=$(THIS) RN=$$d ) \
	  done
	$(MAKE) local_$@

local_esma_install local_install: $(LIB)
	@echo No local install in here

local_esma_clean local_clean:
	-$(RM) *~ *.[aox] *.mod *.pyf *.so

local_esma_doc local_doc:
	@echo No doc to build here

esma_distclean distclean:
	-$(RM) -r *~ *.[aoxd] *.mod *.pyf *.so $(SUBDIRS)

#                  --------------------
#                  User Defined Targets
#                  --------------------

r4_install: 
	$(MKDIR) r4
	echo Making install for r4 in $(PWD)
	$(MAKE) -C r4 -f ../GNUmakefile_r4r8 -e install NTHIS=$(THIS) RN=r4

r8_install:
	$(MKDIR) r8
	echo Making install for r8 in $(PWD)
	$(MAKE) -C r8 -f ../GNUmakefile_r4r8 -e install NTHIS=$(THIS) RN=r8

dist:
	$(MAKE) distclean
	( cd ..  ;\
          $(TAR)  cvf evac-`date '+%d%b%Y'`.tar src ;\
          $(GZIP) evac-`date '+%d%b%Y'`.tar )

#...............................................................

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
