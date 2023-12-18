#------------------------------------------------------------------------------
#                  Harvard-NASA Emissions Component (HEMCO)                   !
#------------------------------------------------------------------------------
#BOP
#
# !MODULE: Makefile (in the HEMCO/run subdirectory)
#
# !DESCRIPTION: Makefile for building an executable file for the standalone
#  HEMCO simulation.  It also creates a customized version of the HEMCO
#  Configuration file (HEMCO_Config.rc) and runs a short test simulation.
#\\
#\\
# !REMARKS:
# To build the programs, call "make" with the following syntax:
#
#   make -jN TARGET [ OPTIONAL-FLAGS ]
#
# To display a complete list of options, type "make help".
#
# !REVISION HISTORY: 
#  16 Jul 2014 - R. Yantosca - Initial version
#EOP
#------------------------------------------------------------------------------
#BOC

###############################################################################
###                                                                         ###
###  Initialization section                                                 ###
###                                                                         ###
###############################################################################

# Directories
ROOT      :=..
BIN       :=$(ROOT)/bin
DOC       :=$(ROOT)/doc
LIB       :=$(ROOT)/lib
MOD       :=$(ROOT)/mod
SRC       :=$(ROOT)/src
RUN       :=$(SRC)/Interfaces
XPL       :=./example1

# Include header file.  This returns CC, F90, FREEFORM, LD, R8, SHELL,
# as well as the default Makefile compilation rules for source code files.
include $(ROOT)/Makefile_header.mk

# Executable file
OBJx      :=hemco_standalone.o
EXE       :=$(OBJx:.o=.x)

###############################################################################
###                                                                         ###
###  Makefile targets: type "make help" for a complete list!                ###
###                                                                         ###
###############################################################################

.PHONY: all check clean debug

all: check

check:
	@$(MAKE) run

clean:
	rm -f *.o *.mod *.a *.x *.nc *.log 
	rm -f $(XPL)/*.x
	rm -f $(XPL)/*.nc
	rm -f $(XPL)/output/*

run:
	cp -f $(RUN)/*.x ${XPL}/
	cd ${XPL}; ./$(EXE) "HEMCO_sa_Config.rc"

debug:
	@echo "Targets : $(MAKECMDGOALS)"
	@echo "ROOT    : $(ROOT)"
	@echo "XPL     : $(XPL)"
	@echo "EXE     : $(EXE)"
	@echo "RUN     : $(RUN)"
	@echo "F90     : $(F90)"
#EOC



