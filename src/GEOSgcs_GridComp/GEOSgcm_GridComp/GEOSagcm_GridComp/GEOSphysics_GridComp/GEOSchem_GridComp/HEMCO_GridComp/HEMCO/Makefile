#------------------------------------------------------------------------------
#                  Harvard-NASA Emissions Component (HEMCO)                   !
#------------------------------------------------------------------------------
#BOP
#
# !MODULE: Makefile (in the HEMCO main directory)
#
# !DESCRIPTION: This is the top-level makefile for HEMCO.  It calls Makefiles 
#  in the various subdirectories to build the HEMCO code and documentation.
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
ROOT :=.
DOC  :=$(ROOT)/doc
HELP :=$(ROOT)/help
SRC  :=$(ROOT)/src
EXT  :=$(ROOT)/ext
RUN  :=$(ROOT)/examples
LIB  :=$(ROOT)/lib
MOD  :=$(ROOT)/mod
BIN  :=$(ROOT)/bin

###############################################################################
###                                                                         ###
###  Makefile targets: type "make help" for a complete list!                ###
###                                                                         ###
###############################################################################

.PHONY: all      check clean debug distclean doc 
.PHONY: docclean exe   help  lib   realclean test

all:
	@$(MAKE) -C $(EXT) all
	@$(MAKE) -C $(SRC) all
	@$(MAKE) exe

check:  
	@$(MAKE) -C $(RUN) all

clean:  
	@$(MAKE) -C $(EXT) clean 
	@$(MAKE) -C $(SRC) clean
	@$(MAKE) -C $(RUN) clean

debug:
	@$(MAKE) -C $(EXT) debug
	@$(MAKE) -C $(SRC) debug

distclean:
	@$(MAKE) realclean

doc:
	@$(MAKE) -C $(DOC) all

docclean:
	@$(MAKE) -C $(DOC) clean

exe: check

help:
	@$(MAKE) -C $(HELP) help

lib:
	@$(MAKE) -C $(EXT) lib
	@$(MAKE) -C $(SRC) lib

realclean:
	@$(MAKE) clean
	@$(MAKE) docclean
	rm -f $(LIB)/*.a
	rm -f $(MOD)/*.mod
	rm -f $(BIN)/*.x

test: check

#EOC
