#------------------------------------------------------------------------------
#                  Harvard-NASA Emissions Component (HEMCO)                   !
#------------------------------------------------------------------------------
#BOP
#
# !MODULE: Makefile (in the help subdirectory)
#
# !DESCRIPTION: Displays the HEMCO help screen.
#\\
#\\
# !REMARKS:
# To build the programs, call "make" with the following syntax:
#                                                                             .
#   make -jN TARGET [ OPTIONAL-FLAGS ]
#                                                                             .
# To display a complete list of options, type "make help".
#
# !REVISION HISTORY: 
#  17 Jul 2014 - R. Yantosca - Initial version
#EOP
#------------------------------------------------------------------------------
#BOC

###############################################################################
###                                                                         ###
###  Makefile targets: type "make help" for a complete listing!             ###
###                                                                         ###
###############################################################################

.PHONY: all help

all: help

help:
	@echo '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
	@echo '%%%                  HEMCO Help Screen                  %%%'
	@echo '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
	@echo
	@echo 'Usage: make -jN TARGET [ OPTIONAL-FLAGS ]'
	@echo ''
	@echo '-jN           Compiles N files at a time (to reduce compilation time)'
	@echo ''
	@echo 'TARGET may be one of the following:'
	@echo 'all           Default target (synonym for "lib exe")'
	@echo 'lib           Builds HEMCO source code and creates libraries'
	@echo 'check         Builds the standalone HEMCO executable and runs a short test simulation'
	@echo 'exe           Synonym for "check"'
	@echo 'test          Synonym for "check"'
	@echo 'debug         Displays the values of makefile variables'
	@echo 'clean         Removes *.o, *.mod files in source code subdirs only'
	@echo 'realclean     Removes all *.o, *mod, *.lib *.a, *.tex, *ps, *pdf files everywhere'
	@echo 'distclean     Synonym for "realclean"'
	@echo 'doc           Builds GEOS-Chem documentation (*.ps, *.pdf) in doc/'	
	@echo 'docclean      Removes all *.tex, *.pdf, *,ps files from doc/ subdirectory'
	@echo 'help          Displays this help screen'
	@echo ''
	@echo 'OPTIONAL-FLAGS may be:'
	@echo 'COMPILER=___  Specifies the compiler'
	@echo '              --> Options: ifort pgi (default is ifort)'
	@echo 'DEBUG=yes     Builds HEMCO for use w/ a debugger (with -g -O0)'
	@echo 'BOUNDS=yes    Turns on subscript-array checking (for debugging)'
	@echo 'FPE=yes       Turns on floating-point error checking (ifort only)'
	@echo 'OMP=[yes|no]  Turns OpenMP parallelization on/off (default is yes)'
