#------------------------------------------------------------------------------
#                  Harvard-NASA Emissions Component (HEMCO)                   !
#------------------------------------------------------------------------------
#BOP
#
# !MODULE: Makefile (in the HEMCO/doc subdirectory)
#
# !DESCRIPTION: Makefile for building the HEMCO documentation from comments in 
#  the source-code subroutine headers.  A PDF file will be generated.
#\\
#\\
# !REMARKS:
# To build the documentation, call "make" with the following syntax:
#
#   make -jN TARGET [ OPTIONAL-FLAGS ]
#
# To display a complete list of options, type "make help".
#
# You must have the LaTeX utilities (latex, dvips, dvipdf) installed
# on your system in order to build the documentation.
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
ROOT  := ..
EXT   :=$(ROOT)/ext
SRC   :=$(ROOT)/src
HCO   :=$(SRC)/Core
HCOI  :=$(SRC)/Interfaces
HCOX  :=$(SRC)/Extensions
RUN   :=$(ROOT)/run

# Include header file, which returns various Makefile variables, defines 
# the compilation rules, and sets the correct C-preprocessor switches.
include $(ROOT)/Makefile_header.mk

###############################################################################
###                                                                         ###
###  Makefile targets: type "make help" for a complete listing!             ###
###                                                                         ###
###############################################################################

.PHONY: all doc docclean help

all: doc

clean:
	rm -f *.tex *.ps *.pdf *.aux *.dvi *.log *.toc

doc: hemcodoc

docclean: clean

###############################################################################
###                                                                         ###
###  Build the HEMCO documentation                                          ###
###                                                                         ###
###############################################################################

# List of source code files (the order is important!)
SRC :=                             \
./intro.hemco                      \
$(EXT)/charpak_mod.F	           \
$(HCO)/hco_arr_mod.F90	           \
$(HCO)/hco_calc_mod.F90	           \
$(HCO)/hco_chartools_mod.F90       \
$(HCO)/hco_clock_mod.F90           \
$(HCO)/hco_config_mod.F90          \
$(HCO)/hco_datacont_mod.F90        \
$(HCO)/hco_diagn_mod.F90           \
$(HCO)/hco_driver_mod.F90          \
$(HCO)/hco_emislist_mod.F90        \
$(HCO)/hco_error_mod.F90           \
$(HCO)/hco_extlist_mod.F90         \
$(HCO)/hco_filedata_mod.F90        \
$(HCO)/hco_fluxarr_mod.F90         \
$(HCO)/hco_geotools_mod.F90        \
$(HCO)/hco_logfile_mod.F90         \
$(HCO)/hco_readlist_mod.F90        \
$(HCO)/hco_state_mod.F90           \
$(HCO)/hco_tidx_mod.F90	           \
$(HCO)/hco_unit_mod.F90	           \
$(HCO)/hcoio_dataread_mod.F90      \
$(HCO)/hcoio_messy_mod.F90         \
$(HCO)/hcoio_diagn_mod.F90         \
$(EXT)/inquireMod.F90	           \
$(EXT)/julday_mod.F                \
$(EXT)/regrid_a2a_mod.F90          \
$(HCOX)/EF_MGN20_mod.F		   \
$(HCOX)/drydep_toolbox_mod.F90	   \
$(HCOX)/hcox_custom_mod.F90	   \
$(HCOX)/hcox_driver_mod.F90	   \
$(HCOX)/hcox_dustdead_mod.F	   \
$(HCOX)/hcox_dustginoux_mod.F90	   \
$(HCOX)/hcox_finn_mod.F90	   \
$(HCOX)/hcox_gc_RnPbBe_mod.F90	   \
$(HCOX)/hcox_gfed3_mod.F90	   \
$(HCOX)/hcox_lightnox_mod.F90	   \
$(HCOX)/hcox_megan_mod.F	   \
$(HCOX)/hcox_paranox_mod.F90	   \
$(HCOX)/hcox_seaflux_mod.F90	   \
$(HCOX)/hcox_seasalt_mod.F90	   \
$(HCOX)/hcox_soilnox_mod.F90	   \
$(HCOX)/hcox_state_mod.F90	   \
$(HCOX)/ocean_toolbox_mod.F90      \
$(HCOX)/paranox_util_mod.F         \
$(HCOI)/hcoi_standalone_mod.F90    \
$(RUN)/hemco_standalone.F90

# Output file names
TEX := HEMCO_Reference.tex
DVI := HEMCO_Reference.dvi
PDF := HEMCO_Reference.pdf

# Make commands
hemcodoc: 
	rm -f $(TEX)
	protex -sf $(SRC) > $(TEX)
	latex $(TEX)
	latex $(TEX)
	latex $(TEX)
	dvipdf $(DVI) $(PDF)
	rm -f *.aux *.dvi *.log *.toc

#EOC


#EOC
