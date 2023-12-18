#------------------------------------------------------------------------------
#                  Harvard-NASA Emissions Component (HEMCO)                   !
#------------------------------------------------------------------------------
#BOP
#
# !IROUTINE: Makefile_header.mk
#
# !DESCRIPTION: This sub-makefile defines the variables which specify
# compilation options for the different supported compiler/platform
# combinations.  Also, the default makefile compilation rules are specified 
# here.
#\\
#\\
# !REMARKS:
# To build the programs, call "make" with the following syntax:
#
#   make -jN TARGET [ OPTIONAL-FLAGS ]
#
# To display a complete list of options, type "make help".
#
# The following variables are exported to the main-level Makefile:
#
# Variable   Description
# --------   -----------
# CC         Contains the default C compilation commands (for PGI only)
# F90        Contains the Fortran compilation commands
# FREEFORM   Contains the command to force F90 "free format" compilation
# LD         Contains the command to link to libraries & make executable
# LINK       Contains the commands to link to HEMCO built libraries
# R8         Contains the command to force REAL -> REAL*8
# SHELL      Contains the default Unix shell to use when building code
# NC_LINK    Contains the default netCDF library link commands
#
# FFLAGS is a local variable that is not returned to the "outside world", 
# but is only used locally.  COMPILER, HDF5, and OMP are all input via the
# command line or via environment variables.
#
# NOTE: We now use SHELL :=/bin/bash as the default Unix shell.  This allows
# us to extend the Makefile ifeq statements so that we can test for more than
# one string.  The following example is used to ensure that the met field name
# selected by the user is case-insensitive:
#
#   # %%%%% DEBUG %%%%%
#   REGEXP    :=(^[Yy]|^[Yy][Ee][Ss])
#   ifeq ($(shell [[ "$(DEBUG)" =~ $(REGEXP) ]] && echo true),true)
#   USER_DEFS += -DDEBUG
#   endif
#
# The [[ ]] in bash is an evaluation.  The above ifeq statement uses regular
# expressions to test if the DEBUG variable matches the string "Y" or "YES" 
# (case-insensitive.  This will return true (via the "echo true" statement) 
# for combinations Y, y, Yes, YEs, YES, yES, yeS, YES, thus making errors
# caused by user mistyping more unlikely.
#
# ALSO NOTE: The user must set the following environment variables in his/her
# .cshrc or .bashrc:
#
# (1) BIN_NETCDF : Points to the netCDF binary  dir (e.g. netcdf/bin    )
# (2) INC_NETCDF : Points to the netCDF include dir (e.g. netcdf/include)
# (3) LIB_NETCDF : Points to the netCDF library dir (e.g. netcdf/lib    )
#
# Ask your sysadmin where these directories may be found on your system.
#
# !REVISION HISTORY: 
#  16 Jul 2014 - R. Yantosca - Initial version, based on GEOS-Chem
#EOP
#------------------------------------------------------------------------------
#BOC

###############################################################################
###                                                                         ###
###  Set the default Unix shell and some error message variables            ###
###                                                                         ###
###############################################################################

# Set default shell to bash, for use with the Makefile conditionals
SHELL          :=/bin/bash

# Error message for bad COMPILER input
ERR_CMPLR      :="Select a compiler: COMPILER=ifort, COMPILER=pgi"

###############################################################################
###                                                                         ###
###  Set C-preprocessor switches representing user options.  These are not  ###
###  specific to any compiler, but are general options for the simulation.  ###
###                                                                         ###
###  NOTE: To make the user input more robust, we use regular expression    ###
###  syntax to match characters in the various Makefile variables.  See     ###
###  this web page for more info:                                           ###
###  http://www.tldp.org/LDP/abs/html/x17046.html                           ###
###                                                                         ###
###############################################################################

# %%%%% OpenMP parallelization default) %%%%%
ifndef OMP
OMP            :=yes
endif

# %%%%% Set default compiler %%%%%
ifndef COMPILER
COMPILER       :=ifort
endif

# %%%%% Test if IFORT compiler is selected %%%%%
REGEXP         :=(^[Ii][Ff][Oo][Rr][Tt])
ifeq ($(shell [[ "$(COMPILER)" =~ $(REGEXP) ]] && echo true),true)
COMPILER       :=ifort
REGEXP         :=(^[Yy][Ee][Ss])
ifeq ($(shell [[ "$(MPI)" =~ $(REGEXP) ]] && echo true),true)
COMPILE_CMD    :=mpiifort
else
COMPILE_CMD    :=ifort
endif
USER_DEFS      += -DLINUX_IFORT
endif

# %%%%% Test if PGI compiler is selected  %%%%%
REGEXP         :=(^[Pp][Gg][Ii])
ifeq ($(shell [[ "$(COMPILER)" =~ $(REGEXP) ]] && echo true),true)
COMPILER       :=pgi
COMPILE_CMD    :=pgf90
USER_DEFS      += -DLINUX_PGI
endif

# %%%%% ERROR CHECK!  Make sure our COMPILER selection is valid! %%%%%
REGEXP         :=((-DLINUX_)?IFORT|PGI)
ifneq ($(shell [[ "$(USER_DEFS)" =~ $(REGEXP) ]] && echo true),true)
$(error $(ERR_CMPLR))
endif

# %%%%% DEBUG %%%%%
REGEXP         :=(^[Yy]|^[Yy][Ee][Ss])
ifeq ($(shell [[ "$(DEBUG)" =~ $(REGEXP) ]] && echo true),true)
USER_DEFS      += -DDEBUG
endif

# %%%%% ESMF %%%%%
REGEXP    := (^[Yy]|^[Yy][Ee][Ss])
ifeq ($(shell [[ "$(ESMF)" =~ $(REGEXP) ]] && echo true),true)
USER_DEFS      += -DESMF_
NO_GRID_NEEDED :=1
endif

###############################################################################
###                                                                         ###
###  Set linker commands for local and external libraries (incl. netCDF)    ###
###                                                                         ###
###############################################################################

# Directories to include for building netCDF and NcdfUtilities
NC_INC         := -I$(NCU_MOD) -I$(INC_NETCDF) -$(INC_HDF5)

# Library link path: first try to get the list of proper linking flags
# for this build of netCDF with nf-config and nc-config. 
NC_LINK        := $(shell $(BIN_NETCDF)/nf-config --flibs)
NC_LINK        += $(shell $(BIN_NETCDF)/nc-config --libs)
NC_LINK        := $(filter -l%,$(NC_LINK))

#------------------------------------------------------------------------------
# NOTE TO USERS: If you do not have netCDF-4.2 installed, then you can 
# add/modify the linking sequence here.  (This sequence is a guess, but is 
# probably good enough for other netCDF builds.)
ifeq ($(NC_LINK),) 
NC_LINK         :=-lnetcdf -lhdf5_hl -lhdf5 -lz
endif
#------------------------------------------------------------------------------

# Prepend the library directory path to the linking sequence
NC_LINK        :=-L$(LIB_NETCDF) -L$(LIB_HDF5) $(NC_LINK)

# Command to link to all library files
LINK_HCO       :=-L$(LIB) -lHCOI -lHCOX -lHCO -lEXT -L$(NCU_LIB) -lNcUtils $(NC_LINK)

###############################################################################
###                                                                         ###
###  IFORT compilation options.  This is the default compiler.              ###
###                                                                         ###
###############################################################################

ifeq ($(COMPILER),ifort) 

# Default optimization level for all routines (-O2)
ifndef OPT
OPT            := -O2
endif

# Pick compiler options for debug run or regular run 
REGEXP         := (^[Yy]|^[Yy][Ee][Ss])
ifeq ($(shell [[ "$(DEBUG)" =~ $(REGEXP) ]] && echo true),true)
FFLAGS         :=-cpp -w -O0 -auto -noalign
FFLAGS         += -g -check arg_temp_created -debug all
TRACEBACK      := yes
USER_DEFS      += -DDEBUG
else
FFLAGS         :=-cpp -w $(OPT) -auto -noalign
FFLAGS         += -vec-report0
endif

# Prevent any optimizations that would change numerical results
FFLAGS         += -fp-model source

# Turn on OpenMP parallelization
REGEXP         :=(^[Yy]|^[Yy][Ee][Ss])
ifeq ($(shell [[ "$(OMP)" =~ $(REGEXP) ]] && echo true),true)
FFLAGS         += -openmp
endif

# Get Operating System (Linux = Linux; Darwin = MacOSX)
ifndef UNAME
UNAME          :=$(shell uname)
endif

# OSX compilation options
ifeq ($(UNAME),Darwin)
FFLAGS         += -Wl,-stack_size,0x2cb410000 # Allow 12GB of stack space
ifdef DEBUG
FFLAGS         += -g0 -debug -save-temps -fpic -Wl,-no_pie
endif
endif

# Add options for medium memory model.  This is to prevent G-C from 
# running out of memory at hi-res, especially when using netCDF I/O.
ifneq ($(UNAME),Darwin)
FFLAGS         += -mcmodel=medium -shared-intel
endif

# Turn on checking for floating-point exceptions
REGEXP         :=(^[Yy]|^[Yy][Ee][Ss])
ifeq ($(shell [[ "$(FPE)" =~ $(REGEXP) ]] && echo true),true)
FFLAGS         += -fpe0 -ftrapuv
endif
ifeq ($(shell [[ "$(FPEX)" =~ $(REGEXP) ]] && echo true),true)
FFLAGS         += -fpe0 -ftrapuv
endif

# Add special IFORT optimization commands
REGEXP         :=(^[Yy]|^[Yy][Ee][Ss])
ifeq ($(shell [[ "$(IPO)" =~ $(REGEXP) ]] && echo true),true)
FFLAGS         += -ipo -static
endif

# Add option for "array out of bounds" checking
REGEXP    := (^[Yy]|^[Yy][Ee][Ss])
ifeq ($(shell [[ "$(BOUNDS)" =~ $(REGEXP) ]] && echo true),true)
FFLAGS         += -check bounds
endif

# Also add traceback option
REGEXP         :=(^[Yy]|^[Yy][Ee][Ss])
ifeq ($(shell [[ "$(TRACEBACK)" =~ $(REGEXP) ]] && echo true),true)
FFLAGS         += -traceback
endif

# Append the user options in USER_DEFS to FFLAGS
FFLAGS         += $(USER_DEFS)

# Add any additional flags
FFLAGS         += $(EXTRA_FLAGS)

# Include options (i.e. for finding *.h, *.mod files)
INCLUDE        := -module $(MOD) $(NC_INC)

# Set the standard compiler variables
CC             :=
F90            :=$(COMPILE_CMD) $(FFLAGS) $(INCLUDE)
LD             :=$(COMPILE_CMD) $(FFLAGS)
FREEFORM       := -free
R8             := -r8

endif

###############################################################################
###                                                                         ###
###  Portland Group (PGF90) compilation options                             ###
###                                                                         ###
###############################################################################

ifeq ($(COMPILER),pgi) 

# Default optimization level for all routines (-fast)
ifndef OPT
OPT            :=-fast
endif

# Pick compiler options for debug run or regular run 
REGEXP         := (^[Yy]|^[Yy][Ee][Ss])
ifeq ($(shell [[ "$(DEBUG)" =~ $(REGEXP) ]] && echo true),true)
FFLAGS         :=-byteswapio -Mpreprocess -Bstatic -g -O0 
USER_DEFS      += -DDEBUG
else
FFLAGS         :=-byteswapio -Mpreprocess -Bstatic $(OPT)
endif

# Add options for medium memory model.  This is to prevent G-C from 
# running out of memory at hi-res, especially when using netCDF I/O.
FFLAGS         += -mcmodel=medium

# Turn on OpenMP parallelization
REGEXP         :=(^[Yy]|^[Yy][Ee][Ss])
ifeq ($(shell [[ "$(OMP)" =~ $(REGEXP) ]] && echo true),true)
FFLAGS         += -mp -Mnosgimp -Dmultitask
endif

# Add option for suppressing PGI non-uniform memory access (numa) library 
REGEXP         :=(^[Yy]|^[Yy][Ee][Ss])
ifeq ($(shell [[ "$(NONUMA)" =~ $(REGEXP) ]] && echo true),true)
FFLAGS         += -mp=nonuma
endif

# Add option for "array out of bounds" checking
REGEXP         :=(^[Yy]|^[Yy][Ee][Ss])
ifeq ($(shell [[ "$(BOUNDS)" =~ $(REGEXP) ]] && echo true),true)
FFLAGS         += -C
endif

# Also add traceback option
REGEXP         :=(^[Yy]|^[Yy][Ee][Ss])
ifeq ($(shell [[ "$(TRACEBACK)" =~ $(REGEXP) ]] && echo true),true)
FFLAGS         += -traceback
endif

# Append the user options in USER_DEFS to FFLAGS
FFLAGS         += $(USER_DEFS)

# Include options (i.e. for finding *.h, *.mod files)
INCLUDE        := -I$(HDR) -module $(MOD) $(NC_INC)

# Set the standard compiler variables
CC             :=gcc
F90            :=$(COMPILE_CMD) $(FFLAGS) $(INCLUDE)
LD             :=$(COMPILE_CMD) $(FFLAGS)
FREEFORM       := -Mfree
R8             := -Mextend -r8

endif

###############################################################################
###                                                                         ###
###  Specify pattern rules for compiliation                                 ###
###  (i.e. tell "make" how to compile files w/ different extensions)        ###
###                                                                         ###
###############################################################################

%.o : %.f
	$(F90) -c $<
%.o : %.F
	$(F90) -c $<
%.o : %.f90
	$(F90) -c $(FREEFORM) $<
%.o : %.F90
	$(F90) -c $(FREEFORM) $<
%.o : %.c
	$(CC) -c $*.c

###############################################################################
###                                                                         ###
###  Export global variables so that the main Makefile will see these       ###
###                                                                         ###
###############################################################################

export CC
export F90
export FREEFORM
export LD
export LINK_HCO
export R8
export SHELL
#EOC

###############################################################################
###                                                                         ###
###  Debug print output.  Normally you will leave the following lines       ###
###  commented out.  Uncomment these lines for testing.                     ###
###                                                                         ###
###############################################################################

#header_debug:
#	@@echo '####### in Makefile_header.mk ########' 
#	@@echo "compiler: $(COMPILER)"
#	@@echo "debug   : $(DEBUG)"
#	@@echo "bounds  : $(BOUNDS)"
#	@@echo "f90     : $(F90)"
#	@@echo "cc      : $(CC)"
#	@@echo "include : $(INCLUDE)"
#	@@echo "link    : $(LINK_HCO)"
#	@@echo "userdefs: $(USER_DEFS)"
