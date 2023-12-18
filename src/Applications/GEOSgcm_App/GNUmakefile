#
# Makefile for ESMA components.
#
# REVISION HISTORY:
#
# 09Jun2003  da Silva  First crack.
# 21Feb2005  da Silva  Implemented SDF macfros of NetCDF/HDF interoperability.
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
  include $(ESMADIR)/Config/GMAO_base.mk  # GMAO Generic stuff

  include $(ESMADIR)/Config/vectorize.mk  # Vectorize

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
	@echo "         SITE = $(SITE)"

THIS = GEOSgcm
BINS = $(THIS).x idfupd.x

GCMDOC := GCM_doc
TEXFILES := $(wildcard $(ESMADOC)/*GridComp.tex)

esma_install install: $(BINS)
	$(MKDIR) $(ESMABIN) $(ESMAETC) 
	$(CP) -p *.x    $(ESMABIN)
	$(CP) -p *.rc   $(ESMAETC)
	$(CP) -p *.tmpl $(ESMAETC)

esma_clean clean:
	-$(RM) *~ *.[aox] *.mod *.x $(GCMDOC).*


esma_distclean distclean:
	-$(RM) *~ *.[aoxd] *.mod *.x $(GCMDOC).*

esma_doc doc:
	@echo TEXFILES: $(TEXFILES)
	$(CAT) $(ESMADOC)/GCM_Preamble.tex > $(ESMADOC)/$(GCMDOC).tex
	$(foreach texfile,$(TEXFILES),$(CAT) $(texfile) >> $(ESMADOC)/$(GCMDOC).tex;)
	@echo "\\end{document}" >> $(ESMADOC)/$(GCMDOC).tex
	$(PDFLATEX) $(ESMADOC)/$(GCMDOC).tex
	$(PDFLATEX) $(ESMADOC)/$(GCMDOC).tex
	$(CP) $(GCMDOC).pdf $(ESMADOC)
	$(RM) $(TEXFILES)
	$(RM) $(ESMADOC)/GCM_Preamble.tex $(ESMADOC)/$(GCMDOC).tex
	$(RM) $(ESMADOC)/*.tex



#                  ---------------------------
#                  Availability Driven Options
#                  ---------------------------

LIB_GEOSCHEM = $(wildcard $(ESMALIB)/libGEOSchem_GridComp.a \
                          $(ESMALIB)/libStratChem_GridComp.a\
                          $(ESMALIB)/libSC_GridComp.a\
                          $(ESMALIB)/libGEOSpchem_GridComp.a\
                          $(ESMALIB)/libGEOSachem_GridComp.a\
                          $(ESMALIB)/libGMIchem_GridComp.a\
                          $(ESMALIB)/libGEOSCHEMchem_GridComp.a\
                          $(ESMALIB)/libGAAS_GridComp.a\
                          $(ESMALIB)/libGOCART_GridComp.a\
                          $(ESMALIB)/libCFC_GridComp.a\
                          $(ESMALIB)/libCARMA_GridComp.a\
                          $(ESMALIB)/libCARMAchem_GridComp.a\
                          $(ESMALIB)/libRn_GridComp.a\
                          $(ESMALIB)/libBC_GridComp.a\
                          $(ESMALIB)/libBRC_GridComp.a\
                          $(ESMALIB)/libCO2_GridComp.a\
                          $(ESMALIB)/libCO_GridComp.a\
                          $(ESMALIB)/libDU_GridComp.a\
                          $(ESMALIB)/libO3_GridComp.a\
                          $(ESMALIB)/libOC_GridComp.a\
                          $(ESMALIB)/libSS_GridComp.a\
                          $(ESMALIB)/libSU_GridComp.a\
                          $(ESMALIB)/libCH4_GridComp.a\
                          $(ESMALIB)/libNI_GridComp.a\
                          $(ESMALIB)/libMAMchem_GridComp.a\
                          $(ESMALIB)/libTR_GridComp.a\
                          $(ESMALIB)/libMATRIXchem_GridComp.a\
                          $(ESMALIB)/libDNA_GridComp.a\
                          $(ESMALIB)/libHEMCO_GridComp.a)

INC_GEOSCHEM = $(wildcard $(ESMAINC)/Chem_Base \
                          $(ESMAINC)/Chem_Shared \
                          $(ESMAINC)/GEOSchem_GridComp \
                          $(ESMAINC)/GMIchem_GridComp \
                          $(ESMAINC)/GEOSpchem_GridComp )

ifeq ( $(wildcard $(ESMALIB)/libmom.a), $(null) )
ifeq ($(FV_PRECISION), R4)
   LIB_FMS = $(ESMALIB)/libGFDL_fms_r4.a
else
   LIB_FMS = $(ESMALIB)/libGFDL_fms_r8.a
endif
else
   LIB_FMS = $(ESMALIB)/libGFDL_fms_r8.a
endif

LIB_OCEAN = $(wildcard $(ESMALIB)/libGEOSdatasea_GridComp.a \
                       $(ESMALIB)/libPoseidon_GridComp.a \
                       $(ESMALIB)/libGuestOcean_GridComp.a \
                       $(ESMALIB)/libMOM_GEOS5PlugMod.a \
                       $(ESMALIB)/libmom.a \
                       $(ESMALIB)/libmit.a \
                       $(ESMALIB)/libGEOSoceanbiogeochem_GridComp.a \
                       $(ESMALIB)/libGEOSoceanbiosimple_GridComp.a \
                       $(ESMALIB)/libGEOSdataseaice_GridComp.a \
                       $(ESMALIB)/libGEOSCICEDyna_GridComp.a \
                       $(ESMALIB)/libGEOSorad_GridComp.a \
                       $(ESMALIB)/libGEOSoradbio_GridComp.a )

THIS_SP     = NCEP_sp_r4i4
THIS_W3     = NCEP_w3_r4i4
THIS_BACIO  = NCEP_bacio_r4i4
THIS_BUFR   = NCEP_bufr_r8i4
THIS_NEMSIO = NCEP_nemsio

LIB_GFSIO   = $(ESMALIB)/libNCEP_gfsio.a
ifeq ( $(wildcard $(LIB_CRTM)), $(null) )
   LIB_ANASUPP =
else
   LIB_ANASUPP = $(LIB_GFSIO) $(LIB_SIGIO) $(LIB_CRTM) $(LIB_IRSSE) \
                 $(LIB_NEMSIO) $(LIB_BACIO) $(LIB_BUFR) $(LIB_W3) \
                 $(LIB_SP) $(LIB_SFCIO) $(LIB_TRANSF)  
   USER_LDFLAGS = $(OMPFLAG)
   ifdef I_MPI_ROOT
      LIB_MPI := $(LIB_MPI_OMP)
   endif
endif

LIB_SHARED = $(wildcard \
             $(LIB_ANASUPP) \
             $(LIB_SP) \
             $(ESMALIB)/libChem_Shared.a \
             $(ESMALIB)/libChem_Base.a \
             $(ESMALIB)/libGEOS_Shared.a \
             $(ESMALIB)/libMAPL_Base.a \
             $(ESMALIB)/libMAPL_cfio_r4.a \
             $(ESMALIB)/libGMAO_gfio_r4.a \
             $(ESMALIB)/libGMAO_ods.a  \
             $(ESMALIB)/libGMAO_gems.a  \
             $(ESMALIB)/libGMAO_hermes.a \
             $(ESMALIB)/libGMAO_mpeu.a  )

LIB_AANA = $(wildcard $(ESMALIB)/libGEOSaana_GridComp.a \
                      $(ESMALIB)/libGEOSgsi_Coupler.a \
                      $(ESMALIB)/libGEOSgsi_Util.a \
                      $(ESMALIB)/libGSI_GridComp.a \
                      $(ESMALIB)/libGSI_Other.a \
                      $(ESMALIB)/libGSI_Solver.a \
                      $(ESMALIB)/libGSI_Observer.a \
                      $(ESMALIB)/libGSI_Other.a \
                      $(ESMALIB)/libGSI_Util.a )

LIB_ANA = $(wildcard $(ESMALIB)/libGEOSana_GridComp.a \
                     $(ESMALIB)/libGMAOodas_GridComp.a \
                     $(LIB_AANA) )

LIB_DYN = $(wildcard $(ESMALIB)/libGEOSsingcol_GridComp.a\
                     $(ESMALIB)/libGEOSsuperdyn_GridComp.a\
                     $(ESMALIB)/libGEOStopo_GridComp.a\
                     $(ESMALIB)/libFVdycore_GridComp.a \
                     $(ESMALIB)/libFVdycoreCubed_GridComp.a \
                     $(ESMALIB)/libARIESg3_GridComp.a \
                     $(ESMALIB)/libGEOSdatmodyn_GridComp.a\
                     $(LIB_FMS) \
                     $(ESMALIB)/libfvdycore.a \
                     $(ESMALIB)/libGMAO_pilgrim.a \
                     $(ESMALIB)/libGEOSdataatm_GridComp.a)

LIB_PHYSICS = $(wildcard $(ESMALIB)/libGEOSphysics_GridComp.a\
                         $(ESMALIB)/libGEOSradiation_GridComp.a \
                         $(ESMALIB)/libGEOSsolar_GridComp.a \
                         $(ESMALIB)/libGEOSirrad_GridComp.a \
                         $(ESMALIB)/libRRTMG.a \
                         $(ESMALIB)/libGEOSgwd_GridComp.a \
                         $(ESMALIB)/libRRTMG_mods.a \
                         $(ESMALIB)/libRRTMG_SW.a \
                         $(ESMALIB)/libRRTMG_SW_mods.a \
                         $(ESMALIB)/libGEOSsatsim_GridComp.a \
                         $(ESMALIB)/libGEOS_RadiationShared.a \
                         $(ESMALIB)/libGEOSmoist_GridComp.a \
                         $(ESMALIB)/libGEOSturbulence_GridComp.a \
                         $(ESMALIB)/libGEOSsurface_GridComp.a \
                         $(ESMALIB)/libGEOSlake_GridComp.a \
                         $(ESMALIB)/libGEOSlandice_GridComp.a \
                         $(ESMALIB)/libGEOSsaltwater_GridComp.a \
                         $(ESMALIB)/libGEOSland_GridComp.a \
                         $(ESMALIB)/libGEOScatch_GridComp.a \
                         $(ESMALIB)/libGEOScatchCN_GridComp.a \
                         $(ESMALIB)/libGEOSvegdyn_GridComp.a \
                         $(ESMALIB)/libGEOS_LandShared.a \
                         $(ESMALIB)/libGEOS_SurfaceShared.a $(LIB_GEOSCHEM))

LIB_LANL_CICE = $(wildcard $(ESMALIB)/libLANL_cice.a)

LIB_ATM = $(wildcard $(ESMALIB)/libGEOSagcm_GridComp.a \
                     $(ESMALIB)/libGEOSdataatm_GridComp.a \
                     $(ESMALIB)/libGEOSmkiau_GridComp.a \
                     $(LIB_PHYSICS) $(LIB_DYN))


#                  --------------------
#                  User Defined Targets
#                  --------------------

FREAL = $(FREAL4)
SRCS = $(THIS).F90
OBJS = $(SRCS:.F90=.o) 

DIR_GC = $(wildcard $(ESMAINC)/GEOS[a-z]*GridComp $(INC_GEOSCHEM) )  
INC_GC = $(foreach FF,$(DIR_GC),$(M)$(FF))

INC_DIRS = . $(INC_GEOS_GCS) $(INC_GMAO_SHARED) $(INC_ESMF) $(INC_MPI)
MOD_DIRS = . $(INC_DIRS)

USER_FINCS  = $(foreach dir,$(INC_DIRS),$(I)$(dir))
USER_FMODS  = $(foreach dir,$(MOD_DIRS),$(M)$(dir))
USER_FFLAGS = $(USER_DEFS) $(BIG_ENDIAN)
USER_LDFLAGS += $(MCMODEL)

vpath % $(MOD_DIRS)

LIB_GCM = $(ESMALIB)/libGEOSgcm_GridComp.a $(LIB_ATM) \
          $(ESMALIB)/libGEOSogcm_GridComp.a $(LIB_OCEAN) $(LIB_LANL_CICE)

LIB_GCS = $(wildcard $(ESMALIB)/libGEOSgcs_GridComp.a $(LIB_GCM) $(LIB_ANA))

LIB_COMP = $(LIB_GCS) $(LIB_SHARED)

ifneq ( $(wildcard $(ESMALIB)/libGEOSgcs_GridComp.a), $(null) ) 
   USER_FDEFS += -DUSE_GCS
endif


ifeq ($(LIB_SDF),$(LIB_HDF))
   LIB_SDF :=  $(ESMALIB)/libGMAO_mfhdf3.a $(LIB_SDF)
endif

%.x : %.o $(LIB_COMP)
	$(LD) -o $@ $(LDFLAGS) $*.o -Wl,--start-group $(LIB_COMP) -Wl,--end-group $(LIB_APROF)   \
                                   $(LIB_ESMF) $(LIB_SDF) \
                                   $(LIB_SCI) $(LIB_MPI) $(LIB_SYS)

#                  --------------------
#                      Dependencies
#                  --------------------

$(THIS).o: $(LIB_COMP)

  -include $(ESMADIR)/Config/ESMA_post.mk  # ESMA additional targets, macros

#.
