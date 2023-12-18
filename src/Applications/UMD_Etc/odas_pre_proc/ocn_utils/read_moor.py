#! /usr/bin/env python

# Get the obs profiles to plot subsurface field, anomaly and tendency
# TAO eq Pacific,  RAMA eq Indian, PIRATA eq Atlantic

from netCDF4 import Dataset
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import numpy as np
import glob
import sys
import getopt
import os.path
import math
import pickle
from matplotlib.ticker import MultipleLocator, FormatStrFormatter
import scipy.stats as scstats
import itertools
sys.path.append('/gpfsm/dnb42/projects/p17/rkovach/geos5/OceanPost/ocn_utils/')
import read_grid
class dataclass:
	pass

########### BAD!!! ########
np.seterr(invalid='ignore')

def _find_nearest(a, a0):
    """Returns the index of the element in nd array `a` closest to the scalar value `a0`"""
    idx = np.abs(a - a0).argmin()
    return idx

def _nan_filt(a):
    """Returns the array a with large values (>9999.9) replaced with np.nan"""
    a[np.abs(a)>9999.9]=np.nan
    return a

def _zero_filt(a):
    """Returns the array a with values exactly equal to 0 replaced with np.nan"""
    a[np.abs(a)==0.0]=np.nan
    return a

###############################################################################################################3
def get_eq_slice(year,mon,instr,DEPcom,LONcom):
    """Returns xz equatorial T cross-section from Mooring obs file"""

    climdir = '/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/POST/PLOT_UTILS/OCEAN_CLIMATE/Clim'
    obsdir  = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00//RC/OBS/'+instr+'_6.0/'
    if (int(year) == 2016):
    	obsdir = '/discover/nobackup/projects/gmao/ssd/g5odas//production/GEOS5odas-5.00/RC/OBS/REAL_TIME/'
    flobs   = obsdir+'T_'+instr+'_'+str(year)+'.nc'
    #print 'file ',flobs
    isfile = 'T'
    if not os.path.isfile(flobs):
	print 'File does not exist:'
	isfile='F'
	Tprof = 0
	return (Tprof,isfile)

    ncfile  = Dataset(flobs, mode="r", clobber=True, format='NETCDF4')
    DTobs   = ncfile.variables['DATE_TIME'][:]
    LONobs  = ncfile.variables['LON'][:]
    LATobs  = ncfile.variables['LAT'][:]
    Tobs    = ncfile.variables['TEMP'][:,:]
    DEPobs  = ncfile.variables['DEPTH'][:,:]
    ncfile.close()

    Tobs   = _nan_filt(Tobs)
    DEPobs = _nan_filt(DEPobs)
    DTobs  = np.floor(DTobs/10000)
    #print np.shape(Tobs), np.min(DTobs), np.max(DTobs)

    ########### for EQ, get all moorings at 2N, EQ, 2S
    #LATind = np.where(np.logical_or.reduce((LATobs == 0, LATobs == -2, LATobs == 2)))
    LATind = np.where(np.logical_and.reduce((LATobs >= -2, LATobs <= 2)))
    LONeq  = np.sort(np.unique(LONobs[LATind]))
    #print LONeq

    ########## convert lons from -180:180 to 0:360 format for TAO only
    if instr == 'TAO':
       LONeq180 = LONeq
       ind180 = np.where(LONeq<0)
       LONeq180[ind180] = LONeq[ind180] + 360
       LONeq180 = sorted(LONeq180) 
       LONobs180 = LONobs
       ind180 = np.where(LONobs<0)
       LONobs180[ind180] = LONobs[ind180] + 360
       LONeq = LONeq180
       LONobs = LONobs180

    ######### Time index for current month #############
    DTind = np.where(DTobs == year*100+mon)
    #print np.shape(DTind)

    ######### Get data at each eq location for a given month ##############
    Tcom = [] 
    for lon in LONeq:
       LONind = np.where(LONobs == lon)
       OBSind = np.intersect1d(np.asarray(LONind), np.asarray(LATind))
       OBSind = np.intersect1d(np.asarray(OBSind), np.asarray(DTind))
       Ttemp = []
       if len(OBSind) > 0:
          for ind in OBSind:
             gooddep = np.where(np.isfinite(np.squeeze(DEPobs[ind,:])))
             goodt = np.where(np.isfinite(np.squeeze(Tobs[ind,:])))
             goodind = np.intersect1d(np.asarray(gooddep),np.asarray(goodt))
             try:
                f = np.interp(DEPcom,np.squeeze(DEPobs[ind,goodind]),np.squeeze(Tobs[ind,goodind]),left = np.nan, right = np.nan)
             except:
                print 'nothing to interpolate', np.shape(goodind)[0]
                f = np.empty(np.shape(DEPcom)[0])
                f[:] = np.nan
             Ttemp.append(f)
             temp = scstats.nanmean(Ttemp,axis = 0)
       else:
          print 'no data at the location',lon
          temp = np.empty(np.shape(DEPcom)[0])
          temp[:] = np.nan
       
       Tcom.append(temp)

    a    = list(itertools.chain.from_iterable(Tcom)) 
    Tcom = np.reshape(a,(np.shape(LONeq)[0],np.shape(DEPcom)[0]))

    Tprof = [] 
    for d in range(0,np.shape(DEPcom)[0]):
      f = np.interp(LONcom,LONeq,Tcom[:,d],left = np.nan, right = np.nan)
      Tprof.append(f)  

    return Tprof, isfile

###############################################################################################################3
def xz(year,mon,instr):
	data = dataclass()

	data.id    = 'TAO'
	DEPcom = np.arange(0,300,1)   # set of common depths
	LONcom = np.arange(130,280,1) # set of common lons
	clim   = '1993_2010'          # default
	if instr=='PIR':
 		LONcom  = np.arange(-55,6,1)
		clim    = '1997_2010'
		data.id = 'PIR'
	if instr=='RAMA':
 		LONcom  = np.arange(40,100,1)
		clim    = '2005_2010'
		data.id = 'RAMA'

	# CLIM 
	# Climatology information is hardcoded; assume the file TAO_eq_clim_1993_2010.dat is available
	# If not, use TAO_xz_eq_clim.py to compute 
	climdir   = '/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/POST/PLOT_UTILS/OCEAN_CLIMATE/Clim'
	f         = open(climdir+'/'+instr+'_eq_clim_'+clim+'.dat','r')
	INSTRclim = pickle.load(f)
	INSTRlon  = pickle.load(f)
	f.close()

	# Get T profiles and anomalies for current and previous month #############
	TprofC,data.file = get_eq_slice(int(year),int(mon),instr,DEPcom,LONcom)

	INSTRanomC = np.transpose(TprofC) - INSTRclim[int(mon)-1,:,:]

	if mon > 1:
  	 TprofP,data.file = get_eq_slice(int(year), int(mon)-1,instr,DEPcom,LONcom)
  	 INSTRanomP = np.transpose(TprofP) - INSTRclim[int(mon)-1,:,:]
	elif mon == 1:
	   TprofP,data.file = get_eq_slice(int(year)-1, 11,instr,DEPcom,LONcom)
  	INSTRanomP = np.transpose(TprofP) - INSTRclim[11,:,:]

	########### Tendencies #############
	INSTRtend      = np.transpose(TprofC) - np.transpose(TprofP) 
	INSTRanomtend  = INSTRanomC - INSTRanomP

        x360          = np.where(LONcom<0)
        LONcom[x360]  = LONcom[x360]+360 
	if instr=='PIR':
        	x360          = np.where(LONcom<=5)
        	LONcom[x360]  = LONcom[x360]+360 

	data.lev   = DEPcom
	data.lon   = LONcom
	data.var   = TprofC	   # T full current
	data.vara  = INSTRanomC    # T anom current
	data.vart  = INSTRtend     # T tend
	data.varat = INSTRanomtend # T anom tend

	#data.varp  = TprofP	# T full prev
	#data.varap = INSTRanomP # T anom prev

        print data.id,':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0], data.lon[-1]

        return (data)	


