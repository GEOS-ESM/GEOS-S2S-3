#! /usr/bin/env python

# get the obs profiles to plot subsurface field, anomaly and tendency (TAO eq Pacific, RAMA eq Indian, PIRATA eq Atlantic)
# instr_xz_anom.py -y YYYY -m M(M) -i TAO/PIR/RAMA -c clim (clim: 1993_2010 for TAO, 1997_2010 for PIR, 2005_2010 for RAMA) 
#
# read_tao.py -y 2005 -m 08 -i TAO -c 1993_2010

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
sys.path.append('/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/POST/PLOT_UTILS/UTILS/')
import info

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

def get_eq_slice(year,mon,instr):
    """Returns xz equatorial T cross-section from TAO/RAMA/PIR obs file"""

###############################################################################################################3
    climdir = '/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/POST/PLOT_UTILS/OCEAN_CLIMATE/Clim'

    obsdir = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00//RC/OBS/'+instr+'_6.0/'
    #obsdir = '/discover/nobackup/projects/gmao/ssd/g5odas//production/GEOS5odas-5.00/RC/OBS/REAL_TIME/'
    flobs = obsdir+'T_'+instr+'_'+str(year)+'.nc'
    print flobs
    ncfile = Dataset(flobs, mode="r", clobber=True, format='NETCDF4')
    DTobs = ncfile.variables['DATE_TIME'][:]
    LONobs = ncfile.variables['LON'][:]
    LATobs = ncfile.variables['LAT'][:]
    Tobs = ncfile.variables['TEMP'][:,:]
    DEPobs = ncfile.variables['DEPTH'][:,:]
    ncfile.close()

    Tobs = _nan_filt(Tobs)
    DEPobs = _nan_filt(DEPobs)
    DTobs = np.floor(DTobs/10000)
    #print np.shape(Tobs), np.min(DTobs), np.max(DTobs)

########### for EQ, get all moorings at 2N, EQ, 2S
    #LATind = np.where(np.logical_or.reduce((LATobs == 0, LATobs == -2, LATobs == 2)))
    LATind = np.where(np.logical_and.reduce((LATobs >= -2, LATobs <= 2)))

    LONeq = np.sort(np.unique(LONobs[LATind]))
    print LONeq

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
    print np.shape(DTind)
######### Get data at each eq location for a given month ##############
    Tcom = [] 
    for lon in LONeq:
       LONind = np.where(LONobs == lon)
       OBSind = np.intersect1d(np.asarray(LONind), np.asarray(LATind))
       OBSind = np.intersect1d(np.asarray(OBSind), np.asarray(DTind))
       Ttemp = []
       #print lon,OBSind
       #print lon
       if len(OBSind) > 0:
          for ind in OBSind:
             #print ind, DEPobs[ind,:],Tobs[ind,:]
             gooddep = np.where(np.isfinite(np.squeeze(DEPobs[ind,:])))
             goodt = np.where(np.isfinite(np.squeeze(Tobs[ind,:])))
             goodind = np.intersect1d(np.asarray(gooddep),np.asarray(goodt))
             try:
                #print np.squeeze(DEPobs[ind,goodind])
                #print np.squeeze(Tobs[ind,goodind])
                f = np.interp(DEPcom,np.squeeze(DEPobs[ind,goodind]),np.squeeze(Tobs[ind,goodind]),left = np.nan, right = np.nan)
             	#print f
                #Ttemp.append(f)
             except:
                print 'nothing to interpolate', np.shape(goodind)[0]
                f = np.empty(np.shape(DEPcom)[0])
                f[:] = np.nan
             Ttemp.append(f)
             temp = scstats.nanmean(Ttemp,axis = 0)
             #print f[0:10]
	     #raw_input('stop')
       else:
          print 'no data at the location',lon
          temp = np.empty(np.shape(DEPcom)[0])
          temp[:] = np.nan
       
       Tcom.append(temp)
       #print Ttemp
       #raw_input('stop')

    #print np.shape(Tcom), np.shape(LONeq180), np.shape(DEPcom)
    a = list(itertools.chain.from_iterable(Tcom)) 
    Tcom = np.reshape(a,(np.shape(LONeq)[0],np.shape(DEPcom)[0]))

    Tprof = [] 
    for d in range(0,np.shape(DEPcom)[0]):
      f = np.interp(LONcom,LONeq,Tcom[:,d],left = np.nan, right = np.nan)
      Tprof.append(f)  

    return Tprof

##########  Climatology information is hardcoded; assume the file TAO_eq_clim_1993_2010.dat is available ###########
##########  If not, use TAO_xz_eq_clim.py to compute ###############################################################

monnms = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']

# DO NOT CHANGE THESE VALUES, THEY MUST MATCH THOSE IN TAO_xz_eq_clim.py
DEPcom = np.arange(0,300,1) # set of common depths
LONcom = np.arange(130,280,1) # set of common lons

clim='1993-2010' # default

options, remainder = getopt.getopt(sys.argv[1:], 'y:m:i:c:')
print options

for opt, arg in options:
    print opt, arg
    if opt=='-y':
        year=int(arg)
    elif opt=='-m':
        mon=int(arg)
    elif opt=='-i':
        instr=arg
    elif opt=='-c':
        clim=arg

if mon<10:
  figname_f = instr+'_T_0'+str(mon)+str(year)+'.png'
  figname_a = instr+'_Tanom_0'+str(mon)+str(year)+'.png'
  figname_t = instr+'_Ttend_0'+str(mon)+str(year)+'.png'
  figname_at = instr+'_Tanomtend_0'+str(mon)+str(year)+'.png'
else:  
  figname_f = instr+'_T_'+str(mon)+str(year)+'.png'
  figname_a = instr+'_Tanom_'+str(mon)+str(year)+'.png'
  figname_t = instr+'_Ttend_'+str(mon)+str(year)+'.png'
  figname_at = instr+'_Tanomtend_'+str(mon)+str(year)+'.png'

################ CLIM #############
climdir = '/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/POST/PLOT_UTILS/OCEAN_CLIMATE/Clim'

f = open(climdir+'/'+instr+'_eq_clim_'+clim+'.dat','r')
INSTRclim = pickle.load(f)
INSTRlon = pickle.load(f)
f.close()

########### Plot Settings #########
pos_ax = [0.1, 0.15, 0.7, 0.7]
pos_cax = [0.86, 0.25, 0.025, 0.55]


LONcom = np.arange(130,280,1) # set of common lons
xtxlbs = ['140E','160E','180E','160W','140W','120W','100W']
xtx = [140,    160,   180,   200,   220,   240,  260]
xlims = [140,260]
ytx = np.arange(0,310,100)
ytxlbs = ['0','100','200','300']

xminorLocator   = MultipleLocator(5)
yminorLocator   = MultipleLocator(10)

############ Get Variable Info (mainly for units and contours) #############
var_info = info.var_info('T')

############ Get T profiles and anomalies for current and previous month #############
TprofC = get_eq_slice(year,mon,instr)
#raw_input('stop')
INSTRanomC = np.transpose(TprofC) - INSTRclim[mon-1,:,:]
if mon > 1:
   TprofP = get_eq_slice(year, mon-1,instr)
   INSTRanomP = np.transpose(TprofP) - INSTRclim[mon-1,:,:]
elif mon == 1:
   TprofP = get_eq_slice(year-1, 11,instr)
   INSTRanomP = np.transpose(TprofP) - INSTRclim[11,:,:]

########### Tendencies #############
INSTRtend = np.transpose(TprofC) - np.transpose(TprofP) 
INSTRanomtend  = INSTRanomC - INSTRanomP

####### PLOT Full Field ###########
fig1 = plt.figure(num=None, figsize=(6,3), facecolor='w')  
ax1 = plt.axes(pos_ax) 

var_info.datarange  = np.arange(0,  32, 1)
var_info.contours   = np.arange(0, 32, 2)
hf = plt.contourf(LONcom,DEPcom,TprofC,var_info.datarange,cmap=cm.jet, extend='both')
hf2 = plt.contour(hf, levels=var_info.contours, colors = 'k', hold='on')
plt.clabel(hf2, fmt = '%d',fontsize=8)
ax1 = plt.gca()
ax1.invert_yaxis()
ax1.set_xlim(xlims)
ax1.set_xticks(xtx) 
ax1.set_xticklabels(xtxlbs,fontsize = 12,rotation=30)
ax1.xaxis.set_minor_locator(xminorLocator)
ax1.set_ylim(300,0)
ax1.set_yticks(ytx)
ax1.set_yticklabels(ytxlbs,fontsize = 12)#,rotation=30)
ax1.yaxis.set_minor_locator(yminorLocator)
ax1.set_title(instr+' T '+monnms[mon-1]+' '+str(year), fontsize = 12)

cx  = fig1.add_axes(pos_cax) 
cbar=plt.colorbar(hf,cax=cx,orientation='vertical',extend='both')
cbar.ax.set_xlabel(var_info.units,size=12)

plt.show()
raw_input('stop')

   
