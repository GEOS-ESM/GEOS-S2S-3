#! /usr/bin/env python
from netCDF4 import Dataset
import matplotlib.pyplot as plt
import numpy as np
import array
import matplotlib.cm as cm
from mpl_toolkits.basemap import Basemap
import glob
import struct
import time
import sys
from mpl_toolkits.basemap import Basemap, shiftgrid, addcyclic
from scipy import interpolate
import getopt
import string
from datetime import date
import scipy.interpolate as interp
import scipy.optimize as optm
import subprocess
from utils import get_state, write_state, write_mom_restart
#sys.path.append('/usr/local/other/SLES11/mpi4py-1.3/lib/python/')
#from mpi4py import MPI
#Ne=int(sys.argv[1])

class Ods:
    def __init__(self, fname):
        ncfile = Dataset(fname, 'r')
        self.OBSID = ncfile.variables['OBSID'][:]
        self.lon = ncfile.variables['lon'][:]
        self.lat = ncfile.variables['lat'][:]
        self.lev = ncfile.variables['lev'][:]
        self.obs = ncfile.variables['obs'][:]
        self.sigo = ncfile.variables['sigo'][:]
        self.instid = ncfile.variables['instid'][:]
        self.omf = ncfile.variables['omf'][:]
        self.qc = ncfile.variables['qc'][:]
        ncfile.close()

#  eric try to eliminate qc=0 data
#    eric 10/27/22 leave this for mean??
#       I=np.where(self.qc==1)
#       self.OBSID=self.OBSID[I]
#       self.lon=self.lon[I]
#       self.lat=self.lat[I]
#       self.lev=self.lev[I]
#       self.obs=self.obs[I]
#       self.sigo=self.sigo[I]
#       self.instid=self.instid[I]
#       self.omf=self.omf[I]
#       self.qc=self.qc[I]
#    eric 10/27/22 leave this for mean??

#       self.OBSID=np.squeeze(self.OBSID.flatten()[I])#[I])
#       self.lon=np.squeeze(self.lon.flatten()[I])#[I])
#       self.lat=np.squeeze(self.lat.flatten()[I])#[I])
#       self.obs=np.squeeze(self.obs.flatten()[I])#[I])
#       self.sigo=np.squeeze(self.sigo.flatten()[I])#[I])
#       self.omf=np.squeeze(self.omf.flatten()[I])#[I])
#       self.qc=np.squeeze(self.qc.flatten()[I])#[I])
#  end erics try

        self.std_omf = np.zeros(np.shape(self.omf))
        self.oma = np.zeros(np.shape(self.omf))
        self.std_oma = np.zeros(np.shape(self.omf))

def ana_stats(yyyy, mm, dd, hh, stats_type='omf'):
    #ods_flist=glob.glob('./oana-'+yyyy+mm+dd+'_'+seq+'/'+stats_type+'/*.dat.nc')
    #ods_flist=glob.glob('./ocean_observer_'+yyyy+mm+dd+'_'+hh+'/*.dat.nc')
    ods_flist=glob.glob('./*.dat.nc')
    ods_flist.sort()

    # Scan data and limit to only data with QC=1
    #============
    index=0.
    for ods_fname in ods_flist:
        print('in mean_ods.py scan for QC', ods_fname)
        if (index==0.):
            obs=Ods(ods_fname)
            qcout=obs.qc
        else:
            obs_tmp = Ods(ods_fname)
            for i in range (np.shape(obs.qc)[0]):
              if (obs_tmp.qc[i] != 1 and  qcout[i] == 1):
                print ('found new qc ')
                qcout[i]=0
        index+=1.
        print('in mean_ods.py index', index)

    #  now trim the data to all be the same size 
    
    print('in mean_ods.py', ods_flist)
    #Compute mean
    #============
    index=0.
    for ods_fname in ods_flist:
        print('in mean_ods.py', ods_fname)
        if (index==0.):
            obs=Ods(ods_fname)

          #   now smash it to get rid of others qc=0 from qcout
            I=np.where(qcout==1)
            obs.OBSID=obs.OBSID[I]
            obs.lon=obs.lon[I]
            obs.lat=obs.lat[I]
            obs.lev=obs.lev[I]
            obs.obs=obs.obs[I]
            obs.sigo=obs.sigo[I]
            obs.instid=obs.instid[I]
            obs.omf=obs.omf[I]
            obs.oma=obs.oma[I]
            obs.qc=obs.qc[I]
        else:
            obs_tmp = Ods(ods_fname)

          #   now smash it to get rid of others qc=0 from qcout
            I=np.where(qcout==1)
            obs_tmp.OBSID=obs_tmp.OBSID[I]
            obs_tmp.lon=obs_tmp.lon[I]
            obs_tmp.lat=obs_tmp.lat[I]
            obs_tmp.lev=obs_tmp.lev[I]
            obs_tmp.obs=obs_tmp.obs[I]
            obs_tmp.sigo=obs_tmp.sigo[I]
            obs_tmp.instid=obs_tmp.instid[I]
            obs_tmp.omf=obs_tmp.omf[I]
            obs_tmp.oma=obs_tmp.oma[I]
            obs_tmp.qc=obs_tmp.qc[I]
            print ('lenth of index=0 obs.omf',len(obs.omf))
            print ('lenth of index!=0 obs_tmp.omf',len(obs_tmp.omf))

#           obs_tmp.omf = obs.omf + obs_tmp.omf
#           print ('in loop95 obs_tmp.omf,stats_type  ', obs_tmp.omf[1:5],stats_type )
            obs.omf = obs.omf + obs_tmp.omf
            print ('in loop95 obs_tmp.omf,stats_type  ', obs.omf[1:5], obs_tmp.omf[1:5],stats_type )
        index+=1.
        print('in mean_ods.py index', index)
    if (stats_type=='omf'):
        obs.omf = obs.omf/index
#       obs.omf = obs_tmp.omf/index
        print ('line135 obs_tmp.omf,stats_type  ', obs.omf[1:5],stats_type )
    if (stats_type=='oma'):
        obs.oma = obs.omf/index
#       obs.oma = obs_tmp.omf/index
        print ('line138 obs_tmp.omf,stats_type  ', obs.omf[1:5],stats_type )
        print ('line138 obs_tmp.omf,stats_type  ', obs.oma[1:5],stats_type )

    #Compute std
    #============
    index=0.
    for ods_fname in ods_flist:
        if index==0:
            std_omf = np.zeros(np.shape(obs.omf))
            if (stats_type=='oma'):
                std_oma = np.zeros(np.shape(obs.omf))
        obs_tmp = Ods(ods_fname)

    #   now smash it to get rid of others qc=0 from qcout
        I=np.where(qcout==1)
        obs_tmp.OBSID=obs_tmp.OBSID[I]
        obs_tmp.lon=obs_tmp.lon[I]
        obs_tmp.lat=obs_tmp.lat[I]
        obs_tmp.lev=obs_tmp.lev[I]
        obs_tmp.obs=obs_tmp.obs[I]
        obs_tmp.sigo=obs_tmp.sigo[I]
        obs_tmp.instid=obs_tmp.instid[I]
        obs_tmp.omf=obs_tmp.omf[I]
        obs_tmp.oma=obs_tmp.oma[I]
        obs_tmp.qc=obs_tmp.qc[I]

        if (stats_type=='omf'):
#           std_omf = std_omf + (obs.omf - obs_tmp.omf)**2
            std_omf = std_omf + (obs_tmp.omf - obs.omf)**2
        if (stats_type=='oma'):
#           std_oma = std_oma + (obs.oma - obs_tmp.omf)**2
#           std_oma = std_oma + (obs_tmp.oma - obs.oma)**2
#           std_oma = std_oma + (obs_tmp.oma - obs.oma)**2
            std_oma = std_oma + (obs_tmp.omf - obs.oma)**2

        print ('in loop142 obs_tmp.omf,stats_type  ', obs_tmp.omf[1:5],stats_type )
        index+=1.

    print ('index is',index)
    if (stats_type=='omf'):
        obs.std_omf = np.zeros(np.shape(obs.omf))
        obs.std_omf = (std_omf/(index-1.))**0.5
        print ('in ana_stats omf',len(obs.omf))
    if (stats_type=='oma'):
        obs.std_oma = np.zeros(np.shape(obs.oma))
        obs.std_oma = (std_oma/(index-1.))**0.5
        print ('in ana_stats oma',len(obs.oma))

    return obs

yyyy       = sys.argv[1]     # '2012'
mm         = sys.argv[2]     # '12'
dd         = sys.argv[3]     # '01'
hh        = sys.argv[4]     # DA sequence
#seq        = sys.argv[5]     # DA sequence
stats_type = sys.argv[5]     # omf or oma

outfile='obs-'+yyyy+mm+dd+'_'+hh+'.nc'
print('in mean_ods.py', outfile, stats_type)

#try:
obs=ana_stats(yyyy, mm, dd, hh, stats_type=stats_type)

if (stats_type=='omf'):

    ncfile = Dataset(outfile,'w')
    ncfile.createDimension('nobs',len(obs.omf))

    tmp = ncfile.createVariable('OBSID',np.dtype('int32').char,('nobs'))
    tmp[:] = obs.OBSID

    tmp = ncfile.createVariable('lon',np.dtype('float32').char,('nobs'))
    tmp[:] = obs.lon

    tmp = ncfile.createVariable('lat',np.dtype('float32').char,('nobs'))
    tmp[:] = obs.lat

    tmp = ncfile.createVariable('lev',np.dtype('float32').char,('nobs'))
    tmp[:] = obs.lev

    tmp = ncfile.createVariable('obs',np.dtype('float32').char,('nobs'))
    tmp[:] = obs.obs

    tmp = ncfile.createVariable('sigo',np.dtype('float32').char,('nobs'))
    tmp[:] = obs.sigo

    tmp = ncfile.createVariable('instid',np.dtype('float32').char,('nobs'))
    tmp[:] = obs.instid

    tmp = ncfile.createVariable('omf',np.dtype('float32').char,('nobs'))
    print ('in mean_ods.py omf output', len(obs.omf), len(tmp),len(obs.omf))
    tmp[:] = obs.omf
    print ('in mean_ods.py omf output', len(obs.omf), len(tmp),len(obs.omf))

    tmp = ncfile.createVariable('std_omf',np.dtype('float32').char,('nobs'))
    tmp[:] = obs.std_omf

if (stats_type=='oma'):
    #obs=ana_stats(stats_type='oma')

    ncfile = Dataset(outfile,'a')
    ncfile.createDimension('nobsa',len(obs.omf))

    tmpa = ncfile.createVariable('OBSIDa',np.dtype('int32').char,('nobsa'))
    tmpa[:] = obs.OBSID

    tmpa = ncfile.createVariable('lona',np.dtype('float32').char,('nobsa'))
    tmpa[:] = obs.lon

    tmpa = ncfile.createVariable('lata',np.dtype('float32').char,('nobsa'))
    tmpa[:] = obs.lat

    tmpa = ncfile.createVariable('leva',np.dtype('float32').char,('nobsa'))
    tmpa[:] = obs.lev

    tmpa = ncfile.createVariable('obsa',np.dtype('float32').char,('nobsa'))
    tmpa[:] = obs.obs

    tmpa = ncfile.createVariable('sigoa',np.dtype('float32').char,('nobsa'))
    tmpa[:] = obs.sigo

    tmpa = ncfile.createVariable('instida',np.dtype('float32').char,('nobsa'))
    tmpa[:] = obs.instid

    tmpa = ncfile.createVariable('oma',np.dtype('float32').char,('nobsa'))
    print ('in mean_ods.py oma output', len(obs.oma), len(tmpa),len(obs.omf))
    tmpa[:] = obs.oma
    print ('in mean_ods.py oma output', len(obs.oma), len(tmpa),len(obs.omf))

    tmpa = ncfile.createVariable('std_oma',np.dtype('float32').char,('nobsa'))
    tmpa[:] = obs.std_oma

ncfile.close()
#except:
#    print 'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDddd'
#    print 'pass ...'
#    pass
