#! /usr/bin/env python
# /////////////////////////////////////////////////////////////////////////
# /**
# * Title: ocean_obs.py
# *
# * Description: Extract the ocean observations for a specific date/time window.
# *
# * Args:
# *              yyyy     : 4 digit year
# *              mm       : 2 digit month
# *              dd       : 2 digit day
# *              hh       : 2 digit hour
# *              obs_type : Entered as a list
# *                         Argo
# *                         CTD
# *                         XBT
# *                         TAO
# *                         PIRATA
# *                         RAMA
# *                         SMOS (Level 2)
# *                         SMOSSUB (SUbset Level 2)
# *                         SMOSL3 (Level 3)
# *                         AQUARIUS (Level 2)
# *                         AQUARIUS V5 RIM5 V1 (Level 2)
# *                         SMAP (Level 2)
# *                         SMAPV4.1 (Level 2)
# *                         SMAPV4.2 (Level 2)
# *                         SMAPV4.3 (Level 2)
# *                         AQUARIUSL3
# *                         SMAPL3
# *                         CryoSat-2
# *                         Jason-1
# *                         Jason-2
# *                         Jason-3
# *                         Saral
# *                         Sentinel-3a
# *                         ERS-1
# *                         ERS-2
# *                         TOPEX
# *                         GEOSAT-2
# *                         Envisat
# *                         HY-2A
# *                         Reynolds
# *                         OSTIA
# *                         AVHRR-18
# *                         NOAA-16
# *                         METOP-A
# *                         NASA-TEAM-2
# *                         NOAA-AICE
# *                         OIB-HICE
# *
# * Example:
# *
# *             ocean_obs.py 2012 03 15 03 Argo CTD Jason-2 CryoSat-2
# *
# *             Will create a netcdf file and a series of png figures containing/showing the observations
# *             that are within the assimilation window.
# *
# * @author: Guillaume Vernieres
# */
#
# /////////////////////////////////////////////////////////////////////////
# Date: Dec 2015
#from memory_profiler import profile
import xarray as xr
from netCDF4 import Dataset
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.cm as cm
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import sys
from datetime import datetime, timedelta
import os
import subprocess
import time
import io
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)

#SLES VERSION
#SLN = os.popen("cat /etc/os-release | grep VERSION_ID | cut -d'=' -f2 | cut -d'.' -f1 | tr -d '\"'").read().strip()

#   test of 50 layers Argo data for 2012 only
OBSDIR='/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/RC/OBS/'
LEV50INSITUOBSDIR='/discover/nobackup/projects/gmao/m2oasf/aogcm/g5odas/obs/assim/'
ERICLEV50INSITUOBSDIR='/gpfsm/dnb78s2/projects/p26/ehackert/ODAS/OBS/V3_WITHHOLD_BY_WMO/'
ADTOBSDIR='/discover/nobackup/projects/gmao/m2oasf/aogcm/g5odas/obs/assim/AVISO/V3/'
SatSSSOBSDIR='/discover/nobackup/projects/gmao/m2oasf/aogcm/g5odas/obs/assim/'
SatSSSSMOSOBSDIR='/discover/nobackup/lren1/pre_proc/NRT/SMOS/L2_SMOS_SSS_7.0/'
SYNOBSDIR='/discover/nobackup/projects/gmao/m2oasf/aogcm/g5odas/obs/assim/SYN_7.0/'
WOA18='/discover/nobackup/lren1/pre_proc/NRT/WOA/'

#   artificially ramp up smos error
smosrampuperr=1.

# OIB data
OIBDIR='/discover/nobackup/projects/gmao/ssd/g5odas/gvernier/SAND_BOXES/sea_ice_da/ICE_BRIDGE/'

# NESDIS L2-SST
NESDISDIR='/discover/nobackup/projects/gmao/m2oasf/aogcm/g5odas/obs/raw/L2_SST/NESDIS/'
prefix_dict = {
    'NOAA16'  : '0000-STAR-L2P_GHRSST-SSTskin-AVHRR16_G-ACSPO_V2.40-v02.0-fv01.0.nc',
    'METOPA'  : '0000-STAR-L2P_GHRSST-SSTskin-AVHRRMTA_G-ACSPO_V2.40-v02.0-fv01.0.nc'
    }

# Note on sigos from ORAS4 from 2012 tech-memo "The NEMOVAR ocean data assimilation system ..."
# Mostly errors of representation, so same for XBT, CTD and Argo (while measurement errors is quite different)
# T sigo: 0.18 deg C, single global surface value but depth dependent, lower bound is 0.07 deg C
# S sigo: 0.18 psu, single global surface value but depth dependent, lower bound is 0.02 psu
# SSH: 5 cm

T_prof_sigo = 0.25   # deg C
S_prof_sigo = 0.025  # psu

xsigo_s    = 1.0 #1.0
xsigo_t    = 1.0 #1.0
xsigo_sst  = 2.0
xsigo_sss  = 1.0
xsigo_ssh  = 0.25
xsigo_aice = 1.0
xsigo_hice = 0.01

try:
    EXP_NDAYS = float(os.environ['ODAS_NDAYS'])
except:
    EXP_NDAYS = 0.125 # Default value for the observation window [yyyymmdd_hh-EXP_NDAY

logit_transform = False
use_obs_old = False

try:
    T_prof_sigo = float(os.environ['ODAS_T_prof_sigo'])
    S_prof_sigo = float(os.environ['ODAS_S_prof_sigo'])
    ADT_sigo = float(os.environ['ODAS_ADT_sigo'])
    SSS_sigo = float(os.environ['ODAS_SSS_sigo'])

    xsigo_t    = T_prof_sigo
    xsigo_s    = S_prof_sigo
    xsigo_ssh  = ADT_sigo
    xsigo_sss  = SSS_sigo

    SCRDIR = os.environ['SCRDIR']
    EXPDIR = os.environ['EXPDIR']
    EXPID = os.environ['EXPID']
except:
    print('Environement variables not set, reverting to default:')

print('NDAYS=',EXP_NDAYS)
print('T_prof_sigo=',T_prof_sigo)
print('S_prof_sigo=',S_prof_sigo)
print('ADT_sigo=',ADT_sigo)
print('SSS_sigo=',SSS_sigo)

TSE_TMPDIR = os.environ['TSE_TMPDIR']
TMPDIR = os.environ['LOCAL_TMPDIR']
BOMB_ON_SIGO = os.environ['BOMB_ON_SIGO']
print(f'TSE save: {TSE_TMPDIR}')
print(f'Node save: {TMPDIR}')

# Obs id as defined in the UMD_oletkf
obsid_dict = {
    'id_s_obs'   : 5521, # Salinity
    'id_t_obs'   : 3073, # Temperature
    'id_sst_obs' : 5525, # SST
    'id_sss_obs' : 5522, # Sea surface Salinity
    'id_ssh_obs' : 5526, # SSH (Not used ...)
    'id_eta_obs' : 5351, # SSH
    'id_aice_obs': 6000, # AICE
    'id_hice_obs': 6001  # HICE
    }

def inv_dict(dict, ID):
    for key, value in dict.items():
        if value == ID:
            return key

def wait_for_file_completion(fname, min_file_minutes=5):
    """
    Waits until the file is at least `min_file_minutes` old before proceeding.

    Args:
        fname (str): Path to the file.
        min_file_minutes (int): Minimum age of the file in minutes before proceeding.
        check_interval (int): Time (in seconds) to wait before checking again.
    """
    while True:
        # Get the file's last modification time
        file_age_minutes = (time.time() - os.stat(fname).st_mtime) / 60
        if file_age_minutes >= min_file_minutes:
            print(f"File {fname} is ready (age: {file_age_minutes:.2f} minutes).")
            break
        else:
            print(f"Waiting... File {fname} is only {file_age_minutes:.2f} minutes old.")
        check_interval = min_file_minutes-file_age_minutes
        time.sleep(check_interval*60)  # Sleep before rechecking
        
def inv_logit(p):
    return np.exp(p) / (1 + np.exp(p))

def da_window(yyyy, mm, dd, hh, NDAYS):
    date = datetime.strptime(f'{yyyy}{mm}{dd}{hh}', '%Y%m%d%H')
    sd = int((date - timedelta(days=NDAYS)).strftime('%Y%m%d%H'))
    ed = int((date + timedelta(days=NDAYS)).strftime('%Y%m%d%H'))
    return sd,ed

def flatten(da, mask=None):
    if mask is not None:
        da = da.where(mask)
    if 'N_LEVS' in da.dims:
        da = da.stack(flatten=('N_PROF', 'N_LEVS'))
    else: 
        da = da.rename({'N_PROF':'flatten'})
    return da.dropna(dim = 'flatten').values

def standard_obs_reader(fname, vartype, yyyy, mm, dd, hh, EXP_NDAYS, TSE_TMPDIR):
    print('\nIN standard_obs_reader for',vartype)
    print(fname)
    intermediate = f'{TSE_TMPDIR}/extracted_{yyyy}{mm}{dd}{hh}_{os.path.basename(fname)}'
    if os.path.isfile(intermediate):
        print(f'Intermediate file {intermediate} found, loading obs')
    else:
        wait_for_file_completion(fname, min_file_minutes=5) # makes sure fname is not actively being updated
        keep = ['N_LEVS', 'DEPTH', vartype, 'QC_LEV', 'QC_PRF', 'LON', 'LAT', 'DATE_TIME', 'OBS_ERROR', 'INST_ID']
        obs = xr.open_dataset(fname, chunks='auto') # solution to memory issue in Milan
        obs = obs[keep]
        print('obs ini shape:', obs[vartype].shape)

        # Hammer solution to memory issue in Milan:
        ################################################
        max_mem = 30000000
        print(f'Chunk sizes: {[ (s, obs[vartype].chunksizes.get(s)[0]) for s in obs[vartype].chunksizes]}')
        chunk_in_memory = np.prod([v[0] for v in obs[vartype].chunksizes.values()])
        if chunk_in_memory > max_mem:
            print('reducing auto chunks')
            if obs.dims['N_LEVS']>1:
                nlev_chunk = min(50, obs.dims['N_LEVS'])
                npro_chunk = int(max_mem/nlev_chunk)
                obs = obs.chunk({'N_PROF': npro_chunk, 'N_LEVS': nlev_chunk})
            else:
                obs = obs.chunk({'N_PROF':max_mem})
            print("New chunks:", [ (s, obs[vartype].chunksizes.get(s)[0]) for s in obs[vartype].chunksizes])
        ################################################
        if obs.dims['N_LEVS']> 50:
            obs = obs.isel(N_LEVS=slice(0, 50))
            print('obs reduced n_levels shape:', obs.DEPTH.shape)
        else:
            obs = obs.squeeze()
        sd, ed = da_window(yyyy, mm, dd, hh, EXP_NDAYS)
        print('time window:', (sd,ed))
        time_mask = (obs.DATE_TIME>=sd) & (obs.DATE_TIME <= ed)
        time_mask = time_mask.compute()
        obs = obs.where(time_mask, drop=True)
        print('obs reduced time window shape:', obs[vartype].shape)
        qc_mask = (obs.QC_PRF == 1).compute()
        obs = obs.where(qc_mask, drop=True)
        print('obs reduced QCPRF shape:', obs[vartype].shape)
        if obs[vartype].shape[0] == 0:
            return xr.zeros_like(obs)
        print(f'Computing intermediate file: {intermediate}')
        obs = obs.compute()
        obs.to_netcdf(intermediate)
        del obs
    obs = xr.open_dataset(intermediate)
    return obs

def M2_sst_reader(yyyy, mm, dd, hh, path2scratch):
    vartype='sst'
    print('\nIN M2_sst_reader')
    expid = path2scratch.split('/')[-2]
    fname=path2scratch+'/sst_'+yyyy+mm+dd+'_'+hh+'00z.nc'
    fname2=path2scratch+'/AICE_'+yyyy+mm+dd+'_'+hh+'00z.nc'
    fname3=path2scratch+'/'+expid+'.ice_inst_6hr_glo_T1440x1080_slv.'+yyyy+mm+dd+'_1200z.nc4'
    if (os.path.exists(fname)):
        print(fname)
        print(fname2)
        print(fname3)
        obs = xr.open_dataset(fname, chunks='auto') # solution to memory issue in Milan
        if (os.path.exists(fname2)):
            sic = xr.open_dataset(fname2, chunks='auto')['AICE']
            print('dims:', sic.dims)
            print('in M2_sst_reader past AICE data read')
            print('length of sic',sic.shape)
        else:
            print('MISSING AICE FILE',fname2)
            sys.exit(1)
        if (os.path.exists(fname3)):
            sicmod = xr.open_dataset(fname3, chunks='auto')['AICE']
            sicmod =sicmod.rename({'Ydim':'lat', 'Xdim':'lon'}).squeeze()
            print('dims:', sicmod.dims)
            print('in M2_sst_reader past AICE model data read')
            print('length of sicmod',sicmod.shape)
        else:
            print('MISSING AICE MODEL FILE',fname3)
            sys.exit(1)        
        
        print('length of sic',sic.shape,sicmod.shape,obs[vartype].shape)
        
        gridfile=path2scratch+'/grid_spec.nc'
        print('grid file:',gridfile)
        grid = xr.open_dataset(gridfile, chunks='auto')
        #Get rid of land values
        obs[vartype] = obs[vartype].where(obs['mask']==1.)
        obs = obs.drop_vars('mask')
        print(f'Flattened shape after mask: {obs[vartype].count().values}')
        #Get rid of SST under ice
        obs[vartype] = obs[vartype].where(sic==0.)
        print(f'Flattened shape after sic: {obs[vartype].count().values}')
        #Get rid of SST under model ice
        obs[vartype] = obs[vartype].where(sicmod==0.)
        print(f'Flattened shape after sicmod: {obs[vartype].count().values}')
        #add other vars
        obs['LAT'] = (obs[vartype].dims, grid['y_T'].values)
        obs['LON'] = (obs[vartype].dims, grid['x_T'].values)
        obs['LON'] = obs['LON'].where(obs['LON']>=-180, obs['LON']+360)
        obs = obs.drop_vars(['lons', 'lats', 'time'])
        # for var in ['sst', 'LON', 'LAT']:
        obs = obs.stack(N_PROF=[...]).reset_index('N_PROF')
        obs = obs.drop_vars(['lon', 'lat'])
        # obs.LON[obs.LON<-180.0]=obs.LON[obs.LON<-180.0]+360.0
        obs['DEPTH'] = xr.ones_like(obs[vartype])
        obs['DATE_TIME'] = int(yyyy+mm+dd+hh)*xr.ones_like(obs[vartype])
        obs['INST_ID'] = 516*xr.ones_like(obs[vartype])
        obs['OBS_ERROR'] = .2*xr.ones_like(obs[vartype])
        obs['QC_LEV'] = xr.ones_like(obs[vartype])
        obs['QC_PRF'] = xr.ones_like(obs[vartype])
        skip = 4       
        obs = obs.isel(N_PROF = slice(None, None, skip))
        print(f'Flattened shape after skip: {obs[vartype].count().values}')
        obs = obs.dropna(dim="N_PROF", how="any")
        obs = obs.compute()
        del grid, sic, sicmod
    else:
        print(f'{fname} not found')
        return None
    return obs

class Obs:
    """
    """
    def __init__(self, yyyy, mm, dd, hh, fname, id_obs, vartype='TEMP', xsigo=1.0, descriptor='', platform = '', color='r', markersize=5, NDAYS=EXP_NDAYS):
        """ This function exctract the observations that are within the assimilation window
        Arg
            yyyy (int)     : 4 digit year
              mm (int)     : 2 digit month
              dd (int)     : 2 digit day
              hh (int)     : 2 digit hour
              fname        : Name of file containing the obs
              id_obs       : Obs identifier
              vartype      : Variable type, one of ['TEMP','SALT','ADT','ICE'] Note that ICE is ice fraction
              xsigo        : Used to rescale the obserror. new_sigo = xsigo x old_sigo
              descriptor   : String describing the instance of the object (ex: T-Argo, Jason-1-ADT, ...)
              NDAYS (float): Used to define the size of the assimilation window centered at yyyymmddhh [yyyymmddhh-NDAYS, yyyymmddhh+NDAYS].
    """
        # LOADING
        print('\n:::::::::::::::::::',descriptor, platform)
        if (platform == 'M2-SST'):
            obs = M2_sst_reader(yyyy, mm, dd, hh, path2scratch=SCRDIR)
            vartype= 'sst'
            if obs is None:
                self.no_obs(descriptor = descriptor, platform = platform, color = color, size = markersize, present=False)
                return
        elif not os.path.exists(fname) and not os.path.exists(f'{TSE_TMPDIR}/extracted_{yyyy}{mm}{dd}{hh}_{os.path.basename(fname)}'):
            # This is a file check that will only triger a no obs if fname is not found in the 1st ocean_obs pass.
            # Reader will search fname in the 1st pass and in the 2nd pass will search for the inermediate file (generated in the 1st pass).
            # This assures that the 1st and 2nd pass run on the same set of observations prior model masking (time window)
            self.no_obs(descriptor = descriptor, platform = platform, color = color, size = markersize, present=False)
            print(f'No file {fname}')
            return
        else:
            obs = standard_obs_reader(fname, vartype, yyyy, mm, dd, hh, EXP_NDAYS, TSE_TMPDIR)
        if obs[vartype].shape[0]==0:
            self.no_obs(descriptor = descriptor, platform = platform, color = color, size = markersize, present=False)
            print(f'No obs for time window in {fname}')
            return
        print(f'Time window shape: {obs[vartype].count().values}')
        # DEALING WITH ICE
        if ( (id_obs==obsid_dict['id_t_obs']) | (id_obs==obsid_dict['id_s_obs']) ): # Profiles
            afile = f'{SCRDIR}/rawM2sic_{int(yyyy)}{int(mm):02d}{int(dd):02d}.nc'
            AICE = xr.open_dataset(afile)
            dTdZ = xr.zeros_like(obs[vartype])
            dTdZ[:,:-1] = (np.abs(obs[vartype].diff('N_LEVS'))/obs.DEPTH.diff('N_LEVS')).values
            dTdZ[:,-1] = dTdZ[:,-2]
            LON = obs['LON'].values
            LAT = obs['LAT'].values
            if descriptor in ['CTD-S', 'CTD-T', 'XBT-T', 'Argo-S', 'Argo-T']:
                for indexp in range(obs.dims['N_PROF']):
                    lon, lat = LON[indexp], LAT[indexp]
                    lonminidx = np.argmin(np.abs(AICE['lonout'] - lon).values)
                    latminidx = np.argmin(np.abs(AICE['latout'] - lat).values)
                    if AICE['AICE'][latminidx, lonminidx]>.1:
                        print(('Eliminate ice',lon,lat,indexp,AICE['AICE'][latminidx,lonminidx].values), descriptor)
                        obs['QC_LEV'][indexp,:]=0
                        dTdZ[indexp,0]=np.nan
            dTdZ = dTdZ.where(dTdZ>=1e-3, 1e-3)
            obs['OBS_ERROR'][:]=2.0*dTdZ
        
        # MASKING
        mask = (obs['QC_LEV'] == 1) #* (obs['OBS_ERROR'] <100.)
        print(f'Flattened shape after profile QC: {obs[vartype].where(mask).count().values}')
        
        if (id_obs==obsid_dict['id_s_obs']):
            print('Setting S profile mask')
            mask = mask * (obs[vartype] >= 29) * (obs[vartype] <= 40)
        elif (id_obs==obsid_dict['id_t_obs']):
            print('Setting T profile mask')
            mask = mask * (obs[vartype] > -2.)
        elif (id_obs==obsid_dict['id_sst_obs']):    #'SST'
            print('Setting SST oerr')
            oerr_min=1e-2
            obs['OBS_ERROR'] = obs['OBS_ERROR'].where(obs['OBS_ERROR']>oerr_min, oerr_min)
        elif (id_obs==obsid_dict['id_sss_obs']):
            if (descriptor=='SMAP_L2_SSS' or descriptor=='SMOS_L2_SSS' or descriptor=='Aquarius_L2_SSS' or descriptor=='SMOS_RC_L2_SSS'):
                print(descriptor)
                print('Setting SSS oerr mask')
                obs['OBS_ERROR'] = obs['OBS_ERROR']*smosrampuperr
                oerr_min=0.1
                mask = mask * (obs[vartype] >= 29) * (obs[vartype] <= 40) * (obs['OBS_ERROR']>=oerr_min)
            if (descriptor=='SMAP_L3_SSS' or descriptor=='SMOS_L3_SSS' or descriptor=='Aquarius_L3_SSS'):
                print('L3')
                oerr_min=0.1   # 0.1 psu
                obs['OBS_ERROR'] = obs['OBS_ERROR'].where(obs['OBS_ERROR']>oerr_min, oerr_min)
                #Impose large sigos in the high latitudes from 
                # https://aquarius.umaine.edu/docs/aqsci2015_meissner_2.pdf page 17
                # error map (from v4.0 data) linear from 30-60 max=0.5
                sigo_scale = xr.ones_like(obs['OBS_ERROR'])
                sigo_scale = sigo_scale*0.4*((np.abs(obs['LAT'])-30.)/30.0)
                sigo_scale = sigo_scale.where(sigo_scale<0.4, 0.4)
                sigo_scale = sigo_scale.where(sigo_scale>0.0, 0.0)
                obs['OBS_ERROR'] = sigo_scale + obs['OBS_ERROR']

        elif id_obs==obsid_dict['id_eta_obs']: #'ADT'
            print('Setting ADT oerr mask')
            mask = mask * (np.abs(obs[vartype]) < 10.)
            print(f'Flattened shape after ADT<10: {obs[vartype].where(mask).count().values}')
            mask = mask * (obs['LAT']>-50)
            print(f'Flattened shape after ADT LAT -50: {obs[vartype].where(mask).count().values}')
            mask = mask * (obs['OBS_ERROR']<999)
            oerr_min=0.1   # 0.1 psu
            obs['OBS_ERROR'] = obs['OBS_ERROR'].where(obs['OBS_ERROR']>oerr_min, oerr_min)
            #Impose large sigos in the high latitudes
            sigo_scale=xr.ones_like(obs['OBS_ERROR'])*0.1*(obs['LAT']/90.0)**4
            obs['OBS_ERROR'] = sigo_scale + obs['OBS_ERROR']
            sigo_sigma=obs['OBS_ERROR'].where(mask).std().values
            if (sigo_sigma < .05):
                if BOMB_ON_SIGO == 'True':
                    print ("BAD ADT SIGO FULL STOP ANALYSIS CYCLE")
                    print ('PARENT ID IS', os.getppid(), os.getpid())
            #      create a file to tell parents that obs are bad
                    command='touch '+SCRDIR+'/BADOBS'
                    os.system(command)
                    sys.exit(1)
                elif BOMB_ON_SIGO == 'False':
                    self.no_obs(descriptor = descriptor, platform = platform, color = color, size = markersize, present=False)
                    print(f'BAD ADT SIGO. DROPING {descriptor}')
                    return

        elif (id_obs==obsid_dict['id_aice_obs']):
            print('Setting aice sigo')
            obs['OBS_ERROR'] = 0.05*xr.ones_like(obs[vartype]) #value*0.05

        elif (id_obs==obsid_dict['id_hice_obs']):
            print('Setting hice sigo')
            obs['OBS_ERROR'] = 0.3*xr.ones_like(obs[vartype]) #value*0.05
        
        mask = mask  * (~obs[vartype].isnull()) * (obs['OBS_ERROR']<999.9)

        self.lon   = flatten(obs.LON, mask)
        self.lat   = flatten(obs.LAT, mask)
        self.depth = flatten(obs.DEPTH, mask)
        self.value = flatten(obs[vartype], mask)
        self.instid = flatten(obs.INST_ID, mask)
        self.oerr  = xsigo*flatten(obs.OBS_ERROR, mask)  # Rescaled sigos
        self.typ   = id_obs*np.ones(self.value.shape).astype(int)
        self.descriptor = descriptor
        self.platform = platform
        self.color=color
        self.size=markersize
        
        print(f"obs final flat shape after masking:", self.value.shape)
        print(f'Reading {descriptor} done\n')
        if (len(self.typ)>0):
            self.present=True
        else:
            self.present=False

    def no_obs(self, descriptor='', platform='', color='r',size=10,present=False):
        self.typ   = []
        self.lon   = []
        self.lat   = []
        self.depth = []
        self.value = []
        self.oerr  = []
        self.instid= []
        self.descriptor = descriptor
        self.platform = platform
        self.color=color
        self.size=size
        self.present=False

    def transform(self, transtyp='logit'):
        '''
        Transform the obs and sigo, currently only supports the logit transform ...
        '''
        if self.present:
            if transtyp=='logit':
            #Deal with 0% and 100%
                tiny=1e-3
                self.value[self.value<tiny]=tiny
                self.value[self.value>1.0-tiny]=1.0-tiny

            elif transtyp=='invlogit':
                self.value = np.exp(self.value) / (1 + np.exp(self.value))

    def plot(self, pngname):
        if self.present:
            x, y = self.lon, self.lat
            fig = plt.figure(figsize=(10, 8))
            if ( (self.typ[0]==5351) | (self.typ[0]==5525) | (self.typ[0]==5522) ):
                if (self.typ[0]==5351):
                    valmin=-1.5
                    valmax=1.5
                    errmax=0.25
                if (self.typ[0]==5525):
                    print('plotting SST ....')
                    print('sst max:',np.min(self.lat))
                    valmin=-2.0
                    valmax=31.0
                    errmax=1.0
                if (self.typ[0]==5522):
                    print('plotting SSS ....')
                    print('sss max:',np.min(self.lat))
                    valmin=30.0
                    valmax=38.0
                    if (self.descriptor=='SMAP_L2_SSS'):
                        efact=2.
                    if (self.descriptor=='Aquarius_L2_SSS'):
                        efact=0.5
                    if (self.descriptor=='Aquarius_RIM_L2_SSS'):
                        efact=0.5
                    if (self.descriptor=='SMAP_RIM_L2_SSS'):
                        efact=2.0
                    if (self.descriptor=='SMOS_L2_SSS'):
                        efact=2.0*smosrampuperr
                    if (self.descriptor=='SMOS_RC_L2_SSS'):
                        efact=2.0*smosrampuperr
                    if (self.descriptor=='SMOS_L3_SSS' or self.descriptor=='SMAP_L3_SSS' or self.descriptor=='Aquarius_L3_SSS'):
                        efact=0.5
                    errmax=efact*xsigo_sss
                ax = plt.subplot(2,1,1, projection=ccrs.Mollweide(central_longitude=-80))   
                ax.set_global()
                ax.coastlines()    
                ax.add_feature(cfeature.BORDERS, lw=.5)
                ax.add_feature(cfeature.RIVERS)
                c= ax.scatter(x, y, s=1, c=self.value, transform=ccrs.PlateCarree(),
                            cmap=cm.jet, vmin=valmin, vmax=valmax, edgecolor=None, lw=0)
                fig.colorbar(c, shrink=0.5, ax = ax)

                ax2 = plt.subplot(2,1,2, projection=ccrs.Mollweide(central_longitude=-80))
                ax2.set_global()
                ax2.coastlines()    
                ax2.add_feature(cfeature.BORDERS, lw=.5)
                ax2.add_feature(cfeature.RIVERS)
                ax2.add_feature(cfeature.LAND, facecolor=("coral"))
                c= ax2.scatter(x, y, s=1, c=self.oerr, transform=ccrs.PlateCarree(),
                                  cmap=cm.jet,vmin=0,vmax=errmax,edgecolor=None,lw=0)
                fig.colorbar(c, shrink=0.5, ax=ax2)

            elif ( (self.typ[0]==6000) | (self.typ[0]==6001)):
                valmin=0.
                if (self.typ[0]==6000):
                    valmax=1.0
                if (self.typ[0]==6001):
                    valmax=4.0
                errmax=0.5

                ax = plt.subplot(2,1,1, projection=ccrs.NorthPolarStereo())
                ax.coastlines() 
                ax.set_extent([-180, 180, 55, 90], crs=ccrs.PlateCarree())
                ax.add_feature(cfeature.BORDERS, lw=.5,)
                ax.add_feature(cfeature.RIVERS)
                ax.add_feature(cfeature.LAND, facecolor=("coral"), )
                if logit_transform:
                    c = ax.scatter(x, y, s=1, c=inv_logit(self.value), transform=ccrs.PlateCarree(),
                                cmap=cm.jet,vmin=valmin,vmax=valmax,edgecolor=None,lw=0)
                else:
                    c = ax.scatter(x, y, 1, c=self.value, transform=ccrs.PlateCarree(),
                                cmap=cm.jet,vmin=valmin,vmax=valmax,edgecolor=None,lw=0)

                fig.colorbar(c, shrink=0.5, ax = ax)

                ax2 = plt.subplot(2,1,2, projection=ccrs.SouthPolarStereo())
                ax2.coastlines() 
                ax2.set_extent([-180, 180, -90, -55], crs=ccrs.PlateCarree())
                ax2.add_feature(cfeature.BORDERS, lw=.5)
                ax2.add_feature(cfeature.RIVERS)
                ax2.add_feature(cfeature.LAND, facecolor=("coral"))
                if logit_transform:
                    c = ax2.scatter(x, y, 1, c=inv_logit(self.value), transform=ccrs.PlateCarree(),
                                    cmap=cm.jet,vmin=valmin,vmax=valmax,edgecolor=None,lw=0)
                else:
                    c = ax2.scatter(x, y, 1, c=self.value, transform=ccrs.PlateCarree(),
                                    cmap=cm.jet,vmin=valmin,vmax=valmax,edgecolor=None,lw=0)
                fig.colorbar(c, shrink=0.5, ax = ax2)

            else:
                ax = fig.add_subplot(111, projection=ccrs.Mollweide(central_longitude=-80))
                ax.set_global()
                ax.coastlines()    
                ax.add_feature(cfeature.BORDERS, lw=.5)
                ax.add_feature(cfeature.RIVERS)
                ax.add_feature(cfeature.LAND, facecolor=("coral"))
                ax.plot(x, y, color=self.color, transform=ccrs.PlateCarree(), marker='.', markersize= self.size, linestyle='None',alpha=0.2)

            titlestr=str(len(self.value))+' Obs'
            ax.set_title(titlestr)
            fig.savefig(self.descriptor+pngname)
            plt.clf()

def update_list_of_obs(list_of_obs, obs):
    if obs.present:
        list_of_obs.append(obs)

def no_file_crash(fname, yyyy, mm, dd, hh):
    # Model will bomb if fname or intermediate are not present
    # Because intermediate is generated in the 1st pass, this will crash the model only in the 1st pass
    print(f'Checking required file {fname}')
    intermediate = f'{TSE_TMPDIR}/extracted_{yyyy}{mm}{dd}{hh}_{os.path.basename(fname)}'
    if os.path.isfile(fname) or os.path.isfile(intermediate):
        print('File check pass')
    else:
        print(f"YOU ARE MISSING type {fname} platform")
        print('PARENT ID IS', os.getppid(), os.getpid())
    #   create a file to tell parents that obs are bad
        command='touch '+SCRDIR+'/BADOBS'
        os.system(command)
        sys.exit(1)

# Profiling drifters, moorings, ...
#==================================
def argo(list_of_obs):
    fname = LEV50INSITUOBSDIR+'/ARGO/V3/FINAL/T_ARGO_'+yyyy+'.nc'
    no_file_crash(fname, yyyy, mm, dd, hh) # model bomb if fname or intermediate are not present
    argo_t  = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_t_obs'], vartype='TEMP', xsigo=xsigo_t, color='c', markersize=5, descriptor='Argo-T', NDAYS=EXP_NDAYS)
    fname = LEV50INSITUOBSDIR+'/ARGO/V3/FINAL/S_ARGO_'+yyyy+'.nc'
    no_file_crash(fname, yyyy, mm, dd, hh)
    argo_s  = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_s_obs'], vartype='SALT', xsigo=xsigo_s, color='c', descriptor='Argo-S', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, argo_t)
    update_list_of_obs(list_of_obs, argo_s)
    return list_of_obs

#   5 Randomly generated subsets of Argo 
def argo_1(list_of_obs):  
    argo_t_1  = Obs(yyyy, mm, dd, hh, fname=ERICLEV50INSITUOBSDIR+'/V3/FINAL/T_ARGO_'+yyyy+'_1.nc', id_obs=obsid_dict['id_t_obs'], vartype='TEMP', xsigo=xsigo_t, color='c', markersize=5, descriptor='Argo_1-T', NDAYS=EXP_NDAYS)
    argo_s_1  = Obs(yyyy, mm, dd, hh, fname=ERICLEV50INSITUOBSDIR+'/V3/FINAL/S_ARGO_'+yyyy+'_1.nc', id_obs=obsid_dict['id_s_obs'], vartype='SALT', xsigo=xsigo_s, color='c', descriptor='Argo_1-S', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, argo_t_1)
    update_list_of_obs(list_of_obs, argo_s_1)
    return list_of_obs

def argo_2(list_of_obs):  
    argo_t_2  = Obs(yyyy, mm, dd, hh, fname=ERICLEV50INSITUOBSDIR+'/V3/FINAL/T_ARGO_'+yyyy+'_2.nc', id_obs=obsid_dict['id_t_obs'], vartype='TEMP', xsigo=xsigo_t, color='c', markersize=5, descriptor='Argo_2-T', NDAYS=EXP_NDAYS)
    argo_s_2  = Obs(yyyy, mm, dd, hh, fname=ERICLEV50INSITUOBSDIR+'/V3/FINAL/S_ARGO_'+yyyy+'_2.nc', id_obs=obsid_dict['id_s_obs'], vartype='SALT', xsigo=xsigo_s, color='c', descriptor='Argo_2-S', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, argo_t_2)
    update_list_of_obs(list_of_obs, argo_s_2)
    return list_of_obs

def argo_3(list_of_obs):  
    argo_t_3  = Obs(yyyy, mm, dd, hh, fname=ERICLEV50INSITUOBSDIR+'/V3/FINAL/T_ARGO_'+yyyy+'_3.nc', id_obs=obsid_dict['id_t_obs'], vartype='TEMP', xsigo=xsigo_t, color='c', markersize=5, descriptor='Argo_3-T', NDAYS=EXP_NDAYS)
    argo_s_3  = Obs(yyyy, mm, dd, hh, fname=ERICLEV50INSITUOBSDIR+'/V3/FINAL/S_ARGO_'+yyyy+'_3.nc', id_obs=obsid_dict['id_s_obs'], vartype='SALT', xsigo=xsigo_s, color='c', descriptor='Argo_3-S', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, argo_t_3)
    update_list_of_obs(list_of_obs, argo_s_3)
    return list_of_obs

def argo_4(list_of_obs):  
    argo_t_4  = Obs(yyyy, mm, dd, hh, fname=ERICLEV50INSITUOBSDIR+'/V3/FINAL/T_ARGO_'+yyyy+'_4.nc', id_obs=obsid_dict['id_t_obs'], vartype='TEMP', xsigo=xsigo_t, color='c', markersize=5, descriptor='Argo_4-T', NDAYS=EXP_NDAYS)
    argo_s_4  = Obs(yyyy, mm, dd, hh, fname=ERICLEV50INSITUOBSDIR+'/V3/FINAL/S_ARGO_'+yyyy+'_4.nc', id_obs=obsid_dict['id_s_obs'], vartype='SALT', xsigo=xsigo_s, color='c', descriptor='Argo_4-S', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, argo_t_4)
    update_list_of_obs(list_of_obs, argo_s_4)
    return list_of_obs

def argo_5(list_of_obs):  
    argo_t_5  = Obs(yyyy, mm, dd, hh, fname=ERICLEV50INSITUOBSDIR+'/V3/FINAL/T_ARGO_'+yyyy+'_5.nc', id_obs=obsid_dict['id_t_obs'], vartype='TEMP', xsigo=xsigo_t, color='c', markersize=5, descriptor='Argo_5-T', NDAYS=EXP_NDAYS)
    argo_s_5  = Obs(yyyy, mm, dd, hh, fname=ERICLEV50INSITUOBSDIR+'/V3/FINAL/S_ARGO_'+yyyy+'_5.nc', id_obs=obsid_dict['id_s_obs'], vartype='SALT', xsigo=xsigo_s, color='c', descriptor='Argo_5-S', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, argo_t_5)
    update_list_of_obs(list_of_obs, argo_s_5)
    return list_of_obs

def ctd(list_of_obs):
#    ctd_t   = Obs(yyyy, mm, dd, hh, fname=LEV50INSITUOBSDIR+'/CTD/V3.1/T_CTD_'+yyyy+'.nc', id_obs=obsid_dict['id_t_obs'], vartype='TEMP', xsigo=xsigo_t, color='c', descriptor='CTD-T', NDAYS=EXP_NDAYS)
    ctd_t   = Obs(yyyy, mm, dd, hh, fname=LEV50INSITUOBSDIR+'/CTD/V3/FINAL/T_CTD_'+yyyy+'.nc', id_obs=obsid_dict['id_t_obs'], vartype='TEMP', xsigo=xsigo_t, color='c', descriptor='CTD-T', NDAYS=EXP_NDAYS)
#    ctd_s   = Obs(yyyy, mm, dd, hh, fname=LEV50INSITUOBSDIR+'/CTD/V3.1/S_CTD_'+yyyy+'.nc', id_obs=obsid_dict['id_s_obs'], vartype='SALT', xsigo=xsigo_s, color='c', descriptor='CTD-S', NDAYS=EXP_NDAYS)
    ctd_s   = Obs(yyyy, mm, dd, hh, fname=LEV50INSITUOBSDIR+'/CTD/V3/FINAL/S_CTD_'+yyyy+'.nc', id_obs=obsid_dict['id_s_obs'], vartype='SALT', xsigo=xsigo_s, color='c', descriptor='CTD-S', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, ctd_t)
    update_list_of_obs(list_of_obs, ctd_s)
    return list_of_obs

def xbt(list_of_obs):
#    xbt_t   = Obs(yyyy, mm, dd, hh, fname=LEV50INSITUOBSDIR+'/XBT/V3.1/T_XBT_'+yyyy+'.nc', id_obs=obsid_dict['id_t_obs'], vartype='TEMP', xsigo=xsigo_t, color='c', descriptor='XBT-T', NDAYS=EXP_NDAYS)
    xbt_t   = Obs(yyyy, mm, dd, hh, fname=LEV50INSITUOBSDIR+'/XBT/V3/FINAL/T_XBT_'+yyyy+'.nc', id_obs=obsid_dict['id_t_obs'], vartype='TEMP', xsigo=xsigo_t, color='c', descriptor='XBT-T', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, xbt_t)
    return list_of_obs

def xbt_synS(list_of_obs):
    # print(SYNOBSDIR+'/SYN_XBT_')
    xbt_s   = Obs(yyyy, mm, dd, hh, fname=SYNOBSDIR+'/XBT/SYN_XBT_'+yyyy+'.nc', id_obs=obsid_dict['id_s_obs'], vartype='SALT', xsigo=xsigo_s, color='c', descriptor='XBT-SYN-S', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, xbt_s)
    return list_of_obs

def woa18_atargo(list_of_obs):
    woa18_atargo_t  = Obs(yyyy, mm, dd, hh, fname=WOA18+'/WOA_ARGO/T_WOAatARGO_'+yyyy+'.nc', id_obs=obsid_dict['id_t_obs'], vartype='TEMP', xsigo=xsigo_t, color='c', markersize=5, descriptor='WOA18_atArgo-T', NDAYS=EXP_NDAYS)
    woa18_atargo_s  = Obs(yyyy, mm, dd, hh, fname=WOA18+'/WOA_ARGO/S_WOAatARGO_'+yyyy+'.nc', id_obs=obsid_dict['id_s_obs'], vartype='SALT', xsigo=xsigo_s, color='c', descriptor='WOA18_atArgo-S', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, woa18_atargo_t)
    update_list_of_obs(list_of_obs, woa18_atargo_s)
    return list_of_obs

def woa18_atna(list_of_obs):
    woa18_atna_t= Obs(yyyy, mm, dd, hh, fname=WOA18+'/WOA_NA/T_WOAatNA_'+yyyy+'.nc', id_obs=obsid_dict['id_t_obs'], vartype='TEMP', xsigo=xsigo_t, color='c', markersize=5, descriptor='WOA18_atNA-T', NDAYS=EXP_NDAYS)
    woa18_atna_s= Obs(yyyy, mm, dd, hh, fname=WOA18+'/WOA_NA/S_WOAatNA_'+yyyy+'.nc', id_obs=obsid_dict['id_s_obs'], vartype='SALT', xsigo=xsigo_s, color='c', descriptor='WOA18_atNA-S', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, woa18_atna_t)
    update_list_of_obs(list_of_obs, woa18_atna_s)
    return list_of_obs

def tao(list_of_obs):
#    tao_t   = Obs(yyyy, mm, dd, hh, fname=LEV50INSITUOBSDIR+'/TAO/V3.1/T_TAO_'+yyyy+'.nc',   id_obs=obsid_dict['id_t_obs'], vartype='TEMP', xsigo=xsigo_t, color='r', descriptor='TAO-T', NDAYS=EXP_NDAYS)
    tao_t   = Obs(yyyy, mm, dd, hh, fname=LEV50INSITUOBSDIR+'/TAO/V3/FINAL/T_TAO_'+yyyy+'.nc',   id_obs=obsid_dict['id_t_obs'], vartype='TEMP', xsigo=xsigo_t, color='r', descriptor='TAO-T', NDAYS=EXP_NDAYS)
#    tao_s   = Obs(yyyy, mm, dd, hh, fname=LEV50INSITUOBSDIR+'/TAO/V3.1/S_TAO_'+yyyy+'.nc',   id_obs=obsid_dict['id_s_obs'], vartype='SALT', xsigo=xsigo_s, color='r', descriptor='TAO-S', NDAYS=EXP_NDAYS)
    tao_s   = Obs(yyyy, mm, dd, hh, fname=LEV50INSITUOBSDIR+'/TAO/V3/FINAL/S_TAO_'+yyyy+'.nc',   id_obs=obsid_dict['id_s_obs'], vartype='SALT', xsigo=xsigo_s, color='r', descriptor='TAO-S', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, tao_t)
    update_list_of_obs(list_of_obs, tao_s)
    return list_of_obs

def pirata(list_of_obs):
#    pir_t   = Obs(yyyy, mm, dd, hh, fname=LEV50INSITUOBSDIR+'/PIRATA/V3.1/T_PIR_'+yyyy+'.nc',   id_obs=obsid_dict['id_t_obs'], vartype='TEMP', xsigo=xsigo_t, color='r', descriptor='PIRATA-T', NDAYS=EXP_NDAYS)
    pir_t   = Obs(yyyy, mm, dd, hh, fname=LEV50INSITUOBSDIR+'/PIRATA/V3/FINAL/T_PIR_'+yyyy+'.nc',   id_obs=obsid_dict['id_t_obs'], vartype='TEMP', xsigo=xsigo_t, color='r', descriptor='PIRATA-T', NDAYS=EXP_NDAYS)
#    pir_s   = Obs(yyyy, mm, dd, hh, fname=LEV50INSITUOBSDIR+'/PIRATA/V3.1/S_PIR_'+yyyy+'.nc',   id_obs=obsid_dict['id_s_obs'], vartype='SALT', xsigo=10.0*xsigo_s, color='r', descriptor='PIRATA-S', NDAYS=EXP_NDAYS)
    pir_s   = Obs(yyyy, mm, dd, hh, fname=LEV50INSITUOBSDIR+'/PIRATA/V3/FINAL/S_PIR_'+yyyy+'.nc',   id_obs=obsid_dict['id_s_obs'], vartype='SALT', xsigo=10.0*xsigo_s, color='r', descriptor='PIRATA-S', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, pir_t)
    update_list_of_obs(list_of_obs, pir_s)
    return list_of_obs

def rama(list_of_obs):
#   rama_t  = Obs(yyyy, mm, dd, hh, fname=LEV50INSITUOBSDIR+'/RAMA/V3.1/T_RAMA_'+yyyy+'.nc', id_obs=obsid_dict['id_t_obs'], vartype='TEMP', xsigo=xsigo_t, color='r', descriptor='RAMA-T', NDAYS=EXP_NDAYS)
    rama_t  = Obs(yyyy, mm, dd, hh, fname=LEV50INSITUOBSDIR+'/RAMA/V3/FINAL/T_RAMA_'+yyyy+'.nc', id_obs=obsid_dict['id_t_obs'], vartype='TEMP', xsigo=xsigo_t, color='r', descriptor='RAMA-T', NDAYS=EXP_NDAYS)
#   rama_s  = Obs(yyyy, mm, dd, hh, fname=LEV50INSITUOBSDIR+'/RAMA/V3.1/S_RAMA_'+yyyy+'.nc', id_obs=obsid_dict['id_s_obs'], vartype='SALT', xsigo=10.0*xsigo_s, color='r', descriptor='RAMA-S', NDAYS=EXP_NDAYS)
    rama_s  = Obs(yyyy, mm, dd, hh, fname=LEV50INSITUOBSDIR+'/RAMA/V3/FINAL/S_RAMA_'+yyyy+'.nc', id_obs=obsid_dict['id_s_obs'], vartype='SALT', xsigo=10.0*xsigo_s, color='r', descriptor='RAMA-S', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, rama_t)
    update_list_of_obs(list_of_obs, rama_s)
    return list_of_obs

# SSS satellite
#===================
def aq_L2_sss(list_of_obs):     #Aquarius V5.0
    fname=SatSSSOBSDIR+'L2_AQ_SSS_7.0/V5/'+'SSS_TRK_AQ_V5_'+yyyy+'.nc'
    print('fname is',fname)
#   aq_sss  = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_sss_obs'], vartype='SSS',xsigo=xsigo_sss, descriptor='Aquarius_L3_SSS', color='y')
    aq_sss  = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_sss_obs'], vartype='SSS',xsigo=xsigo_sss, descriptor='Aquarius_L2_SSS', color='y')
    update_list_of_obs(list_of_obs, aq_sss)
#   print 'list_of_obs is ',list_of_obs
    return list_of_obs

# SSS satellite (Aquarius modified with RIM v1 5m salt adjustment)
#===================
def aq_RIM5_L2_sss(list_of_obs):     #Aquarius V5.0 RIM v1 adjusted to 5m
    fname=SatSSSOBSDIR+'L2_AQ_SSS_7.0/V5_RIMV1_5m/'+'SSS_TRK_AQ_V5_RIMV1_5m_'+yyyy+'.nc'
    print ('fname is',fname)
#   aq_sss  = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_sss_obs'], vartype='SSS',xsigo=xsigo_sss, descriptor='Aquarius_L3_SSS', color='y')
    aq_RIM_sss  = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_sss_obs'], vartype='SSS',xsigo=xsigo_sss, descriptor='Aquarius_RIM_L2_SSS', color='y')
    update_list_of_obs(list_of_obs, aq_RIM_sss)
#   print 'list_of_obs is ',list_of_obs
    return list_of_obs

# SSS satellite (SMAP modified with RIM v1 5m salt adjustment)
#=================== default to V4 2015-2018
def smap_RIM5_L2_sss(list_of_obs):     #Aquarius V5.0 RIM v1 adjusted to 5m
    fname=SatSSSOBSDIR+'L2_SMAP_SSS_7.0/V4_RIMV1_5m/'+'SSS_TRK_SMAP_V4_RIMV1_5m_'+yyyy+'.nc'
    print ('fname is',fname)
    smap_RIM_sss  = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_sss_obs'], vartype='SSS',xsigo=xsigo_sss, descriptor='SMAP_RIM_L2_SSS', color='y')
    update_list_of_obs(list_of_obs, smap_RIM_sss)
#   print 'list_of_obs is ',list_of_obs
    return list_of_obs

#=SMAP RIM FROM V5==  v5 2019
def smap_RIM5_L2_sssV5(list_of_obs):     #Aquarius V5.0 RIM v1 adjusted to 5m
    fname=SatSSSOBSDIR+'L2_SMAP_SSS_7.0/V5_RIMV1_5m/'+'SSS_TRK_SMAP_V5_RIMV1_5m_'+yyyy+'.nc'
    print ('fname is',fname)
    smap_RIM_sss  = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_sss_obs'], vartype='SSS',xsigo=xsigo_sss, descriptor='SMAP_RIM_L2_SSS', color='y')
    update_list_of_obs(list_of_obs, smap_RIM_sss)
#   print 'list_of_obs is ',list_of_obs
    return list_of_obs

def smap_L2_sss(list_of_obs):     #SMAP V4.0
    fname=SatSSSOBSDIR+'L2_SMAP_SSS_7.0/V4/'+'SSS_TRK_SMAP_V4_'+yyyy+'.nc'
    print('fname is',fname)
    smap_sss  = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_sss_obs'], vartype='SSS',xsigo=xsigo_sss, descriptor='SMAP_L2_SSS', color='y')
    update_list_of_obs(list_of_obs, smap_sss)
#   print 'list_of_obs is ',list_of_obs
    return list_of_obs

def smap_v4_1_L2_sss(list_of_obs):     #SMAP V4.1  currently only for 6/18-9/18
    fname=SatSSSOBSDIR+'L2_SMAP_SSS_7.0/V4.1/'+'SSS_TRK_SMAP_V4.1_'+yyyy+'.nc'
    print('fname is',fname)
    smap_sss  = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_sss_obs'], vartype='SSS',xsigo=xsigo_sss, descriptor='SMAP_L2_SSS', color='y')
    update_list_of_obs(list_of_obs, smap_sss)
#   print 'list_of_obs is ',list_of_obs
    return list_of_obs

def smap_v4_2_L2_sss(list_of_obs):     #SMAP V4.2  currently only for 2019
    fname=SatSSSOBSDIR+'L2_SMAP_SSS_7.0/V4.2/'+'SSS_TRK_SMAP_V4.2_'+yyyy+'.nc'
    print('fname is correct',fname)
    smap_sss  = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_sss_obs'], vartype='SSS',xsigo=xsigo_sss, descriptor='SMAP_L2_SSS', color='y')
    update_list_of_obs(list_of_obs, smap_sss)
#   print 'list_of_obs is ',list_of_obs
    return list_of_obs

def smap_v4_3_L2_sss(list_of_obs):     #SMAP V4.3
    fname=SatSSSOBSDIR+'L2_SMAP_SSS_7.0/V4.3/'+'SSS_TRK_SMAP_V4.3_'+yyyy+'.nc'
    print('fname is correct',fname)
    smap_sss  = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_sss_obs'], vartype='SSS',xsigo=xsigo_sss, descriptor='SMAP_L2_SSS', color='y')
    update_list_of_obs(list_of_obs, smap_sss)
#   print 'list_of_obs is ',list_of_obs
    return list_of_obs

def smap_v5_0_L2_sss(list_of_obs):     #SMAP V5.0
    fname=SatSSSOBSDIR+'L2_SMAP_SSS_7.0/V5.0/'+'SSS_TRK_SMAP_V5.0_'+yyyy+'.nc'
    print('fname is correct',fname)
    smap_sss  = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_sss_obs'], vartype='SSS',xsigo=xsigo_sss, descriptor='SMAP_L2_SSS', color='y')
    update_list_of_obs(list_of_obs, smap_sss)
#   print 'list_of_obs is ',list_of_obs
    return list_of_obs

def smos_L2_sss(list_of_obs):     #SMOS V3.0
    #fname=SatSSSSMOSOBSDIR+'SSS_TRK_SMOS_L32Q_'+yyyy+'.nc'
    fname = LEV50INSITUOBSDIR + 'L2_SMOS_SSS_7.0/L32Q/SSS_TRK_SMOS_L32Q_'+yyyy+'.nc'
    print('fname is',fname,hh)
    smos_sss  = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_sss_obs'], vartype='SSS',xsigo=xsigo_sss, descriptor='SMOS_L2_SSS', color='y')
#  only grab the 12z data
#   if (hh=='12'):
#     smos_sss  = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_sss_obs'], vartype='SSS',xsigo=xsigo_sss, descriptor='SMOS_L2_SSS', color='y')

    update_list_of_obs(list_of_obs, smos_sss)
#   print 'list_of_obs is ',list_of_obs
    return list_of_obs

def smos_RC_L2_sss(list_of_obs):     #SMOS RAIN CORRECTED V3.0
    #fname=SatSSSSMOSOBSDIR+'SSS_TRK_SMOS_L32Q_RC_'+yyyy+'.nc'
    fname = LEV50INSITUOBSDIR + 'L2_SMOS_SSS_7.0/L32Q/SSS_TRK_SMOS_L32Q_RC'+yyyy+'.nc'
    print('fname is',fname,hh)
    smos_RC_sss  = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_sss_obs'], vartype='SSS',xsigo=xsigo_sss, descriptor='SMOS_RC_L2_SSS', color='y')
    update_list_of_obs(list_of_obs, smos_RC_sss)
#   print 'list_of_obs is ',list_of_obs
    return list_of_obs

def smossub_L2_sss(list_of_obs):     #SMOS V3.0
    fname=SatSSSSMOSOBSDIR+'SSS_TRK_SMOS_L32Q_'+yyyy+'.nc'
#    fname=SatSSSOBSDIR+'L2_SMOS_SSS_7.0/L32Q/'+'SSS_TRK_SMOS_L32Q_'+yyyy+'.nc'
    print('fname is',fname,hh)
    smos_sss  = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_sss_obs'], vartype='SSS',xsigo=xsigo_sss, descriptor='SMOS_L2_SSS', color='y',platform='SMOSSUB')
#  only grab the 12z data
#   if (hh=='12'):
#     smos_sss  = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_sss_obs'], vartype='SSS',xsigo=xsigo_sss, descriptor='SMOS_L2_SSS', color='y')

    update_list_of_obs(list_of_obs, smos_sss)
#   print 'list_of_obs is ',list_of_obs
    return list_of_obs

def aq_L3_sss(list_of_obs):     #Aquarius V5.0
#  ERIC AS IT NOW STANDS IT ONLY LIKES 12z
    fname=SatSSSOBSDIR+'L3_AQ_SSS_7.0/7DAYRUN/'+'SSS_GRD_AQ_'+yyyy+'.nc'
    print('fname is',fname)
    hh12=12  # pick up 12z all day
#   aq_sss  = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_sss_obs'], vartype='SSS',xsigo=xsigo_sss, descriptor='Aquarius_L3_SSS', color='y')
    aq_sss  = Obs(yyyy, mm, dd, hh12, fname=fname, id_obs=obsid_dict['id_sss_obs'], vartype='SSS',xsigo=xsigo_sss, descriptor='Aquarius_L3_SSS', color='y')
    update_list_of_obs(list_of_obs, aq_sss)
#   print 'list_of_obs is ',list_of_obs
    return list_of_obs

#def smap_L3_sss(list_of_obs):     #SMAP V2.0
##  ERIC AS IT NOW STANDS IT ONLY LIKES 12z
#    fname=SatSSSOBSDIR+'L3_SMAP_SSS_7.0/8DAYRUN/'+'SSS_GRD_SMAP_'+yyyy+'.nc'
def smap_L3_sss(list_of_obs):     #SMAP V4.0
#  ERIC AS IT NOW STANDS IT ONLY LIKES 12z
    fname=SatSSSOBSDIR+'L3_SMAP_SSS_7.0/8DAYRUN/'+'SSS_GRD_SMAP_V4_'+yyyy+'.nc'
    print('SMAP L3 fname is',fname)
    hh12=12  # pick up 12z all day
    smap_sss  = Obs(yyyy, mm, dd, hh12, fname=fname, id_obs=obsid_dict['id_sss_obs'], vartype='SSS',xsigo=xsigo_sss, descriptor='SMAP_L3_SSS', color='y')
    update_list_of_obs(list_of_obs, smap_sss)
#   print 'list_of_obs is ',list_of_obs
    return list_of_obs

def smos_L3_sss(list_of_obs):     #SMOS V3.0
#  ERIC AS IT NOW STANDS IT ONLY LIKES 12z
    fname=SatSSSOBSDIR+'L3_SMOS_SSS_7.0/'+'SSS_GRD_SMOS_'+yyyy+'.nc'
    print('SMOS fname is',fname)
    hh12=12  # pick up 12z all day
    smos_sss  = Obs(yyyy, mm, dd, hh12, fname=fname, id_obs=obsid_dict['id_sss_obs'], vartype='SSS',xsigo=xsigo_sss, descriptor='SMOS_L3_SSS', color='y')
    update_list_of_obs(list_of_obs, smos_sss)
#   print 'list_of_obs is ',list_of_obs
    return list_of_obs

# Altimeters
#===================
def cryosat2(list_of_obs):     #CryoSat-2
    fname=ADTOBSDIR+'ADT_TRK_C2_'+yyyy+'.nc'
    c2_adt  = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='CryoSat-2-ADT', color='y')
    update_list_of_obs(list_of_obs, c2_adt)
    return list_of_obs

def cryosat2_N(list_of_obs):     #CryoSat-2-N
    fname=ADTOBSDIR+'ADT_TRK_C2N_'+yyyy+'.nc'
    c2n_adt  = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='CryoSat-2-N-ADT', color='y')
    update_list_of_obs(list_of_obs, c2n_adt)
    return list_of_obs

def sentinel3a(list_of_obs):        #S3A-Sentinel
    fname=ADTOBSDIR+'ADT_TRK_S3A_'+yyyy+'.nc'
    s3a_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='Sentinel-3a-ADT', color='m')
    update_list_of_obs(list_of_obs, s3a_adt)
    return list_of_obs

def sentinel6a(list_of_obs):        #S6A-Sentinel
    fname=ADTOBSDIR+'ADT_TRK_S6A_'+yyyy+'.nc'
    s6a_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='Sentinel-6a-ADT', color='m')
    update_list_of_obs(list_of_obs, s6a_adt)
    return list_of_obs

def jason1(list_of_obs):        #Jason-1
    fname=ADTOBSDIR+'ADT_TRK_J1_'+yyyy+'.nc'
    j1_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='Jason-1-ADT', color='m')
    update_list_of_obs(list_of_obs, j1_adt)
    return list_of_obs

def jason1_N(list_of_obs):        #Jason-1
    fname=ADTOBSDIR+'ADT_TRK_J1N_'+yyyy+'.nc'
    j1n_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='Jason-1-N-ADT', color='m')
    update_list_of_obs(list_of_obs, j1n_adt)
    return list_of_obs

def jason1G(list_of_obs):        #Jason-1
    fname=ADTOBSDIR+'ADT_TRK_J1G_'+yyyy+'.nc'
    j1g_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='Jason-1G-ADT', color='m')
    update_list_of_obs(list_of_obs, j1g_adt)
    return list_of_obs

def jason2(list_of_obs):        #Jason-2
    fname=ADTOBSDIR+'ADT_TRK_J2_'+yyyy+'.nc'
    j2_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='Jason-2-ADT', color='m')
    update_list_of_obs(list_of_obs, j2_adt)
    return list_of_obs

def jason2_N(list_of_obs):        #Jason-2-N
    fname=ADTOBSDIR+'ADT_TRK_J2N_'+yyyy+'.nc'
    j2n_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='Jason-2-N-ADT', color='m')
    update_list_of_obs(list_of_obs, j2n_adt)
    return list_of_obs

def jason3(list_of_obs):        #Jason-3
    fname=ADTOBSDIR+'ADT_TRK_J3_'+yyyy+'.nc'
# ADT_TRK_J3_2017_sles12.nc
#   fname=ADTOBSDIR+'ADT_TRK_J3_'+yyyy+'_sles12_sigo.nc'
    j3_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='Jason-3-ADT', color='m')
    update_list_of_obs(list_of_obs, j3_adt)
    return list_of_obs

def jason3N(list_of_obs):        #Jason-3N
    fname=ADTOBSDIR+'ADT_TRK_J3N_'+yyyy+'.nc'
    j3n_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='Jason-3N-ADT', color='m')
    update_list_of_obs(list_of_obs, j3n_adt)
    return list_of_obs

def saral(list_of_obs):        #Saral/Altica
    fname=ADTOBSDIR+'ADT_TRK_AL_'+yyyy+'.nc'
#    fname=ADTOBSDIR+'ADT_TRK_AL_'+yyyy+'_sles12_sigo.nc'
    al_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='Saral-Altika-ADT', color='m')
    update_list_of_obs(list_of_obs, al_adt)
    return list_of_obs

def ers1(list_of_obs):        #ERS-1
    fname=ADTOBSDIR+'ADT_TRK_E1_'+yyyy+'.nc'
    e1_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='ERS-1-ADT', color='m')
    update_list_of_obs(list_of_obs, e1_adt)
    return list_of_obs

def ers2(list_of_obs):        #ERS-2
    fname=ADTOBSDIR+'ADT_TRK_E2_'+yyyy+'.nc'
    e2_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='ERS-2-ADT', color='m')
    update_list_of_obs(list_of_obs, e2_adt)
    return list_of_obs

def topex_poseidon(list_of_obs):        #TOPEX/POSEIDON
    fname=ADTOBSDIR+'ADT_TRK_TP_'+yyyy+'.nc'
    tp_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='TOPEX-POSEIDON-ADT', color='m')
    update_list_of_obs(list_of_obs, tp_adt)
    return list_of_obs

def topex_poseidon_N(list_of_obs):        #TOPEX/POSEIDON
    fname=ADTOBSDIR+'ADT_TRK_TPN_'+yyyy+'.nc'
    tpn_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='TOPEX-POSEIDON-N-ADT', color='m')
    update_list_of_obs(list_of_obs, tpn_adt)
    return list_of_obs

def geosat_follow_on(list_of_obs):      #GEOSAT follow on
    fname=ADTOBSDIR+'ADT_TRK_G2_'+yyyy+'.nc'
    g2_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='GEOSAT-follow-on-ADT', color='m')
    update_list_of_obs(list_of_obs, g2_adt)
    return list_of_obs

def envisat(list_of_obs):       #Envisat
    fname=ADTOBSDIR+'ADT_TRK_EN_'+yyyy+'.nc'
    en_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='Envisat-ADT', color='m')
    update_list_of_obs(list_of_obs, en_adt)
    return list_of_obs

def envisat_N(list_of_obs):       #Envisat-N
    fname=ADTOBSDIR+'ADT_TRK_ENN_'+yyyy+'.nc'
    enn_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='Envisat-N-ADT', color='m')
    update_list_of_obs(list_of_obs, enn_adt)
    return list_of_obs

def hy2a(list_of_obs):        #HY-2A
    fname=ADTOBSDIR+'ADT_TRK_H2_'+yyyy+'.nc'
    h2_adt = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_eta_obs'], vartype='ADT',xsigo=xsigo_ssh, descriptor='HY-2A-ADT', color='m')
    update_list_of_obs(list_of_obs, h2_adt)
    return list_of_obs

# SST retrieval
#===================
# Need to choose between Reyn, OSTIA and Hadley depending on date <<<<<<<======= Should be on the to do list
def reyn_L3_sst(list_of_obs):
    fname='/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/RC/OBS/SST_6.0/SST_REYN_'+yyyy+'.nc'
    reyn_t = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_sst_obs'], vartype='TEMP',xsigo=xsigo_sst, descriptor='Reynolds-SST', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, reyn_t)
    return list_of_obs

def ostia_L3_sst(list_of_obs):
    fname   = '/discover/nobackup/projects/gmao/m2oasf/aogcm/g5odas/obs/raw/OSTIA/ASSIM_QRT_SUB/'+yyyy+'/SST_OSTIA_'+yyyy+mm+dd+'.nc'
    ostia_t = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_sst_obs'], vartype='TEMP',xsigo=xsigo_sst, descriptor='OSTIA SST', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, ostia_t)
    return list_of_obs

def avhrr18_L2_sst(list_of_obs):
    fname='/discover/nobackup/projects/gmao/m2oasf/aogcm/g5odas/obs/assim/L2_SST_7.0/'+yyyy+'/AVHRR18_G_'+yyyy+mm+dd+'.nc'
    avhrr18 = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_sst_obs'], vartype='TEMP',xsigo=0.1, descriptor='AVHRR18 L2 SST', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, avhrr18)
    return list_of_obs

def noaa16_L2_sst(list_of_obs):
    noaa16 = Obs(yyyy, mm, dd, hh, fname='', id_obs=obsid_dict['id_sst_obs'], vartype='TEMP',xsigo=0.1, descriptor='NOAA-16', platform = 'NOAA16', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, noaa16)
    return list_of_obs

def metopa_L2_sst(list_of_obs):
    metopa = Obs(yyyy, mm, dd, hh, fname='', id_obs=obsid_dict['id_sst_obs'], vartype='TEMP',xsigo=0.1, descriptor='METOP-A', platform = 'METOPA', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, metopa)
    return list_of_obs

def gmi_L2_sst(list_of_obs):
    gmi = Obs(yyyy, mm, dd, hh, fname='', id_obs=obsid_dict['id_sst_obs'], vartype='TEMP',xsigo=0.1, descriptor='GMI-RSS', platform = 'GMI', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, gmi)
    return list_of_obs

def merra2_sst(list_of_obs):
    m2sst = Obs(yyyy, mm, dd, hh, fname='', id_obs=obsid_dict['id_sst_obs'], vartype='TEMP',xsigo=1.0, descriptor='M2-SST', platform = 'M2-SST', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, m2sst)
    return list_of_obs

#def pathfinder(list_of_obs):


# Ice Fraction retrieval
#=======================
def nsidc_aice(list_of_obs):
    fname=OBSDIR+'/AICE_6.0/ICE_NSIDC_'+yyyy+'.nc'
    nsidc_a = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_aice_obs'], vartype='ICE',xsigo=xsigo_aice, descriptor='NASA-TEAM-2', NDAYS=EXP_NDAYS)
    if logit_transform:
        nsidc_a.transform() #logit transform for sea ice fraction
    update_list_of_obs(list_of_obs, nsidc_a)
    return list_of_obs

def reyn_aice(list_of_obs):
    fname=OBSDIR+'/AICE_6.0/ICE_REYN_'+yyyy+'.nc'
    reyn_a = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_aice_obs'], vartype='ICE',xsigo=xsigo_aice, descriptor='NOAA-AICE', NDAYS=EXP_NDAYS)
    if logit_transform:
        reyn_a.transform() #logit transform for sea ice fraction
    update_list_of_obs(list_of_obs, reyn_a)
    return list_of_obs

def ostia_aice(list_of_obs):
    fname   = '/discover/nobackup/projects/gmao/m2oasf/aogcm/g5odas/obs/raw/OSTIA/ASSIM_QRT_SUB/'+yyyy+'/ICE_OSTIA_'+yyyy+mm+dd+'.nc'
    ostia_a = Obs(yyyy, mm, dd, hh, fname=fname, id_obs=obsid_dict['id_aice_obs'], vartype='ICE',xsigo=xsigo_aice, descriptor='OSTIA-AICE', NDAYS=EXP_NDAYS)
    if logit_transform:
        ostia_a.transform() #logit transform for sea ice fraction
    update_list_of_obs(list_of_obs, ostia_a)
    return list_of_obs

# Ice Thickness/Freeboard/Snow depth
#===================================
def oib_hice(list_of_obs):
    oib_hi = Obs(yyyy, mm, dd, hh, fname='', id_obs=obsid_dict['id_hice_obs'], platform='AIR-BORN', xsigo=xsigo_hice, descriptor='OIB', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, oib_hi)
    return list_of_obs

def cs2_hice(list_of_obs):
    cs2_hi = Obs(yyyy, mm, dd, hh, fname='', id_obs=obsid_dict['id_hice_obs'], platform='CS2-HICE', xsigo=xsigo_hice, descriptor='CS2-HICE', NDAYS=EXP_NDAYS)
    update_list_of_obs(list_of_obs, cs2_hi)
    return list_of_obs

switch_obs = {
    "Argo"        : argo,
    "Argo_1"      : argo_1,
    "Argo_2"      : argo_2,
    "Argo_3"      : argo_3,
    "Argo_4"      : argo_4,
    "Argo_5"      : argo_5,  # this is WMO *8, *9 and is the %20 to be withheld
    "CTD"         : ctd,
    "XBT"         : xbt,
    "XBT-SYN-S"   : xbt_synS,
    "WOA18_atArgo": woa18_atargo,
    "WOA18_atNA"  : woa18_atna,
    "TAO"         : tao,
    "PIRATA"      : pirata,
    "RAMA"        : rama,
    "SMOS"        : smos_L2_sss,
    "SMOS_RC"     : smos_RC_L2_sss,
    "SMOSSUB"     : smossub_L2_sss,
    "SMOSL3"      : smos_L3_sss,
    "SMAP"        : smap_L2_sss,
    "SMAPV4.1"    : smap_v4_1_L2_sss,
    "SMAPV4.2"    : smap_v4_2_L2_sss,
    "SMAPV4.3"    : smap_v4_3_L2_sss,
    "SMAPV5.0"    : smap_v5_0_L2_sss,
    "SMAPL3"      : smap_L3_sss,
    "AQUARIUS"    : aq_L2_sss,
    "AQUARIUS_RIM": aq_RIM5_L2_sss,
    "SMAP_RIM"    : smap_RIM5_L2_sss,
    "SMAP_RIMV5"  : smap_RIM5_L2_sssV5,
    "AQUARIUSL3"  : aq_L3_sss,
    "CryoSat-2"   : cryosat2,
    "CryoSat-2-N" : cryosat2_N,
    "Jason-1"     : jason1,
    "Jason-1-N"   : jason1_N,
    "Jason-1G"    : jason1G,
    "Jason-2"     : jason2,
    "Jason-2-N"   : jason2_N,
    "Jason-3"     : jason3,
    "Jason-3N"    : jason3N,
    "Saral"       : saral,
    "Sentinel-3a" : sentinel3a,
    "Sentinel-6a" : sentinel6a,
    "ERS-1"       : ers1,
    "ERS-2"       : ers2,
    "TOPEX"       : topex_poseidon,
    "TOPEX-N"     : topex_poseidon_N,
    "GEOSAT-2"    : geosat_follow_on,
    "Envisat"     : envisat,
    "Envisat-N"   : envisat_N,
    "HY-2A"       : hy2a,
    "Reynolds"    : reyn_L3_sst,
    "OSTIA"       : ostia_L3_sst,
    "M2-SST"      : merra2_sst,
    "AVHRR-18"    : avhrr18_L2_sst,
    "NASA-TEAM-2" : nsidc_aice,
    "NOAA-AICE"   : reyn_aice,
    "OSTIA-AICE"  : ostia_aice,
    "NOAA-16"     : noaa16_L2_sst,
    "METOP-A"     : metopa_L2_sst,
    "GMI-RSS"     : gmi_L2_sst,
    "OIB-HICE"    : oib_hice,
    "CS2-HICE"    : cs2_hice
 }

switch_inst_ids = {
    "TAO"         : 501,
    "PIRATA"      : 502,
    "XBT"         : 503,
    "RAMA"        : 504,
#   "ADCP"        : 505,
#   "Curr Meter"  : 506,
    "Argo"        : 508,
#   "Levitus"     : 509,
    "CTD"         : 513,
#   "Reynolds"    : 516,
    "GEOSAT-2"    : 510,
    "ERS-1"       : 511,
    "Envisat"     : 512,
    "TOPEX"       : 514,
    "Jason-1"     : 515,
    "Jason-2"     : 517,
    "CryoSat-2"   : 532,
    "CryoSat-2-N" : 543,
    "Envisat-N"   : 533,
    "Saral/Altika": 534,
    "Jason-1G"    : 535,
    "ERS-2"       : 536,
    "HY-2A"       : 537,
    "Jason-1-N"   : 538,
    "TOPEX-N"     : 539,
    "Jason-2-N"   : 540,
    "Jason-3"     : 541,
    "Jason-3N"    : 545,
    "Sentinel-3a" : 542,
    "Sentinel-6a" : 544,
    "SMOS"        : 555,
#   "SMOSSUB"     : 555,
#   "SMOSL3"      : 556,
#   "SMAP"        : 556,
#   "SMAPV4.1"    : 556,
#   "SMAPV4.2"    : 556,
#   "SMAPV4.3"    : 556,
    "SMAPV5.0"    : 556,
#   "SMAPL3"      : 551,
    "AQUARIUS"    : 553,
    "AQUARIUSL3"  : 550,
    "Levitus SSS" : 521,
#   "NSIDC AICE"  : 518,
    "NASA-TEAM-2" : 518,
    "CMIP5 AICE"  : 519,
    "CMIP5 SST"   : 520,
    "Reynold AICE": 523,
    "NOAA-AICE"   : 523,
    "OSTIA-AICE"  : 526,
    "PIOMAS HICE" : 528,
    "O1B HICE"    : 527,
    "O1B VICE"    : 529,
#   "MODS Chlor"  : 524,
    "Cryosat HICE": 530,
#   "MODIS_GSFC_A": 600,
#   "MODIS_A"     : 601,
#   "AVHRR18_G"   : 602,
    "XBT-SYN-S"   : 531,
#   "OSTIA"       : ostia_L3_sst,
    "WOA18_atArgo": 557,
    "WOA18_atNA"  : 558,
    "M2-SST"      : 516,
#   "AVHRR-18"    : avhrr18_L2_sst,
#   "NOAA-16"     : noaa16_L2_sst,
#   "METOP-A"     : metopa_L2_sst,
#   "GMI-RSS"     : gmi_L2_sst,
#   "OIB-HICE"    : oib_hice,
#   "CS2-HICE"    : cs2_hice
 }

yyyy     = sys.argv[1]      # '2012'
mm       = sys.argv[2]      # '12'
dd       = sys.argv[3]      # '01'
hh       = sys.argv[4]      # '01'
obs_type = sys.argv[5:]     #

list_of_obs = []
for doobs in obs_type:
    list_of_obs = switch_obs[doobs](list_of_obs)

print('============== Extracting obs ============')
if list_of_obs:
    cnt=0
    for obs in list_of_obs:
        print(obs.descriptor, obs.typ[0])
        pngname='obs-'+mm+'-'+dd+'-'+yyyy
        obs.plot(pngname)
        if (cnt==0):
            typ=obs.typ
            lon=obs.lon
            lat=obs.lat
            depth=obs.depth
            value=obs.value
            oerr=obs.oerr
            instid=obs.instid

            print(np.shape(oerr), np.shape(obs.oerr))
        else:
            typ=np.concatenate( (typ, obs.typ) )
            lon=np.concatenate( (lon,obs.lon) )
            lat=np.concatenate( (lat,obs.lat) )
            depth=np.concatenate( (depth,obs.depth) )
            value=np.concatenate( (value,obs.value) )
            instid=np.concatenate( (instid,obs.instid) )

            print(np.shape(oerr), np.shape(obs.oerr))

            oerr=np.concatenate( (oerr,obs.oerr) )
        del obs
        cnt+=1

#***code to manually fix 180 data ***************************
    for i in range (0,len(lon)):
        if lon[i]==180.:
             lon[i]=-179.9999
        if lon[i]==-180.:
             lon[i]=-179.9999
#************************************************************

    nobs=len(typ)
    print('nobs=',nobs)
#   with open('Nobs', 'wb') as fh:
    with open('Nobs', 'w') as fh:
        fh.write(str(nobs)+'\n')

#   check to make sure minimum observation types are in gmao- file
    minreq=[3073, 5521, 5351]  #Tz, Sz, ADT
#   minreq=[3073, 5351]  #Tz, ADT
#   minreq=[3073]  #Tz,
#   minreq[3073, 5521]  #Tz, Sz
#   minreq=[5351]  #ADT

    for oid in minreq:
        if oid in typ:
            print("Yes,found in List : ",oid, inv_dict(obsid_dict, oid))
        else:
            print(f"YOU ARE MISSING type {oid} obs, {inv_dict(obsid_dict, oid)}")
            print('PARENT ID IS', os.getppid(), os.getpid())

    #   create a file to tell parents that obs are bad
            command='touch '+SCRDIR+'/BADOBS'
            os.system(command)
            sys.exit(1)


    fnameout='gmao-obs-'+yyyy+mm+dd+'.nc'
    ncfile = Dataset(fnameout,'w')
    ncfile.createDimension('nobs',nobs)

    tmp = ncfile.createVariable('typ',np.dtype('int32').char,('nobs'))
    tmp[:] = typ

    tmp = ncfile.createVariable('lon',np.dtype('float32').char,('nobs'))
    tmp[:] = lon

    tmp = ncfile.createVariable('lat',np.dtype('float32').char,('nobs'))
    tmp[:] = lat

    tmp = ncfile.createVariable('depth',np.dtype('float32').char,('nobs'))
    tmp[:] = depth

    tmp = ncfile.createVariable('value',np.dtype('float32').char,('nobs'))
    tmp[:] = value

    tmp = ncfile.createVariable('oerr',np.dtype('float32').char,('nobs'))
    tmp[:] = oerr

    tmp = ncfile.createVariable('instid',np.dtype('float32').char,('nobs'))
    tmp[:] = instid

    ncfile.close()
    print('Saved ',str(nobs),'obs in ',fnameout)

#   now replace with the proper gmao- file  DANGEROUS
#    assumes you will be running in the same directory tree
#    (i.e. eh0??/ocean_das/oana-YYYYMMDD_HH/ocean_observer_YYYYMMDD_HH)
    use_old_obs = os.environ['ODAS_USE_OLD_OBS']
    if (use_old_obs=='True'):
        use_old_obs = True
    else:
        use_old_obs = False
#   if(os.environ['ODAS_USE_OLD_OBS']):
    if( use_old_obs == True ):
        print('TRYING TO REPLACE OBS WITH OLD VERSION')
        ODAS_DIR_OLD_OBS = os.environ['ODAS_DIR_OLD_OBS']
        currentpwd = os.getcwd ()
#       lastnode=os.path.split(currentpwd)[-1]
        path_list = currentpwd.split(os.sep)
        lastnode = path_list[-1]
        lastnode2 = path_list[-2]
        checkFile=ODAS_DIR_OLD_OBS+lastnode2+'/'+lastnode+'/'+fnameout
        print(checkFile)
        if os.path.isfile(checkFile):
            command = 'mv '+fnameout+' '+fnameout+'.didnotuse'
            print(command)
            os.system(command)
            command = 'mv Nobs Nobs.didnotuse'
            print(command)
            os.system(command)
#   DANGEROUS!!!!!!!!!!!!!!!!!
            print('***REPLACING OBS WITH',ODAS_DIR_OLD_OBS+lastnode2+'/'+lastnode+'/'+fnameout,'**********')
#          command='cp '+ODAS_DIR_OLD_OBS+lastnode2+'/'+lastnode+'/'+fnameout+' '+fnameout  3/16/21 from Yehui
            command='cp -f '+ODAS_DIR_OLD_OBS+lastnode2+'/'+lastnode+'/'+fnameout+' '+fnameout
            print(command)
            os.system(command)
#          command='cp '+ODAS_DIR_OLD_OBS+lastnode2+'/'+lastnode+'/Nobs Nobs' 3/16/21 from Yehui
            command='cp -f '+ODAS_DIR_OLD_OBS+lastnode2+'/'+lastnode+'/Nobs Nobs'
            print(command)
            os.system(command)
#  now check  to make sure it has instid, if not add a variable instid with 0's
#######################################
            infile=fnameout
            outfile='tempout.nc'
            cmd = "ncdump -v instid "+infile
#          print cmd
            returned_value = os.system(cmd)
#          print 'return is',returned_value
            if (returned_value != 0):
#                cmd = "/discover/swdev/gmao_SIteam/Baselibs/latest-mpiuni-SLES12/Linux/bin/ncap2 -s 'instid[$nobs]=-999.' "+infile+' '+outfile
                # cmd = f"/discover/swdev/gmao_SIteam/Baselibs/latest-mpiuni-SLES{SLN}/Linux/bin/ncap2 -s 'instid[$nobs]=-999.' "+infile+' '+outfile
                cmd = f"/discover/swdev/gmao_SIteam/Baselibs/latest-mpiuni-SLES15/Linux/bin/ncap2 -s 'instid[$nobs]=-999.' "+infile+' '+outfile
#                  print cmd
                print('ADDING INST_ID TO NETCDF FILE')
                os.system(cmd)
                cmd = "mv "+outfile+' '+fnameout
                os.system(cmd)
#######################################

        else:
            print('SORRY', checkFile, 'DOES NOT EXIST')
else:
#    with open('Nobs', 'wb') as fh:
    with open('Nobs', 'w') as fh:
        fh.write('0'+'\n')
try:
    command = './oceanobs_nc2bin.x -y '+yyyy+' -m '+mm+' -d '+dd+' -indir1 gmao-obs- -outdir .'
    print(command)
    os.system(command)
except Exception as err:
    print(f"Unexpected {err}, {type(err)}")
    print('Could not convert to NCEP binary format')
