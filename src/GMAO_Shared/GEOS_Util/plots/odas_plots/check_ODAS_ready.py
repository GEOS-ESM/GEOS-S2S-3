#! /usr/bin/env python

#    plot_odas_obsfile.py
# * Args:
# *              directory      : directory to  find obs-YYYYMMDD_HH.nc files
# *              yyyys start    : 4 digit year
# *              mms   start    : 2 digit month
# *              dds   start    : 2 digit day
# *              hhs   start    : 2 digit hour
# *              yyyye end      : 4 digit year
# *              mme   end      : 2 digit month
# *              dde   end      : 2 digit day
# *              hhe   end      : 2 digit hour
# *              obs_type       : Entered as 'SZ, TZ '
# *              lats  south    : 3 digit latitude
# *              latn  north    : 3 digit latitude
# *              lonw  west     : 4 digit longitude
# *              late  east     : 4 digit longitude
# *              levtop         : 4 digit top level (close to the surface)
# *              levbot         : 4 digit bot level (close to bottom)
# *              sensor         : what sensor to plot (TZ,SZ - Argo, XBT, TAO, RAMA, PIRATA, CTD, ALL))

#import matplotlib
#matplotlib.use('Agg')
import matplotlib.pyplot as plt
import sys
import glob
import numpy as np
import array as arr
from netCDF4 import Dataset
from datetime import date
import datetime
import matplotlib.dates as mdates
import matplotlib.cm as cm
import os

switch_obs = {
     'SZ'   : 5521, # Salinity
     'TZ'   : 3073, # Temperature
     'SST'  : 5525, # SST
     'SSS'  : 5522, # Sea surface Salinity
     'SSH'  : 5351, # SSH
     'AICE' : 6000, # AICE
     'HICE' : 6001  # HICE
     }
#

switch_instids = {
    'TAO'         : 501,  #
    'PIRATA'      : 502,  #
    'XBT'         : 503,  #
    'RAMA'        : 504,  #
    'Argo'        : 508,  #
    'CTD'         : 513, #
    'GEOSAT-2'    : 510,
    'ERS-1'       : 511,
    'Envisat'     : 512,
    'TOPEX'       : 514,
    'Jason-1'     : 515,
    'Jason-2'     : 517,
    'CryoSat-2'   : 532,
    'Envisat-N'   : 533,
#   'Saral/Altika': 534,
    'Saral'       : 534,
    'Jason-1G'    : 535,
    'ERS-2'       : 536,
    'HY-2A'       : 537,
    'Jason-1-N'   : 538,
    'TOPEX-N'     : 539,
    'Jason-2-N'   : 540,
    'Jason-3'     : 541,
    'Sentinel-3a' : 542,
    'Sentinel-6a' : 544,
    'Sentinel-3b' : 547,
    'SWOTN'       : 546,
    'Jason-3N'    : 545,
    'CryoSat-2-N' : 543,
    'SMOS'        : 555,
#   'SMOSSUB'     : 555,
#   'SMOSL3'      : 556,
#   'SMAP'        : 556,
#   'SMAPV4.1'    : 556,
#   'SMAPV4.2'    : 556,
#   'SMAPV4_3'    : 556,
    'SMAPV5_0'    : 556,
#   'SMAPL3'      : 551,
    'AQUARIUS'    : 553,
    'AQUARIUSL3'  : 550,
    'Levitus SSS' : 521,
#   'NSIDC AICE'  : 518,
    'NASA-TEAM-2' : 518,
    'CMIP5 AICE'  : 519,
    'CMIP5 SST'   : 520,
    'Reynold AICE': 523,
    'NOAA-AICE'   : 523,
    'OSTIA-AICE'  : 526,
    'PIOMAS HICE' : 528,
    'O1B HICE'    : 527,
    'O1B VICE'    : 529,
#   'MODS Chlor'  : 524,
    'Cryosat HICE': 530
#   'MODIS_GSFC_A': 600,
#   'MODIS_A'     : 601,
#   'AVHRR18_G'   : 602,
#   'XBT-SYN-S'   : xbt_synS,
#   'OSTIA'       : ostia_L3_sst,
#   'M2-SST'      : merra2_sst,
#   'AVHRR-18'    : avhrr18_L2_sst,
#   'NOAA-16'     : noaa16_L2_sst,
#   'METOP-A'     : metopa_L2_sst,
#   'GMI-RSS'     : gmi_L2_sst,
#   'OIB-HICE'    : oib_hice,
#   'CS2-HICE'    : cs2_hice
    }


switchback_instids = {
     501        :  'TAO',
     502        :  'PIRATA',
     503        :  'XBT',  #
     504        :  'RAMA',  #
     508        :  'Argo',  #
     513        :  'CTD', #
     510        :  'GEOSAT-2',
     511        :  'ERS-1',
     512        :  'Envisat',
     514        :  'TOPEX',
     515        :  'Jason-1',
     517        :  'Jason-2',
     535        :  'Jason-1G',
     532        :  'CryoSat-2',
     543        :  'CryoSat-2-N',
     533        :  'Envisat-N',
     534        :  'Saral',
     536        :  'ERS-2',
     537        :  'HY-2A',
     538        :  'Jason-1-N',
     539        :  'TOPEX-N',
     540        :  'Jason-2-N',
     541        :  'Jason-3',
     542        :  'Sentinel-3a',
     555        :  'SMOS',
     544        :  'Sentinel-6a',
     546        :  'SWOTN',
     547        :  'Sentinel-3b',
     545        :  'Jason-3N',
#    556        :  'SMAPV4.3',
     556        :  'SMAPV5_0',
     553        :  'AQUARIUS',
     518        :  'NASA-TEAM-2'
    }



def update_list_of_sensors(list_of_sensors, sensor):
    if sensor.present:
        list_of_sensor.append(sensor)

def da_window(yyyys, mms, dds, hhs, yyyye,mme,dde,hhe, OBSDATE):
    """
    This function exctract the observations that are in [yyyymmdd_hh-NDAYS, yyyymmdd_hh+NDAYS]
    Args:
        yyyy (int)     : 4 digit year
          mm (int)     : 2 digit month
          dd (int)     : 2 digit day
          hh (int)     : 2 digit hour
          OBSDATE (int): Observation date in a YYYYMMDDHH format

    Returns:
        Array of indices that corresponds to the observations that are within the window
    """

    #Lower bound for obs time
    obs_date_min=datetime.datetime(int(yyyys), int(mms), int(dds), int(hhs))
    yyyyo=str(obs_date_min.year)
    mmo=str(obs_date_min.month).zfill(2)
    ddo=str(obs_date_min.day).zfill(2)
    hho=str(obs_date_min.hour).zfill(2)

    #Upper bound for obs time
    obs_date_max=datetime.datetime(int(yyyye), int(mme), int(dde), int(hhe))
    yyyye=str(obs_date_max.year)
    mme=str(obs_date_max.month).zfill(2)
    dde=str(obs_date_max.day).zfill(2)
    hhe=str(obs_date_max.hour).zfill(2)

    try:
        return list(np.squeeze(np.where( (OBSDATE>=int(yyyyo+mmo+ddo+hho)) & (OBSDATE<=int(yyyye+mme+dde+hhe)) )))
    except:
        return []


    return data

def count_single_sensor(instid_list, obstyp, obs, target_instid, required_obstyp=None):
    count = 0
    for i in range(len(obs)):
        if instid_list[i] == target_instid:
            if required_obstyp is None or obstyp == required_obstyp:
                count += 1
    return count




#if len(sys.argv) >= 1:

#    obstyp   = sys.argv[1]      # 'TZ'
    
#yyyy = '201[8-9]'
#yyyy = '202[4-5]'
#yyyy = '2025'
#year = '????'
#mm   = '??'
#dd   = '??'
hh   = '12'

expdir=os.environ.get('EXPDIR')

with open(f'{expdir}/incr_date', "r") as file: 
     content = file.read()
obsdatebeg=content[0:8]

with open(f'{expdir}/cap_restart', "r") as file:
     content = file.read()
obsdateend=content

#flist=glob.glob('ocean_obs*/obs-*.nc')
#directory = '/gpfsm/dnb42/projects/p17/production/geos5/exp/S2S-2_1_ANA_002/ocean_das/oana-*/ocean_obs*/'
directory = f'/gpfsm/dnb07/projects/p236/GiOcean-NRT/ocean_das/oana-{obsdatebeg}_00/ocean_obs*/' 
flist=glob.glob(str(directory)+'obs-*_'+hh+'.nc')
flist.sort()
#    print 'directory is ', directory

#    print(flist)

#    obsdatebeg=datetime.datetime(int(yyyys),int(mms),int(dds),int(hhs))
#    obsdateend=datetime.datetime(int(yyyye),int(mme),int(dde),int(hhe))
    #print obsdatebeg, obsdateend
tz_count  = np.full(len(flist),np.nan)
sz_count  = np.full(len(flist),np.nan)
ssh_count = np.full(len(flist),np.nan)
sss_count = np.full(len(flist),np.nan)
nfound = 0
ymdh=[]
for n in range(len(flist)):
   fname=flist[n]
   print(fname)
 
   ymdh.append(datetime.datetime(int(fname[-30:-26]), int(fname[-26:-24]), int(fname[-24:-22])))
   sdate = str(ymdh[-1])
   sdate = sdate[0:10]
   nfound=nfound+1
 #           print 'found', fname,obsdate,obsdatebeg,obsdateend
   ncfile = Dataset(fname, 'r')
   nvar = len (list(ncfile.variables))
   
   obsid=ncfile.variables['OBSID'][:]
   lon=ncfile.variables['lon'][:]
   lat=ncfile.variables['lat'][:]
   obs=ncfile.variables['obs'][:]
   lev=ncfile.variables['lev'][:]

   if nfound == 0:
       print('NO DATA FOUND IN TIME RANGE',sdate)
       exit()



   I=np.where(lev<=300)
   lon=lon[I]
   lat=lat[I]
   obsid=obsid[I]
   obs=obs[I]
   lev=lev[I]
#   print(lev)
#    instid=instid[I]
   

   idx = np.where(obsid == 3073)
   tz_count[n] = np.size((idx))

   idx = np.where(obsid == 5521) 
   sz_count[n] = np.size(idx)

   idx = np.where(obsid == 5351)
   ssh_count[n] = np.size((idx))
  
   idx = np.where(obsid == 5522)
   sss_count[n]= np.size(idx)

tz_min = 1759
tz_max = 4309

sz_min = 1338
sz_max = 3456

sss_min = 111216/2
sss_max = 218318


if (
    (np.median(tz_count) >= tz_min and np.median(tz_count) < tz_max) and
    (np.median(sz_count) >= sz_min and np.median(sz_count) < sz_max) and
    (np.median(sss_count) >= sss_min and np.median(sss_count) < sss_max)
):
    with open("data_ODAS_ready", "w") as file:
         file.write(obsdateend)
         
