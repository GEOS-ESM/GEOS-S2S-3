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
# *              obs_type       : Entered as 'SZ, TZ, SSS, SSH, AICE, HICE '
# *              levtop         : 4 digit top level (close to the surface)
# *              levbot         : 4 digit bot level (close to bottom)
# *              whatparm       : what parameter to plot (obs, oma, omf, omf-oma)
# *              sensor         : what sensor to plot (TZ,SZ - Argo, XBT, TAO, RAMA, PIRATA, CTD, ALL), SSS (AQUARIUS, SMAP, SMOS), SSH (CryoSat-2, Jason-1,Jason-2,Jason-3, Saral, Sentinel-3a, ERS-1, ERS-2, TOPEX, GEOSAT-2, Envisat, HY-2A)

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import sys
import glob
import numpy as np
from netCDF4 import Dataset
from datetime import date
import datetime
import matplotlib.cm as cm
from mpl_toolkits.basemap import Basemap

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
#   'Jason-1G     : 535,
    'ERS-2'       : 536,
    'HY-2A'       : 537,
    'Jason-1-N'   : 538,
    'TOPEX-N'     : 539,
    'Jason-2-N'   : 540,
    'Jason-3'     : 541,
    'Sentinel-3a' : 542,
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
     532        :  'CryoSat-2',
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

if len(sys.argv) > 14:

    directory= sys.argv[1]      # 'ocean_obs\*/'
    yyyys    = sys.argv[2]      # '2012'
    mms      = sys.argv[3]      # '12'
    dds      = sys.argv[4]      # '01'
    hhs      = sys.argv[5]      # '00'
    yyyye    = sys.argv[6]      # '2012'
    mme      = sys.argv[7]      # '12'
    dde      = sys.argv[8]      # '05'
    hhe      = sys.argv[9]      # '01'
    obstyp   = sys.argv[10]      # 'TZ'
    levtop   = sys.argv[11]      # '0'
    levbot   = sys.argv[12]      # '50'
    whatparm = sys.argv[13]      # 'oma','omf','obs'
    sensor   = sys.argv[14:]     # 'Argo', 'XBT', etc.

    sensor2  = sensor
#       print 'sensor2 is', sensor2

    list_of_obs = switch_obs[obstyp]

    mme=str(mme).zfill(2)
    mms=str(mms).zfill(2)
    dde=str(dde).zfill(2)
    dds=str(dds).zfill(2)

    list_of_sensors = []
    senstit=''
    print 'sensor,obstyp',sensor, obstyp
    if(sensor[0] == 'ALL' and obstyp == 'SSH'):
        ALLsensor=['CryoSat-2', 'Jason-1','Jason-2','Jason-3', 'Saral', 'Sentinel-3a', 'ERS-1', 'ERS-2', 'TOPEX', 'GEOSAT-2', 'Envisat', 'HY-2A']
        sensor2=ALLsensor
#               print 'sensor is from ALL',ALLsensor
    if(sensor[0] == 'ALL' and obstyp == 'TZ'):
        ALLsensor=['Argo','TAO','PIRATA', 'RAMA', 'CTD','XBT']
        sensor2=ALLsensor
#               print 'sensor is from ALL',ALLsensor
    if(sensor[0] == 'ALL' and obstyp == 'SZ'):
        ALLsensor=['Argo','TAO','PIRATA', 'RAMA', 'CTD']
        sensor2=ALLsensor
#               print 'sensor is from ALL',ALLsensor
    if(sensor[0] == 'ALL' and obstyp == 'SSS'):
#       ALLsensor=['SMAPV4.3','SMAPV5.0','SMOS','AQUARIUS']
        ALLsensor=['SMAPV5_0','SMOS','AQUARIUS']
        sensor2=ALLsensor
#               print 'sensor is from ALL',ALLsensor
    for doobs in sensor2:
#          print 'sensor is', sensor, doobs
#          print 'output is ', switch_instids[doobs]
        list_of_sensors.append(switch_instids[doobs])
        senstit=senstit+doobs

#       print 'HERE', list_of_sensors
#       print 'senstit',senstit
    print list_of_obs
    #print obs_date_min
    #print obs_date_max
    print 'levtop, levbot', levtop, levbot

    #flist=glob.glob('ocean_obs*/obs-*.nc')
    flist=glob.glob(str(directory)+'obs-*.nc')
    flist.sort()

    print flist

    obsdatebeg=datetime.datetime(int(yyyys),int(mms),int(dds),int(hhs))
    obsdateend=datetime.datetime(int(yyyye),int(mme),int(dde),int(hhe))
    #print obsdatebeg, obsdateend

    nfound=0
    for n in range(len(flist)):
        fname=flist[n]
    #       print 'n is', n, fname
    #       print fname
    #  obs-20150807_12.nc
        hho=str(fname[-5:-3])
        ddo=str(fname[-8:-6])
        mmo=str(fname[-10:-8])
        yyyyo=str(fname[-14:-10])
    #       print yyyyo, mmo, ddo, hho
        obsdate = datetime.datetime(int(yyyyo),int(mmo),int(ddo),int(hho))
        if((obsdate >= obsdatebeg) and (obsdate <= obsdateend)):
            nfound=nfound+1
            print 'found', fname,obsdate,obsdatebeg,obsdateend
            ncfile = Dataset(fname, 'r')
    #        if(n==0):
            if(nfound==1):
                obsid=ncfile.variables['OBSID'][:]
                lon=ncfile.variables['lon'][:]
                lat=ncfile.variables['lat'][:]
                omf=ncfile.variables['omf'][:]
                obs=ncfile.variables['obs'][:]
                oma=ncfile.variables['oma'][:]
                lev=ncfile.variables['lev'][:]
                instid=ncfile.variables['instid'][:]
            else:
                obsid1=ncfile.variables['OBSID'][:]
                obsid=np.concatenate((obsid,obsid1),axis=0)
                lon1=ncfile.variables['lon'][:]
                lon=np.concatenate((lon,lon1),axis=0)
                lat1=ncfile.variables['lat'][:]
                lat=np.concatenate((lat,lat1),axis=0)
                omf1=ncfile.variables['omf'][:]
                omf=np.concatenate((omf,omf1),axis=0)
                obs1=ncfile.variables['obs'][:]
                obs=np.concatenate((obs,obs1),axis=0)
                oma1=ncfile.variables['oma'][:]
                oma=np.concatenate((oma,oma1),axis=0)
                lev1=ncfile.variables['lev'][:]
                lev=np.concatenate((lev,lev1),axis=0)
                instid1=ncfile.variables['instid'][:]
                instid=np.concatenate((instid,instid1),axis=0)

    #               print np.size(obsid)
    #                       print obsid
    #       else:
    #               print 'NOT FOUND'
    #   limit the data to correct variable
    #I=np.where(obsid==3073)

    if nfound == 0:
        print 'NO DATA FOUND IN TIME RANGE',obsdatebeg,obsdateend
        exit()

    I=np.where(obsid==list_of_obs)
    lon=lon[I]
    lat=lat[I]
    obsid=obsid[I]
    obs=obs[I]
    oma=oma[I]
    omf=omf[I]
    lev=lev[I]
    instid=instid[I]

    print 'length of instid is',len(instid),'after sensor trimming'
#       print instid

#  now limit the data to the sensors  (i.e. Argo, xbt, etc.)
    goodsens=np.zeros(len(instid))
#       print 'list_of_sensors', list_of_sensors
    for j in range(0,len(instid)):
        for sens in list_of_sensors:     #  loop over sensors
#               print 'LOOP', sens,j
            if(instid[j]==sens):
                goodsens[j]=1
#       print 'goodsens is', goodsens

    I=np.where(goodsens==1)
    lon=lon[I]
    lat=lat[I]
    obsid=obsid[I]
    obs=obs[I]
    oma=oma[I]
    omf=omf[I]
    lev=lev[I]
    instid=instid[I]

    uniquevalues = []

    senstit2 = ''
# traversing the list
    for i in instid:
# check unique valueis present or not
        if i not in uniquevalues:
            uniquevalues.append(i)
    print 'uniquevalues',uniquevalues
    for i in uniquevalues:
#           senstit2.append(switchback_instids[i])
        tit2=switchback_instids[i]
        senstit2=senstit2+tit2
    print 'senstit2 is ',senstit2
#       print 'senstit is ',senstit

    if len(obsid) == 0:
        print 'THIS OBS NOT FOUND',list_of_obs
        exit()

    #  limit the data to levels

    I=np.where((lev>=int(levtop)) & (lev<=int(levbot)))
    lon=lon[I]
    lat=lat[I]
    obsid=obsid[I]
    obs=obs[I]
    oma=oma[I]
    omf=omf[I]
    lev=lev[I]
    instid=instid[I]

    omfmoma=np.zeros(len(omf))
    for k in range(len(omf)):
        omfmoma[k]=abs(omf[k])-abs(oma[k])

#       print 'omf-oma is ', omfmoma[1:30]
#       print 'omf is ', omf[1:30]
#       print 'oma is ', oma[1:30]

    if len(obsid) == 0:
        print 'NO DATA FOUND DEPTHS',levtop,levbot
        exit()

    #print obsid
    #print obs
    #print lev

    #whatparm='omf'
    #whatparm='obs'
    #whatparm='oma'
    if (whatparm != 'obs' and whatparm != 'omf' and whatparm != 'oma' and whatparm != 'omf-oma'):
        print 'THIS PARMAMETER NOT (omf,oma,obs,omf-oma) FOUND',whatparm
        exit()
    print 'plotting ',whatparm

    if(whatparm=='obs'):
        var=obs
    if(whatparm=='omf'):
        var=omf
    if(whatparm=='oma'):
        var=oma
    if(whatparm=='omf-oma'):
        var=omfmoma

    #  now do a mean on the 1x1 grid
    gridout=np.zeros((360,180,2))
    for k in range(len(var)):
        ilon=int(lon[k]+180)
        ilat=int(lat[k]+90)
#   print var[k],ilon,ilat
        gridout[ilon,ilat,0]=gridout[ilon,ilat,0]+var[k]
        gridout[ilon,ilat,1]=gridout[ilon,ilat,1]+1.
#          print gridout[ilon,ilat,0],var[k],ilon,ilat,k,lon[k],lat[k]

#       print gridout
    ic=0
    gridout2=[]
    lon2=[]
    lat2=[]
    for i in range(0,359):
        for j in range(0,179):
#           print gridout[i,j,1],i,j
            if( gridout[i,j,1] > 0.):
                gridout2.append(gridout[i,j,0]/gridout[i,j,1])
                lonout=float(i)-180.
                latout=float(j)-90.
                lon2.append(lonout)
                lat2.append(latout)
#             print gridout2[ic],lonout,latout,ic, i,j
                ic=ic+1
    ssize=5*np.ones(len(gridout2))
#       print ssize
    fig = plt.figure(num=1, figsize=(10,8), facecolor='w')
    fig.add_subplot(111)
    map = Basemap(projection='moll', llcrnrlat=-90, urcrnrlat=90,llcrnrlon=-180, urcrnrlon=180, resolution='c', lon_0=-80)
    x, y = map(lon2, lat2)
    if ( (obsid[0]==5351) | (obsid[0]==5525) | (obsid[0]==5522) ):
        if (obsid[0]==5351):
            if(whatparm=='obs'):
                valmin=-1.5
                valmax=1.5
                descriptor='SSH'
                errmax=0.25
            if( (whatparm=='oma') | (whatparm=='omf') ):
                valmin=-0.5
                valmax=0.5
                descriptor='SSH'
                errmax=0.25
            if(whatparm=='omf-oma'):
                valmin=-0.02
                valmax=0.02
                descriptor='SSH'
                errmax=0.25
        if (obsid[0]==5525):
            print 'plotting SST ....'
            print 'sst max:',np.min(lat)
            descriptor='SST'
            valmin=-2.0
            valmax=31.0
            errmax=1.0
        if (obsid[0]==5522):
            print 'plotting SSS ....'
            print 'sss max, min:',np.max(gridout2),np.min(gridout2)
            descriptor='SSS'
            if(whatparm=='obs'):
                valmin=30.0
                valmax=38.0
            if(whatparm=='omf'):
                valmin=-2.0
                valmax=2.0
            if(whatparm=='oma'):
                valmin=-2.0
                valmax=2.0
            if(whatparm=='omf-oma'):
                valmin=-0.1
                valmax=0.1

        plt.subplot(211)
        map.drawcoastlines()
        map.drawcountries()
        map.drawmapboundary()
#           print 'here1'
        map.scatter(x, y, 1, c=gridout2,cmap=cm.jet,vmin=valmin,vmax=valmax,edgecolor=None,lw=0,alpha=1)
        plt.colorbar(shrink=0.5)

    elif ( (obsid[0]==6000) | (obsid[0]==6001)):
        if (obsid[0]==6000):
            if(whatparm=='obs'):
                valmin=0.
                valmax=1.0
            if(whatparm=='omf'):
                valmin=-0.2
                valmax=0.2
            descriptor='AICE'
        errmax=0.5
        map = Basemap(projection='npstere',lon_0=0,boundinglat=55, resolution='c')
        x, y = map(lon, lat)
        plt.subplot(211)
        map.drawcoastlines()
        map.drawcountries()
        map.fillcontinents(color='coral')
        map.drawmapboundary()
        if logit_transform:
            print 'here2'
            map.scatter(x, y, 1, c=inv_logit(gridout2),cmap=cm.jet,vmin=valmin,vmax=valmax,edgecolor=None,lw=0)
        else:
            print 'here3'
            map.scatter(x, y, 1, c=gridout2,cmap=cm.jet,vmin=valmin,vmax=valmax,edgecolor=None,lw=0)
        plt.colorbar(shrink=0.5)

        map = Basemap(projection='spstere',lon_0=0,boundinglat=-55, resolution='c')
        x, y = map(lon, lat)
        plt.subplot(212)
        map.drawcoastlines()
        map.drawcountries()
        map.fillcontinents(color='coral')
        map.drawmapboundary()
        if logit_transform:
            print 'here4'
            map.scatter(x, y, 1, c=inv_logit(gridout2),cmap=cm.jet,vmin=valmin,vmax=valmax,edgecolor=None,lw=0)
        else:
            print 'here5'
            map.scatter(x, y, 1, c=gridout2,cmap=cm.jet,vmin=valmin,vmax=valmax,edgecolor=None,lw=0)
        plt.colorbar(shrink=0.5)
    else:
        if (obsid[0]==5521):
            if(whatparm=='obs'):
                valmin=32.
                valmax=39.
            if(whatparm=='omf'):
                valmin=-1.
                valmax=1.
            if(whatparm=='oma'):
                valmin=-1.
                valmax=1.0
            if(whatparm=='omf-oma'):
                valmin=-0.2
                valmax=0.2
            descriptor='Sz'
            plt.subplot(211)
            map.drawcoastlines()
            map.drawcountries()
            map.fillcontinents(color='coral')
            map.drawmapboundary()
#                      print 'here6'
            map.scatter(x, y, 1, c=gridout2,cmap=cm.jet,vmin=valmin,vmax=valmax,edgecolor=None,lw=2)
            plt.colorbar(shrink=0.5)
    #              map.plot(x, y, color=gridout2, marker='.', markersize= 5, linestyle='None',alpha=0.2)
        if (obsid[0]==3073):
    #              print 'here'
            if(whatparm=='obs'):
                valmin=0.
                valmax=30.
            if(whatparm=='omf'):
                valmin=-1.
                valmax=1.
            if(whatparm=='oma'):
                valmin=-1.
                valmax=1.
            if(whatparm=='omf-oma'):
                valmin=-0.5
                valmax=0.5
            descriptor='Tz'
            plt.subplot(211)
            map.drawcoastlines()
            map.drawcountries()
            map.fillcontinents(color='coral')
            map.drawmapboundary()
#                      print 'here7'
            map.scatter(x, y, 1, c=gridout2,cmap=cm.jet,vmin=valmin,vmax=valmax,edgecolor=None,lw=2,alpha=1)
            plt.colorbar(shrink=0.5)
    #              map.plot(x, y, color=gridout2, marker='.', markersize= 5, linestyle='None',alpha=0.2)

    datestart=yyyys+mms+dds+hhs
    dateend=yyyye+mme+dde+hhe
    titlestr=str(len(gridout2))+' '+descriptor+' '+whatparm+' '+str(yyyys)+str(mms)+str(dds)+str(hhs)+'-'+str(yyyye)+str(mme)+str(dde)+str(hhe)+'\n '+str(levtop)+'-'+str(levbot)+' m '+senstit2
    plt.title(titlestr)
    plt.savefig(descriptor+'_'+whatparm+'_'+datestart+'-'+dateend+'_'+levtop+'-'+levbot+'m_binned'+senstit2)
#plt.clf()
#       plt.show()  # to output on the screen
    plt.close()
else:
    print '# * Args:  '
    print '# *              directory      : directory to  find obs-YYYYMMDD_HH.nc files'
    print '# *              yyyys start    : 4 digit year'
    print '# *              mms   start    : 2 digit month'
    print '# *              dds   start    : 2 digit day'
    print '# *              hhs   start    : 2 digit hour'
    print '# *              yyyye end      : 4 digit year'
    print '# *              mme   end      : 2 digit month'
    print '# *              dde   end      : 2 digit day'
    print '# *              hhe   end      : 2 digit hour'
    print '# *              obs_type       : Entered as SZ, TZ, SSS, SSH, AICE, HICE '
    print '# *              levtop         : 4 digit top level (close to the surface)'
    print '# *              levbot         : 4 digit bot level (close to bottom)'
    print '# *              whatparm       : what parameter to plot (obs, oma, omf)'
    print '# *              sensors        : what sensors to plot (Argo, XBT, CTD, SMAP)'
