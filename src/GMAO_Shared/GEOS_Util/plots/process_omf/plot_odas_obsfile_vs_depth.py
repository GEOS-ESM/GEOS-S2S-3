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

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import sys
import glob
import numpy as np
import array as arr
from netCDF4 import Dataset
from datetime import date
import datetime
import matplotlib.cm as cm

from mpl_toolkits.basemap import Basemap

switch_obs = {
     'SZ'   : 5521, # Salinity
     'TZ'   : 3073, # Temperature
     }
#
switch_instids = {
    'TAO'         : 501,  #
    'PIRATA'      : 502,  #
    'XBT'         : 503,  #
    'RAMA'        : 504,  #
    'Argo'        : 508,  #
    'CTD'         : 513  #
    }

switchback_instids = {
     501        :  'TAO',
     502        :  'PIRATA',
     503        :  'XBT',  #
     504        :  'RAMA',  #
     508        :  'Argo',  #
     513        :  'CTD', #
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

if len(sys.argv) > 17:

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
    lats     = sys.argv [11]     # 'south latitude'
    latn     = sys.argv [12]     # 'north latitude'
    lonw     = sys.argv [13]     # 'west longitude 0-360'
    lone     = sys.argv [14]     # 'east longitude 0-360'
    levtop   = sys.argv[15]      # '0'
    levbot   = sys.argv[16]      # '50'
    sensor   = sys.argv[17:]     # 'Argo', 'XBT', etc.
    sensor2  = sensor
#       print 'sensor2 is', sensor2

    mme=str(mme).zfill(2)
    mms=str(mms).zfill(2)
    dds=str(dds).zfill(2)
    dde=str(dde).zfill(2)

#    kick out any non-profile data right away
    if(obstyp == 'SSH' or obstyp == 'SSS'):
        print 'this routine plots only profile data',obstyp
        exit()
#    test inputs
    if(int(lonw) < 0):
        lonwold=lonw
        lonw = int(lonw) + 360.
        print 'fix <0 lon to lon east',lonwold,lonw
    if(int(lone) < 0):
        loneold=lone
        lone = int(lone) + 360.
        print 'fix <0 lon to lon east',loneold,lone
    if(int(lonw) > int(lone)):
        print 'west lon gt east lon',lonw,lone
        exit()
    if(int(lats) > int(latn)):
        print 'south lat gt north lat',lats,latn
        exit()
    if(int(levtop) > int(levbot)):
        print 'top level gt bottom',levtop,levbot
        exit()

    list_of_obs = switch_obs[obstyp]

    list_of_sensors = []
    senstit=''
    print 'sensor,obstyp',sensor, obstyp
    if(sensor[0] == 'ALL' and obstyp == 'TZ'):
        ALLsensor=['Argo','TAO','PIRATA', 'RAMA', 'CTD','XBT']
        sensor2=ALLsensor
#               print 'sensor is from ALL',ALLsensor
    if(sensor[0] == 'ALL' and obstyp == 'SZ'):
        ALLsensor=['Argo','TAO','PIRATA', 'RAMA', 'CTD']
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
    print 'directory is ', directory

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

#       min = lon.data.min()
#       max = lon.data.max()
#       print 'max min long is', max, min

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

    # convert to degree east
    for k in range(len(lon)):
        if(lon[k] < 0.):
#               print 'lon before', lon[k]
            lon[k]=lon[k]+360.
#               print 'lon after',lon[k]

    #  limit the data to longitudes

    I=np.where((lon>=int(lonw)) & (lon<=int(lone)))
    lon=lon[I]
    lat=lat[I]
    obsid=obsid[I]
    obs=obs[I]
    oma=oma[I]
    omf=omf[I]
    lev=lev[I]
    instid=instid[I]

    #  limit the data to latitudes

    I=np.where((lat>=int(lats)) & (lat<=int(latn)))
    lon=lon[I]
    lat=lat[I]
    obsid=obsid[I]
    obs=obs[I]
    oma=oma[I]
    omf=omf[I]
    lev=lev[I]
    instid=instid[I]

    min1 = lon.data.min()
    max1 = lon.data.max()
    print 'max min long is', min1, max1
    min2 = lat.data.min()
    max2 = lat.data.max()
    print 'max min lat is', min2, max2
    min3 = lev.data.min()
    max3 = lev.data.max()
    print 'max min lev is', min3, max3

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

    #  now average everything into one profile
    depthmod = np.zeros(50)
    depthtop = np.zeros(50)
    depthbot = np.zeros(50)
    y = np.zeros(50)
    depthmod = [5.03354978561401, 15.1006498336792, 25.2193508148193,  \
    35.3584518432617, 45.5763511657715, 55.8532485961914,  \
    66.2617492675781, 76.802848815918, 87.5769500732422,  \
    98.6232528686523, 110.096199035645, 122.10669708252,  \
    134.908599853516, 148.746597290039, 164.053802490234, 181.3125,  \
    201.263000488281, 224.777297973633, 253.068099975586,  \
    287.550811767578, 330.0078125, 382.365112304688, 446.726287841797,  \
    524.982421875, 618.703125, 728.692077636719, 854.993530273438,  \
    996.715270996094, 1152.3759765625, 1319.99694824219,  \
    1497.56201171875, 1683.05700683594, 1874.78796386719, 2071.251953125,  \
    2271.32299804688, 2474.04296875, 2678.75708007812, 2884.89794921875,  \
    3092.11694335938, 3300.0859375, 3508.63305664062, 3717.56689453125,  \
    3926.81298828125, 4136.2509765625, 4345.86376953125,  \
    4555.56591796875, 4765.369140625, 4975.208984375, 5185.11083984375,  \
    5395.02294921875]

#       flip depth to negative for plotting
    m1=-1.
    y=np.multiply(depthmod,m1)
    levtop2 =  float(levtop) * float(m1)
    levbot2 = float(levbot) * float(m1)

    depthtop[0]=0.

    i = 1
    while i < len(depthmod):
        depthtop[i] = ((depthmod[i]-depthmod[i-1])/2.)+depthmod[i-1]
        depthbot[i-1] = depthtop[i]
        i += 1
    depthbot[49]=(depthmod[49]-depthmod[48])/2.+depthmod[49]
    i = 0

    obsouts=np.empty(shape=(50),dtype='float')
    omfouts=np.empty(shape=(50),dtype='float')
    omaouts=np.empty(shape=(50),dtype='float')
    omfmomaouts=np.empty(shape=(50),dtype='float')
    obsoutn=np.empty(shape=(50),dtype='float')
    omfoutn=np.empty(shape=(50),dtype='float')
    omaoutn=np.empty(shape=(50),dtype='float')
    omfmomaoutn=np.empty(shape=(50),dtype='float')
    obsouts=[0. for col in range(50)]
    omfouts=[0. for col in range(50)]
    omaouts=[0. for col in range(50)]
    omfmomaouts=[0. for col in range(50)]
    obsoutn=[0. for col in range(50)]
    omfoutn=[0. for col in range(50)]
    omaoutn=[0. for col in range(50)]
    omfmomaoutn=[0. for col in range(50)]
    obsout2=np.empty(shape=(50),dtype='float')
    omfout2=np.empty(shape=(50),dtype='float')
    omaout2=np.empty(shape=(50),dtype='float')
    omfmomaout2=np.empty(shape=(50),dtype='float')
    obsout2=[0. for col in range(50)]
    omfout2=[0. for col in range(50)]
    omaout2=[0. for col in range(50)]
    omfmomaout2=[0. for col in range(50)]
#       sum over depths here (obsouts is sum, obsoutn is num)
    for i in range(len(omf)):             #  do i=1,nobs
        for k in range(len(depthmod)):    #  do k=0,49
            if (lev[i] >= depthtop[k] and lev[i] < depthbot[k]):
                obsouts[k] = obsouts[k] + obs[i]
                obsoutn[k]=obsoutn[k]+1.
                omfouts[k]=omfouts[k]+omf[i]
                omfoutn[k]=omfoutn[k]+1.
                obsouts[k]=obsouts[k]+obs[i]
                obsoutn[k]=obsoutn[k]+1.
                omaouts[k]=omaouts[k]+oma[i]
                omaoutn[k]=omaoutn[k]+1.
                omfmomaouts[k]=omfmomaouts[k]+omfmoma[i]
                omfmomaoutn[k]=omfmomaoutn[k]+1.
#                       print 'filling ',k,depthtop[k],lev[i],depthbot[k]
#       do the averaging here
    for k in range(len(depthmod)):    #  do k=0,49
        if(omfoutn[k] > 0):
            omfout2[k] = omfouts[k]/omfoutn[k]
            omaout2[k] = omaouts[k]/omaoutn[k]
            obsout2[k] = obsouts[k]/obsoutn[k]
            omfmomaout2[k] = omfmomaouts[k]/omfmomaoutn[k]
        else:
            omfout2[k] = np.nan
            omaout2[k] = np.nan
            obsout2[k] = np.nan
            omfmomaout2[k] = np.nan

    print obsout2
    print obsoutn
#       fill zeroline
    zeroline=np.zeros(len(obsout2))

    plt.ylim(levbot2,levtop2)
    plt.plot(omfout2, y,color='r')
    plt.plot(omaout2, y,color='b')
#       plt.plot(obsout2,y,color='g')
    plt.plot(zeroline,y)

    plt.legend(['OMF', 'OMA'], loc=4)
# naming the x axis
    plt.xlabel('x - axis')
# naming the y axis
    plt.ylabel('y - axis')
    datestart=yyyys+mms+dds+hhs
    dateend=yyyye+mme+dde+hhe
    titlestr=str(len(obs))+' '+obstyp+' OMAOMF '+str(yyyys)+str(mms)+str(dds)+str(hhs)+'-'+str(yyyye)+str(mme)+str(dde)+str(hhe)+'\n '+str(lonw)+'-'+str(lone)+' '+str(lats)+'-'+str(latn)+' '+str(levtop)+'-'+str(levbot)+' m '+senstit2
    plt.title(titlestr)
    plt.savefig(obstyp+'_'+'OMAOMF_'+datestart+'-'+dateend+'_'+lonw+'-'+lone+'_'+lats+'-'+latn+'_'+levtop+'-'+levbot+'m_binned'+senstit2)
#plt.clf()
#       plt.show()  # to output on the screen
    plt.close()
#   now plot the obs
    plt.ylim(levbot2,levtop2)
    plt.plot(obsout2,y,color='g')
    titlestr=str(len(obs))+' '+obstyp+' OBS '+str(yyyys)+str(mms)+str(dds)+str(hhs)+'-'+str(yyyye)+str(mme)+str(dde)+str(hhe)+'\n '+str(lonw)+'-'+str(lone)+' '+str(lats)+'-'+str(latn)+' '+str(levtop)+'-'+str(levbot)+' m '+senstit2
    plt.title(titlestr)
    plt.savefig(obstyp+'_OBS_'+datestart+'-'+dateend+'_'+lonw+'-'+lone+'_'+lats+'-'+latn+'_'+levtop+'-'+levbot+'m_binned'+senstit2)
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
    print '# *              lats south     : 3 digit latitude'
    print '# *              latn north     : 3 digit latitude'
    print '# *              lonw west      : 4 digit longitude'
    print '# *              lone east      : 4 digit longitude'

    print '# *              levtop         : 4 digit top level (close to the surface)'
    print '# *              levbot         : 4 digit bot level (close to bottom)'
    print '# *              sensors        : what sensors to plot (Argo, XBT, CTD, ALL)'
