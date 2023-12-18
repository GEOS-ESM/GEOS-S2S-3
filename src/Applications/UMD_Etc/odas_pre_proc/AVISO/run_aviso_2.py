#! /usr/bin/env python

# run_aviso_2.py syear sat
# Creates assimilation file

from netCDF4 import Dataset
from netCDF4 import num2date, date2num
import matplotlib.pyplot as plt
import numpy as np
import array
import matplotlib.cm as cm
from mpl_toolkits.basemap import Basemap
import glob
import struct
import time
import sys
import scipy.io.netcdf as nc
from mpl_toolkits.basemap import Basemap, shiftgrid, addcyclic
import getopt
import string
import scipy
import scipy.stats as stats
import datetime
from time import strftime
import os
#sys.path.append('/gpfsm/dhome/gmaofcst/ODAS/OBS/V2/UTILS/')
sys.path.append('/discover/nobackup/lren1/pre_proc/NRT/UTILS/')
import write_nc_assim

########################################################################

syear = sys.argv[1]
sat   = sys.argv[2]

# RT: AL, C2 J3

INST  = 'AVISO'
VAR   = 'ADT'
svar  = 'adt'
SVAR  = 'ADT'

iyear = int(syear)

param = write_nc_assim.param(INST,VAR)

if sat=='J2':
	param.INST_ID=517
	ssat = 'j2'
if sat=='C2':
	param.INST_ID=532
	ssat = 'c2'
if sat=='ENN':
	param.INST_ID=533
	ssat = 'enn'
if sat=='AL':
	param.INST_ID=534
	ssat = 'al'
if sat=='J1G':
	param.INST_ID=535
	ssat = 'j1g'
if sat=='G2':
	param.INST_ID=510
	ssat = 'g2'
if sat=='TPN':
	param.INST_ID=539
	ssat = 'tpn'
if sat=='EN':
	param.INST_ID=512
	ssat = 'en'
if sat=='J1':
	param.INST_ID=515
	ssat = 'j1'
if sat=='E1':
	param.INST_ID=511
	ssat = 'e1'
if sat=='E2':
	param.INST_ID=536
	ssat = 'e2'
if sat=='H2':
	param.INST_ID=537
	ssat = 'h2'
if sat=='J1N':
	param.INST_ID=538
	ssat = 'j1n'
if sat=='TP':
	param.INST_ID=514
	ssat = 'tp'
if sat=='J2N':
	param.INST_ID=540
	ssat = 'j2n'
if sat=='J3':
	param.INST_ID=541
	ssat = 'j3'
if sat=='S3A':
	param.INST_ID=542
	ssat = 's3a'
if sat=='C2N':
	param.INST_ID=543
	ssat = 'c2n'
if sat=='S6A':
        param.INST_ID=544
        ssat = 's6a'
if sat=='J3N':
        param.INST_ID=545
        ssat = 'j3n'

	
#outfile = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/assim/ADT_TRK_7.0/YEARLY_FILES/ADT_TRK_'+sat+'_'+syear+'.nc'

outfile = '/discover/nobackup/lren1/pre_proc/NRT/AVISO/assim/ADT_TRK_7.0/YEARLY_FILES/ADT_TRK_'+sat+'_'+syear+'.nc'


tag = 'w'
if outfile:
	tag = 'a'

##################

iyear = int(syear)

#dir_main  = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/AVISO/RAW'
dir_main  = '/discover/nobackup/lren1/pre_proc/NRT/AVISO/RAW'

tmpdir1   = dir_main+'/trk_adt_nrt/'+ssat+'/'+syear+'/*vfec_l3_'+syear+'*.nc'
tmpdir2   = dir_main+'/trk_adt_nrt/'+ssat+'/'+syear+'/*phy_l3_'+syear+'*.nc'

#if sat=='J3':
#	tmpdir1   = dir_main+'/trk_adt_nrt/'+ssat+'/'+syear+'/*adt_vfec_'+syear+'*.nc'
#	tmpdir2   = dir_main+'/trk_adt_nrt/'+ssat+'/'+syear+'/*vfec_l3_'+syear+'*.nc'
	
flist1    = sorted(list(glob.glob(tmpdir1)))
flist2    = sorted(list(glob.glob(tmpdir2)))
flist     = np.concatenate((flist1,flist2))

if ( (iyear==2012) ):	
	tmpdir = dir_main+'/trk_adt_dt/'+ssat+'/'+syear+'/*_vfec_l3_'+syear+'*.nc'
	flist  = sorted(list(glob.glob(tmpdir)))

tm = np.shape(flist)[0]
N = tm

########################################################################
fcnt=0
for fname in flist[:]:
        ncfile = Dataset(fname,'r')
        sdate  = fname[-20:-12]+'12'  
        sfile  = fname[-23:-21]
        sfile2 = fname[-26:-24]
       
	print len(fname), sdate
	print fname
	 
	#print sfile
	#print sfile2
	#sys.exit()

	#if len(fname) == 120:
	#	data = (ncfile.variables['ADT'][:])

# This IF statement doesn't work for S6a due to the length of file names
#        if len(fname) >= 121:
#                data   = (ncfile.variables['adt_filtered'][:])
#	
#        else:
#                data   = (ncfile.variables['mdt'][:])
# Li Ren replaced the following statement 2022.07.26
        varlist = list(ncfile.variables)
        if u'mdt' in varlist:
            data1   = ncfile.variables['mdt'][:]
            data2   = ncfile.variables['sla_filtered'][:]           
            data1[data1>=32767]=np.nan
            data2[data2>=32767]=np.nan
            data=data1+data2
            print 'reading mdt'
        else:
#            data = ncfile.varialbes['adt_filtered'][:]
            data = ncfile['adt_filtered'][:]
            data[data>=32767]=np.nan
            print 'reading adt_filtered'
        dtime  = (ncfile.variables['time'][:])*1.0
        lat    = (ncfile.variables['latitude'][:])*1.0
        lon    = (ncfile.variables['longitude'][:])*1.0
        ncfile.close()
        cnt     = 0;
        MaxData = np.shape(data)[0];
       
        nc.lon   = np.ones([MaxData,1])
        nc.lat   = np.ones([MaxData,1])
        nc.data  = np.ones([MaxData,1])
        nc.idate = np.ones([MaxData,1])

        Gtime = num2date(np.round(dtime,5),units='days since 1950-01-01 00:00:00',calendar='standard')

        idate = np.ones([MaxData,1])
        for t in range(MaxData):
                #print (t, data[t])
                idate[t]  = (Gtime[t].strftime('%Y%m%d%H'))
                if t==0:
                        idate0=idate[t]
                if ~np.isnan(lon[t]) & ~np.isnan(lat[t]) & ~np.isnan(data[t]):
                        nc.lon[cnt]   = lon[t]
                        nc.lat[cnt]   = lat[t]
                        nc.data[cnt]  = data[t]
                        nc.idate[cnt] = idate[t]

                        cnt = cnt+1
        Ne = cnt
       
	#print '   ',fname[-20:-12], sfile,MaxData, Ne, np.nanmin(data), np.nanmax(data), np.nanmin(lon), np.nanmax(lon)
        nc.lon   = nc.lon[0:Ne]
        nc.lat   = nc.lat[0:Ne]
        nc.data  = nc.data[0:Ne]
        nc.idate = nc.idate[0:Ne]

	# Sort by date
        ind = np.argsort(nc.idate,axis=0)

	# Assim File Parameters
	############################################
        N_PROF    = np.ones([Ne,1])*1.0
        DATA_ID   = np.ones([Ne,1])
        DATE_TIME = np.ones([Ne,1])
        LON       = np.ones([Ne,1])*1.0
        LAT       = np.ones([Ne,1])*1.0
        NPTS      = np.ones([Ne,1])
        QC_FLAG   = np.zeros([Ne,1])
        INST_ID   = np.ones([Ne,1])*param.INST_ID
        QC_PRF    = np.ones([Ne,1])*1.0

        DATA      = np.ones([Ne,1])*1.0
        OBS_ERROR = np.ones([Ne,1])*0.04
        DEPTH     = np.zeros([Ne,1])*1.0
        QC_LEV    = np.ones([Ne,1])*1.0
	#############################################

        DATA      = nc.data[ind]
        DATA      = DATA[:,0]
        LON       = nc.lon[ind]
        LAT       = nc.lat[ind]
        DATE_TIME = nc.idate[ind]

        for t in range(Ne):
                N_PROF[t]   = t
                DATA_ID[t] = (param.INST_ID*100000)+t

        x360 = np.where(LON>180)
        LON[x360]=LON[x360]-360 

        # Writing of Netcdf file
        if (fcnt==0):
                write_nc_assim.write_nc_2D(outfile,param,N_PROF,DATA_ID,DATE_TIME,LON,LAT,NPTS,QC_FLAG,INST_ID,QC_PRF,DATA,OBS_ERROR,DEPTH,QC_LEV)
        else:
                write_nc_assim.append_nc_2D(outfile,param,N_PROF,DATA_ID,DATE_TIME,LON,LAT,NPTS,QC_FLAG,INST_ID,QC_PRF,DATA,OBS_ERROR,DEPTH,QC_LEV)
        
        fcnt = fcnt+1

	
print ('Output: ',outfile)
print ('   ',fname[-20:-12], sfile,MaxData, Ne, np.nanmin(data), np.nanmax(data), np.nanmin(lon), np.nanmax(lon))
print ('   ',sat, ' ', tm,' ', fname[-20:-12], np.nanmin(data), np.nanmax(data))

