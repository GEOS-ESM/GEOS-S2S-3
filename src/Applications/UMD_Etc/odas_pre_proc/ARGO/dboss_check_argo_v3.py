#! /usr/bin/env python

import matplotlib
import numpy as np
from netCDF4 import Dataset
import os
import sys 
import getopt
import pandas as pd
from datetime import timedelta
from datetime import datetime as dt
import datetime

###########################################
status = open("status","w")
current_date  = datetime.date.today()
current_year  = current_date.year
current_month = current_date.month 
current_day   = current_date.day 
today         = pd.to_datetime('today') 

yesterday = today + timedelta(days=-1)
yesterday_year  = str(yesterday)[:4]
yesterday_month = str(yesterday)[5:7]
yesterday_day   = str(yesterday)[8:10]

print 'Checking Argo V3 Data File'
print '   Current Date ',current_year, current_month, current_day

mindate = str(current_year)+'010100'
maxdate = yesterday_year+yesterday_month+yesterday_day+'24'
#print '   Min Date ',mindate, 'Max Date ',maxdate

fname = '/discover/nobackup/lren1/pre_proc/NRT/ARGO/FINAL/T_ARGO_'+str(current_year)+'.nc'
tmin = -5
tmax = 40
odasgrp = Dataset(fname, 'r', format='NETCDF4') 
dataT   = np.squeeze(odasgrp.variables['TEMP'][:]) 
idate   = np.squeeze(odasgrp.variables['DATE_TIME'][:]) 
odasgrp.close()	
dataT[dataT>99999]=np.nan
#print 'T Data  ', np.nanmin(dataT), np.nanmax(dataT)
# Min date should be first day of the year
# Max date should be one day behind real-time
Tfile_mindate = str(np.nanmin(idate))
Tfile_maxdate = str(np.nanmax(idate))
#print '   T File Dates ', Tfile_mindate, Tfile_maxdate

fname = '/discover/nobackup/lren1/pre_proc/NRT/ARGO/FINAL/S_ARGO_'+str(current_year)+'.nc'
smin = 0
smax = 50
odasgrp = Dataset(fname, 'r', format='NETCDF4') 
dataS   = np.squeeze(odasgrp.variables['SALT'][:]) 
idate   = np.squeeze(odasgrp.variables['DATE_TIME'][:]) 
odasgrp.close()	
dataS[dataS>99999]=np.nan
#print 'S Data  ', np.nanmin(dataS), np.nanmax(dataS)

# Min date should be first day of the year
# Max date should be one day behind real-time
Sfile_mindate = str(np.nanmin(idate))
Sfile_maxdate = str(np.nanmax(idate))
#print '   S File Dates ', Sfile_mindate, Sfile_maxdate

# Check Values
dataTerr = 0
if np.nanmin(dataT) < tmin:
        print '   ERROR: in minimum value of T data ', np.nanmin(dataT)
        dataTerr = 1
if np.nanmax(dataT) > tmax:
        print '   ERROR: in maximum value of T data ', np.nanmax(dataT)
        dataTerr = 1

dataSerr = 0
if np.nanmin(dataS) < smin:
        print '   ERROR: in minimum value of T data ', np.nanmin(dataS)
        dataSerr = 1
if np.nanmax(dataS) > smax:
        print '   ERROR: in maximum value of T data ', np.nanmax(dataS)
        dataSerr = 1    
        
Tmincheck = 0
Tmaxcheck = 0  
          
Smincheck = 0
Smaxcheck = 0

if Tfile_mindate == mindate:
        Tmincheck = 0
else:
        print '   ERROR: in minimum date of Tfile ', Tfile_mindate
        Tmincheck = 1
        print 'check = 1'
       
if Tfile_maxdate >= maxdate:
        Tmaxcheck = 0
else:
        print '   ERROR: in maximum date of Tfile ', Tfile_maxdate
        Tmaxcheck = 1
        print 'check = 1'

if Sfile_mindate == mindate:
        Smincheck = 0
else:
        print '   ERROR: in minimum date of Sfile ', Sfile_mindate
        Smincheck = 1
        print 'check = 1'
       
if Sfile_maxdate >= maxdate:
        Smaxcheck = 0
else:
        print '   ERROR: in maximum date of Sfile ', Sfile_maxdate
        Smaxcheck = 1
        print 'check = 1'
         
if (Tmincheck==0) & (Tmaxcheck==0) & (dataTerr==0) & (Smincheck==0) & (Smaxcheck==0) & (dataSerr==0)  :
        print '   ARGO V3 Data File is Ready'
        print '   check = 0'
        status.write('0')
        status.close() 
else:
        print '   ARGO V3 Data File is NOT Ready'
        print 'check = 1'
        status.write('1')
        status.close() 
       



#





