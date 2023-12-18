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
# run_sigo_all.csh: Command not found.
#
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

print 'Checking AVISO V2 Data File'
print '   Current Date ',current_year, current_month, current_day

mindate = str(current_year)+'010100'
maxdate = yesterday_year+yesterday_month+yesterday_day+'24'
#print '   Min Date ',mindate, 'Max Date ',maxdate
tmin = -5
tmax = 5

#fdir =  '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/assim/ADT_TRK_7.0/YEARLY_FILES/'
fdir = '/discover/nobackup/lren1/pre_proc/NRT/AVISO/assim/ADT_TRK_7.0/YEARLY_FILES/'

# C2 only goes through July 30
sats   = ['AL','C2N','J3N','S3A','S6A',]
checks = [0,0,0,0,0]

for t in range(len(sats)):
        fname   = fdir+'ADT_TRK_'+sats[t]+'_'+str(current_year)+'.nc'
        print fname
        odasgrp = Dataset(fname, 'r', format='NETCDF4') 
        data    = np.squeeze(odasgrp.variables['ADT'][:]) 
        error   = np.squeeze(odasgrp.variables['OBS_ERROR'][:])
        idate   = np.squeeze(odasgrp.variables['DATE_TIME'][:]) 
        odasgrp.close()	
        data[data>99999]=np.nan
        error[error>99999]=np.nan
        mean_error = np.nanmean(error)
        max_error = np.nanmax(error)
        print '   '+sats[t]+' Data  ', np.nanmin(data), np.nanmax(data)
        print '   '+sats[t]+' OBS Error  ', np.nanmin(error), np.nanmax(error), np.nanmean(error)
        #print sats[t]+' Date  ', np.nanmin(idate), np.nanmax(idate)
       
        file_mindate = str(np.nanmin(idate))
        file_maxdate = str(np.nanmax(idate))
        dataerr = 0
        if np.nanmin(data) < tmin:
                print '   ERROR: in minimum value of '+sat+' ADT data ', np.nanmin(data)
                dataerr = 1
        if np.nanmax(data) > tmax:
                print '   ERROR: in maximum value of '+sat+' ADT data ', np.nanmax(data)
                dataerr = 1
        if max_error == mean_error:
                print '   ERROR: in mean value of '+sat+' OBS_ERROR data ', mean_error
                dataerr = 1
        if max_error <= 0.04:
                print '   ERROR: in max value of '+sat+' OBS_ERROR data ', max_error
                dataerr = 1
        mincheck = 0
        maxcheck = 0
        if file_mindate == mindate:
                mincheck = 0
        else:
                print '   ERROR: in minimum date of file ', file_mindate
                mincheck = 1
                print 'check = 1'      
        if file_maxdate >= maxdate:
                maxcheck = 0
        else:
                print '   ERROR: in maximum date of file ', file_maxdate
                maxcheck = 1
                print 'check = 1'         
        if (mincheck==0) & (maxcheck==0) & (dataerr==0):
                print '   '+sats[t]+' ADT File is Ready'
                print '   check = 0'
                checks[t] = 0
        
if min(checks)==0:
        print '   All ADT Files are Ready'
        print '   check = 0'     
        status.write('0')
        status.close() 
else:
        print '   AVISO V2 Data Files are NOT Ready'
        print 'check = 1'
        status.write('1')
        status.close() 
              
#





