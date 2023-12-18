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

yesterday       = today + timedelta(days=-1)
yesterday_year  = str(yesterday)[:4]
yesterday_month = str(yesterday)[5:7]
yesterday_day   = str(yesterday)[8:10]

yesterday2       = today + timedelta(days=-2)
yesterday_year2  = str(yesterday2)[:4]
yesterday_month2 = str(yesterday2)[5:7]
yesterday_day2   = str(yesterday2)[8:10]

print 'Checking Mooring V2 Data Files'
print '   Current Date ',current_year, current_month, current_day

mindate = str(current_year)+'010112'
maxdate = yesterday_year+yesterday_month+yesterday_day+'12'
print '   Min Date ',mindate, 'Max Date ',maxdate

fname_rama = '/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OBS/REAL_TIME/T_RAMA_'+str(current_year)+'.nc'
fname_pir  = '/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OBS/REAL_TIME/T_PIR_'+str(current_year)+'.nc'
fname_tao  = '/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OBS/REAL_TIME/T_TAO_'+str(current_year)+'.nc'

tmin = 0
tmax = 35

print ' '
# RAMA
odasgrp = Dataset(fname_rama, 'r', format='NETCDF4') 
data1    = np.squeeze(odasgrp.variables['TEMP'][:]) 
idate1   = np.squeeze(odasgrp.variables['DATE_TIME'][:]) 
odasgrp.close()	
data1[data1>99999]=np.nan
# Max date should be one day behind real-time
file_mindate1 = str(np.nanmin(idate1))
file_maxdate1 = str(np.nanmax(idate1))
print '   RAMA ',file_mindate1, file_maxdate1, np.nanmin(data1), np.nanmax(data1)

# PIRATA
odasgrp = Dataset(fname_pir, 'r', format='NETCDF4') 
data2    = np.squeeze(odasgrp.variables['TEMP'][:]) 
idate2   = np.squeeze(odasgrp.variables['DATE_TIME'][:]) 
odasgrp.close()	
data2[data2>99999]=np.nan
# Max date should be one day behind real-time
file_mindate2 = str(np.nanmin(idate2))
file_maxdate2 = str(np.nanmax(idate2))
print '   PIR  ',file_mindate2, file_maxdate2, np.nanmin(data2), np.nanmax(data2)

#TAO
odasgrp = Dataset(fname_tao, 'r', format='NETCDF4') 
data3    = np.squeeze(odasgrp.variables['TEMP'][:]) 
idate3   = np.squeeze(odasgrp.variables['DATE_TIME'][:]) 
odasgrp.close()	
data3[data3>99999]=np.nan
# Max date should be two days behind real-time
file_mindate3 = str(np.nanmin(idate3))
file_maxdate3 = str(np.nanmax(idate3))
print '   TAO  ',file_mindate3, file_maxdate3, np.nanmin(data3), np.nanmax(data3)
maxdate_tao = yesterday_year2+yesterday_month2+yesterday_day2+'12'

print ' '

# Check Values
data1err = 0
data2err = 0
data3err = 0
if np.nanmin(data1) < tmin:
        date1err = 1
if np.nanmax(data1) > tmax:
        data1err = 1
if np.nanmin(data2) < tmin:
        data2err = 1
if np.nanmax(data2) > tmax:
        data2err = 1       
if np.nanmin(data3) < tmin:
        data3err = 1
if np.nanmax(data3) > tmax:
        data3err = 1

check1 = 1
check2 = 1
check3 = 1        
        
# RAMA
mincheck = 0
maxcheck = 0
if file_mindate1 <= mindate:
        mincheck = 0
else:
        print '   ERROR: in minimum date of RAMA file ', file_mindate1
        mincheck = 1     
if file_maxdate1 >= maxdate:
        maxcheck = 0
else:
        print '   ERROR: in maximum date of RAMA file ', file_maxdate1
        maxcheck = 1
if (mincheck==0) & (maxcheck==0) & (data1err==0):
        print '   RAMA Mooring V2 Data File is Ready'
        check1 = 0
       
# PIRATA
mincheck = 0
maxcheck = 0
if file_mindate2 <= mindate:
        mincheck = 0
else:
        print '   ERROR: in minimum date of PIRATA file ', file_mindate2
        mincheck = 1    
        print 'check = 1'
         
if file_maxdate2 >= maxdate:
        maxcheck = 0
else:
        print '   ERROR: in maximum date of PIRATA file ', file_maxdate2
        maxcheck = 1
        print 'check = 1'
if (mincheck==0) & (maxcheck==0) & (data2err==0):
        print '   PIR  Mooring V2 Data File is Ready'
        check2 = 0
        

# TAO
mincheck = 0
maxcheck = 0
if file_mindate3 <= mindate:
        mincheck = 0
else:
        print '   ERROR: in minimum date of TAO file ', file_mindate3
        mincheck = 1     
if file_maxdate3 >= maxdate_tao:
        maxcheck = 0
else:
        print '   ERROR: in maximum date of TAO file ', file_maxdate3
        maxcheck = 1
if (mincheck==0) & (maxcheck==0) & (data3err==0):
        print '   TAO  Mooring V2 Data File is Ready'
        check3 = 0
         
if (check1==0) & (check2==0) & (check3==0):
        print '   All Mooring Files are Ready' 
        print 'check = 0'      
        status.write('1')
        status.close() 
else:
        print '   Mooring Files are NOT Ready'
        print 'check = 1'
        status.write('1')
        status.close() 
#





