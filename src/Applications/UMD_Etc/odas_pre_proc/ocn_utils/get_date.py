#! /usr/bin/env python
#

from netCDF4 import Dataset, num2date, date2num
from netCDF4 import MFDataset
import matplotlib
import numpy as np
import array
import datetime
from datetime import date


#Gtime = num2date(dtime,units='days since 1950-01-01 00:00:00',calendar='standard')
#dtmon    = datetime.datetime(int(ayear2),01,30)                    
#dtmonsec =  (dtmon - datetime.datetime(1970, 1, 1)).total_seconds() 

#time_ref = md.num2date(md.date2num(datetime(1981,1,1,0,0,0) + timedelta(seconds=np.float(sst_time[0]))))
#tmpnum = md.date2num(time_ref + timedelta(seconds=np.float(dt[j,i])))
#tmpdate = md.num2date(tmpnum)
#ymdhms[j,i]  = (tmpdate.strftime('%Y%m%d%H%M%S'))

#tmp = datetime.datetime(2013,1,1,0,0,10) 
#tmp = time.strftime('%H%M%S', time.gmtime(308.138))
#print tmp[0:2], tmp[2:4],tmp[4:6]

########################################################################################
def ymd2doy(year,month,day):

	doy  = (datetime.datetime(int(year),int(month),int(day))-datetime.datetime(int(year), 1, 1)).days + 1	

	return(doy)

########################################################################################
def date2ymd(yyyymmdd):
        date=yyyymmdd
        y = ((date)/10000)
        m = ((date-y*10000)/100)
        d = ((date-y*10000-m*100))
        return y,m,d
########################################################################################
def ymd2date(year,month,day):
        syear  = str(year)
        smonth = str(month)
        sday   = str(day)
        if int(month)<=9:
                smonth = '0'+smonth
        if int(day)<=9:
                sday = '0'+sday
        sdate = syear+smonth+sday

        return sdate

########################################################################################
def datetime2date(datetime):
        # yyyymmddhh to yyyymmdd
        year  = datetime/1000000
        month = (datetime-year*1000000)/10000
        day   = (datetime-year*1000000-month*10000)/100
        date  = year*10000 + month*100 + day
        return date

########################################################################################
def datetime2ymd(datetime):
        # yyyymmddhh to year, month, day
        year  = datetime/1000000
        month = (datetime-year*1000000)/10000
        day   = (datetime-year*1000000-month*10000)/100
        #date  = year*10000 + month*100 + day
        return year, month, day

##########################################################################################
def jd_to_date(jd):
        jd = jd + 0.5
        F, I = math.modf(jd)
        I = int(I)
        A = math.trunc((I - 1867216.25)/36524.25)
        if I > 2299160:
                B = I + 1 + A - math.trunc(A / 4.)
        else:
                B = I
        C = B + 1524
        D = math.trunc((C - 122.1) / 365.25)
        E = math.trunc(365.25 * D)
        G = math.trunc((C - E) / 30.6001)
        day = C - E + F - math.trunc(30.6001 * G)
        if G < 13.5:
                month = G - 1
        else:
                month = G - 13
        if month > 2.5:
                year = D - 4716
        else:
                year = D - 4715
        if month<10:
                smon = '0'+str(month)
        else:
                smon = str(month)

        if day<10:
                sday = '0'+str(int(day))
        else:
                sday = str(int(day))
        sdate = str(year)+smon+sday

        return sdate

########################################################################################



