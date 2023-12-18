import os
from ftplib import FTP
import sys
import numpy as np
import glob
from datetime import datetime,date,timedelta
import datetime
import time
from time import strftime
import write_nc_assim
import tarfile
from netCDF4 import Dataset
from netCDF4 import num2date, date2num
import netCDF4 as nc
import array
import glob
import struct
import time
import sys
import getopt
import string
import scipy
import write_nc_assim
sys.path.append('../ocn_utils/')
import get_date
import misc
import h5py
import xarray as xr
import warnings
warnings.filterwarnings("ignore")





# defining data downloading function

def download_data(year,mons,days,hostname,username,passwd,raw):
 
#    year = str(2023)
#    mons = str(9).zfill(2)
#    days = str(15).zfill(2)
#    url = 'https://cmr.earthdata.nasa.gov/virtual-directory/collections/C2208418228-POCLOUD/temporal'

    day_of_year=date(int(year),int(mons),int(days)).timetuple().tm_yday
    doy = str(day_of_year).zfill(3)

    ftp_source_pathD  = '/Ocean_products/GRIDDED/L3OS/OPER/NRT_CSF2QD/' + year +'/'
    ftp_source_pathA  = '/Ocean_products/GRIDDED/L3OS/OPER/NRT_CSF2QA/' + year +'/'



    ftp = FTP(hostname)
    ftp.login(username,passwd)
    
# Get All Files

#downloading descending data files
    
    ftp.cwd(ftp_source_pathD)
    if doy in ftp.nlst():
       ftp.cwd(ftp_source_pathD+doy)
       files = ftp.nlst() 
       for file in files: 
         ftp.retrbinary("RETR " + file ,open(raw + file, 'wb').write)
    else: 
       print("Missing " + year + mons+days +" Descending  data")
# downloading ascending data files
    ftp.cwd(ftp_source_pathA)
    if doy in ftp.nlst():
       ftp.cwd(ftp_source_pathA+doy)
       files = ftp.nlst()
       for file in files:
         ftp.retrbinary("RETR " + file ,open(raw + file, 'wb').write)
    else:
       print("Missing " + year + mons + days + " Ascending  data")
    os.chdir(raw)
    
    files = os.listdir('.')
    print('downloaded files: ',files)
    for file in files:
       tar = tarfile.open(file,'r:gz')
       for item in tar:
         tar.extract(item)

       os.remove(file)



# generating yearly netcdf file

def make_assim(syear,doy,assim_path,raw_path,ver):
    

   INST  = 'SMOS'
   VAR   = 'S'
   svar  = 'sss'
   SVAR  = 'SSS'
   param = write_nc_assim.param(INST,VAR)
   im    = 1388
   jm    = 584
   inc   = 1
   N     = im*jm
   iyear = int(syear)
# Every other grid point
#inc = 2
#im  = im0/inc
#jm  = jm0/inc
#N   = im*jm

   outfile = assim_path + '/SSS_TRK_SMOS_L32Q_'+syear+'.nc'
   tag     = 'w'
   if os.path.exists(outfile):
      tag = 'a'


##################
   dir_main = raw_path +'/' + syear+'/' + doy + '/'
   tmpdir   = dir_main+'SM*_331_*.nc'
   flist    = sorted(list(glob.glob(tmpdir)))
   tm = len(flist) #np.shape(flist)[0]
#   print (tm,' Raw Files')
   print (N, ' Max Observations')


   cnt=0
   for fname in flist:  #[:]:
       print(fname)
       doy = fname[-71:-68]
       ascdsc = fname[-50]  #  character in filename that ids it as ascending or desc.
        #print 'ascdsc is ', ascdsc
       gdate = datetime.date(iyear, 1, 1) + datetime.timedelta(int(doy) - 1)
       idate = datetime.datetime.strftime(gdate, '%Y%m%d%H')

	#odasgrp  = Dataset(fname, 'r', format='NETCDF4')
	#data_b = odasgrp.variables['Sea_Surface_Salinity_Model1_Value'][:]
	#data_b = abs(data_b)
	#data_b = np.ma.masked_values(data_b, -odasgrp.variables['Sea_Surface_Salinity_Model1_Value']._FillValue)
	#data_b = data_b * float(odasgrp.variables['Sea_Surface_Salinity_Model1_Value'].scale_factor)
	#data_b += float(odasgrp.variables['Sea_Surface_Salinity_Model1_Value'].add_offset)
       odasgrp  = xr.open_mfdataset(fname,decode_times=False)
       data     = odasgrp.Sea_Surface_Salinity.values
       err      = odasgrp.Sea_Surface_Salinity_Error.values
       bias     = odasgrp.Sea_Surface_Salinity_Bias_Correction.values
       lon      = odasgrp.lon.values
       lat      = odasgrp.lat.values
       time     = odasgrp.Mean_Acq_Time.values
       xswath   = odasgrp.X_Swath.values
       odasgrp.close()
#	data[data == fill] = np.nan
#	data = data*scale
#	data = data+add
#	err[err == fill] = np.nan
#	err = err*scale
#	bias[bias == fill] = np.nan
#	bias = bias*scale
#       xswath[xswath == fill] = np.nan
 	#print np.nanmin(time), np.nanmax(time), np.min(time), np.max(time)

#	time[time == fill]     = np.nan
#	time[time == 'nan']    = np.nan
#	time[time == 'NaNf']   = np.nan
#	time[misc.isNaN(time)] = np.nan
  	#print np.nanmin(time), np.nanmax(time), np.min(time), np.max(time)

        #good = (~np.isnan(data))
       good = ((~np.isnan(data)) & (~np.isnan(time)))
       I=np.where(good)
       Ne=len(I[0])
	#print 'Check 1 data ', Ne
       mx,my = np.meshgrid(lon,lat)
       data2 = data[I]
       err2  = err[I]
       bias2 = bias[I]
       lon2  = mx[I]
       lat2  = my[I]
       time2 = time[I]
       xswath2 = xswath[I]

        # Sort by date/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/assim/L2_SMOS_SSS_7.0/L32Q
       ind = np.argsort(time2,axis=0)
       data3 = data2[ind]
       err3  = err2[ind]
       bias3 = bias2[ind]
       xswatch3 = xswath2[ind]
       lon3  = lon2[ind]
       lat3  = lat2[ind]
       time3 = time2[ind]
       mask       = np.ones([Ne])
       date_time  = np.ones([Ne])
       if time3[0] < 70000:
           a = num2date(time3,units='days since 2000-01-01 00:00:00',calendar='standard')
       else:
           a = num2date(time3,units='days since 0000-01-01 00:00:00',calendar='standard')

       for t in range(Ne):
		#a = num2date(time3[t],units='days since 0000-01-01 00:00:00',calendar='standard')
          if time3[t] < 2:
             mask[t] = 0
             print ('error',time3[t], mask[t])
             sys.exit()
          else:
#		  	adate = datetime.datetime.strftime(a[t], '%Y%m%d%H')
             adate = a[t].strftime('%Y%m%d%H')
             date_time[t] = adate
       
       N_PROF    = np.ones([Ne,1])*1.0
       DATA_ID   = np.ones([Ne,1])
       TRK_ID    = np.ones([Ne,1])
       DATE_TIME = np.zeros([Ne,1])
       LON       = np.ones([Ne,1])*1.0
       LAT       = np.ones([Ne,1])*1.0
       NPTS      = np.ones([Ne,1])
       QC_FLAG   = np.ones([Ne,1])
       INST_ID   = np.ones([Ne,1])*param.INST_ID
       QC_PRF    = np.ones([Ne,1])*1.0
       DATA      = np.ones([Ne,1])*1.0
       OBS_ERROR = np.ones([Ne,1])*0.1
       OBS_BIAS  = np.ones([Ne,1])*0.1
       OBS_XSWATH= np.ones([Ne,1])*1.0
       DEPTH     = np.ones([Ne,1])*5.0
       QC_LEV    = np.ones([Ne,1])*1.0

       DATA[0:Ne,0]           = data3
       OBS_ERROR[0:Ne,0]      = err3
       OBS_BIAS[0:Ne,0]       = bias3
       OBS_XSWATH[0:Ne,0]     = xswatch3
       LON[0:Ne,0]            = lon3
       LAT[0:Ne,0]            = lat3
       DATE_TIME[0:Ne,0]      = date_time
       
       for t in range(Ne):
          N_PROF[t]   = t
          DATA_ID[t] = (param.INST_ID*100000)+t
          if ascdsc=='A':
             TRK_ID[t]=0
          if ascdsc=='D':
             TRK_ID[t]=1

       x360 = np.where(LON>180)
       LON[x360]=LON[x360]-360 

       print (Ne, gdate, np.nanmin(data), np.nanmax(data), np.nanmin(DATE_TIME[:,0]), np.nanmax(DATE_TIME[:,0]))

       if (cnt==0):
           write_nc_assim.write_nc_2D(outfile,param,N_PROF[0:Ne],DATA_ID[0:Ne],DATE_TIME[0:Ne],LON[0:Ne],LAT[0:Ne],NPTS[0:Ne],QC_FLAG[0:Ne],INST_ID[0:Ne],QC_PRF[0:Ne],DATA[0:Ne],OBS_ERROR[0:Ne],DEPTH[0:Ne],QC_LEV[0:Ne],OBS_BIAS[0:Ne],OBS_XSWATH[0:Ne],TRK_ID[0:Ne])
       else:
           write_nc_assim.append_nc_2D(outfile,param,N_PROF[0:Ne],DATA_ID[0:Ne],DATE_TIME[0:Ne],LON[0:Ne],LAT[0:Ne],NPTS[0:Ne],QC_FLAG[0:Ne],INST_ID[0:Ne],QC_PRF[0:Ne],DATA[0:Ne],OBS_ERROR[0:Ne],DEPTH[0:Ne],QC_LEV[0:Ne],OBS_BIAS[0:Ne],OBS_XSWATH[0:Ne],TRK_ID[0:Ne])

	#sys.exit()
	#if cnt==30:
	#	sys.exit()
       cnt = cnt+1

   print ('Output File: ',outfile)








