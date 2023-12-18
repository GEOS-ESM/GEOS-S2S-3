#! /usr/bin/env python
#
# Usgage: run_smos.py YYYYMMDD
# Purpose: download SMOS L2 data and generate yearly netcdf file for GEOSodas
# 
# Author: Li Ren
# 
# Last update: 20231005

import yaml
import datetime
import os
import glob
import sys
import smos_util
import warnings
warnings.filterwarnings("ignore")




if len(sys.argv)>1:
    idate=sys.argv[1]

# reading config.yaml 
smos_config = yaml.load( open( 'config.yaml', "r"), Loader=yaml.FullLoader)
ver         = smos_config['ver']
raw_path    = smos_config['path']['raw'] + '/' + ver 
run_path    = smos_config['path']['run']
assim_path  = smos_config['path']['assim'] 
ftp_nrt     = smos_config['url']['nrt']
ftp_dt      = smos_config['url']['dt']
ftp_user    = smos_config['login']['username']
ftp_passwd  = smos_config['login']['password']
ndays       = int(smos_config['ndayAGO'])

year  = idate[0:4]
month = idate[4:6]
day   = idate[6:8]
hour  = 00
min   = 00

print(year+'-'+month+'-'+day)


yearPath = raw_path + '/' + year
isExist  = os.path.exists(yearPath)
if not isExist:
   os.makedirs(yearPath)


# 1. Downloading SMOS L2 data 

# ndays before the input date
ndays = 30

today = datetime.datetime(int(year),int(month),int(day))
for n_days_ago in range(ndays):
   
   d = datetime.timedelta(days = n_days_ago)
   a = today - d
   doy = (a - datetime.datetime(a.year, 1, 1)).days + 1
   print('****** processing SMOS data on ' + str(a.year) + str(a.month).zfill(2) + str(a.day).zfill(2) + ' ******')
   doyPath = raw_path +'/'+ str(a.year)+'/'+str(doy).zfill(3)+ '/'
   isExist = os.path.exists(doyPath)   
   if not isExist:
      os.makedirs(doyPath)
   os.chdir(doyPath)
   for f in glob.glob("*"):
       os.remove(f)     
   smos_util.download_data(str(a.year),str(a.month).zfill(2),str(a.day).zfill(2),ftp_nrt,ftp_user,ftp_passwd,doyPath)


   nfiles = len(sorted(list(glob.glob('*'))))
#   print('Number of raw files:', nfiles)
   if (nfiles < 1):
       print("Not enough raw files:", nfiles)
       sys.exit()
   

# 2. Producing yearly netcdf file

   print('producing yearly netcdf file:')
   os.chdir(run_path)
   smos_util.make_assim(str(a.year),str(doy).zfill(3),assim_path,raw_path,ver)


