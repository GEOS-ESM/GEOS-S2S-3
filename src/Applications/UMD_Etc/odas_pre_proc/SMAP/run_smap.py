#! /usr/bin/env python
#
# https://cmr.earthdata.nasa.gov/virtual-directory/collections/C2208418228-POCLOUD/temporal/
#
# Sep 2023: clound server
# Earthdata, user: lren1, pass: ren@GMAP2023
#
# Usage: run_smap.py YYYYMMDD NRT
# purpose: download the SMOS L2 data and generate yearly netcdf file for GEOodas 
# Author: Li Ren
# Last update: 20231005
#



import yaml
import datetime
import os
import glob
import sys
#import subprocess
import smap_util
import warnings
warnings.filterwarnings("ignore")




if len(sys.argv)>2:
    idate=sys.argv[1]
    ftype = str(sys.argv[2])
    print('ftype:', ftype)

# reading config.yaml to 
smap_config = yaml.load( open( 'config.yaml', "r"), Loader=yaml.FullLoader)
ver         = smap_config['ver']
raw_path    = smap_config['path']['raw'] + '/' + ver
run_path    = smap_config['path']['run']
assim_path   = smap_config['path']['assim']
ascii_path  = raw_path + '/' + 'results.' + ver
url_nrt     = smap_config['url']['nrt']
url_dt      = smap_config['url']['dt']
ndays       = int(smap_config['ndayAGO'])


year  = idate[0:4]
month = idate[4:6]
day   = idate[6:8]
hour  = 00
min   = 00

print(year+'-'+month+'-'+day)

ins = 'JPL' 

yearPath = raw_path + '/' + year
isExist  = os.path.exists(yearPath)
if not isExist:
   os.makedirs(yearPath)


today = datetime.datetime(int(year),int(month),int(day))
for n_days_ago in range(ndays):

   d = datetime.timedelta(days = n_days_ago)
   a = today - d
   doy = (a - datetime.datetime(a.year, 1, 1)).days + 1
   print('****** processing SMAP on' + str(a.year) + str(a.month).zfill(2) + str(a.day).zfill(2) +' ******')
   doyPath = raw_path +'/'+ str(a.year)+'/'+str(doy).zfill(3)   
   isExist = os.path.exists(doyPath)   
   if not isExist:
      os.makedirs(doyPath)
   if (ftype == 'NRT'):
       os.chdir(doyPath)
       for f in glob.glob("*.h5"):
           os.remove(f)     
       smap_util.download_data(str(a.year),str(a.month).zfill(2),str(a.day).zfill(2),url_nrt)


# NON-NRT

   if (ftype == 'DT'):
       os.chdir(doyPath)
       for f in glob.glob("*.h5"):
           os.remove(f)
       smap_util.download_data(str(a.year),str(a.month).zfill(2),str(a.day).zfill(2),url_dt)

   nfiles = len(sorted(list(glob.glob('*.h5'))))
   print('Number of raw files:', nfiles)
   if (nfiles < 1):
       print("Not enough raw files:", nfiles)
       sys.exit()
   
# generating ascii files
    # remove any files first
   os.chdir(ascii_path)
   for f in glob.glob(str(a.year) + str(doy).zfill(3) +'*'):
       print(f)
       os.remove(f)
#   myList = [str(a.year), str(a.month),str(a.day)]
#   p2 = subprocess.Popen(["python","convert_hdf2ascii.py"] + myList) 
   smap_util.convert_to_ascii(str(a.year),str(a.month).zfill(2),str(a.day).zfill(2),raw_path,ascii_path)


# producing netcdf yearly file

print('producing netcdf yearly file ')
os.chdir(run_path)
smap_util.make_assim(year,ascii_path,assim_path,raw_path,ver)
if int(a.year) < int(year):
    smap_util.make_assim(str(a.year),ascii_path,assim_path,raw_path,ver)




