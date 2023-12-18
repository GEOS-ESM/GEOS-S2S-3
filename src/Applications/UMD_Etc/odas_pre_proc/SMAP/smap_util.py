import requests
import os
from bs4 import BeautifulSoup
import sys
import numpy as np
import glob
from datetime import datetime,date,timedelta
from netCDF4 import Dataset
from netCDF4 import num2date, date2num
import h5py
import array
import struct
import time
import scipy.io.netcdf as nc
import getopt
import string
import scipy
import scipy.stats as stats
from time import strftime
import write_nc_assim
import warnings
warnings.filterwarnings("ignore")



# defining data downloading function

def download_data(year,mons,days,url):
 
#    year = str(2023)
#    mons = str(9).zfill(2)
#    days = str(15).zfill(2)
#    url = 'https://cmr.earthdata.nasa.gov/virtual-directory/collections/C2208418228-POCLOUD/temporal'
    urls = url + '/' + year + '/' + mons + '/' + days + '/'
    print(urls)
    grab = requests.get(urls)
    soup = BeautifulSoup(grab.text, 'html.parser')
    wget_str='wget --user=lren1 --password=ren@GMAO2023'
# opening a file in write mode
#f = open("test1.txt", "w")
# traverse paragraphs from soup
    for link in soup.find_all("a"):
        data = link.get('href')
        if str(data).find('archive.podaac.earthdata.nasa.gov') >0:
           os.system(wget_str+' '+str(data))
    
#   f.write(data)
#   f.write("\n")
 
#f.close()

# converting raw data to ascii format 

def convert_to_ascii(yyys,mons,dds,raw_path,ascii_path):

   
   print(yyys,mons,dds)
   
   day_of_year=date(int(yyys),int(mons),int(dds)).timetuple().tm_yday
   days = str(day_of_year).zfill(3)
 

   rawdir = raw_path + '/' + yyys + '/' + days
   flist    = sorted(list(glob.glob(rawdir+'/'+'*'+yyys+mons+'*.h5')))
#   outdir = '/discover/nobackup/lren1/pre_proc/NRT/SMAP/RAW/V5.0/results.V5.0'

   fname  = yyys+days+'.file'

   fout = ascii_path + '/' + fname

   f2 = open(fout,'w')


   ic3=0
   iflag=[0]*4

   for n in range(len(flist)):
      fn = flist[n]
      print(fn)
      f1 = h5py.File(fn,mode='r')
      sss=f1['smap_sss'][()]
      lon=f1['lon'][()]
      I = np.where(lon<0)
      lon[I]=lon[I]+360
      lat=f1['lat'][()]
      qual=f1['quality_flag'][()]
      sssu=f1['smap_sss_uncertainty'][()]
      [nx,ny]=np.shape(sss)
      year = float(yyys)
      h5_LOC=fn.find('h5')
      if 'NRT' in fn:
         hhs  = float(fn[h5_LOC-7:h5_LOC-5])
         mms  = float(fn[h5_LOC-5:h5_LOC-3])
         sec  = float(fn[h5_LOC-3:h5_LOC-1])
      else:
         hhs  = float(fn[h5_LOC-19:h5_LOC-17])
         mms  = float(fn[h5_LOC-17:h5_LOC-15])
         sec  = float(fn[h5_LOC-15:h5_LOC-13])
      f1.close()
      for ix in range(nx):
         for iy in range(ny):
            if qual[ix,iy]==0:
               if sss[ix,iy]>=29. and sss[ix,iy]<=40. and abs(lat[ix,iy])<=90. and lon[ix,iy] <=360.:

#               f2.write(format(fyear,'6.0f')+format(fday,'5.0f')+format(fhour,'4.0f')+format(fmin,'4.0f')+ \
#               format(fsec,'4.0f'+" "+format(lon[ix,iy],'10.4f')+" "+format(lat[ix,iy],'10.4f')+ \
#               format(sss[ix,iy],'10.4f')+" "+format(sssu[ix,iy],'10.4f')+" "+format(ic3,'4i')+ \
#               format(iflag[0],'10i')+format(iflag[1],'10i')+format(iflag[2],'10i')+ \
#               format(iflag[3],'10i')+"\n")
                    f2.write(format(year,'6.0f')+format(day_of_year,'5.0f')+format(hhs,'4.0f')+format(mms,'4.0f')+\
                    format(sec,'4.0f')+" "+ format(lon[ix,iy],'10.4f')+" "+format(lat[ix,iy],'10.4f')+" "+ \
                    format(sss[ix,iy],'10.4f')+" "+ format(sssu[ix,iy],'10.4f')+" "+format(ic3,'4d')+\
                    format(iflag[0],'10d')+format(iflag[1],'10d')+ \
                    format(iflag[2],'10d')+format(iflag[3],'10d')+"\n")



#              write(20,'(f6.0,f5.0,f4.0,f4.0,f4.0,1x,f10.4,1x,f10.4,1x,f10.4,1x,f10.4,1x,i4,4i10)')  &
#           fyear,fday,fhour,fmin,fsec,lon,lat,sss,sssu,ic3,iflag(1),iflag(2),iflag(3),iflag(4) 


# generating yearly netcdf file

def make_assim(syear,ascii_path,assim_path,raw_path,ver):

    INST  = 'SMAP'
    VAR   = 'SSS'
    param = write_nc_assim.param(INST,VAR)

    outfile = assim_path + '/' + ver+'/SSS_TRK_SMAP_'+ver+'_'+ str(syear)+'.nc'
    #print 'Ouput File: ', outfile

    tag = 'w'

    ##################

    iyear = int(syear)

#    dir_main = '/discover/nobackup/lren1/pre_proc/NRT/SMAP/RAW/V5.0/results.'+VER
    tmpdir   = ascii_path + '/' + syear + '*.file'
    flist    = sorted(list(glob.glob(tmpdir)))

    dir_raw = raw_path + '/' + syear+'/'


#tmpdir = dir_main+'/'+syear+'365.file'
#f1     = sorted(list(glob.glob(tmpdir)))
#tmpdir = dir_main+'/'+syear+'366.file'
#f2     = sorted(list(glob.glob(tmpdir)))
#flist = np.concatenate((f1,f2))

    tm = np.shape(flist)[0]
#print tm,' Files'
    N = tm

# Read ascii file
#2015.  37.  0. 36. 58.   286.3021   -59.7665    33.6680    3         0      2052         0      2048
#f6.0,f5.0,f4.0,f4.0,f4.0,1x,f10.4,1x,f10.4,1x,f10.4,1x,i4,4i10

    fcnt      = 0
    N_PROF0   = 0
    DATA_ID0  = param.INST_ID*100000

    for fname in flist[:]:
        tmp   = open(fname)
        print(fname)
        lines = tmp.readlines()
        nobs  = len(lines)
        print ('Track ',fcnt, fname[-12:-5], nobs, 'nobs')
        ddd = fname[-8:-5]
    #ddd = '279'
# convert day of the year to day and month
        ddd.rjust(3 + len(ddd), '0')
        strt_date = date(iyear, 1, 1)
        res_date = strt_date + timedelta(days=int(ddd) - 1)
        caldat= str(res_date)
        dd = caldat[8:10]
        month = caldat[5:7]
# check the raw data to determine NRT or DT
        raw_file_list = sorted(list(glob.glob(dir_raw+ddd+'/*')))
#        print(raw_file_list)
#        print(raw_file_list[0])
        raw_file1      =  raw_file_list[0]
        raw_file_type = raw_file1[-30:-27]
        file_type = 1
        if raw_file_type == 'NRT':
           file_type = 0
    #print ddd, raw_file_type, file_type
    #sys.exit()

        nc.lon     = np.ones([nobs,1])
        nc.lat     = np.ones([nobs,1])
        nc.data    = np.ones([nobs,1])
        nc.err     = np.ones([nobs,1])
        nc.idate   = np.ones([nobs,1])
        nc.data_id = np.ones([nobs,1])
        nc.prof    = np.ones([nobs,1])
        nc.file_type = np.ones([nobs,1])

        cnt  = 0
        for line in lines:
            line = line.strip()
            iyear1,ijday1,ihour1,imin1,isec1,lon1,lat1,sss1,err1,beam1,flag,flag,flag,flag = line.split()

        #sss1[sss1<29]=np.nan
        #sss1[sss1>40]=np.nan
        #err1[sss1<29]=np.nan
        #err1[sss1>40]=np.nan#

            if int(float(iyear1)) == iyear:
               ihour = int(float(ihour1))
               if ihour >=23:
                  ihour = 23

               lon1     = float(lon1)
               lat1     = float(lat1)
               if lon1>180:
                  lon1 = lon1-360

               doy     = int(float(ijday1)-1)
#              dtdelta = datetime.timedelta(doy)
               dtdelta = timedelta(doy)
#              dt      = datetime.datetime(int(float(iyear1)), 1, 1, ihour, int(float(imin1)), int(float(isec1)))
               dt      = datetime(int(float(iyear1)), 1, 1, ihour, int(float(imin1)), int(float(isec1)))
               idate   = dt+dtdelta
               ymdhms  = idate.strftime('%Y%m%d%H%M%S')
               ymdh    = idate.strftime('%Y%m%d%H')

               nc.prof[cnt]     = N_PROF0  + cnt
               nc.data_id[cnt]  = DATA_ID0 + cnt
               nc.data[cnt]     = sss1
               nc.err[cnt]      = err1
               nc.lon[cnt]      = lon1
               nc.lat[cnt]      = lat1
               nc.idate[cnt]    = ymdh
               nc.file_type[cnt] =file_type

            #print '   Obs ',fcnt, cnt, idate,lon1,lat1,sss1,err1,ymdh, nc.prof[cnt], nc.data_id[cnt]
            #sys.exit()
               cnt = cnt+1
        Ne = cnt

        nc.lon     = nc.lon[0:Ne]
        nc.lat     = nc.lat[0:Ne]
        nc.data    = nc.data[0:Ne]
        nc.err     = nc.err[0:Ne]
        nc.idate   = nc.idate[0:Ne]
        nc.prof    = nc.prof[0:Ne]
        nc.data_id = nc.data_id[0:Ne]
        nc.file_type = nc.file_type[0:Ne]

        INST_ID   = np.ones([Ne,1])*param.INST_ID
        QC_FLAG   = np.ones([Ne,1])
        QC_PRF    = np.ones([Ne,1])
        DATA_MODE = np.ones([Ne,1])
        NPTS      = np.ones([Ne,1])
        DEPTH     = np.ones([Ne,1])*0.0
        QC_LEV    = np.ones([Ne,1])*1.0

    # Sort by time
    #print '   before',nc.idate[0], nc.idate[-1]
        ind = np.argsort(nc.idate,axis=0)

        N_PROF    = nc.prof[ind,0]
        DATA_ID   = nc.data_id[ind,0]
        DATA      = nc.data[ind,0]
        OBS_ERROR = nc.err[ind,0]
        LON       = nc.lon[ind,0]
        LAT       = nc.lat[ind,0]
        DATE_TIME = nc.idate[ind,0]
        DATA_MODE = nc.file_type[ind,0]

        OBS_ERROR[OBS_ERROR<-999]=9.99e11
        if (fcnt==0):
           print ('     Create ', fcnt, cnt-1, N_PROF[cnt-1])
           write_nc_assim.write_nc_2D(outfile,param,N_PROF,DATA_ID,DATE_TIME,LON,LAT,NPTS,QC_FLAG,INST_ID,QC_PRF,DATA,OBS_ERROR,DEPTH,QC_LEV,DATA_MODE)
        else:
           print ('     Append ', fcnt, cnt-1,N_PROF[cnt-1])
           write_nc_assim.append_nc_2D(outfile,param,N_PROF,DATA_ID,DATE_TIME,LON,LAT,NPTS,QC_FLAG,INST_ID,QC_PRF,DATA,OBS_ERROR,DEPTH,QC_LEV,DATA_MODE)
 
        N_PROF0  = N_PROF[cnt-1]+1
        DATA_ID0 = DATA_ID[cnt-1]+1
    #print cnt, N_PROF0, DATA_ID0

    #sys.exit()
        fcnt = fcnt+1

    print ('   ',fcnt-1, DATE_TIME[0], DATE_TIME[-1])



