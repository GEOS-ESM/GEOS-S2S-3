#! /usr/bin/env python
# r(reg) must be 'nino1+2' 'nino3' 'nino3.4' 'nino1' 'nino2' 'nino4' 'tasi' 'idm' 'ta' 'idm_east' 'idm_west'
# comman line argument is "label" yyyymmddensmem, e.g. 20170401ens900

import numpy as np
import matplotlib.pyplot as plt
from netCDF4 import Dataset
import glob
import datetime
import string
import time
import calendar
import sys
import math
from scipy.io import netcdf
import getopt
import os

########################################################################################################################################
# daily sst for stratification
varname='TS'
collIC =  'geosgcm_sst'

# Default vlaues
reg='nino3.4'

# USER input
options, remainder = getopt.getopt(sys.argv[1:], 'r:')
print(options)
print( ' AVAILABLE OPTIONS:')
print( ' -r REGION: nino3, nino4, nino1+2, nino3.4, (not yet) nino1, nino2, tasi, ta, idm, idm_west, idm_east')
for opt, arg in options:
    print( opt, arg)
    if opt in ('-r'):
        reg=arg
    else:
      print( 'bad option')
if len(remainder) > 0:
   label = remainder[0]
else:
   print( 'Provide date and ens label. See run_indices_daily_labels.csh')
   exit()

idate = label[0:8]
emem = label[8:14]
imon = idate[4:6]
iyear = idate[0:4]
fmon = int(imon)+2  # forecast month fmon is the 2nd after the initialization month imon
fyear = int(iyear)
if fmon > 12:
   fmon = fmon - 12
   fyear = fyear + 1

listMO=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
fmonl = listMO[fmon-1]
print('idate',idate,'emem',emem,'imon',imon,'fyear',fyear,'fmonl', fmonl)

#######################################################################################
# Initialize from arg, check for existance
filebaseIN = '/gpfsm/dnb02/projects/p58/aogcm/g5fcst/forecast/study4ver3/geos-s2s/knakada_s2s3_unstable_11252019/' 
filebaseOUT = '/discover/nobackup/projects/gmao/oceanval/post_processing/data/S2S-3_0/TEST_DAILY_ENSO/knakada_s2s3_unstable_11252019/'

#######################################################################################
# days in the forecast month fmon
numdays = calendar.monthrange(int(iyear), fmon)[1]
print( fmon, numdays)

# get grids info NEED REGULAR GRID 720x361
#fcstgridfl = '/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/GRIDS/grid_spec_720x361.nc'

fcstgridfl = filebaseIN+'/'+idate+'/'+emem+'/'+collIC+'/'+idate+'.'+collIC+'.'+str(fyear)+str(fmon).zfill(2)+'02_0900z.nc4'  # this is special case for Mar01 start date

ncfile = Dataset(fcstgridfl, mode="r", clobber=True, format='NETCDF4')
fsst = np.squeeze(ncfile.variables['TS'][0,:,:])
fy, fx = np.shape(fsst)
print('fy,fx',fy,fx)
fmask = np.ones(np.shape(fsst))
badind = np.where(np.isinf(fsst).mask)
fmask[badind] = 0
ncfile.close()
#print(sum(np.ravel(fmask)))

# Define regions, Nino for now, add the rest later
# Nino3:   150W-90W, 5S-5N # iN3fcst = np.where(((flat > -5) & (flat < 5)) & ((flon > -150) & (flon < -90)))
# Nino3.4: 170W-120W, 5S-5N # iN34fcst = np.where(((flat > -5) & (flat < 5)) & ((flon > -170) & (flon < -120)))
# Nino4:   160E-150W, 5S-5N # iN4fcst = np.where(((flat > -5) & (flat < 5)) & ((flon > -200) & (flon < -150)))
# Nino1+2:  90W-80W, 10S-0 # iN12fcst = np.where(((flat > -10) & (flat < 0)) & ((flon > -90) & (flon < -80)))
# Nino1:  90W-80W, 10S-5S  
# Nino2:  90W-80W, 5S-0
# WTIO: 50E - 70E, 10S - 10N
# SETIO: 90E - 110E, 10S - 0
# NAT: 40W - 20W, 5N - 20N
# SAT: 15W - 5E, 20S - 5S
# TASI = NAT - SAT
# IDM = WTIO - SETIO

# indeces into lat and lon addrays (be careful! - redo for 720x361)
iN3fcstLON1 = 60  #260
iN3fcstLON2 = 181 #379
iN3fcstLAT1 = 170 #176
iN3fcstLAT2 = 191 #215
#-----------------------
iN34fcstLON1 = 20 #220 
iN34fcstLON2 = 121 #319
iN34fcstLAT1 = 170 #176
iN34fcstLAT2 = 191 #215
#-----------------------
iN4fcstLON1 = [680, 0] #iN4fcstLON1 = 160 # Nino4 is in 2 pieces in 720x361 grid
iN4fcstLON2 = [720, 61] #iN4fcstLON2 = 259
iN4fcstLAT1 = 170 #176
iN4fcstLAT2 = 191 #215
#-----------------------
iN12fcstLON1 = 180 #380
iN12fcstLON2 = 201 #399
iN12fcstLAT1 = 160 #156
iN12fcstLAT2 = 181 #195
#-----------------------
iN1fcstLON1 = 180 #380
iN1fcstLON2 = 201 #399
iN1fcstLAT1 = 160 #156
iN1fcstLAT2 = 171 #195
#-----------------------
iN2fcstLON1 = 180 #380
iN2fcstLON2 = 201 #399
iN2fcstLAT1 = 170 #156
iN2fcstLAT2 = 181 #195
#-----------------------
iWIfcstLON1 = 460
iWIfcstLON2 = 501
iWIfcstLAT1 = 160
iWIfcstLAT2 = 201 
#-----------------------
iSEIfcstLON1 = 540
iSEIfcstLON2 = 581
iSEIfcstLAT1 = 160
iSEIfcstLAT2 = 181
#-----------------------
iNATfcstLON1 = 280
iNATfcstLON2 = 321
iNATfcstLAT1 = 190
iNATfcstLAT2 = 221
#-----------------------
iSATfcstLON1 = 330
iSATfcstLON2 = 371
iSATfcstLAT1 = 140
iSATfcstLAT2 = 171
#-----------------------

if reg == 'nino1':
   lon1 = iN1fcstLON1
   lon2 = iN1fcstLON2
   lat1 = iN1fcstLAT1
   lat2 = iN1fcstLAT2
elif reg == 'nino2':
   lon1 = iN2fcstLON1
   lon2 = iN2fcstLON2
   lat1 = iN2fcstLAT1
   lat2 = iN2fcstLAT2
elif reg == 'nino3':
   lon1 = iN3fcstLON1
   lon2 = iN3fcstLON2
   lat1 = iN3fcstLAT1
   lat2 = iN3fcstLAT2
elif reg == 'nino4':
   lon1 = iN4fcstLON1
   lon2 = iN4fcstLON2
   lat1 = iN4fcstLAT1
   lat2 = iN4fcstLAT2
elif reg == 'nino1+2':
   lon1 = iN12fcstLON1
   lon2 = iN12fcstLON2
   lat1 = iN12fcstLAT1
   lat2 = iN12fcstLAT2
elif reg == 'nino3.4':
   lon1 = iN34fcstLON1
   lon2 = iN34fcstLON2
   lat1 = iN34fcstLAT1
   lat2 = iN34fcstLAT2
elif reg == 'tasi':
   lon1n = iNATfcstLON1
   lon2n = iNATfcstLON2
   lat1n = iNATfcstLAT1
   lat2n = iNATfcstLAT2
   lon1s = iSATfcstLON1
   lon2s = iSATfcstLON2
   lat1s = iSATfcstLAT1
   lat2s = iSATfcstLAT2
elif reg == 'idm':
   lon1w = iWIfcstLON1
   lon2w = iWIfcstLON2
   lat1w = iWIfcstLAT1
   lat2w = iWIfcstLAT2
   lon1e = iSEIfcstLON1
   lon2e = iSEIfcstLON2
   lat1e = iSEIfcstLAT1
   lat2e = iSEIfcstLAT2
elif reg == 'idm_east':
   lon1 = iSEIfcstLON1
   lon2 = iSEIfcstLON2
   lat1 = iSEIfcstLAT1
   lat2 = iSEIfcstLAT2
elif reg == 'idm_west':
   lon1 = iWIfcstLON1
   lon2 = iWIfcstLON2
   lat1 = iWIfcstLAT1
   lat2 = iWIfcstLAT2
elif reg == 'nat':
   lon1 = iNATfcstLON1
   lon2 = iNATfcstLON2
   lat1 = iNATfcstLAT1
   lat2 = iNATfcstLAT2
elif reg == 'sat':
   lon1 = iSATfcstLON1
   lon2 = iSATfcstLON2
   lat1 = iSATfcstLAT1
   lat2 = iSATfcstLAT2
else:
   print( 'incorrect region specified')
   exit()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Hplumes=np.zeros((numdays))  # for daily
Hplumes[:] = np.nan

# create release directory if it doesn't exist
release_dir = os.path.dirname(filebaseOUT+idate+'/'+str(fyear)+'/'+fmonl+'/')
print( 'resease_dir ',release_dir)
if not os.path.exists(release_dir):
    os.makedirs(release_dir)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# test for the hindcast to be done
fcstfltst = filebaseIN+'/'+idate+'/'+emem+'/'+collIC+'/'+idate+'.'+collIC+'.'+str(fyear)+str(fmon).zfill(2)+'02_0900z.nc4'  # this is special case for Mar01 start date
if not(os.path.exists(fcstfltst)):
   print( 'NO FCST FILE: ', fcstfltst)
   exit()
else:
   mem = emem[3:]
   fn = open(release_dir+'/DAILY_'+reg+'_'+mem+'.sst', 'w')
#   fn = open('./DAILY_'+reg+'_'+mem+'.sst', 'w')
   expstr = idate+'\n'
   fn.write(expstr)  
   format_out = []
   #for days in inputM:
   for d in range(1,numdays+1):
      filepathIN = idate+'/'+emem+'/'+collIC+'/'
      filenameIN = idate+'.'+collIC+'.'+str(fyear)+str(fmon).zfill(2)+str(d).zfill(2)+'_0900z.nc4'
      print( '_______________________________________________________')
      print( 'YEAR _______________________ ',str(fyear))
      print( 'MONTH ______________________ ',fmonl)
      print( 'DAY ________________________ ',d)
      print( 'filepathIN ______________________ ',filepathIN)
      print( 'filenameIN ______________________ ',filenameIN)
      print( '_______________________________________________________')
# this is special for Mar01 start (no d=1 file), leave index value as NaN
      if not(os.path.exists(filebaseIN+filepathIN+filenameIN)):
         print( 'NO FCST FILE: ', filebaseIN+filepathIN+filenameIN)
         exit()  
# Start processing members and ensembles loop
      else:
         Hfld = np.zeros((fx,fy))
         filename = filebaseIN+filepathIN+filenameIN
         print( 'filename: ',filename)
         ncfile = Dataset(filename, 'r', format='NETCDF4')
         F = np.squeeze(ncfile.variables[varname][:])
         try:
            time=ncfile.variables['time']    
            year=time.begin_date/10000
            month=(time.begin_date-year*10000)/100
            day=(time.begin_date-year*10000-month*100)
         except:
            print( 'No date in FCST file')
         ncfile.close()

         if reg=='nino4':
            Find0 = F[lat1:lat2,lon1[0]:lon2[0]]
            Find1 = F[lat1:lat2,lon1[1]:lon2[1]]
            Find = np.concatenate((Find0, Find1),axis=1)
            Rind = np.mean(Find)
         elif reg=='idm':
            Find0 = F[lat1w:lat2w,lon1w:lon2w]
            Find1 = F[lat1e:lat2e,lon1e:lon2e]
            Rind0 = np.mean(Find0) 
            Rind1 = np.mean(Find1)
            Rind = Rind0 - Rind1
         elif reg=='tasi':
            Find0 = F[lat1n:lat2n,lon1n:lon2n]
            Find1 = F[lat1s:lat2s,lon1s:lon2s]
            Rind0 = np.mean(Find0) 
            Rind1 = np.mean(Find1)
            Rind = Rind0 - Rind1
         else:
            Find = F[lat1:lat2,lon1:lon2]
            Rind = np.mean(Find)
         print( 'mem,d,ind: ', mem,d,Rind)
         Hplumes[d-1] = Rind
      format_out.append('%1.3f')

      dat = np.column_stack((Hplumes))
   np.savetxt(fn,dat,fmt=format_out,delimiter=' ')
   fn.close()

print( 'Hplumes: ')
print( Hplumes)

