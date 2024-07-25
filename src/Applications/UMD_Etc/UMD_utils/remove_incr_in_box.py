#! /usr/bin/env python

from netCDF4 import Dataset
import sys
import os


#   Description :   add file2 to file1

#   Args: 
#		filename1 - temp_increment.nc or salt_increment.nc
# 		yyyy - year start of segment
# 		mm   - month start of segment
# 		dd   - day start of segment

fname=sys.argv[1]
yyyy=sys.argv[2]
mm=sys.argv[3]
dd=sys.argv[4]

SCRDIR = os.environ['SCRDIR']
EXPID  = os.environ['EXPID']
xleft  = os.environ['WESTLON']
xright = os.environ['EASTLON']
ybot   = os.environ['SOUTHLAT']
ytop   = os.environ['NORTHLAT']
bscale = os.environ['BOXSCALE']

file_input = Dataset(fname,'r+')
#   find ose002a.geosgcm_ocn3dT.20160614_1200z.nc4
#   this will change for production experiment
#latlon_file=SCRDIR+'/'+EXPID+'.geosgcm_ocn3dT.'+yyyy+mm+dd+'_1200z.nc4'
latlon_file=SCRDIR+'/'+EXPID+'.ocn_inst_6hr_glo_T1440x1080_z50.'+yyyy+mm+dd+'_1200z.nc4'
file_latlon = Dataset(latlon_file)  #  an example with the lat lons I need
#  do add file_input2 to file_input for temperature
if ( fname == 'temp_increment.nc'):
    temp  = file_input.variables['temp'][:,:,:,:]
if ( fname == 'salt_increment.nc'):
    temp  = file_input.variables['salt'][:,:,:,:]
xaxis = file_latlon.variables['lons'][0,:]    #[lat,lon]
yaxis = file_latlon.variables['lats'][:,0]    #[lat,lon]

#  find your box
lonmin=9999.
for i in range (0,1439):
    a = abs(float(xleft)-xaxis[i])
    if(a < lonmin):
        lonmin=a
        istart=i
lonmin=9999.
for i in range (0,1439):
    a = abs(float(xright)-xaxis[i])
    if(a < lonmin):
        lonmin=a
        iend=i
latmin=9999.
for j in range (0,1079):
    a = abs(float(ybot)-yaxis[j])
    if(a < latmin):
        latmin=a
        jstart=j
latmin=9999.
for j in range (0,1079):
    a = abs(float(ytop)-yaxis[j])
    if(a < latmin):
        latmin=a
        jend=j

print (istart, iend, jstart, jend)

boxscal=float(bscale)
for i in range (istart,iend):
    for j in range (jstart,jend):
#  temp[:,:,j,i] = 0.
       temp[:,:,j,i] = temp[:,:,j,i]*boxscal

print (temp.shape)

#print (xaxis)
#print (yaxis)
if (fname == 'temp_increment.nc'):
    file_input['temp'][:,:,:,:] = temp[:,:,:,:]
if (fname == 'salt_increment.nc'):
    file_input['salt'][:,:,:,:] = temp[:,:,:,:]
file_input.close()
