#! /usr/bin/env python
from netCDF4 import Dataset
import numpy as np
import scipy.stats as stats
import glob
import os
import sys
import read_grid
import get_info
sys.path.append('/gpfsm/dnb42/projects/p17/gvernier/SAND_BOXES/FDV/repo/EDA')
import obs_operator
class dataclass:
	pass

###########################################################
def mon(year,month,svar):
	data = dataclass()

	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST') ):
		lvar = 'T'
	if ( (svar=='ice') | (svar=='aice') | (svar=='AICE') | (svar=='ICE')):
		lvar = 'AICE'

	
	val       = get_info.validation('reyn',lvar,year)
	fname     = val.fname
	data.file = 'T'
	if not os.path.isfile(fname):
		print 'File does not exist:'
		data.file='F'
		return (data)

        odasgrp = Dataset(fname, 'r', format='NETCDF4')
        tmp     = np.squeeze(odasgrp.variables[lvar][int(month)-1,:,:])*1.0
	lon     = np.squeeze(odasgrp.variables['lon'][:])*1.0
	lat     = np.squeeze(odasgrp.variables['lat'][:])*1.0	
	miss    = odasgrp.variables[lvar].missing_value
	data.units = odasgrp.variables[lvar].units
        odasgrp.close()
	tmp[tmp==miss]=np.nan
	if lvar=='AICE':
		tmp[tmp<0.15]=np.nan

	# Shift to start at 20E
  	#x,xinc    = read_grid.find_nearest(lon,20)
	#lon       = read_grid.shift_data_1D(lon,xinc)
	#tmp       = read_grid.shift_data_2D(tmp,xinc)
	#x360      = np.where((lon>=0) & (lon<x))
	#lon[x360] = lon[x360]+360 

	data.var = tmp 
	data.lon, data.lat = np.meshgrid(lon,lat)
	data.id  = val.long_name

        print data.id, ':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0], data.lon[-1,-1]

	return (data)

###########################################################
def year(year,svar):
	data = dataclass()

	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST') ):
		lvar = 'T'
	if ( (svar=='ice') | (svar=='aice') | (svar=='AICE') | (svar=='ICE')):
		lvar = 'AICE'

	
	val       = get_info.validation('reyn',lvar,year)
	fname     = val.fname
	data.file = 'T'
	if not os.path.isfile(fname):
		print 'File does not exist:'
		data.file='F'
		return (data)

        odasgrp = Dataset(fname, 'r', format='NETCDF4')
        tmp     = np.squeeze(odasgrp.variables[lvar][:])*1.0
	lon     = np.squeeze(odasgrp.variables['lon'][:])*1.0
	lat     = np.squeeze(odasgrp.variables['lat'][:])*1.0	
	miss    = odasgrp.variables[lvar].missing_value
	data.units = odasgrp.variables[lvar].units
        odasgrp.close()
	tmp[tmp==miss]=np.nan
	tmp = np.squeeze(stats.nanmean(tmp,0))
	if lvar=='AICE':
		tmp[tmp<0.15]=np.nan

	# Shift to start at 20E
  	#x,xinc    = read_grid.find_nearest(lon,20)
	#lon       = read_grid.shift_data_1D(lon,xinc)
	#tmp       = read_grid.shift_data_2D(tmp,xinc)
	#x360      = np.where((lon>=0) & (lon<x))
	#lon[x360] = lon[x360]+360 

	data.var = tmp 
	data.lon, data.lat = np.meshgrid(lon,lat)
	data.id  = val.long_name

        print data.id, ':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0], data.lon[-1,-1]

	return (data)
###########################################################
def mae_tri(year,svar):
	data = dataclass()

	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST') ):
		lvar = 'sst'
	if ( (svar=='ice') | (svar=='aice') | (svar=='AICE') | (svar=='ICE')):
		lvar = 'AICE'

	data.file = 'T'
	ogrid = obs_operator.Grid(filename='/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/RC/GRIDS/grid_spec.nc')
	jm,im = np.shape(ogrid.lon)

	data.var   = (np.zeros((12,jm,im)))*1.0
	for m in range(12):
        	smon = str(m+1)
		if (m+1)<=9:
			smon = '0'+smon
		tmpdir = '/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/VAL/720x410/REYN/'+year+'/avhrr-only-v2.'+year+smon+'*.nc'
		flist  =sorted(list(glob.glob(tmpdir)))
		cnt = len(flist)

		for fname in flist[:]:
        		odasgrp = Dataset(fname, 'r', format='NETCDF4')
        		tmp     = np.squeeze(odasgrp.variables[lvar][:])*1.0
        		odasgrp.close()
			tmp[tmp<=-2]=np.nan
			data.var[m,:,:] = tmp+data.var[m,:,:]
		data.var[m,:,:] =  data.var[m,:,:]/cnt
		#print year+smon, cnt, np.nanmin(data.var[m,:,:]), np.nanmax(data.var[m,:,:]), data.var[m,200,200]
	
	data.lat = ogrid.lat
	data.lon = ogrid.lon

        print 'Reyn:', np.nanmin(data.var), np.nanmax(data.var), data.var[0,200,200]

	return (data)
###########################################################



