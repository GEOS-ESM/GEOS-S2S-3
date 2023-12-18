#! /usr/bin/env python
from netCDF4 import Dataset
import numpy as np
import scipy.stats as stats
import glob
import os
import read_grid
import get_info
class dataclass:
	pass

###########################################################
def mon(year,month,svar):
	data = dataclass()

	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST') ):
		lvar = 'T'
	if ( (svar=='ice') | (svar=='aice') | (svar=='AICE') | (svar=='ICE')):
		lvar = 'AICE'

	
	val       = get_info.validation('ostia',lvar,year)
	fname     = val.fname
	data.file = 'T'
	if not os.path.isfile(fname):
		print 'OSTIA File does not exist:',fname
		data.file='F'
		return (data)
	if ( (int(year)==2006) & (int(month)<4) ):
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

	
	val       = get_info.validation('ostia',lvar,year)
	fname     = val.fname
	data.file = 'T'
	if not os.path.isfile(fname):
		print 'OSTIA File does not exist:',fname
		data.file='F'
		return (data)
	if ( (int(year)==2006) ):
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


