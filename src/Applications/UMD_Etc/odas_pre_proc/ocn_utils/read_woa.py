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

# World Ocean Atlas 2009
# /gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/LEVITUS
# /gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/VAL/360x181/LEV
###########################################################
def xy_mon_clim(month,lev,svar):

	data = dataclass()

 	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST')  | (svar=='ts') | (svar=='TS')  ):
		lvar = 'T'
	if ( (svar=='s') | (svar=='sss') | (svar=='S') | (svar=='SSS')  | (svar=='ss') | (svar=='SS') ):
		lvar = 'S'

	print svar, lvar
 	val       = get_info.validation('woa',lvar,'clim')
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
	depth   = np.squeeze(odasgrp.variables['lev'][:])*1.0
	miss    = odasgrp.variables[lvar].missing_value
        odasgrp.close()
	#tmp[tmp==miss]=np.nan
	tmp = np.squeeze(tmp[int(month)-1,int(lev),:,:])

	data.lon, data.lat = np.meshgrid(lon,lat)
	data.var = tmp
  	data.lev = depth[lev]
	data.id  = val.long_name

	print data.id, ': ',np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,-1]

	return (data)
###########################################################
def xy_clim(lev,svar):
	data = dataclass()

 	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST')  | (svar=='ts') | (svar=='TS')  ):
		lvar = 'T'
	if ( (svar=='s') | (svar=='sss') | (svar=='S') | (svar=='SSS')  | (svar=='ss') | (svar=='SS') ):
		lvar = 'S'

 	val       = get_info.validation('woa',lvar,'clim')
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
	depth   = np.squeeze(odasgrp.variables['lev'][:])*1.0
	miss    = odasgrp.variables[lvar].missing_value
        odasgrp.close()
	tmp[tmp==miss]=np.nan
	tmp = np.squeeze(tmp[:,int(lev),:,:])
	tmp = stats.nanmean(tmp,0)

	data.lon, data.lat = np.meshgrid(lon,lat)
	data.var = tmp
  	data.lev = depth[lev]
	data.id  = val.long_name

	print data.id, ': ',np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,-1]

	return (data)


