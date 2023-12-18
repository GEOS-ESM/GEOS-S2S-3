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
def xz(year,month,ilat,svar):
	data = dataclass()

	if ( (svar=='sst') | (svar=='SST') ):
		lvar='T'
		lev = 0
	if ( (svar=='sss') | (svar=='SSS') ):
		lvar='S'
		lev = 0
	if ( (svar=='t') | (svar=='T') ):
		lvar='T'
	if ( (svar=='s') | (svar=='S') ):
		lvar='S'
	if ( (svar=='u') | (svar=='U') ):
		lvar = 'U'
	if ( (svar=='v') | (svar=='V') ):
		lvar = 'V'

	val       = get_info.validation('glorys',lvar,year)
	fname     = val.fname
	data.file = 'T'
	if not os.path.isfile(fname):
		print 'File does not exist:'
		data.file='F'
		return (data)

        odasgrp  = Dataset(fname, 'r', format='NETCDF4')
	lon      = np.squeeze(odasgrp.variables['lon'][:]) 
	lat      = np.squeeze(odasgrp.variables['lat'][:]) 
	lev      = np.squeeze(odasgrp.variables['lev'][:]) 
        tmp      = np.squeeze(odasgrp.variables[lvar][:]) 
        missing  = odasgrp.variables[lvar].missing_value
	data.units = odasgrp.variables[lvar].units
        odasgrp.close()
        #tmp[tmp==missing]=np.nan

	x360      = np.where(lon<0)
	lon[x360] = lon[x360]+360 

	# Extract Latitude
	y,yinc = read_grid.find_nearest(lat,float(ilat))
	tmp    = np.squeeze(tmp[int(month)-1,:,yinc,:])

	# Shift to start at 20E
  	x,xinc    = read_grid.find_nearest(lon,20)
	lon       = read_grid.shift_data_1D(lon,xinc)
	tmp       = read_grid.shift_data_2D(tmp,xinc)
	x360      = np.where((lon>=0) & (lon<x))
	lon[x360] = lon[x360]+360 

	data.var = tmp
        data.lon = lon
        data.lat = lat[yinc]
	data.lev = lev
	data.id  = val.long_name

        print val.long_name,': ', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0], data.lon[-1]
 
	return (data)
###########################################################
def spd(year,month):
	data = dataclass()

	var1 = 'U'
	var2 = 'V'
	val1  = get_info.validation('glorys',var1,year)
	val2  = get_info.validation('glorys',var2,year)
	data.file = 'T'
	if not os.path.isfile(val1.fname):
		print 'File does not exist:'
		data.file='F'
		return (data)

        odasgrp   = Dataset(val1.fname, 'r', format='NETCDF4')
	lon       = np.squeeze(odasgrp.variables['lon'][:]) 
	lat       = np.squeeze(odasgrp.variables['lat'][:]) 
	lev       = np.squeeze(odasgrp.variables['lev'][:]) 
        tmpu      = np.squeeze(odasgrp.variables[var1][:]) 
        missing   = odasgrp.variables[var1].missing_value
	data.units = odasgrp.variables[var1].units
        odasgrp.close()
        #tmpu[tmpu==missing]=np.nan

        odasgrp  = Dataset(val2.fname, 'r', format='NETCDF4')
        tmpv      = np.squeeze(odasgrp.variables[var2][:]) 
        odasgrp.close()
        #tmpv[tmpv==missing]=np.nan

        data.var = np.sqrt(tmpu**2+tmpv**2)
	data.var = data.var[int(month)-1,0,:,:]
        jm, im = np.shape(data.var)

	data.lon, data.lat = np.meshgrid(lon,lat)
	data.id            = val1.long_name

        print data.id, ':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), lon[0], lon[-1]

        return (data)
###########################################################
def xy(year,month,ilev,svar):
	data = dataclass()

	if ( (svar=='sst') | (svar=='SST') ):
		lvar='T'
		lev = 0
	if ( (svar=='sss') | (svar=='SSS') ):
		lvar='S'
		lev = 0
	if ( (svar=='t') | (svar=='T') ):
		lvar='T'
	if ( (svar=='s') | (svar=='S') ):
		lvar='S'
	if ( (svar=='u') | (svar=='U') ):
		lvar = 'U'
	if ( (svar=='v') | (svar=='V') ):
		lvar = 'V'

	val       = get_info.validation('glorys',lvar,year)
	fname     = val.fname
	data.file = 'T'
	if not os.path.isfile(fname):
		print 'File does not exist:'
		data.file='F'
		return (data)

        odasgrp  = Dataset(fname, 'r', format='NETCDF4')
	lon      = np.squeeze(odasgrp.variables['lon'][:]) 
	lat      = np.squeeze(odasgrp.variables['lat'][:]) 
	lev      = np.squeeze(odasgrp.variables['lev'][:]) 
        tmp      = np.squeeze(odasgrp.variables[lvar][:]) 
        missing  = odasgrp.variables[lvar].missing_value
	data.units = odasgrp.variables[lvar].units
        odasgrp.close()
        #tmp[tmp==missing]=np.nan

	x360      = np.where(lon<0)
	lon[x360] = lon[x360]+360 

	# Extract Depth
	z,zinc = read_grid.find_nearest(lev,float(ilev))
	tmp    = np.squeeze(tmp[int(month)-1,zinc,:,:])

	# Shift to start at 20E
  	#x,xinc    = read_grid.find_nearest(lon,20)
	#lon       = read_grid.shift_data_1D(lon,xinc)
	#tmp       = read_grid.shift_data_2D(tmp,xinc)
	#x360      = np.where((lon>=0) & (lon<x))
	#lon[x360] = lon[x360]+360 

	data.lon, data.lat = np.meshgrid(lon,lat)
	data.var = tmp
	data.lev = lev[zinc]
	data.id  = val.long_name

        print val.long_name,': ', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0], data.lon[-1,-1]
 
	return (data)
###########################################################
def xy_year(year,ilev,svar):
	data = dataclass()

	if ( (svar=='sst') | (svar=='SST') ):
		lvar='T'
		lev = 0
	if ( (svar=='sss') | (svar=='SSS') ):
		lvar='S'
		lev = 0
	if ( (svar=='t') | (svar=='T') ):
		lvar='T'
	if ( (svar=='s') | (svar=='S') ):
		lvar='S'
	if ( (svar=='u') | (svar=='U') ):
		lvar = 'U'
	if ( (svar=='v') | (svar=='V') ):
		lvar = 'V'

	val       = get_info.validation('glorys',lvar,year)
	fname     = val.fname
	data.file = 'T'
	if not os.path.isfile(fname):
		print 'File does not exist:'
		data.file='F'
		return (data)

        odasgrp  = Dataset(fname, 'r', format='NETCDF4')
	lon      = np.squeeze(odasgrp.variables['lon'][:]) 
	lat      = np.squeeze(odasgrp.variables['lat'][:]) 
	lev      = np.squeeze(odasgrp.variables['lev'][:]) 
        tmp      = np.squeeze(odasgrp.variables[lvar][:]) 
        missing  = odasgrp.variables[lvar].missing_value
	data.units = odasgrp.variables[lvar].units
        odasgrp.close()
        #tmp[tmp==missing]=np.nan
	tmp = np.squeeze(stats.nanmean(tmp,0))

	x360      = np.where(lon<0)
	lon[x360] = lon[x360]+360 

	# Extract Depth
	z,zinc = read_grid.find_nearest(lev,float(ilev))
	tmp    = np.squeeze(tmp[zinc,:,:])

	# Shift to start at 20E
  	#x,xinc    = read_grid.find_nearest(lon,20)
	#lon       = read_grid.shift_data_1D(lon,xinc)
	#tmp       = read_grid.shift_data_2D(tmp,xinc)
	#x360      = np.where((lon>=0) & (lon<x))
	#lon[x360] = lon[x360]+360 

	data.lon, data.lat = np.meshgrid(lon,lat)
	data.var = tmp
	data.lev = lev[zinc]
	data.id  = val.long_name

        print val.long_name,': ', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0], data.lon[-1,-1]
 
	return (data)
