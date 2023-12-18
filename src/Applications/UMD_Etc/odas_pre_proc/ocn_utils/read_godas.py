#! /usr/bin/env python
from netCDF4 import Dataset
import numpy as np
import glob
import scipy.stats as stats
import os
import sys
import read_grid
import misc
import get_info
sys.path.append('../ocn_monitor')
import get_ild
class dataclass:
	pass
###########################################################
def ild(year,month,svar):
	data = dataclass()

	if ( (svar=='t') | (svar=='T') ):
		lvar='T'
	if ( (svar=='s') | (svar=='S') ):
		lvar='S'
	if ( (svar=='u') | (svar=='U') ):
		lvar = 'U'
	if ( (svar=='v') | (svar=='V') ):
		lvar = 'V'

	val       = get_info.validation('godas',lvar,year)
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

	tmp    = np.squeeze(tmp[int(month)-1,:,:,:])

	mask = np.ones(np.shape(tmp))
    	mask[misc.isNaN(tmp)]=0.0
	tmp[mask==0]=0.0
 	tmp = (tmp-(0.5*(tmp[0,:,:]+tmp[1,:,:])))*mask
	[km,jm,im] = np.shape(tmp)

	tmp  = get_ild.get_ild(tmp,lev,0.5,im,jm,km)

	# Shift to start at 20E
  	x,xinc    = read_grid.find_nearest(lon,20)
	lon       = read_grid.shift_data_1D(lon,xinc)
	data.var  = read_grid.shift_data_2D(tmp,xinc)
	x360      = np.where((lon>=0) & (lon<x))
	lon[x360] = lon[x360]+360 

	data.lon, data.lat = np.meshgrid(lon,lat)
	data.lev = lev
	data.id  = val.long_name

	print data.id, ':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0]

	return (data)
###########################################################
def xz(year,month,ilat,svar):
	data = dataclass()

	if ( (svar=='t') | (svar=='T') ):
		lvar='T'
	if ( (svar=='s') | (svar=='S') ):
		lvar='S'
	if ( (svar=='u') | (svar=='U') ):
		lvar = 'U'
	if ( (svar=='v') | (svar=='V') ):
		lvar = 'V'

	val       = get_info.validation('godas',lvar,year)
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

        tmp[tmp==missing]=np.nan

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
def xy(year,month,ilev,svar):
	data = dataclass()

	if ( (svar=='t') | (svar=='T') ):
		lvar='T'
	if ( (svar=='s') | (svar=='S') ):
		lvar='S'
	if ( (svar=='u') | (svar=='U') ):
		lvar = 'U'
	if ( (svar=='v') | (svar=='V') ):
		lvar = 'V'

	val       = get_info.validation('godas',lvar,year)
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

        tmp[tmp==missing]=np.nan

	# Extract Depth
	z,zinc = read_grid.find_nearest(lev,float(ilev))
	tmp    = np.squeeze(tmp[int(month)-1,zinc,:,:])

	data.lon, data.lat = np.meshgrid(lon,lat)
	data.var = tmp
	data.lev = lev[zinc]
	data.id  = val.long_name

        print val.long_name,': ', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0], data.lon[-1,-1]
 
	return (data)
###########################################################
def xy_year(year,ilev,svar):
	data = dataclass()

	if ( (svar=='t') | (svar=='T') ):
		lvar='T'
	if ( (svar=='s') | (svar=='S') ):
		lvar='S'
	if ( (svar=='u') | (svar=='U') ):
		lvar = 'U'
	if ( (svar=='v') | (svar=='V') ):
		lvar = 'V'

	val       = get_info.validation('godas',lvar,year)
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
        tmp[tmp==missing]=np.nan
	tmp = np.squeeze(stats.nanmean(tmp,0))

	# Extract Depth
	z,zinc = read_grid.find_nearest(lev,float(ilev))
	tmp    = np.squeeze(tmp[zinc,:,:])

	data.lon, data.lat = np.meshgrid(lon,lat)
	data.var = tmp
	data.lev = lev[zinc]
	data.id  = val.long_name

        print val.long_name,': ', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0], data.lon[-1,-1]
 
	return (data)

###########################################################
def xz_raw(year,month,ilat,svar):
	data = dataclass()	

     	fdir   = '/gpfsm/dnb42/gmaofcst/ocean/obs/odas-2/GODAS/RAW/'
	if ( (svar=='u') | (svar=='U') ):
		lvar = 'ucur'
	if ( (svar=='v') | (svar=='V') ):
		lvar = 'vcur'
	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST') ):
		lvar = 'pottmp'
	if ( (svar=='s') | (svar=='sss') | (svar=='S') | (svar=='SSS')):
		lvar = 'salt'

	fname   = '/gpfsm/dnb42/gmaofcst/ocean/obs/odas-2/GODAS/RAW/'+lvar+'/'+lvar+'.'+year+'.nc'
	data.file = 'T'
	if not os.path.isfile(fname):
		print 'File does not exist:'
		data.file='F'
		return (data)

        odasgrp    = Dataset(fname, 'r', format='NETCDF4')
        tmp        = np.squeeze(odasgrp.variables[lvar][:])*1.0
	lon        = np.squeeze(odasgrp.variables['lon'][:])*1.0
	lat        = np.squeeze(odasgrp.variables['lat'][:])*1.0
	lev        = np.squeeze(odasgrp.variables['level'][:])*1.0
	miss       = odasgrp.variables[lvar]._FillValue
	offset     = odasgrp.variables[lvar].add_offset
	scale      = odasgrp.variables[lvar].scale_factor
	data.units = odasgrp.variables[lvar].units
        odasgrp.close()
	tmp[tmp==miss]=np.nan
	tmp = tmp*scale
	tmp = tmp+offset
	if data.units=='kg/kg':
		tmp = tmp*1000
	if ((data.units=='K') | (data.units=='kelvin')):
		tmp = tmp-273.14

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
	data.id  = 'GODAS'

        print data.id, ':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0], data.lon[-1]

	return (data)

