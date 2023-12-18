#! /usr/bin/env python
from netCDF4 import Dataset
import numpy as np
import glob
import scipy.stats as stats
import os
import read_grid
import get_info
class dataclass:
	pass

###########################################################
def xz(year,month,slat,svar):
	data = dataclass()

	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST') ):
		lvar = 'T'
	if ( (svar=='s') | (svar=='sss') | (svar=='S') | (svar=='SSS')):
		lvar = 'S'
	
	val       = get_info.validation('a3d',lvar,year)
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
	#tmp[tmp==miss]=np.nan

	cdir = '/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/VAL/360x181/A3D/clim/'
        odasgrp = Dataset(cdir+'A3D_clim_1993_2010.nc', 'r', format='NETCDF4')
	lev     = np.squeeze(odasgrp.variables['lev'][:])*1.0
        odasgrp.close()

	# Extract latitude
	y,yinc    = read_grid.find_nearest(lat,float(slat))
	tmp       = np.squeeze(tmp[int(month)-1,:,yinc,:])

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

        print data.id, ':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0], data.lon[-1]

	return (data)

###########################################################
def xy(year,month,ilev,svar):
	data = dataclass()

	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST') ):
		lvar = 'T'
	if ( (svar=='s') | (svar=='sss') | (svar=='S') | (svar=='SSS')):
		lvar = 'S'
	
	val       = get_info.validation('a3d',lvar,year)
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
	#tmp[tmp==miss]=np.nan

	cdir = '/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/VAL/360x181/A3D/clim/'
        odasgrp = Dataset(cdir+'A3D_clim_1993_2010.nc', 'r', format='NETCDF4')
	lev     = np.squeeze(odasgrp.variables['lev'][:])*1.0
        odasgrp.close()

	# Extract Depth
	z,zinc = read_grid.find_nearest(lev,float(ilev))
	tmp    = np.squeeze(tmp[int(month)-1,zinc,:,:])

	data.lon, data.lat = np.meshgrid(lon,lat)
	data.var = tmp
	data.lev = lev[zinc]
	data.id  = val.long_name

        print data.id, ':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0], data.lon[-1,-1]

	return (data)

###########################################################
def xy_year(year,ilev,svar):
	data = dataclass()

	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST') ):
		lvar = 'T'
	if ( (svar=='s') | (svar=='sss') | (svar=='S') | (svar=='SSS')):
		lvar = 'S'
	
	val       = get_info.validation('a3d',lvar,year)
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
	#tmp[tmp==miss]=np.nan
	tmp = np.squeeze(stats.nanmean(tmp,0))

	cdir = '/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/VAL/360x181/A3D/clim/'
        odasgrp = Dataset(cdir+'A3D_clim_1993_2010.nc', 'r', format='NETCDF4')
	lev     = np.squeeze(odasgrp.variables['lev'][:])*1.0
        odasgrp.close()

	# Extract Depth
	z,zinc = read_grid.find_nearest(lev,float(ilev))
	tmp    = np.squeeze(tmp[zinc,:,:])

	data.lon, data.lat = np.meshgrid(lon,lat)
	data.var = tmp
	data.lev = lev[zinc]
	data.id  = val.long_name

        print data.id, ':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0], data.lon[-1,-1]

	return (data)
###########################################################
def xz_raw(year,month,ilat,svar):
	data = dataclass()

	if ( (svar=='sst') | (svar=='SST') ):
		lvar='temperature'
		lev = 0
	if  ( (svar=='sss') | (svar=='SSS') ):
		lvar='salinity'
		lev = 0
	if ( (svar=='t') | (svar=='T') ):
		lvar='temperature'
	if ( (svar=='s') | (svar=='S') ):
		lvar='salinity'
	if ( (svar=='u') | (svar=='U') ):
		lvar = 'zvelocity'
	if ( (svar=='v') | (svar=='V') ):
		lvar = 'mvelocity'

	if int(year) <= 2014:
		fdir  = '/gpfsm/dnb42/gmaofcst/ocean/obs/odas-2/ARMOR_3D/RAW/MONTHLY/'
		flist = sorted(list(glob.glob(fdir+'ARMOR3D_REPv3-1_'+year+month+'*.nc')))
	if int(year) >= 2015:
		fdir  = '/gpfsm/dnb42/gmaofcst/ocean/obs/odas-2/ARMOR_3D/RAW/NRT/'
		flist = sorted(list(glob.glob(fdir+'ARMOR3D_TSHUV_'+year+month+'*.nc')))

	if len(flist)>=1:
		data.file = 'T'
	else:
		print 'File does not exist:'
		data.file='F'
		return (data)

	data0  =  0
        for fname in flist:
        	odasgrp = Dataset(fname, 'r', format='NETCDF4')
        	tmp     = np.squeeze(odasgrp.variables[lvar][:])*1.0
		lon     = np.squeeze(odasgrp.variables['longitude'][:])*1.0
		lat     = np.squeeze(odasgrp.variables['latitude'][:])*1.0
		lev     = np.squeeze(odasgrp.variables['depth'][:])*1.0
		data.units = odasgrp.variables[lvar].units
		miss    = odasgrp.variables[lvar]._FillValue
		scale   = odasgrp.variables[lvar].scale_factor
		if ((lvar=='temperature') | (lvar=='salinity')):
			offset  = odasgrp.variables[lvar].add_offset
        	odasgrp.close()
		tmp[tmp==miss]=np.nan
		tmp = tmp*scale
		if ((lvar=='temperature') | (lvar=='salinity')):
			tmp = tmp+offset	

                data0 = tmp+data0

 	data0 = data0/len(flist)

	x360      = np.where(lon<0)
	lon[x360] = lon[x360]+360 

	# Extract Latitude
	y,yinc = read_grid.find_nearest(lat,float(ilat))
	tmp    = np.squeeze(data0[:,yinc,:])

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
	data.id  = 'ARMOR3D'

        print 'A3D:', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0], data.lon[-1]
 
	return (data)
###########################################################


