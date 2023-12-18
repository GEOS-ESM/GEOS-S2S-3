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
def ild(year,month,svar,ver):
	data = dataclass()
		
	lvar = svar
	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST') ):
		lvar = 'T'
	if ( (svar=='s') | (svar=='sss') | (svar=='S') | (svar=='SSS')):
		lvar = 'S'
	
	val       = get_info.validation(ver,lvar,year)
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
	lev     = np.squeeze(odasgrp.variables['lev'][:])*1.0
	miss    = odasgrp.variables[lvar].missing_value
	units   = odasgrp.variables[lvar].units
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
def xz(year,month,slat,svar,ver):
	data = dataclass()

        lvar = svar
	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST') ):
		lvar = 'T'
	if ( (svar=='s') | (svar=='sss') | (svar=='S') | (svar=='SSS')):
		lvar = 'S'
	
	val       = get_info.validation(ver,lvar,year)
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
	lev     = np.squeeze(odasgrp.variables['lev'][:])*1.0
	miss    = odasgrp.variables[lvar].missing_value
	data.units = odasgrp.variables[lvar].units
        odasgrp.close()
	#tmp[tmp==miss]=np.nan

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
def xy(year,month,ilev,svar,ver):
	data = dataclass()
		
	lvar = svar
	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST') ):
		lvar = 'T'
	if ( (svar=='s') | (svar=='sss') | (svar=='S') | (svar=='SSS')):
		lvar = 'S'
	
	val       = get_info.validation(ver,lvar,year)
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
	lev     = np.squeeze(odasgrp.variables['lev'][:])*1.0
	miss    = odasgrp.variables[lvar].missing_value
	units   = odasgrp.variables[lvar].units
        odasgrp.close()
	#tmp[tmp==miss]=np.nan
	if ((units=='K') | (units=='kelvin')):
		tmp = tmp-273.14

	# Extract Depth
	# Extract Depth
	if lvar=='SSH':
		tmp  = np.squeeze(tmp[int(month)-1,:,:])
		zinc = 0
	else:
		z,zinc = read_grid.find_nearest(lev,float(ilev))
		tmp    = np.squeeze(tmp[int(month)-1,zinc,:,:])

	data.lon, data.lat = np.meshgrid(lon,lat)
	data.var = tmp
	data.lev = lev[zinc]
	data.id  = val.long_name

	print data.id, ':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0], data.lon[-1,-1]

	return (data)

###########################################################
def spd(year,month,ver):
	data = dataclass()

	var1 = 'U'
	var2 = 'V'

	val       = get_info.validation(ver,var1,year)
	fname     = val.fname
	data.file = 'T'
	if not os.path.isfile(fname):
		print 'File does not exist:'
		data.file='F'
		return (data)

        odasgrp   = Dataset(fname, 'r', format='NETCDF4')
        tmpu      = np.squeeze(odasgrp.variables[var1][:]) 
        tmpv      = np.squeeze(odasgrp.variables[var2][:]) 
	lon      = np.squeeze(odasgrp.variables['lon'][:]) 
	lat      = np.squeeze(odasgrp.variables['lat'][:]) 
        missing   = odasgrp.variables[var1].missing_value
	data.units = odasgrp.variables[var1].units
        odasgrp.close()

        tmpu[tmpu<=missing]=np.nan
        tmpv[tmpv<=missing]=np.nan

        data.var = np.sqrt(tmpu**2+tmpv**2)
	data.var = data.var[int(month)-1,0,:,:]
        jm, im = np.shape(data.var)

	data.lon, data.lat = np.meshgrid(lon,lat)
	data.id            = val.long_name

        print data.id, ':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), lon[0], lon[-1]

        return (data)

###########################################################



