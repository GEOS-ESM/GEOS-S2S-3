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
def native_spd(year,month):
	data = dataclass()

	var1 = 'u'
	var2 = 'v'

	val       = get_info.validation('soda-native',var1,year)
	fname     = val.fname
	data.file = 'T'
	if not os.path.isfile(fname):
		print 'File does not exist:'
		data.file='F'
		return (data)

        odasgrp   = Dataset(fname, 'r', format='NETCDF4')
        tmpu      = np.squeeze(odasgrp.variables[var1][:]) 
        tmpv      = np.squeeze(odasgrp.variables[var2][:]) 
	lon      = np.squeeze(odasgrp.variables['longitude'][:]) 
	lat      = np.squeeze(odasgrp.variables['latitude'][:]) 
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
# native soda ice
def native_ice(year,month,svar):
	data = dataclass()

	if ( (svar=='aice') | (svar=='AICE') ):
		lvar='cn'
	if ( (svar=='hice') | (svar=='HICE') ):
		lvar='hi'  

	val       = get_info.validation('soda-ice',lvar,year)
	fname     = val.fname
	data.file = 'T'
	if not os.path.isfile(fname):
		print 'File does not exist:'
		data.file='F'
		return (data)

        odasgrp  = Dataset(fname, 'r', format='NETCDF4')
	lon      = np.squeeze(odasgrp.variables['longitude'][:]) 
	lat      = np.squeeze(odasgrp.variables['latitude'][:])  
        tmp      = np.squeeze(odasgrp.variables[lvar][:]) 
        missing  = odasgrp.variables[lvar].missing_value
	data.units = odasgrp.variables[lvar].units
        odasgrp.close()
        tmp[tmp==missing]=np.nan

	if lvar=='cn':
		tmp    = np.squeeze(tmp[int(month)-1,:,:,:])
		tmp = np.sum(tmp,0)
		tmp[tmp<0.15]=np.nan
	else:
		tmp    = np.squeeze(tmp[int(month)-1,:,:])
	

	data.lon, data.lat = np.meshgrid(lon,lat)
	data.var = tmp
	data.id  = val.long_name

        print val.long_name,': ', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0], data.lon[-1,-1]
 
	return (data)
################################################################################
# native soda
def xy_native(year,month,ilev,svar):
	data = dataclass()

	if ( (svar=='t') | (svar=='T') ):
		lvar='temp'
	if ( (svar=='s') | (svar=='S') ):
		lvar='salt'
	if ( (svar=='u') | (svar=='U') ):
		lvar = 'u'
	if ( (svar=='v') | (svar=='V') ):
		lvar = 'Vv'
	if ( (svar=='ssh') | (svar=='adt') ):
		lvar = 'ssh'

	val       = get_info.validation('soda-native',lvar,year)
	fname     = val.fname
	data.file = 'T'
	if not os.path.isfile(fname):
		print 'File does not exist:'
		data.file='F'
		return (data)

        odasgrp  = Dataset(fname, 'r', format='NETCDF4')
	lon      = np.squeeze(odasgrp.variables['longitude'][:]) 
	lat      = np.squeeze(odasgrp.variables['latitude'][:]) 
	lev      = np.squeeze(odasgrp.variables['depth'][:]) 
        tmp      = np.squeeze(odasgrp.variables[lvar][:]) 
        missing  = odasgrp.variables[lvar].missing_value
	data.units = odasgrp.variables[lvar].units
        odasgrp.close()

        tmp[tmp==missing]=np.nan

	# Extract Depth
	if lvar=='ssh':
		tmp    = np.squeeze(tmp[int(month)-1,:,:])
	else:
		z,zinc = read_grid.find_nearest(lev,float(ilev))
		tmp    = np.squeeze(tmp[int(month)-1,zinc,:,:])
		data.lev = lev[zinc]

	data.lon, data.lat = np.meshgrid(lon,lat)
	data.var = tmp

	data.id  = val.long_name

        print val.long_name,': ', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0], data.lon[-1,-1]
 
	return (data)

###########################################################
# 1-deg
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

	val       = get_info.validation('soda',lvar,year)
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
	#print tmp[0,0,90,200]
        #tmp[tmp>=missing]=np.nan

	x360      = np.where(lon<0)
	lon[x360] = lon[x360]+360 

	# Extract Latitude
	y,yinc = read_grid.find_nearest(lat,float(ilat))
	#print tmp[0,0,90,200]
	#print int(month)-1,yinc
	tmp    = np.squeeze(tmp[int(month)-1,:,yinc,:])
	#print np.nanmin(tmp), np.nanmax(tmp)

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
# 1-deg
def mon(year,month,ilev,svar):
	data = dataclass()
		
	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST') ):
		lvar = 'T'
	if ( (svar=='s') | (svar=='sss') | (svar=='S') | (svar=='SSS')):
		lvar = 'S'
	
	val       = get_info.validation('soda',lvar,year)
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
	z,zinc = read_grid.find_nearest(lev,float(ilev))
	tmp    = np.squeeze(tmp[int(month)-1,zinc,:,:])

	data.lon, data.lat = np.meshgrid(lon,lat)
	data.var = tmp
	data.lev = lev[zinc]
	data.id  = val.long_name

	print data.id, ':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0], data.lon[-1,-1]

	return (data)

###########################################################
# 1-deg
def ild(year,month,svar):
	data = dataclass()
		
	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST') ):
		lvar = 'T'
	if ( (svar=='s') | (svar=='sss') | (svar=='S') | (svar=='SSS')):
		lvar = 'S'
	
	val       = get_info.validation('soda',lvar,year)
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


