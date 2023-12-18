#! /usr/bin/env python
from netCDF4 import Dataset
import numpy as np
import glob
import scipy.stats as stats
import matplotlib.pyplot as plt
import os
import sys
import read_grid
import get_info
import misc
sys.path.append('../ocn_monitor')
import get_ild
class dataclass:
	pass
###########################################################
def ild(year,month,svar):
	data = dataclass()
		
	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST') ):
		lvar = 'T'
	if ( (svar=='s') | (svar=='sss') | (svar=='S') | (svar=='SSS')):
		lvar = 'S'
	
	val       = get_info.validation('en4',lvar,year)
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
	tmp  = tmp+273.14
	tmp  = np.squeeze(tmp[int(month)-1,:,:,:])
	mask = np.ones(np.shape(tmp))
    	mask[misc.isNaN(tmp)]=0.0
	tmp[mask==0]=0.0
 	tmp = (tmp-(0.5*(tmp[0,:,:]+tmp[1,:,:])))*mask
	[km,jm,im] = np.shape(tmp)
	#plt.imshow(tmp[0,:,:])
	#plt.show()
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
def xz(year,month,slat,svar):
	data = dataclass()

	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST') ):
		lvar = 'T'
	if ( (svar=='s') | (svar=='sss') | (svar=='S') | (svar=='SSS')):
		lvar = 'S'
	
	val       = get_info.validation('en4',lvar,year)
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
def xz_raw(year,month,slat,svar):
	data = dataclass()
	
	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST') ):
		lvar = 'temperature'
	if ( (svar=='s') | (svar=='sss') | (svar=='S') | (svar=='SSS')):
		lvar = 'salinity'
	
     	fdir  = '/gpfsm/dnb42/gmaofcst/ocean/obs/odas-2/METOFFICE/EN4_v411/GRD/RAW/'
	fname = fdir+year+'/EN.4.1.1.f.analysis.g10.'+year+month+'.nc'

	val       = get_info.validation('en4',lvar,year)
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
	lev     = np.squeeze(odasgrp.variables['depth'][:])*1.0
	miss    = odasgrp.variables[lvar]._FillValue
	offset  = odasgrp.variables[lvar].add_offset
	scale   = odasgrp.variables[lvar].scale_factor
	data.units = odasgrp.variables[lvar].units
        odasgrp.close()
	tmp[tmp==miss]=np.nan
	tmp = tmp*scale
	tmp = tmp+offset
	if ((data.units=='K') | (data.units=='kelvin')):
		tmp = tmp-273.14
		data.units = 'C'

	# Extract latitude
	y,yinc    = read_grid.find_nearest(lat,float(slat))
	tmp       = np.squeeze(tmp[:,yinc,:])

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

	print data.id, ':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0], data.lon[-1,-1]

	return (data)

###########################################################
def xy(year,month,ilev,svar):
	data = dataclass()
		
	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST') ):
		lvar = 'T'
	if ( (svar=='s') | (svar=='sss') | (svar=='S') | (svar=='SSS')):
		lvar = 'S'
	
	val       = get_info.validation('en4',lvar,year)
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
def xy_year(year,ilev,svar):
	data = dataclass()
		
	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST') ):
		lvar = 'T'
	if ( (svar=='s') | (svar=='sss') | (svar=='S') | (svar=='SSS')):
		lvar = 'S'
	
	val       = get_info.validation('en4',lvar,year)
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
	tmp[tmp==miss]=np.nan
	tmp = np.squeeze(stats.nanmean(tmp,0))

	if ((units=='K') | (units=='kelvin')):
		tmp = tmp-273.14

	# Extract Depth
	z,zinc = read_grid.find_nearest(lev,float(ilev))
	tmp    = np.squeeze(tmp[zinc,:,:])

	data.lon, data.lat = np.meshgrid(lon,lat)
	data.var = tmp
	data.lev = lev[zinc]
	data.id  = val.long_name

	print data.id, ':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0], data.lon[-1,-1]

	return (data)




# OLD
###########################################################
def mon_yz(year,month,slon,svar):
 	# t, s
        class dataclass:
		pass
	data = dataclass()
	
	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST') ):
		lvar = 'temperature'
	if ( (svar=='s') | (svar=='sss') | (svar=='S') | (svar=='SSS')):
		lvar = 'salinity'
	
     	fdir  = '/gpfsm/dnb42/gmaofcst/ocean/obs/odas-2/METOFFICE/EN4_v411/GRD/RAW/'
	fname = fdir+year+'/EN.4.1.1.f.analysis.g10.'+year+month+'.nc'

        odasgrp = Dataset(fname, 'r', format='NETCDF4')
        tmp     = np.squeeze(odasgrp.variables[lvar][:])*1.0
	lon     = np.squeeze(odasgrp.variables['lon'][:])*1.0
	lat     = np.squeeze(odasgrp.variables['lat'][:])*1.0
	depth   = np.squeeze(odasgrp.variables['depth'][:])*1.0
	miss    = odasgrp.variables[lvar]._FillValue
	offset  = odasgrp.variables[lvar].add_offset
	scale   = odasgrp.variables[lvar].scale_factor
	units   = odasgrp.variables[lvar].units
        odasgrp.close()
	tmp[tmp==miss]=np.nan
	tmp = tmp*scale
	tmp = tmp+offset
	if ((units=='K') | (units=='kelvin')):
		tmp = tmp-273.14

	# Extract latitude
	rlon,ilon = read_grid.find_nearest(lon,float(slon))
	tmp       = np.squeeze(tmp[:,:,ilon])

	print 'EN4: ',np.nanmin(tmp), np.nanmax(tmp), np.shape(tmp), rlon
	[data.lon,data.lat] = np.meshgrid(lon,lat)
	data.x    = lon
	data.y    = lat
	data.var  = tmp
  	data.lev  = depth

	return (data)




