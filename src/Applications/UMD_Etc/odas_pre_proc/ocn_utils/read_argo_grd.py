from netCDF4 import Dataset
import numpy as np
import glob
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
		lvar = 'TEMP'
	if ( (svar=='s') | (svar=='sss') | (svar=='S') | (svar=='SSS')):
		lvar = 'PSAL'

	val  = get_info.validation('argo',lvar,year)
	fdir = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/ARGO_GRD/RAW/'

	# 201410+ realtime is daily
	if ( (year<='2013') & (year>='2010') ):
		flist = sorted(list(glob.glob(fdir+'NRTOAGL01/'+year+'/OA_NRTOAGL01_'+year+month+'*_fld_'+lvar+'.nc')))
	if ( (year=='2014') & (month<'10') ):
		flist = sorted(list(glob.glob(fdir+'NRTOAGL01/'+year+'/OA_NRTOAGL01_'+year+month+'*_fld_'+lvar+'.nc')))
	if ( (year=='2014') & (month>='10') ):
		flist = sorted(list(glob.glob(fdir+'RTOAGL01/OA_RTOAGL01_'+year+month+'*_fld_'+lvar+'.nc')))
	if ( (year>='2015')):
		flist = sorted(list(glob.glob(fdir+'RTOAGL01/OA_RTOAGL01_'+year+month+'*_fld_'+lvar+'.nc')))

	fname     = flist[0]
	data.file = 'T'
	if not os.path.isfile(fname):
		print 'File does not exist:'
		data.file='F'
		return (data)

	data.var  =  0
        for fname in flist:

        	odasgrp = Dataset(fname, 'r', format='NETCDF4')
        	tmp     = np.squeeze(odasgrp.variables[lvar][:])*1.0
		lon     = np.squeeze(odasgrp.variables['longitude'][:])*1.0
		lat     = np.squeeze(odasgrp.variables['latitude'][:])*1.0
		try:
			depth   = np.squeeze(odasgrp.variables['depth'][:])*1.0
		except:
			depth   = np.squeeze(odasgrp.variables['DEPTH'][:])*1.0
		miss    = odasgrp.variables[lvar]._FillValue
		offset  = odasgrp.variables[lvar].add_offset
		scale   = odasgrp.variables[lvar].scale_factor
		units   = odasgrp.variables[lvar].units
        	odasgrp.close()
		tmp[tmp==miss]=np.nan
		tmp = tmp*scale
		tmp = tmp+offset
                data.var = tmp+data.var	
        data.var = data.var/len(flist)
	tmp = data.var

	mask = np.ones(np.shape(tmp))
    	mask[misc.isNaN(tmp)]=0.0
	tmp[mask==0]=0.0
 	tmp = (tmp-(0.5*(tmp[0,:,:]+tmp[1,:,:])))*mask
	[km,jm,im] = np.shape(tmp)
	tmp  = get_ild.get_ild(tmp,depth,0.5,im,jm,km)

	# Shift Grid
	lon,sinc = read_grid.shift_0(lon)
	data.var = read_grid.shift_data_2D(tmp,sinc)

	data.lon, data.lat = np.meshgrid(lon,lat)
  	data.lev = depth
	data.id  = val.long_name

	print data.id, ':',np.nanmin(tmp), np.nanmax(tmp), np.shape(tmp), lon[0]

	return (data)

###########################################################
def mon(year,month,ilat,svar):
	data = dataclass()
	
	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST') ):
		lvar = 'TEMP'
	if ( (svar=='s') | (svar=='sss') | (svar=='S') | (svar=='SSS')):
		lvar = 'PSAL'

	val  = get_info.validation('argo',lvar,year)
	
	# 201410+ realtime is daily
     	fdir  = '/gpfsm/dnb42/gmaofcst/ocean/obs/odas-2/ARGO_GRD/RAW/'
	flist = sorted(list(glob.glob(fdir+'NRTOAGL01/'+year+'/OA_NRTOAGL01_'+year+month+'*_fld_'+lvar+'.nc')))
	dvar  = 'DEPTH'

	if ( (year=='2014') & (month<'10') ):
		flist = sorted(list(glob.glob(fdir+'NRTOAGL01/'+year+'/OA_NRTOAGL01_'+year+month+'*_fld_'+lvar+'.nc')))
		dvar  = 'DEPTH'

	if ( (year=='2014') & (month>='10') ):
		flist = sorted(list(glob.glob(fdir+'RTOAGL01/OA_RTOAGL01_'+year+month+'*_fld_'+lvar+'.nc')))
		dvar  = 'depth'


	fname     = flist[0]
	data.file = 'T'
	if not os.path.isfile(fname):
		print 'File does not exist:'
		data.file='F'
		return (data)

	data.var  =  0
        for fname in flist:
		#print fname
        	odasgrp = Dataset(fname, 'r', format='NETCDF4')
        	tmp     = np.squeeze(odasgrp.variables[lvar][:])*1.0
		lon     = np.squeeze(odasgrp.variables['longitude'][:])*1.0
		lat     = np.squeeze(odasgrp.variables['latitude'][:])*1.0
		depth   = np.squeeze(odasgrp.variables[dvar][:])*1.0
		miss    = odasgrp.variables[lvar]._FillValue
		offset  = odasgrp.variables[lvar].add_offset
		scale   = odasgrp.variables[lvar].scale_factor
		units   = odasgrp.variables[lvar].units
        	odasgrp.close()
		tmp[tmp==miss]=np.nan
		tmp = tmp*scale
		tmp = tmp+offset
		tmp = np.squeeze(tmp[int(lev),:,:])
                data.var = tmp+data.var	
        data.var = data.var/len(flist)

	# Shift Grid
	lon,sinc = read_grid.shift_0(lon)
	data.var = read_grid.shift_data_2D(data.var,sinc)

	[lon,lat] = np.meshgrid(lon,lat)
	data.lon = lon
	data.lat = lat
  	data.lev = depth[lev]
	data.id  = val.long_name

	print data.id, ':',np.nanmin(tmp), np.nanmax(tmp), np.shape(tmp), lon[0], depth[lev]

	return (data)
###########################################################
def xz(year,month,slat,svar):
	data = dataclass()

	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST') ):
		lvar = 'TEMP'
	if ( (svar=='s') | (svar=='sss') | (svar=='S') | (svar=='SSS')):
		lvar = 'PSAL'

	val       = get_info.validation('argo',lvar,year)

	# 201410+ realtime is daily
     	fdir  = '/gpfsm/dnb42/gmaofcst/ocean/obs/odas-2/ARGO_GRD/RAW/'
	flist = sorted(list(glob.glob(fdir+'NRTOAGL01/'+year+'/OA_NRTOAGL01_'+year+month+'*_fld_'+lvar+'.nc')))
	dvar  = 'DEPTH'

	if ( (year=='2014') & (month<'10') ):
		flist = sorted(list(glob.glob(fdir+'NRTOAGL01/'+year+'/OA_NRTOAGL01_'+year+month+'*_fld_'+lvar+'.nc')))
		dvar  = 'DEPTH'

	if ( (year=='2014') & (month>='10') ):
		flist = sorted(list(glob.glob(fdir+'RTOAGL01/OA_RTOAGL01_'+year+month+'*_fld_'+lvar+'.nc')))
		dvar  = 'depth'

	fname     = flist[0]
	data.file = 'T'
	if not os.path.isfile(fname):
		print 'File does not exist:'
		data.file='F'
		return (data)

	data.var  =  0
        for fname in flist:
		#print fname
        	odasgrp = Dataset(fname, 'r', format='NETCDF4')
        	tmp     = np.squeeze(odasgrp.variables[lvar][:])*1.0
		lon     = np.squeeze(odasgrp.variables['longitude'][:])*1.0
		lat     = np.squeeze(odasgrp.variables['latitude'][:])*1.0
		depth   = np.squeeze(odasgrp.variables[dvar][:])*1.0
		miss    = odasgrp.variables[lvar]._FillValue
		offset  = odasgrp.variables[lvar].add_offset
		scale   = odasgrp.variables[lvar].scale_factor
		units   = odasgrp.variables[lvar].units
        	odasgrp.close()
		tmp[tmp==miss]=np.nan
		tmp = tmp*scale
		tmp = tmp+offset
                data.var = tmp+data.var	
        data.var = data.var/len(flist)

	# Extract latitude
	y,yinc    = read_grid.find_nearest(lat,float(slat))
	tmp       = np.squeeze(data.var[:,yinc,:])

	# Shift to start at 20E
	x360      = np.where((lon<0))
	lon[x360] = lon[x360]+360 
  	x,xinc    = read_grid.find_nearest(lon,20)
	lon       = read_grid.shift_data_1D(lon,xinc)
	tmp       = read_grid.shift_data_2D(tmp,xinc)
	x360      = np.where((lon>=0) & (lon<x))
	lon[x360] = lon[x360]+360 

	data.var = tmp
        data.lon = lon
        data.lat = lat[yinc]
	data.lev = depth
	data.id  = val.long_name

        print data.id, ':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0], data.lon[-1]

	return (data)

