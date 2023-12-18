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
def osi(year,month,svar):
	data = dataclass()

	if ( (svar=='ICE_DRIFT') ):
		lvar = 'ice_drift'

	val       = get_info.validation('osi',lvar,year)
	fname     = val.fname
	data.file = 'T'
	if not os.path.isfile(fname):
		print 'File does not exist:'
		data.file='F'
		return (data)

	flist = glob.glob(val.valdir+'ice_drift_nh_polstere-625_multi-oi_'+year+month+'*.nc')
	
	jm = 177
	im = 119
	
	tm     = np.shape(flist)[0]
	tmpspd = np.zeros((tm,jm,im))
	tmpu   = np.zeros((tm,jm,im))
	tmpv   = np.zeros((tm,jm,im))

	cnt = 0
        for fname in flist:
        	odasgrp  = Dataset(fname, 'r', format='NETCDF4')
		lon0 = np.squeeze(odasgrp.variables['lon'][:]) # lon coordinate at grid origin (at time <start_date>)
		lat0 = np.squeeze(odasgrp.variables['lat'][:]) # lat coordinate at grid origin (at time <start_date>)
		dx   = np.squeeze(odasgrp.variables['dX'][:])  # km component of displacement along the x axis of grid
		dy   = np.squeeze(odasgrp.variables['dY'][:])  # km component of displacement along the y axis of grid
        	odasgrp.close()
		miss2 = -1.e+10
		dx[dx==miss2]=np.nan
		dy[dy==miss2]=np.nan
		#km to meters
		dx = dx*1000
		dy = dy*1000
		#dt = 2  # days
		#dt = 48 # hours
		dt     = 172800 # seconds
		u      = dx/dt
		v      = dy/dt
		spd    = np.sqrt((u**2 + v**2))	

		tmpspd[cnt,:,:] = spd
		tmpu[cnt,:,:] = u
		tmpv[cnt,:,:] = v

		cnt = cnt+1

	data.var = stats.nanmean(tmpspd,0)
	data.ui  = stats.nanmean(tmpu,0)
	data.vi  = stats.nanmean(tmpv,0)

	data.lon = lon0
	data.lat = lat0
	data.id  = val.long_name

        print data.id, ':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var)
	
	return (data)
###########################################################

def nsidc(year,month,svar):
	data = dataclass()

	val       = get_info.validation('nsidc_motion',svar,year)
	fname     = val.fname
	data.file = 'T'
	if not os.path.isfile(fname):
		print 'File does not exist:'
		data.file='F'
		return (data)

	if ( (int(year)==2015) & (int(month)>=6) ):
		data.file='F'
		return (data)
	if ( (int(year)>2015) ):
		data.file='F'
		return (data)

       	odasgrp  = Dataset(fname, 'r', format='NETCDF4')
	data.lon = np.squeeze(odasgrp.variables['lon2d'][:]) 
	data.lat = np.squeeze(odasgrp.variables['lat2d'][:]) 
	data.ui  = np.squeeze(odasgrp.variables['ui'][int(month)-1,:,:]) 
	data.vi  = np.squeeze(odasgrp.variables['vi'][int(month)-1,:,:]) 
 	missing  = odasgrp.variables['ui']._FillValue
      	odasgrp.close()
	data.ui[data.ui<=missing]=np.nan
	data.vi[data.vi<=missing]=np.nan
	data.lon[data.lon>9999]=np.nan
	data.lat[data.lat>9999]=np.nan

	data.var = np.sqrt((data.ui**2 + data.vi**2))	
	data.id  = val.long_name

        print data.id, ':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var)
	
	return (data)

