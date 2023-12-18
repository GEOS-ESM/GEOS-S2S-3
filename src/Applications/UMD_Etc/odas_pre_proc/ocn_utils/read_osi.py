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


