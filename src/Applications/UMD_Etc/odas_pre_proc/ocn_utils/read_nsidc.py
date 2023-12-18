#! /usr/bin/env python
from netCDF4 import Dataset
import numpy as np
import glob
import os
import read_grid
import get_info
class dataclass:
	pass

###########################################################
def mon(year,month,pole):
	data = dataclass()

	if ( (pole=='N') | (pole=='n') | (pole=='north') | (pole=='npole') ):
		svar = 'naicem'
		slon = 'nlon'
		slat = 'nlat'

	if ( (pole=='S') | (pole=='s') | (pole=='south') | (pole=='spole') ):
		svar = 'saicm'
		slon = 'slon'
		slat = 'slat'
		#if int(year) >= 2009:
		#	svar = 'saicem'

	val       = get_info.validation('nsidc',svar,year)
	fname     = val.fname
	data.file = 'T'
	if not os.path.isfile(fname):
		print 'File does not exist:'
		data.file='F'
		return (data)

        odasgrp = Dataset(fname, 'r', format='NETCDF4')
        tmp     = np.squeeze(odasgrp.variables[svar][int(month)-1,:,:])
	lon     = np.squeeze(odasgrp.variables[slon][:])
	lat     = np.squeeze(odasgrp.variables[slat][:])
        odasgrp.close()
	tmp[tmp<0.15]=np.nan

	data.var = tmp
	data.lon = lon
	data.lat = lat
	data.id  = val.long_name

 	print data.id,': ',np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), lon[0,0]
 
	return (data)


