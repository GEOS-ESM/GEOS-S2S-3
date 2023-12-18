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
def mon(year,month,svar):
	data = dataclass()

	if ( (svar=='ice') | (svar=='hice') | (svar=='HICE') | (svar=='ICE')):
		lvar = 'heff'

	val       = get_info.validation('giomas',lvar,year)
	fname     = val.fname

	data.file = 'T'
	if not os.path.isfile(fname):
		print 'File does not exist:'
		data.file='F'
		return (data)

        odasgrp  = Dataset(fname, 'r', format='NETCDF4')
        tmp      = np.squeeze(odasgrp.variables[lvar][int(month)-1,:,:])*1.0
	data.lon = np.squeeze(odasgrp.variables['lon_scaler'][:])*1.0
	data.lat = np.squeeze(odasgrp.variables['lat_scaler'][:])*1.0	
	data.units = odasgrp.variables[lvar].units
        odasgrp.close()
	tmp[tmp==9999.9]=np.nan

	data.var = tmp
	data.id  = val.long_name

        print data.id, ':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0], data.lon[-1,-1]
	
	return (data)
###########################################################


