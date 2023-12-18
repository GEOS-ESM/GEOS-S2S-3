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
def mon_adt(year,month,svar):
	data = dataclass()

	if ( (svar=='adt') | (svar=='ADT') ):
		lvar = 'adt'
	
	val       = get_info.validation('aviso',lvar,year)
	fname     = val.fname
	data.file = 'T'
	if not os.path.isfile(fname):
		print 'File does not exist:'
		data.file='F'
		return (data)

	flist = glob.glob(val.valdir+'*h_'+year+month+'*.nc')
	data.var  =  0
        for fname in flist:
        	odasgrp = Dataset(fname, 'r', format='NETCDF4')
        	tmp     = np.squeeze(odasgrp.variables[lvar][:])*1.0
		lon = np.squeeze(odasgrp.variables['lon'][:])*1.0
		lat = np.squeeze(odasgrp.variables['lat'][:])*1.0	
        	scale   = odasgrp.variables[lvar].scale_factor
		missing = odasgrp.variables[lvar]._FillValue
		data.units = odasgrp.variables[lvar].units
        	odasgrp.close()
		tmp[tmp==missing]=np.nan
		tmp = tmp*scale
                data.var = tmp+data.var	
        data.var = data.var/len(flist)
	data.id  = val.long_name
	data.lon,data.lat = np.meshgrid(lon,lat)

        print data.id, ':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0], data.lon[-1,-1]

	return (data)
###########################################################



