from netCDF4 import Dataset
import numpy as np
import os
import read_grid
import get_info
class dataclass:
	pass

###########################################################
def xz(year,month,svar):
	data = dataclass()

	if ( (svar=='u') | (svar=='U') ):
		lvar = 'U'
	if ( (svar=='v') | (svar=='V') ):
		lvar = 'V'

	val   = get_info.validation('adcp',lvar,year)
	fname = val.fname
	data.file='T'
	if not os.path.isfile(fname):
		print 'File does not exist:'
		data.file='F'
		return (data)

        odasgrp  = Dataset(fname, 'r', format='NETCDF4')
        tmp      = np.squeeze(odasgrp.variables[lvar][:])*1.0
	lon      = np.squeeze(odasgrp.variables['LON'][:])*1.0
	lat      = np.squeeze(odasgrp.variables['LAT'][:])*1.0
	depth    = np.squeeze(odasgrp.variables['DEPTH'][:])*1.0
	data.units = odasgrp.variables[lvar].units
        odasgrp.close()
	tmp   = np.squeeze(tmp[int(month)-1,:,:])

	print 'ADCP: ',np.nanmin(tmp), np.nanmax(tmp), np.shape(tmp), lon[0],lon[-1]
	[lon,lev] = np.meshgrid(lon,depth)
	data.var = tmp
	data.lon = lon
	data.lat = lat
  	data.lev = lev
	data.id  = val.long_name

	return (data)
###########################################################

