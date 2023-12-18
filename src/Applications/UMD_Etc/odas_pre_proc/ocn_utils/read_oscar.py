from netCDF4 import Dataset, num2date
import numpy as np
import time
import os
import read_grid
class dataclass:
	pass

###########################################################
def pentad_hires_spd(year,month):	
	data = dataclass()

	imon  = (int(month))-1
     	fname = '/gpfsm/dnb42/gmaofcst/ocean/obs/odas-2/OSCAR/RAW/oscar-third-pentad-'+year+'.nc'
	data.file='T'
	if not os.path.isfile(fname):
		print 'File does not exist:'
		data.file='F'
		return (data)

        odasgrp    = Dataset(fname, 'r', format='NETCDF4')
        tmpu       = np.squeeze(odasgrp.variables['um'][:,:,:])*1.0
        tmpv       = np.squeeze(odasgrp.variables['vm'][:,:,:])*1.0
 	lon        = np.squeeze(odasgrp.variables['longitude'][:])*1.0
	lat        = np.squeeze(odasgrp.variables['latitude'][:])*1.0
	time       = np.squeeze(odasgrp.variables['time'][:])*1.0
	time_units = odasgrp.variables['time'].units
        odasgrp.close()

	# Grid starts at 20 and goes to 420, we need to cut off grid past 379.66
	x,xinc    = read_grid.find_nearest(lon,379.75)
	lon       = lon[0:xinc]	
	tmpu      = tmpu[:,:,0:xinc]	
	tmpv      = tmpv[:,:,0:xinc]
	
 	x360      = np.where(lon>360)
	lon[x360] = lon[x360]-360 

	# Shift grid to start at zero longitude
	x,xinc    = read_grid.find_nearest(lon,0)
	lon       = read_grid.shift_data_1D(lon,xinc)

	Gtime      = num2date(time,units=time_units,calendar='standard')
	[tm,jm,im] = np.shape(tmpu)
	cnt = 0
	u   = np.zeros([jm,im])
	v   = np.zeros([jm,im])
	for t in range(tm):
		imonth = int(Gtime[t].strftime("%m"))
		if ( (imonth==int(month)) ):
			u = tmpu[t,:,:]+u
			v = tmpv[t,:,:]+v
			cnt = cnt+1
		
	u = u/cnt
	v = v/cnt	

	tmp = np.sqrt(u**2+v**2)
	
	# Shift grid to start at zero longitude
	data.var = read_grid.shift_data_2D(tmp,xinc)
	data.lon, data.lat = np.meshgrid(lon,lat)
	data.id  = 'OSCAR'

 	print 'OSCAR: ',np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0]
 	
	return (data)

###########################################################


