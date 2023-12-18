#! /usr/bin/env python
from netCDF4 import Dataset
import numpy as np
import scipy.stats as stats
import struct
import glob
import os
import bitarray
class dataclass:
	pass

###########################################################
def day(year,doy,sat):
	data = dataclass()

	fdir  = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/L2_SST/NESDIS/'
	#flist = sorted(list(glob.glob(fdir+sat+'/'+year+'/'+doy+'/20030101000000*nc')))
	flist = sorted(list(glob.glob(fdir+sat+'/'+year+'/'+doy+'/*nc')))
 	fnum  = len(flist)

        im = 9000
        jm = 450

        data.sst  = np.ones((fnum,im,jm))*np.nan
        data.std  = np.ones((fnum,im,jm))*np.nan
        data.bias  = np.ones((fnum,im,jm))*np.nan
        data.lon   = np.ones((fnum,im,jm))*np.nan
        data.lat   = np.ones((fnum,im,jm))*np.nan
        cnt = 0
        for fname in flist:
		tmp    = 0.0
		nc     = Dataset(fname,'r') 
		print fname
		lon        = (nc.variables['lon'][:])*1.0
		lat        = (nc.variables['lat'][:])*1.0

		sst        = np.squeeze((nc.variables['sea_surface_temperature'][:])*1.0)
		sst_miss   = nc.variables['sea_surface_temperature']._FillValue
		sst_offset = nc.variables['sea_surface_temperature'].add_offset
		sst_scale  = nc.variables['sea_surface_temperature'].scale_factor

		std        = (np.squeeze(nc.variables['sses_standard_deviation'][:]))*1.0
		std_miss   = nc.variables['sses_standard_deviation']._FillValue
		std_offset = nc.variables['sses_standard_deviation'].add_offset
		std_scale  = nc.variables['sses_standard_deviation'].scale_factor

		bias    = (np.squeeze(nc.variables['sses_bias'][:]))*1.0
		bias_miss   = nc.variables['sses_bias']._FillValue
		bias_offset = nc.variables['sses_bias'].add_offset
		bias_scale  = nc.variables['sses_bias'].scale_factor

		qual       = np.squeeze((nc.variables['quality_level'][:])*1.0)
		nc.close()

		xx,yy = np.shape(sst)
		if xx>im:
			print 'increase im to ', xx
			exit(0)
		if yy>jm:
			print 'increase jm to ', yy
			exit(0)

		sst[sst==sst_miss]=np.nan
		sst  = sst*sst_scale
		#print 'sst',np.nanmin(sst), np.nanmax(sst)
	
		std[std==std_miss]=np.nan		
		std  = std*std_scale	
		std  = std+std_offset
		#print 'std',np.nanmin(std), np.nanmax(std)

		bias = bias*bias_scale	
		bias = bias+bias_offset	
		#print 'bias',np.nanmin(bias), np.nanmax(bias)
		#raw_input('stop')

		sst[qual<5]=np.nan
		std[qual<5]=np.nan
		bias[qual<5]=np.nan

		data.sst[cnt,0:xx,0:yy]  = sst
		data.std[cnt,0:xx,0:yy]  = std
		data.bias[cnt,0:xx,0:yy] = bias

		data.lon[cnt,0:xx,0:yy]  = lon
		data.lat[cnt,0:xx,0:yy]  = lat
		cnt = cnt+1

		print 'sst',np.nanmin(data.sst), np.nanmax(data.sst)
		print 'std',np.nanmin(data.std), np.nanmax(data.std)
		print 'bias',np.nanmin(data.bias), np.nanmax(data.bias)

	return (data)

