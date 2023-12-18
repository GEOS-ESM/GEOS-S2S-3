#! /usr/bin/env python
import numpy as np
import matplotlib.pyplot as plt
from numpy import *
import glob
class dataclass:
	pass

###########################################################
def variable(svar):
	data = dataclass()

	if svar=='ILD':
		data.long_name     = 'Isothermal Layer Depth'
		data.full_levels   = np.arange(0, 100, 5)
		data.full_contours = np.arange(0, 100, 5)
		data.anom_levels   = np.arange(-1, 1.1, 0.1)
		data.anom_contours = np.arange(-2, 2.2, 0.2)
		data.units         = 'm'

	if svar=='AMOC':
		data.long_name     = 'Atlantic Meridional Overturning Circulation'
		data.full_levels   = np.arange(-20,17,2)
		data.full_contours = np.arange(-20,40,2)
		data.anom_levels   = np.arange(-1, 1.1, 0.1)
		data.anom_contours = np.arange(-2, 2.2, 0.2)
		data.units         = 'Sv'

	if svar=='T':
		data.long_name     = 'Temperature'
		data.full_levels   = np.arange(0,32,1)
		data.full_contours = np.arange(-10, 41, 1)
		data.anom_levels   = np.arange(-1, 1.1, 0.1)
		data.anom_contours = np.arange(-2, 2.2, 0.2)
		data.units         = '$^oC$'

	if svar=='S':
		data.long_name     = 'Salinity'
		data.full_levels   = np.arange(30,36.2,.2)
		data.full_contours = np.arange(30,40.2,.2)
		#data.anom_levels   = np.arange(-0.5, 0.55, 0.05)
		data.anom_levels   = np.arange(-0.8, 0.81, 0.1)	
		data.anom_contours = np.arange(-0.8, 0.9, 0.1)
		data.units         = 'psu'

	if svar=='U':
		data.long_name     = 'Zonal Current'
		data.full_levels   = np.arange(-1, 1.05, 0.05)
		data.full_contours = np.arange(-2, 2.1,  0.1)
		data.anom_levels   = np.arange(-.1,0.1,.01) 
		data.anom_contours = np.arange(-.1,0.1,.01)
		data.units         = 'm/s'

	if svar=='V':
		data.long_name     = 'Meridional Current'
		data.full_levels   = np.arange(-0.5,0.55,0.05)
		data.full_contours = np.arange(-1,1.05,0.05)
		data.anom_levels   = np.arange(-.1,0.1,.01)
		data.anom_contours = np.arange(-.1,0.1,.01)
		data.units         = 'm/s'

	if svar=='AICE':
		data.long_name     = 'Ice Concentration'
		data.full_levels   = np.arange(0.15,1.05,0.05)
		data.full_contours = np.arange(0.15,1.05,0.05)
		data.anom_levels   = np.arange(-1,1,.1)
		data.anom_contours = np.arange(-1,1,.1)
		data.units         = '%'

	if svar=='HICE':
		data.long_name     = 'Ice Thickness'
		data.full_levels   = np.arange(0,3.6,0.1)
		data.full_contours = np.arange(0,3.6,0.1)
		data.anom_levels   = np.arange(-1,1,.1)
		data.anom_contours = np.arange(-1,1,.1)
		data.units         = 'm'

	if svar=='ICE_DRIFT':
		data.long_name     = 'Ice Drift'
		data.full_levels   = np.arange(0, 0.21, 0.01)
		data.full_contours = np.arange(0, 0.21, 0.01)
		data.anom_levels   = np.arange(0, 0.21, 0.01)
		data.anom_contours = np.arange(0, 0.21, 0.01)
		data.units         = 'm/s'

	if svar=='SSH':
		data.long_name     = 'Sea-Surface Height'
		data.full_levels   = np.arange(-.1,0.01,.1)
		data.full_contours = np.arange(-.1,0.01,.1)
		data.anom_levels   = np.arange(-.1,0.01,.1)
		data.anom_contours = np.arange(-.1,0.01,.1)
		data.units         = 'm'

	if svar=='SLV':
		data.long_name     = 'ADT'
		data.full_levels   = np.arange(-2,1.6,0.1)
		data.full_contours = np.arange(-2,1.6,0.1)
		data.anom_levels   = np.arange(-2,1.6,0.1)
		data.anom_contours = np.arange(-2,1.6,0.1)
		data.units         = 'm'


	return data


###########################################################
def validation(valid,svar,year):
	data = dataclass()
	dir1 = '/gpfsm/dnb04/projects/p71/aogcm/g5odas'
	dir2 = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/RC/VAL'

 	if valid=='en4':
		data.long_name = 'EN4' 
		data.valdir    = dir2+'/360x181/EN4'
		data.pltdir    = dir1+'/val/'+valid+'/plots'
		data.fname     = data.valdir+'/TS_EN4_'+year+'.nc'
		if (int(year) < 1980) | (year=='clim'):
			data.fname     = data.valdir+'/clim/EN4_clim_1993_2010.nc'
			data.long_name = 'EN4 Clim (1993-2010)'	


	if valid=='argo':
		data.long_name = 'ARGO GRD' 
	

	if valid=='aviso':
		data.long_name = 'AVISO' 
		data.valdir    = dir1+'/obs/raw/AVISO/RAW/GRD_ADT_DT/'+year+'/'
		if (int(year) >= 2016):
			data.valdir    = dir1+'raw/AVISO/RAW/GRD_ADT_NRT/'+year+'/'
		data.flist = glob.glob(data.valdir+'dt_global_allsat_madt_h_'+year+'*.nc')
		data.fname = data.flist[0]

	if valid=='adcp':
		data.long_name = 'ADCP' 
		data.valdir    = dir2+'/native/ADCP'
		data.fname     = data.valdir+'/ADCP_'+year+'_eqpac.nc'
		if (int(year) < 1997) | (int(year) > 2007) | (year=='clim'):
			data.fname = data.valdir+'/ADCP_clim_eqpac.nc'
			data.long_name = 'ADCP Clim (1998-2006)'

	if valid=='glorys':
		data.long_name = 'GLORYS' 
		data.valdir    = dir2+'/360x181/GLORYS'
		data.fname     = data.valdir+'/'+svar+'_GLORYS_'+year+'.nc'
		if (int(year) < 1993) | (int(year) > 2009) | (year=='clim'):
			data.fname = data.valdir+'/clim/GLORYS_'+svar+'_1993_2009.nc'
			data.long_name = 'GLORYS Clim (1993-2009)'	

	if valid=='oras4':
		data.long_name = 'ORAS4' 
		data.valdir    = dir2+'/360x181/ECMWF/ORAS4/'
		data.fname     = data.valdir+'/oras4_'+year+'.nc'
		if (int(year) < 1980) | (year=='clim'):
			data.fname = data.valdir+'/clim/oras4_clim_1993_2010.nc'
			data.long_name = 'ORAS4 Clim (1993-2010)'	

	if valid=='a3d':
		data.long_name = 'ARMOR3D' 
		data.valdir    = dir2+'/360x181/A3D'
		data.fname     = data.valdir+'/TS_A3D_'+year+'.nc'
		if (int(year) < 1993) | (year=='clim'):
			data.fname = data.valdir+'/clim/A3D_clim_1993_2010.nc'
			data.long_name = 'AMROR3D Clim (1993-2010)'	

	if valid=='godas':
		data.long_name = 'GODAS' 
		data.valdir    = dir2+'/360x181/GODAS'
		data.fname     = data.valdir+'/'+svar+'_GODAS_'+year+'.nc'
		if (int(year) < 1980) | (year=='clim'):
			data.fname = data.valdir+'/clim/GODAS_'+svar+'_clim_1993_2010.nc'
			data.long_name = 'GODAS Clim (1993-2010)'

	if valid=='soda':
		data.long_name = 'SODA3.3.1' 
		#data.valdir    = dir2+'/native/SODA/MONTHLY'
		#data.fname     = data.valdir+'/soda3.3.1_mn_ocean_reg_'+year+'.nc'
		data.valdir    = dir2+'/360x181/SODA'
		data.fname     = data.valdir+'/soda_'+year+'.nc'

	if valid=='soda-native':
		data.long_name = 'SODA3.3.1' 
		data.valdir    = dir2+'/native/SODA/MONTHLY'
		data.fname     = data.valdir+'/soda3.3.1_mn_ocean_reg_'+year+'.nc'

	if valid=='soda-ice':
		data.long_name = 'SODA3.3.1' 
		data.valdir    = dir2+'/native/SODA/MONTHLY'
		data.fname     = data.valdir+'/soda3.3.1_mn_ice_reg_'+year+'.nc'

	if valid=='nsidc':
		data.long_name = 'NSIDC' 
		data.valdir    = dir1+'/obs/raw/NSIDC/FINAL/GRD'
		data.fname     = data.valdir+'/ICE_NSIDC_'+year+'.nc'
		if (int(year) < 1979) | (year=='clim'):
			data.fname = data.valdir+'/ICE_NSIDC_clim_1993_2006.nc'
			data.long_name = 'NSIDC Clim (1993-2006)'

	if valid=='reyn':
		data.long_name = 'Reynolds' 
		data.valdir    = dir2+'/360x181/REYN'
		data.fname     = data.valdir+'/REYN_'+year+'.nc'
		if (int(year) < 1982) | (year=='clim'):
			data.fname = data.valdir+'/clim/REYN_clim_1993_2010.nc'
			data.long_name = 'Reynolds Clim (1993-2010)'

	if valid=='ostia':
		data.long_name = 'OSTIA' 
		data.valdir    = dir2+'/360x181/OSTIA'
		data.fname     = data.valdir+'/OSTIA_'+year+'.nc'
		if (int(year) < 2006) | (year=='clim'):
			data.fname = data.valdir+'/clim/OSTIA_clim_2007_2014.nc'
			data.long_name = 'OSTIA Clim (2007-2014)'

	if valid=='cmip5':
		data.long_name = 'CMIP5' 
		data.valdir    = dir2+'/360x181/CMIP5'
		data.fname     = data.valdir+'/CMIP5_'+year+'.nc'
		if (int(year) < 1950) | (year=='clim'):
			data.fname = data.valdir+'/clim/CMIP5_clim_1949_1982.nc'
			data.long_name = 'CMIP5 Clim (1949-1982)'
		if (int(year) > 2009) | (year=='clim'):
			data.fname = data.valdir+'/clim/CMIP5_clim_1949_1982.nc'
			data.long_name = 'CMIP5 Clim (1949-1982)'

	if valid=='osi':
		data.long_name = 'OSI-SAF' 
		data.valdir    = dir1+'/obs/raw/ICE_DRIFT/MET/LR/'+year+'/'
		data.flist     = glob.glob(data.valdir+'ice_drift_nh_polstere-625_multi-oi_'+year+'*.nc')
		data.fname     = data.flist[0]

	if valid=='nsidc_motion':
		# 1979-2015
		data.long_name = 'NSIDC' 
		data.valdir    = dir1+'/obs/raw/ICE_DRIFT/NSIDC/data/'
		if (svar=='npole'):
			data.fname     = data.valdir+'icemotion.nh.'+year+'.nc'
		if (svar=='spole'):
			data.fname     = data.valdir+'icemotion.sh.'+year+'.nc'

	if valid=='giomas':
		data.long_name = 'GIOMAS' 
		data.valdir    = dir1+'/obs/raw/GIOMAS/RAW/heff/'
		data.fname     = data.valdir+'/heff.H'+year+'.nc'
		if (int(year) < 1979) | (year=='clim'):
			data.long_name = 'PIOMAS' 
			data.id        = 'piomas'
			data.valdir    = dir2+'/360x181/PIOMASS'
			data.fname     = data.valdir+'/HICE_PIOMAS_clim.nc'
			data.long_name = 'PIOMAS Clim (1979-2009)'
		if (int(year) > 2013) | (year=='clim'):
			data.id        = 'piomas'
			data.long_name = 'PIOMAS' 
			data.valdir    = dir2+'/360x181/PIOMASS'
			data.fname     = data.valdir+'/HICE_PIOMAS_clim.nc'
			data.long_name = 'PIOMAS Clim (1979-2009)'

	if valid=='piomas':
		data.long_name = 'PIOMAS' 
		data.valdir    = dir2+'/360x181/PIOMASS'
		data.fname     = data.valdir+'/HICE_PIOMAS_'+year+'.nc'
		if (int(year) < 1979) | (year=='clim'):
			data.fname = data.valdir+'/HICE_PIOMAS_clim.nc'
			data.long_name = 'PIOMAS Clim (1979-2009)'
		if (int(year) > 2009) | (year=='clim'):
			data.fname = data.valdir+'/HICE_PIOMAS_clim.nc'
			data.long_name = 'PIOMAS Clim (1979-2009)'

	if valid=='woa':
		data.long_name = 'WOA' 
		data.valdir    = dir2+'/360x181/LEV'
		data.fname     = data.valdir+'/levitus_grd.nc'
		data.long_name = 'WOA Clim'


	if valid=='woa13':
		data.long_name = 'WOA13' 
		data.valdir    = dir2+'/360x181/WOA'
		data.fname     = data.valdir+'/woa13_grd.nc'
		data.long_name = 'WOA13 Clim'

	return data


