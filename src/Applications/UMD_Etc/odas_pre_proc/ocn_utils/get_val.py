#! /usr/bin/env python
import numpy as np
import matplotlib.pyplot as plt
from numpy import *
class dataclass:
	pass

###########################################################
def info(valid,var,year,month,useclim):
	data = dataclass()

	odas           ='GEOS5odas-5.00'
	data.long_name = 'Validation'
	data.valname   =  data.long_name
	data.valdir    = '/discover/nobackup/projects/gmao/ssd/g5odas/production/'+odas+'/RC/VAL/'

 	if valid=='adcp':
		data.long_name = 'ADCP'
		data.valname   = data.long_name 
		data.valdir    = data.valdir+'native/'+data.valname
		data.fname     = data.valdir+'/'+data.valname+'_'+year+'_eqpac.nc'
		if (int(year) < 1997) | (int(year) > 2007) | (useclim=='T'):
			data.fname = data.valdir+'/'+data.valname+'_clim_eqpac.nc'
			data.long_name = 'ADCP Clim (1998-2006)'

	if valid=='oscar':
		data.long_name = 'OSCAR'
		data.valname   = data.long_name 
		data.valdir    = data.valdir+'360x181/'+data.valname+'/'
		data.fname     = data.valdir+'/UV_'+data.valname+'_'+year+'.nc'
		if (int(year) < 1993) | (int(year) > 2011) | (useclim=='T'):
			data.fname = data.valdir+'/UV_'+vdata.valname+'_clim_1993_2006.nc'
			data.long_name = 'OSCAR Clim'

	if valid=='glorys':
		data.long_name = 'GLORYS'
		data.valname   = data.long_name 
		data.valdir    = data.valdir+'360x181/'+data.valname+'/'
		data.fname     = data.valdir+'/'+var+'_GLORYS_'+year+'.nc'
		if (int(year) < 1993) | (int(year) > 2009) | (useclim=='T'):
			data.fname = data.valdir+'/clim/'+data.valname+'_'+var+'_clim_2004_2009.nc'

	if valid=='en4':
		data.long_name = 'EN4'
		data.valname   =  data.long_name 
		data.valdir    = data.valdir+'360x181/'+data.valname+'/'
		data.fname     = data.valdir+'/TS_EN4_'+year+'.nc'

		if (int(year) < 1980) | (useclim=='T'):
			data.fname     = data.valdir+'/clim/EN4_clim_2004_2011.nc'
			data.long_name = 'En4 Clim'



	if valid=='ostia':
		data.long_name = 'OSTIA'
		data.valname   =  data.long_name
		data.valdir    = data.valdir+'360x181/'+data.valname+'/'
		data.fname     = data.valdir+'/OSTIA_'+year+'.nc'

	if valid=='arg':
		data.long_name = 'ArgoOA'
		data.valname   =  'ARGO_GRD'
		data.valdir    = data.valdir+'360x181/'+data.valname+'/'
		data.fname     = data.valdir+'/TS_ARGO_GRD_'+year+'.nc'

 	if valid=='a3d':
		data.long_name = 'Armor3D'
		data.valname   =  'A3D'
		data.valdir    = data.valdir+'360x181/'+data.valname+'/'
		data.fname     = data.valdir+'/TS_A3D_'+year+'.nc'

 	if valid=='lev':
		data.long_name = 'WOD09'
		data.valname   =  'LEV'
		data.valdir    = data.valdir+'360x181/'+data.valname+'/'
		data.fname     = data.valdir+'levitus_grd.nc'

	if valid=='en3':
		data.long_name = 'EN3'
		data.valname   =  data.long_name 
		data.valdir    = data.valdir+'360x181/'+data.valname+'/'
		data.fname     = data.valdir+'TS_'+data.valname+'_'+year+'.nc'
   
		if (int(year) < 1950) | (useclim=='T'):
			data.fname  = data.valdir+'/clim/'+data.valname+'_clim_2004_2011.nc'

	if valid=='godas':
		data.long_name = 'GODAS'
		data.valname   = data.long_name 
		data.valdir    = data.valdir+'360x181/'+data.valname+'/'
		data.fname     = data.valdir+'/'+var+'_GODAS_'+year+'.nc'
		if (int(year) < 1980) | (useclim=='T'):
			data.fname = data.valdir+'/clim/'+data.valname+'_'+var+'_clim_2004_2011.nc'

	if valid=='cfsr':
		data.long_name = 'CFSR'
		data.valname   = data.long_name 
		data.valdir    = data.valdir+'360x181/'+data.valname+'/'
		data.fname     = data.valdir+'/'+var+'_CFSR_'+year+'.nc'
		if (int(year) < 1979) |  (int(year) > 2009) | (useclim=='T'):
			data.fname = data.valdir+'/clim/'+data.valname+'_'+var+'_clim_2004_2009.nc'

	if valid=='reyn':
		data.long_name = 'Reynolds'
		data.valname   = data.long_name 
		data.valdir    = data.valdir+'360x181/'+data.valname+'/'
		data.fname     = data.valdir+'/'+data.valname+'_'+year+'.nc'
		if int(year) < 1982 | (useclim=='T'):
			data.fname = data.valdir+'/'+data.valname+'_clim_1993_2006.nc'
			data.long_name = 'Reynolds Clim'

	if valid=='cmip5':
		data.long_name = 'CMIP5'
		data.valname   = data.long_name 
		data.valdir    = data.valdir+'360x181/'+data.valname+'/'
		data.fname     = data.valdir+'/'+data.valname+'_'+year+'.nc'
		if (int(year) < 1950) | (int(year) > 1982) | (useclim=='T'):
			data.fname = data.valdir+'/'+data.valname+'_clim_1993_2006.nc'
			data.long_name = 'CMIP5 Clim'

	if valid=='aviso':
		data.long_name = 'AVISO'
		data.valname   = data.long_name 
		data.valdir    = data.valdir+'360x181/'+data.valname+'/'
		data.fname     = data.valdir+'/SLA_'+data.valname+'_'+year+'.nc'
		if (int(year) < 1993) | (useclim=='T'):
			data.fname = data.valdir+'/SLA_'+data.valname+'_clim_1993_2009.nc'
			data.long_name = 'AVISO Clim'


	return data




