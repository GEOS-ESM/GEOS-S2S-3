from netCDF4 import Dataset
import numpy as np
import glob
import os
import sys
import read_grid
sys.path.append('../ocn_monitor')
import amoc
sys.path.append('/gpfsm/dnb42/projects/p17/gvernier/SAND_BOXES/FDV/repo/EDA')
import obs_operator
class dataclass:
	pass

###########################################################
def zint(expdir,expid,year,month,svar,lev):
	data = dataclass()

 	lvar = svar
	if ( (svar=='T') | (svar=='t') | (svar=='temp') | (svar=='TEMP')):
		lvar  = 'temp'
	if ( (svar=='S') | (svar=='s') | (svar=='salt') | (svar=='SALT') ):
		lvar  = 'salt'

     	fdir  = expdir+'/ocean_das/'
	flist = sorted(list(glob.glob(fdir+'oana-'+year+month+'*/mean_ana_restart/incr.nc')))

	data.file = 'T'
	if len(flist)==0:
		data.file='F'
		return (data)

	tmp0 = 0
	tmp  = 0
        for fname in flist:
        	odasgrp = Dataset(fname, 'r', format='NETCDF4') 
        	tmp0     = np.squeeze(odasgrp.variables[lvar][:]) 
        	odasgrp.close()
                tmp = tmp+tmp0
        tmp = tmp/len(flist)

	#ogrid = obs_operator.Grid(filename='/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/RC/GRIDS/grid_spec.nc')
	#print ogrid.z
	[nz,ny,nx] = np.shape(tmp)
	grid_mom = read_grid.mom(nx,ny)
	lat      = grid_mom.y
	lon      = grid_mom.x
	
	# Extract Depth
	z,zinc = read_grid.find_nearest(grid_mom.z,float(lev))
	zinc = zinc+1
	tmp    = np.squeeze(tmp[0:zinc,:,:])
	data.lev = grid_mom.z[0:zinc]

	# Integrate over Depth
	data.var = np.zeros((ny,nx))*1.0
	for j in range(ny):
		for i in range(nx):
			data.var[j,i] =(np.sum(tmp[:,j,i]))/len(data.lev)

 	# Shift to start at 0E
 	x360      = np.where(lon<0)
	lon[x360] = lon[x360]+360 
  	x,xinc    = read_grid.find_nearest(lon,0)
	lon       = read_grid.shift_data_1D(lon,xinc)
	data.var  = read_grid.shift_data_2D(data.var,xinc)
	x360      = np.where((lon>=0) & (lon<x))
	lon[x360] = lon[x360]+360 

	data.lon, data.lat = np.meshgrid(lon,lat)

	data.id  = expid

        print expid,':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0], data.lon[-1,-1]

        return (data)

###########################################################
def surf(expdir,expid,year,month,svar):
	data = dataclass()

	lvar = svar
	if ( (svar=='aice') | (svar=='AICE') | (svar=='ice') | (svar=='ICE')):
		lvar  = 'ice'
	if ( (svar=='slv') | (svar=='SLV') | (svar=='SSH') | (svar=='ssh')  | (svar=='adt') | (svar=='ADT') ):
		lvar  = 'SLV'

     	fdir  = expdir+'/ocean_das/'
	flist = sorted(list(glob.glob(fdir+'oana-'+year+month+'*/mean_ana_restart/incr.nc')))

	data.file = 'T'
	if len(flist)==0:
		data.file='F'
		return (data)

	data.var  =  0
        for fname in flist:
        	odasgrp = Dataset(fname, 'r', format='NETCDF4') 
        	tmp     = np.squeeze(odasgrp.variables[lvar][:]) 
        	odasgrp.close()
                data.var = tmp+data.var	
        data.var = data.var/len(flist)
	
	if (lvar=='ice'):
		data.var = np.squeeze(tmp[0,:,:])
	[ny,nx] = np.shape(data.var)
	grid_mom = read_grid.mom(nx,ny)
	lat      = grid_mom.y
	lon      = grid_mom.x
	
 	# Shift to start at 0E
 	x360      = np.where(lon<0)
	lon[x360] = lon[x360]+360 
  	x,xinc    = read_grid.find_nearest(lon,0)
	lon       = read_grid.shift_data_1D(lon,xinc)
	data.var  = read_grid.shift_data_2D(data.var,xinc)
	x360      = np.where((lon>=0) & (lon<x))
	lon[x360] = lon[x360]+360 

	data.lon, data.lat = np.meshgrid(lon,lat)
	data.id  = expid

        print expid,':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0] , data.lon[-1,-1]

        return (data)
###########################################################



