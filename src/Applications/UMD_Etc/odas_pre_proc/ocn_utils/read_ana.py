from netCDF4 import Dataset
import numpy as np
import matplotlib.pyplot as plt
import glob
import os
import sys
import read_grid
sys.path.append('../ocn_monitor')
import get_ild
import amoc
sys.path.append('/gpfsm/dnb42/projects/p17/gvernier/SAND_BOXES/FDV/repo/EDA')
import obs_operator
class dataclass:
	pass

###########################################################
def mon_ild(expdir,expid,collection,year,month,svar):
	data = dataclass()

 	lvar = svar

        fname     = expdir+'/'+collection+'/'+expid+'.'+collection+'.monthly.'+year+month+'.nc4'
	data.file = 'T'
	if not os.path.isfile(fname):
		#print 'EXP File does not exist:',fname
		data.file='F'
		return (data)

        odasgrp    = Dataset(fname, 'r', format='NETCDF4')
	lon        = np.squeeze(odasgrp.variables['lon'][:]) 
	lat        = np.squeeze(odasgrp.variables['lat'][:]) 
        tmp        = np.squeeze(odasgrp.variables[lvar][:]) 
        missing    = odasgrp.variables[lvar].missing_value
	data.units = odasgrp.variables[lvar].units
        odasgrp.close()
	ogrid = obs_operator.Grid(filename='/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/RC/GRIDS/grid_spec.nc')
	mask=np.ones(np.shape(tmp))
    	mask[tmp>=999]=0.0
 	tmp = (tmp-(0.5*(tmp[0,:,:]+tmp[1,:,:])))*mask
	[km,jm,im] = np.shape(tmp)

	data.var  = get_ild.get_ild(tmp,ogrid.depth,0.5,im,jm,km)

	data.id    = expid
	data.lat   = ogrid.lat
	data.lon   = ogrid.lon
	data.depth = ogrid.depth

        print expid,':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0]

        return (data)
###########################################################
def mon_tri(expdir,expid,collection,year,month,svar):
	data = dataclass()

 	lvar = svar

        fname     = expdir+'/'+collection+'/'+expid+'.'+collection+'.monthly.'+year+month+'.nc4'
	data.file = 'T'
	if not os.path.isfile(fname):
		#print 'EXP File does not exist:',fname
		data.file='F'
		return (data)

        odasgrp    = Dataset(fname, 'r', format='NETCDF4')
	lon        = np.squeeze(odasgrp.variables['lon'][:]) 
	lat        = np.squeeze(odasgrp.variables['lat'][:]) 
        tmp        = np.squeeze(odasgrp.variables[lvar][:]) 
        missing    = odasgrp.variables[lvar].missing_value
	data.units = odasgrp.variables[lvar].units
        odasgrp.close()
        tmp[tmp==missing]=np.nan
   	if ( (lvar=='TS') & (data.units=='K') ):
		tmp = tmp-273.14
	ogrid = obs_operator.Grid(filename='/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/RC/GRIDS/grid_spec.nc')

	data.var   = tmp
	data.id    = expid
	data.lat   = ogrid.lat
	data.lon   = ogrid.lon
	data.depth = ogrid.depth

        print expid,':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var)

        return (data)
###########################################################
def mae_tri(expdir,expid,collection,year,svar):
	data = dataclass()

 	lvar  = svar
        fname = expdir+'/'+collection+'/'+expid+'.'+collection+'.monthly.'+year+'01.nc4'	
	data.file = 'T'
	if not os.path.isfile(fname):
		data.file='F'
		#print 'EXP File does not exist:',fname
		return (data)
	flist = sorted(list(glob.glob(expdir+'/'+collection+'/'+expid+'.'+collection+'.monthly.'+year+'*.nc4')))
	tm    = len(flist)
	ogrid = obs_operator.Grid(filename='/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/RC/GRIDS/grid_spec.nc')
	jm,im = np.shape(ogrid.lon)

	data.var   = (np.zeros((tm,jm,im)))*np.nan
	t = 0
       	for fname in flist:
         	odasgrp    = Dataset(fname, 'r', format='NETCDF4')
		lon        = np.squeeze(odasgrp.variables['lon'][:]) 
		lat        = np.squeeze(odasgrp.variables['lat'][:]) 
        	tmp        = np.squeeze(odasgrp.variables[lvar][:]) 
        	missing    = odasgrp.variables[lvar].missing_value
		data.units = odasgrp.variables[lvar].units
        	odasgrp.close()

        	tmp[tmp>=missing]=np.nan
   		if ( (lvar=='T') & (data.units=='K') ):
			tmp = tmp-273.14
   		if ( (lvar=='TS') & (data.units=='K') ):
			tmp = tmp-273.14
		#data.var[t,:,:] = np.squeeze(tmp[0,:,:])
		data.var[t,:,:] = tmp
		t = t+1

	data.id  = expid
	data.lat = ogrid.lat
	data.lon = ogrid.lon

        print expid,':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var)

        return (data)

###########################################################
def yz_amoc(expdir,expid,collection,year,month,svar):
	data = dataclass()

	if ( (svar=='v') | (svar=='V') ):
		lvar = 'V'

        fname     = expdir+'/'+collection+'/'+expid+'.'+collection+'.monthly.'+year+month+'.nc4'
	data.file = 'T'
	if not os.path.isfile(fname):
		data.file='F'
		#print 'EXP File does not exist:',fname
		return (data)

	odasgrp    = Dataset(fname, 'r', format='NETCDF4')
        V          = np.squeeze(odasgrp.variables[lvar][:]) 
        missing    = odasgrp.variables[lvar].missing_value
	data.units = odasgrp.variables[lvar].units
        odasgrp.close()
 
	nx = np.shape(V)[2];
	ny = np.shape(V)[1];
	nz = np.shape(V)[0];

	# Grid
	####################################
	grid_mom = read_grid.mom(nx,ny)
	z        = grid_mom.z
	lat      = grid_mom.y
	lon      = grid_mom.x
	zb       = grid_mom.zb
	x        = np.transpose(grid_mom.lon)
	depth_t  = np.transpose(grid_mom.depth)
	numlev   = np.transpose(grid_mom.numlevs)

	# Mask
	####################################
	ncfile    = Dataset('/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/GRIDS/ANA_glb_amoc_mask.nc', 'r', format='NETCDF4')
	amoc_mask = np.transpose(ncfile.variables['AMOC_MASK'][:])
	ncfile.close()

	Vf = np.zeros((nx,ny,nz))
	for level in range(0,nz):
		Vf[:,:,level]=np.transpose(V[level,:,:])
    
	AMOC = amoc.amoc(Vf,x,zb,amoc_mask,numlev,depth_t)

	AMOC[np.where(AMOC==0.0)]=np.nan

	data.lat, data.lev = np.meshgrid(lat, z)
	data.var           = np.transpose(AMOC)
	data.id            = expid

        print expid,':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lat[0,0] , data.lat[-1,-1]

        return (data)
###########################################################
def surf(expdir,expid,collection,year,month,svar):
	data = dataclass()

	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST')  | (svar=='ts') | (svar=='TS') ):
		lvar = 'TS'
	if ( (svar=='s') | (svar=='sss') | (svar=='S') | (svar=='SSS')  | (svar=='ss') | (svar=='SS') ):
		lvar = 'SS'

        fname     = expdir+'/'+collection+'/'+expid+'.'+collection+'.monthly.'+year+month+'.nc4'
	data.file = 'T'
	if not os.path.isfile(fname):
		#print 'EXP File does not exist:',fname
		data.file='F'
		return (data)

        odasgrp    = Dataset(fname, 'r', format='NETCDF4')
	lon        = np.squeeze(odasgrp.variables['lon'][:]) 
	lat        = np.squeeze(odasgrp.variables['lat'][:]) 
        tmp        = np.squeeze(odasgrp.variables[lvar][:]) 
        missing    = odasgrp.variables[lvar].missing_value
	data.units = odasgrp.variables[lvar].units
        odasgrp.close()
        tmp[tmp==missing]=np.nan
   	if ( (lvar=='TS') & (data.units=='K') ):
		tmp = tmp-273.14
	
 	# Shift to start at 0E
 	x360      = np.where(lon<0)
	lon[x360] = lon[x360]+360 
  	x,xinc    = read_grid.find_nearest(lon,0)
	lon       = read_grid.shift_data_1D(lon,xinc)
	tmp       = read_grid.shift_data_2D(tmp,xinc)
	x360      = np.where((lon>=0) & (lon<x))
	lon[x360] = lon[x360]+360 

	data.lon, data.lat = np.meshgrid(lon,lat)
	data.var = tmp
	data.id  = expid

        print expid,':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0] , data.lon[-1,-1]

        return (data)
###########################################################
def spd(expdir,expid,collection,year,month):
	data = dataclass()

	if collection == 'geosgcm_ocn2dT':
		var1 = 'US'
		var2 = 'VS'
	if collection == 'geosgcm_ocn3dT':
		var1 = 'U'
		var2 = 'V'
        fname     = expdir+'/'+collection+'/'+expid+'.'+collection+'.monthly.'+year+month+'.nc4'
	data.file = 'T'
	if not os.path.isfile(fname):
		#print 'EXP File does not exist:',fname
		data.file='F'
		return (data)

        odasgrp   = Dataset(fname, 'r', format='NETCDF4')
        tmpu      = np.squeeze(odasgrp.variables[var1][:]) 
        tmpv      = np.squeeze(odasgrp.variables[var2][:]) 
        missing   = odasgrp.variables[var1].missing_value
	data.units = odasgrp.variables[var1].units
        odasgrp.close()
        tmpu[tmpu==missing]=np.nan
        tmpv[tmpv==missing]=np.nan

        data.var = np.sqrt(tmpu**2+tmpv**2)

        jm, im = np.shape(data.var)
        grid   = read_grid.mom(im,jm)

        data.lon = grid.lon
        data.lat = grid.lat
	data.id  = expid

        print expid,':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0] 

        return (data)

###########################################################
def xz(expdir,expid,collection,year,month,ilat,var):
	data = dataclass()

        fname   = expdir+'/'+collection+'/'+expid+'.'+collection+'.monthly.'+year+month+'.nc4'
	data.file = 'T'
	if not os.path.isfile(fname):
		#print 'EXP File does not exist:',fname
		data.file='F'
		return (data)

        odasgrp  = Dataset(fname, 'r', format='NETCDF4')
	lon      = np.squeeze(odasgrp.variables['lon'][:]) 
	lat      = np.squeeze(odasgrp.variables['lat'][:]) 
	lev      = np.squeeze(odasgrp.variables['lev'][:]) 
        tmp      = np.squeeze(odasgrp.variables[var][:]) 
        missing  = odasgrp.variables[var].missing_value
	data.units = odasgrp.variables[var].units
        odasgrp.close()
        tmp[tmp==missing]=np.nan
   	if ( (var=='T') & (data.units=='K') ):
		tmp = tmp-273.14

        km, jm, im = np.shape(tmp)

 	x360      = np.where(lon<0)
	lon[x360] = lon[x360]+360 
	
	# Extract Latitude
	y,yinc = read_grid.find_nearest(lat,float(ilat))
	tmp    = np.squeeze(tmp[:,yinc,:])
	
 	# Shift to start at 20E
  	x,xinc    = read_grid.find_nearest(lon,20)
	lon       = read_grid.shift_data_1D(lon,xinc)
	tmp       = read_grid.shift_data_2D(tmp,xinc)
	x360      = np.where((lon>=0) & (lon<x))
	lon[x360] = lon[x360]+360 

	data.var = tmp
        data.lon = lon
        data.lat = lat[yinc]
	#data.lev = lev
	data.id  = expid
	tmp = read_grid.xy(im,jm)
	data.lev = tmp.z

        print expid,':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0], data.lon[-1]

        return (data)

###########################################################
def ice(expdir,expid,collection,year,month,var):
	data = dataclass()

        fname   = expdir+'/'+collection+'/'+expid+'.'+collection+'.monthly.'+year+month+'.nc4'
	data.file = 'T'
	if not os.path.isfile(fname):
		#print 'EXP File does not exist:',fname
		data.file='F'
		return (data)

        odasgrp  = Dataset(fname, 'r', format='NETCDF4')
	lon      = np.squeeze(odasgrp.variables['lon'][:]) 
	lat      = np.squeeze(odasgrp.variables['lat'][:]) 
        tmp      = np.squeeze(odasgrp.variables[var][:]) 
        missing  = odasgrp.variables[var].missing_value
	data.units = odasgrp.variables[var].units
        odasgrp.close()
        tmp[tmp>=missing]=np.nan
	if (var=='sic'):
		tmp = tmp/100
		tmp[tmp<0.15]=np.nan
	if (var=='AICE'):
		tmp[tmp<0.15]=np.nan

        jm, im   = np.shape(tmp)
	grid     = read_grid.mom(im,jm)
        data.lon = grid.lon
        data.lat = grid.lat

 	#x360      = np.where(lon<0)
	#lon[x360] = lon[x360]+360 
	
 	# Shift to start at 20E
  	#x,xinc    = read_grid.find_nearest(lon,20)
	#lon       = read_grid.shift_data_1D(lon,xinc)
	#tmp       = read_grid.shift_data_2D(tmp,xinc)
	#x360      = np.where((lon>=0) & (lon<x))
	#lon[x360] = lon[x360]+360 

	data.var = tmp
	data.id  = expid

	print np.shape(data.var), np.shape(data.lon)
        print expid,':', np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0], data.lon[-1,-1]

        return (data)

###########################################################
def surf_year(expdir,expid,collection,year,svar):
	data = dataclass()

	if ( (svar=='t') | (svar=='sst') | (svar=='T') | (svar=='SST')  | (svar=='ts') | (svar=='TS') ):
		lvar = 'TS'
	if ( (svar=='s') | (svar=='sss') | (svar=='S') | (svar=='SSS')  | (svar=='ss') | (svar=='SS') ):
		lvar = 'SS'

        flist     =  sorted(list(glob.glob(expdir+'/'+collection+'/'+expid+'.'+collection+'.monthly.'+year+'*.nc4')))

	data.file = 'T'
	if len(flist)>0:
		data.file = 'T'
	else:
		#print 'EXP File does not exist:'
		data.file='F'
		return (data)

        total = 0
	for fname in flist:
		#print fname
        	odasgrp    = Dataset(fname, 'r', format='NETCDF4')
		lon        = np.squeeze(odasgrp.variables['lon'][:]) 
		lat        = np.squeeze(odasgrp.variables['lat'][:]) 
        	tmp        = np.squeeze(odasgrp.variables[lvar][:]) 
        	missing    = odasgrp.variables[lvar].missing_value
		data.units = odasgrp.variables[lvar].units
        	odasgrp.close()
        	tmp[tmp==missing]=np.nan
  		if ( (lvar=='TS') & (data.units=='K') ):
			tmp = tmp-273.14
		total = total+tmp
	total = total/len(flist)
	#print len(flist)

 	# Shift to start at 0E
 	x360      = np.where(lon<0)
	lon[x360] = lon[x360]+360 
  	x,xinc    = read_grid.find_nearest(lon,0)
	lon       = read_grid.shift_data_1D(lon,xinc)
	tmp       = read_grid.shift_data_2D(total,xinc)
	x360      = np.where((lon>=0) & (lon<x))
	lon[x360] = lon[x360]+360 

	data.lon, data.lat = np.meshgrid(lon,lat)
	data.var = tmp
	data.id  = expid

        print expid,':', len(flist),np.nanmin(data.var), np.nanmax(data.var), np.shape(data.var), data.lon[0,0] , data.lon[-1,-1]

        return (data)

###############################################################################################
def ice_drift(expdir,expid,collection,year,month,var):
	data = dataclass()

        fname   = expdir+'/'+collection+'/'+expid+'.'+collection+'.monthly.'+year+month+'.nc4'
	data.file = 'T'
	
	if not os.path.isfile(fname):
		#print 'EXP File does not exist:',fname
		data.file='F'
		return (data)

        odasgrp  = Dataset(fname, 'r', format='NETCDF4')
	lon      = np.squeeze(odasgrp.variables['lon'][:]) 
	lat      = np.squeeze(odasgrp.variables['lat'][:]) 
	ui       = np.squeeze(odasgrp.variables['UI'][:]) 
	vi       = np.squeeze(odasgrp.variables['VI'][:]) 
        missing  = odasgrp.variables['UI'].missing_value
	data.units = odasgrp.variables['UI'].units
        odasgrp.close()        
	ui[ui>=missing]=np.nan
        vi[vi>=missing]=np.nan
	spd = np.sqrt((ui**2 + vi**2))
	spd[spd==0]=np.nan

        jm, im   = np.shape(spd)
	grid     = read_grid.mom(im,jm)
        data.lon = grid.lon
        data.lat = grid.lat

	data.var = spd
	data.ui  = ui
	data.vi  = vi
	data.id  = expid

        print expid,':', np.nanmin(data.var), np.nanmax(data.var)

        return (data)
