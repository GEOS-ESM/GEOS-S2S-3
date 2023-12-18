from netCDF4 import Dataset
import numpy as np
import array

###########################################################
# mom(im,jm)    		Read grid files
# indices(im,jm)		Indices to interpolate from tripolar to cartesian 1-deg grid
# xy(im,jm)			X-Y grid from polar
# shift_0(x)			Shift lons to start at 0 to 360
# shift_data_2D(data,sinc) 	Shift 2D data on lon axis
# shift_data_3D(data,sinc) 	Shift 3D data on lon axis
###########################################################

########################################################################################
def linear_interp(lon, lat, z, LON, LAT, undef=np.nan):
    points=np.array( (lon.flatten(), lat.flatten()) ).swapaxes(0, 1)
    zout=interpolate.LinearNDInterpolator(points, z.flatten())(LON, LAT)
    zout[LAT>np.max(lat)]=undef
    zout[LAT<np.min(lat)]=undef
    return zout 
###########################################################

def find_nearest(array,value):
    idx=(np.abs(array-value)).argmin()
    return array[idx], idx

###########################################################
def shift_data_4D(data,sinc):
        class gridclass:
		pass
	grid = gridclass()

	tm, km, jm, im  = np.shape(data)
	s1      = data[:,:,:,sinc:im]
	s2      = data[:,:,:,0:sinc]
	datas 	= np.squeeze([np.concatenate((s1,s2),axis=3)])

	return datas

###########################################################
def shift_data_3D(data,sinc):
        class gridclass:
		pass
	grid = gridclass()

	km, jm, im  = np.shape(data)
	s1      = data[:,:,sinc:im]
	s2      = data[:,:,0:sinc]
	datas 	= np.squeeze([np.concatenate((s1,s2),axis=2)])

	return datas

###########################################################
def shift_data_2D(data,sinc):
        class gridclass:
		pass
	grid = gridclass()

	jm, im  = np.shape(data)
	s1      = data[:,sinc:im]
	s2      = data[:,0:sinc]
	datas 	= np.squeeze([np.concatenate((s1,s2),axis=1)])

	return datas

########################################################################
def shift_data_1D(data,sinc):
        class gridclass:
		pass
	grid = gridclass()

	im      = len(data)
	s1      = data[sinc:im]
	s2      = data[0:sinc]
	datas 	= np.squeeze([np.concatenate((s1,s2),axis=0)])

	return datas
##############################################
def shift_0(x):
        class gridclass:
		pass
	grid = gridclass()
	
	#inc          = np.where(x==0)
	#sinc         = int(inc[0])
	val,arr      = find_nearest(x,0)
	inc          = np.where(x==val)
	sinc         = int(inc[0])
	im	     = len(x)	
	s1           = x[sinc:im]
	s2           = x[0:sinc]
	xs           = [np.concatenate((s1,s2),axis=1)]
	xs           = np.squeeze(xs)
 	x360         = np.where(xs<0)
	xs[x360]     = xs[x360]+360 

	return xs, sinc


###########################################################
def mom(ilon,ilat):
        class gridclass:
		pass
	grid = gridclass()

	proddir  = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/RC/GRIDS/'
	gridfile = proddir+'grid_spec_'+str(ilon)+'x'+str(ilat)+'.nc'
	ds       = Dataset(gridfile, 'r', format='NETCDF3')
	print gridfile
	try:
            	z        = ds.variables['zt'][:]
		zb       = ds.variables['zw'][:]   
		x        = ds.variables['gridlon_t'][:] 
		y        = ds.variables['gridlat_t'][:] 
            	lon      = ds.variables['geolon_t'][:]
            	lat      = ds.variables['geolat_t'][:]
            	mask     = ds.variables['wet'][:]
            	depth    = ds.variables['ht'][:]
            	area     = ds.variables['AREA_OCN'][:] 
		numlevs  = ds.variables['kmt'][:]                       
            	ds.close()
	except:
            	try:
                	z        = ds.variables['zt'][:]
 			zb       = ds.variables['zb'][:]
			x        = ds.variables['grid_x_T'][:] 
			y        = ds.variables['grid_y_T'][:] 
               		lon      = ds.variables['x_T'][:]
                	lat      = ds.variables['y_T'][:]
                	mask     = ds.variables['wet'][:]
                	depth    = ds.variables['depth_t'][:]
                	area     = ds.variables['area_T'][:]            
			numlevs  = ds.variables['num_levels'][:]	 	      
                	ds.close()
            	except:
                	lon        = ds.variables['lon'][:]
                	lat        = ds.variables['lat'][:]
                	z          = ds.variables['lev'][:]
			zb=0.0*z
			x=1.0*lon
			y=1.0*lat
                	mask=0.0*lon
                	depth=0.0*z
                	area=0.0*lon
			numlevs=0.0*lon
			[lon, lat] = np.meshgrid(lon,lat)
                	ds.close()       
	grid.z       = z
	grid.zb      = zb
	grid.x       = x       
	grid.y       = y
	grid.lon     = lon
	grid.lat     = lat
	grid.mask    = mask
	grid.area    = area    
	grid.depth   = depth    
	grid.numlevs = numlevs

	return grid

###########################################################
def xy(ilon,ilat):
        class gridclass:
		pass
	grid = gridclass()

	proddir  = '/discover/nobackup/projects/gmao/ssd/g5odas/production/'

	if ( (ilon==1440) & (ilat==721) ):
		gridfile = proddir + 'GEOS5odas-5.00/RC/GRIDS/grid_spec_1440x1080.nc'
		y        = np.arange(-90.,  90.25,    0.25)
		x        = np.arange(-180., 180, 0.25)
		ds       = Dataset(gridfile, 'r', format='NETCDF3')
		z        = ds.variables['zt'][:]
		zb       = ds.variables['zb'][:]	
		ztopo    = ds.variables['depth_t'][:]
		numlevs  = ds.variables['num_levels'][:]	 	      
		ds.close()	

	if ( (ilon==360) & (ilat==181) ):
		gridfile = proddir + 'GEOS5odas-5.00/RC/GRIDS/grid_spec_720x410.nc'
		y        = np.arange(-90.,  91,  1)
		x        = np.arange(-180., 180, 1)
		ds       = Dataset(gridfile, 'r', format='NETCDF3')
		z        = ds.variables['zt'][:]
		zb       = ds.variables['zb'][:]	
		ztopo    = ds.variables['depth_t'][:]
		numlevs  = ds.variables['num_levels'][:]	 	      
		ds.close()	

	if ( (ilon==720) & (ilat==361) ):
		gridfile = proddir + 'GEOS5odas-5.00/RC/GRIDS/grid_spec_720x361.nc'
		y        = np.arange(-90.,  90.5,  0.5)
		x        = np.arange(-180., 180, 0.5)
	
		gridfile = proddir + 'GEOS5odas-5.00/RC/GRIDS/grid_spec_720x410.nc'	
		ds       = Dataset(gridfile, 'r', format='NETCDF3')
		z        = ds.variables['zt'][:]
		zb       = ds.variables['zb'][:]	
		ztopo    = ds.variables['depth_t'][:]
		numlevs  = ds.variables['num_levels'][:]	 	      
		ds.close()	

	lon, lat = np.meshgrid(x,y)

	im	     = len(x)
	
 	# Shift lon to start at 0 
	tmp,sinc = find_nearest(x,0.)
	s1           = x[sinc:im]
	s2           = x[0:sinc]
	xs           = [np.concatenate((s1,s2),axis=1)]
	xs           = np.squeeze(xs)
 	x360         = np.where(xs<0)
	xs[x360]     = xs[x360]+360 
	lons, lats   = np.meshgrid(xs,y)

	grid.z       = z
	grid.zb      = zb
	grid.x       = x 
	grid.xs      = xs     
	grid.y       = y
	grid.lon     = lon
	grid.lons    = lons
	grid.lats    = lats
	grid.lat     = lat
	grid.ztopo   = ztopo   
	grid.numlevs = numlevs
	grid.shift0  = sinc

	# Shift lon to start at 20 for plotting
 	tmp,grid.shift20 = find_nearest(x,20.)
	s1               = x[grid.shift20:im]
	s2               = x[0:grid.shift20]
	xs               = [np.concatenate((s1,s2),axis=1)]
	xs               = np.squeeze(xs)
 	x360             = np.where(xs<0)
	xs[x360]         = xs[x360]+360 
	tmp,sinc         = find_nearest(xs,0.)
	x360             = np.where((xs>=0) & (xs<=19))
	xs[x360]         = xs[x360]+360 
	lons, lats       = np.meshgrid(xs,y)
	grid.xs20        = xs 
	grid.lons20      = lons
	grid.lats20      = lats

	return grid

###########################################################
def indices(ilon,ilat):
	# Indice to interpolate from tripolar to cartesian 1-deg grid
	proddir  = '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/RC/GRIDS/'
	gridfile = proddir + 'ANA_glb_ind_'+str(ilon)+'x'+str(ilat)+'.nc'
	ds       = Dataset(gridfile, 'r', format='NETCDF3')
	z        = ds.variables['lev'][:]
	x      = ds.variables['lon'][:]
	y      = ds.variables['lat'][:]
	lon      = ds.variables['LON'][:]
	lat      = ds.variables['LAT'][:]
	I        = ds.variables['I'][:]
	J        = ds.variables['J'][:]	  
	WT       = ds.variables['WT'][:]	        
	ds.close()	
	
        class gridclass:
		pass
	grid = gridclass()
	grid.z     = z
	grid.x     = x        
	grid.y     = y
	grid.lon   = lon
	grid.lat   = lat
	grid.I     = I
	grid.J     = J        
	grid.WT    = WT    
		
	return grid
###########################################################

