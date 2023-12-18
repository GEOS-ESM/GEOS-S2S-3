from netCDF4 import Dataset
import matplotlib.pyplot as plt
import numpy as np
import array
import matplotlib.cm as cm
from mpl_toolkits.basemap import Basemap
import glob
import struct
import time
import os
import string
from datetime import date
import scipy.stats as scstats

class gridclass:

    def __init__(self, z, lon, lat, mask, depth, area):
        self.z    = z
	self.lon  = lon
	self.lat  = lat
        self.mask = mask
        self.depth = depth
        self.area = area/1e6



def find_nearest(array,value):
    idx=(np.abs(array-value)).argmin()
    return array[idx], idx

def read_gridspec(gridfile):
	
	ds       = Dataset(gridfile, 'r', format='NETCDF3')
        try:
            z        = ds.variables['zt'][:]
            lon      = ds.variables['geolon_t'][:]
            lat      = ds.variables['geolat_t'][:]
            mask     = ds.variables['wet'][:]
            depth     = ds.variables['ht'][:]
            area     = ds.variables['AREA_OCN'][:]                        
            ds.close()
        except:
            try:
                z        = ds.variables['zt'][:]
                zb        = ds.variables['zb'][:]
                lon      = ds.variables['x_T'][:]
                lat      = ds.variables['y_T'][:]
                mask     = ds.variables['wet'][:]
                depth     = ds.variables['depth_t'][:]
                area     = ds.variables['area_T'][:]            
                ds.close()
            except:
                lon      = ds.variables['LON'][:]
                lat      = ds.variables['LAT'][:]
                z=0.0*lon
                mask=0.0*lon
                depth=0.0*lon
                area=0.0*lon
                ds.close()
        print lon.shape
                
	grid = gridclass( z, lon, lat, mask, depth, area)
	
        print grid

	return grid
    
def get_grid(fname):

	ncfile = Dataset(fname, 'r')

	try:
	   nx=len(ncfile.dimensions['nx'])
	   ny=len(ncfile.dimensions['ny'])
	   #nz=len(ncfile.dimensions['nz'])
	except:
	   try:
	      nx=len(ncfile.dimensions['lon'])
	      ny=len(ncfile.dimensions['lat'])
	      nz=len(ncfile.dimensions['lev'])
	   except:
	      try:
		 nx=len(ncfile.dimensions['xaxis_1'])
		 ny=len(ncfile.dimensions['yaxis_1'])
		 #nz=len(ncfile.dimensions['zaxis_1'])
	      except:
		 try:
		    nx=len(ncfile.dimensions['lon'])
		    ny=len(ncfile.dimensions['lat'])
		    nz=len(ncfile.dimensions['depth_below_sea'])
		 except:
                     try:
                         nx=len(ncfile.dimensions['lon'])
                         ny=len(ncfile.dimensions['lat'])
                         nz=len(ncfile.dimensions['level0'])
                     except:
                         try:
                             nx=len(ncfile.dimensions['lon'])
                             ny=len(ncfile.dimensions['lat'])
                         except:
                             try:
                                 nx=len(ncfile.dimensions['x'])
                                 ny=len(ncfile.dimensions['y'])
                             except:
                                 try:
                                     nx=len(ncfile.dimensions['grid_x_T'])
                                     ny=len(ncfile.dimensions['grid_y_T'])
                                 except:
                                     nx=len(ncfile.dimensions['x_T'])
                                     ny=len(ncfile.dimensions['y_T'])

        
        if (nx==720):
            nz=40
        else:
            nz=50                         
        if (nx==3600):
            nz=40

	try:
	   grid_fname='./data/odas-001.grid_'+str(nx)+'x'+str(ny)+'.nc'
	   ncgridfile = Dataset(grid_fname, 'r')
	   LON=ncgridfile.variables['x'][:]
	   LAT=ncgridfile.variables['y'][:]
	   Z=ncgridfile.variables['z'][:,100,100]   

	   ncgridfile.close()

	except:
	   try:
	      grid_fname='./data/grid_spec_'+str(nx)+'x'+str(ny)+'x'+str(nz)+'.nc'
	      grid = ead_gridspec(grid_fname)
	      LON=grid.lon
	      LAT=grid.lat
	      Z=grid.z
	   except:
	      try:
		 grid_fname='./data/grid_spec_'+str(nx)+'x'+str(ny)+'x'+str(nz)+'.nc'
		 grid = read_gridspec(grid_fname)
		 LON=grid.lon
		 LAT=grid.lat
		 Z=grid.z
	      except:
		 try:
		    grid_fname=fname
		    ncgrfile = Dataset(fname, 'r', format='NETCDF4')
		    #LON=ncgrfile.variables['lon'][:]
		    #LAT=ncgrfile.variables['lat'][:]

		    LON=ncgrfile.variables['lon'][:]
		    LAT=ncgrfile.variables['lat'][:]
                    LON, LAT = np.meshgrid(LON, LAT)
                    Z=ncgrfile.variables['depth'][:]
                    try:
                        Z=ncgrfile.variables['depth_below_sea'][:]
                        year=int(grid_fname[-18:-14])
                        month=int(grid_fname[-14:-12])
                        day=int(grid_fname[-12:-10])
                        print year,month,day
                    except:
                        try:
                            Z=ncgrfile.variables['level'][:]
                        except:
                            Z=0
		    ncgrfile.close()
		 except:
		    try:
		       grid_fname=fname
		       ncgrfile = Dataset(fname, 'r', format='NETCDF4')
		       LON=range(0,360)
		       LAT=range(-90,90)
		       LON, LAT = np.meshgrid(LON, LAT)
		       Z=ncgrfile.variables['level0'][:]
		       ncgrfile.close()
		    except:
                       try:
		          grid_fname=fname
			  ncgrfile = Dataset(fname, 'r', format='NETCDF4')
			  LON=ncgrfile.variables['lon'][:]
			  LAT=ncgrfile.variables['lat'][:]
			  Z=ncgrfile.variables['lev'][:]
                          print np.shape(LON)
                          LON, LAT = np.meshgrid(LON, LAT)
                          print np.shape(LON)
			  ncgrfile.close()
		       except:
                           try:
                               grid_fname=fname
                               ncgrfile = Dataset(fname, 'r', format='NETCDF4')
			  #LON=ncgrfile.variables['nav_lon'][:]
			  #LAT=ncgrfile.variables['nav_lat'][:]
                               LON=ncgrfile.variables['x'][:]
                               LAT=ncgrfile.variables['y'][:]
                               LON, LAT = np.meshgrid(LON, LAT)
                               Z=ncgrfile.variables['deptht'][:]
                               ncgrfile.close()
                           except:
                               grid_fname=fname
                               ncgrfile = Dataset(fname, 'r')#, format='NETCDF4')
                               
                               LON=ncgrfile.variables['x_T'][:]
                               LAT=ncgrfile.variables['y_T'][:]
                               Z=ncgrfile.variables['zt'][:]
                               ncgrfile.close()
	ncfile.close()



	return LON, LAT, Z


def date_from_file(fname):
    
    try:
        print '1'
        year=string.atoi(fname[-11:-7])
        month=string.atoi(fname[-7:-5])
        day=string.atoi(fname[-5:-3])
        time_model=date(int(year),int(month),int(day))
        time_model=time_model.toordinal()

        print 'date:',year,month,day

    except:
        try:
            print '2'
            year=string.atoi(fname[-14:-10])
            month=string.atoi(fname[-10:-8])
            day=string.atoi(fname[-8:-6])
            time_model=date(int(year),int(month),int(day))
            time_model=time_model.toordinal()
        except:
            try:
                print '3'

                year=string.atoi(fname[-17:-13])
                month=string.atoi(fname[-13:-11])
                day=string.atoi(fname[-11:-9])
                if (year<1940):
                    year='a'
                time_model=date(int(year),int(month),int(day))
                time_model=time_model.toordinal()
            except:
                try:
                    year=string.atoi(fname[-18:-14])
                    month=string.atoi(fname[-14:-12])
                    day=string.atoi(fname[-12:-10])
                    print year,month,day
                    time_model=date(int(year),int(month),int(day))
                    time_model=time_model.toordinal()

                except:
                    year=''
                    month=''
                    day=''
                    time_model=0
    print day,month,year

    return year, month, day, time_model

def get_var(LON, LAT, Z, FLIST, user_bias, latlonlev_sec, var, type_of_plot):

    if (type_of_plot == 'm' ):
        print 'in m'
        dumi, lon_index = find_nearest(LON[1,:],latlonlev_sec)
        print 'lon_index:',lon_index
        print latlonlev_sec
        n=0
        for fname in FLIST:
            print 'fname:',fname
            ncfile = Dataset(fname, 'r')
            if (n==0):
                try:
                    VAR=np.transpose(ncfile.variables[var][0,:,:,lon_index])
                except:
                    VAR=np.transpose(ncfile.variables[var][:,:,lon_index])
                ncfile.close()

            else:
                try:
                    VAR=VAR+np.transpose(ncfile.variables[var][0,:,:,lon_index])
                except:
                    VAR=VAR+np.transpose(ncfile.variables[var][:,:,lon_index])
                ncfile.close()
            n+=1
        print 'n:',n
        #VAR=VAR/n
        x = np.squeeze(LAT[:,lon_index])
        y = -Z

    if (type_of_plot == 'z' ):
        #print 'in z'
        dumi, lat_index = find_nearest(LAT[:,1],latlonlev_sec)
        #print 'lat_index:',lat_index
        n=0
        for fname in FLIST:
            print fname
            ncfile = Dataset(fname, 'r')
            try:
                if (n==0):
                    try:
                        #print '4D'
                        print lat_index
                        VAR=np.transpose(ncfile.variables[var][0,:,lat_index,:])
                        #print 'size of var:',np.shape(VAR)
                    except:
                        VAR=np.transpose(ncfile.variables[var][:,lat_index,:])
                    ncfile.close()

                else:
                    try:
                        VAR=VAR+np.transpose(ncfile.variables[var][0,:,lat_index,:])
                    except:
                        VAR=VAR+np.transpose(ncfile.variables[var][:,lat_index,:])
                    ncfile.close()
                n+=1
            except:
                print 'pb'
        #VAR=VAR/n
        x = np.squeeze(LON[lat_index,:])
        y = -Z

    if (type_of_plot == 'h' ):
        print 'in h'
        lev_index = int(latlonlev_sec)
        print 'lev_index:',lev_index
        n=0
        for fname in FLIST:

            print var,fname

            #ncfile = Dataset(fname, 'r')
            ncfile = Dataset(fname, 'r')
            #print 'var=',var
            #junk=ncfile.variables[var][:]
            #print 'shape=',var,junk
            #VAR=VAR+np.transpose(ncfile.variables[var][0,:,:])
            #raw_input()
            try:
                if (n==0):

                    try:
                        print '4D'
                        print lev_index
                        VAR=np.transpose(ncfile.variables[var][0,lev_index,:,:])
                        print 'size of var:',np.shape(VAR)
                    except:
                        try:
                            print '11111'
                            VAR=np.transpose(ncfile.variables[var][lev_index,:,:])
                        except:
                            try:
                                print '2222'
                                VAR=np.transpose(ncfile.variables[var][:,:])
                            except:
                                print '3333'

                                VAR=np.transpose(ncfile.variables[var][0,:,:])
                    ncfile.close()


                else:
                    try:
                        VAR=VAR+np.transpose(ncfile.variables[var][0,lev_index,:,:])
                    except:
                        try:
                            VAR=VAR+np.transpose(ncfile.variables[var][lev_index,:,:])
                        except:
                            try:
                                VAR=VAR+np.transpose(ncfile.variables[var][0,:,:])
                            except:
                                n=n-1
                                pass

                    ncfile.close()


                n+=1
            except:
                print 'pb'

        x = np.squeeze(LON[lev_index,:])
        y = np.squeeze(LAT[:,0])
        
    VAR=VAR/n
    
    if user_bias:
        print 'user bias'
        bias_fname='/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RUNS/odas-503/A004G/clim/ocn_ana_3D/A004.ocn_ana_3D_ann_1993_1999.nc'
        #bias_fname='/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RUNS/odas-503/A004G/clim/ocn_ana_2D/A004.ocn_ana_2D_ann_1993_1999.nc'
        ncfile = Dataset(bias_fname, 'r')
        #CLIM=np.transpose(ncfile.variables['T'][:,lat_index,:])-273.15
        #CLIM=np.transpose(ncfile.variables['SSH'][:])
        print var
        try:
            CLIM=np.transpose(ncfile.variables[var][:,lat_index,:])
        except:
            CLIM=np.transpose(ncfile.variables[var][lat_index,:])
        #ncfile.close()
        ncfile.close()
        print np.shape(CLIM)
        print np.shape(VAR)

        VAR=VAR-CLIM
	
    VAR=1.0*VAR
    MASK=VAR/VAR
    MASK[MASK!=1.0]=np.nan
    #VAR = np.transpose(VAR*MASK)
    VAR = np.transpose(VAR)
    #VAR[np.where(VAR==0.0)]=np.nan
    VAR[np.where(np.abs(VAR)>99999999.0)]=np.nan
    VAR[np.where(VAR==0.0)]=np.nan
    meanVAR=scstats.nanmean(scstats.nanmean(VAR))
    print 'mean VAR:',meanVAR
    print 'size VAR:',np.shape(VAR)
    if ( (var=='T') | (var=='Potential_temperature') | (var=='POT_L160_FcstAvg') ):	
        if (meanVAR>70.0):
            VAR=VAR-273.15

    if ( (var=='Salinity') ):	
        VAR=VAR*1000

    if ( (var=='vosaline') ):	
        VAR=VAR/1000.0+20.0

    VAR=np.squeeze(VAR)

    return x,y,VAR


def smooth(x,window_len=11,window='flat'):
    """smooth the data using a window with requested size.
    
    This method is based on the convolution of a scaled window with the signal.
    The signal is prepared by introducing reflected copies of the signal 
    (with the window size) in both ends so that transient parts are minimized
    in the begining and end part of the output signal.
    
    input:
        x: the input signal 
        window_len: the dimension of the smoothing window; should be an odd integer
        window: the type of window from 'flat', 'hanning', 'hamming', 'bartlett', 'blackman'
            flat window will produce a moving average smoothing.

    output:
        the smoothed signal
        
    example:

    t=linspace(-2,2,0.1)
    x=sin(t)+randn(len(t))*0.1
    y=smooth(x)
    
    see also: 
    
    numpy.hanning, numpy.hamming, numpy.bartlett, numpy.blackman, numpy.convolve
    scipy.signal.lfilter
 
    TODO: the window parameter could be the window itself if an array instead of a string
    Stolen from: http://www.scipy.org/Cookbook/LinearRegression?highlight=%28regress%29
    """
    
    #if x.ndim != 1:
    #    raise ValueError, "smooth only accepts 1 dimension arrays."

    #if x.size < window_len:
    #    raise ValueError, "Input vector needs to be bigger than window size."


    if window_len<3:
        return x

    if not window in ['flat', 'hanning', 'hamming', 'bartlett', 'blackman','std']:
        raise ValueError, "Window is one of 'flat', 'hanning', 'hamming', 'bartlett', 'blackman'"

    if window in ['flat', 'hanning', 'hamming', 'bartlett', 'blackman']:
        s=np.r_[2*x[0]-x[window_len:1:-1],x,2*x[-1]-x[-1:-window_len:-1]]
        if window == 'flat': #moving average
            w=np.ones(window_len,'d')
        else:
            w=eval('np.'+window+'(window_len)')

        tmpy=np.convolve(w/w.sum(),s,mode='same')
        y=tmpy[window_len-1:-window_len+1]

        print 

    if window in ['std']:
        y = std_filter(x,n_std=float(window_len))

    #y[-window_len+1:]=np.nan
    #y[0:window_len]=np.nan

    return y

def std_filter(x,n_std=3.):
    
    y=x
    std=scstats.nanstd(x)
    mean=scstats.nanmean(x)
    for iter in range(0,4):
        y[ np.where( (y>mean+n_std*std) | (y<mean-n_std*std) ) ] = np.nan

    return y





