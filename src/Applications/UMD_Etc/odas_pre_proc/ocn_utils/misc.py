#! /usr/bin/env python
#

from netCDF4 import Dataset, num2date, date2num
from netCDF4 import MFDataset
import matplotlib
import matplotlib.cm as cm
import numpy as np
import array
import datetime
from datetime import date
from scipy import interpolate

########################################################################################
def make_N_colors(cmap_name, N):
     cmap = cm.get_cmap(cmap_name, N)
     return cmap(np.arange(N))[:,:-1] 
##################################################################
def mmdd2doy(year,month,day):
        doy  = (datetime.datetime(int(year),int(month),int(day))-datetime.datetime(int(year), 1, 1)).days + 1
        return doy
##################################################################
def isNaN(num):
    return num != num
##################################################################
def numberOfNonNans(data):
    count = 0
    for i in data:
        if not np.isnan(i):
            count += 1
    return count 
###########################################################
def find_nearest(array,value):
    idx=(np.abs(array-value)).argmin()
    return array[idx], idx

#######################################################################
def global_int_ave(field):
	ncfile=Dataset('/gpfsm/dnb42/projects/p17/gvernier/SAND_BOXES/PLOT_ODAS/DATA/grid_spec_720x410x40.nc', mode="r", clobber=True, format='NETCDF4')
	A        = ncfile.variables['area_T'][:]
	mask     = ncfile.variables['wet'][:]
	lat      = ncfile.variables['y_T'][:]
	ncfile.close()

	field = np.squeeze(field)
	field[np.where(abs(lat)>90.0)]=np.nan

	Iland        = np.where(mask==0.0)
	Ih2o         = np.where(mask==1.0)
	field[Iland] = np.nan
	A[Iland]     = np.nan
	bigA         = np.nansum(A)
	meanfield    = np.nansum(A*field)/bigA 

	return meanfield
#######################################################################
def global_int(field):
	ncfile=Dataset('/gpfsm/dnb42/projects/p17/gvernier/SAND_BOXES/PLOT_ODAS/DATA/grid_spec_720x410x40.nc', mode="r", clobber=True, format='NETCDF4')
	A        = ncfile.variables['area_T'][:]
	mask     = ncfile.variables['wet'][:]
	lat      = ncfile.variables['y_T'][:]
	ncfile.close()

	field = np.squeeze(field)
	field[np.where(abs(lat)>90.0)]=np.nan

	Iland        = np.where(mask==0.0)
	Ih2o         = np.where(mask==1.0)
	field[Iland] = np.nan
	A[Iland]     = np.nan
	bigA         = np.nansum(A)
	meanfield    = np.nansum(A*field)

	return meanfield
########################################################################################
def nearest_interp(lon, lat, z, LON, LAT, undef=np.nan):

    maxLON=np.max(LON.flatten())
    lon[lon>maxLON]=lon[lon>maxLON]-360.0
    points=np.array( (lon.flatten(), lat.flatten()) ).swapaxes(0, 1)

    zout=interpolate.NearestNDInterpolator(points, z.flatten())(LON, LAT)
    zout[LAT>np.max(lat)]=undef
    zout[LAT<np.min(lat)]=undef

    return zout
########################################################################################
def simple_filter(X,N):

     Xt=X

     Ns=np.shape(X)[0]

     XF=Xt
     for iter in range(0,N):
         for i in range(1,Ns-1):
             XF[i]=0.5*(XF[i-1]+XF[i+1])
         XF[Ns-1]=0.5*(XF[Ns-1]+XF[Ns-2])
         XF[0]=0.5*(XF[0]+XF[1])

     return XF
###########################################################


