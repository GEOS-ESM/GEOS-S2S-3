#!/usr/bin/python

'''
main script

SA, Sep 2016
Guillaume Dec 2016
'''
#-----------------------------------------------------------------

import sys

import  numpy                as      np
import  matplotlib.pyplot    as      plt
import  matplotlib.cm        as      cm
import  cartopy.crs          as      ccrs
import  cartopy.feature      as      cfeature

from    netCDF4              import Dataset
from    datetime             import datetime
from    scipy import         interpolate
import  os
from    read_merra2_bcs      import read_bin_data       # read a binary file, uses f2py, see file for compile instructions... change as you please!
#-----------------------------------------------------------------

try:
    sssvar = os.environ['SSSVAR']
except:
    print('Environement variables not set, reverting to default:')
    sssvar = 'nothing'

def get_merra2_file_name(date_in):

    path_ = '/discover/nobackup/projects/gmao/share/dao_ops/fvInput/g5gcm/bcs/'
    file_pref = 'dataoceanfile_MERRA2_'
    rslv = '1440x720'               # 0/4 deg lat-lon resolution of boundary conditions

    sst_path_ = path_ + 'SST/' + rslv + '/'
    sic_path_ = path_ + 'SST/' + rslv + '/'

    # these are yearly files- notice the "%Y" in file name
    sst_file_ = sst_path_ + file_pref + 'SST' + '.' + rslv + '.' + date_in.strftime('%Y') + '.data'
    sic_file_ = sic_path_ + file_pref + 'ICE' + '.' + rslv + '.' + date_in.strftime('%Y') + '.data'

    print('Reading SST from:\n[%s]'%(sst_file_))
    print('\nReading SIC from:\n[%s]'%(sic_file_))

    return sst_file_, sic_file_

#-----------------------------------------------------------------
def get_levitussc_file_name(date_in):

    path_ = '/discover/nobackup/ehackert/levitus.WOA18.data/'
#       head_ = 'woa13_decav_s'
    head_ = 'woa18_decav_s'
#       tail_           = '_04v2.nc'
    tail_ = '_04.nc'

    # these are monthly files
    print('date in:\n[%s]'%(date_in))
    sss_file_ = path_ + head_ + date_in.strftime('%m') + tail_

    print('Reading Levitus SSS from:\n[%s]'%(sss_file_))

    return sss_file_
#-----------------------------------------------------------------
def get_aquariussc_file_name(date_in):

    path_ = '/discover/nobackup/ehackert/AQUARIUS.data/'
    head_ = 'sss.AQUARIUS.sc.v4.0.'
    tail_='.nc'

    # these are monthly files
    print('date in:\n[%s]'%(date_in))
    sss_file_ = path_ + head_ + date_in.strftime('%m') + tail_

    print('Reading Aquarius SSS from:\n[%s]'%(sss_file_))

    return sss_file_
#-----------------------------------------------------------------
def get_smapsc_file_name(date_in):

    path_ = '/discover/nobackup/ehackert/SMAP.data/'
    head_ = 'sss.SMAP.sc.v2.0.'
    tail_='.nc'

    # these are monthly files
    print('date in:\n[%s]'%(date_in))
    sss_file_ = path_ + head_ + date_in.strftime('%m') + tail_

    print('Reading SMAP SSS from:\n[%s]'%(sss_file_))

    return sss_file_
#-----------------------------------------------------------------
def get_smap_file_name(date_in):

    path_ = '/discover/nobackup/ehackert/SMAP.data/interannual/'
    head_ = 'fRSS_smap_SSS_8day_running_'
    tail_='_v02.0.nc'
    # example file name RSS_smap_SSS_8day_running_20161006_v02.0.nc

#       print 'date in:\n[%s]'%(date_in)
#       year = date_in.strftime('%Y')
#       month = date_in.strftime('%m')
#       day = date_in.strftime('%d')
#       print 'DATE IS', year, month, day
#       doy = datetime(int(year),int(month),int(day)).timetuple().tm_yday
    doy = datetime(int(date_in.strftime('%Y')),int(date_in.strftime('%m')),int(date_in.strftime('%d'))).timetuple().tm_yday
#       doystr = str(doy)
#       print 'DAYNUM IS ',doy, doystr
#       sss_file_       = path_ + head_ + date_in.strftime('%Y') + '_' + str(doy).zfill(3) + tail_
    sss_file_ = path_ + head_ + date_in.strftime('%Y') + date_in.strftime('%m').zfill(2) + date_in.strftime('%d').zfill(2) + tail_

    print('Reading SMAP SSS from:\n[%s]'%(sss_file_))

    return sss_file_
#-----------------------------------------------------------------
def get_aquarius_file_name(date_in):

    path_ = '/discover/nobackup/ehackert/AQUARIUS.data/interannual/'
    head_ = 'fsss'
    tail_='.aquarius.v4.6.1.nc'
    # example file name fsss20120729.aquarius.v4.6.1.nc

    # these are 7day files
#       print 'date in:\n[%s]'%(date_in)
#       year = date_in.strftime('%Y')
#       month = date_in.strftime('%m')
#       day = date_in.strftime('%d')
#       print 'DATE IS', year, month, day
#       doy = datetime(int(year),int(month),int(day)).timetuple().tm_yday
    doy = datetime(int(date_in.strftime('%Y')),int(date_in.strftime('%m')),int(date_in.strftime('%d'))).timetuple().tm_yday
#       doystr = str(doy)
#       print 'DAYNUM IS ',doy, doystr
#       sss_file_       = path_ + head_ + date_in.strftime('%Y') + '_' + str(doy).zfill(3) + tail_
    sss_file_ = path_ + head_ + date_in.strftime('%Y') + date_in.strftime('%m').zfill(2) + date_in.strftime('%d').zfill(2) + tail_

    print('Reading AQUARIUS SSS from:\n[%s]'%(sss_file_))

    return sss_file_
#-----------------------------------------------------------------

def apply_mask(field_):

    ls_masked_field2_ = np.copy(field_)

    #fname='/gpfsm/dnb02/projects/p43/bzhao/oceanmask_merra2_1440x720.bin'
    fname='/discover/nobackup/projects/gmao/ssd/decadal/ehackert/GEOSodas-V3/RC/OCEAN_DAS_RC_BASE_3_ALL_MONTHS/oceanmask_merra2_1440x720.bin'
    mask2=np.fromfile(fname, dtype='float32')
    mask2=np.reshape(mask2, (720, 1440))

    ls_masked_field2_[mask2==0]  = np.nan

    return ls_masked_field2_

#-----------------------------------------------------------------

#def plt2d(lon,lat,stuff, minval=-.1, maxval=.1, colmap=cm.spectral):
def plt2d(lon,lat,stuff, minval=-.1, maxval=.1, colmap=cm.jet):
    plt.figure()
    ax = plt.axes(projection=ccrs.Mollweide(central_longitude=-80))
    ax.set_global()
    ax.coastlines()
    levels = np.arange(minval, maxval+(maxval-minval)/25, (maxval-minval)/25)
    ax.add_feature(cfeature.BORDERS, lw=.5)
    ax.add_feature(cfeature.RIVERS)
    ax.add_feature(cfeature.LAND, facecolor=("coral"))
    ch = ax.contourf(lon, lat, stuff, transform=ccrs.PlateCarree(),levels=levels,extend='both',cmap=colmap)
    return ch

##-----------------------------------------------------------------

def write2file(lon,lat,sst_, mask_, fname):

    nx=sst_.shape[1]
    ny=sst_.shape[0]

    ncfile = Dataset(fname,'w')
    ncfile.createDimension('lon',nx)
    ncfile.createDimension('lat',ny)

    sst = ncfile.createVariable('sst',np.dtype('float').char,('lat','lon'))
    sst.long_name='sst'
    sst.units='C'

    mask = ncfile.createVariable('mask',np.dtype('float').char,('lat','lon'))
    mask.long_name='mask'
    mask.units='1'

    sst[:] = sst_[:]-273.15
    mask[:] = mask_[:]

    ncfile.close()

#-----------------------------------------------------------------

def write2file_sic(lon,lat,sic_, mask_, fname):

    nx=sic_.shape[1]
    ny=sic_.shape[0]

    ncfile = Dataset(fname,'w')
    ncfile.createDimension('lon',nx)
    ncfile.createDimension('lat',ny)

    #sic = ncfile.createVariable('sic',np.dtype('float').char,('lat','lon'))
    sic = ncfile.createVariable('AICE',np.dtype('float').char,('lat','lon'))
    #sic.long_name='sic'
    sic.long_name='AICE'
    sic.units='%'

    mask = ncfile.createVariable('mask',np.dtype('float').char,('lat','lon'))
    mask.long_name='mask'
    mask.units='1'

    lonout = ncfile.createVariable('lonout',np.dtype('float').char,('lon'))
    lonout.long_name='longitude'
    lonout.units='degrees'

    latout = ncfile.createVariable('latout',np.dtype('float').char,('lat'))
    latout.long_name='latitude'
    latout.units='degrees'

    sic[:] = sic_[:]
    mask[:] = mask_[:]
#       print 'NY,NX is ',ny,nx
#       print np.shape(lon)
#       print lon
#       print np.shape(lat)
#       print lat
    lon[lon<80.0]=lon[lon<80.0]+360.0
    lon[lon>180.0]=lon[lon>180.0]-360.0
    for i in range(ny):
        latout[i]=lat[i,1]
#          print 'latout is', latout[i]
    for i in range(nx):
        lonout[i]=lon[1,i]
#          print 'lonout is', lonout[i]

#       print 'past latout,lonout'
    ncfile.close()

#-----------------------------------------------------------------

def write2file_sss(lon,lat,sic_, mask_, fname):

    nx=sic_.shape[1]
    ny=sic_.shape[0]

    ncfile = Dataset(fname,'w')
    ncfile.createDimension('lon',nx)
    ncfile.createDimension('lat',ny)

    #sic = ncfile.createVariable('sic',np.dtype('float').char,('lat','lon'))
    sic = ncfile.createVariable('sss',np.dtype('float').char,('lat','lon'))
    #sic.long_name='sic'
    sic.long_name='sss'
    sic.units='PSU'

    mask = ncfile.createVariable('mask',np.dtype('float').char,('lat','lon'))
    mask.long_name='mask'
    mask.units='1'

    sic[:] = sic_[:]
    mask[:] = mask_[:]

    ncfile.close()


#-----------------------------------------------------------------

def write2file_levsss(lon,lat,sic_, mask_, fname):

    nx=sic_.shape[1]
    ny=sic_.shape[0]

    ncfile = Dataset(fname,'w')
    ncfile.createDimension('lon',nx)
    ncfile.createDimension('lat',ny)

    sic = ncfile.createVariable('sss',np.dtype('float').char,('lat','lon'))
    sic.long_name='sss'
    sic.units='PSU'

    mask = ncfile.createVariable('mask',np.dtype('float').char,('lat','lon'))
    mask.long_name='mask'
    mask.units='1'

    sic[:] = sic_[:]
    mask[:] = mask_[:]

    ncfile.close()


def nearest_interp(lon, lat, z, LON, LAT, undef=np.nan):

    lon[lon>80.0]=lon[lon>80.0]-360.0
    I=np.where(np.isfinite(z))
    points=np.array( (lon[I].flatten(), lat[I].flatten()) ).swapaxes(0, 1)
    zout=interpolate.NearestNDInterpolator(points, z[I].flatten())(LON, LAT)

    return zout

def readnc(fname, varname):
    ncfile=Dataset(fname)
    VAR=np.squeeze(ncfile.variables[varname][:])
#   print 'in readnc ',VAR
    ncfile.close()
    #VAR[np.abs(VAR)>999.9]=0.0
    return VAR

class ModelGrid():
    def __init__(self, fname='grid_spec.nc'):
        ncfile = Dataset(fname, 'r')
        self.x = ncfile.variables['x_T'][:]
        self.y = ncfile.variables['y_T'][:]
        self.wet=np.transpose(ncfile.variables['wet'][:])
        ncfile.close()

#class Getlev():
#   def __init__(self, sss_file_):
#       #Read levitus
#       ################
#       ncfile = Dataset(sss_file_, 'r')
#       self = readnc(sss_file_,'s_an')
#       self.x = ncfile.variables['lon'][:]
#       self.y = ncfile.variables['lat'][:]
#       self.nz = self.shape[0]
#       self.ny = self.shape[1]
#       self.nx = self.shape[2]
#       print   'here', self.nx,self.ny,self.nz
##      print   'here', self.y
#
#       mask = np.zeros(np.shape(self))
#       mask[np.abs(self)>0.0]=1.0
#       ncfile.close()

def get_sst_sic_sss(yy_, mm_, dd_):

        #----------------------------------------------------------------------------
        # processing date-- you would want to loop over yy_, mm_, dd_
        #yy_             = 2015          # year
        #mm_             = 10            # month    of above year
        #dd_             = 1             # day      of above month

        #yy_ = int(sys.argv[1])
        #mm_ = int(sys.argv[2])
        #dd_ = int(sys.argv[3])

    proc_date = datetime(yy_, mm_, dd_, 0,0,0)
    #----------------------------------------------------------------------------

    # get names of MERRA-2 SST & SIC file names
    [sst_file_, sic_file_] = get_merra2_file_name(proc_date)

    print(sssvar)

#   if LEVITUS
    if sssvar == 'LEVITUSSC':
        # get the levitus WOA18 salinity name
        sss_file_ = get_levitussc_file_name(proc_date)
        #  read the levitus SSS
        ncfile = Dataset(sss_file_, 'r')
        levsss = readnc(sss_file_,'s_an')
    #       levsss_xmiss  = levsss[0,0]
    #       levsss_xmiss = ncfile:_FillValue
#               print 'Levitus fill value ', s_an:_FillValue
        levsss.x = ncfile.variables['lon'][:]
        levsss.y = ncfile.variables['lat'][:]
        print(levsss.shape)
        levsss.nz = levsss.shape[0]
        levsss.ny = levsss.shape[1]
        levsss.nx = levsss.shape[2]
#               print levsss.x
#               print levsss.y
    if sssvar == 'AQUARIUSSC':
        # get the AQUARIUS salinity name
        sss_file_ = get_aquariussc_file_name(proc_date)

        #  read the levitus SSS
        ncfile = Dataset(sss_file_, 'r')
        levsss = readnc(sss_file_,'SSS')
        print(levsss.shape)
        print('here in aquariussc',levsss[90,180])
        levsss.x = ncfile.variables['lon'][:]
        levsss.y = ncfile.variables['lat'][:]
#               print levsss.x
#               print levsss.y
        levsss.ny = levsss.shape[0]
        levsss.nx = levsss.shape[1]
        levsss.nz = 9999
    if sssvar == 'AQUARIUS':
        # get the AQUARIUS salinity name
        sss_file_ = get_aquarius_file_name(proc_date)

        #  read the levitus SSS
        ncfile = Dataset(sss_file_, 'r')
        levsss = readnc(sss_file_,'SSS')
        print(levsss.shape)
        print('here in aquarius',levsss[90,180])
        levsss.x = ncfile.variables['lon'][:]
        levsss.y = ncfile.variables['lat'][:]
#               print levsss.x
#               print levsss.y
        levsss.ny = levsss.shape[0]
        levsss.nx = levsss.shape[1]
        levsss.nz = 9999
    if sssvar == 'SMAPSC':
        # get the SMAP salinity name
        sss_file_ = get_smapsc_file_name(proc_date)

        #  read the levitus SSS
        ncfile = Dataset(sss_file_, 'r')
        levsss = readnc(sss_file_,'SSS')
        print(levsss.shape)
        print('here in smapsc',levsss[90,180])
        levsss.x = ncfile.variables['lon'][:]
        levsss.y = ncfile.variables['lat'][:]
#               print levsss.x
#               print levsss.y
        levsss.ny = levsss.shape[0]
        levsss.nx = levsss.shape[1]
        levsss.nz = 9999

    if sssvar == 'SMAP':
        # get the SMAP salinity name
        print(proc_date)
        sss_file_ = get_smap_file_name(proc_date)

        #  read the SMAP interannual SSS
        ncfile = Dataset(sss_file_, 'r')
        levsss = readnc(sss_file_,'SSS')
        print(levsss.shape)
        print('here in smap',levsss[360,720])
        levsss.x = ncfile.variables['lon'][:]
        levsss.y = ncfile.variables['lat'][:]
#               print levsss.x
#               print levsss.y
        levsss.ny = levsss.shape[0]
        levsss.nx = levsss.shape[1]
        levsss.nz = 9999


    print('Salinity dims are', levsss.nx,levsss.ny,levsss.nz)
#       print   'here', levsss.y
    print(levsss.shape)

    levmask = np.zeros(np.shape(levsss))
    levmask[np.abs(levsss)>0.0]=1.0
    ncfile.close()

    levsss1 = np.zeros((levsss.ny,levsss.nx))
    if sssvar == 'AQUARIUSSC':
        levsss1[:,:] = levsss[:,:]
    if sssvar == 'AQUARIUS':
        levsss1[:,:] = levsss[:,:]
    if sssvar == 'SMAPSC':
        levsss1[:,:] = levsss[:,:]
    if sssvar == 'LEVITUSSC':
        levsss1[:,:] = levsss[0,:,:]
    if sssvar == 'SMAP':
        levsss1[:,:] = levsss[:,:]
    print(levsss1.shape)
#       print levsss1

    # read the MERRA-2 SST & SIC. Both are of dimension(720,1440); see read_merra2_bcs.pyf
    #----------------------------------------------------------------------------
    [date_ofSST, nlon, nlat, lon, lat, sst_] = read_bin_data(sst_file_, proc_date.strftime('%Y%m%d'))       # SST is in K
    [date_ofSIC, nlon, nlat, lon, lat, sic_] = read_bin_data(sic_file_, proc_date.strftime('%Y%m%d'))       # SIC is a fraction (non-dimensional)
    lon, lat =np.meshgrid(lon, lat)

    #print lon
    #print lat

    if ( date_ofSST-int(proc_date.strftime('%Y%m%d'))):
        sys.exit('date error in reading SST; Input and Output dates do not match')

    if ( date_ofSIC-int(proc_date.strftime('%Y%m%d'))):
        sys.exit('date error in reading SIC; Input and Output dates do not match')
    #----------------------------------------------------------------------------

    # apply a land-sea mask to SSS, SST & SIC
    #----------------------------------------------------------------------------


    sic_ = apply_mask(sic_)
#       print sic_.shape

#       sic_[sic_>0.15] = np.nan

    ogrid=ModelGrid(fname='grid_spec.nc')

    sic_o = nearest_interp(lon, lat, sic_, ogrid.x, ogrid.y, undef=np.nan)

    mask2 = np.ones(np.shape(sic_o))
    mask2[np.isnan(sic_o)]=0.0
    mask2=mask2*np.transpose(ogrid.wet)

    #fname='sic_'+str(yy_)+str(mm_).zfill(2)+str(dd_).zfill(2)+'_1200z.nc'
    fname='AICE_'+str(yy_)+str(mm_).zfill(2)+str(dd_).zfill(2)+'_1200z.nc'
    write2file_sic(ogrid.x,ogrid.y,sic_o, mask2, fname)

#       print sst_
    sst_ = apply_mask(sst_)

#       print sst_
#       sst_[sic_>0.15] = np.nan

    ogrid=ModelGrid(fname='grid_spec.nc')

    sst_o = nearest_interp(lon, lat, sst_, ogrid.x, ogrid.y, undef=np.nan)

#       print sic_o

    mask = np.ones(np.shape(sst_o))
    mask[np.isnan(sst_o)]=0.0
    mask=mask*np.transpose(ogrid.wet)

    fname='sst_'+str(yy_)+str(mm_).zfill(2)+str(dd_).zfill(2)+'_1200z.nc'
    write2file(ogrid.x,ogrid.y,sst_o, mask, fname)

#       levsss1 = apply_mask(levsss1)
#       levsss1[np.isnan(sst_)]  = np.nan
    maskx = (levsss1 > 60.)
    levsss1 [levsss1 > 60.] = np.nan

#       print sic_.shape

#       print levsss1

    ogrid=ModelGrid(fname='grid_spec.nc')

    levsss1_o = nearest_interp(lon, lat, levsss1, ogrid.x, ogrid.y, undef=np.nan)

    mask2 = np.ones(np.shape(levsss1_o))
    mask2[np.isnan(levsss1_o)]=0.0
    mask2=mask2*np.transpose(ogrid.wet)

    fname='SSS_'+str(yy_)+str(mm_).zfill(2)+str(dd_).zfill(2)+'_1200z.nc'
    write2file_levsss(ogrid.x,ogrid.y,levsss1_o, mask2, fname)


#   debug write the original MERRA2 data
    mask3 = np.ones(np.shape(sst_))
    mask3[np.isnan(sst_)]=0.0
    fname='rawM2sst_'+str(yy_)+str(mm_).zfill(2)+str(dd_).zfill(2)+'.nc'
    write2file(lon,lat,sst_,mask3,fname)
    fname='rawM2sic_'+str(yy_)+str(mm_).zfill(2)+str(dd_).zfill(2)+'.nc'
    write2file_sic(lon,lat,sic_,mask3,fname)

    mask4 = np.ones(np.shape(levsss1))
    mask4[np.isnan(levsss1)]=0.0
    fname='rawM2sss_'+str(yy_)+str(mm_).zfill(2)+str(dd_).zfill(2)+'.nc'
    write2file_sss(lon,lat,levsss1,mask4,fname)


    #----------------------------------------------------------------------------
