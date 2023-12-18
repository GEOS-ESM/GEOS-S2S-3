from netCDF4 import Dataset
import numpy as np
import time
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from mpl_toolkits.basemap import Basemap

########################################################################
def param(inst,var):
	param.MISSING = '9.99e11'

 	if (inst=='NSIDC') & (var=='AICE'):
		param.VAR         = 'aice'
		param.INST        = 'NSIDC'
		param.VAR_ID      = 106
		param.INST_ID     = 518
		param.INST_ERROR  = 0.05
		param.UNITS       = '%'
		param.LONG_NAME   = 'Sea-Ice Concentration'
		param.SOURCE      = 'ftp://sidads.colorado.edu/pub/DATASETS/seaice/polar-stereo/nasateam'
		param.TITLE       = 'NSIDC Daily Sea-Ice Concentrations'

	return param


########################################################################
def write_grd_native(fname,param,doy,date,Nlon,Nlat,Slon,Slat,Nice,Sice):

     tm,njm,nim = np.shape(Nice)
     tm,sjm,sim = np.shape(Sice)

     ncfile = Dataset(fname,'w',format='NETCDF3_CLASSIC')

     ncfile.createDimension('Nlon',nim)	
     ncfile.createDimension('Nlat',njm)
     ncfile.createDimension('Slon',sim)	
     ncfile.createDimension('Slat',sjm)

     ncfile.createDimension('time',tm)

     ncfile.title       = param.TITLE 
     ncfile.source      = param.SOURCE
     ncfile.history     = 'Updated on '+(time.strftime("%Y%m%d"))
     ncfile.Conventions = 'COARDS'
     ncfile.Version     = 'netcdf-4.0'

     t    = ncfile.createVariable('date',np.dtype('i4').char,('time'))
     t[:] = date    
     t.long_name = 'YYYYMMDD'

     t    = ncfile.createVariable('time',np.dtype('i4').char,('time'))
     t[:] = doy    
     t.long_name = 'Day of Year'

     t    = ncfile.createVariable('Nlon',np.dtype('float32').char,('Nlat','Nlon'))
     t[:] = Nlon  
     t.long_name = 'North Pole Longitude'
     t.units     = 'degrees-east'

     t    = ncfile.createVariable('Nlat',np.dtype('float32').char,('Nlat','Nlon'))
     t[:] = Nlat  
     t.long_name = 'North Pole Latitude'
     t.units     = 'degrees-north'

     t    = ncfile.createVariable('Slon',np.dtype('float32').char,('Slat','Slon'))
     t[:] = Slon  
     t.long_name = 'South Pole Longitude'
     t.units     = 'degrees-east'

     t    = ncfile.createVariable('Slat',np.dtype('float32').char,('Slat','Slon'))
     t[:] = Slat  
     t.long_name = 'South Pole Latitude'
     t.units     = 'degrees-north'

     t    = ncfile.createVariable('Nice',np.dtype('float32').char,('time','Nlat','Nlon'))
     t[:] = Nice   
     t.long_name            = "North pole Sea Ice Concentration"
     t.units                = param.UNITS
     t.missing_value        = "255"
     t.pole_mask            = "251"
     t.coast_mask           = "253"
     t.land_mask            = "254"
     t.scale_factor         = "0.4"
     t.unpacked_valid_range = "0 255"
     t.valid_range          = "0 102"

     t    = ncfile.createVariable('Sice',np.dtype('float32').char,('time','Slat','Slon'))
     t[:] = Sice   
     t.long_name            = "South pole Sea Ice Concentration"
     t.units                = param.UNITS
     t.missing_value        = "255"
     t.pole_mask            = "251"
     t.coast_mask           = "253"
     t.land_mask            = "254"
     t.scale_factor         = "0.4"
     t.unpacked_valid_range = "0 255"
     t.valid_range          = "0 102"

     ncfile.close()


########################################################################


########################################################################
def write_grd_mom(fname,param,date,doy,month,nlon,nlat,slon,slat,ndata,sdata,ndatam,sdatam):

     tm,njm,nim = np.shape(ndata)
     tm,sjm,sim = np.shape(sdata)

     ncfile = Dataset(fname,'w',format='NETCDF3_CLASSIC')

     ncfile.createDimension('nx',int(nim))	
     ncfile.createDimension('ny',int(njm))
     ncfile.createDimension('sx',int(sim))	
     ncfile.createDimension('sy',int(sjm))
     ncfile.createDimension('time',tm)
     ncfile.createDimension('month',12)

     ncfile.title       = param.TITLE 
     ncfile.source      = param.SOURCE
     ncfile.history     = 'Updated on '+(time.strftime("%Y%m%d"))
     ncfile.Conventions = 'COARDS'
     ncfile.Version     = 'netcdf-4.0'

     t    = ncfile.createVariable('nlon',np.dtype('float32').char,('ny','nx'))
     t[:] = nlon  
     t.long_name = 'Longitude'
     t.units     = 'degrees-east'

     t    = ncfile.createVariable('nlat',np.dtype('float32').char,('ny','nx'))
     t[:] = nlat  
     t.long_name = 'Latitude'
     t.units     = 'degrees-north'

     t    = ncfile.createVariable('slon',np.dtype('float32').char,('sy','sx'))
     t[:] = slon  
     t.long_name = 'Longitude'
     t.units     = 'degrees-east'

     t    = ncfile.createVariable('slat',np.dtype('float32').char,('sy','sx'))
     t[:] = slat  
     t.long_name = 'Latitude'
     t.units     = 'degrees-north'

     t    = ncfile.createVariable('date',np.dtype('i4').char,('time'))
     t[:] = date    
     t.long_name = 'YYYYMMDD'

     t    = ncfile.createVariable('time',np.dtype('i4').char,('time'))
     t[:] = doy    
     t.long_name = 'Day of Year'

     t    = ncfile.createVariable('month',np.dtype('i4').char,('month'))
     t[:] = month    
     t.long_name = 'Month'

     t    = ncfile.createVariable('naice',np.dtype('float32').char,('time','ny','nx'))
     t[:] = ndata   
     t.units         = param.UNITS
     t.long_name     = 'Daily sea-ice north pole'
     t.missing_value = param.MISSING

     t    = ncfile.createVariable('saice',np.dtype('float32').char,('time','sy','sx'))
     t[:] = sdata   
     t.units         = param.UNITS
     t.long_name     = 'Daily sea-ice south pole'
     t.missing_value = param.MISSING

     t    = ncfile.createVariable('naicem',np.dtype('float32').char,('month','ny','nx'))
     t[:] = ndatam  
     t.units         = param.UNITS
     t.long_name     = 'monthly sea-ice north pole'
     t.missing_value = param.MISSING 

     t    = ncfile.createVariable('saicem',np.dtype('float32').char,('month','sy','sx'))
     t[:] = sdatam  
     t.units         = param.UNITS
     t.long_name     = 'monthly sea-ice south pole'
     t.missing_value = param.MISSING 

     ncfile.close()





