from netCDF4 import Dataset
import numpy as np
import time
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from mpl_toolkits.basemap import Basemap

########################################################################
def param(inst,var):
	param.MISSING = '9.99e11'

	if (inst=='ARGO_GRD') & (var=='T'):
		param.INST         = 'ARGO_GRD'
		param.VAR          = 'T'
		param.SOURCE       = 'ftp://eftp.ifremer.fr'
		param.TITLE        = 'ARGO Gridded OA T/S'

	if (inst=='A3D') & (var=='T'):
		param.INST         = 'A3D'
		param.VAR          = 'T'
		param.SOURCE       = 'ftp://ftp.myocean.armor.cls.fr/Core/GLOBAL_ANALYSIS_PHYS_001_003/dataset-armor-3d-v1-myocean'
		param.TITLE        = 'ARMOR_3D T/S'

	if (inst=='A3D') & (var=='U'):
		param.INST         = 'A3D'
		param.VAR          = 'U'
		param.SOURCE       = 'ftp://ftp.myocean.armor.cls.fr/Core/GLOBAL_REP_PHYS_001_013/dataset-armor-3d-rep-monthly-v3-1-myocean'
		param.TITLE        = 'ARMOR_3D TSUV'

	if (inst=='A3D') & (var=='SSH'):
		param.INST         = 'A3D'
		param.VAR          = 'SSH'
		param.SOURCE       = 'ftp://ftp.myocean.armor.cls.fr/Core/GLOBAL_REP_PHYS_001_013/dataset-armor-3d-rep-monthly-v3-1-myocean'
		param.TITLE        = 'ARMOR_3D TSUV,SSH'

	if (inst=='AVISO') & (var=='SLA'):
		param.VAR          = 'sla'
		param.VAR_M        = 'slam'
		param.INST         = 'AVISO'
		param.VAR_ID       = 105
		param.INST_ID      = 517
		param.INST_ERROR   = 0.02
		param.UNITS        = 'm'
		param.LONG_NAME    = 'Daily Sea-Level Anomaly'
		param.LONG_NAME_M  = 'Monthly Sea-Level Anomaly'
		param.SOURCE       = 'ftp.aviso.oceanobs.com/global/nrt/sla'
		param.TITLE        = 'AVISO Gridded Sea Level Anomaly (m)'
		
	if (inst=='AVISO') & (var=='ADT'):
		param.VAR          = 'sla'
		param.VAR_M        = 'slam'
		param.INST         = 'AVISO'
		param.VAR_ID       = 105
		param.INST_ID      = 517
		param.INST_ERROR   = 0.02
		param.UNITS        = 'm'
		param.LONG_NAME    = 'Daily Sea-Level Anomaly'
		param.LONG_NAME_M  = 'Monthly Sea-Level Anomaly'
		param.SOURCE       = 'ftp.aviso.oceanobs.com/global/nrt/sla'
		param.TITLE        = 'AVISO Gridded Sea Level Anomaly (m)'
		
 	if (inst=='REYN') & (var=='T'):
		param.VAR         = 'sst'
		param.VAR_M       = 'sstm'
		param.INST        = 'REYN'
		param.VAR_ID      = 101
		param.INST_ID     = 516
		param.INST_ERROR  = 0.01
		param.UNITS       = 'C'
		param.LONG_NAME   = 'Daily Sea Surface Temperature'
		param.LONG_NAME_M = 'Monthly Sea Surface Temperature'
		param.SOURCE      = 'ftp://eclipse.ncdc.noaa.gov/pub/OI-daily-v2/'
		param.TITLE       = 'REYN-OI V2 Gridded Daily SST'
	
 	if (inst=='REYN') & (var=='ICE'):
		param.VAR         = 'aice'
		param.VAR_M       = 'aicem'
		param.INST        = 'REYN'
		param.VAR_ID      = 106
		param.INST_ID     = 523
		param.INST_ERROR  = 0.05
		param.UNITS       = '%'
		param.LONG_NAME   = 'Daily Ice Concentration'
		param.LONG_NAME_M = 'Monthly Ice Concentration'
		param.SOURCE      = 'ftp://eclipse.ncdc.noaa.gov/pub/OI-daily-v2/'
		param.TITLE       = 'REYN-OI V2 Gridded Daily ICE at North Pole'

 	if (inst=='OSTIA') & (var=='T'):
		param.VAR         = 'sst'
		param.VAR_M       = 'sstm'
		param.INST        = 'OSTIA'
		param.VAR_ID      = 101
		param.INST_ID     = 525
		param.INST_ERROR  = 0.01
		param.UNITS       = 'C'
		param.LONG_NAME   = 'Daily Sea Surface Temperature'
		param.LONG_NAME_M = 'Monthly Sea Surface Temperature'
		param.SOURCE      = '/gpfsm/dnb42/gmaofcst/ocean/obs/odas-2/OSTIA/FULL/'
		param.TITLE       = 'OSTIA Gridded Daily SST'
	
 	if (inst=='OSTIA') & (var=='ICE'):
		param.VAR         = 'aice'
		param.VAR_M       = 'aicem'
		param.INST        = 'OSTIA'
		param.VAR_ID      = 106
		param.INST_ID     = 526
		param.INST_ERROR  = 0.05
		param.UNITS       = '%'
		param.LONG_NAME   = 'Daily Ice Concentration'
		param.LONG_NAME_M = 'Monthly Ice Concentration'
		param.SOURCE      = '/gpfsm/dnb42/gmaofcst/ocean/obs/odas-2/OSTIA/FULL/'
		param.TITLE       = 'OSTIA Gridded Daily ICE'
	return param

########################################################################
def write_ostia_grd_qrt_day(fname,data):

     param.VAR         = 'sst'
     param.INST        = 'OSTIA'
     param.UNITS       = 'C'
     param.LONG_NAME   = 'Daily Sea Surface Temperature'
     param.SOURCE      = '/gpfsm/dnb42/gmaofcst/ocean/obs/odas-2/OSTIA/FULL/'
     param.TITLE       = 'OSTIA Gridded Daily SST'

     jm,im = np.shape(data.var)

     ncfile = Dataset(fname,'w',format='NETCDF3_CLASSIC')

     ncfile.createDimension('lon',im)	
     ncfile.createDimension('lat',jm)

     ncfile.title       = param.TITLE 
     ncfile.source      = param.SOURCE
     ncfile.history     = 'Updated on '+(time.strftime("%Y%m%d"))
     ncfile.Conventions = 'COARDS'
     ncfile.Version     = 'netcdf-4.0'

     t    = ncfile.createVariable('lon',np.dtype('float32').char,('lon'))
     t[:] = data.lon  
     t.long_name = 'Longitude'
     t.units     = 'degrees-east'

     t    = ncfile.createVariable('lat',np.dtype('float32').char,('lat'))
     t[:] = data.lat  
     t.long_name = 'Latitude'
     t.units     = 'degrees-north'

     t    = ncfile.createVariable(param.VAR,np.dtype('float32').char,('lat','lon'))
     t[:] = data.var  
     t.units         = param.UNITS
     t.long_name     = param.LONG_NAME

     if (param.INST=='OSTIA'):
     	t    = ncfile.createVariable('sst_err',np.dtype('float32').char,('lat','lon'))
     	t[:] = data.err   
     	t.units         = 'C'
     	t.long_name     = 'estimated error standard deviation of analysed_sst'

     	t    = ncfile.createVariable('ice',np.dtype('float32').char,('lat','lon'))
     	t[:] = data.ice   
     	t.units         = ' '
     	t.long_name     = 'daily sea ice area fraction'
  
     ncfile.close()


########################################################################
def write_woa13(data):

     param.INST        = 'WOA'
     param.SOURCE      = 'https://www.nodc.noaa.gov/OC5/woa13/'
     param.TITLE       = 'World Ocean Atlas 2013 V2'

     tm,km,jm,im = np.shape(data.Ti_mn)

     ncfile = Dataset(data.fname,'w',format='NETCDF3_CLASSIC')

     ncfile.createDimension('lon',im)	
     ncfile.createDimension('lat',jm)
     ncfile.createDimension('lev',km)	
     ncfile.createDimension('time',tm)

     ncfile.title       = param.TITLE 
     ncfile.source      = param.SOURCE
     ncfile.history     = 'Updated on '+(time.strftime("%Y%m%d"))
     ncfile.Conventions = 'COARDS'
     ncfile.Version     = 'netcdf-4.0'

     t           = ncfile.createVariable('lon',np.dtype('float32').char,('lon'))
     t[:]        = data.lon  
     t.long_name = 'Longitude'
     t.units     = 'degrees-east'

     t           = ncfile.createVariable('lat',np.dtype('float32').char,('lat'))
     t[:]        = data.lat  
     t.long_name = 'Latitude'
     t.units     = 'degrees-north'

     t           = ncfile.createVariable('time',np.dtype('float32').char,('time'))
     t[:]        = data.time  
     t.long_name = 'time'
     t.units     = 'months since 0000-00-00 00:00:00'

     t           = ncfile.createVariable('lev',np.dtype('float32').char,('lev'))
     t[:]        = data.lev 
     t.long_name = 'depth'
     t.units     = 'meters'

     t           = ncfile.createVariable('LON',np.dtype('float32').char,('lat','lon'))
     t[:]        = data.LON  
     t.long_name = 'Longitude'
     t.units     = 'degrees-east'

     t           = ncfile.createVariable('LAT',np.dtype('float32').char,('lat','lon'))
     t[:]        = data.LAT  
     t.long_name = 'Latitude'
     t.units     = 'degrees-north'

     ################# 
     t           = ncfile.createVariable('T',np.dtype('float32').char,('time','lev','lat','lon'))
     t[:]        = data.Ti_an 
     t.units     = "degrees_celsius"
     t.long_name = data.T_an_long

     #t           = ncfile.createVariable('T_mn',np.dtype('float32').char,('time','lev','lat','lon'))
     #t[:]        = data.Ti_mn 
     #t.units     = "degrees_celsius"
     #t.long_name = data.T_mn_long

     #t           = ncfile.createVariable('T_dd',np.dtype('float32').char,('time','lev','lat','lon'))
     #t[:]        = data.Ti_dd
     #t.units     = "degrees_celsius"
     #t.long_name = data.T_dd_long

     #t           = ncfile.createVariable('T_ma',np.dtype('float32').char,('time','lev','lat','lon'))
     #t[:]        = data.Ti_ma
     #t.units     = "degrees_celsius"
     #t.long_name = data.T_ma_long

     #t           = ncfile.createVariable('T_sd',np.dtype('float32').char,('time','lev','lat','lon'))
     #t[:]        = data.Ti_sd
     #t.units     = "degrees_celsius"
     #t.long_name = data.T_sd_long

     #t           = ncfile.createVariable('T_se',np.dtype('float32').char,('time','lev','lat','lon'))
     #t[:]        = data.Ti_se
     #t.units     = "degrees_celsius"
     #t.long_name = data.T_se_long

     #t           = ncfile.createVariable('T_oa',np.dtype('float32').char,('time','lev','lat','lon'))
     #t[:]        = data.Ti_oa
     #t.units     = "degrees_celsius"
     #t.long_name = data.T_oa_long

     #t           = ncfile.createVariable('T_gp',np.dtype('float32').char,('time','lev','lat','lon'))
     #t[:]        = data.Ti_gp
     #t.units     = "degrees_celsius"
     #t.long_name = data.T_gp_long
 
     ################# 
     t           = ncfile.createVariable('S',np.dtype('float32').char,('time','lev','lat','lon'))
     t[:]        = data.Si_an 
     t.units     = "psu"
     t.long_name = data.S_an_long

     #t           = ncfile.createVariable('Smn',np.dtype('float32').char,('time','lev','lat','lon'))
     #t[:]        = data.Si_mn
     #t.units     = "psu"
     #t.long_name = data.S_mn_long

     #t           = ncfile.createVariable('S_dd',np.dtype('float32').char,('time','lev','lat','lon'))
     #t[:]        = data.Si_dd
     #t.units     = "psu"
     #t.long_name = data.S_dd_long

     #t           = ncfile.createVariable('S_ma',np.dtype('float32').char,('time','lev','lat','lon'))
     #t[:]        = data.Si_ma
     #t.units     = "psu"
     #t.long_name = data.S_ma_long

     #t           = ncfile.createVariable('S_sd',np.dtype('float32').char,('time','lev','lat','lon'))
     #t[:]        = data.Si_sd
     #t.units     = "psu"
     t.long_name = data.S_sd_long
     #
     #s           = ncfile.createVariable('S_se',np.dtype('float32').char,('time','lev','lat','lon'))
     #s[:]        = data.Si_se
     #s.units     = "psu"
     #s.long_name = data.S_se_long

     #t           = ncfile.createVariable('S_oa',np.dtype('float32').char,('time','lev','lat','lon'))
     #t[:]        = data.Si_oa
     #t.units     = "psu"
     #t.long_name = data.S_oa_long

     #t           = ncfile.createVariable('S_gp',np.dtype('float32').char,('time','lev','lat','lon'))
     #t[:]        = data.Si_gp
     #t.units     = "psu"
     #t.long_name = data.S_gp_long

     ncfile.close()
########################################################################
def write_grd_one(fname,param,lon,lat,date,data,datam,data_err,data_anom):

     tm,jm,im = np.shape(data)

     ncfile = Dataset(fname,'w',format='NETCDF3_CLASSIC')

     ncfile.createDimension('lon',im)	
     ncfile.createDimension('lat',jm)
     ncfile.createDimension('time',tm)
     ncfile.createDimension('month',12)

     ncfile.title       = param.TITLE 
     ncfile.source      = param.SOURCE
     ncfile.history     = 'Updated on '+(time.strftime("%Y%m%d"))
     ncfile.Conventions = 'COARDS'
     ncfile.Version     = 'netcdf-4.0'

     t    = ncfile.createVariable('lon',np.dtype('float32').char,('lon'))
     t[:] = lon  
     t.long_name = 'Longitude'
     t.units     = 'degrees-east'

     t    = ncfile.createVariable('lat',np.dtype('float32').char,('lat'))
     t[:] = lat  
     t.long_name = 'Latitude'
     t.units     = 'degrees-north'

     t    = ncfile.createVariable('date',np.dtype('i4').char,('time'))
     t[:] = date    
     t.long_name = 'YYYYMMDD'

     t    = ncfile.createVariable(param.VAR,np.dtype('float32').char,('time','lat','lon'))
     t[:] = data   
     t.units         = param.UNITS
     t.long_name     = param.LONG_NAME
     t.missing_value = param.MISSING

     t    = ncfile.createVariable(param.VAR_M,np.dtype('float32').char,('month','lat','lon'))
     t[:] = datam  
     t.units         = param.UNITS
     t.long_name_M   = param.LONG_NAME
     t.missing_value = param.MISSING 

     if (param.VAR=='sst') & (param.INST=='REYN'):
     	t    = ncfile.createVariable('sst_err',np.dtype('float32').char,('time','lat','lon'))
     	t[:] = data_err   
     	t.units         = param.UNITS
     	t.long_name     = 'estimated error standard deviation of analysed_sst'
     	t.missing_value = param.MISSING

     	t    = ncfile.createVariable('sst_anom',np.dtype('float32').char,('time','lat','lon'))
     	t[:] = data_anom   
     	t.units         = param.UNITS
     	t.long_name     = 'daily sea surface temperature anomalies'
     	t.missing_value = param.MISSING

     ncfile.close()
########################################################################
def write_grd_mom_clim(fname,param,LON,LAT,date,data,ice,data_err,data_anom):

     tm,jm,im = np.shape(data)

     ncfile = Dataset(fname,'w',format='NETCDF3_CLASSIC')

     ncfile.createDimension('lon',im)	
     ncfile.createDimension('lat',jm)
     ncfile.createDimension('time',tm)

     ncfile.title       = param.TITLE 
     ncfile.source      = param.SOURCE
     ncfile.history     = 'Updated on '+(time.strftime("%Y%m%d"))
     ncfile.Conventions = 'COARDS'
     ncfile.Version     = 'netcdf-4.0'

     lon = LON[0,:]
     lat = LAT[:,0]

     t    = ncfile.createVariable('lon',np.dtype('float32').char,('lon'))
     t[:] = lon  
     t.long_name = 'Longitude'
     t.units     = 'degrees-east'

     t    = ncfile.createVariable('lat',np.dtype('float32').char,('lat'))
     t[:] = lat  
     t.long_name = 'Latitude'
     t.units     = 'degrees-north'

     t    = ncfile.createVariable('LON',np.dtype('float32').char,('lat','lon'))
     t[:] = LON  
     t.long_name = 'Longitude'
     t.units     = 'degrees-east'

     t    = ncfile.createVariable('LAT',np.dtype('float32').char,('lat','lon'))
     t[:] = LAT  
     t.long_name = 'Latitude'
     t.units     = 'degrees-north'

     t    = ncfile.createVariable('date',np.dtype('i4').char,('time'))
     t[:] = date    
     t.long_name = 'YYYYMMDD'


     if (param.VAR=='sla') & (param.INST=='AVISO'):
        t    = ncfile.createVariable('SLA',np.dtype('float32').char,('time','lat','lon'))
        t[:] = data   
        t.units         = param.UNITS
        t.long_name_M   = param.LONG_NAME
    	t.missing_value = param.MISSING

     if (param.VAR=='sst') & (param.INST=='REYN'):

        t    = ncfile.createVariable('T',np.dtype('float32').char,('time','lat','lon'))
        t[:] = data   
        t.units         = param.UNITS
        t.long_name_M   = param.LONG_NAME
    	t.missing_value = param.MISSING

     	t    = ncfile.createVariable('AICE',np.dtype('float32').char,('time','lat','lon'))
     	t[:] = ice 
     	t.units         = '%'
     	t.long_name_M   = 'Ice Concentration'
     	t.missing_value = param.MISSING 

     	t    = ncfile.createVariable('SST_ERR',np.dtype('float32').char,('time','lat','lon'))
     	t[:] = data_err   
     	t.units         = param.UNITS
     	t.long_name     = 'estimated error standard deviation of analysed_sst'
     	t.missing_value = param.MISSING

     	t    = ncfile.createVariable('SST_ANOM',np.dtype('float32').char,('time','lat','lon'))
     	t[:] = data_anom   
     	t.units         = param.UNITS
     	t.long_name     = 'monthly sea surface temperature anomalies'
     	t.missing_value = param.MISSING

     if (param.VAR=='sst') & (param.INST=='OSTIA'):

        t    = ncfile.createVariable('T',np.dtype('float32').char,('time','lat','lon'))
        t[:] = data   
        t.units         = param.UNITS
        t.long_name_M   = param.LONG_NAME
    	t.missing_value = param.MISSING

     	t    = ncfile.createVariable('AICE',np.dtype('float32').char,('time','lat','lon'))
     	t[:] = ice 
     	t.units         = '%'
     	t.long_name_M   = 'Ice Concentration'
     	t.missing_value = param.MISSING 

     	t    = ncfile.createVariable('SST_ERR',np.dtype('float32').char,('time','lat','lon'))
     	t[:] = data_err   
     	t.units         = param.UNITS
     	t.long_name     = 'estimated error standard deviation of analysed_sst'
     	t.missing_value = param.MISSING


     ncfile.close()

########################################################################
def write_grd_mom(fname,param,LON,LAT,date,data,ice,data_err,data_anom):

     tm,jm,im = np.shape(data)

     ncfile = Dataset(fname,'w',format='NETCDF3_CLASSIC')

     ncfile.createDimension('lon',im)	
     ncfile.createDimension('lat',jm)
     ncfile.createDimension('time',tm)

     ncfile.title       = param.TITLE 
     ncfile.source      = param.SOURCE
     ncfile.history     = 'Updated on '+(time.strftime("%Y%m%d"))
     ncfile.Conventions = 'COARDS'
     ncfile.Version     = 'netcdf-4.0'

     lon = LON[0,:]
     lat = LAT[:,0]

     t    = ncfile.createVariable('lon',np.dtype('float32').char,('lon'))
     t[:] = lon  
     t.long_name = 'Longitude'
     t.units     = 'degrees-east'

     t    = ncfile.createVariable('lat',np.dtype('float32').char,('lat'))
     t[:] = lat  
     t.long_name = 'Latitude'
     t.units     = 'degrees-north'

     t    = ncfile.createVariable('LON',np.dtype('float32').char,('lat','lon'))
     t[:] = LON  
     t.long_name = 'Longitude'
     t.units     = 'degrees-east'

     t    = ncfile.createVariable('LAT',np.dtype('float32').char,('lat','lon'))
     t[:] = LAT  
     t.long_name = 'Latitude'
     t.units     = 'degrees-north'

     t    = ncfile.createVariable('date',np.dtype('i4').char,('time'))
     t[:] = date    
     t.long_name = 'YYYYMMDD'


     if (param.VAR=='sla') & (param.INST=='AVISO'):
        t    = ncfile.createVariable('SLA',np.dtype('float32').char,('time','lat','lon'))
        t[:] = data   
        t.units         = param.UNITS
        t.long_name_M   = param.LONG_NAME
    	t.missing_value = param.MISSING

     if (param.VAR=='sst') & (param.INST=='REYN'):

        t    = ncfile.createVariable('T',np.dtype('float32').char,('time','lat','lon'))
        t[:] = data   
        t.units         = param.UNITS
        t.long_name_M   = param.LONG_NAME
    	t.missing_value = param.MISSING

     	t    = ncfile.createVariable('AICE',np.dtype('float32').char,('time','lat','lon'))
     	t[:] = ice 
     	t.units         = '%'
     	t.long_name_M   = 'Ice Concentration'
     	t.missing_value = param.MISSING 

     	t    = ncfile.createVariable('SST_ERR',np.dtype('float32').char,('time','lat','lon'))
     	t[:] = data_err   
     	t.units         = param.UNITS
     	t.long_name     = 'estimated error standard deviation of analysed_sst'
     	t.missing_value = param.MISSING

     	t    = ncfile.createVariable('SST_ANOM',np.dtype('float32').char,('time','lat','lon'))
     	t[:] = data_anom   
     	t.units         = param.UNITS
     	t.long_name     = 'monthly sea surface temperature anomalies'
     	t.missing_value = param.MISSING

     if (param.VAR=='sst') & (param.INST=='OSTIA'):

        t    = ncfile.createVariable('T',np.dtype('float32').char,('time','lat','lon'))
        t[:] = data   
        t.units         = param.UNITS
        t.long_name_M   = param.LONG_NAME
    	t.missing_value = param.MISSING

     	t    = ncfile.createVariable('AICE',np.dtype('float32').char,('time','lat','lon'))
     	t[:] = ice 
     	t.units         = '%'
     	t.long_name_M   = 'Ice Concentration'
     	t.missing_value = param.MISSING 

     	t    = ncfile.createVariable('SST_ERR',np.dtype('float32').char,('time','lat','lon'))
     	t[:] = data_err   
     	t.units         = param.UNITS
     	t.long_name     = 'estimated error standard deviation of analysed_sst'
     	t.missing_value = param.MISSING


     ncfile.close()

########################################################################
def write_grd_mom_3D(fname,param,LON,LAT,LEV,date,T,S,U,V):

     tm,km,jm,im = np.shape(T)

     ncfile = Dataset(fname,'w',format='NETCDF3_CLASSIC')

     ncfile.createDimension('lon',im)	
     ncfile.createDimension('lat',jm)
     ncfile.createDimension('lev',km)
     ncfile.createDimension('time',tm)

     ncfile.title       = param.TITLE 
     ncfile.source      = param.SOURCE
     ncfile.history     = 'Updated on '+(time.strftime("%Y%m%d"))
     ncfile.Conventions = 'COARDS'
     ncfile.Version     = 'netcdf-4.0'

     lon = LON[0,:]
     lat = LAT[:,0]

     t    = ncfile.createVariable('lon',np.dtype('float32').char,('lon'))
     t[:] = lon  
     t.long_name = 'Longitude'
     t.units     = 'degrees-east'

     t    = ncfile.createVariable('lat',np.dtype('float32').char,('lat'))
     t[:] = lat  
     t.long_name = 'Latitude'
     t.units     = 'degrees-north'

     t    = ncfile.createVariable('LON',np.dtype('float32').char,('lat','lon'))
     t[:] = LON  
     t.long_name = 'Longitude'
     t.units     = 'degrees-east'

     t    = ncfile.createVariable('LAT',np.dtype('float32').char,('lat','lon'))
     t[:] = LAT  
     t.long_name = 'Latitude'
     t.units     = 'degrees-north'

     t    = ncfile.createVariable('date',np.dtype('i4').char,('time'))
     t[:] = date    
     t.long_name = 'YYYYMMDD'

     if (param.INST=='A3D') & (param.VAR=='T'):

        t    = ncfile.createVariable('T',np.dtype('float32').char,('time','lev','lat','lon'))
        t[:] = T   
        t.units         = 'C'
        t.long_name   = 'Temperature'
    	t.missing_value = param.MISSING 

     	t    = ncfile.createVariable('S',np.dtype('float32').char,('time','lev','lat','lon'))
     	t[:] = S 
     	t.units         = ''
     	t.long_name   = 'Salinity'
     	t.missing_value = param.MISSING 

     if (param.INST=='ARGO_GRD') & (param.VAR=='T'):

        t    = ncfile.createVariable('T',np.dtype('float32').char,('time','lev','lat','lon'))
        t[:] = T   
        t.units         = 'C'
        t.long_name   = 'Temperature'
    	t.missing_value = param.MISSING 

     	t    = ncfile.createVariable('S',np.dtype('float32').char,('time','lev','lat','lon'))
     	t[:] = S 
     	t.units         = ''
     	t.long_name   = 'Salinity'
     	t.missing_value = param.MISSING 


     if (param.INST=='A3D') & (param.VAR=='U'):

        t    = ncfile.createVariable('T',np.dtype('float32').char,('time','lev','lat','lon'))
        t[:] = T   
        t.units         = 'C'
        t.long_name   = 'Temperature'
    	t.missing_value = param.MISSING 

     	t    = ncfile.createVariable('S',np.dtype('float32').char,('time','lev','lat','lon'))
     	t[:] = S 
     	t.units         = ''
     	t.long_name   = 'Salinity'
     	t.missing_value = param.MISSING 

     	t    = ncfile.createVariable('U',np.dtype('float32').char,('time','lev','lat','lon'))
     	t[:] = U 
     	t.units         = 'm/s'
     	t.long_name   = 'Zonal Velocity'
     	t.missing_value = param.MISSING 

     	t    = ncfile.createVariable('V',np.dtype('float32').char,('time','lev','lat','lon'))
     	t[:] = V 
     	t.units         = 'm/s'
     	t.long_name   = 'Meridional Velocity'
     	t.missing_value = param.MISSING 

     ncfile.close()
########################################################################
def write_grd_mom_3D_5(fname,param,LON,LAT,LEV,date,T,S,U,V,SSH):

     tm,km,jm,im = np.shape(T)

     ncfile = Dataset(fname,'w',format='NETCDF3_CLASSIC')

     ncfile.createDimension('lon',im)	
     ncfile.createDimension('lat',jm)
     ncfile.createDimension('lev',km)
     ncfile.createDimension('time',tm)

     ncfile.title       = param.TITLE 
     ncfile.source      = param.SOURCE
     ncfile.history     = 'Updated on '+(time.strftime("%Y%m%d"))
     ncfile.Conventions = 'COARDS'
     ncfile.Version     = 'netcdf-4.0'

     lon = LON[0,:]
     lat = LAT[:,0]

     t    = ncfile.createVariable('lon',np.dtype('float32').char,('lon'))
     t[:] = lon  
     t.long_name = 'Longitude'
     t.units     = 'degrees-east'

     t    = ncfile.createVariable('lat',np.dtype('float32').char,('lat'))
     t[:] = lat  
     t.long_name = 'Latitude'
     t.units     = 'degrees-north'

     t    = ncfile.createVariable('LON',np.dtype('float32').char,('lat','lon'))
     t[:] = LON  
     t.long_name = 'Longitude'
     t.units     = 'degrees-east'

     t    = ncfile.createVariable('LAT',np.dtype('float32').char,('lat','lon'))
     t[:] = LAT  
     t.long_name = 'Latitude'
     t.units     = 'degrees-north'

     t    = ncfile.createVariable('date',np.dtype('i4').char,('time'))
     t[:] = date    
     t.long_name = 'YYYYMMDD'

     if (param.INST=='A3D') & (param.VAR=='T'):

        t    = ncfile.createVariable('T',np.dtype('float32').char,('time','lev','lat','lon'))
        t[:] = T   
        t.units         = 'C'
        t.long_name   = 'Temperature'
    	t.missing_value = param.MISSING 

     	t    = ncfile.createVariable('S',np.dtype('float32').char,('time','lev','lat','lon'))
     	t[:] = S 
     	t.units         = ''
     	t.long_name   = 'Salinity'
     	t.missing_value = param.MISSING 

     if (param.INST=='ARGO_GRD') & (param.VAR=='T'):

        t    = ncfile.createVariable('T',np.dtype('float32').char,('time','lev','lat','lon'))
        t[:] = T   
        t.units         = 'C'
        t.long_name   = 'Temperature'
    	t.missing_value = param.MISSING 

     	t    = ncfile.createVariable('S',np.dtype('float32').char,('time','lev','lat','lon'))
     	t[:] = S 
     	t.units         = ''
     	t.long_name   = 'Salinity'
     	t.missing_value = param.MISSING 


     if (param.INST=='A3D') & (param.VAR=='U'):

        t    = ncfile.createVariable('T',np.dtype('float32').char,('time','lev','lat','lon'))
        t[:] = T   
        t.units         = 'C'
        t.long_name   = 'Temperature'
    	t.missing_value = param.MISSING 

     	t    = ncfile.createVariable('S',np.dtype('float32').char,('time','lev','lat','lon'))
     	t[:] = S 
     	t.units         = ''
     	t.long_name   = 'Salinity'
     	t.missing_value = param.MISSING 

     	t    = ncfile.createVariable('U',np.dtype('float32').char,('time','lev','lat','lon'))
     	t[:] = U 
     	t.units         = 'm/s'
     	t.long_name   = 'Zonal Velocity'
     	t.missing_value = param.MISSING 

     	t    = ncfile.createVariable('V',np.dtype('float32').char,('time','lev','lat','lon'))
     	t[:] = V 
     	t.units         = 'm/s'
     	t.long_name   = 'Meridional Velocity'
     	t.missing_value = param.MISSING 

     if (param.INST=='A3D') & (param.VAR=='SSH'):

        t    = ncfile.createVariable('T',np.dtype('float32').char,('time','lev','lat','lon'))
        t[:] = T   
        t.units         = 'C'
        t.long_name   = 'Temperature'
    	t.missing_value = param.MISSING 

     	t    = ncfile.createVariable('S',np.dtype('float32').char,('time','lev','lat','lon'))
     	t[:] = S 
     	t.units         = ''
     	t.long_name   = 'Salinity'
     	t.missing_value = param.MISSING 

     	t    = ncfile.createVariable('U',np.dtype('float32').char,('time','lev','lat','lon'))
     	t[:] = U 
     	t.units         = 'm/s'
     	t.long_name   = 'Zonal Velocity'
     	t.missing_value = param.MISSING 

     	t    = ncfile.createVariable('V',np.dtype('float32').char,('time','lev','lat','lon'))
     	t[:] = V 
     	t.units         = 'm/s'
     	t.long_name   = 'Meridional Velocity'
     	t.missing_value = param.MISSING 

     	t    = ncfile.createVariable('SSH',np.dtype('float32').char,('time','lat','lon'))
     	t[:] = SSH
     	t.units         = 'm'
     	t.long_name   = 'Sea Surface Height'
     	t.missing_value = param.MISSING 

     ncfile.close()
########################################################################
def write_am(data):

     jm,im = np.shape(data.var)

     ncfile = Dataset(data.fout,'w',format='NETCDF3_CLASSIC')

     ncfile.createDimension('lon',im)	
     ncfile.createDimension('lat',jm)

     ncfile.history     = 'Updated on '+(time.strftime("%Y%m%d"))
     ncfile.Conventions = 'COARDS'
     ncfile.Version     = 'netcdf-4.0'

     t    = ncfile.createVariable('lon',np.dtype('float32').char,('lon'))
     t[:] = data.lon  

     t    = ncfile.createVariable('lat',np.dtype('float32').char,('lat'))
     t[:] = data.lat  

     t    = ncfile.createVariable(data.svar,np.dtype('float32').char,('lat','lon'))
     t[:] = data.var  
    
     ncfile.close()
########################################################################


