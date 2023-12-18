from netCDF4 import Dataset
import numpy as np
import time


def param(inst,var):
    param.MISSING = '9.99e11'
    if (inst=='AQUARIUS'):
        param.VAR        = 'SSS'
        param.VAR_ID     = 102
        param.INST_ID    = 553
        param.INST_ERROR = 0.1
        param.UNITS      = 'psu'
        param.LONG_NAME  = 'Sea Surface Salinity'
        param.SOURCE     = 'ftp://podaac-ftp.jpl.nasa.gov/allData/aquarius/L2/V5/SCI'
        param.TITLE      = 'Aquarius L2 V5 Sea Surface Salinity'
    if (inst=='SMAP'):
        param.VAR        = 'SSS'
        param.VAR_ID     = 102
        param.INST_ID    = 556
        param.INST_ERROR = 0.1
        param.UNITS      = 'psu'
        param.LONG_NAME  = 'Sea Surface Salinity'
        param.SOURCE     = 'ftp://podaac-ftp.jpl.nasa.gov/allData/smap/L2/JPL/V5.0/'
        param.TITLE      = 'SMAP Sea Surface Salinity'
    return param


########################################################################
def write_nc_2D(fname,param,N_PROF,DATA_ID,DATE_TIME,LON,LAT,NPTS,QC_FLAG,INST_ID,QC_PRF,DATA,OBS_ERROR,DEPTH,QC_LEV,DATA_MODE):

     tm,zm = np.shape(DATA)

     ncfile = Dataset(fname,'w',format='NETCDF3_CLASSIC')

     ncfile.createDimension('N_PROF',None)
     ncfile.createDimension('N_LEVS',zm)
     ncfile.createDimension('SCALAR',1)

     ncfile.title       = param.TITLE 
     ncfile.source      = param.SOURCE
     ncfile.history     = 'Updated on '+(time.strftime("%Y%m%d"))
     ncfile.Conventions = 'COARDS'
     ncfile.Version     = 'netcdf-4.0'

     t    = ncfile.createVariable('VAR_ID',np.dtype('i4').char,('SCALAR'))
     t[:] = param.VAR_ID   
     t.long_name = 'Variable Identification Code'

     t    = ncfile.createVariable('INST_ERROR',np.dtype('float32').char,('SCALAR'))
     t[:] = param.INST_ERROR   
     t.long_name = 'Instrument Error'

     t    = ncfile.createVariable('MISSING',np.dtype('float32').char,('SCALAR'))
     t[:] = param.MISSING    
     t.long_name = 'Missing Data Value'

     ##########################################################################

     t    = ncfile.createVariable('N_PROF',np.dtype('double').char,('N_PROF'))
     t[:] = N_PROF   

     t    = ncfile.createVariable('DATA_ID',np.dtype('i4').char,('N_PROF'))
     t[:] = DATA_ID    
     t.long_name = 'Unique Data Identifier'

     t    = ncfile.createVariable('DATE_TIME',np.dtype('i4').char,('N_PROF'))
     t[:] = DATE_TIME    
     t.long_name = 'Date of Observation: YYYYMMDDhh'

     t    = ncfile.createVariable('LON',np.dtype('float32').char,('N_PROF'))
     t[:] = LON  
     t.long_name = 'Longitude'
     t.units     = 'degrees-east'

     t    = ncfile.createVariable('LAT',np.dtype('float32').char,('N_PROF'))
     t[:] = LAT    
     t.long_name = 'Latitude'
     t.units     = 'degrees-north'

     t    = ncfile.createVariable('NPTS',np.dtype('i4').char,('N_PROF'))
     t[:] = NPTS    
     t.long_name = 'Number of Data Levels in Profile'

     t    = ncfile.createVariable('QC_FLAG',np.dtype('i4').char,('N_PROF'))
     t[:] = QC_FLAG    
     t.long_name   = 'QC Profile Flag'
     t.conventions = 'No QC : 0'

     t    = ncfile.createVariable('INST_ID',np.dtype('i4').char,('N_PROF'))
     t[:] = INST_ID   
     t.long_name = 'Instrument Identification Code' 

     t    = ncfile.createVariable('QC_PRF',np.dtype('float32').char,('N_PROF'))
     t[:] = QC_PRF
     t.long_name   = 'Profile Quality Control Flag'
     t.conventions = 'Standard Quality : 1'

     t    = ncfile.createVariable('DATA_MODE',np.dtype('i4').char,('N_PROF'))
     t[:] = DATA_MODE
     t.long_name   = ''
     t.conventions = 'Delayed Mode : 1, Near Real Time : 0'


    ##########################################################################

     t    = ncfile.createVariable(param.VAR,np.dtype('float32').char,('N_PROF','N_LEVS'))
     t[:] = DATA   
     t.units         = param.UNITS
     t.long_name     = param.LONG_NAME
     t.missing_value = param.MISSING

     t    = ncfile.createVariable('OBS_ERROR',np.dtype('float32').char,('N_PROF','N_LEVS'))
     t[:] = OBS_ERROR
     t.long_name     = 'Observation Error'
     t.missing_value = param.MISSING

     t    = ncfile.createVariable('DEPTH',np.dtype('float32').char,('N_PROF','N_LEVS'))
     t[:] = DEPTH
     t.units         = 'meters'
     t.long_name     = 'Profile Depth'
     t.missing_value = param.MISSING

     t    = ncfile.createVariable('QC_LEV',np.dtype('float32').char,('N_PROF','N_LEVS'))
     t[:] = QC_LEV
     t.long_name   = 'Level Quality Control Flag'
     t.conventions = 'Standard Quality : 1'


     ncfile.close()

########################################################################
def append_nc_2D(fname,param,N_PROF,DATA_ID,DATE_TIME,LON,LAT,NPTS,QC_FLAG,INST_ID,QC_PRF,DATA,OBS_ERROR,DEPTH,QC_LEV,DATA_MODE):

     ncfile  = Dataset(fname,'a')

     ncfile.history     = 'Updated on '+(time.strftime("%Y%m%d"))

     n_prof0    = ncfile.variables['N_PROF']
     data_id0   = ncfile.variables['DATA_ID']
     date_time0 = ncfile.variables['DATE_TIME']
     lon0       = ncfile.variables['LON']
     lat0       = ncfile.variables['LAT']
     npts0      = ncfile.variables['NPTS']
     qc_flag0   = ncfile.variables['QC_FLAG']
     data_mode0 = ncfile.variables['DATA_MODE']
     inst_id0   = ncfile.variables['INST_ID']
     qc_prf0    = ncfile.variables['QC_PRF']
     data0      = ncfile.variables[param.VAR]
     obs_error0 = ncfile.variables['OBS_ERROR']
     depth0     = ncfile.variables['DEPTH']
     qc_lev0    = ncfile.variables['QC_LEV']

     tm0,zm0 = np.shape(data0)  
     tm,zm   = np.shape(DATA)  

     #print '     Existing ',tm0, ' Appending ',tm, np.shape(N_PROF)[0]

     n_prof0[tm0:tm+tm0]    = N_PROF[:]
     data_id0[tm0:tm+tm0]   = DATA_ID[:] 
     date_time0[tm0:tm+tm0] = DATE_TIME[:]
     lon0[tm0:tm+tm0]       = LON[:]
     lat0[tm0:tm+tm0]       = LAT[:]
     npts0[tm0:tm+tm0]      = NPTS[:]
     qc_flag0[tm0:tm+tm0]   = QC_FLAG[:]
     data_mode0[tm0:tm+tm0] = DATA_MODE[:]
     inst_id0[tm0:tm+tm0]   = INST_ID[:]
     qc_prf0[tm0:tm+tm0]    = QC_PRF[:]
     data0[tm0:tm+tm0]      = DATA[:]
     obs_error0[tm0:tm+tm0] = OBS_ERROR[:]
     depth0[tm0:tm+tm0]     = DEPTH[:]
     qc_lev0[tm0:tm+tm0]    = QC_LEV[:]


     ncfile.close()





