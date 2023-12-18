from netCDF4 import Dataset
import numpy as np
import time

def datetime2date(datetime):
        # yyyymmddhh to yyyymmdd
	year  = datetime/1000000
	month = (datetime-year*1000000)/10000
	day   = (datetime-year*1000000-month*10000)/100
	date  = year*10000 + month*100 + day
	return date

########################################################################
def ncinit(fname,idate1,idate2,slat,nlat):
	f          = Dataset(fname, 'r')
	DATE_TIME  = np.squeeze(f.variables['DATE_TIME'][:])
	LAT        = np.squeeze(f.variables['LAT'][:])
	NPTS       = np.squeeze(f.variables['NPTS'][:])
	maxnpts    = np.max(NPTS)
	obs_date   = datetime2date(DATE_TIME)
	good       = (obs_date>=idate1) & (obs_date<=idate2) & ((LAT>=nlat) | (LAT<=slat))
	I          = np.where(good)
	f.close()

	return I,maxnpts

########################################################################
def ncsub(fname,svar,I,maxnpts):
	# Subset Data
	# I: good obs
	# maxnpts: good levs
	f               = Dataset(fname, 'r')
	ncsub.VAR_ID     = np.squeeze(f.variables['VAR_ID'][:])
	ncsub.INST_ERROR = np.squeeze(f.variables['INST_ERROR'][:])
	ncsub.MISSING    = np.squeeze(f.variables['MISSING'][:])
	ncsub.N_PROF     = np.squeeze(f.variables['N_PROF'][I])
	ncsub.DATA_ID    = np.squeeze(f.variables['DATA_ID'][I])
	ncsub.DATE_TIME  = np.squeeze(f.variables['DATE_TIME'][I])
	ncsub.LON        = np.squeeze(f.variables['LON'][I])
	ncsub.LAT        = np.squeeze(f.variables['LAT'][I])
	ncsub.NPTS       = np.squeeze(f.variables['NPTS'][I])
	ncsub.QC_FLAG    = np.squeeze(f.variables['QC_FLAG'][I])
	ncsub.INST_ID    = np.squeeze(f.variables['INST_ID'][I])
	ncsub.QC_PRF     = np.squeeze(f.variables['QC_PRF'][I])
        ncsub.FIELD      = f.variables[svar][I[0],0:maxnpts]
	ncsub.OBS_ERROR  = np.squeeze(f.variables['OBS_ERROR'][I[0],0:maxnpts])
	ncsub.DEPTH      = np.squeeze(f.variables['DEPTH'][I[0],0:maxnpts])
	ncsub.QC_LEV     = np.squeeze(f.variables['QC_LEV'][I[0],0:maxnpts])
	f.close()

	return ncsub

########################################################################
def nc(fname,svar):
	f         = Dataset(fname, 'r')
	nc.VAR_ID     = np.squeeze(f.variables['VAR_ID'][:])
	nc.INST_ERROR = np.squeeze(f.variables['INST_ERROR'][:])
	nc.MISSING    = np.squeeze(f.variables['MISSING'][:])
	nc.N_PROF     = np.squeeze(f.variables['N_PROF'][:])
	nc.DATA_ID    = np.squeeze(f.variables['DATA_ID'][:])
	nc.DATE_TIME  = np.squeeze(f.variables['DATE_TIME'][:])
	nc.LON        = np.squeeze(f.variables['LON'][:])
	nc.LAT        = np.squeeze(f.variables['LAT'][:])
	nc.NPTS       = np.squeeze(f.variables['NPTS'][:])
	nc.QC_FLAG    = np.squeeze(f.variables['QC_FLAG'][:])
	nc.INST_ID    = np.squeeze(f.variables['INST_ID'][:])
	nc.QC_PRF     = np.squeeze(f.variables['QC_PRF'][:])
	nc.FIELD      = np.squeeze(f.variables[svar][:])
	nc.OBS_ERROR  = np.squeeze(f.variables['OBS_ERROR'][:])
	nc.DEPTH      = np.squeeze(f.variables['DEPTH'][:])
	nc.QC_LEV     = np.squeeze(f.variables['QC_LEV'][:])
	f.close()

	return nc

########################################################################
def subnc(nc,I,maxnpts):
	# Subset Data
	# I: good obs
	# maxnpts: good levs

	subnc.VAR_ID     = nc.VAR_ID 
	subnc.INST_ERROR = nc.INST_ERROR
	subnc.MISSING    = nc.MISSING 

	subnc.N_PROF    = nc.N_PROF[I]
	subnc.DATA_ID   = nc.DATA_ID[I]
	subnc.DATE_TIME = nc.DATE_TIME[I]
	subnc.LON       = nc.LON[I]
	subnc.LAT       = nc.LAT[I]
	subnc.NPTS      = nc.NPTS[I]
	subnc.QC_FLAG   = nc.QC_FLAG[I]
	subnc.INST_ID   = nc.INST_ID[I]
	subnc.QC_PRF    = nc.QC_PRF[I]
	subnc.FIELD     = np.squeeze(nc.FIELD[I,0:maxnpts])
	subnc.OBS_ERROR = np.squeeze(nc.OBS_ERROR[I,0:maxnpts])
	subnc.DEPTH     = np.squeeze(nc.DEPTH[I,0:maxnpts])
	subnc.QC_LEV    = np.squeeze(nc.QC_LEV[I,0:maxnpts])

	return subnc

########################################################################
def write_nc(fname,nc,svar):

     tm,zm = np.shape(nc.FIELD)

     ncfile = Dataset(fname,'w',format='NETCDF3_CLASSIC')

     ncfile.createDimension('N_PROF',None)
     ncfile.createDimension('N_LEVS',zm)
     ncfile.createDimension('SCALAR',1)

     t    = ncfile.createVariable('VAR_ID',np.dtype('i4').char,('SCALAR'))
     t[:] = nc.VAR_ID   

     t    = ncfile.createVariable('INST_ERROR',np.dtype('float32').char,('SCALAR'))
     t[:] = nc.INST_ERROR   

     t    = ncfile.createVariable('MISSING',np.dtype('float32').char,('SCALAR'))
     t[:] = nc.MISSING    
 
     t    = ncfile.createVariable('N_PROF',np.dtype('double').char,('N_PROF'))
     t[:] = nc.N_PROF   

     t    = ncfile.createVariable('DATA_ID',np.dtype('i4').char,('N_PROF'))
     t[:] = nc.DATA_ID    

     t    = ncfile.createVariable('DATE_TIME',np.dtype('i4').char,('N_PROF'))
     t[:] = nc.DATE_TIME    

     t    = ncfile.createVariable('LON',np.dtype('float32').char,('N_PROF'))
     t[:] = nc.LON  

     t    = ncfile.createVariable('LAT',np.dtype('float32').char,('N_PROF'))
     t[:] = nc.LAT    

     t    = ncfile.createVariable('NPTS',np.dtype('i4').char,('N_PROF'))
     t[:] = nc.NPTS    

     t    = ncfile.createVariable('QC_FLAG',np.dtype('i4').char,('N_PROF'))
     t[:] = nc.QC_FLAG    
 
     t    = ncfile.createVariable('INST_ID',np.dtype('i4').char,('N_PROF'))
     t[:] = nc.INST_ID   

     t    = ncfile.createVariable('QC_PRF',np.dtype('float32').char,('N_PROF'))
     t[:] = nc.QC_PRF

     t    = ncfile.createVariable(svar,np.dtype('float32').char,('N_PROF','N_LEVS'))
     t[:] = nc.FIELD  
 
     t    = ncfile.createVariable('OBS_ERROR',np.dtype('float32').char,('N_PROF','N_LEVS'))
     t[:] = nc.OBS_ERROR

     t    = ncfile.createVariable('DEPTH',np.dtype('float32').char,('N_PROF','N_LEVS'))
     t[:] = nc.DEPTH

     t    = ncfile.createVariable('QC_LEV',np.dtype('float32').char,('N_PROF','N_LEVS'))
     t[:] = nc.QC_LEV
 

     ncfile.close()

#
