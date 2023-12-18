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

###########################################################
def grid_mom_1440x1080():
	
	proddir  = '/discover/nobackup/projects/gmao/ssd/g5odas/production/'
	gridfile = proddir + 'GEOS5odas-5.00/RC/GRIDS/grid_spec-1440x1080x50.nc'
	ds       = Dataset(gridfile, 'r', format='NETCDF3')
	z        = ds.variables['zt'][:]
	zb       = ds.variables['zb'][:]
	x        = ds.variables['grid_x_T'][:] 
	y        = ds.variables['grid_y_T'][:] 
	lon      = ds.variables['x_T'][:,:]    
	lat      = ds.variables['y_T'][:,:]     
	mask     = ds.variables['wet'][:]
	areaT    = ds.variables['area_T'][:]	
	ztopo    = ds.variables['depth_t'][:]
	numlevs    = ds.variables['num_levels'][:]	 	      
	ds.close()	
	
        class gridclass:
		pass
	grid = gridclass()

	grid.z     = z
	grid.zb    = zb
	grid.x     = x       
	grid.y     = y
	grid.lon   = lon
	grid.lat   = lat
	grid.mask  = mask
	grid.areaT = areaT        
	grid.ztopo = ztopo   
	grid.numlevs = numlevs
      
	grid.x[grid.x<-180]=grid.x[grid.x<-180]+360
	
	grid.y0 = np.where((grid.y>=-0.1) & (grid.y<=0.3))
	
		
	return grid

###########################################################
def grid_mom_720x410():
	
	proddir  = '/discover/nobackup/projects/gmao/ssd/g5odas/production/'
	gridfile = proddir + 'GEOS5odas-5.00/RC/GRIDS/grid_spec_720x410.nc'
	ds       = Dataset(gridfile, 'r', format='NETCDF3')
	z        = ds.variables['zt'][:]
	zb       = ds.variables['zb'][:]
	x        = ds.variables['grid_x_T'][:] 
	y        = ds.variables['grid_y_T'][:] 
	lon      = ds.variables['x_T'][:,:]    
	lat      = ds.variables['y_T'][:,:]     
	mask     = ds.variables['wet'][:]
	areaT    = ds.variables['area_T'][:]	
	ztopo    = ds.variables['depth_t'][:]	      
	ds.close()	
	
        class gridclass:
		pass
	grid = gridclass()

	grid.z     = z
	grid.zb    = zb
	grid.x     = x       
	grid.y     = y
	grid.lon   = lon
	grid.lat   = lat
	grid.mask  = mask
	grid.areaT = areaT        
	grid.ztopo = ztopo  
      
	grid.x[grid.x<-180]=grid.x[grid.x<-180]+360
	
	grid.y0 = np.where((grid.y>=-0.1) & (grid.y<=0.3))
	
		
	return grid

###########################################################
def grid_mom_360x200():
	
	proddir  = '/discover/nobackup/projects/gmao/ssd/g5odas/production/'
	gridfile = proddir + 'GEOS5odas-5.00/RC/GRIDS/grid_spec_360x200.nc'
	ds       = Dataset(gridfile, 'r', format='NETCDF3')
	lon      = ds.variables['geolon_t'][:,:] 
	lat      = ds.variables['geolat_t'][:,:] 
	x        = ds.variables['gridlon_t'][:] 
	y        = ds.variables['gridlat_t'][:] 
	z        = ds.variables['zt'][:]

	ds.close()	
	
        class gridclass:
		pass
	grid = gridclass()
	grid.lon   = lon
	grid.lat   = lat
	grid.x     = x
	grid.y     = y
	grid.z     = z
	#grid.x[grid.x<-180]=grid.x+360
		
	return grid

###########################################################
def grid_one(expgrid):
	
	proddir  = '/discover/nobackup/projects/gmao/ssd/g5odas/production/'
	gridfile = proddir + 'GEOS5odas-5.00/RC/GRIDS/ANA_glb_ind_'+expgrid+'.nc'
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

	#if expgrid=='360x200':    
	#	grid.x[grid.x<-180]=grid.x+360
		
	return grid

def obs(year,month,day,var,inst,sec,islice,datatype):
        svar = var
	if var=='T':
		svar='TEMP'
	if var=='S':
		svar='SALT'

        day1=int(day)-5
	day2=int(day)+5
	if day1<0:
		day1=1
		day2=10
	if day2>31:
		day2=31
		day1=20
	sdate1=year+month+str(day1)+'00'
	sdate2=year+month+str(day2)+'24'

	if int(day)==0:
		sdate1=year+month+'0100'
		sdate2=year+month+'3124'

	#sdate1=year+month+'2000'
	#sdate2=year+month+'3024'
        #print sdate1, sdate2
	inc = 1

	if int(year) < 2011:
		obsdir = '/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OBS/'+inst+'_5.0/'
                fname = obsdir+var+'_'+inst+'_'+year+'.nc'

		if (var=='U') | (var=='V'):
			obsdir = '/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OBS/'+datatype+'_5.0/'
			fname = obsdir+inst+'_'+var+'_'+datatype+'_'+year+'.nc'

	if int(year) >= 2012:
		obsdir = '/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OBS//REAL_TIME/'
		fname = obsdir+var+'_'+inst+'_'+year+'.nc'
        cnt = 0
        print fname
	try: 
		obsgrp = Dataset(fname, 'r', format='NETCDF4')
		adate  = obsgrp.variables['DATE_TIME'][:]
		alat   = obsgrp.variables['LAT'][:]
		alon   = obsgrp.variables['LON'][:]
		aqcprf = obsgrp.variables['QC_PRF'][:]	
		alev   = obsgrp.variables['DEPTH'][:,0:39]

		if sec=='yz':
			inc=1
			x360 = np.where(alon<0)
			alon[x360]=alon[x360]+360
			if int(islice)<0:
				islice =  islice+360
			good=(adate>=int(sdate1)) & (adate<=int(sdate2)) & (aqcprf==1) & (alon>=islice-inc) & (alon<=islice+inc)			
		if sec=='xz':
			good=(adate>=int(sdate1)) & (adate<=int(sdate2)) & (aqcprf==1) & (alat>=islice-inc) & (alat<=islice+inc)
		#if sec=='xy':
		#	good=(adate>=int(sdate1)) & (adate<=int(sdate2)) & (aqcprf==1) & (alev>=islice-inc) & (alat<=islice+inc)



		I=np.where(good)
		#print np.shape(I), islice
		#print var
		#time.sleep(99999)
		

		if  (len(I[0])!=0):
			alon   = obsgrp.variables['LON'][I]
			alat   = obsgrp.variables['LAT'][I]
			anpts  = obsgrp.variables['NPTS'][I]
			adata  = obsgrp.variables[svar][I[0],0:39]
			aqclev = obsgrp.variables['QC_LEV'][I[0],0:39]
			alev   = obsgrp.variables['DEPTH'][I[0],0:39]
			obsgrp.close()

			x360 = np.where(alon<0)
			alon[x360]=alon[x360]+360

			im = np.shape(adata)[0]
			jm = np.shape(adata)[1]

			obsdat = np.zeros([im*jm])
			obslev = np.zeros([im*jm])
			obslon = np.zeros([im*jm])
			obslat = np.zeros([im*jm])

			for i in range(im):
				for j in range(jm):
					if (adata[i,j]<9999) & (aqclev[i,j]==1):
 						obsdat[cnt]=adata[i,j]
						obslev[cnt]=alev[i,j]
                        			obslon[cnt]=alon[i]
                        			obslat[cnt]=alat[i]
                        			cnt = cnt+1
	except:
 		obs = 0

	class obsclass:
		pass
	obs = obsclass()

	#print obslon[0:cnt-1]
	#print obslat[0:cnt-1]
	#print obslev[0:cnt-1]
	#print obsdat[0:cnt-1]


	#time.sleep(99999)
    
 	if cnt>0:
       		obs.lev = obslev[0:cnt]
		obs.lon = obslon[0:cnt]
		obs.lat = obslat[0:cnt]
		obs.dat = obsdat[0:cnt]
		obs.cnt = cnt
	else:
       		obs.lev = 0
		obs.lon = np.zeros(2)
		obs.lat = 0
 		obs.dat = 0
		obs.cnt = 0

	return obs

###########################################################
def curr(year,month,day,var,inst,sec,islice,datatype):
        svar = var

        day1=int(day)-5
	day2=int(day)+5
	if day1<0:
		day1=1
		day2=10
	if day2>31:
		day2=31
		day1=20
	sdate1=year+month+str(day1)+'00'
	sdate2=year+month+str(day2)+'24'

	if int(day)==0:
		sdate1=year+month+'0100'
		sdate2=year+month+'3124'

	inc = 1

	if int(year) < 2013:
		obsdir = '/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OBS/'+datatype+'_6.0/'
		fname = obsdir+inst+'_'+var+'_'+datatype+'_'+year+'.nc'

	if int(year) >= 2013:
		obsdir = '/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OBS/REAL_TIME/'
		fname = obsdir+inst+'_'+var+'_'+datatype+'_'+year+'.nc'
        cnt = 0
        print fname
	try: 
		obsgrp = Dataset(fname, 'r', format='NETCDF4')
		adate  = obsgrp.variables['DATE_TIME'][:]
		alat   = obsgrp.variables['LAT'][:]
		alon   = obsgrp.variables['LON'][:]
		aqcprf = obsgrp.variables['QC_PRF'][:]	
		alev   = obsgrp.variables['DEPTH'][:,0:39]

		if sec=='yz':
			inc=1
			x360 = np.where(alon<0)
			alon[x360]=alon[x360]+360
			if int(islice)<0:
				islice =  islice+360
			good=(adate>=int(sdate1)) & (adate<=int(sdate2)) & (aqcprf==1) & (alon>=islice-inc) & (alon<=islice+inc)			
		if sec=='xz':
			good=(adate>=int(sdate1)) & (adate<=int(sdate2)) & (aqcprf==1) & (alat>=islice-inc) & (alat<=islice+inc)

		I=np.where(good)
		
		if  (len(I[0])!=0):
			alon   = obsgrp.variables['LON'][I]
			alat   = obsgrp.variables['LAT'][I]
			anpts  = obsgrp.variables['NPTS'][I]
			adata  = obsgrp.variables[svar][I[0],0:39]
			aqclev = obsgrp.variables['QC_LEV'][I[0],0:39]
			alev   = obsgrp.variables['DEPTH'][I[0],0:39]
			obsgrp.close()

			x360 = np.where(alon<0)
			alon[x360]=alon[x360]+360

			im = np.shape(adata)[0]
			jm = np.shape(adata)[1]

			obsdat = np.zeros([im*jm])
			obslev = np.zeros([im*jm])
			obslon = np.zeros([im*jm])
			obslat = np.zeros([im*jm])
			
			for j in range(jm):
				goodi=adata[:,j]<9999
                                I = np.where(goodi)
				goodj = np.shape(I)[1]
				print goodj
				if goodj>0:
 					obsdat[cnt]=np.nanmean(adata[I,j])
					obslev[cnt]=np.nanmean(alev[I,j])
                        		obslon[cnt]=np.nanmean(alon[I])
                        		obslat[cnt]=np.nanmean(alat[I])
                        		cnt = cnt+1
	except:
 		obs = 0

	class obsclass:
		pass
	obs = obsclass()

	#print obslon[0:cnt-1]
	#print obslat[0:cnt-1]
	#print obslev[0:cnt-1]
	#print obsdat[0:cnt-1]

	
	#time.sleep(99999)
    
 	if cnt>0:
       		obs.lev = obslev[0:cnt]
		obs.lon = obslon[0:cnt]
		obs.lat = obslat[0:cnt]
		obs.dat = obsdat[0:cnt]
		obs.cnt = cnt
	else:
       		obs.lev = 0
		obs.lon = np.zeros(2)
		obs.lat = 0
 		obs.dat = 0
		obs.cnt = 0

	return obs

###########################################################
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

        
        class gridclass:
		pass
	grid = gridclass()
	grid.z    = z
	grid.lon  = lon
	grid.lat  = lat
        grid.mask = mask
        grid.depth = depth
        grid.area = area/1e6

	return grid


###########################################################
#Binary reading functions
###########################################################
def read_odas_2d(filename,VARTYPE,level):

    if (VARTYPE=='t'):       vartype=1
    if (VARTYPE=='s'):       vartype=2
    if (VARTYPE=='u'):       vartype=3
    if (VARTYPE=='v'):       vartype=4
    if (VARTYPE=='ssh'):     vartype=5    
    if (VARTYPE=='aice'):    vartype=6
    if (VARTYPE=='hice'):    vartype=7
    if (VARTYPE=='tx'):      vartype=8
    if (VARTYPE=='ty'):      vartype=9
    if (VARTYPE=='rho'):     vartype=10
    if (VARTYPE=='ml'):      vartype=11
    if (VARTYPE=='psi'):     vartype=12
    if (VARTYPE=='grx'):     vartype=-1
    if (VARTYPE=='gry'):     vartype=-2
    
    im=720
    jm=410
    km=40
    #level=1
    DESCRIPTOR = array.array('i',[im, jm, 1, vartype, level])

    #print DESCRIPTOR

    descriptor = array.array('i')
    VAR = array.array('f')
    if os.path.exists(filename):
        f=open(filename, "rb")
    else:
        print "File doesn't exist"
        return

    #NDF=999999.9
    NDF='nan'

    while 1:
        dumi=f.read(4*5)
        descriptor=struct.unpack('>5i',dumi)
        #print descriptor        
        im=descriptor[0]
        jm=descriptor[1]
        km=descriptor[2]
        vartype=descriptor[3]
        level=descriptor[4]
        if not dumi: break
        
        dumi=f.read(4*im*jm)

        
        if ( (descriptor[3]==DESCRIPTOR[3]) and (descriptor[4]==DESCRIPTOR[4]) ):

            fmt='>'+str(im*jm)+'f'
            VAR=struct.unpack(fmt,dumi)
            VAR=np.reshape(VAR,[jm,im])
            VAR[VAR>9999]=NDF

            #print np.nanmax(VAR)
            #VAR[VAR==0]=NDF
            break
        
    f.close()

    return VAR
	
###########################################################
def write_odas2d(filename,VARTYPE,level,VAR,append):
    
    if (VARTYPE=='t'):
        vartype=1
        km=40
    if (VARTYPE=='s'):
        vartype=2
        km=40
    if (VARTYPE=='u'):
        vartype=3
        km=40
    if (VARTYPE=='v'):
        vartype=4
        km=40
    if (VARTYPE=='ssh'):
        vartype=5
        km=1
    if (VARTYPE=='aice'):
        vartype=6
        km=1
    if (VARTYPE=='hice'):
        vartype=7
        km=1
    if (VARTYPE=='tx'):
        vartype=8
        km=1
    if (VARTYPE=='ty'):
        vartype=9
        km=1
    if (VARTYPE=='rho'):
        vartype=10
        km=1
    if (VARTYPE=='ml'):
        vartype=11
        km=1
    if (VARTYPE=='psi'):
        vartype=12
        km=1        
    if (VARTYPE=='grx'):
        vartype=-1
        km=1
    if (VARTYPE=='gry'):
        vartype=-2
        km=1

    if (append==True):
        mode='ab'
    else:
        mode='wb'

    im = VAR.shape[1]
    jm = VAR.shape[0]
    
    VAR = np.reshape( VAR, (im*jm) )
    VAR[VAR=='nan']=0

    descriptor = array.array('i',[im, jm, km, vartype, level])
    fileobj = open(filename, mode=mode)
    for i in range(len(descriptor)):
        fileobj.write(struct.pack('>1i',descriptor[i]))
    for i in range(len(VAR)):
        fileobj.write(struct.pack('>1f',VAR[i]))
    fileobj.close()


###########################################################
def date_from_file(fname):
    
    try:
        year=string.atoi(fname[-15:-11])
        month=string.atoi(fname[-11:-9])
        day=string.atoi(fname[-9:-7])
        time_model=date(int(year),int(month),int(day))
        time_model=time_model.toordinal()
    except:
        try:
            year=string.atoi(fname[-14:-10])
            month=string.atoi(fname[-10:-8])
            day=string.atoi(fname[-8:-6])
            time_model=date(int(year),int(month),int(day))
            time_model=time_model.toordinal()
        except:	     
            year=''
            month=''
            day=''
            time_model=0

    return year, month, day, time_model	

