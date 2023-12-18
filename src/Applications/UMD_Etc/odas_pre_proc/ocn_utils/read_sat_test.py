#! /usr/bin/env python
from netCDF4 import Dataset
import numpy as np
import scipy.stats as stats
import struct
import glob
import os
import bitarray
import get_info
import misc
class dataclass:
	pass

###########################################################
def is_set(x, n):
    return x & 2**n != 0 

###########################################################
def dec2bin(x):

    #print 'x:',x,bin(x)[2:]
    xbit=bin(x)[2:]

    while (len(xbit)<16):
        xbit='0'+xbit
        #print xbit

    return xbit
###########################################################
def my_flag(F,num):

	f = is_set(F,num)

	if (f):
		return True
	else:
		return False

###########################################################

def L2_FLAGGED(F,num):

    f=dec2bin(F)
    
    #descr = ['microwave','land','ice']
    #fset  = False
    #seta = [f[0], f[1], f[2]];
    #print num

    if num<=13:          
    	if ( (f[num]=='1') ):
        	return True
    	else:
		return False

    if num==14:        
    	if ( (f[14]=='0') & (f[15]=='1') ):
        	return True
    	else:
		return False
    if num==15:           
    	if ( (f[14]=='1') & (f[15]=='0') ):
        	return True
    	else:
		return False
###########################################################
def day(year,doy,sat):
	data = dataclass()

	fdir  = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/L2_SST/NESDIS/'
	flist = sorted(list(glob.glob(fdir+sat+'/'+year+'/'+doy+'/20030101000000*nc')))
	#flist = sorted(list(glob.glob(fdir+sat+'/'+year+'/'+doy+'/*nc')))
 	fnum  = len(flist)

        im = 9000
        jm = 450

        data.sst  = np.ones((fnum,im,jm))*np.nan
        data.std  = np.ones((fnum,im,jm))*np.nan
        data.bias  = np.ones((fnum,im,jm))*np.nan
        data.lon   = np.ones((fnum,im,jm))*np.nan
        data.lat   = np.ones((fnum,im,jm))*np.nan
        cnt = 0
        for fname in flist:
		tmp    = 0.0
		nc     = Dataset(fname,'r') 
		print fname
		lon        = (nc.variables['lon'][:])*1.0
		lat        = (nc.variables['lat'][:])*1.0

		sst        = np.squeeze((nc.variables['sea_surface_temperature'][:])*1.0)
		sst_miss   = nc.variables['sea_surface_temperature']._FillValue
		sst_offset = nc.variables['sea_surface_temperature'].add_offset
		sst_scale  = nc.variables['sea_surface_temperature'].scale_factor

		std        = (np.squeeze(nc.variables['sses_standard_deviation'][:]))*1.0
		std_miss   = nc.variables['sses_standard_deviation']._FillValue
		std_offset = nc.variables['sses_standard_deviation'].add_offset
		std_scale  = nc.variables['sses_standard_deviation'].scale_factor

		bias    = (np.squeeze(nc.variables['sses_bias'][:]))*1.0
		bias_miss   = nc.variables['sses_bias']._FillValue
		bias_offset = nc.variables['sses_bias'].add_offset
		bias_scale  = nc.variables['sses_bias'].scale_factor

		qual       = np.squeeze((nc.variables['quality_level'][:])*1.0)
		nc.close()

		xx,yy = np.shape(sst)
		if xx>im:
			print 'increase im to ', xx
			exit(0)
		if yy>jm:
			print 'increase jm to ', yy
			exit(0)

		sst[sst==sst_miss]=np.nan
		sst  = sst*sst_scale
		#print 'sst',np.nanmin(sst), np.nanmax(sst)
	
		std[std==std_miss]=np.nan		
		std  = std*std_scale	
		std  = std+std_offset
		#print 'std',np.nanmin(std), np.nanmax(std)

		bias = bias*bias_scale	
		bias = bias+bias_offset	
		#print 'bias',np.nanmin(bias), np.nanmax(bias)
		#raw_input('stop')

		sst[qual<5]=np.nan
		std[qual<5]=np.nan
		bias[qual<5]=np.nan

		data.sst[cnt,0:xx,0:yy]  = sst
		data.std[cnt,0:xx,0:yy]  = std
		data.bias[cnt,0:xx,0:yy] = bias

		data.lon[cnt,0:xx,0:yy]  = lon
		data.lat[cnt,0:xx,0:yy]  = lat
		cnt = cnt+1

		print 'sst',np.nanmin(data.sst), np.nanmax(data.sst)
		print 'std',np.nanmin(data.std), np.nanmax(data.std)
		print 'bias',np.nanmin(data.bias), np.nanmax(data.bias)

	return (data)
###########################################################
def test(year,doy,sat):
	data = dataclass()

	fdir  = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/L2_SST/NESDIS/'
	flist = sorted(list(glob.glob(fdir+sat+'/'+year+'/'+doy+'/20030101000000*nc')))
	#flist = sorted(list(glob.glob(fdir+sat+'/'+year+'/'+doy+'/*nc')))
 	fnum  = len(flist)

        im = 9000
        jm = 450

        data.sstd1  = np.ones((fnum,im,jm))*np.nan
        data.sstd2  = np.ones((fnum,im,jm))*np.nan
        data.sst1  = np.ones((fnum,im,jm))*np.nan
        data.sst2  = np.ones((fnum,im,jm))*np.nan
        data.lon   = np.ones((fnum,im,jm))*np.nan
        data.lat   = np.ones((fnum,im,jm))*np.nan
        cnt = 0
        for fname in flist:
		tmp    = 0.0
		nc     = Dataset(fname,'r') 
		print fname
		lon    = (nc.variables['lon'][:])*1.0
		lat    = (nc.variables['lat'][:])*1.0
		tmp    = np.squeeze((nc.variables['sea_surface_temperature'][:])*1.0)
		miss   = nc.variables['sea_surface_temperature']._FillValue
		ffset  = nc.variables['sea_surface_temperature'].add_offset
		scale  = nc.variables['sea_surface_temperature'].scale_factor
		flag   = np.squeeze((nc.variables['l2p_flags'][:]))
		qual   = np.squeeze((nc.variables['quality_level'][:])*1.0)
		nc.close()
		xx,yy = np.shape(tmp)
		if xx>im:
			print 'increase im to ', xx
			exit(0)
		if yy>jm:
			print 'increase jm to ', yy
			exit(0)
		tmp[tmp==miss]=np.nan
		tmp  = tmp*scale	
		mask = np.ones((xx,yy))

        	for i in range(np.shape(tmp)[0]):
			for j in range(np.shape(tmp)[1]):				

				if my_flag(flag[i,j],0):
					mask[i,j] = np.nan
				if my_flag(flag[i,j],1):
					mask[i,j] = np.nan
				if my_flag(flag[i,j],2):
					mask[i,j] = np.nan
				if my_flag(flag[i,j],8):
					mask[i,j] = np.nan
				if my_flag(flag[i,j],9):
					mask[i,j] = np.nan
				if my_flag(flag[i,j],10):
					mask[i,j] = np.nan
				if my_flag(flag[i,j],11):
					mask[i,j] = np.nan
				if my_flag(flag[i,j],12):
					mask[i,j] = np.nan
				if my_flag(flag[i,j],13):
					mask[i,j] = np.nan

				f1 = is_set(flag[i,j],14)
				f2 = is_set(flag[i,j],15)
				# 01 Probably Clear
				if ((~f1) & (f2)):
					mask[i,j] = np.nan
				# 10 Cloudy
				if ((f1) & (~f2)):
					mask[i,j] = np.nan
				# 11 Clear-sky mask undefined
				if ((f1) & (f2)):
					mask[i,j] = np.nan
		
		data.sst1[cnt,0:xx,0:yy] = tmp*mask	# All Bit Flags

		tmp[qual<5]=np.nan
		data.sst2[cnt,0:xx,0:yy] = tmp	# qual flag

		for i in range(xx):
			for j in range(yy):
				#if (misc.isNaN(data.sst1[cnt,i,j])) & ~(misc.isNaN(data.sst2[cnt,i,j])):
				#	data.sstd[cnt,i,j] = data.sst2[cnt,i,j]
				#if ~(misc.isNaN(data.sst1[cnt,i,j])) & (misc.isNaN(data.sst2[cnt,i,j])):
				#	data.sstd[cnt,i,j] = data.sst1[cnt,i,j]

				if ~(misc.isNaN(data.sst1[cnt,i,j])) & (misc.isNaN(data.sst2[cnt,i,j])):
					data.sstd1[cnt,i,j] = data.sst1[cnt,i,j]
				if (misc.isNaN(data.sst1[cnt,i,j])) & ~(misc.isNaN(data.sst2[cnt,i,j])):
					data.sstd2[cnt,i,j] = data.sst2[cnt,i,j]

		data.lon[cnt,0:xx,0:yy]  = lon
		data.lat[cnt,0:xx,0:yy]  = lat
		cnt = cnt+1

		print cnt,np.nanmin(data.sst1), np.nanmax(data.sst1)
		print cnt,np.nanmin(data.sst2), np.nanmax(data.sst2)

		print cnt,np.nanmin(data.sstd1), np.nanmax(data.sstd1)
		print cnt,np.nanmin(data.sstd2), np.nanmax(data.sstd2)

	return (data)
###########################################################
def day_flag(year,doy,sat,F1,F2):
	data = dataclass()

	fdir  = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/L2_SST/NESDIS/'
	#flist = sorted(list(glob.glob(fdir+sat+'/'+year+'/'+doy+'/20030101000000*nc')))
	flist = sorted(list(glob.glob(fdir+sat+'/'+year+'/'+doy+'/*nc')))
 	fnum  = len(flist)

        im = 9000
        jm = 450

        data.sst1 = np.ones((fnum,im,jm))*np.nan
        data.sst2 = np.ones((fnum,im,jm))*np.nan
	if (F1>=15):
        	data.sst3 = np.ones((fnum,im,jm))*np.nan

        data.lon  = np.ones((fnum,im,jm))*np.nan
        data.lat  = np.ones((fnum,im,jm))*np.nan
        cnt = 0
        for fname in flist:
		tmp    = 0.0
		nc     = Dataset(fname,'r') 
		print fname
		lon    = (nc.variables['lon'][:])*1.0
		lat    = (nc.variables['lat'][:])*1.0
		tmp    = np.squeeze((nc.variables['sea_surface_temperature'][:])*1.0)
		miss   = nc.variables['sea_surface_temperature']._FillValue
		ffset  = nc.variables['sea_surface_temperature'].add_offset
		scale  = nc.variables['sea_surface_temperature'].scale_factor
		flag   = np.squeeze((nc.variables['l2p_flags'][:]))
		#qual   = np.squeeze((nc.variables['quality_level'][:])*1.0)
		nc.close()
		xx,yy = np.shape(tmp)
		if xx>im:
			print 'increase im to ', xx
			exit(0)
		if yy>jm:
			print 'increase jm to ', yy
			exit(0)
		tmp[tmp==miss]=np.nan
		tmp   = tmp*scale
	
		mask1 = np.ones((xx,yy))
		mask2 = np.ones((xx,yy))
		if (F1>=15):
			mask3 = np.ones((xx,yy))

        	for i in range(np.shape(tmp)[0]):
			for j in range(np.shape(tmp)[1]):				

				if (F1>=15):
					f1 = is_set(flag[i,j],14)
					f2 = is_set(flag[i,j],15)
					# 01 Probably Clear
					if ((~f1) & (f2)):
						mask1[i,j] = np.nan
					# 10 Cloudy
					if ((f1) & (~f2)):
						mask2[i,j] = np.nan
					# 11 Clear-sky mask undefined
					if ((f1) & (f2)):
						mask3[i,j] = np.nan
				else:
					if my_flag(flag[i,j],F1-1):
						mask1[i,j] = np.nan
					if my_flag(flag[i,j],F2-1):
						mask2[i,j] = np.nan


		data.sst1[cnt,0:xx,0:yy] = tmp*mask1
		data.sst2[cnt,0:xx,0:yy] = tmp*mask2
		if (F1>=14):
			data.sst3[cnt,0:xx,0:yy] = tmp*mask3

		data.lon[cnt,0:xx,0:yy]  = lon
		data.lat[cnt,0:xx,0:yy]  = lat
		cnt = cnt+1

		print cnt,np.nanmin(data.sst1), np.nanmax(data.sst1)
		print cnt,np.nanmin(data.sst2), np.nanmax(data.sst2)
		if (F1>=15):
			print cnt,np.nanmin(data.sst3), np.nanmax(data.sst3)

	return (data)

###########################################################
def std_day(year,doy,sat):
	data = dataclass()

	fdir  = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/L2_SST/NESDIS/'
	#flist = sorted(list(glob.glob(fdir+sat+'/'+year+'/'+doy+'/20030101000000*nc')))
	flist = sorted(list(glob.glob(fdir+sat+'/'+year+'/'+doy+'/*nc')))
 	fnum  = len(flist)

        im = 9000
        jm = 450
        data.sst  = np.ones((fnum,im,jm))*np.nan
        data.lon  = np.ones((fnum,im,jm))*np.nan
        data.lat  = np.ones((fnum,im,jm))*np.nan
        cnt = 0
        for fname in flist:
		nc     = Dataset(fname,'r') 
		print fname
		lon    = (nc.variables['lon'][:])*1.0
		lat    = (nc.variables['lat'][:])*1.0
		tmp    = (np.squeeze(nc.variables['sses_standard_deviation'][:]))*1.0
		miss   = nc.variables['sses_standard_deviation']._FillValue
		offset = nc.variables['sses_standard_deviation'].add_offset
		scale  = nc.variables['sses_standard_deviation'].scale_factor
		nc.close()
		xx,yy = np.shape(tmp)
		if xx>im:
			print 'increase im to ', xx
			exit(0)
		if yy>jm:
			print 'increase jm to ', yy
			exit(0)
		#print 'sst  ', np.nanmin(tmp), np.nanmax(tmp)
		tmp[tmp==miss]=np.nan
		#print 'sst  ', np.nanmin(tmp), np.nanmax(tmp)
		tmp = tmp*scale	
		#print 'sst  ', np.nanmin(tmp), np.nanmax(tmp)
		tmp = tmp+offset	
		#print 'sst  ', np.nanmin(tmp), np.nanmax(tmp)
		#raw_input('stop')

		data.sst[cnt,0:xx,0:yy]  = tmp
		data.lon[cnt,0:xx,0:yy]  = lon
		data.lat[cnt,0:xx,0:yy]  = lat
		cnt = cnt+1
		print 'std  ', np.nanmin(data.sst), np.nanmax(data.sst)

	return (data)
###########################################################
def bias_day(year,doy,sat):
	data = dataclass()

	fdir  = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/L2_SST/NESDIS/'
	flist = sorted(list(glob.glob(fdir+sat+'/'+year+'/'+doy+'/20030101000000*nc')))
	#flist = sorted(list(glob.glob(fdir+sat+'/'+year+'/'+doy+'/*nc')))
 	fnum  = len(flist)

        im = 9000
        jm = 450
        data.sst  = np.ones((fnum,im,jm))*np.nan
        data.lon  = np.ones((fnum,im,jm))*np.nan
        data.lat  = np.ones((fnum,im,jm))*np.nan
        cnt = 0
        for fname in flist:
		nc     = Dataset(fname,'r') 
		print fname
		lon    = (nc.variables['lon'][:])*1.0
		lat    = (nc.variables['lat'][:])*1.0
		tmp    = (np.squeeze(nc.variables['sses_bias'][:]))*1.0
		miss   = nc.variables['sses_bias']._FillValue
		offset = nc.variables['sses_bias'].add_offset
		scale  = nc.variables['sses_bias'].scale_factor
		nc.close()
		xx,yy = np.shape(tmp)
		if xx>im:
			print 'increase im to ', xx
			exit(0)
		if yy>jm:
			print 'increase jm to ', yy
			exit(0)

		#for i in range(xx):
		#	for j in range(yy):
		#		if tmp[i,j]>0:
		#			print i, j,tmp[i,j]
		#			raw_input('stop')
		#print  dec2bin(tmp[0,0])
		#print  dec2bin(tmp[2,249])

		#print 'sst  ', np.nanmin(tmp), np.nanmax(tmp), tmp[2,249], dec2bin(tmp[2,249])
		tmp = tmp*scale	
		#print 'sst  ', np.nanmin(tmp), np.nanmax(tmp), tmp[2,249], dec2bin(tmp[2,249])
		tmp = tmp+offset	
		print '   ',np.nanmin(tmp), np.nanmax(tmp), tmp[2,249]#, dec2bin(tmp[2,249])


		#raw_input('stop')

		data.sst[cnt,0:xx,0:yy]  = tmp
		data.lon[cnt,0:xx,0:yy]  = lon
		data.lat[cnt,0:xx,0:yy]  = lat
		cnt = cnt+1
		print 'bias  ', np.nanmin(data.sst), np.nanmax(data.sst)

	return (data)

