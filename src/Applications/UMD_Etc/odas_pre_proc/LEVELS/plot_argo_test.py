#! /usr/bin/env python
#
# plot_argo_test.py year month

import numpy as np
import matplotlib
#matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from netCDF4 import Dataset
from netCDF4 import MFDataset
from numpy import *
from mpl_toolkits.basemap import Basemap
from matplotlib.font_manager import FontProperties
import glob
import datetime
import string
import time
import sys
import math
import pickle
from calendar import monthrange
from Tkinter import *
import array
import scipy.stats as stats
from matplotlib.ticker import MultipleLocator, FormatStrFormatter
from matplotlib.dates import YearLocator, MonthLocator, DayLocator, DateFormatter
sys.path.append('/gpfsm/dnb42/projects/p17/rkovach/geos5/OceanPost/ocn_utils')
import read_obs
class dataclass:
	pass
from matplotlib.ticker import MultipleLocator, FormatStrFormatter

####################################################
def datetime2date(datetime):
        # yyyymmddhh to yyyymmdd
        year  = datetime/1000000
        month = (datetime-year*1000000)/10000
        day   = (datetime-year*1000000-month*10000)/100
        date  = year*10000 + month*100 + day
        return date
####################################################
def date2ymd(date):
        # yyyymmdd to y, m, d
        y = ((date)/10000)
        m = ((date-y*10000)/100)
        d = ((date-y*10000-m*100))
        return y,m,d

####################################################
def get_data_prf_month(fdir,inst,svar,syear,smon):
	data      = dataclass()

	if svar=='T':
		lvar = 'TEMP'
	if svar=='S':
		lvar = 'SALT'
	odasgrp   = Dataset(fdir+svar+'_'+inst+'_'+syear+'.nc', 'r', format='NETCDF4')
	date_time = odasgrp.variables['DATE_TIME'][:]	
	qc_prf    = odasgrp.variables['QC_PRF'][:]
	tmp       = odasgrp.variables[lvar][:,:]
	depth     = odasgrp.variables['DEPTH'][:,:]
	qc_lev    = odasgrp.variables['QC_LEV'][:,:]
	odasgrp.close()
	tmp[tmp>999]=np.nan
	depth[depth>999]=np.nan
	tm,zm  = np.shape(tmp)
	idate1 = int(syear+smon+'0100')
	idate2 = int(syear+smon+'3124')
	good   = ((date_time>=idate1) & (date_time<=idate2) & (qc_prf==1))
	I    = np.where(good)
	tmp = tmp[I[0],:]
	depth = depth[I[0],:]
	qc_lev = qc_lev[I[0],:]	
	#print '   ',svar,' QC LEV ',np.nanmin(qc_lev), np.nanmax(qc_lev)
	data.var = tmp
	data.depth = depth

	return data
####################################################
def get_data_month(fdir,inst,svar,syear,smon):
	data      = dataclass()

	mr        = monthrange(int(syear),int(smon))
	acnt      = np.zeros([mr[1],1])
	adate     = range(mr[1])
	odasgrp   = Dataset(fdir+svar+'_'+inst+'_'+syear+'.nc', 'r', format='NETCDF4')
	date_time = odasgrp.variables['DATE_TIME'][:]	
	qc_prf    = odasgrp.variables['QC_PRF'][:]	
	odasgrp.close()

	date              = datetime2date(date_time)
	year0,month0,day0 = date2ymd(date)
	for j in range(mr[1]):
		iday = j+1
		sday = str(iday)
		if iday<=9:
			sday = '0'+str(iday)
		idate1 = int(syear+smon+sday+'00')
		idate2 = int(syear+smon+sday+'24')

		good = ((date_time>=idate1) & (date_time<=idate2) & (qc_prf==1))
		I    = np.where(good)
		Ne   = len(I[0])
		adate[j] = datetime.date(int(syear),int(smon),int(sday))
		acnt[j]  = Ne
	cnt = 0
	for j in range(mr[1]):
		if acnt[j] >= 0:
			cnt = cnt+1

	data.obscnt = acnt
	data.date   = adate
	data.cnt    = cnt
	#print '   Monthly ',svar,' ',data.date[0],data.date[-1],' ',max(data.obscnt)

	return data
####################################################
def get_data_year(fdir,inst,svar,syear):
	data      = dataclass()

	acnt      = np.zeros([12,1])
	adate     = range(12)

	odasgrp   = Dataset(fdir+svar+'_'+inst+'_'+syear+'.nc', 'r', format='NETCDF4')
	date_time = odasgrp.variables['DATE_TIME'][:]	
	qc_prf    = odasgrp.variables['QC_PRF'][:]
	lon       = odasgrp.variables['LON'][:]
	lat	  = odasgrp.variables['LAT'][:]
	odasgrp.close()
	cntyear = len(lat)

	date              = datetime2date(date_time)
	year0,month0,day0 = date2ymd(date)
	for j in range(12):
		imon = j+1
		smon = str(imon)
		if imon<=9:
			smon = '0'+str(imon)
		idate1 = int(syear+smon+'0100')
		idate2 = int(syear+smon+'3124')
		good = ((date_time>=idate1) & (date_time<=idate2) & (qc_prf==1))
		I    = np.where(good)
		Ne   = len(I[0])
		adate[j] = datetime.date(int(syear),j+1,1)
		acnt[j]  = Ne

	cnt = 0
	for j in range(12):
		if acnt[j] >= 0:
			cnt = cnt+1

	data.obscnt  = acnt
	data.date    = adate
	data.cnt     = cnt
	data.lon     = lon
	data.lat     = lat
	data.cntyear = cntyear
	#print '   Yearly ',svar,' ',data.date[0],data.date[-1],' ',max(data.obscnt)

	return data

####################################################
def get_data_all(inst,svar):
	data      = dataclass()
        f = open(inst+'_'+svar+'_all.dat', 'r')
        data.date = pickle.load(f)
        data.obscnt  = pickle.load(f)
        f.close()
	return data

####################################################
def save_data_all(fdir,inst,svar):
	data      = dataclass()

	now = datetime.datetime.now()
	currentDay   = now.day
	currentMonth = now.month
	currentYear  = now.year
	rt = 12-currentMonth
	tempdir = fdir+svar+'_'+inst+'_201*.nc'
	flist   = sorted(list(glob.glob(tempdir)))	
	jm      = np.squeeze(np.shape(flist))
	tmp     = np.zeros([jm,12])
	date1   = range((jm*12)-rt)
	year1   = np.zeros([jm,1])
	obs1    = np.zeros([(jm*12)-rt,1])
	iyear   = 0
	for fname in flist:  
		syear = fname[-7:-3]
		year1[iyear] = int(syear)
		odasgrp   = Dataset(fname, 'r', format='NETCDF4')
		date_time =  odasgrp.variables['DATE_TIME'][:]	
		qc_prf    =  odasgrp.variables['QC_PRF'][:]	
		odasgrp.close()
		date           = datetime2date(date_time)
		year,month,day = date2ymd(date)
		for j in range(12):
			imon = j+1
			smon = str(imon)
			if imon<=9:
				smon = '0'+str(imon)
			idate1 = int(syear+smon+'0100')
			idate2 = int(syear+smon+'3124')
			good = ((date_time>=idate1) & (date_time<=idate2) & (qc_prf==1))
			I    = np.where(good)
			Ne   = len(I[0])
			tmp[iyear,j]  = Ne
		iyear = iyear+1
	cnt = 0
	for i in range(jm):
		for j in range(12):
			if tmp[i,j] > 0:
				date1[cnt] = datetime.date(year1[i],j+1,1)
				obs1[cnt]  = tmp[i,j]
				cnt = cnt+1

	data.date    = date1[0:cnt-1]
	data.obscnt  = obs1[0:cnt-1]
	#print np.min(data.obscnt), np.max(data.obscnt)
        f = open(inst+'_'+svar+'_all.dat', 'w')
        pickle.dump(data.date, f)
        pickle.dump(data.obscnt, f)
        f.close()

	return data
####################################################
def get_data_loc_month(fdir,inst,svar,year,month):
	data      = dataclass()

	idate1    = int(year+month+'01')
	idate2    = int(year+month+'31')
	odasgrp   = Dataset(fdir+svar+'_'+inst+'_'+syear+'.nc', 'r', format='NETCDF4')
	lon       = odasgrp.variables['LON'][:]
	lat       = odasgrp.variables['LAT'][:]
	qc_prf    =  odasgrp.variables['QC_PRF'][:]	
	date_time =  odasgrp.variables['DATE_TIME'][:]	
	odasgrp.close()
	obsdate   = datetime2date(date_time)
	good      = ( (obsdate>=idate1) & (obsdate<=idate2) & (qc_prf==1) )
	I         = np.where(good)

	data.lon	= lon[I]
	data.lat	= lat[I]
	data.obscnt	= str(len(I[0]))

	return data
################################################################################################################

################################################################################################################
def plot_obs_prf_month(inst,data1,data2,syear,smonth,pos_axis1,pos_axis2): 
	ax     = plt.axes(pos_ax1) 
	zmax   = max(np.nanmax(data1.depth),np.nanmax(data2.depth))
	tmin   = int(round(np.nanmin(data1.var)))
	tmax   = int(round(np.nanmax(data1.var)))
	tmean  = int(round(stats.nanmean(stats.nanmean(data1.var))))
	sdate  = syear+smonth
	plt.grid(True)
	for t in range(len(data1.var)):
		h1=plt.plot(data1.var[t,:],data1.depth[t,:],'.k-')
	#plt.ylim(0,zmax)
	plt.gca().invert_yaxis()
	plt.ylabel('Depth (m)',size=10,fontweight='bold')
	plt.xticks(rotation=0,size=10) 
	plt.yticks(rotation=0,size=10) 
	plt.xlabel('Temperature: '+str(len(data1.var))+' obs',size=10,fontweight='bold',rotation=0)
	if ( (tmin<-5) | (tmin>=35) ):
		ax.text(6,50,'T ERROR',size=14,fontweight='bold',color='r')
		write_error("ARGO T Profile Value Error",sdate)
	smin   = int(round(np.nanmin(data2.var)))
	smax   = int(round(np.nanmax(data2.var)))
	smean  = int(round(stats.nanmean(stats.nanmean(data2.var))))
	ax     = plt.axes(pos_ax2)  
	plt.grid(True)
	for t in range(len(data2.var)):
		h1=plt.plot(data2.var[t,:],data2.depth[t,:],'.m-')
	#plt.ylim(0,zmax)
	plt.gca().invert_yaxis()
	plt.xticks(rotation=0,size=10) 
	plt.yticks(rotation=0,size=10) 
	plt.xlabel('Salinity: '+str(len(data2.var))+' obs',size=10,fontweight='bold',rotation=0)
	ax.set_yticklabels([])
	if ( (smin<15) | (smin>=40) ):
		ax.text(33,50,'S ERROR',size=14,fontweight='bold',color='r')
		write_error("ARGO S Profile Value Error",sdate)
	
	#print '   Profiles: T: ',tmin, tmean, tmax, 'S: ',smin, smean, smax

	return ax

####################################################
def plot_obs_loc_month(inst,data1,data2,syear,smonth,pos_ax): 
	ax     = plt.axes(pos_ax)  
	plt.title('Monthly Observation Counts: '+syear+' '+smonth,size=12,fontweight='bold',rotation=0)
	m = Basemap(projection='mill', llcrnrlon=20, llcrnrlat=-78, urcrnrlon=380,urcrnrlat=85,resolution='l')  
	m.drawmeridians(np.arange(20,380,60),labels=[0,0,0,1],fontsize=10)
	m.drawparallels(np.arange(-80,80,20),labels=[1,0,0,0],fontsize=10)
	m.drawcoastlines()
	m.fillcontinents(color='grey')

	for shift in [0, 360]:

		x, y = m(data1.lon+shift,data1.lat)
		ht = plt.plot(x,y,'ko',ms=3, markeredgecolor='k',markerfacecolor='none')

		x, y = m(data2.lon+shift,data2.lat)
		hs = plt.plot(x,y,'mo',ms=1, markeredgecolor='m',markerfacecolor='m')

	fontP = FontProperties()
	fontP.set_size('small')
	leg=plt.legend([ht[0],hs[0]],(str(data1.obscnt)+' ARGO T',str(data2.obscnt)+' ARGO S'),ncol=3,bbox_to_anchor=(0.85,-0.05),prop = fontP)
	
	return ax
####################################################
def plot_obs_cnt_month(inst,data1,data2,mincnt,maxcnt,syear,smonth,tmean,smean,pos_ax):
	ax     = plt.axes(pos_ax) 
	plt.title('Daily Observation Counts: '+syear+' '+smonth,size=12,fontweight='bold',rotation=0)
	h1 = plt.bar(data1.date,data1.obscnt,color='k',width=0.4,edgecolor='none',label='ARGO T')
	h2 = plt.bar(data2.date,data2.obscnt,color='m',width=0.2,edgecolor='none',label='ARGO S')
	tmean = (tmean/data1.cnt)*np.ones([len(data1.obscnt)])
	smean = (smean/data2.cnt)*np.ones([len(data2.obscnt)])
	sdate = syear+smonth
	tlim = tmean[0]-tmean[0]*0.25
	slim = smean[0]-smean[0]*0.25
	sdate = syear+smonth	
	iyear=int(syear)
	imonth=int(smonth)
	now = datetime.datetime.now()
	cmr = monthrange(iyear,imonth)
	last_dom = cmr[1]
	if ( (iyear==now.year) & (imonth==now.month) ):
		last_dom = now.day
		tlim = tmean[0]-tmean[0]*0.30
		slim = smean[0]-smean[0]*0.30
	#print 'Daily Means ',tmean[0], np.max(data1.obscnt), tlim, smean[0], np.max(data2.obscnt), slim
	if np.max(data1.obscnt) < tlim:
		write_error("ARGO T Daily Max Obs Cnt Low",sdate)
	if np.max(data2.obscnt) < slim:
		write_error("ARGO S Daily Max Obs Cnt Low",sdate)
	for t in range(last_dom):
		if data1.obscnt[t] < tlim:
			write_error("ARGO T Daily Obs Cnt Low",str(data1.date[t]))
		if data2.obscnt[t] < slim:
			write_error("ARGO S Daily Obs Cnt Low",str(data2.date[t]))
	plt.plot_date(data1.date,tmean,'k-',marker='None',linewidth=1.5)
	plt.plot_date(data2.date,smean,'m-',marker='None',linewidth=1.5)
	plt.grid(True)
	months    = DayLocator() 
	monthsFmt = DateFormatter('%d')
	ax.xaxis.set_major_locator(months)
	ax.xaxis.set_major_formatter(monthsFmt)
	plt.ylim(mincnt,maxcnt)
	for tick in ax.yaxis.get_major_ticks():
                tick.label.set_fontsize(8)
	for tick in ax.xaxis.get_major_ticks():
                tick.label.set_fontsize(8)
	fontP = FontProperties()
	fontP.set_size('small')
	leg=ax.legend(ncol=2,loc=0,prop = fontP)
	return ax

####################################################
def plot_obs_cnt_year(inst,data1,data2,mincnt,maxcnt,tmean,smean,syear,imonth,pos_ax): 
	ax     = plt.axes(pos_ax) 
	plt.title('Monthly Observation Counts: '+syear,size=12,fontweight='bold',rotation=0)
	h1 = plt.bar(data1.date,data1.obscnt,color='k',width=6,edgecolor='none',label='ARGO T')
	h2 = plt.bar(data2.date,data2.obscnt,color='m',width=3,edgecolor='none',label='ARGO S')
	tmean  = tmean*np.ones([len(data1.obscnt)])
	smean  = smean*np.ones([len(data2.obscnt)])
	tlim = tmean[0]-tmean[0]*0.25
	slim = smean[0]-smean[0]*0.25	
	sdate = syear+smonth
	iyear=int(syear)
	imonth=int(smonth)
	now = datetime.datetime.now()
	if ( (iyear==now.year) & (imonth==now.month) ):
		tlim = tmean[0]-tmean[0]*(31-now.day)/10
		slim = smean[0]-smean[0]*(31-now.day)/10
	#print 'Monthly Means ',data1.obscnt[imonth-1], tlim, data2.obscnt[imonth-1], slim
	if data1.obscnt[imonth-1] < tlim:
		write_error("ARGO T Monthly Obs Cnt Low",sdate)
	if data2.obscnt[imonth-1] < slim:
		write_error("ARGO S Monthly Obs Cnt Low",sdate)
	plt.plot_date(data1.date,tmean,'k--',marker='None',linewidth=1)
	plt.plot_date(data2.date,smean,'m--',marker='None',linewidth=1)
	plt.grid(True)
	months   = MonthLocator() 
	monthsFmt = DateFormatter('%b')
	ax.xaxis.set_major_locator(months)
	ax.xaxis.set_major_formatter(monthsFmt)
	plt.ylim(mincnt,maxcnt)
	plt.xticks(rotation=30,size=10)
	plt.yticks(rotation=0,size=10)
	fontP = FontProperties()
	fontP.set_size('small')
	leg=ax.legend(ncol=2,loc=0,prop = fontP)
	return ax

####################################################
def plot_obs_cnt_all(inst,data1,data2,mincnt,maxcnt,syear,smonth,pos_ax): 
	ax     = plt.axes(pos_ax)  
	plt.title('Monthly Observation Counts',size=12,fontweight='bold',rotation=0)
	tstart = 36 # 2013
	data1.date   = data1.date[tstart:]
	data2.date   = data2.date[tstart:]
	data1.obscnt = data1.obscnt[tstart:]
	data2.obscnt = data2.obscnt[tstart:]
	ht     = plt.plot_date(data1.date,data1.obscnt,'k-',marker='None',linewidth=1.5,label='ARGO T')
	hs     = plt.plot_date(data2.date,data2.obscnt,'m-',marker='None',linewidth=1.5,label='ARGO S')
	tmean  = stats.nanmean(data1.obscnt)*np.ones([len(data1.obscnt)])
	smean  = stats.nanmean(data2.obscnt)*np.ones([len(data2.obscnt)])
	#print 'Mean Counts for full period ',tmean[0],smean[0]
	plt.plot_date(data1.date,tmean,'k--',marker='None',linewidth=1)
	plt.plot_date(data2.date,smean,'m--',marker='None',linewidth=1)
	Gtime = datetime.date(int(syear),int(smonth),int(1))
	plt.axvline(x=Gtime,linewidth=1.5)
	plt.grid(True)
	years    = YearLocator() 
	yearsFmt = DateFormatter('%Y')
	ax.xaxis.set_major_locator(years)
	ax.xaxis.set_major_formatter(yearsFmt)	
	plt.ylim(mincnt,maxcnt)
	plt.xticks(rotation=30,size=10)
	plt.yticks(rotation=0,size=10)
	fontP = FontProperties()
	fontP.set_size('small')
	leg=ax.legend(ncol=2,loc=0,prop = fontP)

	return ax

def write_error(message, date):
	f = open("error.txt","a")
	f.write("\n")
	f.write(message+': '+date)
	f.close()
	return

###########################################################################################################
# Plots to check
# 201301-just ramping up
# 2016-all
# 201702-Sprf
now = datetime.datetime.now()

syear  = sys.argv[1]	# 0, 2005-2018
smonth = sys.argv[2]	# 0, 01-12

iyear  = int(syear)
imonth = int(smonth)
inst   = 'ARGO'
instlong = 'ARGO'
#fdir   = '/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OBS/'+inst+'_6.0/'
#fdir   = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/raw/ARGO/PROC/'
fdir   = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/assim/ARGO/V3.1/'


#inst   = 'TAO'
#instlong = 'TAO'
#fdir   = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/assim/TAO/V3.1/'
#fdir   = '/gpfsm/dnb42/projects/p17/production/GEOS5odas-5.00/RC/OBS/REAL_TIME/'
#print inst,' ',iyear,' ',imonth

# Yearly and Monthly
tmean = 10800
smean = 9400

fig   = plt.figure(num=None, figsize=(12,10), facecolor='w')  

# Profile plot my month
dataT5  = get_data_prf_month(fdir,inst,'T',syear,smonth)	
dataS5  = get_data_prf_month(fdir,inst,'S',syear,smonth)	
pos_ax1 = [0.1, 0.09, 0.3, 0.8]
pos_ax2 = [0.5, 0.09, 0.3, 0.8]
ax      = plot_obs_prf_month(inst,dataT5,dataS5,syear,smonth,pos_ax1,pos_ax2)
#fig.text(0.16,0.90,'Argo-50 Levels',size=12,fontweight='bold')

plt.savefig(instlong+'_all_'+syear+smonth+'.png')
plt.show()

	
