#! /usr/bin/env python
#

from netCDF4 import Dataset
from netCDF4 import MFDataset
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import array
import matplotlib.cm as cm
from mpl_toolkits.basemap import Basemap
from matplotlib.patches import Polygon

########################################################################################
def draw_box(axis,reg,color):
	#ones  = np.ones(len(Gtime))*1.5
	#Tz   = plt.plot(Gtime,ones,linestyle='--',color='k',marker='None',linewidth=1.0)

	if reg=='ADCP':
		lat1 = 165
		lat2 = 250
		#lon1 = -200
		#lon2 = -10
		lon1 = 200
		lon2 = 10
	lats = [lat1,lat2,lat2,lat1]
	lons = [lon1,lon1,lon2,lon2]	

    	xy = zip(lons,lats)
    	poly = Polygon( xy, facecolor='None', linewidth=2.0, edgecolor=color)
    	plt.gca().add_patch(poly)

	return(poly)

########################################################################################
def zlabels(zmax):
	zmax = int(zmax)
	zticklab = ['0','100','200','300','400','500','600','700','800','900','1000','1100','1200','1300','1400','1500','1600','1700','1800','1900','2000']
	zticknum = [0,-100,-200,-300,-400,-500,-600,-700,-800,-900,-1000,-1100,-1200,-1300,-1400,-1500,-1600,-1700,-1800,-1900,-2000]
	
	# 0-500
	if (zmax <= 500):
  		zticklab = ['0','100','200','300','400','500']
  		zticknum = [0,-100,-200,-300,-400,-500]

	# 0-2000
	if ( (zmax >500) & (zmax <= 2000) ):
  		zticklab = ['0','200','400','600','800','1000','1200','1400','1600','1800','2000']
  		zticknum = [0,-200,-400,-600,-800,-1000,-1200,-1400,-1600,-1800,-2000]

	# 0-4500
	if (zmax > 4000):
  		zticklab = ['0','500','1000','1500','2000','2500','3000','3500','4000','4500','5000']
  		zticknum = [0,-500, -1000, -1500, -2000, -2500, -3000, -3500, -4000, -4500, -5000]

	return(zticklab,zticknum)
########################################################################################
def xzlabels(reg):

	if reg=='glb':
		xticklab = ['30E','60E','90E','120E','150E','180E','150W','120W','90W','60W','30W', '0']
		xticknum = [ 30,   60,   90,   120,   150,   180,   210,   240,   270,  300,  330,   360]
		xlim     = [20,380]

	if reg=='pac':
		xticklab = ['100$^oE$','120$^oE$','140$^oE$','160$^oE$','180$^o$','160$^oW$','140$^oW$','120$^oW$','100$^oW$', '80$^oW$']
		xticknum = [ 100, 120,    140,   160,   180,   200,   220,   240,   260,    280]
		xlim     = [120,270]

	if reg=='atl':
		xticklab = ['60W','45W','30W', '15W', '0']
		xticknum = [ 300,  315,  330,   345,  360]
		xlim     = [310,370]

	if reg=='ind':
		xticklab = ['45E','50E','55E','60E','65E','70E','75E','80E','85E','90E','95E']
		xticknum = [45,    50,   55,   60,   65,   70,   75,   80,   85,   90,   95]
		xlim     = [45,95]

	return(xticklab,xticknum,xlim)
########################################################################################
def yzlabels(reg):

	if reg=='glb':
		xticklab = ['60S','40S','20S','Eq','20N','40N','60N']
		xticknum = [-60,  -40,  -20,   0,   20,   40,   60]

	if reg=='amoc':
		xticklab = ['30S','20S','10S','Eq','10N','20N','30N','40N','50N','60N']
		xticknum = [-30,  -20,  -10,   0,   10,   20,   30,   40,   50,   60]

	return(xticklab,xticknum)

