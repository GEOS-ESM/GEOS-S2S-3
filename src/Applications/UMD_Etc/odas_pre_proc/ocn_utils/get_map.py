#! /usr/bin/env python
#

import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.cm as cm
from mpl_toolkits.basemap import Basemap
from matplotlib.patches import Polygon
import get_map

########################################################################################
def draw_reg(map,reg,ind):

	lats = [ind.lat[0],ind.lat[1],ind.lat[1],ind.lat[0]]

	if ind.rot==0:
		lons = [ind.lon[0],ind.lon[0],ind.lon[1],ind.lon[1]]	
   		x, y = map( lons, lats )
    		xy = zip(x,y)
    		poly = Polygon(xy, facecolor='c', linewidth=2.0, edgecolor='c')
    		plt.gca().add_patch(poly)

	if ind.rot==1:
		lons = [ind.lon[0],ind.lon[0],360,360]	
   	x, y = map( lons, lats )
    	xy = zip(x,y)
    	poly = Polygon(xy, facecolor='c', linewidth=2.0, edgecolor='c')
    	plt.gca().add_patch(poly)

	if ind.rot==1:
		lons = [360,360,360+ind.lon[1],360+ind.lon[1]]
   	x, y = map( lons, lats )
    	xy = zip(x,y)
    	poly = Polygon(xy, facecolor='c', linewidth=2.0, edgecolor='c')
    	plt.gca().add_patch(poly)

	if reg=='atl2':
		lons = [360,360,360+ind.lon[1],360+ind.lon[1]]
   		x, y = map( lons, lats )
    		xy = zip(x,y)
    		poly = Polygon(xy, facecolor='c', linewidth=2.0, edgecolor='c')
    		plt.gca().add_patch(poly)

	if ind.name[0]=='g':
		lons = [ind.lon[1],ind.lon[1],380,380]	
   		x, y = map( lons, lats )
    		xy = zip(x,y)
    		poly = Polygon(xy, facecolor='c', linewidth=2.0, edgecolor='c')
    		plt.gca().add_patch(poly)

	return(map)


########################################################################################
def draw_map(reg,dx,dy):	

	proj,lon1,lon2,lat1,lat2 = get_map.reg(reg)
	if ((proj=='npstere') | (proj=='spstere')):
		map = Basemap(projection=proj,lon_0=lon1,boundinglat=lat1)
		#map = Basemap(projection=proj,lon_0=-45,boundinglat=65)
	else:
		map = Basemap(projection=proj, llcrnrlon=lon1, llcrnrlat=lat1, urcrnrlon=lon2, urcrnrlat=lat2)

	if reg=='glb':
		map.drawmeridians(np.arange(80,360,60),labels=[0,0,0,dx])
		map.drawparallels(np.arange(-90,90,20),labels=[dy,0,0,0])
	else:
		map.drawmeridians(np.arange(20,380,40),labels=[0,0,0,dx])
		map.drawparallels(np.arange(-90,90,20),labels=[dy,0,0,0])
	if ( (reg=='npole') | (reg=='spole') ):
		#map.drawmeridians(np.arange(20,380,20),labels=[dx,0,0,dx])
		#map.drawparallels(np.arange(-90,90,10),labels=[dy,0,0,0])
    		map.drawparallels(np.arange(-90.,120.,15.)) 
    		map.drawmeridians(np.arange(0.,420.,30.)) 

	if ( (reg=='N12') | (reg=='N1') ):
		map.drawmeridians(np.arange(20,380,5),labels=[0,0,0,dx])
		map.drawparallels(np.arange(-90,90,5),labels=[dy,0,0,0])

	map.drawcoastlines()
	map.drawmapboundary()
	map.fillcontinents()

	return(map)

########################################################################################
def draw_box(map,reg,color):

	if reg=='N3':
		lat1 = -5
		lat2 = 5
		lon1 = 210
		lon2 = 270
	if reg=='N34':
		lat1 = -5
		lat2 = 5
		lon1 = 190
		lon2 = 230
	if reg=='N4':
		lat1 = -5
		lat2 = 5
		lon1 = 160
		lon2 = 210
	if reg=='N12':
		lat1 = -10
		lat2 = 0
		lon1 = 270
		lon2 = 280
	if reg=='N1':
		lat1 = -5
		lat2 = 0
		lon1 = 270
		lon2 = 280
	if reg=='N2':
		lat1 = -10
		lat2 = -5
		lon1 = 270
		lon2 = 280

	lats = [lat1,lat2,lat2,lat1]
	lons = [lon1,lon1,lon2,lon2]	

   	x, y = map( lons, lats )
    	xy = zip(x,y)
    	poly = Polygon( xy, facecolor='None', linewidth=2.0, edgecolor=color)
    	plt.gca().add_patch(poly)

	return()

########################################################################################
def plot_obs(map,data,levels,obsize,mark):	
	if (data.nobs>0):
		for shift in [0, 360]:
			x, y = map(data.lon+shift,data.lat)	
			map.scatter(x,y,s=obsize,c=data.var,marker=mark,vmin=levels[0],vmax=levels[-1],cmap=cm.jet,linewidths=.1,edgecolor='k')

	return()
########################################################################################
def reg(reg):	
	proj='mill'
	lon1 = 0
	lat1 = 0
	lon2 = 0
	lat2 = 0

        # Currents
	if reg=='gulf':
		lon1=260
		lat1=10
		lon2=320
		lat2=50
	if reg=='kuro':
		lon1=110
		lat1=20
		lon2=180
		lat2=50
	if reg=='loop':
		lon1=260
		lat1=10
		lon2=290
		lat2=35
	if reg=='agul':
		lon1=0
		lat1=-50
		lon2=50
		lat2=-10
	if reg=='amazon':
		lon1=290
		lat1=-20
		lon2=370
		lat2=20
	if reg=='brazil':
		lon1=295
		lat1=-45
		lon2=350
		lat2=10
	if reg=='nwbrazil':
		lon1=250
		lat1=-20
		lon2=320
		lat2=20
	if reg=='aust':
		lon1=135
		lat1=-45
		lon2=182
		lat2=-20
	if reg=='cirpolar':
		lon1=20
		lat1=-65
		lon2=360
		lat2=-30
	if reg=='indo':
		lon1=80
		lat1=-15
		lon2=140
		lat2=30

        # Global Regions
	if reg=='glb':
		lon1=20
		lat1=-80
		lon2=360
		lat2=90
	if reg=='g30':
		lon1=20
		lat1=-30
		lon2=380
		lat2=30
	if reg=='g15':
		lon1=20
		lat1=-15
		lon2=380
		lat2=15
	if reg=='glbs':
		lon1=20
		lat1=-80
		lon2=380
		lat2=0
	# Southern Oceans
	if ( (reg=='antarctic') | (reg=='spole') ):
		proj='spstere'
		lon1 = -45
		lat1 = -55
	if reg=='nzeal':
		lon1=145
		lat1=-60
		lon2=190
		lat2=-30
	if reg=='weddel':
		lon1=290
		lat1=-80
		lon2=360
		lat2=-50
	if reg=='southern':
		proj='spstere'
		lon1 = -45
		lat1 = -35

	# Northern Oceans
	if ( (reg=='arctic') | (reg=='npole') ):
		proj='npstere'
		lon1 = -45
		lat1 = 55
	if ( (reg=='north')):
		proj='npstere'
		lon1 = -45
		lat1 = 50
	if reg=='npac':
		lon1=170
		lat1=50
		lon2=210
		lat2=70
	if reg=='pacnw':
		lon1=210
		lat1=30
		lon2=240
		lat2=60
	if reg=='nseas':
		lon1=0
		lat1=60
		lon2=60
		lat2=85
	if reg=='kara':
		lon1=50
		lat1=65
		lon2=140
		lat2=90
	if reg=='karaz':
		lon1=60
		lat1=65
		lon2=70
		lat2=75

        # Indian Ocean
	if reg=='ind':
		lon1=30
		lat1=-35
		lon2=130
		lat2=30
	if reg=='eqind':
		lon1=38
		lat1=-20
		lon2=120
		lat2=20
	if reg=='sind':
		lon1=20
		lat1=-70
		lon2=140
		lat2=-30
	if reg=='indo':
		lon1=90
		lat1=-20
		lon2=150
		lat2=30

        # Atlantic Ocean
	if reg=='atl':
		lon1=280
		lat1=-45
		lon2=380
		lat2=45
	if reg=='eqatl':
		lon1=310
		lat1=-20
		lon2=380
		lat2=20
	if reg=='na':
		lon1=290
		lat1=45
		lon2=360
		lat2=75
	if reg=='ne':
		lon1=290
		lat1=30
		lon2=340
		lat2=65
	if reg=='natl':
		lon1=280
		lat1=30
		lon2=360
		lat2=50

        # Pacific Ocean
	if reg=='pac':
		lon1=110
		lat1=-45
		lon2=290
		lat2=45
	if reg=='eqpac':
		lon1=110
		lat1=-20
		lon2=290
		lat2=20
	if reg=='trppac':
		lon1=110
		lat1=-35
		lon2=280
		lat2=20
	if reg=='N12':
		lon1=265
		lat1=-15
		lon2=285
		lat2=5
	if reg=='eastpac':
		lon1=235
		lat1=-30
		lon2=290
		lat2=20
	if reg=='ninos':
		lon1=160
		lat1=-20
		lon2=285
		lat2=20
	if reg=='ken':
		lon1=120
		lat1=-30
		lon2=290
		lat2=30
	if reg=='tp':
		lon1=110
		lat1=-30
		lon2=290
		lat2=30
	if reg=='tpmv':
		lon1=120
		lat1=-30
		lon2=290
		lat2=30

        # South America
	if reg=='peru20':
		lon1=210
		lat1=-20
		lon2=295
		lat2=20
	if reg=='sa':
		lon1=260
		lat1=-55
		lon2=300
		lat2=20
	if reg=='peru':
		lon1=160
		lat1=-35
		lon2=295
		lat2=35
	if reg=='arg':
		lon1=280
		lat1=-60
		lon2=330
		lat2=-10

        # Africa
	if reg=='wafr':
		lon1=0
		lat1=-20
		lon2=25
		lat2=10
	if reg=='safr':
		lon1=0
		lat1=-55
		lon2=80
		lat2=-30
	if reg=='afr':
		lon1=0
		lat1=-40
		lon2=50
		lat2=15


	return(proj,lon1,lon2,lat1,lat2)
