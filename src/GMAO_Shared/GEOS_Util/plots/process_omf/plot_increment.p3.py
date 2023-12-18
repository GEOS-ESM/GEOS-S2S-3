#! /usr/bin/env python
# importing the required module

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from netCDF4 import Dataset
import numpy as np
from mpl_toolkits.basemap import Basemap
import matplotlib.cm as cm
import sys
sys.path.append('/gpfsm/dnb42/projects/p17/ehackert/geos5/exp/process_increment/panoply_colormap_p3.py')
import panoply_colormap_p3


if len(sys.argv) == 8:
    #   GOOD TO GO
    filename = sys.argv[1]      # '/gpfsm/dnb42/projects/p17/ehackert/geos5/exp/eh025/
                                #     ocean_das/oana-20140121_00/mean_ana_restart/incr.nc'
    var      = sys.argv[2]      # 'temp','salt'
    lev      = sys.argv[3]      # 1, 6, 10
    plottit  = sys.argv[4]      # 'PLOT TITLE'
    vmax     = sys.argv[5]      # maximum for the plot (0. lets plot do range)
    vmin     = sys.argv[6]      # minimum for the plot (0. lets plot do range)
    outname  = sys.argv[7]      # output_file_name

    print('level is',type(lev))
    print('filename is',filename)

    #my_example_nc_file = 'eh025/ocean_das/oana-20140121_00_NEW_V3_50LEV_QC1/mean_ana_restart/incr.nc'

    my_example_nc_file = filename
    fh = Dataset(str(my_example_nc_file), mode='r')

    lons=np.zeros(1440)
    for i in range(0,1440):
        lons[i] = (i*0.25)-180.
    lats=np.zeros(721)
    for j in range(0,721):
        lats[j] = (j*0.25)-90.

    #print lats
    level=int(lev)
    level=level-1
    temp = fh.variables[var][0][level][:][:]
    print('shape of temp is',np.shape(temp))

    #temp_units = 'Degrees C'
    if (var == 'salt'):
        temp_units = 'PSU'
    if (var == 'temp'):
        temp_units = 'Degrees C'

    depths= [' 5',' 15',' 25',' 35',' 46',' 56',' 66',' 77',' 88',' 99',' 110',' 122',' 135',' 149',' 164',' 181',' 201',' 225',' 253',' 288',' 330',' 382',' 447',' 525',' 619',' 729',' 855',' 997',' 1152',' 1320',' 1498',' 1683',' 1875',' 2071',' 2271',' 2474',' 2679',' 2885',' 3092',' 3300',' 3509',' 3718',' 3927',' 4136',' 4346',' 4556',' 4765',' 4975',' 5185',' 5395']
    print('depth is',depths[int(lev)-1])

    fh.close()

    fig=plt.figure(figsize=(12,8) )

# Get some parameters for the Stereographic Projection
    lon_0 = lons.mean()
    #lon_0 = 180.
    lat_0 = lats.mean()

    print(lon_0, lat_0)

    m = Basemap(projection='mill',llcrnrlat=-90.00,urcrnrlat=90.00,\
        llcrnrlon=-180,urcrnrlon=180,resolution='c')

# Because our lon and lat variables are 1D,
# use meshgrid to create 2D arrays
# Not necessary if coordinates are already in 2D arrays.
    lon, lat = np.meshgrid(lons, lats)
 # 1st left
    #xi, yi = m(lon, lat)
    #xi, yi = m(lon+360, lat)

    #temp1=np.zeros((721,1440))
    #print np.shape(temp1)
    #temp1=temp[0,0,:,:]

    #map = Basemap(projection='mill',lon_0=180)
# Plot Data
    #cs = m.pcolor(xi,yi,np.squeeze(temp),vmin=-4.,vmax=4.,cmap=cm.jet)
#  then right
    xi, yi = m(lon, lat)
    #cs = m.pcolor(xi,yi,np.squeeze(temp),vmin=-4.,vmax=4.,cmap=cm.jet)
    #if (vmax == 0. & vmin == 0.):
    #cs = m.pcolor(xi,yi,np.squeeze(temp),cmap=cm.jet)
    #else:
#   from Robin
    name    = 'my_cmap'
#       fileName = "GMT_panoply.txt"
    fileName = "/gpfsm/dnb42/projects/p17/ehackert/geos5/exp/process_increment/GMT_panoply.txt"
#   my_cmap  = panoply_colormap.from_ascii(fileName, name)
    my_cmap  = panoply_colormap_p3.from_ascii(fileName, name)
    cm.register_cmap(cmap=my_cmap)
    custom_map  = cm.get_cmap(name)

#    cs = m.pcolor(xi,yi,np.squeeze(temp),vmin=vmin,vmax=vmax,cmap=cm.jet)
    cs = m.pcolor(xi,yi,np.squeeze(temp),vmin=vmin,vmax=vmax,cmap=cm.jet,shading='auto')
#    ROBINS blue/red colormap
    #cs = m.pcolor(xi,yi,np.squeeze(temp),vmin=vmin,vmax=vmax,cmap=custom_map)
    #cs = m.pcolor(xi,yi,np.squeeze(temp),vmin=vmin,vmax=vmax,cmap=cm.gist_rainbow)

# Add Grid Lines
    #m.drawparallels(np.arange(-90., 90., 20.), labels=[1,1,1,1], fontsize=12,fontweight='bold')
    m.drawparallels(np.arange(-80., 90., 20.), labels=[1,1,1,1], fontsize=12,fontweight='bold')
    m.drawmeridians(np.arange(-180., 181., 40.), labels=[0,0,0,1], fontsize=12,fontweight='bold')

# Add Coastlines, States, and Country Boundaries
    m.drawcoastlines()

# Add Colorbar
    cbar = m.colorbar(cs, location='bottom', pad="15%")
    cbar.set_label(temp_units,fontsize=12,fontweight='bold')

# Add Title
    plottit=plottit+' '+depths[int(lev)-1]+' m'
    plt.title(plottit,fontweight='bold',fontsize=14)
    plt.savefig(outname)
else:
    print(' ')
    print('# * Args: ')
    print('# *           filename      :   file with increment')
    print('# *           var           :   variable to plot (\'temp\',\'salt\',\'ice\',\'SLV\')')
    print('# *           level         :   level to plot (1=5m, 2=15m, 6=55m, 10=98m 20=287m, 24=524)')
    print('# *           plottit       :   plot title (\'HERE IS THE PLOT TITLE\')')
    print('# *           vmax          :   max plot value (0. makes plot default)')
    print('# *           vmin          :   min plot value (0. makes plot default)')
    print('# *           outname       :   png file name')
"""
"""
