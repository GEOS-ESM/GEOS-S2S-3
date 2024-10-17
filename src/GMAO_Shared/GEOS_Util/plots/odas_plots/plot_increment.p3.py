#! /usr/bin/env python
# importing the required module

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from netCDF4 import Dataset
import numpy as np
import cartopy.crs as ccrs
import cartopy.feature as cfeature

import matplotlib.cm as cm
import sys
#sys.path.append('/gpfsm/dnb42/projects/p17/ehackert/geos5/exp/process_increment/panoply_colormap_p3.py')
#import panoply_colormap_p3


if len(sys.argv) == 8:
    #   GOOD TO GO
    filename = sys.argv[1]      # '/gpfsm/dnb42/projects/p17/ehackert/geos5/exp/eh025/
                                #     ocean_das/oana-20140121_00/mean_ana_restart/incr.nc'
    var      = sys.argv[2]      # 'temp','salt'
    lev      = int(sys.argv[3])      # 1, 6, 10
    plottit  = sys.argv[4]      # 'PLOT TITLE'
    vmin     = float(sys.argv[5])      # minimum for the plot (0. lets plot do range)
    vmax     = float(sys.argv[6])     # maximum for the plot (0. lets plot do range)
    outname  = sys.argv[7]      # output_file_name

    print('level is',lev)
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

    fig=plt.figure(figsize=(12,8))
    ax = plt.axes(projection=ccrs.PlateCarree(-80))
    ax.set_global()
    ax.coastlines()
    ax.add_feature(cfeature.BORDERS, lw=.5)
    ax.add_feature(cfeature.RIVERS)
    ax.add_feature(cfeature.LAND)

# Plot Data
#   from Robin
#   name    = 'my_cmap'
#   fileName = "/gpfsm/dnb42/projects/p17/ehackert/geos5/exp/process_increment/GMT_panoply.txt"
#   my_cmap  = panoply_colormap_p3.from_ascii(fileName, name)
#   cm.register_cmap(cmap=my_cmap)
#   custom_map  = cm.get_cmap(name)

#   print (temp.shape, type(lons), type(lats), vmin, vmax)
#   cs = ax.pcolor(lons, lats, np.squeeze(temp), 
    cs = ax.pcolormesh(lons, lats, temp, 
                   vmin=vmin, vmax=vmax, transform=ccrs.PlateCarree(),
                   cmap='jet')

# Add Grid Lines
    gl = ax.gridlines(alpha=0.5, linestyle='--', draw_labels=True,
                     xlocs = np.arange(-180., 181., 40.),
                     ylocs = np.arange(-80., 90., 20.))

# Add Colorbar
#   cbar = fig.colorbar(cs, location='bottom', pad="15%")
    cbar = fig.colorbar(cs, location='bottom', shrink=0.5)
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
