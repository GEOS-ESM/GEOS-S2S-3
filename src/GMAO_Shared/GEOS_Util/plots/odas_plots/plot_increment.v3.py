#! /usr/bin/env python
# importing the required module

import numpy as np
import matplotlib
matplotlib.use('Agg')
from netCDF4 import Dataset
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import matplotlib.cm as cm
import sys
import panoply_colormap


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
        odas_test_file = 'ODAS_Check.txt'
        print(filename)
        print('level is',type(lev))

	
        fh = Dataset(filename, mode='r')

        lons = np.array([(i * 0.25) - 180. for i in range(1440)])
        lats = np.array([(j * 0.25) - 90. for j in range(721)])
#	lons=np.zeros(720)
#	for i in range(0,720):
#	 lons[i] = (i*0.5)-180.
#	lats=np.zeros(361)
#	for j in range(0,361):
#	 lats[j] = (j*0.5)-90.

	#print lats
        level=int(lev)-1
        temp = fh.variables[var][0][level][:][:]
        minvar = np.min(temp)
        maxvar = np.max(temp)
        print ('shape of temp is',np.shape(temp), np.min(temp), np.max(temp))
        if (minvar != 0) &( maxvar != 0):
            with open("ODAS_Check.txt", "w") as file1:
               file1.write('1')
        temp_units = 'PSU' if var == 'salt' else 'Degrees C' if var == 'temp' else ''
	#temp_units = 'Degrees C'
	#if (var == 'salt'):
	# temp_units = 'PSU'
	#if (var == 'temp'):
	# temp_units = 'Degrees C'

#        depths= [' 5',' 15',' 25',' 35',' 45',' 55',' 65',' 75',' 85',' 95',' 105',' 115',' 125',' 135',' 145',' 155',' 165',' 175',' 185',' 195',' 205',' 215',' 225',' 238',' 262',' 303',' 367',' 459',' 586',' 747',' 950',' 1194',' 1480',' 1807',' 2175',' 2579',' 3017',' 3483',' 3972',' 4478']

        depths = [' 5',' 15',' 25',' 35',' 45',' 56',' 66',' 77',' 88',' 99',' 110',' 122',' 135', \
                ' 149',' 164',' 181',' 201','225',' 253',' 288',' 330', '382',' 447',' 525',' 619', \
                ' 729',' 855',' 997',' 1152',' 1320',' 1498',' 1683',' 1875',' 2071',' 2271',' 2474', \
                ' 2679',' 2885',' 3092',' 3300',' 3509',' 3718', ' 3927',' 4136',' 4346',' 4556',' 4765', \
                ' 4975',' 5185',' 5395']

        print ('depth is',depths[int(lev)-1])
        fh.close()
        lon, lat = np.meshgrid(lons, lats)
  # Load custom color map
        name = 'my_cmap'
        fileName = "GMT_panoply.txt"
        my_cmap = panoply_colormap.from_ascii(fileName, name)
#        colormaps.register(cmap=my_cmap)       
#        plt.register_cmap(name=name, cmap=my_cmap)
#        matplotlib.colormaps.register(name=name, cmap=my_cmap)
#        custom_map = cm.get_cmap(name)
        custom_map = matplotlib.colormaps.get_cmap(name)
     
#        cm.register_cmap(cmap=my_cmap)
#        custom_map = cm.get_cmap(name)

        fig=plt.figure(figsize=(12,8) )
        ax = plt.axes(projection=ccrs.PlateCarree())
        ax.set_global()

  # Plot the data
        print(np.shape(temp), np.min(temp), np.max(temp), vmin, vmax)
        cs = ax.pcolormesh(lon, lat, np.squeeze(temp), transform=ccrs.PlateCarree(),
                       vmin=vmin, vmax=vmax, cmap=cm.jet)

    # Add features
        ax.coastlines()
        ax.gridlines(draw_labels=True)

    # Colorbar
        cbar = plt.colorbar(cs, orientation='horizontal', pad=0.05)
        cbar.set_label(temp_units, fontsize=12, fontweight='bold')

    # Title
        plottit = plottit + ' ' + depths[int(lev) - 1] + ' m'
        plt.title(plottit, fontweight='bold', fontsize=14)
        plt.savefig(outname)
else:
        print(' ')
        print('# * Args: ')
        print("# *           filename      :   file with increment")
        print("# *           var           :   variable to plot ('temp','salt','ice','SLV')")
        print("# *           level         :   level to plot (1=5m, 2=15m, 6=55m, 10=98m 20=287m, 24=524)")
        print("# *           plottit       :   plot title ('HERE IS THE PLOT TITLE')")
        print("# *           vmax          :   max plot value (0. makes plot default)")
        print("# *           vmin          :   min plot value (0. makes plot default)")
        print("# *           outname       :   png file name")
