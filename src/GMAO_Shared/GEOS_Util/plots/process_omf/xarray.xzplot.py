#! /usr/bin/env python
# importing the required module

#module load python/GEOSpyD
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
plt.style.use('classic')
import xarray as xr                        # import xarray module
import sys

if len(sys.argv) == 6:
	filename = sys.argv[1]   # eg. 'XZ_SALT_-5-5_0-300_2018012100.nc'
        var      = sys.argv[2]   # 'temp' or 'salt'
	vmin     = sys.argv[3]   # -1. for temp, -0.2 for salt
	vmax     = sys.argv[4]   # 1. for temp, 0.2 for salt
	outname  = sys.argv[5]   # eg. 'XZ_SALT_-5-5_0-300_2018012100.png'

        print filename
        print var
        print vmin
        print vmax
        print outname

	if (var == 'salt'):
	  dsname='XZ_SALT'
	if (var == 'temp'):
	  dsname='XZ_TEMP'

        vmin=float(vmin)
        vmax=float(vmax)

	ds=xr.open_dataset(filename)  #  open dataset
	temp=ds[dsname]                           # read data
	#plt.title("TEMP INCR 20180121")
	#temp.plot(vmin=-0.4, vmax=0.4,cmap='jet')                  # plot data for salinity
	temp.plot(vmin=vmin, vmax=vmax,cmap='jet')                  # plot data for salinity
	plt.savefig(outname)                   # save plot to picture file
	#plt.show() 

else:
        print ' '
        print '# * Args: '
        print '# *           filename      :   file with xz output'
        print '# *           var           :   variable to plot (\'temp\',\'salt\')'
        print '# *           vmin          :   min plot value (0. makes plot default)'
        print '# *           vmax          :   max plot value (0. makes plot default)'
        print '# *           outname       :   png file name'
