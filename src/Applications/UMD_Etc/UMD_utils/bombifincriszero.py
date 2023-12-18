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
import statistics
import os
 
SCRDIR = os.environ['SCRDIR']

if len(sys.argv) == 3:
    filename = sys.argv[1]      # '/gpfsm/dnb42/projects/p17/ehackert/geos5/exp/eh025/
    var      = sys.argv[2]

#   read temp_increment.nc, salt_increment.nc and confirm non-zero
    my_example_nc_file = filename
    fh = Dataset(str(my_example_nc_file), mode='r')
    temp = fh.variables[var][0][1][:][:]
#   print('shape of temp is',np.shape(temp))
    temp_mean=np.mean(temp)
    if(temp_mean != 0.0):
       print (temp_mean)
    else:
       print('BAD INCREMENT')
       command = 'touch '+SCRDIR+'/BADINCR'
       os.system(command)
       sys.exit(1)
else:
    print(' ') 
    print('# * Args: ')
    print('# *           filename      :   file with increment')
    print('# *           var           :   variable to plot (\'temp\',\'salt\',\'ice\',\'SLV\')')

exit(0)
