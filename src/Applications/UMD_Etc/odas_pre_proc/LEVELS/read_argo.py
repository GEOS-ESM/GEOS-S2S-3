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

fname = '/gpfsm/dnb04/projects/p71/aogcm/g5odas/obs/assim/ARGO/V3/STEP1/T_ARGO_2020.nc'
odasgrp   = Dataset(fname, 'r', format='NETCDF4')
date_time = odasgrp.variables['DATE_TIME'][:]	
npts      = odasgrp.variables['NPTS'][:]
qc_prf    = odasgrp.variables['QC_PRF'][:]
temp      = odasgrp.variables['TEMP'][:,:]
depth     = odasgrp.variables['DEPTH'][:,:]
qc_lev    = odasgrp.variables['QC_LEV'][:,:]
odasgrp.close()
temp[temp>999]=np.nan
depth[depth>999]=np.nan

print np.shape(depth)
print 'maxnpts',np.max(npts)
i=1
print npts[0:10]
print depth[i,0:10]

sys.exit()
tm,zm = np.shape(temp)
print tm, zm
tmax = 0
print depth[tmax,0:npts[tmax]]
sys.exit()
zmax = 34
print qc_lev[0,0:zmax]
print ' '
print temp[0,0:zmax]
print ' '
print depth[0,0:zmax]




	
