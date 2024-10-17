#! /usr/bin/env python3

import xarray as xr
import matplotlib.pyplot as plt
from glob import glob
import os
import argparse

# Pass argsi
parser = argparse.ArgumentParser(description=("Script to generate ODAS increment xz plots"))
parser.add_argument('filename', help=f'complete path to incr.nc file')
parser.add_argument('vname', help=f'variable name to plot')
parser.add_argument('min_lat', type=float, help=f'bottom lat for meridional average')
parser.add_argument('max_lat', type=float, help=f'bottom lat for meridional average')
parser.add_argument('depth', type=float,  help=f'depht to plot (positive)')
parser.add_argument('year', metavar='YYYY', help='4 digit year')
parser.add_argument('month', metavar='MM', help='2 digit month')
parser.add_argument('day', metavar='DD', help='2 digit day')
parser.add_argument('hour', metavar='HH', help='2 digit hour')

args = parser.parse_args()
filename, vname = args.filename, args.vname 
blat, tlat, mdepth = args.min_lat, args.max_lat, args.depth,
yyyy, mm, dd, hh = args.year, args.month, args.day, args.hour
date = f'{yyyy}{mm}{dd}'
print (filename, vname, yyyy, mm, dd, hh, blat, tlat, mdepth)

# Presets
gpath = os.environ['ODAS_RC']
expdir = os.environ['EXPDIR']
gfile = f'{gpath}/test_interp_1440x721_to_1440x1080/grid_spec_1440x721x50.nc'
opath = f'{expdir}/plots_odas'
cb_range = {'temp': 1, 'salt':.2}

# Load grid
grid = xr.open_dataset(gfile)['olmask']
grid = grid.rename({'grid_x_T':'xaxis_1', 'grid_y_T': 'yaxis_1', 'zt':'zaxis_1'})

# Load var
ds = xr.open_dataset(filename)
var = ds[vname].squeeze()
var = var.assign_coords(grid.coords)
var['zaxis_1'] = -var['zaxis_1']

# Select and mean
var = var.where(grid.values==1).sel(yaxis_1=slice(blat, tlat), zaxis_1=slice(None, -mdepth))
var  = var.mean('yaxis_1', skipna=True).squeeze()

# Plot ranges:
vmin, vmax = -cb_range[vname], cb_range[vname]

# Plot
fig, ax = plt.subplots(figsize=(10,5),dpi=100)
var.plot(ax=ax, cmap='jet', vmin=vmin, vmax=vmax, cbar_kwargs={'label':f'XZ_{vname.upper()}'})
ax.set_ylabel('Depth (m)')
ax.set_xlabel('lon')
ax.set_title(f'{date}H{hh}')

string=(f'{opath}/XZ_{vname}_{blat}-{tlat}_0-{mdepth}_{date}{hh}.png')
print ('output file is',string)
fig.savefig(f'{opath}/XZ_{vname}_{blat}-{tlat}_0-{mdepth}_{date}{hh}.png')
#fig.savefig(f'XZ_{vname}_{blat}-{tlat}_0-{mdepth}_{date}{hh}.png')

