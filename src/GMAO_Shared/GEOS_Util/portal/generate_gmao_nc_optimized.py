#!/usr/bin/env python3
"""
Standalone generation of gmao-obs-*.nc files from the raw observation archive.

This is the secondary capability of this directory. The primary workflow is
plot generation and portal deployment from an experiment's own production
output (see run_odas_workflow.py and README.md); this module is used when
observation files need to be produced for a standalone window instead.

Data paths below are resolved relative to the current working directory, so
run this module from the directory where the `assim/` observation tree
resolves. run_odas_workflow.py and reproduce_gmao_files.py can instead set up a
temporary `assim` symlink to a configured archive path before importing it.
"""

import os
import sys
import time
import warnings
from datetime import datetime, timedelta

import numpy as np
import xarray as xr
from netCDF4 import Dataset

warnings.simplefilter(action='ignore', category=FutureWarning)

# -------------------------------------------------------------------------
# Global Configurations
# -------------------------------------------------------------------------
EXP_NDAYS = 0.125
T_PROF_SIGO = 0.25   # deg C
S_PROF_SIGO = 0.025  # psu

XSIGO = {
    's': 1.0,
    't': 1.0,
    'sst': 2.0,
    'sss': 1.0,
    'ssh': 0.25,
    'aice': 1.0,
    'hice': 0.01
}

# Source directories: './assim/...' paths are relative to the working directory;
# the remaining paths are absolute archive locations.
DIRS = {
    'lev50': './assim/',
    'eric_lev50': '/gpfsm/dnb78s2/projects/p26/ehackert/ODAS/OBS/V3_WITHHOLD_BY_WMO/',
    'adt': './assim/AVISO/V3/',
    'sat_sss': './assim/',
    'sat_smos': '/discover/nobackup/lren1/pre_proc/NRT/SMOS/L2_SMOS_SSS_7.0/',
    'syn': './assim/SYN_7.0/',
    'woa18': '/discover/nobackup/lren1/pre_proc/NRT/WOA/',
    'obs': '/discover/nobackup/projects/gmao/ssd/g5odas/production/GEOS5odas-5.00/RC/OBS/'
}

SMOS_RAMP_UP_ERR = 1.0

OBS_IDS = {
    'id_s_obs'   : 5521, # Salinity
    'id_t_obs'   : 3073, # Temperature
    'id_sst_obs' : 5525, # SST
    'id_sss_obs' : 5522, # Sea surface Salinity
    'id_ssh_obs' : 5526, # SSH (Not used ...)
    'id_eta_obs' : 5351, # SSH
    'id_aice_obs': 6000, # AICE
    'id_hice_obs': 6001  # HICE
}

# -------------------------------------------------------------------------
# Helper Functions
# -------------------------------------------------------------------------
def get_time_window(yyyy, mm, dd, hh, ndays):
    """Return start and end dates formatted as YYYYMMDDHH integers."""
    date = datetime.strptime(f'{yyyy}{mm}{dd}{hh}', '%Y%m%d%H')
    sd = int((date - timedelta(days=ndays)).strftime('%Y%m%d%H'))
    ed = int((date + timedelta(days=ndays)).strftime('%Y%m%d%H'))
    return sd, ed

def safe_flatten(da, mask=None):
    """Flattens a masked xarray DataArray into a numpy 1D array."""
    if mask is not None:
        da = da.where(mask)
    if 'N_LEVS' in da.dims:
        da = da.stack(flatten=('N_PROF', 'N_LEVS'))
    else:
        da = da.rename({'N_PROF':'flatten'})
    return da.dropna(dim='flatten').values

def print_timing_summary(timing, total_elapsed, title="Processing time per obs type"):
    """Prints a simple table of per-obs-type processing time, sorted slowest first."""
    if not timing:
        return
    print(f"\n--- {title} ---")
    width = max(len(k) for k in timing) + 2
    for obs_type, elapsed in sorted(timing.items(), key=lambda kv: kv[1], reverse=True):
        pct = (elapsed / total_elapsed * 100) if total_elapsed > 0 else 0.0
        print(f"  {obs_type:<{width}} {elapsed:8.2f}s  ({pct:5.1f}%)")
    print(f"  {'TOTAL':<{width}} {total_elapsed:8.2f}s")
    print("-" * (width + 22))

# -------------------------------------------------------------------------
# Main Observation Processor
# -------------------------------------------------------------------------
def process_observation(fname, id_obs, vartype, xsigo, descriptor, platform, yyyy, mm, dd, hh):
    """
    Reads a NetCDF observation file, applies QC masking, and returns a dictionary 
    of flattened numpy arrays containing the valid observations.
    """
    if not os.path.exists(fname):
        print(f'Warning: No file found at {fname}')
        return None

    print(f'Processing {descriptor}: {fname}')
    keep_vars = ['N_LEVS', 'DEPTH', vartype, 'QC_LEV', 'QC_PRF', 'LON', 'LAT', 'DATE_TIME', 'OBS_ERROR', 'INST_ID']

    # Open without Dask chunking: on slow/network-mounted filesystems, chunked
    # lazy reads turn each subsequent .where(...)/.diff(...) into its own
    # round of small, high-latency I/O (and can appear to "hang" since a
    # blocked network read is uninterruptible). We only need a handful of
    # variables and ultimately materialize everything anyway, so select the
    # needed variables and .load() them eagerly in one shot right away.
    with xr.open_dataset(fname) as ds_handle:
        available_vars = [v for v in keep_vars if v in ds_handle.variables or v in ds_handle.dims]
        ds = ds_handle[available_vars].load()

    # Handle Depth Levels
    if ds.dims.get('N_LEVS', 0) > 50:
        ds = ds.isel(N_LEVS=slice(0, 50))
    else:
        ds = ds.squeeze()

    # Apply Time Window Mask
    sd, ed = get_time_window(yyyy, mm, dd, hh, EXP_NDAYS)
    if 'DATE_TIME' in ds:
        time_mask = (ds.DATE_TIME >= sd) & (ds.DATE_TIME <= ed)
        ds = ds.where(time_mask.compute(), drop=True)

    # Apply Profile QC Mask
    if 'QC_PRF' in ds:
        qc_mask = (ds.QC_PRF == 1)
        ds = ds.where(qc_mask.compute(), drop=True)

    # Early exit if no valid data
    if vartype not in ds or ds[vartype].shape[0] == 0:
        print(f'No observations in time window for {descriptor}.')
        ds.close()
        return None

    # Calculate Temperature/Salinity profile derivative (dTdZ / dSdZ)
    if id_obs in (OBS_IDS['id_t_obs'], OBS_IDS['id_s_obs']):
        dTdZ = xr.zeros_like(ds[vartype])
        if 'N_LEVS' in ds.dims:
            # Safe divide: Avoid division by zero warnings when depth difference is 0 or NaN
            diff_var = np.abs(ds[vartype].diff('N_LEVS'))
            diff_depth = ds.DEPTH.diff('N_LEVS')
            # Replace zeros in depth difference with NaN to avoid RuntimeWarning: divide by zero
            diff_depth = diff_depth.where(diff_depth != 0, np.nan)

            dTdZ[:, :-1] = (diff_var / diff_depth).values
            dTdZ[:, -1] = dTdZ[:, -2]
        dTdZ = dTdZ.where(dTdZ >= 1e-3, 1e-3)
        ds['OBS_ERROR'][:] = 2.0 * dTdZ

    # Masking logic based on variable type
    mask = (ds['QC_LEV'] == 1) if 'QC_LEV' in ds else xr.ones_like(ds[vartype], dtype=bool)

    if id_obs == OBS_IDS['id_s_obs']:
        mask &= (ds[vartype] >= 29) & (ds[vartype] <= 40)
    elif id_obs == OBS_IDS['id_t_obs']:
        mask &= (ds[vartype] > -2.)
    elif id_obs == OBS_IDS['id_sst_obs']:
        if 'OBS_ERROR' in ds:
            ds['OBS_ERROR'] = ds['OBS_ERROR'].where(ds['OBS_ERROR'] > 1e-2, 1e-2)
    elif id_obs == OBS_IDS['id_sss_obs']:
        if descriptor in ['SMAP_L2_SSS', 'SMOS_L2_SSS', 'Aquarius_L2_SSS', 'SMOS_RC_L2_SSS', 'Aquarius_RIM_L2_SSS', 'SMAP_RIM_L2_SSS']:
            if 'OBS_ERROR' in ds:
                ds['OBS_ERROR'] *= SMOS_RAMP_UP_ERR
                mask &= (ds['OBS_ERROR'] >= 0.1)
            mask &= (ds[vartype] >= 29) & (ds[vartype] <= 40)
            
        elif descriptor in ['SMAP_L3_SSS', 'SMOS_L3_SSS', 'Aquarius_L3_SSS']:
            if 'OBS_ERROR' in ds:
                ds['OBS_ERROR'] = ds['OBS_ERROR'].where(ds['OBS_ERROR'] > 0.1, 0.1)
                sigo_scale = 0.4 * ((np.abs(ds['LAT']) - 30.) / 30.0)
                sigo_scale = sigo_scale.where(sigo_scale < 0.4, 0.4).where(sigo_scale > 0.0, 0.0)
                ds['OBS_ERROR'] += sigo_scale
                
    elif id_obs == OBS_IDS['id_eta_obs']:
        mask &= (np.abs(ds[vartype]) < 10.)
        if 'LAT' in ds:
            mask &= (ds['LAT'] > -50)
        if 'OBS_ERROR' in ds:
            mask &= (ds['OBS_ERROR'] < 999)
            ds['OBS_ERROR'] = ds['OBS_ERROR'].where(ds['OBS_ERROR'] > 0.1, 0.1)
            ds['OBS_ERROR'] += 0.1 * (ds['LAT'] / 90.0)**4
            
    elif id_obs == OBS_IDS['id_aice_obs']:
        if 'OBS_ERROR' in ds:
            ds['OBS_ERROR'] = 0.05 * xr.ones_like(ds[vartype])
    elif id_obs == OBS_IDS['id_hice_obs']:
        if 'OBS_ERROR' in ds:
            ds['OBS_ERROR'] = 0.3 * xr.ones_like(ds[vartype])

    # Final mask checks
    mask &= (~ds[vartype].isnull())
    if 'OBS_ERROR' in ds:
        mask &= (ds['OBS_ERROR'] < 999.9)
        
    # Extract flattened data using mask
    ds = ds.compute()
    obs_data = {
        'value': safe_flatten(ds[vartype], mask)
    }
    
    # If there's no valid data after masking, return None
    if len(obs_data['value']) == 0:
        ds.close()
        return None

    obs_data['lon'] = safe_flatten(ds.LON, mask) if 'LON' in ds else np.array([])
    obs_data['lat'] = safe_flatten(ds.LAT, mask) if 'LAT' in ds else np.array([])
    obs_data['depth'] = safe_flatten(ds.DEPTH, mask) if 'DEPTH' in ds else np.array([])
    obs_data['instid'] = safe_flatten(ds.INST_ID, mask) if 'INST_ID' in ds else np.array([])
    obs_data['oerr'] = xsigo * safe_flatten(ds.OBS_ERROR, mask) if 'OBS_ERROR' in ds else np.array([])
    obs_data['typ'] = np.full(obs_data['value'].shape, id_obs, dtype=np.int32)
    
    ds.close()
    return obs_data


# -------------------------------------------------------------------------
# Observation Configuration Mappings
# -------------------------------------------------------------------------
# Define all files as a clean dictionary map rather than individual functions
# format: "Name": [(fname_template, id_obs, vartype, xsigo, descriptor, platform, custom_hh)]
OBS_MAP = {
    "Argo": [
        (f"{DIRS['lev50']}ARGO/V3/FINAL/T_ARGO_{{yyyy}}.nc", OBS_IDS['id_t_obs'], 'TEMP', XSIGO['t'], 'Argo-T', None, None),
        (f"{DIRS['lev50']}ARGO/V3/FINAL/S_ARGO_{{yyyy}}.nc", OBS_IDS['id_s_obs'], 'SALT', XSIGO['s'], 'Argo-S', None, None)
    ],
    "Argo_1": [
        (f"{DIRS['eric_lev50']}V3/FINAL/T_ARGO_{{yyyy}}_1.nc", OBS_IDS['id_t_obs'], 'TEMP', XSIGO['t'], 'Argo_1-T', None, None),
        (f"{DIRS['eric_lev50']}V3/FINAL/S_ARGO_{{yyyy}}_1.nc", OBS_IDS['id_s_obs'], 'SALT', XSIGO['s'], 'Argo_1-S', None, None)
    ],
    "Argo_2": [
        (f"{DIRS['eric_lev50']}V3/FINAL/T_ARGO_{{yyyy}}_2.nc", OBS_IDS['id_t_obs'], 'TEMP', XSIGO['t'], 'Argo_2-T', None, None),
        (f"{DIRS['eric_lev50']}V3/FINAL/S_ARGO_{{yyyy}}_2.nc", OBS_IDS['id_s_obs'], 'SALT', XSIGO['s'], 'Argo_2-S', None, None)
    ],
    "Argo_3": [
        (f"{DIRS['eric_lev50']}V3/FINAL/T_ARGO_{{yyyy}}_3.nc", OBS_IDS['id_t_obs'], 'TEMP', XSIGO['t'], 'Argo_3-T', None, None),
        (f"{DIRS['eric_lev50']}V3/FINAL/S_ARGO_{{yyyy}}_3.nc", OBS_IDS['id_s_obs'], 'SALT', XSIGO['s'], 'Argo_3-S', None, None)
    ],
    "Argo_4": [
        (f"{DIRS['eric_lev50']}V3/FINAL/T_ARGO_{{yyyy}}_4.nc", OBS_IDS['id_t_obs'], 'TEMP', XSIGO['t'], 'Argo_4-T', None, None),
        (f"{DIRS['eric_lev50']}V3/FINAL/S_ARGO_{{yyyy}}_4.nc", OBS_IDS['id_s_obs'], 'SALT', XSIGO['s'], 'Argo_4-S', None, None)
    ],
    "Argo_5": [
        (f"{DIRS['eric_lev50']}V3/FINAL/T_ARGO_{{yyyy}}_5.nc", OBS_IDS['id_t_obs'], 'TEMP', XSIGO['t'], 'Argo_5-T', None, None),
        (f"{DIRS['eric_lev50']}V3/FINAL/S_ARGO_{{yyyy}}_5.nc", OBS_IDS['id_s_obs'], 'SALT', XSIGO['s'], 'Argo_5-S', None, None)
    ],
    "CTD": [
        (f"{DIRS['lev50']}CTD/V3/FINAL/T_CTD_{{yyyy}}.nc", OBS_IDS['id_t_obs'], 'TEMP', XSIGO['t'], 'CTD-T', None, None),
        (f"{DIRS['lev50']}CTD/V3/FINAL/S_CTD_{{yyyy}}.nc", OBS_IDS['id_s_obs'], 'SALT', XSIGO['s'], 'CTD-S', None, None)
    ],
    "XBT": [
        (f"{DIRS['lev50']}XBT/V3/FINAL/T_XBT_{{yyyy}}.nc", OBS_IDS['id_t_obs'], 'TEMP', XSIGO['t'], 'XBT-T', None, None)
    ],
    "XBT-SYN-S": [
        (f"{DIRS['syn']}XBT/SYN_XBT_{{yyyy}}.nc", OBS_IDS['id_s_obs'], 'SALT', XSIGO['s'], 'XBT-SYN-S', None, None)
    ],
    "WOA18_atArgo": [
        (f"{DIRS['woa18']}WOA_ARGO/T_WOAatARGO_{{yyyy}}.nc", OBS_IDS['id_t_obs'], 'TEMP', XSIGO['t'], 'WOA18_atArgo-T', None, None),
        (f"{DIRS['woa18']}WOA_ARGO/S_WOAatARGO_{{yyyy}}.nc", OBS_IDS['id_s_obs'], 'SALT', XSIGO['s'], 'WOA18_atArgo-S', None, None)
    ],
    "WOA18_atNA": [
        (f"{DIRS['woa18']}WOA_NA/T_WOAatNA_{{yyyy}}.nc", OBS_IDS['id_t_obs'], 'TEMP', XSIGO['t'], 'WOA18_atNA-T', None, None),
        (f"{DIRS['woa18']}WOA_NA/S_WOAatNA_{{yyyy}}.nc", OBS_IDS['id_s_obs'], 'SALT', XSIGO['s'], 'WOA18_atNA-S', None, None)
    ],
    "TAO": [
        (f"{DIRS['lev50']}TAO/V3/FINAL/T_TAO_{{yyyy}}.nc", OBS_IDS['id_t_obs'], 'TEMP', XSIGO['t'], 'TAO-T', None, None),
        (f"{DIRS['lev50']}TAO/V3/FINAL/S_TAO_{{yyyy}}.nc", OBS_IDS['id_s_obs'], 'SALT', XSIGO['s'], 'TAO-S', None, None)
    ],
    "PIRATA": [
        (f"{DIRS['lev50']}PIRATA/V3/FINAL/T_PIR_{{yyyy}}.nc", OBS_IDS['id_t_obs'], 'TEMP', XSIGO['t'], 'PIRATA-T', None, None),
        (f"{DIRS['lev50']}PIRATA/V3/FINAL/S_PIR_{{yyyy}}.nc", OBS_IDS['id_s_obs'], 'SALT', 10.0*XSIGO['s'], 'PIRATA-S', None, None)
    ],
    "RAMA": [
        (f"{DIRS['lev50']}RAMA/V3/FINAL/T_RAMA_{{yyyy}}.nc", OBS_IDS['id_t_obs'], 'TEMP', XSIGO['t'], 'RAMA-T', None, None),
        (f"{DIRS['lev50']}RAMA/V3/FINAL/S_RAMA_{{yyyy}}.nc", OBS_IDS['id_s_obs'], 'SALT', 10.0*XSIGO['s'], 'RAMA-S', None, None)
    ],
    "SMOS": [
        (f"{DIRS['lev50']}L2_SMOS_SSS_7.0/L32Q/SSS_TRK_SMOS_L32Q_{{yyyy}}.nc", OBS_IDS['id_sss_obs'], 'SSS', XSIGO['sss'], 'SMOS_L2_SSS', None, None)
    ],
    "SMOS_RC": [
        (f"{DIRS['lev50']}L2_SMOS_SSS_7.0/L32Q/SSS_TRK_SMOS_L32Q_RC{{yyyy}}.nc", OBS_IDS['id_sss_obs'], 'SSS', XSIGO['sss'], 'SMOS_RC_L2_SSS', None, None)
    ],
    "SMOSSUB": [
        (f"{DIRS['sat_smos']}SSS_TRK_SMOS_L32Q_{{yyyy}}.nc", OBS_IDS['id_sss_obs'], 'SSS', XSIGO['sss'], 'SMOS_L2_SSS', 'SMOSSUB', None)
    ],
    "SMOSL3": [
        (f"{DIRS['sat_sss']}L3_SMOS_SSS_7.0/SSS_GRD_SMOS_{{yyyy}}.nc", OBS_IDS['id_sss_obs'], 'SSS', XSIGO['sss'], 'SMOS_L3_SSS', None, '12')
    ],
    "SMAP": [
        (f"{DIRS['sat_sss']}L2_SMAP_SSS_7.0/V4/SSS_TRK_SMAP_V4_{{yyyy}}.nc", OBS_IDS['id_sss_obs'], 'SSS', XSIGO['sss'], 'SMAP_L2_SSS', None, None)
    ],
    "SMAPV4.1": [
        (f"{DIRS['sat_sss']}L2_SMAP_SSS_7.0/V4.1/SSS_TRK_SMAP_V4.1_{{yyyy}}.nc", OBS_IDS['id_sss_obs'], 'SSS', XSIGO['sss'], 'SMAP_L2_SSS', None, None)
    ],
    "SMAPV4.2": [
        (f"{DIRS['sat_sss']}L2_SMAP_SSS_7.0/V4.2/SSS_TRK_SMAP_V4.2_{{yyyy}}.nc", OBS_IDS['id_sss_obs'], 'SSS', XSIGO['sss'], 'SMAP_L2_SSS', None, None)
    ],
    "SMAPV4.3": [
        (f"{DIRS['sat_sss']}L2_SMAP_SSS_7.0/V4.3/SSS_TRK_SMAP_V4.3_{{yyyy}}.nc", OBS_IDS['id_sss_obs'], 'SSS', XSIGO['sss'], 'SMAP_L2_SSS', None, None)
    ],
    "SMAPV5.0": [
        (f"{DIRS['sat_sss']}L2_SMAP_SSS_7.0/V5.0/SSS_TRK_SMAP_V5.0_{{yyyy}}.nc", OBS_IDS['id_sss_obs'], 'SSS', XSIGO['sss'], 'SMAP_L2_SSS', None, None)
    ],
    "SMAPL3": [
        (f"{DIRS['sat_sss']}L3_SMAP_SSS_7.0/8DAYRUN/SSS_GRD_SMAP_V4_{{yyyy}}.nc", OBS_IDS['id_sss_obs'], 'SSS', XSIGO['sss'], 'SMAP_L3_SSS', None, '12')
    ],
    "AQUARIUS": [
        (f"{DIRS['sat_sss']}L2_AQ_SSS_7.0/V5/SSS_TRK_AQ_V5_{{yyyy}}.nc", OBS_IDS['id_sss_obs'], 'SSS', XSIGO['sss'], 'Aquarius_L2_SSS', None, None)
    ],
    "AQUARIUS_RIM": [
        (f"{DIRS['sat_sss']}L2_AQ_SSS_7.0/V5_RIMV1_5m/SSS_TRK_AQ_V5_RIMV1_5m_{{yyyy}}.nc", OBS_IDS['id_sss_obs'], 'SSS', XSIGO['sss'], 'Aquarius_RIM_L2_SSS', None, None)
    ],
    "SMAP_RIM": [
        (f"{DIRS['sat_sss']}L2_SMAP_SSS_7.0/V4_RIMV1_5m/SSS_TRK_SMAP_V4_RIMV1_5m_{{yyyy}}.nc", OBS_IDS['id_sss_obs'], 'SSS', XSIGO['sss'], 'SMAP_RIM_L2_SSS', None, None)
    ],
    "SMAP_RIMV5": [
        (f"{DIRS['sat_sss']}L2_SMAP_SSS_7.0/V5_RIMV1_5m/SSS_TRK_SMAP_V5_RIMV1_5m_{{yyyy}}.nc", OBS_IDS['id_sss_obs'], 'SSS', XSIGO['sss'], 'SMAP_RIM_L2_SSS', None, None)
    ],
    "AQUARIUSL3": [
        (f"{DIRS['sat_sss']}L3_AQ_SSS_7.0/7DAYRUN/SSS_GRD_AQ_{{yyyy}}.nc", OBS_IDS['id_sss_obs'], 'SSS', XSIGO['sss'], 'Aquarius_L3_SSS', None, '12')
    ],
    "CryoSat-2": [
        (f"{DIRS['adt']}ADT_TRK_C2_{{yyyy}}.nc", OBS_IDS['id_eta_obs'], 'ADT', XSIGO['ssh'], 'CryoSat-2-ADT', None, None)
    ],
    "CryoSat-2-N": [
        (f"{DIRS['adt']}ADT_TRK_C2N_{{yyyy}}.nc", OBS_IDS['id_eta_obs'], 'ADT', XSIGO['ssh'], 'CryoSat-2-N-ADT', None, None)
    ],
    "Jason-1": [
        (f"{DIRS['adt']}ADT_TRK_J1_{{yyyy}}.nc", OBS_IDS['id_eta_obs'], 'ADT', XSIGO['ssh'], 'Jason-1-ADT', None, None)
    ],
    "Jason-1-N": [
        (f"{DIRS['adt']}ADT_TRK_J1N_{{yyyy}}.nc", OBS_IDS['id_eta_obs'], 'ADT', XSIGO['ssh'], 'Jason-1-N-ADT', None, None)
    ],
    "Jason-1G": [
        (f"{DIRS['adt']}ADT_TRK_J1G_{{yyyy}}.nc", OBS_IDS['id_eta_obs'], 'ADT', XSIGO['ssh'], 'Jason-1G-ADT', None, None)
    ],
    "Jason-2": [
        (f"{DIRS['adt']}ADT_TRK_J2_{{yyyy}}.nc", OBS_IDS['id_eta_obs'], 'ADT', XSIGO['ssh'], 'Jason-2-ADT', None, None)
    ],
    "Jason-2-N": [
        (f"{DIRS['adt']}ADT_TRK_J2N_{{yyyy}}.nc", OBS_IDS['id_eta_obs'], 'ADT', XSIGO['ssh'], 'Jason-2-N-ADT', None, None)
    ],
    "Jason-3": [
        (f"{DIRS['adt']}ADT_TRK_J3_{{yyyy}}.nc", OBS_IDS['id_eta_obs'], 'ADT', XSIGO['ssh'], 'Jason-3-ADT', None, None)
    ],
    "Jason-3N": [
        (f"{DIRS['adt']}ADT_TRK_J3N_{{yyyy}}.nc", OBS_IDS['id_eta_obs'], 'ADT', XSIGO['ssh'], 'Jason-3N-ADT', None, None)
    ],
    "Saral": [
        (f"{DIRS['adt']}ADT_TRK_AL_{{yyyy}}.nc", OBS_IDS['id_eta_obs'], 'ADT', XSIGO['ssh'], 'Saral-Altika-ADT', None, None)
    ],
    "Sentinel-3a": [
        (f"{DIRS['adt']}ADT_TRK_S3A_{{yyyy}}.nc", OBS_IDS['id_eta_obs'], 'ADT', XSIGO['ssh'], 'Sentinel-3a-ADT', None, None)
    ],
    "Sentinel-3b": [
        (f"{DIRS['adt']}ADT_TRK_S3B_{{yyyy}}.nc", OBS_IDS['id_eta_obs'], 'ADT', XSIGO['ssh'], 'Sentinel-3b-ADT', None, None)
    ],
    "Sentinel-6a": [
        (f"{DIRS['adt']}ADT_TRK_S6A_{{yyyy}}.nc", OBS_IDS['id_eta_obs'], 'ADT', XSIGO['ssh'], 'Sentinel-6a-ADT', None, None)
    ],
    "SWOT-nadir": [
        (f"{DIRS['adt']}ADT_TRK_SWON_{{yyyy}}.nc", OBS_IDS['id_eta_obs'], 'ADT', XSIGO['ssh'], 'SWOT-nadir', None, None)
    ],
    "ERS-1": [
        (f"{DIRS['adt']}ADT_TRK_E1_{{yyyy}}.nc", OBS_IDS['id_eta_obs'], 'ADT', XSIGO['ssh'], 'ERS-1-ADT', None, None)
    ],
    "ERS-2": [
        (f"{DIRS['adt']}ADT_TRK_E2_{{yyyy}}.nc", OBS_IDS['id_eta_obs'], 'ADT', XSIGO['ssh'], 'ERS-2-ADT', None, None)
    ],
    "TOPEX": [
        (f"{DIRS['adt']}ADT_TRK_TP_{{yyyy}}.nc", OBS_IDS['id_eta_obs'], 'ADT', XSIGO['ssh'], 'TOPEX-POSEIDON-ADT', None, None)
    ],
    "TOPEX-N": [
        (f"{DIRS['adt']}ADT_TRK_TPN_{{yyyy}}.nc", OBS_IDS['id_eta_obs'], 'ADT', XSIGO['ssh'], 'TOPEX-POSEIDON-N-ADT', None, None)
    ],
    "GEOSAT-2": [
        (f"{DIRS['adt']}ADT_TRK_G2_{{yyyy}}.nc", OBS_IDS['id_eta_obs'], 'ADT', XSIGO['ssh'], 'GEOSAT-follow-on-ADT', None, None)
    ],
    "Envisat": [
        (f"{DIRS['adt']}ADT_TRK_EN_{{yyyy}}.nc", OBS_IDS['id_eta_obs'], 'ADT', XSIGO['ssh'], 'Envisat-ADT', None, None)
    ],
    "Envisat-N": [
        (f"{DIRS['adt']}ADT_TRK_ENN_{{yyyy}}.nc", OBS_IDS['id_eta_obs'], 'ADT', XSIGO['ssh'], 'Envisat-N-ADT', None, None)
    ],
    "HY-2A": [
        (f"{DIRS['adt']}ADT_TRK_H2_{{yyyy}}.nc", OBS_IDS['id_eta_obs'], 'ADT', XSIGO['ssh'], 'HY-2A-ADT', None, None)
    ],
    "Reynolds": [
        (f"{DIRS['obs']}SST_6.0/SST_REYN_{{yyyy}}.nc", OBS_IDS['id_sst_obs'], 'TEMP', XSIGO['sst'], 'Reynolds-SST', None, None)
    ],
    "OSTIA": [
        (f"/discover/nobackup/projects/gmao/m2oasf/aogcm/g5odas/obs/raw/OSTIA/ASSIM_QRT_SUB/{{yyyy}}/SST_OSTIA_{{yyyy}}{{mm}}{{dd}}.nc", OBS_IDS['id_sst_obs'], 'TEMP', XSIGO['sst'], 'OSTIA SST', None, None)
    ],
    "AVHRR-18": [
        (f"./assim/L2_SST_7.0/{{yyyy}}/AVHRR18_G_{{yyyy}}{{mm}}{{dd}}.nc", OBS_IDS['id_sst_obs'], 'TEMP', 0.1, 'AVHRR18 L2 SST', None, None)
    ],
    # For no file provided cases we simulate empty paths to trigger safe no-obs returns
    "NOAA-16": [
        (f"/dev/null/noaa16_{{yyyy}}.nc", OBS_IDS['id_sst_obs'], 'TEMP', 0.1, 'NOAA-16', 'NOAA16', None)
    ],
    "METOP-A": [
        (f"/dev/null/metopa_{{yyyy}}.nc", OBS_IDS['id_sst_obs'], 'TEMP', 0.1, 'METOP-A', 'METOPA', None)
    ],
    "GMI-RSS": [
        (f"/dev/null/gmi_{{yyyy}}.nc", OBS_IDS['id_sst_obs'], 'TEMP', 0.1, 'GMI-RSS', 'GMI', None)
    ]
}

# -------------------------------------------------------------------------
# Aggregation & Output
# -------------------------------------------------------------------------
def process_observation_multi(fname, id_obs, vartype, xsigo, descriptor, platform, windows):
    """
    Like process_observation(), but opens `fname` exactly ONCE and extracts
    observations for MULTIPLE assimilation windows in a single pass. Intended
    for batch generation of several gmao-obs-*.nc cycles that all draw from
    the same per-year source file, to avoid re-opening/re-reading it once per
    cycle (expensive, especially over a slow/network-mounted filesystem).

    Parameters:
    -----------
    windows : list of (sd, ed) int YYYYMMDDHH window-bound tuples, one per
              output cycle, in the same order the caller wants results back.

    Returns:
    --------
    A list the same length as `windows`: each entry is an obs_data dict
    (same shape as process_observation()'s return value) or None if no
    valid observations fall in that particular window.
    """
    if not os.path.exists(fname):
        print(f'Warning: No file found at {fname}')
        return [None] * len(windows)

    print(f'Processing {descriptor} (multi-window, {len(windows)} cycles): {fname}')
    keep_vars = ['N_LEVS', 'DEPTH', vartype, 'QC_LEV', 'QC_PRF', 'LON', 'LAT', 'DATE_TIME', 'OBS_ERROR', 'INST_ID']

    with xr.open_dataset(fname) as ds_handle:
        available_vars = [v for v in keep_vars if v in ds_handle.variables or v in ds_handle.dims]
        ds = ds_handle[available_vars].load()

    if ds.dims.get('N_LEVS', 0) > 50:
        ds = ds.isel(N_LEVS=slice(0, 50))
    else:
        ds = ds.squeeze()

    # Profile QC is NOT time-dependent: apply it once, up front, to shrink the
    # working set before we slice it once per cycle below.
    if 'QC_PRF' in ds:
        qc_mask = (ds.QC_PRF == 1)
        ds = ds.where(qc_mask, drop=True)

    if vartype not in ds or ds[vartype].shape[0] == 0:
        print(f'No observations found at all for {descriptor} (before windowing).')
        return [None] * len(windows)

    # --- Everything below mirrors process_observation()'s TIME-INDEPENDENT logic exactly ---
    if id_obs in (OBS_IDS['id_t_obs'], OBS_IDS['id_s_obs']):
        dTdZ = xr.zeros_like(ds[vartype])
        if 'N_LEVS' in ds.dims:
            diff_var = np.abs(ds[vartype].diff('N_LEVS'))
            diff_depth = ds.DEPTH.diff('N_LEVS')
            diff_depth = diff_depth.where(diff_depth != 0, np.nan)
            dTdZ[:, :-1] = (diff_var / diff_depth).values
            dTdZ[:, -1] = dTdZ[:, -2]
        dTdZ = dTdZ.where(dTdZ >= 1e-3, 1e-3)
        ds['OBS_ERROR'][:] = 2.0 * dTdZ

    base_mask = (ds['QC_LEV'] == 1) if 'QC_LEV' in ds else xr.ones_like(ds[vartype], dtype=bool)

    if id_obs == OBS_IDS['id_s_obs']:
        base_mask &= (ds[vartype] >= 29) & (ds[vartype] <= 40)
    elif id_obs == OBS_IDS['id_t_obs']:
        base_mask &= (ds[vartype] > -2.)
    elif id_obs == OBS_IDS['id_sst_obs']:
        if 'OBS_ERROR' in ds:
            ds['OBS_ERROR'] = ds['OBS_ERROR'].where(ds['OBS_ERROR'] > 1e-2, 1e-2)
    elif id_obs == OBS_IDS['id_sss_obs']:
        if descriptor in ['SMAP_L2_SSS', 'SMOS_L2_SSS', 'Aquarius_L2_SSS', 'SMOS_RC_L2_SSS', 'Aquarius_RIM_L2_SSS', 'SMAP_RIM_L2_SSS']:
            if 'OBS_ERROR' in ds:
                ds['OBS_ERROR'] *= SMOS_RAMP_UP_ERR
                base_mask &= (ds['OBS_ERROR'] >= 0.1)
            base_mask &= (ds[vartype] >= 29) & (ds[vartype] <= 40)
        elif descriptor in ['SMAP_L3_SSS', 'SMOS_L3_SSS', 'Aquarius_L3_SSS']:
            if 'OBS_ERROR' in ds:
                ds['OBS_ERROR'] = ds['OBS_ERROR'].where(ds['OBS_ERROR'] > 0.1, 0.1)
                sigo_scale = 0.4 * ((np.abs(ds['LAT']) - 30.) / 30.0)
                sigo_scale = sigo_scale.where(sigo_scale < 0.4, 0.4).where(sigo_scale > 0.0, 0.0)
                ds['OBS_ERROR'] += sigo_scale
    elif id_obs == OBS_IDS['id_eta_obs']:
        base_mask &= (np.abs(ds[vartype]) < 10.)
        if 'LAT' in ds:
            base_mask &= (ds['LAT'] > -50)
        if 'OBS_ERROR' in ds:
            base_mask &= (ds['OBS_ERROR'] < 999)
            ds['OBS_ERROR'] = ds['OBS_ERROR'].where(ds['OBS_ERROR'] > 0.1, 0.1)
            ds['OBS_ERROR'] += 0.1 * (ds['LAT'] / 90.0)**4
    elif id_obs == OBS_IDS['id_aice_obs']:
        if 'OBS_ERROR' in ds:
            ds['OBS_ERROR'] = 0.05 * xr.ones_like(ds[vartype])
    elif id_obs == OBS_IDS['id_hice_obs']:
        if 'OBS_ERROR' in ds:
            ds['OBS_ERROR'] = 0.3 * xr.ones_like(ds[vartype])

    base_mask &= (~ds[vartype].isnull())
    if 'OBS_ERROR' in ds:
        base_mask &= (ds['OBS_ERROR'] < 999.9)

    # --- Only remaining time-dependent step: slice out each cycle's window ---
    has_time = 'DATE_TIME' in ds
    results = []
    for (sd, ed) in windows:
        if has_time:
            time_mask = (ds.DATE_TIME >= sd) & (ds.DATE_TIME <= ed)
            window_mask = base_mask & time_mask
        else:
            window_mask = base_mask

        value = safe_flatten(ds[vartype], window_mask)
        if value.shape[0] == 0:
            results.append(None)
            continue

        obs_data = {'value': value}
        obs_data['lon'] = safe_flatten(ds.LON, window_mask) if 'LON' in ds else np.array([])
        obs_data['lat'] = safe_flatten(ds.LAT, window_mask) if 'LAT' in ds else np.array([])
        obs_data['depth'] = safe_flatten(ds.DEPTH, window_mask) if 'DEPTH' in ds else np.array([])
        obs_data['instid'] = safe_flatten(ds.INST_ID, window_mask) if 'INST_ID' in ds else np.array([])
        obs_data['oerr'] = xsigo * safe_flatten(ds.OBS_ERROR, window_mask) if 'OBS_ERROR' in ds else np.array([])
        obs_data['typ'] = np.full(value.shape, id_obs, dtype=np.int32)
        results.append(obs_data)

    return results


def write_gmao_file(yyyy, mm, dd, hh, valid_observations):
    """Concatenates a list of obs_data dicts and writes a single
    gmao-obs-YYYYMMDDHH.nc file. Shared by generate_nc_batch(); mirrors the
    tail end of generate_nc() exactly."""
    if not valid_observations:
        print(f"No valid observations found for {yyyy}{mm}{dd}{hh}.")
        nobs = 0
        typ = np.array([], dtype=np.int32)
        lon = np.array([], dtype=np.float32)
        lat = np.array([], dtype=np.float32)
        depth = np.array([], dtype=np.float32)
        value = np.array([], dtype=np.float32)
        oerr = np.array([], dtype=np.float32)
        instid = np.array([], dtype=np.float32)
    else:
        typ = np.concatenate([obs['typ'] for obs in valid_observations])
        lon = np.concatenate([obs['lon'] for obs in valid_observations])
        lat = np.concatenate([obs['lat'] for obs in valid_observations])
        depth = np.concatenate([obs['depth'] for obs in valid_observations])
        value = np.concatenate([obs['value'] for obs in valid_observations])
        oerr = np.concatenate([obs['oerr'] for obs in valid_observations])
        instid = np.concatenate([obs['instid'] for obs in valid_observations])

        lon[(lon == 180.) | (lon == -180.)] = -179.9999
        nobs = len(typ)

    print(f"[{yyyy}{mm}{dd}{hh}] Total observations extracted: {nobs}")

    fnameout = f"gmao-obs-{yyyy}{mm}{dd}{hh}.nc"
    with Dataset(fnameout, 'w', format='NETCDF4') as ncfile:
        ncfile.createDimension('nobs', nobs)
        vars_map = {
            'typ': ('int32', typ),
            'lon': ('float32', lon),
            'lat': ('float32', lat),
            'depth': ('float32', depth),
            'value': ('float32', value),
            'oerr': ('float32', oerr),
            'instid': ('float32', instid)
        }
        for var_name, (dtype, data) in vars_map.items():
            var = ncfile.createVariable(var_name, np.dtype(dtype).char, ('nobs',))
            var[:] = data

    print(f"Successfully saved {nobs} obs in {fnameout}")
    return fnameout


def generate_nc_batch(cycle_times, obs_types):
    """
    Batch version of generate_nc(): generates MULTIPLE gmao-obs-*.nc cycles in
    one pass, opening each distinct source file exactly once no matter how
    many cycles draw from it (rather than once per cycle).

    Parameters:
    -----------
    cycle_times : list of (yyyy, mm, dd, hh) string tuples, e.g.
                  [('2026','04','06','06'), ('2026','04','06','12'), ...]
    obs_types   : list of obs type strings (same as generate_nc), e.g. the
                  list read from the $ODAS_OBS environment variable

    Returns:
    --------
    List of output filenames written (bare names, in the cwd), in the same
    order as `cycle_times`.
    """
    n_cycles = len(cycle_times)
    per_cycle_obs = [[] for _ in range(n_cycles)]
    timing = {}  # obs_type (e.g. "Argo") -> accumulated processing seconds
    run_start = time.perf_counter()

    # Default (non-custom_hh) window: one per cycle.
    default_windows = [get_time_window(yyyy, mm, dd, hh, EXP_NDAYS) for (yyyy, mm, dd, hh) in cycle_times]

    # Cycles sharing the same calendar day, for custom_hh sources (e.g. L3
    # products pinned to 12z) which reuse ONE window for every cycle in a day.
    day_to_cycle_idxs = {}
    for idx, (yyyy, mm, dd, hh) in enumerate(cycle_times):
        day_to_cycle_idxs.setdefault((yyyy, mm, dd), []).append(idx)

    for doobs in obs_types:
        if doobs not in OBS_MAP:
            print(f"Warning: '{doobs}' is not recognized in the OBS_MAP.")
            continue

        configs = OBS_MAP[doobs]
        for (fname_tpl, id_obs, vartype, xsigo, descriptor, platform, custom_hh) in configs:
            if custom_hh is None:
                # Group cycles by their resolved filename so each distinct
                # source file (e.g. one per year) is still opened only once,
                # even though the filename template only depends on yyyy/mm/dd.
                fname_to_idxs = {}
                for idx, (yyyy, mm, dd, hh) in enumerate(cycle_times):
                    fname = fname_tpl.format(yyyy=yyyy, mm=mm, dd=dd)
                    fname_to_idxs.setdefault(fname, []).append(idx)

                for fname, idxs in fname_to_idxs.items():
                    windows = [default_windows[i] for i in idxs]
                    t0 = time.perf_counter()
                    results = process_observation_multi(fname, id_obs, vartype, xsigo, descriptor, platform, windows)
                    elapsed = time.perf_counter() - t0
                    timing[doobs] = timing.get(doobs, 0.0) + elapsed
                    print(f"  [{descriptor}] done in {elapsed:.2f}s (serving {len(idxs)} cycle(s))")
                    for i, obs_data in zip(idxs, results):
                        if obs_data is not None:
                            per_cycle_obs[i].append(obs_data)
            else:
                # Pinned-hour sources: one window per calendar day, shared by
                # every cycle that falls on that day.
                for (yyyy, mm, dd), idxs in day_to_cycle_idxs.items():
                    fname = fname_tpl.format(yyyy=yyyy, mm=mm, dd=dd)
                    window = get_time_window(yyyy, mm, dd, custom_hh, EXP_NDAYS)
                    t0 = time.perf_counter()
                    results = process_observation_multi(fname, id_obs, vartype, xsigo, descriptor, platform, [window])
                    elapsed = time.perf_counter() - t0
                    timing[doobs] = timing.get(doobs, 0.0) + elapsed
                    print(f"  [{descriptor}] done in {elapsed:.2f}s (serving {len(idxs)} cycle(s) on {yyyy}{mm}{dd})")
                    obs_data = results[0]
                    if obs_data is not None:
                        for i in idxs:
                            per_cycle_obs[i].append(obs_data)

    print_timing_summary(timing, time.perf_counter() - run_start)

    # --- Write one output file per cycle ---
    output_paths = []
    for i, (yyyy, mm, dd, hh) in enumerate(cycle_times):
        output_paths.append(write_gmao_file(yyyy, mm, dd, hh, per_cycle_obs[i]))

    return output_paths


def generate_nc(yyyy, mm, dd, hh, obs_types):
    """Parses observations and generates the output NetCDF dataset."""
    valid_observations = []
    timing = {}  # obs_type (e.g. "Argo") -> accumulated processing seconds
    run_start = time.perf_counter()

    for doobs in obs_types:
        if doobs not in OBS_MAP:
            print(f"Warning: '{doobs}' is not recognized in the OBS_MAP.")
            continue

        configs = OBS_MAP[doobs]
        for (fname_tpl, id_obs, vartype, xsigo, descriptor, platform, custom_hh) in configs:
            # Interpolate formatting variables into the filename
            fname = fname_tpl.format(yyyy=yyyy, mm=mm, dd=dd)
            # Use custom hour if required (e.g. 12 for L3 data)
            obs_hh = custom_hh if custom_hh is not None else hh

            t0 = time.perf_counter()
            obs_data = process_observation(
                fname=fname, id_obs=id_obs, vartype=vartype, xsigo=xsigo,
                descriptor=descriptor, platform=platform, yyyy=yyyy, mm=mm, dd=dd, hh=obs_hh
            )
            elapsed = time.perf_counter() - t0
            timing[doobs] = timing.get(doobs, 0.0) + elapsed
            print(f"  [{descriptor}] done in {elapsed:.2f}s")

            if obs_data is not None:
                valid_observations.append(obs_data)

    print_timing_summary(timing, time.perf_counter() - run_start)

    if not valid_observations:
        print("No valid observations found.")
        nobs = 0
        typ = np.array([], dtype=np.int32)
        lon = np.array([], dtype=np.float32)
        lat = np.array([], dtype=np.float32)
        depth = np.array([], dtype=np.float32)
        value = np.array([], dtype=np.float32)
        oerr = np.array([], dtype=np.float32)
        instid = np.array([], dtype=np.float32)
    else:
        # Fast concatenations using numpy
        typ = np.concatenate([obs['typ'] for obs in valid_observations])
        lon = np.concatenate([obs['lon'] for obs in valid_observations])
        lat = np.concatenate([obs['lat'] for obs in valid_observations])
        depth = np.concatenate([obs['depth'] for obs in valid_observations])
        value = np.concatenate([obs['value'] for obs in valid_observations])
        oerr = np.concatenate([obs['oerr'] for obs in valid_observations])
        instid = np.concatenate([obs['instid'] for obs in valid_observations])

        # Correct longitude boundaries rapidly utilizing boolean indexing
        lon[(lon == 180.) | (lon == -180.)] = -179.9999
        nobs = len(typ)

    print(f"Total observations extracted: {nobs}")

    fnameout = f"gmao-obs-{yyyy}{mm}{dd}{hh}.nc"
    print(f"Writing to output file: {fnameout}")

    # Write variables utilizing NetCDF
    with Dataset(fnameout, 'w', format='NETCDF4') as ncfile:
        ncfile.createDimension('nobs', nobs)

        vars_map = {
            'typ': ('int32', typ),
            'lon': ('float32', lon),
            'lat': ('float32', lat),
            'depth': ('float32', depth),
            'value': ('float32', value),
            'oerr': ('float32', oerr),
            'instid': ('float32', instid)
        }

        for var_name, (dtype, data) in vars_map.items():
            var = ncfile.createVariable(var_name, np.dtype(dtype).char, ('nobs',))
            var[:] = data

    print(f"Successfully saved {nobs} obs in {fnameout}")


if __name__ == "__main__":
    if len(sys.argv) < 6:
        print("Usage: python generate_gmao_nc_optimized.py yyyy mm dd hh obs_type [obs_type ...]")
        sys.exit(1)

    yyyy = sys.argv[1]
    mm = sys.argv[2]
    dd = sys.argv[3]
    hh = sys.argv[4]
    obs_types = sys.argv[5:]

    generate_nc(yyyy, mm, dd, hh, obs_types)
