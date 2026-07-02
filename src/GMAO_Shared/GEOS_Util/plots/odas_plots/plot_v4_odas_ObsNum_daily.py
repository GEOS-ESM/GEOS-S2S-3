import os
import glob
import datetime
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from netCDF4 import Dataset

# ==========================================
# 1. MAIN PROCESSING & PLOTTING
# ==========================================

def main(base_path, max_depth, yyyy='202?', mm='??', dd='??', hh='??'):
    """Extract observation counts and plot time series."""
    
    path2files = f"{base_path}/ocean_das/oana-????????_00/ocean_observer_*/obs-{yyyy}{mm}{dd}_{hh}.nc"
    flist = sorted(glob.glob(path2files))

    if not flist:
        raise FileNotFoundError(f"No files found matching: {path2files}")

    print(f"Extracting observation counts from {len(flist)} files...")
    
    # Initialize lists to store data
    ymdh = []
    tz_count = []
    sz_count = []
    ssh_count = []
    sss_count = []
    
    for fname in flist:
        # Robustly parse date from filename
        basename = os.path.basename(fname)
        try:
            dt = datetime.datetime.strptime(basename, "obs-%Y%m%d_%H.nc")
        except ValueError:
            print(f"Warning: Could not parse date from {fname}. Skipping.")
            continue
            
        with Dataset(fname, 'r') as ncfile:
            obsid = ncfile.variables['OBSID'][:]
            lev = ncfile.variables['lev'][:]
            
            # Apply depth filter using numpy vectorization
            valid_idx = np.where(lev <= max_depth)
            obsid_filtered = obsid[valid_idx]
            
            # Vectorized counting (much faster than looping)
            tz_count.append(np.sum(obsid_filtered == 3073))  # Temperature
            sz_count.append(np.sum(obsid_filtered == 5521))  # Salinity
            ssh_count.append(np.sum(obsid_filtered == 5351)) # SSH
            sss_count.append(np.sum(obsid_filtered == 5522)) # SSS
            
            ymdh.append(dt)

    if not ymdh:
        print("No valid data extracted. Exiting.")
        return

    # Extract last date for annotation
    last_date = ymdh[-1].strftime('%Y-%m-%d')

    # ==========================================
    # 2. PLOTTING
    # ==========================================
    
    fig, axes = plt.subplots(nrows=3, ncols=1, figsize=(10, 8), sharex=True)
    ax1, ax2, ax3 = axes

    # --- Subplot 1: T & S Profiles ---
    ax1.plot(ymdh, tz_count, label='Temperature', color='#d62728', linestyle='-', linewidth=1.5, alpha=0.85)
    ax1.plot(ymdh, sz_count, label='Salinity', color='#1f77b4', linestyle='-', linewidth=1.5, alpha=0.85)
    ax1.set_title(f'Temperature and Salinity Profiles (0-{int(max_depth)}m)', fontsize=11, fontweight='bold')
    ax1.legend(loc='upper left', frameon=False, fontsize=10)

    # --- Subplot 2: SSS ---
    ax2.plot(ymdh, sss_count, label='SSS', color='#1f77b4', linestyle='-', linewidth=1.5, alpha=0.85)
    ax2.set_title('Sea Surface Salinity (SMAP & SMOS)', fontsize=11, fontweight='bold')
    ax2.legend(loc='upper left', frameon=False, fontsize=10)

    # --- Subplot 3: SSH ---
    ax3.plot(ymdh, ssh_count, label='SSH', color='#1f77b4', linestyle='-', linewidth=1.5, alpha=0.85)
    ax3.set_title('Sea Level Height (Available Satellites)', fontsize=11, fontweight='bold')
    ax3.legend(loc='upper left', frameon=False, fontsize=10)

    # --- Global Formatting ---
    # Add "Last Date" annotation to the top right of the first panel
    ax1.text(
        0.98, 0.92, f'Last Date: {last_date}', 
        transform=ax1.transAxes, 
        ha='right', va='top', 
        fontsize=11, color='darkblue', fontweight='bold'
    )

    for ax in axes:
        ax.grid(True, linestyle=':', alpha=0.5, color='gray')
        ax.spines['top'].set_visible(False)
        ax.spines['right'].set_visible(False)
        ax.set_ylabel("Count", fontsize=10)
        ax.margins(x=0.01)

    axes[-1].set_xlabel("Date", fontsize=10)

    axes[-1].set_xlim([ymdh[0], ymdh[-1]])
    axes[-1].xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d'))
    fig.autofmt_xdate(rotation=30)
    
    # Use the exact same left and right values as the MAE/BIAS script
    fig.subplots_adjust(left=0.12, right=0.95, top=0.92, bottom=0.08, hspace=0.4)

    output_filename = 'stats_odas_obs_v3_modern.png'
    fig.savefig(output_filename, dpi=300)

# ==========================================
# 3. EXECUTION & CONFIGURATION
# ==========================================

if __name__ == "__main__":
    # ----------------------------------------
    # User-Modifiable Configuration
    # ----------------------------------------
    BASE_PATH = '/gpfsm/dnb07/projects/p236/GiOcean-NRT'
    MAX_DEPTH = 300.0  # Depth limit for profiles
    
    # Date variables
    YYYY = '202?'
    MM   = '??'
    DD   = '??'
    HH   = '12'
    
    main(
        base_path=BASE_PATH,
        max_depth=MAX_DEPTH,
        yyyy=YYYY, 
        mm=MM, 
        dd=DD, 
        hh=HH
    )