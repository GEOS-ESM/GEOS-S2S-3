import os
import glob
import datetime
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
from collections import defaultdict
from netCDF4 import Dataset

# ==========================================
# 1. CONSTANTS & MAPPINGS
# ==========================================

LAT_COND = {
    'glb': lambda lat: np.abs(lat) < 90.0,
    'trp': lambda lat: np.abs(lat) < 30.0,
    'north': lambda lat: lat > 0.0,
    'south': lambda lat: lat < 0.0
}

OBSID_DICT = {
    'SST': 5525, 
    'SSS': 5522, 
    'ADT': 5351, 
    'Tprof': 3073, 
    'Sprof': 5521, 
    'Ice Fraction': 6000
}

# ==========================================
# 2. CLASS DEFINITIONS
# ==========================================

class OdaStats:
    def __init__(self, fname):
        try:
            basename = os.path.basename(fname)
            self.date = datetime.datetime.strptime(basename, "obs-%Y%m%d_%H.nc")
        except ValueError:
            print(f"Warning: Could not parse date from filename: {fname}")
            self.date = None
            
        with Dataset(fname, 'r') as ncfile:
            self.OBSID = ncfile.variables['OBSID'][:]
            self.lat = ncfile.variables['lat'][:]
            self.lev = ncfile.variables['lev'][:]
            self.omf = ncfile.variables['omf'][:]
            self.fcst = ncfile.variables['obs'][:] - self.omf
            
            if 'oma' in ncfile.variables:
                self.oma = ncfile.variables['oma'][:]
                self.OBSIDa = ncfile.variables['OBSIDa'][:]
                self.lata = ncfile.variables['lata'][:]
                self.leva = ncfile.variables['leva'][:]
                self.fcsta = ncfile.variables['obsa'][:] - self.oma
            else:
                self.oma = np.full_like(self.omf, np.nan)

    def mae(self, obstype=3073, region='glb', obsname='Tprof', lev2=1000.0):
        """Compute MAE and Bias for requested region and depth."""
        thresholds = {'Tprof': 10.0, 'Sprof': 5.0, 'ADT': 1.0, 'SSS': 1.0}
        toobig = 5.0 if (region == 'glb' and obsname == 'Tprof') else thresholds.get(obsname, 10.0)

        lat_cond = LAT_COND[region]

        # Process OMF
        v_omf = (self.OBSID == obstype) & (np.abs(self.lev) < float(lev2)) & (self.fcst != 0.0) & lat_cond(self.lat) & (np.abs(self.omf) < toobig)
        
        if np.any(v_omf):
            mae_omf = np.mean(np.abs(self.omf[v_omf]))
            bias_omf = np.mean(self.omf[v_omf])
        else:
            mae_omf, bias_omf = np.nan, np.nan

        # Process OMA
        if hasattr(self, 'OBSIDa'):
            v_oma = (self.OBSIDa == obstype) & (np.abs(self.leva) < float(lev2)) & (self.fcsta != 0.0) & lat_cond(self.lata) & (np.abs(self.oma) < toobig)
            if np.any(v_oma):
                mae_oma = np.mean(np.abs(self.oma[v_oma]))
                bias_oma = np.mean(self.oma[v_oma])
            else:
                mae_oma, bias_oma = np.nan, np.nan
        else:
            mae_oma, bias_oma = np.nan, np.nan
            
        return self.date, mae_omf, mae_oma, bias_omf, bias_oma

# ==========================================
# 3. MAIN PROCESSING & PLOTTING
# ==========================================

def main(obsnames, region, lev1, lev2, base_path, yyyy='202?', mm='??', dd='??', hh='??'):
    path2files = f"{base_path}/ocean_das/oana-????????_00/ocean_observer_*/obs-{yyyy}{mm}{dd}_{hh}.nc"
    flist = sorted(glob.glob(path2files))
    
    if not flist:
        raise FileNotFoundError(f"No files found matching: {path2files}")

    print(f"Extracting data from {len(flist)} files...")
    results_by_obs = defaultdict(list)
    
    for fname in flist:
        stats = OdaStats(fname)
        for obsname in obsnames:
            obstype = OBSID_DICT[obsname]
            res = stats.mae(obstype, region, obsname, lev2)
            
            if np.isfinite(res[1]) and np.isfinite(res[2]):
                results_by_obs[obsname].append(res)

    valid_obsnames = [obs for obs in obsnames if results_by_obs[obs]]
    
    if not valid_obsnames:
        print("No valid data found for any observation type. Exiting.")
        return

    last_date = results_by_obs[valid_obsnames[0]][-1][0]
    sdate = last_date.strftime('%Y-%m-%d') if last_date else "unknown_date"

    # --- Plotting Layout with Adjusted GridSpec ---
    num_obs = len(valid_obsnames)
    
    # Increased height per obs group to allow for more spacing
    fig = plt.figure(figsize=(10, 4.0 * num_obs))
    
    # Increased outer hspace to separate groups
    outer_grid = gridspec.GridSpec(num_obs, 1, hspace=0.8)
    
    all_axes = []
    first_ax = None
    
    for i, obsname in enumerate(valid_obsnames):
        valid_data = results_by_obs[obsname]
        ymdh, mae_omf, mae_oma, bias_omf, bias_oma = map(list, zip(*valid_data))
        
        # Increased inner hspace to prevent BIAS title overlapping MAE plot
        inner_grid = gridspec.GridSpecFromSubplotSpec(2, 1, subplot_spec=outer_grid[i], hspace=0.45)
        
        if first_ax is None:
            ax_mae = fig.add_subplot(inner_grid[0])
            first_ax = ax_mae
        else:
            ax_mae = fig.add_subplot(inner_grid[0], sharex=first_ax)
            
        ax_bias = fig.add_subplot(inner_grid[1], sharex=first_ax)
        all_axes.extend([ax_mae, ax_bias])
        
        # --- Plotting Lines ---
        ax_mae.plot(ymdh, mae_omf, label='MAE OMF', color='#d62728', linestyle='-', linewidth=1.5, alpha=0.85)
        ax_mae.plot(ymdh, mae_oma, label='MAE OMA', color='#1f77b4', linestyle='-', linewidth=1.2, alpha=0.85)
        
        ax_bias.plot(ymdh, bias_omf, label='Bias OMF', color='#ff7f0e', linestyle='-', linewidth=1.5, alpha=0.85)
        ax_bias.plot(ymdh, bias_oma, label='Bias OMA', color='#2ca02c', linestyle='-', linewidth=1.2, alpha=0.85)
        ax_bias.axhline(0, color='black', linewidth=0.8, linestyle='--', alpha=0.4) 

        # --- Titles and Styling ---
        depth_str = f" 0-{int(lev2)}m" if obsname in ['Tprof', 'Sprof'] else ""
        group_title = f"{obsname} {region}{depth_str}"
        
        ax_mae.text(
            0.5, 1.35, group_title, 
            transform=ax_mae.transAxes, 
            ha='center', va='bottom', 
            fontsize=12, fontweight='bold'
        )
        
        ax_mae.set_title("MAE (Mean Absolute Error of OMF/OMA)", fontsize=11, fontweight='bold', pad=10)
        ax_bias.set_title("BIAS (Mean of the OMF/OMA)", fontsize=11, fontweight='bold', pad=10)

        if i == 0:
            ax_mae.text(
                0.98, 0.92, f'Last Date: {sdate}', 
                transform=ax_mae.transAxes, 
                ha='right', va='top', 
                fontsize=11, color='darkblue', fontweight='bold'
            )
        
        ax_mae.set_ylabel("Metric Value", fontsize=10)
        ax_bias.set_ylabel("Metric Value", fontsize=10)

        for ax in (ax_mae, ax_bias):
            ax.grid(True, linestyle=':', alpha=0.5, color='gray')
            ax.spines['top'].set_visible(False)
            ax.spines['right'].set_visible(False)
            ax.margins(x=0.01)

    for ax in all_axes[:-1]:
        plt.setp(ax.get_xticklabels(), visible=False)

    # --- Global Legend ---
    handles_mae, labels_mae = all_axes[0].get_legend_handles_labels()
    handles_bias, labels_bias = all_axes[1].get_legend_handles_labels()
    
    # Adjusted legend y-position to avoid the lifted titles
    fig.legend(
        handles_mae + handles_bias, 
        labels_mae + labels_bias,
        loc='upper center', 
        bbox_to_anchor=(0.5, 0.98), 
        ncol=4, 
        frameon=False, 
        fontsize=10
    )
        
    all_axes[-1].set_xlim([ymdh[0], ymdh[-1]])
    all_axes[-1].xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d'))
    
    all_axes[-1].set_xlabel("Date", fontsize=10)
    fig.autofmt_xdate(rotation=30)
    fig.subplots_adjust(left=0.12, right=0.95, top=0.88, bottom=0.08, hspace=0.8) 
    output_filename = f"stats_combined.png"
    fig.savefig(output_filename, dpi=300)
    print(f"Plot saved successfully to {output_filename}")

# ==========================================
# 4. EXECUTION & CONFIGURATION
# ==========================================

if __name__ == "__main__":
    CONFIG_OBSNAMES = ['Tprof', 'Sprof', 'ADT'] 
    CONFIG_REGION   = 'glb'
    CONFIG_LEV1     = 0.0
    CONFIG_LEV2     = 300.0
    
    BASE_PATH       = '/gpfsm/dnb07/projects/p236/GiOcean-NRT'
    YYYY, MM, DD, HH = '202?', '??', '??', '12'
    
    main(
        obsnames=CONFIG_OBSNAMES,
        region=CONFIG_REGION,
        lev1=CONFIG_LEV1,
        lev2=CONFIG_LEV2,
        base_path=BASE_PATH,
        yyyy=YYYY, 
        mm=MM, 
        dd=DD, 
        hh=HH
    )
