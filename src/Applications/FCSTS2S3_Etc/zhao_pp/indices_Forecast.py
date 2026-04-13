import matplotlib
# matplotlib.use('Agg')  # enable if want non-interactive backend
import numpy as np
import matplotlib.pyplot as plt
from netCDF4 import Dataset
import glob
import os
import sys
import re
from pathlib import Path
from collections import defaultdict

# ------------------------------------------------------------
# Configuration
# ------------------------------------------------------------
N_LEADS = 11  # stored series length = [NaN, NaN, lead2..lead10]

regions = {
    'nino1':   (-10,  5,  -90,  -80),
    'nino2':   ( -5,  0,  -90,  -80),
    'nino1+2': (-10,  0,  -90,  -80),
    'idm_west':(-10, 10,   50,   70),
    'idm_east':(-10,  0,   90,  110),
    'idm':     None,                 # computed as west - east
    'nat':     (  0, 20,  -40,  -20),
    'sat':     (-20,  0,  -40,  -20),
    'tasi':    (  0, 20,   50,   70),
    'nino3.4': ( -5,  5, -170, -120),
    'nino3':   ( -5,  5, -150,  -90),
    'nino4':   ( -5,  5,  160, -150),
}

# Base paths
filebaseIN1 = '/nobackup/knakada/GEOSS2S3/GEOS_fcst4nrt/'
filebaseIN2 = '/nobackup/knakada/GEOSS2S3/GEOS_fcst4nrt/winners/'
filebaseDRFT = '/nobackup/zli7/V3_postprocessing/DRFT/DRFT_2001_2020/'
outputdir_txt = '/nobackup/zli7/V3_postprocessing/IRI/'
collIC = 'ocn_tavg_1mo_glo_L720x361_slv'
samplefile = '/nobackup/zli7/V3_runx/19910101/ens1/ocn_tavg_1mo_glo_L720x361_slv/19910101.ocn_tavg_1mo_glo_L720x361_slv.19910101_0000z.nc4'

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------
ENS_RE = re.compile(r"/ens(\d+)(?:/|$)")

def extract_ens_num_from_path(p: str):
    m = ENS_RE.search(p)
    return int(m.group(1)) if m else None

def extract_inidate_from_path(p: str, follow_symlinks: bool = True):
    parts = Path(p).resolve().parts if follow_symlinks else Path(p).parts
    for seg in parts:
        if len(seg) == 8 and seg.isdigit():
            return seg
    return None

def numeric_sort_ens(paths):
    return sorted(paths, key=lambda s: extract_ens_num_from_path(s) or 10**9)

def convert_longitude(lon_min, lon_max):
    if lon_min > 180: lon_min -= 360
    if lon_max > 180: lon_max -= 360
    return lon_min, lon_max

def get_region_indices(latitudes, longitudes, lat_min, lat_max, lon_min, lon_max):
    latitudes = np.asarray(latitudes)
    longitudes = np.asarray(longitudes)
    lat_idx = np.where((latitudes >= lat_min) & (latitudes <= lat_max))[0]
    if lon_min > lon_max:
        lon_idx = np.where((longitudes >= lon_min) | (longitudes <= lon_max))[0]
    else:
        lon_idx = np.where((longitudes >= lon_min) & (longitudes <= lon_max))[0]
    return lat_idx, lon_idx

def read_data(filename):
    try:
        with Dataset(filename, 'r') as nc:
            latitudes  = nc.variables['lat'][:]
            longitudes = nc.variables['lon'][:]
            vn = 'BULK_OCEANTEMP'
            try:
                data = nc.variables[vn][:]
            except KeyError:
                print(f"Variable {vn} not found in {filename}")
                return latitudes, longitudes, None
            return latitudes, longitudes, data
    except FileNotFoundError:
        return None, None, None

def safe_diff(a: np.ndarray, b: np.ndarray) -> np.ndarray:
    """Elementwise (a - b) but NaN if either side is NaN."""
    out = np.full_like(a, np.nan)
    mask = np.isfinite(a) & np.isfinite(b)
    out[mask] = a[mask] - b[mask]
    return out

# ------------------------------------------------------------
# Discovery
# ------------------------------------------------------------
def discover_early_ens_map(inputYR: str, inputMON: str):
    """
    Find all early members under GEOS_fcst4nrt/YYYYMM??/ens*/.
    Returns dict: ensK -> list[(ens_dir, init_date)]
    """
    pattern = filebaseIN1 + inputYR + inputMON.zfill(2) + '??/*/'
    all_dirs = glob.glob(pattern)
    ens_map = defaultdict(list)
    max_ens = 0
    for d in all_dirs:
        k = extract_ens_num_from_path(d)
        if not k:
            continue
        datelbl = extract_inidate_from_path(d, follow_symlinks=False)
        if not datelbl:
            continue
        ens_map[k].append((d, datelbl))
        max_ens = max(max_ens, k)
    return ens_map, max_ens

def discover_winner_symlinks(inputYR: str, inputMON: str):
    """
    Find all winners under winners/YYYYMM/ens0??/.
    Returns list of dicts for each symlink.
    """
    pattern = filebaseIN2 + inputYR + inputMON.zfill(2) + '/ens0' + '??/'
    win_dirs = numeric_sort_ens(glob.glob(pattern))
    winners_map = []
    for win_dir in win_dirs:
        symlink_name = Path(win_dir.rstrip('/')).name
        realp   = os.path.realpath(win_dir)
        k       = extract_ens_num_from_path(realp)
        datelbl = extract_inidate_from_path(realp)
        winners_map.append({
            'symlink': symlink_name,
            'src_ens': k,
            'datelbl': datelbl,
            'win_dir': win_dir,
            'realpath': realp,
        })
    return winners_map

# ------------------------------------------------------------
# Core per-region series builder
# ------------------------------------------------------------
def compute_region_series(inputYR: str, inputMON: str, reg: str):
    """
    For a single *physical* region (not 'idm'), build:
      early_series[(init_date, ensK)] = length-11 anomaly vector [NaN,NaN,v2,v3,NaN...]
      winners_series['ens0xx']       = length-11 anomaly vector [NaN,NaN,v2..v10]
    Also returns labeling arrays fcstYRs and rolling_months for saving.
    """
    if reg == 'idm':
        raise ValueError("compute_region_series expects a physical region, not 'idm'.")

    # Calendar helpers
    monICn = int(inputMON); yearICn = int(inputYR)
    fcstMOs = ['{:02d}'.format((monICn - 2 + i) % 12 + 1) for i in range(N_LEADS + 2)]
    fcstYRs = [str(yearICn + (monICn - 2 + i) // 12) for i in range(N_LEADS + 2)]
    releaseMO = ['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
    start_index = (monICn - 1 - 1) % 12
    rolling_months = [releaseMO[(start_index + i) % 12] for i in range(11)]
    release = rolling_months[2]  # e.g. 'aug' when InputMON=7
    print(f"[{reg}] release tag: {release}")

    # Region grid
    if regions.get(reg) is None:
        raise ValueError(f"Region '{reg}' is not defined.")
    latitudes, longitudes, _ = read_data(samplefile)
    if latitudes is None or longitudes is None:
        raise RuntimeError(f"Failed to read lat/lon: {samplefile}")
    longitudes = np.where(longitudes > 180, longitudes - 360, longitudes)
    lat_min, lat_max, lon_min_orig, lon_max_orig = regions[reg]
    lon_min, lon_max = convert_longitude(lon_min_orig, lon_max_orig)
    lat_idx, lon_idx = get_region_indices(latitudes, longitudes, lat_min, lat_max, lon_min, lon_max)

    # Discover members
    ens_map, max_ens_early = discover_early_ens_map(inputYR, inputMON)
    winners_map = discover_winner_symlinks(inputYR, inputMON)

    # Arrays for diagnostics (by true ens row)
    totalens = max(max_ens_early, 1)
    Hplumes = np.full((totalens, N_LEADS), np.nan)
    anomaly_indices = np.full((totalens, N_LEADS), np.nan)
    drift_indices = np.full((1, N_LEADS), np.nan)

    # Leads usage
    EARLY_LEADS  = [2, 3]
    WINNER_LEADS = list(range(2, 11))

    # Load drift for relevant leads
    for lead in sorted(set(EARLY_LEADS + WINNER_LEADS)):
        fcstMO = fcstMOs[lead]; fcstYR = fcstYRs[lead]
        filenameDRFT = f'{filebaseDRFT}/{collIC}/{release}/{release}.{collIC}.monthly.drift.{fcstMO}.nc4'
        _, _, drift_data = read_data(filenameDRFT)
        if drift_data is not None:
            drift_data = np.ma.filled(drift_data, np.nan)
            drift_slice = drift_data[:, lat_idx, :][:, :, lon_idx]
            drift_indices[0, lead] = np.nanmean(drift_slice)

    # Build series
    early_series   = {}  # (date, ensK) -> vec(11)
    winners_series = {}  # 'ens0xx' -> vec(11)

    # EARLY (all found)
    for ens_num, entries in ens_map.items():
        for ens_dir, datelbl in entries:
            key = (datelbl, ens_num)
            vec = np.full(N_LEADS, np.nan, dtype=float)
            for lead in EARLY_LEADS:
                fcstMO = fcstMOs[lead]; fcstYR = fcstYRs[lead]
                f = f'{ens_dir}{collIC}/{datelbl}.{collIC}.{fcstYR}{fcstMO}01_0000z.nc4'
                _, _, fc = read_data(f)
                if fc is None or not np.isfinite(drift_indices[0, lead]):
                    continue
                fc = np.ma.filled(fc, np.nan)
                sub = fc[:, lat_idx, :][:, :, lon_idx]
                val = np.nanmean(sub)
                if np.isfinite(val):
                    vec[lead] = val - drift_indices[0, lead]
            early_series[key] = vec

            # diagnostics (mean back to full)
            row = ens_num - 1
            if 0 <= row < totalens:
                vals_full = []
                for lead in EARLY_LEADS:
                    if np.isfinite(vec[lead]) and np.isfinite(drift_indices[0, lead]):
                        vals_full.append(vec[lead] + drift_indices[0, lead])
                if vals_full:
                    mf = float(np.nanmean(vals_full))
                    for lead in EARLY_LEADS:
                        Hplumes[row, lead] = mf
                        anomaly_indices[row, lead] = mf - drift_indices[0, lead]

    # WINNERS (all found)
    for w in winners_map:
        sid     = w['symlink']
        realp   = w['realpath']
        datelbl = w['datelbl']
        src_ens = w['src_ens']
        vec = winners_series.get(sid, np.full(N_LEADS, np.nan, dtype=float))
        if datelbl and src_ens:
            row = src_ens - 1
            for lead in WINNER_LEADS:
                fcstMO = fcstMOs[lead]; fcstYR = fcstYRs[lead]
                f = f'{w["win_dir"]}{collIC}/{datelbl}.{collIC}.{fcstYR}{fcstMO}01_0000z.nc4'
                _, _, fc = read_data(f)
                if fc is None or not np.isfinite(drift_indices[0, lead]):
                    continue
                fc = np.ma.filled(fc, np.nan)
                sub = fc[:, lat_idx, :][:, :, lon_idx]
                val = np.nanmean(sub)
                if np.isfinite(val):
                    vec[lead] = val - drift_indices[0, lead]
                    if 0 <= row < totalens:
                        Hplumes[row, lead] = val
                        anomaly_indices[row, lead] = val - drift_indices[0, lead]
        winners_series[sid] = vec

    # Diagnostics
    with np.errstate(all='ignore'):
        ensmean_anom = np.nanmean(anomaly_indices, axis=0)
        ensmean_full = np.nanmean(Hplumes, axis=0)
    print(f"[{reg}] ensmean anom[2..10]: {ensmean_anom[2:11]}")
    print(f"[{reg}] ensmean full[2..10]: {ensmean_full[2:11]}")
    print(f"[{reg}] drift[2..10]:        {drift_indices[0,2:11]}")

    # Count early by date
    by_date = defaultdict(int)
    for (d, k) in early_series.keys():
        by_date[d] += 1
    if by_date:
        print(f"[{reg}] Early members by init date: " +
              ", ".join(f"{d}:{by_date[d]}" for d in sorted(by_date)))

    return early_series, winners_series, fcstYRs, rolling_months

# ------------------------------------------------------------
# Save (dynamic count)
# ------------------------------------------------------------
def save_dynamic_members(reg, early_series, winners_series, fcstYRs, rolling_months):
    outdir = f'{outputdir_txt}/{fcstYRs[2]}/{rolling_months[2]}/'
    os.makedirs(outdir, exist_ok=True)

    # Sort keys
    early_keys  = sorted(early_series.keys(), key=lambda k: (k[0], k[1]))  # (date, ensK)
    winner_ids  = sorted(winners_series.keys())                            # ens0xx...

    # Save early
    counter = 1
    for (d, e) in early_keys:
        vec = early_series[(d, e)]
        path = f'{outdir}/ens{counter}_{reg}_{fcstYRs[2]}{rolling_months[2]}_indices_anom.txt'
        np.savetxt(path, vec, fmt='%.4f')
        counter += 1
    early_count = counter - 1

    # Save winners
    for sid in winner_ids:
        vec = winners_series[sid]
        path = f'{outdir}/ens{counter}_{reg}_{fcstYRs[2]}{rolling_months[2]}_indices_anom.txt'
        np.savetxt(path, vec, fmt='%.4f')
        counter += 1
    winners_count = len(winner_ids)

    total = early_count + winners_count
    print(f"[{reg}] Saved {early_count} early + {winners_count} winners = {total} files → {outdir}")

    # Optional: IRI mean across all saved members
    if reg == 'nino3.4':
        stack = []
        for (d, e) in early_keys:
            stack.append(early_series[(d, e)])
        for sid in winner_ids:
            stack.append(winners_series[sid])
        if stack:
            A = np.vstack(stack)
            iri = np.nanmean(A, axis=0)
            iri_path = f'{outdir}/IRI_{reg}_{fcstYRs[2]}{rolling_months[2]}_indices_anom.txt'
            np.savetxt(iri_path, iri, fmt='%.4f')
            print(f"[{reg}] Saved IRI mean: {iri_path}")

# ------------------------------------------------------------
# Driver
# ------------------------------------------------------------
def main_indices_script(inputYR, inputMON, _plot_type, regions_to_process):
    targets = regions_to_process if regions_to_process else regions.keys()
    for reg in targets:
        print(f"Processing region: {reg}")
        try:
            if reg != 'idm':
                # Normal region
                early_series, winners_series, fcstYRs, rolling_months = compute_region_series(inputYR, inputMON, reg)
                save_dynamic_members(reg, early_series, winners_series, fcstYRs, rolling_months)
            else:
                # --- IDM special handling: west - east ---
                # Build each side
                ew, ww, fcstYRs, rolling_months = compute_region_series(inputYR, inputMON, 'idm_west')
                ee, we, _, _                    = compute_region_series(inputYR, inputMON, 'idm_east')

                # Combine EARLY: union of keys (date, ensK)
                early_keys = sorted(set(ew.keys()).union(ee.keys()), key=lambda k: (k[0], k[1]))
                early_series_idm = {}
                for key in early_keys:
                    v_w = ew.get(key, np.full(N_LEADS, np.nan, dtype=float))
                    v_e = ee.get(key, np.full(N_LEADS, np.nan, dtype=float))
                    early_series_idm[key] = safe_diff(v_w, v_e)

                # Combine WINNERS: union of symlink names
                win_keys = sorted(set(ww.keys()).union(we.keys()))
                winners_series_idm = {}
                for sid in win_keys:
                    v_w = ww.get(sid, np.full(N_LEADS, np.nan, dtype=float))
                    v_e = we.get(sid, np.full(N_LEADS, np.nan, dtype=float))
                    winners_series_idm[sid] = safe_diff(v_w, v_e)

                # Save combined
                save_dynamic_members('idm', early_series_idm, winners_series_idm, fcstYRs, rolling_months)

        except Exception as e:
            print(f"An error occurred while processing region {reg}: {e}")

# ------------------------------------------------------------
# CLI
# ------------------------------------------------------------
if __name__ == "__main__":
    if len(sys.argv) < 4:
        print("Usage: python indices_Forecast.py <inputYR> <inputMON> <plot_type> [<regions_to_process>]")
        sys.exit(1)

    inputYR = sys.argv[1]
    inputMON = sys.argv[2]
    plot_type = sys.argv[3]
    regions_to_process = sys.argv[4:] if len(sys.argv) > 4 else None

    main_indices_script(inputYR, inputMON, plot_type, regions_to_process)

