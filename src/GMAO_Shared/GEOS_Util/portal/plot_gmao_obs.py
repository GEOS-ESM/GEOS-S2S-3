"""
Plotting support for ODAS experiment diagnostics.

This module builds the daily observation maps and observation-count time
series used by run_odas_workflow.py. Input files are normally an experiment's
own production gmao-obs-*.nc output (the primary plot-and-publish workflow),
but may also come from standalone generation (the secondary workflow).
Counts-history and experiment-file discovery helpers used by both paths live
here as well.

The Cartopy data directory comes from $CARTOPY_DATA_DIR. It points to a
pre-populated Natural Earth shapefile cache on the host that produces the
plots. This is a shared group directory that users cannot write to, so it is
never created or modified here.
"""

import os

import xarray as xr
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import cartopy


def _configure_cartopy_data_dir(required=False):
    """Points Cartopy at $CARTOPY_DATA_DIR, a pre-populated shared cache.

    The directory is not created: it is a group directory without user write
    permission, and Cartopy's Natural Earth shapefiles are expected to be
    present there already.

    The encompassing workflow sets CARTOPY_DATA_DIR. When running standalone
    in csh, set it with:
        setenv CARTOPY_DATA_DIR /discover/nobackup/projects/gmao/SIteam/CartopyDownloads
    """
    value = os.environ.get('CARTOPY_DATA_DIR')
    if not value:
        if required:
            raise SystemExit(
                'CARTOPY_DATA_DIR environment variable is not set or is empty. '
                'In csh, set it with: setenv CARTOPY_DATA_DIR '
                '/discover/nobackup/projects/gmao/SIteam/CartopyDownloads'
            )
        return
    if required and not os.path.isdir(value):
        raise SystemExit(
            f'CARTOPY_DATA_DIR does not exist or is not a directory: {value}. '
            'It is a shared, read-only cache that must already contain '
            'Cartopy\'s Natural Earth shapefiles; it is not created here.'
        )
    cartopy.config['data_dir'] = value


# Configure at import when the variable is available, so help/import still work
# when it is not set; plot_obs() enforces it before drawing a map.
_configure_cartopy_data_dir(required=False)

import cartopy.crs as ccrs
import cartopy.feature as cfeature
import calendar
import csv
import glob
import re
from datetime import datetime, timedelta


# Reference dictionaries mapping standard names to observation IDs
OBS_IDS = {
    'salt': 5521, # Salinity
    'temp': 3073, # Temperature
    'sst': 5525,  # Sea Surface Temperature
    'sss': 5522,  # Sea Surface Salinity
    'ssh': 5526,  # SSH
    'adt': 5351,  # Absolute Dynamic Topography
    'aice': 6000, # Sea Ice Concentration
    'hice': 6001  # Sea Ice Thickness
}

# Reverse lookup for titles
INV_OBS_IDS = {v: k.upper() for k, v in OBS_IDS.items()}

# Instrument ID mapping
INST_IDS = {
    501: "TAO",
    502: "PIRATA",
    503: "XBT",
    504: "RAMA",
    508: "Argo",
    513: "CTD",
    510: "GEOSAT-2",
    511: "ERS-1",
    512: "Envisat",
    514: "TOPEX",
    515: "Jason-1",
    517: "Jason-2",
    532: "CryoSat-2",
    543: "CryoSat-2-N",
    533: "Envisat-N",
    534: "Saral/Altika",
    535: "Jason-1G",
    536: "ERS-2",
    537: "HY-2A",
    538: "Jason-1-N",
    539: "TOPEX-N",
    540: "Jason-2-N",
    541: "Jason-3",
    545: "Jason-3N",
    542: "Sentinel-3a",
    547: "Sentinel-3b",
    544: "Sentinel-6a",
    546: "SWOT-nadir",
    555: "SMOS",
    556: "SMAPV5.0",
    553: "AQUARIUS",
    550: "AQUARIUSL3",
    521: "Levitus SSS",
    518: "NASA-TEAM-2",
    519: "CMIP5 AICE",
    520: "CMIP5 SST",
    523: "NOAA-AICE",
    526: "OSTIA-AICE",
    528: "PIOMAS HICE",
    527: "O1B HICE",
    529: "O1B VICE",
    530: "Cryosat HICE",
    531: "XBT-SYN-S",
    557: "WOA18_atArgo",
    558: "WOA18_atNA",
    516: "M2-SST"
}

# Reverse lookup so instrument names (e.g. 'TAO', 'Argo') can be passed as strings
REVERSE_INST_IDS = {v.lower(): k for k, v in INST_IDS.items()}

def _format_input_list(input_val, mapping_dict=None):
    """Helper to convert input to a list of integers."""
    if input_val is None:
        return None
    if not isinstance(input_val, (list, tuple, np.ndarray)):
        input_val = [input_val]
        
    out_list = []
    for val in input_val:
        if isinstance(val, str) and mapping_dict:
            val = mapping_dict.get(val.lower(), val)
        try:
            out_list.append(int(val))
        except ValueError:
            pass
    return out_list

def extract_date_from_filename(filename):
    """Extracts date strings from filename returning a datetime-like string."""
    # Matches YYYYMMDD or YYYYMMDDHH or YYYYMMDD_HH
    match = re.search(r'(\d{8})(?:_)?(\d{2})?', filename)
    if match:
        date_str = match.group(1)
        yyyy, mm, dd = date_str[:4], date_str[4:6], date_str[6:8]
        if match.group(2):
            hh = match.group(2)
            return f"{yyyy}-{mm}-{dd} {hh}:00", f"{yyyy}{mm}{dd}{hh}"
        else:
            return f"{yyyy}-{mm}-{dd}", f"{yyyy}{mm}{dd}"
    return None, None


def da_day_for_file(filepath):
    """Returns the DA day (date) a cycle file belongs to, or None.

    A DA day runs 06Z -> next-day 00Z: the 00Z cycle closes the preceding
    day's 6-hourly window, so it belongs to that day, not to the date in its
    filename or `ocean_observer_YYYYMMDD_00` directory name.
    """
    m = re.search(r'ocean_observer_(\d{8})_(\d{2})', filepath)
    if m:
        day = datetime.strptime(m.group(1), '%Y%m%d').date()
        hh = m.group(2)
    else:
        d_str, d_sort = extract_date_from_filename(os.path.basename(filepath))
        if not d_sort:
            return None
        day = datetime.strptime(d_sort[:8], '%Y%m%d').date()
        hh = d_sort[8:10]
    if hh == '00':
        day -= timedelta(days=1)
    return day


def aggregate_daily_counts(filepaths, typ_list=None, instid_list=None):
    """
    Aggregates observation counts per DA day (06Z through next-day 00Z)
    across `filepaths`, keyed by (typ_id, instid). Returns
    {date (datetime.date): {(typ, inst): count}}.

    `typ_list`/`instid_list`, if given, must already be resolved lists of
    integer IDs (use `_format_input_list()` at the call site to accept the
    usual string/int/list forms) -- this function stays purely numeric so it
    can double as the backbone of both the live plotting functions and the
    on-disk counts history (`save_counts_history`/`load_counts_history`).
    """
    if isinstance(filepaths, str):
        filepaths = [filepaths]

    daily = {}
    for fp in filepaths:
        day = da_day_for_file(fp)
        if day is None:
            continue
        daily.setdefault(day, {})

        with xr.open_dataset(fp) as ds:
            if 'nobs' not in ds.sizes or ds.sizes['nobs'] == 0:
                continue
            typs = ds['typ'].values
            instids = ds['instid'].values if 'instid' in ds else np.zeros_like(typs)

            mask = np.ones(len(typs), dtype=bool)
            if typ_list is not None:
                mask &= np.isin(typs, typ_list)
            if instid_list is not None:
                mask &= np.isin(instids, instid_list)

            ft, fi = typs[mask], instids[mask]
            if len(ft) == 0:
                continue
            combos = np.stack([ft.astype(np.int64), fi.astype(np.int64)], axis=1)
            uniq, counts = np.unique(combos, axis=0, return_counts=True)
            for (t, i), c in zip(uniq, counts):
                key = (int(t), int(i))
                daily[day][key] = daily[day].get(key, 0) + int(c)

    return daily


def _sum_by(combo_counts, idx):
    """Collapses a {(typ, instid): count} dict to {typ: count} (idx=0) or
    {instid: count} (idx=1) by summing over the other dimension."""
    out = {}
    for key, c in combo_counts.items():
        out[key[idx]] = out.get(key[idx], 0) + c
    return out


def _order_keys(found_keys, force_order, reverse_map):
    """Orders `found_keys` per `force_order` (ints or names, resolved via
    `reverse_map`), appending any leftovers found but not explicitly ordered."""
    if force_order is None:
        return sorted(found_keys)
    forced = _format_input_list(force_order, reverse_map)
    ordered = [k for k in forced if k in found_keys]
    leftovers = sorted(found_keys - set(ordered))
    return ordered + leftovers


HISTORY_FIELDS = ['date', 'typ', 'instid', 'count']


def save_counts_history(filepaths, history_file):
    """
    Computes per-day, per-(typ,instid) observation counts across `filepaths`
    (every combination present, unfiltered) and upserts them into
    `history_file` (a CSV), keyed by (date, typ, instid) -- re-saving a date
    already present overwrites that date's rows rather than duplicating them.
    This lets a LATER run overlay past counts on a time series without
    re-reading (or even still having access to) the original raw obs files.
    """
    daily = aggregate_daily_counts(filepaths)
    if not daily:
        print(f"No observations to save to counts history ({history_file}).")
        return

    existing = {}
    if os.path.exists(history_file):
        with open(history_file, newline='') as f:
            for row in csv.DictReader(f):
                key = (row['date'], int(row['typ']), int(row['instid']))
                existing[key] = int(row['count'])

    for day, combos in daily.items():
        date_str = day.strftime('%Y-%m-%d')
        for (t, i), c in combos.items():
            existing[(date_str, t, i)] = c

    with open(history_file, 'w', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(HISTORY_FIELDS)
        for (date_str, t, i), c in sorted(existing.items()):
            writer.writerow([date_str, t, i, c])

    print(f"Saved observation counts history ({len(existing)} rows) to {history_file}")


def load_counts_history(history_file, typ_list=None, instid_list=None, start_date=None, end_date=None):
    """
    Reads `history_file` (written by save_counts_history) and returns
    {date (datetime.date): {(typ, instid): count}}, optionally filtered to
    `typ_list`/`instid_list` (resolved integer lists) and to the half-open
    date range [start_date, end_date).
    """
    if not history_file or not os.path.exists(history_file):
        return {}

    out = {}
    with open(history_file, newline='') as f:
        for row in csv.DictReader(f):
            day = datetime.strptime(row['date'], '%Y-%m-%d').date()
            if start_date is not None and day < start_date:
                continue
            if end_date is not None and day >= end_date:
                continue
            t, i, c = int(row['typ']), int(row['instid']), int(row['count'])
            if typ_list is not None and t not in typ_list:
                continue
            if instid_list is not None and i not in instid_list:
                continue
            out.setdefault(day, {})
            out[day][(t, i)] = out[day].get((t, i), 0) + c

    return out


def parse_trailing_window(tw_str):
    """
    Parses a trailing-window spec like '30d', '2w', '3m', '1y' and returns a
    function that, given a `datetime.date`, returns that date minus the
    window (calendar-accurate for months/years, not a 30-day approximation).
    """
    m = re.match(r'^\s*(\d+)\s*([dwmy])\s*$', tw_str.strip().lower())
    if not m:
        raise ValueError(f"Invalid trailing window '{tw_str}'. Use e.g. '30d', '2w', '3m', '1y'.")
    n, unit = int(m.group(1)), m.group(2)

    def _subtract(d):
        if unit == 'd':
            return d - timedelta(days=n)
        if unit == 'w':
            return d - timedelta(weeks=n)
        months_back = n if unit == 'm' else n * 12
        total_months = (d.year * 12 + (d.month - 1)) - months_back
        year, month = divmod(total_months, 12)
        month += 1
        last_day = calendar.monthrange(year, month)[1]
        day = min(d.day, last_day)
        return d.replace(year=year, month=month, day=day)

    return _subtract


def discover_exp_gmao_files(exp_dir, start_date=None, end_date=None):
    """
    Scans a GEOS-S2S3 ODAS experiment directory for the PRODUCTION
    gmao-obs-*.nc files it writes during real runs, expected at:
        <exp_dir>/ocean_das/oana-*_00/ocean_observer_*_HH/gmao-obs-YYYYMMDD.nc
    Each `oana-YYYYMMDD_00` pentad directory contains one
    `ocean_observer_YYYYMMDD_HH` subdirectory per 6-hourly cycle across the
    whole 5-day pentad (HH in 00/06/12/18) -- four gmao-obs-*.nc files, one per
    cycle, make up each DA day (06Z, 12Z, 18Z and the following 00Z).

    Returns {DA day (datetime.date): [filepath, ...]}, restricted to the
    half-open range [start_date, end_date) if given. Days are DA days
    (06Z through next-day 00Z), so a cycle with HH=00 is assigned to the
    preceding date by da_day_for_file().

    Deduplicates per 6-hourly CYCLE (the `ocean_observer_YYYYMMDD_HH`
    directory), not per day. A pentad's first cycle (<pentad date>_06) and
    last (<pentad date+5>_00) sit at opposite ends of the pentad's DA days,
    so deduplicating "latest oana- per day" would drop a cycle at a boundary.
    Deduplicating per cycle only discards a file when the SAME cycle genuinely
    appears in more than one pentad (shouldn't happen with non-overlapping
    pentads, but just in case), keeping the one from the latest `oana-`.
    """
    pattern = os.path.join(exp_dir, 'ocean_das', 'oana-*_00', 'ocean_observer_*', 'gmao-obs-*.nc')
    best_oana = {}  # cycle_key ('YYYYMMDD_HH') -> oana_date_str of the latest pentad seen so far
    best_file = {}  # cycle_key -> filepath from that latest pentad

    for fp in glob.glob(pattern):
        m_cycle = re.search(r'ocean_observer_(\d{8}_\d{2})', fp)
        if not m_cycle:
            continue
        cycle_key = m_cycle.group(1)
        day = da_day_for_file(fp)
        if start_date is not None and day < start_date:
            continue
        if end_date is not None and day >= end_date:
            continue

        m_oana = re.search(r'oana-(\d{8})_\d{2}', fp)
        oana_key = m_oana.group(1) if m_oana else ''

        current_best = best_oana.get(cycle_key)
        if current_best is None or oana_key > current_best:
            best_oana[cycle_key] = oana_key
            best_file[cycle_key] = fp
        # else: a stale/older pentad's file for a cycle already covered by a
        # later pentad -- skip it.

    best_files = {}
    for cycle_key, fp in best_file.items():
        day = da_day_for_file(fp)
        best_files.setdefault(day, []).append(fp)

    return best_files


def _dates_in_history(history_file):
    """Returns the set of dates already recorded in a counts-history CSV."""
    if not history_file or not os.path.exists(history_file):
        return set()
    dates = set()
    with open(history_file, newline='') as f:
        for row in csv.DictReader(f):
            dates.add(datetime.strptime(row['date'], '%Y-%m-%d').date())
    return dates


def update_exp_counts_history(exp_dir, history_file, start_date, end_date, cycles_per_day=4):
    """
    Incrementally extends an experiment-derived counts history CSV (kept
    separate from the locally-generated run's own history) by scanning
    `exp_dir` for production gmao-obs-*.nc files via discover_exp_gmao_files().

    Only dates in [start_date, end_date) that are NOT already present in
    `history_file` are opened and aggregated -- once a window has been read
    once, repeat calls covering the same (or an overlapping) window are cheap,
    since only the still-missing dates get read from disk. All sub-cycle files
    sharing a DA day are passed to save_counts_history() together, so
    their counts are correctly summed into one daily total (the same way a
    day is aggregated from standalone-generated 6-hourly cycles).

    Only DA DAYS (06Z through next-day 00Z) with all `cycles_per_day` 6-hourly
    cycles found are written to `history_file`. A day belonging to a
    still-running pentad (e.g. its latest cycles haven't been produced yet)
    is left out and re-checked on a later call, rather than being permanently
    cached at a partial count.
    """
    already = _dates_in_history(history_file)
    exp_files = discover_exp_gmao_files(exp_dir, start_date, end_date)
    candidates = {d: fps for d, fps in exp_files.items() if d not in already}

    incomplete = {d: len(fps) for d, fps in candidates.items() if len(fps) < cycles_per_day}
    new_files = {d: fps for d, fps in candidates.items() if len(fps) >= cycles_per_day}

    n_cached = len(exp_files) - len(candidates)
    if incomplete:
        details = ", ".join(f"{d} ({n}/{cycles_per_day})" for d, n in sorted(incomplete.items()))
        print(f"Skipping {len(incomplete)} incomplete day(s) in {exp_dir} (will retry later): {details}")

    if not new_files:
        print(f"Experiment history ({history_file}) already covers the requested window "
              f"({n_cached} day(s) cached); nothing new to read from {exp_dir}.")
        return

    n_files = sum(len(fps) for fps in new_files.values())
    print(f"Reading {len(new_files)} new day(s) ({n_files} file(s)) from {exp_dir} "
          f"({n_cached} day(s) already cached in {history_file})")
    all_new_files = [fp for fps in new_files.values() for fp in fps]
    save_counts_history(all_new_files, history_file)


def _pentad_cycle_files(exp_dir, oana_date, pentad_days, cycles_per_day):
    """
    Returns (expected_cycles, files_by_cycle) for one oana-<date>_00 pentad.

    A reanalysis pentad runs from 00Z on the oana date to 00Z
    `pentad_days` later, which is also the start of the next pentad. Its
    oana- directory holds exactly `pentad_days * cycles_per_day` 6-hourly
    cycles: <date>_06 through <date+pentad_days>_00. The closing 00Z cycle
    holds the observations used in the final 6-hourly DA window, so it is part
    of the pentad's data, not a boundary to discard.

    `files_by_cycle` maps a cycle key ('YYYYMMDD_HH') to its deduplicated
    production file from discover_exp_gmao_files().
    """
    step = timedelta(hours=24 // cycles_per_day)
    first = datetime(oana_date.year, oana_date.month, oana_date.day, 6)
    expected = [(first + i * step).strftime('%Y%m%d_%H')
                for i in range(pentad_days * cycles_per_day)]
    expected_set = set(expected)

    # Include the following calendar day so the closing <date+pentad_days>_00
    # cycle is part of the scan.
    scan_end = oana_date + timedelta(days=pentad_days + 1)
    files_by_cycle = {}
    for fps in discover_exp_gmao_files(exp_dir, oana_date, scan_end).values():
        for fp in fps:
            m = re.search(r'ocean_observer_(\d{8}_\d{2})', fp)
            if m and m.group(1) in expected_set:
                files_by_cycle[m.group(1)] = fp
    return expected, files_by_cycle


def find_latest_complete_pentad(exp_dir, pentad_days=5, cycles_per_day=4):
    """
    Scans <exp_dir>/ocean_das for oana-*_00 directories and returns the
    NEWEST one that has all of its cycles on disk.

    A pentad covers <date>_06 through <date+pentad_days>_00 (the closing
    00Z cycle is the last 6-hourly DA window and the start of the next
    pentad). Waiting for every one of those cycles, rather than for four
    cycles on each calendar day in [date, date+pentad_days), correctly handles
    both ends of the first pentad of an experiment: it has no previous pentad
    to supply <date>_00, and its <date+pentad_days>_00 cycle is not part of a
    following pentad yet.

    Returns (oana_date: date, files: {date: [filepath, ...]}) covering all
    `pentad_days` DA days, each with `cycles_per_day` cycles (06Z, 12Z, 18Z
    and the following 00Z), or (None, {}) if no pentad is complete.
    """
    pattern = os.path.join(exp_dir, 'ocean_das', 'oana-*_00')
    oana_dates = sorted(
        (datetime.strptime(m.group(1), '%Y%m%d').date()
         for m in (re.search(r'oana-(\d{8})_00', d) for d in glob.glob(pattern)) if m),
        reverse=True,
    )

    for oana_date in oana_dates:
        expected, by_cycle = _pentad_cycle_files(exp_dir, oana_date, pentad_days,
                                                 cycles_per_day)
        if len(by_cycle) != len(expected):
            continue

        files = {}
        for i, cycle in enumerate(expected):
            day = oana_date + timedelta(days=i // cycles_per_day)
            files.setdefault(day, []).append(by_cycle[cycle])
        return oana_date, files

    return None, {}


def plot_obs(filepaths, typ=None, instid=None, extent=None,
             vmin=None, vmax=None, cmap='jet', markersize=2, figsize=(12, 8)):
    """
    Plots GMAO observations from one or more gmao-obs-*.nc files on a map.
    Files may come from experiment production output or standalone generation.

    Requires $CARTOPY_DATA_DIR on the host producing the plots.

    Parameters:
    -----------
    filepaths : str or list of str
        Path(s) to the gmao-obs-*.nc file(s).
    typ : int, str, or list, optional
        Observation type(s) to filter by. Can be the integer ID or string key 
        (e.g., 'temp', 'salt', 'sst'). If provided, the plot is colored by value.
        If None, plots all observations colored by observation type with a legend.
    instid : int or list, optional
        Instrument ID(s) to filter by.
    extent : list or tuple, optional
        [lon_min, lon_max, lat_min, lat_max] for regional plotting. 
        If None, plots globally.
    vmin, vmax : float, optional
        Colorbar limits for the value scatter plot.
    cmap : str, optional
        Colormap for the scatter plot. Default is 'jet'.
    markersize : float, optional
        Size of the scatter points.
    figsize : tuple, optional
        Figure size in inches.
        
    Returns:
    --------
    fig, ax : matplotlib Figure and Axes objects
    """
    _configure_cartopy_data_dir(required=True)

    if isinstance(filepaths, str):
        filepaths = [filepaths]

    all_lons = []
    all_lats = []
    all_typs = []
    all_values = []
    all_instids = []
    
    dates_found = []
    sort_dates = []

    for fp in filepaths:
        fname = os.path.basename(fp)
        d_str, d_sort = extract_date_from_filename(fname)
        if d_str:
            dates_found.append(d_str)
            sort_dates.append(d_sort)
            
        with xr.open_dataset(fp) as ds:
            if 'nobs' not in ds.sizes or ds.sizes['nobs'] == 0:
                continue
                
            all_lons.append(ds['lon'].values)
            all_lats.append(ds['lat'].values)
            all_typs.append(ds['typ'].values)
            all_values.append(ds['value'].values)
            
            if 'instid' in ds:
                all_instids.append(ds['instid'].values)
            else:
                all_instids.append(np.zeros_like(ds['typ'].values))

    if not all_lons:
        print("No observations found in any of the provided files.")
        return None, None
        
    lons = np.concatenate(all_lons)
    lats = np.concatenate(all_lats)
    typs = np.concatenate(all_typs)
    values = np.concatenate(all_values)
    instids = np.concatenate(all_instids)
            
    # Build filter mask
    mask = np.ones(len(lons), dtype=bool)
    
    typ_list = _format_input_list(typ, OBS_IDS)
    if typ_list is not None:
        mask &= np.isin(typs, typ_list)
        
    instid_list = _format_input_list(instid)
    if instid_list is not None:
        mask &= np.isin(instids, instid_list)
        
    if extent is not None:
        lon_min, lon_max, lat_min, lat_max = extent
        mask &= (lons >= lon_min) & (lons <= lon_max)
        mask &= (lats >= lat_min) & (lats <= lat_max)
        
    # Apply mask
    lons = lons[mask]
    lats = lats[mask]
    values = values[mask]
    typs = typs[mask]
    
    if len(lons) == 0:
        print("No observations found matching the given filters.")
        return None, None
        
    # Set up map projection
    fig, ax = plt.subplots(figsize=figsize, subplot_kw={'projection': ccrs.PlateCarree()})
    
    # Add map features
    ax.add_feature(cfeature.LAND, facecolor='lightgray', zorder=0)
    ax.add_feature(cfeature.OCEAN, facecolor='white', zorder=0)
    ax.coastlines(linewidth=0.5, zorder=1)
    ax.add_feature(cfeature.BORDERS, linewidth=0.5, linestyle=':', zorder=1)
    
    if extent is not None:
        ax.set_extent(extent, crs=ccrs.PlateCarree())
    else:
        ax.set_global()
        
    # Plotting logic
    if typ is not None:
        # Color by observation value
        sc = ax.scatter(lons, lats, c=values, vmin=vmin, vmax=vmax, 
                        cmap=cmap, s=markersize, transform=ccrs.PlateCarree(), 
                        edgecolor='none', zorder=2)
        cbar = plt.colorbar(sc, ax=ax, shrink=0.6, pad=0.05)
        
        # Try to extract a sensible label for the colorbar
        if len(typ_list) == 1:
            var_name = INV_OBS_IDS.get(typ_list[0], str(typ_list[0]))
            cbar.set_label(f'{var_name} Value')
        else:
            cbar.set_label('Observation Value')
            
    else:
        # Color by observation type and add legend
        unique_typs = list(np.unique(typs))
        
        # Reorder to make sure SSS (5522) is plotted first so it remains under other observations
        sss_id = OBS_IDS['sss']
        if sss_id in unique_typs:
            unique_typs.remove(sss_id)
            unique_typs.insert(0, sss_id)
        sst_id = OBS_IDS['sst']
        if sst_id in unique_typs:
            unique_typs.remove(sst_id)

        # Use a diverse colormap based on the number of types
        color_map = plt.get_cmap('tab20' if len(unique_typs) > 10 else 'tab10')
        colors = color_map(np.linspace(0, 1, len(unique_typs)))
        
        for i, u_typ in enumerate(unique_typs):
            mask_t = (typs == u_typ)
            label = INV_OBS_IDS.get(u_typ, f"ID: {u_typ}")
            
            # Surface dense obs (SSS, SST, ADT) use standard markersize.
            # Sparse point obs (Argo, CTD, TAO, etc.) use a larger size to be visible over the dense layers.
            if u_typ in [OBS_IDS['sss'], OBS_IDS['sst'], OBS_IDS['adt']]:
                current_size = markersize
                # Explicit zorder: SSS lowest (2), SST/ADT (3)
                current_zorder = 2 if u_typ == OBS_IDS['sss'] else 3
            else:
                # Differentiate size and zorder for SALT vs TEMP since they often superimpose
                if u_typ == OBS_IDS['salt']:
                    current_size = markersize * 2  # Smaller
                    current_zorder = 5             # On top of temp
                elif u_typ == OBS_IDS['temp']:
                    current_size = markersize * 4  # Larger
                    current_zorder = 4             # Below salt
                else:
                    current_size = markersize * 4
                    current_zorder = 4

            ax.scatter(lons[mask_t], lats[mask_t], color=colors[i], s=current_size,
                       transform=ccrs.PlateCarree(), edgecolor='none',
                       zorder=current_zorder, label=label)
            
        # Adjust markerscale in legend so tiny points are visible
        m_scale = max(1.0, 5.0 / markersize)
        ax.legend(title="Obs Type", loc='center left', bbox_to_anchor=(1.02, 0.5), 
                  markerscale=m_scale, frameon=True)
                   
    # Format Title
    title_parts = []
    
    if dates_found:
        if len(dates_found) == 1:
            title_parts.append(f"Date: {dates_found[0]}")
        else:
            sorted_pairs = sorted(zip(sort_dates, dates_found))
            sorted_readable = [pair[1] for pair in sorted_pairs]
            title_parts.append(f"Date Range: {sorted_readable[0]} to {sorted_readable[-1]}")
            
    title_parts.append(f"Total Obs: {len(lons):,}")
    
    if typ_list:
        names = [INV_OBS_IDS.get(t, str(t)) for t in typ_list]
        title_parts.append(f"Type: {', '.join(names)}")
    if instid_list:
        title_parts.append(f"InstID: {', '.join(map(str, instid_list))}")
        
    ax.set_title(" | ".join(title_parts), fontsize=14)
    
    # Add gridlines
    gl = ax.gridlines(draw_labels=True, linewidth=0.5, color='gray', alpha=0.5, linestyle='--')
    gl.top_labels = False
    gl.right_labels = False

    # Adjust layout to accommodate the legend/colorbar nicely
    plt.tight_layout()
    return fig, ax


def plot_timeseries(filepaths, log_y=True, figsize=(14, 7), typ=None):
    """
    Plots a time series of observation counts per type-instid combination
    from a list of gmao-obs-*.nc files.
    """
    if isinstance(filepaths, str):
        filepaths = [filepaths]
        
    counts_dict = {}  
    dates = []
    
    # --- 1. Process typ filter ---
    # We use OBS_IDS exactly as plot_obs does
    typ_list = _format_input_list(typ, OBS_IDS)
    
    # DEBUG 1: Did the string map to the integer correctly?
    print(f"DEBUG: Input typ: '{typ}' | Resolved typ_list: {typ_list}")
    if typ is not None and not typ_list:
        print("WARNING: 'typ' was provided but evaluated to an empty list. All data will be dropped.")

    files_with_dates = []
    for fp in filepaths:
        fname = os.path.basename(fp)
        d_str, d_sort = extract_date_from_filename(fname)
        if d_sort:
            dt_obj = datetime.strptime(d_sort, "%Y%m%d%H") if len(d_sort) == 10 else datetime.strptime(d_sort, "%Y%m%d")
            files_with_dates.append((dt_obj, fp))

    files_with_dates.sort(key=lambda x: x[0])

    if not files_with_dates:
        print("No valid dates extracted from filenames. Cannot plot time series.")
        return None, None

    first_file_debug = True

    for dt_obj, fp in files_with_dates:
        dates.append(dt_obj)
        with xr.open_dataset(fp) as ds:
            if 'nobs' not in ds.sizes or ds.sizes['nobs'] == 0:
                for combo in counts_dict.keys():
                    counts_dict[combo].append(0)
                continue

            typs = ds['typ'].values
            instids = ds['instid'].values if 'instid' in ds else np.zeros_like(typs)
            
            # DEBUG 2 & 3: What does the very first non-empty file actually contain?
            if first_file_debug and typ_list is not None:
                print(f"DEBUG: First valid file being checked: {os.path.basename(fp)}")
                print(f"DEBUG: Unique typs in this file BEFORE masking: {np.unique(typs)}")
                first_file_debug = False

            # --- 2. Apply type filter mask ---
            if typ_list is not None:
                mask = np.isin(typs, typ_list)
                typs = typs[mask]
                instids = instids[mask]

            current_counts = {}
            for t, inst in zip(typs, instids):
                combo = (t, inst)
                current_counts[combo] = current_counts.get(combo, 0) + 1

            for combo, count in current_counts.items():
                if combo not in counts_dict:
                    counts_dict[combo] = [0] * (len(dates) - 1)
                counts_dict[combo].append(count)

            for combo in counts_dict.keys():
                if combo not in current_counts:
                    counts_dict[combo].append(0)

    if not counts_dict:
        print("No observations found in any files matching the given criteria.")
        return None, None

    # --- Plotting ---
    fig, ax = plt.subplots(figsize=figsize)

    # Use diverse colors and markers
    color_map = plt.get_cmap('tab20')
    colors = color_map(np.linspace(0, 1, len(counts_dict)))
    markers = ['o', 's', '^', 'D', 'v', '>', '<', 'p', '*', 'h', 'H', 'x', '+']

    for i, (combo, counts) in enumerate(counts_dict.items()):
        typ_id, inst_id = combo
        typ_name = INV_OBS_IDS.get(typ_id, f"Type:{typ_id}")

        # Only include InstID in label if it's meaningful (not 0)
        if inst_id != 0:
            inst_name = INST_IDS.get(int(inst_id), str(int(inst_id)))
            label = f"{typ_name} (Inst: {inst_name})"
        else:
            label = f"{typ_name}"

        marker = markers[i % len(markers)]

        # Plot lines with markers
        ax.plot(dates, counts, marker=marker, linestyle='-', linewidth=1.5,
                color=colors[i], label=label, markersize=6, alpha=0.8)

    if log_y:
        ax.set_yscale('log')
        ax.set_ylabel('Observation Count (Log Scale)', fontsize=12)
    else:
        ax.set_ylabel('Observation Count', fontsize=12)

    ax.set_xlabel('Date', fontsize=12)
    ax.set_title('Observation Counts per Type/Instrument Combination', fontsize=14)

    # Formatting X-axis dates
    fig.autofmt_xdate(rotation=45)

    # Legend
    ax.legend(title='Obs Configuration', bbox_to_anchor=(1.02, 1), loc='upper left', frameon=True)

    ax.grid(True, which="both", ls="--", alpha=0.5)

    plt.tight_layout()
    return fig, ax


def plot_timeseries_bar(filepaths, typ, log_y=False, figsize=(14, 7), width=0.8):
    """
    Plots a stacked bar chart time series for a SINGLE observation type,
    colored by instrument ID (instid).
    """
    if isinstance(filepaths, str):
        filepaths = [filepaths]
        
    # Format and enforce exactly one typ
    typ_list = _format_input_list(typ, OBS_IDS)
    if not typ_list or len(typ_list) != 1:
        print("Please provide exactly one 'typ' (e.g., typ='temp' or typ=3073).")
        return None, None
    single_typ = typ_list[0]
    
    # Get the pretty name for the title
    typ_name = INV_OBS_IDS.get(single_typ, f"ID {single_typ}")

    # Extract and sort dates from files
    files_with_dates = []
    for fp in filepaths:
        fname = os.path.basename(fp)
        d_str, d_sort = extract_date_from_filename(fname)
        if d_sort:
            dt_fmt = "%Y%m%d%H" if len(d_sort) == 10 else "%Y%m%d"
            files_with_dates.append((datetime.strptime(d_sort, dt_fmt), fp))

    files_with_dates.sort(key=lambda x: x[0])
    
    if not files_with_dates:
        print("No valid dates extracted. Cannot plot time series.")
        return None, None

    # Track counts: {instid: [count_day1, count_day2, ...]}
    counts_dict = {}
    dates = []

    for dt_obj, fp in files_with_dates:
        dates.append(dt_obj)
        with xr.open_dataset(fp) as ds:
            if 'nobs' not in ds.sizes or ds.sizes['nobs'] == 0:
                # Append 0 to all known instruments for empty files
                for inst in counts_dict.keys():
                    counts_dict[inst].append(0)
                continue

            typs = ds['typ'].values
            instids = ds['instid'].values if 'instid' in ds else np.zeros_like(typs)
            
            # Filter efficiently by our single type
            mask = (typs == single_typ)
            filtered_insts = instids[mask]
            
            # Count occurrences of each instrument using vectorized numpy
            unique_inst, counts = np.unique(filtered_insts, return_counts=True)
            current_counts = dict(zip(unique_inst, counts))

            # Add counts to history, initializing new instruments with zeros if necessary
            for inst, count in current_counts.items():
                if inst not in counts_dict:
                    counts_dict[inst] = [0] * (len(dates) - 1)
            
            # Append current counts (or 0) for all discovered instruments
            for inst in counts_dict.keys():
                counts_dict[inst].append(current_counts.get(inst, 0))

    if not counts_dict:
        print(f"No '{typ_name}' observations found in any files.")
        return None, None

    # --- Plotting ---
    fig, ax = plt.subplots(figsize=figsize)
    
    # We use 'bottom' to stack the bars on top of each other
    bottom_array = np.zeros(len(dates))
    
    # Create colormap dynamically based on number of instruments
    colors = plt.cm.get_cmap('tab20', len(counts_dict))
    
    for idx, (inst, count_list) in enumerate(counts_dict.items()):
        inst_name = INST_IDS.get(inst, f"Inst ID: {inst}")
        ax.bar(dates, count_list, width=width, bottom=bottom_array, 
               label=inst_name, color=colors(idx), edgecolor='black', linewidth=0.5)
        
        # Add current counts to the bottom array for the next stack layer
        bottom_array += np.array(count_list)

    if log_y:
        ax.set_yscale('log')
        # Matplotlib log scale bars need a non-zero bottom
        ax.set_ylim(bottom=1) 

    ax.set_ylabel('Observation Count', fontsize=12)
    ax.set_title(f'Time Series of {typ_name} Observations by Instrument', fontsize=14, fontweight='bold')
    
    # Put legend cleanly outside the plot to prevent clutter
    ax.legend(title='Instrument', bbox_to_anchor=(1.02, 1), loc='upper left', frameon=False)
    
    plt.xticks(rotation=45)
    plt.grid(axis='y', linestyle='--', alpha=0.6)
    
    # h_pad prevents overlapping labels
    plt.tight_layout(h_pad=2.0) 
    
    return fig, ax

def _resolve_group_filters(group_by, typ, instid, order):
    """
    Shared filter/label resolution for both the grouped-bar and stacked-area
    drawing cores. Returns None on invalid input (caller should bail out),
    otherwise a dict with: typ_filter, instid_filter, group_name_map,
    group_reverse_map, reduce_idx, order_hint, legend_title, default_title.
    """
    if group_by == 'instid':
        typ_list = _format_input_list(typ, OBS_IDS)
        if not typ_list or len(typ_list) != 1:
            print("Please provide exactly one 'typ' (e.g., typ='temp' or typ=3073).")
            return None
        return dict(
            typ_filter=typ_list,
            instid_filter=_format_input_list(instid, REVERSE_INST_IDS) if instid is not None else None,
            group_name_map=INST_IDS,
            group_reverse_map=REVERSE_INST_IDS,
            reduce_idx=1,
            order_hint=instid if (order is None and instid is not None) else order,
            legend_title='Instrument',
            default_title=f"Daily {INV_OBS_IDS.get(typ_list[0], f'ID {typ_list[0]}')} Observations Grouped by Instrument",
        )
    elif group_by == 'typ':
        typ_list = _format_input_list(typ, OBS_IDS)
        if not typ_list:
            print("Please provide at least one 'typ' (e.g., typ=['temp', 'salt']).")
            return None
        inst_label = ''
        if instid is not None:
            names = instid if isinstance(instid, (list, tuple)) else [instid]
            inst_label = f" ({', '.join(map(str, names))})"
        return dict(
            typ_filter=typ_list,
            instid_filter=_format_input_list(instid, REVERSE_INST_IDS) if instid is not None else None,
            group_name_map=INV_OBS_IDS,
            group_reverse_map=OBS_IDS,
            reduce_idx=0,
            order_hint=order if order is not None else typ_list,
            legend_title='Obs Type',
            default_title=f"Daily Observations by Type{inst_label}",
        )
    else:
        raise ValueError("group_by must be 'instid' or 'typ'")


def _aggregate_with_history(typ_filter, instid_filter, reduce_idx, filepaths,
                             history_file, trailing_window, exp_history_file=None):
    """
    Returns (unique_dates, daily_grouped, n_hist, is_exp):
      - daily_grouped: {date: {key: count}} for the current run's files, with
        any requested history window prepended.
      - n_hist: how many of the leading entries in `unique_dates` come from
        history (0 if no overlay was requested or no history was available).
      - is_exp: whether that leading region came from `exp_history_file`
        (True) or from the plain `history_file` trailing-window overlay
        (False) -- callers use this to pick the right region labels.

    Without `exp_history_file`: unchanged behavior. `daily_grouped` is this
    run's own `filepaths`, with `history_file`'s trailing window (if
    `trailing_window` is also given) prepended as "history".

    With `exp_history_file`: that experiment-derived CSV takes priority, for
    any date it covers, over both this run's own freshly generated files AND
    the plain `history_file`. The window checked is `trailing_window` before
    the earliest run date (or just the run's own date range if no
    `trailing_window`) through the run's last date. Beyond the experiment
    record's last covered date ("cutoff"), `filepaths` fills in the remaining
    dates, falling back to `history_file` for any earlier dates not covered
    by `filepaths` itself -- i.e. "generated" data only appears where the
    experiment record hasn't reached yet.

    The historical DATE RANGE (for either overlay) is determined from
    `typ_filter` alone (any instrument), not `typ_filter` + `instid_filter`
    together -- so a key with genuinely zero historical matches (e.g. an
    instrument that simply wasn't assimilated in an older production run)
    still shows up as a flat zero region spanning the full window, rather
    than silently having no "past" overlay drawn at all.

    Days with zero matches are kept (not dropped) rather than being filtered
    out, even if EVERY day in the window has zero matches for this
    typ/instid -- so a chart for an observation type/instrument with
    genuinely no data anywhere in the run (e.g. CTD/XBT in an NRT-sourced
    run) still gets a correctly-dated axis to render "no data" on, instead of
    the whole chart silently disappearing. Only a truly empty `filepaths` (no
    valid dates extracted at all) returns nothing.
    """
    daily_counts = aggregate_daily_counts(filepaths, typ_list=typ_filter, instid_list=instid_filter)
    daily_grouped = {d: _sum_by(combos, reduce_idx) for d, combos in daily_counts.items()}

    if not daily_grouped:
        return [], {}, 0, False

    run_dates = sorted(daily_grouped.keys())

    if exp_history_file:
        window_start = (parse_trailing_window(trailing_window)(run_dates[0])
                         if trailing_window else run_dates[0])
        window_end = run_dates[-1] + timedelta(days=1)

        exp_any = load_counts_history(exp_history_file, typ_list=typ_filter, instid_list=None,
                                       start_date=window_start, end_date=window_end)
        exp_dates = sorted(exp_any.keys())
        if exp_dates:
            exp_raw = load_counts_history(exp_history_file, typ_list=typ_filter, instid_list=instid_filter,
                                           start_date=window_start, end_date=window_end)
            exp_grouped = {d: _sum_by(exp_raw.get(d, {}), reduce_idx) for d in exp_dates}
            cutoff = exp_dates[-1]

            # Generated data: this run's own files for any date past the
            # experiment's coverage, plus (for earlier such dates not in this
            # run's own files) the local generated-history CSV.
            generated = {d: v for d, v in daily_grouped.items() if d > cutoff}
            if history_file:
                local_any = load_counts_history(history_file, typ_list=typ_filter, instid_list=None,
                                                 start_date=window_start, end_date=window_end)
                local_dates = [d for d in local_any if d > cutoff and d not in generated]
                if local_dates:
                    local_raw = load_counts_history(history_file, typ_list=typ_filter, instid_list=instid_filter,
                                                      start_date=window_start, end_date=window_end)
                    for d in local_dates:
                        generated[d] = _sum_by(local_raw.get(d, {}), reduce_idx)

            unique_dates = exp_dates + sorted(generated.keys())
            merged = dict(exp_grouped)
            merged.update(generated)
            return unique_dates, merged, len(exp_dates), True
        # No experiment coverage in this window at all -- fall through to the
        # plain history_file overlay (if any) below.

    unique_dates = run_dates
    n_hist = 0

    if history_file and trailing_window:
        window_start = parse_trailing_window(trailing_window)(unique_dates[0])
        hist_any = load_counts_history(history_file, typ_list=typ_filter, instid_list=None,
                                        start_date=window_start, end_date=unique_dates[0])
        hist_dates = sorted(hist_any.keys())
        if hist_dates:
            hist_raw = load_counts_history(history_file, typ_list=typ_filter, instid_list=instid_filter,
                                            start_date=window_start, end_date=unique_dates[0])
            hist_grouped = {d: _sum_by(hist_raw.get(d, {}), reduce_idx) for d in hist_dates}
            n_hist = len(hist_dates)
            unique_dates = hist_dates + unique_dates
            merged = dict(hist_grouped)
            merged.update(daily_grouped)
            daily_grouped = merged

    return unique_dates, daily_grouped, n_hist, False


def _draw_grouped_bar_core(ax, filepaths, group_by, typ=None, instid=None, log_y=False,
                            force_order=None, cmap='Set2', history_file=None,
                            trailing_window=None, exp_history_file=None, title=None):
    """
    Core drawing routine shared by plot_timeseries_grouped_bar(),
    plot_timeseries_combined_bar(), and plot_timeseries_grouped_bar_multi().
    Draws a grouped daily bar chart onto the given Axes and returns True if
    anything was drawn, False if there was no matching data.

    group_by='instid': `typ` must resolve to exactly ONE type; bars are
        grouped by instrument (optionally restricted/ordered via `instid`).
    group_by='typ':    `typ` may be a list of types; bars are grouped by
        observation type, optionally restricted to specific instrument(s)
        via `instid` (summed together, not broken out).

    If `exp_history_file` is given, it takes priority over everything else
    for any date it covers -- see _aggregate_with_history() -- and the two
    regions are labeled "Experiment (record)" / "Generated (this run)".
    Otherwise, if both `history_file` and `trailing_window` are given, daily
    counts for the matching type are pulled from `history_file` for the
    window immediately preceding the earliest date in `filepaths` and
    prepended, with the "past" region shaded and labeled separately from the
    "current" (freshly generated) region. NOTE: for dense, multi-day/month
    windows a bar chart gets one x-axis tick per day and can become very
    cluttered -- consider plot_timeseries_stacked_area*() instead for that
    case.
    """
    cfg = _resolve_group_filters(group_by, typ, instid, force_order)
    if cfg is None:
        return False

    unique_dates, daily_grouped, n_hist, is_exp = _aggregate_with_history(
        cfg['typ_filter'], cfg['instid_filter'], cfg['reduce_idx'], filepaths, history_file,
        trailing_window, exp_history_file=exp_history_file)

    if not daily_grouped:
        print("No observations found matching the given type/instrument filters.")
        return False

    found_keys = set()
    for counts in daily_grouped.values():
        found_keys.update(counts.keys())
    ordered_keys = _order_keys(found_keys, cfg['order_hint'], cfg['group_reverse_map'])

    counts_dict = {k: [daily_grouped.get(d, {}).get(k, 0) for d in unique_dates] for k in ordered_keys}

    num_days = len(unique_dates)
    num_keys = len(counts_dict)
    x_base = np.arange(num_days)
    bar_width = 0.8 / max(num_keys, 1)
    colors = plt.cm.get_cmap(cmap, max(8, num_keys))

    if n_hist:
        ax.axvspan(-0.5, n_hist - 0.5, color='lightgray', alpha=0.35, zorder=0)
        ax.axvline(n_hist - 0.5, color='black', linestyle='--', linewidth=1.2, zorder=1)

    for idx, (key, count_list) in enumerate(counts_dict.items()):
        key_name = cfg['group_name_map'].get(key, f"ID: {key}")
        x_offset = (idx - num_keys / 2 + 0.5) * bar_width
        ax.bar(x_base + x_offset, count_list, width=bar_width,
               label=key_name, color=colors(idx), edgecolor='black', linewidth=0.7, zorder=2)

    if log_y:
        ax.set_yscale('log')
        ax.set_ylim(bottom=1)
        ax.grid(axis='y', which='minor', linestyle=':', alpha=0.3)

    ax.set_ylabel('Observation Count' + (' (Log)' if log_y else ''), fontsize=11)
    ax.set_title(title or cfg['default_title'], fontsize=13, fontweight='bold')

    if not ordered_keys:
        ax.text(0.5, 0.5, 'No observations found in this window', ha='center', va='center',
                 transform=ax.transAxes, fontsize=11, color='gray', style='italic', zorder=3)

    date_labels = [d.strftime('%Y-%m-%d') for d in unique_dates]
    ax.set_xticks(x_base)
    ax.set_xticklabels(date_labels, rotation=45, ha='right')

    if n_hist:
        hist_label, cur_label = ('Experiment (record)', 'Generated (this run)') if is_exp \
            else ('Past (history)', 'Current (this run)')
        mid_hist = (n_hist - 1) / 2.0
        # y given as an axes fraction (via get_xaxis_transform) so the labels
        # sit just inside the shaded region, below the plot's top edge/title,
        # rather than directly on the ylim top (which can collide with it).
        label_y = 0.94
        ax.text(mid_hist, label_y, hist_label, ha='center', va='top',
                 fontsize=9, style='italic', color='dimgray', transform=ax.get_xaxis_transform())
        if n_hist < num_days:
            mid_cur = n_hist + (num_days - n_hist - 1) / 2.0
            ax.text(mid_cur, label_y, cur_label, ha='center', va='top',
                     fontsize=9, style='italic', color='black', transform=ax.get_xaxis_transform())

    if ordered_keys:
        ax.legend(title=cfg['legend_title'], bbox_to_anchor=(1.02, 1), loc='upper left', frameon=False)
    ax.set_axisbelow(True)
    ax.grid(axis='y', which='major', linestyle='--', alpha=0.6, zorder=0)
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    return True


def _draw_stacked_area_core(ax, filepaths, group_by, typ=None, instid=None, log_y=False,
                             order=None, cmap='tab10', history_file=None,
                             trailing_window=None, exp_history_file=None, title=None, fill_alpha=0.85):
    """
    Like _draw_grouped_bar_core(), but renders a STACKED, FILLED area/line
    plot (matplotlib stackplot) on a real datetime x-axis instead of grouped
    bars on a categorical one. This lets matplotlib pick sensible date-tick
    spacing automatically (via a Concise date formatter) no matter how wide
    the window is -- e.g. a 3-month trailing window won't force one tick
    label per day the way the bar chart does.

    See _draw_grouped_bar_core() for `exp_history_file` behavior.
    """
    cfg = _resolve_group_filters(group_by, typ, instid, order)
    if cfg is None:
        return False

    unique_dates, daily_grouped, n_hist, is_exp = _aggregate_with_history(
        cfg['typ_filter'], cfg['instid_filter'], cfg['reduce_idx'], filepaths, history_file,
        trailing_window, exp_history_file=exp_history_file)

    if not daily_grouped:
        print("No observations found matching the given type/instrument filters.")
        return False

    found_keys = set()
    for counts in daily_grouped.values():
        found_keys.update(counts.keys())
    ordered_keys = _order_keys(found_keys, cfg['order_hint'], cfg['group_reverse_map'])

    series = [[daily_grouped.get(d, {}).get(k, 0) for d in unique_dates] for k in ordered_keys]
    labels = [cfg['group_name_map'].get(k, f"ID: {k}") for k in ordered_keys]
    color_map = plt.cm.get_cmap(cmap, max(8, len(ordered_keys)))
    face_colors = [color_map(i) for i in range(len(ordered_keys))]

    # Plot the stackplot FIRST so the Axes' x-units are registered as dates
    # before we hand axvspan/axvline a plain datetime.date boundary value.
    # ax.stackplot() errors on zero series (np.vstack needs at least one), so
    # skip it entirely when there's genuinely nothing to stack for any key --
    # ax.plot() with empty data still registers the date-axis units, giving a
    # correctly-dated (empty) chart rather than erroring or vanishing.
    if ordered_keys:
        ax.stackplot(unique_dates, *series, labels=labels, colors=face_colors,
                     alpha=fill_alpha, edgecolor='black', linewidth=0.4, zorder=2)
    else:
        ax.plot(unique_dates, [0] * len(unique_dates), alpha=0)
    ax.set_xlim(unique_dates[0], unique_dates[-1])

    boundary = None
    if n_hist and n_hist < len(unique_dates):
        boundary = unique_dates[n_hist - 1] + (unique_dates[n_hist] - unique_dates[n_hist - 1]) / 2
        ax.axvspan(unique_dates[0], boundary, color='lightgray', alpha=0.35, zorder=0)
        ax.axvline(boundary, color='black', linestyle='--', linewidth=1.2, zorder=1)

    if log_y:
        ax.set_yscale('log')
        ax.set_ylim(bottom=1)

    ax.set_ylabel('Observation Count' + (' (Log)' if log_y else ''), fontsize=11)
    ax.set_title(title or cfg['default_title'], fontsize=13, fontweight='bold')

    if not ordered_keys:
        ax.text(0.5, 0.5, 'No observations found in this window', ha='center', va='center',
                 transform=ax.transAxes, fontsize=11, color='gray', style='italic', zorder=3)

    locator = mdates.AutoDateLocator()
    ax.xaxis.set_major_locator(locator)
    ax.xaxis.set_major_formatter(mdates.ConciseDateFormatter(locator))

    if boundary is not None:
        hist_label, cur_label = ('Experiment (record)', 'Generated (this run)') if is_exp \
            else ('Past (history)', 'Current (this run)')
        mid_hist = unique_dates[0] + (boundary - unique_dates[0]) / 2
        mid_cur = boundary + (unique_dates[-1] - boundary) / 2
        # y given as an axes fraction (via get_xaxis_transform) so the labels
        # sit just inside the shaded region, below the plot's top edge/title,
        # rather than directly on the ylim top (which can collide with it).
        label_y = 0.94
        ax.text(mid_hist, label_y, hist_label, ha='center', va='top',
                 fontsize=9, style='italic', color='dimgray', transform=ax.get_xaxis_transform())
        ax.text(mid_cur, label_y, cur_label, ha='center', va='top',
                 fontsize=9, style='italic', color='black', transform=ax.get_xaxis_transform())

    if ordered_keys:
        ax.legend(title=cfg['legend_title'], bbox_to_anchor=(1.02, 1), loc='upper left', frameon=False)
    ax.set_axisbelow(True)
    ax.grid(axis='y', which='major', linestyle='--', alpha=0.5, zorder=0)
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    return True


def plot_timeseries_grouped_bar(filepaths, typ, instid=None, log_y=False, figsize=(14, 7),
                                force_inst_order=None, cmap='Set2',
                                history_file=None, trailing_window=None, exp_history_file=None):
    """
    Plots a grouped bar chart time series for a SINGLE observation type.
    Aggregates sub-daily files into true daily totals.

    Parameters:
    - instid (int, str, or list): Optional instrument filter (e.g. ['TAO', 'PIRATA', 'RAMA']
                                  or [501, 502, 504]). If given, only these instruments are
                                  included, and (unless force_inst_order is also given) they
                                  are displayed in the order provided here.
    - force_inst_order (list): Optional list of integers or strings to force the order
                               of instruments in the legend and grouping
                               (e.g., ['Argo', 'TAO', 'CTD'] or [508, 501, 513]).
    - cmap (str): Matplotlib colormap name.
    - history_file (str): Optional path to a CSV written by save_counts_history().
    - trailing_window (str): e.g. '30d', '2w', '3m', '1y'. If given together with
      `history_file`, past counts for that trailing window (prior to the earliest
      date in `filepaths`) are loaded from history and shown alongside the
      current run's data, with a shaded region and divider line marking the
      boundary between "Past (history)" and "Current (this run)".
    - exp_history_file (str): Optional path to an experiment-derived counts CSV
      (written by update_exp_counts_history()). If given, it takes priority over
      `history_file`/this run's own files for any date it covers, with the
      boundary labeled "Experiment (record)" / "Generated (this run)" instead.
      See _aggregate_with_history() for the exact merge rule.
    """
    if isinstance(filepaths, str):
        filepaths = [filepaths]
    fig, ax = plt.subplots(figsize=figsize)
    ok = _draw_grouped_bar_core(ax, filepaths, group_by='instid', typ=typ, instid=instid,
                                 log_y=log_y, force_order=force_inst_order, cmap=cmap,
                                 history_file=history_file, trailing_window=trailing_window,
                                 exp_history_file=exp_history_file)
    if not ok:
        plt.close(fig)
        return None, None
    plt.tight_layout(h_pad=2.0)
    return fig, ax


def plot_timeseries_combined_bar(filepaths, typ, instid=None, log_y=False, figsize=(14, 7),
                                  cmap='tab10', history_file=None, trailing_window=None,
                                  exp_history_file=None):
    """
    Plots a grouped bar chart time series that combines MULTIPLE observation types
    (e.g. typ=['temp', 'salt']) into a single chart, grouped by day, for a given
    instrument (or set of instruments). Useful for e.g. a combined TEMP+SALT bar
    chart for Argo.

    Parameters:
    -----------
    filepaths : str or list of str
        Path(s) to the gmao-obs-*.nc file(s).
    typ : str, int, or list
        Observation type(s) to combine into the same chart (e.g. ['temp', 'salt']).
    instid : int, str, or list, optional
        Instrument(s) to restrict the counts to (e.g. 'Argo' or ['Argo']).
        If None, all instruments reporting the given type(s) are included together.
    log_y : bool, optional
        If True, sets the y-axis to a logarithmic scale.
    figsize : tuple, optional
        Figure size in inches.
    cmap : str, optional
        Matplotlib colormap name used to color each observation type's bars.
    history_file, trailing_window, exp_history_file : optional
        See plot_timeseries_grouped_bar() -- same past/current overlay behavior.

    Returns:
    --------
    fig, ax : matplotlib Figure and Axes objects
    """
    if isinstance(filepaths, str):
        filepaths = [filepaths]
    fig, ax = plt.subplots(figsize=figsize)
    ok = _draw_grouped_bar_core(ax, filepaths, group_by='typ', typ=typ, instid=instid,
                                 log_y=log_y, cmap=cmap,
                                 history_file=history_file, trailing_window=trailing_window,
                                 exp_history_file=exp_history_file)
    if not ok:
        plt.close(fig)
        return None, None
    plt.tight_layout(h_pad=2.0)
    return fig, ax


def plot_timeseries_grouped_bar_multi(filepaths, typs, instid=None, log_y=False, figsize=None,
                                       force_inst_order=None, cmap='Set2',
                                       history_file=None, trailing_window=None,
                                       exp_history_file=None, suptitle=None):
    """
    Like plot_timeseries_grouped_bar(), but stacks ONE grouped-bar-by-instrument
    subplot per entry in `typs` (e.g. ['temp', 'salt']) into a single figure,
    sharing the x-axis (dates) and instrument ordering/colors -- e.g. a combined
    TEMP+SALT view for TAO/PIRATA/RAMA, one subplot on top of the other.

    Parameters mirror plot_timeseries_grouped_bar(), except `typs` is a list.

    Returns:
    --------
    fig, axes : matplotlib Figure and a list/array of Axes (one per type)
    """
    if isinstance(filepaths, str):
        filepaths = [filepaths]
    if isinstance(typs, str):
        typs = [typs]

    n = len(typs)
    fig, axes = plt.subplots(n, 1, figsize=figsize or (14, 5.5 * n), sharex=True)
    axes = [axes] if n == 1 else list(axes)

    any_ok = False
    for ax, t in zip(axes, typs):
        t_ids = _format_input_list(t, OBS_IDS)
        t_name = INV_OBS_IDS.get(t_ids[0], str(t)) if t_ids else str(t)
        ok = _draw_grouped_bar_core(ax, filepaths, group_by='instid', typ=t, instid=instid,
                                     log_y=log_y, force_order=force_inst_order, cmap=cmap,
                                     history_file=history_file, trailing_window=trailing_window,
                                     exp_history_file=exp_history_file,
                                     title=f"Daily {t_name} Observations Grouped by Instrument")
        any_ok = any_ok or ok

    if not any_ok:
        plt.close(fig)
        return None, None

    if suptitle:
        fig.suptitle(suptitle, fontsize=15, fontweight='bold', y=1.02)

    plt.tight_layout(h_pad=2.5)
    return fig, axes


def plot_timeseries_stacked_area(filepaths, typ, instid=None, log_y=False, figsize=(14, 6),
                                  order=None, cmap='tab10', history_file=None, trailing_window=None,
                                  exp_history_file=None):
    """
    Stacked-area (filled line plot) counterpart to plot_timeseries_grouped_bar():
    one observation type, stacked by instrument, on a real datetime x-axis.
    Prefer this over the bar-chart version for wide/dense windows (e.g. a
    multi-month `trailing_window`) -- the date axis gets sensibly-spaced
    ticks automatically instead of one per day.

    Parameters mirror plot_timeseries_grouped_bar() (`force_inst_order` is
    named `order` here for brevity). See that function's docstring for
    `history_file`/`trailing_window`/`exp_history_file` behavior.
    """
    if isinstance(filepaths, str):
        filepaths = [filepaths]
    fig, ax = plt.subplots(figsize=figsize)
    ok = _draw_stacked_area_core(ax, filepaths, group_by='instid', typ=typ, instid=instid,
                                  log_y=log_y, order=order, cmap=cmap,
                                  history_file=history_file, trailing_window=trailing_window,
                                  exp_history_file=exp_history_file)
    if not ok:
        plt.close(fig)
        return None, None
    plt.tight_layout()
    return fig, ax


def plot_timeseries_stacked_area_combined(filepaths, typ, instid=None, log_y=False, figsize=(14, 6),
                                           cmap='tab10', history_file=None, trailing_window=None,
                                           exp_history_file=None):
    """
    Stacked-area counterpart to plot_timeseries_combined_bar(): multiple
    observation types (e.g. ['temp', 'salt']) stacked together for a given
    instrument, on a real datetime x-axis. See plot_timeseries_stacked_area()
    for why this is preferable to the bar version over wide windows.
    """
    if isinstance(filepaths, str):
        filepaths = [filepaths]
    fig, ax = plt.subplots(figsize=figsize)
    ok = _draw_stacked_area_core(ax, filepaths, group_by='typ', typ=typ, instid=instid,
                                  log_y=log_y, cmap=cmap,
                                  history_file=history_file, trailing_window=trailing_window,
                                  exp_history_file=exp_history_file)
    if not ok:
        plt.close(fig)
        return None, None
    plt.tight_layout()
    return fig, ax


def plot_timeseries_stacked_area_multi(filepaths, typs, instid=None, log_y=False, figsize=None,
                                        order=None, cmap='tab10', history_file=None,
                                        trailing_window=None, exp_history_file=None, suptitle=None):
    """
    Stacked-area counterpart to plot_timeseries_grouped_bar_multi(): stacks
    one stacked-area-by-instrument subplot per entry in `typs` (e.g.
    ['temp', 'salt']) into a single figure sharing the date axis -- e.g. a
    combined TEMP+SALT view for TAO/PIRATA/RAMA, one subplot above the other.
    """
    if isinstance(filepaths, str):
        filepaths = [filepaths]
    if isinstance(typs, str):
        typs = [typs]

    n = len(typs)
    fig, axes = plt.subplots(n, 1, figsize=figsize or (14, 5 * n), sharex=True)
    axes = [axes] if n == 1 else list(axes)

    any_ok = False
    for ax, t in zip(axes, typs):
        t_ids = _format_input_list(t, OBS_IDS)
        t_name = INV_OBS_IDS.get(t_ids[0], str(t)) if t_ids else str(t)
        ok = _draw_stacked_area_core(ax, filepaths, group_by='instid', typ=t, instid=instid,
                                      log_y=log_y, order=order, cmap=cmap,
                                      history_file=history_file, trailing_window=trailing_window,
                                      exp_history_file=exp_history_file,
                                      title=f"Daily {t_name} Observations Grouped by Instrument")
        any_ok = any_ok or ok

    if not any_ok:
        plt.close(fig)
        return None, None

    if suptitle:
        fig.suptitle(suptitle, fontsize=15, fontweight='bold', y=1.02)

    plt.tight_layout()
    return fig, axes


def plot_timeseries_stacked_area_panels(filepaths, panels, log_y=False, figsize=None,
                                         cmap='tab10', history_file=None, trailing_window=None,
                                         exp_history_file=None, suptitle=None):
    """
    Like plot_timeseries_stacked_area_multi(), but each stacked subplot is
    INDEPENDENTLY configured (different `typ` and/or `instid` per panel),
    rather than varying only `typ` while sharing one `instid` filter across
    all of them. Useful for composite charts combining different instruments
    with different type sets -- e.g. CTD (TEMP+SALT stacked) above XBT
    (TEMP only), in one figure sharing the date axis.

    Parameters:
    -----------
    panels : list of dict, each with:
        'typ'    : str, int, or list -- observation type(s) for this panel
                   (a list stacks multiple types together, e.g. ['temp','salt'])
        'instid' : str, int, or list, optional -- instrument filter for this panel
        'title'  : str, optional -- panel title override (default: auto-generated)
        'order'  : optional -- series order override for this panel
    Other parameters mirror plot_timeseries_stacked_area_multi().

    A panel with no matching data is still shown, annotated "No observations
    found", rather than failing the whole figure; only if EVERY panel has no
    data does this return (None, None).

    Returns:
    --------
    fig, axes : matplotlib Figure and a list of Axes (one per panel)
    """
    if isinstance(filepaths, str):
        filepaths = [filepaths]

    n = len(panels)
    fig, axes = plt.subplots(n, 1, figsize=figsize or (14, 5 * n), sharex=True)
    axes = [axes] if n == 1 else list(axes)

    any_ok = False
    for ax, panel in zip(axes, panels):
        ok = _draw_stacked_area_core(ax, filepaths, group_by='typ',
                                      typ=panel['typ'], instid=panel.get('instid'),
                                      log_y=log_y, order=panel.get('order'), cmap=cmap,
                                      history_file=history_file, trailing_window=trailing_window,
                                      exp_history_file=exp_history_file,
                                      title=panel.get('title'))
        if not ok:
            ax.text(0.5, 0.5, 'No observations found', ha='center', va='center',
                     transform=ax.transAxes, fontsize=12, color='gray', style='italic')
            ax.set_xticks([])
            ax.set_yticks([])
            if panel.get('title'):
                ax.set_title(panel['title'], fontsize=13, fontweight='bold')
        any_ok = any_ok or ok

    if not any_ok:
        plt.close(fig)
        return None, None

    if suptitle:
        fig.suptitle(suptitle, fontsize=15, fontweight='bold', y=1.02)

    plt.tight_layout()
    return fig, axes
