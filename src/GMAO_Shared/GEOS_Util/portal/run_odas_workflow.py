#!/usr/bin/env python3
"""
ODAS experiment plot generation and portal deployment workflow.

PRIMARY MODE (production default) -- no date argument:

  Given $EXPDIR (or an explicit -exp/--exp-dir), find the newest fully
  populated block of --days consecutive days in that experiment's own
  production gmao-obs-*.nc output, write the standard diagnostic plot set
  under $EXPDIR/www/<oana-date>/ (--plot-dir defaults to $EXPDIR/www), and
  optionally publish the PNG-only plot directory to the experiment portal
  (--publish-dir).

  This is the mode used from within an experiment production workflow: no
  observation files are generated. The plots describe the experiment's own
  production output, and the portal copy is the deployed product.

SECONDARY MODE -- a start date (YYYYMMDD) is given:

  1. Generate gmao-obs-*.nc files every 6 hours, starting at <date>_06,
     for N days (default 5), from the raw observation archive.
  2. Persist per-day, per-(type, instrument) observation counts to a history
     CSV so later runs can overlay past counts without re-reading raw data.
  3. Produce the same diagnostic plot set in <outdir>/plots/:
        - one global obs map per complete calendar day (obs_map_YYYYMMDD.png);
        - combined TEMP+SALT time series for TAO/PIRATA/RAMA and for Argo;
        - SSS time series for SMOS/SMAP; ADT for all altimeters;
        - combined CTD (TEMP+SALT) / XBT (TEMP) time series, which may be
          empty for near-real-time experiments.

  Portal publication is not performed in this mode; use the primary mode.

Usage (csh; $EXPDIR is set by the encompassing workflow, or set it first):
    setenv EXPDIR /path/to/experiment
    ./run_odas_workflow.py -c config.yaml
    ./run_odas_workflow.py -c config.yaml -tw 3m
    ./run_odas_workflow.py --publish-dir e1
        # no date: primary mode; -exp defaults to $EXPDIR, plots go to
        # $EXPDIR/www/<oana-date>/, and are published to the portal
    ./run_odas_workflow.py 20260406
    ./run_odas_workflow.py 20260406 --start-hour 06 --days 5
    ./run_odas_workflow.py 20260406 -tw 3m
    ./run_odas_workflow.py 20260406 --assim /path/to/obs/assim/
        # secondary mode with an explicit raw-observation archive
    ./run_odas_workflow.py 20260406 -exp ./GiOCEAN_e1
        # -exp sources counts from the experiment's production gmao-obs-*.nc
        # files for every date that experiment has reached, including dates
        # inside this window; generated data fills in later dates only. Counts
        # are cached in $EXPDIR/www/obs_counts_history.csv, separately from
        # this run's own generated-counts history.
    ./run_odas_workflow.py 20260406 --legacy
        # one file-open per 6-hour cycle instead of the default batched path

The sibling modules (generate_gmao_nc_optimized.py, plot_gmao_obs.py) are
located relative to this file, so the script can be invoked from any working
directory. Generation-mode paths such as `./assim/...` are satisfied by a
temporary symlink when `assim`/--assim is configured; without it, run from a
directory where `assim/` resolves.

Configuration:
    $EXPDIR supplies the experiment directory used by the primary plotting
    mode; --plot-dir defaults to $EXPDIR/www and the experiment-derived counts
    history to $EXPDIR/www/obs_counts_history.csv. The encompassing workflow
    sets these variables. When running standalone from a csh shell, set them
    first with:
        setenv EXPDIR /path/to/experiment
        setenv ODAS_OBS_TYPE_1 "Argo CTD TAO PIRATA RAMA Jason-2"
        setenv CARTOPY_DATA_DIR /discover/nobackup/projects/gmao/SIteam/CartopyDownloads

    $CARTOPY_DATA_DIR supplies the pre-populated Natural Earth shapefile cache
    used by the daily observation maps in both modes. It is a shared group
    directory without user write permission and is never created here.

    The secondary NetCDF-generation mode reads the raw observation archive
    from the `assim` config key (or --assim). The supplied YAML sets it to
    /discover/nobackup/projects/gmao/m2oasf/aogcm/g5odas/obs/assim/; a
    temporary `assim` symlink to that path is used during generation. Without
    `assim`/--assim, the current directory must already contain an assim/
    symlink or directory. The observation-type list is read from $ODAS_OBS_TYPE_1;
    the primary mode uses neither.

Requirements:
    Primary mode: a readable experiment directory from $EXPDIR (or -exp)
    containing ocean_das/oana-*_00/ocean_observer_*/gmao-obs-*.nc, plus
    ssh/scp access to the portal host when --publish-dir is given. $ODAS_OBS_TYPE_1
    is not needed.

    Secondary mode: $ODAS_OBS_TYPE_1 and either the `assim` config key / --assim
    archive path or an `assim/` observation data tree in the working directory.
"""

import argparse
import os
import re
import shutil
import subprocess
import tempfile
import sys
from datetime import datetime, timedelta

try:
    import yaml
except ImportError:
    yaml = None

import matplotlib
matplotlib.use('Agg')  # headless rendering: no display needed to save PNGs
import matplotlib.pyplot as plt

# Make sure we can import the sibling modules regardless of cwd
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from generate_gmao_nc_optimized import generate_nc, generate_nc_batch
from plot_gmao_obs import (
    plot_obs,
    plot_timeseries_stacked_area,
    plot_timeseries_stacked_area_combined,
    plot_timeseries_stacked_area_multi,
    plot_timeseries_stacked_area_panels,
    extract_date_from_filename,
    save_counts_history,
    update_exp_counts_history,
    parse_trailing_window,
    find_latest_complete_pentad,
)

MOORED_BUOYS = ['TAO', 'PIRATA', 'RAMA']
SSS_SATELLITES = ['SMOS', 'SMAPV5.0']

WEBSERVER_HOST = 'gs6101-polar.gsfc.nasa.gov'
WEBSERVER_BASE = 'giocean'  # base directory on the portal host (remote home)

# Default extra files to upload from the experiment directory during portal
# publication, so they land alongside the generated plots without being copied
# into the local plot directory. Relative paths are resolved against --exp-dir;
# the supplied config uses the explicit absolute form
# `$EXPDIR/stats_combined.png` instead.
EXTRA_EXP_FILES = [
    'stats_combined.png',
]

# Matches $NAME and ${NAME} in YAML config values.
_ENV_VAR_RE = re.compile(r'\$(?:\{([A-Za-z_][A-Za-z0-9_]*)\}|([A-Za-z_][A-Za-z0-9_]*))')


def load_obs_types():
    """Returns the observation-type list from the $ODAS_OBS_TYPE_1 environment variable.

    The encompassing workflow sets ODAS_OBS_TYPE_1; when running standalone in csh,
    set it with: setenv ODAS_OBS_TYPE_1 "Argo CTD TAO ..."
    """
    value = os.environ.get('ODAS_OBS_TYPE_1')
    if not value or not value.split():
        raise SystemExit(
            'ODAS_OBS_TYPE_1 environment variable is not set or is empty. '
            'In csh, set it with: setenv ODAS_OBS_TYPE_1 "Argo CTD TAO ..."'
        )
    return value.split()


def resolve_exp_dir():
    """Returns the experiment directory from $EXPDIR.

    The encompassing workflow sets EXPDIR; when running standalone in csh,
    set it with: setenv EXPDIR /path/to/experiment
    """
    value = os.environ.get('EXPDIR')
    if not value:
        raise SystemExit(
            'EXPDIR environment variable is not set or is empty. '
            'In csh, set it with: setenv EXPDIR /path/to/experiment'
        )
    return value


def expand_env_vars(value):
    """Expands $VAR / ${VAR} references in YAML config values.

    Strings are expanded recursively (also inside lists). An unset variable
    referenced by a config file is reported with a csh `setenv` hint instead of
    silently leaving a literal `$VAR` in a path.
    """
    if isinstance(value, str):
        for match in _ENV_VAR_RE.finditer(value):
            name = match.group(1) or match.group(2)
            if name not in os.environ:
                raise SystemExit(
                    f"Environment variable ${name} referenced in the config file "
                    f"is not set. In csh, set it with: setenv {name} <value>"
                )
        return os.path.expandvars(value)
    if isinstance(value, list):
        return [expand_env_vars(item) for item in value]
    return value


def make_assim_source_dir(assim_path):
    """Creates a temporary working directory containing an `assim` symlink.

    generate_nc()/generate_nc_batch() read paths like `./assim/...`, so a
    configured archive path is exposed through a symlink in a fresh directory.
    Generated gmao-obs-*.nc files are also written there and then moved to the
    requested output directory by generate_files().
    """
    if not assim_path or not os.path.isdir(assim_path):
        raise SystemExit(f"Assimilation archive directory not found: {assim_path}")
    target = os.path.abspath(assim_path)
    work_dir = tempfile.mkdtemp(prefix='odas_assim_')
    try:
        os.symlink(target, os.path.join(work_dir, 'assim'))
    except OSError as exc:
        shutil.rmtree(work_dir, ignore_errors=True)
        raise SystemExit(f"Could not link assimilation archive '{target}': {exc}")
    return work_dir


def build_cycle_list(start_date, start_hour, days):
    """Returns the list of (yyyy, mm, dd, hh) string tuples for every 6-hour
    cycle from start_date/start_hour through `days` days later."""
    current = datetime.strptime(f"{start_date}{start_hour}", "%Y%m%d%H")
    end = current + timedelta(days=days)

    cycles = []
    while current < end:
        cycles.append((current.strftime("%Y"), current.strftime("%m"),
                        current.strftime("%d"), current.strftime("%H")))
        current += timedelta(hours=6)
    return cycles


def generate_files(start_date, start_hour, days, obs_types, outdir, source_dir, batch=True):
    """Secondary-mode helper: generates gmao-obs files every 6 hours for `days`
    days, starting at start_date/start_hour. Returns the list of generated file
    paths (cycles whose output already exists in `outdir` are skipped).

    generate_nc()/generate_nc_batch() rely on relative paths like
    './assim/...', so files are produced while cwd is `source_dir` (normally
    a directory where `assim` resolves) and then moved into `outdir`. For a
    configured archive path, main() passes a temporary directory containing
    an `assim` symlink; this avoids writing into the archive or needing a
    symlink in outdir, which some mounted filesystems refuse to allow.

    When `batch` is True (default), every distinct source observation file
    (e.g. T_ARGO_2026.nc) is opened and read exactly ONCE for the whole
    window via generate_nc_batch(), instead of once per 6-hour cycle via
    generate_nc() -- a large reduction in I/O, especially over a slow or
    network-mounted filesystem.
    """
    all_cycles = build_cycle_list(start_date, start_hour, days)
    outdir = os.path.abspath(outdir)
    os.makedirs(outdir, exist_ok=True)

    generated_files = []
    missing_cycles = []
    for (yyyy, mm, dd, hh) in all_cycles:
        fname = f"gmao-obs-{yyyy}{mm}{dd}{hh}.nc"
        fpath = os.path.join(outdir, fname)
        generated_files.append(fpath)
        if os.path.exists(fpath):
            print(f"--- {yyyy}-{mm}-{dd} {hh}:00 : {fname} already exists, skipping ---")
        else:
            missing_cycles.append((yyyy, mm, dd, hh))

    if not missing_cycles:
        return generated_files

    cwd = os.getcwd()
    os.chdir(source_dir)
    try:
        if batch:
            print(f"--- Batch-generating {len(missing_cycles)} cycle(s); "
                  f"each source file is opened once for all of them ---")
            produced_names = generate_nc_batch(missing_cycles, obs_types)
        else:
            produced_names = []
            for (yyyy, mm, dd, hh) in missing_cycles:
                print(f"--- Generating obs for {yyyy}-{mm}-{dd} {hh}:00 ---")
                generate_nc(yyyy, mm, dd, hh, obs_types)
                produced_names.append(f"gmao-obs-{yyyy}{mm}{dd}{hh}.nc")

        for fname in produced_names:
            produced = os.path.join(source_dir, fname)
            fpath = os.path.join(outdir, fname)
            if os.path.exists(produced):
                shutil.move(produced, fpath)
            else:
                print(f"  Warning: expected output '{fname}' was not created.")
    finally:
        os.chdir(cwd)

    return generated_files


def group_files_by_day(filepaths):
    """Groups a list of gmao file paths into calendar-day buckets, where a
    day's window is [00z inclusive, next day's 00z exclusive).

    Because `filepaths` is itself a half-open series (it stops at the
    requested end time), its very last calendar day can end up containing
    only the lone 00z file that marks the *start* of what would be the next
    day's window -- there's no 06z/12z/18z data for it, and by the exclusive
    upper-bound rule it isn't really "a day" of this run's data. That trailing
    single-file day is dropped (unless it's the only day present at all).
    """
    days = {}
    for fp in filepaths:
        fname = os.path.basename(fp)
        _, d_sort = extract_date_from_filename(fname)
        if not d_sort:
            continue
        day, hh = d_sort[:8], d_sort[8:10]
        days.setdefault(day, []).append((hh, fp))

    ordered_days = sorted(days.items())
    if len(ordered_days) > 1:
        last_day, last_entries = ordered_days[-1]
        if len(last_entries) == 1 and last_entries[0][0] == '00':
            ordered_days = ordered_days[:-1]

    return {day: [fp for _, fp in sorted(entries)] for day, entries in ordered_days}


def save_fig(fig, path):
    if fig is None:
        print(f"  Skipped (no figure generated): {path}")
        return None
    fig.savefig(path, dpi=150, bbox_inches='tight')
    plt.close(fig)
    print(f"  Saved: {path}")
    return path


def publish_to_webserver(local_dir, remote_subdir, host=WEBSERVER_HOST,
                         base=WEBSERVER_BASE, extra_files=None):
    """
    Deploys `local_dir` (the finished PNG plot directory) to the portal host:
    copies it recursively via scp -r to `host`, landing at
    <base>/<remote_subdir>/<basename(local_dir)> under the remote user's home
    directory. `remote_subdir` is created first via ssh, since scp itself
    won't create missing parent directories. `local_dir`'s own basename (the
    oana date in primary mode) is preserved on the remote side.

    `extra_files` is an optional list of (local_path, remote_name) pairs. Each
    file is scp'd directly from its source (normally under $EXPDIR) into that
    same remote plot directory; no copy or softlink is made under `local_dir`.

    Requires passwordless (key-based) SSH access to `host`; raises
    CalledProcessError if any ssh or scp step fails.
    """
    remote_parent = '/'.join(p for p in (base, remote_subdir) if p)
    remote_dir = f'{remote_parent}/{os.path.basename(os.path.normpath(local_dir))}'
    print(f"Publishing '{local_dir}' -> {host}:{remote_parent}/ ...")
    subprocess.run(['ssh', host, 'mkdir', '-p', remote_parent], check=True)
    subprocess.run(['scp', '-r', local_dir, f'{host}:{remote_parent}/'], check=True)
    for src, name in extra_files or []:
        subprocess.run(['scp', src, f'{host}:{remote_dir}/{name}'], check=True)
        print(f"  Published extra file: {src} -> {host}:{remote_dir}/{name}")
    print(f"  Published: {host}:{remote_dir}/")


def resolve_extra_exp_files(exp_dir, filenames=EXTRA_EXP_FILES):
    """Resolves --extra-exp-files to (source_path, basename) pairs.

    Absolute paths (e.g. `$EXPDIR/stats_combined.png` after YAML expansion) are
    used as-is; relative paths are resolved against `exp_dir`. Missing files
    are skipped with a warning rather than failing the run. Nothing is copied
    into the plot directory: the returned paths are uploaded directly by
    publish_to_webserver().
    """
    resolved = []
    for path in filenames:
        src = path if os.path.isabs(path) else os.path.join(exp_dir, path)
        if not os.path.isfile(src):
            print(f"  Warning: extra file not found, skipping: {src}")
            continue
        resolved.append((src, os.path.basename(src)))
    return resolved


def save_daily_maps(files, plot_dir):
    """Saves one global obs map per complete calendar day in `files`, named
    obs_map_YYYYMMDD.png."""
    days = group_files_by_day(files)
    for day, day_files in sorted(days.items()):
        fig, ax = plot_obs(day_files, markersize=1)
        save_fig(fig, os.path.join(plot_dir, f"obs_map_{day}.png"))


def save_timeseries_plots(files, plot_dir, history_kwargs):
    """Saves the standard time-series diagnostic plots for `files` (shared by
    the primary experiment-plot mode and the secondary generation mode)."""
    # Stacked area (not bars): over a wide trailing window the date axis gets
    # sensibly-spaced ticks automatically instead of one per day.
    print("--- TEMP+SALT time series for TAO, PIRATA, RAMA (combined) ---")
    fig, axes = plot_timeseries_stacked_area_multi(files, typs=['temp', 'salt'], instid=MOORED_BUOYS,
                                                    suptitle='TAO / PIRATA / RAMA', **history_kwargs)
    save_fig(fig, os.path.join(plot_dir, "timeseries_temp_salt_tao_pirata_rama.png"))

    print("--- Combined TEMP+SALT time series for Argo ---")
    fig, ax = plot_timeseries_stacked_area_combined(files, typ=['temp', 'salt'], instid='Argo', **history_kwargs)
    save_fig(fig, os.path.join(plot_dir, "timeseries_argo_temp_salt.png"))

    print("--- SSS time series for SMOS + SMAP (combined) ---")
    fig, ax = plot_timeseries_stacked_area(files, typ='sss', instid=SSS_SATELLITES, **history_kwargs)
    save_fig(fig, os.path.join(plot_dir, "timeseries_sss_smos_smap.png"))

    print("--- ADT time series (all altimeters) ---")
    fig, ax = plot_timeseries_stacked_area(files, typ='adt', **history_kwargs)
    save_fig(fig, os.path.join(plot_dir, "timeseries_adt.png"))

    # CTD/XBT are almost never present in an NRT run (they take time to
    # become available) but are expected once validating against a
    # reanalysis-style experiment -- shown even if empty for this run (see
    # plot_timeseries_stacked_area_panels()'s per-panel "No observations
    # found" handling).
    print("--- CTD (TEMP+SALT) / XBT (TEMP) time series ---")
    fig, axes = plot_timeseries_stacked_area_panels(
        files,
        panels=[
            {'typ': ['temp', 'salt'], 'instid': 'CTD'},
            {'typ': 'temp', 'instid': 'XBT'},
        ],
        suptitle='CTD / XBT', **history_kwargs)
    save_fig(fig, os.path.join(plot_dir, "timeseries_ctd_xbt.png"))


def run_experiment_only(args, source_dir):
    """
    Primary mode: no date given; -exp/--exp-dir defaults to $EXPDIR. Plots the
    most recent fully populated `--days`-day block directly from the
    experiment's own production gmao-obs-*.nc files (see
    find_latest_complete_pentad()), saved under <--plot-dir>/<oana-date>/
    (--plot-dir defaults to $EXPDIR/www) and optionally published to the
    portal. The experiment-derived counts history lives in
    $EXPDIR/www/obs_counts_history.csv. No observation files are generated.
    """
    print(f"Looking for the latest complete {args.days}-day block in '{args.exp_dir}'...")
    oana_date, files_by_day = find_latest_complete_pentad(args.exp_dir, pentad_days=args.days)
    if oana_date is None:
        raise SystemExit(f"No complete {args.days}-day block found in '{args.exp_dir}' "
                          f"(every oana-* pentad found is still being written, or none exist).")

    oana_str = oana_date.strftime('%Y%m%d')
    plot_dir = os.path.join(args.plot_dir, oana_str)
    os.makedirs(plot_dir, exist_ok=True)
    print(f"Latest complete block: oana-{oana_str}_00 "
          f"({oana_date} through {oana_date + timedelta(days=args.days - 1)}) -> '{plot_dir}/'\n")

    files = sorted(fp for fps in files_by_day.values() for fp in fps)

    history_kwargs = {}
    if args.trailing_window:
        exp_history_file = args.exp_history_file or os.path.join(args.exp_dir, 'www', 'obs_counts_history.csv')
        os.makedirs(os.path.dirname(exp_history_file), exist_ok=True)
        window_start = parse_trailing_window(args.trailing_window)(oana_date)
        run_end_date = oana_date + timedelta(days=args.days)

        print(f"Trailing window enabled: {args.trailing_window} of additional history from "
              f"experiment '{args.exp_dir}' (cached in {exp_history_file})")
        update_exp_counts_history(args.exp_dir, exp_history_file, window_start, run_end_date)
        history_kwargs = dict(exp_history_file=exp_history_file, trailing_window=args.trailing_window)

    print("=" * 70)
    print("STEP 1: Daily observation maps (all obs types)")
    print("=" * 70)
    save_daily_maps(files, plot_dir)

    print("=" * 70)
    print("STEP 2: Time series diagnostics")
    print("=" * 70)
    save_timeseries_plots(files, plot_dir, history_kwargs)

    if args.publish_dir is not None:
        print("=" * 70)
        print("STEP 3: Publishing plots and experiment files to the portal")
        print("=" * 70)
        extra_files = resolve_extra_exp_files(args.exp_dir, args.extra_exp_files)
        publish_to_webserver(plot_dir, args.publish_dir, host=args.webserver_host,
                             base=args.webserver_base, extra_files=extra_files)

    print("\nWorkflow complete.")
    print(f"  Source     : experiment '{args.exp_dir}', oana-{oana_str}_00")
    print(f"  Plots      : {plot_dir}/")


def load_yaml_config(path):
    """
    Reads a YAML config file of CLI flag values (keys match the long flag
    names, dashes or underscores both accepted -- e.g. "exp-dir" or
    "exp_dir") and returns them as a dict of argparse `dest` names, suitable
    for `parser.set_defaults(**result)`. $VAR / ${VAR} references in string
    values (e.g. $EXPDIR) are expanded from the environment. CLI flags always
    override whatever is in the file, since set_defaults() only changes
    what's used when a flag isn't explicitly passed.
    """
    if yaml is None:
        raise SystemExit("PyYAML is required to use -c/--config but is not installed "
                          "in this Python environment.")
    if not os.path.exists(path):
        raise SystemExit(f"Config file not found: {path}")
    with open(path) as f:
        data = yaml.safe_load(f) or {}
    return {k.replace('-', '_'): expand_env_vars(v) for k, v in data.items()}


def main():
    # Pre-parse just -c/--config, since it has to be known before building
    # the full parser's defaults from it (see load_yaml_config()).
    pre_parser = argparse.ArgumentParser(add_help=False)
    pre_parser.add_argument('-c', '--config', default=None)
    pre_args, _ = pre_parser.parse_known_args()

    parser = argparse.ArgumentParser(
        description="Generate and publish ODAS experiment plots; optionally generate "
                    "observation files for a multi-day window.")
    parser.add_argument('-c', '--config', default=None,
                         help="Path to a YAML file providing any of these options as defaults "
                              "(keys match the long flag names, e.g. 'exp-dir', 'plot-dir', "
                              "'publish-dir', 'extra-exp-files', 'webserver-host', "
                              "'webserver-base'). $VAR / ${VAR} references in values are "
                              "expanded from the environment (the supplied config uses "
                              "$EXPDIR). Flags passed on the command line override the "
                              "file's values. See config.yaml in this directory.")
    parser.add_argument('date', nargs='?', default=None,
                         help='Starting date, YYYYMMDD (secondary mode). Omit for the primary '
                              'mode: plot the experiment\'s own latest complete production '
                              'window and publish it (exp-dir defaults to $EXPDIR) -- see '
                              '--plot-dir.')
    parser.add_argument('--start-hour', default='06',
                         help='Hour of the first file, HH (default: 06)')
    parser.add_argument('--days', type=int, default=5,
                         help='Size of the complete block to plot in primary mode (no date), '
                              'or number of days to generate in secondary mode, 4 files/day '
                              '(default: 5).')
    parser.add_argument('--outdir', default=None,
                         help='Output directory (default: ./odas_<date>, or '
                              'work_<exp-dir basename>/odas_<date> if -exp is given)')
    parser.add_argument('--assim', default=None,
                         help='Raw observation archive for the secondary generation mode '
                              '(the supplied YAML sets this to '
                              '/discover/nobackup/projects/gmao/m2oasf/aogcm/g5odas/obs/assim/). '
                              'A temporary `assim` symlink to it is used for generation. '
                              'If omitted, the current directory must already contain an '
                              'assim/ symlink or directory.')
    parser.add_argument('--legacy', action='store_true',
                         help='Generate one cycle at a time (re-opens each source file '
                              'per cycle) instead of the default batched generation.')
    parser.add_argument('--history-file', default=None,
                         help='Path to this run\'s own persistent counts-history CSV '
                              '(default: ./obs_counts_history.csv, or '
                              'work_<exp-dir basename>/obs_counts_history.csv if -exp is given). '
                              'This run\'s counts are always appended/updated here.')
    parser.add_argument('-tw', '--trailing-window', default=None,
                         help="Overlay past counts on the time series plots, covering this "
                              "trailing window before the plotted/generated window (e.g. "
                              "'30d', '2w', '3m', '1y'). By default pulled from --history-file; "
                              "if -exp is given, pulled from --exp-history-file instead "
                              "(see -exp). Omit to only use the experiment's own covered dates.")
    parser.add_argument('-exp', '--exp-dir', default=None,
                         help="Path to a GEOS-S2S3 ODAS experiment directory. In primary "
                              "mode it defaults to $EXPDIR (set in csh with: setenv EXPDIR "
                              "/path/to/experiment) and its production output supplies the "
                              "plots. In secondary mode it also sources counts instead of "
                              "this run's generated files, for any date the experiment "
                              "already covers -- including dates within this run's window, "
                              "not just the -tw trailing window before it. Generated data "
                              "only fills in dates the experiment hasn't reached yet. "
                              "Production gmao-obs-*.nc files are discovered at "
                              "<exp-dir>/ocean_das/oana-*_00/ocean_observer_*/gmao-obs-*.nc. "
                              "Also sets the experiment-derived counts history to "
                              "<exp-dir>/www/obs_counts_history.csv. Production files "
                              "include additional checks based on live model state that "
                              "the standalone generator does not apply, so counts may "
                              "differ slightly. Given with NO date, switches to the "
                              "primary plot-and-publish mode -- see --plot-dir.")
    parser.add_argument('--exp-history-file', default=None,
                         help="Path to the experiment-derived counts-history CSV (default: "
                              "<exp-dir>/www/obs_counts_history.csv, where <exp-dir> is "
                              "$EXPDIR unless -exp is given). Only dates not already cached "
                              "here are re-read from --exp-dir.")
    parser.add_argument('--plot-dir', default=None,
                         help="Base directory for plots in primary mode (no date given). "
                              "Defaults to $EXPDIR/www; a subdirectory named after the latest "
                              "complete block's oana-*_00 date is created under it. Not valid "
                              "together with a date.")
    parser.add_argument('--publish-dir', default=None,
                         help="Primary mode only (no date given). If given, once plotting is "
                              "complete, deploy the plot directory to the portal host "
                              "--webserver-host via scp -r, landing at "
                              "<webserver-base>/<PUBLISH_DIR>/<oana date, e.g. 20260406> under "
                              "the remote home directory -- just the PNGs, no .nc files. The "
                              "remote parent directory is created first. Omit to skip "
                              "publication (default).")
    parser.add_argument('--webserver-host', default=WEBSERVER_HOST,
                         help=f"Portal host to scp the plot directory to when --publish-dir "
                              f"is given (default: {WEBSERVER_HOST}).")
    parser.add_argument('--webserver-base', default=WEBSERVER_BASE,
                         help=f"Base directory on --webserver-host, under the remote home "
                               f"directory, that --publish-dir is relative to (default: "
                              f"{WEBSERVER_BASE}; expected to be a softlink there).")
    parser.add_argument('--extra-exp-files', nargs='+', default=None,
                         help="Primary mode only, with --publish-dir. Extra files to "
                              "upload directly from the experiment directory into the "
                              "remote plot directory; they are not copied into the local "
                              "plot directory. Absolute paths are used as-is (the supplied "
                              "YAML uses $EXPDIR/stats_combined.png); relative paths are "
                              "resolved against --exp-dir. Default: "
                              f"{EXTRA_EXP_FILES}.")

    if pre_args.config:
        parser.set_defaults(**load_yaml_config(pre_args.config))
    args = parser.parse_args()

    if args.extra_exp_files is None:
        args.extra_exp_files = EXTRA_EXP_FILES

    # Experiment-relative defaults are resolved after CLI/YAML values are known.
    if args.exp_dir and not args.exp_history_file:
        args.exp_history_file = os.path.join(args.exp_dir, 'www', 'obs_counts_history.csv')

    source_dir = os.getcwd()

    if args.date is None:
        if not args.exp_dir:
            args.exp_dir = resolve_exp_dir()
            args.exp_history_file = os.path.join(args.exp_dir, 'www', 'obs_counts_history.csv')
        if not args.plot_dir:
            args.plot_dir = os.path.join(args.exp_dir, 'www')
        run_experiment_only(args, source_dir)
        return

    if args.plot_dir:
        parser.error("--plot-dir only applies to primary mode (no date given)")
    if args.publish_dir:
        parser.error("--publish-dir only applies to primary mode (no date given)")

    exp_name = os.path.basename(os.path.normpath(args.exp_dir)) if args.exp_dir else None
    work_dir = None
    if exp_name:
        work_dir = f"work_{exp_name}"
        os.makedirs(work_dir, exist_ok=True)
        print(f"Experiment '{exp_name}' -> outputs/history under '{work_dir}/'\n")

    default_outdir = os.path.join(work_dir, f"odas_{args.date}") if work_dir else f"odas_{args.date}"
    outdir = args.outdir or default_outdir
    plot_dir = os.path.join(outdir, 'plots')
    os.makedirs(plot_dir, exist_ok=True)

    default_history_file = os.path.join(work_dir, 'obs_counts_history.csv') if work_dir else 'obs_counts_history.csv'
    history_file = args.history_file or default_history_file

    obs_types = load_obs_types()
    print(f"Observation types ({len(obs_types)}): {' '.join(obs_types)}\n")

    # --- 1. Generate the gmao-obs files ---
    print("=" * 70)
    print("STEP 1: Generating gmao-obs files")
    print("=" * 70)
    generation_source_dir = source_dir
    temp_assim_dir = None
    if args.assim:
        generation_source_dir = make_assim_source_dir(args.assim)
        temp_assim_dir = generation_source_dir
        print(f"Using assimilation archive: {os.path.abspath(args.assim)}")
    try:
        files = generate_files(args.date, args.start_hour, args.days, obs_types, outdir,
                               generation_source_dir, batch=not args.legacy)
    finally:
        if temp_assim_dir:
            shutil.rmtree(temp_assim_dir, ignore_errors=True)
    print(f"\nGenerated/collected {len(files)} files in '{outdir}'.\n")

    # --- 2. Persist today's counts to the (local) history file ---
    print("=" * 70)
    print("STEP 2: Saving observation counts to history")
    print("=" * 70)
    save_counts_history(files, history_file)

    exp_history_file = None
    history_kwargs = {}
    if args.exp_dir:
        # Source counts from the experiment's production gmao-obs files
        # instead of this run's generated files/history, for any date the
        # experiment already covers -- kept in a SEPARATE cache CSV (never
        # mixed with `history_file`) and only re-read incrementally. This
        # covers both the -tw trailing window before the run AND the run's
        # own dates, so an experiment that's already caught up to (or past)
        # today's date is preferred over freshly-generated data for those
        # dates too; generated data only fills in what the experiment
        # hasn't reached yet.
        exp_history_file = args.exp_history_file
        os.makedirs(os.path.dirname(exp_history_file), exist_ok=True)
        run_start_date = datetime.strptime(args.date, '%Y%m%d').date()
        run_end_date = run_start_date + timedelta(days=args.days)
        window_start = (parse_trailing_window(args.trailing_window)(run_start_date)
                         if args.trailing_window else run_start_date)

        print(f"Sourcing counts from experiment '{args.exp_dir}' for "
              f"{window_start} through {run_end_date} (cached in {exp_history_file})")
        update_exp_counts_history(args.exp_dir, exp_history_file, window_start, run_end_date)

        history_kwargs = dict(history_file=history_file, exp_history_file=exp_history_file,
                               trailing_window=args.trailing_window)
    elif args.trailing_window:
        print(f"Trailing window enabled: {args.trailing_window} of history from {history_file}")
        history_kwargs = dict(history_file=history_file, trailing_window=args.trailing_window)

    # --- 3. Daily global obs maps (all obs types), one per complete calendar day ---
    print("=" * 70)
    print("STEP 3: Daily observation maps (all obs types)")
    print("=" * 70)
    save_daily_maps(files, plot_dir)

    # --- 4-8. Time series diagnostics ---
    print("=" * 70)
    print("STEP 4: Time series diagnostics")
    print("=" * 70)
    save_timeseries_plots(files, plot_dir, history_kwargs)

    print("\nWorkflow complete.")
    print(f"  Data files : {outdir}/gmao-obs-*.nc")
    print(f"  Plots      : {plot_dir}/")
    print(f"  History (generated) : {history_file}")
    if exp_history_file:
        print(f"  History (experiment): {exp_history_file}  (takes priority wherever it has data)")


if __name__ == "__main__":
    main()
