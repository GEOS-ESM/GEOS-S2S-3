#!/usr/bin/env python3
"""
Optional recovery utility: regenerate missing gmao-obs-*.nc files for an
experiment whose own observation output is incomplete or was lost.

This is not part of the normal workflow (plotting/portal deployment or
standalone generation). Use it only to rebuild an experiment's observation
tree from the raw archive, in the layout read by the plotting workflow:

    ocean_das/oana-<pentad_start>_00/ocean_observer_<date>_<hh>/gmao-obs-<date>.nc

Each `oana-` directory is a 5-day pentad; it holds one `ocean_observer_`
subdirectory per 6-hourly cycle, and each file name carries only the calendar
date (the hour is encoded in the directory name).

Regeneration uses generate_nc_batch() from this directory and the observation
type list from the $ODAS_OBS environment variable (provided by the encompassing
workflow). Pass --assim for the raw archive, or run from a directory where
`assim/` resolves to the same archive the experiment originally read. The
reconstructed files are a best-effort approximation, not a faithful recovery
of the previous files:

  - Only the NetCDF observation files are rebuilt. Checks that depended on the
    experiment's live model state at run time (for example ice-related
    screening), other output formats, and any "use previous obs" behavior are
    not reproduced.
  - Observation-error rescaling and the centred +/-3 h assimilation window use
    the defaults in generate_gmao_nc_optimized.py. If the experiment used
    non-default settings (ODAS_T_prof_sigo, ODAS_S_prof_sigo, ODAS_ADT_sigo,
    ODAS_SSS_sigo, ODAS_NDAYS -- not recorded in $ODAS_OBS), the result will
    not match what was originally produced.
  - The raw archive is assumed unchanged since the original run; upstream
    reprocessing of source files can also change counts/values.

Use --pentads with the exact oana-<date>_00 start dates when possible; --range
simply steps 5 days at a time and is only correct when the first date really
is a pentad boundary. Files whose destination already exists are skipped, so
an interrupted recovery can be re-invoked.

Requirements:
    - --assim /path/to/assim/ (the raw observation archive), or an assim/
      symlink in the directory you run from
    - generate_gmao_nc_optimized.py (sibling module)
    - $ODAS_OBS set to the observation-type list. The encompassing workflow
      provides it; when running standalone in csh, set it first with:
          setenv ODAS_OBS "Argo CTD TAO PIRATA RAMA Jason-2"

Usage:
    ./reproduce_gmao_files.py --outdir recovered_ocean_das \
        --assim /discover/nobackup/projects/gmao/m2oasf/aogcm/g5odas/obs/assim/ \
        --pentads 20260301 20260306 20260311

    ./reproduce_gmao_files.py --outdir recovered_ocean_das \
        --assim /discover/nobackup/projects/gmao/m2oasf/aogcm/g5odas/obs/assim/ \
        --range 20260301 20260401
"""

import argparse
import os
import shutil
import sys
import tempfile
from datetime import datetime, timedelta

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from generate_gmao_nc_optimized import generate_nc_batch


def load_obs_types():
    """Returns the observation-type list from the $ODAS_OBS environment variable.

    The encompassing workflow sets ODAS_OBS; when running standalone in csh,
    set it with: setenv ODAS_OBS "Argo CTD TAO ..."
    """
    value = os.environ.get('ODAS_OBS')
    if not value or not value.split():
        raise SystemExit(
            'ODAS_OBS environment variable is not set or is empty. '
            'In csh, set it with: setenv ODAS_OBS "Argo CTD TAO ..."'
        )
    return value.split()


def make_assim_source_dir(assim_path):
    """Creates a temporary working directory containing an `assim` symlink."""
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


def pentad_cycle_list(pentad_start):
    """The 20 (yyyy, mm, dd, hh) cycles belonging to one 5-day pentad: starts
    at <pentad_start>_06 and steps every 6h for 5 days (so the first day has
    only 06/12/18 and the last day only 00)."""
    current = datetime.strptime(f"{pentad_start}06", "%Y%m%d%H")
    end = current + timedelta(days=5)
    cycles = []
    while current < end:
        cycles.append((current.strftime("%Y"), current.strftime("%m"),
                        current.strftime("%d"), current.strftime("%H")))
        current += timedelta(hours=6)
    return cycles


def dest_path(outdir, pentad_start, yyyy, mm, dd, hh):
    return os.path.join(outdir, 'ocean_das', f"oana-{pentad_start}_00",
                         f"ocean_observer_{yyyy}{mm}{dd}_{hh}", f"gmao-obs-{yyyy}{mm}{dd}.nc")


def reproduce_pentad(pentad_start, obs_types, outdir, source_dir):
    """Generates one pentad's cycle files (skipping any whose destination
    already exists) and lays them out as
    ocean_das/oana-<pentad_start>_00/ocean_observer_<date>_<hh>/gmao-obs-<date>.nc"""
    cycles = pentad_cycle_list(pentad_start)
    print(f"\n=== Pentad oana-{pentad_start}_00 ({len(cycles)} cycles) ===")

    missing = []
    for (yyyy, mm, dd, hh) in cycles:
        dest = dest_path(outdir, pentad_start, yyyy, mm, dd, hh)
        if os.path.exists(dest):
            print(f"  {dest} already exists, skipping")
        else:
            missing.append((yyyy, mm, dd, hh))

    if not missing:
        return

    cwd = os.getcwd()
    os.chdir(source_dir)
    try:
        produced_names = generate_nc_batch(missing, obs_types)  # flat gmao-obs-YYYYMMDDHH.nc, written to source_dir
        for (yyyy, mm, dd, hh), fname in zip(missing, produced_names):
            produced = os.path.join(source_dir, fname)
            dest = dest_path(outdir, pentad_start, yyyy, mm, dd, hh)
            os.makedirs(os.path.dirname(dest), exist_ok=True)
            if os.path.exists(produced):
                shutil.move(produced, dest)
                print(f"  {dest}")
            else:
                print(f"  Warning: expected output '{fname}' was not created.")
    finally:
        os.chdir(cwd)


def main():
    parser = argparse.ArgumentParser(
        description="Recover missing gmao-obs-*.nc files for an experiment, "
                    "in the experiment's ocean_das/oana-*/ocean_observer_*/ layout.",
        formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--outdir', required=True,
                         help="Output directory; an ocean_das/ tree is created inside it.")
    parser.add_argument('--assim', default=None,
                         help="Raw observation archive containing the sources referenced as "
                              "./assim/... by generate_gmao_nc_optimized.py. A temporary "
                              "symlink to it is used during generation. If omitted, the "
                              "current directory must already contain an assim/ symlink "
                              "or directory.")
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument('--pentads', nargs='+', metavar='YYYYMMDD',
                        help="Exact oana-<date>_00 pentad start date(s) to reconstruct.")
    group.add_argument('--range', nargs=2, metavar=('START', 'END'),
                        help="Reconstruct every 5-day pentad from START through END "
                             "(inclusive), stepping 5 days from START. Only correct if "
                             "START really is a pentad boundary in that experiment -- "
                             "prefer --pentads if you know the exact boundaries.")
    args = parser.parse_args()

    source_dir = os.getcwd()
    temp_assim_dir = None
    if args.assim:
        source_dir = make_assim_source_dir(args.assim)
        temp_assim_dir = source_dir
        print(f"Using assimilation archive: {os.path.abspath(args.assim)}")
    outdir = os.path.abspath(args.outdir)
    os.makedirs(outdir, exist_ok=True)
    obs_types = load_obs_types()
    print(f"Observation types ({len(obs_types)}): {' '.join(obs_types)}")

    if args.pentads:
        pentad_starts = args.pentads
    else:
        start = datetime.strptime(args.range[0], "%Y%m%d")
        end = datetime.strptime(args.range[1], "%Y%m%d")
        pentad_starts = []
        cur = start
        while cur <= end:
            pentad_starts.append(cur.strftime("%Y%m%d"))
            cur += timedelta(days=5)

    print(f"Reconstructing {len(pentad_starts)} pentad(s): {', '.join(pentad_starts)}")

    try:
        for pentad_start in pentad_starts:
            reproduce_pentad(pentad_start, obs_types, outdir, source_dir)
    finally:
        if temp_assim_dir:
            shutil.rmtree(temp_assim_dir, ignore_errors=True)

    print(f"\nDone. Recovered files are under: {os.path.join(outdir, 'ocean_das')}/")


if __name__ == "__main__":
    main()
