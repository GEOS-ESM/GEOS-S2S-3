# ODAS experiment plots & portal deployment

This directory is the plotting and portal-publication toolkit for GEOS-S2S3
ocean data assimilation (ODAS) experiments. All code, configuration and
documentation live together in this single directory.

**Primary use:** produce the standard diagnostic plot set from an experiment's
own production observation files and deploy those plots to the experiment
portal as part of the production workflow. This is the default mode of
`run_odas_workflow.py` (no date argument).

**Secondary use:** generate `gmao-obs-YYYYMMDDHH.nc` files from the raw
observation archive for a standalone window, and plot those. This mode exists
for testing and verification of available observations; it is not the normal
production path.

**Optional recovery utility:** `reproduce_gmao_files.py` can rebuild an
experiment's missing observation tree from the raw archive in the production
directory layout. It is a best-effort recovery tool, not part of the routine
workflow (see §3.1).

---

## 1. Directory layout

| File | Role |
|---|---|
| `README.md` | This document. |
| `run_odas_workflow.py` | Command-line driver. Selects the plotting/portal mode when no date is given, and the generate-and-plot mode when a date is given. |
| `plot_gmao_obs.py` | Plotting library (daily maps, observation-count time series, counts history) and helpers for reading an experiment's production NetCDF files. |
| `generate_gmao_nc_optimized.py` | Standalone extraction of observations into `gmao-obs-*.nc` files. Used only by the secondary mode and the recovery utility. |
| `reproduce_gmao_files.py` | Optional recovery utility: rebuilds a missing experiment observation tree from the raw archive, in the production `ocean_das/...` layout (§3.1). |
| `config.yaml` | Configuration for the `GiOCEAN_e1` experiment (plots + portal publication). |

The scripts locate their sibling modules relative to their own file location,
so they can be invoked from any working directory. The NetCDF generator reads
paths such as `./assim/...`, but the supplied config sets `assim`, so the
generate-and-plot mode creates a temporary `assim` symlink automatically.
Without a config file, pass `--assim` or run from a directory where `assim/`
resolves.

Directories/files created at runtime:

- `$EXPDIR/www/` — base directory for the primary plotting/portal mode. It
  holds `<oana-date>/` plot directories and the experiment-derived counts
  history `obs_counts_history.csv`.
- `$EXPDIR/www/<oana-date>/` — the published plot set (daily maps and time
  series as PNGs). This is `<plot-dir>/<oana-date>/` with the default
  `--plot-dir` of `$EXPDIR/www`.
- `work_<exp>/` — output and generated-counts history for the secondary
  generate-and-plot mode only; created automatically.
- `<outdir>/plots/` — plots from the generate-and-plot mode (secondary).
- `<outdir>/ocean_das/` — reconstructed observation tree written by the
  optional recovery utility (§3.1).

---

## 2. Primary workflow: plots from an experiment, deployed to the portal

This is the production path. No observation files are generated: the workflow
reads the experiment's own `gmao-obs-*.nc` output, finds the newest complete
block of `--days` consecutive days, and writes one directory of PNGs per block.

```csh
# $EXPDIR is set by the encompassing workflow. For standalone runs:
setenv EXPDIR /path/to/GiOCEAN_e1

# Preferred: all experiment-specific settings come from config.yaml in this directory
./run_odas_workflow.py -c config.yaml

# Same, with an extra trailing window of historical counts on the time series
./run_odas_workflow.py -c config.yaml -tw 3m

# Equivalent explicit invocation (no YAML)
./run_odas_workflow.py -exp "$EXPDIR" \
    --plot-dir "$EXPDIR/www" \
    --publish-dir e1
```

`-c/--config` keys map to long option names (dashes or underscores accepted).
Values in the YAML file are defaults; any option given on the command line
wins. `$VAR` and `${VAR}` references in YAML values are expanded from the
environment at load time. The supplied config uses `$EXPDIR` for
`exp-dir`, `plot-dir` (`$EXPDIR/www`) and `exp-history-file`
(`$EXPDIR/www/obs_counts_history.csv`). See `config.yaml`.

Steps performed in this mode:

1. Scan `<exp-dir>/ocean_das/oana-*_00/` for the newest pentad whose
   `--days` calendar days are all complete (four 6-hourly cycles per day).
   Production files are discovered at
   `<exp-dir>/ocean_das/oana-*_00/ocean_observer_*/gmao-obs-*.nc`.
2. Write plots to `<plot-dir>/<oana-date>/` (by default
   `$EXPDIR/www/<oana-date>/`):
   - `obs_map_YYYYMMDD.png` — global observation map for each complete day;
   - `timeseries_temp_salt_tao_pirata_rama.png`;
   - `timeseries_argo_temp_salt.png`;
   - `timeseries_sss_smos_smap.png`;
   - `timeseries_adt.png`;
   - `timeseries_ctd_xbt.png` (rendered with an annotation when an experiment
     has no CTD/XBT data in the window).
3. If `--publish-dir` is given, publish the plot directory to the portal host
   with `scp -r`. Any files listed in `--extra-exp-files` are uploaded directly
   from `$EXPDIR` into the same remote plot directory; they are not copied or
   symlinked into `$EXPDIR/www`, so no duplicate is kept locally. Configured
   paths are under `$EXPDIR`: the supplied config uses
   `$EXPDIR/stats_combined.png` (relative paths are also accepted and resolved
   against `--exp-dir`). The remote location is
   `<webserver-base>/<publish-dir>/<oana-date>/` under the remote user's home
   directory; the remote parent is created over `ssh` first. The option names
   `--webserver-host` / `--webserver-base` are retained for compatibility and
   identify the portal host and its base directory. Key-based SSH access is
   required. Publishing is skipped when `--publish-dir` is omitted.

With `-tw/--trailing-window`, counts from the experiment's own production
files for the requested period (e.g. `3m`) are cached in
`$EXPDIR/www/obs_counts_history.csv` (`--exp-history-file`) so computation is
not repeated for the same days.

---

## 3. Secondary workflow: generate observation files for a window

Given a start date, the workflow first generates `gmao-obs-*.nc` files, then
produces the same plot set in `<outdir>/plots/` (default
`work_<exp>/odas_<date>/plots/` when `-exp` is given, otherwise
`odas_<date>/plots/`).

```bash
./run_odas_workflow.py 20260406
./run_odas_workflow.py 20260406 --start-hour 06 --days 5
./run_odas_workflow.py 20260406 -tw 3m
./run_odas_workflow.py 20260406 --assim /discover/nobackup/projects/gmao/m2oasf/aogcm/g5odas/obs/assim/
./run_odas_workflow.py 20260406 --legacy   # one file-open per cycle (compatibility option)
```

This mode requires:

- the `$ODAS_OBS` environment variable, containing the space-separated
  observation-type list (see §4.1); the encompassing workflow provides it;
- the raw observation archive. A path to `assim` given in `config.yaml`, is the path
to observation files in gmao format that are ingested by S2S3 ODAS. The
  workflow creates a temporary `assim` symlink to it during generation.
  Without a config file, pass `--assim <path>` or provide an `assim/` symlink
  in the working directory.

Cycles are 6-hourly over a half-open window of `--days` days. The extractor
uses a window centred on each cycle (±3 h); products pinned to a fixed hour
share one window per calendar day. By default each distinct source file is
opened once for the whole window. `--legacy` selects the one-file-open-per-cycle
path (flag name retained for command-line compatibility). Already-present
outputs are skipped, so an interrupted run can be restarted.

Per-day, per-(type, instrument) counts are upserted into a CSV history file
(`work_<exp>/obs_counts_history.csv` by default) so later runs can overlay a
trailing window without re-reading the raw files. When `-exp` is also given,
counts from the experiment's production files take priority for every date
they cover (including dates inside the generated window); generated counts
only fill dates the experiment has not reached. Those experiment-derived
counts are cached separately in `$EXPDIR/www/obs_counts_history.csv`.

Publishing to the portal is not performed in this mode; use the primary mode
above for production deployment.

### 3.1 Optional recovery of missing experiment observation files

`reproduce_gmao_files.py` is an on-demand utility (it imports only
`generate_gmao_nc_optimized.py`, no plotting stack). Use it when an experiment
has lost some or all of its own `gmao-obs-*.nc` output but the raw observation
archive is still available. It uses `$ODAS_OBS` for the observation-type list
and writes the files back in the layout consumed by the plotting workflow:

```text
<outdir>/ocean_das/oana-<pentad_start>_00/ocean_observer_<date>_<hh>/gmao-obs-<date>.nc
```

```csh
# When running outside the encompassing workflow, set the obs-type list first
# (csh syntax):
setenv ODAS_OBS "Argo CTD TAO PIRATA RAMA Jason-2"

# Exact pentad start dates (preferred -- take them from surviving oana-*_00
# directory names so the reconstructed tree lines up with the experiment):
./reproduce_gmao_files.py --outdir recovered_ocean_das \
    --assim /discover/nobackup/projects/gmao/m2oasf/aogcm/g5odas/obs/assim/ \
    --pentads 20260301 20260306 20260311

# Or step every 5 days from a start date (only correct if it is a real
# pentad boundary in that experiment):
./reproduce_gmao_files.py --outdir recovered_ocean_das \
    --assim /discover/nobackup/projects/gmao/m2oasf/aogcm/g5odas/obs/assim/ \
    --range 20260301 20260401
```

Each pentad's 20 six-hourly cycles are generated with the same batched path as
the secondary workflow; the hour moves from the filename into the
`ocean_observer_` directory name. Existing destination files are skipped, so an
interrupted recovery can be re-run. Pass `--assim` for the raw archive (as in
the examples above), or run from a directory where `assim/` resolves to the
archive the experiment originally used.

**Best-effort only.** The regenerated files are not a faithful replacement:
checks that relied on the experiment's live model state at run time, other
output formats and any "use previous obs" behavior are not reproduced;
observation-error scaling and the centred ±3 h window use the extractor's
defaults rather than the experiment's own settings; and the raw archive is
assumed unchanged. Verify the result before pointing the primary plotting
workflow at a recovered tree.

---

## 4. Configuration reference

`config.yaml` currently sets:

| Key | Meaning |
|---|---|
| `exp-dir` | Experiment directory containing `ocean_das/oana-*_00/`; set from `$EXPDIR` in the supplied config. |
| `plot-dir` | Base directory under which `<oana-date>/` plot directories are written; `$EXPDIR/www` in the supplied config. |
| `exp-history-file` | Cache of experiment-derived observation counts; `$EXPDIR/www/obs_counts_history.csv` in the supplied config. |
| `assim` | Raw observation archive used only by the secondary generation mode; `/discover/nobackup/projects/gmao/m2oasf/aogcm/g5odas/obs/assim/` in the supplied config. |
| `publish-dir` | Remote subdirectory under `webserver-base` to publish to. Omit to skip publication. |
| `webserver-host` | Portal host used for `ssh`/`scp` (`gs6101-polar.gsfc.nasa.gov`). |
| `webserver-base` | Remote base directory, relative to the remote home directory. |
| `extra-exp-files` | Extra files under `$EXPDIR` uploaded directly into the remote plot directory when publishing; `$EXPDIR/stats_combined.png` in the supplied config. No local copy under `www`. Relative paths are resolved against `exp-dir`. |

### 4.1 Environment

`$EXPDIR` supplies the experiment directory to the primary plotting/portal
mode and to the supplied YAML config. Its `www` subdirectory is the plot base
directory and the location of the experiment-derived counts history. The
encompassing workflow sets `EXPDIR`; when running outside it in a csh shell,
set it explicitly first:

```csh
setenv EXPDIR /path/to/experiment
```

With that default, the primary mode uses:

- experiment directory: `$EXPDIR`
- plots: `$EXPDIR/www/<oana-date>/`
- counts history: `$EXPDIR/www/obs_counts_history.csv`
- extra published file: `$EXPDIR/stats_combined.png`

The secondary NetCDF-generation mode and the optional recovery utility read the
observation-type list from the `$ODAS_OBS` environment variable. The
encompassing workflow sets this variable before invoking either script. When
running outside that workflow in a csh shell, set it explicitly first:

```csh
setenv ODAS_OBS "Argo CTD TAO PIRATA RAMA Jason-2"
```

The value is the space-separated list of observation types, not a path to a
file. The primary plotting/portal mode does not use `$ODAS_OBS` and does not
require it to be set. An unset variable referenced by the YAML config is reported
with a csh `setenv` hint rather than being left as a literal `$VAR` path.

`$CARTOPY_DATA_DIR` points to the Natural Earth shapefile cache used for the
daily observation maps in both modes. The encompassing workflow sets it; for
standalone csh runs:

```csh
setenv CARTOPY_DATA_DIR /discover/nobackup/projects/gmao/SIteam/CartopyDownloads
```

Other useful options (full list: `./run_odas_workflow.py --help`):

- `--days N` — size of the complete block to look for / number of days to generate (default 5).
- `-tw`, `--trailing-window` — historical span to overlay, e.g. `30d`, `2w`, `3m`, `1y`.
- `--outdir`, `--history-file`, `--exp-history-file` — override the per-experiment default paths.
- `--assim` — override the raw observation archive path for the secondary mode (the supplied config sets it).
- `--extra-exp-files` — override the list of additional files uploaded directly from `$EXPDIR` to the remote plot directory (relative paths are also accepted).

---

## 5. Module reference

### `plot_gmao_obs.py`

The main plotting library. Every plotting function accepts a single file path
or a list of them; dates are parsed from names such as
`gmao-obs-2026092812.nc` or `gmao-obs-20260928_12.nc`.

- `plot_obs()` — Cartopy map of observations (all types coloured by type, or a
  single type coloured by value; optional geographic extent).
- `plot_timeseries_stacked_area()` — stacked daily counts on a real date axis.
- `plot_timeseries_stacked_area_combined()` — stacks by observation type
  instead of instrument.
- `plot_timeseries_stacked_area_multi()` — several stacked panels sharing one
  date axis.
- `plot_timeseries_stacked_area_panels()` — panels configured independently
  (used for CTD and XBT).
- `aggregate_daily_counts()` — shared `{date: {(typ, instid): count}}`
  counting primitive.
- `save_counts_history()` / `load_counts_history()` — upsert/read the flat
  counts CSV (`date,typ,instid,count`).
- `parse_trailing_window()` — parses `30d` / `2w` / `3m` / `1y` specifications.
- `discover_exp_gmao_files()` — finds production `gmao-obs-*.nc` files in an
  experiment, deduplicating by 6-hourly cycle across pentad boundaries so
  pentad-start days are not undercounted.
- `find_latest_complete_pentad()` — selects the newest fully written block for
  the primary workflow.
- `update_exp_counts_history()` — incrementally caches complete days from an
  experiment into its own counts-history CSV.

Stacked-area renderers are used instead of bar charts because a long trailing
window would otherwise put one x-axis tick on every calendar day.

### `generate_gmao_nc_optimized.py` (secondary capability)

- `OBS_MAP` maps an observation-type keyword (e.g. `Argo`, `CTD`) to the source
  file templates, variable, QC rules and error scaling for each contributing
  variable.
- `process_observation()` / `generate_nc()` — one cycle at a time.
- `process_observation_multi()` / `write_gmao_file()` / `generate_nc_batch()` —
  batch path: each source file is opened once for all requested cycles.
- `get_time_window()` — returns the centred ±3 h window bounds.
- Standalone invocation:

```bash
python3 generate_gmao_nc_optimized.py 2026 09 06 12 Argo CTD TAO
```

This writes `gmao-obs-2026090612.nc` in the current directory.

### `reproduce_gmao_files.py` (optional recovery utility)

See §3.1 for purpose and caveats. Reads the observation-type list from
`$ODAS_OBS` (§4.1). Options: `--outdir <dir>`, `--assim <path>`, and either
`--pentads <YYYYMMDD ...>` or `--range START END`.

---

## 6. Operational notes

- **NetCDF reads:** source files are opened without lazy chunking and the
  required variables are loaded eagerly.
- **Daily grouping:** a day covers `[00z, next day 00z)`; a trailing boundary
  day with only the 00z cycle is dropped from the daily maps.
- **Empty charts:** a chart or panel with no matching observations still renders
  with a dated axis and a "No observations found in this window" annotation, so
  the published plot set is complete for every experiment.
- **Cartopy:** the plotting module uses `$CARTOPY_DATA_DIR` as its Natural
  Earth shapefile cache.
- **Portal publication:** requires passwordless SSH to the configured host and
  write access to the configured base directory. Only the PNG plot directory
  (plus any `extra-exp-files`, sent directly from `$EXPDIR`) is published; no
  duplicate of those files is created under `www`.

---
