# GEOS-S2S-3

This repository contains the GEOS-S2S-3 code from CVS.

It has a submodule of `MOM5` from the [GEOS-ESM fork of MOM5](https://github.com/GEOS-ESM/MOM5).

## Cloning

To clone this model and its submodule, use the following command:

```
git clone --recurse-submodules https://github.com/GEOS-ESM/GEOS-S2S-3.git
```

## Building

### One-Step Build

To build the model, in one step, go into the `src` directory and run the `parallel_build.csh` script:

```
cd src/
./parallel_build.csh
```

### Manual Build

To manually build the model:

```
cd src/
source g5_modules
make -jN pinstall |& tee make.log
```
where `N` is the number of cores to use for the build. If on a head node of a cluster, please only use at most two cores (e.g., `-j2`), but on a compute node or non-shared machine, you can use more cores (e.g., `-j10`).
