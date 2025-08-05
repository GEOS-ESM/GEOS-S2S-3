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

To build the model, in one step, go into the `src` directory and run the `parallel_build.csh` script. There are many options to
`parallel_build.csh`, which can be seen by running `./parallel_build.csh -h`.

```
cd src/
./parallel_build.csh
```

#### Specific Option for Building at NCCS

The NCCS build will currently default to building on Milan nodes. 
Although at present this build will work on Cascade Lake nodes as well, one can use the following command to specifically build on the Cascade Lake nodes:

./parallel_build.csh -cas
```

### Manual Build

To manually build the model, the steps are as follows:

```
cd src/
source g5_modules
make -jN pinstall |& tee make.log
```
where `N` is the number of cores to use for the build. If on a head node of a cluster, please only use at most two cores (e.g., `-j2`), but on a compute node or non-shared machine, you can use more cores (e.g., `-j10`).
