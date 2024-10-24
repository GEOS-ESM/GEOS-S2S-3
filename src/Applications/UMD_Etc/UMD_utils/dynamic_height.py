#! /usr/bin/env python

#module load other/comp/gcc-4.9.2-sp3 other/mpi/mvapich2-2.1/gcc-4.9.2-sp3 lib/mkl-15.0.2.164 other/SIVO-PyD/spd_1.24.0_gcc-4.9.2-sp3_mkl-15.0.2.164_mvapich2-2.1

from netCDF4 import Dataset
import numpy as np
import glob
import sys
import utils
from mpi4py import MPI
#Ne=int(sys.argv[1])

num_procs = MPI.COMM_WORLD.Get_size()
my_rank = MPI.COMM_WORLD.Get_rank()
my_name = MPI.Get_processor_name()

#Get center T, S and SLV
#=======================
center_fname='../wrkdir/center3d.nc'
ncfile=Dataset(center_fname, mode='r')
Tc=np.squeeze(ncfile.variables['T'][:])
Sc=np.squeeze(ncfile.variables['S'][:])
ncfile.close()

center_fname='../wrkdir/center2d.nc'
ncfile=Dataset(center_fname, mode='r')
SSHc=np.squeeze(ncfile.variables['SLV'][:])
ncfile.close()

bkg_flist=glob.glob('../wrkdir/bkg/???/ocean_temp_salt.res.nc')
bkg_flist.sort()

#Get ensemble member's background
#================================
file_index=list(range(my_rank,len(bkg_flist),num_procs))
for index in file_index:
    print('Worker ',my_rank,' comuting dynamic height for ensemble member ',file_index)
    bkg_fname = bkg_flist[index]
    Ti, Si, SSHi = utils.get_state(bkg_fname)

    dT=np.squeeze(Tc-Ti)
    dS=np.squeeze(Sc-Si)

    dsteric, dhalosteric, dthermosteric = utils.steric(Tc, Sc, dT, dS, SSHc-SSHi, SSHc)
    SSHi = SSHc + dsteric

    fname = bkg_fname[0:18]+'ocean_barotropic.res.nc'
    utils.write_mom_restart_barotropic(fname, SSHi)
