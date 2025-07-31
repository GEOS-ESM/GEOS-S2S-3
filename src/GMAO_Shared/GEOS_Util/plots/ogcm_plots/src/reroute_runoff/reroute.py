#!/usr/bin/env python

import os
import numpy as np
import netCDF4 as nc
from numpy import f2py

try:
    from reroute import reroute
except ImportError:
    with open("reroute.f90") as sourcefile:
        sourcecode = sourcefile.read()
    f2py.compile(sourcecode, modulename='reroute',extension='.f90')
    from reroute import reroute

dir=os.environ['NOBACKUP']+'/workdir/move_runoff'

# Read ocean land mask
print('Reading ocean land mask')
#gfile=dir+'/grid_spec.nc'
#with nc.Dataset(gfile) as ff:
#    wet=ff.variables['wet'][:]
#wet_orig=wet.copy()

gfile=dir+'/ocean_mask.nc'
with nc.Dataset(gfile) as ff:
    wet=ff.variables['mask'][:]
wet_orig=wet.copy()

# Apply optional mask
#with open('route_mask_cm25.txt') as ff:
#    ff.readline()
#    for ss in ff.readlines():
#        iind,jind=[slice(*map(int,sss.split(':'))) for sss in ss.split(',')]
#        wet[jind,iind] = 0

sh=wet.shape
tmp=np.zeros((sh[0]+2,sh[1]+2))
tmp[1:-1,1:-1]=wet
nwet=tmp[1:-1,1:-1]+tmp[:-2,:-2]+tmp[1:-1,:-2]\
    +tmp[2:,:-2]+tmp[2:,1:-1]+tmp[2:,2:]\
    +tmp[1:-1,2:]+tmp[:-2,2:]+tmp[:-2,1:-1]

# Make slmask
slmask=wet.copy()
slmask[nwet<6.0]=0.0

# Read routing table
print('Reading routing data')
runoff_table=dir+'/runoff.bin'

Nt=np.fromfile(runoff_table,dtype='i4',count=3)[1]

vlist=[('','i4'),('Nt','i4'),('','i4'),
       ('','i4'),('sind','i4',Nt),('','i4'),
       ('','i4'),('dind','i4',Nt),('','i4'),
       ('','i4'),('weight','f4',Nt),('','i4')]

route=np.fromfile(runoff_table,dtype=vlist);

# Read ocean i,j corresponding to tmp from tile file
print('Reading tile data ')
tfile=dir+'/tile.dat'
cols=(0,1,2,3,8,9)
rec=[('type','i4'),('area','f4'),('lon','f4'),('lat','f4'),
     ('ii','i4'),('jj','i4')]
tmp=np.loadtxt(tfile, skiprows=8,usecols=cols,dtype=rec); 
#np.save(dir+'/tdata.npy',tdata)
#tdata=np.load(dir+'/tdata.npy')

# Append tile number
rec.append(('tnum','i4'))
tdata=np.array(np.zeros(tmp.shape),dtype=rec)
tdata['type'][:]=tmp['type'][:]
tdata['area'][:]=tmp['area'][:]
tdata['lon'][:]=tmp['lon'][:]
tdata['lat'][:]=tmp['lat'][:]
tdata['ii'][:]=tmp['ii'][:]
tdata['jj'][:]=tmp['jj'][:]
tdata['tnum']=np.arange(tdata.size)+1
del tmp

# Subset ocean only tiles 
otdata=tdata[np.where(tdata['type']==0)[0]]

# Remove tiles which are over ocean land (slmask[i,j]==0)
tslmask=np.zeros(otdata.size)
for tt, ind in enumerate(otdata[['jj','ii']]):
    tslmask[tt]=slmask[ind[0]-1,ind[1]-1]
otdata=otdata[np.where(tslmask > 0)[0]]

# Rerouting discharge
print('Rerouting discharge...')
ind=(route['sind']!=route['dind'])
Nt=route['dind'][ind].size
vlist=[('','i4'),('Nt','i4'),('','i4'),
       ('','i4'),('sind','i4',Nt),('','i4'),
       ('','i4'),('dind','i4',Nt),('','i4'),
       ('','i4'),('weight','f4',Nt),('','i4')]

route_new=np.array(np.zeros(1,),dtype=vlist)
route_new['f0']=4
route_new['Nt']=Nt
route_new['f2']=4
route_new['f3']=Nt*4
route_new['sind']=route['sind'][ind]
route_new['f5']=Nt*4
route_new['f6']=Nt*4
route_new['dind'],route_new['weight']=reroute(np.ascontiguousarray(route['dind'][ind]),
                                              np.ascontiguousarray(route['weight'][ind]),                    
                                              np.ascontiguousarray(slmask),
                                              np.ascontiguousarray(tdata['ii']), 
                                              np.ascontiguousarray(tdata['jj']),
                                              np.ascontiguousarray(tdata['lon']), 
                                              np.ascontiguousarray(tdata['lat']),
                                              np.ascontiguousarray(tdata['area']),
                                              np.ascontiguousarray(tdata['type']),
                                              np.ascontiguousarray(otdata['tnum']))
route_new['f8']=Nt*4
route_new['f9']=Nt*4
route_new['f11']=Nt*4

route_new.tofile(dir+'/runoff_new.bin')
print('...done')

dstind_old=route['dind'][ind]
gind=np.zeros((Nt,4),dtype='i4')
gind[:,0]=tdata[dstind_old-1]['ii']-1
gind[:,1]=tdata[dstind_old-1]['jj']-1
gind[:,2]=tdata[route_new['dind'][0]-1]['ii']-1
gind[:,3]=tdata[route_new['dind'][0]-1]['jj']-1
