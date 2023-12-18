#! /usr/bin/env python

# Read Argo ListOfFloatsToBeChecked Lists
# Write wmo, date, var in a text file for fortran
# ftp://ftp.ifremer.fr/ifremer/argo/etc/argo-ast9-item13-AltimeterComparison

import numpy as np
import array
import sys
import scipy.io
import string
import glob
from datetime import date

#sys.path.append('/gpfsm/dhome/gmaofcst/ODAS/OBS/V2/UTILS/')
sys.path.append('/discover/nobackup/lren1/pre_proc/NRT/UTILS/')

########################################################################

if len(sys.argv) > 3:
  yyyy = sys.argv[1]
  mm  = sys.argv[2]
  dd  = sys.argv[3] 

mm=str(mm).zfill(2)
dd=str(dd).zfill(2)
yyyy=str(yyyy).zfill(4)
pyyy=str(int(yyyy)-1).zfill(4)


def find_whitespace(st):
    for index, character in enumerate(st):
       if character in string.whitespace:
            yield index

# DAC, WMO, INST, ANOMALIES, RT/DT, CYCLES
tm = 3000;
wmo   = np.zeros(tm,int)
inst  = np.zeros(tm,int)

flist=glob.glob('Floats_*2009.txt')
flist.sort()

print flist

i=0
for n in range(len(flist)):
   fname=flist[n]
   print fname
   f = open(fname,'r')   
   while i>=0:
     line = f.readline()
#     ind = list(find_whitespace(line))
     ind=[i for i, item in enumerate(line) if '\t' in item]
     print line
     if len(ind)==0:
        print 'end of file'
        break
     print ind
     if ind[1]==ind[2]-1:
        inst[i]  = line[ind[2]+1:ind[3]]
     else: 
        inst[i]  = line[ind[1]+1:ind[2]]

     wmo[i]  = int(line[ind[0]+1:ind[1]])       
     if ind[1]==ind[2]-1:
        inst[i]  = line[ind[2]+1:ind[3]]
     else: 
        inst[i]  = line[ind[1]+1:ind[2]]


     print i, wmo[i],inst[i]
     if inst[i] < 100:
        print 'error in inst num'
     i=i+1  
   f.close()

nrows = i
guwmo = np.unique(wmo)


# Write new file
fout  = open('ARGO_CHECKLIST_test.txt','w')
for i in range (0,nrows):
   fout.write('{:<7d}\n'.format(guwmo[i]))
fout.close


# Lets get the set of WMO's that are in the CheckList but NOT in the GreyList
mat = scipy.io.loadmat('ARGO_GREYLIST_test.mat')
uwmo=mat['uwmo'][0,:]
wmodiff = set(guwmo)-set(uwmo)
print wmodiff
# Write new file
fout = open('ARGO_CHECKLIST_DIFF_test.txt','w');
for i in range(len(wmodiff)):
   fout.write('{:<7d}\n'.format(wmodiff[i]))
fout.close()  


