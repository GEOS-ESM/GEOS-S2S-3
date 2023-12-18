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

flist=glob.glob('Floats_*.txt')
flist.sort()

print flist

i=0
for n in range(len(flist)):
   fname=flist[n]
   print fname
   f = open(fname,'r')   
   while i>=0:
     line = f.readline()
#     print line
     ind = list(find_whitespace(line))
     if len(ind)<=1:
       print 'end of file'
       break
#     print ind
     for k in range(1,len(ind)):
        if int(ind[k]) == int(ind[k-1])+1:
          s1 = int(ind[k])
        else:
          break
     if k == 1:
       s1 = int(ind[0])+1
       e1 = int(ind[1])
     else:
       e1 = int(ind[k])  

     kk = k
     s2=int(ind[k])
     for k in range(kk+1,len(ind)):
        if int(ind[k])==int(ind[k-1])+1:
           s2=int(ind[k])
        else:
           break
     e2 = int(ind[k])
     
     wmo[i]  = int(line[s1:e1])
     
     inst[i]  = int(line[s2:e2])
     
     if inst[i]<100:
        print 'error in inst num'
     i=i+1  
   f.close()

nrows = i-1
wmo2=wmo[0:nrows]
guwmo = np.unique(wmo2)
guwmo = guwmo[guwmo.argsort()]

nrows2=np.shape(guwmo)[0]
# Write new file
fout  = open('ARGO_CHECKLIST_test.txt','w')
for i in range (nrows2):
  fout.write('{:<7d}\n'.format(guwmo[i]))
fout.close


# Lets get the set of WMO's that are in the CheckList but NOT in the GreyList
mat = scipy.io.loadmat('ARGO_GREYLIST_test.mat')
uwmo=mat['uwmo'][0,:]
wmodiff = set(guwmo)-set(uwmo)
wmodiff = list(wmodiff)
wmodiff2=np.zeros(len(wmodiff),int)

# Write new file
fout = open('ARGO_CHECKLIST_DIFF_test.txt','w');
for i in range(len(wmodiff)):
   wmodiff2[i]=int(wmodiff[i])
wmodiff2 = wmodiff2[wmodiff2.argsort()]

for i in range(len(wmodiff2)):
   fout.write('{:<7d}\n'.format(wmodiff2[i]))
fout.close()  


