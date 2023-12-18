#! /usr/bin/env python


# Read Argo Grey List text file
# Write wmo, date, var in a text file for fortran
# ftp://ftp.ifremer.fr/ifremer/argo/ar_greylist.txt

import numpy as np
import array
import sys
import scipy.io
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


today=date.today()
today=today.strftime("%Y%m%d")


# WMO, VAR, DATE,empty,QC,comment, dac
#fname = 'ar_greylist_' + today+'.txt'
fname ='argo_greylist.txt'
# get number of lines in the file
tm = sum(1 for line in open(fname))
print tm
f   = open(fname,'r')
wmo   = np.zeros(tm,int)
ivar  = np.zeros(tm,int)
idate1 = np.zeros(tm,int)
idate2 = np.zeros(tm,int)

  
i = 0
while i>=0:
  line = f.readline()
  ind = [j for j in range(len(line)) if line.startswith(",", j)]
  if len(ind)==0:
    print 'end of file'
    break
  
  wmo[i] = int(line[0:ind[0]])
  tmp=line[ind[0]+1:ind[1]]
  ivar[i]=1
  if tmp =='TEMP':
    ivar[i] = 1

  if tmp=='PSAL':
    ivar[i] = 2

  if tmp=='PRES':
     ivar[i] = 3
 
  idate1[i] = int(line[ind[1]+1:ind[2]])
  tmp = line[ind[2]+1:ind[3]]
  if len(tmp)>1:
     idate2[i] = int(tmp)
  else:
     idate2[i] = int(today)
#  print wmo[i],ivar[i],idate1[i],idate2[i]
  i = i +1
nrows = i
f.close()
# Sort by date

sv = np.column_stack((wmo,ivar,idate1, idate2))
sv = sv[sv[:,0].argsort()]
# Write new file
fout  = open('ARGO_GREYLIST_test.txt','w')
for i in range (0,nrows):
   print sv[j,0],sv[i,1],sv[i,2],sv[i,3]
   fout.write('{:<7d}{:<1d}{:<8d}{:<8d}\n'.format(sv[i,0], sv[i,1], sv[i,2], sv[i,3]))
fout.close

uwmo = np.unique(wmo);
scipy.io.savemat('ARGO_GREYLIST_test.mat', mdict={'uwmo': uwmo})

