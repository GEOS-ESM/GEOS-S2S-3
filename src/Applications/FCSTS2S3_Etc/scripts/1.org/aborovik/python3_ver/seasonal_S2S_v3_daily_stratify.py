#! /usr/bin/env python
# plot stratified DAILY forecast plumes for various ensemble experiments with S2S-v2
#

import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from scipy.optimize import leastsq
import pickle
import math
import getopt
import sys
import glob
import os.path
import copy
import datetime
import functools
from sklearn.cluster import KMeans
from pandas import DataFrame
from calendar import monthrange

########################################################################################################################################
# Default vlaues
inputM = 'jan'
inputY = '2016'
expname = 'eh020' #'Chang_odas_V3V2'

# USER input
options, remainder = getopt.getopt(sys.argv[1:], 'm:y:e:')
print( options)
print( ' AVAILABLE OPTIONS:')
print( ' -m FORECAST month, e.g. dec (2nd month of the forecast, initialized in October)')
print( ' -y FORECAST year (must correspond to the FORECAST month)')
print( ' -e experiment name, e.g. eh020')
# ALL forecasts in monthly release will be processes at once, just as in old seasonal system
# e.g. /discover/nobackup/gmaofcst/ssdscr/geos5/seasonal/data/oct/2010/nino2ano.txt
for opt, arg in options:
    print( opt, arg)
    if opt in ('-m'):
        inputM=arg
    elif opt=='-y':
        inputY=arg
    elif opt=='-e':
        expname=arg
    else:
      print( 'bad option')
      exit()

filebase = '/discover/nobackup/projects/gmao/oceanval/post_processing/data/S2S-3_0/TEST_DAILY_ENSO/knakada_s2s3_unstable_11252019/'  # new location for V3 tests
################### END USER INPUT @@@@@@@@@@@@@@@@@@@@

reg = 'nino3.4'
###########################################################################################
# days in the monthM
months = dict(jan=1, feb=2, mar=3, apr=4, may=5, jun=6, jul=7, aug=8, sep=9, oct=10, nov=11, dec=12)
mnum = months[inputM]
numdays = monthrange(int(inputY), mnum)[1]
print( mnum, numdays)

yearIC = inputY
if (inputM == 'jan'):
   yearIC = str(int(inputY) - 1)
monIC = mnum - 2
if monIC <= 0:
   monIC = monIC+12
listMO=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
Fdir = listMO[monIC-1]
print( 'Fdir', Fdir)

# WRITE THIS OUT INTO A NEW TEXT FILE: only the "winners" ensemble labels
stratfl = 'winners_'+expname+'_'+reg+'_'+str(mnum).zfill(2)+'_'+inputY+'.txt'

########  DO NOT CHANGE THESE VALUES ###############################################################
Nclusters = 4
lag_offset = 0 # take into consideration only the the daily values after the lag_offset, i.e. from the 15th to the end of the (2nd) month for the lagged ICs (ens200)
base_period = [0, numdays] 
Nsamples = 10
######## END DO NOT CHANGE THESE VALUES #############################################################

# dates and ens number labels to record subsampled ens members
enslbls = [] 

# Dates convention changed for 2014 (back to forecasts date, not restarts)
if Fdir == 'jan':
   Ldates = ['0101','0106','0111','0116','0121','0126','0131']
elif Fdir == 'feb':
   Ldates = ['0205','0210','0215','0220','0225']
elif Fdir == 'mar':
   Ldates = ['0302','0307','0312','0317','0322','0327']
elif Fdir == 'apr':
   Ldates = ['0401','0406','0411','0416','0421','0426']
elif Fdir == 'may':
   Ldates = ['0501','0506','0511','0516','0521','0526','0531']
elif Fdir == 'jun':
   Ldates = ['0605','0610','0615','0620','0625','0630']
elif Fdir == 'jul':
   Ldates = ['0705','0710','0715','0720','0725','0730']
elif Fdir == 'aug':
   Ldates = ['0804','0809','0814','0819','0824','0829']
elif Fdir == 'sep':
   Ldates = ['0903','0908','0913','0918','0923','0928']
elif Fdir == 'oct':
   Ldates = ['1003','1008','1013','1018','1023','1028']
elif Fdir == 'nov':
   Ldates = ['1102','1107','1112','1117','1122','1127']
elif Fdir == 'dec':
   Ldates = ['1202','1207','1212','1217','1222','1227']
#Lmems = ['200','201','202','203','204']
LmemsA = ['400','401','402','403','404']
LmemsO = ['501','502','503','504','505','506','507','508','509','510']
# ASSUME that the LAST date has 10 extra members

# Let ensS2S0 be the burst set of forecasts with ATM perturbations
NensS2S0 = len(Ldates)*len(LmemsA) # number of dates times number of members per date
ensS2S0 = np.empty(( NensS2S0,numdays ))
ensS2S0[:] = np.nan

lcnt = 0
for d in Ldates:
   for mem in LmemsA: 
      Fdir0 = yearIC+d
      IN_dir = filebase+Fdir0+'/'+inputY+'/'+inputM
      fnsst = IN_dir+'/DAILY_'+reg+'_'+str(mem)+'.sst'
      print( fnsst)
      if os.path.exists(fnsst):
         fnIN = open(fnsst, 'r')
      else:
         print( 'input file with full indices is not found ', fnsst)
         exit()
      hdstrIN = fnIN.readline()
      #print( 'mem',mem, 'hdstrIN ', hdstrIN)
      ln = fnIN.readline()
      ln1 = ln.strip()
      ln2 = ln1.split()
      lnIN = [float(item) for item in ln2]  # map(float,ln2)
      fnIN.close()
      ensS2S0[lcnt,:] = lnIN
      lcnt = lcnt + 1
      enslbls.append(Fdir0+' ens'+str(mem))

# Let ensS2S2 be the burst set of forecasts with OCN perturbations
NensS2S2 = len(LmemsO) 
ensS2S2 = np.empty(( NensS2S2,numdays ))
ensS2S2[:] = np.nan

ecnt = 0
for mem in LmemsO: 
   IN_dir = filebase+Fdir0+'/'+inputY+'/'+inputM
   fnsst = IN_dir+'/DAILY_'+reg+'_'+str(mem)+'.sst'
   print( fnsst)
   if os.path.exists(fnsst):
      fnIN = open(fnsst, 'r')
   else:
      print( 'input file with full indices is not found ', fnsst)
      exit()
   hdstrIN = fnIN.readline()
   #print( 'mem',mem, 'hdstrIN ', hdstrIN)
   ln = fnIN.readline()
   ln1 = ln.strip()
   ln2 = ln1.split()
   lnIN = [float(item) for item in ln2]  # map(float,ln2)
   fnIN.close()
   ensS2S2[ecnt,:] = lnIN
   ecnt = ecnt + 1
   enslbls.append(Fdir0+' ens'+str(mem))


GRAND_ENS = np.concatenate( (ensS2S0[lag_offset:,:],ensS2S2[:,:]),axis = 0 )
Ntotal = NensS2S0 - lag_offset + NensS2S2
print( 'GRAND_ENS dims', np.shape(GRAND_ENS))
print( 'GRAND_ENS')
with np.printoptions(threshold=np.inf):
    print(GRAND_ENS)

########################### FINISH READING MODEL AND OBS DATA ##############################

Data = {}
clmns = []
for d in range(base_period[0],base_period[1]):
   dkey = 'y'+str(d)
   dval = np.concatenate( (ensS2S0[lag_offset:,d],ensS2S2[:,d]),axis = 0 )
   Data[dkey] = dval
   clmns.append(dkey)

df = DataFrame(Data,columns=clmns)

kmeans = KMeans(n_clusters=Nclusters).fit(df)
centroids = kmeans.cluster_centers_
#print(centroids)
cllabels = kmeans.labels_
unique_elements, counts_elements = np.unique(cllabels, return_counts=True)
print( 'initial clisters:')
print( unique_elements)
print( counts_elements)
print( cllabels)

unique_elements, counts_elements = np.unique(cllabels, return_counts=True)

# sort clusters based on their population
sort_inds = np.argsort(counts_elements)
print( 'sorted ', counts_elements, sort_inds)

# from here down everything is done with the sorted clusters
centroids = np.empty((Nclusters,numdays))
for i in sort_inds[0:Nclusters]:  # i in range(0,Nclusters):
   indxs = np.asarray(cllabels == unique_elements[i]).nonzero()
   for d in range(0,numdays):
      centroids[i,d] = np.mean(GRAND_ENS[indxs,d])
   print( 'indxs',indxs)
   #print( centroids[i])

C = []
sample_sizes = np.zeros(Nclusters)

# at random, pick ns elements from the cluster; ns is proportional to the cluster size
# do this for Nclusters-1, sorted from smaller to larger; add up sample_sizes for these Nclusters-1 clusters = Sn1; pick Nsamples - Sn1 elements from the most populous cluster
Sn1 = 0
for i in sort_inds[0:Nclusters-1]:   #range(0,Nclusters):
   indxs = np.asarray(cllabels == unique_elements[i]).nonzero()
   ilen =  len(np.ravel(indxs)) # len(np.squeeze(indxs))
   ns = max(int(round(float(ilen)*float(Nsamples)/float(Ntotal))),1)
   Sn1 = Sn1 + ns
   if ilen > 1:
      sample_indxs = np.random.choice(np.squeeze(indxs),ns,replace=False)
   else:
      sample_indxs = indxs[0]
   sample_sizes[i] = len(sample_indxs)
   C.append(sample_indxs)
   print( 'i, sample_indxs', i, sample_indxs)
# deal with the most populous cluster 
i = sort_inds[Nclusters-1]
indxs = np.asarray(cllabels == unique_elements[i]).nonzero()
ilen =  len(np.ravel(indxs)) # len(np.squeeze(indxs))
ns = Nsamples - Sn1
sample_indxs = np.random.choice(np.squeeze(indxs),ns,replace=False)
sample_sizes[i] = len(sample_indxs)
print( 'i, sample_indxs', i, sample_indxs)
C.append(sample_indxs)

# test the sub-sampled ensemble size
if (sum(sample_sizes) != Nsamples):
   print( 'sample_sizes',sample_sizes, sum(sample_sizes))
   print( 'NEED TO THINK ABOUT THIS!!!')
   exit()

# Ct is to sort (order) the date/ens labels
Ct = [item for sublist in C for item in sublist]
print( 'Ct',sorted(Ct))

smplenslbls = []
for ict in sorted(Ct):
   smplenslbls.append(enslbls[ict])
print( 'enslbls', enslbls)
print( 'sample enslbls', smplenslbls)
fn = open(stratfl,'w')
format = []
for i in range(0,len(smplenslbls)):
   fn.write(smplenslbls[i])
   fn.write("\n")
fn.close()
#################################################################################
exit()


