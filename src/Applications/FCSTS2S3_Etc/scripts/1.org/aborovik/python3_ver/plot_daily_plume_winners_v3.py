#! /usr/bin/env python
# plot stratified DAILY forecast plumes for various ensemble experiments with S2S-v3
# all members and highlighted winners (10)

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
from calendar import monthrange

########################################################################################################################################
# Default vlaues
inputM = 'dec'
inputY = '2015'
reg = 'nino3.4'
expname = 'eh020' #'Chang_odas_V3V2'

# USER input
options, remainder = getopt.getopt(sys.argv[1:], 'm:y:r:f:e:')
print( options)
print( ' AVAILABLE OPTIONS:')
print( ' -m FORECAST mon: jan, feb, etc - one at a time')
print( ' -y FORECAST year')
print( ' -r REGION: nino3, nino4, nino1+2, nino3.4, (not yet) nino1, nino2, tasi, ta, idm, idm_west, idm_east')
# ALL forecasts in monthly release will be processes at once, just as in old seasonal system
# e.g. /discover/nobackup/gmaofcst/ssdscr/geos5/seasonal/data/oct/2010/nino2ano.txt
for opt, arg in options:
    print( opt, arg)
    if opt in ('-m'):
        inputM=arg
    elif opt=='-y':
        inputY=arg
    elif opt=='-r':
        reg=arg
    else:
      print( 'bad option')

########################################################################################################################################
filebase = '/discover/nobackup/projects/gmao/oceanval/post_processing/data/S2S-3_0/TEST_DAILY_ENSO/knakada_s2s3_unstable_11252019/'  # new location for V3 tests

# days in the monthM
months = dict(jan=1, feb=2, mar=3, apr=4, may=5, jun=6, jul=7, aug=8, sep=9, oct=10, nov=11, dec=12)
mnum = months[inputM]
numdays = monthrange(int(inputY), mnum)[1]
print( mnum, numdays)

########################################################################################################################################
#winfile = 'winners_'+expname+'_'+reg+'_'+str(mnum).zfill(2)+'_'+inputY+'.txt'
winfile = '../SAMPLE_winners_'+expname+'_'+reg+'_'+str(mnum).zfill(2)+'_'+inputY+'.txt'

yearIC = inputY
if (inputM == 'jan'):
   yearIC = str(int(inputY) - 1)
monIC = mnum - 2
if monIC <= 0:
   monIC = monIC+12
listMO=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
Fdir = listMO[monIC-1]
print( 'Fdir', Fdir)

########################################################################################################################################
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

########################################################################################################################################
Nsamples = 10

# dates and ens number labels to record subsampled ens members
enslbls = [] 

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
      #print(fnsst)
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
      lnIN = [float(item) for item in ln2]  #map(float,ln2)
      fnIN.close()
      ensS2S0[lcnt,:] = lnIN
      lcnt = lcnt + 1
      enslbls.append(Fdir0+' ens'+str(mem))

# Let ensS2S2 be the burst set of forecasts with OCN perturbations
NensS2S2 =  len(LmemsO) 
ensS2S2 = np.empty(( NensS2S2,numdays ))
ensS2S2[:] = np.nan

ecnt = 0
for mem in LmemsO: 
   #IN_dir = filebase+Fdir+'/'+inputY+'/'+inputM
   IN_dir = filebase+Fdir0+'/'+inputY+'/'+inputM
   fnsst = IN_dir+'/DAILY_'+reg+'_'+str(mem)+'.sst'
   #print( fnsst)
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
   lnIN = [float(item) for item in ln2]  #map(float,ln2)
   fnIN.close()
   ensS2S2[ecnt,:] = lnIN
   ecnt = ecnt + 1
   enslbls.append(Fdir0+' ens'+str(mem))

GRAND_ENS = np.concatenate( (ensS2S0[:,:],ensS2S2[:,:]),axis = 0 )
Ntotal = NensS2S0 + NensS2S2
print( 'GRAND_ENS', np.shape(GRAND_ENS))

# initialize obs and analysis (if any) 
#obs = np.empty(( numdays ))
#anaS2S = np.empty(( numdays ))
#obs[:] = np.nan
# NOW OBS @*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*
#  Forget about obs for now: need to make daily ENSO indices.  Can use some of the stuff in computer_reynolds_anomaly.py (OCEAN_CLIMATE) and make_reyn_indices_text.py (post_processing)
################# FINISH READING MODEL AND OBS DATA #############################

########### SET REININD and Y-LIMS FOR PLOTTING ###################################
if reg == 'nino1':
   regttl = 'Ni'+chr(241)+'o 1'
   reynind = 0
elif reg == 'nino2':
   regttl = 'Ni'+chr(241)+'o 2'
   reynind = 1
elif reg == 'nino3':
   regttl = 'Ni'+chr(241)+'o 3'
   reynind = 3
elif reg == 'nino4':
   regttl = 'Ni'+chr(241)+'o 4'
   reynind = 5
elif reg == 'nino1+2':
   regttl = 'Ni'+chr(241)+'o 1+2'
   reynind = 2
elif reg == 'nino3.4':
   regttl = 'Ni'+chr(241)+'o 3.4'
   reynind = 4
else:
   print( 'incorrect region specified')
   exit()

maxv1 = math.ceil(np.nanmax(np.ravel(GRAND_ENS)))
minv1 = math.floor(np.nanmin(np.ravel(GRAND_ENS)))
ylims = [minv1,maxv1]

if (ylims[1]-ylims[0]<=4):
   ytks = np.arange(ylims[0],ylims[1]+0.1,0.5) 
else:
   ytks = np.arange(ylims[0],ylims[1]+0.1,1) 
###################### END SET Y-LIMS FOR PLOTTING #######################################

smplenslbls = []
# Read winners file to highlight selected members.
fn = open(winfile,'r')
for i in range(0,Nsamples):
   ln = fn.readline()
   ln = ln.rstrip()
   smplenslbls.append(ln)
fn.close()

print( 'enslbls', enslbls)
print( 'sample enslbls', smplenslbls)

#################################################################################

cluster_color_dict = {'C0': 'Olive', 'C1': 'Purple', 'C2':'Sienna','C3':'Teal','C4':'OrangeRed','C5':'LightSlateGray','C6':'DarkSlateGray','C7':'DeepPink','C8':'Blue','C9':'Gold'}

############################################## ***********sub-sampled members highlighted *************** ###############################################
fig, ax = plt.subplots(num=1,figsize=(8, 5))

for ic in range(0,Ntotal):
   if enslbls[ic] in smplenslbls:
      colorname = 'C9'
      lnS, = plt.plot(np.arange(0,numdays),GRAND_ENS[ic,:], color=cluster_color_dict[colorname], linestyle = '-', zorder=20,alpha=1, linewidth = 1)
   else:
      colorname = 'C8'
      lnS, = plt.plot(np.arange(0,numdays),GRAND_ENS[ic,:], color=cluster_color_dict[colorname], linestyle = '-', zorder=10,alpha=0.5, linewidth = 0.5)

#ax.legend(lnc,slbl,loc=9,ncol=Nclusters,frameon=False,labelspacing=0.25,prop={'size':14})

plt.ylim(ylims)
plt.yticks(ytks)
plt.xticks(np.arange(0,numdays,5))
plt.xlabel('days')
plt.ylabel('C')
plt.grid(True)
plt.text(0.15, 0.1, regttl,horizontalalignment='center',verticalalignment='center',transform = ax.transAxes,style='italic')
plt.text(0.5, 0.1, 'IC: '+Fdir+'  FCST: '+inputM+' '+inputY,horizontalalignment='center',verticalalignment='center',transform = ax.transAxes,style='italic')

plt.savefig('subsample_V3_'+reg+'_'+inputM+'_'+inputY+'_daily.png', dpi=150, bbox_inches='tight', pad_inches=0.1)

plt.show()
exit()

