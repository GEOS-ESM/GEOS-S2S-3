#! /usr/bin/env python
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import sys 
import glob
import numpy as np
from netCDF4 import Dataset
from datetime import date
import datetime
import matplotlib.dates as mdates

obsname = sys.argv[1]
region  = sys.argv[2]
lev1    = sys.argv[3]
lev2    = sys.argv[4]

# obstype: SST, SSS, ADT, Tprof, Sprof, Ice Fraction


#obsid_dict = {5525:'SST', 5522:'SSS', 5351:'ADT', 3073: 'Tprof', 5521: 'Sprof', 6000:'Ice Fraction'} 
obsid_dict = {'SST':5525, 'SSS':5522, 'ADT': 5351,'Tprof': 3073, 'Sprof':5521, 'Ice Fraction': 6000} 

def smooth(x,window_len=11,window='flat'):
    """smooth the data using a window with requested size.                                                                                                                                           
                                                                                                                                                                                                     
    This method is based on the convolution of a scaled window with the signal.                                                                                                                      
    The signal is prepared by introducing reflected copies of the signal                                                                                                                             
    (with the window size) in both ends so that transient parts are minimized                                                                                                                        
    in the begining and end part of the output signal.                                                                                                                                               
                                                                                                                                                                                                     
    input:                                                                                                                                                                                           
        x: the input signal                                                                                                                                                                          
        window_len: the dimension of the smoothing window; should be an odd integer                                                                                                                  
        window: the type of window from 'flat', 'hanning', 'hamming', 'bartlett', 'blackman'                                                                                                         
            flat window will produce a moving average smoothing.                                                                                                                                     
                                                                                                                                                                                                     
    output:                                                                                                                                                                                          
        the smoothed signal                                                                                                                                                                          
                                                                                                                                                                                                     
    example:                                                                                                                                                                                         
                                                                                                                                                                                                     
    t=linspace(-2,2,0.1)                                                                                                                                                                             
    x=sin(t)+randn(len(t))*0.1                                                                                                                                                                       
    y=smooth(x)                                                                                                                                                                                      
                                                                                                                                                                                                     
    see also:                                                                                                                                                                                        
                                                                                                                                                                                                     
    numpy.hanning, numpy.hamming, numpy.bartlett, numpy.blackman, numpy.convolve                                                                                                                     
    scipy.signal.lfilter                                                                                                                                                                             
                                                                                                                                                                                                     
    TODO: the window parameter could be the window itself if an array instead of a string                                                                                                            
    Stolen from: http://www.scipy.org/Cookbook/LinearRegression?highlight=%28regress%29                                                                                                              
    """

    #if x.ndim != 1:                                                                                                                                                                                 
    #    raise ValueError, "smooth only accepts 1 dimension arrays."                                                                                                                                 

    #if x.size < window_len:                                                                                                                                                                         
    #    raise ValueError, "Input vector needs to be bigger than window size."                                                                                                                       

    if window_len<3:
        return x

    if not window in ['flat', 'hanning', 'hamming', 'bartlett', 'blackman','std']:
        raise ValueError ("Window is one of 'flat', 'hanning', 'hamming', 'bartlett', 'blackman'")

    if window in ['flat', 'hanning', 'hamming', 'bartlett', 'blackman']:
        s=np.r_[2*x[0]-x[window_len:1:-1],x,2*x[-1]-x[-1:-window_len:-1]]
        if window == 'flat': #moving average                                                                                                                                                         
            w=np.ones(window_len,'d')
        else:
            w=eval('np.'+window+'(window_len)')

        tmpy=np.convolve(w/w.sum(),s,mode='same')
        y=tmpy[window_len-1:-window_len+1]

        print

    if window in ['std']:
        y = std_filter(x,n_std=float(window_len))

    y[-window_len+1:]=np.nan                                                                                                                                                                        
    y[0:window_len]=np.nan                                                                                                                                                                          

    return y

def std_filter(x,n_std=3.):

    y=x
    std=scstats.nanstd(x)
    mean=scstats.nanmean(x)
    for iter in range(0,4):
        y[ np.where( (y>mean+n_std*std) | (y<mean-n_std*std) ) ] = np.nan

    return y

class ExpHist():
    def __init__(self):
        self.date=[]
        self.mae_omf=[]
        self.mae_oma=[]
        self.bias_omf=[]
        self.bias_oma=[]

class OdaStats():
    def __init__(self, fname='obs01020.dat.nc',alpha=1.0, marker='o'):

        self.alpha=alpha
        self.marker=marker
        ncfile = Dataset(fname, 'r')
        self.fname=fname
        self.OBSID=ncfile.variables['OBSID'][:]
        self.lon=ncfile.variables['lon'][:]
        self.lat=ncfile.variables['lat'][:]
        self.lev=ncfile.variables['lev'][:]
        self.obs=ncfile.variables['obs'][:]
        self.sigo=ncfile.variables['sigo'][:]
        self.omf=ncfile.variables['omf'][:]  
        self.fcst=self.obs-self.omf
        try:
          self.oma=ncfile.variables['oma'][:]
          self.OBSIDa=ncfile.variables['OBSIDa'][:]
          self.lona=ncfile.variables['lona'][:]
          self.lata=ncfile.variables['lata'][:]
          self.leva=ncfile.variables['leva'][:]
          self.obsa=ncfile.variables['obsa'][:]
          self.sigoa=ncfile.variables['sigoa'][:]
          self.fcsta=self.obsa-self.oma
          self.std_oma=ncfile.variables['std_oma'][:]
        except:
          self.oma=np.nan*self.omf

        self.std_omf=ncfile.variables['std_omf'][:]
    #    self.std_oma=ncfile.variables['std_oma'][:]  

        self.yyyymmdd=fname[-30:-26]+'-'+fname[-26:-24]+'-'+fname[-24:-22] #fname[-27:-23]+'-'+fname[-23:-21]+'-'+fname[-21:-19]
        self.time=int(fname[-30:-22])
        self.date=datetime.datetime(int(fname[-30:-26]), int(fname[-26:-24]), int(fname[-24:-22]), int(fname[-21:-19]))
        ncfile.close()
    def mae(self, obstype=3073, region='glb'):
        self.IDS=np.unique(self.OBSID)
        ids = obstype
        
        if region=='glb':
          if obsname=='Tprof' :
#             toobig=10.0
              toobig=5.0
          if obsname=='Sprof':
              toobig=5.0
          if obsname=='ADT':
             toobig=1.0
           
          I = np.where( (self.OBSID==ids) & (np.abs(self.lev)<float(lev2)) & (self.fcst!=0.0) & (np.abs(self.lat)<90.0) & (np.abs(self.omf)<toobig))
          try:
             Ia = np.where( (self.OBSIDa==ids) & (np.abs(self.leva)<float(lev2)) & (self.fcsta!=0.0) & (np.abs(self.lata)<90.0) & (np.abs(self.oma)<10.0))
          except:
             print('No OMA')
        if region=='trp':
          if obsname=='Tprof' :
             toobig=10.0
          if obsname=='Sprof':
             toobig=5.0
          if obsname=='ADT':
             toobig=1.0
          if obsname=='SSS':
             toobig=1.0

          I = np.where( (self.OBSID==ids) & (np.abs(self.lev)<float(lev2)) & (self.fcst!=0.0) & (np.abs(self.lat)<30.0) & (np.abs(self.omf)<toobig))
          try:
              Ia = np.where( (self.OBSIDa==ids) & (np.abs(self.leva)<float(lev2)) & (self.fcsta!=0.0) & (np.abs(self.lata)<30.0) & (np.abs(self.oma)<toobig))
          except:
              print('No OMA')
        if region=='north':
           if obsname=='Tprof' :
              toobig=10.0
           if obsname=='Sprof':
              toobig=5.0
           if obsname=='ADT':
              toobig=1.0

           I = np.where( (self.OBSID==ids) & (np.abs(self.lev)<float(lev2)) & (self.fcst!=0.0) & (self.lat>0.0) & (np.abs(self.omf)<toobig) )
           try:
               Ia = np.where( (self.OBSIDa==ids) & (np.abs(self.leva)<float(lev2)) & (self.fcsta!=0.0) & (self.lata>0.0) & (np.abs(self.oma)<toobig) )
           except:
               print('No OMA')
        if region=='south':
           if obsname=='Tprof' :
              toobig=10.0
           if obsname=='Sprof':
              toobig=5.0
           if obsname=='ADT':
              toobig=1.0
           
           I = np.where( (self.OBSID==ids) & (np.abs(self.lev)<float(lev2)) & (self.fcst!=0.0) & (self.lat<0.0) & (np.abs(self.omf)<toobig) )
           try:
              Ia = np.where((self.OBSIDa==ids) & (np.abs(self.leva)<float(lev2)) & (self.fcsta!=0.0) & (self.lata<0.0) & (np.abs(self.oma)<toobig) )
           except:
              print('No OMA')
 
        self.mae_omf = np.mean(np.abs(self.omf[I]))
        self.bias_omf = np.mean(self.omf[I])
        try:
          self.mae_oma = np.mean(np.abs(self.oma[Ia]))
          self.bias_oma = np.mean(self.oma[Ia])
        except:
          self.mae_oma  = np.nan*self.mae_omf
          self.bias_oma = np.nan*self.bias_omf
 
 
        self.nobs=np.shape(I)[1]
 
        return self.date, self.mae_omf, self.mae_oma, self.bias_omf, self.bias_oma, self.nobs




#plt.interactive(True)

#yyyy = '198[5-6]'
#yyyy = '201[6-7]'
#yyyy = '201[7-8]'
#yyyy = '201[8-9]'
yyyy = '202[4-5]'
#yyyy = '2024'
#year = '????'
mm   = '??'
dd   = '??'
hh   = '12'

#obsid_dict = {5525:'SST', 5522:'SSS', 5351:'ADT', 3073: 'Tprof', 5521: 'Sprof', 6000:'Ice Fraction'} 
obsid_dict = {'SST':5525, 'SSS':5522, 'ADT': 5351,'Tprof': 3073, 'Sprof':5521, 'Ice Fraction': 6000} 
#obstype     = obsid_dict['SST']
obstype     = obsid_dict[obsname]
print(obstype)
plot_type   = 'time_series'
#region      = 'glb'
#lev1        = 0
#lev2        = 0
COLORS      = ['b','r','g','k','m']
#window_len  = 40 # make bigger for longer runs
window_len  = 1
#expname     = ['S2S-3','S2S-2']
#expname     = ['S2S-2','eh013']
expname     = ['S2S-3 GEOS-IT']
#expname     = ['S2S-2 FP']
#expname     = ['eh016','odas_V3_2']
description = ['PROFILES ONLY'] # dashed is re-forecast
if ((obsname == 'Tprof') | (obsname == 'Sprof')):
	title       = obsname+' '+region+' '+str(lev1)+'-'+str(lev2)+'m (dashed line is re-forecast)'
else:
	title       = obsname+' '+region+' (dashed line is re-forecast)'
print(title)
#sys.exit()

#PATH=[ '/gpfsm/dnb42/projects/p17/rkovach/geos5/exp/rk019/ocean_das/oana-*',
#       '/gpfsm/dnb42/projects/p17/rkovach/geos5/exp/rk019/ocean_das_sst/oana-*',
#       '/gpfsm/dnb42/projects/p17/rkovach/geos5/exp/rk020/ocean_das/oana-*',
#       '/gpfsm/dnb42/projects/p17/rkovach/geos5/exp/rk015/ocean_das/oana-*']

#PATH=['/gpfsm/dnb42/projects/p17/rkovach/geos5/exp/rk022/ocean_das/oana-*',
#'/gpfsm/dnb42/projects/p17/rkovach/geos5/exp/rk023/ocean_das/oana-*',
#'/gpfsm/dnb42/projects/p17/rkovach/geos5/exp/rk024/ocean_das/oana-*',
#'/gpfsm/dnb42/projects/p17/rkovach/geos5/exp/rk025/ocean_das/oana-*',]

#expname     = ['rk022','rk023','rk024','rk025']
#PATH=['/gpfsm/dnb42/projects/p17/ehackert/geos5/exp/eh017/ocean_das/oana-*','/gpfsm/dnb42/projects/p17/production/geos5/exp/S2S-2_1_ANA_001/ocean_das/oana-*']
#PATH=['/gpfsm/dnb42/projects/p17/production/geos5/exp/S2S-2_1_ANA_001/ocean_das/oana-*','/gpfsm/dnb42/projects/p17/ehackert/geos5/exp/eh013/ocean_das/oana-*']

#PATH=['/gpfsm/dnb42/projects/p17/production/geos5/exp/S2S-2_1_ANA_001/ocean_das/oana-*','/gpfsm/dnb42/projects/p17/ehackert/geos5/exp/eh026/ocean_das/oana-*']
#expname     = ['S2S-2','eh026']

#PATH=['/gpfsm/dnb42/projects/p17/production/geos5/exp/S2S-2_1_ANA_001/ocean_das/oana-*','/gpfsm/dnb42/projects/p17/production/geos5/exp/S2S-2_1_ANA_002/ocean_das/oana-*']
#PATH=['/gpfsm/dnb42/projects/p17/production/geos5/exp/S2S-2_1_ANA_002/ocean_das/oana-*']
#PATH=['/gpfsm/dnb07/projects/p236/GiOcean-NRT/ocean_das/oana-*']
PATH=['/gpfsm/dnb07/projects/p236/GiOcean-NRT/ocean_das/oana-*']


#expname     = ['S2S-2 FPIT','S2S-2 FP']
expname     = ['S2S-3 GEOS-IT']

#PATH=['/gpfsm/dnb42/projects/p17/ehackert/geos5/exp/eh016/ocean_das/oana-*','/gpfsm/dnb42/projects/p17/ychang/odas_V3_2/ocean_das/oana-*']
#PATH=['/gpfsm/dnb42/projects/p17/production/geos5/exp/S2S-2_1_ANA_001/ocean_das/oana-*']
#PATH=['/gpfsm/dnb42/projects/p17/ychang/odas_V3/ocean_das/oana-*']
#expname     = ['S2S-3','S2S-2']
#expname     = ['S2S-2','eh013']
#expname     = ['eh026']

#expname     = ['eh016','odas_V3_2']
#expname     = ['S2S-2']
#expname     = ['ODAS_V3']


cnt_leg = range(len(PATH))
cnt_exp = 0
file1 = open("LastDate.txt","w")
file2 = open("ODAS_DATE.txt","w")
for path in PATH:
    
    path2files = path+'/ocean_observer_*/obs-'+yyyy+mm+dd+'_'+hh+'.nc'
    
    flist = glob.glob(path2files)
    flist.sort()

#    print(flist)
#    print (path2files)
    #print path+'/ocean_observer_*/obs-'+yyyy+mm+dd+'_'+hh+'.nc'
    #raw_input('<>?')

    ymdh=[]
    mae_omf=[]
    mae_oma=[]
    bias_omf=[]
    bias_oma=[]
    nobs=[] 
       
    for fname in flist:
 #       print(fname)       
        exp_stats = OdaStats(fname=fname)
        if (plot_type=='time_series'):
            d, omf_tmp, oma_tmp, bomf, boma, n = exp_stats.mae(obstype=obstype,region=region)
#           if np.isfinite(omf_tmp) and abs(omf_tmp) < 1000. and abs(oma_tmp) < 1000. :
            if np.isfinite(omf_tmp) and np.isfinite(oma_tmp):
                ymdh.append(d)
                mae_omf.append(omf_tmp)
                mae_oma.append(oma_tmp)
                bias_omf.append(bomf)
                bias_oma.append(boma)
                nobs.append(n)
           # print ymdh[-1]
            
            sdate = str(ymdh[-1])
            sdate = sdate[0:10]
            
            sddmm = sdate[8:10]+'-'+sdate[5:7]
            #print sdate # %d-%b
            #print sddmm
            #sddMmm = datetime.datetime.strptime(sdate, "%Y-%m-%d ").strftime('%m-%d')
            #print exp_stats.date
    
    fig = plt.figure(num=1, figsize=(14, 8), facecolor='w')
    fig.suptitle(title, size=14, fontweight='bold')

    if plot_type == 'time_series':
       ax1 = fig.add_subplot(211)
       ax3 = fig.add_subplot(212)

    # === MAE Plot ===
       if obsname == 'Tprof':
          ax1.set_ylim(0.5, 1.0)
       elif obsname == 'Sprof':
          ax1.set_ylim(0.1, 0.3)
    # elif obsname == 'ADT':
    #     ax1.set_ylim(0.030, 0.055)

       ax1.plot(ymdh, smooth(mae_omf, window_len=window_len), '-', color=COLORS[cnt_exp], lw=2, alpha=0.5, label=expname[cnt_exp])
       ax1.plot(ymdh, smooth(mae_oma, window_len=window_len), '--', color=COLORS[cnt_exp], lw=2, alpha=0.5)
       ax1.grid(True)

    # === BIAS Plot ===
       if obsname == 'Tprof':
          ax3.set_ylim(-0.5, 0.5)
       elif obsname == 'Sprof':
          ax3.set_ylim(-0.1, 0.1)
       elif obsname == 'ADT':
          ax3.set_ylim(-0.03, 0.03)

       print('bias:', np.mean(bias_omf), ' std:', np.std(bias_omf))

       ax3.plot(ymdh, smooth(bias_omf, window_len=window_len), '-', color=COLORS[cnt_exp], lw=2, alpha=0.5)
       ax3.plot(ymdh, smooth(bias_oma, window_len=window_len), '--', color=COLORS[cnt_exp], lw=2, alpha=0.5)
       ax3.grid(True)

    # Format x-axis
       for ax in [ax1, ax3]:
           ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d'))
           ax.xaxis.set_major_locator(mdates.AutoDateLocator())
           ax.xaxis_date()  # Ensure datetime-based axis

    # Final touches
       ax1.set_title('MAE (Mean Absolute Error of OMF)', size=12, fontweight='bold')
       ax3.set_title('BIAS (Mean of the OMF)', size=12, fontweight='bold')
       ax1.legend(bbox_to_anchor=(0.90, 1.32))
       fig.autofmt_xdate()

    # Add label
#       plt.text(0.72, 0.9, 'Last Date ' + sdate, transform=ax1.transAxes, size=18, color='blue')

    else:
     # No unused subplot creation — prevents 1970 axis
       pass

    cnt_exp += 1
plt.text(0.72, 0.9, 'Last Date ' + sdate, transform=ax1.transAxes, size=18, color='blue')
# Save figure
plt.savefig('stats_' + obsname + '.png')

# Debug xlim check (optional)
for ax in fig.axes:
    print(f"{ax.get_title()} xlim: {ax.get_xlim()}")

# Save dates
file1.write(sdate)
file1.close()

file2.write(sddmm)
file2.close()

print('Increment Date and Final Restart: ' + sdate)
'''
lon(nobs) ;
float lat(nobs) ;
float lev(nobs) ;
float obs(nobs) ;
float sigo(nobs) ;
float omf(nobs) ;
float std_omf(nobs) ;
float oma(nobs) ;
float std_oma(nobs) ;
'''
