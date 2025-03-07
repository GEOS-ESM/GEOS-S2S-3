#! /usr/bin/env python
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import sys 
import glob
import numpy as np
from netCDF4 import Dataset
from datetime import date
import datetime

obsname = sys.argv[1]
region  = sys.argv[2]
lev1    = sys.argv[3]
lev2    = sys.argv[4]
outname = sys.argv[5]   # eg. 'Tprof_1981.png'

lev1 = float(lev1)
lev2 = float(lev2)
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
        raise ValueError("Window is one of 'flat', 'hanning', 'hamming', 'bartlett', 'blackman'")

    if window in ['flat', 'hanning', 'hamming', 'bartlett', 'blackman']:
        s=np.r_[2*x[0]-x[window_len:1:-1],x,2*x[-1]-x[-1:-window_len:-1]]
        if window == 'flat': #moving average                                                                                                                                                         
            w=np.ones(window_len,'d')
        else:
            w=eval('np.'+window+'(window_len)')

        tmpy=np.convolve(w/w.sum(),s,mode='same')
        y=tmpy[window_len-1:-window_len+1]

        print ()

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
            self.OBSIDa=ncfile.variables['OBSIDa'][:]
            self.lona=ncfile.variables['lona'][:]
            self.lata=ncfile.variables['lata'][:]
            self.leva=ncfile.variables['leva'][:]
            self.obsa=ncfile.variables['obsa'][:]
            self.sigoa=ncfile.variables['sigoa'][:]
            self.oma=ncfile.variables['oma'][:]  
            self.fcsta=self.obsa-self.oma
        except:
            self.OBSIDa=np.nan*self.OBSID
            self.lona=np.nan*self.lon
            self.lata=np.nan*self.lat
            self.leva=np.nan*self.lev
            self.obsa=np.nan*self.obsa
            self.sigoa=np.nan*self.sigoa
            self.oma=np.nan*self.omf
            self.fcsta=np.nan*self.fcst
        self.std_omf=ncfile.variables['std_omf'][:]  
        print(fname,fname[-30:-26],fname[-26:-24],fname[-24:-22])
        self.yyyymmdd=fname[-30:-26]+'-'+fname[-26:-24]+'-'+fname[-24:-22] #fname[-27:-23]+'-'+fname[-23:-21]+'-'+fname[-21:-19]
        self.time=int(fname[-30:-22])
        self.date=datetime.datetime(int(fname[-30:-26]), int(fname[-26:-24]), int(fname[-24:-22]), int(fname[-21:-19]))
        ncfile.close()
    def mae(self, obstype=3073, region='glb'):
        self.IDS=np.unique(self.OBSID)
        ids = obstype
        print('obstype is ',obstype)

#       J=np.where (np.abs(self.oma)>100.)
#       J=np.where (np.abs(self.oma)<1.e-11)
#       print 'BAD OMA', self.OBSID[J], self.oma[J],self.lat[J],self.lon[J],self.date
 

#       K=np.where (np.abs(self.omf)>100.)
#       K=np.where (np.abs(self.omf)<1.e-11)
#       print 'BAD OMF', self.OBSID[K], self.omf[K],self.lat[K],self.lon[K],self.date

        if region=='natl':
            if obsname=='Tprof' :
#             toobig=10.0
              toobig=5.0
            if obsname=='Sprof':
              toobig=5.0
            if obsname=='ADT':
              toobig=1.0
            if obsname=='SST':
              toobig=5.0
            I = np.where( (self.OBSID==ids) & (np.abs(self.lev)<lev2) & (self.fcst!=0.0) & (self.lat > 30.0) & (self.lon > -90.) & (self.lon < 0.) & (np.abs(self.omf)<toobig) )
            II = np.where( (self.OBSIDa==ids) & (np.abs(self.leva)<lev2) & (self.fcsta!=0.0) & (self.lata > 30.0) & (self.lona > -90.) & (self.lona < 0.) & (np.abs(self.oma)<toobig) )
        if region=='glb':
            if obsname=='Tprof' :
              toobig=5.0
            if obsname=='SST' :
              toobig=5.0
            if obsname=='Sprof':
              toobig=5.0
            if obsname=='ADT':
              toobig=1.0
            I = np.where( (self.OBSID==ids) & (np.abs(self.lev)<lev2) & (self.fcst!=0.0) & (np.abs(self.lat)<90.0) & (np.abs(self.omf)<toobig) )
            II = np.where( (self.OBSIDa==ids) & (np.abs(self.leva)<lev2) & (self.fcsta!=0.0) & (np.abs(self.lata)<90.0) & (np.abs(self.oma)<toobig))
        if region=='trp':
            if obsname=='SST' :
              toobig=10.0
            if obsname=='Tprof' :
              toobig=10.0
            if obsname=='Sprof':
              toobig=5.0
            if obsname=='ADT':
              toobig=1.0
            I = np.where( (self.OBSID==ids) & (np.abs(self.lev)<lev2) & (self.fcst!=0.0) & (np.abs(self.lat)<30.0) & (self.omf<toobig))
            II = np.where( (self.OBSIDa==ids) & (np.abs(self.leva)<lev2) & (self.fcsta!=0.0) & (np.abs(self.lata)<30.0) & (np.abs(self.oma)<toobig))
        if region=='north':        
            if obsname=='Tprof' :
              toobig=10.0
            if obsname=='SST' :
              toobig=5.0
            if obsname=='Sprof':
              toobig=5.0
            if obsname=='ADT':
              toobig=1.0
            I = np.where( (self.OBSID==ids) & (np.abs(self.lev)<lev2) & (self.fcst!=0.0) & (self.lat>30.0) & (np.abs(self.omf)<10.0) )
            II = np.where( (self.OBSIDa==ids) & (np.abs(self.leva)<lev2) & (self.fcsta!=0.0) & (self.lata>30.0) & (np.abs(self.oma)<10.0) )
        if region=='south':        
            if obsname=='Tprof' :
              toobig=10.0
            if obsname=='SST' :
              toobig=5.0
            if obsname=='Sprof':
              toobig=5.0
            if obsname=='ADT':
              toobig=1.0
            I = np.where( (self.OBSID==ids) & (np.abs(self.lev)<lev2) & (self.fcst!=0.0) & (self.lat<-30.0) & (np.abs(self.omf)<10.0) )
            II = np.where( (self.OBSIDa==ids) & (np.abs(self.leva)<lev2) & (self.fcsta!=0.0) & (self.lata<-30.0) & (np.abs(self.oma)<10.0) )

        print('number of obs is',len(self.omf[I]),len(self.oma[II]))
        self.mae_omf = np.mean(np.abs(self.omf[I]))
        self.mae_oma = np.mean(np.abs(self.oma[II]))
        self.bias_omf = np.mean(self.omf[I])
        self.bias_oma = np.mean(self.oma[II])
        self.nobs =np.shape(I)[1]

        return self.date, self.mae_omf, self.mae_oma, self.bias_omf, self.bias_oma, self.nobs

#plt.interactive(True)

#yyyy = '198[5-6]'
#yyyy = '201[6-7]'
#yyyy = '200[1-6]'
#yyyy = '199[0-3]'
yyyy = '1982'
#yyyy = '2012'
#year = '????'
mm   = '??'
dd   = '??'
hh   = '12'
#hh   = '06'

#obsid_dict = {5525:'SST', 5522:'SSS', 5351:'ADT', 3073: 'Tprof', 5521: 'Sprof', 6000:'Ice Fraction'} 
obsid_dict = {'SST':5525, 'SSS':5522, 'ADT': 5351,'Tprof': 3073, 'Sprof':5521, 'Ice Fraction': 6000} 
#obstype     = obsid_dict['SST']
obstype     = obsid_dict[obsname]
print (obstype)
plot_type   = 'time_series'
#region      = 'glb'
#lev1        = 0
#lev2        = 0
COLORS      = ['r','b','g','k','m']
#window_len  = 40 # make bigger for longer runs
window_len  = 1
#expname     = ['S2S-3','S2S-2']
#expname     = ['TEST']
#expname     = ['eh016','odas_V3_2']
description = ['PROFILES ONLY'] # dashed is re-forecast
if ((obsname == 'Tprof') | (obsname == 'Sprof')):
	title       = obsname+' '+region+' '+str(lev1)+'-'+str(lev2)+'m (dashed line is re-forecast)'
else:
	title       = obsname+' '+region+' (dashed line is re-forecast)'
print (title)
#sys.exit()

#PATH=['/gpfsm/dnb42/projects/p17/ehackert/geos5/exp/eh019/ocean_das/oana-*','/gpfsm/dnb02/projects/p23/bzhao/sio20/ocean_das/oana-*']
#PATH=['/gpfsm/dnb42/projects/p17/ehackert/geos5/exp/process_omf/tmp/oana-*']

PATH=['/gpfsm/dnb42/projects/p17/ehackert/geos5/exp/process_omf/tmp/oana-*']
expname  = ['A_test']

#PATH=['/discover/nobackup/projects/gmao/merra2-ocean/GiOCEAN_e1/ocean_das/oana-*']
#expname  = ['GiOCEAN']

cnt_leg = range(len(PATH))
cnt_exp = 0
for path in PATH:

    path2files = path+'/ocean_observer_*/obs-'+yyyy+mm+dd+'_'+hh+'.nc'
    flist = glob.glob(path2files)
    flist.sort()

    print (flist)
    #print path2files
    #print path+'/ocean_observer_*/obs-'+yyyy+mm+dd+'_'+hh+'.nc'
    #raw_input('<>?')

    ymdh=[]
    mae_omf=[]
    mae_oma=[]
    bias_omf=[]
    bias_oma=[]
    nobs=[]

    for fname in flist:
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
            #print exp_stats.date

    fig = plt.figure(num=1, figsize=(14,8), facecolor='w')
    fig.suptitle(title,size=14,fontweight='bold')
    if (plot_type=='time_series'):

        ax1 = fig.add_subplot(211)
        #ax2 = ax1.twinx() # Obs Count
        ax3 = fig.add_subplot(212) # obscount stuff
        #ax4=ax3.twinx()
        

        #MAE
#       if (obsname=='Tprof'):
#         ax1.set_ylim(0.5,1.5)
        if (obsname=='Sprof'):
#         ax1.set_ylim(0.1,0.2)
          if (region=='trp'):
             ax1.set_ylim(0.0,0.02)
          if (region=='glb'):
             ax1.set_ylim(0.1,0.5)
        if (obsname=='ADT'):
#         ax1.set_ylim(0.030,0.055)
#tropics
          if (region=='trp'):
#	ax1.set_ylim(0.020,0.06)
                ax1.set_ylim(0.01,0.02)
#global
          if (region=='glb'):
#	        ax1.set_ylim(0.060,0.085)
                ax1.set_ylim(0.040,0.085)
        ax1.plot_date(ymdh[:], smooth(mae_omf[:],window_len=window_len),'-',color=COLORS[cnt_exp],lw=2,alpha=0.5,label=expname[cnt_exp])
        ax1.plot_date(ymdh[:], smooth(mae_oma[:],window_len=window_len),'--',color=COLORS[cnt_exp],lw=2,alpha=0.5)
        #ax2.plot_date(ymdh, nobs,'-',color=COLORS[cnt_exp],lw=1,alpha=0.1) # Obs Count
        ax1.grid(True)

        plt.draw()

        #BIAS
#       if (obsname=='Tprof'):
#         ax3.set_ylim(-0.5,0.5)
        if (obsname=='Sprof'):
          ax3.set_ylim(-0.02,0.02)
#         ax3.set_ylim(-0.3,0.3)
        if (obsname=='ADT'):
          ax3.set_ylim(-0.03,0.03)
        print ('bias:',np.mean(bias_omf),' std:',np.std(bias_omf) )
        ax3.plot_date(ymdh, smooth(bias_omf,window_len=window_len),'-',color=COLORS[cnt_exp],lw=2,alpha=0.5)
        ax3.plot_date(ymdh, smooth(bias_oma,window_len=window_len),'--',color=COLORS[cnt_exp],lw=2,alpha=0.5)
        ax3.grid(True)
            #ax4.plot_date(ymdh, nobs,'-',color=COLORS[cnt_exp],lw=1,alpha=0.01)
        plt.draw()
            #raw_input('<>')

    else:
        plt.subplot(2,2,cnt_exp+1)
        plt.plot(exp_stats.obs,exp_stats.obs-exp_stats.omf,'.',color=COLORS[cnt_exp],alpha=0.1,markeredgecolor = 'none')
        plt.grid(True)
    #ax1.set_title('MAE')

    cnt_exp+=1

ax1.set_title('MAE (Mean Absolute Error of OMF)',size=12,fontweight='bold')
ax3.set_title('BIAS (Mean of the OMF)',size=12,fontweight='bold')
ax1.legend(bbox_to_anchor=(0.90,1.32))
fig.autofmt_xdate()

plt.savefig(outname)
#plt.savefig('stats.eps',format='eps',dpi=1000)
#plt.show()

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
