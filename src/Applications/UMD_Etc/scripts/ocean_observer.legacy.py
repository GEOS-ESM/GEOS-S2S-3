#! /usr/bin/env python
# /////////////////////////////////////////////////////////////////////////
# /**
# * Title: ocean_observer.py
# *
# * Description: Scheduler for ocean_observer.csh
# *              Monitors the scratch directory and submit an observer each time a 
# *              new ocn3dT file is created
# *              
# * @author: Guillaume Vernieres
# */
# 
# /////////////////////////////////////////////////////////////////////////
# Date: May 2015

import glob
import sys
import time
import os

def get_command(Ne, fname, PATH_TO_OBSERVER, ANADIR, GROUP, QOS, seq=1, inovation_type='omf'):

    # ANADIR = oana-yyyymmdd_seq

    yyyy = fname[-18:-14]
    mm = fname[-14:-12]
    dd = fname[-12:-10]
    hh = fname[-9:-7]

    job_name = 'ocnobs_'+yyyy+mm+dd+'_'+hh
    command='sbatch --time=1:00:00 -n '+str(Ne)+' --ntasks-per-node=27 --job-name='+job_name+\
                ' --qos=gmaofcst'+\
                ' -o '+job_name+'.o '+\
                ' -e '+job_name+'.e '+\
                ' -A '+GROUP+' --partition=compute '+PATH_TO_OBSERVER+'/ocean_observer.csh '+\
                yyyy+' '+mm+' '+dd+' '+hh+' '+str(seq)+' '+inovation_type+' '+ANADIR


    return command, yyyy, mm, dd, hh

inovation_type = sys.argv[1]  # 'omf' or 'oma'
ANADIR         = sys.argv[2]
WAIT           = sys.argv[3]  # True: Wait for history to be written
                              # False: Assumes history is present
DA_seq         = sys.argv[4]

Ne = int(os.environ['ODAS_Ne'])
PATH_TO_OBSERVER = os.environ['UMD_LETKFSCRIPTS']
ODAS_group = os.environ['ODAS_group']
ODAS_qos = os.environ['ODAS_qos']
OCN3D =  os.environ['OCN3D']
OCN2D =  os.environ['OCN2D']
OSCRDIR = os.environ['UMD_LETKFSCRDIR']
SCRDIR =  os.environ['SCRDIR']
PIDMAIN = os.environ['SLURM_JOB_ID'] # Sent here to kill model if need be

f1=open('./ocean_observer.out', 'w+')
nw=0
n = 0
old_list = []
if WAIT=='True':
    while True:
        nw+=1
        #new_list = glob.glob('*.'+geosgcm_ocn3dT.*.nc4')
        new_list = glob.glob('*.'+OCN3D+'.*.nc4')
        new_list.sort()
        if (len(new_list)>len(old_list)):
            f1.write('Sending observer on '+new_list[-1]+'\n')
            fname = new_list[-1]
            command, yyyy, mm, dd, hh = get_command(Ne, fname, PATH_TO_OBSERVER, ANADIR, ODAS_group, ODAS_qos, seq=DA_seq,inovation_type=inovation_type)
            f1.write(yyyy+mm+dd+hh)
            f1.write('========================='+'\n')
            f1.write('BEFORE1 ocean_observer.py in ocean_observer.csh'+'\n')
            f1.write(command+'\n')
            os.system(command)   # submit the sbatch observer job
#  now check to see if BADOBS exists, if yes crash it
            command=SCRDIR+'/BADOBS'
            file_exists = os.path.exists(str(command))
            sfile_exists=str(file_exists)
            f1.write ('value of file exists '+sfile_exists+'\n')
            if (file_exists):
              f1.write('BADOBS Observer crash in ocean_observer.py'+'\n')
              WAIT='False'
              command='scancel '+str(PIDMAIN)
              f1.write(command+'\n')
              os.system(command)
              sys.exit(1)
            f1.write('After exit ocean_observer.py in ocean_observer.csh'+'\n')
            n+=1
        else:
#           f1.write('Waiting for new history ... '+str(nw)+'\r')
            time.sleep(1)
        old_list = new_list
        f1.flush()


else:
    flist = glob.glob('*.'+OCN2D+'.*.nc4') #('*.geosgcm_ocn2dT.*.nc4')
    print(flist)
    for fname in flist:
        f1.write('Sending observer on '+fname+'\n')
        command, yyyy, mm, dd, hh = get_command(Ne, fname, PATH_TO_OBSERVER, ANADIR, seq=DA_seq,inovation_type=inovation_type)
        f1.write(yyyy+mm+dd+hh)
        f1.write('========================='+'\n')
        f1.write('BEFORE2 ocean_observer.py in ocean_observer.csh'+'\n')
        f1.write(command+'\n')
        os.system(command)   # submit sbatch observer job 
#  now check to see if BADOBS exists, if yes crash it
        command=SCRDIR+'/BADOBS'
        file_exists = os.path.exists(str(command))
        sfile_exists=str(file_exists)
        f1.write ('value of file exists '+sfile_exists+'\n')
        if (file_exists):
           f1.write('BADOBS Observer crash in ocean_observer.py'+'\n')
           WAIT='False'
           command='scancel '+str(PIDMAIN)
           f1.write(command+'\n')
           os.system(command)
           sys.exit(1)
        f1.write('After2 exit ocean_observer.py in ocean_observer.csh'+'\n')
        n+=1

f1.close()
