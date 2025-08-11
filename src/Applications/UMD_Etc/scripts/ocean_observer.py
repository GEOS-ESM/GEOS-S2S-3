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
# Update: IN_LINE setup Sept 2024 Veronica Ruiz Xomchuk

import glob
import sys
import time
import os

def get_command(Ne, fname, PATH_TO_OBSERVER, ANADIR, GROUP, QOS, seq=1, inovation_type='omf', in_line='False', CONSTRAINT='[sky|cas]'):

    # ANADIR = oana-yyyymmdd_seq

    yyyy = fname[-18:-14]
    mm = fname[-14:-12]
    dd = fname[-12:-10]
    hh = fname[-9:-7]

    job_name = 'ocnobs_'+yyyy+mm+dd+'_'+hh
    if in_line=='False':
        print(f'IN_LINE is {in_line}')
        print('Generating job command')
        command='sbatch --time=1:00:00 -n '+str(Ne)+' --constraint='+CONSTRAINT+' --ntasks-per-node=27 --job-name='+job_name+\
                ' --qos='+QOS+\
                ' -o '+job_name+'.o '+\
                ' -e '+job_name+'.e '+\
                ' -A '+GROUP+' --partition=compute '+PATH_TO_OBSERVER+'/ocean_observer.csh '+\
                yyyy+' '+mm+' '+dd+' '+hh+' '+str(seq)+' '+inovation_type+' '+ANADIR
    elif in_line=='True':
        print(f'IN_LINE is {in_line}')
        print('Send observer in node')
        command= f'{PATH_TO_OBSERVER}/ocean_observer.csh {yyyy} {mm} {dd} {hh} {seq} {inovation_type} {ANADIR} > {job_name}.o 2> {job_name}.e'
    print ('in get_command:',command)
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
IN_LINE = os.environ['IN_LINE']
CONSTRAINT = os.environ['ODAS_CONSTR']

f1=open('./ocean_observer.out', 'w+')
nw=0
n = 0
old_list = []
if WAIT=='True':
    while True:
        nw+=1
        #new_list = glob.glob('*.'+geosgcm_ocn3dT.*.nc4')
        new_list = glob.glob('*.'+OCN3D+'.*.nc4')
        new_list.sort() #print('in ocean_observer.py new_list is',new_list, '\n')
        if (len(new_list)>len(old_list)):
            f1.write('Sending observer on '+new_list[-1]+'\n')
            fname = new_list[-1]
            command, yyyy, mm, dd, hh = get_command(Ne, fname, PATH_TO_OBSERVER, ANADIR, ODAS_group, ODAS_qos,
                                                    seq=DA_seq,inovation_type=inovation_type, in_line=IN_LINE, CONSTRAINT=CONSTRAINT)
            f1.write(yyyy+mm+dd+hh)
            f1.write('========================='+'\n')
            f1.write('BEFORE1 ocean_observer.py in ocean_observer.csh'+'\n')
            f1.write(command+'\n')
            os.system(command)   # submit observer job
            #  now check to see if BADOBS exists, if yes crash it
            if (os.path.exists(f'{SCRDIR}/BADOBS')):
              f1.write('BADOBS Observer crash in ocean_observer.py'+'\n')
              WAIT='False'
              command='scancel '+str(PIDMAIN)
              f1.write(command+'\n')
              os.system(command)
              sys.exit(1)
            f1.write('After exit ocean_observer.py in ocean_observer.csh'+'\n')
            n+=1
        else:
            time.sleep(1)
        old_list = new_list
        f1.flush()


else:
    flist = glob.glob('*.'+OCN2D+'.*.nc4') #
    flist.sort()
    for fname in flist:
        f1.write('Sending observer on '+fname+'\n')
        command, yyyy, mm, dd, hh = get_command(Ne, fname, PATH_TO_OBSERVER, ANADIR, ODAS_group, ODAS_qos,
                                                seq=DA_seq,inovation_type=inovation_type, in_line=IN_LINE)
        print ('command is',command)
        f1.write(yyyy+mm+dd+hh)
        f1.write('========================='+'\n')
        f1.write('BEFORE2 ocean_observer.py in ocean_observer.csh'+'\n')
        f1.write(command+'\n')
        print ('in ocean_observer.py command',command,'\n')
        os.system(command)   # submit observer job 
        #  now check to see if BADOBS exists, if yes crash it
        if (os.path.exists(f'{SCRDIR}/BADOBS')):
           f1.write('BADOBS Observer crash in ocean_observer.py'+'\n')
           WAIT='False'
           command='scancel '+str(PIDMAIN)
           f1.write(command+'\n')
           os.system(command)
           sys.exit(1)
        f1.write('After2 exit ocean_observer.py in ocean_observer.csh'+'\n')
        n+=1

f1.close()

