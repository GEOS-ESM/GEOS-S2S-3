#! /usr/bin/env python
import os
import sys

#
# Computes the mean offset between altimeter and mom from observer's output
#

BASEDIR      = sys.argv[1]   # Path to ncl tools
SCRATCHDIR   = sys.argv[2]   # Path to ncl tools
BARO_RESTART = sys.argv[3]   # Location of barotropic restart

command = 'grep -h OFFSET '+SCRATCHDIR+'/ocnobs_????????_??.o > slv_offset.txt'
print(command)
os.system(command)

f = open('slv_offset.txt', 'r')
offset=0
cnt=0.0
for line in f:
    offset=offset+float(line[9:])
    cnt+=1.0

offset=offset/cnt
print('offset=',offset)

print('Adding offset to barotropic restart')
# Add offset to eta_t
command = 'ncap2 -O -s "eta_t=(eta_t+'+str(offset)+')" '+BARO_RESTART+' '+BARO_RESTART
print(command)
print('PAST NCAP2 in get_slv_offset.py')
os.system(command)

# Add offset to eta_u
command = 'ncap2 -O -s "eta_u=(eta_u+'+str(offset)+')" '+BARO_RESTART+' '+BARO_RESTART
print(command)
print('PAST 2nd NCAP2 in get_slv_offset.py')
os.system(command)
