#! /usr/bin/env python

from netCDF4 import Dataset
import datetime
import sys

#   Description :   add file2 to file1

#   Args: 
#		filename1 - center3d.nc
#		filename2 - incrMMDD.nc
#		month - month for mean increment
#		day - day for mean increment

#   get the closest day for the mean bias from Andrea's list

def bestdate(mon1,day1):
#   here is the list of mean biases that Andrea provided, assumes year=1
    test_date_list = [datetime.datetime(1,1,1), datetime.datetime(1,1,6), datetime.datetime(1,1,11),
    datetime.datetime(1,1,16), datetime.datetime(1,1,21), datetime.datetime(1,1,26), datetime.datetime(1,1,31),
    datetime.datetime(1,2,5), datetime.datetime(1,2,10), datetime.datetime(1,2,15),
    datetime.datetime(1,2,20), datetime.datetime(1,2,25), datetime.datetime(1,3,2), datetime.datetime(1,3,7),
    datetime.datetime(1,3,12), datetime.datetime(1,3,17), datetime.datetime(1,3,22), datetime.datetime(1,3,27),
    datetime.datetime(1,4,1), datetime.datetime(1,4,6), datetime.datetime(1,4,11), datetime.datetime(1,4,16),
    datetime.datetime(1,4,21), datetime.datetime(1,4,26), datetime.datetime(1,5,1), datetime.datetime(1,5,6),
    datetime.datetime(1,5,11), datetime.datetime(1,5,16), datetime.datetime(1,5,21), datetime.datetime(1,5,26),
    datetime.datetime(1,5,31), datetime.datetime(1,6,5), datetime.datetime(1,6,10), datetime.datetime(1,6,15),
    datetime.datetime(1,6,20), datetime.datetime(1,6,25), datetime.datetime(1,6,30), datetime.datetime(1,7,5),
    datetime.datetime(1,7,10), datetime.datetime(1,7,15), datetime.datetime(1,7,20), datetime.datetime(1,7,25),
    datetime.datetime(1,7,30), datetime.datetime(1,8,4), datetime.datetime(1,8,9), datetime.datetime(1,8,14),
    datetime.datetime(1,8,19), datetime.datetime(1,8,24), datetime.datetime(1,8,29), datetime.datetime(1,9,3),
    datetime.datetime(1,9,8), datetime.datetime(1,9,13), datetime.datetime(1,9,18), datetime.datetime(1,9,23),
    datetime.datetime(1,9,28), datetime.datetime(1,10,3), datetime.datetime(1,10,8), datetime.datetime(1,10,13),
    datetime.datetime(1,10,18), datetime.datetime(1,10,23), datetime.datetime(1,10,28), datetime.datetime(1,11,2),
    datetime.datetime(1,11,7), datetime.datetime(1,11,12), datetime.datetime(1,11,17), datetime.datetime(1,11,22),
    datetime.datetime(1,11,27), datetime.datetime(1,12,2), datetime.datetime(1,12,7), datetime.datetime(1,12,12),
    datetime.datetime(1,12,17), datetime.datetime(1,12,22), datetime.datetime(1,12,27)]
    
# date in yyyy/mm/dd format  assume year=1
    test_date = datetime.datetime(int(1), int(mon1), int(day1))
#######
# get all differences with date as values
    res = min(test_date_list, key=lambda sub: abs(sub - test_date))
# printing result
#    print("Nearest date from list : " + str(res))
#    print(str(res))
    str2=str(res)
    monfound=str2[5:7]
    dayfound=str2[8:10]
#    print(monfound, dayfound)
    return str2

fname3d=sys.argv[1]
month0=sys.argv[2]
day0=sys.argv[3]

str2=bestdate(month0,day0)
#print(str2)
monfound=str2[5:7]
dayfound=str2[8:10]
#print(monfound, dayfound)

month=monfound.zfill(2)
day=dayfound.zfill(2)

print('using mean bias from ',month, day)

incrfn='/gpfsm/dnb32/amolod/oceanbias/incr'+month+day+'.nc'
print(incrfn)

file_input = Dataset(fname3d,'r+')
file_input2 = Dataset(incrfn,'r')

#  do add file_input2 to file_input for temperature
incr_temp = file_input2.variables['temp'][:,:,:,:]
temp = file_input.variables['temp'][:,:,:,:]

print('in do_bias_correction_on_incr.py')

#print(temp.shape)
#print(incr_temp.shape)

temp2 = temp[:,:,:,:]+incr_temp[:,:,:,:]
file_input['temp'][:,:,:,:] = temp2[:,:,:,:]

#print(temp.shape)
#print(temp2)

#   do salinity

incr_salt = file_input2.variables['salt'][:,:,:,:]
salt = file_input.variables['salt'][:,:,:,:]

#print(salt.shape)
#print(incr_salt.shape)

salt2 = salt[:,:,:,:]+incr_salt[:,:,:,:]
file_input['salt'][:,:,:,:] = salt2[:,:,:,:]

#print(salt.shape)
#print(salt2)

#   do SLV

incr_SLV = file_input2.variables['SLV'][:,:,:]
SLV = file_input.variables['SLV'][:,:,:]

#print(SLV.shape)
#print(incr_SLV.shape)

SLV2 = SLV[:,:,:]+incr_SLV[:,:,:]
file_input['SLV'][:,:,:] = SLV2[:,:,:]

#print(SLV.shape)
#print(SLV2)

file_input.close()
file_input2.close()
