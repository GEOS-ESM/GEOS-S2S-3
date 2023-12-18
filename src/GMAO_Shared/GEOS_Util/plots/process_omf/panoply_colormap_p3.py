#!/usr/bin/env python

"""
  Registring own colormaps.
"""

#-------------
# Load modules
#-------------
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.colors as colors
import matplotlib.cm as cm
import numpy as np

def from_ascii(filename, name):
    maxval = 1.0
    palette = open(filename)
    lines = palette.readlines()
    carray = np.zeros([len(lines), 3])
    print(('ncolors ',len(lines)))
    for line in lines:
        line = line.strip()
        num, a, b, c = line.split()
        num = int(num.strip())
        carray[num, 0] = float(a)/maxval
        carray[num, 1] = float(b)/maxval
        carray[num, 2] = float(c)/maxval
    cmap1 = colors.ListedColormap(carray, name=name)
    return cmap1

#fileName = "GMT_panoply.txt"
fileName = "/gpfsm/dnb42/projects/p17/ehackert/geos5/exp/process_increment/GMT_panoply.txt"
name     = 'my_cmap'
my_cmap  = from_ascii(fileName, name)
cm.register_cmap(cmap=my_cmap)
