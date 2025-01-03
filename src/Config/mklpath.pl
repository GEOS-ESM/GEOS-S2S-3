#!/usr/bin/perl -w
#=======================================================================
# name - mklpath
# purpose - print value of MKLPATH to standard output
#
# Notes
# 1. Attempt to get value from MKLPATH environment variable
# 2. If MKLPATH does not exist, attempt to determine value from the
#    FPATH environment variable
# 3. Nothing is printed if MKLPATH cannot be determined
# 4. MKL = Math Kernal Library (Intel Fortran compiler)
#
# !Revision History
# ----------------
# 22Jun2009   Stassi    Initial version of code.
#=======================================================================
use strict;

# main program
#-------------
{
    my ($fpath, @lib, $mklpath, $mklroot,@tmp);

    # first check to see if MKLPATH environment variable is defined
    #--------------------------------------------------------------
    $mklpath = $ENV{"MKLPATH"};
    if ($mklpath) {
        print "$mklpath";
        exit;
    }

    # Use linktool
    # ------------
    $mklroot = $ENV{"MKLROOT"};
    if ($mklroot) {
        @tmp = split /-L/, `$mklroot/bin/intel64/mkl_link_tool -libs 2>/dev/null`;
        @tmp = split / /, $tmp[1];
        print "$tmp[0]";
        $mklpath = $tmp[0];
        print "$mklpath";
        exit;
    }

    # next, try to extract MKLPATH from FPATH environment variable
    #-------------------------------------------------------------
    exit unless $ENV{"FPATH"};
    $fpath = $ENV{"FPATH"};
    @lib = split /:/, $fpath;
    foreach (@lib) {
        ($mklpath = $_) =~ s/include/lib\/em64t/;
        last if -d $mklpath;
        $mklpath = ".";
    }
    print "$mklpath";
}
