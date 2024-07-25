#!/bin/tcsh -f
#------------------------------------------------------------------------
# name: parallel_build.csh
# purpose: This subroutine builds using pinstall
#          for compilation parallelization.
#
# !REVISION HISTORY
# 15Mar2007  Stassi  Modified version of g5das_parallel_build.csh
# 22Mar2007  Kokron  Add support for discover
# 28Mar2007  Stassi  Attempt to find default group for job submittal
# 13Apr2007  Stassi  Change order in which LOG/info names get iterated
# 04May2007  Stassi  Hard-code NCPUS=4 for discover
# 10Aug2007  TO/JS   Added distclean option; reordered script
# 15Apr2008  Stassi  Added code to cd to src directory, if not already
#                    there. Rename ESMASRC variable to ESMADIR
# 16Apr2008  Stassi  Use only half of CPUs on palm; mpi needs more memory
# 30Mar2008  Stassi  Add runtime options: -h, -hdf, -q queue
# 13Apr2009  Stassi  Updated for pleiades
# 05Aug2009  Stassi  Added -debug and -tmpdir flags
# 19Aug2010  Stassi  Added -walltime flag
# 01Apr2011  Stassi  Added clean and realclean options
# 04Nov2014  MAT     Moved to sbatch on discover, added Haswell 
#                    as an option, removed some older batch systems
# 07Jul2016  MAT     Added Broadwell at NAS. Removed Westmere. Made
#                    Broadwell default at NAS.
# 10Oct2017  MAT     Added Skylake at NAS. Added option to pass in 
#                    account
# 18Feb2022  MAT     Added Rome nodes to NAS
#------------------------------------------------------------------------
set name = $0
set scriptname = $name
set BUILD_LOG_DIR = BUILD_LOG_DIR

setenv ARCH `uname -s`
#--set time = (1000 "%Uu %Ss %E  %X+%Dk %Mk %I+%Oio %Fpf+%Ww")
set time = ( 1000 "%Uu %Ss %E" )
set NCPUs_min = 6

#=====================
# determine the site 
#=====================
set node = `uname -n`
if ( ($node == dirac)   \
      || ($node =~ borg*)   \
      || ($node =~ warp*)   \
      || ($node =~ discover*)) then
   setenv SITE NCCS

else if (($node =~ pfe*)      \
      || ($node =~ tfe*)      \
      || ($node =~ r[0-9]*i[0-9]*n[0-9]*) \
      || ($node =~ r[0-9]*c[0-9]*t[0-9]*n[0-9]*)) then
   setenv SITE NAS

else
   setenv SITE UNKNOWN
   set NCPUs_min = 1
endif

# if batch, then skip over job submission
#----------------------------------------
if ($?Parallel_build_bypass_flag) then
   goto build
endif

# change to src directory, if not already there
#----------------------------------------------
if ($name != $name:t) then
   set scriptname = $name:t
   cd $name:h
endif
set srcdir = `pwd`

# set defaults
#-------------
setenv ddb    0
setenv debug  ""
setenv fast  ""
setenv interactive 0
setenv optflg ""
setenv proc   ""
setenv prompt 1
setenv queue  ""
setenv partition   ""
setenv account  ""
setenv sdflag "ESMA_SDF=netcdf"
setenv tmpdir ""
setenv walltime ""

# Detect if on compute node already
# ---------------------------------
setenv oncompnode  0
if ($?PBS_JOBID || $?SLURM_JOBID) then
   set oncompnode = 1
endif

# check for runtime parameters
#-----------------------------
@ n = 0
while ($#argv)

   # usage information
   #------------------
   if ("$1" == "-help") goto usage
   if ("$1" == "-h"   ) goto usage

   # compile for HDF
   #----------------
   if ("$1" == "-hdf") setenv sdflag "ESMA_SDF=hdf"

   # developer's debug
   #------------------
   if ("$1" == "-ddb") set ddb = 1

   # compile with debug
   #-------------------
   if (("$1" == "-debug") || ("$1" == "-db")) then
      if ("$optflg" == "") then
         setenv debug "BOPT=g"
         setenv optflg ""
      endif
   endif

   # compile with fast
   #------------------
   if (("$1" == "-fast")) then
      if ("$optflg" == "") then
         setenv debug "BOPT=fast"
         setenv optflg ""
      endif
   endif

   # specify node type
   #------------------
   if ("$1" == "-rom")  set nodeTYPE = "Rome"
   if ("$1" == "-mil")  set nodeTYPE = "Milan"
   if ("$1" == "-cas")  set nodeTYPE = "CascadeLake"
   if ("$1" == "-sky")  set nodeTYPE = "Skylake"
   if ("$1" == "-bro")  set nodeTYPE = "Broadwell"
   if ("$1" == "-has")  set nodeTYPE = "Haswell"

   # set optimization flags
   #-----------------------
   if (("$1" == "-opt") && ($debug == "")) then
      shift; if (! $#argv) goto usage
      set optval = "$1"
      if (($optval != 1) && ($optval != 2) \
        && ($optval != 3) && ($optval != 4) && ($optval != 5)) goto usage
      setenv optflg "FOPT=-O$optval COPT=-O$optval"
   endif

   # reset Fortran TMPDIR
   #---------------------
   if ("$1" == "-tmpdir") then
      shift; if (! $#argv) goto usage
      setenv tmpdir $1
   endif

   # specify Fortran compiler
   #-------------------------
   if ("$1" =~ *=*) then
      set var = `echo $1 | cut -d= -f1`
      set val = `echo $1 | cut -d= -f2`
      if ($var == "ESMA_FC") setenv ESMA_FC $val
   endif

   # run job interactively
   #----------------------
   if ("$1" == "-i") set interactive = 1

   # submit batch job to alternative queue/qos
   #------------------------------------------
   if ("$1" == "-q") then
      shift; if (! $#argv) goto usage
      if ($SITE == NCCS) then
         setenv queue "--qos=$1"
      else
         setenv queue "-q $1"
      endif
   endif

   # submit batch job to specified partition
   #----------------------------------------
   if ("$1" == "-partition") then
      shift; if (! $#argv) goto usage
      setenv partition "--partition=$1"
   endif

   # submit batch job to specified account
   #--------------------------------------
   if ("$1" == "-account") then
      shift; if (! $#argv) goto usage
      setenv account "$1"
   endif

   # set batch walltime
   #-------------------
   if ("$1" == "-walltime") then
      shift; if (! $#argv) goto usage
      setenv walltime $1

      set hr = `echo $walltime | cut -d: -f1`
      set ss = `echo $walltime | cut -d: -f3`
      if (("$hr" == "$walltime") || ("$ss" == "")) then
         echo ""
         echo walltime must be in hr:mm:ss format
         goto usage
      endif
   endif

   # set noprompt option
   #--------------------
   if (("$1" == "-np") || ("$1" == "-noprompt")) then
      setenv prompt 0
   endif
   shift
end

# default nodeTYPE
#-----------------
if (! $?nodeTYPE) then
   if ($SITE == NCCS) set nodeTYPE = "Skylake"
   if ($SITE == NAS)  set nodeTYPE = "Skylake"
endif

# This is a flag needed at NCCS for Cascade Lake. Default is blank
set ntaskspernode = ''

# at NCCS
#--------
if ($SITE == NCCS) then

   set nT = `echo $nodeTYPE| tr "[A-Z]" "[a-z]" | cut -c1-3 `
   if (($nT != has) && ($nT != sky) && ($nT != cas) && ($nT != mil)) then
      echo "ERROR. Unknown node type at NCCS: $nodeTYPE"
      exit 1
   endif

   if ($nT == has) @ NCPUS_DFLT = 28
   if ($nT == sky) @ NCPUS_DFLT = 40
   if ($nT == cas) @ NCPUS_DFLT = 48
   if ($nT == mil) @ NCPUS_DFLT = 126

   if ($nT == has) set proc = 'hasw'
   if ($nT == sky) set proc = 'sky'
   if ($nT == cas) then
      set proc = 'cas'
      # Adding this adds prevents a warning from NCCS about using 48
      # tasks per node on Cascade. This script will never actually run
      # make -j48, (usually make -j10 and only asks for 10 tasks) but
      # this suppresses the warning.
      set ntaskspernode = '--ntasks-per-node=45'
   endif
   if ($nT == mil) set proc = 'mil'

   if ("$queue" == "") then
      set queue = '--qos=debug'
   endif

   if ("$partition" == "") then
      set partition = '--partition=compute'
   endif

endif

# at NAS
#-------
if ( $SITE == NAS ) then

   set nT = `echo $nodeTYPE | cut -c1-3 | tr "[A-Z]" "[a-z]"`
   if (($nT != has) && ($nT != bro) && ($nT != sky) && ($nT != cas) && ($nT != rom) && ($nT != mil)) then
      echo "ERROR. Unknown node type at NAS: $nodeTYPE"
      exit 2
   endif

   if ($nT == mil) set nT = 'mil_ait'
   if ($nT == rom) set nT = 'rom_ait'
   if ($nT == sky) set nT = 'sky_ele'
   if ($nT == cas) set nT = 'cas_ait'
   set proc = ":model=$nT"

   if ($nT == has)     @ NCPUS_DFLT = 24
   if ($nT == bro)     @ NCPUS_DFLT = 28
   if ($nT == sky_ele) @ NCPUS_DFLT = 40
   if ($nT == cas_ait) @ NCPUS_DFLT = 40
   if ($nT == rom_ait) @ NCPUS_DFLT = 128
   if ($nT == mil_ait) @ NCPUS_DFLT = 128

   # TMPDIR needs to be reset
   #-------------------------
   if (! "$tmpdir") then
      set tmpdirDFLT = "/nobackup/$USER/scratch/"
      if ($prompt) then
         echo ""
         echo -n "Define TMPDIR location "
         echo    "where scratch files can be written during build."
         echo -n "TMPDIR [$tmpdirDFLT] "
         setenv tmpdir $<
         if ("$tmpdir" == "") setenv tmpdir $tmpdirDFLT
      endif
   endif
   echo "TMPDIR: $tmpdir"

endif

if ($SITE == UNKNOWN) then
   echo ""
   if ($ARCH == Darwin) then
      @ NCPUS_DFLT = `sysctl -a | grep machdep.cpu.core_count | awk '{print $2}'`
   else
      @ NCPUS_DFLT = `cat /proc/cpuinfo  | grep processor | wc -l`
   endif
   echo "Unknown site. Detected $NCPUS_DFLT cores and setting interactive build"
   set interactive = 1
endif

# developer's debug
#------------------
if ($ddb) then
   echo "sdflag = $sdflag"
   echo "debug = $debug"
   echo "fast = $fast"
   if ($?nodeTYPE) then
      echo "nodeTYPE = $nodeTYPE"
   endif
   echo "optflg = $optflg"
   echo "tmpdir = $tmpdir"
   if ($?ESMA_FC) then
      echo "ESMA_FC = $ESMA_FC"
   endif
   echo "proc = $proc"
   echo "interactive = $interactive"
   echo "queue = $queue"
   if ($SITE == NCCS) then
      echo "partition = $partition"
   endif
   echo "account = $account"
   echo "walltime = $walltime"
   echo "prompt = $prompt"
   echo "NCPUS_DFLT = $NCPUS_DFLT"
   exit
endif

# if user defines TMPDIR ...
#---------------------------
if ("$tmpdir" != "") then

   # ... mkdir if it does not exist
   #-------------------------------
   if (! -d $tmpdir) then
      echo "Making TMPDIR directory: $tmpdir"
      mkdir -p $tmpdir
      if ($status) then
         echo ">> Error << mkdir $tmpdir "
         exit 3
      endif
   endif

   # ... check that it is writeable
   #-------------------------------
   if (! -w $tmpdir) then
      echo ">> Error << TMPDIR is not writeable: $tmpdir"  
      exit 4
   endif
   echo ""

endif

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                            JOB SUBMISSION
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# set environment variables
#--------------------------
source $srcdir/g5_modules
setenv ESMADIR $srcdir:h
setenv Pbuild_source_directory $srcdir
setenv Parallel_build_bypass_flag
set jobname = "parallel_build"

#========
# intro
#========
echo ""
echo "   ================"
echo "    PARALLEL BUILD "
echo "   ================"

#===========================
# query for number of CPUs
#===========================
if ( ($SITE == NCCS) || ($SITE == NAS) ) then

   # use set number of CPUs on NCCS or NAS
   #--------------------------------------
   @ ncpus_val  = $NCPUS_DFLT
   @ numjobs_val  = $ncpus_val / 3  # save some CPUs for memory

   # use only even number of CPUs
   #-----------------------------
   @ check = ( $numjobs_val % 2 )
   if ($check == 1) then
      @ numjobs_val --
      echo "Rounding down to an even $numjobs_val CPUs."
   endif

   # Are we on a compute node?
   # -------------------------
   if ( (! $oncompnode) && $interactive) then
      # If we aren't don't use all the cores
      if ($numjobs_val > 6) @ numjobs_val = 6
   else
      # Just use 10 CPUs at most. GEOS doesn't support more
      # parallelism in the build
      if ($numjobs_val > 10) @ numjobs_val = 10
   endif
   echo ""
   echo -n "The build will proceed with $numjobs_val parallel processes on $ncpus_val CPUs"
   if ( (! $oncompnode) && $interactive) then
      echo " to not monopolize head node."
   else
      echo "."
   endif

else

   # how many?
   #----------
   echo ""
   echo -n "Parallel build using how many CPUs ($NCPUs_min minimum)? "
   echo -n "[$NCPUS_DFLT] "
   set ncpus_val = $<
   if ("$ncpus_val" == "") set ncpus_val = $NCPUS_DFLT
   echo ""

   # check for minimum number of CPUs
   #---------------------------------
   if ($ncpus_val < $NCPUs_min) then
      @ ncpus_val = $NCPUs_min
      echo "Defaulting to minimum $ncpus_val CPUs."
   endif

   # use only even number of CPUs
   #-----------------------------
   @ check = ( $ncpus_val / 2 ) * 2
   if ($check != $ncpus_val) then
      @ ncpus_val ++
      echo "Rounding up to an even $ncpus_val CPUs."
   endif

   # save some CPUs for memory
   #--------------------------
   @ numjobs_val = $ncpus_val - 2
   echo -n "The build will proceed with $numjobs_val parallel processes"
   echo    " on $ncpus_val CPUs."

endif

setenv ncpus   $ncpus_val
setenv numjobs $numjobs_val

#=================================================
# check for LOG and info files from previous build
#=================================================
setenv bldlogdir $Pbuild_source_directory/$BUILD_LOG_DIR
setenv buildlog  $bldlogdir/LOG
setenv buildinfo $bldlogdir/info
setenv cleanlog  $bldlogdir/cleanlog
setenv cleanFLAG ""

ls $buildlog $buildinfo >& /dev/null
if ($status == 0) then
   if ($prompt) then
      echo ''
      echo 'Previous build detected - Do you want to clean?'
      echo '(c)     clean: removes *.[aox] files + *.[Mm][Oo][Dd] files'
      echo '(d) distclean: "clean" + removes *.d files'
      echo '(r) realclean: "distclean" + removes'" src/$ARCH directory"
      echo '(n)  no clean'
      echo ''
      echo -n 'Select (c,d,r,n) <<d>> '

      set do_clean = $<
      if ("$do_clean" != "c" && "$do_clean" != "r" && "$do_clean" != "n") then
         set do_clean = "d"
      endif
   else
      set do_clean = "d"
   endif

   if ("$do_clean" == "c") then
      setenv cleanFLAG clean
      echo  "make clean before rebuild"
   else if ("$do_clean" == "d") then
      setenv cleanFLAG distclean
      echo  "make distclean before rebuild"
   else if ("$do_clean" == "r") then
      setenv cleanFLAG realclean
      echo  "make realclean before rebuild"
   else
      echo "No clean before rebuild"
   endif 
endif

#==============
# info for user
#==============
echo ""
echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
echo "LOG and info will be written to the src/$BUILD_LOG_DIR directory."
echo "Do the following to check the status/success of the build:"
echo ""
echo "  cd $BUILD_LOG_DIR"
echo "  ./gmh.pl [-v] LOG[.n]"
echo ""
echo "Note: Check the latest version of LOG for the most recent build info."
echo ""

# give option to build interactively if in interactive batch mode
#----------------------------------------------------------------
if ($?PBS_JOBID || $?SLURM_JOBID) then
   if ($prompt) then
      echo -n "Run build procedure interactively (y/n)? [n] "
      set dummy = $<
      set reponse = `echo $dummy | tr /A-Z/ /a-z/`
      if (($reponse == "y") || ($reponse == "yes")) goto build
   endif
endif

#=============
# submit job
#=============
set groupflag = ""
if ("$account" != "") then
   if ($SITE == NAS) then
      set groupflag = "-W group_list=$account"
   else if ($SITE == NCCS) then
      set groupflag = "--account=$account"
   endif
else if (-e `which getsponsor` && (! $interactive)) then
   set group = ""
   set sponsorinfo = `getsponsor`

   while ("$group" == "")
      @ n = 1
      while ($n < $#sponsorinfo)
         if ($sponsorinfo[$n] =~ g[0..9]*) then
            set group = $sponsorinfo[$n]
            #--break  # uncomment this line to make 1st entry the default
         endif
         @ n++
      end

      getsponsor
      if ("$group" != "") then
         echo -n "select group: [$group] "
      else
         echo -n "select group:  "
      endif
      if ($prompt) then
         set reponse = $<
         if ("$reponse" != "") set group = $reponse
      endif
   end

   set groupflag = "--account=$group"
endif

if ($interactive) then
   @ numjobs = $numjobs - 2
   goto build
else if ( $SITE == NAS ) then
   if ("$walltime" == "") setenv walltime "1:30:00"
   set echo
   qsub  $groupflag $queue     \
        -N $jobname            \
        -l select=1:ncpus=${ncpus}:mpiprocs=${numjobs}$proc \
        -l walltime=$walltime  \
        -S /bin/csh            \
        -V -j oe -k oed        \
        $0
   unset echo
   sleep 1
   qstat -a | grep $USER
else if ( $SITE == NCCS ) then
   if ("$walltime" == "") setenv walltime "1:00:00"
   set echo
   sbatch $groupflag $partition $queue    \
        --constraint=$proc     \
        --job-name=$jobname    \
        --output=$jobname.o%j  \
        --nodes=1              \
        --ntasks=${numjobs}    \
        $ntaskspernode         \
        --time=$walltime       \
        $0
   unset echo
   sleep 1
   # Add a longer format for the job name for scripting purposes
   squeue -a -o "%.10i %.12P %.10q %.30j %.8u %.8T %.10M %.9l %.6D %.6C %R" -u $USER
else
   echo $scriptname": batch procedures are not yet defined for node=$node at site=$SITE"
endif
exit



build:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                             BUILD SYSTEM
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set SRCDIR = $Pbuild_source_directory
chdir $SRCDIR

setenv ESMADIR $SRCDIR:h
setenv ARCH `uname -s`

#=================================================
# create $BUILD_LOG_DIR, plus LOG and info files
#=================================================

# create build log directory
#---------------------------
if (! -d $bldlogdir) mkdir -p $bldlogdir

# copy gmh.pl script to build log directory
#------------------------------------------
if ((! -e $bldlogdir/gmh.pl) && (-e $SRCDIR/Config/gmh.pl)) then
   \cp $SRCDIR/Config/gmh.pl $bldlogdir
endif

# increment LOG and info files if previous versions exist
#--------------------------------------------------------
ls $buildlog $buildinfo >& /dev/null
if ($status == 0) then
   @ next = 0
   foreach file ( $buildlog $buildinfo $cleanlog )
      @ num = 1
      while (-e ${file}.$num)
         @ num ++
      end
      if ($num > $next) @ next = $num
   end
   set buildinfo = $bldlogdir/info.$next
   set buildlog  = $bldlogdir/LOG.$next
   set cleanlog  = $bldlogdir/cleanlog.$next
endif

# alias definitions
#------------------
alias echo2 "echo \!* |& tee    $buildinfo $buildlog"
alias echo1 "echo \!* |& tee -a $buildinfo"
alias date1 "date     |& tee -a $buildinfo"

# initialize files
#-----------------
echo "Writing LOG and info files to directory: $bldlogdir:t"
echo2 ""

#================
# set environment  
#================
source $SRCDIR/g5_modules

$SRCDIR/Assert.pl >>& $buildinfo
$SRCDIR/Assert.pl
if ($status) exit 2

# write environment info to build log
#------------------------------------
set cvstag = "tag-unknown"
if (-e $SRCDIR/CVS/Tag) set cvstag = `cat $SRCDIR/CVS/Tag | cut -dN -f2-`
if ("$tmpdir" != "") setenv TMPDIR $tmpdir
echo1 "======================================"
echo1 `uname -a`
echo1 "ESMADIR: $ESMADIR"
echo1 "BASEDIR: $BASEDIR"
echo1 "CVSTAG: $cvstag"
echo1 "SITE: $SITE"
echo1 "$sdflag"
if ($?TMPDIR) then
   echo1 "TMPDIR = $TMPDIR"
endif
if ($?ESMA_FC) then
   echo1 "ESMA_FC: $ESMA_FC"
endif
if ("$debug" != "") then
   echo1 "debug: $debug"
endif
if ("$fast" != "") then
   echo1 "fast: $fast"
endif
if ("$optflg" != "") then
   echo1 "optimization: $optflg"
endif
if ("$queue" != "") then
   echo1 "queue: $queue"
endif
if ("$account" != "") then
   echo1 "account: $account"
endif
$SRCDIR/Config/OScheck.pl -q >> $buildinfo

echo1 "======================================"
module list >>& $buildinfo
echo1 "======================================"
echo1 ""
echo1 "The following commands will check the status/success of the build:"
echo1 ""
echo1 "  cd $bldlogdir"
echo1 "  ./gmh.pl [-v] $buildlog:t"

#===============
# build system
#===============
if ( $cleanFLAG != "" ) then
    echo1 ""
    echo1 "--------------------------------------"
    date1
    echo1  "make $cleanFLAG"
    make $cleanFLAG $sdflag |& tee -a $cleanlog
    set buildstatus = $status
    echo1 "clean complete; status = $buildstatus"
    date1
    echo1 "--------------------------------------"
    time >> $buildinfo
endif

set cmd = "make $sdflag $debug$optflg --jobs=$numjobs pinstall"
echo1 "" 
echo1 ""
echo1 "--------------------------------------"
echo1 $cmd
date1
echo1  "Parallel build: $numjobs JOBS on $ncpus CPUs ... "
set echo
$cmd |& tee -a $buildlog
set buildstatus = $status

echo1 "build complete; status = $buildstatus"
date1
echo1 "--------------------------------------"
time >> $buildinfo
echo1 ""

# check build results
#--------------------
chdir $bldlogdir
ls $buildlog >& /dev/null
set logstatus = $status

if (($logstatus == 0) && (-e gmh.pl)) then
   ./gmh.pl -A $buildlog
   ./gmh.pl -A $buildlog >> $buildinfo
else
   echo "${scriptname}: >>> WARNING <<< Unable to assess success of build."
   echo "Check for existence of files, $buildlog and gmh.pl, in $bldlogdir."
endif

exit $buildstatus

usage:
cat <<EOF

usage: $0:t [ESMA_FC=fc] [flagged options]
where
    fc                 user-specified Fortran compiler

flagged options
   -debug (or -db)     compile with debug flags (BOPT=g)
   -fast               compile with fast flags (BOPT=fast)
   -help (or -h)       echo usage information
   -hdf                compile with hdf libraries
   -i                  run interactively rather than queuing job
   -opt n              set optimization flag to n, where n={1,2,3,4,5}
   -q qos/queue        send batch job to qos/queue
   -account account    send batch job to account
   -np                 do not prompt for responses; use defaults
   -tmpdir dir         alternate Fortran TMPDIR location
   -walltime hh:mm:ss  time to use as batch walltime at job submittal

   -rom                 compile on Rome nodes (only at NAS)
   -mil                 compile on Milan nodes
   -cas                 compile on Cascade Lake nodes
   -sky                 compile on Skylake nodes (default)
   -bro                 compile on Broadwell nodes (only at NAS)
   -has                 compile on Haswell nodes
EOF
