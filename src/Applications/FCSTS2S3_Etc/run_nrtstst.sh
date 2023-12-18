#!/usr/bin/env bash

#================================================================================
#                              Assumptions & Notes
#================================================================================
: '
02/03/2023
- execute these scripts from this script:
   run_rstmkpert.sh
   run_rstprep.sh
   run_rstextr.sh
   run_gcmsetup.sh
   run_pckwinner.sh
   run_monitor.sh
   run_getstatus.sh
   run_smessage.sh

- maybe run these scripts too:
   run_submit.sh

- Idea 1
    Create a template script for this and run it for each forecast dates.
    But this may cause issues if/when forecast execution overwrap with other
    forecast dates.

- run_gcmsetup.sh:  By default this script needs to figure out all forecast dates
                    in the last month and dates in the current month. Then,
                    filter those dates (like it normally would with input options
                    like optr, optt, etc). 
                    => DONE 
                    the script  figure out all forecasts from the last month and ones up to 
                    todays date in the current month. 

- run_rstprep.sh  : By default this script figureout all forecast dates in the last 
                    month and ALL dates in the current month. This is similar to what
                    gcmsetup does but add the rest of current month forecast dates in 
                    case restarts are ready (But this should not happen) 
                    For now (02/15/2023), this script will create tar file in output/rstprep 
                    along with coping rst to $FRST dir.
                    => DONE
                    

- run_rstmkpert.sh: Do the same as rstprep. Alos algorithm will change for near realtime
- run_rstextr.sh  : Do the same as rstprep. Algorithm will change for making perturbation
                    depending on that, this script may not be requied.

- run_rstdis2lfe.sh: No need to change for near-realtime.

- run_rstlfe2pfedis.sh : updated to fiture out near realtime forecast dates in the same
                         way as run_rstprep.sh. This script likely will NOT be used
                         for near real time.  

- run_monitor.sh  : By default, this script should figure out what is running and should
                    be monitored.
                    => DONE
                   
- run_pckwinner.sh: By default this script needs to figure out icyyyymm for pckwinners
                    => DONE
                    As of 02/06/2023, the scripts picks the last two icyyyymm plus current
                    month for filter. 

- run_smessage.sh : Cron can run run_smessage.sh every day just like it is doing now.

Other scripts that calculate near real time dates:
    run_archstatus.sh
    run_getstatus.sh
'
#================================================================================
#                                   Functions
#================================================================================

inputcheck(){
    [[ -z  ${arrposarg[@]} ]]   && die "an input is missing"
    (( ${#arrposarg[@]} != 1 )) && die "1 input is required"

    local srcme=$( echo ${arrposarg[0]} | cut -d'_' -f2 )
    [[ "$srcme" != "$hstshort" ]] && die "source file is not for this host (select srcme_${hstshort}_* file)" && exit

    return
}

clean_dir() {
    [[ -f *.pyc ]] && rm -f *.pyc
    [[ -n $flock && -f $flock ]] && rm -f $flock
    return
}

#================================================================================
#                                     Begin
#================================================================================
#startshere
hst=$( hostname )
blnode=false
if [[ "$hst" =~ "pfe"* || "$hst" =~ "lfe"* ]];then
    :
elif [[ "${hst:0:1}" == "dis" ]];then 
    :
elif [[ "${hst:0:1}" == r ]];then 
    #note:  this is for host other than pfe @ NAS which has a name starting with 'r'
    blnode=true
elif [[ "${hst:0:4}" == borg || "${hst:0:4}" == warp ]];then 
    #note:  this is for host other than pfe @ NCCS which has a name starting with 'borg' or 'warp'
    blnode_nccs=true
else 
    exit
fi

[[ -z ${SCR+x} ]] && source ~/knakada/.bashrc

strscr=$( basename "$0" | cut -d'_' -f2 | cut -d'.' -f1 )
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cdir=$SCRIPTPATH

[[ -z $cdir || ! -d $cdir ]] && echo "cdir is undefined" && exit

blcronrun=false
pppid=$( ps h -o ppid= $PPID )
p_command=$( ps h -o %c $pppid )
[[ "$p_command" == "cron" ]] && blcronrun=true


$blcronrun && echo "cron execution of this script is not allowed. Exit" && exit

cd $cdir

flock=$cdir/${strscr}.lock
ftmp=$cdir/tmp_$strscr

[[ ! -f $ftmp ]] && install -D /dev/null $ftmp

stmp=$( find $ftmp -printf "%s\n" )
(( $stmp > 5000 )) && rm -f $ftmp && touch $ftmp


if [[ -f $flock ]];then
    echo "$(TZ=America/New_York date +'%m/%d/%Y %H:%M' ) ... $flock exists..." >> $ftmp
else
    echo $(TZ=America/New_York date +'%m/%d/%Y %H:%M' ) >> $ftmp
fi

#todo:  lock this script
if set -C; 2>/dev/null >$flock; then
    :
else
    exit
fi

if [[ ! -f $cdir/func_fcst.sh ]];then 
    echo "$cdir/func_fcst.sh does not exist";exit
else
    source $cdir/func_fcst.sh
fi

trap clean_dir EXIT

hstshort=$( get_host )
writetofile=0

#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
    execute near-realtime s2sv3 forecasts.       

    Usage: ./$( basename "$0" ) [-chw] srcme_file 

    This would indicate that program should be called with:

    options without operands: a, D, d, e (any of which may be omitted). Note that in this case some parameters are case-sensitive
    exclusive options: f, g (denoted by the vertical bar)
    options with operands: n
    exclusive options with operands: b, c
    required arguments: req1, req2
    optional argument opt1, which may be used with or without opt2 (marked optional within the group by using another set of square brackets)
    optional argument opt2, which requires opt1

    options:
        -a  yadayada
        -b  yadayada
        -d  a full path to an experiment dir
        -c  clean unwanted files
        -h  show this help text
        -w  write stdout/err in a file
"

#USAGE OPTION1 : USE OWN CODE
#REF: https://mywiki.wooledge.org/BashFAQ/035
# Initialize all the option variables.
# This ensures we are not contaminated by variables from the environment.
file=
verbose=0

while :; do
    case $1 in
        -h|-\?|--help)  show_help && exit;; # Display a usage synopsis.
           -f|--file )  [[ "$2" ]] && file=$2 && shift || die "ERROR: "--file" requires a non-empty option argument.";;
           --file=?* )  file=${1#*=};; # Delete everything up to "=" and assign the remainder.
             --file= )  die "ERROR: "--file" requires a non-empty option argument.";;
        -v|--verbose )  verbose=$((verbose + 1)) ;; # Each -v adds 1 to verbosity.
                  -- )  shift && break ;;   # End of all options.
                 -?* )  printf 'WARN: Unknown option (ignored): %s\n' "$1" >&2 ;;
                   * )  break ;; # Default case: No more options, so break out of the loop.
    esac
    shift
done

# if --file was provided, open it for writing, else duplicate stdout
if [ "$file" ]; then
    exec 3> "$file"
else
    exec 3>&1
fi

# Rest of the program here.
# If there are input files (for example) that follow the options, they
# will remain in the "$@" positional parameters.




#todo:  get positional inputs.
shift $((OPTIND-1))
[[ "${1:-}" = "--" ]] && shift
arrposarg=( "$@" )

#================================================================================
#                               Check User Inputs
#================================================================================
i=0
strf=${arrposarg[i]}; i=$(( i + 1 ))
inputcheck
source $strf

[[ "$strexpid" != "fcst" ]] && die "This script works only for s2sv3 forecast" && exit
#================================================================================
#                             Set Host Specific Vars
#================================================================================
if [[ "$hstshort" == "pfe" ]];then
    #do something here (i.e. load modules, location of applications etc)
    #module load cdo/1.9.0
    #. /usr/share/modules/init/bash
    :
elif [[ "$hstshort" == "dis" ]];then
    #do something here (i.e. load modules, location of applications etc)
    #. /usr/share/modules/init/bash
    #module load other/cdo-1.9.1
    #module load other/comp/gcc-5.3-sp3 other/SSSO_Ana-PyD/SApd_4.3.1_py2.7_gcc-5.3-sp3
    :
elif $blnode;then 
    :
else
    clean_dir
    exit
fi

#module load
#module list
#================================================================================
#                                 Set Variables
#================================================================================
#mid
dstdout=$cdir/stdout/$strscr
dmess=$cdir/message
fmessage=$dmess/message_$strscr
fstderr=$dmessr/stderr_$strscr

lastm=0

#RUNCHILD=1

#(( $writetofile == 1 )) && msg_newfile $fmessage

#================================================================================
#                                  Main Process
#================================================================================
#letsbegin




exit
