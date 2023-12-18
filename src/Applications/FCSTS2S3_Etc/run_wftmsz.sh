#!/usr/bin/env bash
inputcheck(){
    [[ -d $input ]] && dinput=$input
    [[ -f $input ]] && fdata=$input
    [[ -z $dinput && -z $fdata ]] && die "an exisiting dir or a file with a list of existing dir is a required input"
}

clean_dir() {
    [[ -f $flock ]] && rm -f $flock
    return
}

#================================================================================
#                                     Begin
#================================================================================
#beg
hst=$( hostname )
if [[ "$hst" =~ "pfe"* || "$hst" =~ "lfe"* ]];then
    :
elif [[ "$hst" =~ "r"* ]];then
    #note:  this is for host other than pfe @ NAS which has a name starting with 'r'
    blnode=true
elif [[ "$hst" =~ "discover"* ]];then
    echo "this script works only on pfe (for now)";exit
else
    exit
fi

strscr=$(basename "$0" | cut -d'_' -f2 | cut -d'.' -f1 )
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cdir=$SCRIPTPATH

[[ -z $cdir || ! -d $cdir ]] && exit

cd $cdir

flock=$cdir/${strscr}.lock
ftmp=$cdir/stdout/rundatetime/tmp_$strscr

#todo:  check tmp file size and create new if it is larger than 5kb
[[ ! -f $ftmp ]] && install -D /dev/null $ftmp

#todo:  write date & time when this script is executed.
if [[ -f $flock ]];then
    echo "$(TZ=America/New_York date +'%m/%d/%Y %H:%M' ) ... $flock exists..." >> $ftmp
else
    echo $(TZ=America/New_York date +'%m/%d/%Y %H:%M' ) >> $ftmp
fi

#todo:  lock this script (ref: https://www.putorius.net/?p=2607)
if set -C; 2>/dev/null >$flock; then
    :
else
    exit
fi

if [[ ! $cdir/func_fcst.sh ]];then 
    echo "$cdir/func_fcst.sh does not exist";exit
else
    source $cdir/func_fcst.sh
fi
trap clean_dir EXIT

writetofile=0
opti=false
optp=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        write timestamp and size of existing files in a given dir or a list of dir in a
        given file. The final list has timestamp, file size, and filename.

        Usage: ./$(basename "$0") [-hc] [-i] datafile 

        input:
            a full path of an existing dir OR a final name, which has a list
            of existing directories.

        options:
            -i  include files in subdirectories
            -p  list full path of each files instead of file name.
            -c  clean unwanted files
            -h  show this help text
            -w  write stdout/err in a file
"

OPTIND=1
while getopts ':hcwpi' option; do
    case "$option" in 
        i)  opti=true;;
        p)  optp=true;;
        h)  echo "$usage"; exit 0;;
        c)  clean_dir; exit 0;;
        w)  writetofile=1;;
        \?) die "Invalid option: -$OPTARG" >&2
            exit 1;;
        :)  die "Option -$OPTARG requires an argument." >&2
            exit 1;;
    esac
done

#todo:  get positional inputs. 
shift $((OPTIND-1))
[[ "${1:-}" = "--" ]] && shift
arrposarg=( "$@" )

(( ${#arrposarg[@]} == 0 )) && die "input is missing"
(( ${#arrposarg[@]} >  1 )) && die "there are more than 1 input"
#================================================================================
#                               Check User Inputs
#================================================================================
input=${arrposarg[0]}
inputcheck

#================================================================================
#                    Set Global Variables and Source Scripts
#================================================================================
#mid
dmess=$cdir/message
fmessage=$dmess/message_$strscr
cdate=$( date +%Y%m%d_%H%M%S )
strfname=${strscr}_flist

msg_newfile $fmessage
#================================================================================
#                                  Main Process
#================================================================================
#main
#todo:  create arrdinput array.
if [[ -n $dinput ]];then
    arrdinput=( $dinput )
elif [[ -n $fdata ]];then
    arrdinput=($( misc_readfbyline $fdata ))
fi

#todo:  run
for dinput in ${arrdinput[@]};do

    fexist=$dinput/${strscr}_flist
     ftmp1=$dinput/${strscr}_tmp1
     ftmp2=$dinput/${strscr}_tmp2
   
    [[ -f $ftmp1 ]] && rm -f $ftmp1
    [[ -f $ftmp2 ]] && rm -f $ftmp2
    
    #todo:  clean output files.
    find $dinput/* -type f -name $strfname -delete 2>/dev/null

    #todo:  get a file with name of existing monthly, daily, & diurnal files from lou.
    if $opti;then
        find $dinput/* -type f 2>/dev/null | xargs -i stat --printf="%Y %s %n\n" {} >> $ftmp1 2>/dev/null
        cat $ftmp1 | sort -V >> $ftmp2 2>/dev/null 
        mv $ftmp2 $ftmp1
    else
        find $dinput/* -maxdepth 0 -type f 2>/dev/null | xargs -i stat --printf="%Y %s %n\n" {} | sort -n >> $ftmp1 2>/dev/null
    fi
    
    #todo:  leave file full paths if optp is true. Otherwise, edit the third column and leave only 
    #       file name instead of a full path. 
    if $optp;then
        mv $ftmp1 $fexist
    else
        awk -f ${strscr}.awk $ftmp1 > $fexist
    fi
    
    wmessage "Output Files : $fexist"
    
    [[ -f $ftmp1 ]] && rm -f $ftmp1
done

exit

