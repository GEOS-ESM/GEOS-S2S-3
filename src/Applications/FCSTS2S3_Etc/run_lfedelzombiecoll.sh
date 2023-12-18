#!/usr/bin/env bash

#note: for debugging
#ref: https://is.gd/kxHve1
#exec 5> debug_$(basename "$0")
#BASH_XTRACEFD="5"
#PS4='$LINENO: '
#set -x


#description:   
clean_dir() {
    if [[ -f $flock ]]; then rm -f $flock ;fi
    return
}

#================================================================================
#                              Beginning of Script
#================================================================================
#beg
hst=$(hostname)
if [[ $hst != "lfe"* ]];then echo "this script can not be executed on host other than lfe. exit.";exit;fi
source ~/.knakada/.bashrc_kn

strscr=$(basename "$0" | cut -d'_' -f2 | cut -d'.' -f1 )
cdir=/u/gmaofcst/GEOS_S2Sv3

cd $cdir

futil=/nobackupp11/gmaofcst/GEOSS2S3/GEOS_util

flock=$futil/${strscr}.lock
ftmp=$futil/stdout/rundatetime/tmp_$strscr

if [[ ! -f $ftmp ]];then 
    mkdir -p $( dirname $ftmp ) && touch $ftmp
fi

#todo:  check tmp file size and create new if it is larger than 5kb
if [[ ! -f $ftmp ]];then touch $ftmp;fi

stmp=$( find $ftmp -printf "%s\n" )
if (( $stmp > 5000 ));then
    rm -f $ftmp;touch $ftmp
fi

#∞∞∞∞∞ debug (start) ∞∞∞∞∞
runhere=0
if (( $runhere == 1 ));then
    #ref:   https://intoli.com/blog/exit-on-errors-in-bash-scripts/
    #note:  exit when any command fails
    set -e
    
    #note:   keep track of the last executed command
    trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
    
    #note:  echo an error message before exiting
    trap 'echo "\"${last_command}\" command filed with exit code $?."' EXIT
fi
#∞∞∞∞∞ debug ( end ) ∞∞∞∞∞

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

source /nobackupp11/gmaofcst/GEOSS2S3/GEOS_util/func_fcst.sh

trap clean_dir EXIT

rundelzombie=0
writetofile=0
#================================================================================
#                                     Usage
#================================================================================
#ref: https://goo.gl/BUBwPV
usage="$(basename "$0") -- this is a program to:
        find \"zombie\" collection directories. When previous archiving fails, 
        collection directories (i.e. geosgcm_ocn2d ) can be created as file instead 
        of dir. When this happens, archiving outputs fails. Therefore, these 
        \"zombie\" dir have to be deleted before archving outputs. 
        
        Usage: ./$(basename "$0") [-hcw] [-d] [-aDde] [-f | -g] [-n number] [-b b_arg | -c c_arg] req1 req2 [opt1 [opt2]]

        options:
            -d  a full path to an experiment dir
            -r  delete \"zombie\" dir
            -c  clean unwanted files
            -h  show this help text
            -w  write stdout/err to a file
"
#ref:https://goo.gl/fddH1
#note: disable the verbose error handling by preceding the whole option string with a colon.
while getopts ':hcwrd:' option; do
    case "$option" in
        d)  dexp=$OPTARG;;    
        r)  rundelzombie=1;; 
        h)  echo "$usage"; exit 0;;
        c)  clean_dir; exit 0;;
        w)  writetofile=1;; 
        \?) die "Invalid option: -$OPTARG" >&2
            exit 1;;
        :)  die "Option -$OPTARG requires an argument." >&2
            exit 1;;
    esac
done

#================================================================================
#                    Set Global Variables and Source Scripts
#================================================================================
#mid
ferr=$futil/message/stderr_${strscr}
fmessage=$futil/message/message_$strscr

dfoutv3=/u/gmaofcst/GEOS_S2Sv3
fhis=/nobackupp11/gmaofcst/GEOSS2S3/GEOS_util/setup/HISTORY_2.rc

if [[ ! -f $fmessage ]];then touch $fmessage;fi
if [[ ! -f $ferr ]];then touch $ferr;fi

#================================================================================
#                                  Main Process
#================================================================================
#proc
#todo: remove "zombie" collection dir, which is a file, but not a  dir. 
arrcoll=($( exp_getcollections $fhis ))

#todo: set dir of interest. 
if [[ -n $dexp ]];then 
    dfout=$dexp
else
    dfout=$dfoutv3
fi

msg_wheader

wmessage checking $dfout
wmessage

#todo:  find zombie collection.
cd $dfout
for coll in ${arrcoll[@]};do
    indcoll=$( IndexOf $coll ${arrcoll[@]} ) 
    wmessage "$( printf '%02g' $indcoll ) of ${#arrcoll[@]} ... $coll ..."
    arrout+=($( find * -maxdepth 2 -mindepth 2 -type f -name $coll ))
done
wmessage
cd - >/dev/null

#todo:  delete "zombie" dir
if (( $rundelzombie == 1 )) && (( ${#arrout[@]} > 0 ));then
    wmessage "Delete:"
    for fout in ${arrout[@]};do
        if [[ -f $fout ]];then 
            wmessage "    $fout"
            rm -f $fout
        fi
    done
else
    if (( ${#arrout[@]} > 0 ));then
        wmessage "Discovered zombies:"            
        printf '        %s\n' ${arrout[@]} | sort
    else
        wmessage "No zombie exists"
    fi
fi

exit
