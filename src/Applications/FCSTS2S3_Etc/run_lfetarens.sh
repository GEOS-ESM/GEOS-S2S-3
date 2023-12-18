#!/usr/bin/env bash

filter() {
    #description: filter exp based on these:
    # - check if dexp exists and ensemble.tar file exist with contents (test -s option)
    # - check if screen is running for dexp

    local arrinput=( "$@" )
    local arr=() dexp

    for dexp in ${arrinput[@]};do 
        local cntbug=0
        local ensm=$( echo $dexp | rev | cut -d'/' -f1 | rev )
        local fcstdate=$( echo $dexp | rev | cut -d'/' -f2 | rev )
        local dparent=$( dirname $dexp )

        #todo:  check if exp dir and ensemble tar file exists and tar file is NOT empty.
        if [[ -d $dexp ]];then
            local numftar=$( find $dparent/* -type f -name "${ensm}*.tar" -not -empty | wc -l )
            if (( $numftar == 0 ));then
                :
            else
                continue
            fi
        else
            continue
        fi

        debug_filter $dexp
        
        #todo:  check if screen is running for this experiment.    
        if [[ -n $dexp ]];then
            screen -ls | grep ${strscr}_$fcstdate$ensm >/dev/null
            if (( $? == 0 ));then 
                continue
            else
                :
            fi
        fi
        
        debug_filter $dexp
            
        if [[ -n $dexp ]];then
            arr+=( $dexp )
        fi
    
    done
    echo "${arr[@]}"
}

clean_dir() {
     [[ -f *.pyc ]] && rm -f *.pyc
    [[ -n $flock && -f $flock ]] && rm -f $flock
    return
}

#================================================================================
#                              Beginning of Script
#================================================================================
#beg
strscr=$(basename "$0" | cut -d'_' -f2 | cut -d'.' -f1 )
hst=$(hostname)

[[ $hst != "lfe"* ]] && echo "this script can not be executed on host other than lfe. exit." && exit
source ~/.knakada/.bashrc_kn

cdir=/nobackupp11/gmaofcst/GEOSS2S3/GEOS_util

cd $cdir

flock=$cdir/${strscr}.lock
ftmp=$cdir/stdout/rundatetime/tmp_$strscr

[[ ! -f $ftmp ]] && mkdir -p $( dirname $ftmp ) && touch $ftmp

#todo:  check tmp file size and create new if it is larger than 5kb
if [[ ! -f $ftmp ]];then touch $ftmp;fi
stmp=$( find $ftmp -printf "%s\n" )
if (( $stmp > 5000 ));then
    rm -f $ftmp;touch $ftmp
fi

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
optb=false
rundebug=false
#================================================================================
#                                     Usage
#================================================================================
#ref: https://goo.gl/BUBwPV
usage="$(basename "$0") -- this is a program to:
        create a tar file for each ensemble member dir for v3 forecast test experiments.
        Usage: ./$(basename "$0") [-hcwb] [-d] 

        options:
            -b  run with a debug mode
            -d  a full path to an experiment dir
            -c  clean unwanted files
            -h  show this help text
            -w  write stdout/err in a file
"

while getopts ':hwbd:' option; do
    case "$option" in
        b)  optb=true;rundebug=true;;
        d)  dexp=$OPTARG;;    
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
fmessage=$cdir/message/message_$strscr
ferr=$cdir/message/stderr_${strscr}
strf=data_${strscr}_
maxscr=5
maxsftc=8

(( $maxscr == 0 )) && exit
[[ ! -f $ferr ]] && touch $ferr

#================================================================================
#                                  Main Process
#================================================================================
#main
numscr=$( screen -ls | grep ${strscr}_ | wc -l )
numsftc=$( shiftc --status=csv | grep run | wc -l )

#todo:  exit when bot of a number of screen session and a number of running sfhitc are 
#       larger than max
(( $numscr >= $maxscr || $numsftc > $maxsftc )) && exit

#todo:  get all exp from data files.
arrfdata=($( find * -maxdepth 0 -name "$strf*" ))

wmessage $LINENO
ahand_warr ${arrfdata[@]}

if (( ${#arrfdata[@]} > 0 ));then
    for fdata in ${arrfdata[@]};do
        while IFS= read -r line; do
            arrdexp+=( $line )
        done < "$fdata"

        #todo:  create a backup copy
        fbak=$cdir/stdout/$strscr/$fdata.bak
        [[ ! -f $fbak ]] && cp $fdata $fbak
    done
else
    exit
fi
#wmessage
#wmessage $LINENO
#ahand_warr ${arrdexp[@]}
#exit
#todo:  filter out exp 
arrfinal=($( filter "${arrdexp[@]}" ))

#todo:  write out arrfinal when optb is true
$optb && ahand_warr ${arrfinal[@]} && exit


(( ${#arrfinal[@]} == 0 )) && exit

msg_wheader
wmessage "... Total number of exp : ${#arrfinal[@]}"

#todo:  start to tar.
for dexp in ${arrfinal[@]};do

    ensm=$( echo $dexp | rev | cut -d'/' -f1 | rev )
    fcstdate=$( echo $dexp | rev | cut -d'/' -f2 | rev )
    dparent=$( dirname $dexp )
    blrunning=false
    fmessage_proc=$cdir/message/message_${strscr}_${fcstdate}${ensm}

    numscr=$( screen -ls | grep ${strscr}_ | wc -l )
    if (( $numscr < $maxscr ));then 
        wmessage "screen $fcstdate$ensm"

        #todo:  tar ensm
        screen -dmS ${strscr}_$fcstdate$ensm bash -c "\
cd $dparent ; \
shiftc -r --wait --create-tar $ensm $ensm.tar >> ${fmessage_proc} 2>&1 ;\
cd - >/dev/null;\
mv $fmessage_proc $cdir/stdout/lfetarens/ "

        blrunning=true
        cntsleep=0
    else
        wmessage "$numscr is running"

        if (( $writetofile == 1 ));then
            screen -ls >> $fmessage 
        else
            screen -ls 
        fi
        exit
    fi
done


exit
