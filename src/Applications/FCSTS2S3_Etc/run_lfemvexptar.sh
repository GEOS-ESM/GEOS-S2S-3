#!/usr/bin/env bash

filter() {
    #description:   check if exp dir is ready for monitoring 
    
    local _arrinput=( "$@" )
    local arr=()
    local input

    for input in ${_arrinput[@]};do
        local cntbug=0

        [[ ! -d $input ]] && continue 
        
        debug_filter $input

        #todo:  check if monitor was already completed.
        if find -- "$input" -prune -type d -empty | grep -q .; then
            continue
        fi
        
        debug_filter $input

        [[ -n $input ]] && arr+=( $input )

    done
    echo "${arr[@]}"
}

clean_dir() {
    [[ -f *.pyc ]] &&  rm -f *.pyc
    [[ -n $flock && -f $flock ]] && rm -f $flock
    return
}

#================================================================================
#                                     Begin
#================================================================================
#beg
hst=$(hostname)
strscr=$(basename "$0" | cut -d'_' -f2 | cut -d'.' -f1 )
if [[ "$hst" =~ "lfe"* ]];then
    cdir=/nobackupp11/gmaofcst/GEOSS2S3/GEOS_util
elif [[ "$hst" =~ "discover"* || "$hst" =~ "pfe"* ]];then
    echo "this script works only on lfe";exit
fi

[[ -z $cdir || ! -d $cdir ]] && echo "cdir is undefined" && exit

cd $cdir

flock=$cdir/${strscr}.lock
ftmp=$cdir/stdout/rundatetime/tmp_$strscr
[[ ! -f $ftmp ]] && install -D /dev/null $ftmp

if [[ ! -f $ftmp ]];then 
    mkdir -p $( dirname $ftmp ) && touch $ftmp
fi

if [[ -f $flock ]];then
    echo "$(TZ=America/New_York date +'%m/%d/%Y %H:%M' ) ... $flock exists..." >> $ftmp
else
    echo $(TZ=America/New_York date +'%m/%d/%Y %H:%M' ) >> $ftmp
fi

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
rundebug=0
optb=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        move forecast exp ensemble tar files (i.e. ens201.tar) to an appropriate 
        location.
        Usage: ./$(basename "$0") [-hcw] 

        options:
            -b  run with a debug mode
            -c  clean unwanted files
            -h  show this help text
            -w  write stdout/err in a file
"

while getopts ':hwb' option; do
    case "$option" in
        b)  optb=true;rundebug=1;;
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
#                                 Set Variables
#================================================================================
#mid
dfcst=/u/$USER/GEOS_S2Sv3
fmessage=$cdir/message/message_$strscr
bldrm=true

#================================================================================
#                                  Main Process
#================================================================================
#main
#todo:  get dexp between beginning and end yyyymm
if (( ${#arrdexp[@]} == 0 ));then

    #%%%%% cvd19 & rim (start) %%%%%
    runcr=false
    if $runcr;then
        dtarexp=/u/gmaofcst/GEOSS2S3/GEOS_exp/testfcst/cvd19
        blrstexp_thesame=false
        arrintens+=($( seq 900 903 ))
        syyyymm=201504
        lyyyymm=201504
        get_beg_and_end $syyyymm $lyyyymm

        arrdexp=($( get_dexp $dfcst $blrstexp_thesame $begyyyymm $endyyyymm ${arrintens[@]} ))
    fi
    #%%%%% cvd19 & rim ( end ) %%%%%

    #%%%%% odas (start) %%%%%
    runodas=false
    if $runodas;then
        dtarexp=/u/gmaofcst/GEOSS2S3/GEOS_exp/testfcst/odas
        blrstexp_thesame=false
        arrintens=( $( seq 400 404 ) $( seq 501 510 ) )
        #arrintens=( 400 )
        syyyymm=201601
        lyyyymm=201601
        get_beg_and_end $syyyymm $lyyyymm

        #** 11/04/2020 201406 to 201508 are moved to $dtarexp
        arrdexp=($( get_dexp $dfcst $blrstexp_thesame $begyyyymm $endyyyymm ${arrintens[@]} ))
    fi
    #%%%%% odas ( end ) %%%%%

    #%%%%% sio20 (start) %%%%%
    runsio=true
    if $runsio;then
        dtarexp=/u/gmaofcst/GEOSS2S3/GEOS_exp/testfcst/sio20
        blrstexp_thesame=false
        #arrintens=($( seq 700 714 ))
        #arrintens+=($( seq 800 814 ))
        #syyyymm=198205
        #lyyyymm=198206
        
        arrintens=($( seq 600 601 ))
        syyyymm=202004
        lyyyymm=202005
        get_beg_and_end $syyyymm $lyyyymm

        #arrdexp=($( get_dexp $dfcst $blrstexp_thesame $begyyyymm $endyyyymm ${arrintens[@]} ))
        arrdexp=($( get_dexp $dfcst $begyyyymm $endyyyymm ${arrintens[@]} ))
    fi
    #%%%%% sio20 ( end ) %%%%%

    #%%%%% noassim (start) %%%%%
    runnoassim=false
    if $runnoassim;then
        dtarexp=/u/gmaofcst/GEOSS2S3/GEOS_exp/testfcst/noassim
        blrstexp_thesame=true
        arrintens=( $( seq 200 204 ) $( seq 301 310 ) )
        
        runhere=false
        if $runhere;then
            #note:  rerun exp
            syyyymm=198208
            lyyyymm=198208
            get_beg_and_end $syyyymm $lyyyymm

            arrdexp=($( get_dexp $dfcst $blrstexp_thesame $begyyyymm $endyyyymm ${arrintens[@]} ))
        fi

        #note:  the rest of noassim
        #** 09/23/2020 all noassim tar are moved to $dtarexp
        syyyymm=198301
        lyyyymm=198302
        get_beg_and_end $syyyymm $lyyyymm

        arrdexp+=($( get_dexp $dfcst $blrstexp_thesame $begyyyymm $endyyyymm ${arrintens[@]} ))
    fi
    #%%%%% noassim ( end ) %%%%%
    
fi

#todo:  get current parent dir path
arrfinal1=($( printf '%s\n' ${arrdexp[@]} | rev | cut -d'/' -f2- | rev | sort | uniq ))
arrfinal=($( filter ${arrfinal1[@]} ))


#todo:  run here if debug mode option (b) is selected
#optb=true
$optb && ahand_warr ${arrfinal[@]} && exit

(( ${#arrfinal[@]} == 0 )) && exit

msg_wheader

#todo:  start to tar.
for dexp in ${arrfinal[@]};do
    dparent=$( echo $dexp | sed 's#'$dfcst'#'$dtarexp'#g' )

    #todo:  check if dexp exists and proceed to process
    if [[ -d $dexp ]];then
        wmessage $dparent

        #todo:  create a parent dir for new location
        [[ ! -d $dparent ]] && mkdir -p $dparent
        
        #todo:  find ens???.tar* files and move them to new location.
        bldemp=$( find $dexp/* -maxdepth 0 -empty -exec echo true \; )
        if $bldemp;then
            arrdrm+=($( find $dexp/* -maxdepth 0 -type f -name 'ens???.tar*' | cut -d'.' -f1 )) 

            find $dexp/* -maxdepth 0 -type f -name 'ens???.tar*' -exec mv {} $dparent/ \; 2>/dev/null
            status_mv=$?

            (( $status_mv > 0 )) && bldrm=false
        fi
    fi
done

#todo:  remove tar'd dir
if $bldrm;then
    wmessage "Delete these dir:"
    ahand_warr ${arrdrm[@]}
    wmessage
    rm -rf ${arrdrm[@]}
fi

exit
