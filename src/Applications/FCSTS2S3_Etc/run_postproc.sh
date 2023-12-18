#!/usr/bin/env bash

#set -e
#trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
inputcheck() {
    [[ -z  ${arrposarg[@]} ]] && die "an input is missing"
    (( ${#arrposarg[@]} != 1 )) && die "1 input is required"

    local srcme=$( echo ${arrposarg[0]} | cut -d'_' -f2 )
    
    [[ "$srcme" != "$hstshort" ]] && die "source file is not for this host (select srcme_${hstshort}_* file)"

    ! $optf && ! $optr && ! $optd && ! $optt && die "opt r, d, t or f has to be selected"
    $optr && [[ -z $userbegyyyymm || -z $userendyyyymm ]] && die "YYYYMM range is a required input for option r."

    if $optr && [[ -n $startyyyymm && -n $endyyyymm ]];then
        local int1=$( misc_isinteger $startyyyymm ) 
        local int2=$( misc_isinteger $endyyyymm ) 
        
        if (( $int1 > 0 || $int2 > 0 ));then
            die "YYYYMM range have to be numbers delimited by a hyphen"
        fi
    fi

    $optd && (( ${#arrdexp[@]} == 0 )) && die "opt d requires an input"
    $optf && [[ ! -f $fdata ]] && die "opt f requires an existing data file"
    return
}

filter() {
    #todo:  check dexp is ready to do postproc
    local _arrinput=( "$@" )
    local arr=()
    local dexp

    #todo:  get all running/q'd jobs 
    local arrjobs=($( /u/scicon/tools/bin/qstat -u $USER -W fmt_Queue="-maxw 40" -W o=+Rank0 | sort -V -k4 -t' ' | tr -s '[:space:]' | cut -d' ' -f4 | cut -d'.' -f1 | sort -V | uniq | grep ens ))

    for dexp in ${_arrinput[@]};do
        
        local cntbug=0
        set_rstfcstdate $dexp

        #todo:  check screen session is running
        screen -ls | grep ${strscr}_${strexpid}_$fcstdate$ensm >/dev/null
        local status_grep=$?
        (( $status_grep == 0 )) && continue
        
        debug_filter

        if ! $optredo;then
            #todo:  check there are any diurnal files.
            local numdyyyymm=($( find $dexp/holding/* -maxdepth 1 -mindepth 1 -type d -name '[1-2]?????' | sort -V | wc -l ))
            (( $numdyyyymm > 0 )) && continue

            debug_filter

            #todo:  check there are any diurnal files.
            local numfdiu=($( find $dexp/* -maxdepth 1 -mindepth 1 -type f -name '*.diurnal.*' | sort -V | wc -l ))
            (( $numfdiu > 0 )) && continue
            
            debug_filter
        fi

        #todo:  check mini_gcm_run.j is running
        printf '%s\n' ${arrjobs[@]} | grep -w M$fcstdate$ensm >/dev/null
        local status_grep=$?
        (( $status_grep == 0 )) && continue

        debug_filter

        #todo:  check gcm_post.j is running
        printf '%s\n' ${arrjobs[@]} | grep -w P$fcstdate$ensm >/dev/null
        local status_grep=$?
        (( $status_grep == 0 )) && continue

        debug_filter
   
        arr+=( $dexp )
    done 
    echo ${arr[@]}
}    

clean_dir() {
    [[ -n $flock && -f $flock ]] && rm -f $flock
    return
}

#================================================================================
#                                     Begin
#================================================================================
#beg
hst=$( hostname )
if [[ "$hst" =~ "pfe"* ]];then
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

[[ -z $cdir || ! -d $cdir ]] &&  echo "cdir is undefined"  && exit

cd $cdir

flock=$cdir/${strscr}.lock
ftmp=$cdir/stdout/rundatetime/tmp_$strscr

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

fmessage=$cdir/message/message_$strscr
hstshort=$( get_host )
writetofile=0
optb=false
optd=false
optf=false
optr=false
optt=false
optredo=false
rundebug=0
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename $0) -- this is a program to:
        run gcmpost_GEOSS2S3.script to move files and execute gcm_post.*.j*.

        Usage: ./$(basename "$0") [-bhwz] [-d exp dir] [-f data file] [-r YYYYMM-YYYYMM] [-t YYYYMMDD] srcme_file

        input:        
            A source file, which set various vars for a set of runs (i.e. srcme_pfe_*)

        options:
            -d        a full path to an experiment dir
            -f        a data filename with a list of experiment to monitor
            -t        specify date ( format: YYYYMMDD )
            -r        YYYYMM and YYYYMM range (format: YYYYMM-YYYYMM) 
               --redo re-doing postprocessing (skipping some of filter criteria)
            -b        run with a debug mode (this will not execute ${strscr}.sh)
            -h        show this help text
            -w        write stdout/err in a file
"
file=
verbose=0
cnt=0
while :; do
    case $1 in
                       -d )  [[ "$2" ]] && arrdexp=( $2 ) && optd=true && shift \
                             || die "opt d requires an argument";;
                       -f )  [[ "$2" ]] && fdata=$2  && optf=true && shift \
                             || die "opt f requires an argument";;
                       -r )  [[ "$2" ]] && userbegyyyymm=$( echo $2 | cut -d'-' -f1 ) && \
                             userendyyyymm=$( echo $2 | cut -d'-' -f2 ) && optr=true && shift \
                             || die "opt r requires an option argument.";;
                       -t )  [[ "$2" ]] && useryyyymmdd=$2 && shift && optt=true \
                             || die "opt t requires an option argument";; 
                   --redo )  optredo=true;;
                       -h )  echo "$usage"; echo "$note"; exit 0;;
                       -b )  optb=true;rundebug=1;; 
                       -w )  writetofile=1;;
             -v|--verbose )  verbose=$((verbose + 1));;     # Each -v adds 1 to verbosity.
            -h|-\?|--help )  echo "$usage";exit;;           # Display a usage synopsis.
                       -- )  shift;break;;                  # End of all options.
                      -?* )  printf 'WARN: Unknown option (ignored): %s\n' "$1" >&2;;
                        * )  cnt=$(( cnt + 1 )); break ;;   # Default case: No more options, so break out of the loop.

    esac
    shift
    cnt=$(( cnt + 1 ))
done
#todo:  get positional inputs.
arrposarg=( "$@" )
#================================================================================
#                               Check User Inputs
#================================================================================
i=0
strf=${arrposarg[i]}; i=$(( i + 1 ))
inputcheck
[[ -n $strf ]] && source $strf

#================================================================================
#                             Set Host Specific Vars
#================================================================================
thishst=$( get_host )
if [[ $thishst == pfe ]];then
    :
elif [[ $thishst == dis ]];then
    :
fi

[[ -z $DFCST || ! -d $DFCST  ]] && die "DFCST is undefined or does not exist"
[[ -z $DARCH ]] && die "DARCH is undefined or does not exist"
#================================================================================
#                                 Set Variables
#================================================================================
#mid
dmess=$cdir/message
   
msg_subject="${thishst}.${strscr}: $strexpid"

#note:  screen limit
limscr=5
#limscr=1

#note:  numsec is wailt time in sec and maxsec is total wait time limit
numsec=60
maxsec=120
maxsec=1200
maxmin=$( echo "$maxsec / 60" | bc  )

feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

[[ ! -f $fmessage ]] && touch $fmessage
#================================================================================
#                                  Main Process
#================================================================================
#main
if $optf;then
    arrdexp=($( misc_readfbyline $fdata )) 
elif $optr ;then
    get_beg_and_end $userbegyyyymm $userendyyyymm
    arrdexp1+=($( get_dexp $DFCST $begyyyymm $endyyyymm ${arrintens[@]} ))
    arrdexp=($( printf '%s\n' ${arrdexp1[@]} | xargs -i bash -c "[[ -d {} ]] && echo {}" ))

elif $optt;then
    arrdexp=$( printf "$DFCST/${useryyyymmdd}/ens%s\n" ${arrintens[@]} | xargs -i bash -c '[[ -d {} ]] && echo {}' )
fi

(( ${#arrdexp[@]} == 0 )) && exit

#todo:  get exps which are ready to be cleaned. 
arrfinal=($( filter "${arrdexp[@]}" ))

$optb && ahand_warr ${arrfinal[@]} && wmessage "Total - ${#arrfinal[@]}" && exit


(( ${#arrfinal[@]} == 0 )) && exit

msg_wheader
wmessage $hst
wmessage "Start processes: "
wmessage "         Total number of exp : $( printf '%+5s\n' ${#arrfinal[@]} ) ( maxlim = $( printf '%+3s\n' $limscr  ) )"
[[ -n $startyyyymm && -n $endyyyymm ]] && wmessage "              Archiving Year : $startyyyymm - $endyyyymm"

for dexp in ${arrfinal[@]};do
    
    #todo:  get index of darch
    for i in "${!arrfinal[@]}"; do
        [[ "${arrfinal[$i]}" == "$dexp" ]] && break 
    done
    
    thisind=$(( i + 1 ))

    set_rstfcstdate $dexp
    
    thismsg=$dmess/message_${strscr}_${strexpid}_${fcstdate}${ensm}
    [[ -f $thismsg ]] && msg_newfile $thismsg

    numscr=$( screen -ls | grep -i detached | grep ${strscr}_${strexpid}_ | wc -l )
    
    blexecuted=false
    blinitialnote=false
    totsec=0
    totmin=0

    sec0=$( date +%s )
    sec1=$sec0

    while ! $blexecuted;do 
        #if (( $numscr < $limscr && $totsec <= $maxsec ));then
        if (( $numscr < $limscr ));then
            
            totsec=0
            blexecuted=true

            wmessage "... $( printf "%+3s of %+3s\n" $thisind ${#arrfinal[@]} ) $dexp  ..."

            screen -dmS ${strscr}_${strexpid}_$fcstdate$ensm bash -c "./postproc.sh -d $dexp $strf >> $thismsg 2>&1" 
            #./postproc.sh -d $dexp $strf
            
        else

            sec2=$( date +%s )
            sec_diff=$(( sec2 - sec1 ))
            totsec=$(( sec2 - sec0 ))
            totmin=$( echo "$totsec / 60" | bc )

            #todo:  break out for both for loop (hence, 2)
            #(( $totsec == $maxsec )) && break 2 

            if ! $blinitialnote ;then 
                wmessage "$( date +'%m/%d/%Y %H:%M' ) ... $( printf '%+2s\n' $numscr ) screen sessions are running ... will wait for max of $maxmin minutes ..."
                blinitialnote=true
            elif (( $sec_diff > 60 ));then
                wmessage "$( date +'%m/%d/%Y %H:%M' ) ... $( printf '%+2s\n' $numscr ) screen sessions are running ... waited for $totmin min ..."
                sec1=$( date +%s )
            fi

        fi
        
        numscr=$( screen -ls | grep -i detached | grep ${strscr}_${strexpid}_ | wc -l )

    done
done

wmessage 
wmessage "Running Screens:"
wmessage "$( screen -ls | grep ${strscr}_${strexpid}_ )"
wmessage 
wmessage 
wmessage "Time Ended - $(TZ=America/New_York date +'%m/%d/%Y %H:%M' )"

#todo:  send email
sizef=$( stat --print='%s' $fmessage )
if (( $sizef > 0 ));then
    msg_cyberpostman "$msg_subject" "$eadds" $fmessage
    (( $? == 0 )) && rm -f $fmessage
fi


exit


