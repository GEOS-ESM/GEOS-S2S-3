#!/usr/bin/env bash

inputcheck() {
    [[ -z $strf ]] && die "srcme file is a required input"

    $optd && $optf && die "Cannot specify option d and f at the same time"
    return
}

filter() {
    
    local _arrinput=( "$@" )
    local cntbug=0
    local arr=()
    local _dexparch     

    for _dexp in ${_arrinput[@]};do
        local cntbug=0

        set_rstfcstdate $_dexp

        local fcapr=cap_restart
        local fcapend=cap_end
        local _fcapr=$_dexp/$fcapr
        local _fcapend=$_dexp/$fcapend
        local capr_yyyymmdd=$( cat $_fcapr | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
        local realend_yyyymmdd=$( cat $_fcapend | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )

        local nummonth1=3
        local strxmonth=$( nextXmonths $( date -d $fcstdate +%Y ) $( printf '%01g' $( date -d $fcstdate +%m ) ) 0 $(( nummonth1 + 1 ))  )
        local end_mm=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f1 )
        local end_year=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f2 )
        local end3_yyyymmdd=$end_year$end_mm"01"

        local blrerun=$( grep -i "RERUN = " $_dexp/gcm_run.j | rev | cut -d' ' -f1 | rev | tr [:upper:] [:lower:] )
        local blwinner=$( exp_checkwinner $_dexp $ddata )

        if $blwinner || $blrerun ;then
            local expenddate=$realend_yyyymmdd
        else
            local expenddate=$end3_yyyymmdd
        fi

        #todo:  check if cap_restart is = to end yyyymm (regardless of winner or not ) 
        if $blwinner || $blrerun ;then
            (( $expenddate != $capr_yyyymmdd )) && continue
        else
            (( $expenddate > $capr_yyyymmdd )) && continue
        fi

        debug_filter $_dexp

        arr+=( $_dexp )
    done

    echo ${arr[@]} 
}

sendmsg() {

    [[ -f $ferr     ]] && local sizeferr=$( stat --print='%s' $ferr )  || local sizeferr=0
    [[ -f $fmessage ]] && local sizef=$( stat --print='%s' $fmessage ) || local sizef=0

    if (( $sizeferr > 0 || $sizef > 0 ));then 
        if (( $sizef > 0 ));then
            msg_wheader_userdefined 40 "-" $( basename $ferr ) 
            wmessage "$( cat $ferr )"
            blrm=true
        fi

        msg_cyberpostman "$msg_subject" "$eadds" $fmessage $blmsgmutt
        local status_email=$?
#wmessage \@$LINENO status_email = $status_email
#wmessage \@$LINENO blsavefmsg = $blsavefmsg

        if $blsavefmsg && [[ -n $fmsg_fname ]] ;then
            mv $fmessage $dstdout/$fmsg_fname
        else
            (( $status_email == 0 )) && rm -f $fmessage
        fi
        
        $blrm && msg_newfile $ferr
    fi
}


clean_dir() {
    [[ -f *.pyc ]] && rm -f *.pyc
    [[ -n $flock && -f $flock ]] && rm -f $flock
    return
}

#================================================================================
#                                     Begin
#================================================================================
#starthere
hst=$( hostname )
blnode=false
if [[ "$hst" =~ "pfe"* || "$hst" =~ "lfe"* ]];then
    :
elif [[ "$hst" =~ "r"* ]];then
    #note:  this is for host other than pfe @ NAS which has a name starting with 'r'
    blnode=true
    :
elif [[ "$hst" =~ "discover"* ]];then
    echo "this script works only on pfe";exit
else 
    exit
fi

strscr=$(basename "$0" | cut -d'_' -f2 | cut -d'.' -f1 )
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd )"
cdir=$SCRIPTPATH

[[ -z $cdir || ! -d $cdir ]] && echo "cdir is undefined" && exit

cd $cdir

flock=$cdir/${strscr}.lock
ftmp=$cdir/stdout/rundatetime/tmp_$strscr

[[ ! -f $ftmp ]] && install -D /dev/null $ftmp

stmp=$( find $ftmp -printf "%s\n" )
(( $stmp > 5000 )) && rm -f $ftmp && touch $ftmp

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

#todo:  set run-by-cron for false if it's empty
[[ -z $RUN_BY_CRON ]] && RUN_BY_CRON=false


hstshort=$( get_host )
rundebug=0
writetofile=0
userlimexp=999

optb=false
optd=false
optf=false
optu=false
optq=false
optfinishline=false
optzeroarc=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        execute run_gcmarch.sh This script is a strip version of run_arch.sh. 
        ** Warning - There are no filters for dexp!! **
        
        Usage: ./$(basename "$0") [-bchw] [-i|-t] [-q qid] [-d exp dir] [-f data file] srcme_file

        input:        
            A source file, which set various vars for a set of runs (i.e. srcme_pfe_*)

        options:
            -d                        a full path to exp dir 
            -f                        a data filename with a list of dexp dir
            --finish-line=[THRESHOLD, archive outputs from experiments with the remaining 
                               LIMIT] outputs are less than threshold (interger; recommend- 1). 
                                      Max # of run_gcmarch.sh that will be executed is LIMIT
                                      (recommend- 3)
            --zero-arc=[LIMIT]        archive outputs from experiments that archive
                                      has not been started yet. Max # of run_gcmarch.sh
                                      that will be executed is LIMIT
            -q                        reserved queue ID
            -u                        update run_gcmarch.sh
            -b                        run with a debug mode 
            -h                        show this help text
            -w                        write stdout/err in a file
"
file=
verbose=0
cnt=0
while :; do
    case $1 in
       -a|--num-archived  )  [[ "$2" ]] && userinput=$2 && opta=true && shift \
                             || die "opt a or --num-archived requires an argument";;

                       -d )  [[ "$2" ]] && arrdexp=( $2 ) && optd=true && shift \
                             || die "opt d requires an argument";;
                       -f )  [[ "$2" ]] && fdata=$2 && optf=true && shift \
                             || die "opt f requires an argument";;
                       -q )  [[ "$2" ]] && qid=$2 && optq=true && shift \
                             || die "opt q requires an argument";;
         --finish-line=?* )  optfinishline=true && userthres=$( echo ${1#*=} | cut -d',' -f1 ) && \
                             userlimexp=$( echo ${1#*=} | cut -d',' -f2 );;
           --finish-line= )  die "--finish-line requires a non-empty option argument.";; 
            --zero-arc=?* )  optzeroarc=true && userlimexp=${1#*=};;
              --zero-arc= )  die "--zero-arc requires a non-empty option argument.";; 
                       -b )  optb=true && rundebug=1;;
                       -u )  optu=true;;
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


#OPTIND=1
#while getopts 'hwbd:f:r:uq:' option; do
#    case "$option" in
#        d)  $optf && die "Cannot specify option d when specifying option f"
#            optd=true; arrdexp=( $OPTARG );;
#        f)  $optd && die "Cannot specify option f when specifying option d"
#            optf=true; fdata=$OPTARG;
#            [[ ! $fdata ]] && die "$fdata does not eist";;
#        q)  optq=true; qid=$OPTARG;;
#        u)  optu=true;;
#        h)  echo "$usage"; echo "$note"; exit 0;;
#        b)  optb=true;rundebug=1;;
#        w)  writetofile=1;;
#        \?) echo "Invalid option: -$OPTARG" >&2
#            exit 1;;
#        :)  echo "Option -$OPTARG requires an argument." >&2
#            exit 1;;
#    esac
#done

#todo:  get positional inputs. 
shift $((OPTIND-1))
[[ "${1:-}" = "--" ]] && shift
arrposarg=( "$@" )
#================================================================================
#                               Check User Inputs
#================================================================================
i=0
strf=${arrposarg[i]}; i=$(( i + 1 ))
[[ -n $strf && -f $strf ]] && source $strf
inputcheck
#================================================================================
#                                 Set Variables
#================================================================================
#mid
cdate=$( date +%Y%m%d_%H%M )
dmess=$cdir/message

[[ -n $rstexpid ]] && blrstexpid=true && strexpid=$strexpid$rstexpid || blrstexpid=false

ddata=$cdir/output/pckwinners/$strdout
fmessage=$dmess/message_$strscr
ferr=$dmess/stderr_$strscr
fcomp=gcmarch_archcompleted
fdel=gcmarch_deloutcompleted

#msg_subject="$thishst.$strscr" 
msg_subject="${hstshort}.${strscr}: $strexpid"
limnumrun=1
    
pdir=$( echo $DFCST | rev | cut -d'/' -f1 | rev )  

#note:  limit # of experiments to work on
limexp=10
bllim=false
#blshuf=true
blshuf=false

#note:  max limit 
#       screen limit
bltimelimit=false
if [[ -n $RUN_BY_CRON ]] && $RUN_BY_CRON ;then
    [[ -z $chh ]] && chh=$( date +'%-H' )
    if (( 5 < $chh && $chh < 18 ));then 
        limscr=1
        $bltimelimit && maxsec=600 || maxsec=600
    else
        limscr=1
        $bltimelimit && maxsec=900 || maxsec=900
    fi
elif $blnode;then
    limscr=3
    $bltimelimit && maxsec=900 || maxsec=900
    
    if $blrsync;then 
        limscr_gcmarch=50 
    else
        limscr_gcmarch=99999
    fi
else
    limscr=5
    $bltimelimit && maxsec=600 || maxsec=$( echo "365 * 10 * 24 * 3600" | bc )  
    
    if $blrsync;then 
        limscr_gcmarch=50 
    else
        limscr_gcmarch=99999
    fi
fi


$bltimelimit && sec_diff_min=60 || sec_diff_min=900

#note:  process limits
numproc_max=$( ulimit -u )
numsubtract=100
numproc_maxhere=$(( numproc_max - numsubtract ))

#note:  numsec is wailt time in sec and maxsec is total wait time limit
#numsec=60
maxmin=$( echo "$maxsec / 60" | bc  )

arcqid=none

#note:  save fmessage in dstdout. This was created 
#       in case sending fmessage doesn't work 
blsavefmsg=false
fmsg_fname=message_${strscr}_${strexpid}_${cdate} 

feadd=$cdir/mailadd
[[ -f $feadd   ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )
[[ ! -d $dmess ]] && mkdir -p $dmess
#================================================================================
#                                  Main Process
#================================================================================
#letsbegin
#todo:  get all dexp 
if $optf;then
    arrdexp=($( misc_readfbyline $fdata )) 

    #note:  don't sort list of dexp for optf. Run as-is. List might be already in the order of what user prefers.
    arrdexp=($( printf '%s\n' ${arrdexp[@]} | xargs -i bash -c "[[   -d {}/archive       ]] && echo {}" ))
    arrdexp=($( printf '%s\n' ${arrdexp[@]} | xargs -i bash -c "[[ ! -f {}/archive/$fdel ]] && echo {}" ))
    #arrdexp=($( printf '%s\n' ${arrdexp[@]} | xargs -i bash -c "[[ ! -f {}/archive/$fdel ]] && echo {}" | sort -V | uniq ))
    
    #todo:  shuffling items in array
    $blshuf && arrdexp=( $( shuf -e "${arrdexp[@]}") )

    $bllim && arrdexp=($( printf '%s\n' ${arrdexp[@]} | head -$limexp ))

    strmsg="$fdata"
    msg_subject="$msg_subject ($( basename $fdata ))"

elif $optd;then
          arrdexp=($( printf '%s\n' ${arrdexp[@]} | xargs -i bash -c "[[   -d {}/archive ]] && echo {}" ))
          arrdexp=($( printf '%s\n' ${arrdexp[@]} | xargs -i bash -c "[[ ! -f {}/archive/$fdel ]] && echo {}" | sort -V | uniq ))

elif $optfinishline;then 
    arrdexp=($( exp_arcfinishline $userthres  $userlimexp ))

    strmsg="${!optfinishline@}"
    msg_subject="$msg_subject ($strmsg)"

elif $optzeroarc;then
    arrdexp=($( exp_arczero $userlimexp ))

    strmsg="${!optzeroarc@}"
    msg_subject="$msg_subject ($strmsg)"
fi

arrdexp=($( filter ${arrdexp[@]} ))

#!!!!!!!!!!Remove this later (start) !!!!!!!!!!!!!!!!
arrdexp=($( printf '%s\n' ${arrdexp[@]} | grep -v 1999 | sort -V ))
#!!!!!!!!!!Remove this later ( end ) !!!!!!!!!!!!!!!!

#todo:  exit out when optb is ture
if $optb;then 
    ahand_print ${arrdexp[@]}
    wmessage
    exit
fi

(( ${#arrdexp[@]} == 0 )) && exit

numscr_gcmarch=$( screen -ls | grep -i detached 2>/dev/null | grep gcmarch_${strexpid}_  2>/dev/null | wc -l )
numproc=$( ps aux | awk '{ print $1 }' | grep -w $USER | wc -l )
! $blrsync && numsftc=$( cnt_sftc $pdir ) 

msg_wheader 
wmessage $hst

          [[ -n $strmsg ]]  && wmessage "                       User Inputs : $strmsg"
$bllim && (( $limexp > 0 )) && wmessage "      Max Number of Exp to Work on : $( printf '%+5s\n' $limexp )"
$optfinishline && (( $userlimexp > 0 )) && \
                               wmessage "      Max Number of Exp to Work on : $( printf '%+5s\n' $userlimexp )"
$optzeroarc    && (( $userlimexp > 0 )) && \
                               wmessage "      Max Number of Exp to Work on : $( printf '%+5s\n' $userlimexp )"
             $optfinishline && wmessage "      Threshold for Missing Output : $( printf '%+5s\n' $userthres )"
                      $optf && wmessage "               Total Number of Exp : $( printf '%+5s\n' ${#arrdexp[@]} ) of $( printf '%+3s\n' ${#arrdexp[@]} ) "
                               wmessage "                   Max # of Screen : $( printf '%+5s\n' $limscr ) "
                 ! $blrsync && wmessage "          Currently Running Shiftc : $( printf '%+5s\n' $numsftc )"
                   $blrsync && wmessage " Currently Running gcmarch Screens : $( printf '%+5s\n' $numscr_gcmarch ) ( maxlim = $( printf '%+5s\n' $limscr_gcmarch ) )"
                               wmessage "       Currently Running Processes : $( printf '%+5s\n' $numproc ) ( maxlim = $( printf '%+5s\n' $numproc_maxhere ) )"
wmessage

blinitexecute=false
for dexp in ${arrdexp[@]};do
    #todo:  get index of dexp
    for i in "${!arrdexp[@]}"; do
        [[ "${arrdexp[$i]}" == "$dexp" ]] && break 
    done
    
    thisind=$(( i + 1 ))

    #todo:  count # of running screen and shiftc
            numscr=$( screen -ls | grep -i detached 2>/dev/null | grep ${strscr}_${strexpid}_ | wc -l )
    numscr_gcmarch=$( screen -ls | grep -i detached 2>/dev/null | grep gcmarch_${strexpid}_  2>/dev/null | wc -l )

    darch=$dexp/archive

    set_rstfcstdate $dexp

    #!!!!! NOTE - keep this code for now (03/12/2022)
    if $optu;then
        exp_updarchscr $arcqid $dexp
    fi

    #todo:  count number of processes
    numproc=$( ps aux | awk '{ print $1 }' | grep -w $USER | wc -l )

    blexecuted=false
    blinitialnote=false
    totsec=0
    totmin=0

    sec0=$( date +%s )
    sec1=$sec0

    while ! $blexecuted;do 
        #if (( $numproc < $numproc_maxhere && $numscr < $limscr && $totsec <= $maxsec ));then 
        if (( $numproc < $numproc_maxhere && $numscr < $limscr && $numscr_gcmarch < $limscr_gcmarch && $totsec <= $maxsec ));then 
            
            totsec=0
            blexecuted=true
            blinitexecute=true

            wmessage "... $( printf "%+3s of %+3s\n" $thisind ${#arrdexp[@]} ) $dexp  ..."

            #+++++ cd to darch (start) +++++
            cd $darch

            if $optq && [[ -n $qid ]];then 
                screen -dmS ${strscr}_${strexpid}_$fcstdate$ensm bash -c "./run_gcmarch.sh -q $qid >> stderr_gcmarch 2>&1"
            else
                screen -dmS ${strscr}_${strexpid}_$fcstdate$ensm bash -c "./run_gcmarch.sh >> stderr_gcmarch 2>&1"
            fi

            cd - >/dev/null
            #+++++ cd to darch (start) +++++

        else
            sec2=$( date +%s )
            sec_diff=$(( sec2 - sec1 ))
            totsec=$(( sec2 - sec0 ))
            totmin=$( echo "$totsec / 60" | bc )

            #todo:  break out for both for 2 loop (hence, 2)
            $blinitexecute && (( $totsec >= $maxsec )) && break 2 

            if ! $blinitialnote ;then 
                #wmessage "$( date +'%m/%d/%Y %H:%M' ) ... Running Screen - $( printf '%+2s\n' $numscr ) and Running Processes - $( printf '%+2s\n' $numproc ) ... will wait for max of $maxmin minutes ..."
                blinitialnote=true

            elif ! $blinitexecute;then
                  $blrsync && wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screen, $( printf '%+2s\n' $numscr_gcmarch ) gcmarch screen, and $( printf '%+2s\n' $numproc ) processes are running"
                ! $blrsync && wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screen, and $( printf '%+2s\n' $numproc ) processes are running" 
                #sleep 1m
                sleep ${sec_diff_min}s
                sec0=$( date +%s )
                sec1=$sec0
                #sec1=$( date +%s )

            elif (( $sec_diff > $sec_diff_min ));then
                  $blrsync && wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screen, $( printf '%+2s\n' $numscr_gcmarch ) gcmarch screen, and $( printf '%+2s\n' $numproc ) processes are running ... waited for $totmin min ( max = $maxmin ) "
                ! $blrsync && wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screen, and $( printf '%+2s\n' $numproc ) processes are running ... waited for $totmin min ( max = $maxmin ) "
                sec1=$( date +%s )
            fi

        fi
        
                numscr=$( screen -ls | grep -i detached 2>/dev/null | grep ${strscr}_${strexpid}_ 2>/dev/null | wc -l )
        numscr_gcmarch=$( screen -ls | grep -i detached 2>/dev/null | grep   gcmarch_${strexpid}_ 2>/dev/null | wc -l )
        numproc=$( ps aux | awk '{ print $1 }' | grep -w $USER | wc -l )

    done

done 

#todo:  wait for the rest of screen sessions to finish. Otherwise, kill them 
blinitialnote=false
totsec=0
totmin=0

sec0=$( date +%s )
sec1=$sec0
! $bltimelimit && maxsec=600
while (( $numscr > 0 ));do 
    sec2=$( date +%s )
    sec_diff=$(( sec2 - sec1 ))
    totsec=$(( sec2 - sec0 ))
    totmin=$( echo "$totsec / 60" | bc )

    #todo:  break out for both for loop (hence, 2)
    if (( $totsec >= $maxsec ));then
        arrscr=($( screen -ls | grep -i detached | grep ${strscr}_${strexpid}_ | tr -d '\t' | cut -d'(' -f1 ))
        printf '%s\n' ${arrscr[@]} | xargs -i bash -c 'screen -XS {} quit' 2>/dev/null

        wmessage "Killed Screens:"
        ahand_warr ${arrscr[@]} 
        break 2 
    fi

    if ! $blinitialnote ;then 
        wmessage "$( date +'%m/%d/%Y %H:%M' ) ... Running Screen - $( printf '%+2s\n' $numscr ) ... will wait for $maxmin minutes at max ..."
        blinitialnote=true

    elif (( $sec_diff > 60 ));then
        wmessage "$( date +'%m/%d/%Y %H:%M' ) ... Running Screen - $( printf '%+2s\n' $numscr ) ... waited for $totmin min ..."
        sec1=$( date +%s )
    fi
    
    numscr=$( screen -ls | grep -i detached | grep ${strscr}_${strexpid}_ | wc -l )

done



if (( $numscr > 0 ));then
    wmessage 
    wmessage "Running Screens:"
    wmessage "$( screen -ls | grep ${strscr}_${strexpid}_ )"
    wmessage 
fi

wmessage
write_quota_fcst
wmessage "Time Ended - $(TZ=America/New_York date +'%m/%d/%Y %H:%M' )"

[[ -f $ferr     ]] && sizeferr=$( stat --print='%s' $ferr     ) || sizeferr=0

if [[ -f $fmessage ]];then 
    sizef=$( stat --print='%s' $fmessage ) 
    numrun=$( grep -w $hst $fmessage 2>/dev/null | wc -l ) 
else
    sizef=0
    numrun=0
fi
wmessage "@$LINENO ( numrun limnumrun ) = ( $numrun $limnumrun )" 

(( $numrun < $limnumrun )) && exit

#todo:  send email
[[ -f $fmessage ]] && sendmsg


exit



if (( $sizef > 0 || $sizeferr > 0 ));then 
    if (( $sizeferr > 0 ));then 
        msg_wheader_userdefined 40 "-" $( basename $ferr ) 
        wmessage "$( cat $ferr )"
        blrm=true
    fi

    msg_cyberpostman "$msg_subject" "$eadds" $fmessage
    status_email=$?
    (( $status_email == 0 )) && rm -f $fmessage
    $blrm && msg_newfile $ferr
fi

exit


