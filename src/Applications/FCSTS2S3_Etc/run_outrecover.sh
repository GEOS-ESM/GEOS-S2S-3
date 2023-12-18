#!/usr/bin/env bash


inputcheck() {
    (( ${#arrposarg[@]} != 1 )) && die "one input is required"
    return
}

check_shiftc(){
    #description:   check shiftc status
    local _fmsg=$1
    local _arrrunning=()
    local strsid="Shift id is "

    local sid=$( grep "$strsid" $_fmsg 2>/dev/null | tail -1 | rev | cut -d' ' -f1 | rev )

    if [[ -n $sid ]];then
        printf '%s\n' ${arrsftid_all[@]} | grep $sid > /dev/null 2>&1
        local sid_status=$?
        (( $sid_status == 0 )) && _arrrunning+=( $sid )

        #local sid_status=$( /usr/local/bin/shiftc --status=csv | grep $sid | cut -d',' -f2 )
        #if [[ "$sid_status" == "run" || "$sid_status" =~ "run"* ]];then 
        #    _arrrunning+=( $sid )
        #fi
    fi

    echo "${_arrrunning[@]}"
}


filter() {
    #description:   check if exp dir is ready for monitoring 
    #note:  this fucntion get a list of exp. But it also checks a few things
    #       about exp:
    #           -   check if exp exists
    #           -   check if monitor was already completed
    
    #note:  set rundebug to 1 to use debugging 
    local _arrinput=( "$@" )
    local arr=()
    local cntbug=0
    local dexp


    #todo:  check if dexp exists
    _arrinput=($( printf '%s\n' ${_arrinput[@]} | xargs -i bash -c '[[ -d {} ]] && echo {}' ))
    
    debug_filter ${_arrinput[@]}

    cntbug_init=$cntbug
    for dexp in ${_arrinput[@]};do
        local cntbug=$cntbug_init

        set_rstfcstdate $dexp

        local _fmsg=message_${strscr}_${strexpid}_${fcstdate}${ensm}
        local _numfmsg=$( find $dstdout/* -type f -name "${_fmsg}*" 2>/dev/null | wc -l )
        (( $_numfmsg > 0 ))  && continue 

        debug_filter

        local _fmsg=$dmess/message_${strscr}_${strexpid}_${fcstdate}${ensm}

        if [[ -f $_fmsg ]];then 
            local arrsftc_running=($( check_shiftc $_fmsg ))
            (( ${#arrsftc_running[@]} > 0 )) && continue
        fi

        debug_filter

        if [[ -f $_fmsg ]];then 
            grep $strpstissue $_fmsg > /dev/null 2>&1
            local _status_grep=$?

            (( $_status_grep == 0 )) && continue
        fi

        debug_filter

        local fmessage_base=message_${strscr}_${strexpid}_${fcstdate}${ensm}
        [[ -f $dstdout/${fmessage_base}_PUnf ]] && continue 

        debug_filter

        arr+=( $dexp )
    done

    echo ${arr[@]}
}

clean_dir() {
    [[ -f *.pyc ]] && rm -f *.pyc
    [[ -n $flock && -f $flock ]] && rm -f $flock
    return
}

#================================================================================
#                                     Begin
#================================================================================
#beg
hst=$( hostname )
blnode=false
if [[ "$hst" =~ "pfe"* || "$hst" =~ "lfe"* ]];then
    :
elif [[ "$hst" =~ "r"* ]];then
    #note:  this is for host other than pfe @ NAS which has a name starting with 'r'
    blnode=true
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
writetofile=0
rundebug=0
optb=false
optd=false
optf=false
optr=false
opti=false
optt=false

#================================================================================
#                                     Usage
#================================================================================
#ref: https://goo.gl/BUBwPV
usage="$(basename "$0") -- this is a program to:
        copy forecast/hindcast outputs from LFE to PFE.
        
        Usage: ./$( basename "$0" ) [-cwh] [-d dexp] [-f file with experiment list] srcme

        input:        
            A source file, which set various vars for a set of runs (i.e. srcme_pfe_*)

        options:
            -b  debug mode
            -d  a full path to an experiment dir
            -f  a file with a list of dexp
            -i  archive winners exp first 
            -t  archive 3-month runs first
            -c  clean unwanted files
            -h  show this help text
            -w  write stdout/err in a file
"

OPTIND=1
while getopts 'hwbitd:f:r:' option; do
    case "$option" in
        d)  if $optf || $optr ;then die "Cannot specify option d when specifying option r or f";fi
            optd=true; arrdexp=( $OPTARG );;
        f)  if $optr || $optd ;then die "Cannot specify option f when specifying option r or d";fi
            optf=true; fdata=$OPTARG ;;
        r)  if $optd || $optf ; then die "Cannot specify option r when specifying option f or d";fi
            optr=true; 
            userbegyyyymm=$( echo $OPTARG | cut -d'-' -f1 );
              userendyyyymm=$( echo $OPTARG | cut -d'-' -f2 );;
        i)  $optt && die "Cannot specify option t when specifying option t";
            opti=true;;
        t)  $opti && die "Cannot specify option t when specifying option i";
            optt=true;;
        h)  echo "$usage"; echo "$note"; exit 0;;
        b)  optb=true;rundebug=1;;
        w)  writetofile=1;;
        \?) echo "Invalid option: -$OPTARG" >&2
            exit 1;;
        :)  echo "Option -$OPTARG requires an argument." >&2
            exit 1;;
    esac
done

#todo:  get positional inputs. 
shift $((OPTIND-1))
[[ "${1:-}" = "--" ]] && shift
arrposarg=( "$@" )

#================================================================================
#                               Check User Inputs
#================================================================================
i=0
scrf=${arrposarg[i]}; i=$(( i + 1 ))
inputcheck
[[ -n $scrf ]] && source $scrf

#================================================================================
#                                 Set Variables
#================================================================================
#mid
dmess=$cdir/message
dstdout=$cdir/stdout/$strscr/$strdout
fmessage=$cdir/message/message_${strscr}
blwinnerfirst=false

#msg_subject="$hstshort.$strscr" 
msg_subject="${hstshort}.${strscr}: $strexpid"


#note:  max limit 
#       screen limit
if [[ -n $RUN_BY_CRON ]] && $RUN_BY_CRON ;then
    [[ -z $chh ]] && chh=$( date +'%-H' )
    if (( 5 < $chh && $chh < 18 ));then 
        limscr=3
        limsftc=20
        maxsec=900
    else
        limscr=2
        limsftc=15
        maxsec=900
    fi
elif $blnode;then
    limscr=3
    limsftc=40
    #maxsec=28800
    maxsec=900
else
    limscr=3
    limsftc=20
    maxsec=900
fi

#note:  numsec is wailt time in sec and maxsec is total wait time limit
#numsec=60
maxmin=$( echo "$maxsec / 60" | bc  )

#note:  process limits
numproc_max=$( ulimit -u )
numsubtract=100
numproc_maxhere=$(( numproc_max - numsubtract ))
arcqid=none

strpstissue="Possible_gcmpost_issue"
strnorec="No_recover"

blupdate_latest=true

feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

RUNCHILD=1

[[ ! -d $dmess ]] && mkdir -p $dmess
[[ ! -d $dstdout ]] && mkdir -p $dstdout

export RUNCHILD strpstissue

#================================================================================
#                                  Main Process
#================================================================================
#main
if [[ "$hstshort" == "pfe" ]];then 
    #todo:  find running shiftc that were executed by run_gcmarch.sh
    arrsftid_all=($( /usr/local/bin/shiftc --status=csv | grep run  | tail -n +2 | cut -d',' -f1 ))
    arrsftid_arch=($( printf '%s\n' ${arrsftid_all[@]} | xargs -i bash -c "/usr/local/bin/shiftc --id={} --status=csv | grep 'run\|lou\|GEOS_fcst' | head -1 | cut -d',' -f3 | cut -d':' -f2" | sort -V ))
fi

if $optf;then 
    arrdexp=($( misc_readfbyline $fdata ))
    strmsg="$fdata"
    msg_subject="$msg_subject ($( basename $fdata ))"
fi 

arrdexp=($( filter ${arrdexp[@]} ))

if $opti;then
    #todo:  sort arrfinal and winner exps will be archived first
    blwinnerfirst=true
    arrfinal=($( exp_sortbywinner $blwinnerfirst ${arrdexp[@]} ))
elif $optt;then
    blwinnerfirst=false
    arrfinal=($( exp_sortbywinner $blwinnerfirst ${arrdexp[@]} ))
else
    arrfinal=( ${arrdexp[@]} )
fi

#todo:  exit out when optb is ture
if $optb;then 
    ahand_warr ${arrfinal[@]}
    wmessage
    wmessage "            Number of exp : ${#arrfinal[@]}"
    wmessage " Number of running shiftc : ${#arrsftid_arch[@]} ( shift limit = $limsftc )"
    exit
fi

(( ${#arrfinal[@]} == 0 )) && exit

msg_wheader 

wmessage $hst
[[ -n $strmsg ]] && wmessage "                 User Inputs : $strmsg"
wmessage "         Total number of exp : $( printf '%+5s\n' ${#arrfinal[@]} ) ( maxlim = $( printf '%+5s\n' $limscr  ) )"
wmessage "    Currently Running shiftc : $( printf '%+5s\n' ${#arrsftid_arch[@]} ) ( maxlim = $( printf '%+5s\n' $limsftc ) )"

numproc=$( ps aux | awk '{ print $1 }' | grep -w $USER | wc -l )
wmessage " Currently Running processes : $( printf '%+5s\n' $numproc ) ( maxlim = $( printf '%+5s\n' $numproc_maxhere ) )"
[[ -n $startyyyymm && -n $endyyyymm ]] && wmessage "              Archiving Year : $startyyyymm - $endyyyymm"

if $opti || $optt ;then
    wmessage "                Current Hour : $( printf '%+5s\n' $chh )"
    $opti && wmessage "                               Archive winner runs first"
    $optt && wmessage "                               Archive 3month runs first"
fi

wmessage
blinitexecute=false
for dexp in ${arrfinal[@]};do

    #todo:  get index of darch
    for i in "${!arrfinal[@]}"; do
        [[ "${arrfinal[$i]}" == "$dexp" ]] && break 
    done
    
    thisind=$(( i + 1 ))

    #todo:  count # of running screen and shiftc
    arrsftid_all=($( /usr/local/bin/shiftc --status=csv | grep run  | tail -n +2 | cut -d',' -f1 ))
    numsftid_arch=($( printf '%s\n' ${arrsftid_all[@]} | xargs -i bash -c "/usr/local/bin/shiftc --id={} --status=csv | grep --color 'run\|lou\|GEOS_fcst' | head -1 | cut -d',' -f3 | cut -d':' -f2" | sort -V | wc -l ))
    numscr=$( screen -ls | grep -i detached | grep ${strscr}_${strexpid}_ | wc -l )

    #todo:  count number of processes
    numproc=$( ps aux | awk '{ print $1 }' | grep -w $USER | wc -l )

    set_rstfcstdate $dexp

    thismsg=$dmess/message_${strscr}_${strexpid}_${fcstdate}${ensm}

    #!!!!! NOTE - keep this code for now (03/12/2022)
    if $blupdate_latest;then
        exp_updarchscr $arcqid $dexp
    fi

    blexecuted=false
    blinitialnote=false
    totsec=0
    totmin=0

    sec0=$( date +%s )
    sec1=$sec0

    while ! $blexecuted;do 
        if (( $numproc < $numproc_maxhere && $numsftid_arch < $limsftc && $numscr < $limscr && $totsec <= $maxsec ));then
            
            totsec=0
            blexecuted=true
            blinitexecute=true

            #msg_newfile $thismsg
            wmessage "$( printf "%+3s of %+3s\n" $thisind ${#arrfinal[@]} ) $dexp"
            screen -dmS ${strscr}_${strexpid}_$fcstdate$ensm bash -c "./${strscr}.sh -w -d $dexp -m $thismsg $scrf >> $thismsg 2>&1"

        else
            sec2=$( date +%s )
            sec_diff=$(( sec2 - sec1 ))
            totsec=$(( sec2 - sec0 ))
            totmin=$( echo "$totsec / 60" | bc )

            #todo:  break out for one loop (hence, 2)
            $blinitexecute && (( $totsec >= $maxsec )) && break 2 

            if ! $blinitialnote ;then 
                blinitialnote=true

            elif ! $blinitexecute;then
                #wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screen and $( printf '%+2s\n' $numproc ) processes are running " #... waited for $totmin min"
                wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screen, $( printf '%+2s\n' $numsftid_arch ) shfitc, and $( printf '%+2s\n' $numproc ) processes are running " #... waited for $totmin min"
                sleep 1m
                sec0=$( date +%s )
                sec1=$sec0

            elif (( $sec_diff > 60 ));then
                #wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screen and $( printf '%+2s\n' $numproc ) processes are running ... waited for $totmin min ( max = $maxmin )"
                wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screen, $( printf '%+2s\n' $numsftid_arch ) shfitc, and $( printf '%+2s\n' $numproc ) processes are running ... waited for $totmin min ( max = $maxmin )"
                sec1=$( date +%s )
            fi

        fi
        
        arrsftid_all=($( /usr/local/bin/shiftc --status=csv | grep run  | tail -n +2 | cut -d',' -f1 ))
        numsftid_arch=($( printf '%s\n' ${arrsftid_all[@]} | xargs -i bash -c "/usr/local/bin/shiftc --id={} --status=csv | grep --color 'run\|lou\|GEOS_fcst' | head -1 | cut -d',' -f3 | cut -d':' -f2" | sort -V | wc -l ))
        numscr=$( screen -ls | grep -i detached | grep ${strscr}_${strexpid}_ | wc -l )
        numproc=$( ps aux | awk '{ print $1 }' | grep -w $USER | wc -l )

    done
done
   
#todo:  wait for the rest of screen sessions to finish. Otherwise, kill them 
blinitialnote=false
totsec=0
totmin=0

sec0=$( date +%s )
sec1=$sec0

while (( $numscr > 0 ));do 
    sec2=$( date +%s )
    sec_diff=$(( sec2 - sec1 ))
    totsec=$(( sec2 - sec0 ))
    totmin=$( echo "$totsec / 60" | bc )

    #todo:  break out for both for loop (hence, 2)
    if (( $totsec >= $maxsec ));then
        numscr=$( screen -ls | grep -i detached | grep ${strscr}_${strexpid}_ | wc -l )
        #arrscr=($( screen -ls | grep -i detached | grep ".${strscr}_${strexpid}_" | tr -d '\t' | cut -d'(' -f1 ))
        #printf '%s\n' ${arrscr[@]} | xargs -i bash -c 'screen -XS {} quit' 2>/dev/null

        #wmessage "Killed Screens:"
        #ahand_warr ${arrscr[@]} 
        break 1 
    fi

    if ! $blinitialnote ;then 
        wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screens - running ... will wait for max of $maxmin minutes"
        blinitialnote=true

    elif (( $sec_diff > 60 ));then
        wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screens - running ... waited for $totmin min"
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
wmessage "Time Ended - $(TZ=America/New_York date +'%m/%d/%Y %H:%M' )"

#todo:  send email
if [[ -f $fmessage ]];then
    sizef=$( stat --print='%s' $fmessage )
    if (( $sizef > 0 ));then
        msg_cyberpostman "$msg_subject" "$eadds" $fmessage
        (( $? == 0 )) && rm -f $fmessage
    fi
fi

exit


