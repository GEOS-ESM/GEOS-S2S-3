#!/usr/bin/env bash
inputcheck() {
    [[ -z  ${arrposarg[@]} ]] && die "an input is missing"
    (( ${#arrposarg[@]} != 1 )) && die "1 input is required"

    local srcme=$( echo ${arrposarg[0]} | cut -d'_' -f2 )
    
    [[ "$srcme" != "$hstshort" ]] && die "source file is not for this host (select srcme_${hstshort}_* file)"

    #! $optf && ! $optr && ! $optd  && die "opt r or f has to be selected"
    $optr && [[ -z $userbegyyyymm || -z $userendyyyymm ]] && die "YYYYMM range is a required input for option r."

    if $optr && [[ -n $userbegyyyymm && -n $userendyyyymm ]];then
        local int1=$( misc_isinteger $userbegyyyymm ) 
        local int2=$( misc_isinteger $userendyyyymm ) 
        
        if (( $int1 > 0 || $int2 > 0 ));then
            die "YYYYMM range have to be numbers delimited by a hyphen"
        fi
    fi

    $optd && (( ${#arrdexp1[@]} == 0 )) && die "opt d requires an input"
    $optf && [[ ! -f $fdata ]] && die "opt f requires an existing data file"
    return
}

check_rerun() {
    local _dexp=$1
    local bl10=false
    local blrerun=false
    local fcstensm output

    set_rstfcstdate $_dexp

    #todo:  check this exp is a winner. If so, this exp will be submitted
    for rstensm in "${!assoc_winensm[@]}"; do
        [[ "$rstensm" == "$fcstdate/$ensm" ]] && bl10=true && break
    done

    #todo:  check if this is rerun
    blrerun=$( grep -i "RERUN" $_dexp/gcm_run.j | head -1 | rev | cut -d' ' -f1 | rev | tr '[:upper:]' '[:lower:]'  )
    output=$bl10":"$blrerun
    
    #!!!!!NOTE: as of 04/14/2021, keep rerun = false
    #output=$bl10":false"
    echo $output
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
    local _arrrmstdout=() 
    local cntbug=0
    local dexp

    #todo:  check if clean_completed marker exists
    _arrinput=($( printf '%s\n' ${_arrinput[@]} | xargs -i bash -c "[[ ! -f {}/clean_completed ]] && echo {}" ))

    debug_filter ${_arrinput[@]}

    #todo:  check if gcm_run.j exists
    _arrinput=($( printf '%s\n' ${_arrinput[@]} | xargs -i bash -c "[[ -f {}/gcm_run.j ]] && echo {}" ))

    debug_filter ${_arrinput[@]}

    #todo:  check if run_gcmarch.sh exists
    _arrinput=($( printf '%s\n' ${_arrinput[@]} | xargs -i bash -c "[[ -f {}/$farch ]] && echo {}" ))

    debug_filter ${_arrinput[@]}

    #todo:  get all running/q'd jobs 
    if [[ $hstshort == pfe ]];then 
        local _arrjobs_dexp=($( exp_getdexp ))
    elif [[ $hstshort == dis ]];then 
        if [[ -n ${arrjobs_dexp[@]} ]];then 
            local _arrjobs_dexp=( ${arrjobs_dexp[@]} ) 
        else
            local _arrjobs_dexp1=($( exp_getdexp ))
            local _arrjobs_dexp=($( printf '%s\n' ${_arrjobs_dexp1[@]} | grep $DFCST ))
        fi
    fi

    cntbug_init=$cntbug
    for dexp in ${_arrinput[@]};do
        local cntbug=$cntbug_init

        set_rstfcstdate $dexp

        #todo:  check if winner is determined. 
        #winners_nino3.4_fcst_198401_198403.txt 
        local _yyyymm=$( echo $fcstdate | cut -c1-6 )
        local _numfwin=$( find $ddata/* -maxdepth 0 -type f -name "winners_${reg}_${strexpid}_${_yyyymm}_*" 2>/dev/null | wc -l )

        local  fout3=message_${strscr}_${strexpid}_${fcstdate}${ensm}_3mon
        local fout10=message_${strscr}_${strexpid}_${fcstdate}${ensm}_10mon
        #local bl10=$( check_rerun $dexp | cut -d':' -f1 ) 
        local bl10=$( exp_checkwinner $dexp $ddata ) 
        local blrerun=$( check_rerun $dexp | cut -d':' -f2 ) 
        
        #todo:  check if monitor output exist in stdout
        if [[ -n $dexp && -d $dexp ]];then
            #local  numfout3=$( find  $dstdout/* -type f -name "${fout3}*" 2>/dev/null | wc -l )
            #local numfout10=$( find $dstdout/* -type f -name "${fout10}*" 2>/dev/null | wc -l )
            local  numfout3=$( fd  $dstdout/${fout3} 2>/dev/null | wc -l )
            local numfout10=$( fd $dstdout/${fout10} 2>/dev/null | wc -l )
            
            (( $numfout3  == 1 )) &&  fout3_fname=$( find  $dstdout/* -type f -name "${fout3}*" 2>/dev/null )
            (( $numfout10 == 1 )) && fout10_fname=$( find  $dstdout/* -type f -name "${fout10}*" 2>/dev/null )

        else
            #todo:  clean message file if exists
            find $dmess/* -type f -name "message_${strscr}_${strexpid}_${fcstdate}${ensm}_*" -delete 2>/dev/null
            continue
        fi

        debug_filter

        #todo: find monitor output in message dir
        ##local numfmon_dmess=$( find $dmess/* -type f -name "message_${strscr}_${strexpid}_${fcstdate}${ensm}_*" 2>/dev/null | wc -l ) 
        #local numfmon_dmess=$( fd $dmess/message_${strscr}_${strexpid}_${fcstdate}${ensm}_ 2>/dev/null | wc -l ) 
        #(( $numfmon_dmess == 0 )) && continue 

        #debug_filter

        #todo:  check if holding dir exists
        if [[ ! -d $dexp/holding ]];then 
            find $dmess/* -type f -name "message_${strscr}_${strexpid}_${fcstdate}${ensm}_*" -delete 2>/dev/null
            continue
        fi

        debug_filter

        #todo:  check gcm_run.j is running
        local _numexp_running=$( printf '%s\n' ${_arrjobs_dexp[@]} | grep -w $dexp 2>/dev/null | wc -l )

        [[ -n $fout3_fname  ]] && local  _numfnewer_3mo=$( find $dexp/* -maxdepth 0 -type f -newermt "$( stat --printf='%.16y\n' $fout3_fname  )" | wc -l ) || local _numfnewer_3mo=0 
        [[ -n $fout10_fname ]] && local _numfnewer_10mo=$( find $dexp/* -maxdepth 0 -type f -newermt "$( stat --printf='%.16y\n' $fout10_fname )" | wc -l ) || local _numfnewer_10mo=0 

#wmessage \@$LINENO        
#wmessage $_numfnewer_3mo $_numfnewer_10mo        
#wmessage $fout3_fname
#wmessage "$( stat --printf='%.16y\n' $fout3_fname)"
#if [[ -n $fout3_fname  ]];then 
#    local  _arrfnewer=$( find $dexp/* -maxdepth 0 -type f -newermt "$( stat --printf='%.16y\n' $fout3_fname  )"  ) 
#fi
#ahand_print ${_arrfnewer[@]} 
##wmessage $fout10_fname
##wmessage "$( stat --printf='%.16y\n' $fout10_fname)"
#wmessage
#exit
        if (( $_numfwin == 1 ));then

            if (( $_numexp_running > 0 ));then 
                local _pass="@$LINENO" 
                find $dmess/* -type f -name "message_${strscr}_${strexpid}_${fcstdate}${ensm}_*" -delete 2>/dev/null

            elif   $bl10 && (( $_numfnewer_10mo > 0 ));then 
                local _pass="@$LINENO" 
                find $dmess/* -type f -name "message_${strscr}_${strexpid}_${fcstdate}${ensm}_*" -delete 2>/dev/null

            elif ! $bl10 && (( $_numfnewer_3mo > 0 ));then 
                local _pass="@$LINENO" 
                find $dmess/* -type f -name "message_${strscr}_${strexpid}_${fcstdate}${ensm}_*" -delete 2>/dev/null

            elif $bl10 && (( $numfout10 == 1 )) ;then
                local _pass="@$LINENO" 
                find $dmess/* -type f -name "message_${strscr}_${strexpid}_${fcstdate}${ensm}_*" -delete 2>/dev/null
                continue

            elif ! $bl10 && (( $numfout3 == 1 )) ;then
                local _pass="@$LINENO" 
                find $dmess/* -type f -name "message_${strscr}_${strexpid}_${fcstdate}${ensm}_*" -delete 2>/dev/null
                continue

            elif $blrerun  && (( $numfout10 == 1 )) ;then
                local _pass="@$LINENO" 
            
                find $dmess/* -type f -name "message_${strscr}_${strexpid}_${fcstdate}${ensm}_*" -delete 2>/dev/null
                continue
            else
                local _pass="@$LINENO" 
                :
            fi
        else
            #todo:  check if monitor was already completed.
            if (( $_numexp_running > 0 ));then 
                local _pass="@$LINENO" 
                find $dmess/* -type f -name "message_${strscr}_${strexpid}_${fcstdate}${ensm}_*" -delete 2>/dev/null

            elif (( $numfout10 == 0 && $numfout3 == 0 ));then
                local _pass="@$LINENO" 
                :

            elif $bl10 && (( $numfout10 == 0 )) ;then
                local _pass="@$LINENO" 
                :
            elif ! $bl10 && (( $_numfnewer_3mo > 0 ));then 
                local _pass="@$LINENO" 
                #note:  03/20/2023 Keep an eye on this elif statement. Make sure this works for hindcast runs.
                find $dmess/* -type f -name "message_${strscr}_${strexpid}_${fcstdate}${ensm}_*" -delete 2>/dev/null

            elif   $bl10 && (( $_numfnewer_10mo > 0 ));then 
                local _pass="@$LINENO" 
                #note:  03/20/2023 Keep an eye on this elif statement. Make sure this works for hindcast runs.
                find $dmess/* -type f -name "message_${strscr}_${strexpid}_${fcstdate}${ensm}_*" -delete 2>/dev/null

            elif $blrerun  && (( $numfout10 == 1 )) ;then
                local _pass="@$LINENO" 
                find $dmess/* -type f -name "message_${strscr}_${strexpid}_${fcstdate}${ensm}_*" -delete 2>/dev/null
                continue

            #elif (( $_numexp_running > 0 && $numfout3 == 1 ));then 
            #    #note:  03/13/2023 Keep an eye on this elif statement. Make sure that this works for hindcast runs.
            #    #                  This was added because near real time forecast test kept writing monitor outputs
            #    #                  even if 3month run was completed.
            #    find $dmess/* -type f -name "message_${strscr}_${strexpid}_${fcstdate}${ensm}_*" -delete 2>/dev/null
            #    continue

            else
                local _pass="@$LINENO" 
                #todo:  clean message file if exists
                find $dmess/* -type f -name "message_${strscr}_${strexpid}_${fcstdate}${ensm}_*" -delete 2>/dev/null
                continue
            fi
        fi

        debug_filter
        (( $rundebug > 0 )) && wmessage "    pass - $_pass"
               
        arr+=( $dexp )
    done

    echo ${arr[@]}
}

filter_sub() {
    #note:  set rundebug to 1 to use debugging 
    local _arrinput=( "$@" )
    local _arrout=()
    local _dexp

    for _dexp in ${_arrinput[@]};do
        cnt_jobs $_dexp
        thisstatus=$( exp_status $_dexp )
        if [[ "$thisstatus" != "Sub" ]];then 
            continue
        else
            _arrout+=( $_dexp )
        fi
    done
    echo "${_arrout[@]}"
}

clean_dir() {
    [[ -n $flock && -f $flock ]] && rm -f $flock
    return
}

#================================================================================
#                                     Begin
#================================================================================
#starthere
hstname=$( hostname )
blnode=false
if [[ ${hstname:0:3} == pfe ]];then
    :
elif [[ ${hstname:0:3} == dis ]];then
    :
elif [[ "${hstname:0:1}" == r ]];then 
    #note:  this is for host other than pfe @ NAS which has a name starting with 'r'
    #source ~/knakada/.bashrc_kn
    blnode=true

elif [[ "${hstname:0:4}" == borg || "${hstname:0:4}" == warp ]];then 
    #note:  this is for host other than pfe @ NCCS which has a name starting with 'borg' or 'warp'
    :
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

if [[ ! $cdir/func_fcst.sh ]];then 
    echo "$cdir/func_fcst.sh does not exist";exit
else
    source $cdir/func_fcst.sh
fi

trap clean_dir EXIT

hstshort=$( get_host )
writetofile=0
writetofile_child=0
runrerun=0
rundebug=0
optb=false
optd=false
optf=false
optr=false
optt=false
optl=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename $0) -- this is a program to:
        monitor forecast experiment.

        Usage: ./$(basename "$0") [-bhwz] [-d exp dir] [-f data file] [-r YYYYMM-YYYYMM] srcme_file

        input:        
            A source file, which set various vars for a set of runs (i.e. srcme_pfe_*)

        options:
            -d  a full path to an experiment dir
            -f  a data filename with a list of experiment to monitor
            -r  YYYYMM and YYYYMM range (format: YYYYMM-YYYYMM) 
            -b  run with a debug mode (this will not execute ${strscr}.sh)
            -l  write details including existing outputs on archive host (Default: false)
            -h  show this help text
            -w  write stdout/err in a file
            -z  write stdout/err of CHILD SCRIPTS to a file
"
OPTIND=1
while getopts 'hwbzld:f:r:' option; do
    case "$option" in
        d)  if $optf || $optr ;then die "Cannot specify option d when specifying option r or f";fi
            optd=true; arrdexp1=( $OPTARG );;
        f)  if $optr || $optd ;then die "Cannot specify option f when specifying option r or d";fi
            optf=true; fdata=$OPTARG ;;
        r)  if $optd || $optf ; then die "Cannot specify option r when specifying option f or d";fi
            optr=true; 
            userbegyyyymm=$( echo $OPTARG | cut -d'-' -f1 );
              userendyyyymm=$( echo $OPTARG | cut -d'-' -f2 );;
        l)  optl=true;; 
        h)  echo "$usage"; echo "$note"; exit 0;;
        b)  optb=true;rundebug=1;;
        w)  writetofile=1;;
        z)  writetofile_child=1;;
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
inputcheck

srcf=$( realpath ${arrposarg[0]} )
source $srcf
#================================================================================
#                             Set Host Specific Vars
#================================================================================
if [[ "$hstshort" == "dis" ]];then
    cmd_gjob=cmd_gjob_nccs 
elif [[ "$hstshort" == "pfe" ]];then
    cmd_gjob=cmd_gjob_nas
fi

[[ -z $DFCST || ! -d $DFCST  ]] && die "DFCST is undefined or does not exist"
[[ -z $DARCH ]] && die "DARCH is undefined or does not exist"
#================================================================================
#                                 Set Variables
#================================================================================
#mid
thishst=$( get_host )
[[ -n $rstexpid ]] && strexpid=$strexpid$rstexpid

cdate=$( date +%Y%m%d_%H%M )
dfcst=$DFCST
dmess=$cdir/message
ddata=$cdir/output/pckwinners/$strdout
dstdout=$cdir/stdout/$strscr/$strdout
dtmpdata=$cdir/data/submit
strgstat=getstatus
dgetstatus=$cdir/output/${strgstat}/${strexpid}
fgetstatus=data_${strgstat}_${strexpid}_list_
fsub=$dtmpdata/data_submit_${strexpid}_${strscr}_$cdate

farch=archive/run_gcmarch.sh
fmessage=$dmess/message_$strscr
ferr=$dmess/stderr_$strscr
blrm=false

scrchild=${strscr}.sh

msg_subject="${hstshort}.${strscr}: $strexpid"

#chh=$( date +%-H )
#cmm=$( date +%-M )
#arrhhsendmsg=( 0 3 6 9 12 15 18 21 )
#totrun=50


numsec=15
maxsec=900
maxmin=$( echo "$maxsec/60" | bc )

#note:  process limits
numproc_max=$( ulimit -u )
numsubtract=50
numproc_maxhere=$(( numproc_max - numsubtract ))

RUNCHILD=1

msg_hrinc=3
msg_hrpref=6

feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

[[ ! -d $dtmpdata ]] && mkdir -p $dtmpdata
[[ ! -d $dmess    ]] && mkdir -p $dmess
[[ ! -d $ddata    ]] && mkdir -p $ddata
[[ ! -d $dstdout  ]] && mkdir -p $dstdout
[[ ! -f $scrchild ]] && die "$scrchild does not exist"

declare -A assoc_winensm
declare -A assoc_ensmwin

export RUNCHILD cdir
#================================================================================
#                                  Main Process
#================================================================================
#letsbegin
if $optf;then
    arrdexp1=($( misc_readfbyline $fdata )) 
    strmsg="$fdata"

    if [[ -z $msg_hrinc && -z $msg_hrpref ]];then 
        msg_subject="$msg_subject ($( basename $fdata ))"
    fi

elif $optr ;then
    get_beg_and_end $userbegyyyymm $userendyyyymm
    arrdexp1+=($( get_dexp $dfcst $begyyyymm $endyyyymm ${arrintens[@]} ))
    strmsg="$userbegyyyymm - $userendyyyymm"

    if [[ -z $msg_hrinc && -z $msg_hrpref ]];then 
        msg_subject="$msg_subject ( $strmsg )"
    fi

elif $optd;then 
    :
else
    #todo:  without options, this code will figureout all forecasts from the last month and ones up to 
    #       today's date in the current month. 
    arryyyymmdd=($( s2sv3_nrtdates )) 
    arrdexp1=($( s2sv3_nrtdexp ${arryyyymmdd[@]} ))
fi
    
#todo:  check if dexp exists
arrdexp=($( printf '%s\n' ${arrdexp1[@]} | xargs -i bash -c "[[ -d {} ]] && echo {}" ))
arrdexpsub1=( ${arrdexp[@]} )

#(( ${#arrdexp[@]} == 0 )) && exit

#todo:  get winners and its new ensmble member.
convert2winnerensm_all $ddata
#printf '%s\n' "${!assoc_winensm[@]}" ${assoc_winensm[@]} | pr -t2 | sort >&2

#todo:  get exps which are ready to be cleaned. 
arrfinal=($( filter "${arrdexp[@]}" ))

#todo:  exit out when optb is ture
if $optb;then
    wmessage "Ready:"
    ahand_print ${arrfinal[@]}
    exit
fi


#(( ${#arrfinal[@]} == 0 )) && exit

#todo:  set max screen sessions basd on who runs this script
if [[ -n $RUN_BY_CRON ]] && $RUN_BY_CRON ;then
    limscr=2
elif $blnode;then 
    limscr=10
else
    limscr=1
fi 


msg_wheader
wmessage $( hostname ) 

[[ -n $strmsg ]] && wmessage "                 User Inputs : $strmsg"
#wmessage \@$LINENO
#realpath $fdata
wmessage "         Total number of exp : $( printf '%+5s\n' ${#arrfinal[@]} ) of $( printf '%+5s\n' ${#arrdexp[@]} ) ( maxlim = $( printf '%+5s\n' $limscr ) )"
    
numproc=$( ps aux | awk '{ print $1 }' | grep -w $USER | wc -l )
wmessage " Currently Running processes : $( printf '%+5s\n' $numproc ) ( maxlim = $( printf '%+5s\n' $numproc_maxhere ) )"
wmessage
    
cnt_dexp=0
for dexp in ${arrfinal[@]};do
    
    cnt_dexp=$(( cnt_dexp + 1 ))
    set_rstfcstdate $dexp
    
    #todo:  count number of screen
    numscr=$( screen -ls | grep -i detached | grep ${strscr}_${strexpid}_ | wc -l )

    #todo:  count number of processes
    numproc=$( ps aux | awk '{ print $1 }' | grep -w $USER | wc -l )

    blexecuted=false
    blinitialnote=false
    totsec=0
    totmin=0

    sec0=$( date +%s )
    sec1=$sec0

    while ! $blexecuted;do 
        if (( $numproc < $numproc_maxhere && $numscr < $limscr && $totsec <= $maxsec ));then 

            totsec=0
            blexecuted=true

            wmessage "$( printf "%+3s of %+3s\n" $cnt_dexp ${#arrfinal[@]} ) $dexp"

            #todo:  run scrchild
            if $optl;then
                if (( $writetofile_child == 1 ));then
                    screen -dmS ${strscr}_${strexpid}_$fcstdate$ensm bash -c "./$scrchild -w -l -f $fsub $srcf $dexp >> $ferr 2>&1"  
                    #./$scrchild -w -f $fsub $srcf $dexp >> $ferr 2>&1
                else
                    ./$scrchild -l -f $fsub $srcf $dexp
                fi
            else
                if (( $writetofile_child == 1 ));then
                    screen -dmS ${strscr}_${strexpid}_$fcstdate$ensm bash -c "./$scrchild -w -f $fsub $srcf $dexp >> $ferr 2>&1"  
                    #./$scrchild -w -f $fsub $srcf $dexp >> $ferr 2>&1
                else
                    ./$scrchild -f $fsub $srcf $dexp
                fi
            fi

        else
            sec2=$( date +%s )
            sec_diff=$(( sec2 - sec1 ))
            totsec=$(( sec2 - sec0 ))
            totmin=$( echo "$totsec / 60" | bc )

            #todo:  break out one loop (hence, 2)
            (( $totsec >= $maxsec )) && break 2

            if ! $blinitialnote;then 
                #wmessage "$( date +'%m/%d/%Y %H:%M' )     Running Screen - $( printf '%+2s\n' $numscr ) and Processes - $( printf '%+2s\n' $numproc ) ... will wait for max of $maxmin minutes    "
                blinitialnote=true
            elif (( $sec_diff > 60 ));then
                wmessage "$( date +'%m/%d/%Y %H:%M' )     Running Screen - $( printf '%+2s\n' $numscr ) and Processes - $( printf '%+2s\n' $numproc ) ... waited for $totmin min ( max = $maxmin )"
                sec1=$( date +%s )
            fi
        fi

        numscr=$( screen -ls | grep ${strscr}_ | wc -l )
        numproc=$( ps aux | awk '{ print $1 }' | grep -w $USER | wc -l )

    done 
done

#todo:  wait for the rest of screen sessions to finish. Otherwise, kill them 
blinitialnote=false
totsec=0
totmin=0

sec0=$( date +%s )
sec1=$sec0
        
[[ -z $numscr ]] && numscr=$( screen -ls | grep ${strscr}_ | wc -l )

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
        wmessage "$( date +'%m/%d/%Y %H:%M' )     Running Screen - $( printf '%+2s\n' $numscr ) ... will wait for $maxmin minutes at max"
        blinitialnote=true

    elif (( $sec_diff > 60 ));then
        wmessage "$( date +'%m/%d/%Y %H:%M' )     Running Screen - $( printf '%+2s\n' $numscr ) ... waited for $totmin min"
        sec1=$( date +%s )
    fi
    
    numscr=$( screen -ls | grep -i detached | grep ${strscr}_${strexpid}_ | wc -l )

done

#todo:  delete fsub file if empty
numsubname=$( find $dtmpdata/* -type f -name "$( basename $fsub )" -empty 2>/dev/null | wc -l )
if (( $numsubname > 0 ));then
    wmessage "    $( basename $fsub ) is empty ... deleted"
    rm -f $fsub
fi

#todo:  get "Sub" status exp and write it in fsub. 
#note:  (W)Sub status exp will pass filter above and monitor.sh will also stops at the beginning.
#       Exp with this status will be "fixed" here. 
num_fgetsatus=$( find $dgetstatus/* -type f -name "${fgetstatus}*" | wc -l )

if (( $num_fgetsatus > 0 ));then 
    arrdexp_prob=($( cat $dgetstatus/${fgetstatus}* | grep -E "Sub|WSub" | cut -d' ' -f2 ))
    arrdexp_add=($( printf '%s\n' ${arrdexp[@]} ${arrdexp_prob[@]} | sort -V | uniq -d ))
    #arrdexp_add=($( printf '%s\n' ${arrfinal[@]} ${arrdexp_prob[@]} | sort -V | uniq -u ))
else
    arrdexp_add=()
fi

if (( ${#arrdexp_add[@]} > 0 ));then
wmessage "@$LINENO arrdexp_add = ${#arrdexp_add[@]}"
    printf '%s\n' ${arrdexp_add[@]} | sort -V >> $fsub
    [[ -f $fsub ]] && wmessage "... created a data file for run_submit.sh ... $fsub "
fi

if (( $numscr > 0 ));then
    wmessage 
    wmessage "Running Screens:"
    wmessage "$( screen -ls | grep ${strscr}_${strexpid}_ )"
    wmessage 
fi
wmessage 
wmessage "Time Ended - $(TZ=America/New_York date +'%m/%d/%Y %H:%M' )"

#todo:  send email
[[ -z $eadds ]] && exit

sizef=$( stat --print='%s' $fmessage )
(( $sizef == 0 )) && exit

if $blnode || $RUN_BY_CRON ;then

    [[ -f $ferr ]] && sizeferr=$( stat --print='%s' $ferr )  || sizeferr=0

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

