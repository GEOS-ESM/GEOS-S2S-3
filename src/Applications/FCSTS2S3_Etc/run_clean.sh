#!/usr/bin/env bash
inputcheck() {
    [[ -z  ${arrposarg[@]} ]] && die "an input is missing"
    (( ${#arrposarg[@]} != 1 )) && die "1 input is required"

    local srcme=$( echo ${arrposarg[0]} | cut -d'_' -f2 )
    
    ! $blnode && [[ "$srcme" != "$hstshort" ]] && die "source file is not for this host (select srcme_${hstshort}_* file)"

    ! $optf && ! $optr && ! $optd  && ! $optt && die "opt r, f, d, or t has to be selected"
    $optr && [[ -z $userbegyyyymm || -z $userendyyyymm ]] && die "YYYYMM range is a required input for option r."

    $optr && $optd && die "Cannot specify option r & d at the same time."
    $optr && $optf && die "Cannot specify option r & f at the same time."
    $optr && $optt && die "Cannot specify option r & t at the same time."
    $optd && $optt && die "Cannot specify option d & t at the same time."
    $optf && $optt && die "Cannot specify option f & t at the same time."
    $optd && $optf && die "Cannot specify option d & f at the same time."

    if $optr && [[ -n $userbegyyyymm && -n $userendyyyymm ]];then
        local int1=$( misc_isinteger $userbegyyyymm ) 
        local int2=$( misc_isinteger $userendyyyymm ) 
        
        if (( $int1 > 0 || $int2 > 0 ));then
            die "YYYYMM range have to be numbers delimited by a hyphen"
        fi

        if (( ${#userbegyyyymm} != 6 || ${#userendyyyymm} != 6 ));then
            die "input has to be YYYYMM format"
        fi
    fi

    $optd && (( ${#arrdexp1[@]} == 0 )) && die "opt d requires an input"
    $optf && [[ ! -f $finput ]] && die "opt f requires an existing data file"
    return
}

filter() {
    #description:    find exp that are read to be cleand after 3-month run.
    #note:  this fucntion get a list of exp to clean. But it also checks a few things
    #       about exp:
    #           -   check if all winner runs are completd
    #           -   dexp exists
    #           -   clean_* screen session is running
    #           -   cleaning is already completed
    #           -   cap_restart is = to end yyyymm (regardless of winner or not ) 
    #           -   no jobs are running or on queue

    #local _arrin=( "$@" )
    local _arrdexp=( "$@" )
    local arr=()
    local fcapr=cap_restart
    local fcapric=cap_restartIC
    local _arryyyymm=($( printf '%s\n' ${_arrin[@]} | rev | cut -d'/' -f2 | rev | cut -c1-6 | sort -V | uniq )) 
    local dexp rstensm _yyyymm
    local _arr=()
    
    local cntbug=0

    #todo:  get all running/q'd jobs 
    local arrjobs=($( /u/scicon/tools/bin/qstat -u $USER -W fmt_Queue="-maxw 40" -W o=+Rank0 | sort -V -k4 -t' ' | tr -s '[:space:]' | cut -d' ' -f4 | cut -d'.' -f1 | sort -V | uniq | grep ens ))

    #todo:  check if all winner gcm_run.j runs are completed for expid=fcst
    #if [[ "$strexpid" == "fcst" ]];then
    #    for _yyyymm in ${_arryyyymm[@]};do

    #        #data_getstatus_list_198204
    #        local fdata=$( find $dstatus/* -type f -name "$strflst$_yyyymm" 2>/dev/null | tail -1 )
    #        if [[ -f $fdata ]];then
    #            local numwin=$( grep -w $strwc10 $fdata 2>/dev/null | wc -l )

    #            if (( $numwin == 10 ));then
    #                _arrdexp+=($( printf '%s\n' ${_arrin[@]} | sort -V | grep $_yyyymm ))
    #            fi
    #        fi
    #    done

    #    debug_filter ${_arrdexp[@]}
    #else
    #    _arrdexp=${_arrin[@]}
    #fi

    #todo:  dexp exists
    _arrdexp=($( printf '%s\n' ${_arrdexp[@]} | xargs -i bash -c "[[ -d {} ]] && echo {}" ))

    debug_filter ${_arrdexp[@]}

    #todo:  check clean status
    _arrdexp=($( printf '%s\n' ${_arrdexp[@]} | xargs -i bash -c "[[ ! -f {}/${strscr}_completed ]] && echo {}" ))

    debug_filter ${_arrdexp[@]}
    
    #todo:  check archiving status 
    _arrdexp=($( printf '%s\n' ${_arrdexp[@]} | xargs -i bash -c "[[ -f {}/archive/gcmarch_deloutcompleted ]] && echo {}" ))

    debug_filter ${_arrdexp[@]}

    #todo:  count number of stdout from monitor
    _arrdexp=($( printf '%s\n' ${_arrdexp[@]} | \
        xargs -i bash -c "source func_fcst.sh ; \
        set_rstfcstdate {}; \
        numf=\$( find message/* -type f -name "'"'"message_monitor_${strexpid}_\${fcstdate}\${ensm}_*"'"'" 2>/dev/null | wc -l ) ; \
        (( \$numf == 0 )) && echo {} \
        " ))
    
    debug_filter ${_arrdexp[@]}

    local initcntbug=$cntbug
    for dexp in ${_arrdexp[@]};do

        cntbug=$initcntbug
        set_rstfcstdate $dexp 
        
        local fcst_yyyymm=$( echo $fcstdate | cut -c1-6 )
        #output/getstatus/fcst/data_getstatus_fcst_list_198406
        local _fgetstatus=$ddata_getstatus/data_getstatus_${strexpid}_list_$fcst_yyyymm
        
        #todo:  check if pckwinner script has already executed for $fcst_yyyymm
        #note   this is only for experiments with 15 ensemble members
        if (( ${#arrintens[@]} == 15 ));then 
            local _numfwin=$( find $ddata_pckwinners/* -maxdepth 0 -type f -name "winners_nino3.4_${strexpid}_${fcst_yyyymm}_*" 2>/dev/null | wc -l ) 
            (( $_numfwin != 1 )) && continue
            
            debug_filter
        fi

        #todo:  clean_* screen session is running
        screen -ls | grep ${strscr}_$fcstdate$ensm > /dev/null
        (( $? == 0 )) && continue

        debug_filter

        #todo:  check status
        local _arrstatus_org=( C3 C3m WC10 )
        #local status_out=$( exp_status $dexp )
        #[[ ! "${_arrstatus_org[@]}" =~ *"$status_out"* ]] && : || continue
        if [[ -n $_fgetstatus ]];then 
            local status_out=$( grep -w $dexp $_fgetstatus 2>/dev/null | cut -d' ' -f1 )
            [[ ! "${_arrstatus_org[@]}" =~ *"$status_out"* ]] && : || continue

        else
            continue
        fi

        debug_filter

        #todo:  check run_gcmarch.sh is running
        printf '%s\n' ${arrjobs[@]} | grep A$fcstdate$ensm >/dev/null
        local status_grep=$?
        (( $status_grep == 0 )) && continue

        debug_filter 

        #todo:  check gcm_post.*.j is running
        printf '%s\n' ${arrjobs[@]} | grep P$fcstdate$ensm >/dev/null
        local status_grep=$?
        (( $status_grep == 0 )) && continue

        debug_filter 

        local fcapr=cap_restart
        local fcapend=cap_end
        local _fcapr=$dexp/$fcapr
        local _fcapend=$dexp/$fcapend
        local capr_yyyymmdd=$( cat $_fcapr | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
        local realend_yyyymmdd=$( cat $_fcapend | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )

        local nummonth1=3
        local strxmonth=$( nextXmonths $( date -d $fcstdate +%Y ) $( printf '%01g' $( date -d $fcstdate +%m ) ) 0 $(( nummonth1 + 1 ))  )
        local end_mm=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f1 )
        local end_year=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f2 )
        local end3_yyyymmdd=$end_year$end_mm"01"
        
        local blrerun=$( grep -i "RERUN" $dexp/gcm_run.j | head -1 | rev | cut -d' ' -f1 | rev | tr '[:upper:]' '[:lower:]'  )
        local blwinner=$( exp_checkwinner $dexp $ddata_pckwinners )

        if $blwinner || $blrerun ;then
            local expenddate=$realend_yyyymmdd
        else
            local expenddate=$end3_yyyymmdd
        fi

        #todo:  check if cap_restart is = to end yyyymm (regardless of winner or not ) 
        if $blwinner || $blrerun;then
            (( $expenddate != $capr_yyyymmdd )) && continue
        else
            (( $expenddate > $capr_yyyymmdd )) && continue
        fi

        debug_filter
        
        #local runhere=false
        #if $runhere;then
        #    #todo:  clean ocean perturbation only if YYYYMM is after 199601
        #    local intensm=$( echo $ensm | sed 's#ens##g' )
        #    if (( 5 < $intensm )) ;then 
        #        (( $fcst_yyyymm >= $yyyymm_cleanocn )) && : || continue
        #    else
        #        :
        #    fi
        #    debug_filter
        #fi


        arr+=( $dexp )
    done

    echo ${arr[@]}
}

write_quota() {
    #todo:  write quota 
    local _nbp=$( echo $DFCST | cut -d'/' -f1-3 ) 
    wmessage
    msg_wheader_userdefined 60 "-" "Quota for $( dirname $_nbp ) (as of $( date +%Y%m%d ))" 
    wmessage 
    if (( $writetofile == 1 ));then
        wmessage "$( lfs quota -h -u $USER $_nbp )" 
    else 
        lfs quota -h -u $USER $_nbp
    fi
    wmessage 
    return
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
wmessage \@$LINENO status_email = $status_email
wmessage \@$LINENO blsavefmsg = $blsavefmsg

        if $blsavefmsg;then
            mv $fmessage $dstdout/$fmsg_fname
        elif ! $blsavefmsg;then
            (( $status_email == 0 )) && rm -f $fmessage
        fi
        
        $blrm && msg_newfile $ferr
    fi
}

clean_dir() {
    [[ -n $flock && -f $flock ]] && rm -f $flock
    return
}

#================================================================================
#                                     Begin
#================================================================================
#starthere
hst=$( hostname )
blnode=false
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

if [[ ! $cdir/func_fcst.sh ]];then 
    echo "$cdir/func_fcst.sh does not exist";exit
else
    source $cdir/func_fcst.sh
fi
trap clean_dir EXIT

hstshort=$( get_host )
writetofile=0
rundebug=0
runclean=0
optb=false
optd=false
optt=false
optf=false
optr=false
optusesstlst=false
#================================================================================
#                                     Usage
#================================================================================
#ref: https://goo.gl/BUBwPV
usage="$(basename $0) -- this is a program to:
        clean forecast experiment *after* exp completes 10-month run.
        Stdout for each dexp is written in message/message_${strscr}_* file.
        Select option c to clean exp dir. By default, this script check 
        through the current status of experiment dir.  

        Usage: ./$(basename "$0") [-bch] [-d exp dir] [-f data file] [-r YYYYMM-YYYYMM] srcme_file

        input:        
            A source file, which set various vars for a set of runs (i.e. srcme_pfe_*)

        options:
            -b                          run with a debug mode (this will not execute ${strscr}.sh)
            -c                          clean exp dir
            -d                          a full path to an exp dir
            -f                          a data filename with a list of *DATES* (format: YYYYMMDD)
            -r                          YYYYMM and YYYYMM range (format: YYYYMM-YYYYMM) 
            -t                          specify forecast date ( format: YYYYMMDD )
                --use-sstlst=[BOOLEAN,  true to retrieve wftmsz_flist from \$DARC/\${reg}/sst or a 
                                 FILE]    retrieved wftmsz_flis file name. 
                                          *Use it to confirm sst file is archived
            -h                          show this help text

        Note:
            1. If you want to force clean dir, create a file with full paths of exp dir:
                $( echo $cdir/data/$strscr/\$strexpid/data_${strscr}_\$strexpid_exceptions)
"
file=
verbose=0
cnt=0
while :; do
    case $1 in
                   -d )  [[ "$2" ]] && arrdexp1=( $2 ) && optd=true    && shift;;
                   -f )  [[ "$2" ]] && finput=$2       && optf=true    && shift;;
                   -r )  [[ "$2" ]] && optr=true       && userinput=$2 && shift;
                         userbegyyyymm=$( echo $userinput | cut -d'-' -f1 );
                         userendyyyymm=$( echo $userinput | cut -d'-' -f2 );;
                   -t )  [[ "$2" ]] && optt=true; yyyymmdd=$2          && shift;;
                   -b )  optb=true && rundebug=1           ;;
      --use-sstlst=?* )  optusesstlst=true && userinput_usesstlst=${1#*=};;
        --use-sstlst= )  die "--use-sstlst requires a non-empty option argument.";; 
                   -w )  writetofile=1;; 
                   -c )  runclean=1;;
        -h|-\?|--help )  echo "$usage";exit;;           # Display a usage synopsis.
                   -- )  shift;break;;                  # End of all options.
                  -?* )  printf 'WARN: Unknown option (ignored): %s\n' "$1" >&2;;
                    * )  cnt=$(( cnt + 1 )); break ;;   # Default case: No more options, so break out of the loop.
    esac
    shift
    cnt=$(( cnt + 1 ))
done


#OPTIND=1
#while getopts 'bhcd:f:r:wt:' option; do
#    case "$option" in
#        d)  if $optf || $optr ;then die "Cannot specify option d when specifying option r or f";fi
#            optd=true; arrdexp1=( $OPTARG );;
#        f)  if $optr || $optd ;then die "Cannot specify option f when specifying option r or d";fi
#            optf=true; finput=$OPTARG ;;
#        r)  if $optd || $optf ; then die "Cannot specify option r when specifying option f or d";fi
#            optr=true; 
#            userbegyyyymm=$( echo $OPTARG | cut -d'-' -f1 );
#            userendyyyymm=$( echo $OPTARG | cut -d'-' -f2 );;
#        t)  $optf && die "Cannot specify option t & f at the same time.";
#            $optr && die "Cannot specify option t & r at the same time.";
#            $optd && die "Cannot specify option t & d at the same time.";
#            optt=true; yyyymmdd=$OPTARG;;
#        b)  optb=true;rundebug=1;; 
#        w)  writetofile=1;;
#        c)  runclean=1;;
#        h)  echo "$usage"; echo "$note"; exit 0;;
#        \?) echo "Invalid option: -$OPTARG" >&2
#            exit 1;;
#        :)  echo "Option -$OPTARG requires an argument." >&2
#            exit 1;;
#    esac
#done

! $optr && ! $optd && ! $optt && blnrt=true || blnrt=false

##todo:  get positional inputs. 
#shift $((OPTIND-1))
#[[ "${1:-}" = "--" ]] && shift

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
[[ -z $DFCST || ! -d $DFCST  ]] && die "DFCST is undefined or does not exist"
[[ -z $DARCH ]] && die "DARCH is undefined or does not exist"
#================================================================================
#                                 Set Variables
#================================================================================
#mid
cdate=$( date +%Y%m%d%H%M )
[[ -n $rstexpid ]] && strexpid=$strexpid$rstexpid
dmess=$cdir/message
#  ddata=$cdir/data/${strscr}/$strdout
dstdout=$cdir/stdout/$strscr/$strdout
dstatus=$cdir/output/getstatus/$strdout
ddata_pckwinners=$cdir/output/pckwinners/$strdout
ddata_getstatus=$cdir/output/getstatus/$strdout

dfcst=$DFCST
ferr=$dmess/stderr_$strscr
fmessage=$dmess/message_$strscr
fdexcept=$cdir/data/$strscr/$strdout/data_${strscr}_${strexpid}_exceptions
farch=archive/run_gcmarch.sh
flstarc=$cdir/${strscr}_arc_sst_wftmsz_flist

strflst=data_getstatus_${strexpid}_list_
strwc10=WC10
scrchild=${strscr}.sh
arrensm_ocn=$( printf "ens%s " $( seq 6 15 )  )
strensm_ocn=$( printf '%s\n' ${arrensm_ocn[@]}  | sed 's#[ \s]*$##g' | sed 's# #|#g' )

#note:  06/29/2022 all ocean perturbation prior to 199601 needs to be re-run, and 
#                   any after 199601 can be cleaned.
#       03/02/2022 This var is no longer requied for v3 hindcast run attempt2. This
#                  was used for attempt1.
#yyyymm_cleanocn=199601
msg_subject="${hstshort}.${strscr}: $strexpid"
$optb     && msg_subject="${hstshort}.${strscr}: DEBUG mode - $strexpid"
(( $runclean == 0 )) && msg_subject="${hstshort}.${strscr}: $strexpid NO CLEANING"


numsec=15
maxsec=900
maxmin=$( echo "$maxsec/60" | bc )

#note:  process limits
numproc_max=$( ulimit -u )
numsubtract=100
numproc_maxhere=$(( numproc_max - numsubtract ))

#todo:  set max screen sessions basd on who runs this script
if (( $runclean == 1 ));then
    if [[ -n $RUN_BY_CRON ]] && $RUN_BY_CRON ;then
        limscr=2
    elif $blnode;then 
        limscr=10
    else
        limscr=5
    fi
else
    limscr=40
fi

#note:  save fmessage in dstdout. This was created 
#       in case sending fmessage doesn't work 
blsavefmsg=true

#[[ "$dfcst" =~ "$USER" ]] && bldoquota=true || bldoquota=false
bldoquota=true

RUNCHILD=1

feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

[[ ! -d $dmess    ]] && mkdir -p $dmess
#[[ ! -d $ddata    ]] && mkdir -p $ddata
[[ ! -d $dstdout  ]] && mkdir -p $dstdout
[[ ! -f $scrchild ]] && die "$scrchild does not exist"

export RUNCHILD cdir
#================================================================================
#                                  Main Process
#================================================================================
#letsbegin
if $optusesstlst;then 
    if [[ $userinput_usesstlst == "true" ]] ;then 
        [[ -f $flstarc ]] && rm -f $flstarc
        get_fwftmsz_nosup $DARCH/$reg/sst $flstarc
        [[ ! -f $flstarc ]] && optusesstlst=false

    elif [[ -f $userinput_usesstlst ]];then 
        flstarc=$userinput_usesstlst
    else
        #optusesstlst=false
        exit
    fi
fi

if $optt;then
    for intens in ${arrintens[@]};do 
        arrdexp1+=( $DFCST/$yyyymmdd/ens$intens )
    done
    strmsg="$yyyymmdd"
    fmsg_fname=message_${strscr}_userinput__${yyyymmdd}_${cdate} 
    msg_subject="$msg_subject ( $strmsg )"

elif $optf;then
    arrdexp1=($( misc_readfbyline $finput )) 
    arrdexp1=($( printf "$DFCST/%s\n" ${arrdexp1[@]} | xargs -i printf "{}/ens%s\n" ${arrintens[@]} )) 
    strmsg="$finput"
    fmsg_fname=message_${strscr}_userinput__${finput}_${cdate}
    msg_subject="$msg_subject ($( basename $finput ))"

elif $optr ;then
    get_beg_and_end $userbegyyyymm $userendyyyymm
    arrdexp1=($( get_dexp $dfcst $begyyyymm $endyyyymm ${arrintens[@]} ))
    strmsg="$userbegyyyymm - $userendyyyymm"
    fmsg_fname=message_${strscr}_userinput__${userbegyyyymm}_${userendyyyymm}_${cdate}    
    msg_subject="$msg_subject ( $strmsg )"
fi

if $optb;then 
    msg_wheader
    wmessage $hst
    [[ -n $strmsg ]] && wmessage "User Inputs : $strmsg"
    wmessage
fi

#todo:  place exception exp into arrfinal to avoid filter. 
if [[ -n $fdexcept && -s $fdexcept ]];then
    arrdexp_exception=($( misc_readfbyline $fdexcept))
    arrdexp_dup=($( printf '%s\n' ${arrdexp1[@]} ${arrdexp_exception[@]} | sort -V | uniq -d ))
    arrdexp=($( printf '%s\n' ${arrdexp1[@]} ${arrdexp_dup[@]} | sort -V | uniq  ))
    arrdexp=($( printf '%s\n' ${arrdexp[@]} | xargs -i bash -c "[[ ! -f {}/${strscr}_completed ]] && echo {}" ))
    arrdexp=($( printf '%s\n' ${arrdexp[@]} | xargs -i bash -c "[[ -d {} ]] && echo {}" ))
    arrfinal=($( filter "${arrdexp1[@]}" ))
else
    arrfinal1=($( filter "${arrdexp1[@]}" ))
    arrfinal=( ${arrfinal1[@]} )
fi

(( ${#arrfinal[@]} == 0 )) && exit

#todo:  stops here if debug mode option (b) is selected
if $optb;then 
    wmessage
    ahand_print ${arrfinal[@]}
    [[ -f $fmessage ]] && sendmsg
    exit
fi

(( ${#arrfinal[@]} == 0 )) && exit


msg_wheader
wmessage $hst

#note:  this is necessary when this script is executed on a specific node other than pfe
if (( $runclean == 1 ));then
    $bldoquota && write_quota_fcst
    write_numcleaned_dexp
    msg_wheader_userdefined 60 "-" "Start Process" 
    [[ -n $strmsg ]] && wmessage "                 User Inputs : $strmsg"
    wmessage "         Total number of exp : $( printf '%+5s\n' ${#arrfinal[@]} ) ( maxlim = $( printf '%+5s\n' $limscr ) )"
else
    [[ -n $strmsg ]] && wmessage "                 User Inputs : $strmsg"
    wmessage "         Total number of exp (NO CLEANING): $( printf '%+5s\n' ${#arrfinal[@]} ) ( maxlim = $( printf '%+5s\n' $limscr ) )"
fi
    
numproc=$( ps aux | awk '{ print $1 }' | grep -w $USER | wc -l )
wmessage " Currently Running processes : $( printf '%+5s\n' $numproc ) ( maxlim = $( printf '%+5s\n' $numproc_maxhere ) )"
wmessage
   
cnt=0
for dexp in ${arrfinal[@]};do
    
    cnt=$(( cnt + 1 ))
    
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

            wmessage "$( printf "%+3s of %+3s\n" $cnt ${#arrfinal[@]} ) $dexp    "

            if (( $runclean == 1 ));then
                screen -dmS ${strscr}_${strexpid}_$fcstdate$ensm bash -c "./$scrchild -c -w -d $dexp $srcf >> $ferr 2>&1"  
                #./clean.sh -c -d $dexp $srcf 
            else
                screen -dmS ${strscr}_${strexpid}_$fcstdate$ensm bash -c "./$scrchild -w -d $dexp $srcf >> $ferr 2>&1"  
                #./clean.sh -d $dexp $srcf 
            fi
            
        else
            sec2=$( date +%s )
            sec_diff=$(( sec2 - sec1 ))
            totsec=$(( sec2 - sec0 ))
            totmin=$( echo "$totsec / 60" | bc )

            #todo:  break out for both for loop (hence, 2)
            (( $totsec >= $maxsec )) && break 2 

            if ! $blinitialnote;then 
                #wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screens are running and $( printf '%+2s\n' $numproc ) processes are running ... will wait for max of $maxmin minutes    "
                blinitialnote=true
            elif (( $sec_diff > 60 ));then
                wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screens are running and $( printf '%+2s\n' $numproc ) processes are running ... waited for $totmin min ( max = $maxmin )"
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
        wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screens - running ... will wait for max of $maxmin minutes"
        blinitialnote=true

    elif (( $sec_diff > 60 ));then
        wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screens - running ... waited for $totmin min"
        sec1=$( date +%s )
    fi
    
    numscr=$( screen -ls | grep -i detached | grep ${strscr}_${strexpid}_ | wc -l )

done


if $bldoquota;then
    wmessage
    wmessage $( date +"%D %T" )
    write_quota_fcst
    write_numcleaned_dexp
fi

numscr_running=$( screen -ls | grep ${strscr}_${strexpid}_ | wc -l )
if (( $numscr ));then 
    wmessage 
    wmessage 
    wmessage "Running Screens:"
    wmessage "$( screen -ls | grep ${strscr}_${strexpid}_ )"
    wmessage 
fi
wmessage 
wmessage "Time Ended - $(TZ=America/New_York date +'%m/%d/%Y %H:%M' )"

[[ -f $flstarc ]] && rm -f $flstarc

#todo:  send email
[[ -f $fmessage ]] && sendmsg



exit

