#!/usr/bin/env bash

inputcheck() {

    (( ${#arrposarg[@]} != 2 )) && die "only two inputs are required"

    local int=$( misc_isinteger $runqsub )
    [[ -z $runqsub ]] && die "one argument is requied"
    (( $int != 0 )) && die "an input argument has to be 0 or 1"
    
    if (( $runqsub == 0 )) || (( $runqsub == 1 ));then
        :
    else
        die "an input argument has to be 0 or 1"
    fi
    
    $optr && $optd && die "Cannot specify option r & d at the same time."
    $optr && $optf && die "Cannot specify option r & f at the same time."
    $optr && $optt && die "Cannot specify option r & t at the same time."
    $optd && $optt && die "Cannot specify option d & t at the same time."
    $optf && $optt && die "Cannot specify option f & t at the same time."
    $optd && $optf && die "Cannot specify option d & f at the same time."

    $optb && $optredo && die "Cannot specify option b & redo at the same time."

    #! $optr && ! $optt && ! $optd && ! $optf && die "opt d, r, t, or f are required"
    $optr && [[ -z $userbegyyyymm || -z $userendyyyymm ]] && die "YYYYMM range is a required input for option r"

    return
}

filter() {
    local _arrinput=( "$@" )
    local arr _arrinput _input 

    #todo:  check if dexp exists
    _arrinput=($( printf '%s\n' ${_arrinput[@]} | xargs -i bash -c 'if [[ ! -d {} ]];then echo {};fi' ))

    debug_filter ${_arrinput[@]}

    #todo:  check if gcmsetup marker exists
    _arrinput=($( printf '%s\n' ${_arrinput[@]} | xargs -i bash -c 'if [[ ! -f {}/$fmark ]];then echo {};fi' ))

    debug_filter ${_arrinput[@]}

    cntbug_init=$cntbug

    #todo:  check if rst dir is ready
    for _input in ${_arrinput[@]};do
        local cntbug=$cntbug_init
        #local fcst_yyyymmdd=$( echo $_input | rev | cut -d'/' -f2 | rev | xargs -i date -d {}-1days +%Y%m%d )
        local fcst_yyyymmdd=$( echo $_input | rev | cut -d'/' -f2 | rev  )
        local icyyyymmdd=$( date -d ${fcst_yyyymmdd}-1days +%Y%m%d )
        local ensn=$( echo $_input | rev | cut -d'/' -f1 | rev )
        local dthisrst=$DRST/$icyyyymmdd/$ensn
        [[ -d $dthisrst ]] && _input=$_input || continue

        debug_filter $_input

        local frstatm=$(find $dthisrst/* -not -type d -name "*_rst"  | wc -l )
        local frstocn=$(find $dthisrst/* -maxdepth 1 -mindepth 1 -not -type d -name "ocean_*" | wc -l )

        if (( $frstatm == $numfrstatm )) && (( $frstocn == $numfrstocn ));then
            :
        else
            arrnopass+=( $_input )
            continue
        fi

        debug_filter $_input

        arr+=( $_input )
    done

    if $optr ;then 
        #todo:  get the first icyyyymm and select experimnets with the icyyyymm
        local _icyyyymm=($( printf '%s\n' ${arr[@]} | rev | cut -d'/' -f2 | rev   | sort -V | uniq | cut -c1-6 | sort -V | uniq | head -1 )) 
        local       arr=($( printf '%s\n' ${arr[@]} | grep $_icyyyymm 2>/dev/null | sort -V | uniq ))
    fi

    echo ${arr[@]}
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
    blnode=true

elif [[ "${hstname:0:4}" == borg || "${hstname:0:4}" == warp ]];then 
    #note:  this is for host other than pfe @ NCCS which has a name starting with 'borg' or 'warp'
    blnode=true
else 
    exit
fi

strscr=$(basename "$0" | cut -d'_' -f2 | cut -d'.' -f1 )
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cdir=$SCRIPTPATH

[[ -z $cdir || ! -d $cdir ]] &&  echo "cdir (parent directory of this script) is undefined"  && exit

cd $cdir

flock=$cdir/${strscr}.lock
ftmp=$cdir/stdout/rundatetime/tmp_$strscr
[[ ! -f $ftmp ]] && install -D /dev/null $ftmp

#todo:  check tmp file size and create new if it is larger than 5kb
stmp=$( find $ftmp -printf "%s\n" )
(( $stmp > 5000 )) && rm -f $ftmp && touch $ftmp

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

optb=false
optd=false
optf=false
optt=false
optr=false
rundebug=0
writetofile=0
childwritetofile=1
optredo=false

#default values
userbegyyyymm=198112
userendyyyymm=201012
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        setup max of 10 forecast directories at a time for hindcast. 
        *When this script is executed without option r, t, or d, it will 
        setupt directories for near-realtime forecast.*
        
        Usage: ./$(basename "$0") [-hbw] [-q qid] [ -f file ] [-r YYYYMM-YYYYMM] [-t YYYYMMDD/ensM] [-d YYYYMMDD] [--redo file ] srcme_file [1 or 0]

        input:
            A source file, which set various vars for a set of runs (i.e. srcme_pfe_fcst)
            Enter 1 to submit a job after exp dir is ready; 0 for setting up exp dir only

        options:
            -b          run with a debug mode ( this will not execute ${strscr}.sh )
            -d          specify forecast date ( format: YYYYMMDD )
            -t          specify forecast date & ens member ( format: YYYYMMDD/ensM )
            -f          a file with a list of dexp full path
            -r          YYYYMM and YYYYMM range ( format: YYYYMM-YYYYMM ) 
            -q          queue id (default: normal for pfe; none for discover)  
                --redo  delete existing exp dir and set it up again
                        *Input file has to have a list of exp dir with full paths
            -h          show this help text
            -w          write stdout/err in a file
"
file=
verbose=0
cnt=0
while :; do
    case $1 in
                   -b )  optb=true && rundebug=1           ;;
                   -q )  [[ "$2" ]] && userinput_qid=$2                && shift;;
                   -d )  [[ "$2" ]] && yyyymmdd=$2     && optd=true    && shift;;
                   -t )  [[ "$2" ]] && yyyymmddensm=$2 && optt=true    && shift;;
                   -f )  [[ "$2" ]] && userfdata=$2    && optf=true    && shift;;
                   -r )  [[ "$2" ]] && optr=true       && userinput=$2 && shift;
                         userbegyyyymm=$( echo $userinput | cut -d'-' -f1 );
                         userendyyyymm=$( echo $userinput | cut -d'-' -f2 );;
               --redo )  [[ "$2" ]] && optredo=true && userinput=$2 && shift;;
                   -h )  echo "$usage"; exit 0;;
                   -w )  writetofile=1;; 
        -h|-\?|--help )  echo "$usage";exit;;           # Display a usage synopsis.
                   -- )  shift;break;;                  # End of all options.
                  -?* )  printf 'WARN: Unknown option (ignored): %s\n' "$1" >&2;;
                    * )  cnt=$(( cnt + 1 )); break ;;   # Default case: No more options, so break out of the loop.
    esac
    shift
    cnt=$(( cnt + 1 ))
done

! $optr && ! $optd && ! $optt && blnrt=true || blnrt=false

arrposarg=( "$@" )
#================================================================================
#                               Check User Inputs
#================================================================================
(( ${#arrposarg[@]} == 0 )) && echo "$usage" && exit

i=0
srcf=${arrposarg[$i]};i=$(( i + 1 ))
runqsub=${arrposarg[$i]};i=$(( i + 1 ))
source $srcf
inputcheck
#================================================================================
#                             Set Host Specific Vars
#================================================================================
hstshort=$( get_host )
if [[ "$hstshort" == "dis" ]];then
    cmd_gjob=cmd_gjob_nccs
elif [[ "$hstshort" == "pfe" ]];then
    qid_default=normal
    cmd_gjob=cmd_gjob_nas
fi

[[   -z $strexpid          ]] && die "strexpid is undefined"
[[   -z $DFCST || -z $DRST ]] && die "DFCST and/or DRST are undefined"
[[ ! -d $DRST   ]] && die "DRST does not exist"
[[   -z $DARCH  ]] && die "DARCH is undefined"
#================================================================================
#                                 Set Variables
#================================================================================
#mid
[[ -n $rstexpid ]] && strexpid=$strexpid$rstexpid

dmess=$cdir/message
dtmpdata=$cdir/data/submit
dstdout=$cdir/stdout/$strscr/$strdout
ferr=$dmess/stderr_${strscr}_$strexpid
fmark=${strscr}_setupready
fmessage=$dmess/message_${strscr}_${strexpid}

#note:  screen limit
#limscr=10
#todo:  set max screen sessions basd on who runs this script
if [[ -n $RUN_BY_CRON ]] && $RUN_BY_CRON ;then
    limscr=2
elif $blnode;then 
    limscr=10
else
    limscr=5
fi 

#note:  numsec is wailt time in sec and maxsec is total wait time limit
numsec=60
maxsec=1200
maxmin=$( echo "$maxsec / 60" | bc  )

secsleep=15

#note:  process limits
numproc_max=$( ulimit -u )
numsubtract=50
numproc_maxhere=$(( numproc_max - numsubtract ))

#note:  limit for total ready-exp. 
maxreadyexp=5

#note:  total restart file # for each atm and ocn
numfrstatm=22
numfrstocn=11

#note:  need to change strf var. data file has to be created based on qid. 
strf=data_submit_${strexpid}_${strscr}_
cdate=$( date +%Y%m%d_%H%M )

thisscr=gcmsetup.sh

msg_subject="$hstshort.$strscr: $strexpid" 

feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

[[ ! -f $fmessage ]] && touch $fmessage
[[ ! -f $thisscr  ]] && die $thisscr does not exist
[[ ! -d $dmess    ]] && mkdir -p $dmess
[[ ! -d $dtmpdata ]] && mkdir -p $dtmpdata
[[ ! -d $dstdout  ]] && mkdir -p $dstdout
[[ ! -f $ferr     ]] && touch $ferr

RUNCHILD=1

#todo:  count a number of ready-exp in data_subtmit_* files
#note:  this only applies when data_submit_* is going to be created (input for submit is 1 )
#if (( $runqsub == 0 ));then
#    numf=$( find $dtmpdata/* -type f 2>/dev/null | wc -l )
#    if (( $numf > 0 ));then
#        numreadyexp=$( cat $dtmpdata/data_submit_* | grep -v archive | sort | uniq | sort -V | wc -l )
#        (( $numreadyexp > $maxreadyexp )) && exit
#    fi
#fi

#todo:  set qid for gcm_run.j
if [[ "$hstshort" == "pfe" ]];then 
    if [[ -z $qid && -n $userinput_qid ]];then
        qid=$userinput_qid
    
    elif [[ -z $qid && -z $userinput_qid ]];then
        
        #todo:  find available nodes within next $inputhour 
        inputhour_end=8
        inputhour_beg=$inputhour_end
        arrqidrun=($( res_Rqid ))
        arrqidend=($( res_qidexpire $inputhour_end ))
        arrqidbeg=($( res_qidstart  $inputhour_beg ))
        arrqidavail_xhrs1=($( printf '%s\n' ${arrqidrun[@]} ${arrqidend[@]} ${arrqidbeg[@]}  | sort | uniq -u )) 
   
        #todo:  divide available nodes into rome or non-rome nodes
        for qidavail_xhrs in ${arrqidavail_xhrs1[@]};do 
            thismname=$( res_mname $qidavail_xhrs )

            if [[ "$thismname" == "rom_ait" ]];then 
                arrqidavail_xhrs_rome+=( $qidavail_xhrs )
            else
                arrqidavail_xhrs_nonrome+=( $qidavail_xhrs )
            fi
        done 
    
        if [[ -n $mname ]];then 
            if [[ "$mname" == "rom_ait" ]];then 
                arrqidavail_xhrs+=( ${arrqidavail_xhrs_rome[@]} )
            else
                arrqidavail_xhrs=( ${arrqidavail_xhrs_nonrome[@]} )
            fi
        else
            strctag=$( echo $ctag | grep -i rome )
            if [[ $strctag == *"ROME"* ]];then 
                arrqidavail_xhrs+=( ${arrqidavail_xhrs_rome[@]} ) 
            else
                arrqidavail_xhrs=( ${arrqidavail_xhrs_nonrome[@]} )
            fi
        fi
    fi

    if [[ -z $qid && -z $userinput_qid && -z ${arrqidavail_xhrs[@]} ]];then 
        qid=$qid_default
    fi

    if (( ${#arrqidavail_xhrs[@]} > 0 ));then 
        #note:  maxreadyexp is a number of exp which will be ready to be submitted
        maxreadyexp=$( echo "$maxreadyexp * ${#arrqidavail_xhrs[@]}" | bc -l )
    fi
    
    #todo:  check if there are any reservations for run_gcmarch.sh
    arrresqid=($( res_qid_all ))
    
    for resqid in ${arrresqid[@]};do
        resmname=$( res_mname $resqid )
    
        [[ "$resmname" == "ivy" ]] && arrqid_ivy+=( $resqid )
    done
    (( ${#arrqid_ivy[@]} > 0 )) && blres_arch=true || blres_arch=false

elif [[ $hstshort == dis ]];then 
    qid=()
fi

cnt_ivy=0

export RUNCHILD
#================================================================================
#                                  Main Process
#================================================================================
#letsbegin
if $optd || $optt;then

    if $optd;then
        arrdexp=($( printf "$DFCST/$yyyymmdd/ens%s\n" ${arrintens[@]} ))
        msg_subject="$msg_subject ( $yyyymmdd )"
    
    elif $optt;then
        arrdexp=( $DFCST/$yyyymmddensm )
        msg_subject="$msg_subject ( $yyyymmddensm )"
    fi 

    #todo:  filter and get exps that are aready to be setup.
    arrfinal=($( filter ${arrdexp[@]} ))

elif $optf; then
    #arrdate=($( misc_readfbyline $userfdata ))
    #arrdexp=($( printf "$DFCST/%s\n" ${arrdate[@]} | xargs -i printf '{}/ens%s\n' ${arrintens[@]} | sort -V )) 

    arrdexp=($( misc_readfbyline $userfdata ))

    #todo:  filter and get exps that are aready to be setup.
    arrfinal=($( filter ${arrdexp[@]} ))
    msg_subject="$msg_subject ($( basename $( realpath $userfdata )) )"

elif $optr; then

    #todo:  get fcst dates
    yyyymm=$userbegyyyymm
    while (( $yyyymm <= $userendyyyymm ));do
        arricyyyymm+=( $yyyymm )
        yyyy=$( echo $yyyymm | cut -c1-4 )
        mm=$( echo $yyyymm | cut -c5- )
        yyyymm=$( fcal_nextmonth $mm $yyyy )
    done
 
    #todo:  get dexp path
    for yyyymm in ${arricyyyymm[@]};do
        get_beg_and_end $yyyymm $yyyymm
        #arrdexp+=($( get_dexp $DFCST $begyyyymm $userendyyyymm ${arrintens[@]} ))
        arrdexp+=($( get_dexp $DFCST $begyyyymm $endyyyymm ${arrintens[@]} ))
    done
    
    arrfinal=($( filter ${arrdexp[@]} ))

    #note:  the following code is to setup one month (icyyyymm) at a time in an order of month
    #       to avoid experiments running in different months. 
    #todo:  find the rest of exp to setup for icyyyymm
    numf=$( find $dtmpdata/* -type f -name "data_submit_$strexpid_*" 2>/dev/null | wc -l )

    if (( $numf > 0 ));then 
        numreadyexp=$( cat $dtmpdata/data_submit_${strexpid}* 2>/dev/null | grep -v archive | sort | uniq | sort -V | wc -l ) 
        arricyyyymm_ready=($( cat $dtmpdata/* 2>/dev/null | grep $DFCST | rev | cut -d'/' -f2 | rev | cut -c1-6 | sort -V | uniq ))
    else
        numreadyexp=0
        arricyyyymm_ready=()
    fi

    #todo:  find icyyyymm_ready experiments which have NOT been created. 
    arrtmp=()
    for icyyyymm_ready in ${arricyyyymm_ready[@]};do
        arrtmp+=($( printf '%s\n' ${arrfinal[@]} | grep $DFCST/$icyyyymm_ready | sort -V | uniq ))
    done 

#arrtmp=( ${arrfinal[@]} ) 
    if (( ${#arrtmp[@]} > 0 ));then
        arrfinal=( ${arrtmp[@]} ) 
    elif (( ${#arrtmp[@]} == 0 )) && (( $numreadyexp > $maxreadyexp ));then 
        if [[ -n $RUN_BY_CRON ]] && $RUN_BY_CRON ;then
            #todo:  when this script is executed by crontab, stop here
            #wmessage "Max # ($maxreadyexp) of experiments is ready. Exit"
            exit
        elif $blnode;then
            exit
        else
            #arrfinal=($( printf '%s\n' ${arrfinal[@]} | sort -V | uniq )) 
            exit
        fi
    fi
    
    
    strmsg="$userbegyyyymm - $userendyyyymm"
    msg_subject="$msg_subject ( $strmsg )"

elif $optredo;then 
    $optb && optb=false
    arrdexp=($( misc_readfbyline $userinput ))
    arrdexp=($( printf '%s\n' ${arrdexp[@]} | xargs -i bash -c "[[ -d {} ]] && echo {}" ))
    arrjob_run=($( $cmd_gjob | grep $USER | cut -d' ' -f4 | cut -d'.' -f1 | grep ens | cut -c2- | sort -V | uniq | sed 's#ens#/ens#g' | xargs -i echo $DFCST/{} ))
    arrjob_run+=($( printf '%s\n' ${arrdexp[@]} | xargs -i bash -c "[[ -f {}/archive/gcmarch.lock ]] && echo {}" ))
    strjob_run=$( echo ${arrjob_run[@]} | sed 's# #|#g' )
    arrdexp=($( printf '%s\n' ${arrdexp[@]} | grep -vE "$strjob_run" ))

    arrdexp1=()
    for dexp in ${arrdexp[@]};do 
        if [[ -d $dexp ]];then 
            rm -rf $dexp
            (( $? == 0 )) && arrdexp1+=( $dexp ) 
        fi
    done 
    
    arrfinal=( ${arrdexp1[@]} )

    #arrfinal=($( filter ${arrdexp1[@]} ))
    msg_subject="$msg_subject ($( basename $( realpath $userinput )) )"

elif $blnrt;then
    #todo:  without options, this code will figureout all forecasts from the last month and ones up to 
    #       today's date in the current month. 
    arryyyymmdd=($( s2sv3_nrtdates )) 
    arrdexp=($( s2sv3_nrtdexp ${arryyyymmdd[@]} ))
    arrfinal=($( filter ${arrdexp[@]} ))

else
    die "\@$LINENO I am in else"
fi


#todo: with debug mode, script stops here. 
if $optb;then
    wmessage "Ready:"
    ahand_print ${arrfinal[@]}
    exit
fi

(( ${#arrfinal[@]} == 0 )) && exit

msg_wheader 
wmessage $( hostname ) 
[[ -n $strmsg ]] && wmessage "                 User Inputs : $strmsg"
wmessage "         Total number of exp : $( printf '%+5s\n' ${#arrfinal[@]} ) ( Screen Max Limit = $( printf '%+5s\n' $limscr ) )"
wmessage

cnt_dexp=0
arrdexp_ready=()
for input in ${arrfinal[@]};do

    cnt_dexp=$(( cnt_dexp + 1 ))
    
    fcstdate=$( echo $input | rev | cut -d'/' -f2 | rev )
    ensm=$( echo $input | rev | cut -d'/' -f1 | rev )
    thismessage=$cdir/message/message_${strscr}_${strexpid}_${fcstdate}$ensm
    [[ -f $thismessage ]] && rm -f $thismessage

    #todo:  select qid when there is a reservation(s) and if qid is not defined
    if [[ "$hstshort" == "pfe" ]];then
        if (( ${#arrqidavail_xhrs[@]} > 0 )) && [[ -z $qid ]];then
            if (( ${#arrqidavail_xhrs[@]} == 1 ));then
                qid=${arrqidavail_xhrs[0]}
            else
                #item=$( shuf -i0-1 -n1 )
                item=$( shuf -i0-$(( ${#arrqidavail_xhrs[@]} -1 )) -n1 )
                qid=${arrqidavail_xhrs[$item]}
            fi
        fi

        #todo:  select qid for run_gcmarch.sh
        if $blres_arch;then
            (( $cnt_ivy >= ${#arrqid_ivy[@]} )) && cnt_ivy=0
            qid_ivy=${arrqid_ivy[$cnt_ivy]}
            cnt_ivy=$(( cnt_ivy + 1 )) 
        fi
    elif [[ "$hstshort" == "dis" ]];then
        :
    fi

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

        if (( $numscr < $limscr && $numproc < $numproc_maxhere && $totsec <= $maxsec ));then 

            totsec=0
            blexecuted=true
            blinitexecute=true
            
            if (( $runqsub == 1 ));then
                wmessage "$( printf "%+3s of %+3s\n" $cnt_dexp ${#arrfinal[@]} )  create & qsub $input ... $qid"
            elif (( $runqsub == 0 ));then
                wmessage "$( printf "%+3s of %+3s\n" $cnt_dexp ${#arrfinal[@]} )  create $input ... $qid"
            fi

            arrdexp_ready+=( $input )
            if $blres_arch && [[ -n $qid_ivy ]] ;then
                screen -dmS ${strscr}_${strexpid}_$fcstdate$ensm bash -c \
                    "./$thisscr -q $qid -a $qid_ivy -m $thismessage $srcf $runqsub $input >> $thismessage 2>&1"  

            elif [[ -n $qid ]];then  
                #./${thisscr} -q $qid $srcf $runqsub $input 
                screen -dmS ${strscr}_${strexpid}_$fcstdate$ensm bash -c \
                    "./$thisscr -q $qid -m $thismessage $srcf $runqsub $input >> $thismessage 2>&1"  

            elif [[ -z $qid ]];then  
                #./${thisscr} -q $qid $srcf $runqsub $input 
                screen -dmS ${strscr}_${strexpid}_$fcstdate$ensm bash -c \
                    "./$thisscr -m $thismessage $srcf $runqsub $input >> $thismessage 2>&1"  
            fi

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
                wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screen and $( printf '%+2s\n' $numproc ) processes are running " #... waited for $totmin min"
                sleep 1m
                sec0=$( date +%s )
                sec1=$sec0

            elif (( $sec_diff > 60 ));then
                wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screen and $( printf '%+2s\n' $numproc ) processes are running ... waited for $totmin min ( max = $maxmin )"
                sec1=$( date +%s )
            fi
        fi
        
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
        arrscr=($( screen -ls | grep -i detached | grep ".${strscr}_${strexpid}_" | tr -d '\t' | cut -d'(' -f1 ))
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


#todo:  create data files for submit script if these are submitted later time (runqsub=0)
if (( $runqsub == 0 )) && (( ${#arrdexp_ready[@]} > 0 ));then
    wmessage 

    #todo:  wait until all screen session are gone.
    #note:  this is necessary when run_submit.sh and run_${strscr}.sh are executed by cron
    #numscr=$( screen -ls | grep -i detached | grep $strscr | grep $strexpid | wc -l )
    numscr=$( screen -ls | grep ${strscr}_${strexpid}_ | wc -l )
    totsec=0

    wmessage "$( date +'%m/%d/%Y %H:%M' ) ... $( printf '%02g\n' $numscr ) screen sessions are running ... will wait for max of $maxmin minutes ..."

    while (( $numscr > 0 )) && (( $totsec != $maxsec )) ;do
        sleep ${numsec}s

        totsec=$(( totsec + numsec ))
        totmin=$( echo "$totsec/$numsec" | bc )
        
        wmessage "$( date +'%m/%d/%Y %H:%M' ) ... $( printf '%02g\n' $numscr ) screen sessions are running ... waited for $totmin min ..."
        
        #numscr=$( screen -ls | grep -i detached | grep $strscr | grep $strexpid | wc -l )
        numscr=$( screen -ls | grep ${strscr}_${strexpid}_ | wc -l )
    done
        
    #todo:  kill screen if running
    if (( $numscr > 0 ));then
        arrscr=($( screen -ls | grep -i detached | grep $strscr | grep $strexpid | tr -d '\t' | cut -d'(' -f1 ))

        wmessage 
        wmessage "... $numscr screen sessions didn't complete within the time limit ( $maxmin minutes ) ..."
        wmessage 
        
        arrrmdexp=()
        for scr in ${arrscr[@]};do

            yyyymmddensm=$( echo $scr | rev | cut -d'_' -f1 | rev )

            #todo:  kill screen 
            screen -XS $scr quit
            wmessage "... screen session, $scr, is deleted ..."

            #message_gcmsetup_fcst_19811202ens1
            thisfmsg=$dmess/message_${strscr}_${strexpid}_$yyyymmddensm

            thissftid=$( grep -i "Shift id" $thisfmsg | rev | cut -d' ' -f1 | rev )
            thissftstatus=$( /usr/local/bin/shiftc --status=csv | grep "${thissftid}," 2>/dev/null | cut -d',' -f2 )
            thisdexp=$( grep "exp location" $thisfmsg | rev | cut -d' ' -f1 | rev )
            
            #todo:  stop shiftc if running
            if [[ -n $thissftid &&  "$thissftstatus" != "done" ]];then
                /usr/local/bin/shiftc --id=$thissftid --stop
                wmessage "... $thissftid shift process has been stopped ..."
                sleep ${secsleep}s
            fi

            #todo:  add dexp to arrrmdexp
            [[ -d $thisdexp ]] && arrrmdexp+=( $thisdexp )

        done
            
        #todo:  add dexp to arrrmdexp 
        wmessage "... deleted ${#arrrmdexp[@]} exp directories ..."
        ahand_warr ${arrrmdexp[@]} 
        rm -rf ${arrrmdexp[@]} 2>/dev/null
            
        wmessage
                
        arrdexp_submit1=($( printf '%s\n' ${arrdexp_ready[@]} ${arrrmdexp[@]} | sort -V | uniq -u ))

    else
        arrdexp_submit1=${arrdexp_ready[@]}
    fi
    
    arrdexp_setupfail=($( printf '%s\n' ${arrdexp_submit1[@]} | xargs -i bash -c '[[ ! -f {}/gcmsetup_setupready ]] && echo {}' ))
       arrdexp_submit=($( printf '%s\n' ${arrdexp_submit1[@]} | xargs -i bash -c '[[   -f {}/gcmsetup_setupready ]] && echo {}' ))

    if (( ${#arrdexp_setupfail[@]} > 0 ));then
        wmessage "Setting Up Dir Failed:"
        if (( $writetofile == 1 ));then
            printf '    %s\n' ${arrdexp_setupfail[@]} >> $fmessage 2>&1
        else
            printf '    %s\n' ${arrdexp_setupfail[@]}
        fi
    fi

    if (( ${#arrdexp_submit[@]} > 0 ));then
        wmessage "... making data files for run_submit.sh script with dir created ( total = ${#arrdexp_submit[@]} ) ..."
        printf '%s\n' ${arrdexp_submit[@]} >> $dtmpdata/${strf}${cdate}
        wmessage
        wmessage "Created : $dtmpdata/${strf}${cdate}"
    fi
fi

#todo:  send email
sizef=$( stat --print='%s' $fmessage )
#(( $writetofile == 1 )) && (( $sizef > 0 )) && mail -s "$msg_subject" "$eadds" < $fmessage &
if (( $sizef > 0 ));then
    msg_cyberpostman "$msg_subject" "$eadds" $fmessage $blmsgmutt
    (( $? == 0 )) && rm -f $fmessage
fi

exit

