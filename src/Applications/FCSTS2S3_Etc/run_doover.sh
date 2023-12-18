#!/usr/bin/env bash

inputcheck(){
    (( ${#arrposarg[@]} != 2 )) && die "2 input is required"

    [[ ! -f $srcf ]] && die "$srcf file does not exist"

    local _blnum=$( misc_isinteger $runqsub ) 
    (( $_blnum > 0 )) && die "input has to be a number ( 0 or 1)."

    if $optq; then 
        local _arrqidall=($( res_qid_all )) 

        printf '%s\n' ${_arrqidall[@]} | grep -w $qid >/dev/null 2>&1 
        local _status_grep=$?
        (( $_status_grep > 0 )) && die "$qid reservations does not exist"
    fi

    $optr && [[ -z $userbegyyyymm || -z $userendyyyymm ]] && die "YYYYMM range is a required input for option r"
    $optd && [[ ! -d $dexp ]] && die "$dexp does not exist"

    return
}

filter() {
    
    local _arrinput=( "$@" )
    local cntbug=0
    local arrjobs=()
    local _strpbs_jname=Job_Name
    local _strpbs_output=Output_Path
    local arr=()
    local _dexp jobid

    #todo:  get all running screen sessions
    local _arrscr=($( screen -ls | tr -d '\t' | grep "detached\|${strscr}_${strexpid}_" | cut -d'(' -f1 | cut -d'.' -f2- | cut -d'_' -f3 | sed 's#ens#/ens#g' | xargs -i printf "$DFCST/%s\n" {} ))

    #todo:  check if clean_completed marker exists
    local _arrinput=($( printf '%s\n' ${_arrinput[@]} | xargs -i bash -c "[[ ! -f {}/$fcln_comp ]] && echo {}" ))
    
    debug_filter ${_arrinput[@]}

    #todo:  check if gcmarch_archcompleted & gcmarch_deloutcompleted exist
    local _arrinput=($( printf '%s\n' ${_arrinput[@]} | xargs -i bash -c "[[ ! -f {}/archive/$farc_comp ]] && echo {}" ))
    
    debug_filter ${_arrinput[@]}

    #todo:  check if screen is running for dexp
    local _arrinput=($( printf '%s\n' ${_arrinput[@]} ${_arrscr[@]} | sort -V | uniq -u ))

    debug_filter ${_arrinput[@]}

    local cntbug_org=$cntbug
    for _dexp in ${_arrinput[@]};do

        cntbug=$cntbug_org

        set_rstfcstdate $_dexp
       
#        #todo:  check if note_gcmarch exists
#        [[ ! -f $_dexp/$farc_note ]] && continue  
#
#        debug_filter $_dexp


        #todo:  get job names
        local arrjobs=($( $_qstat -u $USER -W fmt_Queue="-maxw 40" | grep $fcstdate$ensm | sort -V -k4 -t' ' | tr -s '[:space:]' | cut -d' ' -f4 | cut -d'.' -f1 | sort -V | uniq | grep ens ))

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
        local blwinner=$( exp_checkwinner $_dexp $ddata_pckwinners )

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

        #todo:  stdout file exist
        if $blwinner || $blrerun ;then
            #doover/fcst/message_doover_fcst_19930913ens1
            local _fstdout=$( find $cdir/stdout/${strscr}/$strexpid/* -type f -name "message_${strscr}_${strexpid}_${fcstdate}${ensm}_W" | tail -1 )
        else
            local _fstdout=$( find $cdir/stdout/${strscr}/$strexpid/* -type f -name "message_${strscr}_${strexpid}_${fcstdate}${ensm}" | tail -1 )
        fi
            
        [[ -f $_fstdout ]] && continue 

        debug_filter $_dexp


        #todo:  check gcm_run.j is running 
        printf '%s\n' ${arrjobs[@]} | grep -w R$fcstdate$ensm >/dev/null
        local status_grep=$?
        (( $status_grep == 0 )) && continue
        
        debug_filter $_dexp

        #todo:  check mini_gcm_run.j is running
        printf '%s\n' ${arrjobs[@]} | grep -w M$fcstdate$ensm >/dev/null
        local status_grep=$?
        (( $status_grep == 0 )) && continue

        debug_filter $_dexp

        #todo:  check run_gcmarch.sh is running
        printf '%s\n' ${arrjobs[@]} | grep -w A$fcstdate$ensm >/dev/null
        local status_grep=$?
        (( $status_grep == 0 )) && continue

        debug_filter $_dexp

        #todo:  check gcm_post.*.j is running
        printf '%s\n' ${arrjobs[@]} | grep -w P$fcstdate$ensm >/dev/null
        local status_grep=$?
        (( $status_grep == 0 )) && continue

        debug_filter $_dexp

        arr+=( $_dexp )
    done

    echo ${arr[@]} 
}

exp_sortexp(){
    #description:   sort out input array with dexp. Output is an array wit 3month run exp or 
    #               winner dexp depending on user input.

    local _blwin=$1;shift
    local _arrdin=( "$@" ) 
    local dout_pckwinners=$cdir/output/pckwinners/$strdout
    #winners_nino3.4_fcst_198112_198202.txt
    local strfwin=winners_${reg}_${strexpid}_
    local arrdwin=()
    local _arr=()
    local yyyymm

    local _arryyyymm=($( printf "%s\n" ${_arrdin[@]} | rev | cut -d'/' -f2 | rev | cut -c1-6 | sort -V | uniq )) 

    for yyyymm in ${_arryyyymm[@]};do 
        arrdwin+=($( cat $dout_pckwinners/${strfwin}${yyyymm}_[0-9]*.txt | sort -V | sed "s# #/#g" | xargs -i echo $DFCST/{} ))
    done
  
    local arrdwin=($( printf '%s\n' ${_arrdin[@]} ${arrdwin[@]} | sort -V | uniq -d ))
    local arrd3mon=($( printf '%s\n' ${_arrdin[@]} ${arrdwin[@]} | sort -V | uniq -u ))

    if $_blwin; then
        _arr=( ${arrdwin[@]}  )
    else
        _arr=( ${arrd3mon[@]} )
    fi
    echo ${_arr[@]}
}

clean_dir() {
    [[ -n $flock && -f $flock ]] && rm -f $flock
    [[ -n $flst_lfe && -f $flst_lfe ]] && rm -f $flst_lfe
    return
}

#================================================================================
#                                     Begin
#================================================================================
#beg
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

#strscr=$(basename "$0" | cut -d'_' -f2 | cut -d'.' -f1 )
strscr=$(basename "$0" | cut -d'_' -f2-3 | cut -d'.' -f1 )
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

#note:  this script is only for hindcast yyyyymmdd < 19960101
writetofile=0
rundebug=0
optb=false
opti=false
optt=false
optf=false
optd=false
optr=false
optq=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        re-run experiments, which have missing holding/collection/YYYYMM dir. 
        A list of those experiments are written in $fdatadefault. When opt r is
        selected, experiment directories are extracted from the file. 

        Usage: ./$(basename "$0") [-bchw] [-q qid] [-d exp dir] [-f data file] [-r YYYYMM-YYYYMM] srcme_file [ 1 or 0 ] 

        Input:
            Reserved queue ID
            Source file, which defines various variables for a group of experiments
            Enter 1 to submit a job after exp dir is ready; 0 for setting up exp dir only

        options:
            -b  run with a debug mode 
            -q  queue id
            -d  a full path to an experiment dir
            -f  data file with a list of dexp fullpath
            -r  YYYYMM and YYYYMM range (format: YYYYMM-YYYYMM) 
            -i  doover winners exp
            -t  doover 3-month runs exp
            -c  clean unwanted files
            -h  show this help text
            -w  write stdout/err in a file

"

OPTIND=1
while getopts ':bhwcitf:d:r:' option; do
    case "$option" in
        b)  optb=true;rundebug=1;;
        d)  if $optf || $optr ;then die "Cannot specify option d when specifying option r or f";fi
            optd=true; dexp=$OPTARG; arrdexp=( $OPTARG );;
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
        q)  optq=true; in_qid=$OPTARG;;
        c)  clean_dir; exit 0;;
        h)  echo "$usage"; exit 0;;
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

#================================================================================
#                               Check User Inputs
#================================================================================
i=0
srcf=${arrposarg[$i]};i=$(( i + 1 ))
runqsub=${arrposarg[$i]};i=$(( i + 1 ))
inputcheck
source $srcf

[[ -z $DARCHRST || -z $DRST ]] && die "DARCHRST or DRST is undefied"
#================================================================================
#                             Set Host Specific Vars
#================================================================================
if [[ $hst =~ "pfe"* ]];then
    qid_default=long

    basepath=$PATH
    basepath=/home6/gmaofcst/.cargo/bin:/home6/gmaofcst/knakada/bin:/u/scicon/tools/bin:/u/gmaofcst/bin:$basepath
    basepath=/home6/gmaofcst/bin:/PBS/bin:/usr/local/bin:/nobackup/gmao_SIteam/Baselibs/latest-mpiuni/Linux/bin:$basepath
    basepath=/usr/local/bin:/u/scicon/tools/bin:/u/gmaofcst/bin:/home6/gmaofcst/.cargo/bin:$basepath
    basepath=/home5/knakada/.cargo/bin:/u/scicon/tools/bin:/u/knakada/bin:/usr/local/bin:$basepath
    basepath=/usr/local/sbin:/usr/bin:/bin:/usr/X11R6/bin:/PBS/bin:/usr/sbin:/sbin:/opt/c3/bin:$basepath
    export BASEPATH=/opt/sgi/sbin:/opt/sgi/bin:/opt/c3/bin:/opt/sgi/sbin:/opt/sgi/bin:/opt/sgi/sbin:/opt/sgi/bin:$basepath
    export PATH=$BASEPATH
    export TERM=xterm-256color
    umask 0022

elif [[ $hst =~ "discover"* ]];then 
    :
fi

#================================================================================
#                    Set Global Variables and Source Scripts
#================================================================================
#mid
thishst=$( get_host )
cdate=$( date +%Y%m%d_%H%M )
dmess=$cdir/message
dstdout=$cdir/stdout/$strscr/$strdout
ddata_pckwinners=$cdir/output/pckwinners/$strdout

[[ -z $fdata ]] && fdata=$cdir/data/${strscr}/data_${strscr}_${strexpid}_mkdata

fmessage=$dmess/message_${strscr}_${strexpid}
farc_comp=gcmarch_deloutcompleted
fcln_comp=clean_completed
farc_note=note_gcmarch

_qstat=/u/scicon/tools/bin/qstat

#note:  numsec is wailt time in sec and maxsec is total wait time limit
if $blnode;then
    limscr=3
    maxsec=1200
else
    limscr=3
    maxsec=1800
fi
maxmin=$( echo "$maxsec / 60" | bc  )


#todo:  set qid 
if [[ -z $qid && -n $in_qid ]];then
    qid=$in_qid

elif [[ -z $qid && -z $in_qid ]];then
    inputhour=72
    arrqidrun=($( res_Rqid ))
    arrqidend=($( res_qidexpire $inputhour ))
    arrqidbeg=($( res_qidstart $inputhour ))
    arrqidavail_xhrs1=($( printf '%s\n' ${arrqidrun[@]} ${arrqidend[@]} ${arrqidbeg[@]}  | sort | uniq -u )) 

    for qidavail_xhrs in ${arrqidavail_xhrs1[@]};do 
        thismname=$( res_mname $qidavail_xhrs )
        [[ "$thismname" == "rom_ait" ]] && arrqidavail_xhrs_rome+=( $qidavail_xhrs ) || arrqidavail_xhrs_nonrome+=( $qidavail_xhrs )
    done 

    if [[ -n $mname ]];then 
        [[ "$mname" == "rom_ait" ]] && arrqidavail_xhrs+=( ${arrqidavail_xhrs_rome[@]} ) || arrqidavail_xhrs=( ${arrqidavail_xhrs_nonrome[@]} )
    else
        strctag=$( echo $ctag | grep -i rome )
        [[ $strctag == *"ROME"* ]] && arrqidavail_xhrs+=( ${arrqidavail_xhrs_rome[@]} ) || arrqidavail_xhrs=( ${arrqidavail_xhrs_nonrome[@]} )
    fi
fi

[[ -z $qid && -z $in_qid && -z ${arrqidavail_xhrs[@]} ]] && qid=$qid_default

RUNCHILD=true 

msg_subject="${thishst}.${strscr}: $strexpid"
feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

[[ ! -d $dstdout ]] && mkdir -p $dstdout
[[ ! -d $dmess   ]] && mkdir -p $dmess
[[ ! -d $DRST    ]] && mkdir -p $DRST

export RUNCHILD
#================================================================================
#                                  Main Process
#================================================================================
#main

if $optd;then
    strmsg=$dexp
    msg_subject="$msg_subject ( $strmsg )"

elif $optf;then
    arrdexp=($( misc_readfbyline $fdata )) 

    strmsg=$( basename $fdata )
    msg_subject="$msg_subject ( $strmsg )"

elif $optr ;then
    get_beg_and_end $userbegyyyymm $userendyyyymm
    arrdexp=($( get_dexp $DFCST $begyyyymm $endyyyymm ${arrintens[@]} ))
    arrdexp=($( printf '%s\n' ${arrdexp[@]} | sort -V | uniq | xargs -i bash -c "[[ -d {} ]] && echo {}" ))

    strmsg="$userbegyyyymm - $userendyyyymm ;Data File: $( basename $fdata )"
    msg_subject="$msg_subject ( $strmsg )"
fi

#todo:  filter
arrdexp=($( filter ${arrdexp[@]} ))  

#todo:  extract winners or 3mo runs
if $opti || $optt; then 
    #arrdexp_dooverdone=($( grep "exp location"  stdout/doover/fcst/message_doover_fcst_1993* | rev | cut -d' ' -f1 | rev | sort -V | uniq ))
    #printf '%s\n' ${arrdexp_dooverdone[@]} ${arrdexp[@]} | sort -V #| uniq -d
    arrdexp_win=()
    arrdexp_3mo=()
    for dexp in ${arrdexp[@]};do
        blwinner=$( exp_checkwinner $dexp $ddata_pckwinners )
        if $blwinner;then 
            arrdexp_win+=( $dexp )
        else
            arrdexp_3mo+=( $dexp )
        fi
    done 
fi
#wmessage arrdexp_win:
#ahand_print ${arrdexp_win[@]} 
#wmessage
#wmessage arrdexp_3mo:
#ahand_print ${arrdexp_3mo[@]} 


#todo:  get winners exp or 3month run exp.
if $opti;then
    arrfinal=( ${arrdexp_win[@]} )  
elif $optt;then
    arrfinal=( ${arrdexp_3mo[@]} )  
else
    arrfinal=( ${arrdexp[@]} )
fi

#todo:  exit out when optb is ture
if $optb;then 
    ahand_print ${arrfinal[@]}
    exit
fi

(( ${#arrfinal[@]} == 0 )) && exit

msg_wheader 

wmessage $hst
[[ -n $strmsg ]] && wmessage "                 User Inputs : $strmsg"
wmessage "         Total number of exp : $( printf '%+5s\n' ${#arrfinal[@]} ) ( maxlim = $( printf '%+5s\n' $limscr  ) )"

$optr && [[ -n $begyyyymm && -n $endyyyymm ]] && wmessage "                 Doover Year : $begyyyymm - $endyyyymm"

if $opti || $optt ;then
    #wmessage "                Current Hour : $( printf '%+5s\n' $chh )"
    $optt && wmessage "                               Doover 3month runs"
    $opti && wmessage "                               Doover winner runs"
fi

wmessage
blinitexecute=false
for dexp in ${arrfinal[@]};do

    #todo:  get index of dexp
    for i in "${!arrfinal[@]}"; do
        [[ "${arrfinal[$i]}" == "$dexp" ]] && break 
    done
    
    #todo:  select qid when there is a reservation(s) and if qid is not defined
    if (( ${#arrqidavail_xhrs[@]} > 0 ));then
        if (( ${#arrqidavail_xhrs[@]} == 1 ));then
            qid=${arrqidavail_xhrs[0]}
        else
            item=$( shuf -i0-$(( ${#arrqidavail_xhrs[@]} -1 )) -n1 )
            qid=${arrqidavail_xhrs[$item]}
        fi
    fi

    thisind=$(( i + 1 ))

    #todo:  count # of running screen and shiftc
    numscr=$( screen -ls | grep -i detached | grep ${strscr}_${strexpid}_ | wc -l )

    set_rstfcstdate $dexp

    thismessage=$cdir/message/message_${strscr}_${strexpid}_${fcstdate}$ensm
    [[ -f $thismessage ]] && rm -f $thismessage
    
    blexecuted=false
    blinitialnote=false
    totsec=0
    totmin=0

    sec0=$( date +%s )
    sec1=$sec0

    while ! $blexecuted;do 
        if (( $numscr < $limscr && $totsec <= $maxsec ));then
            
            totsec=0
            blexecuted=true
            blinitexecute=true

            wmessage "$( printf "%+3s of %+3s\n" $thisind ${#arrfinal[@]} ) $dexp"
#wmessage \@$LINENO
#wmessage $thismessage $qid $dexp $srcf $runqsub 
#continue 
            screen -dmS ${strscr}_${strexpid}_$fcstdate$ensm bash -c "./${strscr}.sh -m $thismessage $qid $dexp $srcf $runqsub >> $thismessage 2>&1"
            #screen -dmS ${strscr}_${strexpid}_$fcstdate$ensm bash -c "sleep 4m"
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
                wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screens are running " #... waited for $totmin min"
                sleep 1m
                sec0=$( date +%s )
                sec1=$sec0

            elif (( $sec_diff > 60 ));then
                wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screens are running ... waited for $totmin min ( max = $maxmin )"
                sec1=$( date +%s )
            fi

        fi
        
        numscr=$( screen -ls | grep -i detached | grep ${strscr}_${strexpid}_ | wc -l )

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


