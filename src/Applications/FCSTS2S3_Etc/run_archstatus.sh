#!/usr/bin/env bash

inputcheck(){
    [[ -z  ${arrposarg[@]} ]] && die "missing input"
    (( ${#arrposarg[@]} != 1 )) && die "1 input is required"

    #! $optf && ! $optr && die "option r or f is requied." 

    local srcme=$( echo ${arrposarg[0]} | cut -d'_' -f2 )
    ! $blnonpfe && [[ "$srcme" != "$hstshort" ]] && die "source file is not for this host (select srcme_${hstshort}_* file)"
}

sendfmsg() {
    #description:   send email
    #local _blrm=$blsavefmsg
    local _blrmferr=false

    [[ -f $ferr     ]] && local _sizeferr=$( stat --print='%s' $ferr )  || local _sizeferr=0
    [[ -f $fmessage ]] && local _sizef=$( stat --print='%s' $fmessage ) || local _sizef=0

    if (( $_sizef > 0 || $_sizeferr > 0 ));then
        if (( $_sizeferr > 0 ));then 
            msg_wheader_userdefined 40 "-" $( basename $ferr ) 
            wmessage "$( cat $ferr )"
            _blrmferr=true
        fi

        msg_cyberpostman "$thismsg_subject" "$eadds" $fmessage
        local status_email=$?
        if (( $status_email == 0 )) && [[ -f $fmessage ]] ;then 
            $blsavefmsg && mv $fmessage $fmsg_final
            [[ -f $fmessage ]] && rm -f $fmessage
            $_blrmferr && msg_newfile $ferr
        fi
    fi
    return
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
blnonpfe=false
if [[ ${hstname:0:3} == pfe ]];then
    :
elif [[ ${hstname:0:3} == dis ]];then
    echo "this script works only on pfe";exit
elif [[ "${hstname:0:1}" == r ]];then 
    #note:  this is for host other than pfe @ NAS which has a name starting with 'r'
    blnonpfe=true

elif [[ "${hstname:0:4}" == borg || "${hstname:0:4}" == warp ]];then 
    #note:  this is for host other than pfe @ NCCS which has a name starting with 'borg' or 'warp'
    :
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

hstshort=$( get_host )
writetofile=0
optb=false
optr=false
optf=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        check archive status for each ensemble members by counting outputs. Stdout
        are written for each year and they are sent separarately. When no status is 
        changed for a specific year by comparing stdout from the previous run, it 
        will NOT send stdout.
        
        *When this script is executed without option f or r, it will 
        work on near-realtime forecast dates.*

        Usage: ./$( basename "$0") [-bchw] [-r YYYYMM-YYYYMM] [-f file] srcme_file 

        options:
            -b  run with a debug mode
            -r  YYYYMM and YYYYMM range ( format: YYYYMM-YYYYMM ) 
            -f  a file with a list of directory paths
            -c  clean unwanted files
            -h  show this help text
            -w  write stdout/err in a file
"

OPTIND=1
while getopts ':bhcwr:f:' option; do
    case "$option" in
        b)  optb=true;; 
        f)  $optr && die "Cannot specify option f & r at the same time.";
            optf=true; userinput=$OPTARG;;
        r)  $optf && die "Cannot specify option r & f at the same time.";
            optr=true; 
            userbegyyyymm=$( echo $OPTARG | cut -d'-' -f1 );
            userendyyyymm=$( echo $OPTARG | cut -d'-' -f2 );;
        h)  echo "$usage"; exit 0;;
        c)  clean_dir; exit 0;;
        w)  writetofile=1;; 
        \?) die "Invalid option: -$OPTARG" >&2
            exit 1;;
        :)  die "Option -$OPTARG requires an argument." >&2
            exit 1;;
    esac
done

! $optr && ! $optf && blnrt=true || blnrt=false

#todo:  get positional inputs.
shift $((OPTIND-1))
[[ "${1:-}" = "--" ]] && shift
arrposarg=( "$@" )

#================================================================================
#                               Check User Inputs
#================================================================================
inputcheck
i=0
 strf=${arrposarg[i]}; i=$(( i + 1 ))
[[ -n $strf ]] && source $strf

#================================================================================
#                                 Set Variables
#================================================================================
#mid
thishst=$( get_host )
[[ -n $rstexpid ]] && blrstexpid=true && strexpid=$strexpid$rstexpid || blrstexpid=false

dstdout=$cdir/stdout/$strscr/$strdout
   dmess=$cdir/message
fmessage=$dmess/message_${strscr}_$strexpid
ferr=$dmess/stderr_${strscr}

ftmp1=$cdir/tmp_${strscr}_1
ftmp2=$cdir/tmp_${strscr}_2

msg_subject="${hstshort}.${strscr}: $strexpid"
feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

arryyyymm=()
#note:  save fmessage in dstdout. This was created 
#       in case sending fmessage doesn't work 
blsavefmsg=true

[[ -f $fmessage  ]] && rm -f $fmessage
[[ ! -d $dmess   ]] && mkdir -p $dmess
[[ ! -d $dstdout ]] && mkdir -p $dstdout
#================================================================================
#                                  Main Process
#================================================================================
#letsbegin
if $optr;then
    yyyymm=$userbegyyyymm
    while (( $yyyymm <= $userendyyyymm )) ;do 
        arryyyymm+=( $yyyymm )
        yyyymm=$( fcal_nextmonth $yyyymm ) 
    done 
    strmsg="$userbegyyyymm - $userendyyyymm"

elif $optf;then
    arryyyymm=($( cat $userinput )) 
    strmsg="$( basename $userinput )"

elif $blnrt;then 
    #todo:  without options, this code will figureout all forecasts from the last month and ones up to 
    #       today's date in the current month. 
    arryyyymmdd=($( s2sv3_nrtdates )) 
    arryyyymm=($( printf '%s\n' ${arryyyymmdd[@]} | cut -c1-6 | sort -V | uniq ))
fi

(( ${#arryyyymm[@]} == 0 )) && exit

arrfinal=( ${arryyyymm[@]} ) 

if $optb;then
    wmessage "Ready:"
    ahand_print ${arrfinal[@]}
    exit
fi

(( ${#arrfinal[@]} == 0 )) && exit

for yyyymm in ${arrfinal[@]} ;do
    fmsg_final=$dstdout/message_${strscr}_${strexpid}_${yyyymm}
    thismsg_subject="$msg_subject $yyyymm" #( User Input: $strmsg )"
    
    [[ -f $fmessage ]] && rm -f $fmessage
    [[ -f $ftmp1    ]] && rm -f $ftmp1
    [[ -f $ftmp2    ]] && rm -f $ftmp2

    exp_archstatus $yyyymm

    #todo   diff previous archstatus stdout and fmessage.
    #       If there are difference, email fmessage
    if [[ -f $fmessage ]]; then 
        if [[ -f $fmsg_final ]];then
            cat $fmessage   | tail -n +4 >> $ftmp1
            cat $fmsg_final | tail -n +4 >> $ftmp2

            diff $ftmp1 $ftmp2 >>/dev/null 2>&1
            status_diff=$?
        else
            status_diff=999
        fi

        #todo:  send email
        if (( $status_diff > 0 ));then 
            sendfmsg
        else
            [[ -f $fmessage ]] && rm -f $fmessage
        fi
    fi
    
    [[ -f $ftmp1 ]] && rm -f $ftmp1
    [[ -f $ftmp2 ]] && rm -f $ftmp2

done



exit
