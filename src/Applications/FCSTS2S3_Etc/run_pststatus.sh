#!/usr/bin/env bash

inputcheck(){
    [[ -z  ${arrposarg[@]} ]] && die "an input is missing"
    (( ${#arrposarg[@]} != 1 )) && die "1 input is required"

    $optd && [[ -z $userinput ]] && die "option d requires an argument." 
    $optf && [[ -z $userinput ]] && die "option f requires an argument." 

    if $optr;then 
        [[ -z $userbegyyyymm || -z $userendyyyymm ]] && die "option r requires YYYYMM"
    fi

    local srcme=$( echo ${arrposarg[0]} | cut -d'_' -f2 )
    ! $blnonpfe && [[ "$srcme" != "$hstshort" ]] && die "source file is not for this host (select srcme_${hstshort}_* file)"
}

filter() {
    local _arrinput=( "$@" )
    local arr=()  _input 

    #todo:  check if dexp exists
    local _arrinput=($( printf '%s\n' ${_arrinput[@]} | xargs -i bash -c "[[ -d {} ]] && echo {}" ))

    debug_filter ${_arrinput[@]}

    #todo:  check if cleaning is completed
    _arrinput=($( printf '%s\n' ${_arrinput[@]} | xargs -i bash -c "[[ ! -f {}/clean_completed ]] && echo {}" ))

    debug_filter ${_arrinput[@]}

    #todo:  check if archive is completed
    _arrinput=($( printf '%s\n' ${_arrinput[@]} | xargs -i bash -c "[[ ! -f {}/archive/gcmarch_deloutcompleted ]] && echo {}" ))

    debug_filter ${_arrinput[@]}

    local arr=( ${_arrinput[@]} )

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
blnonpfe=false
if [[ "$hst" =~ "pfe"* || "$hst" =~ "lfe"* ]];then
    :
elif [[ "$hst" =~ "r"* ]];then
    #note:  this is for host other than pfe @ NAS which has a name starting with 'r'
    blnonpfe=true
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
rundebug=0
optb=false
optd=false
optf=false
optr=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        check status of gcm_post.*.j* and run_gcmarch.sh.
        
        Usage: ./$( basename "$0") [-hbw] [-r YYYYMM-YYYYMM] [-d dexp] [-f file] srcme_file

        options:
            -b  run with a debug mode ( this will not execute ${strscr}.sh )
            -r  YYYYMM and YYYYMM range ( format: YYYYMM-YYYYMM ) 
            -d  specify forecast date ( format: YYYYMMDD )
            -f  a file with a list of directory paths
            -c  clean unwanted files
            -h  show this help text
            -w  write stdout/err in a file
"

OPTIND=1
while getopts ':hcwbr:d:f:' option; do
    case "$option" in
        b)  optb=true;rundebug=1;;
        d)  $optf && die "Cannot specify option d & f at the same time.";
            $optr && die "Cannot specify option d & r at the same time.";
            optd=true; userinput=$OPTARG;;
        f)  $optd && die "Cannot specify option f & d at the same time.";
            $optr && die "Cannot specify option f & r at the same time.";
            optf=true; userinput=$OPTARG;;
        r)  $optd && die "Cannot specify option r & d at the same time.";
            $optf && die "Cannot specify option r & f at the same time.";
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

#todo:  get positional inputs.
shift $((OPTIND-1))
[[ "${1:-}" = "--" ]] && shift
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
if [[ "$hstshort" == "pfe" ]];then
    ncdump=/home3/mathomp4/bin/ncdump

elif [[ "$hstshort" == "dis" ]];then
    #do something here (i.e. load modules, location of applications etc)
    #. /usr/share/modules/init/bash
    #module load other/cdo-1.9.1
    #module load other/comp/gcc-5.3-sp3 other/SSSO_Ana-PyD/SApd_4.3.1_py2.7_gcc-5.3-sp3
    :
elif $blnode;then 
    ncdump=/home3/mathomp4/bin/ncdump
else
    clean_dir
    exit
fi
#================================================================================
#                                 Set Variables
#================================================================================
#mid
[[ -n $rstexpid ]] && strexpid=$strexpid$rstexpid

dstdout=$cdir/stdout/$strscr/$strdout
   dmess=$cdir/message
ddata_pckwinners=$cdir/output/pckwinners/$strdout

fmessage=$dmess/message_${strscr}_$strexpid
fmsg_fname=message_${strscr}_userinput__${userinput} 

msg_subject="${hstshort}.${strscr}: $strexpid"
feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

#note:  save fmessage in dstdout. This was created 
#       in case sending fmessage doesn't work 
blsavefmsg=true

[[ ! -d $dmess   ]] && mkdir -p $dmess
[[ ! -d $dstdout ]] && mkdir -p $dstdout
#================================================================================
#                                  Main Process
#================================================================================
#main
if $optr ;then
    get_beg_and_end $userbegyyyymm $userendyyyymm
    arrdexp=($( get_dexp $DFCST $begyyyymm $endyyyymm ${arrintens[@]} ))
    strmsg="$userbegyyyymm - $userendyyyymm"
    msg_subject="$msg_subject ( $strmsg )"

elif $optf;then
    arrdexp=($( cat $fdata )) 
    strmsg="$fdata"
    msg_subject="$msg_subject ($( basename $fdata ))"

elif $optd;then
    arrdexp=( $userinput ) 
    strmsg="${arrdexp[0]}"
    msg_subject="$msg_subject ( ${arrdexp[0]} )"

fi

(( ${#arrdexp[@]} == 0 )) && exit

arrfinal=($( filter ${arrdexp[@]} ))

if $optb;then
    wmessage "Ready:"
    ahand_print ${arrfinal[@]}
    exit
fi

(( ${#arrfinal[@]} == 0 )) && exit

msg_wheader

wmessage $( hostname ) 
[[ -n $strmsg ]] && wmessage "User Inputs : $strmsg"
wmessage
exp_pststatus ${arrfinal[@]} 

#todo:  send email
if [[ -f $fmessage ]];then
    sizef=$( stat --print='%s' $fmessage )
    if (( $sizef > 0 ));then
        msg_cyberpostman "$msg_subject" "$eadds" $fmessage
        (( $? == 0 )) && rm -f $fmessage
    fi
fi


exit


