#!/usr/bin/env bash

#note: for debugging
#ref: https://is.gd/kxHve1
#exec 5> debug_$(basename "$0")
#BASH_XTRACEFD="5"
#PS4='$LINENO: '
#set -x

inputcheck() {
    #$optf && [[ ! -f ${arrfsub[0]} ]] && die "${arrfsub[0]} file doesn't exist."
    :
    return
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
if [[ "$hst" =~ "lfe"* ]];then
    :
elif [[ "$hst" =~ "pfe"* ]];then
    echo "this scriptworks only on lfe";exit
elif [[ "$hst" =~ "discover"* ]];then
    echo "this script works only on lfe";exit
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
#[[ -z $RUN_BY_CRON ]] && RUN_BY_CRON=false

hstshort=$( get_host )
rundebug=0
writetofile=0
optb=false
optd=false
optf=false
optr=false

#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        find corrupted rst tarballs on LOU.
        
        Usage: ./$(basename "$0") [-hw] [-d dir] srcme_file

        input:        
            A source file, which set various vars for a set of runs (i.e. srcme_pfe_*)

        options:
            -d  a full path to a dir where all tar files are (i.e. DARCHRST )
            -h  show this help text
            -w  write stdout/err in a file
"
#            -r  YYYYMM and YYYYMM range (format: YYYYMM-YYYYMM) 
#            -f  a data filename with a list of experiment to monitor
OPTIND=1
while getopts 'hwbd:f:r:' option; do
    case "$option" in
        d)  if $optf || $optr ;then die "Cannot specify option d when specifying option r or f";fi
            optd=true; thisdir=( $OPTARG );;
        f)  if $optr || $optd ;then die "Cannot specify option f when specifying option r or d";fi
            optf=true; fdata=$OPTARG ;;
        r)  if $optd || $optf ; then die "Cannot specify option r when specifying option f or d";fi
            optr=true; 
            startyyyymm=$( echo $OPTARG | cut -d'-' -f1 );
              endyyyymm=$( echo $OPTARG | cut -d'-' -f2 );;
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
strf=${arrposarg[i]}; i=$(( i + 1 ))
[[ -n $strf && -f $strf ]] && source $strf || exit
inputcheck
#================================================================================
#                             Set Host Specific Vars
#================================================================================
#todo: check the name of system (discover or pfe) and set variables.
hst=`hostname`
if [[ $hst == 'pfe'* ]];then
    :
elif [[ $hst == 'discover'* ]];then 
    :
fi
#================================================================================
#                                 Set Variables
#================================================================================
#mid
thishst=$( get_host )
dmess=$cdir/message

fmessage=$dmess/message_${strscr}_${strexpid}
fdata=$cdir/data_${strscr}_${strexpid}

blheader=false
msg_subject="${thishst}.${strscr}: $strexpid"

feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

[[ ! -d $dmess ]] && mkdir -p $dmess
#================================================================================
#                                  Main Process
#================================================================================
#main
arrftar=($( find $thisdir/* -type f -name '*.tar' | sort -V ))

#todo:  check if tar is corrupted
cnt=0
for ftar in ${arrftar[@]};do 
    tar -tf $ftar >> /dev/null 2>&1
    status_tar=$?
    if (( $status_tar > 0 ));then 
        arrftar_crpt+=( $ftar ) 
    fi
    cnt=$(( cnt + 1 ))
    modnumf=$( echo " $cnt % 100 " | bc )

    #if (( $modnumf == 0 ));then
    #    wmessage "... $( printf '%02g' $cnt ) of $( printf '%02g' ${#arrftar[@]} ) ... checked ... corrupted tar = ${#arrftar_crpt[@]} ..."
    #fi
done 

if (( ${#arrftar_crpt[@]} > 0 ));then

    ! $blheader && writeheader && blheader=true

    wmessage
    printf '%s\n' ${arrftar_crpt[@]} | sort -V >> $fmessage 
    printf '%s\n' ${arrftar_crpt[@]} | sort -V >> $fdata
    wmessage
    wmessage "Number of Corrupted Tar : ${#arrftar_crpt[@]} out of ${#arrftar[@]}"
    wmessage
    wmessage "... delete corrupted tars ..."
    rm -f ${arrftar_crpt[@]} 2>/dev/null
#else
#    wmessage "Number of Corrupted Tar : ${#arrftar_crpt[@]} out of ${#arrftar[@]}"
#    wmessage
fi

#todo:  send email
[[ -f $fmessage ]] && sizef=$( stat --print='%s' $fmessage ) || exit

if (( $sizef > 0 && ${#arrftar_crpt[@]} > 0 ));then
    msg_cyberpostman "$msg_subject" "$eadds" $fmessage
    (( $? == 0 )) && rm -f $fmessage
fi


exit

