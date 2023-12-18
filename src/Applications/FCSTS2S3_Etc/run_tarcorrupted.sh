#!/usr/bin/env bash

#note: for debugging
#ref: https://is.gd/kxHve1
#exec 5> debug_$(basename "$0")
#BASH_XTRACEFD="5"
#PS4='$LINENO: '
#set -x

inputcheck() {
    $optf && [[ ! -f ${arrfsub[0]} ]] && die "${arrfsub[0]} file doesn't exist."
}

filter_arch() {
    
    local _arrinput=( "$@" )
    local cntbug=0
    local arr=()
    local _dexparch

    local _arrscr=($( screen -ls | tr -d '\t' | grep "detached\|${strscr}_${strexpid}_" | cut -d'(' -f1 | cut -d'.' -f2- | cut -d'_' -f3 | sed 's#ens#/ens#g' | xargs -i printf "$DFCST/%s/archive\n" {} ))
    
    #todo: check if archiving has completed
    local _arrinput=($( printf '%s\n' ${_arrinput[@]} | xargs -i bash -c "[[ ! -f {}/$fcomp && ! -f {}/$fdel ]] && echo {}" ))
       
    debug_filter ${_arrinput[@]}
    
    #todo:  check if screen  is running for dexp
    local _arrinput=($( printf '%s\n' ${_arrinput[@]} ${_arrscr[@]} | sort -V | uniq -u ))

    debug_filter ${_arrinput[@]}

    ##todo:  check if shiftc is running for dexp
    #local _arrinput=($( printf '%s\n' ${_arrinput[@]} ${arrsftid[@]} | sort -V | uniq -u ))

    #debug_filter ${_arrinput[@]}
    
    local cntbug_org=$cntbug
    for _dexparch in ${_arrinput[@]};do

        cntbug=$cntbug_org
        local _dexp=$( echo $_dexparch | rev | cut -d'/' -f2- | rev )
        set_rstfcstdate $_dexp

        #todo:  check if # of running shiftc is more than the limist set in run_gcmarch.sh
        local _limsft=$( grep "limsftc=" $_dexparch/run_gcmarch.sh 2>/dev/null | rev | cut -d'=' -f1 | rev )
        local _numsft=$( printf '%s\n' ${arrsftid_all[@]} | xargs -i bash -c "/usr/local/bin/shiftc --id={} --status=csv | grep run | grep lou | grep GEOS_fcst/$fcstdate/$ensm | head -1" 2>/dev/null | wc -l ) # | cut -d',' -f3 |cut -d':' -f2 | rev | cut -d'/' -f3-4 | rev | sort -V | uniq " ))
        (( $_numsft >= $_limsft )) && continue

        debug_filter $_dexparch

        #todo:  check if run_gcmarch.sh is running
        #note:  this is checked in monitor.sh (... that's why dexp/archive is listed in data_submit_*)
        #       but check the status again just in case. 
        cnt_jobs $_dexp
        (( $num_rarc > 0 )) && continue

        debug_filter $_dexparch


        arr+=( $_dexparch )
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
if [[ "$hst" =~ "pfe"* || "$hst" =~ "lfe"* ]];then
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
optb=false
optd=false
optf=false
optr=false

#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        find corrupted tarballs.
        
        Usage: ./$(basename "$0") [-bchw] [-d exp dir] [-r YYYYMM-YYYYMM] srcme_file

        input:        
            A source file, which set various vars for a set of runs (i.e. srcme_pfe_*)

        options:
            -b  run with a debug mode 
            -d  a full path to an archive dir (i.e. dexp)
            -r  YYYYMM and YYYYMM range (format: YYYYMM-YYYYMM) 
            -b  run with a debug mode (this will not execute ${strscr}.sh)
            -h  show this help text
            -w  write stdout/err in a file
"
#            -f  a data filename with a list of experiment to monitor
OPTIND=1
while getopts 'hwbd:f:r:' option; do
    case "$option" in
        d)  if $optf || $optr ;then die "Cannot specify option d when specifying option r or f";fi
            optd=true; arrdexp_arch=( $OPTARG/archive );;
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
dfcst=$DFCST

dstatus=$cdir/output/getstatus/$strexpid

falltar_base=$cdir/data_${strscr}_alltarout_
falltarcrpt_base=$cdir/data_${strscr}_tarcrpt_

strflst=data_getstatus_${strexpid}_list_
strarch=gcmarch

fmessage=$dmess/message_$strscr
farchscr=run_${strarch}.sh

fcomp=gcmarch_archcompleted
fdel=gcmarch_deloutcompleted

#msg_subject="$thishst.$strscr" 
msg_subject="${thishst}.${strscr}: $strexpid"

#note:  max limit for total # of shiftc running
limsftc=20

#note:  screen limit
limscr=5

#note:  numsec is wailt time in sec and maxsec is total wait time limit
numsec=60
maxsec=120
maxsec=900
maxmin=$( echo "$maxsec / 60" | bc  )

#RUNCHILD=1

feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

[[ ! -d $dmess ]] && mkdir -p $dmess
#================================================================================
#                                  Main Process
#================================================================================
#main
fhis2=/nobackupp2/knakada/GEOS_Models/knakada_s2s3_unstable_FullModel_20211203_ROME/GEOSodas/src/Applications/GEOSgcm_App/HISTORY.AOGCM-S2Sv3_2.rc.tmpl
arrcoll=($( exp_getcollections $fhis2 ))
arrcollmon=($( printf '%s\n' ${arrcoll[@]} | grep '_1mo_' | sort -V ))
arrcollnonmon=($( printf '%s\n' ${arrcoll[@]} | grep -Ev '_1mo_' | sort -V ))
arrcollnonmon_save=($( printf '%s\n' ${arrcollnonmon[@]} | grep -E '_sfc|_slv' | sort -V ))
arrcollnonmon_rm=($( printf '%s\n' ${arrcollnonmon[@]} | grep -Ev '_sfc|_slv' | sort -V ))

startyyyymm=198112
  endyyyymm=198312

get_beg_and_end $startyyyymm $endyyyymm
arrdexp1+=($( get_dexp $dfcst $begyyyymm $endyyyymm ${arrintens[@]} ))
arrdexp=($( printf '%s\n' ${arrdexp1[@]} | xargs -i bash -c "[[ -d {} ]] && echo {}" | sort -V ))

yyyymm=$startyyyymm
while (( $yyyymm < $endyyyymm ));do
    arrdexpcoll=($( printf '%s\n' ${arrdexp[@]} | xargs -i printf "{}/%s\n" ${arrcollnonmon[@]} ))
    ftarcrpt=$falltarcrpt_base$yyyymm
    arrftar=()
    arrftar_crpt=()

    if [[ ! -f $ftarcrpt ]];then

        wmessage "... $yyyymm find all tar outputs ..."

        for dexpcoll in ${arrdexpcoll[@]};do
            [[ -d $dexpcoll ]] && arrftar+=($( find $dexpcoll/* -maxdepth 0 -type f -name '*.nc4.tar' 2>/dev/null ))
        done 

        wmessage "... check ${#arrftar[@]} tar files ..."

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

            if (( $modnumf == 0 ));then
                wmessage "... $( printf '%02g' $cnt ) of $( printf '%02g' ${#arrftar[@]} ) ... checked ... corrupted tar = ${#arrftar_crpt[@]} ..."
                #wmessage "Number of Corrupted Tar : ${#arrftar_crpt[@]} out of ${#arrftar[@]}"
            fi
        done 
        
        printf '%s\n' ${arrftar_crpt[@]} | sort -V >> $ftarcrpt 
    fi

    yyyymm=$( fcal_nextmonth $yyyymm )

done 
exit

wmessage lineno = $LINENO 
wmessage
wmessage arrdexpcoll
ahand_warr ${arrdexpcoll[@]}
exit
#ahand_warr ${arrdexp[@]} 
#wmessage
#wmessage arrftar
#ahand_warr ${arrftar[@]}
wmessage arrcollnonmon:
ahand_warr ${arrcollnonmon[@]}
wmessage
wmessage arrcollnonmon_save
ahand_warr ${arrcollnonmon_save[@]}
wmessage
wmessage arrcollnonmon_rm:
ahand_warr ${arrcollnonmon_rm[@]}

















exit


