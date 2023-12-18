#!/usr/bin/env bash
inputcheck(){

    $optr && [[ -z $startyyyymm || -z $endyyyymm ]] && die "YYYYMM range is a required input for option r"
    
    if [[ -n $startyyyymm && -n $endyyyymm ]];then
        local int1=$( misc_isinteger $startyyyymm ) 
        local int2=$( misc_isinteger $endyyyymm ) 
        
        if (( $int1 > 0 || $int2 > 0 ));then
            die "YYYYMM range have to be numbers delimited by a hyphen"
        fi
    fi
    return
}


filter() {
    local _arrinput=( "$@" )
    local _input arr arrdexp arrdexist arrfexist

    for _input in ${_arrinput[@]};do
        local cntbug=0
        local yyyyin=${_input:0:4}
        local numic=$( printf '%s\n' $( fcal_calcfcdates ${_input}01 ) | wc -l )

        #note:  5 for atm exp + 1 control;  10 for ocn pert
        local numexp=$( echo "$numic * 5 + 10" | bc )

        #todo:  get the 2nd full month forecast
        local _yyyymm=$_input
        for i in {1..2};do
            _yyyymm=$( fcal_nextmonth $_yyyymm )
        done
        local _yyyymm_twomonplus=$_yyyymm


        #todo:  get all dexp 
        get_beg_and_end $_input $_input
        local arrdexp=($( get_dexp $dfcst $begyyyymm $endyyyymm ${arrintens[@]} ))
        

        #todo: check if collsst dir exists
        local arrdexist=($( printf '%s'/$collsst'\n' ${arrdexp[@]} | xargs -i bash -c '[[ -d {} ]] && echo {}' ))
        (( ${#arrdexist[@]} != $numexp )) && continue
        
        debug_filter $_input
        
        #todo:  check if files exist in collsst
        #geosgcm_sst/20140730.geosgcm_sst.daily.201409.nc4.tar
        arrfexist=($( printf '%s\n' ${arrdexist[@]} | xargs -i bash -c "find {}/* -type f -name '[0-9]*.$collsst.$_yyyymm_twomonplus.nc4.tar' 2>/dev/null" ))
#wmessage lineno = $LINENO ${#arrfexist[@]} $numexp
#ahand_print ${arrfexist[@]}

        #todo:  check if all input sst are ready
        (( ${#arrfexist[@]} != $numexp )) && continue

        debug_filter $_input

        #todo:  check if fmessage exists in stdout dir.
        local fstdout=$dstdout/message_${strscr}_${strexpid}_$_input
        [[ -n $_input && -f $fstdout ]] && continue

        debug_filter $_input

        arr+=( $_input )
    done

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
hst=$( hostname )
blnode=false
if [[ "$hst" =~ "pfe"* ]];then
    :
elif [[ "$hst" =~ "r"* ]];then
    #note:  this is for host other than pfe @ NAS which has a name starting with 'r'
    blnode=true
    :
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

#todo:  write date & time when this script is executed.
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
writetofile_child=1
rundebug=0
optb=false
optr=false

#default values
#startyyyymm=198112
#  endyyyymm=201012
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename $0) -- this is a program to do the following:
        - run various child scripts to determine winners ensemble members for each YYYYMM;
        - archive winners_nino3.4_*, nino*.txt, subsample*.png plot, & the second full month
          sst output on lfe;
        - create symbolic links to winner dir on both pfe & lfe (originate run_setupwinners.sh)
        - create data_submit_* files for run_submit.sh (originates run_setupwinners.sh)

        Usage: ./$(basename "$0") [-bchwz] [-r YYYYMM-YYYYMM ] srcme_file

        input:        
            A source file, which set various vars for a set of runs (i.e. srcme_pfe_fcst)
            YYYYMM and YYYYMM range. Use hyphen for a delimiter

        options:
            -b  run with a debug mode (this will not execute ${strscr}.sh)
            -r  YYYYMM and YYYYMM range (format: YYYYMM-YYYYMM) 
            -c  clean unwanted files
            -h  show this help text
            -w  write stdout/err in a file
        
        Note 1: Delete message_${strscr}_* in stdout dir in order to redo process 
                for YYYYMM.
"

OPTIND=1
while getopts 'bchwr:' option; do
    case "$option" in
        r)  optr=true; 
            startyyyymm=$( echo $OPTARG | cut -d'-' -f1 );
              endyyyymm=$( echo $OPTARG | cut -d'-' -f2 );;
        b)  optb=true;rundebug=1;; 
        c)  clean_dir; exit 0;;
        h)  echo "$usage"; echo "$note"; exit 0;;
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

[[ -z  ${arrposarg[@]} ]] && die "an input is missing"
(( ${#arrposarg[@]} != 1 )) && die "1 input is required"
    
srcme=$( echo ${arrposarg[0]} | cut -d'_' -f2 )
[[ "$srcme" != "$hstshort" ]] && die "source file is not for this host (select srcme_${hstshort}_* file)"
#================================================================================
#                               Check User Inputs
#================================================================================
srcf=${arrposarg[0]}

source $srcf
inputcheck
#================================================================================
#                             Set Host Specific Vars
#================================================================================
#todo: check the name of system (discover or pfe) and set variables.

[[ -z $DFCST || ! -d $DFCST ]] && die "DFCST does not exist or is undefined" 
#================================================================================
#                                 Set Variables
#================================================================================
#mid
[[ -n $rstexpid ]] && strexpid=$strexpid$rstexpid

dfcst=$DFCST
  dmess=$cdir/message
dstdout=$cdir/stdout/$strscr/$strdout
   dout=$cdir/output/$strscr/$strdout

fmessage=$dmess/message_$strscr
fwin=winners_nino3.4_$strexpid
ftmp=${strscr}_tmp

totnummon=3

feadd=$cdir/mailadd
feaddextra=$cdir/mailadd_extra
bleaddextra=true
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

#todo:  add extra email address
if $bleaddextra ;then
    eadds_extra=$( echo $( misc_readfbyline $feaddextra ) | sed -e "s/ /;/g" )
    eadds="$eadds;$eadds_extra"
fi

RUNCHILD=1

[[ ! -f $strscr.sh ]] && die $strscr.sh does not exist
[[ ! -d $dmess     ]] && mkdir -p $dmess

#export DFCST RUNCHILD DARCH cdir
export RUNCHILD cdir
#================================================================================
#                                  Main Process
#================================================================================
#letsbegin
if $optr;then 
    #todo:  get dexp between beginning and end yyyymm
    yyyymm=$startyyyymm
    while (( $yyyymm <= $endyyyymm ));do
        arricyyyymm+=( $yyyymm )
        yyyy=$( echo $yyyymm | cut -c1-4 )
        mm=$( echo $yyyymm | cut -c5- )
        yyyymm=$( fcal_nextmonth $mm $yyyy )
    done
else
    #todo:  without options, this code will figureout the last two icyyyymm and up to 
    #       the current months
    nummon_pre=$(( $totnummon - 1 ))
    cyyyymm=$( date +%Y%m )
    arricyyyymm=( $cyyyymm ) 
    thisyyyymm=$cyyyymm
    for nummon in $( seq 1 $nummon_pre );do 
        yyyymm=$( fcal_previousmonth $thisyyyymm ) 
        arricyyyymm+=( $yyyymm )
        thisyyyymm=$yyyymm
    done
fi

arrfinal=($( filter ${arricyyyymm[@]} ))

#todo:  print which rst file will be transferd when opt b is selected. And
#       stops here. 
$optb && ahand_warr ${arrfinal[@]} && exit
(( ${#arrfinal[@]} == 0 )) && exit

msg_wheader
wmessage "Experimnets : $strexpid"
wmessage " Date Range : $startyyyymm - $endyyyymm"
wmessage "Total # of IC YYYYMM : ${#arrfinal[@]}"
wmessage "$( ahand_warr ${arrfinal[@]} )"


wmessage "Start processes:"
numnum=$( echo -n ${#arrfinal[@]} | wc -m )

for input in ${arrfinal[@]};do

       fmsg=message_${strscr}_${strexpid}_$input
    thismsg=$dmess/$fmsg
    numcomp=$( IndexOf $input ${arrfinal[@]} )  
    wmessage "    $(date +'%I:%M%P')    $(printf '%0'$numnum'g' $numcomp ) of $( printf '%0'$numnum'g' ${#arrfinal[@]} ) ... $input ..."

    if (( $writetofile_child == 1 ));then
        msg_newfile $thismsg
        ./$strscr.sh -i $input $srcf >> $thismsg 2>&1 
    else
        ./$strscr.sh -i $input $srcf
    fi

    #todo:  send email
    fthiswinners=$( find $dout/* -type f -name "${fwin}_${input}_*.txt" 2>/dev/null )
    fthisstdout=$dstdout/$fmsg
    [[ -f $fthisstdout ]] && sizefmsg=$( stat --print='%s' $dstdout/$fmsg ) || sizefmsg=0
    [[ -f $fthiswinners ]] && sizefwinners=$( stat --print='%s' $fthiswinners ) || sizefwinners=0


    if (( $sizefmsg > 0 && $sizefwinners > 0 ));then
        msg_subject="$strscr.$strexpid : $input Winners"

        #note:  writetofile has to be 0 to write msg_wheader into ftmp file
        writetofile_org=$writetofile
        (( $writetofile == 1 )) && writetofile=0
        msg_wheader >> $ftmp 2>&1
        (( $writetofile != $writetofile_org )) && writetofile=$writetofile_org

        cat $fthiswinners  >> $ftmp 2>&1
        cat $dstdout/$fmsg >> $ftmp 2>&1

        #todo:  find an attachement
        #       subsample_nino3.4_fcst_198201_198203.png
        strfplt=subsample_${reg}_${strexpid}_$input
        fplt=$( find $dout/* -maxdepth 0 -mindepth 0 -type f -name "${strfplt}_*.png" 2>/dev/null )

    elif [[ -f $thismsg ]];then
        msg_subject="$strscr.$strexpid : ALERT $input Winners"

        [[ ! -f $fthiswinners ]] && echo "!!!!!Winner file does not exist !!!" >> $ftmp 2>&1
        echo >> $ftmp 2>&1
        cat "$thismsg" >> $ftmp 2>&1
        echo >> $ftmp 2>&1
    fi

        
    if [[ -f $ftmp ]];then
        #msg_cyberpostman "$msg_subject" "$eadds" $ftmp

        if [[ -n $fplt ]];then
            #mutt -s "$msg_subject" -a $fplt -- "$eadds" < $ftmp &
            mutt -s "$msg_subject" -a $fplt -- "$eadds" < $ftmp 
            status_mutt=$?
        else
            #mutt -s "$msg_subject" "$eadds" < $ftmp &
            mutt -s "$msg_subject" "$eadds" < $ftmp 
            status_mutt=$?
        fi
        if (( $status_mutt == 0 ));then
            wmessage "Email - Success"
            [[ -f $dout/$fplt ]] && rm -f $dout/$fplt
            [[ -f $ftmp ]] && rm -f $ftmp 
        else
            wmessage "Email - FAILED"
        fi
    fi
done

exit

