#!/usr/bin/env bash

inputcheck(){
    
    local srcme=$( basename $srcf | cut -d'_' -f2 )
    [[ "$srcme" != "$hstshort" ]] && die "source file is not for this host (select srcme_${hstshort}_* file)"

    [[ -z $dexp ]] && die "a full path to an experiment dir is a required input"
    [[ -n $dexp ]] && [[ ! -d $dexp ]] && die "exp dir does not exist"

    return
}

msg2dstdout() {
    #description:   move message_monitor_* to stdout/monitor/* dir when exp completed
    #todo:  delete existing stdout files
    local numfmessage=$( find $dstdout/* -maxdepth 0 -type f -name "${fmessage_basename}_${nummon_run}mon_*" 2>/dev/null | wc -l )
    (( $numfmessage > 0 )) && rm -f $dstdout/${fmessage_basename}_${nummon_run}mon_*

    #todo:  archive message file
    [[ -f $fmessage ]] && mv $fmessage $dstdout/${fmessage_basename}_${nummon_run}mon_$icdate_yyyymm$newensm
    return
}

write_msg4incomplete(){
    #description:   write fmessage if exp is incomplete
    #!!!!! WORK ON BLRERUN (08/06/2021) !!!!!
    #note:  fmessage is defined here and this is only for unfinished exp
    local _strstatus=$1
    local writetofile=1
    #local fmessage=$dmess/${fmessage_basename}_${nummon_run}mon_$icdate_yyyymm${newensm}_$_strstatus
    local arrstatus1=( R KR WR WKR RdyS WRdyS AR WAR )
    local arrstatus2=( Sub WSub UnfR WUnfR Pnc WPnc)
    local arrstatus3=( PUnf WPUnf AStK WAStK AStuck WAStuck AZero WAZero AUnf WAUnf AUnk WAUnk )
    local blexit=false

   
    if [[ "${arrstatus2[@]}" =~ "$_strstatus" || "${arrstatus3[@]}" =~ "$_strstatus" ]];then
        msg_wheader_userdefined 80 = "$(TZ=America/New_York date +'%m/%d/%Y %H:%M' ) $fcstdate/$ensm"
        wmessage " exp location : $dexp"
        wmessage "arch location : $darch"
        wmessage
        wmessage "cap_restartIC : $capric_yyyymmdd" 
        wmessage "  cap_restart : $capr_yyyymmdd" 
        wmessage "      cap_end : $realend_yyyymmdd"
        wmessage
        count_files $dexp     
        write_table $dexp ${#arrfmiss[@]} 
        write_table_collection $dexp ${arrfsave[@]} 
        wmessage
        [[ -f $farchcomp ]] && wmessage $( stat --print="%y %n" $farchcomp )
        [[ -f $farchdel  ]] && wmessage $( stat --print="%y %n" $farchdel )
        wmessage

        if [[ "${arrstatus2[@]}" =~ "$_strstatus" ]];then
            #todo:  if this exp is already in data_submit_*_resubmit_* file
            fsub_exist=$( grep -l -x $dexp $dtmpdata/* 2>/dev/null )
    
            if [[ -n $fsub_exist ]];then
                wmessage "exp path is in $fsub_exist"
            else
                wmessage "exp path added to $fsub"
                echo $dexp >> $fsub
            fi
            
            #mv $fmessage ${fmessage}_fixed
    
        elif [[ "${arrstatus3[@]}" =~ "$_strstatus" ]]  ;then
            :
            #todo:  if this exp is already in data_submit_*_resubmit_* file
            #fsub_exist=$( grep -l -x $dexp/archive $dtmpdata/* 2>/dev/null )
    
            #if (( $num_rgrn > 0 ));then
            #    mv $fmessage ${fmessage}_exprunning
            #else
            #    if [[ -n $fsub_exist ]];then
            #        echo "exp archive path is in $fsub_exist" >> $fmsghere
            #    else
            #        echo "exp path added to $fsub" >> $fmsghere
            #        echo $dexp/archive >> $fsub
            #    fi
            #    mv $fmsghere ${fmsghere}_fixed
            #fi
        fi

        blexit=true
    else
        msg_wheader_userdefined 80 = "$(TZ=America/New_York date +'%m/%d/%Y %H:%M' ) $fcstdate/$ensm"
        wmessage " exp location : $dexp"
        wmessage
        wmessage "cap_restartIC : $capric_yyyymmdd" 
        wmessage "  cap_restart : $capr_yyyymmdd" 
        wmessage "      cap_end : $realend_yyyymmdd"
        wmessage
        count_files $dexp     
        write_table $dexp ${#arrfmiss[@]} 
        write_table_collection $dexp ${arrfsave[@]} 
        wmessage
        [[ -f $farchcomp ]] && wmessage $( stat --print="%y %n" $farchcomp )
        [[ -f $farchdel  ]] && wmessage $( stat --print="%y %n" $farchdel )
        
        if [[ "${arrstatus1[@]}" =~ "$_strstatus" ]];then
            blexit=true
        else
            blexit=false
        fi
    fi

    writetofile=0
    echo "$blexit:$fmessage"
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
    :
elif [[ "$hst" =~ "discover"* ]];then
    echo "this script works only on pfe (for now)";exit
else
    exit
fi

strscr=$(basename "$0" | cut -d'.' -f1 )
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cdir=$SCRIPTPATH

[[ -z $cdir || ! -d $cdir ]] &&  echo "cdir (parent directory of this script) is undefined"  && exit

cd $cdir

[[ -z $strscr ]] && strscr=$(basename "$0" | cut -d'.' -f1 )

#todo:  check if this script is invoked as child script
[[ -n $RUNCHILD ]] && blchild=true || blchild=false

if ! $blchild;then
    flock=$cdir/${strscr}.lock
    ftmp=$cdir/stdout/rundatetime/tmp_$strscr
    
    if [[ ! -f $ftmp ]];then install -D /dev/null $ftmp;fi
    
    #todo:  check tmp file size and create new if it is larger than 5kb
    stmp=$( find $ftmp -printf "%s\n" )
    (( $stmp > 5000 )) && rm -f $ftmp && touch $ftmp
    
    #todo:  lock this script
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
fi
trap clean_dir EXIT

if [[ ! $cdir/func_fcst.sh ]];then 
    echo "$cdir/func_fcst.sh does not exist";exit
else
    source $cdir/func_fcst.sh
fi

hstshort=$( get_host )
writetofile=0
optl=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        determine if forecast experiments are completed at 3 months or 10 months.
        Use option f to provide data_submit_* file name. Unfinished exp will be 
        written in the file and run_submit.sh will re-submit them.
        
        Usage: ./$(basename "$0") [-hw] [-f data_submit_* file] srcme_file expdir

        input:
            A source file, which set various vars for a set of runs (i.e. srcme_pfe_fcst)
            a full path to an experiment dir

        options:
            -f  data_submit_* file name 
            -l  write details including existing outputs on archive host (Default: false)
            -h  show this help text
            -w  write stdout/err in a file
"

OPTIND=1
while getopts ':hwlf:' option; do
    case "$option" in
        f)  fsub=$OPTARG;; 
        l)  optl=true;; 
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

[[ -z ${arrposarg[@]} ]] && die "inputs are missing"
(( ${#arrposarg[@]} != 2 )) && die "2 inpus are required"
#================================================================================
#                               Check User Inputs
#================================================================================
srcf=${arrposarg[0]}
dexp=${arrposarg[1]}
inputcheck

source $srcf
[[ -z $DARCH ]] && die "DARCH is undefined or does not exist"
#================================================================================
#                                 Set Variables
#================================================================================
#mid
cdate=$( date +%Y%m%d_%H%M )
[[ -n $rstexpid ]] && strexpid=$strexpid$rstexpid

 dstdout=$cdir/stdout/monitor/$strdout
 dstatus=$cdir/output/getstatus/$strdout
   ddata=$cdir/output/pckwinners/$strdout
dtmpdata=$cdir/data/submit
   dmess=$cdir/message
   dhold=holding
strscr_arch=gcmarch
    strflst=data_getstatus_${strexpid}_list_

farchcomp=$dexp/archive/${strscr_arch}_archcompleted
 farchdel=$dexp/archive/${strscr_arch}_deloutcompleted
farchlock=$dexp/archive/${strscr_arch}.lock

fhis2=HISTORY_2.rc
#fpanic=$dexp/PANICSTOP

#bl10mon=false
#blarchcomp=false
#blarchdel=false

#todo:  exit if there is no holding dir.
[[ ! -d $dexp/$dhold ]] && exit

set_rstfcstdate $dexp

darch=$DARCH/$fcstdate/$ensm

icdate_yyyymm=$( date -d $fcstdate +%Y%m )
strat_yyyymm=$icdate_yyyymm
for x in {1..2};do
    strat_yyyymm=$( fcal_nextmonth $strat_yyyymm)
done

fmessage=$dmess/message_${strscr}_${strexpid}_${fcstdate}${ensm}

fmessage_basename=$( basename $fmessage )
flst_lfe=$cdir/${strscr}_${strexpid}_$fcstdate${ensm}_lfefiles
flockarch=$dexp/archive/gcmarch.lock

#winners_nino3.4_cice_202105_202107.txt
#fwinner=winners_${reg}_${strexpid}_${icyyyymm}_${strat_yyyymm}.txt
fwinner=$( find $ddata/* -maxdepth 0 -name "winners_*_${strexpid}_${icdate_yyyymm}_${strat_yyyymm}.txt" 2>/dev/null)

[[ -z $collsst ]] && collsst=sst_tavg_1dy_glo_L720x361_slv

#todo:  get newensemble member if exp is winner. 
declare -A assoc_winensm
[[ -f $fwinner ]] && convert2winnerensm $fwinner

#for x in "${!assoc_winensm[@]}";do wmessage $x ${assoc_winensm[$x]};done;exit
##printf '%s\n' "${!assoc_winensm[@]}" ${assoc_winensm[@]} | pr -t2


#todo:  remove all fmessage* files
find $dmess/* -maxdepth 0 -type f -name "${fmessage_basename}*" -delete 2>/dev/null 

#todo:  check if dexp is a ensemble winner
blwinner=$( exp_checkwinner $dexp $ddata )

#todo:  check RERUN setting in gcm_run.j
blrerun=$( grep -i rerun $dexp/gcm_run.j | head -1 |rev | cut -d' ' -f1 | rev | tr '[:upper:]' '[:lower:]' )

if $blwinner;then
    fcstensm=$fcstdate/$ensm
    newensm=${assoc_winensm[$fcstensm]}
else
    newensm=""
fi

[[ ! -d $dstdout ]] && mkdir -p $dstdout
[[ ! -d $ddata ]] && mkdir -p $ddata

numfstdout=$( find $dexp/* -maxdepth 0 -mindepth 0 -type f -name "R${icyyyymmdd}ens${intens}.o[0-9]*" -o -name "R${icyyyymmdd}ens$x.*.out" 2>/dev/null| wc -l )
 numfready=$( find $dexp/* -maxdepth 0 -mindepth 0 -type f -name gcmsetup_setupready 2>/dev/null | wc -l )
            
#todo:  set various variables
setvars $dexp
cnt_jobs $dexp

blmonmiss=false
blarchdone_nomarker=false

#todo:  delete message file in stdout dir
#note:  this does not delete message_monitor_*_3mon_* or *_10mon_*
find $dstdout/* -maxdepth 0 -type f -name "${fmessage_basename}_${nummon_run}mon_*" -delete 2>/dev/null 

#todo:  check unfinished exp
strstatus=$( grep -w $dexp $dstatus/$strflst* 2>/dev/null | cut -d':' -f2 | cut -d' ' -f1 ) 
[[ -z $strstatus ]] && strstatus=$( exp_status $dexp )
[[ -z $strstatus ]] && strstatus="unknownstatus" 
thisoutput=$( write_msg4incomplete $strstatus )
blexit=$( echo $thisoutput | cut -d':' -f1 )
thisfmessage=$( echo $thisoutput | cut -d':' -f2 )
if $blexit && (( $writetofile == 0 )); then 
    cat $thisfmessage
    exit
fi

if [[ "$fmessage" != "$thisfmessage" ]];then 
   fmessage=$thisfmessage 
fi


##note:  keep this here. this is fmessage for completed exp
#fmessage=$dmess/${fmessage_basename}_${icdate_yyyymm}$newensm
#msg_newfile $fmessage

#todo:  check if gcmarch is running
[[ -f $farchlock ]] && blarchlock=true || blarchlock=false
#================================================================================
#                                  Main Process
#================================================================================
#letsbegin
msg_wheader_userdefined 80 = "$(TZ=America/New_York date +'%m/%d/%Y %H:%M' )"

#tood:  get a file with all output file name in archive dir on lfe
getlfefout $dexp $darch $flst_lfe 

wmessage
wmessage " exp location : $dexp"
wmessage "arch location : $darch"
wmessage
wmessage "cap_restartic                     : $capric_yyyymmdd"
wmessage "cap_restart                       : $capr_yyyymmdd"
wmessage "3month run end                    : $end3_yyyymmdd"
wmessage "winner run end                    : $realend_yyyymmdd"
wmessage "total # of months exp ran         : $nummon_run"
wmessage
wmessage "a number of collections           : ${#arrcoll[@]} (execludes $collsst)"
wmessage "gcmarch is running                : $blarchlock"
wmessage

count_files $dexp $flst_lfe
write_table $dexp ${#arrfmiss[@]} 
$blleaveout && write_table_collection $dexp ${arrfsave[@]} 

wmessage

if $blarchlock;then
    wmessage "run_gcmarch.sh lock timestamp:"

    #+++++ cd to dexp/archive (start) +++++
    cd $dexp/archive
    wmessage "$( stat --print="%y %n" $strscr_arch.lock )"
    cd - >/dev/null
    #+++++ cd to dexp/archive ( end ) +++++
fi

wmessage

#strstatus=$( exp_status $dexp $flst_lfe )
if [[ "$strstatus" == "C3" || "$strstatus" == "C3m" ]];then
    wmessage "3-MONTH RUN COMPLETED"
    wmessage
elif [[ "$strstatus" == "WC10" ]];then
    wmessage "10-MONTH RUN COMPLETED"
    wmessage
else
    wmessage "Currently it is at $nummon_run months"
    wmessage
fi

if $optl;then 
    wmessage
    wmessage "+++++ More Details +++++"
    wmessage
    
    #todo:  list outputs
    coll=$collsst
    #getpfefout_formatted $dexp $coll "${arrfsst[@]}"
    getpfefout_formatted $dexp $coll
    
    #todo:  get a list of outputs on lfe
    if (( $capr_yyyymmdd <= $end3_yyyymmdd ));then
        nummon=3
    elif (( $capr_yyyymmdd == $realend_yyyymmdd )) || (( $capr_yyyymmdd > $end3_yyyymmdd ));then
        nummon=10
    fi
    
    thisoutput=$( getlfefout_formatted $dexp $darch $nummon )
    foutlist=$( echo $thisoutput | cut -d':' -f1 )
    blfoutmiss=$( echo $thisoutput | cut -d':' -f2 )
    
    if [[ -f $foutlist ]];then
        if (( $writetofile == 1 ));then
            cat $foutlist >> $fmessage 
        else
            cat $foutlist
        fi
        wmessage 
    
    elif [[ ! -f $foutlist ]] ;then 
        wmessage "exit @$LINENO"
        mv $fmessage $dmess/${fmessage_basename}_${nummon_run}mon_$icdate_yyyymm${newensm}_${strstatus}_MonitorFailed
        exit
    fi
else
    blfoutmiss=false
fi


[[ -f $farchcomp ]] && wmessage "$farchcomp exists."
[[ -f $farchdel  ]] && wmessage "$farchdel exists."

#todo:  move message files 
if ! $blfoutmiss && [[ "$strstatus" == "C3" || "$strstatus" == "C3m" || "$strstatus" == "WC10" ]];then
    msg2dstdout 

elif $blfoutmiss && [[ "$strstatus" == "C3" || "$strstatus" == "C3m" || "$strstatus" == "WC10" ]];then
    mv $fmessage $dmess/${fmessage_basename}_${nummon_run}mon_$icdate_yyyymm${newensm}_${strstatus}_lfemissout

elif [[ "$strstatus" == "AStK" || "$strstatus" == "WAStK" ]];then
    #todo:  remove lock file
    [[ -f $flockarch ]] && rm -f $flockarch 
    mv $fmessage $dmess/${fmessage_basename}_${nummon_run}mon_$icdate_yyyymm${newensm}_${strstatus}_fixed

elif [[ "$strstatus" == "RdyS" || "$strstatus" == "WRdyS" ]];then
    mv $fmessage $dmess/${fmessage_basename}_${nummon_run}mon_$icdate_yyyymm${newensm}_${strstatus}

    #note:  (W)Sub is fixed in write_msg4incomplete. When status is checked for the second time, status
    #       comes out as (W)RdyS. Therefore, there will be two message_monitor in dmessage
    #fmessage=$dmess/message_${strscr}_${strexpid}_${fcstdate}${ensm}
    rm -f ${fmessage}*_fixed 2>/dev/null

else
    mv $fmessage $dmess/${fmessage_basename}_${nummon_run}mon_$icdate_yyyymm${newensm}_${strstatus}
fi

#clean up    
[[ -f $foutlist ]] && rm -f $foutlist

