#!/usr/bin/env bash 

clean_dir() {
    [[ -n $flock && -f $flock ]] && rm -f $flock
    return
}

#================================================================================
#                                     Begin
#================================================================================
#beg
hst=$( hostname )
blnode=false
if [[ "$hst" =~ "pfe"* || "$hst" =~ "lfe"* ]];then
    :
elif [[ "$hst" =~ "r"* ]];then
    #note:  this is for host other than pfe @ NAS which has a name starting with 'r'
    blnode=true
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
writetofile=0
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        execute several scripts in order to start cleaning process.
        - Run run_clean with debug mode and send a list of experiments ready for cleaning
        - Run run_archstatus for all months in the given year
        - Run run_monitor to update monitor stdout

        Usage: ./$(basename "$0") [-hw] YYYY-YYYY srcme_file
        input:        
            YYYY range; Years to clean (format: YYYY-YYYY) 
            A source file, which set various vars for a set of runs (i.e. srcme_pfe_*)

        options:
            -h  show this help text
            -w  write stdout/err in a file
"
OPTIND=1
while getopts 'hw' option; do
    case "$option" in
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
#================================================================================
#                               Check User Inputs
#================================================================================
i=0
userbegyyyy=$( echo ${arrposarg[i]} | cut -d'-' -f1 )
userendyyyy=$( echo ${arrposarg[i]} | cut -d'-' -f2 ); i=$(( i + 1 ))
scrf=${arrposarg[i]}; i=$(( i + 1 ))
[[ -n $scrf && -f $scrf ]] && source $scrf
#inputcheck
#================================================================================
#                                 Set Variables
#================================================================================
#mid
hstshort=$( get_host )
cdate=$( date +%Y%m%d_%H%M )

dmess=$cdir/message

fmessage=$dmess/message_${strscr}_$strexpid
fsub=$cdir/data/submit/data_submit_fcst_${strscr}_${cdate}

msg_subject="${hstshort}.${strscr}: $strexpid"

feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

maxsec=900
maxmin=$( echo "$maxsec / 60" | bc  )
limscr=5

RUNCHILD=true
export RUNCHILD

#================================================================================
#                                  Main Process
#================================================================================
#main
write_numcleaned_dexp
 
arryyyy=($( seq $userbegyyyy $userendyyyy ))
for yyyy in ${arryyyy[@]};do
    msg_wheader "$yyyy"

    wmessage "Run Clean with Debug"
    wmessage

    #todo:  find which dexp can be cleaned. Opt b create a data file data/clean/data_clean_debug_ready2clean.
    ./run_clean.sh -w -b -r ${yyyy}01-${yyyy}12 $scrf  >> $SCRIPTPATH/message/stderr_clean  2>&1

    #todo:  get necessary data
    arrdexp=($( find $SCRIPTPATH/message/* -type f -name "message_monitor_fcst_${yyyy}*" | xargs -i bash -c "grep "'"'"exp location"'"'" {} | rev | cut -d' ' -f1 | rev" | sort -V )) 
    arryyyymm=($( printf '%s\n' ${arrdexp[@]} | rev | cut -d'/' -f2 | rev |cut -c1-6 | sort -V | uniq ))

    #[[ ! -f archstatus.lock ]] && : || continue 

    wmessage "Run Archstatus ( total = ${#arryyyymm[@]} )"
    wmessage

    #todo:  run archstatus 
    for yyyymm in ${arryyyymm[@]};do 
        wmessage "    Running $yyyymm"
        ./run_archstatus.sh -w -y $yyyymm $scrf >> $SCRIPTPATH/message/stderr_archstatus 2>&1   
    done 

    wmessage
    wmessage "Update message_monitor_${strexpid}_$yyyy ( total = ${#arrdexp[@]} )"
    wmessage

    #todo:  run monitor.sh
    cnt_dexp=0
    for dexp in ${arrdexp[@]};do
   
        cnt_dexp=$(( cnt_dexp + 1 ))
        set_rstfcstdate $dexp
        
        #todo:  count number of screen & processes
        numscr=$( screen -ls | grep -i detached | grep ${strscr}_monitor_${strexpid}_ | wc -l )
        numproc=$( ps aux | awk '{ print $1 }' | grep -w $USER | wc -l )

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
    
                wmessage "    $( printf "%+3s of %+3s\n" $cnt_dexp ${#arrdexp[@]} ) $dexp     "

                #todo:  run scrchild
                screen -dmS ${strscr}_monitor_${strexpid}_$fcstdate$ensm bash -c "./monitor.sh -w -f $fsub $scrf $dexp >> $cdir/message/stderr_monitor 2>&1"  

            else
                sec2=$( date +%s )
                sec_diff=$(( sec2 - sec1 ))
                totsec=$(( sec2 - sec0 ))
                totmin=$( echo "$totsec / 60" | bc )
    
                #todo:  break out one loop (hence, 2)
                (( $totsec >= $maxsec )) && break 2
    
                if ! $blinitialnote;then 
                    wmessage "$( date +'%m/%d/%Y %H:%M' )     Running Screen - $( printf '%+2s\n' $numscr ) and Processes - $( printf '%+2s\n' $numproc ) ... will wait for max of $maxmin minutes    "
                    wmessage "$( date +'%m/%d/%Y %H:%M' )     Running Screen - $( printf '%+2s\n' $numscr ) and Processes - $( printf '%+2s\n' $numproc ) ... waited for $totmin min    "
                    blinitialnote=true
                elif (( $sec_diff > 60 ));then
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
            arrscr=($( screen -ls | grep -i detached | grep ${strscr}_monitor_${strexpid}_ | tr -d '\t' | cut -d'(' -f1 ))
            printf '%s\n' ${arrscr[@]} | xargs -i bash -c 'screen -XS {} quit' 2>/dev/null
    
            wmessage "Killed Screens:"
            ahand_warr ${arrscr[@]} 
            break 2 
        fi
    
        if ! $blinitialnote ;then 
            wmessage "$( date +'%m/%d/%Y %H:%M' )     Running Screen - $( printf '%+2s\n' $numscr ) ... will wait for $maxmin minutes at max    "
            blinitialnote=true
    
        elif (( $sec_diff > 60 ));then
            wmessage "$( date +'%m/%d/%Y %H:%M' )     Running Screen - $( printf '%+2s\n' $numscr ) ... waited for $totmin min    "
            sec1=$( date +%s )
        fi
        
        numscr=$( screen -ls | grep -i detached | grep ${strscr}_monitor_${strexpid}_ | wc -l )
    
    done

    wmessage "Running Screens:"
    wmessage "$( screen -ls | grep monitor )"
done 

#todo:  send email
if [[ -f $fmessage ]];then
    sizef=$( stat --print='%s' $fmessage )
    if (( $sizef > 0 ));then
        msg_cyberpostman "$msg_subject" "$eadds" $fmessage
        (( $? == 0 )) && rm -f $fmessage
    fi
fi


