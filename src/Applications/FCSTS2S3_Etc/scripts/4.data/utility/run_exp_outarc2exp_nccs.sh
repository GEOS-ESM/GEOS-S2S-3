#!/usr/bin/env bash
#================================================================================
#                                   Batch Jobs
#================================================================================
#SBATCH --time=02:00:00
#SBATCH --ntasks=1
#SBATCH --partition=datamove
#SBATCH --job-name=A2E_@FCSTDATE@ENSEMBLE_MEMBER
#SBATCH --account=g0609
#SBATCH --output=stderr_@FUNC_NAME

submit_me() {
    #description:   submit run_gcmarch.sh if archiving did not complete
    local _strsearch1="Pct Walltime Used"
    local _strsearch2="Total CPU-Time Allocated"
    local _strsearch3=": 00:00:"

    if [[ "$hstshort" == "dis" ]];then 

        local _cmdsubmit=/usr/slurm/bin/sbatch
        if [[ -n $cdir && -n $_cmdsubmit && ! -f $fcomp ]];then 
            cd $cdir
            #todo:  check walltime used for run_gcmarch.sh. If the script ran more 
            #       than 10 times with walltime used is < 1min, then stop submitting
            local numrun_lt1min=$( grep -i -w "walltime used" $ferr 2>/dev/null | grep -vE "${_strsearch1}|${_strsearch2}" 2>/dev/null | grep "${_strsearch3}" 2>/dev/null | wc -l )

            if (( $numrun_lt1min <= $lim_numrunlt1min ));then 
                $_cmdsubmit $thisscript 
            else
                wmessage "$( date )"
                wmessage "*Script stopped submitting itself due to a number of jobs that ran less than 1min (${numrun_lt1min})"

                touch $ffail

            fi
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
cdir=@DFCST/@FCSTDATE/@ENSEMBLE_MEMBER/archive
strscr=@FUNC_NAME
hstname=$( hostname )
blnode_nccs=false
if [[ ${hstname:0:3} == dis ]];then
    :
elif [[ "${hstname:0:4}" == borg || "${hstname:0:4}" == warp ]];then 
    #note:  this is for host other than pfe @ NCCS which has a name starting with 'borg' or 'warp'
    blnode_nccs=true
else 
    exit
fi

cd $cdir

flock=$cdir/${strscr}.lock
ftmp=$cdir/tmp_$strscr

#todo:  check tmp file size and create new if it is larger than 5kb
[[ ! -f $ftmp ]] && touch $ftmp

#todo:  write date & time when this script is executed.
if [[ -f $flock ]];then
    echo "$(TZ=America/New_York date +'%m/%d/%Y %H:%M' ) ... $flock exists..." >> $ftmp
else
    cntdate=$(TZ=America/New_York date +'%m/%d/%Y %H:%M' )
    echo "$cntdate" >> $ftmp
fi

#todo:  lock this script (ref: https://www.putorius.net/?p=2607)
if set -C; 2>/dev/null >$flock; then
    :
else
    exit
fi
    
trap 'clean_dir' EXIT

opts=true
optt=false
writetofile=0
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
    copy outputs from archive dir to experiment's collection dir. 

    Usage: ./$(basename "$0") [-chst]

    options:
        -c  delete unwanted files
        -s  skip submitting itself when exit
        -t  write table and exit (also skip submitting itself)
        -h  show this help text
"

while getopts ':hcst' option; do
    case "$option" in
        c)  clean_dir; exit;;
        s)  opts=false;;
        t)  optt=true;opts=false;;
        h)  echo "$usage"; exit 0;;
        \?) die "Invalid option: -$OPTARG" >&2
            exit 1;;
        :)  die "Option -$OPTARG requires an argument." >&2
            exit 1;;
    esac
done

#================================================================================
#                    Set Global Variables and Source Scripts
#================================================================================
#mid
thisscript=run_@FUNC_NAME.sh
scrfunc=@SCRFUNC
srcme=@SCRME

source $scrfunc $srcme

hstshort=$( get_host )
fcstdate=@FCSTDATE
ensm=@ENSEMBLE_MEMBER
dexp=@DFCST/$fcstdate/$ensm
darc=@DARCH/$fcstdate/$ensm
fhis2=$dexp/HISTORY_2.rc
ferr=$cdir/stderr_@FUNC_NAME
fcomp=$cdir/@FUNC_NAME_completed
blrsync=true

lim_numrunlt1min=3
    
[[ ! -f $fhis2 ]] && die "file missing: $fhis2" && exit
[[   -f $fcomp ]] && exit

if $opts;then 
    trap 'clean_dir;submit_me' EXIT
fi
#================================================================================
#                                  Main Process
#================================================================================
#letsbegin

exp_writetable $dexp $srcme
$optt && exit

arrcoll=($( exp_getcollections $fhis2 ))
arrcoll+=( MOM_Output restarts )

cd $dexp
    
cntcoll=1
for coll in ${arrcoll[@]};do 
    wmessage "  $( date +'%I:%M%P' )    $( printf "%02g" $cntcoll ) of $( printf "%02g" ${#arrcoll[@]}) ... $coll ..."

    if $blrsync;then 
        rsync -avzq dirac:$darc/$coll/* $dexp/$coll/ 2>/dev/null
    else
        archive_status=($( bbscp -r3 -z dirac:$darc/$coll/* $dexp/$coll/ 2>/dev/null )) 
        status_bbscp=${archive_status[3]}

        (( $status_bbscp != "OK" )) && wmessage "                              COPY FALIED"
    fi

    cntcoll=$(( cntcoll + 1 ))
done

#note:  assume transfer has been completed if script reaches here.
[[ ! -f $fcomp ]] && touch $fcomp
cd - >/dev/null

exit


