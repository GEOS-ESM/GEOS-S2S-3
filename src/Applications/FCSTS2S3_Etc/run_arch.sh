#!/usr/bin/env bash


inputcheck() {
    (( ${#arrposarg[@]} != 1 )) && die "one input is required"
    return
}

filter_arch() {
    
    local _arrinput=( "$@" )
    local cntbug=0
    local arrjobs=()
    local _strpbs_jname=Job_Name
    local _strpbs_output=Output_Path
    local arr=()
    local _dexparch jobid

    if $optb;then
        local _numfilter=$( grep "debug_filter" $0 | sed 's#^ *##g'| grep -Ev '^#|^##|grep' | wc -l ) 
        wmessage "Number of Filter: $_numfilter" 
    fi

    #todo:  get all running screen sessions
    #local _arrscr=($( screen -ls | tr -d '\t' | grep -iE "detached\|${strscr}_${strexpid}_" | cut -d'(' -f1 | cut -d'.' -f2- | cut -d'_' -f3 | sed 's#ens#/ens#g' | xargs -i printf "$DFCST/%s/archive\n" {} ))
    local _arrscr=()

    #todo:  check if clean_completed marker exists
    local _arrinput=($( printf '%s\n' ${_arrinput[@]} | xargs -i bash -c "[[ ! -f {}/clean_completed ]] && echo {}" ))
    
    debug_filter ${_arrinput[@]}

    #todo:  check if gcmarch_archcompleted & gcmarch_deloutcompleted exist
    #local _arrinput=($( printf '%s\n' ${_arrinput[@]} | xargs -i dirname {} | xargs -i bash -c "[[ -f {}/$fcomp && -f {}/$fdel ]] && :  || echo {}/archive" ))
    local _arrinput=($( printf '%s\n' ${_arrinput[@]} | xargs -i bash -c "[[ ! -f {}/$fcomp ]] && echo {}" | xargs -i bash -c "[[ ! -f {}/$fdel ]] && echo {}" ))
    
    debug_filter ${_arrinput[@]}

    #todo:  check if clean_completed exists
    local _arrinput=($( printf '%s\n' ${_arrinput[@]} | xargs -i dirname {} | xargs -i bash -c "[[ ! -f {}/$fmark_clean ]] && echo {}/archive" ))
    
    debug_filter ${_arrinput[@]}

    #todo:  check if screen is running for dexp
    if (( ${#_arrscr[@]} > 0 ));then 
        local _arrinput=($( printf '%s\n' ${_arrinput[@]} ${_arrscr[@]} | sort -V | uniq -u ))

        debug_filter ${_arrinput[@]}
    fi

    local cntbug_org=$cntbug
    for _dexparch in ${_arrinput[@]};do

        cntbug=$cntbug_org
        local _dexp=$( dirname $_dexparch )
        set_rstfcstdate $_dexp

        #todo:  get job names
        if $blrstexpid;then
            #local     arrjobs=($( $_qstat -u $USER -W fmt_Queue="-maxw 40" | grep $fcstdate$ensm  2>/dev/null | grep -v P$fcstdate$ensm | sort -V -k4 -t' ' | tr -s '[:space:]' | cut -d' ' -f4 | cut -d'.' -f1 | sort -V | uniq | grep ens ))
            local arrjobspst1=($( $_qstat -u $USER -W fmt_Queue="-maxw 40" | grep $fcstdate$ensm 2>/dev/null | cut -d' ' -f1 ))

            for jobid in ${arrjobspst1[@]};do
                #local thisdarc=$( $_qstat -f $jobid 2>/dev/null | grep -i $_strpbs_output | rev | cut -d':' -f1 | rev | xargs -i dirname {} | sed 's#post#archive#g')
                #local thisdexp=$( $_qstat -f $jobid 2>/dev/null | grep -i $_strpbs_output | rev | cut -d':' -f1 | rev | xargs -i dirname {} | xargs -i dirname {} )
                local thisdexp=$( $_qstat -f $jobid 2>/dev/null | grep -i $_strpbs_output | rev | cut -d':' -f1 | rev | xargs -i dirname {} )

                #if [[ "$thisdarc" == "$_dexparch" ]];then 
                if [[ "$thisdexp" == "$_dexp" ]];then 
                    local thisjname=$( $_qstat -f $jobid 2>/dev/null | grep -i $_strpbs_jname | rev | cut -d' ' -f1 | rev )
                    arrjobs+=( $thisjname )
                fi
            done
        else
            local arrjobs=($( $_qstat -u $USER -W fmt_Queue="-maxw 40" | grep R$fcstdate$ensm | sort -V -k4 -t' ' | tr -s '[:space:]' | cut -d' ' -f4 | cut -d'.' -f1 | sort -V | uniq | grep ens ))
        fi

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
        local blwinner=$( exp_checkwinner $_dexp $ddata )

        if $blwinner || $blrerun ;then
            local expenddate=$realend_yyyymmdd
        else
            local expenddate=$end3_yyyymmdd
        fi

        #todo:  check if cap_restart is = to end yyyymm (regardless of winner or not ) 
        if $blwinner;then
            (( $expenddate != $capr_yyyymmdd )) && continue
        elif $blrerun ;then
            #note:  For exp with rerun=true, start archiving once cap_restart reaches to 3month end. 
            (( $end3_yyyymmdd > $capr_yyyymmdd )) && continue
        else
            (( $expenddate > $capr_yyyymmdd )) && continue
        fi

        debug_filter $_dexparch

        ##todo:  check there is diurnal files, which indicate that gcm_post.j has run.
        #local arrfdiu=($( find $_dexp/* -maxdepth 1 -mindepth 1 -type f -name '*.diurnal.*' 2>/dev/null )) 
        #(( ${#arrfdiu[@]} == 0 )) && continue

        #debug_filter $_dexparch

        #todo:  check if gcmarch_archoutcompleted exists. If so, check it is newer than diurnal files.
        local arrfdiu=($( find $_dexp/* -maxdepth 1 -mindepth 1 -type f -name '*.diurnal.*' 2>/dev/null )) 
        local farchcomp=$_dexparch/$fcomp
        
        if [[ -f $farchcomp ]] && (( ${#arrfdiu[@]} > 0 ));then
            local arrfdiu_newer=($( printf '%s\n' ${arrfdiu[@]} | xargs -i bash -c "[[ {} -nt $farchcomp ]] && echo {}"  ))
                
            if (( ${#arrfdiu_newer[@]} == 0 ));then 
                continue 
            fi
        fi

        debug_filter $_dexparch

        #todo:  check if gcmarch_deloutcompleted exists. If so, check it is newer than diurnal files.
        local farchdel=$_dexparch/$fdel
        
        if [[ -f $farchdel ]] && (( ${#arrfdiu[@]} > 0 ));then
            local arrfdiu_newer=($( printf '%s\n' ${arrfdiu[@]} | xargs -i bash -c "[[ {} -nt $farchdel ]] && echo {}"  ))
                
            if (( ${#arrfdiu_newer[@]} == 0 ));then 
                continue 
            fi
        fi

        debug_filter $_dexparch

        #todo:  check gcm_run.j is running 
        printf '%s\n' ${arrjobs[@]} | grep -w R$fcstdate$ensm >/dev/null
        local status_grep=$?
        (( $status_grep == 0 )) && continue
        
        debug_filter $_dexparch

        local runhere=false
         if $runhere;then 
            #todo:  check mini_gcm_run.j is running
            printf '%s\n' ${arrjobs[@]} | grep -w M$fcstdate$ensm >/dev/null
            local status_grep=$?
            (( $status_grep == 0 )) && continue

            debug_filter $_dexparch

            #todo:  check run_gcmarch.sh is running
            printf '%s\n' ${arrjobs[@]} | grep -w A$fcstdate$ensm >/dev/null
            local status_grep=$?
            (( $status_grep == 0 )) && continue

            debug_filter $_dexparch
        fi


        #todo:  check gcm_post.*.j is running
        printf '%s\n' ${arrjobs[@]} | grep -w P$fcstdate$ensm >/dev/null
        local status_grep=$?
        (( $status_grep == 0 )) && continue

        debug_filter $_dexparch

        if ! $blrsync;then 
            #todo:  check if # of running shiftc is more than the limit set in run_gcmarch.sh
            [[ ! -f $_dexparch/run_gcmarch.sh ]] && exp_updarchscr $arcqid $_dexp

            local _limsft=$( grep "limsftc=" $_dexparch/run_gcmarch.sh 2>/dev/null | rev | cut -d'=' -f1 | rev )
            local _numsft=$( printf '%s\n' ${arrsftid_all[@]} | xargs -i bash -c "/usr/local/bin/shiftc --id={} --status=csv | grep run | grep lou | grep GEOS_fcst/$fcstdate/$ensm | head -1" 2>/dev/null | wc -l ) # | cut -d',' -f3 |cut -d':' -f2 | rev | cut -d'/' -f3-4 | rev | sort -V | uniq " ))

            (( $_numsft >= $_limsft )) && continue

            debug_filter $_dexparch
        fi

        arr+=( $_dexparch )
    done

    echo ${arr[@]} 
}

sort_darch(){
    #description:   prioritize dexarchive
    local _blwin=$1;shift
    local _arrdarch=( "$@" ) 
    local dout_pckwinners=$cdir/output/pckwinners/$strdout
    #winners_nino3.4_fcst_198112_198202.txt
    local strfwin=winners_${reg}_${strexpid}_
    local arrdwin=()
    local _arr=()
    local yyyymm

    local _arryyyymm=($( printf "%s\n" ${_arrdarch[@]} | rev | cut -d'/' -f3 | rev | cut -c1-6 | sort -V | uniq )) 

    for yyyymm in ${_arryyyymm[@]};do 
        arrdwin+=($( cat $dout_pckwinners/${strfwin}${yyyymm}_[0-9]*.txt | sort -V | sed "s# #/#g" | xargs -i echo $DFCST/{}/archive ))
    done
  
    local arrdwin_arch=($( printf '%s\n' ${_arrdarch[@]} ${arrdwin[@]} | sort -V | uniq -d ))
    local arrd3mon_arch=($( printf '%s\n' ${_arrdarch[@]} ${arrdwin_arch[@]} | sort -V | uniq -u ))
   
    if $_blwin; then
        _arr=( ${arrdwin_arch[@]} ${arrd3mon_arch[@]} )
    else
        _arr=( ${arrd3mon_arch[@]} ${arrdwin_arch[@]} )
    fi
    echo ${_arr[@]}
}

sendmsg() {

    [[ -f $ferr     ]] && local sizeferr=$( stat --print='%s' $ferr )  || local sizeferr=0
    [[ -f $fmessage ]] && local sizef=$( stat --print='%s' $fmessage ) || local sizef=0

    if (( $sizeferr > 0 || $sizef > 0 ));then 
        if (( $sizef > 0 ));then
            msg_wheader_userdefined 40 "-" $( basename $ferr ) 
            wmessage "$( cat $ferr )"
            blrm=true
        fi

        msg_cyberpostman "$msg_subject" "$eadds" $fmessage $blmsgmutt
        local status_email=$?
wmessage \@$LINENO status_email = $status_email
wmessage \@$LINENO blsavefmsg = $blsavefmsg

        if $blsavefmsg;then
            mv $fmessage $dstdout/$fmsg_fname
        elif ! $blsavefmsg;then
            (( $status_email == 0 )) && rm -f $fmessage
        fi
        
        $blrm && msg_newfile $ferr
    fi
}


clean_dir() {
    [[ -f *.pyc ]] && rm -f *.pyc
    [[ -n $flock && -f $flock ]] && rm -f $flock
    [[ -n $ftmp1 && -f $ftmp1 ]] && rm -f $ftmp1
    [[ -n $ftmp2 && -f $ftmp2 ]] && rm -f $ftmp2
    return
}

#================================================================================
#                                     Begin
#================================================================================
#starthere
hst=$( hostname )
blnode=false
if [[ "$hst" =~ "pfe"* || "$hst" =~ "lfe"* ]];then
    :
elif [[ "$hst" =~ "r"* ]];then
    #note:  this is for host other than pfe @ NAS which has a name starting with 'r'
    blnode=true
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
opti=false
optt=false
optu=false
optr=false
optq=false

#todo:  set opti & optt depending on current hour. 
#note:  this is to priortize archiving based on winner status when cron run this script
runhere=true
if $RUN_BY_CRON && $runhere;then
    chh=$( date +'%-H' )
    #chh_mod=$( echo "$chh % 2" | bc )
    chh_mod=$( echo "$chh % 3" | bc )
    if (( $chh_mod == 0 ));then
        opti=false
        optt=true
    else
        opti=true
        optt=false
    fi
fi

#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        archive big output files for all experiments within exp id (i.e. fcst, rim, etc).
        Each exp has own archiving script, but this will assist it for larger files
        via crontab instead of running on computing nodes.
        
        Usage: ./$(basename "$0") [-bchw] [-i|-t] [-q qid] [-d exp dir] [-f data file] [-r YYYYMM-YYYYMM] srcme_file

        input:        
            A source file, which set various vars for a set of runs (i.e. srcme_pfe_*)

        options:
            -d  a full path to exp dir 
            -f  a data filename with a list of exp dir (fullpath)
            -r  YYYYMM and YYYYMM range (format: YYYYMM-YYYYMM) 
            -q  reserved queue ID
            -u  update run_gcmarch.sh
            -b  run with a debug mode 
            -i  archive winners exp first 
            -t  archive 3-month runs first
            -h  show this help text
            -w  write stdout/err in a file
"
OPTIND=1
while getopts 'hwbitd:f:r:uq:' option; do
    case "$option" in
        d)  if $optf || $optr ;then die "Cannot specify option d when specifying option r or f";fi
            optd=true; arrdexp1=( $OPTARG );;
        f)  if $optr || $optd ;then die "Cannot specify option f when specifying option r or d";fi
            optf=true; fdata=$OPTARG;
            [[ ! $fdata ]] && die "$fdata does not eist";;
        r)  if $optd || $optf ; then die "Cannot specify option r when specifying option f or d";fi
            optr=true; 
            userbegyyyymm=$( echo $OPTARG | cut -d'-' -f1 );
            userendyyyymm=$( echo $OPTARG | cut -d'-' -f2 );;
        u)  optu=true;;
        i)  $optt && die "Cannot specify option t when specifying option t";
            opti=true;;
        t)  $opti && die "Cannot specify option t when specifying option i";
            optt=true;;
        q)  optq=true; qid=$OPTARG;;
        h)  echo "$usage"; echo "$note"; exit 0;;
        b)  optb=true;rundebug=1;;
        w)  writetofile=1;;
        \?) echo "Invalid option: -$OPTARG" >&2
            exit 1;;
        :)  echo "Option -$OPTARG requires an argument." >&2
            exit 1;;
    esac
done
#-f  a data filename with a list of *DATES* (format: YYYYMMDD)

! $optr && ! $optd && ! $optf && blnrt=true || blnrt=false

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
[[ -n $strf && -f $strf ]] && source $strf
#================================================================================
#                                 Set Variables
#================================================================================
#mid
cdate=$( date +%Y%m%d%H%M )
dstdout=$cdir/stdout/$strscr/$strdout
dmess=$cdir/message
dtmpdata=$cdir/data/submit

[[ -n $rstexpid ]] && blrstexpid=true && strexpid=$strexpid$rstexpid || blrstexpid=false

dstatus=$cdir/output/getstatus/$strdout
ddata=$cdir/output/pckwinners/$strdout

_qstat=/u/scicon/tools/bin/qstat

strflst=data_getstatus_${strexpid}_list_
strarch=gcmarch

fmessage=$dmess/message_$strscr
ferr=$dmess/stderr_$strscr
farchscr=run_${strarch}.sh

ftmp1=$cdir/tmp_${strscr}_${strexpid}_1
ftmp2=$cdir/tmp_${strscr}_${strexpid}_2

fcomp=gcmarch_archcompleted
fdel=gcmarch_deloutcompleted
fmark_clean=clean_completed

blthisheader=false

#msg_subject="$hstshort.$strscr" 
msg_subject="${hstshort}.${strscr}: $strexpid"

        
limscr_gcmarcheach=$( grep "limrsync=" $DBUILD/Applications/GEOSgcm_App/$farchscr 2>/dev/null | cut -d'=' -f2 ) 

##note:  max limit for total # of shiftc running
##limsftc=20
#limsftc=10
#
##note:  screen limit
##limscr=5
##limscr=2
#limscr=1

#note:  max limit 
#       screen limit
bltimelimit=false
if [[ -n $RUN_BY_CRON ]] && $RUN_BY_CRON ;then
    [[ -z $chh ]] && chh=$( date +'%-H' )
    if (( 5 < $chh && $chh < 18 ));then 
        limscr=3
        limsftc=20
        limscr_gcmarch=999
        $bltimelimit && maxsec=900 || maxsec=$( echo "365 * 10 * 24 * 3600" | bc )  
    else
        limscr=2
        limsftc=15
        $bltimelimit && maxsec=900 || maxsec=$( echo "365 * 10 * 24 * 3600" | bc )  
    fi
elif $blnode;then
    if $blrsync;then 
        limscr=5
        limsftc=999
        limscr_gcmarch=$( echo "$limscr_gcmarcheach * $limscr" | bc ) 
        $bltimelimit && maxsec=900 || maxsec=$( echo "365 * 10 * 24 * 3600" | bc )  
    else
        limscr=5
        #note:  04/19/2023 limsftc changed from 40 to 120 
        #                  60, instead of 40, worked fine.
        #limsftc=40
        #limsftc=999
        limsftc=40
        limscr_gcmarch=999
        $bltimelimit && maxsec=900 || maxsec=$( echo "365 * 10 * 24 * 3600" | bc )  
    fi
else
    if $blrsync;then 
        limscr=5
        limsftc=999
        limscr_gcmarch=$( echo "$limscr_gcmarcheach * $limscr" | bc ) 
        $bltimelimit && maxsec=900 || maxsec=$( echo "365 * 10 * 24 * 3600" | bc )  
    else
        limscr=3
        limsftc=40
        limscr_gcmarch=999
        $bltimelimit && maxsec=900 || maxsec=$( echo "365 * 10 * 24 * 3600" | bc )  
    fi
fi

$bltimelimit && sec_diff_min=60 || sec_diff_min=600


#note:  numsec is wailt time in sec and maxsec is total wait time limit
#numsec=60
maxmin=$( echo "$maxsec / 60" | bc  )

#note:  process limits
numproc_max=$( ulimit -u )
numsubtract=100
numproc_maxhere=$(( numproc_max - numsubtract ))
arcqid=none

#note:  save fmessage in dstdout. This was created 
#       in case sending fmessage doesn't work 
blsavefmsg=true
fmsg_fname=message_${strscr}_${cdate} 

feadd=$cdir/mailadd
[[ -f $feadd   ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

[[   -f $ftmp1   ]] && rm -f $ftmp1
[[   -f $ftmp2   ]] && rm -f $ftmp2
[[ ! -d $dmess   ]] && mkdir -p $dmess
[[ ! -d $dstdout ]] && mkdir -p $dstdout
#================================================================================
#                                  Main Process
#================================================================================
#letsbegin
#todo:  find running shiftc that were executed by run_gcmarch.sh
if $blrsync; then 
    arrsftid_all=()
    numsftid_arch=0
else
    arrsftid_all=($( /usr/local/bin/shiftc --status=csv | grep run  | tail -n +2 | cut -d',' -f1 ))
    numsftid_arch=($( printf '%s\n' ${arrsftid_all[@]} | xargs -i bash -c "/usr/local/bin/shiftc --id={} --status=csv | grep --color 'run\|lou\|GEOS_fcst' | head -1 | cut -d',' -f3 | cut -d':' -f2" | sort -V | wc -l ))
fi

#todo:  get all dexp 
if $optr ;then
    get_beg_and_end $userbegyyyymm $userendyyyymm
    arrdexp1=($( get_dexp $DFCST $begyyyymm $endyyyymm ${arrintens[@]} ))

    arrdexp_arch=($( printf '%s\n' ${arrdexp1[@]}     | xargs -i bash -c "[[ -d {} ]] && echo {}/archive" ))
    arrdexp_arch=($( printf '%s\n' ${arrdexp_arch[@]} | xargs -i bash -c "[[ ! -f {}/$fdel ]] && echo {}" ))
    strmsg="$userbegyyyymm - $userendyyyymm"
    msg_subject="$msg_subject ( $strmsg )"

elif $optf;then
    arrdexp1=($( cat $fdata )) 
    #note:  don't sort. Use input file as-is
    #arrdexp_arch=($( printf '%s\n' ${arrdexp1[@]} | xargs -i bash -c "[[ -d {}/archive ]] && echo {}/archive" | sort -V ))
    arrdexp_arch=($( printf '%s\n' ${arrdexp1[@]} | xargs -i bash -c "[[ -d {}/archive ]] && echo {}/archive"  ))
    strmsg="$fdata"
    msg_subject="$msg_subject ($( basename $fdata ))"

elif $optd;then
    arrdexp_arch=($( printf '%s\n' ${arrdexp1[@]} | xargs -i bash -c "[[ -d {}/archive ]] && echo {}/archive" ))
    strmsg="${arrdexp1[0]}"
    msg_subject="$msg_subject ( ${arrdexp1[0]} )"

elif $blnrt;then 
    #todo:  without options, this code will figureout all forecasts from the last month and ones up to 
    #       today's date in the current month. 
    arryyyymmdd=($( s2sv3_nrtdates )) 
    arrdexp_arch=($( printf '%s\n' ${arryyyymmdd[@]} | xargs -i printf "$DFCST/{}/ens%s\n" ${arrintens[@]} | xargs -i bash -c "[[ -d {}/archive ]] && echo {}/archive" | sort -V ))
    
fi

arrarch_final=($( filter_arch ${arrdexp_arch[@]} ))
#wmessage \@$LINENO
#ahand_print ${arrarch_final[@]} 
#wmessage
#exit

if $opti;then
    #todo:  sort arrarch_final and winner exps will be archived first
    blwinnerfirst=true
    arrarch_final=($( sort_darch $blwinnerfirst ${arrarch_final[@]} ))

elif $optt;then
    blwinnerfirst=false
    arrarch_final=($( sort_darch $blwinnerfirst ${arrarch_final[@]} ))

elif $optf && [[ "$strexpid" == "fcst" ]] ;then
    :
    ##note:  just for now, dexp are reserve-sort
    #arrarch_final1=($( printf '%s\n' ${arrarch_final[@]} | sort -V -r ))
    #arrarch_final=( ${arrarch_final1[@]} )
fi


#todo:  exit out when optb is ture
if $optb;then 
    ahand_warr ${arrarch_final[@]}
    wmessage
    wmessage "            Number of exp : ${#arrarch_final[@]}"
    wmessage " Number of running shiftc : $numsftid_arch ( shift limit = $limsftc )"
    exit
fi

(( ${#arrarch_final[@]} == 0 )) && exit

msg_wheader 
wmessage "$( hostname )"

numproc=$( ps aux | awk '{ print $1 }' | grep -w $USER | wc -l )
numscr_gcmarch=$( screen -ls | grep -i detached | grep gcmarch_${strexpid}_ 2>/dev/null | wc -l )

runhere=true
runhere=false
if $runhere;then
    wmessage  arrarch_final = ${#arrarch_final[@]}
    wmessage "numsftid_arch = $numsftid_arch ( maxlim = $limsftc )"
    wmessage 
fi

[[ -n $strmsg ]] && \
              wmessage "                       User Inputs : $strmsg"
              wmessage "               Total number of exp : $( printf '%+5s\n' ${#arrarch_final[@]} ) ( screen lim = $( printf '%+5s\n' $limscr  ) )"
  $blrsync && wmessage " Currently Running gcmarch Screens : $( printf '%+5s\n' $numscr_gcmarch ) (     maxlim = $( printf '%+5s\n' $limscr_gcmarch ) )"
! $blrsync && wmessage "          Currently Running shiftc : $( printf '%+5s\n' $numsftid_arch ) (     maxlim = $( printf '%+5s\n' $limsftc ) )"
! $blrsync && wmessage "       Currently Running processes : $( printf '%+5s\n' $numproc ) (     maxlim = $( printf '%+5s\n' $numproc_maxhere ) )"
$optr && [[ -n $userbegyyyymm && -n $userendyyyymm ]] && \
              wmessage "                    Archiving Year : $userbegyyyymm - $userendyyyymm"
     $optq && wmessage "                    User Input QId : $( printf '%+5s\n' $qid)"

if $opti || $optt ;then
    wmessage "                Current Hour : $( printf '%+5s\n' $chh )"
    $opti && wmessage "                               Archive winner runs first"
    $optt && wmessage "                               Archive 3month runs first"
fi

wmessage
blinitexecute=false
for darch in ${arrarch_final[@]};do

    #todo:  get index of darch
    for i in "${!arrarch_final[@]}"; do
        [[ "${arrarch_final[$i]}" == "$darch" ]] && break 
    done
    
    thisind=$(( i + 1 ))

    #todo:  count # of running screen and shiftc
    if $blrsync; then 
        numsftid_arch=0
    else
        arrsftid_all=($( /usr/local/bin/shiftc --status=csv | grep run  | tail -n +2 | cut -d',' -f1 ))
        numsftid_arch=($( printf '%s\n' ${arrsftid_all[@]} | xargs -i bash -c "/usr/local/bin/shiftc --id={} --status=csv | grep --color 'run\|lou\|GEOS_fcst' | head -1 | cut -d',' -f3 | cut -d':' -f2" | sort -V | wc -l ))
    fi

            numscr=$( screen -ls | grep -i detached | grep ${strscr}_${strexpid}_ | grep -vE 'gcmarch|quickarch' | wc -l )
    numscr_gcmarch=$( screen -ls | grep -i detached | grep gcmarch_${strexpid}_  2>/dev/null | wc -l )

    #todo:  count number of processes
    numproc=$( ps aux | awk '{ print $1 }' | grep -w $USER | wc -l )

    dexp=$( echo $darch | rev | cut -d'/' -f2- | rev ) 

    set_rstfcstdate $dexp

    #!!!!! NOTE - keep this code for now (03/12/2022)
    if $optu;then
        exp_updarchscr $arcqid $dexp
    fi

    blexecuted=false
    blinitialnote=false
    totsec=0
    totmin=0

    sec0=$( date +%s )
    sec1=$sec0

    while ! $blexecuted;do 
#wmessage \@$LINENO         
#wmessage $numproc $numproc_maxhere 
#wmessage $numsftid_arch $limsftc 
#wmessage $numscr_gcmarch $limscr_gcmarch 
#wmessage $numscr $limscr 
#wmessage $totsec $maxsec
        #if (( $numproc < $numproc_maxhere && $numsftid_arch < $limsftc && $numscr < $limscr && $totsec <= $maxsec ));then
        if (( $numproc < $numproc_maxhere && $numsftid_arch < $limsftc && $numscr_gcmarch < $limscr_gcmarch && $numscr < $limscr && $totsec <= $maxsec ));then
            
            totsec=0
            blexecuted=true
            blinitexecute=true

            wmessage "$( printf "%+3s of %+3s\n" $thisind ${#arrarch_final[@]} ) $dexp"

            #+++++ cd to darch (start) +++++
            cd $darch

            if [[ -n $qid ]];then 
                screen -dmS ${strscr}_${strexpid}_$fcstdate$ensm bash -c "./run_gcmarch.sh -q $qid >> stderr_gcmarch 2>&1"
            else
                screen -dmS ${strscr}_${strexpid}_$fcstdate$ensm bash -c "./run_gcmarch.sh >> stderr_gcmarch 2>&1"
            fi

            cd - >/dev/null
            #+++++ cd to darch (start) +++++

        else
            sec2=$( date +%s )
            sec_diff=$(( sec2 - sec1 ))
            totsec=$(( sec2 - sec0 ))
            totmin=$( echo "$totsec / 60" | bc )

#wmessage \@$LINENO $sec2 $sec0 $totsec $totmin
            #todo:  break out for one loop (hence, 2)
            $blinitexecute && (( $totsec >= $maxsec )) && break 2 

            if ! $blinitialnote ;then 
                #! $blrsync && wmessage "$( date +'%m/%d/%Y %H:%M' ) ... $( printf '%+2s\n' $numscr ) screen, $( printf '%+2s\n' $numsftid_arch ) shfitc, and $( printf '%+2s\n' $numproc ) processes are running ... will wait for max of $maxmin minutes ..."
                #  $blrsync && wmessage "$( date +'%m/%d/%Y %H:%M' ) ... $( printf '%+2s\n' $numscr ) screen, $( printf '%+2s\n' $numscr_gcmarch ) screen, and $( printf '%+2s\n' $numproc ) processes are running ... will wait for max of $maxmin minutes ..."
                blinitialnote=true

            elif ! $blinitexecute;then
                ! $blrsync && wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screen, $( printf '%+2s\n' $numsftid_arch ) shfitc, and $( printf '%+2s\n' $numproc ) processes are running"  #... waited for $totmin min"
                  $blrsync && wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screen, $( printf '%+2s\n' $numscr_gcmarch ) screen, and $( printf '%+2s\n' $numproc ) processes are running" #... waited for $totmin min"
                sleep ${sec_diff_min}s
                sec0=$( date +%s )
                sec1=$sec0
                #sec1=$( date +%s )

            elif (( $sec_diff > $sec_diff_min ));then
                ! $blrsync && wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screen, $( printf '%+2s\n' $numsftid_arch ) shfitc, and $( printf '%+2s\n' $numproc ) processes are running ... waited for $totmin min ( max = $maxmin )"
                  $blrsync && wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screen, $( printf '%+2s\n' $numscr_gcmarch ) screen, and $( printf '%+2s\n' $numproc ) processes are running ... waited for $totmin min ( max = $maxmin ) "
                sec1=$( date +%s )
            fi

        fi
        
        if $blrsync; then 
            numsftid_arch=0
        else
            arrsftid_all=($( /usr/local/bin/shiftc --status=csv | grep run  | tail -n +2 | cut -d',' -f1 ))
            numsftid_arch=($( printf '%s\n' ${arrsftid_all[@]} | xargs -i bash -c "/usr/local/bin/shiftc --id={} --status=csv | grep --color 'run\|lou\|GEOS_fcst' | head -1 | cut -d',' -f3 | cut -d':' -f2" | sort -V | wc -l ))
        fi
                numscr=$( screen -ls | grep -i detached | grep ${strscr}_${strexpid}_ | grep -vE 'gcmarch|quickarch' | wc -l )
        numscr_gcmarch=$( screen -ls | grep -i detached | grep gcmarch_${strexpid}_  2>/dev/null | wc -l )
        numproc=$( ps aux | awk '{ print $1 }' | grep -w $USER | wc -l )

    done
done
   
#todo:  wait for the rest of screen sessions to finish. Otherwise, kill them 
blinitialnote=false
totsec=0
totmin=0

sec0=$( date +%s )
sec1=$sec0
! $bltimelimit && maxsec=600

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
        wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screens - running... will wait for max of $maxmin minutes"
        blinitialnote=true

    elif (( $sec_diff > 60 ));then
        wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screens - running ... waited for $totmin min"
        sec1=$( date +%s )
    fi
    
            numscr=$( screen -ls | grep -i detached | grep ${strscr}_${strexpid}_ | grep -vE 'gcmarch|quickarch' | wc -l )
    numscr_gcmarch=$( screen -ls | grep -i detached | grep gcmarch_${strexpid}_ | wc -l )

done

 

#todo:  write out $fcomp if exists
for darch in ${arrdexp_arch[@]};do
    if [[ -f $darch/$fcomp && ! -f $darch/$fdel ]];then
        ! $blthisheader && wmessage && wmessage "Existing $fcomp :" && blthisheader=true
        wmessage $( stat --print="%y %n" $darch/$fcomp )
    fi
done 


if (( $numscr > 0 ));then
    wmessage
    wmessage "Running Screens:"
    wmessage "$( screen -ls | grep ${strscr}_${strexpid}_ )"
    wmessage 
fi

wmessage
write_quota_fcst
wmessage
write_numcleaned_dexp
wmessage
wmessage "Time Ended - $(TZ=America/New_York date +'%m/%d/%Y %H:%M' )"

#todo:  send email
[[ -f $fmessage ]] && sendmsg

exit





#todo:  send email
#if $blnode || $RUN_BY_CRON ;then
if [[ -f $fmessage ]];then
    fmsg_final=$dstdout/message_${strscr}_$strexpid

    if [[ -f $fmsg_final ]];then
        cat $fmessage   | tail -n +4 >> $ftmp1
        cat $fmsg_final | tail -n +4 >> $ftmp2

        diff $ftmp1 $ftmp2 >>/dev/null 2>&1
        status_diff=$?
    else
        status_diff=999
    fi

    if (( $status_diff > 0 ));then 

        [[ -f $ferr     ]] && sizeferr=$( stat --print='%s' $ferr )  || sizeferr=0
        [[ -f $fmessage ]] && sizef=$( stat --print='%s' $fmessage ) || sizef=0

        if (( $sizeferr > 0 || $sizef > 0 ));then 

            if (( $sizeferr > 0 ));then 
                msg_wheader_userdefined 40 "-" $( basename $ferr ) 
                wmessage "$( cat $ferr )"
                blrm=true
            fi

            msg_cyberpostman "$msg_subject" "$eadds" $fmessage
            status_email=$?

            $blsavefmsg && mv $fmessage $fmsg_final
            (( $status_email == 0 )) && rm -f $fmessage
            $blrm && msg_newfile $ferr
        fi
    fi
fi

exit


