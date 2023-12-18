#!/usr/bin/env bash

#note: for debugging
#ref: https://is.gd/kxHve1
#exec 5> debug_$(basename "$0")
#BASH_XTRACEFD="5"
#PS4='$LINENO: '
#set -x


#description:   
inputcheck(){
    local srcme=$( basename $srcf | cut -d'_' -f2 )
    [[ "$srcme" != "$hstshort" ]] && die "source file is not for this host (select srcme_${hstshort}_* file)"
    ! $optd && ! $optf && die "optd or optf is requied input"
    $optd && [[ -z $dexp ]] && die "optd requires one input"
    $optf && [[ -z $fdata ]] && die "optf requied one input"
    $optf && [[ ! -f $fdata ]] && die "$fdata does not exist"
    return
}

setmyvars(){
    local _dexp=$1
    [[ -z $_dexp ]] && die "(${FUNCNAME[0]}) input missing"
    [[ ! -d $_dexp ]] && die "(${FUNCNAME[0]}) $_dexp does not exist"

    arrfsave=() 
    arrfname=()
    
    local strmapl="01_0000z.nc4"

    set_rstfcstdate $_dexp
    
    darch=$DARCH/$fcstdate/$ensm

    local fcapr=cap_restart
    local fcapric=cap_restartIC
    local fcapend=cap_end
    local fhis2=HISTORY_2.rc
    local _fhis2=$_dexp/$fhis2
    local _fcapric=$_dexp/$fcapric
    local _fcapr=$_dexp/$fcapr
    local _fcapend=$_dexp/$fcapend

    if [[ -f $_fcapric ]];then 
        capric_yyyymmdd=$( cat $_fcapric | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
        capric_yyyymm=$( echo $capric_yyyymmdd | cut -c1-6 )
        capric_yyyy=$( echo $capric_yyyymmdd | cut -c1-4 )
    fi
    [[ -f $_fcapr   ]] && capr_yyyymmdd=$( cat $_fcapr | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
    if [[ -f $_fcapend ]];then 
        realend_yyyymmdd=$( cat $_fcapend | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
        realend_yyyymm=$( cat $_fcapend | head -1 | tr -s '[:space:]' | cut -d' ' -f1 | cut -c1-6 )
    fi
    
    if [[ -n $fcstdate ]];then
        nummonth1=3
        
        strxmonth=$( nextXmonths $( date -d $fcstdate +%Y ) $( printf '%01g' $( date -d $fcstdate +%m ) ) 0 $(( nummonth1 + 1 ))  )
        end_mm=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f1 )
        end_year=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f2 )
        end3_yyyymmdd=$end_year$end_mm"01"
        end3_yyyymm=$end_year$end_mm
    
        #todo:  calculate a total number of mom outputs 
        #numfmom_calc=$( calc_numseg $fcstdate $capr_yyyymmdd )
    fi
    
        
    numfmom_calc=$( calc_numseg $fcstdate $capr_yyyymmdd )

    bllastdayofthemonth=$( fcal_lastdayofthemonth $capric_yyyymmdd )
    blfirstdayofthemonth=$( fcal_firstdayofthemonth $capric_yyyymmdd )
    
    arrmfull_3mo=($( numfullmonths $capric_yyyymm $end3_yyyymm $bllastdayofthemonth ))
    arrmfull_10mo=($( numfullmonths $capric_yyyymm $realend_yyyymm $bllastdayofthemonth ))

    arrcoll=($( exp_getcollections $_fhis2 ))
    arrcollmon_mapl=($( printf '%s\n' ${arrcoll[@]} | grep _1mo_glo_ ))
    arrcollmonpost=($( exp_getcollfreq n $_fhis2 ))
    arrcollmonpost_diu=($( exp_getcolldiurnal $_fhis2 ))
    arrcollmonpost_savemon=( ${arrcollmonpost[@]} )
    arrcollmonpost_savediu=($( printf '%s\n' ${arrcollmonpost_diu[@]} | sort -V -k3 -t'_' | grep -E '_sfc|_slv' ))
    arrcollmonpost_savedya=( ice_tavg_1dy_glo_T1440x1080_slv )
    arrcollmonpost_savedy3=( sfc_tavg_3hr_glo_L720x361_sfc )
    
    #todo:  get files to save
    (( $capr_yyyymmdd == $realend_yyyymmdd )) && _bl10morun=true || _bl10morun=false
    $_bl10morun && _arrmonthfull=( ${arrmfull_10mo[@]} ) ||  _arrmonthfull=( ${arrmfull_3mo[@]} )
   
    for coll in ${arrcoll[@]};do
        [[ "${arrcollmon_mapl[@]}" =~ "$coll" ]]        && _blsavemapl=true || _blsavemapl=false
        [[ "${arrcollmonpost_savemon[@]}" =~ "$coll" ]] && _blsavemon=true  || _blsavemon=false
        [[ "${arrcollmonpost_savediu[@]}" =~ "$coll" ]] && _blsavediu=true  || _blsavediu=false
        [[ "${arrcollmonpost_savedya[@]}" =~ "$coll" ]] && _blsavedya=true  || _blsavedya=false
        [[ "${arrcollmonpost_savedy3[@]}" =~ "$coll" ]] && _blsavedy3=true  || _blsavedy3=false
        [[ "${arrcollmonpost_noreft[@]}"  =~ "$coll" ]] && _blnoreft=true   || _blnoreft=false
    
        arrfsave+=($( exp_createfname_save $_dexp $coll $blfirstdayofthemonth $bllastdayofthemonth $_blsavemapl $_blsavemon $_blsavediu $_blsavedya $_blsavedy3 $_blnoreft ${_arrmonthfull[@]} ))
    done 
    
    #todo:  create output file names that are supposed to exists in _dexp
    for coll in ${arrcoll[@]};do
        [[ "${arrcollmonpost_noreft[@]}"  =~ "$coll" ]] &&  _blnoreft=true  ||  _blnoreft=false
        arrfname+=($( exp_createfname $_dexp $coll $bllastdayofthemonth $blfirstdayofthemonth $_blnoreft ${_arrmonthfull[@]} ))
    done 
    
    local blwin=$( exp_checkwinner $_dexp $dwin )
    [[ -f $_dexp/gcm_run.j ]] && local blrerun=$( grep -i "RERUN" $_dexp/gcm_run.j 2>/dev/null | head -1 | rev | cut -d' ' -f1 | rev | tr '[:upper:]' '[:lower:]'  ) \
                                     || local blrerun=false
    $blwin || $blrerun && local _blthis=true || local _blthis=false 
    local arrcalc=($( exp_calcoutput $_dexp $_blthis $_dexp/HISTORY_2.rc ))
    #               total, mapl outputs, daily, diurnal, & monthly means

      numftotal_calc=${arrcalc[0]} 
    numfmonmapl_calc=${arrcalc[1]}    
        numfday_calc=${arrcalc[2]}        
        numfdiu_calc=${arrcalc[3]}        
    numfmonpost_calc=${arrcalc[4]}    

    numfsave_mpl=$( printf '%s\n' ${arrfsave[@]} | grep "$strmapl" | wc -l )
    numfsave_mon=$( printf '%s\n' ${arrfsave[@]} | grep "monthly"  | wc -l )
    numfsave_diu=$( printf '%s\n' ${arrfsave[@]} | grep "diurnal"  | wc -l )
    numfsave_day=$( printf '%s\n' ${arrfsave[@]} | grep "daily"    | wc -l )
    numfsave_rst=0
    numfsave_mom=0
    numfsave_tot=$( echo "$numfsave_mpl + $numfsave_mon + $numfsave_diu + $numfsave_day + $numfsave_mom" | bc )

    #todo:  find which file to delete
    arrfremove=($( printf '%s\n' ${arrfname[@]} ${arrfsave[@]} | sort -V | uniq -u | sort -V ))
    
    arrcollmonpost_savedy3_files=($( printf '%s\n' ${arrfsave[@]} | grep ${arrcollmonpost_savedy3[0]} | grep daily | xargs -i basename {} ))

    arrfmiss=($( exp_getfmiss_fexist $_dexp ))

    return
}

create_lstlfeout(){
    local _fout=$1
    
    ssh -q lfe "printf '%s\n' ${arrcollmon_mapl[@]}        | xargs -i bash -c "'"'"find $darch/{}/* -type f -name '${fcstdate}.{}.*z.nc4*'"'"'" "         >> $_fout 2>/dev/null
    ssh -q lfe "printf '%s\n' ${arrcollmonpost_savemon[@]} | xargs -i bash -c "'"'"find $darch/{}/* -type f -name '${fcstdate}.{}.monthly.*.nc4'"'"'" "   >> $_fout 2>/dev/null 
    ssh -q lfe "printf '%s\n' ${arrcollmonpost_savediu[@]} | xargs -i bash -c "'"'"find $darch/{}/* -type f -name '${fcstdate}.{}.diurnal.*.nc4'"'"'" "   >> $_fout 2>/dev/null
    ssh -q lfe "printf '%s\n' ${arrcollmonpost_savedya[@]} | xargs -i bash -c "'"'"find $darch/{}/* -type f -name '${fcstdate}.{}.daily.*.nc4.tar'"'"'" " >> $_fout 2>/dev/null
        
    for fout in ${arrcollmonpost_savedy3_files[@]};do
        coll=${arrcollmonpost_savedy3[0]}
        #19930312.sfc_tavg_3hr_glo_L720x361_sfc.daily.199305.nc4.tar
        numleadspace1=$( echo "${#fout} + 4 " | bc )

        #wmessage "$( printf "%${numleadspace1}s\n" $fout )"
        #shiftc --no-mail --wait lfe:$darch/$coll/$fout $dexp/$coll/
        #ssh -q lfe cp -np $darch/$coll/$fout $dexp/$coll/

        ssh -q lfe "find $darch/$coll/* -type f -name $fout " >> $_fout 2>/dev/null
    done
    return
}

check_shiftc(){
    #description:   check shiftc status
    local _fmsg=$1
    local _arrrunning=()

    local sid=$( grep "$strsid" $_fmsg 2>/dev/null | tail -1 | rev | cut -d' ' -f1 | rev )

    if [[ -n $sid ]];then
        local sid_status=$( /usr/local/bin/shiftc --status=csv | grep $sid | cut -d',' -f2 )

        if [[ "$sid_status" == "run" || "$sid_status" =~ "run"* ]];then 
            _arrrunning+=( $sid )
        fi
    fi

    echo "${_arrrunning[@]}"
}

cooking_shiftc_nowait(){
    local fin=$1

    if $blnode ;then
        if (( $writetofile == 1 ));then
            wmessage $hstname
            /usr/local/bin/shiftc --hosts=1 --streams=1 --no-cron --no-mail -f -d < $fin >> $fmessage 2>&1
        else
            wmessage $hstname
            /usr/local/bin/shiftc --hosts=1 --streams=1 --no-cron --no-mail -f -d < $fin
        fi

    elif [[ "$thishost" == "pfe" ]];then
        if (( $writetofile == 1 ));then
            wmessage $hstname
            /usr/local/bin/shiftc --no-cron --no-mail -f -d < $fin >> $fmessage 2>&1
        else
            wmessage $hstname
            /usr/local/bin/shiftc --no-cron --no-mail -f -d < $fin
        fi

    else
        if (( $writetofile == 1 ));then
            ssh pfe "hostname; /usr/local/bin/shiftc --no-cron --no-mail -f -d < $fin >> $fmessage 2>&1"
            #$thiscmd < $fin >> $fmessage 2>&1
        else
            ssh pfe "hostname; /usr/local/bin/shiftc --no-cron --no-mail -f -d < $fin"
            #$thiscmd < $fin
        fi
    fi
    return
}

cooking_copy(){

    for coll in ${arrcollmon_mapl[@]};do
        numleadspace1=$( echo "${#coll} + 4 " | bc )

        wmessage "$( printf "%${numleadspace1}s\n" $coll )"
        ssh -q lfe cp -np $darch/$coll/*.${coll}.*z.nc4* $dexp/$coll/
    done

    for coll in ${arrcollmonpost_savemon[@]};do
        #19930312.aer_inst_3hr_glo_L720x361_slv.monthly.199312.nc4
        numleadspace1=$( echo "${#coll} + 4 " | bc )
        wmessage "$( printf "%${numleadspace1}s\n" $coll )"
        ssh -q lfe cp -np $darch/$coll/${fcstdate}.${coll}.monthly.*.nc4 $dexp/$coll/ 
    done 

    for coll in ${arrcollmonpost_savediu[@]};do
        #19930131.aer_inst_3hr_glo_L720x361_slv.diurnal.199305.nc4
        numleadspace1=$( echo "${#coll} + 4 " | bc )

        wmessage "$( printf "%${numleadspace1}s\n" $coll )"
        ssh -q lfe cp -np $darch/$coll/${fcstdate}.${coll}.diurnal.*.nc4 $dexp/$coll/
    done
    
    for coll in ${arrcollmonpost_savedya[@]};do
        #19930131.ice_tavg_1dy_glo_T1440x1080_slv.daily.199302.nc4.tar
        numleadspace1=$( echo "${#coll} + 4 " | bc )

        wmessage "$( printf "%${numleadspace1}s\n" $coll )"
        ssh -q lfe cp -np $darch/$coll/${fcstdate}.${coll}.daily.*.nc4.tar $dexp/$coll/
    done
    
    for fout in ${arrcollmonpost_savedy3_files[@]};do
        coll=${arrcollmonpost_savedy3[0]}
        #19930312.sfc_tavg_3hr_glo_L720x361_sfc.daily.199305.nc4.tar
        numleadspace1=$( echo "${#fout} + 4 " | bc )

        wmessage "$( printf "%${numleadspace1}s\n" $fout )"
        ssh -q lfe cp -np $darch/$coll/$fout $dexp/$coll/
    done
    return
}

clean_dir() {
    [[ -n $flock && -f $flock ]] && rm -f $flock
    [[ -n $flst_lfe && -f $flst_lfe ]] && rm -f $flst_lfe
    [[ -n $flfeout_lst && -f $flfeout_lst ]] && rm -f $flfeout_lst
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
elif [[ "$hstname" =~ "r"* ]];then
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

#todo:  check if this script is invoked as child script
[[ -n $RUNCHILD ]] && blchild=true || blchild=false

if ! $blchild;then
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
fi     
trap clean_dir EXIT

if [[ ! $cdir/func_fcst.sh ]];then 
    echo "$cdir/func_fcst.sh does not exist";exit
else
    source $cdir/func_fcst.sh
fi

    
hstshort=$( get_host )
writetofile=0
optd=false
optm=false
optf=false
optr=false

#================================================================================
#                                     Usage
#================================================================================
#ref: https://goo.gl/BUBwPV
usage="$(basename "$0") -- this is a program to:
        copy forecast/hindcast outputs from LFE to PFE.
        
        Usage: ./$( basename "$0" ) [-cwh] [-d dexp] [-m fmessage] srcme

        input:        
            A source file, which set various vars for a set of runs (i.e. srcme_pfe_*)

        options:
            -d  a full path to an experiment dir
            -m  fmessage file name
            -c  clean unwanted files
            -h  show this help text
            -w  write stdout/err in a file
"

OPTIND=1
while getopts 'hwbzd:f:r:m:' option; do
    case "$option" in
        d)  if $optf || $optr ;then die "Cannot specify option d when specifying option r or f";fi
            optd=true; dexp=$OPTARG;;
        f)  if $optr || $optd ;then die "Cannot specify option f when specifying option r or d";fi
            optf=true; fdata=$OPTARG ;;
        r)  if $optd || $optf ; then die "Cannot specify option r when specifying option f or d";fi
            optr=true; 
            userbegyyyymm=$( echo $OPTARG | cut -d'-' -f1 );
              userendyyyymm=$( echo $OPTARG | cut -d'-' -f2 );;
        m)  optm=true;fmessage=$OPTARG;;
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
srcf=${arrposarg[i]}; i=$(( i + 1 ))
source $srcf

inputcheck
[[ -z $DARCH ]] && die "DARCH is undefined or does not exist"
#================================================================================
#                                 Set Variables
#================================================================================
#mid
arcqid=none
dmess=$cdir/message
dstdout=$cdir/stdout/$strscr/$strdout
dwin=$cdir/output/pckwinners/$strexpid

strsid="Shift id is "

#note:  strpstissue is exported from run_outrecover.sh
[[ -z $strpstissue ]] && strpstissue="Possible_gcmpost_issue"
[[ -z $strnorec    ]] && strnorec="No_recover"

setmyvars $dexp

flst_lfe=$cdir/${strscr}_${strexpid}_$fcstdate${ensm}_lfefiles

#todo:  use shiftc to transfer files from lfe to pfe
flfeout_lst=$cdir/tmp_${strscr}_lfeoutlst_${fcstdate}$ensm
fshiftin=$cdir/shiftcin_${strscr}_${fcstdate}$ensm

blshiftc=true

[[ -z $fmessage   ]] && fmessage=$dmess/message_${strscr}_${strexpid}_${fcstdate}${ensm}
fmessage_base=$( basename $fmessage )
[[ -f $dstdout/$fmessage_base ]] && exit


[[ -f $flfeout_lst ]] && rm -f $flfeout_lst
[[ ! -d $dmess   ]] && mkdir -p $dmess
[[ ! -d $dstdout ]] && mkdir -p $dstdout
#================================================================================
#                                  Main Process
#================================================================================
#main
writehere=true
writehere=false
if $writehere;then
    wmessage ${#arrcoll[@]} 
    wmessage ${#arrcollmon_mapl[@]} 
    wmessage ${#arrcollmonpost_savemon[@]} 
    wmessage ${#arrcollmonpost_savediu[@]} 
    wmessage ${#arrcollmonpost_savedya[@]}
    wmessage ${#arrcollmonpost_savedy3_files[@]}
    wmessage ${#arrfmiss[@]} 
    #ahand_warr ${arrcoll_maplmon[@]}
    #wmessage
    ##ahand_warr ${arrcollmonpost[@]}
    #ahand_warr ${arrcollmonpost_diu[@]}
    
    exit
fi

#todo:  check missing output
arrfmiss=($( exp_getfmiss_fexist $dexp ))
arrfmiss=($( printf '%s\n' ${arrfmiss[@]} | sort -V )) 

#todo:  count # of shiftc running 
numshiftc=$( grep "$strsid" $fmessage 2>/dev/null | wc -l )
(( $numshiftc > 0 )) && blsftc_executed=true || blsftc_executed=false

#exp_updarchscr $arcqid $dexp

getlfefout $dexp $darch $flst_lfe 
count_files $dexp $flst_lfe

#todo: check if a number of files on pfe matchs with a calculated number
(( $numfpfe_total_cnt == $numfsave_tot )) && blnumfmatch=true || blnumfmatch=false
numfmiss=($( echo "$numfsave_tot - $numfpfe_total_cnt" | bc )) 

writeheader $dexp $darch
write_table $dexp $numfmiss 
wmessage 

#todo:  check shiftc status 
arrsftc_running=($( check_shiftc $fmessage ))
if $blsftc_executed && (( ${#arrsftc_running[@]} > 0 ));then 
    wmessage "${arrsftc_running[@]} - shfitc is still running "
    exit
else
    [[ -f $fshiftin    ]] && rm -f $fshiftin
fi

#wmessage
#for fmiss in ${arrfmiss[@]} ;do
#    fmiss_bname=$( basename $fmiss ) 
#    timestamp="$( stat --print='%.19y' $fmiss )"
#    fsize="$( stat --print='%15s' $fmiss )"
#
#    #wmessage "$( printf '%+15s' $fsize ) $timestamp $fmiss_bname"
#    wmessage "$fsize $timestamp $fmiss_bname"
#done

wmessage \@$LINENO $blsftc_executed $blnumfmatch ${#arrfmiss[@]} $numfmiss
(( $numfmiss < 20 )) && ahand_warr ${arrfmiss[@]} 

if $blsftc_executed && $blnumfmatch && (( ${#arrfmiss[@]} == 0 ));then
    [[ -f $fshiftin    ]] && rm -f $fshiftin
    (( $writetofile == 1 )) && mv $fmessage $dstdout/
    exit

#elif $blsftc_executed && ! $blnumfmatch && (( $numfmiss > 0 )) ;then
#    wmessage "$strpstissue"
#wmessage "exit @$LINENO"     
#    (( $writetofile == 1 )) && mv $fmessage $dstdout/${fmessage_base}_PUnf
#    exit
    
elif $blsftc_executed && $blnumfmatch ;then
    #note:  arrfmiss includes mom outputs. they don't need to be recovered.
    [[ -f $fshiftin    ]] && rm -f $fshiftin
    (( $writetofile == 1 )) && mv $fmessage $dstdout/
    exit

elif (( $numfmiss <= 0 ));then
    wmessage "$strnorec" 
    (( $writetofile == 1 )) && mv $fmessage $dstdout/${fmessage_base}_NoRec
    exit
fi

#wmessage "exit @$LINENO"     
#exit

if $blshiftc;then
wmessage lineno = $LINENO 
    create_lstlfeout $flfeout_lst
    
    arrflfeout=($( misc_readfbyline $flfeout_lst ))
    for flfeout in ${arrflfeout[@]} ;do 
        fpfeout=$( echo $flfeout | sed "s#$darch#$dexp#g" ) 
        echo "lfe:$flfeout $fpfeout" >> $fshiftin
    done 
    
    [[ -f $fshiftin ]] && cooking_shiftc_nowait $fshiftin 
else
    #todo:  use cp to transfer files from lfe to pfe
    cooking_copy
fi

#count_files $dexp $flst_lfe
#write_table $dexp ${#arrfmiss[@]} 



exit
