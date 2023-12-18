#!/usr/bin/env bash

#description:   
inputcheck(){
    local hstshort=$( get_host )
    [[ -z  ${arrposarg[@]} ]] && die "an input is missing"
    (( ${#arrposarg[@]} != 1 )) && die "1 input is required"

    local srcme=$( echo ${arrposarg[0]} | cut -d'_' -f2 )
    [[ "$srcme" != "$hstshort" ]] && die "source file is not for this host (select srcme_${hstshort}_* file)"

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
        blah blah blah.
        
        Usage: ./$( basename "$0" ) [-chw] [-f file ] [-d path] srcme

        options:
            -f  a filename with a list of experiment
            -d  a full path to an experiment dir
            -c  clean unwanted files
            -h  show this help text
            -w  write stdout/err in a file
"

file=
verbose=0

while :; do
    case $1 in
        -h|-\?|--help)  echo "$usage" && exit;; 
                  -f )  [[ "$2" ]] && userinput=$2 && shift || die "ERROR: opt f requires a non-empty option argument.";;
                  -d )  [[ "$2" ]] && userinput=$2 && shift || die "ERROR: opt d requires a non-empty option argument.";;
                  -- )  shift && break ;;   # End of all options.
                 -?* )  printf 'WARN: Unknown option (ignored): %s\n' "$1" >&2 ;;
                   * )  break ;; # Default case: No more options, so break out of the loop.
    esac
    shift
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
#                                 Set Variables
#================================================================================
#mid
dmess=$cdir/message
ddata=$cdir/data/$strscr

fmessage=$dmess/message_$strscr
 fstderr=$dmess/stderr_$strscr

fdata_inp=$ddata/data_cleanedExp
fdata_pfetar=$ddata/data_${strscr}_pfetar
fdata_pfetar_size=$ddata/data_${strscr}_pfetar_size
fdata_lfetar=$ddata/data_${strscr}_lfetar
fdata_lfedexp=$ddata/data_${strscr}_lfedexp

blheader=false
bldel_pfe=false
bldel_lfe=true
lastm=0
cnt_fdata=0


[[ ! -d $dmess   ]] && mkdir -p $dmess
[[ ! -d $ddata   ]] && mkdir -p $ddata
#================================================================================
#                                  Main Process
#================================================================================
#main
arrdexp=($( misc_readfbyline $fdata_inp ))

! $blheader && msg_wheader && blheader=true

#[[ -f $fdata_pfetar ]] && rm -f $fdata_pfetar
if [[ ! -f $fdata_pfetar ]];then 
    cnt_fdata=$(( cnt_fdata + 1 ))
    wmessage "Data File #${cnt_fdata} - List of Tar Files on PFE: $fdata_pfetar"
    printf '%s\n' ${arrdexp[@]} | xargs -i bash -c "find {}/* -maxdepth 1 -mindepth 1 -type f -name '*[0-9].nc4.tar' 2>/dev/null " | grep -vE 'sst_tavg_1dy_glo_L720x361_slv/|daily' >> $fdata_pfetar 2>&1
fi

if [[ ! -f $fdata_lfetar ]];then 
    cnt_fdata=$(( cnt_fdata + 1 ))
    wmessage "Data File #${cnt_fdata} - List of Tar Files on LFE: $fdata_lfetar"
    ssh -q lfe "find $DARCH/* -maxdepth 3 -mindepth 3 -type f -name '*[0-9].nc4.tar' | grep -vE 'sst_tavg_1dy_glo_L720x361_slv/|daily' >> $fdata_lfetar" 2>/dev/null
fi

if [[ ! -f $fdata_pfetar_size ]];then
    cnt_fdata=$(( cnt_fdata + 1 ))
    wmessage "Data File #${cnt_fdata} - PFE File Size: $fdata_pfetar_size"
    cat $fdata_pfetar | xargs -i bash -c "stat --printf='%s %n \n' {}" >> $fdata_pfetar_size
fi


runhere=true
runhere=false
if $runhere;then
    #todo:  delete tar files on pfe
    write_quota_fcst
    
    arrdexp=($( cat $fdata_pfetar_size | cut -d' ' -f2 | rev | cut -d'/' -f3- | rev | sort -V | uniq )) 
    wmessage "Num Dir : ${#arrdexp[@]}"
    wmessage
    cnt_delfile=0
    for dexp in ${arrdexp[@]};do
    
        #todo:  number of files to delete
        arrftar=($( cat $fdata_pfetar_size | grep $dexp | cut -d' ' -f2 )) 
        (( ${#arrftar[@]} == 0 )) && continue
    
        #todo:  check clean_completed exists
        [[ ! -f $dexp/clean_completed ]] && continue
    
        cnt_delfile=$(( cnt_delfile + ${#arrftar[@]} ))
    
        wmessage "$dexp"
          $blcleancomp && wmessage "  Clean completed?: Yes"
        ! $blcleancomp && wmessage "  Clean completed?: No"
        wmessage "Num File to Delete: ${#arrftar[@]}"
        wmessage "$( printf '%s\n' ${arrftar[@]} | xargs -i basename {} | sort -V | xargs -i printf '    %s\n' {} )"
        $bldel_pfe && rm -f ${arrftar[@]}  
        wmessage 
    done 
    
    wmessage "TOTAL # FILES Deleted: $cnt_delfile"
    wmessage
    write_quota_fcst
fi

runhere=true
runhere=false
if $runhere;then
    arrdexp=($( cat $fdata_pfetar_size | cut -d' ' -f2 | rev | cut -d'/' -f3- | rev | sort -V | uniq )) 
    wmessage "Num Dir : ${#arrdarc[@]}"
    wmessage
    cnt_delfile=0

    for dexp in ${arrdexp[@]};do
        fcstensm=$( echo $dexp | rev | cut -d'/' -f1-2 | rev )
        darc=$DARCH/$fcstensm

        #todo:  number of files to delete
        arrftar=($( cat $fdata_lfetar | grep $fcstensm | cut -d' ' -f2 )) 
        (( ${#arrftar[@]} == 0 )) && continue
    
        #todo:  check clean_completed exists
        [[ ! -f $dexp/clean_completed ]] && wmessage lineno = $LINENO $dexp && continue
    
        cnt_delfile=$(( cnt_delfile + ${#arrftar[@]} ))
    
        wmessage "$darc"
        wmessage "$dexp"
          $blcleancomp && wmessage "  Dexp Clean completed?: Yes"
        ! $blcleancomp && wmessage "  Dexp Clean completed?: No"
        wmessage "Num File to Delete: ${#arrftar[@]}"
        wmessage "$( printf '%s\n' ${arrftar[@]} | xargs -i basename {} | sort -V | xargs -i printf '    %s\n' {} )"
        #$bldel_lfe && rm -f ${arrftar[@]}  
        wmessage 
    done 
    
    wmessage "TOTAL # FILES Deleted: $cnt_delfile"
fi

exit
