#!/usr/bin/env bash

#description:   
inputcheck(){
    [[ -z $fmonth ]] && [[ -z $fyear ]] && exit

    local srcme=$( echo ${arrposarg[0]} | cut -d'_' -f2 )
    [[ "$srcme" != "$hstshort" ]] && die "source file is not for this host (select srcme_${hstshort}_* file)"

    return
}

check_favail() {
    #description: check if ftar exists
    local _arr=( "$@" )

    local   _arrfavail=($( get_itemavail ${_arr[@]} ))
    local _arrfunavail=($( get_itemunavail ${_arr[@]} ))

    #todo:  set a boolean and write out missing files. 
    if (( ${#_arrfunavail[@]} == 0 ));then
        local _blftaravail=true
    else
        local _blftaravail=false
    fi
    #echo $_blftaravail
    echo ${_arrfunavail[@]}
}

extract_daily(){
    #description: extract daily files
    local _arr=( "$@" )
    local ftar _yyyymm _dinsst _din _fdate _arrfavail _numdays _numf _blnumf
    local _dcollsst_sym

    local _arrfavail=($( get_itemavail ${_arr[@]} ))
#wmessage lineno = $LINENO 
#ahand_warr ${_arrfavail[@]} 
#exit
    for ftar in ${_arrfavail[@]};do 
        _yyyymm=$( basename $ftar | cut -d'.' -f3 )
        _dinsst=$( dirname $ftar | sed 's#'$DFCST'#'$FILEIN'#g' | sed 's#/'$collsst'##g' )
        _fdate=$( echo $(dirname $ftar ) | rev | cut -d'/' -f3 | rev )

        #todo:  get a number of days in a given months
        _numdays=$( cal $( echo $_yyyymm | cut -c5- ) $( echo $_yyyymm | cut -c1-4 ) | awk 'NR==1 {MON_YEAR=$1 " " $2};NF {DAYS = $NF}; END {print DAYS}' )
        _blnumf=false

        #todo:  extract tar
        [[ ! -d $_dinsst ]] && mkdir -p $_dinsst
        tar --keep-newer-files --warning=none -xf $ftar -C $_dinsst
        
        #todo:  check a number of daily files.
        #20151102.geosgcm_sst.20160102_0900z.nc4
        _numf=$( find $_dinsst/* -type f -name "$_fdate.$collsst*.${_yyyymm}??_????z*.nc4" 2>/dev/null | grep -v daily | wc -l )
#wmessage lineno = $LINENO _numf _numdays = $_numf $_numdays
        if (( $_numf == $_numdays ));then 
            _blnumf=true
        else
            _blnumf=false
        fi

        if ! $_blnumf ;then
            wmessage "$_dinsst does not have all files"
            blreadytoproceed=false
            break
        fi

    done

    return
}

get_fout() {
    #description:   get required file name and its path
    local _arricall=( "$@" )
    local _lastfdate=${_arricall[-1]}
    local _yyyymm_twomonplus=$( fcal_nextmonth $fmonth $fyear )
    local icdate _arrout fout
    
    #20151102.geosgcm_sst.daily.201511.nc4.tar
    for icdate in ${_arricall[@]};do
        
        #todo: get output file name
        fout=$icdate.$collsst.$_yyyymm_twomonplus.nc4.tar
        _arrout+=($( echo ${arrens4[@]} | sed 's#[^ ]* *#'$DFCST/$icdate/'&#g' | xargs -L 1 printf '%s'"/$collsst/${fout}"'\n' ))

        #todo:   do the same for extra ensembles for the last icdate.
        if (( $icdate == $_lastfdate ));then
            _arrout+=($( echo ${arrens10[@]} | sed 's#[^ ]* *#'$DFCST/$icdate/'&#g' | xargs -L 1 printf '%s'"/$collsst/${fout}"'\n' ))
        fi
    done
    echo ${_arrout[@]}     
}


get_itemavail() {
    #description:   get available files.
    local _arr=( "$@" )
    local f _arrout

    for f in ${_arr[@]};do
        if [[ -f $f ]];then 
            _arrout+=( $f )
        fi
    done

    echo ${_arrout[@]}
}

get_itemunavail() {
    #description:   get available files.
    local _arr=( "$@" )
    local f _arrout

    for f in ${_arr[@]};do
        if [[ ! -f $f ]];then 
            _arrout+=( $f )
        fi
    done

    echo ${_arrout[@]}
}

set_fmonthfyr() {
    #description:   calcuate forecast year and month.
    local _results=$( fcal_setfmonfyr_bash )
    fyear=${_results:0:4}
    fmonth=${_results:4:2}

    #get_cal

    return
}

archive_pfe2lfe () {
    #description:   archive necessary daily output and output text files.
    [[ -z $blrsync ]] && blrsync=false

    #todo:  create sfhitc input batch file. 
    if ! $blrsync;then
        msg_newfile $fshiftin
        local arrftar_arch=($( printf "$hstarc:%s\n" ${arrftar[@]} | sed 's#'$DFCST'#'$darch/sst'#g' | sed 's#'$collsst'/##g' ))
        paste <(printf '%s\n' ${arrftar[@]}) <( printf '%s\n' ${arrftar_arch[@]} ) | sed 's#\t# #g' >> $fshiftin
        echo $dout/$fwinner $hstarc:$darch/$fwinner >> $fshiftin
        echo $dout/$fplt $hstarc:$darch/$fplt >> $fshiftin
        echo $dout/$findexes $hstarc:$darch/$findexes >> $fshiftin
        [[ ! -f $fshiftin ]] && die "$fshiftin does not exist" 
    fi 

    wmessage "$(date +'%m/%d/%Y %H:%M' ) Start Archiving:" 
    wmessage
    
    #todo:  archive ftar on lfe 
    if $blrsync;then
        wmessage "archive sst files..."

        for ftar in ${arrftar[@]};do
            local fdir=$( echo ${ftar#$DFCST/} | cut -d'/' -f1-2 )
            local fname=$( echo $ftar | rev | cut -d'/' -f1 | rev )
            local ddst=$DARCH/$reg/sst/$fdir        
            
            #todo:  check if destination dir exists on lfe
            if (( $writetofile == 1 ));then
                ssh -q $hstarc test -d $ddst >> $fmessage 2>&1
                (( $? > 0 )) && ssh -q $hstarc mkdir -p $ddst >> $fmessage 2>&1
                
                ssh -q $hstarc rsync -az $ftar $DARCH/$reg/sst/$fdir/ >> $fmessage 2>&1
            else
                ssh -q $hstarc test -d $ddst 
                (( $? > 0 )) && ssh -q $hstarc mkdir -p $ddst 
                
                ssh -q $hstarc rsync -az $ftar $DARCH/$reg/sst/$fdir/ 
            fi
        done

        wmessage "archiveing output files..."
        
        #todo:  archive output files 
        if (( $writetofile == 1 ));then
            ssh -q $hstarc rsync -az $dout/*_${icyyyymm}_* $DARCH/$reg/ >> $fmessage 2>&1
        else
            ssh -q $hstarc rsync -az $dout/*_${icyyyymm}_* $DARCH/$reg/
        fi

    else
        if (( $writetofile == 1 ));then
            /usr/local/bin/shiftc --no-cron --no-mail --wait -d < $fshiftin >> $fmessage
            local status_sftc=$?
        else
            /usr/local/bin/shiftc --no-cron --no-mail --wait -d < $fshiftin
            local status_sftc=$?
        fi
        (( $status_sftc == 0 )) && [[ -f $fshiftin ]] && rm -f $fshiftin
    fi

    wmessage

    #cp_outputs
    archfcheck

    return
}

archive_dis2lfe() {
    #description:   archive sst file in $dout/sst dir insted of on lfe/dirac.
    local _arr=( "$@" )
    local ftar 

    #/discover/nobackup/projects/gmao/t2ssp/GEOSS2S3/GEOS_util/aborovik_sst/20150630/ens505/geosgcm_sst/20150630.geosgcm_sst.daily.201508.nc4.tar
    #dout=$cdir/output/pckwinners/scratch
    #ftar=sst/20150630/ens505/20150630.geosgcm_sst.daily.201508.nc4.tar
    #darch=/u/gmaofcst/GEOS_S2Sv3
    #
    ##ref:https://unix.stackexchange.com/a/496181
    #sup rsync -aR $dout/./$ftar lfe:$darch

    wmessage "$(date +'%m/%d/%Y %H:%M' ) Start Archiving:" 
    wmessage
    
    #todo:  create symbolic links in scratch/sst dir
    for ftar in ${_arr[@]};do 
        local _dinsst=$( dirname $ftar | sed 's#'$DFCST'#'$dnino/sst'#g' | sed 's#'$collsst'##g' )

        #todo:  create dir & subdir under scratch dir in a way that should be created in destination dir
        [[ ! -d $_dinsst ]] && mkdir -p $_dinsst

        #!!!!! NOTE: as of 05/20/2021 sst tar is copied into dout/scratch sst
        #todo:  create symlink of a file (not a dir) or copy file
        #+++++ cd to dnino/sst (start) +++++
        cd $_dinsst
        #ln -sf $ftar .
        rsync -az $ftar .
        cd - >/dev/null
        #+++++ cd to dnino/sst ( end ) +++++
    done
        
    #todo:  copy fwinner and findexes files into sst
    [[ -f $dout/$fwinner ]] && cp -p $dout/$fwinner $dnino/
    [[ -f $dout/$findexes ]] && cp -p $dout/$findexes $dnino/
    [[ -f $dout/$fplt ]] && cp -p $dout/$fplt $dnino/

    if [[ $hstarc != $hst ]];then
        #todo:  transfer file with -R opt in rsync. Don't forget to add '.' in front of dir which 
        #       may need to be created at destination.
        #ref:https://unix.stackexchange.com/a/496181
        sup rsync -arLR $dnino/./* $hstarc:$darch

        #todo:  check if files are archive properly.
        archfcheck
        rm -rf $dnino
    else
        local arrdnino=($( find $dnino/* -maxdepth 0 -type d )) 
        
        #todo:  delete dir in darch
        [[ -d $darch/sst ]] && rm -rf $darch/sst
        mv -f $dnino/* $darch/
        rm -rf $dnino
    fi
    
    #todo:  copy contents of fwinner and findexes into fmessage
    cp_outputs

    return
}

cooking_links() {
    local fwin=$1 

    #todo:  get icyyyymm
    #winners_nino3.4_201508_201510.txt
    local icyyyymm=$( basename $fwin | cut -d'_' -f4 )

    [[ ! -d $dwinpfe/$icyyyymm ]] && mkdir -p $dwinpfe/$icyyyymm
    
    if [[ "$hstshort" == "pfe" ]];then
        ssh -q $hstarc test -d $dwinlfe/$icyyyymm 2>/dev/null
        local _status_dexist=$?
        (( $_status_dexist > 0 )) && ssh -q $hstarc mkdir -p $dwinlfe/$icyyyymm 2>/dev/null

    elif [[ "$hstshort" == "dis" ]];then
        [[ ! -d $dwinlfe/$icyyyymm ]] && mkdir -p $dwinlfe/$icyyyymm
    fi

    arrwin=($( cat $fwin | sed 's# #/#g' ))

    #todo:  create ens1-10 symbolic links in dyyyymm (incl. lou)
    create_symlinks_nbp $icyyyymm ${arrwin[@]}
    $blhstarcavail && create_symlinks_arc $icyyyymm ${arrwin[@]}

    return
}


cp_outputs(){
    #description:   copy contents of fwinner and findexes into fmessage
    msg_wheader_userdefined 40 - "$fwinner"
    wmessage "$( cat $dout/$fwinner )"
    wmessage
    msg_wheader_userdefined 40 - "$findexes"
    wmessage "$( cat $dout/$findexes )"
    wmessage
    return
}

archfcheck() {
    #description:   check if files are archive properly.
    local cntfail=0
    local mycmd="ssh -q $hstarc"
    local _arrf=( $fplt $findexes $fwinner )
    local _status cntfail afile

    #note: add sup to mycmd if this script is executed on discover.
    [[ $hst =~ "dis"* ]] && mycmd="sup $mycmd"
        
    #todo:   check if three outputs exist on lfe
    for afile in ${_arrf[@]};do
        $mycmd "test -f $darch/$afile"
        _status=$?
        cntfail=$(( cntfail + _status ))
    done

    if (( $cntfail > 0 ));then
        die "one or more of these files, $( basename $fplt ), $( basename $findexes), & $( basename $fwinner ), are not archived on $hstarc"
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
#beg
hst=$( hostname )
if [[ "$hst" =~ "pfe"* ]];then
    :
elif [[ "$hst" =~ "r"* ]];then
    #note:  this is for host other than pfe @ NAS which has a name starting with 'r'
    blnode=true
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
     
    trap clean_dir EXIT
fi


if [[ ! $cdir/func_fcst.sh ]];then 
    echo "$cdir/func_fcst.sh does not exist";exit
else
    source $cdir/func_fcst.sh
fi

hstshort=$( get_host )
writetofile=0
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        do stratification. Current year and month are calculated and used
        for IC YYYYMM if option i is not selected.
        
        Usage : ./$(basename "$0") [-chw] [-i YYYYMM ] srcme_file
            
        Input:
            A source file, which set various vars for a set of runs (i.e. srcme_pfe_fcst)

        options:
            -i  ic YYYYMM ( use 201507 for test runs )
            -c  clean unwanted files
            -h  show this help text
            -w  write stdout/err in a file
"

OPTIND=1
while getopts ':hwi:' option; do
    case "$option" in
        i)  icyyyymm=$OPTARG;; 
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

[[ -z  ${arrposarg[@]} ]] && die "an input is missing"
(( ${#arrposarg[@]} != 1 )) && die "1 input is required"
#================================================================================
#                               Check User Inputs
#================================================================================
#todo: set forecast month & year.
if [[ -n $icyyyymm ]];then
     icmonth=$( echo $icyyyymm | cut -c5- )
      icyear=$( echo $icyyyymm | cut -c1-4 )

     fyear=$( fcal_nextmonth $icmonth $icyear | cut -c1-4 )
    fmonth=$( fcal_nextmonth $icmonth $icyear | cut -c5- )

#note: As of 11/30/2022, icyyyymm is a requied input and this elif
#       should be removed.
#elif [[ -z $fmonth ]] && [[ -z $fyear ]];then 
#    set_fmonthfyr
#      icyear=$( fcal_previousmonth $fmonth $fyear | cut -c1-4 )
#     icmonth=$( fcal_previousmonth $fmonth $fyear | cut -c5- )
#    icyyyymm=$icyear$icmonth
fi

inputcheck
source ${arrposarg[0]}
[[ -z $reg ]] && die "($strscr) reg is undefined"
[[ -z $thisvar ]] && die "($strscr) thisvar is undefined"
[[ -z $DARCH ]] && die "($strscr) DARCH is undefined"
#================================================================================
#                             Set Host Specific Vars
#================================================================================
#todo: check the name of system (discover or pfe) and set variables.
hst=$( hostname )
if $blnode || [[ $hst =~ "pfe"* ]];then
  
    [[ -z $hstarc ]] && hstarc=lfe 

    if ssh -q $hstarc true >>/dev/null 2>&1;then 
        blhstarcavail=true
    else
        blhstarcavail=false
    fi

    ##note: 06/27/2023 Due to a problem with shiftc, archiving is skipped for now.
    #blhstarcavail=false

    darch=$DARCH/$reg
    siteam_mod=/nobackup/gmao_SIteam/modulefiles
    blarch=true

    if $blhstarcavail;then 
        ssh -q $hstarc "test -d $hstarc:$darch"
        (( $? > 0 )) && ssh -q $hstarc "mkdir -p $darch"
    fi

    . /usr/share/modules/init/bash

    [[ ! "$MODULEPATH" =~ "$siteam_mod" ]] && \
    export MODULEPATH=$MODULEPATH:/nobackup/gmao_SIteam/modulefiles

    module purge
    #module load comp-gcc/9.2.0 
    module load python/GEOSpyD/Ana2019.10_py3.7

elif [[ $hst =~ "discover"* ]];then 

    #!!!!! NOTE: AS OF 05/20/2021, darch IS SET TO CURRENT DIR. CHANGE THIS LATER !!!!!
    #note:  if you have no where to archive files, use dnino as darch. 
    darch=$DARCH/$reg
    darch=$cdir/output/$strscr/$reg
    blarch=true

    [[ "$( echo $darch | cut -d'/' -f1  )" == "u" ]] && hstarc=$hstarc || hstarc=$hst

    if [[ $hstarc != $hst ]];then
        sup ssh -q $hstarc test -d $darch
        (( $? > 0 )) && ssh -q $hstarc "mkdir -p $darch"
        #(( $? > 0 )) && die "$hstarc:$darch does not exist"
    elif [[ $hstarc == $hst ]];then
        [[ ! -d $darch ]] && mkdir -p $darch
    fi

    . /usr/share/modules/init/bash
    module load comp/gcc/9.2.0
    module load python/GEOSpyD/Ana2019.10_py3.7 
fi

#================================================================================
#                                 Set Variables
#================================================================================
#mid
[[ -n $rstexpid ]] && strexpid=$strexpid$rstexpid

dmess=$cdir/message
dstdout=$cdir/stdout/$strscr/$strdout
dout=$cdir/output/$strscr/$strdout
dnino=$dout/scratch/$reg
dtmpdata=$cdir/data/submit
#dtmpdata=$cdir/data_$strscr
FILEIN=$dout/scratch/$icyyyymm/input
FILEOT=$dout/scratch/$icyyyymm
blftaravail=false
blreadytoproceed=true

strat_yyyy=$( echo $( fcal_nextmonth $fmonth $fyear ) | cut -c1-4 )
strat_yyyymm=$( fcal_nextmonth $fmonth $fyear )

fmessage=$dmess/message_${strscr}_${strexpid}_$icyyyymm
fshiftin=$cdir/${strscr}_${strexpid}_${icyyyymm}_shiftcin

findexes=${reg}_${strexpid}_${icyyyymm}_${strat_yyyymm}.txt
 fwinner=winners_${reg}_${strexpid}_${icyyyymm}_${strat_yyyymm}.txt
    fplt=subsample_${reg}_${strexpid}_${icyyyymm}_${strat_yyyymm}.png
 STRATFL=$FILEOT/$fwinner
FNITOPLT=$FILEOT/$fplt
 COLLSST=$collsst
 THISVAR=$thisvar

cdate=$( date +%Y%m%d_%H%M )
 strf=data_submit_${strexpid}_${strscr}_winners_

arrdatestamps4=($( fcal_icall $fmonth $fyear ))
arrdatestamps10=( ${arrdatestamps4[-1]} )
arrlabel4=($( printf '%s\n' ${arrdatestamps4[@]} | xargs -i printf '{}%s\n'  ${arrens4[@]} ))
arrlabel10=($( printf '%s\n' ${arrdatestamps10[@]} | xargs -i printf '{}%s\n'  ${arrens10[@]} ))
arrlabel=( ${arrlabel4[@]} ${arrlabel10[@]} )

#note:  original script - text_indices_S2S_fcst_full_v3_daily_select.py
#                         seasonal_S2S_v3_daily_stratify.py
#                         plot_daily_plume_winners_v3.py
  scrind=s2sv3_calcind_nino34.py
scrstrat=s2sv3_stratification.py
  scrplt=s2sv3_plot_plume.py

#todo:  exit if fmessage exists in dstdout
[[ -f $dstdout/$fmessage ]] && exit

(( $writetofile == 1 )) && msg_newfile $fmessage
[[ ! -d $dstdout ]] && mkdir -p $dstdout
[[ ! -d $dmess   ]] && mkdir -p $dmess

export FILEOT FILEIN STRATFL FNITOPLT COLLSST THISVAR
#================================================================================
#                                  Main Process
#================================================================================
#main
#todo:  get required file name and its fullpath
arrftar=($( get_fout ${arrdatestamps4[@]} ))

#todo:  check if all files are available. set blftaravail var.
arrsst_unavail=($( check_favail ${arrftar[@]} ))

#todo:  exit if there are missing files.
#!  $blftaravail && die "some or all $collsst daily tar files are missing"
if (( ${#arrsst_unavail[@]} > 0 )); then 
    wmessage "Some or all sst daily tar files are NOT available"
    ahand_warr ${arrsst_unavail[@]} 
    exit
fi

#note:  this delete FILEIN dir if exists. Sometime, file extractions don't seem to work
#       properly and this will delete it and create new dir.
fhand_newdir $FILEIN

#todo:  extract daily files for each tar.
    
msg_wheader_userdefined 80 = "$icyyyymm - Extract Daily sst Files"
extract_daily ${arrftar[@]}

if $blreadytoproceed;then
    msg_wheader_userdefined 80 = "IC YYYYMM - $icyyyymm"
    wmessage "$( date )"
    wmessage "hostname : $( hostname )"
    wmessage
    
    msg_wheader_userdefined 40 - "Input Tar Files"
    ahand_warr "${arrftar[@]}"
    wmessage
    
    msg_wheader_userdefined 40 - "$scrind"
    wmessage "         IC YYYYMM : $icyyyymm"
    wmessage "The 2nd Fcst Month : $strat_yyyymm"
    wmessage
    wmessage "IC YYYYMMDD  2nd Month     Ens   # of Days in the 2nd Month"
    wmessage "----------- ----------- -------- --------------------------"
    
    for label in ${arrlabel[@]};do
        icyyyymmdd=${label:0:8}
        ensn=${label:8}
        
        #198205_nino3.4_19820307_ens1.sst
        fsst=$FILEOT/${strat_yyyymm}_${reg}_${icyyyymmdd}_${ensn}.sst
    
        if (( $writetofile == 1 ));then
            ./$scrind -s $strat_yyyymm -i $icyyyymmdd -e $ensn -r $reg >> $fmessage 2>&1
        else
            ./$scrind -s $strat_yyyymm -i $icyyyymmdd -e $ensn -r $reg 
        fi
        
        if [[ ! -f $fsst ]];then 
            wmessage "Stratification Output Not Found : $fsst" 
            blreadytoproceed=false
            break
        fi
    done
fi

#todo:   select winners.
if $blreadytoproceed;then
    wmessage
    msg_wheader_userdefined 40 - "$scrstrat"
    
    if (( $writetofile == 1 ));then
        ./$scrstrat -s $strat_yyyymm -i $icyyyymm ${arrintens[@]} >> $fmessage 2>&1
    else 
        ./$scrstrat -s $strat_yyyymm -i $icyyyymm ${arrintens[@]}
    fi
    
    if [[ ! -f $STRATFL ]];then 
        wmessage "Winner file does not exist - $STRATFL" 
        blreadytoproceed=false
    fi
fi

#todo: save individual nino34 indexes into one file
if $blreadytoproceed;then
    #+++++ cd FILEOT (start) +++++
    cd $FILEOT
    shopt -s nullglob
    more *.sst | cat >> $findexes
    shopt -u nullglob
    cd - >/dev/null
    #+++++ cd FILEOT ( end ) +++++

    if [[ ! -f $FILEOT/$findexes ]];then 
        wmessage "Indexes files does not exist - $FILEOT/$findexes"
        blreadytoproceed=true
    fi
fi

#todo:  create a plot
if $blreadytoproceed;then
    wmessage
    msg_wheader_userdefined 40 - "$scrplt"
    wmessage "*Note : Nothing will be written here if $scrstrat produces no errors."
    
    if (( $writetofile == 1 ));then
        ./$scrplt -s $strat_yyyymm -i $icyyyymm -r nino3.4 ${arrintens[@]} >> $fmessage 2>&1
    else
        ./$scrplt -s $strat_yyyymm -i $icyyyymm -r nino3.4 ${arrintens[@]}
    fi
    
    if [[ ! -f $FNITOPLT ]] ;then
        wmessage "Plot does not exist - $FNITOPLT"
        blreadytoproceed=false
    else
        wmessage
    fi
fi


if $blreadytoproceed;then
    #todo:  move three files to dout
    mv {$STRATFL,$FNITOPLT,$FILEOT/$findexes} $dout/
    
    #todo:  archiving
    msg_wheader_userdefined 40 - "Output Location"
    
    if $blarch ;then 
        if $blnode || [[ $hst =~ "pfe"* ]];then
        wmessage "sst Archive Location : $hstarc:$darch/sst/$icyyyymm"
        wmessage "                Plot : $hstarc:$darch/$fplt"
        wmessage "        Indexes File : $hstarc:$darch/$findexes"
        wmessage "         Winner File : $hstarc:$darch/$fwinner"
        wmessage "                     : $hst:$dout/$fwinner"
        wmessage
    
        elif $blarch && [[ $hst =~ "dis"* ]];then
            wmessage "sst Archive Location : $hstarc:$darch/sst/$icyyyymm"
            wmessage "                Plot : $hstarc:$darch/$fplt"
            wmessage "        Indexes File : $hstarc:$darch/$findexes"
            wmessage "         Winner File : $hstarc:$darch/$fwinner"
            wmessage "                     : $hst:$dout/$fwinner"
            wmessage
        fi
    fi
        
    #todo:  setupwinners:
    if [[ -f $dout/$fwinner ]];then
        msg_wheader_userdefined 40 - "Location of Symlinks to Winners"
    
        #todo:  make symlinks to winner exp dir on both pfe & lfe; 
        cooking_links $dout/$fwinner
        
        #todo:  write a data_submit_winners_* for run_submit.sh
        [[ -f $dtmpdata/$strf${icyyyymm}_$cdate ]] && rm -f $dtmpdata/$strf${icyyyymm}_$cdate
        cat $dout/$fwinner | sed 's# #/#' | xargs -i echo $DFCST/{} >> $dtmpdata/$strf${icyyyymm}_$cdate
    
        if $blnode || [[ $hst =~ "pfe"* ]];then
            wmessage "   pfe:$dwinpfe"
            wmessage "   $hstarc:$dwinlfe"
        
        elif [[ $hst =~ "dis"* ]];then
            wmessage "    discover:$dwinnbp"
            wmessage "    discover:$dwinarch"
        fi
        wmessage
    fi

    cp_outputs
    
    #todo:  start archving
    if $blarch;then 
        if $blnode || [[ "$hstshort" == "pfe" ]];then

            if $blhstarcavail;then 
                msg_wheader_userdefined 40 - "Archiving"
                archive_pfe2lfe
            fi
            
        elif [[ "$hstshort" == "dis" ]];then
            msg_wheader_userdefined 40 - "Archiving"
            archive_dis2lfe ${arrftar[@]}
        fi
    fi
    
    [[ -f $fmessage ]] && mv $fmessage $dstdout/
    
    #todo:  cleaning
    #note:  fplt (plot) is removed after email is sent in run_pckwinners.sh
    #[[ -f $dout/$fplt ]] && rm -f $dout/$fplt
    if $blhstarcavail;then 
        [[ -d $FILEOT         ]] && rm -rf $FILEOT
        [[ -f $dout/$findexes ]] && rm -f $dout/$findexes
    fi
fi

module purge

