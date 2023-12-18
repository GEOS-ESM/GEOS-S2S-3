#!/usr/bin/env bash

inputcheck(){
    (( ${#arrposarg[@]} != 4 )) && die "4 inputs are required"

    [[ ! -d $dexp ]] && die "$dexp does not exist"
    [[ ! -f $srcf ]] && die "$srcf file does not exist"

    local _blnum=$( misc_isinteger $runqsub ) 
    (( $_blnum > 0 )) && die "input has to be a number ( 0 or 1)."
    
    local _arrqidall=($( res_qid_all )) 
    printf '%s\n' ${_arrqidall[@]} | grep -w $qid >/dev/null 2>&1 
    local _status_grep=$?
    (( $_status_grep > 0 )) && die "$qid reservations does not exist"

    return
}

setvars_doover(){

    #description:    set various variables
    local _dexp=$1
    
    [[ -z $_dexp ]] && die "(${FUNCNAME[0]}) a full path to an experiment dir is required input"
    
    local fcap=CAP.rc
    local fcap2=CAP_2.rc
    local fcapr=cap_restart
    local fcapric=cap_restartIC
    local fcapend=cap_end
    local fhis2=HISTORY_2.rc
    fhis1=HISTORY_1.rc

    local _fcapric=$_dexp/$fcapric
    local _fcapend=$_dexp/$fcapend
    local _fhis1=$_dexp/$fhis1
    local _fhis2=$_dexp/$fhis2
    local _farc_exp=$_dexp/archive/run_gcmarch.sh 
    local _fexistlfe=$_dexp/archive/gcmarch_arc
    _fgrn=$_dexp/gcm_run.j           
    _fcapr=$_dexp/$fcapr
    _fcap2=$_dexp/$fcap2
    _fcap=$_dexp/$fcap

    local _farc_default=$DBUILD/Applications/GEOSgcm_App/run_gcmarch.sh 
    local strmapl="01_0000z.nc4"
    local coll
    
    strrst=restarts

    fcstdate=$( echo $_dexp | rev | cut -d'/' -f2 | rev )
        ensm=$( echo $_dexp | rev | cut -d'/' -f1 | rev )
     rstdate=$( date -d "$fcstdate -1days " +%Y%m%d )

    darch=$DARCH/$fcstdate/$ensm
    drstsrc=$DRST/$rstdate/$ensm

    fpst=$dexp/post/gcm_post.j

    blwin=$( exp_checkwinner $_dexp $ddata_pckwinners ) 
    blrerun=$( grep RERUN $_fgrn| head -1 | rev | cut -d' ' -f1 | rev |  tr '[:upper:]' '[:lower:]' )
    blocnpert=$( fcal_ocnpert $_dexp ) 
    
    [[ -f $_dexp/$farc_comp ]] && blarccomp=true || blarccomp=false
    [[ -f $_dexp/$fcln_comp ]] && blclncomp=true || blclncomp=false

    #note:  cap_restart yyyymmdd
    [[ -f $_fcapr   ]] &&    capr_yyyymmdd=$( cat $_fcapr   | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
    [[ -f $_fcapric ]] &&  capric_yyyymmdd=$( cat $_fcapric | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
    [[ -f $_fcapend ]] && realend_yyyymmdd=$( cat $_fcapend | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )

    #todo:  calculate the end date for three-month run.
    #note:  end date is 3 month from fcst date, not rst date.
    if [[ -n $fcstdate ]];then
        nummonth1=3
        
        strxmonth=$( nextXmonths $( date -d $fcstdate +%Y ) $( printf '%01g' $( date -d $fcstdate +%m ) ) 0 $(( nummonth1 + 1 ))  )
        end_mm=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f1 )
        end_year=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f2 )
        end3_yyyymmdd=$end_year$end_mm"01"
    fi

    #todo:  get a number of missing yyyymm dir
    [[ -f $_dexp/archive/$farc_note ]] && numdmiss=$( cat $_dexp/archive/$farc_note 2>/dev/null | sort -V | wc -l ) 

    #todo:  check timestamp of run_gcmarch.sh. Make sure that the script in archive is
    #       the latest version (fullmode/run_gcmarch.sh) 
    sec_farcdefault=$( stat --print='%Y' $_farc_default )
    sec_farcexp=$( stat --print='%Y' $_farc_exp )
    
    if (( $sec_farcdefault <= $sec_farcexp ));then 
        #note:  exp/archive/run_gcmarch.sh is the latest version
        blarcscr=true
    else
        blarcscr=false
    fi

    #todo:  make sure that gcm_post.j has -ignore_nan option 
    grep ignore_nan $fpst >/dev/null 2>&1
    local _status_grep=$?
    
    if (( $_status_grep > 0 ));then 
        #todo:  edit gcm_post.j
        edit_gcmpst
        local _status_editgcmpost=$?  
    
        if (( $_status_editgcmpost == 0 ));then 
            blsedfpst=true
        else
            blsedfpst=false
        fi
    
        #todo:  check again if gcm_post.j has ignore_nan 
        grep ignore_nan $fpst >/dev/null 2>&1
        local _status_grep=$?
        if (( $_status_grep > 0 ));then 
            blfpst=false
        else
            blfpst=true
        fi
    else
        blfpst=true
    fi

    #todo:  get collection and yyyyymm that are missing from holding dir, and determine
    #       minimum yyyymm
    arrmiss_collyyyymm=($( miss_collyyyymm $_dexp ))

    miss_yyyymm_1st=($( printf '%s\n' ${arrmiss_collyyyymm[@]} | cut -d':' -f2 | sort -V | uniq | head -1 ))
    miss_yyyymm_2nd=($( printf '%s\n' ${arrmiss_collyyyymm[@]} | cut -d':' -f2 | sort -V | uniq | tail -1 ))

    #todo:  figure out which segment to rerun based on existing restarts on lfe. 
    #note:  If this exp is winner, re-run only necessary segment. If this exp ran only 3months, 
    #       rerun from the beginning.
    #restarts.e19940101_21z.tar
    local _arrrst_yyyymm=($( grep $strrst $_fexistlfe | xargs -i basename {} | cut -d'.' -f2 | cut -c2-9 ))
    #beg_yyyymmdd=$( printf '%s\n' ${_arrrst_yyyymm[@]} | xargs -i bash -c "(( {} < ${miss_yyyymm_1st}01 )) && echo {}" | sort -V | uniq | tail -1 )
    beg_yyyymmdd=$capric_yyyymmdd

    local _frst_yyyymmddhhz_arcinit=$( grep $strrst $_fexistlfe | rev | cut -d' ' -f1 | rev | sort -V | head -1 )
    
    if $blwin;then 
        end_yyyymmdd=$( printf '%s\n' ${_arrrst_yyyymm[@]} | xargs -i bash -c "(( {} > ${miss_yyyymm_2nd}01 )) && echo {}" | sort -V | uniq | head -1 ) 
        #frst_yyyymmddhhz_arcrerun=($( grep $strrst $_fexistlfe | grep $beg_yyyymmdd | rev | cut -d' ' -f1 | rev ))
        frst_yyyymmddhhz_arcrerun=$_frst_yyyymmddhhz_arcinit
        frst_nextseg_arc=($( grep $strrst $_fexistlfe | grep $end_yyyymmdd | rev | cut -d' ' -f1 | rev ))

    else
        end_yyyymmdd=$end3_yyyymmdd
        frst_yyyymmddhhz_arcrerun=$_frst_yyyymmddhhz_arcinit
    fi

    #note:  if doover has to start from capric, then used restarts in DRST 
    (( $capric_yyyymmdd == $beg_yyyymmdd )) && rstsource=$drstsrc || rstsource=$frst_yyyymmddhhz_arcrerun

    (( $beg_yyyymmdd == ${miss_yyyymm_1st}01 )) && blalert=true || blalert=false
    
    #todo:  compare the INITIAL condition rst (NOT rst to be used for segment re-run) to make sure that rst in $DRST is the same with 
    #       restart tar created by gcm_run.j
    if $blocnpert;then 
        bltarready=$( tarcheck $_frst_yyyymmddhhz_arcinit )
        $bltarready && blcomppert=$( compare_ocnpertrst $_frst_yyyymmddhhz_arcinit ) || blcomppert=false
    else
        bltarready=$( tarcheck $_frst_yyyymmddhhz_arcinit )
        $bltarready && blcomppert=$( compare_atmpertrst $_frst_yyyymmddhhz_arcinit ) || blcomppert=false
    fi

    return
}


miss_collyyyymm() {
    local _dexp=$1
    local _fnote=$_dexp/archive/$farc_note
    local _arr=()

    if [[ -f $_fnote ]];then 
        local _arr=($( cat $_fnote | rev | cut -d' ' -f1 | cut -d'/' -f1-2 | rev | sed 's#/#:#g' ))
    else
        local fstderr=$_dexp/$farc_stderr
        local arrfpst_sub=($( grep gcm_post. $fstderr 2>/dev/null | grep _glo_ 2>/dev/null | grep -v Make | tr -s '[:space:]' | sed 's#^ *##g' | sort -V ))
        local arrfpst=($( printf '%s\n' ${arrfpst_sub[@]} | sort -V | uniq ))
    
        for fpst in ${arrfpst[@]};do 
    
            local arrfoutmiss=($( exp_checkfpst $_dexp $fpst ))
            
            if (( ${#arrfoutmiss[@]} > 0 ));then 
                coll=$( echo $fpst | cut -d'.' -f2 )
                yyyymm=$( echo $fpst | cut -d'.' -f3 | cut -c2- )
                _arr+=( $coll:$yyyymm )  
            fi
        done

    fi
   
    echo ${_arr[@]} 
}

edit_gcmpst(){
    #description: Edit post/gcm_post.j

    local _fsed=$dexp/post/${strscr}_sedfile
    [[ -f $_fsed ]] && rm -f $_fsed

    cat > $_fsed << EOF  
s# @YYYYMM# @YYYYMM -ignore_nan#g
s#nobackupp18#nobackupp28#g
EOF
    sed -i.${strscr}.bak -f $_fsed $fpst
    local _status_sed=$?

    [[ -f $_fsed ]] && rm -f $_fsed 

    return $_status_sed
}

proc_atm() {
    #description:   rename rst files for atm

    #+++++ cd to _drstyyyymmdd_hhz (start) +++++
    cd $_drstyyyymmdd_hhz
    
    local _numfield=$( find * -type f -name "$fcstdate.*" | awk -F'.' '{print NF}' | sort -V | uniq | tail -1 )

    if [[ -n $_numfield ]] && (( $_numfield > 1 ));then 
        paste <(printf 'mv %s\n' $( find * -type f -name "$fcstdate.*" )) <(printf '%s\n' $( find * -type f -name "$fcstdate.*" | cut -d'.' -f2 )) > $tmpscr
        chmod 755 $tmpscr
        ./$tmpscr
        [[ -f $tmpscr ]] && rm -f $tmpscr
    fi 
    cd - >/dev/null
    #+++++ cd to _drstyyyymmdd_hhz ( end ) +++++

    return
}

proc_ocn(){
    #description:   rename RESTART dir

    if [[ ! -d $_drstyyyymmdd_hhz/RESTART ]] ;then 
        local _drstocn=$( find $_drstyyyymmdd_hhz/* -maxdepth 0 -mindepth 0 -type d -name "RESTART*" )

        #+++++ cd to _drstyyyymmdd_hhz/RESTART (start) +++++
        #todo:  rename ocn rst files
        cd $_drstyyyymmdd_hhz

        local _dname=$( basename $_drstocn )
        mv $_dname RESTART
        
        cd - >/dev/null
        #+++++ cd to _drstyyyymmdd_hhz/RESTART ( end ) +++++
    fi
    return 
}

cp_rstlastseg(){
    #description:  move/copy last segment restarts into a different dir.
    local _arrrst_lstseg=($( find $dexp/* -maxdepth 0 -type f -name '*_rst' 2>/dev/null )) #| grep -v _1mo_glo_ )) 

    [[ ! -d $dscr/rst_lstseg ]] && mkdir -p $dscr/rst_lstseg

    local _dempty=$( find $dscr/rst_lstseg -maxdepth 0 -empty )
    if [[ -n $_dempty ]];then
        wmessage "Move restarts from the last segement into $( echo $dscr/rst_lstseg | rev | cut -d'/' -f1-2 | rev ) dir"
        #rsync -az ${_arrrst_lstseg[@]} $dscr/rst_lstseg/ 
        mv ${_arrrst_lstseg[@]} $dscr/rst_lstseg/ 
        local _status_rsync=$?
    else
        #note:   08/05/2022 for now, set this to 0
        local _status_rsync=0
    fi
   
    #note:   make sure to copy cap_restart AFTER rst from the last seg was copied into rst_lstseg dir!! 
    wmessage "Copy cap_restart into $( echo $dscr/rst_lstseg | rev | cut -d'/' -f1-2 | rev ) dir"
    [[ ! -f $dscr/rst_lstseg/cap_restart ]] && cp -p $dexp/cap_restart $dscr/rst_lstseg

    return $_status_rsync
}

cp_rst(){
    local _frst_arctar=$1
    local _rstyyyymmdd_hhz=$( basename $_frst_arctar | cut -d'.' -f2 | cut -c2- )
    local _drstyyyymmdd_hhz=$dscr/rst_$_rstyyyymmdd_hhz

    #todo:  transfer restarts to dexp 
    #rsync -arz $_drstyyyymmdd_hhz/* $dexp/
    /usr/local/bin/shiftc --no-cron --wait -r -L $_drstyyyymmdd_hhz/* $dexp/
    return $?
}

rst_diffcheck(){
    local _drstsrc=$1
    local _status_out=0
    local _status=999
    local frst

    cd $_drstsrc
    local arrfrst=($( find * -name '*_rst' -o -name '*.nc' )) 
    cd - >/dev/null 

    for frst in ${arrfrst[@]}; do
        local _file1=$_drstsrc/$frst
        local _file2=$dexp/$frst
        
        /home3/mathomp4/bin/nccmp -dmfgsB -q $_file1 $_file2 >/dev/null 2>&1
        local _status_nccmp=$?
        if (( $_status_nccmp > 0 ));then 
            _status_out=$_status
            wmessage "diff $frst"
        fi

    done 
    return $_status_out
}

extract_rst(){
    local _frst_arctar=$1
    local _rstyyyymmdd_hhz=$( basename $_frst_arctar | cut -d'.' -f2 | cut -c2- )
    local _drstyyyymmdd_hhz=$dscr/rst_$_rstyyyymmdd_hhz
    local _status=999

    [[ -z $1 ]] && return $_status

    [[ ! -d $_drstyyyymmdd_hhz ]] && mkdir -p $_drstyyyymmdd_hhz

    wmessage "Extract $( basename $_frst_arctar ) "
    local _dempty=$( find $_drstyyyymmdd_hhz -maxdepth 0 -empty )
    [[ -n $_dempty ]] && ssh -q lfe tar --skip-old-files -xf $_frst_arctar -C $_drstyyyymmdd_hhz/  
    
    proc_atm
    proc_ocn
    
    local _numatm_rst_cnt=$( find $_drstyyyymmdd_hhz/* -maxdepth 0 -type f -name '*_rst' | wc -l ) 
    local _numocn_rst_cnt=$( find $_drstyyyymmdd_hhz/RESTART/* -maxdepth 0 -type f -name 'ocean_*.nc' | wc -l ) 
    
    (( $numatm_rst == $_numatm_rst_cnt )) && (( $numocn_rst == $_numocn_rst_cnt )) && _status=0 || _status=1  

    return $_status
}

compare_ocnpertrst(){
    #description:   compare perturbation files in restart.eyyyymmdd_hhz.tar (created by gcm_run.j)
    #               and ones in $DRST/rstyyyymmdd/ensm
     
    local _frst_arctar=$1
    local _rstyyyymmdd_hhz=$( basename $_frst_arctar | cut -d'.' -f2 | cut -c2- )
    local _drstyyyymmdd_hhz=$_dexp/${strscr}/rst_$_rstyyyymmdd_hhz
    local _ocnpert

    [[ ! -d $_drstyyyymmdd_hhz ]] && mkdir -p $_drstyyyymmdd_hhz

    local _dempty=$( find $_drstyyyymmdd_hhz -maxdepth 0 -empty )
    
    if [[ -n $_dempty ]];then 
        for _ocnpert in ${arrocnpert[@]};do  
            ssh -q lfe "tar -xf $_frst_arctar -C $_drstyyyymmdd_hhz/ --wildcards --no-anchored "'"'"*RESTART*/$_ocnpert"'"'" "
        done
    
        
        if [[ ! -d $_drstyyyymmdd_hhz/RESTART ]] ;then 
            local _drstocn=$( find $_drstyyyymmdd_hhz/* -maxdepth 0 -mindepth 0 -type d -name "RESTART*" )

            #+++++ cd to _drstyyyymmdd_hhz/RESTART (start) +++++
            #todo:  rename ocn rst files
            cd $_drstyyyymmdd_hhz
    
            _dname=$( basename $_drstocn )
            mv $_dname RESTART
            
            cd - >/dev/null
            #+++++ cd to _drstyyyymmdd_hhz/RESTART ( end ) +++++
        fi
    
        cd - >/dev/null 
    fi
    
    
    #todo:  compare two perturbation files
    for _ocnpert in ${arrocnpert[@]};do 
        local _file1=$drstsrc/RESTART/$_ocnpert
        local _file2=$_drstyyyymmdd_hhz/RESTART/$_ocnpert
        /home3/mathomp4/bin/nccmp -dmfgsB -q $_file1 $_file2 >/dev/null 2>&1
        local _status_nccmp=$?
        if (( $_status_nccmp > 0 ));then 
            local _bl=false
            break 
        else
            local _bl=true
        fi
    done 
    
    [[ -d $_drstyyyymmdd_hhz ]] && rm -rf $_drstyyyymmdd_hhz

    echo $_bl
}

tarcheck(){
    #description:   check if rst tar is corrupted.

    local _frst_arctar=$1
    ssh -q lfe "tar -tf $_frst_arctar "  >>/dev/null 2>&1
    (( $? > 0 )) && local _bltarready=false || local _bltarready=true

    echo $_bltarready
}

compare_atmpertrst(){
    #description:   compare perturbation files in restart.eyyyymmdd_hhz.tar (created by gcm_run.j)
    #               and ones in $DRST/rstyyyymmdd/ensm
     
    local _frst_arctar=$1
    local _rstyyyymmdd_hhz=$( basename $_frst_arctar | cut -d'.' -f2 | cut -c2- )
    local _drstyyyymmdd_hhz=$dexp/${strscr}/rst_$_rstyyyymmdd_hhz
    local _pert

    [[ -d $_drstyyyymmdd_hhz ]] && rm -rf $_drstyyyymmdd_hhz
    mkdir -p $_drstyyyymmdd_hhz

    local _dempty=$( find $_drstyyyymmdd_hhz -maxdepth 0 -empty )
    if [[ -n $_dempty ]];then 
        for _pert in ${arratmpert[@]};do  
            ssh -q lfe "tar -xf $_frst_arctar -C $_drstyyyymmdd_hhz/ --wildcards --no-anchored "'"'"*${_pert}*"'"'" "
        done
    
        proc_atm 
    fi 
    
    #todo:  compare two perturbation files
    for _pert in ${arratmpert[@]};do 
        local _file1=$drstsrc/$_pert
        local _file2=$_drstyyyymmdd_hhz/$_pert
        /home3/mathomp4/bin/nccmp -dmfgsB -q $_file1 $_file2 >/dev/null 2>&1
        local _status_nccmp=$?
        if (( $_status_nccmp > 0 ));then 
            local _bl=false
            break 
        else
            local _bl=true
        fi
    done 

    #[[ -d $_drstyyyymmdd_hhz ]] && rm -rf $_drstyyyymmdd_hhz
    echo $_bl
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
elif [[ "$hst" =~ "discover"* ]];then
    echo "this script works only on pfe (for now)";exit
else
    exit
fi

#strscr=$(basename "$0" | cut -d'_' -f2 | cut -d'.' -f1 )
strscr=$(basename "$0" | cut -d'_' -f2-3 | cut -d'.' -f1 )
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cdir=$SCRIPTPATH

[[ -z $cdir || ! -d $cdir ]] &&  echo "cdir is undefined"  && exit

cd $cdir

[[ -z $strscr ]] && strscr=$(basename "$0" | cut -d'.' -f1 )

#todo:  check if this script is invoked as child script
[[ -n $RUNCHILD ]] && blchild=true || blchild=false

if ! $blchild;then
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
fi
trap clean_dir EXIT

if [[ ! $cdir/func_fcst.sh ]];then 
    echo "$cdir/func_fcst.sh does not exist";exit
else
    source $cdir/func_fcst.sh
fi

#note:  this script is only for hindcast yyyyymmdd < 19960101
writetofile=0
rundebug=0
optf=false
optd=false
optr=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        run an experiment for one or two segmenst that has missing ouput in 
        holding/collection/YYYYMM dir. 

        Usage: ./$(basename "$0") [-chw] qid dexp srcme_file [1 or 0] 

        Input:
            Reserved queue ID
            Experiment Dir, Full Path
            Source file, which set various vars for a set of runs.
            Enter 1 to submit a job after exp dir is ready; 0 for setting up exp dir only

        options:
            -m  stdout file name
            -c  clean unwanted files
            -h  show this help text
            -w  write stdout/err in a file

"

OPTIND=1
while getopts ':hwcm:' option; do
    case "$option" in
        c)  clean_dir; exit 0;;
        m)  fmsg=$OPTARG;; 
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

#================================================================================
#                               Check User Inputs
#================================================================================
i=0
 qid=${arrposarg[$i]};i=$(( i + 1 ))
dexp=${arrposarg[$i]};i=$(( i + 1 ))
srcf=${arrposarg[$i]};i=$(( i + 1 ))
runqsub=${arrposarg[$i]};i=$(( i + 1 ))

inputcheck

source $srcf

[[ -z $DARCHRST || -z $DRST ]] && die "DARCHRST or DRST is undefied"
#================================================================================
#                             Set Host Specific Vars
#================================================================================
if $blnode || [[ $hst =~ "pfe"* ]];then
    basepath=$PATH
    basepath=/home6/gmaofcst/.cargo/bin:/home6/gmaofcst/knakada/bin:/u/scicon/tools/bin:/u/gmaofcst/bin:$basepath
    basepath=/home6/gmaofcst/bin:/PBS/bin:/usr/local/bin:/nobackup/gmao_SIteam/Baselibs/latest-mpiuni/Linux/bin:$basepath
    basepath=/usr/local/bin:/u/scicon/tools/bin:/u/gmaofcst/bin:/home6/gmaofcst/.cargo/bin:$basepath
    basepath=/home5/knakada/.cargo/bin:/u/scicon/tools/bin:/u/knakada/bin:/usr/local/bin:$basepath
    basepath=/usr/local/sbin:/usr/bin:/bin:/usr/X11R6/bin:/PBS/bin:/usr/sbin:/sbin:/opt/c3/bin:$basepath
    export BASEPATH=/opt/sgi/sbin:/opt/sgi/bin:/opt/c3/bin:/opt/sgi/sbin:/opt/sgi/bin:/opt/sgi/sbin:/opt/sgi/bin:$basepath
    export PATH=$BASEPATH
    export TERM=xterm-256color
    umask 0022
elif [[ $hst =~ "discover"* ]];then 
    :
fi

#================================================================================
#                    Set Global Variables and Source Scripts
#================================================================================
#mid
thishst=$( get_host )
cdate=$( date +%Y%m%d_%H%M )

dmess=$cdir/message
dstdout=$cdir/stdout/$strscr/$strdout
ddata_pckwinners=$cdir/output/pckwinners/$strexpid
dscr=$dexp/$strscr
dtmpdata=$cdir/data/submit

strf=data_submit_${strexpid}_${strscr}_
fdata_quickarch=$cdir/data/quickarch/data_quickarch_fcst
farc_note=note_gcmarch
farc_comp=gcmarch_deloutcompleted
fcln_comp=clean_completed
fcap3=CAP_3.rc
tmpscr=rename.sh
farc_stderr=archive/stderr_gcmarch

numatm_rst=22
numocn_rst=11

strspace1=+10

maxsec=600
maxmin=$( echo "$maxsec / 60" | bc  )

arrocnpert=( ocean_temp_salt.res.nc ocean_velocity.res.nc )
arratmpert=( fvcore_internal_rst moist_internal_rst )

blsuccess=false 

setvars_doover $dexp 

fmessage=$dmess/message_${strscr}_${strexpid}_${fcstdate}${ensm}

stralert="***Alert : Segment start date and missing YYYYMM are the same. 
           Depending on collection frequency, the first output 
           may not be produced."

msg_subject="${thishst}.${strscr}: $strexpid"
feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

[[ ! -d $dstdout ]] && mkdir -p $dstdout
[[ ! -d $dmess   ]] && mkdir -p $dmess
[[ ! -d $DRST    ]] && mkdir -p $DRST
#================================================================================
#                                  Main Process
#================================================================================
#main
  
msg_wheader_userdefined 80 = "$(TZ=America/New_York date +'%m/%d/%Y %H:%M' ) $fcstdate/$ensm"

wmessage " exp location : $dexp"
wmessage "arch location : $darch"
wmessage
wmessage "Directory cleaned?          : ${blclncomp^}"
wmessage "Archiving completed         : ${blarccomp^}"
wmessage "RERUN in gcm_run.j          : ${blrerun^}"
wmessage "Winner?                     : ${blwin^}"
wmessage "cap_restartic               : $capric_yyyymmdd"
wmessage "cap_restart                 : $capr_yyyymmdd"
wmessage "3month run end              : $end3_yyyymmdd"
wmessage "winner run end              : $realend_yyyymmdd"
wmessage
[[ -n $numdmiss ]] && wmessage "# of Missing YYYYMM Dir     : $numdmiss"
wmessage "The First Missing YYYYMM    : $miss_yyyymm_1st"
wmessage "The Last  Missing YYYYMM    : $miss_yyyymm_2nd"
wmessage "Segment Run Start YYYYMMDD  : $beg_yyyymmdd"
wmessage "Segment Run End   YYYYMMDD  : $end_yyyymmdd"
if (( ${#arrmiss_collyyyymm[@]} > 0 ));then 
    wmessage "Missing Collection & YYYYMM :"    
    wmessage "$( printf "    %s\n" ${arrmiss_collyyyymm[@]} | sed 's#:# #g' )"
    wmessage
fi
$blalert && wmessage "$stralert" 
wmessage
#wmessage "Restart File               : $frst_yyyymmddhhz_arcrerun"
$blwin && wmessage "Next Segment Restart File   : $frst_nextseg_arc" && wmessage

if (( $capric_yyyymmdd == $beg_yyyymmdd ));then 
    wmessage "Source of Restarts          : $drstsrc"
else
    wmessage "Source of Restarts          : $frst_yyyymmddhhz_arcrerun"
fi

wmessage
wmessage "Using The latest version of run_gcmarch.sh  : ${blarcscr^} (  $( date -d@$sec_farcexp)  )"
wmessage "(   Default run_gcmarch.sh timestamp        : $( date -d@$sec_farcdefault )  )"
[[ -n $blsedfpst ]] && wmessage "gcm_post.j has been editted?                : ${blsedfpst^}"
wmessage "gcm_post.j has ignore_nan?                  : ${blfpst^} "

if ! $bltarready; then 
    wmessage "Restart Tar File Corrupted                  : True ( $( basename $frst_yyyymmddhhz_arcrerun ) )"
    wmessage "    **Skip restart comparison**"

elif $bltarready;then 
    #note:  restarts in DRST and restarts in initial rst tar file were compared to make
    #       sure that they are the same
      $blocnpert && wmessage "Initial Ocean Pert Rst identical?           : ${blcomppert^}"
    ! $blocnpert && wmessage "Initial Atm Pert Rst identical?             : ${blcomppert^}"
    
    #todo:  exit if any of boolean are false
    #! $blarcscr   && wmessage "Update run_gmcarch.sh and re-run it before doover. Exit."              && exit
    ! $blfpst     && wmessage "Update gcm_post.j by adding ignore_nan option before doover. Exit."    && exit
    ! $blcomppert && wmessage "Initial conditions are different between what was used in exp and what's in DRST. Exit." && exit

elif $blwin && ! $bltarready;then 
    #todo:  if this exp is winner and tar is corrupted, exit here
    exit
fi

#wmessage "exit @$LINENO"; exit

wmessage 

if $blwin && (( $end_yyyymmdd != $realend_yyyymmdd ));then 
wmessage "exit @$LINENO" && exit
    #note:  08/30/2022  a segment run doesn't work unless collection rst are saved in restarts.e*.tar 
    #                   For now, re-run should start from the beginning.
    #Add these function:
    #1. take care of cap_restart when the end of a segment run is != cap_end

    #todo:  move/copy last segment restarts into a different dir.
    cp_rstlastseg
    status_cprstlastseg=$?
    (( $status_cprstlastseg > 0 )) && die "FAILED : copy/move restarts from the last segment" && exit

    #todo:  extract restarts
    if (( $capric_yyyymmdd != $beg_yyyymmdd ));then 
        extract_rst $frst_yyyymmddhhz_arcrerun 
        status_extractrst=$?
        (( $status_extractrst > 0 )) && die "FAILED : extract $( basename frst_yyyymmddhhz_arcrerun )" && exit

    fi

    #todo:  create a symlink CAP.rc
    if  (( $end_yyyymmdd == $realend_yyyymmdd ));then 
        #todo:  make a symlink to CAP_2.rc
        [[ -L $_fcap ]] && unlink $_fcap
        cd $dexp
        ln -s $fcap2 CAP.rc
        cd - >/dev/null 
        [[ ! -L $_fcap ]] && die "FAILED : Making symlink to CAP.rc" && exit

    else
        #todo:  create CAP_3.rc 
        wmessage "Create $fcap3 and create symlink to it" 
        
        fcap3_enddate="END_DATE:     $end_yyyymmdd 210000"
        fcap2_enddate=$( grep -i end_date $_fcap2 ) 

        [[ -f $dscr/$fcap3 ]] && rm -f $dscr/$fcap3

        sed -e "s#${fcap2_enddate}#${fcap3_enddate}#" $_fcap2 >> $dscr/$fcap3
        grep "${fcap3_enddate}" $dscr/$fcap3 >/dev/null 2>&1
        status_grep=$?
        (( $status_grep > 0 ))   && die "FAILED : Edit $fcap3" && exit

        #todo:  make symlink to CAP_3.rc. Make sure to have CAP_3.rc in dexp instead of in $strscr dir.
        [[ -L $_fcap ]] && unlink $_fcap
        cd $dexp
        cp -p $dscr/$fcap3 .
        ln -s $fcap3 CAP.rc
        cd - >/dev/null 
        [[ ! -L $_fcap ]] && die "FAILED : Making symlink to CAP.rc" && exit
    fi

    wmessage "Transfer restarts to dexp and diff them afterwards"

    if (( $capric_yyyymmdd == $beg_yyyymmdd ));then 
        getrst $drstsrc $dexp
        status_cprst=$?
        (( $status_cprst > 0 )) && die "FAILED : transfer restarts from $drstsrc" && exit

        rst_diffcheck $drstsrc
        status_rstdiff=$?
        (( $status_rstdiff > 0 )) && die "Restarts Differ : some restart files are different" && exit
    else
        :
        ##todo:  change cap_restart
        ##[[ -f $dscr/rst_lstseg/cap_restart ]] && msg_newfile $_fcapr
        #msg_newfile $_fcapr
        #wmessage "Edit cap_restart & save it in $( basename $dscr )" 
        #
        #echo "$beg_yyyymmdd 210000" >> $_fcapr
        #fcap_yyyymmdd=$( cat $_fcapr | cut -d' ' -f1 ) 
        #(( $fcap_yyyymmdd != $beg_yyyymmdd )) && die "FAILED : Edit cap_restart" && exit
        #[[ ! -f $dscr/cap_restart ]] && cp -p $_fcapr $dscr/

        #rstyyyymmdd_hhz=$( basename $frst_yyyymmddhhz_arcrerun | cut -d'.' -f2 | cut -c2- )
        #drstyyyymmdd_hhz=$dscr/rst_$rstyyyymmdd_hhz

        #cp_rst $frst_yyyymmddhhz_arcrerun
        #status_cprst=$? 
        #(( $status_cprst > 0 )) && die "FAILED : copy restarts from rst_$rstyyyymmdd_hhz" && exit

        #rst_diffcheck $drstyyyymmdd_hhz
        #status_rstdiff=$?
        #(( $status_rstdiff > 0 )) && die "Restarts Differ : some restart files are different" && exit
    fi

    #todo:  replace nobackupp2 with nobackupp18 if any
    sed -i.${strscr}.bkp -e "s#nobackupp18#nobackupp28#g" $dexp/gcm_run.j
    status_sed=$?
    (( $status_sed > 0 )) && die "FAILED : replace nobackupp2 with nobackupp18, and nobackupp18 with nobackupp28" && exit

    #todo:  check the current queue name in gcm_run.j 
    queue_cur=$( grep "#PBS -q " $_fgrn | grep -Ev '^##|^$|^ ' | rev | cut -d' ' -f1 | rev )
    if [[ "$qid" != normal && "$qid" != long ]];then 
        thisqueuename=$( res_resname $qid )
    else
        thisqueuename=$qid
    fi

    if [[ "$queue_cur" != "$thisqueuename" ]];then 
        wmessage "Edit PBS statement in gcm_run.j"
        exp_editqgrn $qid $_fgrn
    fi

    wmessage 

    if (( $runqsub == 0 ));then 
        wmessage "... making data files for run_submit.sh script ..."
        printf '%s\n' $dexp >> $dtmpdata/${strf}${cdate}
        wmessage
        [[   -s $dtmpdata/$strf$cdate ]] && wmessage "Created : $dtmpdata/${strf}${cdate}" && blsuccess=true
        [[ ! -s $dtmpdata/$strf$cdate ]] && wmessage "!!!!! FAILED to Create : $dtmpdata/${strf}${cdate}" && blsuccess=false

    elif (( $runqsub == 1 ));then 
        wmessage "Ready to submit gcm_run.j"
        cd $dexp
        /PBS/bin/qsub gcm_run.j 
        status_qsub=$?
        cd - >/dev/null 
        
        #todo:  double check if job is on queue 
        wmessage
        wmessage "Qstat:"
        qstat -u $USER -W fmt_Queue="-maxw 40" | grep R${fcstdate}$ensm 
        status_grep=$?
        (( $status_qsub == 0 && $status_grep == 0 )) && blsuccess=true || blsuccess=false

        ! $blsuccess && wmessage "!!!!! FAILED to submit gcm_run.j !!!!!"
    fi
else
#wmessage "exit @$LINENO" && exit

    #3mon run should be setup by gcmsetup as long as blcomppert == true
    [[ -d $dexp ]] && mv $dexp $DFCST/$fcstdate/._$ensm
    
    [[ -d $DFCST/$fcstdate/._$ensm ]] && bldexpmv=true || bldexpmv=false

    ssh -q lfe test -d $darch
    (( $? == 0 )) && ssh -q lfe mv $darch $DARCH/$fcstdate/._$ensm

    ssh -q lfe test -d $DARCH/$fcstdate/._$ensm
    status_mv=$?
    (( $status_mv == 0 )) && bldarchmv=true || bldarchmv=false 

      $bldexpmv && ! $bldarchmv && mv $DFCST/$fcstdate/._$ensm $dexp
    ! $bldexpmv &&   $bldarchmv && ssh -q lfe mv $DARCH/$fcstdate/._$ensm $darch

    ! $bldexpmv  && die "FAILED to move $dexp"  && exit
    ! $bldarchmv && die "FAILED to move $darch" && exit 

    wmessage "Moved ens directores:"
                stat --printf='%.16y %n\n' $DFCST/$fcstdate/._$ensm 
    ssh -q lfe "stat --printf='%.16y %n\n' $DARCH/$fcstdate/._$ensm"
    wmessage
    wmessage "** This exp is 3month run and re-run the entire 3months. (gcmsetup.sh is executed)"
    wmessage

    RUNCHILD=1
            
    thisscr=gcmsetup.sh
    thismessage=$cdir/message/message_gcmsetup_${strexpid}_${fcstdate}$ensm
    [[ -f $thismessage ]] && rm -f $thismessage
    scrname=${strscr}_gcmsetup_${strexpid}_$fcstdate$ensm 

    wmessage "Screen name : $scrname"
    
    screen -dmS $scrname bash -c "./$thisscr -q $qid -m $thismessage $srcf $runqsub $dexp >> $thismessage 2>&1"  

    #todo:   wait for screen session to complete or deleted if exceeds maxsec
    blinitialnote=false
    totsec=0
    totmin=0
    
    sec0=$( date +%s )
    sec1=$sec0
    
    numscr=$( screen -ls | grep -i detached | grep $scrname | wc -l )
    while (( $numscr > 0 ));do 
        sec2=$( date +%s )
        sec_diff=$(( sec2 - sec1 ))
        totsec=$(( sec2 - sec0 ))
        totmin=$( echo "$totsec / 60" | bc )
    
        #todo:  break out for both for loop (hence, 2)
        if (( $totsec >= $maxsec ));then
            screen -XS $scrname quit 2>/dev/null
    
            wmessage "Killed Screens: $scrname"
            wmessage "Delete $arrdexp "
            rm -rf $dexp 2>/dev/null
            wmessage "exit \$LINENO"
            exit 
        fi
    
        if ! $blinitialnote ;then 
            wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screens - running ... will wait for max of $maxmin minutes"
            blinitialnote=true
    
        elif (( $sec_diff > 60 ));then
            wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screens - running ... waited for $totmin min"
            sec1=$( date +%s )
        fi
        
        numscr=$( screen -ls | grep -i detached | grep $scrname | wc -l )
    done

    if (( $runqsub == 0 ));then
        wmessage "... making data files for run_submit.sh script ..."
        printf '%s\n' $dexp >> $dtmpdata/${strf}${cdate}
        wmessage
        [[   -s $dtmpdata/$strf$cdate ]] && wmessage "Created : $dtmpdata/${strf}${cdate}" && blsuccess=true
        [[ ! -s $dtmpdata/$strf$cdate ]] && wmessage "!!!!! FAILED to Create : $dtmpdata/${strf}${cdate}" && blsuccess=false

    elif (( $runqsub == 1 ));then 
        wmessage
        wmessage "qstat:"
        qstat -u $USER -W fmt_Queue="-maxw 40" | grep R${fcstdate}$ensm 
        status_grep=$?
        (( $status_grep == 0 )) && blsuccess=true || blsuccess=false

        ! $blsuccess && wmessage "!!!!! FAILED to submit gcm_run.j !!!!!"
    fi
fi 

#todo:  mv fmessage to a final destination
if $blsuccess && [[ -f $fmst ]] ;then 
    if $blwin;then
        fmsg_name=$( basename $fmsg )_W
    else
        fmsg_name=$( basename $fmsg )
    fi
    [[ -f $dstdout/$fmsg_name ]] && rm -f $dstdout/$fmsg_name
    [[ -f $fmsg ]] && mv $fmsg $dstdout/$fmsg_name

    #todo: write dexp into $fdata_quickarch
    echo $dexp >> $fdata_quickarch
fi 


exit


