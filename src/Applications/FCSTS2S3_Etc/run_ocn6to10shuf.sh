#!/usr/bin/env bash

inputcheck(){
    (( ${#arrposarg[@]} != 1 )) && die "1 inputs are required"

    ! $optr && ! $optd && ! $optf && die "opt d, r or f are required"

    $optr && [[ -z $userbegyyyymm || -z $userendyyyymm ]] && die "YYYYMM range is a required input for option r"

    if $optd;then
        #todo:  check user input. it has to < 199601
        (( $useryyyymmdd >= 19960101 )) && die "opt d input has to be less than 19960101"
    fi

    if $optr; then
        local int1=$( misc_isinteger $startyyyymm ) 
        local int2=$( misc_isinteger $endyyyymm ) 
        (( $int1 > 0 || $int2 > 0 )) && die "YYYYMM range have to be numbers delimited by a hyphen"
    fi 
    return
}


movedir_pfe(){
    local _icyyyymmdd=$1
    local _dicyyyymmdd=$DFCST/$_icyyyymmdd
    local num_mvcomp=10
    local _ens11t15 _ens06t10

    #ens6  => ens11
    #ens7  => ens12
    #ens8  => ens13
    #ens9  => ens14
    #ens10 => ens15

    #todo:  hide (for now) ens11-15
    local cnt_mvcompleted=0
    for _ens11t15 in ${arrens11t15[@]};do
        local _blmvcompleted=false
        local _ensm_06t10=ens$(( _ens11t15 - 5 ))
        local _ensm_11t15=ens$_ens11t15
        local _fnote_11t15=$_dicyyyymmdd/$_ensm_11t15/$note_base${_ensm_06t10}to${_ensm_11t15}_${_icyyyymmdd}

        #note:  requied condtions to hide ens11-15
        #       - [[ -d _ens11t15 && ! -f _fnote_11t15 ]]

        if [[ -d $_dicyyyymmdd/.$_ensm_11t15 && -d $_dicyyyymmdd/$_ensm_11t15 && -f $_fnote_11t15 ]];then
            wmessage "    Hide completed : $_icyyyymmdd/$_ensm_11t15"
            _blmvcompleted=true

        elif [[ ! -d $_dicyyyymmdd/.$_ensm_11t15 && -d $_dicyyyymmdd/$_ensm_11t15 && ! -f $_fnote_11t15 ]];then
            wmessage "    Hide old $_ensm_11t15"
            mv $_dicyyyymmdd/$_ensm_11t15 $_dicyyyymmdd/.$_ensm_11t15
            local _status_mv=$?
            (( $_status_mv == 0 )) && [[ -d $_dicyyyymmdd/.$_ensm_11t15 ]] && _blmvcompleted=true || _blmvcompleted=false

        else
            wmessage "***Hide FAILED : $_ensm_11t15"
            _blmvcompleted=false
        fi
        
        #todo:  count mv sucesses
        $_blmvcompleted && cnt_mvcompleted=$(( cnt_mvcompleted + 1 ))
    done
   
    wmessage  

#wmessage "@$LINENO" $cnt_mvcompleted
#exit
    #todo:  mv ens6-10 to ens11-15
    for _ens06t10 in ${arrens06t10[@]};do
        local _blmvcompleted=false
        local _ensm_06t10=ens$_ens06t10
        local _ensm_11t15=ens$(( _ens06t10 + 5 ))
        local _fnote_06t10=$_dicyyyymmdd/$_ensm_06t10/$note_base${_rstyyyymmdd}$_ensm_06t10
        local _fnote_11t15=$_dicyyyymmdd/$_ensm_11t15/$note_base${_ensm_06t10}to${_ensm_11t15}_${_icyyyymmdd}
        local farchdelout_comp=$_dicyyyymmdd/$_ensm_06t10/archive/gcmarch_deloutcompleted
   
        #note:  requied condtions to move ens6-10 to ens11-15
        #       - [[ ! -d _ensm_11t15  &&   -d _ensm_06t10  ]]
        #       - [[ ! -f _fnote_11t15 && ! -f _fnote_06t10 ]]

        if [[ -f $_fnote_11t15 ]];then 
            wmessage "    Move Completed : $_ensm_06t10 to $_ensm_11t15"
            _blmvcompleted=true

        elif [[ -d $_dicyyyymmdd/$_ensm_06t10 && ! -d $_dicyyyymmdd/$_ensm_11t15 && ! -f $_fnote_06t10 && ! -f $_fnote_11t15 && \
                -f $farchdelout_comp ]];then

            #todo:  mv ens6-10 to ens11-15
            wmessage "Move $_ensm_06t10 to $_ensm_11t15"
            mv $_dicyyyymmdd/$_ensm_06t10 $_dicyyyymmdd/$_ensm_11t15
            local _status_mv=$?
            
            if (( $_status_mv == 0 )) && [[ -d $_dicyyyymmdd/$_ensm_11t15 ]]; then 
                wmessage "    Move Completed : $_ensm_06t10 to $_ensm_11t15"
                _blmvcompleted=true
                createnote_ens11to15 $_ensm_06t10 $_ensm_11t15 $ftar_ocn6to10
                echo "$note_ocnpert_ens11t15" > $_fnote_11t15 
            else
                _blmvcompleted=false
                wmessage "***Move FAILED : $_ensm_06t10 to $_ensm_11t15"
            fi

        else
            _blmvcompleted=false
            wmessage "***Move FAILED : $_ensm_06t10 to $_ensm_11t15"
        fi

        #todo:  count mv sucesses
        $_blmvcompleted && cnt_mvcompleted=$(( cnt_mvcompleted + 1 ))

    done

    (( $num_mvcomp == $cnt_mvcompleted )) && local _blmvdone=true || local _blmvdone=false

    echo $_blmvdone
}


movedir_lfe(){
    local _icyyyymmdd=$1
    local _darch_icyyyymmdd=$DARCH/$_icyyyymmdd
    local _dicyyyymmdd=$DFCST/$_icyyyymmdd
    local num_mvcomp=10
    local _ens11t15 _ens06t10
    
    #ens6  => ens11
    #ens7  => ens12
    #ens8  => ens13
    #ens9  => ens14
    #ens10 => ens15

    #todo:  hide (for now) ens11-15
    local cnt_mvcompleted=0
    for _ens11t15 in ${arrens11t15[@]};do
        local _blmvcompleted=false
        local _ensm_06t10=ens$(( _ens11t15 - 5 ))
        local _ensm_11t15=ens$_ens11t15
        local _fnote_11t15=$_darch_icyyyymmdd/$_ensm_11t15/$note_base${_ensm_06t10}to${_ensm_11t15}_${_icyyyymmdd}

        #note:  requied condtions to hide ens11-15
        #       - [[ -d _ens11t15 && ! -f _fnote_11t15 ]]

        ssh -q lfe test -d $_darch_icyyyymmdd/.$_ensm_11t15 2>/dev/null 
        (( $? == 0 )) && local bldexist_ens11t15_hidden=true || local bldexist_ens11t15_hidden=false

        ssh -q lfe test -d $_darch_icyyyymmdd/$_ensm_11t15 2>/dev/null 
        (( $? == 0 )) && local bldexist_ens11t15=true || local bldexist_ens11t15=false

        ssh -q lfe test -f $_fnote_11t15 2>/dev/null 
        (( $? == 0 )) && local blfnote_11t15=true || local blfnote_11t15=false

#wmessage "@$LINENO"
#wmessage $bldexist_ens11t15_hidden $_darch_icyyyymmdd/.$_ensm_11t15
#wmessage $bldexist_ens11t15 $_darch_icyyyymmdd/$_ensm_11t15
#wmessage $blfnote_11t15 $_fnote_11t15
#break
#exit
       
        #if $bldexist_ens11t15_hidden && $bldexist_ens11t15 && $blfnote_11t15;then
        if $blfnote_11t15;then
            wmessage "    Hide completed : $_icyyyymmdd/$_ensm_11t15"
            _blmvcompleted=true

        elif ! $bldexist_ens11t15_hidden && $bldexist_ens11t15 && ! $blfnote_11t15;then
            wmessage "    Hide old $_ensm_11t15"
            ssh -q lfe mv $_darch_icyyyymmdd/$_ensm_11t15 $_darch_icyyyymmdd/.$_ensm_11t15
            local _status_mv=$?
            (( $_status_mv == 0 )) && [[ -d $_darch_icyyyymmdd/.$_ensm_11t15 ]] && _blmvcompleted=true || _blmvcompleted=false

        else
            wmessage "***Hide FAILED : $_ensm_11t15"
            _blmvcompleted=false
        fi
        
        #todo:  count mv sucesses
        $_blmvcompleted && cnt_mvcompleted=$(( cnt_mvcompleted + 1 ))

    done

    wmessage

    #todo:  mv ens6-10 to ens11-15
    for _ens06t10 in ${arrens06t10[@]};do

        local _blmvcompleted=false
        local _ensm_06t10=ens$_ens06t10
        local _ensm_11t15=ens$(( _ens06t10 + 5 ))
        local _fnote_06t10=$_darch_icyyyymmdd/$_ensm_06t10/$note_base${_rstyyyymmdd}$_ensm_06t10
        local _fnote_11t15=$_darch_icyyyymmdd/$_ensm_11t15/$note_base${_ensm_06t10}to${_ensm_11t15}_${_icyyyymmdd}
        local _fnote_11t15_base=$cdir/$( basename $_fnote_11t15 )

        [[ -f $_fnote_11t15_base ]] && rm -f $_fnote_11t15_base


        #note:  requied condtions to move ens6-10 to ens11-15
        #       - [[ ! -d _ensm_11t15  &&   -d _ensm_06t10  ]]
        #       - [[ ! -f _fnote_11t15 && ! -f _fnote_06t10 ]]

        ssh -q lfe test -f $_fnote_11t15 2>/dev/null 
        (( $? == 0 )) && local blfnote_11t15=true || local blfnote_11t15=false

        ssh -q lfe test -f $_fnote_06t10 2>/dev/null 
        (( $? == 0 )) && local blfnote_06t10=true || local blfnote_06t10=false

        ssh -q lfe test -d $_darch_icyyyymmdd/$_ensm_06t10 2>/dev/null 
        (( $? == 0 )) && local bldexist_ens06t10=true || local bldexist_ens06t10=false

        ssh -q lfe test -d $_darch_icyyyymmdd/$_ensm_11t15 2>/dev/null 
        (( $? == 0 )) && local bldexist_ens11t15=true || local bldexist_ens11t15=false

#wmessage "@$LINENO"
#wmessage $blfnote_06t10 $_fnote_06t10
#wmessage $blfnote_11t15 $_fnote_11t15
#wmessage $bldexist_ens06t10 $_darch_icyyyymmdd/$_ensm_06t10
#wmessage $bldexist_ens11t15 $_darch_icyyyymmdd/$_ensm_11t15
#exit
        if $blfnote_11t15;then 
            wmessage "    Move Completed : $_ensm_06t10 to $_ensm_11t15"
            _blmvcompleted=true

        elif $bldexist_ens06t10 && ! $bldexist_ens11t15 && ! $blfnote_06t10 && ! $blfnote_11t15 ;then

            #todo:  mv ens6-10 to ens11-15
            ssh -q lfe mv $_darch_icyyyymmdd/$_ensm_06t10 $_darch_icyyyymmdd/$_ensm_11t15
            local _status_mv=$?

            ssh -q lfe test -d $_darch_icyyyymmdd/$_ensm_11t15 2>/dev/null 
            (( $? == 0 )) && local bldexist_thisd=true || local bldexist_thisd=false

            if (( $_status_mv == 0 )) && $bldexist_thisd; then 
                wmessage "    Move Completed : $_ensm_06t10 to $_ensm_11t15"
                _blmvcompleted=true
                createnote_ens11to15 $_ensm_06t10 $_ensm_11t15 $ftar_ocn6to10
                echo "$note_ocnpert_ens11t15" > $_fnote_11t15_base
                ssh -q lfe mv $_fnote_11t15_base $_fnote_11t15
            else
                _blmvcompleted=false
                wmessage "***Move FAILED : $_ensm_06t10 to $_ensm_11t15"
            fi
        else
            _blmvcompleted=false
            wmessage "***Move FAILED : $_ensm_06t10 to $_ensm_11t15"
        fi

        #todo:  count mv sucesses
        $_blmvcompleted && cnt_mvcompleted=$(( cnt_mvcompleted + 1 ))

    done

    (( $num_mvcomp == $cnt_mvcompleted )) && local _blmvdone=true || local _blmvdone=false

    echo $_blmvdone
}

write_directories(){
    local _arricyyyymmdd=( "$@" )

    for  _icyyyymmdd in ${_arricyyyymmdd[@]};do 
        local _dicyyyymmdd=$DFCST/$_icyyyymmdd

        msg_wheader_userdefined 40 - "PFE Note Timstamp - $_icyyyymmdd"

        #todo:  get all note filenames
        local arrfnote=($( printf "$DFCST/$_icyyyymmdd/ens%s\n" ${arrens11t15[@]} | xargs -i bash -c 'find {}/* -maxdepth 0 -type f -name "${note_base}*to*${_icyyyymmdd}" 2>/dev/null'  | sort -V ))

        #todo:  wirte dir with timestamps
        if (( $writetofile == 1 ));then
            printf '%s\n' ${arrfnote[@]} | xargs -i stat --print='    %.16y %n\n' {} >> $fmessage 2>&1
        else
            printf '%s\n' ${arrfnote[@]} | xargs -i stat --print='    %.16y %n\n' {}
        fi
    done
        
    wmessage

    for  _icyyyymmdd in ${_arricyyyymmdd[@]};do 

        local _darch_icyyyymmdd=$DARCH/$_icyyyymmdd

        msg_wheader_userdefined 40 - "LFE Note Timstamp - $_icyyyymmdd"

        #todo: create full path to notes
        local arrfnote=() 
        for _ens06t10 in ${arrens06t10[@]};do
            local _ensm_06t10=ens$_ens06t10
            local _ensm_11t15=ens$(( _ens06t10 + 5 ))
            local _fnote_11t15=$_darch_icyyyymmdd/$_ensm_11t15/$note_base${_ensm_06t10}to${_ensm_11t15}_${_icyyyymmdd}
            arrfnote+=( $_fnote_11t15 ) 
        done
        
        #todo:  wirte dir with timestamps
        if (( $writetofile == 1 ));then
            ssh -q lfe "printf '%s\n' ${arrfnote[@]} | sort -V | xargs -i stat --print='    %.16y %n\n' {}" >> $fmessage 2>&1
        else
            ssh -q lfe "printf '%s\n' ${arrfnote[@]} | sort -V | xargs -i stat --print='    %.16y %n\n' {}"
        fi
    done

    return

}

filter() {

    #description:   get a list of perturbation rst file to transfer and extract.
    local _arrinput=( "$@" )
    local arr=() 
    local _input

    for _input in ${_arrinput[@]};do
        local cntbug=0
        local drstic=$DRST/$_input

        #todo:  check missing ens6 to 10
        local _numdir_miss=$( printf "$drstic/ens%s\n" ${arrens06t10[@]} | xargs -i bash -c '[[ ! -d {} ]] && echo {}' | wc -l )
        (( $_numdir_miss == 0 )) && continue 

        debug_filter $_input

        #todo:  check existing ens6 to 10
        local _numdir_miss=$( printf "$DFCST/$_input/ens%s\n" ${arrens06t10[@]} | xargs -i bash -c '[[ ! -d {} ]] && echo {}' | wc -l )
        (( $_numdir_miss == 0 )) && continue 

        debug_filter $_input

        
        #todo:  check if note_ocnpet_ exists 
        local arrfnote=()
        local arrdens=($( printf "$drstic/ens%s\n" ${arrens06t10[@]} ))
        for densm in ${arrdens[@]};do
            ensm=$( basename $densm )
            arrfnote+=( $densm/${note_base}$_input$ensm )
        done
       
        local _numfnote_miss=$( printf '%s\n' ${arrfnote[@]} | xargs -i bash -c '[[ ! -f {} ]] && echo {}' | wc -l )
        (( $_numfnote_miss == 0 )) && continue 

        debug_filter $_input

        arr+=( $_input )
    done

    echo "${arr[@]}"
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

startyyyymm=198201
endyyyymm=201012

#note:  this script is only for hindcast yyyyymmdd < 19960101
yyyymmdd_thres=19960101
writetofile=0
rundebug=0
optb=false
optd=false
optf=false
optr=false
opts=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        remove ens11-15 and rename ens6-10 to ens11-15. This is to run ens6-10 wiht
        re-processed ocean perturbation.
        
        Usage: ./$(basename "$0") [-chwb] [ -r ICYYYYMM-ICYYYYMM ] [-f data file] [ -d ICYYYYMMDD ] srcme_file 

        Input:
            Source file, which set various vars for a set of runs.

        options:
            -b  run with a debug mode
            -r  IC YYYYMM range ( format: ICYYYYMM-ICYYYYMM) 
            -d  specify IC date ( format: YYYYMMDD )
            -f  data file with a list of YYYYMMDD
            -s  control and perturbation rst are transferred regardless of missing files
            -c  clean unwanted files
            -h  show this help text
            -w  write stdout/err in a file

"

OPTIND=1
while getopts ':hwcbsf:r:d:' option; do
    case "$option" in
        b)  optb=true;rundebug=1;; 
        f)  $optd && die "Cannot specify option f when opt d is selected.";
            $optr && die "Cannot specify option f when opt r is selected.";
            optf=true; fdata=$OPTARG;;
        d)  $optf && die "Cannot specify option d when opt f is selected.";
            $optr && die "Cannot specify option d when opt r is selected.";
            optd=true; useryyyymmdd=$OPTARG;;
        r)  $optd && die "Cannot specify option r when opt d is selected.";
            $optf && die "Cannot specify option r when opt f is selected.";
            optr=true; 
            userbegyyyymm=$( echo $OPTARG | cut -d'-' -f1 );
            userendyyyymm=$( echo $OPTARG | cut -d'-' -f2 );;
        s)  opts=true;; 
        c)  clean_dir; exit 0;;
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
srcf=${arrposarg[$i]};i=$(( i + 1 ))
source $srcf

inputcheck

[[ -z $DARCHRST || -z $DRST ]] && die "DARCHRST or DRST is undefied"
#================================================================================
#                             Set Host Specific Vars
#================================================================================
if [[ $hst =~ "pfe"* ]];then
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

icyyyymmdd=19951227
rstyyyymmdd=$( date -d "$icyyyymmdd -1days"  +'%Y%m%d' )
dexp=$DFCST/$icyyyymmdd
#note:  ftar_ocn6to10 is used in createnote_* function
ftar_ocn6to10=${rstyyyymmdd}_pert_ocnens6to10.tar

#dicyyyymmdd=$DFCST/$icyyyymmdd
#darch_icyyyymmdd=$DARCH/$icyyyymmdd
#ens6  => ens11
#ens7  => ens12
#ens8  => ens13
#ens9  => ens14
#ens10 => ens15



dmess=$cdir/message
dtmpdata=$cdir/data/submit
fmessage=$dmess/message_$strscr

flistlfe=wftmsz_flist
flst_lfe=$cdir/${strscr}_lfe_$flistlfe

note_base=note_ocnpert_
strocn6t10=ocn6t10
strf=data_submit_${strexpid}_${strscr}_

arrens06t10=($( seq 6 10 )) 
arrens11t15=($( seq 11 15 )) 
arrocnrstyyyymmdd=( 0130 0224 0326 0425 0530 0629 0729 0828 0927 1027 1126 1226 )
arrocnicyyyymmdd=($( printf '2000%s\n' ${arrocnrstyyyymmdd[@]} | xargs -i bash -c "date -d "'"'" {} +1days"'"'" +%m%d" ))
strocnicyyyymmdd=$( echo ${arrocnicyyyymmdd[@]} | sed 's# #|#g' )

blwheader=false

runqsub=0
thisscr=gcmsetup.sh

#note:  screen limit
maxscr=5

#note:  numsec is wailt time in sec and maxsec is total wait time limit
numsec=60
maxsec=1200
maxmin=$( echo "$maxsec / 60" | bc  )

secsleep=15

#note:  process limits
numproc_max=$( ulimit -u )
numsubtract=50
numproc_maxhere=$(( numproc_max - numsubtract ))

arrresqid=($( res_qid_all ))
qid=R13688215

printf '%s\n' ${arrresqid[@]} | grep $qid >> /dev/null 2>&1
status_grep=$?
(( $status_grep > 0 )) && die "Qid, $qid, does not exist" && exit

#note:  this var is used in gcmsetup.sh
RUNCHILD=true

msg_subject="${thishst}.${strscr}: $strexpid"

feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

[[ ! -d $dmess ]] && mkdir -p $dmess
[[ ! -d $DRST  ]] && mkdir -p $DRST
(( $writetofile == 1 )) && [[ ! -f $fmessage ]] && touch $fmessage

export RUNCHILD
#================================================================================
#                                  Main Process
#================================================================================
#main
if $optd;then
    arricyyyymmdd=( $useryyyymmdd ) 
    strmsg="$useryyyymmdd"
    msg_subject="$msg_subject ( $useryyyymmdd )"

elif $optf;then
    arricyyyymmdd=($( cat $fdata ))
    strmsg="$fdata"
    msg_subject="$msg_subject ($( basename $fdata ))"

elif $optr;then
    strmsg="$userbegyyyymm - $userendyyyymm"
    msg_subject="$msg_subject ( $strmsg )"

    #todo:  get all icyyyymmdd in yyyymm range 
    yyyymm=$userbegyyyymm
    while (( $yyyymm <= $userendyyyymm ));do
        arricyyyymm+=( $yyyymm )
        yyyy=$( echo $yyyymm | cut -c1-4 )
        mm=$( echo $yyyymm | cut -c5- )
        yyyymm=$( fcal_nextmonth $mm $yyyy )
    done
    
    #todo:  get rst dates
    for icyyyymm in ${arricyyyymm[@]};do
        arricyyyymmdd1+=($( fcal_calcfcdates $icyyyymm ))
    done

    #todo:  extract ocean pert dates
    arricyyyymmdd=($( printf '%s\n' ${arricyyyymmdd1[@]} | grep -E "$strocnicyyyymmdd" ))

    #todo:  dates has to be less than yyyymmdd_thres
    arricyyyymmdd=($( printf '%s\n' ${arricyyyymmdd[@]} | xargs -i bash -c "(( {} < $yyyymmdd_thres )) && echo {} " )) 
fi

#wmessage "@$LINENO"
#ahand_warr ${arricyyyymmdd[@]} 
#exit
blmove=false
if $optb ;then
    #todo:  print which rst file will be transferd when opt b is selected.
    wmessage "**** Skip Moving Dir ***"
    wmessage "yyyymmdd (total = ${#arricyyyymmdd[@]} )":
    ahand_warr ${arricyyyymmdd[@]}
    wmessage
    exit

elif $blmove;then 
wmessage "@$LINENO"

    #arricyyyymmdd_ready=()
    #for icyyyymmdd in ${arricyyyymmdd[@]};do
    #   ! $blwheader && msg_wheader && blwheader=true
    #    msg_wheader

    #    msg_wheader_userdefined 40 - "PFE Move Dir - $icyyyymmdd"
    #    blmvsuccess_pfe=$( movedir_pfe $icyyyymmdd ) 
    #    wmessage

    #    msg_wheader_userdefined 40 - "LFE Move Dir - $icyyyymmdd"
    #    blmvsuccess_lfe=$( movedir_lfe $icyyyymmdd ) 
    #    wmessage

    #    $blmvsuccess_pfe && $blmvsuccess_lfe && arricyyyymmdd_ready+=( $icyyyymmdd )
    #    
    #    write_directories $icyyyymmdd
    #    wmessage
    #    

    #    arrdexp1=($( printf "$DFCST/$icyyyymmdd/ens%s\n" ${arrens06t10[@]} ))  
    #    arrdexp_exist=($( printf '%s\n' ${arrdexp1[@]} | xargs -i bash -c "[[ -d {} ]] && echo {}" ))  
    #    arrdexp=($( printf '%s\n' ${arrdexp1[@]} | xargs -i bash -c "[[ ! -d {} ]] && echo {}" ))  
    #    
    #    if (( ${#arrdexp_exist[@]} > 0 ));then
    #        wmessag "ERROR: There directories still exist:"
    #        ahand_warr ${arrdexp[@]}
    #        exit
    #    fi
    #done

    #arricyyyymmdd=($( filter ${arricyyyymmdd_ready[@]} ))
    #arrfinal1=($( printf "$DFCST/%s\n" ${arricyyyymmdd[@]} ))  
    #arrfinal=($( printf "%s\n" ${arrfinal1[@]} | xargs -i printf "{}/ens%s\n" ${arrens06t10[@]}  ))

else
wmessage "@$LINENO"
    ! $blwheader && msg_wheader && blwheader=true

    arricyyyymmdd=($( filter ${arricyyyymmdd[@]} ))
    arrfinal1=($( printf "$DFCST/%s\n" ${arricyyyymmdd[@]} ))  
    arrfinal=($( printf "%s\n" ${arrfinal1[@]} | xargs -i printf "{}/ens%s\n" ${arrens06t10[@]}  ))
fi

wmessage "@$LINENO" 
#ahand_warr ${arrfinal[@]}    
#exit

        
! $blwheader && msg_wheader && blwheader=true
msg_wheader_userdefined 40 - "Setup Exp"
wmessage $hst
[[ -n $strmsg ]] && wmessage "                 User Inputs : $strmsg"
wmessage "         Total number of exp : $( printf '%+5s\n' ${#arrarch_final[@]} )"
wmessage

for input in ${arrfinal[@]};do

    fcstdate=$( echo $input | rev | cut -d'/' -f2 | rev )
    ensm=$( echo $input | rev | cut -d'/' -f1 | rev )
    thismessage=$dmess/message_gcmsetup_${strexpid}_${fcstdate}${ensm}_$strocn6t10
    [[ -f $thismessage ]] && rm -f $thismessage

    #todo:  count number of screen
    numscr=$( screen -ls | grep ${strscr}_${strexpid}_ | wc -l )
    
    #todo:  count number of processes
    numproc=$( ps aux | awk '{ print $1 }' | grep -w $USER | wc -l )


    if (( $numscr < $maxscr )) && (( $numproc < $numproc_maxhere ));then 
        if (( $runqsub == 1 ));then
            wmessage "... create & qsub $input ... $qid"
        elif (( $runqsub == 0 ));then
            wmessage "... create $input ... $qid"
        fi

        arrdexp_ready+=( $input )

        screen -dmS ${strscr}_${strexpid}_$fcstdate$ensm bash -c "./$thisscr -q $qid -m $thismessage $srcf $runqsub $input >> $thismessage 2>&1"  
#wmessage "@$LINENO" ${strscr}_${strexpid}_$fcstdate$ensm 
#wmessage "-q $qid -m $thismessage $srcf $runqsub $input >> $thismessage"

        sleep ${secsleep}s
    fi

done

#todo:  create data files for submit script if these are submitted later time (runqsub=0)
if (( $runqsub == 0 )) && (( ${#arrdexp_ready[@]} > 0 ));then
    wmessage 

    #todo:  wait until all screen session are gone.
    #note:  this is necessary when run_submit.sh and run_${strscr}.sh are executed by cron
    numscr=$( screen -ls | grep -i detached | grep $strscr | grep $strexpid | wc -l )
    totsec=0

    wmessage "$( date +'%m/%d/%Y %H:%M' ) ... $( printf '%02g\n' $numscr ) screen sessions are running ... will wait for max of $maxmin minutes ..."

    #todo:  wait until all screens are gone.
    while (( $numscr > 0 ));do
        sleep ${numsec}s

        totsec=$(( totsec + numsec ))
        totmin=$( echo "$totsec/$numsec" | bc )
        
        wmessage "$( date +'%m/%d/%Y %H:%M' ) ... $( printf '%02g\n' $numscr ) screen sessions are running ... waited for $totmin min ..."
        
        numscr=$( screen -ls | grep -i detached | grep $strscr | grep $strexpid | wc -l )
    done
       
    arrdexp_setupfail=($( printf '%s\n' ${arrdexp_ready[@]} | xargs -i bash -c '[[ ! -f {}/gcmsetup_setupready ]] && echo {}' ))
    arrdexp_submit=($( printf '%s\n' ${arrdexp_ready[@]} | xargs -i bash -c '[[ -f {}/gcmsetup_setupready ]] && echo {}' ))
#wmessage "@$LINENO"

    if (( ${#arrdexp_setupfail[@]} > 0 ));then
        wmessage "Setting Up Dir Failed:"
        if (( $writetofile == 1 ));then
            printf '    %s\n' ${arrdexp_setupfail[@]} >> $fmessage 2>&1
        else
            printf '    %s\n' ${arrdexp_setupfail[@]}
        fi
    fi

    if (( ${#arrdexp_submit[@]} > 0 ));then
        wmessage "... making data files for run_submit.sh script with dir created ( total = ${#arrdexp_submit[@]} ) ..."
        printf '%s\n' ${arrdexp_submit[@]} >> $dtmpdata/${strf}${cdate}
        wmessage
        wmessage "Created : $dtmpdata/${strf}${cdate}"
        
        for dexp in ${arrdexp_submit[@]};do
            fcstdate=$( echo $dexp| rev | cut -d'/' -f2 | rev )
            ensm=$( echo $dexp | rev | cut -d'/' -f1 | rev )
            darch=$DARCH/$fcstdate/$ensm

            createnote_ens6to10 $ensm $ftar_ocn6to10
            fnote=$note_base${fcstdate}${ensm}
            echo "$note_ocnpert_ens6t10" > $fnote 

            ssh -q lfe test -d $darch
            (( $? > 0 )) && ssh -q lfe mkdir -p $darch
            ssh -q lfe cp -p $cdir/$fnote $darch/
            
            [[ -f $fnote ]] && rm -f $fnote
        done
    fi
fi

#todo:  send email
sizef=$( stat --print='%s' $fmessage )
if (( $sizef > 0 ));then
    msg_cyberpostman "$msg_subject" "$eadds" $fmessage
    (( $? == 0 )) && rm -f $fmessage
fi



exit


