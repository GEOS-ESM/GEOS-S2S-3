#!/usr/bin/env bash

inputcheck () {
    if [[ ! -d $dexp ]];then
       die error model exp dir does not exist; exit
    fi
    return
}

filter() {
    local _dexp=$1    
    local cnt_lp=0
    local maxcheck=3
    
    local nstep=0
    local strstep=1
    if  $blwinner && (( $expenddate == $capr_yyyymmdd ));then
        nstep=$(( nstep + 1 ))
        wmessage "... step $strstep ... check cap_restart        ... pass"

    elif ! $blwinner && (( $expenddate <= $capr_yyyymmdd ));then
        nstep=$(( nstep + 1 ))
        wmessage "... step $strstep ... check cap_restart        ... pass"
    else
        wmessage "... step $strstep ... check cap_restart        ... fail ( cap_restart=$capr_yyyymmdd ; this has to be $expenddate if exp is completed)"
    fi

    strstep=$(( $strstep + 1 ))

    if [[ ! -s $_dexp/archive/gcmarch_arcempf ]];then 
        nstep=$(( nstep + 1 ))
        wmessage "... step $strstep ... check gcmarch_arcempf    ... pass"
    elif [[ -s $_dexp/archive/gcmarch_arcempf ]];then 
        wmessage "... step $strstep ... check gcmarch_arcempf    ... fail ( size of gcmarch_arcempf = $( numfmt --to=iec $( stat --print='%s\n' $_dexp/archive/gcmarch_arcempf ))"
    elif [[ ! -f $_dexp/archive/gcmarch_arcempf ]];then 
        wmessage "... step $strstep ... check gcmarch_arcempf    ... fail ( gcmarch_arcempf does not exis )"
    fi

        
    strstep=$(( $strstep + 1 ))

#wmessage lineno = $LINENO 
#wmessage $numfmonmapl_calc $numflfe_monmapl_cnt $numfsave_mpl $numfpfe_monmapl_cnt
#wmessage $numfmonpost_calc $numflfe_monpost_cnt $numfsave_mon $numfpfe_monpost_cnt 
#wmessage $numfdiu_calc     $numflfe_diu_cnt     $numfsave_diu $numfpfe_diu_cnt     
#wmessage $numfday_calc     $numflfe_day_cnt     $numfsave_day $numfpfe_day_cnt     
#wmessage $numfmom_calc     $numflfe_mom_cnt     $numfsave_mom $numfpfe_mom_cnt   
#wmessage 
#exit

    if (( $numfmonmapl_calc == $numflfe_monmapl_cnt && $numfsave_mpl == $numfpfe_monmapl_cnt && \
          $numfmonpost_calc == $numflfe_monpost_cnt && $numfsave_mon == $numfpfe_monpost_cnt && \ 
          $numfdiu_calc     == $numflfe_diu_cnt     && $numfsave_diu == $numfpfe_diu_cnt     && \ 
          $numfday_calc     == $numflfe_day_cnt     && $numfsave_day == $numfpfe_day_cnt     && \ 
          $numfmom_calc     == $numflfe_mom_cnt     && $numfsave_mom == $numfpfe_mom_cnt     )) ;then

        nstep=$(( nstep + 1 ))
        wmessage "... step $strstep ... archive/remove output    ... pass"
        blstep2=false
    else
        wmessage "... step $strstep ... archive/remove output    ... fail"
        blstep2=true
    fi
   
    if [[ "$strexpid" == "fcst" ]]; then
        strstep=$(( $strstep + 1 ))

        #/u/knakada/GEOSS2S3/GEOS_fcst/nino3.4/sst/19840101/ens1/19840101.sst_tavg_1dy_glo_L720x361_slv.198403.nc4.tar
        local fsst=$dsst/$fcstdate/$ensm/$fcstdate.$collsst.${arrmfull_3mo[-1]}.nc4.tar
        if [[ -f $flstarc ]];then 
            local _fsst_size=$( grep $fsst $flstarc 2>/dev/null | cut -d' ' -f2 )
            if (( $_fsst_size > 0 ));then 
                local status_fexist=0
            else
                local status_fexist=999
            fi
        else
            ssh -q $hstarc test -f $fsst
            local status_fexist=$?
        fi

        if (( $status_fexist == 0 ));then
            nstep=$(( nstep + 1 ))
            wmessage "... step $strstep ... check $( basename $fsst )    ... pass"
        else
            wmessage "... step $strstep ... check $( basename $fsst )    ... fail"
            wmessage "file does not exist - $fsst"
        fi
    fi

    echo "$nstep $strstep"    
}

checklastrst() {
    #description:   check if the last restart exist on exp dir and/or archive
    #note:  _rststatus = 2 - last rst doesn't exist either in exp or arch dir
    #       _rststatus = 1 - last rst exist in arch but not in exp dir
    #       _rststatus = 0 - last rst exist in both exp and arch dir
        
    #todo:  check if last rst exists on archive
    if [[ $hstshort == "pfe" ]];then
        ssh -q $hstarc test -f $frstlastarch 2>/dev/null
        local _statusarch=$?
    elif [[ $hstshort == "dis" ]];then
        test -f $frstlastarch
        local _statusarch=$?
    fi

    #todo:  check if last rst exists in exp dir
    test -f $frstlast
    local _statusdisk=$?
    local _rststatus=$(( _statusdisk + _statusarch ))

    #todo:  make sure that last rst exist on archive but not in dexp when _rststatus=1
    #       When _rststatus =1, last rst should only exist in archive. 
    if (( $_rststatus == 1 )) && (( $_statusdisk == 0 )) && (( $_statusarch == 1 ));then
        die "last rst exists only in archive dir, which should not happen... exist"
    fi

    echo $_rststatus
}

compress_dir() {
    local strtar=$1

    if (( $writetofile == 1 ));then
        /usr/local/bin/shiftc -r -f --no-cron --create-tar --no-mail --wait $strtar $dfcstrmltr/$strtar.tar >> $fmessage
        local _status_shiftc=$?
    else
        /usr/local/bin/shiftc -r -f --no-cron --create-tar --no-mail --wait $strtar $dfcstrmltr/$strtar.tar
        local _status_shiftc=$?
    fi

    if (( $_status_shiftc == 0 )) ;then 
        rm -rf $strtar
        wmessage 
        wmessage "... $dfcstrmltr/$strtar.tar is created and $strtar dir is removed ..."
    fi
    return
}

compress_dir_rsync() {
    local strtar=$1

    if (( $writetofile == 1 ));then
        tar cf $dfcstrmltr/$strtar.tar $strtar >> $fmessage
        local _status_shiftc=$?
    else
        tar cf $dfcstrmltr/$strtar.tar $strtar
        local _status_shiftc=$?
    fi

    if (( $_status_shiftc == 0 )) ;then 
        rm -rf $strtar
        wmessage "... $dfcstrmltr/$strtar.tar is created and $strtar dir is removed ..."
    fi
    return
}


cleanexp() {
    local dir

    #+++++ cd to dexp (start) +++++
    cd $dexp

    #todo:  delete all empty dir
    find * -type d -empty -delete 2>/dev/null

    cnt_tar=0

    wmessage
    wmessage "... start to clean ..."

    rstat=0
    hstat=0
    
    if (( $hstat == 0 )) && (( $rstat == 0 ));then
        [[ ! -d $dfcstrmltr ]] && mkdir -p $dfcstrmltr

        wmessage "... various files and directories are compressed and moved to rmltr/$fcstdate/$ensm ..."

        #local _arrdir=( archive RESTART holding $strrst $collsst )
        local _arrdir=( archive post )
        for dir in ${_arrdir[@]};do
            if [[ -d $dir ]];then
                wmessage "... tar $dir ..."
                compress_dir_rsync $dir
                cnt_tar=$(( cnt_tar + 1 ))
            fi
        done

        wmessage "... move stdout files ..."
        #find * -maxdepth 0 -type f -name 'R'${fcstdate}${ensm}'*.o*' | tar -cf stdout.tar -T - --remove-file
        local arrfstdout=($( find * -maxdepth 0 -type f -name 'R'${fcstdate}${ensm}'*.o*' )) 

        #tar -cf stdout.tar *.out gcm_run.o* R$fcstdate$ensm*.out #R$fcstdate$ensm.o*
        if (( ${#arrfstdout[@]} > 0 )) ;then 
            mv ${arrfstdout[@]} $dfcstrmltr/
        fi

        wmessage "... delete config files ..." 
        rm -f ${arrfconfig[@]} 2>/dev/null 
        
        #status_shiftc_total=0
        #wmessage "... tar config files ..."
        #if (( $writetofile == 1 ));then
        #    #/usr/local/bin/shiftc -f --no-cron --create-tar --no-mail --wait ${arrfconfig[@]} $dfcstrmltr/config.tar >> $fmessage
        #    tar cf $dfcstrmltr/config.tar ${arrfconfig[@]} >> $fmessage
        #    status_shiftc=$?
        #else
        #    #/usr/local/bin/shiftc -f --no-cron --create-tar --no-mail --wait ${arrfconfig[@]} $dfcstrmltr/config.tar    
        #    tar cf $dfcstrmltr/config.tar ${arrfconfig[@]}
        #    status_shiftc=$?
        #fi

        #status_shiftc_total=$(( status_shiftc_total + status_shiftc ))

        #if (( $status_shiftc_total == 0 )) ;then 
        #    cnt_tar=$(( cnt_tar + 1 ))
        #    rm -f ${arrfconfig[@]} 
        #    strtar=config
        #    wmessage "... $dfcstrmltr/$strtar.tar is created and $strtar files are removed ..."
        #fi
        
        wmessage "... delete rst files ..."

        rm -f *_rst 2>/dev/null 
        rm -f note* 2>/dev/null

        #wmessage
        #wmessage "... tar rst files ..."
        #wmessage

        #shopt -s nullglob
        #local arrrst=( *_rst )
        #arrrst+=( note* )
        #shopt -u nullglob

        #if [[ -f note_perturbation ]];then arrrst+=( note_perturbation );fi

        #if (( $writetofile == 1 ));then
        #    #/usr/local/bin/shiftc -f --no-cron --create-tar --no-mail --wait ${arrrst[@]} $dfcstrmltr/rst.tar >> $fmessage
        #    tar cf $dfcstrmltr/rst.tar ${arrrst[@]} >> $fmessage
        #    status_shiftc=$?
        #else
        #    #/usr/local/bin/shiftc -f --no-cron --create-tar --no-mail --wait ${arrrst[@]} $dfcstrmltr/rst.tar
        #    tar cf $dfcstrmltr/rst.tar ${arrrst[@]}
        #    status_shiftc=$?
        #fi

        #if (( $status_shiftc == 0 )) ;then 
        #    let cnt_tar++
        #    rm -f *_rst
        #    rm -f note*
        #fi
        
        local _arrrmdir=( RC RC_s2sv3 scratch plot RESTART $collsst .rmtest holding )

        wmessage "... delete ${_arrrmdir[@]}... "
        for dir in ${_arrrmdir[@]};do
            [[ -d $dir ]] && rm -rf $dir 2>/dev/null
        done

        wmessage "... delete various markers (i.e. runpostedit) ..."
        
        find * -maxdepth 0 -type f -name "message_getlfefout*" -delete 2>/dev/null
        find * -maxdepth 0 -type f -name "$fmark_resubwinner*" -delete 2>/dev/null
        find * -maxdepth 0 -type f -name "$fmark_hold*" -delete 2>/dev/null
        find * -maxdepth 0 -type f -name "M${fcstdate}${ensm}*.o*" -delete 2>/dev/null
        [[ -f $fmark_setupready ]] && rm -f $fmark_setupready
        [[ -f $fmark_setupshiftin ]] &&  rm -f $fmark_setupshiftin
        [[ -f $fmark_sherlock ]] &&  rm -f $fmark_sherlock 2>/dev/null
        [[ -f mini_gcm_run.j ]] && rm -f mini_gcm_run.j 2>/dev/null
        [[ -f linkbcs ]] && rm -f linkbcs
        [[ -f model_tags ]] && rm -f model_tags

        #todo:  delete empty dir
        find * -type d -empty -delete 2>/dev/null
        
        wmessage 
        wmessage     "... $cnt_tar tar files should be created ..."
        wmessage     "... cleaning directory completed ..."
        touch $fcomp
        [[ -f $fmessage ]] && mv $fmessage $dstdout/ 
    fi
    
    #+++++ cd to dexp ( end ) +++++
    cd - >/dev/null
    
    return
}

cooking_configarch (){
    #description:   archive config files.

    local blrsync=true 

    cd $dexp
    shopt -s nullglob
    local _arrfconfig1=( AGCM.rc AGCM_*.rc CAP_*.rc HISTORY_*.rc input.nml gcm_run.j archive/run_gcm*.sh archive/stderr_gcmarch cap_* note*_cont \
note_ocnpert_* message_rstmkpert_* $fnote )
    shopt -u nullglob

    local _arrfconfig=($( printf "$dexp/%s\n" ${_arrfconfig1[@]} )) 

    wmessage "$(date +'%m/%d/%Y %H:%M' ) ... archiving config files"
    wmessage

    if $blrsync;then
        if [[ $hstshort == pfe ]];then
            #wmessage "$( /usr/bin/rsync -azPq ${_arrfconfig[@]} $hstarc:$darch/ )"
            ssh -q $hstarc /usr/bin/rsync -azPq ${_arrfconfig[@]} $darch/ 
        elif [[ $hstshort == dis ]];then 
            wmessage "$( rsync -azRPq ${_arrfconfig[@]} $darch )"
        fi
    else
        if [[ $hstshort == pfe ]];then
            wmessage "$( /usr/local/bin/shiftc --no-cron --no-mail --wait -L ${_arrfconfig[@]} $hstarc:$darch/ )"
        elif [[ $hstshort == dis ]];then
            die "(${FUNCNAME[0]})blrsync is $blrsync and it has to be true on $hstshort to run ${FUNCNAME[0]} function"
        fi
    fi

    cd - >/dev/null
    return
}


clean_dir () {
    ! $blchild && [[ -n $flock && -f $flock ]] && rm -f $flock
    [[ -n $fexistlfe && -f $fexistlfe ]] && rm -f $fexistlfe
    return
}

#================================================================================
#                                     Begin
#================================================================================
#starthere
hst=$( hostname )
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

if [[ ! $cdir/func_fcst.sh ]];then 
    echo "$cdir/func_fcst.sh does not exist";exit
else
    source $cdir/func_fcst.sh
fi

trap clean_dir EXIT

writetofile=0
runclean=0
optusesstlst=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename $0) -- this is a program to:
        clean forecast experiment *after* exp completes 10-month run.
        Select option c to clean exp dir. By default, this script check 
        current status of experiment dir.  

        Usage: ./$(basename "$0") [-chw] [-d] srcme_file 

        required:
            Source file, which set various vars for a set of runs (i.e. srcme_pfe_fcst)

        options:
            -d                          a full path to an experiment dir
                --use-sstlst=[BOOLEAN,  true to retrieve wftmsz_flist from \$DARC/\${reg}/sst or a 
                                 FILE]    retrieved wftmsz_flis file name. 
                                          *Use it to confirm sst file is archived
            -c                          clean an experiment dir
            -h                          show this help text
            -w                          write stdout/err in a file
"
file=
verbose=0
cnt=0
while :; do
    case $1 in
                   -d )  [[ "$2" ]] && dexp=$2 && shift;;
                   -c )  runclean=1;;
      --use-sstlst=?* )  optusesstlst=true && userinput_usesstlst=${1#*=};;
        --use-sstlst= )  die "--use-sstlst requires a non-empty option argument.";; 
                   -h )  echo "$usage"; exit 0;;
                   -w )  writetofile=1;; 
        -h|-\?|--help )  echo "$usage";exit;;           # Display a usage synopsis.
                   -- )  shift;break;;                  # End of all options.
                  -?* )  printf 'WARN: Unknown option (ignored): %s\n' "$1" >&2;;
                    * )  cnt=$(( cnt + 1 )); break ;;   # Default case: No more options, so break out of the loop.
    esac
    shift
    cnt=$(( cnt + 1 ))
done
#wmessage \@$LINENO $optusesstlst $userinput
#exit

#OPTIND=1
#while getopts 'hwcd:' option; do
#    case "$option" in
#        h)  echo "$usage"; echo "$note"; exit 0;;
#        d)  dexp=$OPTARG;;
#        c)  runclean=1;;
#        w)  writetofile=1;;
#        \?) die "Invalid option: -$OPTARG" >&2
#            exit 1;;
#        :)  die "Option -$OPTARG requires an argument." >&2
#            exit 1;;
#    esac
#done
#
##todo:  get positional inputs. 
#shift $((OPTIND-1))
#[[ "${1:-}" = "--" ]] && shift
arrposarg=( "$@" )

[[ -z ${arrposarg[@]} ]] && die "an input is missing"
(( ${#arrposarg[@]} != 1 )) && die "one input are required"
#================================================================================
#                               Check User Inputs
#================================================================================
srcf=${arrposarg[0]}
source $srcf
[[ -z $DFCST ]] && die "DFCST is undefined"
[[ -z $DRST  ]] && die "DRST  is undefined"
[[ -z $DARCH ]] && die "DARCH is undefined"
#================================================================================
#                             Set Host Specific Vars
#================================================================================
hstshort=$( get_host)
if [[ "$hstshort" == "pfe" ]] ;then 
    [[ -z $hstarc ]] && hstarc=lfe
elif [[ "$hstshort" == "dis" ]] ;then 
    [[ -z $hstarc ]] && hstarc=dirac
else
    exit
fi
#================================================================================
#                                 Set Variables
#================================================================================
#mid
cdate=$( date +"%m/%d/%Y %H:%M")
[[ -n $rstexpid ]] && strexpid=$strexpid$rstexpid

set_rstfcstdate $dexp


dmess=$cdir/message
dfcst=$( echo $dexp | rev | cut -d'/' -f3- | rev ) 

ddata=$cdir/output/pckwinners/$strdout
dstdout=$cdir/stdout/$strscr/$strdout
dsst=$DARCH/nino3.4/sst

dhold=$dexp/holding
strrst=restarts
strmom=MOM_Output
strmom_search=ocean_
strscrach=scratch

fhis2=HISTORY_2.rc
fexistlfe=$cdir/${strscr}_${strexpid}_$fcstdate${ensm}_lfefiles
fcomp=$dexp/${strscr}_completed
farch=archive/run_gcmarch.sh

#darch1=$( grep "dfout_lfe=" $dexp/$farch | cut -d'=' -f2 | rev | cut -d'/' -f3- | rev )
#darch=$darch1/$fcstdate/$ensm
darch=$DARCH/$fcstdate/$ensm

farchcomp=gcmarch_archcompleted
 farchdel=gcmarch_deloutcompleted

fmark_setupready=gcmsetup_setupready
fmark_setupshiftin=gcmsetup_shiftin
fmark_sherlock=sherlock_caseclosed
fmark_hold=hold_
fmark_resubwinner=resubwinner_*
fdexcept=$cdir/data/$strscr/$strdout/data_${strscr}_${strexpid}_exceptions

blexception=false
bldocont=false
blstep2=false

status_archrst=100

dfcstrmltr=$dfcst/rmltr/$fcstdate/$ensm
dfcstcont=$dfcst/cont/$fcstdate/$ensm

#todo:  get yyyymm which has winner selected
#note: As of 11/30/2022, winner_avail is not in use. 
#      this function will be removed from bf*.sh library 
#arrwinneravail_yyyymm=($( winner_avail $ddata ))

#todo:  set various variables
fcstdate_yyyymm=$( echo $fcstdate | cut -c1-6 ) 
if (( $fcstdate_yyyymm < 199812 ));then 
    _setvars $dexp 
else
    setvars $dexp 
fi
#----------------------------------------
#             Debug (start)
#----------------------------------------
#exp_getfname_lfe $dexp $darch $fexistlfe 
#count_files $dexp $fexistlfe 
#
#wmessage lineno = $LINENO 
#wmessage $numfmonmapl_calc $numflfe_monmapl_cnt $numfsave_mpl $numfpfe_monmapl_cnt
#wmessage $numfmonpost_calc $numflfe_monpost_cnt $numfsave_mon $numfpfe_monpost_cnt 
#wmessage $numfdiu_calc     $numflfe_diu_cnt     $numfsave_diu $numfpfe_diu_cnt     
#wmessage $numfday_calc     $numflfe_day_cnt     $numfsave_day $numfpfe_day_cnt     
#wmessage $numfmom_calc     $numflfe_mom_cnt     $numfsave_mom $numfpfe_mom_cnt   
#wmessage 
#exit
#+++++ Debug ( end ) +++++

fcst_yyyymm=$( echo $fcstdate | cut -c1-6 )
fmessage=$dmess/message_${strscr}_${strexpid}_$fcstdate${ensm}_$fcst_yyyymm

#todo:  get newensemble member if exp is winner. 
#declare -A assoc_ensmwin
declare -A assoc_winensm
convert2winnerensm_all $ddata

#todo:  set exp enddate
if [[ -f $dexp/gcm_run.j ]];then 
    blrerun=$( grep -i "RERUN" $dexp/gcm_run.j | head -1 | rev | cut -d' ' -f1 | rev | tr '[:upper:]' '[:lower:]'  )
else
    blrerun=false
fi

blwinner=$( exp_checkwinner $dexp $ddata )
if $blwinner || $blrerun;then
    expenddate=$realend_yyyymmdd
else
    expenddate=$end3_yyyymmdd
fi

#todo:  get exception exps.
#note:  if clean option (opt c) is on, experiments in fdexcept file is going to be 
#       cleaned even though dexp doens't pass all filter.
if [[ -f $fdexcept ]];then 
    arrexception=($( misc_readfbyline $fdexcept))
    [[ "${arrexception[@]}" =~ "$dexp" ]] && blexception=true || blexception=false
fi

#todo:  exist if ant of these conditions are true:
#       - grm_run.j are running
#       - exp has completed 10-month run 
#       - if exp is a winner and has not been completed 10-month run
#       - if exp is NOT a winner and has not been completed 3-month run
if ! $blexception;then
    if [[ -f $dexp/gcm_run.j ]];then 
        if [[ -f $dexp/archive/run_gcmfcstarchive.sh || -f $fexp/$farch ]];then 
            cnt_jobs $dexp 
            (( $num_rgrn > 0 )) && exit
        fi
    fi

    if $blrerun; then 
        [[ "$realend_yyyymmdd" != "$capr_yyyymmdd" ]] && exit
    else
          $blwinner && (( $expenddate != $capr_yyyymmdd )) && exit
        ! $blwinner && (( $expenddate  > $capr_yyyymmdd )) && exit
    fi
fi

#todo:  get config file names
#+++++ cd dexp (start) +++++
cd $dexp
shopt -s nullglob
arrfconfig=( HISTORY.rc cap_end diag_table field_table gcm_run.j \
             cap_restart cap_restartIC CAP.rc input.nml AGCM.rc  \
             GEOSgcm.x fvcore_layout.rc .HOMDIR )
arrfconfig+=( gcm_run.j.* input_*.nml AGCM_*.rc CAP_*.rc HISTORY_*.rc \
              *diag_table *.bak cap_* message_rst*_* )
shopt -u nullglob
cd - >/dev/null
#+++++ cd dexp ( end ) +++++


#todo:  remove flfefile.
[[ -f $fexistlfe ]] && rm -f $fexistlfe
msg_newfile $fmessage

ssh -q $hstarc test -d $darch
(( $? > 0 )) && die "darch, $darch, does not exists"
#================================================================================
#                                  Main Process
#================================================================================
#letsbegin
if $optusesstlst;then 
    if [[ $userinput_usesstlst == "true" ]] ;then 
        [[ -f $flstarc ]] && rm -f $flstarc

        get_fwftmsz_nosup $DARCH/$reg/sst $flstarc
        [[ ! -f $flstarc ]] && optusesstlst=false
    elif [[ -f $userinput_usesstlst ]];then 
        flstarc=$userinput_usesstlst
    else
        optusesstlst=false
    fi
fi


#tood:  get a file with all output file name in archive dir on $hstarc
exp_getfname_lfe $dexp $darch $fexistlfe 
[[ ! -f $fexistlfe ]] && die "failed to create $fexistlfe"

numfarch_all=$( cat $fexistlfe | grep -vE "$strrst|$strmom_search" |  wc -l )
numfarch_rst=$( cat $fexistlfe | grep $strrst | wc -l )    
numfarch_mom=$( cat $fexistlfe | grep $strmom_search | wc -l )    

#note:  this array is created in setvars
#arrfmiss=($( exp_getfmiss_fexist $dexp ))

#todo:  delete empty dir
find $dexp/* -maxdepth 0 -type d -empty -delete 2>/dev/null

if $blwinner;then
    frstlast=$dexp/$strrst/$strrst.e${realend_yyyymmdd}_21z.tar
else
    frstlast=$dexp/$strrst/$strrst.e${end3_yyyymmdd}_21z.tar
fi
msg_wheader_userdefined 80 = "$cdate $fcstdate/$ensm"

wmessage "    exp location : $dexp"
wmessage "archive location : $darch"
wmessage "   trash can dir : $dfcstrmltr"
wmessage
wmessage "winner ensemble?          : ${blwinner^}"
wmessage "rerun                     : ${blrerun^}"
#if (( $runclean == 1 ));then wmessage "exception exp?                : ${blexception^}" ;fi
wmessage "exception exp?            : ${blexception^}"
wmessage
wmessage "cap_restartic             : $capric_yyyymmdd"
wmessage "3-month run end           : $end3_yyyymmdd"
wmessage "cap_restart               : $capr_yyyymmdd"
wmessage "cap_end                   : $realend_yyyymmdd"
wmessage "Total # of months         : $nummon_run"
wmessage "Last rst file name        : $( basename $frstlast )" 
wmessage
wmessage "Collections               : $( printf '%s\n' $( exp_getcollections $dexp/$fhis2 ) | wc -l )   *exclude $collsst"
wmessage
#+++++ cd dexp/archive (start) +++++
cd $dexp/archive
if [[ -f $farchcomp || -f $fdel ]];then
    wmessage "Archive Status            :"
    [[ -f $farchcomp ]] && wmessage $( stat --print="%.19y %n" $farchcomp )
    [[ -f $farchdel  ]] && wmessage $( stat --print="%.19y %n" $farchdel ) 
else
    wmessage "Archive/Delout Completed? : No"
fi
cd - >/dev/null
#+++++ cd dexp/archive ( end ) +++++
wmessage
count_files $dexp $fexistlfe 
write_table $dexp ${#arrfmiss[@]} 
write_table_collection $dexp ${arrfsave[@]} 


#todo:  run filter
filterout=$( filter $dexp )
numpass=$( echo $filterout | cut -d' ' -f1 )
numfilter=$( echo $filterout | cut -d' ' -f2 )


#todo:  start cleaning.
if (( $runclean == 0 ));then
    wmessage "... $numpass steps out of $numfilter ... this run is monitor only ... "

elif (( $runclean == 1 ));then 
    if (( $numpass != $numfilter ));then
        if $blexception;then
            wmessage "... $numpass steps out of $numfilter ... start force cleaning (exp is an exception)  "
            cooking_configarch
            cleanexp
        else
            wmessage "... $numpass steps out of $numfilter ... unable to clean ... "
        fi
    elif (( $numpass == $numfilter ));then
        wmessage "... $numpass steps out of $numfilter ... start to clean ... "
        cooking_configarch
        cleanexp
    fi
fi

exit

