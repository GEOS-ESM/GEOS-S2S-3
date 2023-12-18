#!/usr/bin/env bash

#note: for debugging
#ref: https://is.gd/kxHve1
#exec 5> debug_$(basename "$0")
#BASH_XTRACEFD="5"
#PS4='$LINENO: '
#set -x

inputcheck(){
    :
}

prepare_archiving() {

    ##todo:  get output file names & its size on lou
    #exp_getfname_lfe $dexp $darc $fexistlfe

    ##todo:  get output file names & its size on pfe
    #exp_getfname_pfe $dexp $fexistpfe
    #
    #[[ ! -f $fexistlfe ]] && die "$fexistlfe does not exists"
    #[[ ! -f $fexistpfe ]] && die "$fexistpfe does not exists"
    

    if [[ -f $dexp/archive/gcmarch_lfe ]];then 
        [[ -f $fexistlfe ]] && rm -f $fexistlfe
        cp -p $dexp/archive/gcmarch_lfe $fexistlfe
    fi

    if [[ -f $dexp/archive/gcmarch_pfe ]];then
        [[ -f $fexistpfe ]] && rm -f $fexistpfe
        cp -p $dexp/archive/gcmarch_pfe $fexistpfe
    fi

    #todo:  find missing and existing output files on lfe. arrfmiss 
    #       and arrfexist_rm, global arrays, are returned
    #get_fmiss_fexist
    #note:  this is done in setvars
    #arrfmiss=($( exp_getfmiss_fexist $dexp ))

    #todo:  create file with existing geosgcm_sst output file name 
    #[[ -f $fexistpfe_sst ]] && rm -f $fexistpfe_sst
    #find $dexp/* -maxdepth 1 -mindepth 1 -type f -name "$fcstdate*.nc*" 2>/dev/null | grep $collsst | xargs -i stat --printf="%s %Y %n\n" {} > $fexistpfe_sst

    #todo:  find if nonmonthly are available to create tar. 
    #get_tarable
    
    #todo:  find corrupted tar (daily) files
    sherlock_findcorruptedtar
    (( ${#arrftar_crpt[@]} > 0 )) && blftarcrpt=true || blftarcrpt=false

    return
}

sherlock_findcorruptedtar(){
    #description:   find corrupted tar files ("daily" files) 
    local arrdexpcoll=($( printf "$dexp/%s\n" ${arrcollmonpost[@]} ))
    local dexpcoll ftar
    #global variable
    arrftar=()
    arrftar_crpt=()

    for dexpcoll in ${arrdexpcoll[@]};do
        [[ -d $dexpcoll ]] && arrftar+=($( find $dexpcoll/* -maxdepth 0 -type f -name '*.nc4.tar' 2>/dev/null ))
    done 

    #todo:  check if tar is corrupted
    local cnt=0
    for ftar in ${arrftar[@]};do 
        tar -tf $ftar >> /dev/null 2>&1
        local status_tar=$?
        (( $status_tar > 0 )) && arrftar_crpt+=( $ftar ) 
    done 
            
    #echo "${arrftar_crpt[@]}"
    return
}

remove(){
    #description:   re-do compressing to make tar files
    local _arrftar=( "$@" )

    for ftar in ${_arrftar[@]};do 
   
        local _dhold=$dexp/holding
        local _fhis1=$dexp/HISTORY_1.rc
        local ftar_base=$( basename $ftar ) 
        local fcstdate=$( echo $ftar_base | cut -d'.' -f1 )
        local     coll=$( echo $ftar_base | cut -d'.' -f2 )
        local  yyyy_mm=$( echo $ftar_base | cut -d'.' -f3 )
        local ldd=$( numdaysinmonth $( echo "${yyyy_mm:4:2}*1" | bc ) ${yyyy_mm:0:4} )

        #todo:  get a full path of individual output files
        local numflastday=$( find $_dhold/$coll/$yyyy_mm/* -type f -name "$fcstdate.$coll.*$yyyy_mm$ldd*.nc?" 2>/dev/null | wc -l )
        local collfreq=$( grep $coll.frequency $_fhis1 |  tr -s '[:space:]' |rev |  cut -d' ' -f1 | tr -d ',' | rev | cut -c1-2 | xargs -i bash -c "echo "'"'"{}*1"'"'" | bc" )
        local calcnumflastday=$( echo "24/$collfreq" | bc ) 

        (( $calcnumflastday != $numflastday )) && continue
        
        #local arrfyyyy_mmpath=($( find $_dhold/$coll/$yyyy_mm/* -type f -name "$fcstdate.$coll.*$yyyy_mm*.nc?" 2>/dev/null ))
        #local arrfyyyy_mm=($( printf '%s\n' ${arrfyyyy_mmpath[@]} | xargs -i basename {} ))
        local arrfyyyy_mm=($( find $_dhold/$coll/$yyyy_mm/* -type f -name "$fcstdate.$coll.*$yyyy_mm*.nc?" -exec basename {} 2>/dev/null ))
        local dyyyy_mm=$_dhold/$yyyy_mm
        local drmltr=$dexp/rmltr

[[ ! -d $dexp/.rmltr ]] && mkdir -p $dexp/.rmltr
mv $ftar $dexp/.rmltr/

        ##+++++ cd to holding/coll/somewhere (start) +++++
        ##cd $_dhold/$coll/$yyyy_mm
        #cd $dyyyy_mm
        #
        #wmessage " $( date +'%D %T' ) ... create tar - $( basename $ftar ) ..."
        #wmessage
        #
        #if $bltar ;then
        #    mtar -cf $ftar ${arrfyyyy_mm[@]}
        #    local status_tar=$?
        #else
        #    /usr/local/bin/shiftc --no-cron --no-mail --wait --create-tar -f ${arrfyyyy_mm[@]} $ftar 
        #    local status_tar=$?
        #fi

        #tar -tf $ftar >> /dev/null 2>&1
        #local status_tar_corrupted=$?
        #
        #if (( $status_tar != 0 || $status_tar_corrupted > 0 ));then
        #    [[ -f $ftar ]] && rm -f $ftar 
        #fi 
        #    
        #cd - >/dev/null
        ##+++++ cd to holding/coll/somewhere ( end ) +++++
                

    done 
    return
}

resubmit_post(){
    #description:   resubmit gcm_post
    local _arrfpst=( "$@" )
    local fpst_org=$dexp/post/gcm_post.j.org
    local fgcmpst_new=$dexp/post/tmp_${strscr}_new_gcm_post.j
    local fgcmpst_tmp=$dexp/post/tmp_thisgcm_post.j
    local _fsed=$dexp/post/${strscr}_sedfile
    local post_t="05:00:00" 
    local batch_group="PBS -W group_list=g0609"
    local batch_outputname="PBS -o "
    local batch_joinouterr="PBS -j oe -k oed"
    local arrqsubme=() 
    local fpst 
    
    #local pbsselect_res_org="PBS -l select=1:ncpus=20:mpiprocs=20:model=ivy:aoe=toss4"
    local pbsselect_res_org="PBS -l select=1:ncpus=20:mpiprocs=20:model=ivy"
    local pbsqid_res_org="PBS -q $pstqid"
    local pbsselect_nrm_org="PBS -l select=1:ncpus=20:mpiprocs=20:model=ivy"
    local pbsqid_nrm_org="PBS -q normal"

    [[ -f $fgcmpst_tmp ]] && rm -f $fgcmpst_tmp
    [[ -f $fgcmpst_new ]] && rm -f $fgcmpst_new
    [[ ! -f $fpst_org ]] && mv $dexp/post/gcm_post.j $fpst_org

    #todo:  recreate gcm_post.j 
    local lnnum=$( grep -n "System Environment Variables" $fpst_default 2>/dev/null | cut -d':' -f1 | xargs -i bash -c 'echo "{} - 2 " | bc' )  
    cat $fpst_default | head -n $lnnum >> $fgcmpst_new
    
    local lnnum1=$( grep -n "System Environment Variables" $fpst_org 2>/dev/null | cut -d':' -f1 | xargs -i bash -c 'echo "{} - 2 " | bc' )  
    local lnnum2=$( grep -n "Perform Post Processing" $fpst_org 2>/dev/null | cut -d':' -f1 | xargs -i bash -c 'echo "{} - 2 " | bc' )  
    cat $fpst_org | head -$lnnum2 | tail -n +$lnnum1 >> $fgcmpst_new
    
    local lnnum=$( grep -n "Perform Post Processing" $fpst_default 2>/dev/null | cut -d':' -f1 | xargs -i bash -c 'echo "{} - 2 " | bc' )  
    cat $fpst_default | tail -n +$lnnum >> $fgcmpst_new
    
    sed -e "s#@\<EXPDIR\>#$dexp#g" $fgcmpst_new >> $fgcmpst_tmp
    [[ -f $fgcmpst_tmp ]] && mv $fgcmpst_tmp $dexp/post/gcm_post.j

    #todo: recreate gcm_post.coll.jyyyymm
    for fpstname in ${arrfpst[@]} ;do 
        #gcm_post.aer_inst_3hr_glo_L720x361_slv.j198401

        local fpst=$dexp/post/$fpstname
        local   coll=$( echo $fpstname | cut -d'.' -f2 )
        local yyyymm=$( echo $fpstname | rev | cut -d'.' -f1 | rev | cut -c2- )
        #gcm_post.atm_inst_6hr_glo_L720x361_p49.o198401
        local pstout=gcm_post.$coll.o$yyyymm

        [[ -f $fpst ]]  && rm -f $fpst
        [[ -f $_fsed ]] && rm -f $_fsed
   
        #todo:  select pbs statements 
        if $blrunres ;then
            pbsselect_res=$pbsselect_res_org
            pbsqid_res=$pbsqid_res_org
            pbsselect_nrm="#$pbsselect_nrm_org"
            pbsqid_nrm="#$pbsqid_nrm_org"
#s?@\<POSTPRC_P\>?${pbsselect_nrm}?g
#s?@\<POST_Q\>?${pbsqid_nrm}?g

            cat > $_fsed << EOF
s?@POST_T?${post_t}?g
s?@\<DFCST\>?${DFCST}?g
s?@\<FCSTDATE\>?${fcstdate}?g
s?@\<ENSEMBLE_MEMBER\>?${ensm}?g
s?@\<COLLECTION\>?${coll}?g
s?@\<YYYYMM\>?${yyyymm}?g
s?@\<POSTPRC_PRES\>?${pbsselect_res}?g
s?@\<POSTPRC_QRES\>?${pbsqid_res}?g
s?@\<BATCH_GROUP\>?${batch_group}?g
s?@\<BATCH_JOINOUTERR\>?${batch_joinouterr}?g
s?@BATCH_OUTPUTNAME?${batch_outputname}?g
s?@POST_O?${pstout}?g
EOF

        elif ! $blrunres; then
            pbsselect_res="#$pbsselect_res_org"
            pbsqid_res="#$pbsqid_res_org"
            pbsselect_nrm="$pbsselect_nrm_org"
            pbsqid_nrm="$pbsqid_nrm_org"
        
            cat > $_fsed << EOF
s?@POST_T?${post_t}?g
s?@\<DFCST\>?${DFCST}?g
s?@\<FCSTDATE\>?${fcstdate}?g
s?@\<ENSEMBLE_MEMBER\>?${ensm}?g
s?@\<COLLECTION\>?${coll}?g
s?@\<YYYYMM\>?${yyyymm}?g
s?@\<POSTPRC_P\>?${pbsselect_nrm}?g
s?@\<POST_Q\>?${pbsqid_nrm}?g
s?@\<BATCH_GROUP\>?${batch_group}?g
s?@\<BATCH_JOINOUTERR\>?${batch_joinouterr}?g
s?@BATCH_OUTPUTNAME?${batch_outputname}?g
s?@POST_O?${pstout}?g
EOF

        fi

        [[ -f $fpst ]] && rm -f $fpst
        sed -f $_fsed $dexp/post/gcm_post.j >> $fpst

        if $blrunres; then
            local fthistmp=${fpst}_tmp
            mv $fpst $fthistmp

            pbsselect_res1=$( grep -w "$pbsselect_res" $fthistmp )
            char2_select=$( echo $pbsselect_res1 | cut -c1-2 )
            
            pbsqid_res1=$( grep "$pbsqid_res" $fthistmp )
            char2_qid=$( echo $pbsqid_res1 | cut -c1-2 )
            
            pbsselect_nrm1=$( grep -w "$pbsselect_nrm" $fthistmp | grep -v "aoe=" )
            char2_select_nrm=$( echo $pbsselect_nrm1 | cut -c1-2 )
            
            pbsqid_nrm1=$( grep "$pbsqid_nrm" $fthistmp )
            char2_qid_nrm=$( echo $pbsqid_nrm1 | cut -c1-2 )


            [[ "$char2_select" == "##" ]] && pbsselect_res=$( echo $pbsselect_res1 | cut -c2- )
            [[ "$char2_qid" == "##" ]] && pbsqid_res=$( echo $pbsqid_res1 | cut -c2- )

            [[ "$char2_select_nrm" != "##" ]] && pbsselect_nrm=$( echo \#$pbsselect_nrm1 )
            [[ "$char2_qid_nrm" != "##" ]] && pbsqid_nrm=$( echo \#$pbsqid_nrm1 )

#wmessage lineno = $LINENO 
##wmessage $pbsselect_res1
#wmessage $pbsselect_res
#wmessage
#wmessage $pbsselect_nrm
#wmessage
#wmessage $pbsqid_res1
#wmessage $pbsqid_res
#wmessage
#wmessage $pbsqid_nrm1
#wmessage $pbsqid_nrm

            if [[ "$pbsselect_res" == "$pbsselect_nrm" ]];then
                sed -e "s/${pbsqid_res1}/${pbsqid_res}/g"       \
                    -e "s/${pbsqid_nrm1}/${pbsqid_nrm}/g"       \
                    -e "s/${pbsselect_res1}/${pbsselect_res}/g" \
                    -e "s/${pbsselect_nrm1}/${pbsselect_nrm}/g" \
                    $fthistmp >> $fpst

                local status_sed=$?

            else
                #NOTE: 02/23/2022 this does not work at this time
                local status_sed=100
            fi

            [[ -f $fthistmp ]] && rm -f $fthistmp
        fi

        chmod 755 $fpst
        arrqsubme+=( $fpstname )
        [[ -f $_fsed ]] && rm -f $_fsed

    done 

    if (( ${#arrqsubme[@]} > 0 ));then
        wmessage
        wmessage "Submit gcm_post:"
        ahand_warr ${arrqsubme[@]}
        wmessage 

        #+++++ cd to dexp/post (start) +++++
        cd $dexp/post
        if (( $writetofile == 1 ));then
            printf '%s\n' ${arrqsubme[@]} | xargs -i /PBS/bin/qsub {} >> $fmessage
        else
            printf '%s\n' ${arrqsubme[@]} | xargs -i /PBS/bin/qsub {}
        fi
        cd - >/dev/null
        #+++++ cd to dexp/post ( end ) +++++
    fi
    
    [[ -f $fgcmpst_new ]] && rm -f $fgcmpst_new

    return
}

sherlock_fnumcheck(){
    #description:   compare file # for calculated and counted on both pfe and lfe
    #               **this does NOT check size of files.
    local _blfnumdiff=false

    if [[ -f $fexistlfe ]];then
            (( $numftotal_calc != $numflfe_total_cnt )) && ! $_blfnumdiff && _blfnumdiff=true
        (( $numfmonmapl_calc != $numflfe_monmapl_cnt )) && ! $_blfnumdiff && _blfnumdiff=true 
                (( $numfday_calc != $numflfe_day_cnt )) && ! $_blfnumdiff && _blfnumdiff=true 
                (( $numfdiu_calc != $numflfe_diu_cnt )) && ! $_blfnumdiff && _blfnumdiff=true 
        (( $numfmonpost_calc != $numflfe_monpost_cnt )) && ! $_blfnumdiff && _blfnumdiff=true  
                (( $numfmom_calc != $numflfe_mom_cnt )) && ! $_blfnumdiff && _blfnumdiff=true 

    else
            (( $numftotal_calc != $numfpfe_total_cnt )) && ! $_blfnumdiff && _blfnumdiff=true
        (( $numfmonmapl_calc != $numfpfe_monmapl_cnt )) && ! $_blfnumdiff && _blfnumdiff=true 
                (( $numfday_calc != $numfpfe_day_cnt )) && ! $_blfnumdiff && _blfnumdiff=true 
                (( $numfdiu_calc != $numfpfe_diu_cnt )) && ! $_blfnumdiff && _blfnumdiff=true 
        (( $numfmonpost_calc != $numfpfe_monpost_cnt )) && ! $_blfnumdiff && _blfnumdiff=true  
                (( $numfmom_calc != $numfpfe_mom_cnt )) && ! $_blfnumdiff && _blfnumdiff=true 
    fi

    echo $_blfnumdiff
}

execute_arch(){
    cd $dexp/archive

    [[ -f $fcomp ]] && rm -f $fcomp
    [[ -f $fdel ]] && rm -f $fdel

    ./run_gcmarch.sh >> stderr_gcmarch 2>&1
    cd - >/dev/null
    return
}

clean_dir() {
    [[ -n $flock && -f $flock ]] && rm -f $flock
    [[ -n $fexistlfe && -f $fexistlfe ]] && rm -f $fexistlfe
    [[ -n $fexistpfe && -f $fexistpfe ]] && rm -f $fexistpfe
    return
}

#================================================================================
#                                     Begin
#================================================================================
#beg
hst=$( hostname )
blnode=false
if [[ "$hst" =~ "pfe"* ]];then
    cdir=/nobackupp11/gmaofcst/GEOSS2S3/GEOS_util
elif [[ "$hst" =~ "r"* ]];then
    #note:  this is for host other than pfe @ NAS which has a name starting with 'r'
    blnode=true
elif [[ "$hst" =~ "discover"* ]];then
    #cdir=/discover/nobackup/projects/gmao/t2ssp/GEOSS2S3/GEOS_util
    echo "this script works only on pfe";exit
fi

strscr=$(basename "$0" | cut -d'.' -f1 )
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
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
    
    
    if [[ -f $flock ]];then
        echo "$(TZ=America/New_York date +'%m/%d/%Y %H:%M' ) ... $flock exists..." >> $ftmp
    else
        echo $(TZ=America/New_York date +'%m/%d/%Y %H:%M' ) >> $ftmp
    fi
    
    #todo:  lock this script
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
optf=false
optq=false
optr=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename $0) -- this is a program to:
        investigate each experiment and find corrupted tar outputs, missing 
        outputs from gcm_post.

        Usage: ./$(basename "$0") [-d exp dir] srcme_file

        input:        
            A source file, which set various vars for a set of runs (i.e. srcme_pfe_*)

        options:
            -d  a full path to an experiment dir
            -q  reserved queue id for gcm_post.j and run_gcmarch.sh (use the same reservation)
            -h  show this help text
"

file=
verbose=0
cnt=0
while :; do
    case $1 in
                       -d )  [[ "$2" ]] && dexp="$2" && optd=true && shift \
                             || die "opt d requires an argument";;
                       -h )  echo "$usage"; echo "$note"; exit 0;;
                       -w )  writetofile=1;;
                       -q )  [[ "$2" ]] && in_qid=$2 && optq=true && shift \
                             || die "opt q requires an argument";;
             -v|--verbose )  verbose=$((verbose + 1));;     # Each -v adds 1 to verbosity.
            -h|-\?|--help )  echo "$usage";exit;;           # Display a usage synopsis.
                       -- )  shift;break;;                  # End of all options.
                      -?* )  printf 'WARN: Unknown option (ignored): %s\n' "$1" >&2;;
                        * )  cnt=$(( cnt + 1 )); break ;;   # Default case: No more options, so break out of the loop.

    esac
    shift
    cnt=$(( cnt + 1 ))
done

#todo:  get positional inputs.
arrposarg=( "$@" )
#================================================================================
#                               Check User Inputs
#================================================================================
i=0
strf=${arrposarg[i]}; i=$(( i + 1 ))
inputcheck
[[ -n $strf ]] && source $strf
#================================================================================
#                             Set Host Specific Vars
#================================================================================
thishst=$( get_host )

[[ -z $DFCST || ! -d $DFCST  ]] && die "DFCST is undefined or does not exist"
[[ -z $DBUILD || ! -d $DBUILD ]] && die "DBUILD is undefined or does not exist"
#================================================================================
#                                 Set Variables
#================================================================================
#mid
[[ -n $rstexpid ]] && strexpid=$strexpid$rstexpid
setvars $dexp
dwin=$cdir/output/pckwinners/$strdout
dhld=$dexp/holding
darc=$DARCH/$fcstdate/$ensm
dmess=$cdir/message
dstdout=$cdir/stdout/$strscr/$strdout

fexistlfe=$dexp/archive/${strscr}_arc
fexistpfe=$dexp/archive/${strscr}_exp
farcruntime=$dexp/archive/tmp_gcmarch
fmessage=$dmess/message_${strscr}_${strexpid}_${fcstdate}${ensm}
fpst_default=$DBUILD/Applications/GEOSgcm_App/gcm_post_GEOSS2S3.j
farc_default=$DBUILD/Applications/GEOSgcm_App/run_gcmarch.sh

fmark=$dexp/${strscr}_caseclosed
fcomp=$dexp/archive/gcmarch_archcompleted
 fdel=$dexp/archive/gcmarch_deloutcompleted
fsftin=$dexp/${strscr}_shiftin

#note:
strnote="
 E - all output exists in holding/coll/yyyymm
LT - legacy tar file exist
EO - all outputs exists outside of holding/coll/yyyymm
MA - all outputs are missing
MS - some outputs are missing
UN - output status is unknown
"
arrstatus_out=( E LT EO MA MS UN )

strdiu=diurnal
strmon=monthly

blheader=false
blredoall=false
blrunres=false
blwin=$( exp_checkwinner $dexp $dwin )

strhead_yyyymm="YYYYMM"
strhead_status="Status"
strhead_missoutyyyymm="OutMissInYYYYMM"
strhead_numout="OutMissTimeAvg"
strhead_lfemiscnt="OutMissTimeAvg_LFE"

numstr_coll=$( printf '%s\n' ${arrcoll[@]} | wc -L )
numstr_yyyymm=${#strhead_yyyymm}
numstr_status=${#strhead_status}
numstr_missoutyyyymm=${#strhead_missoutyyyymm}
numstr_numout=${#strhead_numout}
numstr_lfemiscnt=${#strhead_lfemiscnt}
numspace=3
numstr_sep=$( echo "$numstr_coll + $numstr_yyyymm + $numstr_status + $numstr_missoutyyyymm + $numstr_numout + $numstr_lfemiscnt + $numspace" | bc ) 
str_sep=$( printf -- "-%.0s" $( seq 1 $numstr_sep ) ) 
    
 
#note:  submit reserved nodes or normal queue

if [[ -z $qid && -n $in_qid ]] ;then
    /PBS/bin/pbs_rstat | grep $in_qid >>/dev/null 2>&1
    status_grep=$?

    if (( $status_grep == 0 ));then
        pstqid=$in_qid
        arcqid=$in_qid

        blrunres=true
        
    else
        optq=false
        die "reserved queue, $in_qid, does not exist"
    fi
fi

#pstqid=R12753106
#arcqid=R12753106
arcqid=none


[[ -f $fsftin ]] && rm -f $fsftin
[[ ! -d $dstdout ]] && mkdir -p $dstdout
#================================================================================
#                                  Main Process
#================================================================================
#main
! $blheader && writeheader $dexp $darc && blheader=true
$blwin && wmessage "              Winner? : Yes"
if [[ -f $farcruntime ]];then 
    numarcexec=$( cat $farcruntime | wc -l ) 
    wmessage "       run_gcmarch.sh : executed $numarcexec times "
fi

prepare_archiving

#todo:  counts number of files
count_files $dexp $fexistlfe

write_table $dexp
write_table_collection $dexp ${arrfsave[@]}


#todo:  check file numbers 
blfnumdifferent=$( sherlock_fnumcheck )
#$blfnumdifferent && wmessage "File counts and calculated file numbers are different... " && wmessage

#todo:  check which gcm_post.* need to be re-submitted
arrpst_rerun=($( exp_sherlockpst ))

#todo:  check if all outputs exist in holding/collection/YYYYMM directories
arrpst_missyyyymm=($( exp_sherlockout $fsftin ${arrpst_rerun[@]}  ))

#wmessage "@$LINENO" ${#arrpst_missyyyymm[@]} 
#ahand_warr ${arrpst_missyyyymm[@]} 
#exit

       
num_miss=$( printf '%s\n' ${arrpst_missyyyymm[@]} | rev | cut -d'.' -f1 | rev | grep -E 'MA|MS' | wc -l ) 
if (( $num_miss > 0  ));then 
    msg_wheader_userdefined 60 "-" "holdling/Collection/YYYYMM" 
    wmessage 
    wmessage "# of Missing Collection/YYYYMM Directories : $num_miss"
    wmessage 
    wmessage "# of Outputs Missing Due to Missing Collection/YYYYMM Dir :"
    wmessage 
    wmessage "$( printf "%-${numstr_coll}s %+${numstr_yyyymm}s %+${numstr_status}s %+${numstr_missoutyyyymm}s %+${numstr_numout}s %+${numstr_lfemiscnt}s\n" "Collection" "$strhead_yyyymm" "$strhead_status" "$strhead_missoutyyyymm" "$strhead_numout" "$strhead_lfemiscnt" )"
    wmessage "$str_sep" 

    for fpst in ${arrpst_missyyyymm[@]};do 
#wmessage $fpst        
        #gcm_post.aer_inst_3hr_glo_L720x361_slv.j198401
        thiscoll=$( echo $fpst | cut -d'.' -f2 )
        thisyyyymm=$( echo $fpst | cut -d'.' -f3 | cut -c2- )
        thisstatus=$( echo $fpst | cut -d'.' -f4 )

        [[ "${arrcollmonpost_diu[@]}" =~ "$thiscoll" ]] && bldiu=true || bldiu=false
        [[ "$thisstatus" == "MS" ]] && thisnumfoutmiss=$( echo $fpst | cut -d'.' -f5 ) || thisnumfoutmiss="-"

        $bldiu && thisnumout=3 || thisnumout=2
        wmessage "$( printf "%-${numstr_coll}s %+${numstr_yyyymm}s %+${numstr_status}s %+${numstr_missoutyyyymm}s %+${numstr_numout}s %+${numstr_lfemiscnt}s\n" $thiscoll $thisyyyymm $thisstatus $thisnumfoutmiss $thisnumout $numflfe_mis_cnt )"
    done
    wmessage 
    wmessage "$strnote"
    wmessage
fi

if [[ -f $fsftin ]];then
    num_ftransfer=$( cat $fsftin | wc -l )
    wmessage "Legacy tar files still exist"
    wmessage "Extract $num_ftransfer legacy tar"
    wmessage 
    wmessage "shiftc input : $( realpath $fsftin )"

    #thiscmd="/usr/local/bin/shiftc --no-cron --no-mail --extract-tar -f -d"
    #                
    #if (( $writetofile == 1 ));then
    #    $thiscmd < $fsftin >> $fmessage
    #    exit
    #else
    #    $thiscmd < $fsftin 
    #    exit
    #fi
fi

exit





if $blftarcrpt;then 
    wmessage "Remove ${#arrftar_crpt[@]} corrupted tar files and re-submit run_gcmarch.sh"
    remove ${arrftar_crpt[@]}
    
    exp_updarchscr $arcqid $dexp

    wmessage "Run run_gcmarch.sh"
    execute_arch
    exit
fi

#todo:  run archive script when num of files are different and there are no gcmpost to submit
if $blfnumdifferent && (( ${#arrfpst[@]} == 0 ));then

    exp_updarchscr $arcqid $dexp

    wmessage "Run run_gcmarch.sh"
    execute_arch

elif ! $blfnumdifferent && (( ${#arrfpst[@]} == 0 )) && ! $blftarcrpt && (( ${#arrfmiss[@]} == 0 )) && \
     [[ -f $fcomp && -f $fdel ]] ;then

    wmessage "NO gcm_post.*.j* are running."
    wmessage "NO *.nc4.tar are corrupted."
    wmessage "NO missing output on LFE (file size and timestamp were checked)."
    
    [[ -f $fmessage ]] && mv $fmessage $dstdout/
    touch $fmark
    
elif [[ ! -f $fcomp || ! -f $fdel ]] ;then
    wmessage "Archiving has not been completed (there are no $( basename $fcomp ) and/or $( basename $fdel ) )."
    wmessage "Run run_gcmarch.sh"
    execute_arch
fi


exit
