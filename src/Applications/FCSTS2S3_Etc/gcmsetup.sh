#!/usr/bin/env bash

inputcheck (){
    [[ -z $dexp ]] && die "a exp dir path is a required input"
    
    int=$( misc_isinteger $runqsub )
    
    [[ -z $runqsub ]] && die "option r is a requied input"
    (( $int != 0 )) && die "an argument for option r has to be 0 or 1"
    
    (( $runqsub > 1 )) && die "an argument for option r has to be 0 or 1"

    [[ -n $gexp && ! -e $gexp ]] && die "(${FUNCNAME[0]}) setup does not exists"

    return
}

createfsed() {
    [[ -f $fsed ]] && rm -f $fsed
    if [[ $hstshort == pfe ]];then
    #if [[ $hstshort == pfe || $hstshort == dis ]];then
        #note:  this is for pfe setup
        #note: cice_nprocs doesn't seem to be in any config files. 
#s?@\<BLTAR\>?${bltar}?g
        cat > $fsed << EOF  
s?@\<BLFCST\>?$blfcst?g
s?@\<BLLEAVEOUT\>?$blleaveout?g
s?@\<BLRSYNC\>?${blrsync}?g
s?@\<BLBBSCP\>?${blbbscp}?g
s?@\<DBUILD\>?$DBUILD?g
s?@\<DFCST\>?$DFCST?g
s?@\<DARCH\>?$DARCH?g
s?@\<FCSTDATE\>?$fcstdate?g
s?@\<ENSEMBLE_MEMBER\>?$ensm?g
s?@\<GCMSETUPFILE\>?$gcmsetupfile?g
s?@\<QFEDCLIM\>?${qfedclim}?g
s?@\<END_DAT1\>?${end3_yyyymmdd_hh}?g
s?@\<END_DAT2\>?${end10_yyyymmdd_hh}?g
s?@\<FCSTSEGM\>?$fcstsegm?g
s?@\<MODELTAG\>?$ctag?g
s?@\<AOEGCMRUN\>?${strpbsresev_grn}?g
s?@\<AOEGCMPOST\>?${strpbsresev_pst}?g
s?@\<AOEGCMARCH\>?${strpbsresev_arc}?g
s?@\<RSTDATE\>?$rstdate?g
s?@\<RERUN\>?$rerun?g
s?@\<MODELNAME\>?$mname?g
s?@\<OGCM_NX\>?$ogcm_nx?g 
s?@\<OGCM_NY\>?$ogcm_ny?g
s?@\<OGCM_NX_GEOSS2S3\>?$ogcm_nx_geoss2s3?g
s?@\<OGCM_NY_GEOSS2S3\>?$ogcm_ny_geoss2s3?g
s?@\<NCPUS\>?$ncpus?g
s?@\<NUMC\>?$numc?g
s?@\<ASYNCNODE\>?$asyncnode?g
s?@\<GCMEMIP\>?$gcmemip?g
s?@\<GCMOUT\>?gcm_run.o@RSTDATE?g
s?@\<MODELNAMEPST\>?$model_pst?g
s?@\<NCPUSPST\>?$numc_pst?g
s?@\<WALLTIMEGCMRUN\>?$walltime?g
s?@\<WALLTIMEGCMPST\>?$walltime_pst?g
EOF

    elif [[ $hstshort == dis ]];then
        #note: this is for discover setup
        #s?@\<CICE_NPROCS?${CICE_NPROCS}?g
        #s?@\<EPES_SCORE\>?${epes_score}?g
        #s?@\<EPES_LCORE\>?${epes_lcore}?g
        #s?@\<BLTAR\>?${bltar}?g

        cat > $fsed << EOF
s?@\<BLFCST\>?$blfcst?g
s?@\<BLLEAVEOUT\>?$blleaveout?g
s?@\<BLRSYNC\>?${blrsync}?g
s?@\<BLBBSCP\>?${blbbscp}?g
s?@\<DBUILD\>?$DBUILD?g
s?@\<DFCST\>?$DFCST?g
s?@\<DARCH\>?$DARCH?g
s?@\<FCSTDATE\>?$fcstdate?g
s?@\<ENSEMBLE_MEMBER\>?$ensm?g
s?@\<GCMSETUPFILE\>?$gcmsetupfile?g
s?@\<QFEDCLIM\>?${qfedclim}?g
s?@\<END_DAT1\>?${end3_yyyymmdd_hh}?g
s?@\<END_DAT2\>?${end10_yyyymmdd_hh}?g
s?@\<FCSTSEGM\>?$fcstsegm?g
s?@\<MODELTAG\>?$ctag?g
s?@\<AOEGCMRUN\>?DELETE?g
s?@\<AOEGCMPOST\>?DELETE?g
s?@\<AOEGCMARCH\>?DELETE?g
s?@\<RSTDATE\>?$rstdate?g
s?@\<RERUN\>?${rerun}?g
s?@\<MODELNAME\>?'${mname}'?g
s?@\<OGCM_NX\>?$ogcm_nx?g 
s?@\<OGCM_NY\>?$ogcm_ny?g
s?@\<OGCM_NX_GEOSS2S3\>?$ogcm_nx_geoss2s3?g
s?@\<OGCM_NY_GEOSS2S3\>?$ogcm_ny_geoss2s3?g
s?@\<NCPUS\>?$ncpus?g
s?@\<NUMC\>?$numc?g
s?@\<ASYNCNODE\>?$asyncnode?g
s?@\<GCMEMIP\>?$gcmemip?g
s?@\<GCMOUT\>?gcm_run.o@RSTDATE?g
s?@\<MODELNAMEPST\>?DELETE?g
s?@\<NCPUSPST\>?DELETE?g
s?@\<WALLTIMEGCMRUN\>?$walltime?g
s?@\<WALLTIMEGCMPST\>?$walltime_pst?g
EOF
    fi

    return
}

set_expvars_fixed() {
        
    if [[ $hstshort == pfe ]];then
        [[ -z $rerun        ]] && rerun=FALSE
        [[ -z $walltime     ]] && walltime=08:00:00
        [[ -z $walltime_pst ]] && walltime_pst=08:00:00
        #[[ -z $bltar        ]] && bltar=TRUE
        [[ -z $blrsync      ]] && blrsync=true
        [[ -z $blbbscp      ]] && blbbscp=false
        [[ -z $model_pst    ]] && model_pst=ivy
        [[ -z $numc_pst     ]] && numc_pst=$( getnumcore $model_pst ) 
        [[ -z $nummonth2    ]] && nummonth2=7

    elif [[ $hstshort == dis ]];then

        [[ -z $rerun        ]] && rerun=FALSE
        [[ -z $walltime     ]] && walltime=12:00:00
        [[ -z $walltime_pst ]] && walltime_pst=12:00:00
        #[[ -z $bltar        ]] && bltar=TRUE
        [[ -z $blrsync      ]] && blrsync=true
        [[ -z $blbbscp      ]] && blbbscp=true

        #Note: 05/02/2023 work on these two variables once experiments
        #                 starts running on discover.
        [[ -z $model_pst    ]] && model_pst=""
        [[ -z $numc_pst     ]] && numc_pst=""
        [[ -z $nummonth2    ]] && nummonth2=7

    fi
    gcmsetupfile=gcm_GEOSS2S3_setup
    fcstsegm=00000400
    qfedclim=TRUE
    gcmemip=FALSE
    rsttime=210000
    nummonth1=3
    #nummonth2=7

    return
}

set_expvars() {
    
    #todo:  calculate the end date for three-month run.
    #note:  end date is 3 month from fcst date, not rst date. 
    local strxmonth=$( nextXmonths $( date -d $fcstdate +%Y ) $( printf '%01g' $( date -d $fcstdate +%m ) ) 0 $(( nummonth1 + 1 ))  )
    local end_mm=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f1 )
    local end_year=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f2 )
    end3_yyyymmdd_hh=$end_year$end_mm"01 $rsttime"
 
    #tood:  calculate the end date for seven-month run.
    local nummon_total=$(( nummonth1 + nummonth2 ))
    local strxmonth=$( nextXmonths $( date -d $fcstdate +%Y ) $( printf '%01g' $( date -d $fcstdate +%m ) ) 0 $(( nummon_total + 1 ))  )
    local end_mm=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f1 )
    local end_year=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f2 )
    
    end10_yyyymmdd_hh=$end_year$end_mm"01 $rsttime"

    drstsrc=$DRST/$rstdate/$ensm
    [[ ! -d $drstsrc ]] && wmessage $drstsrc does not exist.
    
    return
}

select_fgrnpbs() {
    local _fgrn=$dexp/gcm_run.j
    local _fsed=$dexp/${FUNCNAME[0]}_sedfile

    [[ -f $_fsed ]] && rm -f $_fsed

    local numln_nonres_qname=$( grep -n "\-q normal" $_fgrn | cut -d':' -f1 )
    local numln_nonres_model=$(( numln_nonres_qname - 1 ))
    
    local numln_res_qname=$(( numln_nonres_qname + 1 ))
    local numln_res_model=$(( numln_nonres_qname + 2 ))
        
    if $blres;then
        echo "${numln_nonres_qname}s?^?#?g" >> $_fsed
        echo "${numln_nonres_model}s?^?#?g" >> $_fsed
        echo "s?@\<RESNAME\>?$queueres?g"   >> $_fsed
    else
        echo "${numln_res_qname}s?^?#?g" >> $_fsed
        echo "${numln_res_model}s?^?#?g" >> $_fsed
    fi

    sed -i.bak -f $_fsed $_fgrn

    [[ -f $_fsed ]] && rm -f $_fsed
    [[ -f $_fgrn.bak ]] && rm -f $_fgrn.bak

    return
}

select_farcpbs() {
    local _farc=$dexp/archive/run_gcmarch.sh
    local _fsed=$dexp/${FUNCNAME[0]}_sedfile

    [[ -f $_fsed ]] && rm -f $_fsed

    local numln_nonres_qname=$( grep -n "\-q normal" $_farc | cut -d':' -f1 )
    local numln_nonres_model=$(( numln_nonres_qname - 1 ))
    
    local numln_res_qname=$(( numln_nonres_qname + 1 ))
    local numln_res_model=$(( numln_nonres_qname + 2 ))
        
    if $blresarc;then
        echo "${numln_nonres_qname}s?^?#?g"          >> $_fsed
        echo "${numln_nonres_model}s?^?#?g"          >> $_fsed
        echo "s?@\<RESNAMEARCH\>?$queueres_arch?g"   >> $_fsed
    else
        echo "${numln_res_qname}s?^?#?g" >> $_fsed
        echo "${numln_res_model}s?^?#?g" >> $_fsed
    fi

    sed -i.bak -f $_fsed $_farc

    [[ -f $_fsed ]] && rm -f $_fsed
    [[ -f $_farc.bak ]] && rm -f $_farc.bak 
    
    return
}

clean_dir (){
    [[ -n $flock && -f $flock ]] && rm -f $flock
}

#================================================================================
#                                     Begin
#================================================================================
#beg
hstname=$( hostname )
blnode_nas=false
blnode_nccs=false
if [[ ${hstname:0:3} == pfe ]];then
    :
elif [[ ${hstname:0:3} == dis ]];then
    :
elif [[ "${hstname:0:1}" == r ]];then 
    #note:  this is for host other than pfe @ NAS which has a name starting with 'r'
    blnode_nas=true

elif [[ "${hstname:0:4}" == borg || "${hstname:0:4}" == warp ]];then 
    #note:  this is for host other than pfe @ NCCS which has a name starting with 'borg' or 'warp'
    blnode_nccs=true
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

    trap clean_dir EXIT
fi

if [[ ! $cdir/func_fcst.sh ]];then 
    echo "$cdir/func_fcst.sh does not exist";exit
else
    source $cdir/func_fcst.sh
fi

optt=false
writetofile=0
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        create fcst exp dir, copy restart files, and submit a job.
        Usage: ./"$(basename $0)"  [-hw] [-q qid] [-a archive qid ] srcme_file [1 or 0] dexp  

        required:
            Source file, which set various vars for a set of runs (i.e. srcme_pfe_fcst)
            Enter 1 to submit a job after exp dir is ready; 0 for setting up exp dir only
            An exp dir path

        options:
            -a  queue id for run_gcmarch.sh
            -q  queue id for gcm_run.j
            -m  stdout file name
            -h  show this help text
"

OPTIND=1
while getopts ':hq:m:a:' option; do
    case "$option" in
        a)  qid_arch=$OPTARG;;
        m)  fmsg=$OPTARG;;
        q)  qid=$OPTARG;;
        h)  echo "$usage"; exit 0;;
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
(( ${#arrposarg[@]} != 3 )) && die "3 inputs are required"
#================================================================================
#                               Check User Inputs
#================================================================================
i=0
srcf=${arrposarg[$i]};i=$(( i + 1 ))
runqsub=${arrposarg[$i]};i=$(( i + 1 ))
dexp=${arrposarg[$i]};i=$(( i + 1 ))
source $srcf
#================================================================================
#                             Set Host Specific Vars
#================================================================================
hstshort=$( get_host )
#[[ "$hstshort" == dis ]] && die "this script runs on pfe only as of 20210929" && exit

if [[ $hstshort == dis ]];then
    cmdsubmit=sbatch
    queue=
    mname="cas|hasw"
    blres=false
    asyncnode=1
    
    #note:  As of 09/30/2022, ncpus is hardcorded since I don't know which model 
    #       v3 real-time will run. For testing, use cas|hasw.
    ncpus=4840

    #NOTE - USE THESE VALUES FOR TESTING PURPOSE (09/12/2022)    
    #blrome=true

elif [[ $hstshort == pfe ]];then
    #cmdsubmit=qsub
    cmdsubmit=/PBS/bin/qsub

    #todo:  get model and qname for gcm_run.j
    if [[ "$qid" == "normal" ]] || [[ "$qid" == "long" ]];then
        queuenonres=$qid
        queueres=$qid
        blres=false
        if [[ "$ctag" =~ "ROME" && -z $mname ]];then
           mname=rom_ait 
        elif [[ -z $mname ]];then
           mname=sky_ele
        fi
        
        if [[ -z $walltime     ]];then 
            [[ $qid == "normal" ]] && walltime=08:00:00 || walltime=12:00:00
        else
            if [[ $qid == "normal" && "$walltime" != "08:00:00" ]];then 
                walltime=08:00:00 
            fi
        fi

    elif [[ -n $qid ]];then
        queuenonres=normal
        #queueres=$( res_resname $qid )
        queueres=$qid
        mname=$( res_mname $qid )
        blres=true
        
        [[ -z $walltime     ]] && walltime=12:00:00
    fi

    #todo:  check if there is a reservation for run_gcmarch.sh
    #note:  ivy node is selected for run_gcmarch.sh. It is hardcoded in gcm_setup. 
    if [[ -n $qid_arch ]];then
        blresarc=true
        queueres_arch=$( res_resname $qid_arch ) 
    else
        blresarc=false
    fi

    
    #todo:  check if this setup is specific for rome nodes
    if [[ "$mname" =~ "rom_ait" ]];then 
        blrome=true
        asyncnode=1
        [[ -z $numc ]] && die "numc is undefined"
    else
        blrome=false
        asyncnode=2
    fi

    #[[ "$ctag" =~ "TOSS" ]] && bltoss=true || bltoss=false 
    bltoss=true

    ncpus=$( getnumcore $mname ) 
fi

[[ -z $numc || -z $ogcm_nx || -z $ogcm_ny || -z $ogcm_nx_geoss2s3 || -z $ogcm_ny_geoss2s3 ]] && \
die "numc, ogcm_nx, ogcm_ny, ogcm_nx_geoss2s3 and/or ogcm_ny_geoss2s3 are undefined"
[[ -z $DFCST || -z $DRST || -z $DARCH || -z $DBUILD ]] && die "DFCST, DRST, DARCH, and/or DBUILD are undefined"
[[ ! -d $DRST ]] && die "DRST does not exist"
#================================================================================
#                                 Set Variables
#================================================================================
#mid
[[ -n $rstexpid ]] && strexpid=$strexpid$rstexpid

dstdout=$cdir/stdout/$strscr/$strdout
fgrn=gcm_run.j
fcapend=cap_end
fhomdir=.HOMDIR
fsed=${strscr}_sedfile
fmark=${strscr}_setupready

set_expvars_fixed

inputcheck
set_rstfcstdate $dexp
set_expvars

dapp=$DBUILD/Applications/GEOSgcm_App
   fsrcme_org=srcme_GEOSS2S3_gcmsetup
fgcmsetup_org=gcm_GEOSS2S3_setup
       fsrcme=srcme_${hstshort}_gcmsetup_${strexpid}_$fcstdate$ensm 
    fgcmsetup=gcm_setup_${strexpid}_$fcstdate$ensm
      fstdout=stdout_gcmsetup_${strexpid}_$fcstdate$ensm

strpbsresev_grn="toss4"
strpbsresev_arc="toss4"
strpbsresev_pst="toss4"

[[ ! -d $dstdout ]] && mkdir -p $dstdout
[[ -d $dexp && ! -f $fmark ]] && rm -rf $dexp
#================================================================================
#                                  Main Process
#================================================================================
#letsbegin
msg_wheader_userdefined 80 =
wmessage "exp location        : $dexp"
wmessage "restart location    : $DRST"
wmessage "restart yyyymmdd    : $rstdate"
wmessage " 3-month end        : $end3_yyyymmdd_hh"
wmessage "10-month end        : $end10_yyyymmdd_hh"

[[ -f $fmark ]] && wmessage "$fmark exist ... exit" && exit 1

wmessage "model               : $mname"
wmessage "walltime            : $walltime"
wmessage "walltime (gcm_post) : $walltime_pst"
wmessage "# of async cores    : $asyncnode"

if [[ $hstshort == pfe ]];then 
    wmessage "name of queue       : $queueres"
elif [[ $hstshort == dis ]];then 
    wmessage "name of partition   : $setup_partition"
    wmessage "quality of service  : $setup_qos"
fi

wmessage

#+++++ cd dapp (start) +++++
cd $dapp 

[[ -f $fgcmsetup ]] && rm -f $fgcmsetup
[[ -f $fsrcme    ]] && rm -f $fsrcme
[[ -f $fstdout   ]] && rm -f $fstdout
[[ -f $fsed      ]] && rm -f $fsed
    
createfsed

sed -e "s#@THISSRCME#$dapp/${fsrcme}#g" $fgcmsetup_org > $fgcmsetup
sed -f $fsed $fsrcme_org > $fsrcme
sed -i '/DELETE/d' $fsrcme
chmod 750 $fgcmsetup $fsrcme

[[ -f $fsed ]] && rm -f $fsed

wmessage

msg_wheader_userdefined 40 - "$fsrcme"
cat $fsrcme 

wmessage 

msg_wheader_userdefined 40 - "$fgcmsetup Stdout"
./$fgcmsetup

cd - >/dev/null 
#+++++ cd dapp ( end ) +++++

[[ -f $dapp/$fgcmsetup ]] && rm -f $dapp/$fgcmsetup 
   [[ -f $dapp/$fsrcme ]] && rm -f $dapp/$fsrcme    

#++++ cd to dexp (start) +++++
cd $dexp
        
#todo:  create symbolic links
arrfslink=( CAP_1.rc HISTORY_1.rc )

if (( $fcstyyyy < 2017 ));then
    arrfslink+=( AGCM_b2017.rc )
else
    arrfslink+=( AGCM_a2017.rc )
fi

for fcore in ${arrfslink[@]};do
    strslink=$( echo $fcore | cut -d'_' -f1 )
    strslinkext=$( echo $fcore | cut -d'.' -f2 )
    if [[ -f $fcore ]];then
        ln -s $fcore $strslink.$strslinkext
    else
        die "$fcore does not exist"
        exit
    fi
done

if (( $fcstyyyy < 2017 ));then
    rm -f gcm_run.j.2017p
elif (( $fcstyyyy >= 2017 )) ;then
    rm -f gcm_run.j
    mv gcm_run.j.2017p gcm_run.j
fi

#todo:   select PBS statements for gcm_run.j
[[ $hstshort == pfe ]] && select_fgrnpbs

#todo:   select PBS statements for run_gcmarch.sh
#select_farcpbs

#note:  10/20/2022 This code is specific for Hunga Tonga experiment
if [[ "$strexpid" == "htesch" || "$strexpid" == "hterpl" ]];then 
    fagcm=$( readlink AGCM.rc ) 
    strsrc1=species.data
    fpchem2=/nobackup/gmao_SIteam/ModelData/bcs/Icarus-NL/Shared/pchem.species.H2OprodlossRate_2022.CMIP-5.1870-2097.z_91x72.nc4
    fpchem1=$( grep $strsrc1 $fgrn | grep -Ev '^#' | rev | cut -d' ' -f2 | rev ) 
    strsrc_prdloss="H2O_ProdLoss" 
    
    wmessage 
    wmessage "**Note: $strexpid requires $( basename $fpchem2 ) species data and $strsrc_prdloss set to 1 in $fagcm" 
    wmessage 

    grep $strsrc_prdloss $fagcm > /dev/null  2>&1
    status_grep=$?
    (( $status_grep > 0 )) && echo "H2O_ProdLoss: 1" >> $fagcm
    
    sed -i.org "s#$fpchem1#$fpchem2#g" $fgrn
fi

#todo:   delete gcmpost*.script. 
shopt -s nullglob
arrgpst=( gcmpost*.script )
shopt -u nullglob
(( ${#arrgpst[@]} > 0 )) && rm -f ${arrgpst[@]} 2>/dev/null


#todo:  get rst files
#note:  02/01/2022  Use rsync to transfer rst files instead of 
#                   shiftc
wmessage "... get ic $fcstdate ($rstdate)"
getrst $drstsrc $dexp


if (( $runqsub == 1 ));then
    #todo:  qsub job
    if [[ -f $dexp/$fgrn ]];then
        wmessage "... submit $fcstdate $ensm "
        $cmdsubmit gcm_run.j
        (( $? > 0 )) && die "qsub failed"
    else
        die "... $fgrn does not exist ... "
    fi
else
    wmessage "... $fcstdate$ensm is ready ... "
fi

cd - >/dev/null
#++++ cd to dexp ( end ) +++++

#todo:  create a marker
touch $dexp/$fmark

[[ ! -f $dexp/$fcapend ]] && echo $end10_yyyymmdd_hh > $dexp/$fcapend

if [[ $hstshort == pfe ]];then 
    #++++ cd to DFCST/fcstdate (start) +++++
    cd $DFCST/$fcstdate
    setfacl -R -m u:knakada:rwx $ensm
    cd - >/dev/null
    #++++ cd to DFCST/fcstdate ( end ) +++++
fi

#todo:  mv fmessage to a final destination
if [[ -n $fmsg ]];then 
    fmsg_name=$( basename $fmsg )
    [[ -f $dstdout/$fmsg_name ]] && rm -f $dstdout/$fmsg
    [[ -f $fmsg ]] && mv $fmsg $dstdout/ 
fi



