#!/usr/bin/env bash

#set -e
#trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
inputcheck() {
    [[ -z  ${arrposarg[@]} ]] && die "an input is missing"
    (( ${#arrposarg[@]} != 1 )) && die "1 input is required"

    local srcme=$( echo ${arrposarg[0]} | cut -d'_' -f2 )
    [[ "$srcme" != "$hstshort" ]] && die "source file is not for this host (select srcme_${hstshort}_* file)"

    return
}

cp_miniscr(){
    local _dexp=$1 
    local fsed=${strscr}_sedfile

    [[ -f $fsed ]] && rm -f $fsed

    #todo:  copy mini_* script to _dexp
    cp -p $cdir/$fgrn $_dexp/
    
    cat > $fsed << EOF
s?@\<DFCST\>?${DFCST}?g
s?@\<FCSTDATE\>?${fcstdate}?g
s?@\<ENSEMBLE_MEMBER\>?${ensm}?g
EOF
    
    #todo:  edit mini_gcm_run.j scr 
    sed -i -f $fsed $_dexp/$fgrn
    
    [[ -f $fsed ]] && rm -f $fsed

}

recreate_gcmpost(){
    #description:   resubmit gcm_post
    local fpst_org=$dexp/post/gcm_post.j.org
    local fgcmpst_new=$dexp/post/tmp_${strscr}_new_gcm_post.j
    local fgcmpst_tmp=$dexp/post/tmp_thisgcm_post.j
    local _fsed=$dexp/post/${strscr}_sedfile
    local post_t="08:00:00" 
    local batch_group="PBS -W group_list=g0609"
    local batch_outputname="PBS -o "
    local batch_joinouterr="PBS -j oe -k oed"
    
    local pbsselect_res_org="PBS -l select=1:ncpus=20:mpiprocs=20:model=ivy:aoe=toss"
    local pbsqid_res_org="PBS -q $pstqid"
    local pbsselect_nrm_org="PBS -l select=1:ncpus=20:mpiprocs=20:model=ivy"
    local pbsqid_nrm_org="PBS -q normal"
        
    [[ -f $_fsed ]] && rm -f $_fsed
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
    
    [[ -f $fgcmpst_tmp ]] && mv $fgcmpst_tmp $fgcmpst_new
  
    #todo:  select pbs statements 
    if $blrunres && $blres;then
        pbsselect_res=$pbsselect_res_org
        pbsqid_res=$pbsqid_res_org
        pbsselect_nrm="#$pbsselect_nrm_org"
        pbsqid_nrm="#$pbsqid_nrm_org"

        cat > $_fsed << EOF
s?@POST_T?${post_t}?g
s?@\<DFCST\>?${DFCST}?g
s?@\<FCSTDATE\>?${fcstdate}?g
s?@\<ENSEMBLE_MEMBER\>?${ensm}?g
s?@\<POSTPRC_PRES\>?${pbsselect_res}?g
s?@\<POSTPRC_QRES\>?${pbsqid_res}?g
s?@\<POSTPRC_P\>?${pbsselect_nrm}?g
s?@\<POST_Q\>?${pbsqid_nrm}?g
s?@\<BATCH_GROUP\>?${batch_group}?g
s?@\<BATCH_JOINOUTERR\>?${batch_joinouterr}?g
s?@BATCH_OUTPUTNAME?${batch_outputname}?g
s?nobackupp18?nobackupp28?g
EOF

    elif ! $blrunres; then
        pbsselect_res="#$pbsselect_res_org"
        pbsqid_res="#$pbsqid_res_org"
        pbsselect_nrm="$pbsselect_nrm_org"
        pbsqid_nrm="$pbsqid_nrm_org"
        numcpus=1 
        cat > $_fsed << EOF
s?@POST_T?${post_t}?g
s?@\<DFCST\>?${DFCST}?g
s?@\<FCSTDATE\>?${fcstdate}?g
s?@\<ENSEMBLE_MEMBER\>?${ensm}?g
s?@\<POSTPRC_P\>?${pbsselect_nrm}?g
s?@\<POST_Q\>?${pbsqid_nrm}?g
s?@\<BATCH_GROUP\>?${batch_group}?g
s?@\<BATCH_JOINOUTERR\>?${batch_joinouterr}?g
s?@BATCH_OUTPUTNAME?${batch_outputname}?g
s?\<NULL\>?${numcpus}?g
s?nobackupp18?nobackupp28?g
EOF

    fi

    [[ -f $dexp/post/gcm_post.j ]] && rm -f $dexp/post/gcm_post.j
    sed -f $_fsed $fgcmpst_new >> $dexp/post/gcm_post.j

    [[ -f $fgcmpst_new ]] && rm -f $fgcmpst_new

    if $blrunres; then
        local fthistmp=$dexp/post/gcm_post.j_tmp
        
        mv $dexp/post/gcm_post.j $fthistmp

        pbsselect_res1=$( grep "$pbsselect_res" $fthistmp )
        char2_select=$( echo $pbsselect_res1 | cut -c1-2 )
        
        pbsqid_res1=$( grep "$pbsqid_res" $fthistmp )
        char2_qid=$( echo $pbsqid_res1 | cut -c1-2 )
        
        [[ "$char2_select" == "##" ]] && pbsselect_rep=$( echo $pbsselect_res1 | cut -c2- )
        [[ "$char2_qid" == "##" ]] && pbsqid_rep=$( echo $pbsqid_res1 | cut -c2- )
        
        sed -e "s/$pbsselect_res1/$pbsselect_rep/g" -e "s/$pbsqid_res1/$pbsqid_rep/g" $fthistmp >> $dexp/post/gcm_post.j

        [[ -f $fthistmp ]] && rm -f $fthistmp
    fi

    chmod 755 $dexp/post/gcm_post.j
        
    [[ -f $_fsed ]] && rm -f $_fsed
    [[ -f $fgcmpst_new ]] && rm -f $fgcmpst_new

    return
}

mvout2holding(){
    local _dexp=$1;shift
    local _darc=$1
    local _flfeout=$cdir/tmp_${strscr}_${strexpid}_${fcstdate}${ensm}_lfeout
    local    blextracttar=true     #extract tars in _dexp/collection into holding/collection
    local   blcopymonthly=true     #copy back mapl-monthly into holding/collection
    local bllfeextracttar=true
    #local blcopyleftover=false    #copy holding dir in src dir into _dexp dir - use this if _dexp and _dexp are different.
    local coll

    local fhis=$_dexp/HISTORY_2.rc
    local dhold=$_dexp/holding
    local arrcoll=($( exp_getcollections $fhis ))
    local arrcollmon_mapl=($( exp_getcollfreq m $fhis ))
    local arrcollmonpost=($( exp_getcollfreq n $fhis ))
    local arrcollmonpost_diu=($( exp_getcolldiurnal $fhis ))
    local arrcollmonpost_save=($( printf '%s\n' ${arrcollmonpost[@]} | grep -E '_slv|_sfc' | sort -V ))
   
    [[ -f $_flfeout ]] && rm -f $_flfeout

    printf "$dhold/%s\n" ${arrcoll[@]} | xargs -i bash -c "[[ ! -d {} ]] && mkdir -p {} && echo created {}"

    wmessage

    if $blextracttar;then
        #todo:  extract files from tar files
        for coll in ${arrcollmonpost[@]};do 
            if [[ -d $_dexp/$coll ]] ;then
                arrftar=($( find $_dexp/$coll/* -maxdepth 0 -type f -name "$fcstdate.$coll.[0-9]?????.tar" 2>/dev/null ))
                
                for ftar in ${arrftar[@]};do
                    local yyyymm=$( basename $ftar | rev | cut -d'.' -f3 | rev )
                    local dhold_yyyymm=$dhold/$coll/$yyyymm
                    [[ ! -d $dhold_yyyymm ]] && mkdir -p $dhold_yyyymm

                    if [[ -d $dhold_yyyymm ]];then
                        wmessage "Extract $( basename $ftar ) on PFE into $dhold_yyyymm"
                        tar --skip-old-files -xf $ftar -C $dhold_yyyymm
                    fi
                done
            else
                mkdir -p $_dexp/$coll
            fi
        done 
    fi

    wmessage

    #local arrcoll_empty=($( printf '%s\n' ${arrcollmonpost[@]} | xargs -i bash -c "find $_dexp/* -maxdepth 0 -type d -name {} -empty -exec basename {} \;" ))

    if $bllfeextracttar;then
        exp_getfname_lfe $_dexp $_darc $_flfeout

        for coll in ${arrcollmonpost[@]};do
            #19820101.atm_inst_6hr_glo_L720x361_p49.198201.nc4.tar
            local arrftar=($( grep "$fcstdate.$coll." $_flfeout 2>/dev/null | grep nc4.tar | grep -v daily | rev | cut -d' ' -f1 | rev | sort -V )) 

            for ftar in ${arrftar[@]};do             
                local yyyymm=$( basename $ftar | rev | cut -d'.' -f3 | rev )
                local dhold_yyyymm=$dhold/$coll/$yyyymm
                [[ ! -d $dhold_yyyymm ]] && mkdir -p $dhold_yyyymm
    
                wmessage "Extract $( basename $ftar ) on LFE into $dhold_yyyymm"
                ssh -q lfe tar --skip-old-files -xf $ftar -C $dhold_yyyymm
                
                #local _dyyyymm_empty=$( find $dhold/$coll/* -type d -name $yyyymm -empty 2>/dev/null )
                #
                #if [[ "$dhold_yyyymm" == "$_dyyyymm_empty" ]];then
                #    #wmessage "... extract $( basename $ftar ) into $dhold_yyyymm..."
                #    #ssh -q lfe tar --skip-old-files -xf $ftar -C $dhold_yyyymm/ 

                #    wmessage "Extract $( basename $ftar ) into $dhold/$coll"
                #    ssh -q lfe tar --skip-old-files -xf $ftar -C $dhold/$coll/
                #else
                #    wmessage "... directory is NOT empty - $dhold_yyyymm"
                #fi
            done

        done

    fi

    wmessage
    
    if $blcopymonthly;then
        local coll fmplmon

        #todo:  make holding/collection dir
        printf "$dhold/%s\n" ${arrcollmon_mapl[@]} | xargs -i bash -c "[[ ! -d {} ]] && mkdir -p {} && echo created {}"

        for coll in ${arrcollmon_mapl[@]};do
            arrfmplmon=($( find $_dexp/$coll/* -type f -name "$fcstdate.$coll.[0-9]???????_????z.nc4" ))

            for fmplmon in ${arrfmplmon[@]}; do 
                #19820302.rad_tavg_1mo_glo_L720x361_p49.19820501_0000z.nc4
                local yyyymm=$( basename $fmplmon | cut -d'.' -f3 | cut -c1-6 )
                local dhold_yyyymm=$dhold/$coll/$yyyymm
                [[ ! -d $dhold_yyyymm ]] && mkdir -p $dhold_yyyymm

                wmessage "Copy $coll mapl-monthly "
                cp -p $fmplmon $dhold_yyyymm./
            done 
        done 
    fi
   
    wmessage

   [[ -f $_flfeout ]] && rm -f $_flfeout

    return

}    

exp_mkcollgcmpost() {
    local _dexp=$1;shift
    local _coll=$1;shift
    local _yyyymm=$1
    local _fpst_org=$_dexp/post/gcm_post.j
    local     _fpst=$_dexp/post/gcm_post.$_coll.j$_yyyymm

    local _fsed=$_dexp/post/tmp_${strscr}_${FUNCNAME[0]}_sedfile

    [[ -f $_fsed ]] && rm -f $_fsed
    
    #POST_O=gcm_post.sfc_tavg_3hr_glo_L720x361_sfc.o198201
    local post_o=gcm_post.$_coll.o$_yyyymm
    cat > $_fsed << EOF
s?@POST_O?${post_o}?g
s?@COLLECTION?${_coll}?g
s?@YYYYMM?${_yyyymm}?g
EOF
    sed -f $_fsed $_fpst_org > $_fpst
    chmod 755 $_fpst
    [[ -f $_fsed ]] && rm -f $_fsed
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

[[ -z $cdir || ! -d $cdir ]] &&  echo "cdir is undefined"  && exit

cd $cdir

#todo:  check if this script is invoked as child script
#[[ -n $RUNCHILD ]] && blchild=true || blchild=false
blchild=true
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
optr=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename $0) -- this is a program to:
        re-create holding dir to execute gcmpost.

        Usage: ./$(basename "$0") [-d exp dir] srcme_file

        input:        
            A source file, which set various vars for a set of runs (i.e. srcme_pfe_*)

        options:
            -d  a full path to an experiment dir
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
#================================================================================
#                                 Set Variables
#================================================================================
#mid
set_rstfcstdate $dexp

dmess=$cdir/message
dstdout=$cdir/stdout/$strscr/$strdout
darc=$DARCH/$fcstdate/$ensm

fmessage=$dmess/message_${strscr}_${strexpid}_${fcstdate}${ensm}
fgrn=mini_gcm_run.j
fhis=$dexp/HISTORY_2.rc
fpst_default=$DBUILD/Applications/GEOSgcm_App/gcm_post_GEOSS2S3.j
farc_default=$DBUILD/Applications/GEOSgcm_App/run_gcmarch.sh

fcomp=$dexp/archive/gcmarch_archcompleted
fdel=$dexp/archive/gcmarch_deloutcompleted
blheader=false

arrcollmonpost=($( exp_getcollfreq n $fhis ))
arrcoll_empty=($( printf '%s\n' ${arrcollmonpost[@]} | xargs -i bash -c "find $dexp/* -maxdepth 0 -type d -name {} -empty -exec basename {} \;" ))

#note:  submit jobs to reserved nodes or normal queue
#blrunres=true
blrunres=false
#blres=true
#pstqid=R12753106

arcqid=none

[[ ! -d $dstdout ]] &&  mkdir -p $dstdout
#================================================================================
#                                  Main Process
#================================================================================
#main
! $blheader && writeheader $dexp $darc && blheader=true

#todo:  copy scripts.
#cp_miniscr $dexp
recreate_gcmpost

#exp_updrunpostscr $dexp
#wmessage

[[ -f $fcomp ]] && rm -f $fcomp 2>/dev/null
[[ -f $fdel  ]] && rm -f $fdel  2>/dev/null
exp_updarchscr $arcqid $dexp


#todo:  put outputs back in holding dir
mvout2holding $dexp $darc
yyyymm_last=$( cat $dexp/cap_restart | cut -d' ' -f1 | cut -c1-6 )

#todo: create missing gcm_post.*.j*
for coll in ${arrcollmonpost[@]};do 

    #todo:  find yyyymm dir
    arrdyyyymm=($( find $dexp/holding/$coll/* -maxdepth 0 -type d -name "[0-9]?????" | grep -v $yyyymm_last | xargs -i basename {} ))

    #gcm_post.sfc_tavg_3hr_glo_L720x361_sfc.j198201
    arrfpstcoll_miss=($( printf "$dexp/post/gcm_gcmpost.$coll.j%s\n" ${arrdyyyymm[@]} | sort -V | xargs -i bash -c '[[ ! -f {} ]] && echo {} ' ))
    for fpstcoll_miss in ${arrfpstcoll_miss[@]};do
        thisyyyymm=$( basename $fpstcoll_miss | rev | cut -d'.' -f1 | cut -c1-6 | rev )
        wmessage "Make $( basename $fpstcoll_miss )"
        exp_mkcollgcmpost $dexp $coll $thisyyyymm
    done 
done 

wmessage 

#todo:  excute run_gcmarch.sh
#note:  as of 07/29/2022, this qid is available
thisqid=R13909086

#+++++ cd dexp/archive (start) +++++
cd $dexp/archive
    ./run_gcmarch.sh -q $thisqid >> stderr_gcmarch 2>&1
cd - >/dev/null 
#+++++ cd dexp/archive ( end ) +++++

[[ -f $fmessage ]] && mv $fmessage $dstdout/ 2>/dev/null 

exit





#+++++ cd dexp (start) +++++
cd $dexp
if (( $writetofile == 1 ));then 
    echo "$( /PBS/bin/qsub $fgrn )" >> $fmessage
    status_qsub=$?
else
    /PBS/bin/qsub $fgrn 
    status_qsub=$?
fi
cd - >/dev/null
#+++++ cd dexp ( end ) +++++


if (( $status_qsub > 0 )) && [[ -n $status_qsub ]] ;then 
    wmessage qsub failed
else
    [[ -f $fmessage ]] && mv $fmessage $dstdout/ 2>/dev/null 
fi

exit


