#!/usr/bin/env bash

inputcheck(){
    [[ ! -f $strf ]] && die "srcme file is required input and does not exist"
    return
}

update_archscr(){
    #!!!!! NOTE:  02/18/2022 - this function is moved to func_fcst.sh as exp_updarchscr !!!!!
    #description:   copy and edit updated gcmarch script to entered exp dir.
    local _arrdexp=( "$@" )
    local fsed=${strscr}_${FUNCNAME[0]}_sedfile
    local groupid=g0609
    local farchscr=run_gcmarch.sh
    local farchscr_beta=run_gcmarch_beta.sh
    local _arrstrexpid=( fcst myexp rim )
    local dexp

    local archive_p="PBS -l select=1:ncpus=20:mpiprocs=20:model=ivy:aoe=sles12"
    local archive_q="PBS -q normal"
    local BATCH_GROUP="PBS -W group_list="
    if [[ -n $blusesetuptar ]] && $blusesetuptar;then
        local dsetup=$DFMOD/$ctag/setup_pfe_${strexpid}_v3
        local farch_beta=$dsetup/archive/$farchscr

    elif [[ -z $blusesetuptar ]] || ! $blusesetuptar;then
        local farch_beta=$dbuild/Applications/GEOSgcm_App/$farchscr_beta
    fi

    [[ -f $farch_beta ]] && : || die "$farch_beta does not exist"

    for dexp in ${_arrdexp[@]};do
        local darch=$dexp/archive
        local farch_exp=$darch/$farchscr
        local farch_tmpexp=$darch/tmp_$farchscr
        local farch_exp_old=$darch/_$farchscr
        local farchlock=$( echo $farchscr | cut -d'_' -f2 | cut -d'.' -f1 ).lock

        [[ -f $farchlock ]] && die "$farchlock exist" 
        [[ -f $farch_exp_old ]] && rm -f $farch_exp_old 

        #todo:  if arch script in archive dir doesn't exist, ignore. If it does
        #       it is renamed with "_" in front of it.
        if [[ ! -d $darch ]];then
            continue
        else
            [[ -f $farch_exp ]] && mv $farch_exp $farch_exp_old
            cp -p $farch_beta $farch_tmpexp
        fi
       
        #todo:  set variables. 
        set_rstfcstdate $dexp

        if [[ "${_arrstrexpid[@]}" =~ "$strexpid" ]];then  

            cat > $fsed << EOF
s?@\<NX_SCORE\>?${nx_score}?g
s?@\<DFCST\>?${DFCST}?g
s?@\<FCSTDATE\>?${fcstdate}?g
s?@\<ENSEMBLE_MEMBER\>?${ensm}?g
s?@\<DARCH\>?${DARCH}?g
s?@BATCH_GROUP?${BATCH_GROUP}${groupid}?g
s?@GROUPID?${groupid}?g
s?@\<BLLEAVEOUT\>?$blleaveout?g
s?@\<BLTAR\>?$bltar?g
s?@\<BLRSYNC\>?$blrsync?g
s?@\<ARCHIVE_P\>?${archive_p}?g
s?@\<ARCHIVE_Q\>?${archive_q}?g
EOF
        else

            cat > $fsed << EOF
s?@\<DFCST\>?${DFCST}?g
s?@\<FCSTDATE\>?${fcstdate}?g
s?@\<ENSEMBLE_MEMBER\>?${ensm}?g
s?@\<DARCH\>?${DARCH}?g
s?@GROUPID?${groupid}?g
s?@\<BLLEAVEOUT\>?$blleaveout?g
s?@\<BLTAR\>?$bltar?g
s?@\<BLRSYNC\>?$blrsync?g
s?@\<ARCHIVE_P\>?${archive_p}?g
s?@\<ARCHIVE_Q\>?${archive_q}?g
EOF
        fi
        #todo:  edit arch scr 
        sed -i -f $fsed $farch_tmpexp

        if [[ -f $farch_exp_old && -f $farch_tmpexp ]];then 
            diff -s $farch_exp_old $farch_tmpexp >/dev/null
            local status_diff=$?
            if (( $status_diff > 0 ));then
                mv $farch_tmpexp $farch_exp
                chmod 755 $farch_exp
            else
                mv $farch_exp_old $farch_exp
            fi

        elif [[ -f $farch_tmpexp ]];then
            mv $farch_tmpexp $farch_exp && chmod 755 $farch_exp

        elif [[ ! -f $farch_tmpexp ]];then 
            wmessage "... $farch_tmpexp does not exist"
        fi

        [[ -f $farch_tmpexp ]] && rm -f $farch_tmpexp
        [[ -f $fsed ]] && rm -f $fsed
    
    done
    return
}


strscr=$( basename "$0" | cut -d'.' -f1 )
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cdir=$SCRIPTPATH

[[ -z $cdir || ! -d $cdir ]] &&  echo "cdir (parent directory of this script) is undefined"  && exit

cd $cdir

source $cdir/func_fcst.sh

optu=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        copy run_gcmarch_beta.sh into dexp/archive and run it in a screen session.
        
        Usage: ./$(basename "$0") [-h] [-u dexp] srcme_file

        -u, --upd-archscr     copy the latest run_gcmarch.sh into exp archive dir **INPUT REQUIRED
"            
file=
verbose=0
cnt=0
while :; do
    case $1 in
         -u|--upd-archscr )  [[ "$2" ]] && arrdexp=( $2 ) && optu=true && shift \
                             || die "opt u requires an argument";;
            -h|-\?|--help )  echo "$usage";exit;;           # Display a usage synopsis.
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
#inputcheck
[[ -n $strf ]] && source $strf

#================================================================================
#                             Set Host Specific Vars
#================================================================================
hst=`hostname`
if [[ $hst == 'pfe'* ]];then
    [[ -n $ctag ]] && dbuild=/nobackupp2/knakada/GEOS_Models/$ctag/GEOSodas/src || dbuild=
elif [[ $hst == 'discover'* ]];then 
    :
fi
#================================================================================
#                                 Set Variables
#================================================================================
#mid
#writetofile=1
#fmessage=message_${strscr}
#[[ -f $fmessage ]] && rm -f $fmessage

#farchscr=run_gcmarch_beta.sh

#================================================================================
#                                  Main Process
#================================================================================

update_archscr ${arrdexp[@]}

exit


