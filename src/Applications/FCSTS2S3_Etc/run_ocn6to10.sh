#!/usr/bin/env bash

inputcheck(){
    (( ${#arrposarg[@]} != 1 )) && die "1 inputs are required"

    #! $optr && ! $optd && ! $optf && die "opt d, r or f are required"

    #$optr && [[ -z $userbegyyyymm || -z $userendyyyymm ]] && die "YYYYMM range is a required input for option r"

    #if $optd;then
    #    #todo:  check user input. it has to < 199601
    #    (( $useryyyymmdd >= 19960101 )) && die "opt d input has to be less than 19960101"
    #fi

    #if $optr; then
    #    local int1=$( misc_isinteger $startyyyymm ) 
    #    local int2=$( misc_isinteger $endyyyymm ) 
    #    (( $int1 > 0 || $int2 > 0 )) && die "YYYYMM range have to be numbers delimited by a hyphen"
    #fi 
    return
}

_write_note_enschange(){

    local _fwin=$1
    local _yyyymm=$( cat $_fwin | head -1 | cut -d' ' -f1 | cut -c1-6 )
    local _blthisheader=false
    local _arrocn=($(      cat $_fwin | sed 's# #/#g' | grep -E "$strensm_ocn"      2>/dev/null )) 
    local _arrocn06t10=($( cat $_fwin | sed 's# #/#g' | grep -E "$strensm_ocn06t10" 2>/dev/null ))
    local _arrocn11t15=($( cat $_fwin | sed 's# #/#g' | grep -E "$strensm_ocn11t15" 2>/dev/null ))
   
    if (( ${#_arrocn[@]} == 0 ));then 
        msg_wheader_userdefined 40 - $_yyyymm && _blthisheader=true
        wmessage "***No Winners from Ocean Perturbations (ens6-15)"
        wmessage "Originally Selected Winners from Ocn Perturbations:"
        ahand_print ${_arrocn[@]} 
    
    elif (( ${#_arrocn[@]} > 0 ));then 
    
        msg_wheader_userdefined 40 - $_yyyymm && _blthisheader=true
    
        wmessage "Originally Selected Winners from Ocn Perturbations:"
        ahand_print ${_arrocn[@]} 
   
        for ocn in ${_arrocn[@]};do
    
            local _fcstdate=$( echo $ocn | cut -d'/' -f1 )
            local     _ensm=$( echo $ocn | cut -d'/' -f2 )
            local _int_ens=$( echo $_ensm | cut -c4- | xargs -i bash -c "echo "'"'"{} * 1"'"'" | bc" )
    
            if (( $_int_ens < 11 )) ;then
    
                local _ensmp5=ens$( echo $_ensm | cut -c4- | xargs -i bash -c "echo "'"'"{} * 1 + 5"'"'" | bc" )
    
                wmessage "$( printf "%-${x1}s %-${x2}s %+${x3}s %+${x4}s" "Winner Run with Corrected Restarts (total 10months) -" "$_fcstdate/$_ensm" )"

                arrrun_10mon+=( "$_fcstdate/$_ensm" )
                cnt_run=$(( cnt_run + 1 ))
    
                printf '%s\' ${_arrocn11t15[@]} | grep $_ensmp5 >>/dev/null  2>&1
                local _status_grep=$?
    
                if (( $_status_grep == 0 ));then 
                    wmessage "$( printf "%-${x1}s %-${x2}s %+${x3}s %+${x4}s" "Winner Completed                                    -" "$_fcstdate/$_ensm" "=>" "$_fcstdate/$_ensmp5" )"
                else
                    wmessage "$( printf "%-${x1}s %-${x2}s %+${x3}s %+${x4}s" "Winner Output (7months) Remove/Hide                 -" "$_fcstdate/$_ensm" "=>" "$_fcstdate/$_ensmp5" )"
                fi 
    
            elif (( $_int_ens > 10 ));then 
                local _ensmm5=ens$( echo $_ensm | cut -c4- | xargs -i bash -c "echo "'"'"{} * 1 - 5"'"'" | bc" )
    
                printf '%s\' ${_arrocn06t10[@]} | grep $_ensmm5 >>/dev/null  2>&1
                local _status_grep=$?
    
                if (( $_status_grep == 0 ));then 
                    #note:  this check was done in if statement above
                    #wmessage \@$LINENO $_ensmm5
                    #wmessage "Winner Completed                                    - $_fcstdate/$_ensmm5 => $_fcstdate/$_ensm"
                    :
                else
                    wmessage "$( printf "%-${x1}s %-${x2}s %+${x3}s %+${x4}s" "Winner Run 4-10 months (total 7months)              -" "$_fcstdate/$_ensmm5" "=>" "$_fcstdate/$_ensm" )"
                    arrrun_07mon+=( "$_fcstdate/$_ensmm5:$_fcstdate/$_ensm" )
                    cnt_run=$(( cnt_run + 1 ))
                fi
            fi

        done
       
        $_blthisheader && wmessage
        $_blthisheader && wmessage "# of Ocean Perturbation to Run : $cnt_run"
        $_blthisheader && wmessage
        $_blthisheader && wmessage "Archive/Clean Status:"
        $_blthisheader && write_note_arccln $_yyyymm
    
    
    fi 

    return

}

write_note_enschange(){

    local _fwin=$1
    local _yyyymm=$( cat $_fwin | head -1 | cut -d' ' -f1 | cut -c1-6 )
    local _yyyy=$( echo $_yyyymm | cut -c1-4 )
    local _blthisheader=false
    local _arrocn=($(      cat $_fwin | sed 's# #/#g' | grep -E "$strensm_ocn"      2>/dev/null )) 
    local _arrocn06t10=($( cat $_fwin | sed 's# #/#g' | grep -E "$strensm_ocn06t10" 2>/dev/null ))
    local _arrocn11t15=($( cat $_fwin | sed 's# #/#g' | grep -E "$strensm_ocn11t15" 2>/dev/null ))
    local _arrocnicyyyymmdd=($( fcal_ocnicyyyymmdd $_yyyy ))  
    local _arrocnicyyyymmdd=($( printf '%s\n' ${_arrocnicyyyymmdd[@]} | grep $_yyyymm 2>/dev/null ))
    local _arrdexp=($( printf "%s\n" ${_arrocnicyyyymmdd[@]} | xargs -i printf "{}/%s\n" ${arrensm_ocn06t10[@]} | sort -V | xargs -i bash -c "[[ -d $DFCST/{} ]] && echo $DFCST/{}" )) 
    local _nummonth1=3
    local _dexp

    local   _strwin_head="winner?"
    local _strmkdir_head="mkdir?"
    local   _strarc_head="arccomp?"
    local   _strrun_head="run?"
    local _strsetup_head="re-setup?"
    local   _strcap_head="cap_restart correct?"
    local    _strmv_head="mv?"
    local   _stract_head="Actions"

    msg_wheader_userdefined 40 - $_yyyymm && _blthisheader=true
    wmessage "Winner File : $( realpath $_fwin )"
    wmessage "Winners:"
    wmessage "$( cat $_fwin | sed 's# #/#g' | xargs -i printf '    %s\n' {} )" 
    wmessage
    wmessage "Winners from Ocn Perturbations:"
    ahand_print ${_arrocn[@]} 

    #todo:  write header        
    wmessage "$( printf "%-${x2}s %+${x3}s %+${x4}s | %+${#_strwin_head}s | %+${#_strmkdir_head}s | %+${#_strarc_head}s | %+${#_strrun_head}s | %+${#_strsetup_head}s | %+${#_strcap_head}s | %+${#_strmv_head}s | %-${#_stract_head}s" \
        "YYYYMMDD/ensm " "  " "             " "$_strwin_head" "$_strmkdir_head" "$_strarc_head" "$_strrun_head" "$_strsetup_head" "$_strcap_head" "$_strmv_head" "$_stract_head" )"
    local _head1=$( echo "$x2 + $x3 + $x4 + ${#_strwin_head} + ${#_strmkdir_head} + ${#_strarc_head} + ${#_strrun_head} + ${#_strsetup_head} + ${#_strcap_head} + ${#_strmv_head} + ${#_stract_head} +50 " | bc )
    wmessage "$( printf -- "-%.0s" $( seq 1 $_head1 ) )"

    #todo:  check ens6-10 with corrected rst
    for _dexp in ${_arrdexp[@]};do
        set_rstfcstdate $_dexp
        local _blwin=$( exp_checkwinner $_dexp   $ddata_pckwinners )

        local _strmkdir="Yes"
        local _strarc="-"
        local _strmv="-"
        local _strsetup="-"
        local _strcap="-"

        if $_blwin;then 
            local _strwin="Yes"
            local _stract="Run 10 months"
            local _strrun="Yes"
            arrrun_10mon+=( $_dexp ) 
        else
            local _strwin="-"
            local _stract="Run 3 months"
            local _strrun="Yes"
            arrrun_03mon+=( $_dexp ) 
        fi
            
        wmessage "$( printf "%-${x2}s %+${x3}s %+${x4}s | %+${#_strwin_head}s | %+${#_strmkdir_head}s | %+${#_strarc_head}s | %+${#_strrun_head}s | %+${#_strsetup_head}s | %+${#_strcap_head}s | %+${#_strmv_head}s | %-${#_stract_head}s" \
                     "$fcstdate/$ensm" "w/" "corrected rst" "$_strwin" "$_strmkdir" "$_strarc" "$_strrun" "$_strsetup" "$_strcap" "$_strmv" "$_stract" )"
    done


    #todo:  check ens6-10 => ens11-15
    for _dexp in ${_arrdexp[@]};do

        set_rstfcstdate $_dexp

        local _ensmp5=ens$( echo $ensm | cut -c4- | xargs -i bash -c "echo "'"'"{} * 1 + 5"'"'" | bc" )
        local _dexpp5=$DFCST/$fcstdate/$_ensmp5
        local   _blwin=$( exp_checkwinner $_dexp   $ddata_pckwinners )
        local _blwinp5=$( exp_checkwinner $_dexpp5 $ddata_pckwinners )
            
        [[ -f $_dexp/$fcln_comp ]] && local _blcln=true || local _blcln=false

        local _strmkdir="-"

        #todo:  check arch status 
        if [[ -d $_dexp/archive && -f $_dexp/archive/$farc_comp ]] || $_blcln ;then 
            local _strarc="Yes"
            local _strmv="Yes"
        elif ! $_blcln && [[ -d $_dexp/archive && ! -f $_dexp/archive/$farc_comp ]];then 
            local _strarc="No"
            local _strmv="ArchFirst" 
        fi

        if $_blwin && $_blwinp5;then
            #"NONE (Winner Completed)"
            local _strwin="Yes"
            local _strsetup="-"
            local _strcap="-"
            local _strrun="-"
            local _stract="NONE (Winner Completed)"
            arrnoaction+=( "$fcstdate/$ensm:$fcstdate/$_ensmp5" ) 

        elif $_blwin && ! $_blwinp5;then
            local _strwin="-"
            local _strsetup="-"
            local _strcap="-"
            local _strrun="-"
            local _stract="Remove/Hide Winner Output (7months)"
            arrmvout+=( "$fcstdate/$ensm:$fcstdate/$_ensmp5" ) 
            
        elif ! $_blwin && $_blwinp5;then 
            local _strwin="Yes"

            if $_blcln && [[ ! -f $_dexp/gcm_run.j ]];then 
                local _strsetup="Yes"
                local _strcap="-"
                local _strrun="Yes"
                local _stract="Run 7 months"
                arrrun_setup07mon+=( "$fcstdate/$ensm:$fcstdate/$_ensmp5" ) 

            elif [[ -f $_dexp/gcm_run.j ]];then 

                #todo check cap_restart
                local strxmonth=$( nextXmonths $( date -d $fcstdate +%Y ) $( printf '%01g' $( date -d $fcstdate +%m ) ) 0 $(( _nummonth1 + 1 ))  )
                local _end_mm=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f1 )
                local _end_year=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f2 )
                local _end3_yyyymmdd=$_end_year$_end_mm"01"
                local _capr_yyyymmdd=$( cat $_dexp/cap_restart | cut -d' ' -f1 ) 

                (( $_end3_yyyymmdd == $_capr_yyyymmdd )) && local _blcaprready=true || local _blcaprready=false

                if $_blcaprready;then 
                    local _strsetup="-"
                    local _strcap="Yes"
                    local _strrun="Yes"
                    local _stract="Run 7 months"
                    arrrun_07mon+=( "$fcstdate/$ensm:$fcstdate/$_ensmp5" ) 
                else
                    local _strsetup="-"
                    local _strcap="No"
                    local _strrun="-"
                    local _stract="***Something wrong with cap_restart***"
                    arr_issuedexp+=( "$fcstdate/$ensm:$fcstdate/$_ensmp5" ) 
                fi
            fi

        elif ! $_blwin && ! $_blwinp5;then 
            local _strwin="-"
            local _strsetup="-"
            local _strcap="-"
            local _strrun="-"
            local _stract="NONE (3-month Run Completed)"
            arrnoaction+=( "$fcstdate/$ensm:$fcstdate/$_ensmp5" ) 
        else
            $_blwinp5 && local _strwin="Yes" || local _strwin="-"
            local _strsetup="-"
            local _strcap="-"
            local _strrun="-"
            local _stract="***Something wrong with exp"
            arr_issuedexp+=( "$fcstdate/$ensm:$fcstdate/$_ensmp5" ) 
        fi

        #wmessage "$( printf "%-${x2}s %+${x3}s %+${x4}s | %+${x9}s | %+${x7}s | %+${x10}s | %+${x7}s | %+${x8}s | %+${x7}s | %-${x6}s" \
        wmessage "$( printf "%-${x2}s %+${x3}s %+${x4}s | %+${#_strwin_head}s | %+${#_strmkdir_head}s | %+${#_strarc_head}s | %+${#_strrun_head}s | %+${#_strsetup_head}s | %+${#_strcap_head}s | %+${#_strmv_head}s | %-${#_stract_head}s" \
                     "$fcstdate/$ensm" "=>" "$fcstdate/$_ensmp5" "$_strwin" "$_strmkdir" "$_strarc" "$_strrun" "$_strsetup" "$_strcap" "$_strmv" "$_stract" )"
            
    done 

    return
}

write_summary(){

    msg_wheader_userdefined 40 - "Summary"
    local _tot_ocn=$( echo "${#arrnoaction[@]} + ${#arr_issuedexp[@]} + ${#arrrun_03mon[@]} + ${#arrrun_setup07mon[@]} + ${#arrrun_07mon[@]} + ${#arrrun_10mon[@]} + ${#arrmvout[@]}" | bc ) 
    local _tot_ocn_calc=120
    (( $_tot_ocn != $_tot_ocn_calc )) && wmessage "check for ocn pert exp is not completed ( $_tot_ocn, $_tot_ocn_calc ) "
    wmessage    
    wmessage "Number of Exp Requires No Actions   : ${#arrnoaction[@]}"
    wmessage "Number of Exp with Unknown Problems : ${#arr_issuedexp[@]}"
    wmessage
    #wmessage "# of Ocean Pert Ensemble to run :"
    #wmessage "$( printf '%s\n' "${arrnumexp[@]}" )"
    #wmessage " TOTAL : $( printf "%+${x3}s" $tot_run )"
    #wmessage
    wmessage "Move/Hide Winner outputs (7months) (total - ${#arrmvout[@]}) :"
    for mvout in ${arrmvout[@]};do 
        local ensm_bf=$( echo $mvout | cut -d':' -f1 )
        local ensm_af=$( echo $mvout | cut -d':' -f2 )
        wmessage "$( printf "    %-${x2}s %+${x3}s %+${x4}s" $ensm_bf "=>" $ensm_af )"
    done 
    wmessage
    wmessage "Setup & Run for 7months (total - ${#arrrun_setup07mon[@]}) :"
    for run07mon in ${arrrun_setup07mon[@]};do 
        local ensm_bf=$( echo $run07mon | cut -d':' -f1 )
        local ensm_af=$( echo $run07mon | cut -d':' -f2 )
        wmessage "$( printf "    %-${x2}s %+${x3}s %+${x4}s" $ensm_bf "=>" $ensm_af )"
    done 
    wmessage
    wmessage "Run for 7months (total - ${#arrrun_07mon[@]}) :"
    for run07mon in ${arrrun_07mon[@]};do 
        local ensm_bf=$( echo $run07mon | cut -d':' -f1 )
        local ensm_af=$( echo $run07mon | cut -d':' -f2 )
        wmessage "$( printf "    %-${x2}s %+${x3}s %+${x4}s" $ensm_bf "=>" $ensm_af )"
    done 
    wmessage
    wmessage "Run for  3 months (total - ${#arrrun_03mon[@]}) :"
    wmessage "$( printf "    %-${x2}s\n" ${arrrun_03mon[@]} )"
    wmessage
    wmessage "Run for 10 months (total - ${#arrrun_10mon[@]}) :"
    wmessage "$( printf "    %-${x2}s\n" ${arrrun_10mon[@]} )"
    wmessage

    return
}

write_instruction(){
    local strinstruction="

1. Run experiments that have to run for another 7 months due to becoming a winner 
   automatically.
   ***Make sure to run them first before renaming their dir to ens11-ens15
2. Complete archive outputs for the experiments from #1.
3. Then, rename their dir on *BOTH* PFE and LFE. 
4. Remove/Hide winner outputs (7months) from experiment dir on *BOTH* PFE & LFE 

"    
}

write_note_arccln(){
    #description:   write archive/clean status for each ocnyyyymmdd/ens*

    local _yyyymm=$1
    local _yyyy=$( echo $_yyyymm | cut -c1-4 )

    local _arrocnicyyyymmdd=($( fcal_ocnicyyyymmdd $_yyyy ))  
    local _arrocnicyyyymmdd=($( printf '%s\n' ${_arrocnicyyyymmdd[@]} | grep $_yyyymm 2>/dev/null ))
    #local _arrocndexp=($( printf "%s\n" ${_arrocnicyyyymmdd[@]} | xargs -i printf "{}/%s\n" ${arrensm_ocn[@]} | sort -V )) 
    local _arrocndexp=($( printf "%s\n" ${_arrocnicyyyymmdd[@]} | xargs -i printf "{}/%s\n" ${arrensm_ocn06t10[@]} | sort -V | xargs -i bash -c "[[ -d $DFCST/{} ]] && echo {}" )) 
    local _arrocndexp_arcdone=($( printf '%s\n' ${_arrocndexp[@]} | xargs -i bash -c "[[ -d $DFCST/{}/archive && -f $DFCST/{}/archive/$farc_comp ]] && echo {}" ))  
    local _arrocndexp_clndone=($( printf '%s\n' ${_arrocndexp[@]} | xargs -i bash -c "[[ -f $DFCST/{}/$fcln_comp ]] && echo {}" ))  
    local _arrocndexp_therest=($( printf '%s\n' ${_arrocndexp[@]} ${_arrocndexp_arcdone[@]} ${_arrocndexp_clndone[@]} |sort -V | uniq -u ))
    local _ocnicyyyymmdd _ensm_ocn

    for _ocnicyyyymmdd in ${_arrocnicyyyymmdd[@]};do

        local _ocnicyyyymm=($( printf '%s\n' ${_arrocndexp[@]} | grep $_ocnicyyyymmdd 2>/dev/null | cut -c1-6 | sort -V | uniq ))
        [[ -z $_ocnicyyyymm ]] && continue 

        for _ensm_ocn in ${arrensm_ocn[@]};do
            local _icyyyymmdd_ensm=$_ocnicyyyymmdd/$_ensm_ocn
            
            printf '%s\n' ${_arrocndexp_arcdone[@]} | grep $_icyyyymmdd_ensm >/dev/null 2>&1
            local _status_arc=$?

            printf '%s\n' ${_arrocndexp_clndone[@]} | grep $_icyyyymmdd_ensm > /dev/null 2>&1
            local _status_cln=$?

            printf '%s\n' ${_arrocndexp_therest[@]} | grep $_icyyyymmdd_ensm > /dev/null 2>&1
            local _status_rest=$?

            if (( $_status_arc ==  0 && $_status_cln > 0  && $_status_rest > 0 ));then
                printf "%-${x5}s %-${x2}s\n" "ArcDone-Ready to Move" $_icyyyymmdd_ensm 

            elif (( $_status_arc > 0 && $_status_cln == 0 && $_status_rest > 0 ));then
                printf "%-${x5}s %-${x2}s\n" "ClnDone-Ready to Move" $_icyyyymmdd_ensm 

            elif (( $_status_arc > 0 && $_status_cln > 0  && $_status_rest == 0 ));then
                if [[ -f $DFCST/$_icyyyymmdd_ensm/gcm_run.j ]];then
                    printf "%-${x5}s %-${x2}s\n" "gcm_run.j Exist" $_icyyyymmdd_ensm 
                else
                    printf "%-${x5}s %-${x2}s\n" "UNKNOWN" $_icyyyymmdd_ensm 
                fi
            fi
        done 
    done 
    #printf '%s\n' ${arrocndexp_arcdonestatus[@]} ${arrocndexp_clndonestatus[@]} ${arrocndexp_thereststatus[@]} | sort -V | sed "s#:#    #g" 

#wmessage \@$LINENO ${#arrocndexp[@]} ${#arrocndexp_arcdone[@]} ${#arrocndexp_clndone[@]} ${#arrocndexp_therest[@]} 
    return

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

#note:  this script is only for hindcast yyyyymmdd < 19960101
yyyymmdd_thres=19960101
writetofile=0
rundebug=0
optf=false
opty=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        print notes about re-running of ocean perturbation. 
        Year and months that needs to be re-processed are:
            198112, 1982-1984, 199112, 1992-1994, 199501-199503, 199505-199508.

        Usage: ./$(basename "$0") [-chw] [ -y YYYY ] [-f data file] srcme_file 

        Input:
            Source file, which set various vars for a set of runs.

        options:
            -y  YYYY
            -f  data file with a list of YYYYMMDD
            -c  clean unwanted files
            -h  show this help text
            -w  write stdout/err in a file

"

OPTIND=1
while getopts ':hwcf:y:' option; do
    case "$option" in
        f)  optf=true; fdata=$OPTARG;;
        y)  opty=true; 
            useryyyy=$( echo $OPTARG | cut -d'-' -f1 );;
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
inputcheck
i=0
srcf=${arrposarg[$i]};i=$(( i + 1 ))
source $srcf

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

dmess=$cdir/message
fmessage_base=$dmess/message_$strscr

#dicyyyymmdd=$DFCST/$icyyyymmdd
#darch_icyyyymmdd=$DARCH/$icyyyymmdd
#ens6  => ens11
#ens7  => ens12
#ens8  => ens13
#ens9  => ens14
#ens10 => ens15

bl_writeinstruction=false

dateprocessed="2022-06-18"
#ddata=$FUTIL/output/pckwinners/pre_ocn6t10_fcst
ddata_pckwinners=$FUTIL/output/pckwinners/pre_ocn6t10_fcst
strfwin_base=winners_nino3.4_${strexpid}_

farc_comp=gcmarch_deloutcompleted
fcln_comp=clean_completed

flistlfe=wftmsz_flist
flst_lfe=$cdir/${strscr}_lfe_$flistlfe

arrensm_ocn=$( printf "ens%s " $( seq 6 15 ) )
arrensm_ocn06t10=$( printf "ens%s " $( seq 6  10 ) )
     strensm_ocn=$( printf "%s " ${arrensm_ocn[@]} | sed 's#[ \s]*$##g' | sed 's# #|#g' )
strensm_ocn11t15=$( printf "ens%s " $( seq 11 15 ) | sed 's#[ \s]*$##g' | sed 's# #|#g' )
strensm_ocn06t10=$( printf "ens%s " $( seq 6  10 ) | sed 's#[ \s]*$##g' | sed 's# #|#g' )

#note;  printf spaces 
x1=55;x2=14;x3=3;x4=14;x5=21;x6=42;x7=10;x8=25;x9=6;x10=4

msg_subject="${thishst}.${strscr}: $strexpid"

feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

[[ ! -d $dmess ]] && mkdir -p $dmess
[[ ! -d $DRST  ]] && mkdir -p $DRST
#================================================================================
#                                  Main Process
#================================================================================
#main
#note:
#1. figure out winners between ens6 and 10 => these become ens11-15 and no need to run as winners
#2. check archive status 
#3. check clean status 
#4. 

if $opty && [[ -n $useryyyy ]];then 
    arryyyy=($( find $ddata_pckwinners/* -type f -name "${strfwin_base}${useryyyy}??_*" -not -newermt "$dateprocessed" | xargs -i basename {} | cut -d'_' -f4 | cut -c1-4 | sort -V | uniq  | grep $useryyyy ))
else
    arryyyy=($( find $ddata_pckwinners/* -type f -name "${strfwin_base}[1-2]?????_*" -not -newermt "$dateprocessed" | xargs -i basename {} | cut -d'_' -f4 | cut -c1-4 | sort -V | uniq  ))
fi

#thisarr=( ${arryyyy[@]} ) 
#ahand_print ${thisarr[@]} 
#exit

for yyyy in ${arryyyy[@]};do 
    cdate=$( date +%Y%m%d%H%M )
    fmessage=${fmessage_base}_${yyyy}_$cdate
    (( $writetofile == 1 )) && msg_newfile $fmessage

    arrfwin=($( find $ddata_pckwinners/* -type f -name "${strfwin_base}${yyyy}??_*" -not -newermt "$dateprocessed" | sort -V  ))
    arrnumexp=() 
                
    arrmvout=() 
    arrrun_03mon=() 
    arrrun_setup07mon=() 
    arrrun_07mon=() 
    arrrun_10mon=() 
    tot_run=0

    msg_wheader $yyyy
    wmessage "$( date +'%Y%m%d %H:%M' )"
    
    
    #todo:  find old/original ens6-10 => ens11-15 that winner runs are completed
    for fwin in ${arrfwin[@]};do
        cnt_run=0
        yyyymm=$( cat $fwin | head -1 | cut -d' ' -f1 | cut -c1-6 )
        
        write_note_enschange $fwin
        wmessage

        tot_run=$(( tot_run + cnt_run ))
        arrnumexp+=( "$yyyymm : $( printf '%+3s' $cnt_run )" )
    done 

    ! $bl_writeinstruction && write_instruction && bl_writeinstruction=true
    write_summary 

#ahand_print ${arrocndexp_arcdone[@]} 
#wmessage
#ahand_print ${arrocndexp_clndone[@]} 
    
done 




exit


