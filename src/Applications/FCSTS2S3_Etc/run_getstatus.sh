#!/usr/bin/env bash

PS4='$LINENO: '

inputcheck() {

    [[   -z $srcf ]] && die "source file is a required input"
    [[ ! -f $srcf ]] && die "$srcf file does not exist" 
    (( ${#arrposarg[@]} != 1 )) && die "only one input is required"

    $optr && [[ -z $userbegyyyymm || -z $userendyyyymm ]] && die "YYYYMM range is a required input for option r"

    return
}

mksummary() {
    #description:   count each status and create a summary table 
    local _arr=( "$@" )
    local strstatus=$( echo ${arrstatus_done[@]} | sed 's# #\|#g' )
    local _arryyyymmdone=()
    local _arryyyymmnotdone=()
    local _arrflst=()
    local yyyymm

    _arrflst=($( printf "$dout/${fdatalst_base}%s\n" ${_arr[@]} | xargs -i bash -c "[[ -f {} ]] && echo {} " ))

    #local   numftbl=$( find $dout/* -maxdepth 0 -type f -name "$fdatatbl_base*" 2>/dev/null | wc -l )

    [[ -f $ftmp1 ]] && rm -f $ftmp1

    #todo:  figure out which YYYY or YYYYMM are completed with C3/C3m/C10 status
    for _flst in ${_arrflst[@]};do
        local _yyyymm=$( echo $_flst | rev | cut -d'_' -f1 | rev )
        local _numincomplete=$( cat $_flst | cut -d' ' -f1 | sort -V | uniq | grep -vE "$strstatus" 2>/dev/null | wc -l ) 
        (( $_numincomplete == 0 )) && _arryyyymmdone+=( $_yyyymm ) || _arryyyymmnotdone+=( $_yyyymm )
    done

    #todo:  add the rest of status
    if (( ${#_arrflst[@]} > 0 ));then
        local _arrstatus=($( cat ${_arrflst[@]} | cut -d' ' -f1 | grep -vE "$strstatus" | sort -V | uniq ))
    else
        local _arrstatus=()
    fi

    #todo:  add the rest of status
    local title="YYYYMM;Total;$( echo ${_arrstatus[@]} | tr ' ' '\;' );others;$( echo ${arrstatus_done[@]} | tr ' ' '\;' );sumcomp;"
    echo "$title" >> $ftmp1

    local _blwstatus=false
    for _flst in ${_arrflst[@]};do
        local yyyymm=$( echo $_flst | rev | cut -d'_' -f1 | rev )

        if $optf;then 
            local numexp=$( cat $_flst | wc -l ) 
        else
            local arricyyyymmdd=($( fcal_calcfcdates $yyyymm ))
            local numexp=$( echo "${#arricyyyymmdd[@]} * 5 +10" | bc )
        fi

        #todo:  check "done" status (i.e. C3, C3m, WC10)
        local strstatus_done=
        local totcnt=0
        local cnt_done=0
        for _status in ${arrstatus_done[@]};do
            local cntstatus=$( grep -w -c $_status $_flst )
            (( $cntstatus == 0 )) && local thisstatus="--" || local thisstatus=$cntstatus

            strstatus_done="${strstatus_done}${thisstatus};"
            totcnt=$(( totcnt + cntstatus ))
            cnt_done=$(( cnt_done + $cntstatus ))
        done
        

        #todo:  check non-completed status (i.e. RdyS,R,etc.)
        strstatus=
        for _status in ${_arrstatus[@]};do
            local cntstatus=$( grep -w -c $_status $_flst )
            (( $cntstatus == 0 )) && local thisstatus="--" || local thisstatus=$cntstatus

            strstatus="${strstatus}${thisstatus};"
            totcnt=$(( totcnt + cntstatus ))
        done

        #todo:  get 'completed' YYYYMM in array
        if (( $cnt_done != $numexp ));then
            #todo:  calc unknown status & write summary for each YYYYMM
            local totrest=$(( numexp - $totcnt ))
            (( $totrest == 0 )) && local thisstatus="--;" || local thisstatus="${totrest};"

            echo $yyyymm\;$numexp\;${strstatus}${thisstatus}${strstatus_done}${cnt_done}\; >> $ftmp1
            _blwstatus=true
        fi
    done 

    #todo:  check if all months completed for yyyy
    local _arryyyy=($( printf '%s\n' ${_arryyyymmdone[@]} | sort -V | cut -c1-4 | uniq ))
    local _arryyyydone=()
    local _arryyyymmdone_toprint=()
    for yyyy in ${_arryyyy[@]};do
        local numyyyy=$( printf '%s\n' ${_arryyyymmdone[@]} | grep $yyyy 2>/dev/null | wc -l )

        if (( $yyyy == 1981 )) && (( $numyyyy == 1 ));then
            _arryyyydone+=( $yyyy )
        elif (( $numyyyy == 12 ));then
            _arryyyydone+=( $yyyy )
        else
            _arryyyymmdone_toprint+=($( printf '%s\n' ${_arryyyymmdone[@]} | grep -v 198112 | grep $yyyy | sort -V  | uniq ))
        fi
    done

    #todo:  write completd YYYYMM & YYYY
    if (( ${#_arryyyydone[@]} > 0 ));then 
        wmessage "YYYY Completed:"
        wmessage "$( printf '%s\n' ${_arryyyydone[@]} | column -x )"
    fi
    if (( ${#_arryyyymmdone_toprint[@]} > 0 ));then 
        wmessage
        wmessage "YYYYMM Completed:"
        wmessage "$( printf '%s\n' ${_arryyyymmdone_toprint[@]} | column -x )"
    fi

    if $_blwstatus;then 
        wmessage 
        #echo "" >> $fmessage
        wmessage "$( rev $ftmp1 | column -t -s';' | rev )"
    fi

    #todo:  cleaning
    [[ -f $ftmp1 ]] && rm -f $ftmp1

    return
}

mktbl_dates(){

    local   title=" YYYYMMDD/ENSM ;Status;AGCMDate;EndDate "
    local divider="===============;========;==========;=========="
    local arrdexp=($( grep -w -E "WR|R" $dout/${fdatalst_base}* | cut -d' ' -f2 ))
    local arrdexp=($( printf '%s\n' ${arrdexp[@]} | xargs -i bash -c "[[ -d {} ]] && echo {}" | sort -V | uniq )) 
    local dexp

    msg_newfile $ftmp3

    echo "$title"   >> $ftmp3 
    echo "$divider" >> $ftmp3 
    
    for dexp in ${arrdexp[@]};do
        local cur_date="$( exp_cdate $dexp )"
        local strstatus=$( grep -w $dexp $dout/${fdatalst_base}* 2>/dev/null | cut -d' ' -f1 | cut -d':' -f2 ) 
        local yyyymmdd=$( echo $cur_date | cut -d' ' -f1-5 | cut -d' ' -f3 | sed "s#/##g") 
    
        if [[ "$strstatus" == "R" ]];then 
            local blwin=false
        elif [[ "$strstatus" == "WR" ]];then 
            local blwin=true
        fi
    
        if $blwin;then 
            local capend=$( cat $dexp/cap_end | cut -d' ' -f1 ) 
        else
            local capend=$( grep -i end_date $dexp/CAP_1.rc | tr -s '[:space:]'| cut -d' ' -f2 ) 
        fi
    
        echo "${dexp#*$dparent/};$strstatus;$yyyymmdd;$capend" >> $ftmp3
            
    done
        
    wmessage "$( cat $ftmp3 | column -t -s';' )"

    [[ -f $ftmp3 ]] && rm -f $ftmp3
    return
}


check_yyyymm(){
    
    #description:   check YYYYMM that completed experiments and all archives. 
    local _blusecln=$1;shift
    local _arryyyymm=( "$@" )
    local _arryyyymm_out=()
    local _yyyymm
    local _arryyyymm_fexist=($( printf "$dout/$fdatalst_base%s\n" ${_arryyyymm[@]} | xargs -i bash -c '[[ -f {} ]] && echo {}' ))
    local _arryyyymm_out=($(    printf "$dout/$fdatalst_base%s\n" ${_arryyyymm[@]} | xargs -i bash -c '[[ -f {} ]] && : || echo {}' | rev | cut -d'_' -f1 | rev  ))

    if $_blusecln;then  
        local strstatus=$( echo ${arrstatus_clean[@]} | sed 's# #\|#g' )
    else
        local strstatus=$( echo ${arrstatus_done[@]} | sed 's# #\|#g' )
    fi

    for _flst in ${_arryyyymm_fexist[@]};do
        local _yyyymm=$( echo $_flst | rev | cut -d'_' -f1 | rev )

#        if $_blusecln;then 
#            #note:  when clean is an indicator for completion of experiments, 
#            #       use clean_completed markers as indicators instead of data list from this script
#            cd $DFCST
#            local _arr_yyyymmddensm=($( find * -maxdepth 1 -mindepth 1 -path "${_yyyymm}*" -type d -name 'ens*' ))
#            cd - >/dev/null 
#
#            local _arrdexp=($( printf "$DFCST/%s\n" ${_arr_yyyymmddensm[@]} | sort -V )) 
#
#            local _numdexpuncleaned=$( printf '%s\n' ${_arrdexp[@]} | xargs -i bash -c "[[ ! -f {}/clean_completed ]] && echo {}" | wc -l )
#            local _numthis=$_numdexpuncleaned
#wmessage \@$LINENO $_yyyymm $_numdexpuncleaned ${#_arrdexp[@]}
#        else
#            local _arrstatus=($( cat $_flst | cut -d' ' -f1 | sort | uniq | grep -vE "${strstatus}" ))
#            local _numthis=${#_arrstatus[@]} 
#        fi
         local _arrstatus=($( cat $_flst | cut -d' ' -f1 | sort | uniq | grep -vE "${strstatus}" ))
         local _numthis=${#_arrstatus[@]} 
#wmessage \@$LINENO $_yyyymm $_numthis ${#_arrdexp[@]}
            
        local numwc10=($( grep -w -E 'WC10|WC10C' $_flst | wc -l ))

        if (( $_numthis > 0 ));then
            _arryyyymm_out+=( $_yyyymm )
        elif (( $_numthis == 0 && $numwc10 != 10 )) ;then
            _arryyyymm_out+=( $_yyyymm )
        fi
    done

    _arryyyymm_out=($( printf '%s\n' ${_arryyyymm_out[@]} | sort -V ))
   
    echo "${_arryyyymm_out[@]}"

}

mklst_dexp(){
    local _arrdexp=( "$@" ) 
    local arricyyyymmdd=()
    local _yyyymm icyyyymmdd dexp 

    local _arryyyymm=($( printf '%s\n' ${_arrdexp[@]}| rev | cut -d'/' -f2 | rev | cut -c1-6 | sort | uniq | sort -V )) 

    #todo:  get icdates to process
    for _yyyymm in ${_arryyyymm[@]};do
        local _fthisdatalst=$fdatalst$_yyyymm
        local _arrdexp_yyyymm=($( printf '%s\n' ${_arrdexp[@]} | grep $_yyyymm | sort -V )) 
        local arrdexp_notexist=($( printf '%s\n' ${_arrdexp_yyyymm[@]} | xargs -i bash -c '[[ ! -d {} ]] && echo {}' ))

        [[ -f $_fthisdatalst ]] && rm -f $_fthisdatalst

        #mklst $_fthisdatalst ${_arrdexp_yyyymm[@]} 
        exp_status_mklst $_fthisdatalst ${_arrdexp_yyyymm[@]} 
    done
    
    #todo:  save data list files in output dir.
    local _arrflist=($( find $cdir/* -maxdepth 0 -name "$fdatalst_base[0-9]?????" 2>/dev/null | sort -V )) 
    (( ${#_arrflist[@]} > 0 )) && mv ${_arrflist[@]} $dout/

    return 
}


mklst_yyyymm(){
    local _arr=( "$@" ) 
    local arricyyyymmdd=()
    local _yyyymm icyyyymmdd dexp 
    local _blusecln=true
    local _scr_base=${strscr}_${strexpid}_${FUNCNAME[0]}

    local arryyyymmfinal=($( check_yyyymm $_blusecln "${_arr[@]}" ))

#wmessage \@$LINENO ${arryyyymmfinal[-1]}
#ahand_print ${arryyyymmfinal[@]} 
#exit
    #todo:  get icdates to process
    for _yyyymm in ${arryyyymmfinal[@]};do
        
        #todo:  count number of screen
        local numscr=$( screen -ls | grep -i detached | grep ${_scr_base}_ | wc -l )

        #todo:  count number of processes
        local numproc=$( ps aux | awk '{ print $1 }' | grep -w $USER | wc -l )

        local blexecuted=false
        local blinitialnote=false
        local totsec=0
        local totmin=0

        local sec0=$( date +%s )
        local sec1=$sec0

        while ! $blexecuted;do 
            if (( $numproc < $numproc_maxhere && $numscr < $limscr && $totsec <= $maxsec ));then 

                totsec=0
                blexecuted=true

                (( $writetofile == 0 )) && wmessage "$( printf "%+3s of %+3s\n" $cnt ${#arryyyymmfinal[@]} ) $_yyyymm    "

                local _fthisdatalst=$fdatalst$_yyyymm
                local _fthisdatalst_prev=$dout/$( basename $_fthisdatalst )
                local arricyyyymmdd=($( fcal_calcfcdates $_yyyymm ))
                local _arrdexp=($( printf '%s\n' ${arricyyyymmdd[@]}  | xargs -i printf "$DFCST/{}/%s\n" ${arrens4[@]}  ))
                     _arrdexp+=($( printf '%s\n' ${arricyyyymmdd[-1]} | xargs -i printf "$DFCST/{}/%s\n" ${arrens10[@]} ))
                local arrdexp_notexist=($( printf '%s\n' ${_arrdexp[@]} | xargs -i bash -c '[[ ! -d {} ]] && echo {}' ))

                [[ -f $_fthisdatalst ]] && rm -f $_fthisdatalst

                #(( ${#arrdexp_notexist[@]} > 0 )) && continue

                #mklst $_fthisdatalst ${_arrdexp[@]} 

                local _scr=${_scr_base}_${_yyyymm}.sh
                local _ferr=stderr_${_scr_base}_${_yyyymm}

                [[ -f $_scr  ]] && rm -f $_scr
                [[ -f $_ferr ]] && msg_newfile $_ferr

cat > $_scr << EOF
cdir=$cdir

cd $cdir
source $cdir/func_fcst.sh
source $srcf

exp_status_mklst $_fthisdatalst ${_arrdexp[@]}
EOF
                chmod 755 $_scr
                screen -dmS ${_scr_base}_${_yyyymm} bash -c "./$_scr >> $_ferr 2>&1"

            else
                sec2=$( date +%s )
                sec_diff=$(( sec2 - sec1 ))
                totsec=$(( sec2 - sec0 ))
                totmin=$( echo "$totsec / 60" | bc )

                #todo:  break out for both for loop (hence, 2)
                (( $totsec >= $maxsec )) && break 2 

                if ! $blinitialnote;then 
                    #wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screens are running and $( printf '%+2s\n' $numproc ) processes are running ... will wait for max of $maxmin minutes    "
                    blinitialnote=true
                elif (( $sec_diff > 60 ));then
                    (( $writetofile == 0 )) && wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screens are running and $( printf '%+2s\n' $numproc ) processes are running ... waited for $totmin min ( max = $maxmin )"
                    sec1=$( date +%s )
                fi
            fi
            
            local numscr=$( screen -ls | grep -i detached | grep ${_scr_base}_ | wc -l )
            local numproc=$( ps aux | awk '{ print $1 }' | grep -w $USER | wc -l )
        done
    done

    local numscr=$( screen -ls | grep -i detached | grep $_scr_base | wc -l ) 

    while (( $numscr > 0 )); do 
        sleep 10s
        numscr=$( screen -ls | grep $_scr_base | wc -l ) 
    done

    find * -maxdepth 0 -type f -name "stderr_${_scr_base}*" -empty -delete 2>/dev/null 
    find * -maxdepth 0 -type f -name "${_scr_base}_*.sh"           -delete 2>/dev/null 

    #todo:  save data list files in output dir.
    local _arrflist=($( find $cdir/* -maxdepth 0 -name "$fdatalst_base[0-9]?????" 2>/dev/null | sort -V )) 
    (( ${#_arrflist[@]} > 0 )) && mv ${_arrflist[@]} $dout/

    return 
}

mklst(){
    local _flst=$1;shift
    local _arr=( "$@" )

    for dexp in ${_arr[@]};do
        local str_ens=""
        [[ ! -d $dexp ]] && continue 

        #todo:  get status 
        local strstatus=$( exp_status $dexp )
        if [[ -n $strstatus ]];then

            str_ens="${str_ens}${strstatus};"

            #todo:  write status to a separate data file
            #note:  this will be used for other scripts (i.e. monitor)
            echo "$strstatus $dexp" >> $_flst
        else
            :
        fi
    done

}

mktbl_dexp(){

    local _arrdexp=( "$@" ) 
    local _lastensmind=$( echo "${#arrensm[@]} - 1" | bc )
    local lastensm=$( echo ${arrensm[-1]} | cut -c4- )
    local _divider_ens="=====;"
    local _divider="========;"
    local _arrfdatayyyymm=()
    local _blusecln=false
    local _yyyymm dexp yyyymmfinal
    local ftmpyyyymm int
    
    #todo:  create a title
    local _title="YYYYMMDD;"
    for intens in ${arrintens[@]};do
        _title="${_title}ens$intens;"
    done

    #todo:  create a divider based on number of ensembles
    for int in $( seq 1 ${#arrintens[@]} );do
        _divider=${_divider}${_divider_ens}
    done

    [[ -f $ftmp2 ]] && rm -f $ftmp2

    echo "" >> $ftmp2
    echo "$_title" >> $ftmp2
    echo "$_divider" >> $ftmp2

    local arryyyymmfinal=($( check_yyyymm $_blusecln "${_arryyyymm[@]}" ))
#wmessage \@$LINENO
#ahand_print ${arryyyymmfinal[@]} 
#exit
    #todo:  get icdates to process
    for _yyyymm in ${arryyyymmfinal[@]};do
        local _arrdexp_yyyymm=($( printf '%s\n' ${_arrdexp[@]} | grep $_yyyymm | sort -V )) 
        local arrdexp_notexist=($( printf '%s\n' ${_arrdexp_yyyymm[@]} | xargs -i bash -c '[[ ! -d {} ]] && echo {}' ))

        local stryyyymmdd=""
        local _fthisdatatbl=$fdatatbl$_yyyymm
        local _fthisdatalst=$( find $dout/* -type f -name "$fdatalst_base$_yyyymm" 2>/dev/null )
        local str_ens=""

        [[ -f $_fthisdatatbl ]] && rm -f $_fthisdatatbl
        [[ ! -f $_fthisdatalst ]] && continue 

        for dexp in ${_arrdexp_yyyymm[@]};do
            local icyyyymmdd=$( echo ${dexp#$DFCST/} | cut -d'/' -f1 )
            local     intens=$( echo ${dexp#$DFCST/} | cut -d'/' -f2 | cut -c4- )

            #todo:  get status 
            if [[ ! -d $dexp ]];then
                local strstatus="--"
            else
                local strstatus=$( grep -w $dexp $_fthisdatalst 2>/dev/null | cut -d' ' -f1 | xargs )
            fi
                
            str_ens="${str_ens}${strstatus};"

            #todo:  write string to a file if this is the last ensm (15)
            if (( $intens == $lastensm ));then
                #str_ens="${str_ens};"
                stryyyymmdd="$icyyyymmdd;$str_ens"
                echo "$stryyyymmdd" >> $_fthisdatatbl
                str_ens=""
            fi 
        done
    done

    #todo:  write output with divider
    for yyyymmfinal in ${arryyyymmfinal[@]};do
        _arrfdatayyyymm+=($( find $cdir/* -maxdepth 0 -name "$fdatatbl_base$yyyymmfinal" 2>/dev/null )) 
    done

    _arrfdatayyyymm=($( printf '%s\n' ${_arrfdatayyyymm[@]} | sort -V ))

    for ftmpyyyymm in ${_arrfdatayyyymm[@]};do
        cat $ftmpyyyymm >> $ftmp2
        [[ "$ftmpyyyymm" != "${_arrfdatayyyymm[-1]}" ]] && echo $_divider >> $ftmp2
    done

    if (( $writetofile == 1 ));then 
        rev $ftmp2 | column -t -s';' | rev  >> $fmessage
    else
        rev $ftmp2 | column -t -s';' | rev  #>> $fmessage
    fi
    [[ -f $ftmp2 ]] && rm -f $ftmp2
    
    (( ${#_arrfdatayyyymm[@]} > 0 )) && mv ${_arrfdatayyyymm[@]} $dout/

    return 
}


mktbl(){
    local _blyyyymm=$1;shift
    local _arr=( "$@" ) 
    local lastensm=$( echo ${arrensm[-1]} | cut -c4- )
    local _blusecln=false
    local _divider_ens="=====;"
    local _divider="========;"
    local _arrfdatayyyymm=()
    local _yyyymm dexp yyyymmfinal
    local ftmpyyyymm int
    
    #todo:  create a title
    local _title="YYYYMMDD;"
    for intens in ${arrintens[@]};do
        _title="${_title}ens$intens;"
    done

    #todo:  create a divider based on number of ensembles
    for int in $( seq 1 ${#arrintens[@]} );do
        _divider=${_divider}${_divider_ens}
    done

    [[ -f $ftmp2 ]] && rm -f $ftmp2

    echo "" >> $ftmp2
    echo "$_title" >> $ftmp2
    echo "$_divider" >> $ftmp2

    if $_blyyyymm;then 
        local _arryyyymm=( ${_arr[@]} )
    else
        local _arrdexp=( ${_arr[@]} )
        local _arryyyymm=($( printf '%s\n' ${_arrdexp[@]} | rev | cut -d'/' -f2 | rev | cut -c1-6 | sort -V | uniq )) 
    fi
    
    local arryyyymmfinal=($( check_yyyymm $_blusecln "${_arryyyymm[@]}" ))

#wmessage \@$LINENO
#wmessage lineno = $LINENO 
#ahand_warr ${_arryyyymm[@]}
#wmessage
#ahand_warr ${arryyyymmfinal[@]}
#exit

    #todo:  get icdates to process
    for _yyyymm in ${arryyyymmfinal[@]};do
        if $_blyyyymm;then 
            local arricyyyymmdd=($( fcal_calcfcdates $_yyyymm ))
            local _arrdexp_yyyymm=($( printf '%s\n' ${arricyyyymmdd[@]} | xargs -i printf "$DFCST/{}/%s\n" ${arrensm[@]} ))
        else
            local _arrdexp_yyyymm=($( printf '%s\n' ${_arrdexp[@]} | grep $_yyyymm | sort -V )) 
        fi
        local arrdexp_notexist=($( printf '%s\n' ${_arrdexp_yyyymm[@]} | xargs -i bash -c '[[ ! -d {} ]] && echo {}' ))

        local stryyyymmdd=""
        local _fthisdatatbl=$fdatatbl$_yyyymm
        local _fthisdatalst=$( find $dout/* -type f -name "$fdatalst_base$_yyyymm" 2>/dev/null )
        local str_ens=""

        [[ -f $_fthisdatatbl ]] && rm -f $_fthisdatatbl
        [[ ! -f $_fthisdatalst ]] && continue 

        for dexp in ${_arrdexp_yyyymm[@]};do
            local icyyyymmdd=$( echo ${dexp#$DFCST/} | cut -d'/' -f1 )
            local     intens=$( echo ${dexp#$DFCST/} | cut -d'/' -f2 | cut -c4- )

            #todo:  get status 
            if [[ ! -d $dexp ]];then
                local strstatus="--"
            else
                local strstatus=$( grep -w $dexp $_fthisdatalst 2>/dev/null | cut -d' ' -f1 | xargs )
            fi
                
            str_ens="${str_ens}${strstatus};"

            #todo:  write string to a file if this is the last ensm (15)
            if (( $intens == $lastensm ));then
                #str_ens="${str_ens};"
                stryyyymmdd="$icyyyymmdd;$str_ens"
                echo "$stryyyymmdd" >> $_fthisdatatbl
                str_ens=""
            fi 
        done
    done

    #todo:  write output with divider
    for yyyymmfinal in ${arryyyymmfinal[@]};do
        _arrfdatayyyymm+=($( find $cdir/* -maxdepth 0 -name "$fdatatbl_base$yyyymmfinal" 2>/dev/null )) 
    done

    _arrfdatayyyymm=($( printf '%s\n' ${_arrfdatayyyymm[@]} | sort -V ))

    for ftmpyyyymm in ${_arrfdatayyyymm[@]};do
        cat $ftmpyyyymm >> $ftmp2
        [[ "$ftmpyyyymm" != "${_arrfdatayyyymm[-1]}" ]] && echo $_divider >> $ftmp2
    done

    if (( $writetofile == 1 ));then 
        rev $ftmp2 | column -t -s';' | rev  >> $fmessage
    else
        rev $ftmp2 | column -t -s';' | rev  #>> $fmessage
    fi
    [[ -f $ftmp2 ]] && rm -f $ftmp2
    
    (( ${#_arrfdatayyyymm[@]} > 0 )) && mv ${_arrfdatayyyymm[@]} $dout/

    return 
}

sendmsg() {

    local _tmp_body=$cdir/_tmp_${strscr}_${FUNCNAME[0]}
    local _cdatetime=$( date +%Y%m%d_%H%M )
    local fmsg_final=$dstdout/message_${strscr}_${strexpid}_$_cdatetime
    
    if [[ -f $fmsg_final ]];then
        cat $fmessage   | tail -n +4 >> $ftmp1
        cat $fmsg_final | tail -n +4 >> $ftmp2

        diff $ftmp1 $ftmp2 >>/dev/null 2>&1
        status_diff=$?
    else
        status_diff=999
    fi
wmessage "@$LINENO status_diff = $status_diff"

    if (( $status_diff > 0 ));then 

        [[ -f $ferr     ]] && local sizeferr=$( stat --print='%s' $ferr )  || local sizeferr=0
        [[ -f $fmessage ]] && local sizef=$( stat --print='%s' $fmessage ) || local sizef=0

        if (( $sizef > 0 ));then 
            #note:  make sure to keep this code here. Otherwise, when diffing output, 
            #       output will be always different
            wmessage 
            wmessage "Time Ended - $(TZ=America/New_York date +'%m/%d/%Y %H:%M' )"
        fi

        if (( $sizeferr > 0 || $sizef > 0 ));then 
            if (( $sizef > 0 ));then
                msg_wheader_userdefined 40 "-" $( basename $ferr ) 
                wmessage "$( cat $ferr )"
                blrm=true
            fi

            #Note 07/18/2023 mutt does not keep text format. Send fmessage as an
            #                attachment via mutt
            msg_cyberpostman "$msg_subject" "$eadds" $fmessage $blmsgmutt

            #echo "See an attached file" >| $_tmp_body
            #mutt -s "$msg_subject" -a $fmessage -- "$eadds" < $_tmp_body
            #local status_email=$?

            $blsavefmsg && mv $fmessage $dstdout/$fmsg_fname
            (( $status_email == 0 )) && rm -f $fmessage
            $blrm && msg_newfile $ferr
            [[ -f $_tmp_body ]] && rm -f $_tmp_body
        fi
    fi
}



clean_dir() {

    [[ -f $ftmp1 ]] && rm -f $ftmp1
    [[ -f $ftmp2 ]] && rm -f $ftmp2
    [[ -f $ftmp3 ]] && rm -f $ftmp3
    [[ -f $flock && -n $flock ]] && rm -f $flock
    [[ -n $fdatatbl ]] && rm -f ${fdatatbl}_* 2>/dev/null
    [[ -n $fdatalst ]] && rm -f ${fdatalst}_* 2>/dev/null
    return
}

#================================================================================
#                                     Begin
#================================================================================
#starthere
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

strscr=$(basename "$0" | cut -d'_' -f2 | cut -d'.' -f1 )
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cdir=$SCRIPTPATH

[[ -z $cdir || ! -d $cdir ]] &&  echo "cdir (parent directory of this script) is undefined"  && exit

cd $cdir

flock=$cdir/${strscr}.lock
ftmp=$cdir/stdout/rundatetime/tmp_$strscr
[[ ! -f $ftmp ]] && install -D /dev/null $ftmp

#todo:  check tmp file size and create new if it is larger than 5kb
stmp=$( find $ftmp -printf "%s\n" )
(( $stmp > 5000 )) && rm -f $ftmp && touch $ftmp

#∞∞∞∞∞ debug (start) ∞∞∞∞∞
runhere=0
if (( $runhere == 1 ));then
    #ref:   https://intoli.com/blog/exit-on-errors-in-bash-scripts/
    #note:  exit when any command fails
    set -e
    
    #note:   keep track of the last executed command
    trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
    
    #note:  echo an error message before exiting
    trap 'echo "\"${last_command}\" command filed with exit code $?."' EXIT
fi
#∞∞∞∞∞ debug ( end ) ∞∞∞∞∞

#todo:  write date & time when this script is executed.
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

#todo:  set run-by-cron for false if it's empty
[[ -z $RUN_BY_CRON ]] && RUN_BY_CRON=false

if [[ ! $cdir/func_fcst.sh ]];then 
    echo "$cdir/func_fcst.sh does not exist";exit
else
    source $cdir/func_fcst.sh
fi

rundebug=0
writetofile=0
optb=false
optf=false
optl=false
opty=false
optr=false
optq=false

#todo:  update list when current minute is greater than 7 min
#note:  crontab for run_runutility.sh is set to run every 15 min. However,
#       this script does not need to run more than once per hour. Instead of 
#       changing crontab setting, this script exit out here.
cminute=$( date +'%-M')
if $RUN_BY_CRON ;then
    if (( $cminute < 7 ));then
        blmklst=true
    else
        #blmklst=false
        #exit
        :
    fi
fi

#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        create excel-like table for experiment status.

        *When this script is executed without option r, f, or u, it will 
        work on near-realtime forecast dates*

        Usage: ./$(basename "$0") [-hbwl] [-f data file] [-r YYYYMM-YYYYMM] [-y YYYY] srcme_file 

        input:
            A source file, which set various vars for a set of runs (i.e. srcme_pfe_fcst)

        options:
            -b  run with a debug mode
            -l  update status 
            -f  a data filename with a list of *DATES* (format: YYYYMMDD)
            -r  YYYYMM and YYYYMM range (format: YYYYMM-YYYYMM) 
            -y  decade (i.e. 1980 1990 2000 )
            -h  show this help text
            -w  write stdout to a file
"
OPTIND=1
while getopts ':hbwf:lr:y:' option; do
    case "$option" in
                   
        b)  optb=true && rundebug=1;;
        l)  optl=true;blmklst=true;;
        y)  opty=true; userinput_decyyyy=$OPTARG;;
        f)  $optr && die "Cannot specify option f when specifying option r"
            optf=true; fdata=$OPTARG;
            [[ ! $fdata ]] && die "$fdata does not eist";;
        r)  $optf && die "Cannot specify option r when specifying option f"
            optr=true; 
            userbegyyyymm=$( echo $OPTARG | cut -d'-' -f1 );
            userendyyyymm=$( echo $OPTARG | cut -d'-' -f2 );;
        h)  echo "$usage"; exit 0;;
        w)  writetofile=1;;
        \?) die "Invalid option: -$OPTARG" >&2
            exit 1;;
        :)  die "Option -$OPTARG requires an argument." >&2
            exit 1;;
    esac
done

! $optr && ! $optf && ! $opty && blnrt=true || blnrt=false

#todo:  get positional inputs.
shift $((OPTIND-1))
[[ "${1:-}" = "--" ]] && shift
arrposarg=( "$@" )
#================================================================================
#                               Check User Inputs
#================================================================================
i=0
srcf=${arrposarg[$i]};i=$(( i + 1 ))
inputcheck

source $srcf
#================================================================================
#                             Set Host Specific Vars
#================================================================================
hstshort=$( get_host )
[[ -z $strexpid ]] && die "strexpid is undefined"
[[ -z $DFCST ||  -z $DRST ]] && die "DFCST and/or DRST are undefined"

#================================================================================
#                                 Set Variables
#================================================================================
#mid
cdate=$( date +%Y%m%d_%H%M )
[[ -n $rstexpid ]] && strexpid=$strexpid$rstexpid 

dstdout=$cdir/stdout/$strscr/$strdout
dout=$cdir/output/$strscr/$strdout
dmess=$cdir/message
dparent=$( echo $DFCST | rev | cut -d'/' -f1 | rev ) 

#fcron=~/crontab_gcmfcstarchive_pfe20
fmessage=$cdir/message/message_${strscr}_$strexpid
    ferr=$cdir/message/stderr_${strscr}

ftmp1=$cdir/tmp_${strscr}_${strexpid}_1
ftmp2=$cdir/tmp_${strscr}_${strexpid}_2
ftmp3=$cdir/tmp_${strscr}_${strexpid}_3
        
fdatatbl_base=data_${strscr}_${strexpid}_table_
fdatatbl=$cdir/$fdatatbl_base
fdatalst_base=data_${strscr}_${strexpid}_list_
fdatalst=$cdir/$fdatalst_base

arrensm=($( echo ${arrintens[@]} | sed 's/[^ ]* */ens&/g' ))

arrstatus_clean=( C3C WC10C )
strstatus_clean=$( echo ${arrstatus_clean[@]} | sed 's# #\|#g' )
arrstatus_done=( C3 C3m WC10 C3C WC10C )
#strstatus_done=$( echo ${arrstatus_done[@]} | sed 's# #\|#g' )
#msg_hrpref=6
#msg_hrinc=3
#msg_chr=$( date +'%0k%M' )

maxsec=6000
maxmin=$( echo "$maxsec/60" | bc )

#note:  process limits
numproc_max=$( ulimit -u )
numsubtract=100
numproc_maxhere=$(( numproc_max - numsubtract ))

#todo:  set max screen sessions basd on who runs this script
if [[ -n $RUN_BY_CRON ]] && $RUN_BY_CRON ;then
    limscr=2
elif $blnode;then 
    limscr=10
else
    limscr=2
fi

#note:  save fmessage in dstdout. This was created 
#       in case sending fmessage doesn't work 
blsavefmsg=true

msg_subject="${hstshort}.${strscr}: $strexpid"
feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

[[ ! -f $fmessage ]] && touch $fmessage
[[ ! -d $dmess    ]] && mkdir -p $dmess
[[ ! -d $dout     ]] && mkdir -p $dout
[[ ! -d $dstdout  ]] && mkdir -p $dstdout
[[   -z $blmklst  ]] && blmklst=false

##note:  keep trap code here. script needs to delete data_strscr_strexpid_*
#trap clean_dir EXIT
#================================================================================
#                                  Main Process
#================================================================================
#letsbegin
if $optr;then
    #todo:  take extra steps to make sure that dexp with yyyyymm exists
    get_beg_and_end $userbegyyyymm $userendyyyymm
    arrdexp1=($( get_dexp $DFCST $begyyyymm $endyyyymm ${arrintens[@]} ))
      arrdexp=($( printf '%s\n' ${arrdexp1[@]} | xargs -i bash -c "[[ -d {} ]] && echo {}" ))
    arryyyymm=($( printf '%s\n' ${arrdexp[@]}| rev | cut -d'/' -f2 | rev | cut -c1-6 | sort -V | uniq )) 
    blin_yyyymm=true

elif $optf;then
    arryyyymmdd=($( cat $fdata )) 
    arryyyymm=($( printf '%s\n' ${arryyyymmdd[@]} | cut -c1-6 | sort -V | uniq )) 
    arrdexp=($( printf '%s\n' ${arryyyymmdd[@]} | xargs -i printf "$DFCST/{}/%s\n" ${arrensm[@]} | sort -V ))
    blin_yyyymm=false

elif $opty;then
    decyyy=$( echo $userinput_decyyyy | cut -c1-3 )
    arryyyymm=($( find $DFCST/* -maxdepth 0 -type d -name "[1-2]???????" 2>/dev/null | rev | cut -d'/' -f1 | rev | cut -c1-6 | sort | uniq | sort -V | grep $decyyy )) 
    blin_yyyymm=true

elif $blnrt;then
    #todo:  without options, this code will figureout all forecasts from the last month and ones up to 
    #       today's date in the current month. 
    arryyyymmdd=($( s2sv3_nrtdates )) 
    arryyyymm=($( printf '%s\n' ${arryyyymmdd[@]} | cut -c1-6 | sort -V | uniq ))
    blin_yyyymm=true
else
    arryyyymm=($( find $DFCST/* -maxdepth 0 -type d -name "[1-2]???????" 2>/dev/null | rev | cut -d'/' -f1 | rev | cut -c1-6 | sort | uniq | sort -V )) 
    blin_yyyymm=true
fi

if $optb;then 
wmessage \@$LINENO
    wmessage "Ready:"
    ahand_print ${arryyyymm[@]}
    exit
fi

msg_wheader

#todo:  update status list
if $blmklst;then 
      $blin_yyyymm && mklst_yyyymm ${arryyyymm[@]}
    ! $blin_yyyymm && mklst_dexp ${arrdexp[@]}
fi

wmessage "$(note_status)"

#todo:  write a table
wmessage 
  $blin_yyyymm && mktbl $blin_yyyymm ${arryyyymm[@]}
! $blin_yyyymm && mktbl $blin_yyyymm ${arrdexp[@]}
wmessage 
wmessage "Summary:"
mksummary ${arryyyymm[@]}
wmessage 
wmessage "Current AGCM Date for Running Exp:" 
mktbl_dates

[[ -f $ftmp1 ]] && rm -f $ftmp1
[[ -f $ftmp2 ]] && rm -f $ftmp2
[[ -f $ftmp3 ]] && rm -f $ftmp3

#todo:  send email
[[ -f $fmessage ]] && sendmsg

