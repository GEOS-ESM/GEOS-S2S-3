#!/usr/bin/env bash

inputcheck(){
    (( ${#arrposarg[@]} != 1 )) && die "1 inputs are required"

    $optd && $optf && die "Cannot specify option d when specifying option f"
    $optd && $optr && die "Cannot specify option d when specifying option r"
    $optf && $optd && die "Cannot specify option f when specifying option d"
    $optf && $optr && die "Cannot specify option f when specifying option r"
    $optr && $optd && die "Cannot specify option r when specifying option d"
    $optr && $optf && die "Cannot specify option r when specifying option f"


    $optr && [[ -z $startyyyymm || -z $endyyyymm ]] && die "YYYYMM range is a required input for option r"

    local int1=$( misc_isinteger $startyyyymm ) 
    local int2=$( misc_isinteger $endyyyymm ) 
    (( $int1 > 0 || $int2 > 0 )) && die "YYYYMM range have to be numbers delimited by a hyphen"
    
    if ssh -q lfe true >>/dev/null 2>&1; then 
        :
    else
        die "$hostarc is unavailable" && exit
    fi
    return
}

filter_cont() {

    #description:   get a list of control rst file to transfer and extract.
    local _arrinput=( "$@" )
    local _arr
    local _input

    [[ ! -f $flstarc ]] && exit

    (( $rundebug == 1 )) && wmessage ${FUNCNAME[0]}

    for _input in ${_arrinput[@]};do
        local cntbug=0
        local fcont=$_input.tar
        local drst_icenscont=$DRST/$_input/$enscont
        local fpfe_tmsz=$DRST/$_input/note_conttmsz

        #todo:  check if ic cont tar exist on $hostarc
        grep $fcont $flstarc 2>&1 >> /dev/null 
        local status_grep=$?
        (( $status_grep > 0 )) && continue

        debug_filter $_input 
        
        #todo:  check if timestamp of tar file on $hostarc and one on pfe
        if [[ -d $drst_icenscont && -f $fpfe_tmsz ]] ;then 
            local flferst_time=$( grep $fcont $flstarc   2>/dev/null | cut -d' ' -f1 )
            local fpferst_time=$( grep $fcont $fpfe_tmsz 2>/dev/null | cut -d' ' -f1 )

            [[ -n $flferst_time && -n $fpferst_time ]] && (( $flferst_time == $fpferst_time )) && continue
        fi

        debug_filter $_input
        
        _arr+=( $_input )
    done
    
    echo "${_arr[@]}"
}

filter_pert() {

    #description:   get a list of perturbation rst file to transfer and extract.
    local _arrinput=( "$@" )
    local arr=() 
    local _input

    [[ ! -f $flstarc ]] && exit

    (( $rundebug == 1 )) && wmessage ${FUNCNAME[0]}

    for _input in ${_arrinput[@]};do
        local cntbug=0
        local fpert=${_input}_pert.tar
        local drstic=$DRST/$_input
        local fpfe_tmsz=$drstic/note_perttmsz

        #todo:  check if ic pert tar exist on $hostarc
        grep $fpert $flstarc >> /dev/null 2>&1 
        local status_grep=$?
        (( $status_grep > 0 )) && continue

        debug_filter $_input

        #todo:  check if timestamp of tar file on $hostarc and one on pfe
        if [[ -d $drstic && -f $fpfe_tmsz ]] ;then 
            local flferst_time=$( grep $fpert $flstarc  2>/dev/null | cut -d' ' -f1 )
            local fpferst_time=$( grep $fpert $fpfe_tmsz 2>/dev/null | cut -d' ' -f1 )

            [[ -n $flferst_time && -n $fpferst_time ]] && (( $flferst_time == $fpferst_time )) && continue
        fi

        debug_filter $_input

        arr+=( $_input )
    done

    echo "${arr[@]}"
}


transfer() {
    local arr=( "$@" )
    local yyyymmdd
    local arrftrans=()

    #todo:  transfer control
    local _status_here=$( transfer_cont ${arr[@]} )
wmessage \@$LINENO "status of transferring control rst = $_status_here"

    wmessage

    if (( $_status_here == 0 ));then 
        #todo:  transfer pert
        transfer_pert ${arr[@]}
    fi

    arrftrans=()
   
    return
}

transfer_cont() {
    local arr=( "$@" )
    local fshiftin=$cdir/${strscr}_${FUNCNAME[0]}_shiftin
    local _yyyymmdd
    local _status_transfer=999

    #todo:  choose transfer command
    if [[ "$hstshort" == "pfe" ]];then 
        local blusetar=false
        local blusecp=true
        local blusesftc=false
    elif [[ "$hstshort" == "dis" ]];then 
        local blusetar=true

        #note:  keep these two false. They won' work on discover.
        local blusecp=false
        local blusesftc=false
    fi

    [[ -f $fshiftin ]] && rm -f $fshiftin

    for _yyyymmdd in ${arr[@]};do
        
        [[ $_yyyymmdd == "wftmsz_tmp" ]] && continue

        local icdate=$( date -d "$_yyyymmdd +1days" +%Y%m%d ) 
        local ftar_cont=$DARCHRST/$_yyyymmdd.tar
        local fnote=$DRST/$_yyyymmdd/note_conttmsz
        local blcont=false
        local _scrcp=$cdir/${strscr}_${FUNCNAME[0]}_${_yyyymmdd}.sh

        [[ -f $_scrcp ]] && rm -f $_scrcp

        #todo:  test if rst file exists on lfe. 
        if [[ "$hstshort" == "pfe" ]];then 
            ssh -q $hostarc test -f $ftar_cont 2>/dev/null 
            (( $? == 0 )) && blcont=true
        elif [[ "$hstshort" == "dis" ]];then 
            sup test -f $hostarc:$ftar_cont 2>/dev/null 
            (( $? == 0 )) && blcont=true
        fi
        
        if $blcont ;then
            wmessage "trasfer $( basename $ftar_cont )"

            if $blusecp;then
                
                cat > $_scrcp << EOF
tar xf $ftar_cont -C $DRST/
status_tarf=\$?
echo \$status_tarf
EOF
                local status_cp=$( ssh -q $hostarc 'bash -s' < $_scrcp ) 
                
                if (( $status_cp == 0 ));then
                    [[   -f $fnote ]] && rm -rf $fnote
                    [[ ! -f $fnote ]] && install -D /dev/null $fnote
                    grep $_yyyymmdd.tar $flstarc >| $DRST/$_yyyymmdd/note_conttmsz
                    _status_transfer=0
                else
                    [[ -d $DRST/$_yyyymmdd     ]] && rm -rf $DRST/$_yyyymmdd
                    _status_transfer=1
                fi

            elif $blusetar;then

                if [[ "$hstshort" == "pfe" ]];then 
                    rsync -aqz $hostarc:$ftar_cont $DRST/ >>/dev/null 2>&1
                    local status_rsync=$?
                elif [[ "$hstshort" == "dis" ]];then 
                    sup rsync -aqz $hostarc:$ftar_cont $DRST/ >>/dev/null 2>&1
                    local status_rsync=$?
                fi

                if (( $status_rsync == 0 )); then
                    #todo: check tarball is corrupted or not
                    #+++++ cd DRST (start) +++++
                    cd $DRST
                    tar tf $_yyyymmdd.tar >> /dev/null 2>&1
                    local status_tartf=$?
                    cd - >/dev/null
                    #+++++ cd DRST ( end ) +++++
                    
                    if (( $status_tartf == 0 ));then
                        #+++++ cd DRST (start) +++++
                        cd $DRST
                        tar -xf $_yyyymmdd.tar 
                        local status_tar=$?
                        cd - >/dev/null
                        #+++++ cd DRST ( end ) +++++

                        if (( $status_tar == 0 ));then
                            [[ -f $fnote ]] && rm -rf $fnote
                            [[ ! -f $fnote ]] && install -D /dev/null $fnote
                            grep $_yyyymmdd.tar $flstarc >| $DRST/$_yyyymmdd/note_conttmsz
                            _status_transfer=0
                        else
                            [[ -f $DRST/$_yyyymmdd.tar ]] && rm -f $DRST/$_yyyymmdd.tar
                            [[ -d $DRST/$_yyyymmdd     ]] && rm -rf $DRST/$_yyyymmdd
                            _status_transfer=2
                        fi
                    else
                        [[ -f $DRST/$_yyyymmdd.tar ]] && rm -f $DRST/$_yyyymmdd.tar
                    fi
                fi
                    
            elif $blusesftc;then
                echo "$hostarc:$ftar_cont $DRST/" >> $fshiftin

                [[   -f $fnote ]] && rm -rf $fnote
                [[ ! -f $fnote ]] && install -D /dev/null $fnote
                grep $_yyyymmdd.tar $flstarc >| $DRST/$_yyyymmdd/note_conttmsz
                _status_transfer=0
            fi

        fi
        
        #todo:  cleaning    
        [[ -f $DRST/$_yyyymmdd.tar ]] && rm -f $DRST/$_yyyymmdd.tar
        [[ -f $_scrcp ]] && rm -f $_scrcp
    done

    if $blusesftc && [[ -f $fshiftin ]];then 
        #note:  status here comes out 0 regarless of success or failure (as of 03/21/2023)
        shipping $fshiftin
        _status_transfer=$?
    fi

    echo $_status_transfer 
}

transfer_pert() {
    local arr=( "$@" )
    local fshiftin=$cdir/${strscr}_${FUNCNAME[0]}_shiftin
    local _yyyymmdd ensn _arryyyymmdd
   
    #todo:  choose transfer command
    if [[ "$hstshort" == "pfe" ]];then 
        local blusetar=true
        #local blusecp=true
        local blusesftc=false

    elif [[ "$hstshort" == "dis" ]];then 
        local blusetar=true

        #note:  keep these two false. They won' work on discover.
        #local blusecp=false
        local blusesftc=false
    fi

    [[ -f $fshiftin ]] && rm -f $fshiftin

    for _yyyymmdd in ${arr[@]};do
        
        [[ $_yyyymmdd == "wftmsz_tmp" ]] && continue

        local blpert=false
        local dunp=$DRST/$_yyyymmdd/$enscont

        [[ ! -d $dunp ]] && continue 

        local icdate=$( date -d "$_yyyymmdd +1days" +%Y%m%d ) 
        local bllasticdateofthemonth=$( fcal_calclasticdateofthemonth $icdate ) 

        $bllasticdateofthemonth && local ensnlast=$pert_end_ocn || local ensnlast=$pert_end_atm

        local ftar_pert=$DARCHRST/${_yyyymmdd}_pert.tar
        #local ftar_pert_fname=${_yyyymmdd}_pert.tar
        
        if [[ "$hstshort" == "pfe" ]];then 
            ssh -q $hostarc test -f $ftar_pert 2>/dev/null 
            (( $? == 0 )) && blpert=true
        elif [[ "$hstshort" == "dis" ]];then 
            sup test -f $hostarc:$ftar_pert 2>/dev/null
            (( $? == 0 )) && blpert=true
        fi

        if $blpert ;then
            
            #todo:  create pertubed ens dir
            for  ensn in $( seq $pert_start $ensnlast );do
                local ddest=$DRST/$_yyyymmdd/ens$ensn

                [[   -d $ddest ]] && mkdir -p $ddest
                [[ ! -d $ddest/RESTART ]] && mkdir -p $ddest/RESTART
               
                #todo:  write notes 
                grep ${_yyyymmdd}.tar $flstarc >| $DRST/$_yyyymmdd/note_perttmsz
                grep ${_yyyymmdd}_pert.tar $flstarc >> $DRST/$_yyyymmdd/note_perttmsz

                #+++++ cd ddest (start) +++++
                cd $ddest
                find $dunp/* -maxdepth 0 -type f -name "*_rst" -exec ln -sf {} . \;
                find $dunp/* -maxdepth 0 -type f -name cap_restart -exec ln -sf {} . \;
                cd - >/dev/null
                #+++++ cd ddest ( end ) +++++

                #+++++ cd ddest (start) +++++
                cd $ddest/RESTART
                ln -sf $dunp/RESTART/* .
                cd - >/dev/null
                #+++++ cd ddest ( end ) +++++

            done 


            if $blusesftc;then 
                #_arryyyymmdd+=( $ftar_pert_fname )
                _arryyyymmdd+=( $_yyyymmdd )
                echo "$hostarc:$ftar_pert $DRST/" >> $fshiftin

            elif $blusetar;then 
                
                if [[ "$hstshort" == "pfe" ]];then 
                    rsync -aqz $hostarc:$ftar_pert $DRST/ >>/dev/null 2>&1
                    local status_rsync=$?
                elif [[ "$hstshort" == "dis" ]];then 
                    sup rsync -aqz $hostarc:$ftar_pert $DRST/ >>/dev/null 2>&1
                    local status_rsync=$?
                fi

                if (( $status_rsync == 0 )); then
                    #todo: check if tarball is corrupted
                    #+++++ cd DRST (start) +++++
                    wmessage "Transfer ${_yyyymmdd}_pert.tar"
                    cd $DRST
                    tar tf ${_yyyymmdd}_pert.tar >> /dev/null 2>&1
                    local status_tartf=$?
                    cd - >/dev/null
                    #+++++ cd DRST ( end ) +++++
                    
                    if (( $status_tartf == 0 ));then
                        _arryyyymmdd+=( $_yyyymmdd )
                    else
                        wmessage "    ... FAILED. Delete $DRST/${_yyyymmdd}_pert.tar and ${_yyyymmdd} directory."
                        [[ -f $DRST/$_yyyymmdd.tar ]] && rm -f  $DRST/${_yyyymmdd}_pert.tar
                        [[ -d $DRST/$_yyyymmdd     ]] && rm -rf $DRST/${_yyyymmdd}
                    fi
                fi
            fi
        fi
    done

    wmessage

    #todo:  transfer pert rst files
    $blusesftc && [[ -f $fshiftin ]] && shipping_pert $fshiftin

    #todo:  extract files from pert tar
    #+++++ cd $DRST (start) +++++
    cd $DRST
    for _yyyymmdd in ${_arryyyymmdd[@]};do
        local maxattempt=3
        local attempt=0
        local blextractsuccess=false
        local blatmsuccess=false
        local blocnsuccess=false
        local ftar_pert=${_yyyymmdd}_pert.tar

        wmessage "Extract ${_yyyymmdd}_pert.tar"

        while (( $attempt < $maxattempt )) && ! $blextractsuccess ;do
        
            tar --overwrite -xf $ftar_pert
            
            #todo:  check if extracting pert file were successfull
            local icdate=$( date -d "$_yyyymmdd +1days" +%Y%m%d ) 
            local bllasticdateofthemonth=$( fcal_calclasticdateofthemonth $icdate ) 
            
            for atmens in ${arrens4[@]};do
                [[ "${arrens4[0]}" == $atmens ]] && continue

                local dens=$_yyyymmdd/$atmens

                if [[ -d $dens ]];then
                    local numfatm=$( find $dens/* -type f -name "*_rst" | wc -l )
                    if (( $numfatm == 2 ));then        
                        blatmsuccess=true
                    else
                        blatmsuccess=false
                        break
                    fi
                else
                    blatmsuccess=false
                    break
                fi
            done

           if $bllasticdateofthemonth;then 
                for ocnens in ${arrens10[@]};do
                    local dens=$_yyyymmdd/$ocnens/RESTART

                    if [[ -d $dens ]];then
                        local numfocn=$( find $dens/* -type f -name "ocean_*" | wc -l )
                        if (( $numfocn == 2 ));then        
                            blocnsuccess=true
                        else
                            blocnsuccess=false
                            break
                        fi
                    else
                        local numfocn=0
                        blocnsuccess=false
                        break
                    fi
                done
            else
                blocnsuccess=true
            fi

            $blatmsuccess && $blocnsuccess && blextractsuccess=true

#wmessage            
#wmessage \@$LINENO 
#wmessage "bllasticdateofthemonth = $bllasticdateofthemonth"
#wmessage "blatmsuccess     = $blatmsuccess"
#wmessage "blocnsuccess     = $blocnsuccess"
#wmessage "blextractsuccess = $blextractsuccess"
#wmessage "         attempt = $attempt"
#wmessage

            $blextractsuccess && break

            attempt=$(( attempt + 1  ))

        done

        rm -f $ftar_pert
        
        if $blextractsuccess;then 
            wmessage "    ... Success"
        else            
            wmessage "    ... FAILED"
            if [[ -d $_yyyymmdd ]];then 
                wmessage "    ...   Delete $_yyyymmdd dir"
                rm -rf $_yyyymmdd
            fi
        fi

        wmessage

    done

    cd - >/dev/null
    #+++++ cd $DRST ( end ) +++++

    #todo:  create a symlink to message_rstmkpert in each pert dir
    #BUG?: 06/22/2023 _arryyyymmdd should have yyyymmdd, not yyyymmdd_*
    for _yyyymmdd in ${_arryyyymmdd[@]};do
        #local _yyyymmdd=$( echo $ftar | cut -d'_' -f1 )

        if [[ -d $_yyyymmdd ]];then 
            local fmess=$( find $DRST/* -type f -name "message_rstmkpert_*_$_yyyymmdd" | xargs -i realpath {} )
            
            local arrensn=($( find $DRST/$_yyyymmdd/* -maxdepth 0 -type d -name "ens*" )) 
            for ensn in ${arrensn[@]};do
                local fmess_base=$( basename $fmess )
                [[ -h $fmess_base ]] && unlink $fmess_base

                #+++++ cd DRST/_yyyymmdd (start) +++++
                cd $ensn
                ln -s $fmess .
                cd - >/dev/null
                #+++++ cd DRST/_yyyymmdd ( end ) +++++
            done
        fi
    done
    return 
}

extract_rst(){

    local _arricyyyymmdd=( "$@" ) 
    
    #extract_rst_cont ${_arricyyyymmdd[@]} 
    extract_rst_pert ${_arricyyyymmdd[@]} 

    return
}


extract_rst_cont(){
    local _arricyyyymmdd=( "$@" ) 
    local _yyyymmdd

    #todo:  extract files from pert tar
    #+++++ cd $DRST (start) +++++
    cd $DRST

#wmessage \@$LINENO    
#ahand_print ${_arricyyyymmdd[@]} 
#exit


    for _yyyymmdd in ${_arricyyyymmdd[@]};do

        local fnote=$DRST/$_yyyymmdd/note_conttmsz

        #todo: check tarball is corrupted or not
        #+++++ cd DRST (start) +++++
        cd $DRST
        tar tf $_yyyymmdd.tar >> /dev/null 2>&1
        local status_tartf=$?
        cd - >/dev/null
        #+++++ cd DRST ( end ) +++++
        
        if (( $status_tartf == 0 ));then
            #+++++ cd DRST (start) +++++
            cd $DRST
            tar -xf $_yyyymmdd.tar 
            local status_tar=$?
            cd - >/dev/null
            #+++++ cd DRST ( end ) +++++

            #if (( $status_tar == 0 ));then
            #    [[ -f $fnote ]] && rm -f $fnote
            #    [[ ! -f $fnote ]] && install -D /dev/null $fnote
            #    grep $_yyyymmdd.tar $flstarc >| $DRST/$_yyyymmdd/note_conttmsz
            #    _status_transfer=0
            #fi
        fi
    done

}



extract_rst_pert(){

    local _arricyyyymmdd=( "$@" ) 
    local _yyyymmdd

    #todo:  extract files from pert tar
    #+++++ cd $DRST (start) +++++
    cd $DRST

#wmessage \@$LINENO    
#ahand_print ${_arricyyyymmdd[@]} 
#exit


    for _yyyymmdd in ${_arricyyyymmdd[@]};do
        local dunp=$DRST/$_yyyymmdd/$enscont
        
        [[ ! -d $dunp ]] && continue 
        
        local icdate=$( date -d "$_yyyymmdd +1days" +%Y%m%d ) 
        local maxattempt=3
        local attempt=0
        local blextractsuccess=false
        local blatmsuccess=false
        local blocnsuccess=false
        local ftar_pert=${_yyyymmdd}_pert.tar

        local bllasticdateofthemonth=$( fcal_calclasticdateofthemonth $icdate ) 
        $bllasticdateofthemonth && local ensnlast=$pert_end_ocn || local ensnlast=$pert_end_atm

        #todo:  create pertubed ens dir with symlinks
        for  ensn in $( seq $pert_start $ensnlast );do
            local ddest=$DRST/$_yyyymmdd/ens$ensn

            [[   -d $ddest ]] && mkdir -p $ddest
            [[ ! -d $ddest/RESTART ]] && mkdir -p $ddest/RESTART
           
            ##todo:  write notes 
            #grep ${_yyyymmdd}.tar $flstarc >| $DRST/$_yyyymmdd/note_perttmsz
            #grep ${_yyyymmdd}_pert.tar $flstarc >> $DRST/$_yyyymmdd/note_perttmsz

            #+++++ cd ddest (start) +++++
            cd $ddest
            find $dunp/* -maxdepth 0 -type f -name "*_rst"     -exec ln -sf {} . \;
            find $dunp/* -maxdepth 0 -type f -name cap_restart -exec ln -sf {} . \;
            cd - >/dev/null
            #+++++ cd ddest ( end ) +++++

            #+++++ cd ddest (start) +++++
            cd $ddest/RESTART
            ln -sf $dunp/RESTART/* .
            cd - >/dev/null
            #+++++ cd ddest ( end ) +++++

        done 

        wmessage "Extract ${_yyyymmdd}_pert.tar"

        while (( $attempt < $maxattempt )) && ! $blextractsuccess ;do
        
            tar --overwrite -xf $ftar_pert
            
            #todo:  check if extracting pert file were successfull
            local fcstdate=$( date -d "$_yyyymmdd +1days" +%Y%m%d ) 
            local bllasticdateofthemonth=$( fcal_calclasticdateofthemonth $fcstdate ) 
            
            for atmens in ${arrens4[@]};do
                [[ "${arrens4[0]}" == $atmens ]] && continue

                local dens=$_yyyymmdd/$atmens

                if [[ -d $dens ]];then
                    local numfatm=$( find $dens/* -type f -name "*_rst" | wc -l )
                    if (( $numfatm == 2 ));then        
                        blatmsuccess=true
                    else
                        blatmsuccess=false
                        break
                    fi
                else
                    blatmsuccess=false
                    break
                fi
            done

           if $bllasticdateofthemonth;then 
                for ocnens in ${arrens10[@]};do
                    local dens=$_yyyymmdd/$ocnens/RESTART

                    if [[ -d $dens ]];then
                        local numfocn=$( find $dens/* -type f -name "ocean_*" | wc -l )
                        if (( $numfocn == 2 ));then        
                            blocnsuccess=true
                        else
                            blocnsuccess=false
                            break
                        fi
                    else
                        local numfocn=0
                        blocnsuccess=false
                        break
                    fi
                done
            else
                blocnsuccess=true
            fi

            $blatmsuccess && $blocnsuccess && blextractsuccess=true

#wmessage            
#wmessage \@$LINENO 
#wmessage "bllasticdateofthemonth = $bllasticdateofthemonth"
#wmessage "blatmsuccess     = $blatmsuccess"
#wmessage "blocnsuccess     = $blocnsuccess"
#wmessage "blextractsuccess = $blextractsuccess"
#wmessage "         attempt = $attempt"
#wmessage

            $blextractsuccess && break

            attempt=$(( attempt + 1  ))

        done

        #rm -f $ftar_pert
        
        if $blextractsuccess;then 
            wmessage "    ... Success"
        else            
            wmessage "    ... FAILED"

            for  ensn in $( seq $pert_start $ensnlast );do
                local _ddest_yyyymmddensm=$_yyyymmdd/ens$ensn
                if [[ -d $_ddest_yyyymmddensm ]];then 
                    wmessage "    ...   Delete $_ddest_yyyymmddensm dir"
                    rm -rf $_ddest_yyyymmddensm
                fi
            done
        fi

        wmessage

    done

    cd - >/dev/null
    #+++++ cd $DRST ( end ) +++++

    #todo:  create a symlink to message_rstmkpert in each pert dir
    for _yyyymmdd in ${_arricyyyymmdd[@]};do

        if [[ -d $_yyyymmdd ]];then 
            local fmess=$( find $DRST/* -type f -name "message_rstmkpert_*_$_yyyymmdd" | xargs -i realpath {} )
            
            local arrensn=($( find $DRST/$_yyyymmdd/* -maxdepth 0 -type d -name "ens*" )) 
            for ensn in ${arrensn[@]};do
                local fmess_base=$( basename $fmess )
                [[ -h $fmess_base ]] && unlink $fmess_base

                #+++++ cd DRST/_yyyymmdd (start) +++++
                cd $ensn
                ln -s $fmess .
                cd - >/dev/null
                #+++++ cd DRST/_yyyymmdd ( end ) +++++
            done
        fi
    done
    return 
}








shipping(){

    local fshiftin=$1
    local arrsftcin=($( fhand_sftcdiv $fshiftin $num_line ))
    local numnum=$( echo -n ${#arrsftcin} | wc -m )
    local _status_shipping=999

    if [[ "$hstshort" == "pfe" ]];then 
        local _thiscmd="/usr/local/bin/shiftc --no-cron --no-mail --wait --extract-tar -f -d"
    elif [[ "$hstshort" == "dis" ]];then 
        local _thiscmd="/home/gmaofcst/bin/sup shiftc --no-cron --no-mail --wait --extract-tar -f -d"
    fi

    #todo:  get a copy of restart files
    local cnt_fcompleted=1
    for sftcin in ${arrsftcin[@]};do
        wmessage
        wmessage "$( date +'%m/%d/%Y %H:%M' ) $(printf '%0'$numnum'g' $cnt_fcompleted ) of $( printf '%0'$numnum'g' ${#arrsftcin[@]} ) $sftcin"
        wmessage 

        if (( $writetofile == 1 ));then
             $_thiscmd < $sftcin >> $fmessage 2>&1
        else
             $_thiscmd < $sftcin 
        fi
        cnt_fcompleted=$(( cnt_fcompleted +1 ))
        
    done

    rm -f ${arrsftcin[@]} 2>/dev/null
    rm -f $fshiftin 2>/dev/null

    #note:  As of 03/21/2023, keep this status =0
    _status_shipping=0
    return $_status_shipping
}

shipping_pert(){

    local fshiftin=$1
    local arrsftcin=($( fhand_sftcdiv $fshiftin $num_line ))
    local numnum=$( echo -n ${#arrsftcin} | wc -m )

    if [[ "$hstshort" == "pfe" ]];then 
        local _thiscmd="/usr/local/bin/shiftc --no-cron --no-mail --wait --extract-tar -f -d"
    elif [[ "$hstshort" == "dis" ]];then 
        local _thiscmd="/home/gmaofcst/bin/sup shiftc --no-cron --no-mail --wait --extract-tar -f -d"
    fi

    #todo:  copy rst tar file
    local cnt_fcompleted=1
    for sftcin in ${arrsftcin[@]};do
        local arrftrans=($( cat $sftcin | cut -d' ' -f1 | xargs -i basename {} ))

        wmessage
        wmessage "$( date +'%m/%d/%Y %H:%M' ) $(printf '%0'$numnum'g' $cnt_fcompleted ) of $( printf '%0'$numnum'g' ${#arrsftcin[@]} ) $sftcin"
        wmessage 

        if (( $writetofile == 1 ));then
            printf '%s\n' ${arrftrans[@]} | column >> $fmessage 2>&1
            wmessage
            $_thiscmd < $sftcin >> $fmessage 2>&1
        else
            printf '%s\n' ${arrftrans[@]} | column
            wmessage
            $_thiscmd < $sftcin
        fi
        cnt_fcompleted=$(( cnt_fcompleted +1 ))
        
    done

    rm -f ${arrsftcin[@]} 2>/dev/null
    rm -f $fshiftin 2>/dev/null
    
    return
}

sendmsg() {
    #description:   send email
    local _blrm=false
    local _cdatetime=$( date +%Y%m%d_%H%M )
    local fmsg_final=$dstdout/message_${strscr}_${strexpid}_$_cdatetime

    [[ -f $ferr     ]] && local _sizeferr=$( stat --print='%s' $ferr )  || local _sizeferr=0
    [[ -f $fmessage ]] && local _sizef=$( stat --print='%s' $fmessage ) || local _sizef=0

    if (( $_sizef > 0 || $_sizeferr > 0 ));then
        if (( $_sizeferr > 0 ));then 
            msg_wheader_userdefined 40 "-" $( basename $ferr ) 
            wmessage "$( cat $ferr )"
            _blrm=true
        fi

        msg_cyberpostman "$msg_subject" "$eadds" $fmessage $blmsgmutt
        local status_email=$?

        $blsavefmsg && mv $fmessage $fmsg_final
        (( $status_email == 0 )) && rm -f $fmessage
        $_blrm && msg_newfile $ferr
    fi
    return
}

clean_dir() {
    [[ -n $flock && -f $flock ]] && rm -f $flock
    [[ -n $flstarc && -f $flstarc ]] && rm -f $flstarc
    return
}

#================================================================================
#                                     Begin
#================================================================================
#starthere
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

strscr=$(basename "$0" | cut -d'_' -f2 | cut -d'.' -f1 )
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

writetofile=0
rundebug=0
optb=false
optd=false
optf=false
optr=false
opts=false
opt_extracttaronly=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        transfer rst tar file from LFE and extract on PFE. 
        For expid = fcst, restarts will be transfered from LFE only if both 
        control and perturbation rst for the same ic date exist.
        
        *When this script is executed without option r, t, or d, it will 
        work on near-realtime forecast dates.*

        Usage: ./$(basename "$0") [-chwb] [ -r YYYYMM-YYYYMM ] [-f data file] [ -d YYYYMMDD ] srcme_file 

        Input:
            Source file, which set various vars for a set of runs.

        options:
            -b                 run with a debug mode
            -r                 YYYYMM range ( format: YYYYMM-YYYYMM) 
            -d                 specify date ( format: YYYYMMDD )
            -f                 data file with a list of YYYYMMDD
                --exttar-only  extract rst tar files when files are already in DRST dir.
            -s                 control and perturbation rst are transferred regardless of missing files
            -c                 clean unwanted files
            -h                 show this help text
            -w                 write stdout/err in a file

"


verbose=0
cnt=0
while :; do
    case $1 in
                   -b )  optb=true && rundebug=1;; 
                   -d )  [[ "$2" ]] && optd=true && yyyymmdd=$2  && shift;;
                   -f )  [[ "$2" ]] && optf=true && fdata=$2     && shift;;
                   -r )  [[ "$2" ]] && optr=true && userinput=$2 && shift;
                         startyyyymm=$( echo $userinput | cut -d'-' -f1 );
                           endyyyymm=$( echo $userinput | cut -d'-' -f2 );;
                   -c )  clean_dir;exit;;
        --exttar-only )  opt_extracttaronly=true ;;
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


#OPTIND=1
#while getopts ':hwcbsf:r:d:' option; do
#    case "$option" in
#        b)  optb=true;rundebug=1;; 
#        f)  $optd && die "Cannot specify option f when opt d is selected.";
#            $optr && die "Cannot specify option f when opt r is selected.";
#            optf=true; fdata=$OPTARG;;
#        d)  $optf && die "Cannot specify option d when opt f is selected.";
#            $optr && die "Cannot specify option d when opt r is selected.";
#            optd=true; yyyymmdd=$OPTARG;;
#        r)  $optd && die "Cannot specify option r when opt d is selected.";
#            $optf && die "Cannot specify option r when opt f is selected.";
#            optr=true; 
#            startyyyymm=$( echo $OPTARG | cut -d'-' -f1 );
#              endyyyymm=$( echo $OPTARG | cut -d'-' -f2 );;
#        s)  opts=true;; 
#        c)  clean_dir; exit 0;;
#        h)  echo "$usage"; exit 0;;
#        w)  writetofile=1;; 
#        \?) die "Invalid option: -$OPTARG" >&2
#            exit 1;;
#        :)  die "Option -$OPTARG requires an argument." >&2
#            exit 1;;
#    esac
#done

! $optr && ! $optd && ! $optf && blnrt=true || blnrt=false

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

[[ -z $DARCHRST || -z $DRST ]] && die "DARCHRST or DRST is undefied"
#================================================================================
#                             Set Host Specific Vars
#================================================================================
hstshort=$( get_host )
if [[ "$hstshort" == "pfe" ]];then
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

    hostarc=lfe

elif [[ "$hstshort" == "dis" ]];then
    umask 0022
    hostarc=dirac
fi

#todo:  keep inputcheck here. It needs host specific vars.
inputcheck

#================================================================================
#                    Set Global Variables and Source Scripts
#================================================================================
#mid
thishst=$( get_host )
[[ -n $rstexpid ]] && strexpid=$strexpid$rstexpid

dmess=$cdir/message
dstdout=$cdir/stdout/$strscr/$strdout

ferr=$dmess/stderr_${strscr}
fmessage=$dmess/message_$strscr

flistlfe=wftmsz_flist
flstarc=$cdir/${strscr}_lfe_$flistlfe
blwheader=false

maxtrans=6
num_line=25
strrst=RESTART
hhz=21z

blsavefmsg=true

msg_subject="${thishst}.${strscr}: $strexpid"

feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

[[ ! -d $dmess   ]] && mkdir -p $dmess
[[ ! -d $dstdout ]] && mkdir -p $dstdout
[[ ! -d $DRST    ]] && mkdir -p $DRST
(( $writetofile == 1 )) && [[ ! -f $fmessage ]] && touch $fmessage
#================================================================================
#                                  Main Process
#================================================================================
#letsbegin
[[ -f $flstarc ]] && rm -f $flstarc

if [[ "$DARCHRST" =~ "nobackup" ]];then 
    cp -p $DARCHRST/$flistlfe $flstarc

elif $opt_extracttaronly;then 
    touch $flstarc
else
    get_fwftmsz_nosup $DARCHRST $flstarc
    [[ ! -f $flstarc ]] && exit
fi

#todo:  find DRST/ICYYYYMMDD dir without ens1 dir.
#note:  This indicates that transfer was failed.
numens1=$( find $DRST/* -maxdepth 1 -mindepth 1 -type d -name ens1 | wc -l ) 
arrdicyyyymmdd=($( find $DRST/* -maxdepth 0 -type d -name "[1-2]???????" ))

if (( ${#arrdicyyyymmdd[@]} != $numens1 ));then 
    #todo:  find missing ens1 dir and remove icyyyymmdd
    arrdicyyyymmdd_miss=($( printf '%s\n' ${arrdicyyyymmdd[@]} | xargs -i bash -c "[[ ! -d {}/ens1 ]] && echo {} " ))

    ! $blwheader && msg_wheader && wmessage "$hostname" && blwheader=true

    if (( ${#arrdicyyyymmdd_miss[@]} > 0 ));then 
        wmessage "!!!!! ALERT !!!!!"
        wmessage "These ICYYYYMMDD are missing ens1 dir and will be removed:"
        ahand_print ${arrdicyyyymmdd_miss[@]}
        wmessage
                
        rm -rf ${arrdicyyyymmdd_miss[@]} 2>/dev/null 
    else
        wmessage "\@$LINENO Something is wrong with this part of code. Debug it!"
        wmessage
    fi

    msg_subject="${msg_subject} - ALEART Included!"

fi

if $optd;then
    arricyyyymmdd=($( date -d "$yyyymmdd -1days" +%Y%m%d ))
    arrfcont=($( filter_cont ${arricyyyymmdd[@]} )) 
    arrfpert=($( filter_pert ${arricyyyymmdd[@]} ))

elif $optf;then
    arryyyymmdd=($( cat $fdata ))
    arricyyyymmdd=($( printf '%s\n' ${arryyyymmdd[@]} | xargs -i bash -c 'date -d "{} -1days" +%Y%m%d'))
    arrfcont=($( filter_cont ${arricyyyymmdd[@]} )) 
    arrfpert=()

elif $optr;then
    #todo:  get all icyyyymmdd in yyyymm range 
    yyyymm=$startyyyymm
    while (( $yyyymm <= $endyyyymm ));do
        arricyyyymm+=( $yyyymm )
        yyyy=$( echo $yyyymm | cut -c1-4 )
        mm=$( echo $yyyymm | cut -c5- )
        yyyymm=$( fcal_nextmonth $mm $yyyy )
    done
    
    #todo:  get rst dates
    for icyyyymm in ${arricyyyymm[@]};do
        arricyyyymmdd1+=($( fcal_calcfcdates $icyyyymm ))
    done
    arricyyyymmdd=($( printf '%s\n' ${arricyyyymmdd1[@]} | xargs -i date -d "{}-1days" +%Y%m%d )) 

    #todo:  filter
    arrfcont=($( filter_cont ${arricyyyymmdd[@]} )) 
    arrfpert=($( filter_pert ${arricyyyymmdd[@]} ))

#wmessage \@$LINENO
#ahand_print ${arrfcont[@]}     
#wmessage
#ahand_print ${arrfpert[@]}     
#exit


elif $opt_extracttaronly;then 
    arrfcont_icyyyymmdd=($( find $DRST/* -maxdepth 0 -type f -name '[1-2]???????.tar'      2>/dev/null | rev | cut -d'/' -f1 | rev | cut -c1-8 ))
    arrfpert_icyyyymmdd=($( find $DRST/* -maxdepth 0 -type f -name '[1-2]???????_pert.tar' 2>/dev/null | rev | cut -d'/' -f1 | rev | cut -c1-8 ))

    arricyyyymmdd1=($( printf '%s\n'  ${arrfcont_icyyyymmdd[@]} ${arrfpert_icyyyymmdd[@]} | sort -V | uniq ))
    arricyyyymmdd=()

    for icyyyymmdd in ${arricyyyymmdd1[@]};do
        fcstdate=$( date -d "$_yyyymmdd +1days" +%Y%m%d ) 
        bllasticdateofthemonth=$( fcal_calclasticdateofthemonth $fcstdate ) 
        $bllasticdateofthemonth && ensnlast=$pert_end_ocn || ensnlast=$pert_end_atm
        numcont=$( echo $enscont | cut -c4- ) 
        arrdexist=($( printf "$DRST/$icyyyymmdd/ens%s\n" $( seq $numcont $ensnlast ) | xargs -i bash -c "[[ -d {} ]] && echo {}" ))

        (( ${#arrdexist[@]} == 0 )) && arricyyyymmdd+=( $icyyyymmdd )
#(( ${#arrdexist[@]} <= 1 )) && arricyyyymmdd+=( $icyyyymmdd )

    done
elif $blnrt;then 
    #todo:  without options, this code will figureout all forecasts from the last month and ones up to 
    #       today's date in the current month. 
    arryyyymmdd=($( s2sv3_nrtdates )) 
    arricdate=($( printf '%s\n' ${arryyyymmdd[@]} | xargs -i bash -c 'date -d "{} -1days" +%Y%m%d'))

    begyyyymm=$( echo ${arryyyymmdd[-1]} | cut -c1-6 )
    endyyyymm=$begyyyymm
    arricdate+=($( get_icdates $begyyyymm $endyyyymm ))


    #todo:  filter
    arrfcont=($( filter_cont ${arricyyyymmdd[@]} )) 
    arrfpert=($( filter_pert ${arricyyyymmdd[@]} ))
fi

#thisarr=( ${arryyyymmdd[@]} ) 
#ahand_print ${thisarr[@]} 
#wmessage

if $opts || $optd || $optf ;then
    arrfinal=($( printf '%s\n'  ${arrfcont[@]} ${arrfpert[@]} | sort -V | uniq | head -$maxtrans ))

elif ! $opt_extracttaronly && [[ "$strexpid" == "fcst"  ]];then
    #todo:  get rstdate which has both control and pert tar.
    #arrfinal=($( printf '%s\n'  ${arrfcont[@]} ${arrfpert[@]} | sort -V | uniq -d | head -$maxtrans))
    #arrfinal=($( printf '%s\n'  ${arrfcont[@]} ${arrfpert[@]} | sort -V | uniq ))
    arrfinal=($( printf '%s\n'  ${arrfpert[@]} | sort -V | uniq | head -$maxtrans ))
    #arrfinal=($( printf '%s\n'  ${arrfcont[@]} | sort -V | uniq | head -$maxtrans ))

elif $opt_extracttaronly;then     
    arrfinal=( ${arricyyyymmdd[@]} )
    #arrfinal=( 19911201 ) 
else
    arrfinal=($( printf '%s\n'  ${arrfcont[@]} ${arrfpert[@]} | sort -V | uniq -d | head -$maxtrans ))
fi

if $optb ;then
    #todo:  print which rst file will be transferd when opt b is selected.
    wmessage "yyyymmdd (total = ${#arrfinal[@]} )":
    ahand_warr ${arrfinal[@]}
    wmessage

elif ! $optb && (( ${#arrfinal[@]} > 0 ));then
    #todo:  start processing
    ! $blwheader && msg_wheader && wmessage "$hostname" && blwheader=true
    wmessage
    if $opt_extracttaronly;then 
        msg_wheader_userdefined 40 - "Skip Transfer and Extract Tar Only"
    else
        msg_wheader_userdefined 40 - "Start Transfer"
    fi
    wmessage "  Source File : ${arrposarg[0]}"
    if $optd || $optf;then
        $optd && wmessage "         Date : $yyyymmdd"
        $optf && wmessage "    Data File : $fdata"
                 wmessage "Total # Dates : ${#arrfinal[@]}"
    elif $opt_extracttaronly;then
        wmessage "  Total # tar : $( echo "${#arrfinal[@]} * 2" | bc )"
    else
        wmessage "   Date Range : ${startyyyymm}-${endyyyymm}"
        wmessage "  Total # tar : $( echo "${#arrfinal[@]} * 2" | bc )"
        wmessage
        wmessage "Transfer files from : $DARCHRST"
        wmessage "                 to : $DRST"
    fi
    wmessage

    if $opt_extracttaronly;then 
        extract_rst ${arrfinal[@]}
    else
        transfer ${arrfinal[@]}
    fi
fi

#todo:  send email
[[ -f $fmessage ]] && sendmsg 

exit


