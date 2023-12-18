#!/usr/bin/env bash

inputcheck(){
    (( ${#arrposarg[@]} != 1 )) && die "1 inputs are required"

    ! $optr && ! $optd && ! $optf && die "opt d, r or f are required"

    if $optd;then
        local _int_isinteger=$( misc_isinteger $useryyyymmdd )
        (( $_int_isinteger  > 0 )) && die "Option argument has to be an integer for opt d"
        (( ${#useryyyymmdd} != 8 )) && die "Option argument has to be 8 digit integer for opt d"
    fi

    $optr && [[ -z $startyyyymm || -z $endyyyymm ]] && die "YYYYMM range is a required input for option r"

    local int1=$( misc_isinteger $startyyyymm ) 
    local int2=$( misc_isinteger $endyyyymm ) 
    (( $int1 > 0 || $int2 > 0 )) && die "YYYYMM range have to be numbers delimited by a hyphen"
    
    return
}

filter() {

    #description:   get a list of perturbation rst file to transfer and extract.
    local _arrinput=( "$@" )
    local arr=() 
    local _input

    [[ ! -f $flst_lfe ]] && exit

    for _input in ${_arrinput[@]};do
        local cntbug=0
        local fpert=${_input}_pert_ocnens6to10.tar
        local drstic=$DRST/$_input
        local fpfe_tmsz=$drstic/note_perttmsz_ocn6t10

        #todo:  check if ic pert tar exist on lfe
        grep $fpert $flst_lfe 2>&1 >> /dev/null 
        local status_grep=$?
        (( $status_grep > 0 )) && continue

        debug_filter $_input
        
        #todo:  check if timestamp of tar file on lfe and one on pfe
        if [[ -d $drstic && -f $fpfe_tmsz ]] ;then 
            local flferst_time=$( grep $fpert $flst_lfe  2>/dev/null | cut -d' ' -f1 )
            local flferst_size=$( grep $fpert $flst_lfe  2>/dev/null | cut -d' ' -f2 )
            local fpferst_time=$( grep $fpert $fpfe_tmsz 2>/dev/null | tail -1 | cut -d' ' -f1 )
            local fpferst_size=$( grep $fpert $fpfe_tmsz 2>/dev/null | tail -1 | cut -d' ' -f2 )

            if [[ -n $flferst_time && -n $flferst_size && -n $fpferst_time && -n $fpferst_size ]] ;then
                (( $flferst_time == $fpferst_time && $flferst_size == $fpferst_size )) && continue
            fi
        fi


        debug_filter $_input


        #todo:  check if all ens6 to 10 exists
        local _numdir_miss=$( printf "$drstic/ens%s\n" ${arrens06t10[@]} | xargs -i bash -c '[[ ! -d {} ]] && echo {}' | wc -l )
        (( $_numdir_miss == 0 )) && continue 

        debug_filter $_input

        
        #todo:  check if note_ocnpet_ note exists 
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


move_dir(){
    local _rstyyyymmdd=$1
    local num_mvcomp=10
    local _ens11t15 _ens06t10

    #ens6  => ens11
    #ens7  => ens12
    #ens8  => ens13
    #ens9  => ens14
    #ens10 => ens15

    #todo:  hide (for now) old ens11-15
    local cnt_mvcompleted=0
    for _ens11t15 in ${arrens11t15[@]};do
        local _blmvcompleted=false
        local ensm=ens$_ens11t15
        local _numfnote=$( find $drst/$ensm/* -type f -name "${note_base}*" | wc -l )

        if [[ -d $drst/$ensm ]];then 
            if (( $_numfnote == 0 ));then
                wmessage "    Hide old $_rstyyyymmdd/$ensm"
                mv $drst/$ensm $drst/.$ensm
                local _status_mv=$?
                (( $_status_mv == 0 )) && [[ -d $drst/.$ensm ]] && _blmvcompleted=true || _blmvcompleted=false

            elif (( $_numfnote > 0 ));then
                wmessage "    Hide completed : $_rstyyyymmdd/$ensm"
                _blmvcompleted=true
            fi
        elif [[ -d $drst/.$ensm ]] &&  (( $_numfnote > 0 ));then 
            wmessage "    Hide completed : $_rstyyyymmdd/$ensm"
            _blmvcompleted=true
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
        local _fnote_06t10=$drst/$_ensm_06t10/$note_base${_rstyyyymmdd}$_ensm_06t10
        local _fnote_11t15=$drst/$_ensm_11t15/$note_base${_ensm_06t10}to${_ensm_11t15}_${_rstyyyymmdd}
   
        if [[ -f $_fnote_06t10 && -f $_fnote_11t15 ]];then 
            wmessage "@$LINENO    Move Completed : $_ensm_06t10 to $_ensm_11t15"
            
        elif [[ -d $drst/$_ensm_06t10 && ! -f $_fnote_06t10 ]];then 

            if [[ ! -d $drst/$_ensm_11t15 ]];then 
                mv $drst/$_ensm_06t10 $drst/$_ensm_11t15
                local _status_mv=$?

                if (( $_status_mv == 0 ));then 
                    wmessage "@$LINENO    Move Completed : $_ensm_06t10 to $_ensm_11t15"
                    _blmvcompleted=true
                    createnote_ens11to15 $_ensm_06t10 $_ensm_11t15 $ftar_ocn6to10
                    echo "$note_ocnpert_ens11t15" > $_fnote_11t15 
                else
                    _blmvcompleted=false
                    wmessage "**Move FAILED : $_ensm_06t10 to $_ensm_11t15"
                fi
                
            elif [[ -f $_fnote_11t15 ]];then
                _blmvcompleted=true
                wmessage "@$LINENO    Move Completed : $_ensm_06t10 to $_ensm_11t15"
            fi   

        elif [[ ! -d $drst/$_ensm_06t10 ]];then
            if [[ -f $_fnote_11t15 ]];then
                _blmvcompleted=true
                wmessage "@$LINENO    Move Completed : $_ensm_06t10 to $_ensm_11t15"
            else
                _blmvcompleted=false
                wmessage "**Missing $drst/$_ensm_06t10 - Unable to move $_ensm_06t10  to $_ensm_11t15"
            fi
        fi
        
        #todo:  count mv sucesses
        $_blmvcompleted && cnt_mvcompleted=$(( cnt_mvcompleted + 1 ))

    done
    (( $num_mvcomp == $cnt_mvcompleted )) && local _blmvdone=true || local _blmvdone=false

    echo $_blmvdone
}

check_sftc(){

    local _srch_term="Shift id is " 
    local _blsftrunning=false

    [[ ! -f $fdata_sftc ]] && echo $_blsftrunning

    local _numln_id=$( grep "$_srch_term" $fdata_sftc 2>/dev/null | wc -l )
    if (( $_numln_id == 1 ));then 
        local _sftid=$( grep "$_srch_term" $fdata_sftc 2>/dev/null | rev | cut -d' ' -f1 | rev )
    else
        #die "There are more than 1 shiftc is running"
        _blsftrunning=true
    fi
    
    shiftc --status=csv --state=run | grep $_sftid >>/dev/null 2>&1
    local _status_grep=$?
    
    if (( $_status_grep == 1 ));then
        local _sftc_status=$( shiftc --status=csv | grep $_sftid 2>/dev/null | cut -d',' -f2 )
        if [[ "$_sftc_status" == "done" ]];then 
            #wmessage "shiftc $_sftid is completed ( status = $_sftc_status ) "
            _blsftrunning=false
        fi
    else
        _blsftrunning=true
    fi

    echo $_blsftrunning
}

transfer_pert() {
    local arr=( "$@" )
    local fshiftin=$cdir/${strscr}_${FUNCNAME[0]}_shiftin
    local arrftar=()
    local arrftar_extracted=()
    local _yyyymmdd ensn 
   
    [[ -f $fshiftin ]] && rm -f $fshiftin

    #todo:  figure out which ftar needs to be transfered from lfe
    for _yyyymmdd in ${arr[@]};do
        
        [[ $_yyyymmdd == "wftmsz_tmp" ]] && continue

        local ftarpert_lfe=$DARCHRST/${_yyyymmdd}_pert_ocnens6to10.tar
        local ftarpert_pfe=$DRST/${_yyyymmdd}_pert_ocnens6to10.tar
        
        ssh -q lfe test -f $ftarpert_lfe 2>/dev/null 
        (( $? == 0 )) && local blpert=true || local blpert=false

        #todo:  check if *_pert_ocnens6to10.tar exists on pfe
        if [[ -f $ftarpert_pfe ]];then 
            local  flfesec=$( grep ${_yyyymmdd}_pert_ocnens6to10.tar $flst_lfe 2>/dev/null | tail -1 | cut -d' ' -f1 )
            local flfesize=$( grep ${_yyyymmdd}_pert_ocnens6to10.tar $flst_lfe 2>/dev/null | tail -1 | cut -d' ' -f2 )

            local fpfesec=$( stat --printf="%Y\n" $ftarpert_pfe ) 
            local fpfesize=$( stat --printf="%s\n" $ftarpert_pfe ) 

            if (( $flfesec == $fpfesec && $flfesize == $fpfesize ));then 
                arrftar+=( $_yyyymmdd )
                continue
            elif $blpert ;then
                rm -f $ftarpert_pfe
                echo "lfe:$ftarpert_lfe $DRST/" >> $fshiftin
            fi

        else
            $blpert && echo "lfe:$ftarpert_lfe $DRST/" >> $fshiftin
        fi
    done

    #todo:  transfer pert rst files
    if ! $blsftc_running;then 
        if [[ -f $fshiftin ]];then 
            msg_wheader_userdefined 40 - "Start Transfer"
            wmessage "  Total # tar : $( cat $fshiftin | wc -l )"
            wmessage
            wmessage "Transfer files from : $DARCHRST"
            wmessage "                 to : $DRST"
            wmessage

            shipping_pert $fshiftin
            #rm -f $fshiftin  2>/dev/null
        fi
    else
        [[ -f $fshiftin ]] && rm -f $fshiftin  2>/dev/null
    fi


    if (( ${#arrftar[@]} > 0 ));then 
        msg_wheader_userdefined 40 - "Extract"
        wmessage "  Total # tar : ${#arrftar[@]}"
        wmessage

        if (( $writetofile == 1 ));then
            printf "    %s_pert_ocnens6to10.tar\n" ${arrftar[@]} | column >> $fmessage 2>&1 
        else
            printf "    %s_pert_ocnens6to10.tar\n" ${arrftar[@]} | column 
        fi

        #todo:  extract pert rst if exsists on pfe
        for _yyyymmdd in ${arrftar[@]};do

            local dunp=$DRST/$_yyyymmdd/$enscont
            local icdate=$( date -d "$_yyyymmdd +1days" +%Y%m%d ) 
            local bllasticdateofthemonth=$( fcal_calclasticdateofthemonth $icdate ) 

            #todo:  create pertubed ens dir
            for  ensn in ${arrens06t10[@]};do
                local ddest=$DRST/$_yyyymmdd/ens$ensn

                [[ -d $ddest ]] && mkdir -p $ddest
                [[ ! -d $ddest/RESTART ]] && mkdir -p $ddest/RESTART
                [[ -f $DRST/$_yyyymmdd/note_perttmsz_ocn6t10 ]] && rm -f $DRST/$_yyyymmdd/note_perttmsz_ocn6t10 

                #todo:  write notes 
                grep ${_yyyymmdd}_pert_ocnens6to10.tar $flst_lfe >> $DRST/$_yyyymmdd/note_perttmsz_ocn6t10 

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

            #todo:  extract files from pert tar
            #+++++ cd $DRST (start) +++++
            cd $DRST

            local maxattempt=3
            local attempt=0
            local blextractsuccess=false
            local blatmsuccess=false
            local blocnsuccess=false
            local ftarpert_pfe=${_yyyymmdd}_pert_ocnens6to10.tar

            while (( $attempt < $maxattempt )) && ! $blextractsuccess ;do
            
                tar --overwrite -xf $ftarpert_pfe
                
                #todo:  check if extracting pert file were successfull
                local bllasticdate=$( fcal_calclasticdateofthemonth $_yyyymmdd )
                
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

                attempt=$(( attempt + 1  ))
            done

            if $blextractsuccess;then 
                rm -f $ftarpert_pfe
                arrftar_extracted+=( $_yyyymmdd )
            fi 

            cd - >/dev/null
            #+++++ cd $DRST ( end ) +++++
        done


        #todo:  create a symlink to message_rstmkpert in each pert dir
        for _rstyyyymmdd in ${arrftar_extracted[@]};do
            local fmess=$( find $DRST/* -type f -name "message_rstmkpert_*_${_rstyyyymmdd}_ocnens6to10" | xargs -i realpath {} )
            local ftar_ocn6to10=${_rstyyyymmdd}_pert_ocnens6to10.tar
            
            for ensn in ${arrens06t10[@]};do
                local ensm=ens$ensn
                local fmess_base=$( basename $fmess )
                local drstensm=$DRST/$_rstyyyymmdd/$ensm
                local _fnote=$drstensm/$note_base${_rstyyyymmdd}$ensm
                [[ -h $fmess_base ]] && unlink $fmess_base

                ln -s $fmess $drstensm/.
                createnote_ens6to10 $ensm $ftar_ocn6to10
                echo "$note_ocnpert_ens6t10" > $_fnote 
            done
        done
    fi

    return 
}

shipping_pert(){

    #local fshiftin=$1
    #local arrsftcin=($( fhand_sftcdiv $fshiftin $num_line ))
    local arrsftcin=( $1 )
    local numnum=$( echo -n ${#arrsftcin} | wc -m )

    [[ -f $fdata_sftc ]] && rm -f $fdata_sftc

    #todo:  copy rst tar file
    for sftcin in ${arrsftcin[@]};do
        local arrftrans=($( cat $sftcin | cut -d' ' -f1 | xargs -i basename {} ))

        wmessage
        #wmessage "$( date +'%m/%d/%Y %H:%M' ) $(printf '%0'$numnum'g' $cnt_fcompleted ) of $( printf '%0'$numnum'g' ${#arrsftcin[@]} ) $sftcin"
        wmessage "$( date +'%m/%d/%Y %H:%M' ) $sftcin"
        wmessage 
        wmessage "Transfer:"
        ahand_warr ${arrftrans[@]}
        wmessage 

        if $blnode;then
            if (( $writetofile == 1 ));then
                /usr/local/bin/shiftc --streams=1 --hosts=1 --no-cron --no-mail -f -d < $sftcin > $fdata_sftc 2>&1
                cat $fdata_sftc >> $fmessage 2>&1
            else
                /usr/local/bin/shiftc --streams=1 --hosts=1 --no-cron --no-mail -f -d < $sftcin > $fdata_sftc 2>&1
                cat $fdata_sftc
            fi
        else
            if (( $writetofile == 1 ));then
                /usr/local/bin/shiftc --no-cron --no-mail -f -d < $sftcin >> $fdata_sftc 2>&1
                cat $fdata_sftc >> $fmessage 2>&1
            else
                /usr/local/bin/shiftc --no-cron --no-mail -f -d < $sftcin >> $fdata_sftc 2>&1
                cat $fdata_sftc
            fi
        fi
    done

    #rm -f ${arrsftcin[@]} 2>/dev/null
    
    return
}

check_ocnpert(){
    local _arrrstyyyymmdd=( "$@" )
    local _arr=()
    local _arrrstyyyymmdd_exist=()
    local _dateprocessed_sec=$( date -d $dateprocessed +%s )
    local _rstyyyymmdd

    #message_rstmkpert_fcst_19951126_ocnens6to10
    #RESTART/ocean_temp_salt.res.nc
    #RESTART/ocean_velocity.res.nc

    #todo:  check if file exists
    for _rstyyyymmdd in ${_arrrstyyyymmdd[@]};do 
        local arrfpert=($( printf "$DRST/$_rstyyyymmdd/ens%s\n" ${arrens06t10[@]} | xargs -i printf "{}/RESTART/%s\n" ${arrfocnpert[@]} ))
        local arrfpert_miss=($( printf '%s\n' ${arrfpert[@]} | xargs -i bash -c "[[ ! -f {} ]] && echo {}" ))

        #todo:  write missing ocean perturbation files.
        (( ${#arrfpert_miss[@]} > 0 )) && _arr+=( "${_rstyyyymmdd}:true" ) || _arrrstyyyymmdd_exist+=( $_rstyyyymmdd ) 
    done 

    #todo:  check existing files are processed after 06/18/2022
    for _rstyyyymmdd in ${_arrrstyyyymmdd_exist[@]};do 
        local arrfpert=($( printf "$DRST/$_rstyyyymmdd/ens%s\n" ${arrens06t10[@]} | xargs -i printf "{}/RESTART/%s\n" ${arrfocnpert[@]} ))
        local arrfpert_exist=($( printf '%s\n' ${arrfpert[@]} | xargs -i bash -c "[[ -f {} ]] && echo {}" ))

        local arrfpert_older=()
        for fpert_exist in ${arrfpert_exist[@]};do
            local ftimstamp_sec=$( stat --print='%Y\n' $fpert_exist )

            (( $_dateprocessed_sec > $ftimstamp_sec )) && _arr+=( "${_rstyyyymmdd}:true" )
        done
    done 

    echo "${_arr[@]}" 
}

write_filetimestamps(){
    local _arrrstyyyymmdd=( "$@" )
    #message_rstmkpert_fcst_19951126_ocnens6to10
    #RESTART/ocean_temp_salt.res.nc
    #RESTART/ocean_velocity.res.nc
    local _rstyyyymmdd
    

    for _rstyyyymmdd in ${_arrrstyyyymmdd[@]};do 
        local arrfpert=($( printf "$DRST/$_rstyyyymmdd/ens%s\n" ${arrens06t10[@]} | xargs -i printf "{}/RESTART/%s\n" ${arrfocnpert[@]} ))
        local arrfpert_miss=($( printf '%s\n' ${arrfpert[@]} | xargs -i bash -c "[[ ! -f {} ]] && echo {}" ))
        local fmsg_rstmkpert=$DRST/$_rstyyyymmdd/${strmsgrstmkp_base1}${_rstyyyymmdd}$strmsgrstmkp_base2

        msg_wheader_userdefined 40 - "Ocean Perturbation - $_rstyyyymmdd"

        if [[ -f $fmsg_rstmkpert ]];then
            if (( $writetofile == 1 ));then 
                stat --print='  %.16y %n\n' $fmsg_rstmkpert >> $fmessage 2>&1
            else
                stat --print='  %.16y %n\n' $fmsg_rstmkpert
            fi
        else
            wmessage "***ALERT - rstmkpert Stdout does NOT Exist***"
            wmessage $fmsg_rstmkpert
        fi

        wmessage

        #todo:  write missing ocean perturbation files.
        if (( ${#arrfpert_miss[@]} > 0 ));then
            local arrfpert_exist=($( printf '%s\n' ${arrfpert[@]} | xargs -i bash -c "[[ -f {} ]] && echo {}" ))

            wmessage "Missing Ocean Perturbation:"

            if (( $writetofile == 1 ));then
                printf '%s\n' ${arrfpert_miss[@]} >> $fmessage 2>&1
            else
                printf '%s\n' ${arrfpert_miss[@]} 
            fi

            wmessage 
            wmessage "Existing Ocean Perturbation:"

            if (( $writetofile == 1 ));then
                printf '%s\n' ${arrfpert_exist[@]} | xargs -i stat --print='    %.16y %n\n' {} >> $fmessage 2>&1
            else
                printf '%s\n' ${arrfpert_exist[@]} | xargs -i stat --print='    %.16y %n\n' {}
            fi

        elif (( ${#arrfpert_miss[@]} == 0 ));then

            if (( $writetofile == 1 ));then
                printf '%s\n' ${arrfpert[@]} | xargs -i stat --print='  %.16y %n\n' {} >> $fmessage 2>&1
            else
                printf '%s\n' ${arrfpert[@]} | xargs -i stat --print='  %.16y %n\n' {}
            fi
        fi
        
        wmessage

    done 
    
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
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        transfer rst tar file from LFE and extract on PFE. This script is specific
        for transfering ocean perturabtion for ens6 to 10 ( *_pert_ocnens6to10.tar ).
        Control perturbation will not be transferred by this script. Refer to usage
        for runs_rstlfe2pfe.sh.

        Usage: ./$(basename "$0") [-chwb] [ -r ICYYYYMM-ICYYYYMM ] [-f data file] [ -d ICYYYYMMDD ] srcme_file 

        Input:
            Source file, which set various vars for a set of runs.

        options:
            -b  run with a debug mode
            -r  IC YYYYMM range (format: ICYYYYMM-ICYYYYMM) 
            -d  specify IC date (format: ICYYYYMMDD )
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
            startyyyymm=$( echo $OPTARG | cut -d'-' -f1 );
              endyyyymm=$( echo $OPTARG | cut -d'-' -f2 );;
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
cdate=$( date +'%m/%d/%Y %H:%M' )

#note:  reprocessing ocean pert for ens6-10 started sometime on 2022-06-18. They 
#       should have timestamp anytime on the day or after.
dateprocessed="2022-06-18"
dmess=$cdir/message
fmessage=$dmess/message_$strscr

flistlfe=wftmsz_flist
flst_lfe=$cdir/${strscr}_lfe_$flistlfe
fdata_sftc=$cdir/${strscr}_sftcrunning
fshiftin=$cdir/${strscr}_${FUNCNAME[0]}_shiftin

note_base=note_ocnpert_

strmsgrstmkp_base1="message_rstmkpert_fcst_"
strmsgrstmkp_base2="_ocnens6to10"

focnpert_salt=ocean_temp_salt.res.nc
focnpert_velo=ocean_velocity.res.nc
arrfocnpert=( $focnpert_salt $focnpert_velo )

arrens06t10=($( seq 6 10 )) 
arrens11t15=($( seq 11 15 )) 
arrocnicyyyymmdd=( 0130 0224 0326 0425 0530 0629 0729 0828 0927 1027 1126 1226 )
strocnicyyyymmdd=$( echo ${arrocnicyyyymmdd[@]} | sed 's# #|#g' )

num_line=25

msg_subject="${thishst}.${strscr}: $strexpid"

feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

[[ -f $flst_lfe ]] && rm -f $flst_lfe
[[ ! -d $dmess ]] && mkdir -p $dmess
[[ ! -d $DRST  ]] && mkdir -p $DRST
(( $writetofile == 1 )) && [[ ! -f $fmessage ]] && touch $fmessage
#================================================================================
#                                  Main Process
#================================================================================
#main
if [[ "$DARCHRST" =~ "nobackup" ]];then 
    cp -p $DARCHRST/$flistlfe $flst_lfe
else
    get_fwftmsz_nosup $DARCHRST $flst_lfe
    [[ ! -f $flst_lfe ]] && exit
fi

if $optd;then
    arrrstyyyymmdd=($(  date -d "$useryyyymmdd -1days" +%Y%m%d ))

elif $optf;then
    arryyyymmdd=($( cat $fdata ))
    arrrstyyyymmdd=($( printf '%s\n' ${arryyyymmdd[@]} | xargs -i bash -c 'date -d "{} -1days" +%Y%m%d'))

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

    arrrstyyyymmdd2=($( printf '%s\n' ${arricyyyymmdd1[@]} | xargs -i date -d "{}-1days" +%Y%m%d )) 

    #todo:  extract ocean pert dates
    arrrstyyyymmdd=($( printf '%s\n' ${arrrstyyyymmdd2[@]} | grep -E "$strocnicyyyymmdd" ))
fi

#wmessage "@$LINENO"
#ahand_warr ${arrrstyyyymmdd[@]}
#exit
        
if $optb;then
    wmessage "**** Skip Moving Dir ***"
    arrfinal=($( filter ${arrrstyyyymmdd[@]} ))

    #todo:  print which rst file will be transferd when opt b is selected.
    wmessage "yyyymmdd ( total = ${#arrfinal[@]} )":
    ahand_warr ${arrfinal[@]}
    wmessage
    exit
else
    msg_wheader
    wmessage "  Source File : ${arrposarg[0]}"
    if $optd || $optf;then
        $optd && wmessage "         Date : $yyyymmdd"
        $optf && wmessage "    Data File : $fdata"
    else
        wmessage "   Date Range : ${startyyyymm}-${endyyyymm}"
    fi
    wmessage

    arrrstyyyymmdd_ready=()
    #todo:  move directories
    for rstyyyymmdd in ${arrrstyyyymmdd[@]};do
        ftar_ocn6to10=${rstyyyymmdd}_pert_ocnens6to10.tar
        drst=$DRST/$rstyyyymmdd
        
        msg_wheader_userdefined 40 - "Move Dir - $rstyyyymmdd"
        blmvsuccess=$( move_dir $rstyyyymmdd ) 
        $blmvsuccess && arrrstyyyymmdd_ready+=( $rstyyyymmdd )
        wmessage
    done
#optb=true;rundebug=1
    arrfinal=($( filter ${arrrstyyyymmdd_ready[@]} ))

fi


#todo:  if shiftc is still running, exit here. 
blsftc_running=$( check_sftc )
$blsftc_running && exit


if (( ${#arrfinal[@]} == 0 ));then
    wmessage "*** NOTHING TO TRANSFER ***"
    wmessage

    #todo:  check if all ocean perturbation exists in DRST
    arrocncheck=($( check_ocnpert ${arrrstyyyymmdd[@]} ))
    
    if (( ${#arrocncheck[@]} > 0 ));then
        wmessage "YYYYMMDD with Missing Ocean Perturbation:"
        if (( $writetofile == 1 ));then
            printf '%s\n' ${arrocncheck[@]} | cut -d':' -f1 | xargs -i printf '%s\n' {} | column >> $fmessage 2>&1
        else
            printf '%s\n' ${arrocncheck[@]} | cut -d':' -f1 | xargs -i printf '%s\n' {} | column 
        fi
        wmessage
    fi
    
    write_filetimestamps ${arrrstyyyymmdd[@]}
   
elif (( ${#arrfinal[@]} > 0 ));then
    #todo:  start processing
    msg_wheader_userdefined 40 - "Start Transfer"
    wmessage "  Total # tar : ${#arrfinal[@]}"
    wmessage
    wmessage "Transfer files from : $DARCHRST"
    wmessage "                 to : $DRST"
    wmessage
    transfer_pert ${arrfinal[@]}
    wmessage

    blsftc_running=$( check_sftc )

    #todo:  check if all ocean perturbation exists in DRST
    if $blsftc_running;then
        arrocncheck=($( check_ocnpert ${arrrstyyyymmdd[@]} ))
        
        if (( ${#arrocncheck[@]} > 0 ));then
            wmessage "YYYYMMDD with Missing Ocean Perturbation:"
            if (( $writetofile == 1 ));then
                printf '%s\n' ${arrocncheck[@]} | cut -d':' -f1 | xargs -i printf '%s\n' {} | column >> $fmessage 2>&1
            else
                printf '%s\n' ${arrocncheck[@]} | cut -d':' -f1 | xargs -i printf '%s\n' {} | column 
            fi
            wmessage
        fi
        
        write_filetimestamps ${arrfinal[@]}
    fi
 
fi

#todo:  send email
if [[ -f $fmessage ]];then
    sizef=$( stat --print='%s' $fmessage )
    if (( $sizef > 0 ));then
        msg_cyberpostman "$msg_subject" "$eadds" $fmessage
        (( $? == 0 )) && rm -f $fmessage
    fi
fi




exit
