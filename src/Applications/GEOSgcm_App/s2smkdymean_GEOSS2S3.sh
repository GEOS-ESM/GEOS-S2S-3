#!/usr/bin/env bash

inputcheck(){
    $optsrcme && [[ ! -f $userinput_srcme ]] && die "file not exist: $userinput_srcme"

    if $optf && [[ ! -f $userfdata ]] ;then 
        die "file not exist: $userfdata"
        exit
    fi

    return
}

IndexOf() {
    local i=1 S=$1;	shift  
    while [[ $S != $1 ]];do
        ((i++)); shift
        [ -z "$1" ] && { i=0; break; }
    done
    echo $i
}

msg_makehead() {
    local _num_repeat=$1
    local _str_char="$2"
     _str_head="$2"
    local i=1;
    while [[ i -lt $_num_repeat ]];do _str_head=${_str_head}${_str_char}; let i++ ;done
    return   
}

msg_wheader(){
    msg_makehead 80 "="

    if [[ -z $1 ]];then 
        local _str_subject="$(TZ=America/New_York date +'%m/%d/%Y %H:%M' )"
    else 
        local _str_subject="$1"
    fi

    local _len=${#_str_subject}
    local _len_head=${#_str_head}
    local _num_spaces=$( echo "$_len_head/2 + $_len/2" | bc  )

    wmessage "$_str_head"
    wmessage "$( printf '%*s\n' ${_num_spaces#0} "${_str_subject#0}" )"
    wmessage "$_str_head"
    return
}

writeheader() {
    msg_wheader
    wmessage "$hstname"
    wmessage "          exp location: "$dexp
#    wmessage "      archive location: "$darc
    wmessage
    return
}

msg_wheader_userdefined(){
    #Usage: ${FUNCNAME[0]} [ number of char to repeat ] [ character] [ a header subject (optional) ] 

    #msg_makehead $1 "$2" 
    local _str_head=$( printf -- "$2%.0s" $( seq 1  $1 ) )

    if [[ -z $3 ]];then 
        local _str_subject="$(TZ=America/New_York date +'%m/%d/%Y %H:%M' )"
        #echo "subject variable empty. Enter an email subject as an input if you wish."
    else 
        local _str_subject="$3"
    fi

    local _len=${#_str_subject}
    local _len_head=${#_str_head}
    local _num_spaces=$( echo "$_len_head/2 + $_len/2" | bc  )

    wmessage "$_str_head"
    wmessage "$( printf '%*s\n' ${_num_spaces#0} "${_str_subject}" )"
    wmessage "$_str_head"
    return
}

die() {
    if [[ -z $writetofile ]];then writetofile=0;fi

    if (( $writetofile == 1 ));then
        [[   -z $fmessage ]] && fmessage=fmessage
        [[ ! -f $fmessage ]] && touch $fmessage && msg_wheader
        echo "ERROR: $* Abort." >> $fmessage
    else
        echo "ERROR: $* Aborting." >&2
    fi

    kill -INT $$
}

wmessage() {
    if [[ -z $writetofile ]];then writetofile=0;fi

    if (( $writetofile == 1 ));then
        [[ -z $fmessage ]] && fmessage=message1
        echo "$*" >> $fmessage
    else
        echo "$*" >&2
    fi
    return
}

ahand_print() {
    
    #description:   print # of items in a given array and list items
    local _arrprint=( "$@" )
    local _numtotal=${#_arrprint[@]} 

    ahand_warr ${_arrprint[@]} 
    wmessage "Total = ${#_arrprint[@]}"
    wmessage
    return
}

ahand_warr() {
    #"print all elements in a given array with 3 spaces as indentation
    #Usage:  ${FUNCNAME[0]} [ array ]" 
    
    local _arr=( "$@" )
    
    if (( ${#_arr[@]} > 0 ));then
        if [[ -z $writetofile ]] || (( $writetofile == 0 )) ;then
            printf '    %s\n' ${_arr[@]} >&2
        elif (( $writetofile == 1 ));then
            printf '    %s\n' ${_arr[@]} >> $fmessage 2>&1
        fi
    fi
    return
}

get_host(){
    local hst1=$( hostname | cut -c1 ) 
    local hst3=$( hostname | cut -c1-3 ) 
    local hst4=$( hostname | cut -c1-4 ) 

    if [[ $hst4 == borg || $hst4 = warp ]];then
        #note:  borg and warp are starting charcters of computing nodes name.
        local thishost=dis

    elif [[ "$hst3" == "pfe" || "$hst3" == "lfe" ]] || [[ "$hst3" == "dis" ]];then 
        if [[ "$hst3" == "pfe" ]];then
            local thishost=pfe
        elif [[ "$hst3" == "lfe" ]];then
            local thishost=lfe
        elif [[ "$hst3" == "dis" ]];then 
            local thishost=dis
        fi
    elif [[ "$hst1" == "r" ]];then
        local thishost=pfe
    fi

    echo $thishost
}

misc_readfbyline(){
    #read a file line by line and return with an array
    #Usage: ${FUNCNAME[0]} [ a file name ]
    #Output: an array

      [[ -z $1 ]] && die "(${FUNCNAME[0]}) require a file name"
    [[ ! -f $1 ]] && die "(${FUNCNAME[0]}) an user input file does not exist"

    local _file=$1
    local arr
    arr=($( cat $_file | grep -Ev "^#|^$" ))

    echo ${arr[@]}
}


misc_isinteger() {
    [[ -z $1 ]] && die "(${FUNCNAME[0]}) require boolean for exp dir name (true or false)" && exit 

    local num=$1
    local re='^[0-9]+$'
    if [[ ! $num =~ $re ]] ; then
        local out=1
    else 
        local out=0
    fi
    echo $out
}

fcal_firstdayofthemonth(){
    #description determine if a given date is the first day of the month.
    #Return true or false
    local expdate=$1
    local intd=$( date -d $expdate +%-d )
    (( $intd == 1 )) && local bl=true || local bl=false
    echo $bl 
}

fcal_lastdayofthemonth(){
    #description determine if a given date is the last day of the month.
    #Return true or false

    local expdate=$1
    local bl=false

    local expdate_m=$( echo "$( date -d $expdate +%m )*1" | bc )
    local lastdate_m=$( echo "$( date -d "$expdate + 1 day" +%m )*1" | bc )

    if (( $lastdate_m != $expdate_m ));then
        bl=true
    fi
    echo $bl 
}


fcal_nextmonth() {
    local input=$1
    local numchar=${#input}
    local intint=$( misc_isinteger $input )

    if (( $intint == 0 )) && (( $numchar == 6 ));then 
        local yyyy=${input:0:4}
        local month=${input:4:2}
        local blreturnyear=true
    else
        if [[ -n $1 ]];then
            local month=$1
        else
            die "(${FUNCNAME[0]}) month (integer) is a required inputs"
        fi

        if [[ -n $2 ]];then
            local yyyy=$2
            local blreturnyear=true
        else
            local blreturnyear=false
        fi
    fi

    local imonth=$( echo "$month * 1" | bc )
    local nextm=$(( imonth + 1 ))

    if $blreturnyear;then
        local nextyear=$yyyy
    fi

    if (( $nextm == 13 ));then
        nextm=1
        if $blreturnyear;then
            nextyear=$(( yyyy + 1 ))
        fi
    fi

    if $blreturnyear;then
        local output=$nextyear$( printf '%02g' $nextm )
    else
        local output=$nextm
    fi

    echo $output
}

numfullmonths(){
    #description:   calculate a number of FULL months that s2sv3 ran
    local _capric_yyyymm=$1;shift
    local _capr_yyyymm=$1;shift
    local _bllastdayofthemonth=$1
    local arrmfull=() 

    [[ -z $_capric_yyyymm       ]] && die "(${FUNCNAME[0]}) missing arg for _capric_yyyymm "
    [[ -z $_capr_yyyymm         ]] && die "(${FUNCNAME[0]}) missing arg for _capr_yyyymm "
    [[ -z $_bllastdayofthemonth ]] && die "(${FUNCNAME[0]}) missing arg for _bllastdayofthemonth"

    if $_bllastdayofthemonth;then
        local yyyymm=$( fcal_nextmonth $_capric_yyyymm )
    else
        local yyyymm=$_capric_yyyymm
        for x in {1..2};do
            local yyyymm=$( fcal_nextmonth $_capric_yyyymm )
        done 
    fi

    while (( $yyyymm < $_capr_yyyymm ));do 
        arrmfull+=( $yyyymm )
        local month=$( date -d ${yyyymm}01 +%m )
        local intmonth=$( echo "$month*1" | bc )
        local yyyy=$( date -d ${yyyymm}01 +%Y )
        local yyyymm=$( fcal_nextmonth $intmonth $yyyy )
    done
    
    echo "${arrmfull[@]}"
}

nextXmonths(){
    local iny=$1
    local inm=$2
    local insft=$3
    local innextm=$4

    declare -a arryyyymm arrout
    arryyyymm=$(nextXmonths_yyyymm $iny $inm $insft $innextm)
    for yyyymm in ${arryyyymm[@]};do
        mm=$( echo $yyyymm | cut -c5-6 )
        yyyy=$( echo $yyyymm | cut -c1-4 )
        arrout+=( ${mm}:${yyyy} )
    done

    echo "${arrout[@]}"
}

nextXmonths_yyyymm(){
    local iny=$1
    local inm=$2
    local insft=$3
    local innextm=$4
    local arrout=()
    local arr=()
    local dumday=1

    local numm=$(( innextm - 1 ))
    local startmm=$(( inm + insft ))
    local numyear=$(( startmm/12 ))

    #todo:  calculate startdate and cdate
    if (( $startmm > 12 ));then
        startmm=$(( startmm - 12 * numyear ))
        iny=$(( iny + numyear ))
    fi
    local startdate=$( date -d $iny-$startmm-$dumday +%Y%m%d )
    local cdate=$startdate
    
    #todo:  calculate endate
    local endmm=$(( startmm + numm ))
    local endyear=$iny
    local numyear=$(( endmm/12 ))
    if (( $endmm > 12 ));then
        endmm=$(( endmm - 12 * numyear ))
        endyear=$(( iny + numyear ))
    fi
    local enddate=$( date -d $endyear-$endmm-$dumday +%Y%m%d )
    
    #todo:  calculate outputs
    arrout+=( $( date -d $cdate +%Y%m) )
    
    while [[ $cdate -lt $enddate ]];do
        cdateyear=$( date -d $cdate +%Y )
        cdatemm=$( printf '%01g' $( date -d $cdate +%m ) )
        cdatemm=$(( cdatemm + 1 ))
        if (( $cdatemm > 12 ));then
            cdatemm=$(( cdatemm -12 ))
            cdateyear=$(( cdateyear + 1 ))
        fi
        arrout+=( $cdateyear$( printf '%02g' $cdatemm ) )
        cdate=$cdateyear$( printf '%02g' $cdatemm )$( printf '%02g' $dumday )
    done
    echo ${arrout[@]}
}

createfsed() {
    [[ -f $fsed ]] && rm -f $fsed

    if $blexe_dexp;then 
        if [[ $hstshort == pfe ]];then
    
            cat > $fsed << EOF  
s?@\<S2SMKDYMEAN_STRSCR\>?$strscr?g
s?@\<COLLECTION\>?$coll?g
s?@\<S2SMKDYMEAN_BLEXE_DEXP\>?${blexe_dexp^^}?g
s?@\<S2SMKDYMEAN_CDIR\>?$cdir?g
s?@\<S2SMKDYMEAN_ARRYYYYMM\>?$( echo ${arrmon_3mo[@]} )?g
s?@\<S2SMKDYMEAN_NUMDYOUT\>?$numdyout?g
s?@\<S2SMKDYMEAN_NUMTHREADS\>?$numthreads?g
s?@\<S2SMKDYMEAN_BLTHREADS\>?${blthreads^^}?g
s?@\<S2SMKDYMEAN_BLWHIST\>?${blwhist^^}?g
s?@\<STREXPID\>?$strexpid?g
EOF

        elif [[ $hstshort == dis ]];then
    
            cat > $fsed << EOF
s?@\<S2SMKDYMEAN_STRSCR\>?$strscr?g
s?@\<COLLECTION\>?$coll?g
s?@\<S2SMKDYMEAN_BLEXE_DEXP\>?${blexe_dexp^^}?g
s?@\<S2SMKDYMEAN_CDIR\>?$cdir?g
s?@\<S2SMKDYMEAN_ARRYYYYMM\>?$( echo ${arrmon_3mo[@]} )?g
s?@\<S2SMKDYMEAN_NUMDYOUT\>?$numdyout?g
s?@\<S2SMKDYMEAN_NUMTHREADS\>?$numthreads?g
s?@\<S2SMKDYMEAN_BLTHREADS\>?${blthreads^^}?g
s?@\<S2SMKDYMEAN_BLWHIST\>?${blwhist^^}?g
s?@\<STREXPID\>?$strexpid?g
EOF
        fi
    else

        if [[ $hstshort == pfe ]];then
   
#s?@\<NCPUSS2SMKDYMEAN\>?$s2smkdymean_ncpus?g
#s?@\<MODELS2SMKDYMEAN\>?$s2smkdymean_model?g

            cat > $fsed << EOF  
           s?@\<BATCH_TIME\>?$batch_time?g 
 s?@\<S2SMKDYMEAN_WALLTIME\>?$s2smkdymean_walltime?g 
     s?@\<S2SMKDYMEAN_PRES\>?$s2smkdymean_pres?g 
     s?@\<S2SMKDYMEAN_QRES\>?$s2smkdymean_qres?g 
        s?@\<S2SMKDYMEAN_P\>?$s2smkdymean_p?g 
        s?@\<S2SMKDYMEAN_Q\>?$s2smkdymean_q?g 
        s?@\<BATCH_JOBNAME\>?$batch_jobname?g 
          s?@\<BATCH_GROUP\>?$batch_group?g 
     s?@\<BATCH_OUTPUTNAME\>?$batch_outputname?g 
     s?@\<BATCH_JOINOUTERR\>?$batch_joinouterr?g 
    s?@\<S2SMKDYMEAN_BLQOS\>?$s2smkdymean_blqos?g 
              s?@\<SETENVS\>?$setenvs?g 
                 s?@\<SITE\>?$site?g 
              s?@\<GEOSDIR\>?$geosdir?g 
              s?@\<GEOSBIN\>?$geosbin?g 
              s?@\<GEOSSRC\>?$geossrc?g 
        s?@\<GEOSS2S3_OPT2\>?$geoss2s3_opt2?g 
s?@\<S2SMKDYMEAN_DDESFINAL\>?$s2smkdymean_ddesfinal?g

s?@\<EXPDIR\>?$dexp?g
s?@\<FCSTDATE\>?$fcstdate?g
s?@\<ENSEMBLE_MEMBER\>?$ensm?g

s?@\<S2SMKDYMEAN_STRSCR\>?$strscr?g
s?@\<COLLECTION\>?$coll?g
s?@\<S2SMKDYMEAN_BLEXE_DEXP\>?${blexe_dexp^^}?g
s?@\<S2SMKDYMEAN_CDIR\>?$cdir?g
s?@\<S2SMKDYMEAN_ARRYYYYMM\>?$( echo ${arrmon_3mo[@]} )?g
s?@\<S2SMKDYMEAN_NUMDYOUT\>?$numdyout?g
s?@\<S2SMKDYMEAN_NUMTHREADS\>?$numthreads?g
s?@\<S2SMKDYMEAN_BLTHREADS\>?${blthreads^^}?g
s?@\<S2SMKDYMEAN_BLWHIST\>?${blwhist^^}?g
s?@\<STREXPID\>?$strexpid?g
EOF
        elif [[ $hstshort == dis ]];then
    
            cat > $fsed << EOF  
s?@\<S2SMKDYMEAN_STRSCR\>?$strscr?g
s?@\<COLLECTION\>?$coll?g
s?@\<S2SMKDYMEAN_BLEXE_DEXP\>?${blexe_dexp^^}?g
s?@\<S2SMKDYMEAN_CDIR\>?$cdir?g
s?@\<S2SMKDYMEAN_ARRYYYYMM\>?$( echo ${arrmon_3mo[@]} )?g
s?@\<S2SMKDYMEAN_NUMDYOUT\>?$numdyout?g
s?@\<S2SMKDYMEAN_NUMTHREADS\>?$numthreads?g
s?@\<S2SMKDYMEAN_BLTHREADS\>?${blthreads^^}?g
s?@\<S2SMKDYMEAN_BLWHIST\>?${blwhist^^}?g
s?@\<STREXPID\>?$strexpid?g
EOF
        fi
 
    fi

    return
}

set_varfixed() {
      
    if $blexe_dexp;then 
        [[ -z $numthreads ]] && numthreads=$numthreads_default
        [[ -z $blthreads  ]] && blthreads=$blthreads_default
        [[ -z $blwhist    ]] && blwhist=$blwhist_default

    else
        #note:  s2smkdymean_* vars should be set in srcme_* file if srcme files is used.  
        if [[ $hstshort == pfe ]];then

            [[ -n $DBUILD                 ]] && geosdir=$( dirname $DBUILD )       || geosdir=
            [[ -n $s2smkdymean_numthreads ]] && numthreads=$s2smkdymean_numthreads || numthreads=$numthreads_default
            [[ -n $s2smkdymean_blthreads  ]] &&  blthreads=$s2smkdymean_blthreads  ||  blthreads=$blthreads_default
            [[ -n $s2smkdymean_blwhist    ]] &&    blwhist=$s2smkdymean_blwhist    ||    blwhist=$blwhist_default  
            [[ -n $userinput_qid          ]] &&    resname=$userinput_qid          ||    resname=@S2SMKDYMEAN_RESNAME
            [[ -z $s2smkdymean_walltime   ]] && s2smkdymean_walltime="00:30:00" #S2SMKDYMEAN_WALLTIME
            [[ -z $s2smkdymean_ncpus      ]] && s2smkdymean_ncpus=20            #NCPUSS2SMKDYMEAN
            [[ -z $s2smkdymean_model      ]] && s2smkdymean_model=ivy           #MODELS2SMKDYMEAN

            s2smkdymean_ddesfinal=$dexp/$coll
                      strpbsresev="toss4"
                s2smkdymean_blqos=DELETE
                          setenvs=DELETE

                   batch_time="PBS -l walltime="

            if $optq;then                   
             s2smkdymean_pres="PBS -l select=1:ncpus=${s2smkdymean_ncpus}:mpiprocs=${s2smkdymean_ncpus}:model=${s2smkdymean_model}:aoe=${strpbsresev}"
             s2smkdymean_qres="PBS -q $resname"
                s2smkdymean_p="#PBS -l select=1:ncpus=${s2smkdymean_ncpus}:mpiprocs=${s2smkdymean_ncpus}:model=${s2smkdymean_model}"
                s2smkdymean_q="#PBS -q normal"

            else
             s2smkdymean_pres="#PBS -l select=1:ncpus=${s2smkdymean_ncpus}:mpiprocs=${s2smkdymean_ncpus}:model=${s2smkdymean_model}:aoe=${strpbsresev}"
             s2smkdymean_qres="#PBS -q $resname"
                s2smkdymean_p="PBS -l select=1:ncpus=${s2smkdymean_ncpus}:mpiprocs=${s2smkdymean_ncpus}:model=${s2smkdymean_model}"
                s2smkdymean_q="PBS -q normal"
            fi

                batch_jobname="PBS -N "
                  batch_group="PBS -W group_list=$( groups )"
             batch_outputname="PBS -o "
             batch_joinouterr="PBS -j oe -k oed"
                geoss2s3_opt2="PBS -W umask=0022"
                         site=NAS
                      geosbin=$geosdir/Linux/bin 
                      geossrc=$DBUILD


        elif [[ $hstshort == dis ]];then

            [[ -n $s2smkdymean_numthreads ]] && numthreads=$s2smkdymean_numthreads || numthreads=$numthreads_default
            [[ -n $s2smkdymean_blthreads  ]] &&  blthreads=$s2smkdymean_blthreads  ||  blthreads=$blthreads_default
            [[ -n $s2smkdymean_blwhist    ]] &&    blwhist=$s2smkdymean_blwhist    ||    blwhist=$blwhist_default
            [[ -z $s2smkdymean_walltime   ]] && s2smkdymean_walltime="00:30:00" #S2SMKDYMEAN_WALLTIME
            [[ -z $s2smkdymean_ncpus      ]] && s2smkdymean_ncpus=120           #NCPUSS2SMKDYMEAN
            [[ -z $s2smkdymean_model      ]] && s2smkdymean_model=mil           #MODELS2SMKDYMEAN
            [[ -z $s2smkdymean_blqos      ]] && s2smkdymean_blqos=true          #S2SMKDYMEAN_BLQOS
            [[ -n $DBUILD                 ]] && geosdir=$( dirname $DBUILD )       || geosdir=

                     setenvs=DELETE

                  batch_time="PBS -l walltime="
            s2smkdymean_pres="PBS -l select=1:ncpus=${s2smkdymean_ncpus}:mpiprocs=${s2smkdymean_ncpus}:model=${s2smkdymean_model}"
            s2smkdymean_qres="PBS -q @S2SMKDYMEAN_RESNAME"
               s2smkdymean_p="PBS -l select=1:ncpus=${s2smkdymean_ncpus}:mpiprocs=${s2smkdymean_ncpus}:model=${s2smkdymean_model}"
               s2smkdymean_q="PBS -q normal"
               batch_jobname="PBS -N "
                 batch_group="PBS -W group_list="
            batch_outputname="PBS -o "
            batch_joinouterr="PBS -j oe -k oed"
                        site=NCCS
                     geosbin=$geosdir/Linux/bin 
                     geossrc=$geosdir/src/GMAO_Shared/GEOS_Util
        fi
    fi

    return
}

check_numfout(){

    #description: check # of individual daily outputs to make sure that counts == calc
    
    local _coll=$1;shift
    local _blnumfout=true
    local _yyyymm

    for _yyyymm in ${arrmon_3mo[@]}; do 
        local _numf_cnt=$( printf '%s\n' ${arrthisfout[@]} | grep $fcstdate.$_coll.${_yyyymm} | grep nc4 | wc -l ) 
        local _numdays=${assoc_yyyymmdays[$_yyyymm]}
        local _numf_calc=$( echo "24 * $_numdays / $freq" | bc ) 

        if (( $_numf_calc != $_numf_cnt ));then 
            wmessage "miss output: $_coll/$_yyyymm ( Available $_numf_cnt instead of $_numf_calc )"
            _blnumfout=false
        fi
    done 

    echo $_blnumfout
}

create_scrkid(){
    local _thisfscr=$1

    sed -f $fsed $fscrkid_j_default >| $_thisfscr.tmp 
    sed "/$str_dellines/d" $_thisfscr.tmp >| $_thisfscr

    [[ -f $_thisfscr.tmp ]] && rm -f $_thisfscr.tmp
    [[ -f $fsed          ]] && rm -f $fsed

    return
}

create_symlink(){
    local          _coll=$1;shift
    local          _freq=$1;shift
    local _dscratch_coll=$1
    local _yyyymm _fout_yyyymmdd
    local _returnval=0

    local _numsym_calc=$( echo "24 / $_freq" | bc ) 
    
    msg_wheader_userdefined 40 - "$coll"

    local _cnt=0
    for _yyyymm in ${arrmon_3mo[@]};do 

        _cnt=$(( _cnt + 1 )) 

        #Ref: 20180610.sfc_tavg_3hr_glo_L720x361_sfc.dailymean.20180610.nc4
        #Ref: 20180610.sfc_tavg_3hr_glo_L720x361_sfc.20180609_2230z.nc4
        local _foutyyyymmdd_base=$fcstdate.$_coll.${_yyyymm}
    
        local _arrfout_yyyymm=($( printf '%s\n' ${arrthisfout[@]}     | grep $dexp/holding/$_coll/$_yyyymm/$fcstdate.$_coll.${_yyyymm} | grep \.nc4 ))
        local    _arryyyymmdd=($( printf '%s\n' ${_arrfout_yyyymm[@]} | xargs -i basename {} | \
                                    rev | cut -d'.' -f2 | rev | cut -c1-8 | sort -V | uniq   ))
        wmessage "    $_cnt of ${#arrmon_3mo[@]}    $_yyyymm"
    
        for _fout_yyyymmdd in ${_arryyyymmdd[@]};do
    
            local _thisdscratch=$_dscratch_coll/$_yyyymm/$_fout_yyyymmdd
            [[ ! -d $_thisdscratch ]] && mkdir -p $_thisdscratch
    
            #+++++ cd _thisdscratch (start) +++++
            cd $_thisdscratch        
            
            printf '%s\n' ${_arrfout_yyyymm[@]} | grep $fcstdate.$_coll.$_fout_yyyymmdd 2>/dev/null | grep nc4 | xargs -i ln -sf {} .

            local _numsym_cnt=$( find * -type l -name "$fcstdate.$_coll.${_fout_yyyymmdd}*" 2>/dev/null | wc -l ) 
            
            if (( $_numsym_calc != $_numsym_cnt ));then 
                wmessage "miss symlink: $_coll/$_yyyymm/$_fout_yyyymmdd ( Available $_numsym_cnt instead of $_numsym_calc ) "
                wmessage
                _returnval=999
                break 2
            fi

            cd - >/dev/null
            #+++++ cd _thisdscratch ( end ) +++++
            
        done
    done

    return $_returnval
}

transfer_fins(){        

    #description:   make inputs files ready for child scripts.
    local _dexp=$1;shift
    local _coll=$1;shift
    local _dscratch_coll=$1
    local _value_return=0

    local _yyyymm=$( date -d $fcstdate +%Y%m )
    local _arryyyymm_3mo=( $_yyyymm )  
    local _numdyout=0

    for x in {1..2};do 
        local _numdays=$( printf "%s\n" $( seq 0 31 ) | xargs -i date -d"${_yyyymm}01 +{}days" +%Y%m%d | grep "^$_yyyymm" | wc -l )
        _numdyout=$(( _numdyout + _numdays )) 
        _yyyymm=$( fcal_nextmonth $_yyyymm )
        _arryyyymm_3mo+=( $_yyyymm ) 
    done

    local _arrfout_fcsttar=($( printf '%s\n' ${_arryyyymm_3mo[@]}   | xargs -i echo $_dexp/$coll/$fcstdate.$coll.daily.{}.nc4.tar | sort -V )) 
    local _numtar_exist=$( printf '%s\n' ${_arrfout_fcsttar[@]} | xargs -i bash -c "[[ -f {} ]] && echo {}" | wc -l ) 
    
    if (( $_numtar_exist == ${#_arryyyymm_3mo[@]} ));then 
        #note:  extractdailies will extract individual files AND organize them by its dates
        extractdailies $_dexp/$_coll $_dscratch_coll ${_arryyyymm_3mo[@]}
    
    elif (( $_numtar_exist == 0 ));then 
    
       local     _fshiftin=$dmess/$fshiftin_base$fcstdate$ensm.$_coll
       local _fdata_sftcid=$dmess/$fdata_sftcid_base$fcstdate$ensm.$_coll
   
        if [[ ! -f $_fdata_sftcid ]];then 
    
            #/u/knakada/GEOSS2S3/GEOS_fcst/YYYYMMDD/ENSM/COLLECTION/YYYYMMDD.COLLECTION.daily.YYYYMM.nc4.tar
            #note:  create shiftcin file
            local _arrfout_fcsttar_base=($( printf '%s\n' ${_arrfout_fcsttar[@]} | sed "s#$DFCST/##g" | sort -V )) 
            local _arrdsrc=($( printf "$hstarc:$DARCH/%s\n" ${_arrfout_fcsttar_base[@]} ))
            local _arrdes_scratch=($( printf "$dout/scratch/$fcstdate/$ensm/$_coll/%s\n" ${_arryyyymm_3mo[@]} ))
    
            #todo:  create destination dir 
            printf '%s\n' ${_arrdes_scratch[@]} | xargs -i mkdir -p {}
    
            #note:  this will dump all individucal outputs files into YYYYMM dir. They are NOT organized
            #       by its dates!!
            paste -d ' ' <( printf '%s\n' ${_arrdsrc[@]} ) <( printf '%s\n' ${_arrdes_scratch[@]} ) >| $_fshiftin
    
            if [[ -s $_fshiftin ]];then 
                #Note: 2024/11/6 three atm_inst_* coollection transfer take ~3hrs!
                msg_wheader_userdefined 40 - "$coll"
                wmessage "Transfer Daily Files from Archive"
                wmessage 
                echo "$( $thiscmd_tarnowait < $_fshiftin )" >> $_fdata_sftcid 2>&1
                echo "$strsftcf"                            >> $_fdata_sftcid 2>&1
                echo "$_fshiftin"                           >> $_fdata_sftcid 2>&1
                [[ -s $_fdata_sftcid ]] && wmessage "$( cat $_fdata_sftcid )"
                wmessage
            
                #todo:  check shiftc id
                local _sftid=$( grep "$strsftid" $_fdata_sftcid | rev |  cut -d' ' -f1 | rev ) 

                if [[ -z $_sftid ]];then 
                    wmessage "Shiftc ID: Unknown"
                    rm -f  $_fshiftin 
                    rm -f  $_fdata_sftcid
                    wmessage "Delete: $_fshiftin"
                    wmessage "Delete: $_fdata_sftcid"
                    [[ -d $_dscratch_coll ]] && rm -rf $_dscratch_coll && \
                        wmessage "Delete: $_dscratch_coll"

                fi
            fi
            _value_return=999
        else

            local _sftid=$( grep "$strsftid" $_fdata_sftcid | rev |  cut -d' ' -f1 | rev ) 
            local _state=$( $cmd_shiftc --status=csv 2>/dev/null | grep $_sftid 2>/dev/null | cut -d',' -f2 )
    
            if [[ "$_state" == "done" ]];then 
                organizedailies $_dexp/$_coll $_dscratch_coll ${_arryyyymm_3mo[@]}
                _value_return=0

            elif [[ "$_state" == "error" || "$_state" == "stop" || -z $_sftid ]];then 
                msg_wheader_userdefined 40 - "$coll"

                if [[ "$_state" == "error" || "$_state" == "stop" ]];then 
                    wmessage "Status of Shiftc: $_state  $_sftid"
                elif [[ -z $_sftid ]];then 
                    wmessage "Shiftc ID: Unknown"
                fi

                rm -f  $_fshiftin 
                rm -f  $_fdata_sftcid
                wmessage "Delete: $_fshiftin"
                wmessage "Delete: $_fdata_sftcid"
                [[ -d $_dscratch_coll ]] && rm -rf $_dscratch_coll && wmessage "Delete: $_dscratch_coll"
            
                _value_return=999

            else
                msg_wheader_userdefined 40 - "$coll"
                wmessage "Status of Shiftc: $_state  $_sftid"
                wmessage
                
                _value_return=999
            fi
        fi
    else
        wmessage "FAILED: ${FUNCNAME[0]} - Unknown Error"
        _value_return=999
    fi

    return $_value_return
}

extractdailies(){
    local         _dcoll=$1;shift
    local _dscratch_coll=$1;shift
    local _arryyyymm_3mo=( "$@" ) 
    local          _coll=$( basename $_dcoll ) 
    local _yyyymm _fout_yyyymmdd
    
    msg_wheader_userdefined 40 - "$_coll"
    wmessage "Extract Daily Outputs"

    local _calc_files=0
    for _yyyymm in ${_arryyyymm_3mo[@]};do 
    
        local    _fout_fcsttar=$_dcoll/$fcstdate.$_coll.daily.$_yyyymm.nc4.tar
        local _dscratch_yyyymm=$_dscratch_coll/$_yyyymm

        [[ ! -d $_dscratch_yyyymm ]] && mkdir -p $_dscratch_yyyymm
    
        local _arrfouttar_yyyymmdd=($( tar -tf $_fout_fcsttar | rev | cut -d'.' -f2 | rev | cut -c1-8 | sort -V | uniq | grep -v $iyyyymmdd )) 
        local _numfhourly=($( tar -tf $_fout_fcsttar 2>/dev/null |  grep -v $iyyyymmdd  | wc -l )) 
        _calc_files=$(( _calc_files + $_numfhourly ))

        wmessage " YYYYMM: $_yyyymm $_numfhourly"
        
        local _arrproc=() 
        for _fout_yyyymmdd in ${_arrfouttar_yyyymmdd[@]};do
    
            #Ref: 20180610.sfc_tavg_3hr_glo_L720x361_sfc.20180609_2230z.nc4
            local _foutyyyymmdd_base=$fcstdate.$_coll.${_fout_yyyymmdd}
            local _arrfoutyyyymmdd=($( tar -tf $_fout_fcsttar 2>/dev/null  | grep ${_foutyyyymmdd_base}_ | grep "\.nc" ))

            _dscratch_yyyymmdd=$_dscratch_yyyymm/$_fout_yyyymmdd
            [[ ! -d $_dscratch_yyyymmdd ]] && mkdir -p $_dscratch_yyyymmdd

            #+++++ cd _dscratch_yyyymmdd (start) +++++
            cd $_dscratch_yyyymmdd        
    
            #tar -xf $_fout_fcsttar ${_foutyyyymmdd_base}_*z.nc4 & 
            tar -xf $_fout_fcsttar ${_arrfoutyyyymmdd[@]}      &
            local _proc=${!}
   
            cd - >/dev/null
            #+++++ cd _dscratch_yyyymmdd ( end ) +++++

            _arrproc+=( $_proc )

            if (( ${#_arrproc[@]} >= $numthreads_extract ));then
                wmessage "    # of Background Threads: $( echo ${_arrproc[@]} | sed 's# #, #g' ) ... wait ..."
                wait
                
                local _arrproc=() 
            fi
        done

        if (( ${#_arrproc[@]} > 0 ));then
            wmessage "    # of Background Threads: $( echo ${_arrproc[@]} | sed 's# #, #g' ) ... wait ..."
            wait
        fi

        wmessage
    done

    local _numfextracted=$( find $_dscratch_coll/* -type f -name "*.nc*" 2>/dev/null | grep -v $iyyyymmdd | wc -l ) 
    if (( $_calc_files == $_numfextracted ));then 
        touch $_dscratch_coll/${strscr}_inputready
    else
        wmessage "ERROR: # of counted input files ( $_numfextracted ) and calculated # ( $_calc_files ) do not match"
#wmessage \@$LINENO 
#wmessage "_numfextracted = $_numfextracted"
#wmessage "    _calc_files = $_calc_files"
    fi

    return
}

organizedailies(){
    local         _dcoll=$1;shift
    local _dscratch_coll=$1;shift
    local _arryyyymm_3mo=( "$@" ) 
    local          _coll=$( basename $_dcoll ) 
    local _yyyymm _fout_yyyymmdd

    msg_wheader_userdefined 40 - "$_coll"
    wmessage "Organize Daily Outputs"

    local _calc_files=0
    for _yyyymm in ${_arryyyymm_3mo[@]};do 
    
        local _dscratch_yyyymm=$_dscratch_coll/$_yyyymm
        [[ ! -d $_dscratch_yyyymm ]] && mkdir -p $_dscratch_yyyymm
    
        #local _arrfouttar_yyyymmdd=($( tar -tf $_fout_fcsttar | rev | cut -d'.' -f2 | rev | cut -c1-8 | sort -V | uniq | grep -v $iyyyymmdd )) 
        local _arrfouttar_yyyymmdd=($( find $_dscratch_yyyymm/* -type f -name "$fcstdate.*.nc4" 2>/dev/null | rev | cut -d'.' -f2 | rev | cut -c1-8 | sort -V | uniq | grep -v $iyyyymmdd )) 
        local _numfhourly=($( find $_dscratch_yyyymm/* -type f -name "$fcstdate.*.nc4" 2>/dev/null |  grep -v $iyyyymmdd  | wc -l )) 
        _calc_files=$(( _calc_files + $_numfhourly ))
        
        wmessage " YYYYMM: $_yyyymm"
        
        local _arrproc=() 
        for _fout_yyyymmdd in ${_arrfouttar_yyyymmdd[@]};do
    
            #Ref: 20180610.sfc_tavg_3hr_glo_L720x361_sfc.20180609_2230z.nc4
            local _foutyyyymmdd_base=$fcstdate.$_coll.${_fout_yyyymmdd}
    
            _dscratch_yyyymmdd=$_dscratch_yyyymm/$_fout_yyyymmdd
            [[ ! -d $_dscratch_yyyymmdd ]] && mkdir -p $_dscratch_yyyymmdd

            #+++++ cd _dscratch_yyyymm (start) +++++
            cd $_dscratch_yyyymm        

            mv ${_foutyyyymmdd_base}_*z.nc4 $_dscratch_yyyymmdd/ 2>/dev/null

            cd - >/dev/null
            #+++++ cd _dscratch_yyyymmdd ( end ) +++++

        done
    done
                
    wmessage

    local _numfextracted=$( find $_dscratch_coll/* -type f -name "*.nc*" 2>/dev/null | grep -v $iyyyymmdd | wc -l ) 
    if (( $_calc_files == $_numfextracted ));then 
        touch $_dscratch_coll/${strscr}_inputready
    else
        wmessage "ERROR: # of counted input files ( $_numfextracted ) and calculated # ( $_calc_files ) do not match"
#wmessage \@$LINENO 
#wmessage "_numfextracted = $_numfextracted"
#wmessage "    _calc_files = $_calc_files"
    fi

    return
}

transfer_fous(){

    #description: archive dailymean outputs.
    local _dexp=$1;shift
    local _valreturn=999

    #19910302.sfc_tavg_3hr_glo_L720x361_sfc.dailymean.199105.nc4.tar
    local _arrfout_fcsttar=($( printf '%s\n' ${arrcoll[@]} | xargs -i bash -c "find $_dexp/{}/* -type f -name \"$fcstdate.{}.dailymean.[1-9]?????.nc4.tar\" 2>/dev/null |sort -V " ))

    local     _fshiftin=$dmess/$fshiftin_base$fcstdate$ensm.arc
    local _fdata_sftcid=$dmess/$fdata_sftcid_base$fcstdate$ensm.arc

    if [[ ! -f $_fdata_sftcid ]];then 
    
        #todo:  create shiftcin file
        local _arrfout_fcsttar_base=($( printf '%s\n' ${_arrfout_fcsttar[@]} | sed "s#$DFCST/##g" | sort -V )) 
        local _arrdarc=($( printf "$hstarc:$DARCH/%s\n" ${_arrfout_fcsttar_base[@]} ))

        paste -d ' ' <( printf '%s\n' ${_arrfout_fcsttar[@]} ) <( printf '%s\n' ${_arrdarc[@]} ) >> $_fshiftin
    
        if [[ -s $_fshiftin ]];then 
            wmessage " Arc Location: $DARCH/$fcstdate/$ensm"
            msg_wheader_userdefined 45 - "Transfer Dailymean to Archive"
            wmessage 

            echo "$( $thiscmd_transfer < $_fshiftin )"  >> $_fdata_sftcid 2>&1
            echo "$strsftcf"                            >> $_fdata_sftcid 2>&1
            echo "$_fshiftin"                           >> $_fdata_sftcid 2>&1

            [[ -s $_fdata_sftcid ]] && wmessage "$( cat $_fdata_sftcid )"

            #todo:  check shiftc id
            local _sftid=$( grep "$strsftid" $_fdata_sftcid | rev |  cut -d' ' -f1 | rev ) 

            if [[ -z $_sftid ]];then 
                wmessage "Shiftc ID: Unknown"
                rm -f  $_fshiftin 
                rm -f  $_fdata_sftcid
                wmessage "Delete: $_fshiftin"
                wmessage "Delete: $_fdata_sftcid"
                [[ -d $_dscratch_coll ]] && rm -rf $_dscratch_coll && \
                    wmessage "Delete: $_dscratch_coll"

            fi

        else
            wmessage "ERROR: Failed to Transfer"
            wmessage
        fi
    else
            
        wmessage " Arc Location: $DARCH/$fcstdate/$ensm"
        msg_wheader_userdefined 40 - "Check Transfer Status"

        local _sftid=$( grep "$strsftid" $_fdata_sftcid | rev |  cut -d' ' -f1 | rev ) 
        local _state=$( $cmd_shiftc --status=csv 2>/dev/null | grep $_sftid 2>/dev/null | cut -d',' -f2 )

        if [[ "$_state" == "done" || "$_state" == "done+alert" ]];then 
            touch $farccomp

            if [[ "$_state" == "run+alert" ]];then 
                wmessage "Shiftc transfer has aleart:"
                wmessage "Status of Shiftc: ${_state}  $_sftid"
            fi
            wmessage "Transfer: COMPLETED"
            wmessage
            _valreturn=0

        elif [[ "$_state" == "error" || "$_state" == "stop" || -z $_sftid ]];then 

            if [[ "$_state" == "error" || "$_state" == "stop" ]];then 
                wmessage "Status of Shiftc: $_state  $_sftid"
            elif -z $_sftid ]];then 
                wmessage "Shiftc ID: Unknown"
            fi

            rm -rf $_fshiftin 
            rm -rf $_fdata_sftcid
            wmessage "Delete: $_fshiftin"
            wmessage "Delete: $_fdata_sftcid"

        else
            wmessage "Status of Shiftc: $_state  $_sftid"
            wmessage
        fi
    fi

    return $_valreturn

}

clean_dir() {
    [[ -f *.pyc ]] && rm -f *.pyc
    [[ -n $flock && -f $flock ]] && rm -f $flock
    return
}

#================================================================================
#                                     Begin
#================================================================================
#starthere
hst=$( hostname )
blnode=false
if [[ "${hst:0:3}" == "pfe" ]];then
    :
elif [[ "${hst:0:3}" == "dis" ]];then
    :
elif [[ "${hst:0:1}" == r ]];then 
    #note:  this is for host other than pfe @ NAS which has a name starting with 'r'
    blnode=true
else 
    exit
fi

[[ -z $strscr ]] && strscr=$( basename "$0" | cut -d'.' -f1 )
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cdir=$SCRIPTPATH

[[ -z $cdir || ! -d $cdir ]] && echo "cdir is undefined" && exit

cd $cdir

#todo:  check if this script is invoked as child script
[[ -n $RUNCHILD ]] && blchild=true || blchild=false

if ! $blchild;then

    flock=$cdir/${strscr}.lock
    ftmp=$cdir/tmp_$strscr
    [[ ! -f $ftmp ]] && install -D /dev/null $ftmp
    
    stmp=$( find $ftmp -printf "%s\n" )
    (( $stmp > 5000 )) && rm -f $ftmp && touch $ftmp
    
    [[ -f $flock ]] && \
        echo "$(TZ=America/New_York date +'%m/%d/%Y %H:%M' ) ... $flock exists..." >> $ftmp || \
        echo $(TZ=America/New_York date +'%m/%d/%Y %H:%M' ) >> $ftmp
    
    if set -C; 2>/dev/null >$flock;then 
        : 
    else
        exit
    fi

    trap clean_dir EXIT
fi

hstshort=$( get_host )

writetofile=0
rundebug=0
optb=false
optd=false
optf=false
optq=false
optredo=false
optsrcme=false

#================================================================================
#                                     Usage
#================================================================================
#ref: https://goo.gl/BUBwPV
usage="$(basename "$0") -- this is a program to:
        to calculate daily mean files for selected s2s-3 colletions.
        
        Usage: ./$(basename "$0") [-hbw] [-q qid] [ -f file ] [--redo] [--fsrc SRCME]

        input:
            A source file, which set various vars for a set of runs (i.e. srcme_pfe_fcst)

        options:
            -b          run with a debug mode ( this will stop before starts to proces)
            -d          specify forecast date ( format: YYYYMMDD )
            -f          a file with a list of dexp full path
            -q          queue id (default: normal for pfe; none for discover)  
                --redo  re-process data. 
                --srcme enter srcme_* file
            -h          show this help text
            -w          write stdout/err in a file
"
file=
verbose=0
cnt=0
while :; do
    case $1 in
                   -b )  optb=true && rundebug=1;;
                   -q )  [[ "$2" ]] && userinput_qid=$2   && optq=true     && shift;;
                   -f )  [[ "$2" ]] && userfdata=$2       && optf=true     && shift;;
              --srcme )  [[ "$2" ]] && userinput_srcme=$2 && optsrcme=true && shift;;
               --redo )  optredo=true ;;
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

#                --redo  re-process data. 
#                         *Input file has to have a list of exp dir with full paths
#================================================================================
#                               Check User Inputs
#================================================================================
i=0
inputcheck
$optsrcme && source $userinput_srcme

#[[ -z $blnrt ]] && blnrt=false

#================================================================================
#                             Set Host Specific Vars
#================================================================================

if [[ "$hstshort" == "pfe" ]];then
    #do something here (i.e. load modules, location of applications etc)
    #module load cdo/1.9.0
    #. /usr/share/modules/init/bash
    cmd_submit=/PBS/bin/qsub

    [[ -z $hstarc ]] && hstarc=lfe

elif [[ "$hstshort" == "dis" ]];then
    #do something here (i.e. load modules, location of applications etc)
    #. /usr/share/modules/init/bash
    #module load other/cdo-1.9.1
    #module load other/comp/gcc-5.3-sp3 other/SSSO_Ana-PyD/SApd_4.3.1_py2.7_gcc-5.3-sp3
    cmd_submit=/usr/slurm/bin/sbatch

elif $blnode;then 
    :
else
    exit
fi

#================================================================================
#                                 Set Variables
#================================================================================
#mid
[[ -z $strexpid ]] && strexpid=none

dexp=@EXPDIR

 fcstdate=$( echo $dexp | rev | cut -d'/' -f2 | rev )
     ensm=$( echo $dexp | rev | cut -d'/' -f1 | rev )
iyyyymmdd=$( date -d"$fcstdate -1days" +%Y%m%d) 
  iyyyymm=$( echo $iyyyymmdd | cut -c1-6 )

ds2su_realp=$( realpath $dexp/s2s_util ) 
 cdir_realp=$( realpath $cdir ) 

if [[ "$ds2su_realp" == "$cdir_realp" ]];then 
    cdir=$dexp/s2s_util
    #note: "execute from dexp"
    blexe_dexp=true
else
    blexe_dexp=false
fi

if ! $blexe_dexp && [[ "$hstshort" == "dis" ]] ;then 
    wmessage "Error: This script will NOT work on discover outside of fcst directory."
    wmessage "Exit."
    exit
fi

if $blexe_dexp;then
            dhold=$dexp/holding
         dscratch=$cdir/scratch
       fdata_coll=data_${strscr}_coll
fscrkid_j_default=$cdir/${strscr}.j

else
    if $blchild && [[ "$hstshort" == "pfe" ]] ;then
        #note:  these vars should be defined in the parent script (run_s2smkdymean.sh)
        #   dmess=$cdir/message
        # dstdout=$cdir/stdout/$strscr/$strexpid
        #    dout=$cdir/output/$strscr/$strexpid
        #dscratch=$dout/scratch/$fcstdate/$ensm 

        [[ -z              $dmess ]] && wmessage "undefined:              dmess" && exit 
        [[ -z            $dstdout ]] && wmessage "undefined:            dstdout" && exit 
        [[ -z               $dout ]] && wmessage "undefined:               dout" && exit 
        [[ -z           $dscratch ]] && wmessage "undefined:           dscratch" && exit 
        [[ -z         $fdata_coll ]] && wmessage "undefined:         fdata_coll" && exit 
        [[ -z  $fscrkid_j_default ]] && wmessage "undefined:  fscrkid_j_default" && exit 
        
        fmessage=$dmess/message_${strscr}_${strexpid}_${fcstdate}${ensm}
            ferr=$dmess/stderr_${strscr}_${strexpid}_${fcstdate}${ensm}
        farccomp=$dout/$strscr.${fcstdate}${ensm}.arc.completed
           fcomp=$dout/$strscr.${fcstdate}${ensm}.completed
        numfcomp=3
    else
        wmessage "ERROR: this scripts will not work outside of dexp/s2s_util dir unless"
        wmessage "       run_s2smkdymean.sh executes this as a child script. This script"
        wmessage "       also will not work outside of pfe at NAS. Exit"
        exit
    fi
fi
fsed=$cdir/${strscr}_${fcstdate}${ensm}_sedfile

fdata_sftcid_base=sftcid_${strscr}_${strexpid}.
    fshiftin_base=shiftin_${strscr}_${strexpid}.

if $optf; then 
    arrcoll=($( misc_readfbyline $userfdata )) 
else
    arrcoll=($( misc_readfbyline $fdata_coll )) 
fi

arrcoll=($( printf '%s\n' ${arrcoll[@]} | xargs -i bash -c "freq=\$( echo {} | cut -d'_' -f3 | cut -c2-3 ); \
        [[ \"\$freq\" == \"hr\" ]] && echo {}" | sort -V )) 

numthreads_default=5
 blthreads_default=true 
   blwhist_default=false   

str_dellines=DELETE
strsftid="Shift id is "    
strsftcf="Shiftc Input File:"
numthreads_extract=2

cmd_shiftc=/usr/local/bin/shiftc 

      thiscmd_tar="$cmd_shiftc --no-cron --no-mail --wait --extract-tar -f -d"
thiscmd_tarnowait="$cmd_shiftc --no-cron --no-mail        --extract-tar -f -d"
 thiscmd_transfer="$cmd_shiftc --no-cron --no-mail                      -f -d"
    

nummonth1=3

declare -A assoc_yyyymmdays

#note:  screen limit
#limscr=10

#todo:  set max screen sessions basd on who runs this script
if [[ -n $RUN_BY_CRON ]] && $RUN_BY_CRON ;then
    limscr=2
elif $blnode;then 
    limscr=10
else
    limscr=5
fi 

lastm=0

[[ -n $dscratch && ! -d $dscratch ]] && mkdir -p $dscratch
[[ -f $fsed ]] && rm -f $fsed
#================================================================================
#                                  Main Process
#================================================================================
#letsbegin

#todo:  get 3mo
  strxmonth=$( nextXmonths $( date -d $fcstdate +%Y ) $( printf '%01g' $( date -d $fcstdate +%m ) ) 0 $(( nummonth1 + 1 ))  )
     end_mm=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f1 )
   end_year=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f2 )
end3_yyyymm=$end_year$end_mm
bllastdayofthemonth=$( fcal_lastdayofthemonth $iyyyymmdd ) 
 arrmon_3mo=($( numfullmonths $iyyyymm $end3_yyyymm $bllastdayofthemonth ))
! $bllastdayofthemonth &&  arrmon_3mo=($( printf '%s\n' $iyyyymm ${arrmon_3mo[@]} | sort -V ))

numdyout=0
for x in "${!arrmon_3mo[@]}" ;do 
        
    yyyymm=${arrmon_3mo[x]}
        numdays=$( printf "%s\n" $( seq 0 31 ) | xargs -i date -d"${yyyymm}01 +{}days" +%Y%m%d | grep "^$yyyymm" | wc -l )

    if (( $x == 0 ));then 
        #todo:  subtract # of days 
        $bllastdayofthemonth && int_days=0 || int_days=$( date -d $iyyyymmdd +%-d )
        numdays=$(( numdays - int_days )) 
    fi 

    numdyout=$(( numdyout + numdays )) 
    assoc_yyyymmdays[$yyyymm]=$numdays

done

##Note: KEEP THIS
#printf '%s\n' "${!assoc_yyyymmdays[@]}" ${assoc_yyyymmdays[@]} | pr -t2  | tr -d '\t' | tr -s '[:space:]' | sed 's# # #g' | sort -V
#exit

#todo:  filter collection
if $optredo;then 
    arrfinal=( ${arrcoll[@]} ) 
    printf '%s\n' ${arrcoll[@]} | xargs -i bash -c "[[ -f $dout/$strscr.${fcstdate}${ensm}.{}.completed ]] && rm -f $dout/$strscr.${fcstdate}${ensm}.{}.completed"
    [[ -f $dout/$strscr.${fcstdate}${ensm}.completed    ]] && rm -f $dout/$strscr.${fcstdate}${ensm}.completed

else
    if $blexe_dexp;then 
        arrfinal=($( printf '%s\n' ${arrcoll[@]} | xargs -i bash -c "[[ ! -f $cdir/$strscr.${fcstdate}${ensm}.{}.completed ]] && echo {}" )) 
    else
        arrfinal=($( printf '%s\n' ${arrcoll[@]} | xargs -i bash -c "[[ ! -f $dout/$strscr.${fcstdate}${ensm}.{}.completed ]] && echo {}" )) 
        [[ -f $dout/$strscr.${fcstdate}${ensm}.completed ]] && bltransfer=false || bltransfer=true
    fi
fi

#todo: with debug mode, script stops here. 
if $optb;then
    wmessage "Ready to Process:"
    ahand_print ${arrfinal[@]}
    wmessage "Ready to Transfer: ${bltransfer^}"
    exit
fi

! $bltransfer && (( ${#arrfinal[@]} == 0 )) && exit

msg_wheader 
wmessage " Exp Location: $dexp"

for coll in ${arrfinal[@]};do

    if $blexe_dexp;then 
        thisdscratch=$dscratch/$coll
           thisfcomp=$cdir/$strscr.$fcstdate$ensm.${coll}.completed
            thisfscr=${strscr}.${fcstdate}${ensm}.${coll}.j
           thisdhold=$dhold/$coll

        [[ ! -d $thisdhold ]] && wmessage "ERROR: dir not exist - $thisdhold" && continue

     else
        thisdscratch=$dscratch/$fcstdate/$ensm/$coll 
           thisfcomp=$dout/$strscr.${fcstdate}${ensm}.${coll}.completed
            thisfscr=$thisdscratch/$strscr.${fcstdate}${ensm}.$coll.j
           thisdhold=$thisdscratch
     fi

    if $optredo;then 
        [[ -f $thisfcomp ]] && rm -f $thisfcomp
    else 
        [[ -f $thisfcomp ]] && continue 
    fi

    if $blexe_dexp;then 
        #note:  global var
        #ref: 20180610.atm_inst_6hr_glo_L720x361_p49.20180829_1500z.nc4
        arrthisfout=($( printf '%s\n' ${arrmon_3mo[@]} | \
                                    xargs -i bash -c "find $thisdhold/{}/* -type f -name \"$fcstdate.$coll.{}??_????z.nc4\" "| \
                                    grep -v "\.${iyyyymmdd}." | sort -V ))

        freq=$( echo $coll | cut -d'_' -f3 | cut -c1 )

        #todo:  check number of individual daily output files for each yyyymm
        blnumf=$( check_numfout $coll ) 
        ! $blnumf && continue

        set_varfixed
        createfsed
        create_scrkid $thisfscr

        create_symlink $coll $freq $thisdscratch    
        status_createsymlink=$?
    
        if (( $status_createsymlink > 0 ));then 
            wmessage "FAILED: $coll"
        else
            wmessage "$( $cmd_submit $thisfscr )"
        fi

    else
        fscr_basename=$( basename $thisfscr ) 

        if $optredo; then 
            rm -f $thisfscr
            [[ -d $thisdscratch ]] && rm -rf $thisdscratch
            find $dmess/* -type f -name "*${strexpid}*${fcstdate}${ensm}*" -not -name $( basename $fmessage ) -delete  2>/dev/null 
        fi 

        if [[ ! -f $thisdscratch/${strscr}_inputready ]];then 
            transfer_fins $dexp $coll $thisdscratch
            status_transfer=$?
#NOTE: 20250109: Be able to repeat transfer_fins when status_transfer fails.            
            (( $status_transfer > 0 )) && continue
        fi


        if [[    -f $thisdscratch/${strscr}_inputready ]];then 

            [[ -f $thisfscr ]] && rm -f $thisfscr
            set_varfixed
            createfsed
            create_scrkid $thisfscr

            [[ -f $fsed ]] && rm -f $fsed

            msg_wheader_userdefined 40 - "$coll"
            wmessage "Submit $fscr_basename"

            #+++++ cd thisdscratch (start) +++++
            cd $thisdscratch
            wmessage "$( $cmd_submit $fscr_basename )"
            cd - >/dev/null
            #+++++ cd thisdscratch ( end ) +++++

            #else
            #    msg_wheader_userdefined 40 - "$coll"

            #    if [[ ! -f $thisdscratch/${strscr}_inputready && ! -f $thisfscr ]] ;then 
            #        wmessage "Input Files - Not Ready"
            #        wmessage
            #    elif [[ -f $thisdscratch/${strscr}_inputready &&   -f $thisfscr ]] ;then 
            #        #wmessage "$fscr_basename - Ready"
            #        #wmessage

            #        wmessage "$fscr_basename - Resubmit"
            #        #+++++ cd thisdscratch (start) +++++
            #        cd $thisdscratch
            #        wmessage "$( $cmd_submit $fscr_basename )"
            #        cd - >/dev/null
            #        #+++++ cd thisdscratch ( end ) +++++
            #    fi
            #fi
        fi
    fi
done

[[ -f $fsed ]] && rm -f $fsed

$blexe_dexp && exit
    
#todo:  transfer dailymean to lou
if $blchild && [[ "$hstshort" == "pfe" ]] ;then

    arrfcomp=($( find $dout/* -type f -name "$strscr.${fcstdate}${ensm}.*.completed" 2>/dev/null )) 

    if (( ${#arrfcomp[@]} == ${#arrcoll[@]} ));then 
        transfer_fous $dexp
        status_transferfous=$?
    elif [[ -f $farccomp ]];then 
        status_transferfous=0
    else
        status_transferfous=999
    fi

    if (( $status_transferfous == 0 ));then 
    
        #todo:  copy log file 
        dscr_yyyymmddensm=$dscratch/$fcstdate/$ensm
        arrscr_j=($( find $dscr_yyyymmddensm/* -maxdepth 1 -mindepth 1 -type f -name "${strscr}.${fcstdate}${ensm}.*.?" 2>/dev/null ))

        for scr_j in ${arrscr_j[@]};do 
            fscr=$( basename $scr_j ) 
            [[ -h $dmess/$fscr ]] && unlink $dmess/$fscr

            [[ -f $scr_j ]] && mv $scr_j $dmess/
        done

        [[ -d $dscr_yyyymmddensm ]] && rm -rf $dscr_yyyymmddensm

        #todo: combine all files and scripts into one message_strscr file and save it in stdout dir
         arrfref=($( find $dmess/* -type f -name "${fshiftin_base}${fcstdate}${ensm}.*"     2>/dev/null | grep -v "\.arc" ))
        arrfref+=($( find $dmess/* -type f -name "${fdata_sftcid_base}${fcstdate}${ensm}.*" 2>/dev/null | grep -v "\.arc" ))

        for coll in ${arrcoll[@]};do 
            arrfref+=($( find $dmess/* -type f -name "${strscr}.${fcstdate}${ensm}.$coll.j" 2>/dev/null  ))
            arrfref+=($( find $dmess/* -type f -name "${strscr}.${fcstdate}${ensm}.$coll.o" 2>/dev/null  ))
        done

        arrfref+=($( find $dmess/* -type f -name "${fshiftin_base}${fcstdate}${ensm}.arc"     2>/dev/null ))
        arrfref+=($( find $dmess/* -type f -name "${fdata_sftcid_base}${fcstdate}${ensm}.arc" 2>/dev/null ))
               
        fmessage_original=$fmessage
        fmessage=$fmessage.tmp

        cat $fmessage_original >| $fmessage

        echo >> $fmessage
        echo >> $fmessage
        echo >> $fmessage

        for fref in ${arrfref[@]};do 
            writetofile=1
            msg_wheader_userdefined 80 "#" $( basename $fref )
            writetofile=0
            cat $fref >> $fmessage
            echo      >> $fmessage
            echo      >> $fmessage
            echo      >> $fmessage
        done

        mv $fmessage $dstdout/$( basename $fmessage_original ) 

        rm -f ${arrfref[@]} $fmessage_original

        #todo:  create the last completed file
        arrfcomp=($( find $dout/* -type f -name "$strscr.${fcstdate}${ensm}.*.completed" 2>/dev/null )) 
        (( ${#arrfcomp[@]} == $numfcomp ))  && touch $fcomp && rm -f ${arrfcomp[@]} 
    fi
fi

module purge

exit
