#!/usr/bin/env bash

setvars(){
    local _dexp=$1
    local _fhis2=$_dexp/HISTORY_2.rc

    fcstdate=$( echo $_dexp | rev | cut -d'/' -f2 | rev )
    ensm=$( echo $_dexp | rev | cut -d'/' -f1 | rev )

    arrcollmonpost=($( exp_getcollfreq n $_fhis2 ))
    arrcollmonpost_diu=($( exp_getcolldiurnal $_fhis2 ))
    arrfpst_collyyyymm=($( find $cdir/* -type f -name "gcm_post.*.j[0-9]?????" | grep -v sst_ ))

}

IndexOf() {
	local i=1 S=$1;	shift  
    while [[ $S != $1 ]]
    do  
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

die() {
    if [[ -z $writetofile ]];then writetofile=0;fi

    if (( $writetofile == 1 ));then
        if [[ -z $fmessage ]];then fmessage=fmessage;fi
        if [[ ! -f $fmessage ]]; then touch $fmessage;msg_wheader; fi
        echo "ERROR: $* Abort." >> $fmessage
    else
        echo "ERROR: $* Aborting." >&2
    fi

    kill -INT $$
}

wmessage() {
    if [[ -z $writetofile ]];then writetofile=0;fi

    if (( $writetofile == 1 ));then
        if [[ -z $fmessage ]];then fmessage=message1;fi
        echo "$*" >> $fmessage
    else
        echo "$*" >&2
    fi
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

exp_getcolldiurnal() {
   #description:   get collections which produce diurnal output
    local fhis2=HISTORY_2.rc

    if [[ -z $1 ]];then
        local _hist=$dexp/HISTORY.rc
    else
        local _hist=$1
    fi

    if [[ ! -f $_hist ]];then die "(${FUNCNAME[0]}) $_hist file does not  exist";fi

    #todo:  input check
    local strfrq=freq
    local intlim=240000
    local arr=()

    #todo:   fill in arrcoll in case it is empty
    local arrcoll=($( printf '%s\n' $( exp_getcollections $_hist ) | sort -V  | grep -v _1mo_glo_ ))
    for coll in ${arrcoll[@]};do
        grep $coll.$strfrq $_hist >/dev/null
        if (( $? == 0 ));then
            local freq=$( grep $coll.$strfrq $_hist 2>/dev/null  | awk -F: '{print $2}' | sed 's/^[ ]*//' | sed -e 's/,//' )
            local intfreq=$( echo "$freq*1" | bc )
            (( $intfreq < $intlim )) && arr+=( $coll )
        fi
    done

    echo ${arr[@]}
}

exp_getcollfreq() {
    #description:   get collections that produce specific frequencies HISTORY.rc ( ingnore commented-out collections with # )
    #               Enter m to get monthly producing collections
    #               Etner n to get non-monthly producing collections
    #               Usage: ${FUNCNAME[0]} [ m or n ] [full path to HISTORY.rc]
    #                NOTE: This function works for s2s v3 HISTORY.rc"
    
    local _input1=$1;shift
    local _input2=$1
    local strmon=monthly
    local _arr=()

    if [[ "$_input1" == "m" ]];then
        local blmon=true
    elif [[ "$_input1" == "n" ]];then
        local blmon=false
    else
        die "(${FUNCNAME[0]}) the first input has to be m or n"
    fi

    if [[ -z $_input2 ]];then 
        local cdir=`pwd`
        local _hist=$cdir/HISTORY.rc
    else
        local _hist=$_input2  
    fi
    
    local _arrcollmon=($( grep $strmon $_hist | tr -s '[:space:]' | cut -d':' -f1| cut -d' ' -f2 | cut -d'.' -f1 ))
    
    if $blmon;then
        _arr=( ${_arrcollmon[@]} )
    else
        local _arrcoll=($( exp_getcollections $_hist ))
        local _arr=($( printf '%s\n' ${_arrcoll[@]} ${_arrcollmon[@]} | sort | uniq -u ))
    fi

    echo ${_arr[@]}
}

exp_getcollections() {
    #description:    a function to get active collections from HISTORY.rc ( ingnore commented-out collections with # )

    local fhis2=HISTORY_2.rc
    if [[ -z $1 ]];then 
        local _hist=$dexp/$fhis2
    else
        local _hist=$1 
    fi

    #todo:  input check
    #_hist=$( inputcheck_hist $_hist )

    local lnum1=$( grep -n "COLLECTIONS" $_hist | cut -d':' -f1 )
    local lnum21=$( grep -n "::" $_hist | head -1  | cut -d':' -f1)
    local lnum2=$(( lnum21 - 1 ))
    local arr=($( sed -n $lnum1','$lnum2'p' $_hist | grep -v "##" | grep -v "#" | cut -d"'" -f2 | sort))

    echo ${arr[@]}
}

findpst2run(){
    #description:   find missing outputs from gcm_post.*.j and print gcm_post*.*j.
    local _dexp=$1
    local arrrunpst=() 
    local strcoll="-collections"
    local strrec="-recover" 
    local fpst_collyyyymm

    for fpst_collyyyymm in ${arrfpst_collyyyymm[@]};do

        local  lncmd=$( grep GEOSUTIL $fpst_collyyyymm | tail -1 )
        local   _coll=$( echo ${lncmd#*$strcoll} | cut -d' ' -f1 ) 
        local _yyyymm=$( echo ${lncmd#*$strrec}  | cut -d' ' -f1 ) 

        if [[ "${arrcollmonpost_diu[@]}" =~ $_coll ]];then
            local  arrfout=( $fcstdate.$_coll.monthly.$_yyyymm.nc4 )
            arrfout+=( $fcstdate.$_coll.daily.$_yyyymm.nc4.tar )
            arrfout+=( $fcstdate.$_coll.diurnal.$_yyyymm.nc4 )
        else 
            local  arrfout=( $fcstdate.$_coll.monthly.$_yyyymm.nc4 )
            arrfout+=( $fcstdate.$_coll.daily.$_yyyymm.nc4.tar )
        fi

        #19841202.aer_inst_3hr_glo_L720x361_slv.monthly.198501.nc4
        local arrfmiss=($( printf '%s\n' ${arrfout[@]} | xargs -i bash -c "[[ ! -f $_dexp/$_coll/{} ]] && echo {}" ))

        (( ${#arrfmiss[@]} > 0 )) && arrrunpst+=( $fpst_collyyyymm )

    done 

    local _arr=($( printf '%s\n' ${arrrunpst[@]} | sort -V ))  

    echo "${_arr[@]}"
}

findmissout(){
    #description:   find missing outputs from gcm_post.*.j and print gcm_post*.*j.
    local _dexp=$1
    local _arr=() 
    local strcoll="-collections"
    local strrec="-recover" 
    local fpst_collyyyymm


    for fpst_collyyyymm in ${arrfpst_collyyyymm[@]};do

        local  lncmd=$( grep GEOSUTIL $fpst_collyyyymm | tail -1 )
        local   _coll=$( echo ${lncmd#*$strcoll} | cut -d' ' -f1 ) 
        local _yyyymm=$( echo ${lncmd#*$strrec}  | cut -d' ' -f1 ) 

        #19841202.aer_inst_3hr_glo_L720x361_slv.monthly.198501.nc4
        if [[ "${arrcollmonpost_diu[@]}" =~ $_coll ]];then
            local  arrfout=( $fcstdate.$_coll.monthly.$_yyyymm.nc4 )
            arrfout+=( $fcstdate.$_coll.daily.$_yyyymm.nc4.tar )
            arrfout+=( $fcstdate.$_coll.diurnal.$_yyyymm.nc4 )
        else 
            local  arrfout=( $fcstdate.$_coll.monthly.$_yyyymm.nc4 )
            arrfout+=( $fcstdate.$_coll.daily.$_yyyymm.nc4.tar )
        fi

        local arrfmiss=($( printf '%s\n' ${arrfout[@]} | xargs -i bash -c "[[ ! -f $_dexp/$_coll/{} ]] && echo {}" ))

        (( ${#arrfmiss[@]} > 0 )) && _arr+=( ${arrfmiss[@]} )

    done 
    
    echo "${_arr[@]}"
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
cdir=@DFCST/@FCSTDATE/@ENSEMBLE_MEMBER/post
#strscr=$(basename "$0" | cut -d'_' -f2 | cut -d'.' -f1 )
strscr=gcmpost

cd $cdir

flock=$cdir/${strscr}.lock
ftmp=$cdir/tmp_$strscr

#todo:  check tmp file size and create new if it is larger than 5kb
[[ ! -f $ftmp ]] && touch $ftmp

#todo:  write date & time when this script is executed.
if [[ -f $flock ]];then
    echo "$(TZ=America/New_York date +'%m/%d/%Y %H:%M' ) ... $flock exists..." >> $ftmp
else
    cntdate=$(TZ=America/New_York date +'%m/%d/%Y %H:%M' )
    echo "$cntdate" >> $ftmp
fi

#todo:  lock this script (ref: https://www.putorius.net/?p=2607)
if set -C; 2>/dev/null >$flock; then
    :
else
    exit
fi

trap clean_dir EXIT

writetofile=0
optb=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        run gcm_post.*.j* as batch on command line instead of computing nodes. 
        Usage: ./$(basename "$0") [-hc]

    options:
        -b  debug mode
        -c  clean unwanted files
        -h  show this help text
"

while getopts ':hcb' option; do
    case "$option" in
        b)  optb=true;;
        h)  echo "$usage"; exit 0;;
        c)  clean_dir; exit 0;;
        \?) die "Invalid option: -$OPTARG" >&2
            exit 1;;
        :)  die "Option -$OPTARG requires an argument." >&2
            exit 1;;
    esac
done

#================================================================================
#                    Set Global Variables and Source Scripts
#================================================================================
#mid
dexp=@DFCST/@FCSTDATE/@ENSEMBLE_MEMBER
ferr=$dexp/post/stderr_${strscr}

numsec=15
maxsec=900
maxmin=$( echo "$maxsec/60" | bc )

#note:  process limits
numproc_max=$( ulimit -u )
numsubtract=50
numproc_maxhere=$(( numproc_max - numsubtract ))

limscr=1
#================================================================================
#                                  Main Process
#================================================================================
#main
setvars $dexp 

arrfpst=($( findpst2run $dexp ))

if $optb;then
    wmessage "gcm_post to run (${#arrfpst[@]})"
    ahand_warr ${arrfpst[@]}
    exit
fi

numproc=$( ps aux | awk '{ print $1 }' | grep -w $USER | wc -l )

msg_wheader
wmessage $hst
wmessage "exp location : $dexp"
wmessage
wmessage " Total # of gcm_post to run: $( printf '%+5s\n' ${#arrfpst[@]} ) ( screen maxlim = $( printf '%+5s\n' $limscr ) )"
wmessage "Currently Running processes: $( printf '%+5s\n' $numproc ) ( maxlim = $( printf '%+5s\n' $numproc_maxhere ) )"
wmessage

#+++++ cd dexp/post (start) +++++
cd $dexp/post
    
thisind=0
for fpst in ${arrfpst[@]};do

     thisind=$(( thisind + 1 ))
    scrchild=$( basename $fpst )
        coll=$( echo $scrchild | cut -d'.' -f2 )
      yyyymm=$( echo $scrchild | rev | cut -d'.' -f1 | rev | cut -c2- )
      stdout=$( echo $scrchild | cut -d'.' -f1-2 ).$yyyymm.out
    
    #todo:  count number of screen
    numscr=$( screen -ls | grep -i detached | grep ${strscr}_${fcstdate}${ensm}_ | wc -l )

    #todo:  count number of processes
    numproc=$( ps aux | awk '{ print $1 }' | grep -w $USER | wc -l )

    blexecuted=false
    blinitialnote=false
    totsec=0
    totmin=0

    sec0=$( date +%s )
    sec1=$sec0

    while ! $blexecuted;do 
        #if (( $numproc < $numproc_maxhere && $numscr < $limscr && $totsec <= $maxsec ));then 
        if (( $numproc < $numproc_maxhere && $numscr < $limscr ));then 

            totsec=0
            blexecuted=true
            wmessage " $( printf "%+8s of %+3s %+4s %-32s %+7s\n" $thisind ${#arrfpst[@]} " " $coll $yyyymm )"

            #todo:  run scrchild
            screen -dmS ${strscr}_${fcstdate}${ensm}_${coll}${yyyymm} bash -c "./$scrchild  >> $stdout 2>&1"  
            #screen -dmS ${strscr}_${fcstdate}${ensm}_${coll}_${yyyymm} bash -c "sleep 15s"  

        else
            sec2=$( date +%s )
            sec_diff=$(( sec2 - sec1 ))
            totsec=$(( sec2 - sec0 ))
            totmin=$( echo "$totsec / 60" | bc )

            #todo:  break out for both for loop (hence, 2)
            #(( $totsec >= $maxsec )) && break 2 

            if ! $blinitialnote;then 
                wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screens are running and $( printf '%+2s\n' $numproc ) processes are running" #- will wait for max of $maxmin minutes"
                blinitialnote=true
            elif (( $sec_diff > 60 ));then
                wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screens are running and $( printf '%+2s\n' $numproc ) processes are running - waited for $totmin min"
                sec1=$( date +%s )
            fi

        fi

        numscr=$( screen -ls | grep -i detached | grep ${strscr}_${fcstdate}${ensm}_ | wc -l )
        numproc=$( ps aux | awk '{ print $1 }' | grep -w $USER | wc -l )

    done 
       
    #todo:  wait for all screen sessions are done 
    if [[ "$fpst" == "${arrfpst[-1]}" ]]; then 
        blinitialnote=false
        totsec=0
        totmin=0
        sec0=$( date +%s )
        sec1=$sec0

        while (( $numscr > 0 ));do 
            sec2=$( date +%s )
            sec_diff=$(( sec2 - sec1 ))
            totsec=$(( sec2 - sec0 ))
            totmin=$( echo "$totsec / 60" | bc )

            if ! $blinitialnote;then 
                wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screens are running" #- will wait for max of $maxmin minutes"
                blinitialnote=true
            elif (( $sec_diff > 60 ));then
                wmessage "$( date +'%m/%d/%Y %H:%M' )     $( printf '%+2s\n' $numscr ) screens are running - waited for $totmin min"
                sec1=$( date +%s )
            fi
            
            numscr=$( screen -ls | grep -i detached | grep ${strscr}_${fcstdate}${ensm}_ | wc -l )
        done
    fi

done

cd - >/dev/null
#+++++ cd dexp/post ( end ) +++++

numscr=$( screen -ls | grep ${strscr}_${fcstdate}${ensm}_ | wc -l )
if (( $numscr > 0 ));then
    wmessage 
    wmessage "Running Screens:"
    wmessage "$( screen -ls | grep ${strscr}_${fcstdate}${ensm}_ )"
    wmessage 
    wmessage "Time Ended - $(TZ=America/New_York date +'%m/%d/%Y %H:%M' )"
fi

wmessage 
wmessage 

#todo:  write missng output
arrfpst_missout=($( printf '%s\n' $( findpst2run $dexp ) | xargs -i basename {} ))
arrmissout=($( findmissout $dexp )) 
wmessage "Rerun required for ${#arrfpst_missout[@]} gcm_post.*.j*:"
ahand_warr ${arrfpst_missout[@]} 
wmessage
wmessage "Missing Outputs: ${#arrmissout[@]}"
ahand_warr ${arrmissout[@]} 


exit


