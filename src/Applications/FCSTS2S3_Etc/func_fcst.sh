#!/usr/bin/env bash
primeuser=knakada
#================================================================================
#                                  bf0001_fcal
#================================================================================
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

fcal_calcfcdates(){
    #description:   get all fcst dates in the month for the input date
    #input:     YYYYMMDD or YYYYMM
    #output:    array  
    local datein=$1 
        
    local blnum=$( misc_isinteger $datein )
    (( $blnum > 0 )) && die "(${FUNCNAME[0]}) input has to be a number."

    if (( ${#datein} == 8 ));then
        local yyyymm=$( date -d $datein +%Y%m )
    elif (( ${#datein} == 6 ));then
        local yyyymm=$datein
    fi

    local nextyyyy=$( echo $( fcal_nextmonth $yyyymm ) | cut -c1-4 )
    local int_nextmon1=$( echo $( fcal_nextmonth $yyyymm ) | cut -c5-6 )
    local int_nextmon=$( echo "$int_nextmon1*1" | bc )
    local arrfcstdate=($( fcal_icall $int_nextmon $nextyyyy )) 

    echo ${arrfcstdate[@]}
}

fcal_calclasticdateofthemonth(){
    #description:   determine input date is the last icdate of the month
    #               input date format should be YYYYMMDD
    
    local datein=$1 
    local arrfcstdate=($( fcal_calcfcdates $datein ))

    local lastyyyymmdd=${arrfcstdate[-1]}
    
    if (( $lastyyyymmdd == $datein ));then
        local bl=true
    else
        local bl=false
    fi
    echo $bl 

}

fcal_all(){
    #calculate all forecast dates
    #returns an array with \"mmdd1 mmdd2 mmdd3 mmdd4 ...\"
    #
    #Usage:  ${FUNCNAME[0]} [ (optional) year ]
    #
    #Note: if you choose to enter year, this function returns
    #an array with \"yyyymmdd1 yyyymmdd2 yyyymmdd3 yyyymmdd4 ...\"
    
    local year=$1
    local month yyyymmdd
    
    if [[ -z $year ]];then
        #note:  2019 is selected because it is non-leap year. 
        local year=2019
        local bl_userinput=false
    else
        local bl_userinput=true
    fi
    local arryyyymmdd=()
    local arr=()
    for month in $( seq 1 12 );do
        arryyyymmdd+=($( fcal_icall $month $year ))
    done

    if $bl_userinput;then
        arr=($( printf '%s\n' ${arryyyymmdd[@]} | xargs -i bash -c "date -d {} +%Y%m%d" ))
    else
        arr=($( printf '%s\n' ${arryyyymmdd[@]} | xargs -i bash -c "date -d {} +%m%d"   ))
    fi

    #for yyyymmdd in ${arryyyymmdd[@]};do
    #    if $bl_userinput;then
    #        arr+=($( date -d $yyyymmdd +%Y%m%d ))
    #    else
    #        arr+=($( date -d $yyyymmdd +%m%d ))
    #    fi
    #done
    echo ${arr[@]}

}

fcal_setfmonfyr_bash(){
    #calcuate forecasting month and year based on current year and month.
    #output is YYYYMM
    #Usage:  ${FUNCNAME[0]}
    
    local current=$( date +%Y%m%d )
    local date0=$( date +%Y%m )07
    local intm=$( echo "$( date -d ${date0} +%m )*1"|bc )
    local date1=$( fcal_nextmonth $intm $( date -d $date0 +%Y ))07
    local fdate

    if (( $current <= $date0 ));then
        fdate=$date0
    elif (( $current <= $date1 ));then
        fdate=$date1
    fi

    local fyyyymm=$( date -d $fdate +%Y%m )
    echo $fyyyymm
}


fcal_isleap(){ 
    #Usage:  ${FUNCNAME[0]} [forecast year (int)]
    local _year=$1
    
    if [[ -n $1 ]];then
        local _year=$1
    else
        local _year=$( date +'%Y' )
    fi
    
    #boolean vars: 0 for true; 1 for false
    local _blleap=false

    if (( $(( _year % 4)) == 0 ));then
        _blleap=true
       
        #todo: check century leap year (happens every 400 years) 
        if (( $(( _year % 100 )) == 0 ));then
            if (( $(( _year % 400 )) == 0 ));then
                _blleap=true
            else
                _blleap=false
            fi
        fi

    fi
    echo $_blleap
    return
}

fcal_icall(){
    #Usage:  ${FUNCNAME[0]} [forecast month (int)] [forecast year (int)]"

    local fm=$1;shift
    local fyr=$1
    local ifm=$( echo "$fm*1" | bc )


    if [[ -n $fm ]] && [[ -n $fyr ]];then
        local monthprev=$(( ifm - 1 ))
        local yearprev=$fyr
        if (( $monthprev < 1 ));then
            monthprev=12
            yearprev=$(( fyr - 1 ))
        fi
    elif [[ -z $fm ]] || [[ -z $fyr ]];then
        die "(${FUNCNAME[0]}) both forecast month and forecast year are required inputs"
    fi
            
    local strymd="+%Y%m%d"
    local strs="+%s"

    local blleapyr=0

    local fdate=$( date -d $fyr-$fm-1 $strymd)
    local fdatesec=$( date -d $fyr-$fm-1 $strs )

    local icdatem=$monthprev
    local icdateyr=$yearprev
    local icstartdate=$( date -d $icdateyr-1-1 $strymd )
    local icstartdatesec=$( date -d $icdateyr-1-1 $strs )
    local icenddatesec=$( date -d $icdateyr-12-31 $strs )

    local arric=( $icstartdate )
    local cdatesec=$icstartdatesec
    local cdate=$icstartdate
    
    declare -a arricedit

    #todo:  check leap year
    blleapyr=$( fcal_isleap $icdateyr )

    #todo:  get all ic dates (every 5 days)
    while [[ $cdatesec -le $icenddatesec ]] ;do
        local cdate=$( date -d "$cdate + 5days" $strymd )
        local cdatesec=$( date -d $cdate $strs )
        if [[ $cdatesec -le $icenddatesec ]];then
            arric+=( $cdate )
        fi
    done
    
    #todo:  add one day after feb 29
    if $blleapyr ;then
        for ic in ${arric[@]};do
            local icmm=$( date -d $ic +%m )
            local icyr=$( date -d $ic +%m )
            local mar=$( date -d $icyr-3-1 +%m )
           
            if [[ $ic == ${arric[${#arric[@]}-1]} ]];then
               : 
            elif [[ ${icmm#0} -ge $mar ]];then 
                local icdateedit=$( date -d "$ic +1days" $strymd )
                arricedit+=( $icdateedit )
            else 
                arricedit+=( $ic )
            fi
        done
        arric=( ${arricedit[@]} )
    fi
    
    #todo:  icdate for forecast month & year.
    for ic in ${arric[@]};do
        local icmm=$( date -d $ic +%m )
        if (( ${icdatem#0} == ${icmm#0} ));then
            arrmdate+=( $ic )
        fi
    done
    echo "${arrmdate[@]}"
}

fcal_previousmonth(){
    #description:   calculates previous month.
    #Usage:  ${FUNCNAME[0]} [ month (int)]"

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
    local prevm=$(( imonth - 1 ))

    if $blreturnyear;then
        local prevyear=$yyyy
    fi

    if (( $prevm < 1 ));then
        prevm=12
        if $blreturnyear;then
            prevyear=$(( yyyy - 1 ))
        fi
    fi

    if $blreturnyear;then
        local output=$prevyear$( printf '%02g' $prevm )
    else
        local output=$prevm
    fi

    echo $output
}

fcal_nextmonth(){
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
            nextyear=$(( ${yyyy#0} + 1 ))
        fi
    fi

    if $blreturnyear;then
        local output=$nextyear$( printf '%02g' $nextm )
    else
        local output=$nextm
    fi

    echo $output
}

fcal_calcnummon(){
    #a function to:
    #determine a number of months between two given dates (YYYYMMDD).
    #This function is mainly used to figure out available outputs,
    #including a partial month but execlude a partial month starting from 
    #its last day, based on cap_restart, cap_restartIC, & cap_end

    #Note:   - For s2s forecast, use YYYYMMDD from cap_restartIC as a start 
    #          month, and use YYYYMMDD from cap_restart as end month.
    #        - end month is excluded from the outputs. 

    #Usage:  ${FUNCNAME[0]} [start month in YYYYMMDD] [end month in YYYYMMDD]
    #Output: [ a number of months ] 

    #i.e.) fcal_calcnummon 20200215 20200916 => output = 7 (7 months)

    local yyyymmdd1=$1;shift
    local yyyymmdd2=$1;shift
    local bllday=$1
    if (( $yyyymmdd1 >= $yyyymmdd2 ));then
        local cnt_mon=0
    else
        local arr=($( fcal_nummon $yyyymmdd1 $yyyymmdd2 ))
        local cnt_mon=${#arr[@]}
    fi
    echo $cnt_mon
}

fcal_nummon(){
    #determine  months between two given dates (YYYYMMDD).
    #This function is mainly used to figure out available outputs,
    #including a partial month but execlude a partial month starting from 
    #its last day, based on cap_restart, cap_restartIC, & cap_end

    #Note:   - For s2s forecast, use YYYYMMDD from cap_restartIC as a start 
    #          month, and use YYYYMMDD from cap_restart as end month.
    #        - end month is excluded from the outputs. 

    #Usage:  ${FUNCNAME[0]} [start month in YYYYMMDD] [end month in YYYYMMDD]
    #Output: [ an array of months ] 

    #i.e.) ${FUNCNAME[0]} 20200215 20200916 => output = 202002 202003 202004 202005 202006 202007 202008

    local yyyymmdd1=$1;shift
    local yyyymmdd2=$1;shift
    
    local yyyymm1=$( echo $yyyymmdd1 | tr -s '[:space:]' | cut -d' ' -f1 | cut -c1-6 )
    local yyyymm2=$( echo $yyyymmdd2 | tr -s '[:space:]' | cut -d' ' -f1 | cut -c1-6 )
    
    local outyyyymm1=$( misc_isinteger $yyyymm1 )
    local outyyyymm2=$( misc_isinteger $yyyymm2 )
    local arrcompyyyymm=()
    local bllday=$( fcal_lastdayofthemonth $yyyymmdd1 )
    
    if (( $outyyyymm1 > 0 )) || [[ -z $yyyymm1 ]] ;then 
        die "(${FUNCNAME[0]}) two YYYYMM inputs are required";exit
    fi
    if (( $outyyyymm2 > 0 )) || [[ -z $yyyymm2 ]] ;then 
        die "(${FUNCNAME[0]}) two YYYYMM inputs are required";exit
    fi
    if [[ -z $bllday ]];then 
        die "(${FUNCNAME[0]}) a boolean for the last day of the month is a required input";exit
    fi
    
    #todo:  if cap_restartIC date is the last day of the month, 
    #       skip adding the month 
    if $bllday;then
        local yyyy=$( date -d ${yyyymm1}01 +%Y )
        local mm=$( echo $yyyymm1 | cut -c5-6 )
        local int_mm=$( echo "$mm*1" | bc )
        yyyymm1=$( fcal_nextmonth $int_mm $yyyy )
    fi

    while [[ "$yyyymm2" != "$yyyymm1" ]];do
        arrcompyyyymm+=( $yyyymm1 )

        local yyyy=$( date -d ${yyyymm1}01 +%Y )
        local mm=$( echo $yyyymm1 | cut -c5-6 )
        local int_mm=$( echo "$mm*1" | bc )
        yyyymm1=$( fcal_nextmonth $int_mm $yyyy )
    done
    echo ${arrcompyyyymm[@]}

}

fcal_month2seas(){
    #description:   convert month in integer to seasons (i.e. djf,mam,jja,son )
    local imonth=$1

    if (( $imonth == 12 || $imonth == 1 || $imonth == 2 ));then 
        local seas=djf
    elif (( $imonth == 3 || $imonth == 4 || $imonth == 5 ));then 
        local seas=mam
    elif (( $imonth == 6 || $imonth == 7 || $imonth == 8 ));then 
        local seas=jja
    elif (( $imonth == 9 || $imonth == 10 || $imonth == 11  ));then
        local seas=son
    fi

    echo $seas
}


fcal_seasdates(){
    
    #description:   create array with all dates for a given seasons (i.e. djf,mam,jja,son )
    local yyyy=$1;shift
    local seas=$( echo $1 | tr '[:upper:]' '[:lower:]' )
    local arrdates sdate edate

    [[ -z $seas ]] && die "(${FUNCNAME[0]}) season (djf,mam,jja, or son) is a required input" 
    [[ -z $yyyy ]] && die "(${FUNCNAME[0]}) year is a required input." 

    #todo: check if yyyy is a leap year
    local blleapyr=$( fcal_isleap $yyyy )

    #todo:  determine previous year 
    #note:  this is required for djf
    if [[ $seas == "djf" ]];then 
        local pyyyy=$(( yyyy - 1 ))
    else
        local pyyyy=$yyyy
    fi

    if [[ $seas == "djf" ]];then 
        local pyyyy=$(( yyyy - 1 ))
        if $blleapyr;then
            sdate=1201; edate=0229
        else
            sdate=1201; edate=0228
        fi

    elif [[ $seas == "mam" ]];then 
        sdate=0301; edate=0531

    elif [[ $seas == "jja" ]];then 
        sdate=0601; edate=0831

    elif [[ $seas == "son" ]];then 
        sdate=0901; edate=1130
    else
        die "(${FUNCNAME[0]}) season input has to be one of djf, mam, jja, or son." 
    fi
    
    sdate=$pyyyy$sdate
    edate=$yyyy$edate

    local thisdate=$sdate
    arrdates+=( $thisdate )

    #todo:  get all dates for $seas
    while [[ "$thisdate" -ne "${edate#0}" ]]; do
        thisdate=$( date -d "$thisdate +1days" +%Y%m%d )
        arrdates+=( $thisdate )
    done 

    echo "${arrdates[@]}" 
}

fcal_seasfcstdates(){
    #calculate forecast dates in a given seasons.
    #
    # Usage: ${FUNCNAME[0]} [season (djf,mam,jja,son) ] [(optional) year]
    #Output: an array with forecast dates with a format of MMDD
    #
    #note: If year is given and season is djf, year for december will be
    #      previous year. For instance, season = djf & year = 2012, 
    #      output will be
    #           20111202
    #           20111207
    #           20111212
    #           20111217
    #           20111222
    #           20111227
    #           20120101
    #           20120106
    #           ....

    local seas=$( echo $1 | tr '[:upper:]' '[:lower:]' );shift
    local yyyy=$1
    local year=2019     #note:  2019 is selected because it is non-leap year.
    local month yyyymmdd 
    local arryyyymmdd=()
    local arr=()

    [[ $seas == "djf" ]] && local arrimonth=( 12 1 2 )
    [[ $seas == "mam" ]] && local arrimonth=( 3 4 5 )
    [[ $seas == "jja" ]] && local arrimonth=( 6 7 8 )
    [[ $seas == "son" ]] && local arrimonth=( 9 10 11)
    
    for month in ${arrimonth[@]};do
        month=$( fcal_nextmonth $month )
        arryyyymmdd+=($( fcal_icall $month $year ))
    done

    for yyyymmdd in ${arryyyymmdd[@]};do
        arr+=($( date -d $yyyymmdd +%m%d ))
    done
   
    #todo:  calculate dates with year if yyyy is user input. 
    if [[ -n $yyyy ]]; then 
        local yyyyp=$( echo "$yyyy - 1" | bc  )
        local arrout=($( printf '%s\n' ${arr[@]} | xargs -i bash -c "if [[ {} == "'"'"12"'"'"?? ]];then  echo ${yyyyp}{}; else echo ${yyyy}{};fi"  ))
        arr=( ${arrout[@]} )
    fi
    echo ${arr[@]}

}

fcal_isicdate(){
    #determine whether or not an input date is ic date
    # Usage:  ${FUNCNAME[0]} [YYYYMMDD]
    #Output:  true or false

    local indate=$1
    local indate_mmdd=$( date -d $indate +%m%d )
    local intint=$( misc_isinteger $indate )
    local intnum=${#indate}
    local blout

    #todo:  input check
    (( $intint > 0 )) && die "opt d required an integer input" 
    (( $intnum != 8 )) && die "opt d required an input with YYYYMMDD format" 

    local arr=($( printf '%s\n' $( fcal_all )  | xargs -i bash -c "date -d '${indate:0:4}{} -1days' +%m%d " | sort ))
    [[ "${arr[@]}" =~ $indate_mmdd ]] && blout=true || blout=false

    #local arr=($( printf '%s\n' $( fcal_all )  | xargs -i bash -c "date -d '${indate:0:4}{} -1days' +%Y%m%d " | sort ))
    #[[ "${arr[@]}" =~ $indate ]] && blout=true || blout=false

    echo $blout
}


fcal_numdaysinmonth(){

    #calculates number of days in a given month.
    #Usage:  ${FUNCNAME[0]} YYYYMM
    local _yyyymm=$1
    local intyyyy=$( echo $_yyyymm | cut -c1-4 | xargs -i echo "{}*1" | bc )
    local   intmm=$( echo $_yyyymm | cut -c5-6 | xargs -i echo "{}*1" | bc )

    local _result=$( numdaysinmonth $intmm $intyyyy )
    echo $_result
}

numdaysinmonth(){
    #calculates number of days in a given month.
    #Usage:  ${FUNCNAME[0]} [month (int)] [year (int)]

    local mon=$1
    local yr=$2
    local ii=$(( mon - 1 ))
    local arrNLMdays=(31 28 31 30 31 30 31 31 30 31 30 31)
    local arrLMdays=(31 29 31 30 31 30 31 31 30 31 30 31)
    
    #todo:  check leap year
    local blleapyr=$( fcal_isleap $yr )

    if $blleapyr;then
        local nummon=${arrLMdays[ii]}
    else
        local nummon=${arrNLMdays[ii]}
    fi
    echo $nummon
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

#================================================================================
#                                  bf0004_misc
#================================================================================
misc_isinteger(){
    #description:   determine if an input value is an integer
    #Usage: ${FUNCNAME[0]} [ a value ]
    #Output: 0 for true, 1 for false" 

    if [[ -z $1 ]];then die "(${FUNCNAME[0]}) require an input ";fi
    local num=$1
    local re='^[0-9]+$'
    if [[ ! $num =~ $re ]] ; then
        local out=1
    else 
        local out=0
    fi
    echo $out
}

misc_countdown(){
    #ref:   https://www.cyberciti.biz/faq/?p=146298
    #Usage: ${FUNCNAME[0]} [text add] [ start time (00:00:00) ]" 
    local _thistext=$1;shift

    IFS=:
    set -- $*
    secs=$(( ${1#0} * 3600 + ${2#0} * 60 + ${3#0} ))
    while [[ $secs -gt 0 ]];do
        sleep 1
        printf "\r$_thistext %02d:%02d:%02d" $((secs/3600)) $(( (secs/60)%60)) $((secs%60))
        secs=$(( $secs - 1 ))
        wait
    done
    echo
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

misc_indexof(){
    local i=1 S=$1; shift
    while [[ $S != $1 ]]
    do
    ((i++)); shift
        [ -z "$1" ] && { i=0; break; }
    done
    echo $i
}

#================================================================================
#                                  bf0005_fhand
#================================================================================

fhand_sftcdiv(){
    #description:
    #divide shiftc batch input file into multiple files based on a number of lines 
    #provided by user.

    #Usage : ${FUNCNAME[0]} [shiftc batch input file name] [a number of lines]
    #Output: an array with a full path of output file

    if [[ -z $1 ]] || [[ -z $2 ]];then die "require two inputs";fi
    
    local fin=$1;shift
    local numline=$1

    [[ ! -f $fin ]] && die "(${FUNCNAME[0]}) $fin does not exist. exit"
    
    #todo:  check numline is an interger or not.
    local numcheck=$( misc_isinteger $numline )
    (( $numcheck == 1 )) && die "(${FUNCNAME[0]}) $numline is not an integer. exit"

    local _finrealpath=$( realpath $fin )
    local _fname=$( basename $_finrealpath )
    local _dfin=$( dirname $_finrealpath )
    local _ftmpa=$_dfin/tmpa
    local _cnt=1
    unset _arrftmpa
    if [[ -f $_finrealpath ]];then cp -p $_finrealpath $_ftmpa;fi
    
    local _ftmpasize=$( stat --printf="%s" $_ftmpa )

    #todo:  get first numline from fout and place in *_fdiv file
    #       until ftmpa file size become 0. 
    while (( $_ftmpasize > 0 ));do
        local _cnttwodig=$( printf '%02g' $_cnt )
        local _fdiv=$_dfin/${_fname}_${numline}_$_cnttwodig
        [[ -f $_fdiv ]] && rm -f $_fdiv

        cat $_ftmpa | head -$numline > $_fdiv
        sed -i.bak '1,'$numline'd' $_ftmpa
        _arrftmpa+=( $_fdiv )
        
        _ftmpasize=$( stat --printf="%s" $_ftmpa )
        let _cnt++
    done
    
    #todo:  remove tmp files.
    if (( $_ftmpasize == 0 ));then rm -f $_ftmpa; rm -f $_ftmpa.bak; fi

    echo ${_arrftmpa[@]} 
}

fhand_sftcarch(){
    #description:
    #execute shiftc with provided batch input files. Shiftc command will wait 
    #for each execution.
    #
    #Usage : ${FUNCNAME[0]} [array of shiftc batch input files]
    #Note  : Shiftc command is executed with option d. This allows the command 
    #        to create a parent dir of archiving file.

    local arrfin=( "$@" )
    local fin
    
    [[ -z ${arrfin[@]} ]] && die "(${FUNCNAME[0]}) missing user input. exit"

    for fin in ${arrfin[@]};do
        local _status=9999

        if [[ -f $fin ]];then
            local numfarchive=$( cat $fin | wc -l )
            wmessage "$(date +'%m/%d/%Y %H:%M' ) ... archiving $numfarchive files ( $( basename $fin ) )"
            wmessage

            if [[ -n $writetofile ]] && (( $writetofile == 1 ));then
                #/usr/local/bin/shiftc --no-mail --wait --remote=fish-tcp --hosts=2 -d < $fin >> $fmessage 2>&1
                /usr/local/bin/shiftc --no-cron --no-mail --wait -d < $fin >> $fmessage 2>&1
                local _status=$?
            else
                #wmessage archive here
                #/usr/local/bin/shiftc --no-mail --wait --remote=fish-tcp --hosts=2 -d < $fin
                /usr/local/bin/shiftc --no-cron --no-mail --wait -d < $fin
                local _status=$?
            fi

            #todo:  rename fin
            if (( $_status == 0 ));then
                mv $fin ${fin}_done
            fi
            wmessage
        else
            wmessage "$( basename $fin ) does not exists. Skip archiving."
        fi
    done 

    #rm -f $dexp/archive/${strscr}_?fefiles_* 2>/dev/null

    #echo $_status
    return
}

fhand_newdir(){
    #delete and create new dir.
    #Usage: ${FUNCNAME[0]} [ single or multiple dir ]"
    local dir

    for dir in "$@";do
        if [[ -d $dir ]]; then rm -rf $dir; mkdir -p $dir;fi
        [[ ! -d $dir ]] && mkdir -p $dir
    done
    
    return
}







#================================================================================
#                                   bf0007_exp
#================================================================================
exp_checkwinner(){
    #description:   check if this experiment is an ensemble winner
    #usage: 
    
    [[ -z $1 ]] && die "(${FUNCNAME[0]}) a full experiment path is a required input"
    [[ -z $2 ]] && die "(${FUNCNAME[0]}) a full path to a ensemble winner file location is a required input"
    
    local _dexp=$1;shift
    local ddata=$1
    local blwinner=false
    local strwinner=winners_
    local fcstensm
        
    set_rstfcstdate $_dexp
    grep -w "$fcstdate $ensm" $ddata/$strwinner* >> /dev/null 2>&1
    local status_grep=$? 

    (( $status_grep == 0 )) && local blwinner=true || local blwinner=false

    #local arr_winensm=($( cat $ddata/$strwinner* | sed -s "s# #/#g" | sort -V ))
    #for thiswinner in ${arr_winensm[@]};do
    #    if [[ "$thiswinner" == "$fcstdate/$ensm" ]];then
    #        local blwinner=true
    #        break
    #    else
    #        local blwinner=false
    #    fi
    #done

    echo $blwinner
}

exp_getcollections(){
#   description:    a function to get active collections from HISTORY.rc ( ingnore commented-out collections with # )

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

exp_getcolldiurnal(){
   #description:   get collections which produce diurnal output
    local fhis2=HISTORY_2.rc

    if [[ -z $1 ]];then
        local _hist=$cdir/HISTORY.rc
    else
        local _hist=$1
    fi

    if [[ ! -f $_hist ]];then die "(${FUNCNAME[0]}) $_hist file does not  exist";fi

    #todo:  input check
    local strfrq=freq
    local intlim=240000
    local arr=()

    #todo:   fill in arrcoll in case it is empty
    local arrcoll=($( printf '%s\n' $( exp_getcollections $_hist ) | sort -V  | grep -v _1mo_glo_ 2>/dev/null ))
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

_exp_getcolldiurnal(){
   #description:   get collections which produce diurnal output
    if [[ -z $1 ]];then
        local _hist=$cdir/HISTORY.rc
    else
        local _hist=$1
    fi

    if [[ ! -f $_hist ]];then die "(${FUNCNAME[0]}) $_hist file does not  exist";fi

    #todo:  input check
    local strfrq=freq
    local intlim=240000
    local arr=()

    local arrcoll=($( exp_getcollections $_hist | grep -v _1mo_ ))

    for coll in ${arrcoll[@]};do
        local freq=$( grep $coll.$strfrq $_hist | awk -F: '{print $2}' | sed 's/^[ ]*//' | sed -e 's/,//' )
        local intfreq=$( echo "$freq*1" | bc )
        if (( $intfreq < $intlim ));then
            arr+=( $coll )
        fi
    done
    
    echo ${arr[@]}
}

exp_getcollfreq(){
    #description: get collections that produce specific frequencies HISTORY.rc ( ingnore commented-out collections with # )
    #   Enter m to get monthly producing collections
    #   Etner n to get non-monthly producing collections
    #       
    #   Usage: ${FUNCNAME[0]} [ m or n ] [full path to HISTORY.rc]
    #    NOTE: This function works mainly for s2s v3 HISTORY.rc"

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
    
    local _arrcoll=($( exp_getcollections $_hist ))
    #local _arrcollmon=($( grep $strmon $_hist | tr -s '[:space:]' | cut -d':' -f1| cut -d' ' -f2 | cut -d'.' -f1 ))
    local _arrcollmon=($( printf '%s\n' ${_arrcoll[@]} | xargs -i bash -c "grep {}.$strmon $_hist 2>/dev/null" | grep -w 1 | tr -s '[:space:]' | cut -d':' -f1| cut -d' ' -f2 | cut -d'.' -f1 ))
    
    if $blmon;then
        _arr=( ${_arrcollmon[@]} )
    else
        local _arr=($( printf '%s\n' ${_arrcoll[@]} ${_arrcollmon[@]} | sort | uniq -u ))
    fi

    echo ${_arr[@]}
}


exp_endyyyymm(){
    #description:   determine end year & month depending on exp type (i.e. noassim, odas, sio20)

    local _dexp=$1
    if [[ -z $_dexp ]];then die "(${FUNCNAME[0]}) exp dir path is a required input";fi
   
    if [[ -z $capr_yyyymm ]];then setvars $_dexp ;fi
    
    local endyyyymm

    #todo:  determine end yyyymm depending on whether or not exp is a winner.
    if $blwinner;then
        endyyyymm=$realend_yyyymm 
    elif ! $blwinner;then
        endyyyymm=$end3_yyyymm
    fi

    echo $endyyyymm
}

exp_checkhold(){
    #description:   check if there are any YYYYMM dir left and determine if runpostedit should be executed.
    #output:    true if any left or false if not
    
    local _dexp=$1
    if [[ -z $_dexp ]];then die "(${FUNCNAME[0]}) exp dir path is a required input";fi
   
    if [[ -z $capr_yyyymm ]];then setvars $_dexp ;fi
    

    local _dhold=$_dexp/holding
    #local collsst=geosgcm_sst
    #local collsrestore=geosgcm_srestore

    [[ -z $collsst ]] && die "(${FUNCNAME[0]}) collsst variable is undefined."
    [[ -z $collsrestore ]] && die "(${FUNCNAME[0]}) collsrestore variable is undefined."

    local endyyyymm=$( exp_endyyyymm $_dexp )

    #todo:  get YYYYMM dir still left in holding
    cd $_dhold
    local arrcollyyyymm=($( find * -mindepth 1 -maxdepth 1 -type d -not -empty | grep -Ev $capr_yyyymm'|'$collsst'|'$collsrestore'|'$endyyyymm ))
    cd - >/dev/null
    
    #if (( ${#arrcollyyyymm[@]} > 0 ));then local blrunpstedit=true; else local blrunpstedit=false;fi
    #echo $blrunpstedit

    echo ${arrcollyyyymm[@]}
}

exp_cdate(){
    #find current AGCM Date in a running exp. 
    #Usage: ${FUNCNAME[0]} [ a full path of exp dir ]" 
    local dexp=$1
    [[   -z $dexp ]] && die "(${FUNCNAME[0]}) a full path to a experiment are required inputs"
    [[ ! -d $dexp ]] && die "(${FUNCNAME[0]}) $dexp does not exist"
    
    local _fcapr=$dexp/cap_restart
    local  _fgrn=$dexp/gcm_run.j
    #local  _fpst=$dexp/post/gcm_post.j

    local _arrfpst_avail=( $dexp/post/gcm_post_full.j $dexp/post/gcm_post_part.j $dexp/post/gcm_post.j )
    local _fpst=($( printf '%s\n' ${_arrfpst_avail[@]} | xargs -i bash -c "[[ -f {} ]] && echo {}" 2>/dev/null | head -1 ))

    [[ ! -f $_fcapr ]] && die "(${FUNCNAME[0]}) $_fcapr does not exist"
    [[ ! -f $_fgrn  ]] && die "(${FUNCNAME[0]}) $_fgrn does not exist"
    [[ ! -f $_fpst  ]] && die "(${FUNCNAME[0]}) $_fpst does not exist"

    [[ -z $_hstshort ]] && local _hstshort=$( get_host ) 

    local caprstyyyy=$( cat $_fcapr | cut -c1-4 )

    if [[ -d $dexp ]];then 
        cd $dexp
        local jname_grn=$( pbs_jname $_fgrn )
        local jname_pst=$( pbs_jname $_fpst )

        if [[ "$_hstshort" == "dis" ]];then
            local _thiscmd=cmd_gjob_nccs
        elif [[ "$_hstshort" == "pfe" ]];then
            local _thiscmd=cmd_gjob_nas
        fi

        #todo: get number of jobs running or on queue.
        [[ -n $jname_grn ]] && local num_rgrn=$( $_thiscmd | grep -w $jname_grn | wc -l ) || local num_rgrn=0
        [[ -n $jname_pst ]] && local num_rpst=$( $_thiscmd | grep -w $jname_pst | grep -E ' R | Q ' | wc -l ) || local num_rpst=0

        local str1=$( cat cap_restart | head -1 | cut -d' ' -f1 )
        local num_out=$( find * -type f -name "*.${str1}.out" -o -name "$jname_grn.o[0-9]*" 2>/dev/null | wc -l ) 
        local pbskoed=$( grep "\-k oed"  gcm_run.j 2>/dev/null; echo $? )

        #todo:  find stdout file if gcm_run.j is currently running.
        if (( $num_rgrn == 1 ));then

            if [[ "$_hstshort" == "dis" ]];then
                local jobid=$( $_thiscmd | grep -w $jname_grn | tr -s '[:space:]' | sed 's#^ *##g'| cut -d' ' -f1 )
            elif [[ "$_hstshort" == "pfe" ]];then
                local jobid=$( $_thiscmd | grep -w $jname_grn | cut -d' ' -f1 )
            fi
                
            local fout=$( find * -type f -name "${jname_grn}.o$jobid" )

            #if (( $pbskoed == 0 )) || [[ "$_hstshort" == "dis" ]];then
            #    local fout=$( find * -name "${jname_grn}.o$jobid" )
            #else
            #    local fout=$( find * -name "*$jobid*.out" )
            #fi

            if [[ -n $fout ]];then 
                local str3=$( grep -i "agcm date" $fout | tail -1 )
            else
                local str3=
            fi

        elif (( $num_out > 0 ));then
                
            local fout=$( find * -type f -name "${jname_grn}.o*" -print0 | xargs -r -0 ls -1 -t | head -1 )

            #if (( $pbskoed == 0 )) || [[ "$_hstshort" == "dis" ]] ;then
            #    local fout=$( find * -type f -name "${jname_grn}.o*" -print0 | xargs -r -0 ls -1 -t | head -1 )
            #else
            #    local fout=$( find * -type f -name '*.'${str1}'.out' -print0 | xargs -r -0 ls -1 -t | head -1 )
            #fi

            if [[ -n $fout ]];then 
                local str3=$( grep -i "agcm date" $fout | tail -1 )
            else
                local str3=
            fi
        fi        

        #str1=$( cat cap_restart | head -1 | cut -d' ' -f1 | xargs -I {} bash -c 'find * -name "*.{}.out"' | tail -1 | xargs -I {} bash -c 'grep -i "agcm date" {} | tail -1' >&2 )
        if [[ -n $str3 ]];then 
            local str2="$( echo "$str3" | tr -d '\n' | tr -d '\t' | tr -d '\r' )"
            local str2="$str2 (cap_restart year: $caprstyyyy )"
        else
            local str2="(cap_restart year: $caprstyyyy )"
        fi

        cd - >/dev/null
    fi

    echo "$str2"
}

exp_listdayoutput(){
    #description:   write outputs on arc host with size and timestamps
    local str=$1;shift
    local strfreq=$1;shift
    local fnum_calc=$1;shift
    local arr=( "$@" )
    local ftmp=tmp_${strscr}_${FUNCNAME[0]}
    if [[ -f $ftmp ]];then rm -f $ftmp;fi
    
    [[ -z $hstarc ]] && die "(${FUNCNAME[0]}) var undefined: hstarc"

    wmessage "$str $strfreq output on $hstarc (total=${#arr[@]}) ..."
    for f in ${arr[@]};do
        #local fsize=$( numfmt --to=iec --format="%.2f" --padding=10 $( echo $f | cut -d':' -f1 ))
        local fsize=$( numfmt --to=iec --padding=10 $( echo $f | cut -d':' -f1 ))
        local fdate=$( date -d @$( echo $f | cut -d':' -f2 ) +%Y%m%d_%H%M )
        local fname=$( echo $f | cut -d':' -f3 )
        wtof $ftmp "  $fsize $fdate $fname"
    done 
    #todo:  check file numbers. 1 is for missing files, 0 means coll has all files. 
    if (( ${#arr[@]} != $fnum_calc ));then 
        local status_day=1 
    else 
        local status_day=0
    fi

    if (( $writetofile == 1 )) && [[ -f $ftmp ]];then
        sort -k3 $ftmp >> $fmessage 
    elif [[ -f $ftmp ]];then
        sort -k3 $ftmp >&2
    fi

    if [[ -f $ftmp ]];then rm -f $ftmp;fi

    wmessage
    echo $status_day
}

exp_calccollfout(){
    #description:   calculate a number of outputs for each monthly, daily and diurnal that should
    #               be available.
    local _bldiu=$1;shift
    local _blfrq3=$1
    local bldiu numfmon_calc numfday_calc numfdiu_calc numfcoll_calc

    #todo:  calculate a number of monthly files that supposedly exsits
    #if $bllastdayofthemonth;then
    if $_blfrq3;then
        numfmon_calc=$nummon_run
        numfmon_calc=$(( numfmon_calc + 1 ))
    else
        numfmon_calc=$nummon_run
    fi

    #todo:  a number of daily is the same as monthly
    numfday_calc=$numfmon_calc
    
    #todo:  calculate a number of diurnal files that supposedly exsits
    if $_bldiu;then 
        #if $bllastdayofthemonth;then
        if $_blfrq3;then
            numfdiu_calc=$nummon_run 
            numfdiu_calc=$(( $numfdiu_calc + 1 ))
        else
            numfdiu_calc=$nummon_run 
        fi
    else
        numfdiu_calc=0
    fi

    #todo:  total number of files
    local numfcoll_calc=$( echo "$numfmon_calc + $numfday_calc + $numfdiu_calc" | bc -l )

    #todo:  create a associative array
    local strfreq=monthly; assoc_numfcalc[$strfreq]=$numfmon_calc; 
    local strfreq=daily;   assoc_numfcalc[$strfreq]=$numfday_calc;
    local strfreq=diurnal; assoc_numfcalc[$strfreq]=$numfdiu_calc;
    local strfreq=all;     assoc_numfcalc[$strfreq]=$numfcoll_calc;

    return
}

exp_lfe_getmissout(){
    local _dexp=$1;shift
    local _flfefile=$1
    local _fhis2=$_dexp/HISTORY_2.rc

    [[ -z $hstarc ]] && die "(${FUNCNAME[0]}) var undefined: hstarc"

    local arrcoll=($( exp_getcollections $_fhis2 ))
    local arrcolldiu=($( exp_getcolldiurnal $_fhis2))
    local arrcoll_frq3=($( grep freq $_fhis2 | grep 030000 | tr -s '[:space:]' | sed 's/^[ ]*//' | cut -d'.' -f1 )) 
    local numfarchcoll bldiu blfrq3 numfarchcoll_mon numfarchcoll_day numfarchcoll_diu strfreq coll
    declare -A assoc_numfarch 
    declare -A assoc_numfcalc

    for coll in ${arrcoll[@]};do 
       
        #todo:  count number of files in a collection 
        numfarchcoll=$( cat $_flfefile | grep -v restarts | grep "/${coll}/" |  wc -l )
        
        #todo:  check if coll is diurnal producing collection 
        if [[ "${arrcolldiu[@]}" =~ "$coll" ]];then bldiu=true;else bldiu=false;fi
        if [[ "${arrcoll_frq3[@]}" =~ "$coll" ]];then blfrq3=true;else blfrq3=false;fi

        #todo:  calculate a number of outputs for each monthly, daily and diurnal that should
        #       be available.
        exp_calccollfout $bldiu $blfrq3
        local numfcoll_calc=${assoc_numfcalc[all]}
       
        if (( $numfarchcoll != $numfcoll_calc ));then
            #todo:  count a number of outputs currently available on arc host
            numfarchcoll_mon=$( cat $_flfefile | grep -v restarts | grep $coll.monthly |  wc -l )
            numfarchcoll_day=$( cat $_flfefile | grep -v restarts | grep $coll.daily |  wc -l )

            if $bldu;then
                numfarchcoll_diu=$( cat $_flfefile | grep -v restarts | grep $coll.diurnal |  wc -l )
            else 
                numfarchcoll_diu=0
            fi
            
            #todo:  print out the rest of output files for missing frequency.
            strfreq=monthly; assoc_numfarch[$strfreq]=$numfarchcoll_mon 
            strfreq=daily;   assoc_numfarch[$strfreq]=$numfarchcoll_day
            strfreq=diurnal; assoc_numfarch[$strfreq]=$numfarchcoll_diu
            
            for strfreq in "${!assoc_numfarch[@]}"; do
                local numfarch=${assoc_numfarch[$strfreq]}
                local numfcalc=${assoc_numfcalc[$strfreq]}

                if (( $numfcalc != $numfarch ));then
                    local arrfcoll=($( grep $coll.$strfreq $_flfefile | sort -k2 | sed 's/ /:/g' ))
                    local status_proc=$( exp_listdayoutput $coll $strfreq $numfcalc "${arrfcoll[@]}" )
                fi
            done
        fi
    done
    return
}

exp_filter(){
    #description: check if experiment exists or not. 
    local dexp=$1
    local parent=$( echo $dexp | cut -d'/' -f2 )
    local blexist=false
    local hst 
    
    if [[ $parent == u ]];then hst=lfe;fi
    
    if [[ -z $hst ]];then
        if [[ -d $dexp ]];then blexist=true; fi

    elif [[ -n $hst ]];then
        ssh -q $hst test -d $dexp
        if (( $? == 0 ));then blexist=true;fi
    fi

    echo $blexist
}

exp_editqname(){

    #local _usage="a function to:
    #   edit queuename in gcm*.j file. gcm*.j.bak is created as backup.
    #   In order for this function to work, a full pbs select statement has to be
    #   provided. ie: #PBS -l select=1:ncpus=20:mpiprocs=20:model=ivy:aoe=toss
    #   Usage: ${FUNCNAME[0]} [a full patht to gcm*.j file] [queue name] [PBS select statement]"
    
    local _fin=$1;shift
    local namqueue=$1;shift
    local sel_new="$1"

    [[ -z $_fin     ]] && die "(${FUNCNAME[0]}) a full path to gcm*.j file is an required input"
    [[ -z $namqueue ]] && die "(${FUNCNAME[0]}) amount of time is an required input"
    
    local _bl_success=1
    local nameqnew="#PBS -q $namqueue"

    local nameqold1=$( grep -n "#PBS -q" $_fin | grep -v "##" | head -1  )
    local numl_q=$( echo $nameqold1 | cut -d':' -f1 )
    local nameqold=$( echo $nameqold1 | cut -d':' -f2 )

    local selold1=$( grep -n "#PBS -l select=" $_fin | grep -v "##" | head -1  )
    local numl_sel=$( echo $selold1 | cut -d':' -f1 )
    local selold=$( echo $selold1 | cut -d':' -f2- )
    sed -i.org -e "${numl_q}s/${nameqold}/${nameqnew}/g" -e "${numl_sel}s/${selold}/${sel_new}/g" $_fin
    (( $? == 0 )) && _bl_success=0
    
    return $_bl_success 
}


#================================================================================
#                                   bf0008_pbs
#================================================================================
pbs_jname(){
    #description:   to get a job name from gcm_*.j. 
    local _f=$1

    [[ -z $_f ]] && die "a name of gcm_*.j file is a required input" && exit
    [[ -z $thishst ]] && local thishst=$( get_host )

    if [[ $thishst == "pfe" ]];then
        local _vout=$( grep -i pbs $_f |grep -v "##"| grep "\-N" | head -1 | tr -s '[:space:]' | rev | cut -d' ' -f1 | rev )
    elif [[ $thishst == "dis" ]];then
        local _vout=$( grep -i sbatch $_f | grep -v "##" | grep "\--job-name"  | tr -s '[:space:]' | rev | cut -d'=' -f1 | rev )
    fi

    echo $_vout
}

pbs_qname(){
    local _f=$1

    #${FUNCNAME[0]} is to get a queue name from gcm_*.j. 
    #Usage: ${FUNCNAME[0]} [ name of gcm_*.j script ]
    if [[ -z $_f ]] ;then die "a name of gcm_*.j file is a required input";fi

    local _vout=$( grep -i pbs $_f | grep -v "##" | grep "\-q" | head -1 | cut -d' ' -f3 )
    echo $_vout
}

pbs_mname(){
    local _f=$1

    #local _usage="    ${FUNCNAME[0]} is to get a model name from gcm_*.j. 
    #Usage: ${FUNCNAME[0]} [ name of gcm_*.j script ]"

    if [[ -z $_f ]] ;then die "a name of gcm_*.j file is a required input";exit;fi

    #ref:https://superuser.com/questions/1001973/bash-find-string-index-position-of-substring
    local _text="$( grep -i pbs $_f |grep -v "##"| grep model | head -1 | tr -s '[:space:]'  )"
    local search=model
    local prefix=${_text%%$search*}
    local numprefix=${#prefix}
    local _vout=$( echo ${_text:$numprefix} |cut -d'=' -f2 |cut -d':' -f1 )

    echo $_vout
}


pbs_mcmd(){
    local _f=$1

    #local _usage="    ${FUNCNAME[0]} is to get pbs command with model name from gcm_*.j. 
    #Usage: ${FUNCNAME[0]} [ name of gcm_*.j script ]" 
    if [[ -z $_f ]] ;then die "a name of gcm_*.j file is a required input";fi

    local _vout=$( grep -i pbs $_f | grep -v "##" | grep model | head -1 )
    echo $_vout
}

pbs_qcmd(){
    local _f=$1

    #local _usage="    ${FUNCNAME[0]} is to get pbs command with queue name from gcm_*.j. 
    #Usage: ${FUNCNAME[0]} [ name of gcm_*.j script ]" 
    if [[ -z $_f ]] ;then die "a name of gcm_*.j file is a required input";fi
    local _vout=$( grep -i pbs $_f | grep -v "##" |  grep "\-q" | head -1 )

    echo $_vout
}

pbs_wtime(){
    local _f=$1

    #local _usage="    ${FUNCNAME[0]} is to get walltime in from gcm_*.j. 
    #Usage: ${FUNCNAME[0]} [ name of gcm_*.j script ]"

    if [[ -z $_f ]] ;then die "a name of gcm_*.j file is a required input";exit;fi
    local strsearch=walltime
    local _vout=$(  grep -i $strsearch $_f | grep -v "##" | rev | cut -d'=' -f1 | rev )
    echo $_vout
}




qstat_getids(){
    #description:   get all job ids.
    #Usage: ${FUNCNAME[0]} [ status of job (R, Q; ignored if empty ) ] [ A name of dir (post or plot)]"

    local _str_status=$1
    local _str_dir=$2
    local _arr_id=()
    local _arr_fstdout=()

    if [[ -z $_str_dir ]] ;then die "a name of directory (post or plot) is a required input";fi

    #todo:  input check
    if [[ -z $_str_status ]];then
        local _arr_allid=($( /u/scicon/tools/bin/qstat -e -u $USER -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep $USER | cut -d' ' -f1 )) 
    else
        local _arr_allid=($( /u/scicon/tools/bin/qstat -e -u $USER -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep -w $_str_status | cut -d' ' -f1 )) 
    fi

    for _x in ${_arr_allid[@]};do
        local _fstdout=$( /u/scicon/tools/bin/qstat -f $_x | grep Output_Path | cut -d'=' -f2 | cut -d'/' -f5- )

        if [[ -n $_str_dir ]] && [[ $_fstdout == *"/${_str_dir}/"* ]];then
            _fstdout=$( basename $_fstdout )
            _arr_id+=( $_x $_fstdout) 
        elif [[ -z $_str_dir ]];then
            _fstdout=$( basename $_fstdout )
            _arr_id+=( $_x $_fstdout) 
        fi
    done

    #ref:   https://is.gd/wKnXOL
    #paste <(printf "%s\n" "${_arr_id[@]}") <(printf "%s\n" "${_arr_fstdout[@]}")
    printf "%s %s\n" "${_arr_id[@]}"
    return
}

qstat_pstid(){
    #description:  get job ids for all gcm-post. 
    qstat_getids "" "post"
    return
}

res_name(){
    #get reserved node name.
    #Usage: ${FUNCNAME[0]} [node id]" 

    local _id=$1
    local _vout=$( /PBS/bin/pbs_rstat -f $_id | grep -e Reserve_Name | cut -d'=' -f2)
    echo $_vout
}

res_mname(){
    #get model namd based on reservation id (i.e. sky_ele)
    #Usage: ${FUNCNAME[0]} [node id]" 
    local _id=$1
    local tmp_str1=$( /PBS/bin/pbs_rstat -f $_id | grep "Resource_List.select" )
    local _vout=$( echo ${tmp_str1#*model=} | cut -d':' -f1 )

    #local tmp_str2=$( echo $tmp_str1 | xargs -i awk -v a={} -v b="model" 'BEGIN{print index(a,b)}' )
    #local tmp_str3=$( echo $tmp_str1 | cut -c"$tmp_str2"- | xargs -i awk -v a={} -v b=":" 'BEGIN{print index(a,b)}' )
    #local tmp_num1=$( echo "${tmp_str2}+${tmp_str3}-2" | bc -l)
    #(( $tmp_str2 < $tmp_num1 )) && local _vout=$(  echo $tmp_str1 | cut -c"$tmp_str2"-"$tmp_num1" | cut -d'=' -f2 )

    echo $_vout
}

res_nodect(){
    #descriptioin:  get a number of nodes for a reservation
    local _id=$1
    local _str_reslist=Resource_List.nodect

    if [[ -z $_id ]];then
        die "(${FUNCNAME[0]}) reserved node id is a required input"
    fi
    local _n=$( /PBS/bin/pbs_rstat -f $_id | grep $_str_reslist | cut -d' ' -f3 )
    echo $_n
}

res_resname(){
    #description get a reserve name (i.e. g0609_xxx)

    local _id=$1
    local _str=Reserve_Name

    if [[ -z $_id ]];then
        die "(${FUNCNAME[0]}) reserved node id is a required input"
    fi
    local _out=$( /PBS/bin/pbs_rstat -f $_id | grep $_str | cut -d' ' -f3 )
    echo $_out
}

res_enddate(){
    #description:   get date and time that reservation ends
    local _id=$1
    local out=$( /PBS/bin/pbs_rstat $_id | tail -1 | rev | cut -d'/' -f1 | rev | sed 's/^[ ]*//' )
    echo $out
}

res_qid_all(){
    #get all reserved node ids 
    #Usage: ${FUNCNAME[0]} " 
    
    local arr=()
    local qid

    #local _arr=($( /PBS/bin/pbs_rstat | grep $USER | cut -d" " -f2 ))
    #local arrqid=($( /PBS/bin/pbs_rstat | grep -e $USER -e $primeuser | cut -d" " -f2 ))
    local arrqid=($( /PBS/bin/pbs_rstat  | tail -n +3 | tr -s '[:space:]' |cut -d' ' -f2 ))

    for qid in ${arrqid[@]};do
        local strauthusers=$( /PBS/bin/pbs_rstat -f $qid | grep Authorized_Users | cut -d'=' -f2  )
        if [[ "$strauthusers" =~ $USER ]];then
            arr+=( $qid )
        fi
    done

    echo ${arr[@]}
}

res_Rqid(){
    #description:   get all running reserved node id 
    #Usage: ${FUNCNAME[0]} " 

    #local _arr=($( /PBS/bin/pbs_rstat | grep -e $USER -e $primeuser | grep -w RN |tr -s '[:space:]' |cut -d' ' -f2 ))
    local _arr=($( /PBS/bin/pbs_rstat | grep g0609 | grep -w RN |tr -s '[:space:]' |cut -d' ' -f2 ))
    echo ${_arr[@]}
}

res_qidexpire(){
    #description:   get qids expiring within a given hours
    #Usage : ${FUNCNAME[0] [# of hours]

    local _hours=$1
    [[ -z $_hours ]] && die "(${FUNCNAME[0]}) a number of hours is required input."

    local _arr=($( res_Rqid ))
    local _arrout=()
    local strresend=reserve_end
    local sec_hrs=$( echo "3600 * $_hours" | bc )
    local qidavail
        
    local _now=$( date +%s )
    local nowp8hrs=$( echo "$_now + $sec_hrs" | bc )

    for qidavail in ${_arr[@]};do
        local sec_end=$( /PBS/bin/pbs_rstat -f $qidavail | grep $strresend | rev | cut -d'=' -f1 | rev | xargs -i date -d {} +%s )
        (( $sec_end <= $nowp8hrs )) && _arrout+=( $qidavail )
    done 
    echo ${_arrout[@]}
}

res_qidstart(){
    #description:   get qids starting within a given hours
    #Usage : ${FUNCNAME[0] [# of hours]

    local _hours=$1
    [[ -z $_hours ]] && die "(${FUNCNAME[0]}) a number of hours is required input."

    local _arr=($( res_qid_all ))
    local _arrout=()
    local strresbeg=reserve_start
    local sec_hrs=$( echo "3600 * $_hours" | bc )
    local qidavail
        
    local _now=$( date +%s )
    local nowp8hrs=$( echo "$_now + $sec_hrs" | bc )

    for qidavail in ${_arr[@]};do
        local sec_beg=$( /PBS/bin/pbs_rstat -f $qidavail | grep $strresbeg | rev | cut -d'=' -f1 | rev | xargs -i date -d {} +%s )
        (( $sec_beg <= $nowp8hrs )) && (( $sec_beg > $_now )) && _arrout+=( $qidavail )
    done 
    echo ${_arrout[@]}
}




#================================================================================
#                                   bf0011_msg
#================================================================================
msg_makehead(){
    local _num_repeat=$1
    local _str_char="$2"
     _str_head="$2"
    local i=1;
    while [[ i -lt $_num_repeat ]];do _str_head=${_str_head}${_str_char}; let i++ ;done
    return   
}

msg_wheader(){
    #msg_makehead 80 "="
    local _str_head=$( printf '=%.0s' {1..80} )

    if [[ -z $1 ]];then 
        local _str_subject="$(TZ=America/New_York date +'%m/%d/%Y %H:%M' )"
    else 
        local _str_subject="$1"
    fi

    local _len=${#_str_subject}
    local _len_head=${#_str_head}
    local _num_spaces=$( echo "$_len_head/2 + $_len/2" | bc  )

    wmessage "$_str_head"
    wmessage "$( printf '%*s\n' ${_num_spaces#0} "${_str_subject}" )"
    wmessage "$_str_head"
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

msg_wheader_thisfile(){
    
    #Usage: ${FUNCNAME[0]} [ a file name ]

    if [[ -n $1 ]];then 
        local fout=$1
        [[ ! -f $fout ]] && touch $fout
    else
        die "(${FUNCNAME[0]}) a file name is required input."
    fi

    #msg_makehead 80 "="
    local _str_head=$( printf -- "$2%.0s" $( seq 1 $1 ) )

    local _str_subject="$(TZ=America/New_York date +'%m/%d/%Y %H:%M' )"
    local _len=${#_str_subject}
    local _len_head=${#_str_head}
    local _num_spaces=$( echo "$_len_head/2 + $_len/2" | bc  )

    wtof $fout "$_str_head"
    wtof $fout "$( printf '%*s\n' ${_num_spaces#0} "${_str_subject}" )"
    wtof $fout "$_str_head"

    return
}

msg_newfile(){
   #delete old file and create new.
   #Usage: ${FUNCNAME[0]} [ single or multiple filenames ]"
 
   for f in "$@";do
       [[ -f $f ]] && rm -f $f && touch $f
       [[ ! -f $f ]] && touch $f
   done
   
   return
}

die(){
    #index number (note: index is starting from 1, not 0)
    #If var of interest doesn't exist in the given array
    #return 0.
    [[ -z $writetofile ]] && local  writetofile=0

    if (( $writetofile == 1 ));then
        if [[ -z $fmessage ]];then 
            [[ -n $strscr ]] && local fmessage=${strscr}_message1 || local fmessage=message1
        fi

        if [[ ! -f $fmessage ]];then 
            touch $fmessage
            msg_wheader
        fi

        echo "ERROR: $* Abort." >> $fmessage
    else
        echo "ERROR: $* Aborting." >&2
    fi

    kill -INT $$ 2>/dev/null
    return
}

wmessage(){
    #note:  this function is different from original
    if [[ -z $writetofile ]];then writetofile=0;fi

    if (( $writetofile == 1 ));then
        if [[ -z $fmessage ]] ;then 
            [[ -n $strscr ]] && fmessage=${strscr}_message1 || fmessage=message1
        fi
        echo "$*" >> $fmessage
    else
        echo "$*" >&2
    fi
    return
}

wtof(){
    #description:   write output text into a specified file by user
    #Usage: ${FUNCNAME[0]} [a filename or a full path to a file if]

    local _fout=$1

    if [[ -z $_fout ]];then die "a file name is reuired input";exit;fi
    if [[ ! -f $_fout ]]; then touch $_fout; fi
    if [[ ! -f $_fout ]];then die "$_fout doesn't exist";exit;fi

    #ref:https://is.gd/MuHBUm
    echo "${@:2}" >> "$_fout"
    return
}

smessage(){
    #send an email with a content of message_* file
    #Usage: ${FUNCNAME[0]} [email addresses]
    #
    #example for muptiple email addresses- 
    #    smessage "'"'"xxx@nasa.gov,yyy@nasa.gov"'"'"
    #
    #Available global variables: msg_hrinc & msg_hrpref (set them in your script)
    #    msg_hrinc  - hour increments 
    #    msg_hrpref - prefered hour to send (i.e. 6 means email sent at 6am)
    #
    #    example: msg_hrinc=8 and msg_hrpref=6
    #             an email will be sent at 6am, 2pm, and 10pm (8 hour increment).

    #todo: send email with last $msg_hrinc hours of results.
    local _chr=$( date +'%k' )
    local eadds="$1"
    local blsend=false
            
    [[ -z "$eadds" ]] && die "(${FUNCNAME[0]}) email address is a required input" && exit
    [[ -z $msg_hrinc ]] && msg_hrinc=0

    if (( $msg_hrinc > 0 )) ;then
        #note:  msg_hrinc & msg_hrpref can be set in a script. If these are undefined, 
        #       use default values, msg_hrinc=0 & msg_hrpref=msg_hrinc.
        #note2: msg_hrinc is replacing lastm
        if [[ -z $msg_hrpref ]];then 
            msg_hrpref=0
            local minhr=0
        else
            local minhr=$( seq $msg_hrpref -$msg_hrinc 0 | tail -1 )
        fi

        local _arr_hrs=($(seq $minhr $msg_hrinc 23))
        local int_ind=$( IndexOf $_chr ${_arr_hrs[@]} )

        if (( $int_ind > 0 ));then
            blsend=true
        else
            blsend=false
        fi

    elif (( $msg_hrinc == 0 )) || [[ -z $msg_hrinc ]] ;then
        blsend=true
    fi

    #todo:  send email
    if $blsend ;then
        [[ -f $fstderr ]] && cat $fstderr >> $fmessage
        [[ -n "$eadds" ]] && mrpostman "$eadds"
        [[ -f $fstderr ]] && msg_newfile $fstderr
        msg_newfile $fmessage
    fi

    return
}

mrpostman(){
    #send an email to a provided email address(es). 
    #Usage: ${FUNCNAME[0]} [ email addresses ] [ a file name for email contents (optional)]"

    local eadds="$1";shift
    local message="$1"
    
    if [[ -z $msg_subject ]];then
        if [[ -z $str_script ]];then 
            msg_subject="NO TITLE" 
        else
            msg_subject=$str_script 
        fi
    fi

        [[ -z "$eadds" ]] && die "(${FUNCNAME[0]}) require email address"
      [[ -z "$message" ]] && message=$fmessage
    [[ ! -f "$message" ]] && die "(${FUNCNAME[0]}) $message does not exist"
    
    if [[ -z $exp ]];then
        mail -s "$msg_subject" "$eadds" < $message &
    else
        mail -s "$msg_subject: $exp" "$eadds" < $message &
    fi
    return
}

msg_cyberpostman(){
    #description:   send email with monospace font
    #Usage: ${FUNCNAME[0]} [email subject] [ email addresses ] [a file name for text body] [true to use mutt]
    local _subject=$1;shift
    local _eadds=$1;shift
    local _fbody=$1;shift
    local _blusemutt=$1
    #local _chhmm=$1
    local limmm=6
    local thishst=$( get_host )
    local writetofile=1
    local fmessage=${strscr}_${FUNCNAME[0]}
    local mhead=mailheader
    local mfoot=mailfooter
    local blsend=false

    [[ -z $_blusemutt ]] && _blusemutt=false

    #if [[ -n $_chhmm ]];then 
    #    local _chr=$( echo "$( echo $_chhmm | cut -c1-2 ) * 1" | bc )
    #    local _cmm=$( echo "$( echo $_chhmm | cut -c3-4 ) * 1" | bc )
    #fi
    #[[ -z $_chr ]] && local _chr=$( date +'%-k' )
    #[[ -z $_cmm ]] && local _cmm=$( date +'%-M' )
    
    local _chr=$( date +'%-k' )
    local _cmm=$( date +'%-M' )

    [[ -z "$_eadds"  ]] && die "(${FUNCNAME[0]}) email address is a required input" && exit
    [[ -z $msg_hrinc ]] && msg_hrinc=0
    
    if (( $msg_hrinc > 0 )) ;then
        #note:  msg_hrinc & msg_hrpref can be set in a script. If these are undefined, 
        #       use default values, msg_hrinc=0 & msg_hrpref=msg_hrinc.
        #note2: msg_hrinc is replacing lastm
        if [[ -z $msg_hrpref ]];then 
            msg_hrpref=0
            local minhr=0
        else
            local minhr=$( seq $msg_hrpref -$msg_hrinc 0 | tail -1 )
        fi

        local _arr_hrs=($( seq $minhr $msg_hrinc 23 ))
        local int_ind=$( IndexOf $_chr ${_arr_hrs[@]} )

        #todo:  check hour integer is one of prefered hour and the current time is within the first 5 min
        #       of the hour.  
        if (( $int_ind > 0 && $_cmm < $limmm ));then
            blsend=true
        else
            blsend=false
        fi

    elif (( $msg_hrinc == 0 )) || [[ -z $msg_hrinc ]] ;then
        blsend=true
    fi

    #todo:  get host location 
    if [[ "$thishst" == "lfe" || "$thishst" == "pfe" ]];then
        local hstloc=nas
    elif [[ $thishst == "dis" ]];then
        #die "(${FUNCNAME[0]}) this function does not work on discover"
        local hstloc=dis
    fi

    if $blsend;then
        if $_blusemutt;then 
            mutt -s "$_subject" "$_eadds" < $_fbody
            local status_send=$?
        else
            msg_newfile $fmessage

            wmessage "From: $USER@$hstloc.nasa.gov>"
            wmessage "To: $_eadds"
            wmessage "Subject: $_subject"

            if [[ -f $mhead || -f $mfoot ]];then
                cat $mhead $_fbody $mfoot >>  $fmessage 2>&1 
            else
                cat $_fbody >> $fmessage 2>&1 
            fi 
            
            cat $fmessage | /usr/sbin/sendmail -t
            local status_send=$?
        fi
        
        #note:  this fmessage is NOT a fmessage in a script which is calling this function
        [[ -f $fmessage ]] && rm -f $fmessage
    else
        local status_send=1
    fi

    return $status_send
}

#================================================================================
#                                  bf0013_ahand
#================================================================================
ahand_warr(){
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

ahand_warrcol(){
    local _usage="a function to:
    print all elements in a given array in a column form
    Usage:  ${FUNCNAME[0]} [ array ]" 

    local OPTIND _option h
    while getopts 'h' _option;do
        case "$_option" in 
            h) echo "$_usage" >&2; return 0;;
        esac
    done
    
    local _arr=( "$@" )
    local saveIFS=$IFS

    IFS=$'\n'
    if [[ -z $writetofile ]] || (( $writetofile == 0 )) ;then
        echo "${_arr[@]}" | column >&2

    elif (( $writetofile == 1 ));then
        echo "${_arr[@]}" | column >> $fmessage 2>&1
    fi
    
    IFS=$saveIFS
    return
}

#================================================================================
#                                 util functions
#================================================================================
setdir_fcst_pfe(){
    #description:   set vars for common used dir
    DFCST=/nobackupp11/gmaofcst/GEOSS2S3/GEOS_fcst
     DRST=/nobackupp13/gmaofcst/GEOSS2S3/GEOS_rst
    DARCH=/u/$USER/GEOSS2S3/GEOS_fcst
    return
 }
     
setdir_fcst_dis(){
    DARCH=/archive/$USER/GEOS_S2Sv3
    return
}

setdir_exp_pfe(){    
    DFCST=/nobackupp11/gmaofcst/GEOSS2S3/GEOS_exp/knakada_s2s3_unstable_11252019
     DRST=/nobackupp13/gmaofcst/GEOSS2S3/GEOS_rst
    DARCH=/u/gmaofcst/GEOSS2S3/GEOS_exp/testfcst/$strexp
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

set_rstfcstdate(){
    local _dexp=$1

    [[ -z $_dexp ]] && wmessage "(${FUNCNAME[0]}) input missing"

    ensm=$( echo $_dexp | rev | cut -d'/' -f1 | rev )
    local expdate=$( echo $_dexp | rev | cut -d'/' -f2 | rev )
    local expyyyy=$( date -d $expdate +%Y )
    #ref: https://stackoverflow.com/a/15149278
    local ens=${ensm#*ens}
    intens=${ensm#*ens}

    fcstdate=$expdate
    fcstyyyy=$expyyyy
    rstdate=$( date -d "$fcstdate -1days" +%Y%m%d )

    return
}

setvars(){
    #description:    set various variables
    local _dexp=$1
    
    [[ -z $_dexp ]] && die "(${FUNCNAME[0]}) a full path to an experiment dir is required input"
    
    local fcap=CAP.rc
    local fcapr=cap_restart
    local fcapric=cap_restartIC
    local fcapend=cap_end
    local fhis2=HISTORY_2.rc
    local fhis1=HISTORY_1.rc

    local _fcap=$_dexp/$fcap
    local _fcapr=$_dexp/$fcapr
    local _fcapric=$_dexp/$fcapric
    local _fcapend=$_dexp/$fcapend
    local _fhis1=$_dexp/$fhis1
    local _fhis2=$_dexp/$fhis2
    local strmapl="01_0000z.nc4"
    local coll
    
    arrfsave=()
    arrfname=()
    strrst=restarts
    strmom=MOM_Output
    strmom_search=ocean_daily
    strscrach=scratch
    strseg=JOB_SGMT

    fcstdate=$( echo $_dexp | rev | cut -d'/' -f2 | rev )
    ensm=$( echo $_dexp | rev | cut -d'/' -f1 | rev )

    local _seg_y=$( grep -iw $strseg $_fcap | tr -s '[:space:]' | cut -d' ' -f2 | cut -c1-4 | xargs -i bash -c ' echo "{} * 1" | bc' ) 
    local _seg_m=$( grep -iw $strseg $_fcap | tr -s '[:space:]' | cut -d' ' -f2 | cut -c5-6 | xargs -i bash -c ' echo "{} * 1" | bc' ) 
    local _seg_d=$( grep -iw $strseg $_fcap | tr -s '[:space:]' | cut -d' ' -f2 | cut -c7-8 | xargs -i bash -c ' echo "{} * 1" | bc' ) 

    #note:  collsst should be set in srcme_* files. If not, use this.
    [[ -z $collsst ]] && local collsst=sst_tavg_1dy_glo_L720x361_slv

    [[ -f $_fhis1 ]] && arrcollnonmon_sstincl=($( exp_getcollfreq n $_fhis1 ))
    
    #note:  cap_restart vars
    if [[ -f $_fcapr ]];then
        capr_yyyymmdd=$( cat $_fcapr | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
        capr_yyyymm=$( echo $capr_yyyymmdd | cut -c1-6 )
        capr_yyyy=$( echo $capr_yyyymmdd | cut -c1-4 )
        capr_yyyymmdd_insec=$( date -d $capr_yyyymmdd +%s )
    fi

    #todo:  calculate the end date for three-month run.
    #note:  end date is 3 month from fcst date, not rst date.
    if [[ -n $fcstdate ]];then
        nummonth1=3
        
        strxmonth=$( nextXmonths $( date -d $fcstdate +%Y ) $( printf '%01g' $( date -d $fcstdate +%m ) ) 0 $(( nummonth1 + 1 ))  )
        end_mm=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f1 )
        end_year=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f2 )
        end3_yyyymmdd=$end_year$end_mm"01"
        end3_yyyymm=$end_year$end_mm
    fi

    if [[ -f $_fcapric ]];then
        capric_yyyymmdd=$( cat $_fcapric | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
        capric_yyyymm=$( echo $capric_yyyymmdd | cut -c1-6 )
        capric_yyyy=$( echo $capric_yyyymmdd | cut -c1-4 )
        capric_yyyymmdd_insec=$( date -d $capric_yyyymmdd +%s )
        capric_hhmmss=$( cat $_fcapric | head -1 | tr -s '[:space:]' | cut -d' ' -f2 )
        bllastdayofthemonth=$( fcal_lastdayofthemonth $capric_yyyymmdd )
    fi

    #note:  cap_end vars
    if [[ -f $_fcapend ]];then
        realend_yyyymmdd=$( cat $_fcapend | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
        realend_yyyymm=$( cat $_fcapend | head -1 | tr -s '[:space:]' | cut -d' ' -f1 | cut -c1-6 )
        realend_yyyy=$( echo $realend_yyyymmdd | cut -c1-4 )
        realend_yyyymmdd_insec=$( date -d $realend_yyyymmdd +%s )
    fi



    #todo:  calculate a number of *full* months that exp ran.
    arrmfull_3mo=($( numfullmonths $capric_yyyymm $end3_yyyymm $bllastdayofthemonth ))
    arrmfull_10mo=($( numfullmonths $capric_yyyymm $realend_yyyymm $bllastdayofthemonth ))
    arrmon_3mo=( $capric_yyyymm ${arrmfull_3mo[@]} )
    arrmon_10mo=( $capric_yyyymm ${arrmfull_10mo[@]} )
    
    arrmfull=($( numfullmonths $capric_yyyymm $capr_yyyymm $bllastdayofthemonth ))
    #nummfull=${#arrmfull[@]}   

    #todo: get collection info.
    [[ -f $_fhis1 ]] && arrcollmonpost_sstincl=($( exp_getcollfreq n $_fhis1 ))
 
    #todo:  calculate a total number of mom outputs 
    if [[ -n $fcstdate ]];then 
        numfmom_3mon_calc=$( calc_numseg_3mon $fcstdate $end3_yyyymmdd ) 

        if (( $capr_yyyymmdd > $end3_yyyymmdd ));then 
            local _beg4mo_yyyymmdd=$end3_yyyymmdd
            local _numsegmom=$( calc_numseg_mom $_beg4mo_yyyymmdd $capr_yyyymmdd $_seg_m ) 
            local _numsegtot=$(( $numfmom_3mon_calc + $_numsegmom ))
        else
            local _numsegtot=$numfmom_3mon_calc
        fi

        numfmom_calc=$_numsegtot
    fi
    if [[ -f $_fhis2 ]];then
        arrcoll=($( exp_getcollections $_fhis2 ))
        arrcollmon_mapl=($( exp_getcollfreq m $_fhis2 ))
        arrcollmonpost=($( exp_getcollfreq n $_fhis2 ))
        arrcollmonpost_diu=($( exp_getcolldiurnal $_fhis2 ))
        arrcollmonpost_save=($( printf '%s\n' ${arrcollmonpost[@]} | grep -E '_slv|_sfc' 2>/dev/null ))
        numcollmonpost=${#arrcollmonpost[@]}

        #todo:  get collections that produces extra monthly mean (*.monthly.* file)
        #note:  for s2sv3 hindcast, ocn_tavg_1dy_glo_T1440x1080_slv creates extra monthly mean IF ic date is the 
        #       first day of the month
        blfirstdayofthemonth=$( fcal_firstdayofthemonth $capric_yyyymmdd )
        arrcollmonpost_noreft=()
        for collmonpost in ${arrcollmonpost[@]};do
            collreftime=$( grep $collmonpost.ref_time $_fhis2 2>/dev/null | tr -s '[:space:]' | rev  | cut -d' ' -f1 | cut -c2- | rev )
        
            if [[ -z $collreftime ]] || (( $capric_hhmmss != $collreftime ));then
                arrcollmonpost_noreft+=( $collmonpost )
            fi
        done

        #note:  array with collections to save
        arrcollmonpost_savemon=( ${arrcollmonpost[@]} )
        arrcollmonpost_savediu=($( printf '%s\n' ${arrcollmonpost_diu[@]} | sort -V -k3 -t'_' | grep -E '_sfc|_slv' 2>/dev/null ))
        arrcollmonpost_savedya=( ice_tavg_1dy_glo_T1440x1080_slv )
        arrcollmonpost_savedy3=( sfc_tavg_3hr_glo_L720x361_sfc sfc_tavg_1hr_glo_L720x361_sfc )
   
        #todo:  get files to save
        (( $capr_yyyymmdd == $realend_yyyymmdd )) && local _bl10morun=true || local _bl10morun=false
        $_bl10morun && local _arrmonthfull=( ${arrmfull_10mo[@]} ) || local _arrmonthfull=( ${arrmfull_3mo[@]} )
        $_bl10morun && local _arrmonth=( ${arrmon_10mo[@]} )       || local _arrmonth=( ${arrmon_3mo[@]} )

        for coll in ${arrcoll[@]};do
            [[ "${arrcollmon_mapl[@]}" =~ "$coll" ]] && local  _blsavemapl=true || local _blsavemapl=false
            [[ "${arrcollmonpost_savemon[@]}" =~ "$coll" ]] && local _blsavemon=true || local _blsavemon=false
            [[ "${arrcollmonpost_savediu[@]}" =~ "$coll" ]] && local _blsavediu=true || local _blsavediu=false
            [[ "${arrcollmonpost_savedya[@]}" =~ "$coll" ]] && local _blsavedya=true || local _blsavedya=false
            [[ "${arrcollmonpost_savedy3[@]}" =~ "$coll" ]] && local _blsavedy3=true || local _blsavedy3=false
            [[ "${arrcollmonpost_noreft[@]}"  =~ "$coll" ]] && local _blnoreft=true  || local _blnoreft=false

            #arrfsave+=($( exp_createfname_save $_dexp $coll $blfirstdayofthemonth $bllastdayofthemonth $_blsavemapl $_blsavemon $_blsavediu $_blsavedya $_blsavedy3 $_blnoreft ${_arrmonthfull[@]} ))
            arrfsave+=($( exp_createfname_save $_dexp $coll $blfirstdayofthemonth $bllastdayofthemonth $_blsavemapl $_blsavemon $_blsavediu $_blsavedya $_blsavedy3 $_blnoreft ${_arrmonth[@]} ))
        done 

        #todo:  create output file names that are supposed to exists in dexp
        for coll in ${arrcoll[@]};do
            [[ "${arrcollmonpost_savedy3[@]}" =~ "$coll" ]] && local _blsavedy3=true || local _blsavedy3=false
            [[ "${arrcollmonpost_noreft[@]}"  =~ "$coll" ]] && local _blnoreft=true  || local _blnoreft=false
            #arrfname+=($( exp_createfname $_dexp $coll $bllastdayofthemonth $blfirstdayofthemonth $_blnoreft ${_arrmonthfull[@]} ))
            arrfname+=($( exp_createfname $_dexp $coll $bllastdayofthemonth $blfirstdayofthemonth $_blsavedy3 $_blnoreft ${_arrmonth[@]} ))
        done 

        #todo:  find which file to delete
        #note:  arrfremove contains file  names. This does not necessarily mean that script has checked output
        #       timestamps and size on arc and pfe. See get_fmiss_fexist function. That's where
        #       those two variables are checked. 
        if $blleaveout; then 
            arrfremove=($( printf '%s\n' ${arrfname[@]} ${arrfsave[@]} | sort -V | uniq -u ))
        else
            arrfremove=( ${arrfname[@]} )
        fi

        [[ -d $_dexp/$strrst ]] && arrfremove+=($( find $_dexp/$strrst/* -type f -name "${strrst}*.tar"        2>/dev/null ))
        [[ -d $_dexp/$strmom ]] && arrfremove+=($( find $_dexp/$strmom/* -type f -name "${strmom_search}*.nc*" 2>/dev/null ))



        ##!!!!! Sanity Check Arrays and Numbers : 03/10/2022 - save these for now (start) !!!!!
        ##todo:  all existing files in dexp (Save this as of 03/10/2022)
        #arrfexist=($( find $_dexp/* -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*.nc4*" | grep -v sst | sort -V ))
        #
        ##todo:  file names that does not exist 
        #arrfname_notexist=($( printf '%s\n'  ${arrfname[@]} | xargs -i bash -c "[[ ! -f {} ]] && echo {}" | sort -V ))
        #
        ##todo:  files that need to be saved but missing 
        #arrfsave_miss=($( printf '%s\n' ${arrfsave[@]}  | xargs -i bash -c "[[ ! -f {} ]] && echo {}" | sort -V ))
        #
        ##todo:  total number of files ( save + delete ). Supposed to equal to total # of outputs.
        #numsaveplusdelete=$( echo "${#arrfremove[@]} + ${#arrfsave[@]}" | bc )
        #
        ##!!!!! Sanity Check Arrays and Numbers : 03/10/2022 - save these for now ( end ) !!!!!
       
        #todo:  These are calculated values (based on created files names) based on 3-month or 10-month run
        if $blleaveout;then
            numfsave_mpl=$( printf '%s\n' ${arrfsave[@]} | grep "$strmapl" | wc -l )
            numfsave_mon=$( printf '%s\n' ${arrfsave[@]} | grep "monthly"  | wc -l )
            numfsave_diu=$( printf '%s\n' ${arrfsave[@]} | grep "diurnal"  | wc -l )
            numfsave_day=$( printf '%s\n' ${arrfsave[@]} | grep "daily"    | wc -l )
            numfsave_rst=0
            numfsave_mom=0
            numfsave_tot=$( echo "$numfsave_mpl + $numfsave_mon + $numfsave_diu + $numfsave_day + $numfsave_mom" | bc )
        else
            numfsave_mpl=0
            numfsave_mon=0
            numfsave_diu=0
            numfsave_day=0
            numfsave_rst=0
            numfsave_mom=0
            numfsave_tot=0
        fi
        if $blleaveout && $bl10morun;then  
            numfremove_mpl=$( printf '%s\n' ${arrfremove[@]} | grep "$strmapl"       | xargs -i bash -c "[[ -f {} ]] && basename {}" | wc -l )
            numfremove_mon=$( printf '%s\n' ${arrfremove[@]} | grep "monthly"        | xargs -i bash -c "[[ -f {} ]] && basename {}" | wc -l )
            numfremove_diu=$( printf '%s\n' ${arrfremove[@]} | grep "diurnal"        | xargs -i bash -c "[[ -f {} ]] && basename {}" | wc -l )
            numfremove_day=$( printf '%s\n' ${arrfremove[@]} | grep "daily"          | xargs -i bash -c "[[ -f {} ]] && basename {}" | grep -v $strmom_search | wc -l )
            numfremove_rst=$( printf '%s\n' ${arrfremove[@]} | grep "$strrst"        | xargs -i bash -c "[[ -f {} ]] && basename {}" | wc -l )
            numfremove_mom=$( printf '%s\n' ${arrfremove[@]} | grep "$strmom_search" | xargs -i bash -c "[[ -f {} ]] && basename {}" | wc -l )
            numfremove_tot=$( echo "$numfremove_mpl + $numfremove_mon + $numfremove_diu + $numfremove_day" | bc )

            numfsavePfrm_mpl=$((  numfsave_mpl +  numfremove_mpl ))      
            numfsavePfrm_mon=$((  numfsave_mon +  numfremove_mon ))      
            numfsavePfrm_diu=$((  numfsave_diu +  numfremove_diu ))      
            numfsavePfrm_day=$((  numfsave_day +  numfremove_day ))      
            numfsavePfrm_rst=$((  numfsave_rst +  numfremove_rst ))      
            numfsavePfrm_mom=$((  numfsave_mom +  numfremove_mom ))      
            numfsavePfrm_tot=$((  numfsave_tot +  numfremove_tot ))      
        fi

        #todo:  calculate a number of total output files.
        numcoll=${#arrcoll[@]}
        numcollmon_mapl=${#arrcollmon_mapl[@]}        
        numcollmon_maplpartial=${#arrcollmon_mapl[@]}        
        numcollmonpost=${#arrcollmonpost[@]}
        numcollmonpost_diu=${#arrcollmonpost_diu[@]}        
        numcollmonpost_nreft=${#arrcollmonpost_noreft[@]}
        numcollmonpost_savedy3=${#arrcollmonpost_savedy3[@]}
        nummon_run=$( fcal_calcnummon $capric_yyyymmdd $capr_yyyymmdd  )

        if $bllastdayofthemonth;then
            numfmonmapl_calc=$( echo "$nummon_run * $numcollmon_mapl    + $numcollmon_maplpartial"                          | bc -l ) 
                numfday_calc=$( echo "$nummon_run * $numcollmonpost     + $numcollmonpost_nreft + $numcollmonpost_savedy3"  | bc -l ) 
                numfdiu_calc=$( echo "$nummon_run * $numcollmonpost_diu + $numcollmonpost_savedy3"                          | bc -l ) 
            numfmonpost_calc=$( echo "$nummon_run * $numcollmonpost     + $numcollmonpost_nreft + $numcollmonpost_savedy3"  | bc -l ) 

        elif $blfirstdayofthemonth;then
            numfmonmapl_calc=$( echo "( $nummon_run - 1 ) * $numcollmon_mapl + $numcollmon_maplpartial" | bc -l ) 

            #note:  subtract 1 from nummon_run when partial month output for  high resolution collections are deleted
            #    numfday_calc=$( echo "( $nummon_run - 1 ) * $numcollmonpost + $numcollmonpost_nreft" | bc -l ) 
            #    numfdiu_calc=$( echo "( $nummon_run - 1 ) * $numcollmonpost_diu" | bc -l ) 
            #numfmonpost_calc=$( echo "( $nummon_run - 1 ) * $numcollmonpost + $numcollmonpost_nreft" | bc -l ) 

                numfday_calc=$( echo "( $nummon_run  ) * $numcollmonpost"     | bc -l ) 
                numfdiu_calc=$( echo "( $nummon_run  ) * $numcollmonpost_diu" | bc -l ) 
            numfmonpost_calc=$( echo "( $nummon_run  ) * $numcollmonpost"     | bc -l ) 

        else
            numfmonmapl_calc=$( echo "( $nummon_run - 1 ) * $numcollmon_mapl + $numcollmon_maplpartial" | bc -l ) 

            #note:  subtract 1 from nummon_run when partial month output for  high resolution collections are deleted
            #    numfday_calc=$( echo "( $nummon_run - 1 ) * $numcollmonpost" | bc -l ) 
            #    numfdiu_calc=$( echo "( $nummon_run - 1 ) * $numcollmonpost_diu" | bc -l ) 
            #numfmonpost_calc=$( echo "( $nummon_run - 1 ) * $numcollmonpost" | bc -l ) 

                numfday_calc=$( echo "( $nummon_run ) * $numcollmonpost" | bc -l ) 
                numfdiu_calc=$( echo "( $nummon_run ) * $numcollmonpost_diu" | bc -l ) 

            numfmonpost_calc=$( echo "( $nummon_run ) * $numcollmonpost" | bc -l ) 

        fi

        numftotal_calc=$( echo "$numfmonmapl_calc + $numfday_calc + $numfdiu_calc + $numfmonpost_calc + $numfmom_calc" | bc -l )

        #todo:  calc 3mon
        local thismon=3
        if $bllastdayofthemonth;then
            numfmonmapl_3mon_calc=$( echo "$thismon * $numcollmon_mapl + $numcollmon_maplpartial" | bc -l ) 
                numfday_3mon_calc=$( echo "$thismon * $numcollmonpost" | bc -l ) 
                numfdiu_3mon_calc=$( echo "$thismon * $numcollmonpost_diu" | bc -l ) 
            numfmonpost_3mon_calc=$( echo "$thismon * $numcollmonpost" | bc -l ) 

        elif $blfirstdayofthemonth;then
            numfmonmapl_3mon_calc=$( echo "( $thismon - 1 ) * $numcollmon_mapl + $numcollmon_maplpartial" | bc -l ) 

            #    numfday_3mon_calc=$( echo "( $thismon - 1 ) * $numcollmonpost + $numcollmonpost_nreft" | bc -l ) 
            #    numfdiu_3mon_calc=$( echo "( $thismon - 1 ) * $numcollmonpost_diu" | bc -l ) 
            #numfmonpost_3mon_calc=$( echo "( $thismon - 1 ) * $numcollmonpost + $numcollmonpost_nreft" | bc -l ) 

            #note:  subtract 1 from nummon_run when partial month output for  high resolution collections are deleted
                numfday_3mon_calc=$( echo "( $thismon - 1 ) * $numcollmonpost + $numcollmonpost_nreft" | bc -l ) 
                numfdiu_3mon_calc=$( echo "( $thismon - 1 ) * $numcollmonpost_diu" | bc -l ) 
            numfmonpost_3mon_calc=$( echo "( $thismon - 1 ) * $numcollmonpost + $numcollmonpost_nreft" | bc -l ) 

        else
            numfmonmapl_3mon_calc=$( echo "( $thismon - 1 ) * $numcollmon_mapl + $numcollmon_maplpartial" | bc -l ) 

            #    numfday_3mon_calc=$( echo "( $thismon - 1 ) * $numcollmonpost" | bc -l ) 
            #    numfdiu_3mon_calc=$( echo "( $thismon - 1 ) * $numcollmonpost_diu" | bc -l ) 
            #numfmonpost_3mon_calc=$( echo "( $thismon - 1 ) * $numcollmonpost" | bc -l ) 

            #note:  subtract 1 from nummon_run when partial month output for  high resolution collections are deleted
                numfday_3mon_calc=$( echo "( $thismon - 1 ) * $numcollmonpost" | bc -l ) 
                numfdiu_3mon_calc=$( echo "( $thismon - 1 ) * $numcollmonpost_diu" | bc -l ) 
            numfmonpost_3mon_calc=$( echo "( $thismon - 1 ) * $numcollmonpost" | bc -l ) 

        fi

        numftotal_3mon_calc=$( echo "$numfmonmapl_3mon_calc + $numfday_3mon_calc + $numfdiu_3mon_calc + $numfmonpost_3mon_calc + $numfmom_3mon_calc" | bc -l )

        numfsst_calc=3 
    fi 

    arrfmiss=($( exp_getfmiss_fexist $_dexp ))

    return
}


_setvars(){
    #description:    set various variables
    local _dexp=$1
    
    [[ -z $_dexp ]] && die "(${FUNCNAME[0]}) a full path to an experiment dir is required input"
    
    local fcap=CAP.rc
    local fcapr=cap_restart
    local fcapric=cap_restartIC
    local fcapend=cap_end
    local fhis2=HISTORY_2.rc
    local fhis1=HISTORY_1.rc

    local _fcap=$_dexp/$fcap
    local _fcapr=$_dexp/$fcapr
    local _fcapric=$_dexp/$fcapric
    local _fcapend=$_dexp/$fcapend
    local _fhis1=$_dexp/$fhis1
    local _fhis2=$_dexp/$fhis2
    local strmapl="01_0000z.nc4"
    local coll
    
    arrfsave=()
    arrfname=()
    strrst=restarts
    strmom=MOM_Output
    strmom_search=ocean_daily
    strscrach=scratch

    fcstdate=$( echo $_dexp | rev | cut -d'/' -f2 | rev )
    ensm=$( echo $_dexp | rev | cut -d'/' -f1 | rev )

    #note:  collsst should be set in srcme_* files. If not, use this.
    [[ -z $collsst ]] && local collsst=sst_tavg_1dy_glo_L720x361_slv

    [[ -f $_fhis1 ]] && arrcollnonmon_sstincl=($( exp_getcollfreq n $_fhis1 ))
    
    #note:  cap_restart vars
    if [[ -f $_fcapr ]];then
        capr_yyyymmdd=$( cat $_fcapr | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
        capr_yyyymm=$( cat $_fcapr | head -1 | tr -s '[:space:]' | cut -d' ' -f1 | cut -c1-6 )
        capr_yyyy=$( echo $capr_yyyymmdd | cut -c1-4 )
        capr_yyyymmdd_insec=$( date -d $capr_yyyymmdd +%s )
    fi

    #note:  cap_restartIC vars
    if [[ -f $_fcapric ]];then
        capric_yyyymmdd=$( cat $_fcapric | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
        capric_yyyymm=$( echo $capric_yyyymmdd | cut -c1-6 )
        capric_yyyy=$( echo $capric_yyyymmdd | cut -c1-4 )
        capric_yyyymmdd_insec=$( date -d $capric_yyyymmdd +%s )
        capric_hhmmss=$( cat $_fcapric | head -1 | tr -s '[:space:]' | cut -d' ' -f2 )
        bllastdayofthemonth=$( fcal_lastdayofthemonth $capric_yyyymmdd )
        blfirstdayofthemonth=$( fcal_firstdayofthemonth $capric_yyyymmdd )
    fi

    #note:  cap_end vars
    if [[ -f $_fcapend ]];then
        realend_yyyymmdd=$( cat $_fcapend | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
        realend_yyyymm=$( cat $_fcapend | head -1 | tr -s '[:space:]' | cut -d' ' -f1 | cut -c1-6 )
        realend_yyyy=$( echo $realend_yyyymmdd | cut -c1-4 )
        realend_yyyymmdd_insec=$( date -d $realend_yyyymmdd +%s )
    fi

    #todo:  calculate the end date for three-month run.
    #note:  end date is 3 month from fcst date, not rst date.
    if [[ -n $fcstdate ]];then
        nummonth1=3
        
        strxmonth=$( nextXmonths $( date -d $fcstdate +%Y ) $( printf '%01g' $( date -d $fcstdate +%m ) ) 0 $(( nummonth1 + 1 ))  )
        end_mm=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f1 )
        end_year=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f2 )
        end3_yyyymmdd=$end_year$end_mm"01"
        end3_yyyymm=$end_year$end_mm

        #todo:  calculate a total number of mom outputs 
        numfmom_calc=$( calc_numseg $fcstdate $capr_yyyymmdd )
    fi


    #todo:  calculate a number of *full* months that exp ran.
    arrmfull_3mo=($( numfullmonths $capric_yyyymm $end3_yyyymm $bllastdayofthemonth ))
    arrmfull_10mo=($( numfullmonths $capric_yyyymm $realend_yyyymm $bllastdayofthemonth ))
    
    arrmfull=($( numfullmonths $capric_yyyymm $capr_yyyymm $bllastdayofthemonth ))
    #nummfull=${#arrmfull[@]}   

    #todo: get collection info.
    [[ -f $_fhis1 ]] && arrcollmonpost_sstincl=($( exp_getcollfreq n $_fhis1 ))
    
    if [[ -f $_fhis2 ]];then
        arrcoll=($( exp_getcollections $_fhis2 ))
        arrcollmon_mapl=($( exp_getcollfreq m $_fhis2 ))
        arrcollmonpost=($( exp_getcollfreq n $_fhis2 ))
        arrcollmonpost_diu=($( exp_getcolldiurnal $_fhis2 ))
        arrcollmonpost_save=($( printf '%s\n' ${arrcollmonpost[@]} | grep -E '_slv|_sfc' ))
        numcollmonpost=${#arrcollmonpost[@]}
        
  
        #todo:  get collections that produces extra monthly mean (*.monthly.* file)
        #note:  for s2sv3 hindcast, ocn_tavg_1dy_glo_T1440x1080_slv creates extra monthly mean IF ic date is the 
        #       first day of the month
        arrcollmonpost_noreft=()
        for collmonpost in ${arrcollmonpost[@]};do
            collreftime=$( grep $collmonpost.ref_time $_fhis2 | tr -s '[:space:]' | rev  | cut -d' ' -f1 | cut -c2- | rev )
        
            if [[ -z $collreftime ]] || (( $capric_hhmmss != $collreftime ));then
                arrcollmonpost_noreft+=( $collmonpost )
            fi
        done
        
        #note:  array with collections to save
        arrcollmonpost_savemon=( ${arrcollmonpost[@]} )
        arrcollmonpost_savediu=($( printf '%s\n' ${arrcollmonpost_diu[@]} | sort -V -k3 -t'_' | grep -E '_sfc|_slv' ))
        arrcollmonpost_savedya=( ice_tavg_1dy_glo_T1440x1080_slv )
        arrcollmonpost_savedy3=( sfc_tavg_3hr_glo_L720x361_sfc sfc_tavg_1hr_glo_L720x361_sfc )
   
        #todo:  get files to save
        (( $capr_yyyymmdd == $realend_yyyymmdd )) && local _bl10morun=true || local _bl10morun=false
        $_bl10morun && local _arrmonthfull=( ${arrmfull_10mo[@]} ) || local _arrmonthfull=( ${arrmfull_3mo[@]} )

        for coll in ${arrcoll[@]};do
            [[ "${arrcollmon_mapl[@]}" =~ "$coll" ]] && local  _blsavemapl=true || local _blsavemapl=false
            [[ "${arrcollmonpost_savemon[@]}" =~ "$coll" ]] && local _blsavemon=true || local _blsavemon=false
            [[ "${arrcollmonpost_savediu[@]}" =~ "$coll" ]] && local _blsavediu=true || local _blsavediu=false
            [[ "${arrcollmonpost_savedya[@]}" =~ "$coll" ]] && local _blsavedya=true || local _blsavedya=false
            [[ "${arrcollmonpost_savedy3[@]}" =~ "$coll" ]] && local _blsavedy3=true || local _blsavedy3=false
            [[ "${arrcollmonpost_noreft[@]}"  =~ "$coll" ]] && local _blnoreft=true  || local _blnoreft=false

            arrfsave+=($( exp_createfname_save $_dexp $coll $blfirstdayofthemonth $bllastdayofthemonth $_blsavemapl $_blsavemon $_blsavediu $_blsavedya $_blsavedy3 $_blnoreft ${_arrmonthfull[@]} ))
        done 

#wmessage lineno = $LINENO 
###local arrtmp=($( printf '%s\n' ${arrfsave[@]} | grep monthly | xargs -i basename {} | sort -V )) 
#local arrtmp=($( printf '%s\n' ${arrfsave[@]} | grep diurnal | xargs -i basename {} | sort -V )) 
#ahand_warr ${arrtmp[@]} 
#exit
#wmessage ${#arrtmp[@]} 
#wmessage
        #todo:  create output file names that are supposed to exists in dexp
        for coll in ${arrcoll[@]};do
            [[ "${arrcollmonpost_noreft[@]}"  =~ "$coll" ]] && local _blnoreft=true  || local _blnoreft=false
            [[ "${arrcollmonpost_savedy3[@]}" =~ "$coll" ]] && local _blsavedy3=true || local _blsavedy3=false
            arrfname+=($( exp_createfname $_dexp $coll $bllastdayofthemonth $blfirstdayofthemonth $_blsavedy3 $_blnoreft ${_arrmonthfull[@]} ))
        done 

        #todo:  find which file to delete
        arrfremove=($( printf '%s\n' ${arrfname[@]} ${arrfsave[@]} | sort -V | uniq -u | sort -V ))


        #!!!!! Sanity Check Arrays and Numbers : 03/10/2022 - save these for now (start) !!!!!
        #todo:  all existing files in dexp (Save this as of 03/10/2022)
        arrfexist=($( find $_dexp/* -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*.nc4*" | grep -v sst | sort -V ))
        
        #todo:  file names that does not exist 
        arrfname_notexist=($( printf '%s\n'  ${arrfname[@]} | xargs -i bash -c "[[ ! -f {} ]] && echo {}" | sort -V ))
        
        #todo:  files that need to be saved but missing 
        arrfsave_miss=($( printf '%s\n' ${arrfsave[@]}  | xargs -i bash -c "[[ ! -f {} ]] && echo {}" | sort -V ))
        
        #todo:  total number of files ( save + delete ). Supposed to equal to total # of outputs.
        numsaveplusdelete=$( echo "${#arrfremove[@]} + ${#arrfsave[@]}" | bc )
        
        #!!!!! Sanity Check Arrays and Numbers : 03/10/2022 - save these for now ( end ) !!!!!
       
        #todo:  These are calculated values (based on created files names) based on 3-month or 10-month run
        if $blleaveout;then
            numfsave_mpl=$( printf '%s\n' ${arrfsave[@]} | grep "$strmapl" | wc -l )
            numfsave_mon=$( printf '%s\n' ${arrfsave[@]} | grep "monthly"  | wc -l )
            numfsave_diu=$( printf '%s\n' ${arrfsave[@]} | grep "diurnal"  | wc -l )
            numfsave_day=$( printf '%s\n' ${arrfsave[@]} | grep "daily"    | wc -l )
            numfsave_rst=0
            numfsave_mom=0
            numfsave_tot=$( echo "$numfsave_mpl + $numfsave_mon + $numfsave_diu + $numfsave_day + $numfsave_mom" | bc )
        else
            numfsave_mpl=0
            numfsave_mon=0
            numfsave_diu=0
            numfsave_day=0
            numfsave_rst=0
            numfsave_mom=0
            numfsave_tot=$( echo "$numfsave_mpl + $numfsave_mon + $numfsave_diu + $numfsave_day + $numfsave_mom" | bc )
        fi
wmessage \@$LINENO $numfsave_mon        

        numfremove_mpl=$( printf '%s\n' ${arrfremove[@]} | grep "$strmapl" | wc -l )
        numfremove_mon=$( printf '%s\n' ${arrfremove[@]} | grep "monthly"  | wc -l )
        numfremove_diu=$( printf '%s\n' ${arrfremove[@]} | grep "diurnal"  | wc -l )
        numfremove_day=$( printf '%s\n' ${arrfremove[@]} | grep "daily"    | grep -v $strmom_search | wc -l )
        numfremove_mom=$numfmom_calc
        numfremove_tot=$( echo "$numfremove_mpl + $numfremove_mon + $numfremove_diu + $numfremove_day" | bc )


        #todo:  calculate a number of total output files.
        numcoll=${#arrcoll[@]}
        numcollmon_mapl=${#arrcollmon_mapl[@]}        
        numcollmon_maplpartial=${#arrcollmon_mapl[@]}        
        numcollmonpost=${#arrcollmonpost[@]}
        numcollmonpost_diu=${#arrcollmonpost_diu[@]}        
        numcollmonpost_nreft=${#arrcollmonpost_noreft[@]}
        nummon_run=$( fcal_calcnummon $capric_yyyymmdd $capr_yyyymmdd  )

        if $bllastdayofthemonth;then
            numfmonmapl_calc=$( echo "$nummon_run * $numcollmon_mapl + $numcollmon_maplpartial" | bc -l ) 
                numfday_calc=$( echo "$nummon_run * $numcollmonpost" | bc -l ) 
                numfdiu_calc=$( echo "$nummon_run * $numcollmonpost_diu" | bc -l ) 
            numfmonpost_calc=$( echo "$nummon_run * $numcollmonpost" | bc -l ) 
              numftotal_calc=$( echo "$numfmonmapl_calc + $numfday_calc + $numfdiu_calc + $numfmonpost_calc" | bc -l )
        elif $blfirstdayofthemonth;then
            numfmonmapl_calc=$( echo "( $nummon_run - 1 ) * $numcollmon_mapl + $numcollmon_maplpartial" | bc -l ) 
                numfday_calc=$( echo "( $nummon_run - 1 ) * $numcollmonpost + $numcollmonpost_nreft" | bc -l ) 
                numfdiu_calc=$( echo "( $nummon_run - 1 ) * $numcollmonpost_diu" | bc -l ) 
            numfmonpost_calc=$( echo "( $nummon_run - 1 ) * $numcollmonpost + $numcollmonpost_nreft" | bc -l ) 
              numftotal_calc=$( echo "$numfmonmapl_calc + $numfday_calc + $numfdiu_calc + $numfmonpost_calc" | bc -l )
        else
            numfmonmapl_calc=$( echo "( $nummon_run - 1 ) * $numcollmon_mapl + $numcollmon_maplpartial" | bc -l ) 
                numfday_calc=$( echo "( $nummon_run - 1 ) * $numcollmonpost" | bc -l ) 
                numfdiu_calc=$( echo "( $nummon_run - 1 ) * $numcollmonpost_diu" | bc -l ) 
            numfmonpost_calc=$( echo "( $nummon_run - 1 ) * $numcollmonpost" | bc -l ) 
              numftotal_calc=$( echo "$numfmonmapl_calc + $numfday_calc + $numfdiu_calc + $numfmonpost_calc" | bc -l )
        fi

        #todo:  calc 3mon
        local thismon=3
        if $bllastdayofthemonth;then
            numfmonmapl_3mon_calc=$( echo "$thismon * $numcollmon_mapl + $numcollmon_maplpartial" | bc -l ) 
                numftar_3mon_calc=$( echo "$thismon * $numcollmonpost" | bc -l ) 
                numfdiu_3mon_calc=$( echo "$thismon * $numcollmonpost_diu" | bc -l ) 
            numfmonpost_3mon_calc=$( echo "$thismon * $numcollmonpost" | bc -l ) 
              numftotal_3mon_calc=$( echo "$numfmonmapl_3mon_calc + $numftar_3mon_calc + $numfdiu_3mon_calc + $numfmonpost_3mon_calc" | bc -l )
        elif $blfirstdayofthemonth;then
            numfmonmapl_3mon_calc=$( echo "( $thismon - 1 ) * $numcollmon_mapl + $numcollmon_maplpartial" | bc -l ) 
                numfday_3mon_calc=$( echo "( $thismon - 1 ) * $numcollmonpost + $numcollmonpost_nreft" | bc -l ) 
                numfdiu_3mon_calc=$( echo "( $thismon - 1 ) * $numcollmonpost_diu" | bc -l ) 
            numfmonpost_3mon_calc=$( echo "( $thismon - 1 ) * $numcollmonpost + $numcollmonpost_nreft" | bc -l ) 
              numftotal_3mon_calc=$( echo "$numfmonmapl_3mon_calc + $numfday_3mon_calc + $numfdiu_3mon_calc + $numfmonpost_3mon_calc" | bc -l )
        else
            numfmonmapl_3mon_calc=$( echo "( $thismon - 1 ) * $numcollmon_mapl + $numcollmon_maplpartial" | bc -l ) 
                numftar_3mon_calc=$( echo "( $thismon - 1 ) * $numcollmonpost" | bc -l ) 
                numfdiu_3mon_calc=$( echo "( $thismon - 1 ) * $numcollmonpost_diu" | bc -l ) 
            numfmonpost_3mon_calc=$( echo "( $thismon - 1 ) * $numcollmonpost" | bc -l ) 
              numftotal_3mon_calc=$( echo "$numfmonmapl_3mon_calc + $numftar_3mon_calc + $numfdiu_3mon_calc + $numfmonpost_3mon_calc" | bc -l )
        fi
        numfsst_calc=3 
    fi 

    arrfmiss=($( exp_getfmiss_fexist $_dexp ))

    return
}

set_capdates(){
    #Usage: ${FUNCNAME[0]} [fcstdate] [ boolean true for winner ]"
    local _fcstdate=$1;shift
    local _blwinner=$1
    local nummonth1=3
    local nummonth2=7

    [[ -z $_fcstdate ]] && die "(${FUNCNAME[0]}) fcstdate is a requied input."
    [[ -z $_blwinner ]] && die "(${FUNCNAME[0]}) boolean for winner is a requied input."

    if ! $_blwinner;then
        local nummon_total=$nummonth1
    elif $_blwinner;then 
        local nummon_total=$(( nummonth1 + nummonth2 ))
    fi

    #todo:  calculate the end date for three-month run.
    #note:  end date is 3 month from fcst date, not rst date. 
    local strxmonth=$( nextXmonths $( date -d $_fcstdate +%Y ) $( printf '%01g' $( date -d $_fcstdate +%m ) ) 0 $(( nummon_total + 1 ))  )
    local end_mm=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f1 )
    local end_year=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f2 )
    local yyyymmdd=$end_year${end_mm}01

    echo $yyyymmdd
}


set_fmonthfyr(){
    #description:   calcuate forecast year and month.
    local _results=$( fcal_setfmonfyr_bash )
    fyear=${_results:0:4}
    fmonth=${_results:4:2}

    #get_cal

    return
}


debug_filter(){
    local _thisinput 
    local _arrargs=()

    #note:  cntbug=0 in a function of interest.
    if [[ -z $1 ]];then
       local _thisinput=$dexp
    else
        local _arrargs=( "$@" )
    fi
    
    if (( $rundebug == 1 ));then 
        cntbug=$(( cntbug + 1 ))
        [[ -n $_thisinput ]] && wmessage "$cntbug $_thisinput"
        
        if [[ -n ${_arrargs[@]} ]];then

            if (( ${#_arrargs[@]} == 1 ));then
                wmessage "$cntbug ${_arrargs[0]}"
            else
                wmessage $cntbug:
                ahand_warr ${_arrargs[@]}
            fi
        fi
    fi
        
    return
}

IndexOf(){
	local i=1 S=$1;	shift  
    while [[ $S != $1 ]];do  
	((i++)); shift
        [ -z "$1" ] && { i=0; break; }
    done
	echo $i
}

cnt_jobs(){
    local _dexp=$1
    local fgrn=gcm_run.j
    local farc=archive/run_gcmarch.sh
    local fpst=post/gcm_post.j
    local _hstshort=$( get_host ) 

    [[ -z $_dexp ]] && die "(${FUNCNAME[0]}) a full path to a exp dir is required input" && exit
    [[ ! -f $_dexp/$fgrn ]] && die "(${FUNCNAME[0]}) $_dexp/$fgrn does not exists" && exit
    #[[ ! -f $_dexp/$fpst ]] && die "(${FUNCNAME[0]}) $_dexp/$fpst does not exists" && exit

    if [[ ! -f $_dexp/$farc ]];then 
        farc=archive/run_gcmfcstarchive.sh
        [[ ! -f $_dexp/$farc ]] && die "(${FUNCNAME[0]}) $_dexp/$farc does not exists"  && exit
    fi

    if [[ ! -f $_dexp/$fpst ]];then 
        fpst=post/gcm_post_full.j
        [[ ! -f $_dexp/$fpst ]] && die "(${FUNCNAME[0]}) $_dexp/$fpst does not exists"  && exit
    fi

    #todo:  get all running/q'd jobs by job name 
    if [[ "$_hstshort" == "dis" ]];then 
        local _cmd_gjob=cmd_gjob_nccs
        local arrjobs=($( $_cmd_gjob | grep $USER | tr -d '\t' | tr -s '[:space:]' | sed 's#^ *##' ))

    elif [[ "$_hstshort" == "pfe" ]];then 
        local _cmd_gjob=cmd_gjob_nas
        local arrjobs=($( $_cmd_gjob | sort -V -k4 -t' ' | tr -s '[:space:]' | cut -d' ' -f4 | cut -d'.' -f1 | sort -V | uniq | grep ens ))
    fi

    #todo:  check fcomp file and exit if it exists
    jname_grn=$( pbs_jname $_dexp/$fgrn )
    jname_arc=$( pbs_jname $_dexp/$farc )
    jname_pst=$( pbs_jname $_dexp/$fpst | cut -d'.' -f1 )

    #todo: get number of jobs running or on queue.
    [[ -n $jname_arc ]] && num_rarc=$( printf '%s\n' ${arrjobs[@]} | grep -w $jname_arc 2>/dev/null | wc -l ) || num_rarc=0
    [[ -n $jname_grn ]] && num_rgrn=$( printf '%s\n' ${arrjobs[@]} | grep -w $jname_grn 2>/dev/null | wc -l ) || num_rgrn=0
    [[ -n $jname_pst ]] && num_rpst=$( printf '%s\n' ${arrjobs[@]} | grep $jname_pst    2>/dev/null | wc -l ) || num_rarc=0
    
    #[[ -n $jname_arc ]] && num_rarc=$( /u/scicon/tools/bin/qstat -e -u $USER -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep -w $jname_arc | wc -l ) || num_rarc=0

    return
}

readdata(){
    local _fdata=$1
    local arr=()
    while IFS= read -r line; do
        #note:  read line only line is not empty and doesn't start with comment out (#)
        if [[ "$line" != "#"* ]] && [[ -n $line ]];then
            arr+=( "$line" )
        fi
    done < "$_fdata"
    echo ${arr[@]}
}

getremainingexp(){
    #description: read rst date and ens member from message_monitor_* file name.
    local dir=$1
    local fname
    local _strdate=""
    local fdata=data_${FUNCNAME[0]}
    
    shopt -s nullglob  
    local _arrfname=( message_${strscr}_* )
    shopt -u nullglob  

    if [[ -f $fdata ]];then rm -f $fdata;fi

    for fname in ${_arrfname[@]};do
        local _rstensm=$( echo $fname | cut -d'_' -f3 )
        local _rstdate=$( echo $_rstensm | cut -c1-8 )
        local _ensm=$( echo $_rstensm | cut -c9- )
        echo $dir/$_rstdate/$_ensm >> $fdata
    done

    local arr=($( readdata $fdata ))
    if [[ -f $fdata ]];then rm -f $fdata;fi

    echo ${arr[@]}
}

getrst(){
    local _drstsrc=$1;shift
    local _drstdst=$1
    local _arrrst=($( find -L $_drstsrc/* -maxdepth 0 -type f ))
    local _arrrstdir=($( find -L $_drstsrc/* -maxdepth 0 -type d ))
    local _fshiftc=${strscr}_${rstdate}${ensm}_shiftin
    #note:  01/28/2022 getrst uses shiftc to transfer restarts and it seems
    #                 having a problem. Use rsync in getrst function. 
    #       02/01/2022 Do NOT used blrsync defined in srcme_ file. 
    local _blrsync=true
  
    #todo:  get host name  
    if [[ -z $thishst ]];then
        local _thishst=$( get_host )
    else
        local _thishst=$thishst
    fi

    [[ -f $_fshiftc ]] && rm -f $_fshiftc

    #todo:  check destination and source dir dir
    if [[ -z $_drstsrc || ! -d $_drstsrc ]];then 
        die "(${FUNCNAME[0]}) a source dir is undefined or does not exist"
    fi
    if [[ -z $_drstdst || ! -d $_drstdst ]];then 
        die "(${FUNCNAME[0]}) a destination dir is undefined or does not exist"
    fi

    #todo:  copy cap_restart
    if [[ -f $_drstsrc/cap_restart ]];then
        cp -p $_drstsrc/cap_restart cap_restartIC
        cp -p $_drstsrc/cap_restart .
    else
        die "(${FUNCNAME[0]}) $_drstsrc/cap_restart does not exist"
    fi

    if ! $_blrsync && [[ $_thishst == pfe ]];then
        wmessage "... create shiftc batch file"   
        
        for rst in ${_arrrst[@]};do
            echo "$rst $_drstdst/" >> $_fshiftc 
        done
        for rstdir in ${_arrrstdir[@]};do
            echo "$rstdir $_drstdst/" >> $_fshiftc 
        done
    
        wmessage "... copying rst files: "
        wmessage "       from $_drstsrc" 
        wmessage "         to $_drstdst" 
        wmessage
        if (( $writetofile == 1 ));then
            /usr/local/bin/shiftc --no-cron --wait -r -L < $_fshiftc >> $fmessage 2>&1
            local _status_shiftc=$?
            (( $_status_shiftc == 0 )) && rm -f $_fshiftc
        else
            /usr/local/bin/shiftc --no-cron --wait -r -L < $_fshiftc
            local _status_shiftc=$?
            (( $_status_shiftc == 0 )) && rm -f $_fshiftc
        fi
        wmessage

    elif $_blrsync || [[ "$_thishst" == "dis" ]];then

        wmessage "... copying rst files from $_drstsrc" 
        wmessage
        if (( $writetofile == 1 ));then
            rsync -ar -L $_drstsrc/* $_drstdst/ >> $fmessage 2>&1
        else
            rsync -ar -L $_drstsrc/* $_drstdst/ 
        fi
    fi

    return 
}

setvars_node(){
    local _qid=$1

    #todo:  count number of gcmsetup_* screen. Limit total number of screen session is equal
    #       to total number of jobs can be submitted.
    numscr=$( ssh -q pfe21 screen -ls 2>/dev/null | grep gcmsetup_ | wc -l )
    #numscr_max=$(( numexpsub - numscr ))
    
    if [[ "$_qid" == "normal" ]] || [[ "$qid" == "long" ]];then
        die "(${FUNCNAME[0]}) qid has to be a reserved node id";exit
    fi

    #todo:  get basic node info
    qname=$( res_resname $_qid )
    mname=$( res_mname $_qid)
    numnode=$( res_nodect $_qid)
    resenddate=$( res_enddate $_qid )
    
    #todo:  check if any running job
    numRexp=$( node_job $_qid R )
    numQexp=$( node_job $_qid Q )
    numRexp_calc=$( calc_numexpcalc $_qid )
    #numexpsub=$( nodeavail $_qid )
    
    
    #todo:  calculate total exp should be submitted
    #note:  as of now, total is limited to (total number exp can run) * 2 = 20
    numtotexp=$(( numRexp_calc * 2 ))
    
    #todo:  calculate a number of exp that can be submitted
    numexptosubmit=$(( numtotexp - numRexp - numQexp - numscr ))

    return
}

calc_numexpcalc(){
    local resid=$1
    if [[ -z $numnodereq ]] || (( $numnodereq == 0 ));then
        local numexp_calc=0
    else
        local numnode=$( res_nodect $resid )
        local numexp_calc=$( echo "$numnode/$numnodereq" | bc )
    fi
    echo $numexp_calc
}

node_job(){
    #description:   determine if there is a runnin job(s) on a specific qid
    #input = qid, R or Q 
    local resid=$1;shift
    local str=$1
    local numexp=$( /u/scicon/tools/bin/qstat -e -u $USER -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep $resid | grep -w $str | wc -l )

    echo $numexp
}

get_dexp(){
    #description:   create exp dir name based on beginning yyyymm and end yyyymm.
    #Usage: ${FUNCNAME[0]} [main dir for exp location (i.e. $FCST) ] [ boolean true when dexp name should be the same as rst date ]"
    #       [ beginning of YYYYYMM ] [ End YYYYMM ] [ array of ensemble members (i.e. 200, 201, etc.)]
    local dir=$1;shift
    #local blrstexpname=$1;shift
    local begyyyymm=$1;shift
    local endyyyymm=$1;shift
    local _arrintens=( "$@" )
    local yyyymm=$begyyyymm
    local arrfcstdate=()
    local arr=()
    local fcstdate intens

    [[ -z $dir ]] && [[ ! -d $dir ]] && die "(${FUNCNAME[0]}) a parent dir name is a required input" && exit
    #if $blrstexpname || ! $blrstexpname ;then 
    #    :
    #else
    #    die "(${FUNCNAME[0]}) require boolean for exp dir name (true or false)  ";exit
    #fi
    [[ -n $begyyyymm ]] && local outbegyyyymm=$( misc_isinteger $begyyyymm )
    [[ -n $endyyyymm ]] && local outendyyyymm=$( misc_isinteger $endyyyymm )

    if [[ -z $begyyyymm ]] || (( $outbegyyyymm > 0 ));then 
        die "(${FUNCNAME[0]}) beginning yyyymm is a required integer input";exit
    fi
    if [[ -z $endyyyymm ]] || (( $outendyyyymm > 0 ));then 
        die "(${FUNCNAME[0]}) end yyyymm is a required input";exit
    fi
    while (( $yyyymm <= $endyyyymm ));do
        local yyyy=$( echo $yyyymm | cut -c1-4 )
        local int_mon=$( echo "$( echo $yyyymm | cut -c5-6 )*1" | bc )
        arrfcstdate+=($( fcal_icall $int_mon $yyyy )) 
        yyyymm=$( fcal_nextmonth $int_mon $yyyy )
    done

    #todo:  create restart date array
    for fcstdate in ${arrfcstdate[@]};do
            
        local dexpdate=$fcstdate
        
        #if $blrstexpname;then
        #    local rstdate_yyyy=$( date -d "$fcstdate -1days" +%Y)
        #    local rstdate_mmdd=$( date -d "$fcstdate -1days" +%m%d )
        #    local dexpdate=$( date -d "$fcstdate -1days" +%Y%m%d )
        #else
        #    local dexpdate=$fcstdate
        #fi
        
        arr+=($( printf $dir/$dexpdate/ens'%s\n' ${_arrintens[@]} ))
        #for intens in ${_arrintens[@]};do
        #    local ensm=ens$intens
        #    arr+=( $dir/$dexpdate/$ensm )
        #done
    done
    echo ${arr[@]}
}


check_fsize_archive(){
    #description:   check file sizes on archost
    
    local flfefile=$1
    if [[ -z $flfefile ]] || [[ ! -f $flfefile ]];then die "(${FUNCNAME[0]}) $flfefile does not exists or variable, flfefile, is not defined.";exit;fi

    local arrline=()
    local arrfzero=()

    while IFS=$'\n' read -r line; do
        intfsize=$( echo $line | cut -d' ' -f1 )
        fname=$( echo $line | cut -d' ' -f2 )
        if (( $intfsize == 0 ));then
            arrfzero+=( $fname )
        fi
    done < $flfefile

    echo ${arrfzero[@]}
}

get_beg_and_end(){
    #description:   get beginning yyyymm and end yyyymm
    #note:  this function was created in order to use fcal_icall functions
    #       to calculate fcst dates.
    local startyyyymm=$1;shift
    local lastyyyymm=$1
    local smm=$( echo "$( date -d ${startyyyymm}01 +%m )*1" | bc )
    local syyyy=$( date -d ${startyyyymm}01 +%Y )
    local lmm=$( echo "$( date -d ${lastyyyymm}01 +%m )*1" | bc )
    local lyyyy=$( date -d ${lastyyyymm}01 +%Y )

    begyyyymm=$( fcal_nextmonth $smm $syyyy )
    endyyyymm=$( fcal_nextmonth $lmm $lyyyy )
     
    return
}

get_icdates(){
    #description:   calculate ic dates, which are -1day of fcst date
    #               i.e.)   fcst date = 20150630
    #                     output date = 20150629
    #input has to be YYYYMM range 
    local start=$1;shift
    local end=$1
    local arr=()
    local arrfcstdate=()
    local yyyy intm yyyymm

    get_beg_and_end $start $end

    local yyyymm=$begyyyymm
    while (( $yyyymm <= $endyyyymm ));do
        yyyy=$( echo $yyyymm | cut  -c1-4 )
        intm=$( echo "$( echo $yyyymm | cut  -c5-6 )*1" | bc )
        arrfcstdate+=($( fcal_icall $intm $yyyy )) #<=== i.e. 6 to get  may fcstdates
        yyyymm=$( fcal_nextmonth $intm $yyyy )
    done
    
    #todo:  create restart date array
    arr=($( printf '%s\n' ${arrfcstdate[@]} | xargs -i bash -c 'date -d "{} -1days" +%Y%m%d'))

    echo "${arr[@]}"
}

winner_start_yyyymm(){
    #description:   calculate a month which is two months before the month used for
    #               winner file name.
    local fwinner=$1
    
    local month=$( echo $fwinner | cut -d'_' -f3 )
    local intmonth=$( echo "$month*1" | bc )
    local yyyy=$( echo $fwinner | cut -d'_' -f4 | cut -d'.' -f1 )
    local prevyyyymm=$( fcal_previousmonth $intmonth $yyyy )
    local int2monthago=2
        

    #note:  winner file has a month from the forecast starting month. thus, 
    #       need to calculate a month which is two month before the month of the file name
    for int in {1..$int2monthago};do
        month=$( date -d ${prevyyyymm}01 +%m )
        intmonth=$( echo "$month*1" | bc )
        yyyy=$( date -d ${prevyyyymm}01 +%Y )
        local prevyyyymm=$( fcal_previousmonth $intmonth $yyyy )
    done

    echo $prevyyyymm
}

winner_avail(){
    #description:   find forecast YYYYMM that have determined winner
    local dir=$1
    #local strwinner=winners_nino3.4_
    local strwinner=winners_
    local arr=()
    local fwin 
    
    if [[ -z $dir ]];then die "(${FUNCNAME[0]}) a parent dir name is a required input";exit;fi

    #todo:  get all winner files
    shopt -s nullglob
    local arrfwin=( $dir/$strwinner* )
    shopt -u nullglob
    
    for fwin in ${arrfwin[@]};do
        local arrwin=($( cat $fwin | sed 's# #/#' ))
        #todo:  determine new ens member (1 to 10) for each forecast month 
        local yyyymm=$( winner_start_yyyymm $( basename $fwin ) )
        arr+=($yyyymm)
    done
    arr=($( printf '%s\n' ${arr[@]} | sort | uniq ))
    echo ${arr[@]}
}

convert2winnerensm(){
    #description:   crate symbolic link ens1-10 in dyyyymm for winners
    #note:  associative array, assoc_winensm, have to be declared in main process
    local fwin=$1
    #local strwinner=$_d/winners_nino3.4_
    local strwinner=$_d/winners_
    local arrwin=($( cat $fwin | sed 's# #/#' ))

    #todo:  determine new ens member (1 to 10) for each forecast month 
    for i in "${!arrwin[@]}";do
        local ensm=ens$(( i + 1 ))
        local fexp=${arrwin[$i]}
        assoc_winensm[$fexp]=$ensm
    done
    return    
}
 
convert2winnerensm_all(){
    #description:   crate symbolic link ens1-10 in dyyyymm for winners
    #note:  associative array, assoc_winensm, have to be declared in main process
    local _d=$1
    #local strwinner=winners_nino3.4_
    local strwinner=winners_
    local fwin ensm fexp

    #arrfwin=($( find $_d/* -type f -name "${strwinner}*" 2>/dev/null ))
    local arrfwin=($( find $_d/* -name "${strwinner}*" 2>/dev/null ))

    for fwin in ${arrfwin[@]};do
        local arrwin=($( cat $fwin | sed 's# #/#' ))

        #todo:  determine new ens member (1 to 10) for each forecast month 
        for i in "${!arrwin[@]}";do
            local _ensm=ens$(( i + 1 ))
            local _fexp=${arrwin[$i]}
            assoc_winensm[$_fexp]=$_ensm
        done
    done
    
    #∞∞∞∞∞ debug (start) ∞∞∞∞∞
    #for x in "${!assoc_winensm[@]}";do wmessage $x ${assoc_winensm[$x]};done
    ##printf '%s\n' "${!assoc_winensm[@]}" ${assoc_winensm[@]} | pr -t2 | sort >&2
    #∞∞∞∞∞ debug ( end ) ∞∞∞∞∞

    return    
}   

calc_numfholdleft(){
    #description:   calculate number of files left in holding dir after exp is completed.
    local _fhis2=$1
    local _dexp=$( dirname $_fhis2 )
    local _arrcoll=($( exp_getcollections $_fhis2))
    local _strfreq=frequency
    local _ftotal=0
    local coll

    for coll in ${_arrcoll[@]};do
        local _freq1=$( grep $coll.$_strfreq $_fhis2 | cut -d':' -f2 | sed -e 's/,//g' -e 's/^[[:space:]]*//g' | cut -c1-2 )
        local _freq=$( echo "$_freq1 * 1" | bc -l )

        if (( $_freq == 24 ));then
            local _numf=$( echo "24/$_freq" | bc -l )
            _ftotal=$( echo "$_ftotal + $_numf" | bc -l )
        
        elif (( $_freq == 6 ));then
            local _numf=$( echo "24/$_freq" | bc -l )
            _ftotal=$( echo "$_ftotal + $_numf" | bc -l )

        elif (( $_freq == 3 ));then
            local _numf=$( echo "( 24/$_freq ) - 1" | bc -l )
            _ftotal=$( echo "$_ftotal + $_numf" | bc -l )
        fi
    done
    
    echo $_ftotal

}

get_icyyyymm2intfwin(){
    local int
    local yyyymm=$1
    local month=$( date -d ${yyyymm}01 +%m )
    local intmonth=$( echo "$month*1" | bc )
    local yyyy=$( date -d ${yyyymm}01 +%Y )

    for int in {1..2};do
        local nextyyyymm=$( fcal_nextmonth $intmonth $yyyy )
        month=$( date -d ${nextyyyymm}01 +%m )
        intmonth=$( echo "$month*1" | bc )
        yyyy=$( date -d ${nextyyyymm}01 +%Y )
    done

    echo $nextyyyymm 
}

get_intfwin2icyyyymm(){
    #description:   create YYYYMM dir to have symbolic links for ens1 to 10 winners
    local int
    local fin=$1
    local month=$( basename $fin | rev | cut -d'.' -f2 | cut -d'_' -f2 | rev  )
    local intmonth=$( echo "$month*1" | bc )
    local yyyy=$( basename $fin | rev | cut -d'.' -f2 | cut -d'_' -f1 | rev )

    for int in {1..2};do
        local prevyyyymm=$( fcal_previousmonth $intmonth $yyyy )
        month=$( date -d ${prevyyyymm}01 +%m )
        intmonth=$( echo "$month*1" | bc )
        yyyy=$( date -d ${prevyyyymm}01 +%Y )
    done

    echo $prevyyyymm 
}

#edit_walltime(){
#    #description:   edit walltime in gcm-run.j
#    local _d=$1;shift
#    local _expwtime=$1;shift
#    local _wtime=$1
#    local fedit=gcm_run.j
#    local feditbak=gcm_run.j.walltime
#    local statussed=999
#
#    #+++++ cd to d (start) +++++
#    cd  $_d
#    if [[ -f $feditbak ]];then rm -f $feditbak;fi
#
#    local oldstr=$( grep -i walltime $fedit )
#    local newstr=$( echo $oldstr | sed 's#'$_expwtime'#'$_wtime'#' )
#    sed -i.walltime 's/'"$oldstr"'/'"$newstr"'/' $fedit
#
#    if (( $? == 0 ));then
#        #todo:  test if sed worked
#        local newwtime=$( pbs_wtime $fedit | cut -d':' -f1 ) 
#        if (( $newwtime != $_wtime )) && [[ -f $feditbak ]];then
#            mv $feditbak $fedit
#            wmessage "sed failed 998"
#        fi
#    else
#        wmessage "sed failsed 997"
#    fi
#
#    cd - >/dev/null
#    #+++++ cd to d ( end ) +++++
#
#    return
#}

exp_editwalltime() {

    #local _usage="a function to:
    #        edit walltime in gcm*.j file. gcm*.j.bak is created as backup.
    #        Usage: ${FUNCNAME[0]} [a full patht to gcm*.j file] [amoutn of time in format of xx:xx:xx]"
    #local OPTIND _option h

    #while getopts 'h' _option;do
    #    case "$_option" in 
    #        h) echo "$_usage" >&2; return 0;;
    #    esac
    #done

    local _fin=$1
    local _timein="$2"
    [[ -z $_fin    ]] && die "(${FUNCNAME[0]}) a full path to gcm*.j file is an required input"
    [[ -z $_timein ]] && die "(${FUNCNAME[0]}) amount of time is an required input"

    local _bl_success=1
    local _ctime=$( grep walltime $_fin | head -1 | cut -d'=' -f2 )
    local _char2=$( echo $_ctime | cut -d':' -f2 )
    local _char3=$( echo $_ctime | cut -d':' -f3 )
    
    #todo: minute and second numbers have to be 2 digits.
    if (( ${#_char2} == 2 )) && (( ${#_char3} == 2 ));then
        sed -i.bak 's/'"$_ctime"'/'"${_timein}"'/g' $_fin
        if (( $? > 0 ));then _bl_success=0;fi
    fi
    
    return $_bl_success 
}



edit_qname(){
    #description:   edit queue name in gcm-run.j
    local _d=$1;shift
    local qid=$1;shift
    local oldqname=$1;shift
    local newqname=$1
    local strpns1="#PBS -q "
    local fedit=gcm_run.j
    local feditbak=gcm_run.j.qname

    #+++++ cd to d (start) +++++
    cd  $_d
    if [[ -f $feditbak ]];then rm -f $feditbak;fi

    if [[ "$qid" == "normal" ]];then
        local oldstr=$( grep normal gcm_run.j | tail -1 )
        local newstr=$( echo $oldstr | sed 's/##/#/' )

        sed -i.qname 's/\<'$oldstr'\>/'$newstr'/' $fedit
        local status_sed=$?
    else    
        local oldstr1=$( grep -n -i pbs $fedit | grep -v "##" | grep "\-q" | grep -i $oldqname )
        local numstr=$( echo $oldstr1 | cut -d':' -f1 )
        local oldstr=$( echo $oldstr1 | cut -d':' -f2 )
        local newstr=$( echo $oldstr | sed 's#\<'$oldqname'\>#'$newqname'#' )
        sed -i.qname "${numstr}s/$oldstr/$newstr/" $fedit
        local status_sed=$?
    fi
    
    if (( $status_sed == 0 ));then
        #todo:  test if sed worked
        grep -i pbs $fedit | grep -v "##" | grep "\-q" | grep -i $newqname 2>/dev/null
        if (( $? > 0 )) && [[ -f $feditbak ]];then
            mv $feditbak $fedit
            wmessage "sed failed 998"
        fi
    else
        wmessage "sed failed 997"
    fi
    

    cd - >/dev/null
    #+++++ cd to d ( end ) +++++
    return
}

edit_model(){
    #description:   edit model name in gcm-run.j
    local _d=$1;shift
    local oldmname=$1;shift
    local newmname=$1
    local fedit=gcm_run.j
    local feditbak=gcm_run.j.model
    local statussed=999

    #+++++ cd to d (start) +++++
    cd $_d
    if [[ -f $feditbak ]];then rm -f $feditbak;fi

    local oldstr=$( grep -i pbs $fedit | grep -v "##" | grep aoe | grep -i $oldmname )
    local newstr=$( echo $oldstr | sed 's#'$oldmname'#'$newmname'#' )
    sed -i.model 's/'"$oldstr"'/'"$newstr"'/' $fedit

    if (( $? == 0 ));then
        #todo:  test if sed worked
        grep -i pbs $fedit | grep -v "##" | grep aoe | grep -i $newmname 2>/dev/null 
        if (( $? > 0 )) && [[ -f $feditbak ]];then
            wmessage "sed failed 998"
            mv $feditbak $fedit
        fi
    else
        wmessage "sed failed 997"
    fi
    cd - >/dev/null
    #+++++ cd to d ( end ) +++++

    return
}

getnumcore(){
    local _model=$1
    local thst=$( get_host )
    
    if [[ "$thst" == "pfe" ]];then
        declare -A _arrcores=( ["rom_ait"]=128 ["sky_ele"]=40 ["cas_ait"]=40 ["bro"]=28 ["bro_ele"]=28 ["has"]=24 ["ivy"]=20 )
    elif [[ "$thst" == "dis" ]];then
        declare -A _arrcores=( ["hasw"]=28 ["sky"]=40 ["cas"]=40 )
    fi

    local _numcore=${_arrcores["$_model"]}
    echo $_numcore
}

calcnumcore(){
    #Note:  03/29/2023 Use exp_calccores_async
    local _model=$1;shift
    local _nx=$1;shift
    local _ny=$1;shift
    local _numcore_geos=$( echo "$_nx*$_ny" | bc -l )
    local _numcore_model=$( getnumcore $_model )

    if (( $_numcore_model >= 100 ));then
        local nodesasync=1
    else
        local nodesasync=2
    fi

    #todo:  calculate a number of nodes 
    local _numnode_geos_calc=$( printf '%.1f' $( echo "$_numcore_geos/$_numcore_model" | bc -l ))
    local _numnode_geos_int=$( echo $_numnode_geos_calc | cut -d'.' -f1 )
    local _numnode_geos_dec=$( echo $_numnode_geos_calc | cut -d'.' -f2 )

    #todo:  rounding up if decimal is greater than 0
    if (( $_numnode_geos_dec > 0 ));then
        _numnode_geos=$( echo "$_numnode_geos_int + 1" | bc )
    elif (( $_numnode_geos_dec == 0 ));then
        _numnode_geos=$_numnode_geos_int
    fi

    local _numnode_total=$( echo "$_numnode_geos+$nodesasync" | bc -l )

    #todo:  calculate a number of extra cores for async run
    #local _epes=$( echo "$nodesasync*$_numcore_model" | bc -l )
    local _epes=$( echo "$_numnode_total*$_numcore_model - $_numcore_geos" | bc -l )
    local _results=${_numnode_total}:${_epes}:${nodesasync}

    echo "$_results" 
}

runpostedit(){
    #note:  this function was used to run run_postedit in $GSCR. However, a similar script, postedit.sh,
    #       is available as of 10/05/2020. Use it instead (save this code for now).

    local runhere=0
    if (( $runhere == 1 ));then
        #todo:  check GSCR and run_postedit.sh
        if [[ -z $GSCR ]];then die "GSCR is undefined";fi
        if [[ ! -f $GSCR/run_postedit.sh ]];then die "$GSCR/run_postedit.sh does not exist";fi
   
        local _dexp=$1
        local fhis=HISTORY.rc
        local fhis1=HISTORY_1.rc
        
        msg_wheader_userdefined 40 - "run_postedit.sh"

        #+++++ cd to $dexp (start) +++++ 
        #todo:  change symlink for HISTORY.rc to HISTORY_1.rc
        cd $_dexp
        local fhislinkorg=$( readlink -f HISTORY.rc  | rev | cut -d'/' -f1 | rev )
        if [[ $fhislinkorg != $fhis1 ]];then 
            unlink $fhis
            ln -s $fhis1 $fhis
        fi
        cd - >/dev/null
        #+++++ cd to $dexp ( end ) +++++ 

        #+++++ cd to $GSCR (start) +++++ 
        cd $GSCR
        if (( $writetofile == 1 ));then
            ./run_postedit.sh -am -d $_dexp >> $fmessage 2>&1
        else
            ./run_postedit.sh -am -d $_dexp >&2
        fi

        cd - >/dev/null
        #+++++ cd to $GSCR ( end ) +++++ 
        
        #+++++ cd to $dexp (start) +++++ 
        #todo:  change back to a symlink for HISTORY.rc to HISTORY_2.rc
        cd $_dexp 
        local fhislink=$( readlink -f HISTORY.rc  | rev | cut -d'/' -f1 | rev )
        if [[ $fhislinkorg != $fhislink ]];then 
            unlink $fhis
            ln -s $fhislinkorg $fhis
        fi
        cd - >/dev/null
        #+++++ cd to $dexp ( end ) +++++ 
    fi
    
    echo "${FUNCNAME[0]} is no longer available. Use postedit.sh."
}

format_lfefout(){
    #description:   write out all outputs for dexp on arc host in formatted text. 
    local _dexp=$1;shift
    local fnum_calc=$1;shift
    local fmsg=$1;shift
    local arr=( "$@" )
    
    local _fcstdate=$( echo $_dexp | rev | cut -d'/' -f2 | rev )
    local _ensm=$( echo $_dexp | rev | cut -d'/' -f1 | rev )

    local ftmp=tmp_${strscr}_${FUNCNAME[0]}_${strexpid}_${_fcstdate}${_ensm}
    [[ -f $ftmp ]] && rm -f $ftmp
    
    for f in ${arr[@]};do
        #local fsize=$( numfmt --to=iec --format="%.2f" --padding=10 $( echo $f | cut -d':' -f2 ))
        local fsize=$( numfmt --to=iec --padding=10 $( echo $f | cut -d':' -f2 ))
        local fdate=$( date -d @$( echo $f | cut -d':' -f1 ) +%Y%m%d_%H%M )
        local fname=$( echo $f | cut -d':' -f3 )
        wtof $ftmp "  $fsize $fdate $fname"
    done 

    #todo:  check file numbers. 1 is for missing files, 0 means coll has all files. 
    if (( ${#arr[@]} != $fnum_calc ));then 
        local status_day=1 
    else 
        local status_day=0
    fi
    if [[ -f $ftmp && -f $fmsg && -n $fmsg ]];then
        sort -k3 $ftmp >> $fmsg
        echo >> $fmsg
    elif [[ -f $ftmp ]];then
        sort -k3 $ftmp >&2
        echo >&2
    fi

    [[ -f $ftmp ]] && rm -f $ftmp

    
    echo $status_day
}

getlfefout_formatted(){
    #write outputs available on arc host.
    #Usage:  ${FUNCNAME[0]} [ exp full path]
    #Output: Output file name"

    [[ -z $1 ]] && die "(${FUNCNAME[0]}) a full path to a forecast exp is required"
    [[ -z $2 ]] && die "(${FUNCNAME[0]}) a full path to an archive dir is required"

    local dexp=$1;shift
    local darch=$1;shift
    local _nummon=$1

    local fhis2=$dexp/HISTORY_2.rc
    local arrcoll=($( exp_getcollections $fhis2 ))
    local arrcolldiu=($( exp_getcolldiurnal $fhis2 ))
    #local writetofile=1
    local strmom=ocean_
    local blfmiss=true
    local coll str arrfout statuscoll arrcoll_missout
        
    [[ -z $fcstdate ]] && set_rstfcstdate $dexp
    local thisfmessage=$cdir/message_${strscr}_${FUNCNAME[0]}_${strexpid}_${fcstdate}${ensm}
    local     flfefile=$cdir/tmp_${strscr}_${FUNCNAME[0]}_${strexpid}_${fcstdate}${ensm}_lfefout

    msg_newfile $thisfmessage
    [[ -f $flfefile ]] && rm -f $flfefile

    getlfefout $dexp $darch $flfefile 
    [[ ! -f $flfefile ]] && die "FAILED to retrieve $flfefile" && return

    #todo:  write out mom outputs
    arrfout=($( cat $flfefile | grep $strmom | sort -k2 | sed 's/ /:/g' ))

    if (( ${#arrfout[@]} > 0 )) ;then
        wtof $thisfmessage "MOM outputs on $hstarc (total=$numfmom_calc):"
        statuscoll=$( format_lfefout $dexp $numfmom_calc $thisfmessage "${arrfout[@]}" )
        (( $statuscoll > 0 )) && arrcoll_missout+=( $strmom )
    elif (( ${#arrfout[@]} ==  0 )) ;then
        arrcoll_missout+=( $coll )
    fi

    #todo:  write out collection outputs. 
    for coll in ${arrcoll[@]};do
        [[ $coll == *"_1mo_"* ]] && local blmonthly=true || blmonthly=false

        local numout=$( calc_numout $_nummon $capric_yyyymmdd $coll $fhis2 $blmonthly )
#[[ "$coll" == "ocn_tavg_1dy_glo_T1440x1080_slv" ]] && \
#wmessage lineno = $LINENO $coll $_nummon $numout   || \
#continue
#exit
        arrfout=($( cat $flfefile | grep -w $coll | sort -k2 | sed 's/ /:/g' ))

        if (( ${#arrfout[@]} > 0 )) ;then
            wtof $thisfmessage "$coll on $hstarc (total=$numout):"
            statuscoll=$( format_lfefout $dexp $numout $thisfmessage "${arrfout[@]}" )
            (( $statuscoll > 0 )) && arrcoll_missout+=( $coll )
        elif (( ${#arrfout[@]} ==  0 )) ;then
            arrcoll_missout+=( $coll )
        fi
    done
    
    if (( ${#arrcoll_missout[@]} > 0 ));then
        blfmiss=true
        wtof $thisfmessage 
        wtof $thisfmessage 
        wtof $thisfmessage "There are missing outputs on $hstarc from these collections:"
        printf '    %s\n' "${arrcoll_missout[@]/./ }" >> $thisfmessage
        wtof $thisfmessage 
    else
        blfmiss=false
    fi
    [[ -f $flfefile ]] && rm -f $flfefile

    echo $thisfmessage:$blfmiss
}

getpfefout_formatted(){
    #description:   write available output on pfe with date & size
    local _dexp=$1;shift
    local _coll=$1;shift
    
    [[ -z $fcstdate ]] && set_rstfcstdate $_dexp
        
    local ftmp=$cdir/tmp_${strscr}_${FUNCNAME[0]}_${strexpid}_${fcstdate}${ensm}

    [[ -f $ftmp ]] && rm -f $ftmp
        
    local arr=($( find $_dexp/$_coll/* -maxdepth 0 -type f -name "*${_coll}*" 2>/dev/null | xargs -i basename {} )) 

    wmessage "$_coll output on pfe (total=${#arr[@]}) ..."
    for f in ${arr[@]};do
        #local fsize=$( numfmt --to=iec --format="%.2f" --padding=10 $(stat --printf="%s" $dexp/$f ))
        local fsize=$( numfmt --to=iec --padding=10 $(stat --printf="%s" $dexp/$_coll/$f ))
        local fdate=$( date -d @$(stat --printf="%Y" $dexp/$_coll/$f ) +%Y%m%d_%H%M )
        wtof $ftmp "  $fsize $fdate $f"
    done

    if [[ -n $writetofile ]] && (( $writetofile == 1 ));then 
        if [[ -f $ftmp ]];then
            sort -k3 $ftmp >> $fmessage 
        elif (( ${#arr[@]} == 0 ));then 
            echo "$_coll - missing outputs from pfe" >> $fmessage
        fi
    elif [[ -f $ftmp ]];then
        sort -k3 $ftmp >&2

    elif (( ${#arr[@]} == 0 ));then 
        echo "$_coll - missing outputs from pfe" 
    fi
    wmessage 
    [[ -f $ftmp ]] && rm -f $ftmp

    return
}
seas_rst_ready(){
    #description:   check if all rst tar files exists. If so, 
    #               returns an array with full path of existing files.

    local ddir=$1;shift
    local _arr=( "$@" )
    local seasdate arroutput

    for seasdate in ${_arr[@]};do
        local numfrst=$( find $ddir/* -type f -name "restarts.e$seasdate*.tar" 2>/dev/null | wc -l )
        if (( $numfrst == 1 ));then
            arroutput+=($( find $ddir/* -type f -name "restarts.e$seasdate*.tar" 2>/dev/null ))
        fi
    done
    echo "${arroutput[@]}"
}

seas_rst_extracted(){
    #description:   check if all necessary files for perturbation proceesses
    #               are extracted from daily rst for a given season
    #note:  input format
    #           yyyyseas=yyyy (year for season; for djf, use year for jan and feb.)
    #               seas=djf,mam,jja,son
    #_numfextracted_each=[integer] (a number of files which needs to be extracted from each daily rst)
    #               _dir=where yyyy_seas dir is (/parent/dir/yyyy_seas)
    #output: if all necessary files are extracted, this function returns true

    local yyyyseas=$1;shift
    local seas=$1;shift
    local _numfextracted_each=$1;shift
    local _dir=$1
    [[ -z $perc ]] && local perc=0.99 
    
    local arrseasdates=($( fcal_seasdates $yyyyseas $seas )) 
    local numfextracted_total_calc=$( echo "scale=2; ${#arrseasdates[@]}*${_numfextracted_each}*$perc" | bc -l )
    local numfextracted_total=$( find $_dir/*  -type f 2>/dev/null | wc -l ) 

    if (( $( echo "$numfextracted_total >= $numfextracted_total_calc" | bc -l )  ));then

    local _arrfpst_avail=( $dexp/post/gcm_post_full.j $dexp/post/gcm_post_part.j $dexp/post/gcm_post.j )
    local _fpst=($( printf '%s\n' ${_arrfpst_avail[@]} | xargs -i bash -c "[[ -f {} ]] && echo {}" 2>/dev/null | head -1 ))
        local blready=true
    else
        local blready=false
    fi
    echo $blready
}

seas_allrstpertexist(){
    #description:   check if all perturabtion files exists on lfe for the seasons, whose dir exists
    #               in output/rstextr dir. 
    #input:         ${FUNCNAME[0]} [ a file with rst name on lfe ] [ array with yyyy_seas ]
    #output:        an array of seasons (yyyy_sss) that all perturbation are ready on lfe.
    
    local _flst_lfe=$1;shift
    local arrin=( "$@" )
    local _arr=() 
    local blmissing=false
    
    [[ ! -f $_flst_lfe ]] && die "(${FUNCNAME[0]}) file does not exist: $_flst_lfe"

    for input in ${arrin[@]};do
        local arrseasfcstdates=($( fcal_seasfcstdates $( echo $input | cut -d'_' -f2 ) $( echo $input | cut -d'_' -f1 ) )) 
        local arrseasicdates=($( printf '%s\n' ${arrseasfcstdates[@]} | xargs -i bash -c 'date -d "{} -1days" +%Y%m%d'))

        #todo:  get all pert files should exist on lfe in order to delete rstextr 
        local arrallfrst=($( printf '%s_pert.tar\n' ${arrseasicdates[@]} ))

        #todo:  get all pert files that exist on discover
        local arrdisfrst=($( find $dpert/* -maxdepth 0 -type f -name "*_pert" 2>/dev/null | xargs -i basename {} ))

        #todo:  get all pert files that exist on lfe
        local arrlfefrst=($( grep _pert.tar $_flst_lfe | rev | cut -d' ' -f1 | rev ))

        #todo:  if nothing saved on lfe, no dir in rstextr should be deleted
        (( ${#arrlfefrst[@]} == 0 )) && continue 

        #todo:  if anything files are still on discover, dirs in rstextr will NOT be removed.
        (( ${#arrdisfrst[@]} > 0 )) && continue
            
        for fpert in ${arrallfrst[@]};do
            grep -w $fpert $_flst_lfe 2>&1 >> /dev/null 
            local thisstatus_grep=$?
            if (( $thisstatus_grep > 0 ));then
                local blmissing=true
                break
            else
                local blmissing=false
            fi
        done 
            
        ! $blmissing && _arr+=( $input )

    done 

    echo "${_arr[@]}"
}

transfer_screen(){
    #!!!!!NOTE: This function is not in use at all as of 2/19/2021. But keep it for now !!!!!!

    #description:   transfer files from discover to lfe using sup
    #transfer_screen [a file to transfer] [destination dir without hst name] [stderr file name] 
    #sample name for stderr $cdir/message/message_${strscr}_suptransfer

    local   ftrans=$1;shift
    local    _ddes=$1;shift
    local _fstderr=$1;shift
    local _scrname=$1

    #todo:  create stderr file 
    [[ ! -f $_fstderr ]] && $_fstderr 

    #todo:  starts to transfer
    msg_wheader_thisfile $_fstderr

    echo >> $_fstderr
    echo "Transfer : $ftrans" >> $_fstderr
    echo >> $_fstderr

    sup test -d lfe:$_ddes
    if (( $? == 1 ));then
        sup mkdir -p lfe:$_ddes
    fi
    
    screen -dmS $_scrname bash -c "sup rsync -az $ftrans lfe:$_ddes/ >> $_fstderr 2>&1"
    return

}

exp_getfname_pfe(){
    #note:  this function is similar to getfname_pfe funtion in run_gcmarch.sh
    #description:   goet all output file name with byte size
    local _dexp=$1;shift
    local _fexistpfe=$1
    [[ -f $_fexistpfe ]]    && rm -f $_fexistpfe
    [[ -z $fcstdate ]]      && set_rstfcstdate $_dexp

    [[ -z $collsst ]]       && local collsst=sst_tavg_1dy_glo_L720x361_slv
    [[ -z $strscrach ]]     && local strscrach=scratch 
    [[ -z $strrst ]]        && local strrst=restarts
    [[ -z $strmom ]]        && local strmom=MOM_Output
    [[ -z $strmom_search ]] && local strmom_search=ocean_daily
    
    find $_dexp/* -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*.nc*" 2>/dev/null  | grep -Ev "$collsst|$strscrach" | xargs -i stat --printf="%Y %s %n\n" {} | sort -V > $_fexistpfe
    [[ -d $_dexp/$strrst ]] && find $_dexp/$strrst/* -type f -name "${strrst}*.tar" 2>/dev/null| xargs -i stat --printf="%Y %s %n\n" {} >> $_fexistpfe
    [[ -d $_dexp/$strmom ]] && find $_dexp/$strmom/* -type f -name "${strmom_search}*.nc*" 2>/dev/null | xargs -i stat --printf="%Y %s %n\n" {} >> $_fexistpfe
    return
}

exp_getfname_lfe(){

    getlfefout $1 $2 $3
    return
}

getlfefout(){    
    #description:   createa  a file with a list of  archived output file name on lfe along with byte size.
    #Usage:          ${FUNCNAME[0]}  [ exp full path ]
    #Output Format:  a file (existing_lfefile_[exp])

    local dexp=$1;shift
    local dlou=$1;shift
    local flfefile=$1
    #local flfefile_bname=$( basename $flfefile )

        [[ -z $dexp ]] && die "(${FUNCNAME[0]}) an exp full path is a required input"
        [[ -z $dlou ]] && die "(${FUNCNAME[0]}) an archive full path is a required input"
    [[ -z $flfefile ]] && die "(${FUNCNAME[0]}) a variable, flfefile, has to be defined"
    [[ -f $flfefile ]] && rm -f $flfefile

    local thishst=$( get_host )

    local fcstdate=$(echo $dexp | rev | cut -d'/' -f2 | rev)
    local ensm=$(echo $dexp | rev | cut -d'/' -f1 | rev)
    local exp=$fcstdate/$ensm
    local strexp=$( echo $exp | sed 's#/#_#g' )
    local intdepth=1

    if [[ "$thishst" == "dis" ]];then
        local hst=dirac
    elif [[ "$thishst" == "pfe" ]];then
        local hst=lfe
    fi

    #ssh -q $hst test -f $ftmpb 2>/dev/null
    #(( $? == 0 )) && ssh -q $hst "rm -f $ftmpb" 2>/dev/null

    local strmon=monthly
    local strday=daily
    local strdiu=diurnal
    local strrst=restarts
    local strarc=archive
    local strmom=MOM_Output
    local strmom_search=ocean_daily
    #local flfefile=existing_lfefile_${strexp}
   
    if [[ $thishst == pfe ]];then 
        ssh -q $hst test -d $dlou >> /dev/null 2>&1
        local _status_test=$?
    elif [[ $thishst == dis ]];then 
        [[ -d $dlou ]] && local _status_test=0 || local _status_test=1
    fi

    if (( $? == 1 ));then
        touch $flfefile
    else
        #todo:  get a file with name of existing monthly, daily, & diurnal files from lou.
        #ssh -q $hst "find $dlou/* -maxdepth $intdepth -mindepth $intdepth -type f -name '${fcstdate}*.nc*' -o -name '*${strrst}*.tar' | grep -v bbftp.tmp | xargs -i stat --printf="'"'"%Y %s %n\n"'"'" {} > $ftmpb" 2>/dev/null
        #ssh -q $hst cp -p $ftmpb $flfefile
        #[[ -f $flfefile ]] && ssh -q $hst "rm -f $ftmpb" 2>/dev/null

        if [[ $thishst == pfe ]];then 
            if ssh -q $hst true >>/dev/null 2>&1; then 
                ssh -q $hst "find $dlou/* -maxdepth $intdepth -mindepth $intdepth -type f -name '${fcstdate}*.nc*' -o -name '*${strrst}*.tar' -o -name '${strmom_search}*'| grep -v bbftp.tmp | xargs -i stat --printf="'"'"%Y %s %n\n"'"'" {} > $flfefile" 2>/dev/null
            else
                touch $flfefile
            fi
        elif [[ $thishst == dis ]];then 
             find $dlou/* -maxdepth $intdepth -mindepth $intdepth -type f -name "${fcstdate}*.nc*" -o -name "*${strrst}*.tar" -o -name "${strmom_search}*" 2>/dev/null | grep -v bbftp.tmp | xargs -i stat --printf="%Y %s %n\n" {} >> $flfefile 2>/dev/null
        fi
        
    fi

    return
}

get_fwftmsz(){
    #description:   use this function mainly when transfering files from external 
    #               host to lfe (i.e. discover -> lfe)
    local _dir=$1;shift
    local _fout=$1
    local flistlfe=wftmsz_flist

            
    sup ssh -q lfe test -f $_dir/$flistlfe >> /dev/null 2>&1
    local status_fexist=$?

    (( $status_fexist > 0 )) && exit

    sup rsync -az lfe:$_dir/$flistlfe $_fout
    
    return
}

get_fwftmsz_nosup(){
    #description:   use this function to transfer files within NAS system. (pfe -> lfe)
    local _dir=$1;shift
    local _fout=$1
    local flistlfe=wftmsz_flist
    local _hst=$( get_host ) 

    if [[ "$_hst" == "pfe" ]]; then  
           
        [[ -z $hstarc ]] && local hstarc=lfe

        if  ssh -q $hstarc true >>/dev/null 2>&1; then 
            ssh -q $hstarc test -f $_dir/$flistlfe
            status_fexist=$?
            (( $status_fexist > 0 )) && exit

            ssh -q $hstarc "cp -p $_dir/$flistlfe $_fout"
            #rsync -aqz lfe:$_dir/$flistlfe $_fout
            #/usr/local/bin/shiftc --no-mail --wait lfe:$_dir/$flistlfe $_fout 2>&1 >> /dev/null
        fi

    elif [[ "$_hst" == "dis" ]]; then  
        sup test -f lfe:$_dir/$flistlfe
        status_fexist=$?
        (( $status_fexist > 0 )) && exit

        sup scp -qp lfe:$_dir/$flistlfe $_fout
    fi

    return
}

calc_numout(){
    #description:   calculate a number of outputs based on collection's monthly setting
    #               and a number of month exp ran.
    #note   :   if rst date is NOT the last day of the month 
    #               a number of outputs:     monthly = a number of months (3 or 10)
    #               a number of outputs: non-monthly = a number of months - 1 (2 or 9)
    #
    #           elif rst date is the last day of the month 
    #               a number of outputs:     monthly = a number of months + 1 (4 or 11)
    #               a number of outputs: non-monthly = a number of months - 1 (3 or 10)
    #note   : Once number of months is figured out, then multiple by 2 (monthly & daily) or 3 (monthly, daily, and diurnal)
      
    local _nummon=$1;shift
    local _icdate=$1;shift
    local _coll=$1;shift
    local _hist=$1;shift
    local _blmon=$1;shift
    local _numout1 _numout

    local collmon=$( grep $_coll.monthly $_hist 2>/dev/null )
    [[ -n $collmon ]] && local blmon=true || local blmon=false

    local collfreq=$( echo $_coll | cut -d'_' -f3 )
    [[ "$collfreq" == "1dy" ]] && local bl1dy=true || local bl1dy=false

    [[ "$_coll" == "ocn_tavg_1dy_glo_T1440x1080_slv" ]] && local blocn1dy=true || local blocn1dy=false

    local blthelastdayofthemonth=$( fcal_lastdayofthemonth $_icdate )
    local blthefirstdayofthemonth=$( fcal_firstdayofthemonth $_icdate )
    
    if $blthelastdayofthemonth;then
        if $blmon;then
            _numout1=$( echo "$_nummon + 1" | bc )
        else
            _numout1=$_nummon
        fi
    else
        if $blmon;then
            _numout1=$_nummon
        else
            _numout1=$( echo "$_nummon - 1" | bc )
        fi
    fi

    if $blmon;then 
        _numout=$_numout1 

    else
        if $blthefirstdayofthemonth && $blocn1dy;then
            _numout=$( echo "( $_numout1 + 1 ) * 2 " | bc )
#wmessage lineno = $LINENO $_numout1 $_numout
        elif $bl1dy ;then
            _numout=$( echo "$_numout1 * 2 " | bc )  
        else
            _numout=$( echo "$_numout1 * 3 " | bc )  
        fi
    fi
#    ! $blmon && _numout=$(( _numout - 1 ))

    echo $_numout 
}


exp_editqgrn(){
    #description:   change PBS statement for queue. 
    local qid=$1;shift
    local fgrn=$1
    local fsed=${FUNCNAME[0]}_sedfile

    local    blres2res=false
    local blres2nonres=false
    local blnonres2res=false

    [[ -f $fsed ]] && rm -f $fsed
    [[ -z $qid ]] && exit
   
    #todo:  check if gcm_Run.j was executed on reserved node
    #grep "#PBS -l " $fgrn | grep 122 | grep -Ev '^##|^$|^ ' | grep "select" >/dev/null 
    grep "#PBS -l " $fgrn | tail -1 | grep -Ev '^##|^$|^ ' | grep "select" >/dev/null 
    local status_thisgrep=$?
   
    (( $status_thisgrep == 0 )) && local blcreserv=true || local blcreserv=false

    #local model_cur=$( grep "#PBS -l " $fgrn | grep -Ev '^##|^$|^ ' | grep 122 | grep select | cut -d':' -f4 | cut -d'=' -f2 )
    local model_cur=$( grep "#PBS -l " $fgrn | grep -Ev '^##|^$|^ ' | grep select | cut -d':' -f4 | cut -d'=' -f2 | tail -1 )
    local queue_cur=$( grep "#PBS -q " $fgrn | grep -Ev '^##|^$|^ ' | rev | cut -d' ' -f1 | rev )
    
    #todo:  figure out if queue is set for reserved nodes in gcm_run.j
    if [[ "$queue_cur" == normal || "$queue_cur" == long ]];then
        local blrunres_cur=false
    else
        local blrunres_cur=true
    fi
    
    if [[ "$qid" == normal || "$qid" == long ]];then
        local blrunres_new=false
        local queue_new=normal
        local model_new=sky_ele
    else
        local blrunres_new=true
        #local queue_new=$( res_name $qid )
        local queue_new=$qid
        local model_new=$( res_mname $qid )
    fi

    #todo:  exit if model name and queue name are the same for current and new PBS setting
    if [[ $queue_cur == $queue_new && $model_cur == $model_new ]];then
        :
    else 
        #todo:  determine setting change from reserved node to non-reserved or vice versa
        if $blrunres_cur && $blrunres_new ;then
            local blres2res=true
        elif $blrunres_cur && ! $blrunres_new;then
            local blres2nonres=true
        elif ! $blrunres_cur && $blrunres_new;then
            local blnonres2res=true
        fi

        #todo:  edit gcm_run.j 
        if $blres2res || $blres2nonres;then
            
            #todo:  find current model setting for reserved node
            #local strmdl_reserve=$( grep model $fgrn 2>/dev/null | grep 122 | grep -Ev '^##|^$|^ ' | grep aoe= )
            #local strmdl_reservenum=$( grep -n model $fgrn 2>/dev/null | grep 122 | grep -v '^##|^$|^ ' | grep aoe= | cut -d':' -f1 )           
            local strmdl_reserve=$( grep model $fgrn 2>/dev/null | grep -Ev '^##|^$|^ ' | grep aoe= | tail -1 )
            local strmdl_reservenum=$( grep -n model $fgrn 2>/dev/null | grep -v '^##|^$|^ ' | grep aoe= | cut -d':' -f1 | tail -1  )
            local   strq_reserve=$( grep -A1 -n "$strmdl_reserve" $fgrn 2>/dev/null | tail -1 | cut -d'-' -f2- )

            #todo:  check these vars are defined
            [[ -n $strmdl_reserve && -n $strmdl_reservenum && -n $strq_reserve ]] && local blproceed=true || local blproceed=false
            
            ! $blproceed && wmessage "(${FUNCNAME[0]}) variables are undefined (something wrong with gcm_run.j)" && return

            local strmdl_reserve_replace=$( echo $strmdl_reserve | sed 's/'$model_cur'/'$model_new'/g' )

            local strq_reservenum=$(( strmdl_reservenum + 1 ))
            local queue_cur=$( echo $strq_reserve | rev | cut -d' ' -f1 | rev  )
            local strq_reserve_replace=$( echo $strq_reserve | sed 's/'$queue_cur'/'$queue_new'/g' )
        
            if $blres2res;then

                wmessage 
                wmessage "reserved -> reserved"
                wmessage 
        
                if [[ "$model_cur" != "$model_new" ]];then
                    wmessage " line number : ${strmdl_reservenum}"
                    wmessage "replace this : ${strmdl_reserve}"
                    wmessage "    ... with : ${strmdl_reserve_replace}"
                    wmessage
                    echo "${strmdl_reservenum}s/${strmdl_reserve}/${strmdl_reserve_replace}/" >> $fsed
                fi
              
                if [[ "$queue_cur" != "$queue_new" ]] ;then
                    wmessage " line number : ${strq_reservenum}"
                    wmessage "replace this : ${strq_reserve}"
                    wmessage "    ... with : ${strq_reserve_replace}"
                    wmessage
                    echo "${strq_reservenum}s/${strq_reserve}/${strq_reserve_replace}/" >> $fsed
                fi  

            elif $blres2nonres;then
                
                wmessage "reserved -> nonreserved"
        
                #todo:  find model setting for non-reserved node
                #local strmdl_nonreserve=$( grep model $fgrn | grep 122 | grep '^##' | grep -v '^$' | grep -v '^ ' | grep -v aoe= )
                #local strmdl_nonreservenum=$( grep -n -w "$strmdl_nonreserve" $fgrn | grep 122 | grep -v '^$' | grep -v '^ ' | grep -v aoe= | cut -d':' -f1 )
                local strmdl_nonreserve=$( grep model $fgrn | grep '^##' | grep -v '^$' | grep -v '^ ' | grep -v aoe= | tail -1 )
                local strmdl_nonreservenum=$( grep -n -w "$strmdl_nonreserve" $fgrn  | grep -v '^$' | grep -v '^ ' | grep -v aoe= | cut -d':' -f1 | tail -1 )
                local strq_nonreserve=$( grep -A1 -n "$strmdl_nonreserve" $fgrn | tail -1 | cut -d'-' -f2-)
                local #strq_nonreservenum=$( grep -A1 -n "$strmdl_nonreserve" $fgrn | tail -1 | cut -d'-' -f1 ) 
                local strq_nonreservenum=$(( strmdl_nonreservenum + 1 ))
            
                #todo:  check if sky_ele is selected
                grep $model_new $strmdl_nonreserve 2>/dev/null
                local status_here=$?
                
                if (( $status_here > 0 ));then
                    local strmdl_nonreserve_sky=$( echo $strmdl_nonreserve | sed 's/'$model_cur'/'$model_new'/g' )
                fi
            
                wmessage " line number : ${strmdl_reservenum}"
                wmessage "replace this : ${strmdl_reserve}"
                wmessage "    ... with : #${strmdl_reserve}"
                wmessage
                wmessage " line number : ${strq_reservenum}"
                wmessage "replace this : ${strq_reserve}"
                wmessage "    ... with : #${strq_reserve}"
                wmessage
                wmessage " line number : ${strmdl_nonreservenum}"
                wmessage "replace this : ${strmdl_nonreserve}"
                wmessage "    ... with : ${strmdl_nonreserve_sky:1}"
                wmessage
                wmessage " line number : ${strq_nonreservenum}"
                wmessage "replace this : ${strq_nonreserve}"
                wmessage "    ... with : ${strq_nonreserve:1}"
                wmessage
            
                cat > $fsed << EOF
${strmdl_reservenum}s/${strmdl_reserve}/#${strmdl_reserve}/
${strq_reservenum}s/${strq_reserve}/#${strq_reserve}/
${strmdl_nonreservenum}s/${strmdl_nonreserve}/${strmdl_nonreserve_sky:1}/
${strq_nonreservenum}s/${strq_nonreserve}/${strq_nonreserve:1}/
EOF
            fi
    
        else
            wmessage "non-reserved -> reserved"
        
            #todo:  find model setting for reserved node
            #local strmdl_reserve=$( grep model $fgrn | grep 122 | grep '^##' | grep -Ev '^$|^ ' | grep aoe= )
            #local strmdl_reservenum=$( grep -n model $fgrn | grep 122 | grep -v '^##|^$|^ ' | grep aoe= | cut -d':' -f1 )
            local strmdl_reserve=$( grep model $fgrn | grep '^##' | grep -Ev '^$|^ ' | grep aoe= | tail -1 )
            local strmdl_reservenum=$( grep -n model $fgrn  | grep -v '^##|^$|^ ' | grep aoe= | cut -d':' -f1 | tail -1 )
            local   strq_reserve=$( grep -A1 -n "$strmdl_reserve" $fgrn | tail -1 | cut -d'-' -f2- )
            local   strq_reserve_replace="#PBS -q $queue_new"
            local   strq_reservenum=$(( strmdl_reservenum + 1 ))

            #todo:  find model setting for non-reserved node
            #local strmdl_nonreserve=$( grep model $fgrn | grep 122 | grep -Ev '^##|^$|^ ' | grep -v aoe= )
            #local strmdl_nonreservenum=$( grep -n -w "$strmdl_nonreserve" $fgrn | grep 122 | grep -Ev '^$|^ |aoe=' | cut -d':' -f1 )
            local strmdl_nonreserve=$( grep model $fgrn | grep -Ev '^##|^$|^ ' | grep -v aoe= | tail -1 )
            local strmdl_nonreservenum=$( grep -n -w "$strmdl_nonreserve" $fgrn | grep -Ev '^$|^ |aoe=' | cut -d':' -f1 | tail -1 )
            local strq_nonreservenum=$(( strmdl_nonreservenum + 1 ))
            local strq_nonreserve=$( head -n $strq_nonreservenum $fgrn | tail -1 )
        
            wmessage " line number : ${strmdl_reservenum}"
            wmessage "replace this : ${strmdl_reserve}"
            wmessage "    ... with : ${strmdl_reserve:1}"
            wmessage
            wmessage " line number : ${strq_reservenum}"
            wmessage "replace this : ${strq_reserve}"
            wmessage "    ... with : ${strq_reserve_replace}"
            wmessage
            wmessage " line number : ${strmdl_nonreservenum}"
            wmessage "replace this : ${strmdl_nonreserve}"
            wmessage "    ... with : #${strmdl_nonreserve}"
            wmessage
            wmessage " line number : ${strq_nonreservenum}"
            wmessage "replace this : ${strq_nonreserve}"
            wmessage "    ... with : #${strq_nonreserve}"
            wmessage
          
            cat > $fsed << EOF
${strmdl_reservenum}s/${strmdl_reserve}/${strmdl_reserve:1}/
${strq_reservenum}s/${strq_reserve}/${strq_reserve_replace}/
${strmdl_nonreservenum}s/${strmdl_nonreserve}/#${strmdl_nonreserve}/
${strq_nonreservenum}s/${strq_nonreserve}/#${strq_nonreserve}/
EOF
        fi
        
        sed -i.bak -f $fsed $fgrn

        wmessage "... edit before/after ..."
        if (( $writetofile == 1 ));then
            diff $fgrn.bak $fgrn >> $fmessage 2>&1
        else
            diff $fgrn.bak $fgrn 2>&1
        fi
        wmessage
    fi

    [[ -f $fsed ]] && rm -f $fsed
    return
}

create_symlinks_nbp(){
    #description:   crate symbolic link ens001-010 in dyyyymm for winners
    local _icyyyymm=$1;shift
    local _arrwin=( "$@" )
    local i

    [[ -z $dwinpfe ]] && die "var undefined: dwinpfe" && exit

    #todo:  create symbolic links on both nbp11 & 13
    for i in "${!_arrwin[@]}";do
        local ensm=ens$( printf '%03g' $(( i + 1 )) )
        local fexp=${_arrwin[$i]}
        local dexp=$DFCST/$fexp

        #+++++ cd to DFCST/winners/icyyyymm (start) +++++
        cd $dwinpfe/$_icyyyymm
          [[ -L $ensm ]] && unlink $ensm
        [[ ! -L $ensm ]] && ln -s $dexp $ensm
        cd - >/dev/null
        #+++++ cd to DFCST/winners/icyyyymm ( end ) +++++
    done

    return
}

create_symlinks_arc(){

    #description:   crate symbolic link ens001-010 in dyyyymm for winners on lfe
    #input : ic yyyymm, array of winners (YYYYMMDD/ensX)
    local _icyyyymm=$1;shift
    local _arrwin=( "$@" )
    local i

    [[ -z $_hstshort ]] && local _hstshort=$( get_host )

    #todo:  create symbolic links
    for i in "${!_arrwin[@]}";do
        local ensm=ens$( printf '%03g' $(( i + 1 )) )
        local fexp=${_arrwin[$i]}
        local dsrc=$DARCH/$fexp
        local ddes=$dwinlfe/$_icyyyymm
        local scrlfe=${strscr}_${FUNCNAME[0]}_$_icyyyymm

        if [[ "$_hstshort" == "pfe" ]];then

            if ssh -q lfe true >>/dev/null 2>&1; then 
                ssh -q lfe test -d $ddes 2>/dev/null
                local _status_dexist=$?

                if (( $_status_dexist > 0 )) ;then 
                    ssh -q lfe mkdir -p $ddes 2>/dev/null
                    ssh -q lfe test  -d $ddes 2>/dev/null
                    local _status_dexist=$?
                fi
                
                if (( $_status_dexist == 0 ));then

cat > $scrlfe << EOF
cd $ddes 
[[ -L $ensm ]] && unlink $ensm
[[ ! -L $ensm ]] && ln -s $dsrc $ensm 
cd - >/dev/null
EOF
                    ssh -q lfe 'bash -s' < $scrlfe

                    [[ -f $scrlfe ]] && rm -f $scrlfe
                fi
            fi

        elif [[ "$_hstshort" == "dis" ]];then
            if [[ -d $dsrc ]];then
                cd $ddes 
                  [[ -L $ensm ]] && unlink $ensm
                [[ ! -L $ensm ]] && ln -s $dsrc $ensm
                cd - >/dev/null
            fi
        fi
    done

    return
}

rmexp_datasubmit(){
    local _arrdexp=( "$@" )
    local _dexp fdata

    local arrfsub=($( find $dtmpdata/* -maxdepth 0 -type f -name "data_submit_*$strexpid*" 2>/dev/null ))

    if (( ${#arrfsub[@]} > 0 ));then 
        for _dexp in ${_arrdexp[@]};do
        
            #note:  some dexp are written in multiple data files.
            local arrfdata=$( grep -l $_dexp ${arrfsub[@]} 2>/dev/null )
            [[ -f ${strscr}_newfile ]] && rm -f ${strscr}_newfile
        
            for fdata in ${arrfdata[@]};do
                wmessage "... $_dexp is removed from ${fdata// /} ..."
                grep -wv "$_dexp" $fdata >> ${strscr}_newfile
        
                [[ -f ${strscr}_newfile ]] && mv ${strscr}_newfile ${fdata// /}
                [[ -f ${strscr}_newfile ]] && rm -f ${strscr}_newfile
            done 
        done
        
        #todo:  delete fdata if it's empty
        for fsub in ${arrfsub[@]};do
            local thisfsub=$( find $dtmpdata/* -type f -name $fsub -empty 2>/dev/null )
            if [[ -n $thisfsub ]];then
                wmessage "... $thisfsub is empty ... deleted"
                rm -f $thisfsub 2>/dev/null 
            fi
        done
    fi

    #for fsub in ${arrfsub[@]};do
    #    if [[ ! -s $fsub ]];then
    #        rm -f $fsub
    #        wmessage "... $fsub is empty ... deleted"
    #    fi
    #done
    return
}

note_status(){
    local _note="
Exp Status (gcm_run.j):
C3C   = 3-month run, archive, & cleaning are completed  
WC10C =  winner run, archive, & cleaning are completed 

C3    = 3-month run & archive are completed  
C3m   = exp ran more than 3months and archive is completed 
WC10  =  winner run & archive are completed 

R      or WR      = gcm_run.j is running or on queue
RR     or WRR     = gcm_run.j with RERUN=true is running or on queue
KR     or WKR     = Kill job (unnecessary run)
RdyS   or WRdyS   = Experiment is ready and will be submitted
Sub    or WSub    = Experiment needs to be added to data_submit file
UnfR   or WUnfR   = Job has not been completed
Pnc    or WPnc    = PanicStop exist

Gcm_post Status:
PMhold or WPMhold = Missing YYYYMM dir in holding/collection
PUnf   or WPUnf   = gcm_post*.j* have not been finished
PUNK   or WPUNK   = Unknown issues with gcm_post

Archive Status:
AR     or WAR     = Archiving script is running or on queue
AComp  or WAComp  = All files are archived successfully but cleaning has not been done in exp dir
AStK   or WAStK   = Archive lock exists but stuck with unknown reasons
AStuck or WAStuck = Archive is stuck for unknown reasons
AZero  or WAZero  = Archive script has not been executed
AF0    or WAF0    = Empty output file(s) exists on archive
AUnf   or WAUnf   = Missing output files in archive dir 
AUnk   or WAUnk   = Unknown issues with archive script

NOTES:
- W for winner
- Problems indicated by (W)Sub & (W)AStK are fixed by monitor.sh
"

#AStS   or WAStS   = archive is likely stuck due to running out walltime
#- Checks archive status when/if 3-month or/and 10-month run is completed
    echo  "$_note" 
}


exp_status(){
    #description:   get fcst exp status
    local _dexp=$1;shift
    local _fexistlfe=$1
    local fcapr=cap_restart
    local fcapric=cap_restartIC
    local fcapend=cap_end
    local _fcapr=$_dexp/$fcapr
    local _fcapric=$_dexp/$fcapric
    local _fcapend=$_dexp/$fcapend
    local dtmpdata=$cdir/data/submit
    local _fcomp_clean=$_dexp/clean_completed
   # local fcron=~/crontab_gcmfcstarchive_pfe20
    local strarch=gcmfcstarchive
    local farchstderr=stderr_gcmfcstarchive
    if [[ ! -f $_dexp/archive/$farchstderr ]];then
        local strarch=gcmarch
        local farchstderr=stderr_$strarch
    fi
    
    local flock=$_dexp/archive/$strarch.lock

    #todo:  check input
    [[   -z $_dexp ]] && die "(${FUNCNAME[0]}) input required"        && exit
    [[ ! -d $_dexp ]] && die "(${FUNCNAME[0]}) $_dexp does not exist" && exit
    
    set_rstfcstdate $_dexp
    [[ -n $_fexistlfe && -f $_fexistlfe ]] && count_files $_dexp $_fexistlfe || count_files $_dexp

    #setvars $_dexp

    [[ -z $fcstdate ]] && die "(${FUNCNAME[0]}) fcstdate is empty" && exit
    [[ -z $DFCST    ]] && die "(${FUNCNAME[0]}) DFCST is empty"    && exit

    #todo:  check winner
    local strfwin=winners_nino3.4_${strexpid}_
    local fyyyymmdd=$fcstdate
    local fyyyymm=$( date -d $fyyyymmdd +%Y%m )
    local fwin=$( find $cdir/output/pckwinners/$strdout/* -name "${strfwin}${fyyyymm}_??????.txt" 2>/dev/null )

    #todo:  check if this is a winners 
    if [[ -n $fwin ]];then
        grep -w "$fyyyymmdd ens$intens" $fwin >/dev/null
        (( $? == 0 )) && local blwin=true || local blwin=false
    else
        blwin=false
    fi

#C3   = 3-month run and archiving have been completed 
#C3m  = exp ran more than 3months and archiving has been completed
#WC10 = winner run and archiving have been completed
    if [[ -f $_fcomp_clean ]] ;then 
        $blwin && local statusrun="WC10C" || local statusrun="C3C"

    else
        #todo: get cap dates
        [[ -f $_fcapr   ]] && local    capr_yyyymmdd=$( cat $_fcapr | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
        [[ -f $_fcapend ]] && local realend_yyyymmdd=$( cat $_fcapend | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
        if [[ -n $fcstdate ]];then
            local nummonth1=3
            local strxmonth=$( nextXmonths $( date -d $fcstdate +%Y ) $( printf '%01g' $( date -d $fcstdate +%m ) ) 0 $(( nummonth1 + 1 ))  )
            local end_mm=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f1 )
            local end_year=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f2 )
            local end3_yyyymmdd=$end_year$end_mm"01"
        fi

        local farchrdy=$( find $DFCST/* -maxdepth 0 -type f -name 'data_archready' 2>/dev/null | tail -1 )
        local darch=$DARCH/$fyyyymmdd/ens$intens

        #todo:  check run status
        if [[ -f $_dexp/gcm_run.j && -f $_dexp/archive/run_gcmarch.sh ]];then
            cnt_jobs $_dexp

            [[ -z $num_rgrn ]] && return 
            if [[ -n $num_rgrn ]] && (( $num_rgrn > 0 ));then
                local blrunning=true 
            elif [[ -n $num_rgrn ]] && (( $num_rgrn == 0 ));then
                local blrunning=false
            fi

            if [[ -n $num_rarc ]] && (( $num_rarc > 0 ));then
                local blrunning_arch=true
            elif [[ -n $num_rarc ]] && (( $num_rarc == 0 ));then
                local blrunning_arch=false
            fi
        else
            [[ -z $num_rgrn ]] && num_rgrn=0
            [[ -z $num_rarc ]] && num_rarc=0
            local blrunning=false
            local blrunning_arch=false
        fi
        
        #todo: check if RERUN is true
        blrerun=$( grep -i "RERUN = " $_dexp/gcm_run.j  | rev | cut -d' ' -f1 | rev | tr [:upper:] [:lower:] )

        if $blrunning;then
            local statusrun=$( check_statusrunning $_dexp $blwin $blrerun )
        elif ! $blrunning;then
            local statusrun=$( check_status $_dexp $blwin $blrerun )
        else
            local statusrun=""
        fi    
    fi

    echo $statusrun        
}

check_statusrunning(){
    #description:   check gcm_run.j status
    local _dexp=$1;shift
    local _blwin=$1;shift
    local _blrerun=$1

    if $_blwin || $_blrerun ;then
        if (( $end3_yyyymmdd <=$capr_yyyymmdd && $capr_yyyymmdd < $realend_yyyymmdd ));then
            local statusrun=WR
        elif (( $capr_yyyymmdd < $end3_yyyymmdd ));then
            local statusrun=WR
        elif (( $capr_yyyymmdd > $realend_yyyymmdd ));then
            #note:  kill the job 
            local statusrun=WKR
        elif $_blrerun;then 
            local statusrun=WRR
        else
            local statusrun=WR?
        fi
    elif ! $_blwin && ! $_blrerun;then
        if (( $capr_yyyymmdd < $end3_yyyymmdd ));then
            if $_blrerun;then 
                local statusrun=RR
            else
                local statusrun=R
            fi
        elif (( $capr_yyyymmdd > $end3_yyyymmdd ));then
            #note:  kill the job 
            local statusrun=KR
        else
            local statusrun=R?
        fi
    fi
    echo $statusrun
}

check_status(){
    #description:   check gcmfcstarchive status
    local _dexp=$1;shift
    local _blwin=$1;shift
    local _blrerun=$1;shift
    local dtmpdata=$cdir/data/submit
    local fpanic=$_dexp/PANICSTOP
    local ferrarch=$_dexp/archive/$farchstderr
    local farch_tmp=$_dexp/archive/tmp_gcmarch
    [[ -z $hstshort ]] && local hstshort=$( get_host ) 

    [[ -z $blleaveout ]] && die "(${FUNCNAME[0]}) blleaveout is undefined. define it or source srcme file." && return

    if [[ "$hstshort" == "pfe" ]];then 
        [[ -z $hstarc ]] && local _hstarc=lfe || local _hstarc=$hstarc
    fi

    $_blwin || $_blrerun && local _blthis=true || local _blthis=false 

    if $blleaveout;then
        #note:  this return # outputs remain on pfe after archve is completed
        local arrcalc=($( exp_calcoutput_save $_dexp $_blthis $_dexp/HISTORY_2.rc ))

    else
        #note:  this return # outputs experment produce 
        local arrcalc=($( exp_calcoutput $_dexp $_blthis $_dexp/HISTORY_2.rc ))
    fi

    local numftotal_calc=${arrcalc[0]}

    [[ ! -d $dtmpdata ]] && die "($strscr ${FUNCNAME[0]}) $dtmpdata does not exist" && exit
    
    #todo:  check _dexp is waiting to be submitted
    local numfsub=$( find $dtmpdata/* -maxdepth 0 -type f 2>/dev/null | wc -l )

    if (( $numfsub > 0 ));then
        cat $dtmpdata/* | sort | uniq | grep $_dexp >/dev/null
        local status_grep=$?
        (( $status_grep == 0 )) && local blwillbesubmitted=true || local blwillbesubmitted=false
    else
        local blwillbesubmitted=false
    fi

    #todo:  check non-running and non-archive status 
    if $_blwin || $_blrerun ;then
        if (( $capr_yyyymmdd == $realend_yyyymmdd ));then
           local statusrun=WC10
        elif $blwillbesubmitted;then
           local statusrun=WRdyS
        elif (( $capr_yyyymmdd < $realend_yyyymmdd ));then
           local statusrun=WSub
        elif [[ -f $fpanic ]];then
           local statusrun=WPnc
        else
           local statusrun=WUnfR
        fi
    elif ! $_blwin && ! $_blrerun ;then
        if [[ -n $capr_yyyymmdd && $end3_yyyymmdd ]] && (( $capr_yyyymmdd == $end3_yyyymmdd ));then
            local statusrun=C3
        elif [[ -n $capr_yyyymmdd && $end3_yyyymmdd ]] && (( $capr_yyyymmdd > $end3_yyyymmdd ));then
            local statusrun=C3m
        elif $blwillbesubmitted;then
            local statusrun=RdyS
        elif [[ -n $capr_yyyymmdd && $end3_yyyymmdd ]] && (( $capr_yyyymmdd < $end3_yyyymmdd ));then
            local statusrun=Sub
        elif [[ -f $fpanic ]];then
            local statusrun=Pnc
        else
            local statusrun=UnfR
        fi
    fi

    #todo:  check gcm_post status 
    if [[ "$statusrun" == "C3" || "$statusrun" == "C3m" || "$statusrun" == "WC10" ]];then
        local numfdiu_calc=${arrcalc[3]}
        local numfday_calc=${arrcalc[2]}
        local numfmonpost_calc=${arrcalc[4]}
       
#wmessage lineno = $LINENO        
#wmessage $numfdiu_calc     $numfpfe_diu_cnt
#wmessage $numfmonpost_calc $numfpfe_monpost_cnt
#wmessage $numfday_calc        $numfpfe_day_cnt
##exit
            
        if [[ -f $ferrarch ]];then 
            #todo:  find missing holding/coll/YYYYMM dir
            local _arrpststatus=()
        
            local _arrfpst_sub=($( grep gcm_post. $ferrarch 2>/dev/null | grep _glo_ 2>/dev/null | grep -v Make | tr -s '[:space:]' | rev | cut -d' ' -f1 | rev | sed 's#^ *##g' | sort -V ))
            local _arrfpst=($( printf '%s\n' ${_arrfpst_sub[@]} | sort -V | uniq ))
            local _blpst_noissue=true
        
            for fpst in ${_arrfpst[@]} ;do
                _status="$( pststatus $_dexp/post/$fpst )"
                _blpst_noissue=$( echo $_status | cut -d':' -f1 ) 
                ! $_blpst_noissue && wmessage \@$LINENO $_status && break
            done
        fi

#wmessage \@$LINENO $statusrun $_blpst_noissue

        if ! $_blpst_noissue;then
            local _numfsftc=$( find $_dexp/archive/* -type f -name "gcmarch_shiftin_*_*[0-9]" 2>/dev/null | wc -l ) 

            if (( $_numfsftc > 0 )) ;then
                $_blwin && local statusrun=WPMhold || local statusrun=PMhold
            else
                $_blwin && local statusrun=WPUNK    || local statusrun=PUNK
wmessage \@$LINENO $statusrun                
            fi

        elif $_blpst_noissue;then  
        
            if $blleaveout;then
                #note:  When blleaveout is true, outputs will stays on pfe. 
                local     numdiff_diu=$(( $numfpfe_diu_cnt     - numfdiu_calc     )) 
                local numdiff_monpost=$(( $numfpfe_monpost_cnt - numfmonpost_calc )) 
                local     numdiff_day=$(( $numfpfe_day_cnt     - numfday_calc     )) 
#wmessage lineno = $LINENO
#wmessage $numfpfe_diu_cnt       $numfdiu_calc       $numdiff_diu
#wmessage $numfpfe_monpost_cnt   $numfmonpost_calc   $numdiff_monpost
#wmessage $numfpfe_day_cnt       $numfday_calc       $numdiff_day
#wmessage $blleaveout
                if (( $numdiff_diu >= 0 && $numdiff_monpost >= 0 && $numdiff_day >= 0 ));then
                    local _blcomp=true
                else
                    local _blcomp=false
                fi
                
            else
                #note:  when blleaveout is false, no outputs will stay on pfe.
                local     numdiff_diu=$(( numfdiu_calc     - $numfpfe_diu_cnt     )) 
                local numdiff_monpost=$(( numfmonpost_calc - $numfpfe_monpost_cnt )) 
                local     numdiff_day=$(( numfday_calc     - $numfpfe_day_cnt     )) 

#wmessage \@$LINENO
#wmessage $numdiff_diu
#wmessage $numdiff_monpost
#wmessage $numdiff_day
#wmessage $blleaveout

                #if (( $numdiff_diu == 0 && $numdiff_monpost == 0 && $numdiff_day == 0 ));then
                if  (( $numfpfe_diu_cnt == 0 && $numfpfe_monpost_cnt == 0 && $numfpfe_day_cnt == 0 ));then
                    local _blcomp=true
                elif [[ "$statusrun" == "C3m" ]] && (( $numdiff_diu < 0 && $numdiff_monpost < 0 && $numdiff_day < 0 ));then
                    local _blcomp=true
                else
                    local _blcomp=false
#wmessage \@$LINENO
#wmessage $numdiff_diu
#wmessage $numdiff_monpost
#wmessage $numdiff_day
#wmessage $blleaveout
                fi
            fi
#wmessage \@$LINENO $_blcomp    
            #if (( $numdiff_diu == 0 && $numdiff_monpost == 0 && $numdiff_day == 0 ));then
            if $_blcomp;then
                if $_blwin;then
                    local statusrun=WPComp
                else
                    local statusrun=PComp
                fi
            else
                if $_blwin;then
                    local statusrun=WPUnf
                else
                    local statusrun=PUnf
                fi
            fi
        fi
    fi

    #todo:  check archive status
    #if [[ "$statusrun" == "C3" || "$statusrun" == "C3m" || "$statusrun" == "WC10" ]];then
    if [[ "$statusrun" == "PComp" || "$statusrun" == "WPComp" ]];then
       if [[ -f $ferrarch ]] ;then

            #todo:  check if lock exists
            if [[ -f $flock ]];then
                local sec_Xdyago=$( date --date="2 days ago" +%s )
                local timelock=$( stat --printf=%Y $flock )
                if (( $timelock < $sec_Xdyago ));then
                    local bllockexistsold=true
                else
                    local bllockexistsold=false
                fi
            else
                local bllockexistsold=false
            fi

            grep -ni "shift id" $ferrarch >/dev/null
            local status_grep=$?

            if (( $status_grep == 0 ));then
                #todo:  check if shiftc is running
                local sftid=$( grep -i "shift id" $ferrarch 2>/dev/null| tail -1 | rev | cut -d' ' -f1 | rev  )
                local sftcstatus=$( /usr/local/bin/shiftc --status=csv | grep run | grep $sftid )

                #todo:  check if arch scr was executed after sftid
                local numline_sftid=$( grep -ni "shift id" $ferrarch 2>/dev/null | tail -1 | cut -d':' -f1 )
                local isnumber_numlinesftid=$( misc_isinteger $numline_sftid )
                if (( $isnumber_numlinesftid == 0 ));then
                    cat $ferrarch | tail -n +$numline_sftid | grep -ni "exp location" >/dev/null 
                    (( $? == 0 )) && local bllastarchrun=false || local bllastarchrun=true
                else
                    local bllastarchrun=false
                fi
            else
                local bllastarchrun=false
            fi

            if $bllastarchrun && [[ -z $sftcstatus ]];then
                local blarchstuck=false
            else 
                local blarchstuck=true
            fi

            #todo:  check a number of archived outputs on hstarc
            if [[ "$hstshort" == "pfe" ]];then 
                if ssh -q $_hstarc  true >>/dev/null 2>&1 ; then 
                    ssh -q $_hstarc "test -d $darch"
                    local _status_test=$?
                else
                    local _status_test=1
                fi

            elif [[ "$hstshort" == "dis" ]];then 
                [[ -d $darch ]] && local _status_test=0 || local _status_test=1
            fi

            if (( $_status_test == 0 ));then
                if [[ "$hstshort" == "pfe" ]];then 
                    local numfarch_all=$( ssh -q $_hstarc "find $darch/* -maxdepth 1 -mindepth 1 -type f -name "'"'"*.nc4*"'"'" | wc -l " )

                elif [[ "$hstshort" == "dis" ]];then 
                    local numfarch_all=$( find $darch/* -maxdepth 1 -mindepth 1 -type f -name "*.nc4*" 2>/dev/null | wc -l )
                fi
            else 
                local numfarch_all=0
            fi


            #todo:  check if all files are archived.
            if [[ -n $capr_yyyymmdd && $end3_yyyymmdd && $numftotal_calc && $numfarch_all ]];then
               if (( $capr_yyyymmdd == $end3_yyyymmdd )) || (( $capr_yyyymmdd == $realend_yyyymmdd ));then 
                   if (( $numftotal_calc  == $numfarch_all ));then
                        local blmissout=false
                    else
                        local blmissout=true
                    fi
               else
                   local blmissout=true
               fi
            else
                local blmissout=true
            fi
            
            if $blmissout && $bllockexistsold ;then
               local blarchstuckwithlock=true 
            else
               local blarchstuckwithlock=false
            fi

            #todo:  empty file exists on arc 
            if [[ -s $_dexp/archive/gcmarch_arcempf ]];then 
                local blfempty=true
            else
                local blfempty=false
            fi

            #todo:  check if archcompleted exists
            if [[ -f $_dexp/archive/gcmfcstarchive_archcompleted || -f $_dexp/archive/gcmarch_archcompleted ]];then
                local blarchdone=true
            else
                local blarchdone=false
            fi

            #todo:  check archive status here
            #if [[ -f $_dexp/archive/gcmfcstarchive_deloutcompleted || -f $_dexp/archive/gcmarch_deloutcompleted ]];then
            if $blarchdone;then
                if [[ "$statusrun" == "PComp" ]];then
                    if [[ -n $capr_yyyymmdd && $end3_yyyymmdd ]] && (( $capr_yyyymmdd > $end3_yyyymmdd ));then
                        statusrun=C3m
                    else
                        statusrun=C3
                    fi
                elif [[ "$statusrun" == "WPComp" ]];then
                    statusrun=WC10

                elif [[ "$statusrun" == "C3" || "$statusrun" == "C3m" || "$statusrun" == "WC10" ]];then
                    #note:  status will be C3 or WC10
                    :
                fi

            elif $_blwin || $_blrerun;then
                if (( $num_rarc > 0 ));then
                    local statusrun=WAR
                elif $blarchdone;then
                    local statusrun=WAComp
                #elif $blarchstuck && ! $blarchstuckwithlock ;then
                #    local statusrun=WAStS
                elif ! $blarchstuck && $blarchstuckwithlock ;then
                    local statusrun=WAStK
                elif $blarchstuck && $blarchstuckwithlock ;then
                    local statusrun=WAStuck
                elif $blmissout;then
                    local statusrun=WAUnf
                elif $blfempty;then
                    local statusrun=WAF0
                else
                    local statusrun=WAUnk
                fi
            elif ! $_blwin && ! $_blrerun ;then
                if (( $num_rarc > 0 ));then
                    local statusrun=AR
                elif $blarchdone;then
                    local statusrun=AComp
                #elif $blarchstuck && ! $blarchstuckwithlock ;then
                #    local statusrun=AStS
                elif ! $blarchstuck && $blarchstuckwithlock ;then
                    local statusrun=AStK
                elif $blarchstuck && $blarchstuckwithlock ;then
                    local statusrun=AStuck
                elif $blfempty;then
                    local statusrun=AF0
                elif $blmissout;then
                    local statusrun=AUnf
                else
                    local statusrun=AUnk
                fi
            fi

        elif [[ ! -f $farch_tmp ]] ;then
            if $_blwin || $_blrerun;then
                local statusrun=WAZero
            elif ! $_blwin && ! $_blrerun ;then
                local statusrun=AZero
            fi
        fi
    fi

    echo $statusrun
}

calc_numseg_winners(){
    #description:   calculate total number of segments for 
    local _fcstyyyymmdd=$1;shift
    local _fcstmm=$( echo $_fcstyyyymmdd | cut -c5-6 )
    local _fcstmmdd=$( echo $_fcstyyyymmdd | cut -c5- )
    local _fcst_intm=$( echo "$_fcstmm * 1" | bc )
    local _fcst_0101=0101
    local arr3segmon=( 1 2 3 6 7 10 )
    if [[ " ${arr3segmon[*]} " =~ " $_fcst_intm " ]];then
        [[ "$_fcst_0101" == "$_fcstmmdd" ]] && local numseg=4 || local numseg=3
    else
       local numseg=4
    fi
    echo $numseg    
}

_calc_numseg_winners(){
    #description:   calculate total number of segments for 
    local _fcstyyyymmdd=$1;shift
    local _fcstyyyy=$( echo $_fcstyyyymmdd | cut -c1-4 )

    local nummonth1=3
    local end3_yyyymm=$( nextXmonths_yyyymm $( date -d $_fcstyyyymmdd +%Y ) $( printf '%01g' $( date -d $_fcstyyyymmdd +%m ) ) 0 $(( nummonth1 + 1 )) | rev | cut -d' ' -f1 | rev  )
    local end3_yyyy=$( echo $end3_yyyymm | cut -c1-4 )
    local end3_intm=$( echo "$( echo $end3_yyyymm | cut -c5-9 )*1" | bc )

    local num2ndseg=$(( nummonth1 + 4 ))
    local end2ndseg_yyyymm=$( nextXmonths_yyyymm $( date -d $_fcstyyyymmdd +%Y ) $( printf '%01g' $( date -d $_fcstyyyymmdd +%m ) ) 0 $(( num2ndseg + 1 )) | rev | cut -d' ' -f1 | rev  )
    local end2ndseg_intm=$( echo "$( echo $end2ndseg_yyyymm | cut -c5-9 )*1" | bc )

    local nummon_total=10
    local end10_yyyymm=$( nextXmonths_yyyymm $( date -d $_fcstyyyymmdd +%Y ) $( printf '%01g' $( date -d $_fcstyyyymmdd +%m ) ) 0 $(( nummon_total + 1 )) | rev | cut -d' ' -f1 | rev )
    local end10_yyyy=$( echo $end10_yyyymm | cut -c1-4 )
    local end10_intm=$( echo "$( echo $end10_yyyymm | cut -c5-9 )*1" | bc )

    if (( $_fcstyyyy == $end10_yyyy || $end3_intm == 1 || $end2ndseg_intm == 1 || $end10_intm == 1 ));then
       local numseg=3
    else
       local numseg=4
    fi
    echo $numseg    
}    

calc_numseg_3mon(){
    
    local _fcstyyyymmdd=$1;shift
    local _endyyyymmdd=$1
    local _fcstyyyy=$( echo $_fcstyyyymmdd | cut -c1-4 )
    local nummonth1=3
    local arrend3_yyyymm=($( nextXmonths_yyyymm $( date -d $_fcstyyyymmdd +%Y ) $( printf '%01g' $( date -d $_fcstyyyymmdd +%m ) ) 0 $(( nummonth1 + 1 )) ))  #| rev | cut -d' ' -f1 | rev  )
    local end3_yyyymm=${arrend3_yyyymm[-1]}
    local end3_yyyy=$( echo $end3_yyyymm | cut -c1-4 )
    local end3_intm=$( echo "$( echo $end3_yyyymm | cut -c5-9 )*1" | bc )

    if (( $_fcstyyyy < $end3_yyyy ));then
        if (( $_endyyyymmdd == ${end3_yyyy}0101 ));then
            local out=1
        else
            local out=2
        fi
    elif (( $_fcstyyyy == $end3_yyyy ));then
        if (( $capric_yyyy < $end3_yyyy ));then
            local out=2
        else
            local out=1
        fi
    fi

    echo $out
}

calc_numseg(){
    #description:   calculate total number of segments for 
    local _fcstyyyymmdd=$1;shift
    local _endyyyymmdd=$1
    
    [[ -z $_fcstyyyymmdd || -z $_endyyyymmdd ]] && die "(${FUNCNAME[0]}) missing one or both inputs"  

    local bllday=$( fcal_lastdayofthemonth $_fcstyyyymmdd )
    local nummon=$( fcal_calcnummon $_fcstyyyymmdd $_endyyyymmdd )
    
    #note:  if the start date is the last date of the month, fcal_calcnummon will 
    #       excludes the first month. Thus add +1
    $bllday && nummon=$(( nummon + 1 ))

    if (( $nummon == 3 ));then 
        #local out=$( echo "$( calc_numseg_3mon $_fcstyyyymmdd $_endyyyymmdd)*2" | bc )
        local out=$( calc_numseg_3mon $_fcstyyyymmdd $_endyyyymmdd ) 
    elif (( $nummon == 10 ));then
        #local out=$( echo "$( calc_numseg_winners $_fcstyyyymmdd ) * 2" | bc )
        local out=$( calc_numseg_winners $_fcstyyyymmdd )
    else
        local out=-9999
    fi 

    echo $out
}    


pbs_dir(){
    #finds full paths of all running or queued experiments.
    #Usage: ${FUNCNAME[0]} [ (optional) qid ]" 
    
    local _arr=()
    local _arr_dexp=()
    local _str_opath=Output_Path
        #capric_hhmmss=$( cat $_fcapric | head -1 | tr -s '[:space:]' | cut -d' ' -f2 )
    local _str=gcm_run
    
    if [[ -n $1 ]];then
        local _qid=$1
        local _arr=($( /PBS/bin/qstat -u $USER | grep $USER | grep $_qid | cut -d' ' -f1 | xargs -i bash -c "/PBS/bin/qstat -f {} | grep -i $_str_opath | cut -d':' -f2" |  xargs -i dirname {} ))
    else
        _arr=($( qstat -u $USER | grep $USER | cut -d' ' -f1 | xargs -i bash -c "/PBS/bin/qstat -f {} | grep -i $_str_opath | cut -d':' -f2" |  xargs -i dirname {} ))
    fi

    (( ${#_arr[@]} > 0 )) && _arr_dexp=($( echo "${_arr[@]}" | tr ' ' '\n' | sort -u | tr '\n' ' '  ))

    echo ${_arr_dexp[@]}
}

exp_getfmiss_fexist(){
    #note:  this is the similar, if not the same,  function in run_gcmarch.sh
    #description:   find missing and existing (with file size other than zero) and place
    #               them in two separate global arrays.
    local _dexp=$1
    #local _fexistpfe=$_dexp/archive/${strscr}_${FUNCNAME[0]}_${fcstdate}${ensm}_pfe
    #local _fexistlfe=$_dexp/archive/${strscr}_${FUNCNAME[0]}_${fcstdate}${ensm}_lfe
    local _fexistpfe=$cdir/${strscr}_${FUNCNAME[0]}_${strexpid}_${fcstdate}${ensm}_hst
    local _fexistlfe=$cdir/${strscr}_${FUNCNAME[0]}_${strexpid}_${fcstdate}${ensm}_arc

    [[ -z $fcstdate || -z $ensm ]] && set_rstfcstdate $_dexp

    #local _darc=$( grep "dfout_lfe=" $_dexp/archive/run_gcmarch.sh | rev | cut -d'=' -f1 | cut -d'/' -f3- | rev )/$fcstdate/$ensm
    local _darc=$DARCH/$fcstdate/$ensm

    exp_getfname_pfe $_dexp $_fexistpfe
    exp_getfname_lfe $_dexp $_darc $_fexistlfe

    [[ ! -f $_fexistpfe ]] && wmessage "(${FUNCNAME[0]}) $_fexistpfe does not exist" && return
    [[ ! -f $_fexistlfe ]] && wmessage "(${FUNCNAME[0]}) $_fexistlfe does not exist" && return

    local arrpfefout=($( cat $_fexistpfe | cut -d' ' -f3 ))
    local pfefout 
    
    local arrfmiss=()

    for pfefout in ${arrpfefout[@]};do
        local fname=($( echo $pfefout | rev | cut -d' ' -f1 | cut -d'/' -f1 | rev  ))
        local fpfesize=$( grep $fname $_fexistpfe | tail -1 | cut -d' ' -f1 )
        local  fpfesec=$( grep $fname $_fexistpfe | tail -1 | cut -d' ' -f2 )
        local fpfepath=$( grep $fname $_fexistpfe | tail -1 | cut -d' ' -f3 )

        #todo:  check if file exists on hstarc
        local _numthisf=$( grep $fname $_fexistlfe 2>/dev/null | grep -v "tar-1" 2>/dev/null | wc -l )

        if (( $_numthisf == 0 ));then
            #todo:  add file to arrfmiss array
            arrfmiss+=( $fpfepath )

        elif (( $_numthisf > 0 ));then
            #todo:  check file size if file exists on hstarc
            local flfesize=$( grep $fname $_fexistlfe | tail -1 | cut -d' ' -f1 | xargs )
            local  flfesec=$( grep $fname $_fexistlfe | tail -1 | cut -d' ' -f2 | xargs )
            local    fcoll=$( basename $fpfepath | cut -d'.' -f2 )

            if [[ -n $flfesize && -n $flfesec ]] && (( $fpfesize == $flfesize )) && (( $fpfesec == $flfesec ));then
                :
            else
                #todo:  add fout in arrfmiss since size is not the same
                arrfmiss+=( $fpfepath )
            fi
        fi
    done

    [[ -f $_fexistlfe ]] && rm -f $_fexistlfe
    [[ -f $_fexistpfe ]] && rm -f $_fexistpfe

    echo ${arrfmiss[@]}
}

exp_calcoutput(){
    #description:   calculate # of output files based on user inputs.
    #               output is returned as an array and the order out #s are:
    #               total, mapl outputs, daily, diurnal, & monthly means

    local dexp=$1;shift
    local blwinner=$1;shift 
    local _fhis=$1
    local fcstdate=$( echo $dexp | rev | cut -d'/' -f2 | rev ) 
    local ensm=$( echo $dexp | rev | cut -d'/' -f1 | rev ) 
    local capric_yyyymmdd=$( date -d "$fcstdate -1days" +%Y%m%d )
    local capr_yyyymmdd=$( set_capdates $fcstdate $blwinner )
    local capric_hhmmss="210000"
    local bllastdayofthemonth=$( fcal_lastdayofthemonth $capric_yyyymmdd )
    local blfirstdayofthemonth=$( fcal_firstdayofthemonth $capric_yyyymmdd )
    local arrcollmonpost_noreft=()

    #todo:  write header
    #wmessage "$( printf '%-15s %+5s %+15s %+5s %+5s %+5s %+5s %+5s\n' yyyymmdd/ensX winner capr_yyyymmdd total mapl daily diur month ) "

    if [[ -f $_fhis ]];then
        local arrcoll=($( exp_getcollections $_fhis ))
        local arrcollmon_mapl=($( exp_getcollfreq m $_fhis ))
        local arrcollmonpost=($( exp_getcollfreq n $_fhis ))
        local arrcollmonpost_diu=($( exp_getcolldiurnal $_fhis ))
        local arrcollmonpost_save=($( printf '%s\n' ${arrcollmonpost[@]} | grep -E '_slv|_sfc' ))
        local numcollmonpost=${#arrcollmonpost[@]}
        
  
        #todo:  get collections that produces extra monthly mean (*.monthly.* file)
        #note:  for s2sv3 hindcast, ocn_tavg_1dy_glo_T1440x1080_slv creates extra monthly mean IF ic date is the 
        #       first day of the month
        for collmonpost in ${arrcollmonpost[@]};do
            local collreftime=$( grep $collmonpost.ref_time $_fhis | tr -s '[:space:]' | rev  | cut -d' ' -f1 | cut -c2- | rev )
        
            if [[ -z $collreftime ]] || (( $capric_hhmmss != $collreftime ));then
                arrcollmonpost_noreft+=( $collmonpost )
            fi
        done
 
        #todo:  calculate a number of total output files.
        local numcoll=${#arrcoll[@]}
        local numcollmon_mapl=${#arrcollmon_mapl[@]}        
        local numcollmon_maplpartial=${#arrcollmon_mapl[@]}        
        local numcollmonpost=${#arrcollmonpost[@]}
        local numcollmonpost_diu=${#arrcollmonpost_diu[@]}        
        #local numcollmonpost_save=${#arrcollmonpost_save[@]}        
        local numcollmonpost_nreft=${#arrcollmonpost_noreft[@]}
        local nummon_run=$( fcal_calcnummon $capric_yyyymmdd $capr_yyyymmdd  )

        if $bllastdayofthemonth;then
            local numfmonmapl_calc=$( echo "$nummon_run * $numcollmon_mapl + $numcollmon_maplpartial" | bc -l ) 
            local     numfday_calc=$( echo "$nummon_run * $numcollmonpost" | bc -l ) 
            local     numfdiu_calc=$( echo "$nummon_run * $numcollmonpost_diu" | bc -l ) 
            local numfmonpost_calc=$( echo "$nummon_run * $numcollmonpost" | bc -l ) 
            local   numftotal_calc=$( echo "$numfmonmapl_calc + $numfday_calc + $numfdiu_calc + $numfmonpost_calc" | bc -l )
        elif $blfirstdayofthemonth;then
            local numfmonmapl_calc=$( echo "( $nummon_run - 1 ) * $numcollmon_mapl + $numcollmon_maplpartial" | bc -l ) 
            local     numfday_calc=$( echo "( $nummon_run - 1 ) * $numcollmonpost + $numcollmonpost_nreft" | bc -l ) 
            local     numfdiu_calc=$( echo "( $nummon_run - 1 ) * $numcollmonpost_diu" | bc -l ) 
            local numfmonpost_calc=$( echo "( $nummon_run - 1 ) * $numcollmonpost + $numcollmonpost_nreft" | bc -l ) 
            local   numftotal_calc=$( echo "$numfmonmapl_calc + $numfday_calc + $numfdiu_calc + $numfmonpost_calc" | bc -l )
        else
            local numfmonmapl_calc=$( echo "( $nummon_run - 1 ) * $numcollmon_mapl + $numcollmon_maplpartial" | bc -l ) 
            local     numfday_calc=$( echo "( $nummon_run - 1 ) * $numcollmonpost" | bc -l ) 
            local     numfdiu_calc=$( echo "( $nummon_run - 1 ) * $numcollmonpost_diu" | bc -l ) 
            local numfmonpost_calc=$( echo "( $nummon_run - 1 ) * $numcollmonpost" | bc -l ) 
            local   numftotal_calc=$( echo "$numfmonmapl_calc + $numfday_calc + $numfdiu_calc + $numfmonpost_calc" | bc -l )
        fi
    fi
#wmessage \@$LINENO ${FUNCNAME[0]}
#wmessage "$numfmonpost_calc = ( $nummon_run - 1 ) * $numcollmonpost" 
    #wmessage "$( printf '%-15s %+6s %+15s %+5s %+5s %+5s %+5s %+5s\n' $fcstdate/$ensm $blwinner $capr_yyyymmdd $numftotal_calc $numfmonmapl_calc $numfday_calc $numfdiu_calc $numfmonpost_calc ) "

    local _arr=( $numftotal_calc $numfmonmapl_calc $numfday_calc $numfdiu_calc $numfmonpost_calc )
    echo "${_arr[@]}"
}

exp_calcoutput_save(){
    #description:   calculate # of output files based on user inputs.
    #               output is returned as an array and the order out #s are:
    #               total, mapl outputs, daily, diurnal, & monthly means

    local dexp=$1;shift
    local blwinner=$1;shift 
    local _fhis=$1
    local fcstdate=$( echo $dexp | rev | cut -d'/' -f2 | rev ) 
    local ensm=$( echo $dexp | rev | cut -d'/' -f1 | rev ) 
    local strmapl="01_0000z.nc4"
    local capric_yyyymmdd=$( date -d "$fcstdate -1days" +%Y%m%d )
    local capric_yyyymm=$( echo $capric_yyyymmdd | cut -c1-6 )
    local capr_yyyymmdd=$( set_capdates $fcstdate $blwinner )
    local capr_yyyymm=$( echo $capr_yyyymmdd | cut -c1-6 )
    local capric_hhmmss="210000"
    local bllastdayofthemonth=$( fcal_lastdayofthemonth $capric_yyyymmdd )
    local blfirstdayofthemonth=$( fcal_firstdayofthemonth $capric_yyyymmdd )
    #local _arrmonthfull=($( numfullmonths $capric_yyyymm $capr_yyyymm $bllastdayofthemonth ))
    local _arrmonthfull=( $capric_yyyymm $( numfullmonths $capric_yyyymm $capr_yyyymm $bllastdayofthemonth ))
    local arrcollmonpost_noreft=()
    local arrfsave=()

    #todo:  write header
    #wmessage "$( printf '%-15s %+5s %+15s %+5s %+5s %+5s %+5s %+5s\n' yyyymmdd/ensX winner capr_yyyymmdd total mapl daily diur month ) "

    if [[ -f $_fhis ]];then

        arrcoll=($( exp_getcollections $_fhis ))
        arrcollmon_mapl=($( exp_getcollfreq m $_fhis ))
        arrcollmonpost=($( exp_getcollfreq n $_fhis ))
        arrcollmonpost_diu=($( exp_getcolldiurnal $_fhis ))

        #todo:  get collections that produces extra monthly mean (*.monthly.* file)
        #note:  for s2sv3 hindcast, ocn_tavg_1dy_glo_T1440x1080_slv creates extra monthly mean IF ic date is the 
        #       first day of the month
        for collmonpost in ${arrcollmonpost[@]};do
            local collreftime=$( grep $collmonpost.ref_time $_fhis | tr -s '[:space:]' | rev  | cut -d' ' -f1 | cut -c2- | rev )
        
            if [[ -z $collreftime ]] || (( $capric_hhmmss != $collreftime ));then
                arrcollmonpost_noreft+=( $collmonpost )
            fi
        done

        #note:  array with collections to save
        local arrcollmonpost_savemon=( ${arrcollmonpost[@]} )
        local arrcollmonpost_savediu=($( printf '%s\n' ${arrcollmonpost_diu[@]} | sort -V -k3 -t'_' | grep -E '_sfc|_slv' ))
        local arrcollmonpost_savedya=( ice_tavg_1dy_glo_T1440x1080_slv )
        local arrcollmonpost_savedy3=( sfc_tavg_3hr_glo_L720x361_sfc sfc_tavg_1hr_glo_L720x361_sfc )
   
        for coll in ${arrcoll[@]};do
            [[ "${arrcollmon_mapl[@]}" =~ "$coll" ]] && local  _blsavemapl=true || local _blsavemapl=false
            [[ "${arrcollmonpost_savemon[@]}" =~ "$coll" ]] && local _blsavemon=true || local _blsavemon=false
            [[ "${arrcollmonpost_savediu[@]}" =~ "$coll" ]] && local _blsavediu=true || local _blsavediu=false
            [[ "${arrcollmonpost_savedya[@]}" =~ "$coll" ]] && local _blsavedya=true || local _blsavedya=false
            [[ "${arrcollmonpost_savedy3[@]}" =~ "$coll" ]] && local _blsavedy3=true || local _blsavedy3=false
            [[ "${arrcollmonpost_noreft[@]}"  =~ "$coll" ]] && local _blnoreft=true  || local _blnoreft=false

#wmessage $dexp 
#wmessage $coll 
#wmessage $blfirstdayofthemonth $bllastdayofthemonth 
#wmessage $_blsavemapl $_blsavemon 
#wmessage $_blsavediu $_blsavedya 
#wmessage $_blsavedy3 $_blnoreft 
#wmessage ${_arrmonthfull[@]}
#exit
#wmessage lineno  = $LINENO 
#wmessage $coll $blfirstdayofthemonth $bllastdayofthemonth $_blsavemapl $_blsavemon $_blsavediu $_blsavedya $_blsavedy3 $_blnoreft ${_arrmonthfull[@]}
            arrfsave+=($( exp_createfname_save $dexp $coll $blfirstdayofthemonth $bllastdayofthemonth $_blsavemapl $_blsavemon $_blsavediu $_blsavedya $_blsavedy3 $_blnoreft ${_arrmonthfull[@]} ))
        done

#wmessage lineno  = $LINENO 
#wmessage "$( printf '%s\n' ${arrfsave[@]} | xargs -i basename {} | grep daily )"
#local testnumf_day=$( printf '%s\n' ${arrfsave[@]} | xargs -i basename {} | grep daily | wc -l )
#wmessage $testnumf_day

        #todo:  These are calculated values (based on created files names) based on 3-month or 10-month run
        local numfsave_mpl=$( printf '%s\n' ${arrfsave[@]} | grep "$strmapl" | wc -l )
        local numfsave_mon=$( printf '%s\n' ${arrfsave[@]} | grep "monthly"  | wc -l )
        local numfsave_diu=$( printf '%s\n' ${arrfsave[@]} | grep "diurnal"  | wc -l )
        local numfsave_day=$( printf '%s\n' ${arrfsave[@]} | grep "daily"    | wc -l )
        local numfsave_rst=0
        local numfsave_mom=0
        local numfsave_tot=$( echo "$numfsave_mpl + $numfsave_mon + $numfsave_diu + $numfsave_day + $numfsave_mom" | bc )

    fi

    local _arr=( $numfsave_tot $numfsave_mpl $numfsave_day $numfsave_diu $numfsave_mon )
    echo "${_arr[@]}"

}


count_files(){
    local _dexp=$1;shift
    local _fexistlfe=$1
    local strscrach=scratch
    local strrst=restarts
    local strmom=MOM_Output
    local strmom_search=ocean_daily
    local strarc=gcmarch
    local _bldel_fexistlfe=false
    local _fdelout=$dexp/archive/${strarc}_delout

    set_rstfcstdate $_dexp

    if [[ -z $_fexistlfe ]];then
        #local _fexistlfe=$_dexp/archive/${strscr}_${FUNCNAME[0]}_${fcstdate}${ensm}_lfe
        local _fexistlfe=$cdir/${strscr}_${FUNCNAME[0]}_${fcstdate}${ensm}_lfe
        #local _darc=$( grep "dfout_lfe=" $_dexp/archive/run_gcmarch.sh | rev | cut -d'=' -f1 | cut -d'/' -f3- | rev )/$fcstdate/$ensm
        local _darc=$DARCH/$fcstdate/$ensm
        exp_getfname_lfe $_dexp $_darc $_fexistlfe
        _bldel_fexistlfe=true
    fi

    cd $_dexp
    #numfdisk_all=$( find * -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*.nc*" | grep -Ev "$collsst|$strscrach" | wc -l )
        numfpfe_tar_cnt=$( find * -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*.??????.nc4.tar" | grep -Ev "$collsst|$strscrach|daily" | wc -l )
      numfpfe_total_cnt1=$( find * -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*.nc4*" | grep -Ev "$collsst|$strscrach" | wc -l )
      numfpfe_total_cnt=$(( numfpfe_total_cnt1 - numfpfe_tar_cnt ))
    numfpfe_monmapl_cnt=$( find * -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*z.nc4*"           | grep -Ev "$collsst|$strscrach" | wc -l )
        numfpfe_day_cnt=$( find * -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*.daily.*.nc4.tar" | grep -Ev "$collsst|$strscrach" | wc -l )
        numfpfe_diu_cnt=$( find * -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*.diurnal.*.nc4"   | grep -Ev "$collsst|$strscrach" | wc -l )
    numfpfe_monpost_cnt=$( find * -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*.monthly.*.nc4"   | grep -Ev "$collsst|$strscrach" | wc -l )

        numfpfe_rst_cnt=$( find * -maxdepth 1 -mindepth 1 -type f -name "$strrst*" | grep -v $strscrach | wc -l  )
        numfpfe_mom_cnt=$( find * -maxdepth 1 -mindepth 1 -type f -name "$strmom_search*" | grep -v $strscrach | grep $strmom | wc -l )

    if [[ -f $_fdelout ]];then
        numfpfe_del_cnt=$( cat $_fdelout | grep -vE "${strmom_search}|${strrst}" 2>/dev/null | sort -V | uniq | wc -l ) 
        numfpfe_mis_cnt=$( echo "$numftotal_calc - $numfpfe_total_cnt - $numfpfe_del_cnt" | bc )
    else
        numfpfe_mis_cnt=$( echo "$numftotal_calc - $numfpfe_total_cnt" | bc )
    fi

    cd - >/dev/null

    #todo: get number of files
    numfpfe=$(( numfpfe_total_cnt + numfpfe_rst_cnt + numfpfe_mom_cnt ))
    
    #note:  these are counting a number of files (no checking size)
    [[ -f $fexistpfe_sst ]] && numfpfe_sst=$( cat $fexistpfe_sst | wc -l ) 
    if [[ -f $_fexistlfe ]];then
                    numflfe=$( cat $_fexistlfe | wc -l )
            numflfe_tar_cnt=$( cat $_fexistlfe | grep .[0-9]*.nc4.tar | grep -Ev "$collsst|$strscrach|daily" | wc -l )
         numflfe_total_cnt1=$( cat $_fexistlfe | grep -Ev "$strrst|$collsst" | wc -l )
          numflfe_total_cnt=$(( numflfe_total_cnt1 - numflfe_tar_cnt ))

        numflfe_monmapl_cnt=$( cat $_fexistlfe | grep z.nc4     | grep -Ev "$collsst|$strscrach" | wc -l )
            #numflfe_tar_cnt=$( cat $_fexistlfe | grep .nc4.tar  | grep -Ev "$collsst|$strscrach" | wc -l )
            numflfe_day_cnt=$( cat $_fexistlfe | grep .daily.[0-9] | grep .nc4.tar | grep -v tar. | grep -Ev "$collsst|$strscrach" | wc -l )
            numflfe_diu_cnt=$( cat $_fexistlfe | grep .diurnal. | grep -Ev "$collsst|$strscrach" | wc -l )
        numflfe_monpost_cnt=$( cat $_fexistlfe | grep .monthly. | grep -Ev "$collsst|$strscrach" | wc -l )

            numflfe_rst_cnt=$( cat $_fexistlfe | grep $strrst | wc -l )
            numflfe_mom_cnt=$( cat $_fexistlfe | grep $strmom_search | wc -l )
            numflfe_mis_cnt=$( echo "$numftotal_calc - $numflfe_total_cnt" | bc )
    fi
    
    #note:  size has been check with these files
    numfmissing=${#arrfmiss[@]}

    $_bldel_fexistlfe && [[ -f $_fexistlfe ]] && rm -f $_fexistlfe 
    return
}



write_table(){
    local _dexp=$1;shift
    local _numfmiss=$1

    [[ -z $hstshort ]] && hstshort=$( get_host ) 

    local _thishost_up=$( echo $hstshort | tr '[:lower:]' '[:upper:]' )
    if [[ $hstshort == pfe ]];then 
        local _darchost="LFE"
    elif [[ $hstshort == dis ]];then 
        local _darchost="ARC"
    fi

    set_rstfcstdate $_dexp

    local _ftmp=$cdir/tmp_${strscr}_${FUNCNAME[0]}_${strexpid}_${fcstdate}${ensm}_1
    [[ -f $_ftmp ]] && rm -f $_ftmp

    if [[ -n $numflfe && -n $numflfe_total_cnt && -n $numflfe_mom_cnt && -n $numflfe_rst_cnt ]];then
        echo  "                     "\;"Calculated"\;"Calc_Save"\;"$_thishost_up"\;"$_darchost" >> $_ftmp 
        echo  "  total # of outputs:"\;$numftotal_calc\;$numfsave_tot\;$numfpfe_total_cnt\;$numflfe_total_cnt >> $_ftmp 
        echo  "        mapl outputs:"\;$numfmonmapl_calc\;$numfsave_mpl\;$numfpfe_monmapl_cnt\;$numflfe_monmapl_cnt >> $_ftmp 
        echo  "             monthly:"\;$numfmonpost_calc\;$numfsave_mon\;$numfpfe_monpost_cnt\;$numflfe_monpost_cnt >> $_ftmp 
        echo  "             diurnal:"\;$numfdiu_calc\;$numfsave_diu\;$numfpfe_diu_cnt\;$numflfe_diu_cnt >> $_ftmp 
        echo  "               daily:"\;$numfday_calc\;$numfsave_day\;$numfpfe_day_cnt\;$numflfe_day_cnt >> $_ftmp 
        echo  "         MOM outputs:"\;$numfmom_calc\;$numfsave_mom\;$numfpfe_mom_cnt\;$numflfe_mom_cnt >> $_ftmp 
        echo  "            restarts:"\;-\;-\;$numfpfe_rst_cnt\;$numflfe_rst_cnt >> $_ftmp 
    else
        echo  "                     "\;"Calculated"\;"Calc_Save"\;"$_thishost_up" >> $_ftmp 
        echo  "  total # of outputs:"\;$numftotal_calc\;$numfsave_tot\;$numfpfe_total_cnt >> $_ftmp 
        echo  "        mapl outputs:"\;$numfmonmapl_calc\;$numfsave_mpl\;$numfpfe_monmapl_cnt >> $_ftmp 
        echo  "             monthly:"\;$numfmonpost_calc\;$numfsave_mon\;$numfpfe_monpost_cnt >> $_ftmp 
        echo  "             diurnal:"\;$numfdiu_calc\;$numfsave_diu\;$numfpfe_diu_cnt >> $_ftmp 
        echo  "               daily:"\;$numfday_calc\;$numfsave_day\;$numfpfe_day_cnt >> $_ftmp 
        echo  "         MOM outputs:"\;$numfmom_calc\;$numfsave_mom\;$numfpfe_mom_cnt >> $_ftmp 
        echo  "            restarts:"\;-\;-\;$numfpfe_rst_cnt >> $_ftmp 
    fi

    #todo:  write it
    wmessage
    if (( $writetofile == 1 ));then
        rev $_ftmp | column -t -s';' | rev  >> $fmessage
    else
        rev $_ftmp | column -t -s';' | rev  
    fi
    wmessage
    #wmessage "Incompleted gcm_post: ${#arrpst[@]} "
    [[ -n $numfsave_tot && -n $numfpfe_total_cnt ]] && (( $numfsave_tot != $numfpfe_total_cnt )) && wmessage "      Missing on $_thishost_up: $numfpfe_mis_cnt"
    wmessage "      Missing on $_darchost: $numflfe_mis_cnt"
    #wmessage "       Corrupted tar: ${#arrftar_crpt[@]} of ${#arrftar[@]}"
    wmessage 
    
    [[ -f $_ftmp ]] && rm -f $_ftmp

    return
}

write_table_collection(){
    local _dexp=$1;shift
    local _arrfpath=( "$@" ) 
    local arrfthiscoll=()
    local numleadspace1="+8"
    local numleadspace2="+10"
    local numleadspace3="-35"
    
    [[ -z $fcstdate || -z $ensm ]] && set_rstfcstdate $_dexp

    local _ftmp=$cdir/tmp_${strscr}_${FUNCNAME[0]}_${strexpid}_${fcstdate}${ensm}_1

    #todo:  get arrcoll array  if not available.
    if [[ -z ${arrcoll[@]} ]];then    
        local fhis2=HISTORY_2.rc
        local _fhis2=$_dexp/$fhis2
        local arrcoll=($( exp_getcollections $_fhis2 ))
    fi

    local arrthiscoll=($( printf '%s\n' ${arrcoll[@]} | grep -v _1mo_glo_ | sort -V -k3 -t'_' ))

    [[ -f $_ftmp ]] && rm -f $_ftmp
        
    local _arrfexist=($( find $_dexp/* -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*.nc4*" | grep -v sst | sort -V ))

    echo "$( printf "%${numleadspace3}s" "Collection" )"\;"$( printf "%${numleadspace2}s" "Total" )"\;"$( printf "%${numleadspace1}s" "monthly" )"\;"$( printf "%${numleadspace1}s" "dirunal" )"\;"$( printf "%${numleadspace1}s" "daily" )" >> $_ftmp
    for thiscoll in ${arrthiscoll[@]};do 
        #arrfthiscoll=($( printf '%s\n' ${arrfsave[@]} | xargs -i basename {} | sort -V | grep $thiscoll ))
        #arrfthiscoll=($( printf '%s\n' ${_arrfexist[@]} | xargs -i basename {} | sort -V | grep $thiscoll ))
        arrfthiscoll=($( printf '%s\n' ${_arrfpath[@]} | xargs -i basename {} | sort -V | grep $thiscoll ))
    
        local nummon=$( printf '%s\n' ${arrfthiscoll[@]} | grep monthly | wc -l | xargs -i bash -c '(( {} == 0 )) && echo "-" || echo {} ')
        local numday=$( printf '%s\n' ${arrfthiscoll[@]} | grep daily   | wc -l | xargs -i bash -c '(( {} == 0 )) && echo "-" || echo {} ')
        local numdiu=$( printf '%s\n' ${arrfthiscoll[@]} | grep diurnal | wc -l | xargs -i bash -c '(( {} == 0 )) && echo "-" || echo {} ')
    
        echo "$( printf "%${numleadspace3}s" "$thiscoll" )"\;"$( printf "%${numleadspace2}s" ${#arrfthiscoll[@]} )"\;"$( printf "%${numleadspace1}s" "$nummon" )"\;"$( printf "%${numleadspace1}s" $numdiu )"\;"$( printf "%${numleadspace1}s" $numday )" >> $_ftmp
    done 

    wmessage

    if (( $writetofile == 1 ));then
        echo "# of Saved Files for Each Collection (execlude mapl outputs):" >> $fmessage
        rev $_ftmp | column -t -s';' | rev  >> $fmessage
    else
        echo "# of Saved Files for Each Collection (execlude mapl outputs):"
        rev $_ftmp | column -t -s';' | rev  
    fi

    wmessage

    [[ -f $_ftmp ]] && rm -f $_ftmp

    return
}



write_notes(){
    wmessage
    wmessage Calculated:
    wmessage " total # of outputs: $numftotal_calc"
    wmessage "        mapl output: $numfmonmapl_calc"
    wmessage "              daily: $numfday_calc"
    wmessage "            diurnal: $numfdiu_calc"
    wmessage "            monthly: $numfmonpost_calc"
    wmessage "        MOM outputs: $numfmom_calc"
    #$blleaveout && wmessage "     outputs on pfe: $numfcollsave_calc"
    wmessage

    if [[ -n $numflfe && -n $numflfe_total_cnt && -n $numflfe_mom_cnt && -n $numflfe_rst_cnt ]];then
        wmessage "Counted (size of files are different):"
        wmessage "# of missing outputs: $numfmissing"
        wmessage
        wmessage $hstarc files:
        wmessage "$strrst + MOM outputs + outputs: $numflfe"
        wmessage "           outputs: $numflfe_total_cnt"
        wmessage "       mapl output: $numflfe_monmapl_cnt"
        wmessage "        tar output: $numflfe_tar_cnt"
        wmessage "           diurnal: $numflfe_diu_cnt"
        wmessage "           monthly: $numflfe_monpost_cnt"
        wmessage "       MOM outputs: $numflfe_mom_cnt"
        wmessage "          $strrst: $numflfe_rst_cnt"
        wmessage
    fi

    wmessage pfe files:
    wmessage "$strrst + MOM outputs + outputs: $numfpfe"
    wmessage "           outputs: $numfpfe_total_cnt"
    wmessage "       mapl output: $numfpfe_monmapl_cnt"
    wmessage "        tar output: $numfpfe_tar_cnt"
    wmessage "           diurnal: $numfpfe_diu_cnt"
    wmessage "           monthly: $numfpfe_monpost_cnt"
    wmessage "       MOM outputs: $numfpfe_mom_cnt"
    wmessage "          $strrst: $numfpfe_rst_cnt"
    [[ -n $numfpfe_sst ]] && wmessage "       $collsst: $numfpfe_sst"
    wmessage
    wmessage
    return
}

check_post(){
    #description:   find missing diurnal file and gcm_post.*.j* script

    local _str=$1;shift
    local _arrcoll=( "$@" )
    local fdelout=$dexp/archive/gcmarch_delout
    local _arrfname=()
    local arrfname1=()
    local arrf=()
   
    [[ -z $arrmfull ]] && die "(${FUNCNAME[0]}) arrmfull is an empty variable"

    #file names:    
    #19840101.aer_inst_3hr_glo_L720x361_slv.diurnal.198402.nc4
    #$fcstdate.$coll.diurnal.YYYYMM.nc4
    #gcm_post.ocn_inst_6hr_glo_L1440x721_z50.j198402
    #gcm_post.$coll.jYYYYMM

    #todo:  create filenames, which are suppoed to exists, based on collection and YYYYMM
    for coll in ${_arrcoll[@]};do
        local fname1=$dexp/$coll/$fcstdate.$coll.${_str}.
        if [[ "$_str" == daily ]];then 
            _arrfname=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4.tar\n" ${arrmfull[@]} ))
        else
            _arrfname=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4\n" ${arrmfull[@]} ))
        fi

        #todo:  check if file exists
        local arrfname_miss=($( printf '%s\n' ${_arrfname[@]} | xargs -i bash -c "[[ ! -f {} ]] && echo {}" | xargs -i basename {} | sort -V  ))
        
        #todo:  find gcm_post.*.j file for missing durinal file
        for fmiss in ${arrfname_miss[@]} ;do 
            #todo:  check if fmiss is already deleted
            grep $fmiss $fdelout > /dev/null 2>&1
            local _status_grep=$?

            (( $_status_grep == 0 )) && continue

            if [[ "$_str" == daily ]];then 
                local fmiss_yyyymm=$( echo $fmiss | rev | cut -d'.' -f3 | rev ) 
            else
                local fmiss_yyyymm=$( echo $fmiss | rev | cut -d'.' -f2 | rev ) 
            fi
            arrf+=( gcm_post.$coll.j$fmiss_yyyymm )
        done 

    done 

    #(( ${#arrf[@]} > 0 )) && wmessage 
    echo "${arrf[@]}"
    
    return
}

exp_sherlockpst(){
    #note:  this function is similar to sherlock_post in sherlock.sh
    #description:   check outputs from gcm_post.*.j* and return name of 
    #               unfinished gcm_post.*.j* 
    local arrfpst=()
    local arr=()
    local strdiu=diurnal
    local strmon=monthly
    local strday=daily

    if (( $numfdiu_calc != $numfpfe_diu_cnt ));then
        arrfpst+=($( check_post $strdiu "${arrcollmonpost_diu[@]}" ))
    fi
    
    if (( $numfmonpost_calc != $numfpfe_monpost_cnt ));then
        arrfpst+=($( check_post $strmon "${arrcollmonpost[@]}" ))
    fi

    if (( $numfday_calc != $numfpfe_day_cnt ));then
        arrfpst+=($( check_post $strday "${arrcollmonpost[@]}" ))
    fi
    arr=($( printf '%s\n' ${arrfpst[@]} | sort -V | uniq ))

    echo "${arr[@]}"
}

exp_missdhold(){
    #descrtipsion: find missing holding/coll/yyyymm dir. 
    #Note:
    #   1.This function is slightly different from one in run_gcmarch.sh. 
    #     This returns a pull path of missing holding/coll/yyyymm dir unlike
    #     returning gcm_post*.j*
    #   2.An array of gcm_post.*.j* name is required. You can get the array
    #     by running these functions:
    #       Set dexp then,
    #       setvars $dexp
    #       count_files $dexp
    #       arrpst=($( exp_sherlockpst ))
    #     Then, run this function
    #       arr=$( exp_missdhold ${arrpst[@]} ) 

    local _arrfpst=( "$@" )
    local _arr=() 
    local _fpst

    #note:  this check has to be done every execution of this script
    #       Due to the timing of moving output from scratch dir, this 
    #       script may consider some YYYYMM dir is missing
    [[ -n $fnote && -f $fnote ]] && rm -f $fnote

    #gcm_post.sfc_tavg_3hr_glo_L720x361_sfc.j198201
    for _fpst in ${_arrfpst[@]};do 
        local _yyyymm=$( echo $_fpst | rev | cut -d'.' -f1 | rev | cut -c2- )
        local   _coll=$( echo $_fpst | rev | cut -d'.' -f2 | rev )
        local _dholdyyyymm=$dexp/holding/$_coll/$_yyyymm 

        if [[ ! -d $_dholdyyyymm ]];then
            #_arr+=( $_fpst ) 
            _arr+=( $_dholdyyyymm ) 

            #todo:  write missing yyyymm dir in holding
            [[ -n $fnote ]] && echo "missing: $_dholdyyyymm" >> $fnote 2>&1
        fi
    done 

    echo "${_arr[@]}" 
}


numfullmonths(){
    #description:   calculate a number of FULL months that s2sv3 ran
    local _capric_yyyymm=$1;shift
    local _capr_yyyymm=$1;shift
    local _bllastdayofthemonth=$1
    local arrmfull=() 

    [[ -z $_capric_yyyymm ]] && die "(${FUNCNAME[0]}) missing arg for _capric_yyyymm "
    [[ -z $_capr_yyyymm ]] && die "(${FUNCNAME[0]}) missing arg for _capr_yyyymm "
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


_exp_updarchscr(){
    #description:   copy and edit updated gcmarch script to entered exp dir.
    #local farchscr=run_gcmarch.sh
    #local farchscr_beta=run_gcmarch_beta.sh
    local farch=$1; shift
    local _arrdexp=( "$@" )
    local farchscr=$( basename $farch )
    local groupid=g0609
    local _arrstrexpid=( fcst myexp rim )
    local dexp

    local archive_p="PBS -l select=1:ncpus=20:mpiprocs=20:model=ivy:aoe=sles12"
    local archive_q="PBS -q normal"
    local BATCH_GROUP="PBS -W group_list="
    local batch_joinouterr="PBS -j oe -k oed"

    #if [[ -n $blusesetuptar ]] && $blusesetuptar;then
    #    local dsetup=$DFMOD/$ctag/setup_pfe_${strexpid}_v3
    #    local farch=$dsetup/archive/$farchscr

    #elif [[ -z $blusesetuptar ]] || ! $blusesetuptar;then
    #    local farch=$dbuild/Applications/GEOSgcm_App/$farchscr
    #fi

    [[ -f $farch ]] && : || die "$farch does not exist"

    for dexp in ${_arrdexp[@]};do
        local fsed=$dexp/${FUNCNAME[0]}_sedfile
        local darch=$dexp/archive
        local farch_exp=$darch/$farchscr
        local farch_tmpexp=$darch/tmp_$farchscr
        local farch_exp_old=$darch/_$farchscr
        local farchlock=$( echo $farchscr | cut -d'_' -f2 | cut -d'.' -f1 ).lock

#wmessage $fsed 
#wmessage $darch 
#wmessage $farch_exp
#wmessage $farch_tmpexp
#wmessage $farch_exp_old
#exit
        [[ -f $farchlock ]] && die "$farchlock exist" 
        [[ -f $farch_exp_old ]] && rm -f $farch_exp_old 

        #todo:  if arch script in archive dir doesn't exist, ignore. If it does
        #       it is renamed with "_" in front of it.
        if [[ ! -d $darch ]];then
            continue
        else
            [[ -f $farch_exp ]] && mv $farch_exp $farch_exp_old
            cp -p $farch $farch_tmpexp
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
s?@\<BATCH_JOINOUTERR\>?${batch_joinouterr}?g
s?@\<BLLEAVEOUT\>?$blleaveout?g
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
s?@\<BATCH_JOINOUTERR\>?${batch_joinouterr}?g
s?@\<BLLEAVEOUT\>?$blleaveout?g
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

exp_updarchscr(){

    #description:   copy and edit updated gcmarch script to entered exp dir.
    local _archqid=$1;shift
    local _arrdexp=( "$@" )
    local groupid=g0609
    local farchscr=run_gcmarch.sh
    local _arrstrexpid=( fcst myexp rim )
    local dexp

    [[ -z $blres_arch ]] && local blres_arch=false
    [[ -z $hstshort   ]] && local hstshort=$( get_host )

    if [[ $hstshort == pfe ]];then 
        /PBS/bin/pbs_rstat | grep $_archqid >>/dev/null 2>&1
        local status_grep=$?

        (( $status_grep == 0 )) && local blresarch_exist=true || local blresarch_exist=false

        local blrunres_arch=true
        local archive_p="PBS -l select=1:ncpus=20:mpiprocs=20:model=ivy"
        local archive_q="PBS -q normal"
        
        if $blresarch_exist ;then
            #local archive_pres="PBS -l select=1:ncpus=20:mpiprocs=20:model=ivy:aoe=sles13"
            local archive_pres="$archive_p"
            local archive_qres="PBS -q $_archqid"
        fi

        local batch_joinouterr="PBS -j oe -k oed"
        local batch_outputname="PBS -o "
        local batch_outputname_geoss2s3="DELETE"       
        local BATCH_GROUP="PBS -W group_list="
        local batch_time="PBS -l walltime="
        local batch_jobname="PBS -N "
        local geoss2s3_opt2="PBS -W umask=0022"
        local archive_t="08:00:00"


    elif [[ $hstshort == dis ]];then 
        wmessage "(${FUNCNAME[0]}) WARNING: This function does NOT update gcmarch.sh"

        local blrunres_arch=false
        local blresarch_exist=false

        local archive_q="SBATCH --partition=datamove"
        local archive_p="SBATCH --ntasks=1"
        local archive_pres="DELETE"
        local archive_qres="DELETE"

        local batch_joinouterr="DELETE"
        local batch_outputname="SBATCH --output="         
        local batch_outputname_geoss2s3="SBATCH --output="
        local batch_time="SBATCH --time="
        local BATCH_GROUP="SBATCH --account="
        local batch_time="SBATCH --time="
        local geoss2s3_opt2="DELETE"                      
        local batch_jobname="SBATCH --job-name="
        local archive_t="02:00:00"
    fi

    local farch=$DBUILD/Applications/GEOSgcm_App/$farchscr
    [[ ! -f $farch ]] && die "$farch does not exist"


    for dexp in ${_arrdexp[@]};do
       
        #todo:  set variables. 
        set_rstfcstdate $dexp

        local darch=$dexp/archive
        local farch_exp=$darch/$farchscr
        local farch_tmpexp=$darch/tmp_$farchscr
        local farch_exp_old=$darch/_$farchscr
        local farchlock=$( echo $farchscr | cut -d'_' -f2 | cut -d'.' -f1 ).lock
        local fsed=$cdir/sedfile_${strscr}_${FUNCNAME[0]}_$fcstdate$ensm

        [[ -f $farchlock ]] && die "$farchlock exist" 
        [[ -f $farch_exp_old ]] && rm -f $farch_exp_old 

        [[ -f $farch_tmpexp ]] && rm -f $farch_tmpexp
        [[ -f $fsed ]] && rm -f $fsed

        #todo:  if arch script in archive dir doesn't exist, ignore. If it does
        #       it is renamed with "_" in front of it.
        if [[ ! -d $darch ]];then
            continue
        else
            [[ -f $farch_exp ]] && mv $farch_exp $farch_exp_old
            cp -p $farch $farch_tmpexp
        fi

        #if [[ "${_arrstrexpid[@]}" =~ "$strexpid" ]];then  
        #if $blres_arch && $blrunres_arch; then

            cat > $fsed << EOF
s?@\<DFCST\>?${DFCST}?g
s?@\<FCSTDATE\>?${fcstdate}?g
s?@\<ENSEMBLE_MEMBER\>?${ensm}?g
s?@\<DARCH\>?${DARCH}?g
s?@BATCH_GROUP?${BATCH_GROUP}${groupid}?g
s?@GROUPID?${groupid}?g
s?@\<BLLEAVEOUT\>?$blleaveout?g
s?@\<BLRSYNC\>?$blrsync?g
s?@\<BLBBSCP\>?$blbbscp?g
s?@\<ARCHIVE_P\>?${archive_p}?g
s?@\<ARCHIVE_Q\>?${archive_q}?g
s?@\<ARCHIVE_PRES\>?${archive_pres}?g
s?@\<ARCHIVE_QRES\>?${archive_qres}?g
s?@\<BATCH_JOINOUTERR\>?${batch_joinouterr}?g
s?@BATCH_OUTPUTNAME?${batch_outputname}?g
s?@BATCH_OUTPUTNAME_GEOSS2S3?${batch_outputname_geoss2s3}?g
s?@BATCH_JOBNAME?${batch_jobname}?g
s?@ARCHIVE_T?${archive_t}?g
s?@\<GEOSS2S3_OPT2\>?${geoss2s3_opt2}?g
s?@BATCH_TIME?${batch_time}?g
EOF

#        else
#
#            cat > $fsed << EOF
#s?@\<DFCST\>?${DFCST}?g
#s?@\<FCSTDATE\>?${fcstdate}?g
#s?@\<ENSEMBLE_MEMBER\>?${ensm}?g
#s?@\<DARCH\>?${DARCH}?g
#s?@BATCH_GROUP?${BATCH_GROUP}${groupid}?g
#s?@GROUPID?${groupid}?g
#s?@\<BLLEAVEOUT\>?$blleaveout?g
#s?@\<BLRSYNC\>?$blrsync?g
#s?@\<ARCHIVE_P\>?${archive_p}?g
#s?@\<ARCHIVE_Q\>?${archive_q}?g
#s?@\<BATCH_JOINOUTERR\>?${batch_joinouterr}?g
#s?@BATCH_OUTPUTNAME?${batch_outputname}?g
#s?@BATCH_OUTPUTNAME_GEOSS2S3?${batch_outputname_geoss2s3}?g
#s?@BATCH_JOBNAME?${batch_jobname}?g
#s?@ARCHIVE_T?${archive_t}?g
#s?@\<GEOSS2S3_OPT2\>?${geoss2s3_opt2}?g
#s?@BATCH_TIME?${batch_time}?g
#EOF
#
#        fi

        #todo:  edit arch scr 
        sed -i -f $fsed $farch_tmpexp
        cat $farch_tmpexp | awk '{ if ( $1  !~ "#DELETE") { print } }' > $farch_exp
        mv $farch_exp $farch_tmpexp

        #todo:  edit PBS statement
        if $blrunres_arch && [[ -f $farch_tmpexp ]] ; then
            
            pbsselect_res1=$( grep "$archive_pres" $farch_tmpexp | head -1 )
            pbsselect_res1=$( grep -w "$archive_pres" $farch_tmpexp )
            char2_select=$( echo $pbsselect_res1 | cut -c1-2 )
            
            pbsqid_res1=$( grep "$archive_qres" $farch_tmpexp )
            char2_qid=$( echo $pbsqid_res1 | cut -c1-2 )

            #pbsselect_nrm1=$( grep "$archive_p" $farch_tmpexp | tail -1 )
            pbsselect_nrm1=$( grep -w "$archive_p" $farch_tmpexp | grep -v "aoe=" )
            char2_select_nrm=$( echo $pbsselect_nrm1 | cut -c1-2 )
            
            pbsqid_nrm1=$( grep "$archive_q" $farch_tmpexp )
            char2_qid_nrm=$( echo $pbsqid_nrm1 | cut -c1-2 )

            [[ "$char2_select" == "##" ]] && pbsselect_res=$( echo $pbsselect_res1 | cut -c2- )
            [[ "$char2_qid" == "##" ]] && pbsqid_res=$( echo $pbsqid_res1 | cut -c2- )

            [[ "$char2_select_nrm" != "##" ]] && pbsselect_nrm=$( echo \#$pbsselect_nrm1 )
            [[ "$char2_qid_nrm" != "##" ]] && pbsqid_nrm=$( echo \#$pbsqid_nrm1 )

            if [[ "$archive_pres" == "$archive_p" ]];then
                sed -e "s/${pbsqid_res1}/${pbsqid_res}/g"       \
                    -e "s/${pbsqid_nrm1}/${pbsqid_nrm}/g"       \
                    $farch_tmpexp >> $farch_exp
                local status_sed=$?

            else
                #NOTE: 02/23/2022 this does not work at this time
                #sed -e "s/${pbsselect_res1}/${pbsselect_res}/g" \
                #    -e "s/${pbsqid_res1}/${pbsqid_res}/g"       \
                #    -e "s/${pbsselect_nrm1}/${pbsselect_nrm}/g" \
                #    -e "s/${pbsqid_nrm1}/${pbsqid_nrm}/g"       \
                #    $farch_tmpexp >> $farch_exp
                #local status_sed=$?

                cat $farch_tmpexp >> $farch_exp
                local status_sed=$?
            fi

            if (( $status_sed == 0 ));then        
                chmod 755 $farch_exp
            else
                wmessage "updating archive script failed - $dexp" 
                mv $farch_exp_old $farch_exp
            fi

        else
            if [[ -f $farch_exp_old && -f $farch_tmpexp ]];then 
                diff -s $farch_exp_old $farch_tmpexp >/dev/null
                local status_diff=$?
                if (( $status_diff > 0 ));then
                    mv $farch_tmpexp $farch_exp
                    chmod 755 $farch_exp
                else
                    mv $farch_exp_old $farch_exp
                    chmod 755 $farch_exp
                fi

            elif [[ -f $farch_tmpexp ]];then
                mv $farch_tmpexp $farch_exp && chmod 755 $farch_exp

            elif [[ ! -f $farch_tmpexp ]];then 
                :
            fi
        fi

        [[ -f $farch_tmpexp ]] && rm -f $farch_tmpexp
        [[ -f $fsed ]] && rm -f $fsed
    
    done
    return
}

_exp_updarchscr(){
    #description:   copy and edit updated gcmarch script to entered exp dir.
    local _archqid=$1;shift
    local _arrdexp=( "$@" )
    local groupid=g0609
    local farchscr=run_gcmarch.sh
    local _arrstrexpid=( fcst myexp rim )
    local dexp

    /PBS/bin/pbs_rstat | grep $_archqid >>/dev/null 2>&1
    local status_grep=$?

    (( $status_grep == 0 )) && local blresarch_exist=true || local blresarch_exist=false

    local batch_joinouterr="PBS -j oe -k oed"
    local archive_p="PBS -l select=1:ncpus=20:mpiprocs=20:model=ivy"
    local archive_q="PBS -q normal"
    
    if $blresarch_exist ;then
        #local archive_pres="PBS -l select=1:ncpus=20:mpiprocs=20:model=ivy:aoe=sles13"
        local archive_pres="$archive_p"
        local archive_qres="PBS -q $_archqid"
    fi

    local BATCH_GROUP="PBS -W group_list="
    local farch=$DBUILD/Applications/GEOSgcm_App/$farchscr
    [[ ! -f $farch ]] && die "$farch does not exist"


    for dexp in ${_arrdexp[@]};do
       
        #todo:  set variables. 
        set_rstfcstdate $dexp

        local darch=$dexp/archive
        local farch_exp=$darch/$farchscr
        local farch_tmpexp=$darch/tmp_$farchscr
        local farch_exp_old=$darch/_$farchscr
        local farchlock=$( echo $farchscr | cut -d'_' -f2 | cut -d'.' -f1 ).lock
        local fsed=$cdir/sedfile_${strscr}_${FUNCNAME[0]}_$fcstdate$ensm

        [[ -f $farchlock ]] && die "$farchlock exist" 
        [[ -f $farch_exp_old ]] && rm -f $farch_exp_old 

        [[ -f $farch_tmpexp ]] && rm -f $farch_tmpexp
        [[ -f $fsed ]] && rm -f $fsed

        #todo:  if arch script in archive dir doesn't exist, ignore. If it does
        #       it is renamed with "_" in front of it.
        if [[ ! -d $darch ]];then
            continue
        else
            [[ -f $farch_exp ]] && mv $farch_exp $farch_exp_old
            cp -p $farch $farch_tmpexp
        fi

        #if [[ "${_arrstrexpid[@]}" =~ "$strexpid" ]];then  
        if $blres_arch && $blrunres_arch; then

            cat > $fsed << EOF
s?@\<DFCST\>?${DFCST}?g
s?@\<FCSTDATE\>?${fcstdate}?g
s?@\<ENSEMBLE_MEMBER\>?${ensm}?g
s?@\<DARCH\>?${DARCH}?g
s?@BATCH_GROUP?${BATCH_GROUP}${groupid}?g
s?@GROUPID?${groupid}?g
s?@\<BLLEAVEOUT\>?$blleaveout?g
s?@\<BLRSYNC\>?$blrsync?g
s?@\<ARCHIVE_P\>?${archive_p}?g
s?@\<ARCHIVE_Q\>?${archive_q}?g
s?@\<ARCHIVE_PRES\>?${archive_pres}?g
s?@\<ARCHIVE_QRES\>?${archive_qres}?g
s?@\<BATCH_JOINOUTERR\>?${batch_joinouterr}?g
EOF

        else

            cat > $fsed << EOF
s?@\<DFCST\>?${DFCST}?g
s?@\<FCSTDATE\>?${fcstdate}?g
s?@\<ENSEMBLE_MEMBER\>?${ensm}?g
s?@\<DARCH\>?${DARCH}?g
s?@BATCH_GROUP?${BATCH_GROUP}${groupid}?g
s?@GROUPID?${groupid}?g
s?@\<BLLEAVEOUT\>?$blleaveout?g
s?@\<BLRSYNC\>?$blrsync?g
s?@\<ARCHIVE_P\>?${archive_p}?g
s?@\<ARCHIVE_Q\>?${archive_q}?g
s?@\<BATCH_JOINOUTERR\>?${batch_joinouterr}?g
EOF

        fi

        #todo:  edit arch scr 
        sed -i -f $fsed $farch_tmpexp

        #todo:  edit PBS statement
        if $blrunres_arch && [[ -f $farch_tmpexp ]] ; then

            #pbsselect_res1=$( grep "$archive_pres" $farch_tmpexp | head -1 )
            pbsselect_res1=$( grep -w "$archive_pres" $farch_tmpexp )
            char2_select=$( echo $pbsselect_res1 | cut -c1-2 )
            
            pbsqid_res1=$( grep "$archive_qres" $farch_tmpexp )
            char2_qid=$( echo $pbsqid_res1 | cut -c1-2 )

            #pbsselect_nrm1=$( grep "$archive_p" $farch_tmpexp | tail -1 )
            pbsselect_nrm1=$( grep -w "$archive_p" $farch_tmpexp | grep -v "aoe=" )
            char2_select_nrm=$( echo $pbsselect_nrm1 | cut -c1-2 )
            
            pbsqid_nrm1=$( grep "$archive_q" $farch_tmpexp )
            char2_qid_nrm=$( echo $pbsqid_nrm1 | cut -c1-2 )

            [[ "$char2_select" == "##" ]] && pbsselect_res=$( echo $pbsselect_res1 | cut -c2- )
            [[ "$char2_qid" == "##" ]] && pbsqid_res=$( echo $pbsqid_res1 | cut -c2- )

            [[ "$char2_select_nrm" != "##" ]] && pbsselect_nrm=$( echo \#$pbsselect_nrm1 )
            [[ "$char2_qid_nrm" != "##" ]] && pbsqid_nrm=$( echo \#$pbsqid_nrm1 )

            if [[ "$archive_pres" == "$archive_p" ]];then
                sed -e "s/${pbsqid_res1}/${pbsqid_res}/g"       \
                    -e "s/${pbsqid_nrm1}/${pbsqid_nrm}/g"       \
                    $farch_tmpexp >> $farch_exp
                local status_sed=$?

            else
                #NOTE: 02/23/2022 this does not work at this time
                #sed -e "s/${pbsselect_res1}/${pbsselect_res}/g" \
                #    -e "s/${pbsqid_res1}/${pbsqid_res}/g"       \
                #    -e "s/${pbsselect_nrm1}/${pbsselect_nrm}/g" \
                #    -e "s/${pbsqid_nrm1}/${pbsqid_nrm}/g"       \
                #    $farch_tmpexp >> $farch_exp
                #local status_sed=$?

                cat $farch_tmpexp >> $farch_exp
                local status_sed=$?
            fi

            if (( $status_sed == 0 ));then        
                chmod 755 $farch_exp
            else
                wmessage "updating archive script failed - $dexp" 
                mv $farch_exp_old $farch_exp
            fi

        else
            if [[ -f $farch_exp_old && -f $farch_tmpexp ]];then 
                diff -s $farch_exp_old $farch_tmpexp >/dev/null
                local status_diff=$?
                if (( $status_diff > 0 ));then
                    mv $farch_tmpexp $farch_exp
                    chmod 755 $farch_exp
                    wmessage "Update completed - $farch_exp"
                    wmessage $( stat --print="%y %n" $farch_exp )
                else
                    mv $farch_exp_old $farch_exp
                    wmessage "$( stat --print="%y %n" $farch_exp )"
                fi

            elif [[ -f $farch_tmpexp ]];then
                mv $farch_tmpexp $farch_exp && chmod 755 $farch_exp
                wmessage $( stat --print="%y %n" $farch_exp )

            elif [[ ! -f $farch_tmpexp ]];then 
                wmessage "... $farch_tmpexp does not exist"
            fi
        fi

        [[ -f $farch_tmpexp ]] && rm -f $farch_tmpexp
        [[ -f $fsed ]] && rm -f $fsed
    
    done
    return
}

exp_updrunpostscr(){
    #description:   copy and update run_gcmpost.sh
    local _arrdexp=( "$@" )
    local fscr=run_gcmpost.sh
    local dexp

    local fpstscr=$DBUILD/Applications/GEOSgcm_App/$fscr
    [[ ! -f $fpstscr ]] && die "$fpstscr does not exist"

    for dexp in ${_arrdexp[@]};do
       
        #todo:  set variables. 
        set_rstfcstdate $dexp

        local dpst=$dexp/post
        local fpst_scr=$dpst/$fscr
        local fpst_tmpscr=$dpst/tmp_$fscr
        local fpst_oldscr=$dpst/_$fscr
        local fpstlock=$( echo $fscr | cut -d'_' -f2 | cut -d'.' -f1 ).lock
        local fsed=$cdir/sedfile_${strscr}_${FUNCNAME[0]}_$fcstdate$ensm
        
        [[ -f $fpstlock ]] && die "$fpstlock exist" 
        [[ -f $fpst_oldscr ]] && rm -f $fpst_oldscr

        [[ -f $fpst_tmpscr ]] && rm -f $fpst_tmpscr
        [[ -f $fsed ]] && rm -f $fsed

        #todo:  if arch script in archive dir doesn't exist, ignore. If it does
        #       it is renamed with "_" in front of it.
        if [[ ! -d $dpst ]];then
            continue
        else
            [[ -f $fpst_scr ]] && mv $fpst_scr $fpst_oldscr
            cp -p $fpstscr $fpst_tmpscr
        fi

        cat > $fsed << EOF
s?@\<DFCST\>?${DFCST}?g
s?@\<FCSTDATE\>?${fcstdate}?g
s?@\<ENSEMBLE_MEMBER\>?${ensm}?g
EOF

        #todo:  edit arch scr 
        sed -i -f $fsed $fpst_tmpscr

        if [[ -f $fpst_oldscr && -f $fpst_tmpscr ]];then 

            diff -s $fpst_oldscr $fpst_tmpscr >/dev/null
            local status_diff=$?
            if (( $status_diff > 0 ));then
                mv $fpst_tmpscr $fpst_scr
                chmod 755 $fpst_scr
                wmessage "Update completed - $fpst_scr"
                wmessage $( stat --print="%y %n" $fpst_scr )
            else
                mv $fpst_oldscr $fpst_scr
                wmessage "$( stat --print="%y %n" $fpst_scr )"
            fi
            
        elif [[ -f $fpst_tmpscr ]];then
            mv $fpst_tmpscr $fpst_scr && chmod 755 $fpst_scr
            wmessage $( stat --print="%y %n" $fpst_scr )
        
        elif [[ ! -f $fpst_tmpscr ]];then 
            wmessage "... $fpst_tmpscr does not exist"
        fi

        [[ -f $fsed ]] && rm -f $fsed
    done
    return
}



exp_updpstscr(){
    #02/18/2022 - work on @SETENV part of gcm_post_GEOSS2S3.j  
    #description:   resubmit gcm_post
    local fpst_default=$1;shift
    local dexp=$1;shift
    local _qid=$1
    local fpst_org=$dexp/post/gcm_post.j.org
    local fgcmpst_new=$dexp/post/tmp_${FUNCNAME[0]}_new_gcm_post.j
    local fgcmpst_tmp=$dexp/post/tmp_thisgcm_post.j
    local _fsed=$dexp/post/${FUNCNAME[0]}_sedfile
    
    die "(${FUNCNAME[0]}) This function is under construction as of Aug 2023"

    [[ -z $walltime_pst ]] && local walltime_pst="08:00:00" 

    local batch_group="PBS -W group_list=g0609"
    local batch_outputname="PBS -o "
    local batch_joinouterr="PBS -j oe -k oed"
    
    local pbsselect_res_org="PBS -l select=1:ncpus=20:mpiprocs=20:model=ivy:aoe=toss4"
    local pbsqid_res_org="PBS -q $pstqid"
    local pbsselect_nrm_org="PBS -l select=1:ncpus=20:mpiprocs=20:model=ivy"
    local pbsqid_nrm_org="PBS -q normal"
       
    /PBS/bin/pbs_rstat | grep $_qid >>/dev/null 2>&1
    local status_grep=$?

    (( $status_grep == 0 )) && local blres_exist=true || local blres_exist=false

    if $blres_exist ;then
        #local archive_pres="PBS -l select=1:ncpus=20:mpiprocs=20:model=ivy:aoe=sles13"
        local archive_pres="$archive_p"
        local archive_qres="PBS -q $_archqid"
    fi

    set_rstfcstdate $dexp 

    [[ -f $_fsed ]] && rm -f $_fsed
    [[ -f $fgcmpst_new ]] && rm -f $fgcmpst_new
    [[ ! -f $fpst_org ]] && mv $dexp/post/gcm_post.j $fpst_org

    #todo:  recreate gcm_post.j 
    local lnnum=$( grep -n "System Environment Variables" $fpst_default 2>/dev/null | cut -d':' -f1 | xargs -i bash -c 'echo "{} - 2 " | bc' )  
    cat $fpst_default | head -n $lnnum >> $fgcmpst_new
    
    lnnum1=$( grep -n "System Environment Variables" $fpst_org 2>/dev/null | cut -d':' -f1 | xargs -i bash -c 'echo "{} - 2 " | bc' )  
    lnnum2=$( grep -n "Perform Post Processing" $fpst_org 2>/dev/null | cut -d':' -f1 | xargs -i bash -c 'echo "{} - 2 " | bc' )  
    cat $fpst_org | head -$lnnum2 | tail -n +$lnnum1 >> $fgcmpst_new
    
    lnnum=$( grep -n "Perform Post Processing" $fpst_default 2>/dev/null | cut -d':' -f1 | xargs -i bash -c 'echo "{} - 2 " | bc' )  
    cat $fpst_default | tail -n +$lnnum >> $fgcmpst_new
    
    sed -e "s#@\<EXPDIR\>#$dexp#g" $fgcmpst_new >> $fgcmpst_tmp
 
    [[ -f $fgcmpst_tmp ]] && mv $fgcmpst_tmp $fgcmpst_new

    #todo:  select pbs statements 
    if $blrunres && $blres;then
        local pbsselect_res=$pbsselect_res_org
        local pbsqid_res=$pbsqid_res_org
        local pbsselect_nrm="#$pbsselect_nrm_org"
        local pbsqid_nrm="#$pbsqid_nrm_org"

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
s?@\<POSTPRC_P\>?${pbsselect_nrm}?g
s?@\<POST_Q\>?${pbsqid_nrm}?g
s?@\<BATCH_GROUP\>?${batch_group}?g
s?@\<BATCH_JOINOUTERR\>?${batch_joinouterr}?g
s?@BATCH_OUTPUTNAME?${batch_outputname}?g
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

writeheader(){
    local _hst=$( hostname )
    msg_wheader
    wmessage $_hst
    wmessage "          exp location: "$1
    wmessage "      archive location: "$2
    return
}

exp_createfname_save(){
    #description:   based on collection and YYYYMM, create filenames that are suppoed to exists and should be
    #               saved on pfe

    [[ -n $1 ]] && local _dexp=$1      && shift || die "(${FUNCNAME[0]}) input argument required" 
    [[ -n $1 ]] && local _coll=$1      && shift || die "(${FUNCNAME[0]}) input argument required" 
    [[ -n $1 ]] && local _blfrstday=$1 && shift || die "(${FUNCNAME[0]}) input argument required" 
    [[ -n $1 ]] && local _bllastday=$1 && shift || die "(${FUNCNAME[0]}) input argument required" 
    [[ -n $1 ]] && local _blmapl=$1    && shift || die "(${FUNCNAME[0]}) input argument required" 
    [[ -n $1 ]] && local _blmon=$1     && shift || die "(${FUNCNAME[0]}) input argument required" 
    [[ -n $1 ]] && local _bldiu=$1     && shift || die "(${FUNCNAME[0]}) input argument required" 
    [[ -n $1 ]] && local _bldya=$1     && shift || die "(${FUNCNAME[0]}) input argument required" 
    [[ -n $1 ]] && local _bldy3=$1     && shift || die "(${FUNCNAME[0]}) input argument required" 
    [[ -n $1 ]] && local _blnoreft=$1  && shift || die "(${FUNCNAME[0]}) input argument required" 
    [[ -n $1 ]] && local _arrmfull=( "$@" )     || die "(${FUNCNAME[0]}) input argument required" 
    local _strmapl="01_0000z.nc4"
    local _arrstr=( daily monthly diurnal )
    local _arrfname=()
    local _str

    [[ ! -d $_dexp  ]] && die "(${FUNCNAME[0]}) exp dir does not exists - $_dexp" 
    [[ -z $fcstdate ]] && set_rstfcstdate $dexp
    
    #mapl file names:    
    #20210531.trb_tavg_1mo_glo_L720x361_p49.20210601_0000z.nc4
    #$fcstdate.$coll.${yyyymm}01_0000z.nc4-partial
    #$fcstdate.$coll.${yyyymm}01_0000z.nc4

    #postproc output file names
    #19840101.aer_inst_3hr_glo_L720x361_slv.diurnal.198402.nc4
    #$fcstdate.$coll.diurnal.YYYYMM.nc4
    #gcm_post.ocn_inst_6hr_glo_L1440x721_z50.j198402
    #gcm_post.$coll.jYYYYMM
    
    $_bllastday && local numfmon=3 || local numfmon=2

    #todo:  create filenames, which are suppoed to exists, based on collection and YYYYMM
    if $_blmapl;then
        local fname1=$_dexp/$_coll/$fcstdate.$_coll.
        _arrmfull=($( printf '%s\n' ${_arrmfull[@]} | grep -v $capric_yyyymm )) 

        if $_bllastday;then 
            _arrfname+=( ${fname1}${capric_yyyymm}${_strmapl}-partial )
            _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s${_strmapl}\n" ${_arrmfull[@]} ))
        else
            _arrfname+=( ${fname1}${fcstdate:0:6}${_strmapl}-partial )
            _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s${_strmapl}\n" ${_arrmfull[@]} ))
        fi
    else
        for _str in ${_arrstr[@]};do
            local fname1=$_dexp/$_coll/$fcstdate.$_coll.${_str}.

            if $_bllastday;then 
                if $_bldy3 || $_blnoreft;then 
                    _arrmfull=( ${_arrmfull[@]} ) 
                else
                    _arrmfull=($( printf '%s\n' ${_arrmfull[@]} | grep -v $capric_yyyymm )) 
                fi
            fi

            if $_bldya && [[ "$_str" == daily ]];then 
                _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4.tar\n" ${_arrmfull[@]} ))

            elif $_bldy3 && [[ "$_str" == daily ]];then 
                #local _arrmfull_3mo=($( printf '%s\n' ${_arrmfull[@]} | sort -n | head -$numfmon ))
                #_arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4.tar\n" ${_arrmfull_3mo[@]} ))
                _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4.tar\n" ${_arrmfull[@]} ))

            elif $_blmon && [[ "$_str" == monthly ]];then 
                #$_blfrstday && $_blnoreft && local _arrmfull_3mo=( $capric_yyyymm ${_arrmfull[@]} ) || local _arrmfull_3mo=( ${_arrmfull[@]} ) 
                #_arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4\n" ${_arrmfull_3mo[@]} ))
                _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4\n" ${_arrmfull[@]} ))

            elif $_bldiu && [[ "$_str" == diurnal ]];then 
                _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4\n" ${_arrmfull[@]} ))
            fi
        done 
    fi

    echo "${_arrfname[@]}"
    
}


_exp_createfname_save(){
    #description:   based on collection and YYYYMM, create filenames that are suppoed to exists and should be
    #               saved on pfe

    [[ -n $1 ]] && local _dexp=$1      && shift || die "(${FUNCNAME[0]}) input argument required" 
    [[ -n $1 ]] && local _coll=$1      && shift || die "(${FUNCNAME[0]}) input argument required" 
    [[ -n $1 ]] && local _blfrstday=$1 && shift || die "(${FUNCNAME[0]}) input argument required" 
    [[ -n $1 ]] && local _bllastday=$1 && shift || die "(${FUNCNAME[0]}) input argument required" 
    [[ -n $1 ]] && local _blmapl=$1    && shift || die "(${FUNCNAME[0]}) input argument required" 
    [[ -n $1 ]] && local _blmon=$1     && shift || die "(${FUNCNAME[0]}) input argument required" 
    [[ -n $1 ]] && local _bldiu=$1     && shift || die "(${FUNCNAME[0]}) input argument required" 
    [[ -n $1 ]] && local _bldya=$1     && shift || die "(${FUNCNAME[0]}) input argument required" 
    [[ -n $1 ]] && local _bldy3=$1     && shift || die "(${FUNCNAME[0]}) input argument required" 
    [[ -n $1 ]] && local _blnoreft=$1  && shift || die "(${FUNCNAME[0]}) input argument required" 
    [[ -n $1 ]] && local _arrmfull=( "$@" )     || die "(${FUNCNAME[0]}) input argument required" 
    local _strmapl="01_0000z.nc4"
    local _arrstr=( daily monthly diurnal )
    local _arrfname=() 
    local _str

#wmessage lineno = $LINENO ${#_arrstr[@]}  ${#_arrfname[@]} ${#arrfexist_save[@]} 
    [[ ! -d $_dexp  ]] && die "(${FUNCNAME[0]}) exp dir does not exists - $_dexp" 
    [[ -z $fcstdate ]] && set_rstfcstdate $dexp
    
    #mapl file names:    
    #20210531.trb_tavg_1mo_glo_L720x361_p49.20210601_0000z.nc4
    #$fcstdate.$coll.${yyyymm}01_0000z.nc4-partial
    #$fcstdate.$coll.${yyyymm}01_0000z.nc4

    #postproc output file names
    #19840101.aer_inst_3hr_glo_L720x361_slv.diurnal.198402.nc4
    #$fcstdate.$coll.diurnal.YYYYMM.nc4
    #gcm_post.ocn_inst_6hr_glo_L1440x721_z50.j198402
    #gcm_post.$coll.jYYYYMM
    
    $_bllastday && local numfmon=3 || local numfmon=2

    #todo:  create filenames, which are suppoed to exists, based on collection and YYYYMM
    if $_blmapl;then
        local fname1=$_dexp/$_coll/$fcstdate.$_coll.

        if $_bllastday;then 
            _arrfname+=( ${fname1}${capric_yyyymm}01${_strmapl}-partial )
            _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s${_strmapl}\n" ${_arrmfull[@]} ))
        else
            _arrfname+=( ${fname1}${fcstdate:0:6}${_strmapl}-partial )
            _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s${_strmapl}\n" ${_arrmfull[@]} ))
        fi
    else
        for _str in ${_arrstr[@]};do
            local fname1=$_dexp/$_coll/$fcstdate.$_coll.${_str}.

            if $_bldya && [[ "$_str" == daily ]];then 
                _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4.tar\n" ${_arrmfull[@]} ))

            elif $_bldy3 && [[ "$_str" == daily ]];then 
                local _arrmfull_3mo=($( printf '%s\n' ${_arrmfull[@]} | sort -n | head -$numfmon ))
                _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4.tar\n" ${_arrmfull_3mo[@]} ))

            elif $_blmon && [[ "$_str" == monthly ]];then 
                $_blfrstday && $_blnoreft && local _arrmfull_3mo=( $capric_yyyymm ${_arrmfull[@]} ) || local _arrmfull_3mo=( ${_arrmfull[@]} ) 
                _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4\n" ${_arrmfull_3mo[@]} ))

            elif $_bldiu && [[ "$_str" == diurnal ]];then 
                _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4\n" ${_arrmfull[@]} ))
            fi
        done 
    fi

    echo "${_arrfname[@]}"
    
}

exp_createfname(){
    #description:   based on collection and YYYYMM, create filenames that are suppoed to exists 
    [[ -n $1 ]] && local _dexp=$1      && shift || die "(${FUNCNAME[0]}) input argument required"
    [[ -n $1 ]] && local _coll=$1      && shift || die "(${FUNCNAME[0]}) input argument required"
    [[ -n $1 ]] && local _bllastday=$1 && shift || die "(${FUNCNAME[0]}) input argument required"
    [[ -n $1 ]] && local _blfrstday=$1 && shift || die "(${FUNCNAME[0]}) input argument required"
    [[ -n $1 ]] && local _bldy3=$1     && shift || die "(${FUNCNAME[0]}) input argument required" 
    [[ -n $1 ]] && local _blnoreft=$1  && shift || die "(${FUNCNAME[0]}) input argument required" 
    [[ -n $1 ]] && local _arrmfull=( "$@" )     || die "(${FUNCNAME[0]}) input argument required"
    local _strmapl="01_0000z.nc4"
    local _arrstr=( daily monthly diurnal )
    local _arrfname=()
    local _str

    [[ ! -d $_dexp  ]] && die "(${FUNCNAME[0]}) exp dir does not exists - $_dexp" 

    #mapl file names:    
    #20210531.trb_tavg_1mo_glo_L720x361_p49.20210601_0000z.nc4
    #$fcstdate.$coll.${yyyymm}01_0000z.nc4-partial
    #$fcstdate.$coll.${yyyymm}01_0000z.nc4

    #postproc output file names
    #19840101.aer_inst_3hr_glo_L720x361_slv.diurnal.198402.nc4
    #$fcstdate.$coll.diurnal.YYYYMM.nc4
    #gcm_post.ocn_inst_6hr_glo_L1440x721_z50.j198402
    #gcm_post.$coll.jYYYYMM

    $_bllastday && local numfmon=3 || local numfmon=2

    [[ "$_coll" == *"_1mo_glo_"* ]] && local _blmapl=true || local _blmapl=false
    [[ "$_coll" == *"_1dy_glo_"* ]] && local _bl1dy=true  || local _bl1dy=false

    #todo:  create filenames that are suppoed to exists, based on collection and YYYYMM
    if $_blmapl;then
        local fname1=$_dexp/$_coll/$fcstdate.$_coll.
        _arrmfull=($( printf '%s\n' ${_arrmfull[@]} | grep -v $capric_yyyymm )) 

        if $_bllastday;then 
            _arrfname+=( ${fname1}${capric_yyyymm}${_strmapl}-partial )
            _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s${_strmapl}\n" ${_arrmfull[@]} ))
        else
            _arrfname+=( ${fname1}${fcstdate:0:6}${_strmapl}-partial )
            _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s${_strmapl}\n" ${_arrmfull[@]} ))
        fi
    else
        for _str in ${_arrstr[@]};do
            local fname1=$_dexp/$_coll/$fcstdate.$_coll.${_str}.
            if $_bllastday;then 
                if $_bldy3 || $_blnoreft ;then 
                    _arrmfull=( ${_arrmfull[@]} )
                else
                    _arrmfull=($( printf '%s\n' ${_arrmfull[@]} | grep -v $capric_yyyymm )) 
                fi
            fi

            if   [[ "$_str" == daily ]];then 
                #if $_blfrstday && $_blnoreft;then
                #    local _arrmfull_3mo=( $capric_yyyymm ${_arrmfull[@]} )  
                #    _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4.tar\n" ${_arrmfull_3mo[@]} ))
                #else
                #    _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4.tar\n" ${_arrmfull[@]} ))
                #fi
                    
                _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4.tar\n" ${_arrmfull[@]} ))

            elif [[ "$_str" == monthly ]];then 
                _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4\n" ${_arrmfull[@]} ))

            elif ! $_bl1dy && [[ "$_str" == diurnal ]];then 
                _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4\n" ${_arrmfull[@]} ))
            fi
        done 
    fi

    echo "${_arrfname[@]}"
    
}


_exp_createfname(){
    #description:   based on collection and YYYYMM, create filenames that are suppoed to exists 
    [[ -n $1 ]] && local _dexp=$1      && shift || die "(${FUNCNAME[0]}) input argument required"
    [[ -n $1 ]] && local _coll=$1      && shift || die "(${FUNCNAME[0]}) input argument required"
    [[ -n $1 ]] && local _bllastday=$1 && shift || die "(${FUNCNAME[0]}) input argument required"
    [[ -n $1 ]] && local _blfrstday=$1 && shift || die "(${FUNCNAME[0]}) input argument required"
    [[ -n $1 ]] && local _blnoreft=$1  && shift || die "(${FUNCNAME[0]}) input argument required" 
    [[ -n $1 ]] && local _arrmfull=( "$@" )     || die "(${FUNCNAME[0]}) input argument required"
    local _strmapl="01_0000z.nc4"
    local _arrstr=( daily monthly diurnal )
    local _arrfname=()
    local _str

    [[ ! -d $_dexp  ]] && die "(${FUNCNAME[0]}) exp dir does not exists - $_dexp" 

    #mapl file names:    
    #20210531.trb_tavg_1mo_glo_L720x361_p49.20210601_0000z.nc4
    #$fcstdate.$coll.${yyyymm}01_0000z.nc4-partial
    #$fcstdate.$coll.${yyyymm}01_0000z.nc4

    #postproc output file names
    #19840101.aer_inst_3hr_glo_L720x361_slv.diurnal.198402.nc4
    #$fcstdate.$coll.diurnal.YYYYMM.nc4
    #gcm_post.ocn_inst_6hr_glo_L1440x721_z50.j198402
    #gcm_post.$coll.jYYYYMM

    $_bllastday && local numfmon=3 || local numfmon=2

    [[ "$_coll" == *"_1mo_glo_"* ]] && local _blmapl=true || local _blmapl=false
    [[ "$_coll" == *"_1dy_glo_"* ]] && local _bl1dy=true  || local _bl1dy=false

    #todo:  create filenames that are suppoed to exists, based on collection and YYYYMM
    if $_blmapl;then
        local fname1=$_dexp/$_coll/$fcstdate.$_coll.

        if $_bllastday;then 
            _arrfname+=( ${fname1}${capric_yyyymm}01${_strmapl}-partial )
            _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s${_strmapl}\n" ${_arrmfull[@]} ))
        else
            _arrfname+=( ${fname1}${fcstdate:0:6}${_strmapl}-partial )
            _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s${_strmapl}\n" ${_arrmfull[@]} ))
        fi
    else
        for _str in ${_arrstr[@]};do
            local fname1=$_dexp/$_coll/$fcstdate.$_coll.${_str}.

            if   [[ "$_str" == daily ]];then 
                if $_blfrstday && $_blnoreft;then
                    local _arrmfull_3mo=( $capric_yyyymm ${_arrmfull[@]} )  
                    _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4.tar\n" ${_arrmfull_3mo[@]} ))
                else
                    _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4.tar\n" ${_arrmfull[@]} ))
                fi

            elif [[ "$_str" == monthly ]];then 
                $_blfrstday && $_blnoreft && local _arrmfull_3mo=( $capric_yyyymm ${_arrmfull[@]} ) || local _arrmfull_3mo=( ${_arrmfull[@]} ) 
                _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4\n" ${_arrmfull_3mo[@]} ))

            elif ! $_bl1dy && [[ "$_str" == diurnal ]];then 
                _arrfname+=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4\n" ${_arrmfull[@]} ))
            fi
        done 
    fi

    echo "${_arrfname[@]}"
    
}

exp_archstatus(){
    local _thisinput=$1;shift
    local _srcme=$1

    [[ -n $_srcme &&   -f $_srcme ]] && source $_srcme
    [[ -n $_srcme && ! -f $_srcme ]] && die "(${FUNCNAME[0]}) srcme file does not exist"

    [[ -z $DFCST || -z $DARCH || -z $strexpid || -z $strdout ]] && \
        die "(${FUNCNAME[0]}) source srcme file before execute or provide it as an input"

    [[ -n $rstexpid ]] && local _strexpid=$strexpid$rstexpid

    local fcomp=gcmarch_archcompleted
    local fdel=gcmarch_deloutcompleted
    local fdelout=gcmarch_delout
    local _farc_stderr=archive/stderr_gcmarch
    local fmark_clean=clean_completed
    local dwin=$cdir/output/pckwinners/$strdout
    local strmsg="$_thisinput"
    local _msgsubject="${thishst}.${strscr} ${FUNCNAME[0]}: $_strexpid ( User Input : $_thisinput )"
    local cnt_done=0
    local _pfrmt1="%-15s" 
    local _pfrmt2="%+8s"
    local _pfrmt3="%+8s" #%+5s %+5s %+5s %+5s %+5s"
    local _space4=5
    local _pfrmt4="%+${_space4}s" #%+5s %+5s %+5s %+5s %+5s"
    local _pfrmt="$_pfrmt1 $_pfrmt2 $_pfrmt3 $_pfrmt4 $_pfrmt4 $_pfrmt4 $_pfrmt4 $_pfrmt4 $_pfrmt4"
    local _head1=69
    local _foot1=64
    local _foot2=69
    local _foot3=$(( _foot1 + 5 ))
    local arrdexp_pfeoutallexist=() 
    local arrdexp_missoutsame=() 
    local arrdexp_pfemommiss=()
    local arrdexp_dholdmiss=()
    local bl_wpstlegend=false
    local x 
    local inputint=$( misc_isinteger $_thisinput )

    [[ -f $_thisinput ]] && local blfle=true || local blfle=false
    (( $inputint == 0 )) && local blint=true || local blint=false

    #todo:  check user input
      $blfle &&   $blint && die "user input is unrecognizable form"
    ! $blfle && ! $blint && die "user input is unrecognizable form"

    if $blfle && ! $blint;then
        local _arrdexp1=($( cat $_thisinput ))
        local _arrdexp=($( printf "%s\n" ${_arrdexp1[@]} | xargs -i bash -c "[[ -d {} ]] && echo {}" ))
    elif ! $blfle && $blint;then
        local _arrdexp=($( find $DFCST/* -maxdepth 1 -mindepth 1 -type d -name 'ens*' 2>/dev/null | grep $_thisinput | sort -V ))
        (( ${#_arrdexp[@]} == 0 )) && return
    fi

    #todo:  extract exp with archive completed. 
    local arrdexp_clndone=($( printf '%s\n' ${_arrdexp[@]} | xargs -i bash -c " [[ -f {}/clean_completed ]] && echo {} " ))
    local       _arrdexp1=($( printf '%s\n' ${_arrdexp[@]} ${arrdexp_clndone[@]} | sort -V | uniq -u ))
    local arrdexp_arcdone=($( printf '%s\n' ${_arrdexp1[@]} | xargs -i bash -c " [[ -f {}/archive/$fcomp && -f {}/archive/$fdel ]] && echo {} " ))
    local arrdexp_arcunfn=($( printf '%s\n' ${_arrdexp1[@]} ${arrdexp_arcdone[@]} | sort -V | uniq -u ))
    local arrdexp_dhldmis=($( printf '%s\n' ${_arrdexp1[@]} | xargs -i bash -c " [[ -f {}/archive/note_gcmarch ]] && echo {} " ))

    #todo:  get all running/q'd jobs 
    local arrjobs=($( /u/scicon/tools/bin/qstat -u $USER -W fmt_Queue="-maxw 40" -W o=+Rank0 | sort -V -k4 -t' ' | tr -s '[:space:]' | cut -d' ' -f4 | cut -d'.' -f1 | sort -V | uniq | grep ens ))

    msg_wheader 

    wmessage "User Inputs : $strmsg"
    wmessage "                      Number of Exp Cleaned : $( printf '%+5s\n' ${#arrdexp_clndone[@]} )"
    wmessage "         Exp with Missing YYYYMM in holding : $( printf '%+5s\n' ${#arrdexp_dhldmis[@]} )"
    wmessage "         Exp with  Archive/Delout Completed : $( printf '%+5s\n' ${#arrdexp_arcdone[@]} )"
    wmessage "                                 Incomplete : $( printf '%+5s\n' ${#arrdexp_arcunfn[@]} )"
    wmessage "Arch Incomplete:"
    ahand_warr ${arrdexp_arcunfn[@]} 
    
    wmessage
    wmessage
    wmessage "+++++ Details +++++"
    wmessage

    wmessage "$( printf "${_pfrmt}\n" yyyymmdd/ensX winner "" total mapl daily diur month mom )"
    wmessage "$( printf -- '-%.0s' $( seq 1 $_head1 ))"

    for x in {1..3};do
        local _thisarrdexp=()

        if (( $x == 1 && ${#arrdexp_arcunfn[@]} > 0 ));then
            local _thisarrdexp=( ${arrdexp_arcunfn[@]} ) 

        elif (( $x == 2 && ${#arrdexp_arcdone[@]} > 0 ));then
            local _thisarrdexp=( ${arrdexp_arcdone[@]} )

        elif (( $x == 3 && ${#arrdexp_clndone[@]} > 0));then
            local _thisarrdexp=( ${arrdexp_clndone[@]} ) 

        else 
            continue
        fi

        for dexp in ${_thisarrdexp[@]};do
            local farchcomp=$dexp/archive/$fcomp
            local farchdel=$dexp/archive/$fdel
            local farchdelout=$dexp/archive/$fdelout
            local farchnote=$dexp/archive/note_gcmarch
            local _fstderr=$dexp/$_farc_stderr
            local blcomp=false
            local bldelo=false

            set_rstfcstdate $dexp

            #todo:  skip exp which hasn't started to run
            if (( $x == 1 || $x == 2 ));then
                local _numfstdout=$( find $dexp/* -maxdepth 0 -type f -name "R${fcstdate}${ensm}.o*" | wc -l ) 
                (( $_numfstdout == 0 )) && continue 
            fi

            count_files $dexp

            local blwin=$( exp_checkwinner $dexp $dwin )
            [[ -f $dexp/gcm_run.j ]] && local blrerun=$( grep -i "RERUN" $dexp/gcm_run.j 2>/dev/null | head -1 | rev | cut -d' ' -f1 | rev | tr '[:upper:]' '[:lower:]'  ) \
                                     || local blrerun=false

            $blwin || $blrerun && local _blthis=true || local _blthis=false 

            local arrcalc=($( exp_calcoutput $dexp $_blthis $dexp/HISTORY_2.rc ))

            if $blleaveout;then
                local arrcalcsave=($( exp_calcoutput_save $dexp $blwin $dexp/HISTORY_2.rc ))
            else
                local arrcalcsave=( 0 0 0 0 0 )
            fi

            local cntfsftc=$( find $dexp/archive/* -type f -name 'gcmarch_shiftin_*' 2>/dev/null | wc -l )
            [[ -f $farchdelout ]] && local numfpfe_del_cnt=$( cat $farchdelout | grep -vE "${strmom_search}|${strrst}" 2>/dev/null | sort -V | uniq | wc -l ) || local numfpfe_del_cnt=0
            local numfpfe_mis_cnt=$( echo "${arrcalcsave[0]} - $numfpfe_total_cnt - $numfpfe_del_cnt" | bc )
            local numflfe_mis_cnt=$( echo "${arrcalc[0]} - $numflfe_total_cnt" | bc )
            local numfpst=$( printf '%s\n' ${arrjobs[@]} | grep P${fcstdate}${ensm}. 2>/dev/null | wc -l )
            [[ -f $farchnote ]] && local numdhold_miss=$( cat $farchnote | wc -l ) || local numdhold_miss=0

            (( $numfpfe_mis_cnt == 0 && $numflfe_mis_cnt > 0 )) && arrdexp_pfeoutallexist+=( $dexp )
            (( $numfpfe_mis_cnt == $numflfe_mis_cnt          )) && arrdexp_missoutsame+=( $dexp )
            (( $numfpfe_mom_cnt <  $numflfe_mom_cnt          )) && arrdexp_pfemommiss+=( $dexp )
            #(( $numdhold_miss   >  0                         )) && arrdexp_dholdmiss+=( $dexp )

            $blwin && local strwin="Winner" || local strwin=" "

            if (( $x == 1 || $x == 2 ));then
                wmessage "$( printf "${_pfrmt}\n" $( echo $dexp | rev | cut -d'/' -f1-2 | rev ) "$strwin" "calc" ${arrcalc[@]} "-" )"
                wmessage "$( printf "${_pfrmt}\n" "" ""    "save" ${arrcalcsave[@]} "0" )"
                wmessage "$( printf "${_pfrmt}\n" "" "" "pfe cnt" $numfpfe_total_cnt $numfpfe_monmapl_cnt $numfpfe_day_cnt $numfpfe_diu_cnt $numfpfe_monpost_cnt $numfpfe_mom_cnt )"

            elif (( $x == 3 ));then
                wmessage "$( printf "${_pfrmt}\n" $( echo $dexp | rev | cut -d'/' -f1-2 | rev ) "$strwin" "pfe cnt" $numfpfe_total_cnt $numfpfe_monmapl_cnt $numfpfe_day_cnt $numfpfe_diu_cnt $numfpfe_monpost_cnt $numfpfe_mom_cnt )"
            fi

            wmessage "$( printf "${_pfrmt}\n" "" "" "$hstarc cnt" $numflfe_total_cnt $numflfe_monmapl_cnt $numflfe_day_cnt $numflfe_diu_cnt $numflfe_monpost_cnt $numflfe_mom_cnt )"

            if (( $x == 1 || $x == 2 ));then

                #+++++ cd dexp/archive (start) +++++
                cd $dexp/archive
                [[ -f $fcomp ]]        && wmessage $( stat --print="%.19y %n" $fcomp ) && local blcomp=true 
                [[ -f $fdel  ]]        && wmessage $( stat --print="%.19y %n" $fdel )  && local bldelo=true 
                cd - >/dev/null
                #+++++ cd dexp/archive ( end ) +++++

                (( $cntfsftc        > 0 )) && wmessage "$( printf "%+${_foot1}s %+4s\n" "Remaining Shiftc Files -" $cntfsftc )"
                (( $numfpfe_mis_cnt > 0 )) && ! $blcomp && ! $bldelo && \
                                              wmessage "$( printf "%+${_foot1}s %+4s\n" "  Missing Files on Pfe -" $numfpfe_mis_cnt )" 
                (( $numflfe_mis_cnt > 0 )) && wmessage "$( printf "%+${_foot1}s %+4s\n" "  Missing Files on Lfe -" $numflfe_mis_cnt )" 
                (( $numfpst         > 0 )) && wmessage "$( printf "%+${_foot1}s %+4s\n" "Running/Queue gcm_post -" $numfpst         )" 
                (( $numdhold_miss   > 0 )) && wmessage "$( printf "%+${_foot1}s %+4s\n" "    Missing YYYYMM dir -" $numdhold_miss   )" 
                $blcomp && $bldelo && (( $cntfsftc == 0 && $numflfe_mis_cnt == 0 )) && \
                                              wmessage "$( printf "%+${_foot2}s\n" "Archive/Delout - DONE Yay!")"
                #todo   get gcm_post that run_gcmarch had to resubmit
                if [[ -f $_fstderr ]] && (( $x == 1 ));then 
                    local _arrpststatus=()

                    local _arrfpst_sub=($( grep gcm_post. $_fstderr 2>/dev/null | grep _glo_ 2>/dev/null | grep -v Make | tr -s '[:space:]' | rev | cut -d' ' -f1 | rev | sed 's#^ *##g' | sort -V ))
                    local _arrfpst=($( printf '%s\n' ${_arrfpst_sub[@]} | sort -V | uniq ))
                    local _blpst_noissue=true

                    for fpst in ${_arrfpst[@]} ;do
                        _status="$( pststatus $dexp/post/$fpst )"
                        _blpst_noissue=$( echo $_status | cut -d':' -f1 ) 
                        ! $_blpst_noissue && break
                    done

                    if ! $_blpst_noissue;then 
                        wmessage
                        wmessage "gcm_post with Errors:"
                        for fpst in ${_arrfpst[@]} ;do
                            local _status="$( pststatus $dexp/post/$fpst )"
                            local _blpst_noissue=$( echo $_status | cut -d':' -f1 ) 
                            
                            ! $_blpst_noissue && wmessage "$( echo $_status | cut -d':' -f2- | sed 's#:# #g' )"
                        done
                    fi
                fi

            elif (( $x == 3 ));then
                [[ -f $dexp/$fmark_clean ]] && wmessage "$( printf "%+${_foot3}s\n"      "********* Cleaning Completed *********")"
            fi

            wmessage
            wmessage "$( printf -- '-%.0s' $( seq 1 $_head1 ))"
        done 
    
        wmessage "$( printf -- '-%.0s' $( seq 1 $_head1 ))"
    done

    wmessage
    wmessage
    if (( ${#arrdexp_pfeoutallexist[@]} > 0 ));then
        wmessage "Number of Exp with All Output Exists but Archive/Delout is not completed : $( printf '%+5s\n' ${#arrdexp_pfeoutallexist[@]}    )"
        ahand_warr ${arrdexp_pfeoutallexist[@]} 
        wmessage 
    fi
    if (( ${#arrdexp_missoutsame[@]} > 0 ));then
        wmessage "# of Output on PFE = # of Outputs on LFE : $( printf '%+5s\n' ${#arrdexp_missoutsame[@]}    )"
        ahand_warr ${arrdexp_missoutsame[@]} 
        wmessage 
    fi
    if (( ${#arrdexp_pfemommiss[@]} > 0 ));then
        local arrdexp_mommiss=($( printf '%s\n' ${arrdexp_pfemommiss[@]} ${arrdexp_arcdone[@]} | sort -V | uniq -u ))

        wmessage "Archiving Incomplete && (( # of MOM Output on PFE < # of MOM Outputs on LFE )): $( printf '%+5s\n' ${#arrdexp_mommiss[@]}    )"
        ahand_warr ${arrdexp_mommiss[@]} 
        wmessage 
    fi
    if $bl_wpstlegend;then 
        note_pststatus
    fi
    
    return 
}


_exp_archstatus(){
    #NOTE: exp_archstatus HAS BEEN UPDATED (20230227). DELETE THIS FUNCTION LATER TIME
    local _thisinput=$1;shift
    local _srcme=$1

    [[ -n $_srcme &&   -f $_srcme ]] && source $_srcme
    [[ -n $_srcme && ! -f $_srcme ]] && die "(${FUNCNAME[0]}) srcme file does not exist"

    [[ -z $DFCST || -z $DARCH || -z $strexpid || -z $strdout ]] && \
        die "(${FUNCNAME[0]}) source srcme file before execute or provide it as an input"

    [[ -n $rstexpid ]] && local _strexpid=$strexpid$rstexpid

    #local fmessage=$dmess/message_${strscr}_${FUNCNAME[0]}_${_strexpid}
    local fcomp=gcmarch_archcompleted
    local  fdel=gcmarch_deloutcompleted
    local  fdelout=gcmarch_delout
    local fmark_clean=clean_completed
    local dwin=$cdir/output/pckwinners/$strdout
    local strmsg="$_thisinput"
    local _msgsubject="${thishst}.${strscr} ${FUNCNAME[0]}: $_strexpid ( User Input : $_thisinput )"
    local cnt_done=0
    local _pfrmt1="%-15s" 
    local _pfrmt2="%+8s"
    local _pfrmt3="%+8s" #%+5s %+5s %+5s %+5s %+5s"
    local _space4=5
    local _pfrmt4="%+${_space4}s" #%+5s %+5s %+5s %+5s %+5s"
    local _pfrmt="$_pfrmt1 $_pfrmt2 $_pfrmt3 $_pfrmt4 $_pfrmt4 $_pfrmt4 $_pfrmt4 $_pfrmt4 $_pfrmt4"
    local _head1=69
    local _foot1=64
    local _foot2=69
    local _foot3=$(( _foot1 + 5 ))
    local arrdexp_pfeoutallexist=() 
    local arrdexp_missoutsame=() 
    local arrdexp_pfemommiss=()
    local arrdexp_dholdmiss=()
    local x 
    local inputint=$( misc_isinteger $_thisinput )

    [[ -f $_thisinput ]] && local blfle=true || local blfle=false
    (( $inputint == 0 )) && local blint=true || local blint=false

    #todo:  check user input
      $blfle &&   $blint && die "user input is unrecognizable form"
    ! $blfle && ! $blint && die "user input is unrecognizable form"

    if $blfle && ! $blint;then
        local _arrdexp1=($( cat $_thisinput ))
        local _arrdexp=($( printf "%s\n" ${_arrdexp1[@]} | xargs -i bash -c "[[ -d {} ]] && echo {}" ))
    elif ! $blfle && $blint;then
        local _arrdexp=($( find $DFCST/* -maxdepth 1 -mindepth 1 -type d -name 'ens*' 2>/dev/null | grep $_thisinput | sort -V ))
        (( ${#_arrdexp[@]} == 0 )) && exit
    fi

    #todo:  extract exp with archive completed. 
    local arrdexp_clndone=($( printf '%s\n' ${_arrdexp[@]} | xargs -i bash -c " [[ -f {}/clean_completed ]] && echo {} " ))
    local       _arrdexp1=($( printf '%s\n' ${_arrdexp[@]} ${arrdexp_clndone[@]} | sort -V | uniq -u ))
    local arrdexp_arcdone=($( printf '%s\n' ${_arrdexp1[@]} | xargs -i bash -c " [[ -f {}/archive/$fcomp && -f {}/archive/$fdel ]] && echo {} " ))
    local arrdexp_arcunfn=($( printf '%s\n' ${_arrdexp1[@]} ${arrdexp_arcdone[@]} | sort -V | uniq -u ))
    local arrdexp_dhldmis=($( printf '%s\n' ${_arrdexp1[@]} | xargs -i bash -c " [[ -f {}/archive/note_gcmarch ]] && echo {} " ))

    #todo:  get all running/q'd jobs 
    local arrjobs=($( /u/scicon/tools/bin/qstat -u $USER -W fmt_Queue="-maxw 40" -W o=+Rank0 | sort -V -k4 -t' ' | tr -s '[:space:]' | cut -d' ' -f4 | cut -d'.' -f1 | sort -V | uniq | grep ens ))

    msg_wheader 

    #wmessage "Number of Exp with Archive/Delout Completed : $cnt_done"
    wmessage "User Inputs : $strmsg"
    wmessage "                      Number of Exp Cleaned : $( printf '%+5s\n' ${#arrdexp_clndone[@]} )"
    wmessage "         Exp with Missing YYYYMM in holding : $( printf '%+5s\n' ${#arrdexp_dhldmis[@]} )"
    wmessage "          Exp with Archive/Delout Completed : $( printf '%+5s\n' ${#arrdexp_arcdone[@]} )"
    wmessage "                                 Incomplete : $( printf '%+5s\n' ${#arrdexp_arcunfn[@]} )"
    wmessage "Arch Incomplete:"
    ahand_warr ${arrdexp_arcunfn[@]} 
    
    wmessage
    wmessage
    wmessage "+++++ Details +++++"
    wmessage

    #yyyymmdd/ensX   winner       total  mapl daily  diur month
    #wmessage "$( printf '%-15s %+6s %+5s %+5s %+5s %+5s %+5s %+5s\n' yyyymmdd/ensX winner "" total mapl daily diur month )"
    wmessage "$( printf "${_pfrmt}\n" yyyymmdd/ensX winner "" total mapl daily diur month mom )"
    wmessage "$( printf -- '-%.0s' $( seq 1 $_head1 ))"

    for x in {1..3};do
        local _thisarrdexp=()

        if (( $x == 1 && ${#arrdexp_arcunfn[@]} > 0 ));then
            local _thisarrdexp=( ${arrdexp_arcunfn[@]} ) 

        elif (( $x == 2 && ${#arrdexp_arcdone[@]} > 0 ));then
            local _thisarrdexp=( ${arrdexp_arcdone[@]} )

        elif (( $x == 3 && ${#arrdexp_clndone[@]} > 0));then
            local _thisarrdexp=( ${arrdexp_clndone[@]} ) 

        else 
            continue
        fi

        for dexp in ${_thisarrdexp[@]};do
            local farchcomp=$dexp/archive/$fcomp
            local farchdel=$dexp/archive/$fdel
            local farchdelout=$dexp/archive/$fdelout
            local farchnote=$dexp/archive/note_gcmarch
            local blcomp=false
            local bldelo=false

            set_rstfcstdate $dexp
            count_files $dexp

            local blwin=$( exp_checkwinner $dexp $dwin )
            [[ -f $dexp/gcm_run.j ]] && local blrerun=$( grep -i "RERUN" $dexp/gcm_run.j 2>/dev/null | head -1 | rev | cut -d' ' -f1 | rev | tr '[:upper:]' '[:lower:]'  ) \
                                     || local blrerun=false

            $blwin || $blrerun && local _blthis=true || local _blthis=false 

            local arrcalc=($( exp_calcoutput $dexp $_blthis $dexp/HISTORY_2.rc ))

            if $blleaveout;then
                local arrcalcsave=($( exp_calcoutput_save $dexp $blwin $dexp/HISTORY_2.rc ))
            else
                local arrcalcsave=( 0 0 0 0 0 )
            fi

            local cntfsftc=$( find $dexp/archive/* -type f -name 'gcmarch_shiftin_*' 2>/dev/null | wc -l )
            [[ -f $farchdelout ]] && local numfpfe_del_cnt=$( cat $farchdelout | grep -vE "${strmom_search}|${strrst}" 2>/dev/null | sort -V | uniq | wc -l ) || local numfpfe_del_cnt=0
            local numfpfe_mis_cnt=$( echo "${arrcalcsave[0]} - $numfpfe_total_cnt - $numfpfe_del_cnt" | bc )
            local numflfe_mis_cnt=$( echo "${arrcalc[0]} - $numflfe_total_cnt" | bc )
            local numfpst=$( printf '%s\n' ${arrjobs[@]} | grep P${fcstdate}${ensm}. 2>/dev/null | wc -l )
            [[ -f $farchnote ]] && local numdhold_miss=$( cat $farchnote | wc -l ) || local numdhold_miss=0

            (( $numfpfe_mis_cnt == 0 && $numflfe_mis_cnt > 0 )) && arrdexp_pfeoutallexist+=( $dexp )
            (( $numfpfe_mis_cnt == $numflfe_mis_cnt          )) && arrdexp_missoutsame+=( $dexp )
            (( $numfpfe_mom_cnt <  $numflfe_mom_cnt          )) && arrdexp_pfemommiss+=( $dexp )
            #(( $numdhold_miss   >  0                         )) && arrdexp_dholdmiss+=( $dexp )

            $blwin && local strwin="Winner" || local strwin=" "

            if (( $x == 1 || $x == 2 ));then
                wmessage "$( printf "${_pfrmt}\n" $( echo $dexp | rev | cut -d'/' -f1-2 | rev ) "$strwin" "calc" ${arrcalc[@]} "-" )"
                wmessage "$( printf "${_pfrmt}\n" "" ""    "save" ${arrcalcsave[@]} "0" )"
                wmessage "$( printf "${_pfrmt}\n" "" "" "pfe cnt" $numfpfe_total_cnt $numfpfe_monmapl_cnt $numfpfe_day_cnt $numfpfe_diu_cnt $numfpfe_monpost_cnt $numfpfe_mom_cnt )"

            elif (( $x == 3 ));then
                wmessage "$( printf "${_pfrmt}\n" $( echo $dexp | rev | cut -d'/' -f1-2 | rev ) "$strwin" "pfe cnt" $numfpfe_total_cnt $numfpfe_monmapl_cnt $numfpfe_day_cnt $numfpfe_diu_cnt $numfpfe_monpost_cnt $numfpfe_mom_cnt )"
            fi

            wmessage "$( printf "${_pfrmt}\n" "" "" "$hstarc cnt" $numflfe_total_cnt $numflfe_monmapl_cnt $numflfe_day_cnt $numflfe_diu_cnt $numflfe_monpost_cnt $numflfe_mom_cnt )"

            if (( $x == 1 || $x == 2 ));then

                #+++++ cd dexp/archive (start) +++++
                cd $dexp/archive
                [[ -f $fcomp ]]        && wmessage $( stat --print="%.19y %n" $fcomp ) && local blcomp=true 
                [[ -f $fdel  ]]        && wmessage $( stat --print="%.19y %n" $fdel )  && local bldelo=true 
                cd - >/dev/null
                #+++++ cd dexp/archive ( end ) +++++

                (( $cntfsftc        > 0 )) && wmessage "$( printf "%+${_foot1}s %+4s\n" "Remaining Shiftc Files -" $cntfsftc )"
                (( $numfpfe_mis_cnt > 0 )) && ! $blcomp && ! $bldelo && \
                                              wmessage "$( printf "%+${_foot1}s %+4s\n" "  Missing Files on Pfe -" $numfpfe_mis_cnt )" 
                (( $numflfe_mis_cnt > 0 )) && wmessage "$( printf "%+${_foot1}s %+4s\n" "  Missing Files on Lfe -" $numflfe_mis_cnt )" 
                (( $numfpst         > 0 )) && wmessage "$( printf "%+${_foot1}s %+4s\n" "Running/Queue gcm_post -" $numfpst         )" 
                (( $numdhold_miss   > 0 )) && wmessage "$( printf "%+${_foot1}s %+4s\n" "    Missing YYYYMM dir -" $numdhold_miss   )" 
                $blcomp && $bldelo && (( $cntfsftc == 0 && $numflfe_mis_cnt == 0 )) && \
                                              wmessage "$( printf "%+${_foot2}s\n" "Archive/Delout - DONE Yay!")"
            elif (( $x == 3 ));then
                [[ -f $dexp/$fmark_clean ]] && wmessage "$( printf "%+${_foot3}s\n"      "********* Cleaning Completed *********")"
            fi

            wmessage
            wmessage "$( printf -- '-%.0s' $( seq 1 $_head1 ))"
        done 
    
        wmessage "$( printf -- '-%.0s' $( seq 1 $_head1 ))"
    done

    wmessage
    wmessage
    if (( ${#arrdexp_pfeoutallexist[@]} > 0 ));then
        wmessage "Number of Exp with All Output Exists but Archive/Delout is not completed : $( printf '%+5s\n' ${#arrdexp_pfeoutallexist[@]}    )"
        ahand_warr ${arrdexp_pfeoutallexist[@]} 
        wmessage 
    fi
    if (( ${#arrdexp_missoutsame[@]} > 0 ));then
        wmessage "# of Output on PFE = # of Outputs on LFE : $( printf '%+5s\n' ${#arrdexp_missoutsame[@]}    )"
        ahand_warr ${arrdexp_missoutsame[@]} 
        wmessage 
    fi
    if (( ${#arrdexp_pfemommiss[@]} > 0 ));then
        local arrdexp_mommiss=($( printf '%s\n' ${arrdexp_pfemommiss[@]} ${arrdexp_arcdone[@]} | sort -V | uniq -u ))

        wmessage "Archiving Incomplete && (( # of MOM Output on PFE < # of MOM Outputs on LFE )): $( printf '%+5s\n' ${#arrdexp_mommiss[@]}    )"
        ahand_warr ${arrdexp_mommiss[@]} 
        wmessage 
    fi

    return 
}
   
exp_mkcollgcmpost(){
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

    [[ -f $_fpst ]] && return 0 || return 1
}

write_allquota(){
    #todo:  write quota 
    local _nbp=$( echo $DFCST | cut -d'/' -f1-3 ) 
    wmessage
    msg_wheader_userdefined 60 "-" "Quota (as of $( date +%Y%m%d ))" 
    wmessage 
    if (( $writetofile == 1 ));then
        wmessage "$( quota -s )"
        wmessage
        wmessage "$( lfs quota -h -u $USER /nobackupp11/$USER )" 
        wmessage
        wmessage "$( lfs quota -h -u $USER /nobackupp13/$USER )"
        wmessage
        wmessage "$( lfs quota -h -u $USER /nobackupp28/$USER )"
    else 
        quota -s 
        wmessage
        lfs quota -h -u $USER /nobackupp11/$USER 
        wmessage
        lfs quota -h -u $USER /nobackupp13/$USER
        wmessage
        lfs quota -h -u $USER /nobackupp28/$USER
    fi
    wmessage 
    return
}

write_quota_fcst(){
    #todo:  write quota 
    local _nbp=$( echo $DFCST | cut -d'/' -f1-3 ) 
    wmessage
    msg_wheader_userdefined 60 "-" "Quota for $( dirname $_nbp ) (as of $( date +%Y%m%d ))" 
    wmessage 
    if (( $writetofile == 1 ));then
        wmessage "$( lfs quota -h -u $USER $_nbp )" 
    else 
        lfs quota -h -u $USER $_nbp
    fi
    wmessage 
    return
}

exp_sherlockout(){
    #description:   check if all outputs exist in holding/collection/YYYYMM directories
    local _fsftin=$1;shift
    local _arrpst=( "$@" )
    local _arrresult=()
    local fpst

    #note:
    # E - all output exists in holding/coll/yyyymm
    #LT - legacy tar file exist
    #EO - all outputs exists outside of holding/coll/yyyymm
    #MA - all outputs are missing
    #MS - some outputs are missing
    #UN - output status is unknown

    for fpst in ${_arrpst[@]};do
               
        local thisind=$( misc_indexof $fpst ${_arrpst[@]} )
        local pstyyyymm=$( echo $fpst | rev | cut -c1-6 | rev )
        local pstcoll=$( echo $fpst | cut -d'.' -f2 )
        local pstfreq=$( echo $pstcoll | cut -d'_' -f3 )
        local pstnumdays=$( fcal_numdaysinmonth $pstyyyymm ) 
    
        [[ "$pstfreq" == *"dy"* ]] && local numout_calc=$pstnumdays
        [[ "$pstfreq" == *"hr"* ]] && local numout_calc=$( echo "$pstnumdays * 24 / ${pstfreq/hr*}" | bc )
    
        #todo:  check if legacy tar files exist
        #19821202.ocn_inst_6hr_glo_L1440x721_z50.198301.nc4.tar
        local arrftar=($( grep $fcstdate.$pstcoll.$pstyyyymm.nc4.tar $fexistpfe | cut -d' ' -f3 ))
        (( ${#arrftar[@]} == 0 )) && arrftar=($( grep $fcstdate.$pstcoll.$pstyyyymm.nc4.tar $fexistlfe | cut -d' ' -f3 ))
        
        if (( ${#arrftar[@]} == 1 ));then
            #wmessage "$( printf "%+3s of %+3s\n" $thisind ${#_arrpst[@]} ) $pstcoll $pstyyyymm - Legacy tar exist"
               
            _arrresult+=( $fpst.LT ) 

            [[ ! -d $dhld/$pstcoll/$pstyyyymm ]] && mkdir -p $dhld/$pstcoll/$pstyyyymm
            thisftar=${arrftar[0]} 
            echo "$hstarc:$thisftar $dhld/$pstcoll/$pstyyyymm" >> $_fsftin
        fi

        #todo:  check if all outputs are in holding/yyyymm
        if [[ -d $dhld/$pstcoll/$pstyyyymm ]];then 
            #19821202.atm_inst_6hr_glo_L720x361_p49.19830131_0300z.nc4
            local numoutput=$( find $dhld/$pstcoll/$pstyyyymm/* -type f -name "$fcstdate.$pstcoll.${pstyyyymm}??_*.nc4" 2>/dev/null | wc -l ) 
            local rem=$( echo "$numoutput % $pstnumdays" | bc )
    
            if  (( $numoutput == $numout_calc ));then
                #note:  all outputs EXIST
                 _arrresult+=( $fpst.E ) 
                 continue 
            elif (( $numoutput > 0 && $numoutput < $numout_calc ));then
                #note:  Some outputs are MISSING
               local numoutmiss=$( echo "${numout_calc} - ${numoutput}" | bc )
                _arrresult+=( $fpst.MS.$numoutmiss ) 
            
            elif (( $numoutput == 0 ));then
                #note:   All outputs MISSING
                _arrresult+=( $fpst.MA ) 
            fi
    
        else
            local arroutput=($( find $dexp/* -type f -name "$fcstdate.$pstcoll.${pstyyyymm}??_*z.nc4" 2>/dev/null ))

            if (( ${#arroutput[@]} == $numout_calc ));then
               #note:   All outputs exist outside of holding dir"
               _arrresult+=( $fpst.EO ) 

            elif (( ${#arroutput[@]} == 0 ));then
               #note:   All outputs MISSING"
               _arrresult+=( $fpst.MA ) 

            elif (( ${#arroutput[@]} > 0 && ${#arroutput[@]} < $numout_calc ));then
               #note:  some outputs are  MISSING"
               local numoutmiss=$( echo "${numout_calc} - ${#arroutput[@]}" | bc )
               _arrresult+=( $fpst.MS.$numoutmiss ) 

            else
               _arrresult+=( $fpst.UN ) 
            fi
        fi
    done

    echo "${_arrresult[@]}"  
}

exp_sortbywinner(){
    #description:   prioritize dexarchive
    local _blwin=$1;shift
    local _arrdexp=( "$@" ) 
    local dout_pckwinners=$cdir/output/pckwinners/$strdout
    #winners_nino3.4_fcst_198112_198202.txt
    local strfwin=winners_${reg}_${strexpid}_
    local arrdwin=()
    local _arr=()
    local yyyymm

    local _arryyyymm=($( printf "%s\n" ${_arrdexp[@]} | rev | cut -d'/' -f2 | rev | cut -c1-6 | sort -V | uniq )) 

    for yyyymm in ${_arryyyymm[@]};do 
        arrdwin+=($( cat $dout_pckwinners/${strfwin}${yyyymm}_[0-9]*.txt | sort -V | sed "s# #/#g" | xargs -i echo $DFCST/{} ))
    done
  
    local arrdwin_arch=($( printf '%s\n' ${_arrdexp[@]} ${arrdwin[@]} | sort -V | uniq -d ))
    local arrd3mon_arch=($( printf '%s\n' ${_arrdexp[@]} ${arrdwin_arch[@]} | sort -V | uniq -u ))
   
    if $_blwin; then
        _arr=( ${arrdwin_arch[@]} ${arrd3mon_arch[@]} )
    else
        _arr=( ${arrd3mon_arch[@]} ${arrdwin_arch[@]} )
    fi
    echo ${_arr[@]}
}


exp_writetable(){
    local _dexp=$1;shift
    local _srcme=$1
    [[ -z $cdir ]] && local cdir=$( pwd )

    [[ -z $_dexp  ]] && die "(${FUNCNAME[0]}) _dexp empty"
    [[ -z $_srcme ]] && die "(${FUNCNAME[0]}) _srcme empty"

    [[ -z $strexpid ]] && source $_srcme

    set_rstfcstdate $_dexp

    local _darch=$DARCH/$fcstdate/$ensm
    local dwin=$cdir/output/pckwinners/$strdout
    local _fcapr=$_dexp/cap_restart
    local _fcapric=$_dexp/cap_restartIC
    local flst_lfe=$cdir/${strscr}_${strexpid}_$fcstdate${ensm}_lfefiles
    local strmapl="01_0000z.nc4"

    [[ -f $_fcapric ]] && local capric_yyyy=$( cat $_fcapric | head -1 | tr -s '[:space:]' | cut -d' ' -f1 | cut -c1-4)
    [[ -f $_fcapr   ]] && local capr_yyyymmdd=$( cat $_fcapr | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
    [[ -d $dwin     ]] && local blwin=$( exp_checkwinner $_dexp $dwin ) || local blwin=false
    [[ -f $_dexp/gcm_run.j ]] && local blrerun=$( grep -i "RERUN" $_dexp/gcm_run.j 2>/dev/null | head -1 | rev | cut -d' ' -f1 | rev | tr '[:upper:]' '[:lower:]'  ) \
                              || local blrerun=false
        
    local numfmom_calc=$( calc_numseg $fcstdate $capr_yyyymmdd )
    
    $blwin || $blrerun && local _blthis=true || local _blthis=false 
    local arrcalc=($( exp_calcoutput $_dexp $_blthis $_dexp/HISTORY_2.rc ))

    if $blleaveout;then
        local arrcalcsave=($( exp_calcoutput_save $_dexp $blwin $_dexp/HISTORY_2.rc ) )
    else
        local arrcalcsave=( 0 0 0 0 0 )
    fi
    
    local   numftotal_calc=${arrcalc[0]} 
    local numfmonmapl_calc=${arrcalc[1]}    
    local     numfday_calc=${arrcalc[2]}        
    local     numfdiu_calc=${arrcalc[3]}        
    local numfmonpost_calc=${arrcalc[4]}    

    local numfsave_tot=${arrcalcsave[0]}
    local numfsave_mpl=${arrcalcsave[1]}
    local numfsave_day=${arrcalcsave[2]}
    local numfsave_diu=${arrcalcsave[3]}
    local numfsave_mon=${arrcalcsave[4]}
    local numfsave_rst=0
    local numfsave_mom=0

    local numfmiss=$( echo "$numftotal_calc - $numfsave_tot" | bc )

    writeheader $_dexp $_darch
    
    getlfefout $_dexp $_darch $flst_lfe 
    count_files $_dexp $flst_lfe
    write_table $_dexp $numfmiss 
    write_table_collection $_dexp ${arrfsave[@]}
    return
}


createnote_ens11to15(){
    local _cdate=$( date +'%m/%d/%Y %H:%M' )
    local _ensm_from=$1;shift
    local _ensm_to=$1;shift
    local _thistar=$1

    note_ocnpert_ens11t15="${_cdate}
During a process of making ocean perturbations, values from rand_10 were incorrectly used. 
Values that were supposed to be used for $_ensm_to were used for ${_ensm_from}. This 
directory was renamed from ${_ensm_from} to ${_ensm_to}.

Perturbation for ens6 to ens10 were re-processed using the original rand_10 file, and saved 
as $_thistar on $hstarc. 

This processes started on 06/18/2022, and timestamps should be newer than that for files 
that created after finding the errors.
"
    return
}

createnote_ens6to10(){
    local _cdate=$( date +'%m/%d/%Y %H:%M' )
    local _thisensm=$1;shift
    local _thistar=$1

    note_ocnpert_ens6t10="${_cdate}
During a process of making ocean perturbations, values from rand_10 were incorrectly used. 
Values that were supposed to be used for $_thisensm were never used. Therefore, ocean perturbations
for ens6 to ens10 were processed separetely using the original rand_10 file, and saved as 
$_thistar on $hstarc. 

This processes started on 06/18/2022, and timestamps should be newer than that for files
that created after finding the errors.
"
    return
}

ahand_print(){
    
    #description:   print # of items in a given array and list items
    local _arrprint=( "$@" )
    local _numtotal=${#_arrprint[@]} 

    ahand_warr ${_arrprint[@]} 
    wmessage "Total = ${#_arrprint[@]}"
    wmessage
    return
}

write_numcleaned_dexp(){
    local yyyy 

    [[ -z $DFCST ]] && wmessage "DFCST is undefined" && return

    local arryyyy=($( find $DFCST/* -maxdepth 0 -type d -name "[1-2]???????" | rev | cut -d'/' -f1 | rev | cut -c1-4 | uniq | sort -V ))

    msg_wheader_userdefined 60 "-" "Number of Cleaned Exp (as of $( date +%Y%m%d ))" 
    wmessage "$( printf '%-5s %+7s %+7s %+11s %+5s\n' "YYYY" "Cleaned" "Arcdone" " the Rest " "Total" )"
    wmessage "$( printf -- '=%.0s' $(seq 1 39 ))"
    
    for yyyy in ${arryyyy[@]};do
        local _numdexp=$( find $DFCST/* -maxdepth 1 -mindepth 1 -type d -name "ens*" | grep $yyyy | wc -l )
        local _numdexp_cleaned=($( find $DFCST/* -maxdepth 1 -mindepth 1 -type d -name "ens*" | grep $yyyy | xargs -i bash -c "[[   -f {}/clean_completed ]] && realpath {}"| sort -V | wc -l ))
        local _numdexp_arcdone=($( find $DFCST/* -maxdepth 1 -mindepth 1 -type d -name "ens*" | grep $yyyy | xargs -i bash -c "[[ ! -f {}/clean_completed && -f {}/archive/gcmarch_deloutcompleted ]] && realpath {}"| sort -V | wc -l ))
        local _numdexp_therest=($( find $DFCST/* -maxdepth 1 -mindepth 1 -type d -name "ens*" | grep $yyyy | xargs -i bash -c "[[ ! -f {}/clean_completed && ! -f {}/archive/gcmarch_deloutcompleted ]] && realpath {}"| sort -V | wc -l ))
        wmessage "$( printf '%-5s %+7s %+7s %+11s %+5s\n' $yyyy $_numdexp_cleaned $_numdexp_arcdone $_numdexp_therest $_numdexp )"
    done
    wmessage
    return
}

fcal_ocnicyyyymmdd(){
    #description:   creates an array with ocean perturbation IC dates with user input YYYY 
    local _yyyy=$1
    local _arrocnrstyyyymmdd=( 0130 0224 0326 0425 0530 0629 0729 0828 0927 1027 1126 1226 )
    local _arrocnicyyyymmdd=($( printf "${_yyyy}%s\n" ${_arrocnrstyyyymmdd[@]} | xargs -i bash -c "date -d "'"'" {} +1days"'"'" +%Y%m%d" ))

    echo "${_arrocnicyyyymmdd[@]}"
}

fcal_ocnpert(){
    #description: return true or false if dexp is ocean perturbation 
    local _dexp=$1
    set_rstfcstdate $_dexp
    local _icyyyy=$( echo $fcstdate | cut -c1-4 ) 
    local arrocnpertdate=($( fcal_ocnicyyyymmdd $_icyyyy ))

    printf '%s\n' ${arrocnpertdate[@]} | grep $fcstdate >/dev/null 2>&1 
    local _status_grep=$?

    (( $_status_grep == 0 )) && local _bl=true || local _bl=false

    echo $_bl
    
}

exp_checkfpst(){
    #description:   find out input gcm_post.*.j* has been failed.
    local _dexp=$1;shift
    local _fpst=$1
    local _fexistlfe=$_dexp/archive/gcmarch_arc

    [[ ! -f $_fexistlfe ]] && local _fexistlfe=$_dexp/archiving/gcmarch_lfe

    set_rstfcstdate $_dexp

    local _arrcolldiu=($( exp_getcolldiurnal $_dexp/HISTORY_2.rc ))

    #gcm_post.sfc_tavg_3hr_glo_L720x361_sfc.j199309
    local _coll=$( echo $_fpst | cut -d'.' -f2 )
    local _yyyymm=$( echo $_fpst | cut -d'.' -f3 | cut -c2- )

    printf '%s\n' ${_arrcolldiu[@]} | grep $_coll >>/dev/null 2>&1
    local _status_grep=$?
    
    (( $_status_grep == 0 )) && local _bldiu=true || local _bldiu=false
   
    #todo:  collect all missing outputs 
    local _arrfoutmiss_lfe=()
    local _arrfoutmiss=()

    #todo:  check on $hstarc
    if [[ -f $_fexistlfe ]];then 

        local fday=$_coll/$fcstdate.$_coll.daily.$_yyyymm.nc4.tar
        local fmon=$_coll/$fcstdate.$_coll.monthly.$_yyyymm.nc4
   $_bldiu && fdiu=$_coll/$fcstdate.$_coll.diurnal.$_yyyymm.nc4

        grep $fday $_fexistlfe >>/dev/null 2>&1
        local _status_grep=$?
        (( $_status_grep > 0 )) && _arrfoutmiss_lfe+=( $( basename $fday ) ) 

        grep $fmon $_fexistlfe >>/dev/null 2>&1
        local _status_grep=$?
        (( $_status_grep > 0 )) && _arrfoutmiss_lfe+=( $( basename $fmon ) ) 
       
        if $_bldiu;then 
            grep $fdiu $_fexistlfe >>/dev/null 2>&1
            local _status_grep=$?
            (( $_status_grep > 0 )) && _arrfoutmiss_lfe+=( $( basename $fdiu ) ) 
        fi

        #todo:  check on pfe
        if (( ${#_arrfoutmiss_lfe[@]} > 0 )) ;then 
    
            for fout in ${_arrfoutmiss_lfe[@]};do
                #19930725.sfc_tavg_3hr_glo_L720x361_sfc.daily.199308.nc4.tar
                local thisout=$_dexp/$_coll/$fout
                [[ ! -f $thisout ]] && _arrfoutmiss+=( $thisout ) 
            done 
        fi

    else
        #todo:   check on pfe

        #19930725.sfc_tavg_3hr_glo_L720x361_sfc.daily.199308.nc4.tar
        local fday=$_dexp/$_coll/$fcstdate.$_coll.daily.$_yyyymm.nc4.tar
        local fmon=$_dexp/$_coll/$fcstdate.$_coll.monthly.$_yyyymm.nc4
   $_bldiu && fdiu=$_dexp/$_coll/$fcstdate.$_coll.diurnal.$_yyyymm.nc4

        if $_bldiu;then 
            [[ ! -f $fday ]] && _arrfoutmiss+=( $fday )  
            [[ ! -f $fmon ]] && _arrfoutmiss+=( $fmon ) 
            [[ ! -f $fdiu ]] && _arrfoutmiss+=( $fdiu ) 
        
        else
            [[ ! -f $fday ]] && _arrfoutmiss+=( $fday )  
            [[ ! -f $fmon ]] && _arrfoutmiss+=( $fmon ) 
        fi
    fi

    echo ${_arrfoutmiss[@]}     
}

cmd_gjob_nas(){
    /u/scicon/tools/bin/qstat -u $USER -W 'fmt_Queue=-maxw 40' -W o=+Rank0
}

cmd_gjob_nas_jlong(){
    /u/scicon/tools/bin/qstat -u $USER -W fmt_Jobname='-maxw 60' -W fmt_Queue='-maxw 40' -W o=+Rank0 
}

cmd_gjob_nccs(){
    #export SQUEUE_FORMAT='%.10i %.9P %.16j %.8u %.8T %.10M %.9l %.6D %.5C'
    export SQUEUE_FORMAT='%.10i %.8u %.9P %.16j %.5C %.6D %.1T %.9M %.9L %15f'
    /usr/slurm/bin/squeue -a -u $USER 
}

exp_getdexp(){

    #description:   get queue'd or running dexp (full path) 
    #Usage: ${FUNCNAME[0]} [ status of job (R, Q; ignored if empty ) ] "
    local _str_status=$1

    local _hstshort=$( get_host )
    if [[ "$_hstshort" == "dis" ]];then
        local _str_wdir=WorkDir
        local cmd_gjob=cmd_gjob_nccs 
        #die "(${FUNCNAME[0]}) funtion not available for $_hstshort"

        if [[ -n $_str_status ]];then 
            local _arrjid1=($( $cmd_gjob | grep -w $_str_status | tr -s '[:space:]' | cut -d' ' -f1 ))
        else
            local _arrjid1=($( $cmd_gjob | grep $USER | tr -d '\t' | tr -s '[:space:]' | sed 's#^ *##' | cut -d' ' -f1 ))
        fi
        
        local _arr=($( printf '%s\n' ${_arrjid1[@]} | xargs -i bash -c "scontrol show job {} | grep $_str_wdir | tr -s '[:space:]' | cut -d'=' -f2" ))


    elif [[ "$_hstshort" == "pfe" ]];then
        local _strpbs_output=Output_Path
        local _strpbs_subarg=Submit_arguments
        local _fgrn=gcm_run.j
        local cmd_gjob=cmd_gjob_nas
    
        if [[ -n $_str_status ]];then 
            local _arrjid1=($( $cmd_gjob | grep -w $_str_status | tr -s '[:space:]' | cut -d' ' -f1 ))
        else
            local _arrjid1=($( $cmd_gjob | tr -s '[:space:]' | cut -d' ' -f1 ))
        fi
        
        local _arrjid=($( printf '%s\n' ${_arrjid1[@]} | xargs -i bash -c "x=\$( /u/scicon/tools/bin/qstat -f {} 2>/dev/null | grep $_strpbs_subarg 2>/dev/null | grep $_fgrn 2>/dev/null ); \
            [[ -n \$x ]] && echo {}" ))
        local _arr=($( printf '%s\n' ${_arrjid[@]} | xargs -i bash -c "/u/scicon/tools/bin/qstat -f {} 2>/dev/null | grep $_strpbs_output" | rev | cut -d':' -f1 | rev | xargs -i dirname {} ))

    fi

    echo ${_arr[@]} 

}

exp_getiddout(){
    
    #description:   get queue'd or running dexp (full path) 
    # Usage: ${FUNCNAME[0]} [ dexp ]
    #Output: array of exp id [0] & dout [1]

    local _dexp=$1
    local _str=GEOS_exp
    set_rstfcstdate $_dexp

    if [[ "$_dexp" == *"${_str}"* ]];then 
        local straf=${_dexp#*${_str}/}
        local strbf=${straf%/$fcstdate/$ensm}
        local numchar=$( echo "$strbf"  | grep -o "/" | wc -l ) 
        
        #note:   there should be 1 or 2 of '/' in strbf
        #        strbf should be ctag/strexpid OR ctag/strexpid/rstexpid 
        if (( $numchar == 1 ));then
            local strexpid=$( echo $strbf | cut -d'/' -f2 ) 
            local strdout=$strexpid
    
        elif (( $numchar == 2 ));then
            local strexpid=$( echo $strbf | cut -d'/' -f2- | sed 's#/##g' )
            local  strdout=$( echo $strbf | cut -d'/' -f2-3 )
        fi
    
    else
        local strexpid=fcst
        local strdout=$strexpid
    fi
   
    local _arr=( $strexpid $strdout )  
    echo "${_arr[@]}"
}

#================================================================================
#                         New Additions since 20230203
#================================================================================
#Note: Functions below have not been included in bf_s2sv3 scripts.

s2sv3_nrtdates(){
    #description: this function to figure out forecast dates based on today's dates.
    #             Output dates are all forecast dates from the last month and ones from
    #             the current month up to today.
    #             When user enter YYYYMMDD, ${FUNCNAME[0]} use it instead of today's 
    #             date
    #
    #input: ${FUNCNAME[@]} [(optional) YYYYMMDD]

    local _yyyymmdd=$1
    local _status=999

    if [[ -n $_yyyymmdd ]];then 

        local _blint=$( misc_isinteger $_yyyymmdd ) 

        (( $_blint > 0 )) && die "not integer: $_yyyymmdd" && return $_status
        (( ${#_yyyymmdd} != 8 )) && die "unknown format: $_yyyymmdd" && return $_status

        local cyyyymmdd=$_yyyymmdd
    else
        local cyyyymmdd=$( date +%Y%m%d ) 
    fi

    local cyyyymm=$( echo $cyyyymmdd | cut -c1-6 )  
    local   cyyyy=$( echo $cyyyymmdd | cut -c1-4 )  

    local pyyyy=$(( cyyyy - 1 )) 
    local pyyyymm=$( fcal_previousmonth $cyyyymm ) 

    #todo: figoure out all forecast dates from the last month and ones upto today's date
    #      in the current month.
    local arryyyymmdd1=($( fcal_calcfcdates $pyyyymm ))
          arryyyymmdd1+=($( fcal_calcfcdates $cyyyymm )) 
    local arryyyymmdd=($( printf '%s\n' ${arryyyymmdd1[@]} | xargs -i bash -c "(( {} <= $cyyyymmdd )) && echo {}" ))
    
    echo ${arryyyymmdd[@]} 
}

s2sv3_nrtdexp(){
    #description:   create dir path for near-real time based on given yyyymmdd

    local _arryyyymmdd=( "$@" ) 
    local _arryyyymmdd_ocn=()
    local _yyyy   

    [[ -z ${_arryyyymmdd[@]} ]] && die "var undefined: _arryyyymmdd" && exit

    local _arryyyy=($( printf "%s\n" ${_arryyyymmdd[@]} | cut -c1-4 | sort -V | uniq  )) 

    #todo:  figure out dates with 5 ensm and 10 ensm (ocn pert)
    for _yyyy in ${_arryyyy[@]};do
        _arryyyymmdd_ocn+=($( fcal_ocnicyyyymmdd $_yyyy ))
    done 

    local _arryyyymmdd_ens10=($( printf '%s\n' ${_arryyyymmdd[@]} ${_arryyyymmdd_ocn[@]}   | sort -V | uniq -d ))
    local _arryyyymmdd_ens05=($( printf '%s\n' ${_arryyyymmdd[@]} ${_arryyyymmdd_ens10[@]} | sort -V | uniq -u ))

    #tood:  get dexp path with appropriate ensm
    local _arrdexp=($( printf '%s\n' ${_arryyyymmdd_ens05[@]} | xargs -i printf "$DFCST/{}/%s\n" ${arrens4[@]}  ))
    _arrdexp+=($( printf '%s\n' ${_arryyyymmdd_ens10[@]} | xargs -i printf "$DFCST/{}/%s\n" ${arrens4[@]}  ))
    _arrdexp+=($( printf '%s\n' ${_arryyyymmdd_ens10[@]} | xargs -i printf "$DFCST/{}/%s\n" ${arrens10[@]} ))
    _arrdexp=($( printf '%s\n' ${_arrdexp[@]} | sort -V ))

    echo ${_arrdexp[@]} 

}

exp_checknumrst(){

    local _usage="a function to:
    count restarts files and return boolean. True for all rst exist; 
    false for not.
    
     Usage:  ${FUNCNAME[0]} [ restart dir ]
    Output:  define global variable (arrays) 
      Note:  WARNING - THIS FUNCTION DEFINES GLOBAL VARIABLES
"    

    local _dens=$1;shift

    [[ ! -d $_dens ]] && die "dir does not exist: $_dens"

    local blatmfexist=false
    local blocnfexist=false
   
    local _ensm=$( echo $_dens | rev | cut -d'/' -f1 | rev )  
    
    local _ind_atmensm=$( misc_indexof $_ensm ${arrens4[@]} )
    local _ind_ocnensm=$( misc_indexof $_ensm ${arrens10[@]} )

    #todo:  determine type of perturbation (or control)
    [[ "$enscont" == "$_ensm"  ]] && local _blcont=true || local _blcont=false 
    (( $_ind_atmensm > 0 )) && local _blatm=true  || local _blatm=false
    (( $_ind_ocnensm > 0 )) && local _blocn=true  || local _blocn=false


    if $_blcont && $_blatm;then
        local numfatm=22
        local numfocn=11
        local numfatm_sl=0
        local numfocn_sl=0
    
        local numfatm=$( find $_dens/* -type f -name "*_rst" 2>/dev/null | wc -l )
        local numfocn=$( find $_dens/RESTART/* -type f -name "*.nc" 2>/dev/null | wc -l )
    
        (( $numfatm == 22 )) && blatmfexist=true || blatmfexist=false
        (( $numfocn == 11 )) && blocnfexist=true || blocnfexist=false

    elif ! $_blcont && $_blatm;then
        local numfatm=0
        local numfocn="-"
        local numfatm_sl=0
        local numfocn_sl=0

        local numfatm=$( find $_dens/* -type f -name "*_rst" 2>/dev/null | wc -l )
        local numfatm_sl=$( find $_dens/* -type l -name "*_rst" 2>/dev/null | wc -l )
        local numfocn_sl=$( find $_dens/RESTART/* -type l -name "*.nc" 2>/dev/null | wc -l )

        (( $numfatm    == 2  )) && (( $numfatm_sl == 20 )) && blatmfexist=true || blatmfexist=false
        (( $numfocn_sl == 11 )) && blocnfexist=true || blocnfexist=false

    elif $_blocn;then
        local numfatm="-"
        local numfocn=0
        local numfatm_sl=0
        local numfocn_sl=0
        local blatmfexist=false
        local blocnfexist=false
        
        local numfocn=$( find $_dens/RESTART/* -type f -name "ocean_*" 2>/dev/null | wc -l )
        local numfatm_sl=$( find $_dens/* -type l -name "*_rst"  2>/dev/null| wc -l )
        local numfocn_sl=$( find $_dens/RESTART/* -type l -name "*.nc" 2>/dev/null | wc -l )

        (( $numfocn    == 2 && $numfocn_sl == 9 )) && blocnfexist=true || blocnfexist=false
        (( $numfatm_sl == 22 )) && blatmfexist=true || blatmfexist=false
    fi
   
    $blatmfexist && $blocnfexist && local _blready=true || local _blready=false

    echo $_blready
}

exp_pststatus(){

    local _arrdexp=( "$@" ) 

    local _ddata_pckwinners=$cdir/output/pckwinners/$strdout
    local _farc_stderr=archive/stderr_gcmarch
    local _farc_del=archive/gcmarch_deloutcompleted
    local _ftmp=tmp_${strscr}_${FUNCNAME[0]}_${strexpid}

    local _qstat=/u/scicon/tools/bin/qstat
    
    local _strspace1=0
    local _strspace2=0
    local _strspace3=0
    local _strspace4=0
    local _strspace5=0

    local _arrfpstcomp=()
    local _arrdexp_3mo_rerun=() 
    local _dexp _fpst
   
    [[ -f $_ftmp     ]] && rm -f $_ftmp
    [[ -f $_ftmp.bak ]] && rm -f $_ftmp.bak

    note_pststatus

    local _strhead1="Total=${#_arrdexp[@]}"
    local _strhead2="YYYYMMDD/ensM"
    local _strhead3=Error
    local _strhead4="Winner?"
    local _strhead5=Notes
               
    #todo:  figure out max #char for each string. Use these values
    #       for header and column divider
    (( $_strspace1 < ${#_strhead1} )) && _strspace1=${#_strhead1}
    (( $_strspace2 < ${#_strhead2} )) && _strspace2=${#_strhead2}
    (( $_strspace3 < ${#_strhead3} )) && _strspace3=${#_strhead3}
    (( $_strspace4 < ${#_strhead4} )) && _strspace4=${#_strhead4}
    (( $_strspace5 < ${#_strhead5} )) && _strspace5=${#_strhead5}

    local cnt=0
    for _dexp in ${_arrdexp[@]};do   
    
        cnt=$(( cnt + 1 ))
    
        set_rstfcstdate $_dexp 
    
        local _dexp_short=$fcstdate/$ensm
        local _fstderr=$_dexp/$_farc_stderr
    
        [[ ! -f $_fstderr ]] && continue

        local _blwin=$( exp_checkwinner $_dexp $_ddata_pckwinners )
            
        local _arrfpst_sub=($( grep gcm_post. $_fstderr 2>/dev/null | grep _glo_ 2>/dev/null | grep -v Make | tr -s '[:space:]' | rev | cut -d' ' -f1 | rev | sed 's#^ *##g' | sort -V ))
        local _arrfpst=($( printf '%s\n' ${_arrfpst_sub[@]} | sort -V | uniq ))
    
        for _fpst in ${_arrfpst[@]};do 
    
            local _status=$( pststatus $_dexp/post/$_fpst ) 
            local _blpst_noissue=$( echo $_status | cut -d':' -f1 ) 
            local _strcode=$( echo $_status | cut -d':' -f2 ) 
            local _strlast=$( echo $_status | cut -d':' -f3 ) 

            if ! $_blpst_noissue;then 
                #todo:  write results in a temp file
                local _str1="$cnt of ${#_arrdexp[@]}"
                local _str2="$fcstdate/$ensm"
                local _str3="$_strcode"
                local _str4=$_blwin
                local _str5=$_strlast

                echo "$_str1;$_str2;$_str3;$_str4;$_str5" >> $_ftmp

                #todo:  figure out max #char for each string. Use these values
                #       for header and column divider
                (( $_strspace1 < ${#_str1} )) && _strspace1=${#_str1}
                (( $_strspace2 < ${#_str2} )) && _strspace2=${#_str2}
                (( $_strspace3 < ${#_str3} )) && _strspace3=${#_str3}
                (( $_strspace4 < ${#_str4} )) && _strspace4=${#_str4}
                (( $_strspace5 < ${#_str5} )) && _strspace5=${#_str5}
            fi 

        done
            
        if $_blpst_noissue;then 
            _arrfpstcomp+=( $_dexp ) 
        elif ! $_blpst_noissue && ! $_blwin;then 
            _arrdexp_3mo_rerun+=( $_dexp ) 
        fi 
    
    done
    
    [[ -e $_ftmp ]] && local _fout_size=$( stat --print='%s' $_ftmp ) || local _fout_size=0

    #todo:  write the rest only if there are gcm_post files with errors.
    if (( $_fout_size > 0 ));then
        #todo:  insert header-column divider in the 2nd line of temp file
        local _strheader_col="$_strhead1;$_strhead2;$_strhead3;$_strhead4;$_strhead5"
        local _strtitle_div="$( printf -- '-%.0s' $( seq 1 $_strspace1 ));$( printf -- '-%.0s' $( seq 1 $_strspace2 ));$( printf -- '-%.0s' $( seq 1 $_strspace3 ));$( printf -- '-%.0s' $( seq 1 $_strspace4 ));$( printf -- '-%.0s' $( seq 1 $_strspace5 ))"
        sed -i.bak "1i $_strtitle_div" $_ftmp 
        sed -i.bak "1i $_strheader_col" $_ftmp 

        wmessage "$( cat $_ftmp | column -t -s';' )"

        if (( ${#_arrdexp_3mo_rerun[@]} > 0 ));then 
            wmessage
            wmessage "3month run for doover:"
            wmessage "$( printf '    %s\n' ${_arrdexp_3mo_rerun[@]} | sort -V | uniq )"
            wmessage
        fi
        
        if (( ${#_arrfpstcomp[@]} > 0 ));then 
            wmessage
            wmessage "No errors with gcm_post"
            wmessage "$( printf '    %s\n' ${_arrfpstcomp[@]} | sort -V | uniq )" 
            wmessage
        fi
    fi 

    [[ -f $_ftmp     ]] && rm -f $_ftmp
    [[ -f $_ftmp.bak ]] && rm -f $_ftmp.bak
   
    return 
} 

pststatus(){
    local _fpst_full=$1

    local _farc_delyyyymm=archive/gcmarch_delyyyymm
    local _numln_search1=100
    local _strsearch1="Warning!  Nan or Infinity"
    local _strsearch2="End of Month not yet reached."
    local _strsearch3="not found!"
    local _strsearch4="ignore_nan"
    
    local _strcode1="nan_inf"
    local _strcode2="unfinmo"
    local _strcode3a="miss_date"
    local _strcode3b="date_corrupted"
    local _strcode3c="unknown_date_issue"
    local _strcode4="miss_input"
    local _strcode5a="miss_yyyymm"
    local _strcode5b="deleted_yyyymm"
    local _strcode6="doover"
 
    local _fpst=$( basename $_fpst_full ) 
    local _dexp=$( echo $_fpst_full | rev | cut -d'/' -f3- | rev ) 

    [[ -z $fcstdate || -z $ensm ]] && set_rstfcstdate $_dexp

    local _blpst_noissue=true
    local _fpst_full_short=$( echo $_fpst_full | rev | cut -d'/' -f1-4 | rev )
    local _fpst_stdout=$_dexp/post/$( echo $_fpst | rev | sed 's#j#o#1' | rev )

    local   _coll=$( echo $_fpst | cut -d'.' -f2 )
    local _yyyymm=$( echo $_fpst | cut -d'.' -f3 | cut -c2- )
    
    local _arrfoutmiss=($( exp_checkfpst $_dexp $_fpst ))
    
    if (( ${#_arrfoutmiss[@]} > 0 ));then 
    
        local _dhold_yyyymm=$_dexp/holding/$_coll/$_yyyymm
        local _dhold_yyyymm_short=$fcstdate/$ensm/holding/$_coll/$_yyyymm

        if [[ -f $_fpst_stdout ]];then 
            cat $_fpst_stdout | tail -$_numln_search1 | grep -i "${_strsearch1}" 2>/dev/null 
            local _status_grep1=$?
    
            cat $_fpst_stdout | tail -$_numln_search1 | grep -i "${_strsearch2}" 2>/dev/null 
            local _status_grep2=$?
            
            cat $_fpst_stdout | tail -$_numln_search1 | grep -i "${_strsearch3}" 2>/dev/null 
            local _status_grep3=$?
    
        else
            local _status_grep1=999
            local _status_grep2=999
            local _status_grep3=999
        fi
   
        if [[ -f $_fpst_full ]]; then  
            cat $_fpst_full | grep -i "${_strsearch4}" >>/dev/null 2>&1
            local _status_grep4=$?
        else
            local _status_grep4=999
        fi

        if [[ ! -d $_dhold_yyyymm ]] ;then 
            grep $_dhold_yyyymm $_dexp/$_farc_delyyyymm 2>/dev/null  
            local _status_grep=$?
    
            if (( $_status_grep == 0 ));then 
                local _strcode=$_strcode5b
                local _strlast="run_gcmarch deleted $_dhold_yyyymm_short" 
            else
                local _strcode=$_strcode5a
                local _strlast=$_dhold_yyyymm_short

#wmessage \@$LINENO "_strlast = $_dhold_yyyymm_short" 

            fi
                
            local _blpst_noissue=false
    
        elif [[ -d $doover ]];then 
            local _strcode=$_strcode6
            local _strlast=$_dexp_short
            local _blpst_noissue=false
        else
            if (( $_status_grep4 > 0 ));then 
                #note:  -ignore_nan is missing
                local _strcode=$_strcode4
                local _strlast=$_fpst_full_short
                local _blpst_noissue=false
            fi
            if (( $_status_grep1 == 0 ));then 
                #note: nan/infinity issue
                local _strcode=$_strcode1
                local _strlast=$_fpst_full_short
                local _blpst_noissue=false
            fi
            if (( $_status_grep2 == 0 ));then 
                #note:  month is unfinished
                local _strcode=$_strcode2
                local _strlast=$_fpst_full_short
                local _blpst_noissue=false
            fi
            if (( $_status_grep3 == 0 ));then 
                #note:  a specific date is missing
                
                #tood:  figure out which output is missing when _status_grep3 == 0 ("not found!")
                local _numln=$( cat -n $_fpst_stdout | tail -$_numln_search1 | grep -B1 -i "${_strsearch3}" | sed 's#^ *##g' | cut -d' ' -f1 | head -1 ) 
                local _fmissout=$( sed "$_numln!d" $_fpst_stdout |sed "s#.*File: ##" | cut -d' ' -f1 )
                local _fmissout_full=$_dhold_yyyymm/$_fmissout

                #todo:  check if file is corrupted
                if [[ -f $_fmissout_full ]];then 
                    $ncdump -h $_fmissout_full >> /dev/null 2>&1
                    local _status_ncdump=$?
                    if (( $_status_ncdump != 0 )) ;then 
                        local _strcode3=$_strcode3b
                        local _strlast="$_fpst,$_fmissout is corrupted"
                    else
                        local _strcode3=$_strcode3c
                        local _strlast="$_fpst,$_fmissout exists but gcm_post says $_strsearch3"
                    fi
                else
                    local _strcode3=$_strcode3a
                    local _strlast="$_fpst,$_fmissout missing"
                fi 
    
                local _strcode=$_strcode3
                local _blpst_noissue=false
            fi
            if [[ -z $_blpst_noissue ]];then 
                local _strcode="unknown"
                local _strlast=$_fpst_full_short
                local _blpst_noissue=false
            fi
        fi
    fi
    
    if $_blpst_noissue;then 
        return
    else
        echo "$_blpst_noissue:${_strcode}:$_strlast" 
    fi

} 
     
note_pststatus(){

    local _strsearch1="Nan or Infinity"
    local _strsearch2="End of Month not yet reached."
    local _strsearch3="not found!"
    local _strsearch4="ignore_nan"
    
    local _strcode1="nan_inf"
    local _strcode2="unfinmo"
    local _strcode3a="miss_date"
    local _strcode3b="date_corrupted"
    local _strcode3c="unknown_date_issue"
    local _strcode4="miss_input"
    local _strcode5a="miss_yyyymm"
    local _strcode5b="deleted_yyyymm"
    local _strcode6="doover"

    local _note="\
gcm_post Status:
$( printf "%-18s %-20s\n" $_strcode1  "= $_strsearch1" )
$( printf "%-18s %-20s\n" $_strcode2  "= $_strsearch2" )
$( printf "%-18s %-20s\n" $_strcode3a "= Missing Date" )
$( printf "%-18s %-20s\n" $_strcode3b "= Corrupted Output File (Specific Date)" )
$( printf "%-18s %-20s\n" $_strcode3c "= Unknown Output File Issue (Specific Date)" )
$( printf "%-18s %-20s\n" $_strcode4  "= Missing Input " )
$( printf "%-18s %-20s\n" $_strcode5a "= Missing YYYYMM dir in holding" )
$( printf "%-18s %-20s\n" $_strcode5b "= YYYYMM in holding Deleted" )
$( printf "%-18s %-20s\n" $_strcode6  "= Re-run gcm_run" )
"
    wmessage "$_note"
}

exp_calccores_async(){
#    local _usage="a function to:
#    return an array with numbers of cores for each usage when async is used.
#    array[0]= NX
#    array[1]= NY
#    array[2]= Number of Cores Used for Each Node
#    array[3]= Number of Async Node (usually = 1)
#    array[4]= Total Number of Cores 
#    array[5]= Number of Cores for GEOSgcm.x
#    array[6]= Number of Cores for Async
#
#     Usage:  ${FUNCNAME[0]} [ srcme file | model name (rom_ait, sky_ele, or cas_ait)]
#    Output:  returns an array
#"
    local _userinput=$1

    if [[ -f $_userinput ]];then 
        local _srcf=$_userinput
        source $_srcf
    else
        local _mname=$_userinput
        local _numcore_model=$( getnumcore $_mname ) 
        local _isint=$( misc_isinteger $_numcore_model ) 
        
        (( $_isint > 0 )) && die "userinput input should be an existing srcme file or a model name (i.e. sky_ele)" 
    fi

    if [[ -n $_srcf && -n $ogcm_nx && -n $ogcm_ny && -n $numc ]];then 
        local _nx=$ogcm_nx
        local _ny=$ogcm_ny
        local _numc=$numc
         
    else
        #note:  Default values and these gives the best performance. 
        if [[ "$_mname" == "sky_ele" || "$_mname" == "cas_ait" ]];then
            local   _nx=80
            local   _ny=60
            local _numc=40

        elif [[ "$_mname" == "rom_ait" ]];then
            local   _nx=60
            local   _ny=60
            local _numc=125
        else
            die "user input model name should be sky_ele, cat_ait, or rom_ait"
        fi
    fi
       
    local _anode=1
    local  _npes=$( echo "$_nx * $_ny"     | bc ) 
    local  _numn=$( echo "$_npes / $_numc" | bc ) 
    local  _totc=$( echo "$_numn * $_numc" | bc )
    local _diffc=$( echo "$_totc - $_npes" | bc )

    while (( $_diffc < 0 ));do 
         _numn=$( echo "$_numn + 1"     | bc ) 
         _totc=$( echo "$_numn * $_numc" | bc ) 
        _diffc=$( echo "$_totc - $_npes" | bc )
    done 

    local _epes=$( echo "$_numc * $_anode + $_diffc" | bc ) 
    local _apes=$( echo "$_npes + $_epes" | bc ) 

    local _arrout=( $_nx $_ny $_numc $_anode $_apes $_npes $_epes ) 

    echo "${_arrout[@]}"     
}



convert_hhmmss2sec(){
    #description: calculate sec from hh:mm:ss
    local hhmmss=$1 #<-- input format has to be hh:mm:ss
    local hh=$( echo $hhmmss | cut -d':' -f1 )
    local mm=$( echo $hhmmss | cut -d':' -f2 )
    local ss=$( echo $hhmmss | cut -d':' -f3 )
    local nums=$( echo "$hh*3600+$mm*60+$ss" | bc -l )

    echo $nums
}

    
exp_status_mklst(){    
    local _flst=$1;shift
    local _arr=( "$@" )
    local dexp

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


exp_outarc2exp() {
    #description:   copy only outputs back from archive dir. These outputs are
    #               the ones should be saved in exp dir permanently.
    local _thisinput=$1;shift
    local _thissrcme=$1
    local _hstshort=$( get_host ) 
    local _thisscr=run_${FUNCNAME}.sh 
    local _dexp

    [[ -d $_thisinput ]] && local bldir=true  || local bldir=false
    [[ -f $_thisinput ]] && local blfile=true || local blfile=false

    #todo:  check user input
      $bldir &&   $blfile && die "user input is unrecognizable form"
    ! $bldir && ! $blfile && die "user input is unrecognizable form"

    if $bldir && ! $blfile;then
        local _arrdexp=( $_thisinput )
    elif ! $bldir && $blfile;then
        local _arrdexp=($( cat $_thisinput ))
    fi
     
    (( ${#_arrdexp[@]} == 0 )) && return

    if [[ "$_hstshort" == "dis" ]];then
        exp_outarc2exp_nccs ${_arrdexp[@]}  

    elif [[ "$_hstshort" == "pfe" ]];then
        exp_outarc2exp_nas ${_arrdexp[@]}  
    fi

    return
}

exp_outarc2exp_nccs(){
    local _arrdexp=( "$@" ) 
    local _thisscr=run_${FUNCNAME}.sh 
    local _dexp
        
    for _dexp in ${_arrdexp[@]};do

        #todo: copy script to dexp/archive
        local _darc=$_dexp/archive
        local _srcf_path=$( realpath $srcf ) 
        #+++++ cd to darch (start) +++++
        cd $_darc

        [[ -f $_fsed    ]] && rm -f $_fsed
        [[ -f $_thisscr ]] && rm -f $_thisscr

#cp -p $ddata/$_thisscr $cdir/
        cp -p $ddata/$_thisscr .

        set_rstfcstdate $_dexp

        local _fsed=sedfile_${strscr}_${FUNCNAME[0]}_$fcstdate$ensm
           
        [[ -f $_fsed ]] && rm -f $_fsed

        cat > $_fsed << EOF
s?@\<DFCST\>?${DFCST}?g
s?@\<FCSTDATE\>?${fcstdate}?g
s?@\<ENSEMBLE_MEMBER\>?${ensm}?g
s?@\<DARCH\>?${DARCH}?g
s?@FUNC_NAME?${FUNCNAME}?g
s?@STRSCR?${strscr}?g
s?@SCRFUNC?$cdir/func_fcst.sh?g
s?@SCRME?$_srcf_path?g
EOF
        
        sed -i -f $_fsed $_thisscr
        local status_sed=$?
    
        if (( $status_sed > 0 ));then 
            rm -f $_thisscr
            wmessage "sed failed"
            return 999
        else
            wmessage "$_thisscr is ready"
            wmessage "Go to $_dexp and submit $_thisscr"
        fi
    
        [[ -f $_fsed ]] && rm -f $_fsed
    
        cd - >/dev/null
        #+++++ cd to darch ( end ) +++++
         
    done
    return
}

exp_outarc2exp_nas() {
    local _arrdexp=( "$@" ) 
    local hstname=lfe    
    local _dexp
        
    local thiscmd="/usr/local/bin/shiftc --no-cron --no-mail -f -d"

    if ssh -q $hstname true >>/dev/null 2>&1; then
        :
    else
        return 999
    fi


    _arrdexp=($( printf '%s\n' ${_arrdexp[@]} | xargs -i bash -c "[[ -d {} ]] && echo {}" ))


    for _dexp in ${_arrdexp[@]};do 
        
        local   _fhis2=$_dexp/HISTORY_2.rc
        local _fcapric=$_dexp/cap_restartIC
        local   _fcapr=$_dexp/cap_restart
        local _fcapend=$_dexp/cap_end 

        set_rstfcstdate $_dexp
        
        local fin=tmp_${strscr}_${fcstdate}${ensm}_shiftin
        [[ -f $fin ]] && rm -f $fin

        local capric_yyyymmdd=$( cat $_fcapric | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
        local capric_yyyymm=$( echo $capric_yyyymmdd | cut -c1-6 )
        local capr_yyyymmdd=$( cat $_fcapr | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
        local realend_yyyymmdd=$( cat $_fcapend | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
        local realend_yyyymm=$( echo $realend_yyyymmdd | cut -c1-6 ) 
        
        #todo:  calculate the end date for three-month run.
        #note:  end date is 3 month from fcst date, not rst date.
        if [[ -n $fcstdate ]];then
            local nummonth1=3
            local strxmonth=$( nextXmonths $( date -d $fcstdate +%Y ) $( printf '%01g' $( date -d $fcstdate +%m ) ) 0 $(( nummonth1 + 1 ))  )
            local end_mm=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f1 )
            local end_year=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | cut -d':' -f2 )
            local end3_yyyymm=$end_year$end_mm
        fi
        
        local  bllastdayofthemonth=$( fcal_lastdayofthemonth  $capric_yyyymmdd )
        local blfirstdayofthemonth=$( fcal_firstdayofthemonth $capric_yyyymmdd )
        
        #todo:  calculate a number of *full* months that exp ran.
        local  arrmfull_3mo=($( numfullmonths $capric_yyyymm $end3_yyyymm    $bllastdayofthemonth ))
        local arrmfull_10mo=($( numfullmonths $capric_yyyymm $realend_yyyymm $bllastdayofthemonth ))
        local    arrmon_3mo=( $capric_yyyymm ${arrmfull_3mo[@]} )
        local   arrmon_10mo=( $capric_yyyymm ${arrmfull_10mo[@]} )

        local             arrcoll=($( exp_getcollections $_fhis2 ))
        #local     arrcollmon_mapl=($( exp_getcollfreq m $_fhis2 ))
        local      arrcollmonpost=($( exp_getcollfreq n  $_fhis2 ))
        local  arrcollmonpost_diu=($( exp_getcolldiurnal $_fhis2 ))
        local arrcollmonpost_save=($( printf '%s\n' ${arrcollmonpost[@]} | grep -E '_slv|_sfc' 2>/dev/null ))

        local arrcollmonpost_savemon=( ${arrcollmonpost[@]} )
        local arrcollmonpost_savediu=($( printf '%s\n' ${arrcollmonpost_diu[@]} | sort -V -k3 -t'_' | grep -E '_sfc|_slv' 2>/dev/null ))
        local arrcollmonpost_savedya=( ice_tavg_1dy_glo_T1440x1080_slv )
        local arrcollmonpost_savedy3=( sfc_tavg_3hr_glo_L720x361_sfc sfc_tavg_1hr_glo_L720x361_sfc )
        
        #todo:  get files to save
        (( $capr_yyyymmdd == $realend_yyyymmdd )) && _bl10morun=true ||  _bl10morun=false
        $_bl10morun && arrmonthfull=( ${arrmfull_10mo[@]} ) || arrmonthfull=( ${arrmfull_3mo[@]} )
        $_bl10morun && arrmonth=( ${arrmon_10mo[@]} )       || arrmonth=( ${arrmon_3mo[@]} )
        
        local arrfsave=()
        for coll in ${arrcoll[@]};do
            [[ "${arrcollmon_mapl[@]}" =~ "$coll" ]] &&       _blsavemapl=true || _blsavemapl=false
            [[ "${arrcollmonpost_savemon[@]}" =~ "$coll" ]] && _blsavemon=true || _blsavemon=false
            [[ "${arrcollmonpost_savediu[@]}" =~ "$coll" ]] && _blsavediu=true || _blsavediu=false
            [[ "${arrcollmonpost_savedya[@]}" =~ "$coll" ]] && _blsavedya=true || _blsavedya=false
            [[ "${arrcollmonpost_savedy3[@]}" =~ "$coll" ]] && _blsavedy3=true || _blsavedy3=false
            [[ "${arrcollmonpost_noreft[@]}"  =~ "$coll" ]] && _blnoreft=true  || _blnoreft=false

            arrfsave+=($( exp_createfname_save $_dexp $coll $blfirstdayofthemonth $bllastdayofthemonth $_blsavemapl $_blsavemon $_blsavediu $_blsavedya $_blsavedy3 $_blnoreft ${arrmonthfull[@]} ))
        done 

        #todo:  create output file names that are supposed to exists in dexp
        local arrfname=()
        for coll in ${arrcoll[@]};do
            [[ "${arrcollmonpost_noreft[@]}"  =~ "$coll" ]] && local _blnoreft=true  || local _blnoreft=false
            #arrfname+=($( exp_createfname $_dexp $coll $bllastdayofthemonth $blfirstdayofthemonth $_blnoreft ${arrmonthfull[@]} ))
            arrfname+=($( exp_createfname $_dexp $coll $bllastdayofthemonth $blfirstdayofthemonth $_blnoreft ${arrmonth[@]} ))
        done 

        if $blleaveout; then 
            local arrfout=( ${arrfsave[@]} )
        else
            local arrfout=( ${arrfname[@]} )
        fi

        
        local arrfmis=($( printf '%s\n' ${arrfout[@]} | xargs -i bash -c "[[ ! -f {} ]] && echo {}" ))
        local arrfarc=()
        for fmis in ${arrfmis[@]};do
            local fcollfname=$( echo $fmis | rev | cut -d'/' -f1-2 | rev )
            local fname_arc=$DARCH/$fcstdate/$ensm/$fcollfname

            #todo:  check if file exsit on archive
            ssh -q lfe test -s $fname_arc
            local _status_file=$?
            if (( $_status_file == 0 ));then  
                wtof $fin "$hostarc:$fname_arc $fmis" 
            fi
        done

        #todo:  run shiftc
        if [[ -s $fin ]];then 
            wmessage "$( $thiscmd < $fin )"
        else
            wmessage "No output files are available on archive"
        fi

    done

    return
}

calc_wait(){

    local cmin=$( date +%-M )
    local inc_min=$1
    local minc=60

    [[ -z $1   ]] && mintowait=0  
    (( $1 > 60 )) && mintowait=999


    if [[ -z $mintowait ]];then 
        #note:  60 has to be divisible by inc_min
        local num_inc=$( echo "$minc / $inc_min" | bc )
        local arrmins=($( printf '%s\n' $( seq $num_inc )  | xargs -i bash -c "echo "'"'"$inc_min * {}"'"'" | bc " ))

        local arrminnext=($( printf '%s\n' ${arrmins[@]} | xargs -i bash -c "(( $cmin < {} )) && echo {}"))
        local minnext=${arrminnext[0]#0}
        local cmin=$( date +%-M )
        local mintowait=$( echo "$minnext - $cmin" | bc ) 
    fi
    echo $mintowait
}
 
cnt_sftc(){
    local _pdir=$1;shift
    local sftid

    [[ -z $_hstshort ]] && local _hstshort=$( get_host ) 

    [[ "$_hstshort" == "dis" ]] && die "(${FUNCNAME[0]}) function does not work for $hostname"
    [[ -z $_pdir             ]] && die "(${FUNCNAME[0]}) var undefined: _pdir"

    local _arrsftid_all=($( /usr/local/bin/shiftc --status=csv | grep run  | tail -n +2 | cut -d',' -f1 ))
    local _cnt_sftid=0
    for sftid in ${_arrsftid_all[@]};do
        #local _sftid_arch=$( /usr/local/bin/shiftc --id=$sftid --status=csv 2>/dev/null | grep "run\|lou\|$_pdir" 2>/dev/null | head -1 | cut -d',' -f3 | cut -d':' -f2 )
        local _sftid_arch=$( /usr/local/bin/shiftc --id=$sftid --status=csv 2>/dev/null | grep run | grep lou | grep $_pdir 2>/dev/null | head -1 | cut -d',' -f3 | cut -d':' -f2 )
        [[ -z $_sftid_arch ]] && continue 

        _cnt_sftid=$(( _cnt_sftid + 1 ))
    done
    
    echo $_cnt_sftid
}

exp_arcfinishline(){
    #description:   get dexp/archive which is almost done with archiving. 
    local thres=$1;shift
    local _limexp=$1;shift
    local strmisfarc="Missing on LFE:"
    local strsid="Shift id is "
    local strsid2="Shiftc ID & Filename:"
    local _ferrarc=stderr_gcmarch

    if [[ -n $thres ]];then 
        local _blisint=$( misc_isinteger $thres )
        (( $_blisint > 0 )) && die "(${FUNCNAME[0]}) var not integer: thres"
    fi

    if [[ -n $_limexp ]];then 
        local _blisint=$( misc_isinteger $_limexp )
        (( $_blisint > 0 )) && die "(${FUNCNAME[0]}) var not integer: _limexp"
    fi

    [[ -z $DFCST ]] && die "var undefined: DFCST" 


    local arrdexp=($( find $DFCST/* -maxdepth 1 -mindepth 1 -type d -name "ens*" 2>/dev/null | \
                    xargs -i sh -c "[[ ! -f {}/clean_completed ]] && echo {}"  | \
                    xargs -i sh -c "[[   -d {}/archive         ]] && echo {}"  | \
                    xargs -i sh -c "[[ ! -f {}/archive/gcmarch_deloutcompleted ]] && echo {}" \
                    ))
    
#ahand_print ${arrdexp[@]} 
#exit
    
    #todo:  figure out dexp which missing file less than thres
     local arrdexp_neardone=($( printf '%s\n' ${arrdexp[@]}| \
                xargs -i        echo {}/archive/$_ferrarc | \
                xargs -i sh -c "grep -H -w "'"'"$strmisfarc"'"'" {} 2>/dev/null | tail -1 | awk '{ print \$1, \$5 }'" | \
                awk -v a=$thres '{ if ( $2 < a ) { print $1, $2 } }' | \
                cut -d':' -f1 | rev | cut -d'/' -f3- | rev | sort -V \
                ))

    local arrdexp=($( printf '%s\n'  ${arrdexp_neardone[@]} )) 

    #todo:  get shiftc id which just started
    local strsid_all1=$( printf '%s\n' ${arrdexp[@]} | \
                    xargs -i         echo {}/archive/$_ferrarc | \
                    xargs -i  sh -c "grep -H -n "'"'"exp location"'"'" {} 2>/dev/null | tail -1 | cut -d':' -f1-2 | sed 's#:# #g' " | \
                    xargs -n2 sh -c 'cat $1 2>/dev/null | tail -n +$2 2>/dev/null' sh | grep "$strsid" | \
                    rev | cut -d' ' -f1 | rev | \
                    tr "\n" "|" | rev | cut -c2- | rev \
                    )  
    
    
    #todo:  get shiftc id from running shiftc
    local strsid_all2=$( printf '%s\n' ${arrdexp[@]} | \
                    xargs -i        echo {}/archive/$_ferrarc                                  | \
                    xargs -i  sh -c "grep -H -n "'"'"$strsid2"'"'" {} 2>/dev/null | tail -1 | cut -d':' -f1-2"  | \
                    sed 's#:# #g'                                                                   | \
                    xargs -n2 sh -c 'cat $1 2>/dev/null | tail -n +$2 2>/dev/null | head -5' sh                             | \
                    grep gcmarch_shiftin_ | cut -d' ' -f1                                           | \
                    tr "\n" "|" | rev | cut -c2- | rev                                                \
                    )  
    

    #todo:  get only running shiftc id
    local strsid_running=$( shiftc --status=csv | grep -E "$strsid_all1|$strsid_all2" | grep run | cut -d',' -f1 | tr '\n' '|' | rev | cut -c2- | rev ) 
    
    local arrdexp_sftcrunning=($( printf '%s\n' ${arrdexp[@]} | \
                    xargs -i        echo {}/archive/$_ferrarc | \
                    xargs -i  sh -c "grep -l -E "'"'"$strsid_running"'"'" {} " | \
                    rev | cut -d'/' -f3- | rev ))
    
    #todo:  extract dexp that shiftc are completed
    #local arrdexp=($( printf '%s\n' ${arrdexp[@]} ${arrdexp_sftcrunning[@]} | sort -V | uniq -u )) 
    local arrdexp=($( printf '%s\n' ${arrdexp[@]} ${arrdexp_sftcrunning[@]} | sort -V | uniq -u | head -n $_limexp )) 

    echo "${arrdexp[@]}"
}

exp_arczero(){
    #description:   get dexp/archive which is almost done with archiving. 
    local _limexp=$1
    local _ferrarc=stderr_gcmarch
    

    if [[ -n $_limexp ]];then 
        local _blisint=$( misc_isinteger $_limexp )
        (( $_blisint > 0 )) && die "(${FUNCNAME[0]}) var not integer: _limexp"
    fi

    [[ -z $DFCST ]] && die "var undefined: DFCST" 


    local arrdexp=($( find $DFCST/* -maxdepth 1 -mindepth 1 -type d -name "ens*" 2>/dev/null | \
                    xargs -i sh -c "[[ ! -f {}/clean_completed ]] && echo {}"  | \
                    xargs -i sh -c "[[   -d {}/archive         ]] && echo {}"  | \
                    xargs -i sh -c "[[ ! -f {}/archive/gcmarch_deloutcompleted ]] && echo {}" \
                    ))

    #todo:  figure out dexp with no archive has started
    local arrdexp_zeroarc=($( printf '%s\n' ${arrdexp[@]} | \
                xargs -i bash -c "[[ -f {}/archive/$_ferrarc ]] && echo {}" | \
                xargs -i bash -c "stat --printf='%n %s\n' {}/archive/$_ferrarc " | \
                awk '{ print $1, $2 }' | awk -v a=0 '{ if ( $2 == a ) { print $1 } }' | \
                rev | cut -d'/' -f3- | rev | sort -V | head -n $_limexp 
                ))


#ahand_print ${arrdexp[@]} 
#exit
    echo "${arrdexp_zeroarc[@]}"
}

calc_numseg_mom(){
    local _begyyyymmdd=$1;shift
    local _endyyyymmdd=$1;shift
    local _seg=$1
    local _arryyyymm=()

    local _endyyyymm=${_endyyyymmdd:0:6}
    local yyyymm=${_begyyyymmdd:0:6}

    while (( $yyyymm < $_endyyyymm ));do
        _arryyyymm+=( $yyyymm )
        yyyymm=$( fcal_nextmonth $yyyymm )
    done 

    if (( ${#_arryyyymm[@]} < $_seg ));then 
        _numseg=$(( _numseg + 1 ))
    else

        local yyyymm=${_begyyyymmdd:0:6}
        local _numseg=0
        local _cnt=0    
        while (( $yyyymm < $_endyyyymm ));do
            _cnt=$(( _cnt + 1 ))
            local _bladded=false
            yyyymm2=$( fcal_nextmonth $yyyymm )
    
            local _yyyy1=${yyyymm:0:4}
            local _yyyy2=${yyyymm2:0:4}

            if (( $_yyyy1 < $_yyyy2 ));then 
                _numseg=$(( _numseg + 1 ))
                _bladded=true
                _cnt=0    
    
            elif (( $_cnt == $_seg ));then 
                _numseg=$(( _numseg + 1 ))
                _bladded=true
                _cnt=0    
            fi

            if ! $_bladded && (( $yyyymm2 == $_endyyyymm ));then 
                _numseg=$(( _numseg + 1 ))
                _bladded=true
            fi

            yyyymm=$yyyymm2
        done
    fi
            
    echo $_numseg
}



