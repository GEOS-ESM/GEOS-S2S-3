#!/usr/bin/env bash
#================================================================================
#                                   Batch Jobs
#================================================================================
#@BATCH_TIME@ARCHIVE_T
##@ARCHIVE_PRES
##@ARCHIVE_QRES
#@ARCHIVE_P
#@ARCHIVE_Q
#@BATCH_JOBNAMEA@FCSTDATE@ENSEMBLE_MEMBER
#@GEOSS2S3_OPT2
#@BATCH_GROUP
#@BATCH_OUTPUTNAMEstderr_gcmarch
#@BATCH_JOINOUTERR

setvars() {
    local _dexp=$1

    local    _fcap=$_dexp/CAP.rc
    local   _fcap0=$_dexp/CAP_0.rc
    local   _fcap1=$_dexp/CAP_1.rc
    local   _fcap2=$_dexp/CAP_2.rc
    local   _fcapr=$_dexp/cap_restart
    local _fcapric=$_dexp/cap_restartIC
    local _fcapend=$_dexp/cap_end
    local   _fhis1=$_dexp/HISTORY_1.rc
    local   _fhis2=$_dexp/HISTORY_2.rc
    local  strmapl="01_0000z.nc4"

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

    if [[ -f $_fcap0 ]];then 
        seg0=$( grep -iw $strseg $_fcap0 | tr -s '[:space:]' | cut -d' ' -f2 )
        seg0_y=$( echo $seg0 | cut -c1-4 | xargs -i bash -c " num=\$( echo \"{} * 1 \" | bc ); (( \$num == 0 )) && echo \"\" || echo \$num" )
        seg0_m=$( echo $seg0 | cut -c5-6 | xargs -i bash -c " num=\$( echo \"{} * 1 \" | bc ); (( \$num == 0 )) && echo \"\" || echo \$num" )
        seg0_d=$( echo $seg0 | cut -c7-8 | xargs -i bash -c " num=\$( echo \"{} * 1 \" | bc ); (( \$num == 0 )) && echo \"\" || echo \$num" )  
        cap0_endyyyymmdd=$( grep -i end_date $_fcap0 | tr -s '[:space:]' | cut -d' ' -f2 ) 
    fi 
    
    if [[ -f $_fcap1 ]];then 
        seg1=$( grep -iw $strseg $_fcap1 | tr -s '[:space:]' | cut -d' ' -f2 )
        seg1_y=$( echo $seg1 | cut -c1-4 | xargs -i bash -c " num=\$( echo \"{} * 1 \" | bc ); (( \$num == 0 )) && echo \"\" || echo \$num" )
        seg1_m=$( echo $seg1 | cut -c5-6 | xargs -i bash -c " num=\$( echo \"{} * 1 \" | bc ); (( \$num == 0 )) && echo \"\" || echo \$num" )
        seg1_d=$( echo $seg1 | cut -c7-8 | xargs -i bash -c " num=\$( echo \"{} * 1 \" | bc ); (( \$num == 0 )) && echo \"\" || echo \$num" )  
        cap1_endyyyymmdd=$( grep -i end_date $_fcap1 | tr -s '[:space:]' | cut -d' ' -f2 ) 
    fi 
    
    if [[ -f $_fcap2 ]];then 
        seg2=$( grep -iw $strseg $_fcap2 | tr -s '[:space:]' | cut -d' ' -f2 )
        seg2_y=$( echo $seg2 | cut -c1-4 | xargs -i bash -c " num=\$( echo \"{} * 1 \" | bc ); (( \$num == 0 )) && echo \"\" || echo \$num" )
        seg2_m=$( echo $seg2 | cut -c5-6 | xargs -i bash -c " num=\$( echo \"{} * 1 \" | bc ); (( \$num == 0 )) && echo \"\" || echo \$num" )
        seg2_d=$( echo $seg2 | cut -c7-8 | xargs -i bash -c " num=\$( echo \"{} * 1 \" | bc ); (( \$num == 0 )) && echo \"\" || echo \$num" )  
        cap2_endyyyymmdd=$( grep -i end_date $_fcap2 | tr -s '[:space:]' | cut -d' ' -f2 ) 
    fi

    if [[ -d $_dexp/holding ]];then
        cd $_dexp/holding
        arrcollyyyymm_monpost=($( find * -maxdepth 1 -mindepth 1 -type d -name '[0-9]?????' | grep -v _1mo_glo_ 2>/dev/null | sort -V ))
        arrcollyyyymm_monmapl=($( find * -maxdepth 1 -mindepth 1 -type d -name '[0-9]?????' | grep    _1mo_glo_ 2>/dev/null | sort -V ))
        #arrsstyyyymm=($( find * -maxdepth 1 -mindepth 1 -type d -name '[0-9]?????' ))
        cd - >/dev/null
    fi

    #todo:  get yyyymmdd cap info
    if [[ -f $_fcapr ]];then
        capr_yyyymmdd=$( cat $_fcapr | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
        capr_yyyymm=$( echo $capr_yyyymmdd | cut -c1-6 )
        capr_yyyy=$( echo $capr_yyyymmdd | cut -c1-4 )
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
        capric_hhmmss=$( cat $_fcapric | head -1 | tr -s '[:space:]' | cut -d' ' -f2 )
        capric_hh=$( echo $capric_hhmmss | cut -c1-2 )
        bllastdayofthemonth=$( fcal_lastdayofthemonth $capric_yyyymmdd )
        blfirstdayofthemonth=$( fcal_firstdayofthemonth $capric_yyyymmdd )
    fi

    if [[ -f $_fcapend ]];then
        realend_yyyymmdd=$( cat $_fcapend | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
        realend_yyyymm=$( cat $_fcapend | head -1 | tr -s '[:space:]' | cut -d' ' -f1 | cut -c1-6 )
    fi

    #todo:  calculate a number of *full* months that exp ran.
    arrmfull_3mo=($( numfullmonths $capric_yyyymm $end3_yyyymm $bllastdayofthemonth ))
    arrmfull_10mo=($( numfullmonths $capric_yyyymm $realend_yyyymm $bllastdayofthemonth ))
    arrmon_3mo=( $capric_yyyymm ${arrmfull_3mo[@]} )
    arrmon_10mo=( $capric_yyyymm ${arrmfull_10mo[@]} )

    arrmonrst_3mo=( $capric_yyyymmdd $end3_yyyymmdd )

    #note:  "4" is for 4-month run for the second segment
    #       "3" is for 3-month run for the last   segment
    arrmonrst_10mo=( ${arrmonrst_3mo[@]} )
    local _yyyymm=$end3_yyyymm
    for x in 4 3;do
        for i in $( seq 1 $x );do
            _yyyymm=$( fcal_nextmonth $_yyyymm ) 
        done
        arrmonrst_10mo+=( ${_yyyymm}01 )   
    done

    arrmonmom_3mo=( $end3_yyyymmdd ) 
    arrmonmom_10mo=( ${arrmonrst_10mo[@]:1} ) 

    (( $capr_yyyymmdd == $realend_yyyymmdd )) && bl10morun=true || bl10morun=false
    $bl10morun && arrmonthfull=( ${arrmfull_10mo[@]} )  || arrmonthfull=( ${arrmfull_3mo[@]} )
    $bl10morun &&     arrmonth=( ${arrmon_10mo[@]} )    ||     arrmonth=( ${arrmon_3mo[@]} )
    $bl10morun && arrmonth_rst=( ${arrmonrst_10mo[@]} ) || arrmonth_rst=( ${arrmonrst_3mo[@]} )
    $bl10morun && arrmonth_mom=( ${arrmonmom_10mo[@]} ) || arrmonth_mom=( ${arrmonmom_3mo[@]} )

    [[ -f $_fhis1 ]] && arrcollmonpost_sstincl=($( exp_getcollfreq n $_fhis1 ))

    if [[ -n $fcstdate ]];then 

        #todo:  calculate a total number of mom outputs *BASED ON CAP_RESTART* 
        local _arr3mo_endseg_yyyymmdd=($( calc_seg_endyyyymmdd $capric_yyyymmdd $end3_yyyymmdd )) 
        numfmom_3mon_calc=${#_arr3mo_endseg_yyyymmdd[@]} 

        if (( $capr_yyyymmdd > $end3_yyyymmdd ));then 
            local _arr_endseg_yyyymmdd=($( calc_seg_endyyyymmdd $capric_yyyymmdd $capr_yyyymmdd )) 
            local _numsegtot=${#_arr_endseg_yyyymmdd[@]} 
        else
            local _numsegtot=$numfmom_3mon_calc
        fi

        numfmom_calc=$_numsegtot

        
        #todo:  calculate a total number of restarts ***BASED ON CAP_RESTART***
        local _rst_yyyymmdd
        numfrst_calc=($( printf '%s\n' ${arrmonrst_10mo[@]} | while read _rst_yyyymmdd;do
                                                                  (( $_rst_yyyymmdd <= $capr_yyyymmdd )) && echo $_rst_yyyymmdd
                                                              done | wc -l ))

        numfrst_3mon_calc=($( printf '%s\n' ${arrmonrst_10mo[@]} | while read _rst_yyyymmdd;do
                                                                  (( $_rst_yyyymmdd <= $end3_yyyymmdd )) && echo $_rst_yyyymmdd
                                                              done | wc -l ))
    fi

    if $blmkdy;then 
        local _fdata_mkdy=$dutl/data_${strmkdy}_coll
        arrcolldymean=($( misc_readfbyline $_fdata_mkdy )) 
    else
        arrcolldymean=()
    fi

    if [[ -f $_fhis2 ]];then

        arrcoll=($( exp_getcollections $_fhis2 ))
        arrcollmapl=($( exp_getcollfreq m $_fhis2 ))
        arrcollpost=($( exp_getcollfreq n $_fhis2 ))
        #arrcollmapl_part=($( exp_getcollpartmon mapl $bllastdayofthemonth $blfirstdayofthemonth ))
        arrcollpost_part=($( exp_getcollpartmon post $bllastdayofthemonth $blfirstdayofthemonth ))
        arrcollpost_diu=($( exp_getcolldiurnal $_fhis2 ))
        arrcollpost_partdiu=($( printf "%s\n" ${arrcollpost_part[@]} ${arrcollpost_diu[@]} | sort -V | uniq -d ))

        if $blmkdy;then 
            arrcolldymean_part=($( printf "%s\n" ${arrcollpost_part[@]} ${arrcolldymean[@]} | sort -V | uniq -d ))
        else
            arrcolldymean_part=()
        fi
        
        #note:  array with collections to save
        arrcollpost_savediu=($( printf '%s\n' ${arrcollpost_diu[@]} | sort -V -k3 -t'_' | grep -E '_sfc|_slv' 2>/dev/null ))
        arrcollpost_partsavediu=($( printf "%s\n" ${arrcollpost_partdiu[@]} ${arrcollpost_savediu[@]} | sort -V | uniq -d ))
        arrcollpost_savedy3=($( misc_readfbyline $fdatapost_savedy3 ))
        arrcollpost_partsavedy3=($( printf "%s\n" ${arrcollpost_part[@]}    ${arrcollpost_savedy3[@]} | sort -V | uniq -d ))

        #Note: 02/06/2026   For s2s-3 NRT forecasts, dailies are no longer save in dexp 
        #                   except for collections in fdatapost_savedy3 and only the first 3months.
        #arrcollmonpost_savedya=( ice_tavg_1dy_glo_T1440x1080_slv mjo_tavg_1dy_glo_L720x361_slv )
        #arrcollmonpost_savedya=( mjo_tavg_1dy_glo_L720x361_slv )

        #todo:  create fmkfname_nop & fmkfsave_nop
        exp_createfname $bllastdayofthemonth ${arrmonth[@]}

        arrfsave=($( misc_readfbyline $fmkfsave_nop ))

        #todo:  find which file to delete
        #note:  arrfremove contains file  names. This does not necessarily mean that script has checked output
        #       timestamps and size on arc and pfe. See get_fmiss_fexist function. That's where
        #       those two variables are checked. 
        if $blleaveout; then 
            local _arrfremove=($( grep -Fxvf $fmkfsave_nop $fmkfname_nop 2>/dev/null | sort -V | uniq -u ))
            arrfremove=($( printf "$dexp/%s\n" ${_arrfremove[@]} ))
        else
            local _arrfremove=($( misc_readfbyline $fmkfname_nop ))
            arrfremove=($( printf "$dexp/%s\n" ${_arrfremove[@]} | sort -V | uniq ))
        fi

        #todo:  These are calculated values (based on created files names) based on 3-month or 10-month run
        if $blleaveout;then
            numfsave_mpl=$( printf '%s\n' ${arrfsave[@]} | grep "$strmapl"       2>/dev/null | wc -l )
            numfsave_mon=$( printf '%s\n' ${arrfsave[@]} | grep "monthly"        2>/dev/null | wc -l )
            numfsave_diu=$( printf '%s\n' ${arrfsave[@]} | grep "diurnal"        2>/dev/null | wc -l )
            numfsave_day=$( printf '%s\n' ${arrfsave[@]} | grep "\.daily\."      2>/dev/null | wc -l )
            numfsave_dmn=$( printf '%s\n' ${arrfsave[@]} | grep "dailymean"      2>/dev/null | wc -l )
            numfsave_rst=$( printf '%s\n' ${arrfsave[@]} | grep "$strrst"        2>/dev/null | wc -l )
            numfsave_mom=$( printf '%s\n' ${arrfsave[@]} | grep "$strmom_search" 2>/dev/null | wc -l )
            numfsave_tot=$( echo "$numfsave_mpl + $numfsave_mon + $numfsave_diu + $numfsave_day + $numfsave_dmn + $numfsave_mom" | bc )
        else
            numfsave_mpl=0
            numfsave_mon=0
            numfsave_diu=0
            numfsave_day=0
            numfsave_dmn=0
            numfsave_rst=0
            numfsave_mom=0
            numfsave_tot=0
        fi

        if $blleaveout && $bl10morun;then  
            numfremove_mpl=$( printf '%s\n' ${arrfremove[@]} | grep "$strmapl"        2>/dev/null | xargs -i bash -c "[[ -f {} ]] && basename {}" | wc -l )
            numfremove_mon=$( printf '%s\n' ${arrfremove[@]} | grep "\.monthly\."     2>/dev/null | xargs -i bash -c "[[ -f {} ]] && basename {}" | wc -l )
            numfremove_diu=$( printf '%s\n' ${arrfremove[@]} | grep "\.diurnal\."     2>/dev/null | xargs -i bash -c "[[ -f {} ]] && basename {}" | wc -l )
            numfremove_day=$( printf '%s\n' ${arrfremove[@]} | grep "\.daily\."       2>/dev/null | xargs -i bash -c "[[ -f {} ]] && basename {}" | grep -v $strmom_search | wc -l )
            numfremove_dmn=$( printf '%s\n' ${arrfremove[@]} | grep "\.dailymean\."   2>/dev/null | xargs -i bash -c "[[ -f {} ]] && basename {}" | wc -l )
            numfremove_rst=$( printf '%s\n' ${arrfremove[@]} | grep "$strrst"         2>/dev/null | xargs -i bash -c "[[ -f {} ]] && basename {}" | wc -l )
            numfremove_mom=$( printf '%s\n' ${arrfremove[@]} | grep "$strmom_search"  2>/dev/null | xargs -i bash -c "[[ -f {} ]] && basename {}" | wc -l )
            numfremove_tot=$( echo "$numfremove_mpl + $numfremove_mon + $numfremove_diu + $numfremove_day + $numfremove_mom" | bc )

            numfsavePfrm_mpl=$((  numfsave_mpl +  numfremove_mpl ))      
            numfsavePfrm_mon=$((  numfsave_mon +  numfremove_mon ))      
            numfsavePfrm_diu=$((  numfsave_diu +  numfremove_diu ))      
            numfsavePfrm_day=$((  numfsave_day +  numfremove_day ))      
            numfsavePfrm_dmn=$((  numfsave_dmn +  numfremove_dmn ))      
            numfsavePfrm_rst=$((  numfsave_rst +  numfremove_rst ))      
            numfsavePfrm_mom=$((  numfsave_mom +  numfremove_mom ))      
            numfsavePfrm_tot=$((  numfsave_tot +  numfremove_tot ))      
        fi

        #todo:  calculate a number of total output files ***BASED ON WHERE cap_restart IS AT***
        numcoll=${#arrcoll[@]}
        numcollmapl=${#arrcollmapl[@]}        
        numcollpost=${#arrcollpost[@]}
        #numcollmapl_part=${#arrcollmapl_part[@]}        
        numcollpost_part=${#arrcollpost_part[@]}
        numcollpost_partdiu=${#arrcollpost_partdiu[@]}
        numcollpost_diu=${#arrcollpost_diu[@]}        
        numcollpost_savediu=${#arrcollpost_savediu[@]}
        numcollpost_savedy3=${#arrcollpost_savedy3[@]}

        numcolldymean_part=${#arrcolldymean_part[@]}
        numcolldymean=${#arrcolldymean[@]}

        nummon_run=$( fcal_calcnummon $capric_yyyymmdd $capr_yyyymmdd  )

        $bllastdayofthemonth && nummon_3mo=${#arrmfull_3mo[@]} || nummon_3mo=${#arrmon_3mo[@]} 

        if $bllastdayofthemonth;then
            numfmonmapl_calc=$( echo "( $nummon_run + 1 ) * $numcollmapl"                    | bc ) 
            numfmonpost_calc=$( echo "$nummon_run * $numcollpost     + $numcollpost_part"    | bc ) 
                numfday_calc=$( echo "$nummon_run * $numcollpost     + $numcollpost_part"    | bc ) 
                numfdiu_calc=$( echo "$nummon_run * $numcollpost_diu + $numcollpost_partdiu" | bc ) 
              #numfdmn_calc=$( echo "$nummon_run * $numcolldymean   + $numcolldymean_part"  | bc ) 
                numfdmn_calc=$( echo "$nummon_run * $numcolldymean"                          | bc ) 

        else
            #numfmonmapl_calc=$( echo "( $nummon_run - 1 ) * $numcollmapl" | bc ) 
            numfmonmapl_calc=$( echo "$nummon_run * $numcollmapl"         | bc ) 
            numfmonpost_calc=$( echo "$nummon_run * $numcollpost"         | bc ) 
                numfday_calc=$( echo "$nummon_run * $numcollpost"         | bc ) 
                numfdiu_calc=$( echo "$nummon_run * $numcollpost_diu"     | bc ) 
                numfdmn_calc=$( echo "$nummon_3mo * $numcolldymean"       | bc ) 
        fi

        numftotal_calc=$( echo "$numfmonmapl_calc + $numfmonpost_calc + $numfday_calc + \
                                $numfdiu_calc     + $numfdmn_calc   + $numfmom_calc"    | bc )

        #todo:  calc 3mon
        #todo:  calculate a number of total output files ***BASED ON WHERE cap_restart IS AT***
        local thismon=3
        if $bllastdayofthemonth;then
            numfmonmapl_3mon_calc=$( echo "$thismon * $numcollmapl"                            | bc ) 
            numfmonpost_3mon_calc=$( echo "$thismon * $numcollpost     + $numcollpost_part"    | bc ) 
                numfday_3mon_calc=$( echo "$thismon * $numcollpost     + $numcollpost_part"    | bc ) 
                numfdiu_3mon_calc=$( echo "$thismon * $numcollpost_diu + $numcollpost_partdiu" | bc ) 
              numfdmean_3mon_calc=$( echo "$thismon * $numcolldymean   + $numcolldymean_part"  | bc ) 

        else
            numfmonmapl_3mon_calc=$( echo "( $thismon - 1 ) * $numcollmapl" | bc ) 
            numfmonpost_3mon_calc=$( echo "$thismon * $numcollpost"         | bc ) 
                numfday_3mon_calc=$( echo "$thismon * $numcollpost"         | bc ) 
                numfdiu_3mon_calc=$( echo "$thismon * $numcollpost_diu"     | bc ) 
              numfdmean_3mon_calc=$( echo "$thismon * $numcolldymean"       | bc ) 
        fi

        numftotal_3mon_calc=$( echo "$numfmonmapl_3mon_calc + $numfmonpost_3mon_calc + \
                                     $numfday_3mon_calc     + $numfdiu_3mon_calc     + \
                                     $numfdmean_3mon_calc   + $numfmom_3mon_calc     + $numfrst_3mon_calc" | bc )

#wmessage \@$LINENO
str="\
$fcstdate:
numfmonmapl_calc=$numfmonmapl_calc $numfmonmapl_3mon_calc
numfmonpost_calc=$numfmonpost_calc $numfmonpost_3mon_calc
    numfday_calc=$numfday_calc $numfday_3mon_calc
    numfdiu_calc=$numfdiu_calc $numfdiu_3mon_calc
    numfdmn_calc=$numfdmn_calc $numfdmean_3mon_calc
    numfmom_calc=$numfmom_calc $numfmom_3mon_calc
    numfrst_calc=$numfrst_calc $numfrst_3mon_calc
  numftotal_calc=$numftotal_calc $numftotal_3mon_calc
"
#wmessage "$str"
#wmessage "@$LINENO exit" && exit
    fi 

    return
}

cmd_gjob_nas(){
    $cmd_qstat -u $USER -W 'fmt_Queue=-maxw 40' -W o=+Rank0
}

cmd_gjob_nas_jlong(){
    $cmd_qstat -u $USER -W fmt_Jobname='-maxw 60' -W fmt_Queue='-maxw 40' -W o=+Rank0 
}

cmd_gjob_nccs() {
    #$cmd_squeue -o "%.10i %.8u %.9P %.20j %.5C %.6D %.1T %.9M %.9L %15f" -a -u $USER 
    $cmd_squeue -o "$SQUEUE_FORMAT" -a -u $USER 
}

cmd_gjob_nccs_jlong(){
    local this_squeue_format='%.10i %.8u %.10q %.9P %60j %.5C %.6D %.1T %.9M %.9L %15f'
    $cmd_squeue -o "$this_squeue_format" -a -u $USER 
}

getnumcore(){
    local _model=$1
    local thst=$( get_host )
    
    if [[ $thst == pfe ]];then
        declare -A _arrcores=( ["rom_ait"]=128 ["sky_ele"]=40 ["cas_ait"]=40 ["bro"]=28 ["bro_ele"]=28 ["has"]=24 ["ivy"]=20 )
    elif [[ $thst == dis ]];then
        declare -A _arrcores=( ["hasw"]=28 ["sky"]=40 ["cas"]=40 )
    fi

    local _numcore=${_arrcores["$_model"]}
    echo $_numcore
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
    elif [[ $hst1 == r ]];then
        local thishost=pfe
    fi

    echo $thishost
}

numfullmonths() {
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

writeheader() {
    msg_wheader
    wmessage "$hstname"
    wmessage "          exp location: "$dexp
    wmessage "      archive location: "$darc
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


pbs_jname() {
    #description:   to get a job name from gcm_*.j. 
    local _f=$1
    if [[ -z $_f ]] ;then die "a name of gcm_*.j file is a required input";exit;fi

    if [[ "$hstshort" == "pfe" ]];then
        local _vout=$( grep -i pbs $_f 2>/dev/null | grep -v "##"| grep "\-N" | head -1 | tr -s '[:space:]' | rev | cut -d' ' -f1 | rev )
    elif [[ "$hstshort" == "dis" ]];then 
        local _vout=$( grep -i sbatch $_f 2>/dev/null | grep -v "##"| grep "\--job-name=" | head -1 | tr -s '[:space:]' | rev | cut -d'=' -f1 | rev )
    fi

    echo $_vout
}

cnt_jobs() {
    local _dexp=$1
    local fgrn=gcm_run.j
    local farc=archive/run_${strscr}.sh

    [[   -z $_dexp       ]] && die "(${FUNCNAME[0]}) a full path to a exp dir is required input"
    [[ ! -f $_dexp/$fgrn ]] && die "(${FUNCNAME[0]}) $fgrn does not exists"
    [[ ! -f $_dexp/$farc ]] && die "(${FUNCNAME[0]}) $farc does not exists"

    #todo:  check fcomp file and exit if it exists
    jname_grn=$( pbs_jname $_dexp/$fgrn )
    jname_arc=$( pbs_jname $_dexp/$farc )

    #todo: get number of jobs running or on queue.
    [[ -n $jname_grn ]] && num_rgrn=$( $cmd_gjob | grep -w $jname_grn 2>/dev/null | wc -l ) || num_rgrn=0
    [[ -n $jname_arc ]] && num_rarc=$( $cmd_gjob | grep -w $jname_arc 2>/dev/null | wc -l ) || num_rarc=0

    return
}


cnt_jobs() {
    local _dexp=$1
    local fgrn=gcm_run.j
    local farc=archive/run_${strscr}.sh

    [[   -z $_dexp       ]] && die "(${FUNCNAME[0]}) a full path to a exp dir is required input"
    [[ ! -f $_dexp/$fgrn ]] && die "(${FUNCNAME[0]}) $fgrn does not exists"
    [[ ! -f $_dexp/$farc ]] && die "(${FUNCNAME[0]}) $farc does not exists"

    #todo:  check fcomp file and exit if it exists
    jname_grn=$( pbs_jname $_dexp/$fgrn )
    jname_arc=$( pbs_jname $_dexp/$farc )

    #todo: get number of jobs running or on queue.
    [[ -n $jname_grn ]] && num_rgrn=$( $cmd_qstat -e -u $USER -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep -w $jname_grn 2>/dev/null | wc -l ) || num_rgrn=0
    [[ -n $jname_arc ]] && num_rarc=$( $cmd_qstat -e -u $USER -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep -w $jname_arc 2>/dev/null | wc -l ) || num_rarc=0

    return
}

nextXmonths() {
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

nextXmonths_yyyymm() {
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

    local lnum1=$( grep -n "COLLECTIONS" $_hist 2>/dev/null | cut -d':' -f1 )
    local lnum21=$( grep -n "::" $_hist | head -1  | cut -d':' -f1)
    local lnum2=$(( lnum21 - 1 ))
    local arr=($( sed -n $lnum1','$lnum2'p' $_hist | grep -v "##" | grep -v "#" | cut -d"'" -f2 | sort))

    echo ${arr[@]}
}

exp_getcolldiurnal() {
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
    
    #local _arrcollmon=($( grep $strmon $_hist 2>/dev/null | tr -s '[:space:]' | cut -d':' -f1| cut -d' ' -f2 | cut -d'.' -f1 ))
    local _arrcoll=($( exp_getcollections $_hist ))
    local _arrcollmon=($( printf '%s\n' ${_arrcoll[@]} | xargs -i bash -c "grep {}.$strmon $_hist 2>/dev/null" | grep -w 1 | tr -s '[:space:]' | cut -d':' -f1| cut -d' ' -f2 | cut -d'.' -f1 ))
    
    if $blmon;then
        _arr=( ${_arrcollmon[@]} )
    else
        local _arr=($( printf '%s\n' ${_arrcoll[@]} ${_arrcollmon[@]} | sort | uniq -u ))
    fi

    echo ${_arr[@]}
}


exp_getcollpartmon(){
    #description:   determine collections that produce partial month outputs

    local _colltype=$1;shift
    local _bllstdy=$1;shift
    local _blfstdy=$1
    local _arr=()
    
    [[ -z $_colltype || -z $_bllstdy || -z $_blfstdy ]] && wmessage "(${FUNCNAME[0]}) input missing" && exit


    if [[ "$_colltype" == "post" ]];then 
        if [[ -n $_bllstdy ]] && $_bllstdy;then 
            local _fdata=$fdatapost_lst
        else
            local _fdata=
        fi
    elif [[ "$_colltype" == "mapl" ]];then 
        #if [[ -n $_bllstdy ]] && $_bllstdy;then 
        #    local _fdata=$fdatamapl_lst
        #else
        #    local _fdata=
        #fi
            
        local _fdata=
    fi

    if [[ -n $_fdata ]];then 
        _arr=($( misc_readfbyline $_fdata ))
    else
        if [[ "$_colltype" == "post" ]];then 
            _arr=( ${arrcollpost[@]} )
        elif [[ "$_colltype" == "mapl" ]];then 
            _arr=( ${arrcollmapl[@]} )
        fi
    fi

    echo "${_arr[@]}"
}

exp_createfname(){
    #description:   *CREATE* file names based on HISTORY.rc and YYYYMM that gcm_run.j ran so far.
    local _bllastday=$1;shift
    local _arrmon=( "$@" ) 
    #local  _bl1stday=$1
    local _strmapl="01_0000z"
    local _yyyymm

    #----------------------------------------
    #              MAPL Outputs
    #----------------------------------------
    #20210531.trb_tavg_1mo_glo_L720x361_p49.20210601_0000z.nc4
    #$fcstdate.$coll.${yyyymm}01_0000z.nc4-partial
    #$fcstdate.$coll.${yyyymm}01_0000z.nc4
    
    #todo:  create file name for mapl output
    local _arrfmapl_part=($( printf "%s\n" ${arrcollmapl[@]} | xargs -i printf '{}/'${fcstdate}'.{}.%s'$_strmapl'.nc4-partial\n' ${_arrmon[0]}   ))
    local _arrfmapl_full=($( printf "%s\n" ${arrcollmapl[@]} | xargs -i printf '{}/'${fcstdate}'.{}.%s'$_strmapl'.nc4\n'         ${_arrmon[@]:1} ))
    arrfmapl=($( printf '%s\n' ${_arrfmapl_part[@]} ${_arrfmapl_full[@]} | sort -V ))

    #----------------------------------------
    #            gcm_post Outputs
    #----------------------------------------
    #19840101.aer_inst_3hr_glo_L720x361_slv.diurnal.198402.nc4
    #$fcstdate.$coll.diurnal.YYYYMM.nc4
    #gcm_post.ocn_inst_6hr_glo_L1440x721_z50.j198402
    #gcm_post.$coll.jYYYYMM
  
    local _arrfpost_partmon=($( printf "%s\n" ${arrcollpost_part[@]}     | xargs -i printf '{}/'${fcstdate}'.{}.monthly.%s.nc4\n'   ${_arrmon[0]} ))
    local _arrfpost_partday=($( printf "%s\n" ${arrcollpost_part[@]}     | xargs -i printf '{}/'${fcstdate}'.{}.daily.%s.nc4.tar\n' ${_arrmon[0]} ))
    local _arrfpost_partdiu=($( printf "%s\n" ${arrcollpost_partdiu[@]}  | xargs -i printf '{}/'${fcstdate}'.{}.diurnal.%s.nc4\n'   ${_arrmon[0]} ))
    local _arrfpost_part=($( printf '%s\n' ${_arrfpost_partmon[@]} ${_arrfpost_partday[@]} ${_arrfpost_partdiu[@]} | sort -V ))

    local _arrfpost_mon=($( printf "%s\n" ${arrcollpost[@]}     | xargs -i printf '{}/'${fcstdate}'.{}.monthly.%s.nc4\n'   ${_arrmon[@]:1} ))
    local _arrfpost_day=($( printf "%s\n" ${arrcollpost[@]}     | xargs -i printf '{}/'${fcstdate}'.{}.daily.%s.nc4.tar\n' ${_arrmon[@]:1} ))
    local _arrfpost_diu=($( printf "%s\n" ${arrcollpost_diu[@]} | xargs -i printf '{}/'${fcstdate}'.{}.diurnal.%s.nc4\n'   ${_arrmon[@]:1} ))
    local _arrfpost_full=($( printf '%s\n' ${_arrfpost_mon[@]} ${_arrfpost_day[@]} ${_arrfpost_diu[@]} | sort -V ))
    arrfpost=($( printf '%s\n' ${_arrfpost_part[@]} ${_arrfpost_full[@]} | sort -V ))

    #----------------------------------------
    #           dailymean Outputs
    #----------------------------------------
    #20250819.sfc_tavg_3hr_glo_L720x361_sfc.dailymean.202508.nc4.tar
    #$fcstdate.$coll.dailymean.$YYYYMM.nc4.tar
    if $_bllastday;then 
        #local _arrfdymean_part=($( printf "%s\n" ${arrcolldymean_part[@]} | xargs -i printf '{}/'${fcstdate}'.{}.dailymean.%s.nc4.tar\n' ${_arrmon[0]}   ))
        local _arrfdymean_part=()
        local      _arrfdymean=($( printf "%s\n" ${arrcolldymean[@]}      | xargs -i printf '{}/'${fcstdate}'.{}.dailymean.%s.nc4.tar\n' ${_arrmon[@]:1} ))
    else
        local _arrfdymean_part=($( printf "%s\n" ${arrcolldymean_part[@]} | xargs -i printf '{}/'${fcstdate}'.{}.dailymean.%s.nc4.tar\n' ${_arrmon[0]}   ))
        local      _arrfdymean=($( printf "%s\n" ${arrcolldymean[@]}      | xargs -i printf '{}/'${fcstdate}'.{}.dailymean.%s.nc4.tar\n' ${_arrmon[@]:1} ))
    fi

    arrfdymean=($( printf '%s\n' ${_arrfdymean_part[@]} ${_arrfdymean[@]} | sort -V ))

    #----------------------------------------
    #            restarts Outputs
    #----------------------------------------
    #restarts.e20241231_21z.tar
    #restarts.e20250401_21z.tar
    #$strrst.e${yyyymmdd}_${capric_hh}z.tar
    arrfrst=($( printf "${strrst}/${strrst}.e%s_${capric_hh}z.tar\n" ${arrmonth_rst[@]} ))

    #----------------------------------------
    #              MOM Outputs
    #----------------------------------------
    #ocean_daily.e20250401_21z.nc
    #ocean_daily.e20250801_21z.nc
    #${strmom_search}.e${yyyymmdd}_${capric_hh}z.nc
    arrfmom=($( printf "${strmom}/${strmom_search}.e%s_${capric_hh}z.nc\n" ${arrmonth_mom[@]} ))

    #------------------------------------------------------------
    #                  gcm_post Outputs to Save
    #------------------------------------------------------------
    #Monthly
    arrfpost_savemon=($( printf '%s\n' ${_arrfpost_partmon[@]} ${_arrfpost_mon[@]} ))

    #Diurnal
    local  _strcoll_savediu=$( echo ${arrcollpost_savediu[@]} | sed "s# #|#g" | rev | cut -c2- | rev ) 
    arrfpost_savediu=($( printf '%s\n' ${_arrfpost_partdiu[@]} ${_arrfpost_diu[@]} | sort -V | grep -E "$_strcoll_savediu" ))

    #Daily
    local _arrsavedy3=($( printf "%s.daily\n" ${arrcollpost_savedy3[@]} | while read strcoll;do
                                                                              for _yyyymm in ${arrmon_3mo[@]};do
                                                                                  echo ${strcoll}.$_yyyymm
                                                                              done
                                                                          done ))
                                                                                   
    local _strsavedy3=$( echo ${_arrsavedy3[@]} | sed "s# #|#g" | rev | cut -c2- | rev )
    arrfpost_savedy3=($( printf '%s\n' ${_arrfpost_partday[@]} ${_arrfpost_day[@]} | sort -V | grep -E "$_strsavedy3" ))

    #todo:  create fmkfname_nop
    printf '%s\n' ${arrfmapl[@]}              | sort -V >| $fmkfname_nop 
    printf '%s\n' ${arrfpost[@]}              | sort -V >> $fmkfname_nop
    printf '%s\n' ${arrfdymean[@]}            | sort -V >> $fmkfname_nop
    printf '%s\n' ${arrfmom[@]} ${arrfrst[@]} | sort -V >> $fmkfname_nop
    
    #todo:  create fmkfsave_nop
    printf '%s\n' ${arrfmapl[@]} ${arrfpost_savemon[@]} ${arrfpost_savediu[@]} ${arrfpost_savedy3[@]} ${arrfdymean[@]} >| $fmkfsave_nop

    return
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

fcal_calcnummon() {
    #figure out a number of months between two given dates (YYYYMMDD).
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

fcal_nummon() {
    #figure out  months between two given dates (YYYYMMDD).

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

fcal_isleap() { 
    #determine whether or not input year is a leap year. Output is boolean.
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

numdaysinmonth() {
    #calculates number of days in a given month.
    #Usage:  ${FUNCNAME[0]} [month (int)] [year (int)]

    local arrNLMdays=(31 28 31 30 31 30 31 31 30 31 30 31)
    local arrLMdays=(31 29 31 30 31 30 31 31 30 31 30 31)
    local mon=$1
    local yr=$2
    local ii=$(( mon - 1 ))
    
    #todo:  check leap year
    local blleapyr=$( fcal_isleap $yr )

    if $blleapyr;then
        nummon=${arrLMdays[ii]}
    else
        nummon=${arrNLMdays[ii]}
    fi
    echo $nummon
}

calc_numseg_winners() {
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

calc_numseg_3mon() {
    
    local _fcstyyyymmdd=$1;shift
    local _endyyyymmdd=$1
    local _fcstyyyy=$( echo $_fcstyyyymmdd | cut -c1-4 )

    local nummonth1=3
    local end3_yyyymm=$( nextXmonths_yyyymm $( date -d $_fcstyyyymmdd +%Y ) $( printf '%01g' $( date -d $_fcstyyyymmdd +%m ) ) 0 $(( nummonth1 + 1 )) | rev | cut -d' ' -f1 | rev  )
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

calc_seg_endyyyymmdd(){
    local _userbeg_yyyymmdd=$1;shift
    local _userend_yyyymmdd=$1
    local _userbeg_yyyy=$( echo $_userbeg_yyyymmdd | cut -c1-4 ) 
    local _userend_yyyy=$( echo $_userend_yyyymmdd | cut -c1-4 ) 
    local arrseg_yyyymmdd=()
    local dd="01"

    #todo:  add YYYY0101 when begyyyy and endyyyy are different
    #note:  MOM create output at the beginning of Year. 
    #if (( $_userbeg_yyyy != $_userend_yyyy ));then 
    #    local _arryyyy=($( seq $_userbeg_yyyy $_userend_yyyy )) 
    #    local _arradd_yyyymmdd=($( printf "%s0101\n" ${_arryyyy[@]} | tail -n +2 ))  
    #fi

    if [[ "$hstshort" == "dis" ]];then 
        #Note 06/26/2024 - num_seg = 0 if you use CAP_0.rc
        #local num_seg=0
        local num_seg=1
    elif [[ "$hstshort" == "pfe" ]];then 
        local num_seg=1
    fi
   
        if (( $num_seg == 0 ));then 
            local endyyyymmdd=$cap0_endyyyymmdd
            local seg_y=$seg0_y
            local seg_m=$seg0_m
            local seg_d=$seg0_d
    
        elif (( $num_seg == 1 ));then 
            local endyyyymmdd=$cap1_endyyyymmdd
            local seg_y=$seg1_y
            local seg_m=$seg1_m
            local seg_d=$seg1_d
    
        elif (( $num_seg == 2 ));then 
            local endyyyymmdd=$cap2_endyyyymmdd
            local seg_y=$seg2_y
            local seg_m=$seg2_m
            local seg_d=$seg2_d
        else
            echo ""
        fi
    
    local yyyymmdd=$_userbeg_yyyymmdd 
    while (( $yyyymmdd < $_userend_yyyymmdd ));do
    
        [[ -n $seg_y ]] && local yyyymm=$( date -d "$yyyymmdd +${seg_y}years" +%Y%m )${dd}
        if [[ -n $seg_m ]];then 
            local yyyymm=$( echo $yyyymmdd | cut -c1-6 ) 
            local yyyy_beg=$( echo $yyyymmdd | cut -c1-4 ) 
            #    dd=$( echo $yyyymmdd | cut -c7-8 ) 

            local xmon=1
            while (( $xmon <= $seg_m )) ;do
                local yyyymm=$( fcal_nextmonth $yyyymm )
                local   yyyy=$( echo $yyyymm | cut -c1-4 )
                #(( $yyyy_beg != $yyyy )) && break
                xmon=$(( xmon + 1 ))
            done
            yyyymmdd=$yyyymm$dd
        fi
        [[ -n $seg_d ]] && yyyymmdd=$( date -d "$yyyymmdd +${seg_d}days" +%Y%m%d )
    
        if (( $yyyymmdd >= $endyyyymmdd ));then 
            yyyymmdd=$endyyyymmdd
            num_seg=$(( num_seg + 1 ))
        fi
        
        if (( $yyyymmdd <= $capr_yyyymmdd ));then 
            arrseg_yyyymmdd+=( $yyyymmdd )
        fi
    done

    local arrseg_yyyymmdd=($( printf '%s\n' ${arrseg_yyyymmdd[@]} | sort -V | uniq ))
    
    echo ${arrseg_yyyymmdd[@]} 
}

fnamearc(){
    local hst=$1;
    local _ftmp=_tmp_${FUNCNAME[0]}
    #local thiscmd="$cmd_sup shiftc --no-cron --no-mail --wait"

    if $blnode_nas || [[ "$hstshort" == "pfe" ]];then 
        ssh -q $hst "find $darc/* -maxdepth 1 -mindepth 1 -type f -name '$fcstdate.*.nc*' -o -name '${strrst}.*.tar' | grep -v bbftp.tmp | xargs -i stat --printf="'"'"%Y %s %n\n"'"'" {} > $fexistarc" 2>/dev/null
        ssh -q $hst "find $darc/$strmom/* -type f | grep -v bbftp.tmp | xargs -i stat --printf="'"'"%Y %s %n\n"'"'" {} >> $fexistarc" 2>/dev/null
        cat $fexistarc 2>/dev/null | sed "s#$darc/##g" >| $fexistarc_nop

    elif $blnode_nccs || [[ "$hstshort" == "dis" ]];then 
        if $blarc2os;then 

            local fwftmsz=$dexp/wftmsz_flist

            #$cmd_sup test -f $hostarc:$darc/$fwftmsz >> /dev/null 2>&1
            #local status_fexist=$?
            
            if [[ -s $fwftmsz ]];then 
                [[ -f $fexistarc ]] && rm -f $fexistarc
                [[ -f $_ftmp     ]] && rm -f $f_ftmp
                
                cp -p $fwftmsz $cdir/$_ftmp 

                #todo:  remove file names, such as CAP.rc, AGCM.rc, etc., from fwftmsz 
                #       if they already exist in archive dir.
                if [[ -f $_ftmp ]];then 
                    local _strfignore=$( cat $_ftmp | cut -d' ' -f3 | sed "s#$darc/##g" | cut -d'/' -f1 | sort -V | uniq | grep -vE "_glo_|$strrst|$strmom" | tr '\n' '|' | rev | cut -c2- |rev )

                    if [[ -z $_strfignore ]];then 
                        mv $_ftmp $fexistarc
                    else
                        cat $_ftmp | grep -vEi "$_strfignore" >> $fexistarc 
                        cat $fexist_arc | sed "s#$darc/##g" >| $fexistarc_nop
                        [[ -f $_ftmp ]] && rm -f $_ftmp
                    fi
                else
                    wmessage "file not available: $fwftmsz. Exit @$LINENO" && exit
                fi

            else
                wmessage "file not available: $fwftmsz. Exit @$LINENO" && exit
            fi

        else
            find $darc/* -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*.nc*" -o -name "${strrst}.*.tar" | grep -v bbftp.tmp | xargs -i stat --printf='%Y %s %n\n' {} > $fexistarc 2>/dev/null
            find $darc/$strmom/* -type f 2>/dev/null | xargs -i stat --printf="%Y %s %n\n" {} >> $fexistarc 2>/dev/null
        fi
    fi
    return
}

getfname_arc() {
    #description:   get a file with output file name and size on lou
    if $blhostarc_avail;then 
        :
    else
        [[ ! -f $fexistarc ]] && touch $fexistarc $fexistarc_nop
        return
    fi

    [[ -f $fexistarc ]] && rm -f $fexistarc $fexistarc_nop

    if [[ "$hstshort" == "dis" && "$hostarc" == "lfe" ]];then 
        local fwftmsz=$dexp/wftmsz_flist

        if [[ ! -s $fwftmsz ]];then 
            #todo:  create empty files
            touch $fexistarc $fexistarc_nop
        else
            fnamearc $hostarc
        fi

    else
        ssh -q $hostarc test -d $darc
        if (( $? > 0 ));then
            #todo:  create empty files
            touch $fexistarc $fexistarc_nop
        else
            fnamearc $hostarc
        fi
    fi

    #todo:  delete tar-1 files which is a product of an unsuccessful shiftc operation
    if ! $blrsync && [[ "$hstshort" == "pfe" ]];then 
        local arrarcrmf=($( grep "tar-1" $fexistarc 2>/dev/null | cut -d' ' -f3 ))
        while (( ${#arrarcrmf[@]} > 0 ));do
            ssh -q $hostarc rm -f ${arrarcrmf[@]}
            [[ -f $fexistarc ]] && rm -f $fexistarc
            fnamearc $hostarc
            local arrarcrmf=($( grep "tar-1" $fexistarc 2>/dev/null | cut -d' ' -f3 ))
        done 
    fi

    return
}

getfname_exp() {
    #description:   get all output file name with byte size
    [[ -f $fexistexp ]] && rm -f $fexistexp

    find $dexp/* -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*.nc*" 2>/dev/null  | grep -Ev "$collsst|$strscrach" | xargs -i stat --printf="%Y %s %n\n" {} | sort -V >| $fexistexp
    [[ -d $dexp/$strrst ]] && find $dexp/$strrst/* -type f -name "${strrst}*.tar" 2>/dev/null| xargs -i stat --printf="%Y %s %n\n" {} >> $fexistexp
    [[ -d $dexp/$strmom ]] && find $dexp/$strmom/* -type f -name "${strmom_search}*.nc*" 2>/dev/null | xargs -i stat --printf="%Y %s %n\n" {} >> $fexistexp

    cat $fexistexp | sed "s#$dexp/##g" >| $fexistexp_nop
    
    return
}

_get_fmiss_fexist() {
    #description:   find missing and existing (with file size other than zero) and place
    #               them in two separate global arrays.
    local arrpfefout=($( cat $fexistexp | cut -d' ' -f3 ))
    local pfefout 

    arrfmiss=()
    for pfefout in ${arrpfefout[@]};do
        local blpasshere=false
        local fname=($( echo $pfefout | rev | cut -d' ' -f1 | cut -d'/' -f1 | rev  ))
        local fpfesize=$( grep $fname $fexistexp 2>/dev/null | tail -1 | cut -d' ' -f1 )
        local  fpfesec=$( grep $fname $fexistexp 2>/dev/null | tail -1 | cut -d' ' -f2 )
        local fpfepath=$( grep $fname $fexistexp 2>/dev/null | tail -1 | cut -d' ' -f3 )

        #todo:  check if file exists on archive host
        local _numthisf=$( grep $fname $fexistarc 2>/dev/null | grep -v "tar-1" 2>/dev/null | wc -l )

        if (( $_numthisf == 0 ));then
            #todo:  add file to arrfmiss array
            arrfmiss+=( $fpfepath )

        elif (( $_numthisf > 0 ));then
            #todo:  check file size if file exists on archive host
            local farcsize=$( grep $fname $fexistarc 2>/dev/null | tail -1 | cut -d' ' -f1 | xargs )
            local  farcsec=$( grep $fname $fexistarc 2>/dev/null | tail -1 | cut -d' ' -f2 | xargs )
            local fcoll=$( basename $fpfepath | cut -d'.' -f2 )
           
            if [[ -n $farcsize && -n $farcsec ]] && (( $fpfesize == $farcsize && $fpfesec == $farcsec && $farcsize > 0 ));then
                printf '%s\n' "${arrfremove[@]}" | grep "$pfefout" >> /dev/null 2>&1
                local status_here=$?

                if (( $status_here == 0 ));then
                    arrfdelrdyexp+=( $fpfepath )
                    blpasshere=true
                fi

            elif [[ -n $farcsize && -n $farcsec ]] && (( $fpfesize != $farcsize || $fpfesec != $farcsec || $farcsize > 0 ));then
                #todo:  add fout in arrfmiss since size is not the same
                arrfmiss+=( $fpfepath )
                blpasshere=true
            fi

            #todo:  check if file was written in fdelout  
            grep $fname $fdelout >> /dev/null 2>&1
            local status_grep=$?

            #todo:  remove line from fdelout 
            #note:  this is important especially when exp was re-run.
            if $blpasshere && (( $status_grep == 0 ));then
                local linenum=$( grep -n $fname $fdelout 2>/dev/null | cut -d':' -f1 )
                sed -i "${linenum}d" $fdelout 
            fi
        fi
    done

    return
}

get_fmiss_fexist() {

    #description:   find missing and existing (with file size other than zero)
    #note:      arrfmiss: used to create_shiftc_fin function
    local _ftmp1=tmp_${strscr}_${FUNCNAME[0]}_1

    local     _ftmp_live_nop=tmp_${strscr}_${FUNCNAME[0]}_existing_nop
    local _ftmp_fdeldone_nop=tmp_${strscr}_${FUNCNAME[0]}_fdeldone

    local _arrfrm=( $_ftmp_live_nop $_ftmp_fdeldone_nop )

    [[ -f $_ftmp1 ]] && rm -f $_ftmp1
    rm -f ${_arrfrm[@]} 2>/dev/null

    #----------------------------------------
    #        Diff mkfname vs. EXP/ARC
    #----------------------------------------
    cat $fexistarc_nop $fexistexp_nop $fdelout_nop 2>/dev/null | cut -d' ' -f3 | sort -V | uniq >| $_ftmp_live_nop

    grep -Fxvf $_ftmp_live_nop  $fmkfname_nop   2>/dev/null >| $fdiff_nop
    grep -Fxvf $fmkfname_nop    $_ftmp_live_nop 2>/dev/null >> $fdiff_nop

    #----------------------------------------
    #           ARC Missing Files
    #----------------------------------------
    #todo:  find missing files on lou
    grep -Fxvf $fexistarc_nop $fexistexp_nop 2>/dev/null >| $ftmp_fmissarc_nop 

    #----------------------------------------
    #       EXP Ready-to-Delete Files
    #----------------------------------------
    wmessage "@$LINENO 02/10/2026 Work on this code here ONCE fdelout HAS CONTENTS!!"  
    grep -Fwvf $fmkfsave_nop $fexistexp_nop 2>/dev/null >| $fdelrdyexp_nop 

    #----------------------------------------
    #             Update fdelout
    #----------------------------------------
    #todo:  check if file was written in fdelout. If it is, remove line from fdelout 
    #note:  this is important especially when exp was re-run.
    wmessage "@$LINENO 02/10/2026 Work on this code here ONCE fdelout HAS CONTENTS!!"  
    grep -Fxvf $fdelrdyexp_nop $fdelout_nop 2>/dev/null >| $_ftmp_fdeldone_nop 

local str="\
@$LINENO
#lines ftmp_fmissarc_nop  : $( cat $ftmp_fmissarc_nop  | wc -l )
#lines fexistexp_nop      : $( cat $fexistexp_nop      | wc -l )    
#lines fexistarc_nop      : $( cat $fexistarc_nop      | wc -l )    
#lines fdelrdyexp_nop     : $( cat $fdelrdyexp_nop     | wc -l )    
#lines _ftmp_fdeldone_nop : $( cat $_ftmp_fdeldone_nop | wc -l )    
"
#wmessage "$str" 

    #----------------------------------------
    #        Create Arrays/Data Files
    #----------------------------------------
    cat $ftmp_fmissarc_nop | cut -d' ' -f3 >| $_ftmp1 
    local      _arrfmiss_nop=($( misc_readfbyline $_ftmp1 ))
    cat $fdelrdyexp_nop    | cut -d' ' -f3 >| $_ftmp1 
    local _arrfdelrdyexp_nop=($( misc_readfbyline $_ftmp1 ))

    (( ${#_arrfmiss_nop[@]}      > 0 )) &&      arrfmiss=($( printf "$dexp/%s\n" ${_arrfmiss_nop[@]}      | sort -V )) 
    (( ${#_arrfdelrdyexp_nop[@]} > 0 )) && arrfdelrdyexp=($( printf "$dexp/%s\n" ${_arrfdelrdyexp_nop[@]} | sort -V )) 
   
    mv $_ftmp_fdeldone_nop $fdelout_nop
    cat $fdelout_nop | xargs -i echo $dexp/{} >| $fdelout
    
    [[ -f $_ftmp1 ]] && rm -f $_ftmp1
    #rm -f ${_arrfrm[@]} 2>/dev/null

    return
}

get_tarable(){
    #description:   find nonmonthly outputs in holding which can be compressed.
    local _dhold=$dexp/holding
    local _fhis1=$dexp/HISTORY_1.rc
    local arryyyymm_dhold=($( find $_dhold/$coll/* -type f -name "*.nc?" 2>/dev/null | cut -d'.' -f3 | sed 's/[^0-9]*//g' | cut -c1-6 | sort | uniq | grep -v $capric_yyyymm ))

    for coll in ${arrcollpost[@]};do
        local collfreq=$( grep $coll.frequency $_fhis1 |  tr -s '[:space:]' |rev |  cut -d' ' -f1 | tr -d ',' | rev | cut -c1-2 | xargs -i bash -c "echo "'"'"{}*1"'"'" | bc" )

        for yyyymm in ${arryyyymm_dhold[@]};do

            local yyyy_mm=${yyyymm:0:4}${yyyymm:4:2}
            local ldd=$( numdaysinmonth $( echo "${yyyymm:4:2}*1" | bc ) ${yyyymm:0:4} )
            local numflastday=$( find $_dhold/$coll/* -type f -name "*.$coll.*$yyyy_mm$ldd*.nc?" | wc -l )
            local calcnumflastday=$( echo "24/$collfreq" | bc ) 
            local ftar=$dexp/$coll/$fcstdate.$coll.$yyyymm.nc4.tar

            #todo:  compress non-monthly output if dexp/coll doesn't have tar
            if [[ ! -f $ftar ]] && (( $calcnumflastday == $numflastday )); then
                arrftarable+=( $ftar )
            fi

        done 
    done
    return
}

find_partialinhold(){
    #description: find mapl monthly partial files in holding dir
    local _dhold=$dexp/holding
    local fhold

    #19911202.ice_tavg_1dy_glo_T1440x1080_slv.daily.199201.nc4.tar
    local arrfpartial_hold=($( find $_dhold/* -type f -name "$fcstdate.*_0000z.nc4-partial" 2>/dev/null | grep -Ev "$collsst" | xargs -i realpath {} ))

    for fhold in ${arrfpartial_hold[@]};do
        #19911202.ice_tavg_1dy_glo_T1440x1080_slv.daily.199201.nc4.tar
        local fhold_bname=$( basename $fhold ) 
        local coll=$( echo $fhold_bname | cut -d'.' -f2 ) 
        local fhold_coll=$dexp/$coll/$fhold_bname

        if [[ ! -f $fhold_coll ]]; then
            arrmvpartial+=( $fhold ) 
        fi
    done 

    return
}

count_files(){

    #----------------------------------------
    #          Count Files in DEXP            
    #----------------------------------------
    cd $dexp
    #numfdisk_all=$( find * -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*.nc*" | grep -Ev "$collsst|$strscrach" | wc -l )
        numfexp_tar_cnt=$( find * -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*.??????.nc4.tar" | grep -Ev "$collsst|$strscrach|daily|dailymean" | wc -l )
        numfexp_mom_cnt=$( find * -maxdepth 1 -mindepth 1 -type f -name "$strmom_search*" | grep -v $strscrach | grep $strmom | wc -l )
        numfexp_rst_cnt=$( find * -maxdepth 1 -mindepth 1 -type f -name "$strrst*"        | grep -v $strscrach | wc -l  )

     numfexp_total_cnt1=$( find * -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*.nc4*" | grep -Ev "$collsst|$strscrach"    | wc -l )
      numfexp_total_cnt=$(( numfexp_total_cnt1 - numfexp_tar_cnt + $numfexp_mom_cnt )) #excl. rst
                numfexp=$(( numfexp_total_cnt + numfexp_rst_cnt ))                     #incl. rst


    numfexp_monmapl_cnt=$( find * -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*z.nc4*"               | grep -Ev "$collsst|$strscrach" | wc -l )
        numfexp_day_cnt=$( find * -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*.daily.*.nc4.tar"     | grep -Ev "$collsst|$strscrach" | wc -l )
        numfexp_diu_cnt=$( find * -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*.diurnal.*.nc4*"      | grep -Ev "$collsst|$strscrach" | wc -l )
    numfexp_monpost_cnt=$( find * -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*.monthly.*.nc4*"      | grep -Ev "$collsst|$strscrach" | wc -l )

    if $blmkdy;then 
        numfexp_dmn_cnt=$( find * -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*.dailymean.*.nc4.tar" | grep -Ev "$collsst|$strscrach" | wc -l )
    else
        numfexp_dmn_cnt=0
    fi

        numfexp_del_cnt=$( cat $fdelout 2>/dev/null | grep -v "${strrst}" 2>/dev/null | sort -V | uniq | wc -l ) 
        numfexp_mis_cnt=$( echo "$numftotal_calc - $numfexp_total_cnt - $numfexp_del_cnt" | bc )

    #note:  add delete output from 3mon-run. This number give # of output on pfe after 3mon output are deleted but before 10mon
    #       output are deleted
    #numftotal_cnt_p3mdel=$( echo "$numfexp_total_cnt + $numfdeleted_3mon" | bc )

    cd - >/dev/null

    #----------------------------------------
    #   grep #Files in gcmarch_wftmsz_exp
    #----------------------------------------
    if [[ -f $fexistexp ]];then 
                      numfexp_wftmsz=$( cat $fexistexp | wc -l )
          numfexp_wftmsz_misctar_cnt=$( cat $fexistexp | grep .[0-9]*.nc4.tar | grep -Ev "$collsst|$strscrach|daily" | wc -l )
        numfexp_wftmsz_fmomfout_cnt1=$( cat $fexistexp | grep -Ev "$strrst|$collsst" | wc -l )
         numfexp_wftmsz_fmomfout_cnt=$(( $numfexp_wftmsz_fmomfout_cnt1 - numfexp_wftmsz_misctar_cnt ))

        numfexp_wftmsz_monmapl_cnt=$( cat $fexistexp | grep z.nc4            | grep -Ev "$collsst|$strscrach" | wc -l )
            numfexp_wftmsz_day_cnt=$( cat $fexistexp | grep .daily.[0-9]     | grep .nc4.tar | grep -v tar.   | grep -Ev "$collsst|$strscrach" | wc -l )
            numfexp_wftmsz_dmn_cnt=$( cat $fexistexp | grep .dailymean.[0-9] | grep .nc4.tar | grep -v tar.   | wc -l )
            numfexp_wftmsz_diu_cnt=$( cat $fexistexp | grep .diurnal.        | grep -Ev "$collsst|$strscrach" | wc -l )
        numfexp_wftmsz_monpost_cnt=$( cat $fexistexp | grep .monthly.        | grep -Ev "$collsst|$strscrach" | wc -l )
            numfexp_wftmsz_rst_cnt=$( cat $fexistexp | grep $strrst          | wc -l )
            numfexp_wftmsz_mom_cnt=$( cat $fexistexp | grep $strmom_search   | wc -l )
            numfexp_wftmsz_mis_cnt=$( echo "$numftotal_calc - $numfexp_wftmsz_fmomfout_cnt" | bc )
    fi

    #----------------------------------------
    #grep #Files in gcmarch_mkfname_fname_nop
    #----------------------------------------
    if [[ -f $fmkfname_nop ]];then 
                      numfexp_mkfname=$( cat $fmkfname_nop | wc -l )
          numfexp_mkfname_misctar_cnt=$( cat $fmkfname_nop | grep .[0-9]*.nc4.tar | grep -Ev "$collsst|$strscrach|daily" | wc -l )
        numfexp_mkfname_fmomfout_cnt1=$( cat $fmkfname_nop | grep -Ev "$strrst|$collsst" | wc -l )
         numfexp_mkfname_fmomfout_cnt=$(( $numfexp_mkfname_fmomfout_cnt1 - numfexp_mkfname_misctar_cnt ))

        numfexp_mkfname_monmapl_cnt=$( cat $fmkfname_nop | grep z.nc4            | grep -Ev "$collsst|$strscrach" | wc -l )
            numfexp_mkfname_day_cnt=$( cat $fmkfname_nop | grep .daily.[0-9]     | grep .nc4.tar | grep -v tar.   | grep -Ev "$collsst|$strscrach" | wc -l )
            numfexp_mkfname_dmn_cnt=$( cat $fmkfname_nop | grep .dailymean.[0-9] | grep .nc4.tar | grep -v tar.   | wc -l )
            numfexp_mkfname_diu_cnt=$( cat $fmkfname_nop | grep .diurnal.        | grep -Ev "$collsst|$strscrach" | wc -l )
        numfexp_mkfname_monpost_cnt=$( cat $fmkfname_nop | grep .monthly.        | grep -Ev "$collsst|$strscrach" | wc -l )
            numfexp_mkfname_rst_cnt=$( cat $fmkfname_nop | grep $strrst          | wc -l )
            numfexp_mkfname_mom_cnt=$( cat $fmkfname_nop | grep $strmom_search   | wc -l )
            numfexp_mkfname_mis_cnt=$( echo "$numftotal_calc - $numfexp_mkfname_fmomfout_cnt" | bc )
    fi

    #----------------------------------------
    # grep #Files in gcmarch_wftmsz_exp_sst
    #----------------------------------------
    #note:  these are counting a number of files (no checking size)
    [[ -f $fexistexp_sst ]] && numfexp_sst=$( cat $fexistexp_sst | grep .tar 2>/dev/null | wc -l ) 

    #----------------------------------------
    #   grep #Files in gcmarch_wftmsz_arc
    #----------------------------------------
    if [[ -f $fexistarc ]];then
                      numfarc=$( cat $fexistarc | wc -l )
          numfarc_misctar_cnt=$( cat $fexistarc | grep .[0-9]*.nc4.tar | grep -Ev "$collsst|$strscrach|daily" | wc -l )
        numfarc_fmomfout_cnt1=$( cat $fexistarc | grep -Ev "$strrst|$collsst" | wc -l )
         numfarc_fmomfout_cnt=$(( $numfarc_fmomfout_cnt1 - numfarc_misctar_cnt ))

        numfarc_monmapl_cnt=$( cat $fexistarc | grep z.nc4            | grep -Ev "$collsst|$strscrach" | wc -l )
            numfarc_day_cnt=$( cat $fexistarc | grep .daily.[0-9]     | grep .nc4.tar | grep -v tar.   | grep -Ev "$collsst|$strscrach" | wc -l )
            numfarc_dmn_cnt=$( cat $fexistarc | grep .dailymean.[0-9] | grep .nc4.tar | grep -v tar.   | wc -l )
            numfarc_diu_cnt=$( cat $fexistarc | grep .diurnal.        | grep -Ev "$collsst|$strscrach" | wc -l )
        numfarc_monpost_cnt=$( cat $fexistarc | grep .monthly.        | grep -Ev "$collsst|$strscrach" | wc -l )
            numfarc_rst_cnt=$( cat $fexistarc | grep $strrst          | wc -l )
            numfarc_mom_cnt=$( cat $fexistarc | grep $strmom_search   | wc -l )
            numfarc_mis_cnt=$( echo "$numftotal_calc - $numfarc_fmomfout_cnt" | bc )

    fi
    
    #note:  size has been check with these files
    numfmissing=${#arrfmiss[@]}

    local _blsanitycheck=true
    local _blsanitycheck=false
    ! $_blsanitycheck && return 

    #============================================================
    #                       Sanity Check!
    #============================================================
    #----------------------------------------
    # DEXP: Comp Counts vs. Grep wftmsz_exp
    #----------------------------------------
    local _blmatch_wftmsz=true
    
    ((               $numfexp_wftmsz != $numfexp             )) && _blmatch_wftmsz=false      
    ((   $numfexp_wftmsz_misctar_cnt != $numfexp_tar_cnt     )) && _blmatch_wftmsz=false      
    ((  $numfexp_wftmsz_fmomfout_cnt != $numfexp_total_cnt   )) && _blmatch_wftmsz=false      
    ((   $numfexp_wftmsz_monmapl_cnt != $numfexp_monmapl_cnt )) && _blmatch_wftmsz=false      
    ((       $numfexp_wftmsz_day_cnt != $numfexp_day_cnt     )) && _blmatch_wftmsz=false      
    ((       $numfexp_wftmsz_diu_cnt != $numfexp_diu_cnt     )) && _blmatch_wftmsz=false      
    ((   $numfexp_wftmsz_monpost_cnt != $numfexp_monpost_cnt )) && _blmatch_wftmsz=false      
    ((       $numfexp_wftmsz_dmn_cnt != $numfexp_dmn_cnt     )) && _blmatch_wftmsz=false      
    ((       $numfexp_wftmsz_rst_cnt != $numfexp_rst_cnt     )) && _blmatch_wftmsz=false      
    ((       $numfexp_wftmsz_mom_cnt != $numfexp_mom_cnt     )) && _blmatch_wftmsz=false      
    ((       $numfexp_wftmsz_mis_cnt != $numfexp_mis_cnt     )) && _blmatch_wftmsz=false      

        local _strnum=" \
         TOTAL: $numfexp_wftmsz,$numfexp             
         #Tars: $numfexp_wftmsz_misctar_cnt,$numfexp_tar_cnt     
     #Non-Tars: $numfexp_wftmsz_fmomfout_cnt,$numfexp_total_cnt   
         #MAPL: $numfexp_wftmsz_monmapl_cnt,$numfexp_monmapl_cnt 
        #daily: $numfexp_wftmsz_day_cnt,$numfexp_day_cnt     
      #diurnal: $numfexp_wftmsz_diu_cnt,$numfexp_diu_cnt     
      #monthly: $numfexp_wftmsz_monpost_cnt,$numfexp_monpost_cnt 
    #dailymean: $numfexp_wftmsz_dmn_cnt,$numfexp_dmn_cnt     
     #restarts: $numfexp_wftmsz_rst_cnt,$numfexp_rst_cnt     
          #MOM: $numfexp_wftmsz_mom_cnt,$numfexp_mom_cnt     
      #Missing: $numfexp_wftmsz_mis_cnt,$numfexp_mis_cnt
"

    if ! $_blmatch_wftmsz;then 
        wmessage
        wmessage "@$LINENO (${FUNCNAME[0]}) WARNING: # of files don't match up between $( basename $fexistexp ) and file counts."
        wmessage "$_strnum"
        wmessage
    fi


    #----------------------------------------
    # DEXP: Grep mkfname vs. Grep wftmsz_exp
    #----------------------------------------
    local _blmatch_mkfname=true
    ((             $numfexp_mkfname != $numfexp             )) && _blmatch_mkfname=false      
    (( $numfexp_mkfname_misctar_cnt != $numfexp_tar_cnt     )) && _blmatch_mkfname=false      
    (( $numfexp_mkfname_fmomfout_cnt != $numfexp_total_cnt   )) && _blmatch_mkfname=false      
    (( $numfexp_mkfname_monmapl_cnt != $numfexp_monmapl_cnt )) && _blmatch_mkfname=false      
    ((     $numfexp_mkfname_day_cnt != $numfexp_day_cnt     )) && _blmatch_mkfname=false      
    ((     $numfexp_mkfname_diu_cnt != $numfexp_diu_cnt     )) && _blmatch_mkfname=false      
    (( $numfexp_mkfname_monpost_cnt != $numfexp_monpost_cnt )) && _blmatch_mkfname=false      
    ((     $numfexp_mkfname_dmn_cnt != $numfexp_dmn_cnt     )) && _blmatch_mkfname=false      
    ((     $numfexp_mkfname_rst_cnt != $numfexp_rst_cnt     )) && _blmatch_mkfname=false      
    ((     $numfexp_mkfname_mom_cnt != $numfexp_mom_cnt     )) && _blmatch_mkfname=false      
    ((     $numfexp_mkfname_mis_cnt != $numfexp_mis_cnt     )) && _blmatch_mkfname=false      

        local _strnum=" \
        TOTAL: $numfexp_mkfname,$numfexp             
        #Tars: $numfexp_mkfname_misctar_cnt,$numfexp_tar_cnt     
    #Non-Tars: $numfexp_mkfname_fmomfout_cnt,$numfexp_total_cnt   
        #MAPL: $numfexp_mkfname_monmapl_cnt,$numfexp_monmapl_cnt 
       #daily: $numfexp_mkfname_day_cnt,$numfexp_day_cnt     
     #diurnal: $numfexp_mkfname_diu_cnt,$numfexp_diu_cnt     
     #monthly: $numfexp_mkfname_monpost_cnt,$numfexp_monpost_cnt 
   #dailymean: $numfexp_mkfname_dmn_cnt,$numfexp_dmn_cnt     
    #restarts: $numfexp_mkfname_rst_cnt,$numfexp_rst_cnt     
         #MOM: $numfexp_mkfname_mom_cnt,$numfexp_mom_cnt     
     #Missing: $numfexp_mkfname_mis_cnt,$numfexp_mis_cnt
"

    if ! $_blmatch_mkfname;then  
        wmessage
        wmessage "@$LINENO (${FUNCNAME[0]}) WARNING: # of files don't match up between $( basename $fmkfname_nop ) and file counts."
        wmessage "$_strnum"
        wmessage
    fi

    return
}

sherlock_findcorruptedtar(){
    #description:   find corrupted tar files ("daily" files" 
    local arrdexpcoll=($( printf "$dexp/%s\n" ${arrcollpost[@]} ))
    local dexpcoll ftar
    #global variable
    arrftar=()
    arrftar_crpt=()

    for coll in ${arrcollpost[@]};do
        [[ -d $dexpcoll ]] && arrftar+=($( find $dexpcoll/* -maxdepth 0 -type f -name '*.nc4.tar' 2>/dev/null | grep daily ))
    done 

    #todo:  check if tar is corrupted
    local cnt=0
    for ftar in ${arrftar[@]};do 
        tar -tf $ftar >> /dev/null 2>&1
        local status_tar=$?
        (( $status_tar > 0 )) && arrftar_crpt+=( $ftar ) 
    done 
            
    return
}

write_table(){
    local _ftmp=$cdir/tmp_${FUNCNAME[0]}_1
    local _thishost_up=$( echo $hstshort | tr '[:lower:]' '[:upper:]' )
    if [[ "$hstshort" == "pfe" ]];then 
        local _darchost="LFE"
    elif [[ "$hstshort" == "dis" ]];then 
        local _darchost="ARC"
    fi

    [[ -f $_ftmp ]] && rm -f $_ftmp

    if [[ -n $numfarc && -n $numfarc_fmomfout_cnt && -n $numfarc_mom_cnt && -n $numfarc_rst_cnt ]];then
        echo  "                     "\;"Calculated"\;"Calc_Save"\;"$_thishost_up"\;"$_darchost" >> $_ftmp 
        echo  "  total # of outputs:"\;$numftotal_calc\;$numfsave_tot\;$numfexp_total_cnt\;$numfarc_fmomfout_cnt >> $_ftmp 
        echo  "        MAPL monthly:"\;$numfmonmapl_calc\;$numfsave_mpl\;$numfexp_monmapl_cnt\;$numfarc_monmapl_cnt >> $_ftmp 
        echo  "             monthly:"\;$numfmonpost_calc\;$numfsave_mon\;$numfexp_monpost_cnt\;$numfarc_monpost_cnt >> $_ftmp 
        echo  "             diurnal:"\;$numfdiu_calc\;$numfsave_diu\;$numfexp_diu_cnt\;$numfarc_diu_cnt >> $_ftmp 
        echo  "               daily:"\;$numfday_calc\;$numfsave_day\;$numfexp_day_cnt\;$numfarc_day_cnt >> $_ftmp 
        echo  "           dailymean:"\;$numfdmn_calc\;$numfsave_dmn\;$numfexp_dmn_cnt\;$numfarc_dmn_cnt >> $_ftmp 
        echo  "         MOM outputs:"\;$numfmom_calc\;$numfsave_mom\;$numfexp_mom_cnt\;$numfarc_mom_cnt >> $_ftmp 
        echo  "            restarts:"\;-\;-\;$numfexp_rst_cnt\;$numfarc_rst_cnt >> $_ftmp 
    else
        echo  "                     "\;"Calculated"\;"Calc_Save"\;"$_thishost_up" >> $_ftmp 
        echo  "  total # of outputs:"\;$numftotal_calc\;$numfsave_tot\;$numfexp_total_cnt >> $_ftmp 
        echo  "        MAPL monthly:"\;$numfmonmapl_calc\;$numfsave_mpl\;$numfexp_monmapl_cnt >> $_ftmp 
        echo  "             monthly:"\;$numfmonpost_calc\;$numfsave_mon\;$numfexp_monpost_cnt >> $_ftmp 
        echo  "             diurnal:"\;$numfdiu_calc\;$numfsave_diu\;$numfexp_diu_cnt >> $_ftmp 
        echo  "               daily:"\;$numfday_calc\;$numfsave_day\;$numfexp_day_cnt >> $_ftmp 
        echo  "           dailymean:"\;$numfdmn_calc\;$numfsave_dmn\;$numfexp_dmn_cnt >> $_ftmp 
        echo  "         MOM outputs:"\;$numfmom_calc\;$numfsave_mom\;$numfexp_mom_cnt >> $_ftmp 
        echo  "            restarts:"\;-\;-\;$numfexp_rst_cnt >> $_ftmp 
    fi

    #todo:  write it
    wmessage
    msg_wheader_userdefined 55 - "File Counts"

    if (( $writetofile == 1 ));then
        rev $_ftmp | column -t -s';' | rev  >> $fmessage
    else
        rev $_ftmp | column -t -s';' | rev  
    fi
    wmessage
    ((  ${#arrpst[@]} >  0 )) && wmessage "Incompleted gcm_post: ${#arrpst[@]} "
    (( $blrmarchready == 0 )) && wmessage "      Missing on $_thishost_up: $numfexp_mis_cnt"
    wmessage "      Missing on $_darchost: $numfarc_mis_cnt"
    wmessage "Timestamp/Size Differ: $numfmissing"
    $bldelhold && wmessage "  YYYYMM Dir Removed: ${#arrdelyyyymm[@]}"
    wmessage 

     local _numdiff=$( cat $fdiff_nop 2>/dev/null | wc -l ) 

    if (( $_numdiff > 0 ));then 
        local _strmsg="\
@$LINENO ***WARNING: These files do not exist in DEXP/DARC ( diff mkfname vs. file living in dexp/darc )***
$( cat $fdiff_nop 2>/dev/null | sort -V | sed 's#^#    #g' )
 Total = $( cat $fdiff_nop | wc -l ) 
"
        wmessage "$_strmsg"
        wmessage
    fi


    
    [[ -f $_ftmp ]] && rm -f $_ftmp

    return
}

write_table_collection(){
    local arrthiscoll=($( printf '%s\n' ${arrcoll[@]} | grep -v _1mo_glo_ | sort -V -k3 -t'_' ))
    local _ftmp=$cdir/tmp_${FUNCNAME[0]}_1
    local numleadspace1="+8"
    local numleadspace2="+10"
    local numleadspace3="-35"
    
    [[ -f $_ftmp ]] && rm -f $_ftmp
        
    local _arrfexist=($( find $dexp/* -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*.nc4*" | grep -vE "$collsst|scratch" | sort -V ))
    echo "$( printf "%${numleadspace3}s" "Collection ( ${#arrthiscoll[@]} )" )"\;"$( printf "%${numleadspace2}s" "Total" )"\;"$( printf "%${numleadspace1}s" "monthly" )"\;"$( printf "%${numleadspace1}s" "dirunal" )"\;"$( printf "%${numleadspace1}s" "daily" )"\;"$( printf "%${numleadspace1}s" "dailymean" )" >> $_ftmp

    for thiscoll in ${arrthiscoll[@]};do 
        #arrfthiscoll=($( printf '%s\n' ${arrfsave[@]} | xargs -i basename {} | sort -V | grep $thiscoll ))
        arrfthiscoll=($( printf '%s\n' ${_arrfexist[@]} | xargs -i basename {} | sort -V | grep $thiscoll ))
    
        nummon=$( printf '%s\n' ${arrfthiscoll[@]} | grep "\.monthly\."   | wc -l | xargs -i bash -c '(( {} == 0 )) && echo "-" || echo {} ')
        numday=$( printf '%s\n' ${arrfthiscoll[@]} | grep "\.daily\."     | wc -l | xargs -i bash -c '(( {} == 0 )) && echo "-" || echo {} ')
        numdiu=$( printf '%s\n' ${arrfthiscoll[@]} | grep "\.diurnal\."   | wc -l | xargs -i bash -c '(( {} == 0 )) && echo "-" || echo {} ')
        numdmn=$( printf '%s\n' ${arrfthiscoll[@]} | grep "\.dailymean\." | wc -l | xargs -i bash -c '(( {} == 0 )) && echo "-" || echo {} ')
    
        echo "$( printf "%${numleadspace3}s" "$thiscoll" )"\;"$( printf "%${numleadspace2}s" ${#arrfthiscoll[@]} )"\;"$( printf "%${numleadspace1}s" "$nummon" )"\;"$( printf "%${numleadspace1}s" $numdiu )"\;"$( printf "%${numleadspace1}s" $numday )"\;"$( printf "%${numleadspace1}s" $numdmn )" >> $_ftmp
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


create_shiftc_fin() {
    #description:   create shiftc batch input file
    local fin=$1;shift
    local _hst=$1;shift
    local _dout=$1;shift
    local _arrinput=( "$@" )
    local fmiss
   
    #note:  if you don't remove this, divide_fin will use old shiftin file. 
    [[ -f $fin ]] && rm -f $fin

    for fmiss in ${_arrinput[@]};do
        local fname=$( echo $fmiss | rev | cut -d'/' -f1 | rev  )
        local frst=$( echo $fname | cut -d'.' -f1 )

        if [[ $frst == $strrst ]];then
            local dcoll=$strrst
        elif [[ $frst == "$strmom_search"* ]];then
            local dcoll=$strmom
        else
            local dcoll=$( echo $fname | cut -d'.' -f2 )
        fi

        #todo:  if fname is already being transfered, skip writing it
        if (( ${#arrfsft_running[@]} > 0 ));then
            grep $fname ${arrfsft_running[@]} >>/dev/null 2>&1
            local _status_grep=$?

            (( $_status_grep == 0 )) && continue 
        fi

        echo $fmiss $_hst:$_dout/$dcoll/$fname >> $fin
    done
    return
}

divide_fin() {
    local _cnt=$1;shift
    local fin=$1
    local _ftmpa=tmpa
    unset _arrftmpa
    unset _arrnumline
    #local _ftmpasize=$( stat --printf="%s" $_ftmpa )
    local _arrfexp=($( cat $fin | cut -d' ' -f1 ))
    local _totalsize=0
    local _cntf=0
    
    [[ -f $fin ]] && cp -p $fin $_ftmpa

    #todo:  figure out linenumbers
    for fexp in ${_arrfexp[@]};do
        #todo:  calculate total size of files that will be transferred at once
        local fexp_size=$( stat --printf="%s" $fexp )
        _totalsize=$(( _totalsize + fexp_size ))
        _cntf=$(( _cntf + 1 ))

        if (( $_totalsize >= $limsize )) || [[ "$fexp" == ${_arrfexp[-1]} ]]; then
            _arrnumline+=( $_cntf )
            _totalsize=0
            _cntf=0
        fi
    done

    #todo:  get first numline from fout and place in *_fdiv file
    #       until ftmpa file size become 0. 
    for numline in ${_arrnumline[@]};do
        local _cnttwodig=$( printf '%02g' $_cnt )
        local _fdiv=${fin}_${numline}_$_cnttwodig
  

        if [[ "${arrfsft_running[@]}" =~ "$_fdiv" ]];then
            _cnt=$(( _cnt + 1 ))
            _cnttwodig=$( printf '%02g' $_cnt )
            _fdiv=${fin}_${numline}_$_cnttwodig
        fi
       
        if [[ -f $_ftmpa ]];then 
            [[ -f $_fdiv ]] && rm -f $fdiv
            cat $_ftmpa | head -$numline >> $_fdiv
            sed -i.bak '1,'$numline'd' $_ftmpa
            _arrftmpa+=( $_fdiv )
            
            #todo:  remove tmp files if its size is zero 
            _ftmpasize=$( stat --printf="%s" $_ftmpa )
            if (( $_ftmpasize == 0 ));then 
                rm -f $_ftmpa $_ftmpa.bak
            else
                _cnt=$(( _cnt + 1 ))
            fi
        else
            break
        fi
    done
    
    echo ${_arrftmpa[@]} 
}

prepare_archiving() {

    ! $blheader && writeheader && blheader=true

    #todo:  mk collection dir if missing
    #note:  After 3mon run, some dir may be delete depending on which outputs are going to stay on pfe
    printf "$dexp/%s\n" ${arrcoll[@]} | xargs -i bash -c "[[ -f {} ]] && rm -f {} && mkdir -p {}"

    #todo:  find mapl monthly partial files that are NOT in dexp/coll
    find_partialinhold

    #todo:  mv mapl monthly partial files
    if (( ${#arrmvpartial[@]} > 0 ));then 
        if [[ "$hstshort" == "pfe" ]] || $blnode_nas;then
            ! $blheader && writeheader && blheader=true
            move_partial
            find_partialinhold
        fi
    fi

#    #todo:  move mapl monthly files
#    #note: 02/04/2026 - gcm_post seems to move mple monthly outputs
#    move_maplmon

    #todo:  get output file names & its size on lou
    ! $bldarcsame && getfname_arc

    #todo:  get output file names & its size on pfe
    getfname_exp
    
    [[ ! -f $fexistarc ]] && die "$fexistarc does not exists"
    [[ ! -f $fexistexp ]] && die "$fexistexp does not exists"

    #todo:  create file with existing geosgcm_sst output file name 
    [[ -f $fexistexp_sst ]] && rm -f $fexistexp_sst
    find $dexp/* -maxdepth 1 -mindepth 1 -type f -name "$fcstdate*.nc*" 2>/dev/null | \
        grep $collsst | grep -v ".monthly.|.daily." | \
        xargs -i stat --printf="%Y %s %n\n" {} > $fexistexp_sst
    
    cat $fexistexp_sst | sed "s#$dexp/##g" >| $fexistexp_sst_nop

    #todo:  find missing and existing output files on arc. arrfmiss 
    #       and arrfdelrdyexp, global arrays, are returned
    get_fmiss_fexist

#    #ref:   s2smkdymean.20240908ens3.atm_inst_6hr_glo_L720x361_p49.completed
#    arrmkdy_fmiss=($( printf '%s\n' ${arrcolldymean[@]} | xargs -i bash -c "[[ ! -f $dutl/$strmkdy.${fcstdate}${ensm}.{}.completed ]] && echo {}" ))
    arrmkdy_fmiss=($( printf '%s\n' ${arrfdymean[@]} | \
        while read fname ;do
            [[ ! -f $dexp/$fname ]] && echo $dexp/$fname
        done ))


    return
}

check_shiftc(){
    #description:   check shiftc status
    local _arrfsft=( "$@" )
    local _arrrunning=()
    local fsft

    if [[ "$hstshort" == "pfe" ]];then 
        local thiscmd="$cmd_shiftc --status=csv"
    elif [[ "$hstshort" == "dis" ]];then 
        local thiscmd="$cmd_sup shiftc --status=csv"
    fi

    for fsft in ${_arrfsft[@]};do 

        local  numln_beg=$( grep -n "$strbtc $fsft" $ferr 2>/dev/null | tail -1 | cut -d':' -f1 )
        [[ -n $numln_beg ]] && local blisnumber=$( misc_isinteger $numln_beg ) || local blisnumber=1

        if [[ -n $numln_beg ]] && (( $blisnumber == 0 )) && (( $numln_beg > 0 ));then
            local        sid=$( cat $ferr | tail -n +$numln_beg | grep "$strsid" 2>/dev/null | head -1 | rev | cut -d' ' -f1 | rev )
            local sid_status=$( $thiscmd | grep $sid | cut -d',' -f2 )

            if [[ "$sid_status" == "run" || "$sid_status" =~ "run"* ]];then 
                _arrrunning+=( $sid )

            elif [[ "$sid_status" == "done" ]];then 
                mv $fsft ${fsft}_${sid_status}
            fi
        fi
    done 

    echo "${_arrrunning[@]}"
}

check_rsync(){
    #description:   check which rsync is running
    local _arrfin=( "$@" ) 
    local _arrscr_running=($( screen -ls | grep $scrnamersync_base 2>/dev/null | sed 's#\t##g' | cut -d'(' -f1 | cut -d'.' -f2 ))
    local _arrid_running=($( printf '%s\n' ${_arrfin[@]} | rev | cut -d'_' -f1-2 | rev ))
    local _arrscr=($( printf "${scrnamersync_base}%s\n" ${_arrid_running[@]} )) 
    local _id_running
    local _arrrunning=()


#wmessage \@$LINENO 
#ahand_print ${_arrfin[@]} 
#wmessage 
#ahand_print ${_arrscr_running[@]}     
#wmessage 
#ahand_print ${_arrid_running[@]} 
#wmessage 
#exit
    for _id_running in ${_arrid_running[@]} ;do 
        local _numrunning=$( printf "%s\n" ${_arrscr_running[@]} | grep ${scrnamersync_base}${_id_running} 2>/dev/null | wc -l )
        local _thisftmp=${frsyncfin_base}_${_id_running}
        local _thisferr=${ferrrsync_base}${_id_running}
        local _thisfscr=${scrnamersync_base}${_id_running}.sh
        if (( $_numrunning == 0 ));then 
            [[ -f $_thisftmp ]] && mv $_thisftmp $cdir/tmp/${_thisftmp}_done
            [[ -f $_thisfscr ]] && mv $_thisfscr $cdir/tmp/${_thisfscr}_done

            if [[ -s $_thisferr ]];then 
                mv $_thisferr $cdir/tmp/${_thisferr}_done
            else
                rm -f $_thisferr 2>/dev/null
            fi 
        else
            _arrrunning+=( $_thisftmp ) 
        fi
    done 

    echo "${_arrrunning[@]}"
}

cooking_hold() {
    #description:   determine which holding/coll/yyyymm dir will be deleted
    #               based on output files exist on PFE. 
    #       note:   Due to setup for v3 hindcast, some outputs are deleted from 
    #               PFE, and this will cause some of YYYYMM to be remained in
    #               holding dir. 
    local collyyyymm
    local _arrdelyyyymm=()
    
    [[   -f $fdelyyyymm ]] && rm -f $fdelyyyymm
    #[[ ! -f $fdelout    ]] && touch $fdelout

    #todo:  check gcmpost outputs (monthly, daily, diurnal)
    for collyyyymm in ${arrcollyyyymm_monpost[@]};do

        local coll=$( echo $collyyyymm | cut -d'/' -f1 )
        local yyyymm=$( echo $collyyyymm | cut -d'/' -f2 )

        #todo:  find if collection has diurnal dir.
        local ind_diu=$( IndexOf $coll ${arrcollpost_diu[@]} )

        if [[ -d $dexp/$coll ]];then 
            local blstat=0


            #todo: check daily
            [[ ! -f $dexp/$coll/$fcstdate.$coll.daily.$yyyymm.nc4.tar ]] && \
            blstat=$(( blstat + 1 ))

            [[ ! "${arrfdelrdyexp[@]}" =~ *"$dexp/$coll/$fcstdate.$coll.daily.$yyyymm.nc4.tar"* ]] && : || blstat=$(( blstat + 1 ))

            #todo: check monthly 
            [[ ! -f $dexp/$coll/$fcstdate.$coll.monthly.$yyyymm.nc4 ]] && \
            blstat=$(( blstat + 1 ))

            [[ ! "${arrfdelrdyexp[@]}" =~ *"$dexp/$coll/$fcstdate.$coll.monthly.$yyyymm.nc4"*   ]] && : || blstat=$(( blstat + 1 ))
         
            #todo: check diurnal
            if (( $ind_diu > 0 ));then
                [[ ! -f $dexp/$coll/$fcstdate.$coll.diurnal.$yyyymm.nc4 ]] && \
                    blstat=$(( blstat + 1 ))

                [[ ! "${arrfdelrdyexp[@]}" =~ *"$dexp/$coll/$fcstdate.$coll.diurnal.$yyyymm.nc4"*   ]] && : ||  blstat=$(( blstat + 1 ))
            fi

            (( $blstat == 0 )) && _arrdelyyyymm+=( $dexp/holding/$coll/$yyyymm )
        fi
    done

    #todo:  check mapl monthly
    for collyyyymm in ${arrcollyyyymm_monmapl[@]};do

        local coll=$( echo $collyyyymm | cut -d'/' -f1 )
        local yyyymm=$( echo $collyyyymm | cut -d'/' -f2 )

        #todo:  find if collection has diurnal dir.
        local ind_diu=$( IndexOf $coll ${arrcollpost_diu[@]} )

        if [[ -d $dexp/$coll ]];then 
            local blstat=0

            #20150110.glc_tavg_1mo_glo_L720x361_slv.20150201_0000z.nc4
            [[ ! -f $dexp/$coll/$fcstdate.$coll.${yyyymm}01_0000z.nc4 ]] && \
            blstat=$(( blstat + 1 ))

            grep $fcstdate.$coll.${yyyymm}01_0000z.nc4 $fexistarc >/dev/null
            (( $? > 0 )) && blstat=$(( blstat + 1 ))

            if (( $blstat == 0 ));then
                _arrdelyyyymm+=( $dexp/holding/$coll/$yyyymm )
            fi
        fi
    done

    #todo:  find YYYYMM to be deleted based on existing files on archive dir  instead of dexp
    arrdelyyyymm=($( printf '%s\n' ${_arrdelyyyymm[@]} | sort -V | uniq ))

    (( ${#arrdelyyyymm[@]} > 0 )) && printf '%s\n' ${arrdelyyyymm[@]} >> $fdelyyyymm

    return 
}

cooking_archiving() {
    #description:   execute shift batch archiving
    local arrfin=( "$@" )
    local _statushere=999
    local fin
    
    if [[ "$hstshort" == "pfe" ]];then  
        local thiscmd="$cmd_shiftc --no-cron --no-mail --wait -f -d"
    elif [[ "$hstshort" == "dis" ]];then 
        local thiscmd="$cmd_sup shiftc --no-cron --no-mail --wait -f -d"
    fi

    if ssh -q $hostarc true; then 
        local _blhstavail=true
    else
        local _blhstavail=false
    fi

    if $_blhstavail;then 
        for fin in ${arrfin[@]};do
            local _status=9999

            if [[ -f $fin ]];then
                local numfarchive=$( cat $fin | wc -l )
                wmessage "$(date +'%m/%d/%Y %H:%M' ) ... archiving $numfarchive files ( $( basename $fin ) )"
                wmessage

                if (( $writetofile == 1 ));then
                    $thiscmd < $fin >> $fmessage 2>&1
                    local _status=$?
                else
                    $thiscmd < $fin
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
        _statushere=0
    else
        _statushere=1
    fi

    return $_statushere
}

cooking_archiving_nowait() {
    #description:   execute shift batch archiving
    local _cntsftc=$1;shift
    local arrfin=( "$@" )
    local arrfin_running=( )
    local _statushere=999
    local fin
        
    if $blnode_nas;then
        local thiscmd="$cmd_shiftc --streams=1 --hosts=1 --no-cron --no-mail -f -d"
    elif [[ "$hstshort" == "pfe" ]];then
        local thiscmd="$cmd_shiftc --no-cron --no-mail -f -d" 
    elif [[ "$hstshort" == "dis" ]];then
        local thiscmd="$cmd_sup shiftc --no-cron --no-mail -f -d" 
    fi

    if [[ "$hstshort" == "dis" && "$hostarc" == "lfe" ]];then 
        $cmd_sup ssh $hostarc test -d $darc_parent
        local _status_testd=$?

        if (( $_status_testd == 0 )) ; then 
            local _blhstavail=true
        else
            local _blhstavail=false
        fi
    else
        if ssh -q $hostarc true; then 
            local _blhstavail=true
        else
            local _blhstavail=false
        fi
    fi

    wmessage 

    if $_blhstavail;then 
        for fin in ${arrfin[@]};do
            
            #todo:  break if # of shiftc executed is more than limit
            (( $_cntsftc > $limsftc )) && break 

            #todo:  check if this fin is running
            if [[ -f $fin ]];then 
                local arrsid=($( check_shiftc $fin ))
                (( ${#arrsid[@]} > 0 )) && continue
            else
                continue
            fi

            if [[ -f $fin ]];then
                local numfarchive=$( cat $fin | wc -l )
                wmessage "$(date +'%m/%d/%Y %H:%M' ) ... archiving $numfarchive files ( $( basename $fin ) )"
                wmessage "shiftc batch file = $( basename $fin )"


                if [[ -n $thiscmd ]];then 
                    if (( $writetofile == 1 ));then
                        wmessage $hstname
                        $thiscmd < $fin >> $fmessage 2>&1
                        _cntsftc=$(( _cntsftc + 1 ))
                    else
                        wmessage $hstname
                        $thiscmd < $fin
                        _cntsftc=$(( _cntsftc + 1 ))
                    fi
                else
                    if (( $writetofile == 1 ));then
                        ssh $hstshort "hostname; $cmd_shiftc --no-cron --no-mail -f -d < $fin >> $fmessage 2>&1"
                        _cntsftc=$(( _cntsftc + 1 ))
                    else
                        ssh $hstshort "hostname; $cmd_shiftc --no-cron --no-mail -f -d < $fin"
                        _cntsftc=$(( _cntsftc + 1 ))
                    fi
                fi

                arrfin_running+=( $fin )
                wmessage
                wmessage

            else
                wmessage "$( basename $fin ) does not exists. Skip archiving."
            fi
        done 
                
        local _arrfin_notinuse=($( printf '%s\n' ${arrfin_running[@]} ${arrfin[@]} | sort -V | uniq -u ))
        (( ${#_arrfin_notinuse[@]} > 0 )) && rm -f ${_arrfin_notinuse[@]} 2>/dev/null
        _statushere=0
    else
        wmessage "$( date +'%Y%m%d %H:%M') $hostarc is Unavailable"
        _statushere=1
    fi

    return $_statushere
}

cooking_archiving_rsync_dis(){
    #description:   archive output with rsync
    local fin=$1

    #local ftmp=_tmp_${strscr}_${FUNCNAME[0]}_
    local _thisdfout=${darc%$fcstdate/*}
    local _statushere=999
    local _coll

    rm -f ${frsyncfin}* 2>/dev/null

    if ssh -q $hostarc true; then 
        local _blhstavail=true
    else
        local _blhstavail=false
    fi

    if $_blhstavail;then 
        #todo:  get collections from fin
        local _arrcoll=($( cat $fin | rev | cut -d'/' -f2 | rev | sort -V | uniq ))

        #note:  sort by size, small to large
        local _arrcoll_sortedbysize=($( printf "$dexp/%s\n" ${_arrcoll[@]} | xargs -i bash -c "du -a {}/* 2>/dev/null | sort -n | head -1" | sort -n | rev | cut -d'/' -f2 | rev  ))
        
        local _cntcoll=1
        #todo:  archive by collection
        for _coll in ${_arrcoll_sortedbysize[@]};do
            local thisftmp=${frsyncfin}_$_coll

            [[ -f $thisftmp ]] && rm -f $thisftmp

            #todo:  create ftmp file
            local arrfcoll=($( grep $_coll $fin | cut -d' ' -f1 ))
            local totsize=0
            for fcoll in ${arrfcoll[@]};do
                local thisfsize=$( stat --printf=%s $fcoll )
                local dfcst=${fcoll%$fcstdate/*}
                local dafter=${fcoll//$dfcst/}
                local thisdarch=${dfcst}./${dafter}
                echo  $thisdarch >> $thisftmp

                totsize=$(( totsize + thisfsize ))
            done
            local totsize_fmt=$( numfmt --to=iec $totsize )

            if [[ -s $thisftmp ]];then
                wmessage "... $(date +'%I:%M%P') ... $( printf "%02g" $_cntcoll ) of $( printf "%02g" ${#_arrcoll_sortedbysize[@]}) ... $_coll ( #files = $( cat $thisftmp | wc -l ); total size = $totsize_fmt ) ..."

                if [[ "$hstshort" == "pfe" ]];then 
                    rsync -avzRPqW --files-from=$thisftmp / $hostarc:$_thisdfout
                    local status_rsync=$?

                elif [[ "$hstshort" == "dis" ]];then 

                    if [[ "$hostarc" == "lfe" ]];then 
                        $cmd_sup rsync -azRPqW --files-from=$thisftmp / $hostarc:$_thisdfout
                        local status_rsync=$?
                    else
                        rsync -azRPqW --files-from=$thisftmp / $hostarc:$_thisdfout
                        local status_rsync=$?
                    fi

                fi
                (( $status_rsync == 0 )) && [[ -f $thisftmp ]] && rm -f $thisftmp
            fi

            _cntcoll=$(( _cntcoll + 1 ))
        done 
        _statushere=0
    else
        _statushere=1
    fi
    return $_statushere
}

cooking_archiving_rsync(){
    #description:   archive output with rsync
    local _arrfin=( "$@" ) 

    local _arrfin_rsync=($( find * -maxdepth 0 -type f -name "${frsyncfin_base}_*" )) 
    local _dexp_dad=${dexp%$fcstdate/*}
    local _thisdfout=${darc%$fcstdate/*}
    local _statushere=999
    local _arralphabets=( {a..z} ) 
    local _fin _arrftmp=() 
    local _ftmp
    local arrfin_running=()

    if [[ "$hstshort" == "dis" && "$hostarc" == "lfe" ]];then 
        $cmd_sup ssh $hostarc test -d $darc_parent
        local _status_testd=$?

        if (( $_status_testd == 0 )) ; then 
            local _blhstavail=true
        else
            local _blhstavail=false
        fi
    else
        if ssh -q $hostarc true; then 
            local _blhstavail=true
        else
            local _blhstavail=false
        fi
    fi

    if ! $_blhstavail;then 
        wmessage "$( date +'%Y%m%d %H:%M') $hostarc is Unavailable" 
        _statushere=1
        return $_statushere
    fi

    local _arrfrsc_running=($( check_rsync "${_arrfin_rsync[@]}" ))

    if (( ${#_arrfrsc_running[@]} >= $limrsync ));then 
        rm -f ${_arrfin[@]} 
        return 0
    fi

    if (( ${#_arrfrsc_running[@]} > 0 ));then 
        local _numfin_ind=$( printf '%s\n' ${_arrfrsc_running[@]} | rev | cut -d'_' -f1 |rev  | xargs -i echo "{} * 1" | bc  | sort -V | uniq  | tail -1 ) 
    else
        local _numfin_ind=0
    fi

    #todo:  write batch input files for rsync using shiftc input files
    for _fin in ${_arrfin[@]};do
        _numfin_ind=$(( _numfin_ind + 1 )) 
        
        local _numfou=$( basename $_fin  | cut -d'_' -f3 )
        local _numfin_new=$( printf '%02g\n' $_numfin_ind ) 
        local thisftmp=${frsyncfin}_${_numfou}_${_numfin_new}

        [[ -f $thisftmp ]] && rm -f $thisftmp

        cat $_fin | cut -d' ' -f1 | sed "s#$_dexp_dad#${_dexp_dad}./#g" >> $thisftmp 2>/dev/null
        [[ -s $thisftmp ]] && _arrftmp+=( $thisftmp )
    done

    rm -f ${_arrfin[@]} 2>/dev/null

    local _cntrsync=$(( ${#_arrfrsc_running[@]} + 1 ))

#wmessage \@$LINENO $_cntrsync $limrsync
#exit
    for _ftmp in ${_arrftmp[@]};do

        (( $_cntrsync > $limrsync )) && break 

        wmessage "... $(date +'%I:%M%P') ... $( printf "%02g" $_cntrsync ) of $( printf "%02g" $limrsync ) ... $( basename $_ftmp ) ( #files = $( cat $_ftmp | wc -l ) ) ..."
        
        local _str1=$( basename $_ftmp | rev | cut -d'_' -f1-2 | rev ) 
        local scrname=${scrnamersync_base}$_str1

        if [[ "$hstshort" == "pfe" ]];then 

            local thisferr=$cdir/${ferrrsync_base}$_str1
            [[ -f $thisferr ]] && rm -f $thisferr

            screen -dmS ${scrname} bash -c "ssh -q $hostarc rsync -avzRPqW --files-from=$_ftmp / $_thisdfout >> $thisferr 2>&1" 

        elif [[ "$hstshort" == "dis" ]];then 

            local _blcodeopt2=false
            if $_blcodeopt2;then 
                #todo:  make gcmarch.sh submittable to a computing node
                #note:  this code was supposed to be used if dirac was available
                #       to keep outputs. It is no longer available but
                #       keep this code here.

                local _fsed=$cdir/${FUNCNAME[0]}_sedfile
                local _scr_def=$cdir/$strscr.sh 
                #local _thisscr=$cdir/${strscr}_${fcstdate}${ensm}_$_str1.sh
                local _thisscr=$cdir/${scrnamersync_base}$_str1.sh

                [[ -f $_fsed ]] && rm -f $_fsed

                cp -p $_scr_def $_thisscr

                local   _str_jname="$( grep "#SBATCH --job-name=A$fcstdate$ensm" $_thisscr )" 
                local _str_fstdout="$( grep "#SBATCH --output=stderr_$strscr"    $_thisscr )" 

                cat > $_fsed << EOF  
s%$_str_jname%${_str_jname}_${_str1}%g
s%${_str_fstdout}%${_str_fstdout}_${_str1}%g
s%@\<INPUTFILE\>%${_ftmp}%g
s%@\<STRSCR\>%${strscr}_${_str1}%g
s%@\<STRID\>%${_str1}%g
s%@\<GCMARCH_HOSTARC\>%${hostarc}%g
EOF

                sed -i.bak -f $_fsed $_thisscr

                [[ -f $_fsed          ]] && rm -f $_fsed
                [[ -f ${_thisscr}.bak ]] && rm -f ${_thisscr}.bak

                $cmd_submit $_thisscr
                
            elif $blarc2os; then
                
                #todo:  archive output from the current host to a host outside (i.e. NCCS -> NAS)
                #       using sup rsync
                local     _thisscr=$cdir/$( basename "$0" ) 
                local   _str_jname="$( grep "#SBATCH --job-name=A$fcstdate$ensm" $_thisscr )" 
                local _str_fstdout="$( grep "#SBATCH --output=stderr_$strscr"    $_thisscr )" 

                #local         _scr=${strscr}_${fcstdate}${ensm}_$_str1.sh
                #local        _ferr=stderr_${strscr}_${fcstdate}${ensm}_$_str1
                #local      _screen=${strscr}_${fcstdate}${ensm}_$_str1
                
                local         _scr=${scrnamersync_base}$_str1.sh
                local        _ferr=${ferrrsync_base}$_str1
                local      _screen=${scrnamersync_base}$_str1

                [[ -f     $_scr  ]] && rm -f     $_scr
                [[ -f tmp_$_scr  ]] && rm -f tmp_$_scr
                [[ -f     $_ferr ]] && rm -f     $_scr

                grep    "bash"    $_thisscr | head -1        >> tmp_$_scr
                grep -w "#SBATCH" $_thisscr | grep -v "grep" >> tmp_$_scr
                echo ""                                      >> tmp_$_scr
                echo "$cmd_sup rsync -azRPW --quiet --files-from=$_ftmp / $hostarc:$_thisdfout" >> tmp_$_scr

                cat tmp_$_scr | sed "s%$_str_jname%${_str_jname}_${_str1}%g" | sed "s%${_str_fstdout}%${_str_fstdout}_${_str1}%g" >> $_scr

                [[ -f tmp_$_scr ]] && rm -f tmp_$_scr

                #$cmd_submit $_scr
              
                chmod 755 $_scr 
                
                screen -dmS $_screen bash -c "./$_scr >> $_ferr 2>&1"
            fi
        fi

        arrfin_running+=( $_ftmp )
        _cntrsync=$(( _cntrsync + 1 ))

    done 

    #todo:  delete unused shiftcin files
    local _arrfin_notinuse=($( printf '%s\n' ${arrfin_running[@]} ${_arrftmp[@]} | sort -V | uniq -u ))
    (( ${#_arrfin_notinuse[@]} > 0 )) && rm -f ${_arrfin_notinuse[@]} 2>/dev/null
    _statushere=0


    return $_statushere
}


cooking_archiving_bbscp(){
    #description:   archive output with rsync
    local fin=$1
    local ftmp=_tmp_${strscr}_${FUNCNAME[0]}_
    local _thisdfout=${darc%$fcstdate/*}
    local _statushere=999
    local _coll

    rm -f $ftmp* 2>/dev/null

    #todo:  get collections from fin
    local _arrcoll=($( cat $fin | rev | cut -d'/' -f2 | rev | sort -V | uniq ))

    #note:  sort by size, small to large
    local _arrcoll_sortedbysize=($( printf "$dexp/%s\n" ${_arrcoll[@]} | xargs -i bash -c "du -a {}/* 2>/dev/null | sort -n | head -1" | sort -n | rev | cut -d'/' -f2 | rev  ))
    
    local _cntcoll=1
    #todo:  archive by collection
    for _coll in ${_arrcoll_sortedbysize[@]};do

        ssh -q $hostarc test -d $darc/$_coll 
        local _status_testd=$?
        if (( $_status_testd > 0 ));then 
            ssh -q $hostarc mkdir  -p $darc/$_coll
            ssh -q $hostarc chmod 755 $darc/$_coll
        fi

        #todo:  create ftmp file
        local arrfcoll=($( grep $_coll $fin | cut -d' ' -f1))
        local totsize=0
        for fcoll in ${arrfcoll[@]};do
            local thisfsize=$( stat --printf=%s $fcoll )
            totsize=$(( totsize + thisfsize ))
        done

        local totsize_fmt=$( numfmt --to=iec $totsize )

        wmessage "  $( date +'%I:%M%P' )    $( printf "%02g" $_cntcoll ) of $( printf "%02g" ${#_arrcoll_sortedbysize[@]}) ... $_coll ( #files = $( cat $fin | wc -l ); total size = $totsize_fmt ) ..."

        for fcoll in ${arrfcoll[@]};do
            wmessage "                          $( basename $fcoll ) "

            archive_status=($( bbscp -r3 -z $fcoll dirac:$darc/$_coll 2>/dev/null )) 
            local status_bbscp=${archive_status[3]}

            if (( $status_bbscp != "OK" )); then 
                wmessage "                              Archiving FALIED"
            fi

        done 

        _cntcoll=$(( _cntcoll + 1 ))
    done 

    return $_statushere 
}

move_maplmon(){
    #description:   move monthly files from holding to dexp/collection
    local _dhold=$dexp/holding
    local coll

    for coll in ${arrcollmapl[@]};do
        mv $_dhold/$coll/$fcstdate.$coll.*.nc* $dexp/$coll/ 2>/dev/null
        local _arrfmonmapl=($( find $_dhold/$coll/* -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.$coll.*.nc*"  2>/dev/null ))
        if (( ${#_arrfmonmapl[@]} > 0 ));then 
            mv ${_arrfmonmapl[@]} $dexp/$coll/
        fi
    done

    return
}

move_partial(){
    #description:   mv mapl monthly partial files from holding to dexp/collection
    local fhold

    for fhold in ${arrmvpartial[@]};do
        local fhold_bname=$( basename $fhold ) 
        local coll=$( echo $fhold_bname | cut -d'.' -f2 ) 

        #mv $fhold $dexp/$coll/ 2>/dev/null
        rsync -az $fhold $dexp/$coll/ 2>/dev/null
        local status_mv=$?
        if (( $status_mv > 0 ));then 
            wmessage "... rsyncing $fhold_bname failed"
        fi
    done 

    return
}

check_post(){
    #description:   find missing diurnal file and gcm_post.*.j* script

    local _str=$1;shift
    local _arrcoll=( "$@" )
    local _arrfname=()
    local arrfname1=()
    local arrf=()
    
    #file names:    
    #19840101.aer_inst_3hr_glo_l720x361_slv.diurnal.198402.nc4
    #$fcstdate.$coll.diurnal.yyyymm.nc4
    #gcm_post.ocn_inst_6hr_glo_l1440x721_z50.j198402
    #gcm_post.$coll.jyyyymm

    #todo:  create filenames, which are suppoed to exists, based on collection and yyyymm
    for coll in ${_arrcoll[@]};do
        local fname1=$dexp/$coll/$fcstdate.$coll.${_str}.
        if [[ "$_str" == daily ]];then 
            #_arrfname=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4.tar\n" ${arrmonthfull[@]} ))
            _arrfname=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4.tar\n" ${arrmonth[@]} ))
        else
            #_arrfname=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4\n" ${arrmonthfull[@]} ))
            _arrfname=($( printf "%s\n" $fname1 | xargs -i printf "{}%s.nc4\n"     ${arrmonth[@]} ))
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
    #description:   check outputs from gcm_post.*.j*
    local arrfpst=()
    local arr=()
    local strdiu=diurnal
    local strmon=monthly
    local strday=daily

    if (( $numfdiu_calc != $numfexp_diu_cnt ));then
        arrfpst+=($( check_post $strdiu "${arrcollpost_diu[@]}" ))
    fi
    
    if (( $numfmonpost_calc != $numfexp_monpost_cnt ));then
        arrfpst+=($( check_post $strmon "${arrcollpost[@]}" ))
    fi

    if (( $numfday_calc != $numfexp_day_cnt ));then
        arrfpst+=($( check_post $strday "${arrcollpost[@]}" ))
    fi

    arr=($( printf '%s\n' ${arrfpst[@]} | sort -V | uniq ))

    echo "${arr[@]}"
}

exp_missdhold(){
    #descrtipsion: find missing holding/coll/yyyymm dir. 
    local _arrfpst=( "$@" )
    local _arr=() 
    local _fpst

    #note:  this check has to be done every execution of this script
    #       Due to the timing of moving output from scratch dir, this 
    #       script may consider some YYYYMM dir is missing
    [[ -f $fnote ]] && rm -f $fnote

    #gcm_post.sfc_tavg_3hr_glo_L720x361_sfc.j198201
    for _fpst in ${_arrfpst[@]};do 
        local _yyyymm=$( echo $_fpst | rev | cut -d'.' -f1 | rev | cut -c2- )
        local   _coll=$( echo $_fpst | rev | cut -d'.' -f2 | rev )
        local _dholdyyyymm=$dexp/holding/$_coll/$_yyyymm 

        if [[ ! -d $_dholdyyyymm ]];then
            _arr+=( $_fpst ) 

            #todo:  write missing yyyymm dir in holding
            echo "missing: $_dholdyyyymm" >> $fnote 2>&1
        fi
    done 

    echo "${_arr[@]}" 
}

exp_mkcollgcmpost() {
    local _dexp=$1;shift
    local _coll=$1;shift
    local _yyyymm=$1
    local _fpst_org=$_dexp/post/gcm_post.j
    local     _fpst=$_dexp/post/gcm_post.$_coll.j$_yyyymm

    local _fsed=$_dexp/post/tmp_${strscr}_${FUNCNAME[0]}_sedfile

    [[ -f $_fsed ]] && rm -f $_fsed
 
    if [[ -f $_dexp/post/gcm_post_full.j && -f $_dexp/post/gcm_post_part.j ]];then 
        if (( $_yyyymm == $capric_yyyymm ));then 
            local _fpst_org=$_dexp/post/gcm_post_part.j
        else
            local _fpst_org=$_dexp/post/gcm_post_full.j
        fi
    else    
        local _fpst_org=$_dexp/post/gcm_post.j
    fi
    
    #POST_O=gcm_post.sfc_tavg_3hr_glo_L720x361_sfc.o198201
    local post_o=gcm_post.$_coll.o$_yyyymm
#s?nobackupp2?nobackupp18?g
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

exp_editfpst() {
    #description:   edit PBS statements in gcm_post.j
    local _arrfpst=( "$@" )
    local _arr=()
    
    for _fpst in ${_arrfpst[@]};do 
        cd $dexp/post
        exp_editqname $_fpst $pbsq "$pbsselect"
        _arr+=( $_fpst) 
        cd - >/dev/null
    done

    echo "${_arr[@]}"

}

cooking_configarch (){
    #description:   archive config files.
    local _dtmp=$cdir/tmp_cofing
    local _ftar=config.tar

    [[ ! -d $_dtmp        ]] && mkdir -p $_dtmp
    [[   -f $_dtmp/$_ftar ]] && rm    -f $_dtmp/$_ftar

    #+++++ cd dexp (start) +++++
    cd $dexp

    shopt -s nullglob
    local _arrfconfig1=( AGCM.rc AGCM_*.rc CAP_*.rc HISTORY_*.rc input.nml gcm_run.j \
archive/run_gcm*.sh archive/stderr_gcmarch cap_* note*_cont note_ocnpert_* message_rstmkpert_* $fnote )
    [[ -f $frstpert ]] && _arrfconfig1+=( $frstpert ) 
    shopt -u nullglob

    cd - >/dev/null
    #+++++ cd dexp ( end ) +++++
    
    local_arrfconfig=($( printf "$dexp/%s\n" ${_arrfconfig1[@]} | xargs -i bash -c "[[ -f {} ]] && echo {}" ))
    
    cp -p ${_arrfconfig[@]} $_dtmp/ 2>/dev/null

    #todo:  create config tar file
    #+++++ cd _dtmp (start) +++++
    cd $_dtmp
    tar -cf $_ftar --remove-files * 
    cd - >/dev/null 
    #+++++ cd _dtmp ( end ) +++++

    wmessage "$(date +'%m/%d/%Y %H:%M' ) ... archiving config files"
    wmessage

    if [[ "$hstshort" == "pfe" ]];then
        #$cmd_shiftc --no-cron --no-mail --wait -L ${_arrfconfig[@]} $hostarc:$darc/
        #ssh -q $hostarc $cmd_rsync -azPqW ${_arrfconfig[@]} $darc/
        [[ -f $_dtmp/$_ftar ]] && ssh -q $hostarc $cmd_rsync -azqW $_dtmp/$_ftar $darc/

    elif $blarc2os;then 
        $cmd_sup ssh $hostarc test -d $darc
        local _status_darc=$?

        if (( $_status_darc == 0 ));then
            if $blrsync;then 
                $cmd_sup rsync -azPqW ${_arrfconfig[@]} $hostarc:$darc/
            else
                #$cmd_sup shiftc --wait -L --no-cron --no-mail ${_arrfconfig[@]} $hostarc:$darc/
                [[ -f $_dtmp/$_ftar ]] && $cmd_sup shiftc --wait --no-cron --no-mail $_dtmp/$_ftar $hostarc:$darc/
            fi
        else
            wmessage "@$LINENO ... FAILED"
        fi

    elif [[ "$hstshort" == "dis" ]];then 
        #$cmd_rsync -azPqW ${_arrfconfig[@]} $hostarc:$darc/
        [[ -f $_dtmp/$_ftar ]] && $cmd_sup shiftc --wait --no-cron --no-mail $_dtmp/$_ftar $hostarc:$darc/
    fi

    [[ -d $_dtmp ]] && rm -rf $_dtmp 2>/dev/null

    return
}

clean_output(){
    local _arrdelme=( "$@" )
    local stuff
    
    bldelout=false

    ! $blheader && writeheader && blheader=true

    #todo:  delete
    wmessage "... delete output files in collection dir"

    for stuff in ${_arrdelme[@]};do
        [[ -d $stuff ]] && rm -rf $stuff

        if [[ -f $stuff ]];then 
            rm -f $stuff
            grep -w $stuff $fexistexp >> $fdelout 2>/dev/null 
        fi
    done

    cat $fdelout 2>/dev/null | sed "s#$dexp/##g" >> $fdelout_nop

    bldelout=true 

    return
}

resub(){
    if [[ ! -f $fcomp_del ]];then
        wmessage "submit run_${strscr}.sh"
        $cmd_submit $cdir/run_${strscr}.sh >> $ferr
        #(( $? == 0 )) && blresubmit=true
        wmessage
        wmessage
    fi

    return
}

exp_editqname() {

    #local _usage="a function to:
    #   edit queuename in gcm*.j file. gcm*.j.bak is created as backup.
    #   In order for this function to work, a full pbs select statement has to be
    #   provided. ie: #PBS -l select=1:ncpus=20:mpiprocs=20:model=ivy:aoe=toss4
    #   Usage: ${FUNCNAME[0]} [a full patht to gcm*.j file] [quene name] [PBS select statement]"
    
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

recooking_hold(){
    #description:  find YYYYMM dir that are ready to be deleted.
    local _dhold=$dexp/holding
    local _numfthres=24
    local dhold_yyyymm2check

    #todo:  find YYYYMM dir except the first and last month
    local _arrdhold_yyyymm=($( find $_dhold/* -maxdepth 1 -mindepth 1 -type d -not -name "$capric_yyyymm" | grep -vE "$capr_yyyymm" | grep -v "_1mo_glo_" )) 

    local _arrdhold_yyyymm2check=($( printf '%s\n' ${_arrdhold_yyyymm[@]} | xargs -i bash -c " \
            numf=\$( find {}/* -type f -name '*z.nc4' 2>/dev/null | wc -l ) 
            (( \$numf > $_numfthres )) && echo {}
            " )) 
    

    local _arrdhold_yyyymm2clean=()    
    for dhold_yyyymm2check in ${_arrdhold_yyyymm2check[@]};do
        local _coll=$( echo $dhold_yyyymm2check | rev | cut -d'/' -f2 | rev ) 
        local _yyyymm=$( echo $dhold_yyyymm2check | rev | cut -d'/' -f1 | rev ) 
    
        [[ "${arrcollpost_diu[@]}" =~ "$_coll" ]] && local _bldiu=true || local _bldiu=false
    
        if $_bldiu;then 
            local arrfname=( $fcstdate.${_coll}.monthly.${_yyyymm}.nc4 $fcstdate.${_coll}.daily.${_yyyymm}.nc4.tar $fcstate.${_coll}.diurnal.${_yyyymm}.nc4)
        else
            local arrfname=( $fcstdate.${_coll}.monthly.${_yyyymm}.nc4 $fcstdate.${_coll}.daily.${_yyyymm}.nc4.tar )
        fi
    
        local numfcnt=$( printf '%s\n' ${arrfname[@]} | xargs -i grep {} $fexistarc 2>/dev/null | wc -l )
    
        if (( $numfcnt == ${#arrfname[@]} ));then
            _arrdhold_yyyymm2clean+=( $dhold_yyyymm2check )
        fi
    done
    
    echo "${_arrdhold_yyyymm2clean[@]}" 
}

exp_arcfempty(){
    #todo:  check empty files on arc
    local _scrarc=$cdir/${strscr}_arcempf.sh
    if $bldarcsame;then 
        local _blfzeroexist=false
        
    elif $blarc2os;then 
        [[ -f $fempty  ]] && rm -f $fempty

        if [[ -f $fexistarc ]];then 
            cat $fexistarc | xargs -n3 bash -c '(( $1 == 0 )) && echo $3' bash | xargs -i bash -c "echo {} | rev | cut -d'/' -f1-2 | rev " >> $fempty

            local _numfempty=$( cat $fempty | wc -l ) 
            (( $_numfempty > 0 )) && local _blfzeroexist=true || local _blfzeroexist=false
        else
            die "(${FUNCNAME[0]}) file does not exist: $( basename $fexistarc )"
            exit
        fi
    else
        if ssh -q $hostarc true >>/dev/null 2>&1; then 
            [[ -f $_scrarc ]] && rm -f $_scrarc
            [[ -f $fempty  ]] && rm -f $fempty

cat > $_scrarc << EOF
cd $darc
find * -maxdepth 1 -mindepth 1 -type f -empty -not -name "restarts*" 
cd - >/dev/null
EOF
                
            ssh -q $hostarc 'bash -s' < $_scrarc >> $fempty
            [[ -f $_scrarc ]] && rm -f $_scrarc
            
            local _numfempty=$( cat $fempty | wc -l ) 
            (( $_numfempty > 0 )) && local _blfzeroexist=true || local _blfzeroexist=false
        else
            local _blfzeroexist=false
        fi
    fi

    echo $_blfzeroexist
}

debug_wnumbers(){

    if [[ -s $fempty ]];then 
        wmessage "Empty File Exist on $hostarc:"
        wmessage "$( cat $fempty )"
        wmessage
    fi

    local _strblleaveout=" \
# of Output Files when blleaveout is true:
    #TOTAL: $numftotal_calc   $numfarc_fmomfout_cnt   $numfsave_tot $numfexp_total_cnt  
     #MAPL: $numfmonmapl_calc $numfarc_monmapl_cnt $numfsave_mpl $numfexp_monmapl_cnt
  #Monthly: $numfmonpost_calc $numfarc_monpost_cnt $numfsave_mon $numfexp_monpost_cnt 
  #Diurnal: $numfdiu_calc     $numfarc_diu_cnt     $numfsave_diu $numfexp_diu_cnt     
    #Daily: $numfday_calc     $numfarc_day_cnt     $numfsave_day $numfexp_day_cnt     
#Dailymean: $numfdmn_calc   $numfarc_dmn_cnt     $numfsave_dmn $numfexp_dmn_cnt
      #MOM: $numfmom_calc     $numfarc_mom_cnt     $numfsave_mom $numfexp_mom_cnt   
  #Missing: $numfmissing      0

"
    wmessage "$_strblleaveout"
 
    if $blleaveout && $bl10morun;then  
        local _strblleaveout10=" \
# of Output Files supposed to be Saved on PFE + Removed after 10mo run:
    #TOTAL: $numftotal_calc   $numfarc_fmomfout_cnt   $numfsavePfrm_tot $numfexp_total_cnt  
     #MAPL: $numfmonmapl_calc $numfarc_monmapl_cnt $numfsavePfrm_mpl $numfexp_monmapl_cnt
  #Monthly: $numfmonpost_calc $numfarc_monpost_cnt $numfsavePfrm_mon $numfexp_monpost_cnt 
  #Diurnal: $numfdiu_calc     $numfarc_diu_cnt     $numfsavePfrm_diu $numfexp_diu_cnt     
    #Daily: $numfday_calc     $numfarc_day_cnt     $numfsavePfrm_day $numfexp_day_cnt     
#Dailymean: $numfdmn_calc     $numfarc_dmn_cnt     $numfsavePfrm_dmn $numfexp_dmn_cnt
      #MOM: $numfmom_calc     $numfarc_mom_cnt     $numfsavePfrm_mom $numfexp_mom_cnt   
  #Missing: $numfmissing      0

"       wmessage "$_strblleaveout10"
    fi

    local _strtot=" \
Total # of Output Files:
    #TOTAL: $numftotal_calc   $numfarc_fmomfout_cnt   $numftotal_calc   $numfexp_total_cnt
     #MAPL: $numfmonmapl_calc $numfarc_monmapl_cnt $numfmonmapl_calc $numfexp_monmapl_cnt
  #Monthly: $numfmonpost_calc $numfarc_monpost_cnt $numfmonpost_calc $numfexp_monpost_cnt 
  #Diurnal: $numfdiu_calc     $numfarc_diu_cnt     $numfdiu_calc     $numfexp_diu_cnt     
    #Daily: $numfday_calc     $numfarc_day_cnt     $numfday_calc     $numfexp_day_cnt     
#Dailymean: $numfdmn_calc     $numfarc_dmn_cnt     $numfdmn_calc     $numfexp_dmn_cnt
      #MOM: $numfmom_calc     $numfarc_mom_cnt     $numfmom_calc     $numfexp_mom_cnt    
  #Missing: $numfmissing      0

"

    wmessage "$_strtot"

    return
}

double_checking(){

    if ((   $numftotal_calc   == $numfarc_fmomfout_cnt && $numfsave_tot == $numfexp_total_cnt   && \
            $numfmonmapl_calc == $numfarc_monmapl_cnt  && $numfsave_mpl == $numfexp_monmapl_cnt && \
            $numfmonpost_calc == $numfarc_monpost_cnt  && $numfsave_mon == $numfexp_monpost_cnt && \
            $numfdiu_calc     == $numfarc_diu_cnt      && $numfsave_diu == $numfexp_diu_cnt     && \
            $numfday_calc     == $numfarc_day_cnt      && $numfsave_day == $numfexp_day_cnt     && \
            $numfmom_calc     == $numfarc_mom_cnt      && $numfmissing  == 0       )) ;then
        local _blrmarchready=true

    elif $bl10morun && $blleaveout && \
        ((  $numftotal_calc   == $numfarc_fmomfout_cnt && $numfsavePfrm_tot == $numfexp_total_cnt   && \
            $numfmonmapl_calc == $numfarc_monmapl_cnt  && $numfsavePfrm_mpl == $numfexp_monmapl_cnt && \
            $numfmonpost_calc == $numfarc_monpost_cnt  && $numfsavePfrm_mon == $numfexp_monpost_cnt && \
            $numfdiu_calc     == $numfarc_diu_cnt      && $numfsavePfrm_diu == $numfexp_diu_cnt     && \
            $numfday_calc     == $numfarc_day_cnt      && $numfsavePfrm_day == $numfexp_day_cnt     && \
            $numfmom_calc     == $numfarc_mom_cnt      && $numfmissing      == 0      ));then
        local _blrmarchready=true

    elif (( $numftotal_calc   == $numfarc_fmomfout_cnt && $numftotal_calc   == $numfexp_total_cnt   && \
            $numfmonmapl_calc == $numfarc_monmapl_cnt  && $numfmonmapl_calc == $numfexp_monmapl_cnt && \
            $numfmonpost_calc == $numfarc_monpost_cnt  && $numfmonpost_calc == $numfexp_monpost_cnt && \
            $numfdiu_calc     == $numfarc_diu_cnt      && $numfdiu_calc     == $numfexp_diu_cnt     && \
            $numfday_calc     == $numfarc_day_cnt      && $numfday_calc     == $numfexp_day_cnt     && \
            $numfmom_calc     == $numfarc_mom_cnt      && $numfmissing      == 0       )) ;then
        local _blrmarchready=true

    else
        local _blrmarchready=false
    fi

    echo $_blrmarchready
}


clean_dir() {
    [[ -n $flock && -f $flock ]] && rm -f $flock
    
    #todo:  resubmit this script when walltime run out. 
    #[[ -n $blresubmit && -n $fcomp_del && -f $fcomp_del ]] && ! $blresubmit && resub

    return
}

debug_tmpsaveout(){
    #description:   this function is mainly used for 
    #               if input, _blmv = true, mv outputs from coll dir to .tmp_output dir
    local _blmv=$1
    local _dtmpsave=$dexp/.tmp_output
    local _arrcoll=($( exp_getcollections $dexp/HISTORY_2.rc )) 
    local stuff 

    if $_blmv;then
        wmessage "Move ${#arrfremove[@]} outputs from coll dir to $( basename $_dtmpsave )"
        
        [[ ! -d $_dtmpsave ]] && mkdir -p $_dtmpsave
        printf '%s\n' ${arrfremove[@]} >| $cdir/lst_arrfremove

        mv ${arrfremove[@]} $_dtmpsave/

        for stuff in ${arrfremove[@]};do
                grep $stuff $fexistexp >> $fdelout 2>/dev/null 
        done

        bldelout=true 

    elif ! $_blmv;then 
        if [[ ! -d $_dtmpsave ]];then 
            mkdir -p $_dtmpsave
        else
            local _numf=$( find $_dtmpsave/* -type f 2>/dev/null | wc -l ) 
            if (( $_numf > 0 ));then 
                wmessage "Move back $_numf outputs from $( basename $_dtmpsave ) to coll dir" 
                printf '%s\n' ${_arrcoll[@]} | xargs -i bash -c "mv $_dtmpsave/*.{}.* $dexp/{}/" 2>/dev/null 
                mv $_dtmpsave/ocean_daily.e* $dexp/MOM_Output/
                mv $_dtmpsave/restarts.e*    $dexp/restarts/
            fi
        fi
    fi
    return
}

note_boolean(){
    local note="
================================================================================
                      Booleans in run_gcmarch.sh Explained
================================================================================
20240709
when blleaveout == true:
    - figure out which fout to be deleted from dexp directory
    - calculate # of output that should remain in dexp accordingly.
    - use clean_output function to remove output from dexp once
      all output files are archived properly.
    - create gcmarch_deloutcompleted marker if outputs are removed successfully.

when blleaveall == true:
    - works the same as blleaveout == true, but skip clean_output.
    - create *only* gcmarch_archcompleted marker (NO gcmarch_deloutcompleted).

when blarc2os == true:
    - fnamearc function gets wftmsz_flist from outside host and rename it as fexistarc
    - cooking_configarch function archive config files to outside host.
    - exp_arcfempty function use fexitarc file to find empty output file

*** bldarcsame is automatically set in run_gcmarh.sh ***
when bldarcsame == true:
    - create *only* gcmarch_archcompleted marker (NO gcmarch_deloutcompleted).
    - exit out when gcmarch_archcompleted exists.

Difference between blleaveall vs. bldarcsame
    - When bldarcsame == true, there are some processes that the script will skip
        such as getfname_arc. Once gcmarch_archcompleted marker is created, the script
        will go through only these three functions to complete.
            count_files
            write_table
            write_table_collection

    - When blleaveall == true, the script will skip clean_output function but go through
        cooking_configarch in addtion to count_files, write_table, and write_table_collection.
"
    echo "$note"
    return
}

#================================================================================
#                                     Begin
#================================================================================
#starthere
pdir=$( dirname $( realpath $0 ) )
cdir=@DFCST/@FCSTDATE/@ENSEMBLE_MEMBER/archive
strscr=gcmarch
hstname=$( hostname )
hstshort=$( get_host )
blnode_nas=false
blnode_nccs=false
if [[ ${hstname:0:3} == "pfe" ]];then
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

trap 'clean_dir' EXIT

optb=false
optq=false
optt=false
writetofile=0
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        archive forcast outputs, delete outputs on exp dir and holding/collection/YYYYMM dir. 

        Usage: ./$(basename "$0") [-bhct] [ -q PFE Queue ID ]

    options:
        -q                  queue id (Works only on PFE)
                            *When you enter normal as queue, you can also enter model name with
                             colon as a delimiter. i.e. normal:bro_ele, normal:ivy
        -t                  write table and exit 
        -b                  debug mode
        -c                  clean unwanted files
        -h                  show this help text
            --note-boolean  show notes on booleans (i.e. blleaveout, blleaveall, blarc2os )
"

file=
verbose=0
cnt=0
while :; do
    case $1 in
        -b            )  optb=true;;
        -q            )  [[ "$2" ]] && userqid=$2 && optq=true && shift \
                           || die "option q requires an argument";;
        -t            )  optt=true;;
        -c            )  clean_dir; exit 0;;
       --note-boolean )  note_boolean; exit 0;;
        -h|-\?|--help )  echo "$usage";exit;;
                   -- )  shift;break;;                  # End of all options.
                  -?* )  printf 'WARN: Unknown option (ignored): %s\n' "$1" >&2;;
                    * )  cnt=$(( cnt + 1 )); break ;;   # Default case: No more options, so break out of the loop.

    esac
    shift
    cnt=$(( cnt + 1 ))
done
 
#================================================================================
#                               Check User Inputs
#================================================================================

if $optq && [[ "$hstshort" == "dis" ]];then 
    wmessage "WARNING: option q works only on PFE"
    optq=false
    userqid=
fi

#================================================================================
#                             Set Host Specific Vars
#================================================================================
#todo:  archiving host
if [[ "$hstshort" == "pfe" ]];then 
    hostarc=@HOSTARC_GCMARC
         cmd_rsync=/usr/bin/rsync
        cmd_submit=/PBS/bin/qsub
          cmd_gjob=cmd_gjob_nas
     cmd_gjobjlong=cmd_gjob_nas_jlong
         cmd_qstat=/u/scicon/tools/bin/qstat
        cmd_shiftc=/usr/local/bin/shiftc

elif [[ "$hstshort" == "dis" ]];then 
    hostarc=@HOSTARC_GCMARC
     cmd_realpath=/usr/bin/realpath
        cmd_rsync=/usr/bin/rsync
       cmd_submit=/usr/slurm/bin/sbatch
     cmd_scontrol=/usr/slurm/bin/scontrol 
       cmd_squeue=/usr/slurm/bin/squeue
         cmd_gjob=cmd_gjob_nccs
    cmd_gjobjlong=cmd_gjob_nccs_jlong
          cmd_sup=$HOME/bin/sup

    export SQUEUE_FORMAT='%.10i %.8u %.9P %.20j %.8T %.10M %.9l %.6D %.5C %15f'
fi

#================================================================================
#                    Set Global Variables and Source Scripts
#================================================================================
#mid
     dexp=@DFCST/@FCSTDATE/@ENSEMBLE_MEMBER
     darc=@DARCH/@FCSTDATE/@ENSEMBLE_MEMBER

[[ "$pdir" != "$dexp/archive" ]] && exit

     ferr=$cdir/stderr_${strscr}
     dutl=$dexp/s2s_util
     ferr=$cdir/stderr_${strscr}
    fcomp=$cdir/${strscr}_archcompleted
fcomp_del=$cdir/${strscr}_deloutcompleted
 fsubstop=$cdir/${strscr}_SUBSTOPPED

dtmp=$cdir/tmp 
 [[ ! -d $dtmp  ]] && mkdir -p $dtmp

fdatapost_savedy3=$cdir/data_${strscr}_GEOSS2S3_collpost_savedy3
    fdatapost_lst=$cdir/data_${strscr}_GEOSS2S3_collpost_lst

[[ ! -f $fdatapost_savedy3 ]] && wmessage "File missing: $( basename $fdatapost_savedy3 )" && exit
[[ ! -f $fdatapost_lst     ]] && wmessage "File missing: $( basename $fdatapost_lst )"     && exit

fmkfname_nop=$cdir/${strscr}_mkfname_fname_nop
fmkfsave_nop=$cdir/${strscr}_mkfname_fsave_nop

#todo:  get expid for fcst
strexpid=$( echo $dexp | rev | cut -d'/' -f3 | rev ) 
[[ "$strexpid" == "GEOS_fcst" ]] && strexpid=$( echo $strexpid | cut -d'_' -f2 ) 

#todo:  exit if output cleaning was already completed
[[ -f $fcomp_del ]] && exit
[[ -z $dexp      ]] && [[ ! -d $dexp ]] && die "dexp is undefined"
[[ -f $fsubstop  ]] && rm -f $fsubstop

collsst=sst_tavg_1dy_glo_L720x361_slv
strmkdy=s2smkdymean

fscr_mkdy=${strmkdy}.sh
[[ -f $dutl/$fscr_mkdy ]] && blmkdy=true || blmkdy=false

#note:  blleaveout should be true *only when you are running s2sv3 hindcast/forecast.
blleaveout=@BLLEAVEOUT

#note:  blleaveall leave ALL outputs in dexp. blleaveout is superseded by 
#       blleaveall regardless of blleaveout is true/false. Set as false for 
#       s2s-3 hindcast.
blleaveall=@BLLEAVEALL
$blleaveall && [[ -f $fcomp ]] && exit

#note:  keep bldarcsame = false at NAS. 
#       06/28/2024 At NCCS, bldarcsame = true for s2s-3 NRT for now. 
[[ "$dexp" == "$darc" ]] && bldarcsame=true || bldarcsame=false
$bldarcsame && [[ -f $fcomp ]] && exit

#todo:  set various vars
setvars $dexp

        cdate=$( date +%Y%m%d%H%M )
        fshiftin_base=${strscr}_shiftin
      fshiftin=$cdir/$fshiftin_base
     fdiff_nop=$cdir/${strscr}_diff_fmkflive
fdelrdyexp_nop=$cdir/${strscr}_exp_fdelrdy_nop

        fexistexp=$cdir/${strscr}_wftmsz_exp
    fexistexp_nop=$cdir/${strscr}_wftmsz_exp_nop
    fexistexp_sst=$cdir/${strscr}_wftmsz_exp_sst
fexistexp_sst_nop=$cdir/${strscr}_wftmsz_exp_sstnop

if $bldarcsame;then 
        fexistarc=$fexistexp
    fexistarc_nop=$fexistexp_nop
else
        fexistarc=$cdir/${strscr}_wftmsz_arc
    fexistarc_nop=$cdir/${strscr}_wftmsz_arc_nop
fi

  ftmp_fmissarc_nop=$cdir/tmp_${strscr}_arc_fmiss_nop

arrfrm=( $ftmp_fmissarc_nop )

   fdelyyyymm=$cdir/${strscr}_delyyyymm
      fdelout=$cdir/${strscr}_delout
  fdelout_nop=$cdir/${strscr}_delout_nop
        fnote=$cdir/note_${strscr}
       fempty=$cdir/${strscr}_arcempf
    frsyncfin=$cdir/${strscr}_rsync

frsyncfin_base=${strscr}_rsync
ferrrsync_base=stderr_rsync_
      frstpert=${capric_yyyymmdd}.log 

scrnamersync_base=${strscr}_${strexpid}_${fcstdate}${ensm}_

dexploc=$( echo $dexp | cut -d'/' -f3 )

#Command Options:
#note:  archive output from current host to outside server/host
#       06/28/2024 If s2s-3 fcst is going to be archive from NCCS to NAS, 
#       set blarc2os = true. Otherwise, false
if [[ "$hstshort" == "dis" && "$hostarc" == "lfe" ]];then
    blarc2os=true
else
    blarc2os=false
fi
#$blarc2os && [[ -f $fcomp ]] && exit

#note:  blrsync=true to use rync to archive output. Default command for archiving is shiftc with no wait (--wait).
#       If this script is going to run computing nodes, set blrsync=false
blrsync=@BLRSYNC

#note:  blsft=true to use "shiftc --wait" to archive. Default command for archiving is shiftc with no wait (--wait).
#       If this script is going to run computing nodes, set blsft=false
blsft=false

#note:  blsubpst=true to submit gcm_post
blsubpst=true

darc_parent=$( echo $darc | rev | cut -d'/' -f3- | rev )
if ! $bldarcsame && [[ "$hstshort" == "dis" && "$hostarc" == "lfe" ]];then 

    $cmd_sup ssh $hostarc test -d $darc_parent 2>/dev/null 
    status_testd=$?

    if (( $status_testd == 0 )) ; then 
        blhostarc_avail=true
    else
        blhostarc_avail=false
    fi
elif [[ "$hstshort" == "pfe" ]];then 
    if ssh -q $hostarc true >>/dev/null 2>&1; then
        blhostarc_avail=true
    else
        blhostarc_avail=false
    fi
else
    blhostarc_avail=false
fi

#250GB
#limsize=268435456000
#100G
#limsize=107374182400
#50G
#limsize=53687091200
#25G
#limsize=26843545600
if $blrsync;then 
    #10G
    limsize=10737418240
else
    #22G
    limsize=23622320128
fi

 limsftc=4
limrsync=4

bldelhold=false
blrmarchready=false
bloutindcoll=false
blheader=false
bldelout=false
blrunning=false
rundelfout=0

strbtc="shiftc batch file ="
strsid="Shift id is "
strpbs_output="Output_Path"
strsbt_wrkdir="WorkDir"
strpbs_resource="Resource_List.select"
model_default=sky_ele

if $optq;then 
    numdelimit=$( echo $userqid | grep -o ":" | wc -l ) 
    if (( $numdelimit == 1 ));then 
          qid=$( echo $userqid | cut -d':' -f1 ) 
        model=$( echo $userqid | cut -d':' -f2 ) 
    else
        qid=$userqid
    fi 

    [[ -z $model ]] && model=$model_default

    if [[ "$qid" == "normal" || "$qid" == "long"  ]];then 
        model=$model
        cpus=$( getnumcore $model ) 
        pbsselect="#PBS -l select=1:ncpus=${cpus}:mpiprocs=${cpus}:model=${model}"
        pbsq=normal
    else
        model=$( pbs_rstat -f $qid | grep $strpbs_resource | cut -d':' -f2 | rev | cut -d'=' -f1 | rev ) 
         cpus=$( pbs_rstat -f $qid | grep $strpbs_resource | cut -d':' -f3 | rev | cut -d'=' -f1 | rev ) 
        pbsselect="#PBS -l select=1:ncpus=${cpus}:mpiprocs=${cpus}:model=${model}:aoe=toss4"
        pbsq=$qid
    fi

else
    model=$model_default
    cpus=$( getnumcore $model ) 
    pbsselect="#PBS -l select=1:ncpus=${cpus}:mpiprocs=${cpus}:model=${model}"
    pbsq=normal
fi

numflast=1

#todo: check if gcm_post are running 
arrjobs=()
if [[ "$hstshort" == "pfe" ]];then 
    dexp_realpath=$dexp

    arrjobid=($( $cmd_gjob | grep P$fcstdate$ensm 2>/dev/null | cut -d' ' -f1 )) 

    for jobid in ${arrjobid[@]};do
        thisdexp=$( $cmd_qstat -f $jobid | grep -i $strpbs_output | rev | cut -d':' -f1 | rev | xargs -i dirname {} | xargs -i dirname {} )
        [[ "$thisdexp" == "$dexp" ]] && arrjobs+=( $jobid )
    done

    if $blmkdy;then
        arrjobid_mkdy=($( $cmd_gjobjlong | grep ${strmkdy}.$fcstdate$ensm 2>/dev/null | cut -d' ' -f1 )) 
        for jobid in ${arrjobid_mkdy[@]};do
            thisdexp=$( $cmd_qstat -f $jobid | grep -i $strpbs_output | rev | cut -d':' -f1 | rev | xargs -i dirname {} | xargs -i dirname {} )
            [[ "$thisdexp" == "$dexp" ]] && arrjobs_mkdy+=( $jobid )
        done
    fi

elif [[ "$hstshort" == "dis" ]];then 
    dexp_realpath=$( /usr/bin/realpath $dexp ) 

    arrjobid=($( $cmd_gjob | grep P$fcstdate$ensm 2>/dev/null | tr -s '[:space:]' | sed 's#^ *##' | cut -d' ' -f1 )) 
    for jobid in ${arrjobid[@]};do
        thisdexp=$( $cmd_scontrol show job $jobid | grep -i $strsbt_wrkdir | rev | cut -d'=' -f1 | cut -d'/' -f2- | rev )
        [[ "$thisdexp" == "$dexp" || "$thisdexp" == "$dexp_realpath" ]] && arrjobs+=( $jobid )
    done

    if $blmkdy;then
        arrjobid_mkdy=($( $cmd_gjobjlong | grep ${strmkdy}.$fcstdate$ensm 2>/dev/null | tr -s '[:space:]' | sed 's#^ *##' | cut -d' ' -f1 )) 

        for jobid in ${arrjobid_mkdy[@]};do
            thisdexp=$( $cmd_scontrol show job $jobid | grep -i $strsbt_wrkdir | rev | cut -d'=' -f1 | cut -d'/' -f2- | rev )
            [[ "$thisdexp" == "$dexp" || "$thisdexp" == "$dexp_realpath" ]] && arrjobs_mkdy+=( $jobid )
        done
    fi
fi

(( ${#arrjobs[@]}      == 0 )) && blpstrunning=false  || blpstrunning=true
(( ${#arrjobs_mkdy[@]} == 0 )) && blmkdyrunning=false || blmkdyrunning=true

#todo:  delete unnecessary files
rm -f $cdir/${strscr}_??? 2>/dev/null
rm -f $fshiftin 2>/dev/null
[[ -f $fnote ]] && rm -f $fnote
[[ -f $fcomp ]] && rundelfout=1

#================================================================================
#                                  Main Process
#================================================================================
#letsbegin

#todo:  find YYYYMM dir to delete, get output file names on archive host & exp host, and find missing outputs, etc.
prepare_archiving
count_files

write_table
$optt && wmessage "exit @$LINENO" && exit

#todo:  delete output files from collection dir.
if (( $rundelfout == 1 && ${#arrfmiss[@]} == 0 ));then
    wmessage "@$LINENO Unknown error: $( basename $fcomp ) exists, but $( basename $fcomp_del ) does not."
    wmessage 
fi

#todo:  exit if fcomp exists
(( $rundelfout == 1 )) && wmessage "exit @$LINENO" && exit

#todo:  make dailymean
if $blmkdy;then 

    if ! $blmkdyrunning;then 
        #+++++ cd dutl (start) +++++
        cd $dutl

        if (( ${#arrmkdy_fmiss[@]} > 0 ));then 
            ./$fscr_mkdy >> stderr_${strmkdy}
        fi

        cd - >/dev/null 
        #+++++ cd dutl ( end ) +++++
    fi
fi

if ! $blpstrunning;then
    #todo:  get unfinished post script
    arrpst1=($( exp_sherlockpst ))

    #todo:  check if holding/coll/yyyymm dir exsits for gcm_post 
    arrpst_dholdmiss=($( exp_missdhold ${arrpst1[@]} ))

    arrpst1=($( printf '%s\n' ${arrpst1[@]} ${arrpst_dholdmiss[@]} | sort -V | uniq -u )) 
     arrfpst_miss=($( printf "%s\n" ${arrpst1[@]} | xargs -i bash -c "[[ ! -f $dexp/post/{} ]] && echo {}" ))
    arrfpst_exist=($( printf "%s\n" ${arrpst1[@]} | xargs -i bash -c "[[   -f $dexp/post/{} ]] && echo {}" ))
    arrpst=()
    
    for fpstcoll_miss in ${arrfpst_miss[@]};do
          thiscoll=$( echo $fpstcoll_miss | rev | cut -d'.' -f2 | rev )
        thisyyyymm=$( echo $fpstcoll_miss | rev | cut -d'.' -f1 | cut -c1-6 | rev )
        wmessage "Make $fpstcoll_miss"
        exp_mkcollgcmpost $dexp $thiscoll $thisyyyymm
        (( $? == 0 )) && arrpst+=( $( basename $fpstcoll_miss ) )
    done 

    (( ${#arrfpst_exist[@]} > 0 )) && arrpst+=( ${arrfpst_exist[@]} ) 

    wmessage

    if $blsubpst && (( ${#arrpst[@]} > 0 )) ;then
        ! $blheader && writeheader && blheader=true

        if [[ "$hstshort" == "pfe" ]];then 
            arrsub=($( exp_editfpst ${arrpst[@]} ))
        elif [[ "$hstshort" == "dis" ]];then 
            arrsub=( ${arrpst[@]} )
        fi

        if (( ${#arrsub[@]} > 0 ));then
            wmessage "Re-submit gcm_post.*.j"
            ahand_warr ${arrsub[@]} 
            wmessage
            cd $dexp/post
            printf '%s\n' ${arrsub[@]} | xargs -i $cmd_submit {} 
            cd - >/dev/null
            wmessage
        fi
    fi

    #todo:  check if dholdmiss is actually missing due to an error
    arrpst_dmiss=()
    for pst_dholdmiss in ${arrpst_dholdmiss[@]};do
        pstyyyymm=$( echo $pst_dholdmiss | rev | cut -c1-6 | rev ) 
         pst_coll=$( echo $pst_dholdmiss | cut -d'.' -f2 ) 

        printf '%s\n' ${arrcollpost_part[@]} | grep $pst_coll >> /dev/null 2>&1
        statusgrep=$?
        if (( $pstyyyymm != $capric_yyyymm && $statusgrep > 0 ));then 
            arrpst_dmiss+=( $pst_dholdmiss )
        fi
    done

    if (( ${#arrpst_dmiss[@]} > 0 ));then
        wmessage "Unable to Submit: coll/yyyymm dir does NOT exist in holding dir"
        ahand_warr ${arrpst_dmiss[@]} 
        wmessage
    fi
fi

! $blhostarc_avail && ! $bldarcsame && wmessage "exit @$LINENO" && exit

! $blheader && writeheader && blheader=true

#todo:  delete empty shfitc file. 
find * -maxdepth 0 -type f -name "$fshiftin_base*" -empty -delete 2>/dev/null

if ! $blrsync ;then
    #todo:  find existing shitin* files 
    arrdiv_previous=($( find * -maxdepth 0 -type f -name "${fshiftin_base}_*" 2>/dev/null | sort -V ))

    if (( ${#arrdiv_previous[@]} > 0 ));then 
        #todo:  check if any shiftc archiving are still running. And rename shfitc files name. 
        arrsid=($( check_shiftc "${arrdiv_previous[@]}" ))
        numsid_running=${#arrsid[@]}

        wmessage
        wmessage "# of running shiftc (max limit=$limsftc) = $numsid_running"
        wmessage
        
        if (( ${#arrsid[@]} > 0 ));then
            wmessage "Shiftc ID & Filename:"
            for sid in ${arrsid[@]};do
                fsft=$( grep -B5 "$strsid$sid" $ferr 2>/dev/null | grep "$strbtc" 2>/dev/null | rev | cut -d' ' -f1 | rev )
                arrfsft_running+=( $fsft ) 
                wmessage "$sid $fsft"
            done

            wmessage

            #todo:  rm shiftcin_* files which are not running 
            arrfsft_notinuse=($( printf '%s\n' ${arrdiv_previous[@]} ${arrfsft_running[@]} | sort -V | uniq -u ))
            numflast1=$( printf '%s\n' ${arrfsft_running[@]} | rev | cut -d'_' -f1 | rev | sort -n | tail -1 ) 
            numflast=$( echo "$numflast1 * 1 + 1" | bc ) 

            wmessage "# of Missing Files  - ${#arrfmiss[@]}"
            wmessage "# of Running Shiftc - ${#arrfsft_running[@]} ( = $numsid_running ) "
            printf '    %s\n' ${arrfsft_running[@]}
            wmessage

            #note:  05/09/2023 Save these shiftc_* files in a tmp dir
            #rm -f ${arrfsft_notinuse[@]} 2>/dev/null
            (( ${#arrfsft_notinuse[@]} > 0 )) && mv ${arrfsft_notinuse[@]} $dtmp/ 2>>/dev/null

            if (( $numsid_running >= $limsftc ));then 
                wmessage "exit @ $LINENO" && exit
            elif (( $numsid_running > 0 ));then 
                blrunning=true
            else
                blrunning=false
            fi

        elif (( ${#arrsid[@]} == 0 ));then
            rm -f $fshiftin_base* 2>/dev/null
        fi
        wmessage
    fi


elif $blrsync ;then 
    arrfrsyncin=($( find * -maxdepth 0 -type f -name "${frsyncfin_base}_*" )) 

    if (( ${#arrfrsyncin[@]} > 0 ));then 
        arrfrsc_running=($( check_rsync "${arrfrsyncin[@]}" ))
        arrfsft_running=( ${arrfrsc_running[@]} )  

        if (( ${#arrfrsc_running[@]} >= $limrsync ));then 
            wmessage "exit @$LINENO"
            exit
        elif (( ${#arrfrsc_running[@]} > 0 ));then 
            blrunning=true
        else
            blrunning=false
        fi
    fi
fi

#todo:  start to archive output to archiving host
#note:  - create fshiftin even when archiving with rsync. when blrsync is true, 
#       create_shiftc_fin function will write fshiftin file with "[exp filename] [DEST]"
#       - when archive, use this file as datafile.
! $bldarcsame && create_shiftc_fin $fshiftin $hostarc $darc "${arrfmiss[@]}"
[[ -f $fshiftin ]] && arrdiv=($( divide_fin $numflast $fshiftin ))

#todo:  if shiftc are still processing and nothing more to archive (#arrdiv == 0 )), then
#       let shiftc complete its work.
[[ -n $numsid_running ]] && (( $numsid_running > 0 )) && (( ${#arrdiv[@]} == 0 )) && wmessage "exit @ $LINENO" && exit

! $blheader && writeheader && blheader=true

#todo:  execute shiftc
if [[ -f $fshiftin ]];then 
    if $blrsync;then
        cooking_archiving_rsync "${arrdiv[@]}"
        status_cooking=$?

    elif $blsft;then
        cooking_archiving "${arrdiv[@]}"
        status_cooking=$?
    else
        #note:  1  is required input for the function. Used as a counter.
        cooking_archiving_nowait 1 "${arrdiv[@]}"
        status_cooking=$?
    fi
       
    wmessage "exit @ $LINENO" && exit

    #prepare_archiving
fi

$blrunning && wmessage "exit @$LINENO" && exit
prepare_archiving

#todo:  count number of files
count_files

#note:  these if statements do the same thing but one for after 3-months run and the other one for
#       after 10month run.
if (( $numftotal_calc == $numfarc_fmomfout_cnt && $numfmom_calc == $numfarc_mom_cnt && $numfmissing == 0 ));then

    #note:  05/09/2023 Save these shiftc_* files in a dir
    mv $cdir/${strscr}_*_shiftin* $cdir/tmp 2>/dev/null

    #todo:  check empty files on archive
    barcmptyexist=$( exp_arcfempty ) 
    if $barcmptyexist;then 
        wmessage "Empty output file exist on $hostarc"
        wmessage "$( cat $fempty )"
    else
        #check 3month run
        touch $fcomp
        blrmarchready=true
    fi

elif $blrmarchready && (( $numftotal_calc == $numfarc_fmomfout_cnt && $numfmom_calc == $numfarc_mom_cnt && $numfmissing == 0 ));then

    #note:  05/09/2023 Save these shiftc_* files in a dir
    mv $cdir/${strscr}_*_shiftin* $cdir/tmp 2>/dev/null

    #todo:  check empty files on archive
    barcmptyexist=$( exp_arcfempty ) 
    if $barcmptyexist;then 
        wmessage "Empty output file exist on $hostarc"
        wmessage "$( cat $fempty )"
    else
        #check 10month run
        touch $fcomp
        blrmarchready=true
    fi
fi

if $optb;then 
    debug_wnumbers 
    [[ -f $fcomp ]] && rm -f $fcomp
    exit
fi

#todo:  double check before delete outputs!!
blrmarchready=$( double_checking ) 

#wmessage "@$LINENO blrmarchready = $blrmarchready"
#wmessage "exit @$LINENO" && exit


if [[ -f $fcomp ]] && (( ${#arrfmiss[@]} == 0 && ${#arrmkdy_fmiss[@]} == 0 ));then

    if $bldarcsame;then 
        #todo:  keep all output files when dexp and archive dir are the same
        count_files
        write_table
        write_table_collection

    else

        #note: double_checking gives false if there are more files in each category 
        #      of output (i.e. daily, monthly, mom, restarts, etc). 
        if ! $blrmarchready;then
wmessage \@$LINENO "you are here? why? ... interesting."
            #if ! $blleaveall;then 
            #    wmessage "@$LINENO ... clean output here"
            #   
            #    arrfdel=($( cat $fdelrdyexp_nop | cut -d' ' -f3 | xargs -i echo $dexp/{} ))

            #    #clean_output ${arrfdel[@]}
            #fi
            blrmarchready=$( double_checking ) 
        fi

        if $blrmarchready;then
            if ! $blleaveall;then 
                wmessage "@$LINENO ... clean output here"
                arrfdel=($( cat $fdelrdyexp_nop | cut -d' ' -f3 | xargs -i echo $dexp/{} ))

                clean_output ${arrfdel[@]} 


                #todo:  backup files
                [[ -s $fdelout_nop    ]] && mv $fdelout_nop    $dtmp/$( basename $fdelout_nop )_$cdate
                [[ -s $fdelout        ]] && mv $fdelout        $dtmp/$( basename $fdelout )_$cdate
                [[ -s $fdelrdyexp_nop ]] && mv $fdelrdyexp_nop $dtmp/$( basename $fdelrdyexp_nop )_$cdate

                $bldelout && touch $fcomp_del
            fi

            count_files
            write_table
            write_table_collection

            cooking_configarch

        fi
    fi
fi

exit


