#!/usr/bin/env bash

inputcheck(){
    [[ ! -f $srcf ]] && die "srcme file is required input and does not exist"

    #note:  these options require accessing archiving host. If it is not accessible
    #       exit out from here.
    if $optk || $optb || $optoutarc2exp || $optrmextra || $optl || $optn;then 
        if ssh -q $hostarc true >>/dev/null 2>&1; then 
            :
        else
            #die "$hostarc is unavailable" && exit
            exit
        fi
    fi

    if (( $writetofile == 1 ));then
       if $optq || $opta || $opto || $optt || $optn || $optm || $optsftcdays || \
          $optarchlock || $optcalcnumout || $optgetwtime || $optrmextra || $optoutarc2exp;then 
           :
       else
           writetofile=0
           wmessage "***opt w is not available for the specified utility process."
           wmessage
       fi
    fi
    return
}

getyyyymm() {
    local arricyyyymm=()
    local yyyymm=$startyyyymm
    while (( $yyyymm <= $endyyyymm ));do
        arricyyyymm+=( $yyyymm )
        yyyy=$( echo $yyyymm | cut -c1-4 )
        mm=$( echo $yyyymm | cut -c5- )
        yyyymm=$( fcal_nextmonth $mm $yyyy )
    done

    echo "${arricyyyymm[@]}"
}

make_nonmonthly_sst(){
    #description:   conpress sst files. 
    local dexp=$1
    local _dhold=$dexp/holding
    local _fhis1=$dexp/HISTORY_1.rc
    local fcapric=cap_restartIC
    local _fcapric=$dexp/$fcapric

    [[ ! -s $_fhis1   ]] && wmessage "(${FUNCNAME[0]}) file does not exist: $_fhis1"   && return 
    [[ ! -s $_fcapric ]] && wmessage "(${FUNCNAME[0]}) file does not exist: $_fcapric" && return 
    [[ ! -d $_dhold   ]] && wmessage "(${FUNCNAME[0]}) dir does not exist: $_dhold"    && return 

    local capric_yyyymmdd=$( cat $_fcapric | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
    local capric_yyyymm=${capric_yyyymmdd:0:6}
    local coll=$collsst
    local bllastdayofthemonth=$( fcal_lastdayofthemonth $capric_yyyymmdd )

    $bllastdayofthemonth && local nummon_add=3 || local nummon_add=2

    set_rstfcstdate $dexp    
    
    #todo:  get all yyyymm
    #ref:   https://tinyurl.com/yglc3awu
    local collfreq=$( grep $coll.frequency $_fhis1 |  tr -s '[:space:]' |rev |  cut -d' ' -f1 | tr -d ',' | rev | cut -c1-2 | xargs -i bash -c "echo "'"'"{}*1"'"'" | bc" )
    local calcnumflastday=$( echo "24/$collfreq" | bc 2>/dev/null ) 

    #todo:  get the 2nd full month
    local yyyymm=$capric_yyyymm
    for i in $( seq 1 $nummon_add );do
        yyyymm=$( fcal_nextmonth $yyyymm)
    done 

    #todo:  create tar for non-monthly files
    local yyyy_mm=${yyyymm:0:4}${yyyymm:4:2}
    local ldd=$( numdaysinmonth $( echo "${yyyymm:4:2}*1" | bc ) ${yyyymm:0:4} )

    #todo:  get a full path of individual output files
    local arrfyyyy_mmpath=($( find $_dhold/$coll/* -type f -name "*.$coll.*$yyyy_mm*.nc?" 2>/dev/null ))
    local numflastday=$( find $_dhold/$coll/* -type f -name "*.$coll.*$yyyy_mm$ldd*.nc?" 2>/dev/null | wc -l )
    local ftar=$dexp/$coll/$fcstdate.$coll.$yyyymm.nc4.tar 

    #19911202.sst_tavg_1dy_glo_L720x361_slv.daily.sst_ta.nc4.tar
    local ftarpst=$( find $_dhold/$coll/* -type f -name "$fcstdate.$coll.daily.sst_ta.nc4.tar" 2>/dev/null )

    #19940625.sst_tavg_1dy_glo_L720x361_slv.daily.199408.nc4.tar
    local ftarday=$( find $dexp/$coll/* -type f -name "$fcstdate.$coll.daily.$yyyymm.nc4.tar" 2>/dev/null )

    
    #todo:  copy *.daily*.nc4 and create ftar if daily files exist
    if [[ ! -f $ftar ]] && [[ -f $ftarday ]];then 
        wmessage "... copy *daily.*.nc4 ..."
        cp -p $ftarday $dexp/$coll/$fcstdate.$coll.$yyyymm.nc4.tar
    fi
    #todo:  compress non-monthly output if dexp/coll doesn't have tar
    if [[ ! -f $ftar && -n $calcnumflastday ]] && (( $calcnumflastday == $numflastday )); then

        local arrfyyyy_mm=($( printf '%s\n' ${arrfyyyy_mmpath[@]} | xargs -i basename {} ))
        local numdyyyy_mm=($( printf '%s\n' ${arrfyyyy_mmpath[@]} | xargs -i dirname {} | sort -V | uniq | wc -l ))
        local dyyyy_mm=($( printf '%s\n' ${arrfyyyy_mmpath[@]} | xargs -i dirname {} | sort -V | uniq ))

        if (( $numdyyyy_mm == 1 ));then
            #+++++ cd to holding/coll/somewhere (start) +++++
            #cd $_dhold/$coll/$yyyy_mm
            cd $dyyyy_mm
            wmessage "... $ftar ..."

            #/usr/local/bin/shiftc --no-cron --no-mail --wait --create-tar ${arrfyyyy_mm[@]} $ftar 
            tar -cf $ftar ${arrfyyyy_mm[@]}
            local status_tar=$?
            (( $status_tar != 0 )) && [[ -f $ftar ]] && rm -f $ftar 

            cd - >/dev/null
            #+++++ cd to holding/coll/somewhere ( end ) +++++
        else
            wmessage "Unable to locate sst output files for $dexp"
        fi
    fi
    
    return
}

arc_screen(){
    #dscription:    run gcmarch in a screen session
    local _arrdexp=( "$@" )
    #local arricyyyymm=($( getyyyymm ))
    local dout=$cdir/output/getstatus/$strexpid
    local farcdel=gcmarch_deloutcompleted
    local strarch=gcmarch

    
    #note:  numsec is wailt time in sec and maxsec is total wait time limit
    local numsec=60
    local maxsec=120
    local maxmin=$( echo "$maxsec / 60" | bc  )
    
    local maxscr=10

    #todo:  get dexp that needs to be run arch script. 
    local arrdexp1=($( printf '%s\n' ${_arrdexp[@]} | xargs -i bash -c "[[ ! -f {}/archive/$farcdel ]] && echo {}" ))
    local  arrdexp=($( printf '%s\n' ${arrdexp1[@]} | xargs -i bash -c "[[ ! -f {}/archive/$strarch.lock ]] && echo {}" ))

    for dexp in ${arrdexp[@]};do
    
        set_rstfcstdate $dexp
        local scrsession=${strscr}_${strexpid}_${fcstdate}ens$intens
       
        #todo:  check if gcm_run.j is running  
        cnt_jobs $dexp
        (( $num_rgrn > 0 )) && continue

        #tood:  check if screen session is running
        screen -ls | grep $scrsession >/dev/null
        (( $? == 0 )) && continue
    
        #todo:  check if archive and deleting output are completed
        [[ -f $farcdel ]] && continue
    
        #todo:  copy the latest gcmarch.sh if different
        update_archscr $dexp >/dev/null

    
        #todo:  sleep when total of $maxscr screen sessions are created
        local numscr=$( screen -ls | grep ${strscr}_${strexpid}_ | wc -l )
        totsec=0
        (( $numscr == $maxscr  )) && wmessage "$( date +'%m/%d/%Y %H:%M' ) ... $( printf '%02g\n' $numscr ) screen sessions are running ... will wait for max of $maxmin minutes ..."

        #while (( $numscr > 0 )) && (( $totsec != $maxsec )) ;do
        while (( $numscr == $maxscr  )) && (( $totsec != $maxsec )) ;do
            
            sleep ${numsec}s
    
            totsec=$(( totsec + numsec ))
            totmin=$( echo "$totsec/$numsec" | bc )
            wmessage "$( date +'%m/%d/%Y %H:%M' ) ... $( printf '%02g\n' $numscr ) screen sessions are running ... waited for $totmin min ..."
            
            numscr=$( screen -ls | grep -i detached | grep $strscr | grep $strexpid | wc -l )
        done

        local blrunning=false
        while ! $blrunning; do
            if (( $numscr < $maxscr ));then 
                wmessage "... screen $dexp ..."
        
                #+++++ cd to dexp/archive (start) +++++
                cd $dexp/archive
                screen -dmS $scrsession bash -c "./run_gcmarch.sh >> stderr_gcmarch 2>&1"
                cd - >/dev/null 
                #+++++ cd to dexp/archive ( end ) +++++
                blrunning=true
            else
                break
            fi
        done 
    done

    return
}

update_runpstscr(){
    #description:   copy and edit updated gcmarch script to entered exp dir.
    local _thisinput=$1
    
    [[ -d $_thisinput ]] && local bldir=true || local bldir=false
    [[ -f $_thisinput ]] && local blfile=true || local blfile=false
    
    #todo:  check user input
    $bldir && $blfile && die "user input is unrecognizable form"
    ! $bldir && ! $blfile && die "user input is unrecognizable form"

    if $bldir && ! $blfile;then
        local _arrdexp=( $_thisinput )
    elif ! $bldir && $blfile;then
        local _arrdexp=($( cat $_thisinput ))
    fi

    for _dexp in ${_arrdexp[@]};do
        exp_updrunpostscr $_dexp
    done 

    return
}


update_archscr(){
    #description:   copy and edit updated gcmarch script to entered exp dir.
    local _thisinput=$1
    local arcqid=none
    [[ -d $_thisinput ]] && local  bldir=true || local  bldir=false
    [[ -f $_thisinput ]] && local blfile=true || local blfile=false
    
    #todo:  check user input
    $bldir && $blfile && die "user input is unrecognizable form"
    ! $bldir && ! $blfile && die "user input is unrecognizable form"

    if $bldir && ! $blfile;then
        local _arrdexp=( $_thisinput )
    elif ! $bldir && $blfile;then
        local _arrdexp=($( cat $_thisinput )) 
    fi

    for _dexp in ${_arrdexp[@]};do
        exp_updarchscr $arcqid $_dexp
    done 

    return
}

cron_headercheck() {
    grep TERM $fcron >/dev/null
    if (( $? > 0 ));then
       # wmessage "!!! cron is missing a header"
       local _bl=false
   else
       local _bl=true
    fi
    echo $_bl
}

add_cron(){
    #description:   set cron for gcmarch for entered exp.
    local _arrdexp=( "$@" )
    local _ftmp=tmp_${strscr}_${FUNCNAME[0]}
    local blheaderexists=$( cron_headercheck )
    local dexp

    for dexp in ${_arrdexp[@]};do

        local ferr=$dexp/archive/stderr_gcmarch
    
        #todo:  define necessary variables 
        set_rstfcstdate $dexp
        
        grep $dexp $fcron >/dev/null
        if (( $? != 0 )); then
            echo "*/15 * * * * /usr/bin/env bash $dexp/archive/run_gcmarch.sh >> $ferr 2>&1" >> $fcron
            wmessage "cron added for $dexp"
        fi
    done

    if ! $blheaderexists && [[ -f $fcronheader ]] ;then
        cat $fcronheader $fcron >> $_ftmp
        mv $_ftmp $fcron
        wmessage "*header added to cron" 
    fi
    
    [[ -f $_ftmp ]] && rm -f $_ftmp
            
    ssh $hstcron crontab $fcron

    return
}

rm_cron(){
    #description:   rm cron for gcmarch for entered exp.

    local _arrdexp=( "$@" )
    local _ftmp=tmp_${strscr}_${FUNCNAME[0]}
    local blheaderexists=$( cron_headercheck )
    local dexp
    
    [[ -f $_ftmp ]] && rm -f $_ftmp

    for dexp in ${_arrdexp[@]};do
        local numline=$( grep -n $dexp $fcron | cut -d':' -f1 2>/dev/null)
        sed -i "${numline}d" $fcron
        if (( $? == 0 ));then
            ssh $hstcron crontab $fcron
            status_rmcro=$?
            (( $status_rmcro == 0 )) && wmessage "cron removed for $dexp."
        fi
    done

    if ! $blheaderexists && [[ -f $fcronheader ]] ;then
        cat $fcronheader $fcron >> $_ftmp
        mv $_ftmp $fcron
        wmessage "*header added to cron" 
        ssh $hstcron crontab $fcron
    fi
    
    [[ -f $_ftmp ]] && rm -f $_ftmp

    return
}

create_winnersymlinks_lfe() {
    local _icyyyymm=$1 
    local fwin 
    local _int_userinput=$( misc_isinteger $_icyyyymm ) 

    if [[ "$_icyyyymm" == "all" ]];then 
        local arrfwin=($( find $dout_pckwinners/* -type f -name "$strfwin*.txt"  )) 
    elif (( $_int_userinput == 0 ));then 
        local arrfwin=($( find $dout_pckwinners/* -type f -name "${strfwin}*_${strexpid}_${_icyyyymm}_*.txt"  )) 
    else
        die "user input is unrecognizable format"
    fi

    for fwin in ${arrfwin[@]};do
        icyyyymm=$( echo $(basename $fwin ) | rev | cut -d'_' -f2 | rev )
        arrwin=($( cat $fwin | sed 's# #/#g' ))
        create_symlinks_arc $icyyyymm ${arrwin[@]}
    done

    return
}

print_winner_yyyymm() {
    local arricyyyymm=($( getyyyymm ))
    local fwin _yyyymm
    
    for _yyyymm in ${arricyyyymm[@]};do
        if (( $_yyyymm >= $startyyyymm && $_yyyymm <= $endyyyymm ));then
            local arrfwin=($( find $dout_pckwinners/* -type f -name "${strfwin}*_${_yyyymm}_[0-9]*.txt"  )) 
            for fwin in ${arrfwin[@]};do
                local icyyyymm=$( echo $(basename $fwin ) | rev | cut -d'_' -f2 | rev )
                local arrwin=($( cat $fwin | sed 's# #/#g' ))
                printf "${DFCST}/%s\n" ${arrwin[@]}
            done
        fi
    done

    return
}

arc_winners(){
    #description: arhive sst files and pckwinner output files (winners_*, plots, & nino_* )
    local icyyyymm=$usericyyyymm
    local  icmonth=$( echo $icyyyymm | cut -c5- )
    local   icyear=$( echo $icyyyymm | cut -c1-4 )
    local  fyear=$( fcal_nextmonth $icmonth $icyear | cut -c1-4 )
    local fmonth=$( fcal_nextmonth $icmonth $icyear | cut -c5- )

    local arrdatestamps4=($( fcal_icall $fmonth $fyear ))

    #todo:  get required file name and its fullpath
    arrftar=($( get_fout ${arrdatestamps4[@]} ))

    #todo:  check if all files are available. set blftaravail var.
    arrsst_unavail=($( printf '%s\n' ${arrftar[@]} | xargs -i bash -c "[[ ! -f {} ]] && echo {}" ))
    
    #todo:  exit if there are missing files.
    (( ${#arrsst_unavail[@]} > 0 )) && wmessage "Some or all sst daily tar files are NOT available" && return

    #todo:  start archiving
    if $blnode_nas || [[ "$hstshort" == "pfe" ]];then

        if $blhstarcavail;then 
            msg_wheader_userdefined 40 - "Archiving - $icyyyymm"
            archive_pfe2lfe
        fi
        
    elif $blnode_nccs || [[ "$hstshort" == "dis" ]];then
        wmessage "UNDER CONSTRUCTION" 
        #msg_wheader_userdefined 40 - "Archiving"
        #archive_dis2lfe ${arrftar[@]}
    fi

    return
}

archive_pfe2lfe () {
    #description:   archive necessary daily output and output text files.
    local darch=$DARCH/$reg
    local strat_yyyy=$( echo $( fcal_nextmonth $fmonth $fyear ) | cut -c1-4 )
    local strat_yyyymm=$( fcal_nextmonth $fmonth $fyear )
    local fshiftin=$cdir/${strscr}_${FUNCNAME[0]}_${strexpid}_${icyyyymm}_shiftcin
    local findexes=${reg}_${strexpid}_${icyyyymm}_${strat_yyyymm}.txt
    local  fwinner=winners_${reg}_${strexpid}_${icyyyymm}_${strat_yyyymm}.txt
    local     fplt=subsample_${reg}_${strexpid}_${icyyyymm}_${strat_yyyymm}.png


    [[ -z $blrsync ]] && blrsync=false

    #todo:  create sfhitc input batch file. 
    if ! $blrsync;then
        msg_newfile $fshiftin
        local arrftar_arch=($( printf "$hstarc:%s\n" ${arrftar[@]} | sed 's#'$DFCST'#'$darch/sst'#g' | sed 's#'$collsst'/##g' ))
        paste <(printf '%s\n' ${arrftar[@]}) <( printf '%s\n' ${arrftar_arch[@]} ) | sed 's#\t# #g' >> $fshiftin
        echo $dout_pckwinners/$fwinner $hstarc:$darch/$fwinner >> $fshiftin
        echo $dout_pckwinners/$fplt $hstarc:$darch/$fplt >> $fshiftin
        echo $dout_pckwinners/$findexes $hstarc:$darch/$findexes >> $fshiftin
        [[ ! -f $fshiftin ]] && die "$fshiftin does not exist" 
    fi 

    wmessage "$(date +'%m/%d/%Y %H:%M' ) Start Archiving:" 
    wmessage
    
    #todo:  archive ftar on lfe 
    if $blrsync;then
        wmessage "archive sst files..."

        for ftar in ${arrftar[@]};do
            local fdir=$( echo ${ftar#$DFCST/} | cut -d'/' -f1-2 )
            local fname=$( echo $ftar | rev | cut -d'/' -f1 | rev )
            local ddst=$DARCH/$reg/sst/$fdir        
            
            #todo:  check if destination dir exists on lfe
            if (( $writetofile == 1 ));then
                ssh -q $hstarc test -d $ddst >> $fmessage 2>&1
                (( $? > 0 )) && ssh -q $hstarc mkdir -p $ddst >> $fmessage 2>&1
                
                ssh -q $hstarc rsync -az $ftar $DARCH/$reg/sst/$fdir/ >> $fmessage 2>&1
            else
                ssh -q $hstarc test -d $ddst 
                (( $? > 0 )) && ssh -q $hstarc mkdir -p $ddst 
                
                ssh -q $hstarc rsync -az $ftar $DARCH/$reg/sst/$fdir/ 
            fi
        done

        wmessage "archiveing output files..."
        
        #todo:  archive output files 
        if (( $writetofile == 1 ));then
            ssh -q $hstarc rsync -az $dout_pckwinners/*_${icyyyymm}_* $DARCH/$reg/ >> $fmessage 2>&1
        else
            ssh -q $hstarc rsync -az $dout_pckwinners/*_${icyyyymm}_* $DARCH/$reg/
        fi

    else
        if (( $writetofile == 1 ));then
            /usr/local/bin/shiftc --no-cron --no-mail --wait -d < $fshiftin >> $fmessage
            local status_sftc=$?
        else
            /usr/local/bin/shiftc --no-cron --no-mail --wait -d < $fshiftin
            local status_sftc=$?
        fi
        (( $status_sftc == 0 )) && [[ -f $fshiftin ]] && rm -f $fshiftin
    fi

    wmessage

    #cp_outputs
    archfcheck

    return
}

archive_dis2lfe() {
    #description:   archive sst file in $dout/sst dir insted of on lfe/dirac.
    local _arr=( "$@" )
    local ftar 

    #/discover/nobackup/projects/gmao/t2ssp/GEOSS2S3/GEOS_util/aborovik_sst/20150630/ens505/geosgcm_sst/20150630.geosgcm_sst.daily.201508.nc4.tar
    #dout=$cdir/output/pckwinners/scratch
    #ftar=sst/20150630/ens505/20150630.geosgcm_sst.daily.201508.nc4.tar
    #darch=/u/gmaofcst/GEOS_S2Sv3
    #
    ##ref:https://unix.stackexchange.com/a/496181
    #sup rsync -aR $dout/./$ftar lfe:$darch

    wmessage "$(date +'%m/%d/%Y %H:%M' ) Start Archiving:" 
    wmessage
    
    #todo:  create symbolic links in scratch/sst dir
    for ftar in ${_arr[@]};do 
        local _dinsst=$( dirname $ftar | sed 's#'$DFCST'#'$dnino/sst'#g' | sed 's#'$collsst'##g' )

        #todo:  create dir & subdir under scratch dir in a way that should be created in destination dir
        [[ ! -d $_dinsst ]] && mkdir -p $_dinsst

        #!!!!! NOTE: as of 05/20/2021 sst tar is copied into dout/scratch sst
        #todo:  create symlink of a file (not a dir) or copy file
        #+++++ cd to dnino/sst (start) +++++
        cd $_dinsst
        #ln -sf $ftar .
        rsync -az $ftar .
        cd - >/dev/null
        #+++++ cd to dnino/sst ( end ) +++++
    done
        
    #todo:  copy fwinner and findexes files into sst
    [[ -f $dout_pckwinners/$fwinner ]] && cp -p $dout_pckwinners/$fwinner $dnino/
    [[ -f $dout_pckwinners/$findexes ]] && cp -p $dout_pckwinners/$findexes $dnino/
    [[ -f $dout_pckwinners/$fplt ]] && cp -p $dout_pckwinners/$fplt $dnino/

    if [[ $hstarc != $hst ]];then
        #todo:  transfer file with -R opt in rsync. Don't forget to add '.' in front of dir which 
        #       may need to be created at destination.
        #ref:https://unix.stackexchange.com/a/496181
        sup rsync -arLR $dnino/./* $hstarc:$darch

        #todo:  check if files are archive properly.
        archfcheck
        rm -rf $dnino
    else
        local arrdnino=($( find $dnino/* -maxdepth 0 -type d )) 
        
        #todo:  delete dir in darch
        [[ -d $darch/sst ]] && rm -rf $darch/sst
        mv -f $dnino/* $darch/
        rm -rf $dnino
    fi
    
    #todo:  copy contents of fwinner and findexes into fmessage
    cp_outputs

    return
}

archfcheck() {
    #description:   check if files are archive properly.
    local cntfail=0
    local mycmd="ssh -q $hstarc"
    local _arrf=( $fplt $findexes $fwinner )
    local _status cntfail afile

    #note: add sup to mycmd if this script is executed on discover.
    [[ $hstshort == "dis" ]] && mycmd="sup $mycmd"
        
    #todo:   check if three outputs exist on lfe
    for afile in ${_arrf[@]};do
        $mycmd "test -f $darch/$afile"
        _status=$?
        cntfail=$(( cntfail + _status ))
    done

    if (( $cntfail > 0 ));then
        die "one or more of these files, $( basename $fplt ), $( basename $findexes), & $( basename $fwinner ), are not archived on $hstarc"
    fi

    return
}



get_fout() {
    #description:   get required file name and its path
    local _arricall=( "$@" )
    local _lastfdate=${_arricall[-1]}
    local _yyyymm_twomonplus=$( fcal_nextmonth $fmonth $fyear )
    local icdate _arrout fout
    
    #20151102.geosgcm_sst.daily.201511.nc4.tar
    for icdate in ${_arricall[@]};do
        
        #todo: get output file name
        fout=$icdate.$collsst.$_yyyymm_twomonplus.nc4.tar
        _arrout+=($( echo ${arrens4[@]} | sed 's#[^ ]* *#'$DFCST/$icdate/'&#g' | xargs -L 1 printf '%s'"/$collsst/${fout}"'\n' ))

        #todo:   do the same for extra ensembles for the last icdate.
        if (( $icdate == $_lastfdate ));then
            _arrout+=($( echo ${arrens10[@]} | sed 's#[^ ]* *#'$DFCST/$icdate/'&#g' | xargs -L 1 printf '%s'"/$collsst/${fout}"'\n' ))
        fi
    done
    echo ${_arrout[@]}     
}


print_output() {
    local arr=( "$@" ) 
    local dexp capr
            
    [[ ! -f $ftmp1 ]] && touch $ftmp1

    for dexp in ${arr[@]};do
        local arrtmp_restart=()
       
        [[ ! -d $dexp ]] && continue

        #+++++ cd $dexp (start) +++++
        cd $dexp
    
        local arrfout=($( find $dexp/* -maxdepth 0 -type f -name "*.o[0-9]*" | sort -V ))
       
        local yyyymmddensm=$( echo $dexp | rev | cut -d'/' -f1-2 | rev | sed 's#/##g' )

        for fout in ${arrfout[@]};do
            #local _fname=${_fname1#*$DFCST/}
            local _fname=$( basename $fout )
            local outjstop1=$( grep "$strjstop" $fout | tail -1 | cut -d':' -f2- | sed 's/ ^*//' )
            local outjstop=$( date -d "$outjstop1" +%Y%m%d_%H%M 2>/dev/null )

            local outwtime=$( grep "$strwtime" $fout 2>/dev/null | tail -1 | cut -d':' -f2- | sed 's/ ^*//' )
            local outqueue=$( grep "$strqueue" $fout 2>/dev/null | tail -1 | cut -d':' -f2- | sed 's/ ^*//' )
           
            grep $_fname $ftmp1 2>&1 >>/dev/null 
            local status_grep=$?
            (( $status_grep == 0 )) && continue
            [[ -z $outwtime || -z $outqueue ]] && continue

            #todo:  get cap_restart for this segment
            local capr=$( grep "Current Date =" $fout 2>/dev/null | head -1 | tr -s '[:space:]' | rev | cut -d' ' -f1 | rev | sed "s#/##g" )
            local capend=$( grep "AGCM Date:" $fout 2>/dev/null | tail -1 | tr -s '[:space:]' | rev | cut -d' ' -f3 | rev | sed "s#/##g" )

            if [[ -n $capend && -n $capr ]] && (( $capr > 0 && $capend > 0 ));then 
                local _int_capr=$( misc_isinteger $capr ) 
                local _int_capend=$( misc_isinteger $capend ) 

                if (( $_int_capr == 0 && $_int_capend == 0 ));then 
                    local _capr_sec=$( date -d $capr +%s )
                    local _capend_sec=$( date -d $capend +%s )
                    local _sim_days=$( echo "( $_capend_sec - $_capr_sec ) / 3600 /24 " | bc ) 
                else
                    continue
                fi
            else
                continue
            fi

            #todo:  get rank0 (nodes)
            local rank0=$( grep "$strrank0" $fout | rev | cut -d' ' -f1 | rev )
        
            #todo:  check PANICSTOP file
            local numfpanic=$( find $dexp/* -maxdepth 0 -name PANICSTOP 2>/dev/null | wc -l  )
            if (( $numfpanic > 0 ));then
                blpanic=true
            else
                blpanic=""
            fi
            
            #todo:  check if exp ran out a requested walltime
            grep "PBS: job killed: walltime" $fout >/dev/null
            local status_grep=$?
            if (( $status_grep == 0 ));then
                #local strwtimeexceeded="walltime exceeded limit"
                local strwtimeexceeded="true"
            else
                local strwtimeexceeded=""
            fi
            echo $_fname\;$capr\;$_sim_days\;"$outjstop"\;"$outwtime"\;"$outqueue"\;"$rank0"\;"$strwtimeexceeded"\;$blpanic >> $ftmp1

        done

        ##todo:  count a number of re-runs.
        #local arrcapr=($( printf '%s\n' ${arrtmp_restart[@]} | sort | uniq ))
        #for capr in ${arrcapr[@]};do
        #    #local numfout=$( find $DFCST/* -type f -name "R$yyyymmddensm.*$capr.out" 2>/dev/null | wc -l )
        #    local numfout=$( printf '%s\n' ${arrtmp_restart[@]} | sort | grep $capr | wc -l )
        #    (( $numfout > $mincnts )) && echo $yyyymmddensm\;$capr\;$numfout >> $ftmp2
        #done 

        cd - > /dev/null 
        #+++++ cd $dexp ( end ) +++++
    done

    #todo:  count a number of re-runs.
    local _arryyyymmddensm=($( cat $ftmp1 | cut -d'/' -f1-2 | sort -V | uniq ))
    for yyyymmddensm in ${_arryyyymmddensm[@]};do 
        local arrcapryyyymmdd=($( grep "$yyyymmddensm/" $ftmp1 | cut -d';' -f2 | uniq ))
        for capryyyymmdd in ${arrcapryyyymmdd[@]};do
            local numcapryyyymmdd=($( grep "$yyyymmddensm/" $ftmp1 | grep $capryyyymmdd | wc -l ))
            (( $numcapryyyymmdd > $mincnts )) && echo $yyyymmddensm\;$capryyyymmdd\;$numcapryyyymmdd >> $ftmp2
        done
    done 

    local _sort_jobstopped=true
    if $_sort_jobstopped;then 
        cat $ftmp1 | sort -V -t';' -k3 >> $ftmp3
        echo "$title_wtime" >> $ftmp4
        cat $ftmp3          >> $ftmp4
        mv $ftmp4 $ftmp3
    else
        echo "$title_wtime" >> $ftmp3
        cat $ftmp1 >> $ftmp3
    fi

    if [[ -s $ftmp3 ]];then 
        #rev $ftmp3 | column -t -s';' | rev >> $fmessage
        column -t -s';' $ftmp3 >> $fmessage
        echo >> $fmessage
    fi
    if [[ -s $ftmp2 ]];then 
        echo ""                >> $fmessage
        echo "$title_rerun"    >> $fmessagee
        column -t -s';' $ftmp2 >> $fmessage
    fi
  
    ##todo:  save ftmp1 if it's different from the previous one 
    #if [[ -s $ftmp1_previous ]];then
    #    diff -q $ftmp1 $ftmp1_previous 2>&1 >>/dev/null
    #    (( $? > 0 )) && mv $ftmp1 $_dstdout/$( basename $ftmp1 ) 
    #else
    #    mv $ftmp1 $_dstdout/$( basename $ftmp1 ) 
    #fi

    return
}

check_panicstop() {    
    local arr=( "$@" ) 
    local dexp
            
    for dexp in ${arr[@]};do
        set_rstfcstdate $dexp

        #+++++ cd to dexp (start) +++++
        cd $dexp
    
        shopt -s nullglob
        local numfpanic=$( find * -name PANICSTOP 2>/dev/null | wc -l  )
        shopt -u nullglob

        (( $numfpanic > 0 )) && echo $fcstdate/$ensm 

        cd - > /dev/null 
        #+++++ cd to dexp ( end ) +++++
    done
    return
}

getwtime() {
    local arricyyyymm=($( getyyyymm ))
    local _dstdout=$dstdout/${FUNCNAME[0]}/$strdout
    local fmessage=$dmess/message_${strscr}_${FUNCNAME[0]}_${startyyyymm}_${endyyyymm}
    local ftmp1=$cdir/tmp_${strscr}_${FUNCNAME[0]}_1
    local ftmp2=$cdir/tmp_${strscr}_${FUNCNAME[0]}_2
    local ftmp3=$cdir/tmp_${strscr}_${FUNCNAME[0]}_3
    local ftmp4=$cdir/tmp_${strscr}_${FUNCNAME[0]}_4
    
    local blcheckpanic=false
    local blgetwtime=true
    local _blsendmsgall=false
    local mincnts=3
    
    local strjstop="Job Stopped"
    local strwtime="Walltime Used"
    local strqueue="Execution Queue"
    local strrank0="g5_modules: Setting BASEDIR and MOMDIR and modules for"
    local strwtexceeded="WtimeExeeded"

    local title_panic="Exp Dir with PANICSTOP"
    local title_wtime="Exp Dir and Stdout"\;"cap_restart"\;"SimDays"\;"${strjstop} (YYYYMMDD_HHMM)"\;"$strwtime"\;Queue\;Rank0\;"$strwtexceeded"\;PanicStop
    local title_rerun=Exp\;"cap_restart"\;"Total Run Counts ( >$mincnts )"
    local sortby=3
    local arrdexp1=()

    #note:  when opt w is selected, getwtime will show you outputs 
    #       written in fmessage. But sendmsg will delete fmessage
    #       if outputs is the same as the previous output.
    local writetofile=1
    #local writetofile=0

    #local msg_hrpref=7
    #local msg_hrinc=3
    local msg_subject="${hstshort}.${strscr} ${FUNCNAME[0]}: $strexpid (${startyyyymm} - ${endyyyymm})"
    
    [[ -f $ftmp1 ]] && rm -f $ftmp1
    [[ -f $ftmp2 ]] && rm -f $ftmp2
    [[ -f $ftmp3 ]] && rm -f $ftmp3
    [[ -f $ftmp4 ]] && rm -f $ftmp4
    
    #todo:  get dexp path
    for yyyymm in ${arricyyyymm[@]};do
        get_beg_and_end $yyyymm $yyyymm
        arrdexp1+=($( get_dexp $DFCST $begyyyymm $endyyyymm ${arrintens[@]} ))
    done

    local arrfinal=($( printf '%s\n' ${arrdexp1[@]} | xargs -i bash -c "[[ ! -f {}/clean_completed ]] && echo {}" | sort -V ))

    [[ ! -d $_dstdout ]] && mkdir -p $_dstdout

    msg_wheader
    wmessage "Parent Dir : $DFCST"
    wmessage
    
    if $blcheckpanic;then
        echo "$title_panic" >> $ftmp2
        check_panicstop ${arrfinal[@]}
    fi
    
    if $blgetwtime;then
        print_output ${arrfinal[@]}
        wmessage
    fi

    if $blshowout;then
        [[ -f $fmessage ]] && cat $fmessage
        echo "** The above table is written in $fmessage"
    fi
    
    #todo:  send email
    if [[ -f $fmessage ]];then 
        #todo:  count number of non-empty lines below the 6th line (below header)    
        local _numln=$( cat $fmessage | tail -n +4 | wc -l )
        if (( $writetofile == 1 && $_numln > 0 ));then 
            sendmsg ${FUNCNAME[0]} $_blsendmsgall $_dstdout $fmessage "$msg_subject"
        fi
    fi

    [[ -f $ftmp1 ]] && rm -f $ftmp1
    [[ -f $ftmp2 ]] && rm -f $ftmp2
    [[ -f $ftmp3 ]] && rm -f $ftmp3
    [[ -f $ftmp4 ]] && rm -f $ftmp4
    
    return

}

clean_shiftc(){
    local strfield_date=7
    local strfcst=GEOS_fcst
    local dfcst=/nobackupp11/gmaofcst/GEOSS2S3/GEOS_fcst
    local dfcstrome=/nobackupp11/knakada/GEOSS2S3/GEOS_fcst
    #local blstopunknown=false
    local blstopunknown=true
    local arrsftcrunid=($( shiftc --status=csv | grep run | tail -n +2 | cut -d',' -f1 ))
    local arrsftciddexp=() 
    
    for sftcrunid in ${arrsftcrunid[@]};do
        local sftcdate=$( shiftc --status=csv | grep $sftcrunid | cut -d',' -f$strfield_date )
    
        #todo:  check if shiftc id is running more than a day (=d)  
        if [[ "$sftcdate" =~ d ]];then
            local sftcop=$( shiftc --id=$sftcrunid --status=csv | grep -v done | tail -1 | cut -d',' -f3 )
            local yyyymmddensm=$( echo ${sftcop#*${strfcst}/} | cut -d'/' -f1-2 ) 
            local dexp=$dfcst/$yyyymmddensm
    
            if [[ ! -d $dexp ]];then 
                local dexp=$dfcstrome/$yyyymmddensm
                [[ ! -d $dexp ]] && echo "$yyyymmddensm dir does not exist in either $dfcst or $dfcstrome" && continue 
            fi
            arrsftciddexp+=( $sftcrunid:$dexp )
        else
            continue   
        fi
    done
    
    wmessage "a number of shiftc operations running more than a day : ${#arrsftciddexp[@]}"
    (( ${#arrsftciddexp[@]} == 0 )) && wmessage "exit" && exit
    
    for sftciddexp in ${arrsftciddexp[@]};do
        local i=1
        local sftcid=$( echo $sftciddexp | cut -d':' -f$i ); i=$(( i + 1 ))
        local   dexp=$( echo $sftciddexp | cut -d':' -f$i )
    
        #todo:  check if sftcrunid is written in stderr file in archive
        local fstderr=$( find $dexp/archive/* -type f -name "stderr_*" 2>/dev/null )
    
        if [[ -f $fstderr ]];then 
            grep $sftcid $fstderr >/dev/null
            local status_grep=$?
            if (( $status_grep == 0 ));then
                local blsftcid_pass=true
            else
                local blsftcid_pass=false
            fi
           
            local numarchlock=$( find $dexp/archive/* -type f -name '*lock' 2>/dev/null | wc -l ) 
            (( $numarchlock == 1 )) && local blarchlockexist=true || local blarchlockexist=false
    
            #todo:  stop shiftc operation. 
            if $blsftcid_pass && ! $blarchlockexist;then
                shiftc --id=$sftcid --stop
                wmessage "$sftcid - stopped"
            else
                if $blstopunknown ;then
                    shiftc --id=$sftcid --stop 
                    wmessage "$sftcid - stopped (a host exp is unknown - possibly $dexp )" 
                else
                    wmessage "$sftcid - a host exp is unknown ( possibly $dexp )" 
                fi
            fi
        fi 
    done 
    return 
}

count_lferst() {
    #description:   count rst on lfe
    local _dstdout=$dstdout/${FUNCNAME[0]}/$strdout
    local fmessage=$dmess/message_${strscr}_${FUNCNAME[0]}
    local msg_subject="${hstshort}.${strscr} ${FUNCNAME[0]}: $strexpid"
    local _ftmp=$cdir/tmp_${strscr}_${strexpid}_${FUNCNAME[0]}
    local _blsendmsgall=false
    local icyyyymm
    local _arrnumfsupposed=( - 6 5 7 6 6 6 6 6 6 6 6 7 )
    
    [[ ! -d $_dstdout ]] && mkdir -p $_dstdout
    [[ -f $_ftmp ]] && rm -f $_ftmp

    #todo:  get yyyymm
    local _arrftar=($( grep tar $flst_lfe | rev | cut -d'/' -f1 | rev ))
    local _arricyyyy=($( cat $flst_lfe | grep tar | rev | cut -d' ' -f1 | cut -d'/' -f1 | rev | cut -c1-4 | sort -n | uniq ))
    local _arricyyyymm=($( printf '%s\n' ${_arricyyyy[@]} | xargs -i printf "{}%02g\n" $( seq 1 12 )  ))

    #todo:  write header
    echo IC YYYYMM\;# of Control\;# of Perturbation\;# should be\;# of Missing\;Control Missing?\;Perturbation Missing? >> $_ftmp
    for icyyyymm in ${_arricyyyymm[@]};do
        local intm=$( date -d ${icyyyymm}01 +%_m )
        local numf_supposed=${_arrnumfsupposed[$intm]}
        local numf_cont=$( printf '%s\n' ${_arrftar[@]} | grep "^$icyyyymm" | grep -v _pert | sort -V | wc -l )
        local numf_pert=$( printf '%s\n' ${_arrftar[@]} | grep "^$icyyyymm" | grep _pert | sort -V | wc -l )

        if (( $numf_cont > 0 || $numf_pert > 0 ));then

            #todo:  check if there are missing files
            if (( $numf_cont < $numf_supposed ));then 
                local _blcontmiss=true
                local _numfcont_miss=$( echo "$numf_supposed - $numf_cont" | bc ) 
            elif (( $numf_cont > $numf_supposed ));then
                local _blcontmiss="?"
                local _numfcont_miss=0
            else
                local _blcontmiss=""
                local _numfcont_miss=0
            fi

            if (( $numf_pert < $numf_supposed ));then 
                local _blpertmiss=true 
                local _numfpert_miss=$( echo "$numf_supposed - $numf_pert" | bc ) 

            elif (( $numf_pert > $numf_supposed ));then
                local _blpertmiss="?"
                local _numfpert_miss="0"
            else
                local _blpertmiss=""
                local _numfpert_miss="0"
            fi
            
            local numftot_miss=$( echo "$_numfpert_miss + $_numfcont_miss" | bc ) 
            if (( $numftot_miss == 0 ));then
                #echo $icyyyymm\;$numf_cont\;$numf_pert\;$numf_supposed\;\;${_blcontmiss^}\;${_blpertmiss^} >> $_ftmp
                :
            else
                echo $icyyyymm\;$numf_cont\;$numf_pert\;$numf_supposed\;$numftot_miss\;${_blcontmiss^}\;${_blpertmiss^} >> $_ftmp
            fi
        fi
    done 

    msg_wheader

    if (( $writetofile == 1 ));then 
        rev $_ftmp | column -t -s';' | rev >> $fmessage
    else
        rev $_ftmp | column -t -s';' | rev 
    fi

    #todo:  send email
    if [[ -f $fmessage ]];then 
        #todo:  count number of non-empty lines below the 6th line (below header)    
        local _numln=$( cat $fmessage | tail -n +4 | wc -l )
        if (( $writetofile == 1 && $_numln > 0 ));then 
            sendmsg ${FUNCNAME[0]} $_blsendmsgall $_dstdout $fmessage "$msg_subject"
        fi
    fi


    [[ -f $_ftmp ]] && rm -f $_ftmp

    return 
}

count_pferst() {
    #description:   count available rst on pfe.
    local _ftmp=$cdir/tmp_${strscr}_${strexpid}_${FUNCNAME[0]}
    local _dstdout=$dstdout/${FUNCNAME[0]}/$strdout
    local fmessage=$dmess/message_${strscr}_${FUNCNAME[0]}
    local msg_subject="${hstshort}.${strscr} ${FUNCNAME[0]}: $strexpid "
    local _blsendmsgall=false
    local _blwheader=false

    local flist=wftmsz_flist
    local icyyyymm

    [[ ! -d $_dstdout ]] && mkdir -p $_dstdout
    [[ -f $_ftmp ]] && rm -f $_ftmp

    #+++++ cd $DRST (start) +++++
    cd $DRST
    local arrdicyyyymmdd=($( find * -maxdepth 0 -type d -name '[1-9]???????' ))
    

    for icyyyymmdd in ${arrdicyyyymmdd[@]};do

        local icdate=$( date -d "$icyyyymmdd +1days" +%Y%m%d ) 
        local bllasticdateofthemonth=$( fcal_calclasticdateofthemonth $icdate ) 

        for ensm in ${arrens4[@]};do
            local dens=$icyyyymmdd/$ensm
            local blatmfexist=false
            local blocnfexist=false

            if [[ "${arrens4[0]}" == $ensm ]];then
                local numfatm=22
                local numfocn=11
                local numfatm_sl=0
                local numfocn_sl=0

                if [[ -d $dens ]];then
                    local numfatm=$( find $dens/* -type f -name "*_rst" 2>/dev/null | wc -l )
                    local numfocn=$( find $dens/RESTART/* -type f -name "*.nc" 2>/dev/null | wc -l )
                    #local numfatm=$( grep $dens $flist  2>/dev/null | grep _rst 2>/dev/null | wc -l ) 
                    #local numfocn=$( grep $dens/RESTART 2>/dev/null | grep .nc  2>/dev/null | wc -l )
                    (( $numfatm == 22 )) && blatmfexist=true || blatmfexist=false
                    (( $numfocn == 11 )) && blocnfexist=true || blocnfexist=false
                else
                    blatmfexist=false
                    blocnfexist=false
                fi

            else
                local numfatm=0
                local numfocn="-"
                local numfatm_sl=0
                local numfocn_sl=0

                if [[ -d $dens ]];then
                    local numfatm=$( find $dens/* -type f -name "*_rst" 2>/dev/null | wc -l )
                    local numfatm_sl=$( find $dens/* -type l -name "*_rst" 2>/dev/null | wc -l )
                    local numfocn_sl=$( find $dens/RESTART/* -type l -name "*.nc" 2>/dev/null | wc -l )

                    (( $numfatm    == 2  )) && (( $numfatm_sl == 20 )) && blatmfexist=true || blatmfexist=false
                    (( $numfocn_sl == 11 )) && blocnfexist=true || blocnfexist=false
                else
                    blatmfexist=false
                    blocnfexist=false
                fi
            fi
              
            if $blatmfexist && $blocnfexist;then
                :
            else
                #todo:  write a header            
                ! $_blwheader && echo ICYYYYMMDD\;Ensemble\;# of Atm Rst file\;# of Atm Rst Symlink\;# of Ocn Rst file\;# of Ocn Rst Symlink\;Atm Rst Exist?\;Ocn Rst Exist? >> $_ftmp && local _blwheader=true
                echo $icyyyymmdd\;$ensm\;$numfatm\;$numfatm_sl\;$numfocn\;$numfocn_sl\;${blatmfexist^}\;${blocnfexist^} >> $_ftmp
            fi
        done
        
        if $bllasticdateofthemonth;then 
            for ensm in ${arrens10[@]};do
                local dens=$icyyyymmdd/$ensm
        
                local numfatm="-"
                local numfocn=0
                local numfatm_sl=0
                local numfocn_sl=0
                local blatmfexist=false
                local blocnfexist=false
                
                if [[ -d $dens ]];then
                    local numfocn=$( find $dens/RESTART/* -type f -name "ocean_*" 2>/dev/null | wc -l )
                    local numfatm_sl=$( find $dens/* -type l -name "*_rst"  2>/dev/null| wc -l )
                    local numfocn_sl=$( find $dens/RESTART/* -type l -name "*.nc" 2>/dev/null | wc -l )

                    (( $numfocn    == 2 && $numfocn_sl == 9 )) && blocnfexist=true || blocnfexist=false
                    (( $numfatm_sl == 22 )) && blatmfexist=true || blatmfexist=false

                else
                    blatmfexist=false 
                    blocnfexist=false 
                fi
                if $blatmfexist && $blocnfexist;then 
                    :
                else
                    ! $_blwheader && echo ICYYYYMMDD\;Ensemble\;# of Atm Rst file\;# of Atm Rst Symlink\;# of Ocn Rst file\;# of Ocn Rst Symlink\;Atm Rst Exist?\;Ocn Rst Exist? >> $_ftmp && local _blwheader=true
                    echo $icyyyymmdd\;$ensm\;$numfatm\;$numfatm_sl\;$numfocn\;$numfocn_sl\;${blatmfexist^}\;${blocnfexist^} >> $_ftmp
                fi
            done
        else
            blocnsuccess=true
        fi
#break        
    done
    
    cd - >/dev/null
    #+++++ cd $DRST ( end ) +++++

    if [[ -s $_ftmp ]];then 
        msg_wheader

        if (( $writetofile == 1 ));then 
            rev $_ftmp | column -t -s';' | rev >> $fmessage
        else
            rev $_ftmp | column -t -s';' | rev 
        fi

        #wmessage "** Outputs are written in $_ftmp" 
        [[ -f $_ftmp ]] && rm -f $_ftmp
        
        if [[ -f $fmessage ]];then 
            #todo:  count number of non-empty lines below the 6th line (below header)    
            local _numln=$( cat $fmessage | tail -n +4 | wc -l )
            if (( $writetofile == 1 && $_numln > 0 ));then 
                sendmsg ${FUNCNAME[0]} $_blsendmsgall $_dstdout $fmessage "$msg_subject"
            fi
        fi
    fi

    return    
}


cnt_sst() {
    #local arricyyyymm=($( getyyyymm ))
    local arricyyyymm=( $usericyyyymm )
    local _dstdout=$dstdout/${FUNCNAME[0]}/$strdout
    local _blsendmsgall=false
    local icyyyymm

    for icyyyymm in ${arricyyyymm[@]};do 
        local arr=()
        local fmessage=$dmess/message_${strscr}_${FUNCNAME[0]}_${icyyyymm}
        local msg_subject="${hstshort}.${strscr} ${FUNCNAME[0]}: $strexpid ${icyyyymm}"
        local  icmonth=$( echo $icyyyymm | cut -c5- )
        local   icyear=$( echo $icyyyymm | cut -c1-4 )
        local  fyear=$( fcal_nextmonth $icmonth $icyear | cut -c1-4 )
        local fmonth=$( fcal_nextmonth $icmonth $icyear | cut -c5- )
        local strat_yyyymm=$( fcal_nextmonth $fmonth $fyear )
        local arrdatestamps4=($( fcal_icall $fmonth $fyear ))
        local arrdatestamps10=( ${arrdatestamps4[-1]} )
        local arrlabel4=($( printf '%s\n' ${arrdatestamps4[@]} | xargs -i printf '{}%s\n'  ${arrens4[@]} ))
        local arrlabel10=($( printf '%s\n' ${arrdatestamps10[@]} | xargs -i printf '{}%s\n'  ${arrens10[@]} ))
        local arrlabel=( ${arrlabel4[@]} ${arrlabel10[@]} )

        for label in ${arrlabel[@]};do
            icyyyymmdd=${label:0:8}
            ensn=${label:8}
            
            fsst=$DFCST/$icyyyymmdd/$ensn/$collsst/${icyyyymmdd}.${collsst}.${strat_yyyymm}.nc4.tar
            fsst2=$icyyyymmdd/$ensn/$collsst/${icyyyymmdd}.${collsst}.${strat_yyyymm}.nc4.tar
            
            [[ ! -s $fsst ]] && arr+=( $fsst2 )
        done

        msg_wheader $icyyyymm
        wmessage "       Experiment ID : $strexpid"
        wmessage "# of Missing sst tar : ${#arr[@]}"
        ahand_warr ${arr[@]}
        wmessage

        if (( $writetofile == 1 ));then 
            sendmsg ${FUNCNAME[0]} $_blsendmsgall $_dstdout $fmessage "$msg_subject"
            #local status_sendmsg=$?
            #(( $status_sendmsg > 0 )) && die "failed to send email"
        fi
    done
    return
}

compress_sst(){

    #dscription:   compress sst outputs for the 2nd full month  
    local fmessage=$dmess/message_${strscr}_${FUNCNAME[0]}_${usericyyyymm}
    local msg_subject="${hstshort}.${strscr} ${FUNCNAME[0]}: $strexpid ${usericyyyymm}"
    local _dstdout=$dstdout/${FUNCNAME[0]}/$strdout
    local _blsendmsgall=false
    local _blheader=false
    local _dexp
 
    [[ -z $DFCST ]] && die "DFCST is undefined" && exit 

    local _arrdexp=($( find $DFCST/* -maxdepth 1 -mindepth 1 -type d -name "ens*?" 2>/dev/null | grep $usericyyyymm | sort -V  ))
    local arrjobs1=($( /u/scicon/tools/bin/qstat -u $USER -W fmt_Queue="-maxw 40" -W o=+Rank0 | sort -V -k4 -t' ' | tr -s '[:space:]' | cut -d' ' -f4 | cut -d'.' -f1 | sort -V | uniq | grep ens | grep R ))
    local arrjobs_dexp=($( printf '%s\n' ${arrjobs1[@]} | sed -e 's#R##g' -e 's#ens#/ens#g' | xargs -i echo $DFCST/{} ))
    
    for _dexp in ${_arrdexp[@]};do
        if [[ ! "${arrjobs_dexp[@]}" =~ "$_dexp" ]];then
            if ! $_blheader;then 
                msg_wheader
                wmessage "    Experiment ID : $strexpid"
                wmessage "User YYYYMM Input : $usericyyyymm" 
                _blheader=true
            fi
            make_nonmonthly_sst $_dexp
        fi
    done

    #todo:  count number of non-empty lines below the 6th line (below header)    
    if [[ -f $fmessage ]];then 
        local _numln=$( cat $fmessage | tail -n +6 | wc -l )

        if (( $writetofile == 1 && $_numln > 0 ));then 
            sendmsg ${FUNCNAME[0]} $_blsendmsgall $_dstdout $fmessage "$msg_subject"
            #local status_sendmsg=$?
            #(( $status_sendmsg > 0 )) && die "failed to send email"
        else
            rm -f $fmessage
        fi
    fi
    return
}

arch_sst(){
      
    #local _dout=$cdir/output/pckwinners/$strexpid 
    local _dout=$dout_pckwinners 
    local  icmonth=$( echo $usericyyyymm | cut -c5- )
    local   icyear=$( echo $usericyyyymm | cut -c1-4 )

    local  fyear=$( fcal_nextmonth $icmonth $icyear | cut -c1-4 )
    local fmonth=$( fcal_nextmonth $icmonth $icyear | cut -c5- )
    local strat_yyyymm=$( fcal_nextmonth $fmonth $fyear )
    
    local arrdatestamps4=($( fcal_icall $fmonth $fyear ))

    msg_wheader
    wmessage "User Inputs : $usericyyyymm"

    local blrunhere=true
    if $blrunhere;then

        #todo:  get required file name and its fullpath
        local arrftar=($( get_fout ${arrdatestamps4[@]} ))

        wmessage "User Inputs : $usericyyyymm"
        wmessage "    Archive : ${#arrftar[@]} sst Files"
        wmessage 

        for ftar in ${arrftar[@]};do
            local fdir=$( echo ${ftar#$DFCST/} | cut -d'/' -f1-2 )
            local fname=$( echo $ftar | rev | cut -d'/' -f1 | rev )
            local ddst=$DARCH/$reg/sst/$fdir        
            
            #todo:  check if destination dir exists on lfe
            ssh -q $hostarc test -d $ddst
            (( $? > 0 )) && ssh -q $hostarc mkdir -p $ddst
            
            wmessage "Archiving $ftar"

            ssh -q $hostarc rsync -az $ftar $DARCH/$reg/sst/$fdir/
        done
    else
        wmessage "**Skip archiging sst files"
    fi


    local _arroutput=($( find $_dout/* -type f -name "*_${usericyyyymm}_*" 2>/dev/null ))

    wmessage "Archiving output files:"
    wmessage "$( printf '    %s\n' ${_arroutput[@]} )"
    wmessage 

    #todo:  archive output files 
    ssh -q $hostarc rsync -az $_dout/*_${usericyyyymm}_* $DARCH/$reg/

    return
}

get_fout() {
    #description:   get required file name and its path
    local _arricall=( "$@" )
    local _lastfdate=${_arricall[-1]}
    local _yyyymm_twomonplus=$( fcal_nextmonth $fmonth $fyear )
    local icdate _arrout fout
    
    #20151102.geosgcm_sst.daily.201511.nc4.tar
    for icdate in ${_arricall[@]};do
        
        #todo: get output file name
        fout=$icdate.$collsst.$_yyyymm_twomonplus.nc4.tar
        _arrout+=($( echo ${arrens4[@]} | sed 's#[^ ]* *#'$DFCST/$icdate/'&#g' | xargs -L 1 printf '%s'"/$collsst/${fout}"'\n' ))

        #todo:   do the same for extra ensembles for the last icdate.
        if (( $icdate == $_lastfdate ));then
            _arrout+=($( echo ${arrens10[@]} | sed 's#[^ ]* *#'$DFCST/$icdate/'&#g' | xargs -L 1 printf '%s'"/$collsst/${fout}"'\n' ))
        fi
    done
    echo ${_arrout[@]}     
}


check_arch(){
    #description:    check running archive status
    local arrdexp=()
    local arrfout=()
    local fcapric=cap_restartIC
    local fcapr=cap_restart
    local fhis2=HISTORY_2.rc
    local dscratch=scratch
    local int_hwidth=40
    local arrjid=($( qstat -u gmaofcst -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep ivy 2>/dev/null | grep -w R 2>/dev/null | cut -d' ' -f1 ))
    local blrsync=$( grep "blrsync=" run_gcmarch.sh 2>/dev/null | cut -d'=' -f2 )

    #todo:  get dexp from job id 
    if [[ -n $qid ]] ;then
        local strarch=$( echo $( pbs_dir $qid ) | rev | cut -d'/' -f1 | rev )
        if [[ "$strarch" == "archive" ]];then 
            arrdexp=($( echo $( pbs_dir $qid ) | rev | cut -d'/' -f2- | rev ))
        else
            die "$qid is not for an archiving job"
        fi
    else
        for jid in ${arrjid[@]};do
            arrdexp+=($( pbs_dir $jid | rev | cut -d'/' -f2- | rev ))
        done 
        
        arrdexp=($( printf '%s\n' ${arrdexp[@]} | sort -V | uniq ))
    fi

    for dexp in ${arrdexp[@]};do        
        local writetofile=1
        local fcstdate=$( echo $dexp | rev | cut -d'/' -f2 | rev )
        local     ensm=$( echo $dexp | rev | cut -d'/' -f1 | rev )
        local fmessage=tmp_${strscr}_${FUNCNAME[0]}_${strexpid}_${fcstdate}${ensm}
        local darch=$DARCH/$fcstdate/$ensm
        local flst_lfe=$cdir/${strscr}_${strexpid}_${FUNCNAME[0]}_$fcstdate${ensm}_lfefiles

        [[ -f $flst_lfe ]] && rm -f $flst_lfe
        [[ -f $fmessage ]] && rm -f $fmessage

        arrfout+=( $fmessage ) 

        getlfefout $dexp $darch $flst_lfe 
        
        #todo:  calculate number of outputs.
        local _fcapr=$dexp/$fcapr
        local _fcapric=$dexp/$fcapric
        local capric_yyyymmdd=$( cat $_fcapric | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
        local capr_yyyymmdd=$( cat $_fcapr | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
        capric_yyyy=$( echo $capric_yyyymmdd | cut -c1-4 )
        local _fhis2=$dexp/$fhis2
        local blleaveout=$( grep "blleaveout=" $dexp/archive/run_gcmarch.sh 2>/dev/null| cut -d'=' -f2 )

        local bllastdayofthemonth=$( fcal_lastdayofthemonth $capric_yyyymmdd )
        local arrcollnonmon=($( exp_getcollfreq n $_fhis2 ))
        local arrcollmon=($( exp_getcollfreq m $_fhis2 ))
        local arrcollnonmon_save=($( printf '%s\n' ${arrcollnonmon[@]} | grep -E '_slv|_sfc' 2>/dev/null ))
        local nummon_run=$( fcal_calcnummon $capric_yyyymmdd $capr_yyyymmdd  )
        local numcoll_save=$( echo "${#arrcollmon[@]} + ${#arrcollnonmon_save[@]}" | bc -l )
        local numcoll_notsave=$( echo "${#arrcollnonmon[@]} - ${#arrcollnonmon_save[@]}" | bc -l )
        local numcoll=$( printf '%s\n' $( exp_getcollections $_fhis2 ) | wc -l )
        local numcollnonmon=${#arrcollnonmon[@]}
        
        (( $nummon_run == 10 )) && local bl10=true || local bl10=false

        if $bllastdayofthemonth;then
            local numfall_calc=$( echo "( $nummon_run + 1 ) * $numcoll - $numcollnonmon" | bc -l )
            local numfcollsave_calc=$( echo "( $nummon_run + 1 ) * $numcoll_save - ${#arrcollnonmon_save[@]} " | bc -l )
            local numfcolldeleted_calc=$( echo "( 4 - 1 ) * $numcoll_notsave " | bc -l )
            
            local numfmon_calc=$( echo "( $nummon_run + 1 ) * ${#arrcollmon[@]}" | bc -l )
            local numfnonmon_calc=$( echo "( $nummon_run + 1 ) * $numcollnonmon - $numcollnonmon" | bc -l )
            local numfcollsavenonmon_calc=$( echo "( $nummon_run + 1 ) * ${#arrcollnonmon_save[@]} - ${#arrcollnonmon_save[@]} " | bc -l )
        else
            local numfall_calc=$( echo "$nummon_run * $numcoll - $numcollnonmon" | bc -l )
            local numfcollsave_calc=$( echo "$nummon_run * $numcoll_save - ${#arrcollnonmon_save[@]} " | bc -l )
            local numfcolldeleted_calc=$( echo "( 3 - 1 ) * $numcoll_notsave " | bc -l )

            local numfmon_calc=$( echo "$nummon_run * ${#arrcollmon[@]}" | bc -l )
            local numfnonmon_calc=$( echo "$nummon_run * $numcollnonmon - $numcollnonmon" | bc -l )
            local numfcollsavenonmon_calc=$( echo "$nummon_run * ${#arrcollnonmon_save[@]} - ${#arrcollnonmon_save[@]} " | bc -l )
    
        fi

        local numfmom_calc=$( calc_numseg $fcstdate $capr_yyyymmdd )

        #todo: counts number of output on pfe
        #+++++ cd dexp (start) +++++
        cd $dexp
        local       numfout=($( find * -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*_glo_*" 2>/dev/null | grep -v $dscratch | grep -v $collsst | wc -l )) 
        local    numfoutsst=($( find * -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*_glo_*" 2>/dev/null | grep -v $dscratch | grep $collsst | wc -l )) 
        local    numfoutmon=($( find * -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*_1mo_glo_*.nc4*" 2>/dev/null | grep -v $dscratch | grep -v $collsst | wc -l )) 
        local numfoutnonmon=($( find * -maxdepth 1 -mindepth 1 -type f -name "$fcstdate.*_glo_*nc4.tar" 2>/dev/null | grep -v $dscratch | grep -v $collsst | wc -l )) 
        local    numfoutmom=($( find MOM_Output/* -maxdepth 0 -mindepth 0 -type f -name "ocean_daily*" 2>/dev/null | grep -v $dscratch | grep -v $collsst | wc -l )) 
        cd - >/dev/null
        #+++++ cd dexp ( end ) +++++

        #todo: counts number of output on lfe
        local numfoutlfe=$( grep -Ev 'restarts|MOM_Output' $flst_lfe 2>/dev/null | grep -v nc4.tar. | wc -l )
        local numfoutlfemon=$( grep -Ev 'restarts|MOM_Output' $flst_lfe 2>/dev/null | grep _1mo_glo_ | grep -v nc4.tar. | wc -l )
        local numfoutlfenonmon=$( grep -Ev 'restarts|MOM_Output' $flst_lfe 2>/dev/null | grep -v _1mo_glo_ | grep -v nc4.tar. | wc -l )
        local numfoutlfemom=$( grep MOM_Output $flst_lfe | wc -l )

        if ! $blrsync;then

            #todo:  count shiftc input file in dexp/archive dir
            #+++++ cd dexp/archive (start) +++++
            cd $dexp/archive

            local numfsftin=$( find * -type f -name 'gcmarch_shiftin_*' 2>/dev/null | wc -l )
            local numfsftin_done=$( find * -type f -name 'gcmarch_shiftin_*_done' 2>/dev/null | wc -l )
            local sftcid=$( grep -i "shift id" stderr_gcmarch  2>/dev/null | tail -1 | rev | cut -d' ' -f1 | rev )

            cd - >/dev/null
            #+++++ cd dexp/archive ( end ) +++++
        fi

        [[ -f $flst_lfe ]] && rm -f $flst_lfe

        #todo:  results
        msg_wheader_userdefined $int_hwidth "=" "$( echo $dexp | rev | cut -d'/' -f1-2 | rev )" 
        wmessage "# months: $nummon_run"
        wmessage
        wmessage Calculated:
        wmessage " total # of outputs: $numfall_calc"
        wmessage "            Montyly: $numfmon_calc"
        wmessage "        Non-Montyly: $numfnonmon_calc"
        wmessage "        MOM outputs: $numfmom_calc"
        if $blleaveout;then
            wmessage 
            wmessage "Non-Monthly    Stay: $numfcollsavenonmon_calc"
            wmessage "Non-Monthly Deleted: $numfcolldeleted_calc"
            wmessage
            wmessage pfe files:
            wmessage "        # of output: $numfout"
            wmessage "            Monthly: $numfoutmon"
            wmessage "        Non-Monthly: $numfoutnonmon"
            wmessage "                MOM: $numfoutmom"
        fi
        wmessage
        wmessage lfe files:
        wmessage "        # of output: $numfoutlfe"
        wmessage "            Monthly: $numfoutlfemon"
        wmessage "        Non-Monthly: $numfoutlfenonmon"
        wmessage "                MOM: $numfoutlfemom"
        wmessage
        ! $blrsync && wmessage "   Shiftc Completed: $numfsftin_done of $numfsftin"
        wmessage

    done

    writetofile=0

    #todo:  print outputs
    local divis2=$( echo "${#arrdexp[@]} % 2" | bc  )
    local divis3=$( echo "${#arrdexp[@]} % 3" | bc  )

    if (( $divis3 == 0 ));then
        #todo:  print three files side-by-side
        local _thiswith=$( echo "$int_hwidth * 3 + 2 * 10 " | bc  )

        for _int in $( seq 0 3 $( echo "${#arrfout[@]} -1 "| bc ) );do
            local _thisint=$_int
            local _f1=${arrfout[$_thisint]}; _thisint=$(( _thisint + 1 ))
            local _f2=${arrfout[$_thisint]}; _thisint=$(( _thisint + 1 ))
            local _f3=${arrfout[$_thisint]} 
            
            pr -m -t -W $_thiswith $_f1 $_f2 $_f3

        done 
    else
        #todo:  print two files side-by-side
        if (( $divis2 == 0 ));then
            local _thiswith=$( echo "$int_hwidth * 2 + 10 " | bc )

            for _int in $( seq 0 2 $( echo "${#arrfout[@]} -1 "| bc ) );do
                local _thisint=$_int
                local _f1=${arrfout[$_thisint]}; _thisint=$(( _thisint + 1 ))
                local _f2=${arrfout[$_thisint]} 
                
                pr -m -t -W $_thiswith $_f1 $_f2 

            done
        else
            #wmessage "ALERT: You need to work on else statement starting @ $LINENO !!!" 
            local _thiswith=$( echo "$int_hwidth * 2 + 10 " | bc )

            for _int in $( seq 0 2 $( echo "${#arrfout[@]} -1 "| bc ) );do
                local _thisint=$_int
                local _f1=${arrfout[$_thisint]}; _thisint=$(( _thisint + 1 ))
                local _f2=${arrfout[$_thisint]} 
                
                pr -m -t -W $_thiswith $_f1 $_f2 

            done

       fi
    fi
    
    wmessage
    $opta && [[ ! -n $qid ]] && wmessage " Total # of Archive Running: ${#arrdexp[@]}"

    (( ${#arrfout[@]} > 0 )) && rm -f ${arrfout[@]}

    return
}

check_archstatus(){
    local _thisinput=$1
    local fmessage=$dmess/message_${strscr}_${FUNCNAME[0]}_${strexpid}
    local _msgsubject="${hstshort}.${strscr} ${FUNCNAME[0]}: $strexpid ( User Input : $_thisinput )"
    local _dstdout=$dstdout/${FUNCNAME[0]}/$strdout
    local _blsendmsgall=false

    exp_archstatus $_thisinput
    
    if (( $writetofile == 1 ));then 
        sendmsg ${FUNCNAME[0]} $_blsendmsgall $_dstdout $fmessage
        #local status_sendmsg=$?
        #(( $status_sendmsg > 0 )) && die "failed to send email"
    fi

    return 

}

outarc2exp() {

    #description: 
    local _thisinput=$1;shift
    local _thissrcme=$1
    local _hstshort=$( get_host ) 
    local _thisscr=run_${FUNCNAME}.sh 
    #local _strsearch1="Missing on PFE:"
    #local _strsearch2="Missing on LFE:"
    local _strsearch2="Missing on ARC:"
    local fmessage=$dmess/message_${strscr}_${FUNCNAME[0]}_${strexpid}

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

    msg_wheader
    
    for _dexp in ${_arrdexp[@]};do

        msg_wheader_userdefined 60 - $_dexp

        set_rstfcstdate $_dexp

        local _darc=$DARCH/$fcstdate/$ensm
        local _ferr=$_dexp/archive/stderr_gcmarch
        local _femp=$_dexp/archive/gcmarch_arcempf
   
        #todo:  check # of missing files on archive 
        local _fmiss_arc=$( grep "$_strsearch2" $_ferr 2>/dev/null | tail -1 | cut -d':' -f2 | wc -l ) 

        if (( $_fmiss_arc == 0 ));then 

            #todo:  check empty files on arc
            if [[ ! -f $_femp ]];then
                local _scrarc=$cdir/${strscr}_${FUNCNAME[0]}_arcempf.sh

                if ssh -q $hostarc true >>/dev/null 2>&1 ; then 
                    [[ -f $_scrarc ]] && rm -f $_scrarc
                    [[ -f $_femp   ]] && rm -f $_femp

cat > $_scrarc << EOF
cd $_darc
find * -maxdepth 1 -mindepth 1 -type f -empty -not -name "restarts*" 
cd - >/dev/null
EOF
                
                    ssh -q $hostarc 'bash -s' < $_scrarc >> $_femp
                    [[ -f $_scrarc ]] && rm -f $_scrarc
                    
                fi
            fi

            if [[ -s $_femp ]];then 
                local _arrfemp=$( cat $_femp ) 
                local _numf=${#_arrfemp[@]} 
    
                local _arrfemp_expmiss=($( printf "$DFCST/%s\n" ${_arrfemp[@]} | xargs -i bash -c "[[ ! -f {} ]] && echo {}" )) 
    
                if (( $_numf == ${#_arrfemp_expmiss[@]} ));then 
                    wmessage "Mising files in exp dir are all empty in archive"
                    continue

                elif (( $_numf != 0 && ${#_arrfemp_expmiss[@]} != 0 ));then 
                    wmessage "Empty Files on Archive: $_numf"
                    wmessage "** Some of empty files are available on _dexp"
                else
                    wmessage "in else" 
                fi
            fi
    
            if (( $writetofile == 1 ));then 
                exp_outarc2exp $_dexp >> $fmessage 2>&1
            else
                exp_outarc2exp $_dexp 
            fi
        fi
        wmessage
    done 

    return
}

exp_rmextra(){
    #description: remove outputs from experiments which was supposed to end
    #               after running 3months (incl.a partial), but kept running.
    local _thisinput=$1
    local _dstdout=$dstdout/${FUNCNAME[0]}/$strdout
    local _strstatus_c3="C3"
    local _farcdel=gcmarch_deloutcompleted
    local _dexp

    [[ -d $_thisinput ]] && local  bldir=true || local  bldir=false
    [[ -f $_thisinput ]] && local blfile=true || local blfile=false
    
    #todo:  check user input
      $bldir &&   $blfile && die "user input is unrecognizable form"
    ! $bldir && ! $blfile && die "user input is unrecognizable form"

    if $bldir && ! $blfile;then
        local _arrdexp=( $_thisinput )
    elif ! $bldir && $blfile;then
        local _arrdexp=($( cat $_thisinput )) 
    fi

    _arrdexp=($( printf '%s\n' ${_arrdexp[@]} | xargs -i bash -c "[[ -d {} ]] && echo {}" ))

    [[ ! -d $_dstdout ]] && mkdir -p $_dstdout

#wmessage \@$LINENO
#ahand_print ${_arrdexp[@]} 
#exit

    for _dexp in ${_arrdexp[@]};do 

        set_rstfcstdate $_dexp 


        local fmessage=$cdir/message/message_${strscr}_${FUNCNAME[0]}_$fcstdate$ensm
        local _blwinner=$( exp_checkwinner $_dexp $dout_pckwinners )
        local _yyyymm=$( echo $fcstdate | cut -c1-6 ) 
        local _fstatuslst=$dout_getstatus/data_getstatus_${strexpid}_list_$_yyyymm
        local _fwin=$( find $dout_pckwinners/* -type f -name "${strfwin}*${strexpid}_${_yyyymm}_[1-9]*.txt"  ) 
        local _fcapr=$_dexp/cap_restart
        local _fcap1=$_dexp/CAP_1.rc

        #note:  make sure that exp that will be removing outputs from is NOT a winner. 
        [[   -z $_fwin       ]] && wmessage "(${FUNCNAME[0]}) var undefined: _fwin"              && continue 
        [[ ! -f $_fstatuslst ]] && wmessage "(${FUNCNAME[0]}) file does not exist: $_fstatuslst" && continue
        [[ ! -f $_fwin       ]] && wmessage "(${FUNCNAME[0]}) file does not exist: $_fwin"       && continue

        local _arrdexp_win=($( cat $_fwin | sed 's# #/#g' | xargs -i printf "$DFCST/%s\n" {} ))

        #todo:  find ens member that archive is completed and is not a winner
        local  _arrdexp_arcdone=($( find $DFCST/$fcstdate/* -maxdepth 2 -mindepth 2 -type f -name $_farcdel | rev | cut -d'/' -f3- | rev | sort -V )) 
        #local  _arrdexp_arcdone=($( grep $_strstatus_c3 $_fstatuslst | cut -d' ' -f2 | grep $fcstdate )) 
        local _arrdexp_winfdate=($( printf '%s\n' ${_arrdexp_win[@]} | grep $fcstdate )) 
        local _strdexp_winfdate=$( echo ${_arrdexp_winfdate[@]} | sed 's# #|#g' )
        local    _arrdexp_avail=($( printf '%s\n' ${_arrdexp_arcdone[@]} | grep -vE "$_strdexp_winfdate|$_dexp" | sort -V | uniq -u  )) 

        local _dexp_sample=${_arrdexp_avail[0]}
#_dexp_sample=$DFCST/19850317/ens3
wmessage \@$LINENO $_dexp_sample
exit


        [[ -z $_dexp_sample ]] &&  wmessage "(${FUNCNAME[0]}) var undefined: _dexp_sample"       && continue

        if [[ "$strexpid" == "fcst" ]];then 
            local _yyyymmddens_sample=${_dexp_sample#*GEOS_fcst/}
        else
            local _yyyymmddens_sample=${_dexp_sample#*${strexpid}/}
        fi
wmessage \@$LINENO $_yyyymmddens_sample


        local _dextra_nbp=$DFCST/.${strscr}_${FUNCNAME[0]}/$fcstdate/$ensm
        local _dextra_arc=$DARCH/.${strscr}_${FUNCNAME[0]}/$fcstdate/$ensm
        local _flstrmout_nbp=$cdir/tmp_${strscr}_${FUNCNAME[0]}_${strexpid}_${fcstdate}${ensm}_mvlst_nbp
        local _flstrmout_arc=$cdir/tmp_${strscr}_${FUNCNAME[0]}_${strexpid}_${fcstdate}${ensm}_mvlst_arc


        [[ -f $_flstrmout_nbp ]] && rm -f $_flstrmout_nbp
        [[ -f $_flstrmout_arc ]] && rm -f $_flstrmout_arc

        #todo:  create $_dextra_
        [[ ! -d $_dextra_nbp ]] && mkdir -p $_dextra_nbp

        msg_wheader

        #todo:  mv output on nbp
        exp_rmextra_nbp $_dexp $DFCST/$_yyyymmddens_sample $_dextra_nbp $_flstrmout_nbp

        #todo:  recreate cap_restart with 3mo enddate
        [[ -f $_fcapr ]] && mv $_fcapr ${_fcapr}_org

        if [[ ! -f $_fcapr && -f $_fcap1 ]];then 
            grep -i end_date $_fcap1 | tr -s '[:space:]'| cut -d' ' -f2- >| $_fcapr
        fi

exit        

        #todo:  mv output on arc host
        if ssh -q $hostarc true;then 
            ssh -q $hostarc test -d $_dextra_arc 2>/dev/null
            local _status_dexist=$?

            if (( $_status_dexist > 0 )) ;then 
                ssh -q $hostarc mkdir -p $_dextra_arc 2>/dev/null
                ssh -q $hostarc test  -d $_dextra_arc 2>/dev/null
                local _status_dexist=$?
            fi
            
            if (( $_status_dexist >0 ));then
                return 3
            fi

            local _blskiparc=false
        else
            local _blskiparc=true
        fi

        ! $_blskiparc && exp_rmextra_arc $_dexp $DARCH/$fcstdate/$ensm $_yyyymmddens_sample $_dextra_arc $_flstrmout_arc

        #todo:  move fmessage    
        [[ -f $fmessage ]] && local sizef=$( stat --print='%s' $fmessage ) || local sizef=0
        (( $sizef > 0 )) && mv $fmessage $_dstdout/

        [[ -f $_flstrmout_nbp ]] && rm -f $_flstrmout_nbp
        [[ -f $_flstrmout_arc ]] && rm -f $_flstrmout_arc
        
    done 

    return 0
}

exp_rmextra_arc(){
    #description: remove extra outputs from darc
  
    local _dexp=$1;shift
    local _darc=$1;shift
    local _yyyymmddens_sample=$1;shift
    local _dextra_arc=$1;shift
    local _flst=$1
    local _dsample_nbp=$DFCST/$_yyyymmddens_sample
    local _dsample_arc=$DARCH/$_yyyymmddens_sample
    local _fcstdate_sample=$( echo $_yyyymmddens_sample | cut -d'/' -f1 ) 
    local     _ensm_sample=$( echo $_yyyymmddens_sample | cut -d'/' -f2 ) 
    local _fhistory=HISTORY_2.rc
    local _coll

    set_rstfcstdate $_dexp

    local    _fexistlfe_dexp=$cdir/tmp_${strscr}_${FUNCNAME[0]}_${strexpid}_${fcstdate}${ensm}_arc
    local _fexistlfe_dsample=$cdir/tmp_${strscr}_${FUNCNAME[0]}_${strexpid}_${_fcstdate_sample}${_ensm_sample}_arc

    local _scr=$cdir/${strscr}_${FUNCNAME[0]}

    [[ -f $_scr ]] && rm -f $_scr

    exp_getfname_lfe $_dexp        $_darc        $_fexistlfe_dexp
    exp_getfname_lfe $_dsample_nbp $_dsample_arc $_fexistlfe_dsample

    local _arrcoll=($( exp_getcollections $_dexp/$_fhistory  ))

    #todo:  get a list of outputs that should exist
    #NOTE: RESTARTS & MOM OUTPUTS
    [[ -s $_fexistlfe_dsample ]] && local _arrfout_keep=($( cat $_fexistlfe_dsample | cut -d' ' -f3 | rev | cut -d'/' -f1-2 | rev )) || local _arrfout_keep=() 
    [[ -s $_fexistlfe_dexp    ]] && local  _arrfout_all=($( cat $_fexistlfe_dexp    | cut -d' ' -f3 | rev | cut -d'/' -f1-2 | rev )) || local _arrfout_all=()

    #todo:  list of outputd should be removed 
    local   _arrfout_mv=($( printf '%s\n' ${_arrfout_all[@]} ${_arrfout_keep[@]} | sort -V | uniq -u ))
    local  _arrdcoll_mk=($( printf '%s\n' ${_arrfout_mv[@]} | cut -d'/' -f1 | sort -V | uniq )) 
    local _arryyyymm_mv=($( printf '%s\n' ${_arrfout_mv[@]} | grep -vE 'restarts|ocean_' | sed 's#.tar##g' | rev | cut -d'.' -f2 | rev | cut -c1-6 | sort -V | uniq ))
    local _numfdel=$( echo "${#_arrfout_all[@]} - ${#_arrfout_keep[@]}" | bc )

    msg_wheader_userdefined 40 "-" "Remove Extra Outputs from darc" 
    wmessage "              Fcst date: $fcstdate/$ensm"
    wmessage "       File Destination: $_dextra_arc"

    if (( ${#_arrfout_mv[@]} > 0 ));then 
        _runhere=true
        if $_runhere;then 
        wmessage "   # of Files to Remove: $_numfdel"
        wmessage "Output YYYYMM to Remove:"
        ahand_print ${_arryyyymm_mv[@]} 

        wmessage "Move Files:"
        ahand_print ${_arrfout_mv[@]} 

        printf "mkdir -p $_dextra_arc/%s\n" ${_arrdcoll_mk[@]} >| $_scr
        ssh -q $hostarc 'bash -s' < $_scr

        paste <( printf "mv $DARCH/$fcstdate/$ensm/%s\n" ${_arrfout_mv[@]} ) <( printf "$_dextra_arc/%s\n" ${_arrfout_mv[@]} ) >| $_scr 
        ssh -q $hostarc 'bash -s' < $_scr

        local _numfmoved=$( ssh -q $hostarc "find $_dextra_arc/* -type f 2>/dev/null | wc -l " )

        wmessage "# of Files Moved:$_numfmoved"
        fi
    else
        local _numfmoved=$( ssh -q $hostarc "find $_dextra_arc/* -type f 2>/dev/null | wc -l " )
        wmessage "# of Files are Already Moved:$_numfmoved"
    fi

    [[ -f $_scr ]] && rm -f $_scr

    return
}

exp_rmextra_nbp(){
    #description: remove extra outputs from dexp
  
    local _dexp=$1;shift
    local _dsample=$1;shift
    local _dextra_nbp=$1;shift
    local _flst=$1
    local _fhistory=HISTORY_2.rc
    local _coll

    set_rstfcstdate $_dexp

    local _arrcoll=($( exp_getcollections $_dexp/$_fhistory  ))
    
    #todo:  get a list of outputs that should exist
    cd $_dsample
    local _arrfout_keep=($( printf '%s\n' ${_arrcoll[@]} | xargs -i bash -c "find {}/* -type f -name "'"'"${fcstdate}.{}.*"'"'" " ))
    cd - >/dev/null 
    
    cd $_dexp
    local  _arrfout_all=($( printf '%s\n' ${_arrcoll[@]} | xargs -i bash -c "find {}/* -type f -name "'"'"${fcstdate}.{}.*"'"'" " ))
    cd - >/dev/null 
  
    #todo:  list of outputd should be removed 
    local  _arrfout_mv=($( printf '%s\n' ${_arrfout_all[@]} ${_arrfout_keep[@]} | sort -V | uniq -u ))
    local _arryyyymm_mv=($( printf '%s\n' ${_arrfout_mv[@]} | sed 's#.tar##g' | rev | cut -d'.' -f2 | rev | cut -c1-6 | sort -V | uniq ))
    local _numfdel=$( echo "${#_arrfout_all[@]} - ${#_arrfout_keep[@]}" | bc )

    msg_wheader_userdefined 40 "-" "Remove Extra Outputs from dexp" 
    wmessage "              Fcst date: $fcstdate"
    wmessage "       File Destination: $_dextra_nbp"
  
    if (( ${#_arrfout_mv[@]} > 0 ));then 
        printf '%s\n' ${_arrfout_mv[@]} >> $_flst

        wmessage "   # of Files to Remove: $_numfdel"
        wmessage "Output YYYYMM to Remove:"
        ahand_print ${_arryyyymm_mv[@]} 
        wmessage "Move Files:"
        wmessage "$( printf '    %s\n'  $( cat $_flst ) )" 
        
        printf '%s\n' ${_arrcoll[@]} | xargs -i bash -c "[[ ! -d $_dextra_nbp/{} ]] && mkdir -p $_dextra_nbp/{}"

        #+++++ cd _dexp (start) +++++ 
        cd $_dexp
        for _coll in ${_arrcoll[@]};do
            local _arrfmv=($( grep $_coll $_flst 2>/dev/null ))
            if (( ${#_arrfmv[@]} > 0 ));then 
                #wmessage "... move to $_dextra_nbp/$_coll"        
                mv ${_arrfmv[@]} $_dextra_nbp/$_coll/
            fi
        done 
        cd - >/dev/null
        #+++++ cd _dexp ( end ) +++++ 
        
        local _numfmoved=$( find $_dextra_nbp/* -type f | wc -l  )
        wmessage "# of Files Moved:$_numfmoved"

    else
        local _numfmoved=$( find $_dextra_nbp/* -type f | wc -l  )
        wmessage "# of Files are Already Moved:$_numfmoved"
    fi
    return
}


clean_datasubmit(){
    #description:    remove duplicates and empty file in data_submit_* dir
    local _cdatetime=$( date +%Y%m%d_%H%M )
    local _ftmp=${strscr}_${strexpid}_${FUNCNAME[0]}
    local _fsub=data_submit_${strexpid}_${strscr}_${_cdatetime}
    
    [[ -f $_ftmp ]] && rm -f ${arrfsub[@]}

    #todo:  exit if run_submit or ru_monitor are running
    [[ -f submit.lock || -f monitor.lock ]] && exit

    #+++++  cd dtmpdata (start) +++++
    cd $dtmpdata
    #todo:  delete empty files
    find * -maxdepth 0 -type f -empty -delete 2>/dev/null

    #tod:   remove duplicate experiments
    local arrfsub=($( find * -type f -name "data_submit_${strexpid}_*" 2>/dev/null ))

    if (( ${#arrfsub[@]} > 0 ));then
        cat ${arrfsub[@]} | sort -V | uniq -u >> $_ftmp
        [[ -f $_ftmp ]] && rm -f ${arrfsub[@]}
    fi
    
    return
}

expsetup_check(){

    #local arricyyyymm=($( getyyyymm ))

    local arrdir=( RC RC_s2sv3 RESTART archive )
    local arrfconfig=( AGCM_a2017.rc AGCM_b2017.rc CAP_1.rc CAP_2.rc HISTORY_1.rc HISTORY_2.rc \
                       diag_table field_table gcm_run.j GEOSgcm.x archive/run_gcmarch.sh )
    local arrsml=( AGCM.rc CAP.rc HISTORY.rc )
    local fsready=gcmsetup_setupready
    local numfatm=22
    local numfocn=11
    local icyyyymm


    get_beg_and_end $startyyyymm $endyyyymm
    local arrdexp1=($( get_dexp $DFCST $begyyyymm $endyyyymm ${arrintens[@]} ))
    local arrdexp=($( printf '%s\n' ${arrdexp1[@]} | xargs -i bash -c "[[ -d {} ]] && echo {}" | sort -V ))

    for dexp in ${arrdexp[@]};do
        local blheader=false

        #todo:  find missing files and dir
        local arrdir_notexist=($( printf '%s\n' ${arrdir[@]} | xargs -i bash -c "[[ ! -d $dexp/{} ]] && echo {}" ))
        local arrfconfig_notexist=($( printf '%s\n' ${arrfconfig[@]} | xargs -i bash -c "[[ ! -f $dexp/{} ]] && echo {}" ))
        local arrsml_notexist=($( printf '%s\n' ${arrsml[@]} | xargs -i bash -c "[[ ! -h $dexp/{} ]] && echo {}" ))
        [[ ! -f $dexp/$fsready ]] && local arrf_notexist=( $fsready ) 
        arrf_notexist+=( ${arrdir_notexist[@]} ${arrfconfig_notexist[@]} ${arrsml_notexist[@]} )

        #todo:  count rst files. 
        local numfatm_exp=$( find $dexp/* -maxdepth 0 -type f -name '*_rst' | wc -l ) 
        local numfocn_exp=$( find $dexp/RESTART/*  -maxdepth 0 -type f -name '*.nc' | wc -l ) 

        (( $numfatm == $numfatm_exp)) && local blatm=true || local blatm=false
        (( $numfocn == $numfocn_exp)) && local blocn=true || local blocn=false

        if (( ${#arrf_notexist[@]} > 0 ));then
            ! $blheader && msg_wheader_userdefined 40 - $dexp && blheader=true
            wmessage "Missing files and directories:"
            ahand_warr ${arrf_notexist[@]}
        fi

        #note:  once exp runs, there will  be more *_rst files. As an indicator
        #       check if holding dir exists
        if [[ ! -d $dexp/holding ]];then
            if ! $blatm;then 
                ! $blheader && msg_wheader_userdefined 40 -  $dexp && blheader=true
                local nummiss=$(( numfatm - numfatm_exp ))
                wmessage "$nummiss atm restarts are missing"
            fi

            if ! $blocn;then 
                ! $blheader && msg_wheader_userdefined 40 - $dexp && blheader=true
                local nummiss=$(( numfocn - numfocn_exp ))
                wmessage "$nummiss ocn restarts are missing"
            fi
        #else
            #! $blheader && msg_wheader_userdefined 40 - $dexp && blheader=true
            #wmessage "exp was already executed"
        fi

    done

    return
}

write_countarchshiftc(){
    #description:    write running shiftc for run_gcmarch.sh in a temp file
    local _ftmp=tmp_${strscr}_${FUNCNAME[0]}_1
    local arrsftid_all=($( /usr/local/bin/shiftc --status=csv | grep run  | tail -n +2 | cut -d',' -f1 ))
    local _pdir=$( echo $DFCST | rev | cut -d'/' -f1 | rev )  

    [[ -f $_ftmp ]] && msg_newfile $_ftmp

    #if [[ "$strexpid" == "fcst" ]];then
    #    local arrsftid_arch=($( printf '%s\n' ${arrsftid_all[@]} | xargs -i bash -c "/usr/local/bin/shiftc --id={} --status=csv | grep --color 'run\|lou\|GEOS_fcst' | head -1 | cut -d',' -f3 | cut -d':' -f2" | sort -V ))
    #else
    #    local arrsftid_arch=($( printf '%s\n' ${arrsftid_all[@]} | xargs -i bash -c "/usr/local/bin/shiftc --id={} --status=csv | grep --color "'"'"run\|lou\|$strexpid"'"'" | head -1 | cut -d',' -f3 | cut -d':' -f2" | sort -V ))
    #fi

    if (( ${#arrsftid_all[@]} > 0 ));then
        for sftid in ${arrsftid_all[@]};do
           local  sftid_arch=$( /usr/local/bin/shiftc --id=$sftid --status=csv | grep "run\|lou\|$_pdir" 2>/dev/null | head -1 | cut -d',' -f3 | cut -d':' -f2 )

            [[ -z $sftid_arch ]] && continue 

            local yyyymmddens=$( echo $sftid_arch | rev | cut -d'/' -f3-4 | rev  )
            local sftid_fpath=$( echo $yyyymmddens | xargs -i echo $DFCST/{}/archive )

            echo $sftid $sftid_fpath >> $_ftmp
        done 
    fi   

    echo $_ftmp
}

count_archshiftc(){
    #description:    count running shiftc for run_gcmarch.sh  
    local fmessage=$dmess/message_${strscr}_${FUNCNAME[0]}_${strexpid}
    local _ftmp=$( write_countarchshiftc )
    local _dstdout=$dstdout/${FUNCNAME[0]}/$strdout
    local _blsendmsgall=false

    if [[ -f $_ftmp ]];then 
        local arrdexparch=($( cat $_ftmp | cut -d' ' -f2 | rev | cut -d'/' -f2- | rev | sort -V | uniq  ))
        msg_wheader
        wmessage "-------- --------------------------------------------------------"
        wmessage " Counts                 Experiment with Shiftc IDs               "
        wmessage "-------- --------------------------------------------------------"

        local _cnt_sftc=0
        for dexp in ${arrdexparch[@]};do
            local arrsftc=($( grep "$dexp/" $_ftmp | cut -d' ' -f1 ))
            if (( $writetofile == 1 ));then
                printf '    %s     %s\n' ${#arrsftc[@]} $dexp >> $fmessage
                printf '              %s\n' ${arrsftc[@]} | sort -V >> $fmessage
            else    
                printf '    %s     %s\n' ${#arrsftc[@]} $dexp 
                printf '              %s\n' ${arrsftc[@]} | sort -V 
            fi
            _cnt_sftc=$( echo "$_cnt_sftc + ${#arrsft[@]}" | bc )
        done 

        wmessage
        wmessage "     Total Exp: ${#arrdexparch[@]}"
        wmessage "Running Shiftc: $_cnt_sftc"
        wmessage
        rm -f $_ftmp 2>/dev/null 
    elif (( $writetofile == 1 ));then
        wmessage "there are no running shiftc for archiving"
    fi

    if (( $writetofile == 1 ));then 
        sendmsg ${FUNCNAME[0]} $_blsendmsgall $_dstdout $fmessage
        #local status_sendmsg=$?
        #(( $status_sendmsg > 0 )) && die "failed to send email"
    fi
    return
}

sendmsg(){
    #description:   send email 
    #Note:  04/12/2023 Work on _blsendall. It is NOT used for now. 
    local _funcname=$1;shift
    local _blsendall=$1;shift
    local _thisdstdout=$1;shift
    local fmessage=$1;shift
    local msg_subject="$1"
    local feadd=$cdir/mailadd
    local ftmp1=$cdir/tmp_${FUNCNAME[0]}_${_funcname}_1
    local ftmp2=$cdir/tmp_${FUNCNAME[0]}_${_funcname}_2
    local _thisstatus=999
    local _blrm=false

    [[ -z $msg_subject ]] && local msg_subject="${hstshort}.${strscr} ${_funcname}: $strexpid"
    [[ -f $feadd       ]] && local eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" ) || return 1

    [[ ! -d $_thisdstdout ]] && mkdir -p $_thisdstdout
    [[   -f $ftmp1 ]] && rm -f $ftmp1
    [[   -f $ftmp2 ]] && rm -f $ftmp2

    #todo:  send email
    [[ -f $fmessage ]] && local sizef=$( stat --print='%s' $fmessage ) || return 2
  
    local _fmsg_final=$_thisdstdout/$( basename $fmessage )

    #todo   diff previous archstatus stdout and fmessage.
    #       If there are difference, email fmessage
    if (( $sizef > 0 ));then
        if [[ -f $_fmsg_final ]];then
            cat $fmessage    | tail -n +4 >> $ftmp1
            cat $_fmsg_final | tail -n +4 >> $ftmp2

            diff $ftmp1 $ftmp2 >>/dev/null 2>&1
            local _status_diff=$?
        else
            local _status_diff=999
        fi

        #todo:  send email
        if (( $_status_diff > 0 ));then 
            [[ -f $ferr     ]] && local sizeferr=$( stat --print='%s' $ferr )  || local sizeferr=0
            [[ -f $fmessage ]] && local sizef=$( stat --print='%s' $fmessage ) || local sizef=0

            if (( $sizeferr > 0 || $sizef > 0 ));then 
                if (( $sizeferr > 0 ));then
                    msg_wheader_userdefined 40 "-" $( basename $ferr ) 
                    wmessage "$( cat $ferr )"
                    _blrm=true
                fi

                msg_cyberpostman "$msg_subject" "$eadds" $fmessage $blmsgmutt 
                local _status_email=$?
                mv $fmessage $_fmsg_final

                if (( $_status_email == 0 ));then 
                    rm -f $fmessage
                    $_blrm && msg_newfile $ferr
                    local _thisstatus=0
                else
                    local _thisstatus=3
                fi
            fi
        else
            rm -f $fmessage
            local _thisstatus=3
        fi
    fi
 
    [[ -f $ftmp1 ]] && rm -f $ftmp1
    [[ -f $ftmp2 ]] && rm -f $ftmp2

    return $_thisstatus
}

sftc_days(){
    #description:    find shiftc for run_gcmarch.sh running for days.
    local cnt=0
    local _ftmp=$( write_countarchshiftc )

    local _ftmpout=tmp_${strscr}_${FUNCNAME[0]}_1
    local fmessage=$dmess/message_${strscr}_${FUNCNAME[0]}_${strexpid}
    local _dstdout=$dstdout/${FUNCNAME[0]}/$strdout
    local _blsendmsgall=false
    #local _limhrs=10
    local _limhrs=5
    #local _limhrs=4

    [[ -f $_ftmpout ]] && rm -f $_ftmpout 2>/dev/null 
    
    if [[ -f $_ftmp ]];then 
        local arrsftcid=($( cat $_ftmp | cut -d' ' -f1 ))

        for sftcid in ${arrsftcid[@]};do
            #local _timelength1=$( shiftc --id=$sftcid --status=csv | grep run | head -1 | rev | cut -d',' -f1-9 | rev | cut -d',' -f1 )
            local _timelength1=$( /usr/local/bin/shiftc --status=csv | grep $sftcid | grep run | head -1 | rev | cut -d',' -f1-10 | rev | cut -d',' -f1 )

            [[ *"$_timelength1"* =~ d ]] && local bldays=true || local bldays=false
            [[ *"$_timelength1"* =~ h ]] && local  blhrs=true || local blhrs=false

            if $bldays; then 
                local _timelength2=$( echo $_timelength1 | cut -d'd' -f1 )
    
            elif $bldays && $blhrs;then 
                local _timelength2=$( echo $_timelength1 | cut -d'd' -f2 | cut -d'h' -f1 )

            elif ! $bldays && $blhrs;then 
                local _timelength2=$( echo $_timelength1 | cut -d'h' -f1 )
            else
                continue
            fi

            [[ -n $_timelength2 ]] && local blisint=$( misc_isinteger $_timelength2 ) || continue

            #todo:  check if timelengh1 is integer or not.
            (( $blisint > 0 )) &&  continue

            #todo:  check timelength 
            ! $bldays && $blhrs && (( $_timelength2 < $_limhrs )) && continue

            local _dexp=$( grep $sftcid $_ftmp 2>/dev/null | cut -d' ' -f2 )

            #printf ' %s %s %s\n' $_timelength1 $sftcid $_dexp >> $_ftmpout
            echo $_timelength1 $sftcid $_dexp >> $_ftmpout
            cnt=$(( cnt + 1 )) 
        done
    fi

    if [[ -f $_ftmpout ]];then
        msg_wheader
        if (( $writetofile == 1 ));then
            cat $_ftmpout | sort -V -k3 -t' ' | column -t -s' ' >> $fmessage
        else
            cat $_ftmpout | sort -V -k3 -t' ' | column -t -s' '
        fi
        wmessage
        wmessage "TOTAL = $cnt"
        wmessage 

        if $blsftc; then
            cat $_ftmpout | cut -d' ' -f2 | xargs -i /usr/local/bin/shiftc --id={} --stop 2>/dev/null  
            wmessage "** shiftc above are all stopped"
        fi
        local blsendmsg=true
    else
        (( $writetofile == 0 )) && wmessage "there are no gcmarch shiftc that running for more than $_limhrs hrs."
        local blsendmsg=false
    fi

    [[ -f $_ftmp ]] && rm -f $_ftmp 2>/dev/null 
    [[ -f $_ftmpout ]] && rm -f $_ftmpout 2>/dev/null 
  
    #todo:  send email 
    if $blsendmsg && (( $writetofile == 1 ));then 
        sendmsg ${FUNCNAME[0]} $_blsendmsgall $_dstdout $fmessage
        #local status_sendmsg=$?
        #(( $status_sendmsg > 0 )) && die "failed to send email"
    fi

    return
}

calculate_outputsize(){
    #description: calculate total size of YYYYMM dir 
    local _fhis2=HISTORY_2.rc
    local hst=$hostarc
    local strrst=restarts
    local strmom=ocean_daily
    local arrdexp=($( find $DFCST/* -maxdepth 1 -mindepth 1 -type d -name 'ens*' | grep $usericyyyy | sort -V ))
    local thishis2=$( printf "%s/$_fhis2\n" ${arrdexp[@]} | xargs -i bash -c '[[ -f {} ]] && echo {}' | sort -V | head -1 )
    local arrcoll=($( exp_getcollections $thishis2 ))
    local arrcoll_mon=($( printf "%s\n" ${arrcoll[@]} | grep _1mo_glo | sort -V ))
    local arrcoll_nonmon=($( printf "%s\n" ${arrcoll[@]} | grep -v _1mo_glo | sort -V ))
    local arrcoll_nonmonsave=($( printf "%s\n" ${arrcoll_nonmon[@]} | grep -E '_sfc|_slv' | sort -V ))
    local arrcoll_nonmonrm=($( printf "%s\n" ${arrcoll_nonmon[@]} | grep -vE '_sfc|_slv' | sort -V ))
    local arrcoll_allsave=( ${arrcoll_mon[@]} ${arrcoll_nonmonsave[@]} )
    local arrcoll_lfeonly=( ${arrcoll_nonmonrm[@]} $strrst $strmom )
    local  ftmp=$dmess/${strscr}_${FUNCNAME[0]}_$usericyyyy
    local ftmp2=$dmess/${strscr}_${FUNCNAME[0]}_${usericyyyy}_numf
    local ftmp3=$dmess/${strscr}_${FUNCNAME[0]}_${usericyyyy}_LFE
    local ftmp4=$dmess/${strscr}_${FUNCNAME[0]}_${usericyyyy}_LFE_numf
    local ftmp5=$dmess/${strscr}_${FUNCNAME[0]}_${usericyyyy}_LFEonlyColl
    local ftmp6=$dmess/${strscr}_${FUNCNAME[0]}_${usericyyyy}_LFE_rstartsonly
    local dexp coll

    #note: ls --help says:
    #   SIZE is an integer and optional unit (example: 10M is 10*1024*1024).  Units
    #   are K, M, G, T, P, E, Z, Y (powers of 1024) or KB, MB, ... (powers of 1000).
    #ls -l --si message_monitor
    #       gives si unit ( 1k = 1000bytes)


    if [[ ! -f $ftmp ]];then
        #todo:  write output size for outputs which will stay on PFE
        for dexp in ${arrdexp[@]};do
            set_rstfcstdate $dexp
            for coll in ${arrcoll_allsave[@]};do
                find $dexp/$coll/* -type f -name "${fcstdate}*.nc*" | grep -v bbftp.tmp | xargs -i stat --printf="%Y %s %n\n" {} >> $ftmp  2>/dev/null
            done
        done 
    fi

    
    if [[ ! -f $ftmp2 ]];then
        #todo:  count a number of output files on PFE for each collection and each exp
        for coll in ${arrcoll_allsave[@]};do
            for dexp in ${arrdexp[@]};do
                set_rstfcstdate $dexp
                local numf=$( cat $ftmp | grep $dexp/$coll 2>/dev/null | wc -l ) 
                wmessage $fcstdate/$ensm $coll $numf >> $ftmp2  2>&1 
            done
        done 
    fi

    if [[ ! -f $ftmp3 ]];then
        #todo:  write output size for outputs which will stay on LFE
        local intdepth=1

        for dexp in ${arrdexp[@]};do
            set_rstfcstdate $dexp
            local dlou=$DARCH/$fcstdate/$ensm

            #todo:  get a file with name of existing monthly, daily, & diurnal files from lou.
            ssh -q $hst "find $dlou/* -maxdepth $intdepth -mindepth $intdepth -type f -name '${fcstdate}*.nc*' -o -name '*${strrst}*.tar' -o -name '*${strmom}*.nc' | grep -v bbftp.tmp | xargs -i stat --printf="'"'"%Y %s %n\n"'"'" {} >> $ftmp3" 2>/dev/null
        done 
    fi

    if [[ ! -f $ftmp4 ]];then
        #todo:  count a number of output files on PFE for each collection and each exp
        for coll in ${arrcoll_allsave[@]};do
            for dexp in ${arrdexp[@]};do
                set_rstfcstdate $dexp
                local numf=$( cat $ftmp3 | grep $dexp/$coll 2>/dev/null | wc -l ) 
                wmessage $fcstdate/$ensm $coll $numf >> $ftmp4  2>&1 
            done
        done 
    fi

    if [[ ! -f $ftmp5 ]];then
        #todo:  extract files that exists only on LFE
        for coll in ${arrcoll_lfeonly[@]};do
            cat $ftmp3 | grep $coll 2>/dev/null >> $ftmp5
        done
    fi

    if [[ ! -f $ftmp6 ]];then
        #todo:  extract restarts
        cat $ftmp3 | grep $strrst 2>/dev/null >> $ftmp6
    fi

    #todo:  calculate total size for PFE
    local arrsize=($( cat $ftmp | cut -d' ' -f2 ))
    local numadd=$( echo ${arrsize[@]} | sed 's# # + #g' )
    local numtot_bytes=$( echo "$numadd" | bc -l )
    local numtot=$( echo "scale=2;( $numtot_bytes ) / 1024^4" | bc -l )

    wmessage "Number of Collections on PFE : ${#arrcoll_allsave[@]} (out of ${#arrcoll[@]})"
    wmessage "             Number of Files : $( LC_ALL=en_US.UTF-8 printf "%'d" $( cat $ftmp | wc -l ) )"
    wmessage "         Total Size in bytes : $( LC_ALL=en_US.UTF-8 printf "%'d" $numtot_bytes )"
    wmessage "             Total Size in T : ${numtot}T"
    wmessage


    #todo:  calculate total size for LFE
    local arrsize=($( cat $ftmp3 | cut -d' ' -f2 ))
    local numadd=$( echo ${arrsize[@]} | sed 's# # + #g' )
    local numtot_bytes=$( echo "$numadd" | bc -l )
    local numtot=$( echo "scale=2;( $numtot_bytes ) / 1024^4" | bc -l )

    wmessage "      Number of Files on LFE : $( LC_ALL=en_US.UTF-8 printf "%'d" $( cat $ftmp3 | wc -l ) )"
    wmessage "         Total Size in bytes : $( LC_ALL=en_US.UTF-8 printf "%'d" $numtot_bytes )"
    wmessage "             Total Size in T : ${numtot}T"
    wmessage

    #todo:  calculate total size for files exist only on LFE
    local arrsize=($( cat $ftmp5 | cut -d' ' -f2 ))
    local numadd=$( echo ${arrsize[@]} | sed 's# # + #g' )
    local numtot_bytes=$( echo "$numadd" | bc -l )
    local numtot=$( echo "scale=2;( $numtot_bytes ) / 1024^4" | bc -l )

    wmessage " Number of Files Only on LFE : $( LC_ALL=en_US.UTF-8 printf "%'d" $( cat $ftmp5 | wc -l ) )"
    wmessage "         Total Size in bytes : $( LC_ALL=en_US.UTF-8 printf "%'d" $numtot_bytes )"
    wmessage "             Total Size in T : ${numtot}T"
    wmessage
    local arrsize=($( cat $ftmp6 | cut -d' ' -f2 ))
    local numadd=$( echo ${arrsize[@]} | sed 's# # + #g' )
    local numtot_bytes=$( echo "$numadd" | bc -l )
    local numtot=$( echo "scale=2;( $numtot_bytes ) / 1024^4" | bc -l )

    wmessage "   Number of Restarts on LFE : $( LC_ALL=en_US.UTF-8 printf "%'d" $( cat $ftmp6 | wc -l ) )"
    wmessage "         Total Size in bytes : $( LC_ALL=en_US.UTF-8 printf "%'d" $numtot_bytes )"
    wmessage "             Total Size in T : ${numtot}T"


    return
}

calculate_numout(){
    #description:   claculate total number of YYYY outputs
    local _dapp=$DBUILD/Applications/GEOSgcm_App
    local _fhis2=$_dapp/HISTORY.AOGCM-S2Sv3_2.rc.tmpl
    #local ddata=$cdir/output/pckwinners/$strexpid
    local _ddata=$dout_pckwinners
    local fmessage=$dmess/message_${strscr}_${FUNCNAME[0]}_${usericyyyy}_NumFiles
    local arricyyyymm=() arrdexp=()
    local dexp grandtotal=0

    declare -A assoc_winensm
    convert2winnerensm_all $_ddata

    [[   -z $DBUILD ]] && die "DBUILD is undefined"
    [[ ! -f $_fhis2 ]] && die "$_fhis2 does not exist"

    local arrintens5=($( printf '%s\n' ${arrintens[@]} | head -5 ))
    local arrintens10=($( printf '%s\n' ${arrintens[@]} | tail -10 ))

    #todo:  get fcst dates
    local endyyyymm=${usericyyyy}12
    local yyyymm=${usericyyyy}01
    while (( $yyyymm <= $endyyyymm ));do
        arricyyyymm+=( $yyyymm )
        yyyy=$( echo $yyyymm | cut -c1-4 )
        mm=$( echo $yyyymm | cut -c5- )
        yyyymm=$( fcal_nextmonth $mm $yyyy )
    done
    
    #todo:  get dexp path
    for icyyyymm in ${arricyyyymm[@]};do
        get_beg_and_end $icyyyymm $icyyyymm

        local yyyymm=$begyyyymm
        local arrfcstdate=()
        local arrdexp1=()
        while (( $yyyymm <= $endyyyymm ));do
            local yyyy=$( echo $yyyymm | cut -c1-4 )
            local int_mon=$( echo "$( echo $yyyymm | cut -c5-6 )*1" | bc )
            arrfcstdate+=($( fcal_icall $int_mon $yyyy )) 
            yyyymm=$( fcal_nextmonth $int_mon $yyyy )
        done
        
        arrdexp1+=($( printf "$DFCST/%s\n" ${arrfcstdate[@]} | xargs -i printf "{}/ens%s\n" ${arrintens5[@]} ))
        arrdexp1+=($( echo $DFCST/${arrfcstdate[-1]} | xargs -i printf "{}/ens%s\n" ${arrintens10[@]} ))
        arrdexp+=( ${arrdexp1[@]} )
    done
   
    #todo:  write header
    wmessage "$( printf '%-15s %+5s %+15s %+5s %+5s %+5s %+5s %+5s\n' yyyymmdd/ensX winner capr_yyyymmdd total mapl daily diur month ) "

    #todo:  calculate # of files
    for dexp in ${arrdexp[@]} ;do  
        local blwinner=$( exp_checkwinner $dexp $_ddata )
        local fcstdate=$( echo $dexp | rev | cut -d'/' -f2 | rev ) 
        local ensm=$( echo $dexp | rev | cut -d'/' -f1 | rev ) 

        local capric_yyyymmdd=$( date -d "$fcstdate -1days" +%Y%m%d )
        local capr_yyyymmdd=$( set_capdates $fcstdate $blwinner )
        local capric_hhmmss="210000"
        local bllastdayofthemonth=$( fcal_lastdayofthemonth $capric_yyyymmdd )
        local blfirstdayofthemonth=$( fcal_firstdayofthemonth $capric_yyyymmdd )
        arrcollmonpost_noreft=()

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
            for collmonpost in ${arrcollmonpost[@]};do
                collreftime=$( grep $collmonpost.ref_time $_fhis2 | tr -s '[:space:]' | rev  | cut -d' ' -f1 | cut -c2- | rev )
            
                if [[ -z $collreftime ]] || (( $capric_hhmmss != $collreftime ));then
                    arrcollmonpost_noreft+=( $collmonpost )
                fi
            done
 
            #todo:  calculate a number of total output files.
            numcoll=${#arrcoll[@]}
            numcollmon_mapl=${#arrcollmon_mapl[@]}        
            numcollmon_maplpartial=${#arrcollmon_mapl[@]}        
            numcollmonpost=${#arrcollmonpost[@]}
            numcollmonpost_diu=${#arrcollmonpost_diu[@]}        
            numcollmonpost_save=${#arrcollmonpost_save[@]}        
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
        fi
        grandtotal=$(( grandtotal + $numftotal_calc ))
        wmessage "$( printf '%-15s %+6s %+15s %+5s %+5s %+5s %+5s %+5s\n' $fcstdate/$ensm $blwinner $capr_yyyymmdd $numftotal_calc $numfmonmapl_calc $numfday_calc $numfdiu_calc $numfmonpost_calc ) "
    done

    wmessage 
    wmessage "Number of Exp (per Year): $( printf '%+5s\n' ${#arrdexp[@]} )"
    wmessage "  Grand Total (per Year): $( printf '%+5s\n' $grandtotal )"
    return
}

analyzing_outputsize(){
     
    #description: calculate total size of YYYYMM dir 
    local _fhis2=HISTORY_2.rc
    local hst=$hostarc
    local strrst=restarts
    local strmom=ocean_daily
    local arrdexp=($( find $DFCST/* -maxdepth 1 -mindepth 1 -type d -name 'ens*' | grep $usericyyyy | sort -V ))
    local thishis2=$( printf "%s/$_fhis2\n" ${arrdexp[@]} | xargs -i bash -c '[[ -f {} ]] && echo {}' | sort -V | head -1 )
    local arrcoll=($( exp_getcollections $thishis2 ))
    local arrcoll_mon=($( printf "%s\n" ${arrcoll[@]} | grep _1mo_glo | sort -V ))
    local arrcoll_nonmon=($( printf "%s\n" ${arrcoll[@]} | grep -v _1mo_glo | sort -V ))
    local arrcoll_nonmonsave=($( printf "%s\n" ${arrcoll_nonmon[@]} | grep -E '_sfc|_slv' | sort -V ))
    local arrcoll_nonmonrm=($( printf "%s\n" ${arrcoll_nonmon[@]} | grep -vE '_sfc|_slv' | sort -V ))
    local arrcoll_allsave=( ${arrcoll_mon[@]} ${arrcoll_nonmonsave[@]} )
    local arrcoll_lfeonly=( ${arrcoll_nonmonrm[@]} $strrst $strmom )
    local ftmp=tmp_${strscr}_${FUNCNAME[0]}_${usericyyyy}_deleteme
    local ftmpp=tmp_${strscr}_${FUNCNAME[0]}_${usericyyyy}_deleteme2
    local ftmp1=$dmess/${strscr}_${FUNCNAME[0]}_${usericyyyy}_LFE
    local ftmp2=$dmess/${strscr}_${FUNCNAME[0]}_${usericyyyy}_LFE_sizebycollection
    local ftmp3=$dmess/${strscr}_${FUNCNAME[0]}_${usericyyyy}_LFE_LessThanEq2Threshold
    local ftmp4=$dmess/${strscr}_${FUNCNAME[0]}_${usericyyyy}_LFE_GreaterThreshold
    local ftmp5=$dmess/${strscr}_${FUNCNAME[0]}_${usericyyyy}_LFE_nonmon_3mon
    local ftmp6=$dmess/${strscr}_${FUNCNAME[0]}_${usericyyyy}_LFE_nonmon_3mon_total
    local threshold_T=20
    local threshold=$( echo "$threshold_T * 1024^4" | bc -l )
    local peta=$( echo "1024^5" | bc -l )
    local totyears=40
    #local ftmp6=$dmess/${strscr}_${FUNCNAME[0]}_${usericyyyy}_LFE_rstartsonly
    local dexp coll

    #note: ls --help says:
    #   SIZE is an integer and optional unit (example: 10M is 10*1024*1024).  Units
    #   are K, M, G, T, P, E, Z, Y (powers of 1024) or KB, MB, ... (powers of 1000).
    #ls -l --si message_monitor
    #       gives si unit ( 1k = 1000bytes)

    if [[ ! -f $ftmp1 ]];then
        #todo:  write output size for outputs which will stay on LFE
        local intdepth=1

        for dexp in ${arrdexp[@]};do
            set_rstfcstdate $dexp
            local dlou=$DARCH/$fcstdate/$ensm

            #todo:  get a file with name of existing monthly, daily, & diurnal files from lou.
            ssh -q $hst "find $dlou/* -maxdepth $intdepth -mindepth $intdepth -type f -name '${fcstdate}*.nc*' -o -name '*${strrst}*.tar' -o -name '*${strmom}*.nc' | grep -v bbftp.tmp | xargs -i stat --printf="'"'"%Y %s %n\n"'"'" {} >> $ftmp1" 2>/dev/null
        done 
    fi

    if [[ ! -f $ftmp2 ]];then
        
        #wmessage "Collection # of Files Bytes T"

        #todo:  calculate total size by collection
        for coll in ${arrcoll[@]};do
            [[ -f $ftmp ]] && rm -f $ftmp
            cat $ftmp1 | grep $coll 2>/dev/null >> $ftmp
            
            #todo:  calculate total size for PFE
            local arrsize=($( cat $ftmp | cut -d' ' -f2 ))
            local numadd=$( echo ${arrsize[@]} | sed 's# # + #g' )
            local numtot_bytes=$( echo "$numadd" | bc -l )
            local numtot=$( echo "scale=2;( $numtot_bytes ) / 1024^4" | bc -l )
            local numtot_scale0=$( echo "scale=0;( $numtot_bytes ) / 1024^4" | bc -l )
            local thisunit=T

            if (( $numtot_scale0 < 1 ));then
                local numtot=$( echo "scale=2;( $numtot_bytes ) / 1024^3" | bc -l )
                local thisunit=G
            fi

            #echo "$coll ${numtot}$thisunit $( LC_ALL=en_US.UTF-8 printf "%'d" $( cat $ftmp | wc -l ) ) $( LC_ALL=en_US.UTF-8 printf "%'d" $numtot_bytes )" >> $ftmp2
            #echo "$coll ${numtot}$thisunit $( LC_ALL=en_US.UTF-8 printf "%'d" $( cat $ftmp | wc -l ) ) $numtot_bytes " >> $ftmp2
            echo "$coll ${numtot}$thisunit $( cat $ftmp | wc -l ) $numtot_bytes " >> $ftmp2

            [[ -f $ftmp ]] && rm -f $ftmp
        done
    fi

    #todo:  create a list with large and small, based on threshold,  output in separate files.
    if [[ ! -f $ftmp3 || ! -f $ftmp4 ]];then 
        [[ -f $ftmp ]] && rm -f $ftmp
        
        local numln=$( cat $ftmp2 | wc -l )

        for x in $( seq 1 $numln );do 
            local thisline=$( cat $ftmp2 | head -n +$x | tail -1 )
            local numbytes=$( echo $thisline | rev | cut -d' ' -f1 | rev )

            if (( $numbytes <= $threshold ));then
                echo $thisline >> $ftmp3 
            else
                echo $thisline >> $ftmp4
            fi
        done
        cat $ftmp3 | sort -V -k4 -t' ' >> $ftmp
        mv $ftmp $ftmp3
        cat $ftmp4 | sort -V -k4 -t' ' >> $ftmp
        mv $ftmp $ftmp4
    fi

    #[[ -f $ftmp5 ]] && rm -f $ftmp5

    #todo:  extract the first three month outputs for non-monthly files 
    if [[ ! -f $ftmp5 ]];then
        
        [[ -f $ftmp ]] && rm -f $ftmp

        #todo:  get yyyymmdd
        local arryyyymmdd=()
        local arryyyymmddens=()
        local arryyyymmddenscoll=()

        #todo:  get all icdates
        for x in {2..12};do
            arryyyymmdd+=($( fcal_icall $x $usericyyyy ))
        done 
        arryyyymmdd+=($( fcal_icall 1 $(( usericyyyy +1 )) ))
        
        #todo:  combine date, ensemble members, and collections
        arryyyymmddens=($( printf '%s\n' ${arryyyymmdd[@]} | xargs -i printf "{}/ens%s\n" ${arrintens[@]} | xargs -i bash -c "[[ -d $DFCST/{} ]] && echo {}" ))
        arryyyymmddenscoll=($( printf '%s\n' ${arryyyymmddens[@]} | xargs -i printf "{}/%s\n" ${arrcoll_nonmon[@]} | sort -V )) 

        #todo:  extract nonmonthly files. 
        for yyyymmddenscoll in ${arryyyymmddenscoll[@]};do

            local arryyyymm=()

            #todo:  get the first and the second full month
            local yyyymmdd=$( echo $yyyymmddenscoll | cut -c1-8 )
            local yyyymm=$( echo  $yyyymmdd | cut -c1-6 )

            for x in {1..2};do 
                yyyymm=$( fcal_nextmonth $yyyymm ) 
                arryyyymm+=( $yyyymm )
            done 


            local     coll=$( echo $yyyymmddenscoll | cut -d'/' -f3 )
            local arrfname=($( printf "$DARCH/$yyyymmddenscoll/$yyyymmdd.$coll.%s.nc4.tar\n" ${arryyyymm[@]} ))
            local strfname=$( echo ${arrfname[@]} | sed 's# #|#g' )

            grep -E "$strfname" $ftmp1 2>/dev/null >> $ftmp
        done

        local totnumf=$( echo "${#arryyyymmddenscoll[@]} * 2 " | bc )
        [[ -f $ftmp ]] && mv $ftmp $ftmp5 
    fi
    
    #todo:  calculate non-monthly file size by collections
    if [[ -f $ftmp5 && ! -f $ftmp6 ]];then 

        [[ -f $ftmp ]] && rm -f $ftmp

        #todo:  extract non-monthly from ftmp5 and calculate total
        for coll in ${arrcoll_nonmon[@]} ;do 
            cat $ftmp5 | grep $coll 2>/dev/null >> $ftmp
            
            #todo:  calculate total size for PFE
            local arrsize=($( cat $ftmp | cut -d' ' -f2 ))
            local numadd=$( echo ${arrsize[@]} | sed 's# # + #g' )
            local numtot_bytes=$( echo "$numadd" | bc -l )
            local numtot=$( echo "scale=2;( $numtot_bytes ) / 1024^4" | bc -l )
            local numtot_scale0=$( echo "scale=0;( $numtot_bytes ) / 1024^4" | bc -l )
            local thisunit=T

            if (( $numtot_scale0 < 1 ));then
                local numtot=$( echo "scale=2;( $numtot_bytes ) / 1024^3" | bc -l )
                local thisunit=G
            fi

            echo "$coll ${numtot}$thisunit $( cat $ftmp | wc -l ) $numtot_bytes " >> $ftmp6

            [[ -f $ftmp ]] && rm -f $ftmp
        done
        
        cat $ftmp6| sort -V -k4 -t' ' | grep -v _1mo_glo_ >> $ftmp
        [[ -f $ftmp ]] && mv $ftmp $ftmp6
    fi

    local runhere=false
    if $runhere;then 
        #todo:  print outputs    
        local arrftmp=( $ftmp3 $ftmp4 )
        local numtot_grandtotal=0

        for thisfile in ${arrftmp[@]};do
            [[ -f $ftmp ]] && rm -f $ftmp

            local arrsize=($( cat $thisfile | cut -d' ' -f4 ))
            local numadd=$( echo ${arrsize[@]} | sed 's# # + #g' )
            local numtot_bytes=$( echo "$numadd" | bc -l )
            local numtot=$( echo "scale=2;( $numtot_bytes ) / 1024^4" | bc -l )
            local numtotyears_bytes=$( echo "$numtot_bytes * $totyears " | bc -l )
            local numtotyears=$( echo "scale=2;( $numtotyears_bytes ) / 1024^5" | bc -l )
            local arrfnum=($( cat $thisfile | cut -d' ' -f3 ))
            local numadd_numf=$( echo ${arrfnum[@]} | sed 's# # + #g' )
            local numtot_numf=$( echo "$numadd_numf" | bc -l )
            local arrthiscoll=($( cat $thisfile | cut -d' ' -f1 | sort -V  ))
            local numln=$( cat $thisfile | wc -l )

            numtot_grandtotal=$( echo "$numtot_grandtotal + $numtot_bytes " | bc -l )

            if [[ "$thisfile" == "utility_analyzing_outputsize_1982_LFE_LessThanEq2Threshold" ]]; then
                #wmessage "FIles <= ${threshold_T}T:"
                wmessage "            Number of Files : $( LC_ALL=en_US.UTF-8 printf "%'d" $numtot_numf )"
                wmessage "        Total Size in bytes : $( LC_ALL=en_US.UTF-8 printf "%'d" $numtot_bytes )"
                wmessage "   Total Size in T per Year : ${numtot}T"
                wmessage "   Total Size in P (40yrs)  : ${numtotyears}P"
                wmessage
            #else
            #    wmessage "Files  > ${threshold_T}T:"
            fi

            echo "  Collections Size/Year 40YR-Total" >> $ftmp
            for x in $( seq 1 $numln );do
                local thisline=$( cat $thisfile | head -n +$x | tail -1 )
                local coll=$( echo $thisline | cut -d' ' -f1 ) 
                local size=$( echo $thisline | cut -d' ' -f2 )
                local size_bytes=$( echo $thisline | cut -d' ' -f4 )
                local size_all=$( echo "$size_bytes * 40" )

                if (( $size_all > $peta ));then
                    local size_big="$( echo "scale=2; $size_all / 1024^5 " | bc -l )P"
                else
                    local size_big="$( echo "scale=2; $size_all / 1024^4 " | bc -l )T"
                fi

                if [[ *"${arrcoll_nonmonsave[@]}"* =~ "$coll" ]];then
                    echo "  $coll *$size $size_big" >> $ftmp
                else
                    echo "  $coll $size $size_big" >> $ftmp
                fi
            done 
                
            rev $ftmp | column -t -s' ' | rev   
            [[ -f $ftmp ]] && rm -f $ftmp

            wmessage
        done

        numtot_grandtotal_T=$( echo "scale=2; $numtot_grandtotal * $totyears / 1024^5 " | bc -l )

        [[ -f $ftmp ]] && rm -f $ftmp
            
        wmessage "Non-monthly Collections Kept on PFE :"
        echo "  Collections Size/Year 40YR-Total" >> $ftmp
        for coll in ${arrcoll_nonmonsave[@]};do 
            local coll=$( cat $ftmp2 | grep $coll | cut -d' ' -f1 ) 
            local size=$( cat $ftmp2 | grep $coll | cut -d' ' -f2 ) 
            local size_bytes=$( cat $ftmp2 | grep $coll | cut -d' ' -f4 ) 
            local size_all=$( echo "$size_bytes * 40" )

            if (( $size_all > $peta ));then
                local size_big="$( echo "scale=2; $size_all / 1024^5 " | bc -l )P"
            else
                local size_big="$( echo "scale=2; $size_all / 1024^4 " | bc -l )T"
            fi

            echo "  $coll $size $size_big" >> $ftmp
        done 
        rev $ftmp | column -t -s' ' | rev   
        [[ -f $ftmp ]] && rm -f $ftmp

        wmessage 
        wmessage "Grand Total : ${numtot_grandtotal_T}P"

    fi 

    local runhere=true
    if $runhere;then
        [[ -f $ftmp ]] && rm -f $ftmp

        local grandtotal_mon_bytes=0
        local grandtotal_nonmon_bytes=0
        local grandtotal_nonmon_bytes_3mononly=0
        local grandtotal_nonmon_bytes_pfe=0
        local grandtotal_nonmon_bytes_3mononly_pfe=0
        local arrcoll_nonmon_sortedbysize=($( cat $ftmp2 | grep -v _1mo_glo | sort -V -k4 -t' ' | cut -d' ' -f1 ))
        local    arrcoll_mon_sortedbysize=($( cat $ftmp2 | grep _1mo_glo | sort -V -k4 -t' ' | cut -d' ' -f1 ))

        wmessage "v3 Collections & Size:"
        echo "   Current Current 3monOnly 3monOnly " >> $ftmp
        echo "  Collections Size/Year 40YR-Total Size/Year 40YR-Total Diff" >> $ftmp

        for coll in ${arrcoll_mon_sortedbysize[@]};do 
            local coll=$( cat $ftmp2 | grep $coll | cut -d' ' -f1 ) 
            local size=$( cat $ftmp2 | grep $coll | cut -d' ' -f2 ) 
            local size_bytes=$( cat $ftmp2 | grep $coll | cut -d' ' -f4 ) 
            local size_all=$( echo "$size_bytes * 40" )

            #todo:  determine unit
            if (( $size_all > $peta ));then
                local size_big="$( echo "scale=2; $size_all / 1024^5 " | bc -l )P"
            else
                local size_big="$( echo "scale=2; $size_all / 1024^4 " | bc -l )T"
            fi

            grandtotal_mon_bytes=$(( grandtotal_mon_bytes + $size_all )) 
            echo "  $coll $size $size_big   " >> $ftmp
        done 

        for coll in ${arrcoll_nonmon_sortedbysize[@]};do
            #todo:  calculate currnet size for nonmonthly 
            local coll_before=$( cat $ftmp2 | grep $coll | cut -d' ' -f1 ) 
            local size_before=$( cat $ftmp2 | grep $coll | cut -d' ' -f2 ) 
            local size_bytes_before=$( cat $ftmp2 | grep $coll | cut -d' ' -f4 ) 
            local size_all_before=$( echo "$size_bytes_before * 40" )

            grandtotal_nonmon_bytes=$(( grandtotal_nonmon_bytes + $size_all_before ))

            if (( $size_all_before > $peta ));then
                local size_big_before="$( echo "scale=2; $size_all_before / 1024^5 " | bc -l )P"
            else
                local size_big_before="$( echo "scale=2; $size_all_before / 1024^4 " | bc -l )T"
            fi

            #todo:  calculate size for nonmonthly if we were to save only the first 2 full months.
            local coll=$( cat $ftmp6 | grep $coll | cut -d' ' -f1 ) 
            local size=$( cat $ftmp6 | grep $coll | cut -d' ' -f2 ) 
            local size_bytes=$( cat $ftmp6 | grep $coll | cut -d' ' -f4 ) 
            local size_all=$( echo "$size_bytes * 40" )

            grandtotal_nonmon_bytes_3mononly=$(( grandtotal_nonmon_bytes_3mononly + $size_all ))

            if (( $size_all > $peta ));then
                local size_big="$( echo "scale=2; $size_all / 1024^5 " | bc -l )P"
            else
                local size_big="$( echo "scale=2; $size_all / 1024^4 " | bc -l )T"
            fi

            local diff_bytes=$( echo "scale=2; $size_all_before - $size_all " | bc -l )

            if (( $diff_bytes > $peta ));then
                local diff="$( echo "scale=2; $diff_bytes / 1024^5 " | bc -l )P"
            else
                local diff="$( echo "scale=2; $diff_bytes / 1024^4 " | bc -l )T"
            fi

            if [[ *"${arrcoll_nonmonsave[@]}"* =~ "$coll" ]];then
                grandtotal_nonmon_bytes_pfe=$(( grandtotal_nonmon_bytes_pfe + size_all_before ))
                grandtotal_nonmon_bytes_3mononly_pfe=$(( grandtotal_nonmon_bytes_3mononly_pfe + size_all ))

                echo "  $coll *$size_before *$size_big_before *$size *$size_big *$diff" >> $ftmp
            else
                echo "  $coll $size_before $size_big_before $size $size_big $diff" >> $ftmp
            fi

        done 
   
        rev $ftmp | column -t -s' ' | rev   
        [[ -f $ftmp ]] && rm -f $ftmp

        local grandtotal_mon=$( echo "scale=2; $grandtotal_mon_bytes / 1024^5" | bc -l )P
        
        local grandtotal_nonmon_PFE_before=$( echo "scale=2; $grandtotal_nonmon_bytes_pfe / 1024^5" | bc -l )P
        local grandtotal_nonmon_PFE_after=$( echo "scale=2; $grandtotal_nonmon_bytes_3mononly_pfe / 1024^5" | bc -l )P
        local grandtotal_nonmon_LFE_before=$( echo "scale=2; $grandtotal_nonmon_bytes / 1024^5" | bc -l )P
        local grandtotal_nonmon_LFE_after=$( echo "scale=2; $grandtotal_nonmon_bytes_3mononly / 1024^5" | bc -l )P

        local grandtotal_PFE_before_bytes=$( echo "$grandtotal_mon_bytes + $grandtotal_nonmon_bytes_pfe " | bc -l )
        local  grandtotal_PFE_after_bytes=$( echo "$grandtotal_mon_bytes + $grandtotal_nonmon_bytes_3mononly_pfe" | bc -l )
        local grandtotal_PFE_before=$( echo "scale=2; $grandtotal_PFE_before_bytes / 1024^5" | bc -l )P
        local grandtotal_PFE_after=$( echo "scale=2; $grandtotal_PFE_after_bytes / 1024^5" | bc -l )P
        local grandtotal_PFE_diff=$( echo "scale=2; ( $grandtotal_PFE_before_bytes - $grandtotal_PFE_after_bytes ) / 1024^5 " | bc -l )P

        local grandtotal_LFE_before_bytes=$( echo "$grandtotal_mon_bytes + $grandtotal_nonmon_bytes" | bc -l )
        local grandtotal_LFE_after_bytes=$( echo "$grandtotal_mon_bytes + $grandtotal_nonmon_bytes_3mononly" | bc -l )
        local grandtotal_LFE_before=$( echo "scale=2;$grandtotal_LFE_before_bytes / 1024^5 " | bc -l )P
        local grandtotal_LFE_after=$( echo "scale=2; $grandtotal_LFE_after_bytes  / 1024^5 " | bc -l )P
        local grandtotal_LFE_diff=$( echo "scale=2; ( $grandtotal_LFE_before_bytes - $grandtotal_LFE_after_bytes ) / 1024^5 " | bc -l )P

        wmessage
        wmessage
        echo "  40YR-TOTAL_NOW 40YR-TOTAL_3monOnly" >> $ftmp
        echo " Monthly $grandtotal_mon $grandtotal_mon" >> $ftmp
        echo " NonMonthly_PFE $grandtotal_nonmon_PFE_before $grandtotal_nonmon_PFE_after" >> $ftmp
        echo " NonMonthly_LFE $grandtotal_nonmon_LFE_before $grandtotal_nonmon_LFE_after" >> $ftmp

        rev $ftmp | column -t -s' ' | rev   
        [[ -f $ftmp ]] && rm -f $ftmp

        wmessage
        wmessage
        echo " Host 40YR-TOTAL_Now 40YR-TOTAL_3monOnly Diff" >> $ftmp
        echo " PFE $grandtotal_PFE_before $grandtotal_PFE_after $grandtotal_PFE_diff" >> $ftmp
        echo " LFE $grandtotal_LFE_before $grandtotal_LFE_after $grandtotal_LFE_diff" >> $ftmp

        rev $ftmp | column -t -s' ' | rev   
        [[ -f $ftmp ]] && rm -f $ftmp
    fi

    return
}

arch_lock() {
    #description:   find gcmarch.lock that has been existed more than 24 hors.
    local _blwhead=false
    local fmessage=$dmess/message_${strscr}_${FUNCNAME[0]}_${strexpid}
    local arrfarchlock=($( find $DFCST/* -maxdepth 3 -mindepth 3 -type f -name gcmarch.lock 2>/dev/null ))
    local _dstdout=$dstdout/${FUNCNAME[0]}/$strdout
    local _blsendmsgall=false

    (( ${#arrfarchlock[@]} == 0 )) && (( $writetofile == 0 )) && wmessage "** NO gcmarch.lock exists." && return

    local hrbefore=10
    local sec_now=$( date +%s )
    local sec_past=$( echo " $sec_now - ( 3600 * $hrbefore )" | bc )
    local arrfarchlock_delete=()
    local farchlock

    #todo:  get all running/q'd jobs 
    local arrjobs=($( /u/scicon/tools/bin/qstat -u $USER -W fmt_Queue="-maxw 40" -W o=+Rank0 | sort -V -k4 -t' ' | tr -s '[:space:]' | cut -d' ' -f4 | cut -d'.' -f1 | sort -V | uniq | grep ens ))
    
    #todo:  check if lock is older than hrbefore
    for farchlock in ${arrfarchlock[@]};do
        local _dexp=$( echo $farchlock | rev | cut -d'/' -f3- | rev )
        
        [[ ! -f $farchlock ]] && continue 

        local fdate_sec=$(stat --printf="%Y" $farchlock )
        local fdate=$( date -d @$fdate_sec +%Y%m%d_%H%M )

        if (( $fdate_sec < $sec_past ));then
            set_rstfcstdate $_dexp
            #cnt_jobs $_dexp
            local num_rarc=$( printf '%s\n' ${arrjobs[@]} | grep A$fcstdate$ensm >/dev/null | wc -l )

            #todo:  check if archive is running 
            (( $num_rarc  > 0 )) && local blarchrunning=true || local blarchrunning=false

            #todo:  check if screen is running for archive:
            #ssh $hst screen -ls | grep arch_${strexpid}_$fcstdate$ensm 2>/dev/null
            screen -ls | grep arch_${strexpid}_$fcstdate$ensm 2>/dev/null
            local _status_grep=$?
            (( $_status_grep > 0 )) && local blscr=false || local blscr=true

            if ! $blarchrunning && ! $blscr ;then
                if ! $_blwhead;then
                   wmessage "------------- ----------------------------------------------------------------------------"
                   wmessage "yyyymmdd_hhmm                            Experiment"
                   wmessage "------------- ----------------------------------------------------------------------------"
                    _blwhead=true
                fi

                $bluserrmlock && arrfarchlock_delete+=( $farchlock )
                  
                wmessage "$fdate $farchlock"
            fi
        fi
    done 

    wmessage

    if $bluserrmlock && (( ${#arrfarchlock_delete[@]} > 0 )) ;then
        rm -f ${arrfarchlock_delete[@]} 2>/dev/null 
        wmessage "** locks are removed"
        local blsendmsg=true
    elif (( ${#arrfarchlock_delete[@]} == 0 ));then 
        local blsendmsg=false
    fi
   
    #todo:  send email 
    if $blsendmsg && (( $writetofile == 1 ));then 
        sendmsg ${FUNCNAME[0]} $_blsendmsgall $_dstdout $fmessage
        #local status_sendmsg=$?
        #(( $status_sendmsg > 0 )) && die "failed to send email"
    fi
    return
}

resend_email() {
    #description:   resending email with pckwinners stdout and a plot

    local dout=$cdir/output/pckwinners/$strdout
    local dstdout=$cdir/stdout/pckwinners/$strdout
    local icyyyymm=$usericyyyymm
    local msg_subject="pckwinners.fcst : $icyyyymm Winners"
    local strfplt=subsample_${reg}_${strexpid}_$icyyyymm
    local fplt=$( find $dout/* -type f -name "${strfplt}_[1-2]*.png" 2>/dev/null )
    local fwinner=$( find $dout/* -type f -name "winners_${reg}_${strexpid}_${icyyyymm}_*.txt" 2>/dev/null )
    #local findexes=$( find $dout/* -type f -name "${reg}_${strexpid}_${icyyyymm}_*.txt" 2>/dev/null | xargs -i basename {} )
    local fmessage_real=$( find $dstdout/* -type f -name "message_pckwinners_fcst_$icyyyymm" 2>/dev/null )
    local fmessage=message_${strscr}_${FUNCNAME[0]}
    local feadd=$cdir/mailadd
    local feaddextra=$cdir/mailadd_extra
    local bleaddextra=true

    [[ -f $feadd ]] && local eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

    #todo:  add extra email address
    if $bleaddextra ;then
        local eadds_extra=$( echo $( misc_readfbyline $feaddextra ) | sed -e "s/ /;/g" )
        local eadds="$eadds;$eadds_extra"
        local arreadds=($( echo $eadds | sed -e "s/;/ /g" ))
    fi
    [[ -f $fmessage ]] && rm -f $fmessage

    local writetofile=1
    msg_wheader 
    local writetofile=0
    
    cat $fwinner       >> $fmessage
    cat $fmessage_real >> $fmessage
    
#wmessage "@$LINENO" "mutt -s $msg_subject -a $fplt -- $eadds < $fmessage"
#exit
    mutt -s "$msg_subject" -a $fplt -- "$eadds" < $fmessage & 
    [[ -f $fmessage ]] && rm -f $fmessage

    wmessage "Email sent to:"
    ahand_warr ${arreadds[@]} 
    
    return
}


clean_dir() {
    [[ -n $flock && -f $flock ]] && rm -f $flock
    [[ -n $flst_lfe && -f $flst_lfe ]] && rm -f $flst_lfe
    
    $opta && find $cdir/* -maxdepth 0 -type f -name "tmp_${strscr}_*_${strexpid}_*" 2>/dev/null

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
    source ~/knakada/.bashrc_kn
    blnode_nas=true

elif [[ "${hstname:0:4}" == borg || "${hstname:0:4}" == warp ]];then 
    #note:  this is for host other than pfe @ NCCS which has a name starting with 'borg' or 'warp'
    blnode_nccs=true
else 
    exit
fi

strscr=$(basename "$0" | cut -d'.' -f1 | cut -d'_' -f2 )
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cdir=$SCRIPTPATH

[[ -z $cdir || ! -d $cdir ]] && echo "cdir is undefined or does not exist" && exit

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

writetofile=0
opta=false
optb=false
optd=false
optf=false
optg=false
opti=false
optk=false
optl=false
optm=false
optn=false
opto=false
optp=false 
optq=false 
optr=false 
optR=false 
opts=false
optt=false
optu=false
optw=false
optarchlock=false
optsftcdays=false
optredopost=false
optresendemail=false
optcalcnumout=false
optoutarc2exp=false
optupdrunpstscr=false
optgetwtime=false
optrmextra=false
optarcwinners=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
    do many many many things.
    
    Usage: program [opt] [-a|-g|-p ICYYYYMM-ICYYYYMM] [-k|-l ICYYYY ] [-b|-q|-t ICYYYYMM] srcme_file

    Operations:
    Gcm_post Related:
            --upd-runpstscr=[FILE,DEXP]     copy the latest run_gcmpost.sh into EXP or 
                                            EXP listed in FILE 
    Archive Related:
        -a, --num-archived=[FILE,DEXP]      print # of archived files for YYYYMM or
                                            EXP list in FILE 
        -u, --upd-archscr=[FILE,DEXP]       copy the latest run_gcmarch.sh into EXP or 
                                            EXP listed in FILE 
        -o, --cnt-shiftc                    count running shiftc for archiving fcst  
            --arch-lock=BOOLEAN             find gcmarch.lock that has been existed 
                                            more than 10 hours and delete
                                            them if user's input is true ** INPUT REQUIRED
            --sftc-days=BOOLEAN             find shiftc that are running for archiving, 
                                            run_gcmarch.sh, more than 5 hours.
                                            true to stop shiftcs.
            --outarc2exp [FILE,DEXP]        transfer output back from archive to exp dir for
                                            EXP or EXP listed in FILE
            --rmextra [FILE,DEXP]           remove extra outputs from exp ran more than 
                                            3 months (Works only for non-winners)
                                            
    Restarts Related:
        -n, --cnt-lferst                    count number of control and perturbed 
                                            rst tar files on LFE
        -m, --mis-pferst                    print missing restart files on PFE

    Exp Setup Related:
        -g, --check-dexp                    check if exp dir between YYYYMM and
                                            YYYYMM has all files that will need 
                                            to run **INPUT REQUIRED
    Submit Related:
        -r, --clean-subdata                 remove empty files in data/submit dir 
                                            and delete duplicate exp in data_submit_* 
                                            files.
    Sst Related:
        -q, --cnt-sst ICYYYYMM              count sst tar for the second full 
                                            month for the given
        -b, --arch-sst ICYYYYMM             archive sst for the second full month 
                                            for the given ICYYYYMM 
        -t, --compress-sst ICYYYYMM         compress sst outputs for the 2nd full 
                                            month for ICYYYYMM that necessry for 
                                            pckwinners
    Winner Related:
        -s, --mksymlink-lfe [ICYYYYMM,all]  make symlinks to winners on lfe winner dir
                                            enter "'"'"all "'"'" to make all ICYYYMM
        -p, --print-winners YYYYMM-YYYYMM   print winner between YYYYMM-YYYYMM 
            --resend-email  ICYYYYMM        re-send email with pckwinners stdout 
                                            and winner selections for ICYYYYMM 
            --arc-winners ICYYYYMM          arhive sst files and pckwinner output
                                            files (winners_*, plots, & nino_* )
    Run-time Related:
            --getwtime                      read exp stdout and write used walltime 
                                            between YYYYMM-YYYYMM *INPUT REQUIRED
    
    Misc Operations
        -i, --clean-sftc                    stop gcm archiving shiftc operations 
                                            that become zombies
        -k, --out-size YYYYMM               output size for YYYYMM *INPUT REQUIRED
        -l, --calc-output YYYY              calculate size of YYYY outputs that 
                                            stay on PFE by collection 
            --calc-numout YYYY              calculate number of YYYY outputs

    Other options:
        -d                                  a full path to an experiment dir
        -f, --input-file                    a file with a list of experiment 
                                            directories 
        -h  --help                          show this help text
        -w  --write-message                 write stdout/err in a file
"
#        -a                    check currently running archive status.
#                              If you enter qid as an option argument, script will check  status of the 
#                              archiving. Otherwise, it will check all running archive.
#        -a, --add-cron        add gcmarch to cron (require opt d or f)
#        -m, --rm-cron         remove gcmarch from cron (require opt d or f)
#        -c                    remove duplicates from data_submit_* files 
#        -R                    execute run_gcmarch.sh in a screen session (require opt d or f) 

#ref: https://mywiki.wooledge.org/BashFAQ/035
# Initialize all the option variables.
# This ensures we are not contaminated by variables from the environment.
file=
verbose=0
cnt=0
while :; do
    case $1 in
#Gcm_post related        
        --upd-runpstscr   )  [[ "$2" ]] && userinput=$2 && optupdrunpstscr=true && shift \
                             || die "opt d requires an argument";;
#Archive Related
       -a|--num-archived  )  [[ "$2" ]] && userinput=$2 && opta=true && shift \
                             || die "opt a or --num-archived requires an argument";;
       -u|--upd-archscr   )  [[ "$2" ]] && userinput=$2 && optu=true && shift \
                             || die "opt d requires an argument";;
       -o|--cnt-shiftc    )  opto=true;;
          --arch-lock     )  optarchlock=true;;
          --arch-lock=?*  )  optarchlock=true && bluserrmlock=${1#*=};;
          --arch-lock=    )  die "--arch-lock requires a non-empty option argument.";; 
          --sftc-days     )  optsftcdays=true;;
          --sftc-days=?*  )  optsftcdays=true && blsftc=${1#*=};;
          --sftc-days=    )  die "--sftc-days requires a non-empty option argument.";; 
          --outarc2exp    )  [[ "$2" ]] && userinput=$2 && optoutarc2exp=true && shift \
                             || die "--outarc2exp requires an argument";;
          --rmextra       )  [[ "$2" ]] && userinput=$2 && optrmextra=true && shift \
                             || die "--rmextra requires an argument";;
#Restarts Related:
       -n|--cnt-lferst    )  optn=true;; 
       -m|--mis-pferst    )  optm=true;; 
#Exp Setup Related:
       -g|--check-dexp    )  [[ "$2" ]] && startyyyymm=$( echo $2 | cut -d'-' -f1 ) && \
                             endyyyymm=$( echo $2 | cut -d'-' -f2 ) && optg=true && shift \
                             || die "opt g requires an option argument.";;
#Submit Related:
       -r|--clean-subdata )  optr=true;; 
#Sst Related:
       -q|--cnt-sst       )  [[ "$2" ]] && usericyyyymm=$2 && optq=true && shift \
                             || die "opt q or --cnt-sst requires an argument";;
       -b|--arch-sst      )  [[ "$2" ]] && usericyyyymm=$2 && optb=true && shift \
                             || die "opt b or --arch-sst requires an argument";;
       -t|--compress-sst  )  [[ "$2" ]] && usericyyyymm=$2 && optt=true && shift \
                             || die "opt t or --compress-sst requires an argument";;
#Winner Related:
       -s|--mksymlink-lfe )  [[ "$2" ]] && usericyyyymm=$2 && opts=true && shift \
                             || die "opt s requires an argument";;
       -p|--print-winners )  [[ "$2" ]] && startyyyymm=$( echo $2 | cut -d'-' -f1 ) && \
                             endyyyymm=$( echo $2 | cut -d'-' -f2 ) && optp=true && shift \
                             || die "opt p requires an argument";;
          --resend-email  )  [[ "$2" ]] && usericyyyymm=$2 && optresendemail=true && shift \
                             || die "option --resend-email requires an argument";;
          --arc-winners   )  [[ "$2" ]] && usericyyyymm=$2 && optarcwinners=true && shift \
                             || die "option --arc-winners requires an argument";;
#Run-time Related
          --getwtime      )  [[ "$2" ]] && startyyyymm=$( echo $2 | cut -d'-' -f1 ) && \
                             endyyyymm=$( echo $2 | cut -d'-' -f2 ) && optgetwtime=true && shift \
                             || die "option --getwtime requires an option argument";;
#Misc Operations
       -i|--clean-sftc    )  opti=true;;
       -k|--out-size      )  [[ "$2" ]] && usericyyyy=$2 && optk=true && shift \
                             || die "opt k or --out-size requires an argument";;
       -l|--calc-output   )  [[ "$2" ]] && usericyyyy=$2 && optl=true && shift \
                             || die "opt l or --calc-output requires an argument";;
          --calc-numout   )  [[ "$2" ]] && usericyyyy=$2 && optcalcnumout=true && shift \
                             || die "option --calc-numout requires an argument";;
#Other options:
       -d                 )  [[ "$2" ]] && arrdexp=( $2 ) && optd=true && shift \
                             || die "opt d requires an argument";;
       -f|--input-file    )  # Takes an option argument; ensure it has been specified.
                             [[ -n "$2" ]] && fdexp=$2 && optf=true && shift || die "opt f or --file requires a non-empty option argument.";;
          --input-file=?* )  fdexp=${1#*=};;                # Delete everything up to "=" and assign the remainder.
          --input-file=   )  die "--file requires a non-empty option argument.";; # Handle the case of an empty --file=
                       -w )  optw=true; writetofile=1;;
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
srcf=${arrposarg[i]}; i=$(( i + 1 ))
[[ -n $srcf ]] && source $srcf
#================================================================================
#                             Set Host Specific Vars
#================================================================================
[[ -z $DFMOD || -z $DFCST || -z $DBUILD || -z $DARCH || -z $DARCHRST ]] && \
    die "DFMOD, DFCST, DBUILD, DARCH, or DARCHRST is undefined."
[[ -z $ctag     ]] && die "ctag is undefined"
[[ -z $strexpid ]] && die "strexpid is undefined"

hstshort=$( get_host )
if [[ "$hstshort" == "pfe" ]];then 
    [[ -n $ctag && -n $DBUILD ]] && dbuild=$DBUILD || dbuild=

    hostarc=lfe

elif [[ "$hstshort" == "dis" ]];then 
    hostarc=dirac
    
    #export SQUEUE_FORMAT='%.10i %.9P %.16j %.8u %.8T %.10M %.9l %.6D %.5C'
fi

#todo:  keep inputcheck here. It needs host specific vars.
inputcheck

#================================================================================
#                                 Set Variables
#================================================================================
#mid
[[ -n $rstexpid ]] && strexpid=$strexpid$rstexpid
dmess=$cdir/message
ddata=$cdir/scripts/4.data/utility
dtmpdata=$cdir/data/submit
dout_pckwinners=$cdir/output/pckwinners/$strdout
dout_getstatus=$cdir/output/getstatus/$strdout
dstdout=$cdir/stdout/$strscr

ferr=$dmess/stderr_$strscr
flistlfe=wftmsz_flist
flst_lfe=$cdir/${strscr}_lfe_${strexpid}_$flistlfe

strfwin=winners_

#hstcron=pfe20
#fcron=~/crontab_gcmarch_pfe20
#fcronheader=~/crontabheader

[[ ! -d $dmess ]] && mkdir -p $dmess
#================================================================================
#                                  Main Process
#================================================================================
#letsbegin
$optf && arrdexp=($( misc_readfbyline $fdexp ))
$optn && get_fwftmsz_nosup $DARCHRST $flst_lfe


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#            Gcm_post Related
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
$optupdrunpstscr && update_runpstscr $userinput

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#            Archive Related
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#todo:  count and calculate # of output files that are archived if not completed yet
$opta && check_archstatus $userinput

#todo:  copy and edit updated gcmarch script to entered exp dir.
$optu && update_archscr $userinput

#todo:  counts running shiftc for run_gcmarch script
$opto && count_archshiftc

#todo:  find gcmarch.lock that has been existed more than Xhours
$optarchlock && arch_lock

#todo:  find shiftc for run_gcmarch.sh for days
$optsftcdays && sftc_days

#todo:  transfer output back from lfe to pfe
#note:  this option works only for MOM outputs
$optoutarc2exp && outarc2exp $userinput 

#todo:  remove extra outputs from exp ran more than 
#       3 months (Works only for non-winners)
$optrmextra && exp_rmextra $userinput

#todo:  run archive in screen sessions
#$optR && arc_screen ${arrdexp[@]}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#           Exp Setup Related
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
$optg && expsetup_check

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#            Restarts Related
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#todo:  count available rst files on $hostarc
$optn && count_lferst

#todo:  find missing restart files on pfe
$optm && count_pferst

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#             Submit Related
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#todo:  remove exp from data_submit_* files
$optr && clean_datasubmit 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#              SST Related
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#tood:  tar the 2nd full month sst outputs. work on ONLY the 2nd month
$optt && compress_sst

#todo:  check if all sst are ready for pckwinner
$optq && cnt_sst

#todo:  archive sst for the second full month to $hostarc
$optb && arch_sst

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#             Winner Related
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#todo:  create symlinks for winners on $hostarc 
$opts && create_winnersymlinks_lfe $usericyyyymm

#todo:  print winners between given YYYYMMs
$optp && print_winner_yyyymm

#todo:  resend email with pckwinners stdout and a plot for YYYYMM 
$optresendemail && resend_email

#todo:  re-archive sst and pckwinners output files.
$optarcwinners && arc_winners

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#           Run-time Related
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#todo:  getwtime
#note:  this function is transferred from run_getwtime.sh
if $optgetwtime;then 
    #note:  when opt w is selected, getwtime will show you outputs 
    #       written in fmessage. But sendmsg will delete fmessage
    #       if outputs is the same as the previous output.
    writetofile=0
    $optw && blshowout=true || blshowout=false
    getwtime 
fi

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#            Misc Operations
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#todo:  stop zombie shiftc operations
$opti && clean_shiftc

#todo:  calculate size of YYYYMM output files that stay on PFE 
$optl && calculate_outputsize

#todo:  calculate # of YYYYMM outputs files 
$optcalcnumout && calculate_numout

#todo:  output size analysis
$optk && analyzing_outputsize 



#%%%%% No Longer in Use (But Save them for now as of 20210927) (start) %%%%%
##todo:  add cron statement on pfe20
#$opta && add_cron ${arrdexp[@]}
#
##todo:  remove cron statement on pfe20
#$optm && rm_cron ${arrdexp[@]}

#todo:  check status of currently running archive
#$opta && check_arch

#todo:  check status of currently running archive
#$opta && _check_archstatus
#%%%%% No Longer in Use (But Save them for now as of 20210927) ( end ) %%%%%


exit

