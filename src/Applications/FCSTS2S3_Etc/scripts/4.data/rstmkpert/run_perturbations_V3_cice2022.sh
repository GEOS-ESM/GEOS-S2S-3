#!/usr/bin/env bash

inputcheck(){
    [[ -z $pertdate ]] && die "ic date is a required input"
    
    #todo:  check if pertdate exist
    if [[ -n $pertdate ]];then
        local bldate=$( check_dates $pertdate )
        ! $bldate && die "there is no such date as $pertdate"
    fi
    return
}

check_dates() {
    #description:   check if an input date exists
    local _date=$1
    local bl=true

    date -d $_date > /dev/null 2>&1
    (( $? > 0 )) && bl=false
    echo $bl    
}

debug_arrays(){

    arrdjf=($( echo ${_atmDJF[@]} ${atmDJF[@]} | tr ' ' '\n' | sort | uniq -u ))
    arrmam=($( echo ${_atmMAM[@]} ${atmMAM[@]} | tr ' ' '\n' | sort | uniq -u ))
    arrjja=($( echo ${_atmJJA[@]} ${atmJJA[@]} | tr ' ' '\n' | sort | uniq -u ))
    arrson=($( echo ${_atmSON[@]} ${atmSON[@]} | tr ' ' '\n' | sort | uniq -u ))

    (( ${#arrdjf[@]} > 0 )) && wmessage djf has diff
    (( ${#arrmam[@]} > 0 )) && wmessage mam has diff
    (( ${#arrjja[@]} > 0 )) && wmessage jja has diff
    (( ${#arrson[@]} > 0 )) && wmessage son has diff

    return
}

setvars_perturbations(){

        dumday=1
         dumdd=01
       dumyyyy=2015
       numdays=31
       maxloop=3
    
    #arratmens=($( seq 2 5 ))
    #arrocnens=($( seq 6 15 ))
    
    #abnote:    This is a list of calendar dates on which forecasts are run each year.
    #           These dates are the dates of the restarts, i.e. mmdd_21z  The forecast directory 
    #           is usually named with the next date at 00z, i.e. restart 1231_21z, forecast 0101_01z
    #ATM pertubation dates
    #_atmDJF=( 1201 1206 1211 1216 1221 1226 1231 0105 0110 0115 0120 0125 0130 0204 0209 0214 0219 0224 )
    #_atmMAM=( 0301 0306 0311 0316 0321 0326 0331 0405 0410 0415 0420 0425 0430 0505 0510 0515 0520 0525 0530 )
    #_atmJJA=( 0604 0609 0614 0619 0624 0629 0704 0709 0714 0719 0724 0729 0803 0808 0813 0818 0823 0828 )
    #_atmSON=( 0902 0907 0912 0917 0922 0927 1002 1007 1012 1017 1022 1027 1101 1106 1111 1116 1121 1126 )

    atmDJF=($( printf '%s\n' $( fcal_seasfcstdates djf ) | xargs -i date -d "$dumyyyy{} -1days" +%m%d ))
    atmMAM=($( printf '%s\n' $( fcal_seasfcstdates mam ) | xargs -i date -d "$dumyyyy{} -1days" +%m%d ))
    atmJJA=($( printf '%s\n' $( fcal_seasfcstdates jja ) | xargs -i date -d "$dumyyyy{} -1days" +%m%d ))
    atmSON=($( printf '%s\n' $( fcal_seasfcstdates son ) | xargs -i date -d "$dumyyyy{} -1days" +%m%d ))
    arratm=( ${atmDJF[@]} ${atmMAM[@]} ${atmJJA[@]} ${atmSON[@]} )

    #debug_arrays

    # OCN perturbation dates
    ocnDJF=( 1226 0130 0224 )
    ocnMAM=( 0326 0425 0530 )
    ocnJJA=( 0629 0729 0828 )
    ocnSON=( 0927 1027 1126 )
    arrocn=( ${ocnDJF[@]} ${ocnMAM[@]} ${ocnJJA[@]} ${ocnSON[@]} )
   
    arrNLMdays=( 31 28 31 30 31 30 31 31 30 31 30 31 )


     year=$( date -d $pertdate +%Y )
    iyear=$year
      mon=$( date -d $pertdate +%m  )
      imm=$( echo "$mon*1" | bc )

    mmdd=$( date -d $pertdate +%m%d )

    #todo:  determine if input date is for atm and/or perturbation
    if [[ -z $doATMperts ]];then
        [[ "${arratm[@]}" =~ "$mmdd" ]] && doATMperts=1 || doATMperts=0 
    fi
    if [[ -z $doOCNperts ]];then
        [[ "${arrocn[@]}" =~ "$mmdd" ]] && doOCNperts=1 || doOCNperts=0 
    fi
    
    season=$( fcal_month2seas $imm | tr '[:lower:]' '[:upper:]' )
    
    [[ "$season" == "DJF" ]] && imonth=12
    [[ "$season" == "MAM" ]] && imonth=3
    [[ "$season" == "JJA" ]] && imonth=6
    [[ "$season" == "SON" ]] && imonth=9

    numdays=${arrNLMdays[$(( imonth - 1 ))]}
        
    mm=$( printf '%02g\n' $imonth )

    (( $imm != 12 )) && [[ "$season" == "DJF" ]] && iyear=$(( iyear - 1 ))

    return
}

setvars_atm(){
    #description:   set necessary vars for Atm pert process

       datmperts=$datm/perts
        datmdata=$datm/data
            dens=$datmdata/ens200
    
          frand5=$datm/rand_5_pert${pertdate}.txt
        fatmseps=$datm/seps_facs.txt
     fatmalpha10=$datm/alpha10_rho_${season}_9ddiffs_V3.txt
        
        strmoist=moist_internal_rst
       strfvcore=fvcore_internal_rst
          fmoist=$drstextract/$strmoist.${pertdate}_2100z.nc4
         ffvcore=$drstextract/$strfvcore.${pertdate}_2100z.nc4
     fmoist_dens=$dens/$strmoist
    ffvcore_dens=$dens/$strfvcore
    
    return
}

setvars_ocn() {
    #description:   set necessary vars for ocn pert process

      docnrst=$drstextract/RESTART
    docnperts=$docn/perts
    
    #abnote:    cdata - Restarts that the perturbations will be centered at
    #           pdata - positively perturbed
    #           ndata - negatively perturbed
         dcdata=$docn/cdata
         dpdata=$docn/pdata 
         dndata=$docn/ndata 
    
        frand10=$docn/rand_10_pert${pertdate}.txt
       focnseps=$docn/seps_facs.txt
    focnalpha10=$docn/ocn_alpha10_${season}_9ddiffs_5levs_V3.txt
    
        focnexe=perturb_ogcm_scaleT.exe
      focnpanic=$docn/PANICSTOP_PT
          fcoef=coef.dat
        strsalt=ocean_temp_salt.res.nc
        strvelc=ocean_velocity.res.nc
          fsalt=$docnrst/${pertdate}_2100z.$strsalt
          fvelc=$docnrst/${pertdate}_2100z.$strvelc 

    return
}

atm_writemoredata() {
    #description:   write more data just for information only purpose.
    local _ftoread=$1;shift
    local nt

    wmessage
    wmessage "rdat, difr, & fact for $pertdate ( $_ftoread ):"
    for nt in {1..5};do
        local _datsepfac=$( cat $_ftoread | head -n $_nt | tail -n 1 )
        local _rdat=$( echo $_datsepfac | cut -c1-8 )
        local _difr=$( echo $_datsepfac | cut -d' ' -f2 )
        local _fact=$( echo $_datsepfac | cut -d' ' -f3 )

        wmessage "    $_rdat $_difr $_fact"

    done
    wmessage
    return
}

mkfseps() {
    #description:   create fseps file
    local _falpha=$1;shift
    local _fseps=$1

    #abnote:    The file with separations and corresponding weights (from the .../SANDBOX/ENSEMBLE/)
    #           This file must be pre-made (see README_V3_perts)
    local     Sep=($( cat $_falpha | head -1 | sed 's/,/ /g' | cut -d' ' -f2- ))
    local SeasFac=($( cat $_falpha | head -2 | tail -1 ))

    printf '%s\n' ${Sep[@]} ${SeasFac[@]} | pr -2t -s" " >> $_fseps
    return
}

mkdate() {
    #description:   Create random dates from the adjacent season and random separation.   
    local _fout=$1;shift
    local _atmorocn=$1
    local i 

    if [[ "$_atmorocn" == "atm" ]];then
        local _ind2beg=($( shuf -i1-${shuf_val} -n4 ))
        local _ind4seps=($( shuf -i1-9 -n4 ))
        local _seps=$fatmseps

    elif [[ "$_atmorocn" == "ocn" ]];then
        local _ind2beg=($( shuf -i1-${shuf_val} -n10 ))
        
        #abnote:    shuf does not allow repetitions, so need something different for ocn perts
        local _ind4seps=($( seq 10 | xargs -Iz  perl -e 'print int(rand() * 9 + 1) . "\n";' ))
        local _seps=$focnseps
    fi

    #todo:  Create proper date labels for the restart files to get these perturbations from.
    for i in ${!_ind2beg[@]};do
        local ind=${_ind2beg[i]}
        local pertbdates=$( date -d "$iyear$(printf '%02g' $imonth )$dumdd +${ind}days" +%Y%m%d )

        #todo:  check if pertbdates exists
        local bldexist=$( check_dates $pertbdates )
        ! $blexist && die "$pertbdates does not exist"

        local sepfac=$( cat $_seps | head -${_ind4seps[$i]} | tail -n 1 )
        echo $pertbdates $sepfac >> $_fout
    done
    wmessage
    return
}
check_atmfileavail(){
    local frand=$1
    local arrrd=($( cat $frand | cut -d' ' -f1 ))
    local arrpd=($( cat $frand | cut -d' ' -f1-2 | sed 's\ \+\g' | xargs -i bash -c 'date -d {}days "+%Y%m%d" ' ))
    local i 
    local bl=false

    for i in ${!arrrd[@]};do
        local rd=${arrrd[$i]}
        local pd=${arrpd[$i]}
        local file1=$drstextract/$strfvcore.${rd}_2100z.nc4
        local file2=$drstextract/$strfvcore.${pd}_2100z.nc4
        local file3=$drstextract/$strmoist.${rd}_2100z.nc4
        local file4=$drstextract/$strmoist.${pd}_2100z.nc4
            
        if [[ -f $file1 && -f $file2 && -f $file3 && -f $file4 ]];then
            bl=true
        else
            bl=false
            break
        fi
    done

    echo $bl
}

check_ocnfileavail(){
    local frand=$1
    local arrrd=($( cat $frand | cut -d' ' -f1 ))
    local arrpd=($( cat $frand | cut -d' ' -f1-2 | sed 's\ \+\g' | xargs -i bash -c 'date -d {}days "+%Y%m%d" ' ))
    local i 
    local bl=false

    for i in ${!arrrd[@]};do
        local rd=${arrrd[$i]}
        local pd=${arrpd[$i]}
        local file1=$docnrst/${rd}_2100z.$strsalt
        local file2=$docnrst/${rd}_2100z.$strvelc
        local file3=$docnrst/${pd}_2100z.$strsalt
        local file4=$docnrst/${pd}_2100z.$strvelc

        if [[ -f $file1 && -f $file2 && -f $file3 && -f $file4 ]];then
            bl=true
        else
            bl=false
            break
        fi
    done

    echo $bl
}


proc_fvcore() {
    local factor=$1 
    
    #U
    cdo -W --no_warnings --silent -L replace fort.13 -add -selname,U fort.13 -mulc,$factor -sub -selname,U fort.12 -selname,U fort.11 fort.1031
    #cdo --silent -L replace fort.13 -sub -selname,U fort.13 -mulc,$factor -sub -selname,U fort.12 -selname,U fort.11 fort.1031
    
    #V
    cdo -W --no_warnings --silent -L replace fort.1031 -add -selname,V fort.13 -mulc,$factor -sub -selname,V fort.12 -selname,V fort.11 fort.1131
    #cdo --silent -L replace fort.1031 -sub -selname,V fort.13 -mulc,$factor -mulc,1.5 -sub -selname,V fort.12 -selname,V fort.11 fort.1131
    
    #PT
    cdo -W --no_warnings --silent -L replace fort.1131 -add -selname,PT fort.13 -mulc,$factor -sub -selname,PT fort.12 -selname,PT fort.11 fort.31
    #cdo --silent -L replace fort.1131 -sub -selname,PT fort.13 -mulc,$factor -sub -selname,PT fort.12 -selname,PT fort.11 fort.31
    
    [[ -f fort.1031 ]] && rm -f fort.1031
    [[ -f fort.1131 ]] && rm -f fort.1131

    return
}

proc_moist() {
    local factor=$1

    #Q
    cdo -W --no_warnings --silent -L replace fort.23 -add -selname,Q fort.23 -mulc,$factor -sub -selname,Q fort.22 -selname,Q fort.21 fort.41
    #cdo --silent -L replace fort.23 -sub -selname,Q fort.23 -mulc,$factor -sub -selname,Q fort.22 -selname,Q fort.21 fort.41
    return
}

atm_setupcontrol() {

    #abnote:    location of experimental perturbations 
    #           populate data directory (when all V# restarts are in one place)
     [[ ! -f $fmoist_dens && -f $fmoist ]]  && ln -s  $fmoist $fmoist_dens
    [[ ! -f $ffvcore_dens && -f $ffvcore ]] && ln -s $ffvcore $ffvcore_dens
    return  
}

atm_mkpert(){
    #description:   create atm perturbation files
    local i blrd blpd

    #abnote:    These files will be perturbed, i.e. added and subtracted from
    cp -p $ffvcore_dens fort.13
    cp -p  $fmoist_dens fort.23
    
    msg_wheader_userdefined 60 "#" "Create Atm Perturbation - $pertdate"    
    wmessage "rand_5_pert*.txt : $( basename $frand5 )"
    wmessage "      moist file : $(basename $fmoist) "
    wmessage "     fvcore file : $(basename $fmoist) "
    wmessage "      Atm sepfac :"
    wmessage "$( sed "s/^/                   /" $fatmseps )"
    wmessage

    for i in ${!arratmens[@]};do 
    
        local ens=ens${arratmens[i]}
        local nt=$(( i + 1 ))
        
        local pert_dir=$datmdata/$ens

          [[ -d $pert_dir ]] && rm -rf $pert_dir
        [[ ! -d $pert_dir ]] && mkdir -p $pert_dir
    
        #abnote:    These files must be premade: separation in days and scaling factors that match
        #datsepfac=$( cat ../files_${ananame}/rand_5_pert${pertdate}.txt | head -$nt | tail -1 )
        local datsepfac=$( cat $frand5 | head -$nt | tail -1 )
    
        local     rd=$( echo $datsepfac | cut -c1-8 )
        local   difr=$( echo $datsepfac | cut -d' ' -f2 )
        local   fact=$( echo $datsepfac | cut -d' ' -f3 )
        local     pd=$( date -d "$rd +${difr}days" +%Y%m%d )

        local nfile1=$drstextract/$strfvcore.${rd}_2100z.nc4
        local pfile1=$drstextract/$strfvcore.${pd}_2100z.nc4
        local nfile2=$drstextract/$strmoist.${rd}_2100z.nc4
        local pfile2=$drstextract/$strmoist.${pd}_2100z.nc4
    
        #todo:  check if pertbdates exists
        local blrd=$( check_dates $rd )
        local blpd=$( check_dates $pd )
            
        [[ ! $blrd || ! $blpd ]] && die "one or both dates, $rd & $pd, does not exist"
        
        wmessage "    difr fact : $difr $fact"
        wmessage "           rd : $rd"
        wmessage "           pd : $pd"
        wmessage "    rd  moist : $( basename $nfile2 )"
        wmessage "    rd fvcore : $( basename $nfile1 )"
        wmessage "    pd  moist : $( basename $pfile2 )"
        wmessage "    pd fvcore : $( basename $pfile1 )"
        wmessage "output  moist : $ens/$strmoist"
        wmessage "output fvcore : $ens/$strfvcore" 
        wmessage
    
        if [[ -f $pfile2 && -f $nfile2 && -f $pfile1 && -f $nfile1 ]];then
            [[ -f $pfile2 ]] && cp -p $pfile2 fort.22
            [[ -f $nfile2 ]] && cp -p $nfile2 fort.21
            [[ -f $pfile1 ]] && cp -p $pfile1 fort.12
            [[ -f $nfile1 ]] && cp -p $nfile1 fort.11
        else
            wmessage "one or more following files do not exist:"
            wmessage "    $( basename $pfile2 )"
            wmessage "    $( basename $nfile2 )"
            wmessage "    $( basename $pfile1 )"
            wmessage "    $( basename $nfile1 )"
            die ""
        fi
    
        proc_moist $fact
        proc_fvcore $fact
    
        if [[ -f fort.41 ]];then
            mv fort.41 $pert_dir/$strmoist
        else
            die fort.41 does not exist
        fi
        
        if [[ -f fort.31 ]];then 
            mv fort.31 $pert_dir/$strfvcore
        else
            die fort.31 does not exist
        fi
    
        [[ -f fort.11 ]] && rm -f fort.11
        [[ -f fort.12 ]] && rm -f fort.12
        [[ -f fort.21 ]] && rm -f fort.21
        [[ -f fort.22 ]] && rm -f fort.22
      
    done
    
    return
}

ocn_setupcontrol(){
    #abnote:    These files will be perturbed, i.e. added and subtracted from
    ln -f $fsalt $dcdata/a$strsalt
    ln -f $fvelc $dcdata/a$strvelc

    return
}

ocn_mkpert() {
    #description:   create ocean preturbation.
    local i

    msg_wheader_userdefined 60 "#" "Create Ocn Perturbation - $pertdate"    
    wmessage "Positively Purturbed Files : $strpdata"
    wmessage "         rand_10_pert*.txt : $( basename $frand10 )"
    wmessage "                 salt file : $( basename $fsalt )" 
    wmessage "             velocity file : $( basename $fvelc )"
    wmessage "                Ocn sepfac :"
    wmessage "$( sed "s/^/                            /" $focnseps )"
    wmessage

    #for nt in ${arrocnens[@]};do
    for i in ${!arrocnens[@]};do 
        local ens=ens${arrocnens[i]}
        local nt=$(( i + 1 ))
        
#wmessage "@$LINENO $nt"

        local datsepfac=$( cat $frand10 | head -n $nt | tail -n 1 )
        #local fpert_stdout=$docn/perturb$( printf '%02g' $ens ).out
        local fpert_stdout=$docn/perturb_$ens.out

        local   rd=$( echo $datsepfac | cut -c1-8 )
        local difr=$( echo $datsepfac | cut -d' ' -f2 ) 
        local fact=$( echo $datsepfac | cut -d' ' -f3 )
        local   pd=$( date -d "$rd +${difr}days" +%Y%m%d )
    
        local fsalt_rd=$docnrst/${rd}_2100z.$strsalt
        local fvelc_rd=$docnrst/${rd}_2100z.$strvelc
        local fsalt_pd=$docnrst/${pd}_2100z.$strsalt
        local fvelc_pd=$docnrst/${pd}_2100z.$strvelc
        
        wmessage "      difr fact : $difr $fact"
        wmessage "             rd : $rd"
        wmessage "             pd : $pd"
        wmessage "    rd     salt : $( basename $fsalt_rd )"
        wmessage "    rd velocity : $( basename $fvelc_rd )"
        wmessage "    pd     salt : $( basename $fsalt_pd )"    
        wmessage "    pd velocity : $( basename $fvelc_pd )"
        wmessage "output     salt : $ens/RESTART/$strsalt"
        wmessage "output velocity : $ens/RESTART/$strvelc"
        wmessage
    
        #todo:  copy necessary files into ndata & pdata dir.
        #abnote:  save current analysis docnrst in ndata as templates
        cp -L $fsalt $dndata/$strsalt
        cp -L $fvelc $dndata/$strvelc
    
        #abnote:   save current analysis docnrst in pdata as templates
        cp -L $fsalt $dpdata/$strsalt
        cp -L $fvelc $dpdata/$strvelc
    
        if [[ -f $dndata/$strsalt && -f $dndata/$strvelc && -f $dpdata/$strsalt && -f $dpdata/$strvelc ]];then
            :
        else
            wmessage "one or more following files do not exist:"
            wmessage "    $dndata/$strsalt"
            wmessage "    $dndata/$strvelc"
            wmessage "    $dpdata/$strsalt"
            wmessage "    $dpdata/$strvelc"
            die 
        fi
    
        #todo:  check if files exist
        if [[ -f $fsalt_rd && -f $fvelc_rd && -f $fsalt_pd && -f $fvelc_pd ]]; then
            #abtodo:    Get all restarts needed to ndata, pdata (ndata&pdata will be updated)
            #abnote:    link first sample of docnrst in ndata
            ln -sf  $fsalt_rd $dndata/u$strsalt
            ln -sf  $fvelc_rd $dndata/u$strvelc
            
            #abnote:    link second sample of docnrst in pdata
            ln -sf  $fsalt_pd $dpdata/u$strsalt
            ln -sf  $fvelc_pd $dpdata/u$strvelc
        else
            wmessage "one or more following files do not exist:"
            wmessage "    $fsalt_rd"
            wmessage "    $fvelc_rd"
            wmessage "    $fsalt_pd"
            wmessage "    $fvelc_pd"
            die ""
        fi
        
        #todo:  create coef.dat file
        echo $fact > $fcoef
    
        #todo:  execute fortran code
        msg_newfile $fpert_stdout
        
        echo "perturb_ogcm : $ens - $( cat $fcoef )" >> $fpert_stdout 2>&1
        echo >> $fpert_stdout
    
        if [[ -f $focnexe ]]; then
            ./$focnexe >> $fpert_stdout 2>&1
            pertstat=$?
        else
            die $focnexe does not exist
        fi
    
        #todo:  check status and exist if error
        local PTR=$( grep GENERATED $fpert_stdout | wc -l )
        local nnn=$( grep ErrVAR $fpert_stdout | wc -l )

        (( $PTR != 2 )) && local pertstat=99
    
        if (( $pertstat != 0 )) || [[ ! -f $fpert_stdout ]] ; then
            wmessage pertstat = $pertstat
            touch $focnpanic
            die 
        fi
    
        if (( $nnn > 0 )); then
            touch $focnpanic
            die "ERROR - ErrVAR RANGE ERROR"
        fi
    
        #todo:  mv pert files to final dest & clean them up by deleting unncessary vars
        if [[ ! -f $focnpanic ]]; then
   
            docnens=$dpdata/$ens/RESTART
            fhand_newdir $docnens

            mv $dpdata/$strsalt $docnens/
            mv $dpdata/$strvelc $docnens/
    
            #this is needed for NAS runs; checksum craches the model, needs to be removed from the original analysis restarts
            ncatted -h -O -a checksum,"v",d,, $docnens/$strvelc
            ncatted -h -O -a checksum,"u",d,, $docnens/$strvelc
            ncatted -h -O -a checksum,"temp",d,, $docnens/$strsalt
            ncatted -h -O -a checksum,"salt",d,, $docnens/$strsalt
    
            #todo:  delete ndata dir 
            #note:  02/09/2021 - Based on chats wit Anna, we are going to use pdata
            #       for upcoming hindcast v3 runs.
            [[ -d $dndata ]] && rm -f $dndata/*
    
        fi
    
        wmessage
    
    done
    return
}

#================================================================================
#                              Beginning of Script
#================================================================================
#beg
strscr=$( basename "$0" | cut -d'_' -f2 )

#futil=@FUTIL
futil=/discover/nobackup/projects/gmao/t2ssp/GEOSS2S3/GEOS_util
source $futil/func_fcst.sh

writetofile=0
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        create perturbation rst files. This is a child script of mkpert.sh 
        
        Usage: ./$(basename "$0") [-bhc] srcme_file YYYYMMDD

        Input:
            A source file, which set various vars for a set of runs (i.e. srcme_dis_*)
            Fcst date in YYYYMMDD format (not ic date!)

        options:
            -a  run atm perturbation. 1 to run ( default = 0 )
            -o  run ocn perturbation. 1 to run ( default = 0 )
            -b  run with a debug mode (this will not execute ${strscr}.sh)
            -h  show this help text
            -c  remove unnecessary files
"

OPTIND=1
while getopts ':bhca:o:' option; do
    case "$option" in
        a)  doATMperts=$OPTARG;; 
        o)  doOCNperts=$OPTARG;; 
        c)  clean_dir;exit;;
        b)  optb=true;rundebug=1;; 
        h)  echo "$usage"; exit 0;;
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

[[ -z ${arrposarg[@]} ]] && die "input is missing"
(( ${#arrposarg[@]} != 2 )) && die "2 inputs are required"
#================================================================================
#                               Check User Inputs
#================================================================================
pertdate=${arrposarg[1]}

inputcheck
source ${arrposarg[0]}

#================================================================================
#                   Set Host Specific Variables & Load Modules
#================================================================================
#todo: check the name of system (discover or pfe) and set variables.
hst=$(hostname)
if [[ $hst == 'pfe'* ]] || [[ $hst == 'discover'* ]];then 
    if [[ $hst == 'pfe'* ]];then
        die "this script works only on discover"
        
        . /usr/share/modules/init/bash
        module load nco
        
        status_cdo=$? 
        (( $status_cdo > 0 )) && module load cdo

    elif [[ $hst == 'discover'* ]];then 
        . /usr/share/modules/init/bash
        module load nco
        
        status_cdo=$? 
        (( $status_cdo > 0 )) && module load cdo
        umask 0022
    fi
else
    die "hostname is unknown"
fi
#================================================================================
#                         Set & Export Environment Vars
#================================================================================
runtest=false
if $runtest;then
    drstextract=/discover/nobackup/projects/gmao/t2ssp/GEOSS2S3/GEOS_util/output/rstextr/2015_son
    thisdir=/discover/nobackup/projects/gmao/t2ssp/GEOSS2S3/GEOS_util/output/mkpert/scratch/$pertdate
else    
    #  thisdir=/gpfsm/dnb02/projects/p58/aogcm/g5fcst/s2sv3/
    thisdir=@thisdir
    drstextract=@drstextract
fi
#================================================================================
#                                 Set Variables
#================================================================================
#mid
thisdir=@thisdir
drstextract=@drstextract
datm=$thisdir/AtmPertsV3
docn=$thisdir/OcnPertsV3

[[ -z $shuf_val ]] && shuf_val=60
dstdout=$futil/stdout/rstmkpert/$strexpid

setvars_perturbations

#todo:  exit when input dates are not for restarts.
(( ! $doATMperts && ! $doOCNperts )) && exit

#todo:  set atm vars and exit if necessary rsts do not exist
if (( $doATMperts ));then
    setvars_atm

    [[ ! -f $fmoist || ! -f $ffvcore ]] && exit

    fhand_newdir $dens
    fhand_newdir $datmperts
fi

#todo:  set ocn vars and exit if necessary rsts do not exist
if (( $doOCNperts ));then
    #06/28/2022 -   These dates failed to create perturbation even after several attempts.
    #               Anna B. provided different ocn_alpha10* files and these dates is set to
    #               use those file
    #arrpertdate_exception=( 19960629 19990425 ) 
    #arrpertdate_exception=( 19860629 ) 

    setvars_ocn

    #todo:  exit
    [[ ! -f $fsalt || ! -f $fvelc ]] && exit

    fhand_newdir $dcdata
    fhand_newdir $dpdata
    fhand_newdir $dndata
    
    [[ -f $focnpanic ]] && rm -f $focnpanic
   
    #todo:  set string & use it for writing stdout 
    numchar_start=$(( ${#thisdir} + 2 ))
    strpdata=$( echo $dpdata | cut -c${numchar_start}- )
fi
#================================================================================
#                                  Main Process
#================================================================================
#main
msg_wheader
wmessage "  Processing Directory : $thisdir"
wmessage "Extracted Rst Location : $drstextract"
wmessage "       User Input Date : $pertdate"
wmessage "                Season : $season"
wmessage "            doATMperts : $doATMperts"
wmessage "            doOCNperts : $doOCNperts"
wmessage "               numdays : $numdays"
wmessage "              shuf_val : $shuf_val"
(( $doOCNperts )) && wmessage " ocn_alpha10 File Name : $( basename $focnalpha10 )"
wmessage 

#todo:  do atm perturbations
if (( $doATMperts ));then

    #+++++ cd to AtmPertsV3 (start) +++++
    cd $datm

    #todo:  read alpha* file and create fatmseps file.
    msg_newfile $fatmseps
    mkfseps $fatmalpha10 $fatmseps

    blfavail=false;cntloop=0
    while ! $blfavail && (( $cntloop < 3 ));do
        #todo:  make dates
        msg_newfile $frand5
        mkdate $frand5 atm

        blfavail=$( check_atmfileavail $frand5 )
    done

    #note:  this part doesn't seem to be too important.
    runwritemoredata=false
    if $runwritemoredata;then
        #todo:  Test the text file with the dates, separations and weights and move it to the files_$ananame directory
        #       This "while" loop can be deleted, if everything looks ok.
        atm_writemoredata $frand5 
    fi

    #note:  code from setup_LB_V3_perts_from_file.sh starts here
    #todo:  creaet dir with control 
    atm_setupcontrol
    [[ ! -f $fmoist_dens || ! -f $ffvcore_dens ]] && exit 

    cd - >/dev/null
    #+++++ cd to AtmPertsV3 ( end ) +++++


    #todo:  start making atm perturbation 
    #+++++ cd AtmPertsV3/perts (start) +++++
    cd $datmperts
    atm_mkpert
    cd - >/dev/null
    #+++++ cd AtmPertsV3/perts ( end ) +++++

    #todo:  clean
    [[ -d $datmperts ]] && rm -rf $datmperts
    [[ -d $fatmseps ]] && rm -rf $fatmseps
    
fi

#todo:  do ocn perturbations
if (( $doOCNperts ));then

    #+++++ cd OcnPertsV3 (start) +++++
    cd $docn
    
    #todo:  read alpha* file and create fatmseps file.
    msg_newfile $focnseps
    mkfseps $focnalpha10 $focnseps

    blfavail=false;cntloop=0
    while ! $blfavail && (( $cntloop < 3 ));do
        #todo:  make dates
        msg_newfile $frand10
        mkdate $frand10 ocn

        blfavail=$( check_ocnfileavail $frand10 )
        cntloop=$(( cntloop + 1 ))
    done


    #note;   code from setup_ocn_taper_perts_V3_from_file.csh starts here 
    #todo:  creaet dir with control 
    ocn_setupcontrol
    [[ ! -f  $dcdata/a$strsalt || ! -f  $dcdata/a$strvelc ]] && exit

    ocn_mkpert

    cd - >/dev/null
    #+++++ cd OcnPertsV3 ( end ) +++++

    #todo:  clean dir & files.     
    [[ -d $dndata ]] && rm -rf $dndata
    [[ -d $dcdata ]] && rm -rf $dcdata

fi
