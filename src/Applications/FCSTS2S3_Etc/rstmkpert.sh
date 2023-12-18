#!/usr/bin/env bash

inputcheck(){
    [[ -z $icyyyymmdd ]] && die "ic date is a required input"
}

setvars_mkpert(){
    local x arr3

    [[ -n $rstexpid ]] && strexpid=$strexpid$rstexpid

    stratm=AtmPertsV3
    strocn=OcnPertsV3
    
       dscr=scripts/4.data/$strscr
       dout=output/$strscr/$strdout
    dstdout=stdout/$strscr/$strdout
    #   dout=output/$strscr/$strexpid
    #dstdout=stdout/$strscr/$strexpid
    
    douticyyyymmdd=$dout/$icyyyymmdd
    dscratch=$dout/scratch/$icyyyymmdd
    
    datm=$dscratch/$stratm
    docn=$dscratch/$strocn
    
    datmout=$datm/data
    docnout=$docn/pdata
    
       strrand=rand_
       strseps=seps_
       strpert=perturb

    if [[ -z $fmessage ]];then
        if $opto;then
            fmessage=$cdir/message/message_${strscr}_${strexpid}_${icyyyymmdd}_ocnens6to10
        else
            fmessage=$cdir/message/message_${strscr}_${strexpid}_$icyyyymmdd
        fi
    fi

    if $opto;then
        strmsgpert=message_${strscr}_${strexpid}_${icyyyymmdd}_pertv3_ocnens6to10
        ftar=${icyyyymmdd}_pert_ocnens6to10.tar
    else    
        strmsgpert=message_${strscr}_${strexpid}_${icyyyymmdd}_pertv3
        ftar=${icyyyymmdd}_pert.tar
    fi

    fm_pert=$cdir/message/$strmsgpert
    
    arrfstr_atm=( fvcore_internal_ moist_internal_ )
    arrfstr_ocn=( _temp_salt.res.nc _velocity.res.nc )
    numfextracted_each=$( printf '%s\n' ${arrfstr_atm[@]} ${arrfstr_ocn[@]} | wc -l )
    
    yyyy=$( date -d $icyyyymmdd +%Y )
    mmdd=$( date -d $icyyyymmdd +%m%d )
      mm=$( date -d $icyyyymmdd +%m )
      dd=$( date -d $icyyyymmdd +%d )
     imm=$( echo "$mm*1" | bc )

     #note:  these are used in run_perturbations_V3.csh
    if [[ "$strexpid" == "cice" ]] && (( $yyyy == 2022 ));then
        fscrpert=run_perturbations_V3_cice2022.sh

    elif [[ "$strexpid" == "cice" ]] && (( $yyyy == 2023 ));then
        fscrpert=run_perturbations_V3_cice2022.sh

    elif [[ "$strexpid" == "test" ]];then
        fscrpert=run_perturbations_V3_test.sh
        
    elif $opto;then
        fscrpert=run_perturbations_V3_ocn6to10.sh
    else
        fscrpert=run_perturbations_V3.sh
    fi

    #todo:  yyyyseas is used to determine 
    if (( $imm == 12 ));then  
        #note:  in fcal_seasdates, one year is substructed from user input year for djf seasons
        #       Thus, you have to add if mm == 12
        yyyyseas=$(( yyyy + 1 ))
    else
        yyyyseas=$yyyy
    fi

    imonth=$( echo "$( date -d $icyyyymmdd +%m )*1" | bc )
    seas=$( fcal_month2seas $imonth )

    dextract=$cdir/output/rstextr/$strdout/${yyyyseas}_$seas

    [[ "$strexpid" == "test" ]] && dextract_yyyy=$cdir/output/rstextr/$strdout/${yyyyseas}

    return
}

setup_dscratch() {
    #description:   setup scratch/icyyyymmdd dir.
    local fsed=sedfile

    #todo:  cp necessary scripts. 
    cp -pr $dscr/* $dscratch/
    
    #todo:  copy 'correct' *_perturb_ogcm_scaleT.f90 and rename it to perturb_ogcm_scaleT.f90
    cp -fp $docn/$thisfscalet $docn/$fscalet

    #todo:  replace some vars 
    #+++++ cd dscratch (start) +++++
    cd $dscratch

    [[ -f $fsed ]] && rm -f $fsed

    if [[ -f $fscrpert ]];then
        
        if [[ "$strexpid" == "test" ]];then 
            local _dextract=$dextract_yyyy
        else
            local _dextract=$dextract
        fi

        #s?@ANANAME?$ananame?g
cat > $fsed << EOF
s?@thisdir?$cdir/$dscratch?g
s?@drstextract?$_dextract?g
EOF
        sed -i -f $fsed $fscrpert
        [[ -f $fsed ]] && rm -f $fsed
    fi

    cd - >/dev/null
    #+++++ cd dscratch (start) +++++


    #todo:  compile perturb_ogcm_scaleT 
    #+++++ cd docn (start) +++++
    cd $docn
    
    make -s
    
    if [[ ! -f odas_resolution.mod || ! -f odas_types.mod  || ! -f my_stats.mod || ! -f perturb_ogcm_scaleT.exe ]];then
        die "compiling scaleT executable FAILED. exit"
    fi
    
    cd - >/dev/null
    #+++++ cd docn ( end ) +++++
    return
}

mv2dfinal() {

    #description:   move outputs to a final destination
    if [[ -d $datmout ]];then 
        cpout $datmout "${arratmens[@]}"
    fi
    
    if [[ -d $docnout ]] && $blocean;then
        cpout $docnout "${arrocnens[@]}"
    fi
    return 
}

cpout() {
    #description:   mv atm pert files to output dir.
    local _dout=$1;shift
    local _arrens=( "$@" )
    local ens numffinal 

    wmessage move files:

    for ens in ${_arrens[@]};do
        numffinal=$( find $_dout/ens$ens/* -type f 2>/dev/null | wc -l )

        if [[ -d $_dout/ens$ens && -d $douticyyyymmdd ]] && (( $numffinal == 2 ));then
            cp -rp $_dout/ens$ens $douticyyyymmdd/
            wmessage "    $_dout/ens$ens => $douticyyyymmdd/ens$ens"
        else
            wmessage "    FAILED: Pert files are missing from $_dout/ens$ens"
        fi
    done

    wmessage
    return 
}


cp_text() {
    #description:   copy various data and stdout intp fmessage
    local _arrd=( $datm $docn )
    local fatm _dir fdata

    #Notes:  stdout from perturbation script & file_* in each Atm & Ocn dir 
    #        are copied into fmessage file. It is saved in final tar files
    #        and stdout/mkpert dir.

    #todo:  write frand for atm
    if [[ -d $datmout ]];then
        local numfrand=$( find $datm/* -maxdepth 0 -type f -name "$strrand*" 2>/dev/null | wc -l )
        
        if (( $numfrand == 1 ));then
            local frand=$( find $datm/* -maxdepth 0 -type f -name "$strrand*" 2>/dev/null ) 
            msg_wheader_userdefined 60 "@" "$( echo $datm | rev | cut -d'/' -f1 | rev ) - $( basename $frand )"
            cat $frand >> $fmessage
            wmessage
        fi
    fi

    #todo:  write frand for ocn 
    if [[ -d $docnout ]] && $blocean;then
        local numfrand=$( find $docn/* -maxdepth 0 -type f -name "$strrand*" 2>/dev/null | wc -l )
        
        if (( $numfrand == 1 ));then
            local frand=$( find $docn/* -maxdepth 0 -type f -name "$strrand*" 2>/dev/null )
            msg_wheader_userdefined 60 "@" "$( echo $docn | rev | cut -d'/' -f1 | rev ) - $( basename $frand )"
            cat $frand >> $fmessage
            wmessage
        fi
    fi

    wmessage 
    #todo:  copy stdout from perturbation script
    local arrfattempt=($( find $cdir/message/* -type f -name "*${icyyyymmdd}_*_attempt*" 2>/dev/null | sort -V ))
    for fattempt in ${arrfattempt[@]};do
        msg_wheader_userdefined 80 "@" "$fscrpert: $( basename $fattempt )"
        cat $fattempt >> $fmessage
        (( $? == 0 )) && rm -f $fattempt
        wmessage
    done

    #todo:  copy ocean perturbation stdout
    if [[ -d $docnout ]] && $blocean;then
        local arrfperturb=($( find $docn/* -type f -name "$strpert*.out" 2>/dev/null | sort -V ))
        for fperturb in ${arrfperturb[@]};do
            msg_wheader_userdefined 60 "@" "$fperturb"
            cat $fperturb >> $fmessage
            (( $? == 0 )) && rm -f $fperturb
            wmessage
        done
    fi

    #todo:  send email
    if [[ -f $fmessage ]];then
        sizef=$( stat --print='%s' $fmessage )
        if (( $sizef > 0 ));then
            msg_cyberpostman "$msg_subject" "$eadds" $fmessage
            (( $? > 0 )) && wmessage "\@$LINENO Email FAILED"
        fi
    fi


    cp -p $fmessage $douticyyyymmdd/
    mv $fmessage $dstdout/ 

    return
}

delete_stuff() {
    [[ -f $fmessage ]] && rm -f $fmessage
    [[ -d $douticyyyymmdd ]] && rm -rf  $douticyyyymmdd
         [[ -d  $dscratch ]] && rm -rf $dscratch
    find $cdir/message/* -type f -name "*$strmsgpert*" -delete 2>&1
    return
}

clean_mylock() {
    [[ -n $thislock && -f $thislock ]] && rm -f $thislock
    return
}

clean_dir() {
    [[ -n $flock && -f $flock ]] && rm -f $flock
    return
}
#================================================================================
#                                     Begin
#================================================================================
#beg
hst=`hostname`
if [[ "$hst" =~ "dis" ]];then
    cdir=/discover/nobackup/projects/gmao/t2ssp/GEOSS2S3/GEOS_util
else
    echo "this script works only on discover";exit
    #cdir=/nobackupp11/gmaofcst/GEOSS2S3/GEOS_util
fi

[[ -z $cdir || ! -d $cdir ]] &&  echo "cdir is undefined" &&exit

cd $cdir

[[ -z $strscr ]] && strscr=$(basename "$0" | cut -d'.' -f1 )

#todo:  check if this script is invoked as child script
[[ -n $RUNCHILD ]] && blchild=true || blchild=false

if ! $blchild;then

    flock=$cdir/${strscr}.lock
    ftmp=$cdir/stdout/rundatetime/tmp_$strscr
    
    if [[ ! -f $ftmp ]];then install -D /dev/null $ftmp;fi
    
    #todo:  check tmp file size and create new if it is larger than 5kb
    stmp=$( find $ftmp -printf "%s\n" )
    (( $stmp > 5000 )) && rm -f $ftmp && touch $ftmp
    
    #todo:  lock this script
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
     
    trap clean_dir EXIT
fi

if [[ ! $cdir/func_fcst.sh ]];then 
    die "$cdir/func_fcst.sh does not exist"
else
    source $cdir/func_fcst.sh
fi

freadme=$cdir/scripts/1.org/aborovik/perts/README_V3_perts

blrerun=false
writetofile=0
opto=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename $0) -- this is a program to:
        create purturbed ic files for s2s forecasts.

        Usage: ./$(basename "$0") [-chwr] srcme_file [YYYYMMDD]

        Input:
            A source file, which set various vars for a set of runs (i.e. srcme_dis_*)
            Initial condition date in YYYYMMDD format.

        options:
            -r  re-do processes for given initial condition date 
            -o  re-do ocean perturbation for ens6 to ens10 (see note below)
            -m  fmessage file name
            -h  show this help text
            -n  show aborovik's Readme file
            -w  write stdout/err in a file
        
        Note 1: stdout from perturbation script & file_* in each Atm & Ocn dir 
                are fopied into fmessage file. It is saved in final tar files
                and stdout/mkpert dir.

        Note 2: 06/21/2022  There was a bug in ocean perturbation and need to be re-processed
                            for ens6 to ens10. Original ocn pert for ens6 and ens10 were for ens11
                            to 15. Thus, those exp dir is renamed appropriately.
"

OPTIND=1
while getopts 'hnf:rwom:' option; do
    case "$option" in
        r)  blrerun=true;;
        m)  fmessage=$OPTARG;;
        o)  opto=true;; 
        h)  echo "$usage"; exit 0;;
        n)  msg_wheader_userdefined 80 = "aborovik's $( basename $freadme )"; cat $freadme; exit 0;;
        w)  writetofile=1;; 
        \?) echo "Invalid option: -$OPTARG" >&2
            exit 1;;
        :)  echo "Option -$OPTARG requires an argument." >&2
            exit 1;;
    esac
done

#todo:  get positional inputs.
shift $((OPTIND-1))
[[ "${1:-}" = "--" ]] && shift
arrposarg=( "$@" )

[[ -z  ${arrposarg[@]} ]] && die "input is missing"
(( ${#arrposarg[@]} != 2 )) && die "2 inputs are required"
#================================================================================
#                               Check User Inputs
#================================================================================
icyyyymmdd=${arrposarg[1]}
inputcheck

srcf=$( realpath ${arrposarg[0]} )
source $srcf
#================================================================================
#                                Create Lock File
#================================================================================
if $blchild;then
    thislock=$cdir/${strscr}_$icyyyymmdd.lock
    if set -C; 2>/dev/null > $thislock;then
        :
    else
        exit
    fi
    trap clean_mylock EXIT
fi

#================================================================================
#                             Set Host Specific Vars
#================================================================================
if [[ $hst == 'pfe'* ]];then
    :
elif [[ $hst == 'discover'* ]];then 

    hstshort=$( get_host) 

    export MODULESHOME=/usr/share/lmod/lmod
    modulepath=/discover/swdev/gmao_SIteam/modulefiles-SLES12
    modulepath=/usr/local/other/modulefiles/Core:$modulepath
    modulepath=/usr/local/share/modulefiles/Core:$modulepath
    modulepath=/usr/share/lmod/lmod/modulefiles/Core:$modulepath
    modulepath=/usr/share/lmod/modulefiles:$modulepath
    export MODULEPATH=$modulepath
    umask 0022

    . /usr/share/modules/init/bash
    module load comp/intel/19.1.3.304
fi

#================================================================================
#                                 Set Variables
#================================================================================
#mid
setvars_mkpert
maxattempt=5
attempt=0
blsuccess=false

#todo:  exit if dextract does not exist, or ftar exists.
[[ ! -d $dextract ]] && exit


#note:  if this is re-run, ftar output will be deleted.
$blrerun && rm -f $dout/$ftar

[[ -f $dout/$ftar ]] && exit

fhand_newdir $douticyyyymmdd 
fhand_newdir $dscratch

msg_subject="${hstshort}.${strscr}: $strexpid $icyyyymmdd"
feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

bleaddextra=true
feaddextra=$cdir/mailadd_extra
if $bleaddextra && [[ -f $feaddextra ]];then 
    eaddsextra=$( echo $( misc_readfbyline $feaddextra ) | sed -e "s/ /;/g" )
    eadds="$eadds;$eaddsextra"
fi

[[ ! -d $dstdout ]] && mkdir -p $dstdout

#todo:  determine if icyyyymmdd is the date which supposed to have ocean pert. 
arr1=($( fcal_all ))
arr2=($( printf "$yyyy%s\n" ${arr1[@]} ))

for x in ${arr2[@]};do 
    bl=$( fcal_calclasticdateofthemonth $x )
    $bl && arr3+=($( date -d "$x -1days" +%m%d))
done

if [[ "${arr3[@]}" =~ "$mmdd" ]]; then 
    blocean=true
else
    blocean=false
fi

    
fscalet=perturb_ogcm_scaleT.f90

if [[ "$strexpid" == "fcst" ]];then
    #06/28/2022 -   These dates failed to create perturbation even after several attempts.
    #               Anna B. provided different ocn_alpha10* files and these dates is set to
    #               use those file
    #07/21/2022 -   It turns out there is minimum value set as a limit in perturb_ogcm_scaleT.f90
    #               that is preventing perturbation script to create pert files. 
    #               For these dates below, a different perturb_ogcm_scaleT.f90 will be used
    #08/09/2022 -   v3 hindcast has been halted and will start from 1992. Keep this code for now
    #               but comment this out
    #arrpertdate_exception=( 19960629 19990425 19930729 20020530 ) 
    arrpertdate_exception=( 19860629 )
    
    int=$( IndexOf $icyyyymmdd ${arrpertdate_exception[@]} )
    if (( $int > 0 ));then
        thisfscalet=20220718_perturb_ogcm_scaleT.f90
    elif (( $int == 0 ));then
        thisfscalet=20210107_perturb_ogcm_scaleT.f90
    fi
else
    thisfscalet=20210107_perturb_ogcm_scaleT.f90
fi


#================================================================================
#                                  Main Process
#================================================================================
#main
#todo:  check if all restar are ready
blready=$( seas_rst_extracted $yyyyseas $seas $numfextracted_each $dextract )
! $blready && delete_stuff && exit

#todo:  delete any attempt stdout files in message dir
find $cdir/message/* -type f -name "*$strmsgpert*" -delete 2>&1

msg_wheader
wmessage "Extracted Rst Location : $dextract"
wmessage "       User Input Date : $icyyyymmdd"
wmessage "     Used FORTRAN Code : $thisfscalet"
wmessage 

while (( $attempt < $maxattempt ));do

    strattempt=$(( attempt + 1 ))
    wmessage "Attempt $strattempt"
    wmessage

    fm_thispert=${fm_pert}_attempt$strattempt
    msg_newfile $fm_thispert

    setup_dscratch

    #+++++ cd dscratch (start) +++++
    cd $dscratch

#wmessage "@$LINENO"
#wmessage "$fscrpert $srcf $icyyyymmdd >> $fm_thispert 2>&1" 
#exit 

    if (( $writetofile == 1 ));then
        ./$fscrpert $srcf $icyyyymmdd >> $fm_thispert 2>&1
    else
        ./$fscrpert $srcf $icyyyymmdd
    fi
    
    cd - >/dev/null
    #+++++ cd dscratch ( end ) +++++
    
    #todo:  transfer output rst from scratch dir to output dir.
    if $opto;then
        #todo:  delete ens11-15 rst. These rst should come from 
        #       ens6-10 of original *_pert.tar files
        cpout $docnout "${arrocnens[@]}"
        for ens in ${arrocnens[@]};do
            (( $ens < 11 )) && continue
            if [[ -d $douticyyyymmdd/ens$ens ]];then 
                rm -rf $douticyyyymmdd/ens$ens
                (( $? == 0 )) && wmessage "**ens$ens perturbation is deleted"
            fi
        done
        wmessage
    else
        mv2dfinal
    fi
    
    #todo:  set total number of final files in icyyyymmdd & check if there are all pert files
    numftotalcnt=$( find $douticyyyymmdd/* -type f -name "*nc" -o -name "*_rst" 2>/dev/null | wc -l )
    if $opto;then
        numftotalfinal=10
    elif [[ -d $docnout ]] && $blocean;then
        numftotalfinal=28
    else    
        numftotalfinal=8
    fi
    
    if (( $numftotalfinal == $numftotalcnt ));then 
        blsuccess=true
        break
    fi
    
    attempt=$(( attempt + 1 )) 

done     

! $blsuccess && exit

#todo:  copy data into fmessage
cp_text
    
#####################################################################
#     NOTE: FROM HERE, DO NOT PRINT (wmessage or echo) ANYTHING.    #
#####################################################################

#+++++ cd dout (start) +++++
#todo:  create tar files
cd $dout
tar --remove-files -cf $ftar $icyyyymmdd
status_tar1=$?

tar tf $ftar >> /dev/null 2>&1
status_tar2=$?

cd - >/dev/null
#+++++ cd dout (start) +++++

if (( $status_tar1 == 0 && $status_tar2 == 0 )); then 
    [[ -d $douticyyyymmdd ]] && rm -rf $douticyyyymmdd
          [[ -d $dscratch ]] && rm -rf $dscratch
fi


exit
