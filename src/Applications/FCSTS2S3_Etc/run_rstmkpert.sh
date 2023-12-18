#!/usr/bin/env bash

inputcheck() {
    #todo:  check opt r arguments

    [[ -z  ${arrposarg[@]} ]] && die "an input is missing"
    (( ${#arrposarg[@]} > 1 )) && die "there are more than 1 input"

    $optr        && [[ -z $startyyyymm || -z $endyyyymm ]] && die "YYYYMM range is a required input for option r."
    $opto        && [[ -z $startyyyymm || -z $endyyyymm ]] && die "YYYYMM range is a required input for option o."
    $opt_redoocn && [[ -z $startyyyymm || -z $endyyyymm ]] && die "YYYYMM range is a required input for option redo-ocn."
    $opt_skiplst && ! $optr                                && die "option skip-lst should be used along with option r."
    
    if $optr;then
        local int1=$( misc_isinteger $startyyyymm ) 
        local int2=$( misc_isinteger $endyyyymm ) 
        
        if (( $int1 > 0 || $int2 > 0 ));then
            die "YYYYMM range have to be numbers delimited by a hyphen"
        fi
    fi

    return
}

get_seasonsicdate() {
    local arrinput=( "$@" )
    local yyyy seas yyyyp
    local arricyyyymmdd=()
    local arrtmp=()

    for yyyyseas in ${arrinput[@]};do
        yyyy=$( echo $yyyyseas | cut -d'_' -f1 )
        seas=$( echo $yyyyseas | cut -d'_' -f2 )
        yyyyp=$(( yyyy - 1 ))

        arrtmp=($( fcal_seasfcstdates $seas $yyyy ))
        arricyyyymmdd+=($( printf '%s\n' ${arrtmp[@]} | xargs -i bash -c 'date -d "{} -1days" +%Y%m%d' ))
    done
    echo "${arricyyyymmdd[@]}"
}

filter_yyyyseas() {
    local arrinput=( "$@" )
    local numfextracted_each=4
    local input arr=()
    #       -check if extract dir exists for the season 
    #       -check if all necessary files are extracted in output/rstextr.

    for input in ${arrinput[@]};do
        local cntbug=0 
        local yyyyseas=$( echo $input | cut -d'_' -f1 )
        local  seas=$( echo $input | cut -d'_' -f2 )

        #todo:  extract dir exists for the season 
        [[ ! -d $drstextract/$input ]] && continue 

        debug_filter $input

        #todo:  check if all necessary files are extracted in output/rstextr.
        local blready=$( seas_rst_extracted $yyyyseas $seas $numfextracted_each $drstextract/$input )
        
        ! $blready && continue 

        debug_filter $input
            
        arr+=( $input )
    done

    echo ${arr[@]}
}

filter() {
    local arrinput=( "$@" )
    local hh=21
    local arr=() input
    #       -check if the last day rst exists
    #       -check if *_pert.tar exist on lfe
    #       -check if *_pert.tar exits in output dir and the last day rst is older than this perturbation tar.
    #       -check if processing is ongoing 

    for input in ${arrinput[@]};do
        local cntbug=0 

        local yyyy=${input:0:4}
        local int_m=$( echo "${input:4:2}*1" | bc )
        local seas=$( fcal_month2seas $int_m )
            
        (( $int_m == 12 )) && yyyy=$(( yyyy + 1 )) 

        #todo:  check if the last day rst exists
        local arrseasdates=($( fcal_seasdates $yyyy $seas )) 
        local thisyyyy=$( echo ${arrseasdates[-1]} | cut -c1-4 )
        local flastday=$DRSTPROC1/restarts.e${arrseasdates[-1]}_${hh}z.tar
        if [[ ! -f $flastday ]];then 
            local flastday=$DRSTPROC2/Y$thisyyyy/restarts.e${arrseasdates[-1]}_${hh}z.tar
            [[ ! -f $flastday ]] && continue
        fi


        debug_filter $input
      

        if $opto;then 
            local _ftar=${input}_pert_ocnens6to10.tar
        else
            local _ftar=${input}_pert.tar
        fi
        
        local flastdayrst_sec=$( stat --printf="%Y" $flastday )

        grep -w $_ftar $flstarc >/dev/null
        local status_grep=$?

        #todo:  check if the lastday rst is older than perturbation tar file. 
        #       make sure that pert file on lfe is processed AFTE lastday file 
        #       was created.
        #if (( $status_grep == 0 ));then
        #    local fpertlfe_sec=$( grep -w ${input}_pert.tar $flstarc | cut -d' ' -f1 )
        #    #wmessage "@$LINENO" 
        #    #wmessage "$( date -d@$flastdayrst_sec )"
        #    #wmessage "$( date -d@$fpertlfe_sec )"
        #        
        #    (( $flastdayrst_sec < $fpertlfe_sec )) && continue
        #fi
        
        #todo:  check if *_pert.tar exist on lfe
        if (( $status_grep == 0 ));then
            continue
        else
            :
        fi


        debug_filter $input
        

        #todo:  check if *_pert.tar exits in output dir and the last day rst is older than this perturbation tar.
        local fpert=output/$strscr/$strdout/$_ftar
        if [[ -f $fpert ]];then
            local fpert_sec=$( stat --printf="%Y" $fpert )
            (( $flastdayrst_sec < $fpert_sec )) && continue
        fi


        debug_filter $input
        

        #todo:  check if processing is ongoing 
        local thisflock=$cdir/${strscr}_${strexpid}_$icyyyymmdd.lock
        [[ -f $thisflock ]] && continue


        debug_filter $input

        
        ##todo:  check if daily rst exist for icyyyymmdd
        #local frst=$DRST/restarts.e${input}_21z.tar
        #[[ ! -f $frst ]] && continue
        #debug_filter $input

        arr+=( $input ) 

        #(( ${#arr[@]} == $maxproc )) && break

    done

    [[ -f $flstarc ]] && rm -f $flstarc
    echo ${arr[@]}
}

filter_ens6to10() {
    local arrinput=( "$@" )
    local hh=21
    local arr=() input
    #       -check if *_pert_ocnens6to10.tar exist on lfe
    #       -check if *_pert.tar exist on lfe
    #       -compare pert.tar timestamp with 2022-06-18
    #       -check if the lastday rst is older than perturbation tar file. 
    #       -check if *_pert.tar exits in output dir and the last day rst is older than this perturbation tar.
    #       -check if processing is ongoing 

    for input in ${arrinput[@]};do
        local cntbug=0 

        local yyyy=${input:0:4}
        local int_m=$( echo "${input:4:2}*1" | bc )
        local seas=$( fcal_month2seas $int_m )
            
        (( $int_m == 12 )) && yyyy=$(( yyyy + 1 )) 

        local _ftar_ocnens6to10=${input}_pert_ocnens6to10.tar
        grep -w $_ftar_ocnens6to10 $flstarc >/dev/null
        local status_grep=$?

        #todo:  check if *_pert_ocnens6to10.tar exist on lfe
        if (( $status_grep == 0 ));then
            continue
        else
            :
        fi


        debug_filter $input


        #todo:  check if *_pert.tar exist on lfe
        #note:  *_pert.tar has to exist in order to "re-doing" this process. 
        #       Otherwise, perturbation for date ($input) should be processed normally (without opto)
        local _ftar=${input}_pert.tar
        grep -w $_ftar $flstarc >/dev/null 
        local status_grep=$?

        if (( $status_grep == 0 ));then
            :
        else
            continue
        fi
        

        debug_filter $input


        #todo:  compare pert.tar timestamp with 2022-06-18
        #note:  2022-06-18 is the date that the process of 
        #       recreating ocn pert started
        local _sec=$( date -d $dateprocessed +%s )
        local _ftar_sec=$( grep -w $_ftar $flstarc 2>/dev/null | cut -d' ' -f1 )
        if (( $_ftar_sec < $_sec ));then 
            :
        else
            continue
        fi


        debug_filter $input


        #todo:  check if the last day rst exists
        local arrseasdates=($( fcal_seasdates $yyyy $seas )) 
        local thisyyyy=$( echo ${arrseasdates[-1]} | cut -c1-4 )
        local flastday=$DRSTPROC1/restarts.e${arrseasdates[-1]}_${hh}z.tar
        if [[ ! -f $flastday ]];then 
            local flastday=$DRSTPROC2/Y$thisyyyy/restarts.e${arrseasdates[-1]}_${hh}z.tar
            [[ ! -f $flastday ]] && continue
        fi


        debug_filter $input
        

        #todo:  check if *_pert.tar exits in output dir and the last day rst is older than this perturbation tar.
        local flastdayrst_sec=$( stat --printf="%Y" $flastday )
        local fpert=output/$strscr/$strdout/$_ftar_ocnens6to10
        if [[ -f $fpert ]];then
            local fpert_sec=$( stat --printf="%Y" $fpert )
            (( $flastdayrst_sec < $fpert_sec )) && continue
        fi


        debug_filter $input
        

        #todo:  check if processing is ongoing 
        local thisflock=$cdir/${strscr}_${strexpid}_$icyyyymmdd.lock
        [[ -f $thisflock ]] && continue


        debug_filter $input
        
        arr+=( $input ) 

        #(( ${#arr[@]} == $maxproc )) && break

    done

    [[ -f $flstarc ]] && rm -f $flstarc
    echo ${arr[@]}
}

sendfmsg(){    
    #description:   send email
    local _blrm=false
    
    [[ -f $ferr     ]] && local _sizeferr=$( stat --print='%s' $ferr )  || local _sizeferr=0
    [[ -f $fmessage ]] && local _sizef=$( stat --print='%s' $fmessage ) || local _sizef=0

    if (( $_sizef > 0 || $_sizeferr > 0 ));then
        if (( $_sizeferr > 0 ));then 
            msg_wheader_userdefined 40 "-" $( basename $ferr ) 
            wmessage "$( cat $ferr )"
            _blrm=true
        fi

        msg_cyberpostman "$msg_subject" "$eadds" $fmessage
        local status_email=$?
        (( $status_email == 0 )) && rm -f $fmessage
        $_blrm && msg_newfile $ferr
    fi
    return
}

clean_dir() {
    [[ -f $flock ]] && rm -f $flock
    return
}

#================================================================================
#                                     Begin
#================================================================================
#starthere
strscr=$(basename "$0" | cut -d'_' -f2 | cut -d'.' -f1 )
hst=`hostname`
if [[ "$hst" =~ "dis" ]];then
    cdir=/discover/nobackup/projects/gmao/t2ssp/GEOSS2S3/GEOS_util
else
    #cdir=/nobackupp11/gmaofcst/GEOSS2S3/GEOS_util
    echo "this script works only on discover";exit
fi
[[ -z $cdir || ! -d $cdir ]] && exit

cd $cdir

flock=$cdir/${strscr}.lock
ftmp=$cdir/stdout/rundatetime/tmp_$strscr
[[ ! -f $ftmp ]] && install -D /dev/null $ftmp

#todo:  check tmp file size and create new if it is larger than 5kb
stmp=$( find $ftmp -printf "%s\n" )
if (( $stmp > 5000 ));then
    rm -f $ftmp;touch $ftmp
fi

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

if [[ ! $cdir/func_fcst.sh ]];then 
    echo "$cdir/func_fcst.sh does not exist";exit
else
    source $cdir/func_fcst.sh
fi
trap clean_dir EXIT

rundebug=0
writetofile=0
optb=false
optr=false
opto=false
opt_redoocn=false
opt_skiplst=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        create perturbation rst files. This program will figure out
        available daily files based on directories in
        output/rstextr (yyyy_seas).
        
        Usage: ./$(basename "$0") [-hbc] [-r|-o|--redo-ocn YYYYMM-YYYYMM] srcme_file

        Input: 
            A source file, which set various vars for a set of runs (i.e. srcme_dis_*)
 
        options:
            -r              process data for icdate between given YYYYMM and 
                            YYYYMM (hyphen for a delimiter)
            -b              run with a debug mode (this will not execute ${strscr}.sh)
            -o              redo ocean perturbation for ens6 to ens10 and create *pert_ocnens6to10.tar 
                            (see note below)
                --redo-ocn  redo perturbation for the last icdate of the month between YYYYMM-YYYYMM
                            **THIS OPTION IS ONLY FOR S2SV3 HINDCAST ATTEMPT 1**
                --skip-lst  skip getting a list of restarts that are already on archive dir
                            **This options should be used along with option r.
            -h              show this help text
            -c              remove unnecessary files
            -w              write stdout/err in a file

        note:  06/21/2022   There was a bug in ocean perturbation and need to be re-processed
                            for ens6 to ens10. Original ocn pert for ens6 and ens10 were for ens11
                            to 15. Thus, those exp dir is renamed appropriately.
"

verbose=0
cnt=0
while :; do
    case $1 in
                   -r )  [[ "$2" ]] && optr=true && userinput=$2 && shift;
                         startyyyymm=$( echo $userinput | cut -d'-' -f1 );
                           endyyyymm=$( echo $userinput | cut -d'-' -f2 );;
                   -c )  clean_dir;exit;;
                   -b )  optb=true && rundebug=1;; 
                   -o )  [[ "$2" ]] && opto=true && optr=true && userinput=$2 && shift;
                         startyyyymm=$( echo $userinput | cut -d'-' -f1 );
                           endyyyymm=$( echo $userinput | cut -d'-' -f2 );;
           --redo-ocn )  [[ "$2" ]] && opt_redoocn=true && optr=true && userinput=$2 && shift;
                         startyyyymm=$( echo $userinput | cut -d'-' -f1 );
                           endyyyymm=$( echo $userinput | cut -d'-' -f2 );;
           --skip-lst )  opt_skiplst=true ;;
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

! $optr && ! $opto && ! $opt_redoocn && ! $opt_skiplst && blnrt=true || blnrt=false
arrposarg=( "$@" )
#================================================================================
#                               Check User Inputs
#================================================================================
inputcheck
srcf=${arrposarg[0]}
source $srcf

[[ -z $DARCHRST || -z $strexpid || -z $DRSTPROC1 || -z $DRSTPROC2 ]] && die "DARCHRST, DRSTPROC1, DRSTPROC2, or strexpid are not defined" 
#================================================================================
#                             Set Host Specific Vars
#================================================================================
if [[ $hst =~ "pfe"* ]];then
    :
    #hstshort=$( get_host) 
elif [[ $hst =~ "discover"* ]];then 

    hstshort=$( get_host) 

    export NB=/discover/nobackup/knakada
    export NB_LOCAL=$NB/local
    basepath=$PATH
    basepath=/usr/local/other/SLES11.3/Grep/3.1/gcc-4.3.4/bin:/usr/local/other/SLES11.3/bash/4.3.30/bin:$basepath
    basepath=/usr/slurm/bin:/home/gmaofcst/bin:/usr/local/bin:/usr/bin:/bin:/usr/bin/X11:/usr/games:$basepath
    basepath=/opt/ibutils/bin:/usr/lib/mit/bin:/usr/lib/mit/sbin:/opt/gnome/bin:/usr/slurm/bin:$basepath
    export BASEPATH=/discover/nobackup/knakada/local/cdo/1.9.8/cdo-1.9.8/src:$basepath
    export PATH=$BASEPATH
    export TERM=xterm-256color
    umask 0022

fi

[[ ! -d $DRSTPROC1 ]] && die $DRSTPROC1 does not exist 
[[ ! -d $DRSTPROC2 ]] && die $DRSTPROC2 does not exist 
#================================================================================
#                                 Set Variables
#================================================================================
#mid
[[ -n $rstexpid ]] && strexpid=$strexpid$rstexpid

dmess=$cdir/message
ferr=$dmess/stderr_${strscr}
fmessage=$dmess/message_${strscr}_${strexpid}

fwftmsz=wftmsz_flist
flstarc=$cdir/${strscr}_arc_$fwftmsz

msg_subject="${hstshort}.${strscr}: $strexpid"

numsec=15
maxsec=3600
maxmin=$( echo "$maxsec/60" | bc )

#note:  ocean perturbation dates
arrocn_mmdd=( 1226 0130 0224 0326 0425 0530 0629 0729 0828 0927 1027 1126 )

#note:  reprocessing ocean pert for ens6-10 started sometime on 2022-06-18. They
#       should have timestamp anytime on the day or after.
dateprocessed="2022-06-18"

#note:  max number of processes per run. Process only 20 per script 
#       execution instead of processing many at a time. 
maxproc=20

maxscr=10
dumyyyy=2015
drstextract=$cdir/output/rstextr/$strdout

RUNCHILD=1

feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

[[ ! -f $strscr.sh ]] && die "$strscr.sh does not exist"
[[ ! -f $ferr ]] && touch $ferr

[[ -z $DRSTPROC1 || ! -d $DRSTPROC1 ]] && exit
[[ -z $DRSTPROC2 || ! -d $DRSTPROC2 ]] && exit
(( $writetofile == 1 )) && [[ ! -f $fmessage ]] && touch $fmessage

[[ ! -d $dmess ]] && mkdir -p $dmess

export RUNCHILD
#================================================================================
#                                  Main Process
#================================================================================
#letsbegin
#todo:  get a file with a list of rst on lfe.
blskip=true
blskip=false

if $blskip;then 
    #cdate=$( date +%Y%m%d_%H%M )
    #flstarc=$cdir/$( basename $flst_arc_org )_$cdate 
    
    flstarc_org=/discover/nobackup/knakada/GEOSS2S3/GEOS_util/wftmsz_flist
    cp -p $flstarc_org $flstarc

else
    
    if $opt_skiplst || [[ $strexpid == "test" ]];then 
        touch $flstarc
    else
        sup ssh $HSTARCHRST test -d /u/$USER >>/dev/null 2>&1
        status_hst=$?
    
        (( $status_hst > 0 )) && exit
    
        get_fwftmsz $DARCHRST $flstarc
        if [[ ! -f $flstarc ]];then
            #wmessage "$( basename $flstarc ) was not retrieved from lfe"
            #[[ -f $fmessage ]] && sendfmsg
            exit
    
            #wmessage "skip retrieving but create empty $( basename $flstarc )"
            #touch $flstarc
        fi
    fi
fi


if $optr;then
    startyyyy=$( echo $startyyyymm | cut -c1-4 )
      endyyyy=$( echo $endyyyymm   | cut -c1-4 )

    #todo:  get icdate based on user's input
    #startyyyymm=201501
    #  endyyyymm=201503     #<== exclusive
    arricyyyymmdd_userinput=($( get_icdates $startyyyymm $endyyyymm ))


    #todo:  figure out seasons that ic dates belong to
    arrtmp=()
    for icyyyymmdd in ${arricyyyymmdd_userinput[@]};do
#wmessage \@$LINENO $icyyyymmdd        
        yyyy=${icyyyymmdd:0:4}
        int_m=$( echo "${icyyyymmdd:4:2}*1" | bc )
        seas=$( fcal_month2seas $int_m )

        (( $int_m == 12 )) && yyyy=$(( yyyy + 1 )) 
        [[ -d $drstextract/${yyyy}_$seas ]] && arrtmp+=( ${yyyy}_$seas )
    done
    arryyyyseas=($( printf '%s\n' ${arrtmp[@]} | sort | uniq ))

    #todo:  get all ic dates that belongs to seasons that files are available for.     
    arricyyyymmdd_seasonavail=($( get_seasonsicdate ${arryyyyseas[@]} ))

#wmessage \@$LINENO 
#ahand_print ${arrtmp[@]} 

    #todo:  based on seasons avail, figure out icdates that can be processed. 
    arrtmp=()
    for icyyyymmdd in ${arricyyyymmdd_userinput[@]};do
        [[ "${arricyyyymmdd_seasonavail[@]}" =~ "$icyyyymmdd" ]] && arrtmp+=( $icyyyymmdd )
    done

    arricyyyymmdd=( ${arrtmp[@]} )

else
    #todo:  get fcst dates based on available YYYY_seas dir
      arryyyyseas=($( find $drstextract/* -maxdepth 0 -type d 2>/dev/null | xargs -i basename {} ))
    arricyyyymmdd=($( get_seasonsicdate ${arryyyyseas[@]} ))

fi


#todo:  redoing ocn perturbation from 1996 ~
if $opt_redoocn;then

    #note:  06/18/2022 1996~ ocn perturbation needs to be re-processed. This
    #                   code was used only once for the processes. 
    arricyyyymmdd_ocn=()
    for ocnicyyyymmdd in ${arrocn_mmdd[@]};do
        arricyyyymmdd_ocn+=($( printf '%s\n' ${arricyyyymmdd[@]} | grep $ocnicyyyymmdd ))
    done

    arricyyyymmdd=($( printf '%s\n' ${arricyyyymmdd_ocn[@]} | sort -V ))
fi

#wmessage "@$LINENO"
##ahand_warr ${arricyyyymmdd[@]} 
#ahand_warr ${arricyyyymmdd_userinput[@]} 
#wmessage
#exit

if $opto;then

    #arrfinal=($( filter ${arricyyyymmdd[@]}))
    #arrfinal=( ${arricyyyymmdd[@]} )

    arricyyyymmdd_ocn=()
    for ocnicyyyymmdd in ${arrocn_mmdd[@]};do
        arricyyyymmdd_ocn+=($( printf '%s\n' ${arricyyyymmdd[@]} | grep $ocnicyyyymmdd ))
    done

    arricyyyymmdd=($( printf '%s\n' ${arricyyyymmdd_ocn[@]} | sort -V ))

    arrfinal=($( filter_ens6to10 ${arricyyyymmdd[@]}))

elif [[ "$strexpid" == "cice" ]] && (( $startyyyy == 2022 && $endyyyy == 2022 ));then
    #For 2022 May experiments, March and parts of April restarts did not exist. Perturbation
    #rst were created based on available restarts from April & May. There was only 60% of 
    arrfinal=( ${arricyyyymmdd[@]} )

elif [[ "$strexpid" == "cice" ]] && (( $startyyyy == 2023 && $endyyyy == 2023 ));then
    #For 2023 Aug experiments, only June and July restarts were available. Perturbation
    #rst were created based on available restarts from June and July. There was only 60% of 
    arrfinal=( ${arricyyyymmdd[@]} )

else
    arrfinal=($( filter ${arricyyyymmdd[@]}))
fi


#todo:  keep only maxproc
if $opt_redoocn || $opto;then
    arrfinal=($( printf '%s\n' ${arrfinal[@]} | sort -V ))
else
    arrfinal=($( printf '%s\n' ${arrfinal[@]} | sort -V | head -$maxproc ))
fi

#todo:  run here if debug mode option (b) is selected
$optb && ahand_print ${arrfinal[@]} && exit

#todo:  exit here when nothing to process
(( ${#arrfinal[@]} == 0 )) && exit

#arrfinal=( 20230430 ) 

msg_wheader
wmessage "... Total number of exp : ${#arrfinal[@]}"

#todo:  setup arrays.
cnt_dexp=0
for ind in ${!arrfinal[@]};do    
    cnt_dexp=$(( cnt_dexp + 1 ))

    ind1=$(( ind + 1 ))
    icyyyymmdd=${arrfinal[$ind]}

    #todo:  count number of screen
    numscr=$( screen -ls | grep -i detached | grep ${strscr}_${strexpid}_ | wc -l )

    #todo:  count number of processes
    #numproc=$( ps aux | awk '{ print $1 }' | grep -w $USER | wc -l )

    blexecuted=false
    blinitialnote=false
    totsec=0
    totmin=0

    sec0=$( date +%s )
    sec1=$sec0


    #todo:  exist when total of 5 screen sessions are created
    while ! $blexecuted; do
        #numscr=$( screen -ls | grep ${strscr}_ | wc -l )
        if (( $numscr < $maxscr && $totsec <= $maxsec ));then 

            totsec=0
            blexecuted=true

            wmessage "$( printf "%+3s of %+3s\n" $cnt_dexp ${#arrfinal[@]} ) $icyyyymmdd"

            if $opto;then
                thismessage=$cdir/message/message_${strscr}_${strexpid}_${icyyyymmdd}_ocnens6to10
                scrname=${strscr}_${strexpid}_${icyyyymmdd}_ocnens6to10
            else
                thismessage=$cdir/message/message_${strscr}_${strexpid}_$icyyyymmdd
                scrname=${strscr}_${strexpid}_${icyyyymmdd}
            fi

#wmessage "@$LINENO"
#wmessage "$strscr.sh -m $thismessage $srcf $icyyyymmdd >> $thismessage 2>&1"
#echo  "$strscr.sh -m $thismessage $srcf $icyyyymmdd >> $thismessage 2>&1"  > mini_$icyyyymmdd.sh
#exit
            msg_newfile $thismessage

            if $opto;then
                screen -dmS $scrname bash -c "./$strscr.sh -w -o -m $thismessage $srcf $icyyyymmdd >> $thismessage 2>&1"  
            else
                screen -dmS $scrname bash -c "./$strscr.sh -w -m $thismessage $srcf $icyyyymmdd >> $thismessage 2>&1"  
                ##./$strscr.sh -m $thismessage $srcf $icyyyymmdd >> $thismessage 2>&1 
                #./$strscr.sh -m $thismessage $srcf $icyyyymmdd
            fi

        else

            sec2=$( date +%s )
            sec_diff=$(( sec2 - sec1 ))
            totsec=$(( sec2 - sec0 ))
            totmin=$( echo "$totsec / 60" | bc )

            #todo:  break out one loop (hence, 2)
            (( $totsec >= $maxsec )) && break 2

            if ! $blinitialnote;then 
                blinitialnote=true
            elif (( $sec_diff > 60 ));then
                wmessage "$( date +'%m/%d/%Y %H:%M' )     Running Screen - $( printf '%+2s\n' $numscr ) and Processes - $( printf '%+2s\n' $numproc ) ... waited for $totmin min ( max = $maxmin )"
                sec1=$( date +%s )
            fi
        fi

        numscr=$( screen -ls | grep ${strscr}_ | wc -l )
        #numproc=$( ps aux | awk '{ print $1 }' | grep -w $USER | wc -l )
    done
done

#todo:  wait for the rest of screen sessions to finish. Otherwise, kill them 
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

    #todo:  break out for both for loop (hence, 2)
    if (( $totsec >= $maxsec ));then
        arrscr=($( screen -ls | grep -i detached | grep ${strscr}_${strexpid}_ | tr -d '\t' | cut -d'(' -f1 ))
        printf '%s\n' ${arrscr[@]} | xargs -i bash -c 'screen -XS {} quit' 2>/dev/null

        wmessage "Killed Screens:"
        ahand_warr ${arrscr[@]} 
        break 2 
    fi

    if ! $blinitialnote ;then 
        wmessage "$( date +'%m/%d/%Y %H:%M' )     Running Screen - $( printf '%+2s\n' $numscr ) ... will wait for $maxmin minutes at max"
        blinitialnote=true

    elif (( $sec_diff > 60 ));then
        wmessage "$( date +'%m/%d/%Y %H:%M' )     Running Screen - $( printf '%+2s\n' $numscr ) ... waited for $totmin min"
        sec1=$( date +%s )
    fi
    
    numscr=$( screen -ls | grep -i detached | grep ${strscr}_${strexpid}_ | wc -l )

done


#todo:  send email
[[ -f $fmessage ]] && sendfmsg
exit


