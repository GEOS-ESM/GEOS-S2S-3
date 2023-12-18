#!/usr/bin/env bash

inputcheck(){
    [[ -z  ${arrposarg[@]} ]] && die "an input is missing"
    (( ${#arrposarg[@]} != 2 )) && die "2 inputs are required"
    
    startyyyy=$( echo ${arrposarg[0]} | cut -d'-' -f1 )
      endyyyy=$( echo ${arrposarg[0]} | cut -d'-' -f2 )
    [[ -z $startyyyy || -z $endyyyy ]] && die "year range is required" 
   
    return
}

cnt_fextracted() {
    #description:   count files in $dextract
    local cnt_total=0
    local arrfstr=( ${arrfstr_atm[@]} ${arrfstr_ocn[@]} )
    local _strf

    if [[ -d $dextract ]];then
        for _strf in ${arrfstr_atm[@]};do
            local numf=$( find $dextract/* -type f -name "${_strf}*" | wc -l )
            cnt_total=$(( cnt_total + numf ))
        done
        
        for _strf in ${arrfstr_ocn[@]};do
            local numf=$( find $dextract/* -type f -name "*${_strf}" | wc -l )
            cnt_total=$(( cnt_total + numf ))
        done
    fi 
    echo $cnt_total
}

filter_cice() {
    local _arrinput=( "$@" )
    local cntbug=0
    local _arrout=()
    local yyyyseas 

    #filters:    
    #       -check if the first day rst is ready
    #       -check if the last day rst is ready
    #       -check if the first day rst timestamp is older than the last day rst timestamp    
    #       -check if the first ic day perturbation exists on lfe and 
    #           the lastday rst is older than the first icday perturbation
    #       -check if ALL perturbation for the season exists on lfe
    #       -check if all necessary files are extracted.

local _arrinput=( 2023_jja )      
    local arr_allrstpertexits=($( seas_allrstpertexist $flstarc ${_arrinput[@]} ))

wmessage \@$LINENO 
ahand_print ${_arrinput[@]}
wmessage 
#ahand_warr ${arr_allrstpertexits[@]}
#wmessage 
#exit
    for yyyyseas in ${_arrinput[@]};do
        local cntbug=0 

        #todo:  check if ALL perturbation for the season exists on fle
        #[[ "${arr_allrstpertexits[@]}" =~ *" $yyyyseas "* ]] && continue 

        printf '%s\n' ${arr_allrstpertexits[@]} | grep -w $yyyyseas 2>&1 >>/dev/null 
        local _status_grep=$?
        
        (( $_status_grep == 0 )) && continue 

        debug_filter $yyyyseas

        local yyyy=${yyyyseas:0:4}
        local seas=${yyyyseas:5}
        local arrseasdates=($( fcal_seasdates $yyyy $seas )) 

        #todo:  check if restart exist in two different location
        local arrfrstready=($( seas_rst_ready $DRSTPROC1 "${arrseasdates[@]}" ))
        local arrfrstready1=($( seas_rst_ready $DRSTPROC2 "${arrseasdates[@]}" ))
        for frstready in ${arrfrstready1[@]};do 
            local thisfname=$( echo $frstready | rev | cut -d'/' -f1 | rev )
            local thisrstnumf=$( printf '%s\n'${arrfrstready[@]} | grep $thisfname 2>/dev/null | wc -l )
            (( $thisrstnumf == 0 )) && arrfrstready+=( $frstready )
        done

        local numseasdates_perc=$( echo "${#arrseasdates[@]}*$perc/1" | bc )

        #todo:  check if all daily files are available for seas.
        #note:  keep this code here in order to write above message in fmessage file
        #       and leave it in message dir for a record.
        if (( $numseasdates_perc <= ${#arrfrstready[@]} ));then
            : 
        else
            continue
        fi


        debug_filter $yyyyseas


local runhere=false        
        if $runhere;then 
            #todo:  check if the last day rst is ready
            local lastday=$( echo ${arrseasdates[-1]} )
            local capr_yyyymmdd=$( cat $DFCST/cap_restart | cut -d' ' -f1 )
#wmessage \@$LINENO 
#wmessage $lastday $capr_yyyymmdd
#wmessage $capr_yyyymmdd
#wmessage
#exit       
            (( $lastday <= $capr_yyyymmdd )) && : || continue 

            debug_filter $yyyyseas
        fi

        #todo:  check if all necessary files are extracted.
        if [[ -d $dout/$yyyyseas ]];then
            blready=$( seas_rst_extracted $yyyy $seas $numfextracted_each $dout/$yyyyseas )
            ! $blready && rm -rf $dout/$yyyyseas
        else
            blready=false
        fi

        $blready && continue

        debug_filter $yyyyseas
        
        _arrout+=( $yyyyseas )

        (( ${#_arrout[@]} == $maxproc )) && break
    done

    echo "${_arrout[@]}"
}


filter() {
    local _arrinput=( "$@" )
    local cntbug=0
    local _arrout=()
    local yyyyseas 

    #filters:    
    #       -check if the first day rst is ready
    #       -check if the last day rst is ready
    #       -check if the first day rst timestamp is older than the last day rst timestamp    
    #       -check if the first ic day perturbation exists on lfe and 
    #           the lastday rst is older than the first icday perturbation
    #       -check if ALL perturbation for the season exists on lfe
    #       -check if all necessary files are extracted.

    local arr_allrstpertexits=($( seas_allrstpertexist $flstarc ${_arrinput[@]} ))

#wmessage \@$LINENO 
#ahand_warr ${_arrinput[@]}
#wmessage 
#ahand_warr ${arr_allrstpertexits[@]}
#wmessage 
#exit

    for yyyyseas in ${_arrinput[@]};do
        local cntbug=0 

        #todo:  check if ALL perturbation for the season exists on fle
        [[ "${arr_allrstpertexits[@]}" =~ *" $yyyyseas "* ]] && continue 

        printf '%s\n' ${arr_allrstpertexits[@]} | grep -w $yyyyseas 2>&1 >>/dev/null 
        local _status_grep=$?
        
#wmessage \@$LINENO $yyyyseas $_status_grep

        (( $_status_grep == 0 )) && continue 

        debug_filter $yyyyseas


        local yyyy=${yyyyseas:0:4}
        local seas=${yyyyseas:5:3}
        local arrseasdates=($( fcal_seasdates $yyyy $seas )) 

        #todo:  check if restart exist in two different location
        local arrfrstready=($( seas_rst_ready $DRSTPROC1 "${arrseasdates[@]}" ))
        local arrfrstready1=($( seas_rst_ready $DRSTPROC2 "${arrseasdates[@]}" ))
        for frstready in ${arrfrstready1[@]};do 
            local thisfname=$( echo $frstready | rev | cut -d'/' -f1 | rev )
            local thisrstnumf=$( printf '%s\n'${arrfrstready[@]} | grep $thisfname 2>/dev/null | wc -l )
            (( $thisrstnumf == 0 )) && arrfrstready+=( $frstready )
        done

        local numftar_perc=$( echo "${#arrseasdates[@]}*$perc/1" | bc )
        local numseasdates_perc=$( echo "${#arrseasdates[@]}*$perc/1" | bc )
        
#wmessage lineno = $LINENO        
#wmessage $numftar_perc ${#arrfrstready[@]} ${#arrseasdates[@]}
#wmessage

        #todo:  check if all daily files are available for seas.
        #note:  keep this code here in order to write above message in fmessage file
        #       and leave it in message dir for a record.
        if (( $numseasdates_perc <= ${#arrfrstready[@]} ));then
            : 
        else
            continue
        fi

        debug_filter $yyyyseas


        #todo:  check if the first day rst is ready
        local thisyyyy=$( echo ${arrseasdates[0]} | cut -c1-4 )
        local ffirstday=$DRSTPROC1/restarts.e${arrseasdates[0]}_${hh}z.tar
        if [[ ! -f $ffirstday ]];then 
            local ffirstday=$DRSTPROC2/Y$thisyyyy/restarts.e${arrseasdates[0]}_${hh}z.tar
            [[ ! -f $ffirstday ]] && continue
        fi


        debug_filter $yyyyseas


        #todo:  check if the last day rst is ready
        local thisyyyy=$( echo ${arrseasdates[-1]} | cut -c1-4 )
        local flastday=$DRSTPROC1/restarts.e${arrseasdates[-1]}_${hh}z.tar
        if [[ ! -f $flastday ]];then 
            local flastday=$DRSTPROC2/Y$thisyyyy/restarts.e${arrseasdates[-1]}_${hh}z.tar
            [[ ! -f $flastday ]] && continue
        fi

        debug_filter $yyyyseas


        local blrunhere=false
        if $blrunhere;then
            #todo: check if the first day rst timestamp is older than the last day rst timestamp    
            local ffirstrst_sec=$( stat --printf="%Y" $ffirstday )
            local flastrst_sec=$( stat --printf="%Y" $flastday )
            
            if [[ -n $ffirstrst_sec && -n $flastrst_sec ]];then 

                if (( $ffirstrst_sec >  $flastrst_sec ));then
                    local dextract=$dout/$yyyyseas
                    [[ -d $dextract ]] && rm -rf $dextract
                    continue
                fi
            else
                continue
            fi

            debug_filter $yyyyseas

            #todo:  check if the first ic day perturbation exists on lfe and 
            #       the lastday rst is older than the first icday perturbation
            local arrseasfcstdates=($( fcal_seasfcstdates  $seas $yyyy )) 
            local arricdates=($( printf '%s\n' ${arrseasfcstdates[@]} | xargs -i date -d "{} -1days" +%Y%m%d ))

            grep -w ${arricdates[0]}_pert.tar $flstarc >/dev/null
            status_grep=$?

            if (( $status_grep == 0 ));then
                local flastpert_sec=$( grep -w ${arricdates[0]}_pert.tar $flstarc | cut -d' ' -f1 )
                local flastrst_sec=$( stat --printf="%Y" $flastday )
#wmessage $flastrst_sec "$( date -d@$flastrst_sec ) " $flastday
#wmessage $flastpert_sec "$( date -d@$flastpert_sec ) " ${arricdates[0]}_pert.tar           
                #(( $flastrst_sec < $flastpert_sec )) && wmessage "lastrst is older than last pert file" 
                (( $flastrst_sec < $flastpert_sec )) && continue
            fi

            debug_filter $yyyyseas

        fi


        #todo:  check if all necessary files are extracted.
        if [[ -d $dout/$yyyyseas ]];then
            blready=$( seas_rst_extracted $yyyy $seas $numfextracted_each $dout/$yyyyseas )
            ! $blready && rm -rf $dout/$yyyyseas
        else
            blready=false
        fi

        $blready && continue

        debug_filter $yyyyseas
        
        _arrout+=( $yyyyseas )

        (( ${#_arrout[@]} == $maxproc )) && break
    done

    echo "${_arrout[@]}"
}

sendfmsg() {
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
    [[ -n $flock && -f $flock ]] && rm -f $flock
    [[ -n $flstarc && -f $flstarc ]] && rm -f $flstarc

    return
}

#================================================================================
#                                     Begin
#================================================================================
#beg
hstname=$( hostname )
if [[ $hstname == 'pfe'* ]];then
    echo "this script works only on discover";exit
elif [[ $hstname == 'discover'* ]];then 
    :
fi
    
strscr=$(basename "$0" | cut -d'_' -f2 | cut -d'.' -f1 )
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cdir=$SCRIPTPATH

[[ -z $cdir || ! -d $cdir ]] && echo "cdir is undefined" && exit

cd $cdir

flock=$cdir/${strscr}.lock
ftmp=$cdir/stdout/rundatetime/tmp_$strscr

[[ ! -f $ftmp ]] && install -D /dev/null $ftmp

#todo:  check tmp file size and create new if it is larger than 5kb
stmp=$( find $ftmp -printf "%s\n" )
(( $stmp > 5000 )) && rm -f $ftmp && touch $ftmp

#todo:  write date & time when this script is executed.
if [[ -f $flock ]];then
    echo "$(TZ=America/New_York date +'%m/%d/%Y %H:%M' ) ... $flock exists..." >> $ftmp
else
    echo $(TZ=America/New_York date +'%m/%d/%Y %H:%M' ) >> $ftmp
fi

#todo:  lock this script 
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

hstshort=$( get_host )
optb=false
optf=false
opto=false
rundebug=0
writetofile=0
opt_skiplst=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename $0) -- this is a program to:
        extract necessary restart files from daily tars for creating perturbation.
        
        Usage: ./$(basename "$0") [-chw] YYYY-YYYY srcme_file 

        Input: 
            IC YYYY range (format: YYYY-YYYY) 
            A source file, which set various vars for a set of runs (i.e. srcme_dis_*)
 
        options:
            -b              run with a debug mode (this will stop before extracting files) 
            -o              re-do ocean perturbation for ens6 to ens10 (see note below)
                --skip-lst  skip getting a list of restarts that are already on archive dir
                            **This options should be used along with option r.
            -c              clean unwanted files
            -h              show this help text
            -w              write stdout/err in a file
        
        Note1: For s2s_v3 hindcast, the range is 198112-201012.
        Note2: 06/21/2022 There was a bug in ocean perturbation and need to be re-processed
                          for ens6 to ens10. Original ocn pert for ens6 to 10 were for 
                          ens11 to 15. Thus, those exp dir is renamed appropriately.
"
verbose=0
cnt=0
while :; do
    case $1 in
                   -b )  optb=true && rundebug=1;; 
                   -o )  opto=true;; 
                   -c )  clean_dir;exit;;
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

#OPTIND=1
#while getopts 'chwbo' option; do
#    case "$option" in
#        b)  optb=true;rundebug=1;;
#        o)  opto=true;;
#        c)  clean_dir; exit 0;;
#        h)  echo "$usage"; exit 0;;
#        w)  writetofile=1;; 
#        \?) echo "Invalid option: -$OPTARG" >&2
#            exit 1;;
#        :)  echo "Option -$OPTARG requires an argument." >&2
#            exit 1;;
#    esac
#done
##f)  startyyyy=${OPTARG:0:4};endyyyy=${OPTARG:5:4};; 

#todo:  get positional inputs. 
shift $((OPTIND-1))
[[ "${1:-}" = "--" ]] && shift
arrposarg=( "$@" )

#================================================================================
#                               Check User Inputs
#================================================================================
inputcheck
srcf=${arrposarg[1]}
source ${arrposarg[1]}
[[ -z $DARCHRST || -z $strexpid ]] && die "DARCHRST or strexpid are not defined" 
#================================================================================
#                             Set Host Specific Vars
#================================================================================
if [[ $hstshort == "pfe" ]];then
    :
elif [[ "$hstshort" == "dis" ]];then 
    hstname=$( get_host )
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

divby=20
hh=21
[[ -z $perc ]] && perc=0.65
#perc2=$( echo "$perc*100/1" | bc )

#note:  max number of processes per run. Process only 20 per script 
#       execution instead of processing many at a time. 
maxproc=20

dmess=$cdir/message
dout=$cdir/output/$strscr/$strdout
dstdout=$cdir/stdout/$strscr/$strdout
ferr=$dmess/stderr_$strscr
fwftmsz=wftmsz_flist
flstarc=$cdir/${strscr}_arc_$fwftmsz

msg_subject_base="${hstshort}.${strscr}: $strexpid"

arrseas=( djf mam jja son )
arrfstr_atm=( fvcore_internal_ moist_internal_ )
arrfstr_ocn=( _temp_salt.res.nc _velocity.res.nc )

numfextracted_each=$( printf '%s\n' ${arrfstr_atm[@]} ${arrfstr_ocn[@]} | wc -l )

feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

[[ ! -d $dmess   ]] && mkdir -p $dmess
[[ ! -d $dstdout ]] && mkdir -p $dstdout

#================================================================================
#                                  Main Process
#================================================================================
#letsbegin
blskip=true
blskip=false

if $blskip;then 
    flstarc_org=/discover/nobackup/knakada/GEOSS2S3/GEOS_util/wftmsz_flist
    cp -p $flstarc_org $flstarc

else
    if [[ $strexpid == "test" ]];then 
        :
    elif $opt_skiplst;then 
        touch $flstarc
    else
        sup ssh $HSTARCHRST test -d /u/$USER >>/dev/null 2>&1
        status_hst=$?
    
        (( $status_hst > 0 )) && exit
    
        get_fwftmsz $DARCHRST $flstarc
        if [[ ! -f $flstarc ]];then
            exit
            #wmessage "skip retrieving but create empty $( basename $flstarc )"
            #touch $flstarc
        fi
    fi
fi

#todo:  get all seasons
arryyyy=($( seq $startyyyy $endyyyy ))
arryyyyseas=($( printf '%s\n' ${arryyyy[@]} | xargs -i printf "{}_%s\n" ${arrseas[@]} ))

#note:  due to lack of restarts, 2022 cice experiments will use only available rst to 
#       create perturbation rst.
if $opto;then
    arrfinal=( ${arryyyyseas[@]} )

elif [[ "$strexpid" == "cice" ]];then 
    if (( $startyyyy == 2022 && $endyyyy == 2022 || $startyyyy == 2023 && $endyyyy == 2023));then
        arrfinal=($( filter_cice "${arryyyyseas[@]}" )) 
    else
        arrfinal=($( filter "${arryyyyseas[@]}" ))
    fi
else
    arrfinal=($( filter "${arryyyyseas[@]}" ))
fi

#todo:  keep only maxproc
arrfinal=($( printf '%s\n' ${arrfinal[@]} | sort -V | head -$maxproc ))

if $optb;then 
    ahand_warr ${arrfinal[@]}
    wmessage
    $opto && wmessage "** opto is true; Skip filter **"
    wmessage "Number of Seasons : ${#arrfinal[@]}"
    exit
fi

#todo:  check if all restar are ready
for yyyyseas in ${arrfinal[@]};do

    yyyy=${yyyyseas:0:4} 
    seas=${yyyyseas:5} 

    arrseasdates=($( fcal_seasdates $yyyy $seas )) 
    numftar_perc=$( echo "${#arrseasdates[@]}*$perc/1" | bc )


    #todo:  check if restart exist in two different location
     arrfrstready=($( seas_rst_ready $DRSTPROC1 "${arrseasdates[@]}" ))
    arrfrstready1=($( seas_rst_ready $DRSTPROC2 "${arrseasdates[@]}" ))
    for frstready in ${arrfrstready1[@]};do 
          thisfname=$( echo $frstready | rev | cut -d'/' -f1 | rev )
        thisrstnumf=$( printf '%s\n'${arrfrstready[@]} | grep $thisfname 2>/dev/null | wc -l )
        (( $thisrstnumf == 0 )) && arrfrstready+=( $frstready )
    done

    #note:  06/07/2022 Moved this part to filter.
    ##todo:  check if all daily files are available for seas.
    ##note:  keep this code here in order to write above message in fmessage file
    ##       and leave it in message dir for a record.
    #(( $( echo "$numftar_perc >= ${#arrfrstready[@]}" | bc )  )) && : || continue

    dextract=$dout/$yyyyseas
    dextract_rst=$dextract/RESTART
    fmessage=$cdir/message/message_${strscr}_${strexpid}_${yyyy}_$seas

    (( $writetofile == 1 )) && [[ ! -f $fmessage ]] && touch $fmessage
    msg_subject="$msg_subject_base ( $yyyy $( echo $seas | tr '[:lower:]' '[:upper:]' ) )"
    msg_wheader
    wmessage "           Extracted File Locations : $dextract"
    wmessage "       A Number of Days in $yyyy $( echo $seas | tr '[:lower:]' '[:upper:]' ) : ${#arrseasdates[@]} days"
    wmessage "Total number of Available tar files : ${#arrfrstready[@]}"
    wmessage "                         srcme File : $srcf"
    #wmessage "       ${perc2}% of Total Number of files : ${numftar_perc}"
    wmessage

    #todo:  start extracting
    fhand_newdir $dextract_rst

    #note:  original script is:
    #       /gpfsm/dnb02/projects/p58/aogcm/g5fcst/s2sv3/netcdf/V3/tiny_tar.csh
    numnum=$( echo -n ${#arrfrstready[@]} | wc -m )
    cnt_fcompleted=1
    for fl in ${arrfrstready[@]};do
        tar -xf $fl -C $dextract --wildcards --no-anchored --warning=no-ignore-newer --keep-newer-files "${arrfstr_atm[0]}*" "${arrfstr_atm[1]}*" "*${arrfstr_ocn[0]}" "*${arrfstr_ocn[1]}"
                    
        cnt_fcompleted=$(( cnt_fcompleted + 1 ))
        if (( $(( $cnt_fcompleted % $divby )) == 0 )) || (( $cnt_fcompleted == ${#arrfrstready[@]} )) ;then
            wmessage "    ... $(date +'%I:%M%P') ... $(printf '%0'$numnum'g' $cnt_fcompleted ) of $( printf '%0'$numnum'g' ${#arrfrstready[@]} ) files are processed"
        fi
    
    done

    #todo:  send email
    [[ -f $fmessage ]] && sendfmsg
    #if [[ -f $fmessage ]];then
    #    sizef=$( stat --print='%s' $fmessage )
    #    if (( $sizef > 0 ));then
    #        msg_cyberpostman "$msg_subject" "$eadds" $fmessage
    #        mv $fmessage $dstdout
    #    fi
    #fi

done 


exit


