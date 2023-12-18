#!/usr/bin/env bash

inputcheck() {
    [[ -z  ${arrposarg[@]} ]]   && die "an input is missing"
    (( ${#arrposarg[@]} != 1 )) && die "1 input is required"

    $optd && $optf && die "Cannot specify option d when specifying option f"
    $optd && $optr && die "Cannot specify option d when specifying option r"
    $optf && $optd && die "Cannot specify option f when specifying option d"
    $optf && $optr && die "Cannot specify option f when specifying option r"
    $optr && $optd && die "Cannot specify option r when specifying option d"
    $optr && $optf && die "Cannot specify option r when specifying option f"

    #$opt_skiplst && ! $optr && die "option skip-lst should be used along with option r"

    if [[ -n $begyyyymm && -n $endyyyymm ]];then
        local int1=$( misc_isinteger $begyyyymm ) 
        local int2=$( misc_isinteger $endyyyymm ) 
        
        (( $int1 > 0 || $int2 > 0 )) && die "YYYYMM range have to be numbers delimited by a hyphen" && exit
    fi
 
    if $optd;then
        local intint=$( misc_isinteger $yyyymmdd )
        (( $intint > 0 )) && die "opt f requires an integer input" && exit
        
        local intnum=${#yyyymmdd}
        (( $intint != 0 )) && die "opt f requires an input with YYYYMMDD format" 
    fi
    
    $optf && [[ ! -f $fdata ]] && die "$fdata does not exist" 
   
    return
}


filter() {
    
    local _arrinput=( "$@" )
    local input bldistar
    local _arr=()


    #todo:  check original rst
        _arryyyymmdd1=($(  printf "%s\n" ${_arrinput[@]} | xargs -i bash -c "[[   -e $DRSTPROC1/restarts.e{}_${hh}z.tar ]] && echo {}" ))
    _arryyyymmdd_mis=($(  printf "%s\n" ${_arrinput[@]} | xargs -i bash -c "[[ ! -e $DRSTPROC1/restarts.e{}_${hh}z.tar ]] && echo {}" ))
        _arryyyymmdd2=($(  printf "%s\n" ${_arryyyymmdd_mis[@]} | xargs -i bash -c "input_yyyy=\$( echo {} | cut -c1-4 ); [[ -e $DRSTPROC2/Y\$input_yyyy/restarts.e{}_${hh}z.tar ]] && echo {}" ))
        _arryyyymmdd=( ${_arryyyymmdd1[@]} ${_arryyyymmdd2[@]} )
    
    debug_filter ${_arrinput[@]}


    for input in ${_arrinput[@]};do
        local cntbug=0

        #local input_yyyy=$( echo $input | cut -c1-4 )
        ##todo:  check if original rst exists
        #local ftar=$DRSTPROC1/restarts.e${input}_${hh}z.tar
        #if [[ ! -e $ftar ]];then 
        #    local ftar=$DRSTPROC2/Y$input_yyyy/restarts.e${input}_${hh}z.tar
        #    [[ ! -e $ftar ]] && continue
        #fi

        ##[[ -h $ftar ]] && ftar=$( readlink $ftar )

        #debug_filter $input

        #todo:  check if input.tar exists on discover
        [[ -f $dout/$input.tar ]] && bldistar=true || bldistar=false

        if $opt_skiplst;then 
            $bldistar && continue 

        elif ! $blnrt ;then

            #todo:  check if input.tar exists on lfe
            grep -w $input.tar $flstarc >/dev/null
            local _status=$?

            #todo:  check if file on lfe is newer or not. If it's older than original tar file
            #       restarts will be re-processed.
            if (( $_status == 0 ));then
                #local ftarlfe_sec=$( grep -w $input.tar $flstarc | cut -d' ' -f1 )
                #local ftarpfe_sec=$( stat --printf="%Y" $ftar )
                #(( $ftarpfe_sec < $ftarlfe_sec )) && continue
                continue
            elif $bldistar && (( $_status > 0 ));then
                continue
            fi
            
        fi
            
        debug_filter $input


        if $blnrt;then
            #todo: check if all rst files are ready in DRST
            #note:  this filter is specific for near realtime. 
            if [[ -d $DRST/$input/ens1 ]];then 
                local _blready=$( exp_checknumrst $DRST/$input/$enscont ) 
            else
                local _blready=false
            fi
          
            if $_blready && (( $_status == 0 ));then
                continue
            elif $_blready && $bldistar && (( $_status > 0 ));then
                continue
            fi
            
            debug_filter $input 
        fi

        [[ -d $dscratch/$input ]] && rm -rf $dscratch/$input

        debug_filter $input

        _arr+=( $input )

        (( ${#_arr[@]} == $maxproc )) && break
    done

    echo "${_arr[@]}"
}

proc_atm() {
    #description:   rename rst files for atm
    #+++++ cd to dens1 (start) +++++
    cd $dens1
    
    paste <(printf 'mv %s\n' $( find * -type f -name "*.nc4" )) <(printf '%s\n' $( find * -type f -name "*.nc4" | cut -d'.' -f1 )) > $tmpscr
    chmod 755 $tmpscr
    ./$tmpscr
    [[ -f $tmpscr ]] && rm -f $tmpscr
    
    cd - >/dev/null
    #+++++ cd to dens1 ( end ) +++++

    return
}

proc_ocn(){
    #description:   renam ocn rst files

    #+++++ cd to dens1/RESTART (start) +++++
    #todo:  rename ocn rst files
    cd $dens1/RESTART

    paste <(printf 'mv %s\n' $( find * -type f -name "*.nc" )) <(printf '%s\n' $( find * -type f -name "*.nc" | cut -d'.' -f2- )) > $tmpscr
    chmod 755 $tmpscr
    ./$tmpscr
    [[ -f $tmpscr ]] && rm -f $tmpscr
    
    cd - >/dev/null
    #+++++ cd to dens1/RESTART ( end ) +++++

    return 
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
    [[ -n $flstarc && -f $flstarc ]] && rm -f $flstarc
    [[ -n $flock   && -f $flock   ]] && rm -f $flock
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
    cdir=/discover/nobackup/projects/gmao/t2ssp/GEOSS2S3/GEOS_util
elif [[ "${hstname:0:1}" == r ]];then 
    #note:  this is for host other than pfe @ NAS which has a name starting with 'r'
    blnode_nas=true

elif [[ "${hstname:0:4}" == borg || "${hstname:0:4}" == warp ]];then 
    #note:  this is for host other than pfe @ NCCS which has a name starting with 'borg' or 'warp'
    blnode_nccs=true
else 
    exit
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
optd=false
optf=false
opt_skiplst=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        rename control restart files and create cap_restart 
        
         NOTE: When this script is executed without options ( r, t, or d ), it will 
               setupt directories for near-realtime forecast.*

        Usage: ./$(basename "$0") [-bchw] [-f data file] [-r YYYYMM-YYYYMM] [-d YYYYMMDD] srcme_file    
        Input: 
            A source file, which set various vars for a set of runs (i.e. srcme_dis_*)
 
        Options:
            -b              run with a debug mode
            -c              clean unwanted files
            -f              data file with a list of YYYYMMDD
            -d              specify forecast date ( format: YYYYMMDD )
            -r              YYYYMM and YYYYMM range ( format: YYYYMM-YYYYMM ) 
                --skip-lst  skip getting a list of restarts that are already on archive dir
                            **This options should be used along with option r.
            -h              show this help text
            -w              write stdout/err in a file
            
        Note: 
         1. For s2s_v3 hindcast, the range is 198112-201012.
         2. Option f allows to prepare any dates outside of what listed in s2s forecast
            calendar (i.e. 20200425 --> create 20200424.tar)
"
verbose=0
cnt=0
while :; do
    case $1 in
                   -b )  optb=true && rundebug=1;; 
                   -d )  [[ "$2" ]] && optd=true && yyyymmdd=$2  && shift;;
                   -f )  [[ "$2" ]] && optf=true && fdata=$2     && shift;;
                   -r )  [[ "$2" ]] && optr=true && userinput=$2 && shift;
                         begyyyymm=$( echo $userinput | cut -d'-' -f1 );
                         endyyyymm=$( echo $userinput | cut -d'-' -f2 );;
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
#while getopts ':bchf:r:wd:' option; do
#    case "$option" in
#        b)  optb=true;rundebug=1;; 
#        d)  $optf && die "Cannot specify option d when specifying option f"
#            $optr && die "Cannot specify option d when specifying option r"
#            optd=true; yyyymmdd=$OPTARG;;
#        f)  $optd && die "Cannot specify option f when specifying option d"
#            $optr && die "Cannot specify option f when specifying option r"
#            optf=true; fdata=$OPTARG;;
#        r)  $optd && die "Cannot specify option r when specifying option d"
#            $optf && die "Cannot specify option r when specifying option f"
#            optr=true; 
#            begyyyymm=$( echo $OPTARG | cut -d'-' -f1 );
#            endyyyymm=$( echo $OPTARG | cut -d'-' -f2 );;
#        h)  echo "$usage"; exit 0;;
#        c)  clean_dir; exit 0;;
#        w)  writetofile=1;; 
#        \?) die "Invalid option: -$OPTARG" >&2
#            exit 1;;
#        :)  die "Option -$OPTARG requires an argument." >&2
#            exit 1;;
#    esac
#done

! $optr && ! $optd && ! $optf && ! $opt_skiplst && blnrt=true || blnrt=false

#todo:  get positional inputs. 
shift $((OPTIND-1))
[[ "${1:-}" = "--" ]] && shift
arrposarg=( "$@" )
#================================================================================
#                               Check User Inputs
#================================================================================
srcf=${arrposarg[0]}
source $srcf
inputcheck

#================================================================================
#                             Set Host Specific Vars
#================================================================================
hstshort=$( get_host )
if [[ $hstshort == pfe ]];then
    :
elif [[ $hstshort == dis ]];then 
    
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

[[ -z $strexpid ]] && die "strexpid is undefined"
[[ -z $DRSTPROC1 || ! -d $DRSTPROC1 ]] && die "DRSTPROC1 are undefined and/or does not exist"
[[ -z $DRSTPROC2 || ! -d $DRSTPROC2 ]] && die "DRSTPROC2 are undefined and/or does not exist"
[[ -z $DARCHRST  ||    -z $strexpid ]] && die "DARCHRST or strexpid are not defined" 
#================================================================================
#                                 Set Variables
#================================================================================
#mid
[[ -n $rstexpid ]] && strexpid=$strexpid$rstexpid

dout=$cdir/output/$strscr/$strdout
dscratch=$dout/scratch
dmess=$cdir/message

ferr=$dmess/stderr_${strscr}
fwftmsz=wftmsz_flist
flstarc=$cdir/${strscr}_${strexpid}_arc_$fwftmsz
fnote=note_${strscr}_cont

msg_subject="${hstshort}.${strscr}: $strexpid"
fmessage=$dmess/message_${strscr}_$strexpid

tmpscr=rename.sh
blheader=false

#note:  max number of processes per run. Process only 20 per script 
#       execution instead of processing many at a time. 
maxproc=20
hh=21
hhmmss=${hh}0000

feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

#note: 2023/02/15 For now, this script will create tar files and let run_dis2lfe.sh transfer to lfe
#      When this is decided to be unnecessary, turn this to false
#blmktar=true
$blnrt && blmktar=false || blmktar=true

[[ -z $blgetlst ]] && blgetlst=true

[[ ! -d $dmess ]] && mkdir -p $dmess
[[ ! -d $dout  ]] && mkdir -p $dout
(( $writetofile == 1 )) && [[ ! -f $fmessage ]] && touch $fmessage

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
    if ! $blnrt;then    
        if $opt_skiplst;then 
            touch $flstarc
        else        
            sup ssh $HSTARCHRST test -d /u/$USER >>/dev/null 2>&1
            status_hst=$?
    
            (( $status_hst > 0 )) && exit
    
            get_fwftmsz $DARCHRST $flstarc
            [[ ! -f $flstarc ]] && exit 
        fi
    fi
fi


if $optd;then
    #todo:  get final ic dates for processing
    arricdate1=($( echo $yyyymmdd | xargs -i bash -c 'date -d "{} -1days" +%Y%m%d'))

    #todo:  check icdate
    arricdate=()
    for icdate in ${arricdate[@]};do
        blicdate=$( fcal_isicdate $icdate ) 
        $blicdate && arricdate+=( $icdate ) 
    done 

elif $optf;then
    arryyyymmdd=($( cat $fdata )) 
    #todo:  get final ic dates for processing
    arricdate=($( printf '%s\n' ${arryyyymmdd[@]} | xargs -i bash -c 'date -d "{} -1days" +%Y%m%d'))

elif $optr;then
    #todo:  get fcst date
    arricdate=($( get_icdates $begyyyymm $endyyyymm ))

elif $blnrt;then
    #todo:  without options, this code will figureout all forecasts from the last month and ones up to 
    #       today's date in the current month. 
    arryyyymmdd=($( s2sv3_nrtdates )) 
    arricdate=($( printf '%s\n' ${arryyyymmdd[@]} | xargs -i bash -c 'date -d "{} -1days" +%Y%m%d'))

    begyyyymm=$( echo ${arryyyymmdd[-1]} | cut -c1-6 )
    endyyyymm=$begyyyymm
    arricdate+=($( get_icdates $begyyyymm $endyyyymm ))

    arricdate=($( printf '%s\n' ${arricdate[@]} | sort -V | uniq ))

fi
#wmessage \@$LINENO
#thisarr=( ${arricdate[@]} ) 
#ahand_print ${thisarr[@]} 
#exit

#todo:  get final ic dates for processing
arrfinal=($( filter "${arricdate[@]}" ))

#todo:  debug mode stops here
$optb && ahand_print ${arrfinal[@]} && exit

#todo:  keep only maxproc
arrfinal=($( printf '%s\n' ${arrfinal[@]} | sort -V | head -$maxproc ))

#todo:  delete all remaining dir in scratch
printf '%s\n' ${arrfinal[@]} | xargs -i bash -c "test -d $dscratch/{} && rm -rf $dscratch/{}"

#todo:  rename individual restart file.
numnum=$( echo -n ${#arrfinal[@]} | wc -m )
cntproc=0
for icdate in ${arrfinal[@]};do
    
    icdate_yyyy=$( echo $icdate | cut -c1-4 )
    cntproc=$(( cntproc + 1 ))
    dicdate=$dscratch/$icdate
      dens1=$dicdate/$enscont
   
    #todo:  find restart 
    frst=$DRSTPROC1/restarts.e${icdate}_${hh}z.tar
    if [[ ! -f $frst ]];then 
        frst=$DRSTPROC2/Y$icdate_yyyy/restarts.e${icdate}_${hh}z.tar
        [[ ! -f $frst ]] && continue
    fi

    #todo:  check if frst is a symlink
    [[ -h $frst ]] && frst_original=$( readlink -f $frst ) && blsymlink=true || blsymlink=false
    
    ffinal=$dout/$icdate.tar
    
    fhand_newdir $dens1

    ! $blheader && msg_wheader && blheader=true

    wmessage "$( printf '%0'$numnum'g' $cntproc) of $( printf '%'$numnum'g' ${#arrfinal[@]})"
    wmessage "Location RST     : $frst"
    $blsymlink && wmessage "RST is linked to : $frst_original"
    wmessage "          Output : $ffinal"

    #todo: write create cap_restart
    echo "$icdate $hhmmss" >| $dens1/cap_restart
    echo "  Processed Date : $( date )" >| $dens1/$fnote
    echo "    Location RST : $frst" >> $dens1/$fnote
    $blsymlink && echo "RST is linked to : $frst_original" >> $dens1/$fnote
    tar -xf $frst -C $dens1/ 
    
    proc_atm 
 
    proc_ocn

    blready=$( exp_checknumrst $dens1 ) 
        
    ! $blready && wmessage "FAILED - not all rst files exist in $dens1" && rm -rf $dens1 && continue

    if $blnrt && [[ "$strexpid" == "fcst" ]];then
        #todo:  move output dir to DRST
        dfinal=$DRST/$icdate/$enscont
        [[ ! -d $DRST/$icdate ]] && mkdir -p $DRST/$icdate

        cp -rp $dens1 $dfinal
        wait

        blready=$( exp_checknumrst $dfinal ) 
        
        ! $blready && wmessage "FAILED - not all rst files exist in $dfinal" && rm -rf $dfinal

    fi

    if ! $blnrt ;then
        #+++++ cd to dscratch (start) +++++
        cd $dscratch
        tar --remove-files -cf $dout/$icdate.tar $icdate 
        status_tar=$?
        cd - >/dev/null
        #+++++ cd to dscratch ( end ) +++++

        (( $status_tar > 0 )) && wmessage "FAILED - compressing output" && rm -f $dout/$icdate.tar

        tar tf $dout/$icdate.tar >> /dev/null 2>&1
        status_tar=$?
        (( $status_tar > 0 )) && wmessage "FAILED - tar tf test" && rm -f $dout/$icdate.tar

        if (( $status_tar == 0 )) && [[ -f $dout/$icdate.tar ]] ;then 
            fsize=$( numfmt --to=iec $( stat --printf='%s' $dout/$icdate.tar ) )
            wmessage "Output File Size : $fsize"
            wmessage
        fi

    else
        rm -rf $ens1
    fi

done

#todo:  clean empty dir
#+++++ cd to dscratch (start) +++++
cd $dscratch
find * -type d -empty -delete 2>/dev/null 
cd - >/dev/null
#+++++ cd to dscratch ( end ) +++++

#todo:  send email
[[ -f $fmessage ]] && sendfmsg

#todo:  cleaning
[[ -f $flstarc ]] && rm -f $flstarc

exit





