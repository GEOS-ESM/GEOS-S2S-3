#!/usr/bin/env bash
inputcheck(){
    if $opty;then
        local _int_userinput=$( misc_isinteger $userinput_decyyyy )
        (( $_int_userinput > 0 )) && die "decade value has to be an integer"
    fi

    return
}

create_data(){
    local _scrid=$1;shift
    local _arryyyymm=( "$@" ) 

    if $opty;then 
        local _fdataout=$ddata/$_scrid/data_${_scrid}_${strexpid}_${strscr}_$decyyy
    else
        local _fdataout=$ddata/$_scrid/data_${_scrid}_${strexpid}_${strscr}
    fi

    #todo:  create parent dir and file
    if (( ${#_arryyyymm[@]} > 0 ));then 
        ! $blwheader && msg_wheader && blwheader=true

        [[ -f $_fdataout ]] && msg_newfile $_fdataout
        printf '%s\n' ${_arryyyymm[@]} >> $_fdataout
        (( $? == 0 )) && wmessage "$( stat --printf="%.${x1}y %${x2}s %n\n" $_fdataout | sort -V )"
    fi
    return
}

create_data_exception(){
    local _scrid=$1;shift
    local _strexcept=$1;shift
    local _blreverse=$1;shift
    local _arryyyymm=( "$@" ) 

    if $opty;then 
        local _fdataout=$ddata/$_scrid/data_${_scrid}_${strexpid}_${strscr}_$decyyy
    else
        local _fdataout=$ddata/$_scrid/data_${_scrid}_${strexpid}_${strscr}
    fi

    if (( ${#_arryyyymm[@]} > 0 )) ;then

        #todo:  create parent dir and file
        [[ ! -f $_fdataout ]] && install -D /dev/null $_fdataout || msg_newfile $_fdataout

        if [[ "$_strexcept" == "none" ]];then
            if $_blreverse;then
                printf '%s\n' ${_arryyyymm[@]} | sort -V -r >> $_fdataout
            else
                printf '%s\n' ${_arryyyymm[@]} >> $_fdataout
            fi

        else
            if $_blreverse;then
                printf '%s\n' ${_arryyyymm[@]} | grep -vE "$_strexcept" 2>/dev/null  | sort -V -r >> $_fdataout
            else
                printf '%s\n' ${_arryyyymm[@]} | grep -vE "$_strexcept" 2>/dev/null >> $_fdataout
            fi
        fi
        if [[ -s $_fdataout ]];then 
            ! $blwheader && msg_wheader && blwheader=true
            wmessage "$( stat --printf="%.${x1}y %${x2}s %n\n" $_fdataout | sort -V )"
        fi
    fi

    return
}

create_data_clean(){
    local _scrid=$1;shift
    local _arryyyymm=( "$@" ) 
    local _arryyyymm_rdy=()

    if $opty;then 
        local _fdataout=$ddata/$_scrid/data_${_scrid}_${strexpid}_${strscr}_$decyyy
    else
        local _fdataout=$ddata/$_scrid/data_${_scrid}_${strexpid}_${strscr}
    fi

    if (( ${#_arryyyymm[@]} > 0 )) ;then

        #todo:  create parent dir and file
        [[ ! -f $_fdataout ]] && install -D /dev/null $_fdataout || msg_newfile $_fdataout

        for _yyyymm in ${_arryyyymm[@]};do 

            local fstatus=$( printf '%s\n' ${arrfstatus[@]} | grep ${foutgetsatus_basename}$_yyyymm )
            local _arrstatus=$( cat $fstatus | cut -d' ' -f1 | sort -V | uniq )
            local thisstrstatus=$( echo ${_arrstatus[@]} | sed 's# #\|#g' )

            if [[ "$thisstrstatus" == "$strstatus_comp" ]];then 
                local _totdexp=$( cat $fstatus | wc -l ) 
                local _numfstdout=$( find $dmess/* -maxdepth 0 -type f -name "message_clean_fcst_${_yyyymm}*" 2>/dev/null | wc -l ) 

                #todo:  if there is stdout in dmess, check if all dexp are ready to be cleaned
                if (( $_totdexp == $_numfstdout ));then 
                    local _numrdy=$( grep "3 steps out of " $dmess/message_clean_${strexpid}_${_yyyymm}* 2>/dev/null | wc -l ) 
                    (( $_numrdy != $_totdexp )) && _arryyyymm_rdy+=( $_yyyymm ) 
                else
                    _arryyyymm_rdy+=( $_yyyymm ) 
                fi
            fi
        done

        if (( ${#_arryyyymm_rdy[@]} > 0 ));then 
            printf '%s\n' ${_arryyyymm_rdy[@]} >> $_fdataout

            if [[ -s $_fdataout ]];then 
                ! $blwheader && msg_wheader && blwheader=true
                wmessage "$( stat --printf="%.${x1}y %${x2}s %n\n" $_fdataout | sort -V )"
            fi
        else
            [[ -f $_fdataout ]] && rm -f $_fdataout
        fi
    fi

    return
}

create_data_pckwinners(){

    local _scrid=$1;shift
    local _strexcept=$1;shift
    local _arryyyymm=( "$@" ) 

    if $opty;then
        local _fdataout=$ddata/$_scrid/data_${_scrid}_${strexpid}_${strscr}_$decyyy
    else
        local _fdataout=$ddata/$_scrid/data_${_scrid}_${strexpid}_${strscr}
    fi

    local _arrfwin=($( find $dout_pckwinners/* -type f -name "${foutpckwinners_basename}*" -not -empty 2>/dev/null | sort -V )) 

    if $opty;then
        local _arrwinyyyymm=($( printf '%s\n' ${_arrfwin[@]} | xargs -i basename {} | cut -d'_' -f4 | grep -vE "$stryyyymm_cln" | grep $decyyy | sort -V )) 
    else
        local _arrwinyyyymm=($( printf '%s\n' ${_arrfwin[@]} | xargs -i basename {} | cut -d'_' -f4 | grep -vE "$stryyyymm_cln" | sort -V )) 
    fi

    local _strwin_except=$( echo ${_arrwinyyyymm[@]} | sed 's# #\|#g' )

    if [[ "$_strexcept" == "none" ]];then
        #_strexcept="$_strwin_except"
        local _arryyyymm_final=($( printf '%s\n' ${_arryyyymm[@]} ${_arrwinyyyymm[@]} | sort -V | uniq -u ))
    else
        _strexcept="$_strexcept|$_strwin_except"
        local _arryyyymm_final=($( printf '%s\n' ${_arryyyymm[@]} ${_arrwinyyyymm[@]} | grep -vE "$_strexcept" | sort -V | uniq -u ))
    fi

    #todo:  create parent dir and file
    if (( ${#_arryyyymm_final[@]} > 0 ));then 
        [[ -f $_fdataout ]] && msg_newfile $_fdataout

        ! $blwheader && msg_wheader && blwheader=true
        printf '%s\n' ${_arryyyymm_final[@]} >> $_fdataout

        (( $? == 0 )) && wmessage "$( stat --printf="%.${x1}y %${x2}s %n\n" $_fdataout | sort -V )"
    fi

    return
}

create_data_monitor(){
    local _scrid=$1;shift
    local _arryyyymm=( "$@" ) 

    if $opty;then
        local _fdataout=$ddata/$_scrid/data_${_scrid}_${strexpid}_${strscr}_$decyyy
        #message_monitor_fcst_19921222ens1_3mon_199212_PUnf
        local _arryyyymm_fmonitor=($( find $dmess/* -maxdepth 0 -type f -name "${fstdmonitor_basename}[1-2]???????ens?*_*" 2>/dev/null | xargs -i basename {} | cut -d'_' -f4 | cut -c1-6 | sort -V | uniq | grep $decyyy ))
    else
        local _fdataout=$ddata/$_scrid/data_${_scrid}_${strexpid}_${strscr}
        local _arryyyymm_fmonitor=($( find $dmess/* -maxdepth 0 -type f -name "${fstdmonitor_basename}[1-2]???????ens?*_*" 2>/dev/null | xargs -i basename {} | cut -d'_' -f4 | cut -c1-6 | sort -V | uniq ))
    fi
    
    local _arryyyymm_final=($( printf '%s\n'  ${_arryyyymm[@]} ${_arryyyymm_fmonitor[@]} | sort -V | uniq ))

    #todo:  create parent dir and file
    if (( ${#_arryyyymm_final[@]} > 0 ));then 
        ! $blwheader && msg_wheader && blwheader=true

        [[ -f $_fdataout ]] && msg_newfile $_fdataout
        printf '%s\n' ${_arryyyymm_final[@]} >> $_fdataout
        (( $? == 0 )) && wmessage "$( stat --printf="%.${x1}y %${x2}s %n\n" $_fdataout | sort -V )"
    fi

    return
}

create_data_doover(){
    local _scrid=$1;shift
    local _fdataout=$ddata/$_scrid/data_${_scrid}_${strexpid}_${strscr}
    
    [[ ! -f $_fdataout ]] && install -D /dev/null $_fdataout || msg_newfile $_fdataout

    find $DFCST/* -maxdepth 1 -mindepth 1 -type d -name 'ens*' 2>/dev/null | xargs -i bash -c '[[ ! -f {}/clean_completed && ! -f {}/archive/gcmarch_deloutcompleted ]] && realpath {} ' | xargs -i bash -c '[[ -f {}/archive/note_gcmarch ]] && echo {}' | sort -V >> $_fdataout

    [[ -f $_fdataout ]] && wmessage "$( stat --printf="%.${x1}y %${x2}s %n\n" $_fdataout | sort -V )"
    return
} 

create_data_gcmsetup(){
    local _scrid=$1
    local _yyyy_excpet81=1981
    local _yyyy_excpet91=1991
    local _numdexptot_except=40

    if $opty;then 
        local _fdataout=$ddata/$_scrid/data_${_scrid}_${strexpid}_${strscr}_$decyyy
        local _cyyyy=($( find $DFCST/* -maxdepth 0 -type d -name "[1-2]???????" 2>/dev/null | xargs -i basename {} | cut -c1-4 | sort -V | uniq | grep $decyyy | tail -1 ))
    else
        local _fdataout=$ddata/$_scrid/data_${_scrid}_${strexpid}_${strscr}
        local _cyyyy=($( find $DFCST/* -maxdepth 0 -type d -name "[1-2]???????" 2>/dev/null | xargs -i basename {} | cut -c1-4 | sort -V | uniq | tail -1 ))
    fi

    #+++++ cd to dfcst (start) +++++
    cd $DFCST
    local _numdexp=$( find * -maxdepth 1 -mindepth 1 -path "${_cyyyy}*" -type d -name 'ens*' 2>/dev/null | wc -l )
    cd - >/dev/null
    #+++++ cd to dfcst ( end ) +++++
   
    if (( $_numdexp == $numtotdexp_year ));then
        local _yyyy=$(( _cyyyy + 1 ))
    elif [[ -n $_cyyyy ]] && (( $_cyyyy == $_yyyy_excpet81 && $_numdexp == $_numdexptot_except ));then 
        local _yyyy=$(( _cyyyy + 1 ))
    elif [[ -n $_cyyyy ]] && (( $_cyyyy == $_yyyy_excpet91 && $_numdexp == $_numdexptot_except ));then 
        local _yyyy=$(( _cyyyy + 1 ))
    else
        local _yyyy=$_cyyyy
    fi
    
    #todo:  create parent dir and file
    if [[ -n $_yyyy ]];then 
        ! $blwheader && msg_wheader && blwheader=true
        [[ -f $_fdataout ]] && msg_newfile $_fdataout
        echo $_yyyy >> $_fdataout
        (( $? == 0 )) && wmessage "$( stat --printf="%.${x1}y %${x2}s %n\n" $_fdataout | sort -V )"
    fi
    
    return
}

create_data_miscdexpsize(){
    local _scrid=$1
    local _dscrid=$( echo $_scrid | cut -c1-4 )
    local _fscrid=$( echo $_scrid | cut -c5- )
    local _ftmp_dexp=$ddata/$_dscrid/tmp_${_fscrid}_dexp
    local _scrname=${strscr}_${_fscrid}
    local _fdataout=$ddata/$_dscrid/data_${_fscrid}_${strexpid}_${strscr}


    screen -ls | grep $_scrname >/dev/null 2>&1
    local _status_grep=$?
    if (( $_status_grep == 0 ));then 
        wmessage "*Screen session, $_scrname, is already running for making $( basename $_fdataout )." 
    else
        [[ ! -f $_fdataout  ]] && install -D /dev/null $_fdataout || msg_newfile $_fdataout
        [[   -f $_ftmp_dexp ]] && rm -f $_ftmp_dexp

        find $DFCST/* -maxdepth 1 -mindepth 1 -type d -name 'ens*' 2>/dev/null  | xargs -i bash -c "[[ ! -f {}/clean_completed ]] && realpath {}" | sort -V > $_ftmp_dexp

        #note:  reverse the order to start from the newest dexp
        screen -dmS $_scrname bash -c "cat $_ftmp_dexp | sort -r | xargs -i du -d 0 {} >> $_fdataout 2>&1"

        screen -ls | grep $_scrname >/dev/null 2>&1
        local _status_grep=$?
        (( $_status_grep == 0 )) && wmessage "*Screen session, $_scrname, started to run for making $( basename $_fdataout )." 
    fi

    return 
}

_create_data_quickarch(){
    #Note: Remove this function  after 06/27/2023
    #description:   get dexp from archstatus outputs. 
    local _scrid=$1
    local _scrid_data=getstatus
    local _lim=5

    [[ -z $_scrid ]] && wmessage "(${FUNCNAME[0]}) undefined var: _scrid"
    local _ddatastatus=$cdir/output/$_scrid_data/$strexpid
    local _arrfdataarcs=($( find $_ddatastatus/* -maxdepth 0 -type f -name "data_${_scrid_data}_${strexpid}_list_*" ))
    local _fdataout=$ddata/$_scrid/data_${_scrid}_${strexpid}_${strscr}
    local _strsearch="AUnf"
    local _arrdexp=($( cat ${_arrfdataarcs[@]} | grep $_strsearch | cut -d' ' -f2 | sort -V | uniq | head -n 5 ))

    if (( ${#_arrdexp[@]} > 0 ));then 
        ! $blwheader && msg_wheader && blwheader=true
        [[ ! -f $_fdataout ]] && install -D /dev/null $_fdataout || msg_newfile $_fdataout
        printf '%s\n' ${_arrdexp[@]} | sort -V | uniq | head - >> $_fdataout
        (( $? == 0 )) && wmessage "$( stat --printf="%.${x1}y %${x2}s %n\n" $_fdataout | sort -V )"
    fi

    return
}

create_data_quickarch(){
    local _scrid=$1
    local _scrid_data=getstatus

    [[ -z $_scrid ]] && wmessage "(${FUNCNAME[0]}) undefined var: _scrid"

    local _ddatagetstatus=$cdir/output/$_scrid_data/$strexpid
    local _arrfdataarcs=($( find $_ddatagetstatus/* -maxdepth 0 -type f -name "data_${_scrid_data}_${strexpid}_list_*" ))
    local _fdataout=$ddata/$_scrid/data_${_scrid}_${strexpid}_${strscr}
    local _strsearch1="AUnf"   
    local _arrdexp=()
    local _fdataarcs

    _arrdexp=($( printf '%s\n' ${_arrfdataarcs[@]} | xargs -i bash -c "grep -i "$_strsearch1" {}" | cut -d' ' -f2 )) 

    _arrdexp=($( printf '%s\n' ${_arrdexp[@]} | xargs -i bash -c "[[   -d {}/archive       ]] && echo {}" ))
    _arrdexp=($( printf '%s\n' ${_arrdexp[@]} | xargs -i bash -c "[[ ! -f {}/archive/$fdel ]] && echo {}" | sort -V | uniq ))

    if (( ${#_arrdexp[@]} > 0 ));then 
        ! $blwheader && msg_wheader && blwheader=true
        [[ ! -f $_fdataout ]] && install -D /dev/null $_fdataout || msg_newfile $_fdataout
        printf '%s\n' ${_arrdexp[@]} | sort -V | uniq >> $_fdataout
        (( $? == 0 )) && wmessage "$( stat --printf="%.${x1}y %${x2}s %n\n" $_fdataout | sort -V )"
    fi

    return
}

clean_dir() {
    [[ -n $flock && -f $flock ]] && rm -f $flock
    return
}

#================================================================================
#                                     Begin
#================================================================================
#starthere
hst=$( hostname )
blnode=false
if [[ "$hst" =~ "pfe"* ]];then
    :
elif [[ "$hst" =~ "r"* ]];then
    #note:  this is for host other than pfe @ NAS which has a name starting with 'r'
    blnode=true
elif [[ "$hst" =~ "discover"* ]];then
    echo "this script works only on pfe (for now)";exit
else
    exit
fi

strscr=$(basename "$0" | cut -d'_' -f2 | cut -d'.' -f1 )
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cdir=$SCRIPTPATH

[[ -z $cdir || ! -d $cdir ]] && exit

cd $cdir

flock=$cdir/${strscr}.lock
ftmp=$cdir/stdout/rundatetime/tmp_$strscr

#todo:  check tmp file size and create new if it is larger than 5kb
if [[ ! -f $ftmp ]];then touch $ftmp;fi
stmp=$( find $ftmp -printf "%s\n" )
if (( $stmp > 5000 ));then
    rm -f $ftmp;touch $ftmp
fi

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

trap clean_dir EXIT

if [[ ! $cdir/func_fcst.sh ]];then 
    echo "$cdir/func_fcst.sh does not exist";exit
else
    source $cdir/func_fcst.sh
fi

writetofile=0
opty=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        make data files for various scripts. The files are used mainly in run_run*.sh.

        Usage: ./$(basename "$0") [-chw] [-y YYYY] [ -f datafile] srcme_file
        
        input:        
            A source file, which set various vars for a set of runs (i.e. srcme_pfe_*)

        options:
            -y  decade (i.e. 1980 1990 2000 )
            -c  clean unwanted files
            -h  show this help text
            -w  write stdout/err in a file
"

OPTIND=1
while getopts ':hcwf:y:' option; do
    case "$option" in
        y)  opty=true; userinput_decyyyy=$OPTARG;;
        h)  echo "$usage"; exit 0;;
        c)  clean_dir; exit 0;;
        w)  writetofile=1;; 
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
#================================================================================
#                               Check User Inputs
#================================================================================
i=0 
    strf=${arrposarg[i]}; i=$(( i + 1 ))
[[ -n $strf && -f $strf ]] && source $strf
inputcheck
#================================================================================
#                                 Set Variables
#================================================================================
#mid
[[ -n $rstexpid ]] && strexpid=$strexpid$rstexpid

hstshort=$( get_host )
dmess=$cdir/message
dout_getstatus=$cdir/output/getstatus/$strdout
dout_pckwinners=$cdir/output/pckwinners/$strdout
ddata=$cdir/data

fmessage=$dmess/message_$strscr
ferr=$dmess/stderr_$strscr
foutgetsatus_basename=data_getstatus_${strexpid}_list_
#winners_nino3.4_fcst_199403_199405.txt
foutpckwinners_basename=winners_nino3.4_${strexpid}_
fstdmonitor_basename=message_monitor_${strexpid}_
blwheader=false
blrm=false

arrstatus_done=( C3 C3m WC10 C3C WC10C )
arrstatus_cleaned=( C3C WC10C )
arrstatus_comp=( C3 WC10 )

strstatus_cleaned=$( echo ${arrstatus_cleaned[@]} | sed 's# #\|#g' )
   strstatus_comp=$( echo ${arrstatus_comp[@]}    | sed 's# #\|#g' )

numtotdexp_year=485

arrscrid=( monitor gcmsetup archstatus arch pckwinners quickarch )
arrscrid+=( utility_compress_sst utility_getwtime clean )
arrscrid+=( miscdexpsize ) 

#note:  these are exceptions for particular scripts
#strexcept_arch="1992|1993|1994|1995"
#strexcept_archstatus="1992|1993|1994|1995"
#strexcept_pckwinners="1990|1991|1992|1993|1994|1995"
strexcept_archstatus="none"
strexcept_arch="none"
strexcept_pckwinners="none"

x1=16;x2=6

msg_subject="${hstshort}.${strscr}: ${strexpid}"
feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

[[ ! -d $dmess    ]] && mkdir -p $dmess
[[ ! -f $fmessage ]] && touch $fmessage

#================================================================================
#                                  Main Process
#================================================================================
#letsbegin
if $opty;then 
    decyyy=$( echo $userinput_decyyyy | cut -c1-3 )
    msg_subject="${msg_subject} ($decyyy)" 
else
    decyyy=
fi

#todo:  get YYYYMM, which has not completed cleaning, from getstatus list files. 
arrfstatus=($( find $dout_getstatus/* -type f -name "${foutgetsatus_basename}${decyyy}*" 2>/dev/null )) 
        
#ahand_print ${arrfstatus[@]} 
#exit


if (( ${#arrfstatus[@]} == 0 ));then 
    arryyyymm=()
    #note:  gcmsetup is only one that does not require arryyyymm array
    arrscrid=( gcmsetup ) 
else
    #note:  use this array for monitor, arch, and archstatus
    arryyyymm_unfin=($( cat ${arrfstatus[@]} | grep -wvE "$strstatus_cleaned" | cut -d' ' -f2 | rev | cut -d'/' -f2 | rev | cut -c1-6 | sort -V | uniq | grep -v variab ))

    #note:  array for the rest of processes
    arryyyymm_cln=($( cat ${arrfstatus[@]} | grep -wE  "$strstatus_cleaned" | cut -d' ' -f2 | rev | cut -d'/' -f2 | rev | cut -c1-6 | sort -V | uniq | grep -v variab )) 
    stryyyymm_cln=$( echo ${arryyyymm_cln[@]} | sed 's# #\|#g' )

    #todo:  remove all cleaned yyyymm
    arryyyymm=($( cat ${arrfstatus[@]} | grep -w -vE "$strstatus_cleaned" | cut -d' ' -f2 | rev | cut -d'/' -f2 | rev | cut -c1-6 | sort -V | uniq | grep -v variab )) 
    arryyyymm=($( printf '%s\n' ${arryyyymm[@]} | grep -w -vE "$stryyyymm_cln" )) 

    ##todo:  add missing yyyymm in case getstatus script is slow creating list files.
    #yyyy_last=$( echo ${arryyyymm[-1]} | cut -c1-4 )
    #arryyyymm1=($( find $DFCST/* -maxdepth 1 -mindepth 1 -type d -name 'ens*' | grep $yyyy_last | rev | cut -d'/' -f2 | rev | cut -c1-6 | sort -V | uniq ))
    #arryyyymm=($( printf '%s\n' ${arryyyymm[@]} ${arryyyymm1[@]} | sort -V | uniq ))
fi
   
#wmessage \@$LINENO 
#ahand_print ${arryyyymm_unfin[@]} 
#wmessage
##ahand_print ${arryyyymm[@]} 
##wmessage
##ahand_print ${arryyyymm_cln[@]} 
##exit
#arrscrid=( arch ) 

#todo:  create array with output filesname
for scrid in ${arrscrid[@]};do
    if [[ "$scrid" == "arch" ]];then
        blreverseorder=false
        create_data_exception $scrid $strexcept_arch $blreverseorder ${arryyyymm_unfin[@]} 
    elif [[ "$scrid" == "archstatus" ]];then
        blreverseorder=false
        create_data_exception $scrid $strexcept_archstatus $blreverseorder ${arryyyymm_unfin[@]} 
    elif [[ "$scrid" == "pckwinners" || "$scrid" == "utility_compress_sst" ]];then
        create_data_pckwinners $scrid $strexcept_pckwinners ${arryyyymm[@]} 
    elif [[ "$scrid" == "monitor" ]];then
        create_data_monitor $scrid ${arryyyymm_unfin[@]} 
    elif [[ "$scrid" == "gcmsetup" ]];then
        create_data_gcmsetup $scrid 
    elif [[ "$scrid" == "quickarch" ]];then
        create_data_quickarch $scrid
    elif [[ "$scrid" == "utility_getwtime" ]];then
        blreverseorder=false
        create_data_exception $scrid $strexcept_arch $blreverseorder ${arryyyymm[@]} 
    elif [[ "$scrid" == "miscdexpsize" ]];then
        create_data_miscdexpsize $scrid 
    elif [[ "$scrid" == "clean" ]];then
        create_data_clean $scrid ${arryyyymm[@]}  
    #elif [[ "$scrid" == "doover" ]];then
    #    create_data_doover $scrid 
    else
        create_data $scrid ${arryyyymm[@]}
    fi
done

#todo:  send email
[[ -z $eadds ]] && exit

if $blnode || $RUN_BY_CRON ;then

    msg_hrinc=12
    msg_hrpref=6

    [[ -f $ferr     ]] && sizeferr=$( stat --print='%s' $ferr     ) || sizeferr=0
    [[ -f $fmessage ]] &&    sizef=$( stat --print='%s' $fmessage ) || sizef=0

    if (( $sizeferr > 0 ));then 
        msg_wheader_userdefined 40 "-" $( basename $ferr ) 
        wmessage "$( cat $ferr )"
        blrm=true
    fi

    msg_cyberpostman "$msg_subject" "$eadds" $fmessage
    status_email=$?
    (( $status_email == 0 )) && rm -f $fmessage
    $blrm && msg_newfile $ferr
fi

exit
