#!/usr/bin/env bash

inputcheck() {

    [[ -z  ${arrposarg[@]} ]] && die "an input is missing"
    (( ${#arrposarg[@]} > 1 )) && die "there are more than 1 input"
    return
}

del_stuff(){
    
    #todo:  delete files 
    del_files

    #todo:  delete extr dir
    del_dir

    return
}

del_dir(){
    
    local arrinput=($( find $dextr/* -maxdepth 0 -type d 2>/dev/null | xargs -i basename {}  ))
    local arrfinal=($( seas_allrstpertexist $flstarc ${arrinput[@]} ))

    if (( ${#arrfinal[@]} > 0 ));then 
        cd $dextr
        ! $blmsg && msg_wheader && blmsg=true
        wmessage "... ${#arrfinal[@]} directories in output/rstextr/$strdout will be deleted"
        printf '%s\n' ${arrfinal[@]} | column 
        wmessage
        rm -rf ${arrfinal[@]} 2>/dev/null
        cd - >/dev/null
    fi

    return
}

del_files(){
    local flstarc_cont=${flstarc}_cont
    local flstarc_pert=${flstarc}_pert

    #todo:  create lfe file lists. one for perturbation,
    #       another for control 
    #note:  ">|" overwrite a file when noclobber is set
    #ref:   https://stackoverflow.com/a/35589472
    grep -E  '_pert.tar|_pert_ocnens6to10.tar' $flstarc >| $flstarc_pert
    grep -vE '_pert.tar|_pert_ocnens6to10.tar' $flstarc >| $flstarc_cont
    
    #todo:  get exisiting files on lfe
    local arrfexist_cont=($( cat $flstarc_cont | sort | rev | cut -d' ' -f1 | rev | xargs -i basename {} ))
    local arrfexist_pert=($( cat $flstarc_pert | sort | rev | cut -d' ' -f1 | rev | xargs -i basename {} ))

    local arrfexist_cont_dis=($( printf '%s\n' ${arrfexist_cont[@]} | xargs -i bash -c "[[ -f $dcont/{} ]] && echo {}" )) 
    local arrfexist_pert_dis=($( printf '%s\n' ${arrfexist_pert[@]} | xargs -i bash -c "[[ -f $dpert/{} ]] && echo {}" )) 

    #todo:  removed files, which exist on lfe, from discover 
    if (( ${#arrfexist_cont_dis[@]} > 0 ));then 
        cd $dcont
        ! $blmsg && msg_wheader && blmsg=true
        wmessage "... ${#arrfexist_cont_dis[@]} control files will be deleted"
        printf '%s\n' ${arrfexist_cont_dis[@]} | column 
        wmessage
        rm -f ${arrfexist_cont_dis[@]} 2>/dev/null
        cd - >/dev/null
    fi
    
    if (( ${#arrfexist_pert_dis[@]} > 0 ));then 
        cd $dpert
        ! $blmsg && msg_wheader && blmsg=true
        wmessage "... ${#arrfexist_pert_dis[@]} perturbation files will be deleted"
        printf '%s\n' ${arrfexist_pert_dis[@]} | column 
        wmessage
        rm -f ${arrfexist_pert_dis[@]} 2>/dev/null
        cd - >/dev/null
    fi

    rm -f $flstarc_pert 2>/dev/null
    rm -f $flstarc_cont 2>/dev/null
    rm -f $flistlfe 2>/dev/null

    return
}

rst_ckfsize_cont(){
    #description:  check files size of control restart save on lfe 

    local _bluseflst=$1
    local _ftmp=_tmp_${strscr}_${FUNCNAME[0]}
    local _arr_ftmp=()
    local _numlnmax_default=999999999999 
    local _numlnmin_default=0
    local _fsizeprob _fsizeexpected_cont _fsize_expected_cont

    [[ -f $_ftmp ]] && rm -f $_ftmp 

    #todo:   get file size on lfe. 
    #        ICYYYYMMDD.tar should be 25G 
    if $_bluseflst;then 
        if [[ -f $flstarc ]];then
            local _arr_fsizeexpected_cont=($( cat $flstarc | grep -v _pert | cut -d' ' -f2 | sort -V | uniq | xargs -i numfmt --to=iec {} | sort -V | uniq ))
        else
            local _arr_fsizeexpected_cont=()
        fi
    else
        local _arr_fsizeexpected_cont=($( find $dcont/* -maxdepth 0 -type f -name "[1-2]???????.tar" 2>/dev/null | xargs -i stat --printf='%s %n\n' {} | numfmt --field=1 --to=iec | cut -d' ' -f1 | sort -V | uniq ))
    fi

    #todo:  check on control rst
    if (( ${#_arr_fsizeexpected_cont[@]} == 1 ));then 
        _fsize_expected_cont=${_arr_fsizeexpected_cont[0]}

    else            
        #todo:  divide filenames based on file size and write them into a separate tmp file.
        local _numlnmin=$_numlnmin_default
        for _fsizeexpected_cont in ${_arr_fsizeexpected_cont[@]};do

            [[ -f $_thisftmp ]] && rm -f $_thisftmp

            local _thisftmp=${_ftmp}_$_fsizeexpected_cont
            if $_bluseflst;then
                cat $flstarc | grep -v _pert.tar | numfmt --field=2 --to=iec --padding=5 | grep $_fsizeexpected_cont | tr -s '[:space:]' >| $_thisftmp
            else
                find $dcont/* -maxdepth 0 -type f -name "[1-2]???????.tar" 2>/dev/null | xargs -i stat --printf='%s %n\n' {} | numfmt --field=1 --to=iec | grep $_fsizeexpected_cont >| $_thisftmp
            fi


            #todo:  figure out expected filesize
            local _numln=$( cat $_thisftmp | wc -l )
            if (( $_numln > $_numlnmin ));then 
                local _numlnmin=$_numln
                _fsize_expected_cont=$_fsizeexpected_cont

            elif (( $_numln == $_numlnmin ));then 
                local _numlnmin=$_numlnmin_default
                _fsize_expected_cont=
            fi

            [[ -f $_thisftmp ]] && rm -f $_thisftmp

        done 
           
        #todo:  get files that have file size problems.
        if [[ -n $_fsize_expected_cont ]];then 
            local _arr_fsizeprob=($( printf '%s\n' ${_arr_fsizeexpected_cont[@]} | grep -v $_fsize_expected_cont )) 
            for _fsizeprob in ${_arr_fsizeprob[@]};do
                if $_bluseflst;then 
                    cat $flstarc | grep -v _pert.tar | numfmt --field=2 --to=iec --padding=5 | grep $_fsizeprob | tr -s '[:space:]' | cut -d' ' -f2- >> $_ftmp
                else
                    find $dcont/* -maxdepth 0 -type f -name "[1-2]???????.tar" 2>/dev/null | xargs -i stat --printf='%s %n\n' {} | numfmt --field=1 --to=iec | grep $_fsizeexpected_cont >> $_ftmp
                fi
            done
        fi
    fi

    if [[ -n $_fsize_expected_cont && -f $_ftmp ]];then 
        echo "$_fsize_expected_cont:$_ftmp"
    elif [[ -n $_fsize_expected_cont ]];then 
        echo $_fsize_expected_cont
    else
        echo 
    fi
}

rst_ckfsize_pert(){
    #description:  check files size of perturbation restart (both atm and ocn) save on lfe 

    local _bluseflst=$1
    local _ftmp=_tmp_${strscr}_${FUNCNAME[0]}
    local _arr_ftmp=()
    local _numlnmax_default=999999999999 
    local _numlnmin_default=0
    local _fsizeprob _fsizeexpected_atmpert _fsizeexpected_ocnpert t_fsize_expected_atmpert _fsize_expected_ocnpert

    [[ -f $_ftmp ]] && rm -f $_ftmp 

    #todo:   get file size of perturbation rst on lfe. 
    #        ICYYYYMMDD*_pert.tar ( atm pert rst) should be 4.9G
    #        ICYYYYMMDD*_pert.tar ( ocn pert rst) should be 31G
    if $_bluseflst;then 
        if [[ -f $flstarc ]];then 
            local _arr_fsize_atmpert=($( cat $flstarc | grep _pert | grep -Ev "$strocnicmmdd" | cut -d' ' -f2 | sort -V | uniq | xargs -i numfmt --to=iec {} | sort -V | uniq ))
            local _arr_fsize_ocnpert=($( cat $flstarc | grep _pert | grep -E  "$strocnicmmdd" | cut -d' ' -f2 | sort -V | uniq | xargs -i numfmt --to=iec {} | sort -V | uniq ))
        else
            local _arr_fsize_atmpert=()
            local _arr_fsize_ocnpert=()
        fi
    else
        local _arr_fsize_atmpert=($( find $dpert/* -maxdepth 0 -type f -name "[1-2]???????_pert.tar" 2>/dev/null | grep -Ev "$strocnicmmdd" | xargs -i stat --printf='%s %n\n' {} | numfmt --field=1 --to=iec | cut -d' ' -f1 | sort -V | uniq ))
        local _arr_fsize_ocnpert=($( find $dpert/* -maxdepth 0 -type f -name "[1-2]???????_pert.tar" 2>/dev/null | grep -E  "$strocnicmmdd" | xargs -i stat --printf='%s %n\n' {} | numfmt --field=1 --to=iec | cut -d' ' -f1 | sort -V | uniq ))
    fi

    #todo:  check on atm pert rst
    if (( ${#_arr_fsize_atmpert[@]} == 0 ));then 
        _fsize_expected_atmpert="nan"
    elif (( ${#_arr_fsize_atmpert[@]} == 1 ));then 
        _fsize_expected_atmpert=${_arr_fsize_atmpert[0]}
    else            
        #todo:  divide filenames based on file size and write them into a separate tmp file.
        local _numlnmin=$_numlnmin_default
        for _fsizeexpected_atmpert in ${_arr_fsize_atmpert[@]};do
            local _thisftmp=${_ftmp}_$_fsizeexpected_atmpert
            if $_bluseflst;then 
                cat $flstarc | grep _pert.tar | numfmt --field=2 --to=iec --padding=5 | grep $_fsizeexpected_atmpert | tr -s '[:space:]' >| $_thisftmp
            else
                find $dpert/* -maxdepth 0 -type f -name "[1-2]???????_pert.tar" 2>/dev/null | grep -Ev "$strocnicmmdd" | xargs -i stat --printf='%s %n\n' {} | numfmt --field=1 --to=iec | grep $_fsizeexpected_atmpert | tr -s '[:space:]' >| $_thisftmp
            fi

            #todo:  figure out expected filesize
            local _numln=$( cat $_thisftmp | wc -l )
            if (( $_numln > $_numlnmin ));then 
                local _numlnmin=$_numln
                _fsize_expected_atmpert=$_fsizeexpected_atmpert

            elif (( $_numln == $_numlnmin ));then 
                local _numlnmin=$_numlnmin_default
                _fsize_expected_atmpert="nan"
            fi

            [[ -f $_thisftmp ]] && rm -f $_thisftmp

        done 
           
        #todo:  get files that have file size problems.
        if [[ -n $_fsize_expected_atmpert && "$_fsize_expected_atmpert" != "nan" ]];then 
            local _arr_fsizeprob=($( printf '%s\n' ${_arr_fsize_atmpert[@]} | grep -v $_fsize_expected_atmpert )) 
            for _fsizeprob in ${_arr_fsizeprob[@]};do
                if $_bluseflst;then 
                    cat $flstarc | grep _pert.tar | numfmt --field=2 --to=iec --padding=5 | grep $_fsizeprob | tr -s '[:space:]' >> $_ftmp
                else
                    find $dpert/* -maxdepth 0 -type f -name "[1-2]???????_pert.tar" 2>/dev/null | xargs -i stat --printf='%s %n\n' {} | numfmt --field=1 --to=iec | grep $_fsizeexpected_atmpert >> $_ftmp
                fi
            done
        fi
    fi
    
    #todo:  check on ocn pert rst
    if (( ${#_arr_fsize_ocnpert[@]} == 0 ));then 
        _fsize_expected_ocnpert="nan"
    elif (( ${#_arr_fsize_ocnpert[@]} == 1 ));then 
        _fsize_expected_ocnpert=${_arr_fsize_ocnpert[0]}

    else            
        #todo:  divide filenames based on file size and write them into a separate tmp file.
        local _numlnmin=$_numlnmin_default
        for _fsizeexpected_ocnpert in ${_arr_fsize_ocnpert[@]};do
            local _thisftmp=${_ftmp}_$_fsizeexpected_ocnpert
            if $_bluseflst;then 
                cat $flstarc | grep _pert.tar | numfmt --field=2 --to=iec --padding=5 | grep $_fsizeexpected_ocnpert | tr -s '[:space:]' >| $_thisftmp
            else
                find $dpert/* -maxdepth 0 -type f -name "[1-2]???????_pert.tar" 2>/dev/null | grep -Ev "$strocnicmmdd" | xargs -i stat --printf='%s %n\n' {} | numfmt --field=1 --to=iec | grep $_fsizeexpected_atmpert | tr -s '[:space:]' >| $_thisftmp
            fi

            #todo:  figure out expected filesize
            local _numln=$( cat $_thisftmp | wc -l )
            if (( $_numln > $_numlnmin ));then 
                local _numlnmin=$_numln
                _fsize_expected_ocnpert=$_fsizeexpected_ocnpert

            elif (( $_numln == $_numlnmin ));then 
                local _numlnmin=$_numlnmin_default
                _fsize_expected_ocnpert="nan"
            fi

            [[ -f $_thisftmp ]] && rm -f $_thisftmp

        done 
           
        #todo:  get files that have file size problems.
        if [[ -n $_fsize_expected_ocnpert && "$_fsize_expected_ocnpert" != "nan" ]];then 
            local _arr_fsizeprob=($( printf '%s\n' ${_arr_fsize_ocnpert[@]} | grep -v $_fsize_expected_ocnpert )) 

            for _fsizeprob in ${_arr_fsizeprob[@]};do
                if $_bluseflst;then 
                    cat $flstarc | grep _pert.tar | numfmt --field=2 --to=iec --padding=5 | grep $_fsizeprob | tr -s '[:space:]' >> $_ftmp
                else
                    find $dpert/* -maxdepth 0 -type f -name "[1-2]???????_pert.tar" 2>/dev/null | xargs -i stat --printf='%s %n\n' {} | numfmt --field=1 --to=iec | grep $_fsizeprob  >> $_ftmp
                fi
            done
        fi
    fi
 
    if [[ -n $_fsize_expected_atmpert  && -n $_fsize_expected_ocnpert && -f $_ftmp ]];then 
        echo "$_fsize_expected_atmpert:$_fsize_expected_ocnpert:$_ftmp"
    elif [[ -n $_fsize_expected_atmpert && -n $_fsize_expected_ocnpert ]];then 
        echo "$_fsize_expected_atmpert:$_fsize_expected_ocnpert"
    else
        echo 
    fi

}

rst_ckfsize(){
    #description:   file size check for files on lfe AND output/rstmkpert and output/rstprep 
    #               before transferred to lfe.

    #todo:  check control rst file size on lfe and dis
    local arrout1=($(  echo $( rst_ckfsize_cont true  ) | sed 's#:# #g' ))
          arrout1+=($( echo $( rst_ckfsize_cont false ) | sed 's#:# #g' ))
    local arrout=($( printf '%s\n' ${arrout1[@]} | sort -V | uniq ))          

    if (( ${#arrout[@]} == 2 ));then 
        local i=0
        #global variable
        fsize_expected_cont=${arrout[i]};i=$(( i + 1 ))
        local fcont_prob=${arrout[i]}
    
    elif (( ${#arrout[@]} == 1 ));then 
        local i=0
        #global variable
        fsize_expected_cont=${arrout[i]}
        local fcont_prob=
    fi
    
    if [[ -n $fcont_prob && -f $fcont_prob ]];then 
       cat  $fcont_prob >> $fprob
       rm -f $fcont_prob
    fi
    

    #todo:  check perturbation rst file size
    local arrout1=($(  echo $( rst_ckfsize_pert true  ) | sed 's#:# #g' ))
          arrout1+=($( echo $( rst_ckfsize_pert false ) | sed 's#:# #g' ))
    local arrout=($( printf '%s\n' ${arrout1[@]} | grep -v "nan" | sort -V | uniq ))          

    if (( ${#arrout[@]} == 3 ));then 
        local i=0
        #global variables
        fsize_expected_atmpert=${arrout[i]};i=$(( i + 1 ))
        fsize_expected_ocnpert=${arrout[i]};i=$(( i + 1 ))
        local       fpert_prob=${arrout[i]}
    
    elif (( ${#arrout[@]} == 2 ));then 
        local i=0
        #global variables
        fsize_expected_atmpert=${arrout[i]};i=$(( i + 1 ))
        fsize_expected_ocnpert=${arrout[i]}
        local fpert_prob=
    fi
    
    if [[ -n $fpert_prob && -f $fpert_prob ]];then 
        cat $fpert_prob >> $fprob
        rm -f $fpert_prob
    fi
    
    return
}

sendfmsg() {
    #description:   send email
    local _blrm=false
    
    [[ -f $ferr     ]] && local _sizeferr=$( stat --print='%s' $ferr )  || local _sizeferr=0

    if [[ -f $fmessage ]];then 
        local _sizef=$( stat --print='%s' $fmessage )        
        if (( $_sizef > 0 ));then 
            wmessage "Time Ended - $( TZ=America/New_York date +'%m/%d/%Y %H:%M' )"
        fi

    else
        local _sizef=0
    fi


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
    [[ -n ${arrfrm[@]} ]] && rm -f ${arrfrm[@]}
    
    #todo:  delete rstextr dir    
    [[ -n $flstarc && -f $flstarc ]] && rm -f $flstarc

    return
}

#================================================================================
#                                     Begin
#================================================================================
#starthere
hst=$( hostname )
[[ $hst == 'pfe'* ]] && echo "this script works only on discover" && exit

strscr=$(basename "$0" | cut -d'_' -f2 | cut -d'.' -f1 )
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cdir=$SCRIPTPATH

[[ -z $cdir || ! -d $cdir ]] &&  echo "cdir (parent directory of this script) is undefined"  && exit

cd $cdir

flock=$cdir/${strscr}.lock
ftmp=$cdir/stdout/rundatetime/tmp_$strscr
[[ ! -f $ftmp ]] && install -D /dev/null $ftmp

#todo:  check tmp file size and create new if it is larger than 5kb
stmp=$( find $ftmp -printf "%s\n" )
(( $stmp > 5000 )) && rm -f $ftmp && touch $ftmp

#todo:  record script execution datetime
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
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        transfer control & pertubed rst files. This script is capable of determine
        which files to transfer without any inputs. 
        
        Usage: program [-chw] srcme_file

        Input: 
            A source file, which set various vars for a set of runs (i.e. srcme_dis_*)
 
        options:
            -b  run with a debug mode (this will not execute ${strscr}.sh)
            -c  clean unwanted files
            -h  show this help text
            -w  write stdout/err in a file
"

OPTIND=1
while getopts ':hcwb' option; do
    case "$option" in
        h)  echo "$usage"; exit 0;;
        c)  clean_dir; exit 0;;
        w)  writetofile=1;; 
        b)  optb=true;rundebug=1;; 
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
inputcheck
source ${arrposarg[0]}

[[ -z $DARCHRST || -z $strexpid ]] && die "DARCHRST or strexpid are not defined" 
#================================================================================
#                             Set Host Specific Vars
#================================================================================
thishst=$( get_host )

if [[ $thishst == "pfe" ]];then
    :
elif [[ $thishst == "dis" ]];then
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

#================================================================================
#                                 Set Variables
#================================================================================
#mid
dmess=$cdir/message
dcont=$cdir/output/rstpreps/$strdout
dpert=$cdir/output/rstmkpert/$strdout
dextr=$cdir/output/rstextr/$strdout

ferr=$dmess/stderr_${strscr}
flistlfe=wftmsz_flist
flstarc=$cdir/${strscr}_arc_$flistlfe

msg_subject="${hstshort}.${strscr}: $strexpid"

fmessage=$dmess/message_${strscr}
blmsg=false

fprob=$cdir/tmp_${strscr}_fpertProb
arrocnicyyyyymmdd=($( fcal_ocnicyyyymmdd 2000 ))
arrocnicmmdd1=($( printf '%s\n' ${arrocnicyyyyymmdd[@]} | xargs -i date -d"{} -1days" +%Y%m%d | cut -c5- | xargs -i echo {}_pert.tar ))
strocnicmmdd=$( echo  ${arrocnicmmdd1[@]} | sed 's# #|#g' )

#note:  limmaxf         : max limit for a number of files transfered at once
#       limmaxftot_pert : limit for total number of pert files transfered 
#       limmaxftot_cont : limit for total number of cont files transfered 
limmaxf=5
limmaxftot_pert=20
limmaxftot_cont=20

feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

[[ ! -d $dcont && ! -d $dpert ]] && exit
[[ ! -f $fmessage ]] && touch $fmessage
[[ ! -d $dmess    ]] && mkdir -p $dmess

#================================================================================
#                                  Main Process
#================================================================================
#letsbegin
blskip=true
blskip=false

if $blskip;then 
    #cdate=$( date +%Y%m%d_%H%M )
    #flstarc=$cdir/$( basename $flst_arc_org )_$cdate 
    
    flstarc_org=/discover/nobackup/knakada/GEOSS2S3/GEOS_util/wftmsz_flist
    cp -p $flstarc_org $flstarc

else
    get_fwftmsz $DARCHRST $flstarc
    if [[ ! -f $flstarc ]];then
        wmessage
        ! $blmsg && msg_wheader && blmsg=true
        wmessage "FAILED to retrieve $( basename $flstarc ). Exit"
        exit
    
        #! $blmsg && msg_wheader && blmsg=true
        #wmessage "skip retrieving but create empty $( basename $flstarc )"
        #touch $flstarc
    fi
fi

#todo:  remove cont/pert files if they exists and have the same size on lfe.
del_stuff 

arrftrans_cont=($( [[ -d $dcont ]] && find $dcont/* -maxdepth 0 -type f -name "*.tar" 2>/dev/null | sort -V | head -$limmaxftot_cont )) 
arrftrans_pert=($( [[ -d $dpert ]] && find $dpert/* -maxdepth 0 -type f -name "*.tar" 2>/dev/null | sort -V | head -$limmaxftot_pert )) 

#optb=true
if $optb; then
    wmessage arrftrans_cont:
    printf '%s\n' ${arrftrans_cont[@]} | column 
    wmessage
    wmessage arrftrans_pert:
    printf '%s\n' ${arrftrans_pert[@]} | column 
    exit
fi

#todo: checkout rst file size on lfe and ones on discover
#      Global variables, fsize_expected_*, are defined and fprob
#      may be created
rst_ckfsize
#wmessage \@$LINENO $fsize_expected_cont $fsize_expected_atmpert $fsize_expected_ocnpert
#[[ -f $fprob ]] && echo $fprob
#exit

if [[ -f $fprob ]];then 

    #Note:  03/28/2023 Once you get message with "ALEART!" in subject line,
    #       work on this code here. Without fprob file, it is tricky to work on this!
    sizefprob=$( stat --print='%s' $fprob ) || sizefprob=0
    if (( $sizefprob > 0 ));then 
        msg_wheader_userdefined 40 "-" $( basename $fprob ) 
        wmessage "$( cat $fprob )"
        wmessage
wmessage "\@$LINENO ( contsize,atmpertsize,ocnpertsize ) = ( $fsize_expected_cont $fsize_expected_atmpert $fsize_expected_ocnpert ) "
        rm -f $fprob 
    fi

    msg_subject="${hstshort}.${strscr}: $strexpid - ALERT! SENDING $( basename $fprob ) "

    #todo:  send email
    [[ -f $fmessage ]] && sendfmsg

    msg_subject="${hstshort}.${strscr}: $strexpid"

    exit
fi

! $blmsg && (( ${#arrftrans_pert[@]} > 0  ||  ${#arrftrans_cont[@]} > 0 )) && msg_wheader && blmsg=true

#todo:  rsync perturbation rst 
if (( ${#arrftrans_pert[@]} > 0 ));then
    arrtrans=()
    msg_wheader_userdefined 60 "∞" "Transfer Perturbation" 
    wmessage $( hostname )
    wmessage "Transfer ${#arrftrans_pert[@]} Files"
    wmessage "Transfer files from : $dpert"
    wmessage "                 to : $DARCHRST"
    wmessage

    #todo:  calculate a number of transfer 
    numtrans=$( echo "${#arrftrans_pert[@]}/$limmaxf" | bc )
    numremain=$( echo "${#arrftrans_pert[@]}%$limmaxf" | bc )
    (( $numremain != 0 )) && numtrans=$(( numtrans + 1 ))

    #+++++ cd dpert (start) +++++
    cd $dpert
    cnttot=0
    cnttrans=1
    while (( $cnttot < ${#arrftrans_pert[@]} ));do
        arrtrans+=( ${arrftrans_pert[$cnttot]} )

        #todo:  transfer limmaxf # of files for ( limmaxftot_pert /limaxf ) times 
        #       5 files per transfer for 4 times (as of 20211207) 
        if (( ${#arrtrans[@]} == $limmaxf && $cnttrans != $numtrans )) || \
           (( ${#arrftrans_pert[@]} == $(( cnttot + 1 )) && $cnttrans == $numtrans ));then

            wmessage "Transfer $( printf '%02g' $cnttrans ) of $( printf '%02g' $numtrans) ... $( date +'%Y%m%d %H:%M' )"
            wmessage "$( printf '%s\n' ${arrtrans[@]} | sort -V )"
            if (( $writetofile == 1 ));then
                sup rsync -az ${arrtrans[@]} lfe:$DARCHRST/ >> $fmessage
            else 
                sup rsync -az ${arrtrans[@]} lfe:$DARCHRST/ 
            fi
            wmessage
            cnttrans=$(( cnttrans + 1 ))
            arrtrans=()
        fi
        cnttot=$(( cnttot + 1 ))
    done

    cd - >/dev/null
    #+++++ cd dpert ( end ) +++++

    wmessage

fi


#todo:  transfer control rst
if (( ${#arrftrans_cont[@]} > 0 ));then 
    arrtrans=()
    msg_wheader_userdefined 60 "∞" "Transfer Control" 
    wmessage $( hostname )
    wmessage "Transfer ${#arrftrans_cont[@]} Files"
    wmessage "Transfer files from : $dcont"
    wmessage "                 to : $DARCHRST"
    wmessage

    #todo:  calculate a number of transfer 
    numtrans=$( echo "${#arrftrans_cont[@]}/$limmaxf" | bc )
    numremain=$( echo "${#arrftrans_cont[@]}%$limmaxf" | bc )
    (( $numremain != 0 )) && numtrans=$(( numtrans + 1 ))

    #+++++ cd dcont (start) +++++
    cd $dcont
    cnttot=0
    cnttrans=1
    while (( $cnttot < ${#arrftrans_cont[@]} ));do
        arrtrans+=( ${arrftrans_cont[$cnttot]} )

        #todo:  transfer limmaxf # of files for ( limmaxftot_cont /limaxf ) times 
        #       5 files per transfer for 20 times (as of 20211207) 
        if (( ${#arrtrans[@]} == $limmaxf && $cnttrans != $numtrans )) || \
           (( ${#arrftrans_cont[@]} == $(( cnttot + 1 )) && $cnttrans == $numtrans ));then

            wmessage "Transfer $( printf '%02g' $cnttrans ) of $( printf '%02g' $numtrans) ... $( date +'%Y%m%d %H:%M' )"
            wmessage "$( printf '%s\n' ${arrtrans[@]} | sort -V )"
            if (( $writetofile == 1 ));then
                sup rsync -az ${arrtrans[@]} lfe:$DARCHRST/ >> $fmessage
                #for trans in ${arrtrans[@]};do 
                #    sup scp -p $trans lfe:$DARCHRST/ >> $fmessage
                #done 
            else 
                sup rsync -az ${arrtrans[@]} lfe:$DARCHRST/ 
                #for trans in ${arrtrans[@]};do 
                #    sup scp -p $trans lfe:$DARCHRST/
                #done 
            fi
            wmessage

            cnttrans=$(( cnttrans + 1 ))
            arrtrans=()
        fi
        cnttot=$(( cnttot + 1 ))
    done

    cd - >/dev/null
    #+++++ cd dcont ( end ) +++++

    wmessage
fi

#todo:  send email
[[ -f $fmessage ]] && sendfmsg

exit

