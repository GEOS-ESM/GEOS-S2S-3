#!/usr/bin/env bash

message_dis() {
    local numr=$( squeue -u $USER -o "%.10i %.9P %.16j %.8u %.8T %.10M %.9l %.6D %.5C" | grep -i running | wc -l )
    local numq=$( squeue -u $USER -o "%.10i %.9P %.16j %.8u %.8T %.10M %.9l %.6D %.5C" | grep -i pending | wc -l )

    if (( $numr > 0 )) || (( $numq > 0 ));then
        msg_wheader
        wmessage "running = $numr"
        wmessage "  queue = $numq"
        wmessage

        if (( $writetofile == 1 ));then
            squeue -u $USER -o "%.10i %.9P %.16j %.8u %.8T %.10M %.9l %.6D %.5C" | grep $USER >> $fmessage
        else
            squeue -u $USER -o "%.10i %.9P %.16j %.8u %.8T %.10M %.9l %.6D %.5C" | grep $USER
        fi
        wmessage
    fi
    return
}

message_pfe() {

    local arrqid=($( res_qid_all ))
    local _arr=()
    local numcore qid qidnumnode
    
    #todo:  get total number of nodes for each reservation
    for qid in ${arrqid[@]};do
        local numnode=$( res_nodect $qid )
        _arr+=( $qid:$numnode )
    done
    local arr_sortbynodect=($( printf '%s\n' ${_arr[@]} | sort -V -r -k2 -t':' ))
    
    for qidnumnode in ${arr_sortbynodect[@]};do
        local qid=$( echo $qidnumnode | cut -d':' -f1 )
    
        local numr=$( qstat -e -u gmaofcst -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep $qid | grep -w R | wc -l )
        local numq=$( qstat -e -u gmaofcst -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep $qid | grep -wE 'Q|Qs' | wc -l )
        
        if (( $numr > 0 )) || (( $numq > 0 ));then
            wmessage "running = $numr"
            wmessage "  queue = $numq"
            wmessage
        
            if (( $writetofile == 1 ));then
                qstat -e -u gmaofcst -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep $qid  >> $fmessage
            else
                qstat -e -u gmaofcst -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep $qid  
            fi
            wmessage
        fi
    done
   
    local _arrnumcore=( 4880 3750 )  
    local strqid=$( echo ${arrqid[@]} | sed 's# #|#g' )

    #todo: write jobs on non-reserved nodes
    for numcore in ${_arrnumcore[@]};do 
        local numr=$( qstat -e -u gmaofcst -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep $numcore | grep -vE "$strqid" | grep -w R | wc -l )
        local numq=$( qstat -e -u gmaofcst -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep $numcore | grep -vE "$strqid" | grep -wE 'Q|Qs' | wc -l )
        
        if (( $numr > 0 )) || (( $numq > 0 ));then
            wmessage 
            wmessage "running = $numr"
            wmessage "  queue = $numq"
            wmessage

            if (( $writetofile == 1 ));then
                qstat -e -u gmaofcst -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep $numcore | grep -vE "$strqid" >> $fmessage
            else
                qstat -e -u gmaofcst -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep $numcore | grep -vE "$strqid" 
            fi
            wmessage
        fi
    done

    #todo:  write gcm_post.j jobs.
    local numr=$( qstat -e -u gmaofcst -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep ivy | grep normal | grep -w R | wc -l )
    local numq=$( qstat -e -u gmaofcst -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep ivy | grep normal | grep -wE 'Q|Qs' | wc -l )
    if (( $numr > 0 )) || (( $numq > 0 ));then
        wmessage 
        wmessage "running = $numr"
        wmessage "  queue = $numq"
        wmessage 
                
        if (( $writetofile == 1 ));then
            qstat -e -u gmaofcst -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep ivy | grep normal | sort -V -k4 -t' ' >> $fmessage
        else
            qstat -e -u gmaofcst -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep ivy | grep normal | sort -V -k4 -t' ' 
        fi
        wmessage
    fi 
    
    #todo:  exit here if no jobs are on queue. 
    numjobs=$( qstat -e -u gmaofcst -W 'fmt_Queue=-maxw 20' -W o=+Rank0  | wc -l )
    if (( $numjobs == 0 ));then exit;fi

    return
}

sftc_running() {
    #description:   get current shiftc running status 
    local numsftc=$( /usr/local/bin/shiftc --status=csv | grep run | sed 's/,/\t/g' | tail -n +2 | wc -l )

    if (( $numsftc > 0 ));then
        msg_wheader_userdefined 60 "-" "Running Shiftc (as of $( date +%Y%m%d ))" 

        if (( $writetofile == 1 ));then
            /usr/local/bin/shiftc --status=csv | grep run | sed 's/,/\t/g' >> $fmessage
        else
            /usr/local/bin/shiftc --status=csv | grep run | sed 's/,/\t/g'
        fi
        wmessage 
    fi

    return
}

sendmsg() {
    local _tmp_body=$cdir/_tmp_${strscr}_${FUNCNAME[0]}

    [[ -f $ferr     ]] && local sizeferr=$( stat --print='%s' $ferr )  || local sizeferr=0
    [[ -f $fmessage ]] && local sizef=$( stat --print='%s' $fmessage ) || local sizef=0

    if (( $sizeferr > 0 || $sizef > 0 ));then 
        if (( $sizef > 0 ));then
            msg_wheader_userdefined 40 "-" $( basename $ferr ) 
            wmessage "$( cat $ferr )"
            blrm=true
        fi

        #Note: 07/18/2023 msg_cyberpostman does not work for this script
        #                 Use mutt directly.
        msg_cyberpostman "$msg_subject" "$eadds" $fmessage $blmsgmutt
        local status_email=$?

        #echo "See an attached file" >| $_tmp_body
        #mutt -s "$msg_subject" -a $fmessage -- "$eadds" < $_tmp_body
        #local status_email=$?

        if $blsavefmsg && [[ -n $fmsg_fname ]] ;then
            mv $fmessage $dstdout/$fmsg_fname
        else
            (( $status_email == 0 )) && rm -f $fmessage
        fi

        $blrm && msg_newfile $ferr
        [[ -f $_tmp_body ]] && rm -f $_tmp_body
    fi
}



clean_dir() {
    [[ -n $flock && -f $flock ]] && rm -f $flock
    return
}

#================================================================================
#                                     Begin
#================================================================================
#beg
hst=$( hostname )
blnode=false
if [[ "$hst" =~ "pfe"* ]];then
    :
elif [[ "$hst" =~ "r"* ]];then
    #note:  this is for host other than pfe @ NAS which has a name starting with 'r'
    blnode=true
elif [[ "$hst" =~ "discover"* ]];then
    :
    #echo "this script works only on pfe (for now)";exit
    #cdir=/discover/nobackup/projects/gmao/t2ssp/GEOSS2S3/GEOS_util
else
    exit
fi

strscr=$(basename "$0" | cut -d'_' -f2 | cut -d'.' -f1 )
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
cdir=$SCRIPTPATH

[[ -z $cdir || ! -d $cdir ]] &&  echo "cdir (parent directory of this script) is undefined"  && exit

cd $cdir

flock=$cdir/${strscr}.lock
ftmp=$cdir/stdout/rundatetime/tmp_$strscr
[[ ! -f $ftmp ]] && install -D /dev/null $ftmp

stmp=$( find $ftmp -printf "%s\n" )
(( $stmp > 5000 )) && rm -f $ftmp && touch $ftmp

#∞∞∞∞∞ debug (start) ∞∞∞∞∞
runhere=0
if (( $runhere == 1 ));then
    #ref:   https://intoli.com/blog/exit-on-errors-in-bash-scripts/
    #note:  exit when any command fails
    set -e
    
    #note:   keep track of the last executed command
    trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
    
    #note:  echo an error message before exiting
    trap 'echo "\"${last_command}\" command filed with exit code $?."' EXIT
fi
#∞∞∞∞∞ debug ( end ) ∞∞∞∞∞

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

if [[ ! $cdir/func_fcst.sh ]];then 
    echo "$cdir/func_fcst.sh does not exist";exit
else
    source $cdir/func_fcst.sh
fi
trap clean_dir EXIT

[[ -z $RUN_BY_CRON ]] && RUN_BY_CRON=false

writetofile=0
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        send email message with qstat outputs for forecast exp.
        
        Usage: ./$(basename "$0")

        options:
            -c  clean unwanted files
            -h  show this help text
            -w  write stdout/err in a file
"

while getopts ':hcw' option; do
    case "$option" in
        h)  echo "$usage"; exit 0;;
        c)  clean_dir; exit 0;;
        w)  writetofile=1;; 
        \?) die "Invalid option: -$OPTARG" >&2
            exit 1;;
        :)  die "Option -$OPTARG requires an argument." >&2
            exit 1;;
    esac
done
#================================================================================
#                             Set Host Specific Vars
#================================================================================
hstshort=$( get_host )
if [[ "$hstshort" == "pfe" ]];then
    basepath=$PATH
    basepath=/usr/local/bin:/home6/gmaofcst/.cargo/bin:/home6/gmaofcst/knakada/bin:/u/scicon/tools/bin:$basepath
    export BASEPATH=$basepath
    export PATH=$BASEPATH
    export TERM=xterm-256color
    umask 0022
fi

#================================================================================
#                                 Set Variables
#================================================================================
#mid
dmess=$cdir/message
dstdout=$cdir/stdout/$strscr

fmessage=$dmess/message_$strscr
ferr=$dmess/stderr_$strscr
cmm=$( date +%-M )
cdate=$( date +%Y%m%d%H%M )

#msg_hrinc=8
#msg_hrpref=4
#msg_hrinc=3
#msg_hrinc=1
#msg_hrpref=6

blsavefmsg=true
fmsg_fname=message_${strscr}_${cdate} 

#msg_subject="${hstshort}.${strscr}: Fcst Exp - Last $msg_hrinc hrs Results"
if [[ $hstshort == pfe ]];then
    msg_subject="${hstshort}.${strscr}: $hstshort Fcst Exp"
elif [[ $hstshort == dis ]];then
    msg_subject="${hstshort}.${strscr}: $hstshort Submitted Jobs"
fi

feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

[[ ! -d $dmess    ]] && mkdir -p $dmess
[[ ! -d $dstdout  ]] && mkdir -p $dstdout
[[ ! -f $fmessage ]] && touch $fmessage
#================================================================================
#                                  Main Process
#================================================================================
#main
if [[ "$hstshort" == "dis" ]];then
RUN_BY_CRON=true    
    message_dis 
    
elif [[ "$hstshort" == "pfe" ]];then
    numjobs=$( qstat -e -u gmaofcst -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | wc -l )
    if (( $numjobs > 0 ));then
        msg_wheader
        wmessage $hst
        message_pfe

        #todo:  unless fmessage is sent out, the rest of info will be written once 
        #       bfore email is out.
        if (( ${cmm} < 5 ));then
            sftc_running
            write_allquota
            msg_wheader_userdefined 60 "-" "Existing *.lock Files (as of $( date +%Y%m%d ))" 
            wmessage "$( stat --printf='%.16y %n\n'  *.lock | sort -V )"
            wmessage
            #msg_wheader_userdefined 60 "-" "Accounting - SBU Usage Year-to-Date (as of $( date +%Y%m%d ))" 
            #wmessage "$( ssh -q pfe /usr/local/bin/acct_ytd )" 
            #wmessage
        fi
    fi
fi
    
#todo:  send email
[[ -z $eadds ]] && exit

[[ ! -s $fmessage ]] && exit

if $blnode || $RUN_BY_CRON || [[ "$hstshort" == "pfe" ]];then

    #todo:  count a number of time that stdout are written in fmessage
    numrun=$( grep -w $hst $fmessage | wc -l ) 

    if [[ "$hstshort" == "pfe" ]] && (( ${cmm} < 5 ));then
        :
    elif [[ "$hstshort" == "dis" ]] && (( ${cmm} < 5 ));then 
        :
    elif (( ${numrun} >= 3 ));then
        :
    else
        exit
    fi 

    if [[ "$hstshort" == "pfe" ]];then 
        [[ -s $fmessage ]] && sendmsg

    elif [[ "$hstshort" == "dis" ]];then 
        msg_cyberpostman "$msg_subject" "$eadds" $fmessage $blmsgmutt
        status_postman=$?
        (( $status_postman == 0 )) && rm -f $fmessage
    fi
fi


exit
