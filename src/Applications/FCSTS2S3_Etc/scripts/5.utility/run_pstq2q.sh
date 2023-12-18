#!/usr/bin/env bash

inputcheck(){
    [[ -z  ${arrposarg[@]} ]] && die "inputs are missing"
    (( ${#arrposarg[@]} != 4 )) && die "4 inputs are required"

    $opts && $optj && die "option s and j can not be selected at the same time"

    #todo:  check userqid input
    if [[ "$userqid" == "normal" || "$userqid" == long ]];then
        :
    else
        /PBS/bin/pbs_rstat | grep $userqid >> /dev/null 2>&1
        (( $? > 0 )) && die "$userqid does not exist"
    fi

    #todo:  check model name 
    local thisncpus=$( getnumcore $usermname ) 
    local isint=$( misc_isinteger $thisncpus )
    (( $isint > 0 )) && die "model name does not exist"

    #todo:  check number of nodes
    local isint=$( misc_isinteger $usernumnode )
    (( $isint > 0 )) && die "a number of nodes has to be an integer"

    #local srcme=$( echo ${arrposarg[-1]} | cut -d'_' -f2 )
    #[[ "$srcme" != "$hstshort" ]] && die "source file is not for this host (select srcme_${hstshort}_* file)"
}

sendmsg() {

    [[ -f $ferr     ]] && local sizeferr=$( stat --print='%s' $ferr )  || local sizeferr=0
    [[ -f $fmessage ]] && local sizef=$( stat --print='%s' $fmessage ) || local sizef=0

    if (( $sizeferr > 0 || $sizef > 0 ));then 
        if (( $sizef > 0 ));then
wmessage "@$LINENO ${!size@} = $sizef"
            msg_wheader_userdefined 40 "-" $( basename $ferr ) 
            wmessage "$( cat $ferr )"
            blrm=true
        fi

local blmsgmutt=false
        msg_cyberpostman "$msg_subject" "$eadds" $fmessage $blmsgmutt
        local status_email=$?

        if $blsavefmsg;then
            mv $fmessage $dstdout/$fmsg_fname
        elif ! $blsavefmsg;then
            (( $status_email == 0 )) && rm -f $fmessage
        fi
        
        $blrm && msg_newfile $ferr
    fi
}


delete_dupjobs(){
    #description:   delete duplicate gcm_post jobs.
    local jobpst

    local _arrjobpst_all=($( cmd_gjob_nas_jlong | grep $usermname | grep -iv $strstdin | tr -s '[:space:]' | cut -d' ' -f4 | sort -V ))
    local _arrjobpst_dup=($( printf '%s\n' ${_arrjobpst_all[@]}  | sort -V | uniq -d ))

    local _totdel=0
    local _cnt=0
    local _numjobpst=${#_arrjobpst_dup[@]} 
    for jobpst in ${_arrjobpst_dup[@]};do
    
        _cnt=$(( _cnt + 1 ))

        local _numonq=$( cmd_gjob_nas_jlong | grep $usermname | grep -iv $strstdin | tr -s '[:space:]' | grep $jobpst | wc -l )
        if (( $_numonq == 1 ));then 
            continue
    
        elif (( $_numonq > 1 ));then 

            local _numrunning=$( cmd_gjob_nas_jlong | grep $usermname | grep -iv $strstdin | tr -s '[:space:]' | grep $jobpst | grep -w R | wc -l ) 

            if (( $_numrunning == 0 ));then 
                local _arrqid=($( cmd_gjob_nas_jlong | grep $usermname | grep -iv $strstdin | tr -s '[:space:]' | grep $jobpst | grep -w Q | tail -n +2 | cut -d' ' -f1 ))
            elif (( $_numrunning > 0 ));then 
                local _arrqid=($( cmd_gjob_nas_jlong | grep $usermname | grep -iv $strstdin | tr -s '[:space:]' | grep $jobpst | grep -w Q | cut -d' ' -f1 ))
            fi

            msg_wheader_userdefined 40 -   $jobpst
            wmessage "$( printf "%+${#_numjobpst}s of %+${#_numjobpst}s\n" $_cnt ${#_arrjobpst_dup[@]} )"
            cmd_gjob_nas_jlong | grep $usermname | grep -iv $strstdin | tr -s '[:space:]' | grep $jobpst
            wmessage 
    
            _totdel=$(( _totdel + ${#_arrqid[@]} ))
            printf '%s\n' ${_arrqid[@]} | xargs -i /PBS/bin/qdel {} 
    
            wmessage "Deleted:"
            ahand_print ${_arrqid[@]} 
        fi
    
        wmessage "    Total Deleted Jobs so far: $_totdel" 
        #wmessage "Total gcm_post jobs worked on: $_cnt" 
        wmessage 
    
        #(( $_totdel > $lim_deljobs )) && break
        (( $_cnt == $lim_jobpst )) && break
    done

    wmessage "Deleted Jobs Total = $_totdel" 
    wmessage

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
hstname=$( hostname )
blnode=false
if [[ ${hstname:0:3} == pfe ]];then
    :
elif [[ ${hstname:0:3} == dis ]];then
    :
elif [[ "$hst" =~ "r"* ]];then
    #note:  this is for host other than pfe @ NAS which has a name starting with 'r'
    blnode=true
elif [[ "${hstname:0:4}" == borg || "${hstname:0:4}" == warp ]];then 
    #note:  this is for host other than pfe @ NCCS which has a name starting with 'borg' or 'warp'
    :
else 
    exit
fi

strscr=$(basename "$0" | cut -d'_' -f2 | cut -d'.' -f1 )
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd )"
cdir=$SCRIPTPATH

[[ -z $cdir || ! -d $cdir ]] && echo "cdir is undefined" && exit

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

hstshort=$( get_host )
writetofile=0
optn=false
optj=false
opts=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename "$0") -- this is a program to:
        move bulk of submitted gcm_post.*.j* from one queue to another. Three inputs
        are required in a order of qid (i.q. reservation id, normal, long),
        a number of nodes, and srcme file

        Usage: ./$(basename "$0") [-hcw] [-n cpu number] [-s search_key] [-j # of jobs] qid model numnode srcme_file

        options:
            -s  A search keyword
            -j  A number of jobs
            -n  A number of cpus other than model default
            -h  show this help text
            -w  write stdout/err in a file
"

verbose=0

while :; do
    case $1 in
        -h|-\?|--help)  echo "$usage" && exit;; # Display a usage synopsis.
                  -s )  opts=true; usersearchkey=$2; shift;;
                  -j )  optj=true; userjobnumber=$2; shift;;
                  -w )  writetofile=1;;
                  -n )  optn=true; userncpus=$2; shift;;
                  -- )  shift && break ;;   # End of all options.
                 -?* )  printf 'WARN: Unknown option (ignored): %s\n' "$1" >&2 ;;
                   * )  break ;; # Default case: No more options, so break out of the loop.
    esac
    shift
done

#todo:  get positional inputs.
shift $((OPTIND-1))
[[ "${1:-}" = "--" ]] && shift
arrposarg=( "$@" )
#================================================================================
#                               Check User Inputs
#================================================================================
i=0
#search_key=${arrposarg[i]}; i=$(( i + 1 ))
    userqid=${arrposarg[i]}; i=$(( i + 1 ))
  usermname=${arrposarg[i]}; i=$(( i + 1 ))
usernumnode=${arrposarg[i]}; i=$(( i + 1 ))
       strf=${arrposarg[i]}; i=$(( i + 1 ))

inputcheck
[[ -n $strf ]] && source $strf

#================================================================================
#                             Set Host Specific Vars
#================================================================================
if [[ "$hstshort" == "pfe" ]];then
    cmd_qstat=cmd_gjob_nas

elif [[ "$hstshort" == "dis" ]];then
    cmd_qstat=cmd_gjob_nccs

elif $blnode;then 
    :
else
    clean_dir
    exit
fi

#================================================================================
#                                 Set Variables
#================================================================================
#mid
cdate=$( date +%Y%m%d%H%M )
if [[ -n $rstexpid ]];then 
    strexpid=$strexpid$rstexpid
    blrstexpid=true 
else
    blrstexpid=false
fi

dstdout=$cdir/stdout/$strscr/$strdout
   dout=$cdir/output/$strscr/$strdout
  dmess=$cdir/message

fmessage=$dmess/message_${strscr}_${strexpid}
    ferr=$dmess/stderr_$strscr

bldup=false

_qstat=/u/scicon/tools/bin/qstat
_qdel=/PBS/bin/qdel
#_pbs_rstat=/PBS/bin/pbs_rstat

strpbs1=Resource_List.select
strmodel="model="
strncpus="ncpus="
strpbs_jname=Job_Name
strpbs_output=Output_Path
strpbsresev_pst="toss4"
strstdin=stdin
strmsg=

blheader=false

#limjob=1
limjob=30
numdays=10

blsavefmsg=false
fmsg_fname=message_${strscr}_${cdate} 
msg_subject="${hstshort}.${strscr}: $strexpid"

feadd=$cdir/mailadd
[[ -f $feadd ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

#search_key=P2015
#userqid=R13245282
#usermname=ivy
#usernumnode=1

if [[ "$userqid" != "normal" ]];then
    limjob=$( /PBS/bin/pbs_rstat -f R16397202 | grep Resource_List.nodect |rev | cut -d' ' -f1 | rev )

    #note:  exit here when there are many jobs on $userqid
    #       4 in "limjobs * 4" is randomly selected
    numjobs=$( $cmd_qstat | grep $userqid | wc -l )
    limtotjobs=$( echo "$limjob * 2" | bc ) 
    (( $numjobs > $limtotjobs )) && exit
fi

if $optn;then
    ncpus=$userncpus
else
    ncpus=$( getnumcore $usermname )
fi

$opts && strmsg=$usersearchkey
$optj && strmsg=$userjobnumber

if [[ -n $strmsg ]];then 
    strfout=${strscr}_${strmsg}
else
    strfout=${strscr}
fi


#todo:  delete old output files in dout (older > 30 days)
if [[ -d $dout ]];then 
    #+++++ cd dout (start) +++++
    cd $dout
    arrfout=($( find * -type f -not -mtime $numdays 2>/dev/null ))

    if (( ${#arrfout[@]} > 0 ));then 
        #if ! $blheader;then 
        #    msg_wheader
        #    blheader=true
        #fi
        #wmessage "@$LINENO these files can be deleted:"
        #ahand_print ${arrfout[@]} 
        rm -f ${arrfout[@]} 
        #wmessage
    fi

    cd - >/dev/null
    #+++++ cd dout ( end ) +++++
fi

#(( $writetofile == 1 )) && msg_newfile $fmessage

[[ ! -d $dmess   ]] && mkdir -p $dmess
[[ ! -d $dstdout ]] && mkdir -p $dstdout
[[ ! -d $dout    ]] && mkdir -p $dout

#================================================================================
#                                  Main Process
#================================================================================
#letsbegin
#todo:  create PBS select statement:
#PBS -l select=1:ncpus=20:mpiprocs=20:model=ivy
if [[ "$userqid" == "normal" || "$userqid" == "long" ]];then
    thisselect="#PBS -l select=$usernumnode:ncpus=$ncpus:mpiprocs=$ncpus:model=$usermname"
else
    thisselect="#PBS -l select=$usernumnode:ncpus=$ncpus:mpiprocs=$ncpus:model=$usermname:aoe=$strpbsresev_pst"
fi

cnt_run=0
blrun=true
while $blrun;do
    cnt_run=$(( cnt_run + 1 ))
    cdate=$( date +%Y%m%d%H%M%S )
    fdata_jids=$dout/${strfout}_${cdate}_gcmpostIDs
    fdata_psto=$dout/${strfout}_${cdate}_gcmpostO
    fdata_pstj=$dout/${strfout}_${cdate}_gcmpostJ
    fdata_jedt=$dout/${strfout}_${cdate}_gcmpostEditted
    fdata_jsub=$dout/${strfout}_${cdate}_gcmpostSubmitted
    
    if $opts; then 
        arrjobid1=($( $cmd_qstat | grep $usermname | grep -w -E 'Q|Qs' | grep -v $userqid | grep $usersearchkey  | cut -d' ' -f1 ))
    elif $optj;then
        arrjobid1=($( $cmd_qstat | grep $usermname | grep -w -E 'Q|Qs' | grep -v $userqid | tail -$userjobnumber | cut -d' ' -f1 ))
    else
        arrjobid1=($( $cmd_qstat | grep $usermname | grep -w -E 'Q|Qs' | grep -v $userqid | cut -d' ' -f1 ))
        #arrjobid1=($( $cmd_qstat  | grep -w -E 'Q|Qs' | grep -v $userqid | cut -d' ' -f1 ))
    fi
    
    arrjobid=($( printf '%s\n' ${arrjobid1[@]} | head -$limjob ))

    if [[ "$userqid" != "normal" ]];then
        #note:  exit here when there are many jobs on $userqid
        #       2 in "limjobs * 2" is randomly selected
        numjobs=$( $cmd_qstat | grep $userqid | wc -l )
        limtotjobs=$( echo "$limjob * 2" | bc ) 
        (( $numjobs > $limtotjobs )) && break 
    fi

#wmessage \@$LINENO $numjobs $limtotjobs
    
    (( ${#arrjobid1[@]} > $limjob )) && blrun=true || blrun=false


    #blrun=false

#wmessage \@$LINENO $blrun ${#arrjobid1[@]}
#wmessage $thisselect
#wmessage $usermname 
#ahand_print ${arrjobid[@]}
#exit

    #todo:  get job names
    if $blrstexpid;then
    
        arrjobid1=( ${arrjobid[@]} )
        arrjobid=()
    
        for jobid in ${arrjobid1[@]};do
            thisdpst=$( $_qstat -f $jobid 2>/dev/null | grep -i $strpbs_output | rev | cut -d':' -f1 | rev | xargs -i dirname {} )
            numthisdpst=$( echo $thisdpst | grep $strexpid | grep $rstexpid | wc -l )
            if (( $numthisdpst == 1 ));then
                arrjobid+=( $jobid )
            fi
        done
    fi

#wmessage \@$LINENO
##wmessage $usermname 
#ahand_print ${arrjobid[@]}
#exit

    #todo:  if no gcmpost to transfer to userqid, exit
    (( ${#arrjobid[@]} == 0 )) && break
   
    arrjobpst_all=($( cmd_gjob_nas_jlong | grep $usermname | grep -iv $strstdin | tr -s '[:space:]' | cut -d' ' -f4 | sort -V ))
    arrjobpst_uniq=($( printf '%s\n' ${arrjobpst_all[@]} | sort -V | uniq    ))
     arrjobpst_dup=($( printf '%s\n' ${arrjobpst_all[@]} | sort -V | uniq -d ))

    ! $blheader && msg_wheader && blheader=true

    msg_wheader_userdefined 40 "-" "Run #$cnt_run" 
    wmessage $hstname

    [[ -n $numjobs ]] && wmessage "       # of Jobs on $userqid: $numjobs"
    wmessage "# of gcm_post Jobs Submitted: ${#arrjobpst_all[@]} "
    wmessage "     # of Uniq gcm_post Jobs: ${#arrjobpst_uniq[@]}"
    wmessage "# of Duplicate gcm_post Jobs: ${#arrjobpst_dup[@]} "
    wmessage

    if (( ${#arrjobpst_dup[@]} > 0 ));then 
        wmessage "ALERT - There are ${#arrjobpst_dup[@]} of duplicate jobs submitted"
        bldup=true
        break 
    fi


    wmessage "  New PBS queue: $userqid"
    wmessage "  New PBS model: $thisselect"
    
    #todo:  mk backup file
    [[ -f $fdata_psto && ! -f $fdata_psto.bak ]] && cp -p $fdata_psto $fdata_psto.bak
    [[ -f $fdata_pstj && ! -f $fdata_pstj.bak ]] && cp -p $fdata_pstj $fdata_pstj.bak
    
    #todo: get all running gcmpost
    if [[ ! -f $fdata_psto ]];then
        printf '%s\n' ${arrjobid[@]} | sort -V > $fdata_jids
    
        wmessage "           Note: Submit ${#arrjobid[@]} gcm_post*.j to $userqid"
        wmessage "   Data File #1: ${fdata_jids}"
        wmessage "   Data File #2: ${fdata_psto}"
    
        for jobid in ${arrjobid[@]};do
            $_qstat -f $jobid | grep -i output_ | rev | cut -d':' -f1 | rev  >> $fdata_psto
        done
    fi
    
    #todo:  get all gcm_post.*.j* file full path
    if [[ ! -f $fdata_pstj ]];then
        wmessage "   Data File #3: ${fdata_pstj}"
        arrfpsto1=($( misc_readfbyline $fdata_psto ))
        arrfpsto=($( printf '%s\n' ${arrfpsto1[@]} | sort -V | uniq ))
        for fpsto in ${arrfpsto[@]};do
            strlast1=$( echo  $fpsto | rev | cut -d'.' -f1 | rev  )
            fpstj=$( dirname $fpsto )/$( basename $fpsto | rev | cut -d'.' -f2- | rev ).${strlast1/o/j}
            echo $fpstj >> $fdata_pstj
        done
    fi
    
    #numfline=$( cat $fdata_pstj | wc -l )
    #numfline_uniq=$( cat $fdata_pstj | sort -V | uniq | wc -l )
    #
    #wmessage
    #wmessage "$( basename $fdata_pstj ):"
    #wmessage "$fdata_pstj"
    #wmessage "Number of      Lines: $numfline"
    #wmessage "Number of Uniq Lines: $numfline_uniq"
    #exit
    
    #todo:  delete jobs
    numfline=$( cat $fdata_pstj | wc -l )
    if (( $numfline > 0 ));then
        arrjobdelete=($( misc_readfbyline $fdata_jids ))
        printf '%s\n' ${arrjobdelete[@]} | xargs -i $_qdel {} 
        wmessage "      Qdel jobs: ${#arrjobdelete[@]} "
    else
        exit
    fi

    #todo:  edit PBS statement in gcm_post.*.j* 
    if [[ ! -f $fdata_jedt ]];then
    
        wmessage "Edit gcm_pst*.j: ${fdata_pstj}"
        arrfpstj1=($( misc_readfbyline $fdata_pstj ))
        arrfpstj=($( printf '%s\n' ${arrfpstj1[@]} | sort -V | uniq ))
    
        for fpstj in ${arrfpstj[@]};do
            [[ -f $fpstj.org ]] && mv $fpstj.org $fpstj.org.$cdate
            [[ -f $fpstj.org ]] && mv $fpstj.org $fpstj

            exp_editqname $fpstj $userqid "$thisselect"
            status_edit=$?
            
            if (( $status_edit > 0 ));then
                echo "FAILED $fpstj" >> $fdata_jedt 
                [[ -f $fpstj.org ]] && mv $fpstj.org $fpstj
            else
                echo "Success $fpstj" >> $fdata_jedt
            fi
            
        done
    fi
    
    if [[ -f $fdata_jedt ]];then 
        wmessage "      Submitted: ${fdata_jsub}"
        arrfpstj_submit=($( cat $fdata_jedt | grep -i success | rev | cut -d' ' -f1 | rev | sort -V | uniq ))
        for fpstj_submit in ${arrfpstj_submit[@]};do
            cd $( dirname $fpstj_submit ) 
            echo "$fpstj_submit $( /PBS/bin/qsub $( basename $fpstj_submit ))" >> $fdata_jsub 2>&1
            cd - >/dev/null 
        done 
    fi
#break 

    (( $cnt_run == 2 )) && break
    sleep 10s
done     

#todo:  delete duplicate gcm_post jobs
if $bldup;then 
    #wmessage \@$LINENO $bldup
    delete_dupjobs
fi

#todo:  send email
[[ -z $eadds ]] && exit

#todo:  send email
[[ -f $fmessage ]] && sendmsg

exit








if $blnode || $RUN_BY_CRON ;then

    if ! $bldup; then 
        msg_hrinc=6
        msg_hrpref=6
    else
        msg_subject="$msg_subject - ALERT! gcm_post Jobs Deleted"
    fi

    [[ -f $ferr     ]] && sizeferr=$( stat --print='%s' $ferr     ) || sizeferr=0
    [[ -f $fmessage ]] &&    sizef=$( stat --print='%s' $fmessage ) || sizef=0

    if (( $sizef > 0 || $sizeferr > 0 ));then 

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
fi

exit
