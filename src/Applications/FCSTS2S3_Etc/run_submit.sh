#!/usr/bin/env bash
PS4='$LINENO: '

inputcheck(){
    $optf && [[ ! -f $userinput ]] && die "$userinput file doesn't exist."
    $optq && [[   -z $userinput_qid4grn ]] && die "opt q requires an input."

    if $optt;then 
        isint=$( misc_isinteger $thres_grn )
        (( $isint == 1 )) && die "OPTARG for optt is NOT an integer."
    fi
}

filter_arch(){
    
    local _arrdexp=( "$@" )
    local arr=()
    local fdelcomp=gcmarch_deloutcompleted

    for thisdexp in ${_arrdexp[@]};do
        local cntbug=0

        [[ -f $thisdexp/$fdelcomp ]] && continue

        debug_filter $dexp 
            
        arr+=( $thisdexp ) 

    done 
    echo ${arr[@]} 
}

filter_dexp(){
    local _arrinput=( "$@" )
    local nummonth1=3
    local arr=()
    local mon2 _dexp

    #todo:  check if clean_completed marker exists
    _arrinput=($( printf '%s\n' ${_arrinput[@]} | xargs -i bash -c "[[ ! -f {}/clean_completed ]] && echo {}" ))

    debug_filter ${_arrinput[@]}

    #todo:  check if gcm_run.j exists
    _arrinput=($( printf '%s\n' ${_arrinput[@]} | xargs -i bash -c "[[ -f {}/gcm_run.j ]] && echo {}" ))

    debug_filter ${_arrinput[@]}

    for _dexp in ${_arrinput[@]};do
            
        local cntbug=0

        #todo:  check if exp is already on queue
        cnt_jobs $_dexp
        (( $num_rgrn > 0 )) && continue

        debug_filter $_dexp

        #todo:  check if this is a winner exp
        if [[ "$dexp" == *"GEOS_exp"* ]];then 
            straf=${dexp#*GEOS_exp/}
            strbf=${straf%/$fcstdate/$ensm}
            numchar=$( echo "$strbf"  | grep -o "/" | wc -l ) 
            
            #note:   there should be 1 or 2 of '/' in strbf
            #        strbf should be ctag/strexpid OR ctag/strexpid/rstexpid 
            if (( $numchar == 1 ));then
                local strexpid=$( echo $strbf | cut -d'/' -f2 ) 
                local strdout=$strexpid

            elif (( $numchar == 2 ));then
                local strexpid=$( echo $strbf | cut -d'/' -f2- | sed 's#/##g' )
                local  strdout=$( echo $strbf | cut -d'/' -f2-3 )
            fi

        else
            local strexpid=fcst
            local strdout=$strexpid
        fi

        local ddatawin=$cdir/output/pckwinners/$strdout
        local fwin=$ddatawin/winners_nino3.4_$strexpid

        local fcstyyyymmdd=$( echo $_dexp | rev | cut -d'/' -f2 | rev  )
        local fcstyyyymm=$( echo $fcstyyyymmdd | cut -c1-6 )
        local fcstensm=$( echo $_dexp | rev | cut -d'/' -f1 | rev )
        
        local mon2=$fcstyyyymm
        for int in {1..2};do
            mon2=$( fcal_nextmonth $mon2 )
        done
        local blrerun=$( grep -i "RERUN" $_dexp/gcm_run.j | head -1 | rev | cut -d' ' -f1 | rev | tr '[:upper:]' '[:lower:]'  )
        local thisfwin=${fwin}_${fcstyyyymm}_${mon2}.txt
        if [[ -f $thisfwin ]];then
            grep $fcstyyyymm $thisfwin | grep $fcstensm >/dev/null
            local status_grep=$?
            (( $status_grep == 0 )) && local blwin=true || local blwin=false
        else
            local blwin=false     
        fi
        
        #todo:  check cap_* files exist
        local fcapr=$_dexp/cap_restart
        local fcapric=$_dexp/cap_restartIC
        local fcapend=$_dexp/cap_end
        if [[ -f $fcapr && -f $fcapric && -f $fcapend ]];then
            :
        else
            continue
        fi

        debug_filter $_dexp

        local    capr_yyyymmdd=$( cat $fcapr   | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
        local  capric_yyyymmdd=$( cat $fcapric | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
        local realend_yyyymmdd=$( cat $fcapend | head -1 | tr -s '[:space:]' | cut -d' ' -f1 )
        local strxmonth=$( nextXmonths $( date -d $fcstyyyymmdd +%Y ) $( printf '%01g' $( date -d $fcstyyyymmdd +%m ) ) 0 $(( nummonth1 + 1 ))  )
        local mmyyyy=$( echo $strxmonth | rev | cut -d' ' -f1 | rev | sed 's#:##g' )
        local end3_yyyymmdd=$( echo $mmyyyy | cut -c3-6 )$( echo $mmyyyy | cut -c1-2 )01
        
              
        #todo:  check if 3month or 10month run are completed
        if [[ -n $capr_yyyymmdd && -n $capric_yyyymmdd && -n $realend_yyyymmdd ]];then
                
            if (( $capr_yyyymmdd == $capric_yyyymmdd ));then
                #note:  submit this exp if cap_restart is ic date regardelss of winner or not
                :
            else
                if $blwin || $blrerun;then
                    if (( $capr_yyyymmdd >= $end3_yyyymmdd )) && (( $capr_yyyymmdd <= $realend_yyyymmdd ));then
                        :
                    else
                        continue
                    fi
                else
                    if (( $capr_yyyymmdd >= $capric_yyyymmdd )) && (( $capr_yyyymmdd <= $end3_yyyymmdd ));then
                        :
                    else
                        continue
                    fi
                fi
            fi
        else
            continue
        fi

        debug_filter $_dexp
        

        arr+=( $_dexp )

    done

    echo ${arr[@]}
}

exp_getHsjobs(){
    #todo:  get jobs with Hs status 
    local _str_status=Hs
    local _arrjobs_dexp=($( exp_getdexp $_str_status ))
    local _cmd_gjob=cmd_gjob_nas
    local _arrjid=($( $_cmd_gjob | grep -w $_str_status | tr -s '[:space:]' | cut -d' ' -f1 ))

    #todo:  delete jobs with Hs status
    printf '%s\n' ${_arrjid[@]} | xargs -i /PBS/bin/qdel {} 
    wait 

    echo "${_arrjobs_dexp[@]}"
}

clean_dir(){
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
    :
elif [[ "$hst" =~ "discover"* ]];then
    echo "this script works only on pfe (for now)";exit
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

#todo:  check tmp file size and create new if it is larger than 5kb
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
rundebug=0
#userthres=0
opte=false
optd=false
optf=false
optb=false
#opti=false
optt=false
optq=false
#================================================================================
#                                     Usage
#================================================================================
usage="$(basename $0) -- this is a program to:
        submit exps which are ready to run. If there are available reserved nodes
        for gcm_run.j, this program will edit gcm_run.j and submit it to the nodes. 

        Usage: ./$(basename "$0") [-hwb] [-d dexp] [-f filename] [-q qid] [-e expid]

        options:
            -b  run with a debug mode 
            -d  a full path to an experiment dir
            -e  submit experiments for a given experiment id (i.e. fcst, cice)
                *If there is rstid, enter [expid][rstid] (i.e. ose003)
            -f  a file with a list of experiment path
            -q  queue id (i.e. reserved node ID, normal, long)
            -t  a max number of experiments submitted per queue (default = 10 )
            -h  show this help text
            -w  write stdout/err in a file

        Note:  - By default, this program search and read data_$strscr* files. These
                 files have to have a list of full paths to exp dirs
               - When opt e is selected, experiments with only the entered ID are submitted.
"

while getopts 'hbwd:f:t:q:i:e:' option; do
    case "$option" in
        b)  optb=true;rundebug=1;; 
        h)  echo "$usage"; echo "$note"; exit 0;;
        d)  arrdexp=( $OPTARG );;
        e)  opte=true; strexpid=$OPTARG;;
        f)  optf=true; userinput=$OPTARG;;
        q)  optq=true; userinput_qid4grn=$OPTARG;; 
        t)  optt=true; thres_grn=$OPTARG;; 
        w)  writetofile=1;; 
        \?) echo "Invalid option: -$OPTARG" >&2
            exit 1;;
        :)  echo "Option -$OPTARG requires an argument." >&2
            exit 1;;
    esac
done

#NOTE: Save for later
#            -i  [gcm_run.j] experiment ID (Require option q) 
#                *If there is rstid, enter [expid][rstid] (i.e. ose003)
#        i)  ! $optq && die "Specify option q when opt i is entered.";
#            opti=true; strexpid=$OPTARG;; 

#================================================================================
#                               Check User Inputs
#================================================================================
inputcheck

#================================================================================
#                                 Set Variables
#================================================================================
#mid
hstshort=$( get_host )
dstdout=$cdir/stdout/$strscr/$strdout
dmess=$cdir/message
dtmpdata=$cdir/data/submit

fmessage=$dmess/message_${strscr}
ferr=$dmess/stderr_${strscr}
farchscr=run_gcmarch.sh
fexppri=data_${strscr}_priority

blqidexist=false
blwheader=false
blresnode=false

thres_grn_default=5

msg_hrinc=3
msg_hrpref=6
msg_subject="$hstshort.$strscr" 
feadd=$cdir/mailadd
[[ -f $feadd      ]] && eadds=$( echo $( misc_readfbyline $feadd ) | sed -e "s/ /;/g" )

[[ ! -f $fmessage ]] && touch $fmessage
[[   -f $fexppri  ]] && arrpri=($( misc_readfbyline $fexppri ))

#todo:  figure out which reserved nodes are available next $inputhour 
arrqidavail=($( res_qid_all ))
if (( ${#arrqidavail[@]} > 0 ));then 
    #todo:  figure out available qid_grn in $inputhour. 
    inputhour=8
    arrqidrun=($( res_Rqid ))
    arrqidend=($( res_qidexpire $inputhour ))
    arrqidbeg=($( res_qidstart  $inputhour ))
    arrqidrunavail_xhrs=($( printf '%s\n' ${arrqidrun[@]} ${arrqidend[@]} | sort | uniq -u )) 
       arrqidavail_xhrs=($( printf '%s\n' ${arrqidrunavail_xhrs[@]} ${arrqidbeg[@]} | sort | uniq ))

#arrqidavail_xhrs=( R13021885 R13036287 )    
    for qidavail_xhrs in ${arrqidavail_xhrs[@]};do 
        thismname=$( res_mname $qidavail_xhrs )
        if [[ "$thismname" == "rom_ait" ]];then 
            arrqidavail_xhrs_rome+=( $qidavail_xhrs ) 
        elif [[ "$thismname" != "ivy" ]];then 
            arrqidavail_xhrs_nonrome+=( $qidavail_xhrs )
        fi
    done 

    #note:  this array have model other than ivy
    arrqidavail_xhrs=( ${arrqidavail_xhrs_rome[@]} ${arrqidavail_xhrs_nonrome[@]} )
fi

#wmessage \@$LINENO
#ahand_print ${arrqidavail_xhrs_rome[@]} 
#wmessage
#ahand_print ${arrqidavail_xhrs_nonrome[@]} 
#wmessage
#ahand_print ${arrqidavail_xhrs[@]}


#todo:  set qid for gcm_run.j if optq is true
if $optq;then
    if [[ "$userinput_qid4grn" == normal || "$userinput_qid4grn" == long ]];then
        qid_grn="$userinput_qid4grn"

    else
        if [[ "${arrqidavail[@]}" == *"$userinput_qid4grn"* ]];then 
            qid_grn="$userinput_qid4grn"  
        else
            qid_grn=
        fi
    fi
fi


#Note:  03/29/2023 
#       This if statement has been changed since the code became confusing with 
#       different options. Keep an eye on this to make sure that this works.
#       06/01/2023
#       When qid_grn is normal or long, this part should be skipped. 
if [[ "$qid_grn" != "normal" && "$qid_grn" != long ]];then

    if [[ -n $qid_grn && -z $thres_grn ]] ;then
        mname=$( res_mname $qid_grn )
    
        if [[ "$mname" == "sky_ele" || "$mname" == "cas_ait" ]];then
            #note: this should be numcores_job=4880
            arrnumcore_async=($( exp_calccores_async "$mname" )) 
            numcores_job=${arrnumcore_async[4]}
    
        elif [[ "$mname" == "rom_ait" ]];then
            #note: this should be numcores_job=3750
            arrnumcore_async=($( exp_calccores_async "$mname" )) 
            numcores_job=${arrnumcore_async[4]}
        fi
    
        numtotcores=$( /PBS/bin/pbs_rstat -f $qid_grn | grep Resource_List.ncpus | rev | cut -d' ' -f1 | rev )
        thres_grn=$( echo "$numtotcores / $numcores_job" | bc )
    
    fi
fi

#note:  these qid don't have accurate values for "Resource_List.ncpus". 
#arrnode_exception=( R13228340 R13228440 )

[[ ! -d $dstdout  ]] && mkdir -p $dstdout
[[ ! -d $dmess    ]] && mkdir -p $dmess
[[ ! -d $dtmpdata ]] && mkdir -p $dtmpdata
#================================================================================
#                                  Main Process
#================================================================================
#letsbegin
#todo:  exit when dtmpdata dir is empty
dtmpdata_empty=$( find * -maxdepth 0 -type d -name $( basename $dtmpdata ) -empty 2>/dev/null)
[[ -n $dtmpdata_empty ]] && exit 

#todo:  get exp path based on priority
#if $opti && $optq ;then
if $optd ;then
    :
elif $optq ;then
    arrdexp=($( find $dtmpdata/* -maxdepth 0 -type f -name "data_submit_${strexpid}_*" -exec cat {} \; 2>/dev/null | sort | uniq | sort -V | grep -v archive ))

elif $optf;then
    arrdexp_tmp=($( misc_readfbyline $userinput ))
    arrdexp=($( printf '%s\n' ${arrdexp_tmp[@]} | sort -V | uniq ))

elif $opte;then
    arrdexp=($( find $dtmpdata/* -maxdepth 0 -type f -name "data_submit_${strexpid}_*" -exec cat {} \; 2>/dev/null | sort | uniq | sort -V | grep -v archive ))

elif ! $optf && (( ${#arrdexp[@]} == 0 ));then
    
    if [[ -n ${arrpri[@]} ]];then
        arrdexp_all=($( cat $dtmpdata/* 2>/dev/null | grep -v archive | sort | uniq | sort -V ))

        #todo:  prioritize exp
        for pri in ${arrpri[@]};do
            arrfsub+=($( find $dtmpdata/* -maxdepth 0 -type f -name "data_submit_*$pri*" 2>/dev/null ))
            arrdexp+=($( find $dtmpdata/* -maxdepth 0 -type f -name "data_submit_*$pri*" -exec cat {} \; 2>/dev/null | sort | uniq | sort -V | grep -v archive ))
        done
        
        #todo:  get non-prioritized experiments
        arrdexp_therest=($( printf '%s\n' ${arrdexp[@]} ${arrdexp_all[@]} | sort -V | uniq -u | sort -V ))

        #todo:  combine prioritized and non-prioritized exp in an array
        arrdexp+=( ${arrdexp_therest[@]} )
    fi
fi


#todo:  backup original data_submit_* files if no backup exists.
for fsub in ${arrfsub[@]};do
    fsub_name=$( basename $fsub )
    [[ ! -f $dstdout/$fsub_name.bak ]] && cp -p $fsub $dstdout/$fsub_name.bak
done

#todo:  get exps which are ready to be submitted. 
arrdexp_final=($( filter_dexp ${arrdexp[@]} ))

#todo:  find submitted jobs with Hs status and delete them and resubmit.
arrdexp_Hs=($( exp_getHsjobs ))
if (( ${#arrdexp_Hs[@]} > 0 ));then
    ! $blwheader && msg_wheader && wmessage $hstshort && blwheader=true
    wmessage "Jobs with Hs Status were deleted and will be resubmitted:"
    ahand_print ${arrdexp_Hs[@]}
    wmessage
    arrdexp_final=( ${arrdexp_Hs[@]} ${arrdexp_final[@]} )
fi

#todo:  exit there if no exp to submit
(( ${#arrdexp_final[@]} == 0 )) && exit

#todo: with debug mode, script stops here. 
if $optb;then 
    wmessage "Ready:"
    ahand_print ${arrdexp_final[@]} 
    wmessage 
    exit
fi

#todo:  submit gcm_run.j
for input in ${arrdexp_final[@]};do

    fgrn=$input/gcm_run.j

    [[ ! -f $fgrn ]] && continue
    
    #todo:  check the current queue name in gcm_run.j 
    queue_cur=$( grep "#PBS -q " $fgrn | grep -Ev '^##|^$|^ ' | rev | cut -d' ' -f1 | rev )

    if [[ -n $qid_grn ]];then
        thisqid=$qid_grn

        #todo:  define tresthold
        [[ -n $thres_grn ]] && thisthres=$thres_grn || thisthres=$thres_grn_default

    elif [[ -z $qid_grn ]] && (( ${#arrqidavail_xhrs[@]} > 0 ));then 

#wmessage \@$LINENO $queue_cur
#ahand_print ${arrqidavail_xhrs[@]} 

        #todo:  pick new queue if current queue is no longer available 
        if [[ -n $queue_cur ]] && [[ "${arrqidavail_xhrs[@]}" == *"$queue_cur"* ]];then 
            thisqid="$queue_cur"  
            mname=$( res_mname $thisqid )
#wmessage \@$LINENO $mname
#wmessage $input 
#wmessage $thisqid 
#wmessage $queue_cur
#exit
            arrnumcore_async=($( exp_calccores_async "$mname" )) 
#wmessage \@$LINENO 
#ahand_print ${arrnumcore_async[@]} 
            numcores_job=${arrnumcore_async[4]}
            numtotcores=$( /PBS/bin/pbs_rstat -f $thisqid | grep Resource_List.ncpus | rev | cut -d' ' -f1 | rev )
            thres_grn=$( echo "$numtotcores / $numcores_job" | bc )
            thisthres=$thres_grn

            #todo:  check number of jobs on thisqid 
             numexpr=$( /u/scicon/tools/bin/qstat -e -u $USER -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep $thisqid | grep -v ivy | grep -w "R"  | wc -l )
             numexpq=$( /u/scicon/tools/bin/qstat -e -u $USER -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep $thisqid | grep -v ivy | grep -w "Q"  | wc -l )
            numexphs=$( /u/scicon/tools/bin/qstat -e -u $USER -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep $thisqid | grep -v ivy | grep -w "Hs" | wc -l )
            numexp_tot=$( echo "$numexpr + $numexpq + $numexphs" | bc )

            (( $numexp_tot >= $thisthres )) && thisqid=
        elif [[ -z $queue_cur ]];then 
            continue
        fi


        if [[ -z $qid_grn && -z $thisqid ]];then 
            #todo:  figure out if this geosgcm runs on rome
            strgeosdir=$( grep -w GEOSDIR  $fgrn | grep setenv  | tr -s '[:space:]' | cut -d' ' -f3 )

            if [[ $strgeosdir == *"ROME"* ]] && (( ${#arrqidavail_xhrs_rome[@]} > 0 ));then 
                arrqidres=( ${arrqidavail_xhrs_rome[@]} ) 

            elif [[ $strgeosdir != *"ROME"* ]] && (( ${#arrqidavail_xhrs_nonrome[@]} > 0 ));then 
                arrqidres=( ${arrqidavail_xhrs_nonrome[@]} ) 

            else
                thisqid="normal"
                thisthres=$thres_grn_default
            fi

            if [[ -z $thisqid ]] && (( ${#arrqidres[@]} > 0 ));then 

                #todo:  select qid_grn which has the least number of jobs running or on queue
                arrqid_sortbynum=()
                for qidres in ${arrqidres[@]};do
                    numjobs=$( /u/scicon/tools/bin/qstat -u $USER -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep $qidres | wc -l ) 
                    mname=$( res_mname $qidres )
                    arrnumcore_async=($( exp_calccores_async "$mname" )) 
                    numcores_job=${arrnumcore_async[4]}
                    numtotcores=$( /PBS/bin/pbs_rstat -f $qidres | grep Resource_List.ncpus | rev | cut -d' ' -f1 | rev )
                    thisthres_grn_each=$( echo "$numtotcores / $numcores_job" | bc )
                    (( $numjobs < $thisthres_grn_each )) && arrqid_sortbynum+=( $numjobs:$qidres )
                done

#wmessage \@$LINENO 
#wmessage "$( printf '%s\n' ${arrqid_sortbynum[@]} | sort -n )" #| head -1 | cut -d':' -f2
                if (( ${#arrqid_sortbynum[@]} > 0 ));then 
                    thisqid=$( printf '%s\n' ${arrqid_sortbynum[@]} | sort -n | head -1 | cut -d':' -f2 )
                    mname=$( res_mname $thisqid )
                    arrnumcore_async=($( exp_calccores_async "$mname" )) 
                    numcores_job=${arrnumcore_async[4]}
                    numtotcores=$( /PBS/bin/pbs_rstat -f $thisqid | grep Resource_List.ncpus | rev | cut -d' ' -f1 | rev )
                    thres_grn=$( echo "$numtotcores / $numcores_job" | bc )
                    thisthres=$thres_grn
                else
                    break 
                fi
            fi
        fi

    elif [[ -z $qid_grn ]];then 
        thisqid="normal" 
        thisthres=$thres_grn_default
    fi

    #todo:  check number of jobs on thisqid 
     numexpr=$( /u/scicon/tools/bin/qstat -e -u $USER -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep $thisqid | grep -v ivy | grep -w "R"  | wc -l )
     numexpq=$( /u/scicon/tools/bin/qstat -e -u $USER -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep $thisqid | grep -v ivy | grep -w "Q"  | wc -l )
    numexphs=$( /u/scicon/tools/bin/qstat -e -u $USER -W 'fmt_Queue=-maxw 20' -W o=+Rank0 | grep $thisqid | grep -v ivy | grep -w "Hs" | wc -l )
    numexp_tot=$( echo "$numexpr + $numexpq + $numexphs" | bc )

    if [[ "$thisqid" == "normal" ]];then 
        (( $numexp_tot >= $thisthres )) && break
    else 

        #note:  use "continue" instead of "break". Script will choose available qid 
        #when multiple reservations are available. 
        (( $numexp_tot >= $thisthres )) && continue 
    fi
#wmessage \@$LINENO $queue_cur $thisqid
#exit
    #todo:  edit gcm_run.j if queue in gcm_run.j is different $qid_grn
    if [[ "$queue_cur" != "$thisqid" ]];then 
        ! $blwheader && msg_wheader && wmessage $hstshort && blwheader=true
        exp_editqgrn $thisqid $fgrn
    fi
    
    if [[ "$thisqid" == "normal" ]];then 
        ! $blwheader && msg_wheader && wmessage $hstshort && blwheader=true
        exp_editwalltime $fgrn "08:00:00" 
    fi

    ! $blwheader && msg_wheader && wmessage $hstshort && blwheader=true

    #+++++ cd dexp (start) +++++
    cd $input
    if (( $writetofile == 1 ));then
        wmessage "Submit : $input"
        /PBS/bin/qsub gcm_run.j >> $fmessage 2>&1
        status_qsub=$?
    else
        wmessage "Submit : $input"
        /PBS/bin/qsub gcm_run.j
        status_qsub=$?
    fi
    wmessage
    cd - >/dev/null
    #+++++ cd dexp ( end ) +++++
    
    (( $status_qsub == 0 )) && arrdexprm+=( $input )
done

if (( ${#arrdexprm[@]} > 0 ));then
    wmessage "... ${#arrdexprm[@]} jobs were submitted ..."
    wmessage
fi

#todo:  find data_submit_* file that contains exps that are successfully submitted
! $optf && rmexp_datasubmit ${arrdexprm[@]}

#todo:  delete data/submit if it's empty.
find $dtmpdata/* -maxdepth 0 -type f -empty -delete 2>/dev/null

#todo:  send email
[[ -f $fmessage ]] &&    sizef=$( stat --print='%s' $fmessage ) || sizef=0
[[ -f $ferr     ]] && sizeferr=$( stat --print='%s' $ferr )     || sizeferr=0

if (( $sizef > 0 || $sizeferr > 0 ));then 
    if (( $sizeferr > 0 ));then 
        msg_wheader_userdefined 40 "-" $( basename $ferr ) 
        wmessage "$( cat $ferr )"
        blrm=true
    fi

    msg_cyberpostman "$msg_subject" "$eadds" $fmessage $blmsgmutt
    (( $? == 0 )) && rm -f $fmessage
    $blrm && msg_newfile $ferr
fi


