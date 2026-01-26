#!/usr/bin/env bash
#@BATCH_TIME@ARCHIVE_T
##@ARCHIVE_PRES
##@ARCHIVE_QRES
#@ARCHIVE_P
#@ARCHIVE_Q
#@BATCH_JOBNAMEA@FCSTDATE@ENSEMBLE_MEMBER
#@GEOSS2S3_OPT2
#@BATCH_GROUP
#@BATCH_OUTPUTNAMEstderr_gcmarch
#@BATCH_JOINOUTERR

cmd_gjob_nccs() {
    /usr/slurm/bin/squeue -o "%.10i %.8u %.9P %.20j %.5C %.6D %.1T %.9M %.9L %15f" -a -u $USER 
}

submit_me(){
    $cmdsubmit $myname
}

cdir=@DFCST/@FCSTDATE/@ENSEMBLE_MEMBER/archive
fcstdate=@FCSTDATE
ensm=@ENSM
darc=@DARCH
strscr=@STRSCR
myname=${strscr}_@STRID.sh
hostarc=@GCMARCH_HOSTARC
cmdsubmit=/usr/slurm/bin/sbatch
blcompleted=false
fcomp=$cdir/${strscr}_completed

[[ -f $fcomp ]] && exit
    
rsync -azRPW --quiet --files-from=@INPUTFILE / $hostarc:$darc/

if (( $? == 0 ));then 
    touch $fcomp
    arrrjobs=($( cmd_gjob_nccs | tail -n +2 | tr -s '[:space:]' | sed 's#^ *##g' | cut -d' ' -f4 | grep A${fcstdate}${ensm}_ ))

    if (( ${#arrrjobs[@]} > 0 ));then 
        exit
    elif (( ${#arrrjobs[@]} == 1 ));then 
        $cmdsubmit run_gcmarch.sh
    fi
else
    submit_me
fi

exit

