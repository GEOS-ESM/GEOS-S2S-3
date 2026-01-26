#!/bin/csh -f

#================================================================================
#                    Batch Parameters for Post-Processing Job
#================================================================================
#@BATCH_TIME@S2SMKDYMEAN_WALLTIME
#@S2SMKDYMEAN_PRES
#@S2SMKDYMEAN_QRES
#@S2SMKDYMEAN_P
#@S2SMKDYMEAN_Q
#@BATCH_JOBNAME@S2SMKDYMEAN_STRSCR.@FCSTDATE@ENSEMBLE_MEMBER.@COLLECTION
#@BATCH_GROUP
#@BATCH_OUTPUTNAME@S2SMKDYMEAN_STRSCR.@FCSTDATE@ENSEMBLE_MEMBER.@COLLECTION.o
#@BATCH_JOINOUTERR
#@S2SMKDYMEAN_BLQOS
#@GEOSS2S3_OPT2

#================================================================================
#                                System Settings
#================================================================================
umask 022

limit stacksize unlimited

@SETENVS
#================================================================================
#                  Architecture Specific Environment Variables
#================================================================================
setenv ARCH `uname`

setenv SITE      @SITE
setenv GEOSDIR   @GEOSDIR 
setenv GEOSBIN   @GEOSBIN 
setenv GEOSUTIL  @GEOSSRC/GMAO_Shared/GEOS_Util

source $GEOSBIN/g5_modules

if ( $SITE == NAS ) then
    set cmd_cdo = "cdo -O -s -P 8 --no_warnings"

else if ( $SITE == NCCS ) then 
    setenv PATH $BASEDIR/Linux/bin:$PATH
    set cmd_cdo = "cdo -O -s -P 8 -w"
endif

setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BASEDIR}/${ARCH}/lib

module load nco
module load cdo
module list 

echo 
echo 
   
#================================================================================
#                                 Set Variables
#================================================================================
#mid
set     strscr = @S2SMKDYMEAN_STRSCR
set blexe_dexp = @S2SMKDYMEAN_BLEXE_DEXP
set       dexp = @EXPDIR
set       cdir = @S2SMKDYMEAN_CDIR 
set   fcstdate = @FCSTDATE
set       ensm = @ENSEMBLE_MEMBER
set       coll = @COLLECTION 
set  arryyyymm = ( @S2SMKDYMEAN_ARRYYYYMM ) 
set   numdyout = @S2SMKDYMEAN_NUMDYOUT 
set numthreads = @S2SMKDYMEAN_NUMTHREADS
set  blthreads = @S2SMKDYMEAN_BLTHREADS
set    blwhist = @S2SMKDYMEAN_BLWHIST

set  blnum    = FALSE
set  blsiz    = FALSE

#note:  TRUE if this scripts is submitted by dexp/s2s_util/s2smkdymean.sh ("executed within dexp") 
if ( $blexe_dexp == TRUE ) then 
    set     dscratch = $cdir/scratch/$coll 
    set         ddes = $dexp/$coll
    set        fcomp = $cdir/$strscr.${fcstdate}${ensm}.${coll}.completed
else
    set     strexpid = @STREXPID
    set        dmess = $cdir/message
    set         dout = $cdir/output/$strscr/$strexpid
    set     dscratch = $dout/scratch/$fcstdate/$ensm/$coll 
    set         ddes = @S2SMKDYMEAN_DDESFINAL 
    set        fcomp = $dout/$strscr.${fcstdate}${ensm}.${coll}.completed
    set         fscr = $dscratch/$strscr.${fcstdate}${ensm}.$coll.j
    set      fstdout = $dscratch/$strscr.${fcstdate}${ensm}.$coll.o
endif

if ( -e $fcomp ) then 
    echo "file exist: `basename $fcomp`. exit "
    exit
endif

if ( ! -e $ddes     ) mkdir -p $ddes
if ( ! -e $dscratch ) mkdir -p $dscratch

#================================================================================
#                                  Main Process
#================================================================================
#letsbegin

if ( $blexe_dexp == FALSE ) then 
    echo "Create symlinks to `basename $fscr` and `basename $fstdout` in $dmess"

    set   ln_fstdout = `echo $fstdout | sed "s#$cdir/##" `
    set      ln_fscr = `echo $fscr    | sed "s#$cdir/##" `
    set fstdout_base = `basename $fstdout`
    set    fscr_base = `basename $fscr`

    #+++++ cd dmess (start) +++++
    cd $dmess
    if ( -l $fstdout_base ) unlink $fstdout_base
    if ( -l $fscr_base    ) unlink $fscr_base
    if ( -e $fstdout      ) ln -s  ../$ln_fstdout .
    if ( -e $fscr         ) ln -sf ../$ln_fscr    .
    cd - >/dev/null
    #+++++ cd dmess ( end ) +++++
    
    if ( ! -e $fstdout ) echo "file does NOT exist: $fstdout"
    if ( ! -e $fscr    ) echo "file does NOT exist: $fscr"
endif

set arrfout_all = ()
set arrftar_all = ()
foreach yyyymm ( $arryyyymm )

    set   dyyyymm = $dscratch/$yyyymm
    #tar out: 20180610.sfc_tavg_3hr_glo_L720x361_sfc.dailymean.201806.nc4.tar
    set fouttar = $fcstdate.$coll.dailymean.$yyyymm.nc4.tar
    
    #+++++ cd dyyyymm (start) +++++
    cd $dyyyymm 
    set arrdyyyymmdd = `find * -maxdepth 0 -type d -name "[1-9]???????" | sort -V `
    cd - >/dev/null
    #+++++ cd dyyyymm ( end ) +++++
    
    set numtot = `echo $#arrdyyyymmdd`
    
    echo 
    echo 
    
    set arrfout = ()
    set arrproc = () 
    foreach yyyymmdd ( $arrdyyyymmdd )

        set    yyyymm =  `echo $yyyymmdd | cut -c1-6`
        set dyyyymmdd = $dyyyymm/$yyyymmdd

        #Ref: 20180610.sfc_tavg_3hr_glo_L720x361_sfc.dailymean.20180610.nc4
        set foutyyyymmdd_base = $fcstdate.$coll.$yyyymmdd
        set         fout_mean = $fcstdate.$coll.dailymean.$yyyymmdd.nc4
   
        #+++++ cd dyyyymmdd (start) +++++ 
        cd $dyyyymmdd 
        
        if ( $blthreads == TRUE ) then
    
            echo "Making `basename $fout_mean`"

            if ( $SITE == NCCS ) then
                $cmd_cdo daymean -mergetime  ${foutyyyymmdd_base}_*z.nc4  ../$fout_mean & 
            else if ( $SITE == NAS ) then 
                $cmd_cdo daymean -mergetime "${foutyyyymmdd_base}_*z.nc4" ../$fout_mean &
            endif

            set proc = ${!}
            set arrproc = ( $arrproc $proc )

            set arrfout     = ( $arrfout     $fout_mean ) 
            set arrfout_all = ( $arrfout_all $yyyymm/$fout_mean ) 
    
            if ( $#arrproc >= $numthreads ) then
                echo "# of Background Threads: $#arrproc"
                echo "                Threads: ` echo $arrproc | sed 's# #, #g' ` "
                echo "    wait... ` date ` "
                wait
                echo "    done... ` date ` "
                echo
                echo
    
                set arrproc = () 
    
            endif

        else    
            echo "Making `basename $fout_mean`"

            if ( $SITE == NCCS ) then
                $cmd_cdo daymean -mergetime  ${foutyyyymmdd_base}_*z.nc4  ../$fout_mean
            else if ( $SITE == NAS ) then 
                $cmd_cdo daymean -mergetime "${foutyyyymmdd_base}_*z.nc4" ../$fout_mean
            endif

            set arrfout     = ( $arrfout     $fout_mean           ) 
            set arrfout_all = ( $arrfout_all $yyyymm/$fout_mean ) 

            #todo:  add and delete global attribute
            ncatted -h -O -a creator,global,c,c,$USER $fout_mean $fout_mean.tmp

            ncatted -h -O -a   Title,global,d,, $fout_mean.tmp
            ncatted -h -O -a History,global,d,, $fout_mean.tmp
            ncatted -h -O -a Contact,global,d,, $fout_mean.tmp
            ncatted -h -O -a Comment,global,d,, $fout_mean.tmp
            ncatted -h -O -a CDI,global,d,,     $fout_mean.tmp
            ncatted -h -O -a cdo_openmp_thread_number,global,d,, $fout_mean.tmp
            
            
            if ( $blwhist == FALSE ) then 
                ncatted -h -O -a history,global,d,, $fout_mean.tmp
            endif

            /bin/rm -f $fout_mean
            mv $fout_mean.tmp ../$fout_mean

    
        endif
        
        cd - >/dev/null
        #+++++ cd dyyyymmdd ( end ) +++++ 
    
    end 
    
    if ( $#arrproc > 0 ) then
        echo "# of Background Threads: $#arrproc"
        echo "                Threads: ` echo $arrproc | sed 's# #, #g' ` "
        echo "    wait... ` date ` "
        wait
        echo "    done... ` date ` "
        echo
        echo
    endif
    
    echo 
    echo "Check output files ..."
    echo 
    
    #todo:  check output files size and total # of outputs.
    if( $blthreads == TRUE ) then
        #+++++ cd dyyyymm (start) +++++
        cd $dyyyymm 

        foreach fout ( $arrfout )
            ncatted -h -O -a creator,global,c,c,$USER $fout $fout.tmp

            ncatted -h -O -a   Title,global,d,, $fout.tmp
            ncatted -h -O -a History,global,d,, $fout.tmp
            ncatted -h -O -a Contact,global,d,, $fout.tmp
            ncatted -h -O -a Comment,global,d,, $fout.tmp
            ncatted -h -O -a CDI,global,d,,     $fout.tmp
            ncatted -h -O -a cdo_openmp_thread_number,global,d,, $fout.tmp

            if ( $blwhist == FALSE ) then 
                ncatted -h -O -a history,global,d,, $fout.tmp
            endif

            mv $fout.tmp $fout
        end

        cd - >/dev/null
        #+++++ cd dyyyymm ( end ) +++++
    endif
    
    set arrfsiz = `find $dyyyymm/* -type f -name "$fcstdate.$coll.dailymean.*.nc4" -empty | sort -V | uniq `
    
    if ( $#arrfout == $numtot ) set blnum = TRUE
    if ( $#arrfsiz == 0       ) set blsiz = TRUE
    
    echo 
    
    if( $blnum == FALSE ) then 
        echo "ERROR - total number of output files = $#arrfout ( should be $numtot )."
    endif
    
    if( $blsiz == FALSE ) then 
        echo "ERROR - there are empty output files."
        printf '%s\n' $arrfout | xargs -i stat --printf='%s %n\n' {} | xargs -n 2 bash -c 'echo "    " $( numfmt --to=iec $1 ) $2' bash
    endif

    #todo:  create tar flies                 
    if( $blnum == TRUE && $blsiz == TRUE ) then 
        #+++++ cd dyyyymm (start) +++++
        cd $dyyyymm
        
        tar cf $fouttar $arrfout
        set status_tar = $?
    
        if ( $status_tar == 0 ) then 
            mv $fouttar $ddes/
            set arrftar_all = ( $arrftar_all $fouttar ) 
        endif
    
        cd - >/dev/null
        #+++++ cd dyyyymm ( end ) +++++
    endif

end

set arrfouttar = `find $ddes/* -type f -name "$fcstdate.$coll.dailymean.[0-9]?????.nc4.tar" -not -empty | sort -V | uniq `

if ( $#arryyyymm == $#arrfouttar ) then 

    #+++++ cd $dscratch (start) +++++
    cd $dscratch
    echo "Daily Mean Calculations - COMPLETED"
    echo "Total # of Dailymean Files: $#arrfouttar ( supposed to be $numdyout )"
    printf '%s\n' $arrfout_all | sort -V | xargs -i stat --printf='%s %n\n' {} | xargs -n 2 bash -c 'echo "    " $( numfmt --to=iec $1 ) $2' bash
    cd - >/dev/null
    #+++++ cd $dscratch ( end ) +++++
    
    echo  

    #+++++ cd $ddes (start) +++++
    cd $ddes
    echo "Total # of Output Tar Files: $#arrftar_all ( supposed to be $#arryyyymm )"
    stat --printf='%s %n\n' $arrftar_all | xargs -n 2 bash -c 'echo "    " $( numfmt --to=iec $1 ) $2' bash

    touch $fcomp 
    echo 
    echo "A marker has been created:"
    echo "     $fcomp"
    cd - >/dev/null
    #+++++ cd $ddes ( end ) +++++

    if ( $blexe_dexp == TRUE ) then 
        #todo: clean scratch dir
        if ( -e $dscratch ) /bin/rm -rf $dscratch

    else
        echo 
        echo "Output Destination Directory:"
        echo "    $ddes" 
    endif

endif

exit

