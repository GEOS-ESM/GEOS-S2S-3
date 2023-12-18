#!/usr/bin/env bash

#note: for debugging
#ref: https://is.gd/kxHve1
#exec 5> debug_$(basename "$0")
#BASH_XTRACEFD="5"
#PS4='$LINENO: '
#set -x


#description:   
inputcheck(){
    [[ ! -z "$1" ]] && fin=$1 || die "req1 is a required input"
    [[ ! -z "$2" ]] && fin=$1 || die "req2 is a required input"
}

transout_s2s() {
    local _dexp=$1;shift
    local _darc=$1;shift
    local _strid=$1;shift
    local _fthishis=$1;shift
    local _dscratch1=$1
    local _dscratch=$_dscratch1/$_strid
    local ftarnc=$dscratch/${strscr}_${_strid}_tar_nc
    local ii coll

    [[ ! -d $_dscratch ]] && mkdir -p $_dscratch

    #todo: get all collection names from HISTORY.rc
    local arrcollmon_mapl=($( exp_getcollfreq m $_fthishis ))
    local arrcollmonpost=($( exp_getcollfreq n $_fthishis ))

    #todo:  copy mapl monthly output
    local arrfmon_mapl=($( printf '%s\n' ${arrcollmon_mapl[@]} | xargs -i bash -c "find $_dexp/{}/* -maxdepth 0 -type f -name '$fcstdate.{}.${yyyymm_next}??_[0-9]*z.nc4'" 2>/dev/null ))
    printf '%s\n'  ${arrfmon_mapl[@]} | xargs -i cp -up {} $_dscratch/  
    (( ${#arrfmon_mapl[@]} > 0 )) && printf '%s\n' ${arrfmon_mapl[@]} | xargs -i basename {} >> $fname_all

    #todo:  copy monthly output
    local arrfmonpost=($( printf '%s\n' ${arrcollmonpost[@]} | xargs -i echo $_darc/{}/$fcstdate.{}.monthly.$yyyymm_next.nc4 ))
    printf '%s\n' ${arrfmonpost[@]} | xargs -i ssh -q lfe cp -up {} $_dscratch/
    (( ${#arrfmonpost[@]}  > 0 )) && printf '%s\n' ${arrfmonpost[@]}  | xargs -i basename {} >> $fname_all

    #todo:  extract one file from daily, copy it to _dscratch, and rename it.
    local arrfdaypost=($( printf '%s\n' ${arrcollmonpost[@]} | xargs -i echo $_darc/{}/$fcstdate.{}.daily.$yyyymm_next.nc4.tar ))

    for fday in ${arrfdaypost[@]};do

        #todo: save names of files-to-be-copied in $fname_all text.
        [[ -n $fday ]] && echo $( basename $fday ) >> $fname_all

        local thisf=$( ssh -q lfe tar -tf $fday | head -1 )
        ssh -q lfe tar -xvf $fday -C $_dscratch $thisf > /dev/null

        #+++++ cd $_dscratch (start) +++++ 
        cd $_dscratch
        
        #todo: add ".daily." to a file name (i.e. dec27.geosgcm_ocn3d.daily.20191227_0900z.nc4 )
        local thisff=''
        IFS='.' read -ra var <<< "$thisf"
        for((a=0; a<${#var[@]}; a++)); do
            if [[ $a == 2 ]]; then
                thisff="$thisff""daily.""${var[a]}""."
            else
                thisff="$thisff""${var[a]}""."
            fi
        done
    
        #todo: remove the last character, ".", from $thisff
        local thisfff=${thisff:0:(${#thisff}-1)}
    
        #todo: rename the daily file
        mv $thisf $thisfff
    	echo $fday $thisf >> $cdir/$ftarnc 

    	cd - > /dev/null
        #+++++ cd $_dscratch ( end ) +++++ 
    done
    
    #todo: copy mom output
    local dmom=$_darc/$strmom
    ssh -q lfe find $dmom/* -type f -name "$strmom_search\*" 2>/dev/null | sort -V |  tail -1 | xargs -i ssh -q lfe cp -up {} $_dscratch/

    return
}

renamenc(){
    #FOR V3, THIS FUNCTION MAY NOT BE NECESSARY.
    #description:    rename outputs files for pyrsob

    #note:
    #change name this:
    #dec27.geosgcm_00zins.daily.20191227_0000z.nc4
    #to this:
    #out_dec27.instD_3d_ccc_Np.geosgcm_00zins.daily.20191227_0000z.nc4

    local _strid=$1;shift
    local fnames=concatnames_${_strid}.txt

    local focean=$( find $_dscratch/* -type f -name "$strmom_search*" 2>/dev/null )
    if [[ -n $focean ]];then
        focean=$( basename $focean )
        
        IFS='_' read -ra ocean <<< "$focean"
        local foceannew=$_strid"."${ocean[0]}"."${ocean[1]}
        [[ ! -f $foceannew ]] && mv $focean $foceannew
    fi 

    #+++++ cd _dout_geoos (start) +++++
    cd $_dscratch

    #todo: get all output file names in an array
    shopt -s nullglob
    arrout=(*.nc4)
    arrout+=(*.nc)
    shopt -u nullglob
    
    cd - > /dev/null
    #+++++ cd _dout_geoos ( end ) +++++
    
    #todo: read concatnames.txt and place data in two separate arrays.
    while IFS= read -r var ; do
    	IFS=':' read -ra var2 <<< "$var"
    	arrorg+=(${var2[0]})
    	arrconcatname+=(${var2[1]})
    	arrtitle+=("${var2[2]}")
    done < "$fnames"
    
    
    cnt=0
    #todo: rename the output nc files by adding 'freq_dims_group_HV' info and add title to renamed output file.
    for outf in ${arrout[@]}; do
    	for (( i=0; i< ${#arrorg[@]}; i++)); do
    		x=${arrorg[i]}
    		y=0
    
    		if [[ $outf == $x"."*".nc4" ]]; then 
    			y=1
    		elif [[ $outf == $x"."*".nc" ]]; then 
    			y=1
    		elif [[ $outf == *".""$x""."* ]]; then
    			y=1
    		fi	
    		
    		if [[ $y == 1 ]]; then
    			IFS='.' read -ra orgname <<< "$outf"
    
    			tot=$((${#orgname[@]} + 1))
    
    			for (( ii=0; ii<$tot; ii++ )); do
    				if [[ $ii == 0 ]]; then
    					newname=${orgname[$ii]}
    
    				elif [[ $ii == 1 ]]; then
    					newname="$newname""."${arrconcatname[i]}
    				else
    					newname="$newname""."${orgname[$(( ii - 1 ))]}
    
    				fi
    				#newname=${orgname[0]}"."${arrconcatname[i]}"."${orgname[1]}"."${orgname[2]}"."${orgname[3]}"."${orgname[4]}
    			done
    			#echo $outf $newname
    			#echo $newname
    
    			#mv $_dscratch/$outf $_dscratch/$newname
    			cp -p $_dscratch/$outf $_dscratch/$newname
    
    			$dirnco/ncatted -h -a Title,global,o,c,"${arrtitle[i]}" $_dscratch/$newname $_dscratch/"out_"$newname
    			rm -f $_dscratch/$newname
    
    			cnt=$(( cnt + 1))
    		fi
    	done
    done


}

runpyrob(){

    local _strid=$1;shift
    local _dscratch1=$1
    local _dscratch=$_dscratch1/$_strid
    
    [[ ! -d $_dscratch ]] && mkdir -p $_dscratch 
    
    wmessage "...running pyrob..."
    
    export cdir _dscratch ftype
    
    ./pyrob.sh
    
    local pyrobout=$?
    if [[ $pyrobout == 1 ]];then 
    	die "PyRob Failed" 
    elif [[ $pyrobout == 0 ]];then
    	#todo: move output .doc to the current directory
        if [ -f $_dscratch/spec.doc ]; then 
            mv $_dscratch/spec.doc $cdir/spec_$_strid.doc 
            arrfdoc+=("spec_"$_strid".doc")
        elif [ -f $_dscratch/spec.txt ]; then 
            mv $_dscratch/spec.txt $cdir/spec_$_strid.txt 
            arrfdoc+=("spec_"$_strid".txt")
        fi
    fi
    
    wmessage "...output files are:"
    #todo: print the file name if it exists.
    if [[ ${#arrfdoc} > 0 ]] ; then
        for f in ${arrfdoc[@]}; do
            wmessage $f
        done
    fi
}


clean_dir() {
    [[ -f *.pyc ]] && rm -f *.pyc
    [[ -n $flock && -f $flock ]] && rm -f $flock
    return
}

#================================================================================
#                                     Begin
#================================================================================
#beg
hst=$( hostname )
if [[ "$hst" =~ "pfe"* || "$hst" =~ "lfe"* ]];then
    :
elif [[ "$hst" =~ "discover"* ]];then
    echo "this script works only on pfe";exit
else 
    exit
fi

strscr=$(basename "$0" | cut -d'_' -f2 | cut -d'.' -f1 )
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd )"
cdir=$SCRIPTPATH

[[ -z $cdir || ! -d $cdir ]] && echo "cdir is undefined" && exit

blcronrun=false
pppid=$( ps h -o ppid= $PPID )
p_command=$( ps h -o %c $pppid )
[[ "$p_command" == "cron" ]] && blcronrun=true


$blcronrun && echo "cron execution of this script is not allowed. Exit" && exit

cd $cdir

#[[ -z ${SCR+x} ]] && source ~/.bashrc

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
 
trap clean_dir EXIT

if [[ -f $cdir/func_fcst.sh ]];then
    source $cdir/func_fcst.sh
else
    echo "$cdir/func_fcst.sh does not exist";exit
fi

writetofile=0


#================================================================================
#                                     Usage
#================================================================================
#ref: https://goo.gl/BUBwPV
usage="$(basename "$0") -- this is a program to:
        create s2s seasonal forecast spec document.
        
        Usage: program [-chw]

        options:
            -c  clean unwanted files
            -h  show this help text
            -w  write stdout/err in a file
"

file=

while :; do
    case $1 in
        -h|-\?|--help)  echo "$usage" && exit;; 
           -f|--file )  [[ "$2" ]] && file=$2 && shift || die "ERROR: "--file" requires a non-empty option argument.";;
           --file=?* )  file=${1#*=};; # Delete everything up to "=" and assign the remainder.
             --file= )  die "ERROR: "--file" requires a non-empty option argument.";;
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
strf=${arrposarg[i]}; i=$(( i + 1 ))
[[ -n $strf && -f $strf ]] && source $strf
[[ -z $DFCST  ]] && die "DFCST is undefined"
[[ -z $DARCH  ]] && die "DARCH is undefined"
#inputcheck
#================================================================================
#                             Set Host Specific Vars
#================================================================================
#todo: check the name of system (discover or pfe) and set variables.
if [[ $hst == 'pfe'* ]];then
    #do something here (i.e. load modules, location of applications etc)
    #. /usr/share/modules/init/bash
    #module load nco
    #module load cdo/1.9.0
    :
elif [[ $hst == 'discover'* ]];then 
    #do something here (i.e. load modules, location of applications etc)
    #. /usr/share/modules/init/bash
    #module load other/cdo-1.9.1
    #module load other/comp/gcc-5.3-sp3 other/SSSO_Ana-PyD/SApd_4.3.1_py2.7_gcc-5.3-sp3
    :
fi

#module load
#module list
#================================================================================
#                                 Set Variables
#================================================================================
#mid
dstdout=$cdir/stdout/$strscr
dmess=$cdir/message
dscratch=$cdir/output/$strscr/scratch

fmessage=$dmess/message_$strscr
 fstderr=$dmess/stderr_$strscr
strmom=MOM_Output
strmom_search=ocean_daily

fname_all=$dscratch/${strscr}_allfiles

#note:  set output file type (text or .doc). "t" for text. "r" for
ftype=t
dexp=$DFCST/19920401/ens1
darc=$DARCH/19920401/ens1
fhis=/nobackupp2/knakada/GEOS_Models/knakada_s2s3_unstable_FullModel_20211203_ROME/GEOSodas/src/Applications/GEOSgcm_App/HISTORY.AOGCM-S2Sv3_2.rc.tmpl
strid=fcst

set_rstfcstdate $dexp
fcstdate_yyyymm=$( date -d $fcstdate +%Y%m )
yyyymm_next=$( fcal_nextmonth $fcstdate_yyyymm )

ddata=$cdir/2.rundata

#todo: create array with file names.
#shopt -s nullglob
#cd $ddata
#arrdata=(*.*)
#cd - > /dev/null
#shopt -u nullglob

#todo: copy all necessary scripts from "scripts" dir to the current directory and run.
#cp -p $ddata/* .


lastm=0

#RUNCHILD=1

#(( $writetofile == 1 )) && msg_newfile $fmessage

[[ ! -d $dmess     ]] && mkdir -p $dmess
[[ ! -d $dstdout   ]] && mkdir -p $dstdout
[[ ! -d $dscratch ]] && mkdir -p $dscratch
#================================================================================
#                                  Main Process
#================================================================================
#main
msg_wheader

#todo:  copy outputs
#transout_s2s $dexp $darc $strid $fhis $dscratch

renamenc $strid

#todo:  runpyrob
#runpyrob $strid $dscratch

#module purge

exit
