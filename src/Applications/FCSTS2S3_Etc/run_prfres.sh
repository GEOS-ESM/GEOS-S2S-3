#!/bin/bash

#description: Extrac profiling results from exp's stdout file. 
clean_dir() {
    #if [[ -f $tcase ]]; then rm  -f $tcase ;fi
    if [[ -f $fdata ]]; then rm  -f $fdata ; fi
    if [[ -f $flock ]]; then rm  -f $flock ; fi
    return
}

crtarr_totalpes(){
    #todo: create an array based on search words
    local _cpus=$1
    local _freq=$2
    local sword_totalpes="Total PEs"
    local sword_agcmdate="AGCM Date"
    local ftmp

    cd $dexp
    shopt -s nullglob
    arr_ftmp=( *nas.nasa.gov* )
    shopt -u nullglob

    #todo: find job segment that used total cpus of $_cpus
    for ftmp in ${arr_ftmp[@]}; do
        cpu_inf=($(grep "$sword_totalpes" $ftmp | head -1 | tr -s '[:space:]' | rev | cut -d' ' -f1 | rev ))
        if [[ "$cpu_inf" != "$_cpus" ]];then arr_del+=($ftmp);fi
    done
    for del in ${arr_del[@]};do
        arr_ftmp=( "${arr_ftmp[@]/$del}")
    done
    arr_del=()

    #todo: find job segment that ran for the frequency of $_freq
    for ftmp in ${arr_ftmp[@]}; do
        #todo: find currently running job based on yyyymm in cap_reastart and ftmp file name.
        c_yyyymm=$( echo cap_restart | cut -d' ' -f1)
        f_yyyymm=$( echo $ftmp | cut -d'.' -f6 )
        if [[ $c_yyyymm == $f_yyyymm ]]; then arr_del+=($ftmp); fi

        #todo: get starting yyyymm and ending yyyymm
        _start=($(grep "$sword_agcmdate" $ftmp | head -1 | tr -s '[:space:]' | cut -d'T' -f1 | cut -d':' -f2 | sed "s/^[ \t]*//" | sed 's#/##g' ))
        _end=($(grep "$sword_agcmdate" $ftmp | tail -2 | head -1 | tr -s '[:space:]' | cut -d'T' -f1 | cut -d':' -f2 | sed "s/^[ \t]*//" | sed 's#/##g' ))

        yyyy_start=${_start:0:4}
        mm_start=${_start:4:2}
        yyyy_end=${_end:0:4}
        mm_end=${_end:4:2}

        if [[ "${mm_start:0:1}" == 0 ]]; then mm_start=$(echo $mm_start | sed 's/^0*//' ); fi
        if [[ "${mm_end:0:1}" == 0 ]]; then mm_end=$(echo $mm_end | sed 's/^0*//' ); fi

        #todo: figure out number of months.
        if (( $mm_start < $mm_end ));then 
            freq_inf=$( echo "${mm_end}-${mm_start}+1"| bc -l )
        elif (( $mm_start == $mm_end ));then 
            freq_inf=1
        else 
            freq_inf=$( echo "12-${mm_start}+1+${mm_end}" | bc -l );fi

        if (( $freq_inf != $_freq ));then arr_del+=($ftmp);fi
    done
    for del in ${arr_del[@]};do
        arr_ftmp=( "${arr_ftmp[@]/$del}")
    done

    cd - >/dev/null
    echo ${arr_ftmp[@]}
#    echo ${arr_del[@]}
}
#================================================================================
#                              Beginning of Script
#================================================================================
#beg
cdir=$(pwd)
strscr=$(basename "$0" | cut -d'_' -f2 | cut -d'.' -f1 )

flock=$cdir/${strscr}.lock
ftmp=$cdir/stdout/rundatetime/tmp_$strscr

if [[ -f $ftmp ]];then 
    mkdir -p $( dirname $ftmp ) && touch $ftmp
fi

#todo:  check tmp file size and create new if it is larger than 5kb
if [[ ! -f $ftmp ]];then touch $ftmp;fi

#todo:  check tmp file size and create new if it is larger than 5kb
stmp=$( find $ftmp -printf "%s\n" )
if (( $stmp > 5000 ));then
    rm -f $ftmp;touch $ftmp
fi

if [[ -f $flock ]];then
    echo "$(TZ=America/New_York date +'%m/%d/%Y %H:%M' ) ... $flock exists..." >> $ftmp
else
    echo $(TZ=America/New_York date +'%m/%d/%Y %H:%M' ) >> $ftmp
fi

#todo:  lock this script (ref: https://www.putorius.net/?p=2607)
#note:  -C  Prevent output redirection using '>', '>&', and '<>' from overwriting existing files.  -o noclobber
#       ref:https://ss64.com/bash/set.html
if set -C; 2>/dev/null >$flock; then
    trap clean_dir EXIT
    source $SCR/bf/bf0001_fcal.sh
    source $SCR/bf/bf0005_fhand.sh
    source $SCR/bf/bf0007_exp.sh
else
    exit
fi
 
runmt=0
runallprof=1    #<== get all profiling results from $CEXP or user input

trap clean_dir EXIT
#================================================================================
#                                     Usage
#================================================================================
#clear
usage="$(basename "$0") [-h] -- this is a program to:
        extract profiling results

required inputs:
    -d  a fullpath of experiment dir
    
options:        
    -h  show this help text
    -c  clean unwanted files
"

#     1.Extract profiling results from experiments' standard output files if 
#      profiling results are available. And write all the results in a file. 
#        process specific required input:     mt
#    -p  specify a process (mt or comp)

#while getopts ':hcd:p:' option; do
while getopts ':hcd:' option; do
    case "$option" in
        h)  echo "$usage"
            exit 1;;
        c) 
            clean_dir
            exit 1;;
#        p)
#            if [[ $OPTARG == "mt" ]];then
#                runmt=1         #<== grep "Model Throughput" in exp's stdout files in $CEXP 
#                                #       or user input exp. Append the results and 
#                                #       write them in a output file.
#                runallprof=0
#            
#            elif [[ $OPTARG == "comp" ]];then #note:'comp' for comparison 
#                #runt4gcm=0
#                runmt=0
#                runallprof=1    #<== get all profiling results from $CEXP or user input
#                                #       exp. And ... do blah blah blah.
#            fi
#            ;;

        d)  dexp=$OPTARG;;
        \?)
            echo "Invalid option: -$OPTARG" >&2
            exit 1;;
        :)
            echo "Option -$OPTARG requires an argument." >&2
            exit 1;;
    esac
done

#================================================================================
#                          Input Check & Sourcing Files
#================================================================================
if [[ "$dexp" == "" ]];then
    echo "A full path to an experiment dir is required input. Use -d option to enter the path. exit."
    exit 1
fi
if (( $runmt == 0 )) && (( $runallprof == 0));then
    echo "Process input is required. Use -p option to enter your option (comp or mt)."
    exit 1
fi
#================================================================================
#                             Set Host Specific Vars
#================================================================================
#todo: check the name of system (discover or pfe) and set variables.
hst=`hostname`
if [[ $hst == 'pfe'* ]] || [[ $hst == 'discover'* ]];then 
    if [[ $hst == 'pfe'* ]];then
        . /usr/share/modules/init/bash
        #module load tensorflow/1.6
        #source activate anapy27
        module load python/2.7.15
        
    elif [[ $hst == 'discover'* ]];then
        . /usr/share/modules/init/bash
        module purge
        module load other/comp/gcc-5.3-sp3
        module load other/SSSO_Ana-PyD/SApd_4.2.0_py2.7_gcc-5.3-sp3 
        
    fi
else
    echo "hostname is unknown."
    echo "exit the program."
    exit
fi

#================================================================================
#                    Set Global Variables and Source Scripts
#================================================================================
exp=$( basename  $dexp )
expu=$( echo $( exp_getexpvar $dexp ) | sed 's#/#_#g' )
frun=$dexp/gcm_run.j
fcap=$dexp/CAP.rc
fdata=profile_categories
#dout=out_prfres
dtag=$( echo $dexp | rev | cut -d'/' -f2 | rev)
ddata=$GSCR/scripts/4.data/data_prfres

#todo: cp data file
if [[ ! -f $fdata ]];then cp -p $ddata/$fdata . ;fi

cd $dexp
arr_fstdout=($( grep -Ili --directories=skip -w "model throughput" *.out *.o[0-9]* 2>/dev/null ))
cd - >/dev/null

#todo:get job segment.
freq=$( grep -i -w JOB_SGMT $fcap | tr -s '[:space:]' | cut -d' ' -f2 ) 

#todo:calculate cpus, model name (i.e. sky_ele, bro)
str_mod=$( grep "#PBS -l select=" $frun | grep -i -v "##pbs"  )
n_node=$( echo $str_mod | sed 's/[^0-9,^=]*//g' | sed 's/=/ /g' | tr -s '[:space:]' | sed "s/^[ \t]*//" | cut -d' ' -f1)
n_cpus=$( echo $str_mod | sed 's/[^0-9,^=]*//g' | sed 's/=/ /g' | tr -s '[:space:]' | sed "s/^[ \t]*//" | cut -d' ' -f2)
cpus=$( echo "$n_node*$n_cpus" | bc -l )
mod=$( echo ${str_mod/*model/} | cut -d':' -f1 | sed 's/=//g')

echo "Experiment Dir: " $dexp

#================================================================================
#                                  Main Process
#================================================================================

if (( $runallprof == 1 ));then
    fout=${strscr}_${expu}.$mod.$cpus.$(date +"%Y%m%d")
    fdata_tmp1=fdata_ftemp1
    ftmp_allproforg=ftemp1_profilout
    ftmp_numonly=ftemp3_numonly_profileout
    ftmp_allnum=ftemp_allnums
    ftmp_allnum2=ftemp_allnums2
    ftmp_minavgmax=ftemp_minavgmax
    ftmp_min=ftemp_min
    ftmp_avg=ftemp_avg
    ftmp_max=ftemp_max
    ftmp_dochead=ftemp_dochead            #<== this is the beginning part of the final output file.
    ftmp_rowhead=ftemp_rowhead
    ftmp_colminavgmax=ftemp_colminavgmax
    ftmp_ttable=ftemp_ttable
    ftmp_empty=fempty
    sword_throughput="Model Throughput"
    sword_rep="x          x         x                 0.00      0.00      0.00"
  
    #todo: cleaning files
    [[ -f $fout              ]] && rm -f $fout
    [[ -f $ftmp_allnum       ]] && rm -f $ftmp_allnum
    [[ -f $ftmp_allnum2      ]] && rm -f $ftmp_allnum2
    [[ -f $ftmp_minavgmax    ]] && rm -f $ftmp_minavgmax
    [[ -f $ftmp_min          ]] && rm -f $ftmp_min
    [[ -f $ftmp_avg          ]] && rm -f $ftmp_avg
    [[ -f $ftmp_max          ]] && rm -f $ftmp_max
    [[ -f $ftmp_dochead      ]] && rm -f $ftmp_dochead
    [[ -f $ftmp_rowhead      ]] && rm -f $ftmp_rowhead
    [[ -f $ftmp_colminavgmax ]] && rm -f $ftmp_colminavgmax
    [[ -f $ftmp_ttable       ]] && rm -f $ftmp_ttable
    touch $ftmp_empty
    touch $ftmp_allnum
    touch $ftmp_allnum2

    #todo: create the beginning of itle header. 
    arr_title=("row_header" "--" "Min" "Avg" "Max" "ignore" )
    #arr_title=("row_header" "--" "Min" "Avg" "Max" )
    title=$(printf "%s," "${arr_title[@]}")

    ##todo: put together row header (ftmp_allnum and empyt file) and $ftmp_empty file.
    ##note: when the final output is formatted with 'showtable' cmd, the second column
    ##       from the left is aligned to left. The rest of the columns after that are 
    ##       aligned to right. Thus, this empty column is added and eventually removed.
    cp -p $fdata $ftmp_rowhead
    
    paste $ftmp_rowhead $ftmp_empty > $fout
    mv $fout $ftmp_rowhead
    [[ -f $ftmp_empty ]] && rm -f $ftmp_empty

    #todo: Add a line to document header
    echo "File IDs :" >> $ftmp_dochead


    #todo: get child names.
    arr_num_child=($( grep -n "Times for " $fdata | tr -s '[:space:]' | cut -d ' ' -f1,4 | tr -d ' ' ))
    cnt=0
    for fstdout in ${arr_fstdout[@]};do 
        [[ -f $ftmp_numonly    ]] && rm -f $ftmp_numonly && touch $ftmp_numonly
        [[ -f $ftmp_allproforg ]] && rm -f $ftmp_allproforg # touch $ftmp_allproforg; fi
        
        #todo: create header for each column with file id
        num_f=$(printf '%02g' $(( cnt + 1 )) )

        #todo: get number of days for the current month( or freq).
        [[ ! -f $dexp/$fstdout ]] && echo "file exists"
        
        numds_geosm=$( grep -w "Time: 18:00:00" $dexp/$fstdout | wc -l )

       # title="${title}f${num_f}_${numds_geosm}dd,"
        
        #todo: extract only profiling results from exp stdout.
        lbegin=$( grep -n "Times for " $dexp/$fstdout | head -1  | cut -d':' -f1 )
        lend=$( grep -n "$sword_throughput" $dexp/$fstdout | head -1  | cut -d':' -f1 )
        sed -n ${lbegin},${lend}p $dexp/$fstdout > $ftmp_allproforg
    
        arr_num_child_sout=($( grep -n "Times for " $ftmp_allproforg | tr -s '[:space:]' | cut -d ' ' -f1,4 | tr -d ' ' ))
        
        #todo: go through each element of each child and extract the line
        for i in ${!arr_num_child[@]};do
            ii=$(( i + 1 ))
            
            #note: use this number (lnum*) to extract words that should be used for grep.
            lnum1=$( echo ${arr_num_child[i]} | cut -d':' -f1 )
            child1=$( echo ${arr_num_child[i]} | cut -d':' -f2 )
            lnum2=$( echo ${arr_num_child[ii]} | cut -d':' -f1 )
            child2=$( echo ${arr_num_child[ii]} | cut -d':' -f2 )
    
            #note: use these numbers (lnum?_sout) to extract profiling results
            lnum1_sout=$( echo ${arr_num_child_sout[i]} | cut -d':' -f1 )
            lnum2_sout=$( echo ${arr_num_child_sout[ii]} | cut -d':' -f1 )
                    
            ftmp2=ftemp2_$child1

            #todo: create a file with data only for $child1
            if [[ "$child2" == "" ]];then
                #note: this 'if' is for the very last data listed.
                [[ -f $ftmp2 ]] && rm -f $ftmp2 && touch $ftmp2
                lnum2=$( grep -n "$sword_throughput" $fdata | head -1 | cut -d':' -f1 )
                lnum3=$(( lnum2 - 1 ))
                
                #todo: extract the profiling categories from fdata between two lines with
                #       has 'Times for', and place them in $fdata_tmp1.
                #       The results are transfered into an array 
                sed -ne ${lnum1},${lnum3}p $fdata > $fdata_tmp1
                readarray -t arr_ditem < $fdata_tmp1 

                #todo: get profiling results from the line number, lnum3_sout, 
                #       to the end of $ftmp_allproforg.
                lnum3_sout=$(( lnum2_sout - 1 ))
                sed -n ${lnum1_sout}',$p' $ftmp_allproforg > $ftmp2
            else
                [[ -f $ftmp2 ]] && rm -f $ftmp2    # touch $ftmp2; fi 
                lnum3=$(( lnum2 - 1 ))
                #todo: extract the profiling categories from fdata between two lines with
                #       has 'Times for', and place them in $fdata_tmp1.
                #       The results are transfered into an array
                [[ -f $fdata_tmp1 ]] && rm -f $fdata_tmp1
                sed -ne ${lnum1},${lnum3}p $fdata > $fdata_tmp1
                
                readarray -t arr_ditem < ${fdata_tmp1}

                #todo: get profiling results from the line number, lnum3_sout, 
                #       to the end of $ftmp_allproforg.
                lnum3_sout=$(( lnum2_sout - 1 ))
                sed -ne ${lnum1_sout},${lnum3_sout}p $ftmp_allproforg > $ftmp2
            fi
            
            #todo: extract only profiling output values from $ftmp2
            #      and write them in $ftmp_numonly. $ftmp2 has misc lines and  
            #      $ftmp_numonly has only profiling outputs. Those lines are cleand out in ftmp_numonly.
            for iii in ${!arr_ditem[@]};do
                ptn="${arr_ditem[iii]}"
                
                if [[ -z "${ptn}" ]];then
                    echo "${ptn}" >> $ftmp_numonly
                elif [[ ! -z "${ptn}" ]] || (( $iii != 0 ));then
                    ptn1=${ptn:0:1}
                    if (( $iii == 0 ));then
                        echo "">> $ftmp_numonly
                    elif [[ "$ptn1" == "-" ]];then
                        gout=$( grep  -w -- "${ptn}" ${ftmp2} | cut -d':' -f2 | tr -s '[:space:]')
                        echo $gout >> $ftmp_numonly
                    else
                        gout=$(grep -w "${ptn}" ${ftmp2} | cut -d':' -f2 | tr -s '[:space:]')
                        echo $gout >> $ftmp_numonly
                    fi
                fi
            done
               
            [[ -f $ftmp2      ]] && rm -f $ftmp2
            [[ -f $fdata_tmp1 ]] && rm -f $fdata_tmp1
        done

        #todo: append Model Throughput into a file ONLY when it is a positive value.
        #note: Need the average without negative values.   
        gout=$( grep "$sword_throughput" $ftmp_allproforg | tail -1 | cut -d':' -f2 | tr -s '[:space:]' | cut -d' ' -f2 )
            
        if (( $(bc -l <<< "scale=2; $gout > 0" ) )); then
    
            title="${title}f${num_f}_${numds_geosm}dd,"
            echo $gout >> $ftmp_numonly

#cp -p $ftmp_allnum allnum_$cnt
#cp -p $ftmp_numonly numonly_$cnt

            paste $ftmp_allnum $ftmp_numonly > $fout
            #paste $ftmp_allnum $ftmp_numonly > $ftmp_allnum2
            mv $fout $ftmp_allnum
            
            #todo: write file number id to $ftmp_dochead
            echo "f"$num_f"  : "$fstdout >> $ftmp_dochead
        else
            echo "f"$num_f"  : "$fstdout "excluded from table due to negative model throughput.">> $ftmp_dochead
        fi
        [[ -f $ftmp_allproforg ]] && rm -f $ftmp_allproforg
        [[ -f $ftmp_numonly    ]] && rm -f $ftmp_numonly
        #if [ -f $ftmp_numonly ]; then mv $ftmp_numonly "f${num_f}"; fi
        #if (( $cnt == 1 )); then break; fi
        #if (( $cnt == 2 )); then exit; fi
        let cnt++
    done

    #todo: calculate min, avg, & max. 
    touch $ftmp_min
    touch $ftmp_avg
    touch $ftmp_max
    touch $ftmp_minavgmax
                
    readarray -t arr_allnum < "$ftmp_allnum"

    cnt=0
    for allnum in "${arr_allnum[@]}";do
            
        tmp_allnum=$( echo $allnum | tr -s '[:space:]' )  
        tmp_test=$( echo $tmp_allnum | tr -d ':' | tr -d '.' )
        if [[ $tmp_test =~ ^[0]+$ ]];then
           # echo "$tmp_allnum" >> $ftmp_minavgmax
            echo "" >> $ftmp_min
            echo "" >> $ftmp_avg
            echo "" >> $ftmp_max
            
        elif [[ "$tmp_allnum" == "" ]]; then
            
            echo "x" >> $ftmp_min
            echo "x" >> $ftmp_avg
            echo "x" >> $ftmp_max
        else
                tmp_cat=''
                tmp_cat=$( echo $tmp_allnum | tr ' ' ':' )
                
                #minmaxavg=$( python -c "import pf0000_calcminmax; pf0000_calcminmax.calc_minmax( '""$tmp_cat""','""$tmp_cat""','""$tmp_cat""')")
                minmaxavg=$( python -c "import os; \
                            hmk=os.environ['HMK']; \
                            dpf=hmk + '/scripts/pf'; \
                            os.sys.path.append(dpf); \
                            import pf0000_calcminmax; pf0000_calcminmax.calc_minmax( '""$tmp_cat""','""$tmp_cat""','""$tmp_cat""')")

                min=$( echo $minmaxavg | cut -d':' -f1 )
                max=$( echo $minmaxavg | cut -d':' -f2 )
                avg=$( echo $minmaxavg | cut -d':' -f3 )
                
                echo $min >> $ftmp_min
                echo $avg >> $ftmp_avg
                echo $max >> $ftmp_max
        fi
        let cnt++
    done
    
    paste $ftmp_min $ftmp_avg $ftmp_max >> $ftmp_minavgmax

#    #todo: remove the first character, tab, from each line of $ftmp_allnum2 file
#    while IFS="" read -r p || [ -n "$p" ];do
#        line=$( echo $p | cut -c1- )
#        printf '%s\n' "$line" >> $fout
#    done < $ftmp_allnum2
#    cp -p $fout ftemp_tabremoved
#    mv $fout $ftmp_allnum2

    #todo: combine data into a table.
    #paste $ftmp_rowhead $ftmp_minavgmax $ftmp_allnum2 > $ftmp_ttable
    paste $ftmp_rowhead $ftmp_minavgmax $ftmp_allnum > $ftmp_ttable
    showtable -t -ti="$title" $ftmp_ttable > $fout
 
    [[ -f $ftmp_min          ]] && rm -f $ftmp_min
    [[ -f $ftmp_avg          ]] && rm -f $ftmp_avg
    [[ -f $ftmp_max          ]] && rm -f $ftmp_max
    [[ -f $ftmp_minavgmax    ]] && rm -f $ftmp_minavgmax
    [[ -f $ftmp_rowhead      ]] && rm -f $ftmp_rowhead
    [[ -f $ftmp_allnum       ]] && rm -f $ftmp_allnum
    [[ -f $ftmp_allnum2      ]] && rm -f $ftmp_allnum2 
    [[ -f $ftmp_ttable       ]] && rm -f $ftmp_ttable 
    [[ -f $ftmp_colminavgmax ]] && rm -f $ftmp_colminavgmax

    cnt=0
    #todo: remove lines with only zeros and no row header
    num_timesfor=$( grep "Times for" $fdata | wc -l )
    for (( i=1; i<=$num_timesfor; i++)) ; do
        timesfor=$( grep "Times for" $fdata | head -$i | tail -1 )
        lnum_timesfor=$( grep -n "Times for" $fout | head -$i | tail -1 | cut -d':' -f1)
        lnum1=$(( lnum_timesfor - 2 ))
        lnum2=$(( lnum_timesfor - 1 ))
        
        #note: the first 'Times for' has header right above it. Thus, ignore when lnum1=3
        if (( $lnum_timesfor != 3 ));then
            sed -i ${lnum2}'d' $fout
            sed -i ${lnum1}'d' $fout 
        fi
    done
    
    #todo: remove lines right above 'Modeling Throughput'.
    lnum_timesfor=$( grep -n "${sword_throughput}" $fout | cut -d':' -f1 )
    lnum2=$(( lnum_timesfor - 1 ))
    
    #note: the first 'Times for' has header right above it. Thus, ignore when lnum1=3
    if (( $lnum_timesfor != 3 ));then
        sed -i ${lnum2}'d' $fout
        #sed -i ${lnum1}'d' $fout 
    fi

    #todo: discard any characters that comes after"Times for"*
    #       and add blank line before each geos child section.
    for (( i=1; i<=$num_timesfor; i++)) ; do
        timesfor=$( grep "Times for" $fdata | head -$i | tail -1 )
        linetoreplace=$( grep "$timesfor" $fout | head -$i | tail -1 )
        sed -i 's/'"$linetoreplace"'/\n&/ ' $fout
        sed -i 's/'"$linetoreplace"'/'"$timesfor"'/g' $fout
    done

    sed -i 's/'"$sword_throughput"'/\n&/' $fout

    lnum1=$( grep -n "row_header" $fout | head -2 | tail -1 | cut -d':' -f1 )
    lnum=$(( lnum1 + 2 ))

    #todo: copy "Modeling Throughput" line and place right under the header.
    linetomove=$( grep "$sword_throughput" $fout )
    sed -i $lnum'i'"$linetomove" $fout
    sed -i 's/'"$sword_throughput"'/'" $sword_throughput"'/g' $fout
    sed -i 's/'"$sword_rep"'//g' $fout

    #todo: create document header    
    echo >> $ftmp_dochead
    echo >> $ftmp_dochead
    echo >> $ftmp_dochead
    echo "Total CPUs                :   "$cpus >> $ftmp_dochead
    echo "Job Segment (YYYYMMDD)    :   "$freq >> $ftmp_dochead
    echo >> $ftmp_dochead
    echo >> $ftmp_dochead
    echo "Unit - ${sword_throughput}   :      dd/d" >> $ftmp_dochead
    echo "Unit - the rest           :    sec/dd" >> $ftmp_dochead
    echo >> $ftmp_dochead
    cat $fout >> $ftmp_dochead
    mv $ftmp_dochead $fout
    
    if (( $? == 0 ));then
        mv $fout $dexp/
        echo "Output: $dexp/$fout"
    fi

fi

#todo: remove all function files and unnecessary ones. 
clean_dir

#if [[ $hst == 'pfe'* ]];then
#    source deactivate anapy27
#fi

module purge


