#!/usr/bin/env bash
source $SCR/bf/bf0011_msg.sh
arr=($( find * -maxdepth 0 -type d ))
arryyyymm=($( find * -type f -name '*.nc4' | xargs -i basename {} | cut -d'.' -f3 | cut -d'_' -f1 |cut -c1-6| sort | uniq ))

#todo:  make dir
for coll in ${arr[@]};do 
    for yyyymm in ${arryyyymm[@]};do 
        if [[ ! -d $coll/$yyyymm ]];then
            mkdir -p $coll/$yyyymm
        fi
    done
done

#todo:  move files into yyyymm dir.
for coll in ${arr[@]};do
    if [[ -d $coll ]];then 
        cd $coll
        for yyyymm in ${arryyyymm[@]};do
            numf=$( find * -type f -name '*'$yyyymm'*' | wc -l )
            if (( $numf > 0 ));then
                wmessage "... $coll $yyyymm moving files ..." 
                find * -type f -name '*'$yyyymm'*' | xargs -i mv {} $yyyymm/
                if (( $? > 0 ));then 
                    wmessage "    ... moving files failed ..."
                fi
            fi
        done
        cd - >/dev/null 
    fi
done
