#!/usr/bin/env bash


cfexp=/nobackupp11/knakada/t2ssp/geosks2s/GEOS_exp/knakada_s2s3_unstable_11252019
dfcstrmltr=$modelexper/$expertag/rmltr/$rstdate/$ensm

#+++++ cd to cfexp (start) +++++
cd $cfexp
arrdexp=($( find * -maxdepth 2 -mindepth 2 -type f -name 'R1982*.out' -o -name 'gcm_run*.o*' | cut -d'/' -f1-2 | sort | uniq )) 
cd - >/dev/null
#+++++ cd to cfexp ( end ) +++++

for dexp in ${arrdexp[@]};do 

    #+++++ cd to cfexp (start) +++++
    cd $cfexp/$dexp
    drmltr=$cfexp/rmltr/$dexp
    if [[ ! -d $drmltr ]];then 
        echo "... not exist: $drmltr"
        find * -maxdepth 0 -name '*.out' -o -name 'gcm_run.o*' -o -name 'R$rstdate$ensm*.out' -o -name 'R$rstdate$ensm.o*' -delete
    fi

    #find * -maxdepth 0 -name '*.out' -o -name 'gcm_run.o*' -o -name 'R$rstdate$ensm*.out' -o -name 'R$rstdate$ensm.o*' | tar -cf stdout.tar -T - --remove-file
    ##tar -cf stdout.tar *.out gcm_run.o* R$rstdate$ensm*.out #R$rstdate$ensm.o*
    #if (( $? == 0 )) && [[ -f stdout.tar ]];then 
    #    if [[ ! -d $drmltr ]];then mkdir -p $drmltr ;fi
    #    mv stdout.tar $dfcstrmltr/

    cd - >/dev/null
    #+++++ cd to cfexp ( end ) +++++

done
