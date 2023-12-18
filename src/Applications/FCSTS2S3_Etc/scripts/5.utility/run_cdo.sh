#!/usr/bin/env bash

#description:   extracting selected vars from collections for eric.
. /usr/share/modules/init/bash
module load cdo/1.9.0


arrcoll=( geosgcm_ocn2dT geosgcm_ocn3dT geosgcm_seaice )
arrocn2dT=( SSH )
arrocn3dT=( T S )
arrseaice=( AICE HICE HSNO DRAFT )

strocn2dT=SSH 
strocn3dT="T,S"
strseaice="AICE,HICE,HSNO,DRAFT"

arrens=($( seq 301 310 ))
#arrens=( 307 )
arrrdate=( 19820326 19820425 )

for rdate in ${arrrdate[@]};do
    fexp=$CFEXP/$rdate

    for ens in ${arrens[@]};do
        dens=$fexp/ens$ens
        dout=$CFEXP/oletkf/$rdate/ens$ens/geosgcm_extracted
        
        #if [[ -d $dout ]];then rm -rf $dout/* ;fi

        for coll in ${arrcoll[@]};do
            if [[ "$coll" == "geosgcm_ocn2dT" ]];then
                var=$strocn2dT
            elif [[ "$coll" == "geosgcm_ocn3dT" ]];then
                var=$strocn3dT
            elif [[ "$coll" == "geosgcm_seaice" ]];then
                var=$strseaice 
            fi
            
            if [[ -d $dens/$coll ]];then
                #+++++ cd to $fexp/ens$ens (start) +++++ 
                cd $dens
                if [[ ! -d $dout ]];then mkdir -p $dout;fi
    
                arr=($( find * -name '*'$coll'*monthly*.nc4' ))
                for x in ${arr[@]};do
                    fnamein=$( basename $x )
                    fnameout=$( echo $fnamein | rev | cut -d'.' -f2- |rev ).extract.nc4
    
                    echo $x
                    cdo -s -O --no_warnings selname,$var $x $dout/$fnameout
                done
    
                cd - >/dev/null
                #+++++ cd to $fexp/ens$ens ( end ) +++++
            else
               echo "...  $dens/$coll do not exist ..." 
            fi
        done
    done
done
module purge

