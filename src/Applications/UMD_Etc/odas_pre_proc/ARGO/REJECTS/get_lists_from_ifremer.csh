#! /bin/csh -f

#==============================================================================
# SCRIPT
#   get_greylist_from_ifremer
#
# LANGUAGE
#   C-Shell Script
#
#------------------------------------------------------------------------------
# PURPOSE
#   fetch the grey list from ftp.ifremer.fr
#
# USAGE
#    get_greylist_from_ifremer <yyyymmdd>
#
#==============================================================================

#======================================
# $SSS_base is defined in '.cshrc_user'
#======================================
source ~/.cshrc

set host=ftp.ifremer.fr

#-------------------------
# <yyyymmdd> : current date
#-------------------------
set yyyymmdd = `date "+%Y%m%d"`
echo $yyyymmdd

set yyyy=`echo ${yyyymmdd} | cut -c1-4`
set pyyy=`expr $yyyy - 1`


#---------------------------------------------
# <base> : base directory of SMAP Level-2 data
#---------------------------------------------
set base=/discover/nobackup/lren1/pre_proc/NRT/ARGO

#-------------------------------
# <work> : working sub-directory
# <arch> : archive sub-directory
#-------------------------------
set work=${base}/scripts/REJECTS
#-------------------------------------
# download grey list into <work>
#-------------------------------------
cd ${work}
rm ListeOfFloatsToBeChecked*
rm ar_greylist.txt
wget --user='anonymous' --password='li.ren@nasa.gov' ftp://ftp.ifremer.fr/ifremer/argo/ar_greylist.txt
wget --user='anonymous' --password='li.ren@nasa.gov' ftp://ftp.ifremer.fr/ifremer/argo/etc/argo-ast9-item13-AltimeterComparison/ListeOfFloatsToBeChecked-\*\\$pyyy\*
wget --user='anonymous' --password='li.ren@nasa.gov' ftp://ftp.ifremer.fr/ifremer/argo/etc/argo-ast9-item13-AltimeterComparison/ListeOfFloatsToBeChecked-\*\\$yyyy\*
#----------------------------------------------
# remove the first line of ar_greylist.txt
#----------------------------------------------
sed -i 1d ar_greylist.txt
mv ar_greylist.txt ar_greylist_$yyyymmdd.txt

#----------------------------------------------
# remove the first 4 lines of check lists
#----------------------------------------------

sed -i 1,4d ListeOfFloatsToBeChecked*
foreach file (ListeOfFloatsToBeChecked*)
     set fname=`echo $file | cut -c26-31`
     mv $file Floats_$fname.txt
end
exit




