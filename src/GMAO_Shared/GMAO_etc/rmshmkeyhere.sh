#!/bin/bash

segments=$( ipcs -m | grep '^0x' | awk '{print $1}' )
owners=$( ipcs -m | grep '^0x' | awk '{print $3}' | uniq )

if [[ -z $segments ]]
then
   echo "No shared memory segments to remove."
else
   echo "Removing segments..."
   for seg in $segments
   do
      ipcrm -M $seg
      if [[ ! $? ]]
      then
         echo "Unable to remove segments owned by $owners"
      fi
   done
fi
