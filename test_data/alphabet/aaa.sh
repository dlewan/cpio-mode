#!/bin/bash
declare -i ct=0
for l in ${letters} ; do 
    for outFile in ${l} ${l}${l} ${l}${l}${l} ${l}${l}${l}${l} ${l}${l}${l}${l}${l} ; do
	((ct++))
	echo > ${outFile}
	for ((i=0;i<ct;i++)) ; do 
	    printf ${l} >> ${outFile} ; 
	done ; echo >> ${outFile} ; 
    done ; 
done
