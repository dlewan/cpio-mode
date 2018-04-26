#!/bin/bash
for l in ${letters} ; do
    for ((i=1;i<=5;i++)) ; do
	for ((j=1;j<=i;j++)) ; do
	    printf ${l} ;
	done ;
	printf "\t\n" ;
    done ;
done ;

for l in ${letters} ; do
    mkdir ${l}${l}${l}${l}${l}.d
done
