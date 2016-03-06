#!/bin/bash
s=1
for i in ls *.png; do
	mv "${i}" "${s}.png"
	echo "${i} renamed to ${s}"
	let s=s+1
done
