#!/bin/bash
####################################
#This Script will set your background
#depending on the current time!
###################################
HOUR=-1
while true; do

	HOUR="$(date +%H)"
	quot=1
	num=0
	let quot=$HOUR/3
#	echo "The hour is ${HOUR}"
#	echo "The 8th is: ${quot}"
	case $quot  in
		    0)
			let num=8
			;;
		    1)
			let num=9
			;;
		    2)
			let num=2
			;;
		    3)
		    let num=3
			;;
		    4)
			let num=4
			;;
		    5)
			let num=5
			;;
			6)
			let num=6
			;;
			7)
			let num=7
			;;
			
	esac
#	echo "The new wallpaper will be ${num}.png"
	feh --bg-center ~/Pictures/wallpaper/"${num}".png
	sleep 30m
done
