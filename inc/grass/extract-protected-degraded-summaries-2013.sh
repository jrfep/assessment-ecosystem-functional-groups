#!/bin/bash

g.mapset PERMANENT
g.mapsets PERMANENT,indicators,indicativeMaps


for k in $(g.list type=rast mapset=indicativeMaps)
do
	r.stats -aA input=WDPA_2013@indicators,HFP2013x@indicators,MCHI2013x@indicators,${k}@indicativeMaps output=Protected_Degraded_2013_${k}.txt
done
