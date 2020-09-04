#!/bin/bash

g.mapset PERMANENT
g.mapsets PERMANENT,indicators,$VERSION

mkdir -p $WORKDIR/output/$VERSION
for k in $(g.list type=rast mapset=$VERSION)
do
	r.stats -aA input=WDPA_2013@indicators,HFP2013x@indicators,MCHI2013x@indicators,${k}@${VERSION} output=${WORKDIR}/output/${VERSION}/Protected_Degraded_2013_${k}.txt

done

for k in $(g.list type=rast mapset=$VERSION pattern=*[TF]*IM*)
do
	r.stats -aA input=HFPdiff@indicators,${k}@${VERSION} output=${WORKDIR}/output/${VERSION}/Degraded_Change_Terrestrial_${k}.txt
done



for k in $(g.list type=rast mapset=$VERSION pattern=*M*IM*)
do
	r.stats -aA input=MCHIdiff@indicators,${k}@${VERSION} output=${WORKDIR}/output/${VERSION}/Degraded_Change_Marine_${k}.txt
done
