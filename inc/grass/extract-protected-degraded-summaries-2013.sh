#!/bin/bash

g.mapset PERMANENT
g.mapsets PERMANENT,indicators,$VERSION

mkdir -p $WORKDIR/output/$VERSION
for k in $(g.list type=rast mapset=$VERSION)
do
	if [ ! -f ${WORKDIR}/output/${VERSION}/Protected_Degraded_2013_${k}.txt ]
	then
		r.stats -aA input=WDPA_2013@indicators,HFP2013x@indicators,MCHI2013x@indicators,${k}@${VERSION} output=${WORKDIR}/output/${VERSION}/Protected_Degraded_2013_${k}.txt
	fi
	if [ ! -f ${WORKDIR}/output/${VERSION}/Protected_Degraded_all_${k}.txt ]
	then
		r.stats -aA input=WDPA_all@indicators,HFP2013x@indicators,MCHI2013x@indicators,${k}@${VERSION} output=${WORKDIR}/output/${VERSION}/Protected_Degraded_all_${k}.txt
	fi
	if [ ! -f ${WORKDIR}/output/${VERSION}/Protected_year_${k}.txt ]
	then
		r.stats -aA input=WDPA_yr@indicators,${k}@${VERSION} output=${WORKDIR}/output/${VERSION}/Protected_year_${k}.txt
	fi
done

for k in $(g.list type=rast mapset=$VERSION pattern=*[TF]*IM*)
do
	if [ ! -f ${WORKDIR}/output/${VERSION}/Degraded_Change_Terrestrial_${k}.txt ]
	then
		r.stats -aA input=HFPdiff@indicators,${k}@${VERSION} output=${WORKDIR}/output/${VERSION}/Degraded_Change_Terrestrial_${k}.txt
	fi
	if [ ! -f ${WORKDIR}/output/${VERSION}/HFP_Terrestrial_${k}.txt ]
	then
		r.stats -aA input=HFP2013i@indicators,${k}@${VERSION} output=${WORKDIR}/output/${VERSION}/HFP_Terrestrial_${k}.txt
	fi

	if [ ! -f ${WORKDIR}/output/${VERSION}/Transform_Terrestrial_${k}.txt ]
	then
			r.stats -aA input=p_cult@indicators,p_irrig@indicators,p_past@indicators,p_rice@indicators,p_urban@indicators,WDPA_2013@indicators,HFP2013x@indicators,${k}@${VERSION} output=${WORKDIR}/output/${VERSION}/Transform_Terrestrial_${k}.txt
		fi
done

for k in $(g.list type=rast mapset=$VERSION pattern=*M*IM*)
do
	if [ ! -f ${WORKDIR}/output/${VERSION}/Degraded_Change_Marine_${k}.txt ]
	then
		r.stats -aA input=MCHIdiff@indicators,${k}@${VERSION} output=${WORKDIR}/output/${VERSION}/Degraded_Change_Marine_${k}.txt
	fi
	if [ ! -f ${WORKDIR}/output/${VERSION}/MCHI_Marine_${k}.txt ]
	then
		r.stats -aA input=MCHI2013@indicators,${k}@${VERSION} output=${WORKDIR}/output/${VERSION}/MCHI_Marine_${k}.txt
	fi
done
