#!/bin/bash
if [[ $(g.gisenv get=LOCATION_NAME) != "ecosystem_analysis" ]]; then
    echo "Need to be in GRASS Location 'ecosystem_analysis'" 1>&2
    exit 1
fi

g.mapset -c indicators

## using -180 and 180 for longitude produces an output of one single column for HFP files
g.region n=90 s=-90 w=-179.975 e=179.975 res=00:01:00

for k in 2000 2013
do
   r.proj input=HFP${k}i output=HFP${k}i location=HFP mapset=PERMANENT dbase=$GISDB/raw
   r.proj input=WDPA_${k} output=WDPA_${k} location=WDPA mapset=PERMANENT dbase=$GISDB/raw
done

for k in 2008 2013
do
   r.proj input=MCHI${k} output=MCHI${k} location=MCHI mapset=PERMANENT dbase=$GISDB/raw
   r.null MCHI${k} setnull=0
done

for target in p_cult p_past p_urban p_crop p_rice p_irrig
do
   r.proj input=${target} output=${target} location=Anthromes mapset=PERMANENT dbase=$GISDB/raw
done

g.mapset PERMANENT
