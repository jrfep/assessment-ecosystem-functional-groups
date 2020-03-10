#!/bin/bash
if [[ $(g.gisenv get=LOCATION_NAME) != "ecosystem_analysis" ]]; then
    echo "Need to be in GRASS Location 'ecosystem_analysis'" 1>&2
    exit 1
fi

g.mapset -c indicativeMaps

for k in $(ls $GISOUT/ | grep tif$)
do
   export MAPNAME=$(echo $k | sed s/.tif$//g)
   r.in.gdal input=${GISOUT}/${k} output=${MAPNAME} 
done

g.mapset PERMANENT
