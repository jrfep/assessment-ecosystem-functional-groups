#!/bin/bash
if [[ $(g.gisenv get=LOCATION_NAME) != "ecosystem_analysis" ]]; then
    echo "Need to be in GRASS Location 'ecosystem_analysis'" 1>&2
    exit 1
fi

g.mapset -c $VERSION

for k in $(ls $GISOUT/$VERSION | grep tif$)
do
   export MAPNAME=$(echo $k | sed s/.tif$//g)
   r.in.gdal input=${GISOUT}/${VERSION}/${k} output=${MAPNAME}
done

g.mapset PERMANENT
