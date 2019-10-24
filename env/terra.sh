export WORKDIR=$HOME/tmp/EFGassessment
export GISDB=/opt/gisdb
export LOCATION=earth
export MAPSET=IndicativeMaps
export SCRIPTDIR=/home/jferrer/proyectos/UNSW/gaia-gis/
export REFDIR=$HOME/Cloudstor/Shared/EFTglobalmaps
export GISDATA=/opt/gisdata
export WEBMAPDIR=$WORKDIR/output/webmaps
export WEBINFODIR=$WORKDIR/output/mapinfo
export OUTDIR=$WORKDIR/output/tables/ImpactIndicators
export ZIPDIR=$WORKDIR/output/zip
export FIGDIR=$WORKDIR/output/figures

export RDATADIR=$WORKDIR/output/Rdata

export GTIFFPATH=$WORKDIR/output/maps
export GTIFFDIR=unprojected-geotiff
export GJSONDIR=unprojected-geojson
mkdir -p $WORKDIR
mkdir -p $GTIFFPATH/$GTIFFDIR
mkdir -p $GTIFFPATH/$GJSONDIR
mkdir -p $WEBMAPDIR
mkdir -p $WEBINFODIR
mkdir -p $OUTDIR
mkdir -p $FIGDIR
mkdir -p $ZIPDIR
mkdir -p $RDATADIR
