source $HOME/proyectos/IUCN-GET/assessment-ecosystem-functional-groups/env/project-env.sh
mkdir -p $WORKDIR/INPUT
mkdir -p $WORKDIR/OUTPUT

# Time series from CRU climate variables 
cp $GISDATA/climate/global/CRU_TS/pre/cru_ts4.03.1901.2018.pre.dat.nc.gz $WORKDIR/INPUT
cp $GISDATA/climate/global/CRU_TS/pet/cru_ts4.03.1901.2018.pet.dat.nc.gz $WORKDIR/INPUT

cd $WORKDIR/INPUT
gunzip cru_ts4.03.1901.2018.pet.dat.nc.gz
gunzip cru_ts4.03.1901.2018.pre.dat.nc.gz

# gdalinfo NETCDF:"cru_ts4.03.1901.2018.pre.dat.nc" -sd 1 | less

# Terrestrial Ecoregions
cp $GISDATA/ecoregions/global/TEOW/teow_2017_valid.gpkg $WORKDIR/INPUT

# for T5.5 use this file
#ls $REFDIR/*/*T5-5*

# Sea ice time series
find $GISDATA/cryosphere/global/SeaIceIndex/north/monthly/geotiff/ -name "*extent*"

