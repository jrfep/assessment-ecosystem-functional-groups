source $HOME/proyectos/IUCN-GET/assessment-ecosystem-functional-groups/env/project-env.sh
mkdir -p $WORKDIR/INPUT


# Time series from CRU climate variables 
cp $GISDATA/climate/global/CRU_TS/pre/cru_ts4.03.1901.2018.pre.dat.nc.gz $WORKDIR/INPUT
cp $GISDATA/climate/global/CRU_TS/pet/cru_ts4.03.1901.2018.pet.dat.nc.gz $WORKDIR/INPUT

cd $WORKDIR/INPUT
gunzip cru_ts4.03.1901.2018.pet.dat.nc.gz
gunzip cru_ts4.03.1901.2018.pre.dat.nc.gz

# gdalinfo NETCDF:"cru_ts4.03.1901.2018.pre.dat.nc" -sd 1 | less
