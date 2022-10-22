

```sh

cd $WORKDIR
unzip $GISDATA/topo/Quantarctica/Quantarctica3.zip

ogrinfo -al -geom=no Quantarctica3/Basemap/ADD_Coastline_low_res_polygon.shp
##grass --text -c  Quantarctica3/Basemap/ADD_Coastline_low_res_polygon.shp $GISDB/raw/Quantarctica3
grass --gui -c  Quantarctica3/Basemap/ETOPO1_IBCSO_RAMP2_500m_HS5.tif $GISDB/raw/Quantarctica3

v.in.ogr --overwrite input=Quantarctica3/Basemap/ADD_Coastline_low_res_polygon.shp output=ADD_Coastline snap=1e-09
v.to.rast input=ADD_Coastline output=ADD_Land use=val val=1 where="SURFACE='land'"
v.to.rast input=ADD_Coastline output=ADD_IceShelf use=val val=1 where="SURFACE='ice shelf'"
v.to.rast input=ADD_Coastline output=ADD_Rumple use=val val=1 where="SURFACE='rumple'"
v.to.rast input=ADD_Coastline output=ADD_Coastline use=val val=1
```
