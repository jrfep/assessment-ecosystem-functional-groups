## World database on Protected Areas

The World Database on Protected Areas (WDPA) is the most comprehensive global database of marine and terrestrial protected areas, updated on a monthly basis.
[Documentation](http://pp-import-production.s3.amazonaws.com/WDPA_Manual_1_5.pdf)

#### Citation

> UNEP-WCMC and IUCN (2019), Protected Planet: The World Database on Protected Areas (WDPA) [On-line], July 2019, Cambridge, UK: UNEP-WCMC and IUCN. Available at: www.protectedplanet.net.

#### Data access

Data downloaded  from https://www.protectedplanet.net

#### Data preparation

Data downloaded in GDB format

```sh
mkdir -p $GISDATA/areasprotegidas/WDPA/
cd $GISDATA/areasprotegidas/WDPA/
mv ~/Downloads/WDPA_Nov2019_Public.zip $GISDATA/areasprotegidas/WDPA/

```

Importing to postGIS database using using ogr:

```sql
cd $WORKDIR
unzip $GISDATA/areasprotegidas/WDPA/WDPA_Nov2019_Public.zip
psql gisdata  jferrer -c "CREATE SCHEMA wdpa"
ogr2ogr -overwrite -f "PostgreSQL" PG:"host=localhost user=jferrer dbname=gisdata" -lco SCHEMA=wdpa $WORKDIR/WDPA_Nov2019_Public/WDPA_Nov2019_Public.gdb

```

Now access the database using `psql gisdata` and create a subset of selected protected areas for the analysis (all IUCN categories up to VI, exclude: status "Proposed", "Not Reported" and status_yr  0).

First all polygons:

```sql
CREATE TABLE wdpa.wdpa_analysis_all AS
   SELECT wdpaid,name,desig,iucn_cat,status,status_yr,marine, wkb_geometry
   FROM wdpa.wdpa_poly_nov2019
   WHERE iucn_cat IN ('Ia', 'Ib', 'II', 'III', 'IV','V','VI') AND marine IN ('0','1','2') AND desig_type IN ('National','Regional','International') AND status IN ('Adopted', 'Designated', 'Established', 'Inscribed') AND status_yr > 0;
-- 133904 rows
```

And now add point localities (area reported in square kilometers assuming a circular buffer in meters around points)
```sql

INSERT INTO wdpa.wdpa_analysis_all
   SELECT wdpaid,name,desig,iucn_cat,status,status_yr,marine, ST_Multi(ST_BUFFER(wkb_geometry::geography,SQRT((rep_area*1000000)/3.14))::geometry)
   FROM wdpa.wdpa_point_nov2019
   WHERE iucn_cat IN ('Ia', 'Ib', 'II', 'III', 'IV','V','VI') AND marine IN ('0','1','2') AND desig_type IN ('National','Regional','International') AND status IN ('Adopted', 'Designated', 'Established', 'Inscribed') AND status_yr > 0;
-- 12425

```
Include areas up to 2000 and 2013 to match time frames of impact variables

```sql

CREATE TABLE wdpa.wdpa_analysis_2000 AS
   SELECT wdpaid,name,desig,iucn_cat,status,status_yr,marine, wkb_geometry
   FROM wdpa.wdpa_analysis_all
   WHERE status_yr <2001 ;
-- 86479 rows

CREATE TABLE wdpa.wdpa_analysis_2013 AS
   SELECT wdpaid,name,desig,iucn_cat,status,status_yr,marine, wkb_geometry
   FROM wdpa.wdpa_analysis_all
   WHERE status_yr <2014 ;
-- 137253 rows


```

Create a location in GRASS GIS for this dataset:

```sh
grass --text -c EPSG:4326  $GISDB/raw/WDPA
v.in.ogr input="PG:host=localhost dbname=gisdata user=jferrer" layer=wdpa.wdpa_analysis_all output=WDPA_all # 142412 features
##v.in.ogr input="PG:host=localhost dbname=gisdata user=jferrer"  layer=wdpa_analysis_2000 output=WDPA_2000
v.in.ogr input="PG:host=localhost dbname=gisdata user=jferrer"  layer=wdpa_analysis_2013 output=WDPA_2013

g.region n=90 s=-90 w=-180 e=180  res=00:00:30
for k in WDPA_all WDPA_2013
do
   v.to.rast input=${k} output=${k} use=val val=1
done

v.to.rast input=WDPA_all output=WDPA_year use=attr attribute_column=status_yr memory=3000
```
