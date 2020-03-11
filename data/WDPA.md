## World database on Protected Areas

The World Database on Protected Areas (WDPA) is the most comprehensive global database of marine and terrestrial protected areas, updated on a monthly basis.
[Documentation](http://pp-import-production.s3.amazonaws.com/WDPA_Manual_1_5.pdf)

#### Citation

> UNEP-WCMC and IUCN (2019), Protected Planet: The World Database on Protected Areas (WDPA) [On-line], July 2019, Cambridge, UK: UNEP-WCMC and IUCN. Available at: www.protectedplanet.net.

#### Data access

Data downloaded in gdb format from https://www.protectedplanet.net

#### Data preparation

Importing to postGIS database using using ogr:

```sh
ogr2ogr -overwrite -f "PostgreSQL" PG:"host=localhost user=jferrer dbname=IUCN" $GISDATA/areasprotegidas/WDPA/WDPA_Aug2019_Public/WDPA_Aug2019_Public.gdb
```

Now access the database using `psql IUCN` and run this to create a subset of selected protected areas for the analysis:

```sql
-- use this for calculations  -- all IUCN categories up to VI, exclude: status "Proposed", "Not Reported" and status_yr  0
CREATE TABLE WDPA_analysis_all AS
   SELECT wdpaid,name,status_yr, wkb_geometry
   FROM wdpa_poly_aug2019
   WHERE iucn_cat IN ('Ia', 'Ib', 'II', 'III', 'IV','V','VI') AND marine IN ('0','1','2') AND desig_type IN ('National','Regional','International') AND status IN ('Adopted', 'Designated', 'Established', 'Inscribed') AND status_yr > 0;
-- 124698 rows

-- Include areas up to 2000 and 2013 to match time frames of impact variables

CREATE TABLE WDPA_analysis_2000 AS
   SELECT wdpaid,name,status_yr, wkb_geometry
   FROM wdpa_analysis_all
   WHERE status_yr <2001 ;
-- 76057 rows

CREATE TABLE WDPA_analysis_2013 AS
   SELECT wdpaid,name,status_yr, wkb_geometry
   FROM wdpa_analysis_all
   WHERE status_yr <2014 ;
-- 117366 rows


```


```sh
grass --text -c EPSG:4326  $GISDB/raw/WDPA
v.in.ogr input="PG:host=localhost dbname=IUCN user=jferrer"  layer=wdpa_analysis_all output=WDPA_all
v.in.ogr input="PG:host=localhost dbname=IUCN user=jferrer"  layer=wdpa_analysis_2000 output=WDPA_2000
v.in.ogr input="PG:host=localhost dbname=IUCN user=jferrer"  layer=wdpa_analysis_2013 output=WDPA_2013

g.region n=90 s=-90 w=-180 e=180  res=00:01:00
for k in WDPA_all WDPA_2000 WDPA_2013
do
   v.to.rast input=${k} output=${k} use=val val=1
done
```
