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
done

## only for 2013
r.proj input=WDPA_${k} output=WDPA_${k} location=WDPA mapset=PERMANENT dbase=$GISDB/raw

for k in 2008 2013
do
   r.proj input=MCHI${k} output=MCHI${k} location=MCHI mapset=PERMANENT dbase=$GISDB/raw
   r.null MCHI${k} setnull=0
done

for target in p_cult p_past p_urban p_crop p_rice p_irrig
do
   r.proj input=${target} output=${target} location=Anthromes mapset=PERMANENT dbase=$GISDB/raw
done

## modified habitat class (MHC)
#r.proj input=gHM2019 output=gHM2019 location=gHM mapset=PERMANENT dbase=$GISDB/raw
## modified habitat class (MHC)
#r.proj input=modified_habitat output=MHC location=Modified-Habitat-2020 mapset=PERMANENT dbase=$GISDB/raw


##We need to transform the indicators of impact into binary variables (*degraded/non-degraded*).
##r.univar map=HFP2000i@indicators
##r.univar map=MCHI2008@indicators

# r.quantile HFP2000i@indicators quantiles=6 ## median is 4
# r.quantile MCHI2008@indicators quantiles=6 ## median is 2.827246
#r.quantile gHM2019@indicators quantiles=6 ## median is 2.827246

export k=2013
export MT=4
r.mapcalc --overwrite expression="HFP${k}x=if(HFP${k}i@indicators>${MT},1,0)"
export MT=2.827246
r.mapcalc --overwrite  expression="MCHI${k}x=if(MCHI${k}@indicators>${MT},1,0)"

export k=2019
export MT=0.066571
r.mapcalc --overwrite expression="gHM${k}x=if(gHM${k}@indicators>${MT},1,0)"

r.mapcalc expression="HFPdiff=HFP2013i-HFP2000i"
r.stats -an HFPdiff

r.mapcalc expression="MCHIdiff=MCHI2013-MCHI2008"
r.stats -an MCHIdiff

g.mapset PERMANENT
