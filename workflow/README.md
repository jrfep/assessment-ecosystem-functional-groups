# *workflow* folder

This file describes the steps for reproducing the analysis in *Appendix S5. Preliminary global assessment of pressures and protection of Ecosystem Functional Groups, from data import to the production of outputs and products* from Keith *et al.* (submitted)

## Environment and requirements
This workflow has been creating using the following operating system and software:

* Xubuntu 18.04
* R 3.6.1
* Python 3.7.3
* GRASS GIS 7.4.0 (64 bit)
* GDAL 2.2.3
* PostgreSQL 10.12 with postGIS extension

To set-up programming environmental variables edit the files in folder `env/` and source them in a bash shell:
`source env/terra.sh` and `source env/project-env.sh`. We use anaconda for our python configuration, so we need to use `conda deactivate` before starting grass, and `conda activate` before running python scripts.


## Data import

Start from the working directory defined in the programming environment above: `cd $WORKDIR`.

Download and import external data by following the instructions in the *[data](../data)* directory for the *[Anthromes](../data/Anthromes.md)*, *[Human Footprint](../data/HumanFootPrint.md)*, *[Marine Cummulative Human Impact](../data/MarineCumulativeHumanImpact.md)* and *[World Database of Protected Areas](../data/WDPA.md)*.

Import indicative maps from the Zenodo repository for all ecosystems by following  *[these instructions](../data/Ecosystems-indicative-distribution.md)*.

## Analysis

### Set up data

```sh
cd $WORKDIR
conda deactivate
grass --text -c $GISOUT/F1_1.IM.orig.tif $GISDB/ecosystem_analysis
source $SCRIPTDIR/inc/grass/import-indicators-for-analysis.sh
source $SCRIPTDIR/inc/grass/import-indicative-maps-for-analysis.sh

```

 * Import or download spatial data from several sources
 * Create a series of spatial databases for use with Grass GIS software for importing projected data sources.
 * Creates and organizes the main spatial database for the analysis using Grass GIS.

## Create Sankey plots

```sh
cd $WORKDIR
mkdir -p $FIGDIR/sankeyplots/Terrestrial
mkdir -p $FIGDIR/sankeyplots/Marine

```


##mkdir -p $FIGDIR/sankeyplots-simple/Terrestrial
rm DegradedProtectedSummary*

## python script for generating sankey plots:
python3 $SCRIPTDIR/inc/python/EFG-sankeyplots.py ## not working!

## R script for summaries
R --vanilla CMD BATCH $SCRIPTDIR/inc/R/read-degraded-protected-summaries.R

mkdir -p $FIGDIR/manuscript
mkdir -p $FIGDIR/supplement

## R script for figures with ggplot.
R --vanilla CMD BATCH $SCRIPTDIR/inc/R/figure-degraded-protected.R ## not working!
