# *workflow* folder

This file describes the steps for reproducing the analysis in *Appendix S5. Preliminary global assessment of pressures and protection of Ecosystem Functional Groups, from data import to the production of outputs and products* from Keith *et al.* (submitted)

## Environment and requirements
This workflow has been created using the following operating system and software:

* Xubuntu 18.04
* R 3.6.1
* Python 3.7.3
* GRASS GIS 7.4.0 (64 bit)
* GDAL 2.2.3
* PostgreSQL 10.12 with postGIS extension

To set-up programming environmental variables edit the files in folder `env/` and source them in a bash shell: `source env/project-env.sh`. We use anaconda for our python configuration, so we need to use `conda deactivate` before starting grass, and `conda activate` before running python scripts.

## Workflow

To reproduce the entire workflow, you need to follow all these steps:

1. Import or download spatial data from several sources and create a series of spatial databases for use with Grass GIS software for importing projected data sources.
2. Creates and organizes the main spatial database for the analysis using Grass GIS.
3. Cross tabulates map data with indicators of protection, degradation and transformation
4. Perform analysis in R and output figures
5. Run shinyApp

If you only want to reproduce the figures you can just run the *R* code in the *[apps](../apps)* folder (jump to step 5).

### Step 1. Data import

Start from the working directory defined in the programming environment above: `cd $WORKDIR`.

Download and import external data by following the instructions in the *[data](../data)* directory for the *[Anthromes](../data/Anthromes.md)*, *[Human Footprint](../data/HumanFootPrint.md)*, *[Marine Cummulative Human Impact](../data/MarineCummulativeHumanImpact.md)* and *[World Database of Protected Areas](../data/WDPA.md)*.

Import indicative maps from the Zenodo repository for all ecosystems by following  *[these instructions](../data/Ecosystems-indicative-distribution.md)*.

### Step 2. Data set up for analysis

Run this short script to create a new GRASS GIS location and set up the data for analysis

```sh
cd $WORKDIR
conda deactivate
grass --text -c $GISOUT/version-1.1.0/F1.1.IM.orig_v1.0.tif $WORKDIR/ecosystem_analysis
#grass --text $WORKDIR/ecosystem_analysis/PERMANENT
source $SCRIPTDIR/inc/grass/import-indicators-for-analysis.sh
for VERSION in version-1.1.0 version-2.0.1b
do
  source $SCRIPTDIR/inc/grass/import-indicative-maps-for-analysis.sh
done
```


## Step 3. Cross tabulation of map data in GRASS GIS

Now we can source these scripts to calculate the cross-tabulation

```sh
cd $WORKDIR
conda deactivate

grass --text -c $WORKDIR/ecosystem_analysis/PERMANENT
for VERSION in version-1.1.0 version-2.0.1b
do
  source $SCRIPTDIR/inc/grass/extract-protected-degraded-summaries-2013.sh
done
```

### Read tables and summarize data

Now we can read this table into an R-data file:

```sh
cd $WORKDIR
Rscript --vanilla $SCRIPTDIR/inc/R/read-protected-degraded-table.R
Rscript --vanilla $SCRIPTDIR/inc/R/read-change-degraded-table.R
Rscript --vanilla $SCRIPTDIR/inc/R/read-impact-index-table.R
```

## Step 4. Summary and analysis

R script for figures with ggplot.

```sh
cd $WORKDIR

mkdir -p $WORKDIR/output/figures/sankeyplots
R --vanilla CMD BATCH $SCRIPTDIR/inc/R/figure-degraded-protected.R
R --vanilla CMD BATCH $SCRIPTDIR/inc/R/figure-change-degraded.R
R --vanilla CMD BATCH $SCRIPTDIR/inc/R/figure-barplot-all.R
R --vanilla CMD BATCH $SCRIPTDIR/inc/R/figure-boxplots.R
```

Python script for sankey plots.

```sh
cd $WORKDIR
conda activate
python3 $SCRIPTDIR/inc/python/figure-sankey-plots.py
```

```sh
cd $WORKDIR
tar -cjvf Transform-Terrestrial.tar.bz2 output/version-2.0.1b/Transform_Terrestrial_*

```
## Step 5. Shiny App

### Interactive figures

TO DO: Shiny app to show relationships between protected and degraded, with option to select original maps and newer version of maps.

```sh
cd $WORKDIR

##R --vanilla -e "shiny::runApp('${SCRIPTDIR}/apps/shiny/app.R',host='149.171.173.203',port='4826')"
R --vanilla -e "shiny::runApp('${SCRIPTDIR}/apps/shiny/app.R',host='127.0.0.1',port=4826)"

```
