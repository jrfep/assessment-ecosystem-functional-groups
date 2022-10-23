# *workflow* folder

This file describes the steps for reproducing the analysis in ***TO DO Ferrer-Paris et al.***, from data import to visualisation of results.


# Ecosystem Functional Groups

## F2.1
large lakes + GSW
## F1.3 and F2.4
river/lake ice



## T5.5
### Abiotic degradation

## M2.5
### Regional subdivision

Behera et al. 2020 suggest following regions in the Antarctic:
> The five sectors are the Weddell Sea sector (60◦W to 20◦E), the Indian Ocean (20◦E to 90◦E), the Pacific Ocean (90◦E to 160◦E), the Ross Sea (160◦E to 140◦W), and the Bellam or (Amundsen-Bellingshausen) Sea sectors (140◦W to 60◦W), following Lefebvre et al. (2004).

Or see CCAMLR divisions and subareas

### Spatial extent

Sea ice extent from
> Fetterer, F., K. Knowles, W. N. Meier, M. Savoie, and A. K. Windnagel. 2017, updated daily. Sea Ice Index, Version 3. [Indicate subset used]. Boulder, Colorado USA. NSIDC: National Snow and Ice Data Center. doi: https://doi.org/10.7265/N5K072F8. [Date Accessed].


### Abiotic degradation

Sea ice concentration from
> Fetterer, F., K. Knowles, W. N. Meier, M. Savoie, and A. K. Windnagel. 2017, updated daily. Sea Ice Index, Version 3. [Indicate subset used]. Boulder, Colorado USA. NSIDC: National Snow and Ice Data Center. doi: https://doi.org/10.7265/N5K072F8. [Date Accessed].

Sea surface temperature? See Behera et al. 2020

### Biotic disruption

Chlorophyll-a concentration? See Behera et al. 2020


## Environment and requirements
This workflow has been created using the following operating system and software:

* MacOS and Linux Solus
* R ???
* Python ???
* GDAL ???

To set-up programming environmental variables edit the files in folder `env/` and source them in a bash shell: `source env/project-env.sh`.

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
R --vanilla CMD BATCH $SCRIPTDIR/inc/R/figure-change-protected.R
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
