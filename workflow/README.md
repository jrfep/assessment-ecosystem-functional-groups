# *workflow* folder

This file describes the steps for reproducing the analysis in *Appendix S5. Preliminary global assessment of pressures and protection of Ecosystem Functional Groups, from data import to the production of outputs and products* from Keith *et al.* (submitted)

## Environment and requirements
This workflow has been creating using the following operating system and software:
* Xubuntu 18.04
* R 3.6.1
* Python 3.7.3
* GRASS GIS 7.4.0 (64 bit)
* GDAL 2.2.3

To set-up useful environmental variables edit the file in `env/terra.sh` and source it in a bash shell:
`source env/terra.sh`

## Data import
 `workflow/00-gis-data-import.sh` performs the following:
 * Import or download spatial data from several sources
 * Create a series of spatial databases for use with Grass GIS software for importing projected data sources.
 * Creates and organizes the main spatial database for the analysis using Grass GIS.

##

##
