## Global Human Modification map

> The global Human Modification map (HM) provides a cumulative measure of human modification of terrestrial lands across the globe at a 1-km resolution. It is a continuous 0-1 metric that reflects the proportion of a landscape modified based on modeling the physical extents of 13 anthropogenic stressors and their estimated impacts using spatially-explicit global datasets with a median year of 2016.

## DOCUMENTATION:

#### Citation

> Kennedy CM, Oakleaf JR, Theobald DM, Baruch‐Mordo S, Kiesecker J. Managing the middle: A shift in conservation priorities based on the global human modification gradient. Glob Change Biol. 2019;25:811–826. https://doi.org/10.1111/gcb.14549

#### Data access

Files downloaded from:
> M. Kennedy, Christina; Oakleaf, James; M. Theobald, David; Baruch-Mordo, Sharon; Kiesecker, Joseph (2018): Global Human Modification. figshare. Dataset. https://doi.org/10.6084/m9.figshare.7283087.v1

#### Data preparation

```sh
#wget https://ndownloader.figshare.com/articles/7283087/versions/1 and unzip
grass --text -c gHM/gHM.tif $GISDB/raw/gHM
r.in.gdal input=gHM/gHM.tif output=gHM2019
exit
```
