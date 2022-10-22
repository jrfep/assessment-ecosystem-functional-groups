## Cummulative Human Impacts (Marine)

Attribution: https://www.nceas.ucsb.edu/globalmarine/attribution

Original publication: https://www.nature.com/articles/ncomms8615

#### Citation

> Benjamin Halpern, Melanie Frazier, John Potapenko, Kenneth Casey, Kellee Koenig, et al. 2015. Cumulative human impacts: raw stressor data (2008 and 2013). Knowledge Network for Biocomplexity. doi:10.5063/F1S180FS.
> Benjamin Halpern, Melanie Frazier, John Potapenko, Kenneth Casey, Kellee Koenig, et al. 2015. Cumulative human impacts: Supplementary data. Knowledge Network for Biocomplexity. doi:10.5063/F19Z92TW.
> Benjamin Halpern, Melanie Frazier, John Potapenko, Kenneth Casey, Kellee Koenig, et al. 2015. Cumulative human impacts: pressure and cumulative impacts data (2013, all pressures). Knowledge Network for Biocomplexity. doi:10.5063/F15718ZN.
> Benjamin Halpern, Melanie Frazier, John Potapenko, Kenneth Casey, Kellee Koenig, et al. Cumulative human impacts: pressure and cumulative impacts data (2008 and 2013, subset of pressures). Knowledge Network for Biocomplexity. doi:10.5063/F11J97N3.

#### Data access

Files downloaded from https://knb.ecoinformatics.org/view/doi:10.5063/F15718ZN and https://doi.org/10.5063/F11J97N3

#### Data preparation


```sh
grass --text -c global_cumul_impact_2013_all_layers.tif  $GISDB/raw/MCHI

r.in.gdal --overwrite input=global_cumul_impact_2008_all_layers_except_shipping_oceanpollution_invasives.tif output=MCHI2008
r.null  MCHI2008 setnull=0
r.colors map=MCHI2008 color=bcyr

r.in.gdal --overwrite  input=global_cumul_impact_2013_all_layers_except_shipping_oceanpollution_invasives_slr.tif output=MCHI2013
## set null to avoid underestimation over coastal areas
r.null  MCHI2013 setnull=0
r.colors -e map=MCHI2013 color=bcyr

exit

```
