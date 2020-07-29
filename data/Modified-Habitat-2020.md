## Modified-Habitat-2020

Natural and Modified Habitat Screening Layer. This dataset classifies the global terrestrial land surface into one of four categories: likely Natural, potential Natural, potential Modified, and likely Modified.

## DOCUMENTATION:

#### Citation

> Joe Gosling, Matt I. Jones, Andy Arnell, James E.M. Watson, Oscar Venter, Andrea C. Baquero, Neil D. Burgess, A global mapping template for natural and modified habitat across terrestrial Earth, Biological Conservation, 2020, 108674, ISSN 0006-3207, https://doi.org/10.1016/j.biocon.2020.108674.

#### Data access

Files from

#### Data preparation

```sh
# wget --continue https://datadownload-production.s3.amazonaws.com/WCMC_natural_modified_habitat_screening_layer.zip

grass --text -c WCMC_natural_modified_habitat_screening_layer/natural_modified_habitat_screening_layer.tif $GISDB/raw/Modified-Habitat-2020
r.in.gdal input=WCMC_natural_modified_habitat_screening_layer/natural_modified_habitat_screening_layer.tif output=modified_habitat
exit

```
