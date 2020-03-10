# Indicative distribution maps

#### Citation

> Keith, David A., Ferrer-Paris, Jose R., Nicholson, Emily, Bishop, Melanie J., Polidoro, Beth A., Ramirez-Llodra, Eva, … Kingsford, Richard T. (2019). Indicative distribution maps for Ecological Functional Groups - Level 3 of IUCN Global Ecosystem Typology (Version 0.0.1) [Data set]. Zenodo. http://doi.org/10.5281/zenodo.3546514
#### Data access

Download data from Zenodo


#### Data preparation

Unzip files and import to GRASS GIS
```R
R --vanilla
require(zen4R)
zenodoToken <- readLines("~/.ZenodoToken")
zenodo <- ZenodoManager$new(
   token = zenodoToken,
   logger = "INFO"
)
my_rec <- zenodo$getRecordByDOI("10.5281/zenodo.3546514")
```
