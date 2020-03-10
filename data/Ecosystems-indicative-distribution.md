# Indicative distribution maps

#### Citation

> Keith, David A., Ferrer-Paris, Jose R., Nicholson, Emily, Bishop, Melanie J., Polidoro, Beth A., Ramirez-Llodra, Eva, … Kingsford, Richard T. (2019). Indicative distribution maps for Ecological Functional Groups - Level 3 of IUCN Global Ecosystem Typology (Version 0.0.1) [Data set]. Zenodo. http://doi.org/10.5281/zenodo.3546514

![10.5281/zenodo.3546513](https://zenodo.org/badge/doi/10.5281/zenodo.3546513.svg)

#### Data access

Download data from Zenodo

#### Data preparation

It is possible to automate download from the Zenodo repository using the API. For example, using the *R* package **[zen4R](https://github.com/eblondel/zen4R)**:

```R
R --vanilla
require(zen4R)
zenodoToken <- readLines("~/.ZenodoToken")
zenodo <- ZenodoManager$new(
   token = zenodoToken,
   logger = "INFO"
)
my_rec <- zenodo$getRecordByDOI("10.5281/zenodo.3546514")
file_list <- zenodo$getFiles(my_rec$id)

for (k in 1:length(file_list)) {
   system(sprintf("wget --continue %s --output-document=%s",file_list[[k]]$links$download, file_list[[k]]$filename))
   system(sprintf("tar -xjvf %s",file_list[[k]]$filename))
   system(sprintf("mv *.tif %s",Sys.getenv("GISOUT")))
}
q()
```

All GeoTIFF files should be now in the `$GISOUT` folder.
