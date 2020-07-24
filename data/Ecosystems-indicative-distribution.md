# Indicative distribution maps

#### Citation

The latest version of the dataset is available at ![10.5281/zenodo.3546513](https://zenodo.org/badge/doi/10.5281/zenodo.3546513.svg)

Here we will compare version 1.1 and version 2.0 of the indicative maps:

> Keith, David A., Ferrer-Paris, Jose R., Nicholson, Emily, Bishop, Melanie J., Polidoro, Beth A., Ramirez-Llodra, Eva, … Kingsford, Richard T. (2020). Indicative distribution maps for Ecological Functional Groups - Level 3 of IUCN Global Ecosystem Typology (Version 2.0.0) [Data set]. Zenodo. http://doi.org/10.5281/zenodo.3958934

And

> Keith, David A., Ferrer-Paris, Jose R., Nicholson, Emily, Bishop, Melanie J., Polidoro, Beth A., Ramirez-Llodra, Eva, … Kingsford, Richard T. (2020). Indicative distribution maps for Ecological Functional Groups - Level 3 of IUCN Global Ecosystem Typology (Version 1.1.0) [Data set]. Zenodo. http://doi.org/10.5281/zenodo.3958622


#### Data preparation

It is possible to automate download from the Zenodo repository using the API. For example, using the *R* package **[zen4R](https://github.com/eblondel/zen4R)**. We need to retrieve the Zenodo API token from a file in the home directory or from an environment variable.

```R
R --vanilla
require(zen4R)

##output directory
gis.outdir <- Sys.getenv("GISOUT")

system(sprintf("mkdir -p %s/version-0.0.1",gis.outdir))
zenodoToken <- Sys.getenv("ZENODOTOKEN")
##alternative: readLines("~/.ZenodoToken")

zenodo <- ZenodoManager$new(
   token = zenodoToken,
   logger = "INFO"
)

versions <- c("version-1.1.0"="10.5281/zenodo.3958622","version-2.0.0"="10.5281/zenodo.3958934")
for (j in 1:2) {
  system(sprintf("mkdir -p %s/%s",gis.outdir,names(versions)[j]))
  setwd(sprintf("%s/%s",gis.outdir,names(versions)[j]))
  my_rec <- zenodo$getRecordByDOI(versions[j])
  file_list <- zenodo$getFiles(my_rec$id)

  for (k in 1:length(file_list)) {
     system(sprintf("wget --continue '%s?access_token=%s' --output-document=%s",file_list[[k]]$links$download,zenodoToken, file_list[[k]]$filename))
  }
}
q()
```

Now we can go through the folders and decompress the files:

```sh
for version in 1.1.0 2.0.0
do
  cd $GISOUT/version-$version
  for arch in $(ls *tar.bz2)
  do
    tar -xjvf $arch
    rm $arch
  done
done
```

All GeoTIFF files should be now in these two folders.
