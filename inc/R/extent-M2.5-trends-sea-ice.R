require(raster)
require(dplyr)
require(magrittr)

source(sprintf("%s/proyectos/IUCN-GET/assessment-ecosystem-functional-groups/env/project-env.R",Sys.getenv("HOME")))

archs <- list.files(sprintf("%s/cryosphere/global/SeaIceIndex/",gis.data),
                    recursive = TRUE, pattern='extent',full.names = TRUE)
sort(archs)

sea_ice_extent <- tibble()
for (hh in c("N","S")) {
  for (yy in 1978:2020) {
    flt <- sprintf("%s_%s",hh,yy)
    rst <- stack(grep(flt,archs,value=T))
    names(rst) <- gsub("_extent_v3.0","",names(rst))
    vals <- data.frame(values(rst))
    year <- as.numeric(substr(colnames(vals),3,6))
    month <- as.numeric(substr(colnames(vals),7,9))
    sea_ice_extent %<>% 
      bind_rows(tibble(year,month,hemisphere=hh,ext=apply(vals,2,function(x) sum(x %in% 1))))
    
  }
}
out.file <- sprintf("%sOUTPUT/SeaIce-TS-extent.rds",work.dir)
saveRDS(file=out.file,sea_ice_extent)