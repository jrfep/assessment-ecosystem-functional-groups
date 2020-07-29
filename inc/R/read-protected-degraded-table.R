#!R --vanilla
work.dir <- Sys.getenv("WORKDIR")
Rdata.dir <- sprintf("%s/Rdata",Sys.getenv("SCRIPTDIR"))

setwd(work.dir)
system(sprintf("mkdir -p %s",Rdata.dir))

## output of inc/gras/rcross/combined-indicators.sh
versions <- dir(sprintf("%s/output",work.dir))

maps.x.indicators <- data.frame()
for (ver in versions) {
  archs <- dir(sprintf("%s/output/%s",work.dir,ver),pattern="Protected_Degraded_2013")
  for (arch in archs) {
     dts <- read.table(sprintf("%s/output/%s/%s",work.dir,ver,arch),col.names=c("WDPA","HFP","MCHI","map","area_m2"),stringsAsFactors=FALSE)
     dts <- subset(dts,!map %in% "*")
     dts$map <- 1+(as.numeric(dts$map)>1.5)

     dts$map_code <- gsub("Protected_Degraded_2013_|.txt","",basename(arch))
     dts$EFG <- strsplit(dts$map_code,".IM.")[[1]][1]
     dts$version <- ver
     dts$area <- dts$area_m2/1e6
     maps.x.indicators <- rbind(maps.x.indicators,dts)
  }
}

save(file=sprintf("%s/Degraded-protected-2013-all-versions.rda", Rdata.dir), maps.x.indicators)
