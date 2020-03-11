#!R --vanilla
work.dir <- Sys.getenv("WORKDIR")
Rdata.dir <- sprintf("%s/Rdata",Sys.getenv("SCRIPTDIR"))

setwd(work.dir)
system(sprintf("mkdir -p %s",Rdata.dir))

## output of inc/gras/rcross/combined-indicators.sh
archs <- dir(pattern="Protected_Degraded_2013")

all.dts <- data.frame()
for (arch in archs) {
   dts <- read.table(arch,col.names=c("WDPA","HFP","MCHI","map","area_m2"))
   dts$EFG <- gsub("_",".",strsplit(gsub("Protected_Degraded_2013_|.txt","",basename(arch)),".IM.")[[1]][1])
   dts$MAP <- strsplit(gsub("Protected_Degraded_2013_|.txt","",basename(arch)),".IM.")[[1]][2]
   dts$area <- dts$area_m2/1e6
   dts <- subset(dts,!map %in% "*")
   all.dts <- rbind(all.dts,dts)
}


save(file=sprintf("%s/Degraded-protected-2013.rda", Rdata.dir), all.dts)
