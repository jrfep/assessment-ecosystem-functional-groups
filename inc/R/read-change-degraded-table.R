#!R --vanilla
require(dplyr)

work.dir <- Sys.getenv("WORKDIR")
script.dir <- Sys.getenv("SCRIPTDIR")

setwd(work.dir)
system(sprintf("mkdir -p %s/Rdata/",script.dir))

## output of inc/gras/rcross/combined-indicators.sh
versions <- dir(sprintf("%s/output",work.dir))

EFG.difs <- data.frame()
for (ver in versions) {
  archs <- dir(sprintf("%s/output/%s",work.dir,ver),pattern="Degraded_Change_")
  for (arch in archs) {
     dts <- read.table(sprintf("%s/output/%s/%s",work.dir,ver,arch), col.names=c("Diff", "map","area_m2"), stringsAsFactors=FALSE)
     if (nrow(dts)>0) {
       dts$map_code <- gsub("Degraded_Change_Marine_|Degraded_Change_Terrestrial_|.txt","",basename(arch))
       dts$EFG <- strsplit(dts$map_code,".IM.")[[1]][1]
       dts$version <- ver
       dts$indicator <- ifelse(grepl("Marine",arch),"Marine","Terrestrial")
       dts$area <- dts$area_m2/1e6
       dts$p.area <- dts$area_m2/sum(dts$area_m2)
       dts <- subset(dts,!map %in% "*" & !Diff %in% "*")
       dts$Diff <- as.numeric(dts$Diff)
       EFG.difs <- rbind(EFG.difs,dts)
     } else {
       cat(sprintf("%s > %s is empty",ver, arch))
     }
  }
}

#table(EFG.difs$EFG,EFG.difs$ver)


 slc <- unique(EFG.difs$EFG)
 ## exclude the anthropogenic
 slc <- slc[!(grepl("^F3.?",slc) | grepl("^T7.?",slc) | grepl("^M4.?",slc) | grepl("^MT3.?",slc) | grepl("^S2.?",slc) | grepl("^SF2.?",slc))]
 ## Ice and snow groups in the southern hemisphere are not well covered by human impact variables
 slc <- slc[!slc %in% c("T6.1","T6.2","M2.5","F2.10")]
 ## Subterranean EFG are not well covered by the protection/degradation variables
 slc <- slc[!slc %in% !grepl("^S",slc)]


 EFG.difs <- EFG.difs %>% filter(EFG %in% slc)


save(file=sprintf("%s/Rdata/Degraded-change-all-versions.rda", script.dir), EFG.difs)
