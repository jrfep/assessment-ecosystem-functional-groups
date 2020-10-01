#!R --vanilla
require(dplyr)
require(xml2)


work.dir <- Sys.getenv("WORKDIR")
script.dir <- Sys.getenv("SCRIPTDIR")

setwd(work.dir)
system(sprintf("mkdir -p %s/Rdata/",script.dir))

EFG.names <- c()
for (arch in dir(sprintf("%s/indicative-maps/version-2.0.1b/",work.dir),"xml",full.names=T)) {
  x <- read_xml(arch)
  EFG.names <- c(EFG.names,xml_text(xml_find_all(x,"//Short-name")))
}

## output of inc/gras/rcross/combined-indicators.sh
versions <- dir(sprintf("%s/output",work.dir))

EFG.prot <- data.frame()
for (ver in versions) {
  archs <- dir(sprintf("%s/output/%s",work.dir,ver),pattern="Protected_year_")
  for (arch in archs) {
     dts <- read.table(sprintf("%s/output/%s/%s",work.dir,ver,arch), col.names=c("Year", "map","area_m2"), stringsAsFactors=FALSE)
     if (nrow(dts)>0) {
       dts$map_code <- gsub("Protected_year_|.txt","",basename(arch))
       dts$EFG <- strsplit(dts$map_code,".IM.")[[1]][1]
       dts$version <- ver
       dts$area <- dts$area_m2/1e6
       dts <- subset(dts,!map %in% "*" )
       dts$p.area <- dts$area_m2/sum(dts$area_m2)
       dts$Year <- as.numeric(dts$Year)
       tt <- data.frame(EFG=strsplit(dts$map_code,".IM.")[[1]][1],
        version=ver,
         pre.2000=dts %>% filter(!is.na(Year) & Year<2010) %>% select(p.area) %>% sum ,
         cur.2020=dts %>% filter(!is.na(Year)) %>% select(p.area) %>% sum
       )
       EFG.prot <- rbind(EFG.prot,tt)
     } else {
       cat(sprintf("%s > %s is empty",ver, arch))
     }
  }
}

#table(EFG.difs$EFG,EFG.difs$ver)


 slc <- unique(EFG.prot$EFG)
 ## exclude the anthropogenic
 slc <- slc[!(grepl("^F3.?",slc) | grepl("^T7.?",slc) | grepl("^M4.?",slc) | grepl("^MT3.?",slc) | grepl("^S2.?",slc) | grepl("^SF2.?",slc))]
 ## Ice and snow groups in the southern hemisphere are not well covered by human impact variables
 # slc <- slc[!slc %in% c("T6.1","T6.2","M2.5","F2.10")]
 ## Subterranean EFG are not well covered by the protection/degradation variables
 slc <- slc[!grepl("^S",slc)]


 EFG.prot <- EFG.prot %>% filter(EFG %in% slc)


save(file=sprintf("%s/Rdata/Protected-change-all-versions.rda", script.dir), EFG.prot, EFG.names)
