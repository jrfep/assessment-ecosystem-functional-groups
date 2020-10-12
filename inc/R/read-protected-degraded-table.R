#!R --vanilla
require(RColorBrewer)
require(viridis)
require(dplyr)
require(tidyr)
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

#choosing colors

clrs <- ##brewer.pal(8,"Accent")
 brewer.pal(12,"Paired")
clr2 <- inferno(6)
 clr2 <- viridis(7)[-2]


 biome.labels <- c("Rivers","Lakes","Transitional waters", "Marine shelves","Pelagic ocean waters", "Deep seafloor","Brackish tidal systems", "Shoreline systems", "Tropical-subtropical forests", "Temperate-boreal forests and woodlands", "Shrublands & shrub-dominated woodlands", "Tropical-temperate grassy ecosystems", "Deserts and semi-deserts", "Polar/alpine (cryogenic ecosystems)", "Palustrine wetlands","Supralittoral")
 biome.labels <- c("F1 Rivers","F2 Lakes","FM1 Transitional waters", "M1 Marine shelves","M2 Pelagic ocean waters", "M3 Deep seafloor","MFT1 Brackish tidal systems", "MT1 Shoreline systems", "T1 Tropical-subtropical forests", "T2 Temperate-boreal forests", "T3 Shrub-dominated ecosystems", "T4 Savannas and grasslands", "T5 Deserts and semi-deserts", "T6 Cryogenic ecosystems", "TF1 Palustrine wetlands","MT2 Supralittoral systems")

 d.legend <- data.frame(lab=biome.labels, pch=c(17,17,17,15,15,15,15,15,16,16,16,16,16,16,17,0), col=clr2[c(1,6,3,4,2,1,5,6,1:6,2,4)], stringsAsFactors=F)

## output of inc/gras/rcross/combined-indicators.sh
versions <- dir(sprintf("%s/output",work.dir))
for (k in c("2013","all")) {
  maps.x.indicators <- data.frame()
  for (ver in versions) {
    archs <- dir(sprintf("%s/output/%s",work.dir,ver),pattern=sprintf("Protected_Degraded_%s",k))
    for (arch in archs) {
       dts <- read.table(sprintf("%s/output/%s/%s",work.dir,ver,arch),col.names=c("WDPA","HFP","MCHI","map","area_m2"),stringsAsFactors=FALSE)
       dts <- subset(dts,!map %in% "*")
       dts$map <- 1+(as.numeric(dts$map)>1.5)

       dts$map_code <- gsub(sprintf("Protected_Degraded_%s_|.txt",k),"",basename(arch))
       dts$EFG <- strsplit(dts$map_code,".IM.")[[1]][1]
       dts$version <- ver
       dts$area <- dts$area_m2/1e6
       maps.x.indicators <- rbind(maps.x.indicators,dts)
    }
  }
  ## review EFG data
  slc <- unique(maps.x.indicators$EFG)
  ## exclude the anthropogenic
  slc <- slc[!(grepl("^F3.?",slc) | grepl("^T7.?",slc) | grepl("^M4.?",slc) | grepl("^MT3.?",slc) | grepl("^S2.?",slc) | grepl("^SF2.?",slc))]
  ## Ice and snow groups in the southern hemisphere are not well covered by human impact variables
  # slc <- slc[!slc %in% c("T6.1","T6.2","M2.5","F2.10")]
  ## Subterranean EFG are not well covered by the protection/degradation variables
  slc <- slc[!grepl("^S",slc)]


  EFG.dts <- maps.x.indicators %>% filter(EFG %in% slc)

  EFG.dts$clase <- "unknown"

  EFG.dts$clase[with(EFG.dts,(HFP %in% 1) | (MCHI %in% 1))] <- "degraded"
  EFG.dts$clase[with(EFG.dts,(WDPA %in% 1))] <- "protected"
  EFG.dts$clase[with(EFG.dts,((HFP %in% 0) | (MCHI %in% 0)) & (WDPA %in% "*"))] <- "wild unprotected"

  EFG.dts$realm <- gsub("[0-9.]","",EFG.dts$EFG)
  EFG.dts$biome <- gsub("\\.[0-9]+","",EFG.dts$EFG)

  EFG.dts$biome.lab <- biome.labels[pmatch(EFG.dts$biome,biome.labels,duplicates.ok=T)]

 ## separate pixels from terrestrial and marine areas
  EFG.dts$terrestrial <- EFG.dts$HFP %in% c("0","1")
  EFG.dts$marine <- EFG.dts$MCHI %in% c("0","1")

 ## This will allow to filter out areas with inconsistent data (terrestrial ecosystems in marine areas and viceversa)


 ##  large lakes have several water (no data) pixels in terrestrial areas
 EFG.dts %>% pivot_wider(
      id_cols = c("EFG","version","realm","biome","biome.lab","terrestrial","marine"),
      names_from = clase,
      values_from = area,
      values_fn = list(area=sum)
      ) %>%
      replace_na(list(degraded=0,protected=0,unknown=0,`wild unprotected`=0)) %>%
        mutate(total=degraded+unknown+protected+`wild unprotected`) %>%
          mutate(degraded=degraded*100/total,protected=protected*100/total,`wild unprotected`=`wild unprotected`*100/total,unknown=unknown*100/total) %>% filter(EFG %in% "F2.1") %>% select(marine,terrestrial,version,protected,degraded,`wild unprotected`,unknown,total)

 ## and some maps for terrestrial ecosystems overflowing into marine areas near the coast due to aggregation and other resolution problems
 EFG.dts %>% pivot_wider(
     id_cols = c("EFG","version","realm","biome","biome.lab","terrestrial","marine"),
     names_from = clase,
     values_from = area,
     values_fn = list(area=sum)
     ) %>%
     replace_na(list(degraded=0,protected=0,unknown=0,`wild unprotected`=0)) %>%
       mutate(total=degraded+unknown+protected+`wild unprotected`) %>%
         mutate(degraded=degraded*100/total,protected=protected*100/total,`wild unprotected`=`wild unprotected`*100/total,unknown=unknown*100/total) %>% filter(EFG %in% "T2.5") %>% select(marine,terrestrial,version,protected,degraded,`wild unprotected`,unknown,total)

  EFG.dts %>% group_by(clase) %>% summarise(area=sum(area))



 EFG.dts %>%
   pivot_wider(
        id_cols = c("EFG","version","realm","biome","biome.lab","terrestrial","marine"),
        names_from = clase,
        values_from = area,
        values_fn = list(area=sum)
        ) %>%
        replace_na(list(degraded=0,protected=0,unknown=0,`wild unprotected`=0)) %>%
          mutate(total=degraded+unknown+protected+`wild unprotected`) %>%
            mutate(degraded=degraded*100/total,protected=protected*100/total,`wild unprotected`=`wild unprotected`*100/total,unknown=unknown*100/total,group=ifelse(realm %in% c("T") & terrestrial & !marine,"Terrestrial",
              ifelse(realm %in% c("F","TF") & terrestrial & !marine,"Freshwater",
              ifelse(realm %in% c("MFT","FM",  "MT",  "TM") & terrestrial & !marine,"Transitional",
              ifelse(realm %in% c("M", "MT", "MFT", "TM", "FM") & !terrestrial & marine,"Marine",NA))))) %>%
              filter(!is.na(group)) ->
               EFG.data

 save(file=sprintf("%s/Rdata/Degraded-protected-%s-all-versions.rda", script.dir,k), maps.x.indicators, EFG.data,d.legend, EFG.names)
 save(file=sprintf("%s/apps/shiny/Rdata/summary.rda", script.dir), EFG.data,d.legend)

}
