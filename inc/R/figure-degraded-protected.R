##R --vanilla
require(dplyr)
require(tidyr)
require(ggplot2)
require(ggrepel)
require(ggpubr)
require(RColorBrewer)
require(viridis)

work.dir <- Sys.getenv("WORKDIR")
fig.dir <- sprintf("%s/output/",Sys.getenv("SCRIPTDIR"))
Rdata.dir <- sprintf("%s/Rdata/",Sys.getenv("SCRIPTDIR"))
shiny.dir <- sprintf("%s/apps/shiny/",Sys.getenv("SCRIPTDIR"))
setwd(work.dir)

#choosing colors
clrs <- ##brewer.pal(8,"Accent")
 brewer.pal(12,"Paired")
clr2 <- inferno(6)
 clr2 <- viridis(7)[-2]


load(sprintf("%s/Degraded-protected-2013-all-versions.rda", Rdata.dir))

biome.labels <- c("Rivers","Lakes","Transitional waters", "Marine shelves","Pelagic ocean waters", "Deep seafloor","Brackish tidal systems", "Shoreline systems", "Tropical-subtropical forests", "Temperate-boreal forests and woodlands", "Shrublands & shrub-dominated woodlands", "Tropical-temperate grassy ecosystems", "Deserts and semi-deserts", "Polar/alpine (cryogenic ecosystems)", "Palustrine wetlands","Supralittoral")
biome.labels <- c("F1 Rivers","F2 Lakes","FM1 Transitional waters", "M1 Marine shelves","M2 Pelagic ocean waters", "M3 Deep seafloor","MFT1 Brackish tidal systems", "MT1 Shoreline systems", "T1 Tropical-subtropical forests", "T2 Temperate-boreal woodlands", "T3 Shrub-dominated woodlands", "T4 Grassy ecosystems", "T5 Deserts and semi-deserts", "T6 Cryogenic ecosystems", "TF1 Palustrine wetlands","MT2 Supralittoral systems")

d.legend <- data.frame(lab=biome.labels,
pch=c(17,17,17,15,15,15,15,15,16,16,16,16,16,16,17,0),
col=clr2[c(1,6,3,4,2,1,5,6,1:6,2,4)],
stringsAsFactors=F)

## review EFG data

slc <- unique(maps.x.indicators$EFG)
## exclude the anthropogenic
slc <- slc[!(grepl("^F3.?",slc) | grepl("^T7.?",slc) | grepl("^M4.?",slc) | grepl("^MT3.?",slc) | grepl("^S2.?",slc) | grepl("^SF2.?",slc))]
## Ice and snow groups in the southern hemisphere are not well covered by human impact variables
slc <- slc[!slc %in% c("T6.1","T6.2","M2.5","F2.10")]
## Subterranean EFG are not well covered by the protection/degradation variables
slc <- slc[!slc %in% !grepl("^S",slc)]
## These two freshwater groups are problematic
##slc <- slc[!slc %in% c("F2.1","F2.6")]


EFG.dts <- maps.x.indicators %>% filter(EFG %in% slc)

EFG.dts$clase <- "unknown"

EFG.dts$clase[with(EFG.dts,(HFP %in% 1) | (MCHI %in% 1))] <- "degraded"
EFG.dts$clase[with(EFG.dts,(WDPA %in% 1))] <- "protected"
EFG.dts$clase[with(EFG.dts,((HFP %in% 0) | (MCHI %in% 0)) & (WDPA %in% "*"))] <- "wild unprotected"

EFG.dts$realm <- gsub("[0-9.]","",EFG.dts$EFG)
EFG.dts$biome <- gsub("\\.[0-9]","",EFG.dts$EFG)

EFG.dts$biome.lab <- biome.labels[pmatch(EFG.dts$biome,biome.labels,duplicates.ok=T)]

EFG.dts$terrestrial <- EFG.dts$HFP %in% c("0","1")
EFG.dts$marine <- EFG.dts$MCHI %in% c("0","1")

EFG.dts[(EFG.dts$realm %in% c("T","TF")) & EFG.dts$marine & !EFG.dts$terrestrial,"clase"] <- "inconsistent"
EFG.dts[(EFG.dts$realm %in% c("M")) & EFG.dts$terrestrial & !EFG.dts$marine ,"clase"] <- "inconsistent"

EFG.dts %>% group_by(clase) %>% summarise(area=sum(area))

## first test to see if we should exclude some EFGs

EFG.dts %>%
  pivot_wider(
       id_cols = c("EFG","version"),
       names_from = clase,
       values_from = area,
       values_fn = list(area=sum)
     ) %>%        replace_na(list(degraded=0,protected=0,unknown=0,`wild unprotected`=0)) %>%
          mutate(total=degraded+unknown+protected+`wild unprotected`) %>% mutate(degraded=degraded/total,protected=protected/total,`wild unprotected`=`wild unprotected`/total,unknown=unknown/total)     -> tmp


## areas with no-data represent more than 5% of the total
tmp %>% filter(unknown>0.05 ) %>% select(EFG,version,unknown,inconsistent)

## inconsistencies larger than 5% (in this case terrestrial ecosystems overflowing marine areas?)
tmp %>%  select(EFG,version,unknown,inconsistent,total) %>% mutate(inconsistent=inconsistent/total) %>% filter(inconsistent>0.05)


## filtering by marine/terrestrial pixels fixes these problems

EFG.dts %>% filter(realm %in% c("T") & terrestrial & version %in% "version-1.1.0") %>%
     pivot_wider(
       id_cols = c("biome.lab","EFG"),
       names_from = clase,
       values_from = area,
       values_fn = list(area=sum)
     ) %>%  mutate(total=degraded+protected+`wild unprotected`) %>% mutate(degraded=degraded*100/total,protected=protected*100/total,`wild unprotected`=`wild unprotected`*100/total)   -> d.ter.1


EFG.dts %>% filter(realm %in% c("F","TF") & terrestrial & version %in% "version-1.1.0") %>%
pivot_wider(
  id_cols = c("biome.lab","EFG"),
  names_from = clase,
  values_from = area,
  values_fn = list(area=sum)
) %>%  mutate(total=degraded+protected+`wild unprotected`) %>% mutate(degraded=degraded*100/total,protected=protected*100/total,`wild unprotected`=`wild unprotected`*100/total)   -> d.fwt.1


EFG.dts %>% filter(realm %in% c("MFT","FM",  "MT",  "TM") & terrestrial & version %in% "version-1.1.0") %>%
     pivot_wider(
       id_cols = c("biome.lab","EFG"),
       names_from = clase,
       values_from = area,
       values_fn = list(area=sum)
     ) %>%  mutate(total=degraded+protected+`wild unprotected`) %>% mutate(degraded=degraded*100/total,protected=protected*100/total,`wild unprotected`=`wild unprotected`*100/total)   -> d.tra.1



EFG.dts %>% filter(realm %in% c("M", "MT", "MFT", "TM", "FM") & marine & version %in% "version-1.1.0") %>%
pivot_wider(
  id_cols = c("biome.lab","EFG"),
  names_from = clase,
  values_from = area,
  values_fn = list(area=sum)
) %>%  mutate(total=degraded+protected+`wild unprotected`) %>% mutate(degraded=degraded*100/total,protected=protected*100/total,`wild unprotected`=`wild unprotected`*100/total)   -> d.mar.1


    ## EFG plots


pplotT <- ggplot(d.ter.1, aes(degraded, protected, colour = biome.lab, shape=biome.lab)) +
  geom_point( size = 2) +
  scale_colour_manual(unique(d.ter.1$biome.lab),
    values=d.legend$col[unique(match(d.ter.1$biome.lab,d.legend$lab))]) +
  scale_shape_manual(unique(d.ter.1$biome.lab),
    values=d.legend$pch[unique(match(d.ter.1$biome.lab,d.legend$lab))]) +
    geom_hline(yintercept = 17, color="black",lty=3,lwd=.5) +
    geom_vline(xintercept=70,color="black",lty=3,lwd=.5) + coord_cartesian(xlim=c(0,100),ylim=c(0,45))

pplotF <- ggplot(d.fwt.1, aes(degraded, protected, color = biome.lab, shape=biome.lab)) +
  geom_point( size = 2) +
  scale_colour_manual(unique(d.fwt.1$biome.lab),
    values=d.legend$col[unique(match(d.fwt.1$biome.lab,d.legend$lab))]) +
  scale_shape_manual(unique(d.fwt.1$biome.lab),
    values=d.legend$pch[unique(match(d.fwt.1$biome.lab,d.legend$lab))]) +
    geom_hline(yintercept = 17, color="black",lty=3,lwd=.5) +
    geom_vline(xintercept=70,color="black",lty=3,lwd=.5) + coord_cartesian(xlim=c(0,100),ylim=c(0,45))

pplotR <- ggplot(d.tra.1, aes(degraded, protected, colour = biome.lab, shape=biome.lab)) +
  geom_point( size = 2) +
  scale_colour_manual(unique(d.tra.1$biome.lab),
    values=d.legend$col[unique(match(d.tra.1$biome.lab,d.legend$lab))]) +
  scale_shape_manual(unique(d.tra.1$biome.lab),
    values=d.legend$pch[unique(match(d.tra.1$biome.lab,d.legend$lab))]) +
    geom_hline(yintercept = 17, color="black",lty=3,lwd=.5) +
    geom_vline(xintercept=70,color="black",lty=3,lwd=.5) + coord_cartesian(xlim=c(0,100),ylim=c(0,45))

pplotM <- ggplot(d.mar.1, aes(degraded, protected, color = biome.lab, shape=biome.lab)) +
  geom_point( size = 2) +
  scale_colour_manual(unique(d.mar.1$biome.lab),
    values=d.legend$col[unique(match(d.mar.1$biome.lab,d.legend$lab))]) +
  scale_shape_manual(unique(d.mar.1$biome.lab),
    values=d.legend$pch[unique(match(d.mar.1$biome.lab,d.legend$lab))]) +
    geom_hline(yintercept = 17, color="black",lty=3,lwd=.5) +
    geom_vline(xintercept=70,color="black",lty=3,lwd=.5) + coord_cartesian(xlim=c(0,100),ylim=c(0,45))

plotM <- pplotM  + #geom_text_repel(aes(label = Code),colour=1,size=3) +
  labs( x = "% exposed to high pressures", y = "% protected",colour = "Biomes",shape = "Biomes") +
  theme_classic() +
  theme(legend.position = c(.05, .95), legend.justification = c("left", "top"), legend.box.just = "right",  legend.direction = "horizontal", legend.box.background = element_rect(), legend.margin = margin(0, 0, 0, 0),  legend.title=element_blank(), legend.text = element_text(size=6,angle=0,colour ="black"), axis.title = element_text(size = 8), axis.text = element_text(size = 7), panel.border=element_rect(colour="black",fill=NA,size=1))
  plotT <- pplotT  + #geom_text_repel(aes(label = Code),colour=1,size=3) +
    labs( x = "% exposed to high pressures", y = "% protected",colour = "Biomes",shape = "Biomes") +
    theme_classic() +
    theme(legend.position = c(.05, .95), legend.justification = c("left", "top"), legend.box.just = "right",  legend.direction = "horizontal", legend.box.background = element_rect(), legend.margin = margin(1, 1, 1, 1),  legend.title=element_blank(), legend.text = element_text(size=6,angle=0,colour ="black"), axis.title = element_text(size = 8), axis.text = element_text(size = 7), panel.border=element_rect(colour="black",fill=NA,size=1))

plotR <- pplotR  + #geom_text_repel(aes(label = Code),colour=1,size=3) +
      labs( x = "% exposed to high pressures", y = "% protected",colour = "Biomes",shape = "Biomes") +
      theme_classic() +
      theme(legend.position = c(.05, .95), legend.justification = c("left", "top"), legend.box.just = "right",  legend.direction = "horizontal", legend.box.background = element_rect(), legend.margin = margin(1, 1, 1, 1),  legend.title=element_blank(), legend.text = element_text(size=6,angle=0,colour ="black"), axis.title = element_text(size = 8), axis.text = element_text(size = 7), panel.border=element_rect(colour="black",fill=NA,size=1))

plotF <- pplotF  + #geom_text_repel(aes(label = Code),colour=1,size=3) +
  labs( x = "% exposed to high pressures", y = "% protected",colour = "Biomes",shape = "Biomes") +
  theme_classic() +
  theme(legend.position = c(.05, .95), legend.justification = c("left", "top"), legend.box.just = "right",  legend.direction = "horizontal", legend.box.background = element_rect(), legend.margin = margin(1, 1, 1, 1),  legend.title=element_blank(), legend.text = element_text(size=6,angle=0,colour ="black"), axis.title = element_text(size = 8), axis.text = element_text(size = 7), panel.border=element_rect(colour="black",fill=NA,size=1))

##  ggarrange(plotT, plotF, plotM, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")
      ggarrange(plotT, plotF, plotR,plotM, ncol=2, nrow=2, common.legend = F)


mi.legend <- d.legend
mi.legend$cc  <- rep(1,16)
mi.legend$rr <-  16:1
#data.frame(name=biome.labels, cc=rep(1,16), rr=16:1, pch=c(17,17,17,15,15,15,0,0,16,16,16,16,16,16,17,0),col=as.character(clr2[c(1,3,5, 6,1,3,1,4, 1:6,6,4)]),stringsAsFactors=F)

legEnd <- ggplot(mi.legend,aes(x=cc,y=rr,color=lab,shape=lab)) +
  theme_minimal() +
  scale_shape_manual(values=mi.legend$pch,labels=mi.legend$lab)+
  scale_color_manual(values=mi.legend$col,labels=mi.legend$lab)+
  geom_point( size = 2) +
  geom_text(aes(label = lab), colour=1, size=2, nudge_x=.0012, hjust="left") +
  labs( x = "", y = "") +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks=NULL,label=NULL) +
  scale_x_continuous(breaks=NULL,label=NULL) +
  coord_cartesian(xlim=c(1,5),ylim=c(0,16))


  library(cowplot)
  legend <- get_legend(
    # create some space to the left of the legend
    legEnd + theme(legend.box.margin = margin(0, 0, 0, 12))
  )


    prow <- plot_grid(
      plotT ,
      plotF ,
      plotR ,
      plotM ,
      align = 'vh',
      labels = c("A", "B", "C","D"),
      hjust = -1,
      nrow = 2
    )

prow
ggsave(file='DegradedProtectedPlot.pdf',device=pdf)

  prow <- plot_grid(
    plotT + theme(legend.position="none"),
    plotF + theme(legend.position="none"),
    plotR + theme(legend.position="none"),
    plotM + theme(legend.position="none"),
    align = 'vh',
    labels = c("A", "B", "C","D"),
    hjust = -1,
    nrow = 2
  )

  #prow

  plot_grid(prow, legend, rel_widths = c(3, 1))
            ## EFG plots

## now calculate for maps with version 2
d1 <- with(subset(EFG.dts, version %in% "version-2.0.0" & (gsub("[0-9.]","",EFG) %in% c("T", "TF", "MT", "MFT", "TM")) & HFP %in% c("0","1")), tapply(area,list(EFG,clase),sum))
d1 <- d1[,!colnames(d1) %in% c("inconsistent","unknown")]
d1 <- data.frame(d1*100/rowSums(d1))
d1$Code <- gsub("_",".",rownames(d1))
d1$grp <-  "Terrestrial"

d2 <- with(subset(EFG.dts, version %in% "version-2.0.0" & (gsub("[0-9.]","",EFG) %in% c("F", "FM")) & HFP %in% c("0","1")), tapply(area,list(EFG,clase),sum))
d2 <- d2[,!colnames(d2) %in% c("inconsistent","unknown")]
d2 <- data.frame(d2*100/rowSums(d2))
d2$Code <- gsub("_",".",rownames(d2))
d2$grp <-  "Freshwater"

d3 <- with(subset(EFG.dts,version %in% "version-2.0.0" & (gsub("[0-9.]","",EFG) %in% c("M", "MT", "MFT", "FM")) & MCHI %in% c("0","1")), tapply(area,list(EFG,clase),sum))
d3 <- d3[,!colnames(d3) %in% c("inconsistent","unknown")]
d3 <- data.frame(d3*100/rowSums(d3))
d3$Code <- gsub("_",".",rownames(d3))
d3$grp <-  "Marine"

e <- rbind(d1,d2,d3)

# combine version 2 with those unchanged
e <- rbind(e,d[!rownames(d) %in% rownames(e),])
e <- e[rownames(d),]
d$version <- "v1.0"
e$version <- "v2.0"

d <- rbind(d,e)

d$grp <- factor(d$grp,levels=c("Terrestrial","Freshwater","Marine"))
d$biome <- gsub("\\.[0-9]","",d$Code)

## this is for David
## write.csv(file="Table_Degraded_Protected_EFG.csv",d)


save(file=sprintf("%s/Rdata/summary.rda",shiny.dir),d,d.legend)

shiny::runApp(sprintf('%s/app.R',shiny.dir))





  ## Composite figure for manuscript

  ## 300 dpi
  ## width: 89 mm (single column) and 183 mm (double column)
  ## fullpage = 247 mm. use 1/4 to 1/3 of page
  fig.res = 300
  inch.to.mm = 25.4
  fig.width = 183

  fig.height = 247
  fig.width.one = 89
  fig.height.one = 247*2/5
  point.size = 7


Fig3 <-  ggarrange(plotT, plotF, plotM, legEnd,
                         labels = c("a", "b", "c",NA),
                         ncol = 2, nrow = 2)

Fig3.with.labels <-  ggarrange(plotT+geom_text_repel(aes(label = Code),colour=1,size=3),
   plotF+geom_text_repel(aes(label = Code),colour=1,size=3),
   plotM+geom_text_repel(aes(label = Code),colour=1,size=3),
   ##legEnd,
   labels = c("a", "b","c"),
   ncol = 1, nrow = 3)
## test colors
#plotT + scale_color_manual("", values=clrs[c(10,8,2,4,8,10,12,7,10,4)],labels=biome.labels) + theme(legend.position = "right", legend.text = element_text(size=10,angle=0,colour ="black"), axis.title = element_text(size = 18), axis.text = element_text(size = 17))
#plotF + scale_color_manual("", values=clrs[c(2,4,8)],labels=biome.labels) + theme(legend.position = "right", legend.text = element_text(size=10,angle=0,colour ="black"), axis.title = element_text(size = 18), axis.text = element_text(size = 17))
##MFT 10 MT 8
# plotM+    scale_color_manual("", values=clrs[c(8,2,10,9,10,8)],labels=biome.labels) + theme(legend.position = "right", legend.text = element_text(size=10,angle=0,colour ="black"), axis.title = element_text(size = 18), axis.text = element_text(size = 17))


 Fig3.alt.colors <-  ggarrange(plotT + scale_color_manual("", values=clrs[c(10,8,2,4,8,7,12,10,3,4)],labels=biome.labels),
    plotF+
     scale_color_manual("", values=clrs[c(2,4,8)],labels=biome.labels),
    plotM+
       scale_color_manual("", values=clrs[c(8,2,10,9,10,8,2)],labels=biome.labels),
    legEnd+
  scale_color_manual("", values=clrs[c(2,4,8, 2,10,9,10,8, 2,4,8,7,12,10,3,4)],labels=biome.labels),
                              labels = c("a", "b", "c",NA),
                              ncol = 2, nrow = 2)

Fig3.alt.colors.with.labels <-
ggarrange(plotT + scale_color_manual("", values=clrs[c(10,8,2,4,8,7,12,10,3,4)], labels=biome.labels) + geom_text_repel(aes(label = Code),colour=1,size=3) +
labs( x = "",
y = "",colour = "Biomes")  + theme(legend.position = "none", axis.title = element_text(size = 18), axis.text = element_text(size = 14), panel.border=element_rect(colour="black",fill=NA,size=1)) + scale_x_continuous(breaks=NULL,label=NULL)
,
    plotF+
     scale_color_manual("", values=clrs[c(2,4,8)],labels=biome.labels)+geom_text_repel(aes(label = Code),colour=1,size=3) + scale_x_continuous(breaks=NULL,label=NULL)  +
labs( x = "",
y = "% protected",colour = "Biomes")  + theme(legend.position = "none", axis.title = element_text(size = 18), axis.text = element_text(size = 14), panel.border=element_rect(colour="black",fill=NA,size=1)),
    plotM+
       scale_color_manual("", values=clrs[c(8,2,10,9,10,8,2)],labels=biome.labels)+geom_text_repel(aes(label = Code),colour=1,size=3) +
       labs( x = "% exposed to high pressures",
       y = "",colour = "Biomes")  + theme(legend.position = "none", axis.title = element_text(size = 18), axis.text = element_text(size = 14), panel.border=element_rect(colour="black",fill=NA,size=1)),
labels = c("a", "b","c"),
ncol = 1, nrow = 3)


  pdf(sprintf("%s /manuscript/Figure3_EFG_2columns_3panels_impact_protected.pdf",fig.dir),width= fig.width/inch.to.mm,height=(fig.height*2/5)/inch.to.mm,pointsize=point.size)
  Fig3
  dev.off()

  pdf(sprintf("%s/manuscript/Figure3_EFG_2columns_3panels_impact_protected_alternative_colors.pdf",fig.dir),width= fig.width/inch.to.mm,height=(fig.height*2/5)/inch.to.mm,pointsize=point.size)
  Fig3.alt.colors
  dev.off()


    pdf(sprintf("%s/supplement/Figure3_EFG_3panels_impact_protected_withLabels.pdf",fig.dir), width= fig.width/inch.to.mm, height=fig.height/inch.to.mm, pointsize=point.size)
    Fig3.with.labels
    dev.off()

      pdf(sprintf("%s/supplement/Figure3_EFG_3panels_impact_protected_withLabels_alternative_colors.pdf",fig.dir), width= fig.width/inch.to.mm, height=fig.height/inch.to.mm, pointsize=point.size)
      Fig3.alt.colors.with.labels
      dev.off()



### this is to calculate which ones meet the target

 table(aggregate((d$grp %in% "Marine" & d$protected >10 | !(d$grp %in% "Marine") & d$protected >17),list(d$Code),sum)$x)

table(aggregate((d$grp %in% "Marine" & d$protected >=9 | !(d$grp %in% "Marine") & d$protected >=16),list(d$Code),sum)$x)


## review biome data
## T6 has large inconsistency in mapped distribution and impact indicators
## make a note on figure3
biome.dts$clase <- "unknown"
biome.dts$clase[with(biome.dts,(HFP %in% 1) | (MCHI %in% 1))] <- "degraded"
biome.dts$clase[with(biome.dts,((HFP %in% 0) | (MCHI %in% 0)) & (WDPAall %in% "*"))] <- "wild unprotected"
biome.dts$clase[with(biome.dts,(!clase %in% "degraded") & (WDPAall %in% 1))] <- "wild protected"
biome.dts$clase[with(biome.dts,(clase %in% "degraded") & (WDPAall %in% 1))] <- "degraded protected"

biome.dts[(gsub("[0-9_]","",biome.dts$EFG) %in% c("T","TF")) & biome.dts$MCHI %in% c("0","1") & biome.dts$HFP %in% c("*"),"clase"] <- "inconsistent"
biome.dts[(gsub("[0-9_]","",biome.dts$EFG) %in% c("M")) & biome.dts$HFP %in% c("0","1") & biome.dts$MCHI %in% c("*") ,"clase"] <- "inconsistent"

aggregate(biome.dts$area,list(clase=biome.dts$clase),sum)
aggregate(biome.dts$area,list(marine=biome.dts$MCHI,terrestrial=biome.dts$HFP,clase=biome.dts$clase),sum)

## first test to see if we should exclude some EFGs
d1 <- with(biome.dts, tapply(area,list(EFG,clase),sum))
## inconsistencies smaller than 5% for most, except T6. Make note in figure legend
max(round(d1[,"inconsistent"]/ rowSums(d1,na.rm=T),2),na.rm=T)
max(round(d1[,"unknown"]/ rowSums(d1,na.rm=T),2),na.rm=T)
table(round(d1[,"unknown"]/ rowSums(d1,na.rm=T),2)>.05)

## now, set up data for biome plots


rsm <- with(subset(biome.dts, !clase %in%  c("inconsistent","unknown") ), aggregate(data.frame(area=area),list(biome=gsub("T6","T6*",EFG),clase=factor(clase,levels=c("degraded","degraded protected","wild protected","wild unprotected"))),sum))
rsm$area.fill <- rsm$area * ifelse(rsm$clase %in% c("wild unprotected","wild protected"),1,-1)



## Biome plot


  out.of.plot.deg <- subset(rsm,area > 8e7 & clase %in% "degraded")
  out.of.plot.wld <- subset(rsm,area > 8e7 & clase %in% "wild unprotected")

BiomeTotals <- ggplot() +
  geom_bar(data=subset(rsm,area.fill<0), mapping=aes(x=biome, y = area*-1, fill=clase),stat = 'identity') +
  geom_bar(data=subset(rsm,area.fill>0), mapping=aes(x=biome, y = area, fill=factor(clase,levels=c("wild unprotected","wild protected"))),stat = 'identity') +
  scale_x_discrete(limits = rev(unique(rsm$biome))) +
  coord_flip(ylim= c(-8e7,8e7)) +
  theme_classic() +
  scale_fill_manual(values=clrs[c(11,12,4,3)]) +
  labs( y = bquote("Million"~km^2),x = "",fill = "") +
  annotate("text", y=-8e7, x=c(12,11),
    label=sprintf("%0.0f",out.of.plot.deg$area/1e6), vjust=+0.5, color="black", size=1.9) +
    annotate("text", y=8e7, x=c(12,11),
      label=sprintf("%0.0f",out.of.plot.wld$area/1e6), vjust=+0.5, color="black", size=1.9) +
    scale_y_continuous(breaks=seq(-6e7, 6e7,by=2e7),label=abs(seq(-60, 60,by=20))) +
  guides(fill=guide_legend(keyheight=.5,keywidth=.5)) +
  theme(legend.position="top",legend.justification=c(-.1,0), axis.text=element_text(size=7),legend.title = element_text(size = 6),
    legend.text = element_text(size = 6)) +
  geom_segment(data.frame(x1=c(-70e6), x2=c(-67e6), y1=c(10),y2=c(13)), mapping=aes(y=x1, yend=x2, x=y1, xend=y2),show.legend=F,colour="white",size=2) +
geom_segment(data.frame(x1=c(70e6), x2=c(67e6), y1=c(10),y2=c(13)), mapping=aes(y=x1, yend=x2, x=y1, xend=y2),show.legend=F,colour="white",size=2)

## Composite figure for manuscript

## 300 dpi
## width: 89 mm (single column) and 183 mm (double column)
## fullpage = 247 mm. use 1/4 to 1/3 of page
fig.res = 300
inch.to.mm = 25.4
fig.width = 183

fig.height = 247/3
fig.width.one = 89
fig.height.one = 247*2/5
point.size = 7


Scatters <-  ggarrange(plotT, plotF, legEnd, plotM,
                             labels = c("b", "c",NA ,"d"),
                             ncol = 2, nrow = 2)
Fig3 <- ggarrange(BiomeTotals,Scatters,
                           labels = c("a"),
                           ncol = 2, nrow = 1)

pdf(sprintf("%s/manuscript/Figure3_Biomes_EFG_2columns_4panels_year2013_WDPAall.pdf",fig.dir),width= fig.width/inch.to.mm,height=fig.height/inch.to.mm,pointsize=point.size)
Fig3
dev.off()

## 300 dpi
## width: 89 mm (single column) and 183 mm (double column)
## fullpage = 247 mm. use 1/4 to 1/3 of page
inch.to.mm = 25.4
fig.width.one = 89
fig.height.one = 247*2/3
point.size = 7

Scatters <-  ggarrange(legEnd, plotT, plotF, plotM,
                             labels = c(NA,"b", "c","d"),
                             ncol = 2, nrow = 2)
Fig3b <- ggarrange(BiomeTotals,Scatters,
                           labels = c("a"),
                           ncol = 1, nrow = 2)

pdf(sprintf("%s/manuscript/Figure3_Biomes_EFG_1columns_4panels_year2013_WDPAall.pdf",fig.dir),width= fig.width.one/inch.to.mm,height=fig.height.one/inch.to.mm,pointsize=point.size)
Fig3b
dev.off()


## 300 dpi
## width: 89 mm (single column) and 183 mm (double column)
## fullpage = 247 mm. use 1/4 to 1/3 of page
inch.to.mm = 25.4
fig.width = 183*4/5
fig.height = 247*4/5
point.size=12
Scatters <-  ggarrange(plotT+geom_text_repel(aes(label = Code),colour=1,size=3),
  plotF+geom_text_repel(aes(label = Code),colour=1,size=3),
  plotM+geom_text_repel(aes(label = Code),colour=1,size=3),
  ##legEnd,
  labels = c("a", "b","c"),
  ncol = 1, nrow = 3)

pdf(sprintf("%s/supplement/Figure3_EFG_3panels_year2013_WDPAall_withLabels.pdf",fig.dir), width= fig.width/inch.to.mm, height=fig.height/inch.to.mm, pointsize=point.size)
Scatters
dev.off()
