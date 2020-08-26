##R --vanilla
require(dplyr)
require(tidyr)
require(ggplot2)
require(ggrepel)
require(ggpubr)
require(RColorBrewer)
require(viridis)
library(cowplot)


work.dir <- Sys.getenv("WORKDIR")
fig.dir <- sprintf("%s/output/",Sys.getenv("SCRIPTDIR"))
Rdata.dir <- sprintf("%s/Rdata/",Sys.getenv("SCRIPTDIR"))
shiny.dir <- sprintf("%s/apps/shiny/",Sys.getenv("SCRIPTDIR"))
setwd(work.dir)


load(sprintf("%s/Degraded-protected-2013-all-versions.rda", Rdata.dir))


## code for `degraded vs protected` plots

EFG.basic.plot <- function(dataset,y,v="version-2.0.0",g="Terrestrial",h=17) {
  x <- dataset %>% filter(group %in% g & version %in% v)
  ggplot(x, aes(degraded, protected, colour = biome.lab, shape=biome.lab)) +
    geom_point( size = 2) +
    scale_colour_manual(unique(x$biome.lab),
      values=y$col[unique(match(x$biome.lab,y$lab))]) +
    scale_shape_manual(unique(x$biome.lab),
      values=y$pch[unique(match(x$biome.lab,y$lab))]) +
      geom_hline(yintercept = h, color="black",lty=3,lwd=.5) +
      geom_vline(xintercept=70,color="black",lty=3,lwd=.5) + coord_cartesian(xlim=c(0,100),ylim=c(0,30))
}

beautify.plot <- function(x) {
x +  labs( x = "% exposed to high pressures", y = "% protected",colour = "Biomes",shape = "Biomes") +
  theme_classic() +
  theme(legend.position = c(.05, .95), legend.justification = c("left", "top"), legend.box.just = "right",  legend.direction = "horizontal", legend.box.background = element_rect(), legend.margin = margin(0, 0, 0, 0),  legend.title=element_blank(), legend.text = element_text(size=6,angle=0,colour ="black"), axis.title = element_text(size = 8), axis.text = element_text(size = 7), panel.border=element_rect(colour="black",fill=NA,size=1))

}

plotT.2 <- EFG.basic.plot(EFG.data,d.legend,v="version-2.0.0",g="Terrestrial") %>% beautify.plot()
plotF.2 <- EFG.basic.plot(EFG.data,d.legend,v="version-2.0.0",g="Freshwater") %>% beautify.plot()
plotR.2 <- EFG.basic.plot(EFG.data,d.legend,v="version-2.0.0",g="Transitional") %>% beautify.plot()
plotM.2 <- EFG.basic.plot(EFG.data,d.legend,v="version-2.0.0",g="Marine",h=10) %>% beautify.plot()

plotT.1 <- EFG.basic.plot(EFG.data,d.legend,v="version-1.1.0",g="Terrestrial") %>% beautify.plot()
plotF.1 <- EFG.basic.plot(EFG.data,d.legend,v="version-1.1.0",g="Freshwater") %>% beautify.plot()
plotR.1 <- EFG.basic.plot(EFG.data,d.legend,v="version-1.1.0",g="Transitional") %>% beautify.plot()
plotM.1 <- EFG.basic.plot(EFG.data,d.legend,v="version-1.1.0",g="Marine",h=10) %>% beautify.plot()

ggarrange(plotT.1 + geom_text_repel(aes(label = EFG),colour=1,size=3) + labs( title = "version 1.1"),
  plotT.2+ geom_text_repel(aes(label = EFG),colour=1,size=3) + labs( title = "version 2.0"),
  common.legend = TRUE, legend="bottom")

ggsave(file='DegradedProtectedPlot-Terrestrial-both-versions.pdf',device=pdf)

ggarrange(plotF.1 + geom_text_repel(aes(label = EFG),colour=1,size=3) + labs( title = "version 1.1"),
  plotF.2+ geom_text_repel(aes(label = EFG),colour=1,size=3) + labs( title = "version 2.0"),
  common.legend = TRUE, legend="bottom")

ggsave(file='DegradedProtectedPlot-Freshwater-both-versions.pdf',device=pdf)

ggarrange(plotR.1 + geom_text_repel(aes(label = EFG),colour=1,size=3) + labs( title = "version 1.1"),
  plotR.2+ geom_text_repel(aes(label = EFG),colour=1,size=3) + labs( title = "version 2.0"),
  common.legend = TRUE, legend="bottom")

ggsave(file='DegradedProtectedPlot-Transitional-both-versions.pdf',device=pdf)

ggarrange(plotM.1 + geom_text_repel(aes(label = EFG),colour=1,size=3) + labs( title = "version 1.1"),
  plotM.2+ geom_text_repel(aes(label = EFG),colour=1,size=3) + labs( title = "version 2.0"),
  common.legend = TRUE, legend="bottom")

ggsave(file='DegradedProtectedPlot-Marine-both-versions.pdf',device=pdf)



mi.legend <- d.legend
mi.legend$cc  <- rep(1,16)
mi.legend$rr <-  16:1
#data.frame(name=biome.labels, cc=rep(1,16), rr=16:1, pch=c(17,17,17,15,15,15,0,0,16,16,16,16,16,16,17,0),col=as.character(clr2[c(1,3,5, 6,1,3,1,4, 1:6,6,4)]),stringsAsFactors=F)

legEnd <- ggplot(mi.legend,aes(x=cc,y=rr,color=lab,shape=lab)) +
  theme_minimal() +
  scale_shape_manual(values=mi.legend$pch,labels=mi.legend$lab)+
  scale_color_manual(values=mi.legend$col,labels=mi.legend$lab)+
  geom_point( size = 2) +
  geom_text(aes(label = lab), colour=1, size=2, nudge_x=.2, hjust="left") +
  labs( x = "", y = "") +
  theme(legend.position = "right", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks=NULL,label=NULL) +
  scale_x_continuous(breaks=NULL,label=NULL) +
  coord_cartesian(xlim=c(1,5),ylim=c(0,16))


    the.legend <- get_legend(
      # create some space to the left of the legend
      legEnd + theme(legend.box.margin = margin(0, 2, 0, 12)) + labs( colour = "Biomes",shape = "Biomes")
    )


  prow <- plot_grid(plotT , plotF , plotR , plotM , align = 'vh', labels = c("A", "B", "C","D"), hjust = -1, nrow = 2)

    prow <- plot_grid(
      plotT.2 + theme(legend.position="none"),
      plotF.2 + theme(legend.position="none"),
      plotR.2 + theme(legend.position="none"),
      plotM.2 + theme(legend.position="none"),
      align = 'vh',
      labels = c("A", "B", "C","D"),
      hjust = -1,
      nrow = 2
    )

plot_grid(prow, the.legend, rel_widths = c(7, 3))
ggsave(file='DegradedProtectedPlot-version-2.pdf',device=pdf)



prow
ggsave(file='DegradedProtectedPlot.pdf',device=pdf)


  #prow


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
