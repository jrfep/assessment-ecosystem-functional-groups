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
Rdata.dir <- sprintf("%s/Rdata/",Sys.getenv("SCRIPTDIR"))
shiny.dir <- sprintf("%s/apps/shiny/",Sys.getenv("SCRIPTDIR"))
setwd(work.dir)

# use only WDPA data up to 2013
##load(sprintf("%s/Degraded-protected-2013-all-versions.rda", Rdata.dir))
load(sprintf("%s/Degraded-protected-all-all-versions.rda", Rdata.dir))


## code for `degraded vs protected` plots

EFG.basic.plot <- function(dataset,y,v="version-2.0.1b",g="Terrestrial",h=17) {
  x <- dataset %>% filter(group %in% g & version %in% v)
  ggplot(x, aes(degraded, protected, colour = biome.lab, shape=biome.lab)) +
    geom_point( size = 2) +
    scale_colour_manual(unique(x$biome.lab),
      values=y$col[unique(match(x$biome.lab,y$lab))]) +
    scale_shape_manual(unique(x$biome.lab),
      values=y$pch[unique(match(x$biome.lab,y$lab))]) +
      geom_hline(yintercept = h, color="black",lty=3,lwd=.5) +
      geom_vline(xintercept=70,color="black",lty=3,lwd=.5) + coord_cartesian(xlim=c(0,100),ylim=c(0,100))
}

beautify.plot <- function(x) {
x +  labs( x = "% exposed to high pressures", y = "% protected",colour = "Biomes",shape = "Biomes") +
  theme_classic() +
  theme(legend.position = c(.05, .95), legend.justification = c("left", "top"), legend.box.just = "right",  legend.direction = "horizontal", legend.box.background = element_rect(), legend.margin = margin(0, 0, 0, 0),  legend.title=element_blank(), legend.text = element_text(size=6,angle=0,colour ="black"), axis.title = element_text(size = 8), axis.text = element_text(size = 7), panel.border=element_rect(colour="black",fill=NA,size=1))

}

plotT.2 <- EFG.basic.plot(EFG.data,d.legend,v="version-2.0.1b",g="Terrestrial") %>% beautify.plot()
plotF.2 <- EFG.basic.plot(EFG.data,d.legend,v="version-2.0.1b",g="Freshwater") %>% beautify.plot()
plotR.2 <- EFG.basic.plot(EFG.data,d.legend,v="version-2.0.1b",g="Transitional") %>% beautify.plot()
plotM.2 <- EFG.basic.plot(EFG.data,d.legend,v="version-2.0.1b",g="Marine",h=10) %>% beautify.plot()

plotT.1 <- EFG.basic.plot(EFG.data,d.legend,v="version-1.1.0",g="Terrestrial") %>% beautify.plot()
plotF.1 <- EFG.basic.plot(EFG.data,d.legend,v="version-1.1.0",g="Freshwater") %>% beautify.plot()
plotR.1 <- EFG.basic.plot(EFG.data,d.legend,v="version-1.1.0",g="Transitional") %>% beautify.plot()
plotM.1 <- EFG.basic.plot(EFG.data,d.legend,v="version-1.1.0",g="Marine",h=10) %>% beautify.plot()


# compare plots for each group with different versions of the maps

ggarrange(plotT.1 + geom_text_repel(aes(label = EFG),colour=1,size=3) + labs( title = "version 1.1"),
  plotT.2+ geom_text_repel(aes(label = EFG),colour=1,size=3) + labs( title = "version 2.0"),
  common.legend = TRUE, legend="bottom")
ggsave(file=sprintf('%s/output/figures/DegradedProtectedPlot-Terrestrial-both-versions.pdf',work.dir),device=pdf)

ggarrange(plotF.1 + geom_text_repel(aes(label = EFG),colour=1,size=3) + labs( title = "version 1.1"),
  plotF.2+ geom_text_repel(aes(label = EFG),colour=1,size=3) + labs( title = "version 2.0"),
  common.legend = TRUE, legend="bottom")
ggsave(file=sprintf('%s/output/figures/DegradedProtectedPlot-Freshwater-both-versions.pdf',work.dir),device=pdf)

ggarrange(plotR.1 + geom_text_repel(aes(label = EFG),colour=1,size=3) + labs( title = "version 1.1"),
  plotR.2+ geom_text_repel(aes(label = EFG),colour=1,size=3) + labs( title = "version 2.0"),
  common.legend = TRUE, legend="bottom")
ggsave(file=sprintf('%s/output/figures/DegradedProtectedPlot-Transitional-both-versions.pdf',work.dir),device=pdf)

ggarrange(plotM.1 + geom_text_repel(aes(label = EFG),colour=1,size=3) + labs( title = "version 1.1"),
  plotM.2+ geom_text_repel(aes(label = EFG),colour=1,size=3) + labs( title = "version 2.0"),
  common.legend = TRUE, legend="bottom")
ggsave(file=sprintf('%s/output/figures/DegradedProtectedPlot-Marine-both-versions.pdf',work.dir),device=pdf)

fig.res = 300
inch.to.mm = 25.4
fig.width = 150
fig.height = 150
point.size = 12

pdf(sprintf('%s/output/figures/DegradedProtectedPlot-Freshwater.pdf',work.dir), width= fig.width/inch.to.mm, height=fig.height/inch.to.mm, pointsize=point.size)
plotF.2+ geom_text_repel(aes(label = EFG),colour=1,size=3) + theme(legend.position = "top",legend.text = element_text(size=10,angle=0,colour ="black"), axis.title = element_text(size = 12), axis.text = element_text(size = 10))
dev.off()


pdf(sprintf('%s/output/figures/DegradedProtectedPlot-Terrestrial.pdf',work.dir), width= fig.width/inch.to.mm, height=fig.height/inch.to.mm, pointsize=point.size)
plotT.2+ geom_text_repel(aes(label = EFG),colour=1,size=3) + theme(legend.position = "top", legend.text = element_text(size=10,angle=0,colour ="black"), axis.title = element_text(size = 12), axis.text = element_text(size = 10)) + guides(colour=guide_legend(ncol=2))
dev.off()


pdf(sprintf('%s/output/figures/DegradedProtectedPlot-Transitional.pdf',work.dir), width= fig.width/inch.to.mm, height=fig.height/inch.to.mm, pointsize=point.size)
plotR.2+ geom_text_repel(aes(label = EFG),colour=1,size=3) + theme(legend.position = "top", legend.text = element_text(size=10,angle=0,colour ="black"), axis.title = element_text(size = 12), axis.text = element_text(size = 10)) + guides(colour=guide_legend(ncol=3))
dev.off()

pdf(sprintf('%s/output/figures/DegradedProtectedPlot-Marine.pdf',work.dir), width= fig.width/inch.to.mm, height=fig.height/inch.to.mm, pointsize=point.size)
plotM.2+ geom_text_repel(aes(label = EFG),colour=1,size=3) + theme(legend.position = "top",legend.text = element_text(size=10,angle=0,colour ="black"), axis.title = element_text(size = 12), axis.text = element_text(size = 10)) + guides(colour=guide_legend(ncol=2))
dev.off()


brksx <- lblsx <- seq(0,100,length=5)
brksy <- lblsy <- seq(0,100,length=5)
lblsy[length(lblsy)] <- ''

brksmy <- lblsmy <- seq(0,40,length=5)
lblsmy[length(lblsmy)] <- ''

prow <- plot_grid(
  plotT.2 + theme(legend.position="none") + scale_x_continuous( breaks=brksx,labels=lblsx) + scale_y_continuous( breaks=brksy,labels=lblsy)+ coord_cartesian(xlim=c(0,90),ylim=c(0,90)) + geom_text_repel(aes(label = EFG),colour=1,size=3) ,##scale_y_sqrt( breaks=brks,labels=lbls) + scale_x_sqrt( breaks=brks,labels=lblsx),
  plotF.2 + theme(legend.position="none") + scale_x_continuous( breaks=brksx,labels=lblsx) + scale_y_continuous( breaks=brksy,labels=lblsy)+ coord_cartesian(xlim=c(0,90),ylim=c(0,90)) + geom_text_repel(aes(label = EFG),colour=1,size=3),
  plotM.2 + theme(legend.position="none") + scale_x_continuous( breaks=brksx,labels=lblsx) + scale_y_continuous( breaks=brksmy,labels=lblsmy)+ coord_cartesian(xlim=c(0,90),ylim=c(0,40)) + geom_text_repel(aes(label = EFG),colour=1,size=3),
  plotR.2 + theme(legend.position="none") + scale_x_continuous( breaks=brksx,labels=lblsx) + scale_y_continuous( breaks=brksmy,labels=lblsmy)+ coord_cartesian(xlim=c(0,90),ylim=c(0,40)) + geom_text_repel(aes(label = EFG),colour=1,size=3),
  align = 'vh',
  labels = c("A", "B", "C","D"),
  hjust = -1,
  nrow = 2
)
# compose figure with subplots for each group


fig.res = 300
inch.to.mm = 25.4
fig.width = 180
fig.height = 120
point.size = 12

pdf(sprintf('%s/output/figures/DegradedProtectedPlot-Appendix.pdf',work.dir), width= fig.width/inch.to.mm, height=fig.height/inch.to.mm, pointsize=point.size)
prow
dev.off()

#png(sprintf('%s/output/figures/DegradedProtectedPlot-Appendix.png',work.dir), width= 700, height=500, pointsize=point.size)
#prow
#dev.off()



mi.legend <- d.legend
mi.legend$cc  <- rep(1,16)
mi.legend$rr <-  16:1

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


# prow <- plot_grid(plotT , plotF , plotR , plotM , align = 'vh', labels = c("A", "B", "C","D"), hjust = -1, nrow = 2)
brksx <- lblsx <- seq(0,100,length=5)
brksy <- lblsy <- c(0,10,20,30)
lblsy[length(lblsy)] <- ''
##brks <- lbls <- lblsx <- c(1,5,10,20,40,70,100)

prow <- plot_grid(
  plotT.2 + theme(legend.position="none") + scale_x_continuous( breaks=brksx,labels=lblsx) + scale_y_continuous( breaks=brksy,labels=lblsy) + coord_cartesian(xlim=c(10,90),ylim=c(0,30))  ,##scale_y_sqrt( breaks=brks,labels=lbls) + scale_x_sqrt( breaks=brks,labels=lblsx),
  plotF.2 + theme(legend.position="none") + scale_x_continuous( breaks=brksx,labels=lblsx) + scale_y_continuous( breaks=brksy,labels=lblsy)+ coord_cartesian(xlim=c(10,90),ylim=c(0,30)),
  plotR.2 + theme(legend.position="none") + scale_x_continuous( breaks=brksx,labels=lblsx) + scale_y_continuous( breaks=brksy,labels=lblsy)+ coord_cartesian(xlim=c(10,90),ylim=c(0,30)),
  plotM.2 + theme(legend.position="none") + scale_x_continuous( breaks=brksx,labels=lblsx) + scale_y_continuous( breaks=brksy,labels=lblsy)+ coord_cartesian(xlim=c(10,90),ylim=c(0,30)),
  align = 'vh',
  labels = c("A", "B", "C","D"),
  hjust = -1,
  nrow = 2
)

#plot_grid(prow, the.legend, rel_widths = c(7, 3))
# ggsave(file='DegradedProtectedPlot-version-2.pdf',device=pdf)

# for interactive version run:
### shiny::runApp(sprintf('%s/app.R',shiny.dir))

## Composite figure for manuscript
output.fig <- sprintf("%s/output/figures/Figure3_EFG_4panels_year2013_noLabels.pdf",work.dir)
#if (!file.exists(output.fig)) {

  ## 300 dpi
  ## width: 89 mm (single column) and 183 mm (double column)
  ## fullpage = 247 mm. use 1/4 to 1/3 of page
  fig.res = 300
  inch.to.mm = 25.4
  fig.width = 183
  fig.height = 247/3
  point.size = 7

  the.legend <- get_legend(
    legEnd + labs( colour = "Biomes",shape = "Biomes") + theme(legend.key.size = unit(0.4, "cm"))
  )

  pdf(output.fig, width= fig.width/inch.to.mm, height=fig.height/inch.to.mm, pointsize=point.size)
  plot_grid(prow, the.legend, rel_widths = c(70, 30))
  dev.off()

#}

EFG.data %>% filter(version %in% 'version-2.0.1b') %>% select(group,biome.lab,EFG,version,protected,`wild unprotected`,degraded,total) %>% write.csv(file='Summary-table-degraded-protected.csv')
