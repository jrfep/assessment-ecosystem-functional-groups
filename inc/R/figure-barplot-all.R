##R --vanilla
require(Hmisc)
require(RColorBrewer)
require(viridis)
require(dplyr)
require(tidyr)
require(ggpubr)

work.dir <- Sys.getenv("WORKDIR")
script.dir <- Sys.getenv("SCRIPTDIR")
setwd(work.dir)



load(sprintf("%s/Rdata/Degraded-protected-2013-all-versions.rda", script.dir))


clrs <- brewer.pal(12,"Paired")

 slc <- unique(maps.x.indicators$EFG)
 ## exclude the anthropogenic
 slc <- slc[!(grepl("^F3.?",slc) | grepl("^T7.?",slc) | grepl("^M4.?",slc) | grepl("^MT3.?",slc) | grepl("^S2.?",slc) | grepl("^SF2.?",slc))]
 ## Ice and snow groups in the southern hemisphere are not well covered by human impact variables
 slc <- slc[!slc %in% c("T6.1","T6.2","M2.5","F2.10")]
 ## Subterranean EFG are not well covered by the protection/degradation variables
 slc <- slc[!grepl("^S",slc)]


 EFG.dts <- maps.x.indicators %>% filter(EFG %in% slc) %>% mutate(clase=ifelse((HFP %in% "*") & (MCHI %in% "*"),"unknown",paste(ifelse((HFP %in% 1) | (MCHI %in% 1),"degraded","wild"), ifelse((WDPA %in% "1"),"protected","unprotected"))))

 EFG.dts[(gsub("[0-9_]","",EFG.dts$EFG) %in% c("T","TF")) & EFG.dts$MCHI %in% c("0","1") & EFG.dts$HFP %in% c("*"),"clase"] <- "inconsistent"
 EFG.dts[(gsub("[0-9_]","",EFG.dts$EFG) %in% c("M")) & EFG.dts$HFP %in% c("0","1") & EFG.dts$MCHI %in% c("*") ,"clase"] <- "inconsistent"

 EFG.dts %>% select(clase) %>% table
EFG.dts$Name <- EFG.names[pmatch(sprintf("%s ",EFG.dts$EFG),EFG.names,duplicates.ok=T)]
EFG.dts$Name <- gsub("shrublands and grasslands","shrubs/grass",EFG.dts$Name)
EFG.dts$Name <- gsub("and bays","",EFG.dts$Name)



s1 <- with(EFG.dts,version %in% "version-2.0.1b" & clase %in% c("wild protected","wild unprotected"))
s2 <- with(EFG.dts,version %in% "version-2.0.1b" & clase %in% c("degraded protected","degraded unprotected"))
plotAll <- ggplot(subset(EFG.dts, (s1)), aes(x=Name, y = area, fill=factor(WDPA,label=c("low pressure unprotected","low pressure protected")))) + scale_x_discrete(limits = rev(unique(EFG.dts$Name[s1]))) +
   coord_flip(ylim= c(-3e7,3e7)) +
   ##geom_text(data=subset(dp.ter,area > 3e7 & clase %in% "degraded"),
   ##aes(label=sprintf("%1ef",area),y=Name,x=-3e7), vjust=1.6, color="white", size=3.5)+
geom_bar(stat = 'identity') + ## theme(legend.position=c(1e07,10)) +
geom_bar(data=subset(EFG.dts,s2 ), mapping=aes(x=Name, y = -1*area, fill=factor(WDPA,label=c("high pressure","high pressure protected"))),stat = 'identity') +  scale_fill_manual(values=clrs[c(11,12,4,3)])+ labs( y = bquote("Million"~km^2), x = "",fill = "")

sumDeg <- with(subset(EFG.dts,version %in% "version-2.0.1b" & clase %in% c("degraded unprotected","degraded protected")),
aggregate(area,list(Name),sum))
out.of.plot <- subset(sumDeg,x > 3.0e7)
sumWld <- with(subset(EFG.dts,version %in% "version-2.0.1b" & clase %in% c("wild unprotected","wild protected")),
aggregate(area,list(Name),sum))
out.of.plot2 <- subset(sumWld,x > 3.0e7)

pdf(sprintf("%s/output/figures/Figure_S5_1_BarPlots.pdf",work.dir), width=7,height=9)

plotAll+theme_classic()+
scale_y_continuous(breaks=seq(-3e7, 3e7,by=5e6),label=abs(seq(-30, 30,by=5)))+
annotate("text", y=-2.995e7, x=82-as.numeric(rownames(out.of.plot)),
  label=sprintf("%0.1f",out.of.plot$x/1e6), vjust=+0.5, color="black", size=1.9)+
  annotate("text", y=2.995e7, x=82-as.numeric(rownames(out.of.plot2)),
    label=sprintf("%0.1f",out.of.plot2$x/1e6), vjust=+0.5, color="black", size=1.9)+
guides(fill=guide_legend(keyheight=.5,keywidth=.5)) +
theme(legend.position="top",legend.justification=c(-.1,0), axis.text=element_text(size=7),legend.title = element_text(size = 6),
  legend.text = element_text(size = 6))

dev.off()
