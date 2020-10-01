##R --vanilla
require(Hmisc)
require(RColorBrewer)
require(viridis)
require(dplyr)
require(tidyr)
require(ggpubr)
require(ggrepel)

work.dir <- Sys.getenv("WORKDIR")
script.dir <- Sys.getenv("SCRIPTDIR")
setwd(work.dir)

clrs <- ##brewer.pal(8,"Accent")
 brewer.pal(12,"Paired")
 clr2 <- ##brewer.pal(8,"Accent")
  viridis(6)

load(file=sprintf("%s/Rdata/Protected-change-all-versions.rda", script.dir))


df1 <- subset(EFG.prot,version %in% "version-2.0.1b" & !grepl("M[0-9]",EFG))

df2 <- subset(EFG.prot,version %in% "version-2.0.1b" & grepl("M[0-9]",EFG))


plotT <- ggplot(df1, aes(x=pre.2000, y=cur.2020)) + scale_shape_manual( values=c(1,16)) + geom_point(colour=clr2[5]) + labs( x = "% Protected in 2010", y = "% Currently protected")  + theme_classic() + scale_x_sqrt(breaks=c(.01,.05,.10,.17,.25,.5,.75,1.00),label=c(1,5,10,17,25,50,75,100)) + scale_y_sqrt(breaks=c(.01,.05,.10,.17,.25,.5,.75,1.00),label=c(1,5,10,17,25,50,75,100)) + geom_abline(color="grey77",lty=2,lwd=1) + geom_hline(yintercept=.17,color="black",lty=3,lwd=.5) + geom_vline(xintercept=.17,color="black",lty=3,lwd=.5) + geom_text_repel(aes(label = EFG),data=subset(df1,cur.2020>.17),colour=1,size=3)



plotM <- ggplot(df2, aes(x=pre.2000, y=cur.2020)) + scale_shape_manual( values=c(1,16)) + geom_point(colour=clr2[1]) + labs( x = "% Protected in 2010", y = "% Currently protected")  + theme_classic() + scale_x_sqrt(breaks=c(.01,.05,.10,.17,.25,.75,1.00),label=c(1,5,10,17,25,75,100)) + scale_y_sqrt(breaks=c(.01,.05,.10,.17,.25,.75,1.00),label=c(1,5,10,17,25,75,100)) + geom_abline(color="grey77",lty=2,lwd=1) + geom_hline(yintercept=.10,color="black",lty=3,lwd=.5) + geom_vline(xintercept=.10,color="black",lty=3,lwd=.5) + geom_text_repel(aes(label = EFG),data=subset(df2,cur.2020>.10),colour=1,size=3)



Fig5 <-  ggarrange( plotT, plotM,
                             labels = c("a", "b"),
                             ncol = 2, nrow = 1)

## 300 dpi
## width: 89 mm (single column) and 183 mm (double column)
## fullpage = 247 mm. use 1/4 to 1/3 of page
fig.res = 300
inch.to.mm = 25.4
fig.width = 183
fig.height = 247/2

point.size = 7

pdf(sprintf("%s/output/figures/FigureS6-5_EFG_2column_2panels_change_protected.pdf",work.dir),width= fig.width/inch.to.mm,height=fig.height/inch.to.mm,pointsize=point.size)
Fig5
dev.off()
