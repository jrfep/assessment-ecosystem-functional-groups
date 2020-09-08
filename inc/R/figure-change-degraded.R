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

#tbl <- read_excel(sprintf("%s/input/Table_Degraded_Protected_EFG.xlsx",script.dir))
#EFG.names <- gsub("1Trop","1 Trop",gsub("  "," ",gsub(" 1","1",tbl$nam...3)))
#names(EFG.names) <- sapply(EFG.names,function(x) strsplit(x," ")[[1]][1])
#EFG.names <- subset(EFG.names,!is.na(EFG.names))

clrs <- ##brewer.pal(8,"Accent")
 brewer.pal(12,"Paired")
 clr2 <- ##brewer.pal(8,"Accent")
  viridis(6)

load(file=sprintf("%s/Rdata/Degraded-change-all-versions.rda", script.dir))
# EFG.difs.t <-  subset(EFG.difs.t,!EFG %in% "T6_1")
N <- 1000
mu0 <- 0

## may need to do this with bootstrap rather than one single sample, otherwise results might change each time we run this:
set.seed(2125387)


df <- data.frame()
for (ii in c("Terrestrial","Marine")) {
  for (vv in c("version-1.1.0","version-2.0.1b")) {
    for (ee in unique(subset(EFG.difs,indicator == ii & version %in% vv)$EFG)) {
       dts <- subset(EFG.difs,indicator == ii & version %in% vv & EFG %in% ee)
       ##N <- sum(dts[,"area"]
       ss <- sample(dts$Diff,prob=dts$p.area,size=N,replace=T)
       ##mu <- wtd.mean(dts[,"HFPdiff"], weights=dts[,"area"])
       mu <-  mean(ss)
       ##sg <- sqrt(wtd.var(dts[,"HFPdiff"], weights=dts[,"area"]))
       sg <- sd(ss)
       df <- rbind(df,
          data.frame(EFG=gsub("_",".",ee),version=vv,indicator=ii,
          mu,
          sd=sg,
          N)
       )
     }
   }
}
##https://en.wikipedia.org/wiki/Student%27s_t-test
df$t <- (df$mu-mu0)/(df$sd/sqrt(df$N))
df$p <- pt(df$t,df$N-1)
df$se <- df$sd/sqrt(df$N)
df$mu.min <- df$mu-(1.96*df$se)
df$mu.max <- df$mu+(1.96*df$se)
df <- df[order(df$mu),]
df$x <- seq(along=df$EFG)
df$test <- df$p<0.025 | df$p>0.975
df$test <- df$p>.95


df$Name <- EFG.names[as.character(df$EFG)]


 oo <- with(df,aggregate(mu,list(EFG),mean))
oo <- oo[order(oo$x),]
df$EFG <- factor(df$EFG,levels=oo$Group.1)

## this is for David
## write.csv(file="Table_Change_Impact_EFG_Transitional.csv", df)

df1 <- subset(df,version %in% "version-2.0.1b" & indicator %in% "Terrestrial")
plotT <- ggplot(df1, aes(x=x, y=mu, shape=test)) +
 scale_shape_manual( values=c(1,16)) +
geom_errorbar(aes(ymin=mu.min, ymax=mu.max), width=.1, colour=clr2[5]) +
geom_point(colour=clr2[5]) + labs( x = "", y = "change in pressure index")  + theme_classic() + coord_flip(xlim=c(1,50)) + geom_hline(yintercept=0,color="black",lty=3,lwd=.5) +
 scale_x_continuous(breaks=1:nrow(df1),label=df1$Name) + theme(legend.position = "none", legend.text = element_text(size=5,angle=0,colour ="black"), axis.title = element_text(size = 8), axis.text = element_text(size = 7), panel.border=element_rect(colour="black",fill=NA,size=1))
 ## + annotate("text", y=c(-.1,+.2)[2], x=nrow(df),    label=c("less pressure","more pressure")[2], vjust=+0.5, color="black", size=3)
## this is for David
##write.csv(file="Table_Change_Impact_EFG_T.csv",df)


 plotM <- ggplot(df2, aes(x=x, y=mu,  shape=test)) +
  scale_shape_manual( values=c(1,16)) +
 geom_errorbar(aes(ymin=mu.min, ymax=mu.max), width=.1, colour=clr2[2]) + labs( x = "", y = "change in pressure index")  + theme_classic() +
 geom_point(colour=clr2[2]) + coord_flip() + geom_hline(yintercept=0,color="black",lty=3,lwd=.5) +
  scale_x_continuous(breaks=1:nrow(df2),label=df2$Name) + theme(legend.position = "none", legend.text = element_text(size=5,angle=0,colour ="black"), axis.title = element_text(size = 8), axis.text = element_text(size = 7), panel.border=element_rect(colour="black",fill=NA,size=1))
#  + annotate("text", y=c(-.1,+.1), x=nrow(df),     label=c("less pressure","more pressure"), vjust=+0.5, color="black", size=3)

## this is for David
##write.csv(file="Table_Change_Impact_EFG_M.csv",df)

pd <- position_dodge(0.75) # move them .05 to the left and right

plotNT <- ggplot(df3, aes(x=Name, y=mu,colour=ind,  shape=test)) +
 scale_shape_manual( values=c(1,16)) +
 geom_errorbar(aes(ymin=mu.min, ymax=mu.max), width=.1, position=pd) +
 geom_point(position=pd) + labs( x = "", y = "change in pressure index",colour="pressure index")  + theme_classic() + coord_flip(xlim=c(0,11)) + geom_hline(yintercept=0,color="black",lty=3,lwd=.5) + theme(legend.position = "none", legend.text = element_text(size=8,angle=0,colour ="black"), axis.title = element_text(size = 8), axis.text = element_text(size = 7), panel.border=element_rect(colour="black",fill=NA,size=1)) +
 scale_color_manual(values=clr2[c(5,2)]) +
  annotate("text", y=c(-.35,+.35), x=c(0,0),
    label=c("less pressure","more pressure"), vjust=+0.5, color="black", size=2)
##  scale_x_continuous(breaks=1:nrow(df),label=df$EFG)


  #


rightside <-  ggarrange( plotM, plotNT,
                             labels = c("b", "c"),
                             ncol = 1, nrow = 2)
Fig4 <- ggarrange(plotT,rightside,
                           labels = c("a"),
                           ncol = 2, nrow = 1)

## 300 dpi
## width: 89 mm (single column) and 183 mm (double column)
## fullpage = 247 mm. use 1/4 to 1/3 of page
fig.res = 300
inch.to.mm = 25.4
fig.width = 183
fig.height = 247/2

point.size = 7

pdf(sprintf("%s/manuscript/Figure4_EFG_2column_3panels_change_impact_indices_corrected.pdf",fig.dir),width= fig.width/inch.to.mm,height=fig.height/inch.to.mm,pointsize=point.size)
Fig4
dev.off()
