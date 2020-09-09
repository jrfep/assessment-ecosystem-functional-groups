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
EFG.dts$Name <- EFG.dts$EFG

d1 <- with(subset(EFG.dts,version %in% "version-2.0.1b"), tapply(area,list(EFG,clase),sum))
d1 <- data.frame(d1[,!colnames(d1) %in% c("inconsistent","unknown")])
d1$Name <- rownames(d1)
##d1 <- data.frame(d1*100/rowSums(d1))
##d1$Names <- gsub("_",".",rownames(d1))



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

##98-as.numeric(out.of.plot$Name)
##98-as.numeric(out.of.plot2$Name)
pdf(sprintf("%s/Figure_S5_1_BarPlots.pdf",work.dir), width=7,height=9)

plotAll+theme_classic()+
scale_y_continuous(breaks=seq(-3e7, 3e7,by=5e6),label=abs(seq(-30, 30,by=5)))+
annotate("text", y=-2.995e7, x=82-as.numeric(rownames(out.of.plot)),
  label=sprintf("%0.1f",out.of.plot$x/1e6), vjust=+0.5, color="black", size=1.9)+
  annotate("text", y=2.995e7, x=82-as.numeric(rownames(out.of.plot2)),
    label=sprintf("%0.1f",out.of.plot2$x/1e6), vjust=+0.5, color="black", size=1.9)+
guides(fill=guide_legend(keyheight=.5,keywidth=.5)) +
theme(legend.position="top",legend.justification=c(-.1,0), axis.text=element_text(size=7),legend.title = element_text(size = 6),
  legend.text = element_text(size = 6))

##dev.off()




  read.rstats <- function(arch,map.col=9,idx.col=8,area.col=10) {
  x <- read.table(arch)
    x$map <-  as.numeric(as.character(x[,map.col]))
    x$idx <-  as.numeric(as.character(x[,idx.col]))
    x$area <- x[,area.col]/1e6
    mu <- with(x,wtd.mean(idx,area))
    sg <- sqrt(with(x,wtd.var(idx,area)))
    x$zscore <- (x$idx-mu)/sg
    x$logidx <- log1p(x$idx)
    mu <- with(x,wtd.mean(logidx,area))
    sg <- sqrt(with(x,wtd.var(logidx,area)))
    x$logzscore <- (x$logidx-mu)/sg
    return(x)
  }
ind.bxp <- function(x,wgt,scol="zscore",type=c("quantile","(i-1)/(n-1)","i/(n+1)", "i/n")) {
  bxp1 <- wtd.quantile(x[,scol], weights=with(x,eval(wgt)),
  type=type,probs=c(0.025, .25, .5, .75, .975))
  mn1 <- wtd.mean(x[,scol], weights=with(x,eval(wgt)))
  return(c(bxp1,mn1))
 }

bxp.plot <- function(x,hgt,os,clr=c(1,1),...) {
	segments(x[1],hgt,x[5],hgt,col=clr[2])
	rect(x[2],hgt-os,x[4],hgt+os,col=clr[1],border=clr[2])
	points(x[6],hgt,...,col=clr[2])
}

 require(Hmisc)
T1.1 <- read.rstats(sprintf("ImpactIndicators/Terrestrial/%s","T1_1.txt"))

mit <- "quantile"

plot(NA,xlim=c(-1,3),ylim=c(0,1),main="global vs. T1.1",xlab='zscore',ylab='',axes=F)
axis(1)
box()
bxp.plot(x=ind.bxp(T1.1,wgt=expression(area),"zscore",type=mit),
	hgt=.25,os=.1,clr=c("grey77","black",pch=19))
bxp.plot(x=ind.bxp(subset(T1.1,!is.na(map)),
	wgt=expression(area/map),
	"zscore",type=mit),
	hgt=.65,os=.1,clr=c("green","darkgreen",pch=19))
text(0,.4,"global HFP")
text(0,.85,"HFP for T1.1",col="darkgreen")





archsT <- dir("ImpactIndicators/Terrestrial",full.name=T)
zHFP <- lzHFP <- data.frame()
for (qq in archsT) {
  if (grepl("^F",basename(qq))) {
    	dts <- read.rstats(arch=qq,map.col=4,idx.col=3,area.col=5)
  } else {
    	dts <- read.rstats(arch=qq)
  }


	ss <- subset(dts,!is.na(map))
	if (any(!is.na(ss$zscore))) {
		zHFP <- rbind(zHFP,ind.bxp(ss,expression(area*map),scol="zscore",type=mit))
	} else {
		zHFP <- rbind(zHFP,rep(NA,6))
	}
	if (any(!is.na(ss$zscore))) {
		lzHFP <- rbind(lzHFP,ind.bxp(ss,expression(area*map),scol="logzscore",type=mit))
	} else {
		lzHFP <- rbind(lzHFP,rep(NA,6))
	}

}
zHFP <- t(zHFP)
lzHFP <- t(lzHFP)
colnames(lzHFP) <- colnames(zHFP) <-  gsub(".txt$","",basename(archsT))
rownames(lzHFP) <- rownames(zHFP) <- NULL


archsM <- dir("ImpactIndicators/Marine",full.name=T)
zMCHI <- lzMCHI <- data.frame()
for (qq in archsM) {
    	dts <- read.rstats(arch=qq,map.col=4,idx.col=3,area.col=5)


	ss <- subset(dts,!is.na(map))
	if (any(!is.na(ss$zscore))) {
		zMCHI <- rbind(zMCHI,ind.bxp(ss,expression(area*map),scol="zscore",type=mit))
	} else {
		zMCHI <- rbind(zMCHI,rep(NA,6))
	}
	if (any(!is.na(ss$zscore))) {
		lzMCHI <- rbind(lzMCHI,ind.bxp(ss,expression(area*map),scol="logzscore",type=mit))
	} else {
		lzMCHI <- rbind(lzMCHI,rep(NA,6))
	}

}
zMCHI <- t(zMCHI)
lzMCHI <- t(lzMCHI)
colnames(lzMCHI) <- colnames(zMCHI) <-  gsub(".txt$","",basename(archsM))
rownames(lzMCHI) <- rownames(zMCHI) <- NULL

  pdf("Figure_S5_3_BoxPlots.pdf",width=9,height=7)
layout(matrix(1:2,ncol=2))

qry <- c('T1_1','T1_2','T1_3','T1_4','T2_1','T2_2','T2_3','T2_4','T2_5','T2_6','T3_1','T3_2','T3_3','T3_4','T4_1','T4_2','T4_3','T4_4','T4_5','T5_1','T5_2','T5_3','T5_4','T5_5','T6_1','T6_2','T6_3','T6_4','T6_5')

qry <- gsub(".txt","",grep("T",basename(archsT),value=T))
qry <- typology$code[typology$code %in%  gsub("_",".",gsub(".txt","",basename(archsT)))]
##qry <- grep("^T7",qry,inv=T,val=T)

ys <- rev(seq(.05,length=length(qry),by=.3))
nms <- typology$EFG[match(gsub("_",".",qry),typology$code)]
par(mar=c(4,13,0,0),yaxs="i")
plot(NA,xlim=c(-1.5,2),ylim=c(min(ys)-.15,max(ys)+.15),main=NA,xlab='zscore(log (HFP))',ylab='',axes=F)
abline(v=0,lty=2,col="maroon")
axis(1)
axis(2,ys,nms,las=2,cex.axis=.5)
box()
for (k in 1:length(ys)) {
 bxp.plot(x=lzHFP[,gsub("\\.","_",qry[k])],
 hgt=ys[k],os=.1,clr=c("green","darkgreen"),pch=19)
	}

  qry <- typology$code[typology$code %in%  gsub("_",".",gsub(".txt","",basename(archsM)))]

  ys <- rev(seq(.05,length=length(qry),by=.3))
  nms <- typology$EFG[match(gsub("_",".",qry),typology$code)]
  ##par(mar=c(4,15,1,1),yaxs="i")
  plot(NA,xlim=c(-3.7,3.7),ylim=c(min(ys)-.15,max(ys)+.15),main="",xlab='zscore(log (MCHI))',ylab='',axes=F)
  abline(v=0,lty=2,col="maroon")
  axis(1)
  axis(2,ys,nms,las=2,cex.axis=.6)
  box()
  for (k in 1:length(ys)) {
   bxp.plot(x=lzMCHI[,gsub("\\.","_",qry[k])],
   hgt=ys[k],os=.1,clr=c("green","darkgreen"),pch=19)
  	}
    dev.off()
