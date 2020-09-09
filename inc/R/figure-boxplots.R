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


load(sprintf("%s/Rdata/Impact-index-all-versions.rda", script.dir))

  read.rstats <- function(x) {
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

mit <- "quantile"

lzHFP <- lzMCHI <- data.frame()
qq <- subset(EFG.idx,version %in% "version-2.0.1b" & index %in% "HFP2013")
dts <- read.rstats(qq)
for (efg in unique(dts$EFG)) {
  ss <- subset(dts,EFG %in% efg)
  	if (any(!is.na(ss$zscore))) {
  		lzHFP <- rbind(lzHFP,ind.bxp(ss,expression(area*as.numeric(map)),scol="logzscore",type=mit))
  	} else {
  		lzHFP <- rbind(lzHFP,rep(NA,6))
  	}
}
lzHFP <- t(lzHFP)
colnames(lzHFP) <-  unique(dts$EFG)
rownames(lzHFP) <- NULL

qq <- subset(EFG.idx,version %in% "version-2.0.1b" & index %in% "MCHI2013")
dts <- read.rstats(qq)
for (efg in unique(dts$EFG)) {
  ss <- subset(dts,EFG %in% efg)
  	if (any(!is.na(ss$zscore))) {
  		lzMCHI <- rbind(lzMCHI,ind.bxp(ss,expression(area*as.numeric(map)),scol="logzscore",type=mit))
  	} else {
  		lzMCHI <- rbind(lzMCHI,rep(NA,6))
  	}
}

lzMCHI <- t(lzMCHI)
colnames(lzMCHI) <-  unique(dts$EFG)
rownames(lzMCHI) <- NULL

pdf("Figure_S5_3_BoxPlots.pdf",width=9,height=7)

layout(matrix(1:2,ncol=2))

ys <- rev(seq(.05,length=ncol(lzHFP),by=.3))
nms <- EFG.names[pmatch(sprintf("%s ",colnames(lzHFP)),EFG.names,duplicates.ok=T)]
nms <- gsub("shrublands and grasslands","shrubs/grass",nms)
nms <- gsub("and bays","",nms)

par(mar=c(4,10,0,0),yaxs="i")
plot(NA,xlim=c(-1.5,2.0),ylim=c(min(ys)-.15,max(ys)+.15),main=NA,xlab='zscore(log (HFP))',ylab='',axes=F)
abline(v=0,lty=2,col="maroon")
axis(1)
axis(2,ys,nms,las=2,cex.axis=.5)
box()
for (k in 1:length(ys)) {
 bxp.plot(x=lzHFP[,k],
 hgt=ys[k],os=.1,clr=c("green","darkgreen"),pch=19)
	}

  ys <- rev(seq(.05,length=ncol(lzMCHI),by=.3))
  nms <- EFG.names[pmatch(sprintf("%s ",colnames(lzMCHI)),EFG.names,duplicates.ok=T)]
  nms <- gsub("shrublands and grasslands","shrubs/grass",nms)
  nms <- gsub("and bays","",nms)

  par(mar=c(4,10,0,0),yaxs="i")
  plot(NA,xlim=c(-5.5,2.5),ylim=c(min(ys)-.15,max(ys)+.15),main=NA,xlab='zscore(log (MCHI))',ylab='',axes=F)
  abline(v=0,lty=2,col="maroon")
  axis(1)
  axis(2,ys,nms,las=2,cex.axis=.5)
  box()
  for (k in 1:length(ys)) {
   bxp.plot(x=lzMCHI[,k],
   hgt=ys[k],os=.1,clr=c("green","darkgreen"),pch=19)
  	}

    dev.off()
