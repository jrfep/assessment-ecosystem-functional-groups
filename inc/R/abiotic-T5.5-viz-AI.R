sumfile <- sprintf("%sOUTPUT/AI-TS-summary.rds",work.dir,k)
AI_summary <- readRDS(sumfile)

dats <- AI_summary %>% filter(class %in% "hyper-arid") %>% transmute(eco,cells,cprop=cells/total,yprop=cell_years/(cells*118))

require(ggplot2)
require(plotly)

p <- ggplot(dats) + geom_point(aes(x=cprop,y=yprop,text=eco,size=cells))
ggplotly(p)
ggplot(AI_data,aes(x=year,y=AI)) + geom_point(aes(colour=AIclass))
ggplot(AI_data,aes(x=year,y=AI)) + geom_line(aes(group=id,colour=AIclass))

AI <- pres[[1]]/(pets[[1]]*30)
matplot(t(AI),type="l")
rowMeans(AI<0.05)


for (yy in (2018-50):2018) {
  ix <- grep(sprintf("X%s",yy),names(pet))
  if (!exists("AI")) {
    AI <- stack(sum(subset(pre,ix))/(sum(subset(pet,ix))*30))
  } else {
    AI <- addLayer(AI,sum(subset(pre,ix))/(sum(subset(pet,ix))*30))
  }
  ##plot(AI)
}
plot(AI,25)


writeRaster(mean(AI),file="CRU-AI.tif",format="GTiff")
plot(mean(AI),col=brewer.pal(5,"Pastel1"),breaks=c(0,0.05,0.2,0.5,0.65,400))
table(cut(values(mean(AI)),breaks=c(0,0.05,0.2,0.5,0.65,Inf),labels=c("hyper-arid","arid","semi-arid","dry sub-humid","humid")))
