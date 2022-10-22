#! R --vanilla
require(raster)
require(ncdf4)
require(RColorBrewer)

pet <- stack("/srv/scratch/cesdata/gisdata/climate/global/CRU_TS/pet/cru_ts4.03.1901.2018.pet.dat.nc")
pre <- stack("/srv/scratch/cesdata/gisdata/climate/global/CRU_TS/pre/cru_ts4.03.1901.2018.pre.dat.nc",varname="pre")
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
