require(dplyr)
require(magrittr)
require(ggplot2)
require(plotly)
source(sprintf("%s/proyectos/IUCN-GET/assessment-ecosystem-functional-groups/env/project-env.R",Sys.getenv("HOME")))

sumfile <- sprintf("%sOUTPUT/AI-TS-summary.rds",work.dir)
AI_summary <- readRDS(sumfile)
AI_summary %>% distinct(eco)
dats <- AI_summary %>% filter(class %in% "hyper-arid") %>% transmute(oid,eco,cells,cprop=cells/total,yprop=cell_years/(cells*118))
dats

p <- ggplot(dats) + geom_point(aes(x=cprop,y=yprop,text=eco,size=cells))
ggplotly(p)

# Nile Delta flooded savanna
# Namib desert 462
k <- 462
input.file <- sprintf("%sOUTPUT/AI-TS-%04d.rds",work.dir,k)
AI_data <- readRDS(input.file)

id_smr <-AI_data %>% group_by(id) %>% summarise(meanAI=mean(AI),prop=mean(AI<0.05)) 
id_smr %>% mutate(grp=case_when(prop<0.75~"unstable",prop>0.9~"stable",TRUE~"transitional")) %>% pull(grp) %>% table
summary(id_smr$prop)

AI_data %<>% left_join(id_smr,by="id") 
AI_data %<>% mutate(year=as.numeric(year),
                    grp=case_when(prop<0.75~"unstable",prop>0.9~"stable",TRUE~"transitional"))


ggplot(AI_data,aes(x=year,y=AI)) + geom_point(colour='peru') + 
  geom_smooth(formula=y~x,method='loess',colour='red',fill='pink') +theme_bw()+ facet_wrap(grp~id) 


# First nest by id
require(purrr)
AI_data.nested <- AI_data %>%
  group_by(id) %>%
  nest_by()

# Now apply the linear model call by group using the data.
AI_data.nested %>% map(., ~ loess(AI ~ year, data = .$data))
  mutate(models = map(data, ~ loess(AI ~ year, data = .)))

  RS <-  function(x,CV=0.05) {
    IV <- x$fit[1]
    FV <- x$fit[2]
    RS <- case_when(
      IV>CV ~ as.numeric(NA),
      FV<IV ~ 0,
      FV>CV ~ 100,
      TRUE ~ 100 * (FV-IV)/(CV-IV)
    )
    return(RS)
  }
  
    
 RS_rslt <-  AI_data %>%
    split(.$id) %>%
    map(~ loess(AI ~ year, data = .x)) %>%  
    map(~predict(.x,data.frame(year=c(1901,2018)),se=T)) %>%
    map_dbl(~RS(.x))
  


 mean(RS_rslt>30,na.rm=T)
 mean(RS_rslt>50,na.rm=T)
 mean(RS_rslt>80,na.rm=T)
summary(RS_rslt)
 
globalRS <- tibble()
 for (k in dats$oid) {
   input.file <- sprintf("%sOUTPUT/AI-TS-%04d.rds",work.dir,k)
   AI_data <- readRDS(input.file)
   RS_rslt <-  AI_data %>% filter(!is.na(AI) & is.finite(AI)) %>% 
     split(.$id) %>%
     map(~ loess(AI ~ year, data = .x)) %>%  
     map(~predict(.x,data.frame(year=c(1901,2018)),se=T)) %>%
     map_dbl(~RS(.x))
  #if (any(RS_rslt>0))
    globalRS %<>% bind_rows(tibble(id=k,RS=RS_rslt)) 
     
 }
globalRS %>% filter(!is.na(RS)) %>% group_by(id) %>% summarise(meanRS=mean(RS)) %>%
ggplot(aes(x=meanRS)) + geom_histogram(bins=10)

globalRS %>% filter(!is.na(RS)) %>% 
  ggplot(aes(x=RS)) + geom_histogram(bins=10)

   
ggplot(AI_data,aes(x=year,y=AI)) + geom_line(aes(group=id,colour=AIclass)) + 
  geom_smooth(method='loess') + facet_wrap(.~grp) 

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
