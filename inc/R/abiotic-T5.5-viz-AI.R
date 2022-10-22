require(dplyr)
require(magrittr)
require(ggplot2)
require(plotly)
require(purrr)
source(sprintf("%s/proyectos/IUCN-GET/assessment-ecosystem-functional-groups/env/project-env.R",Sys.getenv("HOME")))
RS <-  function(x,t0=1,tF=2,CV=0.05) {
  IV <- x$fit[t0]
  FV <- x$fit[tF]
  RS <- case_when(
    IV>CV ~ as.numeric(NA),
    FV<IV ~ 0,
    FV>CV ~ 100,
    TRUE ~ 100 * (FV-IV)/(CV-IV)
  )
  return(RS)
}

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


# First split by id

AI_loess <-  AI_data %>% filter(!is.na(AI) & is.finite(AI)) %>% 
  split(.$id) %>%
  map(~ loess(AI ~ year, data = .x)) %>%  
  map(~predict(.x,data.frame(year=c(1915,1965,2015)),se=T)) 
RS100yr <- AI_loess %>% map_dbl(~RS(.x,t0=1,tF=3))
RS50yr <- AI_loess %>% map_dbl(~RS(.x,t0=2,tF=3))



mean(RS100yr>30,na.rm=T)
mean(RS50yr>30,na.rm=T)
mean(RS100yr>50,na.rm=T)
 mean(RS100yr>80,na.rm=T)
 summary(RS100yr)
 summary(RS50yr)
 
 
 
 globalRS <- readRDS(sprintf("%sOUTPUT/AI-TS-RS.rds",work.dir))
 globalRS %>% filter(!is.na(RS100yr)) %>% group_by(id) %>% summarise(n=n(),meanRS=mean(RS100yr)) %>%
   ggplot(aes(x=meanRS)) + geom_histogram(bins=10)
 globalRS %>% filter(!is.na(RS50yr)) %>% group_by(id) %>% summarise(n=n(),meanRS=mean(RS50yr)) %>%
   ggplot(aes(x=meanRS)) + geom_histogram(bins=10)

 globalRS %>% filter(!is.na(RS100yr)) %>% group_by(id) %>% 
   summarise(n=n(),meanRS_100yr=mean(RS100yr,na.rm=T),meanRS_50yr=mean(RS50yr,na.rm=T)) %>% print(n=100)
globalRS %>% filter(!is.na(RS)) %>% 
  ggplot(aes(x=RS)) + geom_histogram(bins=10)


require(sf)
slc <- globalRS %>% filter(!is.na(RS100yr)) %>% pull(id) %>% unique
teow <- read_sf(sprintf("%s/INPUT/teow_2017_valid.gpkg",work.dir))
t5_5 <- teow  %>% filter(OBJECTID %in% slc) %>% st_centroid 

t5_5 %<>% left_join({globalRS %>% filter(!is.na(RS100yr)) %>% group_by(id) %>% 
  summarise(n=n(),meanRS_100yr=mean(RS100yr,na.rm=T),meanRS_50yr=mean(RS50yr,na.rm=T))}, by=c('OBJECTID'='id'))
   
p <-ggplot(t5_5) +geom_sf(aes(size=n,colour=round(meanRS_100yr,1),text=ECO_NAME))
ggplotly(p)

#ggplot(AI_data,aes(x=year,y=AI)) + geom_line(aes(group=id,colour=AIclass)) + 
#  geom_smooth(method='loess') + facet_wrap(.~grp) 


