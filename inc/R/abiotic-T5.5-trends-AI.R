#! R --vanilla
require(raster)
require(ncdf4)
require(sf)
require(tidyr)
require(stringr)
require(dplyr)
require(magrittr)
source(sprintf("%s/proyectos/IUCN-GET/assessment-ecosystem-functional-groups/env/project-env.R",Sys.getenv("HOME")))

slc <- c('Arabian sand desert','Atacama desert','Cordillera Central páramo',
         'Djibouti xeric shrublands','East Sahara Desert','Eritrean coastal desert',
         'Kaokoveld desert','Namib Desert','Red Sea-Arabian Desert shrublands', 
         'Red Sea coastal desert','Sechura desert','West Sahara desert','Arabian desert')

teow <- read_sf(sprintf("%s/INPUT/teow_2017_valid.gpkg",work.dir))
t5_5 <- teow  %>% filter(ECO_NAME %in% slc) %>% pull(OBJECTID)
all_ids <- teow  %>% filter(!ECO_NAME %in% "Rock and Ice") %>%pull(OBJECTID)

pet <- stack(sprintf("%s/INPUT/cru_ts4.03.1901.2018.pet.dat.nc",work.dir))
pre <- stack(sprintf("%s/INPUT/cru_ts4.03.1901.2018.pre.dat.nc",work.dir),varname="pre")

sumfile <- sprintf("%sOUTPUT/AI-TS-summary.rds",work.dir)
AI_summary <- tibble()

for (k in sample(all_ids)) {
   econame <- teow %>% filter(OBJECTID %in% k) %>% pull(ECO_NAME)
   outfile <- sprintf("%sOUTPUT/AI-TS-%04d.rds",work.dir,k)
   if (!file.exists(outfile)) {
      pres <- raster::extract(pre,teow %>% filter(OBJECTID %in% k))
      pets <- raster::extract(pet,teow %>% filter(OBJECTID %in% k))
      total_cells <- nrow(pres[[1]])
      # do this for each ecoregion, pivot longer with year and month, join, transform pet (x30), 
      # group by year and summarise AI, check frequency of AI<0.05 
   
      pre_data <- tibble(id=1:nrow(pres[[1]]),data.frame(pres[[1]])) %>% 
         pivot_longer(cols=X1901.01.16:X2018.12.16,names_to = "datestring",values_to = "PRE") 
      pet_data <- tibble(id=1:nrow(pets[[1]]),data.frame(pets[[1]])) %>% 
         pivot_longer(cols=X1901.01.16:X2018.12.16,names_to = "datestring",values_to = "PET") 
      
      prepet_data <- pre_data %>% left_join(pet_data,by=c("id","datestring")) %>% 
         mutate(year=str_extract(datestring,"[0-9]+")) 
      
      AI_data <- prepet_data %>% 
         group_by(id,year) %>% summarise(PRE=sum(PRE),PET=sum(PET*30),AI=PRE/PET) %>% # not sure where the *30 comes from?
         mutate(AIclass=cut(AI,breaks=c(0,0.05,0.2,0.5,0.65,400),
                            labels=c("hyper-arid","arid","semi-arid","dry sub-humid","humid")))
      saveRDS(file=outfile,AI_data)
      
   } else {
      AI_data <- readRDS(outfile)
      total_cells <- AI_data %>% pull(id) %>% unique() %>% length
   }
   
   AI_summary %<>% bind_rows({AI_data %>% group_by(AIclass) %>% summarise(cells=n_distinct(id),cell_years=n_distinct(id,year)) %>%
      transmute(oid=k,eco=econame,total=total_cells,class=AIclass,cells,cell_years)})
   saveRDS(file=sumfile,AI_summary)
}
