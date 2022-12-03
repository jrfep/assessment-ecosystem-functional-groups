require(dplyr)
require(magrittr)
require(purrr)
source(sprintf("%s/proyectos/IUCN-GET/assessment-ecosystem-functional-groups/env/project-env.R",
               Sys.getenv("HOME")))

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
# dats

globalRS <- tibble()
for (k in dats$oid) {
  input.file <- sprintf("%sOUTPUT/AI-TS-%04d.rds",work.dir,k)
  AI_data <- readRDS(input.file)
  AI_loess <-  AI_data %>% filter(!is.na(AI) & is.finite(AI)) %>% 
    split(.$id) %>%
    map(~ loess(AI ~ year, data = .x)) %>%  
    map(~predict(.x,data.frame(year=c(1915,1965,2015)),se=T)) 
  RS100yr <- AI_loess %>% map_dbl(~RS(.x,t0=1,tF=3))
  RS50yr <- AI_loess %>% map_dbl(~RS(.x,t0=2,tF=3))
  globalRS %<>% bind_rows(tibble(id=k,RS100yr,RS50yr)) 
}
out.file <- sprintf("%sOUTPUT/AI-TS-RS.rds",work.dir)
saveRDS(file=out.file,globalRS)
