#!R --vanilla
require(readr)
require(dplyr)
require(units)
require(magrittr)
require(tidyr)

# unique(system("v.db.select slc_ecoregions columns=BIOME_NAME,BIOME_ID,BIOME_NUM",intern=T))

require("RPostgreSQL")
source(sprintf("%s/proyectos/IUCN-GET/assessment-ecosystem-functional-groups/env/project-env.R",Sys.getenv("HOME")))

drv <- dbDriver("PostgreSQL") ## remember to update .pgpass file

con <- dbConnect(drv, dbname = dbinfo["database"],
                 host = dbinfo["host"],
                 port = dbinfo["port"],
                 user = dbinfo["user"])

require(sf)

read_sf()

## buscar copia de resolve typology xwalk
# xwalk <- dbGetQuery(con,'select * FROM resolve.typology_xwalk')

# ## which one have multiple versions
# xwalk %>% group_by(efg_code) %>% summarise(code=n_distinct(map_code),ver=n_distinct(map_version)) %>% filter(code>1 | ver>1) %>% pull(efg_code) -> slc
# xwalk %>% filter(efg_code %in% slc) %>% distinct(map_code,map_version) %>% arrange(map_code,map_version)
#
# xwalk %>% group_by(efg_code) %>% summarise(code=n_distinct(map_code),ver=n_distinct(map_version)) %>% filter(code>1 | ver==2) %>% pull(efg_code) -> slc
#
# slc_xwalk <- xwalk %>% filter(map_code %in% "T2.4.IM.orig" & map_version %in% "v3.0")
# slc_xwalk %<>% bind_rows(xwalk %>% filter(efg_code %in% slc & map_version %in% "v2.0"))
# xwalk %>% group_by(efg_code) %>% summarise(code=n_distinct(map_code),ver=n_distinct(map_version)) %>% filter(code>1 | ver==1) %>% pull(efg_code) -> slc
# slc_xwalk %<>% bind_rows(xwalk %>% filter(efg_code %in% slc & map_version %in% "v1.0"))


dbDisconnect(con)
