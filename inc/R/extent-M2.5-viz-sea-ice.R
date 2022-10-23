require(sf)
source(sprintf("%s/proyectos/IUCN-GET/assessment-ecosystem-functional-groups/env/project-env.R",Sys.getenv("HOME")))

mpa <- read_sf(sprintf("%s/cryosphere/regional/CCAMLR/data/geographical_data/mpa/mpa.json",gis.data))
plot(mpa['GAR_Short_Label'])

asd <- read_sf(sprintf("%s/cryosphere/regional/CCAMLR/data/geographical_data/asd/asd.json",gis.data))
plot(asd['GAR_Short_Label'])

plot(asd %>% slice(grep("Subarea",GAR_Name)) %>% dplyr::select(GAR_Name))

miny <- 50
maxy <- 90
minx <- 120
maxx <- 180
s1 <- rbind(c(minx, miny), c(minx, maxy), c(maxx, maxy), c(maxx, miny),c(minx, miny))
minx <- 60
maxx <- 120
s2 <- rbind(c(minx, miny), c(minx, maxy), c(maxx, maxy), c(maxx, miny),c(minx, miny))
minx <- 0
maxx <- 60
s3 <- rbind(c(minx, miny), c(minx, maxy), c(maxx, maxy), c(maxx, miny),c(minx, miny))
minx <- -60
maxx <- 0
s4 <- rbind(c(minx, miny), c(minx, maxy), c(maxx, maxy), c(maxx, miny),c(minx, miny))
minx <- -120
maxx <- -60
s5 <- rbind(c(minx, miny), c(minx, maxy), c(maxx, maxy), c(maxx, miny),c(minx, miny))

sfc_ex <- st_sfc(list(st_polygon(list(s1)),
                      st_polygon(list(s2)),
                      st_polygon(list(s3)),
                      st_polygon(list(s4)),
                      st_polygon(list(s5))))
df_ex <- data.frame(name = c("A", "B", "C","D","E"))
df_ex$geometry <- sfc_ex
sf_ex <- st_as_sf(df_ex)
st_crs(sf_ex) <- crs("+datum=WGS84 +proj=longlat")

polar_regs <- sf_ex %>% st_transform(crs=rst@crs)

plot(rst,1)
plot(polar_regs,add=T)

# plot(polar_regs)
