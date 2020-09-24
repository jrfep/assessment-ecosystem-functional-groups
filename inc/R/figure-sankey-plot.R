##R --vanilla
library(plotly)
require(tidyr)

work.dir <- Sys.getenv("WORKDIR")
script.dir <- Sys.getenv("SCRIPTDIR")
setwd(work.dir)

clrs <-
c(EFG="#A6CEE3",
wild="#B2DF8A",
`wild unprotected`="#B2DF8A",
degraded="#FFFF99",
protected="#33A02C",
seminatural="#FFFF99",
transformed="#FB9A99",
mosaic="#FFFF99",
urban="#6A3D9A",
pastures="#CAB2D6",
crops="#FDBF6F",
rice="#FF7F00"  )

archs <- dir(sprintf("%s/output/version-2.0.1b/",work.dir),"Transform_Terrestrial")
archs <- grep("T7|F3|MT3|SF",archs,inv=T,value=T)

for (arch in archs) {
  EFG <- gsub("(*).IM.*","\\1",gsub("Transform_Terrestrial_","",arch))
  output.fig <- sprintf("output/figures/sankeyplots/%s.png",EFG)
  if (!file.exists(output.fig)) {

    data <- read.table(sprintf("%s/output/version-2.0.1b/%s", work.dir,arch),header=F,col.names=c("p_cult","p_irrig","p_past","p_rice","p_urban","WDPA","degraded","map","area_m2"),na.strings = "*") %>% filter(!is.na(map),!is.na(degraded)) %>% replace_na(list(p_cult=0,p_irrig=0,p_past=0,p_rice=0,p_urban=0,WDPA=0)) %>% mutate(area=area_m2/1e6)

    links <- rbind(
      data.frame(source=EFG,target="wild",
      value=data %>% filter(degraded %in% 0) %>% select(area) %>% sum),
      data.frame(source=EFG,target="degraded",
      value=data %>% filter(degraded %in% 1) %>% select(area) %>% sum),
      data.frame(source="wild",target="wild unprotected",
      value=data %>% filter(degraded %in% 0 & WDPA %in% 0) %>% select(area) %>% sum),
      data.frame(source="wild",target="protected",
      value=data %>% filter(degraded %in% 0 & WDPA %in% 1) %>% select(area) %>% sum),
      data.frame(source="transformed",target="urban",value=data %>% filter(degraded %in% 1) %>% transmute(t.urban=area*p_urban) %>% sum),
      data.frame(source="transformed",target="pastures",value=data %>% filter(degraded %in% 1) %>% transmute(t.urban=area*p_past) %>% sum),
      data.frame(source="transformed",target="crops",value=data %>% filter(degraded %in% 1) %>% transmute(t.urban=area*p_cult) %>% sum),
      data.frame(source="transformed",target="rice",value=data %>% filter(degraded %in% 1) %>% transmute(t.urban=area*p_rice) %>% sum)
    )

    links <- rbind(links,
      data.frame(source="degraded",target="transformed",value=links %>% filter(source %in% "transformed") %>% select(value) %>% sum))

      links <- rbind(links,
        data.frame(source="degraded",target="seminatural",value=(links %>% filter(target %in% "degraded") %>% select(value) %>% sum) - (links %>% filter(source %in% "transformed") %>% select(value) %>% sum)))
        links <- rbind(links,
          data.frame(source="seminatural",target="protected",value=data %>% filter(degraded %in% 1 & WDPA %in% 1) %>% select(area) %>% sum))
          links <- rbind(links,
            data.frame(source="seminatural",target="mosaic",value=(links %>% filter(target %in% "seminatural") %>% select(value) %>% sum) - (links %>% filter(source %in% "seminatural") %>% select(value) %>% sum)))

            links$color <- clrs[as.character(links$target)]
            links$source_n <- c(0,0,1,1,4,4,4,4,3,3,2,2)
            links$target_n <- c(1,3,5,6,8,9,10,11,4,2,6,7)

            totals = links %>% filter(target_n>4) %>% group_by(target_n) %>% summarise(total=sum(value)) %>% select(total)

            b=totals/2
            c=cumsum(totals)
            a=c-b
            a=a/max(c)


            ##  ,
            ##  x=c(0,.3,.6,.3,.6,1,1,1,1,1,1,1), y=c(1,.25,.25,.75,.75,a)

            fig <- plot_ly(
              type = "sankey",
              orientation = "h",
              node=list(label=c(EFG,"wild","seminatural","degraded","transformed", "wild unprotected","protected","mosaic", "urban","pastures","crops","rice"),
              x=c(0,.3,.6,.3,.6,1,1,1,1,1,1,1), y=c(1, 1/mean(1/a$total[1:2]),a$total[3],  mean(a$total[2:7]),1/mean(1/a$total[4:7]),a$total),
              color=clrs[c("EFG","wild","seminatural","degraded","transformed", "wild unprotected","protected","mosaic", "urban","pastures","crops","rice")],
              pad = 15,
              thickness = 20,
              line = list(
                color = "black",
                width = 0.5)),
                link = list(
                  source = links$source_n,
                  target = links$target_n,
                  color = links$color,
                  value =  links$value
                )
              )
              fig <- fig %>% layout(
                #title = EFG ,
                font = list(size = 20)
              )
              ##fig
              orca(fig,output.fig)
            }

          }
