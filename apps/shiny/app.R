# Load packages
library(shiny)
#library(shinythemes)
library(dplyr)
require(plotly)
#library(readr)

# Load data
load("Rdata/summary.rda")
data <- d

# Define UI
ui <- fluidPage(##theme = shinytheme("lumen"),
  titlePanel("Ecosystem functional groups"),
  h1("% protected/degraded"),
  sidebarLayout(
    sidebarPanel(

      # Select type of trend to plot
      selectInput(inputId = "type", label = strong("Realm"),
                  choices = unique(data$grp),
                  selected = "Terrestrial"),
      selectInput(inputId = "vers", label = strong("Version"),
                  choices = unique(data$version),
                  selected = "v1.0")
               ),

    # Output: Description, lineplot, and reference
    mainPanel(
      plotlyOutput(outputId = "lineplot", height = "600px"),#,click = "plot_click"),

    )

  )
)


# Define server function
server <- function(input, output) {

  # Subset data
  selected_grps <- reactive({
       data %>%
      filter(
        grp == input$type
        ) %>%
       filter(
         version == input$vers
         )
  })


  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlotly({
    clr2 = d.legend$col[match(unique(selected_grps()$biome.lab), d.legend$lab)] #c(0,0,16,16,16,16,16,16,17,0)
    smbl = d.legend$pch[match(unique(selected_grps()$biome.lab), d.legend$lab)] #c(0,0,16,16,16,16,16,16,17,0)
    labs = d.legend$lab[match(unique(selected_grps()$biome.lab), d.legend$lab)]
    par(mar = c(4, 4, 1, 1))
    p <-   ggplot(selected_grps(), aes(degraded, protected, color = biome, shape=biome)) +
             scale_shape_manual("", values=smbl,labels=labs)+
             scale_color_manual("", values=clr2,labels=labs)+
             geom_point(aes(text=Names), size = 5) + #geom_text_repel(aes(label = Names),colour=1,size=3) +
             labs( x = "% exposed to high pressures",
             y = "% protected",colour = "Biomes") + theme_classic() +
             theme(legend.position = "right", legend.text = element_text(size=9,angle=0,colour ="black"), axis.title = element_text(size = 12), axis.text = element_text(size = 12), panel.border=element_rect(colour="black",fill=NA,size=1)) +
             geom_hline(yintercept = 17, color="black",lty=3,lwd=.5) +
             geom_vline(xintercept=70,color="black",lty=3,lwd=.5) + coord_cartesian(xlim=c(0,100),ylim=c(0,50))
      ggplotly(p, tooltip=c("text","x", "y"))
        #print(p)

     })


}

# Create Shiny object
shinyApp(ui = ui, server = server)
