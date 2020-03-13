# Load packages
library(shiny)
#library(shinythemes)
library(dplyr)
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
                  selected = "Terrestrial")
               ),

    # Output: Description, lineplot, and reference
    mainPanel(
      plotOutput(outputId = "lineplot", height = "300px",click = "plot_click"),
       verbatimTextOutput("info")
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
        )
  })


  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    color = d.legend$col[match(selected_grps()$biome.lab, d.legend$lab)]
    smbl = d.legend$pch[match(selected_grps()$biome.lab, d.legend$lab)]
    labs = rownames(selected_grps())
    par(mar = c(4, 4, 1, 1))
    plot(x = selected_grps()$degraded, y = selected_grps()$protected, type = "p",
         xlab = "% degraded", ylab = "% protected", col = color, pch=smbl,  fg = "white", col.lab = "black", col.axis = "#677377")
     })

 output$info <- renderPrint({
    # With base graphics, need to tell it what the x and y variables are.
    nearPoints(data[,c("biome.lab","Names","degraded","protected")], input$plot_click, xvar = "degraded", yvar = "protected")
    # nearPoints() also works with hover and dblclick events
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)
