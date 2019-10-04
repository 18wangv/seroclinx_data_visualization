library(shiny)
library(datasets)
library(ggplot2)
library(ggfortify)
library(dplyr)
data("iris")
# Renamed columns to match with my inputs from dropdowns
renamed_iris <- iris %>% rename(`Sepal Width` = Sepal.Width, `Sepal Length` = Sepal.Length, `Petal Width` = Petal.Width, `Petal Length` = Petal.Length)
renamed_iris$Species <- as.factor(renamed_iris$Species)
# Keeping only numeric columns for pca
pca_df <- renamed_iris[c(1,2,3,4)]
ui <- fluidPage(titlePanel("Iris Data Visualization"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("display", "Pick a display", c("PCA", "Scatterplot", "Histogram")),
                    # Only appears when Scatterplot is selected
                    conditionalPanel(condition = "input.display==Scatterplot", 
                                     uiOutput("scatter_1"),
                                     uiOutput("scatter_2")),
                    # Only appears when Histogram is selected
                    conditionalPanel(condition = "input.display==Histogram", 
                                     uiOutput("hist"))
                  ),
                  # Output from plots
                  mainPanel(plotOutput("display_plot"))
                )
)
server <- function(input, output) {
  # Renders top button for when scatterplot is picked
  output$scatter_1 <- renderUI({
    if (input$display=="Scatterplot"){
      selectInput("scatter_display_1", "Pick variable of interest", c("Sepal Width", "Sepal Length", "Petal Length", "Petal Width"), selected = "Petal Width")
    }
  })
  # Renders second button for when scatterplot is picked
  output$scatter_2 <- renderUI({
    if (input$display=="Scatterplot"){
      selectInput("scatter_display_2", "Pick comparison varaible", c("Sepal Width", "Sepal Length", "Petal Length", "Petal Width"), selected = "Petal Length")
    }
  })
  # Renders first button fir when histogram is picked
  output$hist <- renderUI(({
    if (input$display=="Histogram"){
      selectInput("hist_display", "Pick variable of interest", c("Sepal Width", "Sepal Length", "Petal Length", "Petal Width"), selected = "Petal Length")
    }
  }))
  # Output of the graph
  output$display_plot <- renderPlot({
    if (input$display=="PCA"){
      autoplot(prcomp(pca_df), renamed_iris, colour = "Species")
    }
    else if (input$display=="Scatterplot"){
      x_axis = input$scatter_display_1
      p <- ggplot(renamed_iris, aes(x=as.numeric(unlist(select(renamed_iris, input$scatter_display_1))), y=as.numeric(unlist(select(renamed_iris, input$scatter_display_2))), color=renamed_iris$Species))
      p + geom_point() + labs(x = input$scatter_display_1, y = input$scatter_display_2, title = paste(input$scatter_display_1, "compared against", input$scatter_display_2, sep = " "), color = "Species")
    }
    else {
      p <- ggplot(renamed_iris, aes(x=as.numeric(unlist(select(renamed_iris, input$hist_display)))))
      p + geom_histogram(binwidth = 0.2, color="white", fill = "light blue") + labs(x = input$hist_display, y = "count", title = paste(input$hist_display, "Distribution", sep = " "), color = "Species")
    }
  })
}
shinyApp(ui = ui, server = server)