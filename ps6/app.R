library(shiny)
library(dplyr)

# Define UI
ui <- navbarPage("Iris Measurements",
                 tabPanel("Overview",
                          em("This app uses the", strong("IRIS"), "dataset from ggplot2."),
                          br(),
                          em("Irises are named after the Greek Goddess of the",
                             em("rainbow.")),
                          p("They are pretty cool"),
                          p("The main panel will display a small smaple of this", strong("dataset")),
                          p("This dataset contains", nrow(iris), "rows and", ncol(iris), "columns"),
                          mainPanel(
                            dataTableOutput("set")
                          )),
                 tabPanel("Data Table",
                          sidebarLayout(
                            sidebarPanel(
                              p("Here you can modify the sliders to find unique species of Irises
                         based on their", em("Sepal Length, Petal Length and Sepal Width.")),
                              sliderInput("sepal_length", "Select Sepal Length", min = 0, max = max(iris$Sepal.Length), 
                                          value = c(0, max(iris$Sepal.Length)), step = 0.2),
                              sliderInput("petal_length", "Select Petal Length", min = 0, max = max(iris$Petal.Length), 
                                          value = c(0, max(iris$Petal.Length)), step = 0.2),
                              sliderInput("sepal_width", "Select Sepal Width", min = 0, max = max(iris$Sepal.Width), 
                                          value = c(0, max(iris$Sepal.Width)), step = 0.2)
                            ),
                            mainPanel(
                              dataTableOutput("table"),
                              textOutput("row_count")
                            )
                          )
                 ),
                 
                 tabPanel("Histogram",
                          sidebarLayout(
                            sidebarPanel(p("In this page, you can adjust the color and settings of the",
                                           strong("histogram")), 
                                         selectInput("var", "Select value from Iris measures",
                                                     choices = c("Sepal.Length"=1, "Sepal.Width"=2, "Petal.Length"=3, "Petal.Width"=4)),
                                         sliderInput("bins", "Select bin size", min = 6, max = 30, value = 12),
                                         radioButtons("color", "Select Histogram Color", choices = 
                                                        c("darkolivegreen1", "paleturquoise2","lightsalmon","lightcyan1"),
                                                      selected = "lightsalmon") 
                                         
                            ),
                            mainPanel(
                              plotOutput("hist"),
                              textOutput("hitext")
                            ))
                 ))

# Define server
server <- function(input, output) {
  output$set <- renderDataTable({
    iris %>% 
      sample_n(10)
  })
  filtered <- reactive({
    data <- iris %>% filter(Sepal.Length >= input$sepal_length[1] & Sepal.Length <= input$sepal_length[2])
    data <- data %>% filter(Sepal.Width >= input$sepal_width[1] & Sepal.Width <= input$sepal_width[2])
    data <- data %>% filter(Petal.Length >= input$petal_length[1] & Petal.Length <= input$petal_length[2])
    return(data)
  })
  output$table <- renderDataTable({
    filtered()
  })
  output$row_count <- renderText({
    paste("Number of rows:", nrow(filtered()))
  })
  
  output$hist <- renderPlot({
    co <- as.numeric(input$var)
    irises <- (iris[,co])
    breaks <- seq(0, max(iris[,co]), l=input$bins+1) 
    col <- input$color
    rtext <- paste("Histogram of", names(iris)[co],
                   "with", input$bins, "bins and color", col)
    output$hitext <- renderText(rtext)
    hist(irises, breaks = breaks, col = col)
    output$hitext <- renderText("")
    
    
    
  })
  
  
}


shinyApp(ui, server)


