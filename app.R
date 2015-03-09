# Deploy App in console ---------------------------------------------------
## library(shinyapps)
## deployApp()

# Library -----------------------------------------------------------------
library(shiny)
library(shinydashboard)

library(dplyr)
library(ggplot2)
library(caret)

# Header -----------------------------------------------------------------

header = dashboardHeader(
  title = "iris dataset"
)

# Sidebar ----------------------------------------------------------------

sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "overview"),
    menuItem("Descriptive", tabName = "descriptive", 
             icon = icon("dashboard")),
    menuItem("Prediction", tabName="prediction",
             badgeLabel = "Working", 
             badgeColor = "yellow"),
    menuItem("Reference", tabName = "reference")
  )
)

# Body -------------------------------------------------------------------

body = dashboardBody(
  tabItems(
    tabItem("overview",
            "Overview on iris dataset"
    ),
    tabItem("descriptive",
            fluidRow(
              box(title = "Input",
                  radioButtons("species", 
                               label = "Species",
                               choices = c(
                                 "Setosa" =" setosa",
                                 "Versicolor" = "versicolor",
                                 "Virgincia" = "virginca"
                               ),
                               selected = "setosa"
                  ),
                  selectInput("measure",
                              label = "Mesaure",
                              choices = c(
                                "Sepal.Length" = "Sepal.Length",
                                "Sepal.Width" = "Sepal.Width",
                                "Petal.Length" = "Petal.Length",
                                "Petal.Width" = "Petal.Width"
                              ),
                  )
              )
            ),
            fluidRow(
              box(title = "Histogram",
                  solidHeader = TRUE,
                  collapsible =  TRUE,
                  plotOutput("hist", height = 250)
              ),
              box("Summary",
                  solidHeader = TRUE, 
                  tableOutput("summary")
              )
            )
    ), 
    tabItem("prediction",
            fluidRow(
                   box("Input",
                       sliderInput("Sepal.Length",
                                   "Length of Sepal:",
                                   min = 4.2, max = 8, value = 5),
                       sliderInput("Sepal.Width",
                                   "Width of Sepal:",
                                   min = 2, max = 5.5, value = 3),
                       sliderInput("Petal.Length",
                                   "Length of Petal:",
                                   min = 1, max = 7.5, value = 4),
                       sliderInput("Petal.Width",
                                   "Width of Petal:",
                                   min = 0, max = 2.5, value = 1)
                   )
            ),
            fluidRow(
              valueBoxOutput("prediction"),
              tableOutput("inputValue")
            )			
    ),
    tabItem("reference",
            fluidRow(
              a(href = "http://rstudio.github.io/shinydashboard/",
                valueBox("Shiny DB", 
                         subtitle = "Official site",
                         color = "light-blue"
                )
              ),
              a(href = "https://archive.ics.uci.edu/ml/datasets/Iris",
                valueBox("Iris dataset",
                         subtitle = "uci machine learning respository",
                         color = "blue"
                )
              ),
              a(href = "http://docs.ggplot2.org/current/",
                valueBox("ggplot2",
                         subtitle = "documentation site",
                         color = "light-blue"
                )
              )
            )
    )
  )
)


# Server -----------------------------------------------------------------

server = function(input, output) { 
  ## selectedData 
  selectedData = reactive({
    iris = read.csv(file = "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data",
                     header = FALSE)
    names(iris) = c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")
    iris$Species = gsub("Iris-","",iris$Species)
    filter(iris, 
           Species == input$type)
  })
  ## histogram 
  output$hist = renderPlot({
    h = qplot(input$measure, data = selectedData(),  
              geom = "histogram", binwidth = 0.1)
    h + labs(title = input$type)
  })
  ## summary
  output$sumamry = renderTable ({
    summary(selectedData())
  })
  ## rpart
  fit = train(iris[, 1:4], iris[,5], data = iris, method = "rpart")
  species = levels(iris$Species)
  
  inputValue = reactive({
    input = data.frame(input$Sepal.Length, input$Sepal.Width, 
                       input$Petal.Length, input$Petal.Width)
    names(input) = names(iris[,1:4])
    input
  })
  
  prediction = reactive({
    pred = predict(fit, inputValue())
    which.max(pred)
  })
  output$inputValue = renderTable({
    inputValue()
  })
  
  speciesColor = c("green", "yellow", "red")
  output$prediction = renderValueBox({
    preditedSpecies = prediction()
    valueBox(species[preditedSpecies], 
             subtitle = "Predicted Species",
             color = speciesColor[preditedSpecies]
    )
  })
}

# Render -----------------------------------------------------------------

shinyApp(
  ui = dashboardPage(header, sidebar, body, skin="black"),
  server = server
)

# Reference ---------------------------------------------------------------

# http://datadrivensecurity.info/blog/posts/2015/Jan/building-security-dashboards-with-r-and-shiny-shinydashboard/