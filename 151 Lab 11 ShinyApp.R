install.packages("shiny")
library(shiny)
library(tidyverse)
install.packages("car")
library(car)

data(Prestige)
help(Prestige)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(car)
library(tidyverse)
library(shiny)

ui <- fluidPage(
  
  titlePanel("Prestige and Income data"),   
  
  sidebarPanel( 
    selectInput(inputId = "type", 
                label = "Job type", 
                choices = Prestige$type)
  ),
  mainPanel(
    plotOutput(outputId = "scatterplot")
  )
  #use commas between every argument of fluidPage fxn
)

server <- function(input, output) {
  
  output$scatterplot <- renderPlot({ 
    dat <- Prestige %>%
      filter(type == input$type)
    ggplot(dat, aes(x=education,y=prestige)) + 
      geom_point() + geom_smooth(method = 'lm') +
      geom_rug()
  })
  
}

shinyApp(ui, server)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(car)
library(tidyverse)
library(shiny)

ui <- fluidPage(
  
  titlePanel("My Shiny App"),
  sliderInput('slide', 
              'Choose a number of cases: ',
              value = 50, max = 100, min = 1),
  sliderInput('nbins', 
              'Choose a number of bins: ',
              value = 5, max = 10, min = 5),
  plotOutput(outputId = "histogram")
  
)

server <- function(input, output) {
  output$histogram <- renderPlot(
    {
      dat <- data.frame(x = rnorm(input$slide))
      ggplot(dat, aes(x = x)) + geom_histogram(bins = input$nbins)
    }
  )
}

shinyApp(ui, server)
