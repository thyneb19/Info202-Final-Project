#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
breaches <- readRDS("C:/Users/thyne/Documents/GitHub/Info202-Final-Project/CyberSecurity_DataExploration_App/data/Breach_Data.RDS")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("somevalue", "Some value", choices = unique(breaches$Sector), FALSE),
        verbatimTextOutput("value")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      ggplot(breaches, aes(x = Sector, y = Records_Lost)) + geom_boxplot()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

