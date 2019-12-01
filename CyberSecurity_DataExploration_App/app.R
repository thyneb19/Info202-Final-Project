library(shinydashboard)
library(shiny)
library(ggplot2)
library(scales)

breaches <- readRDS("/Users/arikaverma/Documents/GitHub/Info202-Final-Project/CyberSecurity_DataExploration_App/data/cleanedBreachData.RDS")


ui <- dashboardPage(
    dashboardHeader(title = "Data Breaches"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Bar Chart", tabName = "BarChart"),
            menuItem("Box Plot", tabName = "BoxPlot")
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "BarChart",
                    fluidRow(
                        box(plotOutput("plot1", height = 400, width = 1000))
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "BoxPlot",
                    # Application title
                    titlePanel(
                        h1("Method of Data Breach vs. Records Lost", align = "center")),
                    
                    # Sidebar with a slider input for number of bins 
                    sidebarLayout(
                        sidebarPanel(
                            checkboxGroupInput("variable", "View By Sector:",
                                               c("Academic" = "Academic", "Energy" = "Energy","Financial" = "Financial", "Government" = "Government", "Healthcare" = "Healthcare", "Legal" = "Legal", 
                                                 "Media" = "Media", "Retail" = "Retail", "Tech" = "Tech", "Telecoms" = "Telecoms", "Transport" = "Transport")),
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                            plotOutput("plot2")
                        )
                    )
            )
                        
 
        )
    )
  )


server <- function(input, output) {
    output$plot1 <- renderPlot({
        ggplot(data = breaches, aes(x = Sector, y = Records_Lost, fill = Method)) + scale_fill_brewer(palette = "Set2") + geom_bar(stat='identity') + labs(y= "Records Lost", title = "Method of Data Breaches Organized by Sector vs. Records Lost") + scale_y_continuous(labels = comma)})

    output$plot2 <- renderPlot({
        
        if (input$variable == "Academic"){
            
            newdata <- subset(breaches, breaches$Sector == "academic")
            ggplot(data = newdata, aes(x= Method, y= Records_Lost, fill= Method)) + scale_fill_brewer(palette = "Set2") + geom_boxplot() + labs(x= "Method", y= "Records Lost") + scale_y_continuous(labels = comma)}
        
        else if (input$variable == "Energy"){
            newdata2 <- subset(breaches, breaches$Sector == "energy")
            ggplot(data = newdata2, aes(x= Method, y= Records_Lost, fill= Method)) + scale_fill_brewer(palette = "Set2") + geom_boxplot() + labs(x= "Method", y= "Records Lost") + scale_y_continuous(labels = comma)}
        
        else if (input$variable == "Financial"){
            newdata3 <- subset(breaches, breaches$Sector == "financial")
            ggplot(data = newdata3, aes(x= Method, y= Records_Lost, fill= Method)) + scale_fill_brewer(palette = "Set2") + geom_boxplot() + labs(x= "Method", y= "Records Lost") + scale_y_continuous(labels = comma)}
        
        else if (input$variable == "Government"){
            newdata4 <- subset(breaches, breaches$Sector == "government")
            ggplot(data = newdata4, aes(x= Method, y= Records_Lost, fill= Method)) + scale_fill_brewer(palette = "Set2") + geom_boxplot() + labs(x= "Method", y= "Records Lost") + scale_y_continuous(labels = comma)}
        
        else if (input$variable == "Healthcare"){
            newdata5 <- subset(breaches, breaches$Sector == "healthcare")
            ggplot(data = newdata5, aes(x= Method, y= Records_Lost, fill= Method)) + scale_fill_brewer(palette = "Set2") + geom_boxplot() + labs(x= "Method", y= "Records Lost") + scale_y_continuous(labels = comma)}
        
        else if (input$variable == "Legal"){
            newdata6 <- subset(breaches, breaches$Sector == "legal")
            ggplot(data = newdata6, aes(x= Method, y= Records_Lost, fill= Method)) + scale_fill_brewer(palette = "Set2") + geom_boxplot() + labs(x= "Method", y= "Records Lost") + scale_y_continuous(labels = comma)}
        
        else if (input$variable == "Media"){
            newdata7 <- subset(breaches, breaches$Sector == "media")
            ggplot(data = newdata7, aes(x= Method, y= Records_Lost, fill= Method)) + scale_fill_brewer(palette = "Set2") + geom_boxplot() + labs(x= "Method", y= "Records Lost") + scale_y_continuous(labels = comma)}
        
        else if (input$variable == "Retail"){
            newdata8 <- subset(breaches, breaches$Sector == "retail")
            ggplot(data = newdata8, aes(x= Method, y= Records_Lost, fill= Method)) + scale_fill_brewer(palette = "Set2") + geom_boxplot() + labs(x= "Method", y= "Records Lost") + scale_y_continuous(labels = comma)}
        
        else if (input$variable == "Tech"){
            newdata9 <- subset(breaches, breaches$Sector == "tech")
            ggplot(data = newdata9, aes(x= Method, y= Records_Lost, fill= Method)) + scale_fill_brewer(palette = "Set2") + geom_boxplot() + labs(x= "Method", y= "Records Lost") + scale_y_continuous(labels = comma)}
        
        else if (input$variable == "Telecoms"){
            newdata10 <- subset(breaches, breaches$Sector == "telecoms")
            ggplot(data = newdata10, aes(x= Method, y= Records_Lost, fill= Method)) + scale_fill_brewer(palette = "Set2") + geom_boxplot() + labs(x= "Method", y= "Records Lost") + scale_y_continuous(labels = comma)}
        
        else if (input$variable == "Transport"){
            newdata11 <- subset(breaches, breaches$Sector == "transport")
            ggplot(data = newdata11, aes(x= Method, y= Records_Lost, fill= Method)) + scale_fill_brewer(palette = "Set2") + geom_boxplot() + labs(x= "Method", y= "Records Lost") + scale_y_continuous(labels = comma)}     
    })
    
    }

shinyApp(ui, server)