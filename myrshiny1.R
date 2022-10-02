#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application that draws a histogram

library(shiny)
library(shinyWidgets)
library(mvtnorm)
library(leaflet)
source('try.R')
realdata = read.csv("realdata.csv")
provide.data = realdata[,-(1:2)]

row.names(provide.data) = realdata$City

ui <- fluidPage(
  
  # Application title
  titlePanel("City Search Engine"),
  
  sidebarLayout(
    sidebarPanel(
      
      h3('Input Your Preference(s): \n'),
      
      sliderTextInput(inputId = 'X1', label = 'Movehub Rating', 
                      choices = c("None", "Low", "Medium", "High"),
                      selected = "Low", #if you want any default values 
                      animate = F, grid = T, 
                      hide_min_max = T, from_fixed = FALSE,
                      to_fixed = FALSE),
      
      sliderTextInput(inputId = 'X2', label = 'Purchase Power', 
                      choices = c("None", "Low", "Medium", "High"),
                      selected = "None", #if you want any default values 
                      animate = F, grid = T, 
                      hide_min_max = T, from_fixed = FALSE,
                      to_fixed = FALSE),
      
      sliderTextInput(inputId = 'X3', label = 'Health Care',
                      choices = c("None", "Low", "Medium", "High"),
                      selected = "High", #if you want any default values
                      animate = F, grid = T,
                      hide_min_max = T, from_fixed = FALSE,
                      to_fixed = FALSE),
      
      sliderTextInput(inputId = 'X4', label = 'Pollution',
                      choices = c("None", "Low", "Medium", "High"),
                      selected = "Low", #if you want any default values
                      animate = F, grid = T,
                      hide_min_max = T, from_fixed = FALSE,
                      to_fixed = FALSE),
      
      sliderTextInput(inputId = 'X5', label = 'Quality Of Life',
                      choices = c("None", "Low", "Medium", "High"),
                      selected = "Medium", #if you want any default values
                      animate = F, grid = T,
                      hide_min_max = T, from_fixed = FALSE,
                      to_fixed = FALSE),
      
      sliderTextInput(inputId = 'X6', label = 'Crime Rating',
                      choices = c("None", "Low", "Medium", "High"),
                      selected = "Low", #if you want any default values
                      animate = F, grid = T,
                      hide_min_max = T, from_fixed = FALSE,
                      to_fixed = FALSE),
      
      sliderTextInput(inputId = 'X7', label = 'Sunny Days', 
                      choices = c("None", "Low", "Medium", "High"),
                      selected = "None", #if you want any default values 
                      animate = F, grid = T, 
                      hide_min_max = T, from_fixed = FALSE,
                      to_fixed = FALSE),
      
      sliderTextInput(inputId = 'X8', label = 'Avg. Internet Speed', 
                      choices = c("None", "Low", "Medium", "High"),
                      selected = "None", #if you want any default values 
                      animate = F, grid = T, 
                      hide_min_max = T, from_fixed = FALSE,
                      to_fixed = FALSE),
      
      sliderTextInput(inputId = 'X9', label = 'Covid-19 Deaths (per Million)',
                      choices = c("None", "Low", "Medium", "High"),
                      selected = "High", #if you want any default values
                      animate = F, grid = T,
                      hide_min_max = T, from_fixed = FALSE,
                      to_fixed = FALSE),
      
      sliderTextInput(inputId = 'X10', label = 'Covid-19 Tests (per Million)',
                      choices = c("None", "Low", "Medium", "High"),
                      selected = "High", #if you want any default values
                      animate = F, grid = T,
                      hide_min_max = T, from_fixed = FALSE,
                      to_fixed = FALSE),
      
      sliderTextInput(inputId = 'X11', label = 'Peace Index (2020)',
                      choices = c("None", "Low", "Medium", "High"),
                      selected = "Medium", #if you want any default values
                      animate = F, grid = T,
                      hide_min_max = T, from_fixed = FALSE,
                      to_fixed = FALSE),
      
      sliderTextInput(inputId = 'X12', label = 'Happiness Index',
                      choices = c("None", "Low", "Medium", "High"),
                      selected = "None", #if you want any default values
                      animate = F, grid = T,
                      hide_min_max = T, from_fixed = FALSE,
                      to_fixed = FALSE),
    ),
    
    mainPanel(
      
      h3('Cities in Global Map:'),
      leafletOutput("map_plot"),
      tableOutput("df"),
      uiOutput('link')
    )
    )

  
  # Show a plot of the generated distribution
  # mainPanel(
  #     plotOutput("distPlot")
  # )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # out = renderUI({city.search(xmat, c(input$X1, input$X2))})
  
  # output$plot <- renderPlot({
  #   
  #   out = city.search(xmat, c(input$X1, input$X2))
  #   
  #   # x1    <- faithful[, 1]
  #   # bins1 <- seq(min(x1), max(x1), length.out = input$bins1 + 1)
  #   # hist(x1, breaks = bins1, col = 'darkgray', border = 'white')
  #   
  #   plot(xmat[out$city,1], xmat[out$city,2], pch = 16,
  #                      cex = (3*out$score)/100 + 1)
  # })
  
  output$df <- renderTable({
    
    out = city.search(provide.data, 
                      c(input$X1, input$X2, input$X3, input$X4, input$X5, input$X6,
                        input$X7, input$X8, input$X9, input$X10, input$X11, input$X12))
    
    data.frame("City" = c(head(out$city, n = 5), tail(out$city, n = 5)),
               "Score" = c(head(out$score, n = 5), tail(out$score, n = 5)),
               "Latitude" = c(head(out$lat, n = 5), tail(out$lat, n = 5)),
               "Longitude" = c(head(out$lng, n = 5), tail(out$lng, n = 5)),
               "Preference" = c(rep("Preferred",5), rep("Not Preferred",5)))
  })
  
  output$map_plot = renderLeaflet({

    out = city.search(provide.data,
                      c(input$X1, input$X2, input$X3, input$X4, input$X5, input$X6,
                        input$X7, input$X8, input$X9, input$X10, input$X11, input$X12))

    orderedplaces_tag = cbind.data.frame(c(head(out$city, n = 5), tail(out$city, n = 5)),
                                         c(head(out$score, n = 5), tail(out$score, n = 5)),
                                         c(head(out$lat, n = 5), tail(out$lat, n = 5)),
                                         c(head(out$lng, n = 5), tail(out$lng, n = 5)),
                                         c(rep(1,5),rep(2,5)))
    colnames(orderedplaces_tag) <- c("places","score","lat","lon","tag")
    # first 10 quakes
    df.5_5 <- orderedplaces_tag

    getColor <- function(orderedplaces_tag) {
      sapply(orderedplaces_tag$tag, function(tag) {
        if(tag == 1) {
          "green"
        } else if(tag == 2) {
          "red"
        } })
    }

    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(df.5_5)
    )

    #leaflet(df.5_5) %>% addTiles() %>%
    #  addAwesomeMarkers(~lon, ~lat, icon=icons, label=~as.character(places))

    # # Output 2
    # df.5_5_Output2 = as.data.frame(cbind(df.5_5[1:4],c(rep("Preferred",5),rep("Not Preferred",5))))
    # colnames(df.5_5_Output2) <- c("City", "Score","Lattitude","Longitude","Preference")
    #df.5_5_Output2

    legend.palette = colorFactor(palette = c('green', 'red'),
                                 domain = c('Preferred', 'Not Preferred'),
                                 ordered = T)
    leaflet(df.5_5) %>% addTiles() %>%
      addAwesomeMarkers(~lon, ~lat, icon=icons, label=~as.character(places)) %>%
      addLegend(position = 'bottomright', pal = legend.palette,
                values = c('Preferred', 'Not Preferred'), title = 'Preference')
  })
  
  url = a('link', href = "https://www.google.com/maps")
  output$link = renderUI({tagList("URL link:", url)})
}

# Run the application 
shinyApp(ui = ui, server = server)






