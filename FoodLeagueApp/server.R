<<<<<<< HEAD
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(shiny)


function(input, output, session) {
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  #Create first vis in Menu
  output$hist1 <- renderPlot({
    hist(rnorm(1000000),
         breaks = 50,
         main = "Normal Distribution Histogram",
         xlab = "Value",
         ylab = "Frequency",
         col = '#00DD00',
         border = 'white')
    })
  output$hist2 <- renderPlot({ 
    hist(runif(1000000),
         breaks = 50,
         main = "Uniform Distribution Histogram",
         xlab = "Value",
         ylab = "Frequency",
         col = '#00DD00',
         border = 'white')
    })
  
  output$biketable <- DT::renderDataTable({
    df <- filter(cleantable,
          Age >= input$minYear,
          Age <= input$maxYear,
          is.null(input$terrain) | Terrain %in% input$terrain,
          is.null(input$sex) | Sex %in% input$sex
       ) 
    #   mutate(Action = paste('<a class="go-map" href="" data-lat="', Latitude, '" data-long="', Longitude, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    #action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, escape = FALSE)
  })
  
  
  
}
=======

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })

})
>>>>>>> 4a5f4a2fa7cd5cf20ec062af290c9ef2d81947dc
