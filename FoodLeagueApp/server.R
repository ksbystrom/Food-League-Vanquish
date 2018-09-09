library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(shiny)
library(RColorBrewer)

function(input, output, session) {
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -123.0, lat = 49.24, zoom = 11)
  })
  
  #Create first vis in Menu
  output$hist1 <- renderPlot({
    hist(rnorm(10000),
         breaks = 20,
         main = "Normal Distribution Histogram",
         xlab = "Value",
         ylab = "Frequency",
         col = '#00DD00',
         border = 'white')
    })
  output$hist2 <- renderPlot({ 
    hist(runif(10000),
         breaks = 20,
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
          is.null(input$sex) | Sex %in% input$sex,
          is.null(input$direction) | Direction %in% input$direction
       ) 
    #   mutate(Action = paste('<a class="go-map" href="" data-lat="', Latitude, '" data-long="', Longitude, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    #action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, escape = FALSE)
  })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
 observe({
   colorBy <- input$color
   colorData <- bikemaps[[colorBy]]
   pal <- colorBin("Greens", 1:15, pretty = TRUE)
   leafletProxy("map", data = bikemaps) %>% 
     clearShapes() %>%
     addCircles(~longitude, ~latitude, radius=300,
                stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
     addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
              layerId="colorLegend")
  })

  
  
}