library(leaflet)
library(shinycssloaders)

navbarPage("Food League", id="nav",
  tabPanel("Interactive map",
    div(class="outer",
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      #Show map
      leafletOutput("map", width="100%", height="100%"),
      
      #Create Input Panel that fades in/ out
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                    width = 290, height = "auto",
                    h2("Bike Collision explorer"),
                    selectInput("color", "Color Variable", c("Cyclist Sex", "Cyclist Direction"), selected = "Cyclist Sex"),
                    
                    withSpinner(plotOutput("hist1", height = 200)),
                    withSpinner(plotOutput("hist2", height = 250))
      )
    )
  ),
  navbarMenu("Data Analysis",
  tabPanel("Data explorer",
           tags$head(
             # Include our custom CSS
             includeCSS("styles.css"),
             includeScript("gomap.js")
           ),
           fluidRow(
             column(3,
                    selectInput("terrain", "Terrain", c("All terrains"="", structure(unique(bikemaps$terrain), names = c("Uphill", "Flat", "Downhill"))), multiple=TRUE)
                    ),
             column(3,
                    selectInput("sex", "Sex", 
                                c("All sexes"="", structure(unique(bikemaps$sex),names = c("M", "F", "Other"))), multiple=TRUE)
                    )
           ),
           fluidRow(
             column(3,
                    numericInput("maxYear", "Youngest Birth Year", min=1900, max=2020, value=2018)
             ),
             column(3,
                    numericInput("minYear", "Oldest Birth Year", min=1900, max=2020, value=1918)
             )
           ),
           hr(),
           DT::dataTableOutput("biketable")
  ),
  tabPanel("About the Developers",
           tags$head(
             # Include our custom CSS
             includeCSS("styles.css"),
             includeScript("gomap.js"))
           )),
  conditionalPanel("false", icon("crosshair")))
