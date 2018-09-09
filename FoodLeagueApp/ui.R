 library(leaflet)
library(shinycssloaders)
library(shiny)

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
                    selectInput("color", "Color Variable", c("Collision Cluster", "Injury Severity"), selected = "Cyclist Sex"),
                    
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
                    selectInput("terrain", "Terrain", c("All terrains"="", "Uphill" = "Uphill", "Flat" = "Flat", "Downhill" = "Downhill"), multiple=TRUE)
                    ),
             column(3,
                    selectInput("sex", "Sex",  c("All sexes"="", "Other" = "Other", "Male" = "M", "Female" = "F"), multiple=TRUE)
                    ),
             column(3,
                    selectInput("direction", "Direction",  c("All directions"="", "North" ="N", "South" = "S", "East" = "E", "West" = "W"), multiple=TRUE)
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
  tabPanel("Additional Plots",
           tags$head(
             # Include our custom CSS
             includeCSS("styles.css"),
             includeScript("gomap.js"))
           )),
  tabPanel("About the Developers",
           tags$head(
             # Include our custom CSS
             includeCSS("styles.css"),
             includeScript("gomap.js")
           ),

           fluidRow( # Picture Row
             column(4, align = "center",
                    img(src='Kristen.jpg', align = "center", height = '200px', width = '200px')),
             column(4, align = "center",
                    img(src='ZhiYuh.jpg', align = "center", height = '200px', width = '200px')),
             column(4, align = "center",
                    img(src='Alice.png', align = "center", height = '200px', width = '200px'))
           ),
           fluidRow( # Name Row
             column(4, align = "center",
                    h3("Kristen Bystrom")),
             column(4, align = "center",
                    h3("Zhi Yuh Ou Yang")),
             column(4, align = "center",
                    h3("Alice Roberts"))
           ),
           fluidRow( # Job Title
             column(4, align = "center",
                    h6("Data Scientist/ Web Developer")),
             column(4, align = "center",
                    h6("Data Scientist/ Data Engineer")),
             column(4, align = "center",
                    h6("Data Scientist/ Visualization Lead"))
           ),
           fluidRow( # green bar
             column(4, align = "center",
                    img(src='greenbar.png', align = "center", height = "15px", width = "100%")),
             column(4, align = "center",
                    img(src='greenbar.png', align = "center", height = "15px", width = "100%")),
             column(4, align = "center",
                    img(src='greenbar.png', align = "center", height = "15px", width = "100%"))
           ),
           fluidRow( # Bio
             column(4, align = "center",
                    h4("Kristen is a 5th year statistics and computing science student at Simon Fraser University. Her research collides the world of social media marketing with data science and statistics. She is the president of the SFU Statistics and Actuarial Science Student Association and in her free time enjoys seeing live theatre and playing board games.")),
             column(4, align = "center",
                    h4("Zhi Yuh is a 4th year statistics and economics student from Simon Fraser University. He has been involved in weather, health, and language statistics reasearch projects. You may have seen him present at the Joint Statistical Meeting this year. He loves badminton and the rainy Vancouver weather.")),
             column(4, align = "center",
                    h4("Alice Roberts is a 4th year applied mathematics student with a minor in economics from Simon Fraser University. She is passionate about finding cutting edge mathematical tools that she can use for applications in data science and finance. Her hobbies include volleyball, dance, shopping and hiking."))
           ),
           fluidRow( # green bar
             column(4, align = "center",
                    img(src='greenbar.png', align = "center", height = "15px", width = "100%")),
             column(4, align = "center",
                    img(src='greenbar.png', align = "center", height = "15px", width = "100%")),
             column(4, align = "center",
                    img(src='greenbar.png', align = "center", height = "15px", width = "100%"))
           )
           ),
  conditionalPanel("false", icon("crosshair"))
)