library(shiny)

ui <- fluidPage(
  
  titlePanel("Seattle Bike Share Data"),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Daily Distribution of Bike Trips", 
                         dateInput("Day", "Date:", value = "2014-10-13", min = "2014-10-13", max = "2016-12-31"), 
                         plotOutput("BikeFreq"),
		                     htmlOutput("BikeDesc")),
                
                tabPanel("Distribution of Bikes within Stations",
                         selectInput(inputId = "region", label = strong('Region:'),
                                     choices = c("Belltown (BT)" = "BT",
                                                 "Central Business District (CBD)" = "CBD",
                                                 "Central District (CD)" = "CD",
                                                 "Capitol Hill (CH)" = "CH",
                                                 "Eastlake (EL)" = "EL",
                                                 "First Hill (FH)" = "FH",
                                                 "International District (ID)" = "ID",
                                                 "Pioneer Square (PS)" = "PS",
                                                 "South Lake Union (SLU)" = "SLU",
                                                 "University District (UD)" = "UD",
                                                 "University of Washington (UW)" = "UW",
                                                 "Waterfront (WF)" = "WF"),
                                     selected = "BT"),
                         plotOutput("StationPlot"),
                         textOutput("StationDesc")),
                         
                tabPanel("About", 
                         htmlOutput("Name"),
                         htmlOutput("About"),
                         br(),
                         tags$img(src='Shiny_Photo.png', align ='center'),
                         br(),
                         tags$a(href = "https://github.com/isaackwy",
                                "Github", target = "_blank"),
                         br(),
                         tags$a(href = "https://www.linkedin.com/in/isaac-yu-430a3314b/", 
                                "Linkedin", target = "_blank")
                )
    ))
)
