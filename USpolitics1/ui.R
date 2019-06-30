library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Trump vs Sanders: 2019 Twitter Data"),
  radioButtons("candidate", "",
               c("Donald Trump" = "Drumpf",
                 "Bernie Sanders" = "Birdie")),
    
  sidebarLayout(
    sidebarPanel(
      
      # Horizontal Average Line
      checkboxInput("mean", strong("Add horizontal average line?"), FALSE)
    ),
  
    # Tags
    mainPanel(
      tabsetPanel(type = "tabs",
                 tabPanel("Tweets per Day", 
                          sliderInput("month", "Month:", value = as.Date("2019-01-01"), 
                                    min = as.Date("2019-01-01"), max = as.Date("2019-05-01"), timeFormat = "%b %Y"),
                          plotOutput("TweetFreq"),
                          htmlOutput("TweetDesc")),
                
                 tabPanel("Keywords per Day",
                          selectInput(inputId = "keyword", label = strong("Keyword:"),
                                      choices = c("Economy","Economic","Jobs","Justice","Trade"),
                                      selected = "Economy"),
                          plotOutput("KeywordFreq"),
			                    htmlOutput("KeywordDesc")), 
                 
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
)
