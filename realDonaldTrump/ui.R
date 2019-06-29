library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Politicians' 2019 Twitter Data"),
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
                          selectInput(inputId = "month", label = strong("Month:"),
                                      choices = c("January","February","March","April",
                                                  "May"),
                                      selected = "January"),
                          plotOutput("TweetFreq"),
                          htmlOutput("TweetDesc")),
                
                 tabPanel("Keywords per Day",
                          selectInput(inputId = "keyword", label = strong("Keyword:"),
                                      choices = c("Economy","Jobs"),
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
