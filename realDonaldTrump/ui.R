library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Donald Trump 2019 Twitter Data"),
    
  sidebarLayout(
    sidebarPanel(
      
      # Smooth Line
      checkboxInput("smoother", strong("Add smooth trend line?"), FALSE),
      conditionalPanel(condition = "input.smoother == true",
                       sliderInput(inputId = "f", label = "Smoother span:",
                                   min = 0.20, max = 2, value = 0.75, step = 0.01,
                                   animate = animationOptions(interval = 100)),
                       HTML("Higher values give more smoothness.")
      ),
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
                                      choices = c("Collusion","Fake News","Mueller","Witch Hunt"),
                                      selected = "Collusion"),
                          plotOutput("KeywordFreq"),
			                    htmlOutput("KeywordDesc")), 
                 
                 tabPanel("About", 
                          htmlOutput("Name"),
                          tags$a(href = "https://www.linkedin.com/in/isaac-yu-430a3314b/", 
                                 "Linkedin", target = "_blank"),
                          br(),
                          tags$img(src='Shiny_Photo.png', align ='center'),
                          htmlOutput("About")
                          )
   ))
  )
)
