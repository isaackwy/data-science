library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

server <- function(input, output) {

  # Number of Tweets Per Day
  output$TweetFreq <- renderPlot({
    
    # Load either Donald Trump or Bernie Sanders' twitter data
    # Tweets are from January 2019 to May 2019
    if(input$candidate == "Drumpf") {
      load("Drumpf2019.RData")
      Tweets_df <- Drumpf2019[447:2510,]
    }
    else if(input$candidate == "Birdie") {
      load("Birdie2019.RData")
      Tweets_df <- Birdie2019[520:1662,]
    }
    else {
      break
    }
    
    # Tweets per day
    Day <- format(Tweets_df$created_at, "%y-%m-%d")
    DailyTweets <- as.data.frame(table(Day))
    
    # Tweets per day over each month
    DailyTweets$Day <- as.Date(DailyTweets$Day)
    Month <- format(DailyTweets$Day, "%y-%m")
    MonthDay <- cbind(Month, DailyTweets)
    
    # Filter daily number of tweets tweets by month
    Jan <- filter(MonthDay, Month == "19-01")
    Feb <- filter(MonthDay, Month == "19-02")
    Mar <- filter(MonthDay, Month == "19-03")
    Apr <- filter(MonthDay, Month == "19-04")
    May <- filter(MonthDay, Month == "19-05")
    
    # Monthly graphs
    Janplot <- ggplot(Jan, aes(x = Day, y = Freq)) + geom_histogram(stat="identity") + 
        theme(axis.text.x = element_text(angle = 90)) + 
        xlab("Date") + ylab("Number of Tweets") + 
        scale_x_date(breaks = '1 day', date_labels = '%b %d', expand = c(0,0)) +
        ggtitle("Number of Tweets Per Day")
    Febplot <- ggplot(Feb, aes(x = Day, y = Freq)) + geom_histogram(stat="identity") + 
        theme(axis.text.x = element_text(angle = 90)) + 
        scale_x_date(breaks = '1 day', date_labels = '%b %d', expand = c(0,0)) +
        xlab("Date") + ylab("Number of Tweets") + ggtitle("Number of Tweets Per Day")
    Marplot <- ggplot(Mar, aes(x = Day, y = Freq)) + geom_histogram(stat="identity") + 
        theme(axis.text.x = element_text(angle = 90)) + 
        scale_x_date(breaks = '1 day', date_labels = '%b %d', expand = c(0,0)) +
        xlab("Date") + ylab("Number of Tweets") + ggtitle("Number of Tweets Per Day")
    Aprplot <- ggplot(Apr, aes(x = Day, y = Freq)) + geom_histogram(stat="identity") + 
        theme(axis.text.x = element_text(angle = 90)) + 
        scale_x_date(breaks = '1 day', date_labels = '%b %d', expand = c(0,0)) +
        scale_y_discrete(limits = seq(0,40,5)) +
        xlab("Date") + ylab("Number of Tweets") + ggtitle("Number of Tweets Per Day")
    Mayplot <- ggplot(May, aes(x = Day, y = Freq)) + geom_histogram(stat="identity") + 
        theme(axis.text.x = element_text(angle = 90)) + 
        scale_x_date(breaks = '1 day', date_labels = '%b %d', expand = c(0,0)) +
        xlab("Date") + ylab("Number of Tweets") + ggtitle("Number of Tweets Per Day")
    
    # If Donald Trump tweets are selected
    if (exists("Drumpf2019")) {
      Janplot <- Janplot + scale_y_discrete(limits = seq(0,25,5), expand = expand_scale(add = 0))
      Febplot <- Febplot + scale_y_discrete(limits = seq(0,20,5), expand = expand_scale(add = 1))
      Marplot <- Marplot + scale_y_discrete(limits = seq(0,35,5), expand = expand_scale(add = 1))
      Aprplot <- Aprplot + scale_y_discrete(limits = seq(0,40,5))
      Mayplot <- Mayplot + scale_y_discrete(limits = seq(0,70,10))
    }
    # Else if Bernie Sanders tweets are selected
    else if (exists("Birdie2019")) {
      Janplot <- Janplot + scale_y_discrete(limits = seq(0,8,1), expand = expand_scale(add = 0))
      Febplot <- Febplot + scale_y_discrete(limits = seq(0,25,5), expand = expand_scale(add = 1))
      Marplot <- Marplot + scale_y_discrete(limits = seq(0,45,5), expand = expand_scale(add = 2))
      Aprplot <- Aprplot + scale_y_discrete(limits = seq(0,40,5), expand = expand_scale(add = 2))
      Mayplot <- Mayplot + scale_y_discrete(limits = seq(0,25,5))
    }    
    
    # If horizontal average line
    if (input$mean) {
      if (input$month == "January")  {
        Janplot + geom_hline(yintercept = mean(Jan$Freq), linetype="dashed", colour="red")
      }
      else if (input$month == "February") {
        Febplot + geom_hline(yintercept = mean(Feb$Freq), linetype="dashed", colour="red")
      }
      else if (input$month == "March") {
        Marplot + geom_hline(yintercept = mean(Mar$Freq), linetype="dashed", colour="red")
      }
      else if (input$month == "April") {
        Aprplot + geom_hline(yintercept = mean(Apr$Freq), linetype="dashed", colour="red")
      }
      else if (input$month == "May") {
        Mayplot + geom_hline(yintercept = mean(May$Freq), linetype="dashed", colour="red")
      }
    }
    # Else (no average line)
    else {
      if (input$month == "January") {Janplot}
      else if (input$month == "February") {Febplot}
      else if (input$month == "March") {Marplot}
      else if (input$month == "April") {Aprplot}
      else if (input$month == "May") {Mayplot}
    }
  })

  output$KeywordFreq <- renderPlot({
    
    # Load either Donald Trump or Bernie Sanders' twitter data
    # Tweets are from January 2019 to May 2019
    if(input$candidate == "Drumpf") {
      load("Drumpf2019.RData")
      Tweets_df <- Drumpf2019[447:2510,]
    }
    else if(input$candidate == "Birdie") {
      load("Birdie2019.RData")
      Tweets_df <- Birdie2019[520:1662,]
    }
    else {
      break
    }
    
    # Keyword frequency
    if (input$keyword == "Economy") {
      tw_subset <- Tweets_df %>% 
        filter(str_detect(text, fixed("economy", ignore_case=TRUE)))
    }
    else if (input$keyword == "Jobs") {
      tw_subset <- Tweets_df %>% 
        filter(str_detect(text, fixed("jobs", ignore_case=TRUE)))
    }
    
    # Tweets per day. Prepare to merge with missing data.
    Day <- format(tw_subset$created_at, "%Y-%m-%d")
    DailyTweets <- as.data.frame(table(Day))
    DailyTweets$Day <- as.Date(DailyTweets$Day)
    
    # Prepare to fill in missing data in a time series. References:  
    # https://stackoverflow.com/questions/6058677/how-to-create-na-for-missing-data-in-a-time-series
    # https://stackoverflow.com/questions/8161836/how-do-i-replace-na-values-with-zeros-in-an-r-dataframe
    TweetDays <- seq(as.Date("2019-01-01"), as.Date("2019-05-31"), "days")    
    TweetDays <- as.data.frame(TweetDays)
    colnames(TweetDays) <- "Day"
    
    # Merge the tweet, quoted retweet and missing data dataset.
    # Change the missing values to 0.
    DailyTweets2 <- full_join(TweetDays, DailyTweets, by="Day") %>%
      mutate(Freq = replace_na(Freq, 0))
    
    # Make the ggplot
    KeywordPlot <- ggplot(DailyTweets2, aes(x = Day, y = Freq)) + geom_line() + 
      theme(axis.text.x = element_text(angle = 90)) + 
      xlab("Date") + ylab("Number of Tweets") +
      scale_x_date(date_labels = '%b %d', expand = c(0,0)) +
      ggtitle("Number of Tweets Per Day")
    
    # Fix y-axis labels
    if (input$candidate =="Birdie" && input$keyword == "Economy") {
      KeywordPlot <- KeywordPlot + scale_y_discrete(limits = seq(0,2,1), expand = expand_scale(add = 0.1))
    }
    
    if (input$mean) {KeywordPlot + geom_hline(yintercept = mean(DailyTweets2$Freq), linetype="dashed", colour="red")}
    else {KeywordPlot}
    
  })
  
  output$TweetDesc <- renderUI({
    str1 <- paste0("Donald Trump tweeted 361 times in January (an average of 11.65 times per day), 
       237 times in February (average 8.46 per day), 394 times in March (average 13.13 per day), 
       417 times in April (average 13.90 per day) and 655 times in May (average 21.13 per day).")
    str2 <- paste0("By contrast, Bernie Sanders tweeted 71 times in January (an average of 3.38 times per day), 
       136 times in February (average 6.47 per day), 302 times in March (average 9.74 per day), 
       322 times in April (average 10.73 per day) and 312 times in May (average 10.06 per day).")
    HTML(paste(str1, str2, sep = '</br></br>'))
  })
  
  output$KeywordDesc <- renderUI({
    str3 <- paste0("From January 2019 - May 2019, Donald Trump tweeted \"Economy\" 68 times and 
                   \"Jobs\" 44 times.")
    str4 <- paste0 ("By contrast, Bernie Sanders tweeted \"Economy\" 28 times and
                   \"Jobs\" 47 times.")
    HTML(paste(str3, str4, sep = '</br></br>'))
  })
  
  output$Name <- renderUI({
    str5 <- paste0("Author: ", "<b>", "Isaac Yu", "</b>", "</br>")
    HTML(paste(str5, sep = '<br/>'))
  })
  
  output$About <- renderUI({
    str6 <- paste0("</br>", "I graduated from Simon Fraser University with a major in sociology 
                   and a minor in statistics.", "</br>")
    str7 <- ("This Shiny App gathers tweets from Donald Trump and Bernie Sanders' accounts.  
          It displays the number of tweets per day over each month.  It also displays 
          the frequency of certain key words, such as \"Economy\".")
    HTML(paste(str6, str7, sep = '<br/>'))
  })
}
