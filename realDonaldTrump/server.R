library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

server <- function(input, output) {

  # Number of Tweets Per Day
  output$TweetFreq <- renderPlot({
    
    # Load tweets from Jan 2019 to present	  
    load("TrumpTweets2019.RData")
    Drumpf <- TrumpTweets2019[1:2422,]
    
    # Tweets per day
    Day <- format(Drumpf$created_at, "%y-%m-%d")
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
        scale_y_discrete(limits = seq(0,25,5), expand = expand_scale(add = 0)) +
        ggtitle("Number of Tweets Per Day")
    Febplot <- ggplot(Feb, aes(x = Day, y = Freq)) + geom_histogram(stat="identity") + 
        theme(axis.text.x = element_text(angle = 90)) + 
        scale_x_date(breaks = '1 day', date_labels = '%b %d', expand = c(0,0)) +
        scale_y_discrete(limits = seq(0,20,5), expand = expand_scale(add = 1)) +
        xlab("Date") + ylab("Number of Tweets") + ggtitle("Number of Tweets Per Day")
    Marplot <- ggplot(Mar, aes(x = Day, y = Freq)) + geom_histogram(stat="identity") + 
        theme(axis.text.x = element_text(angle = 90)) + 
        scale_x_date(breaks = '1 day', date_labels = '%b %d', expand = c(0,0)) +
        scale_y_discrete(limits = seq(0,35,5), expand = expand_scale(add = 1)) +
        xlab("Date") + ylab("Number of Tweets") + ggtitle("Number of Tweets Per Day")
    Aprplot <- ggplot(Apr, aes(x = Day, y = Freq)) + geom_histogram(stat="identity") + 
        theme(axis.text.x = element_text(angle = 90)) + 
        scale_x_date(breaks = '1 day', date_labels = '%b %d', expand = c(0,0)) +
        scale_y_discrete(limits = seq(0,40,5)) +
        xlab("Date") + ylab("Number of Tweets") + ggtitle("Number of Tweets Per Day")
    Mayplot <- ggplot(May, aes(x = Day, y = Freq)) + geom_histogram(stat="identity") + 
        theme(axis.text.x = element_text(angle = 90)) + 
        scale_x_date(breaks = '1 day', date_labels = '%b %d', expand = c(0,0)) +
        scale_y_discrete(limits = seq(0,70,10)) +
        xlab("Date") + ylab("Number of Tweets") + ggtitle("Number of Tweets Per Day")

    if (input$month == "January") 
      if (input$smoother & input$mean) {Janplot +
        geom_smooth(se = FALSE, span = input$f) +
        geom_hline(yintercept = mean(Jan$Freq), linetype="dashed", colour="red")}
      else if (input$smoother) {Janplot + geom_smooth(se = FALSE, span = input$f)}
      else if (input$mean) {Janplot + geom_hline(yintercept = mean(Jan$Freq), linetype="dashed", colour="red")}
      else {Janplot}
    else if (input$month == "February") 
      if (input$smoother & input$mean) {Febplot +
        geom_smooth(se = FALSE, span = input$f) +
        geom_hline(yintercept = mean(Feb$Freq), linetype="dashed", colour="red")}
      else if (input$smoother) {Febplot + geom_smooth(se = FALSE, span = input$f)}
      else if (input$mean) {Febplot + geom_hline(yintercept = mean(Feb$Freq), linetype="dashed", colour="red")}
      else {Febplot}
    else if (input$month == "March") 
      if (input$smoother & input$mean) {Marplot +
        geom_smooth(se = FALSE, span = input$f) +
        geom_hline(yintercept = mean(Mar$Freq), linetype="dashed", colour="red")}
      else if (input$smoother) {Marplot + geom_smooth(se = FALSE, span = input$f)}
      else if (input$mean) {Marplot + geom_hline(yintercept = mean(Mar$Freq), linetype="dashed", colour="red")}
      else {Marplot}
    else if (input$month == "April") 
      if (input$smoother & input$mean) {Aprplot +
        geom_smooth(se = FALSE, span = input$f) +
        geom_hline(yintercept = mean(Apr$Freq), linetype="dashed", colour="red")}
      else if (input$smoother) {Aprplot + geom_smooth(se = FALSE, span = input$f)}
      else if (input$mean) {Aprplot + geom_hline(yintercept = mean(Apr$Freq), linetype="dashed", colour="red")}
      else {Aprplot}
    else if (input$month == "May") 
      if (input$smoother & input$mean) {Mayplot + 
        geom_smooth(se = FALSE, span = input$f) +
        geom_hline(yintercept = mean(May$Freq), linetype="dashed", colour="red")}
      else if (input$smoother) {Mayplot + geom_smooth(se = FALSE, span = input$f)} 
      else if (input$mean) {Mayplot + geom_hline(yintercept = mean(May$Freq), linetype="dashed", colour="red")}
      else {Mayplot}
  })
 
  output$KeywordFreq <- renderPlot({
    
     # Load tweets from Jan 2019 to May 2019
     load("TrumpTweets2019.RData")
     Drumpf <- TrumpTweets2019[359:2422,]
     
     # Keyword frequency
     if (input$keyword == "Collusion"){
        subset1 <- Drumpf %>% 
          filter(str_detect(text, fixed("collusion", ignore_case=TRUE)))
        
        # Tweets per day. Prepare to merge with missing data.
        Day <- format(subset1$created_at, "%Y-%m-%d")
        DailyTweets <- as.data.frame(table(Day))
        DailyTweets$Day <- as.Date(DailyTweets$Day)
        
        # Prepare to fill in missing data in a time series.  
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
        Collusion <- ggplot(DailyTweets2, aes(x = Day, y = Freq)) + geom_line() + 
        theme(axis.text.x = element_text(angle = 90)) + 
        xlab("Date") + ylab("Number of Tweets") +
        scale_x_date(date_labels = '%b %d', expand = c(0,0)) +
        scale_y_discrete(limits = seq(0,10,2)) +
        ggtitle("Number of Tweets Per Day")
        
        if (input$smoother & input$mean) {Collusion + 
            geom_smooth(se = FALSE, span = input$f) +
            geom_hline(yintercept = mean(DailyTweets2$Freq), linetype="dashed", colour="red")}
        else if (input$smoother) {Collusion + geom_smooth(se = FALSE, span = input$f)}
        else if (input$mean) {Collusion + geom_hline(yintercept = mean(DailyTweets2$Freq), linetype="dashed", colour="red")}
        else {Collusion}
     }
     else if (input$keyword == "Fake News"){
        subset2 <- Drumpf %>% 
           filter(str_detect(text, fixed("fake news", ignore_case=TRUE))) 

        # Tweets per day. Prepare to merge with missing data.
        Day <- format(subset2$created_at, "%Y-%m-%d")
        DailyTweets <- as.data.frame(table(Day))
        DailyTweets$Day <- as.Date(DailyTweets$Day)
        
        # Prepare to fill in missing data in a time series.
        TweetDays <- seq(as.Date("2019-01-01"), as.Date("2019-05-31"), "days")    
        TweetDays <- as.data.frame(TweetDays)
        colnames(TweetDays) <- "Day"

        # Merge the tweet, quoted retweet and missing data dataset.
        # Change the missing values to 0.
        DailyTweets2 <- full_join(TweetDays, DailyTweets, by="Day") %>%
          mutate(Freq = replace_na(Freq, 0))
        
        # Make the ggplot
        FakeNews <- ggplot(DailyTweets2, aes(x = Day, y = Freq)) + geom_line() + 
        theme(axis.text.x = element_text(angle = 90)) + 
        xlab("Date") + ylab("Number of Tweets") +
        scale_x_date(date_labels = '%b %d', expand = c(0,0)) +
        scale_y_discrete(limits = seq(0,6,1)) +
        ggtitle("Number of Tweets Per Day")
        
        if (input$smoother & input$mean) {FakeNews + 
            geom_smooth(se = FALSE, span = input$f) +
            geom_hline(yintercept = mean(DailyTweets2$Freq), linetype="dashed", colour="red")}
        else if (input$smoother) {FakeNews + geom_smooth(se = FALSE, span = input$f)}
        else if (input$mean) {FakeNews + geom_hline(yintercept = mean(DailyTweets2$Freq), linetype="dashed", colour="red")}
        else {FakeNews}
     }    
     else if (input$keyword == "Mueller"){
        subset3 <- Drumpf %>% 
           filter(str_detect(text, fixed("mueller", ignore_case=TRUE)))
        
        # Tweets per day. Prepare to merge with missing data.
        Day <- format(subset3$created_at, "%Y-%m-%d")
        DailyTweets <- as.data.frame(table(Day))
        DailyTweets$Day <- as.Date(DailyTweets$Day)
        
        # Prepare to fill in missing data in a time series.
        TweetDays <- seq(as.Date("2019-01-01"), as.Date("2019-05-31"), "days")    
        TweetDays <- as.data.frame(TweetDays)
        colnames(TweetDays) <- "Day"
        
        # Merge the tweet, quoted retweet and missing data dataset.
        # Change the missing values to 0.
        DailyTweets2 <- full_join(TweetDays, DailyTweets, by="Day") %>%
          mutate(Freq = replace_na(Freq, 0))
        
        # Make the ggplot
        Mueller <- ggplot(DailyTweets2, aes(x = Day, y = Freq)) + geom_line() + 
        theme(axis.text.x = element_text(angle = 90)) + 
        xlab("Date") + ylab("Number of Tweets") + 
        scale_x_date(date_labels = '%b %d', expand = c(0,0)) +
        scale_y_discrete(limits = seq(0,14,2)) +
        ggtitle("Number of Tweets Per Day")
        
        if (input$smoother & input$mean) {Mueller + 
            geom_smooth(se = FALSE, span = input$f) +
            geom_hline(yintercept = mean(DailyTweets2$Freq), linetype="dashed", colour="red")}
        else if (input$smoother) {Mueller + geom_smooth(se = FALSE, span = input$f)}
        else if (input$mean) {Mueller + geom_hline(yintercept = mean(DailyTweets2$Freq), linetype="dashed", colour="red")}
        else {Mueller}
     }
     else if (input$keyword == "Witch Hunt"){
       subset4 <- Drumpf %>% 
         filter(str_detect(text, fixed("witch hunt", ignore_case=TRUE)))
       
       # Frequency of tweets per day.
       Day <- format(subset4$created_at, "%Y-%m-%d")
       DailyTweets <- as.data.frame(table(Day))
       DailyTweets$Day <- as.Date(DailyTweets$Day)
       
       # Prepare to fill in missing data in a time series.
       TweetDays <- seq(as.Date("2019-01-01"), as.Date("2019-05-31"), "days")    
       TweetDays <- as.data.frame(TweetDays)
       colnames(TweetDays) <- "Day"
       
       # Merge the tweet, quoted retweet and missing data dataset.
       # Change the missing values to 0.
       DailyTweets2 <- full_join(TweetDays, DailyTweets, by="Day") %>%
         mutate(Freq = replace_na(Freq, 0))
       
       # Make the ggplot
       WitchHunt <- ggplot(DailyTweets2, aes(x = Day, y = Freq)) + geom_line() + 
         theme(axis.text.x = element_text(angle = 90)) + 
         xlab("Date") + ylab("Number of Tweets") + 
         scale_x_date(date_labels = '%b %d', expand = c(0,0)) +
         scale_y_discrete(limits = seq(0,5,1)) +
         ggtitle("Number of Tweets Per Day")  
       
       if (input$smoother & input$mean) {WitchHunt + 
           geom_smooth(se = FALSE, span = input$f) +
           geom_hline(yintercept = mean(DailyTweets2$Freq), linetype="dashed", colour="red")}
       else if (input$smoother) {WitchHunt + geom_smooth(se = FALSE, span = input$f)}
       else if (input$mean) {WitchHunt + geom_hline(yintercept = mean(DailyTweets2$Freq), linetype="dashed", colour="red")}
       else {WitchHunt}
     }
  })

  output$TweetDesc <- renderUI({
    str1 <- paste0("Donald Trump tweeted 361 times in January (an average of 11.65 times per day), 
       237 times in February (average 8.46 per day), 394 times in March (average 13.13 per day), 
       417 times in April (average 13.90 per day) and 655 times in May (average 21.13 per day).")
    HTML(paste(str1, sep = '</br>'))
  })
  
  output$KeywordDesc <- renderUI({
    str3 <- paste0("From January 2019 - May 2019, Donald Trump said \"Collusion\" 140 times, \"Fake News\" 83 times,
		   \"Mueller\" 124 times and \"Witch Hunt\" 44 times.", "</br>", "</br>",
       "Notably,", "</br>",
       "- On Apr 22, he tweeted \"Collusion\" 9 times and \"Mueller\" 11 times.", "</br>",
       "- On May 11, he tweeted \"Collusion\" 10 times and \"Mueller\" 14 times.", "</br>",
       "- On May 22, he tweeted \"Mueller\" 9 times and \"Witch Hunt\" 5 times.", "</br>",
       "- He also tweeted \"Fake News\" 6 times on January 19.")
    HTML(paste(str3, sep = '<br/>'))
  })
  
  output$Name <- renderUI({
    str4 <- paste0("Author: ", "<b>", "Isaac Yu", "</b>", "</br>")
    HTML(paste(str4, sep = '<br/>'))
  })
  
  output$About <- renderUI({
    str5 <- paste0("</br>", "I graduated from Simon Fraser University with a major in sociology 
                   and a minor in statistics.", "</br>")
    str6 <- ("This Shiny App gathers tweets from \"Tweeter in Chief\" Donald Trump's account.  
          It displays the number of tweets per day over each month.  It also displays 
          the frequency of certain key words, such as \"Collusion\", \"Fake News\", \"Mueller\" and \"Witch Hunt\".")
    HTML(paste(str5, str6, sep = '<br/>'))
  })
}
