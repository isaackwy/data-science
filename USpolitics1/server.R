library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# References:
# Filling missing data in a time series:
# https://stackoverflow.com/questions/6058677/how-to-create-na-for-missing-data-in-a-time-series
# https://stackoverflow.com/questions/8161836/how-do-i-replace-na-values-with-zeros-in-an-r-dataframe

server <- function(input, output) {

  # Number of Tweets Per Day
  output$TweetFreq <- renderPlot({
    
    # Load Donald Trump or Bernie Sanders' twitter data
    # Tweets are from January 2019 to June 2019
    if(input$candidate == "Trump") {
      load("Trump2019.RData")
      Tweets_df <- Trump2019[18:2535,]
    }
    else if(input$candidate == "Sanders") {
      load("Sanders2019.RData")
      Tweets_df <- Sanders2019[7:1681,]
    }
    else {
      break
    }
    
    # Tweets per day
    Day <- format(Tweets_df$created_at, "%Y-%m-%d")
    DailyTweets <- as.data.frame(table(Day))
    DailyTweets$Day <- as.Date(DailyTweets$Day)
    
    # Prepare to fill in missing data in a time series. References:
    TweetDays <- seq(as.Date("2019-01-01"), as.Date("2019-06-30"), "days")    
    TweetDays <- as.data.frame(TweetDays)
    colnames(TweetDays) <- "Day"
    
    # Merge the tweet, quoted retweet and missing data dataset.
    # Change the missing values to 0.
    DailyTweets2 <- full_join(TweetDays, DailyTweets, by="Day") %>%
      mutate(Freq = replace_na(Freq, 0))
    
    # Tweets per day over each month
    DailyTweets2$Day <- as.Date(DailyTweets2$Day)
    Month <- format(DailyTweets2$Day, "%Y-%m")
    MonthDay <- cbind(Month, DailyTweets2)
    
    # Filter daily number of tweets tweets by month
    UserMonth <- as.character(format(input$month, format="%Y-%m"))
    Monthly <- filter(MonthDay, Month == UserMonth)
    
    # Monthly graphs
    Freq_plot <- ggplot(Monthly, aes(x = Day, y = Freq)) + geom_histogram(stat="identity") + 
        theme(axis.text.x = element_text(angle = 90)) + 
        xlab("Date") + ylab("Number of Tweets") + 
        scale_x_date(breaks = '1 day', date_labels = '%b %d', expand = c(0,0)) +
        ggtitle("Number of Tweets Per Day")
    
    # Fix y-axis labels
    # If Trump
    if (input$candidate == "Trump") {
      if (UserMonth == "2019-01") {
        Freq_plot <- Freq_plot + scale_y_discrete(limits = seq(0,25,5), expand = expand_scale(add = 0))
      }
      else if (UserMonth == "2019-02") {
        Freq_plot <- Freq_plot + scale_y_discrete(limits = seq(0,20,5), expand = expand_scale(add = 1))
      }
      else if (UserMonth == "2019-03") {
        Freq_plot <- Freq_plot + scale_y_discrete(limits = seq(0,35,5), expand = expand_scale(add = 1))
      }
      else if (UserMonth == "2019-04") {
        Freq_plot <- Freq_plot + scale_y_discrete(limits = seq(0,40,5))
      }
      else if (UserMonth == "2019-05") {
        Freq_plot <- Freq_plot + scale_y_discrete(limits = seq(0,70,10))
      }
      else if (UserMonth == "2019-06") {
        Freq_plot <- Freq_plot + scale_y_discrete(limits = seq(0,40,5))
      }
    }
    # Else if Bernie
    if (input$candidate == "Sanders") {
      if (UserMonth == "2019-03") {
        Freq_plot <- Freq_plot + scale_y_discrete(limits = seq(0,45,5), expand = expand_scale(add = 2))
      }
      else if (UserMonth == "2019-04") {
        Freq_plot <- Freq_plot + scale_y_discrete(limits = seq(0,40,5), expand = expand_scale(add = 2))
      }
      else if (UserMonth == "2019-06") {
        Freq_plot <- Freq_plot + scale_y_discrete(limits = seq(0,50,5))
      }
    }
    
    # If horizontal average line
    if (input$mean) {
        Freq_plot + geom_hline(yintercept = mean(Monthly$Freq), linetype="dashed", colour="red")
    }
    # Else (no average line)
    else {
        Freq_plot
    }
  })

  output$KeywordFreq <- renderPlot({
    
    # Load Donald Trump or Bernie Sanders' twitter data
    # Tweets are from January 2019 to June 2019
    if(input$candidate == "Trump") {
      load("Trump2019.RData")
      Tweets_df <- Trump2019[18:2535,]
    }
    else if(input$candidate == "Sanders") {
      load("Sanders2019.RData")
      Tweets_df <- Sanders2019[7:1681,]
    }
    else {
      break
    }
    
    # Keyword frequency
    if (input$keyword == "Economy") {
      tw_subset <- Tweets_df %>%
        filter(str_detect(text, fixed("economy", ignore_case=TRUE)))
    }
    else if (input$keyword == "Economic") {
      tw_subset <- Tweets_df %>%
        filter(str_detect(text, fixed("economic", ignore_case=TRUE)))
    }    
    else if (input$keyword == "Jobs") {
      tw_subset <- Tweets_df %>% 
        filter(str_detect(text, fixed("jobs", ignore_case=TRUE)))
    }
    else if (input$keyword == "Justice") {
      tw_subset <- Tweets_df %>% 
        filter(str_detect(text, fixed("justice", ignore_case=TRUE)))
    }
    else if (input$keyword == "Trade") {
      tw_subset <- Tweets_df %>% 
        filter(str_detect(text, fixed("trade", ignore_case=TRUE)))
    }
    
    # Tweets per day. Prepare to merge with missing data.
    Day <- format(tw_subset$created_at, "%Y-%m-%d")
    DailyTweets <- as.data.frame(table(Day))
    DailyTweets$Day <- as.Date(DailyTweets$Day)
    
    # Prepare to fill in missing data in a time series. 
    TweetDays <- seq(as.Date("2019-01-01"), as.Date("2019-06-30"), "days")    
    TweetDays <- as.data.frame(TweetDays)
    colnames(TweetDays) <- "Day"
    
    # Merge the tweet, quoted retweet and missing data dataset.
    # Change the missing values to 0.
    DailyTweets2 <- full_join(TweetDays, DailyTweets, by="Day") %>%
      mutate(Freq = replace_na(Freq, 0))
    
    # Make the ggplot
    Keyword_plot <- ggplot(DailyTweets2, aes(x = Day, y = Freq)) + geom_line() + 
      theme(axis.text.x = element_text(angle = 90)) + 
      xlab("Date") + ylab("Number of Tweets") +
      scale_x_date(date_labels = '%b %Y', expand = c(0,0)) +
      ggtitle("Number of Tweets Per Day")
    
    # Fix y-axis labels
    # If Trump
    if (input$candidate == "Trump") {
      if (input$keyword == "Economic") {
        Keyword_plot <- Keyword_plot + scale_y_discrete(limits = seq(0,2,1), expand = expand_scale(add = 0.1))
      }
      else if (input$keyword == "Trade") {
        Keyword_plot <- Keyword_plot + scale_y_discrete(limits = seq(0,2,1), expand = expand_scale(add = 0.1))
      }
    }
    # Else if Sanders
    else if (input$candidate == "Sanders") {
      if (input$keyword == "Economy") {
        Keyword_plot <- Keyword_plot + scale_y_discrete(limits = seq(0,2,1), expand = expand_scale(add = 0.1))
      }
      else if (input$keyword == "Economic") {
        Keyword_plot <- Keyword_plot + scale_y_discrete(limits = seq(0,12,2))
      }
    }
    
    if (input$mean) {Keyword_plot + geom_hline(yintercept = mean(DailyTweets2$Freq), linetype="dashed", colour="red")}
    else {Keyword_plot}
    
  })
  
  output$TweetDesc <- renderText({
    UserMonth <- as.character(format(input$month, format="%Y-%m"))
    if (input$candidate == "Trump") {
      if (UserMonth == "2019-01") {
        paste("Donald Trump tweeted 361 times in January (an average of 11.65 times per day).")
      }
      else if (UserMonth == "2019-02") {
        paste("Donald Trump tweeted 237 times in February (an average of 8.46 times per day).")
      }
      else if (UserMonth == "2019-03") {
        paste("Donald Trump tweeted 394 times in March (an average of 13.13 times per day).")
      }
      else if (UserMonth == "2019-04") {
        paste("Donald Trump tweeted 417 times in April (an average of 13.90 times per day).")
      }
      else if (UserMonth == "2019-05") {
        paste("Donald Trump tweeted 655 times in May (an average of 21.13 times per day).")
      }
      else if (UserMonth == "2019-06") {
        paste("Donald Trump tweeted 454 times in June (an average of 15.13 times per day).")
      }
    }
    else if (input$candidate == "Sanders") {
      if (UserMonth == "2019-01") {
        paste("Bernie Sanders tweeted 72 times in January (an average of 2.32 times per day).")
      }
      else if (UserMonth == "2019-02") {
        paste("Bernie Sanders tweeted 136 times in February (an average of 4.86 times per day).")
      }
      else if (UserMonth == "2019-03") {
        paste("Bernie Sanders tweeted 302 times in March (an average of 9.74 times per day).")
      }
      else if (UserMonth == "2019-04") {
        paste("Bernie Sanders tweeted 322 times in April (an average of 10.73 times per day).")
      }
      else if (UserMonth == "2019-05") {
        paste("Bernie Sanders tweeted 312 times in May (an average of 10.06 times per day).")
      }
      else if (UserMonth == "2019-06") {
        paste("Bernie Sanders tweeted 531 times in June (an average of 17.70 times per day).")
      }
    }
  })
  
  output$KeywordDesc <- renderUI({
    str1 <- paste0("From January - June 2019, Donald Trump tweeted \"Economy\" 79 times,
                   \"Economic\" 36 times, \"Jobs\" 54 times, \"Justice\" 25 times
                   and \"Trade\" 51 times.")
    str2 <- paste0("By contrast, Bernie Sanders tweeted \"Economy\" 35 times,
                   \"Economic\" 57 times, \"Jobs\" 53 times, \"Justice\" 92 times
                   and \"Trade\" 26 times. Sanders notably tweeted \"Economic\" 33 times
                   in June. In particular, he tweeted \"Economic\" 12 times on June 12.")
    HTML(paste(str1, str2, sep = '</br></br>'))
  })
  
  output$Name <- renderUI({
    str3 <- paste0("Author: ", "<b>", "Isaac Yu", "</b>", "</br>")
    HTML(paste(str3, sep = '<br/>'))
  })
  
  output$About <- renderUI({
    str4 <- paste0("</br>", "I graduated from Simon Fraser University with a major in sociology 
                   and a minor in statistics.", "</br>")
    str5 <- ("This Shiny App gathers compares tweets from Donald Trump and Bernie Sanders' accounts.  
          It displays the number of tweets per day over each month.  It also displays 
          the frequency of certain key words, such as \"Economy\".")
    HTML(paste(str4, str5, sep = '<br/>'))
  })
}
