library(shiny)
library(RSQLite)
library(DBI)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(reshape2)

# References:
# Fill missing data (e.x. in a time series)
# https://stackoverflow.com/questions/6058677/how-to-create-na-for-missing-data-in-a-time-series

server <- function(input, output) {
  output$BikeFreq <- renderPlot({
    dbcon = dbConnect(SQLite(), dbname = "stat240Apr3.sqlite")   
    
    biketrips_dplyr <- dbReadTable(dbcon, "biketrips") %>%
      select(starttime, usertype) %>%
      mutate(starttime = as.POSIXct(starttime, format = "%m/%d/%Y %H:%M"),
             start_day = as.character(starttime, format = "%Y-%m-%d"),
             start_hour = as.integer(format(starttime, "%H")),
             usertype = as.factor(usertype)) %>% 
      filter(start_day == format(input$Day, format = "%Y-%m-%d"))
   
    # Number of bike trips per hour, organized by membership type 
    biketrips_table <- biketrips_dplyr %>%
      group_by(start_hour, usertype) %>%
      count(start_hour)
    
    # Coerce to a data frame. Then prepare to fill in missing 'start_hour' data
    biketrips_table <- as.data.frame(biketrips_table) 
    Hours <- as.data.frame(seq(0, 23))
    colnames(Hours) <- "start_hour"
    biketrips_table2 <- full_join(biketrips_table, Hours, by="start_hour") %>% 
      mutate(usertype = replace_na(usertype, 'Member'),
             n = replace_na(n, 0))
    
    # Display the plot 
    ggplot(biketrips_table2, aes(x = start_hour, y = n, fill = usertype)) + 
      geom_bar(stat = "identity") + 
      theme(axis.text.x = element_text(angle = 90)) +
      xlab("Hour") + ylab("Number of Bike Trips") +
      ggtitle("Bike Trips Per Hour") + 
      scale_x_continuous(breaks = seq(0, 23), 
                         expand = expand_scale(add = 0)) +
      scale_fill_discrete(name = "Rider Type")
  })
  
  output$StationPlot <- renderPlot ({
    dbcon = dbConnect(SQLite(), dbname = "stat240Apr3.sqlite") 
    
    # While the 'stationlocation' table has 60 entries,
    # the 'tofromcounts' table has 59 entries.
    # In joining the two tables, ignore the row in the 'stationlocation' table
    # with missing count_from and count_to entries
    tofromcounts_join <- "SELECT station_id, count_from, count_to
                 FROM stationlocation INNER JOIN tofromcounts
                 ON tofromcounts.from_station_id = stationlocation.station_id"
    tofromcounts_query <- dbGetQuery(dbcon, tofromcounts_join)
    
    # Add in station group and arrange by alphabetical order
    # Filter by region
    tofromcounts_query2 <- tofromcounts_query %>%
      mutate(station_group = str_extract(station_id, '[A-Z]+')) %>% 
      arrange(station_id) %>%
      filter(station_group == input$region)
   
    # Prepare for a side-by-side plot 
    tofromcounts_query3 <- melt(tofromcounts_query2, by = 'station_id')
    
    # Display the Plot
    ggplot(tofromcounts_query3, aes(x = station_id, y = value, fill = variable)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme(axis.text.x = element_text(angle = 90)) +
      xlab("Station ID") + ylab("Number of Trips Taken") +
      ggtitle("Number of Trips Taken, by Station") +
      scale_fill_discrete(name = "", labels = c("Departures", "Arrivals"))
  }) 
  
  output$StationDesc <- renderText({
    # The first two paragraphs are virtually identical to the section above
    dbcon = dbConnect(SQLite(), dbname = "stat240Apr3.sqlite") 
    tofromcounts_join <- "SELECT station_id, count_from, count_to
                 FROM stationlocation INNER JOIN tofromcounts
                 ON tofromcounts.from_station_id = stationlocation.station_id"
    tofromcounts_query <- dbGetQuery(dbcon, tofromcounts_join)
    
    # Add in station group and arrange by alphabetical order
    # Filter by region
    tofromcounts_query2 <- tofromcounts_query %>%
      mutate(station_group = str_extract(station_id, '[A-Z]+')) %>% 
      arrange(station_id) %>%
      filter(station_group == input$region)
    
    # Compare the total number of trips taken to and from each of the stations, by groups
    tofromcounts_sums <- tofromcounts_query2 %>% 
      group_by(station_group) %>% 
      summarize(count_from = sum(count_from), count_to = sum(count_to))
    
    # Compare the amount of departures from stations to arrivals
    if (tofromcounts_sums$count_from > tofromcounts_sums$count_to) {
      paste0("There are more trip departures from stations in this region than those arriving.")
    }
    else {
      paste0("There are fewer trip departures from stations in this region than those arriving.")
    }
  })
 
  output$BikeDesc <- renderUI({
    str1 <- paste0("Members, who purchased Monthly, Annual or Special Passes, are more likely to ride during the weekdays, in early mornings (7am - 9am) and late afternoons (3pm - 5pm). 
                   By contrast, Short-Term Pass Holders, who purchased 24-Hour or 3-Day Passes are more likely to ride during the weekends, in early afternoons (1pm - 3pm). 
                   These trends respectively indicate commute and recreational patterns.")
    HTML(paste(str1, sep = '<br/>'))
  })

  output$Name <- renderUI({
    str4 <- paste0("Author: ", "<b>", "Isaac Yu", "</b>", "</br>")
    HTML(paste(str4, sep = '<br/>'))
  })
  
  output$About <- renderUI({
    str5 <- paste0("</br>", "I graduated from Simon Fraser University with a major in sociology 
                   and a minor in statistics.", "</br>")
    str6 <- paste0("This Shiny App displays Seattle's Pronto Cycle Share data.
                   It shows the number of bike trips per hour over each day.
                   It also compares the number of trips leaving each station with the number of arrivals.") 
    HTML(paste(str5, str6, sep = '<br/>'))
  })
}
