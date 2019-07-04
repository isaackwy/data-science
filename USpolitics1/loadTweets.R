library(rtweet)

# Connect to API. Keys not uploaded to github.
source('connect_to_twitter.R')

Trump2019 <- get_timeline("realDonaldTrump", n = 3200)
Sanders2019 <- get_timeline("BernieSanders", n = 3200)
save(Trump2019, file="Trump2019.RData")
save(Sanders2019, file="Sanders2019.RData")
