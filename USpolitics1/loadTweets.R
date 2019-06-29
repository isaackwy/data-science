library(rtweet)

# Connect to API. Keys not in repo.
source('connect_to_twitter.R')

Drumpf2019 <- get_timeline("realDonaldTrump", n = 3200)
Birdie2019 <- get_timeline("BernieSanders", n = 3200)
save(Drumpf2019, file="Drumpf2019.RData")
save(Birdie2019, file="Birdie2019.RData")
