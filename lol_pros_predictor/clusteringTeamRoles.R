library("cluster")
library("rattle.data")
library(dplyr)

# NALCS
nalcs_season_summoner_avgs <- read.csv("datasets/nalcs/nalcs_spring2018_season_summoner_avgs.csv") %>%
  select(-X) %>%
  filter(wins + losses >= 6) 


# Remove columns 1:6, 8, 10, 30:33, 41, 44:56
nalcs_km_data <- nalcs_season_summoner_avgs %>%
  select(kills, assists, magicDamageDealt, physicalDamageDealt, magicDamageDealtToChampions, physicalDamageDealtToChampions, totalHeal, totalUnitsHealed, damageSelfMitigated, totalDamageTaken,  neutralMinionsKilled, timeCCingOthers, totalTimeCrowdControlDealt, champLevel, visionWardsBoughtInGame, wardsPlaced, wardsKilled) %>%
  scale()


wssplot <- function(data, nc = 15, seed = 1234) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)
  }

  plot(1:nc, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
}
wssplot(nalcs_km_data)


library(NbClust)
set.seed(1234)
nc <- NbClust(nalcs_km_data, min.nc = 2, max.nc = 15, method = "kmeans")
barplot(table(nc$Best.n[1,]),
  xlab = "Number of Clusters", ylab = "Number of Criteria",
  main = "Number of Clusters Chosen by 26 Criteria")

set.seed(1234)
nalcs_fit.km <- kmeans(nalcs_km_data, 5, iter.max = 1000)
clusplot(nalcs_km_data, nalcs_fit.km$cluster, main = "Clusplot")
table(nalcs_fit.km$cluster, nalcs_season_summoner_avgs$teamRole)

# Clustering EU LCS 
eulcs_season_summoner_avgs <- read.csv("datasets/eulcs/eulcs_spring2018_season_summoner_avgs.csv") %>%
  select(-X) %>%
  filter(wins + losses >= 6)

eulcs_km_data <- eulcs_season_summoner_avgs %>%
  select(kills, assists, magicDamageDealt, physicalDamageDealt, magicDamageDealtToChampions, physicalDamageDealtToChampions, totalHeal, totalUnitsHealed, damageSelfMitigated, totalDamageTaken, neutralMinionsKilled, timeCCingOthers, totalTimeCrowdControlDealt, champLevel, visionWardsBoughtInGame, wardsPlaced, wardsKilled) %>%
  scale()

set.seed(1234)
eulcs_fit.km <- kmeans(eulcs_km_data, 5, iter.max = 1000)
clusplot(eulcs_km_data, eulcs_fit.km$cluster, main = "Clusplot")
table(eulcs_season_summoner_avgs$teamRole, eulcs_fit.km$cluster)