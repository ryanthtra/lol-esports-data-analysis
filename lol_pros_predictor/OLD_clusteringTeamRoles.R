library("cluster")
library("rattle.data")
library(dplyr)

# NALCS
nalcs_season_summoner_avgs <- read.csv("datasets/nalcs/nalcs_spring2018_season_summoner_avgs.csv") %>% select(-X)
# EULCS
eulcs_season_summoner_avgs <- read.csv("datasets/eulcs/eulcs_spring2018_season_summoner_avgs.csv") %>% select(-X)
# LCK
lck_season_summoner_avgs <- read.csv("datasets/lck/lck_spring2018_season_summoner_avgs.csv") %>% select(-X)
# LMS
lms_season_summoner_avgs <- read.csv("datasets/lms/lms_spring2018_season_summoner_avgs.csv") %>% select(-X)
# MSI
msi_season_summoner_avgs <- read.csv("datasets/msi/msi_2018_season_summoner_avgs.csv") %>% select(-X)

# Putting all five together
all_leagues_summoner_avgs <- msi_season_summoner_avgs %>%
  bind_rows(lms_season_summoner_avgs) %>%
  bind_rows(lck_season_summoner_avgs) %>%
  bind_rows(nalcs_season_summoner_avgs) %>%
  bind_rows(eulcs_season_summoner_avgs)

# Split dataset into training and testing
#install.packages('caret')
library(caret)
train_index <- caret::createDataPartition(all_leagues_summoner_avgs$teamRole, p = 0.3, list = FALSE, times = 1)
train_avgs_data <- all_leagues_summoner_avgs[train_index,]
test_avgs_data <- all_leagues_summoner_avgs[-train_index,]

# Remove columns 1:6, 8, 10, 30:33, 41, 44:56
nalcs_km_data <- nalcs_season_summoner_avgs %>%
  select(kills, assists, magicDamageDealt, physicalDamageDealt, magicDamageDealtToChampions, physicalDamageDealtToChampions, totalHeal, totalUnitsHealed, damageSelfMitigated, totalDamageTaken, neutralMinionsKilled, timeCCingOthers, totalTimeCrowdControlDealt, champLevel, visionWardsBoughtInGame, wardsPlaced, wardsKilled) %>%
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
table(nalcs_season_summoner_avgs$teamRole, nalcs_fit.km$cluster)


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

# Using knn to test EULCS data using k-means model centroids as training model
library(FNN)
pred_eulcs_players <- get.knnx(nalcs_fit.km$centers, eulcs_km_data, 1)$nn.index[, 1]
library(gmodels)
CrossTable(x = eulcs_season_summoner_avgs$teamRole,
           y = pred_eulcs_players,
           prop.chisq = FALSE)

# Using knn to test EULCS match-by-match individual performances
eulcs_season_match_data <- read.csv("datasets/eulcs/eulcs_spring2018_match_player_stats.csv") %>% mutate(roleLane = paste(role, lane, sep = "_"))
eulcs_playermatch_km_data <- eulcs_season_match_data %>%
  select(kills, assists, magicDamageDealt, physicalDamageDealt, magicDamageDealtToChampions, physicalDamageDealtToChampions, totalHeal, totalUnitsHealed, damageSelfMitigated, totalDamageTaken, neutralMinionsKilled, timeCCingOthers, totalTimeCrowdControlDealt, champLevel, visionWardsBoughtInGame, wardsPlaced, wardsKilled) %>%
  scale()
library(FNN)
pred_eulcs_matchplayers <- get.knnx(nalcs_fit.km$centers, eulcs_playermatch_km_data, 1)$nn.index[, 1]
library(gmodels)
CrossTable(x = eulcs_season_match_data$teamRole,
           y = eulcs_season_match_data$roleLane,
           prop.chisq = FALSE)
table(eulcs_season_match_data$teamRole,
      eulcs_season_match_data$roleLane)
CrossTable(x = eulcs_season_match_data$teamRole,
           y = pred_eulcs_matchplayers,
           prop.chisq = FALSE)
table(eulcs_season_match_data$teamRole,
      pred_eulcs_matchplayers)