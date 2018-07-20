library("cluster")
library("rattle.data")
library(dplyr)

# Import averages data for all available leagues
nalcs_season_summoner_avgs <- read.csv("datasets/nalcs/nalcs_spring2018_season_summoner_avgs.csv") %>% select(-X) 
eulcs_season_summoner_avgs <- read.csv("datasets/eulcs/eulcs_spring2018_season_summoner_avgs.csv") %>% select(-X)
lck_season_summoner_avgs <- read.csv("datasets/lck/lck_spring2018_season_summoner_avgs.csv") %>% select(-X)
lms_season_summoner_avgs <- read.csv("datasets/lms/lms_spring2018_season_summoner_avgs.csv") %>% select(-X)
msi_season_summoner_avgs <- read.csv("datasets/msi/msi_2018_season_summoner_avgs.csv") %>% select(-X) 

# Putting all leagues together
all_leagues_summoner_avgs <-
  eulcs_season_summoner_avgs %>%
  bind_rows(lms_season_summoner_avgs) %>%
  bind_rows(lck_season_summoner_avgs) %>%
  bind_rows(nalcs_season_summoner_avgs) %>%
  #bind_rows(msi_season_summoner_avgs) %>%
  filter(wins + losses >= 6) 

remove(nalcs_season_summoner_avgs, eulcs_season_summoner_avgs, lck_season_summoner_avgs, lms_season_summoner_avgs, msi_season_summoner_avgs)

# Import match-by-match player data 
nalcs_season_match_player_stats <- read.csv("datasets/nalcs/nalcs_spring2018_match_player_stats.csv") %>% select(-X)
eulcs_season_match_player_stats <- read.csv("datasets/eulcs/eulcs_spring2018_match_player_stats.csv") %>% select(-X)
lck_season_match_player_stats <- read.csv("datasets/lck/lck_spring2018_match_player_stats.csv") %>% select(-X)
lms_season_match_player_stats <- read.csv("datasets/lms/lms_spring2018_match_player_stats.csv") %>% select(-X)
msi_season_match_player_stats <- read.csv("datasets/msi/msi_2018_match_player_stats.csv") %>% select(-X)

all_leagues_match_player_stats <-
  nalcs_season_match_player_stats %>%
  bind_rows(eulcs_season_match_player_stats) %>%
  bind_rows(lck_season_match_player_stats) %>%
  bind_rows(lms_season_match_player_stats) %>%
  bind_rows(msi_season_match_player_stats) %>%
  mutate(roleLane = paste(role, lane, sep = ", "))

remove(nalcs_season_match_player_stats, eulcs_season_match_player_stats, lck_season_match_player_stats, lms_season_match_player_stats, msi_season_match_player_stats)

get_teamrole_avgs <- function(league_matches_player_stats) {
  league_season_participants_accum <- league_matches_player_stats

  # Create avg stats DF groups by lane and role
  league_teamrole_avgs <-
    league_season_participants_accum %>%
    group_by(teamRole) %>%
    summarize_at(vars(duration, kills:assists, totalDamageDealt:trueDamageDealt,
                      totalDamageDealtToChampions:firstBloodKill,
                      firstTowerKill:firstInhibitorAssist, 'creepsPerMinDeltas.10.20',
                      'creepsPerMinDeltas.0.10', 'xpPerMinDeltas.10.20',
                      'xpPerMinDeltas.0.10', 'goldPerMinDeltas.10.20',
                      'goldPerMinDeltas.0.10', 'damageTakenPerMinDeltas.10.20',
                      'damageTakenPerMinDeltas.0.10'), mean)

  # Adding KDA Ratio column
  league_teamrole_avgs <- league_teamrole_avgs %>%
    mutate(KDA = (kills + assists) / deaths)

  # Reordering columns
  league_season_participants_accum <- league_season_participants_accum[
  , c(1:5, 52, 6:51)]

  return(league_teamrole_avgs)
}
library(tidyr)
all_leagues_teamrole_avgs <- get_teamrole_avgs(all_leagues_match_player_stats)
all_leagues_teamrole_avgs_scaled <- all_leagues_teamrole_avgs %>%
  select(kills, assists, magicDamageDealt, physicalDamageDealt, magicDamageDealtToChampions, physicalDamageDealtToChampions, totalHeal, totalUnitsHealed, damageSelfMitigated, totalDamageTaken, neutralMinionsKilled, timeCCingOthers, totalTimeCrowdControlDealt, champLevel, visionWardsBoughtInGame, wardsPlaced, wardsKilled) %>%
  # Use z-value scaling of the features.
  scale()

# Split dataset into training and testing
#install.packages('caret')
library(caret)
train_index <- caret::createDataPartition(all_leagues_summoner_avgs$teamRole, p = 0.4, list = FALSE, times = 1)
train_avgs_data <- all_leagues_summoner_avgs[train_index,]
test_avgs_data <- all_leagues_summoner_avgs[-train_index,]

# Remove columns 1:6, 8, 10, 30:33, 41, 44:56 (Thus, using only 17 features.)
train_avgs_data_scaled <- train_avgs_data %>%
  select(kills, assists, magicDamageDealt, physicalDamageDealt, magicDamageDealtToChampions, physicalDamageDealtToChampions, totalHeal, totalUnitsHealed, damageSelfMitigated, totalDamageTaken, neutralMinionsKilled, timeCCingOthers, totalTimeCrowdControlDealt, champLevel, visionWardsBoughtInGame, wardsPlaced, wardsKilled) %>%
  # Use z-value scaling of the features.
  scale()

# Using k-means on the training set to make centroids
set.seed(1234)
train_fit.km <- kmeans(train_avgs_data_scaled, 5, iter.max = 1000)
table(train_avgs_data$teamRole, train_fit.km$cluster)
clusplot(train_avgs_data_scaled, train_fit.km$cluster, main = "Clusplot", labels = 4)

#train_avgs_data <- train_avgs_data %>% bind_cols(data.frame(train_fit.km$cluster))
#train_avgs_data <- train_avgs_data[, c(1:4,57,5:56)]

# Using knn to classify observations in the test set
test_avgs_data_scaled <- test_avgs_data %>%
  select(kills, assists, magicDamageDealt, physicalDamageDealt, magicDamageDealtToChampions, physicalDamageDealtToChampions, totalHeal, totalUnitsHealed, damageSelfMitigated, totalDamageTaken, neutralMinionsKilled, timeCCingOthers, totalTimeCrowdControlDealt, champLevel, visionWardsBoughtInGame, wardsPlaced, wardsKilled) %>%
  # Use z-value scaling of the features.
  scale()
library(FNN)
knn_pred_test_avgs <- get.knnx(train_fit.km$centers, test_avgs_data_scaled, 1)$nn.index[, 1]
#library(gmodels)
#CrossTable(x = test_avgs_data$teamRole,
           #y = knn_pred_test_avgs,
           #prop.chisq = FALSE)
table(test_avgs_data$teamRole, knn_pred_test_avgs)


# Using knn to classify all match-by-match individual performances
all_leagues_match_player_stats_scaled <- all_leagues_match_player_stats %>%
  select(kills, assists, magicDamageDealt, physicalDamageDealt, magicDamageDealtToChampions, physicalDamageDealtToChampions, totalHeal, totalUnitsHealed, damageSelfMitigated, totalDamageTaken, neutralMinionsKilled, timeCCingOthers, totalTimeCrowdControlDealt, champLevel, visionWardsBoughtInGame, wardsPlaced, wardsKilled) %>%
  # Use z-value scaling of the features.
  scale()
knn_pred_match_players <- get.knnx(train_fit.km$centers, all_leagues_match_player_stats_scaled, 1)$nn.index[, 1]
table(all_leagues_match_player_stats$teamRole, knn_pred_match_players)

knn_pred_match_players_2 <- get.knnx(all_leagues_teamrole_avgs_scaled, all_leagues_match_player_stats_scaled, 1)$nn.index[, 1]
table(all_leagues_match_player_stats$teamRole, knn_pred_match_players_2)

# Compare to Riot's system of assigning role-lane combos
table_roles <- table(all_leagues_match_player_stats$roleLane, all_leagues_match_player_stats$teamRole)
table_roles
table(knn_pred_match_players, all_leagues_match_player_stats$teamRole)
#kable(table_roles, caption = "NA LCS Spring Split 2018 Team Role Assignments")

