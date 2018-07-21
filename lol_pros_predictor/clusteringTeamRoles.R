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


##################################################################################
##################################################################################
#
#  BEGIN CLUSTERING BELOW
#
##################################################################################
##################################################################################

# Split dataset into training and testing
#install.packages('caret')
library(caret)
set.seed(1234)
train_index <- caret::createDataPartition(all_leagues_summoner_avgs$teamRole, p = 0.4, list = FALSE, times = 1)
train_avgs_data <- all_leagues_summoner_avgs[train_index,]
test_avgs_data <- all_leagues_summoner_avgs[-train_index,]

# Remove columns 1:6, 8, 10, 30:33, 41, 44:56 (Thus, using only 17 features.)
train_avgs_data_scaled <- train_avgs_data %>%
  select(kills, assists, magicDamageDealt, physicalDamageDealt, magicDamageDealtToChampions, physicalDamageDealtToChampions, totalHeal, totalUnitsHealed, damageSelfMitigated, totalDamageTaken, neutralMinionsKilled, timeCCingOthers, totalTimeCrowdControlDealt, champLevel, visionWardsBoughtInGame, wardsPlaced, wardsKilled,
  totalMinionsKilled, damageTakenPerMinDeltas.0.10, physicalDamageDealtToChampionsToTakenRatio, creepsPerMinDeltas.0.10) %>%
  # Use z-value scaling of the features.
  scale()

# Using k-means on the training set to make centroids
set.seed(1234)
train_fit.km <- kmeans(train_avgs_data_scaled, 5, iter.max = 1000)
km_train_table <- table(train_fit.km$cluster, train_avgs_data$teamRole)
km_train_table <- km_train_table[c(2, 1, 3, 4, 5),]
rownames(km_train_table) <- c("BOTCARRY", "JUNGLE", "MID", "SUPPORT", "TOP")
km_train_table
#clusplot(train_avgs_data_scaled, train_fit.km$cluster, main = "Clusplot", labels = 4)

#train_avgs_data <- train_avgs_data %>% bind_cols(data.frame(train_fit.km$cluster))
#train_avgs_data <- train_avgs_data[, c(1:4,57,5:56)]

# Using knn to classify observations in the test set
test_avgs_data_scaled <- test_avgs_data %>%
  select(kills, assists, magicDamageDealt, physicalDamageDealt, magicDamageDealtToChampions, physicalDamageDealtToChampions, totalHeal, totalUnitsHealed, damageSelfMitigated, totalDamageTaken, neutralMinionsKilled, timeCCingOthers, totalTimeCrowdControlDealt, champLevel, visionWardsBoughtInGame, wardsPlaced, wardsKilled,
  totalMinionsKilled, damageTakenPerMinDeltas.0.10, physicalDamageDealtToChampionsToTakenRatio, creepsPerMinDeltas.0.10) %>%
  # Use z-value scaling of the features.
  scale()
library(FNN)
set.seed(1234)
knn_pred_test_avgs <- get.knnx(train_fit.km$centers, test_avgs_data_scaled, 1)$nn.index[, 1]
#library(gmodels)
#CrossTable(x = test_avgs_data$teamRole,
           #y = knn_pred_test_avgs,
           #prop.chisq = FALSE)
knn_test_avgs_table <- table(knn_pred_test_avgs, test_avgs_data$teamRole)
knn_test_avgs_table <- knn_test_avgs_table[c(2, 1, 3, 4, 5),]
rownames(knn_test_avgs_table) <- c("BOTCARRY", "JUNGLE", "MID", "SUPPORT", "TOP")
knn_test_avgs_table


# Using knn to classify all match-by-match individual performances
all_leagues_match_player_stats_scaled <- all_leagues_match_player_stats %>%
  select(kills, assists, magicDamageDealt, physicalDamageDealt, magicDamageDealtToChampions, physicalDamageDealtToChampions, totalHeal, totalUnitsHealed, damageSelfMitigated, totalDamageTaken, neutralMinionsKilled, timeCCingOthers, totalTimeCrowdControlDealt, champLevel, visionWardsBoughtInGame, wardsPlaced, wardsKilled,
  totalMinionsKilled, damageTakenPerMinDeltas.0.10, physicalDamageDealtToChampionsToTakenRatio, creepsPerMinDeltas.0.10) %>%
  # Use z-value scaling of the features.
  scale()
set.seed(1234)
knn_pred_test_single_games <- get.knnx(train_fit.km$centers,
                                       all_leagues_match_player_stats_scaled,
                                       k=1)$nn.index[, 1]
knn_test_singles_table <- table(knn_pred_test_single_games,
                                all_leagues_match_player_stats$teamRole)
knn_test_singles_table <- knn_test_singles_table[c(2, 1, 3, 4, 5),]
rownames(knn_test_singles_table) <- c("BOTCARRY", "JUNGLE", "MID", "SUPPORT", "TOP")
library(caret)
confusionMatrix(knn_test_singles_table)


all_leagues_match_player_stats_with_pred <- all_leagues_match_player_stats
all_leagues_match_player_stats_with_pred["predTeamRole"] <- knn_pred_test_single_games
# Labeling predicted team roles
for (j in 1:nrow(all_leagues_match_player_stats_with_pred)) {
  if (all_leagues_match_player_stats_with_pred[j, 'predTeamRole'] == 1) {
    all_leagues_match_player_stats_with_pred[j, 'predTeamRole'] = "JUNGLE"
  } else if (all_leagues_match_player_stats_with_pred[j, 'predTeamRole'] == 2) {
    all_leagues_match_player_stats_with_pred[j, 'predTeamRole'] = "BOTCARRY"
  } else if (all_leagues_match_player_stats_with_pred[j, 'predTeamRole'] == 3) {
    all_leagues_match_player_stats_with_pred[j, 'predTeamRole'] = "MID"
  } else if (all_leagues_match_player_stats_with_pred[j, 'predTeamRole'] == 5) {
    all_leagues_match_player_stats_with_pred[j, 'predTeamRole'] = "TOP"
  } else {
    all_leagues_match_player_stats_with_pred[j, 'predTeamRole'] = "SUPPORT"
  }
}
all_leagues_match_player_stats_with_pred <- all_leagues_match_player_stats_with_pred %>%
  mutate("truePositive" = (as.character(teamRole) == as.character(predTeamRole)))

fn_top_table <- all_leagues_match_player_stats_with_pred %>%
  filter(teamRole == "TOP") %>% filter(predTeamRole != "TOP")
table(fn_top_table$name, fn_top_table$predTeamRole)
fn_bot_table <- all_leagues_match_player_stats_with_pred %>%
  filter(teamRole == "BOTCARRY") %>% filter(predTeamRole != "BOTCARRY")
table(fn_bot_table$name, fn_bot_table$predTeamRole)
fn_jungle_table <- all_leagues_match_player_stats_with_pred %>%
  filter(teamRole == "JUNGLE") %>% filter(predTeamRole != "JUNGLE")
table(fn_jungle_table$name, fn_jungle_table$predTeamRole)
fn_mid_table <- all_leagues_match_player_stats_with_pred %>%
  filter(teamRole == "MID") %>% filter(predTeamRole != "MID")
table(fn_mid_table$name, fn_mid_table$predTeamRole)
fn_support_table <- all_leagues_match_player_stats_with_pred %>%
  filter(teamRole == "SUPPORT") %>% filter(predTeamRole != "SUPPORT")
table(fn_support_table$name, fn_support_table$predTeamRole)



library(tidyr)
all_leagues_match_player_stats_with_pred %>%
  gather(kills, assists, magicDamageDealt, physicalDamageDealt, magicDamageDealtToChampions,
    physicalDamageDealtToChampions, key = "varName", value = "valuePerGame") %>%
    ggplot() +
    geom_boxplot(mapping = aes(x = predTeamRole, y = valuePerGame),
    size = 1.25, alpha = .6, show.legend = FALSE) +
    geom_jitter(width = 0.35, size = 0.6, mapping = aes(
    x = predTeamRole, y = valuePerGame, color = teamRole), show.legend = TRUE) +
    facet_grid(varName ~ truePositive, scales = "free") +
    theme(axis.text.x = element_text(angle = 10, vjust = 0.6)) +
    labs(
  title = "False Negatives and True Positives, Grouped by Predicted Roles, Dot Colors Actual Roles",
      subtitle = "Distribution Across Team Roles")

all_leagues_match_player_stats_with_pred %>%
  gather(totalHeal, totalUnitsHealed, damageSelfMitigated, totalDamageTaken,
    neutralMinionsKilled, timeCCingOthers, key = "varName", value = "valuePerGame") %>%
    ggplot() +
    geom_boxplot(mapping = aes(x = predTeamRole, y = valuePerGame),
    size = 1.25, alpha = .6, show.legend = FALSE) +
    geom_jitter(width = 0.35, size = 0.6, mapping = aes(
    x = predTeamRole, y = valuePerGame, color = teamRole), show.legend = TRUE) +
    facet_grid(varName ~ truePositive, scales = "free") +
    theme(axis.text.x = element_text(angle = 10, vjust = 0.6)) +
    labs(
  title = "False Negatives and True Positives, Grouped by Predicted Roles, Dot Colors Actual Roles",
      subtitle = "Distribution Across Team Roles")

all_leagues_match_player_stats_with_pred %>%
  gather(totalTimeCrowdControlDealt, champLevel, visionWardsBoughtInGame,
    wardsPlaced, wardsKilled, key = "varName", value = "valuePerGame") %>%
    ggplot() +
    geom_boxplot(mapping = aes(x = predTeamRole, y = valuePerGame),
    size = 1.25, alpha = .6, show.legend = FALSE) +
    geom_jitter(width = 0.35, size = 0.6, mapping = aes(
    x = predTeamRole, y = valuePerGame, color = teamRole), show.legend = TRUE) +
    facet_grid(varName ~ truePositive, scales = "free") +
    theme(axis.text.x = element_text(angle = 10, vjust = 0.6)) +
    labs(
  title = "False Negatives and True Positives, Grouped by Predicted Roles, Dot Colors Actual Roles",
      subtitle = "Distribution Across Team Roles")



# Analyzing model success by wins and losses
# Wins
all_leagues_match_player_stats_wins <- all_leagues_match_player_stats_with_pred %>% filter(win == TRUE)
knn_test_wins_table <- table(all_leagues_match_player_stats_wins$predTeamRole,
                             all_leagues_match_player_stats_wins$teamRole)
knn_test_wins_table <- knn_test_wins_table[c(2, 1, 3, 5, 4),]
rownames(knn_test_wins_table) <- c("BOTCARRY", "JUNGLE", "MID", "SUPPORT", "TOP")
caret::confusionMatrix(knn_test_wins_table)


# Losses
all_leagues_match_player_stats_losses <- all_leagues_match_player_stats_with_pred %>% filter(win == FALSE)
knn_test_losses_table <- table(all_leagues_match_player_stats_losses$predTeamRole,
                             all_leagues_match_player_stats_losses$teamRole)
knn_test_losses_table <- knn_test_losses_table[c(2, 1, 3, 5, 4),]
rownames(knn_test_losses_table) <- c("BOTCARRY", "JUNGLE", "MID", "SUPPORT", "TOP")
caret::confusionMatrix(knn_test_losses_table)














## Partition by wins and losses
#all_leagues_player_wl_avgs <- get_league_season_summoner_avgs(all_leagues_match_player_stats,
                                                              #only_regseason = FALSE,
                                                              #split_winloss = TRUE) %>%
                                                              #mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
                                                              #filter(n >= 10) %>% 
                                                              #ungroup()
## Winners
#all_leagues_player_wins_avgs_featured <- all_leagues_player_wl_avgs %>%
  #filter(win == TRUE)
#facet_box_plot_league_season_player_avgs(all_leagues_player_wins_avgs_featured)
#all_leagues_player_wins_avgs_scaled <- all_leagues_player_wins_avgs_featured %>%
  #select(kills, assists, magicDamageDealt, physicalDamageDealt, magicDamageDealtToChampions, physicalDamageDealtToChampions, totalHeal, totalUnitsHealed, damageSelfMitigated, totalDamageTaken, neutralMinionsKilled, timeCCingOthers, totalTimeCrowdControlDealt, champLevel, visionWardsBoughtInGame, wardsPlaced, wardsKilled) %>%
  #scale()
#set.seed(1234)
#train_fit_wins.km <- kmeans(all_leagues_player_wins_avgs_scaled, 5, iter.max = 1000)
#km_train_table_wins <- table(train_fit_wins.km$cluster, all_leagues_player_wins_avgs_featured$teamRole)
##km_train_table_wins <- km_train_table_wins[c(2, 1, 3, 5, 4),]
##rownames(km_train_table) <- c("BOTCARRY", "JUNGLE", "MID", "SUPPORT", "TOP")
#km_train_table_wins

#all_leagues_match_player_wins_scaled <- all_leagues_match_player_stats %>%
  #filter(win == TRUE) %>% 
  #select(kills, assists, magicDamageDealt, physicalDamageDealt, magicDamageDealtToChampions, physicalDamageDealtToChampions, totalHeal, totalUnitsHealed, damageSelfMitigated, totalDamageTaken, neutralMinionsKilled, timeCCingOthers, totalTimeCrowdControlDealt, champLevel, visionWardsBoughtInGame, wardsPlaced, wardsKilled) %>%
  ## Use z-value scaling of the features.
  #scale()
#set.seed(1234)
#knn_pred_test_single_wins <- get.knnx(train_fit_wins.km$centers,
                                       #all_leagues_match_player_wins_scaled,
                                       #k = 1)$nn.index[, 1]
#knn_test_wins_table <- table(knn_pred_test_single_wins,
                                #(all_leagues_match_player_stats %>%
                                #filter(win == TRUE))$teamRole)
#knn_test_wins_table <- knn_test_wins_table[c(2, 1, 3, 5, 4),]
#rownames(knn_test_wins_table) <- c("BOTCARRY", "JUNGLE", "MID", "SUPPORT", "TOP")
#knn_test_wins_table


## Losers
#all_leagues_player_losses_avgs_featured <- all_leagues_player_wl_avgs %>%
  #filter(win == FALSE)
#facet_box_plot_league_season_player_avgs(all_leagues_player_losses_avgs_featured)
#all_leagues_player_losses_avgs_scaled <- all_leagues_player_losses_avgs_featured %>%
  #select(kills, assists, magicDamageDealt, physicalDamageDealt, magicDamageDealtToChampions, physicalDamageDealtToChampions, totalHeal, totalUnitsHealed, damageSelfMitigated, totalDamageTaken, neutralMinionsKilled, timeCCingOthers, totalTimeCrowdControlDealt, champLevel, visionWardsBoughtInGame, wardsPlaced, wardsKilled) %>%
  #scale()
#set.seed(1234)
#train_fit_losses.km <- kmeans(all_leagues_player_losses_avgs_scaled, 5, iter.max = 1000)
#km_train_table_losses <- table(train_fit_losses.km$cluster, all_leagues_player_losses_avgs_featured$teamRole)
##km_train_table_losses <- km_train_table_losses[c(2, 1, 3, 5, 4),]
##rownames(km_train_table_losses) <- c("BOTCARRY", "JUNGLE", "MID", "SUPPORT", "TOP")
#km_train_table_losses

#all_leagues_match_player_losses_scaled <- all_leagues_match_player_stats %>%
  #filter(win == FALSE) %>%
  #select(kills, assists, magicDamageDealt, physicalDamageDealt, magicDamageDealtToChampions, physicalDamageDealtToChampions, totalHeal, totalUnitsHealed, damageSelfMitigated, totalDamageTaken, neutralMinionsKilled, timeCCingOthers, totalTimeCrowdControlDealt, champLevel, visionWardsBoughtInGame, wardsPlaced, wardsKilled) %>%
  ## Use z-value scaling of the features.
  #scale()
#set.seed(1234)
#knn_pred_test_single_losses <- get.knnx(train_fit_losses.km$centers,
                                       #all_leagues_match_player_losses_scaled,
                                       #k = 1)$nn.index[, 1]
#knn_test_losses_table <- table(knn_pred_test_single_losses,
                                #(all_leagues_match_player_stats %>%
                                #filter(win == FALSE))$teamRole)
#knn_test_losses_table <- knn_test_losses_table[c(2, 1, 3, 5, 4),]
#rownames(knn_test_losses_table) <- c("BOTCARRY", "JUNGLE", "MID", "SUPPORT", "TOP")
#knn_test_losses_table

## Compare to Riot's system of assigning role-lane combos
#table_roles <- table(all_leagues_match_player_stats$roleLane, all_leagues_match_player_stats$teamRole)
#table_roles
#table(knn_pred_match_players, all_leagues_match_player_stats$teamRole)
##kable(table_roles, caption = "NA LCS Spring Split 2018 Team Role Assignments")

#' Title
#'
#' @param league_season_player_avgs
#' @param str_league_season
#' @param min_games
#'
#' @return
#' @export
#'
#' @examples
facet_box_plot_league_season_player_avgs <- function(league_season_player_avgs,
                                                 str_league_season = "2018 Spring Split",
                                                 min_games = 3) {
  league_season_player_avgs <- league_season_player_avgs %>%
    filter(n >= min_games)
  league_season_player_avgs_gathered <- league_season_player_avgs %>%
  gather(kills, assists, magicDamageDealt, physicalDamageDealt, magicDamageDealtToChampions, physicalDamageDealtToChampions, totalHeal, totalUnitsHealed, damageSelfMitigated, totalDamageTaken, neutralMinionsKilled, timeCCingOthers, totalTimeCrowdControlDealt, champLevel, visionWardsBoughtInGame, wardsPlaced, wardsKilled, key = "varName", value = "valuePerGame")

  league_season_player_avgs_gathered %>% ggplot() +
    geom_boxplot(mapping = aes(x = teamRole, y = valuePerGame, fill = teamRole), size = 1.25, alpha = .6) +
    geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = valuePerGame, color = teamRole)) +
    facet_wrap(~varName, scales = "free", ncol = 3) +
    theme(axis.text.x = element_text(angle = 10, vjust = 0.6)) +
    labs(
  title = paste("Player Averages per Game Box Plots", str_league_season, sep = ", "),
      subtitle = "Distribution Across Team Roles")
}


library(tidyr)
library(ggplot2)
all_leagues_summoner_avgs <- all_leagues_summoner_avgs %>%
  mutate("damageDealtToChampionsToTakenRatio" = totalDamageDealtToChampions / totalDamageTaken) %>%
  mutate("magicDamageDealtToChampionsToTakenRatio" = magicDamageDealtToChampions / totalDamageTaken) %>%
  mutate("physicalDamageDealtToChampionsToTakenRatio" = physicalDamageDealtToChampions / totalDamageTaken) %>%
  mutate("totalHealDamageDealtToChampionsRatio" = totalHeal / totalDamageDealtToChampions) %>%
  mutate("totalHealPerUnitHealed" = totalHeal / totalUnitsHealed)
all_leagues_summoner_avgs %>%
  gather(totalMinionsKilled, xpPerMinDeltas.0.10, key = "varName", value = "valuePerGame") %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = teamRole, y = valuePerGame, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = valuePerGame, color = teamRole)) +
  facet_wrap(~varName, scales = "free", ncol = 3)

all_leagues_match_player_stats <- all_leagues_match_player_stats %>%
  mutate("damageDealtToChampionsToTakenRatio" = totalDamageDealtToChampions / totalDamageTaken) %>%
  mutate("magicDamageDealtToChampionsToTakenRatio" = magicDamageDealtToChampions / totalDamageTaken) %>%
  mutate("physicalDamageDealtToChampionsToTakenRatio" = physicalDamageDealtToChampions / totalDamageTaken) %>%
  mutate("totalHealDamageDealtToChampionsRatio" = totalHeal / totalDamageDealtToChampions) %>%
  mutate("totalHealPerUnitHealed" = totalHeal / totalUnitsHealed)
all_leagues_match_player_stats %>%
  gather(totalMinionsKilled, damageTakenPerMinDeltas.0.10, physicalDamageDealtToChampionsToTakenRatio, creepsPerMinDeltas.0.10, key = "varName", value = "valuePerGame") %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = teamRole, y = valuePerGame, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = valuePerGame, color = teamRole)) +
  facet_wrap(~varName, scales = "free", ncol = 2)