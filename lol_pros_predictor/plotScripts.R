library(dplyr)
library(tidyr)
library(ggplot2)

# The "template" datasets created from Script.R
nalcs_matches_champ_bans <- read.csv("datasets/nalcs/nalcs_spring2018_champ_bans.csv")
nalcs_matches_player_stats <- read.csv("datasets/nalcs/nalcs_spring2018_match_player_stats.csv")
nalcs_matches_team_stats <- read.csv("datasets/nalcs/nalcs_spring2018_match_team_stats.csv")

# EU LCS datasets
eulcs_matches_champ_bans <- read.csv("datasets/eulcs/eulcs_spring2018_champ_bans.csv")
eulcs_matches_player_stats <- read.csv("datasets/eulcs/eulcs_spring2018_match_player_stats.csv")
eulcs_matches_team_stats <- read.csv("datasets/eulcs/eulcs_spring2018_match_team_stats.csv")

nalcs_plot_rsplayer_avgs <- nalcs_season_summoner_avgs %>%
  filter(wins + losses >= 6) %>%
  ggplot()


## nalcs summoner blue/red plots
#nalcs_plot_rsplayer_br_avgs <- nalcs_regseason_summoner_avgs_br_df %>% ggplot()
#nalcs_plot_rsplayer_br_avgs +
#geom_histogram(mapping = aes(x = kills, y = ..density.., color = teamName, fill = teamRole), size = 1.25, alpha = .6, binwidth = .125) +
#facet_grid(teamId ~ .)
#nalcs_plot_rsplayer_br_avgs +
#geom_density(mapping = aes(x = kills, color = teamRole, fill = teamRole), alpha = .3, size = 1.25) +
#facet_grid(teamId ~ .) 
#nalcs_plot_rsplayer_br_avgs +
#geom_histogram(mapping = aes(x = deaths, y = ..density.., color = teamName, fill = teamRole), size = 1.25, alpha = .6, binwidth = .125) +
#facet_grid(teamId ~ .)
#nalcs_plot_rsplayer_br_avgs +
#geom_density(mapping = aes(x = deaths, color = teamRole, fill = teamRole), alpha = .3, size = 1.25) +
#facet_grid(teamId ~ .)
#nalcs_plot_rsplayer_br_avgs +
#geom_histogram(mapping = aes(x = (kills + assists) / deaths, y = ..density.., color = teamName, fill = teamRole), size = 1.25, alpha = .6, binwidth = .125) +
#facet_grid(teamId ~ .)
#nalcs_plot_rsplayer_br_avgs +
#geom_density(mapping = aes(x = (kills + assists) / deaths, color = teamRole, fill = teamRole), alpha = .3, size = 1.25) +
#facet_grid(teamId ~ .)


## nalcs summoner blue/red win/loss plots
#nalcs_plot_rsplayer_brwl_avgs <- nalcs_regseason_summoner_avgs_wlbr_df %>% ggplot()
#nalcs_plot_rsplayer_brwl_avgs +
#geom_histogram(mapping = aes(x = kills, y = ..density.., color = teamName, fill = teamRole), size = 1.25, alpha = .6, binwidth = .125) +
#facet_grid(teamId ~ win)
#nalcs_plot_rsplayer_brwl_avgs +
#geom_density(mapping = aes(x = kills, color = teamRole, fill = teamRole), alpha = .3, size = 1.25) +
#facet_grid(teamId ~ win)
#nalcs_plot_rsplayer_brwl_avgs +
#geom_histogram(mapping = aes(x = deaths, y = ..density.., color = teamName, fill = teamRole), size = 1.25, alpha = .6, binwidth = .125) +
#facet_grid(teamId ~ win)
#nalcs_plot_rsplayer_brwl_avgs +
#geom_density(mapping = aes(x = deaths, color = teamRole, fill = teamRole), alpha = .3, size = 1.25) +
#facet_grid(teamId ~ win)
#nalcs_plot_rsplayer_brwl_avgs +
#geom_histogram(mapping = aes(x = (kills + assists) / deaths, y = ..density.., color = teamName, fill = teamRole), size = 1.25, alpha = .6, binwidth = .125) +
#facet_grid(teamId ~ win)
#nalcs_plot_rsplayer_brwl_avgs +
#geom_density(mapping = aes(x = (kills + assists) / deaths, color = teamRole, fill = teamRole), alpha = .3, size = 1.25) +
#facet_grid(teamId ~ win)

## Kill deficit per game
#nalcs_matches_killdiff <- nalcs_matches_team_stats %>%
#group_by(gameNumber, duration) %>%
#sample_n(1) %>% 
#transmute(killDiff = abs(kills - deaths))
#nalcs_plot_killdiff <- nalcs_matches_killdiff %>% ggplot()
#nalcs_plot_killdiff +
#geom_point(aes(x = duration, y = killDiff)) +
#geom_smooth(aes(x = duration, y = killDiff), method = "lm", se = T)
#nalcs_plot_killdiff +
#geom_density(aes(x = killDiff)) +
#geom_vline(aes(xintercept = mean(killDiff)), linetype = "dashed")
#nalcs_plot_killdiff +
#geom_density(aes(x = duration)) +
#geom_vline(aes(xintercept = mean(duration)), linetype = "dashed")


eulcs_plot_rsplayer_avgs <- eulcs_regseason_summoner_avgs_df %>%
  filter(wins + losses >= 6) %>%
  ggplot()
# Get opponent's data (just swapping the team names of each game in the previous DF)
eulcs_matches_oppteam_stats <- eulcs_matches_team_stats %>%
  group_by(gameNumber) %>%
  mutate(teamName = ifelse(teamId == "Blue", as.character(lead(teamName)), as.character(lag(teamName))))
eulcs_regseason_team_totals_df <- get_league_regseason_team_totals(eulcs_matches_team_stats)
eulcs_regseason_teamopps_totals_df <- get_league_regseason_team_totals(eulcs_matches_oppteam_stats)
# Rename the columns of regular season team opponents totals DF
eulcs_regseason_teamopps_totals_df <- eulcs_regseason_teamopps_totals_df %>%
  rename_at(vars(wins:firstDragons), function(x) {
    return(paste("O", x, sep = "_"))
  })
# Make a differential DF
eulcs_regseason_team_totaldiffs_df <- data.frame(teamName = eulcs_regseason_team_totals_df$teamName, wins = eulcs_regseason_team_totals_df$wins, losses = eulcs_regseason_team_totals_df$losses, totalDuration = eulcs_regseason_team_totals_df$duration, eulcs_regseason_team_totals_df[5] - eulcs_regseason_teamopps_totals_df[5], eulcs_regseason_team_totals_df[7:56] - eulcs_regseason_teamopps_totals_df[7:56])

eulcs_regseason_team_bluered_totals_df <- get_league_regseason_team_totals(eulcs_matches_team_stats, split_bluered = TRUE)
eulcs_regseason_team_avgs_df <- get_league_regseason_team_avgs(eulcs_matches_team_stats)
eulcs_regseason_teamopps_avgs_df <- get_league_regseason_team_avgs(eulcs_matches_oppteam_stats)
# Rename the columns of regular season team opponents averages DF
eulcs_regseason_teamopps_avgs_df <- eulcs_regseason_teamopps_avgs_df %>%
  rename_at(vars(win:riftHeraldKills), function(x) {
    return(paste("O", x, sep = "_"))
  })
# Make an average differential DF
eulcs_regseason_team_avgdiffs_df <- data.frame(teamName = eulcs_regseason_team_avgs_df$teamName, winPct = eulcs_regseason_team_avgs_df$win, duration = eulcs_regseason_team_avgs_df$duration, eulcs_regseason_team_avgs_df[4] - eulcs_regseason_teamopps_avgs_df[4], eulcs_regseason_team_avgs_df[6:56] - eulcs_regseason_teamopps_avgs_df[6:56])



# Side-by-side plots NALCS, EULCS Summoner Per Game Averages Distributions
# Kills
nalcs_plot_rsplayer_avgs +
  geom_histogram(mapping = aes(x = kills, y = ..density.., color = teamName, fill = teamRole), size = 1.25, alpha = .6, binwidth = .125) +
  labs(
    title = "Kills Per Game Histogram, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution of Kills")
eulcs_plot_rsplayer_avgs +
  geom_histogram(mapping = aes(x = kills, y = ..density.., color = teamName, fill = teamRole), size = 1.25, alpha = .6, binwidth = .125) +
  labs(
    title = "Kills Per Game Histogram, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution of Kills")
nalcs_plot_rsplayer_avgs +
  geom_density(mapping = aes(x = kills, color = teamRole, fill = teamRole), alpha = .3, size = 1.25) +
  labs(
    title = "Kills Per Game Density Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution of Kills Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_density(mapping = aes(x = kills, color = teamRole, fill = teamRole), alpha = .3, size = 1.25) +
  labs(
    title = "Kills Per Game Density Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution of Kills Across Team Roles")
nalcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = kills, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = kills, color = teamRole)) +
  labs(
    title = "Kills Per Game Box Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution of Kills Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = kills, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = kills, color = teamRole)) +
  labs(
    title = "Kills Per Game Box Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution of Kills Across Team Roles")
# Deaths
nalcs_plot_rsplayer_avgs +
  geom_histogram(mapping = aes(x = deaths, y = ..density.., color = teamName, fill = teamRole), size = 1.25, alpha = .6, binwidth = .125) +
  labs(
    title = "Deaths Per Game Histogram, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution of Deaths")
eulcs_plot_rsplayer_avgs +
  geom_histogram(mapping = aes(x = deaths, y = ..density.., color = teamName, fill = teamRole), size = 1.25, alpha = .6, binwidth = .125) +
  labs(
    title = "Deaths Per Game Histogram, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution of Deaths")
nalcs_plot_rsplayer_avgs +
  geom_density(mapping = aes(x = deaths, color = teamRole, fill = teamRole), alpha = .3, size = 1.25) +
  labs(
    title = "Deaths Per Game Density Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution of Deaths Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_density(mapping = aes(x = deaths, color = teamRole, fill = teamRole), alpha = .3, size = 1.25) +
  labs(
    title = "Deaths Per Game Density Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution of Deaths Across Team Roles")
nalcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = deaths, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = deaths, color = teamRole)) +
  labs(
    title = "Deaths Per Game Box Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution of Assists Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = deaths, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = deaths, color = teamRole)) +
  labs(
    title = "Deaths Per Game Box Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution of Assists Across Team Roles")
# Assists
nalcs_plot_rsplayer_avgs +
  geom_histogram(mapping = aes(x = assists, y = ..density.., color = teamName, fill = teamRole), size = 1.25, alpha = .6, binwidth = .125) +
  labs(
    title = "Assists Per Game Histogram, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution of Assists")
eulcs_plot_rsplayer_avgs +
  geom_histogram(mapping = aes(x = assists, y = ..density.., color = teamName, fill = teamRole), size = 1.25, alpha = .6, binwidth = .125) +
  labs(
    title = "Assists Per Game Histogram, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution of Assists")
nalcs_plot_rsplayer_avgs +
  geom_density(mapping = aes(x = assists, color = teamRole, fill = teamRole), alpha = .3, size = 1.25) +
  labs(
    title = "Assists Per Game Density Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution of Assists Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_density(mapping = aes(x = assists, color = teamRole, fill = teamRole), alpha = .3, size = 1.25) +
  labs(
    title = "Assists Per Game Density Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution of Assists Across Team Roles")
nalcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = assists, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = assists, color = teamRole)) +
  labs(
    title = "Assists Per Game Box Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution of Assists Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = assists, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = assists, color = teamRole)) +
  labs(
    title = "Assists Per Game Box Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution of Assists Across Team Roles")
# KDA ratio (Kills + Assists / Deaths)
nalcs_plot_rsplayer_avgs +
  geom_histogram(mapping = aes(x = (kills + assists) / deaths, y = ..density.., color = teamName, fill = teamRole), size = 1.25, alpha = .6, binwidth = .25) +
  labs(
    title = "KDA Ratio Histogram, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution of KDA Ratio")
eulcs_plot_rsplayer_avgs +
  geom_histogram(mapping = aes(x = (kills + assists) / deaths, y = ..density.., color = teamName, fill = teamRole), size = 1.25, alpha = .6, binwidth = .25) +
  labs(
    title = "KDA Ratio Histogram, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution of KDA Ratio")
nalcs_plot_rsplayer_avgs +
  geom_density(mapping = aes(x = (kills + assists) / deaths, color = teamRole, fill = teamRole), alpha = .3, size = 1.25) +
  labs(
    title = "KDA Ratio Density Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution of KDA Ratio Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_density(mapping = aes(x = (kills + assists) / deaths, color = teamRole, fill = teamRole), alpha = .3, size = 1.25) +
  labs(
    title = "KDA Ratio Density Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution of KDA Ratio Across Team Roles")
nalcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = (kills + assists) / deaths, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = (kills + assists) / deaths, color = teamRole)) +
  labs(
    title = "KDA Ratio Box Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution of KDA Ratio Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = (kills + assists) / deaths, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = (kills + assists) / deaths, color = teamRole)) +
  labs(
    title = "KDA Ratio Box Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution of KDA Ratio Across Team Roles")
# totalDamageDealtToChampions
nalcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = totalDamageDealtToChampions, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = totalDamageDealtToChampions, color = teamRole)) +
  labs(
    title = "Total Damage Dealt to Champions per Game Box Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = totalDamageDealtToChampions, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = totalDamageDealtToChampions, color = teamRole)) +
  labs(
    title = "Total Damage Dealt to Champions per Game Box Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
# magicDamageDealtToChampions
nalcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = magicDamageDealtToChampions, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = magicDamageDealtToChampions, color = teamRole)) +
  labs(
    title = "Magic Damage Dealt to Champions per Game Box Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = magicDamageDealtToChampions, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = magicDamageDealtToChampions, color = teamRole)) +
  labs(
    title = "Magic Damage Dealt to Champions per Game Box Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
# physicalDamageDealtToChampions
nalcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = physicalDamageDealtToChampions, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = physicalDamageDealtToChampions, color = teamRole)) +
  labs(
    title = "Physical Damage Dealt to Champions per Game Box Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = physicalDamageDealtToChampions, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = physicalDamageDealtToChampions, color = teamRole)) +
  labs(
    title = "Physical Damage Dealt to Champions per Game Box Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
# trueDamageDealtToChampions
nalcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = trueDamageDealtToChampions, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = trueDamageDealtToChampions, color = teamRole)) +
  labs(
    title = "True Damage Dealt to Champions per Game Box Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = trueDamageDealtToChampions, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = trueDamageDealtToChampions, color = teamRole)) +
  labs(
    title = "True Damage Dealt to Champions per Game Box Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
# totalHeal
nalcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = totalHeal, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = totalHeal, color = teamRole)) +
  labs(
    title = "Total Heal per Game Box Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = totalHeal, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = totalHeal, color = teamRole)) +
  labs(
    title = "Total Heal per Game Box Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
# totalUnitsHealed
nalcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = totalUnitsHealed, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = totalUnitsHealed, color = teamRole)) +
  labs(
    title = "Total Units Healed per Game Box Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = totalUnitsHealed, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = totalUnitsHealed, color = teamRole)) +
  labs(
    title = "Total Units Healed per Game Box Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
# totalDamageTaken
nalcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = totalDamageTaken, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = totalDamageTaken, color = teamRole)) +
  labs(
    title = "Total Damage Taken per Game Box Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = totalDamageTaken, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = totalDamageTaken, color = teamRole)) +
  labs(
    title = "Total Damage Taken per Game Box Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
# damageSelfMitigated
nalcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = damageSelfMitigated, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = damageSelfMitigated, color = teamRole)) +
  labs(
    title = "Damage Self-Mitigated per Game Box Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = damageSelfMitigated, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = damageSelfMitigated, color = teamRole)) +
  labs(
    title = "Damage Self-Mitigated per Game Box Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
# visionScore
nalcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = visionScore, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = visionScore, color = teamRole)) +
  labs(
    title = "Vision Score per Game Box Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = visionScore, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = visionScore, color = teamRole)) +
  labs(
    title = "Vision Score per Game Box Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
# visionWardsBoughtInGame
nalcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = visionWardsBoughtInGame, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = visionWardsBoughtInGame, color = teamRole)) +
  labs(
    title = "Vision Wards Bought per Game Box Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = visionWardsBoughtInGame, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = visionWardsBoughtInGame, color = teamRole)) +
  labs(
    title = "Vision Wards Bought per Game Box Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
# wardsPlaced
nalcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = wardsPlaced, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = wardsPlaced, color = teamRole)) +
  labs(
    title = "Wards Placed per Game Box Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = wardsPlaced, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = wardsPlaced, color = teamRole)) +
  labs(
    title = "Wards Placed per Game Box Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
# wardsKilled
nalcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = wardsKilled, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = wardsKilled, color = teamRole)) +
  labs(
    title = "Wards Killed per Game Box Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = wardsKilled, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = wardsKilled, color = teamRole)) +
  labs(
    title = "Wards Killed per Game Box Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
# timeCCingOthers
nalcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = timeCCingOthers, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = timeCCingOthers, color = teamRole)) +
  labs(
    title = "Time CCing Others per Game Box Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = timeCCingOthers, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = timeCCingOthers, color = teamRole)) +
  labs(
    title = "Time CCing Others per Game Box Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
# totalTimeCrowdControlDealt
nalcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = totalTimeCrowdControlDealt, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = totalTimeCrowdControlDealt, color = teamRole)) +
  labs(
    title = "Total Time Crowd Control Dealt per Game Box Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = totalTimeCrowdControlDealt, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = totalTimeCrowdControlDealt, color = teamRole)) +
  labs(
    title = "Total Time Crowd Control Dealt per Game Box Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
# totalMinionsKilled
nalcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = totalMinionsKilled, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = totalMinionsKilled, color = teamRole)) +
  labs(
    title = "Total (Non-neutal) Minions Killed per Game Box Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = totalMinionsKilled, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = totalMinionsKilled, color = teamRole)) +
  labs(
    title = "Total (Non-neutal) Minions Killed per Game Box Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
# neutralMinionsKilled
nalcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = neutralMinionsKilled, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = neutralMinionsKilled, color = teamRole)) +
  labs(
    title = "Neutral Minions Killed per Game Box Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = neutralMinionsKilled, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = neutralMinionsKilled, color = teamRole)) +
  labs(
    title = "Neutral Minions Killed per Game Box Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
# champLevel
nalcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = champLevel, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = champLevel, color = teamRole)) +
  labs(
    title = "Average Finishing Champ Level Box Plot, NALCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")
eulcs_plot_rsplayer_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = champLevel, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = champLevel, color = teamRole)) +
  labs(
    title = "Average Finishing Champ Level Box Plot, EULCS 2018 Spring Split Regular Season",
    subtitle = "Distribution Across Team Roles")


# Kill deficit per game
nalcs_matches_killdiff <- nalcs_matches_team_stats %>%
  group_by(gameNumber, duration) %>%
  sample_n(1) %>%
  transmute(killDiff = abs(kills - deaths))
eulcs_matches_killdiff <- eulcs_matches_team_stats %>%
  group_by(gameNumber, duration) %>%
  sample_n(1) %>%
  transmute(killDiff = abs(kills - deaths))
nalcs_plot_killdiff <- nalcs_matches_killdiff %>% ggplot()
eulcs_plot_killdiff <- eulcs_matches_killdiff %>% ggplot()
nalcs_plot_killdiff +
  geom_point(aes(x = duration, y = killDiff)) +
  geom_smooth(aes(x = duration, y = killDiff), method = "lm", se = T) +
  labs(title = "Kill Difference vs Game Duration, NALCS 2018 Spring Split (All Games)",
    subtitle = "Plus Best Fit Line")
eulcs_plot_killdiff +
  geom_point(aes(x = duration, y = killDiff)) +
  geom_smooth(aes(x = duration, y = killDiff), method = "lm", se = T) +
  labs(title = "Kill Difference vs Game Duration, EULCS 2018 Spring Split (All Games)",
    subtitle = "Plus Best Fit Line")
nalcs_plot_killdiff +
  geom_density(aes(x = killDiff)) +
  geom_vline(aes(xintercept = mean(killDiff)), linetype = "dashed") +
  labs(title = "Kill Difference Density Plot, NALCS 2018 Spring Split (All Games)",
    subtitle = "Plus Average Kill Difference")
eulcs_plot_killdiff +
  geom_density(aes(x = killDiff)) +
  geom_vline(aes(xintercept = mean(killDiff)), linetype = "dashed") +
  labs(title = "Kill Difference Density Plot, EULCS 2018 Spring Split (All Games)",
    subtitle = "Plus Average Kill Difference")
nalcs_plot_killdiff +
  geom_density(aes(x = duration)) +
  geom_vline(aes(xintercept = mean(duration)), linetype = "dashed") +
  labs(title = "Game Length Density Plot, NALCS 2018 Spring Split (All Games)",
    subtitle = "Plus Average Game Length")
eulcs_plot_killdiff +
  geom_density(aes(x = duration)) +
  geom_vline(aes(xintercept = mean(duration)), linetype = "dashed") +
  labs(title = "Game Length Density Plot, EULCS 2018 Spring Split (All Games)",
    subtitle = "Plus Average Game Length")



##################################################################
#################  Season Champion Averages ######################
##################################################################
library(dplyr)
library(tidyr)
library(ggplot2)
nalcs_matches_player_stats <- read.csv("datasets/nalcs/nalcs_spring2018_match_player_stats.csv")
eulcs_matches_player_stats <- read.csv("datasets/eulcs/eulcs_spring2018_match_player_stats.csv")
nalcs_season_champion_avgs <- get_league_season_champion_avgs(nalcs_matches_player_stats)
eulcs_season_champion_avgs <- get_league_season_champion_avgs(eulcs_matches_player_stats)
#### Facet-wrapped plots
nalcs_season_champ_avgs_gathered <- nalcs_season_champion_avgs %>%
  filter(wins + losses >= 6) %>%
  gather(kills, assists, magicDamageDealt, physicalDamageDealt, magicDamageDealtToChampions, physicalDamageDealtToChampions, totalHeal, totalUnitsHealed, damageSelfMitigated, totalDamageTaken, neutralMinionsKilled, timeCCingOthers, totalTimeCrowdControlDealt, champLevel, visionWardsBoughtInGame, wardsPlaced, wardsKilled, key = "varName", value = "valuePerGame")
nalcs_plot_champ_avgs <- nalcs_season_champ_avgs_gathered %>%
  ggplot()
nalcs_plot_champ_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = valuePerGame, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = valuePerGame, color = teamRole)) +
  facet_wrap(~varName, scales = "free", ncol = 3) +
  labs(
    title = "Champion Averages per Game Box Plots, NALCS 2018 Spring Split",
    subtitle = "Distribution Across Team Roles (minimum 6 games played)")
nalcs_plot_champ_avgs +
  geom_histogram(mapping = aes(x = valuePerGame, y = ..density.., fill = teamRole), color = "black", alpha = .6) +
  facet_wrap(~varName, scales = "free", ncol = 2) +
  labs(
    title = "Champion Averages Per Game Histograms, NALCS 2018 Spring Split",
    subtitle = "Distribution Across Team Roles (minimum 6 games played)")
nalcs_plot_champ_avgs +
  geom_density(mapping = aes(x = valuePerGame, color = teamRole, fill = teamRole), alpha = .3, size = 1) +
  facet_wrap(~varName, scales = "free", ncol = 2) +
  labs(
    title = "Champion Averages Per Game Density Plots, NALCS 2018 Spring Split",
  subtitle = "Distribution Across Team Roles (minimum 6 games played)")




################# Team Performance #####################
nalcs_plot_rsteam_totaldiffs <- nalcs_regseason_team_totaldiffs_df %>% ggplot();
eulcs_plot_rsteam_totaldiffs <- eulcs_regseason_team_totaldiffs_df %>% ggplot();
# Kills Diff
nalcs_plot_rsteam_totaldiffs +
  geom_point(mapping = aes(x = wins, y = kills, color = teamName), size = 3) +
  geom_smooth(aes(x = wins, y = kills), method = "lm", se = T) +
  labs()
eulcs_plot_rsteam_totaldiffs +
  geom_point(mapping = aes(x = wins, y = kills, color = teamName), size = 3) +
  geom_smooth(aes(x = wins, y = kills), method = "lm", se = T)
# Assists Diff
nalcs_plot_rsteam_totaldiffs +
  geom_point(mapping = aes(x = wins, y = assists, color = teamName), size = 3) +
  geom_smooth(aes(x = wins, y = assists), method = "lm", se = T)
eulcs_plot_rsteam_totaldiffs +
  geom_point(mapping = aes(x = wins, y = assists, color = teamName), size = 3) +
  geom_smooth(aes(x = wins, y = assists), method = "lm", se = T)


# Plots with gathered dataset and plotting with facet_wrap
library(tidyr)
library(ggplot2)
library(dplyr)
nalcs_season_summoner_avgs <- read.csv("datasets/nalcs/nalcs_spring2018_season_summoner_avgs.csv") %>%
  select(-X) %>%
  filter(wins + losses >= 6)
nalcs_season_summoner_avgs_gathered <- nalcs_season_summoner_avgs %>%
  gather(kills, assists, magicDamageDealt, physicalDamageDealt, magicDamageDealtToChampions, physicalDamageDealtToChampions, totalHeal, totalUnitsHealed, damageSelfMitigated, totalDamageTaken, neutralMinionsKilled, timeCCingOthers, totalTimeCrowdControlDealt, champLevel, visionWardsBoughtInGame, wardsPlaced, wardsKilled, key = "varName", value = "valuePerGame")
nalcs_plot_player_avgs <- nalcs_season_summoner_avgs_gathered %>%
  filter(wins + losses >= 6) %>%
  ggplot()
nalcs_plot_player_avgs +
  geom_boxplot(mapping = aes(x = teamRole, y = valuePerGame, fill = teamRole), size = 1.25, alpha = .6) +
  geom_jitter(width = 0.15, mapping = aes(x = teamRole, y = valuePerGame, color = teamRole)) +
  facet_wrap(~ varName, scales = "free", ncol = 3) +
  labs(
    title = "Player Averages per Game Box Plots, NALCS 2018 Spring Split",
    subtitle = "Distribution Across Team Roles")
nalcs_plot_player_avgs +
  geom_histogram(mapping = aes(x = valuePerGame, y = ..density.., fill = teamRole), color = "black", alpha = .6) +
  facet_wrap(~varName, scales = "free", ncol = 2) +
  labs(
    title = "Player Averages Per Game Histograms, NALCS 2018 Spring Split",
    subtitle = "Distribution of Values")
nalcs_plot_player_avgs +
  geom_density(mapping = aes(x = valuePerGame, color = teamRole, fill = teamRole), alpha = .3, size = 1) +
  facet_wrap(~varName, scales = "free", ncol = 2) +
  labs(
    title = "Player Averages Per Game Density Plots, NALCS 2018 Spring Split",
    subtitle = "Distribution of Values")



facet_histogram_plot_league_season_player_avgs <- function(league_season_player_avgs,
                                                 str_league_season = "2018 Spring Split",
                                                 min_games = 6) {
  league_season_player_avgs <- league_season_player_avgs %>%
    select(-X) %>%
    filter(wins + losses >= min_games)
  league_season_player_avgs_gathered <- league_season_player_avgs %>%
  gather(kills, assists, magicDamageDealt, physicalDamageDealt, magicDamageDealtToChampions, physicalDamageDealtToChampions, totalHeal, totalUnitsHealed, damageSelfMitigated, totalDamageTaken, neutralMinionsKilled, timeCCingOthers, totalTimeCrowdControlDealt, champLevel, visionWardsBoughtInGame, wardsPlaced, wardsKilled, key = "varName", value = "valuePerGame")

  league_season_player_avgs_gathered %>% ggplot() +
    geom_histogram(mapping = aes(x = valuePerGame, y = ..density.., fill = teamRole), color = "black", alpha = .6) +
    facet_wrap(~varName, scales = "free", ncol = 2) +
    labs(
      title = paste("Player Averages Per Game Histograms", str_league_season, sep = ", "),
      subtitle = "Distribution of Values")
}

facet_density_plot_league_season_player_avgs <- function(league_season_player_avgs,
                                                 str_league_season = "2018 Spring Split",
                                                 min_games = 6) {
  league_season_player_avgs <- league_season_player_avgs %>%
    select(-X) %>%
    filter(wins + losses >= min_games)
  league_season_player_avgs_gathered <- league_season_player_avgs %>%
  gather(kills, assists, magicDamageDealt, physicalDamageDealt, magicDamageDealtToChampions, physicalDamageDealtToChampions, totalHeal, totalUnitsHealed, damageSelfMitigated, totalDamageTaken, neutralMinionsKilled, timeCCingOthers, totalTimeCrowdControlDealt, champLevel, visionWardsBoughtInGame, wardsPlaced, wardsKilled, key = "varName", value = "valuePerGame")

league_season_player_avgs_gathered %>% ggplot() +
    geom_density(mapping = aes(x = valuePerGame, color = teamRole, fill = teamRole), alpha = .3, size = 1) +
    facet_wrap(~varName, scales = "free", ncol = 2) +
    labs(
      title = paste("Player Averages Per Game Density Plots", str_league_season, sep = ", "),
      subtitle = "Distribution of Values")
}

facet_box_plot_league_season_player_avgs <- function(league_season_player_avgs,
                                                 str_league_season = "2018 Spring Split",
                                                 min_games = 6) {
  league_season_player_avgs <- league_season_player_avgs %>%
    select(-X) %>%
    filter(wins + losses >= min_games)
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

msi_season_summoner_avgs <- read.csv("datasets/msi/msi_2018_season_summoner_avgs.csv")
facet_box_plot_league_season_player_avgs(msi_season_summoner_avgs, str_league_season = "MSI 2018")
lms_season_summoner_avgs <- read.csv("datasets/lms/lms_spring2018_season_summoner_avgs.csv")
facet_box_plot_league_season_player_avgs(lms_season_summoner_avgs, str_league_season = "LMS 2018 Spring Split", min_games = 5)
lck_season_summoner_avgs <- read.csv("datasets/lck/lck_spring2018_season_summoner_avgs.csv")
facet_box_plot_league_season_player_avgs(lck_season_summoner_avgs, str_league_season = "LCK 2018 Spring Split", min_games = 5)
nalcs_season_summoner_avgs <- read.csv("datasets/nalcs/nalcs_spring2018_season_summoner_avgs.csv")
eulcs_season_summoner_avgs <- read.csv("datasets/eulcs/eulcs_spring2018_season_summoner_avgs.csv")

all_leagues_summoner_avgs <- msi_season_summoner_avgs %>%
  bind_rows(lms_season_summoner_avgs) %>%
  bind_rows(lck_season_summoner_avgs) %>%
  bind_rows(nalcs_season_summoner_avgs) %>%
  bind_rows(eulcs_season_summoner_avgs)
facet_box_plot_league_season_player_avgs(all_leagues_summoner_avgs, str_league_season = "All Leagues 2018 Spring", min_games = 5)