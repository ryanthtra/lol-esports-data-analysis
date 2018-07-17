library(dplyr)
library(tidyr)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
get_league_winprobs <- function(league_match_team_stats, split_teams = FALSE, split_bluered = FALSE) {
  if (split_teams == FALSE && split_bluered == FALSE) {
    # League as a whole
    league_match_team_stats <- (league_match_team_stats %>%
      group_by(firstBlood) %>%
      summarise_at(vars(win), mean) %>%
      spread(firstBlood, win) %>% rename('firstBloodWinPct' = 'TRUE') %>%
      select(firstBloodWinPct)
    ) %>% bind_cols(league_match_team_stats %>%
      group_by(firstTower) %>%
      summarise_at(vars(win), mean) %>%
      spread(firstTower, win) %>% rename('firstTowerWinPct' = 'TRUE') %>%
      select(firstTowerWinPct)
    ) %>% bind_cols(league_match_team_stats %>%
      group_by(firstInhibitor) %>%
      summarise_at(vars(win), mean) %>%
      spread(firstInhibitor, win) %>% rename('firstInhibitorWinPct' = 'TRUE') %>%
      select(firstInhibitorWinPct)
    ) %>% bind_cols(league_match_team_stats %>%
      group_by(firstBaron) %>%
      summarise_at(vars(win), mean) %>%
      spread(firstBaron, win) %>% rename('firstBaronWinPct' = 'TRUE') %>%
      select(firstBaronWinPct)
    ) %>% bind_cols(league_match_team_stats %>%
      group_by(firstDragon) %>%
      summarise_at(vars(win), mean) %>%
      spread(firstDragon, win) %>% rename('firstDragonWinPct' = 'TRUE') %>%
      select(firstDragonWinPct)
    ) %>% bind_cols(league_match_team_stats %>%
      group_by(firstRiftHerald) %>%
      summarise_at(vars(win), mean) %>%
      spread(firstRiftHerald, win) %>% rename('firstRiftHeraldWinPct' = 'TRUE') %>%
      select(firstRiftHeraldWinPct)
    )
  } else if (split_teams == FALSE && split_bluered == TRUE) {
    # League as a whole split by blue/red teams
    league_match_team_stats <- (league_match_team_stats %>%
      group_by(teamId, firstBlood) %>%
      summarise_at(vars(win), mean) %>%
      spread(firstBlood, win) %>% rename('firstBloodWinPct' = 'TRUE') %>%
      select(firstBloodWinPct)
    ) %>% inner_join(league_match_team_stats %>%
      group_by(teamId, firstTower) %>%
      summarise_at(vars(win), mean) %>%
      spread(firstTower, win) %>% rename('firstTowerWinPct' = 'TRUE') %>%
      select(firstTowerWinPct)
    ) %>% inner_join(league_match_team_stats %>%
      group_by(teamId, firstInhibitor) %>%
      summarise_at(vars(win), mean) %>%
      spread(firstInhibitor, win) %>% rename('firstInhibitorWinPct' = 'TRUE') %>%
      select(firstInhibitorWinPct)
    ) %>% inner_join(league_match_team_stats %>%
      group_by(teamId, firstBaron) %>%
      summarise_at(vars(win), mean) %>%
      spread(firstBaron, win) %>% rename('firstBaronWinPct' = 'TRUE') %>%
      select(firstBaronWinPct)
    ) %>% inner_join(league_match_team_stats %>%
      group_by(teamId, firstDragon) %>%
      summarise_at(vars(win), mean) %>%
      spread(firstDragon, win) %>% rename('firstDragonWinPct' = 'TRUE') %>%
      select(firstDragonWinPct)
    ) %>% inner_join(league_match_team_stats %>%
      group_by(teamId, firstRiftHerald) %>%
      summarise_at(vars(win), mean) %>%
      spread(firstRiftHerald, win) %>% rename('firstRiftHeraldWinPct' = 'TRUE') %>%
      select(firstRiftHeraldWinPct)
    )
  } else if (split_teams == TRUE && split_bluered == FALSE) {
    # Teams' events win probabilities
    league_match_team_stats <- (league_match_team_stats %>%
      group_by(teamName, firstBlood) %>%
      summarise_at(vars(win), mean) %>%
      spread(firstBlood, win) %>% rename('firstBloodWinPct' = 'TRUE') %>%
      select(firstBloodWinPct)
    ) %>% inner_join(league_match_team_stats %>%
      group_by(teamName, firstTower) %>%
      summarise_at(vars(win), mean) %>%
      spread(firstTower, win) %>% rename('firstTowerWinPct' = 'TRUE') %>%
      select(firstTowerWinPct)
    ) %>% inner_join(league_match_team_stats %>%
      group_by(teamName, firstInhibitor) %>%
      summarise_at(vars(win), mean) %>%
      spread(firstInhibitor, win) %>% rename('firstInhibitorWinPct' = 'TRUE') %>%
      select(firstInhibitorWinPct)
    ) %>% inner_join(league_match_team_stats %>%
      group_by(teamName, firstBaron) %>%
      summarise_at(vars(win), mean) %>%
      spread(firstBaron, win) %>% rename('firstBaronWinPct' = 'TRUE') %>%
      select(firstBaronWinPct)
    ) %>% inner_join(league_match_team_stats %>%
      group_by(teamName, firstDragon) %>%
      summarise_at(vars(win), mean) %>%
      spread(firstDragon, win) %>% rename('firstDragonWinPct' = 'TRUE') %>%
      select(firstDragonWinPct)
    ) %>% inner_join(league_match_team_stats %>%
      group_by(teamName, firstRiftHerald) %>%
      summarise_at(vars(win), mean) %>%
      spread(firstRiftHerald, win) %>% rename('firstRiftHeraldWinPct' = 'TRUE') %>%
      select(firstRiftHeraldWinPct)
    )
  } else if (split_teams == TRUE && split_bluered == TRUE) {
    # League as a whole split by blue/red teams
    league_match_team_stats <- (league_match_team_stats %>%
      group_by(teamName, teamId, firstBlood) %>%
      summarise_at(vars(win), mean) %>%
      spread(firstBlood, win) %>% rename('firstBloodWinPct' = 'TRUE') %>%
      select(firstBloodWinPct)
    ) %>% inner_join(league_match_team_stats %>%
      group_by(teamName, teamId, firstTower) %>%
      summarise_at(vars(win), mean) %>%
      spread(firstTower, win) %>% rename('firstTowerWinPct' = 'TRUE') %>%
      select(firstTowerWinPct)
    ) %>% inner_join(league_match_team_stats %>%
      group_by(teamName, teamId, firstInhibitor) %>%
      summarise_at(vars(win), mean) %>%
      spread(firstInhibitor, win) %>% rename('firstInhibitorWinPct' = 'TRUE') %>%
      select(firstInhibitorWinPct)
    ) %>% inner_join(league_match_team_stats %>%
      group_by(teamName, teamId, firstBaron) %>%
      summarise_at(vars(win), mean) %>%
      spread(firstBaron, win) %>% rename('firstBaronWinPct' = 'TRUE') %>%
      select(firstBaronWinPct)
    ) %>% inner_join(league_match_team_stats %>%
      group_by(teamName, teamId, firstDragon) %>%
      summarise_at(vars(win), mean) %>%
      spread(firstDragon, win) %>% rename('firstDragonWinPct' = 'TRUE') %>%
      select(firstDragonWinPct)
    ) %>% inner_join(league_match_team_stats %>%
      group_by(teamName, teamId, firstRiftHerald) %>%
      summarise_at(vars(win), mean) %>%
      spread(firstRiftHerald, win) %>% rename('firstRiftHeraldWinPct' = 'TRUE') %>%
      select(firstRiftHeraldWinPct)
    )
  }
}

nalcs_matches_team_stats <- read.csv("datasets/nalcs/nalcs_spring2018_match_team_stats.csv")
eulcs_matches_team_stats <- read.csv("datasets/eulcs/eulcs_spring2018_match_team_stats.csv")
lck_matches_team_stats <- read.csv("datasets/lck/lck_spring2018_match_team_stats.csv")
lms_matches_team_stats <- read.csv("datasets/lms/lms_spring2018_match_team_stats.csv")
msi_matches_team_stats <- read.csv("datasets/msi/msi_2018_match_team_stats.csv")
