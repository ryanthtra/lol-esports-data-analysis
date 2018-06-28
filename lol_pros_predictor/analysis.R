library(dplyr)
library(tidyr)

# The "template" datasets created from Script.R
nalcs_matches_champ_bans <- read.csv("datasets/nalcs/nalcs_spring2018_champ_bans.csv")
nalcs_matches_player_stats <- read.csv("datasets/nalcs/nalcs_spring2018_match_player_stats.csv")
nalcs_matches_team_stats <- read.csv("datasets/nalcs/nalcs_spring2018_match_team_stats.csv")


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
#                                  Analysis Helper Functions
#
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
get_league_regseason_team_totals <- function(league_matches_team_stats, split_bluered = FALSE) {
  # Filter for just regular season games
  league_regseason_tpc_df <- league_matches_team_stats %>%
  filter(isTiebreaker == FALSE & isPlayoff == FALSE)

  # Create a total stats DF grouped by teams
  league_regseason_team_totals_df <- data.frame(NULL)
  if (split_bluered == FALSE) {
    league_regseason_team_totals_df <-
    # First parentheses group: sums of stats by team
    (league_regseason_tpc_df %>%
      group_by(teamName) %>%
      summarise_at(vars(duration, kills:wardsKilled, towerKills:riftHeraldKills, 'creepsPerMinDeltas.10.20', 'creepsPerMinDeltas.0.10', 'xpPerMinDeltas.10.20', 'xpPerMinDeltas.0.10', 'goldPerMinDeltas.10.20', 'goldPerMinDeltas.0.10', 'damageTakenPerMinDeltas.10.20', 'damageTakenPerMinDeltas.0.10'), sum)) %>%
    # More parenthesis groups: tallying first-objective columns
    inner_join(league_regseason_tpc_df %>%
      group_by(teamName, firstBlood) %>%
      tally() %>% spread(firstBlood, n) %>% select('TRUE') %>% rename('firstBloods' = 'TRUE')) %>%
    inner_join(league_regseason_tpc_df %>%
      group_by(teamName, firstTower) %>%
      tally() %>% spread(firstTower, n) %>% select('TRUE') %>% rename('firstTowers' = 'TRUE')) %>%
    inner_join(league_regseason_tpc_df %>%
      group_by(teamName, firstInhibitor) %>%
      tally() %>% spread(firstInhibitor, n) %>% select('TRUE') %>% rename('firstInhibitors' = 'TRUE')) %>%
    inner_join(league_regseason_tpc_df %>%
      group_by(teamName, firstBaron) %>%
      tally() %>% spread(firstBaron, n) %>% select('TRUE') %>% rename('firstBarons' = 'TRUE')) %>%
    inner_join(league_regseason_tpc_df %>%
      group_by(teamName, firstDragon) %>%
      tally() %>% spread(firstDragon, n) %>% select('TRUE') %>% rename('firstDragons' = 'TRUE')) %>%
    # Last parentheses group: tallying wins and losses by team
    inner_join(league_regseason_tpc_df %>%
      group_by(teamName, win) %>%
      tally() %>%
      spread(win, n) %>% # "transposes" the DF so that TRUE (win) and FALSE (loss) are the column names
      rename('losses' = 'FALSE', 'wins' = 'TRUE')) # renames the T/F columns to W/L

    # Reordering columns - teamName, wins, losses, <everything else>
    league_regseason_team_totals_df <- league_regseason_team_totals_df[, c(1, 56, 55, 2:54)]
  } else {
    league_regseason_team_totals_df <-
    # First parentheses group: sums of stats by team
    (league_regseason_tpc_df %>%
      group_by(teamName, teamId) %>%
      summarise_at(vars(duration, kills:wardsKilled, towerKills:riftHeraldKills, 'creepsPerMinDeltas.10.20', 'creepsPerMinDeltas.0.10', 'xpPerMinDeltas.10.20', 'xpPerMinDeltas.0.10', 'goldPerMinDeltas.10.20', 'goldPerMinDeltas.0.10', 'damageTakenPerMinDeltas.10.20', 'damageTakenPerMinDeltas.0.10'), sum)) %>%
    # More parenthesis groups: tallying first-objective columns
    inner_join(league_regseason_tpc_df %>%
      group_by(teamName, teamId, firstBlood) %>%
      tally() %>% spread(firstBlood, n) %>% select('TRUE') %>% rename('firstBloods' = 'TRUE')) %>%
    inner_join(league_regseason_tpc_df %>%
      group_by(teamName, teamId, firstTower) %>%
      tally() %>% spread(firstTower, n) %>% select('TRUE') %>% rename('firstTowers' = 'TRUE')) %>%
    inner_join(league_regseason_tpc_df %>%
      group_by(teamName, teamId, firstInhibitor) %>%
      tally() %>% spread(firstInhibitor, n) %>% select('TRUE') %>% rename('firstInhibitors' = 'TRUE')) %>%
    inner_join(league_regseason_tpc_df %>%
      group_by(teamName, teamId, firstBaron) %>%
      tally() %>% spread(firstBaron, n) %>% select('TRUE') %>% rename('firstBarons' = 'TRUE')) %>%
    inner_join(league_regseason_tpc_df %>%
      group_by(teamName, teamId, firstDragon) %>%
      tally() %>% spread(firstDragon, n) %>% select('TRUE') %>% rename('firstDragons' = 'TRUE')) %>%
    # Last parentheses group: tallying wins and losses by team
    inner_join(league_regseason_tpc_df %>%
      group_by(teamName, teamId, win) %>%
      tally() %>%
      spread(win, n) %>% # "transposes" the DF so that TRUE (win) and FALSE (loss) are the column names
      rename('losses' = 'FALSE', 'wins' = 'TRUE')) # renames the T/F columns to W/L

    # Reordering columns - teamName, wins, losses, <everything else>
    league_regseason_team_totals_df <- league_regseason_team_totals_df[, c(1, 2, 57, 56, 3:55)]
  }
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
get_league_regseason_team_avgs <- function(league_matches_team_stats, split_bluered = FALSE) {
  # Filter for just regular season games
  league_regseason_tpc_df <- league_matches_team_stats %>%
  filter(isTiebreaker == FALSE & isPlayoff == FALSE)

  ## Create a avg stats DF grouped by teams
  if (split_bluered) {
    league_regseason_team_totals_df <-
    (league_regseason_tpc_df %>%
    group_by(teamName, teamId) %>%
    tally() %>% rename(games = n)) %>%
    inner_join(league_regseason_tpc_df %>%
    group_by(teamName, teamId) %>%
    summarise_at(vars(win, duration, kills:riftHeraldKills), mean))
  } else {
    league_regseason_team_totals_df <-
    (league_regseason_tpc_df %>%
    group_by(teamName) %>%
    summarise_at(vars(win, duration, kills:riftHeraldKills), mean))
  }
}


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
get_league_regseason_summoner_avgs <- function(league_matches_player_stats) {
  # Filter for just regular season games
  league_regseason_participants_accum <- league_matches_player_stats %>%
    filter(isTiebreaker == FALSE & isPlayoff == FALSE)

  # Create avg stats DF groups by lane and role
  league_regseason_participants_accum <-
  (league_regseason_participants_accum %>%
  group_by(teamName, summonerName, teamRole) %>%
  summarize_at(vars(duration, kills:assists, totalDamageDealt:trueDamageDealt, totalDamageDealtToChampions:firstBloodKill, firstTowerKill:firstInhibitorAssist, 'creepsPerMinDeltas.10.20', 'creepsPerMinDeltas.0.10', 'xpPerMinDeltas.10.20', 'xpPerMinDeltas.0.10', 'goldPerMinDeltas.10.20', 'goldPerMinDeltas.0.10', 'damageTakenPerMinDeltas.10.20', 'damageTakenPerMinDeltas.0.10'), mean)) %>%
  # Tallying wins and losses by summoner name
  inner_join(league_regseason_participants_accum %>%
      group_by(teamName, summonerName, teamRole, win) %>%
      tally() %>%
      spread(win, n) %>% # "transposes" the DF so that TRUE (win) and FALSE (loss) are the column names
      rename('losses' = 'FALSE', 'wins' = 'TRUE') %>% # renames the T/F columns to W/L
      mutate_at(vars(wins, losses), funs(replace(., is.na(.), 0))))

  # Reordering columns - teamName, wins, losses, <everything else>
  league_regseason_participants_accum <- league_regseason_participants_accum[, c(1, 2, 55, 54, 3:53)]
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
get_league_regseason_summoner_totals <- function(league_matches_player_stats, split_bluered = FALSE) {
  # Filter for just regular season games
  league_regseason_participants_df <- league_matches_player_stats %>%
  filter(isTiebreaker == FALSE & isPlayoff == FALSE)

  # Create a total stats DF grouped by teams
  league_regseason_summoners_totals_df <- data.frame(NULL)
  if (split_bluered == FALSE) {
    league_regseason_summoners_totals_df <-
    # First parentheses group: sums of stats by team
    (league_regseason_participants_df %>%
      group_by(teamName, summonerName, teamRole) %>%
      summarise_at(vars(duration, kills:wardsKilled, 'creepsPerMinDeltas.10.20', 'creepsPerMinDeltas.0.10', 'xpPerMinDeltas.10.20', 'xpPerMinDeltas.0.10', 'goldPerMinDeltas.10.20', 'goldPerMinDeltas.0.10', 'damageTakenPerMinDeltas.10.20', 'damageTakenPerMinDeltas.0.10'), sum)) %>%
    # More parenthesis groups: tallying first-objective columns
    inner_join(league_regseason_participants_df %>%
      group_by(teamName, summonerName, firstBloodKill) %>%
      tally() %>% spread(firstBloodKill, n) %>% select('TRUE') %>% rename('firstBloodKills' = 'TRUE')) %>%
    ## All firstBloodAssists are FALSE, so commented out
    #inner_join(league_regseason_participants_df %>%
    #group_by(teamName, summonerName, firstBloodAssist) %>%
    #tally() %>% spread(firstBloodAssist, n) %>% select('TRUE') %>% rename('firstBloodAssists' = 'TRUE')) %>%
    inner_join(league_regseason_participants_df %>%
      group_by(teamName, summonerName, firstTowerKill) %>%
      tally() %>% spread(firstTowerKill, n) %>% select('TRUE') %>% rename('firstTowerKills' = 'TRUE')) %>%
    inner_join(league_regseason_participants_df %>%
      group_by(teamName, summonerName, firstTowerAssist) %>%
      tally() %>% spread(firstTowerAssist, n) %>% select('TRUE') %>% rename('firstTowerAssists' = 'TRUE')) %>%
    inner_join(league_regseason_participants_df %>%
      group_by(teamName, summonerName, firstInhibitorKill) %>%
      tally() %>% spread(firstInhibitorKill, n) %>% select('TRUE') %>% rename('firstInhibitorKills' = 'TRUE')) %>%
    inner_join(league_regseason_participants_df %>%
      group_by(teamName, summonerName, firstInhibitorAssist) %>%
      tally() %>% spread(firstInhibitorAssist, n) %>% select('TRUE') %>% rename('firstInhibitorAssists' = 'TRUE')) %>%
    # Last parentheses group: tallying wins and losses by summoner
    inner_join(league_regseason_participants_df %>%
      group_by(teamName, summonerName, win) %>%
      tally() %>%
      spread(win, n) %>% # "transposes" the DF so that TRUE (win) and FALSE (loss) are the column names
      rename('losses' = 'FALSE', 'wins' = 'TRUE')) # renames the T/F columns to W/L

    league_regseason_summoners_totals_df = league_regseason_summoners_totals_df %>%
    mutate_at(vars(firstBloodKills:firstInhibitorAssists), funs(replace(., is.na(.), 0)))

    # Reordering columns - teamName, wins, losses, <everything else>
    league_regseason_summoners_totals_df <- league_regseason_summoners_totals_df[, c(1, 2, 65, 64, 3:63)]
  } else {
    league_regseason_summoners_totals_df <-
    # First parentheses group: sums of stats by team
    (league_regseason_participants_df %>%
      group_by(teamName, teamId, summonerName) %>%
      summarise_at(vars(kills:wardsKilled, 'creepsPerMinDeltas.10.20', 'creepsPerMinDeltas.0.10', 'xpPerMinDeltas.10.20', 'xpPerMinDeltas.0.10', 'goldPerMinDeltas.10.20', 'goldPerMinDeltas.0.10', 'damageTakenPerMinDeltas.10.20', 'damageTakenPerMinDeltas.0.10'), sum)) %>%
    # More parenthesis groups: tallying first-objective columns
    inner_join(league_regseason_participants_df %>%
      group_by(teamName, teamId, summonerName, firstBloodKill) %>%
      tally() %>% spread(firstBloodKill, n) %>% select('TRUE') %>% rename('firstBloodKills' = 'TRUE')) %>%
    ## All firstBloodAssists are FALSE, so commented out
    #inner_join(league_regseason_participants_df %>%
    #group_by(teamName, teamId, summonerName, firstBloodAssist) %>%
    #tally() %>% spread(firstBloodAssist, n) %>% select('TRUE') %>% rename('firstBloodAssists' = 'TRUE')) %>%
    inner_join(league_regseason_participants_df %>%
      group_by(teamName, teamId, summonerName, firstTowerKill) %>%
      tally() %>% spread(firstTowerKill, n) %>% select('TRUE') %>% rename('firstTowerKills' = 'TRUE')) %>%
    inner_join(league_regseason_participants_df %>%
      group_by(teamName, teamId, summonerName, firstTowerAssist) %>%
      tally() %>% spread(firstTowerAssist, n) %>% select('TRUE') %>% rename('firstTowerAssists' = 'TRUE')) %>%
    inner_join(league_regseason_participants_df %>%
      group_by(teamName, teamId, summonerName, firstInhibitorKill) %>%
      tally() %>% spread(firstInhibitorKill, n) %>% select('TRUE') %>% rename('firstInhibitorKills' = 'TRUE')) %>%
    inner_join(league_regseason_participants_df %>%
      group_by(teamName, teamId, summonerName, firstInhibitorAssist) %>%
      tally() %>% spread(firstInhibitorAssist, n) %>% select('TRUE') %>% rename('firstInhibitorAssists' = 'TRUE')) %>%
    # Last parentheses group: tallying wins and losses by summoner
    inner_join(league_regseason_participants_df %>%
      group_by(teamName, teamId, summonerName, win) %>%
      tally() %>%
      spread(win, n) %>% # "transposes" the DF so that TRUE (win) and FALSE (loss) are the column names
      rename('losses' = 'FALSE', 'wins' = 'TRUE') %>% # renames the T/F columns to W/L
      mutate_at(vars(wins, losses), funs(replace(., is.na(.), 0))))

    # Reordering columns - teamName, wins, losses, <everything else>
    #league_regseason_summoners_totals_df <- league_regseason_summoners_totals_df[, c(1, 54, 53, 2:52)]
  }
}



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
#                                  START -- ANALYSIS "MAIN" CODE
#
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Get opponent's data (just swapping the team names of each game in the previous DF)
nalcs_matches_oppteam_stats <- nalcs_matches_team_stats %>%
  group_by(gameNumber) %>%
  mutate(teamName = ifelse(teamId == "Blue", as.character(lead(teamName)), as.character(lag(teamName))))
nalcs_regseason_team_totals_df <- get_league_regseason_team_totals(nalcs_matches_team_stats)
nalcs_regseason_teamopps_totals_df <- get_league_regseason_team_totals(nalcs_matches_oppteam_stats)
# Rename the columns of regular season team opponents totals DF
nalcs_regseason_teamopps_totals_df <- nalcs_regseason_teamopps_totals_df %>%
  rename_at(vars(wins:firstDragons), function(x) {
    return(paste("O", x, sep = "_"))
  })
# Make a differential DF
nalcs_regseason_team_totaldiffs_df <- data.frame(teamName = nalcs_regseason_team_totals_df$teamName, wins = nalcs_regseason_team_totals_df$wins, losses = nalcs_regseason_team_totals_df$losses, totalDuration = nalcs_regseason_team_totals_df$duration, nalcs_regseason_team_totals_df[5] - nalcs_regseason_teamopps_totals_df[5], nalcs_regseason_team_totals_df[7:56] - nalcs_regseason_teamopps_totals_df[7:56])

nalcs_regseason_team_bluered_totals_df <- get_league_regseason_team_totals(nalcs_matches_team_stats, split_bluered = TRUE)
nalcs_regseason_team_avgs_df <- get_league_regseason_team_avgs(nalcs_matches_team_stats)
nalcs_regseason_teamopps_avgs_df <- get_league_regseason_team_avgs(nalcs_matches_oppteam_stats)
# Rename the columns of regular season team opponents averages DF
nalcs_regseason_teamopps_avgs_df <- nalcs_regseason_teamopps_avgs_df %>%
  rename_at(vars(win:riftHeraldKills), function(x) {
    return(paste("O", x, sep = "_"))
  })
# Make an average differential DF
nalcs_regseason_team_avgdiffs_df <- data.frame(teamName = nalcs_regseason_team_avgs_df$teamName, winPct = nalcs_regseason_team_avgs_df$win, duration = nalcs_regseason_team_avgs_df$duration, nalcs_regseason_team_avgs_df[4] - nalcs_regseason_teamopps_avgs_df[4], nalcs_regseason_team_avgs_df[6:56] - nalcs_regseason_teamopps_avgs_df[6:56])

nalcs_regseason_team_bluered_avgs_df <- get_league_regseason_team_avgs(nalcs_matches_team_stats, split_bluered = TRUE)
nalcs_regseason_summoner_avgs_df <- get_league_regseason_summoner_avgs(nalcs_matches_player_stats)
nalcs_regseason_summoner_totals_df <- get_league_regseason_summoner_totals(nalcs_matches_player_stats)