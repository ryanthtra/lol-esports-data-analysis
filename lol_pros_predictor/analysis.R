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
get_league_regseason_team_avgs <- function(league_matches_team_stats, split_bluered = FALSE, split_winloss = FALSE) {
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
get_league_season_summoner_avgs <- function(league_matches_player_stats, only_regseason = TRUE, split_winloss = FALSE, split_bluered = FALSE) {
  # Filter for just regular season games
  league_season_participants_accum <- league_matches_player_stats
  if (only_regseason == TRUE) {
    league_season_participants_accum <- league_matches_player_stats %>%
      filter(isTiebreaker == FALSE & isPlayoff == FALSE)
  } 

  # Create avg stats DF groups by lane and role
  if (split_winloss == FALSE && split_bluered == FALSE) {

    league_season_participants_accum <-
    (league_season_participants_accum %>%
    group_by(teamName, summonerName, teamRole) %>%
    summarize_at(vars(duration, kills:assists, totalDamageDealt:trueDamageDealt, totalDamageDealtToChampions:firstBloodKill, firstTowerKill:firstInhibitorAssist, 'creepsPerMinDeltas.10.20', 'creepsPerMinDeltas.0.10', 'xpPerMinDeltas.10.20', 'xpPerMinDeltas.0.10', 'goldPerMinDeltas.10.20', 'goldPerMinDeltas.0.10', 'damageTakenPerMinDeltas.10.20', 'damageTakenPerMinDeltas.0.10'), mean)) %>%
    # Tallying wins and losses by summoner name
    inner_join(league_season_participants_accum %>%
      group_by(teamName, summonerName, teamRole, win) %>%
      tally() %>%
      spread(win, n) %>% # "transposes" the DF so that TRUE (win) and FALSE (loss) are the column names
      rename('losses' = 'FALSE', 'wins' = 'TRUE') %>% # renames the T/F columns to W/L
      mutate_at(vars(wins, losses), funs(replace(., is.na(.), 0))))

    # Adding KDA Ratio column
    league_season_participants_accum <- league_season_participants_accum %>%
      mutate(KDA = (kills + assists) / deaths)

    # Reordering columns - teamName, wins, losses, <everything else>
    league_season_participants_accum <- league_season_participants_accum[, c(1, 2, 55, 54, 3:7, 56, 8:53)]

  } else if(split_winloss == TRUE && split_bluered == FALSE) {

    league_season_participants_accum <-
    (league_season_participants_accum %>%
    group_by(teamName, summonerName, teamRole, win) %>%
    summarize_at(vars(duration, kills:assists, totalDamageDealt:trueDamageDealt, totalDamageDealtToChampions:firstBloodKill, firstTowerKill:firstInhibitorAssist, 'creepsPerMinDeltas.10.20', 'creepsPerMinDeltas.0.10', 'xpPerMinDeltas.10.20', 'xpPerMinDeltas.0.10', 'goldPerMinDeltas.10.20', 'goldPerMinDeltas.0.10', 'damageTakenPerMinDeltas.10.20', 'damageTakenPerMinDeltas.0.10'), mean)) %>%
    inner_join(league_season_participants_accum %>%
      group_by(teamName, summonerName, teamRole, win) %>%
      tally())

    # Reordering columns 
    league_season_participants_accum <- league_season_participants_accum[, c(1:4, 55, 5:54)]

  } else if (split_winloss == FALSE && split_bluered == TRUE) {

    league_season_participants_accum <-
    (league_season_participants_accum %>%
    group_by(teamName, summonerName, teamRole, teamId) %>%
    summarize_at(vars(duration, kills:assists, totalDamageDealt:trueDamageDealt, totalDamageDealtToChampions:firstBloodKill, firstTowerKill:firstInhibitorAssist, 'creepsPerMinDeltas.10.20', 'creepsPerMinDeltas.0.10', 'xpPerMinDeltas.10.20', 'xpPerMinDeltas.0.10', 'goldPerMinDeltas.10.20', 'goldPerMinDeltas.0.10', 'damageTakenPerMinDeltas.10.20', 'damageTakenPerMinDeltas.0.10'), mean)) %>%
    inner_join(league_season_participants_accum %>%
      group_by(teamName, summonerName, teamRole, teamId) %>%
      tally())

    # Reordering columns 
    league_season_participants_accum <- league_season_participants_accum[, c(1:4, 55, 5:54)]

  } else {

    league_season_participants_accum <-
    (league_season_participants_accum %>%
    group_by(teamName, summonerName, teamRole, teamId, win) %>%
    summarize_at(vars(duration, kills:assists, totalDamageDealt:trueDamageDealt, totalDamageDealtToChampions:firstBloodKill, firstTowerKill:firstInhibitorAssist, 'creepsPerMinDeltas.10.20', 'creepsPerMinDeltas.0.10', 'xpPerMinDeltas.10.20', 'xpPerMinDeltas.0.10', 'goldPerMinDeltas.10.20', 'goldPerMinDeltas.0.10', 'damageTakenPerMinDeltas.10.20', 'damageTakenPerMinDeltas.0.10'), mean)) %>%
    inner_join(league_season_participants_accum %>%
      group_by(teamName, summonerName, teamRole, teamId, win) %>%
      tally())

    # Reordering columns 
    #league_season_participants_accum <- league_season_participants_accum[, c(1:4, 55, 5:54)]
  }

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


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
get_league_season_champion_avgs <- function(league_matches_player_stats, split_winloss = FALSE, split_bluered = FALSE) {
  ## Filter for just regular season games
  #league_regseason_participants_accum <- league_matches_player_stats %>%
    #filter(isTiebreaker == FALSE & isPlayoff == FALSE)

  # Create avg stats DF groups by lane and role
  if (split_winloss == FALSE && split_bluered == FALSE) {

    league_matches_player_stats <-
    (league_matches_player_stats %>%
    group_by(name, teamRole) %>%
    summarize_at(vars(duration, kills:assists, totalDamageDealt:trueDamageDealt, totalDamageDealtToChampions:firstBloodKill, firstTowerKill:firstInhibitorAssist, 'creepsPerMinDeltas.10.20', 'creepsPerMinDeltas.0.10', 'xpPerMinDeltas.10.20', 'xpPerMinDeltas.0.10', 'goldPerMinDeltas.10.20', 'goldPerMinDeltas.0.10', 'damageTakenPerMinDeltas.10.20', 'damageTakenPerMinDeltas.0.10'), mean)) %>%
    # Tallying wins and losses by summoner name
    inner_join(league_matches_player_stats %>%
      group_by(name, teamRole, win) %>%
      tally() %>%
      spread(win, n) %>% # "transposes" the DF so that TRUE (win) and FALSE (loss) are the column names
      rename('losses' = 'FALSE', 'wins' = 'TRUE') %>% # renames the T/F columns to W/L
      mutate_at(vars(wins, losses), funs(replace(., is.na(.), 0))))

    # Adding KDA Ratio column
    league_matches_player_stats <- league_matches_player_stats %>%
      mutate(KDA = (kills + assists) / deaths)

    # Reordering columns - teamName, wins, losses, <everything else>
    league_matches_player_stats <- league_matches_player_stats[, c(1, 2, 54, 53, 3:6, 55, 7:52)]

  } else if (split_winloss == TRUE && split_bluered == FALSE) {

    league_matches_player_stats <-
    (league_matches_player_stats %>%
    group_by(name, teamRole, win) %>%
    summarize_at(vars(duration, kills:assists, totalDamageDealt:trueDamageDealt, totalDamageDealtToChampions:firstBloodKill, firstTowerKill:firstInhibitorAssist, 'creepsPerMinDeltas.10.20', 'creepsPerMinDeltas.0.10', 'xpPerMinDeltas.10.20', 'xpPerMinDeltas.0.10', 'goldPerMinDeltas.10.20', 'goldPerMinDeltas.0.10', 'damageTakenPerMinDeltas.10.20', 'damageTakenPerMinDeltas.0.10'), mean)) %>%
    inner_join(league_matches_player_stats %>%
      group_by(name, teamRole, win) %>%
      tally())

    # Reordering columns 
    league_matches_player_stats <- league_matches_player_stats[, c(1:4, 55, 5:54)]

  } else if (split_winloss == FALSE && split_bluered == TRUE) {

    league_matches_player_stats <-
    (league_matches_player_stats %>%
    group_by(name, teamRole, teamId) %>%
    summarize_at(vars(duration, kills:assists, totalDamageDealt:trueDamageDealt, totalDamageDealtToChampions:firstBloodKill, firstTowerKill:firstInhibitorAssist, 'creepsPerMinDeltas.10.20', 'creepsPerMinDeltas.0.10', 'xpPerMinDeltas.10.20', 'xpPerMinDeltas.0.10', 'goldPerMinDeltas.10.20', 'goldPerMinDeltas.0.10', 'damageTakenPerMinDeltas.10.20', 'damageTakenPerMinDeltas.0.10'), mean)) %>%
    inner_join(league_matches_player_stats %>%
      group_by(name, teamRole, teamId) %>%
      tally())

    # Reordering columns 
    league_matches_player_stats <- league_matches_player_stats[, c(1:4, 55, 5:54)]

  } else {

    league_matches_player_stats <-
    (league_matches_player_stats %>%
    group_by(name, teamRole, teamId, win) %>%
    summarize_at(vars(duration, kills:assists, totalDamageDealt:trueDamageDealt, totalDamageDealtToChampions:firstBloodKill, firstTowerKill:firstInhibitorAssist, 'creepsPerMinDeltas.10.20', 'creepsPerMinDeltas.0.10', 'xpPerMinDeltas.10.20', 'xpPerMinDeltas.0.10', 'goldPerMinDeltas.10.20', 'goldPerMinDeltas.0.10', 'damageTakenPerMinDeltas.10.20', 'damageTakenPerMinDeltas.0.10'), mean)) %>%
    inner_join(league_matches_player_stats %>%
      group_by(name, teamRole, teamId, win) %>%
      tally())

    # Reordering columns 
    #league_matches_player_stats <- league_matches_player_stats[, c(1:4, 55, 5:54)]
  }

}


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
get_league_most_banned_champs <- function(league_champ_bans_df) {
  num_games = nrow(league_champ_bans_df) / 10
  return(league_champ_bans_df %>%
    group_by(name) %>%
    tally() %>%
    rename(bans = n) %>%
    arrange(desc(bans)) %>%
    mutate(banRate = bans/num_games)
  )
}
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
get_league_count_banned_champs_by_turn <- function(league_champ_bans_df) {
  return(league_champ_bans_df %>%
    group_by(pickTurn, name) %>%
    tally() %>%
    rename(bans = n) %>%
    arrange(pickTurn, desc(bans)))
}
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
get_league_most_played_champs <- function(league_matches_player_stats) {
  num_games = nrow(league_matches_player_stats) / 10
  return(league_matches_player_stats %>%
    group_by(name) %>%
    tally() %>%
    rename(games = n) %>%
    arrange(desc(games)) %>%
    mutate(playRate = games / num_games))
}
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
get_league_most_played_champs_role <- function(league_matches_player_stats) {
  num_games = nrow(league_matches_player_stats) / 10
  return(league_matches_player_stats %>%
    group_by(teamRole, name) %>%
    tally() %>%
    rename(games = n) %>%
    arrange(teamRole, desc(games)) %>%
    mutate(playRate = games / num_games))
}
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
get_league_most_playbanned_champs <- function(league_most_banned_champs, league_most_played_champs, league_matches_champ_bans) {
  num_games = nrow(league_matches_champ_bans) / 10
  ret_df <- league_most_banned_champs %>%
    full_join(league_most_played_champs, by = "name") %>%
    mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
    mutate(playBans = bans + games) %>% 
    mutate(playBanRate = playBans / num_games) %>%
    arrange(desc(playBanRate))
  return(ret_df)
}




#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
#                                  START -- ANALYSIS "MAIN" CODE
#
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#############################################
#                   NA LCS
#############################################
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
nalcs_regseason_summoner_avgs_df <- get_league_season_summoner_avgs(nalcs_matches_player_stats)
nalcs_regseason_summoner_avgs_wl_df <- get_league_season_summoner_avgs(nalcs_matches_player_stats, split_winloss = TRUE)
nalcs_regseason_summoner_avgs_br_df <- get_league_season_summoner_avgs(nalcs_matches_player_stats, split_bluered = TRUE)
nalcs_regseason_summoner_avgs_wlbr_df <- get_league_season_summoner_avgs(nalcs_matches_player_stats, split_bluered = TRUE, split_winloss = TRUE)
nalcs_regseason_summoner_totals_df <- get_league_regseason_summoner_totals(nalcs_matches_player_stats)


# Getting entire season summoner avgs
nalcs_season_summoner_avgs <- get_league_season_summoner_avgs(nalcs_matches_player_stats, only_regseason = FALSE)

# Tallying the most-banned and most-played champions
nalcs_most_banned_champs <- get_league_most_banned_champs(nalcs_matches_champ_bans)
nalcs_most_banned_champs %>%
  top_n(10, bans) %>%
  arrange(desc(bans)) %>%
  ggplot() + geom_bar(mapping = aes(x = name, y = bans), stat = "identity")

nalcs_banned_champs_by_turn <- get_league_count_banned_champs_by_turn(nalcs_matches_champ_bans)
nalcs_most_played_champs <- get_league_most_played_champs(nalcs_matches_player_stats)
nalcs_most_played_champs_by_role <- get_league_most_played_champs_role(nalcs_matches_player_stats)
nalcs_most_playbanned_champs <- get_league_most_playbanned_champs(nalcs_most_banned_champs, nalcs_most_played_champs, nalcs_matches_champ_bans)

#############################################
#                   EU LCS
#############################################
# Plots of regular season summoner averages
eulcs_regseason_summoner_avgs_df <- get_league_season_summoner_avgs(eulcs_matches_player_stats)
# Getting entire season summoner avgs
eulcs_season_summoner_avgs <- get_league_season_summoner_avgs(eulcs_matches_player_stats, only_regseason = FALSE)