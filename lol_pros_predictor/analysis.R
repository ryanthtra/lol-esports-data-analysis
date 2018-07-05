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
get_league_regseason_summoner_avgs <- function(league_matches_player_stats, split_winloss = FALSE, split_bluered = FALSE) {
  # Filter for just regular season games
  league_regseason_participants_accum <- league_matches_player_stats %>%
    filter(isTiebreaker == FALSE & isPlayoff == FALSE)

  # Create avg stats DF groups by lane and role
  if (split_winloss == FALSE && split_bluered == FALSE) {

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

    # Adding KDA Ratio column
    league_regseason_participants_accum <- league_regseason_participants_accum %>%
      mutate(KDA = (kills + assists) / deaths)

    # Reordering columns - teamName, wins, losses, <everything else>
    league_regseason_participants_accum <- league_regseason_participants_accum[, c(1, 2, 55, 54, 3:7, 56, 8:53)]

  } else if(split_winloss == TRUE && split_bluered == FALSE) {

    league_regseason_participants_accum <-
    (league_regseason_participants_accum %>%
    group_by(teamName, summonerName, teamRole, win) %>%
    summarize_at(vars(duration, kills:assists, totalDamageDealt:trueDamageDealt, totalDamageDealtToChampions:firstBloodKill, firstTowerKill:firstInhibitorAssist, 'creepsPerMinDeltas.10.20', 'creepsPerMinDeltas.0.10', 'xpPerMinDeltas.10.20', 'xpPerMinDeltas.0.10', 'goldPerMinDeltas.10.20', 'goldPerMinDeltas.0.10', 'damageTakenPerMinDeltas.10.20', 'damageTakenPerMinDeltas.0.10'), mean)) %>%
    inner_join(league_regseason_participants_accum %>%
      group_by(teamName, summonerName, teamRole, win) %>%
      tally())

    # Reordering columns 
    league_regseason_participants_accum <- league_regseason_participants_accum[, c(1:4, 55, 5:54)]

  } else if (split_winloss == FALSE && split_bluered == TRUE) {

    league_regseason_participants_accum <-
    (league_regseason_participants_accum %>%
    group_by(teamName, summonerName, teamRole, teamId) %>%
    summarize_at(vars(duration, kills:assists, totalDamageDealt:trueDamageDealt, totalDamageDealtToChampions:firstBloodKill, firstTowerKill:firstInhibitorAssist, 'creepsPerMinDeltas.10.20', 'creepsPerMinDeltas.0.10', 'xpPerMinDeltas.10.20', 'xpPerMinDeltas.0.10', 'goldPerMinDeltas.10.20', 'goldPerMinDeltas.0.10', 'damageTakenPerMinDeltas.10.20', 'damageTakenPerMinDeltas.0.10'), mean)) %>%
    inner_join(league_regseason_participants_accum %>%
      group_by(teamName, summonerName, teamRole, teamId) %>%
      tally())

    # Reordering columns 
    league_regseason_participants_accum <- league_regseason_participants_accum[, c(1:4, 55, 5:54)]

  } else {

    league_regseason_participants_accum <-
    (league_regseason_participants_accum %>%
    group_by(teamName, summonerName, teamRole, teamId, win) %>%
    summarize_at(vars(duration, kills:assists, totalDamageDealt:trueDamageDealt, totalDamageDealtToChampions:firstBloodKill, firstTowerKill:firstInhibitorAssist, 'creepsPerMinDeltas.10.20', 'creepsPerMinDeltas.0.10', 'xpPerMinDeltas.10.20', 'xpPerMinDeltas.0.10', 'goldPerMinDeltas.10.20', 'goldPerMinDeltas.0.10', 'damageTakenPerMinDeltas.10.20', 'damageTakenPerMinDeltas.0.10'), mean)) %>%
    inner_join(league_regseason_participants_accum %>%
      group_by(teamName, summonerName, teamRole, teamId, win) %>%
      tally())

    # Reordering columns 
    #league_regseason_participants_accum <- league_regseason_participants_accum[, c(1:4, 55, 5:54)]
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
nalcs_regseason_summoner_avgs_df <- get_league_regseason_summoner_avgs(nalcs_matches_player_stats)
nalcs_regseason_summoner_avgs_wl_df <- get_league_regseason_summoner_avgs(nalcs_matches_player_stats, split_winloss = TRUE)
nalcs_regseason_summoner_avgs_br_df <- get_league_regseason_summoner_avgs(nalcs_matches_player_stats, split_bluered = TRUE)
nalcs_regseason_summoner_avgs_wlbr_df <- get_league_regseason_summoner_avgs(nalcs_matches_player_stats, split_bluered = TRUE, split_winloss = TRUE)
nalcs_regseason_summoner_totals_df <- get_league_regseason_summoner_totals(nalcs_matches_player_stats)


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

nalcs_plot_rsplayer_avgs <- nalcs_regseason_summoner_avgs_df %>%
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


#############################################
#                   EU LCS
#############################################
# Plots of regular season summoner averages
eulcs_regseason_summoner_avgs_df <- get_league_regseason_summoner_avgs(eulcs_matches_player_stats)
eulcs_plot_rsplayer_avgs <- eulcs_regseason_summoner_avgs_df %>%
  filter(wins + losses >= 6) %>%
  ggplot()


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