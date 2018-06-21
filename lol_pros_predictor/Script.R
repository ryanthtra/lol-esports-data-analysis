library(httr)
library(dplyr)
library(tidyr)
library(jsonlite)
suppressMessages(library(dplyr))
#library(future)

#prefix_domain <- "https://na1.api.riotgames.com"

# This api_token will eventually go in a separate file which will be ignored by git.
#api_token <- "RGAPI-afa7ca6b-3ccd-465d-90c4-51616b4fabb8"


# get_summoner_by_name <- function(str_name) {
#   response <- GET(paste(prefix_domain, "/lol/summoner/v3/summoners/by-name/", str_name, sep = ""), add_headers("X-Riot-Token" = api_token))
#   if (response$status_code == 200) {
#     json <- jsonlite::fromJSON(content(response, as = "text"))
#     return (json)
#   }
# }
# 
# 
# get_matchlist_by_accountid_and_season <- function(num_accountid, num_season) {
#   response <- GET(paste(prefix_domain, "/lol/match/v3/matchlists/by-account/", num_accountid, "?season=", num_season, sep = ""), add_headers("X-Riot-Token" = api_token))
#   if (response$status_code == 200) {
#     json <- jsonlite::fromJSON(content(response, as = "text"))
#     return (json)
#   }
# }
# 
# 
# get_match_by_matchid <- function(num_matchid) {
#   response <- GET(paste(prefix_domain, "/lol/match/v3/matches/", num_matchid, sep = ""), add_headers("X-Riot-Token" = api_token))
#   if (response$status_code == 200) {
#     json <- jsonlite::fromJSON(content(response, as = "text"))
#     return (json)
#   }
# }
# 
# 
# get_match_timeline_by_matchid <- function(num_matchid) {
#   response <- GET(paste(prefix_domain, "/lol/match/v3/timelines/by-match/", num_matchid, sep = ""), add_headers("X-Riot-Token" = api_token))
#   if (response$status_code == 200) {
#     json <- jsonlite::fromJSON(content(response, as = "text"))
#     return (json)
#   }
# }

acs_prefix_domain <- "https://acs.leagueoflegends.com"
#######################################################
# API calls using ACS domain
#######################################################

# API call helper from response
process_uri <- function(str_uri) {

  print(str_uri)
  #future_response <- future({
  #return (GET(str_uri))
  #}) %plan% multiprocess
  #response <- value(future_response)
  response <- GET(str_uri)
  print(response$status_code)
  while (response$status_code == 429) {
    Sys.sleep(2)
    response <- GET(str_uri)
  }
  json <- jsonlite::fromJSON(content(response, as = "text"))
  return(json)
}

get_acs_summoner_by_name_and_region <- function(str_name, str_region) {
  uri <- paste(acs_prefix_domain, "/v1/players?name=", str_name, "&region=", str_region, sep = "")
  return(process_uri(uri))
}

get_acs_player_history_by_platform_and_account_id <- function(chr_platform_id = "EUW1", num_account_id = 23402463) {
  uri <- paste(acs_prefix_domain, "/v1/stats/player_history/", chr_platform_id, "/", num_account_id, sep = "")
  return(process_uri(uri))
}

get_acs_match_by_matchid <- function(chr_platform_id, num_match_id, chr_game_hash = "") {
  uri <- paste(acs_prefix_domain, "/v1/stats/game/", chr_platform_id, "/", num_match_id, ifelse(chr_game_hash != "", paste("?gameHash=", chr_game_hash, sep = ""), ""), sep = "")
  return(process_uri(uri))
}

get_acs_match_timeline_by_matchid <- function(chr_platform_id, num_match_id, chr_game_hash = "") {
  uri <- paste(acs_prefix_domain, "/v1/stats/game/", chr_platform_id, "/", num_match_id, "/timeline", ifelse(chr_game_hash != "", paste("?gameHash=", chr_game_hash, sep = ""), ""), sep = "")
  return(process_uri(uri))
}
#*************************************
# End -- API methods
#*************************************

######################################
# Static API methods from ddragon
######################################
ddragon_prefix_domain <- "https://ddragon.leagueoflegends.com"
get_champion_data_by_version <- function(chr_version_number = "8.8.2") {
  uri <- paste(ddragon_prefix_domain, "/cdn/", chr_version_number, "/data/en_US/champion.json", sep = "")
  return(process_uri(uri))
}

#*************************************
# END -- Static API methods
#*************************************



######################################
# Helper and wrangling methods
######################################

get_league_match_data_list <- function(league_matchid_df) {
  # OLD FOR LOOP IMPLEMENTATION
  matchlist <- list()
  for (i in 1:nrow(league_matchid_df)) {
    matchlist[[i]] <- get_acs_match_by_matchid(league_matchid_df$Region.ID[[i]], league_matchid_df$Game.ID[[i]], chr_game_hash = league_matchid_df$Hash.ID[[i]])
  }

  #matchlist <- apply(league_matchid_df[c(1:10), c('Region.ID', 'Game.ID', 'Hash.ID', 'Blue.Team', 'Red.Team')], 1, function(row) {
  #current_match <- get_acs_match_by_matchid(row[1], row[2], chr_game_hash = row[3])
  #Sys.sleep(1.0)
  #return (current_match)
  #})

  ##matchlist <- sapply(1:10, function(i) {
  #matchlist <- sapply(1:nrow(league_matchid_df), function(i) {
  ##print(paste("API Call #", i, sep=""))
  #current_match <- get_acs_match_by_matchid(league_matchid_df$Region.ID[[i]], league_matchid_df$Game.ID[[i]], chr_game_hash = league_matchid_dfHash.ID[[i]])

  ##if ((i %% 14) == 0) {
  ##Sys.sleep(0.5)
  ##}
  #return(current_match)
  #})

  return(matchlist)
}

get_league_timeline_data_list <- function(league_matchid_df) {
  # OLD FOR LOOP IMPLEMENTATION
  timelinelist <- list()
  for (i in 1:nrow(league_matchid_df)) {
    timelinelist[[i]] <- get_acs_match_by_matchid(league_matchid_df$Region.ID[[i]], league_matchid_df$Game.ID[[i]], chr_game_hash = league_matchid_df$Hash.ID[[i]])
  }
  return(timelinelist)
}

get_accum_matches_teams <- function(league_matchlist, league_matchid_df) {
  league_matches_teams_accum <- data.frame(NULL)
  for (i in 1:length(league_matchlist)) {
    # Convert win column (Win/Fail) to logical (TRUE/FALSE)
    league_matchlist[[i]]$teams["win"] <- as.logical(eval(league_matchlist[[i]]$teams["win"]) == "Win")
    # Add team names column
    league_matchlist[[i]]$teams["teamName"] <- unname(unlist(c(league_matchid_df[i, c("Blue.Team", "Red.Team")])))
    # Add game number column
    league_matchlist[[i]]$teams["gameNumber"] <- c(i, i)
    # Add tiebreaker and playoff column
    league_matchlist[[i]]$teams["isTiebreaker"] <- unname(unlist(c(league_matchid_df[i, c("Tiebreaker", "Tiebreaker")])))
    league_matchlist[[i]]$teams["isPlayoff"] <- unname(unlist(c(league_matchid_df[i, c("Playoff", "Playoff")])))
    # Add game duration 
    league_matchlist[[i]]$teams["duration"] <- rep(league_matchlist[[i]]$gameDuration, 2)
    
    # Concatenate rows from current match onto the accumulation DF
    league_matches_teams_accum <- league_matches_teams_accum %>% bind_rows(league_matchlist[[i]]$teams %>% select(-bans))
  }
  #Change all teamId = 100/200 to Blue/Red
  # and remove some irrelevant columns
  league_matches_teams_accum <- league_matches_teams_accum %>%
    mutate(teamId = replace(teamId, grepl('100', teamId), 'Blue')) %>%
    mutate(teamId = replace(teamId, grepl('200', teamId), 'Red')) %>%
    select(-vilemawKills, -dominionVictoryScore)
  return(league_matches_teams_accum)
}

get_accum_matches_bans <- function(league_matchlist, league_matchid_df) {
  league_matches_bans_accum <- data.frame(NULL)
  for (i in 1:length(league_matchlist)) {
    # Alter bans DFs so that we can get associated champion name with the key
    ret_bans <- lapply(league_matchlist[[i]]$teams$bans, function(item) {
      # Add 6 to 4th and 5th rows of pickTurn in order to get the correct ban order.
      item[4:5,] <- item[4:5,] %>%
        mutate(pickTurn = as.numeric(pickTurn) + 6)
      item <- left_join(item, champions_df_simple)
      return (item)
    })
    my_bans <- as.data.frame(ret_bans)
    my_bans2 <- my_bans %>% select(championId.1:name.1)
    my_bans2 <- my_bans2 %>% rename(championId = championId.1, pickTurn = pickTurn.1, name = name.1)
    my_bans <- my_bans %>% select(championId:name)
    ret_bans <- my_bans %>% bind_rows(my_bans2) %>% arrange(pickTurn)
    ret_bans["gameNumber"] <- rep(i, 10)
    # Add tiebreaker and playoff column
    ret_bans["isTiebreaker"] <- unname(unlist(c(league_matchid_df[i, rep("Tiebreaker", 10)])))
    ret_bans["isPlayoff"] <- unname(unlist(c(league_matchid_df[i, rep("Playoff", 10)])))

    league_matches_bans_accum <- league_matches_bans_accum %>% bind_rows(ret_bans)
  }
  return (league_matches_bans_accum)
}


# Concatenates the "participants" DFs of each match together
# combine_teammate_stats determines whether we want to combine the stats of the teammates
# into total numbers 
get_accum_matches_participants <- function(league_matchlist, league_matchid_df, combine_teammate_stats = FALSE) {
  league_matches_participants_accum <- data.frame(NULL)
  for (i in 1:length(league_matchlist)) {
    flattened_df <- get_flattened_match_participants_df(league_matchlist[[i]]$participants, league_matchlist[[i]]$participantIdentities)
    flattened_df["gameNumber"] <- rep(i, 10)
    flattened_df["isTiebreaker"] <- rep(league_matchid_df[i,]$Tiebreaker, 10)
    flattened_df["isPlayoff"] <- rep(league_matchid_df[i,]$Playoff, 10)
    tmp_fdf1 <- flattened_df %>% filter(teamId == "Blue")
    tmp_fdf1["teamName"] <- unname(unlist(c(league_matchid_df[i, rep("Blue.Team", 5)])))
    tmp_fdf2 <- flattened_df %>% filter(teamId == "Red")
    tmp_fdf2["teamName"] <- unname(unlist(c(league_matchid_df[i, rep("Red.Team", 5)])))

    # TODO: figure out why NAs can't be replaced
    #tmp_fdf1 <- tmp_fdf1 %>%
      ##mutate_all(funs(replace(., is.na(.), 0)))
      #mutate_at(vars("creepsPerMinDeltas.10-20":"damageTakenDiffPerMinDeltas.0-10", "creepsPerMinDeltas.30-end":"damageTakenDiffPerMinDeltas.20-30"), funs(replace(., is.na(.), 0)))
    #tmp_fdf2 <- tmp_fdf2 %>%
      ##mutate_all(funs(replace(., is.na(.), 0)))
      #mutate_at(vars("creepsPerMinDeltas.10-20":"damageTakenDiffPerMinDeltas.0-10", "creepsPerMinDeltas.30-end":"damageTakenDiffPerMinDeltas.20-30"), funs(replace(., is.na(.), 0)))

    # Combine teammates stats together
    if (combine_teammate_stats == TRUE) {
      tmp_fdf1 <- get_match_combined_participant_stats_df(tmp_fdf1)
      tmp_fdf2 <- get_match_combined_participant_stats_df(tmp_fdf2)
    }
    flattened_df <- bind_rows(tmp_fdf1, tmp_fdf2)
    league_matches_participants_accum <- league_matches_participants_accum %>% bind_rows(flattened_df)
  }

  # TODO: Remove some irrelevant columns
  return(league_matches_participants_accum)
}

get_flattened_match_participants_df <- function(match_participants_df, match_participantids_df) {
  # uses jsonlite flatten() function
  ret_df <- match_participants_df %>%
    select(-stats, - timeline) %>%
    bind_cols(match_participantids_df$player) %>%
    inner_join(champions_df_simple) %>%
    inner_join(match_participants_df$stats) %>%
    inner_join(match_participants_df$timeline %>%
                 flatten())
  # Change teamId = 100/200 to Blue/Red, replace NA's in the Deltas with 0s
  ret_df <- ret_df %>%
    mutate(teamId = replace(teamId, grepl('100', teamId), 'Blue')) %>%
    mutate(teamId = replace(teamId, grepl('200', teamId), 'Red'))

  return(ret_df)
}

get_match_combined_participant_stats_df <- function(match_team_df) {
  #match_team_df <- match_team_df %>%
    #group_by(teamName, teamId, win, gameNumber, isTiebreaker, isPlayoff) %>%
    #summarize_at(vars(kills:assists, totalDamageDealt:trueDamageDealt, totalDamageDealtToChampions:wardsKilled, "creepsPerMinDeltas.10-20":"damageTakenDiffPerMinDeltas.0-10", "creepsPerMinDeltas.30-end":"damageTakenDiffPerMinDeltas.20-30"),
    #sum)

  match_team_df <- match_team_df %>%
    mutate_at(vars(contains("Deltas")), funs(replace(., is.na(.), 0)))

  match_team_df <- match_team_df %>%
    group_by(teamName, teamId, win, gameNumber, isTiebreaker, isPlayoff) %>%
    summarize_at(vars(kills:assists, totalDamageDealt:trueDamageDealt, totalDamageDealtToChampions:goldSpent, totalMinionsKilled:wardsKilled, 'creepsPerMinDeltas.10-20', 'creepsPerMinDeltas.0-10', 'xpPerMinDeltas.10-20', 'xpPerMinDeltas.0-10', 'goldPerMinDeltas.10-20', 'goldPerMinDeltas.0-10', 'damageTakenPerMinDeltas.10-20', 'damageTakenPerMinDeltas.0-10'), sum)
  return(match_team_df)
}



get_league_bluered_winpct_by_team <- function(league_matches_teams_accum) {
  return_df <- inner_join(
    (league_matches_teams_accum %>% group_by(teamName, teamId) %>% count() %>% rename(games = n)),
    (league_matches_teams_accum %>% group_by(teamName, teamId) %>% filter(win == "Win") %>% count() %>% rename(wins = n))
  ) %>%
    mutate("win%" = wins / games)

  return(return_df)
}


get_league_regseason_team_totals <- function(league_matches_tpc_accum) {
  # Filter for just regular season games
  league_regseason_tpc_df <- league_matches_tpc_accum %>%
  filter(isTiebreaker == FALSE & isPlayoff == FALSE)

  # Create a total stats DF grouped by teams
  league_regseason_team_totals_df <-
  # First parentheses group: sums of stats by team
  (league_regseason_tpc_df %>%
      group_by(teamName) %>%
      summarise_at(vars(kills:wardsKilled, baronKills:duration, 'creepsPerMinDeltas.10-20', 'creepsPerMinDeltas.0-10', 'xpPerMinDeltas.10-20', 'xpPerMinDeltas.0-10', 'goldPerMinDeltas.10-20', 'goldPerMinDeltas.0-10', 'damageTakenPerMinDeltas.10-20', 'damageTakenPerMinDeltas.0-10'), sum)) %>%
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
      spread(win, n) %>%
      rename('losses' = 'FALSE', 'wins' = 'TRUE'))

  # Reordering columns
  league_regseason_team_totals_df <- league_regseason_team_totals_df[, c(1, 54, 53, 2:52)]
}


get_league_regseason_team_avgs <- function(league_matches_tpc_accum) {
  # Filter for just regular season games
  league_regseason_tpc_df <- league_matches_tpc_accum %>%
  filter(isTiebreaker == FALSE & isPlayoff == FALSE)

  ## Create a avg stats DF grouped by teams
  #league_regseason_team_totals_df <-
  #(league_matches_tpc_accum %>%
    #group_by(teamName) %>%
    #summarise_at())
}
#*************************************
# End -- Helper methods
#*************************************



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
#                                  START -- "MAIN" CODE
#
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# Champion data

champions_list <- get_champion_data_by_version()$data
champions_df <- data.frame(NULL)
for (i in 1:length(champions_list)) {
  champions_df <- champions_df %>% bind_rows(data.frame(champions_list[[i]]))
}
champions_df_simple <- champions_df %>%
  select(name, key) %>%
  distinct() %>%
  rename(championId = key) %>%
  mutate(championId = as.numeric(championId))
remove(champions_df)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# NA LCS data (Spring Split 2018)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# NA LCS 2018 Spring Split -- Regular Season and Playoffs
nalcs_matchid_df <- read.csv("gameid_data/NALCS_Spring2018.csv")

nalcs_matches <- get_league_match_data_list(nalcs_matchid_df)

nalcs_single_match <- get_acs_match_by_matchid(nalcs_matchid_df$Region.ID[[1]], nalcs_matchid_df$Game.ID[[1]], chr_game_hash = nalcs_matchid_df$Hash.ID[[1]])

# Get the "teams" data frame, which contains who won/lost, first blood, first baron, etc.
# Will need to wrangle so that team names are in each row, "Team 100/200" is changed to Blue/Red,
# and each entry in the list is concatenated into a large list, in order to do data visualization.

nalcs_single_match$teams["teamName"] <- unname(unlist(c(nalcs_matchid_df[1, c("Blue.Team", "Red.Team")])))

nalcs_matches_teams_accum <- get_accum_matches_teams(nalcs_matches, nalcs_matchid_df)
nalcs_matches_bans_accum <- get_accum_matches_bans(nalcs_matches, nalcs_matchid_df)
nalcs_matches_participants_accum <- get_accum_matches_participants(nalcs_matches, nalcs_matchid_df)
#nalcs_matches_no_nas <- nalcs_matches_participants_accum %>%
  #select(c(contains("Deltas"))) %>%
  #mutate_at(vars(contains("Deltas")), funs(replace(., is.na(.), 0)))
nalcs_matches_participants_accum <- nalcs_matches_participants_accum %>%
  mutate_at(vars(contains("Deltas")), funs(replace(., is.na(.), 0)))
nalcs_matches_participants_combined_accum <- get_accum_matches_participants(nalcs_matches, nalcs_matchid_df, combine_teammate_stats = TRUE)

# Joins the "teams" DF and the "participants combined" DF together
nalcs_matches_tpc_accum <- nalcs_matches_participants_combined_accum %>%
  inner_join(nalcs_matches_teams_accum)

nalcs_regseason_team_totals_df <- get_league_regseason_team_totals(nalcs_matches_tpc_accum)

#nalcs_regseason_tpc_df <- nalcs_matches_tpc_accum %>%
  #filter(isTiebreaker == FALSE & isPlayoff == FALSE)
## Create a total stats DF grouped by teams
#nalcs_regseason_team_totals_df <-
  ## First parentheses group: sums of stats by team
  #(nalcs_regseason_tpc_df %>%
    #group_by(teamName) %>%
    #summarise_at(vars(kills:wardsKilled, baronKills:duration, 'creepsPerMinDeltas.10-20', 'creepsPerMinDeltas.0-10', 'xpPerMinDeltas.10-20', 'xpPerMinDeltas.0-10', 'goldPerMinDeltas.10-20', 'goldPerMinDeltas.0-10', 'damageTakenPerMinDeltas.10-20', 'damageTakenPerMinDeltas.0-10'), sum)) %>%
    ## More parenthesis groups: tallying first-objective columns
  #inner_join(nalcs_regseason_tpc_df %>%
    #group_by(teamName, firstBlood) %>%
    #tally() %>% spread(firstBlood, n) %>% select('TRUE') %>% rename('firstBloods' = 'TRUE')) %>%
  #inner_join(nalcs_regseason_tpc_df %>%
    #group_by(teamName, firstTower) %>%
    #tally() %>% spread(firstTower, n) %>% select('TRUE') %>% rename('firstTowers' = 'TRUE')) %>%
  #inner_join(nalcs_regseason_tpc_df %>%
    #group_by(teamName, firstInhibitor) %>%
    #tally() %>% spread(firstInhibitor, n) %>% select('TRUE') %>% rename('firstInhibitors' = 'TRUE')) %>%
  #inner_join(nalcs_regseason_tpc_df %>%
    #group_by(teamName, firstBaron) %>%
    #tally() %>% spread(firstBaron, n) %>% select('TRUE') %>% rename('firstBarons' = 'TRUE')) %>%
  #inner_join(nalcs_regseason_tpc_df %>%
    #group_by(teamName, firstDragon) %>%
    #tally() %>% spread(firstDragon, n) %>% select('TRUE') %>% rename('firstDragons' = 'TRUE')) %>%
    ## Last parentheses group: tallying wins and losses by team
  #inner_join(nalcs_regseason_tpc_df %>%
    #group_by(teamName, win) %>%
    #tally() %>%
    #spread(win, n) %>%
    #rename('losses' = 'FALSE', 'wins' = 'TRUE'))
## Reordering columns
#nalcs_regseason_team_totals_df <- nalcs_regseason_team_totals_df[, c(1, 54, 53, 2:52)]

#nalcs_matches_regseason_teams <- nalcs_matches_teams_accum %>%
  #filter(isTiebreaker == FALSE & isPlayoff == FALSE)
#nalcs_matches_regseason_participants <- nalcs_matches_participants_accum %>%
  #filter(isTiebreaker == FALSE & isPlayoff == FALSE)

na_matches_all_total_games <- nrow(nalcs_matches)

na_matches_all_teams_winlose_avg_stats <- nalcs_matches_teams_accum %>%
  group_by(win) %>%
  summarise_each(funs(mean), towerKillAvg = towerKills, inhibitorKillAvg = inhibitorKills, baronKillAvg = baronKills, dragonKillAvg = dragonKills, riftHeraldKillAvg = riftHeraldKills)

na_matches_all_teams_pctwin_stats <- nalcs_matches_teams_accum %>%
  group_by(win) %>%
  summarise_each(funs(mean), firstBlood, firstTower, firstInhibitor, firstBaron, firstDragon, firstRiftHerald)

na_matches_all_teams_pctwin_stats_by_team <- nalcs_matches_teams_accum %>%
  group_by(teamName, win) %>%
  summarise_each(funs(mean), firstBlood, firstTower, firstInhibitor, firstBaron, firstDragon, firstRiftHerald, duration)

na_matches_tally_firstbaron <- nalcs_matches_teams_accum %>%
  filter(firstBaron == TRUE) %>%
  nrow()
na_matches_winprob_firstbaron <- nalcs_matches_teams_accum %>%
  filter(firstBaron == TRUE) %>%
  group_by(win) %>%
  summarise(probability = n() / na_matches_tally_firstbaron)

na_bluered_avg_stats <- nalcs_matches_teams_accum %>%
  group_by(teamId) %>%
  summarise_each(funs(mean), towerKillAvg = towerKills, inhibitorKillAvg = inhibitorKills, baronKillAvg = baronKills, dragonKillAvg = dragonKills, riftHeraldKillAvg = riftHeraldKills)

na_bluered_wins <- nalcs_matches_teams_accum %>%
  group_by(teamId, win) %>%
  filter(win == "Win") %>%
  count(win)
na_bluered_avgs_by_team <- nalcs_matches_teams_accum %>%
  group_by(teamName, teamId) %>%
  summarise_each(funs(mean), towerKillAvg = towerKills, inhibitorKillAvg = inhibitorKills, baronKillAvg = baronKills, dragonKillAvg = dragonKills, riftHeraldKillAvg = riftHeraldKills)
#na_bluered_totals_by_team <- nalcs_matches_teams_accum %>%
#group_by(teamName, teamId) %>%
#summarise_each(funs(sum), towerKillTot = towerKills, inhibitorKillTot = inhibitorKills, baronKillTot = baronKills, dragonKillTot = dragonKills, riftHeraldKillTot = riftHeraldKills)
na_bluered_winpct_by_team <- get_league_bluered_winpct_by_team(nalcs_matches_teams_accum)





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# EU LCS data (Spring Split 2018)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## EU LCS 2018 Spring Split -- Regular Season and Playoffs
#eulcs_matchid_df <- read.csv("EULCS_Spring2018.csv")

#eulcs_matches <- get_league_match_data_list(eulcs_matchid_df)
#eulcs_matches_teams_accum <- get_accum_matches_teams(eulcs_matches, eulcs_matchid_df)
#eu_bluered_avg_stats <- eulcs_matches_teams_accum %>%
#group_by(teamId) %>%
#summarise_each(funs(mean), towerKillAvg = towerKills, inhibitorKillAvg = inhibitorKills, baronKillAvg = baronKills, dragonKillAvg = dragonKills, riftHeraldKillAvg = riftHeraldKills)
#eu_bluered_wins <- eulcs_matches_teams_accum %>%
#group_by(teamId, win) %>%
#filter(win == "Win") %>%
#count(win)
#eu_bluered_avgs_by_team <- eulcs_matches_teams_accum %>%
#group_by(teamName, teamId) %>%
#summarise_each(funs(mean), towerKillAvg = towerKills, inhibitorKillAvg = inhibitorKills, baronKillAvg = baronKills, dragonKillAvg = dragonKills, riftHeraldKillAvg = riftHeraldKills)
#eu_bluered_winpct_by_team <- get_league_bluered_winpct_by_team(eulcs_matches_teams_accum)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# MSI 2018 data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## MSI 2018 Play-In Stage
#msipi_matchid_df <- read.csv("MSI_PlayInAll2018.csv")
## Group Stage 
#msigs_matchid_df <- read.csv("MSI_GroupStage2018.csv")
## Group Stage Tiebreakers and Knockouts
#msiko_matchid_df <- read.csv("MSI_GroupStageTBKO2018.csv")
## All MSI 2018
#msiall_matchid_df <- read.csv("MSI_All2018.csv")

#msipi_matches <- get_league_match_data_list(msipi_matchid_df)
#msipi_matches_teams_accum <- get_accum_matches_teams(msipi_matches, msipi_matchid_df)
#msipi_bluered_avg_stats <- msipi_matches_teams_accum %>%
#group_by(teamId) %>%
#summarise_each(funs(mean), towerKillAvg = towerKills, inhibitorKillAvg = inhibitorKills, baronKillAvg = baronKills, dragonKillAvg = dragonKills, riftHeraldKillAvg = riftHeraldKills)
#msipi_bluered_wins <- msipi_matches_teams_accum %>%
#group_by(teamId, win) %>%
#filter(win == "Win") %>%
#count(win)

#msiall_matches <- get_league_match_data_list(msiall_matchid_df)
#msiall_matches_teams_accum <- get_accum_matches_teams(msiall_matches, msiall_matchid_df)
#msiall_bluered_avg_stats <- msiall_matches_teams_accum %>%
  #group_by(teamId) %>%
  #summarise_each(funs(mean), towerKillAvg = towerKills, inhibitorKillAvg = inhibitorKills, baronKillAvg = baronKills, dragonKillAvg = dragonKills, riftHeraldKillAvg = riftHeraldKills)
#msiall_bluered_wins <- msiall_matches_teams_accum %>%
  #group_by(teamId, win) %>%
  #filter(win == "Win") %>%
  #count(win)
#msiall_bluered_winpct_by_team <- get_league_bluered_winpct_by_team(msiall_matches_teams_accum)