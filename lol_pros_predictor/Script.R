library(httr)
library(dplyr)
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
    # Add team names column
    league_matchlist[[i]]$teams["teamName"] <- unname(unlist(c(league_matchid_df[i, c("Blue.Team", "Red.Team")])))
    # Add game number column
    league_matchlist[[i]]$teams["gameNumber"] <- c(i, i)
    # TODO: Alter bans DFs so that we can get associated champion name with the key
    league_matchlist[[i]]$teams$bans <- lapply(league_matchlist[[i]]$teams$bans, function(item) {
      # Add 6 to 4th and 5th rows of pickTurn in order to get the correct ban order.
      item[4:5,] <- item[4:5,] %>%
        mutate(pickTurn = as.numeric(pickTurn) + 6)
      left_join(item, champions_df_simple)
    })
    # Concatenate rows from current match onto the accumulation DF
    league_matches_teams_accum <- league_matches_teams_accum %>% bind_rows(league_matchlist[[i]]$teams)
  }
  #Change all teamId = 100/200 to Blue/Red
  league_matches_teams_accum <- league_matches_teams_accum %>%
    mutate(teamId = replace(teamId, grepl('100', teamId), 'Blue')) %>%
    mutate(teamId = replace(teamId, grepl('200', teamId), 'Red'))
  return(league_matches_teams_accum)
}

get_accum_matches_participants <- function(league_matchlist, league_matchid_df) {
  league_matches_participants_accum <- data.frame(NULL)
  for (i in 1:length(league_matchlist)) {
    flattened_df <- get_flattened_match_participants_df(league_matchlist[[i]]$participants, league_matchlist[[i]]$participantIdentities)
    flattened_df["gameNumber"] <- rep(i, 10)

    league_matches_participants_accum <- league_matches_participants_accum %>% bind_rows(flattened_df)
  }
  return(league_matches_participants_accum)
}

get_flattened_match_participants_df <- function(match_participants_df, match_participantids_df) {
  # uses jsonline flatten() function
  ret_df <- match_participants_df %>%
    select(-stats, - timeline) %>%
    bind_cols(match_participantids_df$player) %>%
    inner_join(champions_df_simple) %>%
    inner_join(match_participants_df$stats) %>%
    inner_join(match_participants_df$timeline %>%
                 flatten())
  # Change teamId = 100/200 to Blue/Red
  ret_df <- ret_df %>%
    mutate(teamId = replace(teamId, grepl('100', teamId), 'Blue')) %>%
    mutate(teamId = replace(teamId, grepl('200', teamId), 'Red'))

  return(ret_df)
}



get_league_bluered_winpct_by_team <- function(league_matches_teams_accum) {
  return_df <- inner_join(
    (league_matches_teams_accum %>% group_by(teamName, teamId) %>% count() %>% rename(games = n)),
    (league_matches_teams_accum %>% group_by(teamName, teamId) %>% filter(win == "Win") %>% count() %>% rename(wins = n))
  ) %>%
    mutate("win%" = wins / games)

  return(return_df)
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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# NA LCS data (Spring Split 2018)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# NA LCS 2018 Spring Split -- Regular Season and Playoffs
nalcs_matchid_df <- read.csv("NALCS_Spring2018.csv")

nalcs_matches <- get_league_match_data_list(nalcs_matchid_df)

nalcs_single_match <- get_acs_match_by_matchid(nalcs_matchid_df$Region.ID[[1]], nalcs_matchid_df$Game.ID[[1]], chr_game_hash = nalcs_matchid_df$Hash.ID[[1]])

# Get the "teams" data frame, which contains who won/lost, first blood, first baron, etc.
# Will need to wrangle so that team names are in each row, "Team 100/200" is changed to Blue/Red,
# and each entry in the list is concatenated into a large list, in order to do data visualization.

nalcs_single_match$teams["teamName"] <- unname(unlist(c(nalcs_matchid_df[1, c("Blue.Team", "Red.Team")])))

nalcs_single_match$teams$bans <- lapply(nalcs_single_match$teams$bans, function(item) {
  item[4:5,] <- item[4:5,] %>%
    mutate(pickTurn = as.numeric(pickTurn) + 6)
  left_join(item, champions_df_simple)
})
nalcs_matches_teams_accum <- get_accum_matches_teams(nalcs_matches, nalcs_matchid_df)
nalcs_matches_participants_accum <- get_accum_matches_participants(nalcs_matches, nalcs_matchid_df)
#na_win_blue <- nalcs_matches_teams_accum %>% filter(teamId == "Blue" & win == "Win")
#na_win_red <- nalcs_matches_teams_accum %>% filter(teamId == "Red" & win == "Win")
#na_win_firstblood <- nalcs_matches_teams_accum %>% filter(firstBlood == "TRUE" & win == "Win")
#na_win_firsttower <- nalcs_matches_teams_accum %>% filter(firstTower == "TRUE" & win == "Win")
#na_win_firstinhib <- nalcs_matches_teams_accum %>% filter(firstInhibitor == "TRUE" & win == "Win")
#na_win_firstbaron <- nalcs_matches_teams_accum %>% filter(firstBaron == "TRUE" & win == "Win")
#na_win_firstdragon <- nalcs_matches_teams_accum %>% filter(firstDragon == "TRUE" & win == "Win")
#na_win_riftheraldkill <- nalcs_matches_teams_accum %>% filter(riftHeraldKills == 1 & win == "Win")

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

# MSI 2018 Play-In Stage
msipi_matchid_df <- read.csv("MSI_PlayInAll2018.csv")
# Group Stage 
msigs_matchid_df <- read.csv("MSI_GroupStage2018.csv")
# Group Stage Tiebreakers and Knockouts
msiko_matchid_df <- read.csv("MSI_GroupStageTBKO2018.csv")
# All MSI 2018
msiall_matchid_df <- read.csv("MSI_All2018.csv")

#msipi_matches <- get_league_match_data_list(msipi_matchid_df)
#msipi_matches_teams_accum <- get_accum_matches_teams(msipi_matches, msipi_matchid_df)
#msipi_bluered_avg_stats <- msipi_matches_teams_accum %>%
#group_by(teamId) %>%
#summarise_each(funs(mean), towerKillAvg = towerKills, inhibitorKillAvg = inhibitorKills, baronKillAvg = baronKills, dragonKillAvg = dragonKills, riftHeraldKillAvg = riftHeraldKills)
#msipi_bluered_wins <- msipi_matches_teams_accum %>%
#group_by(teamId, win) %>%
#filter(win == "Win") %>%
#count(win)

msiall_matches <- get_league_match_data_list(msiall_matchid_df)
msiall_matches_teams_accum <- get_accum_matches_teams(msiall_matches, msiall_matchid_df)
msiall_bluered_avg_stats <- msiall_matches_teams_accum %>%
  group_by(teamId) %>%
  summarise_each(funs(mean), towerKillAvg = towerKills, inhibitorKillAvg = inhibitorKills, baronKillAvg = baronKills, dragonKillAvg = dragonKills, riftHeraldKillAvg = riftHeraldKills)
msiall_bluered_wins <- msiall_matches_teams_accum %>%
  group_by(teamId, win) %>%
  filter(win == "Win") %>%
  count(win)
msiall_bluered_winpct_by_team <- get_league_bluered_winpct_by_team(msiall_matches_teams_accum)