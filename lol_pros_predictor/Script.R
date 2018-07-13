library(httr)
library(dplyr)
library(tidyr)
library(jsonlite)
library(Kmisc)
suppressMessages(library(dplyr))
#library(future)

acs_prefix_domain <- "https://acs.leagueoflegends.com"
#######################################################
# API calls using ACS domain
#######################################################

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# API call helper from response
process_uri <- function(str_uri) {

  print(str_uri)

  response <- GET(str_uri)
  print(response$status_code)

  while (response$status_code != 200) {
    Sys.sleep(2)
    response <- GET(str_uri)
  }
  json <- jsonlite::fromJSON(content(response, as = "text"))
  return(json)
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
get_acs_summoner_by_name_and_region <- function(str_name, str_region) {
  uri <- paste(acs_prefix_domain, "/v1/players?name=", str_name, "&region=", str_region, sep = "")
  return(process_uri(uri))
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
get_acs_player_history_by_platform_and_account_id <- function(chr_platform_id = "EUW1", num_account_id = 23402463) {
  uri <- paste(acs_prefix_domain, "/v1/stats/player_history/", chr_platform_id, "/", num_account_id, sep = "")
  return(process_uri(uri))
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
get_acs_match_by_matchid <- function(chr_platform_id, num_match_id, chr_game_hash = "") {
  uri <- paste(acs_prefix_domain, "/v1/stats/game/", chr_platform_id, "/", num_match_id, ifelse(chr_game_hash != "", paste("?gameHash=", chr_game_hash, sep = ""), ""), sep = "")
  return(process_uri(uri))
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
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


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
get_league_match_data_list <- function(league_matchid_df) {
  # OLD FOR LOOP IMPLEMENTATION
  matchlist <- list()

  for (i in 1:nrow(league_matchid_df)) {
    matchlist[[i]] <- get_acs_match_by_matchid(league_matchid_df$Region.ID[[i]], league_matchid_df$Game.ID[[i]], chr_game_hash = league_matchid_df$Hash.ID[[i]])
  }

  return(matchlist)
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
get_league_timeline_data_list <- function(league_matchid_df) {
  # OLD FOR LOOP IMPLEMENTATION
  timelinelist <- list()
  for (i in 1:nrow(league_matchid_df)) {
    timelinelist[[i]] <- get_acs_match_by_matchid(league_matchid_df$Region.ID[[i]], league_matchid_df$Game.ID[[i]], chr_game_hash = league_matchid_df$Hash.ID[[i]])
  }
  return(timelinelist)
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
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

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
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


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
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
    flattened_df["duration"] <- rep(league_matchlist[[i]]$gameDuration, 10)
    tmp_fdf1 <- flattened_df %>% filter(teamId == "Blue")
    tmp_fdf1["teamName"] <- unname(unlist(c(league_matchid_df[i, rep("Blue.Team", 5)])))
    tmp_fdf2 <- flattened_df %>% filter(teamId == "Red")
    tmp_fdf2["teamName"] <- unname(unlist(c(league_matchid_df[i, rep("Red.Team", 5)])))

    # Combine teammates stats together
    if (combine_teammate_stats == TRUE) {
      tmp_fdf1 <- get_match_combined_participant_stats_df(tmp_fdf1)
      tmp_fdf2 <- get_match_combined_participant_stats_df(tmp_fdf2)
    } 
    flattened_df <- bind_rows(tmp_fdf1, tmp_fdf2)


    if (combine_teammate_stats == FALSE) {
      flattened_df['teamRole'] <- NULL
      # Get team roles
      for (j in 1:nrow(flattened_df)) {
        if (flattened_df[j, 'participantId'] == 1 || flattened_df[j, 'participantId'] == 6) {
          flattened_df[j, 'teamRole'] = "TOP"
        } else if (flattened_df[j, 'participantId'] == 2 || flattened_df[j, 'participantId'] == 7) {
          flattened_df[j, 'teamRole'] = "JUNGLE"
        } else if (flattened_df[j, 'participantId'] == 3 || flattened_df[j, 'participantId'] == 8) {
          flattened_df[j, 'teamRole'] = "MID"
        } else if (flattened_df[j, 'participantId'] == 4 || flattened_df[j, 'participantId'] == 9) {
          flattened_df[j, 'teamRole'] = "BOTCARRY"
        } else {
          flattened_df[j, 'teamRole'] = "SUPPORT"
        }
      }
    }
    league_matches_participants_accum <- league_matches_participants_accum %>% bind_rows(flattened_df)
  }

  return(league_matches_participants_accum)
}


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
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

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
get_match_combined_participant_stats_df <- function(match_team_df) {

  # Replace all the NAs in the "Deltas" columns with zeroes 
  match_team_df <- match_team_df %>%
    mutate_at(vars(contains("Deltas")), funs(replace(., is.na(.), 0)))

  # Groups observations by game number and team name
  # (the other group-by variables are used just so they can be included in the output DF)
  # Sums up a bunch of columns together 
  match_team_df <- match_team_df %>%
    group_by(teamName, teamId, win, gameNumber, duration, isTiebreaker, isPlayoff) %>%
    summarize_at(vars(kills:assists, totalDamageDealt:trueDamageDealt, totalDamageDealtToChampions:goldSpent, totalMinionsKilled:wardsKilled, 'creepsPerMinDeltas.10-20', 'creepsPerMinDeltas.0-10', 'xpPerMinDeltas.10-20', 'xpPerMinDeltas.0-10', 'goldPerMinDeltas.10-20', 'goldPerMinDeltas.0-10', 'damageTakenPerMinDeltas.10-20', 'damageTakenPerMinDeltas.0-10'), sum)
  return(match_team_df)
}


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
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
remove(champions_df)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# NA LCS data (Spring Split 2018)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## NA LCS 2018 Spring Split -- Regular Season and Playoffs
#nalcs_matchid_df <- read.csv("gameid_data/NALCS_Spring2018.csv")
#nalcs_matches <- get_league_match_data_list(nalcs_matchid_df)

##nalcs_single_match <- get_acs_match_by_matchid(nalcs_matchid_df$Region.ID[[1]], nalcs_matchid_df$Game.ID[[1]], chr_game_hash = nalcs_matchid_df$Hash.ID[[1]])
##nalcs_single_match$teams["teamName"] <- unname(unlist(c(nalcs_matchid_df[1, c("Blue.Team", "Red.Team")])))

#nalcs_matches_teams_accum <- get_accum_matches_teams(nalcs_matches, nalcs_matchid_df)
#nalcs_matches_bans_accum <- get_accum_matches_bans(nalcs_matches, nalcs_matchid_df)
#nalcs_matches_participants_accum <- get_accum_matches_participants(nalcs_matches, nalcs_matchid_df)
#nalcs_matches_participants_accum <- nalcs_matches_participants_accum %>%
  #mutate_at(vars(contains("Deltas")), funs(replace(., is.na(.), 0)))
#nalcs_matches_participants_combined_accum <- get_accum_matches_participants(nalcs_matches, nalcs_matchid_df, combine_teammate_stats = TRUE)

## Joins the "teams" DF and the "participants combined" DF together
#nalcs_matches_tpc_accum <- nalcs_matches_participants_combined_accum %>%
  #inner_join(nalcs_matches_teams_accum)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# EU LCS data (Spring Split 2018)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# EU LCS 2018 Spring Split -- Regular Season and Playoffs
#eulcs_matchid_df <- read.csv("gameid_data/EULCS_Spring2018.csv")

#eulcs_matches <- get_league_match_data_list(eulcs_matchid_df)
#eulcs_matches_teams_accum <- get_accum_matches_teams(eulcs_matches, eulcs_matchid_df)
#eulcs_matches_bans_accum <- get_accum_matches_bans(eulcs_matches, eulcs_matchid_df)
#eulcs_matches_participants_accum <- get_accum_matches_participants(eulcs_matches, eulcs_matchid_df)
#eulcs_matches_participants_accum <- eulcs_matches_participants_accum %>%
  #mutate_at(vars(contains("Deltas")), funs(replace(., is.na(.), 0)))
#eulcs_matches_participants_combined_accum <- get_accum_matches_participants(eulcs_matches, eulcs_matchid_df, combine_teammate_stats = TRUE)
#eulcs_matches_tpc_accum <- eulcs_matches_participants_combined_accum %>%
  #inner_join(eulcs_matches_teams_accum)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Korea LCK data (Spring Split 2018)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Korea LCK 2018 Spring Split -- Regular Season, Promotion, and Playoffs
#lck_matchid_df <- read.csv("gameid_data/LCK_Spring2018.csv")

#lck_matches <- get_league_match_data_list(lck_matchid_df)
#lck_matches_teams_accum <- get_accum_matches_teams(lck_matches, lck_matchid_df)
#lck_matches_bans_accum <- get_accum_matches_bans(lck_matches, lck_matchid_df)
#lck_matches_participants_accum <- get_accum_matches_participants(lck_matches, lck_matchid_df)
#lck_matches_participants_accum <- lck_matches_participants_accum %>%
#mutate_at(vars(contains("Deltas")), funs(replace(., is.na(.), 0)))
#lck_matches_participants_combined_accum <- get_accum_matches_participants(lck_matches, lck_matchid_df, combine_teammate_stats = TRUE)
#lck_matches_tpc_accum <- lck_matches_participants_combined_accum %>%
#inner_join(lck_matches_teams_accum)



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