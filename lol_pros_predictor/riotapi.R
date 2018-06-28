prefix_domain <- "https://na1.api.riotgames.com"

# This api_token will eventually go in a separate file which will be ignored by git.
#api_token <- ""


get_summoner_by_name <- function(str_name) {
  response <- GET(paste(prefix_domain, "/lol/summoner/v3/summoners/by-name/", str_name, sep = ""), add_headers("X-Riot-Token" = api_token))
  if (response$status_code == 200) {
    json <- jsonlite::fromJSON(content(response, as = "text"))
    return (json)
  }
}
 
 
get_matchlist_by_accountid_and_season <- function(num_accountid, num_season) {
  response <- GET(paste(prefix_domain, "/lol/match/v3/matchlists/by-account/", num_accountid, "?season=", num_season, sep = ""), add_headers("X-Riot-Token" = api_token))
  if (response$status_code == 200) {
    json <- jsonlite::fromJSON(content(response, as = "text"))
    return (json)
  }
}
 
 
get_match_by_matchid <- function(num_matchid) {
  response <- GET(paste(prefix_domain, "/lol/match/v3/matches/", num_matchid, sep = ""), add_headers("X-Riot-Token" = api_token))
  if (response$status_code == 200) {
    json <- jsonlite::fromJSON(content(response, as = "text"))
    return (json)
  }
}
 
 
get_match_timeline_by_matchid <- function(num_matchid) {
  response <- GET(paste(prefix_domain, "/lol/match/v3/timelines/by-match/", num_matchid, sep = ""), add_headers("X-Riot-Token" = api_token))
  if (response$status_code == 200) {
    json <- jsonlite::fromJSON(content(response, as = "text"))
    return (json)
  }
}