player_stats_career <- function(last_name, first_name = ""){
  id <- player_id(last_name, first_name)
  n <- page_num_career(last_name, first_name)
  stats <- data.frame()
  
  for(i in 1:n){
    url <- paste0("https://free-nba.p.rapidapi.com/stats?player_ids[]=", id)
    
    queryString <- list(
      page = i,
      per_page = "25"
    )
    
    response <- VERB("GET", url, query = queryString, add_headers('X-RapidAPI-Key' = '3009c8c91amsh65cad163db7085ap15d3d2jsnad2333f55e76', 'X-RapidAPI-Host' = 'free-nba.p.rapidapi.com'), content_type("application/octet-stream"))
    
    temp <- fromJSON(rawToChar(response$content))$data
    attributes(temp$team)$names[1] <- "teamid"
    temp <- bind_cols(temp %>% select(-team), temp$team)
    attributes(temp$game)$names[1] <- "gameid"
    temp <- bind_cols(temp %>% select(-game), temp$game)
    attributes(temp$player)$names[1] <- "playerid"
    temp <- bind_cols(temp %>% select(-player), temp$player)
    
    temp <- temp %>% select("first_name", "last_name", "abbreviation", "date", "season", "home_team_id", "home_team_score", "visitor_team_id", "visitor_team_score", "ast", "blk", "min", "pf", "pts", "reb", "stl")
    stats <- rbind(stats, temp)
  }
  stats <- stats %>% mutate(min=substr(stats$min, 1, 2))
  stats$min <- as.numeric(stats$min)
  attributes(stats)$names[3] <- "team"
  
  return(stats)
  #return(colMeans(select(stats, ast:stl), na.rm=TRUE))
}
player_stats_career("bryant", "kobe")
