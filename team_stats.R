team_stats <- function(team, season, postseason = FALSE){
  id <- get_team_id(team)
  n <- page_num_team(team, season, postseason)
  team_stats <- data.frame()
  
  for(i in 1:n){
    url <- paste0("https://free-nba.p.rapidapi.com/games?seasons[]=", season, "&team_ids[]=", id, "&postseason=", postseason)
    
    queryString <- list(
      page = "0",
      per_page = "25"
    )
    
    response <- VERB("GET", url, add_headers('X-RapidAPI-Key' = '3009c8c91amsh65cad163db7085ap15d3d2jsnad2333f55e76', 'X-RapidAPI-Host' = 'free-nba.p.rapidapi.com'), content_type("application/octet-stream"))
    
    temp <- fromJSON(rawToChar(response$content))$data
    temp <- bind_cols(temp %>% select(-home_team), temp$home_team)
    attributes(temp)$names[12:17] <- c("home_team_id", "home_team_city", "home_team_conf", "home_team_div", "home_team", "home_team_name")
    temp <- bind_cols(temp %>% select(-visitor_team), temp$visitor_team)
    attributes(temp)$names[18:23] <- c("visitor_team_id", "visitor_team_city", "visitor_team_conf", "visitor_team_div", "visitor_team", "visitor_team_name")
    
    temp <- temp %>% select("date", "postseason", "season", "home_team_id", "home_team_city", "home_team_conf", "home_team_div", "home_team_name", "home_team_score", "visitor_team_id", "visitor_team_city", "visitor_team_conf", "visitor_team_div", "visitor_team_name", "visitor_team_score")
    
    team_stats <- rbind(team_stats, temp)
  }

  return(team_stats)
}
spurs <- team_stats("Spurs", "2014")
spurs
