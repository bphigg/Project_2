player_stats_career <- function(last_name, first_name = ""){
  id <- player_id(last_name, first_name)
  n <- page_num(last_name, first_name)
  stats <- data.frame()
  
  for(i in 1:n){
    url <- paste0("https://free-nba.p.rapidapi.com/stats?seasons[]=", year, "&player_ids[]=", id)
    
    queryString <- list(
      page = i,
      per_page = "25"
    )
    
    response <- VERB("GET", url, add_headers('X-RapidAPI-Key' = '3009c8c91amsh65cad163db7085ap15d3d2jsnad2333f55e76', 'X-RapidAPI-Host' = 'free-nba.p.rapidapi.com'), content_type("application/octet-stream"))
    
    temp <- fromJSON(rawToChar(response$content))$data
    temp <- temp %>% select("ast", "blk", "dreb", "fg3_pct", "fg3a", "fg3m", "fg_pct", "fga", "fgm", "ft_pct", "fta", "ftm", "min", "oreb", "pf", "pts", "reb", "stl")
    stats <- rbind(stats, temp)
  }
  stats$min <- ms(stats$min, quiet = TRUE)
  
  #return(stats)
  return(colMeans(select(stats, ast:stl), na.rm=TRUE))
}
}