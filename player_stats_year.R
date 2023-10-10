player_stats_year <- function(last_name, first_name = "", year){
  id <- player_id(last_name, first_name)
  n <- page_num(last_name, first_name, year)
  stats <- data.frame()
  
  for(i in 1:n){
    url <- paste0("https://free-nba.p.rapidapi.com/stats?seasons[]=", year, "&player_ids[]=", id)
    
    queryString <- list(
      page = i,
      per_page = "25"
    )
    
    response <- VERB("GET", url, add_headers('X-RapidAPI-Key' = '3009c8c91amsh65cad163db7085ap15d3d2jsnad2333f55e76', 'X-RapidAPI-Host' = 'free-nba.p.rapidapi.com'), content_type("application/octet-stream"))
    
    temp <- fromJSON(rawToChar(response$content))$data
    
    temp <- temp %>% select("ast", "blk", "min", "pf", "pts", "reb", "stl")
    stats <- rbind(stats, temp)
  }
  stats <- stats %>% mutate(min=substr(stats$min, 1, 2))
  stats$min <- as.numeric(stats$min)
  
  #return(stats)
  return(colMeans(select(stats, ast:stl), na.rm=TRUE))
}
player_stats_year("duncan", "tim", 2014)

