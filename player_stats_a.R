player_id <- function(last_name, first_name = ""){
  
  url <- "https://free-nba.p.rapidapi.com/players"
  
  queryString <- list(
    page = "0",
    per_page = "25",
    search = paste(first_name, last_name)
  )
  
  response <- VERB("GET", url, query = queryString, add_headers('X-RapidAPI-Key' = '3009c8c91amsh65cad163db7085ap15d3d2jsnad2333f55e76', 'X-RapidAPI-Host' = 'free-nba.p.rapidapi.com'), content_type("application/octet-stream"))
  
  id <- fromJSON(rawToChar(response$content))$data
  id <- bind_cols(id %>% select(-team), id$team)
  attributes(id)$names[1] <- "player_id"
  attributes(id)$names[8] <- "team_id"
  attributes(id)$names[9] <- "team"
  
  return(id %>% select("player_id", "first_name", "last_name", "team"))
}
temp <- player_id("finley", "")
