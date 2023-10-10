get_team_id <- function(team){

url <- "https://free-nba.p.rapidapi.com/teams"

queryString <- list(page = "0")

response <- VERB("GET", url, query = queryString, add_headers('X-RapidAPI-Key' = '3009c8c91amsh65cad163db7085ap15d3d2jsnad2333f55e76', 'X-RapidAPI-Host' = 'free-nba.p.rapidapi.com'), content_type("application/octet-stream"))

parsed <- fromJSON(rawToChar(response$content))$data

filter <- parsed %>% filter(name == team)
return(filter$id)
}
get_team_id("Spurs")

##### page num with year ######

page_num_team <- function(team, season, postseason){
  id <- get_team_id(team)
  
  url <- paste0("https://free-nba.p.rapidapi.com/games?seasons[]=", year, "&team_ids[]=", id, "&postseason=", postseason)
  
  queryString <- list(
    page = "0",
    per_page = "25"
  )
  
  response <- VERB("GET", url, add_headers('X-RapidAPI-Key' = '3009c8c91amsh65cad163db7085ap15d3d2jsnad2333f55e76', 'X-RapidAPI-Host' = 'free-nba.p.rapidapi.com'), content_type("application/octet-stream"))
  
  n <- fromJSON(rawToChar(response$content))$meta$total_pages[[1]]
  return(n)
}
page_num_team("Spurs", 2014, TRUE)
