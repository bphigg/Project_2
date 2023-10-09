page_num <- function(last_name, first_name = "", year = "career"){
  id <- player_id(last_name, first_name)
  
  url <- paste0("https://free-nba.p.rapidapi.com/stats?seasons[]=", year, "&player_ids[]=", id)
  
  queryString <- list(
    page = "0",
    per_page = "25"
  )
  
  response <- VERB("GET", url, add_headers('X-RapidAPI-Key' = '3009c8c91amsh65cad163db7085ap15d3d2jsnad2333f55e76', 'X-RapidAPI-Host' = 'free-nba.p.rapidapi.com'), content_type("application/octet-stream"))
  
  n <- fromJSON(rawToChar(response$content))$meta$total_pages[[1]]
  return(n)
}


player_id <- function(last_name, first_name = ""){
  
  url <- "https://free-nba.p.rapidapi.com/players"
  
  queryString <- list(
    page = "0",
    per_page = "25",
    search = paste(first_name, last_name)
  )
  
  response <- VERB("GET", url, query = queryString, add_headers('X-RapidAPI-Key' = '3009c8c91amsh65cad163db7085ap15d3d2jsnad2333f55e76', 'X-RapidAPI-Host' = 'free-nba.p.rapidapi.com'), content_type("application/octet-stream"))
  
  id <- fromJSON(rawToChar(response$content))$data[1]
  attributes(id)$names[1] <- "player_id"
  
  return(id)
}