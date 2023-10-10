library(httr)

url <- "https://free-nba.p.rapidapi.com/stats?seasons[]=2009&player_ids[]=942"

queryString <- list(
  page = "0",
  per_page = "25"
)

response <- VERB("GET", url, add_headers('X-RapidAPI-Key' = '3009c8c91amsh65cad163db7085ap15d3d2jsnad2333f55e76', 'X-RapidAPI-Host' = 'free-nba.p.rapidapi.com'), content_type("application/octet-stream"))

content(response, "text")

library(dplyr)
library(jsonlite)

str(response)
parsed <- fromJSON(rawToChar(response$content))$data
parsed <- bind_cols(parsed %>% select(-team), parsed$team)
parsed <- bind_cols(parsed %>% select(-game), parsed$game)
parsed <- bind_cols(parsed %>% select(-player), parsed$player)
str(parsed)
head(parsed)
parsed$data
parsed <- parsed$data
str(parsed)
parsed
