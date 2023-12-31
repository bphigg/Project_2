library(httr)

url <- "https://free-nba.p.rapidapi.com/games?seasons[]=2010&team_ids[]=14&postseason=TRUE"

queryString <- list(
  page = "0",
  per_page = "25"
)

response <- VERB("GET", url, query = queryString, add_headers('X-RapidAPI-Key' = '3009c8c91amsh65cad163db7085ap15d3d2jsnad2333f55e76', 'X-RapidAPI-Host' = 'free-nba.p.rapidapi.com'), content_type("application/octet-stream"))

content(response, "text")

library(dplyr)
library(jsonlite)

str(response)
parsed <- fromJSON(rawToChar(response$content))
parsed$data
parsed <- parsed$data
str(parsed)
parsed
