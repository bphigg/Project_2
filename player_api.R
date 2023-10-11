library(httr)
library(lubridate)

url <- "https://free-nba.p.rapidapi.com/players/942"

queryString <- list(
  page = "0",
  per_page = "100"
)

response <- VERB("GET", url, add_headers('X-RapidAPI-Key' = '3009c8c91amsh65cad163db7085ap15d3d2jsnad2333f55e76', 'X-RapidAPI-Host' = 'free-nba.p.rapidapi.com'), content_type("application/octet-stream"))

content(response, "text")

library(dplyr)
library(jsonlite)

str(response)
parsed <- fromJSON(rawToChar(response$content))$data
str(parsed)
head(parsed)
filter(parsed, parsed$last_name == "Bryant")
parsed$data
parsed <- parsed$data
str(parsed)
parsed
