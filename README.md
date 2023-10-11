-   [0.1 Summary](#summary)
-   [0.2 Required Packages](#required-packages)
-   [0.3 Query API](#query-api)

## 0.1 Summary

This project encompasses querying data from an API and manipulating the
results to produce some insightful EDA’s.

I chose the basketball API [Free
NBA](https://rapidapi.com/theapiguy/api/free-nba) from
[rapidapi](https://rapidapi.com) because I thought it would be
interesting to compare player and team information in a way that gave
the user more control than traditional basketball reference websites. As
we will see, this did provide some interesting and frustrating results.

## 0.2 Required Packages

Here is a list of the packages that were used in this project.

    library(httr)
    library(tidyverse)
    library(dplyr)
    library(tidyr)
    library(jsonlite)
    library(ggplot2)
    library(gganimate)
    library(gifski)
    library(png)

## 0.3 Query API

My plan for the API query was to create a call that would allow the user
to pull player and game data. For the player data, I would allow the
user to select a specific player and season to collect data on or they
could collect data on a player’s entire career. For the game data, I
wanted to allow the user to select the team, season, and specify whether
they wanted regular or postseason data.

The endpoints of the API:  
\* `Players` - contains player name, id, physical info (height, weight,
etc), drafted team name \* `Teams` - contains team name, id, city, team
abbreviation, team full name \* `Games` - date of game, game id, team
ids (home & visitor), scores (home & visitor), season, postseason \*
`Stats` - essentially the box score for each player on every game
they’ve played (points, rebounds, etc)

There were a few obstacles to creating the API query that should be
mentioned here since a large part of constructing the functions was to
get around them.

The first issue is that the Free NBA API appears to be designed for
websites or apps and NOT for data mining. Results from the API query
come back “paged” with only the first page being pulled. This makes
sense if one is pulling the data in through a website or app and can
click on “page 2” to pull the next page of data. But for my purposes, a
loop needed to be written to collect each page of data at a time.

The second issue was that the parameter options provided by the API were
not very helpful. Specific players or teams could be searched for if one
had the `player_id` or `team_id` - both of which were numeric keys that
had no immediate reference.

The first step to creating the `player_stats()` query was to write a
small function that could take in the player’s name and return the
`player_id` to be used in the `Stats` endpoint.

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

<br> Next, in order to loop through all the “pages” and collect the
data, I’ll need to know how many pages are associated with the queried
player. Additionally, the API query will have the option of collecting
data from one particular season or for a whole career, so the length of
the data can vary considerably. Therefore, I set up a function to
determine the total number of pages that would be returned based on the
input parameters. This function is designed for a specific `year`
parameter:

    # The final function will have year default to "career" if it is left empty. For this piece, year will have a numeric value (2010, 2015, 2023, etc)
    page_num <- function(last_name, first_name = "", year = "career"){
    # This is the previous function that identifies the player_id
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

The input parameters for this function will be the same as the input
parameters of the final function. In fact, this function returns the
first “page” of our query, but since we don’t know initially how many
total pages the query is, we have to run this function first to get `n`,
the total number of pages we need to loop through.

If no year is specified in the query parameter, then the query will
default to career stats. Since the URL in the above function is set up
for “?seasons\[\]= year”, I’ll need to modify it slightly in order to
get the total number of pages for the player’s career stats.

    page_num_career <- function(last_name, first_name = ""){
      id <- player_id(last_name, first_name)
    # I've removed the season parameter from the URL
      url <- paste0("https://free-nba.p.rapidapi.com/stats?player_ids[]=", id)
      
      queryString <- list(
        page = "0",
        per_page = "25"
      )
      
      response <- VERB("GET", url, add_headers('X-RapidAPI-Key' = '3009c8c91amsh65cad163db7085ap15d3d2jsnad2333f55e76', 'X-RapidAPI-Host' = 'free-nba.p.rapidapi.com'), content_type("application/octet-stream"))
      
      n <- fromJSON(rawToChar(response$content))$meta$total_pages[[1]]
      return(n)
    }

<br> Now I’m ready to construct the query that will return the player’s
stats by year. As I mentioned above, it is very similar to the function
that the returns the number of pages. The main difference is in that
function I accessed the `$meta` component in the `response$content`, but
now I will access the `$data` component.

Let’s take a look at the `player_stats_year()` function

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
        
        response <- VERB("GET", url, query = queryString, add_headers('X-RapidAPI-Key' = '3009c8c91amsh65cad163db7085ap15d3d2jsnad2333f55e76', 'X-RapidAPI-Host' = 'free-nba.p.rapidapi.com'), content_type("application/octet-stream"))
        
        temp <- fromJSON(rawToChar(response$content))$data
        temp <- bind_cols(temp %>% select(-team), temp$team)
        temp <- bind_cols(temp %>% select(-game), temp$game)
        temp <- bind_cols(temp %>% select(-player), temp$player)
        
        temp <- temp %>% select("first_name", "last_name", "abbreviation", "date", "season", "home_team_id", "home_team_score", "visitor_team_id", "visitor_team_score", "ast", "blk", "min", "pf", "pts", "reb", "stl")
        stats <- rbind(stats, temp)
      }
      stats <- stats %>% mutate(min=substr(stats$min, 1, 2))
      stats$min <- as.numeric(stats$min)
      attributes(stats)$names[3] <- "team"
      
      return(stats)
    }

After the `player_id()` and `page_num()` functions, I’ve initiated the
empty dataframe `stats`. This is where I’ll collect my pages of data
from the `for loop`. Next is the `for loop` in which I’ll iterate the
API call 1:n times, with each time being a successive page number. This
is reflected in the `queryString` as page = i.

After I collected the `response`, things got a little tricky. Initially,
the `for loop` did not work because each successive page had the same
index (1:25) and so it was throwing an error about rows already
existing. So I decided the best way around this was to go ahead and
select the relevant columns before it executed `rbind()`. However, this
led to another issue - the returned `$data` data frame had subsetted
data frames. Essentially, all the relevant Player, Team, and Game
information from the API call were subsetted data frames. So in order to
bring those subsetted data frames up to a column level that can be
`selected()`, I used `bind_cols()`. I’m not sure if this was the best
way to go about it, but it did work.  
Finally, there were two different ways the `min` data was recorded - as
an integer and in ms format (mm:ss). After some unsuccessful trials with
`lubridate()` to get these two different formats uniform, I decided the
seconds portion of the `min` variable was kind of unnecessary. So I
removed the seconds portion and converted the entire column to numeric
so it could be aggregated.

I have a very similar function for the Player Stats by Career query.
Exactly like the `page_num_career()` function, it simply has a modified
URL.

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
        temp <- bind_cols(temp %>% select(-team), temp$team)
        temp <- bind_cols(temp %>% select(-game), temp$game)
        temp <- bind_cols(temp %>% select(-player), temp$player)
        
        temp <- temp %>% select("first_name", "last_name", "abbreviation", "date", "season", "home_team_id", "home_team_score", "visitor_team_id", "visitor_team_score", "ast", "blk", "min", "pf", "pts", "reb", "stl")
        stats <- rbind(stats, temp)
      }
      stats <- stats %>% mutate(min=substr(stats$min, 1, 2))
      stats$min <- as.numeric(stats$min)
      attributes(stats)$names[3] <- "team"
      
      return(stats)
    }

<br> Finally, we can put it all together in one wrapper function that
will determine which function to use: `player_stats_year()` or
`player_stats_career()`

    player_stats <- function(last_name, first_name = "", year = 'career'){
      if(year == "career"){
        return(player_stats_career(last_name, first_name))
      } else {
        return(player_stats_year(last_name, first_name, year))
      }
    }

<br>
