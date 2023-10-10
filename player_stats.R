player_stats <- function(last_name, first_name = "", year = 'career'){
  if(year == "career"){
    return(player_stats_career(last_name, first_name))
  } else {
    return(player_stats_year(last_name, first_name, year))
  }
}
player_stats("duncan", "tim", year = "career")

