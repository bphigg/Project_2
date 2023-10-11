spurs_2013_r <- team_stats("Spurs", 2013)
str(spurs_2013_r)
spurs_2013_r <- spurs_2013_r %>%
  mutate(W_L = if_else(home_team_id == "SAS" & home_team_score > visitor_team_score, "W", 
                       if_else(visitor_team_id == "SAS" & visitor_team_score > home_team_score, "W", "L")))
table(spurs_2013_r$W_L)
spurs_2013_home <- spurs_2013_r %>% filter(visitor_team_id != "SAS")
spurs_2013_away <- spurs_2013_r %>% filter(home_team_id != "SAS")

table(spurs_2013_home$W_L)
table(spurs_2013_away$W_L)

spurs_2013_conf <- spurs_2013_away %>% select(date, home_team_id, home_team_conf, home_team_div, home_team_score, visitor_team_id, visitor_team_score, W_L) %>% rename(opponent = home_team_id, opponent_conf = home_team_conf, opponent_div = home_team_div, opponent_score = home_team_score, SA = visitor_team_id, spurs_score = visitor_team_score)

spurs_2013_conf_ <- spurs_2013_home %>% select(date, visitor_team_id, visitor_team_conf, visitor_team_div, visitor_team_score, home_team_id, home_team_score, W_L) %>% rename(opponent = visitor_team_id, opponent_conf = visitor_team_conf, opponent_div = visitor_team_div, opponent_score = visitor_team_score, SA = home_team_id, spurs_score = home_team_score)

spurs_2013_conf <- rbind(spurs_2013_conf, spurs_2013_conf_)
table(spurs_2013_conf$W_L, spurs_2013_conf$opponent_conf)
table(spurs_2013_conf$W_L, spurs_2013_conf$opponent_div)

spurs_2013_conf %>% summarize(mean(opponent_score), mean(spurs_score))