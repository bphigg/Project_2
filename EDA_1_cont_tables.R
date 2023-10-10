spurs_2012_r <- team_stats("Spurs", 2012)
str(spurs_2012_r)
spurs_2012_r <- spurs_2012_r %>%
  mutate(W_L = if_else(home_team_id == "SAS" & home_team_score > visitor_team_score, "W", 
                       if_else(visitor_team_id == "SAS" & visitor_team_score > home_team_score, "W", "L")))
table(spurs_2012_r$W_L)
spurs_2012_home <- spurs_2012_r %>% filter(visitor_team_id != "SAS")
spurs_2012_away <- spurs_2012_r %>% filter(home_team_id != "SAS")

table(spurs_2012_home$W_L)
table(spurs_2012_away$W_L)

spurs_2012_conf <- spurs_2012_away %>% select(home_team_id, home_team_conf, home_team_div, home_team_score, visitor_team_id, visitor_team_score, W_L) %>% rename(opponent = home_team_id, opponent_conf = home_team_conf, opponent_div = home_team_div, opponent_score = home_team_score, SA = visitor_team_id, spurs_score = visitor_team_score)

spurs_2012_conf_ <- spurs_2012_home %>% select(visitor_team_id, visitor_team_conf, visitor_team_div, visitor_team_score, home_team_id, home_team_score, W_L) %>% rename(opponent = visitor_team_id, opponent_conf = visitor_team_conf, opponent_div = visitor_team_div, opponent_score = visitor_team_score, SA = home_team_id, spurs_score = home_team_score)

spurs_2012_conf <- rbind(spurs_2012_conf, spurs_2012_conf_)
table(spurs_2012_conf$W_L, spurs_2012_conf$opponent_conf)
table(spurs_2012_conf$W_L, spurs_2012_conf$opponent_div)

spurs_2012_conf %>% summarize(mean(opponent_score), mean(spurs_score))