timmy <- player_stats("duncan", "tim")
str(timmy)
kobe <- player_stats("bryant", "kobe")
str(kobe)
kobe
colMeans(select(timmy, ast:stl), na.rm=TRUE)
spurs_2014_ps <- team_stats("Spurs", 2014, TRUE)
timmy_2014_ps <- merge(timmy, spurs_2014_ps, by = "date")
timmy_2014_ps
spurs_2014_ps <- spurs_2014_ps %>%
  mutate(W_L = if_else(home_team_id == "SAS" & home_team_score > visitor_team_score, "W", 
                       if_else(visitor_team_id == "SAS" & visitor_team_score > home_team_score, "W", "L")))
spurs_2014_ps

kobe_v_spurs <- kobe %>% filter(home_team_id == 27 | visitor_team_id == 27)
kobe_v_spurs
timmy_v_lakers <- timmy %>% filter(home_team_id == 14 | visitor_team_id == 14)
timmy_v_lakers
timmy_v_kobe <- rbind(timmy_v_lakers, kobe_v_spurs)
timmy_v_kobe %>% drop_na(pts, reb, ast, blk, min) %>% group_by(team) %>% summarize(mean(pts), mean(reb), mean(ast), mean(blk), mean(min))
