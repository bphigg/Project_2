# Tim Duncan pts to rebs W/L.

spurs_2013_ps <- team_stats("Spurs", 2013, TRUE)
str(spurs_2013_ps)
spurs_2013_ps <- spurs_2013_ps %>% filter(home_team_id == "MIA" | visitor_team_id == "MIA")
timmy <- player_stats("duncan", "tim", 2013)
timmy_career_ <- player_stats("duncan", "tim")
timmy_career <- timmy_career_ %>% drop_na(pts, reb) %>% group_by(season) %>% summarize(mean(pts), mean(reb))

timmy_2013_team <- merge(timmy, spurs_2013_conf, by = "date")
str(timmy_2013_team)

g <- ggplot(timmy_2013_team, aes(x = pts, y = reb))
g + geom_point(aes(colour = location), alpha = 0.5, position = "jitter") +
  labs(x = "points", y = "rebounds", title = "Tim Duncan Points and Rebounds by location 2013-2014") + guides(color = guide_legend(title = "location")) + facet_wrap(~ opponent_div)

g <- ggplot(timmy_2013_team, aes(x = pts, y = reb))
g + geom_point(aes(colour = opponent_div), alpha = 0.5, position = "jitter") +
  labs(x = "points", y = "rebounds", title = "Tim Duncan Points and Rebounds vs division 2013-2014") + guides(color = guide_legend(title = "division")) + facet_wrap(~ W_L)

g <- ggplot(timmy_2013_team, aes(x = W_L, y = pts))
g + geom_boxplot(fill = 'grey') +
  labs(x = "Win/Loss", y = "points", title = "Tim Duncan Points by Win/Loss 2013-2014")


g <- ggplot(spurs_2013_conf, aes(x=spurs_score, fill = W_L))
g + geom_histogram(position = "dodge", binwidth=1) +
  labs(x = "Spurs Score", y = "Frequency", title = "Final Regular Season Scores by Win/Loss 2013-2014", fill = "W/L")