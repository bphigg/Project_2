library(httr)
library(tidyverse)
library(dplyr)
library(tidyr)
library(jsonlite)
library(ggplot2)
install.packages('gganimate')
library(gganimate)
install.packages("gifski")
library(gifski)
install.packages("png")
library(png)

ggplot(timmy_career, aes(points, rebounds)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  labs(title = "Season: {frame_time}", x = "points", y = "rebounds") +
  transition_time(season) +
  ease_aes("linear")

