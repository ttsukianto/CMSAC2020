library(tidyverse)

nfl <- read_csv("http://www.stat.cmu.edu/cmsac/sure/materials/data/eda_projects/nfl_teams_season_summary.csv")
str(nfl)

# Q1. Does good defense tend to win games?
# Q2. Who is the GOAT in each division? / overall?
# Q3. Which is better, passing or running with the ball?

nfl %>%
  ggplot(aes(x = wins, color = division)) +
  geom_freqpoly()  +
  theme_bw() +
  theme(legend.position = "bottom")


nfl %>%
  ggplot(aes(x = wins, 
             y = pass_off_epa_per_att)) +
  annotate(geom = "text", x = 12, y = -0.2, label = paste("r =", cor(nfl$wins, nfl$pass_off_epa_per_att))) +
  geom_point()

