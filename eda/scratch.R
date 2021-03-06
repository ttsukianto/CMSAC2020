library(tidyverse)
library(patchwork)
library(GGally)

nfl <- read_csv("http://www.stat.cmu.edu/cmsac/sure/materials/data/eda_projects/nfl_teams_season_summary.csv")
str(nfl)

# Q1. Does good defense tend to win games?

nfl_strat <- nfl %>%
  mutate(matches = wins + losses,
         win_loss_ratio = wins/losses,
         run_def = ifelse(-1*run_def_total_epa >= run_off_total_epa, TRUE, FALSE),
         pass_def = ifelse(-1*pass_def_total_epa >= pass_off_total_epa, TRUE, FALSE),
         def_total_epa = run_def_total_epa + pass_def_total_epa, # overall "defensiveness"
         off_total_epa = run_off_total_epa + pass_off_total_epa, # overall "offensiveness"
         strategy = as.factor(ifelse(run_def & pass_def, "Pure Defense",
                                     ifelse(!run_def & pass_def, "Def Pass/Off Run",
                                            ifelse(run_def & !pass_def, "Off Pass/Def Run",
                                                   ifelse(!run_def & !pass_def, "Pure Offense", "")
                                            )
                                     ))
         ),
         strategy = fct_relevel(strategy, c("Pure Defense", "Def Pass/Off Run", "Off Pass/Def Run", "Pure Offense"))
  ) %>%
  group_by(season, division) %>%
  mutate(final_rank = as.factor(dense_rank(desc(win_loss_ratio)))) %>%
  ungroup()

#nfl_strat %>%
  #ggplot(aes(final_rank, fill = strategy)) +
  #geom_bar() +
  #facet_wrap(vars(division)) +
  #theme_bw()

#nfl_strat %>%
  #ggplot(aes(strategy, wins)) +
  #geom_violin() +
  #geom_jitter() +
  #theme_bw()

pass <- nfl_strat %>%
  ggplot(aes(-1*pass_def_epa_per_att, pass_off_epa_per_att, z = wins)) +
  stat_summary_hex(color = "black",
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  labs(x = "-1 * Pass Defense EPA per Attempt",
       y = "Pass Offense EPA per Attempt",
       title = "Mean Wins by EPA") + 
  theme_bw() +
  theme(legend.position = "none") +
  coord_fixed()

run <- nfl_strat %>% 
  ggplot(aes(-1*run_def_epa_per_att, run_off_epa_per_att, z = wins)) +
  stat_summary_hex(color = "black",
                   fun = mean) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  labs(x = "-1 * Run Defense EPA per Attempt",
       y = "Run Offense EPA per Attempt",
       fill = "Mean Wins") +
  theme_bw() +
  coord_fixed()

pass + run + plot_layout(guides = "collect")

# Multivariate hierarchical clustering

nfl_multidim_clust <- protoclust(dist(dplyr::select(nfl_strat, pass_def_epa_per_att, pass_off_epa_per_att, run_def_epa_per_att, run_off_epa_per_att)))

nfl_multidim_clusters <- protocut(nfl_multidim_clust, k = 4)
table("Final Rank" = nfl_strat$final_rank,
      "Clusters" = nfl_multidim_clusters$cl)

nfl_strat <- nfl_strat %>% 
  mutate(full_minimax_clusters = as.factor(nfl_multidim_clusters$cl))

ggpairs(nfl_strat, columns = c("pass_def_epa_per_att", "pass_off_epa_per_att", "run_def_epa_per_att", "run_off_epa_per_att"),
        aes(color = full_minimax_clusters))


large_div <- nfl %>% dplyr::mutate(conference = substr(division, 1, 3))

large_div %>%
  ggplot(aes(x = fct_reorder(as.factor(team), wins), y = wins, fill = conference)) +
  geom_bar(stat = "identity") + 
  theme_bw() +
  #facet_wrap(~season, ncol = 3) +
  labs(title = "Which is stronger: AFC vs. NFC? (200-2019)", 
       subtitle = "Regular season wins during the 2019 season",
       x = "Team",
       y = "Total Wins") +
  coord_flip() +
  geom_vline(xintercept = 16.5, 
             linetype = "dashed", 
             color = "black")

nfl_team_rename <- nfl %>% 
  mutate(team = ifelse(team == "STL", "LA", team),
         team = ifelse(team == "SD", "LAC", team))
