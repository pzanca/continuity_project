#### LOAD LIBRARIES, PREP DATA ####

library(tidyverse)
library(lme4)
library(merTools)

select <- dplyr::select

#Import schedule and results since 1995
schedules_all <- read_csv("data\\schedules_all.csv")

#Clean up the schedule data
schedules_clean <- schedules_all %>%
  mutate(date = lubridate::mdy(str_sub(date, start = 6))) %>%
  arrange(season, team_abbrev, g) %>%
  group_by(season, team_abbrev) %>%
  mutate(result = ifelse(l == lag(l), 1, 0),
         result = ifelse(is.na(result), w, result),
         season_w = sum(result),
         season_l = n() - season_w,
         season_wl_pct = season_w / (season_w + season_l),
         season_pts = sum(pts),
         season_opts = sum(opts),
         season_score_diff = (season_pts - season_opts) / n()) %>%
  ungroup() %>%
  filter(!is.na(result)) %>%
  select(season, date, team_master, team_local = team_abbrev, g, result, w, l, pts, opts, season_wl_pct, season_score_diff)

#Get previous seasons' wl_pct and score_diff
prev_stats <- schedules_clean %>%
  distinct(season, team_master, team_local, season_wl_pct, season_score_diff) %>%
  arrange(team_master, season) %>%
  group_by(team_master) %>%
  mutate(prev_wl_pct = lag(season_wl_pct),
         prev_score_diff = lag(season_score_diff)) %>%
  ungroup() %>%
  select(season, team_master, team_local, prev_wl_pct, prev_score_diff)

remove(schedules_all)


#Import player's MP by game since 1995
gamelogs_all <- read_csv("data\\gamelogs_all.csv")

#Clean up the gamelog data
gamelogs_clean <- gamelogs_all %>%
  filter(rk != "Rk") %>%
  mutate(date = as.Date(date),
         min = as.numeric(str_sub(mp, end = -4)) * 60,
         sec = as.numeric(str_sub(mp, start = -2)),
         mp = (min + sec) / 60) %>%
  select(season, date, team_local = tm, mp, player_link = link)

remove(gamelogs_all)


#Identify players who are returning to a team from one season to the next
returning_players <- gamelogs_clean %>%
  left_join(schedules_clean %>%
              distinct(season, team_master, team_local),
            by = c("season", "team_local")) %>%
  distinct(season, team_master, team_local, player_link) %>%
  arrange(player_link, team_master, season) %>%
  group_by(player_link, team_master) %>%
  mutate(returning_player = replace_na(ifelse(team_master == lag(team_master) & season == lag(season) + 1, 1, NA), 0)) %>%
  ungroup()

#Identify players who are returning to a team for at least a second season
returning_players_2 <- gamelogs_clean %>%
  left_join(schedules_clean %>%
              distinct(season, team_master, team_local),
            by = c("season", "team_local")) %>%
  distinct(season, team_master, team_local, player_link) %>%
  arrange(player_link, team_master, season) %>%
  group_by(player_link, team_master) %>%
  mutate(returning_player_2 = replace_na(ifelse((team_master == lag(team_master) & (season == lag(season) + 1)) &
                                                (team_master == lag(team_master, n = 2) & (season == lag(season, n = 2) + 2)) , 1, NA), 0)) %>%
  ungroup()

#Identify players who are returning to a team for at least a third season
returning_players_3 <- gamelogs_clean %>%
  left_join(schedules_clean %>%
              distinct(season, team_master, team_local),
            by = c("season", "team_local")) %>%
  distinct(season, team_master, team_local, player_link) %>%
  arrange(player_link, team_master, season) %>%
  group_by(player_link, team_master) %>%
  mutate(returning_player_3 = replace_na(ifelse((team_master == lag(team_master) & (season == lag(season) + 1)) &
                                                  (team_master == lag(team_master, n = 2) & (season == lag(season, n = 2) + 2)) &
                                                  (team_master == lag(team_master, n = 3) & (season == lag(season, n = 3) + 3)) , 1, NA), 0)) %>%
  ungroup()


#Add returning player variable to gamelog data
gamelogs_final <- gamelogs_clean %>%
  left_join(returning_players,
            by = c("season", "team_local", "player_link")) %>%
  left_join(returning_players_2,
            by = c("season", "team_master", "team_local", "player_link")) %>%
  left_join(returning_players_3,
            by = c("season", "team_master", "team_local", "player_link"))

remove(gamelogs_clean)


#Join all data together in one dataframe and remove the 1995 season since we don't have continuity for it.
full_data <- schedules_clean %>%
  left_join(prev_stats,
            by = c("season", "team_master", "team_local")) %>%
  left_join(gamelogs_final,
            by = c("season", "date", "team_master", "team_local")) %>%
  filter(season > 1995) %>%
  select(-season_wl_pct, -season_score_diff)


#### ANALYSIS ####

#### FIRST HYPOTHESIS ####

#Calculate continuity, win percentage, and avg point differential for each team and season.
continuity <- full_data %>%
  group_by(season, team_master, g) %>%
  mutate(returning_mp = mp * returning_player) %>% 
  summarize(result = max(result),
            pts = max(pts),
            opts = max(opts),
            returning_mp = sum(returning_mp, na.rm = TRUE),
            total_mp = sum(mp, na.rm = TRUE),
            prev_wl_pct = max(prev_wl_pct),
            prev_score_diff = max(prev_score_diff)) %>%
  ungroup() %>% 
  group_by(season, team_master) %>%
  summarize(wins = sum(result),
            losses = n() - wins,
            pts = sum(pts),
            opts = sum(opts),
            score_diff = (pts - opts) / n(),
            returning_mp = sum(returning_mp, na.rm = TRUE),
            total_mp = sum(total_mp, na.rm = TRUE),
            prev_wl_pct = max(prev_wl_pct),
            prev_score_diff = max(prev_score_diff),
            continuity = returning_mp / total_mp) %>%
  ungroup() %>%
  mutate(wl_pct = wins / (wins + losses)) %>%
  select(season, team_master, continuity, wins, losses, wl_pct, score_diff, prev_wl_pct, prev_score_diff)

## WIN-LOSS PERCENTAGE

#Visualize the relationship
continuity %>%
  ggplot(aes(x = continuity, y = wl_pct)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Roster Continuity",
       y = "Win-Loss Percentage",
       title = "Roster Continuity and Win-Loss Percentage",
       subtitle = "1995-96 through 2019-20 seasons") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#Simple linear regression model
lm_model <- lm(wl_pct ~ continuity, data = continuity)
summary(lm_model)

## AVG POINT DIFFERENTIAL

#Visualize the relationship
continuity %>%
  ggplot(aes(x = continuity, y = score_diff)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Roster Continuity",
       y = "Avg. Point Differential",
       title = "Roster Continuity and Avg. Point Differential",
       subtitle = "1995-96 through 2019-20 seasons") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#Simple linear regression model
lm_model <- lm(score_diff ~ continuity, data = continuity)
summary(lm_model)



#### SECOND HYPOTHESIS

#Look at continuity and [W/L Pct AND point differential] in following chunks:
# first half of season / second half of season
# 20 game blocks
# 10 game blocks
# 10 game rolling average over course of season

#How many blocks of games?
num_blocks <- 8

#Set up the data
game_blocks <- full_data %>%
  group_by(season, team_master, g) %>%
  mutate(returning_mp = mp * returning_player) %>% 
  summarize(result = max(result),
            pts = max(pts),
            opts = max(opts),
            returning_mp = sum(returning_mp, na.rm = TRUE),
            total_mp = sum(mp, na.rm = TRUE),
            prev_wl_pct = max(prev_wl_pct),
            prev_score_diff = max(prev_score_diff)) %>%
  mutate(season_block = ntile(g, num_blocks)) %>%
  ungroup() %>% 
  group_by(season, team_master, season_block) %>%
  summarize(wins = sum(result),
            losses = n() - wins,
            pts = sum(pts),
            opts = sum(opts),
            score_diff = (pts - opts) / n(),
            returning_mp = sum(returning_mp, na.rm = TRUE),
            total_mp = sum(total_mp, na.rm = TRUE),
            prev_wl_pct = max(prev_wl_pct),
            prev_score_diff = max(prev_score_diff),
            continuity = returning_mp / total_mp) %>%
  ungroup() %>%
  mutate(wl_pct = wins / (wins + losses)) %>%
  select(season, team_master, season_block, continuity, wins, losses, wl_pct, score_diff, prev_wl_pct, prev_score_diff)

## WIN-LOSS PERCENTAGE

#Plot continuity versus wl_pct
game_blocks %>%
  ggplot(aes(x = continuity, y = wl_pct)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Roster Continuity",
       y = "Win-Loss Percentage",
       title = "Roster Continuity and Win-Loss Percentage, by Portion of the Season",
       subtitle = "1995-96 through 2019-20 seasons") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~season_block, ncol = 2) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#Random Intercepts Model
random_int_model <- lmer(wl_pct ~ continuity + (1|season_block), data = game_blocks) # + prev_wl_pct
summary(random_int_model)

#Plot the random effects
plotREsim(REsim(random_int_model), labs = TRUE)

## AVG POINT DIFFERNTIAL

#Switch to score_diff
game_blocks %>%
  ggplot(aes(x = continuity, y = score_diff)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Roster Continuity",
       y = "Avg. Point Differential",
       title = "Roster Continuity and Avg. Point Differential, by Portion of the Season",
       subtitle = "1995-96 through 2019-20 seasons") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~season_block) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#Random Intercepts Model
random_int_model <- lmer(score_diff ~ continuity + (1|season_block), data = game_blocks) # + prev_score_diff
summary(random_int_model)

#Plot the random effects
plotREsim(REsim(random_int_model), labs = TRUE)


#### MULTIPLE YEAR CONTINUITY ####

#Calculate continuity for at least two seasons on the roster
continuity_2 <- full_data %>%
  group_by(season, team_master, g) %>%
  mutate(returning_mp = mp * returning_player_2) %>% 
  summarize(result = max(result),
            pts = max(pts),
            opts = max(opts),
            returning_mp = sum(returning_mp, na.rm = TRUE),
            total_mp = sum(mp, na.rm = TRUE),
            prev_wl_pct = max(prev_wl_pct),
            prev_score_diff = max(prev_score_diff)) %>%
  ungroup() %>% 
  group_by(season, team_master) %>%
  summarize(wins = sum(result),
            losses = n() - wins,
            pts = sum(pts),
            opts = sum(opts),
            score_diff = (pts - opts) / n(),
            returning_mp = sum(returning_mp, na.rm = TRUE),
            total_mp = sum(total_mp, na.rm = TRUE),
            prev_wl_pct = max(prev_wl_pct),
            prev_score_diff = max(prev_score_diff),
            continuity = returning_mp / total_mp) %>%
  ungroup() %>%
  mutate(wl_pct = wins / (wins + losses)) %>%
  select(season, team_master, continuity, wins, losses, wl_pct, score_diff, prev_wl_pct, prev_score_diff) %>%
  filter(season > 1996)

## WIN-LOSS PERCENTAGE

#Visualize the relationship
continuity_2 %>%
  ggplot(aes(x = continuity, y = wl_pct)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Roster Continuity",
       y = "Win-Loss Percentage",
       title = "Two-Year Roster Continuity and Win-Loss Percentage",
       subtitle = "1995-96 through 2019-20 seasons") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#Simple linear regression model
lm_model <- lm(wl_pct ~ continuity, data = continuity_2)
summary(lm_model)

## AVG POINT DIFFERENTIAL

#Visualize the relationship
continuity_2 %>%
  ggplot(aes(x = continuity, y = score_diff)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Roster Continuity",
       y = "Avg. Point Differential",
       title = "Two-Year Roster Continuity and Avg. Point Differential",
       subtitle = "1995-96 through 2019-20 seasons") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#Simple linear regression model
lm_model <- lm(score_diff ~ continuity, data = continuity_2)
summary(lm_model)


#Calculate continuity for at least three seasons on the roster
continuity_3 <- full_data %>%
  group_by(season, team_master, g) %>%
  mutate(returning_mp = mp * returning_player_3) %>% 
  summarize(result = max(result),
            pts = max(pts),
            opts = max(opts),
            returning_mp = sum(returning_mp, na.rm = TRUE),
            total_mp = sum(mp, na.rm = TRUE),
            prev_wl_pct = max(prev_wl_pct),
            prev_score_diff = max(prev_score_diff)) %>%
  ungroup() %>% 
  group_by(season, team_master) %>%
  summarize(wins = sum(result),
            losses = n() - wins,
            pts = sum(pts),
            opts = sum(opts),
            score_diff = (pts - opts) / n(),
            returning_mp = sum(returning_mp, na.rm = TRUE),
            total_mp = sum(total_mp, na.rm = TRUE),
            prev_wl_pct = max(prev_wl_pct),
            prev_score_diff = max(prev_score_diff),
            continuity = returning_mp / total_mp) %>%
  ungroup() %>%
  mutate(wl_pct = wins / (wins + losses)) %>%
  select(season, team_master, continuity, wins, losses, wl_pct, score_diff, prev_wl_pct, prev_score_diff) %>%
  filter(season > 1997)

## WIN-LOSS PERCENTAGE

#Visualize the relationship
continuity_3 %>%
  ggplot(aes(x = continuity, y = wl_pct)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Roster Continuity",
       y = "Win-Loss Percentage",
       title = "Three-Year Roster Continuity and Win-Loss Percentage",
       subtitle = "1995-96 through 2019-20 seasons") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#Simple linear regression model
lm_model <- lm(wl_pct ~ continuity, data = continuity_3)
summary(lm_model)

## AVG POINT DIFFERENTIAL

#Visualize the relationship
continuity_3 %>%
  ggplot(aes(x = continuity, y = score_diff)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Roster Continuity",
       y = "Avg. Point Differential",
       title = "Three-Year Roster Continuity and Avg. Point Differential",
       subtitle = "1995-96 through 2019-20 seasons") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#Simple linear regression model
lm_model <- lm(score_diff ~ continuity, data = continuity_3)
summary(lm_model)



