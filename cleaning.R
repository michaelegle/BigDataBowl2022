####### Data Cleaning and Setup ######
library(tidyverse)
library(janitor)

data18 <- read_csv("data/tracking2018.csv")
data19 <- read_csv("data/tracking2019.csv")
data20 <- read_csv("data/tracking2020.csv")

plays <- read_csv("data/plays.csv")
players <- read_csv("data/players.csv")
games <- read_csv("data/games.csv")

data18 <- data18 %>%
  janitor::clean_names()
data19 <- data19 %>%
  janitor::clean_names()
data20 <- data20 %>%
  janitor::clean_names()
plays <- plays %>%
  janitor::clean_names()
players <- players %>%
  janitor::clean_names()
games <- games %>%
  janitor::clean_names()


plays <- left_join(plays, games, by = "game_id") %>%
  mutate(defense_team = ifelse(possession_team == home_team_abbr,
                              visitor_team_abbr,
                              home_team_abbr))

######## add the play-level data #######

data18 <- data18 %>%
  left_join(plays %>%
              select(game_id, play_id, possession_team, yardline_side, yardline_number,
                     home_team_abbr, visitor_team_abbr),
            by = c("game_id", "play_id"))

data19 <- data19 %>%
  left_join(plays %>%
              select(game_id, play_id, possession_team, yardline_side, yardline_number,
                     home_team_abbr, visitor_team_abbr),
            by = c("game_id", "play_id"))

data20 <- data20 %>%
  left_join(plays %>%
              select(game_id, play_id, possession_team, yardline_side, yardline_number,
                     home_team_abbr, visitor_team_abbr),
            by = c("game_id", "play_id"))

####### add extra player level data ########

data18 <- data18 %>%
  mutate(defense_team = ifelse(possession_team == home_team_abbr, # define which team is on defense
                              visitor_team_abbr,
                              home_team_abbr),
         team_abbr = ifelse(team == "home",
                           home_team_abbr,
                           visitor_team_abbr),
         on_defense = ifelse(defense_team == team_abbr, T, F), # is a player on defense?
         on_offense = ifelse(possession_team == team_abbr, T, F))

data19 <- data19 %>%
  mutate(defense_team = ifelse(possession_team == home_team_abbr, 
                               visitor_team_abbr,
                               home_team_abbr),
         team_abbr = ifelse(team == "home",
                            home_team_abbr,
                            visitor_team_abbr),
         on_defense = ifelse(defense_team == team_abbr, T, F), 
         on_offense = ifelse(possession_team == team_abbr, T, F))

data20 <- data20 %>%
  mutate(defense_team = ifelse(possession_team == home_team_abbr,
                               visitor_team_abbr,
                               home_team_abbr),
         team_abbr = ifelse(team == "home",
                            home_team_abbr,
                            visitor_team_abbr),
         on_defense = ifelse(defense_team == team_abbr, T, F),
         on_offense = ifelse(possession_team == team_abbr, T, F))

####### Standardize coordinates #######

data18 <- data18 %>%
  mutate(x = ifelse(play_direction == "left",
                    120-x, 
                    x), 
         y = ifelse(play_direction == "left",
                    160/3 - y, 
                    y),
         dir = ifelse(play_direction == "left",
                      ifelse(dir <= 180,
                             dir + 180,
                             dir - 180),
                      dir),
         o = ifelse(play_direction == "left",
                    ifelse(o <= 180,
                           o + 180,
                           o - 180),
                    o),
         dir = pi*dir/180,
         o = pi*o/180,
         dx = dis * sin(dir),
         dy = dis * cos(dir),
         x_los = case_when(
           possession_team == yardline_side ~ 10 + yardline_number,
           defense_team == yardline_side ~ 110 - yardline_number,
           yardline_number == 50 ~ 60
         ),
         x_depth = x - x_los)

data19 <- data19 %>%
  mutate(x = ifelse(play_direction == "left",
                    120-x, 
                    x), 
         y = ifelse(play_direction == "left",
                    160/3 - y, 
                    y),
         dir = ifelse(play_direction == "left",
                      ifelse(dir <= 180,
                             dir + 180,
                             dir - 180),
                      dir),
         o = ifelse(play_direction == "left",
                    ifelse(o <= 180,
                           o + 180,
                           o - 180),
                    o),
         dir = pi*dir/180,
         o = pi*o/180,
         dx = dis * sin(dir),
         dy = dis * cos(dir),
         x_los = case_when(
           possession_team == yardline_side ~ 10 + yardline_number,
           defense_team == yardline_side ~ 110 - yardline_number,
           yardline_number == 50 ~ 60
         ),
         x_depth = x - x_los)

data20 <- data20 %>%
  mutate(x = ifelse(play_direction == "left",
                    120-x, 
                    x), 
         y = ifelse(play_direction == "left",
                    160/3 - y, 
                    y),
         dir = ifelse(play_direction == "left",
                      ifelse(dir <= 180,
                             dir + 180,
                             dir - 180),
                      dir),
         o = ifelse(play_direction == "left",
                    ifelse(o <= 180,
                           o + 180,
                           o - 180),
                    o),
         dir = pi*dir/180,
         o = pi*o/180,
         dx = dis * sin(dir),
         dy = dis * cos(dir),
         x_los = case_when(
           possession_team == yardline_side ~ 10 + yardline_number,
           defense_team == yardline_side ~ 110 - yardline_number,
           yardline_number == 50 ~ 60
         ),
         x_depth = x - x_los)

###### Add estimated end coordinates based on player's direction and speed ######

data18 <- data18 %>%
  mutate(x_end = (s * cos(dir)) + x,
         y_end = (s * sin(dir)) + y)

data19 <- data19 %>%
  mutate(x_end = (s * cos(dir)) + x,
         y_end = (s * sin(dir)) + y)

data20 <- data20 %>%
  mutate(x_end = (s * cos(dir)) + x,
         y_end = (s * sin(dir)) + y)

###### Filter out non-punt plays as well as fake punts #######

data18 <- data18 %>%
  filter(special_teams_play_type == "Punt") %>%
  filter(special_teams_result != "Non-Special Teams Result")

data19 <- data19 %>%
  filter(special_teams_play_type == "Punt") %>%
  filter(special_teams_result != "Non-Special Teams Result")

data20 <- data20 %>%
  filter(special_teams_play_type == "Punt") %>%
  filter(special_teams_result != "Non-Special Teams Result")


