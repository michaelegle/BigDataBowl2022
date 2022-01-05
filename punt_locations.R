####### Punt directions #######

# Same code from the cleaning.R file
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

tictoc::tic()

plays <- left_join(plays, games, by = "game_id") %>%
  mutate(defense_team = ifelse(possession_team == home_team_abbr,
                               visitor_team_abbr,
                               home_team_abbr))

######## add the play-level data #######

data18 <- data18 %>%
  left_join(plays %>%
              select(game_id, play_id, possession_team, yardline_side, yardline_number,
                     home_team_abbr, visitor_team_abbr, special_teams_play_type, special_teams_result),
            by = c("game_id", "play_id"))

data19 <- data19 %>%
  left_join(plays %>%
              select(game_id, play_id, possession_team, yardline_side, yardline_number,
                     home_team_abbr, visitor_team_abbr, special_teams_play_type, special_teams_result),
            by = c("game_id", "play_id"))

data20 <- data20 %>%
  left_join(plays %>%
              select(game_id, play_id, possession_team, yardline_side, yardline_number,
                     home_team_abbr, visitor_team_abbr, special_teams_play_type, special_teams_result),
            by = c("game_id", "play_id"))

data18 <- data18 %>%
  filter(special_teams_play_type == "Punt") %>%
  filter(special_teams_result != "Non-Special Teams Result")

data19 <- data19 %>%
  filter(special_teams_play_type == "Punt") %>%
  filter(special_teams_result != "Non-Special Teams Result")

data20 <- data20 %>%
  filter(special_teams_play_type == "Punt") %>%
  filter(special_teams_result != "Non-Special Teams Result")

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

print("Data Cleaned")

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


data18 <- data18 %>%
  filter(display_name == "football")

data19 <- data19 %>%
  filter(display_name == "football")

data20 <- data20 %>%
  filter(display_name == "football")

test <- data18 %>%
  head()

punt_departure_events <- c("punt", "autoevent_kickoff")
punt_arrival_events <- c("fair_catch", "punt_received", "punt_muffed", "kick_received", "punt_land",
                         "punt_downed")

data18 <- data18 %>%
  group_by(game_id, play_id) %>%
  mutate(departure_frame = min(frame_id[event %in% punt_departure_events]),
         arrival_frame = min(frame_id[event %in% punt_arrival_events]),
         ball_in_air = frame_id > departure_frame & frame_id < arrival_frame) %>%
  ungroup()

data18 <- data18 %>%
  filter(arrival_frame != Inf, departure_frame != Inf)

data19 <- data19 %>%
  group_by(game_id, play_id) %>%
  mutate(departure_frame = min(frame_id[event %in% punt_departure_events]),
         arrival_frame = min(frame_id[event %in% punt_arrival_events]),
         ball_in_air = frame_id > departure_frame & frame_id < arrival_frame) %>%
  ungroup()

data19 <- data19 %>%
  filter(arrival_frame != Inf, departure_frame != Inf)

data20 <- data20 %>%
  group_by(game_id, play_id) %>%
  mutate(departure_frame = min(frame_id[event %in% punt_departure_events]),
         arrival_frame = min(frame_id[event %in% punt_arrival_events]),
         ball_in_air = frame_id > departure_frame & frame_id < arrival_frame) %>%
  ungroup()

data20 <- data20 %>%
  filter(arrival_frame != Inf, departure_frame != Inf)

data18 <- data18 %>%
  filter(frame_id == arrival_frame)



data19 <- data19 %>%
  filter(frame_id == arrival_frame)

data20 <- data20 %>%
  filter(frame_id == arrival_frame)


punt_arrivals <- data18 %>%
  bind_rows(data19) %>%
  bind_rows(data20)

punt_arrivals <- punt_arrivals %>%
  filter(display_name == "football")

test <- punt_arrivals %>%
  head()

mean(punt_arrivals$x_depth) # Average punt goes 44 yards downfield, seems like the 40 yards deep on the grid
# makes sense

punt_arrivals <- punt_arrivals %>%
  mutate(punt_land_area = case_when(y <= 160/9 ~ "left",
                                    y > 160/9 & y <= 320/9 ~ "center",
                                    T ~ "right"))


punt_arrivals <- punt_arrivals %>%
  filter(y < 160/3, y > 0, x < 110)

punt_arrival_locations <- punt_arrivals %>%
  select(game_id, play_id, x_depth, y)

write_csv(punt_arrival_locations, "punt_locs.csv")

punt_arrivals %>%
  ggplot(aes(x = x_depth, y = y)) +
  geom_density_2d_filled() +
  coord_fixed() +
  geom_hline(yintercept = c(160/9, 320/9), linetype = "dotted")


punt_arrival_locations %>%
  inner_join(punt_arrivals %>% select(game_id, play_id, x_depth, y)) %>%
  ggplot(aes(x = x_depth, y = y, color = punt_land_area)) + 
  geom_point()
