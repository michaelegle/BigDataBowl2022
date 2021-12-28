####### Create block grids #######

library(tidyverse)

data18 <- read_csv("punts18.csv")
data19 <- read_csv("punts19.csv")
data20 <- read_csv("punts20.csv")
plays <- read_csv("data/plays.csv")
plays <- plays %>%
  janitor::clean_names()

punt_plays <- plays %>%
  filter(special_teams_play_type == "Punt")

punt_plays <- punt_plays %>%
  filter(special_teams_result %in% c("Muffed", "Return", "Fair Catch"))

###### Update data ######

data18 <- data18 %>%
  inner_join(punt_plays)

data18 <- data18 %>%
  mutate(x = round(x_depth),
         y = round(y))

data19 <- data19 %>%
  inner_join(punt_plays)

data19 <- data19 %>%
  mutate(x = round(x_depth),
         y = round(y))

data20 <- data20 %>%
  inner_join(punt_plays)

data20 <- data20 %>%
  mutate(x = round(x_depth),
         y = round(y))

punt_departure_events <- c("punt", "autoevent_kickoff")
punt_arrival_events <- c("fair_catch", "punt_received", "punt_muffed", "kick_received", "punt_land",
                         "punt_downed")

##### Find points where the ball is in the air #######

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

game_id_list <- punt_plays$game_id
play_id_list <- punt_plays$play_id
return_yd_list <- punt_plays$kick_return_yardage

####### Combine all data sets #######

data <- data18 %>%
  bind_rows(data19) %>%
  bind_rows(data20)

####### Finally, create the blocking grid #######

all_grids <- data.frame()

print("Beginning loop")

tictoc::tic()

for (i in 1:length(game_id_list))
{
  this_game_id <- game_id_list[i]
  this_play_id <- play_id_list[i]
  this_return_yds <- return_yd_list[i]
  
  this_play <- data %>%
    filter(game_id == this_game_id, play_id == this_play_id)
  
  field_grid <- expand.grid(x = 0:39, y = 1:53)
  
  block_locs <- this_play %>%
    filter(x >= 0, x <= 39, y >= 1, y <= 53) %>%
    filter(is_blocking) %>%
    filter(ball_in_air) %>%
    select(x, y, is_blocking)
  
  field_grid <- field_grid %>%
    left_join(block_locs) %>%
    mutate(block = ifelse(is_blocking, 1, 0),
           block = ifelse(is.na(is_blocking), 0, block))
  
  field_grid <- field_grid %>%
    group_by(x, y) %>%
    summarize(block = max(block)) %>%
    ungroup()
  
  field_grid_pivot <- field_grid %>%
    pivot_wider(values_from = block, names_from = c(x, y))
  
  field_grid_pivot <- field_grid_pivot %>%
    mutate(game_id = this_game_id,
           play_id = this_play_id,
           return_yds = this_return_yds)
  
  all_grids <- all_grids %>%
    bind_rows(field_grid_pivot)
}

tictoc::toc()

write_csv(all_grids, "blocking_model_data.csv")

