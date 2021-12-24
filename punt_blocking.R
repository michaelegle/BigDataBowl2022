####### Punt Blocking #######

library(tidyverse)

data18 <- read_csv("punts18.csv")
data19 <- read_csv("punts19.csv")
data20 <- read_csv("punts20.csv")

tictoc::tic()

get_opponent_separation <- function(play_data_df) {
  
  # data frame of only defense players
  def_team_df <- play_data_df %>% 
    filter(on_defense == T) %>% 
    select(
      # play description columns
      game_id, play_id, time, possession_team, defense_team,
      # frame/ event cols
      frame_id, event, 
      # player columns
      nfl_id, display_name, position, team_abbr, x, y, s, a, dis, o, dir, x_end, y_end,
      # other columns
      dx, dy, x_los, x_depth
    )
  
  # data frame of only offense players
  off_team_df <- 
    play_data_df %>% 
    filter(on_offense == T) %>% 
    select(
      # frame
      game_id, play_id, frame_id,
      # opposing player columns
      nfl_id_opp = nfl_id, opp_player_name = display_name, opp_pos = position, team_opp = team_abbr, 
      x_opp = x, y_opp = y, s_opp = s, a_opp = a, dis_opp = dis, o_opp = o, dir_opp = dir,
      x_end_opp = x_end, y_end_opp = y_end
    )
  
  # join data on frame id, calculate distance defensive player is from each offensive player
  distance_to_opp_df <- 
    def_team_df %>% left_join(off_team_df, by = c("game_id", "play_id","frame_id")) %>% 
    mutate(distance_from_opp = sqrt((x - x_opp)^2 + (y - y_opp)^2),
           distance_from_opp_end = sqrt((x_end - x_end_opp)^2 + (y_end - y_end_opp)^2)) 
  
  return(distance_to_opp_df)
}

retrieve_blocking <- function(play_data_df)
{
  play_data_df <- play_data_df %>%
    get_opponent_separation()
  
  play_data_df <- play_data_df %>%
    group_by(game_id, play_id, frame_id, nfl_id) %>%
    mutate(nearest_opp_dist = min(distance_from_opp)) %>%
    ungroup() %>%
    filter(distance_from_opp == nearest_opp_dist) %>%
    select(-nearest_opp_dist)
  
  play_data_df <- play_data_df %>%
    mutate(is_blocking = case_when(distance_from_opp < 1 & distance_from_opp_end < distance_from_opp ~ T,
                                   T ~ F))
  
  return(play_data_df)
}

data18 <- data18 %>%
  retrieve_blocking()

print("Done with 2018 Blocking")

data19 <- data19 %>%
  retrieve_blocking()

print("Done with 2019 Blocking")

data20 <- data20 %>%
  retrieve_blocking()

print("Done with 2020 blocking")

tictoc::toc()

write_csv(data18, "punts18.csv")
print("Done writing 2018 CSV")
write_csv(data19, "punts19.csv")
print("Done writing 2019 CSV")
write_csv(data20, "punts20.csv")
print("Done writing 2020 CSV")


