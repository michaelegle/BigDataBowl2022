######## Blocking modeling and analysis #######
library(randomForest)
library(tidyverse)
library(viridis)

block <- read_csv("blocks_cluster.csv")

left_block <- block %>%
  filter(punt_land_area == "left") %>%
  select(-(x_depth:punt_land_area))

# This line takes an extremely long time to run, even in command line

print("Starting Model Training")
tictoc::tic()
blocking_rf_left <- randomForest(return_yds ~ ., data = left_block, mtry = 100, importance = T, ntree = 1000)
tictoc::toc()

saveRDS(blocking_rf_left, "blocking_model_left.rds")

print("DONE")



