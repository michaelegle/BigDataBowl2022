######## Blocking modeling and analysis #######
library(randomForest)
library(tidyverse)
library(viridis)

block <- read_csv("blocks_cluster.csv")

right_block <- block %>%
  filter(punt_land_area == "right") %>%
  select(-(x_depth:punt_land_area))

# This line takes an extremely long time to run, even in command line

print("Starting Model Training")
tictoc::tic()
blocking_rf_right <- randomForest(return_yds ~ ., data = right_block, mtry = 100, importance = T, ntree = 1000)
tictoc::toc()

saveRDS(blocking_rf_right, "blocking_model_right.rds")

print("DONE")


