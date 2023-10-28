# Load packages
library(readr)
library(dplyr)
library(ggplot2)

# Load data
player_data <- read_csv("NBA_Player_Prop/Data/damian_lilard2.csv")

# Remove rows with NA in G column
player_data <- player_data %>% 
  filter(!is.na(G))

# Calculate statistics (both median and standard deviation)
stats <- player_data %>% 
  summarise(across(c(PTS, AST, STL, TRB, BLK, TOV, PF), list(median = median, sd = sd)))

# Simulate 1000 games
sim_games <- 1000
sim_data <- data.frame(
  PTS = rnorm(sim_games, mean = stats$PTS_median, sd = stats$PTS_sd),
  AST = rnorm(sim_games, mean = stats$AST_median, sd = stats$AST_sd),
  STL = rnorm(sim_games, mean = stats$STL_median, sd = stats$STL_sd),
  TRB = rnorm(sim_games, mean = stats$TRB_median, sd = stats$TRB_sd),
  BLK = rnorm(sim_games, mean = stats$BLK_median, sd = stats$BLK_sd),
  TOV = rnorm(sim_games, mean = stats$TOV_median, sd = stats$TOV_sd),
  PF = rnorm(sim_games, mean = stats$PF_median, sd = stats$PF_sd)
)

# Calculate median of simulations
sim_data_median <- round(summarise(sim_data, across(everything(), median)), 1)

# Show the player simulations
print(sim_data_median)