# Load libraries
library(rvest)
library(dplyr)

# Function to scrape player game log for a given NBA year and player ID
scrape_player_game_log <- function(player_id, year) {
  
  # Construct URL
  url <- paste0("https://www.basketball-reference.com/players/d/", player_id, "/gamelog/", year)
  
  # Read HTML and extract table
  tbl <- url %>%
    read_html() %>%
    html_nodes("table") %>%
    html_table()
  
  # Check if table exists
  if(length(tbl) < 8) {
    stop("Table not found!")
  }
  
  # Convert the table into a data frame and clean it up
  df <- data.frame(tbl[[8]]) %>% 
    filter(!is.na(G) & G != "G")
  
  # Return cleaned data frame
  return(df)
}

# Function to generate a Basketball Reference player ID based on full name
get_player_id <- function(player_full_name) {
  
  # Validate input
  if (length(unlist(strsplit(player_full_name, " "))) < 2) {
    stop("Full name must include both first and last name.")
  }
  
  # Remove all special characters
  player_full_name <- gsub('[^[:alnum:] ]', '', player_full_name)
  
  # Remove accents from characters
  player_full_name <- iconv(player_full_name, to = 'ASCII//TRANSLIT')
  
  # Separate into first and last names and convert to lowercase
  name_parts <- tolower(unlist(strsplit(player_full_name, " ")))
  first_name <- name_parts[1]
  last_name <- name_parts[2]
  
  # Create player id using first 5 characters of last name and first 2 of first name
  player_id <- paste0(substr(last_name, 1, 5), substr(first_name, 1, 2), '01')
  
  # Return the generated player id
  return(player_id)
}

# Enter the players Full Name
player_id <- get_player_id('Jalen Brunson')

# Show the player ID
# player_id

# Generate player game log. change the year to get different years
player_game_log <- scrape_player_game_log(player_id, 2024)

# Remove rows with NA, blank, or 'G' in G column and convert specified columns to numeric
player_game_log <- player_game_log %>%
  filter(!is.na(G) & G != "" & G != "G") %>%
  #select(c(PTS, AST, STL, TRB, BLK, TOV, PF)) %>%
  mutate(across(c(PTS, AST, STL, TRB, BLK, TOV, PF), as.numeric))


# Calculate statistics (both median and standard deviation)
stats <- player_game_log %>% 
  summarise(across(c(PTS, AST, STL, TRB, BLK, TOV, PF), list(median = median, sd = sd)))

# Simulate 1000 games
sim_games <- 1000
sim_data <- data.frame(
  PTS = pmax(0, rnorm(sim_games, mean = stats$PTS_median, sd = stats$PTS_sd)),
  AST = pmax(0, rnorm(sim_games, mean = stats$AST_median, sd = stats$AST_sd)),
  STL = pmax(0, rnorm(sim_games, mean = stats$STL_median, sd = stats$STL_sd)),
  TRB = pmax(0, rnorm(sim_games, mean = stats$TRB_median, sd = stats$TRB_sd)),
  BLK = pmax(0, rnorm(sim_games, mean = stats$BLK_median, sd = stats$BLK_sd)),
  TOV = pmax(0, rnorm(sim_games, mean = stats$TOV_median, sd = stats$TOV_sd)),
  PF = pmax(0, rnorm(sim_games, mean = stats$PF_median, sd = stats$PF_sd))
)

# Calculate median of simulations
sim_data_median <- round(summarise(sim_data, across(everything(), median)), 1)

# Show the player simulations
print(sim_data_median)