library(bit64)
library(bit)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(readxl)
library(MASS)
library(nortest)

file_path <- '/Users/shreyamishra/Desktop/IPL_ball_by_ball_updated till 2024.csv'
salary_file_path <- '/Users/shreyamishra/Desktop/IPL SALARIES 2024.xlsx'
ipl_data <- read.csv(file_path)
str(ipl_data)
head(ipl_data)
correct_season <- function(season) {
  if (grepl("/", season)) {
    years <- unlist(strsplit(season, "/"))
    return(paste0("20", years[2]))
  }
  return(season)
}

ipl_data$Season <- sapply(ipl_data$Season, correct_season)
season_corrections <- c("8" = "2008", "10" = "2010", "21" = "2020")
ipl_data$Season <- ifelse(ipl_data$Season %in% names(season_corrections),
                          season_corrections[ipl_data$Season],
                          ipl_data$Season)
unique(ipl_data$Season)
ipl_data$Date <- dmy(ipl_data$Date)
ipl_data$Season <- as.character(ipl_data$Season)

duplicates <- duplicated(ipl_data)
cat("Number of duplicate rows:", sum(duplicates), "\n")
ipl_data <- ipl_data[!duplicates, ]
team_name_corrections <- c(
  "Delhi Daredevils" = "Delhi Capitals",
  "Rising Pune Supergiant" = "Rising Pune Supergiants",
  "Gujarat Lions" = "Gujarat Titans",
  "Pune Warriors" = "Rising Pune Supergiants",
  "Deccan Chargers" = "Sunrisers Hyderabad",
  "Royal Challengers Bangalore" = "Royal Challengers Bengaluru",
  "Kings XI Punjab" = "Punjab Kings"
)

ipl_data$`Batting.team` <- ifelse(ipl_data$`Batting.team` %in% names(team_name_corrections),
                                  team_name_corrections[ipl_data$`Batting.team`],
                                  ipl_data$`Batting.team`)

ipl_data$`Bowling.team` <- ifelse(ipl_data$`Bowling.team` %in% names(team_name_corrections),
                                  team_name_corrections[ipl_data$`Bowling.team`],
                                  ipl_data$`Bowling.team`)

ipl_data <- ipl_data[!is.na(ipl_data$`Match.id`) & !is.na(ipl_data$Date), ]


str(ipl_data)
head(ipl_data)
batting_teams <- unique(ipl_data$`Batting.team`)
bowling_teams <- unique(ipl_data$`Bowling.team`)
all_teams <- unique(c(batting_teams, bowling_teams))
all_teams_sorted <- sort(all_teams)
print(all_teams_sorted)

batting_teams_updated <- unique(ipl_data$`Batting.team`)
bowling_teams_updated <- unique(ipl_data$`Bowling.team`)

all_teams_updated <- unique(c(batting_teams_updated, bowling_teams_updated))
all_teams_sorted_updated <- sort(all_teams_updated)
print(all_teams_sorted_updated)

# Display the top 30 rows of the cleaned data
top_30_rows <- head(ipl_data, 30)
print(top_30_rows)

# Group data by 'Match.id' to represent each round
round_wise_data <- ipl_data %>%
  group_by(`Match.id`)

# Aggregate runs scored by each batsman per match
batsman_aggregate <- ipl_data %>%
  group_by(`Match.id`, Striker) %>%
  summarise(runs = sum(runs_scored))

# Aggregate wickets taken by each bowler per match
bowler_aggregate <- ipl_data %>%
  filter(wicket_confirmation == 1) %>%
  group_by(`Match.id`, Bowler) %>%
  summarise(wickets = sum(wicket_confirmation))

# Function to get top 3 performers in each round
top_performers <- function(df, group_col, value_col, top_n = 3) {
  df %>%
    arrange(desc(!!sym(value_col))) %>%
    group_by(!!sym(group_col)) %>%
    slice_max(order_by = !!sym(value_col), n = top_n)
}

# Get top 3 run-scorer in each match
top_run_getters <- top_performers(batsman_aggregate, 'Match.id', 'runs')

# Get top 3 wicket-takers in each match
top_wicket_takers <- top_performers(bowler_aggregate, 'Match.id', 'wickets')

# Display the results
cat("Top 3 Run-Getters in Each Match:\n")
print(top_run_getters)

cat("\nTop 3 Wicket-Takers in Each Match:\n")
print(top_wicket_takers)

# Aggregate runs scored by each batsman per season
batsman_aggregate_season <- ipl_data %>%
  group_by(Season, Striker) %>%
  summarise(runs = sum(runs_scored))

# Aggregate wickets taken by each bowler per season
bowler_aggregate_season <- ipl_data %>%
  filter(wicket_confirmation == 1) %>%
  group_by(Season, Bowler) %>%
  summarise(wickets = sum(wicket_confirmation))

top_performers_season <- function(df, group_col, value_col, top_n = 3) {
  df %>%
    arrange(desc(!!sym(value_col))) %>%
    group_by(!!sym(group_col)) %>%
    slice_max(order_by = !!sym(value_col), n = top_n)
}

top_run_getters_season <- top_performers_season(batsman_aggregate_season, 'Season', 'runs')

# Get top 3 wicket-takers in each season
top_wicket_takers_season <- top_performers_season(bowler_aggregate_season, 'Season', 'wickets')

# Organize and display the results
cat("Top 3 Run-Getters in Each Season:\n")
seasons <- unique(top_run_getters_season$Season)
for (season in seasons) {
  cat("\nSeason:", season, "\n")
  season_data <- filter(top_run_getters_season, Season == season)
  for (i in 1:nrow(season_data)) {
    cat(paste("Batsman:", season_data$Striker[i], ", Runs:", season_data$runs[i], "\n"))
  }
}

cat("\nTop 3 Wicket-Takers in Each Season:\n")
for (season in seasons) {
  cat("\nSeason:", season, "\n")
  season_data <- filter(top_wicket_takers_season, Season == season)
  for (i in 1:nrow(season_data)) {
    cat(paste("Bowler:", season_data$Bowler[i], ", Wickets:", season_data$wickets[i], "\n"))
  }
}

# Ensure data is sorted by Season in descending order and by runs/wickets in descending order
top_run_getters_season <- top_run_getters_season %>%
arrange(desc(Season), desc(runs))

top_wicket_takers_season <- top_wicket_takers_season %>%
arrange(desc(Season), desc(wickets))
print(top_run_getters_season)
print(top_wicket_takers_season)

# Assuming 'seasons' is defined as a vector of seasons to iterate over
seasons <- c(2024, 2023, 2022)

library(dplyr)

for (season in seasons) {
  cat("\nSeason:", season, "\n")
  
  cat("Top 3 Run-Getters:\n")
  season_data_batsmen <- head(filter(top_run_getters_season, Season == season), 3)
  for (i in 1:nrow(season_data_batsmen)) {
    team <- filter(ipl_data, Season == season & Striker == season_data_batsmen$Striker[i])$`Batting.team`[1]
    if (!is.na(team)) {
      cat(paste(i, ". Batsman:", season_data_batsmen$Striker[i], "(", team, "), Runs:", season_data_batsmen$runs[i], "\n"))
    } else {
      cat(paste(i, ". Batsman:", season_data_batsmen$Striker[i], ", Runs:", season_data_batsmen$runs[i], " (Team data not available)\n"))
    }
  }
  
  cat("\nTop 3 Wicket-Takers:\n")
  season_data_bowlers <- head(filter(top_wicket_takers_season, Season == season), 3)
  for (i in 1:nrow(season_data_bowlers)) {
    team <- filter(ipl_data, Season == season & Bowler == season_data_bowlers$Bowler[i])$`Bowling.team`[1]
    if (!is.na(team)) {
      cat(paste(i, ". Bowler:", season_data_bowlers$Bowler[i], "(", team, "), Wickets:", season_data_bowlers$wickets[i], "\n"))
    } else {
      cat(paste(i, ". Bowler:", season_data_bowlers$Bowler[i], ", Wickets:", season_data_bowlers$wickets[i], " (Team data not available)\n"))
    }
  }
}



# Filter data for the last three IPL seasons
last_three_seasons <- c('2024', '2023', '2022')
top_batsmen_last_three_seasons <- filter(top_run_getters_season, Season %in% last_three_seasons)
top_bowlers_last_three_seasons <- filter(top_wicket_takers_season, Season %in% last_three_seasons)


# Function to remove outliers using IQR method
remove_outliers <- function(data) {
  Q1 <- quantile(data, 0.25)
  Q3 <- quantile(data, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  data_filtered <- data[data >= lower_bound & data <= upper_bound]
  return(data_filtered)
}

# Function to fit and plot normal distribution
fit_and_plot_normal <- function(data, title, xlabel) {
  if (length(data) == 0) {
    cat(paste("No data available for", title, "\n"))
    return(NULL)
  }
  
  mu <- mean(data)
  std <- sd(data)
  
  # Adjust margins and create a new plot window
  par(mar = c(5, 4, 4, 2) + 0.1)
  quartz(width = 10, height = 6)
  
  # Plot histogram
  hist(data, breaks = 10, freq = FALSE, col = 'thistle', main = title, xlab = xlabel, ylab = 'Density')
  
  # Add normal distribution curve
  curve(dnorm(x, mean = mu, sd = std), col = 'palevioletred', lwd = 2, add = TRUE)
  
  cat(paste(title, "\nFit results: mu =", round(mu, 2), ", std =", round(std, 2), "\n"))
}
# Remove outliers and fit normal distribution for runs scored by top batsmen in the last three seasons
runs_data <- remove_outliers(top_batsmen_last_three_seasons$runs)
fit_and_plot_normal(runs_data, 'Normal Distribution of Runs Scored by Top Batsmen (Last 3 IPL Seasons)', 'Runs')

# Remove outliers and fit normal distribution for wickets taken by top bowlers in the last three seasons
wickets_data <- remove_outliers(top_bowlers_last_three_seasons$wickets)
fit_and_plot_normal(wickets_data, 'Normal Distribution of Wickets Taken by Top Bowlers (Last 3 IPL Seasons)', 'Wickets')


# Function to apply Box-Cox transformation
boxcox_transform <- function(data) {
  data_positive <- data + 1 # Adding 1 to avoid negative values
  
  # Perform Box-Cox transformation
  transformed_data <- MASS::boxcox(data_positive)
  
  return(transformed_data)
}

# Function to apply Box-Cox transformation
boxcox_transform <- function(data) {
  data_positive <- data + 1 # Adding 1 to avoid negative values
  
  # Perform Box-Cox transformation
  bc <- MASS::boxcox(data_positive ~ 1, plotit = FALSE)
  lambda <- bc$x[which.max(bc$y)]
  transformed_data <- ((data_positive^lambda - 1) / lambda)
  
  return(transformed_data)
}

# Apply Box-Cox transformation and fit normal distribution for runs scored by top batsmen in the last three seasons
runs_data_bc <- boxcox_transform(runs_data)
fit_and_plot_normal(runs_data_bc, 'Box-Cox Normal Distribution of Runs Scored by Top Batsmen (Last 3 IPL Seasons)', 'Box-Cox(Runs)')

# Apply Box-Cox transformation and fit normal distribution for wickets taken by top bowlers in the last three seasons
wickets_data_bc <- boxcox_transform(wickets_data)
fit_and_plot_normal(wickets_data_bc, 'Box-Cox Normal Distribution of Wickets Taken by Top Bowlers (Last 3 IPL Seasons)', 'Box-Cox(Wickets)')

# Function to perform and print results of normality tests
normality_tests <- function(data, data_label) {
  cat(paste("\nNormality tests for", data_label, ":\n"))
  
  # Shapiro-Wilk Test
  shapiro_test <- shapiro.test(data)
  cat(paste("Shapiro-Wilk Test: Statistics =", round(shapiro_test$statistic, 3), ", p-value =", round(shapiro_test$p.value, 3), "\n"))
  
  # Kolmogorov-Smirnov Test
  ks_test <- ks.test(data, "pnorm", mean(data), sd(data))
  cat(paste("Kolmogorov-Smirnov Test: Statistics =", round(ks_test$statistic, 3), ", p-value =", round(ks_test$p.value, 3), "\n"))
  
  # Anderson-Darling Test
  ad_test <- ad.test(data)
  cat(paste("Anderson-Darling Test: Statistics =", round(ad_test$statistic, 3), "\n"))
}

# Extract and print critical values
ad_critical_values <- c(0.576, 0.656, 0.787, 0.918, 1.092) # Standard critical values for Anderson-Darling test
ad_levels <- c(15, 10, 5, 2.5, 1) # Corresponding significance levels

for (i in seq_along(ad_levels)) {
  sig_level <- ad_levels[i]
  crit_val <- ad_critical_values[i]
  cat(paste("At", sig_level, "% significance level: critical value =", round(crit_val, 3), "\n"))
}

# Perform normality tests for runs scored by top batsmen in the last three seasons
normality_tests(runs_data_bc, 'Box-Cox Transformed Runs Scored by Top Batsmen')

# Perform normality tests for wickets taken by top bowlers in the last three seasons
normality_tests(wickets_data_bc, 'Box-Cox Transformed Wickets Taken by Top Bowlers')

library(readxl)
library(dplyr)

# Load the salary data
salary_data <- read_excel('/Users/shreyamishra/Desktop/IPL SALARIES 2024.xlsx')
head(salary_data)

# Convert salary to actual figures
salary_data$Salary <- salary_data$Rs * 100000 # 1 lakh = 100000

# Filter the IPL data for "K Rabada"
K_Rabada_data <- ipl_data %>%
  filter(Striker == "K Rabada" | Bowler == "K Rabada")

# Replace "K Rabada" with "Kagiso Rabada" in the filtered data
K_Rabada_data$Striker <- gsub("K Rabada", "Kagiso Rabada", K_Rabada_data$Striker)
K_Rabada_data$Bowler <- gsub("K Rabada", "Kagiso Rabada", K_Rabada_data$Bowler)

# Merge the filtered and renamed IPL data with the salary data
merged_data <- left_join(K_Rabada_data, salary_data, by = c("Striker" = "Player"))

# Calculate total runs scored by "Kagiso Rabada" in 2023
total_runs_2023 <- sum(merged_data %>% filter(Season == "2023" & Striker == "Kagiso Rabada") %>% pull(runs_scored), na.rm = TRUE)

# Calculate total wickets taken by "Kagiso Rabada" in 2023
total_wickets_2023 <- sum(merged_data %>% filter(Season == "2023" & Bowler == "Kagiso Rabada") %>% pull(wicket_confirmation), na.rm = TRUE)

# Add a new column for performance in 2023 in the salary data
salary_data <- salary_data %>%
  mutate(Performance_2023_Runs = ifelse(Player == "Kagiso Rabada", total_runs_2023, NA),
         Performance_2023_Wickets = ifelse(Player == "Kagiso Rabada", total_wickets_2023, NA))

# Display the updated salary data
head(salary_data)
















