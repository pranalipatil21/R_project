# Load required libraries
install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

# Load your dataset
ipl_data <- read.csv("C:/Users/patil/Downloads/IPL.csv",header=TRUE)

# View dataset structure
View(ipl_data)
str(ipl_data)
summary(ipl_data)

#1. Who are the top 10 players in terms of 'Player of the Match' awards?
#barplot
top_players <- ipl_data %>% count(player_of_match, sort = TRUE) %>% head(10)
ggplot(top_players, aes(x = reorder(player_of_match, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Top 10 Players with 'Player of the Match' Awards", x = "Player", y = "Number of Awards")
  
#2 . Is there a correlation between winning the toss and winning the match?
ggplot(ipl_data, aes(x = toss_winner == winner)) +
  geom_bar(fill = "orange") +
  labs(title = "Correlation between Toss Win and Match Win", x = "Toss Winner = Match Winner", y = "Frequency")
 #bar_plot

#3.Which team played maximum super overs?
super_over_teams <- ipl_data %>% 
  filter(super_over == "Y") %>%
  count(team1, team2, sort = TRUE) %>%
  head(1)

print(super_over_teams)

#4.Venues with maximum and minimum scores?

max_score_venue <- ipl_data %>%
  group_by(venue) %>%
  summarize(max_score = max(target_runs, na.rm = TRUE)) %>%
  arrange(desc(max_score)) %>%
  head(1)

min_score_venue <- ipl_data %>%
  group_by(venue) %>%
  summarize(min_score = min(target_runs, na.rm = TRUE)) %>%
  arrange(min_score) %>%
  head(1)

print(max_score_venue)
print(min_score_venue)

#5.Most popular venues for matches?
popular_venues <- ipl_data %>%
  count(venue, sort = TRUE) %>%
  head(5)

ggplot(popular_venues, aes(x = reorder(venue, n), y = n)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Top 5 Popular Venues", x = "Venue", y = "Number of Matches") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 6. How many wins did each team achieve across different seasons?
ggplot(ipl_data, aes(x = season, fill = winner)) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Win Distribution by Season", x = "Season", y = "Number of Wins")


  
#7. What is the relationship between target runs and result margin?
ggplot(ipl_data, aes(x = target_runs, y = result_margin)) +
  geom_point(color = "blue") +
  labs(title = "Relationship between Target Runs and Result Margin", x = "Target Runs", y = "Result Margin")

#8. How do total runs vary seasonally across the dataset?
seasonal_runs <- ipl_data %>% group_by(season) %>% summarize(total_runs = sum(target_runs, na.rm = TRUE))
ggplot(seasonal_runs, aes(x = season, y = total_runs, group = 1)) +
  geom_line(color = "red") +
  labs(title = "Seasonal Variation in Total Runs", x = "Season", y = "Total Runs")

# 9. How does team performance change over different seasons?
ggplot(ipl_data, aes(x = season, fill = winner)) +
  geom_bar() +
  labs(title = "Team Performance Across Seasons", x = "Season", y = "Number of Wins")

#10.wineer of each season?
# Load required libraries
library(dplyr)

# Find the winner of each season
season_winners <- ipl_data %>%
  filter(!is.na(winner)) %>%         # Remove rows with NA winners
  group_by(season, winner) %>%       # Group by season and winner
  tally() %>%                        # Count the number of wins per team per season
  arrange(season, desc(n)) %>%       # Arrange by season and descending wins
  slice_max(n, n = 1, with_ties = FALSE) # Select the team with the most wins in each season

# Display the result
print(season_winners)

# 11. Are there any notable trends in the winning margin by team?
ggplot(ipl_data, aes(x = winner, y = result_margin)) +
  geom_boxplot(fill = "lightgreen") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Win Margin by Team", x = "Team", y = "Win Margin")


# 12. What is the relationship between toss winner and match winner by season?
ggplot(ipl_data, aes(x = season, fill = toss_winner == winner)) +
  geom_bar(position = "dodge") +
  labs(title = "Relationship between Toss Winner and Match Winner by Season", x = "Season", y = "Frequency")

# 13. Which team had the highest average win margin over all seasons?
avg_win_margin <- ipl_data %>% group_by(winner) %>% summarize(avg_margin = mean(result_margin, na.rm = TRUE))
ggplot(avg_win_margin, aes(x = reorder(winner, avg_margin), y = avg_margin)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Average Win Margin by Team", x = "Team", y = "Average Win Margin")

# 14. What is the average win margin by match type?
ggplot(ipl_data, aes(x = match_type, y = result_margin)) +
  geom_bar(stat = "summary", fun = "mean", fill = "violet") +
  labs(title = "Average Win Margin by Match Type", x = "Match Type", y = "Average Win Margin")
 
#15. Top 5 cities with the most matches
top_cities <- ipl_data %>%
  count(city, sort = TRUE) %>%
  head(5)

# Rename columns if needed
colnames(top_cities) <- c("city", "matches_count")

# Display the result
top_cities


# Plotting the top 5 cities with most matches
# Load required libraries
library(ggplot2)

# Plotting the top 5 cities with the most matches in R
ggplot(top_cities, aes(x = city, y = matches_count)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Top 5 Cities with Most IPL Matches", x = "City", y = "Number of Matches") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

