# Tidy Tuesday link for this found below
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-04-07/readme.md


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)


# Data Import -------------------------------------------------------------

tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')
summary(tdf_winners)


# Variable Creation -------------------------------------------------------

tdf_winners <- tdf_winners %>% 
  mutate(
    race_year = year(start_date),
    race_month = month(start_date)
  )


# Analysis & Visualization ------------------------------------------------


# Have winners of the TDF gotten younger over time?
# No clear pattern here
tdf_winners %>% 
  ggplot(aes(race_year, age, color = edition)) +
  geom_point() +
  xlab('Race Start Year') +
  ylab('Age of Winner') +
  ggtitle("Tour De France Winner's Age Over Time") +
  theme(legend.position = 'none')

# Where are most of the TDF winners from?
# By far mostly from France
tdf_winners %>% 
  group_by(birth_country) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x = reorder(birth_country, n), n)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = n)) +
  coord_flip() +
  xlab('Country of Birth') + 
  ylab('Count') +
  ggtitle("Tour De France Winners' Birthplace")

# Do winners now win more or less stages?
# No obvious themes here
tdf_winners %>% 
  ggplot(aes(race_year, stage_wins, color = edition)) +
  geom_point()

table(tdf_winners$winner_team)

# Who is the winningest team?
# France is. 
# The USPS had a team?? Yes, was Lance Armstrong's team.
tdf_winners %>% 
  group_by(winner_team) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  slice(1:10) %>% 
  ggplot(aes(reorder(winner_team, n), n)) +
  geom_bar(stat = 'identity') +
  geom_label(aes(label = n)) +
  coord_flip() +
  xlab("Team Name") +
  ylab("Count Wins") +
  ggtitle("Top 10 Winningest Teams") +
  scale_y_continuous(breaks = c(0:13))

# How has distance of the race changed over time?
# Interestingly the TDF has gotten shorter over time. How has that corresponded with rider speed?
tdf_winners %>% 
  ggplot() +
  geom_point(aes(race_year, distance, color = edition)) +
  xlab("Year") +
  ylab("Distance (KM)") +
  ggtitle("Distance of the TDF over time") +
  theme(legend.position = 'none')

# The races may be shorter but they are MUCH faster
tdf_winners %>% 
  mutate(
    avg_speed = distance / time_overall 
  ) %>% 
  ggplot() +
  geom_point(aes(race_year, avg_speed, color = edition)) +
  xlab("Year") +
  ylab("Avg KM / H") +
  ggtitle("Average Speed of TDF Winners Over Time") +
  theme(legend.position = 'none')

# Are riders smaller than they used to be?
tdf_winners %>%  # no clear pattern
  ggplot() +
  geom_point(aes(race_year, height))

tdf_winners %>%  # No clear patter
  ggplot() +
  geom_point(aes(race_year, weight))

tdf_winners %>%  # Maybe a pattern here - smaller BMI over time?
  ggplot() +
  geom_point(aes(race_year, weight / height, color = edition))

# Has the competitiveness of the race increased or decreased over time?
# The tour has become much more competitive over time with margin of victory shrinking from nearly three hours to 8 seconds
tdf_winners %>% 
  ggplot() +
  geom_point(aes(race_year, time_margin, color = edition)) +
  xlab("Year") +
  ylab("Winning")

