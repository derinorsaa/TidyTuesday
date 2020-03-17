library(tidyverse)

office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')

# Preparing-------------------------------------------------------------------------

highest_rating <- office_ratings %>% 
  group_by(season) %>% 
  filter(imdb_rating == max(imdb_rating)) %>% 
  top_n(total_votes, n = 1)

lowest_rating <- office_ratings %>% 
  group_by(season) %>% 
  filter(imdb_rating == min(imdb_rating)) %>% 
  filter(total_votes == min(total_votes))

# Graphing------------------------------------------------------------------------- 

office_ratings %>% 
  ggplot(aes(factor(season), imdb_rating, col = factor(season))) +
  geom_point(aes(size = total_votes), position = position_jitter(width = 0.04), alpha = 0.65) +
  labs(title = "The Office's IMDb Rating", subtitle = "Which Episode Got the Highest and Lowest?", x = "Season", y = "Rating", size = "Total Votes") +
  guides(col = FALSE) +
  geom_text(data = highest_rating, aes(label = title), size = 3, nudge_y = 0.095) +
  geom_text(data = lowest_rating, aes(label = title), size = 3, nudge_y = -0.095) +
  scale_color_brewer(palette = "Paired") +
  ggsave("friends.png")
