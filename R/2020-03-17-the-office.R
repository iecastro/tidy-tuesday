library(tidyverse)
library(panelr)
library(tidytext)

office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')

glimpse(office_ratings)

office <- panel_data(office_ratings,
           id = season,
           wave = episode) %>% 
  mutate(season_avg = mean(imdb_rating),
         episodes = n(),
         season_votes = mean(total_votes),
         week_day = lubridate::wday(air_date,
                                    label = TRUE)) %>% 
  unpanel() %>% 
  mutate(season = paste0("S-", season))

glimpse(office)

ggplot(office,
       aes(air_date, imdb_rating,
           color = week_day)) +
  geom_point(size = 1) +
  geom_line() +
  geom_hline(aes(yintercept = season_avg),
             lty = "dashed") +
  scale_x_date(date_labels = "%b '%y",
               name = NULL) +
  theme_minimal(base_family = "serif") +
  theme(panel.grid.major = element_blank(),
        axis.text = element_text(
          color = "black"),
        axis.title.y = element_text(hjust = 1,
                                    face = "bold.italic")) +
  facet_wrap(~season,
             scales = "free_x") +
  gameofthrones::scale_color_got_d(
    option = "lannister",
    direction = -1,
    name = NULL) +
  labs(y = "IMDB episode rating")
       

ggplot(office,
       aes(episode, imdb_rating)) +
  geom_point() + 
  facet_wrap(~season, 
             scales = "free_x")


office %>% 
  select(season, season_avg, season_votes) %>% 
  distinct() %>% 
  ggplot(aes(season, season_votes)) +
  geom_point(aes(color = season_avg),
             size = 2) +
  geom_segment(aes(x = season, xend = season,
                   y = 0, yend = season_votes,
                   color = season_avg)) +
  gameofthrones::scale_color_got(option = "lannister",
                                 name = "Average \nrating") +
  theme_minimal(base_family = "serif") +
  theme(panel.grid.major.x = element_blank(),
        axis.text = element_text(
          color = "black"),
        axis.title.y = element_text(hjust = 1,
                                    face = "bold.italic")) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Avg. # of votes",
       x = NULL)

#==== text ====

titles <- office %>% 
  group_by(season) %>% 
  unnest_tokens(word, title,
                drop = FALSE) %>% 
  anti_join(stop_words) %>% 
  left_join(get_sentiments("bing"))

