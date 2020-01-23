library(tidyverse)
library(factoextra)
library(parsnip)
library(recipes)

#-- ingest --
spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')


glimpse(spotify_songs)

spotify_songs %>% 
  distinct(track_id) # n = 28,3526

janitor::get_dupes(spotify_songs, track_id) # n=7,643
# dupes due to songs in multiple playlists

#-- dupeless data set
spotify <- spotify_songs %>% 
  distinct(track_id,
           .keep_all = TRUE) %>% 
  distinct(track_name,
           .keep_all = TRUE)

spotify %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  corrplot::corrplot(method = "color",
                     order = "hclust",
                     type = "lower",
                     tl.col = "black", 
                     tl.srt = 45)


spotify %>% 
  ggplot(aes(reorder(playlist_genre, 
           track_popularity),
           track_popularity)) + 
  geom_jitter(position = 
                position_jitter(width = .3,
                                seed = 123),
              alpha = .6) +
  geom_boxplot(fill = NA) +
  theme_minimal()

spotify %>% 
  separate(track_album_release_date,
           into = c("year", "month")) %>%
  filter(as.numeric(year) > 2009 &
           as.numeric(year) < 2020) %>% 
  ggplot(aes(year,
             track_popularity)) + 
  geom_jitter(position = 
                position_jitter(
                  width = .15,
                  seed = 123,
                ),
              alpha = 1/10,
              aes(color = playlist_genre )) +
  geom_boxplot(fill = NA,
               aes(color = playlist_genre)) +
  cowplot::theme_minimal_hgrid() +
  facet_wrap(~playlist_genre) +
  theme(axis.text.x = element_text(angle = 45,
                                   size = 8),
        legend.position = "none") +
  scico::scale_color_scico_d(palette = "berlin") +
  labs(x="",
       y = "Track Popularity")


spotify %>% 
  ggplot(aes(energy, 
             track_popularity)) +
  geom_point(alpha = .4) +
  geom_smooth() +
  facet_wrap(~playlist_genre) +
  theme_minimal()


#--- random forest ---
#----- data split
set.seed(7564)

model_data <- spotify %>% 
  select(-ends_with("id"),
         -ends_with("name"),
         -track_album_release_date) %>% 
  filter(!is.na(track_artist)) # n=1
  
recipe <- recipe(track_popularity ~ .,
                 data = model_data) %>% 
  step_center(all_numeric()) %>% 
  step_dummy(playlist_genre,
             playlist_subgenre,
             one_hot = TRUE) %>% 
  step_string2factor(track_artist) %>% 
  prep()
  
recipe$template
  
data_split <- 
  rsample::initial_split(recipe$template,
                         strata = "track_popularity", 
                         p = 0.75)

data_train <- rsample::training(data_split)
data_test  <- rsample::testing(data_split)


#---- model 
# random forest
rf_mod <- rand_forest(mode = "regression",
                      mtry = 12,
                      trees = 200) %>% 
  set_engine("ranger",
             seed = 5462,
             importance = "impurity",
             respect.unordered.factors = TRUE) %>% 
  fit(track_popularity ~ .,
      data = data_train)

#---- predictions
results <- data_test %>%
  select(track_popularity) %>%
  bind_cols(
    predict.model_fit(rf_mod,
                      new_data = expense_test)
  )

rf_mod$fit$variable.importance %>% 
  tidy() %>%
  arrange(desc(x)) %>%
  ggplot(aes(reorder(names, x), x)) +
  geom_col() +
  coord_flip() +
  ggtitle("Variable Importance") +
  theme_minimal() +
  labs(x = "", y = "")

ggplot(results,
       aes(.pred, 
           track_popularity)) +
  geom_point(alpha = .4) +
  #scale_y_log10() +
  #scale_x_log10() +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  xlab("predicted values (log10)") +
  ylab("observed values (log10)")
