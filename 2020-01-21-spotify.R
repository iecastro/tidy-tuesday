library(tidyverse)
library(factoextra)
library(corrr)
library(parsnip)
library(recipes)

#-- ingest --
spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

#-- EDA -- 
glimpse(spotify_songs)

spotify_songs %>% 
  distinct(track_id) # n = 28,3526

# dupes in data due to songs in multiple playlists
# duped var track_id
janitor::get_dupes(spotify_songs, track_id) # n=7,643

# duped var track_name with diff track_id
spotify_songs %>% 
  distinct(track_id,
           .keep_all = TRUE) %>% 
  janitor::get_dupes(track_name) %>% # ~3K duped track names
  select(1, track_id, 2, 4, 5,7, 
         playlist_name, playlist_genre)

#-- dupeless data set
spotify <- spotify_songs %>% 
  distinct(track_id,
           .keep_all = TRUE) %>%
  group_by(track_name) %>% 
  slice(which.max(track_popularity)) %>% 
  ungroup()

spotify %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  corrplot::corrplot(method = "color",
                     order = "hclust",
                     type = "lower",
                     tl.col = "black", 
                     tl.srt = 45)

scico::scico(3, palette = "roma")

spotify %>% 
  select_if(is.numeric) %>% 
  select(-key, -mode) %>% # weak corr, no path plotted
  correlate() %>% 
  network_plot(colours = 
                 c("#7E1900", "#D1ECC9", "#1A3399"),
               min_cor = .05) 

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


#-- rf with artist factor  
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
                      new_data = data_test)
  )

rf_mod$fit$variable.importance %>% 
  tidy() %>%
  arrange(desc(x)) %>%
  top_n(10) %>% 
  mutate(names = tools::toTitleCase(names)) %>% 
  ggplot(aes(reorder(names, x), x)) +
  geom_col() +
  coord_flip() +
  ggtitle("Variable Importance") +
  theme_minimal() +
  labs(x = "", y = "")

ggplot(results,
       aes(.pred, 
           track_popularity)) +
  geom_point(alpha = .3) +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  xlab("predicted values") +
  ylab("observed values")

# --- subgenre clusters

subgenre_medians <- spotify %>% 
  group_by(playlist_subgenre) %>% 
  summarise_if(is.numeric, median) %>% 
  column_to_rownames(var = "playlist_subgenre")

clust_df <- subgenre_medians %>% scale()

set.seed(3456)

kclust <- kmeans(clust_df, 
                 centers = 4, 
                 nstart = 25)


# ------ FINAL PLOTS -- 
cor_net <- spotify %>% 
  select_if(is.numeric) %>% 
  select(-key, -mode) %>% # weak corr, no path plotted
  correlate() %>% 
  network_plot(colours = 
                 c("#7E1900", "#D1ECC9", "#1A3399"),
               min_cor = .05) +
  theme(legend.position = "bottom") +
  ggtitle("A song's popularity is not related \nto any of its track characteristics")

pop_by_year <- spotify %>% 
  separate(track_album_release_date,
           into = c("year", "month")) %>%
  filter(as.numeric(year) > 2009 &
           as.numeric(year) < 2020) %>% 
  mutate(playlist_genre = 
           str_to_upper(playlist_genre)) %>% 
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
  cowplot::theme_minimal_hgrid(10) +
  facet_wrap(~playlist_genre) +
  theme(axis.text.x = element_text(angle = 45,
                                   size = 8),
        legend.position = "none") +
  scico::scale_color_scico_d(palette = "berlin") +
  labs(x="",
       y = "Track Popularity")

#save(pop_by_year, file = "data/spotify-ggplot.Rdata")

#- models 
importance_plot <- rf_mod$fit$variable.importance %>% 
  tidy() %>%
  arrange(desc(x)) %>%
  top_n(10) %>% 
  mutate(names = tools::toTitleCase(names)) %>% 
  ggplot(aes(reorder(names, x), x)) +
  geom_col(fill = "#112632",
           color = NA,
           alpha = .6) +
  coord_flip() +
  ggtitle("Variable Importance",
          subtitle = "Impurity mode") +
  theme_minimal() +
  labs(x = "", y = "")

rf_plot <- ggplot(results,
                  aes(.pred, 
                      track_popularity)) +
  geom_point(alpha = .3,
             color = "#112632") +
  geom_smooth(se = FALSE,
              method = "loess",
              color = "#B36556") +
  theme_minimal() +
  xlab("predicted values") +
  ylab("observed values") +
  ggtitle("Random Forest regression",
          subtitle = R^2~"(OOB) = 0.63")


clusters <- fviz_cluster(kclust, data = subgenre_medians,
                         main = "Sub-genres",
                         repel = TRUE) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scico::scale_color_scico_d(palette = "roma", direction = -1) +
  scico::scale_fill_scico_d(palette = "roma", direction = -1) +
  labs(subtitle = 
         "clustered by similarities in song characteristics") 


library(patchwork)

(cor_net + clusters)  

#ggsave("plots/2020-01-21-spotify/patch_one.png",
#       plot = last_plot())

pop_by_year / (rf_plot |importance_plot) +
  plot_annotation(title = "The most important predictor of a tracks popularity is the artist",
                  subtitle = "I suppose we all believe the hype")

#ggsave("plots/2020-01-21-spotify/patch_two.png",
#       plot = last_plot())


animate <- pop_by_year +
  transition_states(year) +
  enter_grow()

#save(animate,
#     file = "plots/2020-01-21-spotify/animate.RData")


