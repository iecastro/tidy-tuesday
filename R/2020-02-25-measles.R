library(tidyverse)
library(tigris)
library(sf)

options(tigris_class = "sf")

#-- Ingest -- 
measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')

glimpse(measles)

measles %>%  # as far as I can tell duplicates are 
  janitor::get_dupes(county, name) # present due to diff in lat/long

measles <- measles %>% 
  distinct(county, name,
           .keep_all = TRUE) 
#---
wsj_measles <- readr::read_csv("https://raw.githubusercontent.com/WSJ/measles-data/master/all-measles-rates.csv")

glimpse(wsj_measles)

wsj_measles %>%  # as far as I can tell duplicates are 
  janitor::get_dupes(county, name) # present due to diff in lat/long



ny_mmr <- readr::read_csv("https://raw.githubusercontent.com/WSJ/measles-data/master/individual-states/newYork.csv")

glimpse(ny_mmr)

counties <- tigris::counties("NY", cb = TRUE)

mmr_sf <- ny_mmr %>% 
  filter(!is.na(lat) &
           !lng > 0) %>% 
  sf::st_as_sf(coords = c("lng", "lat")) %>% 
  st_set_crs(st_crs(counties))


ggplot() + 
  geom_sf(data = counties,
          fill = "gray80",
          color = "#FFFFFF",
          size = .75) +
  geom_sf(data = mmr_sf, 
          aes(color = mmr),
          alpha = .6,
          size = 1.3) +
  theme_void() +
  scico::scale_color_scico(palette = "bamako",
                           direction = 1) 

# measles herd immunity needs rate of 96% or higher
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5544919/
# diff threshold of 93-95%
#https://www.npr.org/sections/health-shots/2019/04/30/718820350/how-did-we-get-here-7-things-to-know-about-measles

mmr_sf %>% #n 702
  filter(mmr < 96)

ggplot() +
  geom_sf(data = counties,
          fill = "gray80",
          color = "#FFFFFF",
          size = .75) +
  stat_density2d(data = 
                   st_coordinates(
                     mmr_sf %>% #n 702
                       filter(mmr <= 95.9)) %>% 
                   as_tibble(),
                 aes(X, Y,
                     fill = ..level..), 
                 alpha = .6,
                 geom = "polygon") +
  theme_minimal() +
  scico::scale_fill_scico()


mmr_rates <- mmr_sf %>% 
  mutate(rate = 
           case_when(
             mmr >= 96 ~ "96 or more",
             mmr >= 93 &
               mmr < 96 ~ "93 to 95.9",
             mmr < 93 ~ "less than 93"
           ),
         rate = fct_inorder(rate),
         type = ifelse(type != "BOCES",
                       paste0(type, " Schools"),
                       type))

#-- final plot --

ggplot() + 
  geom_sf(data = counties,
          fill = "gray90",
          color = "#FFFFFF",
          size = .75) +
  geom_sf(data = mmr_rates,
          fill = NA,
          aes(color = rate),
          alpha = .6,
          size = 1,
          show.legend = "point") +
  theme_minimal(base_family = "serif",
             base_size = 14) +
  theme(axis.text = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        panel.grid = element_blank()) +
  gameofthrones::scale_color_got_d(direction = -1) +
  guides(color = guide_legend(
    title = "% of students vaccinated (2017-18):",
    override.aes = list(size = 3,
                        alpha = 1))) +
  labs(title = "To maintain herd immunity from Measles,",
       subtitle = "vaccination rates need to be roughly 96%",
       caption = "data source: github.com/WSJ/measles-data") +
  facet_wrap(~type)

#ggsave("plots/2020-02-25-measles/ny-rates.png", plot = last_plot())

#================ junk drawer ======================
list_states <- mmr_sf %>% 
  st_set_geometry(NULL) %>% 
  distinct(state) %>% 
  pull() %>% 
  left_join()

states_geo <- tigris::states(cb = TRUE) %>% 
  filter(!NAME %in% 
           c("American Samoa",
             "Puerto Rico",
             "Alaska", "Hawaii",
             "United States Virgin Islands",
             "District of Columbia",
             "Commonwealth of the Northern Mariana Islands"))

measles_sf <- measles %>%  
  filter(!is.na(lat) &
           !lng > 0) %>% 
  sf::st_as_sf(coords = c("lng", "lat")) %>% 
  st_set_crs(st_crs(states_geo))



ggplot() + 
  geom_sf(data = states_geo,
          fill = "gray80",
          color = "#FFFFFF",
          size = .75) +
  geom_sf(data = measles_sf, 
          aes(color = mmr < 96),
          alpha = .4,
          size = 1.3) +
  theme_void() +
  scico::scale_color_scico_d(palette = "bamako",
                             direction = -1)

states_geo <- tidycensus::state_laea

states_dat_geo <- states_geo %>% 
  filter()

ggplot() +
  geom_sf(data = states,
          fill = "lightblue", 
          color = "#FFFFFF") +
  #geom_sf(data = mmr_sf)
  stat_density2d(data = st_coordinates(
    mmr_sf) %>% 
      as_tibble(),
    aes(X, Y,
        fill = ..level..), 
    alpha = .4,
    geom = "polygon") +
  theme_minimal() +
  scico::scale_fill_scico()



split_geo <- mmr_sf %>% 
  split(.$state) %>% 
  map(~ ggplot() +
  stat_density2d(data = st_coordinates(
    .) %>% 
      as_tibble(),
    aes(X, Y,
        fill = ..level..), 
    alpha = .4,
    geom = "polygon") +
  theme_minimal() +
  scico::scale_fill_scico()
  )
  
