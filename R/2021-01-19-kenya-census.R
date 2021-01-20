library(tidyverse)
library(sf)


load(url("https://github.com/Shelmith-Kariuki/rKenyaCensus/raw/master/data/KenyaCounties_SHP.rda"))


kenya <- KenyaCounties_SHP %>% 
  st_as_sf()

crops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/crops.csv')

households <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/households.csv')

k <- crops %>% 
  filter(SubCounty == "KENYA") %>% 
  select(-SubCounty, -Farming) %>% 
  gather() %>% 
  arrange(desc(value))

crops <- crops %>% filter(SubCounty != "KENYA")

geocrops <- crops %>% 
  mutate(County = 
           ifelse(SubCounty == "NAIROBI", 
                  "NAIROBI CITY", SubCounty)) %>% 
  tigris::geo_join(kenya, ., 
                   by_sp = "County", 
                   by_df = "County") %>% 
  mutate_all(replace_na, 0)

#-- generate dots 
# reference  
# https://www.cultureofinsight.com/post/multivariate-dot-density-maps-in-r-with-sf-ggplot2

random_round <- function(x) {
  v=as.integer(x)
  r=x-v
  test=runif(length(r), 0.0, 1.0)
  add=rep(as.integer(0),length(r))
  add[r>test] <- as.integer(1)
  value=v+add
  ifelse(is.na(value) | value<0,0,value)
  return(value)
}

# data frame of number of dots to plot 
num_dots <- as.data.frame(geocrops) %>% 
  select(Avocado, Mango, Coffee, Tea, Macadamia, Citrus) %>% 
  #select(Tea:`Khat (Miraa)`) %>% 
  mutate_all(list(~(. / 2500))) %>%
  mutate_all(random_round)

# generates data frame with coordinates for each point 
sf_dots <- map_df(names(num_dots), 
                  ~ st_sample(geocrops, 
                              size = num_dots[,.x], 
                              type = "random") %>% 
                    # generate the points in each polygon
                    st_cast("POINT") %>%  # cast the geom set as 'POINT' data
                    st_coordinates() %>%  # pull out coordinates into a matrix
                    as_tibble() %>%       # convert to tibble
                    setNames(c("lon","lat")) %>% # set column names
                    mutate(crop = .x)  # add categorical variable
) %>% 
  slice(sample(1:n())) # once map_df binds rows randomise order to avoid bias in plotting order


p <- ggplot() +
  geom_sf(data = geocrops,
          color = "#FFFFFF", fill = "gray80") +
  geom_point(data = sf_dots, 
             aes(lon, lat,
                 color = crop),
             size = .5, alpha = .6)  +
  theme_void(24, base_family = "mono") +
  labs(caption = "1 dot = 2,500 households \nDots are spatially random within a county \nand do not represent actual household locations",
       title = "Distribution of households \nfarming Kenya's top crops") +
  facet_wrap(~crop, nrow = 2) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  theme(legend.position = "none")

ggsave("plots/2021-01-19-kenya-census/crops-map.png", 
       plot = p, 
       dpi = 320, width = 15, 
       height = 10, units = "in")
