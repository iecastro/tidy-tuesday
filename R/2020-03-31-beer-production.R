library(tidyverse)

#-- ingest --
materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')


#-- 
order <- states %>% 
  filter(year == 2019 &
           type == "Bottles and Cans" &
           state != "total") %>% 
  arrange(barrels) %>% 
  pull(state)

states %>% 
  filter(year %in% 
           c(2008, 2019) &
           state != "total") %>% 
  mutate(year = as.factor(year),
         state = fct_relevel(state,
                             order)) %>% 
  ggplot(aes(barrels, state)) +
  geom_line(aes(group = state),
            color = "#F49E71") +
  geom_point(aes(color = year)) +
  scale_x_log10(labels = scales::comma) +
  scale_color_manual(values = 
                       c("2019" = "#001959",
                         "2008" = "#F49E71")) +
  facet_wrap(~type) +
  theme_minimal(base_family = "serif") +
  theme(axis.text.y = element_text(size = 7.5),
        axis.text = element_text(color = "black"),
        legend.justification= c(0, 1),
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(size = 12,
                                  face = "italic")) +
  labs(x = NULL,
       y = NULL)



#-- junk drawer --
materials %>% 
  glimpse()

materials %>% distinct(type)

mat2017 <- materials %>% 
  filter(material_type %in% 
           c("Grain Products", "Non-Grain Products") &
           year == 2017)

mat2017 %>% 
  mutate(month = lubridate::month(month, label = TRUE)) %>% 
  ggplot(aes(month, month_current)) +
  geom_point(aes(color = type)) +
  geom_line(aes(color = type,
                group = type)) +
  facet_wrap(~material_type)


# pounds used during Christmas season
mat2017 %>% 
  filter(month == 12) %>% 
  ggplot(aes(reorder(type, month_current),
             month_current)) +
  geom_point() +
  geom_segment(aes(x = type, xend = type,
                   y = 0, yend = month_current)) +
  scale_y_log10(labels = scales::comma) +
  coord_flip()




