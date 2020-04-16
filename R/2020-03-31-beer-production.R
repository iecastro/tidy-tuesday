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
                                  face = "italic"),
        axis.title.x = element_text(hjust = 1,
                                    size = 12,
                                    face = "bold"),
        axis.ticks.y = element_line(color = "black")) +
  labs(x = "Barrels produced",
       y = NULL)

#-- state bins --
states %>% 
  filter(type == "Bottles and Cans" &
           state != "total") %>% 
  count(state, wt = barrels, name = "total") %>% 
  mutate(total = total / 1000000) %>% 
  statebins::statebins(value_col = "total",
                       round = TRUE) +
  theme_void(base_family = "serif",
             base_size = 12) +
  gameofthrones::scale_fill_got(labels = scales::comma,
                                direction = -1) +
  labs(fill = "Barrels \nproduced \n(in millions)",
       title = "State level production of beer \nin cans and bottles (2008-2019)")


#-- junk drawer -

#-- taxed --
taxed %>% 
  filter(year >= 2018 & 
           type == "In bottles and cans" & 
           month == 12) %>%  # cannot be linked to state production
  select(year, type, ytd_current, tax_rate) 

#-- materials 
mat_all <- materials %>% 
  filter(material_type %in% 
           c("Grain Products", 
             "Non-Grain Products") & 
           month == 12 &
           !is.na(ytd_current)) %>% 
  select(year, material_type,type,  ytd_current) %>% 
  mutate(year = as.factor(year))

mat_all  %>% 
  ggplot(aes(year, ytd_current)) +
  geom_point(aes(color = type)) +
  geom_line(aes(color = type,
                group = type)) +
  facet_wrap(~material_type,
             nrow = 2,
             scales = "free_y") +
  scale_y_log10(labels = scales::comma) +
  ggrepel::geom_text_repel(
    data = mat_all %>% 
      filter(year == 2014),
    aes(year, ytd_current,
        label = type),
    nudge_y = - 0.5, 
    direction = "x") +
  labs(y = "pounds of material used",
       x = NULL) +
  theme_minimal(base_family = "serif") +
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank())


size %>% 
  filter(brewer_size == "Total") %>% 
  ggplot(aes(as.factor(year),
             n_of_brewers)) +
  geom_point(aes(color = total_barrels,
                 size = total_barrels)) +
  scico::scale_color_scico(labels = scales::comma) +
  theme_minimal()
  

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




