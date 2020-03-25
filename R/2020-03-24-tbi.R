library(tidyverse)

#-- ingest
tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')

tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')

tbi_military <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')


tbi_age %>% 
  filter(age_group %in% c("0-17", "0-4")) %>% 
  ggplot(aes(reorder(injury_mechanism, number_est),
             number_est)) +
  geom_col(aes(fill = age_group)) +
  coord_flip()


tbi_military %>% 
  ggplot(aes(as.factor(year),
             diagnosed,
             fill = fct_reorder(severity, diagnosed)
  )) +
  geom_col(alpha = .8) +
  gameofthrones::scale_fill_got_d(direction = -1) +
  theme_minimal()


tbi_year %>% 
  separate(year,
           into = c("cent", "year"),
           sep = 2) %>% 
  mutate(year = paste0("'", year),
         type = fct_inorder(type),
         injury_mechanism = 
           fct_relevel(injury_mechanism,
                       "Unintentional falls",
                       after = 0L),
         injury_mechanism =
           fct_relevel(injury_mechanism,
                       "Intentional self-harm",
                       after = Inf)) %>% 
  filter(injury_mechanism != "Total" ) %>% 
  ggplot(aes(year, number_est,
             group = type,
             color = type
  )) +
  geom_point() + 
  geom_line() +
  facet_wrap(~injury_mechanism,
             scales = "free_y",
             labeller = labeller(
               injury_mechanism = label_wrap_gen(30))) +
  scico::scale_color_scico_d() +
  theme_minimal(base_family = "serif",
                base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "bottom") +
  scale_y_continuous(labels = scales::comma,
                     name = NULL) +
  labs(x = NULL,
       color = NULL,
       title = "Traumatic brain injury (TBI) outcomes in the US",
       subtitle = "by cause of injury")

#save(year,
#     file = "data/tbi-ggplot.RData")

# lost year variable after new commit
tbi_military %>% 
  group_by(service, severity) %>% 
  summarise(total = sum(diagnosed, na.rm = TRUE)) %>% 
  ggplot(aes(severity,
             total,
             fill = fct_reorder(service, -total))) +
  geom_col(alpha = .8,
           position = "dodge") +
  gameofthrones::scale_fill_got_d(
    option = "targaryen2",
    direction = -1) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# age x cause
tbi_age %>% 
  filter(!age_group %in% c("0-17","Total")) %>% 
  mutate(age_group = 
           fct_relevel(age_group, "0-4", "5-14"),
         injury_mechanism = fct_reorder(injury_mechanism,
                                        -number_est)) %>% 
  ggplot(aes(age_group, number_est)) + 
  geom_col(position = "fill",
           aes(fill = fct_rev(injury_mechanism)),
           alpha = .8) +
  scico::scale_fill_scico_d(direction = -1,
                            labels = label_wrap_gen(30)) +
  scale_y_continuous(labels = scales::percent,
                     sec.axis = dup_axis()) +
  theme_minimal(base_family = "serif") +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(color="black")) +
  labs(fill = NULL,
       title = "Causes of TBI by age-group",
       x = NULL,
       y = NULL)



