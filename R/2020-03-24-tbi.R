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


time <- tbi_year %>% 
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
       color = "Outcome:",
       subtitle = "Causes and outcomes of traumatic brain injury (TBI) in the US")

#save(year,
#     file = "data/tbi-ggplot.RData")

# year variable back after new commit
mil <- tbi_military  %>% 
  separate(year,
           into = c("cent", "year"),
           sep = 2) %>% 
  mutate(year = paste0("'", year),
         severity = 
           fct_relevel(severity,
                       "Moderate",
                       "Mild",
                       after = Inf)) %>% 
  ggplot(aes(as.factor(year),
             diagnosed,
             fill = severity)) +
  geom_col(alpha = .8) +
  gameofthrones::scale_fill_got_d(
    direction = -1) +
  theme_minimal(base_family = "serif") +
  theme(axis.text.x = element_text(color="black"), legend.position = "bottom") +
  facet_wrap(~service,
             scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  labs(y = NULL,
       x = NULL,
       fill = "TBI severity",
       subtitle = "TBI diagnoses across Active and Guard/Reserve service members")

# age x cause
props <- tbi_age %>% 
  filter(!age_group %in% c("0-17","Total")) %>% 
  mutate(age_group = 
           fct_relevel(age_group, "0-4", "5-14"),
         injury_mechanism = fct_reorder(injury_mechanism,
                                        -number_est)) %>% 
  ggplot(aes(age_group, number_est)) + 
  geom_col(position = "fill",
           aes(fill = fct_rev(injury_mechanism))) +
  scico::scale_fill_scico_d(direction = -1,
                            labels = label_wrap_gen(30)) +
  scale_y_continuous(breaks = c(1,.2,.4,.6,.8,1),
                     labels = scales::percent,
                     sec.axis = dup_axis()) +
  theme_minimal(base_family = "serif",
                base_size = 12) +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(color="black",
                                   size = 12),
        plot.caption = element_text(face = "italic")) +
  labs(fill = NULL,
       title = "Falls cause the highest relative proportion \nof TBI injuries across most age-groups",
       x = NULL,
       y = NULL,
       caption = "Data: 2014 CDC surveillance report on TBI")

tab <- tbi_age %>% 
  filter(!age_group %in% c("0-17","Total")) %>% 
  mutate(age_group = 
           fct_relevel(age_group, "0-4", "5-14")) %>% 
  group_by(age_group) %>% 
  summarise(count = sum(number_est, na.rm = TRUE)) %>% 
  mutate(count = scales::comma(count)) %>% 
  spread(key = age_group, value = count) %>% 
  rownames_to_column() %>% 
  mutate(rowname = "observations:") %>% 
  rename(" " = rowname)


# save with grob
ggsave("plots/2020-03-24-tbi/agebycause-props2.png", 
       plot = 
         gridExtra::grid.arrange(
           props, 
           gridExtra::tableGrob(tab, rows = NULL,
                                theme = gridExtra::ttheme_minimal()), 
           ncol = 1,
           heights = c(7,.75))
)

