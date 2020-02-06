library(tidyverse)

attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')

standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')

games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')


merged <- standings %>% 
  filter(year %in% c("2018", "2019")) %>% 
  select(year,
         team,
         team_name,
         wins,
         margin_of_victory,
         simple_rating) %>% 
  left_join(games %>% 
              filter(year %in% c("2018", "2019")) %>% 
              select(year,
                     week,
                     team = home_team_city,
                     team_name = home_team_name),
            by = c("year", "team", "team_name")
            ) %>% 
  left_join(attendance %>% 
              filter(year %in% c("2018", "2019")) %>% 
              select(year,
                     team,
                     team_name,
                     week,
                     weekly_attendance) %>% 
              mutate(week = as.character(week)),
            by = c("year", "team", "team_name", "week")
            )


wins_2018 <- merged %>% 
  filter(year == "2018") %>% 
  select(team,
         team_name,
         wins_2018 = wins,
         srs_2018 = simple_rating) %>% 
  distinct()
  
attend_2019 <- merged %>% 
  filter(year == "2019") %>% 
  mutate(week = parse_number(week)) %>% 
  filter(!is.na(week)) %>% 
  group_by(team_name) %>% 
  slice(which.min(week)) %>% 
  select(team,
         team_name,
         attend_first_home_game =
           weekly_attendance
  ) %>% 
  ungroup() %>% 
  left_join(wins_2018,
            by = c("team", "team_name"))
  
  

#--- 
ggplot(standings, aes(as.factor(year), 
                      margin_of_victory))+
  geom_point(color = "Gray80",
             alpha = .7) + 
  geom_boxplot(fill = NA, 
               outlier.color = NA,
               color = "Gray70",
               alpha = .7) +
  #scico::scale_color_scico_d() +
  scale_color_viridis_d(option = "inferno") +
  guides(color = FALSE) +
  coord_flip() +
  ggrepel::geom_text_repel(aes(as.factor(year),
                               margin_of_victory,
                               label = team_name),
                           data = standings %>% 
                             filter(sb_winner == "Won Superbowl") %>% 
                             group_by(team_name) %>% 
                             slice(which.min(margin_of_victory)),
                           hjust = -.75,
                           nudge_x = .75,
                           nudge_y = 5,
                           direction = "both") +
  geom_point(data = standings %>% 
               filter(sb_winner == "Won Superbowl"),
             aes(color = team_name),
             size = 1.3) +
  theme_minimal() +
  theme(axis.text = element_text(color = "black"),
        panel.background = element_rect(color = "#fffff8"),
        panel.grid.major.y = element_blank()) +
  labs(x = NULL,
       y = "Season margin of victory") +
  ggtitle("The NY Giants have the lowest margins of victory \namong Superbowl winners in the past 20 seasons")

ggsave("plots/2020-02-04-nfl-attendance/margins.jpeg",
       plot = last_plot())

