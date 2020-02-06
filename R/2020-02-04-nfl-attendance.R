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
        panel.background = element_rect(fill = "#fffff8",
                                        color = "#fffff8"),
        panel.grid.major.y = element_blank()) +
  labs(x = NULL,
       y = "Season margin of victory") +
  ggtitle("The NY Giants have the lowest margins of victory \namong Superbowl winners in the past 20 seasons")

#ggsave("plots/2020-02-04-nfl-attendance/margins.jpeg",
#       plot = last_plot())


sb_winners <- standings %>% 
  filter(sb_winner == "Won Superbowl") %>% 
  distinct(team_name) %>% 
  pull()


merged_att <- attendance %>% 
  select(team, team_name,
         year, home, away,
         total) %>% 
  distinct() %>% 
  left_join(
    standings %>% 
      select(team, team_name, year, sb_winner),
    by = c("team", "team_name", "year")
  )


merged_att %>% 
  filter(team_name %in%
           sb_winners) %>% 
  separate(year, into = 
             c("cent", "year"),
           sep = 2) %>% 
  mutate(year = paste0("'", year),
         year = as.factor(year)) %>% 
  ggplot(aes(year, away)) +
  geom_point(aes(color = sb_winner)) +
  geom_segment(aes(x = year, xend = year,
                   y = 0, yend = away,
                   color = sb_winner)) +
  facet_wrap(~team_name,
             ncol = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8,
                                   face = "bold.italic"),
        axis.text = element_text(color = "black"),
        panel.grid = element_blank(),
        legend.position = "top",
        axis.title = element_text(hjust = 1,
                                  face = "bold.italic"),
        axis.line.x.bottom = element_line()) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = 
                       c("No Superbowl" = "Gray80",
                         "Won Superbowl" = "#001959")) +
  labs(color = "",
       y = "Away attendance",
       x = "")


#ggsave("plots/2020-02-04-nfl-attendance/away_attend.jpeg",
#       plot = last_plot())




