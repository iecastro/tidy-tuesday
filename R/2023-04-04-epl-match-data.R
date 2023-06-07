library(tidyverse)

soccer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-04/soccer21-22.csv')

soccer %>% 
  select(Referee, HR, AR) %>% 
  pivot_longer(c(HR:AR),
               names_to = "cards",
               values_to = "n") %>% 
  group_by(Referee) %>% 
  summarise(reds = sum(n)) %>% 
  arrange(desc(reds))


soccer %>% 
  select(team = HomeTeam, HY) %>% 
  pivot_longer(HY,
               names_to = "cards",
               values_to = "n") %>% 
  bind_rows(
    soccer %>% 
      select(team = AwayTeam, AY) %>% 
      pivot_longer(AY,
                   names_to = "cards",
                   values_to = "n")
  ) %>% 
  group_by(team) %>% 
  summarise(yellows = sum(n)) %>% 
  arrange(desc(yellows))

soccer %>% 
  select(shots_on_target = HST, goals = FTHG) %>% 
  bind_rows(
    soccer %>% 
      select(shots_on_target = AST, goals = FTAG)
  ) %>% 
  filter(shots_on_target > 0) %>% 
  group_by(shots_on_target, goals) %>% tally() %>% 
  ggplot(aes(shots_on_target, goals)) + 
  geom_tile(aes(fill = n)) + 
  scale_x_continuous(breaks = c(1,3,5,7,9,11,13,15)) +
  scale_y_continuous(breaks = c(0,2,4,6,8), limits = c(-0.5,9)) +
  theme_classic() +
  labs(fill = "# of games",
       x = "Shots on target", y = "Goals scored",
       title = "Premier League 21/22",
       caption = "EPL match data via Evan Gower | github.com/rfordatascience/tidytuesday") +
  theme(legend.position = "top",
        legend.justification = "left",
        axis.text = element_text(color = "black"),
        axis.title.x = element_text(hjust = 0, face = "italic"),
        axis.title.y = element_text(hjust = 1, face = "italic")) +
  annotate("text", x = 14, y = 8.2, 
           label = "Man City 7\nLeeds     0") +
  annotate("point", x = 15, y = 7) +
  geom_segment(aes(x = 14.2, xend = 14.9, 
                   y = 8, yend = 7.2),
               arrow = arrow(length = unit(0.2, "inches"))) + 
  geom_segment(aes(x = 15, xend = 15, 
                   y = 4.5, yend = 5.8),
               arrow = arrow(length = unit(0.2, "inches"))) +
  annotate("point", x = 15, y = 6) +
  annotate("text", x = 15, y = 4.2, 
           label = "Liverpool 6\nLeeds     0") +
  scico::scale_fill_scico(palette = "bamako", direction = -1) 

ggsave("plots/2023-04-04-epl/shots_on_target.png",
       plot = last_plot(),
       dpi = 320, width = 12,
       height = 8, units = "in")


# junk drawer
soccer %>% filter(HST == max(HST) | AST == max(AST)) %>% 
  select(HomeTeam, AwayTeam,HST, AST, FTHG, FTAG)

soccer %>% filter((HST == 9 | AST == 9) &
                   (FTHG == 6 | FTAG == 6)) %>% 
  select(HomeTeam, AwayTeam,HST, AST, FTHG, FTAG)

# HomeTeam AwayTeam    HST   AST  FTHG  FTAG
# Man City Leicester     9     8     6     3

