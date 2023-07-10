library(tidyverse)
library(patchwork)

soccer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-04/soccer21-22.csv')

epl22 <- readr::read_csv("data/epl_results_2022-23.csv")

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

shots <- soccer %>% 
  bind_rows(epl22) %>% 
  select(shots_on_target = HST, goals = FTHG) %>% 
  bind_rows(
    soccer %>%
      bind_rows(epl22) %>% 
      select(shots_on_target = AST, goals = FTAG)
  ) %>% 
  filter(shots_on_target > 0)

p1 <- 
  shots %>% 
  group_by(shots_on_target, goals) %>% tally() %>% 
  ggplot(aes(shots_on_target, goals)) + 
  geom_tile(aes(fill = n)) + 
  scale_x_continuous(breaks = c(1,3,5,7,9,11,13,15)) +
  scale_y_continuous(breaks = c(0,2,4,6,8), limits = c(-0.5,9)) +
  theme_classic() +
  labs(fill = "# of games",
       x = "Shots on target", y = "Goals scored",
       title = "Premier League 21/22 & 22/23",
       caption = "EPL match data via Evan Gower | github.com/rfordatascience/tidytuesday | visual: @iecastro") +
  theme(legend.position = "top",
        legend.justification = "left",
        axis.text = element_text(color = "black"),
        axis.title.x = element_text(hjust = 0, face = "italic"),
        axis.title.y = element_text(hjust = 1, face = "italic")) +
  annotate("text", x = 14, y = 8.6, 
           label = "Man City 7 (ST 15)\nLeeds     0 (ST 3)") +
  annotate("point", x = 15, y = 7) +
  geom_segment(aes(x = 14.2, xend = 14.9, 
                   y = 8, yend = 7.2),
               arrow = arrow(length = unit(0.2, "inches"))) + 
  geom_segment(aes(x = 15, xend = 15, 
                   y = 4.5, yend = 5.8),
               arrow = arrow(length = unit(0.2, "inches"))) +
  annotate("point", x = 15, y = 6) +
  annotate("text", x = 14, y = 4.1, 
           label = "Liverpool 6 (ST 15)\nLeeds     0 (ST 2)") + 
  annotate("point", x = 8, y = 7) +
  annotate("point", x = 4, y = 0) +
  geom_segment(aes(x = 4, xend = 4, 
                   y = 6.3, yend = .4),
                 arrow = arrow(length = unit(0.2, "inches"))) +
  geom_segment(aes(x = 4.7, xend = 7.8, 
                   y = 7, yend = 7),
               arrow = arrow(length = unit(0.2, "inches"))) +
  annotate("text", x = 3.3, y = 6.8, 
           label = "Liverpool 7 (ST 8)\nMan United 0 (ST 4)") + 
  scico::scale_fill_scico(palette = "bamako", direction = -1)


p2 <- ggplot(data = shots %>% filter(goals < 6),
       aes(shots_on_target, goals)) +
  geom_point(position = 
             position_jitter(width = .5,
                             height = .5),
           alpha = .4) +
  geom_smooth() +
  theme_minimal() +
  labs(fill = "# of games",
       x = "Shots on target", y = "Goals scored") +
  theme(legend.position = "top",
        legend.justification = "left",
        axis.text = element_text(color = "black"),
        axis.title.x = element_text(hjust = 0, face = "italic"),
        axis.title.y = element_text(hjust = 1, face = "italic"))


p1+p2 + plot_layout(widths = c(3, 2))


# ggsave("plots/2023-04-04-epl/shots_on_target.png",
#        plot = last_plot(),
#        dpi = 320, width = 12,
#        height = 8, units = "in")

ggsave("plots/2023-04-04-epl/shots_on_target_patch.png",
       plot = last_plot(),
       dpi = 320, width = 15,
       height = 8, units = "in")

d <- soccer %>% 
  filter(HomeTeam == "Man United" |
           AwayTeam == "Man United") %>% 
  mutate(game = 
           if_else(HomeTeam == "Man United", "Home", "Away"),
         half_time = 
           case_when(HomeTeam == "Man United" &
                       HTR == "H" ~ 1,
                     AwayTeam == "Man United" &
                       HTR == "A" ~ 1,
                     HTR == "D" ~ 0,
                     TRUE ~ -1),
         full_time = 
           case_when(HomeTeam == "Man United" &
                       FTR == "H" ~ 1,
                     AwayTeam == "Man United" &
                       FTR == "A" ~ 1,
                     FTR == "D" ~ 0,
                     TRUE ~ -1),
         opponent = if_else(HomeTeam == "Man United", 
                            AwayTeam, HomeTeam),
         # conceded goals
         shots_on_target_diff = if_else(HomeTeam == "Man United",
                                        HST - AST,
                                        AST - HST)
  )

d22 <- epl22 %>% 
  filter(HomeTeam == "Man United" |
           AwayTeam == "Man United") %>% 
  mutate(game = 
           if_else(HomeTeam == "Man United", "Home", "Away"),
         half_time = 
           case_when(HomeTeam == "Man United" &
                       HTR == "H" ~ 1,
                     AwayTeam == "Man United" &
                       HTR == "A" ~ 1,
                     HTR == "D" ~ 0,
                     TRUE ~ -1),
         full_time = 
           case_when(HomeTeam == "Man United" &
                       FTR == "H" ~ 1,
                     AwayTeam == "Man United" &
                       FTR == "A" ~ 1,
                     FTR == "D" ~ 0,
                     TRUE ~ -1),
         opponent = if_else(HomeTeam == "Man United", 
                            AwayTeam, HomeTeam),
         shots_on_target_diff = if_else(HomeTeam == "Man United",
                                        HST - AST,
                                        AST - HST)
  )



plot_change <- function(.data){
  d <- .data
  
  ggplot(d) + 
    geom_point(aes(full_time, 
                   opponent,
                   color = shots_on_target_diff > 0, 
                   size = shots_on_target_diff)) +
    # win change at full time
    geom_segment(data = d %>% filter(full_time > half_time),
                 aes(x = half_time, 
                     xend = full_time - .075,
                     y = opponent, yend = opponent),
                 arrow = arrow(length = unit(0.02, "npc"),
                               type = "closed")) +
    # loss change at full time
    geom_segment(data = d %>% filter(full_time < half_time),
                 aes(x = half_time, 
                     xend = full_time + .075,
                     y = opponent, yend = opponent),
                 arrow = arrow(length = unit(0.02, "npc"),
                               type = "closed")) +
    facet_wrap(~game, scales = "free") +
    scale_x_continuous(breaks = c(-1,0,1),
                       labels = c("Loss", "Draw","Win")) +
    scale_color_manual(values =
                         c('FALSE' = "Gray40",
                           'TRUE' = "firebrick"),
                       guide = "none") +
    theme_minimal() +
    scale_size_area(max_size = 4, 
                    guide = 
                      guide_legend(
                        override.aes = 
                          list(color = c("Gray40", "Gray40", 
                                         "Firebrick", "Firebrick")))) + 
    theme(legend.position = "top",
          legend.justification = "left",
          axis.text = element_text(color = "black"))
}

united21 <- plot_change(d) +
  labs(x = NULL, y = NULL,
       title = "Manchester United 21/22 EPL season",
       size = "United's FT shots on goal relative to opponents",
       subtitle = 
         expression("Change in points dropped" %<-% "or"  %->%  "gained from half time to full time"))


united22 <- plot_change(d22) +
  labs(x = NULL, y = NULL,
       title = "Manchester United 22/23 EPL season",
       subtitle = 
         expression("Points dropped" %<-% "or"  %->%  "gained from half time to full time"),
       size = "United's FT shots on goal relative to opponents",
       caption = "data source: kaggle.com/datasets/evangower/premier-league-2022-2023\nvisual: @iecastro")


united22
ggsave("plots/2023-04-04-epl/half-to-fulltime.png",
       plot = last_plot(),
       dpi = 320, width = 12,
       height = 8, units = "in")


united21 / united22

# ggsave("plots/2023-04-04-epl/half-to-fulltime2.png",
#        plot = last_plot(),
#        dpi = 320, width = 12,
#        height = 10, units = "in")


united_shots <- d %>% 
  mutate(season = "2021-22") %>% 
  bind_rows(d22 %>% select(-Time) %>% 
              mutate(season = "2022-23")) %>% 
  mutate(st =  if_else(HomeTeam == "Man United", HST, AST),
         goals =  if_else(HomeTeam == "Man United", FTHG, FTAG)) %>% 
  group_by(season, game) %>% 
  summarise(goals = sum(goals),
            st_total = sum(st),
            st_avg = mean(st),
            st_dif = mean(shots_on_target_diff))

