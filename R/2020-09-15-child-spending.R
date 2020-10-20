library(tidyverse)
library(patchwork)

# ingest 

kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')

glimpse(kids)

kids %>% 
  filter(variable == "pubhealth") %>% 
  ggplot(aes(as.factor(year),
             inf_adj_perchild)) +
   geom_line(aes(group = state)) +
  facet_wrap(~state)


#--- statbins
ph2016 <- kids %>% 
  filter(variable == "pubhealth" & 
           year == 2016)


binmap <- ph2016 %>% 
  mutate(spent = inf_adj_perchild*1000) %>% 
  statebins::statebins(value_col = "spent",
                       round = TRUE,
                       dark_label = "white",
                       light_label = "gray30") +
  theme_void(base_size = 12) +
  scico::scale_fill_scico(palette = "lapaz",
                          direction = -1,
                          label = scales::dollar,
                          name = "") +
  theme(legend.position = "bottom",
        legend.key.width = unit(.5, "in"))

#--- top v bottom 5
st <- kids %>% 
  filter(variable == "pubhealth" &
           year == 2016) %>% 
  mutate(best_5 = min_rank(desc(inf_adj_perchild)),
         worst_5 = min_rank(inf_adj_perchild)) %>% 
  filter(best_5 <= 5 |
           worst_5 <= 5) %>% 
  mutate(spending = case_when(
    best_5 <= 5 ~ "Top 5",
    worst_5 <= 5 ~ "Bottom 5"
  )) %>% 
  select(state, spending)

st %>% 
  left_join(kids %>% 
              filter(variable == "pubhealth"),
            by = "state") %>% 
  ggplot(aes(year, inf_adj_perchild, 
             group = state, color = state)) +
  geom_line(size = 1) +
  theme_minimal(12, base_family = "serif") +
  scale_color_viridis_d()  +
  facet_wrap(~spending)


#--- slopegraph
ranks <- kids %>% 
  group_by(year) %>% 
  filter(variable == "pubhealth" &
           year %in% c(1997, 2016)) %>% 
  mutate(rank = case_when(
    year == 1997 ~ min_rank(desc(inf_adj_perchild)),
    year == 2016 ~ min_rank(desc(inf_adj_perchild))
  )) %>% 
  select(state, year, rank) %>% 
  mutate(year = paste0("rank", year)) %>% 
  spread(year, rank) %>% 
  arrange(rank1997) %>% 
  mutate(diff = rank1997 - rank2016,
         diff = case_when(
           diff < -10 ~ "worsen", 
           diff > 10 ~ "improved",
           TRUE ~ "nochange"
         ))


states <- bind_cols(enframe(state.abb), 
                    enframe(state.name)) %>% 
  select(abb = value...2, state = value...4) %>% 
  add_row(abb = "D.C.", state = "District of Columbia")
  
  
ranks <- ranks %>% 
  left_join(states, by = "state")

slope <- ggplot() +
  geom_segment(data = ranks, 
               aes(x = "Spending rank \nin 1997", 
                   xend = "Spending rank \nin 2016",
                   y = rank1997, yend = rank2016,
                   color = diff, size = diff)) +
  geom_vline(aes(xintercept = c(1, 2)), 
             color = "#b2b2b2") +
  scale_x_discrete(position = "top") +
  geom_text(data = ranks, size = 2.75, 
            aes("Spending rank \nin 1997", rank1997, 
                label = state),
                hjust = 1, nudge_x = -.01) +
  geom_text(data = ranks, size = 2.5, 
            aes("Spending rank \nin 2016", rank2016, 
                label = state),
                hjust = 0, nudge_x = .01) +
  scale_y_reverse(expand = expansion(
   mult = c(.05, .05)
  )) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(face = "bold"),
        panel.grid = element_blank()) +
  scale_color_manual(values = 
                       c("worsen" =  "#F5CAAE",
                         "improved" =  "#001959",
                         "nochange" = "gray80")) +
  scale_size_manual(values = 
                      c("worsen" = .75,
                        "improved" =  .75,
                        "nochange" = .3))


wrap_plots(binmap, slope, widths = 3:2) +
  plot_annotation(title = "Public health spending per child in the U.S. (2016)", 
                  subtitle = "State-by-State Spending on Kids Dataset",
                  caption = "Source: Urban Institute; {tidykids}",
                  theme = theme(plot.title = element_text(size = 18))) & 
  theme(text = element_text('mono'))

