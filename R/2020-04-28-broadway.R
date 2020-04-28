library(tidyverse)
library(patchwork)

grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv')

synopses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/synopses.csv')

cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')


#-- weekly gross over time

# cpi adjustment
# https://www.alexcookson.com/post/most-successful-broadway-show-of-all-time/
jan_2020 <- cpi %>% 
  mutate(jan_2020_dollars = 
           cpi[year_month == "2020-01-01"] / cpi)

weekly <- grosses %>% 
  distinct(week_ending, 
           .keep_all = TRUE) %>% 
  mutate(year_month = 
           lubridate::floor_date(week_ending, 
                                 unit = "month"),
         year = lubridate::year(week_ending)) %>%
  left_join(jan_2020, by = "year_month") %>% 
  mutate(gross = weekly_gross_overall,
         gross_adj = 
           weekly_gross_overall*jan_2020_dollars) %>%
  separate(year, 
           into = c("century", "year"),
           sep = 2) %>% 
  mutate(year = paste0("'", year),
         year = fct_inorder(year))

p <- weekly %>% 
  ggplot(aes(year, gross)) +
  geom_point(alpha = .7, 
             size = .7,
             color = "#a6aab3",
             position = 
               position_jitter(width = .1)) +
  geom_boxplot(fill = NA, 
               outlier.alpha = 0) +
  cowplot::theme_minimal_hgrid(
    font_size = 11,
    font_family = "serif") +
  scale_y_log10(breaks = scales::breaks_log(8),
                labels = scales::dollar) +
  labs(x = NULL, y = NULL) +
  ggtitle("Weekly box office gross for Broadway shows")


p_adj <- weekly %>% 
  gather(gross:gross_adj,
         key = "type",
         value = "value") %>% 
  group_by(year, type) %>% 
  summarise(med_gross = median(value)) %>% 
  ggplot(aes(year, med_gross,
             color = type)) +
  geom_point() +
  geom_line(aes(group = type)) + 
  cowplot::theme_minimal_hgrid(
    font_size = 11,
    font_family = "serif") +
  scale_y_log10(breaks = scales::breaks_log(8),
                labels = scales::dollar) +
  labs(x = NULL, y = NULL,
       subtitle = "Adjusted for Jan 2020 Consumer Price Index") +
  theme(plot.subtitle = element_text(face = "bold")) +
  guides(color = FALSE) +
  scale_color_manual(values = 
                       c("gross" = "#001959",
                         "gross_adj" = "#99a1a8")) +
  annotate("text", x = "'89", y = 16000000, 
           label = "CPI adjusted \nmedian gross",
           color = "#99a1a8", size = 3) +
  geom_segment(aes(x = "'91", xend = "'94",
                   y = 15000000, yend = 13000000),
               color = "#99a1a8",
               arrow = arrow(length =
                               unit(0.04, "npc"))) +
  annotate("text", x = "'07", y = 12000000, 
           label = "Median \ngross sales", 
           color = "#001959", size = 3) +
  geom_segment(aes(x = "'06", xend = "'03",
                   y = 12000000, yend = 12000000),
               color = "#001959",
               arrow = arrow(length =
                               unit(0.04, "npc")))

# patchwork
p / p_adj


# higher gross weeks are NYE week
grosses %>% 
  mutate(year = 
           lubridate::year(week_ending)) %>%
  group_by(year) %>% 
  slice(which.max(weekly_gross_overall)) %>% 
  filter(year %in%
           c(1989, 1995, 1996, 2000, 2006, 2012, 2017))



#=========== junk drawer ==============
p_adj <- weekly %>% 
  ggplot(aes(year, gross_adj)) +
  geom_point(alpha = .4, 
             size = 1,
             position = 
               position_jitter(width = .025)) +
  geom_boxplot(fill = NA, outlier.size = 1,
               outlier.alpha = .6) +
  cowplot::theme_minimal_grid(
    font_size = 11,
    font_family = "serif") +
  scale_y_log10(labels = scales::dollar) +
  labs(x = NULL, y = NULL,
       subtitle = "Adjusted for Jan 2020 Consumer Price Index")

weekly %>% 
  gather(gross:gross_adj,
         key = "type",
         value = "value") %>% 
  ggplot(aes(year, value,
             color = type)) +
  #geom_point(alpha = .4, 
  #           size = 1,
  #           position = 
  #             position_jitter(width = .025)) +
  geom_boxplot(fill = NA, outlier.size = 1,
               outlier.alpha = 0) +
  cowplot::theme_minimal_grid(
    font_size = 11,
    font_family = "serif") +
  scale_y_log10(labels = scales::dollar) +
  labs(x = NULL, y = NULL) +
  scale_color_manual(values = 
                       c("gross" = "#001959",
                         "gross_adj" = "#99a1a8"))

#-- hamilton
grosses %>%
  filter(show == "Hamilton") %>%
  mutate(pct_ham =
           weekly_gross / weekly_gross_overall,
         med_pct = median(pct_ham)) %>% # .0872
  ggplot(aes(week_ending, pct_ham)) +
  #geom_point() +
  geom_line(aes(group = lubridate::year(week_ending))) +
  scale_x_date(breaks = scales::pretty_breaks(18),
               labels = scales::label_date_short()) +
  hrbrthemes::theme_ipsum() +
  geom_hline(yintercept = 0.0872)


yr_gross <- grosses %>% 
  mutate(year = 
           lubridate::year(week_ending)) %>%
  filter(year >= 2015 &
           year < 2019) %>% 
  group_by(year, show) %>% 
  count(wt = weekly_gross) %>% 
  mutate(hamilton = ifelse(
    show == "Hamilton",
    "Hamilton", "Other"))


ggplot(yr_gross,
       aes(year, n)) +
  geom_line(aes(group = show,
                color = hamilton)) +
  scale_y_log10(labels = scales::dollar) +
  scale_color_manual(values = 
                       c("Hamilton" = "tomato",
                         "Other" = "Gray80")) +
  theme_minimal()
