library(tidyverse)
library(ggCyberPunk)

gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')

type_groups <- gdpr_violations %>% 
  mutate(failure = str_detect(type, 
                              regex("failure to",
                                    ignore_case = TRUE)),
         non_compliance = str_detect(type,
                                     regex("non-compliance",
                                           ignore_case = TRUE)),
         cooperation = str_detect(type,
                                  regex("cooperation",
                                        ignore_case = TRUE)),
         date = lubridate::mdy(date),
         year = lubridate::year(date),
         quarter = zoo::as.yearqtr(date)
  ) %>% 
  gather(failure:cooperation,
         key = "violation",
         value = "truth") %>% 
  filter(truth == "TRUE")



type_groups %>% # 1970 dates are likely 2018 Q1
  filter(year > 1970) %>% 
  mutate(violation = tools::toTitleCase(violation)) %>% 
  group_by(quarter, violation) %>% 
  count() %>% 
  ungroup() %>% 
  ggplot(aes(quarter, n,
             color = violation,
             fill = violation)) +
  zoo::scale_x_yearqtr(n = 8) +
  geom_glowing_area() +
  theme_cyberpunk() +
  geom_linesaber() +
  scale_color_linesaber(palette = "main") +
  labs(fill = NULL,
       color = NULL,
       x = "Year/Quarter",
       y = NULL) +
  ggtitle("Violations to the General Data \nProtection Regulation (EU) \n2016/679 (GDPR)")



