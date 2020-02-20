library(tidyverse)
library(interactions)


#-- ingest --  
consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

glimpse(consumption)

consumption %>% distinct(food_category)
consumption %>% distinct(country)

#-- 
continents <- gapminder::gapminder %>% 
  filter(year == 2007) %>% 
  select(country, continent)

data <- consumption %>% 
  left_join(continents,
            by = "country")

plot_data <- data %>%
  mutate(product = 
           case_when(
             food_category %in%
               c("Beef", "Eggs", "Fish",
                 "Lamb & Goat", 
                 "Milk - inc. cheese",
                 "Pork", "Poultry") ~ "Animal foods",
             !food_category %in% 
               c("Beef", "Eggs", "Fish",
                 "Lamb & Goat", 
                 "Milk - inc. cheese",
                 "Pork", "Poultry") ~ "Plant-based foods"
           ))

p1 <- ggplot(plot_data,
       aes(consumption, 
           co2_emmission, 
           color = food_category)) +
  geom_point() +
  scale_y_log10(name = "Emission (kg CO2/person/year)",
                label = scales::comma) +
  scale_x_log10(labels = scales::number,
                name = "Consumption (kg/person/year)") +
  scico::scale_color_scico_d() +
  facet_wrap(~product) +
  theme_minimal() +
  ggrepel::geom_text_repel(
    data = plot_data %>% 
      distinct(food_category, 
               product,
               .keep_all = TRUE),
    aes(consumption, 
        co2_emmission,
        label = food_category),
    nudge_x = -2.5,
    nudge_y = -.25,
    direction = "both",
    hjust = 2,
    vjust = 1.5,
    seed = 300) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        panel.grid.minor.x = element_blank())




mod <- lm(co2_emmission ~ consumption + product,
              data = plot_data)

mod_int <- lm(co2_emmission ~ consumption*product,
              data = plot_data)

jtools::summ(mod)
jtools::summ(mod_int,
             center = TRUE)

anova(mod, mod_int)

p_int <- interact_plot(mod_int, 
              pred = consumption,
              modx = product,
              interval = TRUE,
              colors = "Qual1",
              rug = TRUE,
              rug.sides = "bl",
              x.label = "Consumption (kg/person/year)",
              y.label = "Emission (kg CO2/person/year)",
              legend.main = "") +
  theme(legend.position = "top") +
  annotate("text", x = 120, y= 1500, 
           label = "F(3, 1426) = 123.61, p < 0.001, R2 = 0.21")


tab <- sjPlot::tab_model(mod, mod_int,
                  show.fstat = TRUE,
                  show.aic = TRUE)


library(patchwork)

p_normal <- ggplot(plot_data,
                   aes(consumption, 
                       co2_emmission, 
                       color = food_category)) +
  geom_point() +
  scico::scale_color_scico_d() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(subtitle = "No transformation")

p_log_x  <- ggplot(plot_data,
                   aes(consumption, 
                       co2_emmission, 
                       color = food_category)) +
  geom_point() +
  scale_x_log10(labels = scales::number) +
  scico::scale_color_scico_d() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(y = "",
       subtitle = "Consumption (log10)")


p_log_y <- ggplot(plot_data,
                  aes(consumption, 
                      co2_emmission, 
                      color = food_category)) +
  geom_point() +
  scale_y_log10(label = scales::comma) +
  scico::scale_color_scico_d() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(y = "",
       subtitle = "Emissions (log10)")


p_normal + p_log_x + p_log_y
