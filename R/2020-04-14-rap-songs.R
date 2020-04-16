library(tidyverse)
library(cowplot)
library(magick)

polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')

rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')


# replicate datawrapper plot:
# https://blog.datawrapper.de/best-hip-hop-songs-of-all-time-visualized/

plot <- ggplot(rankings,
               aes(year, points)) +
  geom_jitter(
    size = 2,
    width = .3, 
    height = .05,
    alpha = .7,
    aes(color = 
          fct_relevel(gender,
                      "male", "female", "mixed"))
  ) +
  theme_half_open() +
  background_grid() +
  geom_hline(yintercept = 25) +
  ylim(0,150) +
  labs(y = NULL,
       x = NULL) +
  scale_color_manual(
    name = NULL,
    values = 
      c("male" = "#2B818EFF",
        "female" = "#792427FF",
        "mixed" = "purple"),
    labels = 
      c("Male rappers", "Female rappers", "Mixed bands/Collabs"),
  ) +
  guides(color = guide_legend(
    override.aes = list(size = 4,
                        alpha = 1))
  ) + 
  theme(legend.position = "top",
        axis.text = element_text(
          color = "Gray40")) +
  annotate("text", x = 1979, y = 150, 
           label = "Critic \nrating",
           color = "Gray40") +
  annotate("text", x = 2019, y = 32,
           label = "Top 25",
           color = "Gray40") +
  geom_segment(aes(x = 2017, xend = 2017,
                   y = 28, yend = 34),
               color = "Gray40",
               arrow = arrow(length = unit(0.02, "npc"))) +
  geom_curve(aes(x = 1999, xend = 1994.7,
                 y = 135, yend = 141),  
             curvature = 0.2, 
             arrow = arrow(length = unit(0.04, "npc")))


# juicy album image
# ref: https://wilkelab.org/cowplot/articles/drawing_with_on_plots.html

img <- image_read("https://www.theedgesusu.co.uk/wp-content/uploads/2018/03/notorious-big-e1522484589588.jpg") %>%
  image_resize("570x380") %>%
  image_colorize(30, "white")

# draw img + plot
ggdraw() + 
  draw_image(img) + 
  draw_plot(plot)

