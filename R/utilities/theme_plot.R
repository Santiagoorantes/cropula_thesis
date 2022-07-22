
# Theme for the presentation plots

big_size <- 12
med_size <- 10
sm_size <- 8

my_theme <- list(theme_fivethirtyeight() +
                   theme(axis.title = element_text()) +
                   theme(
                     plot.title = element_text(size = big_size),
                     plot.subtitle = element_text(size = med_size),
                     axis.title.x = element_text(size = med_size),
                     axis.title.y = element_text(size = med_size),
                     axis.text.x = element_text(size = med_size, angle = 0),
                     axis.text.y = element_text(size = med_size),
                     panel.grid.minor = element_blank(),
                     legend.background = element_blank(),
                     legend.box.background = element_blank(),
                     panel.background = element_blank(),
                     plot.background = element_blank(),
                     legend.position = "bottom",
                     legend.direction = "horizontal",
                     legend.title = element_text(size = med_size),
                     legend.text = element_text(size = med_size)
                   )
)
