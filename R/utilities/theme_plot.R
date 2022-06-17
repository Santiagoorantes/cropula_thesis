
# Theme for the presentation plots

my_theme <- list(theme_fivethirtyeight() +
                   theme(axis.title = element_text()) +
                   theme(
                     plot.title = element_text(size = 25),
                     plot.subtitle = element_text(size = 20),
                     axis.title.x = element_text(size = 20),
                     axis.title.y = element_text(size = 20),
                     axis.text.x = element_text(size = 20, angle = 0),
                     axis.text.y = element_text(size = 15),
                     panel.grid.minor = element_blank(),
                     legend.background = element_blank(),
                     legend.box.background = element_blank(),
                     panel.background = element_blank(),
                     plot.background = element_blank(),
                     legend.position = "none",
                     legend.title = element_text(size = 20),
                     legend.text = element_text(size = 20)
                   )
)
