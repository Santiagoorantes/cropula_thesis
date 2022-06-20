#-------------------------------------------------------------------------------
# Custom ggplots
#-------------------------------------------------------------------------------

# --- Density/HEX 2d plot with Histogram margins ---

# Default theme
my_theme <- (
  theme_fivethirtyeight() +
    theme(
      axis.title.x = element_text(colour = "Black", size = 15),
      axis.title.y = element_text(colour = "Black", size = 15),
      axis.title = element_text(),
      text = element_text(family = "sans"),
      legend.position = "none",
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      plot.background = element_blank(),
      panel.border = element_rect(color = "black", size = 1),
      panel.background = element_rect(fill = "transparent")
    )
)

# Hexbin 2d plot
make_2d_hexbin <- function(x, y, ...) {
  
  ggplot() + 
    geom_point(aes(x, y), alpha = 0) +
    geom_hex(aes(x, y, ...)) + 
    scale_fill_viridis_c() +
    theme(legend.position = "none")
  
}

# Density 2d plot
make_2d_density <- function(x, y, ...) {
  
  ggplot() + 
    geom_point(aes(x, y), alpha = 0) +
    geom_density_2d_filled(aes(x, y), ...) +
    theme(legend.position = "none")
  
}

# Scatter density/HEX plot with histogram margins
scatter_hist_2d <- function(x, y, type = "density",
                            theme_opts = my_theme, ...) {
  
  if (tolower(type) == "hexbin") {
    
    p <- make_2d_hexbin(x, y, ...)
    
  } else {
    
    p <- make_2d_density(x, y, ...)
    
  }
  
  p <- p +
    my_theme +
    scale_y_continuous(
      name = "Price (USD/bushel)",
      # name = "v",
      limits = c(0, 2000),
      labels = as.character(seq(0, 2000, 200)),
      breaks = seq(0, 2000, 200)
    ) +
    scale_x_continuous(
      name = "Yield (Tonnes/Ha)",
      # name = "u"
      # labels = as.character(seq(floor(min(x)), ceiling(max(x)), 2)),
      # breaks = seq(floor(min(x)), ceiling(max(x)), 2)
      labels = as.character(seq(0, 14, 2)),
      breaks = seq(0, 14, 2)
    )
  
  # Marginal Histograms
  ggMarginal(p,
             type = "histogram",
             size = 3,
             fill = viridis::viridis(1, begin = 0.5, end = 0.5),
             xparams = list(bins = 30),
             yparams = list(bins = 30))
  
}

