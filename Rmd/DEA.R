
# --- Load required packages ---
source("global.R")

# --- Load raw data ---
source("data.R")


# ------------------------------------------------------------------------------
# --- 1. Data preparation ---
# ------------------------------------------------------------------------------

#Change to time series object
#corn_ts <- xts(corn_ts, order.by = as.Date(corn_ts$Date, "%Y-%m-%d"))
class(corn_ts)

corn_ts$Date <- as.Date(corn_ts$Date, "%Y-%m-%d")

ini_date <- as.Date("2003-01-01")
end_date <- as.Date("2020-12-31")

corn_df <- filter(corn_ts, Date >= ini_date, Date <= end_date)

price_color <- "#69b3a2"

corn_df %>%
  ggplot(aes(x = Date, y = Settle)) +
  geom_line(size = 1.1, colour = price_color) +
  geom_smooth(method=glm, formula = y ~ poly(x, 5),
              aes(colour = "grey"),
              fullrange=TRUE, linetype=2, level = 0.99,
              #add linear trend line
              se=TRUE, col='darkgrey', size=1) + 
  labs(title = "Corn price time series",
       subtitle = "CME_C3",
       y = "Settle price") + 
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(color = price_color, size=20)) +
  scale_x_date(breaks=date_breaks("1 year"),
               labels=date_format("%Y")) +
  scale_y_continuous(breaks = seq(round(min(corn_df$Settle),-3)+100,
                                  round(max(corn_df$Settle),-3)+100,
                                  100)) +
  theme(legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank())


ggsave("misc/plots/CME_C3_plot.pdf",
       width = 30,
       height = 15,
       units = "cm",
       bg = "transparent",
       device = cairo_pdf)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Before working with the SIAP df (agr_df) we will change the column names
## to their English equivalent in a standardized format.


colnames(agr_df) <- c("year",
                      "id_state",
                      "state",
                      "id_district",  
                      "district", # DDR (rural-development-district)
                      "id_cader",
                      "cader", # Further split of the DDR
                      "id_municipality",
                      "municipality",
                      "id_cycle",
                      "cycle",
                      "id_type",
                      "type",
                      "id_unit",
                      "unit",
                      "id_crop",
                      "crop",
                      "sowed",
                      "harvested",
                      "damaged",
                      "volume",
                      "yield",
                      "price",
                      "production_value")


### Plotting the top 10 States with highest plowed hectares
by_year_state <- agr_df %>% 
  group_by(year, state)

hect_year_state <- by_year_state %>% 
  summarise(sowed = sum(sowed),
            damaged = sum(damaged)) %>%
  arrange(desc(sowed), .by_group = TRUE)

## Top 10 states in 2020
states <- head(hect_year_state %>% filter(year == 2020) %>% .$state, 10)

## Dataframe with the top 10 states per year
top_hect_year_state <- hect_year_state %>% 
  filter(state %in% states)

years <- unique(top_hect_year_state$year)

top_hect_year_state %>%
  ggplot(aes(x = year, y = sowed, color = state)) + 
  geom_line(size = 1.5, alpha = 0.8) +
  labs(title = "Total sowed hectares per year",
       subtitle = "By state",
       y = "Hectares",
       color = "State") + 
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(labels = as.character(years), breaks = years)


### Plotting top 5 crops with highest plowed hectares

by_year_crop <- agr_df %>% 
  group_by(year, crop)

hect_year_crop <- by_year_crop %>% 
  summarise(sowed = sum(sowed),
            damaged = sum(damaged)) %>%
  arrange(desc(sowed), .by_group = TRUE)

crops <- head(hect_year_crop %>% filter(year == 2020) %>% .$crop, 10)

## Dataframe with the top 10 crops per year
top_hect_year_crop <- hect_year_crop %>% 
  filter(crop %in% crops)

years <- unique(top_hect_year_crop$year)

top_hect_year_crop %>%
  ggplot(aes(x = year, y = sowed, color = crop)) + 
  geom_line(size = 1.5, alpha = 0.8) +
  labs(title = "Total plowed hectares per year",
       subtitle = "By crop",
       y = "Hectares",
       color = "Crop") + 
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(labels = as.character(years), breaks = years)


### Plotting top 4 crops per Cicle with highest plowed hectares

by_year_crop <- agr_df %>% 
  group_by(year, crop, cycle)

hect_year_crop <- by_year_crop %>% 
  summarise(sowed = sum(sowed),
            damaged = sum(damaged)) %>%
  arrange(desc(sowed), .by_group = TRUE)

perenne <- head(hect_year_crop %>%
                  filter(year == 2020, 
                         cycle %in% c("Perennes")) %>%
                  arrange(desc(sowed)) %>%
                  .$crop, 4)

pv <- head(hect_year_crop %>%
             filter(year == 2020, 
                    cycle %in% c("Primavera-Verano")) %>%
             arrange(desc(sowed)) %>%
             .$crop, 4)

oi <- head(hect_year_crop %>%
             filter(year == 2020, 
                    cycle %in% c("Otoño-Invierno")) %>%
             arrange(desc(sowed)) %>%
             .$crop, 4)

## Dataframe with the top 4 crops per Cicle
top_hect_year_crop <- hect_year_crop %>% 
  filter(cycle %in% c("Primavera-Verano") & crop %in% pv |
           cycle %in% c("Otoño-Invierno") & crop %in% oi)

years <- unique(top_hect_year_crop$year)

top_hect_year_crop %>%
  ggplot(aes(x = year, y = sowed, color = crop, linetype = cycle)) + 
  geom_line(size = 1.5, alpha = 0.8) +
  labs(title = "Total plowed hectares per year",
       subtitle = "Top 4 crops by productive cicle",
       y = "Hectares",
       color = "Crop: ") + 
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(labels = as.character(years), breaks = years)


### Plotting top 5 states per cicle with highest plowed hectares

by_year_cicle <- agr_df %>% 
  group_by(year, state, cycle)

hect_year_cicle <- by_year_cicle %>% 
  summarise(sowed = sum(sowed),
            damaged = sum(damaged)) %>%
  arrange(desc(sowed), .by_group = TRUE)

# Top n states
top_states <- head(hect_year_state %>% filter(year == 2020) %>% .$state, 5)

# Dataframe with the top states per Cicle

top_hect_year_cicle <- hect_year_cicle %>% 
  filter(state %in% top_states, cycle != "Perennes")

years <- unique(top_hect_year_cicle$year)

top_hect_year_cicle %>%
  ggplot(aes(x = year, y = sowed, color = state)) + 
  geom_line(aes(linetype = cycle),size = 1.5, alpha = 0.8) +
  labs(title = "Total plowed hectares per year",
       subtitle = "Top states by productive cicle",
       y = "Hectares",
       color = "State: ") + 
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(labels = as.character(years), breaks = years) +
  scale_linetype_manual(values = c("longdash", "solid"))

#### guasave Maíz_PVR por cader

maiz_oir_sinaloa <- agr_df %>%
  filter(state %in% c("Sinaloa") &
           crop %in% c("Maíz grano") &
           cycle %in% c("Otoño-Invierno") &
           type == "Riego") %>%
  group_by(year, municipality, district)

guasave <- maiz_oir_sinaloa %>% filter(municipality == "Guasave")

guasave_year <- guasave %>% 
  summarise(av_yield = sum(volume) / sum(sowed),
            av_price = sum(production_value) / sum(volume))

damage <- guasave %>%
  summarise(av_damg = sum(damaged)/sum(sowed))

years <- unique(guasave$year)

price1 <- guasave_year %>%
  ggplot(aes(x = year, y = av_price, color = district, group = district)) + 
  geom_line(size = 1.5, alpha = 0.8) +
  geom_smooth(method=lm, formula = y~x,
              aes(group = 1, linetype = "Trendline", colour = "grey"),
              fullrange=TRUE, linetype=2, 
              se=TRUE, col='darkgrey', size=1) + #add linear trend line
  labs(title = "Average annual yield",
       subtitle = "Corn, Autumn-Winter, Guasave-Sinaloa",
       y = "Tons/Hect",
       x = "Year",
       color = "District: ") + 
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(labels = as.character(years), breaks = years) +
  theme(legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank())

guasave_year %>%
  ggplot(aes(x = year, y = av_yield, color = district, group = district)) + 
  geom_line(size = 1.5, alpha = 0.8) +
  geom_smooth(method=lm, formula = y~x,
              aes(group = 1, linetype = "Trendline", colour = "grey"),
              fullrange=TRUE, linetype=2, 
              se=TRUE, col='darkgrey', size=1) + #add linear trend line
  labs(title = "Average annual yield",
       subtitle = "Corn, Autumn-Winter, Guasave-Sinaloa",
       y = "Tons/Hect",
       x = "Year",
       color = "District: ") + 
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(labels = as.character(years), breaks = years) +
  theme(legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank())

#~~~~~~ Comparing Yield and Price ~~~~~~#

## Everything about Non-standard evaluation
#http://adv-r.had.co.nz/Computing-on-the-language.html

# Choosing color palettes
#https://projects.susielu.com/viz-palette

# generic geom_line ggplot for certain variables 
my_plot <- function(data,
                    x,
                    y,
                    color_by = "",
                    ylab = "",
                    title = "")
{
  # using substitute() and eval() to locally evaluate the var names
  x_call <- substitute(x)
  y_call <- substitute(y)
  color_by_call <- substitute(color_by)
  years <- unique(eval(x_call, data))
  # the actual plot
  data %>%
    ggplot(aes(x = eval(x_call, data),
               y = eval(y_call, data),
               color = eval(color_by_call, data))) + 
    geom_line(size = 1.5, alpha = 0.8) +
    # labels of the plot
    labs(title = title,
         y = ylab,
         x = "",
         color = paste0(color_by_call,": ")) + 
    theme_fivethirtyeight() +
    theme(axis.title = element_text()) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_x_continuous(labels = as.character(years), breaks = years) +
    theme(legend.background = element_blank(),
          legend.box.background = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank())
  
}

#data_plot <- guasave_year

# Only plotting the top 5 municipalities
mun <- maiz_oir_sinaloa %>%
  filter(year == 2020) %>%
  group_by(municipality) %>%
  summarise(sowed = sum(sowed)) %>%
  arrange(desc(sowed)) %>%
  .$municipality %>%
  head(5)

data_plot <- maiz_oir_sinaloa %>%
  filter(municipality %in% mun) %>%
  group_by(year, municipality) %>%
  summarise(av_yield = sum(volume) / sum(sowed),
            av_price = sum(production_value) / sum(volume))

p1 <- my_plot(data = data_plot, x=year, y=av_yield,
              color_by = municipality,
              ylab = "Yield (ton/hect)",
              title = "Corn OI-R Sinaloa")
p2 <- my_plot(data = data_plot, x=year, y=av_price,
              color_by = municipality,
              ylab = "Price (pesos/ton)",
              title = "Corn OI-R Sinaloa")

# Package patchwork
p_all <- p1 / p2
p_all + plot_layout(guides = 'collect') &
  theme(legend.position = "top")

ggsave("yield_price_sinaloa.pdf",
       bg = "transparent",
       height = 20,
       width = 30,
       units = "cm",
       device = cairo_pdf)


#### Detrending the series

y <- maiz_oir_sinaloa %>%
  filter(municipality == "Ahome") %>%
  group_by(year, municipality) %>%
  summarise(av_yield = sum(volume) / sum(sowed)) %>%
  .$av_yield
l <- length(y)
t <- seq(1, l, 1)

model <- lm(y~t)
y_detrend <- model$fitted.values[l] * (1 + model$residuals / model$fitted.values)

y1 <- ggplot() +
  geom_line(aes(x = years, y = y),
            colour = "steelblue",
            size = 1.2) +
  geom_line(aes(x = years, y = y_detrend),
            colour = "orange",
            size = 1.2,
            linetype = 2) +
  labs(y = "yield (tons/hect)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(labels = as.character(years), breaks = years) +
  scale_y_continuous(limits = c(4,16),
                     breaks = c(seq(4, 16, 1)))


p <- maiz_oir_sinaloa %>%
  filter(municipality == "Ahome") %>%
  group_by(year, municipality) %>%
  summarise(av_price = sum(production_value) / sum(volume)) %>%
  .$av_price
l <- length(p)
t <- seq(1, l, 1)

model <- lm(p~t)
p_detrend <- model$fitted.values[l] * (1 + model$residuals / model$fitted.values)

p1 <- ggplot() +
  geom_line(aes(x = years, y = p),
            colour = "steelblue",
            size = 1.2) +
  geom_line(aes(x = years, y = p_detrend),
            colour = "orange",
            size = 1.2,
            linetype = 2) +
  labs(y = "yield (tons/hect)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(labels = as.character(years), breaks = years) +
  scale_y_continuous(limits = c(1000,6000),
                     breaks = c(seq(1000, 6000, 1000)))

# Comparing plots (patchwork)
y1 / p1

# Double axis plot

price_color <- "#69b3a2"
yield_color <- "#003f5c"
yield_color2 <- "#bc5090"

coef <- p_detrend / y_detrend
coef <- 400
min_break <- round(min(y),0)
max_break <- round(max(y_detrend),0)
ggplot() +
  geom_line(aes(x = years, y = y),
            colour = yield_color2, size = 1.2) +
  geom_line(aes(x = years, y = y_detrend),
            colour = yield_color2, linetype = 2, size = 1.2) +
  geom_line(aes(x = years, y = p / coef),
            colour = price_color, size = 1.2) +
  geom_line(aes(x = years, y = p_detrend / coef),
            colour = price_color, linetype = 2, size = 1.2) +
  scale_y_continuous(
    name = "Yield (tons/hect)",
    breaks = c(seq(min_break, max_break, 1)),
    sec.axis = sec_axis(~.*coef,
                        name="Price (pesos/ton)",
                        breaks=c(seq(min_break*coef,max_break*coef,coef)))
  ) + 
  scale_x_continuous(labels = as.character(years), breaks = years) +
  labs(title = "Corn OI-R Guasave",
       subtitle = "Original vs Detrended Series",
       x = "Year") +
  theme_fivethirtyeight() +
  theme(
    axis.title = element_text(),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(color = yield_color2, size=20),
    axis.title.y.right = element_text(color = price_color, size=20),
    panel.grid.minor = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank()
  )


ggsave("misc/plots/yield_price_ahome.pdf",
       bg = "transparent",
       height = 15,
       width = 30,
       units = "cm",
       device = cairo_pdf)

# Correlation
meth <- c("pearson", "kendall", "spearman")
# Biased series
for (i in seq_along(meth)) {
  print(cor(y, p, method = meth[i]))
}
# Detrended/unbiased series
for (i in seq_along(meth)) {
  print(cor(y_detrend, p_detrend, method = meth[i]))
}
stat_rho <- cor(y_detrend, p_detrend, method = meth[3])

rho <- numeric()
for (i in 1:(l-10)) {
  rho[i] <- (cor(y_detrend[i:(i+10)],p_detrend[i:(i+10)],method = "spearman"))
}
ggplot() + geom_line(aes(x = years[11:l], y=rho)) +
  geom_abline(intercept = stat_rho, slope = 0)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Animated visualization of our SIAP agricultural data

### Grouped data at the State level on a Yearly basis

state_agr_df <- agr_df %>%
  group_by(year,
           state,
           cycle,
           type,
           crop) %>%
  summarise(sowed = sum(sowed),
            harvested = sum(harvested),
            damaged = sum(damaged),
            volume = sum(volume),
            av_yield = sum(volume) / sum(sowed),
            av_price = sum(production_value) / sum(volume),
            production_value = sum(production_value))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## DataFrame for the First Visualization

df_corn_pvr <- state_agr_df %>%
  filter(crop == "Maíz grano" &
           cycle == "Primavera-Verano" &
           type == "Riego") %>%
  group_by(year,
           state) %>%
  summarise(sowed = sum(sowed),
            harvested = sum(harvested),
            damaged = sum(damaged),
            volume = sum(volume),
            av_yield = if_else(sowed == 0, 0, sum(volume) / sum(sowed)),
            av_price = if_else(volume == 0, 0, sum(production_value) / sum(volume)),
            production_value = sum(production_value))

# Function to plot for each year with the info for the first visualization

# Taking a look at the ranges of our values for the plot
library(patchwork)
bp1 <- ggplot(df_corn_pvr, aes(av_yield)) +
  geom_boxplot()
bp2 <- ggplot(df_corn_pvr, aes(av_price)) +
  geom_boxplot()
bp1 / bp2

# Filtering year 2003 to test and build the plot
df_03 <- df_corn_pvr %>%
  filter(year == 2003)

## Function that plots the desired comparison for a single year
yield_price_plot <- function(data, subtitle){
  data %>%
    arrange(desc(sowed)) %>%
    ggplot(aes(x = av_yield, y = av_price)) +
    geom_point(aes(size = sowed, colour = state),
               stroke = 0, shape = 16,
               alpha = 0.8, na.rm = TRUE) +
    scale_size_continuous(limits = c(0, max(data$sowed)),
                          range = c(0,40)) +
    guides(size = "none") +
    scale_x_continuous(limits = c(0,12),
                       breaks = seq(0,12,1)) +
    scale_y_continuous(limits = c(1000,6000),
                       breaks = c(seq(1000, 6000, 500))) +
    labs(title = "Corn, Irrigation",
         subtitle = subtitle,
         y = "average price (pesos/ton)",
         x = "average yield (tons/hect)",
         color = "State") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(),
          text = element_text(family = "sans"))
}

## Testing the function
yield_price_plot(df_03, "Year: 2003")

## Splitting the dataframe into groups by year
by_year <- df_corn_pvr %>%
  group_split(year)

## Building the plots for each year
years <- df_corn_pvr %>% group_keys() %>% pull(year)
subtitles <- paste0("Year: ", years)
plots <- map2(by_year, subtitles, yield_price_plot)

## Saving the plots into a folder
paths <- paste0("corn_pvr_yearly/", years, ".png")
walk2(paths, plots, ggsave, width = 14, height = 8)

## Making a GIF
gifski::gifski(paths, "corn_pvr.gif", width=1400, height=800, delay=0.5)

## Using package animate
animation <- df_corn_pvr %>%
  yield_price_plot("Year: {frame_time}") +
  transition_time(year) +
  shadow_wake(wake_length = 0.05)

animate(animation,
        height = 800,
        width = 1400,
        fps = 30,
        duration = 18,
        end_pause = 40,
        res = 100)

# saving the animation
anim_save("animation_corn_pvr.gif")

## Wrapping all plots in one to compare at the same time
df_corn_pvr %>%
  yield_price_plot("All years") +
  facet_wrap(~year) +
  scale_size(range = c(0,4), guide = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## DataFrame for the Next Visualization

df_corn_oir <- state_agr_df %>%
  filter(crop == "Maíz grano" &
           cycle == "Otoño-Invierno" &
           type == "Riego") %>%
  group_by(year,
           state) %>%
  summarise(sowed = sum(sowed),
            harvested = sum(harvested),
            damaged = sum(damaged),
            volume = sum(volume),
            av_yield = if_else(sowed == 0, 0, sum(volume) / sum(sowed)),
            av_price = if_else(volume == 0, 0, sum(production_value) / sum(volume)),
            production_value = sum(production_value))

# Splitting the dataframe into groups by year
by_year <- df_corn_oir %>%
  group_split(year)

## Building the plots for each year
years <- df_corn_oir %>% group_keys() %>% pull(year)
subtitles <- paste0("Year: ", years)
plots <- map2(by_year, subtitles, yield_price_plot)

## Saving the plots into a folder
paths <- paste0("corn_oir_yearly/", years, ".png")
walk2(paths, plots, ggsave, width = 14, height = 8)

## Making a GIF
gifski::gifski(paths, "corn_oir.gif", width=1400, height=800, delay=0.5)

## Creating animation

animation <- df_corn_oir %>%
  yield_price_plot("Year: {frame_time}") +
  transition_time(year) +
  shadow_wake(wake_length = 0.05)

animate(animation,
        height = 800,
        width = 1400,
        fps = 30,
        duration = 18,
        end_pause = 40,
        res = 100)

# saving the animation
anim_save("animation_corn_oir.gif")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Visualizing the main 3 crops to analyze (corn, wheat, soybean)

###### Filtering for PV-R (Summer-Spring, Irrigation) and the main crops ######
main_pvr <- agr_df %>%
  filter(crop %in% c("Maíz grano", "Cebada grano", "Soya") &
           cycle == "Primavera-Verano" &
           type == "Riego") %>%
  group_by(year,
           state,
           crop,
           cycle,
           type) %>%
  summarise(sowed = sum(sowed),
            harvested = sum(harvested),
            damaged = sum(damaged),
            volume = sum(volume),
            av_yield = if_else(sowed == 0, 0, sum(volume) / sum(sowed)),
            av_price = if_else(volume == 0, 0, sum(production_value) / sum(volume)),
            production_value = sum(production_value))

# Filtering year 2003 to test and build the plot
df_03 <- main_pvr %>%
  filter(year == 2003)
df_20 <- main_pvr %>%
  filter(year == 2020)

## Function that plots the desired comparison for a single year
main_compare_plot <- function(data, subtitle){
  data %>%
    arrange(desc(sowed)) %>%
    ggplot(aes(x = av_yield, y = av_price)) +
    geom_point(aes(size = sowed, colour = crop),
               stroke = 0, shape = 16,
               alpha = 0.8, na.rm = TRUE) +
    scale_size_continuous(limits = c(0, max(data$sowed)),
                          trans = "sqrt",
                          range = c(0,40)) +
    guides(size = "none") +
    scale_x_continuous(limits = c(0,12),
                       breaks = seq(0,12,1)) +
    scale_y_continuous(limits = c(0,9000),
                       breaks = c(seq(0, 9000, 500))) +
    labs(title = "Basic crops comparison (irrigation)",
         subtitle = subtitle,
         y = "average price (pesos/ton)",
         x = "average yield (tons/hect)",
         color = "Crop: ") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(),
          text = element_text(family = "sans"))
}

## Testing the function
main_compare_plot(df_03, "Spring-Summer: 2003")
main_compare_plot(df_20, "Spring-Summer: 2020")

## Splitting the dataframe into groups by year
by_year <- main_pvr %>%
  group_by(year) %>%
  group_split(year)

## Building the plots for each year
years <- main_pvr %>% group_by(year) %>% group_keys() %>% pull(year)
subtitles <- paste0("Spring-Summer: ", years)
plots <- map2(by_year, subtitles, main_compare_plot)

## Saving the plots into a folder
paths <- paste0("main_pvr_yearly/", years, ".png")
walk2(paths, plots, ggsave, width = 14, height = 8)

## Creating animation
animation <- main_pvr %>%
  main_compare_plot("Spring-Summer: {frame_time}") +
  transition_time(year) +
  shadow_wake(wake_length = 0.05)

animate(animation,
        height = 800,
        width = 1400,
        fps = 30,
        duration = 18,
        end_pause = 40,
        res = 100)

# saving the animation
anim_save("anim_main_pvr.gif")


###### Filtering for OI-R (Autumn-Winter, Irrigation) and the main crops ######
main_oir <- agr_df %>%
  filter(crop %in% c("Maíz grano", "Cebada grano", "Soya") &
           cycle == "Otoño-Invierno" &
           type == "Riego") %>%
  group_by(year,
           state,
           crop,
           cycle,
           type) %>%
  summarise(sowed = sum(sowed),
            harvested = sum(harvested),
            damaged = sum(damaged),
            volume = sum(volume),
            av_yield = if_else(sowed == 0, 0, sum(volume) / sum(sowed)),
            av_price = if_else(volume == 0, 0, sum(production_value) / sum(volume)),
            production_value = sum(production_value))

# Filtering year 2003 to test and build the plot
df_03 <- main_oir %>%
  filter(year == 2003)
df_20 <- main_oir %>%
  filter(year == 2020)

## Testing the function
main_compare_plot(df_03, "Autumn-Winter: 2003")
main_compare_plot(df_20, "Autumn-Winter: 2020")

## Splitting the dataframe into groups by year
by_year <- main_oir %>%
  group_by(year) %>%
  group_split(year)

## Building the plots for each year
years <- main_oir %>% group_by(year) %>% group_keys() %>% pull(year)
subtitles <- paste0("Autumn-Winter: ", years)
plots <- map2(by_year, subtitles, main_compare_plot)

## Saving the plots into a folder
paths <- paste0("main_oir_yearly/", years, ".png")
walk2(paths, plots, ggsave, width = 14, height = 8)

## Creating animation
animation <- main_oir %>%
  main_compare_plot("Autumn-Winter: {frame_time}") +
  transition_time(year) +
  shadow_wake(wake_length = 0.05)

animate(animation,
        height = 800,
        width = 1400,
        fps = 20,
        duration = 18,
        end_pause = 30,
        res = 120)

# saving the animation
anim_save("anim_main_oir.gif")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Visualizing the main 3 crops (corn, wheat, soybean) in Sinaloa OI-R

corn_sin_oir <- agr_df %>%
  filter(crop %in% c("Maíz grano") &
           cycle == "Otoño-Invierno" &
           type == "Riego" &
           state == "Sinaloa") %>%
  group_by(year,
           municipality,
           #crop,
           cycle,
           type) %>%
  summarise(sowed = sum(sowed),
            harvested = sum(harvested),
            damaged = sum(damaged),
            volume = sum(volume),
            av_yield = if_else(sowed == 0, 0, sum(volume) / sum(sowed)),
            av_price = if_else(volume == 0, 0, sum(production_value) / sum(volume)),
            production_value = sum(production_value))

# Filtering year 2003 to test and build the plot
df_03 <- corn_sin_oir %>%
  filter(year == 2003)
df_20 <- corn_sin_oir %>%
  filter(year == 2020)

## Function that plots the desired comparison for a single year
state_compare_plot <- function(data, subtitle){
  data %>%
    arrange(desc(sowed)) %>%
    ggplot(aes(x = av_yield, y = av_price)) +
    geom_point(aes(size = sowed, colour = municipality),
               stroke = 0, shape = 16,
               alpha = 0.8, na.rm = TRUE) +
    scale_size_continuous(limits = c(0, max(data$sowed)),
                          trans = "sqrt",
                          range = c(0,40)) +
    guides(size = "none") +
    scale_x_continuous(limits = c(0,12),
                       breaks = seq(0,12,1)) +
    scale_y_continuous(limits = c(0,5000),
                       breaks = c(seq(0, 5000, 500))) +
    labs(title = "Corn (irrigation) in Sinaloa",
         subtitle = subtitle,
         y = "average price (pesos/ton)",
         x = "average yield (tons/hect)",
         color = "Municipality: ") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(),
          text = element_text(family = "sans"))
}

## Testing the function
state_compare_plot(df_03, "Spring-Summer: 2003")
state_compare_plot(df_20, "Spring-Summer: 2020")

## Splitting the dataframe into groups by year
by_year <- corn_sin_oir %>%
  group_by(year) %>%
  group_split(year)

## Building the plots for each year
years <- corn_sin_oir %>% group_by(year) %>% group_keys() %>% pull(year)
subtitles <- paste0("Autumn-Winter: ", years)
plots <- map2(by_year, subtitles, state_compare_plot)

## Saving the plots into a folder
paths <- paste0("sinaloa_corn_oir/", years, ".png")
walk2(paths, plots, ggsave, width = 14, height = 8)

## Creating animation
animation <- corn_sin_oir %>%
  state_compare_plot("Autumn-Winter: {frame_time}") +
  transition_time(year) +
  shadow_wake(wake_length = 0.05)

animate(animation,
        height = 800,
        width = 1400,
        fps = 20,
        duration = 18,
        end_pause = 30,
        res = 120)

# saving the animation
anim_save("anim_sin_corn_oir.gif")


plot(x = corn_sin_oir$year, y = corn_sin_oir$av_yield)
