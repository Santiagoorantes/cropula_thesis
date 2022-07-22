
# --------------------------- 0.0 Loading --------------------------------------

# --- 0.01 Load required packages ---
source("global.R")

# --- 0.02 Load data ---
source("data.R")

# -------------------------- 0.1 Constants -------------------------------------

# simulations
n <- 100000 
# Bootstraping loops gofCopula 
M <- 2500
MJ <- 1000 # boot for gofKernel
# Time of cultivation (days)
days <- 180
# Bushels in a Tonne
bush_tonne <- 39.368

# ------------------------- 1. Yield Distribution ------------------------------

# --- 1.0 Applying filters ---

muns <- c("Ahome", "Culiacán", "Guasave", "Navolato")

# Define filter
my_filter <-  list(
  crop = c("Maíz grano"),
  type = c("Riego"),
  cycle = c("Otoño-Invierno"),
  state = c("Sinaloa"),
  municipality = muns
)

# Yield per year given filter and group_by
filtered_df <- filter_df(agr_df, my_filter)

group_vars <- c("year", names(my_filter))
trgt_df <- group_agr_df(filtered_df, group_vars)

# --- 1.1 Detrending the series ---

years <- unique(trgt_df$year)
muns_df <- split(trgt_df, trgt_df$municipality)
# NOTE: make sure that there is only ONE value per year to detrend the series

# - Phillips-Perron Unit Root Test (H0: has unit root) -
lapply(muns_df, function(x) PP.test(x$av_yield))
# We reject unit root if < 0.05

# - Berush-Pagan Test against heteroskedasticity (H0: homoskedastic) -
lapply(muns_df, function(mun) bptest(mun$av_yield ~ mun$year))
# Rejects if too much of the variance is explained by regressors (<0.05)

lmodel <- function(list, x, y, order) {
  yield_lm <- lapply(list, function(l) glm(unlist(l[y]) ~ poly(unlist(l[x]), order)))
  return(yield_lm)
}

yield_lm <- lmodel(muns_df, "year", "av_yield", order = 1)
lapply(yield_lm, summary)

par(mfrow = c(2,2))
for (i in 1:length(muns)) acf(yield_lm[[i]]$residuals, main = muns[i])

lmodel_coef <- lapply(lapply(yield_lm, summary), coefficients)
vars <- c("Intercept", "t")
lmodel_coef <- lapply(lmodel_coef, function(x) cbind(vars, data.frame(x)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# --- Table 1: Results of fitted linear model to time ---
lmodel_coef_df <- Reduce(function(x, y) merge(x, y, all=TRUE, sort = FALSE), lmodel_coef)
lmodel_coef_df <- cbind("mun" = rep(muns, each = 2), lmodel_coef_df)

# signif_cell <- emphasize.strong.cells(which(lmodel_coef_df$Pr...t.. < 0.05, arr.ind = TRUE))

collapse_rows_df <- function(df, variable){
  group_var <- enquo(variable)
  df %>%
    group_by(!! group_var) %>%
    mutate(groupRow = 1:n()) %>%
    ungroup() %>%
    mutate(!!quo_name(group_var) := ifelse(groupRow == 1, as.character(!! group_var), "")) %>%
    dplyr::select(-c(groupRow))
}

lmodel_coef_df <- collapse_rows_df(lmodel_coef_df, mun)

lmodel_coef_df <- lmodel_coef_df %>%
  mutate(Estimate = round(Estimate, 3),
         Std..Error = round(Std..Error, 3),
         t.value = round(t.value, 3),
         Pr...t.. = as.character(signif(Pr...t.., 3)))

# lmodel_coef_df$Estimate[signif_cell] <- cell_spec(lmodel_coef_df$Estimate[signif_cell],
#                                                   "html", bold = T)

names(lmodel_coef_df) <- c("Municipality", "Coefficient", "Estimate", "Std Error", "$t$-stat", "$P(>|t|)$")

table_1 <- lmodel_coef_df %>%
  kbl(caption = "<span style='font-size:400%'><center>Table 1: Linear model wrt time </center></span>",
      booktabs = T, linesep = "", align = "c", format = "html") %>%
  kable_styling(latex_options = c("striped", "hold_position"), font_size = 50) %>%
  kable_paper(full_width = F) %>%
  row_spec(0, bold = T, color = spec_color(1)) %>%
  column_spec(1, color = spec_color(1, option = "A")) %>% 
  collapse_rows(columns = 1, valign = "middle")

#save_kable(table1, file = "Table_1_lmyield.html")
#webshot("Table_1_lmyield.html", "Table_1_lmyield.pdf")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# The simple linear regression for the yield was sufficient
lmodel_coef_df <- Reduce(function(x, y) merge(x, y, all=TRUE, sort = FALSE), lmodel_coef)
lmodel_coef_df <- cbind("mun" = rep(muns, each = 2), lmodel_coef_df)

signif_muns <- lmodel_coef_df %>% 
  filter(vars == "t",
         Pr...t.. < 0.05) %>%
  .$mun
cat("The lm was significant for the following municipalities: \n", signif_muns)  


# - de_trending relevant municipalities -

l <- length(years)
dt_yield_lm <- yield_lm[signif_muns]

# y_bar_t = y_hat_2020 * (1 + e_hat_t / y_hat_t )
yield_dt <- lapply(dt_yield_lm, function(model) {
  model$fitted.values[l] * (1 + model$residuals / model$fitted.values)
})

# Using Ljung-Box test to check for check for temporal dependency
# H0: The residuals are independently distributed.
# HA: The residuals are not independently distributed; they exhibit serial correlation.

orig_yield <- data.frame(lapply(muns_df, function(mun) mun$av_yield))
dt_yield <- orig_yield
dt_yield[signif_muns] <- data.frame(yield_dt)

# Temporal dependency
lapply(orig_yield, function(x) Box.test(x, lag = 1, type = "Ljung-Box"))
lapply(dt_yield[signif_muns], function(x) Box.test(x, lag = 1, type = "Ljung-Box"))

# Homoscedasticity of variances
lapply(orig_yield, function(x) Box.test(x^2, lag = 1, type = "Ljung-Box"))
lapply(dt_yield[signif_muns], function(x) Box.test(x^2, lag = 1, type = "Ljung-Box"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# --- Plot 1: Yield Plots (Original vs De-trended) ---

orig_yield["Series"] <- "Original"
dt_yield["Series"] <- "De-trended"

y <- rbind(orig_yield, dt_yield)

y_df <- data.frame("years" = rep(years,2), y)

df_melted <- reshape2::melt(y_df, id = c("years", "Series"))
plt_df <- split(df_melted, df_melted$variable)
y <- df_melted$value
y_color <- "#62AC72"
p1 <- ggplot(plt_df$Ahome) +
  geom_line(aes(x = years, y = value, linetype = Series, colour = variable),
            size = 1.2, color = y_color) +
  labs(title = muns[1],
       x = "Year",
       y = "Yield (Ton/Ha)") +
  scale_x_continuous(labels = as.character(years), breaks = years) +
  scale_y_continuous(limits = c(floor(min(y)),ceiling(max(y))),
                     breaks = c(seq(floor(min(y)), ceiling(max(y)), 1))) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  my_theme +
  theme(axis.text.x = element_text(angle = 90))
p2 <- ggplot(plt_df$Culiacán) +
  geom_line(aes(x = years, y = value, linetype = Series, colour = variable),
            size = 1.2, color = y_color) +
  labs(title = muns[2],
       x = "Year",
       y = "Yield (Ton/Ha)") +
  scale_x_continuous(labels = as.character(years), breaks = years) +
  scale_y_continuous(limits = c(floor(min(y)),ceiling(max(y))),
                     breaks = c(seq(floor(min(y)), ceiling(max(y)), 1))) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  my_theme +
  theme(axis.text.x = element_text(angle = 90))
p3 <- ggplot(plt_df$Guasave) +
  geom_line(aes(x = years, y = value, linetype = Series, colour = variable),
            size = 1.2, color = y_color) +
  labs(title = muns[3],
       x = "Year",
       y = "Yield (Ton/Ha)") +
  scale_x_continuous(labels = as.character(years), breaks = years) +
  scale_y_continuous(limits = c(floor(min(y)),ceiling(max(y))),
                     breaks = c(seq(floor(min(y)), ceiling(max(y)), 1))) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  my_theme +
  theme(axis.text.x = element_text(angle = 90))
p4 <- ggplot(plt_df$Navolato) +
  geom_line(aes(x = years, y = value, linetype = Series, colour = variable),
            size = 1.2, color = y_color) +
  labs(title = muns[4],
       x = "Year",
       y = "Yield (Ton/Ha)") +
  scale_x_continuous(labels = as.character(years), breaks = years) +
  scale_y_continuous(limits = c(floor(min(y)),ceiling(max(y))),
                     breaks = c(seq(floor(min(y)), ceiling(max(y)), 1))) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  my_theme +
  theme(axis.text.x = element_text(angle = 90))

guide_area() + 
  (p1 + p2) / (p3 + p4) +
  plot_layout(guides = "collect", 
              nrow = 2, heights = c(1,10)) + 
  plot_annotation(title = "Original vs De-trended Yield",
                  theme = theme(plot.title = element_text(family = "sans",
                                                          size = 20,
                                                          color = '#414141')))

# ggsave("yield_plot.pdf",
#        bg = "transparent",
#        height = 20,
#        width = 30,
#        units = "cm",
#        device = cairo_pdf)

orig_yield <- subset(orig_yield, select = -c(Series))
dt_yield <- subset(dt_yield, select = -c(Series))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# --- 1.2 Fitting distributions to the YIELD ---

# Pearson (based on Cullen and Frey) plots
p1 <- pearson_plot(dt_yield$Ahome, boot = 1000, method = "sample", alpha = 0.7, subtitle = "Ahome")
p2 <- pearson_plot(dt_yield$Culiacán, boot = 1000, method = "sample", alpha = 0.7, subtitle = "Culiacán")
p3 <- pearson_plot(dt_yield$Guasave, boot = 1000, method = "sample", alpha = 0.7, subtitle = "Guasave")
p4 <- pearson_plot(dt_yield$Navolato, boot = 1000, method = "sample", alpha = 0.7, subtitle = "Navolato")

(p1 + p2) / (p3 + p4)

# ggsave("pearson_plot.pdf",
#        bg = "transparent",
#        height = 20,
#        width = 30,
#        units = "cm",
#        device = cairo_pdf)

# Fits given distributions for every column of the df
fit_all <- function(df, dist_list, method, ...) {
  fit_list <- list()
  for (i in 1:length(dist_list)) {
    if (dist_list[i] != "shift_beta") {
      fit_list[[dist_list[i]]] <- lapply(
        df, function(x) {
          fitdist(x, distr = dist_list[[i]], method = method)
        }
      )
    } else {
      fit_list[[dist_list[i]]] <- lapply(
        df, function(x) {
          # Extra parameters for shift_beta
          eps <- 0.3
          a <- 0
          b <- ceiling(max(x) + eps * sd(x))
          print(b)
          fit <- fitdist(x, distr = dist_list[[i]], method = method,
                         fix.arg = list(a = a, b = b), ...)
          return(fit)
        }
      )
    }
  }
  return(fit_list)
}


dist_list <- c("norm", "gamma", "weibull", "shift_beta")
all_dists <- fit_all(dt_yield,
                     dist_list = dist_list,
                     method = "mle",
                     start = list(alpha = 10, beta = 1))

# --- 2.3 Comparing fitted distributions ---

gof_fit_yield <- lapply(all_dists, function(d) lapply(d, gofstat))

params_yield <- lapply(all_dists, function(d) lapply(d, coef))

gof_yield <- lapply(gof_fit_yield, function(d) lapply(d, function(x) {
  data.frame("KS" = x$ks,
             "CvM" = x$cvm,
             "AD" = x$ad,
             "AIC" = x$aic,
             "BIC" = x$bic)  
  }
))

# Arranging the GOF tests for the table
gof_yield_mun <- lapply(
  gof_yield, function(mun){
    df <- Reduce(function(x, y) merge(x, y, all=TRUE, sort = FALSE), mun)
    df["Municipality"] <- names(mun)
    return(df)
  }
)

gof_yield_df <- Reduce(function(x, y) merge(x, y, all=TRUE, sort = FALSE), gof_yield_mun)

dist_names <- c("Normal", "Gamma", "Weibull", "Beta")
gof_yield_df["Distribution"] <- rep(dist_names, each = 4)

# Which have smallest BIC
test_dist <- split(gof_yield_df, gof_yield_df$Municipality)
min_test <- lapply(test_dist, function(d) min(d$BIC))
# Best fitting distribution
selected_distributions <- lapply(test_dist, function(mun) mun$Distribution[which(mun$BIC %in% min(mun$BIC))])
selected_dist_df <- data.frame("mun" = names(selected_distributions),"name" = unlist(selected_distributions))
dist_df <- data.frame("name" = dist_names, "R_name" = dist_list)

(selected_dist_df <- merge(selected_dist_df, dist_df, by = "name", sort = FALSE))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# --- Table 2: GoF Tests for the yield distribution ---

gof_yield_df <- gof_yield_df %>%
  group_by(Distribution) %>%
  arrange(Municipality) %>%
  dplyr::select(-c(Municipality))

  
gof_yield_df <- cbind("Distribution" = gof_yield_df$Distribution,
                      gof_yield_df[names(gof_yield_df)!="Distribution"])


params_df <- matrix(unlist(lapply(params_yield, data.frame)), ncol = 2, byrow = TRUE)
params_df <- cbind("Municipality" = rep(muns, 4),
                   "Distribution" = rep(dist_names, each = 4),
                   data.frame(params_df))
names(params_df)[3:4] <- c("1", "2")

params_df <- params_df %>%
  arrange(Municipality)

tbl_df <- cbind(gof_yield_df, params_df[c("1", "2")])
# Selected Distribution (to bold in table)
slected_dist <- which(tbl_df$BIC %in% min_test)
# Rounding cols
tbl_df[2:4] <- round(tbl_df[2:4], 4)
tbl_df[5:ncol(tbl_df)] <- round(tbl_df[5:ncol(tbl_df)], 3)


table_2 <- tbl_df %>%
  kbl(caption = "<span style='font-size:400%'><center>Table 2:Selection criteria for fitted distributions</center></span>",
      booktabs = T, linesep = "", align = "c") %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                font_size = 50) %>%
  kable_paper(full_width = T) %>%
  add_header_above(c("Municipality", "Criteria / GoF Test" = 5, "Parameters" = 2),
                   color = spec_color(1, option = "A"), bold = T) %>%
  column_spec(2, color = "white",
              background = spec_color(c(unlist(gof_yield_df[,2])), option = "D", end = 0.8, direction = -1)) %>%
  column_spec(3, color = "white",
              background = spec_color(c(unlist(gof_yield_df[,3])), option = "D", end = 0.8, direction = -1)) %>%
  column_spec(4, color = "white",
              background = spec_color(c(unlist(gof_yield_df[,4])), option = "D", end = 0.8, direction = -1)) %>%
  column_spec(5, color = "white",
              background = spec_color(c(unlist(gof_yield_df[,5])), option = "D", end = 0.8, direction = -1)) %>%
  column_spec(6, color = "white",
              background = spec_color(c(unlist(gof_yield_df[,6])), option = "D", end = 0.8, direction = -1)) %>%
  pack_rows(muns[1], start_row=1, end_row=4, 
            color=spec_color(1, option = "A")) %>%
  pack_rows(muns[2], start_row=5, end_row=8,
            color=spec_color(1, option = "A")) %>%
  pack_rows(muns[3], start_row=9, end_row=12,
            color=spec_color(1, option = "A")) %>%
  pack_rows(muns[4], start_row=13, end_row=16,
            color=spec_color(1, option = "A")) %>%
  row_spec(slected_dist[1], bold = T, color = "black") %>%
  row_spec(slected_dist[2], bold = T, color = "black") %>%
  row_spec(slected_dist[3], bold = T, color = "black") %>%
  row_spec(slected_dist[4], bold = T, color = "black") %>%
  row_spec(0, bold = T, color = spec_color(1)) %>%
  column_spec(1, bold = T, color = spec_color(1))
# 
# save_kable(table_2, file = "table_2.html")
# webshot::webshot("table_2.html", "table_2.pdf")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

probs <- seq(0, 0.9999, length.out = 1000)

eps <- 0.3
a <- lapply(dt_yield, function(x) 0)
b <- lapply(dt_yield, function(x) ceiling(max(x) + eps * sd(x)))

n_mun <- length(muns)
n_dist <- length(dist_list)

q_Norm <- lapply(params_yield$norm, function(x) qnorm(probs, x["mean"], x["sd"]))
q_Gamma <- lapply(params_yield$gamma, function(x) qgamma(probs, x["shape"], x["rate"]))
q_Weibull <- lapply(params_yield$weibull, function(x) qweibull(probs, x["shape"], x["scale"]))

q_Beta <- list()
for (i in muns) {
  a_min <- a[[i]]
  b_max <- b[[i]]
  q_Beta[[i]] <- qshift_beta(probs, params_yield$shift_beta[[i]]["alpha"],
                             params_yield$shift_beta[[i]]["beta"],
                             a = a_min, b = b_max)
}

# q_Beta <- lapply(params_yield$shift_beta, function(x) qshift_beta(probs, x["alpha"], x["beta"],
#                                                                   a = a, b = max))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# --- Plot 3: Fitted Distributions ---

fit_list <- list()
plt_df <- list()
for (i in muns) {
  df <- data.frame(q_Norm[[i]], q_Gamma[[i]], q_Weibull[[i]], q_Beta[[i]])
  colnames(df) <- dist_names
  plt_df[[i]] <- df %>% reshape2::melt(value.name = "Yield")
  fit_list[[i]] <- df
}


# Histogram - comparing fitted density plots vs actual data
bw <- 0.5
p <- list()
for (mun in muns) {
  dt_y <- dt_yield[[mun]]
  p[[mun]] <- ggplot(data.frame(dt_y), aes(x = dt_y)) + 
    geom_histogram(aes(y = ..density..),
                   binwidth = bw,
                   color = "white") +
    geom_density(data = plt_df[[mun]], 
                 aes(
                   y = ..density.. / bw,
                   x = Yield,
                   color = variable,
                   fill = variable
                 ),
                 alpha = 0.4) +
    xlim(0, max(unlist(b))) +
    labs(title = mun,
         x = "Yield (Ton/Ha)",
         y = "Density") +
    my_theme +
    theme(legend.position = "bottom")
}

guide_area() + 
  (p$Ahome + p$Culiacán) / (p$Guasave + p$Navolato) +
  plot_layout(guides = "collect", 
              nrow = 2, heights = c(1,10)) + 
  plot_annotation(title = "Fitted Distributions by Municipality",
                  theme = theme(plot.title = element_text(family = "sans",
                                                          size = 20,
                                                          color = '#414141')))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# --------------------- 2. Price Modelling -------------------------------------

my_colors <- c(my_brick="#CC704B", my_lgreen="#9FC088", my_lbrown="#E8C07D",
               my_lblue="#8FBDD3", my_cyan="#5AA897", my_lred="#DD4A48")

row.names(corn_ts) <- corn_ts$Date
corn_ts <- corn_ts %>% dplyr::select(-c(Date))
settle_price <- corn_ts$Settle
lr <- diff(log(settle_price))[-1]
l <- length(lr)

# --- 3.1 Model fit using the "rugarch" package ---

# Create the model, specify the variance.model, the mean.model and the distribution.model
# We set the orders of our model as GARCH(1,1) and ARMA(1,1)

model <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                     mean.model = list(armaOrder = c(1, 1), include.mean = FALSE),
                     distribution.model = "std")

lr_df <- data.frame(lr)
row.names(lr_df) <- row.names(corn_ts)[-c(1:2)]

model@model$pars
# Estimation/fitting of the model
model_fit <- ugarchfit(spec = model, data = lr_df, solver = "solnp")
# Fitted Model
model_fit
# empirical density
plot(model_fit, which=8)

#par(bg = "transparent")
plot.garchfit.8(model_fit, size = 2)

# QQ-Plot
plot(model_fit, which=9)
plot.garchfit.9(model_fit, size = 2)

# VaR Limits
cl <- 0.95
alpha <- (1-cl)/2
nu <- coef(model_fit)["shape"]
t <- qt(alpha, nu, lower.tail = F)
u <- rev(model_fit@fit$sigma)
m <- 0

df_plt <- data.frame("log-returns" = lr_df$lr,
                     "VaR_0.975" = m + t * u,
                     "VaR_0.025" = m - t * u,
                     "Date" = as.Date(row.names(lr_df)))

df_melted <- reshape2::melt(df_plt, id = "Date")

l_size = 1.1
plt <- ggplot(df_melted) +
  geom_line(aes(x = Date, y = value, colour = variable, linetype = variable, size = variable)) +
  scale_color_manual(name = "", values = c("darkgrey", 3, 2)) +
  scale_linetype_manual(values = c("solid", "dashed", "dashed")) +
  scale_size_manual(values = c(l_size, l_size, l_size)) +
  labs(colour = "", linetype = "") +
  guides(size= "none", linetype = guide_legend(override.aes=list(colour=c("darkgrey", 3, 2),
                                                                 size = c(l_size,l_size,l_size)),
                                                                 linetype = c("solid", "dashed", "dashed"))) +
  scale_x_date(breaks=date_breaks("1 year"),
               labels=date_format("%Y")) +
  labs(title = paste0("Log-returns with VaR limits"),
       subtitle = paste0(cl*100, "% Confidence level"),
       x = "Date",
       y = "log-returns")

plt + my_theme +
  theme(legend.position = "bottom")

# estimated coefficients
coef(model_fit)

# Standardize residuals
std_res <- residuals(model_fit, standardize=TRUE)

nu <- coef(model_fit)["shape"]
sim_t <- rt(n, nu)

plot(model_fit, which="all")

# Actual residuals
res <- residuals(model_fit)


# --- 3.2 Simulating Paths ---

# Simulation
set.seed(123)
model_sim <- ugarchsim(model_fit, n.sim = days,
                       n.start = 1, m.sim = n,
                       startMethod = "sample")
#plot(model_sim)

# Simulated log-returns
lr_sim <- model_sim@simulation$seriesSim
# Last Price of the dataset
(p_0 <- corn_ts$Settle[1])
exp_lr <- exp(lr_sim)
x <- rbind(rep(p_0, n), exp_lr)
# Simulated Prices
p_t <- apply(x, 2, cumprod)

summary(p_t[days,])

# Plotting the paths
alpha <- 0.01
uq <- 1 - alpha/2
lq <- 1 - uq
uq_t <- apply(p_t, 1, function(x) quantile(x, uq))
lq_t <- apply(p_t, 1, function(x) quantile(x, lq))

# par(bg = "transparent")
# matplot(x = seq(0, days, length.out = nrow(p_t)), y = p_t,
#         type = "l", lty = 1, lwd = 1.5, col = 4:5,
#         main = "Simulated paths ARMA(1,1)-GARCH(1,1)",
#         xlab = "", ylab = "", xaxt = "n",
#         cex.main = 2, cex.axis = 1.5)
# lines(x = seq(0, days, length.out = nrow(p_t)), y = uq_t,
#       lty = 2, lwd = 4, col = "red")
# lines(x = seq(0, days, length.out = nrow(p_t)), y = lq_t,
#       lty = 2, lwd = 4, col = "red")
# legend("topleft",legend = c(paste0("quant_", uq), paste0("quant_", lq)),
#        cex = 1.5, col=c("red", "red"), lty = c(3,3), lwd = c(4,4))
# mtext("Days", 1, line = 3, cex = 1.8)
# mtext("Price (usd_cent/bushel)", 2, line = 2.5, cex = 1.8)
# axis(1, seq(0, days, 20), cex.axis = 1.5)


end_p <- p_t[days,]
end_avp <- apply(p_t[(days-30):days,], 2, mean)
plot_df <- data.frame("Final_Price" = end_p,
                      "Final_1m_avPrice" = end_avp) 

melt_df <- reshape2::melt(plot_df)
ggplot(melt_df) +
  geom_histogram(aes(y = ..density.., x = value, fill = variable),
                 position = "dodge", bins = 30) +
  labs(title = "End-Price vs Last Month Average Price") + 
  xlab("Price (usd_cent/bushel)") +
  my_theme


# Empirical Distribution of the simulated prices
plot(ecdf(end_avp))
emp_cdf <- ecdf(end_avp)
quantile(emp_cdf, 0.99)

# Quantiles of the empirical distribution
price_quantiles <- emp_cdf(end_avp)


# ----------------------- 3. Rank Correlation ----------------------------------

# --- 3.1 Empirical Correlations ---

# // Option 1: CME-based harvest price //
# (Prices in CME are in USD cents per bushel)

dates <- rownames(corn_ts)
harvest_price <- data.frame("year" = year(dates),
                            "month" = month(dates),
                            "day" = day(dates),
                            "price" = corn_ts$Settle)

# May and June are the months where almost all of the harvest happens
harvest_months <- c(5, 6)
harvest_price <- harvest_price %>%
  group_by(year) %>%
  filter(month %in% harvest_months) %>%
  summarise(av_price = mean(price)) %>%
  mutate(USD_tonne = av_price/100 * bush_tonne)

p <- harvest_price$USD_tonne

# Correlation of original vs detrended CME price series
l <- length(p)
t <- seq(1, l, 1)

t2 <- t^2
model <- lm(p ~ t + t2)
# model <- lm(p ~ t)
# In this case, t is not significant so we can use the price directly
summary(model)
price_dt <- model$fitted.values[l] * (1 + model$residuals / model$fitted.values)
plot(p, type="l")
lines(price_dt, lty = 2)

# Correlation
meth <- c("pearson", "kendall", "spearman")

# *** Series to use for the RANK CORRELATION ***
price_ts <- price_dt
# price_ts <- p

m <- muns[1]
yield_dt <- dt_yield[[m]]

# I this case we use the price directly
stat_rho <- unlist(lapply(meth, function(m) cor(yield_dt, price_ts, method = m)))
names(stat_rho) <- meth
stat_rho

# Data Frame with Historic and Rolling correlations
roll <- 10

df_rho <- as.data.frame(t(stat_rho))
df_rows <- matrix(rep(as.vector(stat_rho), l-roll), ncol = 3, byrow = TRUE)
colnames(df_rows) <- meth
df_rho <- rbind(df_rho, df_rows)

for (i in 1:(l-roll+1)) {
  rho <- unlist(lapply(meth, function(m) cor(yield_dt[i:(roll+i-1)],price_ts[i:(roll+i-1)], method = m)))
  df_rho <- rbind(df_rho, rho)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type <- c(rep("Historic", l-roll+1), rep("Rolling", l-roll+1))
df_rho <- cbind(df_rho, type)
Year <- rep(years[(l-roll+2):l], 2)
df_rho <- cbind(df_rho, Year)

df_plt <- reshape2::melt(df_rho, id = c("type", "Year"))
inf <- -1
sup <- max(df_plt$value, 0)

ggplot(df_plt, aes(x = Year, y = value,
                   colour = variable, linetype = type)) +
  geom_line(size = 1.2) +
  facet_grid(variable ~ .) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(labels = as.character(years[roll:l]), breaks = years[roll:l]) +
  scale_y_continuous(limits = c(inf, sup),
                     labels = as.character(c(round(seq(inf, sup, 0.2),1))),
                     breaks = c(seq(inf, sup, 0.2))) +
  labs(title = paste0("Correlation comparison ", m),
       subtitle = paste0(roll, "-year rolling correlation"),
       x = "Year",
       y = "Corr") +
  my_theme +
  theme(strip.text.y = element_text(size = 25)) +
  guides(colour = "none",
         linetype = guide_legend(override.aes=list(colour=c("black", "black"),
                                                   size = c(0.8,1)),
                                 linetype = c("solid", "dashed")))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# --------------------------- 4. CROPULAS --------------------------------------

# --- 4.1 Fit and Simulate Copulas ---

# Empirical CDF of the price
emp_cdf_p <- ecdf(end_avp)

# Lists of Copulas
cops <- list(
  
  # - Implicit Copulas -
  normalCopula(),
  tCopula(),
  
  # - Explicit Copulas -
  # other
  amhCopula(),
  frankCopula(),
  plackettCopula(),
  
  # nonnegative
  gumbelCopula(),
  claytonCopula(),
  joeCopula()
  
)


# --- 4.2 Fitting Criteria and Goodness-of-Fit ---

empcop_mun <- lapply(dt_yield, function(y) matrix(cbind(y, price_ts), ncol = 2))
obs_mun <- lapply(empcop_mun, pobs) 

# --- Perform chosen tests for chosen copulas ---

cores <- parallel::detectCores()
tests <- c("gofCvM", "gofKendallCvM", "gofKendallKS", "gofKS",
           "gofRosenblattSnB", "gofRosenblattSnC",
           "gofKernel")
copulae <- c("normal", "t", "amh", "frank", "plackett", "gumbel", "clayton", "joe")
rotation <- c(0, 0, 0, 0, 0, 270, 90, 270)

# Bootstraping loops gofCopula 
# M <- 1000
# MJ <- 1000 # boot for gofKernel

system.time({
fitted_cops_mun <- lapply(empcop_mun,
  function(mun) {
    fitted_copulas <- gofCopula::gof(mun, M = M, MJ = MJ,
                          processes = 6,
                          tests = tests,
                          copula = copulae,
                          flip = rotation)
    return(fitted_copulas)
  }
)  
})

lapply(fitted_cops_mun, function(mun) unlist(lapply(mun, function(c) c["theta"])))

# Pirate plot
lapply(muns, function(m) {
  
  pdf(file = paste0("plt_GOF_", m, ".pdf"),
      width = 12, height = 5,
      bg = "transparent",
      colormodel = "srgb", # (cmyk is required for most publications)
      paper = "special")
  plot(fitted_cops_mun[[m]], hybrid = c(1:7), main = m)
  dev.off()
  
  }
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# --- Table 4: GOF tests Copulas ---

statistics <- lapply(fitted_cops_mun,
  function(mun) {
    rbind(
      t(mun$normal$res.tests[1:(length(tests)),]),
      t(mun$t$res.tests[1:(length(tests)),]),
      t(mun$amh$res.tests[1:(length(tests)),]),
      t(mun$frank$res.tests[1:(length(tests)),]),
      t(mun$plackett$res.tests[1:(length(tests)),]),
      t(mun$clayton$res.tests[1:(length(tests)),]),
      t(mun$gumbel$res.tests[1:(length(tests)),]),
      t(mun$joe$res.tests[1:(length(tests)),])
    )  
  }
)

statistics <- lapply(statistics, function(x) round(x, 5))

theta <- lapply(fitted_cops_mun,
  function(mun) {
    unlist(lapply(mun,
      function(cop) {
        c(cop["theta"],
          ifelse(is.null(unlist(cop["df"])[1]), "", unlist(cop["df"])[1]))
      }
    ))  
  }
)

Parameters <- lapply(theta, function(mun) round(as.numeric(mun), 4))
tbl_df <- lapply(muns, function(mun) cbind(statistics[[mun]], Parameters[[mun]]))
names(tbl_df) <- muns

for (mun in muns) {
  
  table_4 <- tbl_df[[mun]] %>%
    kbl(caption = "<span style='font-size:400%'><center>Table 4:Goodness-of-Fit Tests</center></span>",
        booktabs = T, linesep = "", align = "c") %>%
    kable_styling(latex_options = c("striped", "hold_position"), font_size = 50) %>%
    add_header_above(c("Copula", "Test" = 7, "Theta / df" = 1),
                     color = spec_color(1, option = "A"), bold = T) %>%
    column_spec(9, width = "6cm")%>%
    kable_paper(full_width = T) %>%
    row_spec(0, bold = T, color = spec_color(1)) %>%
    pack_rows("Normal", start_row=1, end_row=2, 
              color=spec_color(1, option = "A")) %>%
    pack_rows("t", start_row=3, end_row=4,
              color=spec_color(1, option = "A")) %>%
    pack_rows("AMH", start_row=5, end_row=6,
              color=spec_color(1, option = "A")) %>%
    pack_rows("Frank", start_row=7, end_row=8,
              color=spec_color(1, option = "A")) %>%
    pack_rows("Plackett", start_row=9, end_row=10,
              color=spec_color(1, option = "A")) %>%
    pack_rows("Gumbel", start_row=11, end_row=12,
              color=spec_color(1, option = "A")) %>%
    pack_rows("Clayton", start_row=13, end_row=14,
              color=spec_color(1, option = "A")) %>%
    pack_rows("Joe", start_row=15, end_row=16,
              color=spec_color(1, option = "A")) %>%
    column_spec(1, bold = T, color = spec_color(1)) %>%
    column_spec(2, color = "white",
                background = spec_color(c(unlist(statistics[[mun]][,1])), option = "D", end = 0.8, direction = -1)) %>%
    column_spec(3, color = "white",
                background = spec_color(c(unlist(statistics[[mun]][,2])), option = "D", end = 0.8, direction = -1)) %>%
    column_spec(4, color = "white",
                background = spec_color(c(unlist(statistics[[mun]][,3])), option = "D", end = 0.8, direction = -1)) %>%
    column_spec(5, color = "white",
                background = spec_color(c(unlist(statistics[[mun]][,4])), option = "D", end = 0.8, direction = -1)) %>%
    column_spec(6, color = "white",
                background = spec_color(c(unlist(statistics[[mun]][,5])), option = "D", end = 0.8, direction = -1)) %>%
    column_spec(7, color = "white",
                background = spec_color(c(unlist(statistics[[mun]][,6])), option = "D", end = 0.8, direction = -1)) %>%
    column_spec(8, color = "white",
                background = spec_color(c(unlist(statistics[[mun]][,7])), option = "D", end = 0.8, direction = -1))
  
  f <- paste0("gofCopulas", chartr("áéíóú", "aeiou", mun), ".html")
  f_pdf <- paste0("gofCopulas", chartr("áéíóú", "aeiou", mun), ".pdf")
  save_kable(table_4, file = f)
  webshot::webshot(f, f_pdf)
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Replacing the thetas for the fitted ones

unlist(lapply(cops, function(c) attr(c, "parameters")))
unlist(lapply(fitted_copulas, function(c) c["theta"]))

mun_cops <- lapply(fitted_cops_mun,
  function(mun_fit) {
    for (i in 1:length(cops)) {
      theta <- unlist(mun_fit[[i]]["theta"])
      attr(cops[[i]], "parameters")[1] <- theta
    }
    return(cops)
  }
)

for (i in 1:length(cops)) {
  theta <- unlist(fitted_copulas[[i]]["theta"])
  attr(cops[[i]], "parameters")[1] <- theta
}

# Assign degrees of freedom to t-dist

deegf <- lapply(fitted_cops_mun, function(mun_fit) unlist(mun_fit$t["df"])[1])
df <- unlist(fitted_copulas[[2]]["df"])[1]

for (i in muns) {
  attr(mun_cops[[i]][[2]], "parameters")[2] <- deegf[[i]]  
}


# Dimensions (currently unused)
d <- 2

# Rank Correlation
tau <- stat_rho[2]
# # Fit via Kandalls Tau
# cops <- lapply(cops, function(c) fit_cop(c, tau = tau))

# Parameters of selected yield distribution per municipality
params_mun <- list()
for (i in 1:n_mun) {
  params_mun[muns[i]] <- list(params_yield[[selected_dist_df$R_name[i]]][[selected_dist_df$mun[i]]])
  if (selected_distributions[muns[i]] == "Beta") {
    params_mun[muns[i]] <- Map(c, params_mun[muns[i]], "a" = a[muns[i]], "b" = b[muns[i]]) 
  }
}

(parse(text = paste0("q", selected_dist_df$R_name, "(", paste(0.55, params_mun, sep = ","), ")")))


# --- Simulation ---
set.seed(123)

# Simulate all fitted copulas for each Municipality
sim_mun_cops <- lapply(mun_cops, function(cops) {
  lapply(cops, function(c) simulate_cop(n, c, tau = tau))
})

# Fix list names
cop_names_by_mun <- lapply(mun_cops, function(cops) unlist(lapply(cops, function(c) class(c)[1])))
for (m in muns) names(sim_mun_cops[[m]]) <- cop_names_by_mun[[m]]

# List of dataframes with all simulated copulas (per Municipality)
mun_copsim_df <- lapply(sim_mun_cops, data.frame)

even_cols <- seq(2, ncol(mun_copsim_df[[1]]), 2)
# Price cols (v <-> X)
mun_v_df <- lapply(mun_copsim_df, function(df) df[, even_cols])
# Yield cols (u <-> Y)
mun_u_df <- lapply(mun_copsim_df, function(df) df[, -c(even_cols)])


# --- Prices and Yields from simulated copulas ---

sim_mun_prices <- lapply(mun_v_df, function(mun_df) {
  apply(mun_df, 2, function(v) quantile(emp_cdf_p, v))
})


# Get quantiles from simulated copula for the respective marginals
sim_mun_yields <- vector("list", length = n_mun)
names(sim_mun_yields) <- muns
for (m in muns) {
  cat("Estimating Yields for: ", m) 
  print(system.time({
    
    sim_mun_yields[[m]] <- apply(mun_u_df[[m]], 2, function(u) {
      
      unlist(
        lapply(parse(text = paste0(
        paste0("q", selected_dist_df$R_name[selected_dist_df$mun == m],
               "(",
               u,
               ",",
               paste(params_mun[[m]], collapse = ","),
               ")")
        )), eval)
      )    
    
    })  
    
  }))
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# --- Density and Hexbin Plots ---

i <- 1
plt <- scatter_hist_2d(mun_u_df[[4]][,i], mun_v_df[[4]][,i], type = "density")
plt + labs(title = copulae[i],
           subtitle = paste0("Theta = ", round(as.numeric(theta[2*i-1]),4)))

plt <- scatter_hist_2d(sim_mun_yields[[3]][,i], sim_mun_prices[[3]][,i], type = "hexbin")
plt

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# --------------------------- 5. Pricing ---------------------------------------

# Note: make sure the settle_price is in the correct order
t0 <- length(settle_price)
coverage_levels <- list("55%" = 0.55,
                        "65%" = 0.65,
                        "75%" = 0.75,
                        "85%" = 0.85,
                        "100%" = 1)
# last five years for APH
years <- 5
y_g <- mean(tail(yield_dt, years))
mun_y_g <- apply(dt_yield, 2, function(m) mean(tail(m, years)))

# --- Guaranteed Yield (y_g) ---

mun_Y0 <- lapply(mun_y_g, function(y_g) vapply(coverage_levels, function(cl) y_g * cl, numeric(1)))

Y0 <- vapply(coverage_levels, function(cl) y_g * cl, numeric(1))

# --- Guaranteed Price (x_g) ---
corn_ts["Date"] <- as.Date(row.names(corn_ts), "%Y-%m-%d")
ini_date <- as.Date("2020-12-01")
end_date <- as.Date("2020-12-31")
sow_month_prices <- filter(corn_ts, Date >= ini_date, Date <= end_date) %>%
  .$Settle
x_g <- mean(sow_month_prices)

# --- Optimal Premium ---

mun_risk_prem <- lapply(muns, function(m) {
  risk_premium(coverage_levels,  mun_y_g[m], x_g, sim_mun_yields[[m]], sim_mun_prices[[m]])
})
names(mun_risk_prem) <- muns

risk_prem <- risk_premium(coverage_levels, y_g, x_g, sim_yields, sim_prices)

# --- Comparision assuming independent yield and price  ---

sim_mun_yield_indep <- vector("list", length = n_mun)
names(sim_mun_yield_indep) <- muns
for (m in muns) {
  cat("Estimating Independent Revenue for: ", m) 
  print(system.time({
    
    sim_mun_yield_indep[[m]] <- unlist(
      
        lapply(parse(text = paste0(
          paste0("r", selected_dist_df$R_name[selected_dist_df$mun == m],
                 "(",
                 n,
                 ",",
                 paste(params_mun[[m]], collapse = ","),
                 ")")
        )), eval)
    )
    
  }))
}

indep_rp <- risk_premium(coverage_levels, y_g, x_g, data.frame(sim_BetaShift), data.frame(end_avp))

mun_indep_rp <- lapply(muns, function(m) {
  risk_premium(coverage_levels, mun_y_g[m], x_g, data.frame(sim_mun_yield_indep[[m]]), data.frame(end_avp))
})
names(mun_indep_rp) <- muns

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# --- Table with results ---

mun_risk_prem <- lapply(muns, function(m) round(rbind(mun_risk_prem[[m]], mun_indep_rp[[m]]) * 100, 3)) 
names(mun_risk_prem) <- muns

cop_names <- c("Normal", "t", "AMH", "Frank", "Plackett", "Gumbel", "Clayton", "Joe")
# mun <- muns[4]

for (mun in muns) {
  
  risk_prem_df <- data.frame(mun_risk_prem[[mun]], row.names = c(cop_names, "Independent"))
  colnames(risk_prem_df) <- paste0(" ", names(coverage_levels), " ")
  
  table_5 <- risk_prem_df %>%
    kbl(caption = "<span style='font-size:400%'><center>Table 5:Risk Premium for fitted Copula Models</center></span>",
        booktabs = T, linesep = "", align = "c") %>%
    kable_styling(latex_options = c("striped", "hold_position"), font_size = 50) %>%
    add_header_above(c("", "Premium (%) for Coverage Levels" = 5),
                     color = spec_color(1, option = "A"), bold = T) %>%
    kable_paper(full_width = T) %>%
    column_spec(1, bold = T, color = spec_color(1)) %>%
    row_spec(0, bold = T, color = spec_color(1)) %>%
    pack_rows("Copula", start_row=1, end_row=8,
              color=spec_color(1, option = "A")) %>%
    pack_rows("Indep Margins", start_row=9, end_row=9,
              color=spec_color(1, option = "A")) %>%
    column_spec(2, color = "white",
                background = spec_color(c(unlist(risk_prem[,1])), option = "D", end = 0.8, direction = -1)) %>%
    column_spec(3, color = "white",
                background = spec_color(c(unlist(risk_prem[,2])), option = "D", end = 0.8, direction = -1)) %>%
    column_spec(4, color = "white",
                background = spec_color(c(unlist(risk_prem[,3])), option = "D", end = 0.8, direction = -1)) %>%
    column_spec(5, color = "white",
                background = spec_color(c(unlist(risk_prem[,4])), option = "D", end = 0.8, direction = -1)) %>%
    column_spec(6, color = "white",
                background = spec_color(c(unlist(risk_prem[,5])), option = "D", end = 0.8, direction = -1))
  
  f <- paste0("risk_prem", chartr("áéíóú", "aeiou", mun), ".html")
  f_pdf <- paste0("risk_prem", chartr("áéíóú", "aeiou", mun), ".pdf")
  save_kable(table_5, file = f)
  webshot::webshot(f, f_pdf)

}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
# --- Risk Measures ---

risk_levels <- list(0.95, 0.99, 0.995, 0.999, 0.9995)

# --- Comparison assuming independent yield and price  ---
# (Make sure the number of simulations is the same)

all_mun_sim_y <- lapply(muns, function(m) {
  cbind(sim_mun_yields[[m]], sim_mun_yield_indep[[m]])
})
all_mun_sim_p <- lapply(muns, function(m) {
  cbind(sim_mun_prices[[m]], end_avp)
})
names(all_mun_sim_y) <- names(all_mun_sim_p) <- muns

mun_qrm <- lapply(muns, function(m) {
  risk_measures(mun_y_g[m], x_g, all_mun_sim_y[[m]], all_mun_sim_p[[m]], risk_levels)
})
names(mun_qrm) <- muns


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tbl_6 <- lapply(mun_qrm, function(m) round(cbind(m$VaR, m$ES) * 100, 2))

for (mun in muns) {
  
  rownames(tbl_6[[mun]]) <- c(cop_names, "Independent")
  table_6 <- tbl_6[[mun]] %>%
    kbl(caption = "<span style='font-size:400%'><center>Table 6:Risk Measures for fitted Copula Models</center></span>",
        booktabs = T, linesep = "", align = "c") %>%
    kable_styling(latex_options = c("striped", "hold_position"), font_size = 50) %>%
    add_header_above(c("", "Value at Risk (%)" = 5, "Expected Shortfall (%)" = 5),
                     color = spec_color(1, option = "A"), bold = T) %>%
    kable_paper(full_width = T) %>%
    column_spec(1, bold = T, color = spec_color(1)) %>%
    row_spec(0, bold = T, color = spec_color(1)) %>%
    pack_rows("Copula", start_row=1, end_row=8,
              color=spec_color(1, option = "A")) %>%
    pack_rows("Indep Margins", start_row=9, end_row=9,
              color=spec_color(1, option = "A")) %>%
    column_spec(2, color = "white",
                background = spec_color(c(unlist(tbl_6[[mun]][,1])), option = "D", end = 0.8, direction = -1)) %>%
    column_spec(3, color = "white",
                background = spec_color(c(unlist(tbl_6[[mun]][,2])), option = "D", end = 0.8, direction = -1)) %>%
    column_spec(4, color = "white",
                background = spec_color(c(unlist(tbl_6[[mun]][,3])), option = "D", end = 0.8, direction = -1)) %>%
    column_spec(5, color = "white",
                background = spec_color(c(unlist(tbl_6[[mun]][,4])), option = "D", end = 0.8, direction = -1)) %>%
    column_spec(6, color = "white",
                background = spec_color(c(unlist(tbl_6[[mun]][,5])), option = "D", end = 0.8, direction = -1)) %>%
    column_spec(7, color = "white",
                background = spec_color(c(unlist(tbl_6[[mun]][,6])), option = "D", end = 0.8, direction = -1)) %>%
    column_spec(8, color = "white",
                background = spec_color(c(unlist(tbl_6[[mun]][,7])), option = "D", end = 0.8, direction = -1)) %>%
    column_spec(9, color = "white",
                background = spec_color(c(unlist(tbl_6[[mun]][,8])), option = "D", end = 0.8, direction = -1)) %>%
    column_spec(10, color = "white",
                background = spec_color(c(unlist(tbl_6[[mun]][,9])), option = "D", end = 0.8, direction = -1)) %>%
    column_spec(11, color = "white",
                background = spec_color(c(unlist(tbl_6[[mun]][,10])), option = "D", end = 0.8, direction = -1))
  
  f <- paste0("qrm", chartr("áéíóú", "aeiou", mun), ".html")
  f_pdf <- paste0("qrm", chartr("áéíóú", "aeiou", mun), ".pdf")
  save_kable(table_6, file = f)
  webshot::webshot(f, f_pdf)
  
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# --- Loaded Premium ---

# # Standard Deviation
# (sd_indem <- apply(indem, 2, sd))
# (sd_PR <- sd_indem / guaranteed_revenue)
# 
# alpha <- 0.01
# ret_period <- 1 / alpha
# f <- function(x, cl = 1-alpha) quantile(x, cl)
# VaR <- apply(indem, 2, f) / guaranteed_revenue
# 
# # Expenses Adjustment (management, reinsurance, utility)
# expenses <- 0.1 + 0.15 + 0.05
# # Market Rate Estimate (VaR and SD method)
# (rates <- (PR + pmax(VaR - PR, 0) / ret_period)) 
# rates / (1-expenses)
# 
# indem_cdf <- apply(indem, 2, ecdf)
# # return period of the eps * sd
# epsilon <- 3
# VaR_equiv <- lapply(1:length(indem_cdf), function(x) indem_cdf[[x]](avg_indem[x] + epsilon * sd_indem[x]))
# ret_period2 <- unlist(lapply(VaR_equiv, function(x) 1 / (1-x)))
# (rates2 <- (PR + epsilon * sd_PR / ret_period2)) 
# rates2 / (1-expenses)
# 


