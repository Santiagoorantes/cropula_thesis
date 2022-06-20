
# --------------------------- 0. Loading ---------------------------------------

# --- 0.1 Load required packages ---
source("global.R")

# --- 0.2 Load data ---
source("data.R")

# --- 0.3 Load Utilities and Modules ---
folders <- c("utilities", "modules")
file_sources <- list.files(folders, pattern = "\\.R$", full.names = TRUE)
sapply(file_sources, source, .GlobalEnv)


# ------------------------- 1. Yield Distribution ------------------------------

# --- 1.0 Applying filters ---

# Define filter
my_filter <-  list(
  crop = c("Maíz grano"),
  type = c("Riego"),
  cycle = c("Otoño-Invierno"),
  state = c("Sinaloa"),
  municipality = c("Guasave")
)

# Yield per year given filter and group_by
filtered_df <- filter_df(agr_df, my_filter)

group_vars <- c("year", names(my_filter))
trgt_df <- group_agr_df(filtered_df, group_vars)

# --- 1.1 Detrending the series ---

# NOTE: make sure that there is only ONE value per year to detrend the series
nrow(trgt_df) == length(unique(trgt_df$year))

yield <- as.numeric(trgt_df$av_yield)
l <- length(yield)
t <- seq(1, l, 1)

# The simple linear regression for the yield was sufficient,
# anything of higher order was not significant.

# y_t = alpha + beta*T + e_t
yield_model <- glm(yield ~ t)
summary(yield_model)
acf(yield_model$residuals)

coefficients(summary(yield_model)) %>%
  kbl(caption = "<span style='font-size:600%'><center>Table 1:Linear Model for the Yield</center></span>",
      booktabs = T, linesep = "", align = "c") %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                font_size = 75) %>%
  kable_paper(full_width = T) %>%
  row_spec(0, bold = T, color = spec_color(1))

#save_kable(table1, file = "Table_1_lmyield.html")
#webshot("Table_1_lmyield.html", "Table_1_lmyield.pdf")


# y_bar_t = y_hat_2020 * (1 + e_hat_t / y_hat_t )
yield_dt <- yield_model$fitted.values[l] * (1 + yield_model$residuals / yield_model$fitted.values)

# Using Ljung-Box test to check for check for temporal dependency
# H0: The residuals are independently distributed.
# HA: The residuals are not independently distributed; they exhibit serial correlation.

# Temporal dependency
Box.test(yield, lag = 1, type = "Ljung")
Box.test(yield_dt, lag = 1, type = "Ljung")

# Homoscedasticity of variances
Box.test(yield^2, lag = 1, type = "Ljung")
Box.test(yield_dt^2, lag = 1, type = "Ljung")

# Yield Plot
years <- trgt_df$year
y_df <- data.frame(years, yield, yield_dt)
df_melted <- reshape2::melt(y_df, id = "years")
y <- df_melted$value
y_color <- "#bc5090"
y_plot <- ggplot(df_melted) +
  geom_line(aes(x = years, y = value, linetype = variable),
            colour = y_color,
            size = 1.2) +
  labs(title = "Corn Yield Series in Ahome",
       subtitle = "Original vs Detrended Series",
       x = "Year",
       y = "Yield (Ton/Ha)") +
  scale_x_continuous(labels = as.character(years), breaks = years) +
  scale_y_continuous(limits = c(floor(min(y)),ceiling(max(y))),
                     breaks = c(seq(floor(min(y)), ceiling(max(y)), 1))) +
  my_theme
y_plot


# --- 1.2 Fitting distributions to the YIELD ---

# Pearson (based on Cullen and Frey) plots
pearson_plot(yield_dt, boot = 1000, method = "sample", alpha = 0.7)
# ggsave("pearson_plot.pdf",
#        bg = "transparent",
#        height = 20,
#        width = 30,
#        units = "cm",
#        device = cairo_pdf)


# Normal distribution
fit_norm <- fitdist(yield_dt, distr = "norm", method = "mle")

# Gamma distribution
fit_Gam <- fitdist(yield_dt, distr = "gamma", method = "mle")

# Weibull distribution
fit_Wei <- fitdist(yield_dt, distr = "weibull", method = "mle")

# Beta distribution
a <- 0
# Should the additional parameter "b" be taken into account in the AIC and BIC?
# Check how different levels of epsilon change the fit and the price of the insurance
# Sensitivity wrt eps
eps <- 0.3
b <- ceiling(max(yield_dt) + eps * sd(yield_dt))
b <- ceiling(max(yield_dt))

fit_BetaShift <- fitdist(yield_dt, distr = "shift_beta",
                       start = list(alpha = 10, beta = 1),
                       fix.arg = list(a = a, b = b),
                       method = c("mle"))

fit_Beta_KS <- fitdist(yield_dt, distr = "shift_beta",
                       start = list(alpha = 10, beta = 1),
                       fix.arg = list(a = a, b = b),
                       method = "mge", gof = "KS")

fit_Beta_AD <- fitdist(yield_dt, distr = "shift_beta",
                       start = list(alpha = 10, beta = 1),
                       fix.arg = list(a = a, b = b),
                       method = "mge", gof = "AD")

# --- 2.3 Comparing fitted distributions ---

# Goodness of fit statistics and criteria
fitted_dists <- list(fit_norm, fit_Gam, fit_Wei, fit_BetaShift)
gof <- gofstat(fitted_dists)

# DataFrame of Goodness-of-fit
(gof_df <- data.frame("KS" = gof$ks,
                     "CvM" = gof$cvm,
                     "AD" = gof$ad,
                     "AIC" = gof$aic,
                     "BIC" = gof$bic))

parameters <- round(unlist(lapply(fitted_dists, coef)), 4)
param_df <- matrix(parameters, ncol = 2, byrow = TRUE)

tbl_df <- cbind(gof_df, param_df)

table_2 <- tbl_df %>%
  kbl(caption = "<span style='font-size:600%'><center>Table 2:Selection criteria for fitted distributions</center></span>",
      booktabs = T, linesep = "", align = "c") %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                font_size = 75) %>%
  kable_paper(full_width = T) %>%
  add_header_above(c("Distribution", "Criteria / GoF Test" = 5, "Parameters" = 2),
                   color = spec_color(1, option = "A"), bold = T) %>%
  row_spec(0, bold = T, color = spec_color(1)) %>%
  row_spec(nrow(gof_df), bold = T, color = "black") %>%
  column_spec(1, bold = T, color = spec_color(1)) %>%
  column_spec(2, color = "white",
              background = spec_color(c(unlist(gof_df[,1])), option = "D", end = 0.8, direction = -1)) %>%
  column_spec(3, color = "white",
              background = spec_color(c(unlist(gof_df[,2])), option = "D", end = 0.8, direction = -1)) %>%
  column_spec(4, color = "white",
              background = spec_color(c(unlist(gof_df[,3])), option = "D", end = 0.8, direction = -1)) %>%
  column_spec(5, color = "white",
              background = spec_color(c(unlist(gof_df[,4])), option = "D", end = 0.8, direction = -1)) %>%
  column_spec(6, color = "white",
              background = spec_color(c(unlist(gof_df[,5])), option = "D", end = 0.8, direction = -1))
# 
# save_kable(table_2, file = "table_2.html")
# webshot::webshot("table_2.html", "table_2.pdf")

full_names <- c("Kolmogorov-Smirnov statistic", "Cramer-von Mises statistic",
                  "Anderson-Darling statistic", "Akaike's Information Criterion",
                  "Bayesian Information Criterion")

probs <- seq(0, 0.9999, length.out = 1000)
yield_Norm <- qnorm(probs, fit_norm$estimate[1], fit_norm$estimate[2])
yield_Gamma <- qgamma(probs, fit_Gam$estimate[1], fit_Gam$estimate[2])
yield_Weibull <- qweibull(probs, fit_Wei$estimate[1], fit_Wei$estimate[2])
yield_Beta <- qshift_beta(probs, fit_BetaShift$estimate[1], fit_BetaShift$estimate[2],
                               a = a, b = b)

sim_Norm <- rnorm(n, mean=fit_norm$estimate[1], sd=fit_norm$estimate[2])
sim_Gamma <- rgamma(n, shape=fit_Gam$estimate[1], rate=fit_Gam$estimate[2])
sim_Weibull <- rweibull(n, shape=fit_Wei$estimate[1], scale=fit_Wei$estimate[2])
sim_BetaShift <- rshift_beta(n, alpha=fit_BetaShift$estimate[1], beta=fit_BetaShift$estimate[2],
                             a = a, b = b)

fit_df <- data.frame(yield_Norm, yield_Gamma, yield_Weibull, yield_Beta)
fit_df <- fit_df %>% reshape2::melt(value.name = "yield_fit")

# Histogram - comparing fitted density plots vs actual data
bw <- 0.5
p <- ggplot(data.frame(yield_dt), aes(x = yield_dt)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = bw,
                 color = "white") +
  geom_density(data = fit_df, 
               aes(
                 y = ..density.. / bw,
                 x = yield_fit,
                 color = variable,
                 fill = variable
               ),
               alpha = 0.4) +
  xlim(0, b + 1) +
  labs(title = "Fitted Distributions",
       x = "Yield (Ton/Ha)",
       y = "Density")

p + my_theme +
  theme(legend.position = "bottom")
  

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
sim_t <- rt(10000, nu)

plot(model_fit, which="all")

# Actual residuals
res <- residuals(model_fit)


# --- 3.2 Simulating Paths ---

# Simulation
days <- 180
nsim <- 100000
set.seed(123)
model_sim <- ugarchsim(model_fit, n.sim = days,
                       n.start = 1, m.sim = nsim,
                       startMethod = "sample")
#plot(model_sim)

# Simulated log-returns
lr_sim <- model_sim@simulation$seriesSim
# Last Price of the dataset
(p_0 <- corn_ts$Settle[1])
exp_lr <- exp(lr_sim)
x <- rbind(rep(p_0, nsim), exp_lr)
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

bush_tonne <- 39.368

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
model <- lm(p ~ t)
# In this case, t is not significant so we can use the price directly
summary(model)
price_dt <- model$fitted.values[l] * (1 + model$residuals / model$fitted.values)
plot(p, type="l")
lines(price_dt, lty = 2)

# Correlation
meth <- c("pearson", "kendall", "spearman")

# *** Series to use for the RANK CORRELATION ***
price_ts <- p

# I this case we use the price directly
stat_rho <- unlist(lapply(meth, function(m) cor(yield_dt, price_ts, method = m)))
names(stat_rho) <- meth
stat_rho

# Data Frame with Historic and Rolling correlations
years <- trgt_df$year
roll <- 10

df_rho <- as.data.frame(t(stat_rho))
df_rows <- matrix(rep(as.vector(stat_rho), l-roll), ncol = 3, byrow = TRUE)
colnames(df_rows) <- meth
df_rho <- rbind(df_rho, df_rows)

for (i in 1:(l-roll+1)) {
  rho <- unlist(lapply(meth, function(m) cor(yield_dt[i:(roll+i-1)],price_ts[i:(roll+i-1)], method = m)))
  df_rho <- rbind(df_rho, rho)
}

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
  labs(title = "Correlation comparison",
       subtitle = paste0(roll, "-year rolling correlation"),
       x = "Year",
       y = "Corr") +
  my_theme +
  theme(strip.text.y = element_text(size = 25)) +
  guides(colour = "none",
         linetype = guide_legend(override.aes=list(colour=c("black", "black"),
                                                   size = c(0.8,1)),
                                 linetype = c("solid", "dashed")))


# // OPTION 2 //
# --- Working with the residuals of the ARMA-GARCH ---
# 
# dates <- date(res)
# price_res <- data.frame(
#   "year" = year(dates),
#   "month" = month(dates),
#   "day" = day(dates),
#   "residual" = res
# )
# 
# # May and June are the months where almost all of the harvest happens
# harvest_months <- c(5,6)
# price_res <- price_res %>%
#   group_by(year) %>%
#   filter(month %in% harvest_months) %>%
#   summarise(av_res = mean(residual))
# 
# trgt_res <- price_res$av_res
# plot(trgt_res, type="l")
# 
# 
# price_ts <- trgt_res
# 
# # I this case we use the price directly
# stat_rho <- unlist(lapply(meth, function(m) cor(yield_dt, price_ts, method = m)))
# names(stat_rho) <- meth
# stat_rho
# 
# # Data Frame with Historic and Rolling correlations
# years <- trgt_df$year
# roll <- 10
# 
# df_rho <- as.data.frame(t(stat_rho))
# df_rows <- matrix(rep(as.vector(stat_rho), l-roll), ncol = 3, byrow = TRUE)
# colnames(df_rows) <- meth
# df_rho <- rbind(df_rho, df_rows)
# 
# for (i in 1:(l-roll+1)) {
#   rho <- unlist(lapply(meth, function(m) cor(yield_dt[i:(roll+i-1)],price_ts[i:(roll+i-1)], method = m)))
#   df_rho <- rbind(df_rho, rho)
# }
# 
# type <- c(rep("Historic", l-roll+1), rep("Rolling", l-roll+1))
# df_rho <- cbind(df_rho, type)
# Year <- rep(years[(l-roll+2):l], 2)
# df_rho <- cbind(df_rho, Year)
# 
# df_plt <- reshape2::melt(df_rho, id = c("type", "Year"))
# inf <- -1
# sup <- max(df_plt$value, 0)
# 
# ggplot(df_plt, aes(x = Year, y = value,
#                    colour = variable, linetype = type)) +
#   geom_line(size = 1.2) +
#   facet_grid(variable ~ .) +
#   scale_linetype_manual(values = c("dashed", "solid")) +
#   scale_x_continuous(labels = as.character(years[roll:l]), breaks = years[roll:l]) +
#   scale_y_continuous(limits = c(inf, sup),
#                      labels = as.character(c(round(seq(inf, sup, 0.2),1))),
#                      breaks = c(seq(inf, sup, 0.2))) +
#   labs(title = "Correlation comparison",
#        subtitle = paste0(roll, "-year rolling correlation"),
#        x = "Year",
#        y = "Corr") +
#   my_theme +
#   theme(strip.text.y = element_text(size = 25)) +
#   guides(colour = "none",
#          linetype = guide_legend(override.aes=list(colour=c("black", "black"),
#                                                   size = c(1,1)),
#                                                   linetype = c("solid", "dashed")))


# --------------------------- 4. CROPULAS --------------------------------------

# --- 4.1 Fit and Simulate Copulas ---

# Empirical CDF of the price
emp_cdf_p <- ecdf(end_avp)

# Lists of Copulas
cops <- list(
  
  # - Explicit Copulas -
  
  normalCopula(),
  tCopula(),
  
  # - Archimedean Copulas -
  
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

# Empical Copula
empcop <- matrix(cbind(yield_dt, price_ts), ncol = 2)
obs  <- pobs(empcop)

# Performs chosen tests for chosen copulas
# Using 6 cores: takes approx 390 seconds to run M = 1000, MJ = 1000.
# (~ 6.5 min)
# Using 12 cores: takes approx 470 seconds to run M = 1000, MJ = 1000.
# (~8 min)
library("gofCopula")
library(parallel)

cores <- parallel::detectCores()
tests <- c("gofCvM", "gofKendallCvM", "gofKendallKS", "gofKS",
           "gofRosenblattSnB", "gofRosenblattSnC",
           "gofKernel")
copulae <- c("normal", "t", "amh", "frank", "plackett")

empcop <- matrix(cbind(yield_dt, price_ts), ncol = 2)
system.time({
  test_copulas <- gof(empcop, M = 1500, MJ = 1000,
                       processes = 6,
                       tests = tests,
                       copula = copulae,)
})

unlist(lapply(test_copulas, function(c) c["theta"]))

# Pirate plot
plot(test_copulas, hybrid = c(1:4))

empcop <- matrix(cbind(yield_dt*(-1), price_ts), ncol = 2)
nonneg_copulae <- c("gumbel", "clayton", "joe")
system.time({
  test_nonegcopulas <- gof(empcop, M = 1500, MJ = 1000,
                           processes = 6,
                           tests = tests,
                           copula = nonneg_copulae)
})

par(bg = "transparent")
plot(test_nonegcopulas, hybrid = 1:3)



all_cops <- c(test_copulas, test_nonegcopulas)
unlist(lapply(all_cops, function(c) c["theta"]))

statistics <- rbind(
  t(all_cops$normal$res.tests[1:(length(tests)),]),
  t(all_cops$t$res.tests[1:(length(tests)),]),
  t(all_cops$amh$res.tests[1:(length(tests)),]),
  t(all_cops$frank$res.tests[1:(length(tests)),]),
  t(all_cops$plackett$res.tests[1:(length(tests)),]),
  t(all_cops$clayton$res.tests[1:(length(tests)),]),
  t(all_cops$gumbel$res.tests[1:(length(tests)),]),
  t(all_cops$joe$res.tests[1:(length(tests)),])
)

theta <- unlist(lapply(all_cops, function(cop) {
  c(cop["theta"], ifelse(is.null(unlist(cop["df"])[1]), "", unlist(cop["df"])[1]))
  }))

Parameters <- round(as.numeric(theta), 4)
tbl_df <- cbind(statistics,Parameters)


table_4 <- tbl_df %>%
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
              background = spec_color(c(unlist(statistics[,1])), option = "D", end = 0.8, direction = -1)) %>%
  column_spec(3, color = "white",
              background = spec_color(c(unlist(statistics[,2])), option = "D", end = 0.8, direction = -1)) %>%
  column_spec(4, color = "white",
              background = spec_color(c(unlist(statistics[,3])), option = "D", end = 0.8, direction = -1)) %>%
  column_spec(5, color = "white",
              background = spec_color(c(unlist(statistics[,4])), option = "D", end = 0.8, direction = -1)) %>%
  column_spec(6, color = "white",
              background = spec_color(c(unlist(statistics[,5])), option = "D", end = 0.8, direction = -1)) %>%
  column_spec(7, color = "white",
              background = spec_color(c(unlist(statistics[,6])), option = "D", end = 0.8, direction = -1)) %>%
  column_spec(8, color = "white",
              background = spec_color(c(unlist(statistics[,7])), option = "D", end = 0.8, direction = -1))

# save_kable(table_4, file = "gofCopulas.html")
# webshot::webshot("gofCopulas.html", "gofCopulas.pdf")
# 

# Replacing the thetas for the fitted ones

unlist(lapply(cops, function(c) attr(c, "parameters")))
unlist(lapply(all_cops, function(c) c["theta"]))

for (i in 1:length(cops)) {
  theta <- unlist(all_cops[[i]]["theta"])
  attr(cops[[i]], "parameters")[1] <- theta
}

# Assign degrees of freedom to t-dist
df <- unlist(all_cops[[2]]["df"])[1]
attr(cops[[2]], "parameters")[2] <- df 


# Dimensions (currently unused)
d <- 2

# Rank Correlation
tau <- stat_rho[2]
# Fit via Kandalls Tau
# cops <- lapply(cops, function(c) fit_cop(c, tau = tau))

# --- Simulation ---
n <- 100000
set.seed(123)
sim_allcops <- lapply(cops, function(c) simulate_cop(n, c, tau = tau))

names(sim_allcops) <- unlist(lapply(cops, function(c) class(c)[1]))
copsim_df <- data.frame(sim_allcops)

even_cols <- seq(2, ncol(copsim_df), 2)
v_df <- copsim_df[, even_cols]
u_df <- copsim_df[, -c(even_cols)]

params <- coef(fit_BetaShift)
sim_yields <- apply(u_df, 2, function(u) qshift_beta(u, params["alpha"], params["beta"], a, b))
sim_prices <- apply(v_df, 2, function(v) quantile(emp_cdf_p, v))

# - Plots for the presentation -
i <- 1
plt <- scatter_hist_2d(u_df[,i], v_df[,i], type = "density")
plt + labs(title = cop_names[i],
           subtitle = paste0("Theta = ", round(as.numeric(theta[2*i-1]),4)))

plt <- scatter_hist_2d(sim_yields[,i], sim_prices[,i], type = "hexbin")
plt


# --------------------------- 5. Pricing ---------------------------------------

# Note: make sure the settle_price is in the correct order
t0 <- length(settle_price)
coverage_levels <- list("55%" = 0.55,
                        "65%" = 0.65,
                        "75%" = 0.75,
                        "85%" = 0.85,
                        "100%" = 1)
years <- 5
y_g <- mean(tail(yield_dt, years))
# --- Guaranteed Yield (y_g) ---
Y0 <- vapply(coverage_levels, function(cl) y_g * cl, numeric(1))

# --- Guaranteed Price (x_g) ---
corn_ts["Date"] <- as.Date(row.names(corn_ts), "%Y-%m-%d")
ini_date <- as.Date("2020-12-01")
end_date <- as.Date("2020-12-31")
sow_month_prices <- filter(corn_ts, Date >= ini_date, Date <= end_date) %>%
  .$Settle
x_g <- mean(sow_month_prices)

# --- Optimal Premium ---
risk_prem <- risk_premium(coverage_levels, y_g, x_g, sim_yields, sim_prices)

# --- Comparision assuming independent yield and price  ---
sim_rev_indep <- sim_BetaShift * end_avp

indep_rp <- risk_premium(coverage_levels, y_g, x_g, data.frame(sim_BetaShift), data.frame(end_avp))

# --- Table with results ---

risk_prem <- round(rbind(risk_prem, indep_rp) * 100, 3)

prem_list <- split(risk_prem, 1:nrow(risk_prem))

cop_names <- c("Normal", "t", "AMH", "Frank", "Plackett", "Gumbel", "Clayton", "Joe")
risk_prem_df <- data.frame(risk_prem, row.names = c(cop_names, "Independent"))
colnames(risk_prem_df) <- paste0(" ", names(coverage_levels), " ")

risk_prem_df %>%
  kbl(caption = "<span style='font-size:400%'><center>Table 5:Risk Premium for fitted Copula Models</center></span>",
      booktabs = T, linesep = "", align = "c") %>%
  kable_styling(latex_options = c("striped", "hold_position"), font_size = 55) %>%
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

# save_kable(table_5, file = "table_5.html")
# webshot::webshot("table_5.html", "table_5.pdf")


# --- Risk Measures ---

risk_levels <- list(0.95, 0.99, 0.995, 0.999, 0.9995)
qrm <- risk_measures(y_g, x_g, sim_yields, sim_prices, risk_levels)

qrm$VaR
qrm$ES

# --- Comparison assuming independent yield and price  ---
# Make sure the number of simulations is the same
all_sim_y <- cbind(sim_yields, sim_BetaShift)
all_sim_p <- cbind(sim_prices, end_avp)

qrm <- risk_measures(y_g, x_g, all_sim_y, all_sim_p, risk_levels)

qrm$VaR
qrm$ES

tbl_6 <- round(cbind(qrm$VaR, qrm$ES) * 100, 2)
rownames(tbl_6) <- c(cop_names, "Independent")

table_6 <- tbl_6 %>%
  kbl(caption = "<span style='font-size:400%'><center>Table 6:Risk Measures for fitted Copula Models</center></span>",
      booktabs = T, linesep = "", align = "c") %>%
  kable_styling(latex_options = c("striped", "hold_position"), font_size = 55) %>%
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
              background = spec_color(c(unlist(tbl_6[,1])), option = "D", end = 0.8, direction = -1)) %>%
  column_spec(3, color = "white",
              background = spec_color(c(unlist(tbl_6[,2])), option = "D", end = 0.8, direction = -1)) %>%
  column_spec(4, color = "white",
              background = spec_color(c(unlist(tbl_6[,3])), option = "D", end = 0.8, direction = -1)) %>%
  column_spec(5, color = "white",
              background = spec_color(c(unlist(tbl_6[,4])), option = "D", end = 0.8, direction = -1)) %>%
  column_spec(6, color = "white",
              background = spec_color(c(unlist(tbl_6[,5])), option = "D", end = 0.8, direction = -1)) %>%
  column_spec(7, color = "white",
              background = spec_color(c(unlist(tbl_6[,6])), option = "D", end = 0.8, direction = -1)) %>%
  column_spec(8, color = "white",
              background = spec_color(c(unlist(tbl_6[,7])), option = "D", end = 0.8, direction = -1)) %>%
  column_spec(9, color = "white",
              background = spec_color(c(unlist(tbl_6[,8])), option = "D", end = 0.8, direction = -1)) %>%
  column_spec(10, color = "white",
              background = spec_color(c(unlist(tbl_6[,9])), option = "D", end = 0.8, direction = -1)) %>%
  column_spec(11, color = "white",
              background = spec_color(c(unlist(tbl_6[,10])), option = "D", end = 0.8, direction = -1))


# save_kable(table_6, file = "table_6.html")
# webshot::webshot("table_6.html", "table_6.pdf")




# Standard Deviation
(sd_indem <- apply(indem, 2, sd))
(sd_PR <- sd_indem / guaranteed_revenue)

alpha <- 0.01
ret_period <- 1 / alpha
f <- function(x, cl = 1-alpha) quantile(x, cl)
VaR <- apply(indem, 2, f) / guaranteed_revenue

# Expenses Adjustment (management, reinsurance, utility)
expenses <- 0.1 + 0.15 + 0.05
# Market Rate Estimate (VaR and SD method)
(rates <- (PR + pmax(VaR - PR, 0) / ret_period)) 
rates / (1-expenses)

indem_cdf <- apply(indem, 2, ecdf)
# return period of the eps * sd
epsilon <- 3
VaR_equiv <- lapply(1:length(indem_cdf), function(x) indem_cdf[[x]](avg_indem[x] + epsilon * sd_indem[x]))
ret_period2 <- unlist(lapply(VaR_equiv, function(x) 1 / (1-x)))
(rates2 <- (PR + epsilon * sd_PR / ret_period2)) 
rates2 / (1-expenses)



