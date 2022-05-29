# ------------------------------------------------------------------------------
# --- 0. Loading packages, functions and data ---
# ------------------------------------------------------------------------------

# --- Load required packages ---
source("global.R")

# --- Load raw data ---
source("data.R")

# --- Load Utilities ---
folders <- c("utilities")
file_sources <- list.files(folders, pattern = "\\.R$", full.names = TRUE)
sapply(file_sources, source, .GlobalEnv)


# ------------------------------------------------------------------------------
# --- 1. Data preparation ---
# ------------------------------------------------------------------------------

# Corn continuous futures
corn_ts$Date <- as.Date(corn_ts$Date, "%Y-%m-%d")

# Timeframe of interest
ini_date <- as.Date("2003-01-01")
end_date <- as.Date("2020-12-31")

corn_ts <- dplyr::filter(corn_ts, Date >= ini_date, Date <= end_date)


## Change column_names of SIAP db (agr_df) to their English equivalent.
colnames(agr_df) <- c(
  "year",
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
  "production_value"
)

# ------------------------------------------------------------------------------
# --- 2. Fit empirical and parametric marginals for the Yield ---
# ------------------------------------------------------------------------------

# --- 2.0 Applying filters ---

# Define filter
my_filter <-  list(
  crop = c("Maíz grano"),
  type = c("Riego"),
  cycle = c("Otoño-Invierno"),
  state = c("Sinaloa"),
  municipality = c("Guasave")
)

# Filter target crop data 
filtered_df <- filter_df(agr_df, my_filter)

# Check if there are further distric divisions
districts <- unique(filtered_df$district)

group_vars <- c("year", names(my_filter))
# DF of Yield and Price per year given filtered df
trgt_df <- filtered_df %>%
  group_by(!!!syms(group_vars)) %>%
  summarise(
    av_yield = sum(volume) / sum(sowed),
    av_price = sum(production_value) / sum(volume)
  )

# --- 2.1 Detrending the series ---

# NOTE: make sure that there is only ONE value per year to detrend the series
nrow(trgt_df) == length(unique(trgt_df$year))

yield <- as.numeric(trgt_df$av_yield)
l <- length(yield)
t <- seq(1, l, 1)

# The simple linear regression for the yield was sufficient,
# anything of higher order was not significant.

# y_t = alpha + beta*T + e_t
model <- glm(yield ~ t)
summary(model)
acf(model$residuals)

# y_bar_t = y_hat_2020 * (1 + e_hat_t / y_hat_t )
yield_dt <- model$fitted.values[l] * (1 + model$residuals / model$fitted.values)

# Using Ljung-Box test to check for check for temporal dependency
# H0: The residuals are independently distributed.
# HA: The residuals are not independently distributed; they exhibit serial correlation.

# Temporal dependency
Box.test(yield, lag = 1, type = "Ljung")
Box.test(yield_dt, lag = 1, type = "Ljung")

# Homoscedasticity of variances
Box.test(yield^2, lag = 1, type = "Ljung")
Box.test(yield_dt^2, lag = 1, type = "Ljung")


# --- 2.2 Fitting distributions to the YIELD ---

# Pearson (based on Cullen and Frey) plots
pearson_plot(yield_dt, boot = 1000, method = "sample", alpha = 0.7)

# Normal distribution
fit_norm <- fitdist(yield_dt, distr = "norm", method = "mle")

# Gamma distribution
fit_Gam <- fitdist(yield_dt, distr = "gamma", method = "mle")

# Weibull distribution
fit_Wei <- fitdist(yield_dt, distr = "weibull", method = "mle")

# Beta distribution
a <- 0
# Should the additional parameter "b" be taken into account in the AIC and BIC?
b <- ceiling(max(yield_dt) + sd(yield_dt))
b <- ceiling(max(yield_dt))

fit_BetaShift <- fitdist(yield_dt, distr = "shift_beta",
                       start = list(alpha = 10, beta = 1),
                       fix.arg = list(a = a, b = b),
                       method = c("mle"))

# --- Comparing fitted distributions ---

# Goodness of fit statistics and criteria
fitted_dists <- list(fit_norm, fit_Gam, fit_Wei, fit_BetaShift)
gof <- gofstat(fitted_dists)
qqcomp(fitted_dists)
cdfcomp(fitted_dists)

# DataFrame of Goodness-of-fit
(gof_df <- data.frame("KS" = gof$ks,
                     "CvM" = gof$cvm,
                     "AD" = gof$ad,
                     "AIC" = gof$aic,
                     "BIC" = gof$bic))

full_names <- c("Kolmogorov-Smirnov statistic", "Cramer-von Mises statistic",
                  "Anderson-Darling statistic", "Akaike's Information Criterion",
                  "Bayesian Information Criterion")

probs <- seq(0, 0.9999, length.out = 1000)
yield_Norm <- qnorm(probs, fit_norm$estimate[1], fit_norm$estimate[2])
yield_Gamma <- qgamma(probs, fit_Gam$estimate[1], fit_Gam$estimate[2])
yield_Weibull <- qweibull(probs, fit_Wei$estimate[1], fit_Wei$estimate[2])
yield_BetaShift <- qshift_beta(probs, fit_BetaShift$estimate[1], fit_BetaShift$estimate[2],
                               a = a, b = b)

sim_Norm <- rnorm(10000, mean=fit_norm$estimate[1], sd=fit_norm$estimate[2])
sim_Gamma <- rgamma(10000, shape=fit_Gam$estimate[1], rate=fit_Gam$estimate[2])
sim_Weibull <- rweibull(10000, shape=fit_Wei$estimate[1], scale=fit_Wei$estimate[2])
sim_BetaShift <- rshift_beta(10000, alpha=fit_BetaShift$estimate[1], beta=fit_BetaShift$estimate[2],
                             a = a, b = b)

fit_df <- data.frame(yield_Norm, yield_Gamma, yield_Weibull, yield_BetaShift)
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
  xlim(0, b + 1)

  p + my_theme +
    theme(legend.position = "bottom")


# ------------------------------------------------------------------------------
# --- 3. Fit ARMA-GARCH model to the Price Series ---
# ------------------------------------------------------------------------------

my_colors <- c(my_brick="#CC704B", my_lgreen="#9FC088", my_lbrown="#E8C07D",
               my_lblue="#8FBDD3", my_cyan="#5AA897", my_lred="#DD4A48")

row.names(corn_ts) <- corn_ts$Date
corn_ts <- corn_ts %>% dplyr::select(-c(Date))
settle_price <- corn_ts$Settle
lr <- diff(log(settle_price))[-1]
l <- length(lr)

# --- 3.0 Analyzing the Series ---

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
# t-student QQ-plot
plot(model_fit, which=9)
# estimated coefficients
coef(model_fit)

# Standardize residuals
std_res <- residuals(model_fit, standardize=TRUE)

nu <- coef(model_fit)["shape"]
sim_t <- rt(10000, nu)

plot(model_fit, which="all")

# --- Actual residuals ---
# Residuals to use for the copulas
res <- residuals(model_fit)


# ------------------------------------------------------------------------------
# Forecasting / Simulating Paths

# Forecasting
model_forc <- ugarchforecast(model_fit, n.ahead = 100)
plot(model_forc, which = 1)
plot(model_forc, which = 3)

# Simulation
days <- 180
nsim <- 1000
model_sim <- ugarchsim(model_fit, n.sim = days,
                       n.start = 1, m.sim = nsim,
                       startMethod = "sample")

#plot(model_sim)

class(model_sim@simulation)

# Simulated log-returns
lr_sim <- model_sim@simulation$seriesSim
# Last Price of the dataset
p_0 <- corn_ts$Settle[1]
exp_lr <- exp(lr_sim)
x <- rbind(rep(p_0, nsim), exp_lr)
# Simulated Prices
p_t <- apply(x, 2, cumprod)

# Plotting the paths
alpha <- 0.05
uq <- 1 - alpha/2
lq <- 1 - uq
uq_t <- apply(p_t, 1, function(x) quantile(x, uq))
lq_t <- apply(p_t, 1, function(x) quantile(x, lq))
matplot(x = seq(0, days, length.out = nrow(p_t)), y = p_t,
        type = "l", lty = 1, col = 4:5,
        main = "Simulated paths ARMA(1,1)-GARCH(1,1)",
        xlab = "t", ylab = "Price (USD)")
lines(x = seq(0, days, length.out = nrow(p_t)), y = uq_t,
      lty = 3, lwd = 3, col = "red")
lines(x = seq(0, days, length.out = nrow(p_t)), y = lq_t,
      lty = 3, lwd = 3, col = "red")
legend("topleft",legend = c(paste0("quant_", uq), paste0("quant_", lq)),
       cex = 1, col=c("red", "red"), lty = c(3,3), lwd = c(3,3))

boxplot(p_t[days,])


# ------------------------------------------------------------------------------
# --- 4. CROPULAS ---
# ------------------------------------------------------------------------------

# --- 4.1 Rank Correlations ---

# Option 1: SIAP Price (detrended)
p <- trgt_df$av_price
l <- length(p)
t <- seq(1, l, 1)

model <- lm(p ~ t)
price_dt <- model$fitted.values[l] * (1 + model$residuals / model$fitted.values)


# Correlation
meth <- c("pearson", "kendall", "spearman")
# Biased series
for (i in seq_along(meth)) {
  print(cor(yield_dt, p, method = meth[i]))
}
# Detrended/unbiased series
for (i in seq_along(meth)) {
  print(cor(yield_dt, price_dt, method = meth[i]))
}
stat_rho <- cor(yield_dt, price_dt, method = meth[3])

years <- trgt_df$year
roll <- 10
rho <- numeric()
for (i in 1:(l-roll)) {
  rho[i] <- (cor(yield_dt[i:(i+roll)],price_dt[i:(i+roll)],method = "spearman"))
}
ggplot() + geom_line(aes(x = years[(roll+1):l], y=rho)) +
  geom_abline(intercept = stat_rho, slope = 0) +
  ylim(c(-1,1))

# Option 2: CME-based harvest price
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
plot(p, type="l")

# Correlation of original vs detrended CME price series
l <- length(p)
t <- seq(1, l, 1)

model <- lm(p~t)
price_dt <- model$fitted.values[l] * (1 + model$residuals / model$fitted.values)


# Correlation
meth <- c("pearson", "kendall", "spearman")
# Biased series
for (i in seq_along(meth)) {
  print(cor(yield_dt, p, method = meth[i]))
}
# Detrended/unbiased series
for (i in seq_along(meth)) {
  print(cor(yield_dt, price_dt, method = meth[i]))
}
stat_rho <- cor(yield_dt, price_dt, method = meth[2])

years <- trgt_df$year
roll <- 10
rho <- numeric()
for (i in 1:(l-roll)) {
  rho[i] <- (cor(yield_dt[i:(i+roll)],price_dt[i:(i+roll)],method = "kendall"))
}
ggplot() + geom_line(aes(x = years[(roll+1):l], y=rho)) +
  geom_abline(intercept = stat_rho, slope = 0) +
  ylim(c(-1,1))

# We can notice at least some of the negative correlation
fig <- ggplot() + 
  geom_point(aes(x = yield_dt, y = price_dt), 
             colour = "steelblue",
             size = 2)
ggMarginal(fig,
           type = "histogram",
           size = 3,
           fill = "steelblue",
           xparams = list(binwidth = 0.5),
           yparams = list(binwidth = 6))

scatter_hist_2d(yield_dt, price_dt, type = "density")


# --- Working with the residuals of the ARMA-GARCH ---

dates <- date(res)
price_res <- data.frame(
  "year" = year(dates),
  "month" = month(dates),
  "day" = day(dates),
  "residual" = res
)

# May and June are the months where almost all of the harvest happens
harvest_months <- c(5,6)
price_res <- price_res %>%
  group_by(year) %>%
  filter(month %in% harvest_months) %>%
  summarise(av_res = mean(residual))

trgt_res <- price_res$av_res
plot(trgt_res, type="l")

# Rank Correlation with the detrended yields
for (i in seq_along(meth)) {
  print(cor(yield_dt, trgt_res, method = meth[i]))
}
stat_rho <- cor(yield_dt, trgt_res, method = meth[2])

years <- price_res$year
roll <- 10
rho <- numeric()
for (i in 1:(l-roll)) {
  rho[i] <- (cor(yield_dt[i:(i+roll)],trgt_res[i:(i+roll)],method = "kendall"))
}
ggplot() + geom_line(aes(x = years[(roll+1):l], y=rho)) +
  geom_abline(intercept = stat_rho, slope = 0) +
  ylim(c(-1,1))

# We can notice at least some of the negative correlation
fig <- ggplot() + 
  geom_point(aes(x = yield_dt, y = trgt_res), 
             colour = "steelblue",
             size = 2)
ggMarginal(fig,
           type = "histogram",
           size = 3,
           fill = "steelblue",
           xparams = list(binwidth = 0.5),
           yparams = list(binwidth = 0.0005))

scatter_hist_2d(yield_dt, trgt_res, type = "density")


# --- Marginals ---
# We have our marginals for the yield and price

n <- 10000

# Shifted Beta dist
params <- coef(fit_BetaShift)
sim_y <- rshift_beta(n,
                     alpha = params["alpha"], beta = params["beta"],
                     a = a, b = b)
# CDF 
y <- pshift_beta(sim_y, params["alpha"], params["beta"], a, b)

# t-dist for the price (ARMA-GARCH residuals)
ag_params <- coef(model_fit)
nu <- coef(model_fit)["shape"]
sim_t <- rt(n, nu)
# CDF
x <- pt(sim_t, df = nu)

scatter_hist_2d(x = sim_y, y = sim_t, type = "hexbin")

# --- Fitting the Copulas ---

# Implicit theta value for a given tau
d <- 2
noneg_acops <- list(gumbelCopula(dim = d),
                   claytonCopula(dim = d),
                   joeCopula(dim = d))
acops <- list(amhCopula(dim = d),
             frankCopula(dim = d))
cops <- list(normalCopula(dim = d),
             tCopula(dim = d),
             fgmCopula(dim = d))

# Choose Copula
i <- 1
mycop <- acops[[i]]

# Kendall's tau
tau <- ifelse(list(mycop) %in% noneg_acops, abs(stat_rho), stat_rho)

# Corresponding parameter of the Archimedian Cop
theta <- iTau(mycop, tau)

# Fix the copula parameter parameter
attr(mycop, "parameters")[1] <- theta
mycop

## Only for Archimedian Copulas
#C2 <- onacopula(mycop, C(theta, 1:2))
#dim(U2 <- rnacopula(n, C2))
#cor(U2, method="kendall")
#splom2(sign(stat_rho) * U2)

# Simulating the Copula
simcop <- rCopula(n, mycop)
u <- simcop[,1]
v <- simcop[,2]
# Simulated values of the Yield
Y <- qshift_beta(u, params["alpha"], params["beta"], a, b)
# Simulated values of the residuals of the lr of the Price
X <- qt(v,  nu)

scatter_hist_2d(Y, (-sign(tau)) * X, type = "density")
scatter_hist_2d(Y, (-sign(tau)) * X, type = "hexbin")
 

# --- Fitting and Goodness-of-Fit ---

# The gof test is suuper slow so we decrease n
simcop <- rCopula(1000, mycop)
obs  <- pobs(simcop)
fit_mpl <- fitCopula(mycop, obs, method = "mpl")
fit_ml <- fitCopula(mycop, simcop, method = "ml", traceOpt=TRUE)
summary(fit_mpl)
summary(fit_ml)

AIC(fit_mpl)
AIC(fit_ml)
BIC(fit_mpl)
BIC(fit_ml)

N <- 100
gof <- gofCopula(mycop, simcop, N = N, method = "Sn", estim.method = "ml")
gofCopula(mycop, simcop, N = N, method = "SnB")


# VineCopula
#fit_Cop <- BiCopSelect(Y, P, familyset = NA)


# ------------------------------------------------------------------------------
# --- TEST: Parametric fit to Price ---
# ------------------------------------------------------------------------------

# Price parametric dists just for fun

test_fit1 <- fitdist(price_dt, distr = "weibull")
summary(test_fit1)  
test_fit2 <- fitdist(price_dt, distr = "norm")
summary(test_fit2)  
test_fit3 <- fitdist(price_dt, distr = "lnorm")
summary(test_fit3)  
test_fit4 <- fitdist(price_dt, distr = "gamma")
summary(test_fit4)  
test_fit5 <- fitdist(price_dt, distr = "Gumbel",
                     start = list(location = 0, scale = 1))
summary(test_fit5)  

# Goodness of fit statistics and criteria
fitted_dists <- list(test_fit1, test_fit2, test_fit3, test_fit4, test_fit5)
gof <- gofstat(fitted_dists)
qqcomp(fitted_dists)
cdfcomp(fitted_dists)

# DataFrame of Goodness-of-fit
(gof_df <- data.frame("KS" = gof$ks,
                      "CvM" = gof$cvm,
                      "AD" = gof$ad,
                      "AIC" = gof$aic,
                      "BIC" = gof$bic))

# Gumbel-dist for the detrended price
params <- coef(test_fit5)
sim_Gum <- rGumbel(n, location = params["location"], scale = params["scale"])

# CDF
P_Gum <- pGumbel(sim_Gum, params["location"], params["scale"])

scatter_hist_2d(sim_y, sim_Gum, type = "hexbin")
