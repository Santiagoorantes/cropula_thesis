
# ------------------------------------------------------------------------------
# --- 1. Plotting the Pearson family distributions ---
# https://en.wikipedia.org/wiki/Pearson_distribution#The_Pearson_type_VII_distribution
# # https://stats.stackexchange.com/questions/189941/skewness-kurtosis-plot-for-different-distribution#:~:text=The%20Pearson%20plot%20diagram%20characterises%20distributions%20that%20are,Gamma%20distribution%20defines%20the%20green%20Type%20III%20line.
# ------------------------------------------------------------------------------


light_col <- c("#F76E11", "#A6B1E1", "#D9CE3F", "#219F94")
dark_col <- c("#00818A", "#610094", "#ED6363")
letter_size <- 3
letter_stroke <- 4
text_size <- 4

big_size <- 12
med_size <- 10
sm_size <- 8

my_theme <- {
  theme_fivethirtyeight() +
    theme(
      axis.title = element_text(),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank()
    )
}

my_theme <- list(theme_fivethirtyeight() +
                   theme(
                     axis.title = element_text(),
                     plot.title = element_text(size = big_size),
                     plot.subtitle = element_text(size = big_size),
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
                     legend.title = element_text(size = med_size),
                     legend.text = element_text(size = med_size)
                   )
)

#-------------------------------------------------------------------------------

#' Render Pearson Distribution Family Plot for given observations
#' Kurtosis vs Skewness^2
#'
#' @param data vector of observations
#' @param boot number of bootstraps
#' @param method bootstrap method: "unbiased", "biased"
#' @param alpha transparency level
#' @param region_col colors of the family 2-dim regions
#' @param line_col colors of the family 1-dim regions
#' @param obs.col observations color
#' @param obs_shape obsevations shape (pch)
#' @param boot.col bootstrap color
#' @param annotation_col distribution names color
#' @param theme_options additional ggplot2 theme elements
#'
#' @return ggplot2
#' @export
#'
#' @examples
pearson_plot <- function(data, boot = NULL, method = "unbiased", alpha = 0.8,
                         region_col = light_col, line_col = dark_col,
                         obs.col = "#1E5128", obs_shape = 20, boot.col = "#3E7C17",
                         annotation_col = "#191A19", theme_options = my_theme,
                         subtitle = "") {

  # if(is.mcnode(data)) data <- as.vector(data)
  if (missing(data) || !is.vector(data, mode = "numeric")) {
    stop("data must be a numeric vector")
  }
  if (length(data) < 4) {
    stop("data must be a numeric vector containing at least four values")
  }
  moment <- function(data, k) {
    m1 <- mean(data)
    return(sum((data - m1)^k) / length(data))
  }
  if (method == "unbiased") {
    skewness <- function(data) {
      # unbiased estimation (Fisher 1930)
      sd <- sqrt(moment(data, 2))
      n <- length(data)
      gamma1 <- moment(data, 3) / sd^3
      unbiased.skewness <- sqrt(n * (n - 1)) * gamma1 / (n - 2)
      return(unbiased.skewness)
    }
    kurtosis <- function(data) {
      # unbiased estimation (Fisher 1930)
      n <- length(data)
      var <- moment(data, 2)
      gamma2 <- moment(data, 4) / var^2
      unbiased.kurtosis <- (n - 1) / ((n - 2) * (n - 3)) * ((n + 1) * gamma2 - 3 * (n - 1)) + 3
      return(unbiased.kurtosis)
    }
    standdev <- function(data) {
      sd(data)
    }
  } else
  if (method == "sample") {
    skewness <- function(data) {
      sd <- sqrt(moment(data, 2))
      return(moment(data, 3) / sd^3)
    }
    kurtosis <- function(data) {
      var <- moment(data, 2)
      return(moment(data, 4) / var^2)
    }
    standdev <- function(data) {
      sqrt(moment(data, 2))
    }
  } else {
    stop("The only possible value for the argument method are 'unbiased' or 'sample'")
  }

  res <- list(
    min = min(data), max = max(data), median = median(data),
    mean = mean(data), sd = standdev(data),
    skewness = skewness(data), kurtosis = kurtosis(data), method = method
  )


  skewdata <- res$skewness
  kurtdata <- res$kurtosis

  # --- Building the plot ---

  if (!is.null(boot)) {
    if (!is.numeric(boot) || boot < 10) {
      stop("boot must be NULL or a integer above 10")
    }
    n <- length(data)

    databoot <- matrix(sample(data, size = n * boot, replace = TRUE), nrow = n, ncol = boot)
    s2boot <- sapply(1:boot, function(iter) skewness(databoot[, iter])^2)
    kurtboot <- sapply(1:boot, function(iter) kurtosis(databoot[, iter]))

    kurtmax <- max(10, ceiling(max(kurtboot)))
    xmax <- max(4, ceiling(max(s2boot)))
  } else {
    kurtmax <- max(10, ceiling(kurtdata))
  }

  # --- Building the regions for the Pearson type distributions ---

  # Region 0 (Imposible)
  df <- data.frame("x" = c(0, 0, 1, 15), "y" = c(14, 0, 0, 14))
  p <- ggplot(df) +
    geom_polygon(aes(x = x, y = y), fill = "gray", alpha = alpha) +
    coord_cartesian(xlim = c(0, 25), ylim = c(0, 14))

  # Region I (Beta)
  df_r1 <- data.frame("x" = c(15, 1, 3, 3 + 21), "y" = c(14, 0, 0, 14))
  p <- p +
    geom_polygon(data = df_r1, aes(x = x, y = y), fill = region_col[1], alpha = alpha)

  # Gamma (III)
  gamma_df <- data.frame("x" = c(3, 3 + 21), "y" = c(0, 14))
  p <- p +
    geom_line(
      data = gamma_df,
      aes(x = x, y = y),
      linetype = 2, size = 1.2, colour = line_col[1]
    )

  # region VI Beta Prime, F
  a <- rev(exp(seq(log(5.759), log(1000), .2)))

  df_r6 <- data.frame(
    "x" = c(25, 3 + 21, 3, 3 * (a + 5) * (a - 2) / (a - 3) / (a - 4)),
    "y" = c(14, 14, 0, 16 * (a - 2) / (a - 3)^2)
  )
  p <- p + geom_polygon(data = df_r6, aes(x = x, y = y), fill = region_col[2], alpha = alpha)

  # LogNormal (NOT type VI but falls into the region)
  es2 <- seq(1, 1.724, length.out = 100)

  lnorm_df <- data.frame(
    "x" = c(3, (es2^4 + 2 * es2^3 + 3 * es2^2 - 3)),
    "y" = c(0, (es2 + 2)^2 * (es2 - 1))
  )
  p <- p + geom_line(
    data = lnorm_df,
    aes(x = x, y = y, colour = "Lognormal"),
    linetype = 2, size = 1.2, colour = line_col[2]
  )

  # Inverse Gamma (V)
  invgam_df <- data.frame(
    "x" = c(3, 3 * (a + 5) * (a - 2) / (a - 3) / (a - 4)),
    "y" = c(0, 16 * (a - 2) / (a - 3)^2)
  )
  p <- p + geom_line(
    data = invgam_df,
    aes(x = x, y = y, colour = "Lognormal"),
    linetype = 2, size = 1.2, colour = line_col[3]
  )

  # region IV
  a <- rev(a)

  df_r4 <- data.frame(
    "x" = c(3 * (a + 5) * (a - 2) / (a - 3) / (a - 4), 3, 25),
    "y" = c(16 * (a - 2) / (a - 3)^2, 0, 0)
  )
  p <- p + geom_polygon(data = df_r4, aes(x = x, y = y), fill = region_col[3], alpha = alpha)

  # bootstrap sample for observed distribution
  if (!is.null(boot)) {
    # Bootstrap plot
    boot_df <- data.frame("x" = kurtboot, "y" = s2boot)
    p <- p + geom_point(
      data = boot_df, aes(x, y),
      shape = obs_shape, size = 2, color = boot.col, alpha = 0.5
    )
  }
  # Observed distribution (data)
  p <- p + geom_point(aes(kurtosis(data), skewness(data)^2),
    shape = obs_shape, size = 6, color = obs.col, stroke = 2
  )

  # Normal dist
  p <- p + geom_point(aes(3, 0),
    shape = "N", size = letter_size, stroke = letter_stroke,
    colour = annotation_col
  )
  # Uniform dist
  p <- p + geom_point(aes(9 / 5, 0),
    shape = "U", size = letter_size, stroke = letter_stroke,
    colour = annotation_col
  )
  # Exponential dist
  p <- p + geom_point(aes(9, 2^2),
    shape = "E", size = letter_size, stroke = letter_stroke,
    colour = annotation_col
  )
  # Logistic dist
  p <- p + geom_point(aes(4.2, 0),
    shape = "L", size = letter_size, stroke = letter_stroke,
    colour = annotation_col
  )

  # Annotations
  p <- p +
    annotate("text",
      x = 4, y = 10, label = "Imposible",
      family = "serif", fontface = "bold", colour = annotation_col,
      size = text_size
    ) +
    annotate("text",
      x = 12, y = 9, label = "Beta (I)",
      family = "serif", fontface = "bold", colour = annotation_col,
      size = text_size, angle = 0
    ) +
    annotate("text",
      x = 15, y = 8.3, label = "Gamma (III)",
      family = "serif", fontface = "bold", colour = annotation_col,
      size = text_size, angle = 29
    ) +
    annotate("text",
      x = 20, y = 9, label = "Beta prime or F (VI)",
      family = "serif", fontface = "bold", colour = annotation_col,
      size = text_size, angle = 0
    ) +
    annotate("text",
      x = 16, y = 6.6, label = "Lognormal",
      family = "serif", fontface = "bold", colour = annotation_col,
      size = text_size, angle = 20
    ) +
    annotate("text",
      x = 18, y = 6.1, label = "Inverse gamma (V)",
      family = "serif", fontface = "bold", colour = annotation_col,
      size = text_size, angle = 15
    ) +
    annotate("text",
      x = 20, y = 3, label = "(IV)",
      family = "serif", fontface = "bold", colour = annotation_col,
      size = text_size, angle = 0
    ) +
    annotate("text",
      x = 15, y = 0, label = "t (VII)",
      family = "serif", fontface = "bold", colour = annotation_col,
      size = text_size, angle = 0
    )

  p <- p + xlab("(Skewness)^2") + ylab("Kurtosis") +
    labs(title = "Pearson family plot",
         subtitle = subtitle) +
    theme_options

  return(p)
}
