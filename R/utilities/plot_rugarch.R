plot.garchfit.8 <- function(x, size = 2,...) {
  
  vmodel  = x@model$modeldesc$vmodel
  zseries = as.numeric(residuals(x, standardize=TRUE))
  distribution = x@model$modeldesc$distribution
  idx = x@model$pidx
  pars  = x@fit$ipars[,1]
  skew  = pars[idx["skew",1]]
  shape = pars[idx["shape",1]]
  if(distribution == "ghst") ghlambda = -shape/2 else ghlambda = pars[idx["ghlambda",1]]
  xmean 	= mean(zseries)
  xmedian = median(zseries)
  xsd 	= sd(zseries)
  xlim 	= c(min(zseries), max(zseries))
  result 	= hist(x = zseries, col = "grey", border = "white",
                 breaks = "Scott", main = "Empirical Density of Standardized Residuals", xlim = xlim, ylim = c(0,0.6),
                 probability = TRUE, ylab="", xlab = "", cex.axis = size - 0.5, cex.main = size + 0.5, ...)
  box()
  #s 	= seq(xlim[1], xlim[2], length = 201)
  s = result$breaks
  y	= ddist(distribution, s, lambda = ghlambda, skew = skew, shape = shape)
  lines(s, dnorm(s, 0, 1), lwd = 3, col = "blue")
  lines(s, y, lwd = 3, col = "orange")
  abline(v = xmean, lwd = 2, col = "red")
  abline(v = xmedian, lwd = 2, col = "darkgreen")
  mtext("Probability", side = 2, line = 2.5, cex = size)
  mtext("Standardized Residuals", side=1, line = 3, cex = size)
  abline(h = 0, col = "grey")
  lg.txt = c("normal Density", paste(distribution," (0,1) Fitted Density",sep=""))
  legend("topleft", legend = lg.txt, col = c("blue","orange"), lty = 1, lwd = 2, cex = size, bty = "n")
  grid()
  
}

# ------------------------------------------------------------------------------

plot.garchfit.9 <- function(x, size = 2,...)
{
  vmodel  = x@model$modeldesc$vmodel
  zseries = as.numeric(residuals(x, standardize=TRUE))
  distribution = x@model$modeldesc$distribution
  idx = x@model$pidx
  pars  = x@fit$ipars[,1]
  skew  = pars[idx["skew",1]]
  shape = pars[idx["shape",1]]
  if(distribution == "ghst") ghlambda = -shape/2 else ghlambda = pars[idx["ghlambda",1]]
  
  .qqDist(y = zseries, dist = distribution, lambda = ghlambda, skew = skew, shape = shape,
          cex.main = size, cex.axis = size-0.5)

}


.qqDist <- function (y, dist = "norm", ylim = NULL, main = paste(dist, "- QQ Plot"),
                    xlab = "", ylab = "", doplot = TRUE,
                    datax = FALSE, cex.main = 1, cex.axis = 1, ...)
{	
  y = as.vector(y)
  if (has.na <- any(ina <- is.na(y))) {
    yN = y
    y = y[!ina]
  }
  if (0 == (n <- length(y))) stop("y is empty or has only NAs")
  x = qdist(distribution = dist, p = ppoints(n), ...)[order(order(y))]
  if (has.na) {
    y = x
    x = yN
    x[!ina] = y
    y = yN
  }
  if (doplot) {
    if (is.null(ylim)) ylim = range(y)
    if (datax) {
      plot(y, x, main = main, xlab = ylab, ylab = xlab, xlim = ylim,
           col = "steelblue", pch = 19, lwd = cex.axis,
           cex.main = cex.main, cex.axis = cex.axis)
    } else {
      plot(x, y, main = main, xlab = xlab, ylab = ylab, ylim = ylim,
           col = "steelblue", pch = 19, lwd = cex.axis,
           cex.main = cex.main, cex.axis = cex.axis)
    }
    .qqLine(y = y, dist = dist, datax = datax, ...)
    
    mtext("Theoretical Quantiles", 1, line = 3, cex = cex.main)
    mtext("Sample Quantiles", 2, line = 2.5, cex = cex.main)
    
    grid()
  }
  invisible(if (datax) list(x = y, y = x) else list(x = x, y = y))
}


.qqLine <- function (y, dist = "norm", datax = FALSE, ...)
{   
  y = as.vector(y)
  y = quantile(y[!is.na(y)], c(0.25, 0.75))
  x = qdist(distribution = dist, p = c(0.25, 0.75), ...)
  if (datax) {
    slope = diff(x)/diff(y)
    int = x[1] - slope * y[1]
  } else {
    slope = diff(y)/diff(x)
    int = y[1] - slope * x[1]
  }
  abline(int, slope, lwd = 2)
}
