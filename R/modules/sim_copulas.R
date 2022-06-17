
#-------------------------------------------------------------------------------
# Simulate and given Copula Families
#
# (--- Using Copula Package ---)
#-------------------------------------------------------------------------------

# --- Lists of Copulas ---

cops <- list(
  
  # - Explicit Copulas -
  
  normalCopula(),
  tCopula(),
  
  # - Archimedean Copulas -
  
  # nonnegative
  gumbelCopula(),
  claytonCopula(),
  joeCopula(),
  
  # other
  amhCopula(),
  frankCopula()

)
             


# --- Fitting the copulas ---

fit_cop <- function(cop, tau = NULL, rho = NULL) {
    
    nonneg <- class(cop)[1] %in% c("gumbelCopula", "claytonCopula", "joeCopula")
    if (nonneg) {
      
      theta <- ifelse(!is.null(tau), iTau(cop, abs(tau)), iRho(cop, abs(rho)))
      attr(cop, "parameters")[1] <- theta
      return(cop)
      
    }
    
    theta <- ifelse(!is.null(tau), iTau(cop, tau), iRho(cop, rho))
    attr(cop, "parameters")[1] <- theta
    return(cop)
    
} 

# newcops <- lapply(cops, function(c) fit_cop(c, tau = -0.2))


# --- Simulating (and fitting) the copulas ---

simulate_cop <- function(n, cop, tau = NULL, rho = NULL) {
  
  # Only estimate parameter if missing
  if (is.na(attr(cop, "parameters")[1])) {
    cop <- fit_cop(cop, tau = tau, rho = rho)  
  }
  
  sim <- copula::rCopula(n,cop)

  # Rotate copula if rank_corr < 0 and mycop in nonnegative cops
  
  stat_rho <- ifelse(!is.null(tau), tau, rho)
  
  nonneg <- class(cop)[1] %in% c("gumbelCopula", "claytonCopula", "joeCopula")
  if (nonneg & stat_rho < 0) {
    
    condition <- class(cop)[1] %in% c("gumbelCopula", "joeCopula")
    if (condition) {
      sim[,2] <- 1 - sim[,2]
    } else {
      sim[,1] <- 1 - sim[,1]
    }
    
    print(paste0("Rotated ", class(cop)[1]))
    return(sim)
    
  }
  
  print(class(cop)[1])
  return(sim)
  
}



