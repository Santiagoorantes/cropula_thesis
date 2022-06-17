
# Pricing

# coverage_levels <- list("55%" = 0.55,
#                         "65%" = 0.65,
#                         "75%" = 0.75,
#                         "85%" = 0.85,
#                         "100%" = 1)
# 
# name_copulas <- c("gumbelCopula", "claytonCopula", "joeCopula", 
#                   "amhCopula", "frankCopula",
#                   "normalCopula", "tCopula")


risk_premium <- function(coverage_levels, y_g, x_g, Y_df, X_df) {
  
  n <- nrow(Y_df)
  m <- ncol(Y_df)
  l <- length(coverage_levels)
  
  # guaranteed yield
  y_levels <- vapply(coverage_levels, function(cl) y_g * cl, numeric(1))
  
  guaranteed_revenue <- vapply(y_levels, function(y) y * x_g, numeric(1))
  
  # R is smort and does 1v1 product df
  sim_rev_df <- Y_df * X_df
  
  loss_prob <- data.frame(matrix(NA, nrow = m, ncol = l))
  indemnity <- data.frame(matrix(NA, nrow = m, ncol = l))
  PR <- data.frame(matrix(NA, nrow = m, ncol = l))
  for (i in 1:m) {
    
    # Prob of being below GR
    loss_prob[i,] <- vapply(guaranteed_revenue, function(gr) mean(sim_rev_df[,i] < gr), numeric(1))
    
    # Indemnity/Payout
    indem <- vapply(guaranteed_revenue, function(gr) pmax(gr - sim_rev_df[,i], 0), numeric(n))  
    avg_indem <- apply(indem, 2, mean)
    indemnity[i,] <- avg_indem 
    
    # Fair Value / Risk Premium
    PR[i,] <- avg_indem / guaranteed_revenue  
    
  }
  
  if(m>1){
    rownames(loss_prob) <- colnames(sim_yields)  
    rownames(indemnity) <- colnames(sim_yields) 
    rownames(PR) <- colnames(sim_yields)
  }
  
  colnames(loss_prob) <- names(coverage_levels)
  colnames(indemnity) <- names(coverage_levels)
  colnames(PR) <- names(coverage_levels)
  
  
  print(loss_prob)
  print(indemnity)
  
  return(PR)
  
}



