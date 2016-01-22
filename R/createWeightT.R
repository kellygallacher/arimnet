# function to create weights based on temporal autocorrelation
createWeightT <- function(n, rho, corr.form="AR1") {
  if(rho <= 0 | rho >=1) {stop("rho must be between 0 and 1")}
  
  if(corr.form=="AR1") {
  weightT <- diag(n)
  weightT <- rho^abs(row(weightT)-col(weightT))
  }  
  return(weightT)
}


# # test
# temp.wt <- createWeightT(n=5, rho=0.5)
# temp.wt <- createWeightT(n=5, rho=0)
# temp.wt <- createWeightT(n=5, rho=6)