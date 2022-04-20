# Factors generator function -----
generate_X <- function(i, j, k) {
  "Generate factors by the given i, j, k (i.e. number 
  of A level, B level, and repetition) respectively."
  
  n = i * j * k # Define the number of overall sample
  
  A = factor(c(rep(1:i-1, each=j*k)))
  B = factor(c(rep(1:j-1, i*k)))
  AB = as.numeric(A) * as.numeric(B)
  
  X = model.matrix(~ A*B, data=data.frame(A,B))
  #result = list('A'=A, 'B'=B, 'AB'=AB, 'X'=X)
  return(X)
}


# Dependent variable generator function -----
generate_y <- function(X, seed=42, beta=runif(ncol(X),-10,10)){
  "Generate Y values by specified X (factors) and beta (optional)."
  
  set.seed(seed) # Set random seed so the result reproducable

  n = nrow(X)
  u = exp(X^2/2) + rnorm(n) 
  u = t(t(rowSums(u)))
  galat = rnorm(n)
  Y = X %*% beta
  Y.hat = (X %*% beta) + u
  mod = lm(Y~X+0)
  betax = mod$coefficients
  result = list('X'=X, 'Y'=Y.hat, 'beta'=betax)
  return(result)
}


# Simulating function -----
simulate_model <- function(i, j, k, s, seed=NULL){
  "Simulating OLS and FGLS model with given arguments."
  
  MAE_OLS = c() # Initialize empty lists
  MAE_FGLS = c()
  MSE_OLS = c()
  MSE_FGLS = c()
  bias_OLS = c()
  bias_FGLS = c()
  var_OLS = c()
  var_FGLS = c()
  
  set.seed(seed)         # Setting random seeds so
  seeds = sample(1e6, s) # the result reproducable
  
  for (q in 1:s) {
    X = generate_X(i, j, k)
    data = generate_y(X,seed=seeds[q])
    beta = data$beta
    Y = data$Y
    OLS = lm(Y~X+0) # Define an OLS Model
    u = resid(OLS) # Save the residual as new variable
    u1 = exp(fitted(lm(log(u^2)~((X))^2))) # Skedastic function
    FGLS = lm(Y~X+0, weights = 1/u1) # Define an FGLS model
    
    maeOLS = mean(abs(summary(OLS)$residual))
    maeFGLS = mean(abs(summary(FGLS)$residual))
    mseOLS = mean(summary(OLS)$residual^2)
    mseFGLS = mean(summary(FGLS)$residual^2)
    biasOLS = abs(mean(beta - coef(OLS)))
    biasFGLS = abs(mean(beta - coef(FGLS)))
    varOLS = mseOLS - biasOLS
    varFGLS = mseFGLS - biasFGLS
    
    MAE_OLS = c(MAE_OLS, maeOLS)
    MAE_FGLS = c(MAE_FGLS, maeFGLS)
    MSE_OLS = c(MSE_OLS, mseOLS)
    MSE_FGLS = c(MSE_FGLS, mseFGLS)
    bias_OLS = c(bias_OLS, biasOLS)
    bias_FGLS = c(bias_OLS, biasFGLS)
    var_OLS = c(var_OLS, varOLS)
    var_FGLS = c(var_FGLS, varFGLS)
  }
  
  metric = cbind('MAE_OLS'=mean(MAE_OLS), 'MAE_FGLS'=mean(MAE_FGLS),
                 'MSE_OLS'=mean(MSE_OLS), 'MSE_FGLS'=mean(MSE_FGLS),
                 'Bias_OLS'=mean(bias_OLS), 'Bias_FGLS'=mean(bias_FGLS),
                 'var_OLS'=mean(var_OLS), 'var_FGLS'=mean(var_FGLS))
  rownames(metric) = c(paste(i, 'Ax', j, 'B(', k, ')', sep=''))
  metric = data.frame(metric)
  
  return(metric)
}
