# Factors generator function -----
generate_X <- function(i, j, k) {
  "Generate factors by the given i, j, k (i.e. number 
  of A level, B level, and repetition) respectively."
  
  n = i * j * k # Define the number of overall sample
  
  A = factor(c(rep(1:i, each=j*k)))
  B = factor(c(rep(1:j, i*k)))
  AB = as.numeric(A) * as.numeric(B)
  
  X = model.matrix(~ A*B, data=data.frame(A,B))
  result = list('A'=A, 'B'=B, 'AB'=AB, 'X'=X)
  return(result)
}


# Dependent variable generator function -----
generate_y <- function(X, seed=42, beta=runif(ncol(X),-10,10)){
  "Generate Y values by specified X (factors) and beta (optional)."
  
  set.seed(seed) # Set random seed so the result reproducable

  X = X$X
  n = nrow(X)
  u = exp(sqrt(X^2)) + rnorm(n) 
  u = t(t(rowSums(u)))
  galat = rnorm(n)
  Y = X %*% beta
  Y.hat = -(X %*% beta) + u/5
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


# Another Simulating function -----
simulate_model2 <- function(i, j, k, s, seed=NULL){
  "Simulating OLS and FGLS model with given arguments."
  
  MAE_OLS = c() # Initialize empty lists
  MAE_FGLS = c()
  MSE_OLS = c()
  MSE_FGLS = c()
  AMSE_OLS = c()
  AMSE_FGLS = c()
  AIC_OLS = c()
  AIC_FGLS = c()
  
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
    amseOLS = mean(abs(beta - coef(OLS))^2)
    amseFGLS = mean(abs(beta - coef(FGLS))^2)
    aicOLS = AIC(OLS)
    aicFGLS = AIC(FGLS)
    
    MAE_OLS = c(MAE_OLS, maeOLS)
    MAE_FGLS = c(MAE_FGLS, maeFGLS)
    MSE_OLS = c(MSE_OLS, mseOLS)
    MSE_FGLS = c(MSE_FGLS, mseFGLS)
    AMSE_OLS = c(AMSE_OLS, amseOLS)
    AMSE_FGLS = c(AMSE_FGLS, amseFGLS)
    AIC_OLS = c(AIC_OLS, aicOLS)
    AIC_FGLS = c(AIC_FGLS, aicFGLS)
  }
  
  metric = cbind('MAE_OLS'=mean(MAE_OLS), 'MAE_FGLS'=mean(MAE_FGLS),
                 'MSE_OLS'=mean(MSE_OLS), 'MSE_FGLS'=mean(MSE_FGLS),
                 'AMSE_OLS'=mean(AMSE_OLS), 'AMSE_FGLS'=mean(AMSE_FGLS),
                 'AIC_OLS'=mean(AIC_OLS), 'AIC_FGLS'=mean(AIC_FGLS))
  rownames(metric) = c(paste(i, 'Ax', j, 'B(', k, ')', sep=''))
  metric = data.frame(metric)
  
  return(metric)
}



# User friendly simulating function -----
simulator <- function(i, j, k, rep, seed=NULL){
  "Simulating OLS and FGLS model with given arguments."

  set.seed(seed) # Setting random seeds so
  seeds = sample(1e6, rep) # the result reproducable
  
  # ols = fgls = fgls1 = fgls2 = c() # Placeholder for results of each iteration
  ols = fgls = fgls1 = c() # Placeholder for results of each iteration
  # res = list(ols, fgls, fgls1, fgls2) # Then bring it to the list
  res = list(ols, fgls, fgls1) # Then bring it to the list
  
  for (r in 1:rep) { # Iterating over s times
    X = generate_X(i, j, k)
    data = generate_y(X,seed=seeds[r])
    beta = data$beta
    Y = data$Y
    OLS = lm(Y~X+0) # Define an OLS Model
    u = resid(OLS) # Save the residual as new variable
    u1 = exp(fitted(lm(log(u^2)~log(abs(X+0.1))))) # Skedastic function
    ut = transform_u(u)
    w1 = exp(fitted(lm(log(ut^2)~log(abs(X+0.1)))))
    w2 = exp(fitted(lm(log(ut^2)~X)))
    FGLS = lm(Y~X+0, weights = 1/u1) # Define an FGLS model
    FGLS1 = lm(Y~X+0, weights = 1/w1) # Define an FGLS model
    # FGLS2 = lm(Y~X+0, weights = 1/w2) # Define an FGLS model
    
    # models = list(OLS, FGLS, FGLS1, FGLS2) 
    models = list(OLS, FGLS, FGLS1) 
    for (m in 1:length(models)){
      MAE = MSE = Bias = Var = c()
      mae = mean(abs(summary(models[[m]])$residual))
      mse = mean(summary(models[[m]])$residual^2)
      bias = abs(mean(beta - coef(models[[m]])))
      var = mse - bias
      MAE = c(MAE, mae)
      MSE = c(MSE, mse)
      Bias = c(Bias, bias)
      Var = c(Var, var)
      rowth = cbind(MAE, MSE, Bias, Var)
      res[[m]] = rbind(res[[m]], rowth)
    }
  }
  
  result = rbind(colMeans(res[[1]]), colMeans(res[[2]]), 
                 colMeans(res[[3]])) # Count the average from all iteraions
  
  name = c() # Give name for each model
  for (mod in c('OLS', 'correct', 'FGLS')) {
    name = c(name, paste(i, 'Ax', j, 'Bx', k, 'R(', mod, ')', sep=''))
  }
  rownames(result) = name
  
  return(result)
}


# Transform u function ------
 transform_u <- function(u, tres=2){
  "Function that transform u to have minimum desired value."
  ut = c()
  for (i in u){
    if (i^2 < tres^2) {
      ut = c(ut, tres)
    }
    else {
      ut = c(ut, i)
    }
  }
  return(ut)
}


# User friendly simulating function -----
simulator1 <- function(i, j, k, rep, seed=NULL){
  "Simulating OLS and FGLS model with given arguments."
  
  set.seed(seed) # Setting random seeds so
  seeds = sample(1e6, rep) # the result reproducable
  
  ols = fgls = fgls1 = c() # Placeholder for results of each iteration
  # res = list(ols, fgls, fgls1, fgls2) # Then bring it to the list
  res = list(ols, fgls, fgls1) # Then bring it to the list
  
  for (r in 1:rep) { # Iterating over s times
    X = generate_X(i, j, k)
    data = generate_y(X,seed=seeds[r])
    beta = data$beta
    Y = data$Y
    X = X$X
    OLS = lm(Y~X+0) # Define an OLS Model
    u = resid(OLS) # Save the residual as new variable
    u1 = exp(fitted(lm(log(u^2)~log(abs(X+0.1))))) # Skedastic function
    ut = transform_u(u)
    w1 = exp(fitted(lm(log(ut^2)~log(abs(X+0.1)))))
    w2 = exp(fitted(lm(log(ut^2)~X)))
    FGLS = lm(Y~X+0, weights = 1/u1) # Define an FGLS model
    FGLS1 = lm(Y~X+0, weights = 1/w1) # Define an FGLS model
    # FGLS2 = lm(Y~X+0, weights = 1/w2) # Define an FGLS model
    
    # models = list(OLS, FGLS, FGLS1, FGLS2) 
    models = list(FGLS, OLS, FGLS1) 
    for (m in 1:length(models)){
      AMSE = AIC = Fval = c()
      mse = mean(summary(models[[m]])$residual^2)
      aic = mean(AIC(models[[m]]))
      f = mean(summary(models[[m]])$f[1])
      AMSE = c(AMSE, mse)
      AIC = c(AIC, aic)
      Fval = c(Fval, f)
      rowth = cbind(AMSE, AIC, Fval)
      res[[m]] = rbind(res[[m]], rowth)
    }
  }
  
  result = rbind(colMeans(res[[1]]), colMeans(res[[2]]), 
                 colMeans(res[[3]])) # Count the average from all iteraions
  
  name = c() # Give name for each model
  for (mod in c('correct', 'OLS', 'FGLS')) {
    name = c(name, paste(i, 'Ax', j, 'Bx', k, 'R(', mod, ')', sep=''))
  }
  rownames(result) = name
  
  return(result)
}
