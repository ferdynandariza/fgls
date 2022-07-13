library(tidyr)

# Factors generator function
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


# Dependent variable generator function
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


# Membangkitkan data berukuran 60
X <- generate_X(3,4,5)
Y <- generate_y(X)
ds <- data.frame('A'=as.factor(X$A), 
                 'B'=as.factor(X$B), 
                 'R'=rep(rep(1:5, each=4), 3),
                 'Y'=round(Y$Y, 3))

# Simpan dalam layout faktorial RAL
da <- data.frame(pivot_wider(ds, names_from=B, values_from=Y))
colnames(da) <- c('Faktor A', 'Ulangan', 'B1', 'B2', 'B3', 'B4')
da

# Eksplorasi Data
par(mfrow=c(1,2))
plot(ds$Y, ds$A, ylab='Faktor A', xlab='Y')
plot(ds$Y, ds$B, ylab='Faktor B', xlab='Y')

par(mfrow=c(1,1))
qqnorm(Y$Y); qqline(Y$Y) # Normal probability plot

# Pendugaan
ols <- lm(Y~A*B, data=ds) # Menduga model dengan metode OLS

u <- transform_u(resid(lm(Y$Y~X$X))) # Membuat galat OLS terbatas pada nilai delta
w <- exp(fitted(lm(log(u^2)~log(abs(X$X+0.1))))) # Menduga galat dengan fungsi skedastik
fgls <- lm(Y~A*B, data=ds, weights = 1/w) # Menduga model dengan metode FGLS

anova(ols) # Menentukan tabel ANOVA model OLS
anova(fgls) # Menentukan tabel ANOVA model FGLS

mse_ols <- mean(summary(ols)$residuals^2)
mse_ols # Menampilkan MSE model OLS
mse_fgls <- mean(summary(fgls)$residuals^2)
mse_fgls # Menampilkan MSE model FGLS

AIC(ols) # Menghitung nilai AIC model OLS
AIC(fgls) # Menghitung nilai AIC model FGLS


