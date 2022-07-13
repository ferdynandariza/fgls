# Some helper Function ----
pivot = tidyr::pivot_wider

max_u_delta <- function(u, delta=3){
  "Choose bigger value between u_i and delta."
  maxi = ifelse(u>delta, delta, 
                ifelse(u<(-delta), -delta, u))
  return(maxi)
}

naming_X <- function(X, i, j){
  "Renaming columns of the Kronecker product matrix."
  name_A = name_B = name_AB = c()
  for (i_ in 1:i) {
    name_A = c(name_A, paste('A', i_, sep=''))
  }
  for (j_ in 1:j) {
    name_B = c(name_B, paste('B', j_, sep=''))
  }
  for (i_ in 1:i) {
    for (j_ in 1:j) {
      name_AB = c(name_AB, paste('A', i_, 'B', j_, sep=''))
    }
  }
  name_X = c('1', name_A, name_B, name_AB)
  colnames(X) = name_X
  return(X)
}

# Define parameter values ----
mu = 12
tau = c(-4.5, 1.0, 3.5) # fixed A factor
beta = c(3.3, 2.2, -0.1, -5.4) # fixed B factor
# set.seed(42)
# tau = rnorm(3, 0, 8) # random A factor
# beta = rnorm(4, 0, 5) # random B factor
tau_beta = c()
for (ta in tau) {
  for (bet in beta) {
    tau_beta = c(tau_beta, ta*bet)
  }
}

parameters = c(mu, tau, beta, tau_beta)

# Define factors' level and repetition ----
i = length(tau)
j = length(beta)
k = 5
n = i*j*k

A = factor(c(rep(1:i, each=j*k)))
B = factor(c(rep(rep(1:j, i), each=k)))

# Building matrices ----
Ii = diag(i) # Identity matrices
Ij = diag(j)
Ik = diag(k)
ones_i = rep(1, i) # 1s column matrices
ones_j = rep(1, j)
ones_k = rep(1, k)

# Obtain Kronecker Products ----
M_kr = kronecker(ones_i, kronecker(ones_j, ones_k))
A_kr = kronecker(Ii, kronecker(ones_j, ones_k))
B_kr = kronecker(ones_i, kronecker(Ij, ones_k))
AB_kr = kronecker(Ii, kronecker(Ij, ones_k))
X_matrix = cbind(M_kr, A_kr, B_kr, AB_kr)

# Naming the X matrix (to avoid confusion)
X = naming_X(X_matrix, i, j)

# Response generation ----
# Generate heteroskedastic errors
set.seed(1999) # Set random seed so errors can be reproducible
e = rnorm(n, 0, seq(1,k*2,length=n))
# e = rnorm(n)
# Applying linear model
Y = X %*% t(t(parameters)) + e

# Save data into DoE layout ----
ds = data.frame(
  'A'=A, 'B'=B, 
  'R'=rep(1:k, i*j),
  'Y'=round(Y, 3))
da = data.frame(pivot(ds, names_from=B, values_from=Y))
colnames(da) = c('Faktor A', 'Ulangan', 'B1', 'B2', 'B3', 'B4')

# Homogenity Test ----
car::leveneTest(ols, center='mean')

# Estimating ----
# Estimate with OLS
ols = lm(Y~A*B) 

# Pre FGLS
u = resid(ols) 
u_or_delta = max_u_delta(u, 7)
abs_X = ifelse(X==0, 1e-5, 1) 
skedastik = lm(log(u_or_delta^2)~log(abs_X))
log_u_hat = predict(skedastik)
u_hat_2 = exp(log_u_hat)

# Estimate with FGLS
fgls = lm(Y~A*B, weights = 1/u_hat_2) 

# ANOVA ----
anova(ols)
anova(fgls)

# Performance measurement ----
AIC(ols)
AIC(fgls)
BIC(ols)
BIC(fgls)
mse_ols = mean(summary(ols)$residuals^2)
mse_fgls = mean(summary(fgls)$residuals^2)
mse_ols
mse_fgls

# # Export data ----
# name <- paste('3x', j, 'x5.csv', sep='')
# write.csv(da, name, row.names = F)
# 
# # Save ANOVA
# o = anova(ols); fg = anova(fgls)
# namea <- paste('ANOVA_3x', j, 'x5.csv', sep='')
# nama_kol <- c('Df', 'SS', 'MS', 'F', 'P')
# df_anova <- rbind(data.frame(o), nama_kol, data.frame(fg))
# colnames(df_anova) <- nama_kol
# write.csv(df_anova, namea)
