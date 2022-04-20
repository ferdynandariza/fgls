source('data-generator.R')

X <- generate_X(4,4,5)
Y <- generate_y(X)

simulate_model(4,4,10,2,42)

repetition = c()
for (k in 3:12){
  repetition = rbind(repetition, simulate_model(4,4,k,1000,42))
}
repetition
write.csv(round(repetition,4), 'on-repetition.csv')

factors_list <- list()
factors <- c()
for (i in 2:12) {
  for (j in 2:12){
    factors = rbind(factors, simulate_model(i,j,5,1000,123))
  }
  factors = rbind(factors, rep(NA, nrow(factors)))
}
str(factors)
write.csv(round(factors,4), 'on-factors.csv')

# Show some plots -----
# plot(res[1:12,]$MSE_OLS, type='l',ylim = c(0,200))
# lines(res[1:12,]$MSE_FGLS)
# library(ggplot2)
# ggplot(data=res[1:11,],) +
#   geom_point(mapping = aes(x=1:11,y=MSE_OLS), col='blue') +
#   geom_line(mapping = aes(x=1:11,y=MSE_OLS), col='blue') +
#   geom_point(mapping = aes(x=1:11,y=MSE_FGLS), col='red') +
#   geom_line(mapping = aes(x=1:11,y=MSE_FGLS), col='red') +
#   #ggtitle('Perubahan Nilai MSE', 'terhadap jumlah faktor A') +
#   ylab('Mean Squared Error') +
#   xlab('Jumlah Faktor A')

