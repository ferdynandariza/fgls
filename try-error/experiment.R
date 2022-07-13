source('D:/Skripsi/r-codes/data-generator.R')

simulator1(3,3,5,10,42)

X <- generate_X(3,4,5)
Y <- generate_y(X)
ds <- data.frame('Y'=Y$Y, 'A'=as.factor(X$A), 'B'=as.factor(X$B))
plot(Y~A, data=ds)
anova(lm(Y~A*B, data=ds, weights = 1/w))
Y$beta
u <- transform_u(resid(lm(Y$Y~X$X)))
w <- exp(fitted(lm(log(u^2)~log(abs(X$X+0.1)))))
ols <- lm(Y$Y~X$X); summary(ols)
fgls <- lm(Y$Y~X$X, weights = 1/w); summary(fgls)

par(mfrow=c(2,1), family='serif', pch=19, lwd=2)
plot(Y$Y, as.factor(X$A), ylab='Faktor A', xlab='Y', axes=F, col='blue')
axis(2, at = c(1, 2, 3), labels = c('A1', 'A2', 'A3'), las=1)
axis(1)
plot(Y$Y, as.factor(X$B), ylab='Faktor B', xlab='Y', axes=F, col='red')
axis(2, at = c(1, 2, 3, 4), labels = c('B1', 'B2', 'B3', 'B4'), las=1)
axis(1)
qqnorm(Y$Y, axes=F)
qqline(Y$Y, lty=2, lwd=3, col='#666666')
mods <- lm(Y$Y~X$X)
plot(fitted(mods), resid(mods), axes=F, ylim=c(-6,6),
     xlab='Y dugaan', ylab='Galat', col='#666666')
axis(2, las=1, col='white');axis(1, col='white')
abline(mean(resid(mods)), 0, lty=2, lwd=3, col='gray')
abline(0,0, lwd=1000, col='#0000FF10')
abline(8,0.15, lwd=200, col='white')
abline(-9,-0.15, lwd=200, col='white')
points(fitted(mods), resid(mods), col='#666666')

plot(lm(Y$Y~X$X))

repetition = c()
for (k in 2:12){
  repetition = rbind(repetition, simulator(4,4,k,1000,42))
}
repetition
write.csv(round(repetition,4), 'on-repetition.csv')

r1 = r2 = r3 = c()
for (i in 1:(nrow(repetition)/3)){
  r1 = rbind(r1, repetition[(3*i-2),])
  r2 = rbind(r2, repetition[(3*i-1),])
  r3 = rbind(r3, repetition[(3*i),])
}
par(family='serif')

plot(2:11, r1[,2], type='l',col='gray', lty=1, 
     lwd=4, ylim=c(0,10), main='MSE terhadap Ulangan (r)',
     ylab='MSE', xlab='Ulangan (r)', frame.plot=F)
lines(2:11, r2[,2], col='red', lwd=4, lty=1)
lines(2:11, r3[,2], col='navy', lwd=4, lty=1)
legend(9, 6, legend = c('OLS', 'Correct', 'FGLS'), 
       col=c('gray','red','navy'), lty=1, lwd=4)


factors <- c()
for (i in 3:3) {
  for (j in 2:12){
    factors = rbind(factors, simulator1(i,j,5,10,123))
    #factors = rbind(factors, rep(NA, nrow(factors)))
  }
}
str(factors)
write.csv(round(factors,3), 'on-factors1.csv')

factors <- read.csv('on-factors1.csv')[-34,-1]
library(comprehenr)
correct <- to_vec(for (t in 1:nrow(factors)) if (t%%3==1) factors[t,1])
ols <- to_vec(for (t in 1:nrow(factors)) if (t%%3==2) factors[t,1])
fgls <- to_vec(for (t in 1:nrow(factors)) if (t%%3==0) factors[t,1])

par(family='serif', pch=19, lwd=2)
plot(2:12, ols, type='o', col='navy', ylim=c(0,max(ols)), frame.plot=F, 
     ylab='MSE', xlab='Level Faktor B (Faktor A = 3)', las=1)
lines(2:12, fgls, type='o', col='tomato')
lines(2:12,correct, type='o', col='gray')
legend('right', legend=c('OLS', '', 'FGLS', '', 'Correct'), lwd=2,
       pch=19, col=c('navy', 0, 'tomato', 0, 'gray'), box.col=F)

correct <- to_vec(for (t in 1:nrow(factors)) if (t%%3==1) factors[t,2])
ols <- to_vec(for (t in 1:nrow(factors)) if (t%%3==2) factors[t,2])
fgls <- to_vec(for (t in 1:nrow(factors)) if (t%%3==0) factors[t,2])

par(family='serif', pch=19, lwd=10)
plot(2:12,correct/ols, type='h', col='gray', xlim=c(1.5,16), axes=F,
     ylim=c(min(fgls/ols)-.1,max(correct[-12]/ols)), frame.plot=F, 
     ylab='AIC relatif thd OLS', xlab='Level Faktor B (Faktor A = 3)')
axis(1, at = 2:12, labels = 2:12)
axis(2, las=1)
lines(2:12-.25,fgls/ols, type='h', col='tomato')
lines(2:12+.25,ols/ols, type='h', col='navy')
legend('bottomright', legend=c('OLS', '', 'FGLS', '', 'Correct'), lwd=10,
       col=c('navy', 0, 'tomato', 0, 'gray'), box.col=F)


colors()
ols <- lm(Y~X)
u <- resid(ols)
w1 <- exp(fitted(lm(log(u^2)~log(abs(X+0.1)))))
w2 <- exp(fitted(lm(log(u^2)~X)))



transform_u(u)
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


# ----
(summary(lm(Y~X)))$f[1]
