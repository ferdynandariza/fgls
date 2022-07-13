source('D:/Skripsi/r-codes/data-generator.R')
library(tidyr)

mse_ols = mse_fgls = aic_ols = aic_fgls = n = c()
for (j in 4:12) {
  
  # Bangkitkan data ----
  X <- generate_X(3,j,5)
  Y <- generate_y(X)
  ds <- data.frame('Y'=round(Y$Y, 3), 'A'=as.factor(X$A), 
                   'B'=as.factor(X$B), 
                   'R'=rep(rep(1:5, each=j), 3))
  da <- data.frame(pivot_wider(ds, names_from=B, values_from=Y))
  
  # # Plot ----
  # par(mfrow=c(1,2), family='serif', pch=19, lwd=2)
  # plot(ds$Y, ds$A, ylab='Faktor A', xlab='Y', axes=F, col='#00000044')
  # axis(2, at = c(1, 2, 3), col='white', las=1)
  # axis(1, col='white')
  # plot(ds$Y, ds$B, ylab='Faktor B', xlab='Y', axes=F, col='#00000044')
  # axis(2, at = 1:j, las=1, col='white')
  # axis(1, col='white')
  # 
  # par(mfrow=c(1,1))
  # qqnorm(Y$Y, axes=F)
  # qqline(Y$Y, lty=2, lwd=3, col='#666666')

  # # Export data ----
  # name <- paste('3x', j, 'x5.csv', sep='')
  # write.csv(da, name, row.names = F)
  
  # Pendugaan ----
  ols <- lm(Y~A*B, data=ds); #summary(ols); 
  o <- anova(ols)
  u <- transform_u(resid(lm(Y$Y~X$X)))
  w <- exp(fitted(lm(log(u^2)~log(abs(X$X+0.1)))))
  fgls <- lm(Y~A*B, data=ds, weights = 1/w); #summary(fgls); 
  fg <- anova(fgls)
  
  # Evaluasi ----
  mse_ols <- c(mse_ols, mean(summary(ols)$residuals^2))#o[4,3])
  mse_fgls <- c(mse_fgls, mean(summary(fgls)$residuals^2))#fg[4,3])
  aic_ols <- c(aic_ols, AIC(ols))
  aic_fgls <- c(aic_fgls, AIC(fgls))
  n = c(n, nrow(ds))
  
  # # Save ANOVA ----
  # namea <- paste('ANOVA_3x', j, 'x5.csv', sep='')
  # nama_kol <- c('Df', 'SS', 'MS', 'F', 'P')
  # df_anova <- rbind(data.frame(o), nama_kol, data.frame(fg))
  # colnames(df_anova) <- nama_kol
  # write.csv(df_anova, namea)
}

eval <- round(
  data.frame(n, mse_ols, mse_fgls, aic_ols, aic_fgls),
  3)
write.csv(eval, 'Evaluasi.csv', row.names=F)

plot(mse_ols~n, data=eval, type='l', ylim=c(0,60))
lines(mse_fgls~n, data=eval)
plot(eval$n-1.75, eval$aic_ols/eval$aic_ols, type='h', 
     lwd=20, col='navy')
lines(eval$n+1.75, eval$aic_fgls/eval$aic_ols, type='h', 
      lwd=20, col='tomato')
mean((predict(fgls)-ds$Y)^2)

plot(fitted(ols), resid(ols), axes=F, ylim=c(-6,6),
     xlab='Y dugaan', ylab='Galat', col='#00000011',
     pch=19, )
axis(2, las=1, col='white');axis(1, col='white')
abline(mean(resid(ols)), 0, lty=2, lwd=3, col='gray')
abline(0,0, lwd=1000, col='#0000FF10')
abline(9,-0.133, lwd=200, col='white')
abline(-10,0.133, lwd=200, col='white')
points(fitted(ols), resid(ols), pch=19, col='#00000088')