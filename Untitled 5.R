library(ggplot2)
library(dplyr)
library(mvtnorm)
install.packages("woolridge")
install.packages("multcomp")
library(multcomp)
library(car)

#2
data("wage2")
fit=lm(lwage~educ+exper+tenure, data=wage2)
summary(fit)

#Way 1
fit$coefficients[3]
fit$coefficients[4]

B3_B2= fit$coefficients[4]-fit$coefficients[3]
B3_B2 #t-statistic numerator

vcov(fit)

se_B3_B2=sqrt(vcov(fit)[3,3]+vcov(fit)[4,4]-2*vcov(fit)[3,4]) #t-statistic denominator

t1=B3_B2/se_B3_B2
t1

sig_level=0.05
critical_val=qt(1-sig_level/2,df=fit$df.residual) #finding critical value for 5% significance
lower=B3_B2-critical_val*se_B3_B2
upper=B3_B2+critical_val*se_B3_B2
sprintf("95 percent confidence interval= [%.3f %.3f]", lower, upper)

#Way 2
wage2$theta=wage2$exper+wage2$tenure #make that new variable 
rewritten_fit<-lm(lwage~educ+theta+tenure,data=wage2) 
summary(rewritten_fit) #we see it's the same
confint(rewritten_fit,'tenure',level=0.95,df=fit$df.residual)

#3
data("k401ksubs")
ksubs <- k401ksubs[k401ksubs$fsize== 1,] #only fsize = 1
fit=lm(nettfa~inc+age, data=ksubs) 
summary(fit) #result

coef(fit)
fit$coefficients[3] #B2 is for age
coef(summary(fit))[, 2][3] #getting the se for age

sig_level=0.01
critical_val=qt(sig_level,df=fit$df.residual)
critical_val

t <- (fit$coefficients[3] - 1) / coef(summary(fit))[, 2][3]
t

pnorm(t)

#4
#ii
data("discrim")
discrim2 <- discrim[,c('prpblck', 'prppov')]
discrim2$lpsoda <- log(discrim$psoda)
discrim2$lincome <- log(discrim$income)
cor.test(discrim2$prppov, discrim2$lincome) 

fit=lm(lpsoda~prpblck+lincome+prppov, data=discrim2)
summary(fit) 

coef(summary(fit))[, 3][4]
2*(1-pnorm(coef(summary(fit))[, 3][4]))

coef(summary(fit))[, 3][3]
2*(1-pnorm(coef(summary(fit))[, 3][3]))

#iii
discrim2$lhseval <- log(discrim$hseval)
fit2=lm(lpsoda~prpblck+lincome+prppov+lhseval, data=discrim2)
summary(fit2)
coef(summary(fit2))[, 3][5]
2*(1-pnorm(coef(summary(fit2))[, 3][5]))

#iv
coef(summary(fit2))[, 3][4]
2*(1-pnorm(coef(summary(fit2))[, 3][4]))

coef(summary(fit))[, 3][3]
2*(pnorm(coef(summary(fit))[, 3][3]))

restricted=lm(lpsoda~prpblck+lhseval, data=discrim2) 
unrestricted=lm(lpsoda~prpblck+lincome+prppov+lhseval, data=discrim2) 
ssr_rest=sum(restricted$residuals^2)
ssr_unrest=sum(unrestricted$residuals^2)
F4=((ssr_rest-ssr_unrest)/2)/(ssr_unrest/unrestricted$df.residual) 
F4

qf(0.95 , df1=2, df2=unrestricted$df.residual)
1-pf(F4, df1=2, df2=unrestricted$df.residual)
