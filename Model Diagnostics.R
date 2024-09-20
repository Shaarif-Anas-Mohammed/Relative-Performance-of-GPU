#DIAGNOSTIC PLOTS FOR OUTLIERS

n = nrow(data)
p = ncol(data)

# Outliers
e_star <- model.best$residuals/summary(model.best)$sigma
plot(x=model.best$fitted.values, y=e_star, xlab='Predicted Value', ylab='RStudent', main='Studentized Residual vs. Predicted Value', )
abline(h=1.96, col='red')
abline(h=-1.96, col='red')

#leverage
h.ii <- hatvalues(model=model.best)
p <- length(model.best$coefficients)
round(h.ii[h.ii>2*p/n], 4)

# rstudent vs. leverage
plot(x=h.ii, y=e_star, xlab='Leverage', ylab='Rstudent', main = 'Studentized Residual vs. Leverage')
abline(v=2*p/n, col='red')
abline(h=1.96, col='red')
abline(h=-1.96, col='red')

# DFFITS vs. observation number
dffits.data <- dffits(model=model.best)
dffits.data[abs(dffits.data)>2*sqrt(p/n)]  

plot(x=1:n, y=dffits.data, xlab='Observation number', ylab='DFFITS', main='DFFITS vs. observation number')
abline(h=0)
abline(h=c(-2*sqrt(p/n), 2*sqrt(p/n)), col='red')

# cook's distance
cook.i <- cooks.distance(model=model.best)
plot(x=1:n, y= cook.i, xlab='Observation number', ylab="Cook's D", main = "Cook's D vs. observation number")
abline(h=4/n, col='red')


#WORKING ON OUTLIERS
#Studentized deleted residuals;
alpha = 0.05
n = nrow(data)
t.d = rstudent(model.best)

#Bonferronis Test for outliers;
qt(p=1-alpha/(2*n), df=model.best$df.residual-1)
t.d[abs(t.d) > qt(p=1-alpha/(2*n), df=model.best$df.residual-1)]

#DFFITS, DFBETAS and Cook's Distance;
dffits.data = dffits(model.best)
cook.data = cooks.distance(model.best)
dfbeta.data = dfbetas(model.best)

#Check for Influence;
n = nrow(data)
p = ncol(data)
dffits.data[dffits.data > 2*(sqrt(p/n))]
cook.data[cook.data > qf(0.50,p,(n-p))]
dfbeta.data[dfbeta.data > 2/(sqrt(n))]

#leverage
h.ii <- hatvalues(model=model.best)
p <- length(model.best$coefficients)
round(h.ii[h.ii>2*p/n], 4)

#Add an observation number variable to existing data set;
library(dplyr)
data = data %>%
  mutate(obs = 1:209)

#Create new data frame without 10,32,200 observation
data.206 = data[c(-10,-32,-200),]
attach(data.206)
model.best.206 =lm(prp ~ myct + mmin + mmax + cach + chmax + myct:cach + 
                     mmin:mmax + mmin:cach + mmin:chmax + mmax:chmax, data.206)
summary(model.best.206)

#Predict Y variables with regression equation of 209 and 206 observations respectively;
y.209 = predict(model.best,data.206)
y.206 = predict(model.best.206,data.206)

#create another variable for percent change in y;
y.per.chg = (((y.206 - y.209)/ y.209)*100)
mean(abs(y.per.chg))
#create a table with obs number, y.209, y.206 and y.per.chg;
y.table = data.frame(data.206$obs,y.209,y.206,y.per.chg)

#Write the table results as .xlsx file
library("writexl")
write_xlsx(y.table,"C:\\Users\\SHAARIF ANAS\\Desktop\\MAT 456\\Final Project\\y.table.xlsx")

#CHECKING AFTER REMOVING 10,32 AND 200 th OBSERVATION

n = nrow(data.206)
p = ncol(data.206)

# Outliers
e_star <- model.best.206$residuals/summary(model.best.206)$sigma
plot(x=model.best.206$fitted.values, y=e_star, xlab='Predicted Value', ylab='RStudent', main='Studentized Residual vs. Predicted Value', )
abline(h=1.96, col='red')
abline(h=-1.96, col='red')

#leverage
h.ii <- hatvalues(model=model.best.206)
p <- length(model.best.206$coefficients)
round(h.ii[h.ii>2*p/n], 4)

# rstudent vs. leverage
plot(x=h.ii, y=e_star, xlab='Leverage', ylab='Rstudent',main = 'Studentized Residual vs. Leverage')
abline(v=2*p/n, col='red')
abline(h=1.96, col='red')
abline(h=-1.96, col='red')

# DFFITS vs. observation number
n = nrow(data.206)
p = ncol(data.206)
dffits.data.206 <- dffits(model=model.best.206)
dffits.data.206[abs(dffits.data.206)>2*sqrt(p/n)]  

plot(x=1:n, y=dffits.data.206, xlab='Observation number', ylab='DFFITS', main='DFFITS vs. observation number')
abline(h=0)
abline(h=c(-2*sqrt(p/n), 2*sqrt(p/n)), col='red')

# cook's distance
cook.i <- cooks.distance(model=model.best.206)
plot(x=1:n, y= cook.i, xlab='Observation number', ylab="Cook's D",main = "Cook's D vs. observation number")
abline(h=4/n, col='red')



#REMOVING ANOTHER SET OF OBSERVATIONS
#Create new data frame without  observation
data.197 = data.206[c(-30,-94,-95,-136,-152,-155,-165,-197,-205),] #observation numbers change as 3 observations are already removed
attach(data.197)
model.best.197 =lm(prp ~ myct + mmin + mmax + cach + chmax + myct:cach + 
                     mmin:mmax + mmin:cach + mmin:chmax + mmax:chmax, data.197)
summary(model.best.197)

#Predict Y variables with regression equation of 209 and 206 observations respectively;
y.197 = predict(model.best.197,data.197)
y.206 = predict(model.best.206,data.197)

#create another variable for percent change in y;
y.per.chg = (((y.197 - y.206)/ y.206)*100)
mean(abs(y.per.chg))

#create a table with obs number, y.209, y.206 and y.per.chg;
y.table = data.frame(data.197$obs,y.206,y.197,y.per.chg)

#Write the table results as .xlsx file
library("writexl")
write_xlsx(y.table,"C:\\Users\\SHAARIF ANAS\\Desktop\\MAT 456\\Final Project\\y.table197.xlsx")

#CHECKING AFTER REMOVING another set of OBSERVATION
n = nrow(data.197)
p = ncol(data.197)

# Outliers
e_star <- model.best.197$residuals/summary(model.best.197)$sigma
plot(x=model.best.197$fitted.values, y=e_star, xlab='Predicted Value', ylab='RStudent', main='Studentized Residual vs. Predicted Value', )
abline(h=1.96, col='red')
abline(h=-1.96, col='red')

#leverage
h.ii <- hatvalues(model=model.best.197)
p <- length(model.best.197$coefficients)
round(h.ii[h.ii>2*p/n], 4)

# rstudent vs. leverage
plot(x=h.ii, y=e_star, xlab='Leverage', ylab='Rstudent',main = 'Studentized Residual vs. Leverage')
abline(v=2*p/n, col='red')
abline(h=1.96, col='red')
abline(h=-1.96, col='red')

# DFFITS vs. observation number
dffits.data.197 <- dffits(model=model.best.197)
dffits.data.197[abs(dffits.data.197)>2*sqrt(p/n)]  

plot(x=1:n, y=dffits.data.197, xlab='Observation number', ylab='DFFITS', main='DFFITS vs. observation number')
abline(h=0)
abline(h=c(-2*sqrt(p/n), 2*sqrt(p/n)), col='red')

# cook's distance
cook.i <- cooks.distance(model=model.best.197)
plot(x=1:n, y= cook.i, xlab='Observation number', ylab="Cook's D",main = "Cook's D vs. observation number")
abline(h=4/n, col='red')


#final model
model.final =lm(prp ~ myct + mmin + mmax + cach + chmax + myct:cach + 
                  mmin:mmax + mmin:cach + mmin:chmax + mmax:chmax, data.206)
summary(model.final)

#Checking Model Assumptions

n = nrow(data.206)
p = ncol(data.206)
#Correct specification of explanatory variables;
par(mfrow=c(2,3))
title(main = 'Residuals vs. Explanatory variables')
plot(x=myct, y=model.final$residuals, xlab='myct', ylab='Residual')
abline(h=0, col='red')

plot(x=mmin, y=model.final$residuals, xlab='mmin', ylab='Residual')
abline(h=0, col='red')

plot(x=mmax, y=model.final$residuals, xlab='mmax', ylab='Residual')
abline(h=0, col='red')

plot(x=cach, y=model.final$residuals, xlab='cach', ylab='Residual')
abline(h=0, col='red')

plot(x=chmax, y=model.final$residuals, xlab='chmax', ylab='Residual')
abline(h=0, col='red')

# e vs. Y_hat
plot(x=model.final$fitted.values, y=model.final$residuals, xlab='Predicted Value', ylab='Residual', main='Residual vs. Predicted Value')
abline(h=0, col='red')

# Independence
plot(x=seq(1:n), y=model.final$residuals, xlab='Observation Number', ylab='Residual', main='Sequence Plot of the Residuals', type='l', col='Red')

# Normality
hist(model.final$residuals, prob=TRUE, xlab='Residual', main='Histogram of Residual')
curve(dnorm(x, mean=mean(model.final$residuals), sd=sd(model.final$residuals)), add=TRUE)

qqnorm(model.final$residuals, col='Blue')
qqline(model.final$residuals)

#VIF
vif(model.final)