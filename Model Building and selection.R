#Read the data with Appropriate column names;
data = read.delim(file.choose(),header=F,sep=",",col.names = c('myct','mmin','mmax','cach','chmin','chmax','prp'))
attach(data)

model1 = lm(prp ~myct+mmin+mmax+cach+chmin+chmax, data)
summary(model1)

#partial F-tests on insignificant variables
model.r = lm(prp ~myct+mmin+mmax+cach+chmax)
summary(model.r)

anova(model1, model.r)

#scatter plot matrix and correlation matrix
library(car)
scatterplotMatrix(formula=~prp+myct+mmin+mmax+cach+chmax)

cor = round(cor(cbind(prp,myct,mmin,mmax,cach,chmax), method = "pearson"),4)
cor=as.data.frame(cor)
library("writexl")
write_xlsx(cor,"C:\\Users\\SHAARIF ANAS\\Desktop\\MAT 456\\Final Project\\corr.xlsx")

#Added variable plots
library(car)
avPlots(model=model.r)

#step wise model selection

#step wise using only square terms
modelsq.for = lm(prp~1)
sel.for.sq = step(modelsq.for, direction="forward", scope=list(lower=prp~1, upper=prp ~ myct + mmin + mmax + cach + chmax +
                                                                 I(myct^2) + I(mmin^2) + I(mmax^2) + I(cach^2) + I(chmax^2)))
sel.for.sq
summary(sel.for.sq)

modelsq.back = lm(prp ~ myct + mmin + mmax + cach + chmax +
                    I(myct^2) + I(mmin^2) + I(mmax^2) + I(cach^2) + I(chmax^2))
sel.back.sq = step(modelsq.back, direction="backward", scope=list(lower=prp~1, upper=prp ~ myct + mmin + mmax + cach + chmax +
                                                                    I(myct^2) + I(mmin^2) + I(mmax^2) + I(cach^2) + I(chmax^2)))
sel.back.sq
summary(sel.back.sq)

#step wise selection using only interaction term
modelint.for = lm(prp~1)
sel.for.int = step(modelint.for, direction="forward", scope=list(lower=prp~1, upper=prp ~ myct + mmin + mmax + cach + chmax +
                                                                   myct:mmin + myct:mmax + myct:cach + myct:chmax +
                                                                   mmin:mmax + mmin:cach + mmin:chmax +
                                                                   mmax:cach + mmax:chmax +
                                                                   cach:chmax))
sel.for.int
summary(sel.for.int)

modelint.back = lm(prp ~ myct + mmin + mmax + cach + chmax +
                     myct:mmin + myct:mmax + myct:cach + myct:chmax +
                     mmin:mmax + mmin:cach + mmin:chmax +
                     mmax:cach + mmax:chmax +
                     cach:chmax)
sel.back.int = step(modelint.back, direction="backward", scope=list(lower=prp~1, upper=prp ~ myct + mmin + mmax + cach + chmax +
                                                                      myct:mmin + myct:mmax + myct:cach + myct:chmax +
                                                                      mmin:mmax + mmin:cach + mmin:chmax +
                                                                      mmax:cach + mmax:chmax +
                                                                      cach:chmax))
sel.back.int
summary(sel.back.int)

#stepwise selection using both square and interaction term
model1.for = lm(prp~1)
sel.for.1 = step(model1.for, direction="forward", scope=list(lower=prp~1, upper=prp ~ myct + mmin + mmax + cach + chmax +
                                                               I(myct^2) + I(mmin^2) + I(mmax^2) + I(cach^2) + I(chmax^2) +
                                                               myct:mmin + myct:mmax + myct:cach + myct:chmax +
                                                               mmin:mmax + mmin:cach + mmin:chmax +
                                                               mmax:cach + mmax:chmax +
                                                               cach:chmax))
sel.for.1
summary(sel.for.1)

model1.back = lm(prp ~ myct + mmin + mmax + cach + chmax +
                   myct:mmin + myct:mmax + myct:cach + myct:chmax +
                   I(myct^2) + I(mmin^2) + I(mmax^2) + I(cach^2) + I(chmax^2) +
                   mmin:mmax + mmin:cach + mmin:chmax +
                   mmax:cach + mmax:chmax +
                   cach:chmax)
sel.back.1 = step(model1.back, direction="backward", scope=list(lower=prp~1, upper=prp ~ myct + mmin + mmax + cach + chmax +
                                                                  I(myct^2) + I(mmin^2) + I(mmax^2) + I(cach^2) + I(chmax^2) +
                                                                  myct:mmin + myct:mmax + myct:cach + myct:chmax +
                                                                  mmin:mmax + mmin:cach + mmin:chmax +
                                                                  mmax:cach + mmax:chmax +
                                                                  cach:chmax))
sel.back.1
summary(sel.back.1)

#Function for AIC and BIC for First order model;
aic.bic = function (model){
  mod.fit = lm(model)
  aic.mod = AIC(mod.fit)
  bic.mod = BIC(mod.fit)
  data.frame(AIC=aic.mod, BIC=bic.mod)
}

sel.for.sq = aic.bic(sel.for.sq)
sel.back.sq = aic.bic(sel.back.sq)
sel.for.int = aic.bic(sel.for.int)
sel.back.int = aic.bic(sel.back.int)
sel.for.1 = aic.bic(sel.for.1)
sel.back.1 = aic.bic(sel.back.1)
model.r = aic.bic(model.r)
model1 = aic.bic(model1)



aic_bic = rbind(sel.for.sq, sel.back.sq, sel.for.int, sel.back.int,sel.for.1, sel.for.1, model.r,model1)
mod.name =  c('modelsq.for', 'modelsq.back', 'modelint.for', 'modelint.back', 'model1.for', 'model1.back','model.r', 'model1')

aic_bic.table = as.data.frame(cbind(mod.name,aic_bic))

#Write the table results as .xlsx file
library("writexl")
write_xlsx(aic_bic.table,"C:\\Users\\SHAARIF ANAS\\Desktop\\MAT 456\\Final Project\\Model AIC BIC.xlsx")


#chosen best model
model.best = lm(prp ~ myct + mmin + mmax + cach + chmax + myct:cach + 
                  mmin:mmax + mmin:cach + mmin:chmax + mmax:chmax)
summary(model.best)
