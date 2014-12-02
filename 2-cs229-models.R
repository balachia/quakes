library(data.table)

rm(list=ls())
gc()
options(max.print=1000)

setwd('~/Data/CT/Yaan')

dyads3 <- readRDS('Rds/dyad-statistics2.Rds')

set.seed(1)
dyads3[,rselect := runif(.N)]

# first call variable
dyads3[, first_call := call.order == 1]


m1 <- glm(same_famid ~ egocall + altercall + egotext + altertext,
          data=dyads3[rselect < 0.5], family=binomial)
summary(m1)

m2 <- glm(same_famid ~ egocall + altercall + egotext + altertext + 
            egocall.var + altercall.var + egotext.var + altertext.var,
          data=dyads3[rselect < 0.5], family=binomial)
summary(m2)

m3 <- glm(same_famid ~ egocall + altercall + egotext + altertext + 
            egocall.var + altercall.var + egotext.var + altertext.var +
            med.intertime + intertime.1hr,
          data=dyads3[rselect < 0.5], family=binomial)
summary(m3)


m4 <- glm(first_call ~ egocall + altercall + egotext + altertext,
          data=dyads3[rselect < 0.5], family=binomial)
summary(m4)

m5 <- glm(first_call ~ egocall + altercall + egotext + altertext + 
            egocall.var + altercall.var + egotext.var + altertext.var,
          data=dyads3[rselect < 0.5], family=binomial)
summary(m5)

m6 <- glm(first_call ~ egocall + altercall + egotext + altertext + 
            egocall.var + altercall.var + egotext.var + altertext.var +
            med.intertime + intertime.1hr,
          data=dyads3[rselect < 0.5], family=binomial)
summary(m6)


# predictions
error.func <- function(mod,y) {
    train <- predict(mod, newdata=dyads3[rselect<0.5])
    test <- predict(mod, newdata=dyads3[rselect>0.5])

    train.t <- table(train > 0,dyads3[rselect<0.5,get(y)])
    test.t <- table(test > 0,dyads3[rselect>0.5,get(y)])

    train.rmse <- sqrt(mean((dyads3[rselect<0.5,get(y)-(train>0)])^2, na.rm=TRUE))
    test.rmse <- sqrt(mean((dyads3[rselect>0.5,get(y)-(test>0)])^2, na.rm=TRUE))

    train.f1 <- (2*train.t[2,2]) / (2*train.t[2,2] + train.t[1,2] + train.t[2,1])
    test.f1 <- (2*test.t[2,2]) / (2*test.t[2,2] + test.t[1,2] + test.t[2,1])

#     list(train=train.t, test=test.t)
    list(train.rmse=train.rmse, test.rmse=test.rmse,
         train.f1=train.f1, test.f1=test.f1)
}

m1.errs <- error.func(m1,'same_famid')
m2.errs <- error.func(m2,'same_famid')
m3.errs <- error.func(m3,'same_famid')

m4.errs <- error.func(m4,'first_call')
m5.errs <- error.func(m5,'first_call')
m6.errs <- error.func(m6,'first_call')

