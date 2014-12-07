library(data.table)
library(e1071)

rm(list=ls())
gc()
options(max.print=1000)

setwd('~/Data/CT/Yaan')

dyads3 <- readRDS('Rds/dyad-statistics2.Rds')

set.seed(1)
dyads3[,rselect := runif(.N)]

# first call variable
dyads3[, first_call := call.order == 1]

dyads3[, same_famid := as.numeric(same_famid)]
dyads3[, fisrt_call := as.numeric(first_call)]

rhs1 <- apply(expand.grid(c('egocall','altercall','egotext','altertext'),
                          c('','_na')),
              1,function(...) paste0(...,collapse=''))
rhs2 <- apply(expand.grid(c('egocall.var','altercall.var','egotext.var','altertext.var'),
                          c('','_na')),
              1,function(...) paste0(...,collapse=''))
rhs3 <- apply(expand.grid(c('med.intertime','intertime.1hr'),
                          c('','_na')),
              1,function(...) paste0(...,collapse=''))
rhs2 <- c(rhs1,rhs2)
rhs3 <- c(rhs2,rhs3)


# logistic models

m1 <- glm(reformulate(rhs1,'same_famid'),
          data=dyads3[rselect < 0.5], family=binomial)
summary(m1)

m2 <- glm(reformulate(rhs2,'same_famid'),
          data=dyads3[rselect < 0.5], family=binomial)
summary(m2)

m3 <- glm(reformulate(rhs3,'same_famid'),
          data=dyads3[rselect < 0.5], family=binomial)
summary(m3)


m4 <- glm(reformulate(rhs1,'first_call'),
          data=dyads3[rselect < 0.5], family=binomial)
summary(m4)

m5 <- glm(reformulate(rhs2,'first_call'),
          data=dyads3[rselect < 0.5], family=binomial)
summary(m5)

m6 <- glm(reformulate(rhs3,'first_call'),
          data=dyads3[rselect < 0.5], family=binomial)
summary(m6)

# save models
saveRDS(m1,'Rds/logit-m1.Rds',compress=FALSE)
saveRDS(m2,'Rds/logit-m2.Rds',compress=FALSE)
saveRDS(m3,'Rds/logit-m3.Rds',compress=FALSE)
saveRDS(m4,'Rds/logit-m4.Rds',compress=FALSE)
saveRDS(m5,'Rds/logit-m5.Rds',compress=FALSE)
saveRDS(m6,'Rds/logit-m6.Rds',compress=FALSE)


# predictions
error.func <- function(mod,y,crit=0.5,rhs=names(dyads3)) {
    train <- predict(mod, newdata=dyads3[rselect<crit,rhs,with=FALSE])
    test <- predict(mod, newdata=dyads3[rselect>1-crit,rhs,with=FALSE])

    if('glm' %in% class(mod)){
        train <- as.numeric(train>0)
        test <- as.numeric(test>0)
    } else if('svm' %in% class(mod)) {
        train <- as.numeric(train==1)
        test <- as.numeric(test==1)
    }

    train.t <- table(train,dyads3[rselect<crit,get(y)])
    test.t <- table(test,dyads3[rselect>1-crit,get(y)])

    train.rmse <- sqrt(mean((dyads3[rselect<crit,get(y)-(train)])^2, na.rm=TRUE))
    test.rmse <- sqrt(mean((dyads3[rselect>1-crit,get(y)-(test)])^2, na.rm=TRUE))

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


# svm models
svm.crit <- 0.002
system.time(svm.m1 <- svm(reformulate(rhs1,'same_famid'),
                          data=dyads3[rselect < svm.crit],
                          cost=100,gamma=1,type='C-classification'))
system.time(svm.m2 <- svm(reformulate(rhs2,'same_famid'),
                          data=dyads3[rselect < svm.crit],
                          cost=100,gamma=1,type='C-classification'))
system.time(svm.m3 <- svm(reformulate(rhs3,'same_famid'),
                          data=dyads3[rselect < svm.crit],
                          cost=100,gamma=1,type='C-classification'))

svm.m1.errs <- error.func(svm.m1,'same_famid',svm.crit,rhs1)
svm.m2.errs <- error.func(svm.m2,'same_famid',svm.crit,rhs2)
svm.m3.errs <- error.func(svm.m3,'same_famid',svm.crit,rhs3)

