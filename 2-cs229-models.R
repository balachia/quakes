library(data.table)
library(e1071)
library(igraph)
library(RColorBrewer)
library(parallel)
library(txtplot)

rm(list=ls())
gc()
options(max.print=1000)

setwd('~/Data/CT/Yaan')

dyads3 <- readRDS('Rds/dyad-statistics2.Rds')

set.seed(1)
dyads3[,rselect := runif(.N)]
set.seed(1)
dyads3[,rselect2 := runif(1), by=ego]

# select by person id
dyads3[, `:=`(rselect=rselect2,rselect2=rselect)]

# first call variable
dyads3[, first_call := call.order == 1]

dyads3[, same_famid := as.numeric(same_famid)]
dyads3[, first_call := as.numeric(first_call)]

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


# svm settings
svm.crit <- 0.002


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


# short logistic regressions
lgt.m1 <- glm(reformulate(rhs1,'same_famid'),
              data=dyads3[rselect < svm.crit], family=binomial)
lgt.m2 <- glm(reformulate(rhs2,'same_famid'),
              data=dyads3[rselect < svm.crit], family=binomial)
lgt.m3 <- glm(reformulate(rhs3,'same_famid'),
              data=dyads3[rselect < svm.crit], family=binomial)
lgt.m4 <- glm(reformulate(rhs1,'first_call'),
              data=dyads3[rselect < svm.crit], family=binomial)
lgt.m5 <- glm(reformulate(rhs2,'first_call'),
              data=dyads3[rselect < svm.crit], family=binomial)
lgt.m6 <- glm(reformulate(rhs3,'first_call'),
              data=dyads3[rselect < svm.crit], family=binomial)

# save models
saveRDS(m1,'Rds/logit-m1.Rds',compress=FALSE)
saveRDS(m2,'Rds/logit-m2.Rds',compress=FALSE)
saveRDS(m3,'Rds/logit-m3.Rds',compress=FALSE)
saveRDS(m4,'Rds/logit-m4.Rds',compress=FALSE)
saveRDS(m5,'Rds/logit-m5.Rds',compress=FALSE)
saveRDS(m6,'Rds/logit-m6.Rds',compress=FALSE)

if (FALSE) {
    m1 <- readRDS('Rds/logit-m1.Rds')
    m2 <- readRDS('Rds/logit-m2.Rds')
    m3 <- readRDS('Rds/logit-m3.Rds')
    m4 <- readRDS('Rds/logit-m4.Rds')
    m5 <- readRDS('Rds/logit-m5.Rds')
    m6 <- readRDS('Rds/logit-m6.Rds')
}

# predictions
ctable <- function(pred,true) {
    tab <- matrix(0,2,2)
    tab[1,1] <- sum(pred == 0 & true == 0)
    tab[1,2] <- sum(pred == 0 & true == 1)
    tab[2,1] <- sum(pred == 1 & true == 0)
    tab[2,2] <- sum(pred == 1 & true == 1)
    colnames(tab) <- c('T0','T1')
    rownames(tab) <- c('P0','P1')
    tab
}

error.func <- function(mod,y,crit=0.5,rhs=names(dyads3),
                       train=NULL,test=NULL) {
    if(is.null(train)) {
        train <- predict(mod, newdata=dyads3[rselect<crit,rhs,with=FALSE])
    }
    if(is.null(test)) {
        test <- predict(mod, newdata=dyads3[rselect>1-crit,rhs,with=FALSE])
    }

    if('glm' %in% class(mod)){
        train <- as.numeric(train>0)
        test <- as.numeric(test>0)
    } else if('svm' %in% class(mod)) {
        train <- as.numeric(train==1)
        test <- as.numeric(test==1)
    }

    train.t <- ctable(train,dyads3[rselect<crit,get(y)])
    test.t <- ctable(test,dyads3[rselect>1-crit,get(y)])

    train.rmse <- sqrt(mean((dyads3[rselect<crit,get(y)-(train)])^2, na.rm=TRUE))
    test.rmse <- sqrt(mean((dyads3[rselect>1-crit,get(y)-(test)])^2, na.rm=TRUE))

    train.f1 <- (2*train.t[2,2]) / (2*train.t[2,2] + train.t[1,2] + train.t[2,1])
    test.f1 <- (2*test.t[2,2]) / (2*test.t[2,2] + test.t[1,2] + test.t[2,1])

#     list(train=train.t, test=test.t)
    list(train.rmse=train.rmse, test.rmse=test.rmse,
         train.f1=train.f1, test.f1=test.f1,
         train.ctable=train.t, test.ctable=test.t)
}

m1.errs <- error.func(m1,'same_famid')
m2.errs <- error.func(m2,'same_famid')
m3.errs <- error.func(m3,'same_famid')

m4.errs <- error.func(m4,'first_call')
m5.errs <- error.func(m5,'first_call')
m6.errs <- error.func(m6,'first_call')

lgt.m1.errs <- error.func(lgt.m1,'same_famid',svm.crit)
lgt.m2.errs <- error.func(lgt.m2,'same_famid',svm.crit)
lgt.m3.errs <- error.func(lgt.m3,'same_famid',svm.crit)

lgt.m4.errs <- error.func(lgt.m4,'first_call',svm.crit)
lgt.m5.errs <- error.func(lgt.m5,'first_call',svm.crit)
lgt.m6.errs <- error.func(lgt.m6,'first_call',svm.crit)

# grid parameter selection
param.crit <- 0.001
grid.func <- function(target,...) {
    ptm <- proc.time()
    svm.mparam <- svm(reformulate(rhs3,target),
                      data=dyads3[rselect < param.crit],
#                       cross=3,
                      type='C-classification',...)
    t1 <- proc.time() - ptm
    err <- error.func(svm.mparam,target,param.crit,rhs3)
    t2 <- proc.time() - ptm
    res <- c(list(...),
#              acc=svm.mparam$tot.accuracy,
             acc=err$test.rmse,
             f1=err$test.f1)
    do.call(cat,
            c(t1[3],t2[3] - t1[3],'::',
              res,'\n'))
    res
}

# SVM parameter selection
pgrid <- CJ(C=10^seq(-1,3,length.out=20),
            gamma=exp(seq(1,6,length.out=20)))
pgrid.sigmoid <- CJ(C=10^seq(-1,3,length.out=15),
                    gamma=exp(seq(1,6,length.out=10)),
                    coef0=seq(-2,2,length.out=5),
                    kernel='sigmoid')

system.time(res <- do.call(mcmapply,
               c(function(...) grid.func(target='same_famid',...),
                 mc.cores=22,as.list(pgrid))))
svm.rbf.sf <- as.data.table(t(res))
saveRDS(svm.rbf.sf,'Rds/svmgrid_rbf_sf.Rds')

# SVM sigmoid parameter selection
system.time(res <- do.call(mcmapply,
               c(function(...) grid.func(target='first_call',...),
                 mc.cores=22,as.list(pgrid))))
svm.rbf.fc <- as.data.table(t(res))
saveRDS(svm.rbf.fc,'Rds/svmgrid_rbf_fc.Rds')

system.time(res <- do.call(mcmapply,
               c(function(...) grid.func(target='same_famid',...),
                 mc.cores=22,as.list(pgrid.sigmoid))))
svm.sig.sf <- as.data.table(t(res))
saveRDS(svm.sig.sf,'Rds/svmgrid_sig_sf.Rds')

system.time(res <- do.call(mcmapply,
               c(function(...) grid.func(target='first_call',...),
                 mc.cores=22,as.list(pgrid.sigmoid))))
svm.sig.fc <- as.data.table(t(res))
saveRDS(svm.sig.sf,'Rds/svmgrid_sig_sf.Rds')

res <- mclapply(1:nrow(pgrid),
                mc.cores=20,
                function (i) {
    C <- pgrid[i,C]
    gamma <- pgrid[i,gamma]
    system.time(svm.mparam <- svm(reformulate(rhs3,'same_famid'),
                                  data=dyads3[rselect < param.crit],
                                  cost=C,gamma=gamma,type='C-classification',
                                  cross=3))
    err <- error.func(svm.mparam,'same_famid',param.crit,rhs3)
    res <- c(C=C,gamma=gamma,
             acc=svm.mparam$tot.accuracy,f1=err$test.f1)
    cat(C,gamma, res['acc'], 100 - res['acc'], err$test.f1, '\n')
    res
})
Csearch <- t(sapply(res, function (x) x))
Csearch <- as.data.table(Csearch)
txtplot(log10(Csearch$C),Csearch$acc)
txtplot(Csearch$gamma,Csearch$acc)

# SVM-sigmoid parameter selection
param.crit <- 0.001
pgrid.sigmoid <- CJ(C=10^seq(-1,3,length.out=15),
                    gamma=exp(seq(1,6,length.out=10)),
                    coef0=seq(-2,2,length.out=5))
res <- mclapply(1:nrow(pgrid.sigmoid),
                mc.cores=20,
                function (i) {
    C <- pgrid.sigmoid[i,C]
    gamma <- pgrid.sigmoid[i,gamma]
    coef0 <- pgrid.sigmoid[i,coef0]
    system.time(svm.mparam <- svm(reformulate(rhs3,'same_famid'),
                                  data=dyads3[rselect < param.crit],
                                  cost=C,gamma=gamma,coef0=coef0,
                                  type='C-classification',kernel='sigmoid',
                                  cross=3))
    err <- error.func(svm.mparam,'same_famid',param.crit,rhs3)
    res <- c(C=C,gamma=gamma,coef0=coef0,
             acc=svm.mparam$tot.accuracy,f1=err$test.f1)
    cat(C,gamma, res['acc'], 100 - res['acc'], err$test.f1, '\n')
    res
})
Csearch.sigmoid <- t(sapply(res, function (x) x))
Csearch.sigmoid <- as.data.table(Csearch.sigmoid)
txtplot(log10(Csearch.sigmoid$C),Csearch.sigmoid$acc)
txtplot(Csearch.sigmoid$gamma,Csearch.sigmoid$acc)

best.idx <- which.max(Csearch$acc)
cost <- Csearch[best.idx,C]
gamma <- Csearch[best.idx,gamma]

# svm models
system.time(svm.m1 <- svm(reformulate(rhs1,'same_famid'),
                          data=dyads3[rselect < svm.crit],
                          cost=cost,gamma=gamma,type='C-classification'))
system.time(svm.m2 <- svm(reformulate(rhs2,'same_famid'),
                          data=dyads3[rselect < svm.crit],
                          cost=cost,gamma=gamma,type='C-classification'))
system.time(svm.m3 <- svm(reformulate(rhs3,'same_famid'),
                          data=dyads3[rselect < svm.crit],
                          cost=cost,gamma=gamma,type='C-classification'))

system.time(svm.m4 <- svm(reformulate(rhs1,'first_call'),
                          data=dyads3[rselect < svm.crit],
                          cost=cost,gamma=gamma,type='C-classification'))
system.time(svm.m5 <- svm(reformulate(rhs2,'first_call'),
                          data=dyads3[rselect < svm.crit],
                          cost=cost,gamma=gamma,type='C-classification'))
system.time(svm.m6 <- svm(reformulate(rhs3,'first_call'),
                          data=dyads3[rselect < svm.crit],
                          cost=cost,gamma=gamma,type='C-classification'))

svm.m1.errs <- error.func(svm.m1,'same_famid',svm.crit,rhs1)
svm.m2.errs <- error.func(svm.m2,'same_famid',svm.crit,rhs2)
svm.m3.errs <- error.func(svm.m3,'same_famid',svm.crit,rhs3)

svm.m4.errs <- error.func(svm.m4,'first_call',svm.crit,rhs1)
svm.m5.errs <- error.func(svm.m5,'first_call',svm.crit,rhs2)
svm.m6.errs <- error.func(svm.m6,'first_call',svm.crit,rhs3)



# summaries
all.models <- CJ(type=c('lgt','svm'),m=1:3)
all.models <- rbind(all.models,CJ(type=c('lgt','svm'),m=4:6))
all.models[,mname := paste0(type,'.m',m,'.errs')]

msums <- sapply(all.models$mname, function(x) {
    moderrs <- get(x)
    c(train=moderrs$train.rmse,test=moderrs$test.rmse)
})
msums <- t(msums)

# test confusion megatable
lgt.m123 <- cbind(lgt.m1.errs$test.ctable,
                  lgt.m2.errs$test.ctable,
                  lgt.m3.errs$test.ctable)
svm.m123 <- cbind(svm.m1.errs$test.ctable,
                  svm.m2.errs$test.ctable,
                  svm.m3.errs$test.ctable)

lgt.m456 <- cbind(lgt.m4.errs$test.ctable,
                  lgt.m5.errs$test.ctable,
                  lgt.m6.errs$test.ctable)
svm.m456 <- cbind(svm.m4.errs$test.ctable,
                  svm.m5.errs$test.ctable,
                  svm.m6.errs$test.ctable)

ctable.m123 <- rbind(lgt.m123,svm.m123)
ctable.m456 <- rbind(lgt.m456,svm.m456)

# add names and shit
colnames(ctable.m123) <- paste0('M',rep(1:3,each=2),':T',rep(0:1,3))
rownames(ctable.m123) <- paste0(rep(c('Logit','SVM'),each=2),':P',rep(0:1,2))

colnames(ctable.m456) <- paste0('M',rep(4:6,each=2),':T',rep(0:1,3))
rownames(ctable.m456) <- paste0(rep(c('Logit','SVM'),each=2),':P',rep(0:1,2))


# network graphs
test.pred <- predict(svm.m3,newdata=dyads3[rselect > 1-svm.crit,rhs3,with=FALSE])
test.pred <- as.numeric(test.pred==1)
dyads3[rselect > 1-svm.crit,svm.m3.pred := test.pred]

tn.egos <- dyads3[same_famid==0 & svm.m3.pred==0,unique(ego)]
tp.egos <- dyads3[same_famid==1 & svm.m3.pred==1,unique(ego)]
fn.egos <- dyads3[same_famid==1 & svm.m3.pred==0,unique(ego)]
fp.egos <- dyads3[same_famid==0 & svm.m3.pred==1,unique(ego)]

varied.egos <- unique(c(tp.egos,fn.egos,fp.egos))

net.dyads <- dyads3[ego %in% varied.egos]
net.ego.stats <- net.dyads[,list(tp=sum(svm.m3.pred * same_famid)/.N,
                                 fp=sum(svm.m3.pred * (1-same_famid))/.N,
                                 fn=sum((1-svm.m3.pred) * same_famid)/.N),by=ego]

egos.all <- net.ego.stats[tp > 0 & fp > 0 & fn > 0, ego]
egos.tp.fn <- net.ego.stats[tp > 0 & fp == 0 & fn > 0, ego]
egos.tp.fp <- net.ego.stats[tp > 0 & fp > 0 & fn == 0, ego]
egos.fp.fn <- net.ego.stats[tp == 0 & fp > 0 & fn > 0, ego]


# TP, FP, FN
ego.dyads <- net.dyads[ego==egos.all[2],
                       list(ego,alter,
                            wgt=log(egocall+1)+1,
                            true=same_famid,
                            pred=svm.m3.pred)]
alters <- ego.dyads[,alter]
ego.dyads <- rbind(ego.dyads,
                   dyads3[ego %in% alters & alter %in% alters,
                          list(ego,alter,
                               wgt=log(egocall+1)+1,
                               true=NA,
                               pred=NA)])
g <- graph.data.frame(ego.dyads)

png('demo-egoall.png',width=1000,height=1000)
pal <- brewer.pal(8,'Dark2')
ego.dyads[,type := 6]
ego.dyads[true==1 & pred==1,type := 1]
ego.dyads[true==0 & pred==1,type := 2]
ego.dyads[true==1 & pred==0,type := 3]
vcols <- c(4,ego.dyads[!is.na(true),type])
plot(g,vertex.label=NA,vertex.color=pal[vcols],
     vertex.size=8,
     edge.width=ego.dyads$wgt,edge.arrow.mode='-')
legend('topleft',pch=19,cex=2,
       col=pal[c(1,6,2,3)],
       legend=c('TP','TN','FP','FN'))
dev.off()

# TP, FN
ego.dyads <- net.dyads[ego==egos.tp.fn[3],
                       list(ego,alter,
                            wgt=log(egocall+1)+1,
                            true=same_famid,
                            pred=svm.m3.pred)]
alters <- ego.dyads[,alter]
ego.dyads <- rbind(ego.dyads,
                   dyads3[ego %in% alters & alter %in% alters,
                          list(ego,alter,
                               wgt=log(egocall+1)+1,
                               true=NA,
                               pred=NA)])
g <- graph.data.frame(ego.dyads)

png('demo-egotpfn.png',width=1000,height=1000)
pal <- brewer.pal(8,'Dark2')
ego.dyads[,type := 6]
ego.dyads[true==1 & pred==1,type := 1]
ego.dyads[true==0 & pred==1,type := 2]
ego.dyads[true==1 & pred==0,type := 3]
vcols <- c(4,ego.dyads[!is.na(true),type])
plot(g,vertex.label=NA,vertex.color=pal[vcols],
     vertex.size=8,
     edge.width=ego.dyads$wgt,edge.arrow.mode='-')
legend('topleft',pch=19,cex=2,
       col=pal[c(1,6,2,3)],
       legend=c('TP','TN','FP','FN'))
dev.off()

# FP, FN
ego.dyads <- net.dyads[ego==egos.fp.fn[8],
                       list(ego,alter,
                            wgt=log(egocall+1)+1,
                            true=same_famid,
                            pred=svm.m3.pred)]
alters <- ego.dyads[,alter]
ego.dyads <- rbind(ego.dyads,
                   dyads3[ego %in% alters & alter %in% alters,
                          list(ego,alter,
                               wgt=log(egocall+1)+1,
                               true=NA,
                               pred=NA)])
g <- graph.data.frame(ego.dyads)

png('demo-egofpfn.png',width=1000,height=1000)
pal <- brewer.pal(8,'Dark2')
ego.dyads[,type := 6]
ego.dyads[true==1 & pred==1,type := 1]
ego.dyads[true==0 & pred==1,type := 2]
ego.dyads[true==1 & pred==0,type := 3]
vcols <- c(4,ego.dyads[!is.na(true),type])
plot(g,vertex.label=NA,vertex.color=pal[vcols],
     vertex.size=8,
     edge.width=ego.dyads$wgt,edge.arrow.mode='-')
legend('topleft',pch=19,cex=2,
       col=pal[c(1,6,2,3)],
       legend=c('TP','TN','FP','FN'))
dev.off()

