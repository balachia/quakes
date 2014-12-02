library(foreign)
library(data.table)
library(ffbase)

rm(list=ls())

setwd('~/Data/CT/Yaan')

resdts <- list()
i <- 1
for(x in paste0(3:6, 'edge')) {
    dt <- fread(paste0(x, '.csv'), verbose=TRUE, showProgress=TRUE)
    resdts[[i]] <- dt
    i <- i+1
    #print(resdts)
    #saveRDS(dt, file=paste0('Rds/',x,'.Rds'))
}

resdt <- rbindlist(resdts)
rm(dt, resdts)

saveRDS(resdt, paste0('Rds/all_edge.', Sys.info()['nodename'], '.Rds'))
system.time(resdt <- readRDS(paste0('Rds/all_edge.', Sys.info()['nodename'], '.Rds')))

system.time(numeric.cols <- lapply(resdt, function (x) {
    !any(is.na(as.numeric(x)))
}))

if(!is.numeric(resdt$SERV_ID)) resdt[, SERV_ID := as.numeric(SERV_ID)]
if(!is.numeric(resdt$ACC_NBR)) resdt[, ACC_NBR := as.numeric(ACC_NBR)]
if(!is.numeric(resdt$DURATION)) resdt[, DURATION := as.numeric(DURATION)]
if(!is.numeric(resdt$CALL_TYPE)) resdt[, CALL_TYPE := as.numeric(CALL_TYPE)]
if(!is.numeric(resdt$CALLING_NBR)) resdt[, CALLING_NBR := as.numeric(CALLING_NBR)]

#for(x in names(numeric.cols)[unlist(numeric.cols)]) {
    #print(x)
    #resdt[, x := as.numeric(get(x))]
#}

saveRDS(resdt, 'Rds/all_edge.Rds')
saveRDS(resdt, 'Rds/all_edge_nc.Rds', compress=FALSE)

# parse times
if(!any(class(resdt$START_TIME) == 'POSIXct')) resdt[, START_TIME := as.POSIXct(strptime(START_TIME, format='%Y/%m/%d %H:%M:%S', tz='UTC'))]
if(!any(class(resdt$END_TIME) == 'POSIXct')) resdt[, END_TIME := as.POSIXct(strptime(END_TIME, format='%Y/%m/%d %H:%M:%S', tz='UTC'))]


# transform remaining to factors
if(!is.factor(resdt$CALLED_AREA_CODE)) resdt[, CALLED_AREA_CODE := as.factor(CALLED_AREA_CODE)]
if(!is.factor(resdt$CALLING_AREA_CODE)) resdt[, CALLING_AREA_CODE := as.factor(CALLING_AREA_CODE)]
if(!is.factor(resdt$ETL_TYPE_NAME)) resdt[, ETL_TYPE_NAME := as.factor(ETL_TYPE_NAME)]
if(!is.factor(resdt$CALLED_NBR)) resdt[, CALLED_NBR := as.factor(CALLED_NBR)]

saveRDS(resdt, 'Rds/all_edge.Rds')
saveRDS(resdt, 'Rds/all_edge_nc.Rds', compress=FALSE)

# turn to ff
ffd <- as.ffdf(resdt)
save.ffdf(ffd, dir='ffdf/all_edge/', overwrite=TRUE)

