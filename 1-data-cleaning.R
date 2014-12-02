library(data.table)

rm(list=ls())
gc()
options(max.print=1000)

setwd('~/Data/CT/Yaan')

calls <- readRDS('Rds/all_edge_nc.Rds')
locs <- readRDS('Rds/location_summary.Rds')
towns <- readRDS('Rds/town_code_etc.Rds')
fpdt <- readRDS('Rds/family_plan_list.Rds')

eqtime <- as.POSIXct('2013-04-20 8:02:00', tz='UTC')
eqtime.n <- as.numeric(eqtime)

# clean out bad calls
# the NAs here appear to be buttdials and are few
calls[, CALLED_NBR := as.numeric(as.character(CALLED_NBR))]

# identify calls vs texts
# tho apparently, DURATION == 0 should be enough to identify
call.types <- calls[, sort(unique(ETL_TYPE_NAME))]
calls[,is.text := ETL_TYPE_NAME==call.types[1]]

# calls.small <- calls[, list(SERV_ID, ACC_NBR,
#                             DURATION, CALL_TYPE,
#                             START_TIME, CALLED_NBR,
#                             CALLING_NBR, is.text)]
# print(object.size(calls.small), units='auto')

# how many directed dyads
edges <- calls[, .N, by=list(CALLED_NBR,CALLING_NBR)]
dyads <- calls[, list(egoall=.N), by=list(ego=CALLING_NBR,alter=CALLED_NBR)]
dyads <- merge(dyads,
               dyads[,list(ego=alter,alter=ego,alterall=egoall)],
               by=c('ego','alter'), all=TRUE)

# how many acct nbrs per service id?
uacc.nbrs <- calls[,length(unique(ACC_NBR)), by=SERV_ID]

# how many serv ids per acct nbr?
userv.ids <- calls[,length(unique(SERV_ID)), by=ACC_NBR]

# acc_nbr serv_id map
acc2serv <- calls[, list(SERV_ID=unique(SERV_ID)), by=ACC_NBR]

# serv_id identifies family plan
# acc_nbr identifies individual in family plan
# have multiple acc_nbr per serv_id, but not vice versa

# merge in service ids
dyads <- merge(dyads,
               acc2serv[, list(ego=ACC_NBR,ego.serv.id=SERV_ID)],
               by='ego', all.x=TRUE)
dyads <- merge(dyads,
               acc2serv[, list(alter=ACC_NBR,alter.serv.id=SERV_ID)],
               by='alter', all.x=TRUE)

# merge family plan into dyad
fpdt.merge <- fpdt[,list(famid=max(famid)),by=nbr]
dyads <- merge(dyads,
               fpdt.merge[,list(ego=nbr,ego.famid=famid)],
               by='ego', all.x=TRUE)
dyads <- merge(dyads,
               fpdt.merge[,list(alter=nbr,alter.famid=famid)],
               by='alter', all.x=TRUE)

dyads[, same_famid := FALSE]
dyads[ego.famid==alter.famid, same_famid := TRUE]


# look at post-quake calls
# system.time(pq.calls <- calls[START_TIME > eqtime,
#                 list(mintime=min(START_TIME)),
#                 by=list(CALLED_NBR,CALLING_NBR)])

calls.short <- calls[, list(CALLED_NBR,
                            CALLING_NBR,
                            start.time=as.numeric(START_TIME))]
system.time(pq.calls2 <- calls.short[start.time > eqtime.n,
                list(mintime=min(start.time) - eqtime.n),
                by=list(CALLED_NBR,CALLING_NBR)])

pq.calls2[, CALLED_order := order(mintime), by=CALLED_NBR]
pq.calls2[, CALLING_order := order(mintime), by=CALLING_NBR]

dyads <- merge(dyads,
               pq.calls2[,list(ego=CALLING_NBR,
                               alter=CALLED_NBR,
                               calling.order=CALLING_order,
                               calling.time=mintime)],
               by=c('ego','alter'), all.x=TRUE)
dyads <- merge(dyads,
               pq.calls2[,list(ego=CALLED_NBR,
                               alter=CALLING_NBR,
                               called.order=CALLED_order,
                               called.time=mintime)],
               by=c('ego','alter'), all.x=TRUE)
dyads[,call.time:=pmin(calling.time,called.time,na.rm=TRUE),by=ego]
dyads[,call.order:=order(call.time),by=ego]

dyads2 <- dyads[!is.na(ego.serv.id) | !is.na(ego.famid)]
dyads3 <- dyads[!is.na(ego.serv.id)]

saveRDS(dyads3,'Rds/dyad-statistics.Rds',compress=FALSE)
# dyads3 <- readRDS('Rds/dyad-statistics.Rds')

# add ivs:
# generate pre-quake calls
pre.calls <- calls[as.numeric(START_TIME) < eqtime.n]

# calling
call.dt <- pre.calls[(!is.text),
                     .N,
                     by=list(CALLING_NBR,CALLED_NBR)]
dyads3 <- merge(dyads3, call.dt[,list(egocall=N,
                                      ego=CALLING_NBR,
                                      alter=CALLED_NBR)],
                all.x=TRUE, by=c('ego','alter'))
dyads3 <- merge(dyads3, call.dt[,list(altercall=N,
                                      alter=CALLING_NBR,
                                      ego=CALLED_NBR)],
                all.x=TRUE, by=c('ego','alter'))
dyads3[,egocall_na := is.na(egocall)]
dyads3[,altercall_na := is.na(altercall)]
dyads3[(egocall_na), egocall := 0]
dyads3[(altercall_na), altercall := 0]
rm(call.dt)

# texting
text.dt <- pre.calls[(is.text),
                     .N,
                     by=list(CALLING_NBR,CALLED_NBR)]
dyads3 <- merge(dyads3, text.dt[,list(egotext=N,
                                      ego=CALLING_NBR,
                                      alter=CALLED_NBR)],
                all.x=TRUE, by=c('ego','alter'))
dyads3 <- merge(dyads3, text.dt[,list(altertext=N,
                                      alter=CALLING_NBR,
                                      ego=CALLED_NBR)],
                all.x=TRUE, by=c('ego','alter'))
dyads3[,egotext_na := is.na(egotext)]
dyads3[,altertext_na := is.na(altertext)]
dyads3[(egotext_na), egotext := 0]
dyads3[(altertext_na), altertext := 0]
rm(text.dt)


# regularity = weekly call deviance
weekly.calls <- pre.calls[,list(ntotal=.N,ntext=sum(is.text)),
                          by=list(CALLING_NBR,CALLED_NBR,time.week=week(START_TIME))]
weekly.calls <- weekly.calls[!is.na(time.week)]

pad <- function(x,len=length(x),pad=0) {
    c(x,rep(pad,len-length(x)))
}

# weekly.agg <- weekly.calls[,list(vartotal=var(pad(ntotal,18))),
#                            by=list(CALLING_NBR,CALLED_NBR)]
system.time(weekly.agg <- weekly.calls[,list(
    vartotal=(sum((ntotal-sum(ntotal/8))^2)+(8-.N)*sum(ntotal/8)^2)/7,
    vartext=(sum((ntotal-sum(ntext/8))^2)+(8-.N)*sum(ntext/8)^2)/7),
                           by=list(CALLING_NBR,CALLED_NBR)])

dyads3 <- merge(dyads3,weekly.agg[,list(ego=CALLING_NBR,
                                        alter=CALLED_NBR,
                                        egocall.var=vartotal,
                                        egotext.var=vartext)],
                by=c('ego','alter'), all.x=TRUE)
dyads3 <- merge(dyads3,weekly.agg[,list(ego=CALLED_NBR,
                                        alter=CALLING_NBR,
                                        altercall.var=vartotal,
                                        altertext.var=vartext)],
                by=c('ego','alter'), all.x=TRUE)
dyads3[, c('egocall.var_na','egotext.var_na',
           'altercall.var_na','altertext.var_na') :=
       list(is.na(egocall.var),is.na(egotext.var),
            is.na(altercall.var),is.na(altertext.var))]
dyads3[(egocall.var_na), egocall.var := 0]
dyads3[(egotext.var_na), egotext.var := 0]
dyads3[(altercall.var_na), altercall.var := 0]
dyads3[(altertext.var_na), altertext.var := 0]
rm(weekly.calls,weekly.agg)


# response time = median inter-call time
system.time(call.times <- calls[,list(start.time=as.numeric(START_TIME),
                                      dir=1),
                                by=list(ego=CALLING_NBR,alter=CALLED_NBR)])
call.times <- rbind(call.times,
                    call.times[,list(start.time,
                                     dir=0,
                                     ego=alter,
                                     alter=ego)])

# restrict to pairs with > 1 call, and restrict to alternating calls
system.time(call.times <- call.times[order(ego,alter,start.time)])
call.times[,samepair := ego == c(NA,head(ego,-1)) & alter == c(NA,head(alter,-1))]
call.times[is.na(samepair),samepair := FALSE]
call.times[,hasnext := samepair | c(tail(samepair,-1),FALSE)]
call.times <- call.times[(hasnext)]
call.times[,dirchange := dir - c(1,head(dir,-1))]
call.times[,time.gap := start.time - c(NA,head(start.time,-1))]

# restrict to calls where ego responds to alter
call.times <- call.times[dirchange==1 & samepair]

system.time(call.times.agg <- call.times[,list(med.intertime=median(time.gap),
                                               intertime.1hr=mean(time.gap < 3600)),
                                         by=list(ego,alter)])

# merge in
dyads3 <- merge(dyads3, call.times.agg,
                by=c('ego','alter'), all.x=TRUE)
dyads3[, med.intertime_na := is.na(med.intertime)]
dyads3[, intertime.1hr_na := is.na(intertime.1hr)]
dyads3[(med.intertime_na), med.intertime := 0]
dyads3[(intertime.1hr_na), intertime.1hr := 0]

# save and write!
# change na's to 0's

#dyads3[is.na(egocall),egocall := 0]
#dyads3[is.na(altercall),altercall := 0]
#dyads3[is.na(egotext),egotext := 0]
#dyads3[is.na(altertext),altertext := 0]

saveRDS(dyads3,'Rds/dyad-statistics2.Rds',compress=FALSE)


