

#get data
    d2 = as.data.table(baseline)
    d2 = d2[, .(ID, year_, box, nest, tarsus, weight, hatch_day, fledge_day, fledge_age,fledge_time, order, survive, epy, sex )]
    d2[, brood := max(order), by=nest]

#add delta time
    setorder(d2, nest, order)
    d2[, foo1 := fledge_time - shift(fledge_time, 1), by=c('nest', 'fledge_age')]
    d2[, foo2 := fledge_time - shift(fledge_time, -1), by=c('nest', 'fledge_age')]
    d2[, delta := ifelse(is.na(d2$foo1),abs(d2$foo2),
                  ifelse(is.na(d2$foo2),abs(d2$foo1),
                  ifelse(abs(d2$foo1)<abs(d2$foo2), abs(d2$foo1), abs(d2$foo2))))]
    d2[, foo1 :=NULL]
    d2[, foo2 :=NULL]
    d2[, delta := delta*60]

#Get summary statistics
    n = d2[, .(min_age=min(fledge_age), max_age=max(fledge_age), min_time=min(fledge_time), max_time=max(fledge_time), n=.N), by=nest]
    n[, age_range := max_age-min_age]
    n[, time_range := max_time-min_time]
    table(n$age_range)
    median(n$time_range[n$age_range==0])
    min(n$time_range[n$age_range==0 & n$n>1])*60
    max(n$time_range[n$age_range==0])

