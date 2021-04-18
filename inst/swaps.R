
require(SNB2)
require(sdb)
require(glue)
require(data.table)
require(ggplot2)
require(chron)
require(gridExtra)
require(dplyr)


#Call datasets
  chicks = data.table(chicks)
  swaps = data.table(swaps)
  fledge = data.table(fledge)
  hatch_date = data.table(hatch_date)
  dead = data.table(dead)


#Combine datasets
  d = merge(chicks[,.(box, ring, weight, p3, transp)], fledge, by=c("transp"), all.x=TRUE, all.y=TRUE)
  d = merge(d, hatch_date, by=c("box"))
  d = merge(d, swaps[,.(transp, swap_date, foster_box)], by=c("transp"), all.x=TRUE)
  d = merge(d, hatch_date, by.x=c("foster_box"), by.y=c("box"), suffixes=c("","_foster"), all.x=TRUE)
  d = merge(d, dead, by=c("ring"), all.x=TRUE)

#Tidy dataset and create new variables
  d = d[,.(box, transp, ring, weight, p3, foster_box, fledge_box, hatch_date, fledge, fledge_date, fledge_time, hatch_date, hatch_date_foster, swap_date, dead)]
  d[, fledge_day := yday(fledge_date)]
  d[, fledge_age := yday(fledge_date) - yday(hatch_date)]
  d[, fledge_time := as.numeric(chron(times=fledge_time))*24 + 2] #correcting for 2 hour time discrepancy
  d[, fledge_agetime := fledge_age + fledge_time/24]
  d[, group := ifelse(yday(swap_date)-yday(hatch_date)==14,"younger","older")]
  d[, treat := ifelse(hatch_date==hatch_date_foster,"control","treatment")]
  d[, treat_num := ifelse(hatch_date==hatch_date_foster,1,2) ]
  d[, age_diff := as.numeric(as.Date(hatch_date)-as.Date(hatch_date_foster))]
  d[, swap_date := NULL]
  d[, hatch_date := NULL]
  d[, hatch_date_foster := NULL]
  d[, fledge_box := ifelse(!is.na(foster_box), foster_box, box)]
  d[, brood := .N, by=.(fledge_box)]
  d[, dead :=ifelse(is.na(dead),0,1)]
  #Create treatment variable per box
    box_treat = d[treat=='treatment', .(fledge_box, box_treat=group)]
    box_treat[, box_treat_num := ifelse(box_treat=="older",1,2) ]
  d = merge(d, box_treat, by=c("fledge_box"))
  d = d[dead==0] #Remove chciks that were found dead in the nest
  d = d[fledge_box!=150 & fledge_box!=265] #>2 missing fledging times - box defect?
  d = d[!is.na(fledge)] #Remove chicks with no fledging time


#Get delta fledging times
  setorder(d, fledge_box, fledge_date, fledge_time)
  d[, order:= 1:.N, by=.(fledge_box)]
  d[, order_rel := (order-1)/(brood-1)] #Creates variable of relative order with the brood
  d[, mins_to_prev  := difftime( fledge, shift(fledge, type = 'lag') , units = 'mins') %>% as.numeric , by = .(fledge_box)]
  d[, mins_to_next  := difftime( shift(fledge, type = 'lead'), fledge, units = 'mins') %>% as.numeric , by = .(fledge_box)]
  d[, mins_to_closest := ifelse(is.na(mins_to_prev),mins_to_next,
                          ifelse(is.na(mins_to_next),mins_to_prev,
                          ifelse(mins_to_prev<mins_to_next, mins_to_prev, mins_to_next)))]
  d[, mins_to_closest := ifelse(fledge_box==150 | fledge_box==265, NA, mins_to_closest)] #>2 missing fledging times
  d[, days_to_prev  := abs(yday(fledge_date) - shift(yday(fledge_date),1)), by = .(fledge_box)]
  d[, days_to_next  := abs(yday(fledge_date) - shift(yday(fledge_date),-1)), by = .(fledge_box)]
  d[, alone := sign(ifelse(is.na(days_to_prev),days_to_next,
               ifelse(is.na(days_to_next),days_to_prev,
               ifelse(days_to_prev<days_to_next, days_to_prev, days_to_next))))] #Creates variables whether nestling only one of the brood to fledge on a particular day

##Get data overview
  nrow(d[group=='older' & treat=='treatment'])
  nrow(d[group=='older' & treat=='control'])
  nrow(d[group=='younger' & treat=='treatment'])
  nrow(d[group=='younger' & treat=='control'])

#Create data set that only includes swapped nestlings
  e = d[!is.na(treat), .(box, fledge_age, fledge_agetime, mins_to_closest, order_rel, group, treat, treat_num, alone)]
  e[, n := .N, by=box]
  e = e[n==2] #Only include chicks when data of both siblings are available

  #Show summary statistics
    mean((e$fledge_agetime[e$group=='older' & e$treat=='treatment'])-mean(e$fledge_agetime[e$group=='older' & e$treat=='control']))*24
    sd((e$fledge_agetime[e$group=='older' & e$treat=='treatment'])-sd(e$fledge_agetime[e$group=='older' & e$treat=='control']))*24
    mean((e$fledge_agetime[e$group=='younger' & e$treat=='treatment'])-mean(e$fledge_agetime[e$group=='younger' & e$treat=='control']))*24
    sd((e$fledge_agetime[e$group=='younger' & e$treat=='treatment'])-sd(e$fledge_agetime[e$group=='younger' & e$treat=='control']))*24



#PERFORM STATISTICAL TESTS

  wilcox.test(e$fledge_agetime[e$group=='older' & e$treat=='treatment'],
              e$fledge_agetime[e$group=='older' & e$treat=='control'],
              paired=TRUE)
  wilcox.test(e$fledge_agetime[e$group=='younger' & e$treat=='treatment'],
              e$fledge_agetime[e$group=='younger' & e$treat=='control'],
              paired=TRUE)

  wilcox.test(e$order_rel[e$group=='older' & e$treat=='treatment'],
              e$order_rel[e$group=='older' & e$treat=='control'],
              paired=TRUE)
  wilcox.test(e$order_rel[e$group=='younger' & e$treat=='treatment'],
              e$order_rel[e$group=='younger' & e$treat=='control'],
              paired=TRUE)

  wilcox.test(e$mins_to_closest[e$group=='older' & e$treat=='treatment'],
              e$mins_to_closest[e$group=='older' & e$treat=='control'],
              paired=TRUE)
  wilcox.test(e$mins_to_closest[e$group=='younger' & e$treat=='treatment'],
              e$mins_to_closest[e$group=='younger' & e$treat=='control'],
              paired=TRUE)

  x1 <- factor(e$alone[e$group=='older' & e$treat=='treatment'], levels = c("0", "1"))
  x2 <- factor(e$alone[e$group=='older' & e$treat=='control'], levels = c("0", "1"))
  table(x1, x2)
  mcnemar.test(table(x1, x2))

  x1 <- factor(e$alone[e$group=='younger' & e$treat=='treatment'], levels = c("0", "1"))
  x2 <- factor(e$alone[e$group=='younger' & e$treat=='control'], levels = c("0", "1"))
  table(x1, x2)
  mcnemar.test(table(x1, x2))



#PLOT TIME OF FLEDGING

  #Get mean values
    setorder(e, box)
    s = e[, n := .N, by=c("box")]
    s = s[n==2, mean(fledge_agetime), by=c('treat_num', 'group')]

p1 = ggplot() +
      geom_line(data=e[group=="older" & !is.na(fledge_agetime)], aes(x=treat_num, y=fledge_agetime, group=box)) +
      geom_line(data=s[group=="older"], aes(x=treat_num, y=V1), col="red", size=1.5) +
      geom_point(data=e[group=="older" & !is.na(fledge_agetime)], aes(x=treat_num, y=fledge_agetime), size=1) +
      scale_y_continuous(limits = c(17,23.6), expand = c(0, 0), breaks=seq(17,23.5,.5), labels=c("","17","","18","","19","","20","","21","","22","","23"),
                         sec.axis = sec_axis(~ . * 1, breaks=seq(17,23.5,.5), labels=c("00:00","12:00","00:00","12:00","00:00","12:00","00:00","12:00","00:00","12:00","00:00","12:00","00:00","12:00"))) +
      scale_x_continuous(limits = c(.7,2.3), expand = c(0, 0), breaks=c(1,2), labels=c("day 17 nestling in\nday 17 brood","day 17 nestling in\nday 15 brood")) +
      ylab("Fledging age / time") + xlab("") +
      labs(tag = "a)", size=12) +
      annotate("rect", xmin = .7, xmax = 2.3, ymin = 17.00, ymax = 17.23,  alpha = .25) +
      annotate("rect", xmin = .7, xmax = 2.3, ymin = 17.87, ymax = 18.23,  alpha = .25) +
      annotate("rect", xmin = .7, xmax = 2.3, ymin = 18.87, ymax = 19.23,  alpha = .25) +
      annotate("rect", xmin = .7, xmax = 2.3, ymin = 19.87, ymax = 20.23,  alpha = .25) +
      annotate("rect", xmin = .7, xmax = 2.3, ymin = 20.87, ymax = 21.23,  alpha = .25) +
      annotate("rect", xmin = .7, xmax = 2.3, ymin = 21.87, ymax = 22.23,  alpha = .25) +
      annotate("rect", xmin = .7, xmax = 2.3, ymin = 22.87, ymax = 23.23,  alpha = .25) +
      theme_classic()
    p1


p2 = ggplot() +
      geom_line(data=e[group=="younger" & !is.na(fledge_agetime)], aes(x=treat_num, y=fledge_agetime, group=box)) +
      geom_line(data=s[group=="younger"], aes(x=treat_num, y=V1), col="red", size=1.5) +
      geom_point(data=e[group=="younger" & !is.na(fledge_agetime)], aes(x=treat_num, y=fledge_agetime), size=1) +
      scale_y_continuous(limits = c(17,23.6), expand = c(0, 0), breaks=seq(17,23.5,.5), labels=c("","17","","18","","19","","20","","21","","22","","23"),
      sec.axis = sec_axis(~ . * 1, breaks=seq(17,23.5,.5), labels=c("00:00","12:00","00:00","12:00","00:00","12:00","00:00","12:00","00:00","12:00","00:00","12:00","00:00","12:00"))) +
      scale_x_continuous(limits = c(.7,2.3), expand = c(0, 0), breaks=c(1,2), labels=c("day 15 nestling in\nday 15 brood","day 15 nestling in\nday 17 brood")) +
      ylab("Fledging age / time") + xlab("") +
      labs(tag = "b)") +
      annotate("rect", xmin = .7, xmax = 2.3, ymin = 17.00, ymax = 17.23,  alpha = .25) +
      annotate("rect", xmin = .7, xmax = 2.3, ymin = 17.87, ymax = 18.23,  alpha = .25) +
      annotate("rect", xmin = .7, xmax = 2.3, ymin = 18.87, ymax = 19.23,  alpha = .25) +
      annotate("rect", xmin = .7, xmax = 2.3, ymin = 19.87, ymax = 20.23,  alpha = .25) +
      annotate("rect", xmin = .7, xmax = 2.3, ymin = 20.87, ymax = 21.23,  alpha = .25) +
      annotate("rect", xmin = .7, xmax = 2.3, ymin = 21.87, ymax = 22.23,  alpha = .25) +
      annotate("rect", xmin = .7, xmax = 2.3, ymin = 22.87, ymax = 23.23,  alpha = .25) +
      theme_classic()
    p2

  grid.arrange(p1, p2, ncol=2, widths = c(1, 1))



###PLOT FLEDGING ORDER

  #Produce summary statistics
  median(na.omit(d$order_rel[d$group=='older' & d$treat=='treatment']))
  median(na.omit(d$order_rel[d$group=='younger' & d$treat=='treatment']))
  nrow(d[group=='older' & treat=='treatment' & order==1])/nrow(d[group=='older' & treat=='treatment'])
  nrow(d[group=='younger' & treat=='treatment' & order==brood])/nrow(d[group=='younger' & treat=='treatment'])

  median(na.omit(d$order_rel[d$group=='older' & d$treat=='control']))
  median(na.omit(d$order_rel[d$group=='younger' & d$treat=='control']))
  nrow(d[group=='older' & treat=='control' & order==1])/nrow(d[group=='older' & treat=='control'])
  nrow(d[group=='younger' & treat=='control' & order==brood])/nrow(d[group=='younger' & treat=='control'])

  #Get mean values
  s = d[!is.na(order_rel) & !is.na(treat)]
  s[, n := .N, by=c("box")]
  s = s[n==2, mean(order_rel), by=c('treat_num', 'group')]

  p1 = ggplot() +
    geom_line(data=e[group=="older"], aes(x=treat_num, y=order_rel, group=box))  +
    geom_line(data=s[group=="older"], aes(x=treat_num, y=V1), col="red", size=1.5) +
    geom_point(data=e[group=="older"], aes(x=treat_num, y=order_rel), size=1) +
    ylab("Relative fledging order") + xlab("") +
    scale_x_continuous(limits = c(.7,2.3), expand = c(0, 0), breaks=c(1,2), labels=c("day 17 nestling in\nday 17 brood","day 17 nestling in\nday 15 brood")) +
    scale_y_continuous(limits = c(0,1)) +
    labs(tag = "a)") +
    theme_classic()

  p2 = ggplot() +
    geom_line(data=e[group=="younger"], aes(x=treat_num, y=order_rel, group=box))  +
    geom_line(data=s[group=="younger"], aes(x=treat_num, y=V1), col="red", size=1.5) +
    geom_point(data=e[group=="younger"], aes(x=treat_num, y=order_rel), size=1) +
    ylab("Relative fledging order") + xlab("") +
    scale_x_continuous(limits = c(.7,2.3), expand = c(0, 0), breaks=c(1,2), labels=c("day 15 nestling in\nday 15 brood","day 15 nestling in\nday 17 brood")) +
    scale_y_continuous(limits = c(0,1)) +
    labs(tag = "b)") +
    theme_classic()

  grid.arrange(p1, p2, ncol=2, widths = c(1, 1))



###PLOT DELTA FLEDGING TIMES

  #Produce summary statistics
    median(na.omit(e$mins_to_closest[e$group=='older' & e$treat=='treatment']))
    median(na.omit(e$mins_to_closest[e$group=='older' & e$treat=='control']))
    median(na.omit(e$mins_to_closest[e$group=='younger' & e$treat=='treatment']))
    median(na.omit(e$mins_to_closest[e$group=='younger' & e$treat=='control']))

    nrow(e[group=='older' & treat=='treatment' & alone==1])/nrow(e[group=='older' & treat=='treatment'])
    nrow(e[group=='older' & treat=='control' & alone==1])/nrow(e[group=='older' & treat=='control'])
    nrow(e[group=='younger' & treat=='treatment' & alone==1])/nrow(e[group=='younger' & treat=='treatment'])
    nrow(e[group=='younger' & treat=='control' & alone==1])/nrow(e[group=='younger' & treat=='control'])

  #Get mean values
    s = e[!is.na(mins_to_closest) & !is.na(treat)]
    s[, n := .N, by=c("box")]
    s = s[n==2, exp(median(log(mins_to_closest))), by=c('treat_num', 'group')]

p1 = ggplot() +
      geom_line(data=e[group=="older" & !is.na(mins_to_closest)], aes(x=treat_num, y=mins_to_closest, group=box)) +
      geom_line(data=s[group=="older"], aes(x=treat_num, y=V1), col="red", size=1.5) +
      geom_point(data=e[group=="older" & !is.na(mins_to_closest) & alone==0], aes(x=treat_num, y=mins_to_closest), size=1) +
      geom_point(data=e[group=="older" & !is.na(mins_to_closest) & alone==1], aes(x=treat_num, y=mins_to_closest), colour='blue', size=1) +
      scale_y_log10(limits = c(.1,6000), breaks=c(1,10,100,1000)) +
      scale_x_continuous(limits = c(.7,2.3), expand = c(0, 0), breaks=c(1,2), labels=c("day 17 nestling in\nday 17 brood","day 17 nestling in\nday 15 brood")) +
      ylab("∆ Fledging time (minutes, log-scale)") + xlab("") +
      labs(tag = "a)") +
      theme_classic()
    p1

p2 = ggplot() +
      geom_line(data=e[group=="younger" & !is.na(mins_to_closest)], aes(x=treat_num, y=mins_to_closest, group=box)) +
      geom_line(data=s[group=="younger"], aes(x=treat_num, y=V1), col="red", size=1.5) +
      geom_point(data=e[group=="younger" & !is.na(mins_to_closest) & alone==0], aes(x=treat_num, y=mins_to_closest), size=1) +
      geom_point(data=e[group=="younger" & !is.na(mins_to_closest) & alone==1], aes(x=treat_num, y=mins_to_closest), colour='blue', size=1) +
      scale_y_log10(limits = c(.1,6000), breaks=c(1,10,100,1000)) +
      scale_x_continuous(limits = c(.7,2.3), expand = c(0, 0), breaks=c(1,2), labels=c("day 15 nestling in\nday 15 brood","day 15 nestling in\nday 17 brood")) +
      ylab("∆ Fledging time (minutes, log-scale)") + xlab("") +
      labs(tag = "b)") +
      theme_classic()
    p2

    grid.arrange(p1, p2, ncol=2, widths = c(1, 1))









