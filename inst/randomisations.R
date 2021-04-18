
#subset to relevant columns
p3 = d2[, .(nest, fledge_age, fledge_time)]

#transform fledge time to seconds
p3[, fledge_time := fledge_time * 60*60]

#exclude days and nests with fewer than 3 fledglings
p3[, group_size := .N, by = .(nest, fledge_age)]
p3 = p3[group_size > 2,]

#find first and last fledge time within a nest on a given day
p3[, min_ := min(fledge_time), by = .(nest, fledge_age)]
p3[, max_ := max(fledge_time), by = .(nest, fledge_age)]

#setup figure
plot_nearest = TRUE #if false, the time to the next sibling is plotted; if you want to plot both, use
#par(mfrow = c(2,1))
XLAB = ifelse(plot_nearest == TRUE, "Fleding time difference with nearest sibling (min.)", "Time to next sibling")
par(las = 1)
par(mar = c(4.1, 4.1, 0.1, 0.1))
plot(c(0, 3.5), c(0,1), type = "n", xlab = XLAB, ylab = "Density", xaxt = "n")
axis(1, at = c(0, 1, 2, 3), labels = c(0, 10^1, 10^2, 10^3))

#setup randomization
R = list()
RUNS = 10

#run randomization (it doesn't take too long to compute so I didn't bother to set up parallel cores)
for(i in 1 : (RUNS+1)){
  #display status
  if(i %% 10 == 0) print(paste0(i/RUNS*100, "%"))
  
  #randomize the fledging times unless the data is the original data (i=0)
  if(i > RUNS) {p3[, rand_time := fledge_time]; COL = "red"; LWD = 2 } else {
    p3[, rand_time := sample((min_+1):(max_-1), 1), by = rownames(p3)];
    #set first and last fledging of a day to the original value
    p3[fledge_time == min_ | fledge_time == max_, rand_time := fledge_time];
    COL = alpha("black", alpha = 0.5); LWD = 1;
  }
  
  #calculate delta-times
  setorder(p3, nest, rand_time)
  p3[, delta_next := shift(rand_time, type = "lead") - rand_time, by = .(nest, fledge_age)]
  p3[, delta_min := rand_time - shift(rand_time), by = .(nest, fledge_age)]
  p3[, delta_min := min(delta_min, delta_next, na.rm = TRUE), by = rownames(p3)]
  
  #change unit to minutes
  p3[, delta_next := delta_next/60]
  p3[, delta_min := delta_min/60]
  #add lines of log-transformed data (non-log doesn't make sense for a density plot)
  #here, use either delta_next (=time to next sibling) or delta_min (=time to nearest sibling). The variable is set in the plot setup
  if(plot_nearest == TRUE) lines(density(na.omit(log10(p3[, delta_min]+1))), lwd = LWD, col = COL) else lines(density(na.omit(log10(p3[, delta_next]+1))), lwd = LWD, col = COL)
}



