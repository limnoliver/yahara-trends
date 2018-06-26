create_list <- function(...) {
  dat <- list(...)
  return(dat)
}

construct_flow_fig <- function(dat, file_name, stat) {
  plot_titles <- c('Water Year', 'Winter', 'Spring', 'Summer')
  stat_titles <- c('min of 1-day mean', 'min of 7-day daily mean', 'min of 30-day daily mean', 'median of period daily mean', 'mean of period daily mean', 'max of 30-day daily mean', 'max of 7-day daily mean', 'max of 1-day mean')
  
  png(file_name, height = 700, width = 700)
  par(mfrow = c(2,2), mar = c(2,5,4,1), cex = 1.2)
  
  for (i in 1:length(dat)) {
    plotFlowSingle(dat[[i]], istat = stat, printTitle = F, customPar = T)
    title(main = paste(plot_titles[i], stat_titles[stat]))
  }
  
  dev.off()
}

construct_var_fig <- function(dat, file_name) {
  plot_titles <- c('Water Year', 'Winter', 'Spring', 'Summer')
  addendum <- "variability in discharge"
  png(file_name, height = 700, width = 700)
  par(mfrow = c(2,2), mar = c(2,5,4,1), cex = 1.2)
  
  for (i in 1:length(dat)){
    plotSDLogQ(dat[[i]], printTitle = F, customPar = T)
    title(main = paste(plot_titles[i], addendum))
  }
  
  dev.off()
}

construct_q_fig <- function(dat, file_name, qlim) {
  plot_titles <- c('Water Year', 'Winter', 'Spring', 'Summer')
  addendum <- "variability in discharge"

  png(file_name, height = 400, width = 1200)
  
  plotQTimeDaily(dat, lwd = 1, qLower = qlim)
  
  dev.off()

}