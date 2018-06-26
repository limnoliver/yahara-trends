create_list <- function(...) {
  dat <- list(...)
  return(dat)
}

construct_flow_fig <- function(dat, file_name, stat) {
  plot_titles <- c('Water Year', 'Winter', 'Spring', 'Summer')
  stat_titles <- c('1-day min', '7-day min', '30-day min', 'median', 'mean', '30-day max', '7-day max', '1-day max')
  
  png(file_name, height = 700, width = 700)
  par(mfrow = c(2,2), mar = c(2,5,4,1), cex = 1.2)
  
  for (i in 1:length(dat)) {
    plotFlowSingle(dat[[i]], istat = stat, printTitle = F, customPar = T)
    title(main = paste(plot_titles[i], stat_titles[stat]))
  }
  
  dev.off()
}