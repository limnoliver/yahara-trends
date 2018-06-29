# series analysis results

# table of caluclated conc/flux
# tableResults(tp_spring_out)

# plot of conc

##plotConcHist(tp_spring_out)

# plot of flux

#plotFluxHist(tp_spring_out)

construct_conc_fig <- function(mod_out, file_name) {
  plot_titles <- c('Winter', 'Spring', 'Summer', 'Fall')
  extra_text <- c('Conc (dots), FN Conc (line)')

  png(file_name, height = 700, width = 700)
  par(mfrow = c(2,2), mar = c(2,5,4,1), cex = 1.2)
  
  for (i in 1:length(mod_out)) {
    plotConcHist(mod_out[[i]], istat = stat, printTitle = F, customPar = T)
    title(main = paste(plot_titles[i], extra_text))
  }
  
  dev.off()
}