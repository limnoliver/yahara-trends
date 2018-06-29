# series analysis results

# table of caluclated conc/flux
# tableResults(tp_spring_out)

# plot of conc

##plotConcHist(tp_spring_out)

# plot of flux

#plotFluxHist(tp_spring_out)

construct_conc_fig <- function(mod_out, seasonal, constituent, file_name) {
  if (seasonal == TRUE) {
    
    plot_titles <- c('Winter', 'Spring', 'Summer', 'Fall')
    extra_text <- c('Conc (dots), FN Conc (line)')
    
    png(file_name, height = 700, width = 700)
    
    par(mfrow = c(2,2), mar = c(2,5,4,1), cex = 1.2)
    
    for (i in 1:length(mod_out)) {
      plotConcHist(mod_out[[i]], printTitle = F, customPar = T)
      title(main = paste(plot_titles[i], constitudent, extra_text))
    }
    
    dev.off()
    
  } else {
   
    plot_title = paste("Water Year", constituent, "Conc (dots), FN Conc (line)")
    
    png(file_name, height = 400, width = 400)
    
    par(cex = 1.2)
    
    plotConcHist(mod_out, printTitle = F, customPar = T)
    title(main = plot_title)
    
    dev.off()
    
  }
  
}