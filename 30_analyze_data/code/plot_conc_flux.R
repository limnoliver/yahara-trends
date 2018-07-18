construct_conc_fig <- function(eList_out, mod_out, type, constituent, file_name = target_name) {
  if (type == 'seasonal') {
    
    plot_titles <- c('Winter', 'Spring', 'Summer', 'Fall')
    extra_text <- c('Conc (dots), FN Conc (line)')
    
    png(file_name, height = 700, width = 700)
    
    par(mfrow = c(2,2), mar = c(2,5,4,1), cex = 1.2)
    
    for (i in 1:length(mod_out)) {
      plotConcHistBoot(eList_out[[i]], mod_out[[i]], printTitle = F, customPar = T)
      title(main = paste(plot_titles[i], constituent, extra_text))
    }
    
    dev.off()
    
  } else if (type == 'wy'){
   
    plot_title = paste("Water Year", constituent, "Conc (dots), FN Conc (line)")
    
    png(file_name, height = 400, width = 400)
    
    par(cex = 1.2)
    
    plotConcHistBoot(eList_out, mod_out, printTitle = F, customPar = T)
    title(main = plot_title)
    
    dev.off()
    
  }
  
}

construct_flux_fig <- function(eList_out, mod_out, type, constituent, file_name = target_name) {
  if (type == 'seasonal') {
    
    plot_titles <- c('Winter', 'Spring', 'Summer', 'Fall')
    extra_text <- c('Flux (dots), FN Flux (line)')
    
    png(file_name, height = 700, width = 700)
    
    par(mfrow = c(2,2), mar = c(2,5,4,1), cex = 1.2)
    
    for (i in 1:length(mod_out)) {
      plotFluxHistBoot(eList_out[[i]], mod_out[[i]], printTitle = F, customPar = T)
      title(main = paste(plot_titles[i], constituent, extra_text))
    }
    
    dev.off()
    
  } else if (type == 'wy'){
    
    plot_title = paste("Water Year", constituent, "Flux (dots), FN Flux (line)")
    
    png(file_name, height = 400, width = 400)
    
    par(cex = 1.2)
    
    plotFluxHistBoot(eList_out, mod_out, printTitle = F, customPar = T)
    title(main = plot_title)
    
    dev.off()
    
  }
  
}