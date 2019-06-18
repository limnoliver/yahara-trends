create_bias_plot <- function(eList, file_name) {
  
  pdf(file_name, height = 8, width = 8) 
  fluxBiasMulti(eList)
  dev.off()
}