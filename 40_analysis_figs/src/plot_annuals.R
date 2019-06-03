plot_flux_annuals <- function(target_name, mod, mod_boot, unit) {
  
  plot_title = paste("Water Year", mod$INFO$constitAbbrev, "Flux (dots), FN Flux (line)")
  
  png(target_name, height = 400, width = 500)
  
  par(cex = 1.2, mai = c(0.5,1.2,0.5,0.5))
  
  plotFluxHistBoot(mod, mod_boot, printTitle = F, customPar = T, fluxUnit = unit, col.pred = 'blue')
  title(main = plot_title)

  dev.off()
}
