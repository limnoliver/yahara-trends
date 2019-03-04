compare_dat_inputs <- function(figure_name){
  
  pair_tests <- scipiper::list_all_targets(remake_file = '29_analyze_data_source_tests.yml')
  pair_tests <- pair_tests[grep('pair', pair_tests)]
  conc_all <- data.frame()
  perc_all <- data.frame()
  for (i in pair_tests) {
    conc_change <- make(i, remake_file = '29_analyze_data_source_tests.yml')
    perc_change <- attr(conc_change, 'Other')
    perc_change <- bind_rows(perc_change$PercentChangeConc, perc_change$PercentChangeFlux)
    
    conc_all <- bind_rows(conc_all, conc_change)
    perc_all <- bind_rows(perc_all, perc_change)
  }
  
  conc_all$variable <- rep(c('concentration', 'flux'), 9)
  conc_all$data <- rep(pair_tests, each = 2)
  
  perc_all$variable <- rep(c('concentration', 'flux'), 9)
  perc_all$data <- rep(pair_tests, each = 2)
  perc_all <- tidyr::gather(perc_all, key = 'key', value = 'value', -variable, -data)
  
  perc_all_conc <- filter(perc_all, variable == 'concentration')
  perc_all_conc <- filter(perc_all, variable == 'concentration')
  
  p <- ggplot(perc_all, aes(x = data, y = value)) +
    geom_point(aes(color  = key)) +
    facet_wrap(~variable, ncol = 1, scales = 'free_y')+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
  ggsave(figure_name, p, height = 6, width = 6)
}
