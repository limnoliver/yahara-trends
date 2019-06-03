create_monthly_fig <- function(fig_dat, fig_name, fig_height, fig_width, type) {
  month_boot <- grep('boot', fig_dat, value = TRUE)
  month <-fig_dat[-grep('boot', fig_dat)]
  
  all_boot_dat <- data.frame()
  for (i in 1:length(month_boot)) {
    temp_dat <- make(month_boot[i], remake_file = '34_analyze_data_groups_monthly.yml')
    plot_dat <- temp_dat[[1]] %>%
      mutate(month = month_boot[i])
    
    all_boot_dat <- bind_rows(all_boot_dat, plot_dat)
  }
  
  all_month_dat <- data.frame()
    for (i in 1:length(month)) {
      temp_dat <- make(month[i], remake_file = '34_analyze_data_groups_monthly.yml')
      plot_dat <- temp_dat %>%
        mutate(month = month[i], 
               type = row.names(temp_dat))
      
      all_month_dat <- bind_rows(all_month_dat, plot_dat)
    }
  
  if (type == 'conc') {
    plot_dat <- data.frame(month = factor(gsub('tp_groups_boot_', '', all_boot_dat$month), levels=gsub('tp_groups_boot_', '', all_boot_dat$month)) ,
                           conc_est = all_boot_dat$estC,
                           conc_est_low90 = all_boot_dat$lowC,
                           conc_est_high90 = all_boot_dat$upC50,
                           conc_est_cqtc = all_month_dat$CQTC[all_month_dat$type == 'Conc'], 
                           conc_est_qtc = all_month_dat$QTC[all_month_dat$type == 'Conc'])
    y_label <- 'Change in FN TP (mg/L)'
  } else if (type == 'flux') {
    plot_dat <- data.frame(month = factor(gsub('tp_groups_boot_', '', all_boot_dat$month), levels=gsub('tp_groups_boot_', '', all_boot_dat$month)) ,
                           conc_est = all_boot_dat$estF*1000000*2.205,
                           conc_est_low90 = all_boot_dat$lowF*1000000*2.205,
                           conc_est_high90 = all_boot_dat$upF50*1000000*2.205,
                           conc_est_cqtc = all_month_dat$CQTC[all_month_dat$type == 'Flux']*1000000*2.205, 
                           conc_est_qtc = all_month_dat$QTC[all_month_dat$type == 'Flux']*1000000*2.205)
    
    y_label <- 'Change in Flux (pounds/year)'
  }  else {
    plot_dat <- data.frame(month = factor(gsub('tp_groups_boot_', '', all_boot_dat$month), levels=gsub('tp_groups_boot_', '', all_boot_dat$month)) ,
                           conc_est = 100*all_boot_dat$estC/all_boot_dat$baseConc,
                           conc_est_low90 = 100*all_boot_dat$lowC/all_boot_dat$baseConc,
                           conc_est_high90 = 100*all_boot_dat$upC50/all_boot_dat$baseConc,
                           conc_est_cqtc = 100*all_month_dat$CQTC[all_month_dat$type == 'Conc']/all_boot_dat$baseConc, 
                           conc_est_qtc = 100*all_month_dat$QTC[all_month_dat$type == 'Conc']/all_boot_dat$baseConc)
    
    y_label <- '% Change in TP (FN Concentration)'
  }
  
  p <- ggplot(plot_dat, aes(x = month, y = conc_est)) +
    geom_point() +
    geom_vline(xintercept = c(1:12), size = 12, alpha = 0.5, color = 'gray') +
    geom_point() +
    geom_errorbar(aes(ymin = conc_est_low90, ymax = conc_est_high90), width = 0.2) +
    geom_point(aes(x = as.numeric(month)-0.2, y = conc_est_cqtc), color = 'red') +
    geom_point(aes(x = as.numeric(month)+0.2, y = conc_est_qtc), color = 'blue') +
    theme_bw() +
    labs(title = 'Trends in TP between 1990-2004 and 2006-2018',
         subtitle = 'Black dots show estimated trend and 90% CI. \nRed dots are the change associated with the relationship between C ~ Q.\nBlue dots are the change associated with changes in Q.',
         y = y_label,
         x = '')
  

ggsave(fig_name, p, height = fig_height, width = fig_width)
  
  
}
