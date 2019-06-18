
plot_four_panels <- function(gen, fixed, conc = TRUE, file_name) {
  
  #gen <- make('tp_wy_out', remake_file = '30_analyze_data_series.yml')
  #fixed <- make('tp_wy_out_stationary', remake_file = '30_analyze_data_series.yml')
  
  gen_annual <- tableResults(gen) %>% 
    mutate(trend_type = 'Water quality trend')
  fix_annual <- tableResults(fixed) %>%
    mutate(trend_type = 'MTC')
  
  annuals <- bind_rows(gen_annual, fix_annual)

  
  if (conc == TRUE) {
    p1 <- # first plot annual estimates with lines
      ggplot(annuals, aes(x = Year, y = `FN Conc [mg/L]`)) +
      geom_line(aes(group = trend_type, color = trend_type)) +
      scale_color_manual(values = c('yellow3', 'purple4')) +
      geom_point(dat = gen_annual, aes(x = Year, y = `Conc [mg/L]`)) +
      labs(y = 'Conc [mg/L]', color = '', x = 'Water year') +
      theme_bw() +
      guides(color = guide_legend(nrow = 1))
    

  } else {
    # first plot annual estimates with lines
    p1 <- ggplot(annuals, aes(x = Year, y = `FN Flux [10^6kg/yr]`)) +
      geom_line(aes(group = trend_type, color = trend_type)) +
      scale_color_manual(values = c('yellow3', 'purple4')) +
      geom_point(dat = gen_annual, aes(x = Year, y = `Flux [10^6kg/yr]`)) +
      labs(y = 'Flux [10^6kg/yr]', color = '', x = 'Water year') +
      theme_bw() +
      guides(color = guide_legend(nrow = 1))
    
    
  }
 
  # plot annual Q statistics
  annual_Q <- gen$Daily %>%
    group_by(waterYear) %>%
    summarize(`10th percentile` = quantile(Q, probs = 0.1),
              `max day` = max(Q),
              `median` = median(Q)) %>%
    gather(key = 'variable', value = 'value', -waterYear)

  p2 <- ggplot(annual_Q, aes(x = waterYear, y = value)) +
    geom_point(aes(color = variable)) +
    geom_smooth(method = 'lm', se = FALSE, aes(group = variable, color = variable)) +
    scale_y_log10() +
    theme_bw() +
    labs(y = "Q [cms]", x = 'Water year', color = '') +
    guides(color = guide_legend(nrow = 1))
  
  
  # create conc vs discharge over different times/years
  conc_q_period <- gen$Sample %>%
    mutate(month = lubridate::month(Date)) %>%
    mutate(period = case_when(month %in% c(2:4) & waterYear <= 1999 ~ 'Feb-Apr 1990-1999',
                              month %in% c(2:4) & waterYear >= 2009 ~ 'Feb-Apr 2009-2018',
                              month %in% c(5:7) & waterYear <= 1999 ~ 'May-Jul 1990-1999',
                              month %in% c(5:7) & waterYear > 2009 ~ 'May-Jul 2009-2018')) %>%
    filter(!is.na(period))
  
  p3 <- ggplot(conc_q_period, aes(x = Q, y = ConcAve)) +
    geom_point(color = 'darkgray', alpha = 0.5, show.legend = FALSE) +
    geom_smooth(se = FALSE, method = 'lm', aes(color = period, group= period), size = 1, show.legend = FALSE) +
    scale_x_log10() +
    scale_y_log10() +
    scale_color_manual(values = c('skyblue2', 'skyblue4', 'tomato', 'tomato4')) +
    theme_bw() +
    labs(y = 'Conc [mg/L]', x = 'Q [cms]')
  
  # now do cdf of Q per period
  q_period <- gen$Daily %>%
    mutate(month = lubridate::month(Date)) %>%
    mutate(period = case_when(month %in% c(2:4) & waterYear <= 1999 ~ 'Feb-Apr 1990-1999',
                              month %in% c(2:4) & waterYear >= 2009 ~ 'Feb-Apr 2009-2018',
                              month %in% c(5:7) & waterYear <= 1999 ~ 'May-Jul 1990-1999',
                              month %in% c(5:7) & waterYear > 2009 ~ 'May-Jul 2009-2018')) %>%
    filter(!is.na(period))
  
  p4 <- ggplot(q_period, aes(x = Q, color = period, group = period)) +
    stat_ecdf(geom = 'line', size = 1) +
    scale_x_log10() +
    scale_color_manual(values = c('skyblue2', 'skyblue4', 'tomato', 'tomato4')) +
    theme_bw() +
    labs(y = 'Cumulative probability', color = '', x = 'Q [cms]') +
    guides(color = guide_legend(nrow = 2))
  
  # put it all together!
  p1b <- p1 + theme(legend.position = 'none')
  p2b <- p2 + theme(legend.position = 'none')
  p3b <- p3 + theme(legend.position = 'none')
  p4b <- p4 + theme(legend.position = 'none')
  
  ptop <- plot_grid(p1b, p2b, p3b, p4b, align = 'h', ncol = 4, rel_widths = c(1.3, 1.3,1,1))
  pbottom <- plot_grid(get_legend(p1), get_legend(p2), get_legend(p4), ncol = 3, rel_widths = c(1.3, 1.3, 2))
  
  pall <- plot_grid(ptop, pbottom, align = 'v', ncol = 1, rel_heights = c(1, 0.2))
  
  ggsave(file_name, pall, height = 4, width = 12)
}
# recreate four-panel plot from murphy



