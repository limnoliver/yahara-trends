library(dplyr)
library(lubridate)
library(ggplot2)

combine_parameters <- function(tp, diss_p, ortho_p_filt, ortho_p_unfilt, ss) {
  all_dat <- bind_rows(tp, diss_p, ortho_p_filt, ortho_p_unfilt, ss) %>%
    mutate(parameter = c(rep("Total Phosphorus", nrow(tp)),
                         rep("Dissolved P", nrow(diss_p)),
                         rep("Orthophosphate, filtered", nrow(ortho_p_filt)),
                         rep("Orthophosphate, unfiltered", nrow(ortho_p_unfilt)),
                         rep("Suspended Sediment", nrow(ss))))
  
  return(all_dat)
}
plot_n_per_year <- function(data, fig_name) {
  summary_dat <- data %>%
    group_by(Date, parameter) %>%
    summarize(count = n()) %>%
    mutate(year = year(Date)) %>%
    group_by(year, parameter) %>%
    summarize(count = n())
  
  p <- ggplot(summary_dat, aes(y = count, x = year)) +
    geom_point() +
    facet_wrap(~parameter, ncol = 1, scales = 'free_y') +
    annotate("rect", xmin = decimal_date(as.Date('1980-09-29')), xmax = decimal_date(as.Date('1981-02-22')), ymin = -Inf, ymax = Inf, 
              fill = 'gray', alpha = 0.3) +
    annotate("rect", xmin = decimal_date(as.Date('1981-12-31')), xmax = decimal_date(as.Date('1989-10-01')), ymin = -Inf, ymax = Inf, 
              fill = 'gray', alpha = 0.3) +
    scale_y_continuous(limits = c(0,NA)) +
    scale_x_continuous(breaks = seq(from = 1975, to = 2015, by = 10)) +
    theme_bw()
  
  ggsave(fig_name, p, height = 6, width = 5)
}

plot_annual_loads <- function(data, fig_name) {
  annual <- data %>%
    mutate(year = dataRetrieval::calcWaterYear(Date)) %>%
    group_by(year) %>%
    summarize(annual_load = sum(tp_pounds), count = n()) %>%
    filter(count > 364)
  
  p <- ggplot(annual, aes(x = year, y = annual_load)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = mean(annual$annual_load), linetype = 2)+
    #geom_smooth(method = 'lm') +
    theme_bw() +
    labs(x = 'Water Year', y = 'TP Annual Load (pounds)')
  
  ggsave(fig_name, p,  height = 4, width = 6)
}

plot_annual_discharge <- function(data, fig_name) {
  annual <- data %>%
    mutate(year = dataRetrieval::calcWaterYear(Date)) %>%
    group_by(year) %>%
    summarize(annual_dis = sum(Q), count = n()) %>%
    filter(count > 364)
  
  p <- ggplot(annual, aes(x = year, y = annual_dis)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = mean(annual$annual_dis), linetype = 2)+
    #geom_smooth(method = 'lm') +
    theme_bw() +
    labs(x = 'Water Year', y = 'Annual Discharge')
  
  ggsave(fig_name, p, height = 4, width = 6)
    
}
