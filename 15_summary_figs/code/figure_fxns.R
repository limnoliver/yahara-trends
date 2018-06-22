library(dplyr)
library(lubridate)

combine_parameters <- function(tp, ortho_p, ss) {
  all_dat <- bind_rows(tp, ortho_p, ss) %>%
    mutate(parameter = c(rep("Total Phosphorus", nrow(tp)),
                         rep("Orthophosphate", nrow(ortho_p)),
                         rep("Suspended Sediment", nrow(ss))))
  
  return(all_dat)
}
plot_n_per_year <- function(data, fig_name) {
  summary_dat <- data %>%
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

