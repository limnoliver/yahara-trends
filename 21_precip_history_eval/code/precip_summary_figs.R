# 
# # date that you achieve 90% of rain for water year
# library(lubridate)
# dat_cum <- group_by(dat_fix, waterYear) %>%
#   mutate(precip_cum = cumsum(precip_in)) %>%
#   ungroup() %>%
#   left_join(select(dat_yearly, waterYear, yearly_precip)) %>%
#   mutate(prop_yearly_total = precip_cum/yearly_precip) %>%
#   mutate(over_90 = ifelse(prop_yearly_total >= 0.9, TRUE, FALSE)) %>%
#   group_by(waterYear) %>%
#   filter(over_90 %in% TRUE) %>%
#   summarize(date_over_90 = min(Date)) %>%
#   ungroup() %>%
#   mutate(day_over_90 = yday(date_over_90)) %>%
#   filter(waterYear < 2018)
# 
# plot(dat_cum$day_over_90 ~ dat_cum$waterYear)
# 
# dat_cum <- group_by(dat_fix, waterYear) %>%
#   mutate(precip_cum = cumsum(precip_in)) %>%
#   ungroup() %>%
#   left_join(select(dat_yearly, waterYear, yearly_precip)) %>%
#   mutate(prop_yearly_total = precip_cum/yearly_precip) %>%
#   mutate(over_60 = ifelse(prop_yearly_total >= 0.6, TRUE, FALSE)) %>%
#   group_by(waterYear) %>%
#   filter(over_60 %in% TRUE) %>%
#   summarize(date_over_60 = min(Date)) %>%
#   ungroup() %>%
#   mutate(day_over_60 = yday(date_over_60)) %>%
#   filter(waterYear < 2018)
# 
# plot(dat_cum$day_over_60 ~ dat_cum$waterYear)

# month/year analysis

plot_precip_time <- function(fig_name, precip_dat, start_year, time_period, type) {
  month.cols<-c(viridis(6, begin=.2, end=.99), rev(magma(6, begin=.2, end=.95)))
  precip_dat <- filter(precip_dat, waterYear >= start_year)
  if (time_period == 'month' & type == 'cumulative') {

    p <- ggplot(precip_dat, aes(y = cumulative_month_sum, x = waterYear)) +
      geom_point(aes(color = month)) +
      geom_line(aes(group = month, color = month)) +
      scale_color_manual(values = month.cols) +
      theme_bw() +
      labs(x = 'Water Year', y = 'Cumulative rain @ MSN \nsince 1940 (inches)')
    
   } else if (time_period == 'month' & type == 'totals') {
    
    p <- ggplot(precip_dat, aes(x = waterYear, y = month_sum)) +
      geom_point(aes(color = month)) +
      geom_smooth(aes(group = month, color = month), method = 'lm', se = T) +
      facet_wrap(~month, ncol = 2, scales = 'free_y', dir = 'v') +
      scale_color_manual(values = month.cols, guide = F) +
      theme_bw() +
      labs(x = 'Water Year', y = 'Rain total @ MSN (inches)')
    
  } else if (time_period == 'year' & type == 'totals') {
    p <- ggplot(precip_dat, aes(x = waterYear, y =yearly_precip)) +
      geom_point() +
      geom_smooth(method = 'lm', se = T) +
      theme_bw() +
      labs(x = 'Water Year', y = 'Rain total @ MSN (inches)')
  } else if (time_period == 'year' & type == 'cumulative') {
    p <- ggplot(precip_dat, aes(y = cumulative_yearly_precip, x = waterYear)) +
      geom_point() +
      theme_bw() +
      labs(x = 'Water Year', y = 'Cumulative rain @ MSN \nsince 1940 (inches)')
    
  }
  
  ggsave(fig_name, p, height = ifelse(time_period == 'month' & type == 'totals', 12, 4), 
         width = ifelse(time_period == 'month' & type == 'totals', 9, 6))

}

plot_precip_extremes <- function(fig_name, precip_dat, time_period) {
  
  if (time_period == 'month') {
    p <- ggplot(precip_dat, aes(x = waterYear, y = days_1inch)) +
      geom_point(aes(color = month)) +
      geom_smooth(aes(group = month, color = month), method="glm", method.args=list(family=poisson), se = T) +
      facet_wrap(~month, ncol = 2, scales = 'free_y', dir = 'v') +
      scale_color_manual(values = month.cols, guide = F) +
      theme_bw() + 
      labs(x = "Water Year", y = "Days with 1+ inches \nof rain @ MSN")
  } else {
    p <- ggplot(precip_dat, aes(x = waterYear, y =days_1inch)) +
      geom_point() +
      geom_smooth(method="glm", method.args=list(family=poisson), se = T) +
      theme_bw() + 
      labs(x = "Water Year", y = "Days with 1+ inches \nof rain @ MSN")
  }
  
  ggsave(fig_name, p, height = ifelse(time_period == 'month', 12, 4), 
         width = ifelse(time_period == 'month', 9, 6))

}


library(ggplot2)
library(viridis)
# Use viridis and magma as color choices.
month.cols<-c(viridis(6, begin=.2, end=.99), rev(magma(6, begin=.2, end=.95)))



# 
# dat_month_1990 <- filter(dat_month, waterYear >1989)
# 
# ggplot(dat_month_1990, aes(x = waterYear, y = days_1inch)) +
#   geom_point(aes(color = month)) +
#   geom_smooth(aes(group = month, color = month), method="glm", method.args=list(family=poisson), se = T) +
#   facet_wrap(~month, ncol = 2, scales = 'free_y') +
#   scale_color_manual(values = month.cols, guide = F)
# 
# ggplot(dat_month_1990, aes(x = waterYear, y = month_sum)) +
#   geom_point(aes(color = month)) +
#   geom_smooth(aes(group = month, color = month), method = 'lm', se = T) +
#   facet_wrap(~month, ncol = 2, scales = 'free_y') +
#   scale_color_manual(values = month.cols, guide = F)
# 
# 
# 
# ggplot(dat_yearly, aes(x = waterYear, y =yearly_2inch)) +
#   geom_point() +
#   geom_smooth(method="glm", method.args=list(family=poisson), se = T) +
#   theme_bw() +
#   labs(x = "Water Year", y = "Days with 2+ inches of rain @ MSN")
# 
# 
# 
# ggplot(dat_yearly, aes(x = waterYear, y =yearly_1inch)) +
#   geom_point() +
#   geom_smooth(method="glm", method.args=list(family=poisson), se = T) +
#   theme_bw() +
#   labs(x = "Water Year", y = "Days with 1+ inches of rain @ MSN")
# 
# dat_yearly_1990
# 
