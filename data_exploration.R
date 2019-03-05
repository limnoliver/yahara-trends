# plot flow normalized esimates

tp <- make('tp_wy_out', remake_file = '30_analyze_data_series.yml')

tp_bands <- tidyr::gather(tp_wy_out_boot, key = 'variable', value = 'value', -Year)
tp_bands_flux <- filter(tp_bands, variable %in% c('FNFluxLow', 'FNFluxHigh'))

ggplot(tp_bands_flux, aes(x = Year, y = value)) +
  geom_smooth(aes(group = variable))
  
tableResults(tp_wy_out_boot)
yearly_tp <- tableResults(tp)

year_points <- c(1991, 2000, 2009, 2018)
tp_change <- tableChangeSingle(tp, yearPoints = year_points)
plotConcHist(tp)
plotContours(tp, yearStart = 1991, yearEnd = 2018,
             contourLevels = seq(0,0.8,0.1), qBottom = 0.28,
             qTop = 1.7)

# how has the relationship between conc and Q changed?
plotConcQSmooth(tp, date1="1991-01-01",
                date2="2004-01-01",date3="2018-01-01",
                qLow=0.2,qHigh=10, logScale=TRUE,
                legendLeft=2,legendTop=0.2,printTitle=FALSE)

# how does the relationship between conc and Q change over seasons?
plotConcQSmooth(tp, date1=,"2018-03-01",
                date2="2018-06-01",date3="2018-09-01",
                qLow=0.2,qHigh=10, logScale=TRUE,
                legendLeft=2,legendTop=0.2,printTitle=FALSE)

# how has conc changed over time at different Qs?
plotConcTimeSmooth(tp, q1=0.32, q2=0.58, q3=1.18, centerDate="03-01",
                   yearStart=1991, yearEnd=2018, 
                   logScale=FALSE, legendLeft=1994, legendTop=0.35,
                   printTitle=FALSE, printLegend = FALSE)
plotFlux
library(dplyr)
tp_n_events_50 <- tp$Daily %>%
  mutate(doy = lubridate::yday(Date),
         monthday = format(Date, format = '%m-%d'),
         month = lubridate::month(Date, label = TRUE)) %>%
  #filter(waterYear == 2018) %>%
  arrange(waterYear, -FluxDay) %>%
  group_by(waterYear) %>%
  mutate(prop_total = FluxDay/sum(FluxDay)) %>%
  mutate(cumulative_prop_total = cumsum(prop_total),
         day = rank(-prop_total)) %>%
  ungroup() %>%
  select(waterYear, doy, day, monthday, month, cumulative_prop_total, prop_total)

daily_prop_total <- group_by(tp_n_events_50, doy) %>%
  summarize(prop_total_median = median(prop_total),
            prop_total_90 = quantile(prop_total, 0.9),
            prop_total_10 = quantile(prop_total, 0.1),
            prop_total_mean = mean(prop_total)) %>%
  mutate(rolling_mean = zoo::rollmean(prop_total_median, 14, na.pad = TRUE))

monthly_prop_total <- group_by(tp_n_events_50, waterYear, month) %>%
  summarize(month_prop_total = sum(prop_total))

pmonth <- ggplot(monthly_prop_total, aes(x = month, y = month_prop_total)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = '', y = 'Proportion of annual load')

ggsave('figures/monthly_prop_annual_load.png',pmonth, height = 4, width = 6)
p <- ggplot(daily_prop_total, aes(x = doy, y = prop_total_median)) +
  geom_rect(aes(xmin = 46, xmax = 135, ymin = -Inf, ymax = Inf), fill = 'lightgray', alpha = 0.1) +
  geom_rect(aes(xmin = 159, xmax = 189, ymin = -Inf, ymax = Inf), fill = 'lightgray', alpha = 0.1) +
  geom_point(color = 'darkgray', alpha = 0.8) +
  scale_x_continuous(breaks = c(1,60, 121, 182,244, 305), 
                     labels = c('Jan 1', 'March 1', 'May 1', 'Jul 1', 'Sep 1', 'Nov 1')) +
  geom_line(aes(y = zoo::rollmean(prop_total_median, 14, na.pad = TRUE)), color = 'red') +
  
  theme_bw() +
  labs(x = '', y = "Proportion of annual load (1990-2018)")

p2 <- ggplot(daily_prop_total, aes(x = doy, y = prop_total_mean)) +
  geom_rect(aes(xmin = 40, xmax = 110, ymin = -Inf, ymax = Inf), fill = 'lightgray', alpha = 0.1) +
  geom_rect(aes(xmin = 145, xmax = 180, ymin = -Inf, ymax = Inf), fill = 'lightgray', alpha = 0.1) +
  geom_point(color = 'darkgray', alpha = 0.8) +
  scale_x_continuous(breaks = c(1,60, 121, 182,244, 305), 
                     labels = c('Jan 1', 'March 1', 'May 1', 'Jul 1', 'Sep 1', 'Nov 1')) +
  geom_line(aes(y = zoo::rollmean(prop_total_mean, 14, na.pad = TRUE)), color = 'red') +
  
  theme_bw() +
  labs(x = '', y = "Proportion of annual load (1990-2018)")
  
ggsave('figures/daily_prop_annual_load_median.png', p, height = 4, width = 6)
ggsave('figures/daily_prop_annual_load_mean.png', p2, height = 4, width = 6)


days10 <- filter(tp_n_events_50, day == 10)

tp_days_to_50 <- tp_n_events_50 %>%
  group_by(waterYear) %>%
  summarize(day_to_50 = which.min(abs(cumulative_prop_total - 0.50)))

tp_days_to_30 <- tp_n_events_50 %>%
  group_by(waterYear) %>%
  summarize(day_to_30 = which.min(abs(cumulative_prop_total - 0.30)))
library(ggplot2)

top_10_days <- filter(tp_n_events_50, day %in% 1:10)

length(which())
ggplot(top_10_days, aes(x = waterYear, y = doy)) +
  geom_point(aes(color = cumulative_prop_total))

ggplot(tp_n_events_50, aes(x = day, y = cumulative_prop_total)) +
  geom_line(aes(group = waterYear, color = waterYear)) +
  scale_color_gradient2(low = 'blue', mid = 'gray', high = 'red', midpoint = 2005) +
  coord_cartesian(xlim = c(0, 50))

plot(tp_n_events_50$cumulative_prop_total ~ c(1:365))
