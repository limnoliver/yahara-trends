# plot flow normalized esimates

tp <- make('tp_wy_out', remake_file = '30_analyze_data_series.yml')
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
  mutate(doy = lubridate::yday(Date)) %>%
  #filter(waterYear == 2018) %>%
  arrange(waterYear, -FluxDay) %>%
  group_by(waterYear) %>%
  mutate(prop_total = FluxDay/sum(FluxDay)) %>%
  mutate(cumulative_prop_total = cumsum(prop_total),
         day = rank(-prop_total)) %>%
  ungroup() %>%
  select(waterYear, doy, day, cumulative_prop_total)

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
