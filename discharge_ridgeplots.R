library(ggridges)
library(ggplot2)
library(viridis)

annual <- mutate(annual, rank = rank(annual_dis))
discharge <- dis %>%
  mutate(year = dataRetrieval::calcWaterYear(Date)) %>%
  left_join(select(annual, year, rank))

p <- ggplot(discharge, aes(x = Q, y = year, group = year, fill = rank)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2, alpha = 0.8) +
  coord_cartesian(xlim = c(0.1, 2)) +
  #scale_x_log10() +
  scale_y_reverse(breaks = seq(1990, 2015, by = 5)) +
  #theme_ridges() +
  scale_fill_gradient2(low = 'blue', mid = 'gray', high = 'red', midpoint = 14.5) +
  theme_classic() +
  labs(y = '', x = "Daily Q (ft3/s)")

ggsave('figures/daily_q_ridgeplot.png', p, height = 8, width = 4)

