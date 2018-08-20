compare_names <- c('Yahara', "Black Earth", "Badger Mill", 'Spring Harbor', "Pheasant Branch", "E.B. Pecatonica")
compare_abb <- c('YAH', 'BEC', 'BMC', 'SHC', 'PBC', 'EBP')

yah <- make('discharge', remake_file = '10_get_data.yml')
other <- make('q_compare', remake_file = '11_get_comparison_gage_data.yml')
all_streams <- c(list(yah), other)

# use dplyr to calculate annual cumulative discharge year to hear
library(dplyr)

cum_discharge <- as.data.frame(matrix(ncol = 4, nrow = 0))
names(cum_discharge) <- c('waterYear', 'n', 'cum_discharge', 'site')
for (i in 1:length(all_streams)) {
  temp <- all_streams[[i]]
  
  annual_cum_discharge <- temp %>%
    group_by(waterYear) %>%
    summarize(n = n(),
              cum_discharge = sum(Q)) %>%
    mutate(site = compare_names[i])
  
  cum_discharge <- bind_rows(cum_discharge, annual_cum_discharge)
}

cum_discharge <- filter(cum_discharge, n >= 365)

p_all <- ggplot(cum_discharge, aes(x = waterYear, y = cum_discharge)) +
  geom_point(aes(color = site)) +
  geom_smooth(aes(group = site, color = site), method = 'lm') +
  geom_line(aes(group = site, color = site), alpha = 0.3) +
  scale_y_log10() +
  theme_bw() +
  labs(x = '', y = 'Annual Discharge')

cum_discharge_1990 <- filter(cum_discharge, waterYear >= 1990)

p_1990 <- ggplot(cum_discharge_1990, aes(x = waterYear, y = cum_discharge)) +
  geom_point(aes(color = site)) +
  geom_smooth(aes(group = site, color = site), method = 'lm') +
  geom_line(aes(group = site, color = site), alpha = 0.3) +
  scale_y_log10() +
  theme_bw() +
  labs(x = '', y = 'Annual Discharge')

ggsave('figures/site_flow_comparison_allyears.png', p_all, height = 6, width = 8)
ggsave('figures/site_flow_comparison_1990.png', p_1990, height = 6, width = 8)
