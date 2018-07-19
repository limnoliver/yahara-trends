library(dplyr)
library(tidyr)
library(ggplot2)
# for each water year, pull out all days with > 1 inch of rain
precip <- make('msn_precip', remake_file = '10_get_data.yml')

# get all daily loads from gclas
loads <- make('tp_loads_gclas', remake_file = '10_get_data.yml')

# define precip extremes as those with >99th percentile of rain
rain99 <- quantile(precip$precip_in, probs = .99)
annual_extremes <- precip %>%
  filter(waterYear >= 1990 & waterYear < 2018,
         precip_in >= rain99)

# find median date and number of extreme events per year
n_extremes <- annual_extremes %>%
  group_by(waterYear) %>%
  summarize(n_99th = n(),
            median_date = median(Date)) %>%
  mutate(median_date = yday(median_date))

# be sure to add zeros if there is no data for a water year (since we filtered out non-extremes)
all_dates <- 1990:2017
years_missing <- all_dates[-which(all_dates %in% n_extremes$waterYear)]
fill_dat <- data.frame(waterYear = years_missing, 
                       n_99th= 0, 
                       median_date = NA)

n_extremes <- bind_rows(n_extremes, fill_dat) %>%
  arrange(waterYear)

# calculate the number of days between extreme events
dates_extreme <- data.frame(Date = annual_extremes$Date, extreme_precip_day = 1) %>%
  mutate(diff = c(NA, diff(Date)), end_Date = Date + 3)

# create loop that fixes end dates if 4-day window
# bumps up against another extreme day
new_dates <- as.Date('1988-01-01')

for (i in 2:nrow(dates_extreme)) {
  
  if (dates_extreme$diff[i] < 4) {
    new_dates[i-1] <- as.Date(dates_extreme$Date[i-1]) + (dates_extreme$diff[i] - 1)
  } else {
    new_dates[i-1] <- as.Date(dates_extreme$end_Date[i-1])
  }
}
# fill in last date
new_dates[nrow(dates_extreme)] <- as.Date(dates_extreme$end_Date[nrow(dates_extreme)])
dates_extreme$new_dates <- new_dates

# now calculate sum of loads based on start end dates
for (i in 1:nrow(dates_extreme)) {
  start <- dates_extreme$Date[i]
  end <- dates_extreme$new_dates[i]
  
  extreme_load <- filter(loads, Date >= start & Date <= end)

  dates_extreme$sum_load[i] <- sum(extreme_load$tp_pounds)
}  
  
head(dates_extreme)
dates_extreme <- addWaterYear(dates_extreme)

# sum "extreme" loads by water year
annual_extreme_loads <- dates_extreme %>%
  group_by(waterYear) %>%
  summarize(sum_extreme_load = sum(sum_load))

# be sure to add zeros if there is no data for a water year (since we filtered out non-extremes)
all_dates <- 1990:2017
years_missing <- all_dates[-which(all_dates %in% annual_extreme_loads$waterYear)]
fill_dat <- data.frame(waterYear = years_missing, 
                       sum_extreme_load= 0)

annual_extreme_loads <- bind_rows(annual_extreme_loads, fill_dat) %>%
  arrange(waterYear)

loads <- addWaterYear(loads)

# calculate annual total loads
annual_total_loads <- loads %>%
  group_by(waterYear) %>%
  summarize(annual_load = sum(tp_pounds))

# create an df with both annual loads, extreme annual loads
# and the diff between the two which we'll call "normal" loads
annual_loads <- left_join(annual_total_loads, annual_extreme_loads) %>%
  mutate(normal_loads = annual_load - sum_extreme_load) %>%
  select(-annual_load) %>%
  gather(key = load_types, value = annual_load, -waterYear)

# create a df of just "normal" loads
annual_normal_loads <- filter(annual_loads, load_types == 'normal_loads')

plot(annual_normal_loads$annual_load ~ annual_normal_loads$waterYear)

# plot number of extremes through time
plot(n_extremes$n_99th~n_extremes$waterYear)
abline(lm(n_extremes$n_99th~n_extremes$waterYear))

plot(annual_extreme_loads$sum_extreme_load ~ annual_extreme_loads$waterYear, 
     xlab = "Water Year", ylab = "Annual load associated \nwith > 1inch precip")
abline(lm(annual_extreme_loads$sum_extreme_load ~ annual_extreme_loads$waterYear))

#
ggplot(annual_loads, aes(x = waterYear, y = annual_load)) +
  geom_bar(stat = 'identity', aes(fill = load_types))

# calculate annual totals associated with "normal" days 
# and "extreme" (99th percentile) days
annual_total_precip <- precip %>%
  filter(waterYear >= 1990, waterYear < 2017) %>% 
  group_by(waterYear) %>%
  summarize(normal_precip = sum(precip_in[precip_in < rain99]),
            extreme_precip = sum(precip_in[precip_in >= rain99]), 
            total_precip = sum(precip_in), 
            extreme_percent = 100*(extreme_precip/total_precip))

annual_precip_long <- select(annual_total_precip, -total_precip, -extreme_percent) %>%
  gather(key = 'precip_type', value = 'precip_total', -waterYear)

annual_precip_long$precip_type <- factor(annual_precip_long$precip_type, levels = c('normal_precip', 'extreme_precip'))

ggplot(annual_precip_long, aes(x = waterYear, y = precip_total)) +
  geom_bar(stat = 'identity', aes(fill = precip_type))

annual_precip_long <- select(annual_total_precip, -normal_precip, -extreme_percent) %>%
  gather(key = 'precip_type', value = 'precip_total', -waterYear)

ggplot(annual_precip_long, aes(x = waterYear, y = precip_total)) +
  geom_line(aes(color = precip_type, group = precip_type))

head(annual)
