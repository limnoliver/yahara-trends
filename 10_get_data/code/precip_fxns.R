# get precip data from Yahara watershed
get_precip <- function(file_loc) {
  dat <- read.csv(file_loc, header = F, stringsAsFactors = F)
  names(dat) <- c('Date', 'max_temp_F', 'min_temp_F', 'precip_in', 'snowfall_in', 'snowdepth_in', 'time_obs1', 'time_obs2', 'time_obs3', 'time_obs4', 'time_obs5')
  dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")
  
  
  # add water year
  dat <- addWaterYear(dat)
  
  # sum by year, get cumulative precip
  
  dat_fix <- mutate(dat, precip_in = ifelse(precip_in %in% "T", 0, precip_in)) %>%
    mutate(precip_in = as.numeric(precip_in))
  
  return(dat_fix)
}

summarize_yearly <- function(precip_dat) {
  dat_yearly <- precip_dat %>%
    group_by(waterYear) %>%
    summarize(yearly_precip = sum(precip_in, na.rm = T),
              days_1inch = sum(precip_in >= 1, na.rm = T),
              days_2inch = sum(precip_in >= 2, na.rm = T), 
              days_3inch = sum(precip_in >= 3, na.rm = T)) %>%
    filter(waterYear < 2018) %>%
    ungroup() %>%
    mutate(cumulative_yearly_precip = cumsum(yearly_precip))
  
  return(dat_yearly)
}

summarize_monthly <- function(precip_dat) {
  dat_month <- mutate(precip_dat, month = month(Date, label = T)) %>%
    group_by(waterYear, month) %>%
    summarize(month_sum = sum(precip_in),
              days_1inch = sum(precip_in > 1)) %>%
    ungroup()
  
  cumulative_month <- dat_month %>%
    group_by(month) %>%
    mutate(cumulative_month_sum = cumsum(month_sum)) %>%
    filter(waterYear < 2018) %>%
    ungroup() %>%
    left_join(dat_month)
  
  return(cumulative_month)
}