# get dv values for tp and then subset them to only observed days
get_gclas_p <- function(start, end, site, pcode) {
  dat <- readNWISdv(siteNumbers = site, parameterCd = pcode, startDate = start, endDate = end)
  dat <- rename(dat, tp_pounds = X_91050_00003, tp_cd = X_91050_00003_cd)
  return(dat)
}
get_daily_p <- function(start, end, site, pcode, tp_samples, tp_1997) {
  dat <- readNWISdv(siteNumbers = site, parameterCd = pcode, startDate = start, endDate = end)
  
  # now subset to just days where we have observations
  dates_keep <- unique(tp_samples$Date)
  
  tp_keep <- tp_samples %>%
    select(-ConcLow, -ConcHigh, -ConcAve) %>%
    distinct()
  
  # some dvs are missing -- find the ones that are missing, and just take the mean
  missing_97 <- tp_1997 %>%
    select(Date, ConcLow = `TP.Conc..mg.L.`) %>%
    mutate(ConcHigh = ConcLow, ConcAve = ConcLow, Date = as.Date(Date, format = '%m/%d/%Y')) %>%
    filter(Date %in% dates_keep)
  
  dat_fixed <- filter(dat, Date %in% dates_keep) %>%
    select(Date, ConcLow = X_00665_00003) %>%
    mutate(ConcHigh = ConcLow,
           ConcAve = ConcLow) %>%
    bind_rows(missing_97) %>%
    left_join(tp_keep) %>%
    mutate(Uncen = 1) %>%
    arrange(Date)
  
  # 81050 is TP load
  return(dat_fixed)
    
}

# get dv values for tp and then subset them to only observed days

get_daily_p_fn <- function(start, end, site, pcode, q, tp_samples, tp_1997) {
  dat <- readNWISdv(siteNumbers = site, parameterCd = pcode, startDate = start, endDate = end)
  
  # now merge with Q dat to get daily discharge
  dat_q <- left_join(dat, select(q, Date, Q))
  
  # calculate flow-normalized concentration
  # convert to correct conc units
  dat_q <- mutate(dat_q, X_91050_00003 = (X_91050_00003/Q)*(0.18539/35.314667))
  
  # now subset to just days where we have observations
  dates_keep <- unique(tp_samples$Date)
  
  tp_keep <- tp_samples %>%
    select(-ConcLow, -ConcHigh, -ConcAve) %>%
    distinct()
  
  # keep only dvs from dates sampled, change column names
  dat_fixed <- filter(dat_q, Date %in% dates_keep) %>%
    select(Date, ConcLow = X_91050_00003) %>%
    mutate(ConcHigh = ConcLow,
           ConcAve = ConcLow)
  
  # some dvs are missing -- find the ones that are missing, and just take the mean
  missing_97 <- tp_1997 %>%
    select(Date, ConcLow = `TP.Conc..mg.L.`) %>%
    mutate(ConcHigh = ConcLow, ConcAve = ConcLow, Date = as.Date(Date, format = '%m/%d/%Y')) %>%
    filter(Date %in% dates_keep) %>%
    filter(!(Date %in% dat_fixed$Date))
  
  dat_fixed <- dat_fixed %>%
    bind_rows(missing_97) %>%
    left_join(tp_keep) %>%
    mutate(Uncen = 1) %>%
    arrange(Date)
  
  # 81050 is TP load
  return(dat_fixed)
  
}

get_daily_ss <- function(start, end, site, pcode, ss_samples) {
  dat <- readNWISdv(siteNumbers = site, parameterCd = pcode, startDate = start, endDate = end)
  
  # now subset to just days where we have observations
  dates_keep <- unique(ss_samples$Date)
  
  ss_keep <- ss_samples %>%
    select(-ConcLow, -ConcHigh, -ConcAve) %>%
    distinct()
  
  
  dat_fixed <- filter(dat, Date %in% dates_keep) %>%
    select(Date, ConcLow = X_80154_00003) %>%
    mutate(ConcHigh = ConcLow,
           ConcAve = ConcLow) %>%
    left_join(ss_keep) %>%
    mutate(Uncen = 1) %>%
    arrange(Date)
  
  return(dat_fixed)
}