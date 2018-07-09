# get dv values for tp and then subset them to only observed days

get_daily_p <- function(start, end, site, pcode, tp_samples, tp_1997) {
  dat <- readNWISdv(siteNumbers = site, parameterCd = pcode, startDate = start, endDate = end)
  
  # now subset to just days where we have observations
  dates_keep <- unique(tp_samples$Date)
  
  tp_keep <- tp_samples %>%
    select(-ConcLow, -ConcHigh, -ConcAve)
  
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
  
  return(dat_fixed)
    
}