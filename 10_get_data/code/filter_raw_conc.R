subset_raw_conc <- function(raw_dat, stat = 'first') {
  
  if (stat == 'first') {
    filt_dat <- raw_dat %>%
      group_by(Date) %>%
      summarize_all(funs(first))
  } else if (stat == 'last') {
    filt_dat <- raw_dat %>%
      group_by(Date) %>%
      summarize_all(funs(last))
  } else if (stat == 'mean') {
    filt_dat <- raw_dat %>%
      group_by(Date) %>%
      summarize_all(funs(mean))
  } else if (stat == 'random') {
    filt_dat <- raw_dat %>%
      group_by(Date) %>%
      sample_n(size = 1)
  }
  
  return(filt_dat)
}