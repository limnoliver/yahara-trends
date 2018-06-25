combine_p_dat <- function(diss_p, ortho_p_filt, ortho_p_unfilt) {
  ortho_p_filt_dates <- select(ortho_p_filt, Date) %>%
    distinct() %>%
    pull(Date)
  
  all_p <- bind_rows(ortho_p_filt, filter(diss_p, !(Date %in% ortho_p_filt_dates)), filter(ortho_p_unfilt, !(Date %in% ortho_p_filt_dates))) %>%
    arrange(Date)
  
  return(all_p)
}