instaVals <- dataRetrieval::readNWISqw(site, c('00665', '00671', '70507', '00666', '80154', '00061'))
library(dplyr)
insta_wide <- instaVals %>%
  select(sample_dt, sample_tm, startDateTime, parm_cd, remark_cd, result_va) %>%
  tidyr::spread(key = 'parm_cd', value = "result_va")


library(ggplot2)
yah_params <- filter(dataRetrieval::parameterCdFile, parameter_cd %in% c('00665', '00671', '70507', '00666', '80154', '00061'))

instaVals <- left_join(instaVals, select(yah_params, parameter_cd, parameter_nm), by = c('parm_cd' = 'parameter_cd'))
p <- ggplot(instaVals, aes(x = sample_dt, y = result_va)) +
  geom_point(alpha = 0.8, size = 0.7) +
  facet_wrap(~parameter_nm, ncol = 1, scales = 'free_y') +
  theme_bw() +
  labs(x = '', y = '')


write.csv(insta_wide, '10_get_data/out/yahara_instantaneous_vals.csv', row.names = FALSE)
write.csv(yah_params, '10_get_data/out/yahara_parameter_descriptions.csv', row.names = FALSE)
ggsave('10_get_data/out/data_availability_throughtime.png', height = 7, width = 10)
