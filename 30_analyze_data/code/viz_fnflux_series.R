tp_series <- make('tp_wy_out', remake_file = '30_analyze_data_series.yml')
yearly_tp <- tableResults(tp_series, fluxUnit = 5)
year_tp_boot <- make('tp_wy_out_boot', remake_file = '30_analyze_data_series.yml')

head(yearly_tp)
