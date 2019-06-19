# dat_out <- tp_mod
# dat_out_ws1 <- make('tp_wy_out_ws1', remake_file = '30_analyze_data_series.yml')

plot_contours <- function(eList, target_name, conc_breaks, yearStart = 1990, yearEnd = 2018) {
  png(target_name, height = 500, width = 700, pointsize = 16)
  plotContours(eList, yearStart = yearStart, yearEnd = yearEnd,
               contourLevels = conc_breaks, printTitle = FALSE,
               customPar = TRUE)
  dev.off()
}
calc_probs <- function(eList, probs) {
  dat <- eList$Daily$Q
  return(round(quantile(dat, probs = probs), 2))
}
plot_conditions <- function(file_name, eList, year1, year2, year3, date1, date2, qlegend, tlegend,
                            qvals, conc_range1, conc_range2) {
  png(file_name, height = 600, width = 800)
  par(mfrow = c(2,2))
  plotConcQSmooth(eList, date1=paste0(year1, '-', date1),
                  date2=paste0(year2, '-', date1),date3=paste0(year3, '-', date1),
                  qLow=0.2,qHigh=10, logScale=TRUE,
                  legendLeft=qlegend[1],legendTop=qlegend[2], cex.legend = 0.7,
                  concMin=conc_range1[1], concMax=conc_range1[2])
  plotConcQSmooth(eList, date1=paste0(year1, '-', date2),
                  date2=paste0(year2, '-', date2),date3=paste0(year3, '-', date2),
                  qLow=0.2,qHigh=10, logScale=TRUE,
                  legendLeft=qlegend[1],legendTop=qlegend[2], cex.legend = 0.7,
                  concMin=conc_range1[1], concMax=conc_range1[2])
  plotConcTimeSmooth(eList, q1 = qvals[1], q2 = qvals[2], q3 = qvals[3],
                     centerDate = date1, yearStart = 1990, yearEnd = 2018,
                     cex.legend = .7, legendLeft=tlegend[1],legendTop=tlegend[2],
                     concMin = conc_range2[1], concMax = conc_range2[2])
  plotConcTimeSmooth(eList, q1 = qvals[1], q2 = qvals[2], q3 = qvals[3],
                     centerDate = date2, yearStart = 1990, yearEnd = 2018,
                     cex.legend = .7, legendLeft=tlegend[1],legendTop=tlegend[2],
                     concMin = conc_range2[1], concMax = conc_range2[2])
  dev.off()

}
