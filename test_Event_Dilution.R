

#setwd("C:/Users/shuprmp/Documents/GitHub/yahara-trends")


tp_mod <- make('tp_wy_out', remake_file = '30_analyze_data_series.yml')


sample = tp_mod$Sample
#subset to 4 or 5 later, but keep for calculating lag time for earliest events
#Could include all months instead of growing season
summerSample = subset (sample, Month >3 & Month <11)

daily = tp_mod$Daily
summerDaily = subset (daily, Month >3 & Month <11)

#use a percentile of all observations as cutoff for event
quantile(summerDaily$Q, c(0.5,0.6,0.7,0.8,0.9,0.95))

#flag all events with a 1
summerDaily$Event = ifelse(summerDaily$Q > quantile(summerDaily$Q, c(0.8)), 1, 0)

#just for a quick plot of model output
summerDaily$col = ifelse(summerDaily$waterYear >2005, 1, 2)
t = subset(summerDaily, Event == 1)
plot(log(t$ConcDay)~log(t$Q), col=t$col)

#Find day directly before event, want low Q in C-Q relationship. edit:Do we???
#for(i in 2:nrow(summerDaily)){
#  if(summerDaily$Event[i] == 1)summerDaily$Event[i-1] = 1
#}

#Identify start date of events
summerDaily$EventStart = 0
for(j in 2:nrow(summerDaily)){
  if(summerDaily$Event[j] == 1 & summerDaily$Event[j-1] != 1) summerDaily$EventStart[j] = 1
}

##############################################################
#Timeframe that defines short&long, how to choose??
#Run all of this code to rerun analysis and plots with different lag times
time = 14
#identify lags? Start of event to end of previous event
summerDaily$lag = NA
for(k in time:nrow(summerDaily)){
  
  if(summerDaily$EventStart[k] == 1){
    x = sum(summerDaily$Event[(k-time):(k-1)])
    summerDaily$lag[k] = ifelse(x > 0, "short", "long")
  }
  
}

# how to pick time lag that is long and short?
nrow(subset(summerDaily, lag == "short"))
nrow(subset(summerDaily, lag == "long"))

#Need to identify all event days as long or short (not just first day)
for(l in 2:nrow(summerDaily)){
  if(summerDaily$Event[l] == 1 & summerDaily$Event[l-1] == 1) summerDaily$lag[l] = summerDaily$lag[l-1]
}

######################################################################
#####################################
# create a numeric varaible for lag days instead of two groups
summerDaily$days_since_last_event = NA
event_days = which(summerDaily$Event == 1)
for (r in 72:nrow(summerDaily)) {
  row = summerDaily[r,]
  if (row$Event == 1) {
    summerDaily$days_since_last_event[r] = 0
  } else {
    days_between_events = r - event_days
    days_between_events[days_between_events < 0] = NA
    summerDaily$days_since_last_event[r] = min(days_between_events, na.rm=T)
  }
}

#plot(summerDaily$days_since_last_event, type = 'l')

#the above loop did not identify numeric value for events, 
#assign events the value fo the previous day non-event plus 1
#events covering multiple days have the same time since last event numeric value
for(s in 1:nrow(summerDaily)){
  if(summerDaily$Event[s] == 1) summerDaily$days_since_last_event[s] = summerDaily$days_since_last_event[s-1]+1
  if(summerDaily$Event[s] == 1 & summerDaily$EventStart[s] == 0) summerDaily$days_since_last_event[s] = summerDaily$days_since_last_event[s-1]
}
#####################################
######################################################################


#merge events (from Daily) with samples, remove April, split by earl/late time periods
tmp = summerDaily[,c("Date", "Event", "lag", "col", "days_since_last_event")]
tmpp = merge(summerSample, tmp, by="Date", all.y = FALSE)
#remove April from analysis if desired
summerSampleM = subset (tmpp, Month >=4 & Month <11)
#first event in record does not have days calcualted
summerSampleM = summerSampleM[2:nrow(summerSampleM),]
#plot(log(ConcAve)~log(Q), summerSampleM, col = col)

summerSampleEarlyLong  = subset(summerSampleM, waterYear < 2005 & lag == "long")
summerSampleEarlyShort = subset(summerSampleM, waterYear < 2005 & lag == "short")
summerSampleLateLong   = subset(summerSampleM, waterYear >=2005 & lag == "long")
summerSampleLateShort  = subset(summerSampleM, waterYear >=2005 & lag == "short")

earlyLong  = lm(log(ConcAve)~log(Q), summerSampleEarlyLong)
earlyShort = lm(log(ConcAve)~log(Q), summerSampleEarlyShort)
lateLong   = lm(log(ConcAve)~log(Q), summerSampleLateLong)
lateShort  = lm(log(ConcAve)~log(Q), summerSampleLateShort)

#2.62 99th percentile of observed log(Q) in summerSampleM (0.11 = log 1.12)
plot(log(ConcAve)~log(Q), summerSampleM, col = 'snow3', xlim = c(0.11,2.62), ylim=c(-3.5,2))
legend(1.1,-2.2, legend = c(
  paste("WY <  2005 event C~Q lag >",time, "days n=", nrow(summerSampleEarlyLong)),
  paste("WY <  2005 event C~Q lag <",time, "days n=", nrow(summerSampleEarlyShort)),
  paste("WY >= 2005 event C~Q lag >",time, "days n=", nrow(summerSampleLateLong)),
  paste("WY >= 2005 event C~Q lag <",time, "days n=", nrow(summerSampleLateShort))),
  col = c("red4", "red", "steelblue4", "steelblue1"),
  lty=1,
  lwd=2,
  bg = "grey98",
  cex=0.8
)

#points(log(ConcAve)~log(Q), summerSampleEarlyLong, col = 'red4')
#points(log(ConcAve)~log(Q), summerSampleEarlyShort,col = 'red')
#points(log(ConcAve)~log(Q), summerSampleLateLong,  col = 'steelblue4')
#points(log(ConcAve)~log(Q), summerSampleLateShort, col = 'steelblue1')
abline(earlyLong, lwd=3, col="red4")
abline(earlyShort,lwd=3, col="red")
abline(lateLong,  lwd=3, col="steelblue4")
abline(lateShort, lwd=3, col="steelblue1")


round(summary(earlyLong) $r.squared, digits = 2)
round(summary(earlyShort)$r.squared, digits = 2)
round(summary(lateLong)  $r.squared, digits = 2)
round(summary(lateShort) $r.squared, digits = 2)




################
########
###
#Does the realtionship between C-Q depend on time between events and water year, can we remove artificial 
#bifurcations and use continuous variables?
#How to set this up?? 3-way interaction? 
#make lag a numeric variable (done at approx line 60)
summary(lm(log(days_since_last_event)~waterYear,summerSampleM))
#days between events is decreasing
#Ancova, not sure I'm doing/interpreting this correctly
lagTimeEffect = aov(log(ConcAve)~log(Q)*log(days_since_last_event)*waterYear, summerSampleM)
summary(lagTimeEffect)
#effect of Q, days since last event and water year
#No interction effects, C-Q slope does not depend on interaction of waterYear*days beteween events
#Should we include all months or just focus on "summer"?

b = barplot(tapply(summerSampleM$days_since_last_event,summerSampleM$waterYear, FUN=mean),
        log = "y",
        ylim = c(1,200),
        ylab = "Mean Days Between Discharge Events",
        xlab = "Water Year")

sd =tapply(summerSampleM$days_since_last_event,summerSampleM$waterYear, FUN=sd)
n =tapply(summerSampleM$days_since_last_event,summerSampleM$waterYear, FUN=length)
mean = tapply(summerSampleM$days_since_last_event,summerSampleM$waterYear, FUN=mean)
se = sd/sqrt(n)
segments(b, (mean-se), b, (mean+se))


##################
plotConcQSmooth(tp_mod, "1995-05-01",NA,"2015-05-01",0.2,10,logScale=TRUE,
                legendLeft=3,legendTop=0.1,printTitle=TRUE, qUnit = 2)
#additionally, the C-Q realtionship is loxer for almost the entire stage (except 0.2 which is <1st percentile [99th exceed]
#of all Q). Dilution (e.g. supply limited watershed) seems unlikey. 

###########################################################
########
################


###############################
######################
################




#test effects edit:using categorical values, probably obsolete given anlysis with continuous variables
summerSampleM$time = ifelse(summerSampleM$waterYear <2005, "early", "late")

lagEffect = aov(log(ConcAve)~log(Q)*lag, summerSampleM)
lagEffectPlus = aov(log(ConcAve)~log(Q)+lag, summerSampleM)
summary(lagEffect)
summary(lagEffectPlus)
print(anova(lagEffect,lagEffectPlus))


timeEffect = aov(log(ConcAve)~log(Q)*time, summerSampleM)
timeEffectPlus = aov(log(ConcAve)~log(Q)+time, summerSampleM)
summary(timeEffect)
summary(timeEffectPlus)
print(anova(timeEffect,timeEffectPlus))
#among all samples no interaction effect of la, but maybe time depending on setup

summerSampleEarly= subset(summerSampleM, waterYear <  2005)
summerSampleLate = subset(summerSampleM, waterYear >= 2005)

#Early
lagEffect = aov(log(ConcAve)~log(Q)*lag, summerSampleEarly)
lagEffectPlus = aov(log(ConcAve)~log(Q)+lag, summerSampleEarly)
summary(lagEffect)
summary(lagEffectPlus)
print(anova(lagEffect,lagEffectPlus))


#Late
lagEffect = aov(log(ConcAve)~log(Q)*lag, summerSampleLate)
lagEffectPlus = aov(log(ConcAve)~log(Q)+lag, summerSampleLate)
summary(lagEffect)
summary(lagEffectPlus)
print(anova(lagEffect,lagEffectPlus))
# No interaction effect of lag on c~Q in early or late samples (although obvious directionality from plotting)
# Although, Interaction effect of time if we include April in the analysis, depending on lag time cut off

#End test effects
################
######################
###############################


