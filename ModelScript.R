options(max.print=100000)
# install.packages(dplyr)
# install.packages(zoo)
# install.packages(tidyverse)
#install.packages(visreg)
library(dplyr)
library(zoo)
library(tidyverse)
library(ggplot2)
library(visreg)
library(MASS)
library(stargazer)
library(reshape2)

#We are trying to look at interaction between 
#Event - announcement/date of festival
#Treatment - artist selected for festival
#in terms of the daily trend in number of followers, popularity and number of listeners
loader<-function(filename, eventdate, nMonths, dateformat){
  data<-read.csv(filename, header = TRUE)
  data$popularity<-na.locf(na.locf(data$popularity), fromLast = TRUE)
  data$date = as.Date(data$timestp,format = dateformat)
  announcement_date = eventdate
  byparam_end = paste(nMonths,"months")
  byparam = paste("-",byparam_end,sep="")
  subset_start_dt = seq(announcement_date, length = 2, by = byparam)[2]
  subset_end_dt = seq(announcement_date, length = 2, by = byparam_end)[2]
  subset_data = subset(data, date>subset_start_dt & date < subset_end_dt)
  subset_data = subset_data %>%
    group_by(artist) %>%
    mutate(normListeners = listeners/first(listeners))
  subset_data = subset_data %>%
    group_by(artist) %>%
    mutate(normFollowers = followers/first(followers))
  subset_data = subset_data %>%
    group_by(artist) %>%
    mutate(normPopularity = popularity/first(popularity))
  subset_data$artist = as.factor(subset_data$artist)
  #Create dummy variable for post event
  subset_data$Event <- 0
  subset_data$Event[subset_data$date>announcement_date]=1
  #Create time-step variable
  subset_data = subset_data %>% 
    group_by(artist) %>% 
    mutate(timestep = row_number())
  #Create event and time step interaction
  #We want to see the effect on the daily trend in listeners before and after announcement
  #We will control for artist level fixed effects(separate intercept per artist)
  subset_data$EventInteraction <- subset_data$Event * subset_data$timestep
  subset_data
}

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
remove_outlier_artists<-function(in_data){
  in_data_ro = in_data %>%group_by(artist) %>% mutate(new = remove_outliers(listeners))
  in_data_ro = in_data_ro[!is.na(in_data_ro$new),]
  #anything where the variable more than doubles during the study is considered hypergrowth and removed
  in_data_ro = in_data_ro %>%
    group_by(artist) %>%
    filter(!any(log(normListeners) > 2))
  in_data_ro
}
linear_growth_model<-function(in_data){
  mod = lm(normListeners~timestep+artist+EventInteraction+treatedInteraction+didInteraction, data = in_data)
  mod
}
exponential_growth_model<-function(in_data){
  mod = lm(log(normListeners)~timestep+artist+EventInteraction+treatedInteraction+didInteraction, data = in_data)
  mod
}
chart_compare_means_by_artist<-function(selected, control){
  ####CHARTING MEANS CONTROL vs TREATED#########
  #These are the charts with blue and orange lines comparing control and selected artists mean growth rate
  control_summary = control %>% group_by(date) %>% summarise(mean=mean(normListeners), sd=sd(normListeners))
  selected_summary = selected %>% group_by(date) %>% summarise(mean=mean(normListeners), sd=sd(normListeners))
  means = data.frame(selected = selected_summary$mean,control = control_summary$mean, dates = selected_summary$date)
  meltmeans <- melt(means, id.vars="dates")
  ggplot(meltmeans, aes(dates,value, col=variable)) + 
    geom_point() + 
    geom_vline(xintercept=means$dates[31]) +
    stat_smooth() 
}

sample_by_days<-function(in_data, interval){
  in_data[seq(1, length(in_data$date), interval),]
}

#USAGE - change the event date and the window you want to investigate around the event
#NMonth means that the study will take place from event date - NMonth months to event date + NMonth months
#event_dates = c(as.Date("1/2/2019","%m/%d/%Y"),as.Date("4/14/2019","%m/%d/%Y"),as.Date("4/21/2019","%m/%d/%Y"))
event_date = as.Date("4/21/2019","%m/%d/%Y")
nMonth = 2
#Create dataset, this will deal with missing data, add interaction variables for time effects
subset_data=loader('spotify_fan_metrics_binded2.csv',event_date,nMonth,"%m/%d/%Y")
##DID Model###
control_data = loader('control_artists.csv',event_date,nMonth,"%Y-%m-%d")
subset_data$treated = 1
control_data$treated = 0
chart_compare_means_by_artist(subset_data, control_data)

did_data = bind_rows(subset_data, control_data)
did_data$treatedInteraction = did_data$treated * did_data$timestep
did_data$did = did_data$Event * did_data$treated
did_data$didInteraction = did_data$did * did_data$timestep
#Linear growth rates
summary(linear_growth_model(did_data))
#exponential growth rates
summary(exponential_growth_model(did_data))
#Try some outlier removal
did_data_ro = remove_outlier_artists(did_data)
summary(exponential_growth_model(did_data_ro))
#Outputs are very similar
#####STARGAZER tables
#stargazer(mod2, type="html", out = "LinearModel.htm")
#stargazer(mod3, type="html", out = "ExponentialModel.htm")
#######VisReg visualizations
#Chart of logNormListeners vs Event interaction
#visreg(lm(log(normListeners)~timestep+EventInteraction+treatedInteraction+didInteraction, data = did_data),"EventInteraction")
#Chart of logNormlisteners, comparing selected vs control artists
#visreg(lm(log(normListeners)~timestep+treated, data = did_data),"timestep","treatedInteraction")
#Chart of logNormlisteners, comparing selected vs control artists after removing outlieres
#visreg(lm(log(normListeners)~timestep+treated, data = did_data_ro),"timestep","treatedInteraction")
library(gridExtra)
did_sampled<-sample_by_days(did_data_ro,15)
plot1 = ggplot(subset(did_sampled, treated==0), aes(y=log(normListeners), x=timestep)) + geom_point(color="turquoise") + stat_smooth()+ coord_cartesian(ylim=c(-2,2)) +ggtitle("Control")+geom_vline(xintercept=60)
plot2 = ggplot(subset(did_sampled, treated==1), aes(y=log(normListeners), x=timestep)) + geom_point(color="lightcoral") + stat_smooth()+ coord_cartesian(ylim=c(-2,2))+ ggtitle("Selected") +geom_vline(xintercept=60)
grid.arrange(plot1, plot2, ncol=2)
