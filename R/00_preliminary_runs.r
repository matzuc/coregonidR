
# the aim of this script is to do some preliminar test on the egg development model / daily cohorts


library(ggplot2)
library(lubridate)
library(ggthemes)
library(dplyr)



# 01 - temperature data -----------------------
# load temperature data for the Garda lake (Salmaso et al., 2003) as an example
# data frame with date, year and wt (= water temperature - upper layer [0-15 m] in this case)

load(here::here("R/data/wtemp.RData"))
wtemp$date <- as.Date(wtemp$date)
# ggplot(wtemp, aes(date, wt)) + geom_line() + theme_few() + ylab('Water temperature (°C)') + scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") + theme(axis.text.x=element_text(angle=90, hjust=1))





# 02 - define spawning characteristics---------
# spawning phenology is define as a normal function (Jones et al., 2003), but the first and last day of spawning must be provided, or they are obtained considering the start/stop temperature (should be given) as in Jones et al (2003), considering the first days with running average temperature reaching the critical thresholds

wtemp$t5 <- caTools::runmean(wtemp$wt, 5, align = 'right') # 5 days rolling average

startsp <- 11 # °C [MODEL PARAMETER]
stopsp <- 8.5 # °C [MODEL PARAMETER] # not sure this makes sense at all



# define spawning windows

# start with first season (Example)
dat01 <- subset(wtemp, date >= as_date('1996-10-01') & date <= as_date('1997-06-01'))
#ggplot(dat01, aes(date, wt)) + geom_line() + theme_few() + ylab('Water temperature (°C)') + scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") + theme(axis.text.x=element_text(angle=90, hjust=1))

mystartsp <- dat01$date[min(which(dat01$t5 < startsp))]
mystopsp <- dat01$date[min(which(dat01$t5 < stopsp))]

mu <- as_date((as.numeric(mystartsp) + as.numeric(mystopsp)) / 2) # center of spawning windows
sigm <- (as.numeric(mystopsp) - as.numeric(mu)) / 2.58 # Days
#ggplot(dat01, aes(date, wt)) + geom_line() + theme_few() + ylab('Water temperature (°C)') + scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") + theme(axis.text.x=element_text(angle=90, hjust=1)) + geom_vline(xintercept = mu, colour = "red") + geom_vline(xintercept = as_date(c(as.numeric(mu) - sigm, as.numeric(mu) + sigm)), colour = "blue", lty = 3)



# 03 Population matrix ----------------------
# let's follow a population of N fertilized eggs

N <- 100 # define how many embryos we want to follow

spawning <- data.frame(date = as_date(sort(floor(rnorm(N, as.numeric(mu), sigm)))))
# ggplot(spawning, aes(date)) + geom_histogram() + theme_few()

# create a population matrix
pop <- matrix(data = NA, ncol= N, nrow = nrow(dat01)) # colums = specimesn; rows = days


pop[spawning$date[1] ==  dat01$date, 1:N] <- 0 # initial population 0= spawning date

# 04 Degree days Model -----------------------
# define the degreedays model
#As an example we use the model for C. lavaretus defined by Eckman (1987) after Luczynski & Kirklwska (1984)
#lnDRi= a -b * ln T : Di = rime for DS14 (50% hatch)

