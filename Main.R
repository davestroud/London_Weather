londontmp <- read.csv("londontmp.txt")
dim(londontmp)
londontmp <- londontmp[, 2:3] #subset date and measurement
class(londontmp$DATE)

# Lubricate package is used to correctly format the dates
library(lubridate)
Sys.setenv(TZ = "Europe/London") #only for locals!
londontmp[, 1] <- ymd(londontmp[, 1], locale = Sys.getlocale("LC_TIME")) #locale can be skipped
Sys.getlocale("LC_TIME") #locale can be skipped

colnames(londontmp) <- c("ds", "y")
londontmp$y <- londontmp$y / 10
summary(londontmp$y)

h <- hist(londontmp$y, xlab = "Degrees Celcius",
          main = "Heathrow Temperature Readings 1960-2017",
          ylim = c(0, 3000))
xfit <- seq(min(londontmp$y), max(londontmp$y)) 
yfit <- dnorm(xfit, mean = mean(londontmp$y), sd = sd(londontmp$y)) 
yfit <- yfit * diff(h$mids[1:2]) * length(londontmp$y) 
lines(xfit, yfit, col = "#5694f1", lwd = 2)

plot(density(londontmp$y)) #if you are so inclined.

londontmp$year <- substr(londontmp$ds, 1, 4) #extract first four characters
londontmp$year <- as.integer(londontmp$year)
londontmp$decade <- ifelse(londontmp$year < 1970, "60s",
                           ifelse(londontmp$year < 1980, "70s",
                                  ifelse(londontmp$year < 1990, "80s",
                                         ifelse(londontmp$year < 2000, "90s",
                                                ifelse(londontmp$year < 2010, "00s", "10s")))))
londontmp$decade <- as.factor(londontmp$decade)
londontmp$decade <- factor(londontmp$decade,
                           levels(londontmp$decade)[c(3:6, 1:2)]) #correct order


library(sm)
library(RColorBrewer)
colfill <- brewer.pal(6, "BrBG") #diverging palette
sm.density.compare(x = londontmp$y,
                   group = londontmp$decade,
                   xlab = "Mean Temperature (Celcius)",
                   col = colfill, lty = c(rep(1, 6)),
                   lwd = c(rep(2, 6)), xlim = c(-10, 30))
title(main = "Distributions by Decade")
legend("topright", levels(londontmp$decade),
       fill = colfill, bty = "n")


library(psych)
describe.by(londontmp[, 2], londontmp$decade)


library(prophet)
set.seed(1895) # in casee you pass mcmc.samples below
m <- prophet(londontmp, daily.seasonality = FALSE)

future <- make_future_dataframe(m, periods = 362 * 2, include_history = FALSE)
head(future)


