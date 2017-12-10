londontmp <- read.csv("Home/londontmp.txt")
dim(londontmp)
colnames(londontmp)
londontmp <- londontmp[, 2:3] # subset date and measurement
class(londontmp$DATE)
library(lubridate)
Sys.setenv(TZ = "Europe/London") # only for locals
londontmp[, 1] <- ymd(londontmp[, 1], locale = 
                        Sys.getlocale("LC_TIME")) # locale can be skipped

colnames(londontmp) <- c("ds", "y")
londontmp$y <-londontmp$y / 10
summary(londontmp$y)

h <- hist(londontmp$y, xlab = "Degrees Celsius", 
          main = "Heathrow Temperature Readings 1960-2017",
          ylim = c(0, 3000))
xfit <- seq(min(londontmp$y), max(londontmp$y))
yfit <- dnorm(xfit, mean = mean(londontmp$y), sd=
                sd(londontmp$y))
yfit <- yfit * diff(h$mids[1:2]) * length(londontmp$y)
lines(xfit, yfit, col= "#5694f1", lwd = 2)
