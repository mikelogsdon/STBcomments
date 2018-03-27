library(lubridate)
library(ggplot2)
library(plyr)
load("allSTB.rda")

allComments$timestamp <- with_tz(allComments$timestamp, tzone = "America/Los_Angeles")
allComments$date <- as.Date(allComments$timestamp)
allComments$hour <- hour(allComments$timestamp) + 0.5
allComments$year <- year(allComments$timestamp)


byHour2 <- aggregate(text ~ hour + date, data = allComments, FUN = length)
byHour3 <- ddply(byHour2, .(hour), function(x) {
  data.frame(
    "ave" = mean(x$text),
    "se" = sd(x$text) / sqrt(nrow(x))
  )
})
byHour3$ymin <- byHour3$ave - 2 * byHour3$se
byHour3$ymax <- byHour3$ave + 2 * byHour3$se
hourBreaks <- seq(0, 24, 2)
hourLabels <- c("Midnight", "2AM", "4AM", "6AM", "8AM", "10AM", "Noon", "2PM", "4PM", "6PM", "8PM", "10PM", "Midnight")
p <- ggplot(byHour3) + theme_bw() +
  geom_point(aes(hour, ave)) +
  geom_errorbar(aes(x = hour, ymin = ymin, ymax = ymax)) +
  scale_x_continuous(breaks = hourBreaks, labels = hourLabels) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Hour of Day") + ylab("Comments per Day") +
  ggtitle("Seattle Transit Blog Comment Timestamps")
# p

