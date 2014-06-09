rm(list=ls())

#load packages
library(ggplot2)
library(scales)

#load data
#C:/Users/Markus/Desktop/processed data/
success = read.csv("/zpool1/s10859017/Seminar/processed data/Success.csv")
#success = read.csv("C:/Users/Markus/Desktop/processed data/Success.csv")
fail = read.csv("/zpool1/s10859017/Seminar/processed data/fail.csv")
#fail = read.csv("C:/Users/Markus/Desktop/processed data/fail.csv")

data = rbind(cbind(success, success = 1), cbind(fail, success = 0))

#remove and make subset of data as it is too big (let it run on the server with full data set)
#rm(success)
#rm(fail)
#data = data[c(1:1000, (nrow(data)-1000):nrow(data)), ]

pdf(file="/zpool1/s10859017/plots.pdf")
#timeSinceFirst: nur mit Last=1 => Dauer vom ersten bis letzten
p = ggplot(data=subset(data, data$Last==1), mapping = aes(x = timeSinceFirstDay, fill = factor(success)))

p + geom_bar(aes(y = (..count..)/sum(..count..)), stat = "bin", position = "dodge", binwidth=5) +
  labs(x = "Beobachtungsdauer in Tagen", y = "Relative Haeufigkeit") +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein")) +
  scale_y_continuous(limits = c(0,.05), oob=squish) #squish => beob. mit groesseren werten werden NICHT gelöscht

p + geom_bar(aes(y = (..count..)/sum(..count..)), stat = "bin", position = "dodge", binwidth=1) +
  labs(x = "Beobachtungsdauer in Tagen", y = "Relative Haeufigkeit") + xlim(c(0,50)) +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein")) +
  scale_y_continuous(limits = c(0,.05), oob=squish) #squish => beob. mit groesseren werten werden NICHT gelöscht

#timeSinceLast und timeSinceLast2: Frequenz der Kontaktpunkte
#"-1"-er auf 0 setzten; muss beim preprocessing noch angepasst werden
data$timeSinceLastDay[data$timeSinceLastDay<0]=0
data$timeSinceLast2Day[data$timeSinceLast2Day<0]=0

p = ggplot(data=data, mapping = aes(x = timeSinceLastDay, fill = factor(success)))

p + geom_bar(aes(y = (..count..)/sum(..count..)), stat = "bin", position = "dodge", binwidth = 5) +
  labs(x = "Dauer zwischen Kontaktpunkten in Tagen", y = "Relative Haeufigkeit") +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein")) +
  scale_y_continuous(limits = c(0,.1), oob=squish) 

p + geom_bar(aes(y = (..count..)/sum(..count..)), stat = "bin", position = "dodge", binwidth = 1) +
  labs(x = "Dauer zwischen Kontaktpunkten in Tagen", y = "Relative Haeufigkeit") + xlim(0,50) +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein")) +
  scale_y_continuous(limits = c(0,.1), oob=squish) 

p = ggplot(data=data, mapping = aes(x = timeSinceLast2Day, fill = factor(success)))

p + geom_bar(aes(y = (..count..)/sum(..count..)), stat = "bin", position = "dodge", binwidth = 5) +
  labs(x = "Dauer zwischen drei Kontaktpunkten in Tagen", y = "Relative Haeufigkeit") +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein")) +
  scale_y_continuous(limits = c(0,.1), oob=squish) 

p + geom_bar(aes(y = (..count..)/sum(..count..)), stat = "bin", position = "dodge", binwidth = 1) +
  labs(x = "Dauer zwischen drei Kontaktpunkten in Tagen", y = "Relative Haeufigkeit") + xlim(0,50) +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein")) +
  scale_y_continuous(limits = c(0,.1), oob=squish) 

#hour und weekday
p = ggplot(data=data, mapping = aes(x = hour, fill = factor(success)))
p + geom_bar(aes(y = (..count..)/sum(..count..)), stat = "bin", position = "dodge", binwidth = 1) +
  labs(x = "Uhrzeit", y = "Relative Haeufigkeit") +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein"))

p = ggplot(data=data, mapping = aes(x = weekday, fill = factor(success)))
p + geom_bar(aes(y = (..count..)/sum(..count..)), stat = "bin", position = "dodge", binwidth = 1) +
  labs(x = "Wochentag", y = "Relative Haeufigkeit") +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein"))

#touchPointType
p = ggplot(data=data, mapping = aes(x = factor(touchpointType), fill = factor(success)))
p + geom_bar(aes(y = (..count..)/sum(..count..)), stat = "bin", position = "dodge") +
  labs(x = "Click oder View", y = "Relative Haeufigkeit") +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein"))

#clickCount: nur mit Last=1
maxPosition = cbind(max(subset(data, data$success==0)$Position), max(subset(data, data$success==1)$Position))
clickCountMean = data.frame(position = c(1:maxPosition[1], 1:maxPosition[2]), 
                            success = c(rep(0, maxPosition[1]), rep(1, maxPosition[2])),
                            mean = numeric(sum(maxPosition)))
for(j in 0:1){ #success and fail
  for(i in 1:maxPosition[j+1]){ #for each position
    idx = (j * maxPosition[1]) + i
    clickCountMean$mean[idx] = mean(subset(data, (data$Position==clickCountMean$position[i] & data$success==j))$clickCount)
  }
}
save(clickCountMean, file = "/zpool1/s10859017/clickCountMean.RData")
p = ggplot(data=clickCountMean, mapping = aes(x = position, y = mean, color = factor(success)))
p + geom_line() +
  labs(x = "Position", y = "Haeufigkeit der Clicks im Mittel") +
  scale_color_discrete(name="Konvertiert?", labels=c("ja", "nein")) +
  geom_abline(aes(intercept=0, slope=1))

#hasClicked:
maxPosition = cbind(max(subset(data, data$success==0)$Position), max(subset(data, data$success==1)$Position))
hasClickedProb = data.frame(position = c(1:maxPosition[1], 1:maxPosition[2]), 
                            success = c(rep(0, maxPosition[1]), rep(1, maxPosition[2])),
                            prob = numeric(sum(maxPosition)))
for(j in 0:1){ #success and fail
  for(i in 1:maxPosition[j+1]){ #for each position
    idx = (j * maxPosition[1]) + i
    hasClickedProb$prob[idx] = mean(subset(data, 
            (data$Position==hasClickedProb$position[i] & data$success==j))$hasClicked)
  }
}
save(hasClickedProb, file = "/zpool1/s10859017/hasClickedProb.RData")
p = ggplot(data=hasClickedProb, mapping = aes(x = position, y = prob, color = factor(success)))
p + geom_line() +
  labs(x = "Position", y = "Anteil mit hasClicked=1") +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein"))

#plots with First=1 only
p = ggplot(data=subset(data, data$First==1), mapping = aes(x = factor(touchpointType), fill = factor(success)))
p + geom_bar(aes(y = (..count..)/sum(..count..)), stat = "bin", position = "dodge") +
  labs(x = "Click oder View (erster Kontakt)", y = "Relative Haeufigkeit") +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein"))

#plots with Last=1 only
p = ggplot(data=subset(data, data$Last==1), mapping = aes(x = factor(touchpointType), fill = factor(success)))
p + geom_bar(aes(y = (..count..)/sum(..count..)), stat = "bin", position = "dodge") +
  labs(x = "Click oder View (letzter Kontakt)", y = "Relative Haeufigkeit") +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein"))

#funnel length
p = ggplot(data=subset(data, data$Last==1), mapping = aes(x = funnelLength, fill = factor(success)))
p + geom_bar(aes(y = (..count..)/sum(..count..)), stat = "bin", position = "dodge", binwidth = 5) +
  labs(x = "Funnel Length", y = "Relative Haeufigkeit") +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein"))

p + geom_bar(aes(y = (..count..)/sum(..count..)), stat = "bin", position = "dodge", binwidth=1) +
  labs(x = "Funnel Length", y = "Relative Haeufigkeit") + xlim(0,50) +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein")) +
  scale_y_continuous(limits = c(0,.1), oob=squish)

p + geom_bar(aes(y = (..count..)/sum(..count..)), stat = "bin", position = "dodge", binwidth=1) +
  labs(x = "Funnel Length", y = "Relative Haeufigkeit") + xlim(0,50) +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein")) +
  scale_y_continuous(limits = c(0,.05), oob=squish)

dev.off()
#projectType fehlt noch