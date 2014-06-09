rm(list=ls())

#load packages
library(ggplot2)

#load data
success = read.csv("/zpool1/s10859017/Seminar/processed data/Success.csv")
fail = read.csv("/zpool1/s10859017/Seminar/processed data/fail.csv")

data = rbind(cbind(success, success = 1), cbind(fail, success = 0))

#remove and make subset of data as it is too big (let it run on the server with full data set)
#rm(success)
#rm(fail)
#data = data[c(1:1000, (nrow(data)-1000):nrow(data)), ]

pdf(file="/zpool1/s10859017/plots.pdf")
#timeSinceFirst: nur mit Last=1 => Dauer vom ersten bis letzten
p = ggplot(data=subset(data, data$Last==1), mapping = aes(x = timeSinceFirstDay, fill = factor(success)))
p + geom_bar(mapping = NULL, data = NULL, stat = "bin", position = "dodge") +
  labs(x = "Beobachtungsdauer", y = "Haeufigkeit") +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein"))

#timeSinceLast und timeSinceLast2: Frequenz der Kontaktpunkte
p = ggplot(data=data, mapping = aes(x = timeSinceLastDay, fill = factor(success)))
p + geom_bar(mapping = NULL, data = NULL, stat = "bin", position = "dodge") +
  labs(x = "Dauer zwischen Kontaktpunkten", y = "Haeufigkeit") +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein"))

p = ggplot(data=data, mapping = aes(x = timeSinceLast2Day, fill = factor(success)))
p + geom_bar(mapping = NULL, data = NULL, stat = "bin", position = "dodge") +
  labs(x = "Dauer zwischen Kontaktpunkten (2)", y = "Haeufigkeit") +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein"))

#hour und weekday
p = ggplot(data=data, mapping = aes(x = hour, fill = factor(success)))
p + geom_bar(mapping = NULL, data = NULL, stat = "bin", position = "dodge", binwidth = 1) +
  labs(x = "Uhrzeit", y = "Haeufigkeit") +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein"))

p = ggplot(data=data, mapping = aes(x = weekday, fill = factor(success)))
p + geom_bar(mapping = NULL, data = NULL, stat = "bin", position = "dodge", binwidth = 1) +
  labs(x = "Wochentag", y = "Haeufigkeit") +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein"))

#touchPointType
p = ggplot(data=data, mapping = aes(x = factor(touchpointType), fill = factor(success)))
p + geom_bar(mapping = NULL, data = NULL, stat = "bin", position = "dodge") +
  labs(x = "Click oder View", y = "Haeufigkeit") +
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
p = ggplot(data=clickCountMean, mapping = aes(x = position, y = mean, shape = factor(success)))
p + geom_point() +
  labs(x = "Anteil an Clicks", y = "Haeufigkeit") +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein"))

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
p = ggplot(data=hasClickedProb, mapping = aes(x = position, y = prob, shape = factor(success)))
p + geom_point() +
  labs(x = "Anteil an Clicks", y = "Haeufigkeit") +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein"))

#plots with First=1 only
p = ggplot(data=subset(data, data$First==1), mapping = aes(x = factor(touchpointType), fill = factor(success)))
p + geom_bar(mapping = NULL, data = NULL, stat = "bin", position = "dodge") +
  labs(x = "Click oder View (erster Kontakt)", y = "Haeufigkeit") +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein"))

#plots with Last=1 only
p = ggplot(data=subset(data, data$Last==1), mapping = aes(x = factor(touchpointType), fill = factor(success)))
p + geom_bar(mapping = NULL, data = NULL, stat = "bin", position = "dodge") +
  labs(x = "Click oder View (letzter Kontakt)", y = "Haeufigkeit") +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein"))

#funnel length
p = ggplot(data=subset(data, data$Last==1), mapping = aes(x = factor(funnelLength), fill = factor(success)))
p + geom_bar(mapping = NULL, data = NULL, stat = "bin", position = "dodge") +
  labs(x = "Funnel Length", y = "Haeufigkeit") +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein"))
dev.off()
#projectType fehlt noch