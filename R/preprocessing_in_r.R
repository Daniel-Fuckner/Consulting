rm(list=ls())

###load packages
#library(plyr)
#library(dplyr)
#vignette("datatable-intro")
library(data.table)

###support function
source("C:/Users/Markus/Desktop/processed data/support_functions/createFeatures.R")
source("C:/Users/Markus/Desktop/processed data/support_functions/createPosition.R")

system.time(succ <- fread(input="C:/Users/Markus/Desktop/processed data/succ_sql.csv",stringsAsFactors=FALSE,na.strings="\\N",sep=","))
#system.time(succ <- fread(input="C:/Users/Markus/Desktop/processed data/fail_sql.csv",stringsAsFactors=FALSE,na.strings="\\N",sep=","))

system.time(newFeat <- succ[, createFeatures(clickTimestamp,touchpointType), by="id"])
newFeat$vid <- c(1:length(newFeat$id))
succ$vid <- c(1:length(newFeat$id))

setkey(newFeat,vid)
setkey(succ,vid)
succ <- merge(succ, newFeat, by="vid", suffixes = c("", ".y"))

#system.time(succ <- fread(input="C:/Users/Markus/Desktop/processed data/succ.csv",stringsAsFactors=FALSE,na.strings="\\N",sep=","))

#ueberfluessige spalten loeschen
succ[,c("V1","vid","isLast","isTransaction","id.y","clickTimestamp"):=NULL]

#setnames(succ, "id.x", "id")

nrow(subset(succ, funnelLength>250)) #71423 touchpoints mit Laenge groesser 250
length(unique(subset(succ, funnelLength>250)$id)) #189 success funnels mit Laenge groesser 250
length(unique(subset(succ, funnelLength>300)$id)) #106
length(unique(subset(succ, funnelLength>1000)$id)) #4

succ = subset(succ, funnelLength<=250) #nach 250 abschneiden

nrow(subset(succ, touchpointType == 1)) #281689 views
succClicks = subset(succ, touchpointType == 2) #data table, dass nur clicks enthält

#################Frage: war 1 view und 2 click oder umgekehrt?
#################das mit den views loeschen, duerfen wir eig erst machen, nachdem wir mit den campaigns gemerged haben

#write.csv(succ,file = "C:/Users/Markus/Desktop/processed data/succ.csv")
#write.csv(succClicks,file = "C:/Users/Markus/Desktop/processed data/succClicks.csv")

system.time(succCampaign <- fread(input="C:/Users/Markus/Desktop/processed data/campaign_succ.csv",stringsAsFactors=FALSE,na.strings="\\N",sep=",",header=F))

names(succCampaign) = c(namesC, "creationtime")
succCampaign[,c("projectId", "projectIdFunnelKeyword"):=NULL]
setnames(succCampaign, "succId", "ID")
succCampaign$ID = as.integer(succCampaign$ID)
succCampaign$creationtime = as.integer(succCampaign$creationtime)
system.time(pos <- succCampaign[, createPosition(creationtime), by="ID"])

pos$vid <- c(1:length(pos$ID))
succCampaign$vid <- c(1:length(pos$ID))

setkey(pos,vid)
setkey(succCampaign,vid)
succCampaign <- merge(succCampaign, pos, by="vid")
setnames(succCampaign, "ID.x", "ID")
 
new = merge(succ, succCampaign, by=c("ID","Position"))


















