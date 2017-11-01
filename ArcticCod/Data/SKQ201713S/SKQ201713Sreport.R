## Process some data for SKQ201713S Cruise Report:

setwd('/Users/chrissy/JointProgram/ArcticCod/SKQ201713S/')

# read in the MW Trawl station info
MWstations<- read.csv('mwtrawl_stations.csv', as.is=1:16)
cod<- read.csv('mwtrawl_ArcticCod.csv')

# total catch:
names(MWstations)[14]<- 'TotalCatchCod'
MWstations$TotalCatchCod
MWstations$TotalCatchCod[1]<- NA
MWstations$TotalCatchCod<- as.numeric(MWstations$TotalCatchCod)
sum(MWstations$TotalCatchCod, na.rm=TRUE)

# Minimum/maximum size caught:
min(cod$FL_mm)
max(cod$FL_mm)
hist(cod$FL_mm, xlab='Fork Length (mm)', main=NA)

# Numbers of stomachs, etc:
sum(cod$Gut_Bagged.=='y')+sum(cod$Gut_Bagged.=='Y')
sum(cod$Liver_Bagged.=='y')+sum(cod$Liver_Bagged.=='Y')
sum(cod$Musc_Bagged.=='y')+sum(cod$Musc_Bagged.=='Y')
sum(cod$Head_Bagged.=='y')+sum(cod$Head_Bagged.=='Y')
sum(cod$Frozen_Whole.=='y')+sum(cod$Frozen_Whole.=='Y')
sum(cod$RNAlater.=='y')+sum(cod$RNAlater.=='Y')


# Fish from Plankton Samples:
tucker<- read.csv('planktontow_fish.csv')
head(tucker)
sum(tucker$No_Fish[tucker$Gear=='Tucker'])
sum(tucker$No_Fish[tucker$Gear=='Ring'])
sum(tucker$No_Fish[tucker$Gear=='Bongo'])
sum(tucker$No_Fish)
