## Straits of Florida Vertical Distributions
# Re-doing the vertical distribution work that I did previously in Access
# In Progress Sept 16, 2017

setwd("/Users/chrissy/JointProgram/SOF/VerticalDistribution//")

## Step 1: Proportions at depth with length:

# Read in Net table:
nets<- read.csv('/Users/chrissy/JointProgram/SOF/SOFdataTablesForChrissy/NetTable.csv')
nets$Sample.<- toupper(nets$Sample.)
# Edit the neuston rows so that they have non-zero abundance
I<- which(nets$TargetMax==0)
nets$Pmax[I]<- 0.5
nets$DepthBin<- NA
I<- which(nets$TargetMax==0 | nets$TargetMax==25)
nets$DepthBin[I]<- '0-25'
I<- which(nets$TargetMax==50)
nets$DepthBin[I]<- '25-50'
I<- which(nets$TargetMax==75)
nets$DepthBin[I]<- '50-75'
I<- which(nets$TargetMax==100)
nets$DepthBin[I]<- '75-100'

# Load lengths from 2013 work:
load('/Users/chrissy/JointProgram/SOF/PC_files/LengthBins.rda')
lengthdata<- length.bins
lengthdata$Sample.<- toupper(as.character(lengthdata$Sample.))

mullidae<- lengthdata[lengthdata$FamilyName=='Mullidae', c('Sample.', 'Lengthmm')]
lengthrange<- c(min(mullidae$Lengthmm), max(mullidae$Lengthmm))

# do a prop at depth for thalassoma:
# not enough length data for Thalassoma in the length.bins dataframe
thalassoma<- lengthdata[lengthdata$GenusName=='Thalassoma', c('Sample.', 'Lengthmm')]
thalassoma2<- read.csv('Thalassoma_length_dietpaper.csv', as.is=1)
thalassoma2$Sample.<-toupper(thalassoma2$Sample.)
thalassoma3<- read.csv('Thalassoma_lenth_growthpaper.csv', as.is=8)
thalassoma3$Sample.<-toupper(thalassoma3$Sample.)
th4<- merge(thalassoma2, thalassoma3[,c('Sample.', 'Individual.', 'Length')], 
            by=c('Sample.', 'Individual.'), all.x=TRUE, all.y=TRUE)
th4$Lengthmm<- NA
# When the individuals are in both the growth and the diet study, use the lengths from growth 
# study (Length.y). If they are only in one or the other, use all of them.
I<- which(!is.na(th4$Length.x) & !is.na(th4$Length.y))
th4$Lengthmm[I]<- th4$Length.y[I]
I<- which(is.na(th4$Length.x) & !is.na(th4$Length.y))
th4$Lengthmm[I]<- th4$Length.y[I]
I<- which(!is.na(th4$Length.x) & is.na(th4$Length.y))
th4$Lengthmm[I]<- th4$Length.x[I]

# get the columns I need from th4 and nets (sample ID, lengths, depth bins, volume filtered, min and max depth)
th4<- th4[,c('Sample.', 'Lengthmm')]
th4<- merge(th4, nets[,c('Sample.', 'TargetMax', 'vol', 'Pmin', 'Pmax', 'DepthBin')])

# Calculate size classes:
minth<- min(th4$Lengthmm)
maxth<- max(th4$Lengthmm)
rangeth<- maxth-minth
SC<- c(minth-0.1, minth+0.25*rangeth, minth+0.5*rangeth, minth+0.75*rangeth, maxth)
th4$sizeclass<- NA
# Count how many in each size class along the way:
thalcount<- data.frame(sizeclass=1:4, count=NA)
for (i in 1:4){
  j<- which(th4$Lengthmm>SC[i] & th4$Lengthmm<=SC[i+1])
  th4$sizeclass[j]<- i
  thalcount$count[i]<- length(j)
}



# Calculate abundances for size classes for each sample:
thabund<- aggregate(Lengthmm~Sample.+sizeclass, data=th4, FUN=length)
names(thabund)[3]<-'Nindiv'
thabund<- merge(thabund, th4[,-2])
thabund$abundance<- thabund$Nindiv/thabund$vol*(thabund$Pmax-thabund$Pmin)

# Sum the abundances for size class and depth bin:
thabund2<- aggregate(abundance~sizeclass+DepthBin, data=thabund, FUN=sum)

# Sum the abundances for size class (demonimator for prop at depth):
thabund3<- aggregate(abundance~sizeclass, data=thabund2, FUN=sum)
names(thabund3)[2]<- 'Denominator'

# Calculate proportion at depth
thabund2<- merge(thabund2, thabund3, all.x=TRUE)
thabund2$proportion<- thabund2$abundance/thabund2$Denominator

# Plot it!
# add the missing rows:
newrows<- data.frame(sizeclass=c(1,4), DepthBin=c('75-100', '0-25'), abundance=c(NA,NA),
                     Denominator=c(NA, NA), proportion=c(0,0))
thabund2<- rbind(thabund2, newrows)
# make DepthBin a factor so you can order them
thabund2$DepthBin<- as.factor(thabund2$DepthBin)

png("Thalassoma_SC3.9mm_20171030.png", width=600, height=250)
par(mfrow=c(1,4))
for (i in 1:4){
  toplot<- thabund2[thabund2$sizeclass==i,]
  toplot<-toplot[order(toplot$DepthBin, decreasing=TRUE),]
  barplot(toplot$proportion, names.arg=toplot$DepthBin, horiz=TRUE)
}
dev.off()


## Proportions at depth with length for Pomacentridae
poma<- lengthdata[lengthdata$FamilyName=='Pomacentridae',c('Sample.', 'Lengthmm')]
poma$Sample.<- toupper(poma$Sample.)

# get the columns I need from nets (depth bins, volume filtered, min and max depth)
poma<- merge(poma, nets[,c('Sample.', 'TargetMax', 'vol', 'Pmin', 'Pmax', 'DepthBin')])

# Calculate size classes:
minth<- min(poma$Lengthmm)
maxth<- max(poma$Lengthmm)
rangeth<- maxth-minth
SC<- c(minth-0.1, minth+0.25*rangeth, minth+0.5*rangeth, minth+0.75*rangeth, maxth)
# Those are not useful size classes (see the histogram if desired), so re-do:
maxth<- SC[3]
rangeth<- maxth-minth
SC<- c(minth-0.1, minth+0.25*rangeth, minth+0.5*rangeth, minth+0.75*rangeth, maxth)
# calculate counts in each size class along the way
pomacount<- data.frame(sizeclass=1:4, count=NA)
poma$sizeclass<- NA
for (i in 1:4){
  j<- which(poma$Lengthmm>SC[i] & poma$Lengthmm<=SC[i+1])
  poma$sizeclass[j]<- i
  pomacount$count[i]<- length(j)
}

hist(poma$sizeclass, breaks=c(0.5, 1.5, 2.5, 3.5, 4.5), main="Pomacentridae", xlab='Size Classes')

# Calculate abundances for size classes for each sample:
pomaabund<- aggregate(Lengthmm~Sample.+sizeclass, data=poma, FUN=length)
names(pomaabund)[3]<-'Nindiv'
pomaabund<- merge(pomaabund, poma[,-2])
pomaabund$abundance<- pomaabund$Nindiv/pomaabund$vol*(pomaabund$Pmax-pomaabund$Pmin)

# Sum the abundances for size class and depth bin:
pomaabund2<- aggregate(abundance~sizeclass+DepthBin, data=pomaabund, FUN=sum)

# Sum the abundances for size class (demonimator for prop at depth):
pomaabund3<- aggregate(abundance~sizeclass, data=pomaabund2, FUN=sum)
names(pomaabund3)[2]<- 'Denominator'

# Calculate proportion at depth
pomaabund2<- merge(pomaabund2, pomaabund3, all.x=TRUE)
pomaabund2$proportion<- pomaabund2$abundance/pomaabund2$Denominator

# Plot it!
# add the missing rows:
newrows<- data.frame(sizeclass=c(4,4), DepthBin=c('75-100', '0-25'), abundance=c(NA,NA),
                     Denominator=c(NA, NA), proportion=c(0,0))
pomaabund2<- rbind(pomaabund2, newrows)
# make DepthBin a factor so you can order them
pomaabund2$DepthBin<- as.factor(pomaabund2$DepthBin)

png('Pomacentridae_SC4.8.85mm_20171030.png', height=250, width=600)
par(mfrow=c(1,4))
for (i in 1:4){
  toplot<- pomaabund2[pomaabund2$sizeclass==i,]
  toplot<-toplot[order(toplot$DepthBin, decreasing=TRUE),]
  barplot(toplot$proportion, names.arg=toplot$DepthBin, horiz=TRUE)
}
dev.off()

## Mullidae

# get the columns I need from lengthdata and nets (sample ID, lengths, depth bins, volume filtered, min and max depth)
mullid<- lengthdata[lengthdata$FamilyName=='Mullidae', c('Sample.', 'Lengthmm')]
mullid<- merge(mullid, nets[,c('Sample.', 'TargetMax', 'vol', 'Pmin', 'Pmax', 'DepthBin')])

# Calculate size classes:
minth<- min(mullid$Lengthmm)
maxth<- max(mullid$Lengthmm)
rangeth<- maxth-minth
SC<- c(minth-0.1, minth+0.25*rangeth, minth+0.5*rangeth, minth+0.75*rangeth, maxth)
mullid$sizeclass<- NA
# Drop the last 2 size classes:
maxth<- SC[3]
rangeth<- maxth-minth
SC<- c(minth-0.1, minth+0.25*rangeth, minth+0.5*rangeth, minth+0.75*rangeth, maxth)
# Count how many in each size class along the way:
mullcount<- data.frame(sizeclass=1:4, count=NA)
for (i in 1:4){
  j<- which(mullid$Lengthmm>SC[i] & mullid$Lengthmm<=SC[i+1])
  mullid$sizeclass[j]<- i
  mullcount$count[i]<- length(j)
}

# Calculate abundances for size classes for each sample:
mullabund<- aggregate(Lengthmm~Sample.+sizeclass, data=mullid, FUN=length)
names(mullabund)[3]<-'Nindiv'
mullabund<- merge(mullabund, mullid[,-2])
mullabund$abundance<- mullabund$Nindiv/mullabund$vol*(mullabund$Pmax-mullabund$Pmin)

# Sum the abundances for size class and depth bin:
mullabund2<- aggregate(abundance~sizeclass+DepthBin, data=mullabund, FUN=sum)

# Sum the abundances for size class (demonimator for prop at depth):
mullabund3<- aggregate(abundance~sizeclass, data=mullabund2, FUN=sum)
names(mullabund3)[2]<- 'Denominator'

# Calculate proportion at depth
mullabund2<- merge(mullabund2, mullabund3, all.x=TRUE)
mullabund2$proportion<- mullabund2$abundance/mullabund2$Denominator

# make DepthBin a factor so you can order them
mullabund2$DepthBin<- as.factor(mullabund2$DepthBin)

# Plot it!
# add the missing rows:
newrows<- data.frame(sizeclass=c(1,2,4), DepthBin=c('75-100', '50-75', '50-75'), 
                     abundance=rep(NA,3), Denominator=rep(NA,3), proportion=rep(0,3))
mullabund2<- rbind(mullabund2, newrows)

png('Mullidae_SC4.13mm_20171030.png', width=600, height=250)
par(mfrow=c(1,4))
for (i in 1:4){
  toplot<- mullabund2[mullabund2$sizeclass==i,]
  toplot<-toplot[order(toplot$DepthBin, decreasing=TRUE),]
  barplot(toplot$proportion, names.arg=toplot$DepthBin, horiz=TRUE)
}
dev.off()
