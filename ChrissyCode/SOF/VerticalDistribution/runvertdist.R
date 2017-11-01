# ## Straits of Florida Vertical Distributions
# Re-doing the vertical distribution work that I did previously in Access
# In Progress Sept 16, 2017
# New File Nov 1, 2017 with function

setwd('/Users/chrissy/JointProgram/SOF/VerticalDistribution/')

# source the necessary functions
source('/Users/chrissy/JointProgram/Computing/LlopizLab/ChrissyCode/SOF/VerticalDistribution/vertdistFN.R')

# Read in Net table:
nets<- read.csv('/Users/chrissy/JointProgram/SOF/SOFdataTablesForChrissy/NetTable.csv')
nets$Sample.<- toupper(nets$Sample.)
# Edit the neuston rows so that they have non-zero abundance
I<- which(nets$TargetMax==0)
nets$Pmax[I]<- 0.5

# Load lengths from 2013 work:
load('/Users/chrissy/JointProgram/SOF/PC_files/LengthBins.rda')
lengthdata<- length.bins
lengthdata$Sample.<- toupper(as.character(lengthdata$Sample.))

