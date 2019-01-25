setwd('/Users/helena.mcmonagle/Desktop/Llopiz Lab/Arctic project')
##read in data from post larvae and event log
postlarvae<- read.csv('planktontow_fish.csv', header=T)
## header tells R to call first row the column names
eventlog<- read.csv('R2R_ELOG_skq201713s_FINAL_EVENTLOG_20170918_143412-1.csv', header=T)
# pull out rows from bongo and tucker trawls. Use only recovery location for lat and long
# might change to average of deployment and recovery for final map, but this is just to choose stations
#make dummy variable "I" (instrument), which is the indices of event log that is either a bongo or tucker
# levels(eventlog$Instrument) tells you how you wrote out tucker and bongo in the csv file
I <- which(eventlog$Instrument %in% c("Bongo Nets", "Tucker Trawl"))
I
# [] shows you row numbers you want to pull out of larger dataset
# levels(eventlog$Action) # tells you (don't need to have this in final script ,just to set up script)
# levels line tells you what the lat/long column name for recovery is called
# make data frame with all rows from bongo and tucker
# rows, then columns in []. Rows I, columns all. 
bongotucker<- eventlog[I,]
I <- which(bongotucker$Action=='recover')
I # to view
bt_recovery<-bongotucker[I,]
#now choose just the stations of interest: 75, 80, 82, 138, 140, 142, 144
I<- which(bt_recovery$Station %in% c(75, 80, 82, 138, 140, 142, 144))
# when read to plot, can write "to plot"
toplot<- bt_recovery[I,]
# View() opens as a separate tab instead of showing in the console
View(toplot)
# most stations have a tucker and bongo, so they'll sometimes overlap in the map plot
# now mapping. need libraries below
install.packages('oce') 
##can delete this second time running the script. when installing, had to say "no" for the complication question that pops up partway through install
install.packages('ocedata')
install.packages("ncdf4")
library(oce)
library(ocedata)
library(ncdf4)
#now load the data, fine scale coastline
data(coastlineWorldFine)
# read in bathymetry data
# now: ncid compresses files. But you don't read it in that format, read in R 
# tell R where the file is. Gives R an ID number, a handle for that file
ncid<- nc_open("RN-7787_1532350309466/GEBCO_2014_2D.nc")
print(ncid)
# to plot, need lat long and contours
# after print ncid, scroll to see name of elevation. The short name is elevation
# pull out lat long that correspond
bathylat<- ncvar_get(ncid, varid='lat')
bathylon<- ncvar_get(ncid, varid='lon')
bathy<- ncvar_get(ncid, varid='elevation')
# now I have the whole world's bathymetry
# need to narrow it down, or else too big to load
I<- which(bathylat>70 & bathylat<73)
J<- which(bathylon<(-148) & bathylon>(-155))
bathylat2<- bathylat[I]
bathylon2<- bathylon[J]
bathy2<- bathy[J,I]
# now close so R knows not to keep using
nc_close(ncid) 

# can plot in a separate viewer, instead of in the plots window, if you want the plot to look consistent each time.
# as is, plot view is dependent on size of plot window to the left. OK for now. 
# to plot contours,
contour(bathylon2, bathylat, bathy,
        levels = c(-10, -20, -50, -100, -200, -500, -1000),
       drawlabels = T, add=TRUE, lwd=0.75, col='black')
points(toplot$Longitude, toplot$Latitude, pch=18)
I<- which(as.numeric(as.character(toplot$Station))<100)
text(toplot$Longitude[I], toplot$Latitude[I], toplot$Station[I], pos=4)
text(toplot$Longitude[-I], toplot$Latitude[-I], toplot$Station[-I], pos=2)
# to look up details on plotting, 
?text
?points
# above: points can change the way the 
# decided to do stations 75, 144, 82, 138, and 140
# to compare between two time periods, in shore and offshore, in same location. Also chose 140 because it's in between and had a lot of fish.
