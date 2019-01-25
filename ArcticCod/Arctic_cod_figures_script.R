# Try plot with only sagittae, or only lapilli, and examine any correlations between
# otolith size and standard length, R/D and station, R/D and standard length, R/D and Fulton's K, otolith asymmetry and R/D, and R/D and date of measurement
library("dplyr")
library("ggpubr")
all_sagitta_data <- read.csv("/Users/helena.mcmonagle/Desktop/Llopiz Lab/R/RNA_DNA_Ratios/data/all_shape_r_sagitta_data.csv")
View(all_sagitta_data)
all_lapillus_data <- read.csv("/Users/helena.mcmonagle/Desktop/Llopiz Lab/R/RNA_DNA_Ratios/data/all_shape_r_lapillus_data.csv")
View(all_lapillus_data)

# To subset and use only data from left lapillus, 
# replace "all_lapillus_data" with LL_only
# LL = left lapilli, LS = left sagittae
LL_only <- all_lapillus_data[all_lapillus_data$Otolith == "LL" ,]
View(LL_only)
# Do the same for sagitta data, where sagitta data will be used (if at all in final manuscript)
LS_only <- all_sagitta_data[all_sagitta_data$Otolith == "LS" ,]

# Does standard length correlate with otolith area? 
plot(x=LS_only$length_cm, y=LS_only$otolith.area, xlab = 'Standard Length', ylab = 'Otolith Area', main="Body length and otolith area", xlim = c(1.5, 5), ylim=c(0,1), col='blue')
par(new=TRUE) # add lapilli datapoint to same plot
plot(x=LL_only$length_cm, y=LL_only$otolith.area, xlab = 'Standard Length', ylab = 'Otolith Area', main="Body length and otolith area", xlim = c(1.5, 5), ylim=c(0,1), col='red')
legend(x=1.7, y=0.9, legend=c('Sagittae', 'Lapilli'), col=c('blue', 'red'), pch=c(1,1))
abline(lm(LL_only$otolith.area ~ 1 + LL_only$length_cm, data=LL_only))
abline(lm(LS_only$otolith.area ~ 1 + LS_only$length_cm, data=LS_only))
View(LS_only)

cor(LS_only$length_cm, LS_only$otolith.area) # = 0.924
cor(LL_only$length_cm, LL_only$otolith.area) # = 0.922


# Does ratio correlate with standard length?
plot(x=LL_only$length_cm, y=LL_only$Ratio, xlab = 'Standard Length', ylab = 'RNA DNA Ratio', main="RNA DNA ratio and standard length", col='blue')
cor(LL_only$length_cm, LL_only$Ratio)# = 0.323258
abline(lm(LL_only$Ratio ~ 1 + LL_only$length_cm, data=LL_only))
# There appears to be a trend that ratio slightly increases with increasing standard length.  

# Calculate residuals. 
ratio.lm = lm(Ratio ~ length_cm, data=LL_only) 
ratio.res = resid(ratio.lm)

# plot the residual against the observed values of standard length ('length_cm')
plot(LL_only$length_cm, ratio.res, 
  ylab="Residuals", xlab="Standard Length", 
  main="Residuals of standard length and ratio")
abline(0, 0)                  # the horizon

# standardize the residuals
ratio.lm = lm(Ratio ~ length_cm, data=LL_only) 
ratio.stdres = rstandard(ratio.lm)
# Plot the standardized residual against the observed values of the variable "length_cm"

plot(LL_only$length_cm, ratio.stdres, 
  ylab="Standardized Residuals", 
  xlab="Length_cm", 
  main="Standardized residuals of standard length and ratio") 
abline(0, 0)                  # the horizon

# residuals plot is fairly symmetrically distributed with no extremely high residuals
# Still need to re-do ANOVA of ratio by station, correcting for residuals?

# Does ratio correlate with Fulton's K?
plot(x=LL_only$Fulton, y=LL_only$Ratio, xlab = 'Fultons K Index', ylab = 'RNA DNA Ratio', main="RNA DNA ratio and Fultons K", xlim = c(0, 1.0), ylim=c(0,6),col='blue')
cor(LL_only$Fulton, LL_only$Ratio)# = 0.0003789265... no correlation
abline(lm(LL_only$Ratio ~ 1 + LL_only$Fulton, data=LL_only))
# Fulton's K index tells you condition as an average over entire lifetime. 
# Ratio does not appear to correlate with ratio. 

# Does ratio correlate with asymmetry? Could try the following as simple asymmetry measures:
# absolute value of (left sagitta area - right sagitta area)
# absolute value of (left lapillus area - right lapillus area)
# absolute value of (left sagitta width - right sagitta width) *R and L sides were different
# absolute value of (left sagitta length - right sagitta length) 
# Will do more advanced asymmetry measurements using Justin's method. 

#re-shape dataframe and create a subset for finding differences between L and R otoliths

#subset only rows with LS or RS
LS_data=all_sagitta_data[all_sagitta_data$Otolith=='LS',]
View(LS_data)
RS_data=all_sagitta_data[all_sagitta_data$Otolith=='RS',]
View(RS_data)
LS_otolith_data=LS_data[,c('Fish_ID','Station','Otolith','Ratio','otolith.area','otolith.length','otolith.width','otolith.perimeter','Mass','Fulton')]
RS_otolith_data=RS_data[,c('Fish_ID','Otolith','otolith.area','otolith.length','otolith.width','otolith.perimeter')]
names(LS_otolith_data)[3]='left_sagitta'
names(LS_otolith_data)[5]='LS_area'
names(LS_otolith_data)[6]='LS_length'
names(LS_otolith_data)[7]='LS_width'
names(LS_otolith_data)[8]='LS_perimeter'
View(LS_otolith_data)

names(RS_otolith_data)[2]='right_sagitta'
names(RS_otolith_data)[3]='RS_area'
names(RS_otolith_data)[4]='RS_length'
names(RS_otolith_data)[5]='RS_width'
names(RS_otolith_data)[6]='RS_perimeter'

diff_btw_LS_and_RS=merge(LS_otolith_data,RS_otolith_data,by="Fish_ID")
diff_btw_LS_and_RS$sagitta_area_diff=abs(diff_btw_LS_and_RS$LS_area-diff_btw_LS_and_RS$RS_area)

# now, make plot of sagitta area differences (between L and R) and ratio:
plot(x=diff_btw_LS_and_RS$sagitta_area_diff, y=diff_btw_LS_and_RS$Ratio, xlab = 'Difference in L and R sagitta area', ylab = 'Ratio', main="Sagitta area asymmetry and ratio",col='blue')
cor(diff_btw_LS_and_RS$sagitta_area_diff, diff_btw_LS_and_RS$Ratio)# = 0.266 ...is this significant?
abline(lm(diff_btw_LS_and_RS$Ratio ~ 1 + diff_btw_LS_and_RS$sagitta_area_diff, data=diff_btw_LS_and_RS))

# make same plot but with Fulton's K as a proxy for condition instead of ratio
plot(x=diff_btw_LS_and_RS$sagitta_area_diff, y=diff_btw_LS_and_RS$Fulton, xlab = 'Difference in L and R sagitta area', ylab = 'Fultons K', main="Sagitta area asymmetry and Fultons K",col='blue')
cor(diff_btw_LS_and_RS$sagitta_area_diff, diff_btw_LS_and_RS$Fulton)# = 0.0673...is this significant?
abline(lm(diff_btw_LS_and_RS$Fulton ~ 1 + diff_btw_LS_and_RS$sagitta_area_diff, data=diff_btw_LS_and_RS))
View(diff_btw_LS_and_RS)

# Plotted sagitta area differences against ratio and Fulton's K. Now try 
# sagitta area difference and sagitta width difference against these condition proxies. 

# make column for sagitta_length_diff
diff_btw_LS_and_RS$sagitta_length_diff=abs(diff_btw_LS_and_RS$LS_length-diff_btw_LS_and_RS$RS_length)

# plot sagitta length diff and ratio
plot(x=diff_btw_LS_and_RS$sagitta_length_diff, y=diff_btw_LS_and_RS$Ratio, xlab = 'Difference in L and R sagitta length', ylab = 'Ratio', main="Sagitta length asymmetry and ratio",col='blue')
cor(diff_btw_LS_and_RS$sagitta_length_diff, diff_btw_LS_and_RS$Ratio)# = .279...is this significant?
abline(lm(diff_btw_LS_and_RS$Ratio ~ 1 + diff_btw_LS_and_RS$sagitta_length_diff, data=diff_btw_LS_and_RS))

# plot sagitta length diff and Fulton's
plot(x=diff_btw_LS_and_RS$sagitta_length_diff, y=diff_btw_LS_and_RS$Fulton, xlab = 'Difference in L and R sagitta length', ylab = 'Fultons K', main="Sagitta length asymmetry and Fultons K",col='blue')
cor(diff_btw_LS_and_RS$sagitta_length_diff, diff_btw_LS_and_RS$Fulton)# = -0.0649...is this significant?
abline(lm(diff_btw_LS_and_RS$Fulton ~ 1 + diff_btw_LS_and_RS$sagitta_length_diff, data=diff_btw_LS_and_RS))

# So far, no clear negative correlations between asymmetry (measured as difference in L and R)
# and Fulton's K or Ratio. The only plot with a negative correlation so far is sagitta_length_and_Fultons

# make column for sagitta_width_diff
diff_btw_LS_and_RS$sagitta_width_diff=abs(diff_btw_LS_and_RS$LS_width-diff_btw_LS_and_RS$RS_width)
View(diff_btw_LS_and_RS)

# plot sagitta width diff and ratio
plot(x=diff_btw_LS_and_RS$sagitta_width_diff, y=diff_btw_LS_and_RS$Ratio, xlab = 'Difference in L and R sagitta width', ylab = 'Ratio', main="Sagitta width asymmetry and ratio",col='blue')
cor(diff_btw_LS_and_RS$sagitta_width_diff, diff_btw_LS_and_RS$Ratio)# = -0.141...is this significant?
abline(lm(diff_btw_LS_and_RS$Ratio ~ 1 + diff_btw_LS_and_RS$sagitta_width_diff, data=diff_btw_LS_and_RS))

# plot sagitta width diff and Fulton's
plot(x=diff_btw_LS_and_RS$sagitta_width_diff, y=diff_btw_LS_and_RS$Fulton, xlab = 'Difference in L and R sagitta width', ylab = 'Fultons K', main="Sagitta width asymmetry and Fultons K",col='blue')
cor(diff_btw_LS_and_RS$sagitta_width_diff, diff_btw_LS_and_RS$Fulton)# = -0.205...is this significant?
abline(lm(diff_btw_LS_and_RS$Fulton ~ 1 + diff_btw_LS_and_RS$sagitta_width_diff, data=diff_btw_LS_and_RS))

#So far, only plots with negative correlations are 
# difference in sagitta lengths plotted against Fulton's K
# difference in sagitta widths plotted against Ratio
# difference in sagitta widths plotted against Fulton's K

# Once we have otolith increment width data available, compare whether there is 
# a relationship between increment width in our 2, 3, or 4 rings and ratio.
# Can also compare Fulton's K with outer increment widths. 

# Run anova on ratios, by station
# use LL_only as raw data frame for ANOVA
library(tidyverse)
library(dplyr)
View(LL_only)

# make simplified data frame for ANOVA
levels(LL_only$Station) # says "NULL"
LL_only$Station <- ordered(LL_only$Station, levels = c("75", "82", "138", "142", "144"))
group_by(LL_only, Station) %>% summarise(count = n(),mean = mean(Ratio, na.rm = TRUE),sd = sd(Ratio, na.rm = TRUE))
# output: means all very similar
#group count  mean    sd
#1 75       15  4.20 0.575
#2 82       15  4.26 0.770
#3 138      15  4.09 0.653
#4 142      15  4.40 0.832
#5 144      15  4.34 0.676

# Create plot of ratio by station
install.packages("ggpubr")
library("ggpubr")
ggboxplot(LL_only, x = "Station", y = "Ratio", color = "Station", palette = c("red", "orange", "green", "blue", "purple"), order = c("75", "82", "138", "142", "144"), ylab = "Ratio", xlab = "Station")
res.aov <- aov(Ratio ~ Station, data = LL_only)
summary(res.aov) 
# output
#Df Sum Sq Mean Sq F value Pr(>F)
#group        4   0.91  0.2263   0.453   0.77
#Residuals   70  34.99  0.4999  
# Pr(>F) = 0.77, so not signficantly different (would need to be <.05)
TukeyHSD(res.aov)
# p values for all combinations are >0.05, so all stations would have a matching letter. 

# Now, see whether ratios group based on date of measurement
ratios_and_date <- read.csv("/Users/helena.mcmonagle/Desktop/Llopiz Lab/R/RNA_DNA_Ratios/data/RNA_DNA_by_date.csv")
View(ratios_and_date)
levels(ratios_and_date$group)
ratios_and_date$group <- ordered(ratios_and_date$group, levels = c("June27", "July25", "July27", "August28", "August29", "August30", "September4", "September6", "September7", "September11", "September12", "September18", "September20", "September21", "October2"))
levels(ratios_and_date$group)
group_by(ratios_and_date, group) %>% summarise(count = n(),mean = mean(ratio, na.rm = TRUE),sd = sd(ratio, na.rm = TRUE))
#output (note some NAs where couldn't compute SD, because too few data points for that date)
#group       count   mean      sd
#<fctr>      <int>  <dbl>   <dbl>
#1 ""              4 NaN    NaN    
#2 August28        6   3.47   0.250
#3 August29        6   4.42   0.656
#4 August30        6   3.98   0.482
#5 July25          5   4.86   0.372
#6 July27          1   4.37  NA    
#7 June27          1   4.67  NA    
#8 October2        2   5.27   0.212
#9 September11     6   3.97   0.331
#10 September12     6   3.83   0.425
#11 September18     6   4.37   0.956
#12 September20     6   4.87   0.558
#13 September21     6   5.05   0.679
#14 September4      6   4.07   0.795
#15 September6      6   4.01   0.170
#16 September7      6   3.87   0.425

ggboxplot(ratios_and_date, x = "group", y = "ratio", color = "group", palette = (rainbow(15)), order = c("June27", "July25", "July27", "August28", "August29", "August30", "September4", "September6", "September7", "September11", "September12", "September18", "September20", "September21", "October2"), ylab = "Ratio", xlab = "Date")
# There does seem to be difference by date--try ANOVA
res.aov <- aov(ratio ~ group, data = ratios_and_date)
summary(res.aov) 
# output
#Df Sum Sq Mean Sq F value   Pr(>F)    
#group       14  17.57  1.2553    4.11 5.76e-05 ***
#Residuals   60  18.32  0.3054                     
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#4 observations deleted due to missingness

TukeyHSD(res.aov)

install.packages('multcomp')
library(multcomp)
summary(glht(res.aov, linfct = mcp(group = "Tukey")))
# Some dates show statistical difference, and there does appear to be a trend of increasing
# ratio toward the last few days, but this could be by chance. 
