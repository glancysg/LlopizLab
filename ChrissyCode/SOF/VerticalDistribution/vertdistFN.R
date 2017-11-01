## Functions for calculating Vertical Distributions

propatdepth<-function(vertdistdata, taxon, nSC=4, maxlength=NULL, zbins=c(25,50,75,100)){
  # function to calculate proportions at depth from lengths
  # assumes MOCNESS samples with standard depth bins
  # vertdistdata MUST BE a dataframe with columns [SampleID Taxon Lengthmm TargetMax vol Pmin Pmax]
  # taxon must be a string, for example "Mullidae"
  # nSC selects the number of size classes to use, default is 4
  # maxlength sets the maximum length, default is to use the longest length in the data
  # zbins sets the bottom of the depth bins. The default is 25m deep bins, with Neuston combined in the top bin
  
  # rename the columns of vertdistdata:
  names(vertdistdata)<- c("SampleId", "Taxon", "Length", "TargetMax", "Vol", "Pmin", "Pmax")
  # discard all the lengthdata that isn't the taxon of interest
  I<- which(vertdistdata$Taxon==taxon)
  vertdistdata<- vertdistdata[I,]
  
  # discard the lengths that are greater that maxlength
  if (!is.null(maxlength)){
    I<- which(vertdistdata$Length>maxlength)
    vertdistdata<- vertdistdata[-I,]
  }
  
  # Create Size Classes
  minlength<- min(vertdistdata$Length)
  if (is.null(maxlength)){
    maxlength<- max(vertdistdata$Length)
  }
  range<- maxlength-minlength
  SC<- seq(from=minlength-0.05, to=maxlength, length.out=nSC+1)
  
  # make the depth bin column for easier summarizing and nice labels on plots later
  vertdistdata$DepthBin <- NA
  for (i in 1:length(DepthBins)){
    if (i==1 & zbins[i]!=0){
      DepthBini<- paste(0,'-',zbins[i], sep='')
      I<- which(vertdistdata$TargetMax<=zbins[i])
      }
    else {
      DepthBini<- paste(zbins[i-1],'-',zbins[i], sep='')
      I<- which(vertdistdata$TargetMax> zbins[i-1] & vertdistdata$TargetMax<= zbins[i]) 
      }
    vertdistdata$DepthBin[I]<- DepthBini
  }
  
  # Bin the rows into size classes: 
  # Also, count how many in each size class along the way:
  SCcount<- data.frame(sizeclass=1:nSC, count=NA)
  for (i in 1:4){
    j<- which(th4$Lengthmm>SC[i] & th4$Lengthmm<=SC[i+1])
    th4$sizeclass[j]<- i
    thalcount$count[i]<- length(j)
  }
  
  output<- list(proportions=propz ,SCcount=SCcount)
  return(output)
}