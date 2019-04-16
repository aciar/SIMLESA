# Household Cluster Analysis (to be run after PCA)
# Instructions.
# 1. Your household survey data file needs to have a variable for the ID of each household
# and this variable needs to be called “hhldid”
# 2. Run R, change ‘directory’ to your work area (i.e. computer folder where the data is)
# 3. Edit your household data file to save as a comma delimited csv file 
# 4. Then edit the name of your .csv file (the spreadsheet with your data) in the code below 
# 5. Edit the list of variables to include into the ‘keep1’ list
# make sure you type the upper and lower cases for your variables the same as in the csv file
# 6. Interpret the Cluster dendrogram plot (see Chapter 2) to identify how many clusters you 
# want to keep (then run the cluster analysis again with that number)
# 7. You will need to download the ‘ape’ and ‘sparcl’ R packages to use this script 
# (see Quick R for information on downloading packages)

# Set Working Directory (location of your datafile) – change example filepath in “” below 
setwd("C:/My Computer/SIMLESA Typologies/ country chapters/Kenya/Analysis")

# load packages needed from library
library(ape)
library(sparcl)

# remove previously loaded lists and data from workspace
rm(list=ls())

# Load household data file (change name below to your file name - letters are case sensitive)
HH_survey_data<- "YOUR HOUSEHOLD DATA FILE.csv"

# Select the number of clusters to keep (currently set to 3). This should be adjusted after examining dendrograms and all subsequent code run again.
nclust<-3

# list variables (from PCA + any additions) that you want to use in Cluster Analysis 
keep1<-c("variable 1","variable 2","variable 3","variable 4","variable 5")

# read data
dfAll<-read.csv(HH_survey_data)
df<-dfAll[ , names(dfAll) %in% keep1]

## ensure all data is numeric - convert factor data
for (x in names(df)) {if (is.factor(df[[x]])) {cat(x," is transformed\n"); df[[x]]<-xtfrm(df[[x]])}}

# replace any missing values with averages
df<-as.data.frame(apply(df,2,function(x) {x[is.na(x)] <- mean(x,na.rm=T) ; x} ))

#create distance matrix using Euclidean distance
d<-dist(scale(df), method = "euclidean")

# Ward’s Hierarchical Clustering of distance matrix
fit <- hclust(d, method="ward")
# apply number of clusters to keep
c <- cutree(fit, k = nclust)

# Create dendrogram plots 
plot(fit, xlab="Households")
# draw red rectangles around clusters – specify number by changing “3” in code below
rect.hclust(fit, k=3, border="red")
# Save dendrogram, (adjust plot dimensions – i.e. width and length - by changing the ‘500, 500’ below)
png("FILENAME FOR SAVING DENDROGRAM HERE.png", 500, 500)
y = cutree(fit, 3)
# create dendrogram with each household coloured per cluster. Don’t forget to change the plot title.
ColorDendrogram(fit, y = y, labels = names(y), main = "ENTER PLOT TITLE HERE", 
                branchlength = 80)
dev.off()
# Finish export to excel
dist.cluster <- function(i, distmat, clusters) {
  ind <- (clusters == i)
  return(rowSums( distmat[ind, ind]))
}
d1<-as.matrix(d)
out<-data.frame(hhldid=dfAll$hhldid,cluster=c,dist=NA,cluster.rank=NA)
for (i in unique(c)) {
  print (i)
  m<-dist.cluster(i,d1,c)
  out$dist[as.numeric(names(m))] <- m
  out$cluster.rank[as.numeric(names(m))] <- order(m)
}

write.csv(out, "FILENAME FOR SAVING DATA WITH HOUSEHOLD ID AND ALLOCATED CLUSTER.csv",row.names=F)
dev.off(filename="FILENAME FOR FINAL COLOUR DENDROGRAM PLOT.png")

################################################################################

### Calculating file for medians of each observed variable given for each cluster groups
# Read data
dfAll<-read.csv(HH_survey_data)
for (x in names(dfAll)) {if (is.factor(dfAll[[x]])) {cat(x," is transformed\n"); dfAll[[x]]<-xtfrm(dfAll[[x]])}}
dfAll<-as.data.frame(apply(dfAll,2,function(x) {x[is.na(x)] <- mean(x,na.rm=T) ; x} ))

statclus <- aggregate(dfAll[,-c(1,2)],list(c),median)
medianstats<-data.frame(Cluster=statclus[,1],Freq=as.vector(table(c)),statclus[,-1])
write.csv(medianstats, file="FILENAME FOR NEW FILE WITH MEDIANS OF OBSERVED VARIABLES FOR EACH CLUSTER.csv")
