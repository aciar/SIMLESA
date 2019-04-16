# Instructions
# 1. Before you can run this code, you need to add in a variable (i.e. column) into your 
# survey data called ‘cluster’. This needs to have the cluster number input for every 
# household. These cluster numbers can be taken from the output file created from the 
# cluster analysis script
# 2. Run R, change ‘directory’ to your work area (i.e. computer folder where the data is)
# 3. Edit your household data file to save as a comma delimited csv file 
# 4. Then edit the name of your .csv file (the spreadsheet with your data) in the code below 
# 5. You will need to download the ‘psych’ R package to use this script 
# (see Quick R for information on downloading packages)

### Cluster analysis - creating boxplots and descriptive statistics
#Analysing Clusters for describe by stats

# Set Working Directory (location of your datafile) – change example filepath in “” below 
setwd("C:/My Computer/SIMLESA Typologies/ country chapters/Kenya/Analysis/Cluster results")

# Remove previously loaded data from workspace
rm(list=ls())
# load packages needed
library(psych)

# Name of your csv file, remember this needs to have cluster numbers in a column for 
# each household (taken from earlier cluster analysis)
HH_survey_data<- "NAME OF YOUR DATAFILE HERE.csv"

# Change the name of the directory where you want the output files to be
# make sure there is a folder with the exact same name in the same location 
# where your household data is
out.dir<-" OUTPUTS DATA FOLDER NAME HERE"

############################################################################
### Run analysis for boxplots and descriptive statistics

# make boxplots
cnt.n <- function(x,n) sum(x>=n,na.rm=T)
draw.boxplot <- function(v1) {
  boxplot(as.formula(paste(v1, "~ cluster")),data=df,main=v1,ylab=v1,xlab="cluster")
  m1 <- tapply(df[[v1]],df$cluster,mean,na.rm=T)
  cls.no<-length(unique(df$cluster))
  y <- matrix(m1,nrow=cls.no,ncol=2)
  x <- outer(1:cls.no,c(-0.4,0.4),"+")
  for (i in 1:cls.no) lines(x[i,],y[i,],lty=2)
}

# read raw data again
df<-read.csv(HH_survey_data)

printSummaryOld<-function(v) {
  cat("Dataset,\"", HH_survey_data, "\"\n")
  cat("Variable,", v, "\n")
  all<-as.data.frame(describeBy(df[[v]], group=rep("all",nrow(df)))[1])
  for (clust in describeBy(df[[v]], df$cluster)) { all<- rbind(all, unlist(clust))}
  
  names(all)<-c("var","n","mean","sd","median","trimmed","mad","min","max","range","skew","kurtosis")
  row.names(all)<-c("all", levels(df$cluster))
  cat(write.csv(all))
}

printSummary<-function(v) {
  cat("Dataset,\"", HH_survey_data, "\"\n")
  cat("Variable,", v, "\n")
  all<-as.data.frame(describeBy(df[[v]] ))
  all$sum<-sum(df[[v]])
  c<-NULL
  for (clust in describeBy(df[[v]], df$cluster)) { 
    c<- rbind(c, unlist(clust))
  }
  c<-cbind(c,sum=as.vector(by(df[[v]], df$cluster,sum)))
  out<-rbind(all, c)
  row.names(out)<-c("all", as.character(unique(df$cluster)))
  cat(write.csv(out))
}

for (v1 in names(df)) {
  if (is.numeric(df[[v1]])) {
    png(paste(out.dir,"/",v1,".png",sep=""))
    try(draw.boxplot(v1), silent=T)
    dev.off()
    sink(paste(out.dir,"/",v1,".csv",sep=""))
    try(printSummary(v1), silent=T)
    sink()
  }
}
