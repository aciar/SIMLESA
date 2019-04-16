# Factor Analysis of HH survey data
# Instructions.
# 1. Run R, change ‘directory’ to your work area (i.e. computer folder where the data is)
# 2. "Source R code" -> P4-2.R (this file)
# 3. Edit your household data file to save as a comma delimited csv file
# 4. Then edit below the name of your .csv file (the spreadsheet with your data) and the list of 
# variables to include into the ‘keep1’ list make sure you type the upper and lower cases for your 
# variables the same as in the csv file
# 5. Interpret the Factor.txt file (see Chapter 2) and select the names of the variables you
# will use for the cluster analysis
# 6. You will need to download the ‘psych’ and ‘nFactors’ R packages to use this script
# (see Quick R for information on downloading packages)

rm(list=ls()) # removes any lists and dataframes already loaded in to R

# Change the file name
HH_survey_data<- "YOUR DATA FILE NAME HERE.csv"

# Change the name of the directory the name of a new folder where your data is located
# You will need to create a new folder on your computer with the same name (letters are case sensitive)
# This is where the outputs of your analysis will be saved 
# Example provided for outputs from Western Kenya
out.dir<-"Kenya_West"

# List of variables you want to include for your principal component analysis 
# Remember for each variable you should include at least 10 observations 
# (i.e. 10 variables requires at least 100 households)
# Principal Component Analysis can only properly run with around 20 variables at most
keep1<-c("variable 1","variable 2","variable 3","variable 4","variable 5","variable 6","variable 7",
         "variable 8","variable 9")

# read data
df<-read.csv(HH_survey_data)

# Calculate any extra variables needed 
# (example given for new plot size calculated from plot length and plot width variables)
#df$PLOTSIZE<-df$PLOTLENGTH * df$PLOTWIDTH
#keep1 <- c(keep1, "PLOTSIZE")
# Any extra variables ^^^^^^^^^^^^

# only keep variables we're interested in
df<-df[ , names(df) %in% keep1]

## ensure all data is numeric - convert factor variables
for (x in names(df)) {if (is.factor(df[[x]])) {cat(x," is a factor - now transformed\n"); df[[x]]<-xtfrm(df[[x]])}c}

# replace any missing values
df<-as.data.frame(apply(df,2,function(x) {x[is.na(x)] <- mean(x,na.rm=T) ; x} ))

for (x in names(df)) {if (sd(df[[x]]) == 0 || is.na(sd(df[[x]]))) {cat(x," has no variance\n"); df <- df[,!(x==names(df))] }}

# Principal Components Analysis creating 15 principal components (i.e. artificial variables)
cat("Doing PCA\n")
library(psych)
# change the number “15” in the code below this line if you want to adjust the number of principal components to be created from your data
pc <- principal(df, nfactors=min(ncol(df),15), rotate="varimax") #rotated
sink(paste(out.dir,"/factors.txt",sep=""))
print(summary(pc)) # print the variance accounted for by each principal component
print(loadings(pc)) # pc loadings for each observed variable
sink()

# create scree plot (to help decide how many PCs to keep)
png(paste(out.dir,"/scree plot 1.png",sep="")) 
plot(pc$values,type="l",main="", xlab="# factors", ylab="Eigenvalue") # scree plot
dev.off()

# Create second scree plot of eigenvalues (look for the elbow in the line)
library(nFactors)
ev <- eigen(cor(df))
ap <- parallel(subject=nrow(df),var=ncol(df), rep=100, cent=.05)
nS <- nScree(ev$values, ap$eigen$qevpea)
png(paste(out.dir,"/ scree plot 2 - elbow.png",sep=""))
plotnScree(nS)
dev.off()

cat("PC loadings\n")
print(loadings(pc))
sink(paste(out.dir,"/ScreeValues.txt",sep=""))
print (pc$values)
sink()
flush(stdout())
