# Instructions
# 1. Before you can run this code, you need to create a datafile with the variables 
# you want included in your heatmap (saved as comma delimited file ‘.csv’)
# In this example we use the variables from the heatmaps in this book
# 2. Run R, change ‘directory’ to your work area (i.e. computer folder where the data is)
# 3. Edit the name of your heatmap .csv file (the spreadsheet with your data) in the code 
# 5. You will need to download the ‘ggplot2’, ‘plyr’, ‘scales’ and ‘reshape2’ R packages to use this code
# (see Quick R for information on downloading packages)

# Generating coloured heatmaps for household clusters 
# Load packages needed
library(ggplot2)
library(plyr)
library(scales)
library(reshape2)

# Set Working Directory (location of your datafile) – change example filepath in “” below 
setwd("C:/My Computer/SIMLESA Typologies/ country chapters/Kenya/Analysis/Cluster results")

# remove previously loaded data from your R workspace
rm(list=ls())

# The name of your .csv heatmap data file should be inserted below:
# Load data
df0 <- read.csv("YOUR HEATMAP FILE NAME HERE.csv",as.is=T)

# calculate order of typologies by total income (TotalInc) for heatmap 
# highest to lowest income
attach(df0)
heatmaporder<-df0[order(-TotalInc),] 

detach(df0)

# read order for graph
heatmaporder$Farm.type

####### Rescale data for heatmap figures #######
df1 <- melt(df0)
df2 <- ddply(df1, .(variable), transform,scale = rescale(value))

# Subset only variables for heatmap 
# (if you have more variables in your heatmap file than you want in your figure)
dfs<-subset(df2, variable=="Farm.type"|variable=="Land"|variable=="TLU_all" |variable=="CE"|variable=="Education"|variable=="wlkminse"|variable=="h.female.perc"|variable=="cropsales"|variable=="off.farm.inc"|variable=="other.income"|variable=="TotalInc") 
# need to make the variables name above are correctly written as they appear in your datafile

# Reorder clusters for final figure. This can be according to the order in "heatmaporder" values of 
# "Farm.type" - SEE ABOVE) or simply however you want your clusters ordered. 
# Change cluster names in the quotation marks below
dfs$Farm.type <- factor(dfs$Farm.type,ordered=T,levels= rev(c("CLUSTER 1","CLUSTER 2","CLUSTER 3","CLUSTER 4","CLUSTER 5","CLUSTER 6")))

# Create the graph (red colour)
p <- ggplot(dfs, aes(variable,Farm.type)) + geom_tile(aes(fill = scale),
                                                      colour = "white")+labs(y="Household Cluster", x="") + scale_fill_gradient(low = "white",
                                                                                                                                high = "darkred")+theme_bw()+theme(axis.text.x = element_text(angle = -45, hjust = 0), axis.text=element_text(size=16), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title=element_text(size=18,face="bold"), legend.title=element_text(size=16,face="bold"), legend.text=element_text(size=16), strip.text=element_text(size=20, face="bold"), plot.title=element_text(size=18, face="bold"))+labs(colour="Relative value of Variable (0-1)")

# Create the graph (green colour)
p2 <- ggplot(dfs, aes(variable,Farm.type)) + geom_tile(aes(fill = scale),
                                                       colour = "white")+labs(y="Household Cluster", x="") + scale_fill_gradient(low = "white",
                                                                                                                                 high = "darkgreen")+theme_bw()+theme(axis.text.x = element_text(angle = -45, hjust = 0), axis.text=element_text(size=16), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title=element_text(size=18,face="bold"), legend.title=element_text(size=16,face="bold"), legend.text=element_text(size=16), strip.text=element_text(size=20, face="bold"), plot.title=element_text(size=18, face="bold"))+labs(colour="Relative value of Variable (0-1)")


# Create the graph (blue colour)
p3 <- ggplot(dfs, aes(variable,Farm.type)) + geom_tile(aes(fill = scale),
                                                       colour = "white")+labs(y="Household Cluster", x="") + scale_fill_gradient(low = "white",
                                                                                                                                 high = "darkblue")+theme_bw()+theme(axis.text.x = element_text(angle = -45, hjust = 0), axis.text=element_text(size=16), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title=element_text(size=18,face="bold"), legend.title=element_text(size=16,face="bold"), legend.text=element_text(size=16), strip.text=element_text(size=20, face="bold"), plot.title=element_text(size=18, face="bold"))+labs(colour="Relative value of Variable (0-1)")


# Save the graphs to file (may need to adjust height or width dimensions depending on your data)
ggsave(p, filename="Kenya Heatmap - red.png", width=350, height=250, units="mm", dpi=350)
ggsave(p2, filename="Kenya Heatmap - green.png", width=350, height=250, units="mm", dpi=350)
ggsave(p3, filename="Kenya Heatmap - blue.png", width=350, height=250, units="mm", dpi=350)
# Identify whether clusters are either low income or high Income, 
# (based on being in the top half of the observed income range)
# (in other words, by scaled income above 0.5) – remember to rename “TotalInc” 
# to your income variable
inor<-subset(dfs, variable=="TotalInc")
inor$incrank<-ifelse(inor$scale > 0.5,c("High Income"), c("Low Income"))
View(inor)
####################################################
