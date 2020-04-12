## Course Project 2 Plot 1
library(tidyverse)

#Reading in the data

NEI = readRDS("summarySCC_PM25.rds")
SCC = readRDS("Source_Classification_Code.rds")

## Plot 1: Have total emissions from pm25 decreased in the US from 1999 to 2008? Use base plotting system
## to plot the total pm25 emission from all sources for the years 1999, 2002, 2005, 2008.

#Create df with sum of emissions for each year
TE = NEI %>% group_by(year) %>% summarize(TotalEmissions = sum(Emissions)) 

#Set margins
par(mar = c(4,4,2,2))

#Creating plot
with(TE, plot(year, TotalEmissions, #x,y variables to plot
              xlab = "Year", #x axis label
              ylab = "Total Emissions", #y axis label
              pch = 18, #using diamond points
              col = c("red", "green","blue","purple"), #manually setting colors for each point
              main = "Total pm25 Emissions by Year")) #plot title

#Adding lines connecting points
lines(x = TE$year, y = TE$TotalEmissions, 
      type = "l", 
      lwd = 1, 
      lty = 3, 
      col = "red")

#Adding legend 
legend("topright", 
       legend = c("1999","2002","2005","2008"), 
       pch = 18, 
       col = c("red","green","blue","purple"))

#Labelling each point with total emissions value for that year
text(x = TE$year, y = TE$TotalEmissions, 
     labels = round(TE$TotalEmissions), 
     cex = 0.7, pos = c(1,1,3,2), 
     col = "black")

dev.copy(png, file = "plot1.png", width = 900, height = 480, units = "px")
dev.off()