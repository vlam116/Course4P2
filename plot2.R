## Course Project 2 Plot 2
library(tidyverse)

#Reading in the data

NEI = readRDS("summarySCC_PM25.rds")
SCC = readRDS("Source_Classification_Code.rds")

## Plot 2: Have total emissions from pm25 decreased in Baltimore City from 1999 to 2008? Use base plotting system.

## Create a new DF filtering observations with fips = 24510, then create df with sum of emissions for each year
## in Baltimore. 
NEI24510 = NEI %>% filter(fips == "24510")
BaltimoreTE = NEI24510 %>% group_by(year) %>% summarize(TotalEmissions = sum(Emissions))

#Set margins
par(mar = c(4,4,2,2))

#Creating plot
with(BaltimoreTE, plot(year, TotalEmissions, #x,y variables to plot
              xlab = "Year", #x axis label
              ylab = "Total Emissions in Baltimore", #y axis label
              pch = 18, #using diamond points
              col = c("red", "green","blue","purple"), #manually setting colors for each point
              main = "Total pm25 Emissions by Year in Baltimore")) #plot title

#Adding lines connecting points
lines(x = BaltimoreTE$year, y = BaltimoreTE$TotalEmissions, 
      type = "l", 
      lwd = 1, 
      lty = 3, 
      col = "red")

#Adding legend 
legend("topright", 
       legend = c("1999","2002","2005","2008"), 
       pch = 18, 
       col = c("red","green","blue","purple"))

#Labelling each point with total emissions value
text(x = BaltimoreTE$year, y = BaltimoreTE$TotalEmissions, 
     labels = round(BaltimoreTE$TotalEmissions), 
     cex = 0.7, pos = c(1,1,3,2), 
     col = "black")

dev.copy(png, file = "plot2.png", width = 900, height = 480, units = "px")
dev.off()