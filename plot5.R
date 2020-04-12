## Course Project 2 Plot 5
library(tidyverse)

#Reading in the data

NEI = readRDS("summarySCC_PM25.rds")
SCC = readRDS("Source_Classification_Code.rds")

## Plot 5: How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

# Create logical index that returns true for each observation where EI.Sector contains mobile
EI_index = grepl("Mobile", SCC$EI.Sector)

# Subset by mobile vehicle sources
MotorVehicles = SCC[EI_index == 1,]

# Create vector with SCC values that overlap in NEI and SCC
intersection = intersect(NEI$SCC, MotorVehicles$SCC)

# Create df that contains observations for only mobile vehicle sources
MVS = subset(NEI, SCC %in% intersection)

# Filter out all locations that aren't baltimore
MVS_Baltimore = MVS %>% filter(fips == "24510") %>% group_by(year) %>% summarize(TE = sum(Emissions))

# Create plot
with(MVS_Baltimore, plot(year, TE, 
                      col = c("red", "green", "blue", "purple"),
                      pch = 18,
                      xlab = "Year",
                      ylab = "Total Emissions from Motor Vehicle Sources",
                      main = "Total Emissions from Motor Vehicle Sources by Year in Baltimore"))

#Adding lines connecting points
with(MVS_Baltimore, lines(x = year, y = TE, 
                       type = "l", 
                       lwd = 1, 
                       lty = 3, 
                       col = "red"))

#Labelling each point with total emissions value for that year
with(MVS_Baltimore, text(x = year, y = TE,
                      labels = round(TE),
                      cex = 0.7,
                      pos = c(1,2,2,2),
                      col = "black"))

dev.copy(png, file = "plot5.png", width = 900, height = 480, units = "px")
dev.off()