## Course Project 2 Plot 6
library(tidyverse)

#Reading in the data

NEI = readRDS("summarySCC_PM25.rds")
SCC = readRDS("Source_Classification_Code.rds")

## Plot 6: Compare emissions from MVS in Baltimore with emissions from MVS in Los Angeles County. Which has seen
## greater changes over time in MVS?

# Create logical index that returns true for each observation where EI.Sector contains mobile
EI_index = grepl("Mobile", SCC$EI.Sector)

# Subset by mobile vehicle sources
MotorVehicles = SCC[EI_index == 1,]

# Create vector with SCC values that overlap in NEI and SCC
intersection = intersect(NEI$SCC, MotorVehicles$SCC)

# Create df that contains observations for only mobile vehicle sources
MVS = subset(NEI, SCC %in% intersection)

# Filter out all locations that aren't baltimore and create df with total emissions by year and % change by year
MVS_Baltimore = MVS %>% filter(fips == "24510") %>% group_by(year) %>% summarize(TE = sum(Emissions)) %>%
  mutate(percent_change = ((TE - lag(TE, n = 1))/lag(TE, n=1))*100)

# Replace NA value with 0
MVS_Baltimore$percent_change[1] = 0

# Filter out all locations that aren't LA and create a df with total emissions by year and % change by year
MVS_LA = MVS %>% filter(fips == "06037") %>% group_by(year) %>% summarize(TE = sum(Emissions)) %>%
  mutate(percent_change = ((TE - lag(TE, n = 1))/lag(TE, n=1))*100)

# Replace NA value with 0
MVS_LA$percent_change[1] = 0

# set up plotting space to fit 4 plots 
par(mfrow = c(2,2))

# Create plot for baltimore emissions by year
with(MVS_Baltimore, plot(year, TE, 
                         col = c("red", "green", "blue", "purple"),
                         pch = 18,
                         xlab = "Year",
                         ylab = "Total Emissions",
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

# Create plot for LA emissions by year
with(MVS_LA, plot(year, TE, 
                         col = c("red", "green", "blue", "purple"),
                         pch = 18,
                         xlab = "Year",
                         ylab = "Total Emissions",
                         main = "Total Emissions from Motor Vehicle Sources by Year in Los Angeles"))

#Adding lines connecting points
with(MVS_LA, lines(x = year, y = TE, 
                          type = "l", 
                          lwd = 1, 
                          lty = 3, 
                          col = "red"))

#Labelling each point with total emissions value for that year
with(MVS_LA, text(x = year, y = TE,
                         labels = round(TE),
                         cex = 0.7,
                         pos = c(3,2,2,2),
                         col = "black"))

# Create plot for the percent change in emissions in baltimore by year
with(MVS_Baltimore, plot(year, percent_change, 
                  col = c("red", "green", "blue", "purple"),
                  pch = 18,
                  xlab = "Year",
                  ylab = "% Change",
                  main = "Percent Change in Emissions from Motor Vehicle Sources by Year in Baltimore"))

#Adding lines connecting points
with(MVS_Baltimore, lines(x = year, y = percent_change, 
                   type = "l", 
                   lwd = 1, 
                   lty = 3, 
                   col = "red"))

# Add a horizontal line at 0 as a reference for no percent change
abline(h = 0, lwd = 1, lty = 3)

#Labelling each point with percent change value for that year
with(MVS_Baltimore, text(x = year, y = percent_change,
                  labels = round(percent_change),
                  cex = 0.7,
                  pos = c(1,2,2,2),
                  col = "black"))
# Create plot for percent change in emissions in LA by year
with(MVS_LA, plot(year, percent_change, 
                  col = c("red", "green", "blue", "purple"),
                  pch = 18,
                  xlab = "Year",
                  ylab = "% Change",
                  main = "Percent Change in Emissions from Motor Vehicle Sources by Year in Los Angeles"))

#Adding lines connecting points
with(MVS_LA, lines(x = year, y = percent_change, 
                   type = "l", 
                   lwd = 1, 
                   lty = 3, 
                   col = "red"))

# Add a horizontal line at 0 as reference for no percent change
abline(h = 0, lwd = 1, lty = 3)

#Labelling each point with percent change value for that year
with(MVS_LA, text(x = year, y = percent_change,
                  labels = round(percent_change),
                  cex = 0.7,
                  pos = c(1,2,2,2),
                  col = "black"))

dev.copy(png, file = "plot6.png", width = 1200, height = 900, units = "px")
dev.off()