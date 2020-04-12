## Course Project 2 Plot 4
library(tidyverse)

#Reading in the data

NEI = readRDS("summarySCC_PM25.rds")
SCC = readRDS("Source_Classification_Code.rds")

## Plot 4: Across the US, how have emissions from coal combustion-related sources changed from 1999-2008?

# Subset only coal combustion related observations by looking through levels of EI.Sector 
Combustion = SCC %>% filter(EI.Sector == "Fuel Comb - Comm/Institutional - Coal")

# Create a vector with the SCC values that overlap 
intersection = intersect(NEI$SCC, Combustion$SCC)

# Create df containing all coal combustion-related observations
CoalEmissions = subset(NEI, SCC %in% intersection)

# Summarize total emissions by year for new df
CE_By_Year = CoalEmissions %>% group_by(year) %>% summarize(TE_Coal = sum(Emissions), M_Coal = mean(Emissions))

# Create plot
with(CE_By_Year, plot(year, TE_Coal, 
                      col = c("red", "green", "blue", "purple"),
                      pch = 18,
                      xlab = "Year",
                      ylab = "Total Emissions from Coal Combustion-related Sources",
                      main = "Total Emissions from Coal Combustion-related Sources by Year"))

#Adding lines connecting points
with(CE_By_Year, lines(x = year, y = TE_Coal, 
      type = "l", 
      lwd = 1, 
      lty = 3, 
      col = "red"))

#Labelling each point with total emissions value for that year
with(CE_By_Year, text(x = year, y = TE_Coal,
                      labels = round(TE_Coal),
                      cex = 0.7,
                      pos = c(1,1,2,2),
                      col = "black"))

dev.copy(png, file = "plot4.png", width = 900, height = 480, units = "px")
dev.off()