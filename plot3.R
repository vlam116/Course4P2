## Course Project 2 Plot 3
library(tidyverse)

#Reading in the data

NEI = readRDS("summarySCC_PM25.rds")
SCC = readRDS("Source_Classification_Code.rds")

## Plot 3: Of the four types of sources indicated by type, which of these four have seen decreases in emissions
## from 1999-2008 for Baltimore City? Which have seen increases? Use ggplot2. 


## Create a new DF filtering observations with fips = 24510, then create df with sum of emissions for each year
## in Baltimore by type. Change type to factor so we can label points by color later. 

NEI24510 = NEI %>% filter(fips == "24510")
EBT = NEI24510 %>% group_by(type, year) %>% summarize(TE = sum(Emissions))
EBT$type = as.factor(EBT$type)

## Plotting total emissions by year and labelling by type.

qplot(x = year,
      y = TE,
      data = EBT,
      color = type, #color codes points by type
      geom = c("point", "line", "text"), #plots points on graph, connects with a line, and labels by TE value
      label = round(TE), # label by rounded TE value
      vjust = 1.5, # adjusting position of each label
      hjust = 1,
      xlab = "Year", # labeling axes and giving title
      ylab = "Total Emissions",
      main = "Total Emissions in Baltimore by Type") +
      scale_color_discrete(name = "Emission Type") # renaming the legend

dev.copy(png, file = "plot3.png", width = 900, height = 480, units = "px")
dev.off()
