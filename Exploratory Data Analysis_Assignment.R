
library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
  
## Plot 1: Have total emissions from PM2.5 decreased in the United States 
# from 1999 to 2008? Using the base plotting system, make a plot showing 
# the total PM2.5 emission from all sources for each of the years 1999, 
# 2002, 2005, and 2008.

total_emissions <- aggregate(Emissions ~ year, NEI, sum)

png("plot1_total_emissions.png", width = 800, height = 600)
barplot(total_emissions$Emissions/1000000,
        names.arg = total_emissions$year,
        ylab = 'Millions of Tons of PM2.5 Emissions',
        xlab = 'Year',
        main = 'PLOT 1: PM2.5 Emissions in US from 1999 to 2008')

dev.off()

## Plot 2: Have total emissions from PM2.5 decreased in the Baltimore City, 
# Maryland from 1999 to 2008? Use the base plotting system to make a plot 
# answering this question.
  
baltimoreNEI <- NEI[which(NEI$fips == "24510"),]
baltimore <- aggregate(Emissions ~ year, baltimoreNEI , sum)
  
png("plot2_baltimore_emissions.png", width = 800, height = 600)
barplot(baltimore$Emissions,
         names.arg = baltimore$year,
         ylab = 'Tons of PM2.5 Emissions',
        xlab = 'Year',
        main = 'PLOT 2: PM2.5 Emissions in Baltimore from 1999 to 2008')

dev.off()

## Plot 3: Of the four types of sources indicated by the type (point, nonpoint, 
# onroad, nonroad) variable, which of these four sources have seen decreases 
# in emissions from 1999-2008 for Baltimore City? Which have seen increases 
# in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot 
# answer this question.
  
baltimore_byType <- 
   ggplot(baltimoreNEI,
   aes(factor(year),Emissions,fill=type)) + 
   geom_col() +
   facet_grid(.~type,scales = "free",space="free") + 
   xlab("Year") + ylab("Total PM2.5 Emission (Tons)") +
   ggtitle("PLOT 3: PM2.5 Emissions in Baltimore from 1999 to 2008 by Source Type")
  
png("plot3_baltimore_byType.png", width = 800, height = 600)
print(baltimore_byType)

dev.off()

## 'Non-Road', 'NonPoint' and 'On-Road' source types has seen a decrease in emissions from 1999 to 2008
## 'Point' source type has seen an increase in emissions from 1999 to 2008

## Plot 4: Across the United States, how have emissions from coal combustion-related sources 
# changed from 1999-2008?

# Index coal combustion in NEI data
combustion <- grepl("comb", SCC$SCC.Level.One, ignore.case=T)
coal <- grepl("coal", SCC$SCC.Level.Four, ignore.case=T) 
combustionSCC <- SCC[combustion & coal,]$SCC
combustionNEI <- NEI[NEI$SCC %in% combustionSCC,]
combustionNEI_agg <- aggregate(Emissions ~ year, combustionNEI , sum)

png("plot4_coalcombustion_emissions.png", width = 800, height = 600)
barplot(combustionNEI_agg$Emissions/1000000,
        names.arg = combustionNEI_agg$year,
        ylab = 'Millions of Tons of PM2.5 Emissions',
        xlab = 'Year',
        main = 'PLOT 4: Coal Combustion PM2.5 Emissions in US from 1999 to 2008')

dev.off()

## Plot 5: How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

baltimoreMotorNEI <- NEI[which(NEI$fips == "24510" & NEI$type == "ON-ROAD"),]
baltimoreMotor <- aggregate(Emissions ~ year, baltimoreMotorNEI , sum)

png("plot5_baltimoreMotor_emissions.png", width = 800, height = 600)
barplot(baltimoreMotor$Emissions,
        names.arg = baltimoreMotor$year,
        ylab = 'Tons of PM2.5 Emissions',
        xlab = 'Year',
        main = 'PLOT 5: PM2.5 Emissions from Motor Vehicles in Baltimore from 1999 to 2008')

dev.off()

## Plot 6: Compare emissions from motor vehicle sources in Baltimore City with emissions from 
# motor vehicle sources in Los Angeles County, California. Which city has seen greater changes 
# over time in motor vehicle emissions?
library(sqldf)
LAvBal_motorNEI <- NEI[which((NEI$fips == "24510" | NEI$fips == "06037") & NEI$type == "ON-ROAD"),]
LAvBal_motor <- sqldf('select year, 
                      sum(case when fips = "24510" then ifnull(Emissions,0) end) as BAL_Emissions, 
                      sum(case when fips = "06037" then ifnull(Emissions,0) end) as LA_Emissions 
                      from LAvBal_motorNEI group by 1 order by 1')

LAvBal_motor$BAL_Emissions_change <- (LAvBal_motor$BAL_Emissions - LAvBal_motor$BAL_Emissions[1])/LAvBal_motor$BAL_Emissions[1]
LAvBal_motor$LA_Emissions_change <- (LAvBal_motor$LA_Emissions - LAvBal_motor$LA_Emissions[1])/LAvBal_motor$LA_Emissions[1]

LAvBal_motor_byCity <- 
  ggplot(LAvBal_motor, aes(year)) + 
  geom_line(aes(y = BAL_Emissions, colour = "Baltimore City")) + 
  geom_line(aes(y = LA_Emissions, colour = "Los Angeles County")) +
  xlab("Year") + ylab("Total PM2.5 Emission (Tons)") +
  ggtitle("PLOT 6A: PM2.5 Emissions from Motor Vehicles in Baltimore and LA County from 1999 to 2008")

png("plot6a_LAvsBaltimoreMotor_byCity.png", width = 800, height = 600)
print(LAvBal_motor_byCity)

dev.off()

LAvBal_motorChange_byCity <- 
  ggplot(LAvBal_motor, aes(year)) + 
  geom_line(aes(y = BAL_Emissions_change, colour = "Baltimore City")) + 
  geom_line(aes(y = LA_Emissions_change, colour = "Los Angeles County")) +
  xlab("Year") + ylab("PM2.5 Emission Percentage Change from base year 1999") +
  ggtitle("PLOT 6B: PM2.5 Emissions Percentage Change from Motor Vehicles in Baltimore and LA County indexed to 1999")

png("plot6b_LAvBal_motorChange_byCity.png", width = 800, height = 600)
print(LAvBal_motorChange_byCity)

dev.off()

##Baltimore City has experienced the greater changes over time in motor vehicle emissions