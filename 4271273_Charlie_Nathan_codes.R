library(ggplot2)
library(dplyr)
library(readxl)
library(ggrepel)
library(scales)

##############################################################################################################
# LOAD DATA
##############################################################################################################

message("loading data...")

# full HPI dataset
HPI_Dataset <- read_excel("Uni_Work/2ndSemester/FIV/HPI_Dataset.xlsx", sheet = "Complete HPI data", range = "B6:J146")

# read the Corruption dataset
world_corruption_index_DataSet <- read_excel("Uni_Work/2ndSemester/FIV/world_corruption_index_DataSet.xlsx", range = "A1:J177")

##############################################################################################################
# DATA MANIPULATION
##############################################################################################################

message("Filtering data...")

# Removing uneeded variable columns from both datasets

HPI_Dataset <- select(HPI_Dataset,-c(6,9,10,11))
world_corruption_index_DataSet <- select(world_corruption_index_DataSet,-c(3,5,6,7,10,14))

# merging world_corruption_index_DataSet with HPI dataset

message("merging data...")

Joined_HPI_Corruption_DataSet <-left_join(HPI_Dataset, world_corruption_index_DataSet, by = c("Country" = "country"))

message("Renaming merged variables...")

# renaming new merged data columns

names(Joined_HPI_Corruption_DataSet)[11] <- "Corruption_Score"
names(Joined_HPI_Corruption_DataSet)[10] <- "Corruption_Index_Rank"
names(Joined_HPI_Corruption_DataSet)[12] <- "Corruption_Score_Lower_Margin"
names(Joined_HPI_Corruption_DataSet)[13] <- "Corruption_Score_Higher_Margin"

##############################################################################################################
# BASIC ANALYSIS AND VISUALIZATION
##############################################################################################################

# Initial questions

message("RQ1: Does economic wealth, mean a more ecologically sustainable country whilst maintaining happiness?")

HPI_GDP_labelled <- ggplot(HPI_Dataset, aes(x=HPI_Rank, y= GDP_per_Capita, color=Region)) + geom_point(size=3)
HPI_GDP_labelled <- HPI_GDP_labelled + ggtitle("HPI Ranking and GDP per Capita") + geom_text_repel(aes(label=Country), size = 2)

message("RQ2: Does the sense of being satisfied and happy in life occur more in countries with a higher life expectancy? And does GDP per capita have an effect?")

LifeExpectancy_Wellbeing <- ggplot(HPI_Dataset, aes(x=Average_Wellbeing, y= Average_Life_Expectancy, size=GDP_per_Capita)) + geom_point() + geom_smooth(method=lm, fullrange = TRUE)

message("RQ3: Does a high economic output (GDP per Capita) result in a high Carbon footprint?")

Region_Carbon_GDP <- ggplot(HPI_Dataset, aes(x=Carbon_Footprint, y=GDP_per_Capita, color = Region)) + geom_point() + facet_grid(Region~ .)

message("RQ4: which regions have the highest and lowest levels of Inequality of Opportunity?")

Region_Inequality <- HPI_Dataset %>% group_by(Region) %>% summarise(Average_Inequality = mean(Inequality))
Region_Inequality_BarChart <- ggplot(Region_Inequality, aes(x=Region, y=Average_Inequality, fill=Region)) + geom_col(colour = "black")

# Follow up questions

message("RQ5: Do Countries with a high sense of corruption generally score worse in the HPI rankings?")

CorruptionIndexRank_AverageWellbeing <- ggplot(Joined_HPI_Corruption_DataSet, aes(x=Corruption_Index_Rank, y= Average_Wellbeing, colour=HPI_Rank)) + geom_point(size=4)
CorruptionIndexRank_AverageWellbeing <- CorruptionIndexRank_AverageWellbeing + scale_colour_gradient2(low=muted('blue'), mid = 'white', high=muted('red'), midpoint=70)
CorruptionIndexRank_AverageWellbeing <- CorruptionIndexRank_AverageWellbeing + stat_smooth(method = loess, colour="black")

message("RQ6: which regions have the highest and lowest sense of corruption?")

Regional_Corruption <- ggplot(Joined_HPI_Corruption_DataSet, aes(x=factor(Region), y=Corruption_Score)) + geom_boxplot()

dev.off()
