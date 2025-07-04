
# PROJECT:     STC Herring
# R VERSION:		4.4.2
# PROGRAMMED:		Perttu Rantanen
# EDITS:        
# UPDATE:       04.7.2025

#clean the R environment
#rm(list = ls())

library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)

run.year = 2025

#PATHS
path <- paste0(getwd(), "/") # Working directory
codePath  <- paste0(path, "prog/")   # Location to store R scripts
outPath   <- paste0(path, "out/",run.year)   # Location to store the results
dataPath  <- paste0(path, "orig/",run.year, "/")   # Location to store the data

# Check if zip file exists:
file.exists(paste0(dataPath, "2024_fdi_catches.zip"))

# UNZIP the zip archive
unzip(paste0(dataPath, "2024_fdi_catches.zip"), exdir = dataPath, overwrite = TRUE, junkpaths = TRUE)

# Read data in
yearsToSubmit <- c(2013:(run.year-2))

# Looping through the years to read in CSV data:
for (year in yearsToSubmit) {
  print(paste0("Start loop for year ", year))
  
  #----------------------------------------------------------------------------
  # 1.1 Load Table A data from CSV file ----
  #----------------------------------------------------------------------------
  
  file_path <- file.path(dataPath, paste0("FDI Catches by country", year, ".csv"))
  
  Table_A <- read.csv(file_path, stringsAsFactors = FALSE)
}


# Create an empty list to store data frames
all_data <- list()

for (year in yearsToSubmit) {
  print(paste0("Start loop for year ", year))
  
  #----------------------------------------------------------------------------
  # 1.1 Load Table A data from CSV file ----
  #----------------------------------------------------------------------------
  
  file_path <- file.path(dataPath, paste0("FDI Catches by country", year, ".csv"))
  
  Table_A <- read.csv(file_path, stringsAsFactors = FALSE)
  Table_A$year <- year # Add a column for the year
  
  # Add the data frame to the list
  all_data[[as.character(year)]] <- Table_A
}

# Combine all data frames into one
combined_data <- do.call(rbind, all_data)


FDI_LAN_HER <- combined_data %>%
  filter(species=='HER' & sub_region %in% c("27.3.A.20","27.3.A.21","27.3.B.23","27.3.C.22","27.3.D.24","27.3.D.25","27.3.D.26",
                                            "27.3.D.27","27.3.D.28.1","27.3.D.28.2","27.3.D.29","27.3.D.30","27.3.D.31","27.3.D.32","27.4.A")
         & !eez_indicator %in% c("UK") & !country %in% c("United Kingdom"))  %>%
  group_by(year, country, sub_region, gear_type, species) %>%
  summarise(TON=sum(as.numeric(as.character(total_live_weight_landed)), na.rm=T))


#TRAWLS
FDI_LAN_HER_TRAWL <- FDI_LAN_HER[FDI_LAN_HER$gear_type %in% c("OTM", "PTM", "OTB", "PTB", "OTT", "TBB", "DRB"),]
FDI_LAN_HER_TRAWL_5_tons <- FDI_LAN_HER_TRAWL[FDI_LAN_HER_TRAWL$TON>5,]

#sort(unique(FDI_LAN_HER_over_5_tons$sub_region))
#FDI_LAN_HER_TRAWL_5_tons <- FDI_LAN_HER_TRAWL_5_tons %>% arrange(desc(year))
sort(unique(FDI_LAN_HER_TRAWL_5_tons$year), decreasing = TRUE)

#PLOT diagrams for gear = all TRAWLS
ggplot(FDI_LAN_HER_TRAWL_5_tons, aes(x=sub_region, y=TON)) +
  geom_bar(fill="#0073C2FF",stat="identity") +
  theme_bw() +  facet_grid(year~country) +
  labs(x="Area", y="Ton herring landed") +
  theme(axis.text.x = element_text(size=8, angle=90), plot.title=element_text(size=8), 
        axis.title = element_text(size=8)) + ggtitle("Herring TRAWL catches by Country and subregion")

#FYKES
FDI_LAN_HER_FYKE <- FDI_LAN_HER[FDI_LAN_HER$gear_type %in% c("FPN", "FPO", "FYK"),]
FDI_LAN_HER_FYKE_5_tons <- FDI_LAN_HER_FYKE[FDI_LAN_HER_FYKE$TON>5,]

#sort(unique(FDI_LAN_HER_over_5_tons$sub_region))
sort(unique(FDI_LAN_HER_FYKE_5_tons$year))

#PLOT diagrams for gear = all FYKES
ggplot(FDI_LAN_HER_FYKE_5_tons, aes(x=sub_region, y=TON)) +
  geom_bar(fill="#0073C2FF",stat="identity") +
  theme_bw() +  facet_grid(year~country) +
  labs(x="Area", y="Ton herring landed") +
  theme(axis.text.x = element_text(size=8, angle=90), plot.title=element_text(size=8), 
        axis.title = element_text(size=8)) + ggtitle("Herring FYKE catches by Country and subregion")


#GILLNETS
FDI_LAN_HER_GILNETS <- FDI_LAN_HER[FDI_LAN_HER$gear_type %in% c("GNS", "GND", "GTR"),]
FDI_LAN_HER_GILNETS_5_tons <- FDI_LAN_HER_GILNETS[FDI_LAN_HER_GILNETS$TON>5,]

#sort(unique(FDI_LAN_HER_over_5_tons$sub_region))
sort(unique(FDI_LAN_HER_GILNETS_5_tons$year))

#PLOT diagrams for gear = all GILLNETS
ggplot(FDI_LAN_HER_GILNETS_5_tons, aes(x=sub_region, y=TON)) +
  geom_bar(fill="#0073C2FF",stat="identity") +
  theme_bw() +  facet_grid(year~country) +
  labs(x="Area", y="Ton herring landed") +
  theme(axis.text.x = element_text(size=8, angle=90), plot.title=element_text(size=8), 
        axis.title = element_text(size=8)) + ggtitle("Herring GILLNET catches by Country and subregion")
