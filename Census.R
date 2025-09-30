#Date Created: 9/23/2025 
#Created by: Lena Wang
#This evaluates census data, this analysis is included in the last sentence of study area, no plot or table was included into the paper
library("dplyr")
library("tidyr")
library("ggplot2")
library("stringr")
library("data.table")
library("scales")
library("lubridate")
library("ggnewscale")
library("ggpubr")
library("gridExtra")
library("ggpmisc")
library("ggrepel")
library("stringr")
library("sp")
library("sf")

setwd("~/Oregon_GWMA/Oregon_GWMA_git")
Well_census <- read.table("Well_Estimates_2020_Blks_OLD.txt", header = TRUE, sep = "\t")
Corvallis <- read.csv("Corvallis_bc.csv", header = T)
Coburg <- read.csv("Coburg_bc.csv", header = T)
Junction <- read.csv("Junction_City.csv", header = T)
Monroe <- read.csv("Monroe_bc.csv", header = T)
Harrisburg <- read.csv("Harrisburg_bc.csv", header = T)
#Creating df that gives us a column for each city
Corvallis$City <- c("Corvallis")
Coburg$City <- c("Coburg")
Junction$City <- c("Junction City")
Monroe$City <- c("Monroe")
Harrisburg$City <- c("Harrisburg")

#########These give the Population Served and Total Population throughout the entire county
#Binding them all together
All <- rbind(Corvallis, Coburg, Junction, Monroe, Harrisburg) %>%
  dplyr::select(c(FULLCODE, City)) %>%
  rename("GEOID_Blk" = "FULLCODE")
  
All$GEOID_Blk <- as.numeric(All$GEOID_Blk)

#Selecting just the two columns of interest
Well_census2 <- Well_census %>%
  dplyr::select(GEOID_Blk, Est_Wells_2020, HU_2020)

#Joining water data and block data, separating out rural and urban
County_census <- left_join(Well_census2, All) %>%
  dplyr::select(c(Est_Wells_2020, GEOID_Blk, City, HU_2020)) %>%
  mutate(City = replace_na(City, "Rural"))

#had to do gsub because there were numbers like 1,000
County_census$Est_Wells_2020 <- as.numeric(gsub(",","",County_census$Est_Wells_2020))



#Pulling out the Census Blocks that are just within the GWMA
GWMA <- read_sf("./Groundwater_Management_Area_So_Willamette_Valley/So_Willamette_Valley_GWMA.shp")
Census2 <- read_sf("./CensusBlocks/CensusBlocks.shp")

#Pulling out the center point of each block
centroids <- st_centroid(Census2)
#Determining if the center point of each block is within the GWMA shapefile
Census2$within <- st_within(centroids, GWMA, sparse = FALSE)

#Keeping only true observations
Census_filter <- Census2 %>%
  subset(within %in% "TRUE")

#Keeping only the GEOID20 and geometry
Census_geoid <- Census_filter %>%
  dplyr::select(GEOID20, POP20) %>%
  rename("GEOID_Blk" = "GEOID20")


#Converting into a dataframe
GWMA_blk_df <- Census_geoid %>% st_drop_geometry()
GWMA_blk_df$GEOID_Blk <- as.numeric(GWMA_blk_df$GEOID_Blk)

#Filtering the Joined so we only pull out block numbers within the GWMA
GWMA_census <- left_join(GWMA_blk_df, County_census, by = "GEOID_Blk")


GWMA_census_county <- GWMA_census %>%
  mutate(County_num = str_sub(GEOID_Blk, 3, 5)) %>%
  mutate(County = case_when(grepl("003", County_num) ~ "Benton",
                            grepl("039", County_num) ~ "Lane",
                            grepl("043", County_num) ~ "Linn")) %>%
  mutate(Estimated_pop = round((POP20/HU_2020) * Est_Wells_2020))





#Calculating city total and rural total
City <- GWMA_census_county %>%
  group_by(County, City) %>%
  mutate(`Estimated Total Population` = sum(POP20, na.rm = T),
         `Estimated Population Served by Wells` = sum(Estimated_pop, na.rm = T)) %>%
  distinct(County, `Estimated Total Population`, `Estimated Population Served by Wells`)

write.csv(City, "Census_City.csv")

