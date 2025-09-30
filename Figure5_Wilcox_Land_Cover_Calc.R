#Date Created 9/23/2025
#Created by Lena Wang
#This code calculats land cover and produces Figure 5 using Wilcoxon test

library("dplyr")
library("tidyr")
library("ggplot2")
library("stringr")
library("data.table")
library("scales")
library("lubridate")
library("ggnewscale")
library("vistime")
library("ggpubr")
library("grid")
library("gridExtra")
library("vars")
library("corrplot")
library("Kendall")
library("ggpmisc")
library("ggrepel")
library("lemon")


setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Oregon_GWMA/Oregon_GWMA_Git")
#reads in all the years of data
`2008` <- read.csv("Crop_perc2008.csv", header = T)
`2009` <- read.csv("Crop_perc2009.csv", header = T)
`2010` <- read.csv("Crop_perc2010.csv", header = T)
`2011` <- read.csv("Crop_perc2011.csv", header = T)
`2012` <- read.csv("Crop_perc2012.csv", header = T)
`2013` <- read.csv("Crop_perc2013.csv", header = T)
`2014` <- read.csv("Crop_perc2014.csv", header = T)
`2015` <- read.csv("Crop_perc2015.csv", header = T)
`2016` <- read.csv("Crop_perc2016.csv", header = T)
`2017` <- read.csv("Crop_perc2017.csv", header = T)
`2018` <- read.csv("Crop_perc2018.csv", header = T)
`2019` <- read.csv("Crop_perc2019.csv", header = T)
`2020` <- read.csv("Crop_perc2020.csv", header = T)
`2021` <- read.csv("Crop_perc2021.csv", header = T)
`2022` <- read.csv("Crop_perc2022.csv", header = T)
`2023` <- read.csv("Crop_perc2023.csv", header = T)

#importing GWAMA Nitrate Data
Nitrate <- read.csv("GWAMA_nitrate_data_Wang_20250204.csv", header = T)
Wells <- read.csv("Well_classifications.csv", header = T)
Nitrogen_app <- read.csv("GWAMA_Crop_Nutrient_Code.csv", header = T)
Ending <- read.csv("Last3_GWMA.csv", header = T)

#Importing Fertilizer Purchase History normalized by Land Cover
Purchase <- read.csv('Fertilizer_purchase_trends.csv', header = T)

# Manipulating data for Nitrate Mann Kendall ------------------------------
Nitrate_rename <- Nitrate %>%
  rename("Location" = "Loc_ID",
         "Nitrate (mg/L)" = "Nitrate")

Nitrate_well <- left_join(Nitrate_rename, Wells) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         Year = paste(format(`Date`, "%Y"))) %>%
  filter(Year >= 2008) %>%
  dplyr::select(-c(Date)) #Wells DW-7 and GW-8 don't have a well classification, so they get filtered out


Nitrate_well_list <- Nitrate_well %>% 
  distinct(Location, N_Classification)

# Crop Application Rates --------------------------------------------------
#This is getting all the crop cover data manipulated together
df_list <- list(`2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`,
                `2018`, `2019`, `2020`, `2021`, `2022`, `2023`)

years <- list("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017",
              "2018", "2019", "2020", "2021", "2022", "2023")

#Making 2008 data numeric, not integer
`2008`[,3:length(`2008`)] <- as.numeric(unlist(`2008`[,3:length(`2008`)]))
#Converting 2008 data from pixels to percentages, pivoting long so I can join all years together
Long_crop <- `2008` %>% mutate(Sum = rowSums(`2008`[,3:length(`2008`)])) %>%
  mutate(
    across(c(3:length(`2008`)),
           .fns = ~./Sum) * 100) %>%
  pivot_longer(, cols = c(3:length(`2008`)), names_to = "Variable", values_to = "Value") %>%
  mutate(Year = "2008")

#iterating through all the data, except 2008 data which is set for
for (i in 1:length(df_list)) {
  #simplifying df_list[[i]] to just df so fewer typos. 
  df <- df_list[[i]]
  `df`[,3:length(`df`)] <- as.numeric(unlist(`df`[,3:length(`df`)]))
  #Converting all data from pixels to percentages, pivoting long
  #calculating the sum
  `df long` <- `df` %>% mutate(Sum = rowSums(`df`[,3:length(`df`)])) %>%
    #Dividing the columns by the sum and multiplying by 100
    mutate(
      across(c(3:length(`df`)),
             .fns = ~./Sum) * 100) %>%
    #turning into long format, where Crops go to variable and percentages go to value
    pivot_longer(, cols = c(3:length(`df`)), names_to = "Variable", values_to = "Value") %>%
    #adding a column for the yeras
    mutate(Year = paste(years[i]))
  
  #binding the looped output together
  Long_crop <- rbind(Long_crop, `df long`) %>% filter(Value != 0)
  
}

Crop_names <- Long_crop %>% distinct(Variable)

#Grouping Crop Types
Forest <- c("EVERGREEN.FOREST", "EVERGREEN_FOREST", "DECIDIUOUS.FOREST", "DECIDUOUS_FOREST", "MIXED_FOREST", "FOREST", "SHRUBLAND")

Developed <- c("DEVELOPED.HIGH.INTENSITY", "DEVELOPED.LOW.INTENSITY", "DEVELOPED.MED.INTENSITY", "DEVELOPED.OPEN.SPACE", "DEVELOPED_HIGH_INTENSITY",
               "DEVELOPED_LOW_INTENSITY", "DEVELOPED_MED_INTENSITY", "DEVELOPED_OPEN_SPACE")

Legumes <- c("DRY_BEANS", "CHICK_PEAS", "LENTILS", "PEAS", "ALFALFA", "VETCH")

Corn <- c("CORN", "SWEET.CORN", "SWEET_CORN")

Grains <- c("BARLEY", "FLAXSEED", "BUCKWHEAT", "OATS", "SORGHUM", "RYE",
            "WINTER_WHEAT", "DBL_CROP_WINWHT_CORN", "SPRING_WHEAT", "TRITICALE")

Fallow <- c("FALLOW.IDLE.CROPLAND", "FALLOW_IDLE_CROPLAND", "BARREN")

Grass_Hay <- c("SOD.GRASS.SEED", "SOD_GRASS_SEED","GRASSLAND.PASTURE", "GRASSLAND_PASTURE", "OTHER.HAY.NON.ALFALFA", "OTHER_HAY_NON_ALFALFA")

Squash <- c("CUCUMBERS", "PUMPKINS", "SQUASH")

Mint <- c("MINT")

Misc <- c("GRAPES", "GREENS", "HERBS", "HOPS", "MISC_VEGS_._FRUITS", " MISC_VEGS___FRUITS ",
          "POTATOES", "PEPPERS", "SUGARBEETS", "OTHER_CROPS",
          "GARLIC", "ONIONS","CLOVER.WILDFLOWERS", "CLOVER_WILDFLOWERS",
          "BROCCOLI", "CABBAGE", "RADISHES", "TURNIPS", "MUSTARD", "CANOLA", "CAULIFLOWER",
          "CHRISTMAS_TREES", "OTHER_TREE_CROPS", "WALNUTS", 
          "CANEBERRIES", "STRAWBERRIES", "CRANBERRIES")

Orchard_Crops <- c("APPLES", "CHERRIES", 'PEACHES', "PEARS", "PLUMS", "PRUNES")

Wetlands <- c("HERBACEOUS_WETLANDS", "WETLANDS", "WOODY_WETLANDS")

Blueberries <- c("BLUEBERRIES")

Open_Water <- c("OPEN_WATER")

#Separating out worsening wells
Worsening_wells <- c("DW-1544", "GW-21", "GW-20", "GW-12","DW-8", "GW-3", "GW-18", "GW-5", "GW-1", "GW-17", "GW-7", "DW-3", "GW-15", "DW-9", "DW-10", 'GW-8', "GW-4D", "DW-17", "DW-1524")
Improving_wells <- c("DW-1525", "DW-6", "GW-9", "GW-27", "GW-4S", "DW-16", "DW-15", "GW-13", "DW-1524")
No_change_wells <- c("GW-22", "GW-19", "DW-13", "DW-12a", "DW-5", "GW-6", "GW-10", "DW-7a", "DW-7", "GW-8R", "GW-25", "GW-24", "DW-11", "GW-11", "GW-16", "DW-2", "DW-1", "GW-23")
Cafos <- c("DW-10", "GW-12", "GW-3")


Long_crop <- Long_crop %>%
  mutate(Group_Crop = case_when(Variable %in% Forest ~ "Forest",
                                Variable %in% Blueberries ~ "Blueberries",
                                Variable %in% Developed ~ "Developed",
                                Variable %in% Legumes ~ "Legumes",
                                Variable %in% Corn ~ "Corn",
                                Variable %in% Squash ~ "Squash",
                                Variable %in% Grains ~ "Grains",
                                Variable %in% Fallow ~ "Fallow",
                                Variable %in% Orchard_Crops ~ "Fruit and Nut Trees",
                                Variable %in% Mint ~ "Mint",
                                Variable %in% Misc ~ "Miscellaneous",
                                Variable %in% Wetlands ~ "Wetlands",
                                Variable %in% Open_Water ~ "Open Water",
                                Variable %in% Grass_Hay ~ "All Grasses and Hay",
                                #This final line of code to catch the few variables that aren't categorizing for some reason
                                grepl("MISC_VEGS___FRUITS", Variable) ~ "Miscellaneous"))

Long_crop2 <- Long_crop %>%
  mutate(Groups = case_when(Variable %in% Forest ~ "Natural Lands",
                                Variable %in% Blueberries ~ "Agricultural",
                                Variable %in% Developed ~ "Other",
                                Variable %in% Legumes ~ "Agricultural",
                                Variable %in% Corn ~ "Agricultural",
                                Variable %in% Squash ~ "Agricultural",
                                Variable %in% Grains ~ "Agricultural",
                                Variable %in% Fallow ~ "Other",
                                Variable %in% Orchard_Crops ~ "Agricultural",
                                Variable %in% Mint ~ "Agricultural",
                                Variable %in% Misc ~ "Agricultural",
                                Variable %in% Wetlands ~ "Natural Lands",
                                Variable %in% Open_Water ~ "Natural Lands",
                                Variable %in% Grass_Hay ~ "Agricultural",
                                #This final line of code to catch the few variables that aren't categorizing for some reason
                                grepl("MISC_VEGS___FRUITS", Variable) ~ "Agricultural")) %>%
  rename("Location" = "LOCATION")

Crop_ending <- left_join(Long_crop2, Ending, by = "Location") %>%
  drop_na(End.Nitrate.Concentration)

Final <- Crop_ending %>%
  subset(Year %in% "2023") %>%
  group_by(Groups, Location) %>%
  mutate(Total = sum(Value)) %>% #Value is already in percentages
  ungroup() %>%
  distinct(Location, Total, End.Nitrate.Concentration, Groups)


Final_wide <- Final %>%
  pivot_wider(values_from = Total, names_from = End.Nitrate.Concentration)

land_use <- list("Agricultural", "Natural Lands", "Other")

wilcox <- data.frame(matrix(nrow = 0, ncol = 0))
for (i in 1:length(concentrations)) {
  veg <- Final_wide %>%
    subset(Groups %in% paste0(land_use[i]))
  
  
  high <- wilcox.test(veg$`> 10 mg-N/L End`, veg$`7-10 mg-N/L End`, exact = FALSE)
  medium <- wilcox.test(veg$`< 7 mg-N/L End`, veg$`7-10 mg-N/L End`, exact = FALSE)
  low  <- wilcox.test(veg$`< 7 mg-N/L End`, veg$`> 10 mg-N/L End`, exact = FALSE)
  
  wilcox[i, "Land_use"] <- paste0(land_use[i])
  wilcox[i, "> 10 mg-N/L End, 7-10 mg-N/L End"] <- high[["p.value"]]
  wilcox[i, "< 7 mg-N/L En, 7-10 mg-N/L End"] <- medium[["p.value"]]
  wilcox[i, "< 7 mg-N/L En, > 10 mg-N/L"] <- low[["p.value"]]
  
}
