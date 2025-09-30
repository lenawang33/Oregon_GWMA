#Date Created 08/23/2025
# Created by: Lena Wang
#This code completes the regional Mann Kendall analysis, using the NADA2 package. As of 09/23/2025, the NADA2 package 
#is no longer available in CRAN. Results from this code are in Table 2

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
library("rkt")
install.packages('NADA2')


setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Oregon_GWMA")
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
    #Dividing the columns by the sum and Mixingplying by 100
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
                                grepl("MISC_VEGS___FRUITS", Variable) ~ "Miscellaneous"),
         #Converting pi*0.5^2km to hectare
         Area = 194)

# Nitrate Application Dataframe----------------------------------------------
Nitrogen_app_s <- Nitrogen_app %>% dplyr::select(Land_Use, Low_lb_kg.ha, High_kg_ha) %>%
  rename("Variable" = "Land_Use")

Crop_n_app <- left_join(Long_crop, Nitrogen_app_s)

Crop_app <- Crop_n_app %>%
  mutate(`Low N (kg/ha)` = Low_lb_kg.ha * (Value/100),
         `High N (kg/ha)` = High_kg_ha * (Value/100)) %>%
  rename("Location" = "LOCATION")

#Summed up amounts of Nitrogen inputs over the years

N_app_year <- Crop_app %>% 
  group_by(Location, Year) %>%
  summarise(`Low Nitrogen (kg/ha)` = sum(`Low N (kg/ha)`, na.rm = T),
            `High Nitrogen (kg/ha)` = sum(`High N (kg/ha)`, na.rm = T)) %>%
  mutate(Year = as.Date(Year, format = "%Y")) %>%
  ungroup() %>%
  mutate(Cafos = case_when(Location %in% Cafos ~ "CAFOS",
                           .default = "No CAFOS"))
  


Nitrate_well$Year <- as.Date(Nitrate_well$Year, format = "%Y")



N_leach_app <- left_join(Nitrate_well, N_app_year)

Location_list <- N_leach_app %>%
  distinct(Location, N_Classification) %>%
  drop_na()

MK_AN <- data.frame(matrix(nrow = 0, ncol = 0))

Filter <- N_leach_app
#subset(Location %in% c(paste(Location_list[[i, 1]])))
Filter <- Filter %>%
  mutate(Date =  as.numeric(format(Year, format = "%Y")),
         Location_a = case_when(grepl("GW", Location) ~ paste0(Location, "1"),
                                grepl("DW", Location) ~ paste0(Location, "2")),
         Location_b = case_when(grepl("GW", Location) ~ paste0("GW"),
                                grepl("DW", Location) ~ paste0("DW")))



#Regular Mann Kendall with just the DW and GW Wells
GW <- Filter %>%
  subset(`Location_b` %in% c("GW"))

GW_mk <- MannKendall(GW$`Low Nitrogen (kg/ha)`)

DW <- Filter %>%
  subset(`Location_b` %in% c("DW"))

DW_mk <- MannKendall(DW$`Low Nitrogen (kg/ha)`)


#Spatial Mann-Kendall Analysis in R 
Filter$Location_a <- gsub("G","",as.character(Filter$Location_a))
Filter$Location_a <- gsub("W", "", as.character(Filter$Location_a))
Filter$Location_a <- gsub("D", "", as.character(Filter$Location_a))
Filter$Location_a <- gsub("a", "", as.character(Filter$Location_a))
Filter$Location_a <- gsub("R", "", as.character(Filter$Location_a))
Filter$Location_a <- gsub("S", "", as.character(Filter$Location_a))
Filter$Location_a <- gsub("-", "", as.character(Filter$Location_a))
Filter$Location_a <- as.numeric(Filter$Location_a)
Filter$y.cen <- 0
Filter$x.cen <- 0

kendall_L <- censeaken(time = Filter$Date, y = Filter$`Low Nitrogen (kg/ha)`, y.cen = Filter$y.cen, x.cen = Filter$x.cen, group = Filter$Location)

kendall_H <- censeaken(time = Filter$Date, y = Filter$`High Nitrogen (kg/ha)`, y.cen = Filter$y.cen, x.cen = Filter$x.cen, group = Filter$Location)

kendall_N <- censeaken(time = Filter$Date, y = Filter$`Nitrate (mg/L)`, y.cen = Filter$y.cen, x.cen = Filter$x.cen, group = Filter$Location)


Filter <- Filter %>%
  mutate(`Well Classification` = case_when(N_Classification == "Stable/Leaching" ~ "Leaching",
                                           N_Classification == "Stable/Mixing" ~ "Mixing",
                                           N_Classification != "Stable/Leaching" ~ N_Classification,
                                           N_Classification != "Stable/Mixing" ~ N_Classification)) %>%
  drop_na(`Well Classification`)


Leaching <- Filter %>%
  subset(`Well Classification` %in% c("Leaching"))

L_kendall_L <- censeaken(time = Leaching$Date, y = Leaching$`Low Nitrogen (kg/ha)`, y.cen = Leaching$y.cen, x.cen = Leaching$x.cen, group = Leaching$Location)

L_kendall_H <- censeaken(time = Leaching$Date, y = Leaching$`High Nitrogen (kg/ha)`, y.cen = Leaching$y.cen, x.cen = Leaching$x.cen, group = Leaching$Location)

L_kendall_N <- censeaken(time = Leaching$Date, y = Leaching$`Nitrate (mg/L)`, y.cen = Leaching$y.cen, x.cen = Leaching$x.cen, group = Leaching$Location)


Mixing <- Filter %>%
  subset(`Well Classification` %in% c("Mixing"))

Mi_kendall_L <- censeaken(time = Mixing$Date, y = Mixing$`Low Nitrogen (kg/ha)`, y.cen = Mixing$y.cen, x.cen = Mixing$x.cen, group = Mixing$Location)

Mi_kendall_H <- censeaken(time = Mixing$Date, y = Mixing$`High Nitrogen (kg/ha)`, y.cen = Mixing$y.cen, x.cen = Mixing$x.cen, group = Mixing$Location)

Mi_kendall_N <- censeaken(time = Mixing$Date, y = Mixing$`Nitrate (mg/L)`, y.cen = Mixing$y.cen, x.cen = Mixing$x.cen, group = Mixing$Location)


Multi <- Filter %>%
  subset(`Well Classification` %in% c("Multi-Proc"))

Mu_kendall_L <- censeaken(time = Multi$Date, y = Multi$`Low Nitrogen (kg/ha)`, y.cen = Multi$y.cen, x.cen = Multi$x.cen, group = Multi$Location)

Mu_kendall_H <- censeaken(time = Multi$Date, y = Multi$`High Nitrogen (kg/ha)`, y.cen = Multi$y.cen, x.cen = Multi$x.cen, group = Multi$Location)

Mu_kendall_N <- censeaken(time = Multi$Date, y = Multi$`Nitrate (mg/L)`, y.cen = Multi$y.cen, x.cen = Multi$x.cen, group = Multi$Location)


Stable <- Filter %>%
  subset(`Well Classification` %in% c("Stable"))

S_kendall_L <- censeaken(time = Stable$Date, y = Stable$`Low Nitrogen (kg/ha)`, y.cen = Stable$y.cen, x.cen = Stable$x.cen, group = Stable$Location)

S_kendall_H <- censeaken(time = Stable$Date, y = Stable$`High Nitrogen (kg/ha)`, y.cen = Stable$y.cen, x.cen = Stable$x.cen, group = Stable$Location)

S_kendall_N <- censeaken(time = Stable$Date, y = Stable$`Nitrate (mg/L)`, y.cen = Stable$y.cen, x.cen = Stable$x.cen, group = Stable$Location)


Dilution <- Filter %>%
  subset(`Well Classification` %in% c("Dilution"))

D_kendall_L <- censeaken(time = Dilution$Date, y = Dilution$`Low Nitrogen (kg/ha)`, y.cen = Dilution$y.cen, x.cen = Dilution$x.cen, group = Dilution$Location)

D_kendall_H <- censeaken(time = Dilution$Date, y = Dilution$`High Nitrogen (kg/ha)`, y.cen = Dilution$y.cen, x.cen = Dilution$x.cen, group = Dilution$Location)

D_kendall_N <- censeaken(time = Dilution$Date, y = Dilution$`Nitrate (mg/L)`, y.cen = Dilution$y.cen, x.cen = Dilution$x.cen, group = Dilution$Location)




#Combining the Mann Kendalls into a dataframe


kendall_N$Group <- c("All")
L_kendall_N$Group <- c("Leaching")
Mi_kendall_N$Group <- c("Mixing")
Mu_kendall_N$Group <- c("Multi_Process")
S_kendall_N$Group <- c("Stable")
D_kendall_N$Group <- c("Dilution")

N_join <- list(kendall_N, L_kendall_N, Mi_kendall_N, Mu_kendall_N, S_kendall_N, D_kendall_N)
List <- Reduce(function(x, y) merge(x, y, all = TRUE), N_join)
List$Type <- c("Nitrate")



kendall_H$Group <- c("All")
L_kendall_H$Group <- c("Leaching")
Mi_kendall_H$Group <- c("Mixing")
Mu_kendall_H$Group <- c("Multi_Process")
S_kendall_H$Group <- c("Stable")
D_kendall_H$Group <- c("Dilution")




A_join <- list(kendall_H, L_kendall_H, Mi_kendall_H, Mu_kendall_H, S_kendall_H, D_kendall_H)
App_list <- Reduce(function(x, y) merge(x, y, all = TRUE), A_join)
App_list$Type <- c("Fertilizer Application")
App_N <- rbind(List, App_list)

write.csv(App_N, "Regional_Mann_Kendall_Nitrate_Application.csv")