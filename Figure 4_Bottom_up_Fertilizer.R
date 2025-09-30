#Date Created 9/23/2025
#Created by: Lena Wang
#This code creates line plots, Specifically Figure 5
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
library(nlme)
library(lme4)
library("plotrix")
library("sf")
#Change the working directory to acces the scripts
setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Oregon_GWMA/Oregon_GWMA_Git")
#reads Crop years
Crop <- read.csv("GWMA_Crop.csv", header = T)
Crop_distinct <- Crop %>% 
  distinct(Crop.Type)

#importing GWAMA Nitrate Data
Nitrate <- read.csv("GWAMA_nitrate_data_Wang_20250204.csv", header = T)
Wells <- read.csv("Well_classifications.csv", header = T)
Nitrogen_app <- read.csv("All_GWMA_Crops_Fertilizer.csv", header = T)
Class_col <- c("Dilution" = "#15BB98", "Leaching" = "#FFA507", "Mixing" = "#5F1657", "Multi-Proc" = "#D81B60",
               "Stable" = "#1E88E5", "Stable/Leaching" =  "#FFC107", "Stable/Mixing" = "#DC69B8")

#importing additional .csv that will help categorization
#This was written out from GWMA Isotope.R script
Well_properties <- read.csv("Well Properties.csv", header = T)


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

#Crop Percentage
Crop_perc <- Crop %>%
  group_by(Year) %>%
  mutate(Sum = sum(Pixels)) %>%
  ungroup() %>%
  mutate(Percentage = Pixels/Sum)


#Grouping Crop Types
Forest <- c("Deciduous Forest", "Evergreen Forest", "Mixed Forest", "Forest", "Shrubland")

Developed <- c("Developed/Open Space", "Developed/Low Intensity", "Developed/Med Intensity", "Developed/Mid Intensity", "Developed/High Intensity")

Legumes <- c("Dry Beans", "Soybeans", "Vetch", "Peas", "Chick Peas", "Alfalfa", "Lentils")

Corn <- c("Corn", "Sweet Corn")

Grains <- c("Barley", "Flaxseed", "Buckwheat", "Oats", "Dbl Crop Oats/Corn", "Sorghum", "Dbl Crop WinWht/Sorghum", "Rye", "Winter Wheat",
            "Spring Wheat", "Triticale", "Dbl Crop Triticale/Corn", "Dbl Crop WinWht/Corn", "Speltz")

Fallow <- c("Fallow/Idle Cropland", "Barren")

Grass_Hay <- c("Sod/Grass Seed", "Grassland/Pasture", "Other Hay/Non Alfalfa")

Squash <- c("Squash", "Pumpkins", "Cucumbers") 

Mint <- c("Mint")

Misc <- c("Grapes", "Greens", "Herbs", "Hops", "Potatoes", "Peppers", "Sugarbeets", "Garlic", "Onions", "Clover/Wildflowers",
          "Broccoli", "Cabbage", "Radishes", "Turnips", "Mustard", "Canola", "Cauliflower", "Christmas Trees", "Other Tree Crops",
          'Walnuts', "Caneberries", "Strawberries", "Cranberries", "Craneberries", "Other Crops", "Misc Vegs & Fruits", "Rape Seed", "Carrots", "Mustard",
          "Camelina", "Sunflower")

Orchard_Crops <- c("Apples", "Cherries", "Peaches", "Pears", "Plums", "Prunes")

Wetlands <- c("Woody Wetlands", "Herbaceous Wetlands", "Wetlands")

Blueberries <- c("Blueberries")

Open_Water <- c("Open Water")




Crop_Group <- Crop_perc %>%
  mutate(Group_Crop = case_when(Crop.Type %in% Forest ~ "Forest",
                                Crop.Type %in% Blueberries ~ "Blueberries",
                                Crop.Type %in% Developed ~ "Developed",
                                Crop.Type %in% Legumes ~ "Legumes",
                                Crop.Type %in% Corn ~ "Corn",
                                Crop.Type %in% Squash ~ "Squash",
                                Crop.Type %in% Grains ~ "Grains",
                                Crop.Type %in% Fallow ~ "Fallow",
                                Crop.Type %in% Orchard_Crops ~ "Fruit and Nut Trees",
                                Crop.Type %in% Mint ~ "Mint",
                                Crop.Type %in% Misc ~ "Miscellaneous",
                                Crop.Type %in% Wetlands ~ "Wetlands",
                                Crop.Type %in% Open_Water ~ "Open Water",
                                Crop.Type %in% Grass_Hay ~ "All Grasses and Hay",
                                #This final line of code to catch the few Crop.Types that aren't categorizing for some reason
                                grepl("MISC_VEGS___FRUITS", Crop.Type) ~ "Miscellaneous"),
         #Converting pi*0.5^2km to hectare
         Area = 194) %>%
  rename("Crop" = "Crop.Type")

Nitrogen_app2 <- Nitrogen_app %>%
  dplyr::select(Crop, Low_lbN.acre, High_lbN.acre)

Crop_Fert <- left_join(Crop_Group, Nitrogen_app2, by = "Crop", relationship = "many-to-many")

Crop_applied <- Crop_Fert %>%
  mutate(High = Percentage * High_lbN.acre * 1.12,
         Low = Percentage * Low_lbN.acre * 1.12) %>%
  group_by(Year) %>%
  mutate("Total High" = sum(High),
         "Total Low" = sum(Low),
         "Total Medium" = sum(High - (High-Low)/2)) %>%
  distinct(Year, `Total High`, `Total Low`, `Total Medium`)

Crop_long <- pivot_longer(Crop_applied, cols = c("Total High", "Total Low", "Total Medium"), names_to = "Recommendation", values_to = "Amount") %>%
  mutate(Recommendation = factor(Recommendation, levels = c("Total High", "Total Medium", "Total Low")))

Color = c("Total High" = "#a31a1a", "Total Medium" = "#1008C3", "Total Low" = "#42BB63")
Applied_plot <- ggplot(Crop_long) +
  geom_line(aes(x = Year, y = Amount, color = `Recommendation`), linewidth = 2) +
  scale_color_manual(values = Color) +
  ylab("Estimated Applied Nitrogen kg/ha/yr") + 
  ggtitle("Estimated Applied Nitrogen from 2008-2023 Based on \n Land Cover and Fertilizer Recommendations") +
  theme_bw()+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25))










Applied_plot <- ggplot(Crop_applied) +
  geom_line(aes(x = Year, y = `Total High`), color = "#a31a1a", linewidth = 2) +
  geom_line(aes(x = Year, y = `Total Medium`), color = "#42BB63", linewidth = 2) +
  geom_line(aes(x = Year, y = `Total Low`), color = "#1008C3", linewidth = 2) +
  guides(fill = guide_legend(title = "Fertilizer  Recommendation (kg/ha)")) +
  #scale_x_continuous(breaks = seq(2006, 2017, 1)) +
  ylab("Estimated Applied Nitrogen kg/ha") + 
  ggtitle("Estimated Applied Nitrogen from 2008-2023 Based on \n Land Cover and Fertilizer Recommendations") +
  theme_bw()+ 

  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25))


