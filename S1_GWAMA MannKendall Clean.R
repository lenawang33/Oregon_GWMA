#Date Created: 09/23/2025
# Created by: Lena Wang
# This code calculates the Mann Kendall of all the Wells within the SWV-GWMA, it produces the plot S1 in the paper. 

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

#Trends for Nitrate and Crop Data 
for (i in 1:nrow(Location_list)){
  Filter <- N_leach_app %>% 
    subset(Location %in% c(paste(Location_list[[i, 1]])))
  Nitrate <- Filter$`Nitrate (mg/L)`
  
  MK_AN[i, "Location"] <- Location_list[[i,1]]
  MK_AN[i, "N_Classification"] <- Location_list[[i, 2]]
  
  
  `Low Nitrogen (kg/ha)` <- Filter$`Low Nitrogen (kg/ha)`
  
  `High Nitrogen (kg/ha)`<- Filter$`High Nitrogen (kg/ha)`
  
  kendall_L <- MannKendall(`Low Nitrogen (kg/ha)`)
  
  kendall_H <- MannKendall(`High Nitrogen (kg/ha)`)
  
  kendall_N <- MannKendall(Nitrate)
  

  
  MK_AN[i, "Low N Tau"] <- kendall_L[["tau"]]
  MK_AN[i, "Low N pvalue"] <- kendall_L[["sl"]]
  
  MK_AN[i, "High N Tau"] <- kendall_H[["tau"]]
  MK_AN[i, "High N pvalue"] <- kendall_H[["sl"]]
  
  MK_AN[i, "Nitrate_Tau"] <- kendall_N[["tau"]]
  MK_AN[i, "Nitrate_pvalue"] <- kendall_N[["sl"]]
  MK_AN[i, "Max"] <- max(Nitrate)
}

write.csv(MK_AN, "Well_Categories_MannKendall_Napplication.csv")

# Linear Regressions for Plot Labels -------------------------------------------------------------------
#Analyzing the application of significant nitrate trends. 
Nitrate_LL <- MK_AN %>%
  distinct(Location, N_Classification)

#Application Linear Regression, help label the following plots
Application_lm <- data.frame()

for (i in 1:nrow(Nitrate_LL)){
  Application_filt <- N_leach_app %>% 
    subset(Location %in% c(paste(Nitrate_LL[[i, 1]])))
  Application_lm1 <- lm(`High Nitrogen (kg/ha)` ~ Year, 
                        data = Application_filt)
  Application_lm[i, "Application"] <- Application_lm1[["fitted.values"]][length(Application_lm1[["fitted.values"]])]
  Application_lm[i, "Date"] <- Application_lm1[["model"]][nrow(Application_lm1[["model"]]), 2]
  Application_lm[i, "Location"] <- Nitrate_LL[[i, 1]]
}

#Analyzing the nitrate trends of significant application trends, help label the following plots
Application_LL <- MK_AN %>%
  distinct(Location, N_Classification)

#Nitrate Linear Regression  
Nitrate_lm <- data.frame()
for (i in 1:nrow(Application_LL)){
  N_leach_filt <- N_leach_app %>% 
    subset(Location %in% c(paste(Application_LL[[i, 1]])))
  Nitrate_lm1 <- lm(`Nitrate (mg/L)` ~ Year, 
                    data = N_leach_filt)
  Nitrate_lm[i, "Nitrate"] <- Nitrate_lm1[["fitted.values"]][length(Nitrate_lm1[["fitted.values"]])]
  Nitrate_lm[i, "Date"] <- Nitrate_lm1[["model"]][nrow(Nitrate_lm1[["model"]]),2]
  Nitrate_lm[i, "Location"] <- Application_LL[[i,1]]
}



# Plots -------------------------------------------------------------------
#Assigning colors to each  Well Group
Class_col <- c("Dilution" = "#15BB98", "Leaching" = "#FFA507", "Mixing" = "#5F1657", "Multi-Proc" = "#D81B60",
               "Stable" = "#1E88E5", "Stable/Leaching" =  "#FFC107", "Stable/Mixing" = "#DC69B8")


# Increasing Nitrate Trends  ----------------------------------------------

#Nitrate Trend
Nitrate_Increase_loc <- MK_AN %>%
  filter(Nitrate_pvalue < 0.2) %>%
  filter(Nitrate_Tau < 0) %>%
  distinct(Location, N_Classification)

Nitrate_Increase_sig <- N_leach_app %>%
  subset(Location %in% Nitrate_Increase_loc[ ,1]) %>%
  mutate(Year = as.Date(Year, format = "%Y")) %>%
  mutate(`Well Classification` = case_when(N_Classification == "Stable/Leaching" ~ "Leaching",
                                           N_Classification == "Stable/Mixing" ~ "Mixing",
                                           N_Classification != "Stable/Leaching" ~ N_Classification,
                                           N_Classification != "Stable/Mixing" ~ N_Classification)) %>%
  drop_na(`Well Classification`) %>%
  drop_na(`High Nitrogen (kg/ha)`)

Nitrate_name_Increase <- Nitrate_lm %>%
  mutate(Year = as.Date(Date, format = "%Y")) %>%
  subset(Location %in% Nitrate_Increase_loc[ ,1])


N_Increase_app <- ggplot(Nitrate_Increase_sig) + 
  geom_point(aes(x = Year, y = `Nitrate (mg/L)`, color =  `Well Classification`), size = 3) +  
  stat_poly_line(aes(x = Year, y = `Nitrate (mg/L)`, group = Location, color = `Well Classification`), se = F, linewidth = 1.5) +
  scale_color_manual(values = Class_col) + 
  theme_bw() +
  # scale_x_date(limits = c(as.Date("2008-01-01"), as.Date("2025-01-01"))) + 
  # geom_text_repel(data = Nitrate_name_Increase, aes(x = Year, y = `Nitrate`, label = Location), 
  #                 nudge_x = .3, nudge_y = .12, color = "black", size = 3, max.overlaps = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(size = 20),
        text = element_text(size = 18)) +
  ylab("Well Nitrate Trend  (mg/L)") +
  labs(title = "Wells with Increasing Nitrate Trends")


#Signif Application in Wells with Decreasing Nitrate Trend
NitrateD_app_sig_loc <- MK_AN %>%
  filter(Nitrate_pvalue < 0.2) %>%
  filter(Nitrate_Tau < 0) %>%
  filter(`High N pvalue` < 0.2) %>%
  distinct(Location, N_Classification)

NitrateD_app_sig <- N_leach_app %>%
  subset(Location %in% NitrateD_app_sig_loc[ ,1]) %>%
  mutate(Year = as.Date(Year, format = "%Y")) %>%
  mutate(`Well Classification` = case_when(N_Classification == "Stable/Leaching" ~ "Leaching",
                                           N_Classification == "Stable/Mixing" ~ "Mixing",
                                           N_Classification != "Stable/Leaching" ~ N_Classification,
                                           N_Classification != "Stable/Mixing" ~ N_Classification)) %>%
  drop_na(`Well Classification`) %>%
  drop_na(`High Nitrogen (kg/ha)`)

Application_name_Increase <- Application_lm %>%
  mutate(Year = as.Date(Date, format = "%Y")) %>%
  subset(Location %in% NitrateD_app_sig_loc[ ,1])


N_Increase_sigapp <- ggplot(NitrateD_app_sig) + 
  geom_point(aes(x = Year, y = `High Nitrogen (kg/ha)`, color =  `Well Classification`), size = 3) +  
  stat_poly_line(aes(x = Year, y = `High Nitrogen (kg/ha)`, group = Location, color = `Well Classification`), se = F, linewidth = 1.5) +
  scale_color_manual(values = Class_col) + 
  theme_bw() +
  scale_x_date(limits = c(as.Date("2008-01-01"), as.Date("2025-01-01"))) + 
  # geom_text_repel(data = Application_name_Increase, aes(x = Year, y = Application, label = Location), 
  #                 nudge_x = .3, nudge_y = .12, color = "black", size = 3, max.overlaps = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(size = 20),
        text = element_text(size = 18)) +
  ylab("Estimated Applied Nitrogen (kg/ha)") +
  labs(title = "Significant Application Trends on Wells \n with Increasing Nitrate Trends")


#Signif Application in Wells with Decreasing Nitrate Trend
NitrateD_app_sig_loc <- MK_AN %>%
  filter(Nitrate_pvalue < 0.2) %>%
  filter(Nitrate_Tau < 0) %>%
  filter(`High N pvalue` < 0.2) %>%
  distinct(Location, N_Classification)

NitrateD_app_sig <- N_leach_app %>%
  subset(Location %in% NitrateD_app_sig_loc[ ,1]) %>%
  mutate(Year = as.Date(Year, format = "%Y")) %>%
  mutate(`Well Classification` = case_when(N_Classification == "Stable/Leaching" ~ "Leaching",
                                           N_Classification == "Stable/Mixing" ~ "Mixing",
                                           N_Classification != "Stable/Leaching" ~ N_Classification,
                                           N_Classification != "Stable/Mixing" ~ N_Classification)) %>%
  drop_na(`Well Classification`) %>%
  drop_na(`High Nitrogen (kg/ha)`)

Application_name_Increase <- Application_lm %>%
  mutate(Year = as.Date(Date, format = "%Y")) %>%
  subset(Location %in% NitrateD_app_sig_loc[ ,1])


N_Increase_sigapp <- ggplot(NitrateD_app_sig) + 
  geom_point(aes(x = Year, y = `High Nitrogen (kg/ha)`, color =  `Well Classification`), size = 3) +  
  stat_poly_line(aes(x = Year, y = `High Nitrogen (kg/ha)`, group = Location, color = `Well Classification`), se = F, linewidth = 1.5) +
  scale_color_manual(values = Class_col) + 
  theme_bw() +
  scale_x_date(limits = c(as.Date("2008-01-01"), as.Date("2025-01-01"))) + 
  # geom_text_repel(data = Application_name_Increase, aes(x = Year, y = Application, label = Location), 
  #                 nudge_x = .3, nudge_y = .12, color = "black", size = 3, max.overlaps = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(size = 20),
        text = element_text(size = 18)) +
  ylab("Estimated Applied Nitrogen (kg/ha)") +
  labs(title = "Significant Application Trends on Wells \n with Increasing Nitrate Trends")

#InSignif Application in Wells with Decreasing Nitrate Trend
NitrateD_app_loc <- MK_AN %>%
  filter(Nitrate_pvalue < 0.2) %>%
  filter(Nitrate_Tau < 0) %>%
  filter(`High N pvalue` > 0.2) %>%
  distinct(Location, N_Classification)

NitrateD_app <- N_leach_app %>%
  subset(Location %in% NitrateD_app_loc [ ,1]) %>%
  mutate(Year = as.Date(Year, format = "%Y")) %>%
  mutate(`Well Classification` = case_when(N_Classification == "Stable/Leaching" ~ "Leaching",
                                           N_Classification == "Stable/Mixing" ~ "Mixing",
                                           N_Classification != "Stable/Leaching" ~ N_Classification,
                                           N_Classification != "Stable/Mixing" ~ N_Classification)) %>%
  drop_na(`Well Classification`) %>%
  drop_na(`High Nitrogen (kg/ha)`)

Application_name_Increase <- Application_lm %>%
  mutate(Year = as.Date(Date, format = "%Y")) %>%
  subset(Location %in% NitrateD_app_loc [ ,1])


N_Increase_nosigapp <- ggplot(NitrateD_app) + 
  geom_point(aes(x = Year, y = `High Nitrogen (kg/ha)`, color =  `Well Classification`), size = 3) +  
  stat_poly_line(aes(x = Year, y = `High Nitrogen (kg/ha)`, group = Location, color = `Well Classification`), se = F, linewidth = 1.5) +
  scale_color_manual(values = Class_col) + 
  theme_bw() +
  scale_x_date(limits = c(as.Date("2008-01-01"), as.Date("2025-01-01"))) + 
  # geom_text_repel(data = Application_name_Increase, aes(x = Year, y = Application, label = Location), 
  #                 nudge_x = .3, nudge_y = .12, color = "black", size = 3, max.overlaps = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(size = 20),
        text = element_text(size = 18)) +
  ylab("Estimated Applied Nitrogen (kg/ha)") +
  labs(title = "Insignificant Application Trends on Wells \n with Increasing Nitrate Trends")

#Saving the plot
Nitrate_arranged <- ggarrange(N_Increase_app, N_Increase_sigapp, N_Increase_nosigapp, nrow = 3, ncol = 1)

ggsave("Application_Trends_NitrateIncrease.pdf", Nitrate_arranged, height = 13, width = 12)


# Decreasing Wells --------------------------------------------------------

Nitrate_Decrease_loc <- MK_AN %>%
  filter(Nitrate_pvalue < 0.2) %>%
  filter(Nitrate_Tau > 0) %>%
  distinct(Location, N_Classification)

Nitrate_Decrease_sig <- N_leach_app %>%
  subset(Location %in% Nitrate_Decrease_loc[ ,1]) %>%
  mutate(Year = as.Date(Year, format = "%Y")) %>%
  mutate(`Well Classification` = case_when(N_Classification == "Stable/Leaching" ~ "Leaching",
                                           N_Classification == "Stable/Mixing" ~ "Mixing",
                                           N_Classification != "Stable/Leaching" ~ N_Classification,
                                           N_Classification != "Stable/Mixing" ~ N_Classification)) %>%
  drop_na(`Well Classification`) %>%
  drop_na(`High Nitrogen (kg/ha)`)

Nitrate_name_Decrease <- Nitrate_lm %>%
  mutate(Year = as.Date(Date, format = "%Y")) %>%
  subset(Location %in% Nitrate_Decrease_loc[ ,1])


N_Decrease_app <- ggplot(Nitrate_Decrease_sig) + 
  geom_point(aes(x = Year, y = `Nitrate (mg/L)`, color =  `Well Classification`), size = 3) +  
  stat_poly_line(aes(x = Year, y = `Nitrate (mg/L)`, group = Location, color = `Well Classification`), se = F, linewidth = 1.5) +
  scale_color_manual(values = Class_col) + 
  theme_bw() +
  scale_x_date(limits = c(as.Date("2008-01-01"), as.Date("2025-01-01"))) + 
  #geom_text_repel(data = Nitrate_name_decrease, aes(x = Year, y = `Nitrate`, label = Location), 
  #               nudge_x = .3, nudge_y = .12, color = "black", size = 3, max.overlaps = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(size = 20),
        text = element_text(size = 18)) +
  ylab("Well Nitrate Trend  (mg/L)") +
  labs(title = "Wells with Decreasing Nitrate Trends")


#Signif Application in Wells with Decreasing Nitrate Trend
NitrateN_app_sig_loc <- MK_AN %>%
  filter(Nitrate_pvalue < 0.2) %>%
  filter(Nitrate_Tau > 0) %>%
  filter(`High N pvalue` < 0.2) %>%
  distinct(Location, N_Classification)

NitrateN_app_sig <- N_leach_app %>%
  subset(Location %in% NitrateN_app_sig_loc[ ,1]) %>%
  mutate(Year = as.Date(Year, format = "%Y")) %>%
  mutate(`Well Classification` = case_when(N_Classification == "Stable/Leaching" ~ "Leaching",
                                           N_Classification == "Stable/Mixing" ~ "Mixing",
                                           N_Classification != "Stable/Leaching" ~ N_Classification,
                                           N_Classification != "Stable/Mixing" ~ N_Classification)) %>%
  drop_na(`Well Classification`) %>%
  drop_na(`High Nitrogen (kg/ha)`)

Application_name_Decrease <- Application_lm %>%
  mutate(Year = as.Date(Date, format = "%Y")) %>%
  subset(Location %in% NitrateN_app_sig_loc[ ,1])


N_Decrease_sigapp <- ggplot(NitrateN_app_sig) + 
  geom_point(aes(x = Year, y = `High Nitrogen (kg/ha)`, color =  `Well Classification`), size = 3) +  
  stat_poly_line(aes(x = Year, y = `High Nitrogen (kg/ha)`, group = Location, color = `Well Classification`), se = F, linewidth = 1.5) +
  scale_color_manual(values = Class_col) + 
  theme_bw() +
  scale_x_date(limits = c(as.Date("2008-01-01"), as.Date("2025-01-01"))) + 
  # geom_text_repel(data = Application_name_decrease, aes(x = Year, y = Application, label = Location), 
  #                 nudge_x = .3, nudge_y = .12, color = "black", size = 3, max.overlaps = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(size = 20),
        text = element_text(size = 18)) +
  ylab("Estimated Applied Nitrogen (kg/ha)") +
  labs(title = "Significant Application Trends on Wells \n with Decreasing Nitrate Trends")

#InSignif Application in Wells with Decreasing Nitrate Trend
NitrateN_app_loc <- MK_AN %>%
  filter(Nitrate_pvalue < 0.2) %>%
  filter(Nitrate_Tau > 0) %>%
  filter(`High N pvalue` > 0.2) %>%
  distinct(Location, N_Classification)

NitrateN_app <- N_leach_app %>%
  subset(Location %in% NitrateN_app_loc [ ,1]) %>%
  mutate(Year = as.Date(Year, format = "%Y")) %>%
  mutate(`Well Classification` = case_when(N_Classification == "Stable/Leaching" ~ "Leaching",
                                           N_Classification == "Stable/Mixing" ~ "Mixing",
                                           N_Classification != "Stable/Leaching" ~ N_Classification,
                                           N_Classification != "Stable/Mixing" ~ N_Classification)) %>%
  drop_na(`Well Classification`) %>%
  drop_na(`High Nitrogen (kg/ha)`)

Application_name_Decrease <- Application_lm %>%
  mutate(Year = as.Date(Date, format = "%Y")) %>%
  subset(Location %in% NitrateN_app_loc [ ,1])


N_Decrease_nosigapp <- ggplot(NitrateN_app) + 
  geom_point(aes(x = Year, y = `High Nitrogen (kg/ha)`, color =  `Well Classification`), size = 3) +  
  stat_poly_line(aes(x = Year, y = `High Nitrogen (kg/ha)`, group = Location, color = `Well Classification`), se = F, linewidth = 1.5) +
  scale_color_manual(values = Class_col) + 
  theme_bw() +
  scale_x_date(limits = c(as.Date("2008-01-01"), as.Date("2025-01-01"))) + 
  # geom_text_repel(data = Application_name_Decrease, aes(x = Year, y = Application, label = Location), 
  #                 nudge_x = .3, nudge_y = .12, color = "black", size = 3, max.overlaps = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(size = 20),
        text = element_text(size = 18)) +
  ylab("Estimated Applied Nitrogen (kg/ha)") +
  labs(title = "Insignificant Application Trends on Wells \n with Decreasing Nitrate Trends")

Nitrate_arranged_Decrease <- ggarrange(N_Decrease_app, N_Decrease_sigapp, N_Decrease_nosigapp, nrow = 3, ncol = 1)

ggsave("Application_Trends_NitrateDecrease.pdf", Nitrate_arranged_Decrease, height = 13, width = 12)






#Mann Kendall on Fert Purchases
Purchase_County <- Purchase %>%
  drop_na() %>%
  rename("1987" = "X1987",
         "1992" = "X1992",
         "1997" = "X1997",
         "2002" = "X2002",
         "2007" = "X2007",
         "2012" = "X2012",
         "2017" = "X2017") %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), mean),
                      across(where(is.character), ~"All Counties"))) %>%
  pivot_longer(c(2:8), names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.Date(Year, format = "%Y"))

County_col <- c("All Counties" = "black", "Lane" = "#52CCB7", "Benton" = "#0F6DBF", "Linn" = "#B3436C")

Purchase_county_plot <- ggplot(Purchase_County) + 
  geom_point(aes(x = Year, y = Value, color = County), size = 3) +  
  geom_path(aes(x = Year, y = Value, color = County), linewidth = 1.5) +
  scale_color_manual(values = County_col) + 
  theme_bw() +
  scale_x_date(limits = c(as.Date("1987-01-01"), as.Date("2020-01-01"))) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(size = 20),
        text = element_text(size = 18)) +
  ylab("Fertilizer Purchased (kg/ha)") +
  labs(title = "Fertilizer Purchase Trends in the SWV-GWMA")

Purchase_Linn <- Purchase_County %>%
  subset(County %in% c("Linn"))
kendall_Linn <- MannKendall(Purchase_Linn$Value)

Purchase_Lane <- Purchase_County %>%
  subset(County %in% c("Lane"))

kendall_Lane <- MannKendall(Purchase_Lane$Value)

Purchase_Benton <- Purchase_County %>%
  subset(County %in% c("Benton"))
kendall_Benton <- MannKendall(Purchase_Benton$Value)

Purchase_Total <- Purchase_County %>%
  subset(County %in% c("All Counties"))
kendall_Total <- MannKendall(Purchase_Total$Value)
