# Date Created 9/23/2025
# Created By: Lena Wang
#This code creates Figure 4 in paper

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
library("ggbreak")
library("stats")

#Change this working directory to match the folder data sits in
setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Oregon_GWMA/Oregon_GWMA_git")
NNI <- read.csv("20250716NNI_Wang.csv", header = T)
Nitrate <- read.csv("GWAMA_nitrate_data_Wang_20250204.csv", header = T)
Well_classes <- read.csv("Well_Categories_MannKendall_Napplication.csv", header = T)

COMID <- read.csv("NNI_COMID.csv", header = T)

#Pulling out the year and the type of nitrate inputs
NNI_long <- data.frame(matrix(ncol = 4))
names(NNI_long) <- c("COMID", "Year", "Nutrient", "Value")

for (i in 3:length(NNI)){
  NNI2 <- data.frame(matrix(ncol = 4, nrow = 31))
  names(NNI2) <- c("COMID", "Year", "Nutrient", "Value")
  
  NNI2$COMID <- NNI$comid
  NNI2$Year <- str_extract(colnames(NNI[i]), "\\d{4}")
  NNI2$Nutrient <- str_sub(colnames(NNI[i]), start = 1, end = 5)
  NNI2$Value <- NNI[,i]

  NNI_long <- rbind(NNI_long, NNI2)
}
#Renaming COMID 
COMID <- COMID %>%
  rename("COMID" = "FEATUREID")

#Dropping coordinates, first row, and changing Year to numeric so it joins properly
NNI_comid <- left_join(NNI_long[-1,], COMID, by = "COMID") %>%
  dplyr::select(-c("X", "geometry")) %>%
  mutate(Year = as.numeric(Year))

#Changing names and pulling out year
Nitrate2 <- Nitrate %>%
  rename("Location" = "Loc_ID") %>%
  mutate(Year = year(as.Date(Date, format = "%m/%d/%Y")))

#Creating a long data frame of nutrient types and nitrate concentrations
NNI_nitrate <- left_join(Nitrate2, NNI_comid, by = c("Location", "Year"))

#write.csv(NNI_nitrate, "NNI_Nitrate_Well_Locations.csv")

#Creating nice names and number groups, since everyhing in group 1 we want to find the sum
NNI_nitrate2 <- NNI_nitrate %>%
  mutate(Nutrient = case_when(grepl("n_ff_", Nutrient) ~ "Farm Fertilizer",
                              grepl("n_lw_", Nutrient) ~ "Livestock Manure",
                              grepl("n_lwr", Nutrient) ~ "Livestock Manure Recovered", #Don't need for inputs
                              grepl("n_cf_", Nutrient) ~ "Crop N-Fixation",
                              grepl("n_cr_", Nutrient) ~ "Crop Removal", #Don't need for inputs
                              grepl("n_dep", Nutrient) ~ "Atmos. Deposition",
                              grepl("n_uf_", Nutrient) ~ "Non-Farm Fertilizer",
                              grepl("n_hw_", Nutrient) ~ "Human Waste",
                              grepl("n_ww_", Nutrient) ~ "Wastewater Loads", #Only have up until 2012, didn't include here
                              grepl("n_leg", Nutrient) ~ "Agricultural Legacy Surplus", #Don't need for inputs
                              grepl("n_tin", Nutrient) ~ "Total Inputs", #Not yet, but can calculate from these numbers
                              grepl("n_ags", Nutrient) ~ "Annual Agricultural Surplus"
                              )) %>%

  drop_na() %>%
  group_by(Nutrient, Year) %>%
  mutate(Inputs = sum(Value),
         Area_ha = sum(AreaSqKM * 100), 
         `Inputs kg/ha` = Inputs/Area_ha) %>%
  ungroup() %>%
  distinct(Nutrient, Year, `Inputs kg/ha`)


#Bar chart of the sums

colorsN <- c('Farm Fertilizer' = '#A3CC51','Crop N-Fixation'='#FFD700', 'Livestock Manure'='#B26F2C','Atmos. Deposition'='#6db6ff','Human Waste'='#E51932','Non-Farm Fertilizer'='black', "Agricultural Legacy Surplus" = "#0dbdb4")

NNI_Class_input <- NNI_nitrate2 %>% 
  filter(!str_detect(Nutrient, "Crop Removal")) %>%
  filter(!str_detect(Nutrient, "Livestock Manure Recovered")) %>%
  filter(!str_detect(Nutrient, "Agricultural Legacy Surplus")) %>%
  filter(!str_detect(Nutrient, "Annual Agricultural Surplus")) %>%
  mutate(`Nutrient` = factor(`Nutrient`, levels = c("Non-Farm Fertilizer",
                                                          "Human Waste",
                                                          "Atmos. Deposition",
                                                          "Livestock Manure",
                                                          "Crop N-Fixation",
                                                          "Farm Fertilizer")))


Nitrogen_Plot <- ggplot(NNI_Class_input, aes(x = Year, y = `Inputs kg/ha`, fill = Nutrient)) +
  geom_bar(position = "stack", stat = "identity") + 
  scale_x_continuous(breaks = seq(2006, 2017, 1)) +
  scale_fill_manual(values = colorsN) +
  ggtitle("Nitrogen Inputs from 2006-2017 within the SWV-GWMA") +
  theme_bw()+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25))

#Calculating new Legacy
Legacy <- NNI_Class_input %>%
  mutate(Surplus = `Inputs kg/ha` * 0.41) %>%
  group_by(Year) %>%
  mutate(Sum = sum(Surplus)) %>%
  distinct(Year, Sum) %>%
  arrange(Year)
  

for (i in 1:nrow(Legacy)) {
  group <- Legacy[1:i,]
  Row <- cumsum(group)
  Legacy[i, "Legacy kg/ha"] = Row[i, "Sum"]
  
}

Legacy_Plot <- ggplot(Legacy, aes(x = Year, y = `Legacy kg/ha`)) +
  geom_bar(position = "stack", stat = "identity", fill = "#0dbdb4" ) + 
  scale_x_continuous(breaks = seq(2006, 2017, 1)) +
  ylab("Nitrogen Legacy kg/ha") + 
  ggtitle("Nitrogen Legacy from 2006-2017 within the SWV-GWMA") +
  theme_bw()+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25))



Plots <- ggarrange(Nitrogen_Plot, Legacy_Plot, nrow = 1, ncol = 2)
#ggsave("Inputs and Surplus_Plot2.pdf", Plots, width = 35, height = 10)
#Plots in the paper are above, extra plots are below
################################################################


NNI_Class_Legacy <- NNI_nitrate2 %>% 
  filter(str_detect(Nutrient, "Agricultural Legacy Surplus")) %>%
  dplyr::select(Year, `Inputs kg/ha`) %>%
  arrange(Year)

#Trying to get the Agricultural Legacy Surplus for 2006 and 2007, which some reason won't import from streamcat
NNI_Class_Annual <- NNI_nitrate2 %>% 
  filter(str_detect(Nutrient, "Annual Agricultural Surplus")) %>%
  dplyr::select(Year, `Inputs kg/ha`) %>%
  arrange(Year)

Leg_0607 = data.frame(Year = c(2006, 2007), Inputs = c(1742.452 - 79.87301 - 95.56545 , 1742.452 - 95.56545))
Leg_0607 <- Leg_0607 %>%
  rename("Inputs kg/ha"= "Inputs")


NNI_Legacy <-  rbind(Leg_0607, NNI_Class_Legacy)


#Bar chart of the legacy
Legacy_Plot <- ggplot(NNI_Legacy, aes(x = Year, y = `Inputs kg/ha`)) +
  geom_bar(position = "stack", stat = "identity", fill = "#0dbdb4" ) + 
  scale_x_continuous(breaks = seq(2006, 2017, 1)) +
  scale_fill_manual(values = colorsN) +
  ylab("Nitrogen Legacy kg/ha") + 
  ggtitle("Nitrogen Legacy from 2006-2017 within the SWV-GWMA") +
  theme_bw()+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25))


Plots <- ggarrange(Nitrogen_Plot, Legacy_Plot, nrow = 1, ncol = 2)
ggsave("Inputs and Surplus_Plot.pdf", Plots, width = 35, height = 10)

#Getting just the sums of far, fertilizer, manure, livestock waste, crop N fixation, crop removal, and Non-farm fertilizer
sums <- subset(NNI_class, Group == 1)



sums_distinct <- sums %>%
  distinct(Location, `Well Classification`,Year, Nitrate, Sum, `Inputs kg/ha`) %>%
  mutate(`Well Type` = case_when(grepl("GW", Location) ~ "Groundwater",
                                 grepl("DW", Location) ~ "Drinking Water"))

Loc_cols <-   c("DW-2" = "#88CCEE", "GW-16" = "#CC6677", "GW-17" = "#DDCC77", "GW-24" = "#117733", "GW-5" = "#332288", #Dilution
                "DW-10" = "#88CCEE", "DW-11" = "#CC6677", "DW-13" = "#DDCC77", "DW-17" = "#117733", "DW-8" = "#332288", "GW-1" = "#f092e0", "GW-20" = "#44AA99", "GW-21" = "#999933", "GW-22" = "#882255", "GW-3" = "#eda395", "GW-4D" = "#586ac7", #Leaching
                "DW-15" = "#88CCEE", "DW-1525" = "#CC6677", "DW-7a" = "#DDCC77", "GW-12" = "#117733", "GW-19" = "#332288", #Mixing
                "DW-1524" = "#88CCEE", "GW-10" = "#CC6677", "GW-11" = "#DDCC77", "GW-18" = "#117733", "GW-4S" = "#332288", "GW-7" = "#f092e0", "GW-8R" = "#44AA99", #Multi-Process
                "DW-16" = "#88CCEE", "DW-3" = "#CC6677", "DW-5" = "#DDCC77", "DW-6" = "#117733", "DW-9" = "#332288", "GW-13" = "#f092e0", "GW-15" = "#44AA99", "GW-9" ="#999933") #Stable


sum_groundwater <- sums_distinct  %>%
  subset(`Well Type` %in% "Groundwater") %>%
  mutate(Location = factor(Location, c("DW-2", "GW-16", "GW-17", "GW-24", "GW-5", #Dilution
                                       "DW-10", "DW-11", "DW-13", "DW-17", "DW-8", "GW-1", "GW-20", "GW-21", "GW-22" , "GW-3", "GW-4D", #Leaching
                                       "DW-15", "DW-1525", "DW-7a", "GW-12", "GW-19", #Mixing
                                       "DW-1524", "GW-10", "GW-11", "GW-18", "GW-4S", "GW-7", "GW-8R", #Multi-Process
                                       "DW-16" , "DW-3", "DW-5", "DW-6", "DW-9", "GW-13", "GW-15", "GW-9"))) #Stable
                           

sum_g <- ggplot(sum_groundwater) + 
  geom_path(aes(x = `Inputs kg/ha`, y = Nitrate, color = Year, group = Location), linewidth = 1) + 
  new_scale_color() +
  geom_point(aes(x = `Inputs kg/ha`, y = Nitrate, color = Location), size = 2) + 
  scale_color_manual(values = Loc_cols) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(size = 15),
        title = element_text(size = 30),
        text = element_text(size = 20)) +
  facet_wrap(vars(`Well Classification`)) +
  xlab("Nitrogen Inputs (kgha)") +
  ggtitle("Groundwater")

#ggsave("sum hysteresis groundwater.pdf", sum_g, width = 20, height = 15)


sum_drinking <- sums_distinct  %>%
  subset(`Well Type` %in% "Drinking Water")

sum_d <- ggplot(sum_drinking) + 
  geom_path(aes(x = `Inputs kg/ha`, y = Nitrate, color = Year, group = Location), linewidth = 1) + 
  new_scale_color() +
  geom_point(aes(x = `Inputs kg/ha`, y = Nitrate, color = Location), size = 2) + 
  scale_color_manual(values = Loc_cols) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(size = 15),
        title = element_text(size = 30),
        text = element_text(size = 20)) +
  facet_wrap(vars(`Well Classification`)) +
  xlab("Nitrogen Inputs (kg/ha)") +
  ggtitle("Drinking Water")

#ggsave("sum hysteresis drinking.pdf", sum_d, width = 20, height = 15)


leaching <- sums_distinct %>%
  subset(`Well Classification` %in% "Leaching")

leaching_plot <- ggplot(leaching) + 
  geom_path(aes(x = `Inputs kg/ha`, y = Nitrate, color = Year, group = Location), linewidth = 1) + 
  new_scale_color() +
  geom_point(aes(x = `Inputs kg/ha`, y = Nitrate, color = Location), size = 2) + 
  #scale_shape_manual(values = c(0, 1, 2, 5, 6, 7, 9, 10, 11, 12, 13)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(size = 15),
        title = element_text(size = 30),
        text = element_text(size = 20),
        axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_blank()) +
  #facet_wrap(vars(`Well Classification`), scales = "free") +
  xlab("Nitrogen Inputs (kg/ha)") +
  ggtitle("Leaching")

dilution <- sums_distinct %>%
  subset(`Well Classification` %in% "Dilution")

dilution_plot <- ggplot(dilution) + 
  geom_path(aes(x = `Inputs kg/ha`, y = Nitrate, color = Year, group = Location), linewidth = 1) + 
  new_scale_color() +
  geom_point(aes(x = `Inputs kg/ha`, y = Nitrate, color = Location), size = 2) + 
  #scale_shape_manual(values = c(0, 1, 2, 5, 6, 7, 9, 10, 11, 12, 13)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(size = 15),
        title = element_text(size = 30),
        text = element_text(size = 20),
        axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_blank()) +
  #facet_wrap(vars(`Well Classification`), scales = "free") +
  xlab("Nitrogen Inputs (kg/ha)") +
  ggtitle("Dilution")


multi <- sums_distinct %>%
  subset(`Well Classification` %in% "Multi-Proc")

multi_plot <- ggplot(multi) + 
  geom_path(aes(x = `Inputs kg/ha`, y = Nitrate, color = Year, group = Location), linewidth = 1) + 
  new_scale_color() +
  geom_point(aes(x = `Inputs kg/ha`, y = Nitrate, color = Location), size = 2) + 
  #scale_shape_manual(values = c(0, 1, 2, 5, 6, 7, 9, 10, 11, 12, 13)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(size = 15),
        title = element_text(size = 30),
        text = element_text(size = 20),
        axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_blank()) +
  #facet_wrap(vars(`Well Classification`), scales = "free") +
  xlab("Nitrogen Inputs (kg/ha)") +
  ggtitle("Multi-Process")

stable <- sums_distinct %>%
  subset(`Well Classification` %in% "Stable")

stable_plot <- ggplot(stable) + 
  geom_path(aes(x = `Inputs kg/ha`, y = Nitrate, color = Year, group = Location), linewidth = 1) + 
  new_scale_color() +
  geom_point(aes(x = `Inputs kg/ha`, y = Nitrate, color = Location), size = 2) + 
  #scale_shape_manual(values = c(0, 1, 2, 5, 6, 7, 9, 10, 11, 12, 13)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(size = 15),
        title = element_text(size = 30),
        text = element_text(size = 20),
        axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_blank()) +
  #facet_wrap(vars(`Well Classification`), scales = "free") +
  xlab("Nitrogen Inputs (kg/ha)") +
  ggtitle("Stable")

mixing <- sums_distinct %>%
  subset(`Well Classification` %in% "Mixing")

mixing_plot <- ggplot(mixing) + 
  geom_path(aes(x = `Inputs kg/ha`, y = Nitrate, color = Year, group = Location), linewidth = 1) + 
  new_scale_color() +
  geom_point(aes(x = `Inputs kg/ha`, y = Nitrate, color = Location), size = 2) + 
  #scale_shape_manual(values = c(0, 1, 2, 5, 6, 7, 9, 10, 11, 12, 13)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(size = 15),
        title = element_text(size = 30),
        text = element_text(size = 20),
        axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_blank()) +
  #facet_wrap(vars(`Well Classification`), scales = "free") +
  xlab("Nitrogen Inputs (kg/ha)") +
  ggtitle("Mixing")



plots <- leaching_plot + dilution_plot + multi_plot + mixing_plot + stable_plot
#ggsave("sum hysteresis all.pdf", plots, width = 40, height = 15)


