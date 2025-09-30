#Date Created 9/23/2025
# Created by: Lena Wang
# This code makes bar plots for the GWMA wells, Figure 2, p-value < 0.2 here

library("dplyr")
library("tidyr")
library("ggplot2")
library("ggpubr")
library(lubridate)



setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Oregon_GWMA/Oregon_GWMA_git")
Wells_MK <- read.csv("Well_Categories_MannKendall_Napplication.csv", header = T)

Wells_MK_broad <- Wells_MK %>%
  mutate(`Well Classification` = case_when(N_Classification == "Stable/Leaching" ~ "Leaching",
                                           N_Classification == "Stable/Mixing" ~ "Mixing",
                                           N_Classification != "Stable/Leaching" ~ N_Classification,
                                           N_Classification != "Stable/Mixing" ~ N_Classification)) %>%
  drop_na(`Well Classification`)

Well_col <- c("Increase" = "#BF2929", "Decrease" = "#32CE4A", "No Change" = "#000AFF")

Class_col <- c("Dilution" = "#15BB98", "Leaching" = "#FFA507", "Mixing" = "#5F1657", "Multi-Proc" = "#D81B60",
               "Stable" = "#1E88E5", "Stable/Leaching" =  "#FFC107", "Stable/Mixing" = "#DC69B8")


# Nitrate Significant Wells -----------------------------------------------

#Change the p-value to 0.05 for the standard values

Nitrate_sig <- Wells_MK_broad %>%
  mutate(`Well Trend` = case_when(Nitrate_Tau > 0 & Nitrate_pvalue < 0.2 ~ "Increase",
                                  Nitrate_Tau < 0 & Nitrate_pvalue < 0.2 ~ "Decrease",
                                  Nitrate_pvalue > 0.2 ~ "No Significance")) %>%
  mutate(`Well Trend` = factor(`Well Trend`, levels = c("Increase", "Decrease", "No Significance"))) %>%
  filter(Nitrate_pvalue < 0.2)

# Application Significant Well --------------------------------------------


App_sig <- Wells_MK_broad %>%
  mutate(`Application Trend` = case_when(High.N.Tau > 0 & High.N.pvalue < 0.2 ~ "Increase",
                                  High.N.Tau < 0 & High.N.pvalue < 0.2 ~ "Decrease",
                                  High.N.pvalue > 0.2 ~ "No Significance")) %>%
  filter(Nitrate_pvalue < 0.2) %>% 
  filter(High.N.pvalue < 0.2) %>%
  mutate(`Application Trend` = factor(`Application Trend`, levels = c("Increase", "Decrease", "No Significance")))


# Plotting only Significant Nitrate and Application Trends ----------------

#This plot doesn't show up in the paper
Nitrate_bar <- ggplot(Nitrate_sig, (aes(x = `Well Trend`, color = `Well Classification`, fill = `Well Classification`))) +
  geom_bar() +
  scale_color_manual(values = Class_col) + 
  scale_fill_manual(values = Class_col) + 
  ylab("Number of Wells") + 
  xlab("Nitrate Trends") +
  labs(title = "Wells with Significant \n Nitrate Trends") +
  scale_y_continuous(breaks = seq(0, 20, by = 2), limits = c(0, 14)) + 
  theme_classic() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 20))

#This plot didn't show up in the paper
App_bar <- ggplot(App_sig, (aes(x = `Application Trend`, color = `Well Classification`, fill = `Well Classification`))) +
  geom_bar() +
  scale_color_manual(values = Class_col) + 
  scale_fill_manual(values = Class_col) + 
  ylab("Number of Wells") + 
  xlab("Fertilizer Application Trends") +
  labs(title = "Fertilizer Application Trends around \n Wells with Significant Nitrate Trends") +
  scale_y_continuous(breaks = seq(0, 20, by = 2), limits = c(0, 14)) + 
  theme_classic() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5), 
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 20))


Arranged <- ggarrange(Nitrate_bar, App_bar, common.legend = T)
#ggsave("Sig Leach Bar and App Bar.pdf", Arranged, width = 13, height = 8)




# Nitrate All Wells -------------------------------------------------------


Nitrate_all <- Wells_MK_broad %>%
  mutate(`Well Trend` = case_when(Nitrate_Tau < 0 & Nitrate_pvalue < 0.2 ~ "Increase",
                                  Nitrate_Tau > 0 & Nitrate_pvalue < 0.2 ~ "Decrease",
                                  Nitrate_pvalue > 0.2 ~ "No Significance")) %>%
  mutate(`Well Trend` = factor(`Well Trend`, levels = c("Increase", "Decrease", "No Significance")))


# Application All Wells ---------------------------------------------------


App_all <- Wells_MK_broad %>%
  mutate(`Application Trend` = case_when(High.N.Tau < 0 & High.N.pvalue < 0.2 ~ "Increase",
                                         High.N.Tau > 0 & High.N.pvalue < 0.2 ~ "Decrease",
                                         High.N.pvalue > 0.2 ~ "No Significance")) %>%
  mutate(`Application Trend` = factor(`Application Trend`, levels = c("Increase", "Decrease", "No Significance")))

# Plotting the All Nitrate and Application Trends -------------------------

Nitrate_all_bar <- ggplot(Nitrate_all, (aes(x = `Well Trend`, color = `Well Classification`, fill = `Well Classification`))) +
  geom_bar() +
  scale_color_manual(values = Class_col) + 
  scale_fill_manual(values = Class_col, drop = F) + 
  scale_x_discrete(drop = F) +
  ylab("Number of Wells") + 
  xlab("Nitrate Trends") +
  labs(title = "Nitrate Trends of \n All Wells") +
  scale_y_continuous(breaks = seq(0, 20, by = 2), limits = c(0, 22)) + 
  theme_classic() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 20))


App_all_bar <- ggplot(App_all, (aes(x = `Application Trend`, color = `Well Classification`, fill = `Well Classification`))) +
  geom_bar() +
  scale_color_manual(values = Class_col) + 
  scale_fill_manual(values = Class_col, drop = F) + 
  scale_x_discrete(drop = F) +
  ylab("Number of Wells") + 
  xlab("Fertilizer Application Trends") +
  labs(title = "Fertilizer Application Trends around \n All Wells") +
  scale_y_continuous(breaks = seq(0, 20, by = 2), limits = c(0, 22)) + 
  theme_classic() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5), 
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 20))


Arranged_all <- ggarrange(Nitrate_all_bar, App_all_bar, common.legend = T)
#ggsave("All Leach Bar and App Bar.pdf", Arranged_all, width = 13, height = 8)



# Getting the Mean of the First 3 Years--------------

Nitrate <- read.csv("GWAMA_nitrate_data_Wang_20250204.csv", header = T)

#All Wells
Nitrate_All <- Wells_MK_broad %>%
  mutate(`Well Trend` = case_when(Nitrate_Tau < 0 & Nitrate_pvalue < 0.2 ~ "Increase", #Reverse because Mann Kendall in earlier code move from 2025-2008
                                  Nitrate_Tau > 0 & Nitrate_pvalue < 0.2 ~ "Decrease",
                                  Nitrate_pvalue > 0.2 ~ "No Significance")) %>%
  mutate(`Well Trend` = factor(`Well Trend`, levels = c("Increase", "Decrease", "No Significance")))
#Getting well names
Well_name <- Nitrate_All %>%
  distinct(Location, `Well Classification`, `Well Trend`)

# Bar Graphs for the First Three Years and Last Three Years ---------------
Nitrate_l3 <- Nitrate %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>% #Turning date in date
  mutate(Year = as.Date(as.character(Date), format = "%Y")) %>% #Turning date into character
  mutate(Year = year(Year)) %>% #pulling out just year as a character
  rename("Location" = "Loc_ID") %>%
  group_by(Location) %>%
  filter(Year >= max(Year)-2)  %>%
  mutate(Last = mean(Nitrate, na.rm = T)) %>%
  ungroup()

#First three years
Nitrate_f3 <- Nitrate %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>% #Turning date in date
  mutate(Year = as.Date(as.character(Date), format = "%Y")) %>% #Turning date into character
  mutate(Year = year(Year)) %>% #pulling out just year as a character
  rename("Location" = "Loc_ID") %>%
  group_by(Location) %>%
  filter(Year <= min(Year)+2) %>%
  mutate(First = mean(Nitrate, na.rm = T)) %>%
  ungroup()

N3_distinct <- Nitrate_f3 %>% 
  distinct(Location)

N3_distinct_l <- Nitrate_l3 %>% 
  distinct(Location)


#First3 <- write.csv( Nitrate_f3, "GWAMA_First3.csv")

F3_Nitrate <- left_join(Nitrate_f3, Well_name) %>%
  drop_na(`Well Classification`) %>%
  mutate(`Beginning Nitrate Concentration` = factor(case_when(First < 7 ~ "< 7 mg-N/L Beginning",
                                                              First <= 10 & First >= 7 ~ "7-10 mg-N/L Beginning",
                                                           First > 10 ~ "> 10 mg-N/L Beginning"),
                                                 levels = c("< 7 mg-N/L Beginning",
                                                            "7-10 mg-N/L Beginning",
                                                            "> 10 mg-N/L Beginning"))) %>%
  mutate(`Well Trend` = factor(`Well Trend`, levels = c("Increase",
                                                        "Decrease",
                                                        "No Significance"))) %>%
  dplyr::select(-c(Nitrate, Year, Date)) %>%
  distinct()

L3_Nitrate <- left_join(Nitrate_l3, Well_name) %>%
  drop_na(`Well Classification`) %>%
  mutate(`End Nitrate Concentration` = factor(case_when(Last < 7 ~ "< 7 mg-N/L Ending",
                                                           Last <= 10 & Last >= 7 ~ "7-10 mg-N/L Ending",
                                                           Last > 10 ~ "> 10 mg-N/L Ending"),
                                                 levels = c("> 10 mg-N/L Ending",
                                                            "7-10 mg-N/L Ending",
                                                            "< 7 mg-N/L Ending"))) %>%
  mutate(`Well Trend` = factor(`Well Trend`, levels = c("Increase",
                                                        "Decrease",
                                                        "No Significance"))) %>%
  dplyr::select(-c(Nitrate, Year, Date, Lat_Dec, Long_Dec)) %>%
  distinct()

Nitrate_33 <- left_join(L3_Nitrate, F3_Nitrate)

Nitrate_33_2 <- Nitrate_33 %>%
  mutate(`Ending Nitrate Concentration` =  factor(case_when(grepl("< 7 mg-N/L Ending", `End Nitrate Concentration`) ~ "< 7 mg-N/L Ending",
                                         grepl("7-10 mg-N/L Ending", `End Nitrate Concentration`) ~ "7-10 mg-N/L Ending",
                                         grepl("> 10 mg-N/L Ending", `End Nitrate Concentration`) ~ "> 10 mg-N/L Ending"),
                               levels = c("> 10 mg-N/L Ending",
                                          "7-10 mg-N/L Ending",
                                          "< 7 mg-N/L Ending"))) %>%
  mutate(`Beginning Nitrate Concentration` =  factor(case_when(grepl("< 7 mg-N/L Beginning", `Beginning Nitrate Concentration`) ~ "< 7 mg-N/L Beginning",
                                         grepl("7-10 mg-N/L Beginning", `Beginning Nitrate Concentration`) ~ "7-10 mg-N/L Beginning",
                                         grepl("> 10 mg-N/L Beginning", `Beginning Nitrate Concentration`) ~ "> 10 mg-N/L Beginning"),
                               levels = c("< 7 mg-N/L Beginning",
                                          "7-10 mg-N/L Beginning",
                                          "> 10 mg-N/L Beginning")))

#write.csv(Nitrate_33_2, "First3_Last3_GWMA.csv")

#THIS PLOT SHOWS UP IN THE PAPER, FIGURE 2
Nitrate_33Plot <- ggplot(Nitrate_33_2, (aes(x = `Well Trend`, color = `Well Classification`, fill = `Well Classification`))) +
  geom_bar() +
  scale_color_manual(values = Class_col) + 
  scale_fill_manual(values = Class_col, drop = F) + 
  ylab("Number of Wells") + 
  xlab("Nitrate Trend") +
  labs(title = "Comparing Average Nitrate Concentrations of \n Wells in the First 3 Years and Last 3 Years") +
  facet_grid(`Ending Nitrate Concentration`~ `Beginning Nitrate Concentration`) + 
  scale_y_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 10)) + 
  theme_classic() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5), 
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 20))

#ggsave("Nitrate_First3Last3_2.pdf", Nitrate_33Plot, width = 17, height = 11)

#Figuring out which Wells switched categories
Switch <- Nitrate_33 %>%
  mutate(`End Nitrate Concentration` = case_when(Last < 7 ~ "< 7 mg-N/L",
                                                        Last <= 10 & Last >= 7 ~ "7-10 mg-N/L",
                                                        Last > 10 ~ "> 10 mg-N/L"),
         `Beginning Nitrate Concentration` = case_when(First < 7 ~ "< 7 mg-N/L",
                                                       First <= 10 & First >= 7 ~ "7-10 mg-N/L",
                                                       First > 10 ~ "> 10 mg-N/L")) %>%
  filter(`Beginning Nitrate Concentration` != `End Nitrate Concentration`)

Nitrate_names <- Nitrate_33 %>% 
  select(Location, `End Nitrate Concentration`, `Beginning Nitrate Concentration`, `Well Trend`, `Well Classification`)

#Bar Graphs for the Max and Most Recent Single Year-----------
#Getting most recent year, most are 2024, Dw-17 stopped in 2020
Nitrate_filt <- Nitrate %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>% #Turning date in date
  mutate(Year = as.Date(as.character(Date), format = "%Y")) %>% #Turning date into character
  mutate(Year = year(Year)) %>% #pulling out just year as a character
  rename("Location" = "Loc_ID") %>%
  group_by(Location) %>%
  filter(Year >= max(Year)) %>%
  ungroup()

# Bar graphs for the Max and Most Single Year -----------------------------



#Bar Graphs foor the Max and 
#Getting max Nitrate from 2008-2024
Max <- Nitrate %>%
  rename("Location" = "Loc_ID") %>%
  group_by(Location) %>%
  filter(Nitrate == max(Nitrate)) %>%
  rename("Max" = "Nitrate") %>%
  ungroup() %>%
  dplyr::select(-c(Date))



#Getting the 2024 value with well names
Annual_Nitrate <- left_join(Nitrate_filt, Well_name) %>%
  drop_na(`Well Classification`) %>%
  mutate(`Recent Nitrate Concentration` = factor(case_when(Nitrate < 7 ~ "< 7 mg-N/L",
                                                    Nitrate <= 10 & Nitrate >= 7 ~ "7-10 mg-N/L",
                                                    Nitrate > 10 ~ "> 10 mg-N/L"),
                                                 levels = c("< 7 mg-N/L",
                                                            "7-10 mg-N/L",
                                                            "> 10 mg-N/L")))

#Getting Max value with Well Names
Max_Nitrate <- left_join(Max, Well_name) %>%
  drop_na(`Well Classification`) %>%
  mutate(`Max Nitrate Concentration` = factor(case_when(Max < 7 ~ "< 7 mg-N/L",
                                                 Max <= 10 & Max >= 7 ~ "7-10 mg-N/L",
                                                 Max > 10 ~ "> 10 mg-N/L"),
                                              levels = c("< 7 mg-N/L",
                                                         "7-10 mg-N/L",
                                                         "> 10 mg-N/L")))


Max_nitrate <- ggplot(Max_Nitrate, (aes(x = `Max Nitrate Concentration`, color = `Well Classification`, fill = `Well Classification`))) +
  geom_bar() +
  scale_color_manual(values = Class_col) + 
  scale_fill_manual(values = Class_col, drop = F) + 
  scale_x_discrete(drop = F) +
  ylab("Number of Wells") + 
  xlab("Nitrate Concentrations") +
  labs(title = "Maximum Nitrate Concentrations \n from 2008-2024 of All Wells") +
  scale_y_continuous(breaks = seq(0, 24, by = 2), limits = c(0, 24)) + 
  theme_classic() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5), 
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 20))

Recent_nitrate <- ggplot(Annual_Nitrate, (aes(x = `Recent Nitrate Concentration`, color = `Well Classification`, fill = `Well Classification`))) +
  geom_bar() +
  scale_color_manual(values = Class_col) + 
  scale_fill_manual(values = Class_col, drop = F) + 
  scale_x_discrete(drop = F) +
  ylab("Number of Wells") + 
  xlab("Nitrate Concentrations") +
  labs(title = "Nitrate  Concentrations of \n All Wells in the 2020s") +
  scale_y_continuous(breaks = seq(0, 24, by = 2), limits = c(0, 24)) + 
  theme_classic() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5), 
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 20))


Arranged2 <- ggarrange(Max_nitrate, Recent_nitrate, common.legend = T)
ggsave("Nitrate Benchmark Bar.pdf", Arranged2, width = 13, height = 8)


# Determining Wells that are bad that improving or vice versa -------------
Nitrate_max_recent <- left_join(Nitrate_all, Annual_Nitrate)

Nitrate_max_recent2 <- Nitrate_max_recent %>%
  dplyr::select(Location, Max, Nitrate, `Well Classification`, `Well Trend`) %>%
  mutate(`Max Nitrate Concentration` = factor(case_when(Max < 7 ~ "< 7 mg-N/L",
                                                        Max <= 10 & Max >= 7 ~ "7-10 mg-N/L",
                                                        Max > 10 ~ "> 10 mg-N/L"),
                                              levels = c("< 7 mg-N/L",
                                                         "7-10 mg-N/L",
                                                         "> 10 mg-N/L")))

Long_term_better <- ggplot(Nitrate_max_recent2, (aes(x = `Well Trend`, color = `Well Classification`, fill = `Well Classification`))) +
  geom_bar() +
  scale_color_manual(values = Class_col) + 
  scale_fill_manual(values = Class_col, drop = F) + 
  ylab("Number of Wells") + 
  xlab("Nitrate Trend") +
  labs(title = "Trends of Wells Grouped by Maximum \n Nitrate Concentration from 2008-2024") +
  facet_wrap(vars(`Max Nitrate Concentration`)) + 
  scale_y_continuous(breaks = seq(0, 8, by = 2), limits = c(0, 8)) + 
  theme_classic() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5), 
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 20))

ggsave("Nitrate_grouped_Max_Trend.pdf", Long_term_better, width = 13, height = 8)


#Grouping it by data from the 2020s
Nitrate_max_recent3 <- Nitrate_max_recent %>%
  dplyr::select(Location, Max, Nitrate, `Well Classification`, `Well Trend`) %>%
  mutate(`Recent Nitrate Concentration` = factor(case_when(Nitrate < 7 ~ "< 7 mg-N/L",
                                                        Nitrate <= 10 & Nitrate >= 7 ~ "7-10 mg-N/L",
                                                        Nitrate > 10 ~ "> 10 mg-N/L"),
                                              levels = c("< 7 mg-N/L",
                                                         "7-10 mg-N/L",
                                                         "> 10 mg-N/L")))

Long_term_better2 <- ggplot(Nitrate_max_recent3, (aes(x = `Well Trend`, color = `Well Classification`, fill = `Well Classification`))) +
  geom_bar() +
  scale_color_manual(values = Class_col) + 
  scale_fill_manual(values = Class_col, drop = F) + 
  ylab("Number of Wells") + 
  xlab("Nitrate Trend") +
  labs(title = "Trends of Wells Grouped by Maximum \n Nitrate Concentration in the 2020s") +
  facet_wrap(vars(`Recent Nitrate Concentration`)) + 
  scale_y_continuous(breaks = seq(0, 8, by = 2), limits = c(0, 8)) + 
  theme_classic() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5), 
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 20))

#ggsave("Nitrate_grouped_Recent_Trend.pdf", Long_term_better2, width = 13, height = 8)

Recent_max_trend <- ggarrange(Long_term_better, Long_term_better2, common.legend = T)
#ggsave("Nitrate_grouped_RecentandMax_Trend.pdf", Recent_max_trend, width = 20, height = 8)



# Land Cover Bar plots ----------------------------------------------------
setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Oregon_GWMA")
Land_cover <- read.csv("LandCovermse.csv", header = T)

Land_col <- c("Natural Lands" = "#1E88E5", "Agricultural" = "#5FB91B", "Other" = "#875500")

Land_cover2 <- Land_cover %>%
  mutate(`Nitrate Concentration` = factor(Concentration, levels = c("< 7 mg-N/L", "7-10 mg-N/L", "> 10 mg-N/L"))) %>%
  rename("Land Cover" = "Land.Cover")


Landcover_plot <- ggplot(Land_cover2, aes(x = `Nitrate Concentration`, y = Median, fill = `Land Cover`)) +
  geom_bar(position = position_dodge(width = 0.9), stat = "identity") + 
  geom_errorbar(aes(y = Median, ymin = Median - Standard.Error, ymax = Median + Standard.Error),
                position = position_dodge(width = 0.9), width = 0.2) +
  scale_color_manual(values = Land_col) + 
  scale_fill_manual(values = Land_col, drop = F) + 
  scale_x_discrete(drop = F) +
  ylab("Median Land Cover %") + 
  xlab("Nitrate Concentrations") +
  labs(title = "Land Cover of Wells with \n Different Nitrate Concentrations") +
  theme_classic() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5), 
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 20))


ggsave("Crop_Land_Cover.pdf", Landcover_plot, width = 10, height = 6)



