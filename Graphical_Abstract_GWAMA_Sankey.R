# Date Created: 9/23/2025
# Created by: Lena Wang
# Produces the sankey plot, graphical abstract. 

library("dplyr")
library("tidyr")
library("ggplot2")
library("ggalluvial")
library("ggpubr")

setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Oregon_GWMA/Oregon_GWMA_git")
Wells_MK <- read.csv("Well_Categories_MannKendall_Napplication.csv", header = T)
Last3 <- read.csv("Last3_GWMA.csv", header = T)
First3 <- read.csv("GWAMA_First3.csv", header = T)

#Filters for p-value 0.05, changes name of well classification to something simpler. 
Wells_MK_broad <- Wells_MK %>%
  mutate(`Well Status` = case_when(Nitrate_Tau > 0 & Nitrate_pvalue < 0.05  ~ "Decreasing", 
                                   Nitrate_Tau < 0 & Nitrate_pvalue < 0.05 ~ "Increasing",
                                   Nitrate_pvalue > 0.05 ~ "No Significance")) %>%
  mutate(`Well Classification` = case_when(N_Classification == "Stable/Leaching" ~ "Leaching",
                                           N_Classification == "Stable/Mixing" ~ "Mixing",
                                           N_Classification!= "Stable/Leaching" ~ N_Classification,
                                           N_Classification != "Stable/Mixing" ~ N_Classification))

Wells_Last3 <- left_join(Wells_MK_broad, Last3)

Wells <- left_join(Wells_Last3, First3)
Well_col <- c("Increasing" = "#E66E6E", "Decreasing" = "#4FAB00", "No Significance" = "#3C43D4")

#Assigns well class colors
Class_col <- c("Dilution" = "#15BB98", "Leaching" = "#F7DE03", "Mixing" = "#5F1657", "Multi-Proc" = "#D81B60",
               "Stable" = "#1E88E5", "Stable/Leaching" =  "#FFC107", "Stable/Mixing" = "#DC69B8")


#All Wells
All_Wells <- Wells_Last3 %>%
  group_by(`Well Status`, End.Nitrate.Concentration) %>%
  summarise(Sum = n()) %>%
  mutate(End.Nitrate.Concentration = factor(End.Nitrate.Concentration, levels = c("> 10 mg-N/L End",
                                                                                  "7-10 mg-N/L End",
                                                                                  "< 7 mg-N/L End")))
  #summarise(Percentage = 100 * n() / nrow(Wells_MK_broad))



#First 3 and Last 3
Wells2 <- replace(Wells, Wells == "Medium", "High")
All_Wells <- Wells2 %>%
  group_by(Beginning.Nitrate.Concentration, Ending.Nitrate.Concentration, Well.Classification) %>%
  summarise(Sum = n()) %>%
  mutate(Beginning.Nitrate.Concentration = factor(Beginning.Nitrate.Concentration, 
                                                  levels = c("High",
                                                             "Medium",
                                                             "Low"))) %>%
  mutate(Ending.Nitrate.Concentration = factor(Ending.Nitrate.Concentration, 
                                                  levels = c("High",
                                                             "Medium",
                                                             "Low")))
  
#Skey <- read.csv("GWMA_Sankey.csv", header = T)



All_Wells <- All_Wells %>%
  rename("Well Classification" = "Well.Classification")

#This is the code that produces the sankey plot
Plot <- ggplot(All_Wells, aes(y = Sum, axis1 = Beginning.Nitrate.Concentration, 
                              axis2 = Ending.Nitrate.Concentration)) +
  geom_alluvium(aes(fill = `Well Classification`)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)), size = 10) +
  #geom_text(stat = "alluvium", aes(label = Sum), nudge_x = 0.2, color = "black", size = 3) +
  scale_x_discrete(limits = c("Beginning Nitrate Concentration", "Ending Nitrate Concentration"), expand = c(.05, .05)) +
  labs(y = "Number of Wells") +
  scale_fill_manual(values = Class_col) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 25)) + 
  labs(title = "Well Nitrate Concentrations at the Beginning and Ending of Monitoring Period", size = 7)

Plot2 <- ggarrange(Plot, common.legend = T )

#ggsave("Sankey_Plot.pdf", Plot2, width = 18, height = 8)