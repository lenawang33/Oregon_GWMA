#Date Created: 9/23/2025
# Created by Lena Wang
#This creates the pie charts in the paper

library("dplyr")
library("tidyr")
library("ggplot2")


setwd("O:/PRIV/CPHEA/PESD/COR/CORFILES/Projects/OregonGWMA/SWV-GWMA/Wang GWMA Nitrate Trends Paper/Data")
Wells_MK <- read.csv("Well_Categories_MannKendall_Napplication.csv", header = T)

#This will only create the bar charts for p-value < 0.2, according to DEQ. To get p-value 0.05, change 0.2 to 0.2
Wells_MK_broad <- Wells_MK %>%
  mutate(`Well Status` = case_when(Nitrate_Tau > 0 & Nitrate_pvalue < 0.2  ~ "Decreasing", 
                                   Nitrate_Tau < 0 & Nitrate_pvalue < 0.2 ~ "Increasing",
                                   Nitrate_pvalue > 0.2 ~ "No Significance")) %>%
  mutate(`Well Classification` = case_when(N_Classification == "Stable/Leaching" ~ "Leaching",
                                           N_Classification == "Stable/Mixing" ~ "Mixing",
                                           N_Classification!= "Stable/Leaching" ~ N_Classification,
                                           N_Classification != "Stable/Mixing" ~ N_Classification))

Well_col <- c("Increasing" = "#E66E6E", "Decreasing" = "#4FAB00", "No Significance" = "#3C43D4")

Class_col <- c("Dilution" = "#15BB98", "Leaching" = "#F7DE03", "Mixing" = "#5F1657", "Multi-Proc" = "#D81B60",
               "Stable" = "#1E88E5", "Stable/Leaching" =  "#FFC107", "Stable/Mixing" = "#DC69B8")


#All Wells
All_Wells <- Wells_MK_broad %>%
  group_by(`Well Status`) %>% 
  summarise(Percentage = 100 * n() / nrow(Wells_MK_broad))
#Currently creates circle pie chart, Figure 1c
All_Wells_plot <- ggplot(All_Wells, aes(x = "", y = `Percentage`, fill = `Well Status`)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_manual(values = Well_col) + 
  coord_polar("y", start = 0) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_blank())



#ggsave('All_Wells.png', All_Wells_plot, bg ='transparent')

#Statistically Significant Wells



#This code is irrelevant to the paper, it just highlights in the wells with significant trends
Wells_MK_broad_sig <- Wells_MK_broad %>%
  filter(Nitrate_pvalue < 0.2)

All_Wells_sig <- Wells_MK_broad_sig %>%
  group_by(`Well Status`) %>% 
  summarise(Percentage = 100 * n() / nrow(Wells_MK_broad_sig))

Sig_Wells_Plot <- ggplot(All_Wells_sig, aes(x = "", y = `Well Status`, fill = `Well Status`)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_manual(values = Well_col) + 
  coord_polar("y", start = 0) +
  theme_void() + 
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    panel.border = element_blank(),
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )

ggsave('All_Wells_sig.png', Sig_Wells_Plot, bg ='transparent')


Increase <- Wells_MK_broad_sig %>% 
  filter(`Well Status` %in% "Increase")

Increase_perc <- Increase %>%
  group_by(`Well Classification`) %>% 
  summarise(Percentage = 100 * n() / nrow(Increase))

Increase_sig <- ggplot(Increase_perc, aes(x = "", y = `Percentage`, fill = `Well Classification`)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_manual(values = Class_col) + 
  coord_polar("y", start = 0) +
  theme_void() +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    panel.border = element_blank(),
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )

ggsave('Increase_wells_sig.png', Increase_sig, bg ='transparent')
  



Decrease <- Wells_MK_broad_sig %>% 
  filter(`Well Status` %in% "Decrease")

Decrease_perc <- Decrease %>%
  group_by(`Well Classification`) %>% 
  summarise(Percentage = 100 * n() / nrow(Decrease))


Decrease_sig <- ggplot(Decrease_perc, aes(x = "", y = `Percentage`, fill = `Well Classification`)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_manual(values = Class_col) + 
  coord_polar("y", start = 0) +
  theme_void() +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    panel.border = element_blank(),
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )

ggsave('Decrease_wells_sig.png', Decrease_sig, bg ='transparent')


No_Change <- Wells_MK_broad %>% 
  filter(`Well Status` %in% "No Change")

NC_perc <- No_Change %>%
  group_by(`Well Classification`) %>% 
  summarise(Percentage = 100 * n() / nrow(No_Change))


ggplot(NC_perc, aes(x = "", y = `Percentage`, fill = `Well Classification`)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_manual(values = Class_col) + 
  coord_polar("y", start = 0) +
  theme_void()


#Leaching 
Leaching <- Wells_MK_broad_sig%>% 
  filter(`Well Classification` %in% "Leaching")


Leaching_perc <- Leaching %>%
  group_by(`Well Status`) %>% 
  summarise(Percentage = 100 * n() / nrow(Leaching))


Leaching_sig <- ggplot(Leaching_perc, aes(x = "", y = `Percentage`, fill = `Well Status`)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_manual(values = Well_col) + 
  coord_polar("y", start = 0) +
  theme_void() +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    panel.border = element_blank(),
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )

ggsave('Leaching_wells_sig.png', Leaching_sig, bg ='transparent')


#Mixing

Mixing <- Wells_MK_broad_sig %>% 
  filter(`Well Classification` %in% "Mixing")


Mixing_perc <- Mixing %>%
  group_by(`Well Status`) %>% 
  summarise(Percentage = 100 * n() / nrow(Mixing))


Mixing_sig <- ggplot(Mixing_perc, aes(x = "", y = `Percentage`, fill = `Well Status`)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_manual(values = Well_col) + 
  coord_polar("y", start = 0) +
  theme_void() +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    panel.border = element_blank(),
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )
ggsave('Mixing_wells_sig.png', Mixing_sig, bg ='transparent')
#Dilution
Dilution <- Wells_MK_broad_sig %>% 
  filter(`Well Classification` %in% "Dilution")


Dilution_perc <- Dilution %>%
  group_by(`Well Status`) %>% 
  summarise(Percentage = 100 * n() / nrow(Dilution))


Dilution_sig <- ggplot(Dilution_perc, aes(x = "", y = `Percentage`, fill = `Well Status`)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_manual(values = Well_col) + 
  coord_polar("y", start = 0) +
  theme_void() +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    panel.border = element_blank(),
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )

ggsave('Dilution_wells_sig.png', Dilution_sig, bg ='transparent')



#Multi-Proc
`Multi-Proc` <- Wells_MK_broad_sig %>% 
  filter(`Well Classification` %in% "Multi-Proc")


`Multi-Proc_perc` <- `Multi-Proc` %>%
  group_by(`Well Status`) %>% 
  summarise(Percentage = 100 * n() / nrow(`Multi-Proc`))


Multi_sig <- ggplot(`Multi-Proc_perc`, aes(x = "", y = `Percentage`, fill = `Well Status`)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_manual(values = Well_col) + 
  coord_polar("y", start = 0) +
  theme_void() +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    panel.border = element_blank(),
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )

ggsave('Multi_sig.png', Multi_sig, bg ='transparent')


#Stable
`Stable` <- Wells_MK_broad_sig %>% 
  filter(`Well Classification` %in% "Stable")


`Stable_perc` <- `Stable` %>%
  group_by(`Well Status`) %>% 
  summarise(Percentage = 100 * n() / nrow(`Stable`))


Stable_sig <- ggplot(`Stable_perc`, aes(x = "", y = `Percentage`, fill = `Well Status`)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_manual(values = Well_col) + 
  coord_polar("y", start = 0) +
  theme_void() +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    panel.border = element_blank(),
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )

ggsave('Stable_sig.png', Stable_sig, bg ='transparent')


