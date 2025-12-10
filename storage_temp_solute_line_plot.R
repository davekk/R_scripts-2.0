library(magrittr)
library(tidyr)
library(dplyr)
library(ggplot2)
##
rm(list=ls())

setwd('D:/Thesis research all/')

run1 <- readxl::read_excel('run1_shelf_life.xlsx') %>% mutate(across(Name:Temp, as.factor))

run2 <- readxl::read_excel('run2_shelf_life.xlsx') %>% mutate(across(Name:Temp, as.factor))

run3 <- readxl::read_excel('run0_shelf_life.xlsx') %>% mutate(across(Name:Temp, as.factor))
##
run1 %>% mutate(Solute = forcats::fct_relevel(Solute, "Water", "Trehalose", "Sorbitol",
                            "Dextrose", "Lactose"),
       Solute = forcats::fct_recode(Solute,
                           "Water" = "Water",
                           "5% Trehalose" = "Trehalose",
                           "5% Sorbitol" = "Sorbitol",
                           "5% Dextrose" = "Dextrose",
                           "10% Lactose" = "Lactose",)) %>% 
  ggplot(aes(x = Days, y = P.E, color = Solute, group = Solute)) +
  geom_line(linewidth = 0.8) +
  theme_bw(base_size = 16) +
  geom_point(size=2) +
  facet_grid(Name ~ Temp, labeller = labeller(
        Temp = c("8" = " 8°C ", "22" = " 22°C "))) +
    labs( y = "Predation Efficiency (log2 scale)",  # renames P.E axis label
    x = "Days in Storage")+
  theme(strip.background = element_blank(),
        strip.text.x = element_text(face = "bold", size = 12),  # top headers
    strip.text.y = element_text(face = "bold", size = 12),
    legend.position = "bottom",                             # move legend to bottom
    legend.title = element_blank(),                         # ensure legend header is blank
    legend.key.width = unit(1, "cm"),                       # optional: adjust key width
    legend.key.height = unit(0.5, "cm")# side headers
  )+
  scale_y_continuous(  trans = "log2",
    limits = c(1, 40),
    breaks = c(1, 2, 4, 8, 16, 32),
    labels = c(1, 2, 4, 8, 16, 32))

#
run2 %>% mutate(Solute = forcats::fct_relevel(Solute, "Water", "Trehalose", 
                                              "Glucose", ),
                Solute = forcats::fct_recode(Solute,
                                             "Water" = "Water",
                                             "5% Trehalose" = "Trehalose",
                                             "5% Dextrose" = "Glucose" )) %>% 
  ggplot(aes(x = Days, y = P.E, color = Solute, group = Solute)) +
  geom_line(linewidth = 0.8) +
  geom_point(size=1) +
  facet_wrap( ~ Name, nrow = 3 ) +
  theme_bw(base_size = 16) +
  labs( y = "Predation Efficiency (log2 scale)",  # renames P.E axis label
        x = "Days in Storage")+
  theme(strip.background = element_blank(),
    strip.text.x = element_text(face = "bold", size = 12),
    legend.position = "bottom",                             # move legend to bottom
    legend.title = element_blank()  )+  # side headers
  
  scale_y_continuous(  trans = "log2", breaks = c(1, 2, 4, 8, 16, 32, 64, 128),
                       labels = c(1, 2, 4, 8, 16, 32, 64, 128)  )+
  scale_x_continuous(                                         # control x-axis scale
    limits = c(35, 114),                                       # start at day 35
    expand = expansion(mult = c(0, 0.05))                     # reduce extra space at edges
  ) 
#
#
run3 %>% 
  ggplot(aes(x = Days, y = P.E, color = Name, group = Name)) +
  geom_line(linewidth = 0.8) +
  geom_point(size=1) +
  facet_grid( Name ~ Temp, labeller = labeller(
    Temp = c("8" = " 8°C ", "22" = " 22°C "))) +
  theme_bw()+
  labs( y = "Predation Efficiency (log2 scale)",  # renames P.E axis label
        x = "Days in Storage")+
  theme(strip.background = element_blank(), 
        strip.text.x = element_text(face = "bold", size = 12),
        strip.text.y = element_text(face = "bold", size = 12))+  # side headers
  
  scale_y_continuous(  trans = "log2", breaks = c(1, 2, 4, 8, 16),
                       labels = c(1, 2, 4, 8, 16 )  )
