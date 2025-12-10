setwd('D:/Geisenheim_PhD/')
library(magrittr)
ori <- readxl::read_xlsx('Low Agar mutants july 2022.xlsx')

source('D:/Geisenheim_PhD/Bea stuff/plots for erbsloh paper/summarySE.R')


ori %>% reshape2::melt(id=c('Sample','rep'))%>% 
  summarySE(groupvars = c("Sample","variable"), measurevar = 'value') ->ori2

ggplot(ori2, 
       aes(x = Sample, y = value)) + 
  geom_bar(stat = "identity", position = "dodge") +
  # Add error bars (here +/- 1.96 SE)
  #geom_errorbar(aes(ymax = value + 1.96*se, 
   #                 ymin = value - 1.96*se),
    #            position = "dodge") +
  xlab("Sample Code") + 
  ylab("Average size (mm)") +
  labs(fill = "") + 
  scale_fill_grey()
