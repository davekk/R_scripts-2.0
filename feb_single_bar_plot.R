require(tidyr); # data frame manipulation
require(dplyr); # data frame manipulation
require(ggplot2); #cool plot
require(reshape2); # convert data into wide and long format
require(magrittr) # pipe and related stuff
library("ggdendro")
library("grid")
setwd('F:/Geisenheim PhD/Bea stuff/Feb_08_plot/')
source('F:/Geisenheim PhD/Bea stuff/plots for erbsloh paper/summarySE.R')
#follow the tutorial from these links
#http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization#prepare-the-data

#http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#helper-functions

# convert all column into numeric column except the first column
  
wine <- readxl::read_xlsx('NOPA FOTs.xlsx',sheet =1,) # read the procossed ankom excel from teh first sheet

names(wine)[1] <- 'samples'

wine$rep <- rep(c('I','II','III'),times=5)

wine %>% reshape2::melt(id=c('samples','rep')) ->wine2 # melt it in preparation for bar plots

#apply our new function on our data, using the samples and variable, and use the value as input. This collapses the rep column
wine3<- summarySE(wine2,groupvars = c("samples", "variable"), measurevar = 'value')

# turn all the name column into factor
order.wine <- unique(as.character(unlist(wine[,1])))
wine3$samples <- factor(x=wine3$samples,levels=order.wine, ordered = TRUE)

p <-  ggplot(wine3, aes(x=samples, y=value, fill=samples,group=samples)) # absolute pressure
   
# plot of
# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.05) # move them .05 to the left and right

#mother of all plots
p + geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=.1, position=pd) +
  geom_bar(position ="dodge", stat = 'identity') +#+ facet_wrap(~variable,scales = 'free')+
  labs(x="Samples",
       y="Concentration [mg/L]",
       caption = "Error bars indicate standard deviation") +
  # change the label of y from value to relevant name
 # ggtitle("Aroma profile\n in incubated samples") +
  # Set tick every 0.5
  theme_bw() + # change the theme to be black and white
  scale_fill_manual(
    name="Samples", values = c("E1"='red',"EC1118"='blue',"FOT-5"= 'green',"FOT - 11"='lightgreen',"FOT 12"='darkgreen'), # sample color scheme
    guide=guide_legend(reverse = F)) +
  theme(legend.justification=c(1,0),
        legend.position='none', # move the legend a bit
        plot.title = element_text(hjust = 0.5), # move the title to the centre of the header
        axis.text.x =element_text(size = 9 ,face = "bold.italic", color = "Black",angle = 25,hjust = 1, colour = "grey50"),strip.text = element_text(size = 10)) 

ggsave('wine_Aroma_bar_plots free scale.png',width = 15,height = 15,units = 'in')
ggsave('wine_Aroma_bar_plots free scale.pdf',width = 10,height = 10,units = 'in')
