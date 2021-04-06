require(readxl); # read excel files
require(tidyr); # data frame manipulation
require(dplyr); # data frame manipulation
require(ggplot2); #cool plot
require(reshape2); # convert data into wide and long format
require(magrittr) # pipe and related stuff
#install.packages('ggsignif')
require(ggsignif)
setwd('F:/Geisenheim PhD/Bea stuff/plots for erbsloh paper/glucose and EtOH/')
source('../summarySE.R')
#follow the tutorial from these links
#http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization#prepare-the-data

#http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#helper-functions

# bar plots.
# pca

beer <- readxl::read_xlsx('1_beer samples.xlsx',sheet = 1,range = 'A4:J18', col_names = T) # read the procossed ankom excel from teh first sheet

names(beer) %<>% strsplit(split=" ") %>% sapply(`[`,1) %>% strsplit(split='|', fixed = T) %>% sapply(`[`,1)# rename collumns
names(beer)[c(1,7)] <- c('sample','Total Sugar')
beer[beer=="<1"] <- '0' # replace nd ith zero as character
beer[2:nrow(beer),-8] ->x
x[complete.cases(x),]->x

a <-c("E1","CBS7001","WS3470")
b <- c("beer-wort","wine-must")
ba <- "Ctrl"
d <- 1:3

x[,1] <- c(paste(rep(a,each=3),d,sep="_"),paste(ba,d[1],sep = "_"))

x[,-1] <- apply(x[,-1],2,as.numeric)
x %<>% reshape2::melt(id='sample')

x %<>% tidyr::separate(sample, into=c('sample','rep'),sep="_") # separate the sample column into 2, sampe and rep


summarySE(x,groupvars = c("sample","variable"), measurevar = 'value') -> beer4

# turn all the name column into factor
beer4$sample <- factor(beer4$sample, levels = c("E1","CBS7001","WS3470","Ctrl"))

p <- ggplot(beer4, aes(x=sample, y=value, fill=sample,group=variable)) # absolute pressure

p+ geom_bar(position ="dodge", stat = 'identity') + facet_wrap(~variable)+
  geom_signif(comparisons = list(c("CBS7001","E1")),map_signif_level = TRUE)

p+  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.1) +
  geom_line(size=1) +
  geom_point()+
  facet_wrap(~variable)


# plot of
# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.05) # move them .05 to the left and right

#mother of all plots
p + geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=.1, position=pd) +
  geom_line(size=1) +
  geom_point()+ facet_wrap(~variable,scales = 'free')+
  labs(x="Samples",
       y="Concentratoin",
       caption = "Error bars indicate standard deviations") +
  # change the label of y from value to relevant name
  ggtitle("Aroma profile\n in incubated samples") +
  # Set tick every 0.5
  theme_bw() + # change the theme to be black and white
  scale_colour_manual(
    name="Samples", values = c("E1"='red',"WS3470"='blue',"CBS7001"='lightgreen', "Ctrl"= 'grey'), # sample color scheme
    guide=guide_legend(reverse = F)) +
  theme(legend.justification=c(1,0),
        legend.position='none', # move the legend a bit
        plot.title = element_text(hjust = 0.5), # move the title to the centre of the header
        axis.text.x =element_text(size = 9 ,face = "bold.italic", color = "Black",angle = 25,hjust = 1, colour = "grey50"),strip.text = element_text(size = 10)) 

ggsave('wine_Aroma_bar_plots free scale.png',width = 15,height = 15,units = 'in')
ggsave('wine_Aroma_bar_plots free scale.pdf',width = 10,height = 10,units = 'in')

##
#radar plot
beer3[,4] <- log(beer3[,4])
beer3[beer3==-Inf] <- 0
beer4[,c(1,2,4)] %>% reshape2::dcast(sample~variable) ->beer5

p <- ggRadar(beer5, aes(group = sample), 
             rescale = FALSE, legend.position = "left",
             size = 1, interactive = FALSE, use.label = TRUE) +
  facet_wrap(~sample) + 
  scale_y_discrete(breaks = NULL) + # don't show ticks 
  theme(axis.text.x = element_text(size =9)) + # larger label sizes
  # adjust colors of radar charts to uniform colors
  ggtitle("Aroma Characteristics")
print(p)
ggsave('beer_Aroma spider log_transformed scale.png',width = 15,height = 15,units = 'in')
ggsave('beer_Aroma spider log_transformed scale.pdf',width = 15,height = 15,units = 'in')
