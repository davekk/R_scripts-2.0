require(tidyr); # data frame manipulation
require(dplyr); # data frame manipulation
require(ggplot2); #cool plot
require(magrittr) # pipe and related stuff
require(ggsignif)
require(ggfortify)
setwd('F:/Geisenheim PhD/Bea stuff/4_10C FERM/')
source('F:/Geisenheim PhD/Bea stuff/plots for erbsloh paper/summarySE.R')
#follow the tutorial from these links
#http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization#prepare-the-data

#http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#helper-functions

clean_dat <-  function(beer) {
  
  beer <- beer[,-2] # drop empty column
  
  beer <- beer[complete.cases(beer),]  #drop empty rows
  
  beer[beer=="nd"] <- '0' # replace nd ith zero as character
  
  beer[beer=="nq"] <- '0' # replace nq with na
  
  ## convert all column into numeric column except the first column
  
  beer[,-1] <- apply(beer[,-1],2,as.numeric)
  beer$Sample <-c(paste(rep(a,each=3),d,sep=" "))#,paste(rep(a,each=3),d,sep=" ")) # rename the sample column
 # rename the sample column
  sup.label <<-c("Ethyl acetate [mg/L]", "Iso butanol [mg/L]", "Ethyl propionate [ug/L]", "Isoamyl alcohol [mg/L]", "Active amyl alcohol [mg/L]", "i-Ethyl butyrate [µg/L]", "Methyl 2-methylbutyrate [µg/L]", "Ethyl butyrate [ug/L]", "Ethyl lactate [mg/L]", "Valeric acid [µg/L]", "Ethyl 2-methylbutyrate [µg/L]", "Hexanol [µg/L]", "Isoamyl acetate [µg/L]", "Active amyl acetate [µg/L]", "Caproic acid [mg/L]", "Ethyl caproate [µg/L]", "Hexyl acetate [µg/L]", "Ethyl leucinic acid [µg/L]", "2 Phenyl ethanol [mg/L]", "Caprylic acid [mg/L]", "Diethyl succinate [µg/L]", "Ethyl caprylate  [µg/L]", " Ethyl phenylacetate [µg/L]", "Phenethyl acetate [µg/L]", "Capric acid [µg/L]", "Ethyl caprinate [µg/L]")
  names(sup.label)<<- as.character(names(beer)[2:ncol(beer)]) 
  
  beer <- beer[,colSums(beer!=0)>0]

   
  return(beer)
}

beer <- readxl::read_xlsx('201214_final results_Niel_Bea_12 samples_9%.xlsx',sheet = 'Concentration',range = 'A4:AB19', col_names = T) # read the procossed ankom excel from teh first sheet
wine <- readxl::read_xlsx('../5_WINES FERM/201210_final results_Niel_6 samples_9%.xlsx',sheet = 'Concentration',range = 'A4:AB11', col_names = T) # read the procossed ankom excel from teh first sheet
dave <- readxl::read_xlsx('../5_WINES FERM/ciao davies.xlsx',sheet = 'Concentration',range = 'A4:AB19', col_names = T) # read the procossed ankom excel from teh first sheet

#a <-  c("E1","WS3470","CBS7001")
#b <- c("beer-wort","wine-must")
#ba <- "Ctrl"
d <- c('I', 'II', 'III')
a <-  c('E1_mt','EC1118_mt','E1_ch','EC1118_ch')

beer2 <- clean_dat(beer) 
wine2 <-  clean_dat(wine)
dave2 <-  clean_dat(dave)

beer2 %>% reshape2::melt(id='Sample') ->beer3 # melt it in preparation for bar plots

beer4 <- beer3 %>% tidyr::separate(Sample, into=c('sample','rep'),sep=" ") # separate the sample column into 2, sampe and rep


beer4 <- summarySE(beer4,groupvars = c("sample","variable"), measurevar = 'value')

# turn all the name column into factor
beer4$sample <- factor(beer4$sample, levels = c('E1_mt','EC1118_mt','E1_ch','EC1118_ch'))
 ##
#generate naming pattern
subset(sup.label,names(sup.label) %in% as.character(unique(beer4$variable))) ->beer.label


p <- ggplot(beer4, aes(x=sample, y=value, fill=sample,group=sample)) # absolute pressure

p+ geom_bar(position ="dodge", stat = 'identity') + facet_wrap(~variable)+
  geom_signif(comparisons = list(c("CBS7001","E1")),map_signif_level = TRUE)

p +  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.1) +
  geom_line(size=1) +
  #geom_point()+
  facet_grid(~sample)#+


# plot of
# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.05) # move them .05 to the left and right

#mother of all plots
p + geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=.1, position=pd) +
  geom_bar(position ="dodge", stat = 'identity') +
    facet_wrap(~variable,,scales = 'free', labeller = labeller(variable=beer.label))+
  labs(x=" ",
       y=" ",
       caption = "Error bars indicate standard deviations") +
  # change the label of y from value to relevant name
  ggtitle("wine ° fermentation") +
  # Set tick every 0.5
  theme_bw() + # change the theme to be black and white
  scale_fill_manual(
    name="Samples",values = c("E1_ch"='red',"EC1118_ch"='blue',"E1_mt"= 'red','EC1118_mt'='blue'), # sample color scheme
    guide=guide_legend(reverse = F)) +
  theme(legend.justification=c(1,0),
        legend.position='none', # move the legend a bit
        plot.title = element_text(hjust = 0.5), # move the title to the centre of the header
        axis.text.x =element_text(size = 9 ,face = "bold.italic", color = "Black",angle = 25,hjust = 1, colour = "grey50"),strip.text = element_text(size = 10)) 

#ggsave('wine_Aroma_bar_plots free scale.png',width = 15,height = 15,units = 'in')
#ggsave('wine_Aroma_bar_plots free scale.pdf',width = 10,height = 10,units = 'in')


