require(tidyr); # data frame manipulation
require(dplyr); # data frame manipulation
require(ggplot2); #cool plot
require(magrittr) # pipe and related stuff
require(ggsignif)
require(ggfortify)
library("ggdendro")
library("grid")
setwd('F:/Geisenheim PhD/Bea stuff/Feb_08_plot/')
source('F:/Geisenheim PhD/Bea stuff/plots for erbsloh paper/summarySE.R')
#follow the tutorial from these links
#http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization#prepare-the-data

#http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#helper-functions


beer <- readxl::read_xlsx('glucose and acids fot.xlsx',sheet = 1,range = 'A4:Q26', col_names = F) %>% t() %>% as.data.frame() %>% unite(x, c(V1,V2), sep = " ", remove = T,na.rm=T) %>% t() %>%as.data.frame() %>%janitor::row_to_names(1)%>%tibble::remove_rownames() 
# read the procossed ankom excel from teh first sheet
names(beer)[1] <- 'sample'
beer <- beer[-c(seq(from=4,to=21, by=4)),-c(15,16)]
order.beer <- unique(as.character(unlist(beer3[,1])))
a <- c('I','II','III')

beer[,1] <-  paste(as.character(unlist(beer[,1])), a, sep = '_')

beer[,-1] <- apply(beer[,-1],2,as.numeric) # convert the df from factor to numeric
beer[is.na(beer)] <- 0 # replace all NAs with `0`

beer %>% reshape2::melt(id='sample')%>% tidyr::separate(sample, into=c('sample','rep'),sep="_") ->beer2 

beer3 <- summarySE(beer2,groupvars = c("sample","variable"), measurevar = 'value')

# turn all the name column into factor

beer3$sample <- factor(beer3$sample, levels=order.beer, ordered = T)


#pattern to ignore
pattern <- c("Ethanol", "Glycerin") 
p <-  
  beer3 %>% dplyr::filter(!grepl(paste(pattern, collapse="|"),variable)) %>%
  ggplot( aes(x=sample, y=value, fill=sample,group=sample)) # absolute pressure

 
p + geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=.1, position=position_dodge(0.05)) +
  geom_bar(position ="dodge", stat = 'identity') +
  facet_wrap(~variable, scales = 'free')+
  labs(x=" ",
       y=" ",
       caption = "Error bars indicate standard deviations") +
  theme_bw() + # change the theme to be black and white
  scale_fill_manual(
    name="Samples",values = c("E1"='red',"EC1118"='blue',"FOT 5"= 'green',"FOT 11"='lightgreen',"FOT 12"='darkgreen',"MUST"='grey'), # sample color scheme
    guide=guide_legend(reverse = F)) +
  theme(legend.justification=c(1,0),
        legend.position='none', # move the legend a bit
        #    plot.title = element_text(hjust = 0.5), # move the title to the centre of the header
        axis.text.x =element_text(size = 9 ,face = "bold.italic", color = "Black",angle = 25,hjust = 1, colour = "grey50"),strip.text = element_text(size = 10)) 

ggsave('./acid/cold_ferm_bar_plots all free scale.png')#,width = 15,height = 15,units = 'in')

#ggsave('wine_Aroma_bar_plots free scale.pdf',width = 10,height = 10,units = 'in')

ggpubr::ggarrange(p_no_n, p_no_n2,
                  labels = c("must", "nitrogen suppliment"),
                  ncol = 1, nrow = 2,common.legend = T) 

## heat map
