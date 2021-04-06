# !diagnostics off
require(tidyr); # data frame manipulation
require(dplyr); # data frame manipulation
require(ggplot2); #cool plot
require(magrittr) # pipe and related stuff
require(ggsignif)
require(ggfortify)
library("ggdendro")
library("grid")
setwd('F:/Geisenheim_PhD/Bea stuff/new data april/')
source('F:/Geisenheim_PhD/Bea stuff/plots for erbsloh paper/summarySE.R')
#follow the tutorial from these links
#http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization#prepare-the-data

#http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#helper-functions
amino <- 
  readxl::read_xlsx('aminoacid result fot genes.xlsx',sheet = 1, col_names = F) %>% t() %>% as.data.frame() %>% unite(x, c(V1,V2), sep = " ", remove = T,na.rm=T) %>%as.data.frame() %>%tibble::remove_rownames()



### barplots
sum_stuff <- function(beer) {
   names(beer)[2:ncol(beer)] <- beer[1,][2:ncol(beer)] %>%unlist %>%as.character()
  beer <- beer[-1,]
  names(beer)[1] <- 'Sample'
 beer[] <-lapply(beer, as.character)
  beer[beer=="n.n."] <- '0'
  
  beer[,-1] <- apply(beer[,-1],2,as.numeric)
  beer <- beer[,colSums(beer!=0)>0]
  
  beer %>% reshape2::melt(id=c('Sample'))%>% tidyr::separate(Sample, into=c('sample','rep'),sep=" ") %>%
    filter(!sample=="EC")->beer2 
  
  beer4 <- summarySE(beer2,groupvars = c("sample",'variable'), measurevar = 'value')
  
  order.beer <- unique(as.character(unlist(beer4[,1])))
  
  # turn all the name column into factor
  beer4$sample <- factor(beer4$sample, levels=order.beer, ordered = T)
  #
  ##
  return(beer4)
}


amino %>% sum_stuff() -> amino2

  amino2 %>% #filter(variable=="Total [mg AS / L]") %>%
    filter(!sample=="Must") %>%
  ggplot( aes(x=sample, y=value,fill=sample,group=sample)) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=.1,position=position_dodge(0.05)) +
  geom_bar(position ="dodge", stat = 'identity') +
  facet_wrap(~variable,scales = 'free',drop = TRUE)+
  labs(x=" ",
       y=" ",
       caption = " ") +
  theme_bw() +
  # scale_y_continuous(breaks=pretty(amino2$value, n=100))+# change the theme to be black and white #                                          
  #scale_fill_manual(    name="Samples", values = c("E1"='#DC3220',"EC1118"='#005AB5'),  guide=guide_legend(reverse = F)) +
  theme(legend.justification=c(1,0),
        legend.position='none', # move the legend a bit
        #    plot.title = element_text(hjust = 0.5), # move the title to the centre of the header
        axis.text.x =element_text(size = 9 ,face = "bold.italic", color = "Black",angle = 25,hjust = 1, colour = "grey50"),strip.text = element_text(size = 10)) 

