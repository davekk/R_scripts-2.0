require(tidyr); # data frame manipulation
require(dplyr); # data frame manipulation
require(ggplot2); #cool plot
require(magrittr) # pipe and related stuff
require(ggsignif)
require(ggfortify)
setwd('F:/Geisenheim PhD/Bea stuff/plots for erbsloh paper/glucose and EtOH/')
source('F:/Geisenheim PhD/Bea stuff/plots for erbsloh paper/summarySE.R')
#follow the tutorial from these links
#http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization#prepare-the-data

#http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#helper-functions



beer <- readxl::read_xlsx('3_PTR2_glucose_acids_etoh.xlsx',sheet = 1,range = 'A4:M22', col_names = F) %>% t() %>% as.data.frame() %>% unite(x, c(V1,V2), sep = " ", remove = T,na.rm=T) %>% t() %>%as.data.frame() %>%janitor::row_to_names(1)%>%tibble::remove_rownames() 
# read the procossed ankom excel from teh first sheet
names(beer)[1] <- 'sample'
#beer <- beer[-c(seq(from=4,to=17, by=4)),-c(14,15)]
beer <- beer[-c(seq(from=4,to=17, by=4)),]
#beer <- beer[complete.cases(beer),] 
## unique names
d <- c('I', 'II', 'III')
a <-  c("EC1118","ptr2-delta","ptr2-locus","E1")
b <- 'Most'
#ba <- c('0','1') # 0-plus, 1-minus


#
beer$sample <-
  #c(paste(rep(a,each =3),d,ba, sep = " "),paste(b,d[1],ba, sep = " "))
  c(paste(rep(a,each=3),d,sep=" "),  paste(b,d[1])) 
# give the samples meaniful names
beer[,-1] <- apply(beer[,-1],2,as.numeric) # convert the df from factor to numeric
beer[is.na(beer)] <- 0 # replace all NAs with `0`

beer %>% reshape2::melt(id='sample')%>% tidyr::separate(sample, into=c('sample','rep'),sep=" ") ->beer2 

beer3 <- summarySE(beer2,groupvars = c("sample","variable"), measurevar = 'value')

# turn all the name column into factor
beer3$sample <- factor(beer3$sample, levels=c("EC1118","ptr2-delta","ptr2-locus","E1",'Most'))
##
#sup.label <- c('Wine must','Wine must with\n Nitrogen supplementation')
#names(sup.label) <- 0:1
p <- 
  beer3 %>% dplyr::filter(grepl('Ethanol Ge',variable)) %>%
  ggplot( aes(x=sample, y=value, fill=sample,group=sample)) # absolute pressure

#p+ geom_bar(position ="dodge", stat = 'identity') + facet_wrap(~variable)
# geom_signif(comparisons = list(c("CBS7001","E1")),map_signif_level = TRUE)

#mother of all plots
#p_no_n2 <- 
p + geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=.1, position=position_dodge(0.05)) +
  geom_bar(position ="dodge", stat = 'identity') +
  facet_wrap(~variable, scales = 'free')+
  labs(x=" ",
       y=" ",
       caption = "Error bars indicate standard deviations") +
  # change the label of y from value to relevant name
  #  ggtitle("Nitrogen fermentation") +
  # Set tick every 0.5
  theme_bw() + # change the theme to be black and white
  scale_fill_manual(#c("EC118","ptr2-delta","ptr2-locus","E1",'Most')
    name="Samples",values = c("EC1118"='blue',"E1"='red',"ptr2-delta"='purple',"ptr2-locus"='violet', "Most"= 'grey'), # sample color scheme
    guide=guide_legend(reverse = F)) +
  theme(legend.justification=c(1,0),
        legend.position='none', # move the legend a bit
        #    plot.title = element_text(hjust = 0.5), # move the title to the centre of the header
        axis.text.x =element_text(size = 9 ,face = "bold.italic", color = "Black",angle = 25,hjust = 1, colour = "grey50"),strip.text = element_text(size = 10)) 

ggsave('./ptr2/ptr2_ferm_bar_plots ethanol free scale.png',width = 15,height = 15,units = 'in')

#ggsave('wine_Aroma_bar_plots free scale.pdf',width = 10,height = 10,units = 'in')

ggpubr::ggarrange(p_no_n, p_no_n2,
                  labels = c("must", "nitrogen suppliment"),
                  ncol = 1, nrow = 2,common.legend = T) 

