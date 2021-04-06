# !diagnostics off
require(tidyr); # data frame manipulation
require(dplyr); # data frame manipulation
require(ggplot2); #cool plot
require(magrittr) # pipe and related stuff
require(ggsignif)
require(ggfortify)
library("ggdendro")
library("grid")
setwd('F:/Geisenheim_PhD/Bea stuff/raw data/')
source('F:/Geisenheim_PhD/Bea stuff/plots for erbsloh paper/summarySE.R')
#follow the tutorial from these links
#http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization#prepare-the-data

#http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#helper-functions

cold_aroma <- readxl::read_xlsx('cold fermentation and 750 MT aromas.xlsx',sheet = 1,range = 'A4:AB19', col_names = T) 

cold_sugar <- readxl::read_xlsx('cold fermentation and 750 MT sugars and acids.xlsx',sheet = 1,range = 'A4:P22', col_names = F) %>% t() %>% as.data.frame() %>% unite(x, c(V1,V2), sep = " ", remove = T,na.rm=T) %>% t() %>%as.data.frame() %>%janitor::row_to_names(1)%>%tibble::remove_rownames()

n_boost_sugar <- readxl::read_xlsx('nitrogen boosted sugar and acids.xlsx',sheet = 1,range = 'A4:P31', col_names = F) %>% t() %>% as.data.frame() %>% unite(x, c(V1,V2), sep = " ", remove = T,na.rm=T) %>% t() %>%as.data.frame() %>%janitor::row_to_names(1)%>%tibble::remove_rownames()

clean_dat <-  function(beer) {
  sup.label <- readxl::read_xlsx('F:/Geisenheim_PhD/Bea stuff/aromas translation.xlsx', sheet = 1, range = 'B7:C31', col_names = F) 
  names(sup.label) <- c('old','new')
  
  # drop empty column
  not_all_na <- function(x) any(!is.na(x))
  beer %<>% select(where(not_all_na))
  
  
  beer <- beer[complete.cases(beer),] 
  
  beer[beer=="nd"] <- '0' # replace nd ith zero as character
  
  beer[beer=="nq"] <- '0' # replace nq with na
  
  beer[,-1] <- apply(beer[,-1],2,as.numeric) # convert the df from factor to numeric
  
  
  
  beer2 <- beer[,colSums(beer!=0)>0]
  
  names(beer2)[2:ncol(beer2)] <- 
    subset(sup.label,sup.label$old %in% names(beer2)[2:ncol(beer2)])[,2] %>% unlist() %>% as.character()
  return(beer2)
}

clean_dat2 <- function(beer) {
   # drop empty column
  beer <- beer[-c(seq(from=4,to=27, by=4)),-c(2,14,15)]
   
  beer[,-1] <- apply(beer[,-1],2,as.numeric) # convert the df from factor to numeric
  beer[is.na(beer)] <- 0 # replace all NAs with `0`
 
  return(beer)
}

cold_aroma %<>% clean_dat() 
cold_sugar %<>% clean_dat2()
n_boost_sugar %<>% clean_dat2()
n_boost_sugar <-  n_boost_sugar[c(1:3,13:15,19),]

## alcohols and esters
#sel <- c("Sample","Ethyl acetate (mg/L)", "Ethyl propionate (µg/L)", "Ethyl isobutyrate (µg/L)","Ethyl butyrate (µg/L)","Ethyl 2-methyl butanoate (µg/L)", "Isoamyl acetate (µg/L)","2-Methylbutyl acetate (µg/L)" ,"Ethyl hexanoate (µg/L)","Hexyl acetate (µg/L)","Ethyl octanoate (µg/L)","Ethyl benzaoate (µg/L)","Phenethyl acetate (µg/L)","Ethyl decanoate (µg/L)")
ester_aroma <- cold_aroma[,c(1,2,4,7,8,10,12,13,15,16,19,20,21,23)]
alco_aroma <- cold_aroma[,c(1,3,5,6,11,17)]
acids <- cold_aroma[,c(1,9,14,18,22)]

##pca
gen_prcomp <- function(beer){
  
  #combined %<>% tidyr::separate(Sample, into=c('sample','rep'),sep=" ")
  beer2 <- beer %>% tidyr::separate(Sample, into=c('sample','rep','cond'),sep="_") %>% 
    reshape2::melt(id=c('sample','rep','cond')) %>%
    mutate(val= ifelse(stringr::str_detect(variable, "mg"),value*1000,value*1))%>%reshape2::dcast(sample +rep+cond~variable,value.var = 'val',fun.aggregate = mean)
  # change all rows into same units
  #beer4[,c(1,2,3,4)] %>% reshape2::dcast(sample~variable) ->beer5
 # beer2[is.na(beer2)] <- 'I'
  
  #beer2 %<>%
  # mutate(sample =  factor(sample, levels = c("EC1118","SFEwt","TFL2","EC1118-SFEwt", "EC1118-TFL2","random must", "must"))) %>%
  #arrange(sample)
  rownames(beer2)<- paste(beer2$sample,beer2$rep,beer2$cond, sep = ' ') # add rownames to the df
  
  return(beer2)
}

# apply terrible function2

alc <- gen_prcomp(alco_aroma)
acid <- gen_prcomp(acids)
ester <- gen_prcomp(ester_aroma)


plot_pc <-  function(beer) {
  beer_new <- beer[,4:ncol(beer)] 
  
  #
  pca_beer <-  prcomp(beer_new, scale. = TRUE)
  return(pca_beer)
}

#

alc_pca <- plot_pc(alc)
acid_pca <-plot_pc(acid) 
ester_pca<-plot_pc(ester)

#p_comb <- 
autoplot(ester_pca,data = ester , colour='rep',label = TRUE,label.size=3)+theme(legend.position = 'none')



### barplots
sum_stuff <- function(beer) {
  names(beer)[1] <- 'Sample'
#  beer[nrow(beer),1] <-c('MT_Most')
beer %>% reshape2::melt(id=c('Sample'))%>% tidyr::separate(Sample, into=c('non','sample','rep'),sep="_") ->beer3 

beer4 <- summarySE(beer3,groupvars = c("sample",'rep',"variable"), measurevar = 'value')

order.beer <- unique(as.character(unlist(beer4[,1])))

# turn all the name column into factor
beer4$sample <- factor(beer4$sample, levels=order.beer, ordered = T)
#
##
return(beer4)
}

acid2 <- sum_stuff(acids)
alco2 <- sum_stuff(alco_aroma)
ester2 <- sum_stuff(ester_aroma)



p_10 <-
  alco2%>% filter(rep=="10°") %>% 
  ggplot( aes(x=sample, y=value, fill=sample,group=sample)) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=.1,position=position_dodge(0.05)) +
  geom_bar(position ="dodge", stat = 'identity') +
  facet_wrap(~variable,scales = 'free',drop = TRUE)+
  labs(x=" ",
       y=" ",
       caption = " ") +
  theme_bw() + # change the theme to be black and white #                                          
  scale_fill_manual(
    name="Samples", values = c("E1"='#DC3220',"EC1118"='#005AB5'),  guide=guide_legend(reverse = F)) +
  theme(legend.justification=c(1,0),
        legend.position='none', # move the legend a bit
        #    plot.title = element_text(hjust = 0.5), # move the title to the centre of the header
        axis.text.x =element_text(size = 9 ,face = "bold.italic", color = "Black",angle = 25,hjust = 1, colour = "grey50"),strip.text = element_text(size = 10)) 

ggpubr::ggarrange(p_10, p_18,
                  labels = c("",""),
                  ncol = 2, nrow = 1, align = "v",vjust = 33) 


### heatmap
heat_dendro <- function(beer){

  
rownames(beer) <-NULL
d <- c('I', 'II', 'III')
beer[,2:ncol(beer)] %<>% log() 
#beer[,2:ncol(beer)] <- scale(beer[,2:ncol(beer)], center = FALSE, scale = apply(beer[,2:ncol(beer)], 2, sd, na.rm = TRUE))
# Run clustering
#beer.matrix <- as.matrix(beer[,-1])
#rownames(beer.matrix) <- paste(beer$Sample,d)
#beer.dendro <- as.dendrogram(hclust(d = dist(x = beer.matrix)))

# Create dendrogram plot
#dendro.plot <- ggdendrogram(data = beer.dendro) + 
 # theme(axis.text.y = element_blank(),
  #      axis.text.x = element_blank())

#plot(dendro.plot)
# Heatmap

# Data wrangling
beer$Sample <- paste(beer$Sample,d)
beer.long <- 
  reshape2::melt(beer, id = "Sample") #%>% tidyr::separate(Sample, into=c('Sample','rep'),sep=" ")%>%
  #  summarySE(groupvars = c("Sample","variable"), measurevar = 'value')
# Extract the order of the tips in the dendrogram

#beer.order <- order.dendrogram(beer.dendro)
# Order the levels according to their position in the cluster
beer.long$Sample <- factor(x = beer.long$Sample,
                           levels = unique(as.character(unlist(beer.long$Sample))),  
                           ordered = TRUE)

# Create heatmap plot
#heatmap.plot <- 
  ggplot(data = beer.long, aes(y = variable, x = Sample)) +
  geom_tile(aes(fill = value),colour = "white") +
  scale_fill_gradient(low = "steelblue", high = "red") +
  theme_grey(base_size = 9) +
   scale_x_discrete(expand = c(0, 0)) +scale_y_discrete(expand = c(0, 0))+
  theme(#axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x =element_text(angle = 90,hjust = 1, colour = "grey50"),strip.text = element_text(size = 10),
    legend.position = "bottom") +
  labs(caption = "colour scheme calculated based on log transformed values of each column") 
 

#ht1 =
#BiocManager::install("ComplexHeatmap")

  
 #labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) +scale_y_discrete(expand = c(0, 0)) + opts(legend.position = "none", axis.ticks = theme_blank(), axis.text.x = theme_text(size = base_size *0.8, angle = 330, hjust = 0, colour = "grey50"))
#ggpubr::ggarrange(dendro.plot, heatmap.plot,
 #                 labels = " ",
  #                ncol = 1, nrow = 2, align = "v") 
}

heat_dendro(acids)
heat_dendro(ester_aroma)
heat_dendro(alco_aroma)

##
beer_rena <- function(beer,te){
  d <- c('I', 'II', 'III')
  beer$Sample <- paste(beer$Sample,d)
  beer[,2:ncol(beer)] %<>% log()
  beer %<>%reshape2::melt(id='Sample')
  beer$face <-te

  return(beer)
  }

bind_rows(beer_rena(acids,te='Acids'),beer_rena(ester_aroma,te='Esters'),beer_rena(alco_aroma,te='Alcohols')) ->reset_aroma

reset_aroma$Sample <- factor(x = reset_aroma$Sample,
                           levels = unique(as.character(unlist(reset_aroma$Sample))),  
                           ordered = TRUE)

reset_aroma[reset_aroma=='-Inf'] <-NA

# Create heatmap plot
#heatmap.plot <- 
reset_aroma  %>%filter(!grepl('benz',variable))%>%
ggplot(aes(y = variable, x = Sample),na.rm=T) +
  geom_tile(aes(fill = value),colour = "white", na.rm = T) +
  scale_fill_gradient(low = "#FEFE62", high = "#D35FB7") +
  theme_grey(base_size = 9) +
  scale_x_discrete(expand = c(0, 0),position = "top")  +scale_y_discrete(expand = c(0, 0))+
  theme(#axis.text.x = element_blank(),
    strip.text.x = element_blank(), # remove facet labels
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x =element_text(angle = 90,hjust = 1, colour = "grey50"),strip.text = element_text(size = 10),
    legend.position = "bottom") +
  labs(caption = "colour scheme calculated based on log transformed values of each column") +coord_flip()+
  facet_grid(~face, scales = 'free', space = 'free')


# sugar plots
cold_sugar
n_boost_sugar
foo <- function(beer,da) {
  names(beer)[1] <- 'Sample'
  d<- c('I','II','III')
  beer$Sample <- paste(beer$Sample,d)
  
  beer%>% reshape2::melt(id=c('Sample'))%>% tidyr::separate(Sample, into=c('sample','rep'),sep=' ')  ->beer3
  beer3[is.na(beer3)] <-'Most'
  beer4 <- summarySE(beer3,groupvars = c("sample","variable"), measurevar = 'value')
  
 # order.beer <- unique(as.character(unlist(beer4[,1])))
  
  # turn all the name column into factor
 # beer4$sample <- factor(beer4$sample, levels=order.beer, ordered = T)
  #
  beer4$non <-da
  return(beer4)}

n_boost_sugar2 <-foo(n_boost_sugar,'boost')
cold_sugar2 <- foo(cold_sugar,'cold')

#n_boost_sugar2$sample <- as.character(n_boost_sugar2$sample)

n_boost_sugar2$sample[n_boost_sugar2$sample=='E1+'] <-'E1'
n_boost_sugar2$sample[n_boost_sugar2$sample=='EC1118+'] <-'EC1118'


#bind_rows(n_boost_sugar2,cold_sugar2) ->mom

#mom<-mom[!is.na(mom$sd),]

p_18<- 
  #n_boost_sugar2 %>%
  cold_sugar2 %>% tidyr::separate(sample, into=c('non','sample','rep'),sep='_') %>%
  drop_na(sd)%>%
 
   #dplyr::filter( non=='cold') %>%
 dplyr::filter(! rep=="10°") %>% 
  dplyr::filter(grepl('Ethanol',variable))%>%# Glycerin
   # dplyr::filter(grepl('Weinsäure|Äpfelsäure|Milchsäure|Essigsäure|Zitronensäure ',variable))%>% 
  ggplot( aes(x=sample, y=value, fill=sample,group=sample))+ # absolute pressure

#geom_bar(position ="dodge", stat = 'identity') + facet_wrap(~variable)
 #geom_signif(comparisons = list(c("EC111*","E1")),map_signif_level = TRUE)

#mother of all plots

   geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=.1, position=position_dodge(0.05)) +
  geom_bar(position ="dodge", stat = 'identity') +
  facet_wrap(~variable, scales = 'free')+
  labs(x=" ",
       y=" ",
       caption = "Error bars indicate standard deviations") +
  # change the label of y from value to relevant name
  #  ggtitle("Nitrogen fermentation") +
  # Set tick every 0.5
  theme_bw() + # change the theme to be black and white
  scale_fill_manual(
    name="Samples",values = c("E1"='#DC3220',"EC1118"='#005AB5'), # sample color scheme
    guide=guide_legend(reverse = F)) +
  theme(legend.justification=c(1,0),
        legend.position='none', # move the legend a bit
        #    plot.title = element_text(hjust = 0.5), # move the title to the centre of the header
        axis.text.x =element_text(size = 9 ,face = "bold.italic", color = "Black",angle = 25,hjust = 1, colour = "grey50"),strip.text = element_text(size = 10)) 

egg::ggarrange(p_10, p_18,p_b,
                    labels = c(" "," ",' '),
                    nrow= 3 ) 
  