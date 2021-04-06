# !diagnostics off
require(tidyr); # data frame manipulation
require(dplyr); # data frame manipulation
require(ggplot2); #cool plot
require(magrittr) # pipe and related stuff
require(ggsignif)
require(ggfortify)
library("ggdendro")
library("grid")
setwd('F:/Geisenheim PhD/Bea stuff/5_WINES FERM/')
source('F:/Geisenheim PhD/Bea stuff/plots for erbsloh paper/summarySE.R')
#follow the tutorial from these links
#http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization#prepare-the-data

#http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#helper-functions

#beer <- readxl::read_xlsx('201210_final results_Niel_6 samples_9%.xlsx',sheet = 1,range = 'A4:AB13', col_names = T) 
beer <- readxl::read_xlsx('201214_final results_Niel_Bea_12 samples_9%.xlsx',sheet = 1,range = 'A4:AB19', col_names = T) 
clean_dat <-  function(beer) {
  sup.label <- readxl::read_xlsx('F:/Geisenheim PhD/Bea stuff/aromas translation.xlsx', sheet = 1, range = 'B7:C31', col_names = F) 
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


beer2 <-clean_dat(beer)
a <- c('E1','EC1118')
b <- c('I', 'II', 'III')
beer2[,1] <- 
 paste(c(paste(rep(a,each=3),'18'),paste(rep(a,each=3),'10')),b, sep = '-')

#beer2[1:6,] ->beerx

beerx %>% reshape2::melt(id='Sample')%>% tidyr::separate(Sample, into=c('sample','rep'),sep=" ") ->beer3 

beer4 <- summarySE(beer3,groupvars = c("sample","variable"), measurevar = 'value')


p <- beer4 %>% 
  ggplot( aes(x=sample, y=value, fill=sample,group=sample)) # absolute pressure

pd <- position_dodge(0.05)

#p_no_n2 <- 
p + geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=.1,position=pd) +
  geom_bar(position ="dodge", stat = 'identity') +
  facet_wrap(~variable,scales = 'free',drop = TRUE)+
  labs(x=" ",
       y=" ",
       caption = "Error bars indicate standard deviations") +
  theme_bw() + # change the theme to be black and white #                                          
  scale_fill_manual( #c("E1 10"='red1',"EC1118 10"='blue1',"E1 18"='red3',"EC1118 18"='blue3')
    name="Samples", values = c("E1"='red',"EC1118"='blue'),  guide=guide_legend(reverse = F)) +
  theme(legend.justification=c(1,0),
        legend.position='none', # move the legend a bit
        #    plot.title = element_text(hjust = 0.5), # move the title to the centre of the header
        axis.text.x =element_blank()) #element_text(size = 9 ,face = "bold.italic", color = "Black",angle = 25,hjust = 1, colour = "grey50"),strip.text = element_text(size = 10)
          #) 

#ggsave('./acid/cold_ferm_bar_plots all free scale.png')#,width = 15,height = 15,units = 'in')

rownames(beer2) <-NULL
names(beer2)[1] <- 'sample'
beer1 <-  beer2
beer1[,3:ncol(beer1)] %<>% scale() # Scale each measurement (independently) to have a mean of 0 and variance of 1
beer1[,3:ncol(beer1)] <- scale(beer1[,3:ncol(beer1)], center = FALSE, scale = apply(beer1[,3:ncol(beer1)], 2, sd, na.rm = TRUE))
# Run clustering
beer1.matrix <- as.matrix(beer1[, -c(1,2)])
rownames(beer1.matrix) <- paste(beer1$sample, beer1$date)
beer.dendro <- as.dendrogram(hclust(d = dist(x = beer1.matrix)))

# Create dendrogram plot
dendro.plot <- ggdendrogram(data = beer.dendro) + 
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank())

plot(dendro.plot)
# Heatmap

# Data wrangling
beer1$sample <- paste(beer1$sample, beer1$date)
beer1$date <- NULL
beer.long <- reshape2::melt(beer1, id = "sample") 
# Extract the order of the tips in the dendrogram

beer.order <- order.dendrogram(beer.dendro)
# Order the levels according to their position in the cluster
beer.long$sample <- factor(x = beer.long$sample,
                           levels = beer1$sample[beer.order],  
                           ordered = TRUE)

# Create heatmap plot
heatmap.plot <- 
  ggplot(data = beer.long, aes(y = variable, x = sample)) +
  geom_tile(aes(fill = value),colour = "white") +
  scale_fill_gradient2() +
  theme(#axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x =element_text(angle = 90,hjust = 1, colour = "grey50"),strip.text = element_text(size = 10),
    legend.position = "bottom")+
  labs(caption = "colour scheme calculated based on SD of each column") 

#ht1 =
#BiocManager::install("ComplexHeatmap")


ggpubr::ggarrange(dendro.plot, heatmap.plot,
                  labels = " ",
                  ncol = 1, nrow = 2, align = "v") 

#grid.newpage()
#print(heatmap.plot, vp = viewport(x = 0.4, y = 0.5, width = 0.8, height = 1.0))
#print(dendro.plot, vp = viewport(x = 0.50, y = 0.63, width = 0.9, height = 0.5))
#####



## heat map
gen_prcomp <- function(beer){
  
  #combined %<>% tidyr::separate(Sample, into=c('sample','rep'),sep=" ")
  
  beer2 <-   beer %>%  reshape2::melt(id=c('sample','date'))%>% tidyr::separate(sample, into=c('sample','rep'),sep="_")%>%
    mutate(val= ifelse(str_detect(variable, "mg"),value*1000,value*1))%>%
    reshape2::dcast(sample +date+rep~variable,value.var = 'val')
  # change all rows into same units
  #beer4[,c(1,2,3,4)] %>% reshape2::dcast(sample~variable) ->beer5
  beer2[is.na(beer2)] <- 'I'
  
  #beer2 %<>%
  # mutate(sample =  factor(sample, levels = c("EC1118","SFEwt","TFL2","EC1118-SFEwt", "EC1118-TFL2","random must", "must"))) %>%
  #arrange(sample)
  rownames(beer2)<- paste(beer2$sample,beer2$rep,beer2$date, sep = ' ') # add rownames to the df
  
  return(beer2)
}

# apply terrible function2

beer5 <- gen_prcomp(beer2)



plot_pc <-  function(beer) {
  beer_new <- beer[,4:ncol(beer)] 
  
  #
  pca_beer <-  prcomp(beer_new, scale. = TRUE)
  return(pca_beer)
}


#
beer_pca <- plot_pc(beer5)
autoplot(beer_pca,data = beer5 , colour='sample',label = TRUE,label.size=3)+ # change the theme to be black and white #                                          
  scale_colour_manual(name="Samples", values = c("E1 (S.uvarum)"='red',"EC (EC1118)"='blue',"F5 (GMO)"= 'green',"F11 (GMO)"='lightgreen',"F12 (GMO)"='darkgreen',"MT-Müller Th. C-must"='grey'),  guide=guide_legend(reverse = F))
