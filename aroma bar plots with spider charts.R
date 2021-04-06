# !diagnostics off
require(tidyr); # data frame manipulation
require(dplyr); # data frame manipulation
require(ggplot2); #cool plot
require(reshape2); # convert data into wide and long format
require(magrittr) # pipe and related stuff
#install.packages('gganimate')
require(fmsb);
#devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE) # you may need to run this command twice
require(ggradar);
#install.packages('ggiraphExtra')
require(ggiraphExtra)
#install.packages('ggsignif')
require(ggsignif)
require(ggfortify)
library("ggdendro")
library("grid")
setwd('F:/Geisenheim PhD/Bea stuff/plots for erbsloh paper/Aromas/')
source('F:/Geisenheim PhD/Bea stuff/plots for erbsloh paper/summarySE.R')
#follow the tutorial from these links
#http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization#prepare-the-data

#http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#helper-functions

clean_dat <-  function(beer) {
  
  sup.label <- readxl::read_xlsx('F:/Geisenheim PhD/Bea stuff/aromas translation.xlsx', sheet = 1, range = 'B7:C31', col_names = F) 
  names(sup.label) <- c('old','new')
  
  # drop empty column
  not_all_na <- function(x) any(!is.na(x))
  beer %<>% select(where(not_all_na))
  
  beer <- beer[complete.cases(beer),]  #drop empty rows
  
  beer[beer=="nd"] <- '0' # replace nd ith zero as character
  
  beer[beer=="nq"] <- '0' # replace nq with na
  
  ## convert all column into numeric column except the first column
  
  beer[,-1] <- apply(beer[,-1],2,as.numeric)
  beer$Sample <-  c(paste(rep(a,each=3),d,sep=" "),paste(ba,d[1],sep = " ")) 
   beer2 <- beer[,colSums(beer!=0)>0]
   
   names(beer2)[2:ncol(beer2)] <- 
     subset(sup.label,sup.label$old %in% names(beer2)[2:ncol(beer2)])[,2] %>% unlist() %>% as.character()
  return(beer2)
}

beer <- readxl::read_xlsx('1_beer samples aromas.xlsx',sheet = 'Concentration',range = 'A4:AB17', col_names = T) # read the procossed ankom excel from teh first sheet
wine <- readxl::read_xlsx('1_wine samples aromas.xlsx',sheet = 'Concentration',range = 'A4:AB17', col_names = T) # read the procossed ankom excel from teh first sheet


a <-  c("E1","WS3470","CBS7001")
b <- c("beer-wort","wine-must")
ba <- "Ctrl"
d <- c('I', 'II', 'III')

beer2 <- clean_dat(beer) 
wine2 <-  clean_dat(wine)

# rename the sample column
beer3 <- wine2 %>% reshape2::melt(id='Sample') %>% tidyr::separate(Sample, into=c('sample','rep'),sep=" ") # melt it in preparation for bar plots

#beer3$rep[is.na(beer3$rep)] <-1 # replace all Na with 1

#the function to make our life easier
#apply our new function on our data, using the samples and variable, and use the value as input. This collapses the rep column
beer3 <- summarySE(beer3,groupvars = c("sample","variable"), measurevar = 'value')

# turn all the name column into factor
beer3$sample <- factor(beer3$sample, levels = c("E1","CBS7001","WS3470","Ctrl"))

p <- ggplot(beer3, aes(x=sample, y=value, fill=sample,group=sample)) # absolute pressure

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
  geom_bar(position ="dodge", stat = 'identity') + facet_wrap(~variable,scales = 'free')+
  labs(x="Samples",
      y="Concentratoin",
      caption = "Error bars indicate standard deviations") +
  # change the label of y from value to relevant name
  #ggtitle("Aroma profile\n in incubated samples") +
  # Set tick every 0.5
  theme_bw() + # change the theme to be black and white
  scale_fill_manual(
    name="Samples", values = c("E1"='red',"WS3470"='blue',"CBS7001"='darkgreen', "Ctrl"= 'grey'), # sample color scheme
    guide=guide_legend(reverse = F)) +
  theme(legend.justification=c(1,0),
        legend.position='none', # move the legend a bit
      #  plot.title = element_text(hjust = 0.5), # move the title to the centre of the header
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

##
require(stringr)
#install.packages('ggfortify')
install.packages('ggbiplot')
require(ggfortify)


beer3$variable %>% str_detect("mg")

# change all rows into same units
beer3 %>% 
  mutate(val= ifelse(str_detect(variable, "mg"),value*1000,value*1)) ->beer4
beer4[,c(1,2,4)] %>% reshape2::dcast(sample~variable) ->beer5

beer5 %<>% tidyr::separate(sample, into=c('sample','rep'),sep="_")

beer_new <- beer5[,3:ncol(beer5)]

pca_beer <-  prcomp(beer_new, scale. = TRUE)

autoplot(pca_beer,data = beer5, colour='sample', loadings=T) # show eigen vectors.

autoplot(pca_beer,data = beer5, colour='sample',frame=T) #  draw frames

autoplot(pca_beer,data = beer5, colour='sample',frame=T, frame.type='norm') #  draw frames

###
#pca
gen_prcomp <- function(beer){
  
  #combined %<>% tidyr::separate(Sample, into=c('sample','rep'),sep=" ")
  beer2 <- beer %>% tidyr::separate(Sample, into=c('sample','rep'),sep=" ") %>% 
    reshape2::melt(id=c('sample','rep')) %>%
    mutate(val= ifelse(stringr::str_detect(variable, "mg"),value*1000,value*1))%>%
    reshape2::dcast(sample +rep~variable,value.var = 'val')
 
  beer2 %<>%
   mutate(sample =  factor(sample, levels = c("E1","WS3470","CBS7001", "Ctrl"))) %>%
  arrange(sample)
  rownames(beer2)<- paste(beer2$sample,beer2$rep, sep = ' ') # add rownames to the df
  
  return(beer2)
}

beer3 <- gen_prcomp(beer2)
wine3 <- gen_prcomp(wine2)

plot_pc <-  function(beer) {
  beer_new <- beer[,3:ncol(beer)] 
  
  #
  pca_beer <-  prcomp(beer_new, scale. = TRUE)
  return(pca_beer)
}

beer_pca <- plot_pc(beer3)
wine_pca <- plot_pc(wine3)

autoplot(beer_pca,data = beer3 , colour='sample',frame=T,label = TRUE,label.size=4) 
autoplot(wine_pca,data = wine3 , colour='sample',frame=T,label = TRUE,label.size=4)

autoplot(beer_pca,data = beer3, colour='sample',frame=T, frame.type='norm') #  draw frames ellipse

##

rownames(beer2) <-NULL
names(beer2)[1] <- 'sample'
beer1 <-  beer2
beer1[,2:ncol(beer1)] %<>% scale() # Scale each measurement (independently) to have a mean of 0 and variance of 1

# Run clustering
beer1.matrix <- as.matrix(beer1[, -1])
rownames(beer1.matrix) <- beer1$sample
beer.dendro <- as.dendrogram(hclust(d = dist(x = beer1.matrix)))

# Create dendrogram plot
dendro.plot <- ggdendrogram(data = beer.dendro) + 
  theme(axis.text.y = element_text(size = 6))

# Heatmap

# Data wrangling
beer.long <- reshape2::melt(beer1, id = "sample")
# Extract the order of the tips in the dendrogram
beer.order <- order.dendrogram(beer.dendro)
# Order the levels according to their position in the cluster
beer.long$sample <- factor(x = beer.long$sample,
                           levels = beer$sample[beer.order], 
                           ordered = TRUE)

# Create heatmap plot
heatmap.plot <- ggplot(data = beer.long, aes(y = variable, x = sample)) +
  geom_tile(aes(fill = value),colour = "white") +
  scale_fill_gradient2() +
  theme(axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom")  

ggpubr::ggarrange(dendro.plot, heatmap.plot,
                  labels = "none",
                  ncol = 1, nrow = 2, align = "v") 

#grid.newpage()
#print(heatmap.plot, vp = viewport(x = 0.4, y = 0.5, width = 0.8, height = 1.0))
#print(dendro.plot, vp = viewport(x = 0.50, y = 0.63, width = 0.9, height = 0.5))
