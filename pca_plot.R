require(readxl); # read excel files
require(tidyr); # data frame manipulation
require(dplyr); # data frame manipulation
require(ggplot2); #cool plot
require(reshape2); # convert data into wide and long format
require(magrittr) # pipe and related stuff
#install.packages('gganimate')
require(ggsignif)
require(stringr)
#install.packages('ggfortify')
#install.packages('ggbiplot')
require(ggfortify)
source('../Bea stuff/plots for erbsloh paper/summarySE.R')
setwd('F:/Geisenheim PhD/Akan data/')

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

 # beer <- beer[,colSums(beer!=0)>0]
  return(beer)
}

# read the data
june <- readxl::read_xlsx('June fermentation M2.xlsx',sheet = 1,range = 'A4:AB23', col_names = T)
aug_1 <- readxl::read_xlsx('200820_final results_GCMS_Aug_I_19samples.xlsx',sheet = 1,range = 'A4:AB29', col_names = T)

aug_2 <- readxl::read_xlsx('200908_final results_GCMS_Aug_II_19samples.xlsx',sheet = 1,range = 'A4:AB29', col_names = T) 
cellar <- readxl::read_xlsx('200907_final results_Madina_cellar samples.xlsx',sheet = 1,range = 'A4:AB20', col_names = T) 
cell2 <- readxl::read_xlsx('201112_final results_Madina_6samples.xlsx',sheet = 1,range = 'A4:AB11', col_names = T) 
cell <- readxl::read_xlsx('201105_final results_Madina_13samples.xlsx',sheet = 1,range = 'A4:AB21', col_names = T) 

cell <- bind_rows(mutate_all(cell, as.character), mutate_all(cell2, as.character))
# apply the terrible function1
june <-  clean_dat(june)
aug_1 <- clean_dat(aug_1)
aug_1$Sample <- june$Sample
aug_2 <-  clean_dat(aug_2)
aug_2$Sample <- june$Sample


cellar1 <- clean_dat(cellar)
cell1 <- clean_dat(cell)

##pca
gen_prcomp <- function(beer){
  
#combined %<>% tidyr::separate(Sample, into=c('sample','rep'),sep=" ")
  beer2 <- beer %>% tidyr::separate(Sample, into=c('sample','rep'),sep=" ") %>% 
    reshape2::melt(id=c('sample','rep')) %>%
    mutate(val= ifelse(str_detect(variable, "mg"),value*1000,value*1))%>%
    reshape2::dcast(sample +rep~variable,value.var = 'val')
  # change all rows into same units
  #beer4[,c(1,2,3,4)] %>% reshape2::dcast(sample~variable) ->beer5
beer2[is.na(beer2)] <- 'I'
  
  #beer2 %<>%
   # mutate(sample =  factor(sample, levels = c("EC1118","SFEwt","TFL2","EC1118-SFEwt", "EC1118-TFL2","random must", "must"))) %>%
    #arrange(sample)
  rownames(beer2)<- paste(beer2$sample,beer2$rep, sep = ' ') # add rownames to the df

  return(beer2)
  }

# apply terrible function2

june1 <- gen_prcomp(june)
aug1 <- gen_prcomp(aug_1)
aug2 <- gen_prcomp(aug_2)

cellar2 <- gen_prcomp(cellar1)

cell2 <- gen_prcomp(cell1)


plot_pc <-  function(beer) {
  beer_new <- beer[,3:ncol(beer)] 
 
  #
    pca_beer <-  prcomp(beer_new, scale. = TRUE)
  return(pca_beer)
}


june_pca <- plot_pc(june1)
aug1_pca <- plot_pc(aug1)
aug2_pca <-  plot_pc(aug2)
cell_pca <- plot_pc(cellar2)
cell2_pca <-plot_pc(cell2) 

p_june <-  autoplot(june_pca,data = june1 , colour='sample',label = TRUE,label.size=3) 
p_aug1 <-  autoplot(aug1_pca,data = aug1 , colour='sample',label = TRUE,label.size=3)
p_aug2 <-  autoplot(aug2_pca,data = aug2 , colour='sample',label = TRUE,label.size=3)

p_cell1 <- autoplot(cell_pca,data = cellar2 , colour='sample',label = TRUE,label.size=3)

p_cell2 <-autoplot(cell2_pca,data = cell2 , colour='sample',label = TRUE,label.size=3)


 autoplot(pca_beer,data = beer10, colour='sample',frame=T, frame.type='norm') #  draw frames ellipse


ggsave('PCA/combinedcellar_pc1_pc2_frames_names_ellipse.png')
## pca plots
#beer10$variable %>% str_detect("mg")
## combined data set 
june1$month <- 'june'
aug1$month <- 'August_1'
aug2$month <- 'August_2'

bind_rows(cell2, cellar2) -> combined
combined <- combined[,colSums(combined!=0)>0]
combined <-  combined[,c(1,2,22,3:21)]
combined[,3:ncol(combined)] %>% select(where(~!any(is.na(.)))) -> comb
pca_comb <- prcomp(comb,scale.=TRUE)
autoplot(pca_comb,data = combined, colour='sample',label = TRUE,label.size=3,frame=T,frame.type='norm') #  draw frames ellipse



# collapse by month and average
combined %>%
  reshape2::melt(id=c('sample','rep','month')) %>%
  group_by(sample,rep,variable)%>% 
  dplyr::summarise(avg=mean(value)) %>%
  mutate(val= ifelse(str_detect(variable, "mg"),avg*1000,avg*1))%>%
  reshape2::dcast(sample + rep~variable, value.var = 'val') %>%mutate(sample =  factor(sample, levels = c("EC1118","SFE_wt","TFL_1","TFL_2","TFL_3","TFL_4","Must"))) %>%
  arrange(sample) ->d
rownames(d)<-june$Sample
 
aver <- plot_pc(d)
autoplot(aver,data = d , colour='sample',label = TRUE,label.size=3,frame=T) # + facet_grid(~month)# show eigen vectors.
#autoplot(pca_beer,data = beer10, colour='sample', loadings=T) # show eigen vectors.

ggpubr::ggarrange(p_june, p_aug1, p_aug2,
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 1,common.legend = T) %>%
ggpubr::ggexport(filename = 'PCA/multiple.pdf')

ggpubr::ggarrange(p_cell1, p_cell2,
                  labels = c("A", "B"),
                  ncol = 2, nrow = 1,common.legend = T) %>%
  ggpubr::ggexport(filename = 'PCA/cellar1_cellar2_no_frames.pdf')
