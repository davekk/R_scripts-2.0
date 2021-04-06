require(readxl); # read excel files
require(tidyr); # data frame manipulation
require(dplyr); # data frame manipulation
require(ggplot2); #cool plot
require(reshape2); # convert data into wide and long format
require(magrittr) # pipe and related stuff
require(ggsignif)
require(stringr)
require(ggfortify)
source('F:/Geisenheim PhD/Bea stuff/plots for erbsloh paper/summarySE.R')
setwd('F:/Geisenheim PhD/Akan data/feb_2021/plots/')

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
  
   beer <- beer[,colSums(beer!=0)>0]
  return(beer)
}

# read the data
dec_co <- readxl::read_xlsx('../December Co ferm all GCMS.xlsx',sheet = 1,range = 'A4:AB35', col_names = T)
dec_sin <- readxl::read_xlsx('../December Single ferm all GCMS.xlsx',sheet = 1,range = 'A4:AA41', col_names = T)


# apply the terrible function1
comb <-  clean_dat(dec_co)
sing <- clean_dat(dec_sin)

a <- c('I','II','III')
comb[,1] <-  apply(comb[,1],2,gsub,patt="_",replace="-")
comb[,1] <-  paste(as.character(unlist(comb[,1])), a, sep = '_')
##pca
gen_prcomp <- function(beer){
  
  #combined %<>% tidyr::separate(Sample, into=c('sample','rep'),sep=" ")
  beer2 <-  beer %>% tidyr::separate(Sample, into=c('sample','rep'),sep="_") %>% 
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

comb2 <- gen_prcomp(comb)

sing2 <- gen_prcomp(sing)


plot_pc <-  function(beer) {
  beer_new <- beer[,3:ncol(beer)] 
  
  #
  pca_beer <-  prcomp(beer_new, scale. = TRUE)
  return(pca_beer)
}


#
comb_pca <- plot_pc(comb2)
sing_pca <-plot_pc(sing2) 

#p_comb <- 
autoplot(comb_pca,data = comb2 , colour='sample',label = TRUE,label.size=3)

#p_sing <-
  autoplot(sing_pca,data = sing2 , colour='sample',label = TRUE,label.size=3)


autoplot(pca_beer,data = beer10, colour='sample',frame=T, frame.type='norm') #  draw frames ellipse


ggsave('PCA/combinedcellar_pc1_pc2_frames_names_ellipse.png')
## pca plots

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
