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
setwd('F:/Geisenheim PhD/Akan data/15_feb/plots/')

#follow the tutorial from these links
#http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization#prepare-the-data

#http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#helper-functions



clean_dat <-  function(beer) {
  
   # drop empty column
  not_all_na <- function(x) any(!is.na(x))
  beer %<>% select(where(not_all_na))
  
  beer <- beer[complete.cases(beer),]  #drop empty rows
  
  beer[beer=="nd"] <- '0' # replace nd ith zero as character
  
  beer[beer=="nq"] <- '0' # replace nq with na
  
  ## convert all column into numeric column except the first column
  
  beer[,-1] <- apply(beer[,-1],2,as.numeric)
  
   beer <- beer[,colSums(beer!=0)>0]
  return(beer)
}

# read the data
dat <- readxl::read_xlsx('../For PCA Davies February.xlsx',sheet = 4, col_names = T)



# apply the terrible function1
dat2 <-  clean_dat(dat)
 
 #dat2 -> sing_dec
#dat2-> co_dec
#dat2 -> cellar_sing
#dat2-> celle_cof
dat3 <- bind_rows(mutate_all(co_dec, as.character), mutate_all(sing_dec[25:nrow(sing_dec),], as.character))
dat4 <- bind_rows(mutate_all(celle_cof, as.character), mutate_all(cellar_sing[c(9,10,11,13),], as.character))
dat2 <- bind_rows(mutate_all(dat3, as.character),mutate_all(dat4, as.character))
#bind all 
#dat2 <- bind_rows(mutate_all(cellar_sing, as.character),mutate_all(sing_dec, as.character))#,mutate_all(celle_cof, as.character),mutate_all(sing_dec, as.character), mutate_all(co_dec, as.character)))

dat2[is.na(dat2)] <- '0'
dat2[,-1] <- apply(dat2[,-1],2,as.numeric)

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

dat3 <- gen_prcomp(dat2)



plot_pc <-  function(beer) {
  beer_new <- beer[,3:ncol(beer)] 
  
  #
  pca_beer <-  prcomp(beer_new, scale. = TRUE)
  return(pca_beer)
}


#
dat_pca <- plot_pc(dat3)
 

#p_comb <- 
autoplot(dat_pca,data = dat3 , colour='sample',label = TRUE,label.size=3)


autoplot(dat_pca,data = dat3, colour='sample',frame=T, frame.type='norm') #  draw frames ellipse


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
