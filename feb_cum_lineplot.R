require(tidyr); # data frame manipulation
require(dplyr); # data frame manipulation
require(ggplot2); #cool plot
require(reshape); # convert data into wide and long format
require(magrittr) # pipe and related stuff
setwd('F:/Geisenheim PhD/Bea stuff/feb_2021/')
source('F:/Geisenheim PhD/Bea stuff/plots for erbsloh paper/summarySE.R')
#followd thetutorial from these links
#http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization#prepare-the-data
#http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#helper-functions


wine <- readxl::read_xlsx('Book1.xlsx') # read the 

get_diff <- function(data){
  
  names(data)[1] <- 'samples'
  ## apply will return data we want in rows, to transpose with t() ----
  newDF <- apply(apply(data[,-1],2,gsub,patt=",",replace="."),2,as.numeric) # in first stage/apply, convert all commas to dot.second apply convert
  newDF <- t(apply(newDF,  1, 
                 function(x){ -diff(x) })) %>% data.frame() # apply a function which calculate the negative difference

names(newDF)<-1:ncol(newDF) # rename the column into sensible days in icncubator, which are numeric
newDF$`0` <- 0
newDF <- newDF[,c(ncol(newDF),1:(ncol(newDF)-1))]
newDF <- cbind(data[,1], newDF)
return(newDF)}

wine_actual <- get_diff(wine)

wine_actual[,1] <-  paste(wine_actual[,1], c('I','II','III'), sep = ' ')

#wine_actual[is.na(wine_actual)] <- 0
wine_actual %>% tidyr::separate(samples, into=c('sample','rep'),sep=' ') %>%  
  reshape2::melt(id=c('sample','rep')) ->tall_wine # using the first 2 column as the identifiers

summarySE(tall_wine,groupvars = c('sample','variable'), measurevar = 'value') ->tgc
order.wine <- unique(as.character(unlist(wine[,1])))
tgc$sample <- factor(x=tgc$sample,levels=order.wine, ordered = TRUE)

#plot of standard error of mean

ggplot(tgc, aes(x=variable, y=value, colour=sample,group=sample)) +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.1) +
  geom_line(size=1) +
  geom_point()

# plot of
# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.05) # move them .05 to the left and right

p <- ggplot(tgc %>% group_by(sample) %>% mutate(cv=cumsum(value)),
            aes(x=variable, y=cv, colour=sample,group=sample))
p <- ggplot(tgc, aes(x=variable, y=value, colour=sample,group=sample))
#mother of all plots
p +
  #plot the tgc dataframe and use Value as Y, variable as X; color schem based on samples; group helps the ggplot separate the lines nicely :)
  geom_errorbar(aes(ymin=cv-sd, ymax=cv+sd), colour="black", width=.1, position=pd) +
  #geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=.1, position=pd) +
  
  # add error bars based on the value and standard deviation, with the width of the error bar tips as 0.1, black and slightly dodgeeach other
  geom_line(position=pd, size=1) +
  # add line plot of 1 mm thickness, dodging other lines slightly
  geom_point(position=pd, size=3, shape=21, fill="white") +
  # add a scatter plot, with white filled circles
  xlab("Days") +
  # change the label of x from variable to relevant name
  ylab("weight loss (g)") +
  # change the label of y from value to relevant name
  # ggtitle("The rate of change of weight\n in incubated samples") +
  #add title line of relevant name
  expand_limits(y=0) +                        # Expand y range
  #scale_y_continuous(breaks=0:10*.5) +         # Set tick every 0.5
  theme_bw() + # change the theme to be black and white
  scale_colour_manual(name="Accessions",    # Legend label, use darker colorsc('E1_ch','EC1118_ch','E1_mt','EC1118_mt')
                      #manually state what each colour corresponds to
                      values = c("E1"='red',"EC1118"='blue',"FOT-5"= 'green',"FOT-11"='lightgreen',"FOT-12"='darkgreen',"C-"='grey'),
                      guide=guide_legend(reverse = F)) +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0.2), # move the legend a bit
        #  plot.title = element_text(hjust = 0.5), # move the title to the centre of the header
        axis.text.x =element_text(size = 9 ,face = "bold.italic", color = "Black", hjust = 0, colour = "grey50"))               # make the x axis text bolder, balck and bigger

ggsave('cumulative line plot.png') # save the resulting plot as a png
ggsave('lin plot.pdf') # save the resulting plot as a pdf
