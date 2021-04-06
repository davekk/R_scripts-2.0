require(readxl); # read excel files
require(tidyr); # data frame manipulation
require(dplyr); # data frame manipulation
require(ggplot2); #cool plot
require(reshape); # convert data into wide and long format
require(magrittr) # pipe and related stuff
setwd('F:/Geisenheim PhD/Bea stuff/4_10C FERM/')
source('F:/Geisenheim PhD/Bea stuff/plots for erbsloh paper/summarySE.R')
#followd thetutorial from these links
#http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization#prepare-the-data
#http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#helper-functions


ferm10c <- readxl::read_xlsx('./fermentation weights 10 C.xlsx',range = 'B2:N11') # read the 
#"numeric"
ferm10c <- ferm10c[-c(1,4,5,8),]


newDF <- t(apply(ferm10c[,2:12], # pick the ferm10c_raw dataset and ignore the first 2 column
                 1, # pick all the rows
                 function(x){ -diff(x) })) %>% data.frame() # apply a function which calculate the negative difference


names(newDF)<-1:10 # rename the column into sensible days in icncubator, which are numeric

## combine into 1 data frame
ferm10c_actual <- cbind(ferm10c[,1], newDF) # bind the metadata of samples and reps into the newD

#ferm10c_actual[,18]==ferm10c[,34]
ferm10c_actual[5,1] <- 'CTRL_1'
ferm10c_actual$`0`=0 # add day 0 into the data
names(ferm10c_actual)[1] <- 'samples'
ferm10c_actual <- ferm10c_actual[,c("samples","0","1","2","3","4","5","6","7","8","9","10")]# change the order of columns
ferm10c_actual[is.na(ferm10c_actual)] <- 0
ferm10c_actual %>% tidyr::separate(samples, into=c('sample','rep')) %>%  reshape2::melt(id=c('sample','rep')) ->tall_ferm10c # using the first 2 column as the identifiers

summarySE(tall_ferm10c,groupvars = c('sample','variable'), measurevar = 'value') ->tgc

tgc$sample <- factor(tgc$sample, levels=c("E1","EC1118","CTRL"))


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
  #geom_errorbar(aes(ymin=cv-sd, ymax=cv+sd), colour="black", width=.2, position=pd) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=.1, position=pd) +
  
  # add error bars based on the value and standard deviation, with the width of the error bar tips as 0.1, black and slightly dodgeeach other
  geom_line(position=pd, size=1.5) +
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
  scale_colour_manual(name="Accessions",    # Legend label, use darker colors
                      #manually state what each colour corresponds to
                      values = c("E1"='red',"EC1118"='blue',"CTRL"= 'grey'),
                      guide=guide_legend(reverse = F)) +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0.2), # move the legend a bit
        #  plot.title = element_text(hjust = 0.5), # move the title to the centre of the header
        axis.text.x =element_text(size = 9 ,face = "bold.italic", color = "Black", hjust = 0, colour = "grey50"))               # make the x axis text bolder, balck and bigger

ggsave('fermentations curves ferm10c duplicates.png') # save the resulting plot as a png
ggsave('fermentations curves ferm10c with niel.pdf') # save the resulting plot as a pdf
