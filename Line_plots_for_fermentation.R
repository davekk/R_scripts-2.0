require(readxl); # read excel files
require(tidyr); # data frame manipulation
require(dplyr); # data frame manipulation
require(ggplot2); #cool plot
require(reshape); # convert data into wide and long format
require(magrittr) # pipe and related stuff
setwd('F:/Geisenheim PhD/Bea stuff/plots for erbsloh paper/')
source('F:/Geisenheim PhD/Bea stuff/plots for erbsloh paper/summarySE.R')
#followd thetutorial from these links
#http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization#prepare-the-data
#http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#helper-functions


ptr2 <- readxl::read_xlsx('./fermentations curves/3_PTR2.xlsx') # read the procossed ptr2 excel from teh second sheet

# change the name of teh first 2 column names into 'samples','rep'
names(ptr2)[1:2] <- c('samples','rep')

newDF <- apply(apply(ptr2[,-c(1,2)],2,gsub,patt=",",replace="."),2,as.numeric) # in first stage/apply, convert all commas to dot.second apply convert all the df from character to numeric

newDF <- t(apply(newDF, # pick the ptr2_raw dataset and ignore the first 2 column
                  1, # pick all the rows
                  function(x){ -diff(x) })) %>% data.frame() # apply a function which calculate the negative difference

names(newDF)<-1:16 # rename the column into sensible days in icncubator, which are numeric

## combine into 1 data frame
ptr2_actual <- cbind(ptr2[,c(1,2)], newDF) # bind the metadata of samples and reps into the newD

ptr2_actual$`0`=0 # add day 0 into the data
ptr2_actual <- ptr2_actual[,c("samples","rep","0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16")]# change the order of columns

ptr2_actual %>%  melt(id=c('samples','rep')) ->tall_ptr2 # using the first 2 column as the identifiers


#apply our new function on our data, using the samples and variable, and use the value as input. This collapses the rep column
summarySE(tall_ptr2,groupvars = c('samples','variable'), measurevar = 'value') ->tgc

# coerse the order of samples into order we want
tgc %>% group_by(samples) %>% filter(samples !="NIEL") -> tgc2
tgc$samples <- factor(tgc$samples, levels=c("E1","EC1118","BEA","NIEL","MOST"))
 
#plot of standard error of mean

ggplot(tgc, aes(x=variable, y=value, colour=samples,group=samples)) +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.1) +
  geom_line(size=1) +
  geom_point()

# plot of
# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.05) # move them .05 to the left and right

# se
ggplot(tgc2, aes(x=variable, y=value, colour=samples,group=samples)) +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=1, position=pd) +
  geom_line(position=pd,size=1) +
  geom_point(position=pd)

# Use 95% confidence interval instead of SEM
ggplot(tgc, aes(x=variable, y=value, colour=samples,group=samples)) +
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci),colour="black", width=.1, position=pd) +
  geom_line(position=pd,size=1) +
  geom_point(position=pd, size=3)

#mother of all plots
ggplot(tgc2, aes(x=variable, y=value, colour=samples,group=samples)) +
  #plot the tgc dataframe and use Value as Y, variable as X; color schem based on samples; group helps the ggplot separate the lines nicely :)
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=.1, position=pd) +
  # add error bars based on the value and standard deviation, with the width of the error bar tips as 0.1, black and slightly dodgeeach other
  geom_line(position=pd, size=1) +
  # add line plot of 1 mm thickness, dodging other lines slightly
  geom_point(position=pd, size=3, shape=21, fill="white") +
  # add a scatter plot, with white filled circles
  xlab("Days") +
  # change the label of x from variable to relevant name
  ylab("weight loss (g)") +

  expand_limits(y=0) +                        # Expand y range
  scale_y_continuous(breaks=0:10*.5) +         # Set tick every 0.5
  theme_bw() + # change the theme to be black and white
  scale_colour_manual(name="Accessions",   
               values = c("E1"='red',"EC1118"='blue',"BEA"='violet',"MOST"= 'grey'),
               guide=guide_legend(reverse = F)) +
  theme(legend.justification=c(1,0),
        legend.position='none', # move the legend a bit
      #  plot.title = element_text(hjust = 0.5), # move the title to the centre of the header
        axis.text.x =element_text(size = 9 ,face = "bold.italic", color = "Black", hjust = 0, colour = "grey50"))               # make the x axis text bolder, balck and bigger

ggsave('fermentations curves ptr2 with neil.png') # save the resulting plot as a png
ggsave('fermentations curves ptr2 with niel.pdf') # save the resulting plot as a pdf
