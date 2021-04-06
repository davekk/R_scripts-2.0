require(readxl); # read excel files
require(tidyr); # data frame manipulation
require(dplyr); # data frame manipulation
require(ggplot2); #cool plot
require(reshape); # convert data into wide and long format
require(magrittr) # pipe and related stuff
setwd('F:/Geisenheim PhD/Bea stuff/plots for erbsloh paper/fermentations curves/nboost/')

#follow the tutorial from these links
#http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization#prepare-the-data

#http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#helper-functions


n_boostf <- readxl::read_xlsx('../2_nitrogen boosted fermentations.xlsx',sheet = 2,col_names = F) # read the procossed n_boostf excel from teh second sheet
#n_boostf_raw <- readxl::read_xlsx('plots for erbsloh paper/fermentations curves/n_boostf raw.xlsx') # read teh raw n_boostf data into r


# change the name of teh first 2 column names into 'samples','rep'
names(n_boostf)[1:3] <- c('samples','rep','condition')
#names(n_boostf_raw)[1:2] <- c('samples','rep')

# get the dimensions aand slass of the n_boostf_raw data
# we need the class to be either dataframe or tibble
dim(n_boostf)
class(n_boostf)

#character
## apply will return data we want in rows, to transpose with t() ----

newDF <- t(apply(n_boostf[,-c(1,2,3)], # pick the n_boostf_raw dataset and ignore the first 2 column
                 1, # pick all the rows
                 function(x){ -diff(x) })) %>% data.frame() # apply a function which calculate the negative difference
# the diff command take the difference in (a, b) form of (b-a). We coerrce the difference to be positive
# pipe everything to be coerced into a dataframe format

# since the output is a numeric matrix (rows and columns of numbers), we force it to be a data.frame or tibble

names(newDF)<-1:16 # rename the column into sensible days in icncubator, which are numeric

## combine into 1 data frame
n_boostf_actual <- cbind(n_boostf[,c(1,2,3)], newDF) # bind the metadata of samples and reps into the newD
# quality control
# the result must be all TRUE
#n_boostf_actual[,3]==n_boostf[,4] # check the corresponding column in the processed n_boostf file and the new df are a match
#n_boostf_actual[,18]==n_boostf[,34]
n_boostf_actual$`0`=0.0 # add day 0 into the data
names(n_boostf_actual)


n_boostf_actual <- n_boostf_actual[,c("samples","rep","condition","0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16")]# change the order of columns

n_boostf_actual %>%  # this is a pipe command which makes the code more readable melt(n_boostf_actual,id=c('samples','rep')) will also work and do teh same thing
  melt(id=c('samples','rep',"condition")) ->tall_n_boostf # using the first 2 column as the identifiers
# daysin incubator will be added as a new column called variable. You can change this to anything you want

#the function to make our life easier
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  #library(plyr)

  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }

  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                       .fun = function(xx, col) {
                         c(N    = length2(xx[[col]], na.rm=na.rm),
                           mean = mean   (xx[[col]], na.rm=na.rm),
                           sd   = sd     (xx[[col]], na.rm=na.rm)
                         )
                       },
                       measurevar
  )

  # Rename the "mean" column
  datac <- plyr::rename(datac, c("mean" = measurevar))

  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult

  return(datac)
}


#apply our new function on our data, using the samples and variable, and use the value as input. This collapses the rep column
summarySE(tall_n_boostf,groupvars = c('samples',"condition",'variable'), measurevar = 'value') ->tgc

# coerse the order of samples into order we want
unique(tgc$samples)
tgc$samples <- factor(tgc$samples, levels=c("E1","EC1118","CBS7001","CTR"))
# coerse the ncondition to be factor
tgc$condition <- factor(tgc$condition, levels = c(0,1))
sup.label <- c('Wine must','Wine must with\n Nitrogen supplementation')
names(sup.label) <- 0:1
#plot of standard error of mean

p <- ggplot(tgc, aes(x=variable, y=value, colour=samples,group=samples))

p <-  ggplot(tgc %>% group_by(samples,condition) %>% mutate(cv=cumsum(value)),
             aes(x=variable, y=cv, colour=samples,group=samples))
 p+  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.1) +
  geom_line(size=1) +
  geom_point()+
  facet_wrap(~condition, labeller = labeller(condition=sup.label))

# plot of
# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.05) # move them .05 to the left and right

# se
ggplot(tgc, aes(x=variable, y=value, colour=samples,group=samples)) +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=1, position=pd) +
  geom_line(position=pd,size=1) +
  geom_point(position=pd)

# Use 95% confidence interval instead of SEM
ggplot(tgc, aes(x=variable, y=value, colour=samples,group=samples)) +
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci),colour="black", width=.1, position=pd) +
  geom_line(position=pd,size=1) +
  geom_point(position=pd, size=3)

#mother of all plots
p +
  #plot the tgc dataframe and use Value as Y, variable as X; color schem based on samples; group helps the ggplot separate the lines nicely :)
  geom_errorbar(aes(ymin=cv-sd, ymax=cv+sd), colour="black", width=.1, position=pd) +
  # add error bars based on the value and standard deviation, with the width of the error bar tips as 0.1, black and slightly dodgeeach other
  geom_line(position=pd, size=1) +
  # add line plot of 1 mm thickness, dodging other lines slightly
  geom_point(position=pd, size=3, shape=21, fill="grey") +

  facet_wrap(~condition, labeller = labeller(condition=sup.label))+  # add a scatter plot, with white filled circles
  xlab("Days") +
  # change the label of x from variable to relevant name
  ylab("weight loss (g)") +
  # change the label of y from value to relevant name
  ggtitle("The rate of fermentation\n in incubated samples") +
  #add title line of relevant name
  expand_limits(y=0) +                        # Expand y range
  scale_y_continuous(breaks=0:20*2) +         # Set tick every 0.5
  theme_bw() + # change the theme to be black and white
  scale_colour_manual(
    name="Samples", values = c("E1"='red',"EC1118"='blue',"CBS7001"='darkgreen', "CTR"= 'grey'), # sample color scheme
    #name="Nitrogen boost", values = c(`1`='red',`0`='blue'), labels=c("With Nitrogen","Without Nitrogen"),
                      guide=guide_legend(reverse = F)) +
  theme(legend.title = element_text( size=2), legend.text=element_text(size=2))+
  theme(legend.justification=c(1,0),
        legend.position=c(1,0.5), # move the legend a bit
        plot.title = element_text(hjust = 0.5), # move the title to the centre of the header
        axis.text.x =element_text(size = 9 ,face = "bold.italic", color = "Black", hjust = 0, colour = "grey50"))               # make the x axis text bolder, balck and bigger

ggsave('Nitrogen boost plot compare sample with nitrogen.png',width = 20, height = 20, units = "cm", dpi = 500, device='png') # save the resulting plot as a png
ggsave('Nitrogen boost plot compare sample with nitrogen.pdf') # save the resulting plot as a pdf
